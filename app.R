library(shiny)
library(tools)
library(fs)
library(zip)
library(magick)

ui <- fluidPage(
  titlePanel("Local File Converter"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput(
        inputId = "files",
        label = "Upload files",
        multiple = TRUE,
        accept = c(
          ".pdf", ".png", ".jpg", ".jpeg",
          ".geojson",
          ".gpkg", ".gpx", ".kml", ".shp"
        )
      ),
      
      selectInput(
        inputId = "conversion",
        label = "Conversion",
        choices = c(
          "merge_pdf",
          "jpg_to_pdf",
          "png_to_pdf",
          "jpg_to_png",
          "png_to_jpg",
          "sort_by_type",
          "sort_by_date",
          "rename_files"
        )
      ),
      
      conditionalPanel(
        condition = "input.conversion == 'rename_files'",
        
        tags$hr(),
        h4("Rename options"),
        
        helpText("Optional: use prefix, suffix, find/replace, or any combination."),
        
        checkboxInput("use_prefix", "Use prefix", TRUE),
        
        conditionalPanel(
          condition = "input.use_prefix == true",
          textInput("rename_prefix", "Filename prefix", "converted_file")
        ),
        
        checkboxInput("use_suffix", "Use suffix", FALSE),
        
        conditionalPanel(
          condition = "input.use_suffix == true",
          textInput("rename_suffix", "Filename suffix", "renamed")
        ),
        
        checkboxInput("use_replace", "Use find and replace", FALSE),
        
        conditionalPanel(
          condition = "input.use_replace == true",
          textInput("replace_find", "Find text", ""),
          textInput("replace_with", "Replace with", "")
        ),
        
        checkboxInput("use_sequence", "Add sequence number", TRUE)
      ),
      
      tags$hr(),
      
      downloadButton(
        outputId = "download_zip",
        label = "Convert and download ZIP"
      )
    ),
    
    mainPanel(
      h4("Uploaded files"),
      tableOutput("file_table"),
      
      conditionalPanel(
        condition = "input.conversion == 'rename_files'",
        h4("Rename preview"),
        tableOutput("rename_preview")
      ),
      
      h4("Status"),
      verbatimTextOutput("status")
    )
  )
)

server <- function(input, output, session) {
  
  output$file_table <- renderTable({
    req(input$files)
    
    data.frame(
      file_name = input$files$name,
      size_kb = round(input$files$size / 1024, 2),
      type = input$files$type,
      stringsAsFactors = FALSE
    )
  })
  
  output$status <- renderText({
    req(input$files)
    
    paste0(
      "Ready to run conversion: ", input$conversion,
      "\nFiles uploaded: ", nrow(input$files)
    )
  })
  
  require_package <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf(
        "The required package '%s' is not installed or bundled with this app.",
        pkg
      ))
    }
  }
  
  copy_uploaded_files <- function(uploaded_files, input_dir) {
    dir_create(input_dir)
    
    copied_paths <- file.path(input_dir, uploaded_files$name)
    
    file.copy(
      from = uploaded_files$datapath,
      to = copied_paths,
      overwrite = TRUE
    )
    
    copied_paths
  }
  
  check_extensions <- function(paths, allowed_extensions) {
    extensions <- tolower(file_ext(paths))
    
    if (!all(extensions %in% allowed_extensions)) {
      stop(sprintf(
        "Invalid file type. Expected: %s",
        paste(allowed_extensions, collapse = ", ")
      ))
    }
  }
  
  convert_image <- function(input_path, output_path, output_format) {
    require_package("magick")
    
    magick::image_read(input_path) |>
      magick::image_write(path = output_path, format = output_format)
  }
  
  image_to_pdf <- function(input_path, output_path) {
    convert_image(input_path, output_path, "pdf")
  }
  
  merge_pdf <- function(input_paths, output_path) {
    require_package("qpdf")
    
    qpdf::pdf_combine(
      input = input_paths,
      output = output_path
    )
  }
  
  convert_each_file <- function(input_paths, output_dir, new_extension, converter) {
    for (input_path in input_paths) {
      base_name <- file_path_sans_ext(basename(input_path))
      output_path <- file.path(output_dir, paste0(base_name, ".", new_extension))
      converter(input_path, output_path)
    }
  }
  
  sort_files_by_type <- function(input_paths, output_dir) {
    for (path in input_paths) {
      extension <- tolower(file_ext(path))
      extension <- ifelse(extension == "", "no_extension", extension)
      
      type_dir <- file.path(output_dir, extension)
      dir_create(type_dir)
      
      file.copy(
        from = path,
        to = file.path(type_dir, basename(path)),
        overwrite = TRUE
      )
    }
  }
  
  sort_files_by_date <- function(input_paths, output_dir) {
    for (path in input_paths) {
      modified_date <- as.Date(file.info(path)$mtime)
      date_dir <- file.path(output_dir, as.character(modified_date))
      
      dir_create(date_dir)
      
      file.copy(
        from = path,
        to = file.path(date_dir, basename(path)),
        overwrite = TRUE
      )
    }
  }
  
  build_new_filename <- function(
    original_name,
    index,
    use_prefix = FALSE,
    prefix = "",
    use_suffix = FALSE,
    suffix = "",
    use_replace = FALSE,
    find_text = "",
    replace_text = "",
    use_sequence = TRUE
  ) {
    extension <- file_ext(original_name)
    base_name <- file_path_sans_ext(basename(original_name))
    
    if (use_replace && nzchar(find_text)) {
      base_name <- gsub(find_text, replace_text, base_name, fixed = TRUE)
    }
    
    if (use_prefix && nzchar(prefix)) {
      base_name <- paste(prefix, base_name, sep = "_")
    }
    
    if (use_suffix && nzchar(suffix)) {
      base_name <- paste(base_name, suffix, sep = "_")
    }
    
    if (use_sequence) {
      base_name <- paste(base_name, sprintf("%03d", index), sep = "_")
    }
    
    if (nzchar(extension)) paste0(base_name, ".", extension) else base_name
  }
  
  get_rename_options <- reactive({
    list(
      use_prefix = isTRUE(input$use_prefix),
      prefix = input$rename_prefix %||% "",
      use_suffix = isTRUE(input$use_suffix),
      suffix = input$rename_suffix %||% "",
      use_replace = isTRUE(input$use_replace),
      find_text = input$replace_find %||% "",
      replace_text = input$replace_with %||% "",
      use_sequence = isTRUE(input$use_sequence)
    )
  })
  
  output$rename_preview <- renderTable({
    req(input$files)
    req(input$conversion == "rename_files")
    
    opts <- get_rename_options()
    
    data.frame(
      original_name = input$files$name,
      new_name = mapply(
        function(name, i) {
          do.call(
            build_new_filename,
            c(
              list(original_name = name, index = i),
              opts
            )
          )
        },
        input$files$name,
        seq_along(input$files$name)
      ),
      stringsAsFactors = FALSE
    )
  })
  
  rename_files <- function(
    input_paths,
    output_dir,
    use_prefix = FALSE,
    prefix = "",
    use_suffix = FALSE,
    suffix = "",
    use_replace = FALSE,
    find_text = "",
    replace_text = "",
    use_sequence = TRUE
  ) {
    for (i in seq_along(input_paths)) {
      new_name <- build_new_filename(
        original_name = basename(input_paths[i]),
        index = i,
        use_prefix = use_prefix,
        prefix = prefix,
        use_suffix = use_suffix,
        suffix = suffix,
        use_replace = use_replace,
        find_text = find_text,
        replace_text = replace_text,
        use_sequence = use_sequence
      )
      
      file.copy(
        from = input_paths[i],
        to = file.path(output_dir, new_name),
        overwrite = TRUE
      )
    }
  }
  
  run_conversion <- function(uploaded_files, conversion, output_dir, rename_options) {
    input_dir <- file.path(output_dir, "_input")
    input_paths <- copy_uploaded_files(uploaded_files, input_dir)
    
    converted_dir <- file.path(output_dir, "converted_files")
    dir_create(converted_dir)
    
    if (conversion == "merge_pdf") {
      check_extensions(input_paths, "pdf")
      merge_pdf(input_paths, file.path(converted_dir, "merged.pdf"))
      return(converted_dir)
    }
    
    if (conversion == "jpg_to_pdf") {
      check_extensions(input_paths, c("jpg", "jpeg"))
      convert_each_file(input_paths, converted_dir, "pdf", image_to_pdf)
      return(converted_dir)
    }
    
    if (conversion == "png_to_pdf") {
      check_extensions(input_paths, "png")
      convert_each_file(input_paths, converted_dir, "pdf", image_to_pdf)
      return(converted_dir)
    }
    
    if (conversion == "jpg_to_png") {
      check_extensions(input_paths, c("jpg", "jpeg"))
      convert_each_file(
        input_paths,
        converted_dir,
        "png",
        function(input_path, output_path) {
          convert_image(input_path, output_path, "png")
        }
      )
      return(converted_dir)
    }
    
    if (conversion == "png_to_jpg") {
      check_extensions(input_paths, "png")
      convert_each_file(
        input_paths,
        converted_dir,
        "jpg",
        function(input_path, output_path) {
          convert_image(input_path, output_path, "jpg")
        }
      )
      return(converted_dir)
    }
    
    if (conversion == "sort_by_type") {
      sort_files_by_type(input_paths, converted_dir)
      return(converted_dir)
    }
    
    if (conversion == "sort_by_date") {
      sort_files_by_date(input_paths, converted_dir)
      return(converted_dir)
    }
    
    if (conversion == "rename_files") {
      do.call(
        rename_files,
        c(
          list(
            input_paths = input_paths,
            output_dir = converted_dir
          ),
          rename_options
        )
      )
      
      return(converted_dir)
    }
    
    stop(sprintf("Unsupported conversion: %s", conversion))
  }
  
  output$download_zip <- downloadHandler(
    filename = function() {
      paste0("converted_files_", Sys.Date(), ".zip")
    },
    
    content = function(file) {
      req(input$files)
      
      work_dir <- file.path(
        tempdir(),
        paste0("file_converter_", as.integer(Sys.time()))
      )
      
      dir_create(work_dir)
      
      converted_dir <- run_conversion(
        uploaded_files = input$files,
        conversion = input$conversion,
        output_dir = work_dir,
        rename_options = get_rename_options()
      )
      
      old_wd <- getwd()
      on.exit(setwd(old_wd), add = TRUE)
      
      setwd(converted_dir)
      
      zip::zipr(
        zipfile = file,
        files = list.files(
          converted_dir,
          recursive = TRUE,
          full.names = FALSE,
          all.files = FALSE
        )
      )
    }
  )
}

shinyApp(ui = ui, server = server)