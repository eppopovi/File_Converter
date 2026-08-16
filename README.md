# Local File Converter

A Shiny app for converting, sorting, merging, and renaming local files.  
Converted files are downloaded as a ZIP archive. Runs best with R and RStudio installed

## Features

- Merge PDF files
- Convert JPG to PDF
- Convert PNG to PDF
- Convert JPG to PNG
- Convert PNG to JPG
- Sort files by type
- Sort files by modified date
- Rename files with optional:
  - prefix
  - suffix
  - find and replace
  - sequence number

## Requirements

Install the required R packages:
**install.packages(c("shiny","tools","zip","fs","magick"))**
