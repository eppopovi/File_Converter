from PIL import Image
import os

# AVIF WARNING IGNORE
import warnings
warnings.filterwarnings("ignore", message="image file could not be identified because AVIF support not installed")


def image_to_pdf(input_path, output_path=None):
    """Converts a JPG or PNG image to a PDF while preserving quality."""
    # Validate file input path 
    if not os.path.exists(input_path):
        raise FileNotFoundError(f"Input file not found: {input_path}")

    # Check file type satisfies
    ext = os.path.splitext(input_path)[1].lower()
    if ext not in ('.jpg', '.jpeg', '.png'):
        raise ValueError("Input file must be a .jpg, .jpeg, or .png image")

    # Default output file name
    if output_path is None:
        output_path = os.path.splitext(input_path)[0] + ".pdf"

    # Open and convert image if needed
    with Image.open(input_path) as img:
        if img.mode in ("RGBA", "P"):
            img = img.convert("RGB")

        # Save as PDF (preserves dimensions & quality)
        img.save(output_path, "PDF", resolution=100.0)

    print(f"\n✅ PDF created successfully:\n{output_path}\n")


if __name__ == "__main__":
    print("How will we be converting our files today?")
    print("Please input your answer in the following format: 'png to pdf' or 'jpg to pdf'")
    conversion_type = input("➡️  ").strip().lower()

    if conversion_type not in ("jpg to pdf", "png to pdf"):
        print("❌ Invalid input. Please type either 'png to pdf' or 'jpg to pdf'.")
        exit(1)

    input_path = input("Please enter the path to your image file: ").strip()

    # 🔍 Clean up path — remove quotes if user included them
    if (input_path.startswith('"') and input_path.endswith('"')) or \
       (input_path.startswith("'") and input_path.endswith("'")):
        input_path = input_path[1:-1]

    try:
        image_to_pdf(input_path)
    except Exception as e:
        print(f"\n⚠️ Error: {e}\n")
