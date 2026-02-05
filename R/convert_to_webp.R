library(magrittr)

dir_path <- "./docs"

# Get all JPEG files recursively
jpeg_files <- list.files(
  path = dir_path,
  pattern = "\\.(jpg|jpeg)$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

jpeg_files %>%
  purrr::map(function(jpeg_file) {
    webp_file <- sub("\\.(jpg|jpeg)$", ".webp", jpeg_file, ignore.case = TRUE)
    thumbnail_file <- sub("\\.(jpg|jpeg)$", "_thumbnail.webp", jpeg_file, ignore.case = TRUE)

    webp_exists <- file.exists(webp_file)
    thumbnail_exists <- file.exists(thumbnail_file)

    if (webp_exists && thumbnail_exists) {
      cat(sprintf("⊘ Skipped (exists): %s\n", basename(jpeg_file)))
      return(NULL)
    }

    tryCatch({
      # Read and auto-orient based on EXIF data
      img <- magick::image_read(jpeg_file)
      img <- magick::image_orient(img)

      # Write full-size WebP
      # if (!webp_exists) {
        magick::image_write(img, path = webp_file, format = "webp")
      # }

      # Create and write thumbnail (300px max width, preserve aspect ratio)
      if (!thumbnail_exists) {
        thumb <- magick::image_scale(img, "300x")
        magick::image_write(thumb, path = thumbnail_file, format = "webp")
      }

      # Remove original JPEG
      # file.remove(jpeg_file)
      cat(sprintf("✓ Converted: %s\n", basename(jpeg_file)))
    }, error = function(e) {
      cat(sprintf("✗ Failed: %s - %s\n", basename(jpeg_file), e$message))
    })
  })
