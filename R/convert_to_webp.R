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
    if (file.exists(webp_file)) {
      cat(sprintf("⊘ Skipped (exists): %s\n", basename(jpeg_file)))
      return(NULL)
    }
    tryCatch({
      # Read and write as WebP
      img <- magick::image_read(jpeg_file)
      magick::image_write(img, path = webp_file, format = "webp")

      # Remove original JPEG
      # file.remove(jpeg_file)
      cat(sprintf("✓ Converted: %s\n", basename(jpeg_file)))
    }, error = function(e) {
      cat(sprintf("✗ Failed: %s - %s\n", basename(jpeg_file), e$message))
    })
  })
