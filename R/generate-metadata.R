#!/usr/bin/env Rscript
# Generate metadata.json template for a new location folder
# Usage: Rscript R/generate-metadata.R docs/images/XXX_location_name
library(magrittr)


args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  stop("Please provide the path to the location folder. Usage: Rscript R/generate-metadata.R docs/images/XXX_location_name")
}

folder_path <- args[1]

if (!dir.exists(folder_path)) {
  stop(paste("Folder does not exist:", folder_path))
}

# Get all image files (webp only)
image_files <- list.files(
  folder_path,
  pattern = "\\.webp$",
  ignore.case = TRUE,
  full.names = FALSE
)

# Exclude thumbnail files
image_files <- image_files[!grepl("thumb", image_files, ignore.case = TRUE)]

# Initialize defaults
date_start <- ""
date_end <- ""
photos <- list()

# Only process dates and photos if images exist
if (length(image_files) > 0) {
  # Try to get EXIF dates for sorting
  get_photo_date <- function(filepath) {
    tryCatch({
      # Try using exifr package
      if (requireNamespace("exifr", quietly = TRUE)) {
        exif_data <- exifr::read_exif(filepath)
        if (!is.null(exif_data$DateTimeOriginal) && !is.na(exif_data$DateTimeOriginal)) {
          return(as.POSIXct(exif_data$DateTimeOriginal, format = "%Y:%m:%d %H:%M:%S"))
        }
      }

      # Fallback to file modification time
      file_info <- file.info(filepath)
      return(file_info$mtime)
    }, error = function(e) {
      # Ultimate fallback: use file modification time
      file_info <- file.info(filepath)
      return(file_info$mtime)
    })
  }

  # Get full paths and their dates
  image_data <- data.frame(
    filename = image_files,
    fullpath = file.path(folder_path, image_files),
    stringsAsFactors = FALSE
  )

  image_data$date <- sapply(image_data$fullpath, get_photo_date)

  # Sort by date
  image_data <- image_data %>%
    dplyr::arrange(date)

  # Get min and max dates for date_start and date_end
  date_start <- lubridate::as_datetime(image_data$date) %>%
    min() %>%
    format("%Y-%m-%d")
  date_end <- lubridate::as_datetime(image_data$date) %>%
    max() %>%
    format("%Y-%m-%d")

  # Create photos array with empty captions, sorted by date
  photos <- lapply(image_data$filename, function(filename) {
    list(
      caption = "",
      pth = filename
    )
  })
}

# Create metadata template
metadata <- list(
  location = "",
  title = "",
  campsites = I(list()),
  date_start = date_start,
  date_end = date_end,
  summary = "",
  photos = photos
)

# Write to metadata.json
output_file <- file.path(folder_path, "metadata.json")
jsonlite::write_json(metadata, output_file, pretty = TRUE, auto_unbox = TRUE)

cat("Created metadata.json in", folder_path, "\n")
if (length(image_files) > 0) {
  cat("Found", length(image_files), ".webp images\n")
  cat("Date range:", date_start, "to", date_end, "\n")
  cat("Please fill in: location, title, summary, and photo captions\n")
} else {
  cat("No .webp images found in folder\n")
  cat("Please fill in: location, title, date_start, date_end, summary, and add photos\n")
}
