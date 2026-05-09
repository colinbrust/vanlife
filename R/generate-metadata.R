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

# Get all image files (webp, jpg, jpeg, png, gif)
image_files <- list.files(
  folder_path,
  pattern = "\\.(webp|jpg|jpeg|png|gif)$",
  ignore.case = TRUE,
  full.names = FALSE
) %>%
  sort()

if (length(image_files) == 0) {
  stop(paste("No image files found in", folder_path))
}

# Create photos array with empty captions
photos <- lapply(image_files, function(filename) {
  list(
    caption = "",
    pth = filename
  )
})

# Create metadata template
metadata <- list(
  location = "",
  title = "",
  campsites = I(list()),
  date_start = "",
  date_end = "",
  summary = "",
  photos = photos
)

# Write to metadata.json
output_file <- file.path(folder_path, "metadata.json")
jsonlite::write_json(metadata, output_file, pretty = TRUE, auto_unbox = TRUE)

cat("Created metadata.json in", folder_path, "\n")
cat("Found", length(image_files), "images\n")
cat("Please fill in: location, title, date_start, date_end, summary, and photo captions\n")
