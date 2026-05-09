#!/usr/bin/env Rscript
# Build locs.json from all metadata.json files in image folders
# Usage: Rscript R/build-locs-json.R

library(magrittr)

images_dir <- "docs/images"

# Get all location folders, sorted numerically
folders <- list.dirs(images_dir, full.names = FALSE, recursive = FALSE) %>%
  sort()

# Filter out .DS_Store and other non-location folders
folders <- folders[!grepl("^\\."), ]

all_locations <- list()

for (folder in folders) {
  metadata_file <- file.path(images_dir, folder, "metadata.json")
  
  if (!file.exists(metadata_file)) {
    warning(paste("No metadata.json found in", folder, "- skipping"))
    next
  }
  
  # Read metadata
  metadata <- jsonlite::read_json(metadata_file)
  
  # Update photo paths to include folder name and full path
  if (!is.null(metadata$photos) && length(metadata$photos) > 0) {
    metadata$photos <- lapply(metadata$photos, function(photo) {
      photo$pth <- file.path("images", folder, photo$pth)
      return(photo)
    })
  }
  
  all_locations[[length(all_locations) + 1]] <- metadata
}

# Write combined locs.json
output_file <- "docs/locs.json"
jsonlite::write_json(all_locations, output_file, pretty = TRUE, auto_unbox = TRUE)

cat("Created", output_file, "with", length(all_locations), "locations\n")
