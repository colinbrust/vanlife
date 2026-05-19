#!/usr/bin/env Rscript
# Build locs.json from all metadata.json files in image folders
# Usage: Rscript R/build-locs-json.R

library(magrittr)

images_dir <- "docs/images"

# Get all location folders, sorted numerically
folders <- list.dirs(images_dir, full.names = FALSE, recursive = FALSE) %>%
  sort()

# Filter out .DS_Store and other non-location folders
# folders <- folders[!grepl("^\\."), ]

all_locations <- list()

for (folder in folders) {
  metadata_file <- file.path(images_dir, folder, "metadata.json")
  
  if (!file.exists(metadata_file)) {
    warning(paste("No metadata.json found in", folder, "- skipping"))
    next
  }
  
  # Read metadata
  metadata <- jsonlite::read_json(metadata_file)
  
  # Handle both single location object and array of locations
  if (!is.null(metadata$location)) {
    # Single location object
    metadata_list <- list(metadata)
  } else if (is.list(metadata) && is.null(names(metadata))) {
    # Array of locations (unnamed list)
    metadata_list <- metadata
  } else {
    # Try to treat as array anyway
    metadata_list <- list(metadata)
  }
  
  # Process each location in the metadata_list
  for (loc_metadata in metadata_list) {
    # Ensure campsites is always an array (even if empty)
    if (is.null(loc_metadata$campsites) || length(loc_metadata$campsites) == 0) {
      loc_metadata$campsites <- I(list())
    } else {
      loc_metadata$campsites <- I(loc_metadata$campsites)
    }
    
    # Update photo paths to include folder name and full path
    if (!is.null(loc_metadata$photos) && length(loc_metadata$photos) > 0) {
      loc_metadata$photos <- lapply(loc_metadata$photos, function(photo) {
        photo$pth <- file.path("images", folder, photo$pth)
        return(photo)
      })
    }
    
    all_locations[[length(all_locations) + 1]] <- loc_metadata
  }
}

# Write combined locs.json
output_file <- "docs/locs.json"
jsonlite::write_json(all_locations, output_file, pretty = TRUE, auto_unbox = TRUE)

cat("Created", output_file, "with", length(all_locations), "locations\n")
