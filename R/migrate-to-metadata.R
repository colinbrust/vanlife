#!/usr/bin/env Rscript
# Migrate existing locs.json into metadata.json files in each folder
# Usage: Rscript R/migrate-to-metadata.R

library(magrittr)

locs_file <- "docs/locs.json"
images_dir <- "docs/images"

# Read the existing locs.json
locations <- jsonlite::read_json(locs_file)

# Get all location folders, sorted
folders <- list.dirs(images_dir, full.names = FALSE, recursive = FALSE) %>%
  sort() %>%
  subset(!grepl("^\\.", .))

if (length(locations) != length(folders)) {
  warning(paste("Mismatch: found", length(locations), "locations but", 
                length(folders), "folders. Proceeding anyway..."))
}

# First, infer folder mapping from photo paths in locations
location_to_folder <- sapply(locations, function(location) {
  if (!is.null(location$photos) && length(location$photos) > 0) {
    # Extract folder from first photo path (format: images/FOLDER_NAME/filename)
    first_photo <- location$photos[[1]]$pth
    parts <- strsplit(first_photo, "/")[[1]]
    if (length(parts) >= 2) return(parts[2])
  }
  return(NA_character_)
})

# Group locations by folder
locations_by_folder <- split(
  seq_along(locations),
  location_to_folder
)

# Write metadata for each folder
for (folder_name in names(locations_by_folder)) {
  location_indices <- locations_by_folder[[folder_name]]
  folder_path <- file.path(images_dir, folder_name)
  
  if (!dir.exists(folder_path)) {
    warning(paste("Folder does not exist:", folder_path))
    next
  }
  
  # Prepare metadata for all locations in this folder
  metadata_list <- lapply(location_indices, function(i) {
    location <- locations[[i]]
    
    metadata <- list(
      location = location$location,
      title = location$title,
      date_start = location$date_start,
      date_end = location$date_end,
      summary = location$summary,
      campsites = if (is.null(location$campsites) || length(location$campsites) == 0) {
        I(list())
      } else {
        I(location$campsites)
      }
    )
    
    # Fix photo paths: remove "images/[folder]/" prefix
    if (!is.null(location$photos) && length(location$photos) > 0) {
      metadata$photos <- lapply(location$photos, function(photo) {
        filename <- basename(photo$pth)
        list(
          caption = photo$caption,
          pth = filename
        )
      })
    } else {
      metadata$photos <- list()
    }
    
    return(metadata)
  })
  
  # If only one location, write as object; if multiple, write as array
  if (length(metadata_list) == 1) {
    output_metadata <- metadata_list[[1]]
  } else {
    output_metadata <- metadata_list
  }
  
  # Write to metadata.json
  output_file <- file.path(folder_path, "metadata.json")
  jsonlite::write_json(output_metadata, output_file, pretty = TRUE, auto_unbox = TRUE)
  
  cat("Migrated", length(metadata_list), "location(s) to", folder_name, "\n")
}

cat("\nMigration complete! Now you can delete the old locs.json (or keep it as backup).\n")
cat("To regenerate locs.json from the metadata files, run: Rscript R/build-locs-json.R\n")
