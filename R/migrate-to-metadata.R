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

# Match locations to folders and write metadata
for (i in seq_along(locations)) {
  location <- locations[[i]]
  folder <- folders[i]
  
  if (is.null(folder) || folder == "") {
    warning(paste("Could not match location", i, "to a folder"))
    next
  }
  
  folder_path <- file.path(images_dir, folder)
  
  # Prepare metadata without the nested paths
  metadata <- list(
    location = location$location,
    title = location$title,
    date_start = location$date_start,
    date_end = location$date_end,
    summary = location$summary,
    campsites = location$campsites
  )
  
  # Fix photo paths: remove "images/[folder]/" prefix
  if (!is.null(location$photos) && length(location$photos) > 0) {
    metadata$photos <- lapply(location$photos, function(photo) {
      # Extract just the filename from the path
      filename <- basename(photo$pth)
      list(
        caption = photo$caption,
        pth = filename
      )
    })
  } else {
    metadata$photos <- list()
  }
  
  # Write to metadata.json
  output_file <- file.path(folder_path, "metadata.json")
  jsonlite::write_json(metadata, output_file, pretty = TRUE, auto_unbox = TRUE)
  
  cat("Migrated location", i, "to", folder, "\n")
}

cat("\nMigration complete! Now you can delete the old locs.json (or keep it as backup).\n")
cat("To regenerate locs.json from the metadata files, run: Rscript R/build-locs-json.R\n")
