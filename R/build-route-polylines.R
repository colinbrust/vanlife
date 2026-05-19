#!/usr/bin/env Rscript
# Build route polylines from location coordinates
# Queries OSRM (Open Source Routing Machine) API for road routes between consecutive points
# Implements caching to only query new route segments when new points are added
#
# Usage: Rscript R/build-route-polylines.R
#
# Output:
#   - docs/routes-cache.json (cache of all queried routes)
#   - docs/routes.geojson (final polyline GeoJSON for mapping)

library(jsonlite)
library(magrittr)
library(httr2)

# Configuration
LOCS_FILE <- "docs/locs-coords.json"
CACHE_FILE <- "docs/routes-cache.json"
OUTPUT_FILE <- "docs/routes.geojson"
OSRM_URL <- "http://router.project-osrm.org/route/v1/driving"

# Read current locations
cat("Reading locations from", LOCS_FILE, "\n")
locs <- read_json(LOCS_FILE)

# Convert to data frame for easier manipulation
locs_df <- data.frame(
  idx = seq_along(locs),
  location = sapply(locs, function(x) x$location),
  lat = sapply(locs, function(x) x$lat),
  lng = sapply(locs, function(x) x$lng),
  date = sapply(locs, function(x) x$date_start)
)

cat("Found", nrow(locs_df), "locations\n")

# Load existing cache
cache <- list()
if (file.exists(CACHE_FILE)) {
  cat("Loading cache from", CACHE_FILE, "\n")
  cache_data <- read_json(CACHE_FILE)
  cache <- cache_data$routes
} else {
  cat("No existing cache found, starting fresh\n")
}

# Function to create a route key from two point indices
make_route_key <- function(from_idx, to_idx) {
  paste0(from_idx - 1, "_", to_idx - 1)  # 0-indexed in the key
}

# Function to query OSRM for a route
query_osrm_route <- function(lat1, lng1, lat2, lng2) {
  # OSRM format: lng,lat (longitude first!)
  coordinates <- paste0(lng1, ",", lat1, ";", lng2, ",", lat2)

  url <- paste0(OSRM_URL, "/", coordinates,
                "?steps=false&geometries=geojson&overview=full")

  tryCatch({
    cat("  Querying OSRM for route...\n")

    response <- request(url) %>%
      req_perform(verbosity = 0)

    if (response$status_code == 200) {
      data <- httr2::response_body_json(response)

      if (data$code == "Ok" && length(data$routes) > 0) {
        route <- data$routes[[1]]
        return(list(
          geometry = route$geometry,
          distance = route$distance,
          duration = route$duration,
          success = TRUE
        ))
      } else {
        warning("OSRM returned non-OK code")
        return(list(success = FALSE, error = "OSRM error"))
      }
    } else {
      warning(paste("HTTP", response$status_code))
      return(list(success = FALSE, error = "HTTP error"))
    }
  }, error = function(e) {
    warning(paste("Error querying OSRM:", e$message))
    return(list(success = FALSE, error = e$message))
  })
}

# Identify which routes are new
routes_to_query <- list()
for (i in 1:(nrow(locs_df) - 1)) {
  from_idx <- i
  to_idx <- i + 1
  route_key <- make_route_key(from_idx, to_idx)

  if (is.null(cache[[route_key]])) {
    routes_to_query[[route_key]] <- list(from_idx = from_idx, to_idx = to_idx)
  }
}

cat("\nRoute segments in cache:", length(cache), "\n")
cat("Route segments to query:", length(routes_to_query), "\n")

# Query new routes with rate limiting (OSRM has limits)
if (length(routes_to_query) > 0) {
  cat("\nQuerying new routes...\n")

  for (route_key in names(routes_to_query)) {
    route_info <- routes_to_query[[route_key]]
    from_idx <- route_info$from_idx
    to_idx <- route_info$to_idx

    from_loc <- locs_df[from_idx, ]
    to_loc <- locs_df[to_idx, ]

    cat(paste0("\n[", from_idx, "→", to_idx, "] ",
               from_loc$location, " → ", to_loc$location, "\n"))

    result <- query_osrm_route(from_loc$lat, from_loc$lng,
                               to_loc$lat, to_loc$lng)

    if (result$success) {
      cache[[route_key]] <- list(
        from_location = from_loc$location,
        to_location = to_loc$location,
        from_date = from_loc$date,
        to_date = to_loc$date,
        geometry = result$geometry,
        distance = result$distance,
        duration = result$duration,
        queried_at = Sys.time()
      )
      cat("  ✓ Route cached\n")
    } else {
      cat("  ✗ Failed:", result$error, "\n")
    }

    # Rate limiting - be respectful to OSRM
    Sys.sleep(0.5)
  }
}

# Save updated cache
cat("\nSaving cache to", CACHE_FILE, "\n")
cache_output <- list(
  routes = cache,
  total_segments = length(cache),
  last_updated = Sys.time()
)
write_json(cache_output, CACHE_FILE, pretty = TRUE)

# Build GeoJSON FeatureCollection from all cached routes
cat("Building GeoJSON output...\n")

features <- list()
for (route_key in names(cache)) {
  route <- cache[[route_key]]

  feature <- list(
    type = "Feature",
    properties = list(
      from = route$from_location,
      to = route$to_location,
      from_date = route$from_date,
      to_date = route$to_date,
      distance_m = route$distance,
      duration_s = route$duration
    ),
    geometry = route$geometry
  )

  features[[length(features) + 1]] <- feature
}

geojson_output <- list(
  type = "FeatureCollection",
  features = features
)

# Write GeoJSON
cat("Writing GeoJSON to", OUTPUT_FILE, "\n")
write_json(geojson_output, OUTPUT_FILE, pretty = TRUE)

cat("\n✓ Complete!\n")
cat("  Cache:", CACHE_FILE, "\n")
cat("  Output:", OUTPUT_FILE, "\n")
cat("  Total route segments:", length(cache), "\n")
