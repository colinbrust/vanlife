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

# Read current locations and merge in flight metadata from locs.json
cat("Reading locations from docs/locs.json and", LOCS_FILE, "\n")
locs <- read_json("docs/locs.json")
coord_locs <- read_json(LOCS_FILE)

make_loc_key <- function(loc) {
  location <- if (is.null(loc$location)) "" else as.character(loc$location)
  date_start <- if (is.null(loc$date_start)) "" else as.character(loc$date_start)
  paste0(location, "|||", date_start)
}

coord_lookup <- setNames(
  lapply(coord_locs, function(x) list(lat = x$lat, lng = x$lng)),
  vapply(coord_locs, make_loc_key, character(1))
)

route_locs <- Filter(function(x) !isTRUE(x$flight), locs)
route_locs_df <- lapply(seq_along(route_locs), function(i) {
  loc <- route_locs[[i]]
  key <- make_loc_key(loc)
  coord <- coord_lookup[[key]]

  if (is.null(coord)) {
    stop(paste("Missing coordinates for route location:", loc$location, "on", loc$date_start))
  }

  list(
    idx = i,
    location = loc$location,
    lat = coord$lat,
    lng = coord$lng,
    date = loc$date_start,
    flight = FALSE
  )
})

locs_df <- as.data.frame(do.call(rbind, route_locs_df), stringsAsFactors = FALSE)
locs_df$lat <- as.numeric(locs_df$lat)
locs_df$lng <- as.numeric(locs_df$lng)

cat("Found", nrow(locs_df), "non-flight locations for route generation\n")

# Load existing cache
cache <- list()
if (file.exists(CACHE_FILE)) {
  cat("Loading cache from", CACHE_FILE, "\n")
  cache_data <- read_json(CACHE_FILE)
  cache <- cache_data$routes
} else {
  cat("No existing cache found, starting fresh\n")
}

# Function to create a route key from two point coordinates
make_route_key <- function(lat1, lng1, lat2, lng2) {
  paste0(
    sprintf("%.6f", lat1), "_",
    sprintf("%.6f", lng1), "_",
    sprintf("%.6f", lat2), "_",
    sprintf("%.6f", lng2)
  )
}

# Migrate legacy index-based cache entries to the new coordinate-based format
legacy_cache <- cache
cache <- list()
for (i in 1:(nrow(locs_df) - 1)) {
  from_loc <- locs_df[i, ]
  to_loc <- locs_df[i + 1, ]

  coord_key <- make_route_key(from_loc$lat, from_loc$lng,
                               to_loc$lat, to_loc$lng)
  legacy_key <- paste0(i - 1, "_", i)

  if (!is.null(legacy_cache[[coord_key]])) {
    cache[[coord_key]] <- legacy_cache[[coord_key]]
  } else if (!is.null(legacy_cache[[legacy_key]])) {
    cache[[coord_key]] <- legacy_cache[[legacy_key]]
  }
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
      data <- httr2::resp_body_json(response)

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

  from_loc <- locs_df[from_idx, ]
  to_loc <- locs_df[to_idx, ]
  route_key <- make_route_key(from_loc$lat, from_loc$lng,
                               to_loc$lat, to_loc$lng)

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

route_rows <- lapply(names(cache), function(route_key) {
  route <- cache[[route_key]]

  coords <- route$geometry$coordinates
  if (is.null(coords) || length(coords) == 0) {
    return(NULL)
  }

  coord_matrix <- matrix(unlist(coords), ncol = 2, byrow = TRUE)
  geom <- sf::st_linestring(coord_matrix)

  props <- list(
    from = if (is.null(route$from_location)) NA else paste(unlist(route$from_location), collapse = ", "),
    to = if (is.null(route$to_location)) NA else paste(unlist(route$to_location), collapse = ", "),
    from_date = if (is.null(route$from_date)) NA else paste(unlist(route$from_date), collapse = ", "),
    to_date = if (is.null(route$to_date)) NA else paste(unlist(route$to_date), collapse = ", "),
    distance_m = if (is.null(route$distance)) NA else as.numeric(route$distance),
    duration_s = if (is.null(route$duration)) NA else as.numeric(route$duration)
  )

  list(props = props, geom = geom)
})

route_rows <- Filter(Negate(is.null), route_rows)

if (length(route_rows) == 0) {
  stop("No valid cached routes found to convert to GeoJSON")
}

props_df <- do.call(rbind, lapply(route_rows, function(row) {
  as.data.frame(row$props, stringsAsFactors = FALSE)
}))

geometry_list <- lapply(route_rows, function(row) row$geom)
sf_obj <- sf::st_sf(props_df, geometry = sf::st_sfc(geometry_list, crs = 4326))

# Write GeoJSON
cat("Writing GeoJSON to", OUTPUT_FILE, "\n")
sf::write_sf(sf_obj, OUTPUT_FILE)

cat("\n✓ Complete!\n")
cat("  Cache:", CACHE_FILE, "\n")
cat("  Output:", OUTPUT_FILE, "\n")
cat("  Total route segments:", length(cache), "\n")
