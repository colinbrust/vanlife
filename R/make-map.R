library(magrittr)
locations <- c("Missoula, MT", "Pocatello, ID", "St. George, UT", "Las Vegas, NV",
               "Boulder City, NV", "Wickenberg, AZ", "Tucson, AZ", "Summerhaven, AZ",
               "Apache City, AZ", "Peoria, AZ", "Sedona, AZ", "Flagstaff, AZ", "Durango, CO",
               "Page, AZ", "Las Vegas, NV", "Joshua Tree National Park, CA", "Borrego Springs, CA",
               "Julian, CA", "Dulzura, CA", "Encinitas, CA", "San")

# Parse and geocode
parse_location <- function(loc) {
  if (grepl("^-?\\d+\\.\\d+,\\s*-?\\d+\\.\\d+$", loc)) {
    coords <- as.numeric(strsplit(gsub(" ", "", loc), ",")[[1]])
    return(data.frame(lat = coords[1], long = coords[2]))
  } else {
    return(tidygeocoder::geo(loc, method = "osm"))
  }
}

coords <- purrr::map(locations, parse_location) %>%
  dplyr::bind_rows

coords_sf <- sf::st_as_sf(coords, coords = c("long", "lat"), crs = 4326)

# Create line between points
line_sf <- sf::st_sf(
  geometry = sf::st_sfc(sf::st_linestring(as.matrix(coords[, c("long", "lat")])), crs = 4326)
)

# Create interactive map
mapview::mapview(line_sf, layer.name = "Route", color = "blue", lwd = 2) +
  mapview::mapview(coords_sf, zcol = "order", layer.name = "Stops",
                   col.regions = viridisLite::viridis(nrow(coords)),
                   popup = leafpop::popupTable(coords[, c("location", "order")]))


# # Create map
# us_map <- ggplot2::map_data("state")
#
# ggplot2::ggplot() +
#   ggplot2::geom_polygon(data = us_map, ggplot2::aes(x = long, y = lat, group = group),
#                         fill = "#f5f5dc", color = "#8b7355", size = 0.3) +
#   ggplot2::geom_path(data = coords, ggplot2::aes(x = long, y = lat),
#                      arrow = arrow(type = 'open', angle = 30, length = unit(0.1, "inches"))) +
#   ggplot2::geom_point(data = coords, ggplot2::aes(x = long, y = lat),
#                       shape = emojifont::emoji("pushpin"), size = 5) +
#   ggplot2::coord_fixed(1.3, xlim = range(coords$long) + c(-8, 2),
#                        ylim = range(coords$lat) + c(-2, 2)) +
#   ggplot2::theme_minimal() +
#   ggplot2::theme(panel.grid = ggplot2::element_blank(),
#                  axis.text = ggplot2::element_blank(),
#                  axis.title = ggplot2::element_blank())
