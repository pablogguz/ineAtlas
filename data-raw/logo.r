
# Load required packages
library(hexSticker)
library(mapSpain)
library(sf)
library(ggplot2)
library(dplyr)
library(geogrid)

# Get Spain provinces map and prepare it
provinces <- esp_get_prov() %>%
  st_transform(3857)

# Create hexagonal grid
# Calculate grid cells
new_cells <- calculate_grid(
  shape = provinces,
  grid_type = "hexagonal",
  seed = 123  # For reproducibility
)

# Create the grid
hex_grid <- assign_polygons(
  shape = provinces,
  new_polygons = new_cells
)

# Create base plot with hexagonal grid
p_pkg <- ggplot() +
  geom_sf(data = hex_grid, 
          fill = "white", 
          color = "white", 
          alpha = 0.9,
          size = 0.5) +
  theme_void() +
  coord_sf(datum = NA) +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )

# Create sticker for main package
sticker_pkg <- sticker(
  p_pkg,
  package = "ineAtlas",
  p_size = 17,           # Package name size
  s_x = 1,               # Subplot x position
  s_y = 1.1,            # Subplot y position
  s_width = 1.5,        # Subplot width
  s_height = 1.5,       # Subplot height
  h_fill = "#FF3B3B",    # Bright red
  h_color = "#8B0000",   # Darker red border
  p_color = "white",    # Package name color
  p_y = 0.45,            # Package name y position
  url = "pablogguz.github.io/ineAtlas",
  u_size = 5,
  u_color = "white",
  filename = "inst/figures/logo.png"
)

# Generate string to copy-paste into README
usethis::use_logo("inst/figures/logo.png", geometry = "480x556", retina = TRUE)