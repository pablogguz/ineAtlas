coords <- tibble::tibble(
    addr = "Calle de Alcalá 1, Madrid"
) |>
    tidygeocoder::geocode(
        address = addr,
        method = "osm"
    )

point <- sf::st_as_sf(
    coords,
    coords = c("long", "lat"),
    crs = 4326
)

tracts <- ineAtlas::get_tract_geom(2022) |>
    dplyr::filter(
        stringr::str_sub(tract_code, 1, 2) == "28"
    ) |>
    sf::st_transform(4326)

tracts |>
  dplyr::slice(
    sf::st_intersects(point, tracts)[[1]]
)

