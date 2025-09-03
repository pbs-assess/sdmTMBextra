test_that("The barrier model works", {
  skip_if_not_installed("rnaturalearth")
  skip_if_not_installed("INLAspacetime")
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("dplyr")

  library(sdmTMB)
  library(dplyr)
  library(ggplot2)

  # For applied situations on finer scales, you may wish to use scale = "large".
  # For that, first: remotes::install_github("ropensci/rnaturalearthhires")
  map_data <- rnaturalearth::ne_countries(
    scale = "medium",
    returnclass = "sf", country = "canada")

  # Crop the polygon for plotting and efficiency:
  sf::st_bbox(map_data)

  suppressWarnings(bc_coast <-
      sf::st_crop(map_data, c(xmin = -134, ymin = 46, xmax = -120, ymax = 57)))

  crs_utm9 <- 3156 # Pick a projection, here UTM9

  sf::st_crs(bc_coast) <- 4326 # 'WGS84'
  bc_coast <- sf::st_transform(bc_coast, crs_utm9)

  # Project our survey data coordinates:
  dat <- pcod
  survey <- dat %>% dplyr::select(lon, lat, density) %>%
    sf::st_as_sf(crs = 4326, coords = c("lon", "lat")) %>%
    sf::st_transform(crs_utm9)

  # Note that a barrier mesh won't do much here for this
  # example data set, but we nonetheless use it as an example.

  # Prepare for making the mesh
  # First, we will extract the coordinates:
  surv_utm_coords <- sf::st_coordinates(survey)

  # Then we will scale coordinates to km so the range parameter
  # is on a reasonable scale for estimation:
  dat$X1000 <- surv_utm_coords[,1] / 1000
  dat$Y1000 <- surv_utm_coords[,2] / 1000

  # Construct our mesh:
  mesh <- sdmTMB::make_mesh(dat, xy_cols = c("X1000", "Y1000"),
    n_knots = 200, type = "kmeans")

  # Add on the barrier mesh component:
  bspde <- sdmTMBextra::add_barrier_mesh(
    mesh, bc_coast, range_fraction = 0.1,
    proj_scaling = 1000, plot = FALSE
  )
  # Now, when we fit our model with the new mesh, it will automatically
  # include a barrier structure in the spatial correlation:
  fit <- sdmTMB(density ~ depth_scaled, data = pcod, mesh = mesh,
    family = tweedie(link = "log"))
  fit_barrier <- sdmTMB(density ~ depth_scaled, data = pcod, mesh = bspde,
    family = tweedie(link = "log"))
  fit_barrier

  b <- tidy(fit_barrier, "ran_pars")
  expect_equal(b$estimate[b$term == "range"], 20.541930, tolerance = 1e-4)
  expect_equal(b$estimate[b$term == "sigma_O"], 2.163180, tolerance = 1e-4)

  bspde2 <- sdmTMBextra::add_barrier_mesh(
    mesh, bc_coast, range_fraction = 0.5,
    proj_scaling = 1000, plot = FALSE
  )
  fit_barrier2 <- sdmTMB(density ~ depth_scaled, data = pcod, mesh = bspde2,
    family = tweedie(link = "log"))

  b <- tidy(fit_barrier2, "ran_pars")
  expect_equal(b$estimate[b$term == "range"], 20.441976, tolerance = 1e-4)
  expect_equal(b$estimate[b$term == "sigma_O"], 2.155292, tolerance = 1e-4)
})
