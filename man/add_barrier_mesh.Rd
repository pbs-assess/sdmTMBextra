% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/barrier.R
\name{add_barrier_mesh}
\alias{add_barrier_mesh}
\title{Transform a mesh object into a mesh with correlation barriers}
\usage{
add_barrier_mesh(
  spde_obj,
  barrier_sf,
  range_fraction = 0.2,
  proj_scaling = 1,
  plot = FALSE
)
}
\arguments{
\item{spde_obj}{Output from \code{\link[sdmTMB:make_mesh]{sdmTMB::make_mesh()}}.}

\item{barrier_sf}{An sf object with polygons defining the barriers. For
example, a coastline dataset for ocean data. \strong{Note that this object must
have the same projection as the data used to generate the x and y columns
in \code{spde_obj}.}}

\item{range_fraction}{The fraction of the spatial range that barrier
triangles have.}

\item{proj_scaling}{If \code{spde_obj} was created with scaling of the coordinates
after the projection (e.g., dividing UTMs by 1000 so the spatial range is
on a reasonable scale) the x and y values in \code{spde_obj} are multiplied by
this scaling factor before applying the projection from \code{barrier_sf}.}

\item{plot}{Logical.}
}
\value{
A list similar to \code{\link[sdmTMB:make_mesh]{sdmTMB::make_mesh()}} but with \code{spde_barrier} and a
couple other helper list elements added.

If \code{plot = TRUE}, then a basic plot will be created as a side effect. Each
grey dot represents the center of a "normal" mesh triangle. Each red cross
represents the center of a "barrier" mesh triangle.
}
\description{
Transform a mesh object into a mesh with correlation barriers
}
\examples{
if (require("sf", quietly = TRUE) &&
  require("ggplot2", quietly = TRUE) &&
  require("dplyr", quietly = TRUE) &&
  require("INLA", quietly = TRUE) &&
  require("sdmTMB", quietly = TRUE) &&
  require("rnaturalearth", quietly = TRUE)) {

library(sdmTMB)
library(dplyr)
library(ggplot2)

# For applied situations on finer scales, you may with to use scale = "large".
# For that, first: remotes::install_github("ropensci/rnaturalearthhires")
map_data <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf", country = "canada")

# Crop the polygon for plotting and efficiency:
sf::st_bbox(map_data)
bc_coast <-
  sf::st_crop(map_data, c(xmin = -134, ymin = 46, xmax = -120, ymax = 57))

crs_utm9 <- 3156 # Pick a projection, here UTM9

sf::st_crs(bc_coast) <- 4326 # 'WGS84'
bc_coast <- sf::st_transform(bc_coast, crs_utm9)

# Project our survey data coordinates:
dat <- pcod
survey <- dat \%>\% dplyr::select(lon, lat, density) \%>\%
  sf::st_as_sf(crs = 4326, coords = c("lon", "lat")) \%>\%
  sf::st_transform(crs_utm9)

# Plot our coast and survey data:
ggplot(bc_coast) +
  geom_sf() +
  geom_sf(data = survey, size = 0.5)

# Note that a barrier mesh won't do much here for this
# example data set, but we nonetheless use it as an example.

# Prepare for making the mesh
# First, we will extract the coordinates:
surv_utm_coords <- sf::st_coordinates(survey)

# Then we will scale coordinates to km so the range parameter
# is on a reasonable scale for estimation:
dat$X1000 <- surv_utm_coords[,1] / 1000
dat$Y1000 <- surv_utm_coords[,2] / 1000

mesh <- sdmTMB::make_mesh(dat, xy_cols = c("X1000", "Y1000"),
  n_knots = 200, type = "kmeans")
plot(mesh)

# Add on the barrier mesh component:
bspde <- sdmTMBextra::add_barrier_mesh(
  mesh, bc_coast, range_fraction = 0.1,
  proj_scaling = 1000, plot = TRUE
)

# In the above, the grey dots are the centre of triangles that are in the
# ocean. The red crosses are centres of triangles that are over land. The
# spatial range will be assumed to be 0.1 (`range_fraction`) over land
# compared to over water.

# We can make a more advanced plot if we want:
mesh_df_water <- bspde$mesh_sf[bspde$normal_triangles, ]
mesh_df_land <- bspde$mesh_sf[bspde$barrier_triangles, ]

ggplot(bc_coast) +
  geom_sf() +
  geom_sf(data = mesh_df_water, size = 1, colour = "blue") +
  geom_sf(data = mesh_df_land, size = 1, colour = "green")

# Now, when we fit our model with the new mesh, it will automatically
# include a barrier structure in the spatial correlation:
fit <- sdmTMB(density ~ s(depth, k = 3), data = pcod, mesh = bspde,
  family = tweedie(link = "log"))
fit
}
}
\references{
Bakka, H., Vanhatalo, J., Illian, J., Simpson, D., and Rue, H. 2019.
Non-stationary Gaussian models with physical barriers.
\url{https://arxiv.org/abs/1608.03787}

\url{https://sites.google.com/a/r-inla.org/www/barrier-model}

\url{https://haakonbakkagit.github.io/btopic107.html}
}
