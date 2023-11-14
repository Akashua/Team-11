# Set the seed for reproducibility
set.seed(123)

# Function to generate random points within a polygon boundary
generate_points <- function(n, poly){
  points <- spsample(poly, n, "random")
  return(coordinates(points))
}

# Function to create a SpatialPolygons object from an extent
create_polygon <- function(ext) {
  coords <- matrix(c(ext@xmin, ext@ymin,
                     ext@xmin, ext@ymax,
                     ext@xmax, ext@ymax,
                     ext@xmax, ext@ymin,
                     ext@xmin, ext@ymin), ncol = 2, byrow = TRUE)
  p <- Polygon(coords)
  ps <- Polygons(list(p), ID = "1")
  sp_poly <- SpatialPolygons(list(ps))
  return(sp_poly)
}

# Define polygons
north_poly <- create_polygon(raster::extent(494, 496, 2150, 2156))
south_poly <- create_polygon(raster::extent(496, 500, 2145, 2150))

# Generate theft locations
theft_locations_north <- generate_points(689, north_poly)
theft_locations_south <- generate_points(3327, south_poly)

# Simulate covariates for the 90 areal units
covariates <- data.frame(
  Pop15 = rnorm(90, mean=1000, sd=200),
  Apart = rnorm(90, mean=500, sd=100),
  Eco = rnorm(90, mean=700, sd=150),
  Employ = rnorm(90, mean=600, sd=120),
  inBorn = rnorm(90, mean=800, sd=160),
  Health = rnorm(90, mean=900, sd=180),
  Scholar = round(rnorm(90, mean=8, sd=1)),
  Extor = rpois(90, lambda=2),
  Murder = rpois(90, lambda=1),
  Burg = rpois(90, lambda=3),
  Shop = rpois(90, lambda=4),
  Public = rpois(90, lambda=5),
  Street = rpois(90, lambda=6),
  Kidnap = rpois(90, lambda=0.5)
)

# Simulate spatial coordinates for the 90 blocks (this is arbitrary)
set.seed(123)  # Ensure reproducibility
covariates$x <- runif(90, min = 494, max = 500)
covariates$y <- runif(90, min = 2145, max = 2156)

# Convert theft locations to data frames
north_theft_locations_df <- data.frame(x = theft_locations_north[, 1], y = theft_locations_north[, 2])
south_theft_locations_df <- data.frame(x = theft_locations_south[, 1], y = theft_locations_south[, 2])

# Find the nearest block for each theft location in the North region
nearest_indices_north <- get.knnx(cbind(covariates$x, covariates$y), cbind(north_theft_locations_df$x, north_theft_locations_df$y), k = 1)$nn.index
north_theft_locations_df <- cbind(north_theft_locations_df, covariates[nearest_indices_north, ])

# Repeat for the South region
nearest_indices_south <- get.knnx(cbind(covariates$x, covariates$y), cbind(south_theft_locations_df$x, south_theft_locations_df$y), k = 1)$nn.index
south_theft_locations_df <- cbind(south_theft_locations_df, covariates[nearest_indices_south, ])

# Convert to ppp objects with marks (covariates)
north_theft_ppp <- spatstat.geom::ppp(x = north_theft_locations_df$x, 
                                      y = north_theft_locations_df$y, 
                                      window = spatstat.geom::owin(xrange = range(north_theft_locations_df$x), 
                                                                   yrange = range(north_theft_locations_df$y)),
                                      marks = north_theft_locations_df[, -c(1, 2)])

south_theft_ppp <- spatstat.geom::ppp(x = south_theft_locations_df$x, 
                                      y = south_theft_locations_df$y, 
                                      window = spatstat.geom::owin(xrange = range(south_theft_locations_df$x), 
                                                                   yrange = range(south_theft_locations_df$y)),
                                      marks = south_theft_locations_df[, -c(1, 2)])
