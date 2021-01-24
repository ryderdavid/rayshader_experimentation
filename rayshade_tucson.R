library(tidyverse)
library(rayshader)
library(geoviz)
library(sp)
library(raster)
library(scales)
library(here)
library(RStoolbox) # for normImage()

# load height data: since it spans two tiles, need to load both
elevation1 <- raster::raster("data/height_data/N32W111.hgt")
elevation2 <- raster::raster("data/height_data/N32W112.hgt")

tucson_elevation <- raster::merge(elevation1, elevation2)

raster_to_matrix(tucson_elevation) %>% 
  height_shade() %>% 
  plot_map()

# 
r16 <- raster::raster("data/Sentinel2_Tucson/B04.TIF")
image_crs <- raster::crs(r16)

tucson_r <- raster::raster("data/Sentinel2_Tucson/B04_corrected.tiff")
tucson_g <- raster::raster("data/Sentinel2_Tucson/B03_corrected.tiff")
tucson_b <- raster::raster("data/Sentinel2_Tucson/B02_corrected.tiff")

# give CRS proj4string back to imagery brightened externally in Pixelmator Pro
# (stripped Geotiff headers)
raster::crs(tucson_r) <- image_crs
raster::crs(tucson_g) <- image_crs
raster::crs(tucson_b) <- image_crs

tucson_rgb <- raster::stack(tucson_r, tucson_g, tucson_b)

# elevation data is in longlat, need to convert to UTM
raster::crs(tucson_elevation)
raster::crs(tucson_r)

tucson_elevation_utm <- raster::projectRaster(tucson_elevation, 
                                              crs = image_crs, 
                                              method = "bilinear")

# crop our elevation to the extent of our image of Tucson -------------
e <- raster::extent(tucson_rgb)
elevation_cropped <- raster::crop(tucson_elevation_utm, e)
tucson_elevation_matrix <- rayshader::raster_to_matrix(elevation_cropped)

tucson_