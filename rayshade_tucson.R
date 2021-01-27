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

tucson_r <- raster::raster("data/Sentinel2_Tucson/B04.tif")
tucson_g <- raster::raster("data/Sentinel2_Tucson/B03.tif")
tucson_b <- raster::raster("data/Sentinel2_Tucson/B02.tif")

tucson_rgb <- raster::stack(tucson_r, tucson_g, tucson_b)

tucson_rgb_corrected <- tucson_rgb ^ (1/1.75)
raster::plotRGB(tucson_rgb_corrected)


# coordinate reference systems
raster::crs(tucson_r)
raster::crs(tucson_elevation)

tucson_elevation_utm <- raster::projectRaster(tucson_elevation, 
                                              crs = crs(tucson_r),
                                              method = 'bilinear')

crs(tucson_elevation_utm)

e <- extent(tucson_rgb)
elevation_cropped <- raster::crop(tucson_elevation_utm, e)


names(tucson_rgb_corrected) <- c("r","g","b")

tucson_r_corrected <- rayshader::raster_to_matrix(tucson_rgb_corrected$r)
tucson_g_corrected <- rayshader::raster_to_matrix(tucson_rgb_corrected$g)
tucson_b_corrected <- rayshader::raster_to_matrix(tucson_rgb_corrected$b)

tucson_rgb_array <-
  array(0, dim = c(nrow(tucson_r_corrected), ncol(tucson_r_corrected), 3))

tucson_rgb_array[,,1] <- (tucson_r_corrected/255) # red
tucson_rgb_array[,,2] <- (tucson_g_corrected/255) # green
tucson_rgb_array[,,3] <- (tucson_b_corrected/255) # blue

tucson_rgb_array <- aperm(tucson_rgb_array, c(2,1,3))

tucson_rgb_contrast <- scales::rescale(tucson_rgb_array, to = c(0, 1))

plot_map(tucson_rgb_contrast)

# 
# plot_3d(elevation_cropped, 
#         windowsize = c(1100, 900),
#         zscale = 15, 
#         shadowdepth = -50,
#         zoom = 0.5,
#         phi = 45, 
#         theta = 45,
#         fov = 70, 
#         background = "#F2E1D0",
#         shadowcolor = "#523E2B")

elevation_cropped %>% 
  sphere_shade()



plot_3d(tucson_rgb_corrected,
        elevation_cropped)
