library(tidyverse)
library(rayshader)
library(geoviz)
library(sp)
library(raster)
library(scales)
library(here)


# Let’s load our data. We’ll start with loading the elevation dataset, since
# it’s an easier process. We’ll use the raster::raster() function to load our
# SRTM hgt files. This gives us two raster objects. We then combine the two with
# the raster::merge() function. Since they’re coming from the same data source,
# we don’t have to worry about transforming coordinate systems to match–they
# should just merge together seamlessly. We’ll plot the elevation using
# rayshader::height_shade() so you can see what it looks like:

# elevation1 = raster::raster("")
# elevation2 = raster::raster("~/Desktop/LC08_L1TP_038034_20191117_20191202_01_T1/N37W114.hgt")




elevation <- raster::raster(here('data', 'N32W112.hgt'))

height_shade(raster_to_matrix(elevation)) %>%
  plot_map()


# Great! Now, let’s load our georeferenced satellite imagery. The red, blue, and
# green bands on Landsat 8 data are bands B4, B3, and B2, respectively. You can
# delete the rest of the bands if you want to free 600 megabytes of space on
# your hard drive. Let’s then plot it with raster::plotRGB().


# pima_r <- raster::raster(here('data', 'LC08_L1TP_036038_20151124_20170225_01_T1_B4.TIF'))
# pima_g <- raster::raster(here('data', 'LC08_L1TP_036038_20151124_20170225_01_T1_B3.TIF'))
# pima_b <- raster::raster(here('data', 'LC08_L1TP_036038_20151124_20170225_01_T1_B2.TIF'))

pima_r <- raster::raster('data/T12SVA_20201215T180741_B04.jp2')
pima_g <- raster::raster('data/T12SVA_20201215T180741_B03.jp2')
pima_b <- raster::raster('data/T12SVA_20201215T180741_B02.jp2')


pima_rgb <- raster::stack(pima_r, pima_g, pima_b)

# raster::plotRGB(pima_rgb, scale=255^4)

# Why is it so dark? We need to apply a gamma correction to the the imagery,
# which corrects raw linear intensity data for our non-linear perception of
# darkness. We do that simply by taking the square root of the data (we can also
# remove the scale argument).


pima_rgb_corrected <- sqrt(pima_rgb)



# Our imagery data is given in UTM coordinates, while our elevation is in
# long/lat. Thankfully, it’s fairly straightforward to transform the elevation
# data from long/lat to UTM with the raster::projectRaster() function. We use
# the “bilinear” method for interpolation since elevation is a continuous
# variable.

elevation_utm <- raster::projectRaster(elevation, crs = crs(pima_r), method = "bilinear")



# let's crop the region down to the park itself.

top_right <- c(y=-111.07496, x=32.24374)
bottom_left <- c(y=-111.12577, x=32.20155) 



extent_latlong <- sp::SpatialPoints(rbind(bottom_left, top_right), proj4string = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
extent_utm <- sp::spTransform(extent_latlong, raster::crs(pima_rgb_corrected))

e_ll <- raster::extent(extent_latlong)

e <- raster::extent(extent_utm)
e


pima_rgb_cropped <- raster::crop(pima_rgb_corrected, e)

elevation_cropped <- raster::crop(elevation_utm, e)

elevation_cropped %>% 
  raster_to_matrix()

names(pima_rgb_cropped) <- c("r","g","b")

pima_r_cropped <- rayshader::raster_to_matrix(pima_rgb_cropped$r)
pima_g_cropped <- rayshader::raster_to_matrix(pima_rgb_cropped$g)
pima_b_cropped <- rayshader::raster_to_matrix(pima_rgb_cropped$b)




pima_el_matrix <- rayshader::raster_to_matrix(elevation_cropped)

pima_rgb_array <- array(0,dim=c(nrow(pima_r_cropped),ncol(pima_r_cropped),3))

pima_rgb_array[,,1] = pima_r_cropped/255 #Red layer
pima_rgb_array[,,2] = pima_g_cropped/255 #Blue layer
pima_rgb_array[,,3] = pima_b_cropped/255 #Green layer

pima_rgb_array = aperm(pima_rgb_array, c(2,1,3))




pima_rgb_contrast <- scales::rescale(pima_rgb_array, to = c(0, 1))






plot_3d(
  pima_rgb_contrast,
  pima_el_matrix,
  windowsize = c(2000, 2000),
  zscale = 15,
  shadowdepth = -10,
  zoom = 0.8,
  phi = 35,
  theta = -25,
  fov = 30,
  lineantialias = T,
  linewidth = 5
)

render_label(
  pima_el_matrix,
  lat = 32.223717,
  long = -111.101754,
  z = 750,
  zscale = 15,
  extent = e_ll,
  text = "Nov 22, 2015 <3",
  textsize = 2,
  linewidth = 5,
  antialias = T
)

render_snapshot(clear = T)
