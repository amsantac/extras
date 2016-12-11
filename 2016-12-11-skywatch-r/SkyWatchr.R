
# Install the package from CRAN
install.packages("SkyWatchr")

# Load the package
library(SkyWatchr)

# Request an API key from http://www.skywatch.co/request-access

# Store your API key in an object
api_key <- readLines("api_key.txt")

# Now you can start sending queries to the SkyWatch API

# Most basic query: date and location
querySW(api_key, time_period = "2015-8,2016-9", longitude_latitude = "6.566358,3.367358,6.586358,3.387358")

# Search by instrument, data level processing, bands, ...
res <- querySW(api_key, time_period = "2015-06,2015-8", longitude_latitude = "31.321119,48.676074", 
               max_resolution = 100, max_cloudcover = 50)
View(res)

# Request output as html to facilitate browsing the results and clicking the download links
res <- querySW(api_key, time_period = "2015-8", longitude_latitude = "6.566358,3.367358,6.586358,3.387358", 
               output = "html")

# Use an Spatial object to define the search location
library(raster)
my_shp <- shapefile("data/study_area_latlon.shp")
res <- querySW(api_key, time_period = "2015-8", longitude_latitude = my_shp)

# extract boundary box of a given dataset
sppolygon <- getPolygon(res, 24)

library(mapview)
map <- mapView(sppolygon)
map + my_shp

# Download all the files retrieved after the query
downloadSW(res)

# Download only a subset of rows
downloadSW(res[c(274, 388), ])

# Download filtered datasets using an expression
downloadSW(res, source == "MOPITT" & size_kb < 2400)
