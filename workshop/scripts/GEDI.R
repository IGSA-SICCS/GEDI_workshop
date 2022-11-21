

## Section 0: Libraries ---------
## Define libraries to use
libs <- c('raster', 'rGEDI', 'devtools')

## Install if aren't yet
installed_libs <- rownames(installed.packages())
sapply(libs, FUN = function(x){
  if (!x %in% installed_libs){
    install.packages(x)
  } 
})




## Loading

library(rGEDI)    # GEDI customized tools
library(raster)   # Spatial - raster operations
library(devtools) # Tools for sourcing codes from external functions
library(sf) # Spatial tools

# require R package dependencies
require(data.table)
require(lidR)
require(sf)
require(httr)


## Define folder paths
wd <- 'N:/Mi unidad/Phd/travels/GEDI workshop AGU 2022/' # Keep the last backslash: /
dir.create(wd, recursive = TRUE)
setwd(wd)
download_path <- paste0(wd, 'data/raw_h5_files/') # Keep the last backslash: /
dir.create(download_path, recursive = TRUE)

setwd(wd)

## Import some functions hosted in Github
source(paste0(wd, 'scripts/gediSim_functions.R'))
gedi_finder
# gedifinder is a web service provided by NASA
# usually the request takes more than 5 seconds




## Section 2: Dowload data ---------
# Specifying bounding box coordinates
ul_lat<- 1 # uper left lat - Y # ymax
ul_lon<- -75  # uper left lon - X # xmin
lr_lat<- 0 # lower rigth lat - Y # ymin
lr_lon<- -74 # lower rigth lat - Y # xmax

# Specifying the date range
daterange=c("2019-07-01","2020-05-22")


# using site_boundary to retrieve bbox coordinates

# site_boundary <- raster::shapefile('N:/Mi unidad/Phd/travels/GEDI workshop AGU 2022/data/aoi_5d_col.shp')
# site_bbox <- st_bbox(site_boundary)
site_bbox <- c(xmin = ul_lon, ymin = lr_lat,  xmax = lr_lon , ymax = ul_lat)

## Order of arguments: xmin ymin xmax ymax 
bbox <- paste(site_bbox[1], site_bbox[2], site_bbox[3], site_bbox[4], sep = ',')

# call the gedi_finder() function to locate overlapping GEDI footprint
# specifying GEDIL1B data product
product <- 'GEDI01_B.002'
granules <- gedi_finder(product, bbox)
found_orbits <- data.frame( url = grep('.h5$', unlist(granules), value = TRUE), 
                            stringsAsFactors = FALSE)

found_orbits$yyyydoy <- substr(start = 0, stop = 7, x = gsub('^.+_B_|_02_005_0.+.h5$', '', found_orbits$url))

found_orbits$yyyymmdd <- as.Date(sapply(found_orbits$yyyydoy, function(x){
  # x  = found_orbits$yyyydoy[1]
  as.character(as.Date(x = as.numeric(substr(x, start = 5, stop = 7)), 
          origin = paste0(substr(x, start = 0, stop = 4), "-01-01")))
}))


## Download the data:

str(found_orbits)
sapply(found_orbits$url, function(x){
  # x  = found_orbits$url[1]
  outName <- paste0(download_path, basename(x))
  # Download if doesn't exists
  if(!file.exists(outName)){
    download.file(url = x, destfile = outName)
  }
})
  
rGEDI::gediDownload(filepath = found_orbits$url, outdir = download_path)

if( is.null(gedi02b_list) ){
  load('GEDI_original_dataset.RData')
}


