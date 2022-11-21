# SCRIPT METADATA ==========================================================================================
# Description: R script to 
# Script Author: Melissa Rose
# Script Date: 11-02-2022
# Dataset Notes:  

# IMPORT ARGUMENTS ==========================================================================================
args <- commandArgs(trailingOnly=TRUE)

current_site <- args[1]
script_directory <- args[2]
lib_directory <- args[3]
processing_project_directory <- args[4]
gediFinder_directory <- args[5]

# SET UP ====================================================================================================
# set lib_directory
.libPaths(c(lib_directory, .libPaths()))

if ("data.table" %in% rownames(installed.packages()) == FALSE) { install.packages("data.table", destdir = lib_directory)}
if ("lidR" %in% rownames(installed.packages()) == FALSE) { install.packages("lidR", destdir = lib_directory)}
if ("sf" %in% rownames(installed.packages()) == FALSE) { install.packages("sf", destdir = lib_directory)}
if ("httr" %in% rownames(installed.packages()) == FALSE) { install.packages("httr", destdir = lib_directory)}

# require R package dependencies
require(data.table)
require(lidR)
require(sf)
require(httr)

#source gediSim function R script 
source(paste0(script_directory,"gediSim_functions.R"))

# DATA PREP =================================================================================================
# specifying GEDIL1B data product
product <- 'GEDI01_B.002'

# path to input_tablr
input_table <- paste0(processing_project_directory, 'input_table.csv')

# reading in the input table and subsetting to current_site 
tile_list <- data.table(read.csv(input_table))
site_tile_list <- tile_list[site == current_site]

# creating a vector of pathnames to all site .las/.laz files and creating an LAScatalog
LASfiles <- as.vector(site_tile_list$als_fullpath)
LAScatalog <- readLAScatalog(LASfiles)

# creating a site boundary geometry
site_boundary <- st_union(st_buffer(LAScatalog@data[["geometry"]], 10))

# using site_boundary to retrieve bbox coordinates
site_bbox <- st_bbox(st_transform(site_boundary, 4326))
bbox <- paste(site_bbox[1], site_bbox[2], site_bbox[3], site_bbox[4], sep = ',')

# call the gedi_finder() function to locate overlapping GEDI footprint
granules <- gedi_finder(product, bbox)
site_orbit_count <- length(granules)
print(sprintf("%s %s Version 2 granules found.", site_orbit_count, product))

# set up output textfile name using the current datetime
outName <- sprintf("%s_%s.txt", current_site, sub('.002', '_002', product))

# save textfile to gediFinder directory
write.table(granules, paste0(gediFinder_directory, outName), row.names = FALSE, col.names = FALSE, quote = FALSE, sep='\n')
print(sprintf("File containing links to intersecting %s Version 2 data has been saved to: %s/%s", product, gediFinder_directory, outName))

# update input_table with orbit_count for each site and save to temporary folder in gediSim_processing_directory
#temp_directory <- paste0(gediFinder_directory,"orbit_tables/")
#ifelse(!dir.exists(temp_directory), dir.create(temp_directory), FALSE)
#tile_list[site == current_site, orbit_count := site_orbit_count]
#write.csv(tile_list, paste0(temp_directory, site, '_table.csv'), row.names = FALSE, quote = FALSE, sep='\n')
