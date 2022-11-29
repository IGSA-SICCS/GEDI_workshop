gedi_finder <- function(product, bbox) {
  
  #Reference: https://git.earthdata.nasa.gov/projects/LPDUR/repos/gedi-finder-tutorial-r/browse/GEDI_Finder.R
  
  #Define the base CMR granule search url, including LPDAAC provider name and max page size (2000 is the max allowed)
  cmr <- "https://cmr.earthdata.nasa.gov/search/granules.json?pretty=true&provider=LPDAAC_ECS&page_size=2000&concept_id="
  
  #Set up dictionary where key is GEDI shortname + version and value is CMR Concept ID
  concept_ids <- list('GEDI01_B.002'='C1908344278-LPDAAC_ECS', 
                      'GEDI02_A.002'='C1908348134-LPDAAC_ECS', 
                      'GEDI02_B.002'='C1908350066-LPDAAC_ECS')
  
  #CMR uses pagination for queries with more features returned than the page size
  page <- 1
  bbox <- sub(' ', '', bbox)  # Remove any white spaces
  granules <- list()          # Set up a list to store and append granule links to
  
  #Send GET request to CMR granule search endpoint w/ product concept ID, bbox & page number
  cmr_response <- GET(sprintf("%s%s&bounding_box=%s&pageNum=%s", cmr, concept_ids[[product]],bbox,page))
  
  #Verify the request submission was successful
  if (cmr_response$status_code==200){
    
    # Send GET request to CMR granule search endpoint w/ product concept ID, bbox & page number, format return as a list
    cmr_response <- content(GET(sprintf("%s%s&bounding_box=%s&pageNum=%s", cmr, concept_ids[[product]],bbox,page)))$feed$entry
    
    # If 2000 features are returned, move to the next page and submit another request, and append to the response
    while(length(cmr_response) %% 2000 == 0 && length(cmr_response) != 0){
      page <- page + 1
      cmr_response <- c(cmr_response, content(GET(sprintf("%s%s&bounding_box=%s&pageNum=%s", cmr, concept_ids[[product]],bbox,page)))$feed$entry)
    }
    
    # CMR returns more info than just the Data Pool links, below use for loop to go through each feature, grab DP link, and add to list
    for (i in 1:length(cmr_response)) {
      granules[[i]] <- cmr_response[[i]]$links[[1]]$href
    }
    
    # Return the list of links
    print(sprintf("Found %s %s overlapping orbits found.", length(granules), product))
    return(granules)
    
  } else {
    
    # If the request did not complete successfully, print out the response from CMR
    print(content(GET(sprintf("%s%s&bounding_box=%s&pageNum=%s", cmr, concept_ids[[product]],bbox,page)))$errors)
  }
}

gedi_temp_filter <- function(granules, product, daterange){
  
  #defining start and end dates
  start_date <- daterange[1]
  end_date <- daterange[2]
  
  #formatting dates from granules list
  found_orbits <- data.frame(url = grep('.h5$', unlist(granules), value = TRUE), stringsAsFactors = FALSE)
  if (product == 'GEDI02_A.002'){
    found_orbits$yyyydoy <- substr(start = 0, stop = 7, x = gsub('^.+_A_|_02_005_0.+.h5$', '', found_orbits$url))
  }else{
    found_orbits$yyyydoy <- substr(start = 0, stop = 7, x = gsub('^.+_B_|_02_005_0.+.h5$', '', found_orbits$url))}
  found_orbits$yyyymmdd <- as.Date(sapply(found_orbits$yyyydoy, function(x){
    as.character(as.Date(x = as.numeric(substr(x, start = 5, stop = 7)),
                         origin = paste0(substr(x, start = 0, stop = 4), "-01-01")))
  }))
  
  #filtering to orbits within date range
  found_orbits <- found_orbits[found_orbits$yyyymmdd >= as.Date(daterange[1]) & found_orbits$yyyymmdd <= as.Date(daterange[2]), ]
  orbit_list <- found_orbits$url
  print(sprintf("Found %s %s overlapping orbits from %s to %s.", length(orbit_list), product, start_date, end_date))
  
  return(orbit_list)
  
}

#Reference: https://github.com/carlos-alberto-silva/rGEDI

gedi_download<-function(filepath, outdir = NULL, overwrite = FALSE, buffer_size = 512, timeout=10){
  if (is.null(outdir)) {
    outdir == tempdir()
  }
  stopifnotMessage(
    "outdir is not a valid path" = checkParentDir(outdir),
    "overwrite is not logical" = checkLogical(overwrite),
    "buffer_size is not an integer" = checkInteger(buffer_size)
  )
  buffer_size = as.integer(buffer_size)
  netrc = getNetRC(outdir)
  
  files<-filepath
  n_files = length(files)
  
  # Download all files in filepath vector
  for (i in 1:n_files) {
    url = files[i]
    message("------------------------------")
    message(sprintf("Downloading file %d/%d: %s", i, n_files, basename(url)))
    message("------------------------------")
    
    if (gediDownloadFile(
      url,
      outdir,
      overwrite,
      buffer_size,
      netrc,
      timeout
    ) == 0) {
      message("Finished successfully!")
    } else {
      stop(sprintf("File %s has not been downloaded properly!", basename(url)))
    }
  }
}

gediDownloadFile = function(url, outdir, overwrite, buffer_size, netrc, timeout) {
  filename <- file.path(outdir, basename(url)) # Keep original filename
  if((! overwrite) && file.exists(filename)) {
    message("Skipping this file, already downloaded!")
    return(0)
  } # SKip if already downloaded
  
  # Temporary to file to resume to
  resume=paste0(filename, ".curltmp")
  if(file.exists(resume)){
    resume_from=file.info(resume)$size # Get current size to resume from
  } else {
    resume_from=0
  }
  
  # Connection config
  h = curl::new_handle()
  curl::handle_setopt(h, netrc=1, netrc_file=netrc, resume_from=resume_from, connecttimeout=timeout)
  
  tryCatch({
    fileHandle=file(resume, open="ab", raw = T)
    message("Connecting...")
    conn = tryCatch(curl::curl(url, handle=h, open="rb"), error = function(e) {
      file.remove(netrc)
      stop(e)
    })
    message("Connected successfully, downloading...")
    headers=rawToChar(curl::handle_data(h)$headers)
    total_size=as.numeric(gsub("[^\u00e7]*Content-Length: ([0-9]+)[^\u00e7]*","\\1",x=headers, perl = T))
    while(TRUE) {
      message(sprintf("\rDownloading... %.2f/%.2fMB (%.2f%%)    ",
                      resume_from/1024.0/1024.0,
                      total_size/1024.0/1024.0,
                      100.0*resume_from/total_size),
              appendLF=FALSE)
      data = readBin(conn, what = raw(), n = 1024*buffer_size)
      size = length(data)
      if (size==0) {
        break
      }
      writeBin(data, fileHandle, useBytes = T)
      resume_from = resume_from + size
    }
    message(sprintf("\rDownloading... %.2f/%.2fMB (100%%)    ",
                    total_size/1024.0/1024.0,
                    total_size/1024.0/1024.0))
    close(fileHandle)
    close(conn)
    file.rename(resume, filename)
    return(0)
  }, interrupt=function(e){
    warning("\nDownload interrupted!!!")
    try(close(conn), silent = TRUE)
    try(close(fileHandle), silent = TRUE)
  }, finally = {
    try(close(conn), silent = TRUE)
    try(close(fileHandle), silent = TRUE)
  })
  return(-1)
}

getNetRC = function(dl_dir) {
  netrc <- file.path(dl_dir,'.netrc')  # Path to netrc file
  # ------------------------------------CREATE .NETRC FILE------------------------------------------ #
  if (file.exists(netrc) == FALSE || any(grepl("urs.earthdata.nasa.gov", readLines(netrc))) == FALSE) {
    netrc_conn <- file(netrc)
    
    # User will be prompted for NASA Earthdata Login Username and Password below
    writeLines(c("machine urs.earthdata.nasa.gov",
                 sprintf("login %s", getPass::getPass(msg = "Enter NASA Earthdata Login Username \n (or create an account at urs.earthdata.nasa.gov) :")),
                 sprintf("password %s", getPass::getPass(msg = "Enter NASA Earthdata Login Password:"))), netrc_conn)
    close(netrc_conn)
    message("A .netrc file with your Earthdata Login credentials was stored in the output directory ")
  }
  return (netrc)
}

stopifnotMessage = function(...) {
  ok = TRUE
  errors = list()
  listargs = list(...)
  for (i in 1:length(listargs)) {
    if (listargs[i] == FALSE) {
      errors[[""]] = names(listargs)[i]
      ok = FALSE
    }
  }
  if (ok == FALSE) {
    stop(paste0("\n\nWhen validating the arguments:\n    ", paste(errors, collapse="\n    ")))
  }
}

checkNumericLength = function(x, len) {
  return (is.null(x) || (length(x) == len && is.numeric(x)))
}

checkNumeric = function(x) {
  return (checkNumericLength(x, 1))
}

checkLogical = function(x) {
  return (is.null(x) || (length(x) == 1 && is.logical(x)))
}

checkInteger = function(x) {
  x_int = as.integer(x)
  return (is.null(x) || (length(x_int) == 1 && is.integer(x_int) && !is.na(x_int)))
}

checkCharacter = function(x) {
  return (is.null(x) || (length(x) == 1 && is.character(x)))
}

checkFilepath = function(x, newFile=TRUE, optional = TRUE) {
  exists = TRUE
  if (is.null(x)) {
    if (optional)
      return (TRUE)
    else
      return (FALSE)
  }
  if (!is.character(x) || length(x) != 1)
    return (FALSE)
  
  
  if (!newFile)
    return (file.exists(x))
  
  return (TRUE)
}

checkParentDir = function(x, optional=FALSE) {
  if (optional && is.null(x)) {
    return (TRUE)
  }
  dirName = fs::path_dir(x)
  return (fs::dir_exists(dirName)[[1]])
}

inputOrInList = function(input) {
  inList=NULL
  if (length(input) > 1) {
    inList = tempfile(fileext=".txt")
    fileHandle = file(inList, "w")
    writeLines(input, fileHandle)
    close(fileHandle)
    return (list(NULL, inList))
  }
  return (list(input, NULL))
}

cleanInList = function(x) {
  if (!is.null(x[[2]]) && file.exists(x[[2]])) {
    file.remove(x[[2]])
  }
}

#setClass("gedi.level1b", representation(h5="H5File",level1b.spdf='SpatialPointsDataFrame'))
#' @importFrom hdf5r H5File
setRefClass("H5File")
requireNamespace("data.table")

#' Class for GEDI level1B
#'
#' @slot h5 Object of class H5File from \emph{hdf5r} package containing the
#'GEDI level1B products: geolocated Waveforms
#'
#' @seealso \code{\link[hdf5r:H5File-class]{hdf5r::H5File}} in the \emph{hdf5r} package and
#'https://lpdaac.usgs.gov/products/gedi01_bv001/
#'
#' @import methods
#' @export
gedi.level1b <- setClass(
  Class="gedi.level1b",
  slots = list(h5 = "H5File")
)

#' Class for GEDI level2A
#'
#' @slot h5 Object of class H5File from \emph{hdf5r} package containing the
#'GEDI level2A products: ground elevation, canopy top height, and relative heights (RH).
#'
#' @seealso \code{\link[hdf5r:H5File-class]{hdf5r::H5File}} in the \emph{hdf5r} package and
#'https://lpdaac.usgs.gov/products/gedi02_av001/
#'
#' @import methods
#' @export
gedi.level2a <- setClass(
  Class="gedi.level2a",
  slots = list(h5 = "H5File")
)

#' Class for GEDI level2B
#'
#' @slot h5 Object of class H5File from \emph{hdf5r} package containing the
#'GEDI level2B products: canopy cover, Plant Area Index (PAI), Plant Area Volume Density (PAVD),
#'and Foliage Height Diversity (FHD).
#'
#' @seealso \code{\link[hdf5r:H5File-class]{hdf5r::H5File}} in the \emph{hdf5r} package and
#'https://lpdaac.usgs.gov/products/gedi02_bv001/
#'
#' @import methods
#' @export
gedi.level2b <- setClass(
  Class="gedi.level2b",
  slots = list(h5 = "H5File")
)

#' Class for GEDI Full-Waveform Simulation
#'
#' @slot h5 Object of class H5File from \emph{hdf5r} package package containing the simulated
#' GEDI full-waveform
#'
#' @seealso
#' i) Hancock, S., Armston, J., Hofton, M., Sun, X., Tang, H., Duncanson, L.I., Kellner,
#' J.R. and Dubayah, R., 2019. The GEDI simulator: A large-footprint waveform lidar simulator
#' for calibration and validation of spaceborne missions. Earth and Space Science.
#' https://doi.org/10.1029/2018EA000506
#'
#' ii) gediSimulator: https://bitbucket.org/StevenHancock/gedisimulator/src/master/
#'
#' @import methods
#' @export
gedi.level1bSim <- setClass(
  Class="gedi.level1bSim",
  slots = list(h5 = "H5File")
)

#' Class for GEDI level1B Full Waveform
#'
#' @slot dt Object of class data.table from \emph{data.table} package containing
#' the extracted GEDI full-waveform elevation and amplitude.
#'
#' @import methods
#' @export
gedi.fullwaveform <- setClass(
  Class="gedi.fullwaveform",
  slots = list(dt = "data.table")
)


#'Plot GEDI* object
#'
#'@param x An object of class "gedi.fullwaveform". (output of \code{\link[rGEDI:getLevel1BWF]{getLevel1BWF}} function)
#'@param y not used (inherited from R base)
#'@param ... will be passed to the main plot
#'
#'@param relative if TRUE, the Waveform Amplitude will be showed in percentage (\%)
#'@param polygon if TRUE, the polygon will be added to the plot
#'
#'@param method methods used for simulating the GEDI full-waveform ("RXWAVEINT","RXWAVEINT" or "RXWAVEINT"). Default is "RXWAVECOUNT".
#'@return No return value
#'
#' @export
#' @method plot gedi.fullwaveform
setGeneric("plot", function(x, y, ...)
  standardGeneric("plot"))

#'@description For gedi.fullwaveform: will plot the full waveform\cr\cr
#'@examples
#'# Specifying the path to GEDI level1B data (zip file)
#'outdir = tempdir()
#'level1B_fp_zip <- system.file("extdata",
#'                   "GEDI01_B_2019108080338_O01964_T05337_02_003_01_sub.zip",
#'                   package="rGEDI")
#'
#'# Unzipping GEDI level1B data
#'level1Bpath <- unzip(level1B_fp_zip,exdir = outdir)
#'
#'# Reading GEDI level1B data (h5 file)
#'level1b<-readLevel1B(level1Bpath=level1Bpath)
#'
#'# Extracting GEDI Full-Waveform
#'wf <- getLevel1BWF(level1b, shot_number="19640521100108408")
#'
#'# Plotting GEDI Full-waveform
#'oldpar<-par()
#'par(mfrow = c(1,2), cex.axis = 1.5)
#'plot(wf, relative=FALSE, polygon=TRUE, type="l", lwd=2, col="forestgreen",
#'xlab="", ylab="Elevation (m)")
#'
#'plot(wf, relative=TRUE, polygon=TRUE, type="l", lwd=2, col="forestgreen",
#'xlab="Waveform Amplitude (%)", ylab="Elevation (m)")
#'
#'par(oldpar) 
#'close(level1b)
#' @rdname plot
setMethod("plot", signature("gedi.fullwaveform", y = "missing"), function(x,relative=FALSE,polygon=FALSE,...) {
  
  if (!class(x)=="gedi.fullwaveform"){
    
    print("Invalid input file. It should be an object of class 'gedi.fullwaveform' ")
  } else {
    
    
    x0<-as.data.frame(x@dt)
    x<-x0[,1]
    z<-x0[,2]
    
    if (relative==TRUE){
      x=c(x-min(x))/(max(x)-min(x))*100
    } else{
      x=x
    }
    
    if (polygon==TRUE){
      
      xstart<-x[which(z==min(z, na.rm=T))]
      xend<-x[which(z==max(z, na.rm=T))]
      
      xl<-c(min(x),min(x),xstart,rev(x),xend,min(x))
      yl<-c(max(z, na.rm=T),min(z, na.rm=T),min(z, na.rm=T),rev(z),max(z, na.rm=T),max(z, na.rm=T))
      
      suppressWarnings({plot(xl,yl,...)})
      suppressWarnings({polygon(xl,yl,...)})
    } else {
      suppressWarnings({plot(x=x,y=z,...)})
    }
  }
})


#'@description for gedi.level1bSim: will plot the simulated waveform
#'
#'@examples
#'outdir <- tempdir()
#' 
#'zipfile_amazon <- system.file("extdata", "Amazon.zip", package="rGEDI")
#'zipfile_Savanna <- system.file("extdata", "Savanna.zip", package="rGEDI")
#'
# specify the path to ALS data
#'lasfile_amazon <- unzip(zipfile_amazon,exdir=outdir)
#'lasfile_Savanna <- unzip(zipfile_Savanna,exdir=outdir)
#'
#'# Reading and plot ALS file
#'libsAvailable = require(lidR) && require(plot3D)
#'if (libsAvailable) {
#'las_amazon<-readLAS(lasfile_amazon)
#'las_Savanna<-readLAS(lasfile_Savanna)
#'
#'# Extracting plot center geolocations
#'xcenter_amazon = mean(las_amazon@bbox[1,])
#'ycenter_amazon = mean(las_amazon@bbox[2,])
#'xcenter_Savanna = mean(las_Savanna@bbox[1,])
#'ycenter_Savanna = mean(las_Savanna@bbox[2,])
#'
#'# Simulating GEDI full-waveform
#'wf_amazon<-gediWFSimulator(
#'                           input=lasfile_amazon,
#'                           output=file.path(
#'                                         outdir,
#'                                         "gediWF_amazon_simulation.h5"
#'                                         ),
#'                           coords = c(xcenter_amazon, ycenter_amazon))
#' wf_Savanna<-gediWFSimulator(
#'                             input=lasfile_Savanna,
#'                             output=file.path(
#'                                           outdir,
#'                                           "gediWF_Savanna_simulation.h5"
#'                                           ),
#'                             coords = c(xcenter_Savanna, ycenter_Savanna))
#'# Plot Full-waveform
#'par(mfrow=c(2,2), mar=c(4,4,0,0), oma=c(0,0,1,1),cex.axis = 1.2)
#'scatter3D(
#'          las_amazon@data$X,
#'          las_amazon@data$Y,
#'          las_amazon@data$Z,
#'          pch = 16, colkey = FALSE, main="",
#'          cex = 0.5, bty = "u", col.panel ="gray90",
#'          phi = 30, alpha=1, theta=45, col.grid = "gray50",
#'          xlab="UTM Easting (m)", ylab="UTM Northing (m)", zlab="Elevation (m)"
#'          )
#'
#'plot(wf_amazon, relative=TRUE, polygon=TRUE, type="l", lwd=2, col="forestgreen",
#'     xlab="", ylab="Elevation (m)", ylim=c(90,140))
#'grid()
#'scatter3D(
#'          las_Savanna@data$X,las_Savanna@data$Y,las_Savanna@data$Z,
#'          pch = 16,colkey = FALSE, main="",
#'          cex = 0.5,bty = "u",col.panel ="gray90",
#'          phi = 30,alpha=1,theta=45,col.grid = "gray50",
#'          xlab="UTM Easting (m)", ylab="UTM Northing (m)", zlab="Elevation (m)"
#'          )
#'
#'plot(wf_Savanna, relative=TRUE, polygon=TRUE, type="l", lwd=2, col="green",
#'     xlab="Waveform Amplitude (%)", ylab="Elevation (m)", ylim=c(815,835))
#'grid()
#'
#' close(wf_amazon)
#' close(wf_Savanna)
#'}
#' @rdname plot
setMethod("plot", signature("gedi.level1bSim", y = "missing"), function(x,relative=FALSE,polygon=FALSE,method="RXWAVEINT",...) {
  
  if (!class(x)=="gedi.level1bSim"){
    
    print("Invalid input file. It should be an object of class 'gedi.fullwaveform' ")
  } else {
    
    wfh5<-x@h5
    z = seq(wfh5[["Z0"]][],wfh5[["ZN"]][], length.out = wfh5[["NBINS"]][])
    
    if  (method=="RXWAVEINT") { x0<-wfh5[["RXWAVEINT"]][,]}
    if  (method=="RXWAVECOUNT") { x0<-wfh5[["RXWAVECOUNT"]][,]}
    if  (method=="RXWAVEFRAC") { x0<-wfh5[["RXWAVEFRAC"]][,]}
    
    x<-x0
    
    if (relative==TRUE){
      x=c(x-min(x))/(max(x)-min(x))*100
    } else{
      x=x
    }
    
    if (polygon==TRUE){
      
      xstart<-x[which(z==min(z, na.rm=T))]
      xend<-x[which(z==max(z, na.rm=T))]
      
      xl<-c(min(x),min(x),xstart,rev(x),xend,min(x))
      yl<-c(max(z, na.rm=T),min(z, na.rm=T),min(z, na.rm=T),rev(z),max(z, na.rm=T),max(z, na.rm=T))
      
      suppressWarnings({plot(xl,yl,...)})
      suppressWarnings({polygon(xl,yl,...)})
    } else {
      suppressWarnings({plot(x=x,y=z,...)})
    }
  }
})

h5closeall = function(con, ...) {
  try(con@h5$close_all(), silent=TRUE)
}


#'Close hdf5 connections from gedi* objects
#'
#' @description 
#' Closing files will avoid locking HDF5 GEDI files.
#' 
#'@param con An object of class gedi*
#'@param ... Inherited from base
#'
#' @export
#' @rdname close
#' @method close gedi.level1b
setGeneric("close", function(con, ...)
  standardGeneric("close"))

#' Handles the \link[rGEDI:gedi.level1bSim-class]{\code{gedi.level1b}}.
#'@rdname close
setMethod("close", signature = c("gedi.level1b"), h5closeall)
#' Handles the \link[rGEDI:gedi.level2a-class]{\code{gedi.level2a}}.
#'@rdname close
setMethod("close", signature = c("gedi.level2a"), h5closeall)
#' Handles the \link[rGEDI:gedi.level2b-class]{\code{gedi.level2b}}.
#'@rdname close
setMethod("close", signature = c("gedi.level2b"), h5closeall)
#' Handles the \link[rGEDI:gedi.level1bSim-class]{\code{gedi.level1bSim}}.
#'@rdname close
setMethod("close", signature = c("gedi.level1bSim"), h5closeall)


readLevel1B <-function(level1Bpath) {
  level1b_h5 <- hdf5r::H5File$new(level1Bpath, mode = 'r')
  level1b<- new("gedi.level1b", h5 = level1b_h5)
  return(level1b)
}

readLevel2A <-function(level2Apath) {
  level2a_h5 <- hdf5r::H5File$new(level2Apath, mode = 'r')
  level2a<- new("gedi.level2a", h5 = level2a_h5)
  return(level2a)
}

readLevel2B <-function(level2Bpath) {
  level2b_h5 <- hdf5r::H5File$new(level2Bpath, mode = 'r')
  level2b<-new("gedi.level2b", h5 = level2b_h5)
  return(level2b)
}

getLevel1BGeo<-function(level1b,select=c("elevation_bin0", "elevation_lastbin")) {
  
  select<-unique(c("latitude_bin0", "latitude_lastbin", "longitude_bin0", "longitude_lastbin","shot_number",select))
  level1b<-level1b@h5
  
  datasets<-hdf5r::list.datasets(level1b, recursive = T)
  datasets_names<-basename(datasets)
  
  selected<-datasets_names %in% select
  
  for ( i in select){
    if  ( i =="shot_number"){
      assign(i,bit64::as.integer64(NaN))
    } else {
      assign(i,numeric())
    }
  }
  
  dtse2<-datasets[selected][!grepl("geolocation/shot_number",datasets[selected])]
  
  
  # Set progress bar
  pb <- utils::txtProgressBar(min = 0, max = length(dtse2), style = 3)
  i.s=0
  
  for ( i in dtse2){
    i.s<-i.s+1
    utils::setTxtProgressBar(pb, i.s)
    name_i<-basename(i)
    if ( name_i =="shot_number"){
      assign(name_i, bit64::c.integer64(get(name_i),level1b[[i]][]))
    } else {
      assign(name_i, c(get(name_i), level1b[[i]][]))
    }
  }
  
  level1b.dt<-data.table::data.table(as.data.frame(get("shot_number")[-1]))
  select2<-select[!select[]=="shot_number"]
  
  for ( i in select2){
    level1b.dt[,i]<-get(i)
  }
  
  colnames(level1b.dt)<-c("shot_number",select2)
  close(pb)
  return(level1b.dt)
}

clipLevel1B = function(level1b, xmin, xmax, ymin, ymax, output=""){
  output = checkOutput(output)
  checkClipExtentInputs(level1b, "gedi.level1b", xmin, xmax, ymin, ymax)
  
  # Get all spatial data as a list of dataframes with spatial information
  spData = getSpatialData1B(level1b)
  
  masks = clipSpDataByExtentLevelB(spData, xmin, xmax, ymin, ymax)
  
  newFile = clipByMask1B(level1b,
                         masks,
                         output)
  output = newFile@h5$filename
  close(newFile)
  result = readLevel1B(output)
  
  return (result)
}

clipLevel1BGeometry = function(level1b, polygon_spdf, output="", split_by=NULL) {
  output = checkOutput(output)
  checkClipGeoInputs(level1b, "gedi.level1b", polygon_spdf, split_by)
  
  spData = getSpatialData1B(level1b)
  
  xmin = polygon_spdf@bbox[1,1]
  xmax = polygon_spdf@bbox[1,2]
  ymin = polygon_spdf@bbox[2,1]
  ymax = polygon_spdf@bbox[2,2]
  
  masks = clipSpDataByExtentLevelB(spData, xmin, xmax, ymin, ymax)
  polygon_masks = getPolygonMaskLevelB(spData, masks, polygon_spdf, split_by)
  results = clipByMasks(level1b, polygon_masks, output, split_by, clipByMask1B)
  
  return (results)
}


# Helper function to return spatial data within a dataframe
getSpatialData1B = function(level1b) {
  level1b.h5<-level1b@h5
  groups_id<-grep("BEAM\\d{4}$",gsub("/","",
                                     hdf5r::list.groups(level1b.h5, recursive = F)), value = T)
  
  beams_spdf = list()
  
  for ( i in gsub("/","",groups_id)){
    beams_spdf[[i]] = data.frame(
      latitude_bin0=level1b.h5[[paste0(i,"/geolocation/latitude_bin0")]][],
      latitude_lastbin=level1b.h5[[paste0(i,"/geolocation/latitude_lastbin")]][],
      longitude_bin0=level1b.h5[[paste0(i,"/geolocation/longitude_bin0")]][],
      longitude_lastbin=level1b.h5[[paste0(i,"/geolocation/longitude_lastbin")]][]
    )
  }
  
  return (beams_spdf)
}

clipByMask1B = function(level1b, masks, output = "") {
  newFile =  hdf5r::H5File$new(output, mode="w")
  if(length(masks) == 0) {
    message("No intersection found!")
    newFile$close_all()
    newFile = hdf5r::H5File$new(output, mode="r")
    result = new("gedi.level1b", h5 = newFile)
    return(result)
  }
  
  for (attr in hdf5r::list.attributes(level1b@h5)) {
    hdf5r::h5attr(newFile, attr) = hdf5r::h5attr(level1b@h5, attr)
  }
  
  all_groups = hdf5r::list.groups(level1b@h5)
  beams_with_value = sapply(masks, length)>0
  beams_with_value = names(which(beams_with_value))
  beams_with_value = c(beams_with_value, "METADATA")
  which_groups = gsub("([^/]*).*","\\1",all_groups) %in% beams_with_value
  groups_with_value = all_groups[which_groups]
  
  # Setup progress bar
  all_datasets = hdf5r::list.datasets(level1b@h5)
  which_datasets = gsub("([^/]*).*","\\1",all_datasets) %in% beams_with_value
  datasets_with_value = all_datasets[which_datasets]
  
  total = length(datasets_with_value)
  pb = utils::txtProgressBar(min = 0, max = total, style = 3)
  progress = 0
  
  for (group in groups_with_value) {
    beam_id = strsplit(group, "/")[[1]][1]
    mask = masks[[beam_id]]
    mask_size = length(mask)
    
    hdf5r::createGroup(newFile, group)
    createAttributesWithinGroup(level1b@h5, newFile, group)
    
    if(beam_id != "METADATA") {
      beam_shot_n = level1b@h5[[beam_id]][["shot_number"]]$dims
      total_waveforms = list()
      total_waveforms[["rx"]] = sum(level1b@h5[[beam_id]][["rx_sample_count"]][])
      total_waveforms[["tx"]] = sum(level1b@h5[[beam_id]][["tx_sample_count"]][])
    }
    
    
    for (dt in hdf5r::list.datasets(level1b@h5[[group]], recursive = FALSE, full.names = T)) {
      if (grepl("[rt]x_sample_start_index$", dt)) {
        progress = progress + 1
        utils::setTxtProgressBar(pb, progress)
        next
      }
      h5_dt = level1b@h5[[dt]]
      dt_dim = h5_dt$dims
      dtype = h5_dt$get_type()
      if (is.na(all(h5_dt$chunk_dims))) {
        chunkdims = NULL
      } else {
        chunkdims = h5_dt$chunk_dims
      }
      
      
      if (length(dt_dim) == 1) {
        if (dt_dim == 1) {
          hdf5r::createDataSet(newFile,dt,h5_dt[], dtype=dtype, chunk_dim=chunkdims)
        } else if (dt_dim == beam_shot_n) {
          hdf5r::createDataSet(newFile,dt,h5_dt[mask], dtype=dtype, chunk_dim=chunkdims)
        } else if (dt_dim %in% total_waveforms | dt_dim %% beam_shot_n == 0) {
          prefix = ifelse(substr(basename(dt),1,2)=="rx", "rx", "tx")
          sampleCount = sprintf("%s_sample_count", prefix)
          sampleStartIndex = sprintf("%s_sample_start_index", prefix)
          countPath=file.path(beam_id, sampleCount, fsep = "/")
          startIdxPath=file.path(beam_id, sampleStartIndex, fsep = "/")
          
          counts = level1b@h5[[countPath]][mask]
          startIdx = level1b@h5[[startIdxPath]][mask]
          v.seq = Vectorize(seq.default,vectorize.args = c("from", "length.out"), SIMPLIFY=T)
          mask_list_waveform=v.seq(startIdx, length.out=counts)
          mask_waveform=Reduce(c, mask_list_waveform)
          newStartIdx = c(1,cumsum(counts)+1)[-mask_size-1]
          newFile$create_dataset(
            name       = startIdxPath,
            robj       = newStartIdx,
            dims       = length(newStartIdx),
            chunk_dims = level1b@h5[[startIdxPath]]$chunk_dims,
            dtype      = level1b@h5[[startIdxPath]]$get_type())
          
          total_size = length(mask_waveform)
          chunk_part = 1
          dt_res=hdf5r::createDataSet(newFile, dt, dtype=dtype, chunk_dim=chunkdims, dims=total_size)
          while (chunk_part < total_size) {
            end = chunk_part+chunkdims-1
            if (end > total_size) {
              end = total_size
            }
            get_part = mask_waveform[(chunk_part):(end)]
            dt_res[chunk_part:end] =  h5_dt[get_part]
            chunk_part = end+1
          }
        }
      } else if (length(dt_dim) == 2 && dt_dim[1] == beam_shot_n) {
        if (length(mask) == 1) {
          chunkdims = chunkdims[[2]]
        }
        hdf5r::createDataSet(newFile,dt,level1b@h5[[dt]][mask,], dtype=dtype, chunk_dim=chunkdims)
      } else {
        stop(paste0("Don't know how to handle dataset: ", dt, "\nContact the maintainer of the package!"))
      }
      
      #Update progress
      progress = progress + 1
      utils::setTxtProgressBar(pb, progress)
    }
  }
  
  newFile$close_all()
  newFile =  hdf5r::H5File$new(output, mode="r")
  result = new("gedi.level1b", h5 = newFile)
  close(pb)
  #spatial = level1B2dt(level1b)
  return (result)
}

clipLevel2A = function(level2a, xmin, xmax, ymin, ymax, output=""){
  output = checkOutput(output)
  checkClipExtentInputs(level2a, "gedi.level2a", xmin, xmax, ymin, ymax)
  
  # Get all spatial data as a list of dataframes with spatial information
  spData = getSpatialData2A(level2a)
  
  masks = clipSpDataByExtentLevel2A(spData, xmin, xmax, ymin, ymax)
  
  newFile = clipByMask2A(level2a,
                         masks,
                         output)
  output = newFile@h5$filename
  close(newFile)
  result = readLevel2A(output)
  
  return (result)
}


clipLevel2AGeometry = function(level2a, polygon_spdf, output="", split_by = NULL) {
  output = checkOutput(output)
  checkClipGeoInputs(level2a, "gedi.level2a", polygon_spdf, split_by)
  
  spData = getSpatialData2A(level2a)
  
  xmin = polygon_spdf@bbox[1,1]
  xmax = polygon_spdf@bbox[1,2]
  ymin = polygon_spdf@bbox[2,1]
  ymax = polygon_spdf@bbox[2,2]
  
  masks = clipSpDataByExtentLevel2A(spData, xmin, xmax, ymin, ymax)
  
  polygon_masks = getPolygonMaskLevel2A(spData, masks, polygon_spdf, split_by)
  
  results = clipByMasks(level2a, polygon_masks, output, split_by, clipByMask2A)
  
  return (results)
}


# Helper function to return spatial data within a dataframe
getSpatialData2A = function(level2a) {
  level2a.h5<-level2a@h5
  groups_id<-grep("BEAM\\d{4}$",gsub("/","",
                                     hdf5r::list.groups(level2a.h5, recursive = F)), value = T)
  
  beams_spdf = list()
  
  for ( i in gsub("/","",groups_id)){
    algs_coordinates = list()
    
    
    n_algs = level2a.h5[[i]][["ancillary/l2a_alg_count"]][1]
    
    
    beams_spdf[[i]] = algs_coordinates
    
    for (j in 1:n_algs) {
      beams_spdf[[i]][[j]] = data.frame(
        latitude_highest=level2a.h5[[paste0(i,"/geolocation/lat_highestreturn_a", j)]][],
        latitude_lowest=level2a.h5[[paste0(i,"/geolocation/lat_lowestreturn_a", j)]][],
        longitude_highest=level2a.h5[[paste0(i,"/geolocation/lon_highestreturn_a", j)]][],
        longitude_lowest=level2a.h5[[paste0(i,"/geolocation/lon_lowestreturn_a", j)]][]
      )
    }
    
    beams_spdf[[i]][["main"]] = data.frame(
      latitude_highest=level2a.h5[[i]][["lat_highestreturn"]][],
      latitude_lowest=level2a.h5[[i]][["lat_lowestmode"]][],
      longitude_highest=level2a.h5[[i]][["lon_highestreturn"]][],
      longitude_lowest=level2a.h5[[i]][["lon_lowestmode"]][]
    )
  }
  
  return (beams_spdf)
}

clipByMask2A = function(level2a, masks, output = "") {
  newFile =  hdf5r::H5File$new(output, mode="w")
  if(length(masks) == 0) {
    message("No intersection found!")
    newFile$close_all()
    newFile = hdf5r::H5File$new(output, mode="r")
    result = new("gedi.level2a", h5 = newFile)
    return(result)
  }
  
  for (attr in hdf5r::list.attributes(level2a@h5)) {
    hdf5r::h5attr(newFile, attr) = hdf5r::h5attr(level2a@h5, attr)
  }
  
  
  all_groups = hdf5r::list.groups(level2a@h5)
  
  
  # Check if the beam has any intersecting area
  beams_with_value = lapply(lapply(masks, function(x) sapply(x, length)), sum)>0
  beams_with_value = names(which(beams_with_value))
  beams_with_value = c(beams_with_value, "METADATA")
  which_groups = gsub("([^/]*).*","\\1",all_groups) %in% beams_with_value
  groups_with_value = all_groups[which_groups]
  
  # Setup progress bar
  all_datasets = hdf5r::list.datasets(level2a@h5)
  which_datasets = gsub("([^/]*).*","\\1",all_datasets) %in% beams_with_value
  datasets_with_value = all_datasets[which_datasets]
  
  total = length(datasets_with_value)
  pb = utils::txtProgressBar(min = 0, max = total, style = 3)
  progress = 0
  
  for (group in groups_with_value) {
    beam_id = strsplit(group, "/")[[1]][1]
    
    hdf5r::createGroup(newFile, group)
    createAttributesWithinGroup(level2a@h5, newFile, group)
    
    # Datasets to loop
    datasets = hdf5r::list.datasets(level2a@h5[[group]], recursive = FALSE, full.names = T)
    
    # Create list of algorithm ids for the datasets to choose right mask
    alg_ids = as.list(as.integer(sapply(regmatches(datasets, regexec("_a(\\d+)", datasets)), function(x) x[2])))
    names(alg_ids) = datasets
    
    for (dt in datasets) {
      # Get right mask
      alg_id = alg_ids[[dt]]
      
      if (is.na(alg_id)) {
        mask = masks[[beam_id]][["main"]]
        beam_shot_n = level2a@h5[[beam_id]][["shot_number"]]$dims
      } else {
        mask = masks[[beam_id]][[alg_id]]
        beam_shot_n = level2a@h5[[sprintf("%s/geolocation/elev_highestreturn_a%d", beam_id, alg_id)]]$dims
      }
      mask_size = length(mask)
      
      if (mask_size == 0) {
        #Update progress
        progress = progress + 1
        utils::setTxtProgressBar(pb, progress)
        next
      }
      
      
      h5_dt = level2a@h5[[dt]]
      dt_dim = h5_dt$dims
      dtype = h5_dt$get_type()
      if (is.na(all(h5_dt$chunk_dims))) {
        chunkdims = NULL
      } else {
        chunkdims = h5_dt$chunk_dims
      }
      
      if (length(dt_dim)[1] == 1) {
        if (dt_dim == 1) {
          hdf5r::createDataSet(newFile,dt,h5_dt[], dtype=dtype, chunk_dim=chunkdims)
        } else if (dt_dim == beam_shot_n) {
          hdf5r::createDataSet(newFile,dt,h5_dt[mask], dtype=dtype, chunk_dim=chunkdims)
        } else if ((dt_dim %% beam_shot_n) == 0) {
          n_waveforms = h5_dt$dims / beam_shot_n
          v.seq = Vectorize(seq.default,vectorize.args = c("from"), SIMPLIFY=T)
          mask_init = mask*n_waveforms - (n_waveforms - 1)
          mask_waveform = matrix(v.seq(mask_init, len=n_waveforms), nrow=1)[1,]
          total_size = n_waveforms*mask_size
          chunk_part = 1
          dt_res=hdf5r::createDataSet(newFile, dt, dtype=dtype, chunk_dim=chunkdims, dims=total_size)
          while (chunk_part < total_size) {
            end = chunk_part+chunkdims-1
            if (end > total_size) {
              end = total_size
            }
            get_part = mask_waveform[(chunk_part):(end)]
            dt_res[get_part] =  h5_dt[get_part]
            chunk_part = end+1
          }
        }
      } else if (length(dt_dim) == 2 && dt_dim[1] == beam_shot_n) {
        if (length(mask) == 1) {
          chunkdims = chunkdims[[2]]
        }
        hdf5r::createDataSet(newFile,dt,h5_dt[mask,][])
      } else if (dt_dim[2] == beam_shot_n) {
        newFile$create_dataset(dt,h5_dt[1:dt_dim[1],mask])
      }
      else {
        stop(paste0("Don't know how to handle the dataset: ", dt, "\nContact the maintainer of the package!"))
      }
      
      #Update progress
      progress = progress + 1
      utils::setTxtProgressBar(pb, progress)
    }
  }
  
  hdf5r::h5flush(newFile)
  newFile$close_all()
  newFile =  hdf5r::H5File$new(output, mode="r")
  result = new("gedi.level2a", h5 = newFile)
  close(pb)
  return (result)
}

clipLevel2B = function(level2b, xmin, xmax, ymin, ymax, output=""){
  # Get all spatial data as a list of dataframes with spatial information
  spData = getSpatialData2B(level2b)
  checkClipExtentInputs(level2b, "gedi.level2b", xmin, xmax, ymin, ymax)
  
  masks = clipSpDataByExtentLevelB(spData, xmin, xmax, ymin, ymax)
  
  if (output == "") {
    output = tempfile(fileext = ".h5")
  }
  output = fs::path_ext_set(output, "h5")
  
  newFile = clipByMask2B(level2b,
                         masks,
                         output)
  output = newFile@h5$filename
  close(newFile)
  result = readLevel2B(output)
  
  return (result)
}

clipLevel2BGeometry = function(level2b, polygon_spdf, output="", split_by=NULL) {
  output = checkOutput(output)
  checkClipGeoInputs(level2b, "gedi.level2b", polygon_spdf, split_by)
  
  spData = getSpatialData2B(level2b)
  
  xmin = polygon_spdf@bbox[1,1]
  xmax = polygon_spdf@bbox[1,2]
  ymin = polygon_spdf@bbox[2,1]
  ymax = polygon_spdf@bbox[2,2]
  
  masks = clipSpDataByExtentLevelB(spData, xmin, xmax, ymin, ymax)
  polygon_masks = getPolygonMaskLevelB(spData, masks, polygon_spdf, split_by)
  results = clipByMasks(level2b, polygon_masks, output, split_by, clipByMask2B)
  
  return (results)
}


# Helper function to return spatial data within a dataframe
getSpatialData2B = function(level2b) {
  level2b.h5<-level2b@h5
  groups_id<-grep("BEAM\\d{4}$",gsub("/","",
                                     hdf5r::list.groups(level2b.h5, recursive = F)), value = T)
  
  beams_spdf = list()
  
  for ( i in gsub("/","",groups_id)){
    beams_spdf[[i]] = data.frame(
      latitude_bin0=level2b.h5[[paste0(i,"/geolocation/latitude_bin0")]][],
      latitude_lastbin=level2b.h5[[paste0(i,"/geolocation/latitude_lastbin")]][],
      longitude_bin0=level2b.h5[[paste0(i,"/geolocation/longitude_bin0")]][],
      longitude_lastbin=level2b.h5[[paste0(i,"/geolocation/longitude_lastbin")]][]
    )
  }
  return (beams_spdf)
}


clipByMask2B = function(level2b, masks, output = "") {
  newFile =  hdf5r::H5File$new(output, mode="w")
  if(length(masks) == 0) {
    message("No intersection found!")
    newFile$close_all()
    newFile = hdf5r::H5File$new(output, mode="r")
    result = new("gedi.level2b", h5 = newFile)
    return(result)
  }
  
  for (attr in hdf5r::list.attributes(level2b@h5)) {
    hdf5r::h5attr(newFile, attr) = hdf5r::h5attr(level2b@h5, attr)
  }
  
  all_groups = hdf5r::list.groups(level2b@h5)
  beams_with_value = sapply(masks, length)>0
  beams_with_value = names(which(beams_with_value))
  beams_with_value = c(beams_with_value, "METADATA")
  which_groups = gsub("([^/]*).*","\\1",all_groups) %in% beams_with_value
  groups_with_value = all_groups[which_groups]
  
  # Setup progress bar
  all_datasets = hdf5r::list.datasets(level2b@h5)
  which_datasets = gsub("([^/]*).*","\\1",all_datasets) %in% beams_with_value
  datasets_with_value = all_datasets[which_datasets]
  
  total = length(datasets_with_value)
  pb = utils::txtProgressBar(min = 0, max = total, style = 3)
  progress = 0
  
  for (group in groups_with_value) {
    beam_id = strsplit(group, "/")[[1]][1]
    mask = masks[[beam_id]]
    mask_size = length(mask)
    
    hdf5r::createGroup(newFile,group)
    createAttributesWithinGroup(level2b@h5, newFile, group)
    
    for (dt in hdf5r::list.datasets(level2b@h5[[group]], recursive = FALSE, full.names = T)) {
      beam_shot_n = level2b@h5[[beam_id]][["shot_number"]]$dims
      h5_dt = level2b@h5[[dt]]
      dt_dim = h5_dt$dims
      dtype = h5_dt$get_type()
      if (is.na(all(h5_dt$chunk_dims))) {
        chunkdims = NULL
      } else {
        chunkdims = h5_dt$chunk_dims
      }
      
      if (length(dt_dim) == 1) {
        if (dt_dim == 1) {
          hdf5r::createDataSet(newFile,dt,h5_dt[], dtype=dtype, chunk_dim=chunkdims)
        } else if (dt_dim == beam_shot_n) {
          hdf5r::createDataSet(newFile,dt,h5_dt[mask], dtype=dtype, chunk_dim=chunkdims)
        } else if ((dt_dim %% beam_shot_n) == 0) {
          n_waveforms = h5_dt$dims / beam_shot_n
          v.seq = Vectorize(seq.default,vectorize.args = c("from"), SIMPLIFY=T)
          mask_init = mask*n_waveforms - (n_waveforms - 1)
          mask_waveform = matrix(v.seq(mask_init, len=n_waveforms), nrow=1)[1,]
          total_size = n_waveforms*mask_size
          chunk_part = 1
          dt_res=hdf5r::createDataSet(newFile, dt, dtype=dtype, chunk_dim=chunkdims, dims=total_size)
          while (chunk_part < total_size) {
            end = chunk_part+chunkdims-1
            if (end > total_size) {
              end = total_size
            }
            get_part = mask_waveform[(chunk_part):(end)]
            dt_res[get_part] =  h5_dt[get_part]
            chunk_part = end+1
          }
        }
      } else if (length(dt_dim) == 2 && dt_dim[1] == beam_shot_n) {
        if (length(mask) == 1) {
          chunkdims = chunkdims[[2]]
        }
        hdf5r::createDataSet(newFile,dt,h5_dt[mask,], dtype=dtype, chunk_dim=chunkdims)
      } else if (length(dt_dim) == 2 && dt_dim[2] == beam_shot_n){
        if (length(mask) == 1) {
          chunkdims = chunkdims[[1]]
        }
        hdf5r::createDataSet(newFile,dt,h5_dt[,mask], dtype=dtype, chunk_dim=chunkdims)
      } else {
        stop(paste0("Don't know how to handle dataset: ", dt, "\nContact the maintainer of the package!"))
      }
      
      #Update progress
      progress = progress + 1
      utils::setTxtProgressBar(pb, progress)
    }
  }
  
  newFile$close_all()
  
  newFile =  hdf5r::H5File$new(output, mode="r")
  result = new("gedi.level2b", h5 = newFile)
  close(pb)
  #spatial = level2b2dt(level2b)
  return (result)
}

createAttributesWithinGroup = function(h5, newFile, group="/") {
  for (attr in hdf5r::list.attributes(h5[[group]])) {
    hdf5r::h5attr(newFile[[group]], attr) = hdf5r::h5attr(h5[[group]], attr)
  }
}

clipSpDataByExtentLevel2A = function(spData, xmin, xmax, ymin, ymax) {
  masks = lapply(spData, function(x) {
    masks2 = lapply(x, function(y) {
      mask = y$longitude_lowest >= xmin &
        y$longitude_lowest <= xmax &
        y$latitude_highest >= ymin &
        y$latitude_highest <= ymax &
        y$longitude_highest >= xmin &
        y$longitude_highest <= xmax &
        y$latitude_lowest >= ymin &
        y$latitude_lowest <= ymax
      
      mask[!stats::complete.cases(mask)] = FALSE
      return ((1:length(y$longitude_lowest))[mask])
    })
    return (masks2)
  })
  if(all(sapply(masks, function(x) sum(sapply(x, length)))==0)){
    stop("The clipping ROI does not intersect with the data!")
  }
  return (masks)
}


getPolygonMaskLevel2A = function(spData, masks, polygon_spdf, split_by) {
  message("Intersecting with polygon...")
  pb = utils::txtProgressBar(min = 0, max = length(masks), style = 3)
  progress = 0
  polygon_masks = list()
  
  if (is.null(split_by)) {
    masknames = ""
  } else {
    masknames = unique(paste0(polygon_spdf@data[[split_by]]))
  }
  
  for (m in masknames) {
    polygon_masks[[m]] = list()
  }
  
  for (beam in names(masks)) {
    masks2 = masks[[beam]]
    
    for (i in 1:length(masks2)) {
      mask = masks2[[i]]
      if (length(mask) == 0) next
      
      spDataMasked = spData[[beam]][[i]][mask,]
      points = sp::SpatialPointsDataFrame(coords=matrix(c(spDataMasked$longitude_highest, spDataMasked$latitude_highest), ncol=2),
                                          data=data.frame(id=mask), proj4string = polygon_spdf@proj4string)
      pts = suppressPackageStartupMessages(raster::intersect(points, polygon_spdf))
      
      
      mask_name = names(masks2)[i]
      if (ncol(pts@data) == 2) {
        split_by2 = 2
      } else {
        split_by2 = split_by
      }
      if (is.null(split_by)) {
        polygon_masks[[1]][[beam]][[mask_name]] = pts@data[,1]
      } else {
        for (pol_id in as.character(unique(pts@data[split_by2])[,1])) {
          polygon_masks[[pol_id]][[beam]][[mask_name]] = pts[(pts@data[split_by2] == pol_id)[,1],]@data[,1]
        }
      }
      
      progress = progress + 1
      utils::setTxtProgressBar(pb, progress)
    }
  }
  close(pb)
  if(all(
    sapply(
      polygon_masks, function(x) {
        res=sapply(x, function(y) sum(sapply(y, length)))
        ifelse(length(res)>0,sum(res),0)
      }
    )==0))
  {
    stop("The clipping polygon does not intersect with the data!")
  }
  return (polygon_masks)
}


clipSpDataByExtentLevelB = function(spData, xmin, xmax, ymin, ymax) {
  masks = lapply(spData, function(x) {
    mask = x$longitude_bin0 >= xmin &
      x$longitude_bin0 <= xmax &
      x$latitude_bin0 >= ymin &
      x$latitude_bin0 <= ymax &
      x$longitude_lastbin >= xmin &
      x$longitude_lastbin <= xmax &
      x$latitude_lastbin >= ymin &
      x$latitude_lastbin <= ymax
    
    mask[!stats::complete.cases(mask)] = FALSE
    return ((1:length(x$longitude_bin0))[mask])
  })
  if (all(sapply(masks, length)==0)) {
    stop("The clipping ROI does not intersect with the data!")
  }
  return (masks)
}

checkOutput = function(output) {
  if (output == "") {
    output = tempfile(fileext = ".h5")
  }
  output = fs::path_ext_set(output, "h5")
  return (output)
}


getPolygonMaskLevelB = function(spData, masks, polygon_spdf, split_by) {
  message("Intersecting with polygons...")
  pb = utils::txtProgressBar(min = 0, max = length(masks), style = 3)
  progress = 0
  polygon_masks = list()
  
  if (is.null(split_by)) {
    masknames = ""
  } else {
    masknames = unique(paste0(polygon_spdf@data[[split_by]]))
  }
  for (m in masknames) {
    polygon_masks[[m]] = list()
  }
  
  for (beam in names(masks)) {
    mask = masks[[beam]]
    
    if (length(mask) == 0) next
    
    spDataMasked = spData[[beam]][mask,]
    points = sp::SpatialPointsDataFrame(coords=matrix(c(spDataMasked$longitude_bin0, spDataMasked$latitude_bin0), ncol=2),
                                        data=data.frame(idrownames=mask), proj4string = polygon_spdf@proj4string)
    pts = suppressPackageStartupMessages(raster::intersect(points, polygon_spdf))
    if (ncol(pts@data) == 2) {
      split_by2 = 2
    } else {
      split_by2 = split_by
    }
    if (is.null(split_by)) {
      polygon_masks[[1]][[beam]] = pts@data[,1]
    } else {
      for (pol_id in unique(as.character(paste0(pts@data[[split_by2]])))) {
        
        polygon_masks[[pol_id]][[beam]] = pts[pts@data[[split_by2]] == pol_id,]@data[,1]
      }
    }
    
    progress = progress + 1
    utils::setTxtProgressBar(pb, progress)
  }
  close(pb)
  
  if (all(sapply(polygon_masks, length)==0)) {
    stop("The polygon does not intersect with the data!")
  }
  return (polygon_masks)
}

clipByMasks = function(h5file, polygon_masks, output, split_by, clipFun) {
  message("Writing new HDF5 files...")
  results = list()
  i = 0
  len_masks = length(polygon_masks)
  for (pol_idx in 1:length(polygon_masks)) {
    pol_id = names(polygon_masks)[pol_idx]
    i = i + 1
    message(sprintf("Writing %s='%s': %d of %d", split_by, pol_id, i, len_masks))
    output2 = gsub("\\.h5$", paste0("_", pol_id,".h5"), output)
    results[[pol_id]] = clipFun(h5file,
                                masks = polygon_masks[[pol_idx]],
                                output = output2)
  }
  
  return (results)
}


checkClipExtentInputs = function(obj, className, xmin, xmax, ymin, ymax) {
  criterias = list()
  criterias[paste0("Object is not from class", className)] = class(obj) == className
  criterias = c(criterias, list(
    "xmin is not numeric" = class(xmin) == "numeric",
    "xmax is not numeric" = class(xmax) == "numeric",
    "ymin is not numeric" = class(ymin) == "numeric",
    "ymax is not numeric" = class(ymax) == "numeric"
  ))
  do.call(stopifnotMessage, criterias)
}

checkClipGeoInputs = function(obj, className, polygon_spdf, split_by) {
  criterias = list()
  criterias[paste0("Object is not from class", className)] = class(obj) == className
  criterias = c(criterias, list(
    "polygon_spdf is not a SpatialPolygonsDataFrame" = class(polygon_spdf) == "SpatialPolygonsDataFrame",
    "split_by is not a valid attribute of polygon_spdf" = is.null(split_by) || split_by %in% colnames(polygon_spdf@data)
  ))
  do.call(stopifnotMessage, criterias)
}

getLevel1BWF<-function(level1b,shot_number){
  
  level1b<-level1b@h5
  groups_id<-grep("BEAM\\d{4}$",gsub("/","",
                                     hdf5r::list.groups(level1b, recursive = F)), value = T)
  
  i=NULL
  for ( k in groups_id){
    gid<-max(level1b[[paste0(k,"/shot_number")]][]==shot_number)
    if (gid==1) {i=k}
  }
  
  if(is.null(i)) {
    stop(paste0("Shot number ", shot_number, " was not found within the dataset!. Please try another shot number"))
  } else{
    
    shot_number_i<-level1b[[paste0(i,"/shot_number")]][]
    shot_number_id<-which(shot_number_i[]==shot_number)
    elevation_bin0<-level1b[[paste0(i,"/geolocation/elevation_bin0")]][]
    elevation_lastbin<-level1b[[paste0(i,"/geolocation/elevation_lastbin")]][]
    rx_sample_count<-level1b[[paste0(i,"/rx_sample_count")]][]
    rx_sample_start_index<-level1b[[paste0(i,"/rx_sample_start_index")]][]
    rx_sample_start_index_n<-rx_sample_start_index-min(rx_sample_start_index)+1
    rxwaveform_i<-level1b[[paste0(i,"/rxwaveform")]][rx_sample_start_index_n[shot_number_id]:(rx_sample_start_index_n[shot_number_id]+rx_sample_count[shot_number_id]-1)]
    rxwaveform_inorm<-(rxwaveform_i-min(rxwaveform_i))/(max(rxwaveform_i)-min(rxwaveform_i))*100
    elevation_bin0_i<-elevation_bin0[shot_number_id]
    elevation_lastbin_i<-elevation_lastbin[shot_number_id]
    z=rev(seq(elevation_lastbin_i,elevation_bin0_i,(elevation_bin0_i-elevation_lastbin_i)/rx_sample_count[shot_number_id]))[-1]
    
    waveform<-new("gedi.fullwaveform", dt = data.table::data.table(rxwaveform=rxwaveform_i,elevation=z))
    
    return(waveform)
  }
}

plotWFMetrics = function(level1b, level2a, shot_number, rh=c(25, 50, 75), ...) {
  # Avoid NOTEs from checking
  elevation = NULL
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar)) 
  
  # Extracting GEDI full waveform for a giving shotnumber
  wf <- getLevel1BWF(level1b, shot_number=shot_number)
  
  level2AM<-getLevel2AM(level2a)
  shotid_mask = which(level2AM$shot_number == shot_number)
  
  level2_shot = level2AM[shotid_mask,]
  ground_z = level2_shot$elev_lowestmode
  rhs = paste0(as.character(c("rh0", as.character(paste0("rh",rh)), "rh100")))
  rh = level2_shot[, rhs, with=FALSE]
  rh_z = rh + ground_z
  
  
  top_z = level2AM[shotid_mask,]$elev_highestreturn
  
  range_energy = range(wf@dt$rxwaveform)
  range_abs_diff = abs(diff(range_energy))
  
  requireNamespace("data.table")
  
  range_z = range(rh_z)
  min_z = min(range_z)
  max_z = max(range_z)
  diff_z = abs(diff(range_z))
  wf_between = wf@dt[elevation %between% range_z,,]
  energy_offset = min(range_energy)
  energy_no_offset = (wf_between$rxwaveform - energy_offset)
  cumsum_energy = cumsum(rev(energy_no_offset))
  
  range_cumsum = range(cumsum_energy)
  range_abs_diff_cumsum = abs(diff(range_cumsum))
  energy_cum_normalized = ((cumsum_energy)/(range_abs_diff_cumsum/range_abs_diff))+energy_offset
  
  par(mar = c(5, 4, 4, 4) + 0.3)
  offset = diff_z*0.2
  ymin = min_z-offset
  ymax = max_z+offset
  wf_interest=wf@dt[wf@dt$elevation >= ymin & wf@dt$elevation <= ymax,]$rxwaveform
  qts=quantile(wf_interest, c(0.05, 1), type=1)
  
  
  z_masked = rev(wf_between$elevation)
  
  
  ticks = seq(min_z, max_z, length=4)
  ticks_label = format(ticks-min_z, digits = 2)
  
  rh_closest_en = list()
  for (i in 1:length(rh_z)) {
    absdiff_rh = abs(z_masked-rh_z[[i]])
    rh_closest_en[[names(rh_z)[[i]]]] = which(absdiff_rh==min(abs(absdiff_rh)))
  }
  
  # Make marks for RH based in a point
  mark = function(x, y, ...) {
    arrows(x, y, x, min_z, length=.1, code = 3)
  }
  
  # Find mid y for rh labels
  ymidpoint = function(x) {
    x-(x-min_z)/2
  }
  
  
  plot(wf, relative=FALSE, polygon=TRUE, type="l", lwd=2, col="forestgreen",
       xlab="Waveform Amplitude", ylab="Elevation (m)", ylim=c(ymin, ymax), xlim=qts+c(0, 0.1*abs(diff(qts))), ...)
  par(new=TRUE)
  plot(energy_cum_normalized, z_masked, lwd=2, axes=F, bty="n", type="l", xlab = "", ylab = "", ylim=c(ymin, ymax), xlim=qts)
  axis(side=4, at = ticks, labels=ticks_label)
  mtext("Height (m)", side=4, line=2)
  for (i in 2:(length(rh_z)-1)) {
    mark(energy_cum_normalized[rh_closest_en[[i]]], rh_z[[i]])
    text(energy_cum_normalized[rh_closest_en[[i]]], ymidpoint(rh_z[[i]]), toupper(names(rh_z)[[i]]), pos = 2)
  }
  text(qts[2]-diff(qts)/2, rh_z[[length(rh_z)]], "RH100", pos=3)
  abline(rh_z[[length(rh_z)]], 0, lty="dashed")
  text(qts[2]-diff(qts)/2, rh_z[[1]], "RH0", pos=1)
  abline(rh_z[[1]], 0, lty="dashed")
  
}

