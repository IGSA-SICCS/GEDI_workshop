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
