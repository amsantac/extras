#' create tables for producing atmospherically-corrected images using CLASlite
#' 
#' create tables to be saved as csv files that will be input for batch processing in CLASlite to convert raw images to reflectance
#' @param character vector containing absolute paths to folders containing images to be processed
#' @param GeoTIFF integer; 1 for output images in GeoTIFF format, 0 for ENVI binary format
#' @param Reduce_masking integer; 0 for reduced masking, 1 for no reduced masking
#' @param no_masking integer; 0 for masking, 1 for no masking
#' @param fmask integer; 0 for no clouds/cloud shadows/snow/water masking using fmask, 1 for masking with fmask
#' @param cldpix integer; dilated number of pixels for cloud with default value of 3 when masking with fmask 
#' @param sdpix integer; dilated number of pixels for cloud shadow with default value of 3 when masking with fmask
#' @param snpix integer; dilated number of pixels for snow with default values of 0 when masking with fmask
#' @param cldprob numeric; cloud probability threshold with default value of 22.5 (range from 0-100) when masking with fmask
#' @export reflectanceImgTable4csv
#' @example 
#' dontrun{
#'   ## set working directory: folder where the output csv files should be written
#'   setwd("C:/amsantac/PhD/Research/dissertation/data/landsat/images/2000")
#'   foldersList <- normalizePath(list.dirs(full.names = TRUE, recursive = FALSE))
#' 
#'   ## run the function and create the corresponding csv file
#'   ## the reflectance images will be created without masking cloud/cloud shadows/water/snow
#'   outDF <- reflectanceImgTable4csv(foldersList, no_masking = 1)
#'   write.table(outDF, "reflectance_2000.csv", row.names = FALSE, quote = FALSE, sep = ", ")
#' }

reflectanceImgTable4csv <- function(foldersList, GeoTIFF = 0, Reduce_masking = 0, no_masking = 0, fmask = 0, 
                                    cldpix = 3, sdpix = 3, snpix = 0, cldprob = 22.5){
  
  outDF <- data.frame(matrix(data = NA, nrow = length(foldersList), ncol = 18))
  colnames(outDF) <- c("Input_FileName", "Date", "Time", "Gain_Settings", "Satellite", "Lead_File", "Therm_File",
                       "QA_File", "Output_File", "GeoTIFF", "Proc_sys", "Reduce_masking", "no_masking", "fmask", 
                       "cldpix", "sdpix", "snpix", "cldprob")
  outDF[, "GeoTIFF"] <- GeoTIFF
  outDF[, 12:18] <- t(apply(outDF[, 12:18], 1, function(x){c(Reduce_masking, no_masking, fmask, cldpix, sdpix, snpix,
                                                             cldprob)}))
  i <- 1
  
  for (folder in foldersList){
    
    rawImg1 <- grep("raw", list.files(folder, full.names = TRUE), value = TRUE)[1] 
    outDF[i, "Input_FileName"] <- gsub("/", "\\", rawImg1, fixed = TRUE)
    
    mtlTxt <- grep("MTL.txt", list.files(folder, full.names = TRUE), value = TRUE)  
    mtl <- readLines(mtlTxt)
    
    ## extract acquisition date
    date1 <- strsplit(mtl[grep("DATE_ACQUIRED", mtl)], "= ")[[1]][2]
    outDF[i, "Date"] <- format(as.Date(date1), "%d%m%Y")
    
    ## extract acquisition time
    time1 <- strsplit(mtl[grep("SCENE_CENTER_TIME", mtl)], "= ")[[1]][2]
    time2 <- paste(unlist(strsplit(time1, ":"))[1:2], collapse = "")
    outDF[i, "Time"] <- gsub("\"", "", time2)
    
    ## extract satellite id
    sid1 <- strsplit(mtl[grep("SPACECRAFT_ID", mtl)], "= ")[[1]][2]
    sid2 <- gsub("\"", "", sid1)
    if(sid2 == "LANDSAT_8") Satellitei <- 0
    if(sid2 == "LANDSAT_7") Satellitei <- 1
    if(sid2 == "LANDSAT_5") Satellitei <- 2
    if(sid2 == "LANDSAT_4") Satellitei <- 3
    if(sid2 == "ALI") Satellitei <- 4
    if(sid2 == "ASTER") Satellitei <- 5
    if(sid2 == "SPOT4") Satellitei <- 6
    if(sid2 == "SPOT5") Satellitei <- 7
    outDF[i, "Satellite"] <- Satellitei
    
    ## extract gains
    gains <- NULL
    
    ## for Landsat 7
    if (Satellitei == 1){
      for (band in c(1:5, 7)){
        bandGain_1 <- strsplit(mtl[grep(paste0(" GAIN_BAND_", band), mtl)], "= ")[[1]][2]
        bandGain_2 <- gsub("\"", "", bandGain_1)
        gains <- c(gains, bandGain_2)
      }
      outDF[i, "Gain_Settings"] <- paste(gains, collapse="")
    }
    
    ## extract processing system
    sys1 <- strsplit(mtl[grep("PROCESSING_SOFTWARE_VERSION", mtl)], "= ")[[1]][2]
    sys2 <- strsplit(gsub("\"", "", sys1), "_")[[1]][1]
    if(sys2 == "LPGS") outDF[i, "Proc_sys"] <- 0
    if(sys2 == "NLAPS") outDF[i, "Proc_sys"] <- 1
    
    ## extract thermal file name
    ThermImg1 <- grep("therm", list.files(folder, full.names = TRUE), value = TRUE)[1]
    outDF[i, "Therm_File"] <- gsub("/", "\\", ThermImg1, fixed = TRUE)
    
    ## extract quality image file
    ## for Landsat 8
    if (Satellitei == 0){
      QAImg1 <- grep("_QA", list.files(folder, full.names = TRUE), value = TRUE)[1]
      outDF[i, "QA_File"] <- gsub("/", "\\", QAImg1, fixed = TRUE)
    }
    
    ## output file name
    outDF[i, "Output_File"] <- sub("_therm", "_refl", outDF[i, "Therm_File"])
    
    i <- i + 1
    
  }
  outDF[is.na(outDF)] <- ""
  return(outDF)
}
