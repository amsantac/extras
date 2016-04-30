tool_exec <- function(in_params, out_params)
{
  # load the required packages
  cat(paste0("\n", "Loading packages...", "\n"))
  
  if (!requireNamespace("dismo", quietly = TRUE))
    install.packages("dismo")
  if (!requireNamespace("raster", quietly = TRUE))
    install.packages("raster")
  require(dismo)
  require(raster)
  
  # read input and output parameters
  occurrence_dataset = in_params[[1]]
  continuous_rasters_folder = in_params[[2]]
  biome_raster = in_params[[3]]
  model = in_params[[4]]
  
  out_raster = out_params[[1]]
  out_table = out_params[[2]]
  out_shp = out_params[[3]]
  
  cat(paste0("\n", "Loading datasets...", "\n"))

  # open the input shapefile dataset  
  d <- arc.open(occurrence_dataset)
  occurrence <- arc.data2sp(arc.select(d))
  
  # read the continuous raster files from a folder
  rfiles1 <- list.files(path = continuous_rasters_folder, full.names = TRUE)  
  rasters1 <- stack(rfiles1[-grep(".aux", rfiles1)])

  # read the categorical raster (e.g., biome)
  raster2 <- raster(gsub("/", "\\\\", biome_raster))

  # create a RasterStack object for the model predictors
  predictors <- stack(rasters1, raster2)

  # extract values from the RasterStack at the locations of the species occurrence
  presvals <- as.data.frame(extract(predictors, occurrence)) 

  cat(paste0("\n", "Adjusting model...", "\n"))
  
  # adjust the desired model: bioclim, domain, Mahalanobis or a generalized linear model
  if(model == "bioclim"){
    fitmodel <- bioclim(subset(presvals, select = -c(biome)))
    p <- predict(predictors, fitmodel)
  }
  
  if(model == "domain"){
    fitmodel <- domain(dropLayer(predictors, 'biome'), as.data.frame(occurrence)[, c('coords.x1', 'coords.x2')])
    p <- predict(dropLayer(predictors, 'biome'), fitmodel)
  }
  
  if(model == "mahal"){
    fitmodel <- mahal(dropLayer(predictors, 'biome'), as.data.frame(occurrence)[, c('coords.x1', 'coords.x2')])
    p <- predict(dropLayer(predictors, 'biome'), fitmodel)
  }
  
  if(model == "glm"){
    set.seed(0)
    backgr <- randomPoints(predictors, 500)
    absvals <- extract(predictors, backgr)
    pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
    sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals)))
    sdmdata[, 'biome'] = as.factor(sdmdata[, 'biome'])
    fitmodel = glm(pb ~ ., data = sdmdata)
    p <- predict(predictors, fitmodel)
  }

  # export the predicted species distribution raster object and the output table and shapefile 
  cat(paste0("\n", "Writing result datasets...", "\n"))
  if (!is.null(out_raster) && out_raster != "NA")
    writeRaster(p, out_raster)

  if (!is.null(out_table) && out_table != "NA")
    arc.write(out_table, presvals)

  if (!is.null(out_shp) && out_shp != "NA")
    arc.write(out_shp, presvals, coords = coordinates(occurrence), shape_info = arc.shapeinfo(d))
    
  cat(paste0("\n", "Done.", "\n"))
  return(out_params)
}
