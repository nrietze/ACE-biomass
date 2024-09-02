library(dplyr)
library(tidyterra)
library(class)
library(caret)
library(terra)

path_data <- "./data/"

# Load training polygons
training_polygons <- vect(paste0(path_data,'feature_layers/Pioneer_samples.shp') ) %>% 
  tidyterra::mutate(.,macroclass = factor(macroclass,))

# Load mask rasters
rast_ice_mask <- rast(paste0(path_data,'raster/NA_Pioneer_ice_mask_raster.tif'))
rast_water_mask <- rast(paste0(path_data,'raster/NA_Pioneer_water_mask_raster.tif'))

# combine mask raster, values that are kept have a mask value = 0
rast_mask <- rast_ice_mask + rast_water_mask

# Load list of raster files and mask them
rast_vnir <- rast(paste0(path_data,'raster/Pioneer_composite.tif')) %>% 
  mask(., rast_mask, inverse = T, maskvalues = 0)
# rename bands
names(rast_vnir) <- c("Green", "Blue", "Red", "RedEdge", "NIR")

rast_ndwi <- rast(paste0(path_data,'raster/Pioneer_ndwi.tif')) %>% 
  mask(., rast_mask, inverse = T, maskvalues = 0)
# rename bands
names(rast_ndwi) <- 'ndwi'

rast_ndvi <- rast(paste0(path_data,'raster/Pioneer_ndvi.tif')) %>% 
  mask(., rast_mask, inverse = T, maskvalues = 0)
# rename bands
names(rast_ndvi) <- 'ndvi'

# Bands to extract (RedEdgeMX = ('Blue','Green','Red','Red Edge','NIR'))
sel_bands <- c("Green", "Blue", "Red", "NIR")

#---- 
# Function to extract pixel values in each polygon
get_raster_values <- function(vnir,ndvi,ndwi, features, sel_bands){
  
  # Extract raster data
  vnir_vals <- terra::extract(vnir, features)[,c(sel_bands,'ID')]
  ndvi_vals <- terra::extract(ndvi, features)[,c(1:2)]
  ndwi_vals <- terra::extract(ndwi, features)[,c(1:2)]
  
  # Change column_names
  # names(raster_focal_vals)[-1] <- paste0(names(raster_focal_vals)[-1], "_sd")
  
  # Combine raster value tables
  raster_vals_all <- cbind(vnir_vals, 
                           select(ndvi_vals,-ID), 
                           select(ndwi_vals,-ID) ) %>% 
    mutate(., fid = ID)
  
  training_data <- features %>%
    as.data.frame() %>%
    select(fid, macroclass) %>%
    full_join(raster_vals_all) 
  
  return(training_data)
  
}

# Apply function
training_data <- get_raster_values(rast_vnir,rast_ndvi,rast_ndwi,
                                   training_polygons, sel_bands)

# Overview of training data
n_polys <- length(training_polygons)
print(training_data %>% group_by(fid,macroclass) %>% tally() , n = n_polys)

"
The smallest polygon only covers 8 pixels, so we might want to sample with replacement.
But then, the sample wouldn't be a random sample from our population.
"

# Subset and shuffle training data
training_sample <- training_data %>%
  group_by(fid,macroclass) %>%
  na.omit() %>%
  sample_n(50,replace = T)

training_sample %>% group_by(fid,macroclass) %>% tally()

# Split into training and validation data (80 - 20 split) and partition by polygon (=fid)
training_sample$id <- 1:nrow(training_sample)

"
I split 80 - 20 in each polygon, so that we can keep the variation of the 
original polygons in both training and validation data.
"

training <- training_sample %>%
  group_by(fid) %>%
  slice_sample(prop = 0.8)
validation <- filter(training_sample, !(id %in% training$id)) 

# Define variables for classification
classif_vars <- c("Green", "Blue", "Red", "NIR","ndvi")

# Set up k-NN classifier
mod_knn = train(x = training[,classif_vars],
                y = training$macroclass,
                method = "knn",
                preProcess = NULL,
                tuneGrid = data.frame(k = c(1:12)), # trains classifier using multiple iterations of k-neighbors
                trControl = trainControl(method = "repeatedcv",
                                         number = 10,
                                         repeats = 3)
)

print(mod_knn)

# Predict classes of validation set
test_preds <- predict(mod_knn, validation[,classif_vars])

# generate confusion matrix
confusionMatrix(test_preds, validation$macroclass)

# Export confusion matrix
'

file_connection <- file("knn_confusion_matrix.txt")
confusionMatrix(test_preds, validation$macroclass) %>%
  print() %>%
  capture.output() %>%
  writeLines(file_connection)
close(file_connection)

'

# predict entire image
predictors <- c(rast_vnir[[sel_bands]] %>% 
                  slice_colrows(
                    cols = c(2e3:3e3, 2e3:3e3),
                    rows = -c(1e3:2e3, 3e3:5e3)
                  ),
                rast_ndvi %>% 
                  slice_colrows(
                    cols = c(2e3:3e3, 2e3:3e3),
                    rows = -c(1e3:2e3, 3e3:5e3)
                  ))

cat("Predicting raster...\n")
preds <- terra::predict(predictors, mod_knn,na.rm = T)
cat("Writing raster...\n")
writeRaster(preds,
            filename = paste0(path_data,"raster/pioneer-TEST_preds.tif"),
            overwrite = T
)
