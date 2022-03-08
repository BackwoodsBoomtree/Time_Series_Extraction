library(terra)

sif_file <- "G:/TROPOMI/esa/gridded/4x5/TROPOMI.ESA.SIF.2018-2021.global.monthly.3.9x5.nc"
map_file <- "G:/TROPOMI/esa/extracted/biome.nc"
out_csv  <- "G:/TROPOMI/esa/extracted/Scot_Mingyang_TROPOMI_monthly_2018-2021.csv"


get_tropomi_data <- function (sif_file, map_file, out_csv){
  
  # # Get list of file names
  # file_list <- list.files(in_dir, pattern = "*.nc", full.names = TRUE)
  # 
  # print("Grabbing data from: ")
  # print(basename(file_list))
  
  # Get maps for masking
  biomes      <- rast(map_file)
  biomes      <- t(biomes)
  biomes      <- flip(biomes, direction = "horizontal")
  biomes      <- flip(biomes, direction = "vertical")
  my_ext      <- ext(c(-180, 180, -90, 90))
  ext(biomes) <- my_ext
  
  trop_forest <- biomes
  trop_forest[trop_forest != 6] <- NA
  trop_grass  <- biomes
  trop_grass[trop_grass != 5] <- NA
  
  n_hemi <- ext(c(-180, 180, 0, 90))
  s_hemi <- ext(c(-180, 180, -90, 0))
  
  n_trop_forest <- crop(trop_forest, n_hemi)
  s_trop_forest <- crop(trop_forest, s_hemi)
  n_trop_grass  <- crop(trop_grass, n_hemi)
  s_trop_grass  <- crop(trop_grass, s_hemi)
  
  n_trop_forest <- extend(n_trop_forest, my_ext)
  s_trop_forest <- extend(s_trop_forest, my_ext)
  n_trop_grass  <- extend(n_trop_grass, my_ext)
  s_trop_grass  <- extend(s_trop_grass, my_ext)
  
  # Get SIF data
  sif         <- rast(sif_file, subds = "SIF_743")
  sif_dc      <- rast(sif_file, subds = "SIF_Corr_743")
  ext(sif)    <- my_ext
  ext(sif_dc) <- my_ext
  
  sif_n_trop_forest <- mask(sif, n_trop_forest)
  sif_s_trop_forest <- mask(sif, s_trop_forest)
  sif_n_trop_grass  <- mask(sif, n_trop_grass)
  sif_s_trop_grass  <- mask(sif, s_trop_grass)
  
  sif_dc_n_trop_forest <- mask(sif_dc, n_trop_forest)
  sif_dc_s_trop_forest <- mask(sif_dc, s_trop_forest)
  sif_dc_n_trop_grass  <- mask(sif_dc, n_trop_grass)
  sif_dc_s_trop_grass  <- mask(sif_dc, s_trop_grass)
  
  # Calc means
  sif_n_trop_forest <- global(sif_n_trop_forest, fun = "mean", na.rm = TRUE)
  sif_s_trop_forest <- global(sif_s_trop_forest, fun = "mean", na.rm = TRUE)
  sif_n_trop_grass  <- global(sif_n_trop_grass, fun = "mean", na.rm = TRUE)
  sif_s_trop_grass  <- global(sif_s_trop_grass, fun = "mean", na.rm = TRUE)
  
  sif_dc_n_trop_forest <- global(sif_dc_n_trop_forest, fun = "mean", na.rm = TRUE)
  sif_dc_s_trop_forest <- global(sif_dc_s_trop_forest, fun = "mean", na.rm = TRUE)
  sif_dc_n_trop_grass  <- global(sif_dc_n_trop_grass, fun = "mean", na.rm = TRUE)
  sif_dc_s_trop_grass  <- global(sif_dc_s_trop_grass, fun = "mean", na.rm = TRUE)
  
  # Build empty data frame for output
  df_out <- data.frame(matrix(ncol = 8 + 1, nrow = length(time(sif))))
  colnames(df_out) <- c("Date", "sif_n_trop_forest", "sif_s_trop_forest", "sif_n_trop_grass", "sif_s_trop_grass",
                        "sif_dc_n_trop_forest", "sif_dc_s_trop_forest", "sif_dc_n_trop_grass", "sif_dc_s_trop_grass")
  
  # Fill df
  df_out[,1] <- time(sif)
  df_out[,2] <- sif_n_trop_forest
  df_out[,3] <- sif_s_trop_forest
  df_out[,4] <- sif_n_trop_grass
  df_out[,5] <- sif_s_trop_grass
  df_out[,6] <- sif_dc_n_trop_forest
  df_out[,7] <- sif_dc_s_trop_forest
  df_out[,8] <- sif_dc_n_trop_grass
  df_out[,9] <- sif_dc_s_trop_grass
    
  # Write the file
  write.csv(df_out, out_csv)
  print(paste0("Saved output to: ", out_csv))
}


get_tropomi_data(sif_file, map_file, out_csv)
