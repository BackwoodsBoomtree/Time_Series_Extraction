library(terra)
library(viridis)

in_dir   <- "G:/TROPOMI/esa/gridded/25km/8day"
out_dir  <- "G:/TROPOMI/class/"
in_csv   <- "E:/site_example2.csv"
csv_name <- "PBIO-5733_TROPOMI_"
var_list <- c("NIRv", "NIRv_Rad", "SIF_743", "REF_665", "REF_781", 
              "n", "NIRv_std", "NIRv_Rad_std", "SIF_743_std", "REF_665_std", "REF_781_std")


get_tropomi_data <- function (in_dir, out_dir, in_csv, csv_name, var_list){
  
  # Get list of file names
  file_list <- list.files(in_dir, pattern = "*.nc", full.names = TRUE)
  
  print("Grabbing data from: ")
  print(basename(file_list))
  
  # Get site list
  sites <- read.csv(in_csv)
  
  # Combine input files so we can get the length of time
  for (f in 1:length(file_list)) {
    if (f == 1) {
      combo <- rast(file_list[f], subds = var_list[1])
    } else {
      combo <- c(combo, rast(file_list[f], subds = var_list[1]))
    }
  }

  # Build empty data frame for output
  df_out <- data.frame(matrix(ncol = length(var_list) + 1, nrow = length(time(combo))))
  colnames(df_out) <- c("Date", var_list)
  
  for (i in 1:(nrow(sites))) {
    
    coord <- vect(cbind(sites$lon[i], sites$lat[i]), type = "points", crs = "+proj=longlat +datum=WGS84")
    
    for (v in 1:length(var_list)) {
      
      # Combine data from files into a single brick
      for (f in 1:length(file_list)) {
        if (f == 1) {
          v_data <- rast(file_list[f], subds = var_list[v])
        } else {
          v_data <- c(v_data, rast(file_list[f], subds = var_list[v]))
        }
      }
      
      # Fill output df column with the data
      ex_data          <- extract(v_data, coord)
      df_out[,(v + 1)] <- as.vector(t(ex_data[-c(1)]))

    }
    
    # Fill the date column
    df_out[,1] <- strftime(as.POSIXlt(time(v_data), format = "%Y/%m/%d"))
    
    # Write the file
    out_name <- paste0(out_dir, csv_name, sites$Name[i], "_", sites$lon[i], "_", sites$lat[i], ".csv")
    write.csv(df_out, out_name)
    print(paste0("Saved output to: ", out_name))
    
  }
}

get_tropomi_data(in_dir, out_dir, in_csv, csv_name, var_list)
