library(terra)

file_lists <- "G:/TROPOMI/esa/extracted/zoe/zoe_files.csv"
site_list  <- "G:/TROPOMI/esa/extracted/zoe/zoe_sites.csv"
out_dir    <- "G:/TROPOMI/esa/extracted/zoe/"
csv_name   <- "Pierrat_TROPOMI_"
var_list   <- c("SIF_743","SIF_Corr_743", "NIRv", "NIRv_Rad", "SIF_743", "REF_665", "REF_781", "Mean_TOA_RAD_743",
                "n", "SIF_743_std", "SIF_Corr_743_std", "NIRv_std", "NIRv_Rad_std", "REF_665_std", "REF_781_std")

get_tropomi_data <- function (file_list, site, out_dir, csv_name, var_list) {
  
  print("Grabbing data from: ")
  print(basename(file_list))
  
  # Combine input files so we can get the length of time
  for (i in 1:length(file_list)) {
    if (i == 1) {
      combo <- rast(file_list[i], subds = var_list[1])
    } else {
      combo <- c(combo, rast(file_list[i], subds = var_list[1]))
    }
  }
  
  # Build empty data frame for output
  df_out <- data.frame(matrix(ncol = length(var_list) + 1, nrow = length(time(combo))))
  colnames(df_out) <- c("Date", var_list)
  
  coord <- vect(cbind(site$lon, site$lat), type = "points", crs = "+proj=longlat +datum=WGS84")
  
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
  
  return(df_out)
}



# Get file lists
files <- read.csv(file_lists)

# Get site list
sites <- read.csv(site_list)

for (i in 1:ncol(files)) {
  
  file_list <- files[,i]
  
  for (j in 1:(nrow(sites))) {
  
    site <- sites[j,]
    
    df <- get_tropomi_data(file_list, site, out_dir, csv_name, var_list)
    
    # Write the file
    out_name <- paste0(out_dir, csv_name, site$Name, "_", site$lon, "_", site$lat, "_", colnames(files[i]), ".csv")
    write.csv(df, out_name)
    print(paste0("Saved output to: ", out_name))
    print("")
  }
}


