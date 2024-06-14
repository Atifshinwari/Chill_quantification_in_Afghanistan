Test:

  # List all files in the directory
  file_names <- list.files(path = "D:/Rdata/Chill_quantification/Future_scenarios/Backup of original weather cmip6/Ge_weather_cmip6", full.names = TRUE)
  
  # Load files in the directory
  dfs <- map(file_names, ~ read.csv(.))
  
  file_names <- list.files(path = "D:/Rdata/Chill_quantification/Future_scenarios/Backup of original weather cmip6/Ge_weather_cmip6", full.names = FALSE)
  
  #Assigning the names
  names(dfs) <- file_names
  
#Applying it to my data
  require(stringr)
  # Assuming you have a list of data frames called dfs with unique names
  # For example:
  dfs_sample <- list("Adraskan_ssp126.BCC-CSM2-MR.2050.csv" = data.frame(x = 1:3),
              "Adraskan_ssp126.BCC-CSM2-MR.2085.csv" = data.frame(y = 4:6),
              "Adraskan_ssp126.GFDL-ESM4.2050.csv" = data.frame(z = 7:9))
  
  # Rename the data frames by replacing the second dot with an underscore
  new_names <- lapply(names(dfs), function(name) str_replace(name, "\\.(.*?)\\.", "_\\1_"))
  names(dfs) <- unlist(new_names)
 
  #Now as I have to save it with these new names
  # Set the directory where you want to save the files
  setwd("D:/Rdata/Chill_quantification/Future_scenarios/Generated_weather_cmip6")
  
  # Save each data frame in dfs with its updated name
  for (name in names(dfs)) {
    # Get the data frame from the dfs list
    df <- dfs[[name]]
    
    # Construct the file name with the updated name and desired file extension
    file_name <- paste0(name, ".csv")
    
    # Save the data frame as a CSV file
    write.csv(df, file = file_name, row.names = FALSE)
  }
  
  
  

  df_weather <- dfs
   
  library(tidyverse)
  # First change the position of 4th and 3rd character in the name
    # Rename the files within the 'dfs' list
  for (i in seq_along(df_weather)) {
    # Extract the parts of the file name
    parts <- unlist(strsplit(names(df_weather)[i], "_"))
    
    # Swap the positions of the third and fourth parts
    new_name <- paste(parts[1], parts[2], parts[4], parts[3], sep = "_")
    
    # Set the new name for the corresponding element in the 'dfs' list
    names(df_weather)[i] <- new_name
  }
  
# Now replace 2050.csv by 2050 and 2085.csv by 2085
  # Iterate over each dataframe in the list
  for (i in 1:length(df_weather)) {
    # Extract the original file name
    original_name <- names(df_weather)[i]
    
    # Replace "2050.csv" with "2050" in the file name
    new_name <- gsub("2050.csv", "2050", original_name)
    
    # Replace "2085.csv" with "2085" in the file name
    new_name <- gsub("2085.csv", "2085", new_name)
    
    # Update the dataframe name in the list
    names(df_weather)[i] <- new_name
    
    # Print the updated name
    cat("Updated dataframe", i, "name:", new_name, "\n")
  }
  

  # Save each data frame in df_weather with its updated name

  write.csv(df_weather, paste0(names(df_weather[i]),".csv"))
  
  setwd("D:/Rdata/Chill_quantification/Future_scenarios/Generated_weather_cmip6")
  # Save each data frame in dfs with its updated name
  for (name in names(df_weather)) {
    # Get the data frame from the dfs list
    df <- df_weather[[name]]
    
    # Construct the file name with the updated name and desired file extension
    file_name <- paste0(name, ".csv")
    
    # Save the data frame as a CSV file
    write.csv(df, file = file_name, row.names = FALSE)
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  require(stringr)

# Assuming you have a list of data frames called dfs with unique names
# For example:
dfs_sample <- list("Adraskan_ssp126.BCC-CSM2-MR.2050.csv" = data.frame(x = 1:3),
                   "Adraskan_ssp126.BCC-CSM2-MR.2085.csv" = data.frame(y = 4:6),
                   "Adraskan_ssp126.GFDL-ESM4.2050.csv" = data.frame(z = 7:9))

# Rename the data frames by moving the last part (Year) before the third part (Model)
new_names <- lapply(names(dfs_sample), function(name) {
  parts <- str_split(name, "\\.")
  year <- parts[[1]][length(parts[[1]]) - 1]  # Extract the Year part
  model <- parts[[1]][length(parts[[1]])]  # Extract the Model part
  
  # Reconstruct the name by moving the Year part before the Model part
  new_name <- str_replace(name, paste0("\\.", year, "\\.", model), paste0("_", year, ".", model, "."))
  return(new_name)
})

# Update the names of the data frames
names(dfs_sample) <- unlist(new_names)

# Set the directory where you want to save the files
setwd("D:/Rdata/Chill_quantification/Future_scenarios/Generated_weather_cmip6")

# Save each data frame in dfs_sample with its updated name
for (name in names(dfs_sample)) {
  # Get the data frame from the dfs_sample list
  df <- dfs_sample[[name]]
  
  # Construct the file name with the updated name and desired file extension
  file_name <- paste0(name, ".csv")
  
  # Save the data frame as a CSV file
  write.csv(df, file = file_name, row.names = FALSE)
}

