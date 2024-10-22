#directory path to dropbox

Drive <- Sys.getenv("HOME")

#Drive <- gsub("\\\\", "/", Drive)
DriveDir <- file.path(Drive, "Urban Malaria Proj Dropbox", "urban_malaria")
dhsDir <- file.path(DriveDir, "data")
# dropbox <- file.path(dhsDir, "/nigeria/urban_microstratification/shiny_app_data")
# results <- file.path(DriveDir, "projects/Manuscripts/ongoing/ShinyAppDevelopment/images/R")

# packges to use
list_of_packages <- c("stringr","ggplot2", "dplyr", "purrr", "haven", "tidyverse",
                      "readxl", "patchwork", "tidyr", "factoextra", "MASS", "broom",
                      "glm2", "viridis", "ggwordcloud")
read_install_pacakges <- function(packages = list_of_packages
){
  new_packages <- packages[!(list_of_packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new_packages)
  return(sapply(list_of_packages, require, character.only = TRUE))
}
read_install_pacakges()