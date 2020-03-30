list_of_packages <- c("remotes")
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages, dependencies = TRUE)

list_of_appling_packages <- c("unitted")
new_appling_packages <- list_of_appling_packages[!(list_of_appling_packages %in% installed.packages()[,"Package"])]
new_appling_packages <- paste("appling/", new_appling_packages, sep="")
if(length(new_appling_packages)) remotes::install_github(new_appling_packages, INSTALL_opts = '--no-lock')
#Based off of: remotes::install_github('appling/unitted', INSTALL_opts = '--no-lock') 

list_of_usgs_packages <- c("streamMetabolizer")
new_usgs_packages <- list_of_usgs_packages[!(list_of_usgs_packages %in% installed.packages()[,"Package"])]
new_usgs_packages <-  paste("USGS-R/", new_usgs_packages, sep="")
if(length(new_usgs_packages)) remotes::install_github(new_usgs_packages, INSTALL_opts = '--no-lock')
#Based off of: remotes::install_github("USGS-R/streamMetabolizer", INSTALL_opts = '--no-lock')

#If receiving error "failed to lock directory"
#Try unlink(<directory-that-is-00LOCK>, recursive = TRUE)

##### FOR ISSUE ON FITTING Ks by Q:
#https://github.com/USGS-R/streamMetabolizer/issues/373
#####

#When preparing data,
#Access ?mm_data for help


library(streamMetabolizer)
library(tidyverse)

depth <- read_csv("./data/QS_depth_190404.csv")
DO <- read_csv("./data/QS_DO_190404.csv")
light <- read_csv("./data/QS_light_190404.csv")
temp <- read_csv("./data/QS_temp_190404.csv")
