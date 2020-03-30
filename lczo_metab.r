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

param_list <-  c("depth", "DO", "light", "temp")

depth <- depth %>% 
  select(datetime = names(depth)[1],
         stage = `Stage Height -unit-ft-processing level-L0`)
head(names(temp)[1])
DO <- DO %>% 
  select(datetime = `Date and Time`,
         conc = `DO Adj Conc -unit-mg/L-processing level-L1 passed QAQC`)

light <- light %>% 
  select(datetime = `Date and Time`,
         lux = `Light Intensity -unit-Lux-processing level-L0`)

temp <- temp %>% 
  select(datetime = `Date and Time`,
         temp = `Water temperature -unit-C-processing level-L1 passed QAQC`)

qs <- depth %>% 
  full_join(DO, by="datetime") %>% 
  full_join(temp, by="datetime") %>% 
  full_join(light, by="datetime")


qs
