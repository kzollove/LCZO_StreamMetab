#install.packages(remotes)
#remotes::install_github('appling/unitted', INSTALL_opts = '--no-lock') 
#remotes::install_github("USGS-R/streamMetabolizer", INSTALL_opts = '--no-lock')

#If receiving error "failed to lock directory"
#Try unlink(<directory-that-is-00LOCK>, recursive = TRUE)

##### FOR ISSUE ON FITTING Ks by Q:
#https://github.com/USGS-R/streamMetabolizer/issues/373
#####

#When preparing data,
#Access ?mm_data for help


library(streamMetabolizer)
library(unitted)
library(lubridate)
library(tidyverse)
library(xts)

depth <- read_csv("./data/QS_depth_190404.csv")
DO <- read_csv("./data/QS_DO_190404.csv")
light <- read_csv("./data/QS_light_190404.csv")
temp <- read_csv("./data/QS_temp_190404.csv")
pres <- read_csv("./data/QS_AbPr_190404.csv")
Q <- read_csv("./data/QS_discharge_190404.csv")

depth <- depth %>% 
  select(datetime = names(depth)[1],
         stage = `Stage Height -unit-ft-processing level-L0`)

DO <- DO %>% 
  select(datetime = `Date and Time`,
         conc = `DO Adj Conc -unit-mg/L-processing level-L1 passed QAQC`)

light <- light %>% 
  select(datetime = `Date and Time`,
         lux = `Light Intensity -unit-Lux-processing level-L0`)

temp <- temp %>% 
  select(datetime = `Date and Time`,
         temp = `Water temperature -unit-C-processing level-L1 passed QAQC`)
pres <- pres %>% 
  select(datetime = `Date and Time`,
         pres = `Barometric pressure -unit-kPa-processing level-L0`)

Q <- Q %>% 
  select(datetime = `Date and Time`,
         discharge = `discharge -unit-cfs-processing level-L0`)
Q$discharge <- Q$discharge/35.314666 #Convert to m3/s

Q_daily <- Q %>% 
  mutate(date = as_date(datetime)) %>% 
  group_by(date) %>% 
  summarize(discharge.daily = mean(discharge))

qs <- depth %>% 
  full_join(DO, by="datetime") %>% 
  full_join(temp, by="datetime") %>% 
  full_join(light, by="datetime") %>% 
  full_join(pres, by="datetime")

qs <- qs %>% 
  drop_na()

qs <- qs %>% 
  mutate(pres_mbar = pres * 10, #convert kPa to mbar
         light = lux * 0.0185 #convert lux to ppfd
         ) 

qs <- qs %>% #Use internal function to calulated DO.sat
  mutate(DO.sat = calc_DO_sat(temp=u(qs$temp, "degC"), press=u(qs$pres_mbar, "mb"), sal=u(0, "PSU")))


qs$datetime <- qs$datetime %>% 
  force_tz(tz='Etc/GMT+5') #Assign timezone (EST, no daylight savings)

qs$solar.time <- calc_solar_time(qs$datetime, longitude = -65.8) #Calculate solar time

#qs now has all data
#we are going to create new, final dataframe that mirrors example

##POSSIBLE TODO:
####Figure out how to incorporate unitted

qs_dat <- qs %>% 
  select(solar.time,
         DO.obs = conc,
         DO.sat,
         depth = stage,
         temp.water = temp,
         light
         )

#qs_dat <-  qs_dat %>% drop_na() 

dat <- data_metab(num_days="3", res='15', attach.units = TRUE)

qs_dat %>% unitted::v() %>%
  mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
  select(solar.time, starts_with('DO')) %>%
  gather(type, DO.value, starts_with('DO')) %>%
  mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
  ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() +
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')

labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)')
qs_dat %>% unitted::v() %>%
  select(solar.time, depth, temp.water, light) %>%
  gather(type, value, depth, temp.water, light) %>%
  mutate(
    type=ordered(type, levels=c('depth','temp.water','light')),
    units=ordered(labels[type], unname(labels))) %>%
  ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() +
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')

mm_classic <- 
  mm_name('mle') %>% 
  specs(day_start = 3.5, day_end = 27.5) %>% #start at 3/29 03:00 and end 4/04 03:00
  metab(qs_dat_trunc)
mm_classic
