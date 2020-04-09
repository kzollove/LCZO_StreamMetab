#install.packages(remotes)
#remotes::install_github('appling/unitted', INSTALL_opts = '--no-lock') 
#remotes::install_github("USGS-R/streamMetabolizer", INSTALL_opts = '--no-lock')

#If receiving error "failed to lock directory"
#Try unlink(<directory-that-is-00LOCK>, recursive = TRUE)

#remove.packages('rstan')
#NEXT LINE WILL REMOVE YOUR .RData file! IF YOU CARE ABOUT THAT DONT RUN THIS LINE
#if (file.exists(".RData")) file.remove(".RData") 
#install.packages('rstan', repos = "https://cloud.r-project.org/", dependencies = TRUE) #This is specifically for bayesian

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

lnK <- Q_daily %>% 
  transmute(lnK = log(Q_daily$discharge.daily * 2369.2 + 18.909)) #Calculate daily lnK from Q ~ K fit 
lnK <- lnK$lnK

qs <- depth %>% 
  full_join(DO, by="datetime") %>% 
  full_join(temp, by="datetime") %>% 
  full_join(light, by="datetime") %>% 
  full_join(pres, by="datetime")

#qs <- qs %>% 
 # drop_na()

qs %>% 
  subset(is.na(stage))

qs_daily <- qs %>% 
  mutate(date = as_date(datetime)) %>%
  group_by(date) %>%
  summarize(conc = mean(conc, na.rm = TRUE),
            temp = mean(temp, na.rm = TRUE),
            stage = mean(stage, na.rm = TRUE),
            lux = mean(lux, na.rm = TRUE),
            pres = mean(pres, na.rm = TRUE)
            )

qs <- qs %>% 
  filter(minute(datetime) %% 15 == 0) #Get rid of extra time points

qs <- qs %>%
  group_by(date(datetime)) %>% 
  mutate(conc = ifelse(is.na(conc), mean(conc, na.rm = TRUE), conc))

qs <- qs %>% 
  mutate(pres_mbar = pres * 10, #convert kPa to mbar
         light = lux * 0.0185 #convert lux to ppfd
         ) 

qs <- qs %>% #Use internal function to calulated DO.sat
  mutate(DO.sat = calc_DO_sat(temp=u(qs$temp, "degC"), press=u(qs$pres_mbar, "mb"), sal=u(0, "PSU")))

qs$datetime <- qs$datetime %>% 
  force_tz(tz='Etc/GMT+5') #Assign timezone (EST, no daylight savings)

qs$solar.time <- calc_solar_time(qs$datetime, longitude = -65.8) #Calculate solar time

#qs$cal_light <- calc_light(u(qs$solar.time), u(18.32, 'degN'), u(-65.81, 'degE'), u(4282.162, 'umol m^-2 s^-1'), attach.units=TRUE)

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

#dat <- data_metab(num_days="3", res='15', attach.units = TRUE)

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
  mm_name('mle', GPP_fun='linlight', ER_fun='constant') %>%
  specs(day_start = 3.5, day_end = 27.5) %>% #start at 3/29 03:00 and end 4/04 03:00
  metab(qs_dat)
mm_classic

get_params(mm_classic) %>%
  select(date, warnings, errors)

bayes_name <- mm_name(type='bayes', pool_K600="binned", err_obs_iid=TRUE, err_proc_iid=TRUE)
bayes_specs <- specs(bayes_name, K600_lnQ_nodes_centers = log(1:7), K600_lnQ_nodes_meanlog = lnK, K600_lnQ_nodes_sdlog = 0.00001, K600_lnQ_nodediffs_sdlog = 1000) 

mm <- metab(bayes_specs, data=qs_dat, data_daily=Q_daily)


plot_DO_preds(mm)

View(qs)
