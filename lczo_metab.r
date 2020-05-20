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
library(directlabels)
library('RPostgreSQL')

# Define series IDs for parameters:

series.datetime.full <- "valuedatetime >= '2015-01-01 00:00:00' and valuedatetime <='2020-01-01 00:00:00'"
series.datetime.drought <- "valuedatetime >= '2015-01-01 00:00:00' and valuedatetime <='2016-01-01 00:00:00'"
series.datetime.hurricane <- "valuedatetime >= '2017-01-01 00:00:00' and valuedatetime <='2019-01-01 00:00:00'"

QS.series.DO <- "7"
QS.series.Temp <- "8"
QS.series.Light <- "16675" #8/03/16 --->
QS.series.Baro <- "16154" #01/12/16 --->
QS.series.Depth <- #Will have to convert from Baro using manual readings
QP.series.DO <- "21"
QP.series.Temp <- "22"
QP.series.Light <- "16672" #8/30/16 --->
QP.series.Baro <- #Will convert from QS
QP.series.Depth <- "18560"
RI.series.DO <- "17217" #2/20/17 --->
RI.series.Temp <- "17218" 
RI.series.Light <- "16774" #9/3/16 --->
RI.series.Baro <- "17270" #2/21/17 --->
RI.series.Depth <- ""



# Connect to local DB and download data -----------------------------------
# Must have db "ODM2LCZO" in Postgres
# Remember to turn local database on

con <- dbDriver("PostgreSQL") %>% 
dbConnect(pg, user="postgres", password="Cranmore12",
                host="127.0.0.1", port=5432, dbname="ODM2LCZO")

### TODO write a new query ###
#Sample Query
dbData <-  dbGetQuery(con, "select to_char(valuedatetime, 'MM-DD-YYYY HH24:MI:SS') as valuedateandtime, datavalue 
                     from odm2.timeseriesresultvalues where resultid=18560 and valuedatetime >= '2015-01-01 00:00:00' and valuedatetime <='2019-01-01 00:00:00'
                          order by valuedatetime") # and valuedatetime >= '2015-01-01 00:00:00' and valuedatetime <='2018-01-01 00:00:00'

# This data is not yet from database
# Need to make a good system of downloading and organizing from database





# Structure ---------------------------------------------------------------

##### TIME PERIODS
## Pre-Drought
## Drought
## Post-Drought
## Pre Hurricane
## Hurricane
## Post Hurricane
##### SITES
# Rio Icacos
# Quebrada Sonadora
# Quebrada Prieta

# TODO --------------------------------------------------------------------
##### Define time periods
## Pre-Drought
# From Gutierrez et al
## Drought
# From Gutierrez et al
## Post-Drought
# From Gutierrez et al
## Pre Hurricane
## Hurricane
## Post Hurricane

##### Download data from db for time periods
### Define data system
## Pre-Drought
## Drought
## Post-Drought
## Pre Hurricane
## Hurricane
## Post Hurricane

##### Refactor current code to run for all new data
### Tidying
## Merging tables
## Daily discharges



depth <- read_csv("./data/depth.csv")
DO <- read_csv("./data/DO.csv")
light <- read_csv("./data/light.csv")
temp <- read_csv("./data/temp.csv")
pres <- read_csv("./data/AbPr.csv")
Q <- read_csv("./data/discharge.csv")

#This function is necessary to make future functions "pipeable"
get_orig_name <- function(df){
  i <- 1
  while(!("chain_parts" %in% ls(envir=parent.frame(i))) && i < sys.nframe()) {
    i <- i+1
  }
  list(name = deparse(parent.frame(i)$lhs), output = df)
}

#Function to select datetime and variable of interest from each individual csv
select_value <- function(df) {

  value_name <- ifelse(
    quo_name(deparse(substitute(df))) == ".",
    get_orig_name(df)$name,
    quo_name(deparse(substitute(df)))
    ) 

  select(
    df,
    datetime = `Date and Time`,
    !! value_name := names(df)[4]
  )
}

depth <- depth %>% select_value()
DO <- DO %>% select_value()
light <- light %>% select_value()
temp <- temp %>% select_value()
pres <- pres %>% select_value()
Q <- Q %>% select_value()

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

# qs <- qs %>%
#   group_by(date(datetime)) %>% 
#   mutate(conc = ifelse(is.na(conc), mean(conc, na.rm = TRUE), conc))

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
  specs(day_start = 1.5, day_end = 25.5) %>% #start at 3/29 03:00 and end 4/04 03:00
  metab(qs_dat)
mm_classic

get_params(mm_classic) %>%
  select(date, warnings, errors)


# bayes_name <- mm_name(type='bayes', pool_K600="binned", err_obs_iid=TRUE, err_proc_iid=TRUE)
# bayes_specs <- specs(bayes_name, K600_lnQ_nodes_centers = log(1:16), K600_lnQ_nodes_meanlog = lnK, K600_lnQ_nodes_sdlog = 0.00001, K600_lnQ_nodediffs_sdlog = 1000)
# 
# mm <- metab(bayes_specs, data=qs_dat, data_daily=Q_daily)
# 
# mm

plot_DO_preds(mm_classic)

K600_compare <- Q_daily %>% 
  mutate(
    date = Q_daily$date,
    LCZO = Q_daily$discharge.daily * 2369.2 + 18.909,
    Fit = mm_classic@fit$K600.daily
    ) %>% 
  gather(key = "K600_origin", value = "K600", LCZO, Fit)

ggplot(K600_compare, aes(x = date, y = K600, group = K600_origin, color = K600_origin)) +
  geom_line() + 
  scale_colour_discrete(guide = 'none') +
  scale_x_date(expand=c(0, 2)) +
  geom_dl(aes(label = K600_origin), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.98)) +
  labs(
  title = 'K600',
  subtitle = 'LCZO and Fit',
  y = 'K600'
)

plot_metab_preds(mm_classic)

kmodel_data

metab_Kmodel(
  specs = specs(mm_name('Kmodel', engine = 'mean', predictors = 'discharge.daily')),
  data = mm_data
)
