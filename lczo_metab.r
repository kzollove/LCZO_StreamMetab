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
library(dataRetrieval)
library('RPostgreSQL')

#List object of all ODM2 Data series IDs
#Baros from calc_air_pressure (will check with QS and RI baro to see how they compare)
#Depths derived from discharge
#Try calc_light_merged
data_id <- list(
  QS = list(
    DO = "7",
    Temp = "8",
    Light = "16675", #8/03/16 --->
    Baro = "16154", #01/12/16 --->
    Discharge = "17274" #09/05/16 --->
  ),
  QP = list(
    DO = "21",
    Temp = "22",
    Light = "16672", #8/30/16 --->
    Discharge = "18657"
    # elev = 384
  ),
  RI = list(
    DO = "17217", #2/20/17 --->
    Temp = "17218",
    Light = "16774", #9/3/16 --->
    Baro = "17270" #2/21/17 --->
  )
)


# Connect to local DB and download data -----------------------------------
# Must have db "ODM2LCZO" in Postgres

get_ODM2_data <- function(
  series = '',
  start = '2015-01-01 00:00:00',
  end = '2019-01-01 00:00:00',
  site = '',
  parameter = ''
  ) {
  
  if (!exists('con')) {
    library('RPostgreSQL')
    con <- dbDriver("PostgreSQL") %>% 
      dbConnect(user="postgres", password="Cranmore12",
                host="127.0.0.1", port=5432, dbname="ODM2LCZO")
  }
  
  if(series == '') {
    series <-  data_id[[site]][[parameter]]
  }
  
  query <- str_interp(
    "select to_char(valuedatetime, 'MM-DD-YYYY HH24:MI:SS')
      as valuedateandtime, datavalue 
      from odm2.timeseriesresultvalues
      where resultid=${series}
      and valuedatetime >= '${start}'
      and valuedatetime <= '${end}'
    order by valuedatetime"
  )
  
    dbGetQuery(con, query)
}

get_all_ODM2 <- function(
  site,
  start = '2015-01-01 00:00:00',
  end = '2019-01-01 00:00:00'
  ) {
  for (i in seq_along(data_id[[site]])) {
    if (data_id[[site]][[i]] != '') {
      if(exists("build_df")) {
        value_name <- quo_name(names(data_id[[site]][i]))
        build_df <- build_df %>%
          full_join(
            get_ODM2_data(series = data_id[[site]][[i]], start = start, end = end) %>%
              select(
                valuedateandtime,
                !! value_name := datavalue
              ),
            by = "valuedateandtime"
          )
      } else {
        value_name <- quo_name(names(data_id[[site]][i]))
        build_df <- get_ODM2_data(series = data_id[[site]][[i]], start = start, end = end) %>% 
          select(
            valuedateandtime,
            !! value_name := datavalue
          )
      }
    }
  }
  final_df <- build_df %>%
    mutate(
      datetime = mdy_hms(valuedateandtime)
    ) %>%
    select(datetime, everything(), -valuedateandtime)
  
  final_df
}

cfs_to_m3s <- function(df) {
  df %>% 
    mutate(Discharge = Discharge / 35.3147)
}



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
#### TODO incorporate calc_depth()
## calc_depth() takes discharge and estimates depth using hydraulic geometry coefficients
# this seems to be elaborated on in Raymond et al 2012


##### Refactor current code to run for all new data
### Tidying
## Merging tables
## Daily discharges


#This function is necessary to make future functions "pipeable"
get_orig_name <- function(df){
  i <- 1
  while(!("chain_parts" %in% ls(envir=parent.frame(i))) && i < sys.nframe()) {
    i <- i+1
  }
  list(name = deparse(parent.frame(i)$lhs), output = df)
}

#Function to select datetime and variable of interest from each individual csv
select_value_odmCSV <- function(df) {

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

QS_data <- get_all_ODM2("QS")
RI_data <- get_all_ODM2("RI")
QP_data <- get_all_ODM2("QP")

RI_usgs_discharge <- readNWISdata( #discharge in cfs
  sites="50075000", service="iv", 
  parameterCd="00060", 
  startDate="2015-01-01T00:00Z",endDate="2019-01-01T00:00Z"
) %>% 
  select(
    datetime = dateTime,
    Discharge = `X_00060_00000`
  )

RI_data <- RI_data %>% 
  full_join(RI_usgs_discharge, by="datetime")

QS_data <- cfs_to_m3s(QS_data)
QP_data <- cfs_to_m3s(QP_data)
RI_data <- cfs_to_m3s(RI_data)




Q_daily <- Q %>% 
  mutate(date = as_date(datetime)) %>% 
  group_by(date) %>% 
  summarize(discharge.daily = mean(discharge))

lnK <- Q_daily %>% 
  transmute(lnK = log(Q_daily$discharge.daily * 2369.2 + 18.909)) #Calculate daily lnK from Q ~ K fit 
lnK <- lnK$lnK

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
