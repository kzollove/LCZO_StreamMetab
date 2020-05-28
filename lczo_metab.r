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

rm(QP_data, QS_data, RI_data) # Clears your environment




# load environment --------------------------------------------------------



library(streamMetabolizer)
library(unitted)
library(lubridate)
library(tidyverse)
library(xts)
library(directlabels)
library(dataRetrieval)
library('RPostgreSQL')
library(ggpubr)

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
    # lat,lon = 18.3213, -65.8171
    # elev = 387
  ),
  QP = list(
    DO = "21",
    Temp = "22",
    Light = "16672", #8/30/16 --->
    Discharge = "18657"
    # lat,lon = 18.323088, -65.815087
    # elev = 384
  ),
  RI = list(
    DO = "17217", #2/20/17 --->
    Temp = "17218",
    Light = "16774", #9/3/16 --->
    Baro = "17270" #2/21/17 --->
    #lat,lon = 18.275448, -65.785497
    # elev = 693
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
                host="127.0.0.1", port=5433, dbname="ODM2LCZO")
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

df_to_depth <- function(df) {
  df %>% 
    mutate(Depth = calc_depth(Discharge))
}

df_to_dosat <- function(df) {
  df %>% 
    mutate(DO.sat = calc_DO_sat(temp.water = Temp, pressure.air = Baro))
}

kpa_to_mbar <- function(df) {
  df %>% 
    mutate(Baro = Baro * 10)
}

fillNA_baro <- function(df, elev) {
  df %>% 
    mutate(Baro_calc = calc_air_pressure(temp.air = Temp, elevation = elev)) %>% 
    mutate(Baro = ifelse(is.na(Baro), Baro_calc, Baro)) %>% 
    select(-Baro_calc)
}

datetime_to_solartime <- function(df, lon=-65.8, tz='Etc/GMT+5') {
  df %>% 
    mutate(datetime = force_tz(datetime, tz=tz)) %>% 
    mutate(solar.time = calc_solar_time(datetime, longitude = lon)) %>% 
    select(solar.time, everything(), -datetime)
}

light_to_PAR <- function(df) {
  df %>% 
    mutate(Light = Light * 0.0185)
}

fillNA_light <- function(df, lat = 18.3, lon = -65.8) {
  df %>% 
    mutate(Light_calc = calc_light(solar.time = solar.time, latitude = lat, longitude = lon)) %>% 
    mutate(Light = ifelse(is.na(Light), Light_calc, Light))
}

PAR_to_merged <- function(df, lat = 18.3, lon = -65.8) {
  PAR.obs <- df %>%
    mutate(solar.date = date(solar.time), solar.hour = hour(solar.time)) %>%
    group_by(solar.date, solar.hour) %>%
    summarize(light = mean(Light, na.rm = TRUE)) %>%
    mutate(solar.time = ymd_h(paste(solar.date, solar.hour))) %>% 
    ungroup() %>%
    select(solar.time, light)
  
  PAR.mod <- df %>%
    select(solar.time, light = Light_calc)
  
  PAR.merged <- calc_light_merged(PAR.obs = PAR.obs,
                                  solar.time = PAR.mod$solar.time,
                                  latitude=lat,
                                  longitude=lon,
                                  max.PAR = PAR.mod$light)
  
  df %>% 
    mutate(Light_merged = PAR.merged$light)

  ggplot(bind_rows(mutate(v(PAR.obs), type='obs'), mutate(v(PAR.mod), type='mod'),
                   mutate(v(PAR.merged), type='merged')) %>%
           mutate(type=ordered(type, levels=c('obs','mod','merged'))),
         aes(x=solar.time, y=light, color=type)) + geom_line() + geom_point() + theme_bw()

}

make_daily <- function(df) {
  df %>% 
    mutate(solar.time = as_date(solar.time)) %>%
    group_by(solar.time) %>%
    summarize(DO = mean(DO, na.rm = TRUE),
              Temp = mean(Temp, na.rm = TRUE),
              Light = mean(Light, na.rm = TRUE),
              Baro = mean(Baro, na.rm = TRUE),
              Discharge = mean(Discharge, na.rm = TRUE),
              Depth = mean(Depth, na.rm = TRUE),
              DO.sat = mean(DO.sat, na.rm = TRUE),
              Light_calc = mean(Light_calc, na.rm = TRUE),
              Light_merged = mean(Light_merged, na.rm = TRUE)
    )
}

remove_leading_trailing_NA <- function(df) {
  df %>%
    group_by(solar.time) %>%
    filter_at(
      vars(DO),
      all_vars(pmin(
        cumsum(!is.na(.)),
        rev(cumsum(!is.na(rev(.))))
        ) != 0)
      )
}

make_final <- function(df) {
  df %>% 
    select(solar.time,
           DO.obs = DO,
           DO.sat,
           depth = Depth,
           temp.water = Temp,
           light = Light
    )
}

prelim_charts_1 <- function(df) {
  df %>% unitted::v() %>%
    mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
    select(solar.time, starts_with('DO')) %>%
    gather(type, DO.value, starts_with('DO')) %>%
    mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
    ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() +
    facet_grid(units ~ ., scale='free_y') + theme_bw() +
    scale_color_discrete('variable')
}

prelim_charts_2 <- function(df) {
  labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)')
  df %>% unitted::v() %>%
    select(solar.time, depth, temp.water, light) %>%
    gather(type, value, depth, temp.water, light) %>%
    mutate(
      type=ordered(type, levels=c('depth','temp.water','light')),
      units=ordered(labels[type], unname(labels))) %>%
    ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() +
    facet_grid(units ~ ., scale='free_y') + theme_bw() +
    scale_color_discrete('variable')
}

prelim_charts_full <- function(df) {
  
  title <- paste(deparse(substitute(df)), " Preliminaries")
  
  one <- prelim_charts_1(df) +
    theme(
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      axis.title.y = element_blank(),
      legend.title = element_blank()
    )
  
  labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)')
  two <- prelim_charts_2(df) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.title = element_blank()
    )
  
  ggarrange(one, two, ncol = 1, align = "v") %>% 
    annotate_figure(top = text_grob(title))
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

QP_data <- QP_data %>% 
  mutate(Baro = calc_air_pressure(temp.air = Temp, elevation = 384))
QS_data <- kpa_to_mbar(QS_data)
RI_data <- kpa_to_mbar(RI_data)

QS_data <- fillNA_baro(QS_data, 387)
RI_data <- fillNA_baro(RI_data, 693)

QS_data <- cfs_to_m3s(QS_data)
QP_data <- cfs_to_m3s(QP_data)
RI_data <- cfs_to_m3s(RI_data)

QS_data <- df_to_depth(QS_data)
QP_data <- df_to_depth(QP_data)
RI_data <- df_to_depth(RI_data)

QS_data <- df_to_dosat(QS_data)
QP_data <- df_to_dosat(QP_data)
RI_data <- df_to_dosat(RI_data)

QS_data <- datetime_to_solartime(QS_data)
QP_data <- datetime_to_solartime(QP_data)
RI_data <- datetime_to_solartime(RI_data)

QS_data <- light_to_PAR(QS_data)
QP_data <- light_to_PAR(QP_data)
RI_data <- light_to_PAR(RI_data)

QS_data <- fillNA_light(QS_data)
QP_data <- fillNA_light(QP_data)
RI_data <- fillNA_light(RI_data)

QS_data <- PAR_to_merged(QS_data)
QP_data <- PAR_to_merged(QP_data)
RI_data <- PAR_to_merged(RI_data)


QS_data <- remove_leading_trailing_NA(QS_data)
QP_data <- remove_leading_trailing_NA(QP_data)
RI_data <- remove_leading_trailing_NA(RI_data)

QS_data <- make_final(QS_data)
QP_data <- make_final(QP_data)
RI_data <- make_final(RI_data)

prelim_charts_full(QS_data) %>% ggsave()
prelim_charts_full(QP_data)
prelim_charts_full(RI_data)



# TODO --------------------------------------------------------------------

#### TODO comparison of light with calc_light, Baro with calc_baro, etc.

#### TODO run streamMet as 15 min, hourly, bi-hourly

#### TODO run as a whole, run as distinct pieces

#### TODO how best to split among NA

#### TODO relate QP discharge to QS discharge

#### TODO run with different light regimes

#### TODO try with multiple GPP_fun/ER_fun arguments

#### TODO try multiple "pool_K600" arguments





# Q_daily <- Q %>% 
#   mutate(date = as_date(datetime)) %>% 
#   group_by(date) %>% 
#   summarize(discharge.daily = mean(discharge))
# 
# lnK <- Q_daily %>% 
#   transmute(lnK = log(Q_daily$discharge.daily * 2369.2 + 18.909)) #Calculate daily lnK from Q ~ K fit 
# lnK <- lnK$lnK
# 
# qs %>% 
#   subset(is.na(stage))
# 
# qs <- qs %>% 
#   filter(minute(datetime) %% 15 == 0) #Get rid of extra time points

# qs <- qs %>%
#   group_by(date(datetime)) %>% 
#   mutate(conc = ifelse(is.na(conc), mean(conc, na.rm = TRUE), conc))



make_final(QS_data) %>% unitted::v() %>%
  mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
  select(solar.time, starts_with('DO')) %>%
  gather(type, DO.value, starts_with('DO')) %>%
  mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
  ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() +
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')

labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)')
make_final(QS_data) %>% unitted::v() %>%
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
