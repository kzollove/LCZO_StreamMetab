#LCZO streamMetabolizer
#Running USGS streamMetabolizer package

# load environment --------------------------------------------------------
setwd("/Users/kylezollovenecek/Desktop/LCZO/LCZO_StreamMetab")
source('./scripts/SM_dependencies.R')
source('./scripts/SM_dataIDs.R')
source('./scripts/SM_convert_units.R')
source('./scripts/SM_ODM2.R')
source('./scripts/SM_make_plots.R')
theme_set(theme_light())






mle_run <- function(df) {
  hour_start <- hour(round_date(head(df$solar.time, 1), "30 mins"))
  minute_start <- minute(round_date(head(QS_data$solar.time, 1), "30 mins")) / 60
  
  hour_end <- hour(round_date(tail(df$solar.time, 1), "30 mins"))
  minute_end <- minute(round_date(tail(QS_data$solar.time, 1), "30 mins")) / 60
  
  day_start = (hour_start + minute_end) - 24
  day_end = 24 + (hour_end + minute_end)
  
  mm_name('mle', GPP_fun='linlight', ER_fun='constant') %>%
    specs(day_start = day_start, day_end = day_end) %>%
    metab(df)
}

# TODO --------------------------------------------------------------------

#### TODO calculate depth from widths and depths (PULSE)
## average of depth vs. stream gage/discharge

#### TODO comparison of light with calc_light, Baro with calc_baro, etc.

#### TODO scale calc_light down with to match collected light 

#### TODO run streamMet

# Secondary TODO ----------------------------------------------------------

#### TODO run streamMet as 15 min, hourly, bi-hourly

#### TODO run as a whole, run as distinct pieces

#### TODO how best to split among NA

#### TODO relate QP discharge to QS discharge

#### TODO run with different light regimes

#### TODO try with multiple GPP_fun/ER_fun arguments

#### TODO try multiple "pool_K600" arguments


# Extra TODO --------------------------------------------------------------

#### TODO refactor into different scripts

#### TODO make lczoMetabolizer





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


QS_data <- read_rds("./data/prelims/QS_data_final")
QP_data <- read_rds("./data/prelims/QP_data_final")
RI_data <- read_rds("./data/prelims/RI_data_final")


DO_data <- QS_data %>%
  full_join(QP_data, by = 'solar.time') %>% 
  full_join(RI_data, by = 'solar.time') %>% 
  select(solar.time,
         DO_QS = DO.obs.x,
         DO_QP = DO.obs.y,
         DO_RI = DO.obs,
         depth_QS = depth.x,
         depth_QP = depth.y,
         depth_RI = depth) %>% 
  gather(
    type,
    do_value,
    DO_QS:DO_RI
  )

DO_data %>% 
  filter(solar.time >= '2017-08-01', solar.time < '2018-04-01') %>% 
  ggplot(aes(x = solar.time, y = do_value, color = type)) + 
  geom_line() +
  geom_line(aes(y = depth_QP), linetype = 'dashed')
  
  
mle_QS <- mle_run(QS_data[!duplicated(QS_data[c('solar.time')]),])
mle_QP <- mle_run(QP_data)
mle_RI <- mle_run(RI_data)


mle_QS %>% write_rds('./data/calculated/mle_QS')
mle_QP %>% write_rds('./data/calculated/mle_QP')
mle_RI %>% write_rds('./data/calculated/mle_RI')

get_params(mle_RI) %>%
  select(date, warnings, errors)






# bayes_name <- mm_name(type='bayes', pool_K600="binned", err_obs_iid=TRUE, err_proc_iid=TRUE)
# bayes_specs <- specs(bayes_name, K600_lnQ_nodes_centers = log(1:16), K600_lnQ_nodes_meanlog = lnK, K600_lnQ_nodes_sdlog = 0.00001, K600_lnQ_nodediffs_sdlog = 1000)
# 
# mm <- metab(bayes_specs, data=qs_dat, data_daily=Q_daily)
# 
# mm



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




