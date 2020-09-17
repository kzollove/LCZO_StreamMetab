setwd("/Users/kylezollovenecek/Desktop/LCZO/LCZO_StreamMetab")
source('./scripts/SM_helper_functions.R')

QS_data <- read_rds("./data/prelims/QS_data_final")
QP_data <- read_rds("./data/prelims/QP_data_final")
RI_data <- read_rds("./data/prelims/RI_data_final")

apr2018 <- mm_name('mle', GPP_fun='satlight', ER_fun='constant') %>%
  specs(day_start = -20, day_end = 4) %>%
  metab(QP_month %>% filter(solar.time <='2018-04-17'))


bayes_name <- mm_name(type='bayes', pool_K600="normal", err_obs_iid=TRUE, err_proc_iid=TRUE)
bayes_specs <- specs(bayes_name, K600_daily_meanlog_meanlog=0, K600_daily_meanlog_sdlog=66) 

bayes_mm <- metab(bayes_specs, data=QS_month)

QP_data_raw <- read_rds("./data/raw/QP_data_odm2")
QP_month_raw <- QP_data_raw %>% filter(datetime >= '2018-09-01', datetime < '2018-10-01')

QP_month_raw %>%
  filter(datetime < '2018-09-03') %>% 
  ggplot(aes(x = datetime, y = DO)) +
  geom_line() +
  geom_line(aes(y = 8+Light/28000), col = 'red')



QS_month <- QS_data %>% filter(solar.time >= '2018-04-01', solar.time < '2018-05-01')
QP_month <- QP_data %>% filter(solar.time >= '2018-04-01', solar.time < '2018-10-01')

QP_month_hourly <- QP_month %>%
  mutate(
    Hour = hour(solar.time),
    Day = day(solar.time)
  ) %>% 
  dplyr::group_by(Day, Hour) %>%
  summarize(across(everything(), mean, na.rm = TRUE))

QS_month_hourly <- QS_month %>%
  mutate(
    Hour = hour(solar.time),
    Day = day(solar.time)
  ) %>% 
  dplyr::group_by(Day, Hour) %>%
  summarize(across(everything(), mean, na.rm = TRUE))

QS_month_hourly %>%
  filter(solar.time < '2018-04-05') %>% 
  ggplot(aes(x = solar.time, y = DO.obs)) +
  geom_line() +
  geom_line(aes(y = 8+light/200), col = 'red')

QP_month %>%
  #filter(solar.time < '2018-04-10') %>% 
  ggplot(aes(x = solar.time, y = DO.sat)) +
  geom_line() #+
geom_line(aes(y = 8+light/200), col = 'red')


QP_data_raw%>%
  filter(datetime < '2018-04-10') %>% 
  ggplot(aes(x = datetime, y = DO)) +
  geom_line() +
  geom_line(aes(y = Discharge), col = 'red')