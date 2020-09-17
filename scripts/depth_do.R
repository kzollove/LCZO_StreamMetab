#Depth/DO

QP_data %>% head()

DO_data <- QS_data %>%
  full_join(QP_data, by = 'solar.time') %>% 
  full_join(RI_data, by = 'solar.time') %>% 
  select(solar.time,
         QS = DO.obs.x,
         QP = DO.obs.y,
         RI = DO.obs) %>% 
  gather(
    site,
    DO,
    QS:RI
  )

depth_data <- QS_data %>%
  full_join(QP_data, by = 'solar.time') %>% 
  full_join(RI_data, by = 'solar.time') %>% 
  select(
QS = depth.x,
QP = depth.y,
RI = depth) %>% 
  gather(
    site,
    depth,
    QS:RI
  )

DO_full <- full_join(DO_data, depth_data)

DO_full %>% 
  filter(solar.time >= '2017-08-01', solar.time < '2018-04-01') %>% 
  ggplot(aes(x = solar.time, y = DO, color = site)) + 
  geom_line() 

+
  geom_line(aes(y = depth_QP), linetype = 'dashed')