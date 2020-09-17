#SM_gapfill

library(missForest)

QP_cond <- read_csv('./data/cond/Prieta_Cond.csv') %>%
  select(datetime = 1, cond = 4) %>% 
  filter(cond > 0)

QP_data_fill <- read_rds(path = './data/raw/QP_data_odm2') %>% 
  left_join(QP_cond) %>% 
  handle_Light() %>% 
  select(solar.time, cond, Discharge)

ggplot(QP_data_fill, aes(x = solar.time, y = Discharge)) +
  geom_line()+
  geom_line(aes(y = cond, color = 'red'))

   
QS_arc <- QS_data_fill %>% 
  filter(
    !is.na(cond)
  ) %>% 
  mutate(timepoint = row_number())
  
QS_data_mat <- as.matrix(
  QS_arc %>% select(
    timepoint, cond, Depth
  )
)
  
  
   

depth.imp<- missForest(QS_data_mat, maxiter = 10, ntree = 100,
                           variablewise = TRUE,decreasing = FALSE, verbose = FALSE, replace = TRUE,
                           classwt = NULL, cutoff = NULL, strata = NULL,
                           sampsize = NULL, nodesize = NULL, maxnodes = NULL,
                           xtrue = NA, parallelize = "no")
depthWM <- as_tibble(depth.imp$ximp) %>% 
  select(timepoint, filled_depth = Depth) %>% 
  full_join(QS_arc) %>%
  select(solar.time, filled_depth) %>% 
  full_join(QS_data_fill)
summary(depthWM)

ggplot(as_tibble(depthWM), aes(x = solar.time, y = Depth)) +
  geom_line(aes(y = cond/100)) +
  geom_line(aes(y = filled_depth, color = 'red', alpha = 0.2))
