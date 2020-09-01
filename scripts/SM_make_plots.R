#SM_make_plots


# Preliminary plots -------------------------------------------------------

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


# Data plots --------------------------------------------------------------

prediction_plots <- function(metab_set) {
  
  title <- paste(deparse(substitute(metab_set)), " Preliminaries")
  
  DO <- plot_DO_preds(metab_set) +
    theme(
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      axis.title.y = element_blank(),
      legend.title = element_blank()
    )
  
  metab <- plot_metab_preds(metab_set) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.title = element_blank()
    )
  
  
  ggarrange(DO, metab, ncol = 1, align = "v") %>% 
    annotate_figure(top = text_grob(title), left = text_grob("Predictions", rot = 90))
  
}
