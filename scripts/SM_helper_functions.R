#SM_helper_functions

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

remove_leading_trailing_NA <- function(df, param = 'DO') {
  df %>%
    group_by(solar.time) %>%
    filter_at(
      vars(param),
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
           light = Light,
           discharge = Discharge
    )
}