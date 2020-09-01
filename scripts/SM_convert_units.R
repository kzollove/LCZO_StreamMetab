#SM_


# Unit Converters ---------------------------------------------------------

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


# Fill NAs ----------------------------------------------------------------

fillNA_baro <- function(df, elev) {
  df %>% 
    mutate(Baro_calc = calc_air_pressure(temp.air = Temp, elevation = elev)) %>% 
    mutate(Baro = ifelse(is.na(Baro), Baro_calc, Baro)) %>% 
    select(-Baro_calc)
}

fillNA_light <- function(df, lat = 18.3, lon = -65.8) {
  df %>% 
    mutate(Light_calc = calc_light(solar.time = solar.time, latitude = lat, longitude = lon)) %>% 
    mutate(Light = ifelse(is.na(Light), Light_calc/2, Light))
}


# Handlers ----------------------------------------------------------------

handle_Depth <- function(df) {
  df <- cfs_to_m3s(df)
  df <- df_to_depth(df)
}

handle_DO <- function(df, elev=384) { #interprets Baro as Kpa units
  if ("Baro" %in% names(df)) {
    df <- kpa_to_mbar(df)
  } else {
    df <- df %>% 
      mutate(Baro = calc_air_pressure(temp.air = Temp, elevation = elev)) 
  }
  
  df <- fillNA_baro(df, elev)
  df <- df_to_dosat(df)
}

handle_Light <- function(df, lat = 18.3, lon = -65.8, tz='Etc/GMT+5') {
  df <- datetime_to_solartime(df, lon = lon, tz = tz)
  df <- light_to_PAR(df)
  df <- fillNA_light(df, lat = lat, lon = lon)
}
