#SM_ODM2

if (!exists('con')) {
  library('RPostgreSQL')
  con <- dbDriver("PostgreSQL") %>% 
    dbConnect(user="postgres", password="Cranmore12",
              host="127.0.0.1", port=5433, dbname="ODM2LCZO")
}

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
  end = '2019-01-01 00:00:00',
  data_id = data_id_L1
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


#This function is necessary to make future functions "pipeable"
#It is possible it does not get used
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