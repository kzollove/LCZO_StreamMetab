#SM_make_prelim_data

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

write_rds(RI_usgs_discharge, path = './data/RI_data_odm2_USGS')

RI_data <- RI_data %>%
  full_join(RI_usgs_discharge, by="datetime")

write_rds(QS_data, path = './data/raw/QS_data_odm2')
write_rds(QP_data, path = './data/raw/QP_data_odm2')
write_rds(RI_data, path = './data/raw/RI_data_odm2')

QS_data <- read_rds(path = './data/raw/QS_data_odm2')
RI_data <- read_rds(path = './data/raw/RI_data_odm2')
QP_data <- read_rds(path = './data/raw/QP_data_odm2')

QP_data <- handle_DO(QP_data, 384)
QS_data <- handle_DO(QS_data, 387)
RI_data <- handle_DO(RI_data, 693)

QP_data <- handle_Depth(QP_data)
QS_data <- handle_Depth(QS_data)
RI_data <- handle_Depth(RI_data)

QP_data <- handle_Light(QP_data)
QS_data <- handle_Light(QS_data)
RI_data <- handle_Light(RI_data)


QS_data <- remove_leading_trailing_NA(QS_data)
QP_data <- remove_leading_trailing_NA(QP_data)
RI_data <- remove_leading_trailing_NA(RI_data)

QS_data <- make_final(QS_data)
QP_data <- make_final(QP_data)
RI_data <- make_final(RI_data)

prelim_charts_full(QS_data) %>% ggsave(filename = "QS_prelims_calc.pdf", device = "pdf", path = "./plots")
prelim_charts_full(QP_data) %>% ggsave(filename = "QP_prelims_calc.pdf", device = "pdf", path = "./plots")
prelim_charts_full(RI_data) %>% ggsave(filename = "RI_prelims_calc.pdf", device = "pdf", path = "./plots")

write_rds(QS_data, "./data/prelims/QS_data_final")
write_rds(QP_data, "./data/prelims/QP_data_final")
write_rds(RI_data, "./data/prelims/RI_data_final")
