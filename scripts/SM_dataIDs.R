#SM_dataIDs

#List object of all ODM2 Data series IDs
#Baros from calc_air_pressure (will check with QS and RI baro to see how they compare)
#Depths derived from discharge
#Try calc_light_merged


data_id_L0 <- list(
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

data_id_L1 <- list(
  QS = list(
    DO = "7", #<- L0, L1 -> 16158
    Temp = "16157",
    Light = "17280", #8/03/16 --->
    Baro = "16154", #01/12/16 ---> #L0
    Discharge = "18641" #09/05/16 --->
    # lat,lon = 18.3213, -65.8171
    # elev = 387
  ),
  QP = list(
    DO = "18614",
    Temp = "18703",
    Light = "18625", #8/30/16 --->
    Discharge = "18668"
    # lat,lon = 18.323088, -65.815087
    # elev = 384
  ),
  RI = list(
    DO = "17217", #2/20/17 ---> L0; try 17220 for Adj Conc
    Temp = "17218", #L0
    Light = "18633", #9/3/16 --->
    Baro = "17270" #2/21/17 ---> L0
    #lat,lon = 18.275448, -65.785497
    # elev = 693
  )
)