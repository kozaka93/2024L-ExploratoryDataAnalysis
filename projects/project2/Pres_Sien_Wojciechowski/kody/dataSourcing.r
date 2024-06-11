library(dplyr)
library("sf")

temp <- tempfile()

for (i in 0:365) {
  temp <- tempfile()
  fileName <- paste("https://coast.noaa.gov/htdata/CMSP/AISDataHandler/2020/AIS_",
                    gsub("-", "_", as.Date(i, origin = "2020-01-01")),".zip",sep = "")
  download.file(fileName, temp)
  df_op <- read.csv(unz(temp, paste("AIS_", gsub("-", "_", as.Date(i, origin = "2020-01-01")), ".csv", sep = "")),header = T, sep=",")
  df_op <- df_op %>% select(MMSI, VesselName, BaseDateTime, LAT, LON, SOG, Cargo) %>%
    filter(VesselName != "" & SOG > 1 & !is.na(Cargo) & Cargo >0)
  final_df <- data.frame()
  for (name in unique(df_op$VesselName)){
    logs <- df_op %>% filter(VesselName == name)
    shipLog <- rbind(logs[1,], tail(logs, n=1))
    final_df <- rbind(final_df, shipLog)
  }
  write.csv(final_df, paste("F:\\DataProject\\dataCleen\\", as.Date(i, origin = "2020-01-01"), ".csv", sep = ""), row.names=FALSE)
  unlink(temp)
}


