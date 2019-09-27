# Hunting for failure modes
library(ggplot2)
library(MazamaSpatialUtils)
initializeMazamaSpatialUtils()
#loadSpatialData("SimpleCountriesEEZ") 

setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")
pas <- pas_load()

#---- Washington station
pas_wa <- pas %>% pas_filter(stateCode == "WA")
pat_wa <- pat_createNew(pas_wa, label = "MV Clean Air Ambassador @ Willowbrook Farm")
pat_multiplot(pat_wa)
pat_scatterplot(pat_wa)
all_parameters <- c("datetime", "pm25_A",
                    "pm25_B", "temperature", "humidity",
                    "uptime", "adc0", "rssi")
pat_scatterplot(pat_wa, parameters = all_parameters)
pat_dygraph(pat_wa)

agg_wa <- pat_aggregate(pat_wa)
chanBmean_wa <- agg_wa$pm25_B_mean
tempsd_wa <- agg_wa$temperature_sd

df_wa <- pat_wa$data
plot(agg_wa$temperature_sd)

ggplot(NULL ) +
  geom_point(data = df_wa, aes(datetime, pm25_A), color = "red")+
  geom_point(data = df_wa, aes(datetime, pm25_B), color = "blue") +
  geom_point(data = agg_wa, aes(datetime, humidity_sd), color = "lightblue") 
  #geom_smooth(data = df_wa, aes(datetime, pm25_B*tempsd_wa), color = "lightblue", alpha = 0.5,
    #        method = "loess", span = 0.01)

#----- California station

pas_ca <- pas %>% pas_filter(stateCode == "CA")
pat_ca <- pat_createNew(pas_ca, label = "Orchard Avenue")

pat_multiplot(pat_ca)
pat_scatterplot(pat_ca)
pat_dygraph(pat_ca)
pat_scatterplot(pat_ca, parameters = all_parameters)

agg_ca <- pat_aggregate(pat_ca)
tempsd_ca <- agg_ca$temperature_sd
humidity_ca <- agg_ca$humidity_mean

df_ca <- pat_ca$data

ggplot(NULL ) +
  geom_point(data = df_ca, aes(datetime, pm25_A), color = "red")+
  geom_point(data = df_ca, aes(datetime, pm25_B), color = "blue") +
  geom_line(data = agg_ca, aes(datetime, humidity_sd), color = "lightblue")
  #geom_smooth(data = agg_ca, aes(datetime, temperature_sd), color = "lightblue", alpha = 0.5,
   #           method = "loess", span = 0.01)
  
#----- Utah station

pas_ut <- pas %>% pas_filter(stateCode == "UT")
pat_ut <- pat_createNew(pas_ut, label = "Breathe easy Farr West")

pat_multiplot(pat_ut)
pat_scatterplot(pat_ut)
pat_dygraph(pat_ut)
pat_scatterplot(pat_ut, parameters = all_parameters)

agg_ut <- pat_aggregate(pat_ut)
tempsd_ut <- agg_ut$temperature_sd

df_ut <- pat_ut$data

ggplot(NULL ) +
  geom_point(data = df_ut, aes(datetime, pm25_A), color = "red")+
  geom_point(data = df_ut, aes(datetime, pm25_B), color = "blue") +
  geom_line(data = agg_ut, aes(datetime, humidity_sd), color = "lightblue")

#----- Colorado station

pas_co <- pas %>% pas_filter(stateCode == "CO")
pat_co <- pat_createNew(pas_co, label = "Tumminaro Hangar")

pat_multiplot(pat_co)
pat_scatterplot(pat_co)
pat_dygraph(pat_co)
pat_scatterplot(pat_co, parameters = all_parameters)

agg_co <- pat_aggregate(pat_co)
tempsd_co <- agg_co$temperature_sd

df_co <- pat_co$data

ggplot(NULL ) +
  geom_point(data = df_co, aes(datetime, pm25_A), color = "red")+
  geom_point(data = df_co, aes(datetime, pm25_B), color = "blue") +
  geom_line(data = agg_co, aes(datetime, humidity_sd), color = "lightblue")
  #geom_smooth(data = df_co, aes(datetime, pm25_B*tempsd_co), color = "lightblue", alpha = 0.5,
   #           method = "loess", span = 0.01)

#----- North Dakota
pas_nd <- pas %>% pas_filter(stateCode == "ND")
pat_nd <- pat_createNew(pas_nd, label = "Reese Home")

pat_multiplot(pat_nd)
pat_scatterplot(pat_nd)
pat_dygraph(pat_nd)
pat_scatterplot(pat_nd, parameters = all_parameters)

agg_nd <- pat_aggregate(pat_nd)
tempsd_nd <- agg_nd$temperature_sd

df_nd <- pat_nd$data

ggplot(NULL ) +
  geom_point(data = df_nd, aes(datetime, pm25_A), color = "red")+
  geom_point(data = df_nd, aes(datetime, pm25_B), color = "blue") +
  geom_line(data = agg_nd, aes(datetime, humidity_sd), color = "lightblue")
#geom_smooth(data = df_co, aes(datetime, pm25_B*tempsd_co), color = "lightblue", alpha = 0.5,
#           method = "loess", span = 0.01)



#----- Mexico station

pas_mx <- pas_createNew(baseUrl = "https://www.purpleair.com/json",
                        countryCodes = c('MX'), lookbackDays = 6)
pat_mx <- pat_createNew(pas_mx, label = "Rufino Tamayo")

pat_multiplot(pat_mx)
pat_scatterplot(pat_mx)
pat_dygraph(pat_mx)
pat_scatterplot(pat_mx, parameters = all_parameters)

agg_mx <- pat_aggregate(pat_mx)
tempsd_mx <- agg_mx$temperature_sd

df_mx <- pat_mx$data

ggplot(NULL ) +
  geom_point(data = df_mx, aes(datetime, pm25_A), color = "red")+
  geom_point(data = df_mx, aes(datetime, pm25_B), color = "blue") +
  #geom_point(data = df, aes(datetime, pm25_B*1.5), color = "lightblue", alpha = 0.5)+
  #geom_line(data = agg, aes(datetime, pm25_B_mean), color = "lightblue", alpha =0.5)+
  #geom_line(data = agg, aes(datetime, pm25_A_mean), color = "pink", alpha =0.8)+
  geom_point(data = df_mx, aes(datetime, pm25_B*tempsd_mx), color = "lightblue", alpha = 0.5)


