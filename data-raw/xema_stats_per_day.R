# Data from metereological automatic stations (XEMA) in Catalonia
#
# Data retrieved from the open data portal of the Catalan Government
# https://analisi.transparenciacatalunya.cat/Medi-Ambient/Metadades-variables-meteorol-giques/4fb2-n3yi
# https://analisi.transparenciacatalunya.cat/es/Medi-Ambient/Metadades-estacions-meteorol-giques-autom-tiques/yqwd-vj5e
#
# License
# From http://governobert.gencat.cat/ca/dades_obertes/dades-obertes-covid-19/:
# Avís legal: D’acord amb l’article 17.1 de la Llei 19/2014, la ©Generalitat de Catalunya permet la reutilització dels continguts i de les dades sempre que se'n citi la font i la data d'actualització i que no es desnaturalitzi la informació (article 8 de la Llei 37/2007) i també que no es contradigui amb una llicència específica.


library(dplyr)
library(lubridate)
library(RSocrata)

variables <- read.socrata("https://analisi.transparenciacatalunya.cat/resource/4fb2-n3yi.json")
metadata <- read.socrata("https://analisi.transparenciacatalunya.cat/resource/yqwd-vj5e.json")
metadata <- metadata[c("codi_estacio", "latitud", "longitud", "codi_municipi", "nom_municipi")]
colnames(metadata) <- c("station_code", "latitude", "longitude", "municipality_code", "municipality_name")

# Server uses dates in ISO-8601 format and UTC timezone. We are interested in CET timezones, as rest of the data.
date_init = format_ISO8601(with_tz(ymd_hms("2020-02-01 00:00:00", tz="CET"), tz="UTC"), usetz = FALSE)
date_end = format_ISO8601(with_tz(ymd_hms("2020-04-30 23:59:59", tz="CET"), tz="UTC"), usetz = FALSE)

# TODO: Use date_init & date_end as soon as this is solved https://stackoverflow.com/questions/61143734/date-trunc-ymd-in-cet-timezone-from-floating-timestamp-in-utc-using-soql-throu
wrong_date_init = format_ISO8601(ymd_hms("2020-03-01 00:00:00", tz="UTC"), usetz = FALSE)
wrong_date_end = format_ISO8601(ymd_hms("2020-04-30 23:59:59", tz="UTC"), usetz = FALSE)

RetrieveValueStatsPerDay <- function(date_init, date_end, variable_code, variable_prefix) {
  soql <- sprintf("SELECT date_trunc_ymd(data_lectura) as date_utc, codi_estacio as station_code, min(valor_lectura) as min_%s, avg(valor_lectura) as avg_%s, max(valor_lectura) as max_%s WHERE (data_lectura between '%s' and '%s' AND codi_variable='%s' AND codi_estat='V') GROUP BY date_utc, codi_estacio", variable_prefix, variable_prefix, variable_prefix, date_init, date_end, variable_code)
  url <- sprintf("https://analisi.transparenciacatalunya.cat/resource/nzvn-apee.json?$query=%s", soql)
  value_stats_per_day <- read.socrata(url);

  columns <- names(value_stats_per_day)
  numeric_columns <- columns[!columns %in% c("date_utc", "station_code")]
  value_stats_per_day[numeric_columns] <- sapply(value_stats_per_day[numeric_columns],as.numeric)
  return(value_stats_per_day)
}


xema_t_per_day <- RetrieveValueStatsPerDay(wrong_date_init, wrong_date_end, variables$codi_variable[variables$acronim == "T"], "t")
xema_rh_per_day <- RetrieveValueStatsPerDay(wrong_date_init, wrong_date_end, variables$codi_variable[variables$acronim == "HR"], "rh")
xema_ws2_per_day <- RetrieveValueStatsPerDay(wrong_date_init, wrong_date_end, variables$codi_variable[variables$acronim == "VV2"], "ws2")

xema_stats_per_day <- inner_join(xema_t_per_day, xema_rh_per_day, by=c("date_utc","station_code"))
xema_stats_per_day <- inner_join(xema_stats_per_day, xema_ws2_per_day, by=c("date_utc","station_code"))
xema_stats_per_day <- left_join(xema_stats_per_day, metadata, by=c("station_code"))


usethis::use_data(xema_stats_per_day, overwrite=TRUE)



