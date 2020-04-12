# Data from metereological automatic stations (XEMA) in Catalonia
#
# Data retrieved from the open data portal of the Catalan Government
# https://analisi.transparenciacatalunya.cat/Medi-Ambient/Dades-meteorol-giques-de-la-XEMA/nzvn-apee
# https://analisi.transparenciacatalunya.cat/Medi-Ambient/Metadades-variables-meteorol-giques/4fb2-n3yi
# https://analisi.transparenciacatalunya.cat/Medi-Ambient/Metadades-estacions-meteorol-giques-autom-tiques/yqwd-vj5e
#
# License https://www.meteo.cat/wpweb/avis-legal/


library(dplyr)
library(drake)
library(lubridate)
library(RSocrata)


# Server uses dates in ISO-8601 format and UTC timezone. We are interested in CET timezones, as rest of the data.
date_init = format_ISO8601(with_tz(ymd_hms("2020-02-01 00:00:00", tz="CET"), tz="UTC"), usetz = FALSE)
date_end = format_ISO8601(with_tz(ymd_hms("2020-04-30 23:59:59", tz="CET"), tz="UTC"), usetz = FALSE)

# TODO: Use date_init & date_end as soon as this is solved https://stackoverflow.com/questions/61143734/date-trunc-ymd-in-cet-timezone-from-floating-timestamp-in-utc-using-soql-throu
wrong_date_init = format_ISO8601(ymd_hms("2020-03-01 00:00:00", tz="UTC"), usetz = FALSE)
wrong_date_end = format_ISO8601(ymd_hms("2020-04-30 23:59:59", tz="UTC"), usetz = FALSE)

FixMetadata <- function(metadata) {
  fixed_metadata <- metadata[c("codi_estacio", "latitud", "longitud", "codi_municipi", "nom_municipi")]
  colnames(fixed_metadata) <- c("station_code", "latitude", "longitude", "municipality_code", "municipality_name")
  return(fixed_metadata)
}

RetrieveValueStatsPerDay <- function(date_init, date_end, variable_code, variable_prefix) {
  soql <- sprintf("SELECT date_trunc_ymd(data_lectura) as date_utc, codi_estacio as station_code, min(valor_lectura) as min_%s, avg(valor_lectura) as avg_%s, max(valor_lectura) as max_%s WHERE (data_lectura between '%s' and '%s' AND codi_variable='%s' AND codi_estat='V') GROUP BY date_utc, codi_estacio", variable_prefix, variable_prefix, variable_prefix, date_init, date_end, variable_code)
  url <- sprintf("https://analisi.transparenciacatalunya.cat/resource/nzvn-apee.json?$query=%s", soql)
  value_stats_per_day <- read.socrata(url);
  return(value_stats_per_day)
}

PrepareValueStatsPerDay <- function(value_stats_per_day_raw) {
  columns <- names(value_stats_per_day_raw)
  columns_to_gather <- columns[!columns %in% c("date_utc", "station_code")]
  value_stats_per_day <- value_stats_per_day_raw %>% gather("key", "value", columns_to_gather)
  value_stats_per_day <- value_stats_per_day %>% separate("key", c("operation", "variable"), "_")
  value_stats_per_day[["value"]] <- as.numeric(value_stats_per_day[["value"]])
  return(value_stats_per_day)
}

  
MergeXema <- function(xema_t_per_day, xema_rh_per_day, xema_ws2_per_day, metadata) {
  xema_stats_per_day <- rbind(xema_t_per_day, xema_rh_per_day, xema_ws2_per_day)
  xema_stats_per_day <- left_join(xema_stats_per_day, metadata, by=c("station_code"))
  return (xema_stats_per_day)  
}

PrepareXema <- function(xema_stats_per_day) {
  columns <- names(xema_stats_per_day)
  xema_stats_per_day$operation <- factor(xema_stats_per_day$operation)
  xema_stats_per_day$variable <- factor(xema_stats_per_day$variable) %>% recode_factor(t = "temperature", rh = "relative humidity", ws2 = "wind speed 2m")
  return(xema_stats_per_day)
}


xema_stats_per_day_plan <- drake_plan(
  variables = read.socrata("https://analisi.transparenciacatalunya.cat/resource/4fb2-n3yi.json"),
  metadata = read.socrata("https://analisi.transparenciacatalunya.cat/resource/yqwd-vj5e.json"),
  fixed_metadata = FixMetadata(metadata),
  xema_t_per_day_raw = RetrieveValueStatsPerDay(wrong_date_init, wrong_date_end, variables$codi_variable[variables$acronim == "T"], "t"),
  xema_rh_per_day_raw = RetrieveValueStatsPerDay(wrong_date_init, wrong_date_end, variables$codi_variable[variables$acronim == "HR"], "rh"),
  xema_ws2_per_day_raw = RetrieveValueStatsPerDay(wrong_date_init, wrong_date_end, variables$codi_variable[variables$acronim == "VV2"], "ws2"),
  xema_t_per_day = PrepareValueStatsPerDay(xema_t_per_day_raw),
  xema_rh_per_day = PrepareValueStatsPerDay(xema_rh_per_day_raw),
  xema_ws2_per_day = PrepareValueStatsPerDay(xema_ws2_per_day_raw),
  xema_stats_per_day_raw = MergeXema(xema_t_per_day, xema_rh_per_day, xema_ws2_per_day, fixed_metadata),
  xema_stats_per_day = PrepareXema(xema_stats_per_day_raw),
  xema_stats_per_day_save_data = save(xema_stats_per_day, file=file_out("data/xema_stats_per_day.rda"))
)
