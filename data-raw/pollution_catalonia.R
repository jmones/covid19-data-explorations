# Data automatic pollution measurement stations in Catalonia
#
# Data retrieved from the open data portal of the Catalan Government
# https://analisi.transparenciacatalunya.cat/Medi-Ambient/Dades-d-immissi-dels-punts-de-mesurament-de-la-Xar/uy6k-2s8r
#
# License https://www.meteo.cat/wpweb/avis-legal/

library(drake)
library(tidyverse)

PreparePollutionCatalonia <- function(pollution_catalonia) {
  pollution_catalonia$date <- as.POSIXct(pollution_catalonia$data)
  pollution_catalonia <- pollution_catalonia %>% select(-c("any", "codi_estaci", "codi_mesurament", "codi_mun", "data", "dia", "geocoded_column.latitude", "geocoded_column.longitude", "magnitud", "mes", "provincia", "punt_most"))
  
  names(pollution_catalonia) <- names(pollution_catalonia) %>%
    recode(
      altitud="altitude", 
      codi_eoi="station_code", 
      contaminant="pollutant", 
      rea_urb="environment",
      latitud="latitude",
      longitud="longitude",
      municipi="municipality", 
      nom_estaci="station_name", 
      tipus_est="station_type", 
      unitats="unit"
  )
  
  pollution_catalonia$environment <- factor(pollution_catalonia$environment)
  pollution_catalonia$pollutant <- factor(pollution_catalonia$pollutant)
  pollution_catalonia$station_code <- factor(pollution_catalonia$station_code)
  pollution_catalonia$station_name <- factor(pollution_catalonia$station_name)
  pollution_catalonia$station_type <- factor(pollution_catalonia$station_type)
  pollution_catalonia$unit <- factor(pollution_catalonia$unit)
  
  for(ii in 1:24) {
    column_h = sprintf("h%02d", ii)
    column_v = sprintf("v%02d", ii)
    
    pollution_catalonia[[column_h]] <- ifelse(pollution_catalonia[[column_v]]=="V", as.numeric(pollution_catalonia[[column_h]]), as.numeric(NA))
                                            
    pollution_catalonia <- pollution_catalonia %>% select(-c(column_v))
  }
  
  pollution_catalonia <- pollution_catalonia %>% tidyr::gather(key="hour", value="value", 1:24 %>% map(~ sprintf("h%02d", .)) %>% simplify())
  pollution_catalonia$hour <- pollution_catalonia$hour %>% map(~ as.numeric(gsub("^h(.*?)$", "\\1", .)))
  
  return(pollution_catalonia)
}

AssureSameValue <- function(values) {
  unique_values <- unique(values)
  unique_values_except_na <- unique_values[!is.na(unique_values)]
  if(length(unique_values_except_na)==0) {
    stop("No value is available")
  }
  if(length(unique_values_except_na)>1) {
    stop("All values expected to be equal")
  }
  return(unique_values_except_na[[1]])
}

BuildStationMetadata <- function(pollution_catalonia) {
  station_metadata <- pollution_catalonia %>%
    select(-c(date, hour, pollutant, value))  %>%
    group_by(station_code) %>%
    summarise(
      altitude=AssureSameValue(altitude),
      environment=AssureSameValue(environment),
      latitude=AssureSameValue(latitude),
      longitude=AssureSameValue(longitude),
      municipality=AssureSameValue(municipality),
      station_name=AssureSameValue(station_name),
      station_type=AssureSameValue(station_type),
  )
  
  return(station_metadata)  
}

BuildPollutantMetadata <- function(pollution_catalonia) {
  pollutant_metadata <- pollution_catalonia %>%
    select(c(pollutant, unit))  %>%
    group_by(pollutant) %>%
    summarise(unit=AssureSameValue(unit))
  
  return(pollutant_metadata)  
}

DailyAggregatePollutionCataloniaSanityCheck <- function(pollution_catalonia) {
  nrow_groupped <- pollution_catalonia      %>%
    select(station_code, date, pollutant)   %>%
    group_by(station_code)                  %>%
    n_distinct()
  
  nrow_groupped_plus_metadata <- pollution_catalonia %>%
    select(- c("hour", "value"))                    %>%
    n_distinct()
  
  if(nrow_groupped != nrow_groupped_plus_metadata) {
    stop("Some metadata columns about the stations differs on different observations")
  }
}

DailyAggregatePollutionCatalonia <- function(pollution_catalonia, pollution_catalonia_pollutant_metadata, pollution_catalonia_station_metadata) {
  DailyAggregatePollutionCataloniaSanityCheck(pollution_catalonia)
  
  pollution_catalonia <-
    pollution_catalonia                                                     %>%
    group_by(station_code, date, pollutant)                                 %>%
    summarise(
      min=min(value, na.rm=TRUE), 
      mean=mean(value, na.rm=TRUE), 
      max=max(value, na.rm=TRUE),
      unit=AssureSameValue(unit)
    )                                                                       %>%
    gather(key="operation", value="value", c("min", "mean", "max"))         %>%
    filter(!is.na(value) && value!=Inf && value!=-Inf)                      %>%
    left_join(pollution_catalonia_station_metadata, by=c("station_code"))   %>%
    left_join(pollution_catalonia_pollutant_metadata, by=c("pollutant"))
  
  return(pollution_catalonia)
}

pollution_catalonia_plan <- drake_plan(
  pollution_catalonia_2020_raw = read.socrata("https://analisi.transparenciacatalunya.cat/resource/uy6k-2s8r.json?$where=any>=2020"),
  pollution_catalonia_2020 = PreparePollutionCatalonia(pollution_catalonia_2020_raw),
  pollution_catalonia_2020_save_data = save(pollution_catalonia_2020, file=file_out("data/pollution_catalonia_2020.rda")),
  pollution_catalonia_pollutant_metadata = BuildPollutantMetadata(pollution_catalonia_2020),
  pollution_catalonia_pollutant_metadata_save_data = save(pollution_catalonia_pollutant_metadata, file=file_out("data/pollution_catalonia_pollutant_metadata.rda")),
  pollution_catalonia_station_metadata = BuildStationMetadata(pollution_catalonia_2020),
  pollution_catalonia_station_metadata_save_data = save(pollution_catalonia_station_metadata, file=file_out("data/pollution_catalonia_station_metadata.rda")),
  pollution_catalonia_daily_2020 = DailyAggregatePollutionCatalonia(pollution_catalonia_2020, pollution_catalonia_pollutant_metadata, pollution_catalonia_station_metadata),
  pollution_catalonia_daily_2020_save_data = save(pollution_catalonia_daily_2020, file=file_out("data/pollution_catalonia_daily_2020.rda"))
)

