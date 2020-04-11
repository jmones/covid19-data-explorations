# Statistics from Catalan municipalities
#
# Retrieved from https://www.idescat.cat/dev/api/emex/
# License https://www.idescat.cat/dev/api/#cdu

library(httr)
library(jsonlite)
library(progress)
library(tibble)


CallEmexApi <- function(endpoint, params=NULL) {
  path <- paste("emex/v1/", endpoint, sep="")
  url <- modify_url("https://api.idescat.cat", path=path)
  http_response <- RETRY("GET", url)
  
  parsed <- fromJSON(content(http_response, "text"), simplifyVector = FALSE)
  if (status_code(http_response) != 200) {
    stop(sprintf("Emex API request failed [%s]: %s", status_code(http_response), url), call. = FALSE)
  }
  
  response <- structure(
    list(
      content = parsed,
      path = path,
      endpoint = endpoint,
      http_response = http_response
    ),
    class = "emex_api"
  )
  
  return(response)
}

ProcessElement <- function(element) {
  df <- tibble(name = element$c, value = element$v)
  return(df)
}

ProcessF <- function(f_tree) {
  if(is.null(f_tree$v))  {
    df <- f_tree %>% map_dfr(~ ProcessElement(.))
  } else {
    df <- ProcessElement(f_tree)
  }
  return(df)
}

ProcessSubgroup <- function(subgroup) {
  df <- ProcessF(subgroup$ff$f)
  df$subgroup <- subgroup$c
  return(df)
}

ProcessT <- function(t_tree) {
  if(is.null(t_tree$ff)) {
    df <- t_tree %>% map_dfr(~ ProcessSubgroup(.))
  } else {
    df <- ProcessSubgroup(t_tree)
  }
  return(df)
}

ProcessGroup <- function(group) {
  df <- ProcessT(group$tt$t)
  df$group <- group$c
  return(df)
}

ProcessG <- function(g_tree) {
  if(is.null(g_tree$tt)) {
    df <- g_tree %>% map_dfr(~ ProcessGroup(.))
  } else {
    df <- ProcessGroup(g_tree)
  }
  return(df)
}


ConvertMunicipalityToTibble <- function(response) {
  df <- ProcessG(response$content$fitxes$gg$g)
  
  mun_tree <- response$content$fitxes$cols$col %>% detect(~ .$scheme=="mun")
  df$municipality <- mun_tree$content
  
  com_tree <- response$content$fitxes$cols$col %>% detect(~ .$scheme=="com")
  df$comarca <- com_tree$content
  return(df)
}

RetrieveMunicipality <- function(id, pb=NULL) {
  response <- CallEmexApi(paste("geo/", id, ".json", sep=""))
  df <- ConvertMunicipalityToTibble(response)
  if(!is.null(pb)) {
    pb$message(paste("Downloaded", df$municipality[1]))
    pb$tick()
  }
  return(df)
}

RetrieveAllMunicipalities <- function(ids, pb=NULL) {
  catalan_municipalities <- ids %>% map_dfr(~ RetrieveMunicipality(., pb))
  if(!is.null(pb)) {
    pb$terminate()
  }
  return(catalan_municipalities)
}

response <- CallEmexApi("dades.json")
ids <- response$content$fitxes$cols$col %>% map(~ .$id)
pb <- progress_bar$new(total = length(ids))

catalan_municipalities <- RetrieveAllMunicipalities(ids, pb)

usethis::use_data(catalan_municipalities, overwrite=TRUE)

