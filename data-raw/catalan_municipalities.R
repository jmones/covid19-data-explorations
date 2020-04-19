# Statistics from Catalan municipalities
#
# Retrieved from https://www.idescat.cat/dev/api/emex/
# License https://www.idescat.cat/dev/api/#cdu

library(drake)
library(httr)
library(jsonlite)
library(tibble)
library(tidyverse)
library(relatable)

source("R/catalan_article_positions.R")


CallEmexApi <- function(endpoint, params=NULL) {
  path <- paste0("emex/v1/", endpoint, ".json?lang=en")
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

RetrieveEmexIds <- function() {
  response <- CallEmexApi("dades")
  return(response)
}

RetrieveEmexMunicipality <- function(id) {
  response <- CallEmexApi(paste0("geo/", id))
  return(response)
}

GetValueOrNa <- function(value) {
  if(is.null(value)) {
    return(NA)
  } else {
    return(value)
  }
}

ParseElement <- function(element) {
  if(is.null(element[["c"]])) {
    stop("c not present in element")  
  }
  
  if(is.null(element[["v"]])) {
    stop("v not present in element")  
  }

    df <- tibble(
    name = element[["c"]], 
    value = as.numeric(gsub("^(.*?),.*", "\\1", element[["v"]])), 
    unit = as.character(GetValueOrNa(element[["u"]])), 
    updated = as.POSIXct(GetValueOrNa(element[["updated"]]))
  )
  
  return(df)
}

ParseFTree <- function(f_tree) {
  if(is.null(f_tree[["v"]]))  {
    df <- f_tree %>% map_dfr(~ ParseElement(.), .default = NA)
  } else {
    df <- ParseElement(f_tree)
  }
  return(df)
}

ParseSubgroup <- function(subgroup) {
  df <- ParseFTree(subgroup[["ff"]][["f"]])
  df[["subgroup"]] <- subgroup[["c"]]
  if(!is.null(subgroup[["updated"]])) {
    df[["updated"]][is.na(df[["updated"]])] <- subgroup[["updated"]]
  }
  if(!is.null(subgroup[["u"]])) {
    df[["unit"]][is.na(df[["unit"]])] <- subgroup[["u"]]
  }
  return(df)
}

ParseTTree <- function(t_tree) {
  if(is.null(t_tree[["ff"]])) {
    df <- t_tree %>% map_dfr(~ ParseSubgroup(.), .default = NA)
  } else {
    df <- ParseSubgroup(t_tree)
  }
  return(df)
}

ParseGroup <- function(group) {
  df <- ParseTTree(group[["tt"]][["t"]])
  df[["group"]] <- group[["c"]]
  return(df)
}

ParseGTree <- function(g_tree) {
  if(is.null(g_tree[["tt"]])) {
    df <- g_tree %>% map_dfr(~ ParseGroup(.), .default = NA)
  } else {
    df <- ParseGroup(g_tree)
  }
  return(df)
}

ParseMunicipalityResponse <- function(response) {
  df <- ParseGTree(response[["content"]][["fitxes"]][["gg"]][["g"]])
  
  mun_tree <- response[["content"]][["fitxes"]][["cols"]][["col"]] %>% detect(~ .[["scheme"]]=="mun")
  df[["municipality"]] <- mun_tree[["content"]]
  
  com_tree <- response[["content"]][["fitxes"]][["cols"]][["col"]] %>% detect(~ .[["scheme"]]=="com")
  df[["comarca"]] <- com_tree[["content"]]
  return(df)
}

RetrieveAllMunicipalities <- function(ids) {
  catalan_municipalities <- ids %>% map_dfr(~ RetrieveMunicipality(.))
  return(catalan_municipalities)
}

BuildMunicipalityNamesDataset <- function(catalan_municipalities_raw) {
  
  # Canonical name will be capitalised start
  alias_comma_end <- unique(simplify(catalan_municipalities_raw %>% select(municipality)))
  names <- CommaEndToCapitalisedStart(alias_comma_end)
  alias <- alias_comma_end
  relation(alias, names, relation_type="bijection")
  
  # All alias should have exactly 1 name, and viceversa
  alias_capitalised_start <- names
  relation(alias_capitalised_start, names, relation_type="bijection")
  alias <- c(alias, alias_capitalised_start)
  
  # All alias should have exactly 1 name, and viceversa
  alias_lowercase_start <- CommaEndToLowercaseStart(alias_comma_end)
  relation(alias_lowercase_start, names, relation_type="bijection")
  alias <- c(alias, alias_lowercase_start)
  
  names <- c(names, names, names)
  municipality_names <- unique(tibble(alias=alias, names=names))

  # all elements in names should have an alias
  relation(municipality_names$alias, municipality_names$names, relation_type="surjection")
  
  return(municipality_names)
}

PrepareCatalanMunicipalities <- function(catalan_municipalities_raw, catalan_municipalities_names) {
  catalan_municipalities_raw$municipality <- catalan_municipalities_raw$municipality %>%
    map(~ simplify(catalan_municipalities_names$names[catalan_municipalities_names$alias == .]))
    
  return(catalan_municipalities_raw)
} 

catalan_municipalities_plan <- drake_plan(
  ids_response_from_server = RetrieveEmexIds(),
  ids = ids_response_from_server[["content"]][["fitxes"]][["cols"]][["col"]] %>% keep(~.[["scheme"]] == "mun") %>% map(~ .[["id"]]),
  catalan_municipality_responses = target(RetrieveEmexMunicipality(ids), dynamic=map(ids)),
  catalan_municipalities_raw = target(ParseMunicipalityResponse(catalan_municipality_responses), dynamic=map(catalan_municipality_responses)),
  catalan_municipality_names = BuildMunicipalityNamesDataset(catalan_municipalities_raw),
  catalan_municipalities_names_save_data = save(catalan_municipality_names, file=file_out("data/catalan_municipality_names.rda")),
  catalan_municipalities = PrepareCatalanMunicipalities(catalan_municipalities_raw, catalan_municipality_names),
  catalan_municipalities_save_data = save(catalan_municipalities, file=file_out("data/catalan_municipalities.rda"))
)

make(catalan_municipalities_plan)


