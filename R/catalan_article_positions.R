

articles <- c("El", "Els", "En", "L'", "La", "Les", "Na", "S'", "Sa", "Ses")


GetIndexesOrInf <- function(words, from, compare_function) {
  indexes <- lst(words, from)                      %>%
    cross()                                         %>%
    map(~ compare_function(.$words, .$from))        %>%
    matrix(nrow=length(words), ncol=length(from))  %>%
    t()                                             %>%
    as.data.frame()                                 %>%
    map(~ suppressWarnings(min(which(as.logical(.)))))
  return (indexes)
}

CommaEndToStart <- function(words, function_to_start) {
  from <- paste(",", c(articles, tolower(articles)))
  to <- function_to_start(c(articles, articles))

  article_indices <- GetIndexesOrInf(words, from, endsWith)

  capitalised_start_words <- map2(
    words,
    article_indices, 
    ~ if(.y==Inf) {
      .x
      } else {
        paste0(
          to[.y], 
          if(!endsWith(to[.y], "'")) " " else "",
          substr(.x, 1, nchar(.x)-nchar(from[.y]))
        )
      }
    )
    
  return (simplify(capitalised_start_words))
}

CommaEndToCapitalisedStart <- function(words) {
  return (CommaEndToStart(words, identity))
}

CommaEndToLowercaseStart <- function(words) {
  return (CommaEndToStart(words, tolower))
}

StartToCommaEnd <- function(words, function_to_end) {
  from <- c(articles, tolower(articles))
  to <- paste(",", function_to_end(c(articles, articles)))
  
  article_indices <- GetIndexesOrInf(words, from, startsWith)
  
  capitalised_start_words <- map2(
    words,
    article_indices, 
    ~ if(.y==Inf) {
      .x
    } else {
      word <- substr(.x, ifelse(!endsWith(to[.y], "'"), nchar(from[.y])+2, nchar(from[.y])+1), nchar(.x)) 

      paste0(
        word,
        to[.y]
      )
    }
  )
  
  return (simplify(capitalised_start_words))
}

StartToCommaCapitalisedEnd <- function(words) {
  return(StartToCommaEnd(words, identity))
}

StartToCommaLowercaseEnd <- function(words) {
  return(StartToCommaEnd(words, tolower))
}
