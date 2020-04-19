


GetUnmapped <- function(new_aliases, aliases) {
  missing_aliases <- setdiff(new_aliases, aliases)
  return(missing_aliases)
}

GetCandidateNames <- function(new_aliases, aliases, names) {
  new_names <- new_aliases %>% map(~ simplify(names[aliases == .]))
  candidate_names <- unique(setdiff(names, new_names))
  return(candidate_names)
}

GetCandidateAliases <- function(aliases, names, candidate_names) {
  candidate_aliases <- candidate_names %>% map(~ simplify(aliases[names == .]))
  return(candidate_aliases)
}