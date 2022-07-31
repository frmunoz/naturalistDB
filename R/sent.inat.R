sent.inat <- function(data) {
  # See POST  options in https://www.inaturalist.org/pages/api+reference
  tab <- data
  
  write.table(tab, file="observations.json")
  
  shell("ruby inaturalist-oauth-pkce-flow.rb")
}