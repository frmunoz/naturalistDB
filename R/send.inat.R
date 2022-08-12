send.inat <- function(obs, id=NULL, conda=NULL, python=NULL) {
  # obs should be a data.frame with some mandatory fields:
  # - Species: binomial species name
  # - Date: date of observation
  # - Lat and Long: geographical coordinates of observation
  # - Precision: precision of observation coordinates
  # - Path: path to image associated to observation
  
  # id should be a data.frame including the following information
  # - user: user account name in iNaturalist
  # - pwd: password associated to user account name in iNaturalist
  # 
  
  require(reticulate)
  
  if(is.null(id)) 
    stop("Missing identification data")
  
  if(!is.data.frame(id))
    stop("id must be a data.frame")
  
  if(any(!c("user","pwd","app.id","app.pwd")%in%names(id)))
     stop("Incorrect identification data")
  
  if(!is.data.frame(obs))
    stop("obs must be a data.frame")
  
  if(any(!c("Species", "Date", "Lat", "Long", "Path", "Precision")%in%names(obs)))
    stop("Incorrect fields in obs")
  
  # See POST  options in https://www.inaturalist.org/pages/api+reference
  # https://pythonawesome.com/an-inaturalist-api-client-for-python/
  
  # Some previous (unsucessful) tries
  # virtualenv_install(envname = "send.inat",
  #                               packages="pyinaturalist")
  # use_virtualenv(virtualenv = "send.inat")
  
  if(!is.null(conda)) {
    use_condaenv(condaenv=conda)
  } 
  
  if(is.null(conda) & !is.null(python)) {
    use_python(python=python)
  }
  
  if(is.null(conda) & !is.null(python)) {
    use_virtualenv("__main__")
    virtualenv_install(envname="__main__", packages ='pyinaturalist')
  }
  
  py_run_string(paste("from pyinaturalist import *\ntoken = get_access_token(username='",
                                  id$user,
                                  "',\npassword='",
                                  id$pwd,
                                  "',\napp_id='",
                                  id$app.id,
                                  "',\napp_secret='",
                                  id$app.pwd,
              "')\ntaxon = get_taxa(q='",
                                  obs$Species,
                                  "', rank=['species'])\nfrom datetime import datetime\nresponse = create_observation(taxon_id=taxon['results'][0]['id'],observed_on_string='",
                                  obs$Date,
                                  "',\nlatitude=",
                                  obs$Lat,
                                  ",\nlongitude=",
                                  obs$Long,
                                  ",\npositional_accuracy=",
                                  obs$Precision,
                                  ",\naccess_token=token)\nnew_observation_id = response['id']\nupload_photos(new_observation_id,'",
                                  obs$Path,
              "', access_token=token)\n", sep=""),
        convert = T)
}