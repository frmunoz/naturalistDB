plantnet.id <- function(files, dir=F, rename=T, threshold=90, key="2a10aXzK3RhetsIvZ6d0Sf55Hu")
{
  # Important: "files" should include complete paths to the image files
  # Otherwise, set dir to T will apply the analysis to all the files of a directory
  
  if(dir & length(files)>1) stop("Please provide a single directory name")
  if(dir) { 
    dir.name <- files
    files=list.files(dir.name, )
  }
  
  ## Command to push to Github
  cmd_list <- list(
    cmd1 = tolower(substr(getwd(),1,2)),
    cmd2 = paste("cd",getwd()),
    cmd3 = "git push"
  )
  cmd <- paste(unlist(cmd_list), collapse = " & ")
  
  ## Open an existing repository
  repo <- git2r::repository("C:/Users/munozfra/Documents/Rprojects/naturalistDB")
  git2r::config(repo, user.name = "frmunoz", user.email = "fmunoz@univ-grenoble-alpes.fr")
  
  res <- list()
  for(x in files) {
    ## Remove previous file, commit and push
    git2r::rm_file(repo, "images/test.jpg")
    git2r::commit(repo, "Delete previous image")
    shell(cmd)
    
    if(!file.copy(from=x, to="images/test.jpg", overwrite=T))
      stop("Copy of image file has failed")
    
    ## Add file, commit and push
    git2r::add(repo, "images/test.jpg")
    git2r::commit(repo, "New image for identification")
    shell(cmd)
    res[[x]] <- plantnet::identify(key, 
             "https://raw.githubusercontent.com/frmunoz/naturalistDB/master/images/test.jpg",
             organ="flower")
  }
  
  # Check excess number of requests
  print(paste(sum(unlist(lapply(res, function(x) ifelse(is.null(ncol(x)), ifelse(x[1]=="Too Many Requests",1,0), 0)))),
              " excess requests to API", sep=""))
  
  # Check number of correct identification
  print(paste(sum(unlist(lapply(res, function(x) ifelse(!is.null(ncol(x)), ifelse(any(x[,1]>=threshold),1,0), 0)))),
              " identified plants with probability above threshold", sep=""))
  correct.id <- unlist(lapply(res, function(x) ifelse(!is.null(ncol(x)), ifelse(any(x[,1]>=threshold),T,F), F)))
  #files[correct.id]
  
  if(dir) save(res, file=paste(dir.name,"/",Sys.Date(),"_plantnet.id.RData",sep=""))
  
  if(rename) {
    # Rename file with PlantNet identification if score is high
    #for(x in files) {
      #if(ncol(res[[x]])==3 & sum(res[[x]][,1]>0.9)==1)
      # file.rename(from=res[[x]], to=paste()))
    #}
  } 
  
  return(res)
}