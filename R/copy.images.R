copy.images <- function(from, to, type = c(".jpg",".JPG"))
{
  f.from <- c()
  for(i in type)
    f.from <- c(f.from, list.files(path=from, pattern=i, recursive = T))
  
  f.to <- c()
  for(i in type)
    f.to <- c(f.to, list.files(path=to, pattern=i, recursive = T))
  
  f.from <- f.from[!f.from%in%intersect(f.from,f.to)]

  for(f in f.from) {
    if(!dir.exists(dirname(paste(to,f,sep="/"))))
       dir.create(dirname(paste(to,f,sep="/")))
    file.copy(from=paste(from,f,sep="/"),
              to=paste(to,f,sep="/"),
              overwrite = F)
  }
}
