backup <- function(from, to, type = c(".jpg",".JPG"))
{
  
  f.from <- c()
  f.to <- c()
  for(i in type) {
    #f.from <- c(f.from, list.files(path=from, pattern=i, recursive = T))
    f.to <- c(f.to, list.files(path=to, pattern=i, recursive = T))
  }
  f.from.names <- sapply(f.from,function(x) strsplit(x,split="/")[[1]][length(strsplit(x,split="/")[[1]])])
  f.to.names <- sapply(f.to,function(x) strsplit(x,split="/")[[1]][length(strsplit(x,split="/")[[1]])])
  
  # Get MD5 code
  print("Build MD5 code")
  f.from.md5 <- c()
  for(i in 1:length(f.from))  {
    f.from.md5 <- c(f.from.md5, tryCatch(digest::digest(algo="md5", file=paste(from, f.from[i], sep="/")), error = function(e) NA))
    basicPlotteR::progress(i, length(f.from))
  }
  f.to.md5 <- c()
  for(i in 1:length(f.to))  {
    f.to.md5 <- c(f.to.md5, tryCatch(digest::digest(algo="md5", file=paste(to, f.to[i], sep="/")), error = function(e) NA))
    basicPlotteR::progress(i, length(f.to))
  }
  
  f.backup <- which(!f.from.md5%in%f.to.md5)
  print(paste(length(f.backup), "files to backup", sep= " "))
  for( f in f.from[f.backup]) {
    if(!dir.exists(dirname(paste(to,f,sep="/"))))
      dir.create(dirname(paste(to,f,sep="/")), recursive = T)
    file.copy(from=paste(from,f,sep="/"),
              to=paste(to,f,sep="/"),
              overwrite = F)
    basicPlotteR::progress(which(f.from[f.backup]==f), length(f.from[f.backup]))
  }
}