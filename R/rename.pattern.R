rename.pattern <- function(path, type = c(".jpg",".JPG"), pattern.to.remove = NULL, remove.spaces = T)
{
  if(!is.null(pattern.to.remove))  {
    pattern.to.remove <- paste("\\", pattern.to.remove, sep="")
    
    f <- c()
    for(i in type)
      f <- c(f, list.files(path=path, pattern=i, recursive = T))
    
    f.names <- sapply(f,function(x) strsplit(x,split="/")[[1]][length(strsplit(x,split="/")[[1]])])
    f.suffix <- sapply(f,function(x) strsplit(x,split="\\.")[[1]][length(strsplit(x,split="\\.")[[1]])])
    
    f.torename <- unlist(sapply(f, function(x) ifelse(grep(x, pattern=pattern.to.remove),T,F)))
    
    for(g in f[f.torename]) {
      file.rename(from=paste(path,g,sep="/"),
                  to=paste(path, 
                           paste0(strsplit(g, split=pattern.to.remove)[[1]], collapse=""),
                           sep="/"))
    }
    
    if(remove.spaces) {
      
      f <- c()
      for(i in type)
        f <- c(f, list.files(path=path, pattern=i, recursive = T))
      
      f.dir <- sapply(f,function(x) paste0(strsplit(x,split="/")[[1]][-length(strsplit(x,split="/")[[1]])], collapse="/"))
      f.names <- sapply(f,function(x) strsplit(x,split="/")[[1]][length(strsplit(x,split="/")[[1]])])
      f.suffix <- sapply(f,function(x) strsplit(x,split="\\.")[[1]][length(strsplit(x,split="\\.")[[1]])])
      
      f.torename <- unlist(sapply(f.names, function(x) filesstrings::before_last(x, pattern="\\ ")))
      f.sel <- which(!is.na(f.torename))
      
      for(i in f.sel) {
          #if(!file.exists(f[i])) buf <- "NA.jpg" else buf=f[i]
          buf <- f[i]
          # Safer than file.renames to avoid overwriting
          utils::renameFile(pathname=paste(path,buf,sep="/"),
                            newPathname=paste(path,f.dir[i],paste(f.torename[i],".",f.suffix[i], sep=""), sep="/"))
      }
    }
  }
}