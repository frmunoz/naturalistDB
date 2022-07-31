clean.duplicates <- function(path, type = c(".jpg",".JPG"), mode = "longer", where ="Duplicates")
{
  if(!dir.exists(paste(path, where, sep="/"))) 
    dir.create(paste(path, where, sep="/"))
  
  f <- c()
  for(i in type)
    f <- c(f, list.files(path=path, pattern=i, recursive = T))
  f.names <- sapply(f,function(x) strsplit(x,split="/")[[1]][length(strsplit(x,split="/")[[1]])])
  
  f.md5 <- c()
  # Get MD5 code
  print("Build MD5 code")
  for(i in 1:length(f))  {
    f.md5 <- c(f.md5, tryCatch(digest::digest(algo="md5", file=paste(path, f[i], sep="/")), error = function(e) NA))
    basicPlotteR::progress(i, length(f))
  }
  
  f.dup <- duplicated(f.md5)
  f.dup.sel <- which(f.dup)
  print(paste("Found ", length(f.dup.sel), " duplicates", sep=""))
  print("Move duplicate files")
  if(mode=="longer") {
    for(j in f.dup.sel) {
      f.sel <- which(f.md5==f.md5[j])
      f.nchar <- which.max(sapply(f.names[f.sel], nchar))
      for(k in f[f.sel[-f.nchar]]) {
        if(file.copy(from=paste(path,k,sep="/"),
                     to=paste(path,where,sep="/"),
                     overwrite = F)) {
          print(paste("Success move of ",k,sep=""))
          file.remove(paste(path,k,sep="/"))
        }
      }
      basicPlotteR::progress(which(f.dup.sel==j), length(f.dup.sel))
    }
  } else stop("Only mode longer currently available")
}