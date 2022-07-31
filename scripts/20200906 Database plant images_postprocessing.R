compteur <- 1

# The path.s containing "My Pictures"
root.path <- c("C:/Users/munozfra", "E:") 

for(i in 1:length(jpg))
{
  # Extract location information from directory names
  x <-  strsplit(jpg[i], split="/")[[1]]
  loc <- c()
  # Three maximum locality items
  for(j in 1:3) if(j<=(length(x)-1)) loc <- c(loc,x[j]) else loc <- c(loc, NA)
  jpg.name <- x[length(x)]
  
  sp.buf <- c()
  # Extract image file name
  sp.chk <- strsplit(jpg.name, split="_")[[1]]
  
  if(sp.chk[1]%in%sp[,2])
  {
    sp[compteur,1] <- jpg[i]
    compteur <- compteur+1
  }
}

temp.dirlist <- read.csv("temp.dirlist.csv")
# Check the file paths in directory
for(i in nrow(temp.dirlist))
# Update the file paths
for(i in 1:nrow(sp)) {
  # search the lower directory path
  dir.src <- ifelse(is.na(sp$Lower.directory[i]), sp$Upper.directory[i], sp$Lower.directory[i])
  if(!dir.src%in%temp.dirlist[,2])
    for(k in 1:length(root.path)) {
      res <- list.files(path=paste(root.path[k],"My pictures",sep="/"),
                        pattern = dir.src, recursive = T, include.dirs = T)
      if(length(res)>1) {
        warnings(paste("Duplicate directories", res, sep="-"))
      } else if(length(res)>0) temp.dirlist <- rbind(temp.dirlist, c(paste(root.path[k], "My Pictures", res, sep="/"),
                                                   sp$Lower.directory[i]))
    }
  #print(i)
}
write.csv(x=temp.dirlist, file="data/temp.dirlist.csv")

# Create MD5 code
sp$MD5 <- NA
for(i in 1:nrow(sp)) {
  dir.src <- ifelse(is.na(sp$Lower.directory[i]), sp$Upper.directory[i], sp$Lower.directory[i])
  dirlist <- temp.dirlist[temp.dirlist[,2]==dir.src,1]
  dirlist <- dirlist[!is.na(dirlist)]
  if(!is.null(dirlist)) for(k in 1:length(dirlist)) {
    f <- paste(dirlist[k],"My pictures",sp[k,3],sep="/")
    if(file.exists(f)) {
      sp$MD5[k] <- digest(algo="md5", file=f)
      print(f)
    }
  }
}
sum(!is.na(sp$MD5))/nrow(sp)

#colnames(sp)
nrow(sp)
