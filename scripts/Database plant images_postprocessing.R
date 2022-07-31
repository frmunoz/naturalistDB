compteur <- 1

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

#colnames(sp)
nrow(sp)
