.First <- function() cat("\n   Welcome to naturalistDB!\n\n")

# Should we need to first install packages
# utils::install.packages("cli",repos = "https://cloud.r-project.org")

# Banbury, Barbara L., et Brian C. O’Meara. Reol: R interface to the Encyclopedia of Life. 
# Ecology and Evolution 4 (2014): 2577‑83. https://doi.org/10.1002/ece3.1109.
#install.packages("remotes")
#remotes::install_github("ropensci/reol")

options(defaultPackages=c(getOption("defaultPackages"),
                          'Taxonstand','Reol')) #ajouter des packages par default

rm_all <- function(r =c()) {
  tmp <- ls(.GlobalEnv)
  r <- c(r,'rm_all')
  tmp <- tmp[-match(r,tmp,nomatch = 0)]
  do.call(rm,as.list(tmp),envir = .GlobalEnv)
}

rm_all()

# Personal key for EOL
# https://opendata.eol.org/user/fmunoz?__no_cache__=True
MyKey <- "f4d1917d-3dbb-4803-8f94-533179c57783"

# The path.s containing "My Pictures"
root.path <- c("C:/Users/munozfra", "D:", "E:") 

# Initial check of directory list
load("data/temp.dirlist.RData")
temp.dirlist$exist <- F
for(i in 1:nrow(temp.dirlist)) if(dir.exists(temp.dirlist[i,1])) temp.dirlist$exist <- T

# Get TaxRef database
# Initial file
#taxref <- read.table("C:/Botanique/Documents/Systématique/TaxRef10/TAXREFv10.0_simpl_encoding.txt", sep="\t", header=T, fill=T, stringsAsFactors = F)
load("taxref.RData")

