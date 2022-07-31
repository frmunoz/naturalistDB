# Table including all detected species names
# Get previously analyzed file names
sp <- read.table("Database images_completeinfo.csv", sep=";", dec=",", header=T, stringsAsFactors = F, row.names=NULL)
#sp <- xlsx::read.xlsx(file="Database images_completeinfo.xls", sheetIndex=1, row.names=NULL, encoding = "UTF_8")
#for(i in 1:ncol(sp)) if(is.factor(sp[,i])) sp[,i] <- as.character(sp[,i])
#sp <- sp[,-1] #href is the first column

# Vector including not found plant names
not.found <- read.table("Database images_notfound.csv", sep=";", dec=",", header=F, stringsAsFactors = F)

# Load workspace
load("data/20200815 Database plant images.RData")

# Previous code for building list of jpg files
# jpg <- c()
# jpg.sub <- c()
# for(i in 1:length(dir))
# {
#   setwd(dir[i])
#   jpg <- c(jpg, paste(dir[i],list.files(pattern=".jpg", recursive=T, ignore.case=T),sep="/"))
#   jpg.sub <- c(jpg.sub, list.files(pattern=".jpg", recursive=T, ignore.case=T))
# }
# length(jpg)  

# Create the global list of jpg files with corresponding MD5 codes
# jpg.old <- jpg
jpg <- list.files(paste(root.path, "My pictures", sep="/"), pattern = ".jpg", 
                  recursive = T, full.names = T, ignore.case = T)
#gif <- list.files(paste(root.path, "My pictures", sep="/"), pattern = ".gif", 
#                  recursive = T, full.names = T, ignore.case = T)
jpg <- data.frame(file=jpg, MD5=NA, sp.id=NA, stringsAsFactors = F)
for(i in 1:nrow(jpg)) if(file.exists(jpg[i,1])) jpg[i,2] <- digest::digest(algo="md5", file=jpg[i,1])
sum(!is.na(jpg[,2])) # 100652
sum(is.na(jpg[,2])) # 34
sum(sp$MD5%in%jpg[,3]) # 2550
sum(!sp$MD5%in%jpg[,3]) # 724
View(sp[!sp$MD5%in%jpg[,3],])

# Correspondance with entries in the table of taxon information  
for(i in 1:nrow(jpg)) if(jpg[i,2]%in%sp$MD5) jpg[i,3] <- sp[which(sp$MD5==jpg[i,2]),"id"][1]
sum(!is.na(jpg[,3])) # 725
sum(is.na(jpg[,3])) # 99961

sp.chk.list <- c()
typo <- c()
multi <- c()
href <- c()
names.tpl <- c("Upper.directory", "Lower.directory", "File.name", "Taxon", "Genus", "Hybrid.marker", "Species", "Abbrev",
               "Infraspecific.rank", "Infraspecific", "Authority", "ID", "Plant.Name.Index", "TPL.version", "Taxonomic.status", 
               "Family", "New.Genus", "New.Hybrid.marker", "New.Species", "New.Infraspecific.rank", 
               "New.Infraspecific", "New.Authority", "New.ID", "New.Taxonomic.status", "Typo", "WFormat",
               "Higher.level", "Date") 
names.taxref <- c("REGNE", "PHYLUM", "CLASSE", "ORDRE", "FAMILLE", "GROUP1_INPN", "GROUP2_INPN", 
                  "GROUPE_GRAND_PUBLIC", "CD_NOM", "CD_TAXSUP", "CD_SUP", "CD_REF", "RANG",
                  "LB_NOM", "LB_AUTEUR", "NOM_COMPLET", "NOM_COMPLET_HTML", "NOM_VALIDE",
                  "NOM_VERN", "NOM_VERN_ENG", "HABITAT", "FR", "GF", "MAR", "GUA", "SM", "SB",
                  "SPM", "MAY", "EPA", "REU", "SA", "TA", "TAAF", "PF", "NC", "WF", "CLI", "URL")
for(i in 1:nrow(jpg))
{
  # Extract location information from directory names
  x <-  strsplit(jpg.sub[i], split="/")[[1]]
  loc <- c()
  # Two maximum locality items
  for(j in 1:2) if(j<=(length(x)-1)) loc <- c(loc,x[j]) else loc <- c(loc,NA)
  jpg.name <- x[length(x)]
  
  sp.buf <- c()
  # Extract image file name
  sp.chk <- strsplit(jpg.name, split="_")[[1]]
  sp.chk.list <- c(sp.chk.list, sp.chk[1])
  
  # For hypertext linking to file in Excel
  href.buf <- paste("=LIEN_HYPERTEXTE(\"",jpg[i],"\";\"",jpg.name,"\")",sep="")
    
  if(is.null(sp) | (!jpg.name%in%sp[,3] & !jpg[i]%in%not.found))
  {
    if(length(sp.chk)==1 | length(grep(sp.chk[1], pattern=" "))==0) 
    {
      if(!sp.chk[1]%in%not.found) not.found <- c(not.found, jpg[i])
    } else 
    {
      # Check in the Plant List
      #tnrs.res <- try(tnrs(sp.chk[1], source = "NCBI,MSW3"))#"iPlant_TNRS"))
      tpl.res <- try(TPLck(sp.chk[1]))
      tpl.res <- tpl.res[tpl.res$Plant.Name.Index,]
      if(min(dim(tpl.res))>0)
      {
        if(sum(!tpl.res$Typo)==0) 
        {
          warning(paste("No perfect match for ", sp.chk[1], sep=""))
          typo <- c(typo, paste(sp.chk[1],"TPL", sep="_"))
        } else 
        {
          if(sum(!tpl.res$Typo)>1) 
          {
            warning(paste("Several perfect matches in TPL for ", sp.chk[1], sep=""))
            multi <- c(multi, paste(sp.chk[1],"TPL", sep="_"))
          }
          else 
            {
              sp.buf <- c(loc, jpg.name, unlist(tpl.res[!tpl.res$Typo,]))
              names(sp.buf) <- names.tpl
            }
        }
      }
      # Check in TaxRef
      if(sp.chk[1]%in%taxref$LB_NOM) 
      {
        if(is.null(sp.buf))
        {
            sp.buf <- c(loc, jpg.name, rep(NA, 25))
            names(sp.buf) <- names.tpl
        }
        if(sum(taxref$LB_NOM==sp.chk[1])>1)
        {
          warning(paste("Several perfect matches in TaxRef for ", sp.chk[1], sep=""))
          multi <- c(multi, paste(sp.chk[1],"TaxRef", sep="_"))
          sp.buf <- c(sp.buf, rep(NA, 39))
          names(sp.buf)[-(1:28)] <- names.taxref
        } else 
          sp.buf <- c(sp.buf, taxref[taxref$LB_NOM==sp.chk[1],])
      } else if(!is.null(sp.buf)) 
        {
          sp.buf <- c(loc, jpg.name, rep(NA, 64))
        }
      if(!is.null(sp.buf))
      {
        if(sum(is.na(sp.buf))!=length(sp.buf))
        {
          print(paste(i, jpg[i]))
          if(!is.null(sp))
          {
            #sp <- rbind(sp, as.character(sp.buf[colnames(sp)]))
            sp <- rbind(sp, sp.buf)
            href <- c(href, href.buf)
          }
          else 
            {
              sp <- as.character(sp.buf)
              names(sp) <- names(sp.buf)
              sp <- data.frame(t(sp), stringsAsFactors = F)
              href <- c(href, href.buf)
            }
        }
      } else not.found <- c(not.found, jpg[i])
    }
  }
}
sp.old <- sp
sp <- cbind(href, sp)

length(not.found) # 10866
length(multi) # 194
length(typo) # 53
dim(jpg) #43962

#colnames(sp)
nrow(sp) # 3263
length(unique(c(sp$Taxon, sp$LB_NOM))) # 581

# Save database
#write.table(sp, file="Database images_completeinfo.csv", sep=";", dec=",", row.names=F, fileEncoding="UTF-8")
xlsx::write.xlsx(sp, file="Database images_completeinfo.xls", row.names=F, showNA = F)
write.table(not.found, file="Database images_notfound.csv", sep=";", dec=",", row.names=F, fileEncoding="UTF-8")

# Simplified dataset
sp.simpl <- sp[,c("href","Upper.directory", "Lower.directory", "Family", "Taxon", "ID", "LB_NOM", "NOM_VALIDE", "URL")]
xlsx::write.xlsx(sp.simpl, file="Database images_simpl.xls", row.names=F, showNA = F)
#write.table(sp.simpl, file="Database images_simpl.csv", sep=";", dec=",", row.names=F, fileEncoding="UTF-8")
