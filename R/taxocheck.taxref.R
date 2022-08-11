taxocheck.taxref <- function(names, max.distance = 2, 
                             taxref.bdd = "taxref.RData", taxref.fld = c("FAMILLE", "CD_NOM", "CD_REF", "NOM_VALIDE", "NOM_VERN"),
                             resolve.infra = F,
                             baseflor.chk = T, baseflor.bdd = "baseflor.RData", bdtfx.bdd = "bdtfx.Rdata",
                             phylo = F)
{
  # names = vector of taxa names (genus species, with space separation)
  if(!is.vector(names))
    if(!"NAMES"%in%toupper(colnames(names)) & !"BINOME"%in%toupper(colnames(names))) 
    {
      stop("input should be a vector of names")
    } else if("NAMES"%in%toupper(colnames(names))) 
      {
        names <- names[,which(toupper(colnames(names))=="NAMES")[1]]
      } else if("BINOME"%in%toupper(colnames(names))) names <- names[,which(toupper(colnames(names))=="BINOME")[1]]
      
  # Remove NA values and void names
  names <- names[!is.na(names) & names!=""]
      
  # Taxref must be used as the database
  keep <- F
  if (!("taxref" %in% ls(envir = .GlobalEnv))) {
    load(taxref.bdd) 
  } else keep = T
  taxref <- taxref[taxref$REGNE=="Plantae",]
  
  names <- na.omit(names)
  names <- tolower(unique(stringr::str_trim(names)))
  names <- unique(Hmisc::capitalize(gsub("(^\\s+|\\s+$|(?<=\\s)\\s)", "", names, perl=T)))
  orig.names <- names;
  tab<-data.frame(row.names=orig.names,Genus= rep(NA,length(orig.names)), Species= rep(NA,length(orig.names)))
      
  # Detect incomplete names or names with number:
  num<-c()
  for(i in 0:9) {num<-rbind(num, stringr::str_detect(names, as.character(i)))}
  num<-apply(num, 2, function(x) any(x))
  num<-cbind(num, unlist(sapply(rownames(tab), function(x) length(unlist(strsplit(x, split=" ")))==1)),
                 unlist(lapply(rownames(tab), function(x) strsplit(x, split=" ")[[1]][2]%in%c("sp.", "sp"," species"))))
  num<-apply(num, 1, function(x) any(x))
  tab$FoundName<-ifelse(num==T, "IncompleteName", NA)
  sel<-tab$FoundName!= "IncompleteName" | is.na(tab$FoundName)
  tab$Genus <- NA
  tab$Species <- NA
  if(sum(sel)!=0)
  {
    tab[sel,]$Genus <- Hmisc::capitalize(do.call(rbind, strsplit(as.vector(names[sel]), " "))[,1])
    tab[sel,]$Species <-  unlist(sapply(names[sel], function(x) ifelse(length(unlist(strsplit(x, " "))) > 1, strsplit(x, " ")[[1]][2], "")))
  }
    
  # Detect infrataxon
  vec0 <- c( "nothossp.", " nothossp ", "nothosubsp.", " nothosubsp ", "cultivar.", 
             " cultivar ",  " subfo ",  "subf."," subf ", " subproles ",  "cf.", " cf ", "aff.", " aff ",  "s.l.", "s.l ",  
             "s.str.", "s.str ", "x.", " x ", "X.", " X ",  "f.", " f ",  "fo.", " fo ", 
             " forma ", "subvar.", " subvar ",  "var.", " var ",  "subsp.", " subsp ",  
             "nm.", " nm ", "prol.", " prol ", " proles ", " race ", "subvar.",  "cv.", " cv ")
  # TODO? investigate suprataxa, e.g., agg.
  InfrataxonRank<-apply(unlist(sapply(names, function(names) 
    unlist(sapply(vec0, function(x) 
      ifelse(length(grep(x, names, fixed = TRUE)) > 0, T, NA))))), 2, function(x) 
          ifelse(all(is.na(x)), NA, names(x[!is.na(x)])))
  InfrataxonRank<-gsub("(^\\s+|\\s+$|(?<=\\s)\\s)", "", InfrataxonRank, perl=T)
  
  if(length(unique(InfrataxonRank))>1)
  {
    for(j in 1:length(unique(InfrataxonRank[!is.na(InfrataxonRank)]))){
      names<-unlist(sapply(names, function(x) gsub(unique(InfrataxonRank[!is.na(InfrataxonRank)])[j]," ", x, fixed = TRUE)))}
    names<-gsub("(^\\s+|\\s+$|(?<=\\s)\\s)", "", names, perl=T)
    ## Problem here because sp is undefined
    #names <- ifelse(substr(names, 1, 1) == " ", substr(sp, 2, nchar(names)), names)
    InfrataxonName <- unlist(sapply(names, function(x) ifelse(length(unlist(strsplit(x, " "))) > 2, strsplit(x, " ")[[1]][3], "")))
    InfrataxonRank<-replace(InfrataxonRank, InfrataxonRank%in%c("subsp", "ssp.", "ssp"), "subsp.")
    InfrataxonRank<-replace(InfrataxonRank, InfrataxonRank%in%c("f", "fo", "fo."), "f.")
    InfrataxonRank<-replace(InfrataxonRank, InfrataxonRank=="var","var.")
    tab$InfrataxonRank<-as.character(InfrataxonRank)
    tab$InfrataxonName<-as.character(InfrataxonName)
    #rownames(tab)[!is.na(tab$InfrataxonRank)]=paste(tab[!is.na(tab$InfrataxonRank),]$Genus, tab[!is.na(tab$InfrataxonRank),]$Species,
    #                                                tab[!is.na(tab$InfrataxonRank),]$InfrataxonRank, tab[!is.na(tab$InfrataxonRank),]$InfrataxonName, sep=" ")
  }
  
  # Research in TaxRef the taxonomic Information
  # FoundName is the name found in the database, which can differ from the original name if there are typos
  # Research names without spelling difference
  sel <- intersect(taxref$LB_NOM,Hmisc::capitalize(rownames(tab)))
  tab[sel,]$FoundName <- sel
  tab$Typo <- ifelse(rownames(tab)%in% sel, F, NA)
  
  # Research names  with spelling errors maxDist
  diff <- setdiff(Hmisc::capitalize(rownames(tab)),taxref$LB_NOM)
  match <- function(x) stringdist::amatch(x,taxref$LB_NOM, maxDist=max.distance)
  diff.match <- as.character(sapply(diff, function(x) taxref$LB_NOM[match(x)]))
  selcor<-diff[!is.na(diff.match)]
  if(length(selcor)>=1) {
    cornames<-diff.match
    tab[selcor,]$FoundName <- cornames[!is.na(cornames)]
    tab[selcor,]$Typo <- T
    tab$Typo[tab$FoundName=="NULL"]<-NA
    tab$FoundName[tab$FoundName=="NULL"]<-NA
  }
  sel<-!is.na(tab$Typo)&tab$FoundName!="IncompleteName"
  
  if(any(sel))
  {
    info <- c()
    for(x in tab[sel,"FoundName"]) {
      buf <- taxref[which(taxref$LB_NOM==x),]
      if(nrow(buf)==1) {
        info <- rbind(info, buf)
      } else {
        buf <- buf[buf$CD_NOM==buf$CD_REF,]
        if(nrow(buf)==1) {
          info <- rbind(info, buf)
        } else info <- rbind(info, c(NA, ncol(buf)))
      }
    }
    start <- ncol(tab)
    tab <- cbind(tab, array(NA,c(nrow(tab),length(taxref.fld))))
    colnames(tab)[-(1:start)] <- taxref.fld
    tab[sel,(start+1):ncol(tab)] <- data.frame(info[,taxref.fld])
    tab[sel,]$Nom.accepte <- taxref[as.character(info$CD_REF),]$LB_NOM
  } else {warning("No match in TaxRef database")}
     
  ## URL in Tropicos
  tab$URL_Tropicos <- unlist(lapply(tab$ID_Tropicos, function(x) ifelse(!is.na(x),
                                                                            paste("http://tropicos.org/Name/", x, sep=""), NA)))
  ## URL in SINP
  tab$URL_SINP <- unlist(lapply(tab$CD_REF, function(x) ifelse(!is.na(x),
                                                                paste("https://inpn.mnhn.fr/espece/cd_nom/", x, sep=""), NA)))
  ## URL in FloreAlpes
  #tab$URL_SINP <- unlist(lapply(tab$LB_NOM, function(x) ifelse(!is.na(x),
  #                                                             paste("https://www.florealpes.com/fiche_", gsub(tolower(x), pattern=" ", replacement=""), ".php", sep=""), NA)))
  
  # Add Baseflor information
  if(baseflor.chk) {
    if (!("baseflor" %in% ls(envir = .GlobalEnv))) {
      load(baseflor.bdd) }
    baseflor <- baseflor[!is.na(baseflor$cd_nom),]
    #if (!("bdtfx" %in% ls(envir = .GlobalEnv))) {
    #  load(bdtfx.bdd) }
    if(min(dim(baseflor))!=0) {
      tab$phytosocio <- sapply(tab$CD_REF, function(x) ifelse(sum(baseflor$cd_nom==x, na.rm=T)==1, baseflor$INDICATION_PHYTOSOCIOLOGIQUE_CARACTERISTIQUE[baseflor$cd_nom==x], NA))
      tab$catminat <- sapply(tab$CD_REF, function(x) ifelse(sum(baseflor$cd_nom==x, na.rm=T)==1, baseflor$code_CATMINAT[baseflor$cd_nom==x], NA))
      tab$pollinisation <- sapply(tab$CD_REF, function(x) ifelse(sum(baseflor$cd_nom==x, na.rm=T)==1, baseflor$pollinisation[baseflor$cd_nom==x], NA))
      tab$fruit <- sapply(tab$CD_REF, function(x) ifelse(sum(baseflor$cd_nom==x, na.rm=T)==1, baseflor$fruit[baseflor$cd_nom==x], NA))
      ellenberg <- c("Lumière", "Température", "Continentalité", "Humidité_atmosphérique", "Humidité_édaphique",
                   "Réaction_du_sol_.pH.", "Niveau_trophique", "Salinité", "Texture",
                   "Matière_organique")
      tab.ellen <- c()
      for(x in tab$CD_REF) {
        if(sum(baseflor$cd_nom==x, na.rm=T)==1) {
          tab.ellen <- rbind(tab.ellen, baseflor[baseflor$cd_nom==x, ellenberg])
          } else tab.ellen <- rbind(tab.ellen, rep(NA,length(ellenberg)))
      }
      colnames(tab.ellen) <- ellenberg  
      tab <- data.frame(tab, tab.ellen)
    }
  }
  
  rownames(tab)<-orig.names
  tab <- data.frame(tab)
  
  if(resolve.infra) {
    #....
  } 
  
  if(!keep) rm(taxref)
      
  if(!phylo)
  {
    # Return a table with original names in Rownames, and information on these taxa in other columns
    return(tab)
  } else
  {
    # Create the phylogeny corresponding to the taxa (create.phylo with default options)
    phylo <- TreeGhats::create.phylo(tab)
    return(list(tab=tab, phylo=phylo$scenario.3))
  }
}
