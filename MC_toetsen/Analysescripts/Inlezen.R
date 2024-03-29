
# Inlezen teleform en invullen metadata -----------------------------------

toetsinfo <- read_xlsx(paste0(Network_directory,"toetsinfo.xlsx"))
databestand <- toetsinfo$databestand
naamtoets <- toetsinfo$`naam toets`
datum <- format(as.Date(toetsinfo$`datum toets`), "%d-%m-%Y") 
nrq <- toetsinfo$`aantal vragen`
nra <- toetsinfo$`aantal antwoordalternatieven`
cesuur <- toetsinfo$cesuur
nrv <- toetsinfo$`aantal versies`
heranalyse <- toetsinfo$heranalyse
taal <- toetsinfo$taal
vakcode <- gsub("_ruwedata.DEL", "", databestand)

if (toetsinfo$samenvoegen == "y") {
  ## Samenvoegen 4 en 6k formulieren
  samenvoegen <- read_xlsx(paste0(Network_directory,"samenvoegen.xlsx"))
  databestand1 <- samenvoegen$databestand1
  databestand2 <- samenvoegen$databestand2
  databestand_new <- samenvoegen$databestand_new
  
  bestanden <-list(paste0(Network_directory,databestand1), paste0(Network_directory,databestand2))
  
  inleesfunctie <- function(x){
   data.table:: fread(x, encoding = 'UTF-8')
  }
  
  teleformdata <- map_df(bestanden ,inleesfunctie)
  
  write.table(teleformdata, paste0(Network_directory,databestand_new), row.names = F, sep="\t")
  
  rm(teleformdata, samenvoegen)
  
  teleformdata <- read.table(paste0(Network_directory,databestand), sep="\t", header = T)

} else {
  ##Open databestand
  teleformdata <- data.table:: fread(paste0(Network_directory,databestand), encoding = 'UTF-8') %>% as.data.frame()
}

teleformdata <- teleformdata %>%
  dplyr:: select(stud_nr, stud_naam, everything())

## Check of er dubbele studentnummers in voorkomen
if (anyDuplicated(teleformdata$stud_nr) > 0) {
  
  write.csv2("Er komen dubbele studentnummers voor, check de ruwe data", paste0(Network_directory,"error.csv"))
  
    stop("dubbele studentnummers")
}
