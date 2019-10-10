################################################################################
### Mainscript Heranalyse.R
################################################################################
### R code voor Tentamenanalyse Vrije Universiteit Amsterdam
###
### Bestandsnaam: Mainscript Heranalyse.R
### Doel: Script voor het inlezen en analyseren van teleform data voor 
### tentamen met 2:6 antwoordalternatieven en 1:4 versies
### 
### Afhankelijkheden: del bestand, volgordeomzetting en toetsinfo
###
### Gebruikte datasets: ruwe data .DEL bestand, nieuwe sleutel in csv
###
### Opmerkingen: geen
### 
################################################################################
### TODO:
### 1)
################################################################################    
### Geschiedenis:
### 24-04-2018: DD: Aanmaken bestand
### 28-06-2018: DD: Script verplaatst naar subscripts
### 17-07-2018: DD: Input toetsvariabele verplaatst naar toetsinfo excel
### 02-08-2018: DD: Bugs verholpen, checks ingebouwd en relatieve paden verbeterd
################################################################################

# 0. Voorbereidingen ------------------------------------------------------
## Bepaal hier de working directory. Als het bestand vanuit Rstudio uitgevoerd
## wordt is er geen wijziging nodig, als het bestand vanuit een .bat bestand 
## wordt uitgevoerd wordt de meegegeven parameter gebruikt als working directory
if (!"RStudio" %in% commandArgs(trailingOnly = F)) {
  ## Verander de slash van forward naar backward
  working_dir <- gsub("\\\\", "/", commandArgs(trailingOnly = T))
  ## Verwijder "MC_toetsen" uit de directory
  working_dir <- gsub("MC_toetsen/","", working_dir)
  setwd(working_dir)
}

## Lees de packages, functies en libraries in
source("Voorbereidingen.R")
# LET OP: Bij meerdere versies bestand Volgordeomzetting.csv klaarzetten in map

# 1. Inlezen --------------------------------------------------------------
# Lees alle benodigde bestanden in:
# defineer naam bestand, datum, aantal vragen, gokkans en cesuur, aantal versies
# in toetsinfo excel bestand
source("MC_toetsen/Analysescripts/Inlezen.R")

# 2. Manipuleren ----------------------------------------------------------
# Data prepareren voor analyse, o.a. volgordeomzetting doorvoeren
source("MC_toetsen/Analysescripts/Manipuleren.R")

for (i in seq_along(ll)) {
  tryCatch(eval(ll[[i]]), 
           error = function(e) message("Oops!  ", as.character(e)))
}

# 3. Uitvoeren aanpassingen heranalyse -------------------------------------
## Upload eventueel aangepaste/nieuwe sleutel
## Om meerdere antwoorden goed te rekenen lever komma gescheiden aan
## LET OP!! Verwijder ook de verwijderde vragen
source("MC_toetsen/Analysescripts/Aanpassingen_heranalyse.R")

# 4. Analyseren -----------------------------------------------------------
# Genereren itemanalyse en scores
source("MC_toetsen/Analysescripts/Analyseren.R")

for (i in seq_along(ll)) {
  tryCatch(eval(ll[[i]]), 
           error = function(e) message("Oops!  ", as.character(e)))
}

## Schrijf indien gewenst de gescoorde data weg naar csv
if (toetsinfo$scoredata == "y") {
  vrn <- names(sleutel)
  newnames <- c("studentnummer", vrn)
  gescoorde_data <- scored_datax
  colnames(gescoorde_data) <- newnames
  write.csv2(gescoorde_data, paste0(Network_directory,"scoreddata.csv"), row.names = F)
}

# 4A. Vul uitslagbestand --------------------------------------------------
if (startsWith(vakcode, "inh")) {
  
  source("MC_toetsen/Analysescripts/Uitslagbestand_inholland.R")
  
} else if (toetsinfo$taal == "e"){ 
  
  source("MC_toetsen/Analysescripts/Uitslagbestand_Engels.R")
  
} else {
  source("MC_toetsen/Analysescripts/Uitslagbestand.R")
}


# 5. Genereren pdf rapport itemanalyse ------------------------------------
if (toetsinfo$taal == "e"){ 
  thetitle=naamtoets; rmarkdown::render("MC_toetsen/Analysescripts/Itemanalyse_Engels.Rmd", 
                                        output_file = paste0(Network_directory,vakcode,"_",
                                                             "Itemanalysis.pdf"))
} else {
  
thetitle=naamtoets; rmarkdown::render("MC_toetsen/Analysescripts/Itemanalyse.Rmd", 
                                      output_file = paste0(Network_directory,vakcode,"_",
                                                           "Itemanalyse.pdf"))
}

# 6. Genereren inzage rapporten per student in pdf ------------------------
# source("MC_toetsen/Analysescripts/Inzage rapport.R")


# 7. Vraaggroepen ---------------------------------------------------------

#Bereken scores per vraaggroep.
# Pas onderstaand script aan welke vragen bij elkaar horen
vrn <- names(sleutel)
newnames <- c("studentnummers", "studentnamen", vrn)
colnames(scored_datax) <- newnames

vraag_groep <- mutate(scored_datax,
                      groep1= V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+V24+V25+V26+V27+V28+V29+V30+V31+V32+V33+V34+V35+V36+V37+V39+V40+V41+V42+V43+V44+V45,
                      groep2= V46+
                        V47+
                        V48+
                        V49+
                        V50+
                        V51+
                        V53+
                        V54+
                        V55+
                        V56+
                        V57+
                        V58+
                        V59+
                        V60+
                        V61+
                        V62+
                        V63+
                        V64+
                        V65+
                        V66+
                        V67+
                        V68+
                        V69+
                        V70+
                        V71+
                        V72+
                        V73+
                        V74+
                        V75+
                        V76+
                        V77+
                        V78+
                        V79+
                        V80+
                        V81+
                        V82+
                        V83+
                        V84+
                        V85+
                        V86+
                        V87+
                        V88+
                        V89+
                        V90+
                        V91+
                        V92+
                        V93+
                        V94+
                        V95,
                      groep3 = V96+
                        V97+
                        V98+
                        V99+
                        V100+
                        V101+
                        V102+
                        V103+
                        V104+
                        V105+
                        V106+
                        V107+
                        V108+
                        V109+
                        V110+
                        V111+
                        V112+
                        V113+
                        V114+
                        V115+
                        V116+
                        V117+
                        V118+
                        V119+
                        V120+
                        V121+
                        V122+
                        V123+
                        V124+
                        V125+
                        V126+
                        V127+
                        V128+
                        V129+
                        V130+
                        V131+
                        V132+
                        V133+
                        V134+
                        V135+
                        V136+
                        V137+
                        V138+
                        V139+
                        V140,
                      groep4 = V141+
                        V142+
                        V143+
                        V144+
                        V145+
                        V146+
                        V147+
                        V149+
                        V150+
                        V151+
                        V152+
                        V153+
                        V154+
                        V155+
                        V156+
                        V157+
                        V158+
                        V159+
                        V160+
                        V161+
                        V162+
                        V163+
                        V164+
                        V165,
                      groep5 = V166+
                        V167+
                        V168+
                        V169+
                        V170+
                        V171+
                        V172+
                        V173+
                        V174+
                        V175+
                        V176+
                        V177+
                        V178+
                        V179+
                        V180)




vraag_groep_score <- dplyr:: select(vraag_groep,
                                    studentnummers,
                                    groep1,
                                    groep2,
                                    groep3,
                                    groep4,
                                    groep5)

# avg_groep_score <- vraag_groep_score %>%
#   summarise(groep1_m = mean(groep1),
#             groep1_sd = sd(groep1),
#             groep2_m = mean(groep2),
#             groep2_sd = sd(groep2)) %>%
#   t() %>% as.data.frame %>% rownames_to_column(var = "Groep")

write.csv2(vraag_groep_score, file=paste0(Network_directory,
                                          "vraaggroepen.csv"), row.names=FALSE)

# write.csv2(avg_groep_score, file=paste0(Network_directory,
#                                           "vraaggroep_mean_sd.csv"), row.names=FALSE)

# 8. Correlatiematrix vragen genereren ------------------------------------
# ## Correlatiematrix vragen
# par(mfrow=c(1,1))
# corP <- psych:: polychoric(scored_data$scored) 
# corrplot:: corrplot(corP$rho) 


# 9. Analyse van afleiders ------------------------------------------------
# (The point-biserial correlation between that reponse and the total score with that item removed)
# disanalyse <- distractorAnalysis(data, sleutel, multiKeySep=",",
#                                  multiKeyScore=c("or","dich"))

################################################################################
## EINDE
################################################################################
## Clean workspace
rm(list = ls())

