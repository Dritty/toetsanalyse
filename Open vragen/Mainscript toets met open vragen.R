################################################################################
### Mainscript toets met open vragen.R
################################################################################
### R code voor Tentamenanalyse Vrije Universiteit Amsterdam
###
### Bestandsnaam: Mainscript toets met open vragen.R
### Doel: Startscript voor het inlezen en analyse van data van 
### een toets met open vragen (op basis van bestand inclusief studentnummers)
### 
### Afhankelijkheden: geen
###
### Gebruikte datasets: gescoorde data en maximale score per vraag
###
### Opmerkingen: geen
### 
################################################################################
### TODO:
### 1) Testen
###
################################################################################    
### Geschiedenis:
### 08-03-2018: DD: Aanmaken bestand
### 23-5-2018: DD: Code ingekort met pipes
################################################################################

############################################################################
## 00 VOORBEREIDINGEN
############################################################################
## Lees de packages, functies en libraries in
source("Voorbereidingen.R")

################################################################################
## 1. INLEZEN
################################################################################
## Lees alle benodigde bestanden in:
##Verander hier de naam van het bestand dat je in wil lezen, 
## defineer aantal vragen, gokkans en cesuur

databestand <- dlgInput("Wat is de naam van het ruwe data bestand? ", 
                        Sys.info()["databestand"])$res
max_score <- dlgInput("Wat is de naam van het bestand met de max score? ", 
                        Sys.info()["max_score"])$res
naamtoets <- dlgInput("Wat is de naam van de toets ", 
                      Sys.info()["naamtoets"])$res
datum <- dlgInput("Datum afname toets ", Sys.info()["datumtoets"])$res
cesuur <- dlgInput("Wat is de cesuur? ", Sys.info()["cesuur"])$res
cesuur <- as.numeric(cesuur)

##Inlezen dataset
scores <- read.csv2(paste0(Network_directory,databestand), row.names = 1)

##Inlezen max score per vraag en bepalen maximale score toets
maxscore <- read.csv2(paste0(Network_directory,max_score), row.names = 1)
maxtest <- sum(maxscore)

##Bepaal aantal vragen en aantal kolommen 
##(voorwaarde: indien eerste kolom studentnummers is)
nrq <- ncol(scores)

################################################################################
## 2. MANIPULEREN
################################################################################
##Lege cellen vervangen met nullen
scores[is.na(scores)] <- 0

##Maken itemanalyse (o.a. cronbachs alpha en testgemiddelde)
itemanalyse <- psych:: alpha(scores[,1:nrq])

##Verwijder niet relevante kolommen uit item analyse en bereken p waarden
itemanalyse_rapport <- itemanalyse$item.stats %>% dplyr:: select(n, r.drop, mean, sd) %>% 
                                                  dplyr:: rename(rir = r.drop) %>% 
                                                  dplyr:: mutate(Max.score = maxscore$maxscore,
                                                                 P = mean/Max.score) %>% 
                                                  dplyr:: select(n, P, rir, Max.score, mean, sd)

##Extract cronbachs alpha
itemanalyse1 <- itemanalyse$total

##Berekenen scores per student
scores$studentscores <- rowSums(scores[1:nrq])

##Toon Histogram scoreverdeling
hist(scores$studentscores, xlab="Studentscores", ylab="Aantal studenten", 
     xlim=range(0:maxtest), main = paste("Histogram studentscores"))

##Berekenen kappa
ca <- itemanalyse1$raw_alpha
tmean <- mean(scores$studentscores)
obvar <- var(scores$studentscores)
kappa <- ((ca)*(obvar)+(tmean-cesuur)^2)/((obvar) + (tmean-cesuur)^2)

## Aanvullen toetswaarden
itemanalyse1 <- itemanalyse1 %>% mutate(kappa = kappa, 
                                        `gemiddelde score` = mean(scores$studentscores),
                                        sd = sd(scores$studentscores),
                                        `aantal studenten` = nrow(scores),
                                        `aantal vragen` = nrq) %>% 
                                dplyr:: select(`aantal studenten`, 
                                       `aantal vragen`, 
                                       raw_alpha, 
                                       kappa, 
                                       `gemiddelde score`, 
                                       sd) %>% 
                                mutate(`P-Gem` = `gemiddelde score`/sum(maxscore$maxscore),
                                       Max.score = sum(maxscore$maxscore),
                                       cesuur = cesuur)

## Wegschrijven bestanden voor rapport
write.csv2(itemanalyse1, paste0(Network_directory,"toetswaarden.csv"))
write.csv2(itemanalyse_rapport, paste0(Network_directory,"itemanalyse.csv"))
write.csv2(scores, paste0(Network_directory,"results_student.csv"))

################################################################################
## 3. Maken rapport in pdf
################################################################################
## Maken van itemanalyse in pdf
thetitle=naamtoets; rmarkdown::render("Open vragen/Itemanalyse.Rmd", 
                                      output_file = paste0(Network_directory,
                                                           "Itemanalyse.pdf"))

################################################################################
## 4. Extra functies
################################################################################
##Maak itemcurves op basis van percentiel groepen

#Bereken totaalscore per student
scores$sum <- rowSums(scores[1:nrq])

#Plaats student in een rank-groep (5 gelijke groepen)
scores$RANK=ntiles(scores, dv = "sum", bins = 5)

## Bereken gemiddelde score vraag per rank-groep
rankgroup <- by(scores[, 1:nrq], scores$RANK, colMeans)

## Creer dataframe met gem. score per rankgroep per vraag en hernoem kopjes.
x <- c(1:nrq)
RankV <- matrix(c(rankgroup$`1`[x], rankgroup$`2`[x], rankgroup$`3`[x], 
                  rankgroup$`4`[x], rankgroup$`5`[x]), ncol=nrq,byrow=TRUE)
RankVdf <- as.data.frame(RankV)

##Kolomnamen hernoemen naar juiste vraagnummer
names = c(colnames(scores[1:nrq]))
colnames(RankVdf) = names

##Bereken p-waarde per vraag per rankgroep
##Transpose max score per vraag
nmaxscore <- t(maxscore)
nmaxscore <- as.data.frame(nmaxscore)
for (i in 1:5) RankVdf[i,] = RankVdf[i,]/nmaxscore

RankVdf <- dplyr:: mutate(RankVdf, Rankgroep=c("rankgroup1", "rankgroup2", 
                                       "rankgroup3", "rankgroup4", 
                                       "rankgroup5"))

##Plot itemcurves in 1 plot met ggplot
ggplot2:: ggplot(reshape2:: melt(RankVdf, id.vars='Rankgroep'), 
                 aes(Rankgroep, value, col=variable)) + 
  geom_point(size = 4, alpha=0.6)+
  geom_line(aes(Rankgroep, value, group=variable)) 

# +facet_grid(variable~.)

################################################################################
## EINDE
################################################################################
## Clean workspace
rm(list = ls())

