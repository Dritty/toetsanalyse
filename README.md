# README #

### Doel van deze repository ###

* R script om de tentamendata uit teleform te verwerken tot een itemanalyse
* De huidige versie heeft de volgende functionaliteiten die werken;
	* Toets met 1:4 verschillende versies (alleen vraagvolgorde gewijzigd)
	* Meerdere antwoorden goed kunnen rekenen
	* 2:6 antwoordalternatieven
	* Uitvoeren van hernalyses waarbij vragen verwijderd worden

### Afhankelijkheden ###

* De teleform data is een tab gescheiden .DEL bestand 
* Het pad van de netwerkmap die gebruikt wordt voor de input data is H:/usr, 
deze is gedefineerd in het voorbereidingen script.
* Installeer Pandoc en tinytex (of vergelijkbare software, ivm de werking van RMarkdown) voor het maken van de pdf-itemanalyse
* Tinytex kan als volgt in R geinstalleerd worden: 
install.packages('tinytex')
tinytex::install_tinytex()
* Installeer de laatste versie van Java (ivm de werking van het XLconnect package en het uitslagbestand)
* Gebruik het analyse.bat bestand om de scripts buiten R uit te voeren, voeg
hiervoor o.a. de R map (bijvoorbeeld C:\Program Files\R\R-3.5.1\bin\i386) toe aan het pad van de system environment.

### Who do I talk to? ###

* Repo owner or admin: d.drittij@vu.nl
