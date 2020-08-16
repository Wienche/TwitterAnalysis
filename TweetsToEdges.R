# ------------------------------
# INTRODUCTIE
# ------------------------------

# Dit script converteert een lijst met tweets uit Coosto naar 
# een edgelist en nodelist die ingelezen kan worden in Gephi. 
# De edgelist en nodelist worden bewaard in de working directory
# onder de naam Edgelist.csv en Nodelist.csv

# Het script bestaat uit tien onderdelen: 

#   1. Libraries laden
#   2. Waarden van variabelen kiezen
#   3. Data inlezen 
#   4. Hashtags extraheren
#   5. Zoekwoorden extraheren
#   6. Timestamp
#   7. Source 
#   8. Target 
#   9. Edgelist 
#  10. Nodelist

# ------------------
# 1. LIBRARIES LADEN
# -------------------

# Installeer (eenmalig) onderstaande packages. 

# install.packages("lubridate")
# install.packages("readr")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("ggplot2")
# install.packages("dummies")
# install.packages("stringr")
# install.packages("stats")

# Activeer de libraries als je een nieuwe R sessie start. 

library(lubridate)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(dummies)
library(stringr)
library(stats)

# --------------------------------
# 2. Waarden van variabelen kiezen
# --------------------------------

# In dit script stel je enkele zaken zelf in door 
# in dit onderdeel de waarden van variabelen te kiezen.  

# Wat is de naam van je working directory (default = "~/Documents/R/TwitterAnalysis/")? 
# Beschrijf het pad vanaf de root van de user. 

var_workingDirectory <- "~/Documents/R/TwitterAnalysis/"

# Wat is (vanuit je working directory) het pad 
# en de naam van je file (default = "data/Coosto_messages.csv")? 

# Om met de default te werken moet je het bestand dat je uit Coosto hebt gehaald
# Coosto_messages.csv noemen en plaatsen in de subdirectory data van de working directory. 

var_data <- "data/Coosto_messages.csv"

# Wat voor separator gebruik je (default = ;)? 
var_delimiter <- ";" 

# Hoeveel hashtags wil je meenemen (default = 20)? 
var_aantalHashtags <- 20

# De binwidth staat nu standaard op 7 (7 dagen - een week). Je kunt dit getal naar believe aanpassen. 
# Maak je het groter, dan krijg je minder kolommen, maak je het kleiner, dan krijg je meer kolommen. 

# Afhankelijk van de tijdspanne van de dataset stel je de binwidth in. Een vuistregel: 
# - Bevat je dataset meerdere jaren -> kies 30 (per maand -> Je krijgt 1 kolom per maand)
# - Bevat je daraset ongeveer een jaar -> kies 7 (per week -> Je krijgt 52 kolommen) 
# - Bevat je dataset ongeveer een mand -> kies 1 (per dag -> Je krijgt 30/31 kolommen)

var_bin <- 7 

# Vul de zoekwoorden die je wilt gebruiken in. Zet elk woord tussen aanhalingstekens, 
# gescheiden door komma's. 

li_z_woord <- c("innovatie", "cluster", "gemeente", "philips")

#Wil je de som van het aantal hashtags of de max (default = max)? 
var_aggregeerNaar <- "sum" 

# ------------------------------
# 2. DATA INLEZEN
# ------------------------------

# Zet de working directory naar je eigen Working directory. 
setwd(var_workingDirectory)

# Dit script gebruikt een Coosto Export (CSV) als uitgangspunt. 
# De file heeft de naam Coosto_messages.csv en gebruikt ";" als separator. 
# Zet het databestand in de subdirectory "data". 

# df_cm_raw <- read_csv2(var_data)

df_cm_raw <- read_delim(var_data, delim = var_delimiter)

# Alle bewerkingen hebben betrekking op de hele dataset. 
# Dus als de dataset zowel tweets als retweets bevat, gaat het resultaat over het totaal. 

# ------------------------------
# 2. HASHTAGS EXTRAHEREN
# ------------------------------

# Het doel is allereerst om de de twintig meest voorkomende hastags te selecteren. 
# Om dit te doen wordt de tekst van de tweet gekopierd naar een nieuwe werkvariable 'tweet'.
# De werkvariabele wordt gebruikt om de originele data intact te laten. 
# Deze werkvariabele 'tweet' wordt in de rest van het script gebruikt. 
# In 'tweet' worden de kapitalen vervangen door kleine letters. 
# Dit om verschillen tussen hoofd- en kleine letters te elimineren. 
# Vervolgens worden de woorden met een # uit de tweet geselecteerd. 
# Al deze hashtags worden als losse items op een lijst geplaatst. 
# De frequentie van alle hashtags wordt geteld.
# Van de top worden dummy variabelen gemaakt. 
# Er wordt een wordcloud getekend van de meest voorkomende Hastags. 
# Voor elke Hashtag wordt een dummy-variabele gemaakt. 

# Maak een nieuwe variabele (tweet) met dezelfde inhoud als 'message text'.
df_cm_raw$tweet = df_cm_raw$`message text` 

# Vervang hoofdletters door kleine letters in tweet. 
df_cm_raw$tweet <- tolower(df_cm_raw$tweet)

# Selecteer de woorden die met een # beginnen. 
df_cm_raw$hashtag <- str_extract_all(df_cm_raw$tweet, "#\\w+")

# Zet de hastags in een dataframe. Wat gebeurt hier...?  

df_cm_raw$hashtag <- sapply(df_cm_raw$hashtag, 
                            function(x) paste(x, collapse=", "))

# De woorden moeten nu bewaard worden ale losse woorden. Om dit te doen: 
# Converteer character to string 
li_hashtagstring <- toString(df_cm_raw$hashtag)

# Vervang alle komma's en spaties
li_hashtagstring <- gsub(",", " ", li_hashtagstring)

# Vervang alle dubbele spaties
li_hashtagstring <- gsub("\\s+", " ", li_hashtagstring)

# Vervang spaties aan het begin (leading) en aan het einde (trailing) 
li_hashtagstring <- trimws(li_hashtagstring)

# Zet de hastags in een dataframe. Wat gebeurt hier...?  
df_hashtags <- as_tibble(data.frame(li_hashtagstring=unlist(strsplit(as.character(li_hashtagstring)," "))))

### VOOR ELKE RIJ SAMENVOEGEN IN LIJST MET VECTOREN. 

df_hashtags <- cbind(df_hashtags, sum = '1')

df_hashtags$sum <- as.integer(df_hashtags$sum)

df_hashtags <- aggregate(df_hashtags$sum, by=list(df_hashtags$li_hashtagstring), FUN=sum)

# Hernoemen van de kolomnamen tot iets begrijpelijks. 
df_hashtags <- rename(df_hashtags, hashtag = Group.1)
df_hashtags <- rename(df_hashtags, aantal = x)

# Sorteer op aantal (niet echt nodig) 
df_hashtags <- arrange(df_hashtags, desc(aantal), hashtag)

# i. Selecteer de top
df_hashtags_top <- as_tibble(top_n(df_hashtags, var_aantalHashtags))

# j. Teken een bar chart
ggplot(df_hashtags_top, aes(x = reorder(hashtag, aantal), y = aantal)) +
    geom_col(color='azure2', fill='cyan3') + 
    coord_flip()

# k. Teken een wordcloud

# install.packages("ggwordcloud")
library(ggwordcloud)

# set.seed(3945786345) -> Nodig!? 
ggplot(df_hashtags_top, aes(label = hashtag, size = aantal)) +
    geom_text_wordcloud() +
    theme_minimal()

# Niet heel fraai. Allerlei instellingen kunnen hier nog worden gemaakt. 
# Meer woorden, dichter op elkaar, horizontaal/verticaal etc. 

# l. Twintig kolommen met dummies van maken 

# Komma's en hashtags verwijderen (anders worden de woorden niet gezien)
df_cm_raw$hashtag <- gsub(",", "", df_cm_raw$hashtag)
df_cm_raw$hashtag <- gsub("#", "", df_cm_raw$hashtag)
df_hashtags_top$hashtag <- gsub("#", "", df_hashtags_top$hashtag)

li_h_woord <- c(df_hashtags_top$hashtag)
print(li_h_woord)

for (i in seq_along(li_h_woord)) {
    var_woord <- li_h_woord[i]
    var_pattern <- paste0("\\b", var_woord, "\\b")
    df_cm_raw <- mutate(df_cm_raw, !!var_woord := as.integer(str_detect(df_cm_raw$hashtag, var_pattern)))
    }

# Veranderen van de namen van de kolommen (zodat ik ze verderop makkelijker kan selecteren). 
for (i in seq_along(li_h_woord)) {
    var_oldname <- li_h_woord[i]
    var_newname <- paste0("h_",var_oldname)
        df_cm_raw <- rename(df_cm_raw, !!var_newname := !!var_oldname)
    }

# ------------------------------
# 3. ZOEKWOORDEN
# ------------------------------

# De basis voor de zoekwoorden is bovenin gemaakt. 

for (i in seq_along(li_z_woord)) {
    var_woord <- li_z_woord[i]
    var_pattern <- paste0("\\b", var_woord, "\\b")
    df_cm_raw <- mutate(df_cm_raw, !!var_woord := as.integer(str_detect(df_cm_raw$tweet, var_pattern)))
}

# Veranderen van de namen van de kolommen (zodat ik ze verderop makkelijker kan selecteren). 
for (i in seq_along(li_z_woord)) {
    var_oldname <- li_z_woord[i]
    var_newname <- paste0("z_",var_oldname)
    df_cm_raw <- rename(df_cm_raw, !!var_newname := !!var_oldname)
}

# Punctuatie nog weghalen in tweet. 
# Frequentie van zoekwoorden uitrekenen. 
# Zoekwoorden en hashtags samenvoegen 
# Tabel maken van 20 hashtags en zoekwoorden 
# Cloud maken van zoekwoorden en hashtags 

# Zet beide lijsten in een vector en voeg ze samen. 
# Kan je maken uit de kolomnamen!! append...?! 
# Combine vecotrs. 

# ------------------------------
# 4. ACTIEVE PERIODE
# ------------------------------

# Omzetten van datum van een string naar een datum. 
df_cm_raw$datum <- as_date(df_cm_raw$date)

# Verloop van de twitterverkeer met ggplot 

ggplot(df_cm_raw, aes(x = datum))+
    geom_histogram(binwidth=7, color='azure2', fill='cyan3')

# De binwidth staat nu standaard op 7 (7 dagen - een week per kolom). Je kunt dit getal aanpassen naar believe. Maak je het groter, dan krijg je minder kolommen, maar je het kleiner, dan krijg je meer kolommen. 
# Afhankelijk van de tijdspanne van de dataset stel je de binwidth in. 
# Een vuistregel: 
# - Meerdere jaren: Kies 30 (per maand -> Je krijgt 1 kolom per maand)
# - Een jaar: Kies 7 (per week -> Je krijgt 52 kolommen) 
# - Een maand: Kies 1 (per dag -> Je krijgt 30/31 kolommen)

# Om de timestamp mee te kunnen nemen naar de edglist... (AANVULLEN)

# Maak een variabele met de maand waarin een tweet verstuurd is. 
# Maak een variabele voor elke maand in maandenset. 
# Geef de waare 1 wanneer een tweet in een bepaalde maand is verstuurd. 
# (en de waarde 0 als dat niet het geval is) 

# Extract jaar en maand uit de datum
df_cm_raw$jaar <- year(df_cm_raw$date)
df_cm_raw$maand <- month(df_cm_raw$date)
df_cm_raw$dag <- day(df_cm_raw$date)

# Voeg jaar en maand samen
df_cm_raw$periode <- ((df_cm_raw$jaar*100) + df_cm_raw$maand)

# Moch je liever dag en maand als eenheid hebben, dan kun je onderstaande 
# regel gebruiken in plaats van bovenstaande (sla die dan over door een # te plaatsen en haal de onderstaande # weg. 

# df_cm_raw$periode <- ((df_cm_raw$jaar*10000) + (df_cm_raw$maand*100) + df_cm_raw$dag)

# Maak dummies voor alle beschikbare periode's

df_cm_raw <- cbind(df_cm_raw, dummy(df_cm_raw$periode, sep="_"))


# GERRIT: 
# Bovenstaande regel leidt tot de volgende error: 
# Error message: 
# Warning message:
#    In model.matrix.default(~x - 1, model.frame(~x - 1), contrasts = FALSE) :
#    non-list contrasts argument ignored

# R: Toon warning niet!? Try... accept! Iets met TryCatch. Of Warning uitzetten. 

#Verwijder de naam van de dataset uit de variabelenamen. 

f_colClean <- function(x){ colnames(x) <- gsub("df_cm_raw_", "p_", colnames(x)); x } 
df_cm_raw <- f_colClean(df_cm_raw) 

# ------------------------------
# 5. MAKEN VAN SOURCE
# ------------------------------

# Maak variabe source (deze vervangt de oorspronkelijke variabele source, maar die doet toch niks)
df_cm_raw$source = df_cm_raw$author

# Vervang hoofdletters door kleine letters. 
df_cm_raw$source <- tolower(df_cm_raw$source)

# ------------------------------
# 6. MAKEN VAN TARGET 
# ------------------------------

# Het maken van Target is een ingewikkelder verhaal. 
# Allereerst moet de @Mention gevonden worden in de tweet. 
# Hiervoor wordt: 
#     - De oorspronkelijke tweet gekopieerd (om de oorspronkelijke data intact te laten)
#     - Vervang hoofdletters door kleine letters. 
#     - Haal de @Mentions eruit en verwijder de '@'
# 
# Het resultaat is een kolom met alle mentions uit de tweet, gescheiden door komma's. 
# Vervolgens worden de Mentions verspreid over meerdere kolommen. 
# De eerste mention wordt in de eerste kolom geplaatst, de tweede kolom geplaatst etc. 
# Wanneer er geen mentions meer zijn blijven de kolommen achter een tweet leeg. 
# Om te weten hoeveel kolommen er moeten komen is het zaak om te achterhalen 
# wat het maximale aantal Mentions is. 
#     - Om het maximale aantal mentions te achterhalen wordt het aantal komma's geteld. 
#     - Het maximale aantal Mentions is het maximale aantal komma's + 1 
#       (Bij "Mention, Mention, Mention" zijn er twee komma's en 2+1 mentions.)
# 
# In de tweede plaats moet de @Mention gekoppeld worden aan de Source. 
# AANVULLEN... 

# Vinden van de @Mentions

# Selecteer de woorden die met een @ beginnen. 
df_cm_raw$mention <- str_extract_all(df_cm_raw$tweet, "@\\w+")

df_cm_raw$mention <- sapply(df_cm_raw$mention, 
                            function(x) paste(x, collapse=", "))

# "@" verwijderen. 

df_cm_raw$mention <- gsub("@", "", df_cm_raw$mention)

# Tel het aantal komma's 
var_aantalKommas <- str_count(df_cm_raw$mention, ",")

# Achterhalen van het maximale aantal komma's 
var_maxAantalKommas <- max(var_aantalKommas)
var_maxAantalKommas

# Achterhalen van het maximale aantal Mentions
var_maxAantalMentions <- var_maxAantalKommas+1

# Aanmaken van de namen van de kolommen 
var_mentionKolommen <- paste("Mention",1:var_maxAantalMentions)

# Verspreiden van de Mentions over de kolommen. 
df_cm_raw <- separate(df_cm_raw, mention, sep=",", into=c(var_mentionKolommen))

# Na deze functie hebben tweets zonder @Mention een leeg veld in Mention 1. 
# Deze regels worden later in het proces verwijderd 
# (anders verliezen we wellicht cruciale informatie in het ruwe databestand)

# ------------------------------
# 7. EDGELIST
# ------------------------------

# In deze stap gaan we van een brede dataset naar een lange dataset. 
# De functie 'gather' wordt hiervoor gebruikt. 
# Vervolgens wordt de data geagreegeerd 
# Vervolgens wordt de data bewaard in een file. 

# Maak het variabele voor de naam van de variabele met het hoogste aantal mentions. 
var_hoogsteAantalMentions <- paste("Mention", var_maxAantalMentions)

# Maak de edgelist. 
df_Edgelist <- df_cm_raw %>%
    gather (key = "mention", 
            value = "target", 
            na.rm = TRUE, 
            c(`Mention 1`:var_hoogsteAantalMentions))

# Verwijderen van tweets zonder @Mentions (target is leeg). 
df_Edgelist <- df_Edgelist[-which(df_Edgelist$target == ""), ]

# ***
# Aggregeren van de edgelist 

# Om te aggregeren zijn numerieke variabelen nodig. 
# Maak numerieke van de variabelen die beginnen met 
# - h_ (hashtags), 
# - z_ (zoekwoorden) of
# - p_ (timestamp)

# Zet alle variabele namen in een lijst. 
li_variabelen <- colnames(df_cm_raw)

# Selecteer alleen de variabelen die beginnen met h_, z_ of p_
li_variabelen <- grep("^h_|^z_|^p_", li_variabelen, value = TRUE)

# Bewaar alle woorden als lijst. 
li_variabelen <- as.list(li_variabelen)

# Maak numerieke variabelen van de subset van hashtags, zoekwoorden en timestamps. 
for (i in li_variabelen) {
    df_Edgelist[,i] <- as.numeric(df_Edgelist[,i])
    }

# Subset source, target en alle variabelen de Hashtags, Zoekwoorden en perioden. 

# Eerst Source en Target toevoegen aan de lijst van variabelen die we moeten behouden. 
li_variabelen <- c(li_variabelen, "source")
li_variabelen <- c(li_variabelen, "target")

# Subset de dataframe naar alleen source, target en hashtags, zoekwoorden en perioden
var_colsEdges <- match(li_variabelen, names(df_Edgelist))
df_EdgelistSubset <- df_Edgelist[,var_colsEdges] 

# Aggregeer naar de combinatie source en target, tel weight en sommeer de overige variabelen. 

df_EdgelistAggr <- aggregate(. ~ source+target, data = df_EdgelistSubset, 
                             FUN = var_aggregeerNaar)

# Vervang h_ door # 
# f_zetHashtagTerug <- function(x){ colnames(x) <- gsub("h_", "#", colnames(x)); x } 
# df_EdgelistAggr <- f_zetHashtagTerug(df_EdgelistAggr) 

# Vervang z_ door niks
f_haalz_Weg <- function(x){ colnames(x) <- gsub("z_", "", colnames(x)); x } 
df_EdgelistAggr <- f_haalz_Weg(df_EdgelistAggr) 

# Vervang p_ door niks
f_haalp_Weg <- function(x){ colnames(x) <- gsub("p_", "", colnames(x)); x } 
df_EdgelistAggr <- f_haalp_Weg(df_EdgelistAggr) 

# Verander Source en Target. 
names(df_EdgelistAggr)[names(df_EdgelistAggr) == "source"] <- "Source"
names(df_EdgelistAggr)[names(df_EdgelistAggr) == "target"] <- "Target"

# Voeg een eerste kolom in (Apple bug)

df_EdgelistAggr$Nr <- seq.int(nrow(df_EdgelistAggr))

var_move <- "Nr"
df_EdgelistAggr <- df_EdgelistAggr[c(var_move, setdiff(names(df_EdgelistAggr), var_move))]

# Bewaren van de Edglist als .csv bestand. 

write.csv2(df_EdgelistAggr,'Edgelist.csv', row.names=FALSE)

# ------------------------------
# 8. MAKEN VAN NODELIST
# ------------------------------

# Subset de variabelen die voor de nodelist nodig zijn. 

li_nodeVariabelen <- c("source", "views", "influence", "followers")

# Subset de dataframe naar alleen source, target en hashtags, zoekwoorden en perioden
var_colsNodes <- match(li_nodeVariabelen, names(df_Edgelist))
df_NodelistSubset <- df_Edgelist[,var_colsNodes] 

# Aggregeer naar de combinatie source en target, tel weight en sommeer de overige variabelen. 

df_NodelistAggr <- aggregate(. ~ source, data = df_NodelistSubset, 
                             FUN = mean)

# Verander de kolomnaam van source naar id 
names(df_NodelistAggr)[names(df_NodelistAggr) == "source"] <- "Id"

# Bewaren van de Edglist als .csv bestand. 

write.csv(df_NodelistAggr,'Nodelist.csv')


