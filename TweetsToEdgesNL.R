
# ------------------------------
# INTRODUCTIE
# ------------------------------

# Dit script converteert een lijst met tweets uit Coosto naar 
# een edgelist en nodelist die ingelezen kan worden in Gephi. 
# De edgelist en nodelist worden bewaard in de working directory
# onder de naam Edgelist.csv en Nodelist.csv

# De edgelist is een lijst van twitter-accounts en @mentions uit de tweet 
# Daarbij ook de frequentie van de gebruikte hashtags, zoekwoorden en actieve periode's. 

# De nodelist is een lijst van twitter-accounts, het gemiddelde aantal 
# views, influence en followers. 

# Het script bestaat uit tien stappen: 

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

# ------------------------------
# 1. LIBRARIES LADEN
# ------------------------------

# Installeer (eenmalig) onderstaande packages. 

# install.packages("tidyverse")
# install.packages("lubridate")
# install.packages("dummies")
# install.packages("stringr")
# install.packages("stats")

# Activeer de libraries als je een nieuwe R sessie start. 

library(tidyverse)
library(lubridate)
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

var_data <- "dataGroningen/DatasetGroningen.csv"

# Wat voor separator gebruik je (default = ;)? 
var_delimiter <- ";" 

# Hoeveel hashtags wil je meenemen (default = 20)? 
var_aantalHashtags <- 20

# Welke zoekwoorden wil je meenemen? 
# Vul de zoekwoorden die je wilt gebruiken in. Zet elk woord tussen aanhalingstekens, 
# gescheiden door komma's. Je kunt zelf kiezen hoeveel zoekwoorden je gebruikt. 
li_z_woord <- c("innovatie", "cluster", "gemeente", "philips")

# De binwidth staat nu standaard op 7 (7 dagen staat voor een week). Je kunt dit getal naar believe aanpassen. 
# Maak je het groter, dan krijg je minder kolommen, maak je het kleiner, dan krijg je meer kolommen. 

# Afhankelijk van de tijdspanne van de dataset stel je de binwidth in. Een vuistregel: 
# - Bevat je dataset meerdere jaren -> kies 30 (per maand -> Je krijgt 1 kolom per maand)
# - Bevat je dataset ongeveer een jaar -> kies 7 (per week -> Je krijgt 52 kolommen) 
# - Bevat je dataset ongeveer een maand -> kies 1 (per dag -> Je krijgt 30/31 kolommen)

var_bin <- 7 

# ------------------------------
# 3. DATA INLEZEN
# ------------------------------

# Zet de working directory naar je eigen Working directory 
# (als je niet vanuit Github in een project werkt). 

setwd(var_workingDirectory)

# Dit script gebruikt een Coosto Export (CSV) als uitgangspunt. 
# De file heeft de naam Coosto_messages.csv en gebruikt ";" als separator. 
# Zet het databestand in de subdirectory "data". 

df_cm_raw <- read_delim(var_data, delim = var_delimiter)

# Verwijder regels die geen inhoud hebben op "message text"
df_cm_raw <- df_cm_raw[!is.na(df_cm_raw $'bericht tekst'),]

# Alle bewerkingen hebben betrekking op de hele dataset. 
# Dus als de dataset zowel tweets als retweets bevat, gaat het resultaat over het totaal.

# ------------------------------
# 4. HASHTAGS EXTRAHEREN
# ------------------------------

# Het doel is allereerst om de de twintig meest voorkomende hastags te selecteren. 
# Om dit te doen wordt de tekst van de tweet gekopierd naar een nieuwe werkvariable 'tweet'.
# De werkvariabele wordt gebruikt om de originele data intact te laten. 
# In 'tweet' worden de kapitalen vervangen door kleine letters. 
# Dit om verschillen tussen hoofd- en kleine letters te elimineren. 
# Vervolgens worden de woorden met een # uit de tweet geselecteerd. 
# Al deze hashtags worden als losse items op een lijst geplaatst. 
# De frequentie van alle hashtags wordt geteld.
# Van de top 20 (default) van Hashtags worden dummy variabelen gemaakt. 

# Maak een nieuwe variabele (tweet) met dezelfde inhoud als 'message text'.
df_cm_raw$tweet = df_cm_raw$`bericht tekst` 

# Vervang hoofdletters door kleine letters in tweet. 
df_cm_raw$tweet <- tolower(df_cm_raw$tweet)

# Selecteer de woorden die met een # beginnen. 
df_cm_raw$hashtag <- str_extract_all(df_cm_raw$tweet, "#\\w+")

# Zet de hastags in een dataframe.
df_cm_raw$hashtag <- sapply(df_cm_raw$hashtag, 
                            function(x) paste(x, collapse=","))

# De woorden moeten nu bewaard worden ale losse woorden. Om dit te doen: 
li_hashtagstring <- df_cm_raw$hashtag %>%   
    toString() %>%            # Converteer naar string
    gsub("," , " ",.) %>%     # Vervang alle komma's en spaties
    gsub("\\s+", "",.) %>%    # Vervang alle dubbele spaties
    trimws()                  # Vervang spaties aan het begin (leading) 
# en aan het einde (trailing) 

# Zet de hastags in een dataframe.
df_hashtags <- as_tibble(data.frame(li_hashtagstring=unlist(strsplit(as.character(li_hashtagstring),"#"))))

# Hang een teller achter de hashtages. 
df_hashtags <- cbind(df_hashtags, sum = 1)

# Aggregeer de data op de hashtag, sommeer de teller. 
df_hashtags <- aggregate(df_hashtags$sum, by=list(df_hashtags$li_hashtagstring), FUN=sum)

# Hernoemen van de kolomnamen tot iets begrijpelijks. 
df_hashtags <- rename(df_hashtags, hashtag = Group.1)
df_hashtags <- rename(df_hashtags, aantal = x)

# Sorteer op aantal (niet echt nodig) 
df_hashtags <- arrange(df_hashtags, desc(aantal), hashtag)

# i. Selecteer de top (default is op 20 gesteld)
df_hashtags_top <- as_tibble(top_n(df_hashtags, var_aantalHashtags))

# j. Teken een bar chart
ggplot(df_hashtags_top, aes(x = reorder(hashtag, aantal), y = aantal)) +
    geom_col(color='gray90', fill='#00968F') + 
    labs(x="", y="Aantal keer genoemd") +
    coord_flip()

ggsave("plot_hashtags.png")

# l. Twintig kolommen met dummies van maken 

# Komma's en hashtags verwijderen (anders worden de woorden niet gezien)
# Vervang de # door de prefix h_ (is verderop nodig). 

for (i in seq_along(df_hashtags_top$hashtag)) {
    var_woord <- df_hashtags_top$hashtag[i]
    var_pattern <- paste0("\\b", var_woord, "\\b")
    df_cm_raw <- mutate(df_cm_raw, !!paste0("h_",var_woord) := as.integer(str_detect(df_cm_raw$hashtag, var_pattern)))
}

# ------------------------------
# 5. ZOEKWOORDEN
# ------------------------------

# De zoekwoorden zijn in het begin ingevuld (zie sectie 2). 

for (i in seq_along(li_z_woord)) {
    var_woord <- li_z_woord[i]
    var_pattern <- paste0("\\b", var_woord, "\\b")
    df_cm_raw <- mutate(df_cm_raw, !!paste0("z_",var_woord) := as.integer(str_detect(df_cm_raw$tweet, var_pattern)))
}

# ------------------------------
# 6. ACTIEVE PERIODE
# ------------------------------

# Kopier de originele variabele naar een nieuwe naam
df_cm_raw$datumnl <- df_cm_raw$datum

# Omzetten van datum-tijd naar alleen datum. 
df_cm_raw$datum <- dmy_hm(df_cm_raw$datum)
df_cm_raw$datum <- as_date(df_cm_raw$datum)

# Gebruik deze als je de engelse versie hebt. 
# df_cm_raw$datum <- as_date(df_cm_raw$date)

# Verloop van de twitterverkeer met ggplot 
ggplot(df_cm_raw, aes(x = datum))+
    geom_histogram(binwidth=var_bin, color='gray90', fill='#00968F') +
    labs(x="Tijd", y="Aantal tweets")

ggsave("plot_verloop.png")

# Maak een variabele met de maand waarin een tweet verstuurd is. 
# Geef de waarde 1 wanneer een tweet in een bepaalde maand is verstuurd. 
# (en de waarde 0 als dat niet het geval is) 

# Extract jaar, maand en dag uit de datum
df_cm_raw$jaar <- year(df_cm_raw$datum)
df_cm_raw$maand <- month(df_cm_raw$datum)
df_cm_raw$dag <- day(df_cm_raw$datum)

# Voeg jaar en maand samen
df_cm_raw$periode <- ((df_cm_raw$jaar*100) + df_cm_raw$maand)

# Mocht je liever dag als eenheid hebben, dan kun je onderstaande 
# regel gebruiken in plaats van bovenstaande (sla die dan over door een # te plaatsen en haal de onderstaande # weg. 

# df_cm_raw$periode <- ((df_cm_raw$jaar*10000) + (df_cm_raw$maand*100) + df_cm_raw$dag)

# Maak dummies voor alle beschikbare periode's

df_cm_raw <- cbind(df_cm_raw, dummy(df_cm_raw$periode, sep="_"))

# Verwijder de naam van de dataset uit de variabelenamen. 

colnames(df_cm_raw) <- gsub("df_cm_raw_", "p_", colnames(df_cm_raw))

# ------------------------------
# 7. MAKEN VAN SOURCE
# ------------------------------

# Maak variabe source (deze vervangt de oorspronkelijke variabele source, maar die doet toch niks)
df_cm_raw$source = df_cm_raw$auteur

# Vervang hoofdletters door kleine letters. 
df_cm_raw$source <- tolower(df_cm_raw$source)

# ------------------------------
# 8. MAKEN VAN TARGET 
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
                            function(x) paste(x, collapse=","))

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
# 9. EDGELIST
# ------------------------------

# In deze stap gaan we van een brede dataset naar een lange dataset. 
# De functie 'gather' wordt hiervoor gebruikt. 
# Vervolgens wordt de data geagreegeerd 
# Vervolgens wordt de data bewaard in een file. 

# Maak het variabele voor de naam van de variabele met het hoogste aantal mentions. 
var_hoogsteAantalMentions <- paste("Mention", var_maxAantalMentions)

# Vervabg lege eerste @Mentions door NA. 
df_cm_raw$`Mention 1`[df_cm_raw$`Mention 1` == ""] <- NA

# Maak de edgelist. 
df_Edgelist <- df_cm_raw %>%
    gather (key = "mention", 
            value = "target", 
            na.rm = TRUE, 
            c(`Mention 1`:var_hoogsteAantalMentions))

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

# Ook nog datum toevoegen 
li_variabelen <- c(li_variabelen, "datum")

# Subset de dataframe naar alleen source, target en hashtags, zoekwoorden en perioden
var_colsEdges <- match(li_variabelen, names(df_Edgelist))
df_EdgelistSubset <- df_Edgelist[,var_colsEdges] 

# Maak een numerieke teller voor gewicht
df_EdgelistSubset <- cbind(df_EdgelistSubset, weight = '1')
df_EdgelistSubset$weight <- as.numeric(df_EdgelistSubset$weight)

# Hier wordt alsvast een lijst gemaakt 
# van variabelen die in de nodelist moeten komen te staan.
# Dit is straks pas nodig. 

li_NodeVariabelen <- as.list(colnames(df_EdgelistSubset))

# On with the script... 

# De functies hieronder zorgen dat de variabele namen er goed uit komen te zien. 

f_zetHashtagTerug <- function(x){ colnames(x) <- gsub("h_", "#", colnames(x)); x } 
df_EdgelistSubset <- f_zetHashtagTerug(df_EdgelistSubset) 

# Vervang z_ door niks
f_haalz_Weg <- function(x){ colnames(x) <- gsub("z_", "", colnames(x)); x } 
df_EdgelistSubset <- f_haalz_Weg(df_EdgelistSubset) 

# Vervang p_ door niks
f_haalp_Weg <- function(x){ colnames(x) <- gsub("p_", "", colnames(x)); x } 
df_EdgelistSubset <- f_haalp_Weg(df_EdgelistSubset) 

# Verander Source en Target. 
names(df_EdgelistSubset)[names(df_EdgelistSubset) == "source"] <- "Source"
names(df_EdgelistSubset)[names(df_EdgelistSubset) == "target"] <- "Target"

# Voeg een eerste kolom in (Als Gephi draait op een Apple is het nodig dat de 
# eerste kolom niet Source is. Daarom wordt er een kolom voorgezet)

df_EdgelistSubset$Nr <- seq.int(nrow(df_EdgelistSubset))

var_move <- "datum"
df_EdgelistSubset <- df_EdgelistSubset[c(var_move, setdiff(names(df_EdgelistSubset), var_move))]

var_move <- "Target"
df_EdgelistSubset <- df_EdgelistSubset[c(var_move, setdiff(names(df_EdgelistSubset), var_move))]

var_move <- "Source"
df_EdgelistSubset <- df_EdgelistSubset[c(var_move, setdiff(names(df_EdgelistSubset), var_move))]

var_move <- "Nr"
df_EdgelistSubset <- df_EdgelistSubset[c(var_move, setdiff(names(df_EdgelistSubset), var_move))]

# Bewaren van de Edglist als .csv bestand. 

write.csv2(df_EdgelistSubset,'Edgelist.csv', row.names=FALSE)

# ------------------------------
# 10. MAKEN VAN NODELIST
# ------------------------------

# Toevoegen van andere gegevens: 

# Target eraf 
li_NodeVariabelen <- li_NodeVariabelen[li_NodeVariabelen !="target"] 

# Weight eraf 
li_NodeVariabelen <- li_NodeVariabelen[li_NodeVariabelen !="weight"] 

# Datum eraf 
li_NodeVariabelen <- li_NodeVariabelen[li_NodeVariabelen !="datum"] 

# views followers en influence erbij. 
li_NodeVariabelen <- c(li_NodeVariabelen, "views", "volgers","invloed")

# Subset de dataframe naar alleen de variabelen die relevant zijn voor de nodes. 
var_subset <- match(li_NodeVariabelen, names(df_cm_raw))
df_NodelistSubset <- df_cm_raw[,var_subset]

# Toevoegen AantalTweets
df_NodelistSubset <- cbind(df_NodelistSubset, AantalTweets = 1)

# Hernoem de variabelen. 

# Zet # terug
df_NodelistSubset <- f_zetHashtagTerug(df_NodelistSubset) 

# Vervang z_ door niks
df_NodelistSubset <- f_haalz_Weg(df_NodelistSubset) 

# Vervang p_ door niks
df_NodelistSubset <- f_haalp_Weg(df_NodelistSubset) 

# Vervang NA in views met 0 
df_NodelistSubset$views[is.na(df_NodelistSubset$views)] <- 0

# Aggregeer naar source, tel AantalTweets en sommeer de overige variabelen. 

# Sommeer alle kolommen 
df_NodelistAggr <- aggregate(. ~ source, data = df_NodelistSubset, 
                             FUN = sum, na.action=na.omit)

# Verwijder de kolommen waarover je het gemiddelde wilt berekenen 

var_drops <- c("invloed","volgers")
df_NodelistAggr <- df_NodelistAggr[ , !(names(df_NodelistAggr) %in% var_drops)]

# Maak geagregeerde gemiddelden voor Influence en Followers

df_NLA_Influence <- aggregate(invloed ~ source, data = df_NodelistSubset, 
                              FUN = mean)
df_NLA_Followers <- aggregate(volgers ~ source, data = df_NodelistSubset, 
                              FUN = mean)

#Rond af Influence op 1 digit, followers op 0). 
df_NLA_Influence$invloed <- round(df_NLA_Influence$invloed, 1)
df_NLA_Followers$volgers <- round(df_NLA_Followers$volgers, 0)

# Voeg alles samen 

df_NodelistAggr <- full_join(df_NodelistAggr, df_NLA_Influence, by="source")
df_NodelistAggr <- full_join(df_NodelistAggr, df_NLA_Followers, by="source")

# Verander de volgorde van de variabelen in de dataframe. 

var_move <- "views"
df_NodelistAggr <- df_NodelistAggr[c(var_move, setdiff(names(df_NodelistAggr), var_move))]

var_move <- "invloed"
df_NodelistAggr <- df_NodelistAggr[c(var_move, setdiff(names(df_NodelistAggr), var_move))]

var_move <- "volgers"
df_NodelistAggr <- df_NodelistAggr[c(var_move, setdiff(names(df_NodelistAggr), var_move))]

var_move <- "AantalTweets"
df_NodelistAggr <- df_NodelistAggr[c(var_move, setdiff(names(df_NodelistAggr), var_move))]

var_move <- "source"
df_NodelistAggr <- df_NodelistAggr[c(var_move, setdiff(names(df_NodelistAggr), var_move))]

# Verander de kolomnaam van source naar id 
names(df_NodelistAggr)[names(df_NodelistAggr) == "source"] <- "Id"

# Bewaren van de Edglist als .csv bestand. 
write.csv(df_NodelistAggr,'Nodelist.csv', row.names=FALSE)

# Verwijder alle object behalve de objecten die in bewaar staan (haal de hashtags weg). 
#var_bewaar <- c("df_NodelistAggr", "df_EdgelistSubset")
#rm(list=setdiff(ls(), var_bewaar))


 n