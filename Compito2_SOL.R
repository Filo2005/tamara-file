library(tidyverse)
library(haven)
library(ggplot2)
library(vcd)

rm(list=ls(all=TRUE))

# ### Esercizio 1 ###

# 1- Aprire il File Dati "Laureati 2004-ind 2007_compito 2".
data <- read_dta("...\\Laureati 2004-ind 2007_compito 2 (1).dta")


# Creare variabile "Classe sociale di origine" usando principalmente classe sociale del padre, se mancante utilizzare classe sociale della madre.
data$classe_origine <- NA

data$classe_origine[data$classe_p == 1]<-1
data$classe_origine[data$classe_p == 2]<-2
data$classe_origine[data$classe_p == 3]<-3

data$classe_origine[data$classe_p == 9 & data$classe_m ==1]<-1
data$classe_origine[data$classe_p == 9 & data$classe_m ==2]<-2
data$classe_origine[data$classe_p == 9 & data$classe_m ==3]<-3
data$classe_origine[data$classe_p == 9 & data$classe_m ==9]<-9
# Ricodificare le categorie:
data$classe_origine[data$classe_origine == 1] <- "Borghesia"
data$classe_origine[data$classe_origine == 2] <- "Classe media"
data$classe_origine[data$classe_origine == 3] <- "Classe operaia"


# 3- Controllare correttezza e analisi monovariata
table(data$classe_origine, useNA = "ifany")
table(data$classe_origine, data$classe_p)
table(data$classe_origine, data$classe_m)

freq <- table(data$classe_origine, useNA = "ifany")
prop.table(freq) * 100
barplot(freq, main="Classe sociale di origine", ylab="Frequenza", col="skyblue") #this is optional

# La distribuzione della variabile classe sociale di origine mostra che la classe più numerosa è quella media (43.22%), 
#seguita dalla borghesia (35,37%) e, in terza posizione, dalla classe operaia (19,2%). 
#La categoria "Classe media" rappresenta il gruppo prevalente nel campione analizzato, indicando che la maggioranza degli intervistati 
#proviene da contesti socioeconomici intermedi. La classe operaia risulta invece significativamente meno rappresentata, 
#indicando una minore presenza nel campione di soggetti provenienti da ambienti socioeconomici più bassi.

# 4- Creare variabili dummy
data$borghesia <- ifelse(data$classe_origine == "Borghesia", 1, 0)
data$classe_media <- ifelse(data$classe_origine == "Classe media", 1, 0)
data$classe_operaia <- ifelse(data$classe_origine == "Classe operaia", 1, 0)

# Controllo che il procedimento sia corretto
table(data$borghesia, data$classe_origine)
table(data$classe_media, data$classe_origine)
table(data$classe_operaia, data$classe_origine)


# ### Esercizio 2 ###

# a) Classe sociale (indep.) e reddito categoriale (dep.)
cont_table1 <- table(data$classe_origine, data$redd_cat)
prop.table(cont_table1, 1)*100

#Dalla tabella di contingenza percentuale emerge chiaramente che la distribuzione del reddito varia significativamente in base alla classe sociale di origine. In particolare, i laureati provenienti dalla Borghesia mostrano più frequentemente redditi elevati ("oltre 1500 euro") rispetto alle altre classi sociali. La Classe media tende a concentrarsi principalmente nella fascia intermedia ("tra 1000 e 1500 euro"), mentre la Classe operaia mostra una distribuzione più bilanciata tra le diverse categorie di reddito, con una leggera prevalenza della fascia intermedia.
chisq.test(cont_table1)
assocstats(cont_table1)

#Il risultato del test Chi-quadrato conferma che l’associazione tra classe sociale di origine e categoria di reddito è statisticamente significativa (p-value < 0.001), suggerendo che le differenze osservate nelle distribuzioni non sono dovute al caso. Tuttavia, i valori dei coefficienti di associazione risultano relativamente bassi (V di Cramer = 0.059 e Coefficiente di contingenza = 0.083), indicando che l'associazione, sebbene significativa, è piuttosto debole. Pertanto, la classe sociale di origine ha un'influenza statisticamente significativa ma limitata sul livello di reddito percepito dai laureati.

# b) Ripartizione geografica (indep.) e reddito numerico (dep.)
# Creare variabili dummy per ciascuna categoria della variabile "rip_geo"
table(data$rip_geo)

data$ripgeo_1 <- ifelse(data$rip_geo == 1, 1, 0)
data$ripgeo_2 <- ifelse(data$rip_geo == 2, 1, 0)
data$ripgeo_3 <- ifelse(data$rip_geo == 3, 1, 0)

# Controllo dummy
table(data$ripgeo_1, data$rip_geo)
table(data$ripgeo_2, data$rip_geo)
table(data$ripgeo_3, data$rip_geo)

# Regressione lineare del reddito sulle variabili dummy di ripartizione geografica
# Scegliendo ripgeo_1 come categoria di riferimento:

model_geo <- lm(redd_per ~ ripgeo_2 + ripgeo_3, data = data)
summary(model_geo)

#La regressione lineare mostra che la ripartizione geografica ha un effetto statisticamente significativo sul reddito mensile percepito dai laureati.

#Intercetta (Nord): L’intercetta, pari a 1227 euro, rappresenta il reddito medio atteso per chi risiede nel Nord Italia (categoria di riferimento).

#Centro: Il coefficiente di ripgeo_2 (-52.5) indica che, a parità di altre condizioni, i laureati residenti nel Centro percepiscono in media 52 euro in meno rispetto a quelli del Nord. Il risultato è altamente significativo (p < 0.001).

#Sud: Il coefficiente di ripgeo_3 (-151.2) indica che i laureati residenti nel Sud percepiscono in media 151 euro in meno rispetto ai residenti del Nord, con una differenza anch’essa altamente significativa (p < 0.001).

#La significatività statistica di entrambi i coefficienti è confermata dai p-value estremamente bassi (<<0.001), che indicano che le differenze osservate sono molto difficilmente dovute al caso.

#Tuttavia, l’R-quadro del modello è pari a 0.012 (1,2%), a indicare che la sola ripartizione geografica spiega una quota molto limitata della variabilità del reddito. Ciò suggerisce che, pur essendo significativa, la localizzazione geografica da sola non basta a spiegare le disuguaglianze reddituali osservate nel campione.



# c) Laurea fuori corso (indep.) e reddito numerico (dep.)
# Controllo modalità presenti
unique(data$Q1_19)
# ( 1 = fuori corso, 2 = in corso)

# Creazione dummy: 1 = laurea fuori corso, 0 = laurea in corso
data$fuori_corso <- ifelse(data$Q1_19 == 1, 1, 0)

# Controllo correttezza
table(data$fuori_corso, data$Q1_19)

# Regressione lineare: effetto di laurea fuori corso sul reddito percepito
model_fuori_corso <- lm(redd_per ~ fuori_corso, data = data)
summary(model_fuori_corso)

#Il modello mostra che laurearsi fuori corso ha un effetto negativo e statisticamente significativo sul reddito mensile percepito: chi si laurea fuori corso guadagna in media circa 47 euro in meno rispetto a chi si laurea in corso (p < 0.001). Tuttavia, il valore dell’R-quadro è molto basso (0.0015), indicando che la variabile “fuori corso” spiega solo una minima parte della variabilità del reddito. In sintesi: l’effetto è reale e significativo, ma limitato dal punto di vista sostantivo.

# d) Classe sociale (indep.) e laurea fuori corso (dep.)
cont_table2 <- table(data$classe_origine, data$Q1_19)
prop.table(cont_table2, 1)*100
chisq.test(cont_table2)
assocstats(cont_table2)

# La probabilità di laurearsi fuori corso varia in modo statisticamente significativo in base alla classe sociale di origine (p < 0.001). Tuttavia, l’intensità dell’associazione è molto bassa (V di Cramer = 0.07): la classe sociale influisce solo marginalmente sulla probabilità di essere fuori corso. In generale, tra i figli della borghesia la quota di fuori corso è leggermente superiore rispetto agli altri gruppi, ma le differenze non sono sostanziali.

# ### Esercizio 3 ###

# 1) Regressione: effetto della classe sociale sul reddito
model1 <- lm(redd_per ~ classe_media + classe_operaia, data = data)
summary(model1)
# Interpretazione:Il modello indica che, rispetto ai laureati provenienti dalla borghesia (categoria di riferimento), quelli di classe media percepiscono in media 52 euro in meno e quelli di classe operaia 59 euro in meno al mese. Entrambi gli effetti sono statisticamente significativi (p < 0.001). Tuttavia, l’R-quadro è molto basso (0.002), il che significa che la classe sociale di origine spiega solo una quota trascurabile della variabilità del reddito mensile. L’effetto è reale ma di portata limitata.


# 2) Modello con controllo per ripartizione geografica
model2 <- lm(redd_per ~ classe_media + classe_operaia + ripgeo_2 + ripgeo_3, data = data)
summary(model2)
# Interpretazione:Anche controllando per la ripartizione geografica, la classe sociale di origine mantiene un effetto negativo e statisticamente significativo sul reddito percepito: rispetto alla borghesia, i laureati di classe media guadagnano in media 45 euro in meno, mentre quelli di classe operaia circa 55 euro in meno. Inoltre, il reddito medio diminuisce di 52 euro (Centro) e 149 euro (Sud) rispetto al Nord. Tutti i coefficienti sono altamente significativi (p < 0.001). L’R-quadro sale a circa 0.013, segnalando che il modello spiega ancora solo una piccola parte della variabilità del reddito, ma la bontà del modello migliora leggermente rispetto alla regressione precedente.


# 3) Modello con controllo per ripartizione geografica e laurea fuori corso
model3 <- lm(redd_per ~ classe_media + classe_operaia + + ripgeo_2 + ripgeo_3 + fuori_corso, data = data)
summary(model3)
#Nel modello che controlla contemporaneamente per classe sociale di origine, ripartizione geografica e laurea fuori corso, tutti i coefficienti risultano ancora negativi e statisticamente significativi (p < 0.001). In particolare, rispetto ai laureati della borghesia, quelli di classe media guadagnano 44 euro in meno e quelli di classe operaia 52 euro in meno. I residenti al Centro percepiscono in media 49 euro in meno e quelli al Sud 145 euro in meno rispetto al Nord. Inoltre, chi si laurea fuori corso guadagna 33 euro in meno rispetto a chi si laurea in corso. L’R-quadro sale leggermente a 0.014, confermando che tutte queste variabili spiegano solo una piccola parte della variabilità del reddito, anche se il modello nel complesso è altamente significativo.