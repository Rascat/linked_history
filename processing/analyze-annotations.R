# Text Mining & Analysis Coding DaVinci Ost 2018: Linked History ----
meta <- read.csv2("Staatsarchiv_Leipzig_Metadaten_BV-Firmenakten.csv", sep = ",", encoding = "UTF-8")
head(meta)
tail(meta)
summary(meta)

# Ordering by Signatur, deleting as index and creating new one ----
meta <- meta[order(meta$Archivaliensignatur), ]
library(data.table)
meta <- data.table(meta)
meta <- data.frame(meta)

# Formatting time ----
meta[,2:3] <- as.character(c(meta$Datierung.von.Zahl., meta$Datierung.bis.Zahl.))
meta$Datierung.von.Zahl. <- as.Date(meta$Datierung.von.Zahl., format = "%Y%m%d")
meta$Datierung.bis.Zahl. <- as.Date(meta$Datierung.bis.Zahl., format = "%Y%m%d")

# Changing col names ----
names(meta) <- c("sig", "from", "to", "from_to", "class", "archive", "note", "pages", "sort")

# Changing number of pages to integer ----
meta$pages <- as.integer(sub(" B.*", "", meta$pages))

# Transforming empty "notes" to NA ----
meta$note[meta$note %in% ""] <- NA

# Analysis ----
summary(meta)
# Identifying keywords around jewish people, prosecutions, emigration and WWII
library(stringr)
meta$note[str_detect(meta$note, "Jude") %in% TRUE] # 7
meta$note[str_detect(meta$note, "jude") %in% TRUE] # 1
meta$note[str_detect(meta$note, "jüd") %in% TRUE] # 181 - auch zugelassene jüdische
meta$note[str_detect(meta$note, "Pogrom | pogrom") %in% TRUE] # 0
meta$note[str_detect(meta$note, "Kristall | kristall") %in% TRUE] # 0
meta$note[str_detect(meta$note, "Verfolg | verfolg") %in% TRUE] # 1
meta$note[str_detect(meta$note, "Verfolgung") %in% TRUE] # 1
meta$note[str_detect(meta$note, "nichtari") %in% TRUE] # 200
meta$note[str_detect(meta$note, "Arisierung") %in% TRUE] # 67
meta$note[str_detect(meta$note, "Republikflucht") %in% TRUE] # 1
meta$note[str_detect(meta$note, "Flucht | flucht") %in% TRUE] # 2
meta$note[str_detect(meta$note, "Vertreibung") %in% TRUE] # 1
meta$note[str_detect(meta$note, "Feind|feindl") %in% TRUE] # 61
meta$note[str_detect(meta$note, "Auswanderung") %in% TRUE] # 3
meta$note[str_detect(meta$note, "Emigration") %in% TRUE] # 13
meta$note[str_detect(meta$note, "Zwarte") %in% TRUE] # 1
meta$note[str_detect(meta$note, "links | Links") %in% TRUE] # 0
meta$note[str_detect(meta$note, "zerstör | Zerstör") %in% TRUE] # 5
meta$note[str_detect(meta$note, "krieg | Krieg") %in% TRUE] # 9
meta$note[str_detect(meta$note, "Angriff") %in% TRUE] # 5
meta$note[str_detect(meta$note, "Beschlagnahme | beschlagnahmt") %in% TRUE] # 2
meta$note[str_detect(meta$note, "Tod | töt") %in% TRUE] # 4

# Building new data frame with keywords
keywords <- c("Jude", "jude", "jüd", "Verfolg", "verfolg", "Verfolgung", "nichtari",
              "Arisierung", "Republikflucht", "Feind", "Auswanderung", "Emigration",
              "Zwarte", "zerstör", "Zerstör", "krieg", "Krieg", "Angriff", "Flucht",
              "flucht", "Vertreibung", "Beschlagnahme")
nmeta <- meta[str_detect(meta$note, paste0(keywords, collapse = "|")) %in% TRUE,] # 512
nmeta <- droplevels(nmeta)
summary(nmeta)

# Adding categories as binary variables
# Arisierung
nmeta["Arisierung"] <- NA
nmeta$Arisierung <- ifelse(str_detect(nmeta$note, "Arisierung") %in% TRUE, 1, 0)
summary(nmeta$Arisierung)

# jüdisch
nmeta["jüdisch"] <- NA
nmeta$jüdisch <- ifelse(str_detect(nmeta$note, "nichtarisch|jüdi|Jüdi|jude|Jude") %in% TRUE, 1, 0)
summary(nmeta$jüdisch)

# genehmigt
nmeta["genehmigt"] <- NA
nmeta$genehmigt <- ifelse(str_detect(nmeta$note, "Liste des Reichsministeriums für Volksaufklärung und Propaganda")
                          %in% TRUE, 1, 0)
summary(nmeta$genehmigt)

# Feind
nmeta["Feind"] <- NA
nmeta$Feind <- ifelse(str_detect(nmeta$note, "Feind|feindl") %in% TRUE, 1, 0)
summary(nmeta$Feind)

# Emigration
nmeta["Emigration"] <- NA
nmeta$Emigration <- ifelse(str_detect(nmeta$note,
                                      "Emigration|Auswanderung|Emigrant|Auswander|auswander|emigrant")
                           %in% TRUE, 1, 0)
summary(nmeta$Emigration)

# Flucht
nmeta["Flucht"] <- NA
nmeta$Flucht <- ifelse(str_detect(nmeta$note, "Flucht|Flücht|flücht|flucht|Republikflucht") %in% TRUE, 1, 0)
summary(nmeta$Flucht)

# zerstört
nmeta["zerstört"] <- NA
nmeta$zerstört <- ifelse(str_detect(nmeta$note, "Zerstör|zerstör") %in% TRUE, 1, 0)
summary(nmeta$zerstört)

# Vertreibung
nmeta["Vertreibung"] <- NA
nmeta$Vertreibung <- ifelse(str_detect(nmeta$note, "Vertreibung|Vertreib|vertreib")
                            %in% TRUE, 1, 0)
summary(nmeta$Vertreibung)

# Beschlagnahmung
nmeta["Beschlagnahmung"] <- NA
nmeta$Beschlagnahmung <- ifelse(str_detect(nmeta$note, "Beschlagnahme|beschlagnahm")
                            %in% TRUE, 1, 0)
summary(nmeta$Beschlagnahmung)

# Visualizing results ----
results <- as.data.frame(lapply(nmeta[,10:18], FUN = "sum"))
results <- t(results)
results <- results[order(results[1:9,1], decreasing = TRUE),]
barplot(results, main = "Frequency of Events Occurred", xlab = "Categories", ylab = "Frequency")

# Exporting as new CSV ----
library(readr)
write_excel_csv(nmeta, "linked-history-analysis.csv")
