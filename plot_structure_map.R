# Plot the results on the map
# ggmap(corvallis_map, base_layer = ggplot(sales, aes(lon,lat)))+
# geom_point()
library(reshape)
library(tidyverse)
library(mapdata)
library(scatterpie)

getwd()
setwd("/Users/neshcheret/Documents/GitHub/articles/tree-subsets")

# Prepare the data

languages_geo <- read.csv("languages_geo.csv")

rename_languages_geo <- function(data_frame){
  data_frame$Name[data_frame$Name=="Beryozovka Even"] <- "BeryozovkaEven"
  data_frame$Name[data_frame$Name=="Crimean Tatar"] <- "CrimeanTatar"
  data_frame$Name[data_frame$Name=="Eastern Old Japanese"] <- "EasternOldJapanese"
  data_frame$Name[data_frame$Name=="Karakalpak"] <- "KaraKalpak"
  data_frame$Name[data_frame$Name=="Khamnigan Mongol"] <- "KhamniganMongol"
  data_frame$Name[data_frame$Name=="Middle Korean"] <- "MiddleKorean"
  data_frame$Name[data_frame$Name=="Middle Mongol"] <- "MiddleMongol"
  data_frame$Name[data_frame$Name=="Moma Even"] <- "MomaEven"
  data_frame$Name[data_frame$Name=="Northern Uzbek"] <- "NorthernUzbek"
  data_frame$Name[data_frame$Name=="Old Turkic"] <- "OldTurkic"
  data_frame$Name[data_frame$Name=="Shira Yughur"] <- "ShiraYughur"
  data_frame$Name[data_frame$Name=="Altai Oirat" ] <- "Oirat"
  data_frame$Name[data_frame$Name=="Hateruma Yaeyama"] <- "Hateruma"
  data_frame$Name[data_frame$Name=="Minhe Mangghuer (Tu)"] <- "Mangghuer"
  data_frame$Name[data_frame$Name=="Shuri (Central Okinawan)"] <- "Shuri"
  data_frame$Name[data_frame$Name=="Tsuken (Central Okinawan)"] <- "Tsuken"
  data_frame$Name[data_frame$Name=="Shira Yughur"] <- "ShiraYughur"
  data_frame$Name[data_frame$Name=="Yonaguni (Dunan)"] <- "Yonaguni"
  data_frame$Name[data_frame$Name=="Chuvash (Southern dialect)"] <- "Chuvash"
  data_frame$Name[data_frame$Name=="Ikema (Miyako)"] <- "Ikema"
  data_frame$Name[data_frame$Name=="Tarama Miyako"] <- "Tarama"
  data_frame$Name[data_frame$Name=="Ogami (Miyako)"] <- "Ogami"
  data_frame$Name[data_frame$Name=="Ura (Northern Amami-Oshima)"] <- "Ura"
  data_frame$Name[data_frame$Name=="Yuwan (Southern Amami-Oshima)"] <- "Yuwan"
  data_frame$Name[data_frame$Name=="Huzhu Mongghul"] <- "Mongghul"
  return(data_frame)
}

languages_geo_renamed <- rename_languages_geo(languages_geo)

# read in structure results for each subset

phonology_matrix_K4 <- read.csv("phonology_matrix_K4.txt", sep="\t")

word_matrix_K4 <- read.csv("word_matrix_K4.txt", sep="\t")

clause_matrix_K4 <- read.csv("clause_matrix_K4.txt", sep="\t")

# STRUCTURE returns cut versions of some languages - rename those
rename_languages_structure <- function(structure_data_frame){
  structure_data_frame$Language[structure_data_frame$Language=="MiddleMongo"] <- "MiddleMongol"
  structure_data_frame$Language[structure_data_frame$Language=="MiddleKorea"] <- "MiddleKorean"
  structure_data_frame$Language[structure_data_frame$Language=="BeryozovkaE"] <- "BeryozovkaEven"
  structure_data_frame$Language[structure_data_frame$Language=="CrimeanTata"] <- "CrimeanTatar"
  structure_data_frame$Language[structure_data_frame$Language=="KhamnigalMo"] <- "KhamniganMongol"
  structure_data_frame$Language[structure_data_frame$Language=="NorthernUzb"] <- "NorthernUzbek"
  structure_data_frame$Language[structure_data_frame$Language=="EasternOldJ"] <- "EasternOldJapanese"
  return(structure_data_frame)
}

phonology_K4_renamed <- rename_languages(phonology_matrix_K4)
word_K4_renamed <- rename_languages(word_matrix_K4)
clause_K4_renamed <- rename_languages(clause_matrix_K4)

# reshape the data for plotting with ggplot later
phonology_K4_renamed_melt <- melt(phonology_K4_renamed, id=c("Language"))
word_K4_renamed_melt <- melt(word_K4_renamed, id=c("Language"))
clause_K4_renamed_melt <- melt(clause_K4_renamed, id=c("Language"))

#check that language names in geo correspond to language names in structure results before joining the data frames
setdiff(languages_geo_renamed$Name,unique(phonology_K4_renamed_melt$Language))

phonology_K4_renamed_joined <- phonology_K4_renamed %>%
  inner_join(languages_geo_renamed, by = c("Language" = "Name"))

phonology_K4_renamed_joined_melt <- melt(phonology_K4_renamed_joined, id = c("Language", "Glottocode", "Latitude", "Longitude"))

# Prepare the map

par(mar=c(0,0,0,0))
long_lim <- c(30, 170)
lat_lim  <- c(20, 90)
data(world2MapEnv)

eurasia_map <- map_data('world', xlim=long_lim, ylim=lat_lim)

# Set the overall plot theme
theme_set(theme_classic())

# Save the empty map: eurasia_plt
eurasia_plt <- ggplot(eurasia_map) +
  geom_map(map = eurasia_map, aes(long, lat, map_id=region), col = "white", fill = "gray60")
eurasia_plt

# distribution of languages
plot_languages <- eurasia_plt + 
  geom_text(data=languages_geo_renamed,aes(Longitude,Latitude, label = Name))
plot_languages



# with text - not functioning yet
eurasia_plt +
  geom_point(data = phonology_K4_renamed_joined, aes(Longitude, Latitude)) +
  annotate(aes(label = phonology_K4_renamed_joined$Glottocode, Longitude, Latitude), check_overlap = TRUE)

# scatterplot with ancestry proportions
eurasia_plt +
  geom_scatterpie(aes(x=Longitude, y=Latitude), 
                data = phonology_K4_renamed_joined, cols = c("Pop1", "Pop2", "Pop3", "Pop4"), alpha = 0.7) +
  labs(title = "Phonology", x = "Longitude", y = "Latitude" ) +
  scale_fill_manual(name = "Ancestry:",labels = c('Turkic','Tungusic','Mongolo-Koreanic','Japonic'),values=c('#FC8D62','#66C2A5','#8DA0CB','#E78AC3'))













