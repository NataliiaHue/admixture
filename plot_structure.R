setwd("/Users/neshcheret/Documents/GitHub/articles/tree-subsets")
library(RColorBrewer)
library(dplyr)

languages_ordered <- c("Ura","Okinoerabu","Yuwan","Tsuken","Shuri","Hateruma","Yonaguni","Ikema","Ogami","Tarama","Japanese","EasternOldJapanese","MiddleKorean","Korean","Manchu","Oroch","Udihe","Nanai","Orok","Ulch","BeryozovkaEven","MomaEven","Solon","Negidal","Evenki","MiddleMongol","Moghol","Dagur","KhamniganMongol","Khalkha","Oirat","Ordos","Buriat","Kalmyk","Bonan","Dongxiang","ShiraYughur","Mongghul","Mangghuer","OldTurkic","Chuvash","Khalaj","Khakas","Shor","Tuvan","Bashkir","Tatar","KaraKalpak","Kazakh","Nogai","CrimeanTatar","Dolgan","Yakut","Gagauz","Azerbaijani","Turkmen","Turkish","Chagatai","NorthernUzbek","Uighur")

phonology_matrix_K4 <- read.csv("phonology_matrix_K4.txt", sep="\t")
word_matrix_K4 <- read.csv("word_matrix_K4.txt", sep="\t")
clause_matrix_K5 <- read.csv("clause_matrix_K5.txt", sep="\t")

structure_data_frame <- phonology_matrix_K4
rownames(structure_data_frame) <- structure_data_frame$Language
structure_data_frame <- structure_data_frame[,-1] # delete the column with languages - language names are now in the rownames

rownames(structure_data_frame)[rownames(structure_data_frame)=="MiddleMongo"] <- "MiddleMongol"
rownames(structure_data_frame)[rownames(structure_data_frame)=="MiddleKorea"] <- "MiddleKorean"
rownames(structure_data_frame)[rownames(structure_data_frame)=="BeryozovkaE"] <- "BeryozovkaEven"
rownames(structure_data_frame)[rownames(structure_data_frame)=="CrimeanTata"] <- "CrimeanTatar"
rownames(structure_data_frame)[rownames(structure_data_frame)=="KhamnigalMo"] <- "KhamniganMongol"
rownames(structure_data_frame)[rownames(structure_data_frame)=="NorthernUzb"] <- "NorthernUzbek"
rownames(structure_data_frame)[rownames(structure_data_frame)=="EasternOldJ"] <- "EasternOldJapanese"

ordered_structure_data_frame <- structure_data_frame %>%
  slice(match(languages_ordered, rownames(structure_data_frame)))

#setdiff(languages_ordered, rownames(ordered_structure_data_frame))

#par(mar = c(bottom, left, top, right))
par(mar = c(9, 3, 3, 1))
barplot(t(as.matrix(ordered_structure_data_frame)),col=rainbow(5),las=2,cex.names=1,main="Morphology")


#structure_plot(clause_structure_outfile_K3, languages_ordered)

library(mapdata)
library(ggplot2)
library(scatterpie)
worldmap <- map_data ("world")
languages <- read.csv("languages_map.csv",sep=";")
mapplot1 <- ggplot(worldmap) + 
  geom_map(data = worldmap, map = worldmap, aes(x=long, y=lat, map_id=region), col = "white", fill = "gray50") +
  geom_scatterpie(aes(x=longitude, y=latitude), data=structure_data_frame, cols=c("Pop1", "Pop2", "Pop3","Pop4")) +
  coord_sf(xlim = c(0,180), ylim = c(10,80))

mapplot1

mapplot2 <- ggplot(worldmap) +
  coord_sf(xlim = c(0,180), ylim = c(10,80)) +
  geom_text(aes(x=structure_data_frame$longitude,y=structure_data_frame$latitude,label=rownames(structure_data_frame)),check_overlap = TRUE)
mapplot2

geom_scatterpie(data=languages,aes(x=languages$longitude, y=languages$latitude, group = family, r = multiplier*6), data = structure_data_frame, cols = colnames(structure_data_frame))

structure_data_frame$longitude <- languages$longitude
structure_data_frame$latitude <- languages$latitude
ggplot() + geom_scatterpie(aes(x=latitude, y=longitude), data=structure_data_frame, cols=c("Pop1", "Pop2", "Pop3","Pop4"))


library(ggplot2)
d <- data.frame(x=rnorm(5), y=rnorm(5))
d$A <- abs(rnorm(5, sd=1))
d$B <- abs(rnorm(5, sd=2))
d$C <- abs(rnorm(5, sd=3))
ggplot() + geom_scatterpie(aes(x=x, y=y), data=d, cols=c("A", "B", "C")) + coord_fixed()
d <- tidyr::gather(d, key="letters", value="value", -x:-y)
ggplot() + geom_scatterpie(aes(x=x, y=y), data=d, cols="letters", long_format=TRUE) + coord_fixed()
# }