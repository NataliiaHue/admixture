# function plot structure
library(reshape) # for melt()
library(ggplot2)
library(maps)
library(mapdata)
library(maptools)
library(patchwork) 

# Read in the data
setwd("/Users/neshcheret/Documents/GitHub/articles/tree-subsets")

phonology_matrix_K3 <- read.csv("phonology_matrix_K3.txt", sep="\t")
word_matrix_K3 <- read.csv("word_matrix_K3.txt", sep="\t")
clause_matrix_K3 <- read.csv("clause_matrix_K3.txt", sep="\t")

phonology_matrix_K4 <- read.csv("phonology_matrix_K4.txt", sep="\t")
word_matrix_K4 <- read.csv("word_matrix_K4.txt", sep="\t")
clause_matrix_K4 <- read.csv("clause_matrix_K4.txt", sep="\t")

phonology_matrix_K5 <- read.csv("phonology_matrix_K5.txt", sep="\t")
word_matrix_K5 <- read.csv("word_matrix_K5.txt", sep="\t")
clause_matrix_K5 <- read.csv("clause_matrix_K5.txt", sep="\t")


# STRUCTURE returns cut versions of some languages - rename those
rename_languages <- function(structure_data_frame){
  structure_data_frame$Language[structure_data_frame$Language=="MiddleMongo"] <- "MiddleMongol"
  structure_data_frame$Language[structure_data_frame$Language=="MiddleKorea"] <- "MiddleKorean"
  structure_data_frame$Language[structure_data_frame$Language=="BeryozovkaE"] <- "BeryozovkaEven"
  structure_data_frame$Language[structure_data_frame$Language=="CrimeanTata"] <- "CrimeanTatar"
  structure_data_frame$Language[structure_data_frame$Language=="KhamniganMo"] <- "KhamniganMongol"
  structure_data_frame$Language[structure_data_frame$Language=="KhamnigalMo"] <- "KhamniganMongol"
  structure_data_frame$Language[structure_data_frame$Language=="NorthernUzb"] <- "NorthernUzbek"
  structure_data_frame$Language[structure_data_frame$Language=="EasternOldJ"] <- "EasternOldJapanese"
  return(structure_data_frame)
}

phonology_K3_renamed <- rename_languages(phonology_matrix_K3)
word_K3_renamed <- rename_languages(word_matrix_K3)
clause_K3_renamed <- rename_languages(clause_matrix_K3)

phonology_K4_renamed <- rename_languages(phonology_matrix_K4)
word_K4_renamed <- rename_languages(word_matrix_K4)
clause_K4_renamed <- rename_languages(clause_matrix_K4)

phonology_K5_renamed <- rename_languages(phonology_matrix_K5)
word_K5_renamed <- rename_languages(word_matrix_K5)
clause_K5_renamed <- rename_languages(clause_matrix_K5)

# reshape the data for plotting with ggplot later
phonology_K3_renamed_melt <- melt(phonology_K3_renamed, id=c("Language"))
word_K3_renamed_melt <- melt(word_K3_renamed, id=c("Language"))
clause_K3_renamed_melt <- melt(clause_K3_renamed, id=c("Language"))

phonology_K4_renamed_melt <- melt(phonology_K4_renamed, id=c("Language"))
word_K4_renamed_melt <- melt(word_K4_renamed, id=c("Language"))
clause_K4_renamed_melt <- melt(clause_K4_renamed, id=c("Language"))

phonology_K5_renamed_melt <- melt(phonology_K5_renamed, id=c("Language"))
word_K5_renamed_melt <- melt(word_K5_renamed, id=c("Language"))
clause_K5_renamed_melt <- melt(clause_K5_renamed, id=c("Language"))

# The order of the languages to appear in the structure plot
languages_ordered <- c("Ura","Okinoerabu","Yuwan","Tsuken","Shuri","Hateruma","Yonaguni","Ikema","Ogami","Tarama","Japanese","EasternOldJapanese","MiddleKorean","Korean","Manchu","Oroch","Udihe","Nanai","Orok","Ulch","BeryozovkaEven","MomaEven","Solon","Negidal","Evenki","MiddleMongol","Moghol","Dagur","KhamniganMongol","Khalkha","Oirat","Ordos","Buriat","Kalmyk","Bonan","Dongxiang","ShiraYughur","Mongghul","Mangghuer","OldTurkic","Chuvash","Khalaj","Khakas","Shor","Tuvan","Bashkir","Tatar","KaraKalpak","Kazakh","Nogai","CrimeanTatar","Dolgan","Yakut","Gagauz","Azerbaijani","Turkmen","Turkish","Chagatai","NorthernUzbek","Uighur")

# Set default theme
theme_set(theme_classic())

# Set the default path for saving plots
setwd("/Users/neshcheret/Documents/GitHub/articles/tree-subsets")

# Plot K3

phonology_K3_structure_plt <- ggplot(phonology_K3_renamed_melt, aes(x=factor(Language, level=languages_ordered),y=value,fill=variable)) +
  geom_col() +
  theme(axis.ticks.y = element_blank(), axis.line =element_blank()) +
  labs(title = "Phonology", x = element_blank(),y = element_blank() ) +
  scale_fill_manual(name = element_blank(), labels = c('TkM','TgMK','J'),values=c('#FC8D62','#66C2A5','#E78AC3')) + 
  coord_flip() +
  theme(legend.position="bottom")
phonology_K3_structure_plt

morphology_K3_structure_plt <- ggplot(word_K3_renamed_melt, aes(x=factor(Language, level=languages_ordered),y=value,fill=variable)) +
  geom_col() + 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line =element_blank()) +
  labs(title = "Morphology", x = element_blank(), y = element_blank() ) +
  scale_fill_manual(name = element_blank(), labels = c('TgM','Tk','JK'),values=c('#66C2A5','#FC8D62','#E78AC3')) + 
  coord_flip() +
  theme(legend.position="bottom")
morphology_K3_structure_plt

syntax_K3_structure_plt <- ggplot(clause_K3_renamed_melt, aes(x=factor(Language, level=languages_ordered),y=value,fill=variable)) +
  geom_col() + 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line =element_blank()) +
  labs(title = "Syntax", x = element_blank(),y = element_blank()) +
  scale_fill_manual(name = element_blank(), labels = c('Tk','JK','TgM'),values=c('#FC8D62','#E78AC3','#66C2A5')) + 
  coord_flip() +
  theme(legend.position="bottom")
syntax_K3_structure_plt

ggsave("population-structure-k3.pdf", (phonology_K3_structure_plt | morphology_K3_structure_plt | syntax_K3_structure_plt) ,height=10,width=10)

# Plot K4

phonology_K4_structure_plt <- ggplot(phonology_K4_renamed_melt, aes(x=factor(Language, level=languages_ordered),y=value,fill=variable)) +
  geom_col() +
  theme(axis.ticks.y = element_blank(), axis.line =element_blank()) +
  labs(title = "Phonology", x = element_blank(),y = element_blank() ) +
  scale_fill_manual(name = element_blank(), labels = c('J','MK','Tk','Tg'),values=c('#E78AC3','#8DA0CB','#FC8D62','#66C2A5')) + 
  coord_flip() +
  theme(legend.position="bottom")
phonology_K4_structure_plt

morphology_K4_structure_plt <- ggplot(word_K4_renamed_melt, aes(x=factor(Language, level=languages_ordered),y=value,fill=variable)) +
  geom_col() + 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line =element_blank()) +
  labs(title = "Morphology", x = element_blank(), y = element_blank() ) +
  scale_fill_manual(name = element_blank(), labels = c('M','JK','Tk','Tg'),values=c('#8DA0CB','#E78AC3','#FC8D62','#66C2A5')) + 
  coord_flip() +
  theme(legend.position="bottom")
morphology_K4_structure_plt

syntax_K4_structure_plt <- ggplot(clause_K4_renamed_melt, aes(x=factor(Language, level=languages_ordered),y=value,fill=variable)) +
  geom_col() + 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line =element_blank()) +
  labs(title = "Syntax", x = element_blank(),y = element_blank()) +
  scale_fill_manual(name = element_blank(), labels = c('JK','Tg','Tk','M'),values=c('#E78AC3','#66C2A5','#FC8D62','#8DA0CB')) + 
  coord_flip() +
  theme(legend.position="bottom")
syntax_K4_structure_plt

ggsave("population-structure-k4.pdf", (phonology_K4_structure_plt | morphology_K4_structure_plt | syntax_K4_structure_plt) ,height=10,width=10)

# Plot K5

phonology_K5_structure_plt <- ggplot(phonology_K5_renamed_melt, aes(x=factor(Language, level=languages_ordered),y=value,fill=variable)) +
  geom_col() + 
  theme(axis.ticks.y = element_blank(), axis.line = element_blank()) +
  labs(title = "Phonology", x = element_blank(),y = element_blank()) + 
  scale_fill_manual(name = element_blank(), labels = c('A1','J','A2','Tk','Tg'),values=c('#8DA0CB','#E78AC3','#A6D854','#FC8D62','#66C2A5')) +
  coord_flip() +
  theme(legend.position="bottom")
phonology_K5_structure_plt

morphology_K5_structure_plt <- ggplot(word_K5_renamed_melt, aes(x=factor(Language, level=languages_ordered),y=value,fill=variable)) +
  geom_col() + 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line =element_blank()) +
  labs(title = "Morphology", x = element_blank(),y = element_blank()) + 
  scale_fill_manual(name = element_blank(), labels = c('Tk','JK','Tg','A','M'),values=c('#FC8D62','#E78AC3','#66C2A5','#A6D854','#8DA0CB')) +
  coord_flip() +
  theme(legend.position="bottom")
morphology_K5_structure_plt

syntax_K5_structure_plt <- ggplot(clause_K5_renamed_melt, aes(x=factor(Language, level=languages_ordered),y=value,fill=variable)) +
  geom_col() + 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line =element_blank()) +
  labs(title = "Syntax", x = element_blank(),y = element_blank()) +
  scale_fill_manual(name = element_blank(), labels = c('JK','A1','Tk','Tg','A2'),values=c('#E78AC3','#8DA0CB','#FC8D62','#66C2A5','#A6D854')) +
  coord_flip() +
  theme(legend.position="bottom")
syntax_K5_structure_plt

ggsave("population-structure-k5.pdf", (phonology_K5_structure_plt | morphology_K5_structure_plt | syntax_K5_structure_plt) ,height=10,width=10)
