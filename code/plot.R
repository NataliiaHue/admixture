# function plot structure
library(ggplot2)
library(maps)
library(mapdata)
library(maptools)
library(patchwork) 
library(readr)
library(tidyr)

# Read in the data
setwd("./GitHub/admixture")

phonology_K4 <- read.csv("phonology_K4.txt", sep="\t")
morphology_K4 <- read.csv("morphology_K4.txt", sep="\t")
syntax_K4 <- read.csv("syntax_K4.txt", sep="\t")

# The order of the languages to appear in the structure plot
languages_ordered <- c("Ura","Okinoerabu","Yuwan","Tsuken","Shuri","Hateruma",
                       "Yonaguni","Ikema","Ogami","Tarama","Japanese",
                       "EasternOldJapanese","MiddleKorean","Korean","Manchu",
                       "Oroch","Udihe","Nanai","Orok","Ulch","BeryozovkaEven",
                       "MomaEven","Solon","Negidal","Evenki","MiddleMongol",
                       "Moghol","Dagur","KhamniganMongol","Khalkha","Oirat",
                       "Ordos","Buriat","Kalmyk","Bonan","Dongxiang",
                       "ShiraYughur","Mongghul","Mangghuer","OldTurkic",
                       "Chuvash","Khalaj","Khakas","Shor","Tuvan","Bashkir",
                       "Tatar","KaraKalpak","Kazakh","Nogai","CrimeanTatar",
                       "Dolgan","Yakut","Gagauz","Azerbaijani","Turkmen",
                       "Turkish","Chagatai","NorthernUzbek","Uighur")

# load the file with language names and glottocodes
languages <- read_tsv("data/languages_map.tsv")

# replace glottocodes with full language names in the admixture file
rename_gather <- function(dataset){
  dataset %>%
    left_join(languages, by = c("Language" = "glottocode")) %>%
    select(Language = full_name_no_spaces, starts_with("Pop")) %>%
    gather(variable, value, -Language)
}

# labs(title = "Phonology")

plot_admixture <- function(dataset){
  ggplot(dataset, aes(x=factor(Language, level=languages_ordered),y=value,fill=variable)) +
    geom_col() +
    theme(axis.ticks.y = element_blank(), axis.line = element_blank(), legend.position = "none", axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    labs(x = element_blank(),y = element_blank() ) +
    coord_flip()
}

# Set default theme
theme_set(theme_classic())

# Plotting example
morphology_K7_full_names <- rename_gather(morphology_K7)
plot_admixture(morphology_K7_full_names) + theme(axis.text.y = element_blank())

# Plot K3

phonology_K3_structure_plt <- ggplot(phonology_K3_renamed_melt, aes(x=factor(Language, level=languages_ordered),y=value,fill=variable)) +
  geom_col() +
  theme(axis.ticks.y = element_blank(), axis.line =element_blank()) +
  labs(title = "Phonology", x = element_blank(),y = element_blank() ) +
  scale_fill_manual(name = element_blank(), labels = c('TkM','TgMK','J'),values=c('#FC8D62','#66C2A5','#E78AC3')) + 
  coord_flip() +
  theme(legend.position="bottom")
phonology_K3_structure_plt

morphology_K3_structure_plt <- ggplot(morphology_K3_renamed_melt, aes(x=factor(Language, level=languages_ordered),y=value,fill=variable)) +
  geom_col() + 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line =element_blank()) +
  labs(title = "Morphology", x = element_blank(), y = element_blank() ) +
  scale_fill_manual(name = element_blank(), labels = c('TgM','Tk','JK'),values=c('#66C2A5','#FC8D62','#E78AC3')) + 
  coord_flip() +
  theme(legend.position="bottom")
morphology_K3_structure_plt

syntax_K3_structure_plt <- ggplot(syntax_K3_renamed_melt, aes(x=factor(Language, level=languages_ordered),y=value,fill=variable)) +
  geom_col() + 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line =element_blank()) +
  labs(title = "Syntax", x = element_blank(),y = element_blank()) +
  scale_fill_manual(name = element_blank(), labels = c('Tk','JK','TgM'),values=c('#FC8D62','#E78AC3','#66C2A5')) + 
  coord_flip() +
  theme(legend.position="bottom")
syntax_K3_structure_plt

ggsave("plots/population-structure-k3.pdf", (phonology_K3_structure_plt | morphology_K3_structure_plt | syntax_K3_structure_plt) ,height=10,width=10)

# Plot K4

phonology_K4_structure_plt <- ggplot(phonology_K4_renamed_melt, aes(x=factor(Language, level=languages_ordered),y=value,fill=variable)) +
  geom_col() +
  theme(axis.ticks.y = element_blank(), axis.line =element_blank()) +
  labs(title = "Phonology", x = element_blank(),y = element_blank() ) +
  scale_fill_manual(name = element_blank(), labels = c('J','MK','Tk','Tg'),values=c('#E78AC3','#8DA0CB','#FC8D62','#66C2A5')) + 
  coord_flip() +
  theme(legend.position="bottom")
phonology_K4_structure_plt

morphology_K4_structure_plt <- ggplot(morphology_K4_renamed_melt, aes(x=factor(Language, level=languages_ordered),y=value,fill=variable)) +
  geom_col() + 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line =element_blank()) +
  labs(title = "Morphology", x = element_blank(), y = element_blank() ) +
  scale_fill_manual(name = element_blank(), labels = c('M','JK','Tk','Tg'),values=c('#8DA0CB','#E78AC3','#FC8D62','#66C2A5')) + 
  coord_flip() +
  theme(legend.position="bottom")
morphology_K4_structure_plt

syntax_K4_structure_plt <- ggplot(syntax_K4_renamed_melt, aes(x=factor(Language, level=languages_ordered),y=value,fill=variable)) +
  geom_col() + 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line =element_blank()) +
  labs(title = "Syntax", x = element_blank(),y = element_blank()) +
  scale_fill_manual(name = element_blank(), labels = c('JK','Tg','Tk','M'),values=c('#E78AC3','#66C2A5','#FC8D62','#8DA0CB')) + 
  coord_flip() +
  theme(legend.position="bottom")
syntax_K4_structure_plt

ggsave("plots/population-structure-k4.pdf", (phonology_K4_structure_plt | morphology_K4_structure_plt | syntax_K4_structure_plt) ,height=10,width=10)

# Plot K5

phonology_K5_structure_plt <- ggplot(phonology_K5_renamed_melt, aes(x=factor(Language, level=languages_ordered),y=value,fill=variable)) +
  geom_col() + 
  theme(axis.ticks.y = element_blank(), axis.line = element_blank()) +
  labs(title = "Phonology", x = element_blank(),y = element_blank()) + 
  scale_fill_manual(name = element_blank(), labels = c('A1','J','A2','Tk','Tg'),values=c('#8DA0CB','#E78AC3','#A6D854','#FC8D62','#66C2A5')) +
  coord_flip() +
  theme(legend.position="bottom")
phonology_K5_structure_plt

morphology_K5_structure_plt <- ggplot(morphology_K5_renamed_melt, aes(x=factor(Language, level=languages_ordered),y=value,fill=variable)) +
  geom_col() + 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line =element_blank()) +
  labs(title = "Morphology", x = element_blank(),y = element_blank()) + 
  scale_fill_manual(name = element_blank(), labels = c('Tk','JK','Tg','A','M'),values=c('#FC8D62','#E78AC3','#66C2A5','#A6D854','#8DA0CB')) +
  coord_flip() +
  theme(legend.position="bottom")
morphology_K5_structure_plt

syntax_K5_structure_plt <- ggplot(syntax_K5_renamed_melt, aes(x=factor(Language, level=languages_ordered),y=value,fill=variable)) +
  geom_col() + 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line =element_blank()) +
  labs(title = "Syntax", x = element_blank(),y = element_blank()) +
  scale_fill_manual(name = element_blank(), labels = c('JK','A1','Tk','Tg','A2'),values=c('#E78AC3','#8DA0CB','#FC8D62','#66C2A5','#A6D854')) +
  coord_flip() +
  theme(legend.position="bottom")
syntax_K5_structure_plt

ggsave("plots/population-structure-k5.pdf", 
       (phonology_K5_structure_plt | morphology_K5_structure_plt | syntax_K5_structure_plt),height=10,width=10)
