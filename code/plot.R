# function plot structure
library(ggplot2)
library(patchwork) 
library(readr)
library(tidyr)

# Read in the data
setwd("./GitHub/admixture")

phonology_K4 <- read.csv("data/outfiles_cleaned/outfile_phonology_structure_K4_run41_f", sep="\t")
morphology_K4 <- read.csv("data/outfiles_cleaned/outfile_morphology_structure_K4_run6_f", sep="\t")
syntax_K4 <- read.csv("data/outfiles_cleaned/outfile_syntax_structure_K4_run36_f", sep="\t")

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

morphology_K4 %>%
  left_join(languages, by = c("Language" = "glottocode")) %>%
  select(Language = full_name_no_spaces, starts_with("Pop")) %>%
  gather(variable, value, -Language)

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
morphology_K4_full_names <- rename_gather(morphology_K4)
plot_admixture(morphology_K4_full_names) + theme(axis.text.y = element_blank())

# Prepare the data for plotting
phonology_K4_renamed_gathered <- rename_gather(phonology_K4)
morphology_K4_renamed_gathered <- rename_gather(morphology_K4)
syntax_K4_renamed_gathered <- rename_gather(syntax_K4)

# Plot K4

phonology_K4_structure_plt <- ggplot(phonology_K4_renamed_gathered, aes(x=factor(Language, level=languages_ordered),y=value,fill=variable)) +
  geom_col() +
  theme(axis.ticks.y = element_blank(), axis.line =element_blank()) +
  labs(title = "Phonology", x = element_blank(),y = element_blank() ) +
  scale_fill_manual(name = element_blank(), labels = c('MK','Tg','J','Tk'),values=c('#8DA0CB','#66C2A5','#E78AC3','#FC8D62')) + 
  coord_flip() +
  theme(legend.position="bottom")
phonology_K4_structure_plt

morphology_K4_structure_plt <- ggplot(morphology_K4_renamed_gathered, aes(x=factor(Language, level=languages_ordered),y=value,fill=variable)) +
  geom_col() + 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line =element_blank()) +
  labs(title = "Morphology", x = element_blank(), y = element_blank() ) +
  scale_fill_manual(name = element_blank(), labels = c('JK','Tk','Tg','M'),values=c('#E78AC3','#FC8D62','#66C2A5','#8DA0CB')) + 
  coord_flip() +
  theme(legend.position="bottom")
morphology_K4_structure_plt

syntax_K4_structure_plt <- ggplot(syntax_K4_renamed_gathered, aes(x=factor(Language, level=languages_ordered),y=value,fill=variable)) +
  geom_col() + 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line =element_blank()) +
  labs(title = "Syntax", x = element_blank(),y = element_blank()) +
  scale_fill_manual(name = element_blank(), labels = c('Tg','JK','Tk','M'),values=c('#66C2A5','#E78AC3','#FC8D62','#8DA0CB')) + 
  coord_flip() +
  theme(legend.position="bottom")
syntax_K4_structure_plt

ggsave("plots/population-structure-k4.pdf", (phonology_K4_structure_plt | morphology_K4_structure_plt | syntax_K4_structure_plt) ,height=10,width=10)
