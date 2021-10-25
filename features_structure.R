#setwd("/Applications/structure_console/outfiles")

library(ggplot2)
library(dplyr)
library(reshape)

setwd("/Users/neshcheret/Documents/GitHub/articles/tree-subsets")

feature_set <- read.csv("feature_set.txt", sep="\t")

phonology_K4_feature_constributions = read.csv("phonology_K4_feature_contributions.txt",sep="\t")

word_K4_feature_constributions = read.csv("word_K4_feature_contributions.txt",sep="\t")

clause_K4_feature_constributions = read.csv("clause_K4_feature_contributions.txt",sep="\t")

phonology_K4_features_names <- phonology_K4_feature_constributions %>% 
  inner_join(feature_set,by=c("Feature"="ID"))

word_K4_features_names <- word_K4_feature_constributions %>% 
  inner_join(feature_set,by=c("Feature"="ID"))

clause_K4_features_names <- clause_K4_feature_constributions %>% 
  inner_join(feature_set,by=c("Feature"="ID"))

phonology_matrix_K4 <- read.csv("phonology_matrix_K4.txt", sep="\t")
word_matrix_K4 <- read.csv("word_matrix_K4.txt", sep="\t")
clause_matrix_K4 <- read.csv("clause_matrix_K4.txt", sep="\t")

clause_matrix_K5 <- read.csv("clause_matrix_K5.txt", sep="\t")

rename_languages <- function(structure_data_frame){
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

clause_K5_renamed <- rename_languages(clause_matrix_K5)

# reshape the data for plotting with ggplot later
phonology_K4_renamed_melt <- melt(phonology_K4_renamed, id=c("Language"))
word_K4_renamed_melt <- melt(word_K4_renamed, id=c("Language"))
clause_K4_renamed_melt <- melt(clause_K4_renamed, id=c("Language"))

clause_K5_renamed_melt <- melt(clause_K5_renamed, id=c("Language"))

# The order of the languages to appear in the structure plot
languages_ordered <- c("Ura","Okinoerabu","Yuwan","Tsuken","Shuri","Hateruma","Yonaguni","Ikema","Ogami","Tarama","Japanese","EasternOldJapanese","MiddleKorean","Korean","Manchu","Oroch","Udihe","Nanai","Orok","Ulch","BeryozovkaEven","MomaEven","Solon","Negidal","Evenki","MiddleMongol","Moghol","Dagur","KhamniganMongol","Khalkha","Oirat","Ordos","Buriat","Kalmyk","Bonan","Dongxiang","ShiraYughur","Mongghul","Mangghuer","OldTurkic","Chuvash","Khalaj","Khakas","Shor","Tuvan","Bashkir","Tatar","KaraKalpak","Kazakh","Nogai","CrimeanTatar","Dolgan","Yakut","Gagauz","Azerbaijani","Turkmen","Turkish","Chagatai","NorthernUzbek","Uighur")

# Set default theme
theme_set(theme_classic())

# Phonology: feature contributions

phonology_K4_features_selected_1 <- phonology_K4_features_names%>%
  filter(Value==1) %>%
  select(Feature_short,Pop1,Pop2,Pop3,Pop4)

phonology_K4_features_selected_0 <- phonology_K4_features_names %>%
  filter(Value==0) %>%
  select(Feature_short,Pop1,Pop2,Pop3,Pop4)

phonology_K4_features_selected_1_melt <- melt(phonology_K4_features_selected_1, id=c("Feature_short"))

phonology_K4_features_selected_0_melt <- melt(phonology_K4_features_selected_0, id=c("Feature_short"))

# 'J','MK','Tk','Tg

phonology_K4_features_selected_1_melt_plt <- ggplot(phonology_K4_features_selected_1_melt, aes(x=value,y=reorder(Feature_short,value),fill=variable)) +
  geom_col() +
  labs(title = "Feature presence", x = "Proportion in each ancestry",y=element_blank()) +
  scale_fill_manual(name = "Ancestry:",labels = c('J','MK','Tk','Tg'),values=c('#E78AC3','#8DA0CB','#FC8D62','#66C2A5'))

phonology_K4_features_selected_1_melt_plt

phonology_K4_features_selected_0_melt_plt <- ggplot(phonology_K4_features_selected_0_melt, aes(x=value,y=reorder(Feature_short,value),fill=variable)) +
  geom_col() +
  labs(title = "Feature absence", x = "Proportion in each ancestry",y=element_blank()) +
  scale_fill_manual(name = "Ancestry:",labels = c('Turkic','Tungusic','Koreano-Mongolic','Japonic'),values=c('#FC8D62','#66C2A5','#8DA0CB','#E78AC3'))

phonology_K4_features_selected_0_melt_plt

ggsave("phonology-K4-features-1.pdf", phonology_K4_features_selected_1_melt_plt,height=5,width=7)

ggsave("phonology-K4-features-0.pdf", phonology_K4_features_selected_0_melt_plt,height=5,width=7)

# Morphology: feature contributions

word_K4_features_selected_1 <- word_K4_features_names%>%
  filter(Value==1) %>%
  select(Feature_short,Pop1,Pop2,Pop3,Pop4)

word_K4_features_selected_0 <- word_K4_features_names %>%
  filter(Value==0) %>%
  select(Feature_short,Pop1,Pop2,Pop3,Pop4)

word_K4_features_selected_1_melt <- melt(word_K4_features_selected_1, id=c("Feature_short"))

word_K4_features_selected_0_melt <- melt(word_K4_features_selected_0, id=c("Feature_short"))

# Pop1 = Mongolic, Pop2 = Japonic, Pop3 = Turkic, Pop4 = Tungusic

word_K4_features_selected_0_melt_plt <- ggplot(word_K4_features_selected_0_melt, aes(x=value,y=reorder(Feature_short,value),fill=variable)) +
  geom_col() +
  labs(title = "Feature absence", x = "Proportion in each ancestry",y=element_blank()) +
  scale_fill_manual(name = "Ancestry:",labels = c('Mongolic','Japono-Koreanic','Turkic','Tungusic'),values=c('#8DA0CB','#E78AC3','#FC8D62','#66C2A5'))

word_K4_features_selected_0_melt_plt

word_K4_features_selected_1_melt_plt <- ggplot(word_K4_features_selected_1_melt, aes(x=value,y=reorder(Feature_short,value),fill=variable)) +
  geom_col() +
  labs(title = "Feature presence", x = "Proportion in each ancestry",y=element_blank()) +
  scale_fill_manual(name = "Ancestry:",labels = c('Mongolic','Japono-Koreanic','Turkic','Tungusic','Mongolic'),values=c('#8DA0CB','#E78AC3','#FC8D62','#66C2A5'))

word_K4_features_selected_1_melt_plt

ggsave("word-K4-features-0.pdf", word_K4_features_selected_0_melt_plt,height=10,width=7)

ggsave("word-K4_features-1.pdf", word_K4_features_selected_1_melt_plt,height=10,width=7)

# Syntax: feature contributions

clause_K4_features_selected_1 <- clause_K4_features_names%>%
  filter(Value==1) %>%
  arrange(desc(EstAncestralFrequency)) %>%
  select(Feature_short,Pop1,Pop2,Pop3,Pop4)

clause_K4_features_selected_0 <- clause_K4_features_names %>%
  filter(Value==0) %>%
  arrange(desc(EstAncestralFrequency)) %>%
  select(Feature_short,Pop1,Pop2,Pop3,Pop4)
  
clause_K4_features_selected_1_melt <- melt(clause_K4_features_selected_1, id=c("Feature_short"))

clause_K4_features_selected_0_melt <- melt(clause_K4_features_selected_0, id=c("Feature_short"))

#Pop1 = Japonic, Pop2 = Mongolic, Pop3 = Turkic, Pop4 = Tungusic

clause_K4_features_selected_1_melt_plt <- ggplot(clause_K4_features_selected_1_melt, aes(x=value,y=reorder(Feature_short,value),fill=variable)) +
  geom_col() +
  labs(title = "Feature presence", x = "Proportion in each ancestry",y=element_blank()) +
  scale_fill_manual(name = "Ancestry:",labels = c('Japono-Koreanic','Mongolic','Turkic','Tungusic'),values=c('#E78AC3','#8DA0CB','#FC8D62','#66C2A5'))

clause_K4_features_selected_1_melt_plt

clause_K4_features_selected_0_melt_plt <- ggplot(clause_K4_features_selected_0_melt, aes(x=value,y=reorder(Feature_short,value),fill=variable)) +
  geom_col() +
  labs(title = "Feature absence", x = "Proportion in each ancestry",y=element_blank()) +
  scale_fill_manual(name = "Ancestry:",labels = c('Japono-Koreanic','Tungusic','Turkic','Mongolic'),values=c('#E78AC3','#66C2A5','#FC8D62','#8DA0CB'))

clause_K4_features_selected_0_melt_plt

ggsave("clause-K4-features-1.pdf", clause_K4_features_selected_1_melt_plt,height=10,width=7)

ggsave("clause-K4-features-0.pdf", clause_K4_features_selected_0_melt_plt,height=10,width=7)
