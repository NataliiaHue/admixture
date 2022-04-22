#setwd("/Applications/structure_console/outfiles")

library(ggplot2)
library(dplyr)
library(patchwork)

##### Load the data #####
feature_set <- read.csv("data/feature_set.txt", sep="\t")

phonology_K4_feature_contributions = read.csv("data/feature_contributions/outfile_phonology_structure_K4_run41_f",sep="\t")

morphology_K4_feature_contributions = read.csv("data/feature_contributions/outfile_morphology_structure_K4_run6_f",sep="\t")

syntax_K4_feature_contributions = read.csv("data/feature_contributions/outfile_syntax_structure_K4_run36_f",sep="\t")

# rename the columns according to the determined family-ancestry correspondences
names(phonology_K4_feature_contributions) <- c("Feature", "PercentMissing", "ProportionPresent", "Mongolo-Koreanic", "Tungusic", "Japonic", "Turkic")
names(morphology_K4_feature_contributions) <- c("Feature", "PercentMissing", "ProportionPresent", "Japono-Koreanic", "Turkic", "Tungusic", "Mongolic")
names(syntax_K4_feature_contributions) <- c("Feature", "PercentMissing", "ProportionPresent", "Tungusic", "Japono-Koreanic", "Turkic", "Mongolic")

# clean the data:
# rename the features (from glottocode to short feature name)
# remove unnecessary columns from the structure output

phonology_K4_features_names <- phonology_K4_feature_contributions %>% 
  inner_join(feature_set,by=c("Feature"="ID")) %>%
  select(-Feature, -PercentMissing, -ProportionPresent, -Feature.y)

morphology_K4_features_names <- morphology_K4_feature_contributions %>% 
  inner_join(feature_set,by=c("Feature"="ID")) %>%
  select(-Feature, -PercentMissing, -ProportionPresent, -Feature.y)

syntax_K4_features_names <- syntax_K4_feature_contributions %>% 
  inner_join(feature_set,by=c("Feature"="ID")) %>%
  select(-Feature, -PercentMissing, -ProportionPresent, -Feature.y)

# Set default theme
theme_set(theme_classic())

#### Plots ####
# Phonology: feature contributions

# reshape the data for plotting

phonology_K4_features_gather <- phonology_K4_features_names %>%
  gather(variable, value, -Feature_short)

phonology_K4_features_gather_plot <- ggplot(phonology_K4_features_gather, aes(x=value,y=reorder(Feature_short,value),fill=variable)) +
  geom_col() +
  labs(title = "Phonology",  x = "Feature presence frequency", y=element_blank()) +
  theme(legend.position="bottom") +
  scale_fill_manual(name = "Ancestry:", labels = c("J", "MK", "Tg", "Tk"), values=c('#E78AC3','#8DA0CB','#66C2A5','#FC8D62'))

phonology_K4_features_gather_plot

# Morphology: feature contributions

# reshape the data for plotting

morphology_K4_features_gather <- morphology_K4_features_names %>%
  gather(variable, value, -Feature_short)

morphology_K4_features_gather_plot <- ggplot(morphology_K4_features_gather, aes(x=value,y=reorder(Feature_short,value),fill=variable)) +
  geom_col() +
  labs(title = "Morphology", x = "Feature presence frequency",y=element_blank()) +
  theme(legend.position="bottom") +
  scale_fill_manual(name = "Ancestry:", labels = c("JK", "M", "Tg", "Tk"), values=c('#E78AC3','#8DA0CB','#66C2A5','#FC8D62'))

morphology_K4_features_gather_plot

# Syntax: feature contributions

# reshape the data for plotting

syntax_K4_features_gather <- syntax_K4_features_names %>%
  gather(variable, value, -Feature_short)

syntax_K4_features_gather_plot <- ggplot(syntax_K4_features_gather, aes(x=value,y=reorder(Feature_short,value),fill=variable)) +
  geom_col() +
  labs(title = "Syntax", x = "Feature presence frequency", y=element_blank()) +
  theme(legend.position="bottom") +
  scale_fill_manual(name = "Ancestry:", labels = c("JK", "M", "Tg", "Tk"), values=c('#E78AC3','#8DA0CB','#66C2A5','#FC8D62'))

syntax_K4_features_gather_plot

# save all levels in one file
ggsave("plots/features.pdf", (phonology_K4_features_gather_plot | morphology_K4_features_gather_plot | syntax_K4_features_gather_plot) ,height=15,width=15)

# save each level in a separate file
ggsave("plots/phonology-K4-features.pdf", phonology_K4_features_gather_plot,height=5,width=7)
ggsave("plots/morphology-K4_feature.pdf", morphology_K4_features_gather_plot,height=10,width=7)
ggsave("plots/syntax-K4-features.pdf", syntax_K4_features_gather_plot,height=10,width=7)
