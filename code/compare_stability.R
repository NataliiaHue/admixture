# compare results with stability papers
library(readr)
library(dplyr)

stability <- read_tsv("stability/results/SI_summary_table.tsv")
feature_set <- read.csv("admixture/data/feature_set.txt", sep="\t")

phonology_K4_feature_contributions <- read.csv("data/feature_contributions/outfile_phonology_structure_K4_run41_f",sep="\t")

morphology_K4_feature_contributions <- read.csv("data/feature_contributions/outfile_morphology_structure_K4_run6_f",sep="\t")

syntax_K4_feature_contributions <- read.csv("data/feature_contributions/outfile_syntax_structure_K4_run36_f",sep="\t")

# rename the columns according to the determined family-ancestry correspondences
names(phonology_K4_feature_contributions) <- c("Feature", "PercentMissing", "ProportionPresent", "Mongolo_Koreanic", "Tungusic", "Japonic", "Turkic")
names(morphology_K4_feature_contributions) <- c("Feature", "PercentMissing", "ProportionPresent", "Japono_Koreanic", "Turkic", "Tungusic", "Mongolic")
names(syntax_K4_feature_contributions) <- c("Feature", "PercentMissing", "ProportionPresent", "Tungusic", "Japono_Koreanic", "Turkic", "Mongolic")

join_with_stability <- function(dataset_feature_contributions){
  stability %>%
  inner_join(dataset_feature_contributions, by = c("Feature" = "Feature")) %>%
  select(Feature_short, starts_with("p1_"), Mongolo_Koreanic, Tungusic, Japonic, Turkic) %>%
  gather(ancestry, proportion_ancestry, -Feature_short, - starts_with("p1_")) %>%
  gather(family, reconstructed_present, -ancestry, -proportion_ancestry, -Feature_short)
}

select_plot_Japonic <- function(dataset_joined_long){
  dataset_joined_long %>%
    filter(ancestry == "Japonic", family == "p1_japonic") %>%
    ggplot(aes(reconstructed_present, proportion_ancestry)) +
    geom_point(color = "#E78AC3") +
    labs(x = 'Reconstructed as "Present" in Proto-Japonic', y = "Presence in Japonic ancestry", title = "Japonic")
}

select_plot_Turkic <- function(dataset_joined_long){
  dataset_joined_long %>%
    filter(ancestry == "Turkic", family == "p1_turkic") %>%
    ggplot(aes(reconstructed_present, proportion_ancestry)) +
    geom_point(color = "#FC8D62") +
    labs(x = 'Reconstructed as "Present" in Proto-Turkic', y = "Presence in Turkic ancestry", title = "Turkic")
}

select_plot_Tungusic <- function(dataset_joined_long){
  dataset_joined_long %>%
    filter(ancestry == "Tungusic", family == "p1_tungusic") %>%
    ggplot(aes(reconstructed_present, proportion_ancestry)) +
    geom_point(color = "#66C2A5") +
    labs(x = 'Reconstructed as "Present" in Proto-Tungusic', y = "Presence in Tungusic ancestry", title = "Tungusic")
}

select_plot_Mongolo_Koreanic <- function(dataset_joined_long){
  dataset_joined_long %>%
    filter(ancestry == "Mongolo_Koreanic", family == "p1_mongolic" | family == "p1_koreanic") %>%
    ggplot(aes(reconstructed_present, proportion_ancestry, color = family)) +
    geom_point() +
    #theme(legend.position="bottom") +
    labs(x = 'Reconstructed as "Present" for Proto-Mongolic or Proto-Koreanic', 
         y = "Presence in Mongolo-Koreanic ancestry", 
         title = "Mongolo-Koreanic") +
    scale_color_manual(name = "Proto-language:", 
                       labels = c("Proto-Koreanic", "Proto-Mongolic"), 
                       values = c("#DD3497","#8DA0CB"))
}

phonology_japonic_plot <- select_plot_Japonic(phonology_joined_long)
phonology_turkic_plot <- select_plot_Turkic(phonology_joined_long)
phonology_tungusic_plot <- select_plot_Tungusic(phonology_joined_long)
phonology_mongolo_koreanic_plot <- select_plot_Mongolo_Koreanic(phonology_joined_long)

ggsave("plots/phonology_stability.pdf", (phonology_japonic_plot | phonology_turkic_plot | phonology_tungusic_plot | phonology_mongolo_koreanic_plot) ,height=5,width=10)



mng_krn <- phonology_joined_long %>%
  filter(ancestry == "Mongolo_Koreanic", family == "p1_mongolic" | family == "p1_koreanic")

phonology_joined_long %>%
  filter(ancestry == "Mongolo_Koreanic", family == "p1_mongolic" | family == "p1_koreanic") %>%
  ggplot(aes(reconstructed_present, proportion_ancestry, color = family)) +
  geom_point() +
  #theme(legend.position="bottom") +
  labs(x = 'Reconstructed as "Present" for Proto-Mongolic or Proto-Koreanic', 
       y = "Presence in Mongolo-Koreanic ancestry", 
       title = "Mongolo-Koreanic") +
  scale_color_manual(name = "Proto-language:", 
                    labels = c("Proto-Koreanic", "Proto-Mongolic"), 
                    values = c("#DD3497","#8DA0CB"))

# "#8DA0CB","#3F007D"
# Unnecessary code

#phonology_joined_filtered <- phonology_joined_long_Japonic %>%
#  inner_join(phonology_joined_long_Tungusic, by = c("Feature_short" = "Feature_short")) %>%
#  inner_join(phonology_joined_long_Turkic, by = c("Feature_short" = "Feature_short")) 

phonology_joined <- stability %>%
  inner_join(phonology_K4_feature_contributions, by = c("Feature" = "Feature")) %>%
  select(Feature_short, starts_with("p1_"), Mongolo_Koreanic, Tungusic, Japonic, Turkic)

phonology_joined_long <- phonology_joined %>%
  gather(ancestry, proportion_ancestry, -Feature_short, - starts_with("p1_")) %>%
  gather(family, reconstructed_present, -ancestry, -proportion_ancestry, -Feature_short)

phonology_joined_long_Japonic <- phonology_joined_long %>%
  filter(ancestry == "Japonic", family == "p1_japonic") %>%
  ggplot(aes(reconstructed_present, proportion_ancestry)) +
  geom_point(color = "#E78AC3")

phonology_joined_long_Tungusic <- phonology_joined_long %>%
  filter(ancestry == "Tungusic", family == "p1_tungusic")

phonology_joined_long_Turkic <- phonology_joined_long %>%
  filter(ancestry == "Turkic", family == "p1_turkic")


ggplot(phonology_joined_long, aes(reconstructed_present, proportion_ancestry, value, col = family)) +
  geom_point() +
  facet_wrap(.~ ancestry)

ggplot(phonology_joined_long_Japonic, aes(reconstructed_present, proportion_ancestry)) +
  geom_point()

phonology_joined_long_mutate <- phonology_joined_long %>%
  mutate(family_ancestry = case_when(family == "p1_turkic" && ancestry == "Turkic" ~ "p1_turkic_Turkic",
                                     family == "p1_japonic" && ancestry == "Japonic" ~ "p1_japonic_Japonic",
                                     family == "p1_tungusic" && ancestry == "Tungusic" ~ "p1_tungusic_Tungusic",
                                     family == "p1_koreanic" && ancestry == "Mongolo_Koreanic" ~ "p1_koreanic_Mongolo_Koreanic",
                                     family == "p1_mongolic" && ancestry == "Mongolo_Koreanic" ~ "p1_mongolic_Mongolo_Koreanic",
                                     TRUE ~ NA))
