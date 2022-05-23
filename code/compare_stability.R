# compare results with stability paper
# plot recostructed ancestral states against contribution to ancestry
library(readr)
library(dplyr)
library(ggplot2)
library(patchwork)

stability <- read_tsv("./stability/results/SI_summary_table.tsv")
feature_set <- read.csv("./admixture/data/feature_set.txt", sep="\t")

phonology_K4_feature_contributions <- read.csv("./admixture/data/feature_contributions/outfile_phonology_structure_K4_run41_f",sep="\t")

morphology_K4_feature_contributions <- read.csv("./admixture/data/feature_contributions/outfile_morphology_structure_K4_run6_f",sep="\t")

syntax_K4_feature_contributions <- read.csv("./admixture/data/feature_contributions/outfile_syntax_structure_K4_run36_f",sep="\t")

# rename the columns according to the determined family-ancestry correspondences
names(phonology_K4_feature_contributions) <- c("Feature", "PercentMissing", "ProportionPresent", "Mongolo_Koreanic", "Tungusic", "Japonic", "Turkic")
names(morphology_K4_feature_contributions) <- c("Feature", "PercentMissing", "ProportionPresent", "Japono_Koreanic", "Turkic", "Tungusic", "Mongolic")
names(syntax_K4_feature_contributions) <- c("Feature", "PercentMissing", "ProportionPresent", "Tungusic", "Japono_Koreanic", "Turkic", "Mongolic")

phonology_joined_long <- stability %>%
  inner_join(phonology_K4_feature_contributions, by = c("Feature" = "Feature")) %>%
  select(Feature_short, starts_with("p1_"), Mongolo_Koreanic, Tungusic, Japonic, Turkic) %>%
  gather(ancestry, proportion_ancestry, -Feature_short, - starts_with("p1_")) %>%
  gather(family, reconstructed_present, -ancestry, -proportion_ancestry, -Feature_short)


join_morphosyntax_with_stability <- function(dataset_feature_contributions){
  stability %>%
    inner_join(dataset_feature_contributions, by = c("Feature" = "Feature")) %>%
    select(Feature_short, starts_with("p1_"), Japono_Koreanic, Tungusic, Mongolic, Turkic) %>%
    gather(ancestry, proportion_ancestry, -Feature_short, - starts_with("p1_")) %>%
    gather(family, reconstructed_present, -ancestry, -proportion_ancestry, -Feature_short)
}

morphology_joined_long <- join_morphosyntax_with_stability(morphology_K4_feature_contributions)

syntax_joined_long <- join_morphosyntax_with_stability(syntax_K4_feature_contributions)

select_plot_Japonic <- function(dataset_joined_long){
  dataset_joined_long %>%
    filter(ancestry == "Japonic", family == "p1_japonic") %>%
    ggplot(aes(reconstructed_present, proportion_ancestry)) +
    geom_point(color = "#E78AC3", size = 2.5) +
    labs(x = element_blank(), y = element_blank())
}

select_plot_Turkic <- function(dataset_joined_long){
  dataset_joined_long %>%
    filter(ancestry == "Turkic", family == "p1_turkic") %>%
    ggplot(aes(reconstructed_present, proportion_ancestry)) +
    geom_point(color = "#FC8D62", size = 2.5, alpha = 0.5) +
    labs(x = element_blank(), y = element_blank())
}

select_plot_Tungusic <- function(dataset_joined_long){
  dataset_joined_long %>%
    filter(ancestry == "Tungusic", family == "p1_tungusic") %>%
    ggplot(aes(reconstructed_present, proportion_ancestry)) +
    geom_point(color = "#66C2A5", size = 2.5, alpha = 0.5) +
    labs(x = element_blank(), y = element_blank())
}

select_plot_Mongolic <- function(dataset_joined_long){
  dataset_joined_long %>%
    filter(ancestry == "Mongolic", family == "p1_mongolic") %>%
    ggplot(aes(reconstructed_present, proportion_ancestry)) +
    geom_point(color = "#8DA0CB", size = 2.5, alpha = 0.5) +
    labs(x = element_blank(), y = element_blank())
}

select_plot_Mongolo_Koreanic <- function(dataset_joined_long){
  dataset_joined_long %>%
    filter(ancestry == "Mongolo_Koreanic", family == "p1_mongolic" | family == "p1_koreanic") %>%
    ggplot(aes(reconstructed_present, proportion_ancestry, color = family)) +
    geom_point(size = 2.5, alpha = 0.5) +
    #theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
    labs(x = element_blank(), y = element_blank()) +
    scale_color_manual(name = element_blank(), 
                       labels = c("K", "M"), 
                       values = c("#41B6C4","#8DA0CB")) +
    theme(legend.position="bottom") +
    ylim(0,1)
}

select_plot_Japono_Koreanic <- function(dataset_joined_long){
  dataset_joined_long %>%
    filter(ancestry == "Japono_Koreanic", family == "p1_japonic" | family == "p1_koreanic") %>%
    ggplot(aes(reconstructed_present, proportion_ancestry, color = family)) +
    geom_point(size = 2.5, alpha = 0.5) +
    labs(x = element_blank(), y = element_blank()) + 
    scale_color_manual(name = element_blank(), 
                       labels = c("J", "K"),  
                       values = c("#E78AC3","#41B6C4")) +
    theme(legend.position="bottom")
}

phonology_japonic_plot <- select_plot_Japonic(phonology_joined_long) + ylab("Japonic/Koreanic")
phonology_turkic_plot <- select_plot_Turkic(phonology_joined_long) + ggtitle("Phonology") + ylab("Turkic")
phonology_tungusic_plot <- select_plot_Tungusic(phonology_joined_long) + ylab("Tungusic")
phonology_mongolo_koreanic_plot <- select_plot_Mongolo_Koreanic(phonology_joined_long) + ylab("Mongolic/Koreanic")

morphology_turkic_plot <- select_plot_Turkic(morphology_joined_long) + ggtitle("Morphology")
morphology_tungusic_plot <- select_plot_Tungusic(morphology_joined_long)
morphology_japono_koreanic_plot <- select_plot_Japono_Koreanic(morphology_joined_long)
morphology_mongolic_plot <- select_plot_Mongolic(morphology_joined_long) 

syntax_turkic_plot <- select_plot_Turkic(syntax_joined_long) + ggtitle("Syntax")
syntax_tungusic_plot <- select_plot_Tungusic(syntax_joined_long)
syntax_mongolic_plot <- select_plot_Mongolic(syntax_joined_long) 
syntax_japono_koreanic_plot <- select_plot_Japono_Koreanic(syntax_joined_long)

ggsave("plots/reconstruction.pdf", (phonology_turkic_plot | morphology_turkic_plot | syntax_turkic_plot) / 
        ( phonology_tungusic_plot | morphology_tungusic_plot | syntax_tungusic_plot ) /
        ( phonology_japonic_plot | morphology_japono_koreanic_plot | syntax_japono_koreanic_plot) /
        ( phonology_mongolo_koreanic_plot | morphology_mongolic_plot | syntax_mongolic_plot), 
          height=8,width=6)
