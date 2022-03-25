# plot all K's
library(ggplot2)
library(readr)
library(tidyr)

# Read in the data
setwd("./admixture")

# load the file with language names and glottocodes
languages <- read_tsv("data/languages_map.tsv")

filenames_phonology <- list.files("data/outfiles_cleaned", 
                                  pattern = "_phonology_", full.names=TRUE)
filenames_morphology <- list.files("data/outfiles_cleaned", 
                                   pattern = "_morphology_", full.names=TRUE)
filenames_syntax <- list.files("data/outfiles_cleaned", 
                               pattern = "_syntax_", full.names=TRUE)

ldf_phonology <- lapply(filenames_phonology, read_tsv)
ldf_morphology <- lapply(filenames_morphology, read_tsv)
ldf_syntax <- lapply(filenames_syntax, read_tsv)

names(ldf_phonology) = c("K10", "K2", "K3", "K4", "K5", "K6", "K7", "K8", "K9")
names(ldf_morphology) = c("K10", "K2", "K3", "K4", "K5", "K6", "K7", "K8", "K9")
names(ldf_syntax) = c("K10", "K2", "K3", "K4", "K5", "K6", "K7", "K8", "K9")

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

k_ordered <- c("K2", "K3", "K4", "K5", "K6", "K7", "K8", "K9", "K10")

# replace glottocodes with full language names in the admixture file
rename_gather <- function(dataset){
  dataset %>%
    left_join(languages, by = c("Language" = "glottocode")) %>%
    select(Language = full_name_no_spaces, starts_with("Pop")) %>%
    gather(variable, value, -Language)
}

plot_admixture <- function(dataset){
  ggplot(dataset,
         aes(x = factor(Language, level = languages_ordered),
             y = value, fill = variable)) +
    geom_col() +
    theme(axis.ticks.y = element_blank(), 
          axis.line = element_blank(), 
          legend.position = "none", 
          axis.text.x = element_blank(), 
          axis.ticks.x = element_blank()) +
    labs(x = element_blank(),
         y = element_blank() ) +
    coord_flip() +
    facet_wrap(.~factor(K, level = k_ordered), 
               nrow = 1)
}

# Set default theme
theme_set(theme_classic())

# Plot phonology
ldf_phonology_renamed_gathered <- lapply(ldf_phonology, rename_gather)
ldf_phonology_renamed_gathered_bind_rows <- bind_rows(ldf_phonology_renamed_gathered, .id = "K")
phonology_plot <- plot_admixture(ldf_phonology_renamed_gathered_bind_rows)
phonology_plot <- phonology_plot + ggtitle("Phonology")

# Plot morphology
ldf_morphology_renamed_gathered <- lapply(ldf_morphology, rename_gather)
ldf_morphology_renamed_gathered_bind_rows <- bind_rows(ldf_morphology_renamed_gathered, .id = "K")
morphology_plot <- plot_admixture(ldf_morphology_renamed_gathered_bind_rows)
morphology_plot <- morphology_plot + ggtitle("Morphology")

# Plot morphology
ldf_syntax_renamed_gathered <- lapply(ldf_syntax, rename_gather)
ldf_syntax_renamed_gathered_bind_rows <- bind_rows(ldf_syntax_renamed_gathered, .id = "K")
syntax_plot <- plot_admixture(ldf_syntax_renamed_gathered_bind_rows)
syntax_plot <- syntax_plot + ggtitle("Syntax")

ggsave("plots/phonology.pdf", phonology_plot, height = 10, width = 10)
ggsave("plots/morphology.pdf",morphology_plot, height = 10, width = 10)
ggsave("plots/syntax.pdf",syntax_plot, height = 10, width = 10)

