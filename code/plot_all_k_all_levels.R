# plot all K's
library(ggplot2)
library(readr)
library(tidyr)

# Read in the data
setwd("./admixture")

# load the file with language names and glottocodes
languages <- read_tsv("data/languages_map.tsv")

filenames <- list.files("data/outfiles_cleaned_all_levels", 
                                  pattern = "_all_levels_", full.names=TRUE)

ldf <- lapply(filenames, read_tsv)

names(ldf) = c("K10", "K2", "K3", "K4", "K5", "K6", "K7", "K8", "K9")

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
ldf_renamed_gathered <- lapply(ldf, rename_gather)
ldf_renamed_gathered_bind_rows <- bind_rows(ldf_renamed_gathered, .id = "K")
plot_all_levels <- plot_admixture(ldf_renamed_gathered_bind_rows) + ggtitle("All levels")

ggsave("plots/plot-all-levels.pdf", plot_all_levels, height = 10, width = 10)
