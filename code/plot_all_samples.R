# plot all K's
library(ggplot2)
library(readr)
library(tidyr)
library(patchwork)

# load the file with language names and glottocodes
languages <- read_tsv("data/languages_map.tsv")

filenames <- list.files("data/outfiles_cleaned_samples_lnprobdata", 
                        pattern = "_sample_", full.names=TRUE)

ldf <- lapply(filenames, read_tsv)

names(ldf) = c("S1", "S10", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9")

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

s_ordered <- c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9", "S10")

# replace glottocodes with full language names in the admixture file
rename_gather <- function(dataset, s){
  dataset %>%
    left_join(languages, by = c("Language" = "glottocode")) %>%
    select(Language = full_name_no_spaces, starts_with("Pop")) %>%
    gather(variable, value, -Language)
}

# takes a dataset, i.e. a "melted" dataframe with proportions and the number 
# of the sample
plot_admixture <- function(dataset, sample){
  ldf_renamed_gathered_bind_rows %>% filter(S == sample) %>%
    ggplot(aes(x = factor(Language, level = languages_ordered),
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
    ggtitle(sample)
}

# Set default theme
theme_set(theme_classic())

ldf_renamed_gathered <- lapply(ldf, rename_gather)
ldf_renamed_gathered_bind_rows <- bind_rows(ldf_renamed_gathered, .id = "S")

# plot samples from 1 to 10 and save the plot objects separately
for (sample in 1:10){
  assign(paste0("plot_sample_", sample), plot_admixture(ldf_renamed_gathered_bind_rows, 
                                                        paste0("S", sample)))
}

ggsave("plots/plot-all-samples.pdf", 
       (plot_sample_1 / plot_sample_6 | plot_sample_2 / plot_sample_7 | plot_sample_3 / plot_sample_8 | plot_sample_4 / plot_sample_9 | plot_sample_5 / plot_sample_10), height = 5, width = 10)
