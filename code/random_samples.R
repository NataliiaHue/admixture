# make random samples
library(readr)

# Prepare the data
languages <- read_tsv("data/languages_map.tsv")

all_levels <- read_tsv("data/all_levels")
names(all_levels)[names(all_levels) == "...1"] <- "Language"


# Prepare the functions
# sample_languages: returns a tibble with a single column - 
# glottocodes of 10 languages: 2 languages per language family
sample_languages <- function(dataset) {
  dataset %>%
  group_by(family) %>%
  slice_sample(n=2) %>%
  ungroup() %>%
  select(glottocode)
}

# filter_dataset: returns a tibble with glottocodes and data
# before running STRUCTURE, delete the column name "Language"
filter_dataset <- function(dataset){
  all_levels %>%
  right_join(sample_languages(languages), by = c("Language" = "glottocode"))
}

# automate sampling: sample and write tsv
for (i in 1:10){
  sample <- filter_dataset(all_levels)
  sample <- as.data.frame(sample)
  names(sample)[names(sample) == "Language"] <- ""
  write_tsv(sample, paste("data/samples/sample_", i, sep = ""))
}
