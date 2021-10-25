library(reshape)
library(ggplot2)
library(dplyr)

setwd("/Users/neshcheret/Documents/GitHub/articles/tree-subsets")

FEATURES_TO_IGNORE <- c(
  "GB024", "GB025", "GB065", "GB130", "GB193", "GB203",
  "TE001", "TE002", "TE009", "TE012", "TE014", "TE015", "TE016", "TE022", "TE025",
  "TE026", "TE028", "TE029", "TE033", "TE034", "TE036", "TE040", "TE041", "TE042",
  "TE043", "TE044", "TE045", "TE046", "TE047", "TE048", "TE049", "TE051", "TE055",
  "TE056", "TE057", "TE058", "TE060", "TE061", "TE062", "TE063", "TE064", "TE065",
  "TE067", "TE068", "TE069", "TE070", "TE071", "TE072", "TE073", "TE074", "TE076",
  "TE077", "TS081", "TS082", "TS083", "TS083", "TS084", "TS085"
)

q_residual_scores <- read.csv("q_residual_scores.txt", sep = "\t")
languages <- read.csv("languages_map.csv", sep = ";")
data <- read.csv(
  "all_languages_data_categories_full_names.csv",
  sep = ";",
  strip.white = TRUE,
  na.strings = c("?", "-"),
  stringsAsFactors = FALSE
)
data <- data[! data$ID %in% FEATURES_TO_IGNORE, ]
data[is.na(data)] <- '-'

data[data$Level=="NP",5]<-"clause"

data <- melt(data, id=c("ID", "Feature", "PoS", "Function", "Level"))

theme_set(theme_classic())

q_residual_scores %>%
  inner_join(languages, by = )

ggplot(q_residual_scores, aes(x = Delta.Score, y = reorder(Taxon, Delta.Score), )) + 
  geom_point()
  
