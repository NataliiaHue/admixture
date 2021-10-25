setwd("/Users/neshcheret/Documents/GitHub/articles/tree-subsets")

# load the data
FEATURES_TO_IGNORE <- c(
  "GB024", "GB025", "GB065", "GB130", "GB193", "GB203",
  "TE001", "TE002", "TE009", "TE012", "TE014", "TE015", "TE016", "TE022", "TE025",
  "TE026", "TE028", "TE029", "TE033", "TE034", "TE036", "TE040", "TE041", "TE042",
  "TE043", "TE044", "TE045", "TE046", "TE047", "TE048", "TE049", "TE051", "TE055",
  "TE056", "TE057", "TE058", "TE060", "TE061", "TE062", "TE063", "TE064", "TE065",
  "TE067", "TE068", "TE069", "TE070", "TE071", "TE072", "TE073", "TE074", "TE076",
  "TE077", "TS081", "TS082", "TS083", "TS083", "TS084", "TS085"
)
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

# takes input: data$Level, "word", "word.nex"
# argument 1: column, argument 2: one of the categorical categories, argument 3: output file
extract_subset <- function(data_column,category,output_file) {
  # extract features
  data_subset <- data[data_column==category,]
  data_subset <- data_subset[,-(1:5)]
  # write nexus file
  sink(output_file,append=FALSE)
  for (language in 1:length(colnames(data_subset))){
    cat(colnames(data_subset)[language])
    for  (feature in 1:length(rownames(data_subset))) {
      cat(data_subset[feature,language])
    }
    cat("\n")
  }
  cat(";\nEND;")
  sink()
}

# example of usage
extract_subset(data$Level,"word","word.nex")

extract_subset <- function(data_column,category,output_file) {
  # extract features
  data_subset <- data[data_column==category,]
  data_subset <- data_subset[,-(1:5)]
  # write nexus file
  sink(output_file,append=FALSE)
  cat("#NEXUS\nBEGIN DATA;\n")
  cat("    DIMENSIONS NTAX=")
  cat(length(colnames(data_subset)))
  cat(' NCHAR=')
  cat(length(rownames(data_subset)))
  cat(";\n")
  cat('    FORMAT MISSING=- SYMBOLS="01";\n');
  cat("MATRIX\n")
  for (language in 1:length(colnames(data_subset))){
    cat(colnames(data_subset)[language])
    cat(" ")
    for  (feature in 1:length(rownames(data_subset))) {
      cat(data_subset[feature,language])
    }
    cat("\n")
  }
  cat(";\nEND;")
  sink()
}


