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
data[data$Level=="NP",5]<-"clause"
data[data$Level=="phonological shape",5]<-"phonology"
data <- data[! data$ID %in% FEATURES_TO_IGNORE, ]


data[is.na(data)] <- '-'
# takes input: data$Level, "word", "word.nex"
# argument 1: column, argument 2: one of the categorical categories, argument 3: output file
extract_subset_nex <- function(data_column,category,output_file) {
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

# Level
extract_subset_nex(data$Level,"word","word.nex")
extract_subset_nex(data$Level,"clause","clause.nex")
extract_subset_nex(data$Level,"NP","NP.nex")
extract_subset_nex(data$Level,"phonological shape","phonological_shape.nex")
extract_subset_nex(data$Level,"multiple","multiple.nex")

# write the dates file for birth-death model
write_dates <- function(some_data, output_file) {
  # extract features
  some_data <- some_data[,-(1:5)]
  sink(output_file,append=FALSE)

  for (language in 1:length(colnames(some_data))){
    cat(colnames(some_data)[language])
    cat("\t2000")
    cat("\n")
  }
  sink()
}

write_dates(data, "dates.txt")


data[is.na(data)] <- '-9'

# write project file for STRUCTURE
# takes input: data$Level, "word", "word.nex"
# argument 1: column, argument 2: one of the categorical categories, argument 3: output file

extract_subset_structure <- function(data_column,category,output_file) {
  # extract features
  data_subset <- data[data_column==category,]
  data_subset <- data_subset[,-(2:5)]
  # write nexus file
  sink(output_file,append=FALSE)
  cat("\t")
  cat(data_subset$ID,sep="\t")
  data_subset <- data_subset[,-1]
  cat("\n")
  for (language in 1:length(colnames(data_subset))){
    cat(colnames(data_subset)[language])
    for  (feature in 1:length(rownames(data_subset))) {
      cat("\t")
      cat(data_subset[feature,language])
      
    }
    cat("\n")
  }
  sink()
}

extract_subset_structure(data$Level,"word","word_structure")
extract_subset_structure(data$Level,"clause","clause_structure")
extract_subset_structure(data$Level,"phonology","phonology_structure")
sum(data$Level=="word")
sum(data$Level=="clause")
sum(data$Level=="phonology")

