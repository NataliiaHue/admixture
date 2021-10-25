data <- data.frame()

generate_random_feature_set <- function(n_features, n_languages) {
  for (language in n_languages) {
    data[language,] <- sample(c(0,1), replace=TRUE, size=n_features)
  }
}

generate_random_feature_set(20,5)

feat1 <- sample(c(0,1), replace=TRUE, size=5)
feat2 <- sample(c(0,1), replace=TRUE, size=5)
feat3 <- sample(c(0,1), replace=TRUE, size=5)
feat4 <- sample(c(0,1), replace=TRUE, size=5)
feat5 <- sample(c(0,1), replace=TRUE, size=5)

feature_vector <- c("feat1", "feat2", "feat3", "feat4", "feat5")
language_vector <- c("lan1", "lan2", "lan3", "lan4", "lan5")

my_data <- data_frame(unlist(feat1), unlist(feat2), unlist(feat3), unlist(feat4), unlist(feat5))
rownames(my_data) <- language_vector
colnames(my_data) <- feature_vector

write_nexus_t <- function(data,output_file) {
  # write nexus file
  sink(output_file,append=FALSE)
  cat("#NEXUS\nBEGIN DATA;\n")
  cat("    DIMENSIONS NTAX=")
  cat(length(rownames(data)))
  cat(' NCHAR=')
  cat(length(colnames(data)))
  cat(";\n")
  cat('    FORMAT MISSING=- SYMBOLS="01";\n');
  cat("MATRIX\n")
  for (language in 1:length(rownames(data))){
    cat(rownames(data)[language])
    cat(" ")
    for  (feature in 1:length(colnames(data))) {
      cat(data[language,feature])
    }
    cat("\n")
  }
  cat(";\nEND;")
  sink()
}

write_nexus_t(my_data, "my_small_radom_dataset.nex")
