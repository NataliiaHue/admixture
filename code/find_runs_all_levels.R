# Find runs for plotting later: one run with maximum LnProbData value per K
outfile <- read.csv("data/outfile_all_levels.txt",sep=";")

# Find the run with the highest LnProbData

max_value <- function(dataset) {
  dataset %>%
    group_by(K) %>%
    mutate(max_LnProbData = round(max(LnProbData), digits = 1)) %>%
    filter(LnProbData == max_LnProbData)
}

runs <- max_value(outfile)