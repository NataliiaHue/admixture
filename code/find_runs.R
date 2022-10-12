# Find runs for plotting later: one run with maximum LnProbData value per K
outfile <- read.csv("data/outfile.txt",sep=";")

# Find the run with the highest LnProbData

max_value <- function(dataset) {
  dataset %>%
  group_by(K, level) %>%
  mutate(max_LnProbData = round(max(LnProbData), digits = 1)) %>%
  filter(LnProbData == max_LnProbData)
}

runs <- max_value(outfile)