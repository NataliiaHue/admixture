# Find runs for plotting later
outfile = read.csv("data/outfile.txt",sep=";")

# load the structure results and filter for each of the levels
phonology_filter <- outfile %>%
  filter(level == "phonology")

morphology_filter <- outfile %>%
  filter(level == "morphology")

syntax_filter <- outfile %>%
  filter(level == "syntax")

# Find the run with the highest LnProbData

max_value <- function(dataset) {
  dataset %>%
  group_by(K) %>%
  mutate(max_LnProbData = round(max(LnProbData), digits = 1)) %>%
  filter(LnProbData == max_LnProbData)
}

runs_phonology <- max_value(phonology_filter)
runs_morphology <- max_value(morphology_filter)
runs_syntax <- max_value(syntax_filter)
