# Calculate admixture per language family
#library(dplyr)
library(readr)
library(tidyr)
library(dplyr)

# Read in the data
setwd("./GitHub/admixture")

phonology_K4 <- read.csv("data/outfiles_cleaned/outfile_phonology_structure_K4_run41_f", sep="\t")
morphology_K4 <- read.csv("data/outfiles_cleaned/outfile_morphology_structure_K4_run6_f", sep="\t")
syntax_K4 <- read.csv("data/outfiles_cleaned/outfile_syntax_structure_K4_run36_f", sep="\t")

# load the file with language names and glottocodes
languages <- read_tsv("data/languages_map.tsv")

# replace glottocodes with full language names in the admixture file 
rename <- function(dataset){
  dataset %>%
    left_join(languages, by = c("Language" = "glottocode")) %>%
    select(Language = full_name_no_spaces, starts_with("Pop"), family)
}

# replace glottocodes with full language names in the admixture file and reshape
rename_gather <- function(dataset){
  dataset %>%
    left_join(languages, by = c("Language" = "glottocode")) %>%
    select(Language = full_name_no_spaces, starts_with("Pop"), family) %>%
    gather(variable, value, -Language, -family)
}

# calculate average Pop proportions per language family and find out,
# which population corresponds to each family
calculate_proportions <- function(dataset){
  dataset %>%
    group_by(family, variable) %>%
    summarise(mean = round(mean(value), digits = 2)) %>%
    pivot_wider(names_from = variable, values_from = mean)
}

# calculate admixture per family per level
calculate_admixture <- function(dataset){
  dataset %>%
    group_by(family, variable) %>%
    summarise(mean = round(mean(value), digits = 2), sd = round(sd(value), digits = 2)) %>%
    mutate(mean = 1 - max(mean), sd = sd[mean == max(mean)]) %>%
    select(family, mean, sd) %>%
    filter(duplicated(family) == FALSE)
}

# attribute each Pop to family
pop_to_family <- function(dataset_rename_gather){
  dataset_rename_gather %>%
    group_by(family, variable) %>%
    mutate(max_mean = max(mean(value))) %>%
    select(-Language, -value) %>%
    filter(duplicated(family) == FALSE) %>%
    group_by(family) %>%
    filter(max_mean == max(max_mean)) 
}

#### Prepare the data #### 

phonology_rename <- rename(phonology_K4)
morphology_rename <- rename(morphology_K4)
syntax_rename <- rename(syntax_K4)

phonology_rename_gather <- rename_gather(phonology_K4)
morphology_rename_gather <- rename_gather(morphology_K4)
syntax_rename_gather <- rename_gather(syntax_K4)

#### Admixture #### 
phonology_admixture <- calculate_admixture(phonology_rename_gather)
morphology_admixture <- calculate_admixture(morphology_rename_gather)
syntax_admixture <- calculate_admixture(syntax_rename_gather)

#### Proportions #### 

# Calculate average proportions of each Pop per family across languages
phonology_proportions <- calculate_proportions(phonology_rename_gather)
morphology_proportions <- calculate_proportions(morphology_rename_gather)
syntax_proportions <- calculate_proportions(syntax_rename_gather)

####  Calculate mean admixture per level #### 
mean(phonology_admixture$mean)
mean(morphology_admixture$mean)
mean(syntax_admixture$mean)

####  Calculate sd for admixture per level #### 
sd(phonology_admixture$mean)
sd(morphology_admixture$mean)
sd(syntax_admixture$mean)

#### Calculate mean admixture per level ####
# Create a table with average admixture values per family per level
admixture <- phonology_admixture %>%
  inner_join(morphology_admixture, by = "family") %>%
  inner_join(syntax_admixture, by = "family")

# rename columns
names(admixture) <- c("family", "phonology", "phonology_sd", "morphology", "morphology_sd", "syntax", "syntax_sd")
# convert tibble to data frame
admixture <- as.data.frame(admixture) 
# save family names in row names
rownames(admixture) <- admixture$family 
admixture <- admixture[,-1] # delete the column with family names
write.table(admixture, "data/admixture_table.txt", quote = FALSE, sep = " & ")

admixture_without_sd <- admixture[,c(1,3,5)]
round(rowMeans(admixture_without_sd), digits = 3) # calculate the mean admixture per family

admixture_without_mean <- admixture[,c(2,4,6)]
round(rowMeans(admixture_without_mean), digits = 3) # calculate the sd admixture per family

# Attribute Pops to families and replace column names in the datasets

pop_to_family(phonology_rename_gather)
pop_to_family(morphology_rename_gather)
pop_to_family(syntax_rename_gather)

names(phonology_rename) <- c("Language", "Mongolo-Koreanic", "Tungusic", "Japonic", "Turkic", "Family")
names(morphology_rename) <- c("Language", "Japono-Koreanic", "Turkic", "Tungusic", "Mongolic", "Family")
names(syntax_rename) <- c("Language", "Tungusic", "Japono-Koreanic", "Turkic", "Mongolic", "Family")

phonology_rename <- phonology_rename %>%
  arrange(desc(Family))

morphology_rename <- morphology_rename %>%
  arrange(desc(Family))

syntax_rename <- syntax_rename %>%
  arrange(desc(Family))
  

write.table(phonology_rename, "data/phonology.txt", quote = FALSE, sep = " & ", row.names = FALSE)
write.table(morphology_rename, "data/morphology.txt", quote = FALSE, sep = " & ", row.names = FALSE)
write.table(syntax_rename, "data/syntax.txt", quote = FALSE, sep = " & ", row.names = FALSE)
