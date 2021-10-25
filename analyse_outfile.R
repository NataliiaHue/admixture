# analyse the output of structure - choose best K etc.

library(ggplot2)
library(dplyr)
library(patchwork) 

setwd("/Applications/structure_console/outfiles")

outfile = read.csv("outfile.txt",sep=";")

#TODO: is the difference between the probabilities bigger between runs or between K?
phonology_filter <- outfile %>%
  filter(level == "phonology")

word_filter <- outfile %>%
  filter(level == "word")

clause_filter <- outfile %>%
  filter(level == "clause")

plt_phonology <- ggplot(data = phonology_filter) + 
  stat_summary(
    mapping = aes(x = factor(K), y = meanLnLikelihood),
    fun.min = min,
    fun.max = max,
    fun = median
  ) +
  labs(title = "Phonology", x = "K (number of groups in the data)", y = "Mean LnLikelihood")
plt_phonology
# Plateaus at 4, huge variation from 5

plt_word <- ggplot(data = word_filter) + 
  stat_summary(
    mapping = aes(x = factor(K), y = meanLnLikelihood),
    fun.min = min,
    fun.max = max,
    fun = median
  ) +
  labs(title = "Morphology", x = "K (number of groups in the data)", y = "Mean LnLikelihood")
plt_word
# Plateaus at 4, no significant variation until 9, choose 4 as the best value

plt_clause <- ggplot(data = clause_filter) + 
  stat_summary(
    mapping = aes(x = factor(K), y = meanLnLikelihood),
    fun.min = min,
    fun.max = max,
    fun = median
  ) +
  labs(title = "Syntax", x = "K (number of groups in the data)", y = "Mean LnLikelihood")
plt_clause
# Plateaus around value 5, variation increase in 6

# Calculate the jump like:

#diffs <- clause_grouped_by_K %>%
#  mutate(median_meanLnLikelihood_across_runs - lag(median_meanLnLikelihood_across_runs)) %>%
#  na.omit() 

# The jump from 5 to 6 is too low compared to jumps before 5, therefore the best K is 5.

setwd("/Users/neshcheret/Documents/GitHub/articles/tree-subsets")
ggsave("variation-across-runs.pdf", (plt_phonology | plt_word | plt_clause) ,height=3,width=10)

theme_set(theme_classic())

phonology_runs_plt <- ggplot(phonology_filter, aes(x=factor(K),y=meanLnLikelihood))+
  geom_point(alpha=0.5) +
  labs(title = "Phonology", x = "K (number of groups in the data)", y = "Mean LnLikelihood")

phonology_runs_plt

word_runs_plt <- ggplot(word_filter, aes(x=factor(K),y=meanLnLikelihood))+
  geom_point(alpha=0.5) +
  labs(title = "Morphology", x = "K (number of groups in the data)", y = "Mean LnLikelihood")

word_runs_plt

clause_runs_plt <- ggplot(clause_filter, aes(x=factor(K),y=meanLnLikelihood))+
  geom_point(alpha=0.5) +
  labs(title = "Syntax", x = "K (number of groups in the data)", y = "Mean LnLikelihood")

clause_runs_plt

setwd("/Users/neshcheret/Documents/GitHub/articles/tree-subsets")
ggsave("variation-across-runs.pdf", (phonology_runs_plt | word_runs_plt | clause_runs_plt) ,height=3,width=10)


# Find the variance among runs and Ks for each level

# Phonology

phonology_runs_variance <- phonology_filter %>%
  group_by(K) %>%
  #calculate the variance between runs for each K
  summarise(variance_among_runs = var(meanLnLikelihood))
  
phonology_K_variance <- phonology_filter %>%
  group_by(Run) %>%
  #calculate the variance between runs for each K
  summarise(variance_among_Ks = var(meanLnLikelihood))

# Morphology

word_runs_variance <- word_filter %>%
  group_by(K) %>%
  #calculate the variance between runs for each K
  summarise(variance_among_runs = var(meanLnLikelihood))

word_K_variance <- word_filter %>%
  group_by(Run) %>%
  #calculate the variance between runs for each K
  summarise(variance_among_Ks = var(meanLnLikelihood))

# Syntax
clause_runs_variance <- clause_filter %>%
  group_by(K) %>%
  #calculate the variance between runs for each K
  summarise(variance_among_runs = var(meanLnLikelihood))

clause_K_variance <- word_filter %>%
  group_by(Run) %>%
  #calculate the variance between runs for each K
  summarise(variance_among_Ks = var(meanLnLikelihood))

mean(phonology_K_variance$variance_among_Ks) - mean(phonology_runs_variance$variance_among_runs)

mean(word_K_variance$variance_among_Ks) - mean(word_runs_variance$variance_among_runs)

mean(clause_K_variance$variance_among_Ks) - mean(clause_runs_variance$variance_among_runs)

# The variance of meanLnLikelihood between Ks is greater than the variance between runs for all three levels

# Choose the best K value: plot medians for K across runs

# Inspect phonology

phonology_grouped_by_K <- outfile %>%
  filter(level == "phonology") %>%
  group_by(K) %>%
  summarise(median_meanLnLikelihood_across_runs = median(meanLnLikelihood))

phonology_plt <- ggplot(phonology_grouped_by_K, aes(x=K,y=median_meanLnLikelihood_across_runs))+
  geom_point()

phonology_plt

# Inspect morphology

word_grouped_by_K <- outfile %>%
  filter(level == "word") %>%
  group_by(K) %>%
  summarise(median_meanLnLikelihood_across_runs = median(meanLnLikelihood))

word_plt <- ggplot(word_grouped_by_K, aes(x=K,y=median_meanLnLikelihood_across_runs))+
  geom_point()

word_plt

# Inspect syntax

clause_grouped_by_K <- outfile %>%
  filter(level == "clause") %>%
  group_by(K) %>%
  summarise(median_meanLnLikelihood_across_runs = median(meanLnLikelihood))

clause_plt <- ggplot(clause_grouped_by_K, aes(x=K,y=median_meanLnLikelihood_across_runs))+
  geom_point()

clause_plt

# Find the most average run: Filter the data to only contain the data on a certain K value and then print the run with the median value

# K 3

phonology_K3 <- phonology_filter %>%
  filter(K==3)
median_phonology_K3 <- round(median(phonology_K3$meanLnLikelihood), digits=1)
phonology_K3$Run[phonology_K3$meanLnLikelihood==median_phonology_K3]
# Run 36 46

word_K3 <- word_filter %>%
  filter(K==3)
median_word_K3 <- round(median(word_K3$meanLnLikelihood), digits=1)
word_K3$Run[word_K3$meanLnLikelihood==median_word_K3]
# Run 15 20 39 12  8 23 42  3 49 31 35

clause_K3 <- clause_filter %>%
  filter(K==3)
median_clause_K3 <- round(median(clause_K3$meanLnLikelihood),digits=1)
clause_K3$Run[clause_K3$meanLnLikelihood==median_clause_K3]
# Run 38, 39

# K 4

phonology_K4 <- phonology_filter %>%
  filter(K==4)
median_phonology_K4 <- round(median(phonology_K4$meanLnLikelihood), digits=1)
phonology_K4$Run[phonology_K4$meanLnLikelihood==median_phonology_K4]
# run 17 50  4 23

word_K4 <- word_filter %>%
  filter(K==4)
median_word_K4 <- round(median(word_K4$meanLnLikelihood), digits=1)
word_K4$Run[word_K4$meanLnLikelihood==median_word_K4]
# Run 1

clause_K4 <- clause_filter %>%
  filter(K==4)
median_clause_K4 <- round(median(clause_K4$meanLnLikelihood),digits=1)
clause_K4$Run[clause_K4$meanLnLikelihood==median_clause_K4]
# Run 46

# K 5

phonology_K5 <- phonology_filter %>%
  filter(K==5)
median_phonology_K5 <- round(median(phonology_K5$meanLnLikelihood), digits=0)
phonology_K5$Run[phonology_K5$meanLnLikelihood==median_phonology_K5]
# Run 6

word_K5 <- word_filter %>%
  filter(K==5)
median_word_K5 <- round(median(word_K5$meanLnLikelihood), digits=1)
word_K5$Run[word_K5$meanLnLikelihood==median_word_K5]
# Run 16

clause_K5 <- clause_filter %>%
  filter(K==5)
median_clause_K5 <- round(median(clause_K5$meanLnLikelihood),digits=1)
clause_K5$Run[clause_K5$meanLnLikelihood==median_clause_K5]
# Run 16


