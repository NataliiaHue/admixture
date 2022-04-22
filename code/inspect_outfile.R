# analyse the output of structure - choose best K etc.

library(ggplot2)
library(dplyr)
library(patchwork) 

outfile = read.csv("data/outfile.txt",sep=";")

phonology_filter <- outfile %>%
  filter(level == "phonology")

morphology_filter <- outfile %>%
  filter(level == "morphology")

syntax_filter <- outfile %>%
  filter(level == "syntax")

theme_set(theme_classic())

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

plt_morphology <- ggplot(data = morphology_filter) + 
  stat_summary(
    mapping = aes(x = factor(K), y = meanLnLikelihood),
    fun.min = min,
    fun.max = max,
    fun = median
  ) +
  labs(title = "Morphology", x = "K (number of groups in the data)", y = "Mean LnLikelihood")
plt_morphology
# Plateaus at 4, no significant variation until 8, choose 4 as the best value

plt_syntax <- ggplot(data = syntax_filter) + 
  stat_summary(
    mapping = aes(x = factor(K), y = meanLnLikelihood),
    fun.min = min,
    fun.max = max,
    fun = median
  ) +
  labs(title = "Syntax", x = "K (number of groups in the data)", y = "Mean LnLikelihood")
plt_syntax
# Plateaus around value 5, variation increase in 6

# Calculate the jump like:

#diffs <- syntax_grouped_by_K %>%
#  mutate(median_meanLnLikelihood_across_runs - lag(median_meanLnLikelihood_across_runs)) %>%
#  na.omit() 

# The jump from 5 to 6 is too low compared to jumps before 5, therefore the best K is 5.

ggsave("plots/variation-across-runs-bars.pdf", (plt_phonology | plt_morphology | plt_syntax) ,height=3,width=10)

phonology_runs_plt <- ggplot(phonology_filter, aes(x=factor(K),y=meanLnLikelihood))+
  geom_point(alpha=0.5) +
  labs(title = "Phonology", x = "K (number of groups in the data)", y = "Mean LnLikelihood")

phonology_runs_plt

morphology_runs_plt <- ggplot(morphology_filter, aes(x=factor(K),y=meanLnLikelihood))+
  geom_point(alpha=0.5) +
  labs(title = "Morphology", x = "K (number of groups in the data)", y = "Mean LnLikelihood")

morphology_runs_plt

syntax_runs_plt <- ggplot(syntax_filter, aes(x=factor(K),y=meanLnLikelihood))+
  geom_point(alpha=0.5) +
  labs(title = "Syntax", x = "K (number of groups in the data)", y = "Mean LnLikelihood")

syntax_runs_plt

ggsave("plots/variation-across-runsdots.pdf", (phonology_runs_plt | morphology_runs_plt | syntax_runs_plt) ,height=3,width=10)


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

morphology_runs_variance <- morphology_filter %>%
  group_by(K) %>%
  #calculate the variance between runs for each K
  summarise(variance_among_runs = var(meanLnLikelihood))

morphology_K_variance <- morphology_filter %>%
  group_by(Run) %>%
  #calculate the variance between runs for each K
  summarise(variance_among_Ks = var(meanLnLikelihood))

# Syntax
syntax_runs_variance <- syntax_filter %>%
  group_by(K) %>%
  #calculate the variance between runs for each K
  summarise(variance_among_runs = var(meanLnLikelihood))

syntax_K_variance <- morphology_filter %>%
  group_by(Run) %>%
  #calculate the variance between runs for each K
  summarise(variance_among_Ks = var(meanLnLikelihood))

mean(phonology_K_variance$variance_among_Ks) - mean(phonology_runs_variance$variance_among_runs)

mean(morphology_K_variance$variance_among_Ks) - mean(morphology_runs_variance$variance_among_runs)

mean(syntax_K_variance$variance_among_Ks) - mean(syntax_runs_variance$variance_among_runs)

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

morphology_grouped_by_K <- outfile %>%
  filter(level == "morphology") %>%
  group_by(K) %>%
  summarise(median_meanLnLikelihood_across_runs = median(meanLnLikelihood))

morphology_plt <- ggplot(morphology_grouped_by_K, aes(x=K,y=median_meanLnLikelihood_across_runs))+
  geom_point()

morphology_plt

# Inspect syntax

syntax_grouped_by_K <- outfile %>%
  filter(level == "syntax") %>%
  group_by(K) %>%
  summarise(median_meanLnLikelihood_across_runs = median(meanLnLikelihood))

syntax_plt <- ggplot(syntax_grouped_by_K, aes(x=K,y=median_meanLnLikelihood_across_runs))+
  geom_point()

syntax_plt

