# analyse the output of structure - choose best K etc.

library(ggplot2)
library(dplyr)
library(patchwork) 

outfile = read.csv("data/outfile_samples.txt",sep=";")

theme_set(theme_classic())

plt_likelihood <- ggplot(data = outfile) + 
  stat_summary(
    mapping = aes(x = factor(K), y = meanLnLikelihood),
    fun.min = min,
    fun.max = max,
    fun = median
  ) +
  labs(x = "K (number of groups in the data)", y = "Mean LnLikelihood")
plt_likelihood
# Plateaus at 3

plt_prob <- ggplot(data = outfile) + 
  stat_summary(
    mapping = aes(x = factor(K), y = LnProbData),
    fun.min = min,
    fun.max = max,
    fun = median
  ) +
  labs(x = "K (number of groups in the data)", y = "Log probability of data")
plt_prob

# Calculate the jump like:

#diffs <- syntax_grouped_by_K %>%
#  mutate(median_meanLnLikelihood_across_runs - lag(median_meanLnLikelihood_across_runs)) %>%
#  na.omit() 

# The jump from 5 to 6 is too low compared to jumps before 5, therefore the best K is 5.

ggsave("plots/variation-across-runs-bars-all-levels.pdf", (plt) ,height=3,width=4)

dots_plt <- ggplot(outfile, aes(x=factor(K),y=meanLnLikelihood))+
  geom_point(alpha=0.5) +
  labs(x = "K (number of groups in the data)", y = "Mean LnLikelihood")

dots_plt

ggsave("plots/variation-across-runs-dots.pdf", (dots_plt) ,height=3,width=4)

# Find the variance among runs and Ks

runs_variance <- outfile %>%
  group_by(K) %>%
  #calculate the variance between runs for each K
  summarise(variance_among_runs = var(meanLnLikelihood))

k_variance <- outfile %>%
  group_by(Run) %>%
  #calculate the variance between runs for each K
  summarise(variance_among_Ks = var(meanLnLikelihood))

mean(runs_variance$variance_among_Ks) - mean(k_variance$variance_among_runs)

# is the variance of meanLnLikelihood between Ks is greater than the variance between runs?

