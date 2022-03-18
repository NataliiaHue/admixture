# Which K value is the true K value?
# Evanno:
# Description of the four steps for the graphical method
# allowing detection of the true number of groups K*.
# (A) Mean L(K) (± SD) over 20 runs for each K value.
# The model considered here is a hierarchical island model
# using all 100 individuals per population and 50 AFLP loci.
# (B) Rate of change of the likelihood distribution (mean ± SD)
# calculated as L′(K) = L(K) – L(K – 1).
# (C) Absolute values of the second order rate of change
# of the likelihood distribution (mean ± SD)
# calculated according to the formula:
# |L′′(K)| = |L′(K + 1) – L′(K)|.
# (D) ∆K calculated as ∆K = m|L′′(K)|/ s[L(K)].
# The modal value of this distribution is the true K(*)
# or the uppermost level of structure, here five clusters.

library(ggplot2)
library(dplyr)
library(patchwork) 

outfile = read.csv("outfile.txt",sep=";")

# Split outfile into phonology, morphology and syntax

phonology_filter <- outfile %>%
  filter(level == "phonology")

morphology_filter <- outfile %>%
  filter(level == "morphology")

syntax_filter <- outfile %>%
  filter(level == "syntax")

# (A) mean lnLikelihood
a_way_phonology <- phonology_filter %>%
  group_by(K) %>%
  ggplot(aes(LnProbData)) +
  stat_summary(
    mapping = aes(x = factor(K), y = LnProbData),
    fun.min = min,
    fun.max = max,
    fun = median
  ) +
  labs(title = "Phonology", 
       x = "K (number of groups in the data)", 
       y = "A metric")

a_way_phonology

# (B) L′(K) = L(K) – L(K – 1)
b_way_phonology <- phonology_filter %>%
  group_by(K) %>%
  #summarise(meanLnLikelihood_per_K = mean(meanLnLikelihood)) %>%
  #calculate the variance between runs for each K
  arrange(Run, K) %>%
  mutate(L_diff = LnProbData - lag(LnProbData)) %>%
  ggplot(aes(L_diff)) +
  stat_summary(
    mapping = aes(x = factor(K), y = L_diff),
    fun.min = min,
    fun.max = max,
    fun = median
  ) +
  labs(title = "Phonology",
       x = "K (number of groups in the data)",
       y = "B metric")

b_way_phonology

# (C) L′′(K)| = |L′(K + 1) – L′(K)

# (D) ∆K = m|L′′(K)|/ s[L(K)] 
# the mean of the absolute values of L′′(K)
# averaged over 20 runs divided by the standard deviation of L(K)

d_way_phonology <- phonology_filter %>%
  group_by(K) %>%
  summarise(mean_likelihood = mean(abs(meanLnLikelihood)), mean_variance = mean(varianceLnLikelihood)) %>%
  #calculate the variance between runs for each K
  mutate(delta_k = mean_likelihood/sqrt(mean_variance)) %>%
  ggplot(aes(delta_k)) +
  stat_summary(
    mapping = aes(x = factor(K), y = delta_k),
    fun.min = min,
    fun.max = max,
    fun = median
  ) +
  labs(title = "Phonology", 
       x = "K (number of groups in the data)", 
       y = "Delta K metric")

d_way_phonology

par(mfrow=c(2,2))
a_way_phonology

d_way_morphology <- morphology_filter %>%
  group_by(K) %>%
  summarise(mean_likelihood = mean(abs(meanLnLikelihood)), mean_variance = mean(varianceLnLikelihood)) %>%
  #calculate the variance between runs for each K
  mutate(delta_k = mean_likelihood/sqrt(mean_variance)) %>%
  ggplot(aes(delta_k)) +
  stat_summary(
    mapping = aes(x = factor(K), y = delta_k),
    fun.min = min,
    fun.max = max,
    fun = median
  ) +
  labs(title = "Morphology", 
       x = "K (number of groups in the data)", 
       y = "Delta K metric")

d_way_morphology

d_way_syntax <- syntax_filter %>%
  group_by(K) %>%
  summarise(mean_likelihood = mean(abs(meanLnLikelihood)), 
            mean_variance = mean(varianceLnLikelihood)) %>%
  #calculate the variance between runs for each K
  mutate(delta_k = mean_likelihood/sqrt(mean_variance)) %>%
  ggplot(aes(delta_k)) +
  stat_summary(
    mapping = aes(x = factor(K), y = delta_k),
    fun.min = min,
    fun.max = max,
    fun = median
  ) +
  labs(title = "Syntax", 
       x = "K (number of groups in the data)", 
       y = "Delta K metric")

d_way_syntax