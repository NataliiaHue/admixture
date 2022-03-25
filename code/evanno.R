# Which K value is the true K value?
# Evanno 2005:
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

outfile <- read.csv("data/outfile.txt",sep=";")

level_ordered <- c("phonology", "morphology", "syntax")

theme_set(theme_bw())

# (A) mean LnProbData
a_way <- function(data_set) {
  data_set %>%
    group_by(K) %>%
    ggplot(aes(LnProbData, color = level)) +
    stat_summary(
      mapping = aes(x = factor(K), y = LnProbData),
      fun.min = min,
      fun.max = max,
      fun = median
    ) +
    labs(x = "K", 
         title = "A metric") +
    facet_wrap(.~factor(level, level = level_ordered), nrow = 3) +
    theme(strip.background = element_blank(), strip.text.x = element_blank(), axis.title.y = element_blank(), legend.position="none")
}

# (B) L′(K) = L(K) – L(K – 1)
b_way <- function(data_set) {
  data_set %>%
    arrange(Run, K) %>%
    mutate(L_diff = LnProbData - lag(LnProbData)) %>%
    ggplot(aes(L_diff , color = level)) +
    stat_summary(
      mapping = aes(x = factor(K), y = L_diff),
      fun.min = min,
      fun.max = max,
      fun = median
    ) +
    labs(x = "K",
         title = "B metric") +
    facet_wrap(.~factor(level, level = level_ordered), nrow = 3) +
    theme(strip.background = element_blank(), strip.text.x = element_blank(), axis.title.y = element_blank(), legend.position="none")
}

# (C) L′′(K)| = |L′(K + 1) – L′(K)
c_way <- function(data_set) {
  data_set %>%
  arrange(Run, K) %>%
  mutate(L_diff = abs(lead(LnProbData) - LnProbData)) %>%
  ggplot(aes(L_diff, color = level)) +
  stat_summary(
    mapping = aes(x = factor(K), y = L_diff),
    fun.min = min,
    fun.max = max,
    fun = median
  ) +
  labs(x = "K",
       title = "C metric") +
    facet_wrap(.~factor(level, level = level_ordered), nrow = 3) +
    theme(strip.background = element_blank(), strip.text.x = element_blank(), axis.title.y = element_blank(), legend.position="none")
}

# (D) ∆K = m|L′′(K)|/ s[L(K)] 
# the mean of the absolute values of L′′(K)
# averaged over 20 runs divided by the standard deviation of L(K)
# expands as: m(|L(K + 1) − 2 L(K ) + L(K − 1)|)/s[L(K )] 
d_way <- function(data_set) {
  data_set %>%
  group_by(level, K) %>%
  summarise(mean_lnprobdata = mean(abs(LnProbData)), sd_lnprobdata = sd(LnProbData)) %>%
  # calculate the variance between runs for each K
  # mutate(delta_k = mean_lnprobdata/sd_lnprobdata) %>%
  mutate(delta_k = (lead(mean_lnprobdata) - 2 * mean_lnprobdata + lag(mean_lnprobdata)) / sd_lnprobdata)%>%
  ggplot(aes(factor(K), delta_k, color = level)) +
  geom_line(aes(group = 1)) +
  geom_point() +
  labs(x = "K", 
       title = "D metric") +
  facet_wrap(.~factor(level, level = level_ordered), nrow = 3) +
  theme(strip.background = element_blank(), strip.text.x = element_blank(), axis.title.y = element_blank())
}

a_way <- a_way(outfile)
b_way <- b_way(outfile)
c_way <- c_way(outfile)
d_way <- d_way(outfile)


a_way
b_way
c_way
d_way

ggsave("plots/evanno.pdf", (a_way | b_way | c_way | d_way),
       height = 8, width = 10)

