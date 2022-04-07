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

# LnLikelihood

lnlikelihood <- function(data_set) {
  data_set %>%
    group_by(K) %>%
    ggplot(aes(meanLnLikelihood, color = factor(level, level = level_ordered))) +
    stat_summary(
      mapping = aes(x = factor(K), y = meanLnLikelihood),
      fun.min = min,
      fun.max = max,
      fun = median
    ) +
    labs(x = "K", 
         title = "Mean LnLikelihood") +
    facet_wrap(.~factor(level, level = level_ordered), nrow = 3, scales = "free") +
    theme(strip.background = element_blank(), strip.text.x = element_blank(), axis.title.y = element_blank(), legend.position="none")
}

# (A) mean LnProbData
a_way <- function(data_set) {
  data_set %>%
    group_by(K) %>%
    ggplot(aes(LnProbData, color = factor(level, level = level_ordered))) +
    stat_summary(
      mapping = aes(x = factor(K), y = LnProbData),
      fun.min = min,
      fun.max = max,
      fun = median
    ) +
    labs(x = "K", 
         title = "A metric: L(K)") +
    facet_wrap(.~factor(level, level = level_ordered), nrow = 3, scales = "free") +
    theme(strip.background = element_blank(), strip.text.x = element_blank(), axis.title.y = element_blank(), legend.position="none")
}

# (B) L′(K) = L(K) – L(K – 1)
b_way <- function(data_set) {
  data_set %>%
    arrange(Run, K) %>%
    mutate(L_diff = LnProbData - lag(LnProbData)) %>%
    ggplot(aes(L_diff , color = factor(level, level = level_ordered))) +
    stat_summary(
      mapping = aes(x = factor(K), y = L_diff),
      fun.min = min,
      fun.max = max,
      fun = median
    ) +
    labs(x = "K",
         title = "B metric: L'(K)") +
    facet_wrap(.~factor(level, level = level_ordered), nrow = 3, scales = "free") +
    theme(strip.background = element_blank(), strip.text.x = element_blank(), axis.title.y = element_blank(), legend.position="none")
}

# (C) L′′(K)| = |L′(K + 1) – L′(K)
c_way <- function(data_set) {
  data_set %>%
  arrange(Run, K) %>%
  mutate(L_diff = abs(lead(LnProbData) - LnProbData)) %>%
  ggplot(aes(L_diff, color = factor(level, level = level_ordered))) +
  stat_summary(
    mapping = aes(x = factor(K), y = L_diff),
    fun.min = min,
    fun.max = max,
    fun = median
  ) +
  labs(x = "K",
       title = expression(paste('C metric: | L"K |'))) +
    facet_wrap(.~factor(level, level = level_ordered), nrow = 3, scales = "free") +
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
  ggplot(aes(factor(K), delta_k, color = factor(level, level = level_ordered))) +
  geom_line(aes(group = 1)) +
  geom_point() +
  labs(x = "K", 
       #title = expression(paste("D metric: ", Delta, "K = m(| L′′(K) |) / s[L(K)]"))) +
        title = expression(paste("D metric: ", Delta, "K"))) +
  facet_wrap(.~factor(level, level = level_ordered), nrow = 3, scales = "free") +
  theme(strip.background = element_blank(), strip.text.x = element_blank(), axis.title.y = element_blank()) +
  scale_colour_discrete(name="Level")
}

lnlikelihood_plot <- lnlikelihood(outfile)
a_way_plot <- a_way(outfile)
b_way_plot <- b_way(outfile)
c_way_plot <- c_way(outfile)
d_way_plot <- d_way(outfile)

ggsave("plots/evanno.pdf", (lnlikelihood_plot | a_way_plot | b_way_plot | c_way_plot | d_way_plot),
       height = 8, width = 15)

