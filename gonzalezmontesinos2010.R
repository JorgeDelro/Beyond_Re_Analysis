
library(tidyverse)
library(effectsize)
library(pwr)

# data
# N = 24
df <- tibble(
  IE_Bosco = rnorm(n = 24,
                   mean = 0.0445,
                   sd = 0.0299),
  IE_Libre = rnorm(n = 24,
                   mean = 0.0825,
                   sd = 0.0708),
)

## Correlation - Pearson´s r
cor(df$IE_Bosco, df$IE_Libre)

## plot linear relation
df %>%
ggplot(aes(x = IE_Libre, y = IE_Bosco)) +
  geom_point() +
  geom_smooth(method='lm') +
  theme_bw()

# Paired t-test
res_t_test <- t.test(x = df$IE_Bosco,
                     y = df$IE_Libre,
                     alternative = "two.sided",
                     paired = TRUE)
res_t_test

# Effect size - cohen d
cohens_d(x = df$IE_Bosco,
         y = df$IE_Libre,
         pooled_sd = TRUE,
         paired = TRUE)

# post-hoc power analysis
pwr.t.test(
  n = 24,
  d = 0.71,
  sig.level = 0.05,
  type = "paired",
  alternative = "two.sided"
)

