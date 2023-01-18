
library(tidyverse)
library(table1)
library(effectsize)
library(pwr)

# data
# N = 16
df <- tibble(
  ID = 1:16,
  age = c(18, 18, 20, 20, 21, 21, 21, 23, 24, 24, 26, 29, 29, 31, 31, 32),
  left_arm = c(9.5, 13.9, 10.4, 11.1, 9.8, 10.4, 12.3, 12.9,
             12.5, 14.0, 12.2, 11.7, 12.4, 12.1, 14.2, 14.8),
  right_arm = c(15.3, 17.2, 16.1, 19.5, 13.9, 15.9, 16.1, 19.6,
             15.4, 20.3, 19.6, 17.6, 20.9, 17.4, 24.1, 25.2)
)


# summary
label(df$left_arm) <- "left arm"
label(df$right_arm) <- "right arm"

units(df$age) <- "years"
units(df$left_arm) <- "m/s^2"
units(df$right_arm) <- "m/s^2"

table1(~ age + left_arm + right_arm, data = df)

# Figure 3
df %>%
  pivot_longer(cols = c("left_arm", "right_arm"),
               names_to = "arm",
               values_to = "speed") %>%
  ggplot(aes(x = age, y = speed, color = arm)) +
  geom_point(size = 4) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "Age (years)", y = expression(paste("Speed ", (m/s)^2))) +
  scale_x_continuous(breaks = seq(18,32,by = 1)) +
  theme_bw() +
  theme(legend.position = "top",
        legend.title = element_blank())

# Boxplot - speed / arm
df %>%
  pivot_longer(cols = c("left_arm", "right_arm"),
               names_to = "arm",
               values_to = "speed") %>%
  ggplot(aes(x = arm, y = speed)) +
  geom_boxplot() +
  labs(x = "Age (years)", y = expression(paste("Speed ", (m/s)^2))) +
  scale_x_discrete(breaks = c("left_arm", "right_arm"),
                   labels = c("left", "right")) +
  theme_bw()


# Paired t-test
res_t_test <- t.test(x = df$left_arm,
                     y = df$right_arm,
                     alternative = "two.sided",
                     paired = TRUE)
res_t_test$conf.int

# Effect size - cohen d
cohens_d(x = df$left_arm,
         y = df$right_arm,
         pooled_sd = TRUE,
         paired = TRUE)

# post-hoc power analysis
pwr.t.test(
  n = 16,
  d = 2.80,
  sig.level = 0.05,
  type = "paired",
  alternative = "two.sided"
)


