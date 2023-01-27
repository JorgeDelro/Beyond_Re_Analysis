
library(tidyverse)
library(ggpmisc) # Add regression equation and R2 to ggplot
library(ggpubr) # ggarrange

# NOTE: original analyses with 66 participants

# Data base
df <- read.csv2("martinezmarti_2016_db")
glimpse(df)

# Pressure sensor - PreECnsole
# Accelerometer - AccECnsole
# Laser - SJS
# high-speed motion capture system - HSC

# Figure 4 - Simple linear regression
# HSC ~ AccECnsole
plot_HSC_AccECnsole <- df %>%
  select(VidSJ1, VidCMJ1, VidABK1,
         AceSJ1, AceCMJ1, AceABK1) %>%
  rename(SJ.HSC = VidSJ1, CMJ.HSC = VidCMJ1, Abk.HSC = VidABK1,
         SJ.AccECnsole = AceSJ1, CMJ.AccECnsole = AceCMJ1, Abk.AccECnsole = AceABK1) %>%
  pivot_longer(everything(), names_to = c('test', '.value'),
               names_sep = '\\.') %>%
  mutate(test = factor(test, levels = c("SJ", "CMJ", "Abk"))) %>%
  ggplot(aes(x = AccECnsole, y = HSC, shape = test, linetype = test, grp.label = test)) +
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(grp.label), "*\": \"*",
                                 after_stat(eq.label), "*\", \"*",
                                 after_stat(rr.label), sep = ""))) +
  geom_point(size = 3) +
  scale_shape_manual(values = c(1, 16, 2)) +
  xlab("AccECnsole (cm)") +
  ylab("HSC (cm)") +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom")

# HSC ~ PreECnsole
plot_HSC_PreECnsole <- df %>%
  select(VidSJ1, VidCMJ1, VidABK1,
         PresSJ1, PresCMJ1, PresABK1) %>%
  rename(SJ.HSC = VidSJ1, CMJ.HSC = VidCMJ1, Abk.HSC = VidABK1,
         SJ.PreECnsole = PresSJ1, CMJ.PreECnsole = PresCMJ1, Abk.PreECnsole = PresABK1) %>%
  pivot_longer(everything(), names_to = c('test', '.value'),
               names_sep = '\\.') %>%
  mutate(test = factor(test, levels = c("SJ", "CMJ", "Abk"))) %>%
  ggplot(aes(x = PreECnsole, y = HSC, shape = test, linetype = test, grp.label = test)) +
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(grp.label), "*\": \"*",
                                 after_stat(eq.label), "*\", \"*",
                                 after_stat(rr.label), sep = ""))) +
  geom_point(size = 3) +
  scale_shape_manual(values = c(1, 16, 2)) +
  xlab("PreECnsole (cm)") +
  ylab("HSC (cm)") +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom")

# SJS ~ AccECnsole
plot_SJS_AccECnsole <- df %>%
  select(LasSJ1, LasCMJ1, LasABK1,
         AceSJ1, AceCMJ1, AceABK1) %>%
  rename(SJ.SJS = LasSJ1, CMJ.SJS = LasCMJ1, Abk.SJS = LasABK1,
         SJ.AccECnsole = AceSJ1, CMJ.AccECnsole = AceCMJ1, Abk.AccECnsole = AceABK1) %>%
  pivot_longer(everything(), names_to = c('test', '.value'),
               names_sep = '\\.') %>%
  mutate(test = factor(test, levels = c("SJ", "CMJ", "Abk"))) %>%
  ggplot(aes(x = AccECnsole, y = SJS, shape = test, linetype = test, grp.label = test)) +
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(grp.label), "*\": \"*",
                                 after_stat(eq.label), "*\", \"*",
                                 after_stat(rr.label), sep = ""))) +
  geom_point(size = 3) +
  scale_shape_manual(values = c(1, 16, 2)) +
  xlab("AccECnsole (cm)") +
  ylab("SJS (cm)") +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom")


# SJS ~ PreECnsole
plot_SJS_PreECnsole <- df %>%
  select(LasSJ1, LasCMJ1, LasABK1,
         PresSJ1, PresCMJ1, PresABK1) %>%
  rename(SJ.SJS = LasSJ1, CMJ.SJS = LasCMJ1, Abk.SJS = LasABK1,
         SJ.PreECnsole = PresSJ1, CMJ.PreECnsole = PresCMJ1, Abk.PreECnsole = PresABK1) %>%
  pivot_longer(everything(), names_to = c('test', '.value'),
               names_sep = '\\.') %>%
  mutate(test = factor(test, levels = c("SJ", "CMJ", "Abk"))) %>%
  ggplot(aes(x = PreECnsole, y = SJS, shape = test, linetype = test, grp.label = test)) +
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(grp.label), "*\": \"*",
                                 after_stat(eq.label), "*\", \"*",
                                 after_stat(rr.label), sep = ""))) +
  geom_point(size = 3) +
  scale_shape_manual(values = c(1, 16, 2)) +
  xlab("PreECnsole (cm)") +
  ylab("SJS (cm)") +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom")

ggarrange(
  plot_HSC_AccECnsole,
  plot_HSC_PreECnsole,
  plot_SJS_AccECnsole,
  plot_SJS_PreECnsole,
  ncol = 2,
  nrow = 2
)

ggsave("figure_4.png", width = 11, height = 8, dpi = 600)

# ANOVA
df %>%
  select(LasSJ1, PresSJ1, AceSJ1, VidSJ1,
         LasCMJ1, PresCMJ1, AceCMJ1, VidCMJ1,
         LasABK1, PresABK1, AceABK1, VidABK1) %>%
  rename(SJ.HSC = VidSJ1, CMJ.HSC = VidCMJ1, Abk.HSC = VidABK1,
         SJ.AccECnsole = AceSJ1, CMJ.AccECnsole = AceCMJ1, Abk.AccECnsole = AceABK1,
         SJ.SJS = LasSJ1, CMJ.SJS = LasCMJ1, Abk.SJS = LasABK1,
         SJ.PreECnsole = PresSJ1, CMJ.PreECnsole = PresCMJ1, Abk.PreECnsole = PresABK1)

# Bland-Altman

