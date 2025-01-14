
# Libraries
library(tidyverse)
library(brms)
library(emmeans)
library(ggthemes)
library(gridExtra)

# Read database
df <- read_csv2("arnedillo2020_db.csv")

# Prepare databases for TM6M / PIM / PEM / CAT / DYSNEA

# TM6M
df_TM6M <- df |>
  select("ID","Condition", "SixMWT_1", "SixMWT_2") |>
  pivot_longer(c("SixMWT_1", "SixMWT_2"), names_to = "Time", values_to = "Value") |>
  mutate(
    Condition = factor(Condition, levels = c("FB", "ONB", "Control"), labels = c("FB", "ONB", "Control")),
    Time = factor(Time, levels = c("SixMWT_1", "SixMWT_2"), labels = c("Pre", "Post"))
  )

# PIM
df_PIM <- df |>
  select("ID","Condition", "PIM_pre", "PIM_post") |>
  pivot_longer(c("PIM_pre", "PIM_post"), names_to = "Time", values_to = "Value") |>
  mutate(
    Condition = factor(Condition, levels = c("FB", "ONB", "Control"), labels = c("FB", "ONB", "Control")),
    Time = factor(Time, levels = c("PIM_pre", "PIM_post"), labels = c("Pre", "Post"))
  )

# PEM
df_PEM <- df |>
  select("ID","Condition", "PEM_pre", "PEM_post") |>
  pivot_longer(c("PEM_pre", "PEM_post"), names_to = "Time", values_to = "Value") |>
  mutate(
    Condition = factor(Condition, levels = c("FB", "ONB", "Control"), labels = c("FB", "ONB", "Control")),
    Time = factor(Time, levels = c("PEM_pre", "PEM_post"), labels = c("Pre", "Post"))
  )

# CAT
df_CAT <- df |>
  select("ID","Condition", "CAT_1", "CAT_2") |>
  pivot_longer(c("CAT_1", "CAT_2"), names_to = "Time", values_to = "Value") |>
  mutate(
    Condition = factor(Condition, levels = c("FB", "ONB", "Control"), labels = c("FB", "ONB", "Control")),
    Time = factor(Time, levels = c("CAT_1", "CAT_2"), labels = c("Pre", "Post"))
  )

# DYSNEA
df_DYSNEA <- df |>
  select("ID","Condition", "mMRT_pre", "mMRT_post") |>
  pivot_longer(c("mMRT_pre", "mMRT_post"), names_to = "Time", values_to = "Value") |>
  mutate(
    Condition = factor(Condition, levels = c("FB", "ONB", "Control"), labels = c("FB", "ONB", "Control")),
    Time = factor(Time, levels = c("mMRT_pre", "mMRT_post"), labels = c("Pre", "Post"))
  )

#######################################################################

## Fit Models

# TM6M
TM6M_mod <- brm(Value ~ Condition * Time + (1 | ID),
            data = df_TM6M, family = student(),
            cores=4, chains=4, sample_prior=TRUE)

### Results
summary(TM6M_mod)
### Interaction plot
plot(marginal_effects(TM6M_mod, effects = "Condition"))
### Posterior Predictive Check
pp_check(TM6M_mod, nsamples = 100)
#### Marginal means
emmeans(TM6M_mod, pairwise ~ Time | Condition)
emmip(TM6M_mod, Condition ~ Time)

PIM_mod <- brm(Value ~ Condition * Time + (1 | ID),
           data = df_PIM, family = student(),
           cores=4, chains=4, sample_prior=TRUE)
### Results
summary(PIM_mod)
### Interaction plot
plot(marginal_effects(PIM_mod, effects = "Condition"))
### Posterior Predictive Check
pp_check(PIM_mod, nsamples = 100)
#### Marginal means
emmeans(PIM_mod, pairwise ~ Time | Condition)
emmip(PIM_mod, Condition ~ Time)

PEM_mod <- brm(Value ~ Condition * Time + (1 | ID),
               data = df_PEM, family = student(),
               cores=4, chains=4, sample_prior=TRUE)
### Results
summary(PEM_mod)
### Interaction plot
plot(marginal_effects(PEM_mod, effects = "Condition"))
### Posterior Predictive Check
pp_check(PEM_mod, nsamples = 100)
#### Marginal means
emmeans(PEM_mod, pairwise ~ Time | Condition)
emmip(PEM_mod, Condition ~ Time)

CAT_mod <- brm(Value ~ Condition * Time + (1 | ID),
               data = df_CAT, family = student(),
               cores=4, chains=4, sample_prior=TRUE)
### Results
summary(CAT_mod)
### Interaction plot
plot(marginal_effects(CAT_mod, effects = "Condition"))
### Posterior Predictive Check
pp_check(CAT_mod, nsamples = 100)
#### Marginal means
emmeans(CAT_mod, pairwise ~ Time | Condition)
emmip(CAT_mod, Condition ~ Time)

DYSNEA_mod <- brm(Value ~ Condition * Time + (1 | ID),
               data = df_DYSNEA, family = student(),
               cores=4, chains=4, sample_prior=TRUE)
### Results
summary(DYSNEA_mod)
### Interaction plot
plot(marginal_effects(DYSNEA_mod, effects = "Condition"))
### Posterior Predictive Check
pp_check(DYSNEA_mod, nsamples = 100)
#### Marginal means
emmeans(DYSNEA_mod, pairwise ~ Time | Condition)
emmip(DYSNEA_mod, Condition ~ Time)

###########################################################

# Create a plot to display results

# Custom function to get data for plotting
make_data_plot <- function(db, brmsmodel) {

  ME <- marginal_effects(brmsmodel)

  ## Control-PRE - nº 4
  Control_pre_estimate <- rep(ME$`Condition:Time`$estimate__[1], 4)
  Control_pre_lower <- rep(ME$`Condition:Time`$lower__[1], 4)
  Control_pre_upper <- rep(ME$`Condition:Time`$upper__[1], 4)

  ## Control-POST
  Control_post_estimate <- rep(ME$`Condition:Time`$estimate__[2], 4)
  Control_post_lower <- rep(ME$`Condition:Time`$lower__[2], 4)
  Control_post_upper <- rep(ME$`Condition:Time`$upper__[2], 4)

  ## ONB-PRE - nº 5
  ONB_pre_estimate <- rep(ME$`Condition:Time`$estimate__[3], 5)
  ONB_pre_lower <- rep(ME$`Condition:Time`$lower__[3], 5)
  ONB_pre_upper <- rep(ME$`Condition:Time`$upper__[3], 5)

  ## ONB-POST
  ONB_post_estimate <- rep(ME$`Condition:Time`$estimate__[4], 5)
  ONB_post_lower <- rep(ME$`Condition:Time`$lower__[4], 5)
  ONB_post_upper <- rep(ME$`Condition:Time`$upper__[4], 5)

  ## FB-PRE - nº 7
  FB_pre_estimate <- rep(ME$`Condition:Time`$estimate__[5], 7)
  FB_pre_lower <- rep(ME$`Condition:Time`$lower__[5], 7)
  FB_pre_upper <- rep(ME$`Condition:Time`$upper__[5], 7)

  ## FB-POST
  FB_post_estimate <- rep(ME$`Condition:Time`$estimate__[6], 7)
  FB_post_lower <- rep(ME$`Condition:Time`$lower__[6], 7)
  FB_post_upper <- rep(ME$`Condition:Time`$upper__[6], 7)

  ## bd order - PRE: FB, ONB, Control. POST: FB, ONB, Control
  Estimate <- c(FB_pre_estimate, ONB_pre_estimate, Control_pre_estimate, FB_post_estimate, ONB_post_estimate, Control_post_estimate)
  Lower <- c(FB_pre_lower, ONB_pre_lower, Control_pre_lower, FB_post_lower, ONB_post_lower, Control_post_lower)
  Upper <- c(FB_pre_upper, ONB_pre_upper, Control_pre_upper, FB_post_upper, ONB_post_upper, Control_post_upper)

  db_new <- db %>% add_column(Estimate, Lower, Upper)

  return(db_new)
}

# TM6M
#### 6MWT
bd_TM6M_new <- make_data_plot(db = bd_TM6M, brmsmodel = TM6M)
ME_TM6M <- marginal_effects(TM6M)
ME_TM6M$`Condition:Time`$estimate__[2]
TM6M_Estimate <- bd_TM6M_new$Estimate
TM6M_Lower <- bd_TM6M_new$Lower
TM6M_Upper <- bd_TM6M_new$Upper

TM6M_plot <- bd_TM6M_new %>%
  select(Sujeto, Condition, Time, Value) %>%
  ggplot(aes(x = Condition, y = Value, group = interaction(Condition, Time), color = Time)) +
  geom_point(position = position_dodge(0.9)) +
  geom_point(aes(y = TM6M_Estimate), position = position_dodge(0.5), shape = 15, size = 5) +
  geom_linerange(aes(ymin= TM6M_Lower, ymax= TM6M_Upper), position = position_dodge(0.5)) +
  geom_segment(aes(x = 0.87, y = ME_TM6M$`Condition:Time`$estimate__[1], xend = 1.13, yend = ME_TM6M$`Condition:Time`$estimate__[2]),
               size=0.5, color="black", arrow = arrow()) +
  geom_segment(aes(x = 1.87, y = ME_TM6M$`Condition:Time`$estimate__[3], xend = 2.13, yend = ME_TM6M$`Condition:Time`$estimate__[4]),
               size=0.5, color = "black", arrow = arrow()) +
  geom_segment(aes(x = 2.87, y = ME_TM6M$`Condition:Time`$estimate__[5], xend = 3.13, yend = ME_TM6M$`Condition:Time`$estimate__[6]),
               size=0.5, color = "black", arrow = arrow()) +
  ylab("6MWT (m)") +
  #facet_grid(. ~ Condition, scales="free_y") +
  theme_bw() +
  theme(legend.position="top",
        legend.title = element_blank(),
        legend.spacing.x = unit(0.5, 'cm'),
        axis.text=element_text(size=12),
        axis.title.y = element_text(size = 12,  margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(size = 12,  margin = margin(t = 10, r = 20, b = 0, l = 0)))


TM6M_plot <- TM6M_plot + annotate("text", x = 2, y = 480, parse = TRUE, size = 7, label = as.character(expression(""^{'$'})))
TM6M_plot <- TM6M_plot + annotate("text", x = 3, y = 530, parse = TRUE, size = 7, label = as.character(expression(""^{'$'})))

Delta_TM6M <- c("19.6", "30.7 ", "65.0")
Delta_TM6M_2 <- c("(-8.3, 46.8)", "(3.8, 54.6)", "(39.9, 88.5)")
Delta_TM6M_3 <- rbind(Delta_TM6M, Delta_TM6M_2)
Delta_TM6M_4 <- tableGrob(d = Delta_TM6M_3,
                          rows = c(expression(Delta), "95% CrI"),
                          cols = NULL)
Delta_TM6M_4$widths <- unit(rep(0.35, ncol(Delta_TM6M_4)), "npc")

TM6M_plot <- cowplot::plot_grid(TM6M_plot,
                                arrangeGrob(Delta_TM6M_4, nullGrob(), widths=c(25,6)),
                                align = "v",
                                ncol=1,
                                axis = "b",
                                rel_heights = c(9/10,1/10))

###########

# PIM
bd_PIM_new <- make_data_plot(db = bd_PIM, brmsmodel = PIM)
ME_PIM <- marginal_effects(PIM)
ME_PIM$`Condition:Time`$estimate__[2]
PIM_Estimate <- bd_PIM_new$Estimate
PIM_Lower <- bd_PIM_new$Lower
PIM_Upper <- bd_PIM_new$Upper

PIM_plot <- bd_PIM_new %>%
  select(Sujeto, Condition, Time, Value) %>%
  ggplot(aes(x = Condition, y = Value, group = interaction(Condition, Time), color = Time)) +
  geom_point(position = position_dodge(0.9)) +
  geom_point(aes(y = PIM_Estimate), position = position_dodge(0.5), shape = 15, size = 5) +
  geom_linerange(aes(ymin= PIM_Lower, ymax= PIM_Upper), position = position_dodge(0.5)) +
  geom_segment(aes(x = 0.87, y = ME_PIM$`Condition:Time`$estimate__[1], xend = 1.13, yend = ME_PIM$`Condition:Time`$estimate__[2]),
               size=0.5, color="black", arrow = arrow()) +
  geom_segment(aes(x = 1.87, y = ME_PIM$`Condition:Time`$estimate__[3], xend = 2.13, yend = ME_PIM$`Condition:Time`$estimate__[4]),
               size=0.5, color = "black", arrow = arrow()) +
  geom_segment(aes(x = 2.87, y = ME_PIM$`Condition:Time`$estimate__[5], xend = 3.13, yend = ME_PIM$`Condition:Time`$estimate__[6]),
               size=0.5, color = "black", arrow = arrow()) +
  ylab(expression(P[Imax]*" (mmHg)")) +
  #facet_grid(. ~ Condition, scales="free_y") +
  theme_bw() +
  theme(legend.position="top",
        legend.title = element_blank(),
        legend.spacing.x = unit(0.5, 'cm'),
        axis.text=element_text(size=12),
        axis.title.y = element_text(size = 12,  margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(size = 12,  margin = margin(t = 10, r = 20, b = 0, l = 0)))

#PIM_plot <- PIM_plot + annotate("text", x = 2, y = 95, parse = TRUE, size = 7, label = as.character(expression(""^{'$'})))
PIM_plot <- PIM_plot + annotate("text", x = 3, y = 125, parse = TRUE, size = 7, label = as.character(expression(""^{'$'})))


Delta_PIM <- c("-3.2", "2.3", "29.1")
Delta_PIM_2 <- c("(-21.0, 13.6)", "(-15.5, 17.9)", "(13.8, 43.2)")
Delta_PIM_3 <- rbind(Delta_PIM, Delta_PIM_2)
Delta_PIM_4 <- tableGrob(d = Delta_PIM_3,
                         rows = c(expression(Delta), "95% CrI"),
                         cols = NULL)
Delta_PIM_4$widths <- unit(rep(0.35, ncol(Delta_PIM_4)), "npc")

PIM_plot <- cowplot::plot_grid(PIM_plot,
                               arrangeGrob(Delta_PIM_4, nullGrob(), widths=c(25,6)),
                               align = "v",
                               ncol=1,
                               axis = "b",
                               rel_heights = c(9/10,1/10))

#########

# CAT
bd_CAT_new <- make_data_plot(db = bd_CAT, brmsmodel = CAT)
ME_CAT <- marginal_effects(CAT)
ME_CAT$`Condition:Time`$estimate__[2]
CAT_Estimate <- bd_CAT_new$Estimate
CAT_Lower <- bd_CAT_new$Lower
CAT_Upper <- bd_CAT_new$Upper

CAT_plot <- bd_CAT_new %>%
  select(Sujeto, Condition, Time, Value) %>%
  ggplot(aes(x = Condition, y = Value, group = interaction(Condition, Time), color = Time)) +
  geom_point(position = position_dodge(0.9)) +
  geom_point(aes(y = CAT_Estimate), position = position_dodge(0.5), shape = 15, size = 5) +
  geom_linerange(aes(ymin= CAT_Lower, ymax= CAT_Upper), position = position_dodge(0.5)) +
  geom_segment(aes(x = 0.87, y = ME_CAT$`Condition:Time`$estimate__[1], xend = 1.13, yend = ME_CAT$`Condition:Time`$estimate__[2]),
               size=0.5, color="black", arrow = arrow()) +
  geom_segment(aes(x = 1.87, y = ME_CAT$`Condition:Time`$estimate__[3], xend = 2.13, yend = ME_CAT$`Condition:Time`$estimate__[4]),
               size=0.5, color = "black", arrow = arrow()) +
  geom_segment(aes(x = 2.87, y = ME_CAT$`Condition:Time`$estimate__[5], xend = 3.13, yend = ME_CAT$`Condition:Time`$estimate__[6]),
               size=0.5, color = "black", arrow = arrow()) +
  ylab("CAT") +
  #facet_grid(. ~ Condition, scales="free_y") +
  theme_bw() +
  theme(legend.position="top",
        legend.title = element_blank(),
        legend.spacing.x = unit(0.5, 'cm'),
        axis.text=element_text(size=12),
        axis.title.y = element_text(size = 12, margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(size = 12, margin = margin(t = 10, r = 20, b = 0, l = 0)))

CAT_plot <- CAT_plot + annotate("text", x = 2, y = 10, parse = TRUE, size = 7, label = as.character(expression(""^{'$'})))
CAT_plot <- CAT_plot + annotate("text", x = 3, y = 9, parse = TRUE, size = 7, label = as.character(expression(""^{'$'})))

Delta_CAT <- c("-1.3", "-2.4", "-3.8")
Delta_CAT_2 <- c("(-3.5, 0.9)", "(-4.3, -0.5)", "(-5.5, -2.0)")
Delta_CAT_3 <- rbind(Delta_CAT, Delta_CAT_2)
Delta_CAT_4 <- tableGrob(d = Delta_CAT_3,
                         rows = c(expression(Delta), "95% CrI"),
                         cols = NULL)
Delta_CAT_4$widths <- unit(rep(0.35, ncol(Delta_CAT_4)), "npc")

CAT_plot <- cowplot::plot_grid(CAT_plot,
                               arrangeGrob(Delta_CAT_4, nullGrob(), widths=c(25,6.5)),
                               align = "v",
                               ncol=1,
                               axis = "b",
                               rel_heights = c(9/10,1/10))

##########

# Arrange plots
all_plots <- ggarrange(CAT_plot,
                       TM6M_plot,
                       PIM_plot,
                       ncol = 1,
                       nrow = 3)

# Save it
ggsave("Figure_3.png", height=14, width=8, units='in', dpi=600)


