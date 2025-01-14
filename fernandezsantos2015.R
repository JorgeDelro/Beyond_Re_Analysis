
# Libraries
library(tidyverse)
library(MASS)

# Load database
load("data/lowerbody.RData")
glimpse(lowerbody)

########################################################

# Functions

reliability <- function(pretest, postest){

  # Intertrial difference
  IT_diff_mean <- mean(postest - pretest)
  IT_diff_sd <- sd(postest - pretest)
  IT_diff_pvalue <- t.test(postest, pretest, paired = T)$p.value

  # Sum of Squared Errors
  SSE <- sum((postest - pretest)^2)

  # Mean Squared Error
  MSE <- mean(SSE)

  # Root Mean Squared Error
  RMSE <- sqrt(MSE)

  # Percentage of error
  Percentage_error <- (RMSE/(max(pretest) - min(pretest))) * 100

  # Standard Error of Estimate
  SEE <- sd(pretest) * (sqrt(1 - (cor(pretest, postest)^2)))

  return(list(
    IT = paste(IT_diff_mean, " + / - ", IT_diff_sd, " , p-value = ", IT_diff_pvalue),
    SSE = SSE,
    MSE = MSE,
    RMSE = RMSE,
    Percentage_error = Percentage_error,
    SEE = SEE
  ))
}

# Load bland-Altman extended function from
# https://github.com/JorgeDelro/Bland_Altman_extended/blob/main/Bland_Altman_extended.R

source("Bland_Altman_extended.R")

####################################################################
####################################################################


# Reliability
reliability(lowerbody$SLJTest, lowerbody$SLJRetest)
reliability(lowerbody$SJTest, lowerbody$SJRetest)
reliability(lowerbody$CMJTest, lowerbody$CMJRetest)
reliability(lowerbody$AbkTest, lowerbody$AbkRetest)

BA_SLJ <- Bland_Altman_extended(method1 = lowerbody$SLJTest,
                                 method2 = lowerbody$SLJRetest,
                                 x_lab = "SLJ Test",
                                 y_lab = "SLJ Retest",
                                 plot_name = "Bland-Altman SLJ")

BA_SJ <- Bland_Altman_extended(method1 = lowerbody$SJTest,
                                method2 = lowerbody$SJRetest,
                                x_lab = "SJ Test",
                                y_lab = "SJ Retest",
                                plot_name = "Bland-Altman SJ")

BA_CMJ <- Bland_Altman_extended(method1 = lowerbody$CMJTest,
                               method2 = lowerbody$CMJRetest,
                               x_lab = "CMJ Test",
                               y_lab = "CMJ Retest",
                               plot_name = "Bland-Altman CMJ")

Abk_CMJ <- Bland_Altman_extended(method1 = lowerbody$AbkTest,
                                method2 = lowerbody$AbkRetest,
                                x_lab = "Abk Test",
                                y_lab = "Abk Retest",
                                plot_name = "Bland-Altman Abk")

# Validity
# Stepwise selection

# SLJ
# full model -
SLJ_full <- lm(Test1RM ~ SLJRetest + weight + height + age + gender, data = lowerbody)
# Stepwise
SLJ_stepwise <- stepAIC(SLJ_full, direction = "both", trace = FALSE)
summary(SLJ_stepwise)

# SJ
# full model -
SJ_full <- lm(Test1RM ~ SJRetest + weight + height + age + gender, data = lowerbody)
# Stepwise
SJ_stepwise <- stepAIC(SLJ_full, direction = "both", trace = FALSE)
summary(SLJ_stepwise)

# CMJ
# full model -
CMj_full <- lm(Test1RM ~ CMJRetest + weight + height + age + gender, data = lowerbody)
# Stepwise
CMj_stepwise <- stepAIC(SLJ_full, direction = "both", trace = FALSE)
summary(SLJ_stepwise)

# Abk
# full model -
Abk_full <- lm(Test1RM ~ AbkRetest + weight + height + age + gender, data = lowerbody)
# Stepwise
Abk_stepwise <- stepAIC(SLJ_full, direction = "both", trace = FALSE)
summary(SLJ_stepwise)



