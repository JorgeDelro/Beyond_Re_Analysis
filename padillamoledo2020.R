
# Libraries
library(ordinal) # clm
library(sure) # Residuals
library(rms) # multicollineality

## All sample

df <- readr::read_csv2("padillamoledo2020_db.csv")
names(df)

# HG/WG
df$HG_WG <- df$X14_04_7d_D_DINAMOMETRIA_MEJOR_MEDIA_año1 / df$x14_15d_D_PESO_MEDIA_año1
# Velocidad buena
df$velocidad_buena <- df$X14_05_3d_D_VELOCIDAD_MEJOR_año1 * (-1)
# to factor
df$c53SALUD_año1 <- factor(df$c53SALUD_año1, labels = c("1","2","3","4","5"))


########################################## COMMENT 1
############## BASELINE
# Model 1
# 20mSRT
All_20SRT <- clm(c53SALUD_año1~Tanner_FINAL+sex_año1+X14_Age_año1+IMC+VO2MAX, data = df)
summary(All_20SRT)
# Odd ratios
round(exp(All_20SRT$beta), 3)
# profile likelihood confidence intervals
round(exp(confint(All_20SRT)),3)

# HG/WG
All_HGWG <- clm(c53SALUD_año1~Tanner_FINAL+sex_año1+X14_Age_año1+IMC+HG_WG, data = df)
summary(All_HGWG)
# Odd ratios
round(exp(All_HGWG$beta), 3)
# profile likelihood confidence intervals
round(exp(confint(All_HGWG)),3)

# SLJ
All_SLJ <- clm(c53SALUD_año1~Tanner_FINAL+sex_año1+X14_Age_año1+IMC+X14_04_8d_D_SALTO_MEJOR_año1, data = df)
summary(All_SLJ)
# Odd ratios
round(exp(All_SLJ$beta), 3)
# profile likelihood confidence intervals
round(exp(confint(All_SLJ)),3)

# 4x10m SRT
All_4x10_SRT <- clm(c53SALUD_año1~Tanner_FINAL+sex_año1+X14_Age_año1+IMC+X14_04_8d_D_SALTO_MEJOR_año1, data = df)
summary(All_4x10_SRT)
# Odd ratios
round(exp(All_4x10_SRT$beta), 3)
# profile likelihood confidence intervals
round(exp(confint(All_4x10_SRT)),3)

# 4x10m SRT
All_PhysicalFitness <- clm(c53SALUD_año1~Tanner_FINAL+sex_año1+X14_Age_año1+IMC+PhysicalFitness, data = df)
summary(All_PhysicalFitness)
# Odd ratios
round(exp(All_PhysicalFitness$beta), 3)
# profile likelihood confidence intervals
round(exp(confint(All_PhysicalFitness)),3)



############## Muscular fitness#
model1_MuscularFitness <- clm(c53SALUD_año1~Tanner_FINAL+sex_año1+IMC+MuscularFitness, data = bdYoungers)
summary(model1_MuscularFitness)
# Odd ratios
round(exp(model1_MuscularFitness$beta), 3)
# profile likelihood confidence intervals
round(exp(confint(model1_MuscularFitness)),3)

model1_MuscularFitness <- clm(c53SALUD_año1~Tanner_FINAL+sex_año1+IMC+MuscularFitness, data = bdOlders)
summary(model1_MuscularFitness)
# Odd ratios
round(exp(model1_MuscularFitness$beta), 3)
# profile likelihood confidence intervals
round(exp(confint(model1_MuscularFitness)),3)


###
bdYoungers$c53SALUD_año1 <- factor(bdYoungers$c53SALUD_año1)
bdOlders$c53SALUD_año1 <- factor(bdOlders$c53SALUD_año1)

bdYoungers$c53SALUD_año3 <- factor(bdYoungers$c53SALUD_año3)
bdOlders$c53SALUD_año3 <- factor(bdOlders$c53SALUD_año3)

chisq.test(bdYoungers)


#### z-scores with HG_WG
bdYoungers$Z_score_HGWG <- scale(bdYoungers$HG_WG)[,1]
bdYoungers$ZX14_05_3d_D_VELOCIDAD_MEJOR_año1 <- bdYoungers$ZX14_05_3d_D_VELOCIDAD_MEJOR_año1*-1
bdYoungers$MF_new <- bdYoungers$Z_score_HGWG + bdYoungers$ZX14_04_8d_D_SALTO_MEJOR_año1
bdYoungers$GFP_new <- bdYoungers$Z_score_HGWG + bdYoungers$ZX14_04_8d_D_SALTO_MEJOR_año1 + bdYoungers$ZX14_05_3d_D_VELOCIDAD_MEJOR_año1 + bdYoungers$ZX14_06_1COURSE_NAVETTE_PALIERS_año1

bdOlders$Z_score_HGWG <- scale(bdOlders$HG_WG)[,1]
bdOlders$ZX14_05_3d_D_VELOCIDAD_MEJOR_año1 <- bdOlders$ZX14_05_3d_D_VELOCIDAD_MEJOR_año1*-1
bdOlders$MF_new <- bdOlders$Z_score_HGWG + bdOlders$ZX14_04_8d_D_SALTO_MEJOR_año1
bdOlders$GFP_new <- bdOlders$Z_score_HGWG + bdOlders$ZX14_04_8d_D_SALTO_MEJOR_año1 + bdOlders$ZX14_05_3d_D_VELOCIDAD_MEJOR_año1 + bdOlders$ZX14_06_1COURSE_NAVETTE_PALIERS_año1



############## Muscular fitness#
model1_MuscularFitness <- clm(c53SALUD_año1~Tanner_FINAL+sex_año1+GFP_new, data = bdYoungers)
summary(model1_MuscularFitness)
# Odd ratios
round(exp(model1_MuscularFitness$beta), 3)
# profile likelihood confidence intervals
round(exp(confint(model1_MuscularFitness)),3)

model1_MuscularFitness <- clm(c53SALUD_año1~Tanner_FINAL+sex_año1+GFP_new, data = bdOlders)
summary(model1_MuscularFitness)
# Odd ratios
round(exp(model1_MuscularFitness$beta), 3)
# profile likelihood confidence intervals
round(exp(confint(model1_MuscularFitness)),3)
