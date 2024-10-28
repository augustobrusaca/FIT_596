# Autor: Luiz Augusto Brusaca
# Data: 25/10/2024
# Disciplina: FIT 596 - Comportamentos físicos medidos através de acelerometria: 
# coleta, processamento e análise de dados


# Install Packages from Repositories or Local Files
install.packages("magrittr")
install.packages("tidyverse")
install.packages("readxl")
install.packages("compositions")
install.packages("car")


# Load packages required for analysis
library(magrittr)
library(tidyverse)
library(readxl)
library(compositions)
library(car)


########## Load accelerometer data ##########
# Dataset downloaded from https://ijbnpa.biomedcentral.com/articles/10.1186/s12966-017-0521-z

# Fairclough SJ, Dumuid D, Taylor S, Curry W, McGrane B, Stratton G, Maher C, 
# Olds T. Fitness, fatness and the reallocation of time between children's daily 
# movement behaviours: an analysis of compositional data. Int J Behav Nutr Phys 
# Act. 2017 May 10;14(1):64. doi: 10.1186/s12966-017-0521-z.

# It is the Additional File 7.

# Load data
df <- readxl::read_excel(
  "C:/Users/B307324/Documents/Filkassen/PhD_UFSCar/Disciplina acelerometria/Dados/Fairclough_2017.xlsx", 
  sheet = "Data") %>% 
  data.frame() %>% 
  dplyr::arrange(ID)


# Behaviours as composition
df$Behaviours <- cbind.data.frame(df[, c("SB", "LPA", "MVPA", "Sleep")])
df$Behaviours_comp <- compositions::acomp(df$Behaviours)


########## isometric log-ratio (ilr) transformation ##########
# Define sequential binary partition (SBP)
# 1. Sleep vs. [SB, LPA, MVPA] (sleep vs. wake)
# 2. SB vs. [LPA, MVPA] (sedentary vs. non-sedentary)
# 3. LPA vs MVPA (light vs. more heavy intensity PA)

SBP <- matrix(c(-1, -1, -1, 1,
                1, -1, -1, 0,
                0, 1, -1, 0),
              ncol = 4, byrow = T)

# Construct contrast matrix (the orthonormal matrix)
contr_matrix <- compositions::gsi.buildilrBase(t(SBP))

# Create isometric log ratios (ilr) using the above SBP and orthonormal basis V = contr_matrix.
df_ilr <- as.data.frame(compositions::ilr(df[,c("SB", "LPA", "MVPA", "Sleep")], V = contr_matrix))
colnames(df_ilr) <- paste0("ilr", 1:length(df_ilr))

df <- cbind(df, df_ilr)

rm(df_ilr)

# Mean ilr-coordinate values
df %>% 
  dplyr::select(Sex, dplyr::starts_with("ilr")) %>% 
  dplyr::group_by(Sex) %>% 
  rstatix::get_summary_stats(type = "mean_sd")


########## Regression ##########
# Adjusting some variables
df <- df %>% 
  dplyr::mutate(
    IMD_Decile = round(IMD_Decile), 
    IMD_Decile = as.factor(IMD_Decile), 
    Sex = as.factor(Sex))

# Running the regression
lm_model <- lm(zBMI ~ ilr1 + ilr2 + ilr3 + IMD_Decile + Sex + Age, df)
res_lm <- summary(lm_model)

res_lm

# Analysis of Variance
# Verifying whether the whole composition is significant associated with 
# variable outcome
res_Anova <- car::Anova(lm(zBMI ~ cbind(ilr1, ilr2, ilr3) + IMD_Decile + Sex + 
                             Age, df), type = 2, test.statistic = "Wald")

res_Anova

# The set of ilr coordinates (cbind(ilr1, ilr2, ilr3)), i.e., the overall 
# composition is a significant predictor of zBMI (p-value < 0.001).

# The car::Anova() function was used because it adjusts each predictor for all 
# remaining covariates. Pay attention that R has a different anova() function 
# that doesn't make sequential adjustment (adjusts each predictor only for the 
# previous variables in the model, so that the first variable would be completely 
# unadjusted)


