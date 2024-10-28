# Autor: Luiz Augusto Brusaca
# Data: 23/10/2024
# Disciplina: FIT 596 - Comportamentos físicos medidos através de acelerometria: 
# coleta, processamento e análise de dados


# Install Packages from Repositories or Local Files
install.packages("magrittr")
install.packages("tidyverse")
install.packages("readxl")
install.packages("data.table")
install.packages("rstatix")
install.packages("compositions")
install.packages("robCompositions")


# Load packages required for analysis
library(magrittr)
library(tidyverse)
library(readxl)
library(data.table)
library(rstatix)
library(compositions)
library(robCompositions)


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


########## Unit example ##########
# Example that the metric that the variable is presented in does not matter, 
# as long as it is from the same volunteer. This is because we are dealing with 
# compositions.

# Minutes
x <- df[1,c("SB", "LPA", "MVPA", "Sleep")]
# Hours
x[2,] <- df[1,c("SB", "LPA", "MVPA", "Sleep")]/60
# Percentage
x[3,] <- df[1,c("SB", "LPA", "MVPA", "Sleep")]/1440*100
# Composition
x[4,] <- as.data.frame(compositions::acomp(df[1,c("SB", "LPA", "MVPA", "Sleep")]))

compositions::ilr(x)

robCompositions::pivotCoord(x, pivotvar = 1)
robCompositions::pivotCoord(x, pivotvar = 2)
robCompositions::pivotCoord(x, pivotvar = 3)
robCompositions::pivotCoord(x, pivotvar = 4)


robCompositions::pivotCoord(x[,c("SB", "LPA", "MVPA", "Sleep")], pivotvar = 1)
robCompositions::pivotCoord(x[,c("LPA", "MVPA", "Sleep", "SB")], pivotvar = 1)
robCompositions::pivotCoord(x[,c("MVPA", "Sleep", "SB", "LPA")], pivotvar = 1)
robCompositions::pivotCoord(x[,c("Sleep", "SB", "LPA", "MVPA")], pivotvar = 1)

rm(x)


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


########## one-way MANOVA ##########
# Running the one-way multivariate analysis of variance (MANOVA)
MANOVA_model <- manova(cbind(ilr1, ilr2, ilr3) ~ Sex, df)

res_MANOVA <- summary(MANOVA_model, test = "Wilks")$stats %>% 
  data.frame() %>% 
  dplyr::select(-Df) %>% 
  stats::na.omit() %>% 
  dplyr::relocate(Wilks, .after = Pr..F.)

res_MANOVA_ES <- summary(MANOVA_model, test = "Pillai")$stats %>% 
  data.frame() %>% 
  dplyr::select(Pillai) %>% 
  stats::na.omit()

res_MANOVA <- data.frame(
  F = paste0(round(res_MANOVA$approx.F, 2), " (", 
             res_MANOVA$num.Df, ", ", res_MANOVA$den.Df, ")"), 
  p = res_MANOVA$Pr..F., 
  Wilks = res_MANOVA$Wilks,
  ES = res_MANOVA_ES$Pillai)

res_MANOVA


########## Student t-test ##########
# Splitting the dataset according to gender
df_Boy <- df %>% 
  dplyr::filter(Sex == "Boy")

df_Girl <- df %>% 
  dplyr::filter(Sex == "Girl")

# Pivoting long the dataset
df_long <- df %>% 
  dplyr::select(ID, Sex, dplyr::starts_with("ilr")) %>% 
  tidyr::pivot_longer(!c(ID, Sex), names_to = "ilr", values_to = "value")


# Running the t-test
T_test_ilr1 <- stats::t.test(df_Boy$ilr1, df_Girl$ilr1, paired = F, 
                             alternative = "two.sided", conf.level = 0.95)

T_test_ilr2 <- stats::t.test(df_Boy$ilr2, df_Girl$ilr2, paired = F, 
                             alternative = "two.sided", conf.level = 0.95)

T_test_ilr3 <- stats::t.test(df_Boy$ilr3, df_Girl$ilr3, paired = F, 
                             alternative = "two.sided", conf.level = 0.95)

# Effect size
Effect_size_ilr1 <- df_long %>% 
  dplyr::filter(ilr == "ilr1") %>% 
  rstatix::cohens_d(value ~ Sex, paired = F, hedges.correction = T, ci = F, 
                    conf.level = 0.95, ci.type = "bca", nboot = 1000) %>% 
  data.frame()

Effect_size_ilr2 <- df_long %>% 
  dplyr::filter(ilr == "ilr2") %>% 
  rstatix::cohens_d(value ~ Sex, paired = F, hedges.correction = T, ci = F, 
                    conf.level = 0.95, ci.type = "bca", nboot = 1000) %>% 
  data.frame()

Effect_size_ilr3 <- df_long %>% 
  dplyr::filter(ilr == "ilr3") %>% 
  rstatix::cohens_d(value ~ Sex, paired = F, hedges.correction = T, ci = F, 
                    conf.level = 0.95, ci.type = "bca", nboot = 1000) %>% 
  data.frame()


# Gathering all the results
res_T_test <- data.frame(
  ilr = "ilr1", 
  Mean_Boy = T_test_ilr1$estimate[1], Mean_Girl = T_test_ilr1$estimate[2], 
  t = T_test_ilr1$statistic, 
  MD = T_test_ilr1$estimate[1] - T_test_ilr1$estimate[2],  
  L_CI = T_test_ilr1$conf.int[1], U_CI = T_test_ilr1$conf.int[2], 
  p = T_test_ilr1$p.value, ES = abs(Effect_size_ilr1$effsize)) %>% 
  rbind(cbind(
    ilr = "ilr2", 
    Mean_Boy = T_test_ilr2$estimate[1], Mean_Girl = T_test_ilr2$estimate[2], 
    t = T_test_ilr2$statistic, 
    MD = T_test_ilr2$estimate[1] - T_test_ilr2$estimate[2], 
    L_CI = T_test_ilr2$conf.int[1], U_CI = T_test_ilr2$conf.int[2], 
    p = T_test_ilr2$p.value, ES = abs(Effect_size_ilr2$effsize))) %>% 
  rbind(cbind(
    ilr = "ilr3", 
    Mean_Boy = T_test_ilr3$estimate[1], Mean_Girl = T_test_ilr3$estimate[2], 
    t = T_test_ilr3$statistic, 
    MD = T_test_ilr3$estimate[1] - T_test_ilr3$estimate[2], 
    L_CI = T_test_ilr3$conf.int[1], U_CI = T_test_ilr3$conf.int[2], 
    p = T_test_ilr3$p.value, ES = abs(Effect_size_ilr3$effsize)))

rownames(res_T_test) <- NULL

res_T_test

# Saving data
openxlsx::write.xlsx(
  res_T_test, 
  "C:/Users/B307324/Documents/Filkassen/PhD_UFSCar/Disciplina acelerometria/4 encontro/test_t_independent.xlsx", 
  row.Names = FALSE, keepNA = FALSE)





