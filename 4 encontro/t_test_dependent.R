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


# Load packages required for analysis
library(magrittr)
library(tidyverse)
library(readxl)
library(data.table)
library(rstatix)
library(compositions)


########## Load accelerometer data ##########
# Dataset downloaded from https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0214058

# Fairclough SJ, Dumuid D, Taylor S, Curry W, McGrane B, Stratton G, Maher C, 
# Olds T. Use of time in chronic obstructive pulmonary disease: Longitudinal 
# associations with symptoms and quality of life using a compositional 
# analysis approach. PLOS ONE. 2019 14(3): e0214058. 
# https://doi.org/10.1371/journal.pone.0214058.

# It is the Supplementary File 1.

# Load data
df <- readxl::read_excel(
  "C:/Users/B307324/Documents/Filkassen/PhD_UFSCar/Disciplina acelerometria/Dados/Lewthwaite_2019.xlsx", 
  sheet = "Data") %>% 
  data.frame() %>% 
  dplyr::arrange(ID) %>% 
  dplyr::select(
    ID, Group, SB_T0 = SB_MARCA_baseline, LPA_T0 = LPA_MARCA_baseline, 
    MVPA_T0 = MVPA_MARCA_baseline, Sleep_T0 = Sleep_MARCA_baseline, 
    SB_T1 = SB_MARCA_time2, LPA_T1 = LPA_MARCA_time2, MVPA_T1 = MVPA_MARCA_time2, 
    Sleep_T1 = Sleep_MARCA_time2) %>% 
  dplyr::filter(Group != "UC")


# Splitting the dataset according to measurement
df_T0 <- df %>% 
  dplyr::select(ID, Group, SB_T0, LPA_T0, MVPA_T0, Sleep_T0) %>%
  dplyr::rename_with(~ gsub("_T0", "", .), dplyr::everything()) %>% 
  dplyr::mutate(Evaluation = "Baseline") %>% 
  dplyr::relocate(Evaluation, .after = ID)

df_T1 <- df %>% 
  dplyr::select(ID, Group, SB_T1, LPA_T1, MVPA_T1, Sleep_T1) %>%
  dplyr::rename_with(~ gsub("_T1", "", .), dplyr::everything()) %>% 
  dplyr::mutate(Evaluation = "Follow_up") %>% 
  dplyr::relocate(Evaluation, .after = ID)


# Merging the Baseline and follow-up dataset
df <- rbind(df_T0, df_T1) %>% 
  dplyr::mutate(
    Evaluation = factor(Evaluation, levels = c("Baseline", "Follow_up")))


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
  dplyr::select(Evaluation, dplyr::starts_with("ilr")) %>% 
  dplyr::group_by(Evaluation) %>% 
  rstatix::get_summary_stats(type = "mean_sd")


########## one-way MANOVA ##########
# Running the one-way multivariate analysis of variance (MANOVA)
MANOVA_model <- manova(cbind(ilr1, ilr2, ilr3) ~ Evaluation + Error(factor(ID)), df)

res_MANOVA <- summary(MANOVA_model, test = "Wilks")$'Error: Within'[4] %>% 
  data.frame() %>% 
  dplyr::select(-stats.Df) %>% 
  stats::na.omit() %>% 
  dplyr::relocate(stats.Wilks, .after = stats.Pr..F.)

res_MANOVA_ES <- summary(MANOVA_model, test = "Pillai")$'Error: Within'[4] %>% 
  data.frame() %>% 
  dplyr::select(stats.Pillai) %>% 
  stats::na.omit()

res_MANOVA <- data.frame(
  F = paste0(round(res_MANOVA$stats.approx.F, 2), " (", 
             res_MANOVA$stats.num.Df, ", ", res_MANOVA$stats.den.Df, ")"), 
  p = res_MANOVA$stats.Pr..F., 
  Wilks = res_MANOVA$stats.Wilks,
  ES = res_MANOVA_ES$stats.Pillai)

res_MANOVA


########## Student t-test ##########
# Splitting the dataset according to gender
df_T0 <- df %>% 
  dplyr::filter(Evaluation == "Baseline")

df_T1 <- df %>% 
  dplyr::filter(Evaluation == "Follow_up")

# Pivoting long the dataset
df_long <- df %>% 
  dplyr::select(ID, Evaluation, dplyr::starts_with("ilr")) %>% 
  tidyr::pivot_longer(!c(ID, Evaluation), names_to = "ilr", values_to = "value")


# Running the t-test
T_test_ilr1 <- stats::t.test(df_T0$ilr1, df_T1$ilr1, paired = T, 
                             alternative = "two.sided", conf.level = 0.95)

T_test_ilr2 <- stats::t.test(df_T0$ilr2, df_T1$ilr2, paired = T, 
                             alternative = "two.sided", conf.level = 0.95)

T_test_ilr3 <- stats::t.test(df_T0$ilr3, df_T1$ilr3, paired = T, 
                             alternative = "two.sided", conf.level = 0.95)

# Effect size
Effect_size_ilr1 <- df_long %>% 
  dplyr::filter(ilr == "ilr1") %>% 
  rstatix::cohens_d(value ~ Evaluation, paired = T, hedges.correction = T, ci = F, 
                    conf.level = 0.95, ci.type = "bca", nboot = 1000) %>% 
  data.frame()

Effect_size_ilr2 <- df_long %>% 
  dplyr::filter(ilr == "ilr2") %>% 
  rstatix::cohens_d(value ~ Evaluation, paired = T, hedges.correction = T, ci = F, 
                    conf.level = 0.95, ci.type = "bca", nboot = 1000) %>% 
  data.frame()

Effect_size_ilr3 <- df_long %>% 
  dplyr::filter(ilr == "ilr3") %>% 
  rstatix::cohens_d(value ~ Evaluation, paired = T, hedges.correction = T, ci = F, 
                    conf.level = 0.95, ci.type = "bca", nboot = 1000) %>% 
  data.frame()


# Gathering all the results
res_T_test <- data.frame(
  ilr = "ilr1", 
  Mean_T0 = mean(df_T0$ilr1), Mean_T1 = mean(df_T1$ilr1), 
  t = T_test_ilr1$statistic, MD = T_test_ilr1$estimate,  
  L_CI = T_test_ilr1$conf.int[1], U_CI = T_test_ilr1$conf.int[2], 
  p = T_test_ilr1$p.value, ES = abs(Effect_size_ilr1$effsize)) %>% 
  rbind(cbind(
    ilr = "ilr2", 
    Mean_T0 = mean(df_T0$ilr2), Mean_T1 = mean(df_T1$ilr2), 
    t = T_test_ilr2$statistic, MD = T_test_ilr2$estimate, 
    L_CI = T_test_ilr2$conf.int[1], U_CI = T_test_ilr2$conf.int[2], 
    p = T_test_ilr2$p.value, ES = abs(Effect_size_ilr2$effsize))) %>% 
  rbind(cbind(
    ilr = "ilr3", 
    Mean_T0 = mean(df_T0$ilr3), Mean_T1 = mean(df_T1$ilr3), 
    t = T_test_ilr3$statistic, MD = T_test_ilr3$estimate, 
    L_CI = T_test_ilr3$conf.int[1], U_CI = T_test_ilr3$conf.int[2], 
    p = T_test_ilr3$p.value, ES = abs(Effect_size_ilr3$effsize)))

rownames(res_T_test) <- NULL

res_T_test

# Saving data
openxlsx::write.xlsx(
  res_T_test, 
  "C:/Users/B307324/Documents/Filkassen/PhD_UFSCar/Disciplina acelerometria/4 encontro/test_t_dependent.xlsx", 
  row.Names = FALSE, keepNA = FALSE)





