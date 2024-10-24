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


# Starting (head) and ending (tail) rows of the data set
head(df)
tail(df)


# Checking the variables within the dataset
str(df)


# Behaviours as composition
df$Behaviours <- cbind.data.frame(df[, c("SB", "LPA", "MVPA", "Sleep")])
df$Behaviours_comp <- compositions::acomp(df$Behaviours)


########## Geometric mean ##########
df_geom_mean_Behaviours <- compositions::mean.acomp(df$Behaviours_comp)
# compositions::geometricmeanCol(df$Behaviours_comp)
# exp(apply(log(df$Behaviours_comp), 2, mean))

# Hours
df_geom_mean_Behaviours_h <- clo(df_geom_mean_Behaviours, total = 24) %>% 
  data.frame(Behaviours = c("SB", "LPA", "MVPA", "Sleep")) %>% 
  dplyr::rename(value = '.') %>% 
  tidyr::pivot_wider(names_from = Behaviours, values_from = value) %>% 
  dplyr::mutate(Var_metric = "Hours") %>% 
  dplyr::relocate(Var_metric, .before = SB) %>% 
  data.frame()

# Minutes
df_geom_mean_Behaviours_min <- clo(df_geom_mean_Behaviours, total = 1440) %>% 
  data.frame(Behaviours = c("SB", "LPA", "MVPA", "Sleep")) %>% 
  dplyr::rename(value = '.') %>% 
  tidyr::pivot_wider(names_from = Behaviours, values_from = value) %>% 
  dplyr::mutate(Var_metric = "Minutes") %>% 
  dplyr::relocate(Var_metric, .before = SB) %>% 
  data.frame()

# Percentage
df_geom_mean_Behaviours_perc <- clo(df_geom_mean_Behaviours, total = 100) %>% 
  data.frame(Behaviours = c("SB", "LPA", "MVPA", "Sleep")) %>% 
  dplyr::rename(value = '.') %>% 
  tidyr::pivot_wider(names_from = Behaviours, values_from = value) %>% 
  dplyr::mutate(Var_metric = "Percentage") %>% 
  dplyr::relocate(Var_metric, .before = SB) %>% 
  data.frame()

# Gathering all the results
rbind(df_geom_mean_Behaviours_h, 
      df_geom_mean_Behaviours_min, 
      df_geom_mean_Behaviours_perc)


########## Arithmetic mean ##########
# Hours
df_arith_mean_Behaviours_h <- df %>% 
  dplyr::select(SB, LPA, MVPA, Sleep) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(dplyr::across(SB:Sleep, ~ . / 60)) %>% 
  dplyr::ungroup() %>% 
  rstatix::get_summary_stats(., type = "mean_sd") %>% 
  dplyr::select(-n) %>% 
  dplyr::mutate(Minutes = paste0(format(round(mean, 1), nsmall = 1), " (", 
                                 format(round(sd, 1), nsmall = 1),")")) %>% 
  dplyr::select(-c(mean, sd)) %>% 
  tidyr::pivot_wider(names_from = variable, values_from = Minutes) %>% 
  dplyr::mutate(Var_metric = "Minutes") %>% 
  dplyr::relocate(Var_metric, .before = SB) %>% 
  data.frame()

# Minutes
df_arith_mean_Behaviours_min <- df %>% 
  dplyr::select(SB, LPA, MVPA, Sleep) %>% 
  rstatix::get_summary_stats(., type = "mean_sd") %>% 
  dplyr::select(-n) %>% 
  dplyr::mutate(Minutes = paste0(format(round(mean, 1), nsmall = 1), " (", 
                                 format(round(sd, 1), nsmall = 1),")")) %>% 
  dplyr::select(-c(mean, sd)) %>% 
  tidyr::pivot_wider(names_from = variable, values_from = Minutes) %>% 
  dplyr::mutate(Var_metric = "Minutes") %>% 
  dplyr::relocate(Var_metric, .before = SB) %>% 
  data.frame()

# Percentage
df_arith_mean_Behaviours_perc <- df %>% 
  dplyr::select(SB, LPA, MVPA, Sleep) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(
    Total = sum(SB, LPA, MVPA, Sleep, na.rm = TRUE), 
    dplyr::across(SB:Sleep, ~ . / Total * 100)) %>% 
  dplyr::select(-Total) %>% 
  dplyr::ungroup() %>% 
  rstatix::get_summary_stats(., type = "mean_sd") %>% 
  dplyr::select(-n) %>% 
  dplyr::mutate(Minutes = paste0(format(round(mean, 1), nsmall = 1), " (", 
                                 format(round(sd, 1), nsmall = 1),")")) %>% 
  dplyr::select(-c(mean, sd)) %>% 
  tidyr::pivot_wider(names_from = variable, values_from = Minutes) %>% 
  dplyr::mutate(Var_metric = "Minutes") %>% 
  dplyr::relocate(Var_metric, .before = SB) %>% 
  data.frame()

# Gathering all the results
rbind(df_arith_mean_Behaviours_h, 
      df_arith_mean_Behaviours_min, 
      df_arith_mean_Behaviours_perc)


