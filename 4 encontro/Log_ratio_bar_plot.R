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
install.packages("ggplot2")


# Load packages required for analysis
library(magrittr)
library(tidyverse)
library(readxl)
library(data.table)
library(rstatix)
library(compositions)
library(ggplot2)


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


########## Log-ratio bar plot ##########
# Splitting the dataset according to gender
df_Boy <- df %>% 
  dplyr::filter(Sex == "Boy")

df_Girl <- df %>% 
  dplyr::filter(Sex == "Girl")

# Geometric mean
df_geom_Boy <- compositions::mean.acomp(df_Boy$Behaviours_comp)
df_geom_Girl <- compositions::mean.acomp(df_Girl$Behaviours_comp)


# Closure of a composition
df_clo_Boy <- compositions::clo(df_geom_Boy, total = 1440)
df_clo_Girl <- compositions::clo(df_geom_Girl, total = 1440)


# Log-ratio difference
df_diff_Boy_Girl <- log(df_clo_Boy/df_clo_Girl)


# Percentage of change
df_perc_change_Boy_Girl <- 1:length(df_clo_Boy)
for (i in 1:length(df_diff_Boy_Girl)) {
  df_perc_change_Boy_Girl[i] <- 100-(100*(exp(df_diff_Boy_Girl[i])))
}


# Plot the log-ratio difference
data.frame(
  Behaviours = c("SB", "LPA", "MVPA", "Sleep"), 
  Log_ratio = df_diff_Boy_Girl) %>% 
  ggplot2::ggplot(aes(x = Behaviours, y = Log_ratio)) + 
  ggplot2::geom_bar(stat = "identity", width = 0.8) + 
  ggplot2::geom_hline(yintercept = 0, colour = "black", linewidth = 0.4) + 
  ggplot2::scale_x_discrete(
    limits = c("SB", "LPA", "MVPA", "Sleep"), 
    labels = c("SB", "LPA", "MVPA", "Sleep")) + 
  ggplot2::labs(
    title = NULL, 
    x = "Behaviours", 
    y = "Log-ratio difference") + 
  ggplot2::theme_classic() + 
  ggplot2::scale_y_continuous(
    limits = c(-0.1, 0.4), 
    breaks = seq(-0.1,0.4,0.1), 
    labels = c("-0.1", "0.0", "0.1", "0.2", "0.3", "0.4"), 
    expand = c(0,0)) + 
  ggplot2::theme(
    plot.title = element_text(hjust = 0.5, color = "black", size = 12, face = "bold"),
    axis.title = element_text(color = "black", size = 12, face = "bold"),
    axis.text.y = element_text(color = "black", size = 10),
    axis.text.x = element_text(color = "black", size = 10), 
    panel.grid.major.y = element_line(colour = "grey90"))


# Gathering all the results
df_clo_Boy_Girl <- rbind(
  df_clo_Boy, 
  df_clo_Girl, 
  df_diff_Boy_Girl, 
  df_perc_change_Boy_Girl) %>% 
  data.frame() %>% 
  dplyr::mutate(Sex = c("Boy", "Girl", "Log-ratio difference", "% difference")) %>% 
  dplyr::relocate(Sex, .before = X1)

rownames(df_clo_Boy_Girl) <- NULL
colnames(df_clo_Boy_Girl)[2:5] <- c("SB", "LPA", "MVPA", "Sleep")

df_clo_Boy_Girl








