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
install.packages("ggtern")


# Load packages required for analysis
library(magrittr)
library(tidyverse)
library(readxl)
library(data.table)
library(rstatix)
library(compositions)
library(ggplot2)
library(ggtern)


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
df$Behaviours <- cbind.data.frame(df[, c("SB", "LPA", "MVPA")])
df$Behaviours_comp <- compositions::acomp(df$Behaviours)


########## Geometric mean ##########
df_geom_mean_Behaviours <- compositions::mean.acomp(df$Behaviours_comp)
# compositions::geometricmeanCol(df$Behaviours_comp)
# exp(apply(log(df$Behaviours_comp), 2, mean))

# Percentage
df_geom_mean_Behaviours_perc <- clo(df_geom_mean_Behaviours, total = 100) %>% 
  data.frame(Behaviours = c("SB", "LPA", "MVPA")) %>% 
  dplyr::rename(value = '.') %>% 
  tidyr::pivot_wider(names_from = Behaviours, values_from = value) %>% 
  dplyr::mutate(Var_metric = "Percentage") %>% 
  dplyr::relocate(Var_metric, .before = SB) %>% 
  data.frame()


########## Ternary plot ##########
# Physical behaviour data
df %>% 
  ggtern::ggtern(ggtern::aes(x = SB, y = LPA, z = MVPA)) + 
  ggplot2::geom_point(size = 1.2)


df %>% 
  ggtern::ggtern(ggtern::aes(x = SB, y = LPA, z = MVPA)) + 
  ggplot2::geom_point(size = 1.5) + 
  ggtern::theme_showarrows()


df %>% 
  ggtern::ggtern(ggtern::aes(x = SB, y = LPA, z = MVPA)) + 
  ggplot2::geom_point(size = 1.5) + 
  ggtern::theme_rgbg()


# Physical behaviour data and BMI
df %>% 
  ggtern::ggtern(ggtern::aes(x = SB, y = LPA, z = MVPA, color = Shuttles_20m)) + 
  ggplot2::geom_point(size = 1.5) + 
  ggplot2::scale_color_gradient(low = "yellow", high = "red") + 
  ggplot2::labs(color = "20 m shuttle run test") + 
  ggtern::theme_showarrows()


df %>% 
  ggtern::ggtern(ggtern::aes(x = SB, y = LPA, z = MVPA, color = Shuttles_20m)) + 
  ggplot2::geom_point(size = 1.5) + 
  ggplot2::scale_color_gradient(low = "yellow", high = "red") + 
  ggplot2::theme(legend.position.inside = c(0,1), legend.justification = c(0,1)) + 
  ggplot2::labs(fill = "20 m shuttle run test") + 
  ggtern::theme_rgbg()



# Geometric mean of the composition
df_geom_mean_Behaviours_perc %>% 
  ggtern::ggtern(ggtern::aes(x = SB, y = LPA, z = MVPA)) + 
  ggplot2::geom_point(size = 3) + 
  ggplot2::theme(legend.position.inside = c(0,1), legend.justification = c(0,1)) + 
  ggplot2::labs(x = "SB (%)", y = "LPA (%)", z = "MVPA (%)", fill = "BMI") + 
  ggtern::theme_rgbg() + 
  ggtern::theme_hidetitles() + 
  ggtern::theme_arrowcustomlength(.1,.9) + 
  ggtern::theme_legend_position(x = "topleft") + 
  ggtern::theme_anticlockwise() + 
  ggtern::theme_mesh(5) + # or 8
  ggtern::theme_ticklength(major = unit(1.0,'mm'), minor = unit(1.0,'mm'))



# Physical behaviour data, BMI, and Geometric mean of the composition
df %>% 
  ggtern::ggtern(ggtern::aes(x = SB, y = LPA, z = MVPA, color = Shuttles_20m)) + 
  ggplot2::geom_point(size = 1.5) + 
  ggplot2::scale_color_gradient(low = "yellow", high = "red") + 
  ggplot2::geom_point(
    data = df_geom_mean_Behaviours_perc[1,], size = 2.5, color = "black", 
    fill = "black", shape = 24) + 
  ggtern::geom_crosshair_tern(
    data = df_geom_mean_Behaviours_perc[1,], ggtern::aes(x = SB, y = LPA, z = MVPA), 
    size = 0.5, color = "black") + 
  ggplot2::labs(
    title = NULL, 
    x = "SB (%)", 
    y = "LPA (%)", 
    z = "MVPA (%)", 
    color = "20 m shuttle run test") + 
  ggtern::theme_bw() + 
  ggtern::theme_custom(
    base_size = 12, base_family = "", tern.plot.background = "white", 
    tern.panel.background = "white", col.T = "black", col.L = "black", 
    col.R = "black", col.grid.minor = NULL) + 
  ggtern::theme_hidetitles() + 
  ggtern::theme_showarrows() + 
  ggtern::theme_arrowcustomlength(.1,.9) + 
  ggtern::theme_legend_position(x = "topright") + 
  ggtern::theme_anticlockwise() + 
  ggtern::theme_mesh(5) + # or 8
  ggtern::theme_ticklength(major = unit(1.0,'mm'), minor = unit(1.0,'mm'))


df %>% 
  ggtern::ggtern(ggtern::aes(x = SB, y = LPA, z = MVPA, color = Shuttles_20m)) + 
  ggplot2::geom_point(size = 1.5) + 
  ggplot2::scale_color_gradient(low = "yellow", high = "red") + 
  ggplot2::geom_point(
    data = df_geom_mean_Behaviours_perc[1,], size = 2.5, color = "black", 
    fill = "black", shape = 24) + 
  ggtern::geom_crosshair_tern(
    data = df_geom_mean_Behaviours_perc[1,], ggtern::aes(x = SB, y = LPA, z = MVPA), 
    size = 0.5, color = "black") + 
  ggplot2::labs(
    title = NULL, 
    x = "SB (%)", 
    y = "LPA (%)", 
    z = "MVPA (%)", 
    color = "20 m shuttle run test") + 
  ggtern::theme_rgbg() + 
  ggtern::theme_hidetitles() + 
  ggtern::theme_showarrows() + 
  ggtern::theme_arrowcustomlength(.1,.9) + 
  ggtern::theme_legend_position(x = "topright") + 
  ggtern::theme_anticlockwise() + 
  ggtern::theme_mesh(5) + # or 8
  ggtern::theme_ticklength(major = unit(1.0,'mm'), minor = unit(1.0,'mm'))










