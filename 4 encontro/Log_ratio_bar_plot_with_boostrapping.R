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
install.packages("boot")


# Load packages required for analysis
library(magrittr)
library(tidyverse)
library(readxl)
library(data.table)
library(rstatix)
library(compositions)
library(boot)


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


########## Log-ratio bar plot using bootstraping ##########
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


# Bootstraping
set.seed(12345)

boot_log_ratio <- function(data, indice){
  global_counter <<- iteration_counter <<- iteration_counter + 1
  # print(paste("Bootstrap iteration:", paste0("Boot_", global_counter)))
  
  # Sampling the data
  if (global_counter == "1"){
    df_boot_Boy <- data %>% 
      dplyr::select(dplyr::ends_with("_Boy")) %>% 
      rstatix::drop_na() %>%
      dplyr::rename_with(~ gsub("_Boy", "", .), dplyr::everything())
  } else {
    df_boot_Boy <- data %>% 
      dplyr::select(dplyr::ends_with("_Boy")) %>% 
      rstatix::drop_na() %>%
      dplyr::rename_with(~ gsub("_Boy", "", .), dplyr::everything()) %>% 
      dplyr::slice_sample(n = nrow(data) - 1, replace = TRUE)
  }
  
  df_boot_Girl <- data %>% 
    dplyr::select(dplyr::ends_with("_Girl")) %>%
    dplyr::rename_with(~ gsub("_Girl", "", .), dplyr::everything())
  
  df_boot_Girl <- df_boot_Girl[indice,]
  
  # Behaviours as composition
  df_boot_Boy$Behaviours <- cbind.data.frame(df_boot_Boy[, c("SB", "LPA", "MVPA", "Sleep")])
  df_boot_Boy$Behaviours_comp <- compositions::acomp(df_boot_Boy$Behaviours)
  
  df_boot_Girl$Behaviours <- cbind.data.frame(df_boot_Girl[, c("SB", "LPA", "MVPA", "Sleep")])
  df_boot_Girl$Behaviours_comp <- compositions::acomp(df_boot_Girl$Behaviours)
  
  # Geometric mean
  df_boot_geom_Boy <- compositions::mean.acomp(df_boot_Boy$Behaviours_comp)
  df_boot_geom_Girl <- compositions::mean.acomp(df_boot_Girl$Behaviours_comp)
  
  # Closure of a composition
  df_boot_clo_Boy <- compositions::clo(df_boot_geom_Boy, total = 1440)
  df_boot_clo_Girl <- compositions::clo(df_boot_geom_Girl, total = 1440)
  
  # Log-ratio difference
  df_boot_diff_Boy_Girl <- log(df_boot_clo_Boy/df_boot_clo_Girl)
  
  return(df_boot_diff_Boy_Girl)
}


# Merging the Boys and Girls dataset
data_Boy <- df_Boy %>% 
  dplyr::select(ID, Sex, SB, LPA, MVPA, Sleep) %>% 
  dplyr::add_row(ID = NA, Sex = NA, SB = NA, LPA = NA, MVPA = NA, Sleep = NA) %>% 
  dplyr::rename_with(~ paste0(., "_Boy"), c("ID", "Sex", "SB", "LPA", "MVPA", "Sleep"))

data_Girl <- df_Girl %>% 
  dplyr::select(ID, Sex, SB, LPA, MVPA, Sleep) %>% 
  dplyr::rename_with(~ paste0(., "_Girl"), c("ID", "Sex", "SB", "LPA", "MVPA", "Sleep"))

data_Boy_Girl <- cbind(data_Boy, data_Girl)

rm(list = c("data_Boy", "data_Girl"))

# Setting the global counter to 0
iteration_counter <- 0

# Performing the bootstrapping
res_boot_log <- boot::boot(data = data_Boy_Girl, statistic = boot_log_ratio, 
                           R = 1000, sim = "ordinary")

# Calculate 95% confidence intervals for each of the bootstrapped log-ratio difference values
# - Using "Bias Corrected and Accelerated" (BCa) method
CI_SB <- boot::boot.ci(boot.out = res_boot_log, index = 1, conf = 0.95, type = 'bca')
CI_LPA <- boot::boot.ci(boot.out = res_boot_log, index = 2, conf = 0.95, type = 'bca')
CI_MVPA <- boot::boot.ci(boot.out = res_boot_log, index = 3, conf = 0.95, type = 'bca')
CI_Sleep <- boot::boot.ci(boot.out = res_boot_log, index = 4, conf = 0.95, type = 'bca')


# Gathering the bootstrapping results
df_diff_CI <- data.frame(
  Behaviours = c("SB", "LPA", "MVPA", "Sleep"), 
  Log_ratio = df_diff_Boy_Girl, 
  CI = rbind(CI_SB$bca[,c(4,5)], CI_LPA$bca[,c(4,5)], 
             CI_MVPA$bca[,c(4,5)], CI_Sleep$bca[,c(4,5)])) %>% 
  dplyr::rename(Lower_CI = CI.V1, Upper_CI = CI.V2)


# Plot the log-ratio difference
df_diff_CI %>% 
  ggplot2::ggplot(aes(x = Behaviours, y = Log_ratio)) + 
  ggplot2::geom_point(size = 3) + 
  ggplot2::geom_errorbar(aes(ymax = Lower_CI, ymin = Upper_CI), width = .1) + 
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
    limits = c(-0.2, 0.6), 
    breaks = seq(-0.2,0.6,0.1), 
    labels = c("-0.2", "-0.1", "0.0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6"), 
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








