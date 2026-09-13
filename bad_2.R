library(broom)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(scales)
library(tibble)
library(tidyr)

ucb_data_full <- read.csv("berkeley.csv",header=TRUE)

ls.str(ucb_data_full)

ucb_3d <- xtabs(~ Admission + Sex + Major, data = ucb_data_full)

ucb_2d <- xtabs(~ Admission + Sex, data = ucb_data_full)

addmargins(ucb_2d)

prop.table(ucb_2d)

# Calculate sum across rows
margin.table(ucb_2d, margin = 1)

my_ucb <- margin.table(ucb_3d, c(1,2))

ucb_data_full_df <- as.data.frame(ucb_data_full)

summary(ucb_data_full_df)

# Using a predefined vector
rows_to_keep <- c('A', 'B', 'C', 'D', 'E', 'F')

# 1. Filter, select, and save to a new data frame
ucb_data_full_filtered <- ucb_data_full %>% 
  filter(Major %in% rows_to_keep) %>%
  mutate(Admission = ifelse(Admission == "Accepted", "Admitted", Admission))

summary(ucb_data_full_filtered)

str(ucb_data_full_filtered)

ucb_3d <- xtabs(~ Admission + Sex + Major, data = ucb_data_full_filtered)

head(ucb_data_full_filtered)

ucb_data_full_filtered %>%
  count(Sex, Admission) %>%
  group_by(Sex) %>%
  mutate(prop_admit = n / sum(n))

ggplot(ucb_data_full_filtered, aes(y=Sex, fill = Admission)) +
  geom_bar(position = "fill") +
  labs(title = "Admission by Gender", y = NULL, x = NULL)

ucb_data_full_filtered %>%
  count(Major, Sex, Admission) %>%
  pivot_wider(names_from = Major, values_from = n)

head(UCBAdmissions)
