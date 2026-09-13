library(broom)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(scales)
library(tibble)

ucb_data_full <- read.csv("berkeley.csv",header=TRUE)

ls.str(ucb_data_full)

ucb_3d <- xtabs(~ Admission + Sex, data = ucb_data_full)

my_ucb <- margin.table(UCBAdmissions, c(1,2))


ucb_data_full_df <- as.data.frame(ucb_data_full)

summary(my_ucb)

# Load the dataset
data(UCBAdmissions)

head(ucb_df)

ucb_df_tibble <- as_tibble(ucb_df)

head(ucb_df_tibble)

# View the 3-dimensional array structure
str(UCBAdmissions)
dim(UCBAdmissions)      # Dimensions: Admit (2) x Gender (2) x Dept (6)
dimnames(UCBAdmissions) # View variable levels

# Aggregate over departments
agg_data <- apply(UCBAdmissions, c(1, 2), sum)
print(agg_data)

# Calculate percentages
prop.table(agg_data, margin = 2) * 100

# View breakdown by department
ftable(UCBAdmissions, row.vars = c("Dept", "Gender"), col.vars = "Admit")

# Department-specific acceptance rates
prop.table(UCBAdmissions, margin = c(2, 3)) * 100

# Aggregate mosaic plot
mosaicplot(agg_data, main = "Overall: Student Admissions at UC Berkeley")

# Multi-panel department mosaic plot
par(mfrow = c(2, 3))
for(i in 1:6) {
  mosaicplot(UCBAdmissions[,,i], main = paste("Department", LETTERS[i]))
}

# Convert UCBAdmissions to tidy format
ucb_tidy <- tidy(UCBAdmissions)

# Print tidy dataset to console
head(ucb_tidy)

#Let's take a look into the columns, we will use this to ensure the correct values are entered into the columns.
unique(ucb_tidy$Dept)
unique(ucb_tidy$Admit)
unique(ucb_tidy$Gender)

# Count NA values in each column using dplyr
na_counts_dplyr <- ucb_tidy %>%
  summarise_all(~ sum(is.na(.)))
print(na_counts_dplyr)

summary(ucb_tidy$n)

# Aggregate over department
ucb_tidy_aggregated <- ucb_tidy %>% 
  group_by(Admit, Gender) %>% 
  summarize(Num_of_Applicants = sum(n)) %>% 
  ungroup() %>% 
  group_by(Gender) %>% 
  mutate(Percentage = (Num_of_Applicants / sum(Num_of_Applicants))) %>% 
  filter(Admit == "Admitted")

# Print aggregated dataset
tibble(ucb_tidy_aggregated)

# Prepare the bar plot
gg_bar <- ucb_tidy_aggregated %>% 
  ggplot(aes(x = Gender, y = Percentage, fill = Gender)) +
  geom_col() +
  geom_text(aes(label = percent(Percentage)), vjust = -1) +
  labs(title = "Acceptance rate of male and female applicants",
       subtitle = "University of California, Berkeley (1973)",
       y = "Acceptance rate") 


# Print the bar plot
print(gg_bar)

my_ucb <- margin.table(UCBAdmissions, c(1,2))

my_ucb_test <- chisq.test(my_ucb)
my_ucb_test$statistic
#> X-squared
#>  91.6096
my_ucb_test$p.value
#> [1] 1.055e-21

my_ucb_test


