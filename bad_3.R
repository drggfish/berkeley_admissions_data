library(broom)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(scales)
library(tibble)
library(tidyr)

ucbadmit_raw <- read.csv("ucb-admit.csv",header=TRUE)

head(ucbadmit_raw)
summary(ucbadmit)

# 1. Filter, select, and save to a new data frame
ucbadmit <- ucbadmit_raw %>%
  rename(gender = Gender) %>%
  rename(admit = Admit) %>%
  rename(dept = Dept) %>%
  mutate(
    admit = factor(admit, levels = c("Rejected", "Admitted")),
    gender = factor(gender),
    dept = factor(dept, ordered = TRUE)
  )

ucbadmit %>%
  count(gender)

ucbadmit %>%
  count(gender, admit)

ucbadmit %>%
  count(gender, admit) %>%
  group_by(gender) %>%
  mutate(prop_admit = n / sum(n))

ggplot(ucbadmit, aes(y = gender, fill = admit)) +
  geom_bar(position = "fill") + 
  labs(title = "Admit by gender",
       y = NULL, x = NULL)

ucbadmit %>%
  count(dept, gender, admit)

ucbadmit %>%
  count(dept, gender, admit) %>%
  pivot_wider(names_from = dept, values_from = n)

ggplot(ucbadmit, aes(y = gender, fill = admit)) +
  geom_bar(position = "fill") +
  facet_wrap(. ~ dept) +
  scale_x_continuous(labels = label_percent()) +
  labs(title = "Admissions by gender and department",
       x = NULL, y = NULL, fill = NULL) +
  theme(legend.position = "bottom")

ucbadmit %>%
  count(dept, gender, admit) %>%
  group_by(dept, gender) %>%
  mutate(
    n_applied  = sum(n),
    prop_admit = n / n_applied
  ) %>%
  filter(admit == "Admitted") %>%
  rename(n_admitted = n) %>%
  select(-admit) %>%
  print(n = 12)

glm_gender <- glm(admit ~ gender, data = ucbadmit, family = "binomial")

summary(glm_gender)

# When you predict the probability of admission as a function of gender alone, the effect is statistically significant (p < 0.01). 
# Specifically, you are exp(0.61035) = 1.84 times more likely to be admitted if you are a man.

# What happens if we control the department?

glm_genderdept <- glm(admit ~ gender + dept, data = ucbadmit, family = "binomial")

summary(glm_genderdept)

# When you control for the effect of department on the probability of admission, the effect of gender disappears. 
# In fact, it even reverses, suggesting that – controlling for department – you were actually more likely to be admitted as a woman! 
# However, this effect is not statistically significant (p > 0.05), so we conclude that there was not a campus-wide bias against applicants of either gender in 1973.

# Let's take a look at Department A, where 82.4% of women were admitted but only 62.1% of men. Is the difference statistically significant?

dept_a <- ucbadmit %>%
  filter(dept == "A")

# Run the regression
glm_gender_depta <- glm(admit ~ gender, data = dept_a, family = "binomial")

# Summarize the results
summary(glm_gender_depta)

# Well then! If we take Department A in isolation, we find there is a statistically significant bias in favour of women.

#######################################################################################################################################

# Load the UC Berkeley admissions dataset (built into R)
data("UCBAdmissions")

# View the structure or summary
ucb_df <- as.data.frame(UCBAdmissions)
head(ucb_df)

# Create a contingency table for Gender vs Admit
table_ucb <- xtabs(Freq ~ Gender + Admit, data = ucb_df)
table_ucb

# Perform the Chi-Square Test of Independence
chi_test_ucb <- chisq.test(table_ucb)
chi_test_ucb

ucadmit_table <- ucbadmit %>%
  count(gender, admit) %>%
  mutate(
    Freq  = n,
  )
  
table <- xtabs(Freq ~ gender + admit, ucadmit_table)

chi_test_ucb <- chisq.test(table)
chi_test_ucb

my_ucb <- margin.table(UCBAdmissions, c(1, 2))
my_ucb
