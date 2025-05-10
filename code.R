# Packages
library(tidyverse)    # Collection of packages: dplyr, ggplot2, readr, etc.
library(lubridate)    # Handling dates and times
library(janitor)      # Cleaning column names
library(stringr)      # Working with strings
library(tidyr)        # Data tidying
library(ggplot2)      # Visualization (also included in tidyverse)

# File paths
intakes_path <- "C:/Users/bebel/Desktop/analiza danych/Animal-Shelter-Adoption-Analysis/AAC_Intakes.csv"
outcomes_path <- "C:/Users/bebel/Desktop/analiza danych/Animal-Shelter-Adoption-Analysis/AAC_Outcomes.csv"

# Loading datasets
intakes <- read_csv(intakes_path)
outcomes <- read_csv(outcomes_path)

# Quick data overview
glimpse(intakes)
glimpse(outcomes)

# Cleaning column names
intakes <- clean_names(intakes)
outcomes <- clean_names(outcomes)

# Sorting and preparing data for merging
intakes <- intakes %>%
  arrange(animal_id, datetime)

outcomes <- outcomes %>%
  arrange(animal_id, datetime)

# Keeping only the first record per animal_id
intakes_unique <- intakes %>%
  group_by(animal_id) %>%
  slice(1) %>%
  ungroup()

outcomes_unique <- outcomes %>%
  group_by(animal_id) %>%
  slice(1) %>%
  ungroup()

# Merging intakes and outcomes datasets
data <- intakes_unique %>%
  left_join(outcomes_unique, by = "animal_id", suffix = c("_intake", "_outcome"))

# Converting date fields and character fields
data <- data %>%
  mutate(
    datetime_intake = parse_date_time(datetime_intake, orders = c("ymd HMS", "ymd HM", "ymd")),
    datetime_outcome = parse_date_time(datetime_outcome, orders = c("ymd HMS", "ymd HM", "ymd")),
    age_upon_intake = as.character(age_upon_intake),
    age_upon_outcome = as.character(age_upon_outcome)
  )

# Data summary
summary(data)

# Saving merged data to CSV
write_csv(data, "C:/Users/bebel/Desktop/analiza danych/Animal-Shelter-Adoption-Analysis/combined_data.csv")

# ___________________________________________________________________

# Analyzing the distribution of animal outcomes (adoption, return, euthanasia, etc.)
data %>%
  count(outcome_type) %>%
  ggplot(aes(x = reorder(outcome_type, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Distribution of Animal Shelter Outcome",
    x = "Outcome Type",
    y = "Count"
  ) +
  theme_minimal()

# Analyzing types of animals entering the shelter
data %>%
  count(animal_type_intake) %>%
  ggplot(aes(x = reorder(animal_type_intake, n), y = n)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(
    title = "Types of Animals That End Up in the Shelter",
    x = "Animal Type",
    y = "Count"
  ) +
  theme_minimal()

# Analyzing which animals are adopted most frequently
data %>%
  filter(outcome_type == "Adoption") %>%
  count(animal_type_intake) %>%
  ggplot(aes(x = reorder(animal_type_intake, n), y = n)) +
  geom_col(fill = "skyblue") +
  coord_flip() +
  labs(
    title = "Which Animals Are Adopted Most Frequently?",
    x = "Animal Type",
    y = "Number of Adoptions"
  ) +
  theme_minimal()

# Top 10 most adopted breeds
data %>%
  filter(outcome_type == "Adoption") %>%
  count(breed_outcome, sort = TRUE) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(breed_outcome, n), y = n)) +
  geom_col(fill = "orange") +
  coord_flip() +
  labs(
    title = "Top 10 Adopted Breeds",
    x = "Breed",
    y = "Number of Adoptions"
  ) +
  theme_minimal()

# Adoption analysis by animal gender
data %>%
  filter(outcome_type == "Adoption") %>%
  count(sex_upon_outcome, sort = TRUE) %>%
  ggplot(aes(x = reorder(sex_upon_outcome, n), y = n)) +
  geom_col(fill = "purple") +
  coord_flip() +
  labs(
    title = "Adoptions by Animal Gender",
    x = "Gender at Outcome",
    y = "Number of Adoptions"
  ) +
  theme_minimal()

# ___________________________________________________________________

# Quick top 10 most common ages at adoption
data %>%
  filter(outcome_type == "Adoption") %>%
  count(age_upon_intake, sort = TRUE) %>%
  slice_head(n = 10)

# Top 10 most adopted animal colors
data %>%
  filter(outcome_type == "Adoption") %>%
  count(color_outcome, sort = TRUE) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(color_outcome, n), y = n)) +
  geom_col(fill = "pink") +
  coord_flip() +
  labs(
    title = "Top 10 Adopted Colors",
    x = "Color",
    y = "Number of Adoptions"
  ) +
  theme_minimal()

# ___________________________________________________________________

# Creating a table: number of adoptions + percentage for the top 10 age groups
age_adoption_percent <- data %>%
  filter(outcome_type == "Adoption") %>%
  count(age_upon_intake, sort = TRUE) %>%
  mutate(
    percentage = n / sum(n) * 100
  ) %>%
  slice_head(n = 10)

# Bar chart: top 10 ages at adoption (%)
ggplot(age_adoption_percent, aes(x = reorder(age_upon_intake, -percentage), y = percentage)) +
  geom_col(fill = "lightblue", color = "black") +
  labs(
    title = "Adopted Animals by Age Group (%)",
    x = "Age Upon Intake",
    y = "Percentage of Total Adoptions"
  ) +
  theme_minimal()

# ___________________________________________________________________

# Converting animal age to number of days for better grouping
convert_age_to_days <- function(age_string) {
  if (is.na(age_string)) return(NA)
  
  parts <- str_split(age_string, " ", simplify = TRUE)
  number <- as.numeric(parts[1])
  unit <- tolower(parts[2])
  
  case_when(
    str_detect(unit, "day") ~ number,
    str_detect(unit, "week") ~ number * 7,
    str_detect(unit, "month") ~ number * 30,
    str_detect(unit, "year") ~ number * 365,
    TRUE ~ NA_real_
  )
}

# Adding a new column with age in days
data <- data %>%
  mutate(age_in_days = map_dbl(age_upon_intake, convert_age_to_days))

# Creating age groups
data <- data %>%
  mutate(age_group = case_when(
    age_in_days < 60 ~ "<2 months",
    age_in_days >= 60 & age_in_days < 180 ~ "2-6 months",
    age_in_days >= 180 & age_in_days < 365 ~ "6-12 months",
    age_in_days >= 365 & age_in_days < 1095 ~ "1-3 years",
    age_in_days >= 1095 ~ "3+ years",
    TRUE ~ NA_character_
  ))

# Counting adoptions per age group
age_group_adoption <- data %>%
  filter(outcome_type == "Adoption", !is.na(age_group)) %>%
  count(age_group) %>%
  mutate(
    percentage = n / sum(n) * 100
  ) %>%
  arrange(factor(age_group, levels = c("<2 months", "2-6 months", "6-12 months", "1-3 years", "3+ years")))

# Bar chart: adoptions by age group
ggplot(age_group_adoption, aes(x = age_group, y = percentage)) +
  geom_col(fill = "lightgreen", color = "black") +
  labs(
    title = "Adoptions by Age Group (%)",
    x = "Age Group",
    y = "Percentage of Total Adoptions"
  ) +
  theme_minimal()

# ___________________________________________________________________

# Preparing labels with percentages for the pie chart
age_group_adoption <- age_group_adoption %>%
  mutate(label = paste0(round(percentage, 1), "%"))

# Pie chart: adoptions by age group with labels
ggplot(age_group_adoption, aes(x = "", y = percentage, fill = age_group)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            color = "white", size = 4) +
  labs(
    title = "Adoptions by Age Group (%)",
    fill = "Age Group"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )
