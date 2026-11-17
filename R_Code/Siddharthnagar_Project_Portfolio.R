# PROJECT: SIDDHARTHNAGAR DISTRICT PROFILE
# Subject: Statistics with R


# Load packages
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

# Import Excel dataset
data <- read_excel(
  "Data/PLFS25Uttar.xlsx",
  sheet = "PLFS_25_up"
)

# Inspect dataset
head(data)
str(data)
summary(data)
names(data)
dim(data)

# Select Siddharthnagar (District code 53)
district_data <- data %>%
  filter(District == 53)

# Select entire Uttar Pradesh
state_data <- data

# Check number of observations
nrow(district_data)
nrow(state_data)

# Check district name through district code
table(district_data$District)

# Check missing values in important variables
colSums(is.na(data[, c(
  "HHID",
  "District",
  "sector",
  "edu_yrs",
  "household_income"
)]))

# Check education range
summary(data$edu_yrs)

# Check household income range
summary(data$household_income)

# Check zero and negative incomes
sum(data$household_income == 0, na.rm = TRUE)
sum(data$household_income < 0, na.rm = TRUE)

# Create education groups
edu_data <- data %>%
  mutate(
    area = ifelse(
      District == 53,
      "Siddharthnagar",
      "Uttar Pradesh"
    ),
    education_group = case_when(
      edu_yrs == 0 ~ "0 years",
      edu_yrs >= 1 & edu_yrs <= 5 ~ "1-5 years",
      edu_yrs >= 6 & edu_yrs <= 8 ~ "6-8 years",
      edu_yrs >= 9 & edu_yrs <= 10 ~ "9-10 years",
      edu_yrs >= 11 & edu_yrs <= 12 ~ "11-12 years",
      edu_yrs > 12 ~ "More than 12 years",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(edu_yrs), edu_yrs >= 0)

# Calculate mean education
education_mean <- edu_data %>%
  group_by(area) %>%
  summarise(
    valid_observations = n(),
    mean_education = mean(edu_yrs)
  )

print(education_mean)

# Calculate education group percentages
education_groups <- edu_data %>%
  mutate(
    education_group = factor(
      education_group,
      levels = c(
        "0 years",
        "1-5 years",
        "6-8 years",
        "9-10 years",
        "11-12 years",
        "More than 12 years"
      )
    )
  ) %>%
  count(area, education_group, .drop = FALSE) %>%
  group_by(area) %>%
  mutate(
    percentage = (n / sum(n)) * 100
  ) %>%
  ungroup()

print(education_groups)

# Education group table
 table2<- education_groups %>%
  select(area, education_group, percentage) %>%
  pivot_wider(
    names_from = area,
    values_from = percentage
  )

print(table2)

write.csv(
  table2,
  "Tables/Table2_Education.csv",
  row.names = FALSE
)
write.csv(
  education_mean,
  "Tables/Education_Mean.csv",
  row.names = FALSE
)

# Check household-level variables
consistency_check <- data %>%
  group_by(HHID) %>%
  summarise(
    income_values = n_distinct(household_income),
    sector_values = n_distinct(sector),
    district_values = n_distinct(District),
    .groups = "drop"
  )

summary(consistency_check)

# Keep only one record per household
hh_data <- data %>%
  distinct(HHID, .keep_all = TRUE)

# Separate district and state households
hh_district <- hh_data %>%
  filter(District == 53)

hh_state <- hh_data

# Check number of unique households
nrow(hh_district)
nrow(hh_state)

# Define income cut-offs using UP households
income_cutoffs <- quantile(
  hh_state$household_income,
  probs = c(1/3, 2/3),
  na.rm = TRUE
)

print(income_cutoffs)


# Create income groups
hh_data <- hh_data %>%
  mutate(
    area = ifelse(
      District == 53,
      "Siddharthnagar",
      "Uttar Pradesh"
    ),
    income_group = case_when(
      is.na(household_income) ~ NA_character_,
      household_income < 0 ~ NA_character_,
      household_income <= income_cutoffs[1] ~ "Low",
      household_income <= income_cutoffs[2] ~ "Middle",
      TRUE ~ "High"
    )
  )

# Retain valid non-negative income values
hh_income <- hh_data %>%
  filter(
    !is.na(household_income),
    household_income >= 0
  )

# Income summary
income_summary <- hh_income %>%
  group_by(area) %>%
  summarise(
    valid_households = n(),
    mean_income = mean(household_income),
    median_income = median(household_income)
  )

print(income_summary)

# Income group percentages
income_groups <- hh_income %>%
  mutate(
    income_group = factor(
      income_group,
      levels = c("Low", "Middle", "High")
    )
  ) %>%
  count(area, income_group, .drop = FALSE) %>%
  group_by(area) %>%
  mutate(
    percentage = (n / sum(n)) * 100
  ) %>%
  ungroup()

print(income_groups)

table3 <- income_groups %>%
  select(area, income_group, percentage) %>%
  pivot_wider(
    names_from = area,
    values_from = percentage
  )

print(table3)

write.csv(
  table3,
  "Tables/Table3_Income.csv",
  row.names = FALSE
)

write.csv(
  income_summary,
  "Tables/Income_Summary.csv",
  row.names = FALSE
)


graph1 <- ggplot(
  education_mean,
  aes(x = area, y = mean_education, fill = area)
) +
  geom_col(width = 0.6) +
  labs(
    title = "Mean Years of Education",
    x = "Area",
    y = "Mean years of education",
    caption = "Source: PLFS unit-level dataset; author's calculations"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(graph1)

ggsave(
  "Graphs/Figure1_Education.png",
  graph1,
  width = 8,
  height = 5
)


graph2 <- ggplot(
  education_groups,
  aes(
    x = education_group,
    y = percentage,
    fill = area
  )
) +
  geom_col(position = "dodge") +
  labs(
    title = "Education Group Comparison",
    x = "Education group",
    y = "Percentage (%)",
    fill = "Area",
    caption = "Source: PLFS unit-level dataset; author's calculations"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 30,
      hjust = 1
    )
  )

print(graph2)

ggsave(
  "Graphs/Figure2_Education_Groups.png",
  graph2,
  width = 9,
  height = 5
)


graph3 <- ggplot(
  hh_income,
  aes(
    x = area,
    y = household_income,
    fill = area
  )
) +
  geom_boxplot(outlier.alpha = 0.2) +
  labs(
    title = "Household Income Distribution",
    x = "Area",
    y = "Household income (Rs.)",
    caption = "Source: PLFS unit-level dataset; author's calculations"
  ) +
  scale_y_continuous(labels = label_comma()) +
  theme_minimal() +
  theme(legend.position = "none")

print(graph3)

ggsave(
  "Graphs/Figure3_Income.png",
  graph3,
  width = 8,
  height = 5
)

# Table 1: Sample profile
table1 <- data.frame(
  Indicator = c(
    "Person records",
    "Unique households",
    "Valid education observations",
    "Valid household income observations"
  ),
  
  Siddharthnagar = c(
    nrow(district_data),
    n_distinct(district_data$HHID),
    sum(!is.na(district_data$edu_yrs)),
    sum(!is.na(hh_district$household_income) &
          hh_district$household_income >= 0)
  ),
  
  Uttar_Pradesh = c(
    nrow(state_data),
    n_distinct(state_data$HHID),
    sum(!is.na(state_data$edu_yrs)),
    sum(!is.na(hh_state$household_income) &
          hh_state$household_income >= 0)
  )
)

print(table1)

write.csv(
  table1,
  "Tables/Table1_Sample_Profile.csv",
  row.names = FALSE
)

education_mean_long <- education_mean %>%
  transmute(
    area,
    Indicator = "Mean years of education",
    Value = mean_education
  )

education_groups_long <- education_groups %>%
  transmute(
    area,
    Indicator = paste0(
      as.character(education_group),
      " (%)"
    ),
    Value = percentage
  )

table2_full <- bind_rows(
  education_mean_long,
  education_groups_long
) %>%
  pivot_wider(
    names_from = area,
    values_from = Value
  )

print(table2_full)

write.csv(
  table2_full,
  "Tables/Table2_Education_Complete.csv",
  row.names = FALSE
)


income_stats_long <- income_summary %>%
  pivot_longer(
    cols = c(
      valid_households,
      mean_income,
      median_income
    ),
    names_to = "statistic",
    values_to = "Value"
  ) %>%
  mutate(
    Indicator = case_when(
      statistic == "valid_households" ~
        "Unique households with valid income",
      statistic == "mean_income" ~
        "Mean household income",
      statistic == "median_income" ~
        "Median household income"
    )
  ) %>%
  select(area, Indicator, Value)

income_groups_long <- income_groups %>%
  transmute(
    area,
    Indicator = paste0(
      as.character(income_group),
      " income group (%)"
    ),
    Value = percentage
  )

table3_full <- bind_rows(
  income_stats_long,
  income_groups_long
) %>%
  pivot_wider(
    names_from = area,
    values_from = Value
  )

print(table3_full)

write.csv(
  table3_full,
  "Tables/Table3_Income_Complete.csv",
  row.names = FALSE
)


# Education by rural and urban sector

edu_sector <- edu_data %>%
  mutate(
    sector = factor(
      sector,
      levels = c(1, 2),
      labels = c("Rural", "Urban")
    )
  ) %>%
  group_by(area, sector) %>%
  summarise(
    valid_education_observations = n(),
    mean_education = mean(edu_yrs),
    .groups = "drop"
  )

print(edu_sector)


# Household income by rural and urban sector

income_sector <- hh_income %>%
  mutate(
    sector = factor(
      sector,
      levels = c(1, 2),
      labels = c("Rural", "Urban")
    )
  ) %>%
  group_by(area, sector) %>%
  summarise(
    valid_households = n(),
    mean_household_income = mean(household_income),
    median_household_income = median(household_income),
    .groups = "drop"
  )

print(income_sector)


# Combine rural-urban comparisons

table4 <- full_join(
  edu_sector,
  income_sector,
  by = c("area", "sector")
)

print(table4)

write.csv(
  table4,
  "Tables/Table4_Rural_Urban.csv",
  row.names = FALSE
)


# Create output folders if they do not exist

dir.create("Tables", showWarnings = FALSE)
dir.create("Graphs", showWarnings = FALSE)