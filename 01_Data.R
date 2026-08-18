# I. Installing packages
# -> tidyverse = collection of tools for data manipulation + plotting

install.packages("tidyverse")
library(tidyverse)

install.packages('fwildclusterboot', repos ='https://s3alfisc.r-universe.dev')
library(fwildclusterboot)

setwd("/Users/popouorbih/Documents/EC6131")

# II. Importing data

income_data <- read_csv("Resident Households.csv")
resale <- read_csv("Resale_20171_20265.csv")

glimpse(income_data)

#III. Reshape Data from wide format -> long format

income_long <- income_data %>%
  pivot_longer(
    cols = -Estate,
    names_to = "income_band",
    values_to = "count"
  )

glimpse(income_long)

# IV. Construction of data

# IV.I Removal of "Total Column"

income_long <- income_long %>%
  filter(income_band != "Total")

# V. Creation of Grant Schedule

income_long <- income_long %>%
  mutate(
    grant_increase = case_when(
      income_band == "NoEmployedPerson" ~ 0,
      income_band == "Below_1_000"      ~ 40000,
      income_band == "1_000_1_999"      ~ 37500,
      income_band == "2_000_2_999"      ~ 32500,
      income_band == "3_000_3_999"      ~ 27500,
      income_band == "4_000_4_999"      ~ 20000,
      income_band == "5_000_5_999"      ~ 15000,
      income_band == "6_000_6_999"      ~ 7500,
      income_band == "7_000_7_999"      ~ 5000,
      
      TRUE ~ 0
    )
  )

# VI. Creation of Exposure Variable

# Here, I am making a new object called "estate_exposure". Within this object,
# I derive data from income_long and I group it by "Estate". I then summarise
# all indiv. estates into one row; generating a variable called exposure.

estate_exposure <- income_long %>%
  group_by(Estate) %>%
  summarise(
    exposure = sum(count * grant_increase) / sum(count)
  )

glimpse(estate_exposure)
print(estate_exposure, n = 50)

# Goal: ln(P) = a + b(Post_t + Exposure) + g + d + X + e

# VII. Deletion of all fully empty rows

resale <- resale %>%
  filter(if_any(everything(), ~ !is.na(.) & . != ""))

# VIII. Joining of "estate_exposure" into resale

# VIII.I Lower-casing all estate

resale$Estate <- tolower(resale$Estate)
estate_exposure$Estate <- tolower(estate_exposure$Estate)

resale <- resale %>%
  left_join(estate_exposure, by = "Estate")

glimpse(resale)

# IX. Creation of the Post_t variable

# IX.I Conversion of resale date into date format

library(lubridate)

resale <- resale %>%
  mutate(month = ym(month),
         Post = if_else(month >= ym("2024-08"), 1, 0))

# X. Introduction of log(P)

resale <- resale %>%
  mutate(log_price = log(resale_price))

# XI. Introduction of Remaining Lease

resale <- resale %>%
  mutate(
    remaining_lease = 99 - (Year - lease_commence_date)
  )

summary(resale$remaining_lease)
