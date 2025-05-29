
# you take your data from the following link
# https://www.gov.uk/government/statistics/annual-flu-reports

# load your stuff
source("https://raw.githubusercontent.com/calpearson/Cal_Functions/main/Cal_Functions.R")
source("https://raw.githubusercontent.com/calpearson/Cal_Functions/main/Cal_packages.R")

library(readxl)

ari <- read_excel("Weekly_output_from_webpage.xlsx", sheet = "ARI") %>% rename(week = `Week number`)
pneumonia <- read_excel("Weekly_output_from_webpage.xlsx", sheet = "PNEUMONIA") %>% rename(week = `Week number`)
ili <- read_excel("Weekly_output_from_webpage.xlsx", sheet = "ILI") %>% rename(week = `Week number`)


# Your initial dataframe is assumed to be called `df`
# Columns: Week, `2018 to 2019`, `2019 to 2020`, ..., `2022 to 2023`

# Step 1: Assign months to weeks
week_to_month <- function(week) {
  case_when(
    week %in% 40:43       ~ "10",  # October
    week %in% 44:48       ~ "11",  # November
    week %in% 49:52       ~ "12",  # December
    week %in% 1:4         ~ "01",  # January
    week %in% 5:8         ~ "02",  # February
    week %in% 9:13        ~ "03",  # March
    week %in% 14:15       ~ "04"   # April
  )
}

# Step 2: Process the data
ari_long <- ari %>%
  mutate(month = week_to_month(week)) %>%
  pivot_longer(cols = -c(week, month), names_to = "season", values_to = "count") %>%
  mutate(month_year = paste0(left(season, 4)), month, )

# View result
head(df_long)
