library(lubridate)
library(dplyr)
library(ggplot2)
# importing data input
total <- read.csv("total school age.csv")
pre_pri <- read.csv("pre-primary.csv")
pri <- read.csv("primary.csv")
low_sec <- read.csv("lower secondary.csv")
up_sec <- read.csv("upper secondary.csv")


# formatting Time.period into year format
intoDatetime <- function(df){
  df$Time.period <- as.factor(df$Time.period)
  df$Time.period <- as.Date(df$Time.period, format = '%Y')
  df$Time.period <- year(df$Time.period)

  # updating the mismatched years in the column Time.period
  df[df$Time.period == 2076, "Time.period"] <- 2019 # Nepal
  df[df$Time.period == 2562, "Time.period"] <- 2019 # Thailand

  return(df)
}

total <- intoDatetime(total)
pre_primary <- intoDatetime(pre_pri)
primary <- intoDatetime(pri)
lower_sec <- intoDatetime(low_sec)
upper_sec <- intoDatetime(up_sec)


# Remove percentage symbols and convert columns to numeric
removingSymbols <- function(df){
  df$Total <- as.numeric(gsub("%", "", df$Total))
  df$Rural <- as.numeric(gsub("%", "", df$Rural))
  df$Urban <- as.numeric(gsub("%", "", df$Urban))
  df$Poorest <- as.numeric(gsub("%", "", df$Poorest))
  df$Richest <- as.numeric(gsub("%", "", df$Richest))

  return(df)
}

total <- removingSymbols(total)
pre_primary <- removingSymbols(pre_primary)
primary <- removingSymbols(primary)
lower_sec <- removingSymbols(lower_sec)
upper_sec <- removingSymbols(upper_sec)


# Filter out rows with missing key columns
cleaned_total <- total %>%
  filter(!(is.na(Rural) | is.na(Urban) | is.na(Poorest) | is.na(Richest)))
cleaned_pre_primary <- pre_primary %>%
  filter(!(is.na(Rural) | is.na(Urban) | is.na(Poorest) | is.na(Richest)))
cleaned_primary <- primary %>%
  filter(!(is.na(Rural) | is.na(Urban) | is.na(Poorest) | is.na(Richest)))
cleaned_lower_sec <- lower_sec %>%
  filter(!(is.na(Rural) | is.na(Urban) | is.na(Poorest) | is.na(Richest)))
cleaned_upper_sec <- upper_sec %>%
  filter(!(is.na(Rural) | is.na(Urban) | is.na(Poorest) | is.na(Richest)))


# Group by Region and Income level
regional_data <- total %>%
  group_by(Region, Income.Group) %>%
  summarize(
    Mean_Total_Connectivity = mean(Total, na.rm = TRUE),
    Median_Total_Connectivity = median(Total, na.rm = TRUE)
  )

# Bar plot: Mean digital connectivity by region and income group
ggplot(regional_data, aes(x = Region, y = Mean_Total_Connectivity, fill = Income.Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip()+
  labs(title = "Mean Digital Connectivity by Region and Income Group", y = "Mean Connectivity (%)", x = "Region")

# Boxplot: Connectivity distribution by income group
ggplot(total, aes(x = Income.Group, y = Total, fill = Income.Group)) +
  geom_boxplot() +
  labs(title = "Distribution of Digital Connectivity by Income Group", y = "Digital Connectivity (%)", x = "Income Group")


