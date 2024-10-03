total <- read.csv("total school age.csv", header = TRUE)
total <- total[-1,]
str(total)

# formatting Time.period into year format
total$Time.period <- as.factor(total$Time.period)
total$Time.period <- as.Date(total$Time.period, format = '%Y')
total$Time.period <- year(total$Time.period)
total <- total[total$Time.period < year(Sys.Date()),]

total$Total <- as.numeric(gsub("%", "", total$Total))
total$Rural <- as.numeric(gsub("%", "", total$Rural))
total$Urban <- as.numeric(gsub("%", "", total$Urban))
total$Poorest <- as.numeric(gsub("%", "", total$Poorest))
total$Richest <- as.numeric(gsub("%", "", total$Richest))

is.na(total)
UM <- total %>%
  filter(Income.Group == "Upper middle income (UM)")



ggplot(total, aes(Total, Color = Countries.and.areas)) +
  geom_bar()
