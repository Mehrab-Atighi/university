# Load necessary libraries
library(fitdistrplus)
library(dplyr)
library(ggplot2)

# Load your dataset (replace 'danish_dataset.csv' with the path to your dataset)
data("danishuni")
# Ensure your date column is in Date format (replace 'Date' with the actual column name if different)

# Find new all-time high loss values
data <- danishuni %>%
  arrange(Date) %>%
  mutate(is_new_record = cumsum(Loss == cummax(Loss)))

# Count the number of new records per year
new_records_per_year <- data %>%
  filter(is_new_record == 1) %>%
  mutate(year = format(Date, "%Y")) %>%
  group_by(year) %>%
  summarise(count = n())

# Plot the number of new records per year
ggplot(data, aes(x = Date, y = is_new_record)) +
  geom_line() +
  geom_point() +
  labs(title = "Number of New All-Time High Loss Records Per Year",
       x = "Year",
       y = "Number of New Records") +
  theme_minimal()
