# load libraries
library(tidyverse)

# read uploaded vehicles.csv file
vehicles <- read.csv("vehicles.csv")

# quick look at dataset
names(vehicles)          # print column names
str(vehicles)            # show structure
summary(vehicles)        # summary stats

# check what fuelType1 values exist
table(vehicles$fuelType1)

# create a fuel category;
# groups specific fuel types into broader categories
vehicles <- vehicles %>%
  mutate(
    fuel_category = case_when(
      # pure electric
      fuelType1 == "Electricity" ~ "Electric",
      
      # plug-in hybrids (electricity + liquid fuel)
      (!is.na(fuelType2) & fuelType2 == "Electricity") ~ "Hybrid",
      
      # standard hybrids (model name or ATV description includes 'Hybrid')
      grepl("Hybrid", model, ignore.case = TRUE) ~ "Hybrid",
      grepl("Hybrid", atvType, ignore.case = TRUE) ~ "Hybrid",
      grepl("Hybrid", eng_dscr, ignore.case = TRUE) ~ "Hybrid",
      
      # gas/diesel vehicles
      grepl("Gasoline|Diesel|Ethanol", fuelType1) ~ "Combustion",
      
      # alt fuels (hydrogen/natural gas/unknown)
      TRUE ~ "Other"
    )
  )

# create period-based data subsets;
# split into three 5-year windows
vehicles_11_15 <- vehicles %>% filter(year >= 2011 & year <= 2015)
vehicles_16_20 <- vehicles %>% filter(year >= 2016 & year <= 2020)
vehicles_21_25 <- vehicles %>% filter(year >= 2021 & year <= 2025)
vehicles_11_25 <- vehicles %>% filter(year >= 2011 & year <= 2025)

# count vehicles of each fuel category per year
fuelTrend <- vehicles %>%
  count(year, fuel_category)

# line chart showing fuel type trends over time
ggplot(fuelTrend,
       aes(x = year, y = n, color = fuel_category)) +
  geom_line(size = 1.2, alpha = 0.4) +
  scale_color_manual(values = c(
    "Electric"       = "#56B4E9",
    "Hybrid"         = "#009E73",
    "Combustion"     = "#D55E00",
    "Other"          = "#F0E442"
  )) +
  labs(title = "Trends in Fuel Types Over Time",
       x = "Year",
       y = "Number of Vehicles",
       color = "Fuel Category") +
  theme_minimal()

# boxplot comparing MPG distributions for fuel categories (2011–2025)
ggplot(vehicles_11_25,
       aes(x = factor(fuel_category,
                      levels = c("Combustion", "Hybrid", "Other", "Electric")),
           y = comb08,
           fill = fuel_category)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c(
    "Electric"       = "#56B4E9",
    "Hybrid"         = "#009E73",
    "Combustion"     = "#D55E00",
    "Other"          = "#F0E442"
  )) +
  labs(title = "Distribution of Combined MPG by Fuel Category (2011–2025)",
       x = "Fuel Category",
       y = "Combined MPG") +
  theme_minimal() +
  theme(legend.position = "none")

# function: build stacked MPG chart for a period
topMakesFuelmpgchart <- function(data, title_text) {
  
  # 1) find top 20 makes by average combined MPG
  topMakes <- data %>%
    group_by(make) %>%
    summarize(avg_comb_mpg = mean(comb08, na.rm = TRUE)) %>%
    arrange(desc(avg_comb_mpg)) %>%
    slice_head(n = 20)
  
  # 2) for those makes, calculate fuel mix proportions
  fuelShare <- data %>%
    semi_join(topMakes, by = "make") %>%   # keep rows for the top 20 makes
    count(make, fuel_category) %>%          # count rows per make + fuel type
    group_by(make) %>%
    mutate(prop = n / sum(n))               # share of each fuel type within that make
  
  # 3) join with avg MPG and compute segment length in MPG units
  fuelMPG <- fuelShare %>%
    left_join(topMakes, by = "make") %>%
    mutate(segMPG = avg_comb_mpg * prop)   # segment size = avg MPG * fuel-type share
  
  # 4) build stacked MPG bar chart for selected period
  ggplot(fuelMPG,
         aes(y = reorder(make, avg_comb_mpg),
             x = segMPG,
             fill = fuel_category)) +
    geom_col() +
    scale_fill_manual(values = c(
      "Electric"   = "#56B4E9",
      "Hybrid"     = "#009E73",
      "Combustion" = "#D55E00",
      "Other"      = "#F0E442"
    )) +
    labs(title = title_text,
         x = "Average Combined MPG (stacked by fuel mix)",
         y = "Make",
         fill = "Fuel Category") +
    theme_minimal()
}

# build charts for each period
chart_11_15 <- topMakesFuelmpgchart(
  vehicles_11_15,
  "Top 20 Most Fuel-Efficient Makes (2011–2015)"
)

chart_16_20 <- topMakesFuelmpgchart(
  vehicles_16_20,
  "Top 20 Most Fuel-Efficient Makes (2016–2020)"
)

chart_21_25 <- topMakesFuelmpgchart(
  vehicles_21_25,
  "Top 20 Most Fuel-Efficient Makes (2021–2025)"
)

# display charts
chart_11_15
chart_16_20
chart_21_25
