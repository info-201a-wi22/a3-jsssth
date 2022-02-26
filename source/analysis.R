# Assignment 3: Incarceration.

# These libraries are required to run all of my code.
library(tidyverse)
library(ggplot2)
library(ggthemes)

# This function gets the incarceration data from the Vera Institude of Justice.
load_incarceration_data <- function() {
  filename <- "https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv"
  df <- read.csv(filename, header = TRUE, stringsAsFactors = FALSE)
  return(df)
}

# This function gets the state data from jasonong on GitHub.
load_state_data <- function() {
  filename <- "https://raw.githubusercontent.com/12ketan/List-of-US-States/master/states.csv"
  df <- read.csv(filename, header = TRUE, stringsAsFactors = FALSE)
  return(df)
}

# This loads the data to the incarceration data frame.
incarceration <- load_incarceration_data()

# Five variables.

# This wrangles the incarceration data frame by key years and provides two extra ratios.
population_data <- incarceration %>% 
  filter(year < "2011", year > "1999") %>% 
  group_by(year) %>%
  summarise(total_US_pop = sum(total_pop, na.rm = TRUE), 
            total_jail_population = sum(total_jail_pop, na.rm = TRUE),
            total_prison_population = sum(total_prison_pop, na.rm = TRUE),
            total_black_jail = sum(black_jail_pop, na.rm = TRUE),
            total_white_jail = sum(white_jail_pop, na.rm = TRUE),
            total_latinx_jail = sum(latinx_jail_pop, na.rm = TRUE),
            total_black_prison = sum(black_prison_pop, na.rm = TRUE),
            total_white_prison = sum(white_prison_pop, na.rm = TRUE),
            total_latinx_prison = sum(latinx_prison_pop, na.rm = TRUE)) %>% 
  mutate(jail_ratio = (total_jail_population/total_US_pop)*100) %>% 
  mutate(prison_ratio = (total_prison_population/total_US_pop)*100)

# Gives the year with the highest ratio of people in jail.
highest_jail_prop_year <- population_data %>%
  filter(max(jail_ratio) == jail_ratio) %>% 
  pull(year)

# Gives the number of people in jail for the year with the highest ratio.
highest_jail_pop <- population_data %>%
  filter(max(jail_ratio) == jail_ratio) %>% 
  pull(total_jail_population) %>% 
  round() %>% 
  prettyNum(big.mark = ",", scientific = FALSE)

# Gives the year with the highest ratio of people in prison.
highest_prison_prop_year <- population_data %>%
  filter(max(prison_ratio) == prison_ratio) %>% 
  pull(year)

# Gives the number of people in prison for the year with the highest ratio.
highest_prison_pop <- population_data %>%
  filter(max(prison_ratio) == prison_ratio) %>% 
  pull(total_prison_population) %>% 
  prettyNum(big.mark = ",", scientific = FALSE)

# Gives the year with the highest total population of black people in jail.
highest_black_jail_pop <- population_data %>%
  filter(max(total_black_jail) == total_black_jail) %>% 
  pull(year)



# Time Trend Chart.

chart_one <- ggplot(data = population_data, mapping = aes(x = year,)) +
  geom_line(mapping = aes(y = total_black_jail, color = "Black"), size = 1.5) +
  geom_line(mapping = aes(y = total_white_jail, color = "White"), size = 1.5) +
  geom_line(mapping = aes(y = total_latinx_jail, color = "Latinx"), size = 1.5) +
  scale_x_continuous(breaks = seq(2000, 2010, by = 2)) +
  labs(
    title = "Jail Population by Race",
    subtitle = "from Years 2000 to 2010",
    x = "Year",
    y = "Population",
    color = "Race"
  ) +
  theme_gdocs() +
  theme(axis.title = element_text())



# Variable Comparison Chart.

chart_two <- ggplot(data = population_data, mapping = aes(x = year,)) +
  geom_point(mapping = aes(y = total_black_prison, color = "Black"), size = 1.9) +
  geom_point(mapping = aes(y = total_white_prison, color = "White"), size = 1.9) +
  scale_x_continuous(breaks = seq(2000, 2010, by = 1)) +
  labs(
    title = "Black and White Prison Population",
    subtitle = "from Years 2000 to 2010",
    x = "Year",
    y = "Population",
    color = "Race"
  ) +
  theme_gdocs() +
  theme(axis.title = element_text())



# Map.

# This loads state data to the states_data data frame.
states_data <- load_state_data() %>%
  mutate(state = str_to_lower(State)) %>%
  select(state, Abbreviation) %>% 
  rename(abv = Abbreviation)

# This prepares our mapping data and joins it with states_data.
graphing_2008_data <- incarceration %>% 
  filter("2008" == year) %>% 
  group_by(state) %>%
  summarise(total_black_jail = sum(black_pop_15to64, na.rm = TRUE)) %>% 
  rename(abv = state) %>%
  left_join(states_data, by = "abv")

# This loads a shapefile of U.S. states using ggplot's `map_data()` function and
#   joins it with my mapping data.
graphing_data_by_state <- map_data("state") %>%
  rename(state = region)  %>% 
  left_join(graphing_2008_data, by = "state") %>% 
  group_by(state) %>% 
  select(long, lat, state, total_black_jail, group)


# Define a minimalist theme for maps.
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(), # remove axis lines
    axis.text = element_blank(), # remove axis labels
    axis.ticks = element_blank(), # remove axis ticks
    axis.title = element_blank(), #remove axis titles
    plot.background = element_blank(), # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank() # remove border around plot
  )

# This create a map of U.S. states filled in by the number of black prisoners.
percent_black_jail_map <- ggplot(data = graphing_data_by_state) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = total_black_jail), 
               color = "white") +
  coord_map() +
  scale_fill_continuous(low = "#b4c2d1", high = "#8c152f") +
  blank_theme +
  labs(
    title = "Population of Black People in Jail by State in 2008",
    subtitle = "Data used only includes people ages 15 to 64",
    fill = "Number of People",
    caption = "Data Retrieved from: Vera Institute of Justice"
  )