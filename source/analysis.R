library(tidyverse)

# The functions might be useful for A4
source("../source/a4-helpers.R")

fname <- "../data/incarceration_trends.csv"
df <- read.csv(fname, nrows=-1)

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function tells is the population of people incarcerated each year 
get_year_jail_pop <- function() {
  year <- df %>%
    select(year, total_jail_pop) %>%
    drop_na() %>%
    group_by(year) %>%
    summarise(year, total_jail_pop)
  year <- aggregate(total_jail_pop~.,year, FUN=sum)
return(year)   
}
get_year_jail_pop()
# This function plots the amount of people in jail each year 
plot_jail_pop_for_us <- function()  {
  year_jail <- get_year_jail_pop()
  total_pop <- ggplot(year_jail, aes(x=year, y=total_jail_pop)) +
    geom_bar(stat='identity')
  return(total_pop)
} 
plot_jail_pop_for_us()
## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
#This returns a data frame of jail populations in each state 
get_jail_pop_by_states <- function(states) {
  change <- df %>%
    drop_na() %>%
    select(total_jail_pop, state, year)
  selected <- change %>%
    filter(state %in% states)
  return(selected)
}

# This function plots a line graph of the jail population in each state 
plot_jail_pop_by_states <- function(states)  {
  line_graph <- ggplot(get_jail_pop_by_states(states), aes(x=year, y=total_jail_pop, color = state))+
    stat_summary(fun="mean", geom = "line") + xlab("Year") +
    ylab("Jail Population") +
    ggtitle("Incarceration trend in the US from 1970-2018")
  return(line_graph)
}
plot_jail_pop_by_states(c("CA", "WA"))
  



## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
#This function returns a bar chart of the percent change in the number of women incarcerated 
women_change <- df %>% 
  group_by(year) %>% 
  filter(year > 1990) %>% 
  summarize(total_women_pop = sum(female_jail_pop, na.rm = TRUE)) %>% 
  mutate(percent_change = ((total_women_pop - lag(total_women_pop, n = 1)) 
                           / total_women_pop * 100))

percent_change_graph <- ggplot(data = women_change) +
  geom_col(aes(x = year, y = percent_change), fill='blue', color="black") +
  labs(title = "Percent Change of the Number of Women Incarcerated (1990 to 2018)", 
       x = "Year", 
       y = "Percent change") +
  scale_y_continuous(limits=c(-7, 15, by = 1)) +
  scale_x_continuous(limits=c(1990,2018, by = 1))

percent_change_graph

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
data <- df %>% 
  group_by(state) %>% 
  summarize(total_jail_pop = sum(total_jail_pop, na.rm = TRUE))
shape <- map_data("state")
state_name <- data.frame(state.abb, state.name)

data <- left_join(data, state_name, by = c('state' = 'state.abb')) %>% 
  mutate(region = tolower(state.name))

shape <- left_join(shape, data)

map <- ggplot(shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = total_jail_pop)) + 
  scale_fill_continuous(low = 'white', high ='black', labels = scales::label_number_si()) +
  coord_map() +
  labs(title = "Individuals incarcerated in the United States")
map

## Load data frame ---- 


