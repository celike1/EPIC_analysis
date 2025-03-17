# This code helps you make plots for each specific age. For example you can compare the US census vs EPIC projections for 40 year olds.
library(epicUS)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(scales)
library(dplyr)
library(tidyr)


USSimulation <- read_csv("US_analysis/data/USSimulation.csv")

settings <- get_default_settings()
settings$record_mode <- 0
settings$n_base_agents <- settings$n_base_agents
init_session(settings = settings)

input <- get_input()
time_horizon <- 56
input$values$global_parameters$time_horizon <- time_horizon

input$values$agent$l_inc_betas <- c(-3.5,0.002,0.00001)

run(input = input$values)
output <- Cget_output_ex()
terminate_session()


epic_popsize_age <- data.frame(year = seq(2015, by = 1, length.out = time_horizon),
                               output$n_alive_by_ctime_age)
colnames(epic_popsize_age)[2:ncol(epic_popsize_age)] <- 1:(ncol(epic_popsize_age) - 1)
epic_popsize_age <- epic_popsize_age[, -(2:40)]
epic_popsize_age_long <- epic_popsize_age %>%
  pivot_longer(!year, names_to = "age", values_to = "EPIC_popsize") %>%
  mutate(age=as.integer(age))


validate_pop_size_scaled <- USSimulation %>%
  rename(US_popsize = value) %>%
  left_join(epic_popsize_age_long, by = c("year", "age")) %>%
  mutate(EPIC_output_scaled = ifelse(year == 2015, US_popsize, NA))


total_epic_by_year <- validate_pop_size_scaled %>%
  group_by(year) %>%
  summarise(total_EPIC_output = sum(EPIC_popsize, na.rm = TRUE)) %>%
  arrange(year) %>%
  mutate(growth_rate = total_EPIC_output / lag(total_EPIC_output))


df_with_growth <- validate_pop_size_scaled %>%
  left_join(total_epic_by_year, by = "year") %>%
  arrange(year, age) %>%
  group_by(age) %>%
  mutate(
    EPIC_output_scaled = ifelse(year == 2015, US_popsize, NA),
    EPIC_output_scaled = replace_na(EPIC_output_scaled, first(US_popsize)) *
      cumprod(replace_na(growth_rate, 1))
  )



df_plot <- df_with_growth %>%
  filter(year <= 2050, age == 40) %>%
  select(year, US_popsize, EPIC_output_scaled,age) %>%
  pivot_longer(cols = c(US_popsize, EPIC_output_scaled),
               names_to = "Population_Type",
               values_to = "Population")

p <- ggplot(df_plot, aes(x = year, y = Population, color = Population_Type)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  theme_tufte(base_size = 14, ticks = FALSE) +
  ggtitle("Comparison of EPIC vs. US Population Over Time") +
  scale_y_continuous(name = "Population", labels = comma) +
  scale_x_continuous(name = "Year", breaks = seq(min(df_plot$year), max(df_plot$year), by = 2)) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )

print(p)

