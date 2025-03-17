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
settings$n_base_agents <- settings$n_base_agents  # Ensure default setting
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


df_with_growth_age_grouped <- df_with_growth %>%
  mutate(age_group = cut(age, breaks = c(seq(40, 100, by = 5), Inf),
                         include.lowest = TRUE, right = FALSE,
                         labels = c(paste(seq(40, 95, by = 5), seq(44, 99, by = 5), sep = "-"), "100+"))) %>%
  mutate(age_group = factor(age_group,
                            levels = c("40-44", "45-49", "50-54", "55-59", "60-64", "65-69",
                                       "70-74", "75-79", "80-84", "85-89", "90-94", "95-99", "100+"),
                            ordered = TRUE)) %>%
  group_by(year, age_group) %>%
  summarise(EPIC_output_scaled = sum(EPIC_output_scaled, na.rm = TRUE),
            US_popsize = sum(US_popsize, na.rm = TRUE)) %>%
  ungroup()


df_plot <- df_with_growth_age_grouped %>%
  filter(year <= 2050) %>%
  select(year, US_popsize, EPIC_output_scaled, age_group) %>%
  pivot_longer(cols = c(US_popsize, EPIC_output_scaled),
               names_to = "Population_Type",
               values_to = "Population")

unique_age_groups <- unique(df_plot$age_group)

for (age in unique_age_groups) {
  p <- ggplot(df_plot %>% filter(age_group == age),
              aes(x = year, y = Population, color = Population_Type)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2) +
    theme_tufte(base_size = 14, ticks = FALSE) +
    ggtitle(paste("EPIC vs. US Population Over Time for", age)) +
    scale_y_continuous(name = "Population", labels = scales::comma) +
    scale_x_continuous(name = "Year", breaks = seq(min(df_plot$year), max(df_plot$year), by = 2)) +
    expand_limits(y=0) +
    theme(legend.title = element_blank(), legend.position = "bottom")

  print(p)
  Sys.sleep(2)
}





