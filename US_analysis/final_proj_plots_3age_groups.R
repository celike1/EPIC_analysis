library(epicUS)
packageVersion("epicUS")
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


# input$values$agent$l_inc_betas <- c(-3.5,0.002,0.00001)
params <- c(-3.48672063032448, 0.00202274171887977, -4.37035899506131e-05, 0, -1e-04, 0, -0.000132065698144107)
input$values$agent$l_inc_betas <- c(params[1], params[2], params[3])


# intercept, y, y2, age
input$values$agent$ln_h_bgd_betas <- c(params[4], params[5], params[6], params[7], b_mi = 0, n_mi = 0, b_stroke = 0,
                                       n_stroke = 0, hf = 0)



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


df_summed_ranges <- df_with_growth %>%
  mutate(
    age_group = case_when(
      age >= 40 & age <= 59 ~ "40-59",
      age >= 60 & age <= 79 ~ "60-79",
      age >= 80 ~ "80+"
    )
  ) %>%
  group_by(year, age_group) %>%
  summarise(total_EPIC_population = sum(EPIC_output_scaled, na.rm = TRUE),
            total_US_population = sum(US_popsize, na.rm = TRUE))

rmse_per_range <- df_summed_ranges %>%
  group_by(age_group) %>%
  summarise(
    rmse = sqrt(mean((total_EPIC_population - total_US_population)^2, na.rm = TRUE)),
    .groups = "drop"
  )



# Loop through unique age groups and generate a plot for each
for(age_grp in unique(df_summed_ranges$age_group)) {

  df_plot <- df_summed_ranges %>%
    gather(key = "Population_Type", value = "Population", total_EPIC_population, total_US_population) %>%
    filter(year <= 2050, age_group == age_grp)

  p <- ggplot(df_plot, aes(x = year, y = Population, color = Population_Type)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2) +
    theme_tufte(base_size = 14, ticks = FALSE) +
    ggtitle(paste("Comparison of EPIC vs. US Population Over Time for Age Group", age_grp)) +
    scale_y_continuous(name = "Population", labels = comma) +
    scale_x_continuous(name = "Year", breaks = seq(min(df_plot$year), max(df_plot$year), by = 2)) +
    expand_limits(y = 0) +
    theme(
      legend.title = element_blank(),
      legend.position = "bottom"
    )

  print(p)
  Sys.sleep(2)
}


print(rmse_per_range)


