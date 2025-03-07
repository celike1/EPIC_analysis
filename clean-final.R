library(tidyverse)
library(epicUS)
library(ggthemes)
library(scales)

# Load US population validation targets
USSimulation <- read_csv("USSimulation.csv")
USlifetables <- read_csv("USLifeTables.csv", col_names = FALSE) %>% mutate(across(everything(), as.numeric))

# Load EPIC and configure settings
settings <- get_default_settings()
settings$record_mode <- 0
settings$n_base_agents <- settings$n_base_agents  # Ensure default setting
init_session(settings = settings)

input <- Cget_inputs()
time_horizon <- 56
input$values$global_parameters$time_horizon <- time_horizon
input$values$agent$p_bgd_by_sex <- as.matrix(USlifetables)

# Set growth rate calibration
input$values$agent$l_inc_betas <- c(-3.4,0.0002,0.000001)
input$values$agent$ln_h_bgd_betas <- c(intercept = 0, y = -0.015, y2 = 0, age = 0, b_mi = 0, n_mi = 0, b_stroke = 0,
                                       n_stroke = 0, hf = 0)

# Run EPIC simulation
run(input = input$values)
output <- Cget_output_ex()
terminate_session()

# Create US census data
total_population <- USSimulation %>%
  filter(between(age, 40, 100)) %>%
  group_by(year) %>%
  summarise(total_population = sum(value, na.rm = TRUE), .groups = "drop")

# Create EPIC population size tibble
epic_totalpopsize <- tibble(
  year = 1:time_horizon + 2014,  # Convert to calendar year in one step
  EPIC_totalpopulation = rowSums(output$n_alive_by_ctime_sex, na.rm = TRUE)
)

# Merge US and EPIC total population data
validate_totalpop_size <- total_population %>%
  left_join(epic_totalpopsize, by = "year")

# Scale population: Set EPIC_output_scaled for 2015
validate_totalpop_size_scaled <- validate_totalpop_size %>%
  mutate(EPIC_totaloutput_scaled = if_else(year == 2015, total_population, NA))

# Compute total EPIC output, growth rate, and scaled output efficiently
df_with_growth_EPICtotal <- validate_totalpop_size_scaled %>%
  group_by(year) %>%
  summarise(EPIC_output = sum(EPIC_totalpopulation, na.rm = TRUE), .groups = "drop") %>%
  arrange(year) %>%
  mutate(growth_rate = EPIC_output / lag(EPIC_output)) %>%
  left_join(validate_totalpop_size_scaled, by = "year") %>%
  arrange(year) %>%
  mutate(
    EPIC_totaloutput_scaled = replace_na(EPIC_totaloutput_scaled, total_population[year == 2015]),
    EPIC_totaloutput_scaled = EPIC_totaloutput_scaled[1] * cumprod(replace_na(growth_rate, 1))
  )

# Prepare the data to plot
df_plot <- df_with_growth_EPICtotal %>%
  filter(year <= 2050) %>%
  select(year, total_population, EPIC_totaloutput_scaled) %>%
  pivot_longer(cols = c(total_population, EPIC_totaloutput_scaled),
               names_to = "Population_Type",
               values_to = "Population")

# Create the plot with year on the x-axis and population on the y-axis
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


