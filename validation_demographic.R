library(epicR)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(scales)
library(dplyr)
library(tidyr)

# load US population validation targets
USSimulation <-read.csv("USSimulation.csv", header = TRUE)
USlifetables <-read.csv("USLifeTables.csv", header = FALSE)
USlifetables_num <- as.matrix(USlifetables)
USlifetables_num <- apply(USlifetables_num, 2, as.numeric)


# load EPIC
settings<- get_default_settings()
settings$record_mode <- 0
n_sim <- settings$n_base_agents
# n_sim <- 100000
init_session(settings = settings)
input <- Cget_inputs()

## set time horizon
time_horizon <- 56
input$values$global_parameters$time_horizon <- time_horizon

## set growth rate

growthrate <- c(-3.5,0.002,0.00001) #one calibration target


input$values$agent$l_inc_betas <- growthrate
input$values$agent$p_bgd_by_sex <- USlifetables_num

## run EPIC
run(input=input$values)
inputs <- Cget_inputs()
output <- Cget_output_ex()


terminate_session()

#CREATE EPIC PROJECTIONS FOR TOTAL POPULATION

## create dataframe for US census
total_population<- USSimulation %>%
  filter(age >= 40 & age <= 100) %>%   # Select ages 40-100
  group_by(year) %>%                   # Group data by year
  summarise(total_population = sum(value, na.rm = TRUE))


## create dataframe for total EPIC population size
epic_totalpopsize<- data.frame(1:time_horizon,output$n_alive_by_ctime_sex)
epic_totalpopsize <- epic_totalpopsize %>%
  rename(year = X1.time_horizon) %>%               # Rename column
  mutate(EPIC_totalpopulation = X1 + X2) %>%      # Sum X1 and X2
  select(year, EPIC_totalpopulation)

# merge US total population and EPIC total population outputs
epic_totalpopsize <- epic_totalpopsize %>%
  mutate(year = year + 2014)              #change EPIC to calender year instead of numbers (e.g. 1, 2, 3)
validate_totalpop_size <-  total_population %>%
  left_join(epic_totalpopsize, by=c("year"))

# Scale population: Set EPIC_output_scaled to be equal to US_popsize for the year 2015
validate_totalpop_size_scaled <- validate_totalpop_size %>%
  mutate(EPIC_totaloutput_scaled = ifelse(year == 2015, total_population, NA))


# Step 2: Group by year and calculate total EPIC_output for each year
epic_by_year <- validate_totalpop_size_scaled %>%
  group_by(year) %>%
  summarise(EPIC_output = sum(EPIC_totalpopulation, na.rm = TRUE))

# Step 3: Calculate the year-over-year growth rate for total population
epic_by_year <- epic_by_year %>%
  arrange(year) %>%
  mutate(growth_rate = (EPIC_output / lag(EPIC_output)))

# View the results with year, total population, and growth rate
print(epic_by_year)


# Step 4: Merge the growth rate back into the original dataset (validate_pop_size)
df_with_growth_EPICtotal <- validate_totalpop_size_scaled %>%
  left_join(epic_by_year, by = "year")


# Step 5: Create a new column multiplying the individual EPIC_output by the growth rate
df_with_growth_EPICtotal <- df_with_growth_EPICtotal %>%
  arrange(year) %>%
  mutate(EPIC_totaloutput_scaled = ifelse(year == 2015, total_population, NA))

# Step 6: Iteratively calculate EPIC_output_scaled for each year after 2015
# For each age group, calculate the population estimate year by year
for (yr in 2016:max(df_with_growth_EPICtotal$year)) {
  df_with_growth_EPICtotal <- df_with_growth_EPICtotal %>%
    mutate(EPIC_totaloutput_scaled = ifelse(year == yr,
                                            lag(EPIC_totaloutput_scaled) * growth_rate,
                                            EPIC_totaloutput_scaled))
}

# Prepare the data to plot
df_plot <- df_with_growth_EPICtotal %>%
  filter(year <= 2050) %>%
  select(year, total_population, EPIC_totaloutput_scaled) %>%
  pivot_longer(cols = c(total_population, EPIC_totaloutput_scaled),
               names_to = "Population_Type",
               values_to = "Population")

# Assuming df_plot is your data frame
p <- ggplot(df_plot, aes(x = year, y = Population, color = Population_Type)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +  # Points to highlight years
  theme_tufte(base_size = 14, ticks = FALSE) +
  ggtitle("Comparison of EPIC vs. US Population Over Time") +
  scale_y_continuous(name = "Population", labels = comma, limits = c(0, NA)) +  # Fixed y-axis to start at 0
  scale_x_continuous(name = "Year", breaks = seq(min(df_plot$year), max(df_plot$year), by = 2)) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )

# Display the plot
print(p)


rmse <- sqrt(mean((df_with_growth_EPICtotal$total_population -
                     df_with_growth_EPICtotal$EPIC_totaloutput_scaled)^2, na.rm = TRUE))
print(rmse)


p_closer <- ggplot(df_plot, aes(x = year, y = Population, color = Population_Type)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +  # Points to highlight years
  theme_tufte(base_size = 14, ticks = FALSE) +
  ggtitle("Comparison of EPIC vs. US Population Over Time") +
  scale_y_continuous(name = "Population", labels = comma) +  # Fixed y-axis to start at 0
  scale_x_continuous(name = "Year", breaks = seq(min(df_plot$year), max(df_plot$year), by = 2)) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )

print(p_closer)




#CREATE EPIC PROJECTIONS BY INDIVIDUAL AGES

## create dataframe of EPIC population size by age
epic_popsize_age<- data.frame(1:time_horizon,output$n_alive_by_ctime_age)
colnames(epic_popsize_age)[1] <- "year"
colnames(epic_popsize_age)[2:ncol(epic_popsize_age)] <- 1:(ncol(epic_popsize_age) - 1)
epic_popsize_age <- epic_popsize_age[, -(2:40)]
epic_popsize_age$year <- seq(2015, by = 1, length.out = nrow(epic_popsize_age))
epic_popsize_age_long <- epic_popsize_age %>%
  pivot_longer(!year, names_to = "age", values_to = "EPIC_popsize") %>%
  mutate(age=as.integer(age))
# merge US sim and EPIC pop outputs
validate_pop_size <-  USSimulation %>%
  rename(US_popsize=value) %>%
  left_join(epic_popsize_age_long, by=c("year","age"))

# Scale population: Set EPIC_output_scaled to be equal to US_popsize for the year 2015
validate_pop_size_scaled <- validate_pop_size %>%
  mutate(EPIC_output_scaled = ifelse(year == 2015, US_popsize, NA))

## Below are steps to automate growth rate calculations for the EPIC population

# Step 1: Filter for ages between 40 and 100
df_filtered <- validate_pop_size_scaled %>%
  filter(age >= 40 & age <= 100)

# Step 2: Group by year and calculate total EPIC_output for each year
total_epic_by_year <- validate_pop_size_scaled %>%
  group_by(year) %>%
  summarise(total_EPIC_output = sum(EPIC_popsize, na.rm = TRUE))

# Step 3: Calculate the year-over-year growth rate for total population
total_epic_by_year <- total_epic_by_year %>%
  arrange(year) %>%
  mutate(growth_rate = (total_EPIC_output / lag(total_EPIC_output)))

# View the results with year, total population, and growth rate
print(total_epic_by_year)

# Step 4: Merge the growth rate back into the original dataset (validate_pop_size)
df_with_growth <- validate_pop_size_scaled %>%
  left_join(total_epic_by_year, by = "year")

# Step 5: Create a new column multiplying the individual EPIC_output by the growth rate
df_with_growth <- df_with_growth %>%
  arrange(year, age) %>%
  group_by(age) %>%
  mutate(EPIC_output_scaled = ifelse(year == 2015, US_popsize, NA))

# Step 6: Iteratively calculate EPIC_output_scaled for each year after 2015
# For each age group, calculate the population estimate year by year
for (yr in 2016:max(df_with_growth$year)) {
  df_with_growth <- df_with_growth %>%
    group_by(age) %>%
    mutate(EPIC_output_scaled = ifelse(year == yr,
                                       lag(EPIC_output_scaled) * growth_rate,
                                       EPIC_output_scaled))
}

# View the updated dataframe
filtered_data <- df_with_growth %>%
  filter(year == 2029) %>%
  select(US_popsize,EPIC_popsize, EPIC_output_scaled)

# Print the filtered data
print(filtered_data)

# View the final dataset
print(df_with_growth)


# Define the 'year' and 'savePlots' variables as needed

savePlots <- TRUE  # Set this to TRUE if you want to save the plot, FALSE if you don't
year <- 2060  # Example year, adjust as needed

# Prepare the data for plotting
dfUSpop <- data.frame(population = df_with_growth$US_popsize[df_with_growth$year == 2035],
                      age = df_with_growth$age[df_with_growth$year == 2035])
dfEPICpop <- data.frame(population = df_with_growth$EPIC_output_scaled[df_with_growth$year == 2035],
                        age = df_with_growth$age[df_with_growth$year == 2035])
# Make the simulated population negative for the pyramid effect
dfEPICpop$population <- dfEPICpop$population * (-1)

# Create the plot
p <- ggplot(NULL, aes(x = age, y = population)) +
  theme_tufte(base_size = 14, ticks = FALSE) +  # Minimal theme
  geom_bar(aes(fill = "EPIC"), data = dfEPICpop, stat = "identity", alpha = 0.5) +  # Simulated population in blue
  geom_bar(aes(fill = "US Population"), data = dfUSpop, stat = "identity", alpha = 0.5) +  # Predicted population in red
  theme(axis.title = element_blank()) +  # Remove axis titles
  ggtitle("EPIC vs. US Population") +  # Add plot title
  theme(legend.title = element_blank()) +  # Remove legend title
  scale_y_continuous(name = "Population", labels = scales::comma) +  # Format y-axis with commas
  scale_x_continuous(name = "Age", labels = scales::comma)  # Format x-axis for age
# Save the plot if 'savePlots' is TRUE
if (savePlots) {
  ggsave(paste0("Population_", year, ".tiff"), plot = p, device = "tiff", dpi = 300)
}

# Display the plot
plot(p)

## EXTRA CODE

# create dataframe of EPIC population size by sex
epic_popsize_sex<- data.frame(1:time_horizon,output$n_alive_by_ctime_sex)
names(epic_popsize_sex) <- c("year", "male", "female")
epic_popsize_sex$overall<- epic_popsize_sex$male + epic_popsize_sex$female
epic_popsize_sex$year <- seq(1, nrow(epic_popsize_sex))

# t-test
library(dplyr)
validate_pop_size_2035 <- USpop_scaled %>% filter(year == 2035)
result_2035 <- t.test(validate_pop_size_2035$US_popsize_scaled, validate_pop_size_2035$EPIC_popsize_scaled, paired = TRUE)

validate_pop_size_2065 <- USpop_scaled %>% filter(year == 2065)
result_2035 <- t.test(validate_pop_size_2035$US_popsize_scaled, validate_pop_size_2035$EPIC_popsize_scaled, paired = TRUE)

## possible calibration settings for growth rate
c(-4.05,0.018,0)
c(-4.75,0,0.003)



