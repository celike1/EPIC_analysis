library(epicUS)
library(tidyverse)
library(nloptr)

USSimulation <- read_csv("USSimulation.csv")
USlifetables <- read_csv("USLifeTables.csv", col_names = FALSE) %>% mutate(across(everything(), as.numeric))

settings <- get_default_settings()
settings$record_mode <- 0
settings$n_base_agents <- settings$n_base_agents

input <- Cget_inputs()
time_horizon <- 56
input$values$global_parameters$time_horizon <- time_horizon
input$values$agent$p_bgd_by_sex <- as.matrix(USlifetables)

# census data
total_population <- USSimulation %>%
  filter(between(age, 40, 100)) %>%
  group_by(year) %>%
  summarise(total_population = sum(value, na.rm = TRUE), .groups = "drop")


# RMSE function for optimization
calculate_rmse_optim <- function(growthrate) {
  init_session(settings = settings)
  input$values$agent$l_inc_betas <- growthrate
  run(input=input$values)
  output <- Cget_output_ex()
  terminate_session()

  epic_totalpopsize <- tibble(
    year = 1:time_horizon + 2014,
    EPIC_totalpopulation = rowSums(output$n_alive_by_ctime_sex, na.rm = TRUE)
  )

  validate_totalpop_size <- total_population %>%
    left_join(epic_totalpopsize, by = "year")

  validate_totalpop_size_scaled <- validate_totalpop_size %>%
    mutate(EPIC_totaloutput_scaled = if_else(year == 2015, total_population, NA_real_))

  df_with_growth_EPICtotal <- validate_totalpop_size_scaled %>%
    group_by(year) %>%
    summarise(EPIC_output = sum(EPIC_totalpopulation, na.rm = TRUE), .groups = "drop") %>%
    arrange(year) %>%
    mutate(growth_rate = (EPIC_output / lag(EPIC_output))) %>%
    left_join(validate_totalpop_size_scaled, by = "year") %>%
    arrange(year) %>%
    mutate(
      EPIC_totaloutput_scaled = replace_na(EPIC_totaloutput_scaled, total_population[year == 2015]),
      EPIC_totaloutput_scaled = EPIC_totaloutput_scaled[1] * cumprod(replace_na(growth_rate, 1))
    )

  rmse <- sqrt(mean((df_with_growth_EPICtotal$total_population -
                       df_with_growth_EPICtotal$EPIC_totaloutput_scaled)^2, na.rm = TRUE))


  print(paste("Growth Rate:", paste(growthrate, collapse = ", ")))
  print(paste("RMSE:", rmse))


  return(rmse)
}

initial_guess <- c(-3.50,0.0005,-0.000001)

# here i am using L-BFGS-B because RMSE is a differentiable function
# we can define lower and upper bounds,
#more memory efficient in BFGS, making it suitable for large parameter spaces.
result_optim <- optim(
  par = initial_guess,
  fn = calculate_rmse_optim,
  lower = c(-3.60,0.0001,-0.00001), upper = c(-3.40,0.001,-0.000001),
  method = "L-BFGS-B",
  control = list(maxit = 20)
)




print(paste("Optimal Growth Rate:", result_optim$par))
print(paste("Optimized RMSE:", result_optim$value))



