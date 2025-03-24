library(tidyverse)
library(epicUS)
library(ggthemes)
library(scales)
library(ggplot2)
library(dplyr)

# Load US population validation targets
USSimulation <- read_csv("US_analysis/data/USSimulation.csv")
USlifetables <- read_csv("US_analysis/data/USLifeTables.csv", col_names = FALSE) %>% mutate(across(everything(), as.numeric))

# Load EPIC and configure settings
settings <- get_default_settings()
settings$record_mode <- 0
settings$n_base_agents <- 1e6
init_session(settings = settings)

input <- get_input()
time_horizon <- 1
input$values$global_parameters$time_horizon <- time_horizon
input$values$agent$p_bgd_by_sex <- as.matrix(USlifetables)

# Set growth rate calibration
input$values$agent$l_inc_betas <- c(-3.5,0.002,0.00001)
input$values$exacerbation$logit_p_death_by_sex <- cbind(
  male = c(intercept = -13000, age = log(1.05), mild = 0, moderate = 0, severe = 7.4,
           very_severe = 8, n_hist_severe_exac = 0),
  female = c(intercept = -13000, age = log(1.05), mild = 0, moderate = 0, severe = 7.4,
             very_severe = 8, n_hist_severe_exac = 0)
)


input$values$agent$ln_h_bgd_betas <- t(as.matrix(c(intercept = 0, y = 0, y2 = 0, age = 0,
                                                   b_mi = 0, n_mi = 0, b_stroke = 0,
                                                   n_stroke = 0, hf = 0)))

# Run EPIC simulation
run(input = input$values)
output <- Cget_output_ex()
terminate_session()

mortality_rate<-Cget_output_ex()$n_death_by_age_sex/Cget_output_ex()$sum_time_by_age_sex

mortality_rate<- mortality_rate[40:nrow(mortality_rate), ]

mortality_rate_df <- as.data.frame(mortality_rate)
colnames(mortality_rate_df) <- c("Male", "Female")

mortality_rate_df$Age <- seq(40, 40 + nrow(mortality_rate_df) - 1)

mortality_rate_df <- mortality_rate_df[, c("Age", "Male", "Female")]



####function

library(tidyverse)
library(epicUS)
library(ggthemes)
library(scales)

run_mortality_simulation <- function(n_agents = 1e6, time_horizon = 10) {
  # Load US population validation targets
  USSimulation <- read_csv("US_analysis/data/USSimulation.csv")
  USlifetables <- read_csv("US_analysis/data/USLifeTables.csv", col_names = FALSE) %>%
    mutate(across(everything(), as.numeric))

  # Load EPIC and configure settings
  settings <- get_default_settings()
  settings$record_mode <- 0
  settings$n_base_agents <- n_agents
  init_session(settings = settings)

  input <- get_input()
  input$values$global_parameters$time_horizon <- time_horizon
  input$values$agent$p_bgd_by_sex <- as.matrix(USlifetables)

  input$values$agent$l_inc_betas <- c(-3.5, 0.002, 0.00001)
  input$values$exacerbation$logit_p_death_by_sex <- cbind(
    male = c(intercept = -13000, age = log(1.05), mild = 0, moderate = 0, severe = 7.4,
             very_severe = 8, n_hist_severe_exac = 0),
    female = c(intercept = -13000, age = log(1.05), mild = 0, moderate = 0, severe = 7.4,
               very_severe = 8, n_hist_severe_exac = 0)
  )

  input$values$agent$ln_h_bgd_betas <- t(as.matrix(c(intercept = 0, y = 0, y2 = 0, age = 0,
                                                     b_mi = 0, n_mi = 0, b_stroke = 0,
                                                     n_stroke = 0, hf = 0)))

  # Run EPIC simulation
  run(input = input$values)
  output <- Cget_output_ex()
  terminate_session()

  mortality_rate <- output$n_death_by_age_sex / output$sum_time_by_age_sex

  mortality_rate <- mortality_rate[40:nrow(mortality_rate), ]

  mortality_rate_df <- as.data.frame(mortality_rate)
  colnames(mortality_rate_df) <- c("Male", "Female")

  mortality_rate_df$Age <- seq(40, 40 + nrow(mortality_rate_df) - 1)
    mortality_rate_df <- mortality_rate_df[, c("Age", "Male", "Female")]

  return(mortality_rate_df)
}

# Example usage: Adjust number of agents as needed
mortality_data1 <- run_mortality_simulation(n_agents = 1e6)
mortality_data2 <-run_mortality_simulation(n_agents = 1e7)
mortality_data3 <-run_mortality_simulation(n_agents = 3e7)


##getting US life

viz_EPICvsUS<- function (epic_data){

  init_session()
  input<-get_input()
  terminate_session()

  USlifetables_num <- input$values$agent$p_bgd_by_sex


  USlifetables_df <- data.frame(
    Age = 1:nrow(USlifetables_num),  # Start age from 1
    Male = USlifetables_num[, 1],
    Female = USlifetables_num[, 2]
  )

  common_ages <- intersect(epic_data$Age, USlifetables_df$Age)

  # filter both so only incmortality_data1# filter both so only include the rows with matching Age
  EPIC_filtered <- epic_data[epic_data$Age %in% common_ages, ]
  USlifetables_filtered <- USlifetables_df[USlifetables_df$Age %in% common_ages, ]

  combined_data_long <- bind_rows(
    EPIC_filtered %>% mutate(Source = "epicUS"),
    USlifetables_filtered %>% mutate(Source = "US Life Tables")
  ) %>%
    pivot_longer(cols = c("Male", "Female"), names_to = "Sex", values_to = "Death_Probability") %>%
    filter(Age > 40)

  ggplot(combined_data_long, aes(x = Age, y = Death_Probability, fill = Source)) +
    geom_col(position = "dodge", width = 1) +
    facet_wrap(~Sex) +
    labs(
      title = "Comparison of epicUS Death Probability vs. US Life Tables",
      x = "Age",
      y = "Death Probability",
      fill = "Source:"
    ) +
    theme_minimal()+
    theme(
      legend.position = "top",
      legend.justification = "center",
      plot.title = element_text(hjust = 0.5, margin = margin(b = 10))
    )

}


viz_EPICvsUS(mortality_data2)
viz_EPICvsUS(mortality_data3)

calculateRMSE <- function(epic_data) {
  init_session()
  input <- get_input()
  terminate_session()

  USlifetables_num <- input$values$agent$p_bgd_by_sex

  USlifetables_df <- data.frame(
    Age = 1:nrow(USlifetables_num),
    Male = USlifetables_num[, 1],
    Female = USlifetables_num[, 2]
  )

  common_ages <- intersect(epic_data$Age, USlifetables_df$Age)

  EPIC_filtered <- epic_data[epic_data$Age %in% common_ages, ]
  USlifetables_filtered <- USlifetables_df[USlifetables_df$Age %in% common_ages, ]

  rmse_male <- sqrt(mean((EPIC_filtered$Male - USlifetables_filtered$Male)^2, na.rm = TRUE))
  rmse_female <- sqrt(mean((EPIC_filtered$Female - USlifetables_filtered$Female)^2, na.rm = TRUE))

  overall_rmse <- sqrt(mean(c((EPIC_filtered$Male - USlifetables_filtered$Male)^2,
                              (EPIC_filtered$Female - USlifetables_filtered$Female)^2),
                            na.rm = TRUE))

  return(data.frame(Sex = c("Male", "Female", "Overall"), RMSE = c(rmse_male, rmse_female, overall_rmse)))
}


# Example usage
rmse_results <- calculateRMSE(mortality_data3)
print(rmse_results)



