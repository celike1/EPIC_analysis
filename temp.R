library(epicUS)
library(dplyr)
library(tidyr)

settings <- get_default_settings()
settings$record_mode <- 2
# settings$agent_stack_size <- 0
settings$n_base_agents <- 1e6
# settings$event_stack_size <- 0

init_session(settings = settings)

input <- Cget_inputs()

# set time horizon
time_horizon <- 1
input$values$global_parameters$time_horizon <- time_horizon

run(input = input$values)
events <- as.data.frame(Cget_all_events_matrix())


## checking if the event has any 7
unique(events$event)
table(events$event)

# rounding down so we can create a group.
events <- events %>%
  mutate(age_and_local = floor(local_time + age_at_creation))

# probability of death by age group and sex
death_prob <- events %>%
  group_by(age_and_local, female) %>%
  summarise(
    #total number of individuals in each group.
    total_count = n(),
    #number of deaths (event == 14) in each group.
    death_count = sum(event == 13),
    death_probability = death_count / total_count
  ) %>%
  ungroup() %>%
  select(age_and_local, female, death_probability) %>%
  pivot_wider(names_from = female, values_from = death_probability, names_prefix = "sex_")

colnames(death_prob) <- c("Age", "Male", "Female")

death_prob


USlifetables <-read.csv("USLifeTables.csv", header = FALSE)
USlifetables_num <- as.matrix(USlifetables)
USlifetables_num <- apply(USlifetables_num, 2, as.numeric)


USlifetables_df <- data.frame(
  Age = 1:nrow(USlifetables_num),  # Start age from 1
  Male = USlifetables_num[, 1],
  Female = USlifetables_num[, 2]
)


common_ages <- intersect(death_prob$Age, USlifetables_df$Age)

# filter both so only include the rows with matching Age
death_prob_filtered <- death_prob[death_prob$Age %in% common_ages, ]
USlifetables_filtered <- USlifetables_df[USlifetables_df$Age %in% common_ages, ]


result_df <- data.frame(
  Age = death_prob_filtered$Age,
  Male = USlifetables_filtered$Male - death_prob_filtered$Male,
  Female = USlifetables_filtered$Female - death_prob_filtered$Female
)


