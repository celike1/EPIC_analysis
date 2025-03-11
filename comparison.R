library(epicUS)
library(dplyr)
library(tidyr)


settings <- get_default_settings()
settings$record_mode <- 2
# settings$agent_stack_size <- 0
settings$n_base_agents <- 1e6
# settings$event_stack_size <- 0

init_session(settings = settings)

input <- get_input()

# set time horizon
time_horizon <- 1
input$values$global_parameters$time_horizon <- time_horizon

## making incidence and longevity 0
input$values$agent$l_inc_betas <- t(as.matrix(c(intercept = 0, y = 0, y2 = 0)))

input$values$agent$ln_h_bgd_betas <- t(as.matrix(c(intercept = 0, y = 0, y2 = 0, age = 0, b_mi = 0, n_mi = 0, b_stroke = 0,
                                            n_stroke = 0, hf = 0)))


run(input = input$values)
events <- as.data.frame(Cget_all_events_matrix())
terminate_session()

## checking if the event has any 7
unique(events$event)
table(events$event)

# rounding down so we can create a group.
events <- events %>%
  mutate(age_and_local = floor(local_time + age_at_creation))


age_info<-events %>%
  mutate(age_and_local = floor(local_time + age_at_creation)) %>%
  group_by(age_and_local,female) %>%
  summarise(count = n())

events_filtered<- events %>%
  mutate(death= ifelse(event==13,1,0)) %>%
  group_by(id) %>%
  mutate(ever_death = sum(death)) %>%
  filter(event==14) %>%
  ungroup()


death_prob2<- events_filtered %>%
  group_by(age_and_local, female) %>%
  summarise(
    total_count = n(),
    death_count = sum(ever_death==1),
    death_probability = death_count / total_count
  )
# %>%
#   ungroup() %>%
#   select(age_and_local, female, death_probability) %>%
#   pivot_wider(names_from = female, values_from = death_probability, names_prefix = "sex_")

colnames(death_prob) <- c("Age", "Male", "Female")




