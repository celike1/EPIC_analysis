library(epicUS)
library(dplyr)
library(tidyr)
library(ggplot2)


packageVersion("epicUS")


settings <- get_default_settings()
settings$record_mode <- 2
settings$n_base_agents <- 1e6

init_session(settings = settings)

input<-get_input()

time_horizon <- 1
input$values$global_parameters$time_horizon <- time_horizon

# Model has too many former smokers so we increased the intercept.
input$values$smoking$logit_p_never_smoker_con_not_current_0_betas <-
  t(as.matrix(c(intercept = 4.85, sex = 0, age = -0.06, age2 = 0, sex_age = 0,
                                                                            sex_age2 = 0, year = -0.02)))


# we are keeping this one the same
input$values$smoking$mortality_factor_current <- t(as.matrix(c(age40to49 = 1,
                                                        age50to59 = 1,
                                                        age60to69 = 1.94,
                                                        age70to79 = 1.86,
                                                        age80p = 1.66 )))

# These are also kept the same
input$values$smoking$mortality_factor_former<- t(as.matrix(c(age40to49 = 1,
                                                      age50to59 = 1,
                                                      age60to69 = 1.54,
                                                      age70to79 = 1.36,
                                                      age80p = 1.27 )))



run(input = input$values)
events <- as.data.frame(Cget_all_events_matrix())
terminate_session()


table(events$event)


# we will group by age so we convert ages into whole numbers.
events <- events %>%
  mutate(age_and_local = floor(local_time + age_at_creation))

# Filter events to identify individuals who have experienced event 14,
# while also creating a flag for whether they ever experienced event 13 (death)
events_filtered<- events %>%
  mutate(death= ifelse(event==13,1,0)) %>%
  group_by(id) %>%
  mutate(ever_death = sum(death)) %>%
  filter(event==14) %>%
  ungroup()


# calculationg probability of death
death_prob<- events_filtered %>%
  group_by(age_and_local, female) %>%
  summarise(
    total_count = n(),
    death_count = sum(ever_death==1),
    death_probability = death_count / total_count
  )

death_prob_clean <- death_prob %>%
  ungroup() %>%
  select(age_and_local, female, death_probability) %>%
  pivot_wider(names_from = female, values_from = death_probability, names_prefix = "sex_")

colnames(death_prob_clean) <- c("Age", "Male", "Female")



USlifetables_num <- input$values$agent$p_bgd_by_sex


USlifetables_df <- data.frame(
  Age = 1:nrow(USlifetables_num),  # Start age from 1
  Male = USlifetables_num[, 1],
  Female = USlifetables_num[, 2]
)

common_ages <- intersect(death_prob_clean$Age, USlifetables_df$Age)

# filter both so only include the rows with matching Age
death_prob_filtered <- death_prob_clean[death_prob_clean$Age %in% common_ages, ]
USlifetables_filtered <- USlifetables_df[USlifetables_df$Age %in% common_ages, ]

combined_data_long <- bind_rows(
  death_prob_filtered %>% mutate(Source = "epicUS"),
  USlifetables_filtered %>% mutate(Source = "US Life Tables")
) %>%
  pivot_longer(cols = c("Male", "Female"), names_to = "Sex", values_to = "Death_Probability") %>%
  filter(Age > 40)

ggplot(combined_data_long, aes(x = Age, y = Death_Probability, fill = Source)) +
  geom_col(position = "dodge", width = 1) +
  facet_wrap(~Sex) +
  labs(
    title = "Comparison of epicUS Death Probability vs. US Life Tables (time_horizon =1)",
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



####checking smokers
output<-Cget_output_ex()
smoking_num<-output$n_smoking_status_by_ctime
smoking_ratio <- smoking_num[1, 3] / sum(smoking_num[1, ])
smoking_ratio



