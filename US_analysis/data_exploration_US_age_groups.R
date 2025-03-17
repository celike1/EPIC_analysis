library(tidyr)
library(ggplot2)
library(dplyr)

# US population 2022 onwards,
US_pop <- read.csv("US_analysis/data/US_census_pop_by_age.csv", header = TRUE)

colnames(US_pop) <- gsub('X', '', colnames(US_pop))
US_pop[, 1] <- gsub("^\\.", "", US_pop[, 1])


US_pop65<- US_pop[, 1:11]

US_pop65[,-1] <- lapply(US_pop65[,-1], function(x) as.numeric(gsub(",", "", x)))

US_pop65 <- US_pop65 %>%
  pivot_longer(cols = -Age_Range, names_to = "Year", values_to = "Population") %>%
  mutate(Year = as.numeric(Year))  # Convert Year to numeric


US_pop65 <- US_pop65 %>%
  mutate(Age_Range = factor(Age_Range,
                            levels = c("40 to 44 years", "45 to 49 years", "50 to 54 years",
                                       "55 to 59 years", "60 to 64 years", "65 to 69 years",
                                       "70 to 74 years", "75 to 79 years", "80 to 84 years",
                                       "85 to 89 years", "90 to 94 years", "95 to 99 years",
                                       "100 years and over"), ordered = TRUE),
         Age_Range = recode(Age_Range,
                            "40 to 44 years" = "40-44",
                            "45 to 49 years" = "45-49",
                            "50 to 54 years" = "50-54",
                            "55 to 59 years" = "55-59",
                            "60 to 64 years" = "60-64",
                            "65 to 69 years" = "65-69",
                            "70 to 74 years" = "70-74",
                            "75 to 79 years" = "75-79",
                            "80 to 84 years" = "80-84",
                            "85 to 89 years" = "85-89",
                            "90 to 94 years" = "90-94",
                            "95 to 99 years" = "95-99",
                            "100 years and over" = "100+"))




pop2015<- read.csv("US_analysis/data/2015pop.csv", header = TRUE)

pop2015 <- pop2015 %>%
  mutate(Age_Range = cut(AGE,
                          breaks = c(seq(40, 100, by = 5), Inf),
                          include.lowest = TRUE,
                          right = FALSE,
                          labels = c(paste(seq(40, 95, by = 5), seq(44, 99, by = 5), sep = "-"), "100+")),
         Year = "2015")



pop2015 <- pop2015 %>%
  mutate(Age_Range = factor(Age_Range,
                            levels = levels(US_pop65$Age_Range),
                            ordered = TRUE),
         Year = as.numeric(Year))

pop2015 <- pop2015 %>%
  group_by(Age_Range,Year) %>%
  summarise(Population = sum(POPESTIMATE2015, na.rm = TRUE) / 1000 )

US_pop65_final <- bind_rows(US_pop65, pop2015) %>%
  arrange(Year)


ggplot(US_pop65_final, aes(x = Age_Range, y = Population, fill = Age_Range)) +
  geom_bar(stat = "identity", width = 0.9) +
  theme_minimal() +
  labs(title = "Population Distribution by Age Range for Each Year",
       x = "Age Range",
       y = "Population (in thousands)") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 8)) +
  facet_wrap(~Year, nrow = 3, ncol = 4)

ggplot(US_pop65_final, aes(x = Year, y = Population, fill = factor(Year))) +
  geom_bar(stat = "identity", width = 0.9, position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(title = "Population Distribution Over Time by Age Range",
       x = "Year",
       y = "Population (in thousands)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  facet_wrap(~Age_Range, scales = "free_y", ncol = 3) +
  scale_fill_viridis_d()

