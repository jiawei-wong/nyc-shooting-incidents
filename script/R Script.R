### Preamble ###
# Purpose: Clean the NYPD Shooting Incidents data
# Author: Julia Wong
# Date: April 24 2022
# Contact: jiawei.wong@mail.utoronto.ca
# Pre-requisites: None

### Workspace Set-up ###
install.packages("knitr")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("kableExtra")
install.packages("bibtex")
install.packages("bookdown")

library(knitr)
library(tidyverse)
library(dplyr)
library(kableExtra)
library(ggplot2)
library(bibtex)
library(bookdown)

### Saving the dataset ###
write_csv(x = NYPD_Shooting_Incident_Data_Historic_,
          file = "raw_data.csv")

raw_data <- read_csv("raw_data.csv",
                     show_col_types = FALSE)

### Cleaning the dataset ###
cleaned_data <- raw_data |>
  select(OCCUR_DATE, BORO, STATISTICAL_MURDER_FLAG, PERP_AGE_GROUP, PERP_SEX, PERP_RACE,
         VIC_AGE_GROUP, VIC_SEX, VIC_RACE) #removed irrelevant columns
  
#created a new column with the extracted years from "OCCUR_DATE"
format(as.Date(cleaned_data$OCCUR_DATE, format="%d/%m/%Y"),"%Y")
cleaned_data$Year <- format(cleaned_data$OCCUR_DATE, format="%Y")
cleaned_data$Month <- format(cleaned_data$OCCUR_DATE, format="%B")

### Summarizing the total shootings each year from 2006 - 2020 ###
total_shootings <- cleaned_data |>
  select(Year) |>
  arrange(Year) |>
  group_by(Year) |>
  count()

total_shootings_percent <- total_shootings |>
  data.frame("Year" = c("2006", "2007", "2008", "2009", "2010",
                                      "2011", "2012", "2013", "2014", "2015",
                                      "2016", "2017", "2018", "2019", "2020"),
             "Total_Number" = c("2055", "1887", "1959", "1828", "1912", "1939", 
                                "1717", "1339", "1464", "1434", "1208",
                                "970", "958", "967", "1948"),
             "Increase/Decrease" = c("NA", "-8.18", "3.82", "-6.69", "4.60",
                                                        "1.41", "-11.45", "-22.05", "9.34", "-2.05",
                                                        "-15.76", "-19.70", "-1.24", "0.94", "101.45")) |>
  select(-c(1:2))
                                                    
#Graphing
total_shootings |>
  ggplot(aes(Year, n) +
  geom_bar(position="dodge", stat="identity") +
  labs(x = "Year", y = "Number of shootings",
      title = "Shootings in New York City Between 2006 - 2020")

#Table
knitr::kable(total_shootings_percent, caption = "The Number of Shooting Incidents and Percentage Growth/Decline (2006 - 2020)",
             col.names = c("Year", "Total Number of Incidents", "Increase/Decrease from Previous Year (%)"),
             booktabs = TRUE) |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) |>
  kable_classic() |>
  kable_styling(latex_options = "HOLD_position")

### Shootings in 2020 by month ###
latest_shootings <- cleaned_data |>
  filter(Year == "2020") |>
  select(Month) |>
  group_by(Month) |>
  count() |>
  mutate(Month = factor(Month, levels = month.name)) |>
  arrange(Month)

#Graphing
latest_shootings |>
  ggplot(aes(Month, n, group = 1)) +
  geom_line() +
  geom_point() +
  labs(x = "Month", y = "Number of Shootings", 
       title = "Number of Shootings in 2020 by Month")

### Shootings in 2020 by race and sex ###
latest_shootings_race_sex <- cleaned_data |>
  select(VIC_RACE, VIC_SEX, Year) |>
  filter(Year == "2020") |>
  mutate(VIC_SEX = recode(VIC_SEX, F = 'Female', M = 'Male'),
         VIC_RACE = recode(VIC_RACE, "ASIAN / PACIFIC ISLANDER" = 'Asian / Pacific Islander',
                           "BLACK" = 'Black',
                           "BLACK HISPANIC" = 'Black Hispanic',
                           "WHITE" = 'White',
                           "WHITE HISPANIC" = 'White Hispanic'))

victim_gender <- latest_shootings_race_sex |>
  group_by(VIC_SEX) |>
  count()
colnames(victim_gender)[1] <- "Gender" #renaming columns
colnames(victim_gender)[2] <- "Total"

victim_race <- latest_shootings_race_sex |>
  group_by(VIC_RACE) |>
  count()
colnames(victim_race)[1] <- "Race"
colnames(victim_race)[2] <- "Total"

#Table
knitr::kable(list(victim_gender, victim_race), 
             caption = "The Gender and Race of Victims in 2020",
             booktabs = TRUE) |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) |>
  kable_classic() |>
  kable_styling(latex_options = "HOLD_position")

### Borough and its shootings ###
borough_shootings <- cleaned_data |>
  select(BORO) |>
  group_by(BORO) |>
  count() |>
  mutate(BORO = recode(BORO, "BRONX" = 'TheBronx', #renaming values of variables
                       "BROOKLYN" = 'Brooklyn',
                       "MANHATTAN" = 'Manhattan',
                       "QUEENS" = 'Queens',
                       "STATEN ISLAND" = 'Staten Island'))

#Graphing
borough_shootings |>
  ggplot(aes(BORO, n)) +
  geom_bar(stat="identity", fill = "grey") +
  geom_text(aes(label=n), size = 4, position = position_stack(vjust = 0.5)) +
  labs(x = "Borough", y = "Number of shootings",
       title = "Shootings in New York City by Boroughs Between 2006 - 2020") +
  theme_light()

### Fatality ###
fatality <- cleaned_data |>
  select(STATISTICAL_MURDER_FLAG, Year) |>
  group_by(STATISTICAL_MURDER_FLAG, Year) |>
  count() |>
  pivot_wider(names_from = STATISTICAL_MURDER_FLAG,
              values_from = n) |>
  mutate(Total = c(""),
         "Non-fatal (%)" = c(""),
         "Fatal (%)" = c(""))

colnames(fatality)[2] <- "Non-fatal"
colnames(fatality)[3] <- "Fatal"

fatality$Total=rowSums(cbind(fatality$`Non-fatal`, fatality$Fatal))

fatality$`Non-fatal (%)`= (fatality$`Non-fatal`/fatality$Total)*100
fatality$`Fatal (%)`= (fatality$`Fatal`/fatality$Total)*100

fatality_new <- fatality |>
  select(1, 5, 6)

#Table
knitr::kable(fatality_new, caption = "Fatality of Incidents (2006 - 2020)",
             digits = 2,
            booktabs = TRUE) |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) |>
  kable_classic() |>
  kable_styling(latex_options = "HOLD_position")
  
### Racial hate (2019 - 2020)###
racial_hate <- cleaned_data |>
  select(VIC_RACE, Year) |>
  filter(Year == "2020" | Year == "2019") |>
  subset(VIC_RACE!="UNKNOWN") |>
  mutate(VIC_RACE = recode(VIC_RACE,
                           "ASIAN / PACIFIC ISLANDER" = 'Asian / Pacific Islander',
                           "BLACK" = 'Black',
                           "BLACK HISPANIC" = 'Black Hispanic',
                           "WHITE" = 'White',
                           "WHITE HISPANIC" = 'White Hispanic')) |>
  arrange(desc(Year)) |>
  group_by(VIC_RACE, Year) |>
  count()

#Graphing
racial_hate |>
  ggplot(aes(Year, n)) +
  geom_bar(stat = "Identity", fill = "grey") +
  geom_text(aes(label=n), size = 4, position = position_stack(vjust = 0.8)) +
  facet_wrap(~ VIC_RACE) + 
  labs(x = "Year", y = "Number of Shootings", title = "Victims of Shootings by Race (2019 - 2020)")

### Fatality in each borough ###
fatality_borough <- cleaned_data |>
  mutate_if(is.logical, as.character) |>
  select(Year, STATISTICAL_MURDER_FLAG, BORO) |>
  filter(Year == "2020") |>
  mutate(BORO = recode(BORO, "BRONX" = 'The Bronx',
                       "BROOKLYN" = 'Brooklyn',
                       "MANHATTAN" = 'Manhattan',
                       "QUEENS" = 'Queens',
                       "STATEN ISLAND" = 'Staten Island'),
         STATISTICAL_MURDER_FLAG = recode(STATISTICAL_MURDER_FLAG, "FALSE" = 'Non-fatal',
                                          "TRUE" = 'Fatal')) |>
  group_by(STATISTICAL_MURDER_FLAG, BORO) |>
  count()

#Graph
fatality_borough |>
  ggplot(aes(BORO, n, fill = STATISTICAL_MURDER_FLAG), las = 2) +
  geom_bar(stat = "Identity") +
  geom_text(aes(label=n), size = 3, position = position_stack(vjust = 0.5)) +
  theme_light() +
  labs(x = "Borough", y = "Number of Shootings", fill = "Incident Type", title = "Fatality of Shooting Incident by Borough") +
  theme_light()

### Shootings of Asians in 2020 by month ###
asian_shootings <- cleaned_data |>
  filter(Year == "2020" & VIC_RACE == "ASIAN / PACIFIC ISLANDER") |>
  select(Month) |>
  group_by(Month) |>
  count() |>
  mutate(Month = factor(Month, levels = month.name)) |>
  arrange(Month)

#Graphing
asian_shootings |>
  ggplot(aes(Month, n, group = 1)) +
  geom_line() +
  geom_point() +
  labs(x = "Month", y = "Number of Shootings",
       title = "Number of Asian and Pacific Islander Shooting Victims by Month (2020)")
