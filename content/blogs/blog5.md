---
categories:
- ""
- ""
date: "2017-10-31T22:42:51-05:00"
# description: The link on the side leads you to a short description of myself.
draft: false
image: covid.jpg
keywords: ""
slug: five
title: Covid-19
---

---
title: "Session 4: Homework 2"
author: "Group 26"
date: "`r Sys.Date()`"
output:
  html_document:
    theme: flatly
    highlight: zenburn
    number_sections: yes
    toc: yes
    toc_float: yes
    code_folding: show
---


```{r, setup, include=FALSE}
knitr::opts_chunk$set(
  message = FALSE, 
  warning = FALSE, 
  tidy=FALSE,     # display code as typed
  size="small")   # slightly smaller font for code
options(digits = 3)

# default figure size
knitr::opts_chunk$set(
  fig.width=6.75, 
  fig.height=6.75,
  fig.align = "center"
)
```


```{r load-libraries, include=FALSE}
library(tidyverse)  # Load ggplot2, dplyr, and all the other tidyverse packages
library(mosaic)
library(ggthemes)
library(lubridate)
library(here)
library(skimr)
library(janitor)
library(httr)
library(readxl)
library(vroom)
library(infer)
```


# Challenge 1: CDC COVID-19 Public Use Data



```{r, cache=TRUE, include= FALSE}
# URL link to CDC to download data
url <- "https://data.cdc.gov/api/views/vbim-akqf/rows.csv?accessType=DOWNLOAD"

covid_data <- vroom(url)%>%
  clean_names()

glimpse(clean_names)
        
```

We are interested in learning more about the death rate among covid patients. Given the data we have, we would like to produce two graphs that show death % rate:

1. by age group, sex, and whether the patient had co-morbidities or not
1. by age group, sex, and whether the patient was admited to Intensive Care Unit (ICU) or not.


```{r covid_challenge, echo=FALSE, out.width="100%", include=FALSE}
knitr::include_graphics(here::here("images", "covid_death_rate_comorbidities.png"), error = FALSE)
knitr::include_graphics(here::here("images", "covid_death_rate_icu.png"), error = FALSE)
```

Before we start the analysis we have to get rid of data entries that don't give us any information.

```{r}

# Cleaning the data set by replacing "Unknown" and "Missing" values with "NA"

covid_data <- covid_data %>% 
  mutate(icu_yn = case_when(
    icu_yn == "Unknown" ~ NA_character_,
    icu_yn == "Missing" ~ NA_character_,
    TRUE ~ icu_yn
    ), death_yn = case_when(
    death_yn == "Unknown" ~ NA_character_,
    death_yn == "Missing" ~ NA_character_,
    TRUE ~ death_yn
    ), medcond_yn = case_when(
    medcond_yn == "Unknown" ~ NA_character_,
    medcond_yn == "Missing" ~ NA_character_,
    TRUE ~ medcond_yn
    ), sex = case_when(
    sex == "Unknown" ~ NA_character_,
    sex == "Missing" ~ NA_character_,
    sex == "Other" ~ NA_character_,
    TRUE ~ sex
    ), age_group = case_when(
    age_group == "Unknown" ~ NA_character_,
    age_group == "Missing" ~ NA_character_,
    TRUE ~ age_group))

```


```{r}

# we group the data by icu_yn to determine the effects of COVID across different groups of people

covid_data_grouped_by_medcond <- covid_data %>%
  group_by(age_group, sex, medcond_yn) %>% 
  filter(death_yn != is.na(death_yn) & sex != is.na(sex) & medcond_yn != is.na(medcond_yn) & age_group != is.na(age_group)) %>% 
  summarise(death_rate = count(death_yn=="Yes") / (count(death_yn=="Yes") + count(death_yn=="No"))) 

```

```{r}

# To make our data easier to understand we change the entries in medcond_yn

covid_data_grouped_by_medcond$medcond_yn <- factor(covid_data_grouped_by_medcond$medcond_yn, levels = c("Yes", "No"), 
                  labels = c("With comorbidities", "Without comorbidities"))

```

After arranging, we can plot our data to make better inferences

```{r}

graph_1<-ggplot(covid_data_grouped_by_medcond, aes(x=death_rate, y=age_group)) + 
  geom_col(aes(show.legend = FALSE), fill="cornflowerblue", alpha=0.7) + 
  facet_grid(medcond_yn ~ sex) +
  theme_bw() +
  ggtitle("Covid death % by age group, sex and presence of co-morbidities") +
  geom_text(aes(label = round(death_rate * 100,1), hjust = -0.1)) +
  theme(axis.title.x = element_blank()) + 
  theme(axis.title.y = element_blank()) +
  theme(legend.position = "none") + 
  labs(caption = "Source: CDC")
  
graph_1
```
It can be seen that there is a clear correlation between age and mortality rate. The same holds true for earlier diagnosis of co-morbidities and mortality rate. Another interesting inference is that there is a higher probability of men dying from COVID than of women.

```{r}

# we group the data by icu_yn to determine the effects of COVID across different groups of people

covid_data_grouped_by_icu <- covid_data %>%   
  group_by(age_group, sex, icu_yn) %>% 
  filter(death_yn != is.na(death_yn) & sex != is.na(sex) & icu_yn != is.na(icu_yn) & age_group != is.na(age_group)) %>% 
  summarise(death_rate = count(death_yn=="Yes") / (count(death_yn=="Yes") + count(death_yn=="No")))

```

```{r}

# To make our data easier to understand we change the entries in icu_yn

covid_data_grouped_by_icu$icu_yn <- factor(covid_data_grouped_by_icu$icu_yn, levels = c("Yes", "No"), 
                  labels = c("Admitted to ICU", "No ICU"))

```

In our second plot we look at correlation of covid deaths and admittance to the intensive care unit

```{r}

graph_2 <- ggplot(covid_data_grouped_by_icu, aes(x=death_rate, y=age_group)) + 
  geom_col(aes(show.legend = FALSE), fill="coral1", alpha = 0.7) + 
  facet_grid(icu_yn ~ sex) +
  #facet_wrap(~sex + icu_yn, scale="free") + 
  theme_bw() +
  ggtitle("Covid death % by age group, sex and whether patient was admitted to ICU") +
  geom_text(aes(label = round(death_rate * 100,1), hjust = -0.1)) +
  theme(axis.title.x = element_blank()) + theme(axis.title.y = element_blank()) +
  theme(legend.position = "none") + 
  labs(caption = "Source: CDC")


graph_2

```
Here it can be clearly seen that on average a much higher percentage of people who were admitted to the ICU died. Again we can also see that men and older people are more likely to die.


