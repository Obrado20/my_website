---
categories:
- ""
- ""
date: "2017-10-31T22:42:51-05:00"
description: In this project my group and I analysed Donald Trump's approval ratings over time.
draft: false
image: Trump.jpg
keywords: ""
slug: six
title: Trump approval rates
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




# Trump's Approval Margins



```{r, cache=TRUE, include=FALSE}
# Import approval polls data

approval_polllist <- read_csv('https://projects.fivethirtyeight.com/trump-approval-data/approval_polllist.csv') 

glimpse(approval_polllist)

# Use `lubridate` to fix dates, as they are given as characters.
approval_polllist_fixdates <- approval_polllist %>% 
  mutate(timestamp = parse_date_time(timestamp, orders = "HMSdmy"),modeldate = mdy(modeldate), startdate = mdy(startdate), enddate = mdy(enddate), createddate = mdy(createddate))

```

## Create a plot

No we are going to calculate the average net approval rate (approve- disapprove) for each week since Donald Trump got into office. 

```{r trump_margins, out.width="100%"}

graph_data <- approval_polllist_fixdates %>%  
  filter(subgroup=="Voters") %>% #To reflect only the people that will have an impact during the election.
  mutate(week = week(enddate), year = year(enddate)) %>% #setting the date ass the end of the poll
  mutate(average_net = adjusted_approve - adjusted_disapprove, year = year(enddate)) %>% #calculating the average net approval using the adjusted data for more precision.
  group_by(week, year) %>% 
  summarise(mean_approval = mean(average_net),
            lower = mean(average_net) - qt(0.975, (n() - 1))*sd(average_net)/sqrt(n()),
            upper = mean(average_net) + qt(0.975, (n() - 1))*sd(average_net)/sqrt(n())) #confidence interval using 95% 
  
#constructing the plot
graph <- ggplot(graph_data, 
                aes(x=week,
                    y=mean_approval, 
                    color = as.factor(year))) + 
  geom_line(alpha=0.6) + 
  geom_point(alpha=0.6) + #making the colors lighter by decreasing the opacity
  geom_hline(yintercept=0, color = "orange") + #setting up the line at 0
  geom_ribbon(aes(ymin=lower, ymax=upper), linetype=1, alpha=0.07) +
  labs(title = "Estimating Net Approval (approve-disapprove) for Donald Trump", #Replicating the exact same texts.
       subtitle = "Weekly average of all polls", 
       y = "Average Net Approval (%)", 
       x = "Week of the year") +
  facet_wrap(~year) +
  theme_bw() +
  theme(legend.position="none") +#removing the legend
  scale_color_manual(values = c("brown1","darkolivegreen3","darkturquoise","darkorchid1")) + 
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = '.'), 
              breaks = c(7.5, 5, 2.5,0.0,-2.5, -5.0, -7.5, -10.0, -12.5, -15.0, -17.5, -20.0)) +# Setting up the intervals on the axis
  scale_x_continuous(breaks = c(0, 13, 26, 39, 52))

graph



```

## Compare Confidence Intervals

The confidence interval from `week 15` (6-12 April 2020) to `week 34` (17-23 August 2020) widens. Showing that Trump's supporter and disapprovers further diverged as the evolving economic situation, social issues (BLM), and pandemic contribute to uncertainty. Such delicate subjects often force the president to take one side or another. As these sides are recently quite decisive, his decisions have a more profound impact on the approval ratings from the people. 




