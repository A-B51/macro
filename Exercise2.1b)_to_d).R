####################################################################### 
# Macroeconomics III: Assignment 2, Exercise 2.1 b)
# 
# Imports data from World Bank
# Input: 
# Output: 
# 
# A. Bittencourt, St. Gallen, 2020
#######################################################################


#SETUP-------------

library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(kableExtra)
library(readr)
library(tidyverse)
library(readxl)
library(kableExtra)

# EXERCISE 2b)----------------------------------------------

#Import Data------------------

d2b <- read_excel("Table2.1b).xlsx")
d2b

#(i) 1990 - 2004:savings and investment rate------

# CLEANING

# selecting data from first to 17th column
d2b_90to04 <- d2b %>% select(1:17)
view(d2b_90to04)
d2b_90to04[d2b_90to04 == 0] <- NA
view(d2b_90to04)
#d2b_90to04_grouped <- group_by(d2b_90to04, `Country Name`, `Series Name`)
#d2b_90to04_grouped <- as.factor(d2b_90to04_grouped$`Series Name`)
d2b_90to04_sav <- filter(d2b_90to04, `Series Name` == "Gross savings (% of GDP)")
d2b_90to04_sav_with_names <- select(d2b_90to04_sav,`Country Name`, `1990`, `1991`, `1992`, `1993`, `1994`, `1995`, `1996`, `1997`, `1998`, `1999`, `2000`, `2001`, `2002`, `2003`, `2004`)
d2b_90to04_inv <- filter(d2b_90to04, `Series Name` == "Gross capital formation (% of GDP)")
d2b_90to04_inv_with_names <- select(d2b_90to04_inv,`Country Name`, `1990`, `1991`, `1992`, `1993`, `1994`, `1995`, `1996`, `1997`, `1998`, `1999`, `2000`, `2001`, `2002`, `2003`, `2004`)
#d2b_90to04_sav_no_names <- select(d2b_90to04_sav, `1990`, `1991`, `1992`, `1993`, `1994`, `1995`, `1996`, `1997`, `1998`, `1999`, `2000`, `2001`, `2002`, `2003`, `2004`)
d2b_90to04_sav_with_names
d2b_90to04_inv_with_names
#d2b_90to04_sav_no_names
#d2b_90to04_sav_with_names %>% mutate(years = 1990:2004, by = `Country Name`)

# making the data longer for the analisys
d2b_90to04_sav_with_names_4Analisys <- d2b_90to04_sav_with_names %>% pivot_longer(c(`1990`, `1991`, `1992`, `1993`, `1994`, `1995`, `1996`, `1997`, `1998`, `1999`, `2000`, `2001`, `2002`, `2003`, `2004`), names_to = "year", values_to = "savings rate")
view(d2b_90to04_sav_with_names_4Analisys)
d2b_90to04_inv_with_names_4Analisys <- d2b_90to04_inv_with_names %>% pivot_longer(c(`1990`, `1991`, `1992`, `1993`, `1994`, `1995`, `1996`, `1997`, `1998`, `1999`, `2000`, `2001`, `2002`, `2003`, `2004`), names_to = "year", values_to = "investment rate")
view(d2b_90to04_inv_with_names_4Analisys)

# grouping by pop of interest
d2b_90to04_sav_grouped <- group_by(d2b_90to04_sav_with_names_4Analisys, `Country Name`)
d2b_90to04_sav_grouped
d2b_90to04_inv_grouped <- group_by(d2b_90to04_inv_with_names_4Analisys, `Country Name`)
d2b_90to04_inv_grouped


# STATS

# getting the means per country
means_saving_rate_90to04 <- summarise(d2b_90to04_sav_grouped,
                                      mean_from_94_to_04_s = mean(`savings rate`, na.rm = TRUE))
means_saving_rate_90to04

means_investment_rate_90to04 <- summarise(d2b_90to04_inv_grouped,
                                      mean_from_94_to_04_i = mean(`investment rate`, na.rm = TRUE))
means_investment_rate_90to04

# Making sure by random controls
Germany_sr_90to04 <- c(24.40,	23.72,	23.51,	22.51,	22.75,	23.04,	22.52,	22.79,	23.23,	22.56,	22.67,	22.54,	22.63,	21.79,	24.32)
mean(Germany_sr_90to04) # OK
Greece_inv_90to04 <- c(27.56,	28.41,	25.41,	24.18,	22.79,	22.47,	23.35,	22.44,	25.18,	24.15,	25.83,	25.69,	24.75,	27.37,	25.31)
mean(Greece_inv_90to04) # OK
Germany_invr_90to04 <- c(24.83,	25.77,	25.26,	23.94,	24.33,	24.33,	23.26,	23.38,	24.02,	24.00,	24.49,	22.96,	20.78,	20.44,	19.84)
mean(Germany_invr_90to04)# OK


#(ii) 2005 - now: savings and investment rate------------

# CLEANING

# separating data to merge afterwards
data_to_merge <- select(d2b, `Country Name`,`Series Name`)
data_to_merge
id <- c(1:32)
data_to_merge_with_id <- mutate(data_to_merge, id)
data_to_merge_with_id

# selecting data from 18th to 31st column
d2b_05to18 <- d2b %>% select(18:31)
view(d2b_05to18)
d2b_05to18[d2b_05to18 == 0] <- NA
d2b_05to18_with_id <- mutate(d2b_05to18, id)
d2b_05to18_with_id

# merging datasets and cleaning them
d2b_05to18_merged <- merge(data_to_merge_with_id, d2b_05to18_with_id, by = "id")
view(d2b_05to18_merged)
d2b_05to18_merged <- select(d2b_05to18_merged, -c(id))
view(d2b_05to18_merged)

#d2b_90to04_grouped <- group_by(d2b_90to04, `Country Name`, `Series Name`)
#d2b_90to04_grouped <- as.factor(d2b_90to04_grouped$`Series Name`)
d2b_05to18_sav <- filter(d2b_05to18_merged, `Series Name` == "Gross savings (% of GDP)")
d2b_05to18_sav <- select(d2b_05to18_sav, -c(`Series Name`))
d2b_05to18_inv <- filter(d2b_05to18_merged, `Series Name` == "Gross capital formation (% of GDP)")
d2b_05to18_inv <- select(d2b_05to18_inv,-c(`Series Name`))
#d2b_90to04_sav_no_names <- select(d2b_90to04_sav, `1990`, `1991`, `1992`, `1993`, `1994`, `1995`, `1996`, `1997`, `1998`, `1999`, `2000`, `2001`, `2002`, `2003`, `2004`)
d2b_05to18_sav
d2b_05to18_inv
#d2b_90to04_sav_no_names
#d2b_90to04_sav_with_names %>% mutate(years = 1990:2004, by = `Country Name`)

# making the data longer for the analisys
d2b_05to18_sav <- d2b_05to18_sav %>% pivot_longer(c(`2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`), names_to = "year", values_to = "savings rate")
view(d2b_05to18_sav)
d2b_05to18_inv <- d2b_05to18_inv %>% pivot_longer(c(`2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`), names_to = "year", values_to = "investment rate")
view(d2b_05to18_inv)

# grouping by pop of interest
d2b_05to18_sav_grouped <- group_by(d2b_05to18_sav, `Country Name`)
d2b_05to18_sav_grouped
d2b_05to18_inv_grouped <- group_by(d2b_05to18_inv, `Country Name`)
d2b_05to18_inv_grouped

# STATS

# getting the means per country
means_saving_rate_05to18 <- summarise(d2b_05to18_sav_grouped,
                                      mean_from_05_to_18_s = mean(`savings rate`, na.rm = TRUE))
means_saving_rate_05to18

means_investment_rate_05to18 <- summarise(d2b_05to18_inv_grouped,
                                          mean_from_05_to_18_i = mean(`investment rate`, na.rm = TRUE))
means_investment_rate_05to18

# Making sure by random controls
Germany_sr_05to18 <- c(24.13,	26.34,	28.27,	27.14,	24.39,	25.77,	27.82,	26.80,	26.58,	27.59,	28.56,	28.74,	28.88,	29.34)
mean(Germany_sr_05to18) # OK
Italy_inv_05to18 <- c(21.17,	21.96,	22.25,	21.78,	19.51,	20.58,	20.47,	17.79,	16.89,	16.96,	17.11,	17.56,	17.91,	18.16)
mean(Italy_inv_05to18) # OK
Netherlands_sr_05to18 <- c(25.70,	28.23,	28.12,	26.43,	26.05,	27.20,	28.66,	28.82,	28.06,	27.10,	28.77,	28.54,	31.42,	31.85)
mean(Netherlands_sr_05to18)# OK

#Preparing World Bank Data (max. Range: 7 years------------
d2b
# we already have definied a data to merge
view(d2b)
data_to_merge
data_to_merge_with_id

# selecting first 7 years:
data1 <- d2b %>% select(3:10)
data1 <- mutate(data1, id)
# merging
data1 <- merge(data_to_merge_with_id, data1, by = "id")
# cleaning id
data1 <- select(data1, -c(id))

# second:
data2 <- d2b %>% select(11:18)
data2 <- mutate(data2, id)
# merging
data2 <- merge(data_to_merge_with_id, data2, by = "id")
# cleaning id
data2 <- select(data2, -c(id))

# third:
data3 <- d2b %>% select(18:24)
data3 <- mutate(data3, id)
# merging
data3 <- merge(data_to_merge_with_id, data3, by = "id")
# cleaning id
data3 <- select(data3, -c(id))

# fourth:
data4 <- d2b %>% select(25:31)
data4 <- mutate(data4, id)
# merging
data4 <- merge(data_to_merge_with_id, data4, by = "id")
# cleaning id
data4 <- select(data4, -c(id))

#Latex: Importing Tables & Tibbles to LaTex-------------

Table_Latex_World_Bank1 <- kable(data1,
                                row.names = FALSE,
                                digits = 2,
                                caption = "Name",
                                format = 'latex', booktabs = TRUE
)
Table_Latex_World_Bank2 <- kable(data2,
                                 row.names = FALSE,
                                 digits = 2,
                                 caption = "Name",
                                 format = 'latex', booktabs = TRUE
)
Table_Latex_World_Bank3 <- kable(data3,
                                 row.names = FALSE,
                                 digits = 2,
                                 caption = "Name",
                                 format = 'latex', booktabs = TRUE
)
Table_Latex_World_Bank4 <- kable(data4,
                                 row.names = FALSE,
                                 digits = 2,
                                 caption = "Name",
                                 format = 'latex', booktabs = TRUE
)
Table_Latex_World_Bank1

#Latex: Means---------

Table_Latex_means_saving_rate_05to18 <- kable(means_saving_rate_05to18,
                    row.names = FALSE,
                    digits = 2,
                    caption = "Name",
                    format = 'latex', booktabs = TRUE
)
Table_Latex_means_saving_rate_05to18

Table_Latex_means_saving_rate_90to04 <- kable(means_saving_rate_90to04,
                                              row.names = FALSE,
                                              digits = 2,
                                              caption = "Name",
                                              format = 'latex', booktabs = TRUE
)
Table_Latex_means_saving_rate_90to04

Table_Latex_means_investment_rate_05to18 <- kable(means_investment_rate_05to18,
                                                 row.names = FALSE,
                                                 digits = 2,
                                                 caption = "Name",
                                                 format = 'latex', booktabs = TRUE
)
Table_Latex_means_investment_rate_05to18

Table_Latex_means_investment_rate_90to04 <- kable(means_investment_rate_90to04,
                                                  row.names = FALSE,
                                                  digits = 2,
                                                  caption = "Name",
                                                  format = 'latex', booktabs = TRUE
)
Table_Latex_means_investment_rate_90to04


#EXERCISE 2.1c)----------------

# Regression 1990 to 2004-----------

# Relevant Data for Regression
means_saving_rate_90to04
means_investment_rate_90to04

# merge data for regression
data_ols_2c <- merge(means_saving_rate_90to04, means_investment_rate_90to04, by = "Country Name")

# Regression - for later stats analisys
ols_2c <- lm(data_ols_2c$mean_from_94_to_04_i ~ data_ols_2c$mean_from_94_to_04_s, data_ols_2c)
summary(ols_2c)

# Plotting
graph_2c <- ggplot(data_ols_2c, aes(y = data_ols_2c$mean_from_94_to_04_i, x = data_ols_2c$mean_from_94_to_04_s))+
  geom_point(color = "black", size = 3, shape = "diamond")+
  geom_smooth(method='lm', formula= y~x, color = "red", linetype = "11", size = 0.01)+
  labs(x = "Average Savings Rate",
       y = "Average Investment Rate",
       title = "Diagramm Exercise 2c)")
graph_2c
ggsave("graph_2c.pdf")


#Exercise 2.1d)----------------

# Relevant data for Regression
means_saving_rate_05to18
means_investment_rate_05to18

# merge data for graph
data_ols_2d <- merge(means_saving_rate_05to18, means_investment_rate_05to18, by = "Country Name")

# Regression
ols_2d <- lm(data_ols_2c$mean_from_94_to_04_i ~ data_ols_2d$mean_from_05_to_18_s, data_ols_2d)
summary(ols_2d)

# Plotting
graph_2d_both <- ggplot(data_ols_2d, aes(y = data_ols_2d$mean_from_05_to_18_i, x = data_ols_2d$mean_from_05_to_18_s))+
  geom_point(color = "red", size = 3, shape = "diamond")+
  geom_smooth(method='lm', formula= y~x, color = "red", linetype = "11", size = 0.1)+
  labs(x = "Average Savings Rate",
       y = "Average Investment Rate",
       title = "Data 1990-2004 (blue), 2005-2018 (red)")+
  geom_point(data = data_ols_2c, aes(y = data_ols_2c$mean_from_94_to_04_i, x = data_ols_2c$mean_from_94_to_04_s),color = "blue", size = 3, shape = "diamond")+
  geom_smooth(data = data_ols_2c, aes(y = data_ols_2c$mean_from_94_to_04_i, x = data_ols_2c$mean_from_94_to_04_s), method = "lm", formula = y~x,  color = "blue", linetype = "11", size = 0.01)
graph_2d_both
ggsave("graph_2d_both.pdf")
