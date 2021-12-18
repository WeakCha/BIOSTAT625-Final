## This file is the code for 4 people, we have denoted which part belongs to whom.

## Import packages
library(caret)
library(data.table)
library(doMC)
library(doParallel)
library(dplyr)
library(foreach)
library(ggplot2)
library(grid)
library(knitr)
library(magick)
library(parallel)
library(pROC)
library(randomForest)
library(readr)
library(tidyr)
library(usmap)

## use "data.table::fread" function to import data  
n.threads <- getDTthreads()
data.table::setDTthreads(n.threads)
load.data <- fread("US_counties_COVID19_health_weather_data.csv")

## For the purpose of data visualization, we only keep 48 + 1 states
## Exclude: Hawaii, Alaska, Virgin Islands, Puerto Rico and Northern Mariana Islands 
state.names <- as.vector(unique(load.data$state))
exclude.state.names <- c("Hawaii", "Alaska", "Virgin Islands", 
                         "Puerto Rico", "Northern Mariana Islands")
selected.state.names <- state.names[(state.names %in% exclude.state.names) == FALSE]
selected.state.data <- load.data[which(load.data$state %in% selected.state.names == TRUE), ]

## select potential features (preliminary)
data.initial <-
  selected.state.data[, c(
    "date",
    "county",
    "state",
    "total_population",
    "cases",
    "deaths",
    "percent_fair_or_poor_health",
    "average_number_of_physically_unhealthy_days",
    "average_number_of_mentally_unhealthy_days",
    "percent_low_birthweight",
    "percent_smokers",
    "percent_adults_with_obesity",
    "food_environment_index",
    "percent_physically_inactive",
    "percent_with_access_to_exercise_opportunities",
    "percent_excessive_drinking",
    "chlamydia_rate",
    "teen_birth_rate",
    "percent_uninsured",
    "primary_care_physicians_rate",
    "dentist_rate",
    "mental_health_provider_rate",
    "preventable_hospitalization_rate",
    "percent_vaccinated",
    "high_school_graduation_rate",
    "percent_unemployed_CHR",
    "percent_children_in_poverty",
    "percent_single_parent_households_CHR",
    "social_association_rate",
    "violent_crime_rate",
    "injury_death_rate",
    "presence_of_water_violation",
    "average_daily_pm2_5",
    "percent_severe_housing_problems",
    "severe_housing_cost_burden",
    "inadequate_facilities",
    "percent_long_commute_drives_alone",
    "life_expectancy",
    "child_mortality_rate",
    "infant_mortality_rate",
    "percent_frequent_physical_distress",
    "percent_frequent_mental_distress",
    "percent_adults_with_diabetes",
    "hiv_prevalence_rate",
    "percent_food_insecure",
    "percent_limited_access_to_healthy_foods",
    "drug_overdose_mortality_rate",
    "motor_vehicle_mortality_rate",
    "percent_insufficient_sleep",
    "median_household_income",
    "segregation_index",
    "homicide_rate",
    "firearm_fatalities_rate",
    "juvenile_arrest_rate",
    "percent_homeowners",
    "percent_black",
    "percent_asian",
    "percent_hispanic",
    "percent_female",
    "percent_rural",
    "percent_disabled",
    "percent_minorities",
    "percent_no_vehicle",
    "percent_limited_english_abilities"
  )]

rm(load.data, selected.state.data, exclude.state.names, state.names)

############################################################
##################    1. DATA CLEAN     ####################
############################################################

############################################################
## Task1: Visualization
## Credit to Yubo Shao
## 1.1 Pandemic Situation Visualization across States

Visualization.used.data <- 
  data.initial[, c("date", "county", "state",
                   "cases", "deaths", "total_population")]

## calculate state population
unique.county.population <- Visualization.used.data[!duplicated(Visualization.used.data,
                                                                by = c("county", "state"),
                                                                fromLast = T)]
### Note: do not need parallel computing, "for-loop" is fast enough.
for (i in selected.state.names) {
  state.population <- sum(unique.county.population[
    unique.county.population$state == i, total_population], na.rm = T)
  Visualization.used.data$state.population[Visualization.used.data$state == i] <-
    state.population
}

rm(state.population)

## separate monthly data (per 3 months)
Feb <- as.Date("2020-02-01")
Apr <- as.Date("2020-04-01")
Jul <- as.Date("2020-07-01")
Oct <- as.Date("2020-10-01")

first.season.data <- Visualization.used.data[
  which(Visualization.used.data$date >= Feb & Visualization.used.data$date < Apr),]
second.season.data <- Visualization.used.data[
  which(Visualization.used.data$date >= Apr & Visualization.used.data$date < Jul),]
third.season.data <- Visualization.used.data[
  which(Visualization.used.data$date >= Jul & Visualization.used.data$date < Oct),]
fourth.season.data <- Visualization.used.data[
  which(Visualization.used.data$date >= Oct),]

rm(Feb, Apr, Jul, Oct)

## Define two parities (based on the results of presidential election 2020)
blue.state.names <-
  c(
    "Washington",
    "Illinois",
    "California",
    "Arizona",
    "Massachusetts",
    "Wisconsin",
    "Oregon",
    "New York",
    "Georgia",
    "New Hampshire",
    "New Jersey",
    "Colorado",
    "Maryland",
    "Nevada",
    "Minnesota",
    "Pennsylvania",
    "District of Columbia",
    "Vermont",
    "Virginia",
    "Connecticut",
    "Michigan",
    "Delaware",
    "New Mexico",
    "Maine",
    "Rhode Island"
  )

red.state.names <-
  c(
    "Texas",
    "Nebraska",
    "Utah",
    "Florida",
    "North Carolina",
    "Tennessee",
    "Indiana",
    "Kentucky",
    "Oklahoma",
    "South Carolina",
    "Kansas",
    "Missouri",
    "Iowa",
    "Louisiana",
    "Ohio",
    "South Dakota",
    "Arkansas",
    "Mississippi",
    "North Dakota",
    "Wyoming",
    "Alabama",
    "Idaho",
    "Montana",
    "West Virginia"
  )

## Calculate incidence Rate (cases per 100,000)
### Define a function that help calculate the incidence rate of each state
Inci.rate.first.season <- function(dataset, state_name) {
  temp.data <- dataset[dataset$state == state_name,]
  last.day.data <- temp.data %>% group_by(county) %>% slice(which.max(date))
  sum.cases <- sum(last.day.data$cases, na.rm = T)
  Incidence.Rate <- (sum.cases/last.day.data$state.population[1]) * 100000
  return(Incidence.Rate)
}

### use parallel computing
cl.cores <- detectCores()
cl <- makeCluster(cl.cores - 1)
clusterEvalQ(cl, c(library(dplyr)))
incidence.table <-
  cbind(
    parSapply(cl, c(selected.state.names), Inci.rate.first.season, dataset = first.season.data),
    parSapply(cl, c(selected.state.names), Inci.rate.first.season, dataset = second.season.data),
    parSapply(cl, c(selected.state.names), Inci.rate.first.season, dataset = third.season.data),
    parSapply(cl, c(selected.state.names), Inci.rate.first.season, dataset = fourth.season.data)
  )
stopCluster(cl)

### Incidence rate table
incidence.table <- as.data.frame(incidence.table)
incidence.table$party[row.names(incidence.table) %in% blue.state.names] <- "Democratic"
incidence.table$party[row.names(incidence.table) %in% red.state.names] <- "Republican"
colnames(incidence.table) <- 
  c("2020/03/31", "2020/06/30", "2020/09/30", "2020/12/04", "Party")
kable(head(incidence.table, n = 10), align = c("c", "c", "c", "c", "c"), 
      caption = "Incidence rate (per 100,000 people)")

## Similar, calculate mortality Rate (No. per 100,000 people)
Mort.rate.first.season <- function(dataset, state_name) {
  temp.data <- dataset[dataset$state == state_name,]
  last.day.data <- temp.data %>% group_by(county) %>% slice(which.max(date))
  sum.deaths <- sum(last.day.data$deaths, na.rm = T)
  Mortality.Rate <- (sum.deaths/last.day.data$state.population[1]) * 100000
  return(Mortality.Rate)
}

cl.cores <- detectCores()
cl <- makeCluster(cl.cores - 1)
clusterEvalQ(cl, c(library(dplyr)))
mortality.table <-
  cbind(
    parSapply(cl, c(selected.state.names), Mort.rate.first.season, dataset = first.season.data),
    parSapply(cl, c(selected.state.names), Mort.rate.first.season, dataset = second.season.data),
    parSapply(cl, c(selected.state.names), Mort.rate.first.season, dataset = third.season.data),
    parSapply(cl, c(selected.state.names), Mort.rate.first.season, dataset = fourth.season.data)
  )
stopCluster(cl)

### Mortality table
mortality.table <- as.data.frame(mortality.table)
mortality.table$party[row.names(mortality.table) %in% blue.state.names] <- "Democratic"
mortality.table$party[row.names(mortality.table) %in% red.state.names] <- "Republican"
colnames(mortality.table) <-
  c("2020/03/31", "2020/06/30", "2020/09/30", "2020/12/04", "Party")
kable(head(mortality.table, n = 10), align = c("c", "c", "c", "c", "c"), 
      caption = "Mortality rate (per 100,000 people)")

## Calculate fatality rate
mortality.no <- as.matrix(mortality.table %>% select(-c("Party")))
incidence.no <- as.matrix(incidence.table %>% select(-c("Party")))
Fatality.no <- round(mortality.no/incidence.no * 1000, digits = 3)

### fatality table
Fatality.table <- as.data.frame(cbind(Fatality.no, mortality.table$Party))
colnames(Fatality.table) <-
  c("2020/03/31", "2020/06/30", "2020/09/30", "2020/12/04", "Party")
kable(head(Fatality.table, n = 10), align = c("c", "c", "c", "c", "c"), 
      caption = "Fatality rate (per 1,000 patients)")

rm(Fatality.no, incidence.no, mortality.no)

## 1.2 Line Chart of Incidence Rate and Mortality Rate across Country
# Credit to Yubo Shao

### calculate blue and red state total population
calculate.party.population <- unique(Visualization.used.data %>% select(c("state", "state.population")))
blue.state.population <- sum(calculate.party.population[which(calculate.party.population$state %in%
                                                                blue.state.names), "state.population"])
red.state.population <- sum(calculate.party.population[which(calculate.party.population$state %in%
                                                               red.state.names), "state.population"])

### prepare data for line chart
line.chart.data <- Visualization.used.data %>% select(c("date", "state", "cases", "deaths"))
line.chart.data$party[line.chart.data$state %in% blue.state.names] <- "Democratic"
line.chart.data$party[line.chart.data$state %in% red.state.names] <- "Republican"
line.chart.used.data <- line.chart.data %>% select(-c("state"))
unique.date <- unique(line.chart.used.data$date)

#### (1) line chart data of Incidence Rate
##### A function that can calculate the accumulate rate of assigned date among the two parities
accumulate.Incidence.rate <- function(which.date) {
  temp.data <- line.chart.used.data[line.chart.used.data$date == which.date]
  Republican.inci.rate <- round(sum(temp.data[which(temp.data$party == "Republican"), "cases"])/
                                  red.state.population * 100000, digits = 3)
  Democratic.inci.rate <- round(sum(temp.data[which(temp.data$party == "Democratic"), "cases"])/
                                  blue.state.population * 100000, digits = 3)
  rate <- as.vector(c(as.numeric(which.date), Democratic.inci.rate, Republican.inci.rate))
  return(rate)
}

##### prepare a matrix for incidence rate of each date
inci.matrix <- matrix(rep(NA, 3 * length(unique.date)), ncol = 3)
j <- 0
for (i in unique.date) {
  j <- j + 1
  inci.matrix[j, ] <- accumulate.Incidence.rate(i)
}
inci.dataframe <- as.data.frame(inci.matrix)
colnames(inci.dataframe) <- c("date", "Democratic", "Republican")
inci.dataframe$date <- as.Date(inci.dataframe$date, origin="1970-01-01")

##### The table of accumulate rate of two parities
kable(tail(inci.dataframe, n = 10), align = c("c", "c", "c"),
      caption = "Incidence rate on different date (per 100,000 people)")

#### transform into long format
inci.dataframe.long <- gather(inci.dataframe, Party, Incidence.rate, Democratic:Republican, factor_key=TRUE)

#### (2) line chart data of Mortality Rate
##### Use identical logic
accumulate.Mortality.Rate <- function(which.date) {
  temp.data <- line.chart.used.data[line.chart.used.data$date == which.date]
  Republican.inci.rate <- round(sum(temp.data[which(temp.data$party == "Republican"), "deaths"])/
                                  red.state.population * 100000, digits = 3)
  Democratic.inci.rate <- round(sum(temp.data[which(temp.data$party == "Democratic"), "deaths"])/
                                  blue.state.population * 100000, digits = 3)
  rate <- as.vector(c(as.numeric(which.date), Democratic.inci.rate, Republican.inci.rate))
  return(rate)
}

death.matrix <- matrix(rep(NA, 3 * length(unique.date)), ncol = 3)
j <- 0
for (i in unique.date) {
  j <- j + 1
  death.matrix[j, ] <- accumulate.Mortality.Rate(i)
}
death.dataframe <- as.data.frame(death.matrix)
colnames(death.dataframe) <- c("date", "Democratic", "Republican")
death.dataframe$date <- as.Date(death.dataframe$date, origin="1970-01-01")
kable(tail(death.dataframe, n = 10), align = c("c", "c", "c"), 
      caption = "Mortality rate on different date (per 100,000 people)")

##### transform into long format
death.dataframe.long <- gather(death.dataframe, Party, death.rate, Democratic:Republican, factor_key=TRUE)

#### (3) line chart data of Fatality Rate
Fatality.matrix <- matrix(rep(NA, 2 * length(unique.date)), ncol = 2)
##### fatality rate = mortality rate/incidence rate
Fatality.matrix[, 1] <- death.dataframe$Democratic/inci.dataframe$Democratic * 1000
Fatality.matrix[, 2] <- death.dataframe$Republican/inci.dataframe$Republican * 1000
Fatality.dataframe <- cbind(death.dataframe$date, as.data.frame(Fatality.matrix))
colnames(Fatality.dataframe) <- c("date", "Democratic", "Republican")
kable(tail(Fatality.dataframe, n = 10), align = c("c", "c", "c"), 
      caption = "Fatality rate on different date (per 1,000 patients)")
##### transform into long format
Fatality.dataframe.long <- gather(Fatality.dataframe, Party, Fatality.rate, Democratic:Republican, factor_key=TRUE)


rm(calculate.party.population, death.matrix, death.dataframe, Fatality.matrix, Fatality.dataframe, 
   inci.matrix, inci.dataframe, line.chart.data,line.chart.used.data, Visualization.used.data, 
   first.season.data, second.season.data, third.season.data, fourth.season.data, i, j, 
   blue.state.population, red.state.population, unique.date)

############################################################
## Task2: Predictive Model

### Define a function that can find out the data of the last day of each count
### Note: 
### It may or may not be the data on 2020/12/04
### because some counties will stop collecting data early
Gener.pred.model.data <- function(state_name) {
  temp.data <- data.initial[data.initial$state == state_name, ]
  county.last.day.data <- temp.data %>% group_by(county) %>% slice(which.max(date))
  return(county.last.day.data)
}

### use parallel computing to find the last day data of each county
cl <- makeCluster(cl.cores - 1)
registerDoParallel(cl)
clusterEvalQ(cl, c(library(dplyr)))
prediction.data <- foreach (i = selected.state.names, .combine = "rbind") %dopar% {
  Gener.pred.model.data(i)
}
stopCluster(cl)

### calculate the rate of each county
prediction.data$incidence.rate <-
  prediction.data$cases / prediction.data$total_population * 100000
prediction.data$mortality.rate <-
  prediction.data$deaths / prediction.data$total_population * 100000
prediction.data$fatality.rate <-
  prediction.data$mortality.rate / prediction.data$incidence.rate * 1000

### Add one column to specify the party of each county
prediction.data$party[prediction.data$state %in% blue.state.names] <- "Democratic"
prediction.data$party[prediction.data$state %in% red.state.names] <- "Republican"
Prediction.model.data <- prediction.data %>% select(-c("total_population", "cases", "deaths", "date"))

### Find the counts of missing data of each features
missing.features.counts <- sort(colSums(is.na(Prediction.model.data))/nrow(Prediction.model.data))
####  delete features with missing > 10%
high.missing.features.names  <- names(missing.features.counts[which(missing.features.counts > 0.1)])
Prediction.model.select.data <- Prediction.model.data %>% 
  select(- as.vector(high.missing.features.names))

### Find the counts of missing data of each county
row.NA <- rowSums(is.na(Prediction.model.select.data))
Prediction.model.select.data$row.NA <- row.NA
#### delete the county with nearly all missing value
Prediction.model.select.data <- Prediction.model.select.data[which(
  Prediction.model.select.data$row.NA != 52), ]

### Calculate the average rate
average.incidence.rate <- sum(prediction.data$cases)/sum(
  prediction.data$total_population, na.rm = TRUE) * 100000
average.mortality.rate <- sum(prediction.data$deaths)/sum(
  prediction.data$total_population, na.rm = TRUE) * 100000
average.fatality.rate <- average.mortality.rate/average.incidence.rate * 1000

### Add an indicator that whether the rate is above the average
Prediction.model.select.data$I.above.average.incidence[
  Prediction.model.select.data$incidence.rate <= average.incidence.rate] <- "No"
Prediction.model.select.data$I.above.average.incidence[
  Prediction.model.select.data$incidence.rate > average.incidence.rate] <- "Yes"

Prediction.model.select.data$I.above.average.mortality[
  Prediction.model.select.data$mortality.rate <= average.mortality.rate] <- "No"
Prediction.model.select.data$I.above.average.mortality[
  Prediction.model.select.data$mortality.rate > average.mortality.rate] <- "Yes"

Prediction.model.select.data$I.above.average.fatality[
  Prediction.model.select.data$fatality.rate <= average.fatality.rate] <- "No"
Prediction.model.select.data$I.above.average.fatality[
  Prediction.model.select.data$fatality.rate > average.fatality.rate] <- "Yes"

rm(average.fatality.rate, average.incidence.rate, average.mortality.rate,
   high.missing.features.names, missing.features.counts, row.NA, prediction.data,
   Prediction.model.data)

############################################################
##############    2. Data Visualization     ################
############################################################
###map

##incidence
# Credit to Yichu Wang
# add state
incidence <- cbind(row.names(incidence.table), (incidence.table))
colnames(incidence)[1] <- 'state'

#transform to numerical data
incidence$"2020/03/31" <- as.numeric((incidence$"2020/03/31"))
incidence$"2020/06/30" <- as.numeric((incidence$"2020/06/30"))
incidence$"2020/09/30" <- as.numeric((incidence$"2020/09/30"))
incidence$"2020/12/04" <- as.numeric((incidence$"2020/12/04"))

for (i in 1:nrow(incidence)) {
  if(incidence[i, 6] == "Democratic"){
    incidence[i, 2:5] <- incidence[i, 2:5]*(-1)
  }
}

i1 <- plot_usmap(data = incidence, values = "2020/03/31", regions = "states", exclude = c("AK","HI"),labels = TRUE) +
  labs(fill = "Incidence Rate \n (per 100k people)") +
  scale_fill_gradientn(colours = c("deepskyblue2", "white","indianred1"), limit = c(-500, 500),na.value = "white")+
  labs(title = "Incidence Rate (per 100k people) of Blue and Red States", subtitle =" (From 01/21/2020 to 03/31/2020)
") +
  theme(legend.position = "right")

i1$layers[[2]]$aes_params$size <- 2

i2 <- plot_usmap(data = incidence, values = "2020/06/30",regions = "states",exclude = c("AK","HI"),labels = TRUE) +
  labs(fill = 'Incidence Rate \n (per 100k people)') +
  scale_fill_gradientn(colours = c("dodgerblue1", "white","firebrick1"), limit = c(-2100, 2100), na.value = "white")+
  labs(title = "Incidence Rate (per 100k people) of Blue and Red States", subtitle =" (From 03/31/2020 to 06/30/2020)
") +
  theme(legend.position = "right")

i2$layers[[2]]$aes_params$size <- 2

i3 <- plot_usmap(data = incidence, values = "2020/09/30", regions = "states", exclude = c("AK","HI"),labels = TRUE) +
  labs(fill = 'Incidence Rate \n (per 100k people)') +
  scale_fill_gradientn(colours = c("dodgerblue2", "white","firebrick2"), limit = c(-4000, 4000),na.value = "white")+
  labs(title = "Incidence Rate (per 100k people) of Blue and Red States", subtitle = " (From 06/30/2020 to 09/30/2020)
") +
  theme(legend.position = "right")

i3$layers[[2]]$aes_params$size <- 2

i4 <- plot_usmap(data = incidence, values = "2020/12/04", regions = "states", exclude = c("AK","HI"),labels = TRUE) +
  labs(fill = 'Incidence Rate \n (per 100k people)') +
  scale_fill_gradientn(colours = c("dodgerblue3", "white","firebrick3"), limit = c(-11150, 11150),na.value = "white")+
  labs(title = "Incidence Rate (per 100k people) of Blue and Red States", subtitle = " (From 09/30/2020 to 12/04/2020)
") +
  theme(legend.position = "right")

i4$layers[[2]]$aes_params$size <- 2

ggsave(filename = "i1.png", plot = i1, width = 6, height = 6, units = "in", scale = 1)
i.1 <- image_read("i1.png")

ggsave(filename = "i2.png", plot = i2, width = 6, height = 6, units = "in", scale = 1)
i.2 <- image_read("i2.png")

ggsave(filename = "i3.png", plot = i3, width = 6, height = 6, units = "in", scale = 1)
i.3 <- image_read("i3.png")

ggsave(filename = "i4.png", plot = i4, width = 6, height = 6, units = "in", scale = 1)
i.4 <- image_read("i4.png")

img <- c(i.1,i.2,i.3,i.4)
image_append(image_scale(img, "x200"))
i.animation <- image_animate(image_scale(img, "500x500"), fps = 1, dispose = "previous")
image_write(i.animation, incidence)
i.animation


##Fatality

#add state
Fatality <- cbind(row.names(Fatality.table), (Fatality.table))
colnames(Fatality)[1] <- 'state'

#transform to numerical data
Fatality$"2020/03/31" <- as.numeric((Fatality$"2020/03/31"))
Fatality$"2020/06/30" <- as.numeric((Fatality$"2020/06/30"))
Fatality$"2020/09/30" <- as.numeric((Fatality$"2020/09/30"))
Fatality$"2020/12/04" <- as.numeric((Fatality$"2020/12/04"))

for (i in 1:nrow(Fatality)) {
  if(Fatality[i, 6] == "Democratic"){
    Fatality[i, 2:5] <- Fatality[i, 2:5]*(-1)
  }
}

f1 <- plot_usmap(data = Fatality, values = "2020/03/31", regions = "states", exclude = c("AK","HI"),labels = TRUE) +
  labs(fill = "Fatality Rate \n (per 100k people)") +
  scale_fill_gradientn(colours = c("dodgerblue2", "white","firebrick2"), limit = c(-100, 100),na.value = "white")+
  labs(title = "Fatality Rate (per 100k people) of Blue and Red States", subtitle =" (From 01/21/2020 to 03/31/2020)
") +
  theme(legend.position = "right")

f1$layers[[2]]$aes_params$size <- 2

f2 <- plot_usmap(data = Fatality, values = "2020/06/30",regions = "states",exclude = c("AK","HI"),labels = TRUE) +
  labs(fill = 'Fatality Rate \n (per 100k people)') +
  scale_fill_gradientn(colours = c("dodgerblue2", "white","firebrick2"),limit = c(-100, 100),na.value = "white")+
  labs(title = "Fatality Rate (per 100k people) of Blue and Red States", subtitle =" (From 03/31/2020 to 06/30/2020)
") +
  theme(legend.position = "right")

f2$layers[[2]]$aes_params$size <- 2

f3 <- plot_usmap(data = Fatality, values = "2020/09/30",regions = "states",exclude = c("AK","HI"),labels = TRUE) +
  labs(fill = 'Fatality Rate \n (per 100k people)') +
  scale_fill_gradientn(colours = c("dodgerblue2", "white","firebrick2"),limit = c(-100, 100),na.value = "white")+
  labs(title = "Fatality Rate (per 100k people) of Blue and Red States", subtitle =" (From 06/30/2020 to 09/30/2020)
") +
  theme(legend.position = "right")

f3$layers[[2]]$aes_params$size <- 2

f4 <- plot_usmap(data = Fatality, values = "2020/12/04",regions = "states",exclude = c("AK","HI"),labels = TRUE) +
  labs(fill = 'Fatality Rate \n (per 100k people)') +
  scale_fill_gradientn(colours = c("dodgerblue2", "white","firebrick2"),limit = c(-100, 100),na.value = "white")+
  labs(title = "Fatality Rate (per 100k people) of Blue and Red States", subtitle =" (From 09/30/2020 to 12/04/2020)
") +
  theme(legend.position = "right")

f4$layers[[2]]$aes_params$size <- 2

##mortality

#add state
mortality <- cbind(row.names(mortality.table),(mortality.table))
colnames(mortality)[1]='state'

#transform to numerical data
mortality$"2020/03/31"=as.numeric((mortality$"2020/03/31"))
mortality$"2020/06/30"=as.numeric((mortality$"2020/06/30"))
mortality$"2020/09/30"=as.numeric((mortality$"2020/09/30"))
mortality$"2020/12/04"=as.numeric((mortality$"2020/12/04"))

for (i in 1:nrow(mortality)) {
  if(mortality[i, 6] == "Democratic"){
    mortality[i, 2:5] = mortality[i, 2:5]*(-1)
  }
}

m1 <- plot_usmap(data = mortality, values = "2020/03/31",regions = "states",exclude = c("AK","HI"),labels = TRUE) +
  labs(fill = "Mortality Rate \n (per 100k people)") +
  scale_fill_gradientn(colours = c("deepskyblue2", "white","indianred1"), limit = c(-50, 50),na.value = "white")+
  labs(title = "Mortality Rate (per 100k people) of Blue and Red States", subtitle =" (From 01/21/2020 to 03/31/2020)
") +
  theme(legend.position = "right")

m1$layers[[2]]$aes_params$size <- 2

m2 <- plot_usmap(data = mortality, values = "2020/06/30",regions = "states",exclude = c("AK","HI"),labels = TRUE) +
  labs(fill = 'Mortality Rate \n (per 100k people)') +
  scale_fill_gradientn(colours = c("dodgerblue2", "white","firebrick2"), limit = c(-200, 200),na.value = "white")+
  labs(title = "Mortality Rate (per 100k people) of Blue and Red States", subtitle =" (From 03/31/2020 to 06/30/2020)
") +
  theme(legend.position = "right")

m2$layers[[2]]$aes_params$size <- 2

m3 <- plot_usmap(data = mortality, values = "2020/09/30",regions = "states",exclude = c("AK","HI"),labels = TRUE) +
  labs(fill = 'Mortality Rate \n (per 100k people)') +
  scale_fill_gradientn(colours = c("dodgerblue2", "white","firebrick2"), limit = c(-200, 200),na.value = "white")+
  labs(title = "Mortality Rate (per 100k people) of Blue and Red States", subtitle =" (From 06/30/2020 to 09/30/2020)
") +
  theme(legend.position = "right")

m3$layers[[2]]$aes_params$size <- 2

m4 <- plot_usmap(data = mortality, values = "2020/12/04",regions = "states", exclude = c("AK","HI"),labels = TRUE) +
  labs(fill = 'Mortality Rate \n (per 100k people)') +
  scale_fill_gradientn(colours = c("dodgerblue2", "white","firebrick2"), limit = c(-200, 200),na.value = "white")+
  labs(title = "Mortality Rate (per 100k people) of Blue and Red States",subtitle =" (From 09/30/2020 to 12/04/2020)
") +
  theme(legend.position = "right")

m4$layers[[2]]$aes_params$size <- 2


###line chart
I <- ggplot() + geom_line(aes(date, Incidence.rate, color = factor(Party)), inci.dataframe.long) +
  scale_color_manual(name = "Party",values = c("dodgerblue","firebrick"))+
  scale_x_date(date_labels = "%m/%Y")+
  theme(panel.grid = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) + labs(x = "Date", y = "Incidence Rate (per 100,000)") +
  ggtitle("Incidence Rate of Blue and Red States", subtitle = "From 01/21/2020 to 12/04/2020")


M <- ggplot() + geom_line(aes(date, death.rate, color = factor(Party)), death.dataframe.long) +
  scale_color_manual(name = "Party",values = c("dodgerblue","firebrick"))+
  scale_x_date(date_labels = "%m/%Y")+
  theme(panel.grid = element_blank(), panel.background = element_blank(),
        axis.line = element_line(colour = "black")) + labs(x = "Date", y = "Mortality Rate (per 100,000)") +
  ggtitle("Mortality Rate of Blue and Red States", subtitle = "From 01/21/2020 to 12/04/2020")


f <- ggplot() + geom_line(aes(date, Fatality.rate, color = factor(Party)), Fatality.dataframe.long) +
  scale_color_manual(name = "Party",values = c("dodgerblue","firebrick"))+
  scale_x_date(date_labels = "%m/%Y")+
  theme(panel.grid = element_blank(), panel.background = element_blank(),
        axis.line = element_line(colour = "black")) + labs(x = "Date", y = "Fatality Rate (per 100,000)") +
  ggtitle("Fatality Rate of Blue and Red States", subtitle = "From 01/21/2020 to 12/04/2020")

############################################################
###############    3. Predictive Model     #################
############################################################
# Credit to Lingxuan Kong
# Warnings are GLM fit warning, not coding error.

Forward_Select <- function(X, A, N, C) {
  #Variable select
  C <- c(C)
  name <- names(X)
  A <- c(A)
  name <- name[-which(name == A)]
  Res <- data.frame(name, pVal = NA)
  for (i in 1:length(name)) {
    mod <- glm(paste(A, "~", name[i]), data = X, family = "binomial")
    Res[i, 2] <- summary(mod)$coefficients[2, 4]
  }
  Res[, 2] <- round(Res[, 2], 4)
  Res[, "sig"] <- 0
  Res[which(Res$pVal < 0.01), "sig"] <- 1
  #Forward select
  name2 <- Res[which(Res$sig == 1), 1]
  included <- c()
  pVal <- c()
  AIC <- c()
  flag <- FALSE
  j <- 1
  inclu <- name[which.min(Res[, 2])]
  name2 <- name2[-which(name2 == inclu)]
  while (flag == FALSE) {
    pVal <- c()
    AIC <- c()
    for (i in 1:length(name2)) {
      mod <-
        glm(paste(A, "~", inclu, "+", name2[i], sep = ""),
            data = X,
            family = "binomial")
      pVal[i] <- summary(mod)$coefficients[(length(inclu) + 1), 4]
      AIC[i] <- mod$aic
    }
    if (length(which(pVal <= 0.01)) == 0) {
      flag <- TRUE
      return(included)
    }
    if (j > N) {
      return(included)
    }
    if (which.min(pVal) == which.min(AIC)) {
      included[j] <- name2[which.min(pVal)]
      name2 <- name2[-which.min(pVal)]
      inclu <- paste(inclu, "+", included[j])
      j <- j + 1
    } else{
      if (C == "pval") {
        #Criteria = p-value
        included[j] <- name2[which.min(pVal)]
        name2 <- name2[-which.min(pVal)]
        inclu <- paste(inclu, "+", included[j])
        j <- j + 1
      }
      if (C == "AIC") {
        #Criteria = AIC
        included[j] <- name2[which.min(AIC)]
        name2 <- name2[-which.min(AIC)]
        inclu <- paste(inclu, "+", included[j])
        j <- j + 1
      }
    }
  }
}

# Credit to Li Liu

# Load Data
DATA <- read.csv("/Users/shaoyubo/Desktop/UMich/Course/Fall 2021/BIOSTAT 625/HW/Final project/Final_Project/Prediction.model.select.data.csv")
DATA <- DATA[, -57]

# Transforming T/F to 0/1 and then do factors
DATA[which(DATA$presence_of_water_violation == "TRUE"), "presence_of_water_violation"] <- 1
DATA[which(DATA$presence_of_water_violation == "FALSE"), "presence_of_water_violation"] <- 0

DATA[which(DATA$party == "Republican"), "party"] <- 1
DATA[which(DATA$party == "Democratic"), "party"] <- 0
DATA$party <- as.factor(DATA$party)

DATA[which(DATA$I.above.average.fatality == "Yes"), "I.above.average.fatality"] <- 1
DATA[which(DATA$I.above.average.fatality == "No"), "I.above.average.fatality"] <- 0
DATA$I.above.average.fatality <- as.factor(DATA$I.above.average.fatality)

DATA[which(DATA$I.above.average.incidence == "Yes"), "I.above.average.incidence"] <- 1
DATA[which(DATA$I.above.average.incidence == "No"), "I.above.average.incidence"] <- 0
DATA$I.above.average.incidence <- as.factor(DATA$I.above.average.incidence)

DATA[which(DATA$I.above.average.mortality == "Yes"), "I.above.average.mortality"] <- 1
DATA[which(DATA$I.above.average.mortality == "No"), "I.above.average.mortality"] <- 0
DATA$I.above.average.mortality <- as.factor(DATA$I.above.average.mortality)
DATA <- DATA[, 4:59]

Forward_Select(DATA,"I.above.average.fatality", 10, "pval")
Forward_Select(DATA,"I.above.average.incidence", 10, "pval")
Forward_Select(DATA,"I.above.average.mortality", 10, "pval")

#Prediction models
set.seed(100)

# Create DATAset and fit the NA values
DATA <- subset(DATA, select = -c(mortality.rate, fatality.rate, incidence.rate) )
DATA <- na.roughfix(DATA)

Trainingindex <- createDataPartition(DATA$I.above.average.fatality, p = 0.8, list = FALSE)
trainingset <- DATA[Trainingindex, ]
testingset <- DATA[-Trainingindex, ]

# Parallel Computing for Random Forest
unregister_dopar <- function() {
  env <- foreach:::.foreachGlobals
  rm(list = ls(name = env), pos = env)
}

unregister_dopar()
cl <- makeCluster(cl.cores - 1)
registerDoParallel(cl)
start.time <- proc.time()
Max <- 0
Max_i <- 0


for (i in 1:floor(sqrt(nrow(trainingset)))) {
  rf <- foreach(
    ntree = rep(400, 8),
    .combine = randomForest::combine,
    .multicombine = TRUE,
    .packages = 'randomForest'
  ) %dopar% {
    randomForest(
      I.above.average.fatality ~ . - I.above.average.mortality - I.above.average.incidence,
      na.action = na.fail,
      data = trainingset,
      importance = TRUE,
      proximity = TRUE,
      ntree = ntree,
      mtry = i
    )
  }
  pred = predict(rf, testingset)
  ans = sum(pred == testingset$I.above.average.fatality) / length(pred)
  if (ans > Max) {
    Max <- ans
    Max_i <- i
    print(Max) # Used in Parameter Tuning
    print(Max_i)
  }
}

stop.time <- proc.time()
run.time <- stop.time - start.time
print(run.time)
stopCluster(cl)

# Logistic Regression, since the phenomenon of 100% accuracy (See report) will not affect 
# Logistic Regression, we do not remove the other 2 outcomes, but just put all of them into
# features.
model <- glm(I.above.average.fatality~., na.action = na.omit, family = "binomial", data = trainingset)
pred2 <- predict(model, testingset, type = "response")
pred2 <- ifelse(pred2 >= 0.5, 1, 0)
table(pred2, testingset$I.above.average.fatality)
sum(pred2 == testingset$I.above.average.fatality) / length(pred2)

print(roc(as.numeric(pred2), as.numeric(testingset$I.above.average.fatality)))
print(roc(as.numeric(pred), as.numeric(testingset$I.above.average.fatality)))








