# install.packages("lubridate") 
# install.packages("tidyverse")
# install.packages("tmap")
# install.packages("gifski")
# install.packages("ggpubr")

# install.packages("EpiModel")
# install.packages("COVID19")
# install.packages("timetk")
# install.packages("broom")
# install.packages("knitr")
# install.packages("cowplot")

library(lubridate)
library(tidyverse)
library(ggpubr)
data <- read.csv("https://covid19.who.int/WHO-COVID-19-global-data.csv",
                 na.strings = "", fileEncoding = "UTF-8-BOM", stringsAsFactors = F)

###############################################################################
# ---------dinamically calculate days for updated data----------
start <- as.Date("2020-02-15",format="%Y-%m-%d") #starting date we'll filter the firs days where data are few and not so useful
end   <- as.Date(Sys.Date(),format="%Y-%m-%d")
steps <- 7 # days between plot updates
interval <- as.numeric(ceiling((end-start)/steps))
delay <- 1 #set de delay for up to date data
prediction_window <- 180 #day shown in the prediction plot
##################################################################

##Data country names correction
data$Country[data$Country == "Russian Federation"] <- "Russia"
data$Country[data$Country == "Bolivia (Plurinational State of)"] <- "Bolivia"
data$Country[data$Country == "Venezuela (Bolivarian Republic of)"] <- "Venezuela"
data$Country[data$Country == "The United Kingdom"] <- "United Kingdom"
data$Country[data$Country == "Iran (Islamic Republic of)"] <- "Iran"
data$Country[data$Country == "Congo"] <- "Republic of Congo"
data$Country[data$Country == "Côte d’Ivoire"] <- "Ivory Coast"
data$Country[data$Country == "Czechia"] <- "Czech Republic"
data$Country[data$Country == "Republic of Moldova"] <- "Moldova"
data$Country[data$Country == "Syrian Arab Republic"] <- "Syria"
data$Country[data$Country == "Serbia"] <- "Republic of Serbia"
data$Country[data$Country == "Viet Nam"] <- "Vietnam"

##Check and delete null values
str(data)
View(data)
summary(data)
colSums(is.na(data))

##Change format to Date
data$Date_reported <- as.Date(data$Date_reported, format = "%Y-%m-%d")
summary(data)

##Total cases in the World's countries
data %>% 
  group_by(Country) %>% 
  summarise(cases_max = max(Cumulative_cases)) %>%
  arrange(desc(cases_max))

##Total deaths in the World's countries
data %>% 
  group_by(Country) %>% 
  summarise(deaths_max = max(Cumulative_deaths)) %>% 
  arrange(desc(deaths_max))


##Plotting cases in each Continent to date
barcases <- ggplot(data=data %>% filter(data$WHO_region != "Other"), aes(reorder(WHO_region, -New_cases, sum), y=New_cases, fill=WHO_region))+
    geom_col(show.legend=FALSE) + xlab("WHO Region")+
    ylab("Cumulative Cases")+
    theme(axis.text=element_text(size=17),  axis.title=element_text(size=17,face="bold"))

##Plotting deaths in each Continent to date
bardeaths <- ggplot(data=data %>% filter(data$WHO_region != "Other"), aes(reorder(WHO_region, -New_deaths, sum), y=New_deaths, fill=WHO_region))+
    geom_col(show.legend=FALSE) + xlab("WHO Region")+
    ylab("Cumulative Deaths")+
    theme(axis.text=element_text(size=17),  axis.title=element_text(size=17,face="bold"))

ggarrange(barcases, bardeaths, 
          ncol = 2, nrow = 1)

##Plotting cases to date
total_cases <- data %>%
  group_by(Date_reported) %>%
  summarize(cases = sum(New_cases))
total_cases <- ggplot(total_cases, aes(Date_reported, as.numeric(cases))) +
  geom_col(fill = "blue", alpha = 0.6) + 
  theme_minimal(base_size = 14) +
  xlab(NULL) + ylab(NULL) + 
  scale_x_date(date_labels = "%d/%m/%Y")
total_cases + labs(title="COVID-19 Cases to date - Worldwide")  + theme(axis.text=element_text(size=17),  axis.title=element_text(size=17,face="bold"))

##Plotting deaths to date
total_deaths <- data %>%
  group_by(Date_reported) %>%
  summarize(deaths = sum(New_deaths))
total_deaths <- ggplot(total_deaths, aes(Date_reported, as.numeric(deaths))) +
  geom_col(fill = "blue", alpha = 0.6) + 
  theme_minimal(base_size = 14) +
  xlab(NULL) + ylab(NULL) + 
  scale_x_date(date_labels = "%d/%m/%Y")
total_deaths + labs(title="COVID-19 Deaths to date - Worldwide") + theme(axis.text=element_text(size=17),  axis.title=element_text(size=17,face="bold"))

##Total cases curve of the World 
attach(data)
cumulativecasesplot <- data %>%
  group_by(Date_reported) %>%
  summarise(total_cases = sum(Cumulative_cases))%>%
  ggplot(aes(x = Date_reported, y = total_cases)) +
  geom_line(size = 1) +
  ggtitle("Time series graph of cases for World")

##Total deaths curve of the World 
attach(data)
cumulativedeathsplot <-data %>%
  group_by(Date_reported) %>%
  summarise(total_deaths = sum(Cumulative_deaths))%>%
  ggplot(aes(x = Date_reported, y = total_deaths)) +
  geom_line(size = 1) +
  ggtitle("Time series graph of deaths for World")  

ggarrange(cumulativecasesplot, cumulativedeathsplot, 
          ncol = 2, nrow = 1)

##Total cases curve of Italy
italycumulativecasesplot <- data %>%
  group_by(Country_code, Date_reported) %>%
  filter(Country_code == "IT") %>%
  ggplot(aes(x = Date_reported, y = Cumulative_cases)) +
  geom_line(size = 1) +
  ggtitle("Time series graph of cases for Italy")

##Total deaths curve of the Italy
italycumulativedeathsplot <- data %>%
  group_by(Country_code, Date_reported) %>%
  filter(Country_code == "IT") %>%
  ggplot(aes(x = Date_reported, y = Cumulative_deaths)) +
  geom_line(size = 1) +
  ggtitle("Time series graph of deaths for Italy")

ggarrange(italycumulativecasesplot, italycumulativedeathsplot, 
          ncol = 2, nrow = 1)

##Cases by country for select countries in log10 scale
selected_countries <- data %>% filter(Country %in% c("United States of America", "India", "Brazil", "France", "Germany", "Russia", "Italy"))
ggplot(selected_countries, aes(x=Date_reported, y=Cumulative_cases, colour=Country)) + geom_line(size=1) +
  theme_classic() +
  labs(title = "Covid-19 Cases by Country to date", x= "Date", y= "Cumulative cases (log scale)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(trans="log10")

###############################################################################
##Visualize the relationship between cases and deaths in Italy on date
target_country <- "Italy" #to select the desired country to focus
Selected_Country_from_data <- data %>%filter(data$Country == target_country, data$Date_reported > start-1) 

Selected_Country_from_data %>%
  ggplot(aes(Date_reported, New_cases)) +
  geom_line(color = "orange") +
  theme(legend.position = "none") +
  geom_line(aes(x = Date_reported, y = New_deaths*30), color = "red") +
  scale_y_continuous(
    labels = scales::comma,
    name = "Cases",
    sec.axis = sec_axis(New_deaths ~ . /30,
                        name = "Deaths",
                        labels = scales::comma
    )
  ) +
  theme_grey(base_size = 15)+
  theme(
    axis.title.y = element_text(color = "orange", size = 22),
    axis.title.y.right = element_text(color = "red", size = 22)
  ) +
  labs(
    title = "Cases vs Deaths in Italy",
    subtitle = "Data on date",
    caption = "Source: WHO",
    x = "Date"
  )


# ---------------Plot first world map ---------------

library(tmap)
data(World)
class(World)

data_of_the_day <- data %>% filter(Date_reported == (end-delay))
map_of_the_day <- left_join(World, data_of_the_day, by= c("sovereignt"="Country"))

##Plotting World Map of the day
attach(map_of_the_day)
ggplot(data = map_of_the_day) + geom_sf(aes(fill=Cumulative_cases), color="black") +
  ggtitle("World Map of Confirmed Covid Cases",
          subtitle=paste("Total Cases on",(end-delay)))


# ----------------------------Dynamic Plot using daily updated data------------------------

##Create a list of length equals interval days
list_of_days <- vector(mode='list', length= interval )

actual_date <- start
i<-1

while (actual_date <= end)
{
  date_filtered_data <- data %>% filter(data$Date_reported %in% as.Date(actual_date))
  temp_map <- left_join(World, date_filtered_data, by= c("sovereignt"="Country"))
  list_of_days[[i]] <- tm_shape(temp_map)+tm_polygons("Cumulative_cases")+
    tm_layout(title= actual_date, title.position = c('left', 'top'))

  i <- i+1
  actual_date <- actual_date + steps
}
##Animate the plot
tmap_animation(list_of_days, height = 720, width = 990) #use this to show the gif

#Use this to save the gif
#tmap_animation(list_of_days, height = 720, width = 990, filename = "-------directory-------") 


###########################################################################################################


##Plotting Incidence Ratio by WHO Region 
library(ggpubr)
library(dplyr)

colSums(is.na(map_of_the_day))
map_of_the_day <- na.omit(map_of_the_day)
colSums(is.na(map_of_the_day))

##Create IR column
map_of_the_day$IR <- map_of_the_day$New_cases/map_of_the_day$pop_est

JoinData <- map_of_the_day
JoinData$IR <- JoinData$Cumulative_cases/JoinData$pop_est
attach(JoinData)


JoinData <- as.data.frame(JoinData)

asd <- aggregate(JoinData$IR, by=list(JoinData$WHO_region), FUN=mean)
attach(asd)
asd = as.data.frame(asd)
ggplot(data=asd, aes(reorder(asd$Group.1, -asd$x, sum), y=asd$x, fill=asd$Group.1))+
  geom_col(show.legend=FALSE) + xlab("WHO Region")+
  ylab("Incidence Ratio")+
  theme(axis.text=element_text(size=17),  axis.title=element_text(size=17,face="bold"))


##Normality tests

#IR qqplot
ggqqplot(JoinData$IR)+
  ggtitle("Incidence Ratio (IR) of cases")

#Shapiro test to verify the normality of the data
shapiro.test(JoinData$IR[0:5000])


#Density curve and qq-plot
ggdensity(data$New_cases, main="Daily cases in the world", xlab="Daily cases")

qqcasesplot <- ggqqplot(data$New_cases)+
  ggtitle("qqplot daily Cases")+
  theme_grey(base_size = 19)


#Shapiro test to verify the normality of the data
shapiro.test(data$New_cases[0:5000])

#Density curve and qq-plot
ggdensity(data$New_deaths, main="Daily deaths in the world", xlab="Daily deaths")

qqdeathplot <- ggqqplot(data$New_deaths)+
  ggtitle("qqplot daily Deaths")+
  theme_grey(base_size = 19)

#Shapiro test to verify the normality of the data
shapiro.test(data$New_deaths[0:5000])

ggarrange(qqcasesplot, qqdeathplot, 
          ncol = 2, nrow = 1)


# ---------SIR ICM MODEL------------
library(EpiModel)

my_map_Country <- map_of_the_day[map_of_the_day$sovereignt ==target_country,]
attach(my_map_Country)
RR <- (14793420/as.numeric(end-start))/pop_est #total recovered ratio based on Ita data found on https://www.worldometers.info/coronavirus/country/italy/
pop_scale <- 100 #scale the population by result_scale factor to reduce compute complexity while using real population data
#Setting up a SIR model to predict next nsteps days curves iteranting over nsims simulation for better results 
control <- control.icm(type = "SIR", nsteps = 40 , nsims = 3) 
#setting up initial conditions using our up to date data
init <- init.icm(s.num = (pop_est/pop_scale)*(1-IR) , i.num = (pop_est/pop_scale)*(IR) , r.num = (pop_est/pop_scale)*RR) #we need to estimate the already recovered o vax people
# setting up parameters like:
#infection.prob for an encounter, infect encounter rate 10 encounters/day, recovery rate 20 equals to 14gg ca
#arrival rate for births or immigration , departure rate for non infected deaths or emigration
# departure for infected and recovered deaths or emigration
nrate <- 6.7 #births
imrate <- (43479/pop_est)*1000 #immigrations
emrate <- (100356/pop_est)*1000 #emigrations
drate <- 12 #deaths

param <- param.icm(inf.prob = 0.05, act.rate = 10, rec.rate = 1/20, 
                   a.rate = ((nrate+imrate)/365)/1000, ds.rate = (((drate+emrate)/365)/1000)*(1-IR), di.rate = (((drate+emrate)/365)/1000)*(IR), 
                   dr.rate = (((drate+emrate)/365)/1000)*RR)

sim <- icm(param, init, control)
#show SIR predicted curve
plot(sim,main = "SIR predicted curve")

#new cases per day predicted
plot(sim, y = "si.flow", mean.col = "red", qnts.col = "red", main="Predicted new cases")
#if u want to use the interactive version use epiweb("icm") and insert the same values


##################################################################################################
##OUR Custom SIR MODEL 
attach(Selected_Country_from_data)
library(COVID19)
library(timetk) # contains slidify function
library(broom)
library(knitr)
library(cowplot)

#function to smooth data using mean function over steps days
mean_roll_N <- slidify(mean, .period = steps, .align = "middle") 

Selected_Country_from_data$mean_cases_Nday <- (Cumulative_cases - lag(Cumulative_cases, steps))/steps
Selected_Country_from_data$mean_deaths_Nday <- (Cumulative_deaths - lag(Cumulative_deaths, steps))/steps

Selected_Country_from_data$mean_cases_1day <- (Cumulative_cases - lag(Cumulative_cases, 1))
Selected_Country_from_data$mean_deaths_1day <- (Cumulative_deaths - lag(Cumulative_deaths, 1))

#let's omit the NA values created by the lag operator
Selected_Country_from_data <- na.omit(Selected_Country_from_data)
attach(Selected_Country_from_data)

#Selected country plot of cases vs deaths with an interval of steps days
coeff <- 30 #coefficente di scala per vedere meglio i dati
g <- Selected_Country_from_data %>%
  ggplot(aes(Date_reported, mean_cases_Nday)) +
  geom_line(color = "orange") +
  theme(legend.position = "none") +
  geom_line(aes(x = Date_reported, y = mean_deaths_Nday * coeff), color = "red") +
  scale_y_continuous(
    labels = scales::comma,
    name = "Cases",
    sec.axis = sec_axis(mean_deaths_Nday ~ . / coeff,
                        name = "Deaths",
                        labels = scales::comma
    )
  ) +
  theme(
    axis.title.y = element_text(color = "orange", size = 13),
    axis.title.y.right = element_text(color = "red", size = 13)
  ) +
  labs(
    title = "Contry Cases vs. Deaths",
    subtitle = "Steps-Day Average",
    x = "Date"
  )
show(g)

# create columns for deaths led 0 to N days ahead
cutoff_end <- max(Selected_Country_from_data$Date_reported) - steps
max_lead <- 15 # number of days ahead to consider
country_lags <- Selected_Country_from_data %>%
  # create lags by day
  tk_augment_lags(mean_deaths_Nday, .lags = 0:-max_lead, .names = "auto")
# fix names to remove minus sign
names(country_lags) <- names(country_lags) %>% str_replace_all("lag-|lag", "lead")

# use only case dates where we have complete future knowledge of deaths for all lead times.
country_lags <- country_lags %>% filter(Date_reported < cutoff_end - max_lead)

# make long form to nest
# initialize models data frame
models <- country_lags %>%
  ungroup() %>%
  pivot_longer(
    cols = contains("lead"),
    names_to = "lead",
    values_to = "led_deaths"
  ) %>%
  select(Date_reported, mean_cases_Nday, lead, led_deaths) %>%
  mutate(lead = as.numeric(str_remove(lead, "mean_deaths_Nday_lead"))) %>%
  nest(data = c(Date_reported, mean_cases_Nday, led_deaths))
# Run a regression on lagged cases and date vs deaths
models <- models %>% mutate(model = map(
  data,
  function(df) {
    lm(led_deaths ~ mean_cases_Nday, data = df)
  }
))

#Add regression coefficient
#Get adjusted r squared
models <- models %>%
  mutate(adj_r = map(model, function(x) {
    glance(x) %>%
      pull(adj.r.squared)
  })
  %>% unlist())
print(models)

#Show model fit by lead time
#make predictions using best model
best_fit <- models %>%
  summarize(adj_r = max(adj_r)) %>%
  left_join(models, by = "adj_r")

models %>%
  ggplot(aes(lead, adj_r)) +
  geom_line() +
  labs(
    subtitle = paste("Best fit lead =", best_fit$lead, "days"),
    title = "Model Fit By Lag Days",
    x = "Lead Time in Days for Deaths",
    y = "Adjusted R-squared"
  )
best_fit$model[[1]] %>% tidy()

# ------------------------------------------
#See how well our model predicts
make_predictions <- function(newdata,single_model){
  predicted_deaths <-predict(single_model$model[[1]], newdata = newdata)
  Date_reported <- seq.Date(from = min(newdata$Date_reported + single_model$lead),
                   to = max(newdata$Date_reported) + single_model$lead, 
                   by = 1)
  display <- full_join(newdata, tibble(Date_reported, predicted_deaths)) %>% filter(Date_reported> (end-prediction_window))
  return(display)
}

# -----------------------------------------------------------
#Function to create prediction plot for deaths
show_predictions <- function(newdata,model,label) {
  display <- make_predictions(newdata,model)
  gg <- display %>%
    pivot_longer(cols = where(is.numeric)) %>%
    filter(name %in% c("New_deaths", "predicted_deaths")) %>%
    ggplot(aes(Date_reported, value, color = name)) +
    geom_line(size = 1.5 ) +
    labs(
      title = paste("Actual vs. Predicted Deaths",label),
      x = "Date",
      y = "Count",
    ) + theme_grey(base_size = 17)
  gg
}
show_predictions(Selected_Country_from_data,best_fit,"National Model")
