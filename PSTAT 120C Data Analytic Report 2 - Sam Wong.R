#For #1
library(dplyr)
library(stringr)
library(zoo)
library(ggplot2)
library(urbnmapr)

base = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/'
death = 'time_series_covid19_deaths_'
confirm = 'time_series_covid19_confirmed_'

us_death = read.csv(paste0(base,death,"US.csv"))
us_confirm = read.csv(paste0(base,confirm,"US.csv"))
 
col_names = colnames(us_death)
all_dates = as.Date(paste0(str_sub(col_names[13:dim(us_death)[2]], 2, -1), "20"), tryFormats = c("%m.%d.%Y"))

covid19_project_url = "https://api.covidtracking.com/v1/states/daily.csv"
covid_19_project = read.csv(covid19_project_url)
covid_19_project$date = as.Date(as.character(covid_19_project$date), "%Y %m %d")

nation_death = us_death %>%
  dplyr::filter(Country_Region == "US")

nation_confirmed = us_confirm %>%
  dplyr::filter(Country_Region == "US")

nation_death[1,]
nation_death_sum = apply(nation_death[,12:dim(nation_death)[2]], 2, sum)
nation_confirmed_sum = apply(nation_confirmed[,12:dim(nation_confirmed)[2]], 2, sum)

#1) alright we getting there
start_date = as.Date("2020-3-25")
end_date = as.Date("2020-10-25")

nation_death_selected = nation_death_sum[1 + which(all_dates %in% seq.Date(start_date, end_date, by=1))]
nation_confirmed_selected = nation_confirmed_sum[which(all_dates %in% seq.Date(start_date, end_date, by=1))]
nation_death_selected=as.numeric(nation_death_selected)
nation_confirmed_selected=as.numeric(nation_confirmed_selected)

daily_date_selected=date_selected[2:length(date_selected)]
nation_confirmed_selected_daily=nation_confirmed_selected[2:length(nation_confirmed_selected)]-nation_confirmed_selected[1:(length(nation_confirmed_selected)-1)]
daily_confirmed_nation_df = data.frame(date = daily_date_selected, value = nation_confirmed_selected_daily)

nation_death_selected_daily=nation_death_selected[2:length(nation_death_selected)]-nation_death_selected[1:(length(nation_death_selected)-1)]

daily_death_nation_df = data.frame(date = daily_date_selected, value = nation_death_selected_daily)

daily_confirmed_nation_df %>%
  ggplot(aes(x=date, y=value)) +
  geom_bar(stat = 'identity', color="white",  fill="#ff8540", width = 1) +
  ylab("Daily confirmed cases in  US")+
  xlab("Date")+ 
  theme(text = element_text(size = 20),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.key.width=unit(1,"cm"),
        axis.text.y = element_text(angle=90, hjust=1))
dev.off()

nation_test = covid_19_project %>%  
  dplyr::select(date, state,totalTestResultsIncrease, positiveIncrease)

nation_test_aggregated = nation_test %>%
  group_by(date) %>%
  summarise_each(funs(sum), positiveIncrease, totalTestResultsIncrease)

For #2) 
library(deSolve)
library(mFilter)
library(RobustGaSP)
library(matrixStats)

source(file = "data_and_functions/functions_covid19.R")
set.seed(1)

state_name = "Michigan"
state_name_short = "MI"
county_name = "Santa Barbara"

fitted_days_beta=180
gamma = 0.2
theta = 0.1
delta = 0.0066

county_death = us_death%>%
  filter(Admin2 == county_name, Province_State == Michigan) %>%
  select(starts_with(MI))
county_confirmed = us_confirm %>%
  filter(Admin2 == county_name, Province_State == Michigan) %>%
  select(starts_with(MI))

county_death_sum = apply(county_death, 2, sum)

county_confirmed_sum = apply(county_confirmed, 2, sum)


county_death_selected = county_death_sum[which(all_dates %in% seq.Date(start_date, end_date, by=1))]
county_confirmed_selected = county_confirmed_sum[which(all_dates %in% seq.Date(start_date, end_date, by=1))]

county_death_selected=as.numeric(county_death_selected)
county_confirmed_selected=as.numeric(county_confirmed_selected)

county_confirmed_selected_daily_avg = data_seven_day_smoothing(county_confirmed_selected_daily)
daily_confirmed_county_avg_df = data.frame(date = daily_date_selected, value = county_confirmed_selected_daily_avg)

daily_confirmed_county_avg_df %>%
  ggplot(aes(x=date, y=value)) +
  geom_bar(stat = 'identity', color="white",  fill="#ff8540", width = 1) +
  ylab("Daily confirmed cases in Michigan")+
  xlab("Date")+ 
  theme(text = element_text(size = 20),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.key.width=unit(1,"cm"),
        axis.text.y = element_text(angle=90, hjust=1))
dev.off() ##close it

#positive rate stuff
nation_test = covid_19_project %>%  
  dplyr::select(date, state,totalTestResultsIncrease, positiveIncrease)

nation_test_aggregated = nation_test %>%
  group_by(date) %>%
  summarise_each(funs(sum), positiveIncrease, totalTestResultsIncrease)

nation_test_aggregated$positiveIncrease_7_day_avg = data_seven_day_smoothing(nation_test_aggregated$positiveIncrease)

nation_test_aggregated$totalTestResultsIncrease_7_day_avg = data_seven_day_smoothing(nation_test_aggregated$totalTestResultsIncrease)

nation_test_aggregated$positive_rate = nation_test_aggregated$positiveIncrease_7_day_avg / nation_test_aggregated$totalTestResultsIncrease_7_day_avg

#some cleaning stuff
us_death = read.csv(paste0(base,death,"US.csv"))
us_confirm = read.csv(paste0(base,confirm,"US.csv"))

covid19_project_url = "https://api.covidtracking.com/v1/states/daily.csv"

# get state level test data from COVID-19 tracking project
covid_19_project = read.csv(covid19_project_url)
covid_19_project$date = as.Date(as.character(covid_19_project$date), "%Y %m %d")

file_pow_param_path = "data_and_functions/"
file_sudden_death_path = "data_and_functions/"

# counties that have sudden death increase
county_sudden_death = read.csv(paste0(file_sudden_death_path,"counties with sudden death increase.csv" ) )
county_sudden_death$state = as.character(county_sudden_death$state)

# read the file for optim power parameter in US states
us_pow_param = read.csv(paste0(file_pow_param_path, "power parameter in US states.csv"))
us_pow_param$state_name = as.character(us_pow_param$state_name)

# clean the death data
us_death = clean_us_death(us_death)

# extract the dates in the death data
col_names = colnames(us_death)
all_dates = as.Date(paste0(str_sub(col_names[13:dim(us_death)[2]], 2, -1), "20"), tryFormats = c("%m.%d.%Y"))

# calculate the death pre million
death_rate = function(x){x/us_death$Population*10^6}
us_death_rate = us_death %>%
  dplyr::mutate_at(vars(starts_with("X")), death_rate)

n_ini = as.numeric(end_date - start_date_ini) + 1

power_parameter = as.numeric(us_pow_param$pow_param[us_pow_param$state_name == state_name])

state_test = covid_19_project %>%
  dplyr::filter(state == state_name_short) %>%
  dplyr::select(date, totalTestResultsIncrease, positiveIncrease)

state_test$totalTestResultsIncrease[state_test$totalTestResultsIncrease<0] = abs(state_test$totalTestResultsIncrease[state_test$totalTestResultsIncrease<0])
state_test$positiveIncrease[state_test$positiveIncrease<0] = abs(state_test$positiveIncrease[state_test$positiveIncrease<0])
state_test$daily_test_avg = rep(0, dim(state_test)[1])
state_test$daily_positive_avg = rep(0, dim(state_test)[1])
state_test$daily_test_avg = data_seven_day_smoothing(state_test$totalTestResultsIncrease)
state_test$daily_positive_avg = data_seven_day_smoothing(state_test$positiveIncrease)

state_test$PositiveRate = state_test$daily_positive_avg / state_test$daily_test_avg

for (i in 2:(dim(state_test)[1])) {
  if(is.na(state_test$PositiveRate[i]) | is.infinite(state_test$PositiveRate[i])){
    state_test$PositiveRate[i] = state_test$PositiveRate[i-1]
  }
}

state_test$PositiveRate[is.na(state_test$PositiveRate)] = state_test$PositiveRate[which(!is.na(state_test$PositiveRate))[1]]

# change negative rate to positive
state_test$PositiveRate = abs(state_test$PositiveRate)
# change positive rate larger tha 1 to 1
state_test$PositiveRate[state_test$PositiveRate>1] = 1 / state_test$PositiveRate[state_test$PositiveRate>1]

state_death = us_death %>%
  dplyr::filter(Province_State == Michigan)

state_confirmed = us_confirm %>%
  dplyr::filter(Province_State == Michigan)

#########################
# Part 1: Data Cleaning
#########################

# select the county-level death and confirmed cases
county_death_raw = state_death %>%
  dplyr::filter(Admin2 == county_name_selected)

county_confirm_raw = state_confirmed %>%
  dplyr::filter(Admin2 == county_name_selected)

# the real start date is defined as the date that the state confirmed cases is no less than 5 for the first time
start_date=all_dates[which(as.numeric(county_confirm_raw[12:length(county_confirm_raw)])>=5)[1]]

if (start_date < start_date_ini){
  start_date = start_date_ini
}

n = as.numeric(end_date - start_date) + 1

death_with_county_names = get_output_same_time_zone(
  data_type = "death",
  state_name = state_name,
  start_date = start_date,
  state_name_short = state_name_short,
  duration = n ,
  training_length = n,
  criterion_death = 2,
  smoothness = F
)

death_with_county_names_all = get_output_same_time_zone(
  data_type = "death",
  state_name = state_name,
  start_date = start_date,
  state_name_short = state_name_short,
  duration = n+prediction_length ,
  training_length = n,
  criterion_death = 2,
  smoothness = F
)

nation_population = us_daily_death_clean$Population


nation_daily_confirmed_selected = us_daily_confirm_clean[, c(1:11, 11+which(all_dates==date_for_map))]

# Ratio = daily confirmed cases / population
nation_daily_confirmed_rate_selected = nation_daily_confirmed_selected
nation_daily_confirmed_rate_selected[,12] = nation_daily_confirmed_selected[,12] / nation_population
nation_daily_confirmed_rate_selected[,12][nation_daily_confirmed_rate_selected[,12] == 0] = NA
colnames(nation_daily_confirmed_rate_selected)[12] = "Ratio"


nation_daily_confirmed_rate_selected_joint <- left_join(nation_daily_confirmed_rate_selected, counties, by = "county_fips")

range(nation_daily_confirmed_rate_selected$Ratio*100, na.rm=T)

nation_daily_confirmed_rate_selected_joint %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = (Ratio*100)), show.legend = T) +
  geom_polygon(
    data = urbnmapr::states, mapping = aes(x = long, y = lat, group = group),
    fill = NA, color = 'black', size = 1
  ) +
  scale_fill_gradient(low = "yellow", high = "red", na.value = "grey90", trans = "log10"
                      ###, limits = c(0.00038, 1.158)
  )+
  coord_map() +
  labs(fill = expression("Ratio (%)")) +
  ggtitle(paste0("daily confirmed cases / population in the U.S.", ", ", date_for_map))
dev.off() ##close it

# calculate the 7-day averaged daily confirm cases
us_daily_confirm_clean_avg = us_daily_confirm_clean
us_daily_confirm_clean_avg[, 13:dim(us_daily_confirm_clean)[2]] = t(apply(us_daily_confirm_clean[, 13:dim(us_daily_confirm_clean)[2]], 1, data_seven_day_smoothing))

# extract the averaged daily confirmed cases for all US counties on the selected date
nation_daily_confirmed_avg_selected = us_daily_confirm_clean_avg[, c(1:11, 11+which(all_dates==date_for_map))]

# Ratio = 7-day averaged daily confirmed cases / population
nation_daily_confirmed_rate_avg_selected = nation_daily_confirmed_avg_selected
nation_daily_confirmed_rate_avg_selected[,12] = nation_daily_confirmed_rate_avg_selected[,12] / nation_population
nation_daily_confirmed_rate_avg_selected[,12][nation_daily_confirmed_rate_avg_selected[,12] == 0] = NA
colnames(nation_daily_confirmed_rate_avg_selected)[12] = "Ratio"

# joint the averaged daily confirmed cases with the shape file for all US counties
nation_daily_confirmed_rate_avg_selected_joint <- left_join(nation_daily_confirmed_rate_avg_selected, counties, by = "county_fips")

range(nation_daily_confirmed_rate_avg_selected$Ratio*100, na.rm=T)

nation_daily_confirmed_rate_avg_selected_joint %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = (Ratio*100)), show.legend = T) +
  geom_polygon(
    data = urbnmapr::states, mapping = aes(x = long, y = lat, group = group),
    fill = NA, color = 'black', size = 1
  ) +
  scale_fill_gradient(low = "yellow", high = "red", na.value = "grey90", trans = "log10"
                      ###, limits = c(0.00038, 1.158)
  )+
  coord_map() +
  labs(fill = expression("Ratio (%)")) +
  ggtitle(paste0("7-day averaged daily confirmed cases / population in the U.S.", ", ", date_for_map))
dev.off() ##close it

#3)
state_name = "Florida"
state_name_short = "FL"

county_death = us_death%>%
  filter(Admin2 == county_name, Province_State == Florida) %>%
  select(starts_with(FL))
county_confirmed = us_confirm %>%
  filter(Admin2 == county_name, Province_State == Florida) %>%
  select(starts_with(FL))

county_death_sum = apply(county_death, 2, sum)

county_confirmed_sum = apply(county_confirmed, 2, sum)


county_death_selected = county_death_sum[which(all_dates %in% seq.Date(start_date, end_date, by=1))]
county_confirmed_selected = county_confirmed_sum[which(all_dates %in% seq.Date(start_date, end_date, by=1))]

county_death_selected=as.numeric(county_death_selected)
county_confirmed_selected=as.numeric(county_confirmed_selected)

county_confirmed_selected_daily_avg = data_seven_day_smoothing(county_confirmed_selected_daily)
daily_confirmed_county_avg_df = data.frame(date = daily_date_selected, value = county_confirmed_selected_daily_avg)

daily_confirmed_county_avg_df %>%
  ggplot(aes(x=date, y=value)) +
  geom_bar(stat = 'identity', color="white",  fill="#ff8540", width = 1) +
  ylab("Daily confirmed cases in Florida")+
  xlab("Date")+ 
  theme(text = element_text(size = 20),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.key.width=unit(1,"cm"),
        axis.text.y = element_text(angle=90, hjust=1))

state_name = "California"
state_name_short = "CA"
county_name = "Santa Barbara"
##get the death and confirmed cases
county_death = us_death%>%
  filter(Admin2 == SantaBarbara, Province_State == California) %>%
  select(starts_with("x"))
county_confirmed = us_confirm %>%
  filter(Admin2 == SantaBarbara, Province_State == California) %>%
  select(starts_with("x"))

daily_confirmed_county_avg_df %>%
  ggplot(aes(x=date, y=value)) +
  geom_bar(stat = 'identity', color="white",  fill="#ff8540", width = 1) +
  ylab("Daily confirmed cases in Cali SB")+
  xlab("Date")+ 
  theme(text = element_text(size = 20),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.key.width=unit(1,"cm"),
        axis.text.y = element_text(angle=90, hjust=1))
dev.off() ##close it
