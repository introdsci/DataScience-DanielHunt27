## ----message=FALSE, error=FALSE, warning=FALSE, results='hide'----------------
include <- function(library_name){
  if( !(library_name %in% installed.packages()) )
    install.packages(library_name) 
  library(library_name, character.only=TRUE)
}
include("tidyverse")
include("knitr")
#purl("deliverable1.Rmd", output = "part1.r") # produces r source from rmd
#source("part1.r") # executes the source
include("devtools")
include("rjson")
include("caret")
install_github("mikeasilva/blsAPI")


## -----------------------------------------------------------------------------
creat_tib_from_json <- function(json, num) {
  temp <- tibble('year' = character(), 'period'= character(), 'periodName'= character(), 'latest'= character(), 'value'= character())
  for (i in json$Results$series[[num]][[2]]) {
    if("latest" %in% names(i)) {
      temp <- bind_rows(temp, tibble('year' = i$year, 'period'= i$period, 'periodName'= i$periodName, 'latest'= i$latest, 'value'= i$value))
    }
    else {
      temp <- bind_rows(temp, tibble('year' = i$year, 'period'= i$period, 'periodName'= i$periodName, 'value'= i$value))
    }
  }
  temp$year <- as_factor(temp$year)
  temp$period <- as_factor(temp$period)
  temp$periodName <- as_factor(temp$periodName)
  temp$latest <- as.logical(temp$latest)
  temp$value <- as.integer(temp$value)
  return(temp)
}


## ----message=FALSE, error=FALSE, warning=FALSE, results='hide'----------------
library("blsAPI")
payload <- list('seriesid'=c('CES0500000001', 'CES0500000002', 'CES0500000003'), 'startyear'='2009', 'endyear'='2019')
response <- blsAPI(payload)
json <- fromJSON(response)

private_employee_total <- creat_tib_from_json(json, 1)
avg_weekly_hrs_worked <- creat_tib_from_json(json, 2)
avg_hourly_earnings <- creat_tib_from_json(json, 3)


## -----------------------------------------------------------------------------
private_employee_total


## -----------------------------------------------------------------------------
avg_weekly_hrs_worked


## -----------------------------------------------------------------------------
avg_hourly_earnings


## -----------------------------------------------------------------------------
productivity_model <- lm(data=where_2012_is_100, formula=Labor_productivity~Employment+Average_weekly_hours_worked+Compensation)
summary(productivity_model)


## -----------------------------------------------------------------------------
ggplot(data=where_2012_is_100, aes(x=Employment,y=Labor_productivity)) + geom_point() + geom_smooth(method="lm") + labs(x="Employment", y="Labor Productivity")


## -----------------------------------------------------------------------------
ggplot(data=where_2012_is_100, aes(x=Average_weekly_hours_worked,y=Labor_productivity)) + geom_point() + geom_smooth(method="lm") + labs(x="Average Weekly Hours Worked", y="Labor Productivity")


## -----------------------------------------------------------------------------
ggplot(data=where_2012_is_100, aes(x=Compensation,y=Labor_productivity)) + geom_point() + geom_smooth(method="lm") + labs(x="Compensation", y="Labor Productivity")


## -----------------------------------------------------------------------------
sampling = createDataPartition(where_2012_is_100$Labor_productivity, p = 0.75, list = FALSE)
train = where_2012_is_100[sampling, ]
test = where_2012_is_100[-sampling, ]
productivity_model2 <- lm(data=train, formula=Labor_productivity~Employment+Average_weekly_hours_worked+Compensation)
productivity_prediction <- predict(productivity_model2, test)
ggplot(data=test, aes(x=productivity_prediction,y=test$Labor_productivity)) + geom_point() + labs(x="Productivity Prediction", y="Labor Productivity")


## -----------------------------------------------------------------------------
R2(productivity_prediction, test$Labor_productivity)


## -----------------------------------------------------------------------------
MAE(productivity_prediction, test$Labor_productivity)


## -----------------------------------------------------------------------------
RMSE(productivity_prediction, test$Labor_productivity)

