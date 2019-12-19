## -----------------------------------------------------------------------------
suppressMessages(library("tidyverse"))


## -----------------------------------------------------------------------------
data <- read.csv("https://www.bls.gov/lpc/major_sector_lpc_data.csv")


## -----------------------------------------------------------------------------
head(data)
nrow(data)
ncol(data)


## -----------------------------------------------------------------------------
names(data) <- gsub(x = names(data), pattern = "\\.\\.\\.", replacement = "\\.")
names(data) <- gsub(x = names(data), pattern = "\\.\\.", replacement = "\\.")


## -----------------------------------------------------------------------------
colnames(data)


## -----------------------------------------------------------------------------
from_year_ago <- tibble(major_sectors=data$Major.Sectors, year=data$Year, quarter=data$Quarter,
                        Employment=data$Employment.percent.change.from.same.quarter.a.year.ago.,
                        Average_weekly_hours_worked=data$Average.weekly.hours.worked.percent.change.from.same.quarter.a.year.ago.,
                        Hours_worked=data$Hours.worked.percent.change.from.same.quarter.a.year.ago.,
                        Output=data$Output.percent.change.from.same.quarter.a.year.ago.,
                        Current_dollar_output=data$Current.dollar.output.percent.change.from.same.quarter.a.year.ago.,
                        Compensation=data$Compensation.percent.change.from.same.quarter.a.year.ago.,
                        Nonlabor_payments=data$Nonlabor.payments.percent.change.from.same.quarter.a.year.ago.,
                        Labor_productivity=data$Labor.productivity.percent.change.from.same.quarter.a.year.ago.,
                        Hourly_compensation=data$Hourly.compensation.percent.change.from.same.quarter.a.year.ago.,
                        Unit_labor_costs=data$Unit.labor.costs.percent.change.from.same.quarter.a.year.ago.,
                        Unit_nonlabor_costs=data$Unit.nonlabor.costs.percent.change.from.same.quarter.a.year.ago.,
                        Unit_nonlabor_payments=data$Unit.nonlabor.payments.percent.change.from.same.quarter.a.year.ago.,
                        Implicit_price_deflator=data$Implicit.price.deflator.percent.change.from.same.quarter.a.year.ago.,
                        Real_hourly_compensation=data$Real.hourly.compensation.percent.change.from.same.quarter.a.year.ago.,
                        Output_per_job=data$Output.per.job.percent.change.from.same.quarter.a.year.ago.,
                        Labor_share=data$Labor.share.percent.change.from.same.quarter.a.year.ago.,
                        Profits=data$Profits.percent.change.from.same.quarter.a.year.ago.,
                        Unit_profits=data$Unit.profits.percent.change.from.same.quarter.a.year.ago.,
                        Total_unit_costs=data$Total.unit.costs.percent.change.from.same.quarter.a.year.ago.)


## -----------------------------------------------------------------------------
from_previous_quarter <- tibble(major_sectors=data$Major.Sectors, year=data$Year, quarter=data$Quarter,
                        Employment=data$Employment.percent.change.from.previous.quarter.at.annual.rate.,
                        Average_weekly_hours_worked=data$Average.weekly.hours.worked.percent.change.from.previous.quarter.at.annual.rate.,
                        Hours_worked=data$Hours.worked.percent.change.from.previous.quarter.at.annual.rate.,
                        Output=data$Output.percent.change.from.previous.quarter.at.annual.rate.,
                        Current_dollar_output=data$Current.dollar.output.percent.change.from.previous.quarter.at.annual.rate.,
                        Compensation=data$Compensation.percent.change.from.previous.quarter.at.annual.rate.,
                        Nonlabor_payments=data$Nonlabor.payments.percent.change.from.previous.quarter.at.annual.rate.,
                        Labor_productivity=data$Labor.productivity.percent.change.from.previous.quarter.at.annual.rate.,
                        Hourly_compensation=data$Hourly.compensation.percent.change.from.previous.quarter.at.annual.rate.,
                        Unit_labor_costs=data$Unit.labor.costs.percent.change.from.previous.quarter.at.annual.rate.,
                        Unit_nonlabor_costs=data$Unit.nonlabor.costs.percent.change.from.previous.quarter.at.annual.rate.,
                        Unit_nonlabor_payments=data$Unit.nonlabor.payments.percent.change.from.previous.quarter.at.annual.rate.,
                        Implicit_price_deflator=data$Implicit.price.deflator.percent.change.from.previous.quarter.at.annual.rate.,
                        Real_hourly_compensation=data$Real.hourly.compensation.percent.change.from.previous.quarter.at.annual.rate.,
                        Output_per_job=data$Output.per.job.percent.change.from.previous.quarter.at.annual.rate.,
                        Labor_share=data$Labor.share.percent.change.from.previous.quarter.at.annual.rate.,
                        Profits=data$Profits.percent.change.from.previous.quarter.at.annual.rate.,
                        Unit_profits=data$Unit.profits.percent.change.from.previous.quarter.at.annual.rate.,
                        Total_unit_costs=data$Total.unit.costs.percent.change.from.previous.quarter.at.annual.rate.)


## -----------------------------------------------------------------------------
where_2012_is_100 <- tibble(major_sectors=data$Major.Sectors, year=data$Year, quarter=data$Quarter,
                        Employment=data$Employment.2012.100.,
                        Average_weekly_hours_worked=data$Average.weekly.hours.worked.2012.100.,
                        Hours_worked=data$Hours.worked.2012.100.,
                        Output=data$Output.2012.100.,
                        Current_dollar_output=data$Current.dollar.output.2012.100.,
                        Compensation=data$Compensation.2012.100.,
                        Nonlabor_payments=data$Nonlabor.payments.2012.100.,
                        Labor_productivity=data$Labor.productivity.2012.100.,
                        Hourly_compensation=data$Hourly.compensation.2012.100.,
                        Unit_labor_costs=data$Unit.labor.costs.2012.100.,
                        Unit_nonlabor_costs=data$Unit.nonlabor.costs.2012.100.,
                        Unit_nonlabor_payments=data$Unit.nonlabor.payments.2012.100.,
                        Implicit_price_deflator=data$Implicit.price.deflator.2012.100.,
                        Real_hourly_compensation=data$Real.hourly.compensation.2012.100.,
                        Output_per_job=data$Output.per.job.2012.100.,
                        Labor_share=data$Labor.share.2012.100.,
                        Profits=data$Profits.2012.100.,
                        Unit_profits=data$Unit.profits.2012.100.,
                        Total_unit_costs=data$Total.unit.costs.2012.100.)


## -----------------------------------------------------------------------------
summary(from_year_ago)


## -----------------------------------------------------------------------------
summary(from_previous_quarter)


## -----------------------------------------------------------------------------
summary(where_2012_is_100)


## -----------------------------------------------------------------------------
ggplot(from_previous_quarter, aes(x=year, y=Employment, group=major_sectors)) + geom_line(aes(color=major_sectors)) + labs(title="Percent Change in Employment from the Same Quarter a Year Ago", x="Year", y="Percent Change in Employment", color = "Major Sectors")


## -----------------------------------------------------------------------------
ggplot(from_year_ago, aes(x=year, y=Hourly_compensation, group=major_sectors)) + geom_line(aes(color=major_sectors)) + labs(title="Percent Change in Hourly Compensation from the Previous Quarter at an Annual Rate", x="Year", y="Percent Change in Hourly Compensation", color = "Major Sectors")+ theme(plot.title = element_text(size=12))


## -----------------------------------------------------------------------------
ggplot(where_2012_is_100, aes(x=year, y=Labor_productivity, group=major_sectors)) + geom_smooth(aes(color=major_sectors)) + labs(title="Labor Productivity", x="Year", y="Labor Productivity", color = "Major Sectors")

