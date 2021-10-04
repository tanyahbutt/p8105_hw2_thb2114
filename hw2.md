P8105 Homework 2 - Tanya Butt (thb2114)
================

## Problem 1

I will import the “Mr. Trash Wheel” dataset, clean the variable names,
remove non-specific dumpster data, and round the number of sports balls
to the nearest integer.

``` r
mr_trash_wheel_df = read_excel("Trash-Wheel-Collection-Totals-8-6-19.xlsx", 
                               range = "A2:N408")
mr_trash_wheel_df = janitor::clean_names(mr_trash_wheel_df) 
mr_trash_wheel_df = drop_na(mr_trash_wheel_df, dumpster) %>% 
  mutate(sports_balls = round(sports_balls, digits = 0))
```

I will now clean and combine the precipitation data for 2019 and 2018.
Rows without precipitation data were initially omitted, a year variable
was added, and the month variable was converted to a character variable.

``` r
precipitation_2019_df = read_excel("Trash-Wheel-Collection-Totals-8-6-19.xlsx", sheet = 4,
                               range = "A2:B14") 
precipitation_2019_df = drop_na(precipitation_2019_df, Total) %>% 
                        add_column(Year = "2019")

precipitation_2018_df = read_excel("Trash-Wheel-Collection-Totals-8-6-19.xlsx", sheet = 5,
                               range = "A2:B14") 
precipitation_2018_df = drop_na(precipitation_2018_df, Total) %>% 
                        add_column(Year = "2018")

precipitation_2018_2019_df = bind_rows(precipitation_2018_df, precipitation_2019_df) %>% 
  janitor::clean_names() %>% 
  pivot_wider(
    names_from = "year",
    values_from = "total") %>% 
  mutate(month = month.name[as.numeric(month)])

nrow(precipitation_2018_df)
## [1] 12
nrow(precipitation_2019_df)
## [1] 6
```

The overall “Mr. Trash Wheel” dataset includes the following variables:
dumpster, month, year, date, weight\_tons, volume\_cubic\_yards,
plastic\_bottles, polystyrene, cigarette\_butts, glass\_bottles,
grocery\_bags, chip\_bags, sports\_balls, homes\_powered. It has 14
columns and 344 observations.

Monthly and yearly precipitation amounts are also recorded in sheets in
the “Mr. Trash Wheel” dataset. The variables included in these sheets
are: Month, Total, Year.

In 2018, there were 12 months of precipitation. In 2019, only 6 months
of precipitation were recorded.

``` r
total_precip_2018 = sum(pull(precipitation_2018_2019_df, `2018`))
```

For 2018, the total amount of precipitation was 70.33 in.

``` r
mr_trash_wheel_df = select(mr_trash_wheel_df,year, sports_balls) %>% 
  filter(year == "2019")

median_sports_balls_2019 = median(pull(mr_trash_wheel_df, sports_balls))
median_sports_balls_2019 = round(median_sports_balls_2019, digits = 0)
```

The median number of sports balls in a dumpster in 2019 was 8.
