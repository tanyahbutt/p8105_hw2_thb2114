P8105 Homework 2 - Tanya Butt (thb2114)
================

## Problem 1

I will import the “Mr. Trash Wheel” dataset, clean the variable names,
remove non-specific dumpster data, and round the number of sports balls
to the nearest integer.

``` r
mr_trash_wheel_df = read_excel("Trash-Wheel-Collection-Totals-new.xlsx", 
                               range = "A2:N535")
mr_trash_wheel_df = janitor::clean_names(mr_trash_wheel_df) 
mr_trash_wheel_df = drop_na(mr_trash_wheel_df, dumpster) %>% 
  mutate(sports_balls = round(sports_balls, digits = 0))
```

I will now clean and combine the precipitation data for 2019 and 2018.
Rows without precipitation data were initially omitted, a year variable
was added, and the month variable was converted to a character variable.

``` r
precipitation_2019_df = read_excel("Trash-Wheel-Collection-Totals-new.xlsx",
                                   sheet = 6,range = "A2:B14") 
precipitation_2019_df = drop_na(precipitation_2019_df, Total) %>% 
                        add_column(Year = "2019")

precipitation_2018_df = read_excel("Trash-Wheel-Collection-Totals-new.xlsx",
                                   sheet = 7, range = "A2:B14") 
precipitation_2018_df = drop_na(precipitation_2018_df, Total) %>% 
                        add_column(Year = "2018")

precipitation_2018_2019_df = bind_rows(precipitation_2018_df, 
                                       precipitation_2019_df) %>% 
  janitor::clean_names() %>% 
  pivot_wider(
    names_from = "year",
    values_from = "total") %>% 
  mutate(month = month.name[as.numeric(month)])

nrow(precipitation_2018_df)
## [1] 12
nrow(precipitation_2019_df)
## [1] 12
```

The overall “Mr. Trash Wheel” dataset includes the following variables:
dumpster, month, year, date, weight\_tons, volume\_cubic\_yards,
plastic\_bottles, polystyrene, cigarette\_butts, glass\_bottles,
grocery\_bags, chip\_bags, sports\_balls, homes\_powered. It has 14
columns and 454 observations.

Monthly and yearly precipitation amounts are also recorded in sheets in
the “Mr. Trash Wheel” dataset. The variables included in these sheets
are: Month, Total, Year.

In 2018, there were 12 months of precipitation. In 2019, 12 months of
precipitation were also recorded.

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

The median number of sports balls in a dumpster in 2019 was 9.

## Problem 2

I will merge 3 datasets from FiveThirtyEight into a single dataset using
year and month.

``` r
pols_month_df = read_csv("pols-month.csv") %>% 
  janitor::clean_names() %>% 
  separate(mon, c("year", "month", "day")) %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(month = month.abb[as.numeric(month)], 
         month = str_to_lower(month)) %>% 
  mutate(day = as.numeric(day)) %>% 
  mutate(prez_gop = recode(prez_gop, `0` = "dem", 
                           `1` = "gop", `2` = "gop")) %>% 
  select(year, month, prez_gop, gov_gop, sen_gop, rep_gop, gov_dem, sen_dem, 
         rep_dem) %>% 
  rename(president = prez_gop)

snp_df = read_csv("snp.csv") %>% 
  janitor::clean_names() %>% 
  mutate(date = mdy(date)) %>% 
  separate(date, c("year", "month", "day")) %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(year = ifelse(year > 2021, year - 100, year)) %>% 
  mutate(month = as.numeric(month)) %>% 
  mutate(month = month.abb[as.numeric(month)], month = str_to_lower(month)) %>% 
  relocate(year, month, day, close) %>%
  rename(s_p_closing_value = close) %>% 
  select(year, month, s_p_closing_value)

unemployment_df = read_csv("unemployment.csv") %>% 
  janitor::clean_names() %>% 
  pivot_longer(
  jan:dec,
   names_to = "month",
   values_to = "unemployment_percentage")

snp_pols_df = left_join(pols_month_df, snp_df, by = c("year" = "year",
                                                      "month" = "month"))
snp_pols_unemp_df = left_join(snp_pols_df, unemployment_df, by = c("year" = "year", "month" = "month"))
```

The dataset “pols month” contains 822 observations for 9 variables
related to the number of politicians who are democratic or republican
from 1947 to 2015. The variable names in this dataset include: year,
month, president, gov\_gop, sen\_gop, rep\_gop, gov\_dem, sen\_dem,
rep\_dem.

The dataset “snp” contains 787 observations for 3 variables related to
the Standard and Poor’s (S&P) stock market index. The years included
range from 1950 to 2015. The variable names in this dataset include:
year, month, s\_p\_closing\_value.

The dataset “unemployment” contains 816 observations for 3 variables
related to the monthly percentage of unemployment from 1950 to 2015. The
variable names in this dataset include: year, month,
unemployment\_percentage.

The combined version of these datasets include the following variables:
year, month, president, gov\_gop, sen\_gop, rep\_gop, gov\_dem,
sen\_dem, rep\_dem, s\_p\_closing\_value, unemployment\_percentage

## Problem 3

I will now load the popular baby names dataset from NYC Open data. I
will rename the gender predictor, recode the ethnicity variable, and
remove duplicate rows.

``` r
babynames_df = read.csv("Popular_Baby_Names.csv") %>% 
  janitor::clean_names() %>% 
  rename(sex = gender) %>% 
  mutate(ethnicity = recode(ethnicity, "ASIAN AND PACI" = "ASIAN AND PACIFIC ISLANDER",
                            "BLACK NON HISP" = "BLACK NON HISPANIC", 
                            "WHITE NON HISP" = "WHITE NON HISPANIC")) %>% 
  mutate(child_s_first_name = toupper(child_s_first_name)) %>% 
  distinct(year_of_birth, sex, ethnicity, child_s_first_name, count, rank)
```

I will now create a table showing the rank in popularity and name count
of the female baby name “Olivia” over time.

``` r
olivia_df = filter(babynames_df, child_s_first_name == "OLIVIA") %>% 
  pivot_wider(names_from = year_of_birth, values_from = c(rank,count),
              values_fill = 0) %>% 
  relocate(child_s_first_name, sex, ethnicity, rank_2011, count_2011, rank_2012, 
           count_2012, rank_2013, count_2013, rank_2014, count_2014,
           rank_2015, count_2015, rank_2016, count_2016) %>% 
  rename("Child's Name" = child_s_first_name) %>% 
  rename(Sex = sex) %>% 
  rename("Race/Ethnicity" = ethnicity) %>% 
  rename("Rank in 2011" = rank_2011) %>% 
  rename("Count in 2011" = count_2011) %>% 
  rename("Rank in 2012" = rank_2012) %>% 
  rename("Count in 2012" = count_2012) %>%
  rename("Rank in 2013" = rank_2013) %>% 
  rename("Count in 2013" = count_2013) %>% 
  rename("Rank in 2014" = rank_2014) %>% 
  rename("Count in 2014" = count_2014) %>%   
  rename("Rank in 2015" = rank_2015) %>% 
  rename("Count in 2015" = count_2015) %>% 
  rename("Rank in 2016" = rank_2016) %>% 
  rename("Count in 2016" = count_2016) %>% 
  knitr::kable()

olivia_df
```

| Child’s Name | Sex    | Race/Ethnicity             | Rank in 2011 | Count in 2011 | Rank in 2012 | Count in 2012 | Rank in 2013 | Count in 2013 | Rank in 2014 | Count in 2014 | Rank in 2015 | Count in 2015 | Rank in 2016 | Count in 2016 |
|:-------------|:-------|:---------------------------|-------------:|--------------:|-------------:|--------------:|-------------:|--------------:|-------------:|--------------:|-------------:|--------------:|-------------:|--------------:|
| OLIVIA       | FEMALE | ASIAN AND PACIFIC ISLANDER |            4 |            89 |            3 |           132 |            3 |           109 |            1 |           141 |            1 |           188 |            1 |           172 |
| OLIVIA       | FEMALE | BLACK NON HISPANIC         |           10 |            52 |            8 |            58 |            6 |            64 |            8 |            52 |            4 |            82 |            8 |            49 |
| OLIVIA       | FEMALE | HISPANIC                   |           18 |            86 |           22 |            77 |           22 |            87 |           16 |            96 |           16 |            94 |           13 |           108 |
| OLIVIA       | FEMALE | WHITE NON HISPANIC         |            2 |           213 |            4 |           198 |            1 |           233 |            1 |           248 |            1 |           225 |            1 |           230 |

I will now produce a table ranking the most popular name among male
children over time.

``` r
#Identify male names with rank = 1
popular_male_names_df = select(babynames_df, year_of_birth, sex, ethnicity,
                                child_s_first_name, rank) %>% 
  filter(sex == "MALE", rank == '1') 

popular_male_names_df = select(babynames_df, year_of_birth, sex, ethnicity, child_s_first_name, rank) %>% 
  filter(child_s_first_name %in% c("ETHAN", "NOAH","LIAM","JOSEPH","JAYDEN","DAVID", "RYAN","MICHAEL")) %>% 
pivot_wider(names_from = year_of_birth, values_from = rank) %>% 
  relocate(child_s_first_name, ethnicity, '2016', '2015', '2014', '2013', '2012', '2011') %>% 
  select(child_s_first_name, ethnicity, '2016', '2015', '2014', '2013', '2012', '2011') %>% 
  rename("Race/Ethnicity" = ethnicity) %>% 
  rename("Rank in 2016" = '2016') %>% 
  rename("Rank in 2015" = '2015') %>% 
  rename("Rank in 2014" = '2014') %>% 
  rename("Rank in 2013" = '2013') %>% 
  rename("Rank in 2012" = '2012') %>% 
  rename("Rank in 2011" = '2011') %>% 
  arrange(child_s_first_name) %>% 
  rename("Child's Name" = child_s_first_name) %>% 
  knitr::kable()

popular_male_names_df
```

| Child’s Name | Race/Ethnicity             | Rank in 2016 | Rank in 2015 | Rank in 2014 | Rank in 2013 | Rank in 2012 | Rank in 2011 |
|:-------------|:---------------------------|-------------:|-------------:|-------------:|-------------:|-------------:|-------------:|
| DAVID        | ASIAN AND PACIFIC ISLANDER |           43 |           31 |           38 |           27 |           33 |           38 |
| DAVID        | BLACK NON HISPANIC         |           13 |           19 |           28 |           26 |           16 |           15 |
| DAVID        | HISPANIC                   |           21 |           16 |           14 |           12 |           18 |           15 |
| DAVID        | WHITE NON HISPANIC         |            3 |            1 |            2 |            1 |            2 |            4 |
| ETHAN        | ASIAN AND PACIFIC ISLANDER |            1 |            2 |            2 |            2 |            2 |            1 |
| ETHAN        | BLACK NON HISPANIC         |            5 |            5 |            1 |            1 |            3 |            6 |
| ETHAN        | HISPANIC                   |            7 |            3 |            5 |            5 |            4 |            6 |
| ETHAN        | WHITE NON HISPANIC         |           20 |           19 |           18 |           23 |           21 |           26 |
| JAYDEN       | ASIAN AND PACIFIC ISLANDER |            5 |            1 |            1 |            1 |            2 |            2 |
| JAYDEN       | BLACK NON HISPANIC         |           11 |           10 |            3 |            2 |            1 |            1 |
| JAYDEN       | HISPANIC                   |            8 |            7 |            4 |            1 |            1 |            1 |
| JAYDEN       | WHITE NON HISPANIC         |           91 |           78 |           77 |           78 |           74 |           68 |
| JOSEPH       | ASIAN AND PACIFIC ISLANDER |           40 |           43 |           50 |           44 |           42 |           45 |
| JOSEPH       | BLACK NON HISPANIC         |           42 |           37 |           45 |           35 |           24 |           39 |
| JOSEPH       | HISPANIC                   |           26 |           20 |           25 |           19 |           23 |           22 |
| JOSEPH       | WHITE NON HISPANIC         |            1 |            2 |            1 |            2 |            1 |            2 |
| LIAM         | ASIAN AND PACIFIC ISLANDER |            9 |            9 |           16 |           10 |           29 |           41 |
| LIAM         | BLACK NON HISPANIC         |            4 |            2 |            6 |           10 |           11 |           24 |
| LIAM         | HISPANIC                   |            1 |            1 |            1 |           11 |           24 |           32 |
| LIAM         | WHITE NON HISPANIC         |           22 |           20 |           19 |           21 |           20 |           20 |
| MICHAEL      | ASIAN AND PACIFIC ISLANDER |           30 |           30 |           27 |           34 |           36 |           29 |
| MICHAEL      | BLACK NON HISPANIC         |           12 |           12 |           12 |           12 |            6 |           10 |
| MICHAEL      | HISPANIC                   |           22 |           18 |           13 |           16 |           14 |           16 |
| MICHAEL      | WHITE NON HISPANIC         |            2 |            6 |            3 |            3 |            3 |            1 |
| NOAH         | ASIAN AND PACIFIC ISLANDER |           18 |           17 |           29 |           20 |           25 |           47 |
| NOAH         | BLACK NON HISPANIC         |            1 |            1 |            2 |            3 |            9 |           11 |
| NOAH         | HISPANIC                   |            5 |            5 |            3 |            9 |           20 |           19 |
| NOAH         | WHITE NON HISPANIC         |           14 |           20 |           21 |           14 |           21 |           31 |
| RYAN         | WHITE NON HISPANIC         |           81 |           80 |           NA |           NA |           80 |           77 |
| RYAN         | ASIAN AND PACIFIC ISLANDER |            2 |            3 |            3 |            3 |            1 |            3 |
| RYAN         | BLACK NON HISPANIC         |           26 |           33 |           28 |           23 |           13 |           33 |
| RYAN         | HISPANIC                   |           27 |           33 |           32 |           26 |           28 |           26 |
| RYAN         | WHITE NON HISPANIC         |           21 |           25 |           20 |           22 |           14 |           24 |

I will now create a scatter plot showing the number of children with a
name vs. the rank in popularity of that name, for white non-hispanic
male children born in 2016.

``` r
scatter_plot_df = select(babynames_df, year_of_birth, sex, ethnicity, child_s_first_name, rank, count) %>% 
                         filter(year_of_birth == '2016', sex == "MALE", ethnicity == "WHITE NON HISPANIC") 

ggplot(scatter_plot_df, aes(x = rank, y = count)) + 
  geom_point() +
  xlab("Rank in Popularity of that Name") + ylab("Number of Children with Name")
```

![](hw2_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->
