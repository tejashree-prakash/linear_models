Linear Model Lecture
================

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.2     ✔ tibble    3.3.0
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.1.0     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(p8105.datasets)

set.seed(1)
```

Load NYC airbnb data.

``` r
data(nyc_airbnb)
```

Look at the data / do some cleaning.

``` r
nyc_airbnb <-  
  nyc_airbnb %>%
  mutate( # re label the predictor 
    stars = review_scores_location / 2
  ) %>%
  rename(
    borough = neighbourhood_group
  ) %>%
  filter(borough != "Staten Island") %>%
  select(price, stars, borough, room_type, neighbourhood)
```

Do regression!!!

``` r
fit = lm(price ~ stars + borough, data = nyc_airbnb) #R constructed borough from categorical names into a factor, by default it will pick by alphabetical order so bronx becomes the reference 
fit
```

    ## 
    ## Call:
    ## lm(formula = price ~ stars + borough, data = nyc_airbnb)
    ## 
    ## Coefficients:
    ##      (Intercept)             stars   boroughBrooklyn  boroughManhattan  
    ##           -70.41             31.99             40.50             90.25  
    ##    boroughQueens  
    ##            13.21

Do some additional cleaning then refit. Set the reference differently,
and it changes the model output!

``` r
#to check the counts as it informs fctinfreq function 
nyc_airbnb %>%
  count(borough)
```

    ## # A tibble: 4 × 2
    ##   borough       n
    ##   <chr>     <int>
    ## 1 Bronx       649
    ## 2 Brooklyn  16810
    ## 3 Manhattan 19212
    ## 4 Queens     3821

``` r
nyc_airbnb <- 
  nyc_airbnb %>%
  mutate(
    borough = fct_infreq(borough), #putting the categories in order of how commonly they appear in the dataset - in this case the reference is Manhattan as it has the most number of counts (refer to count(borough) before)
    room_type = fct_infreq(room_type)
  )

refit = lm(price ~ stars + borough, data = nyc_airbnb)
refit
```

    ## 
    ## Call:
    ## lm(formula = price ~ stars + borough, data = nyc_airbnb)
    ## 
    ## Coefficients:
    ##     (Intercept)            stars  boroughBrooklyn    boroughQueens  
    ##           19.84            31.99           -49.75           -77.05  
    ##    boroughBronx  
    ##          -90.25

Look at ‘lm’ stuff. It looks like a skewed distribution!

``` r
summary(refit)
names(summary(refit))
summary(refit[["coefficients"]])
summary(refit)[["df"]]

fitted.values(refit)
```

Look at the cleaner `lm` stuff! Pull out the things we care about:
coefficients, estimates, std. errors, p-values, etc.

``` r
refit %>%
  broom::tidy() %>%
  mutate( 
    term = str_replace(term, "borough", "Borough: ")
  ) %>%
  select(term, estimate, p.value) %>%
  knitr::kable(digits = 3)
```

| term              | estimate | p.value |
|:------------------|---------:|--------:|
| (Intercept)       |   19.839 |   0.104 |
| stars             |   31.990 |   0.000 |
| Borough: Brooklyn |  -49.754 |   0.000 |
| Borough: Queens   |  -77.048 |   0.000 |
| Borough: Bronx    |  -90.254 |   0.000 |

``` r
#this will show a bit more things like r squared and adjusted r squared 
refit %>%
  broom::glance()
```

    ## # A tibble: 1 × 12
    ##   r.squared adj.r.squared sigma statistic   p.value    df   logLik    AIC    BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>     <dbl> <dbl>    <dbl>  <dbl>  <dbl>
    ## 1    0.0342        0.0341  182.      271. 6.73e-229     4 -202113. 4.04e5 4.04e5
    ## # ℹ 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>
