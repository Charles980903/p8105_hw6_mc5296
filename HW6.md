HW6
================
2022-11-21

# problem 1

    ## Registered S3 method overwritten by 'hoardr':
    ##   method           from
    ##   print.cache_info httr

    ## using cached file: C:\Users\Lenovo\AppData\Local/Cache/R/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2022-09-29 10:32:41 (8.418)

    ## file min/max dates: 1869-01-01 / 2022-09-30

## r²

``` r
result <- weather_df %>% 
  modelr::bootstrap(n=5000) %>%
  mutate(
    models = map(strap, ~lm(tmax ~ tmin, data = .x) ),
    r_results = map(models, broom::glance),
    beta_results = map(models, broom::tidy))
```

## plot of r²

``` r
result %>%
  select(r_results) %>% 
  unnest()%>%
  ggplot(aes(x=r.squared)) + geom_density()
```

    ## Warning: `cols` is now required when using unnest().
    ## Please use `cols = c(r_results)`

![](HW6_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
result %>%
  select(r_results) %>% 
  unnest()%>%
  summarize(
    mean = mean(r.squared),
    se = sd(r.squared),
    "2.5-quantile" = mean(r.squared)+qnorm(0.025)*sd(r.squared),
    "97.5-quantile" = mean(r.squared)+qnorm(0.775)*sd(r.squared)
  )
```

    ## Warning: `cols` is now required when using unnest().
    ## Please use `cols = c(r_results)`

    ## # A tibble: 1 × 4
    ##    mean      se `2.5-quantile` `97.5-quantile`
    ##   <dbl>   <dbl>          <dbl>           <dbl>
    ## 1 0.911 0.00858          0.895           0.918

R square is likely to be symmetrically distributed with mean 0.91, the
2.5 quantile and 97.5 quantile is 0.894 and 0.918

``` r
beta_df <- result %>% 
  select(beta_results) %>% 
  unnest(beta_results) %>% 
  select(term, estimate) %>% 
  pivot_wider(
    names_from = term,
    values_from = estimate
  ) %>% 
  janitor::clean_names() %>% 
  unnest() %>% 
  mutate(log_value = log(intercept*tmin)) 
```

    ## Warning: Values from `estimate` are not uniquely identified; output will contain list-cols.
    ## * Use `values_fn = list` to suppress this warning.
    ## * Use `values_fn = {summary_fun}` to summarise duplicates.
    ## * Use the following dplyr code to identify duplicates.
    ##   {data} %>%
    ##     dplyr::group_by(term) %>%
    ##     dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    ##     dplyr::filter(n > 1L)

    ## Warning: `cols` is now required when using unnest().
    ## Please use `cols = c(intercept, tmin)`

``` r
beta_df %>%
   ggplot(aes(x = log_value)) + geom_density()
```

![](HW6_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
beta_df %>% 
  summarize(
    mean= mean(log_value),
    se = sd(log_value),
    "2.5-quantile" = mean(log_value)+qnorm(0.025)*sd(log_value),
    "97.5-quantile" = mean(log_value)+qnorm(0.775)*sd(log_value)
  )
```

    ## # A tibble: 1 × 4
    ##    mean     se `2.5-quantile` `97.5-quantile`
    ##   <dbl>  <dbl>          <dbl>           <dbl>
    ## 1  2.01 0.0239           1.97            2.03

log(β1\*β2) is likely to be symmetrically distributed with mean 2.01 .
The 2.5% quantile and 97.5% quantile are 1.966 and 2.031.

# Problem 2

## Import data and tidy data

``` r
url <- 'https://raw.githubusercontent.com/washingtonpost/data-homicides/master/homicide-data.csv'
homicides_df <- read_csv(url(url), na = c(" ", "Unknown"))
```

    ## Rows: 52179 Columns: 12
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (8): uid, victim_last, victim_first, victim_race, victim_sex, city, stat...
    ## dbl (4): reported_date, victim_age, lat, lon
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
homicides_df <- homicides_df %>% 
  mutate(city_state = str_c(city, state, sep = ", "),
         whether_solved = ifelse(
           disposition %in% c("Closed without arrest", "Open/No arrest"), 0, 1)) %>%
  filter(!city_state %in% c("Dallas, TX", "Phoenix, AZ","Kansas City, MO", "Tulsa, AL")) %>%
  filter(victim_race %in% c("White", "Black"))%>%
  filter(!victim_age == "Unknown") %>%
  mutate(victim_age = as.numeric(victim_age),
         victim_race = as.factor(victim_race),
         victim_sex = as.factor(victim_sex))
head(homicides_df)
```

    ## # A tibble: 6 × 14
    ##   uid    repor…¹ victi…² victi…³ victi…⁴ victi…⁵ victi…⁶ city  state   lat   lon
    ##   <chr>    <dbl> <chr>   <chr>   <fct>     <dbl> <fct>   <chr> <chr> <dbl> <dbl>
    ## 1 Alb-0…  2.01e7 SATTER… VIVIANA White        15 Female  Albu… NM     35.1 -107.
    ## 2 Alb-0…  2.01e7 MULA    VIVIAN  White        72 Female  Albu… NM     35.1 -107.
    ## 3 Alb-0…  2.01e7 BOOK    GERALD… White        91 Female  Albu… NM     35.2 -107.
    ## 4 Alb-0…  2.01e7 MARTIN… GUSTAVO White        56 Male    Albu… NM     35.1 -107.
    ## 5 Alb-0…  2.01e7 GRAY    STEFAN… White        43 Female  Albu… NM     35.1 -107.
    ## 6 Alb-0…  2.01e7 DAVID   LARRY   White        52 Male    Albu… NM     NA     NA 
    ## # … with 3 more variables: disposition <chr>, city_state <chr>,
    ## #   whether_solved <dbl>, and abbreviated variable names ¹​reported_date,
    ## #   ²​victim_last, ³​victim_first, ⁴​victim_race, ⁵​victim_age, ⁶​victim_sex

Fit a logistic regression with resolved vs unresolved as the outcome and
victim age, sex and race as predictors for Baltimore, MD.

``` r
logi_baltimore= 
  homicides_df %>% 
  filter(city_state=="Baltimore, MD") %>% 
  glm(whether_solved ~ victim_age+victim_race+victim_sex, data=., family= binomial())

logi_baltimore %>%
  broom::tidy() %>%
   mutate(
    estimate = exp(estimate),
    lower = exp(confint(logi_baltimore)[,1]),
    upper = exp(confint(logi_baltimore)[,2])
   ) %>%
    filter(term == "victim_sexMale") %>%
  select(estimate, lower, upper)
```

    ## Waiting for profiling to be done...
    ## Waiting for profiling to be done...

    ## # A tibble: 1 × 3
    ##   estimate lower upper
    ##      <dbl> <dbl> <dbl>
    ## 1    0.426 0.324 0.558

Thus, we come to conclude that, male has 0.426 times higher odds chance
of getting a resolved case compare to female.

Now, Run for all other cities

``` r
# Create a function
city_odds <- function(df){
  
logi_cities <- glm(whether_solved ~ victim_age+victim_race+victim_sex,data = df , family= binomial())

logi_cities %>%
  broom::tidy() %>%
   mutate(
    estimate = exp(estimate),
    lower = exp(confint(logi_cities)[,1]),
    upper = exp(confint(logi_cities)[,2])
   ) %>%
    filter(term == "victim_sexMale") %>%
  select(estimate, lower, upper)
  
}

# apply function to cities
result_df = 
  homicides_df %>% 
  select(city_state, victim_age, victim_race, victim_sex, whether_solved)%>%
  nest(df = victim_age:whether_solved) %>%
   mutate(
    model = map(df, city_odds)
  ) %>%
  select(city_state, model)%>%
  unnest(model)
result_df
```

    ## # A tibble: 47 × 4
    ##    city_state      estimate lower upper
    ##    <chr>              <dbl> <dbl> <dbl>
    ##  1 Albuquerque, NM    1.77  0.825 3.76 
    ##  2 Atlanta, GA        1.00  0.680 1.46 
    ##  3 Baltimore, MD      0.426 0.324 0.558
    ##  4 Baton Rouge, LA    0.381 0.204 0.684
    ##  5 Birmingham, AL     0.870 0.571 1.31 
    ##  6 Boston, MA         0.667 0.351 1.26 
    ##  7 Buffalo, NY        0.521 0.288 0.936
    ##  8 Charlotte, NC      0.884 0.551 1.39 
    ##  9 Chicago, IL        0.410 0.336 0.501
    ## 10 Cincinnati, OH     0.400 0.231 0.667
    ## # … with 37 more rows

## Create plot

``` r
ggplot(result_df, aes(x=fct_reorder(city_state, estimate), y=estimate))+
  geom_point()+
  geom_errorbar(aes(ymin=lower, ymax=upper))+
  labs(title = "Odds ratio of resolved homicide cases of male against female ", 
       x = "city",
       y = "odds ratio") +
  theme(axis.text.x = element_text(angle = 75, hjust = 1))
```

![](HW6_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->
