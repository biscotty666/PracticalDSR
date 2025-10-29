# Manging Data


``` r
customer_data <- readRDS("repo-clone/Custdata/custdata.RDS")
```

# Cleaning

## Domain-specific

``` r
library(dplyr)
library(data.table)
```

``` r
customer_data_dt <- as.data.table(customer_data)
customer_data_df <- customer_data
```

Dealing with invalid values

``` r
customer_data_df <- customer_data_df %>% 
  mutate(age = na_if(age, 0),
         income = ifelse(income < 0, NA, income))
head(customer_data_df)
```

             custid    sex is_employed income     marital_status health_ins
    7  000006646_03   Male        TRUE  22000      Never married       TRUE
    8  000007827_01 Female          NA  23200 Divorced/Separated       TRUE
    9  000008359_04 Female        TRUE  21000      Never married       TRUE
    10 000008529_01 Female          NA  37770            Widowed       TRUE
    11 000008744_02   Male        TRUE  39000 Divorced/Separated       TRUE
    15 000011466_01   Male          NA  11100            Married       TRUE
                       housing_type recent_move num_vehicles age state_of_res
    7      Homeowner free and clear       FALSE            0  24      Alabama
    8                        Rented        TRUE            0  82      Alabama
    9  Homeowner with mortgage/loan       FALSE            2  31      Alabama
    10     Homeowner free and clear       FALSE            1  93      Alabama
    11                       Rented       FALSE            2  67      Alabama
    15     Homeowner free and clear       FALSE            2  76      Alabama
       gas_usage
    7        210
    8          3
    9         40
    10       120
    11         3
    15       200

``` r
customer_data_dt[, `:=`(age = na_if(age, 0),
                        income = ifelse(income < 0, NA, income))]
customer_data_dt
```

                 custid    sex is_employed income     marital_status health_ins
                 <char> <fctr>      <lgcl>  <num>             <fctr>     <lgcl>
        1: 000006646_03   Male        TRUE  22000      Never married       TRUE
        2: 000007827_01 Female          NA  23200 Divorced/Separated       TRUE
        3: 000008359_04 Female        TRUE  21000      Never married       TRUE
        4: 000008529_01 Female          NA  37770            Widowed       TRUE
        5: 000008744_02   Male        TRUE  39000 Divorced/Separated       TRUE
       ---                                                                     
    73258: 001506841_02 Female        TRUE  18500      Never married      FALSE
    73259: 001507219_01 Female          NA  20800            Widowed       TRUE
    73260: 001513103_01   Male        TRUE  75000            Married       TRUE
    73261: 001519624_01 Female        TRUE  22200 Divorced/Separated      FALSE
    73262: 001520877_01   Male        TRUE  16400      Never married       TRUE
                           housing_type recent_move num_vehicles   age state_of_res
                                 <fctr>      <lgcl>        <num> <num>       <fctr>
        1:     Homeowner free and clear       FALSE            0    24      Alabama
        2:                       Rented        TRUE            0    82      Alabama
        3: Homeowner with mortgage/loan       FALSE            2    31      Alabama
        4:     Homeowner free and clear       FALSE            1    93      Alabama
        5:                       Rented       FALSE            2    67      Alabama
       ---                                                                         
    73258:                       Rented       FALSE            1    25      Wyoming
    73259:     Homeowner free and clear       FALSE            1    86      Wyoming
    73260: Homeowner with mortgage/loan       FALSE            2    50      Wyoming
    73261:     Homeowner free and clear       FALSE            1    61      Wyoming
    73262:                         <NA>          NA           NA    31      Wyoming
           gas_usage
               <num>
        1:       210
        2:         3
        3:        40
        4:       120
        5:         3
       ---          
    73258:        10
    73259:       120
    73260:        90
    73261:        50
    73262:        NA

``` r
nrow(customer_data_df)
```

    [1] 73262

``` r
nrow(customer_data_dt)
```

    [1] 73262

Handling special coding

``` r
customer_data_df <- customer_data_df %>% 
  mutate(gas_with_rent = (gas_usage == 1),
         gas_with_electricity = (gas_usage == 2),
         no_gas_bill = (gas_usage == 3),
         gas_usage = ifelse(gas_usage < 4, NA, gas_usage))
head(customer_data_df)
```

             custid    sex is_employed income     marital_status health_ins
    7  000006646_03   Male        TRUE  22000      Never married       TRUE
    8  000007827_01 Female          NA  23200 Divorced/Separated       TRUE
    9  000008359_04 Female        TRUE  21000      Never married       TRUE
    10 000008529_01 Female          NA  37770            Widowed       TRUE
    11 000008744_02   Male        TRUE  39000 Divorced/Separated       TRUE
    15 000011466_01   Male          NA  11100            Married       TRUE
                       housing_type recent_move num_vehicles age state_of_res
    7      Homeowner free and clear       FALSE            0  24      Alabama
    8                        Rented        TRUE            0  82      Alabama
    9  Homeowner with mortgage/loan       FALSE            2  31      Alabama
    10     Homeowner free and clear       FALSE            1  93      Alabama
    11                       Rented       FALSE            2  67      Alabama
    15     Homeowner free and clear       FALSE            2  76      Alabama
       gas_usage gas_with_rent gas_with_electricity no_gas_bill
    7        210         FALSE                FALSE       FALSE
    8         NA         FALSE                FALSE        TRUE
    9         40         FALSE                FALSE       FALSE
    10       120         FALSE                FALSE       FALSE
    11        NA         FALSE                FALSE        TRUE
    15       200         FALSE                FALSE       FALSE

``` r
customer_data_dt[, `:=`(gas_with_rent = (gas_usage == 1),
                        gas_with_electricity = (gas_usage == 2),
                        no_gas_bill = (gas_usage == 3),
                        gas_usage = ifelse(gas_usage < 4, 
                                           NA, gas_usage))]
head(customer_data_dt)
```

             custid    sex is_employed income     marital_status health_ins
             <char> <fctr>      <lgcl>  <num>             <fctr>     <lgcl>
    1: 000006646_03   Male        TRUE  22000      Never married       TRUE
    2: 000007827_01 Female          NA  23200 Divorced/Separated       TRUE
    3: 000008359_04 Female        TRUE  21000      Never married       TRUE
    4: 000008529_01 Female          NA  37770            Widowed       TRUE
    5: 000008744_02   Male        TRUE  39000 Divorced/Separated       TRUE
    6: 000011466_01   Male          NA  11100            Married       TRUE
                       housing_type recent_move num_vehicles   age state_of_res
                             <fctr>      <lgcl>        <num> <num>       <fctr>
    1:     Homeowner free and clear       FALSE            0    24      Alabama
    2:                       Rented        TRUE            0    82      Alabama
    3: Homeowner with mortgage/loan       FALSE            2    31      Alabama
    4:     Homeowner free and clear       FALSE            1    93      Alabama
    5:                       Rented       FALSE            2    67      Alabama
    6:     Homeowner free and clear       FALSE            2    76      Alabama
       gas_usage gas_with_rent gas_with_electricity no_gas_bill
           <num>        <lgcl>               <lgcl>      <lgcl>
    1:       210         FALSE                FALSE       FALSE
    2:        NA         FALSE                FALSE        TRUE
    3:        40         FALSE                FALSE       FALSE
    4:       120         FALSE                FALSE       FALSE
    5:        NA         FALSE                FALSE        TRUE
    6:       200         FALSE                FALSE       FALSE

## Missing values

``` r
sapply(customer_data_df, function(x) sum(is.na(x)))
```

                  custid                  sex          is_employed 
                       0                    0                25774 
                  income       marital_status           health_ins 
                      45                    0                    0 
            housing_type          recent_move         num_vehicles 
                    1720                 1721                 1720 
                     age         state_of_res            gas_usage 
                      77                    0                35702 
           gas_with_rent gas_with_electricity          no_gas_bill 
                    1720                 1720                 1720 

``` r
summarise(customer_data_df,
          across(everything(), ~ sum(is.na(.))))
```

      custid sex is_employed income marital_status health_ins housing_type
    1      0   0       25774     45              0          0         1720
      recent_move num_vehicles age state_of_res gas_usage gas_with_rent
    1        1721         1720  77            0     35702          1720
      gas_with_electricity no_gas_bill
    1                 1720        1720

``` r
cna <- function(x) {sum(is.na(x))}
customer_data_dt[, lapply(.SD, function(x) sum(is.na(x)))]
```

       custid   sex is_employed income marital_status health_ins housing_type
        <int> <int>       <int>  <int>          <int>      <int>        <int>
    1:      0     0       25774     45              0          0         1720
       recent_move num_vehicles   age state_of_res gas_usage gas_with_rent
             <int>        <int> <int>        <int>     <int>         <int>
    1:        1721         1720    77            0     35702          1720
       gas_with_electricity no_gas_bill
                      <int>       <int>
    1:                 1720        1720

## Automatic treatment of missing variables.

Choose all columns except target and custid.

``` r
varlist <- setdiff(colnames(customer_data_df),
                   c("custid", "health_ins"))
```

``` r
library(vtreat)
```

    Loading required package: wrapr


    Attaching package: 'wrapr'

    The following objects are masked from 'package:data.table':

        :=, let

    The following object is masked from 'package:dplyr':

        coalesce

    The following object is masked from 'package:base':

        grepv

``` r
treatment_plan <- design_missingness_treatment(
  customer_data_df, varlist = varlist)
training_prepared <- prepare(treatment_plan, customer_data_df)
```

``` r
setdiff(colnames(training_prepared),
        colnames(customer_data_df))
```

    [1] "is_employed_isBAD"          "income_isBAD"              
    [3] "recent_move_isBAD"          "num_vehicles_isBAD"        
    [5] "age_isBAD"                  "gas_usage_isBAD"           
    [7] "gas_with_rent_isBAD"        "gas_with_electricity_isBAD"
    [9] "no_gas_bill_isBAD"         

``` r
sum(sapply(training_prepared, function(col) {sum(is.na(col))}))
```

    [1] 0

``` r
htmissing <- which(is.na(customer_data_df$housing_type))
cols <- c("custid", "is_employed", "num_vehicles",
           "housing_type", "health_ins")
customer_data_df[htmissing, cols] %>% head()
```

              custid is_employed num_vehicles housing_type health_ins
    80  000082691_01        TRUE           NA         <NA>      FALSE
    100 000116191_01        TRUE           NA         <NA>       TRUE
    237 000269295_01          NA           NA         <NA>      FALSE
    299 000349708_01          NA           NA         <NA>      FALSE
    311 000362630_01          NA           NA         <NA>       TRUE
    413 000443953_01          NA           NA         <NA>       TRUE

``` r
cols <- c("custid", "is_employed", "is_employed_isBAD",
          "num_vehicles","num_vehicles_isBAD",
          "housing_type", "health_ins")
training_prepared[htmissing, cols] %>% head()
```

              custid is_employed is_employed_isBAD num_vehicles num_vehicles_isBAD
    80  000082691_01   1.0000000                 0       2.0655                  1
    100 000116191_01   1.0000000                 0       2.0655                  1
    237 000269295_01   0.9504928                 1       2.0655                  1
    299 000349708_01   0.9504928                 1       2.0655                  1
    311 000362630_01   0.9504928                 1       2.0655                  1
    413 000443953_01   0.9504928                 1       2.0655                  1
        housing_type health_ins
    80     _invalid_      FALSE
    100    _invalid_       TRUE
    237    _invalid_      FALSE
    299    _invalid_      FALSE
    311    _invalid_       TRUE
    413    _invalid_       TRUE

``` r
customer_data_df %>% 
  summarise(mean_vehicles = mean(num_vehicles, na.rm = T),
            mean_employed = mean(as.numeric(is_employed), na.rm = T))
```

      mean_vehicles mean_employed
    1        2.0655     0.9504928

# Data Transformations

## Normalization

Normalizing income by state

``` r
median_income_table <- readRDS("repo-clone/Custdata/median_income.RDS")
head(median_income_table)
```

      state_of_res median_income
    1      Alabama         21100
    2       Alaska         32050
    3      Arizona         26000
    4     Arkansas         22900
    5   California         25000
    6     Colorado         32000

``` r
training_prepared <- training_prepared %>% 
  left_join(median_income_table, by = "state_of_res") %>% 
  mutate(income_normalized = income / median_income)
head(training_prepared[, c("income", "median_income",
                           "income_normalized")])
```

      income median_income income_normalized
    1  22000         21100         1.0426540
    2  23200         21100         1.0995261
    3  21000         21100         0.9952607
    4  37770         21100         1.7900474
    5  39000         21100         1.8483412
    6  11100         21100         0.5260664

``` r
summary(training_prepared$income_normalized)
```

       Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
     0.0000  0.4049  1.0000  1.5685  1.9627 46.5556 

Normalizing age by mean

``` r
summary(training_prepared$age)
```

       Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
      21.00   34.00   48.00   49.22   62.00  120.00 

``` r
mean_age <- mean(training_prepared$age)
age_normalized <- training_prepared$age/mean_age
summary(age_normalized)
```

       Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
     0.4267  0.6908  0.9753  1.0000  1.2597  2.4382 

## Centering and scaling

``` r
(mean_age <- mean(training_prepared$age))
```

    [1] 49.21647

``` r
(sd_age <- sd(training_prepared$age))
```

    [1] 18.0124

``` r
mean_age + c(-sd_age, sd_age)
```

    [1] 31.20407 67.22886

``` r
training_prepared$scaled_age <- (
  training_prepared$age - mean_age) / sd_age
```

``` r
training_prepared %>%
  filter(abs(age - mean_age) < sd_age) %>%
  select(age, scaled_age) %>%
  head()
```

      age scaled_age
    1  67  0.9872942
    2  54  0.2655690
    3  61  0.6541903
    4  64  0.8207422
    5  57  0.4321210
    6  55  0.3210864

``` r
training_prepared %>%
  filter(abs(age - mean_age) > sd_age) %>%
  select(age, scaled_age) %>%
  head()
```

      age scaled_age
    1  24  -1.399951
    2  82   1.820054
    3  31  -1.011329
    4  93   2.430745
    5  76   1.486950
    6  26  -1.288916

Typical customers have age magnitude less than 1.

Multiple variables with `scale()`

``` r
dataf <- training_prepared[, c("age", "income", 
                               "num_vehicles", "gas_usage")]
summary(dataf)
```

          age             income         num_vehicles     gas_usage     
     Min.   : 21.00   Min.   :      0   Min.   :0.000   Min.   :  4.00  
     1st Qu.: 34.00   1st Qu.:  10700   1st Qu.:1.000   1st Qu.: 50.00  
     Median : 48.00   Median :  26300   Median :2.000   Median : 76.01  
     Mean   : 49.22   Mean   :  41793   Mean   :2.066   Mean   : 76.01  
     3rd Qu.: 62.00   3rd Qu.:  51700   3rd Qu.:3.000   3rd Qu.: 76.01  
     Max.   :120.00   Max.   :1257000   Max.   :6.000   Max.   :570.00  

``` r
dataf_scaled <- scale(dataf, center = T, scale = T)
summary(dataf_scaled)
```

          age               income         num_vehicles        gas_usage      
     Min.   :-1.56650   Min.   :-0.7193   Min.   :-1.78631   Min.   :-1.4198  
     1st Qu.:-0.84478   1st Qu.:-0.5351   1st Qu.:-0.92148   1st Qu.:-0.5128  
     Median :-0.06753   Median :-0.2666   Median :-0.05665   Median : 0.0000  
     Mean   : 0.00000   Mean   : 0.0000   Mean   : 0.00000   Mean   : 0.0000  
     3rd Qu.: 0.70971   3rd Qu.: 0.1705   3rd Qu.: 0.80819   3rd Qu.: 0.0000  
     Max.   : 3.92971   Max.   :20.9149   Max.   : 3.40268   Max.   : 9.7400  

``` r
(means <- attr(dataf_scaled, "scaled:center"))
```

             age       income num_vehicles    gas_usage 
        49.21647  41792.51062      2.06550     76.00745 

``` r
(sds <- attr(dataf_scaled, "scaled:scale"))
```

             age       income num_vehicles    gas_usage 
       18.012397 58102.481410     1.156294    50.717778 

``` r
attributes(dataf_scaled)
```

    $dim
    [1] 73262     4

    $dimnames
    $dimnames[[1]]
    NULL

    $dimnames[[2]]
    [1] "age"          "income"       "num_vehicles" "gas_usage"   


    $`scaled:center`
             age       income num_vehicles    gas_usage 
        49.21647  41792.51062      2.06550     76.00745 

    $`scaled:scale`
             age       income num_vehicles    gas_usage 
       18.012397 58102.481410     1.156294    50.717778 

## Log transformations

For financial data, use *signed log* transformation.

Treats values between 1 and -1 as zero, otherwise applies the
appropriate sign to $\log(|x|)$.

``` r
signedlog10 <- function(x) {
  ifelse(abs(x) <= 1, 0, sign(x)*log10(abs(x)))
}
```

# Sampling for modeling and validation

## Creating a sample group column

``` r
set.seed(25643)
customer_data_df$gp <- runif(nrow(customer_data))
customer_test <- subset(customer_data_df, gp <= 0.1)
customer_train <- subset(customer_data_df, gp > 0.1)
dim(customer_test); dim(customer_train)
```

    [1] 7463   16

    [1] 65799    16

``` r
set.seed(25643)
test <- slice_sample(customer_data_df, prop = 0.1)
dim(test)
```

    [1] 7326   16

``` r
dim(setdiff(customer_data_df, test))
```

    [1] 65936    16

## Record grouping

Ensure households are kept together in the train/test split

``` r
hh_data <- readRDS("repo-clone/Custdata/hhdata.RDS")
hh <- unique(hh_data$household_id)
```

``` r
set.seed(243674)
households <- data.frame(
  household_id = hh, gp = runif(length(hh)),
  stringsAsFactors = F)
household_data <- left_join(hh_data, households,
                            by = "household_id")
head(household_data)
```

      household_id  customer_id age income        gp
    1    000008385 000008385_01  74  45600 0.2063638
    2    000012408 000012408_01  54  16300 0.4543296
    3    000013288 000013288_01  59 622000 0.9931105
    4    000013288 000013288_02  67  43000 0.9931105
    5    000017554 000017554_01  47  98000 0.6279021
    6    000017554 000017554_02  54  31200 0.6279021
