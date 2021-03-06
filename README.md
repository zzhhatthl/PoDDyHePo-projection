[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6333982.svg)](https://doi.org/10.5281/zenodo.6333982)

<!-- README.md is generated from README.Rmd. Please edit that file -->

Introduction
============

The purpose of this package is to project prevalences of health
indicators in future. The method uses data on cross-sectional surveys
and national population forecasts to take into account the changing
population structure. This package contains the following functions:

1.  `PoDDyHePoFreqTable`: creates a descriptive table with sample size
    and amount of missingness in the data.
2.  `PoDDyHePoModelSelection`: selects models for projection step
    according to BIC.
3.  `PoDDyHePoPopulationDF`: uses population forecasts, in example from
    Statistics Finland, and well-formatted survey data to create a data
    frame ready for projection.
4.  `PoDDyHePoProjection`: projects individual-level values of the
    variables using multiple imputation.
5.  `PoDDyHePoPool`: calculates prevalences by pooling results from
    `PoDDyHePoProjection`.
6.  `PoDDyHePoPlot`: plots the prevalences against survey year.

Getting stated
==============

Before getting started, make sure that you have installed and attached
the package. Also, remember to load the following package: `splines`.

    # Install devtools if you do not have.
    # install.packages("devtools")
    library(devtools)
    install_github("zzhhatthl/PoDDyHePo-projection")
    # After installation, please attach both PoDDyHePo-projection and splines.
    library(csprojections)
    library(splines)

For all the examples, please load `testdata` (synthetic data built in
the package):

    data(testdata)

In the `testdata`, we have mandatory variables:

-   `year`: Survey year. Numeric. 1997, 2002, 2007, 2012 and 2017. It is
    not limited and could be any other survey years.
-   `sex`: Categorical. 1 for men and 2 for women. Currently, the
    function supports only 1 and 2 coding. Please ensure that the coding
    used for `sex` is correct.
-   `age`: Numeric. Ranges from 25 - 64, but not limited to this range.
    In your case, it could be any range that you are interested in.

Main variables of interest:

-   `obe`: BMI categories, including obesity, overweight and normal
    weight. Categorical. 0 = Normal weight, 1 = Overweight and 2 =
    Obesity.
-   `smo`: Smoking status. Binary. 0 = Non-smoker, 1 = Smoker.
-   `highbp`: Hypertension. Binary. 0 = Non-hypertensive, 1 =
    Hypertensive.
-   `dbt`: Diabetes. Binary. 0 = Non-diabetic, 1 = Diabetic.
-   `sed`: Sedentary lifestyle. Binary. 0 = Non-sedentary lifestyle, 1 =
    Sedentary lifestyle.
-   `highkol`: Elevated total cholesterol. Binary. 0 = Not high total
    cholesterol, 1 = High total cholesterol.

Other background variables:

-   `edulv`: Education level. Categorical. 1 = Low education level, 2 =
    Middle education level, 3 = High education level.
-   `marsta`: Marital Status. Binary. 0 = Unmarried/Separated or
    Divorced/Widowed, 1 = Married/Cohabiting/Registered relationship.
-   `area`: Categorical. 2 = North Karelia, 3 = North Savonia, 4 = Turku
    and Loimaa, 5 = Helsinki and Vantaa, 6 = Northern Ostrobothnia and
    Kainuu.

When formatting your data set, the data types of year, sex and age
should be numeric, factor and numeric, respectively. Other
binary/categorical variables should be factor and continuous numeric.

Guidelines for Using the package
--------------------------------

**Step 1**: If data from past surveys include missing values, they are
imputed before selecting models for projection.

    imp <- mice(testdata, m = 5, maxit = 10, printFlag = F)

NB: `m` and `maxit` can be any number you want, but please consider the
computational power of your workstation. Usually, `m = 10` or `m = 20`
should be enough. And if `printFlag = T`, `mice` will print history on
console. Here, `printFlag = F` is used for silent computation. Data set
should be well-formatted like `testdata`, which means there are
mandatory variables: year, sex and age named exactly in this way. The
main variables of interest whose prevalences are to be estimated, and
possibly other background variables. Data from different years are
merged into the same data frame.

**Step 2**: Select models with `PoDDyHePoModelSelection`. The user can
specify if some variables are allowed to have non-linear effects, which
are modelled using natural splines. Some of the variables can also be
forced (fixed) to be included in the model. Here in this example, we
define knots and boundary knots in the spline function `ns`, and fix
variables year, sex, age and an interaction term: year:sex.

In the function `PoDDyHePoModelSelection`,

-   `imp` is an object of class from `mice` (see *Step* 1).
-   `DV` is the abbreviation of dependent variable.
-   Arguments `NsVar`, `df`, `knots`, `b.knots` are for customizing ns
    function in the model.
    -   `NsVar` specifies variables with natural spline (two or more
        variables supported),
    -   `df`: degree of freedom,
    -   `knots`: breakpoints that define the spline,
    -   `b.knots`: boundary knots.

If NsVar is NULL, `df`, `knots` and `b.knots` ignored; if it is not, we
can set `df` or `knots` and `b.knots.` Examples are given as follows
based on testdata.

More details about `ns` function can be found via
<a href="https://www.rdocumentation.org/packages/splines/versions/3.6.2/topics/ns" class="uri">https://www.rdocumentation.org/packages/splines/versions/3.6.2/topics/ns</a>.

    # With 1 preditor varaible
    smo <- PoDDyHePoModelSelection(imp, 
                                   NsVar = "year", 
                                   DV = "smo", 
                                   knots = 2002, 
                                   b.knots = list(c(1997, 2017)), 
                                   f.var = c("year", "sex", "age", "year:sex"))

    # With 2 predictor variables
    smo <- PoDDyHePoModelSelection(imp, 
                                   NsVar = c("year", "age"), 
                                   DV = "smo", 
                                   knots = c(2002, 40), 
                                   b.knots = list(c(1997, 2017), c(25, 64)), 
                                   f.var = c("year", "sex", "age", "year:sex"))

NB: Please use `list()` when specifying `b.knots`.

For other variables, if other arguments remain the same, what we need to
do is to change `DV`, like

    obe <- PoDDyHePoModelSelection(imp, 
                                   NsVar = "year", 
                                   DV = "obe", 
                                   knots = 2002, 
                                   b.knots = c(1997, 2017), 
                                   f.var = c("year", "sex", "age", "year:sex"))

This function returns a list with 4 elements. One for recording the
selection process, one for storing the mean BIC from each imputed data
set, one for saving the selected models, and the last one is for the
suggested model. Please notice that, the projections may be sensitive to
the imputation models used, so it is recommended to carry out
sensitivity analyses by trying alternative models.

**Step 3**: Use population forecasts for example from Statistics Finland
and your survey data (here is `testdata`) to create a data frame for
projection.

    testdata2040 <- PoDDyHePoPopulationDF(testdata,
                                          file = "~path/003_139f_2040_20211102-100631.csv", 
                                          size = 10000, 
                                          y2pred = c(2021, 2025, 2030, 2035, 2040))

Basic information (sturcture, data types and summary) of testdata2040
are given below.

    # head(testdata2040) returns first 6 rows of testdata2040.
    # > head(testdata2040)
    #   year sex age  obe  smo highbp edulv marsta area  dbt  sed highkol
    # 1 1997   2  63    2    0      1     1      0    3    0    0       0
    # 2 2007   1  30 <NA> <NA>   <NA>  <NA>   <NA>    6 <NA> <NA>    <NA>
    # 3 2007   2  50    1    0      0     2      1    2    0    0       1
    # 4 2007   2  54    1    0      1     2      1    6 <NA>    1       1
    # 5 2007   2  46    2    0      0     2      1    2    0    0       0
    # 6 2002   2  29 <NA> <NA>   <NA>  <NA>   <NA>    4 <NA> <NA>    <NA>


    # str(testdata2040) shows the structure of testdata2040.
    # > str(testdata2040)
    # 'data.frame': 89172 obs. of  12 variables:
    #  $ year   : num  1997 2007 2007 2007 2007 ...
    #  $ sex    : chr  "2" "1" "2" "2" ...
    #  $ age    : num  63 30 50 54 46 29 30 56 46 33 ...
    #  $ obe    : Factor w/ 3 levels "0","1","2": 3 NA 2 2 3 NA 2 3 2 NA ...
    #  $ smo    : Factor w/ 2 levels "0","1": 1 NA 1 1 1 NA 2 1 2 2 ...
    #  $ highbp : Factor w/ 2 levels "0","1": 2 NA 1 2 1 NA 2 2 1 NA ...
    #  $ edulv  : Factor w/ 3 levels "1","2","3": 1 NA 2 2 2 NA 1 3 3 1 ...
    #  $ marsta : Factor w/ 2 levels "0","1": 1 NA 2 2 2 NA 1 2 2 2 ...
    #  $ area   : Factor w/ 5 levels "2","3","4","5",..: 2 5 1 5 1 3 4 3 3 2 ...
    #  $ dbt    : Factor w/ 2 levels "0","1": 1 NA 1 NA 1 NA 1 1 1 1 ...
    #  $ sed    : Factor w/ 2 levels "0","1": 1 NA 1 2 1 NA 1 1 2 1 ...
    #  $ highkol: Factor w/ 2 levels "0","1": 1 NA 2 2 1 NA 1 2 2 1 ...


    # summeries testdata2040
    # > summary(testdata2040)
    #       year          sex                 age          obe          smo         highbp       edulv        marsta     
    #  Min.   :1997   Length:89172       Min.   :25.00   0   :10040   0   :19626   0   :14838   1   : 8632   0   : 6974  
    #  1st Qu.:2007   Class :character   1st Qu.:34.00   1   : 9434   1   : 6457   1   :10007   2   : 8495   1   :19198  
    #  Median :2020   Mode  :character   Median :44.00   2   : 5152   NA's:63089   NA's:64327   3   : 8843   NA's:63000  
    #  Mean   :2019                      Mean   :44.43   NA's:64546                             NA's:63202               
    #  3rd Qu.:2030                      3rd Qu.:54.00                                                                   
    #  Max.   :2040                      Max.   :64.00                                                                   
    #    area         dbt          sed        highkol     
    #  2   : 7506   0   :23698   0   :20171   0   : 9870  
    #  3   : 7616   1   :  738   1   : 5850   1   :16254  
    #  4   : 7838   NA's:64736   NA's:63151   NA's:63048  
    #  5   : 8381                                         
    #  6   : 7835                                         
    #  NA's:49996                                           

In the function `PoDDyHePoPopulationDF`,

-   `data` is the well-formatted data, in the example `testdata`.
-   `file` is a csv file downloaded from Statistics Finland (with
    special requirements given below).
-   `size` is a sample size.
-   and `y2pred` is the years to be projected. Above, 2021, 2025, 2030,
    2035 and 2040, are the years to be projected, so
    `y2pred = c(2021, 2025, 2030, 2035, 2040)`.

If you want to try our interrnal population forecasts, use
`file = system.file("extdata", "003_128v_2040_20210730-015759.csv", package = "csprojections")`.

In order to make this function work, the population forecast data has to
be in a specific form. In case of Finland, data can be downloaded in the
correct form by following the next steps:

1.  Visit
    <a href="https://pxnet2.stat.fi/PXWeb/pxweb/en/StatFin/StatFin__vrm__vaenn/" class="uri">https://pxnet2.stat.fi/PXWeb/pxweb/en/StatFin/StatFin__vrm__vaenn/</a>,
    scroll down and find Population projection.
2.  Click 139f ??? Population projection 2021: Population according to age
    and sex by area, 2021-2040 \[Size: 5818 Kb\] \[Modified:
    9/30/2021\].
3.  Select Population 31 Dec (projection 2019) (Information) - WHOLE
    COUNTRY (Area) - Select the years to be projected (Year) -Select all
    (Sex) - Select all (Age), and click Show table. (In our example, it
    is 2021, 2025, 2030, 2035 and 2040).
4.  Click ???pivot manual??? and set Area, Age in the Rows box (Order: Area,
    Age) and set Year, Sex, Information in the Columns box (Order: Year,
    Sex, Information) and click ???complete???.
5.  Click ???Save the result as ?????? - select ???Semicolon delimited with
    heading??? and save.

If you use population forecasts outside Finland, please format it
correctly before putting it in the function. You can download the
population forecasts that we used via github to see how it looks like.
It is under extdata in the folder inst. Or by using the following
commands:

    # Create a dataframe ready for projection, skip = 2 tell the function to skip first two rows,
    # and by setting colClasses = c(Area="NULL"), we sikp the first column. 
    # Those are in the csv file when we downloaded it from Statistics Finland, but we do not need.
    population_forecasts <- utils::read.table(system.file("extdata", "003_128v_2040_20210730-015759.csv", package = "csprojections"), 
                                              sep = ";", 
                                              skip = 2, 
                                              header = T, 
                                              check.names = F,
                                              colClasses = c(Area="NULL"))

In the population forecasts, Age should be named exactly Age. Variable
names like 2020 Total Population 31 Dec (projection 2019), 2020 Males
Population 31 Dec (projection 2019) and 2020 Females Population 31 Dec
(projection 2019) can be simplified to 2020 Total, 2020 Males and 2020
Females. Data description is given below.

    # > str(population_forecasts)
    # 'data.frame': 102 obs. of  16 variables:
    #  $ Age                                             : Factor w/ 102 levels "0","1","10","100 -",..: 102 1 2 14 25 36 47 58 69 80 ...
    #  $ 2020 Total Population 31 Dec (projection 2019)  : int  5530922 45710 46123 48338 51539 54368 56932 59305 60339 61821 ...
    #  $ 2020 Males Population 31 Dec (projection 2019)  : int  2732995 23362 23569 25015 26321 27608 29244 30328 31004 31525 ...
    #  $ 2020 Females Population 31 Dec (projection 2019): int  2797927 22348 22554 23323 25218 26760 27688 28977 29335 30296 ...
    #  $ 2025 Total Population 31 Dec (projection 2019)  : int  5556546 44999 45557 46066 46522 46909 47251 47588 49744 52789 ...
    #  $ 2025 Males Population 31 Dec (projection 2019)  : int  2752594 22994 23286 23562 23774 23985 24165 24333 25721 26953 ...
    #  $ 2025 Females Population 31 Dec (projection 2019): int  2803952 22005 22271 22504 22748 22924 23086 23255 24023 25836 ...
    #  $ 2030 Total Population 31 Dec (projection 2019)  : int  5566685 44039 44540 45096 45592 46067 46564 47032 47451 47848 ...
    #  $ 2030 Males Population 31 Dec (projection 2019)  : int  2763297 22507 22766 23056 23313 23546 23817 24052 24255 24465 ...
    #  $ 2030 Females Population 31 Dec (projection 2019): int  2803388 21532 21774 22040 22279 22521 22747 22980 23196 23383 ...
    #  $ 2035 Total Population 31 Dec (projection 2019)  : int  5556472 43670 44077 44449 44810 45222 45611 46009 46490 46908 ...
    #  $ 2035 Males Population 31 Dec (projection 2019)  : int  2764088 22319 22531 22723 22906 23124 23326 23524 23774 23981 ...
    #  $ 2035 Females Population 31 Dec (projection 2019): int  2792384 21351 21546 21726 21904 22098 22285 22485 22716 22927 ...
    #  $ 2040 Total Population 31 Dec (projection 2019)  : int  5525528 43176 43729 44200 44604 44901 45239 45549 45829 46160 ...
    #  $ 2040 Males Population 31 Dec (projection 2019)  : int  2756428 22064 22353 22595 22809 22951 23144 23294 23422 23609 ...
    #  $ 2040 Females Population 31 Dec (projection 2019): int  2769100 21112 21376 21605 21795 21950 22095 22255 22407 22551 ...

So, when formatting your population forecasts, make sure you have Age
and Total, Males, Females for each predicted year. As long as you have
formatted your population forecasts which is similar to the population
forecasts we provide, you can put it in the function. And the function
will help you create a data frame ready for prediction.

**Step 4**: Project values for the future years.

    projection_with_knots <- PoDDyHePoProjection(testdata2040, 
                                                 m = 5, 
                                                 maxit = 10,
                                                 printFlag = F,
                                                 smo ~ ns(year, knots = c(2002), Boundary.knots = c(1997, 2017)) + 
                                                   sex + age + edulv + sed + marsta + obe + 
                                                   ns(year, knots = c(2002), Boundary.knots = c(1997, 2017)):sex + 
                                                   age:edulv + age:marsta + age:obe + sex:age,
                                                 obe$`Suggested Model`,
                                                 dbt$`Suggested Model`,
                                                 sed$`Suggested Model`,
                                                 highbp$`Suggested Model`,
                                                 highkol$`Suggested Model`,
                                                 edulv$`Suggested Model`,
                                                 marsta$`Suggested Model`,
                                                 area$`Suggested Model`)

In the `PoDDyHePoProjection` function,

-   `data` the first argument, is the data set from *Step 3*.
-   `m` is the number of multiple imputations, by default this is 5, but
    much greater value is usually needed in practice.
-   `maxit` is a scalar giving the number of iterations.
-   `printFlag` is `FALSE`, `mice` will not print history on console.
-   The last part is for the formulas of the imputation models. In *Step
    2*, we found models for variables with missingness, and they have
    been saved in the `Suggested Model` in the list given by
    `PoDDyHePoModelSelection`. So, it can be used in a way like `obe`:
    obe$\`Suggested Model\`, or specified our own model like for `smo`
    in the code above. Every variable with missingness has to be given a
    formula.

**Step 5**: Pool the results from the projection step and plot the
prevalences. Given Current smoking and body weight

    # Current smoking
    smopool <- PoDDyHePoPool(projection_with_knots, colName = "smo", grpVar = NULL)
    PoDDyHePoPlot(smopool, year = 2017, grplabels = c("Men", "Women"), title = "Current smoking", y_min = 0, y_max = 40)

    # BMI categories
    obepool <- PoDDyHePoPool(projection_with_knots, colName = "obe", grpVar = "sex")
    PoDDyHePoPlot(obepool, year = 2017, grplabels = c("Men", "Women"), title = "BMI categories", y_min = 0, y_max = 75, sepvarlbl = c("Normal Weight", "Overweight", "Obesity"))

In the function `PoDDyHePoPool`,

-   `imp`, is the imputed data set from *Step 4*.
-   `colName` is the main variable whose prevalences we aim to estimate.
-   `grpVar` is the grouping variable. By default, it is `NULL`. But in
    our case, we use grpVar = ???sex???. The grouping variable should not
    have any missing value, like `year`, `sex` and `age`. In order to
    use `age` as grouping variable, you must change numeric age into
    predefined age group. NOTICE: Grouping variable should not have more
    than five categories.

This function returns the prevalences in numerical form. It gives you a
table with year, prevalences and prediction intervals.

In the function `PoDDyHePoPlot`,

-   `data` is the result from `PoDDyHePoPool`.
-   `year` is the maximum year in the observed data (In `testdata`, it
    is 2017).
-   `grplabels`, the labels of grouping variable. By default,
    `grplabels = NULL`. As we used `sex` as grouping variable, our
    setting is `grplables = c("Men", "Women")`.
-   `title` is the title for the figure.
-   `y_min` and `y_max` are for setting the range of y-axis.
-   When there is a variable has three or more levels, an extra argument
    `sepvarlbl` is available, which is to label the levels of this
    variable. Otherwise, the labels for the newly created variables will
    be `obe_0`, `obe_1` and `obe_2`.

NOTICE: Due the limitation of point type, `PoDDyHePoPlot` function does
not support variable with more than five levels.

Descriptive table with sample size and amount of respondents is also
available by using `PoDDyHePoFreqtable`.

There are two arguments in this function,

-   the observed data (`testdata`).
-   and a variable name of interest. Examples are given below.

<!-- -->

    PoDDyHePoFreqTable(testdata, colName = "smo")
    PoDDyHePoFreqTable(testdata, colName = "obe")

NOTICE: The function creates a table for only one variable. Does not
support several variables at one time.

Acknowledgement
===============

This R code has been developed in the framework of the Projections of
the burden of disease and disability in Finland - health policy
prospects (PoDDy-HePo) project which was funded by the Academy of
Finland no. 307907 (2017-2021).
