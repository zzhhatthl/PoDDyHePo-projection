<!-- README.md is generated from README.Rmd. Please edit that file -->

Introduction
============

The purpose of this package is to project prevalences of health
indicators in future. The method uses data on cross-sectional surveys
and national population forecasts to take into account the changing
population structure. This package contains the following functions:

1.  `PoDDyHePoFreqTable`: creates a descriptive table with sample size
    and amount of missingness in the data.
2.  `PoDDyHePoModelSelection`: selects models for according to BIC for
    projection step.
3.  `PoDDyHePoPopulationDF`: uses population forecasts from Statistics
    Finland and well-formatted survety data to create a data frame ready
    for projection.
4.  `PoDDyHePoProjection`: projects individual-level values of the
    variable using multiple imputation.
5.  `PoDDyHePoPool`: caculates prevalences by pooling results from
    `PoDDyHePoProjection`.
6.  `PoDDyHePoPlot`: plots the prevalences against survey year.

Getting started
===============

Before getting started, make sure the following package has been
attached: `splines`.

    library(splines)

For all the examples, please load `testdata` (synthetic data built in
the package):

    library(csprojections)
    data(testdata)

In the `testdata`, we have mandatory variables:

-   `year`: Survey year. Numeric. 1997, 2002, 2007, 2012 and 2017.
-   `sex`: Categorical. 1 for men and 2 for women.
-   `age`: Categorical. Ranges from 25 to 64.

Main variables of interest:

-   `obe`: Obesity/Overweight/Normal. Categorical. 0 = Normal weight, 1
    = Overweight and 2 = Obesity.
-   `smo`: Smoking status. Binary. 0 = Non-smoker, 1 = Smoker.
-   `highbp`: Hypertension. Binary. 0 = Non-hypertensive, 1 =
    hypertensive.
-   `dbt`: Diabetes. Binary. 0 = Non-diabetic, 1 = diabetic.
-   `sed`: Sedentary lifestyle. Binary. 0 = Non-sedentary lifestyle, 1 =
    sedentary lifestyle.
-   `highkol`: Elevated total cholesterol. Binary. 0 = Not high total
    cholesterol, 1 = high total cholesterol.

Other background variables:

-   `edulv`: Education level. Categorical. 1 = Low education level, 2 =
    Middle education level, 3 = high education level.
-   `marsta`: Marital Status. Binary. 0 = Unmarried/Separated or
    Divorced/Widowed, 1 = Married/Cohabiting/Registered relationship.
-   `area`: Categorical. 2 = North Karelia, 3 = North Savonia, 4 = Turku
    and Loimaa, 5 = Helsinki and Vantaa, 6 = Northern Ostrobothnia and
    Kainuu.

Guidelines for Using the Package
--------------------------------

**Step 1**: If data from past surveys include missing values, they are
imputed before selecting models for projection.

    imp <- mice(testdata, m = 5, maxit = 10, printFlag = F)

NB: `m` and `maxit` can be any number you want, but please consider the
computational power of your workstation. Usually, `m = 10` or `m = 20`
should be enough. And if `printFlag = T`, `mice` will print history on
console. Here, `printFlag=F` is used for silent computation. Data set
should be well-formatted like `testdata`, which means there are
mandatory variables: year, sex and age named exactly in this way, the
main variables of interest whose prevalences to be estimated, and
possibly other background variables. Data from different years are
merged into the same data frame.

**Step 2**: Select models with PoDDyHePoModelSelection. The user can
specify if some variables are allowed to have non-linear effects, which
are modelled using natural splines. Some of the variables can also be
forced (fixed) to be included in the model. Here in this example, we
define knots and boundary knots in the spline function `ns`, and fix
variables year, sex, age and an interaction term: year:sex.

In the function
`PoDDyHePoModelSelection(imp, DV, NsVar = NULL, df = NULL, knots = NULL, b.knots = NULL, f.var = NULL)`,
the first argument `imp` is an object of class mids from `mice()` in
mice package (see *Step 1*). `DV` is the abbreviation of dependent
variable. Arguments `NsVar`, `df`, `knots`, `b.knots` are for
customizing `ns` function in the model. `NsVar` specifies a variable
with a nature spline, if it is NULL, degrees of freedom(`df`),
breakpoints that define the spline (`knots`) and boundary points
(`b.knots`) are ignored; if it is not NULL, we can set df, knots and
b.knots. Examples are given as follows based on `testdata`. More details
about `ns` function can be found via
<a href="https://www.rdocumentation.org/packages/splines/versions/3.6.2/topics/ns" class="uri">https://www.rdocumentation.org/packages/splines/versions/3.6.2/topics/ns</a>.

    smo <- PoDDyHePoModelSelection(imp, 
                                   NsVar = "year", 
                                   DV = "smo", 
                                   knots = 2002, 
                                   b.knots = c(1997, 2017), 
                                   f.var = c("year", "sex", "age", "year:sex"))

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

**Step 3**: Use population forecasts from Statistics Finland and our
survey data (here is `testdata`) to create a data frame for projection.

    testdata2040 <- PoDDyHePoPopulationDF(testdata,
                                          file = "~path/003_139f_2040_20211102-100631.csv", 
                                          size = 10000, 
                                          y2pred = c(2021, 2025, 2030, 2035, 2040))

For the function `PoDDyHePoPopulationDF`, the first argument is the
well-formatted data, in the example `testdata`. The second argument,
which is “file”, is a csv file downloaded from Statistics Finland (with
special requirements given below). The third is a sample size and the
fourth is the years to be projected. Above, 2021, 2025, 2030, 2035 and
2040, are the years to be projected, so
`y2pred = c(2021, 2025, 2030, 2035, 2040)`.

In order to make this function work, the population forecast data has to
be in a specific form. Data can be downloaded in the correct form by
following the next steps:

1.  Visit
    <a href="https://pxnet2.stat.fi/PXWeb/pxweb/en/StatFin/StatFin__vrm__vaenn/" class="uri">https://pxnet2.stat.fi/PXWeb/pxweb/en/StatFin/StatFin__vrm__vaenn/</a>,
    scroll down and find Population projection.
2.  Click 139f – Population projection 2021: Population according to age
    and sex by area, 2021-2040 \[Size: 5818 Kb\] \[Modified:
    9/30/2021\].
3.  Select Population 31 Dec (projection 2019) (Information) - WHOLE
    COUNTRY (Area) - Select the years to be projected (Year) -Select all
    (Sex) - Select all (Age), and click Show table. (In our example, it
    is 2021, 2025, 2030, 2035 and 2040)
4.  Click “pivot manual” and set Area, Age in the Rows box (Order: Area,
    Age) and set Year, Sex, Information in the Columns box (Order: Year,
    Sex, Information) and click “complete”.
5.  Click “Save the result as …” - select “Semicolon delimited with
    heading” and save.

**Step 4**: Project values for the future years.

    projection_with_knots <- PoDDyHePoProjection(testdata2040, 
                                                 m = 5, 
                                                 sep_col = "obe",
                                                 smo ~ ns(year, knots = c(2002), Boundary.knots = c(1997, 2017)) + 
                                                   sex + age + edulv + sed + marsta + obe + ns(year, knots = c(2002), Boundary.knots = c(1997, 2017)):sex + age:edulv + age:marsta +
                                                   age:obe + sex:age
                                                 obe$`Suggested Model`,
                                                 dbt$`Suggested Model`,
                                                 sed$`Suggested Model`,
                                                 highbp$`Suggested Model`,
                                                 highkol$`Suggested Model`,
                                                 edulv$`Suggested Model`,
                                                 marsta$`Suggested Model`,
                                                 area$`Suggested Model`)

In the `PoDDyHePoProjection` function, the first argument is the data
set from *Step 3*. The second argument is the number of multiple
imputations, by default this is 5, but much greater value is usually
needed in practice. The third argument is a categorical variable with
three or more levels, like in the testdata, `obe` has three levels: 0 =
Normal Weight, 1 = Overweight, 2 = Obesity. By specifying
`sep_col = "obe"`, the function calculates the proportions of each
category. The last part is for the formulas of the imputation models. In
*Step 2*, we found models for variables with missingness, and they are
saved in the `Suggested Model` in the list given by
`PoDDyHePoModelSelection`. So, it can be used in a way like `obe`:
obe$\`Suggested Model\`, or specified our own model like for `smo` in
the code above. Every variable with missingness has to be given a
formula. When there is a variable with 3 or more levels like `obe` in
the example, it has to be specified in `sep_col`, as we use Wilson
Confidence Interval in the pooling step, which is for binomial
proportions.

**Step 5**: Pool the results from the projection step and plot the
prevalences.

    # Smoking status
    smopool <- PoDDyHePoPool(projection_with_knots, "smo")
    PoDDyHePoPlot(smopool, year = 2017, "Smoking")

    # Normal Weight/Overweight/Obesity
    obepool <- PoDDyHePoPool(projection_with_knots, "obe")
    PoDDyHePoPlot(obepool, year = 2017, "Normal Weight/Overweight/Obesity", sepvarlbl = c("Normal Weight", "Overweight", "Obesity"))

    # Diabetes
    dbtpool <- PoDDyHePoPool(projection_with_knots, "dbt")
    PoDDyHePoPlot(dbtpool, year = 2017, "Diabetes")

    # Sedentary Lifestyle
    sedpool <- PoDDyHePoPool(projection_with_knots, "sed")
    PoDDyHePoPlot(sedpool, year = 2017, "Sedentary")

    # Hypertension
    highbppool <- PoDDyHePoPool(projection_with_knots, "highbp")
    PoDDyHePoPlot(highbppool, year = 2017, "Hypertension")

    # Elevated Total Cholesterol
    highkolpool <- PoDDyHePoPool(projection_with_knots, "highkol")
    PoDDyHePoPlot(highkolpool, year = 2017, "Elevated Total Cholesterol")

In the function `PoDDyHePoPool`, the first argument is the data with
projected values from *Step 4*, and the second argument is the main
variable whose prevalences we aim to estimate. In the function
`PoDDyHePoPlot`, the first argument is the result from `PoDDyHePoPool`;
the second argument is the maximum year in the observed data (In
`testdata`, it is 2017), the third argument is the title for the figure.
When there is a variable has three or more levels, an extra argument
`sepvarlbl` is available, which is to label the levels of this variable.
Otherwise, the labels for the newly created variables will be `obe_0`,
`obe_1` and `obe_2`.

Descriptive Table
-----------------

Descriptive table with sample size and amount of missingness is also
available by using `PoDDyHePoFreqtable`.

There are two arguments in this function, the first one is the observed
data (`testdata`) while the second argument is a variable name of
interest. Examples are given below.

    PoDDyHePoFreqTable(testdata, colName = "smo")
    PoDDyHePoFreqTable(testdata, colName = "obe")
