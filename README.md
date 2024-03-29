---
title: "Predicting Suicide Rates"
subtitle: "Capstone Project"
author: "Rahma Ali"
date: "18/04/2020"
output:
  html_document:
    df_print: paged
    toc: true
    theme: united
  pdf_document: default
---
```{r load-packages, include=FALSE}
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggcorrplot)) install.packages("ggcorrplot", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
options(scipen=10000)
if(!require(ranger)) install.packages("ranger", repos = "http://cran.us.r-project.org")
if(!require(countrycode)) install.packages("countrycode", repos = "http://cran.us.r-project.org")
options(scipen=10000)
```

This project originally started to be submitted in partial fulfillment of the requirements for obtaining HarvardX Professional Certificate of Data Science, offered via EdX. After the course ended, I made further improvements to the analysis and decided to post it here on Kaggle to get feedback from the community and further improve my analysis.

## 1. Introduction and Project Motivation
This project aims at creating a machine learning algorithm for suicide rate prediction.  It uses country level suicide rates data from the World Health Organization and other country level indicators from The World Bank (WB) and The United Nations Development Program (UNDP), starting in 1985 to 2016. The data contains information on suicide rates per 100k population with respect to populaion age, sex, generation, country Gross Domestinc Product (GDP), population size, and Human Development Index (HDI) country score. 

At first, the data is downloaded from the web. I added the combined csv data file (as downloaded from kaggle link above) to my github repository for ease of download and code reproducibility.

```{r data_download}
# Download data file
################################
dl <- tempfile()
download.file("https://raw.githubusercontent.com/rali314/Suicide_Rates/master/master.csv", 
        dl)
suicide <- read.csv(dl, col.names = c("country", "year", "sex", "age", "suicides_no", 
        "population", "suicide_rate", "country.year", "HDI.for.year", "gdp_for_year", 
        "gdp_per_capita", "generation"))

```

## 2. Data Exploration
Now, we take an overall look on the suicide data.

```{r data_explore}
# Explore the data
################################
glimpse(suicide)
```
The `suicide` dataset in hand includes 12 varibales: 1 target variable and 11 features. The target variable is `suicide_rate`, which is the suicide rate per 100k population. The features, respictively, are: country name, year, sex, age, population size, HDI score, GDP and GDP per capita and generation.

### The Target Variable: `suicide_rate`
The distribution of the `suicide_rate` variable seems to be extremely positivly skewed to the right, with a spike at the first bin closest to the value 0. This shape suggests that the majority of the observations have a very small value of suicide rate with a small number of observations with very high values, causing a very long positive tail to the shape of the distribution. This severe skewness might suggest the use of a transformation. This topic will be discussed further later on in the report.

```{r suicide_rate_histogram, echo=FALSE, fig.align="center", fig.height=3, fig.width=5}
# Distribution of suicide_rate
suicide %>% 
  ggplot(aes(suicide_rate)) +
  geom_histogram(fill="deepskyblue2", color="navy") +
  labs(y="Count", x="Suicide rate per 100k") +
  ggtitle("Distribution of Suicide Rates per 100K")
```

### Exploring Suicide Rate Variability by Country and Continent
Suicide rate varies from one country to another. This is confirmed by the following bar chart. For easier interpretation, the figure shows the top 25 countries in terms of suicide rates. Lithuania is universally the top country in terms of suicide at a little over 40 suicides per 100k population. This rate is extremely high especially that Lithuania is not a big country like Russia, for example, which comes second after Lituania in the ranking. In fact, the average population size in Lithuania between 1985 and 2016 is `r suicide %>% filter(country=="Lithuania") %>% summarize(rate=mean(suicide_rate)) %>% round(.$rate)` compared to `r suicide %>% filter(country=="Russian Federation") %>% summarize(rate=mean(suicide_rate)) %>% round(.$rate)`. This disproportion in the case of Lithuania induces further exploration of the country populatio size.

```{r rates_by_country, echo=FALSE, fig.align="center", fig.width=5}
suicide %>% 
  group_by(country) %>%
  summarize(country_suicide_rate=sum(suicides_no)*100000/sum(population)) %>%
  top_n(25) %>%
  ggplot(aes(reorder(country, country_suicide_rate), country_suicide_rate)) +
  geom_bar(stat="identity", fill="deepskyblue2", color="navy") +
  coord_flip() +
  labs(x="Country", y="Suicide rate per 100k population") +
  ggtitle("Suicide Rates by Country")
```

Continent variable is not present in the dataset but it can easily be added using the countrycode package. 

```{r add_continent}
# Add continent variable
suicide$continent <- countrycode(sourcevar = suicide[, "country"],
                                 origin = "country.name",
                                 destination = "continent")
```

The distribution of suicide rate varies from one continent to another. The highest suicide rates are in Europe and the distribution is severly skewed to the right. We see similar skeweness in Asia and the Americas as well with the presense of a few extreme values in each continent. 
```{r rates_by_continent, echo=FALSE, fig.align="center", fig.width=5}
suicide %>% 
  group_by(country, continent) %>%
  summarize(avg_suicide_rate=mean(suicide_rate)) %>%
  ggplot(aes(continent, avg_suicide_rate)) +
  geom_boxplot( fill="deepskyblue2", color="navy") +
  # coord_flip() +
  labs(x="Continent", y="Suicide reate per 100k population") +
  ggtitle("Suicide rate by Continent")
```

### Suicide Rate Variability by Population Size
There seems to be some sort of positive linear association between the country population size and its corresponding suicide rate. Lithuania and the United States can be considered as two outliers in 2 opposite directions in the data. Presence of such extreme values affects the prediction process.

```{r rates_vs_pop, echo=FALSE, fig.align="center", fig.height=3, fig.width=5}
suicide %>% group_by(country, year) %>%
  summarize(pop=mean(population), suicide_rate=sum(suicides_no)*100000/sum(population), pop=sum(pop)) %>% 
  ungroup() %>%
  group_by(country) %>%
  summarize(pop=sum(pop), 
            suicide_rate=mean(suicide_rate)) %>%
  ggplot(aes(suicide_rate, pop)) +
  geom_point(fill="deepskyblue2", color="navy") +
  geom_text(data = . %>% filter(suicide_rate > 35 | pop > 400000000), 
            aes(label = country, col=country), 
            position="dodge") +
  stat_smooth(method = "lm", color = "red", size = 1) +
  theme(legend.position = "none") +
  labs(x="Suicide rate", y="Population") +
  ggtitle("Suicide Rate Variability by Population Size")
```

It seems that not all years included in the dataset have the same number of observations. The following plot shows that the year 2016 has the least number of observations. For this reason, year 2016 will be excluded later in the analysis
```{r n_observations, echo=FALSE, fig.align="center", fig.height=3, fig.width=5}
suicide %>% group_by(year) %>%
  mutate(n=n()) %>%
  ggplot(aes(year,n)) +
  geom_point(fill="deepskyblue2", color="navy") +
  # geom_text(aes(label=country))
  scale_x_continuous(breaks = seq(1986, 2016, 2)) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(y="Observations", x="Year") +
  ggtitle("Number of Observations per Year")
```

Suicide rates vary from one year to another. The following timeplot shows that before 1995, there was a global ascending trend of suicide. The opposite is true after 1995, as we see suicide rates decrease in a downward trend.
```{r rates_timeplot, echo=FALSE, fig.align="center", fig.height=3, fig.width=5}
suicide %>% group_by(year) %>%
  summarize(suicide_rate=sum(suicides_no)*100000/sum(population)) %>%
  ggplot(aes(year, suicide_rate)) +
  geom_line(col = "deepskyblue2") +
  geom_point(col = "deepskyblue2", size = 2) +
  scale_x_continuous(breaks = seq(1985, 2016, 2)) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(x="Year", y="Suicide rate per 100k population") +
  ggtitle("Suicide Rates Time Plot")
```

## Effect of Nation Wealth on Suicide Rates
The next plot shows suicide rates plotted against per capita GDP of the countries in the dataset. It seems that there is a positive linear correlation between suicide rates per 100k population and the country's GDP per capita. There exists some outlier values in the case of Lithuania, where we have relatively small per capita GDP and very high suicide rate (the highest as seen earlier).

```{r rates_gdp, echo=FALSE, fig.align="center", fig.height=3, fig.width=5}
suicide %>% group_by(country) %>%
  summarize(suicide_rate=sum(suicides_no)*100000/sum(population), 
            gdp_per_capita=mean(gdp_per_capita),
            pop=sum(as.numeric(population))) %>% 
  arrange(desc(gdp_per_capita)) %>%
  ggplot(aes(gdp_per_capita, suicide_rate)) +
  geom_point(fill="deepskyblue2", color="navy") +
  stat_smooth(method = "lm", color = "red", size = 1) +
  geom_text(data = . %>% filter(gdp_per_capita>64000 | 
            suicide_rate>40), aes(gdp_per_capita, 
            suicide_rate, label=country, col=country)) +
  labs(x="Average GDP per capita", y="Suicide rate per 100k population") +
  ggtitle("GDP per Capita vs. Suicide Rate") +
  theme(legend.position = "none") 
```

## Population Characteristics Variability
Now after taking an overview on suicide variability by several country and nation level variables, we move to demographic variables that characterise the populations of these countries. First, we look at suicide variability by age goup. The following plot shows that suicide rates distribution varies from one age group to another. The highest suicide rates are found in individuals aged 75+ and the lowest suicide rates are found in individuals aged between 5-14. 
```{r age_boxplot, echo=FALSE, fig.align="center", fig.height=3, fig.width=5}
level_key_age <- c(`1` ="5-14 years", `2` = "15-24 years", `3` = "25-34 years", `4` = "35-54 years", `5` = "55-74 years", `6` = "75+ years")
suicide$age <- recode_factor(as.character(suicide$age), !!!level_key_age)

suicide %>% group_by(age, country) %>%
  summarize(suicide_rate=sum(suicides_no)*100000/sum(population)) %>%
  ggplot(aes(age, suicide_rate)) +
  geom_boxplot(fill="deepskyblue2", col="navy") +
  labs(x="Age group", y="Suicide rate") +
  ggtitle("Suicide Rate by Age Group") +
  theme(axis.text.x = element_text(angle = 30)) 
```

When looking at suicide rates by sex, the data shows that suicide is more prevalent among males than females, with universal suicide rate of almost 21 versus about 6 suicides per 100k male population.

```{r sex_histogram, echo=FALSE, fig.align="center", fig.height=3, fig.width=5}
suicide %>% group_by(sex) %>%
  summarize(suicide_rate=sum(suicides_no)*100000/sum(population)) %>%
  ggplot(aes(reorder(sex, suicide_rate), suicide_rate, fill=sex)) +
  geom_histogram(stat="identity", color="navy") +
  ggtitle("Suicide Prevalence by Sex") +
  scale_color_manual(values = c("deepskyblue2", "navy"),
        aesthetics = c("colour", "fill")) +
  labs(x="Sex", y="Suicide rate per 100k population", fill="Sex") 
```
Suicide rate trends across time varies by sex as well. The following plot shows that the suicide rates for females exhibit a universal descending trend across time while the trend for males fluctuates around 1995 which, resembles the  trend we saw before.
```{r suicide_trend_bysex, echo=FALSE, fig.align="center", fig.height=3, fig.width=5}
suicide %>% group_by(year, sex) %>%
  summarize(suicide_rate=sum(suicides_no)*100000/sum(population)) %>%
  ggplot(aes(year, suicide_rate, col=sex)) +
  geom_line() +
  geom_point() +
  facet_grid(sex ~ ., scales = "free_y") +
  scale_x_continuous(breaks = seq(1985, 2016, 2)) +
  ggtitle("Suicide Rate Trends by Sex") +
  labs(x="Year", y="Suicide rate per 100k population", col="Sex") +
  scale_color_manual(values = c("deepskyblue2", "navy"),
                     aesthetics = c("colour", "fill")) +
  theme(axis.text.x = element_text(angle = 90), legend.position = "none") 
```

## 3. Method and Analysis
From the exploratory analysis performed above, two main observations emerge:

- Lithuania was found to have an extreme value for suicide rates per 100k population. 

- The year 2016 included the least numnber of observations.

- The target variable, `suicide_rate`, is severly positively skewed

Due to these factors, predicting suicide rates can be highly jeopardized if the data is used as is. Several model attempts are made and performance of each attempt is measured.

### Specifying the Model
Since the target variable, `suicide_rate` is a continuous variable, multiple linear regression algorithm is considered. A regression model is fitted that takes into account country, population size, per capita CGP, year, sex and age group effects on suicide rates per 100k population. The following model is consideres:

$log(suicide\_rate_{c,p,g,y,s,a}) = \mu + \beta_c + \beta_{ct} + \beta_p + \beta_g + \beta_y + \beta_s + \beta_a + \epsilon_{c,p,g,y,s,a}$

where $\beta_c$ is the country effect, $\beta_{ct}$ is the continent effect, $\beta_p$ is the population effect, $\beta_g$ is the per capita GDP effect, $\beta_y$ is the year effect, $\beta_s$ is the sex effect, $\beta_a$ is the age group effect and $\epsilon$ is the model error term.

In addition to linear regression, Random Forests algorithm is attempted as well. Performance of the two models is evaluated.

```{r mode_specs, echo=TRUE}
# Specify explanatory and outcome variables and model formula
vars <- c("continent", "population", "country", "sex", "year", "age", "gdp_per_capita")
outcome <- "suicide_rate_log"
(fmla <- as.formula(paste(outcome, "~", paste(vars, collapse = " + "))))
```


### Prepping the Data for the Model
Since the target variable is far from normality, a transformation is considered in order to scale the distribution to symmetry. Many transformations are discussed in the Staitstical literature for the purpose of transforming a skewed variable to a symmetric one, however, not all of them are suitable for the `suicide_rate` variable. The natural $log$ transformation produces infinte values since `suicide_rates` include zero values. For this reason, the value 1 is added to the variable prior to appling the natural $log$ transformation.

```{r data_prep, echo=TRUE}
# Variable transformation
suicide <- suicide %>%
  mutate(suicide_rate_log=log(1+suicide_rate))
```

### Create Training and Testing sets
Training and testing datasets are created. Testing set is 20% of the entire dataset.

```{r data_split, echo=TRUE}
# Split to training and testing datasets
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = suicide$suicide_rate_log, times = 1, 
            p = 0.2, list = FALSE)
train <- suicide[-test_index,]
test <- suicide[test_index,]
```

### Train the Model
#### Linear Regression
A linear regression model is fitted using the train data using the `lm` function.
```{r train_lm, echo=TRUE}
# Linear regression
lm1 <- train %>% 
  lm(fmla, data=.)
```
#### Random Forests
Fitted using the `ranger` package
```{r train_rf, echo=TRUE}
# Random forests
set.seed(1, sample.kind="Rounding")
rf1 <- ranger(fmla, # formula 
       train, # data
       num.trees = 500, 
       respect.unordered.factors = "order",
       seed = 1)
```
### Test the Model
After this, the model is applied to the test data. This is done through the `predict` function. To test the performance of the model, the Root Mean Squared Errors (RMSE) is considered. After generating the model predictions, the RMSE is calculated by comparing the model predictions against the true value of the suicide rates.

```{r test_model, include=FALSE}
# Generate predictions using the test data
test$lm <- predict(lm1, newdata = test)
test$rf <- predict(rf1, test)$predictions


# Calculate RMSE
case1 <- test %>% gather(key=model, value=log_pred, lm, rf) %>%
  mutate(pred=exp(log_pred),
         residuals=suicide_rate-pred) %>%
  group_by(model) %>%
  summarize(rmse=sqrt(mean(residuals^2)))
```
The model with the lowest RMSE in this case is random forests.

Other models are attempted with the following changes in each attempt:

- Eliminate the year 2016

- Eliminate Lithuania

Training and testing data sets are generated again to accommodate the above changes for each case and the models are refitted. Comparison between models perfomance is done through RMSE values.

```{r model_2_attempts, include=FALSE}
############  Model Attempt2 ############
suicide_trim <- suicide %>% 
  filter(year!=2016, country!="Lithuania")

# Split to training and testing datasets
set.seed(1, sample.kind="Rounding")
test_index_trim <- createDataPartition(y = suicide_trim$suicide_rate_log, times = 1, p = 0.2, list = FALSE)
train_trim <- suicide_trim[-test_index_trim,]
test_trim <- suicide_trim[test_index_trim,]

# Train the model
# Linear regression
lm2 <- train_trim %>% 
  lm(fmla, data=.)

# Random forests
set.seed(1, sample.kind="Rounding")
rf2 <- ranger(fmla, # formula 
       train_trim, # data
       num.trees = 500, 
       respect.unordered.factors = "order",
       seed = 1)

# Generate predictions using the test data
test_trim$lm <- predict(lm2, newdata = test_trim)
test_trim$rf <- predict(rf2, test_trim)$predictions

# Calculate RMSE
case2 <- test_trim %>% gather(key=model, value=log_pred, lm, rf) %>%
  mutate(pred=exp(log_pred),
         residuals=suicide_rate-pred) %>%
  group_by(model) %>%
  summarize(rmse=sqrt(mean(residuals^2)))
```


## 4. Results and Conclusions
The suicide rates per 100k populations prediction algorithm includes several country-level variables in addition to population demographic characteristics. The model used for prediction is a multiple linear regression model fitted to the training data and tested on the testing data. Random Forests model is also attempted at number of trees of 500. Models are fitted based on 2 different cases. Assessment of the perfomance is based on the value of RMSE. 

The following table summarizes the performance of each model at the 2 specified cases:

```{r model_rmse, echo=TRUE}
case1
case2
```

The Random Forests model yielded the least value of RMSE and is considered the best model for predicting suicide rates per 100k population. Eliminating data of the year 2016 and of Lithuania did not improve the value of RMSE.

The following graph shows predictions from the linear regression model and the random forests model compared to the true values of suicide rates in the test dataset for each year. The plot shows that annual predictions generated by random forests model are closer to the true value of suicide rates than those preducted by the linear regression model.

```{r predict_visual, echo=TRUE}
test %>% mutate(lm=exp(lm), rf=exp(rf)) %>%
  gather(key=valuetype, value=rate, suicide_rate, lm, rf) %>%
  mutate(suicides=rate*population/100000) %>%
  group_by(year, valuetype) %>%
  mutate(rate_year=sum(suicides)*100000/sum(population)) %>%
  ggplot(aes(year, rate_year, col=valuetype)) +
  geom_line() +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(1985, 2016, 2)) +
  theme(axis.text.x = element_text(angle = 45))
```

Additional modeling attempts were made. They are based on the concept of regularization; to penalize the least squares estimates using the parameter `lambda` to optimize for `lambda` that minimizes the RMSE. The attempt yielded value of zero for `lambda`: suggesting that no penalty for the least squares estimates of the model would further enhance the model performance. The attempt is not discussed in the report but it is included in the R script file.

There are other variables that exist in the dataset but not included in the analysis and the modeling. These variables are: 

- `gdp_for_year`, which is the GDP of the country at a given year. It is eliminated as it is highly correlated with `gdp_per_capita` to eliminate multicollinearity in the model. `gdp_per_capita` was selected over `gdp_for_year` as it is a better measure for the GDP and wealth of the nations that takes into account population size.

- `generation`, which is a categorical variable for the generation of the population. It is left out as it is highly correlated with `age`. `age` is selected over `generation` to include in the analysis as it more easily and intuitively understood.

- `HDI.for.year`, as over two thirds of the variable is missing.


## 5. Limitations and Future Work
In this project, a transformation for the target variable is introduced which is the log transformation after adding +1 to the variable. This particular transfomration and the value 1 were are selected based on convenience. Other transformations can be explored and optimized for the purpose of predicting suicide rates. 

In one of the model fitting attemps, records for Lithuania are eliminated. This is because Lithuania was identified as an outlier amongst the other countries in the data. This approach may not be the best to handle such case, especially if the suicide rate values reported for Lithuania are accurate. Further work can go into exploring methods to better understand and penalize the leverage that Lithuania imposes on the data

One aspect that I did not address in my analysis is parameter tuning for the random forests algorithm. The number of trees used for the model was set at 500. I looked up resources on optimiation of this parameter and got more confused in the process. It seems that there is no consensus on the viability of optimizing the number of tunes. As I explore optimization further, I will come back and make more edits to the analysis. More on this I found on this link (https://stats.stackexchange.com/questions/344220/how-to-tune-hyperparameters-in-a-random-forest).
