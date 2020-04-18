############ META ##############
# File name   : Suicide_Rates_Script_v2.R
# Author      : Rahma Ali
# Email       : rahma.diab@gmail.com
# Date        : Jan 2020
# Description : A script file part of a machine learning project. The code 
# aims to explore and create prediction algorithm for suicide_rates data. 
# The file is submitted in partial fulfillment of the requirements for obtaining 
# HarvardX Professional Certificate in Data Science. 
################################

################################
# Preamble
################################
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggcorrplot)) install.packages("ggcorrplot", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(countrycode)) install.packages("countrycode", repos = "http://cran.us.r-project.org")
if(!require(ranger)) install.packages("ranger", repos = "http://cran.us.r-project.org")
options(scipen=10000)

################################
# Download data file
################################
dl <- tempfile()
download.file("https://raw.githubusercontent.com/rali314/Suicide_Rates/master/master.csv", dl)
suicide <- read.csv(dl, col.names = c("country", "year", "sex", "age", "suicides_no", 
          "population", "suicide_rate", "country.year", "HDI.for.year", 
          "gdp_for_year", "gdp_per_capita", "generation"))

################################
# Feature engineering
################################
# Add continent variable
suicide$continent <- countrycode(sourcevar = suicide[, "country"],
                                 origin = "country.name",
                                 destination = "continent")

################################
# Explore the data
################################
str(suicide)

n_distinct(suicide$country)
n_distinct(suicide$year)
levels(suicide$age)

# Distribution of suicide_rate
suicide %>% 
  ggplot(aes(suicide_rate)) +
  geom_histogram(fill="deepskyblue2", color="navy") +
  labs(y="Count", x="Suicide rate per 100k") +
  ggtitle("Distribution of Suicide Rates per 100K")

# Number of observations per year  
suicide %>% group_by(year) %>%
  summarize(n=n()) %>%
  ggplot(aes(year,n)) +
  geom_point(fill="deepskyblue2", color="navy") +
  # geom_text(aes(label=country))
  scale_x_continuous(breaks = seq(1986, 2016, 2)) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(y="Observations", x="Year") +
  ggtitle("Number of Observations per Year")

# Number of observations per country  
suicide %>% group_by(country) %>%
  summarize(n=n()) %>%
  top_n(-10) %>%
  ggplot(aes(reorder(country,n), n)) +
  geom_bar(fill="deepskyblue2", color="navy", stat="identity") +
  # geom_text(aes(label=country))
  # scale_x_continuous(breaks = seq(1986, 2016, 2)) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(y="Observations", x="Year") +
  ggtitle("Number of Observations per Year")


# Explore continent effect on suicide variability
suicide %>% 
  group_by(continent) %>%
  summarize(ct_suicide_rate=sum(suicides_no)*100000/sum(population)) %>%
  ggplot(aes(reorder(continent, ct_suicide_rate), ct_suicide_rate)) +
  geom_bar(stat="identity", fill="deepskyblue2", color="navy") +
  coord_flip() +
  labs(x="Continent", y="Suicide rate per 100k population") +
  ggtitle("Suicide Rates by Continent")

suicide %>% 
  group_by(continent) %>%
  summarize(ct_suicides=mean(suicides_no)) %>%
  ggplot(aes(reorder(continent, ct_suicides), ct_suicides)) +
  geom_bar(stat="identity", fill="deepskyblue2", color="navy") +
  coord_flip() +
  labs(x="Continent", y="Suicides") +
  ggtitle("Suicides by Continent")

# Boxplot
suicide %>% 
  group_by(country, continent) %>%
  summarize(avg_suicide_rate=mean(suicide_rate)) %>%
  ggplot(aes(continent, avg_suicide_rate)) +
  geom_boxplot( fill="deepskyblue2", color="navy") +
  # coord_flip() +
  labs(x="Continent", y="Suicide reate per 100k population") +
  ggtitle("Suicide rate by Continent")

suicide %>% 
  group_by(continent) %>%
  summarize(ct_suicides=mean(suicides_no)) %>%
  ggplot(aes(reorder(continent, ct_suicides), ct_suicides)) +
  geom_bar(stat="identity", fill="deepskyblue2", color="navy") +
  coord_flip() +
  labs(x="Continent", y="Suicides") +
  ggtitle("Suicides by Continent") 

suicide %>% 
  group_by(continent) %>%
  group_by(country, continent) %>%
  summarize(avg_suicides=mean(suicides_no),
            avg_gdp=mean(gdp_per_capita)) %>%
  ggplot(aes(avg_suicides, avg_gdp, col=continent)) +
  geom_point() +
  coord_flip() +
  geom_text(data = . %>% filter(avg_suicides>2000), aes(avg_suicides, 
                                                        avg_gdp, label=country))

# Explore country effect on suicide variability
# Plotting top 25 total suicides per 100k population by country
suicide %>% 
  group_by(country) %>%
  summarize(country_suicide_rate=sum(suicides_no)*100000/sum(population)) %>%
  top_n(25) %>%
  ggplot(aes(reorder(country, country_suicide_rate), country_suicide_rate)) +
  geom_bar(stat="identity", fill="deepskyblue2", color="navy") +
  coord_flip() +
  labs(x="Country", y="Suicide rate per 100k population") +
  ggtitle("Suicide Rates by Country")

# Explore population size effect on suicide variability
suicide %>% group_by(country, year) %>%
  summarize(pop=mean(population), suicide_rate=sum(suicides_no)*100000/sum(population), pop=sum(pop)) %>% 
  ungroup() %>%
  group_by(country) %>%
  summarize(pop=sum(pop), suicide_rate=mean(suicide_rate)) %>%
  ggplot(aes(suicide_rate, pop)) +
  geom_point(fill="deepskyblue2", color="navy") +
  geom_text(data = . %>% filter(suicide_rate > 35 | pop > 400000000), 
            aes(label = country, col=country), 
            position="dodge") +
  stat_smooth(method = "lm", color = "red", size = 1) +
  theme(legend.position = "none") +
  labs(x="Suicide rate", y="Population") +
  ggtitle("Suicide Rate Variability by Population Size")


# Explore year effect on suicide variability
# Time plot
suicide %>% group_by(year) %>%
  summarize(suicide_rate=sum(suicides_no)*100000/sum(population)) %>%
  ggplot(aes(year, suicide_rate)) +
  geom_line(color="navy") +
  geom_point(color="navy", size = 2) +
  scale_x_continuous(breaks = seq(1985, 2016, 2)) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(x="Year", y="Suicide rate per 100k population") +
  ggtitle("Suicide Rates Time Plot")

# Year boxplot
suicide %>% group_by(country, year) %>%
  summarize(total_suicide=sum(suicides_no), population=sum(population)) %>%
  mutate(suicide_rate=total_suicide*100000/population) %>%
  # filter(country %in% c("Sri Lanka", "Russia", "Lithuania")) %>%
  ggplot(aes(factor(year), suicide_rate)) +
  geom_boxplot(fill="deepskyblue2") +
  geom_text(data = . %>% filter(suicide_rate>48), 
            aes(factor(year), suicide_rate, label=country)) +
  labs(x="Year", y="Suicide rate per 100k population") +
  ggtitle("Suicide Rates per 100k Population by Year") +
  theme(axis.text.x = element_text(angle = 90)) 


# Explore GDP effect on suicide variability
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


# Explore age effect on suicide variability
level_key_age <- c(`1` ="5-14 years", `2` = "15-24 years", `3` = "25-34 years", `4` = "35-54 years", `5` = "55-74 years", `6` = "75+ years")
suicide$age <- recode_factor(as.character(suicide$age), !!!level_key_age)

# Age group bar 
suicide %>% 
  group_by(age) %>%
  summarize(suicide_rate=sum(suicides_no)*100000/sum(population)) %>%
  ggplot(aes(reorder(age,suicide_rate), suicide_rate)) +
  geom_bar(stat="identity", fill="deepskyblue2", col="navy") +
  coord_flip() +
  labs(x="Age group", y="Suicide rate") +
  ggtitle("Suicide Rate by Age Group") 

# Age groups boxplot
suicide %>% group_by(age, country) %>%
  summarize(suicide_rate=sum(suicides_no)*100000/sum(population)) %>%
  ggplot(aes(age, suicide_rate)) +
  geom_boxplot(fill="deepskyblue2", col="navy") +
  labs(x="Age group", y="Suicide rate") +
  ggtitle("Suicide Rate by Age Group") +
  theme(axis.text.x = element_text(angle = 30)) 

# Explore gender effect on suicide variability
suicide %>% group_by(sex) %>%
  summarize(suicide_rate=sum(suicides_no)*100000/sum(population)) %>%
  ggplot(aes(reorder(sex, suicide_rate), suicide_rate, fill=sex)) +
  geom_histogram(stat="identity", color="navy") +
  ggtitle("Suicide Rate by Sex") +
  scale_color_manual(values = c("deepskyblue2", "navy"),
        aesthetics = c("colour", "fill")) +
  labs(x="Sex", y="Suicide rate per 100k population", fill="Sex") 

# Suicide rate trend by gender
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

# Explore HDI effect on suicide rate
# HDI is problematic. Detect percentage of missing data
sum(is.na(suicide$HDI.for.year))/nrow(suicide)

#######################
# Machine learning algorithm
#######################
# Specify explanatory and outcome variables and model formula
vars <- c("continent", "population", "country", "sex", "year", "age", "gdp_per_capita")
outcome <- "suicide_rate_log"
(fmla <- as.formula(paste(outcome, "~", paste(vars, collapse = " + "))))

############  Model Attempt1 ############  
# Variable transformation
suicide <- suicide %>%
  mutate(suicide_rate_log=log(1+suicide_rate))

# Split to training and testing datasets
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = suicide$suicide_rate_log, times = 1, 
                                  p = 0.2, list = FALSE)
train <- suicide[-test_index,]
test <- suicide[test_index,]

# Train the model
# Linear regression
lm1 <- train %>% 
  lm(fmla, data=.)

# Random forests
set.seed(1, sample.kind="Rounding")
rf1 <- ranger(fmla, # formula 
              train_trim, # data
              num.trees = 500, 
              respect.unordered.factors = "order",
              seed = 1)

# Generate predictions using the test data
# Generate predictions using the test data
test$lm <- predict(lm1, newdata = test)
test$rf <- predict(rf1, test)$predictions


# Calculate RMSE
case1 <- test %>% gather(key=model, value=log_pred, lm, rf) %>%
  mutate(pred=exp(log_pred),
         residuals=suicide_rate-pred) %>%
  group_by(model) %>%
  summarize(rmse=sqrt(mean(residuals^2)))


############  Model Attempt2 ############
######### Eliminating year 2016 ######### 
############# and Lithuania ############# 
suicide_trim <- suicide %>% 
  filter(year!=2016, country!="Lithuania")

# Split to training and testing datasets
set.seed(1, sample.kind="Rounding")
test_index_trim <- createDataPartition(y = suicide_trim$suicide_rate_log, times = 1, p = 0.2, list = FALSE)
train_trim <- suicide[-test_index_trim,]
test_trim <- suicide[test_index_trim,]

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

##########
# Compare 3 model attempts performance
##########
rmse_results <- tibble(method = c("Model1: log trans", 
                                  "Model2: log trans, 2016 trim",
                                  "Model3: log transform, 2016 and Lithuania trim"), 
                       RMSE = c(rmse, rmse_trim, rmse_trim2))
rmse_results


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

###########################
# Attempting regularization
# Penalize the least squares estimates using lambda and see
# which lambda value minimizes the RMSE
###########################
lambdas <- seq(0, 15, 0.5)
mu_hat_trim_log <- mean(train_trim_log$suicide_rate_log)

rmses_trim_log <- sapply(lambdas, function(l){
  sex_avgs <- train_trim_log %>% 
    group_by(sex) %>%
    summarize(b_s=sum(suicide_rate_log - mu_hat_trim_log)/(n()+l), n_s = n())
  
  country_avgs <- train_trim_log %>% 
    left_join(sex_avgs, by='sex') %>%
    group_by(country) %>% 
    summarize(b_c = sum(suicide_rate_log - mu_hat_trim_log - b_s)/(n()+l), n_c = n())
  
  year_avgs <- train_trim_log %>% 
    left_join(country_avgs, by='country') %>%
    left_join(sex_avgs, by='sex') %>%
    group_by(year) %>%
    summarize(b_y=sum(suicide_rate_log - mu_hat_trim_log - b_c - b_s)/(n()+l), n_y = n())
  
  age_group_avgs <- train_trim_log %>% 
    left_join(country_avgs, by='country') %>%
    left_join(sex_avgs, by='sex') %>%
    left_join(year_avgs, by='year') %>%
    group_by(age) %>%
    summarize(b_a=sum(suicide_rate_log - mu_hat_trim_log - b_c - b_s - b_y)/(n()+l), n_a = n())
  
  gdp_percapita_avgs <- train_trim_log %>%
    left_join(country_avgs, by='country') %>%
    left_join(sex_avgs, by='sex') %>%
    left_join(year_avgs, by='year') %>%
    left_join(age_group_avgs, by='age') %>%
    group_by(gdp_per_capita) %>%
    summarize(b_d=sum(suicide_rate_log - mu_hat_trim_log - b_c - b_s - b_y - b_a)/(n()+l), n_d = n())
  
  pop_avgs <- train_trim_log %>%
    left_join(country_avgs, by='country') %>%
    left_join(sex_avgs, by='sex') %>%
    left_join(year_avgs, by='year') %>%
    left_join(age_group_avgs, by='age') %>%
    left_join(gdp_percapita_avgs, by='gdp_per_capita') %>%
    group_by(gdp_per_capita) %>%
    summarize(b_p=sum(suicide_rate_log - mu_hat_trim_log - b_c - b_s - b_y - b_a - b_d)/(n()+l), n_d = n())
  
  predicted_rates <- train_trim_log %>%
    left_join(country_avgs, by='country') %>%
    left_join(sex_avgs, by='sex') %>%
    left_join(year_avgs, by='year') %>%
    left_join(age_group_avgs, by='age') %>%
    left_join(gdp_percapita_avgs, by='gdp_per_capita') %>%
    left_join(pop_avgs, by='gdp_per_capita') %>%
    mutate(pred = mu_hat_trim_log + b_c + b_s + b_y + b_a + b_d + b_p) %>%
    pull(pred)
  return(RMSE(predicted_rates, train_trim_log$suicide_rate_log))
})

qplot(lambdas, rmses_trim_log)

# Selecting lambda that minimizes the RMSE
lambda <- lambdas[which.min(rmses_trim_log)]   
lambda # 0





