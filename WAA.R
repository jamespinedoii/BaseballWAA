if(!require(tidyverse)) install.packages("tidyverse", 
                                         repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", 
                                     repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", 
                                          repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

if(!require(RCurl)) install.packages("RCurl", 
                                          repos = "http://cran.us.r-project.org")
library(RCurl)

#Loading in the data - all team hitting data available

x3 <- getURL("https://raw.githubusercontent.com/jamespinedoii/BaseballWAA/main/WAA%20Hitting%20Data.csv")
waa_hitting <- read.csv(text = x3)

waa_hitting <- waa_hitting %>% mutate(year = waa_hitting$ï..Year) %>% 
  select(-ï..Year)

ave_w_perc <- mean(waa_hitting$WL_perc)
ave_w_perc

hitting <- waa_hitting

#Let's Add a column to show actual wins above average percentage (WAAP)
hitting <- hitting %>% mutate(WAAP = WL_perc - ave_w_perc)
which.max(hitting$WAAP)
hitting[49,]

which.min(hitting$WAAP)
hitting[10,]

#So average wins percentage is almost .5 and the best team was .4 above that
#The worst team was didn't win a game
#that's quite a spread!

#might be helpful to put this into a modern lens. A normal baseball season today
#lasts 162 games. Let's translate this percentage to a 162 game season

hitting <- hitting %>% mutate(WAA_162 = WAAP*162)

#most recent year
hitting %>% filter(year == 2021) %>% select(WAA_162, W, L, WL_perc)

#Still the same best and worst teams

which.max(hitting$WAA_162)
hitting[49,]

which.min(hitting$WAA_162)
hitting[10,]

#Let's see how accurate we can get! Using only simple counting or average stats

#first we will normalize the data
hitting <- hitting %>%
  mutate(h_g = H/G, x2b_g = X2B/G, x3b_g = X3B/G, hr_g = HR/G, rbi_g = RBI/G,
         bb_g = BB/G)

#creating test and training sets
set.seed(1, sample.kind="Rounding")
test_index_hitting <- createDataPartition(y = hitting$WAA_162, 
                                          times = 1, p = 0.1, list = FALSE)
train_hitting <- hitting[-test_index_hitting,]
test_hitting <- hitting[test_index_hitting,]

#setting a baseline
ave_y <- mean(train_hitting$WAA_162)
ave_y
naive_rmse <- sqrt(mean((ave_y - test_hitting$WAA_162)^2))
naive_rmse

#We will be about 16 games off on average if we guess the average

#start with linear regression as a baseline
#fitting a lm model to make predictions

fit <- lm(WAA_162 ~ hr_g + BA + OBP + SLG + h_g + x2b_g + x3b_g + 
            rbi_g + bb_g, data = train_hitting)

y_hat <- predict(fit, test_hitting)
y_hat

rmse_lm <- sqrt(mean((y_hat - test_hitting$WAA_162)^2))
rmse_lm

#This gives us a RMSE of 13.40354
#perhaps there are some noisy predictors? Taking away
#BB ratio below
fit <- lm(WAA_162 ~ hr_g + BA + OBP + SLG + h_g +
            x2b_g + x3b_g + rbi_g, data = train_hitting)

y_hat <- predict(fit, test_hitting)
y_hat

rmse_lm <- sqrt(mean((y_hat - test_hitting$WAA_162)^2))
rmse_lm

#RMSE of 13.40846 better to look at walks - taking away x3b ratio below

fit <- lm(WAA_162 ~ hr_g + BA + OBP + SLG + h_g + bb_g +
            x2b_g + rbi_g, data = train_hitting)

y_hat <- predict(fit, test_hitting)
y_hat

rmse_lm <- sqrt(mean((y_hat - test_hitting$WAA_162)^2))
rmse_lm

#RMSE is now 13.4261 will keep x3b in the equation. Try taking out rbi ratio:

fit <- lm(WAA_162 ~ hr_g + BA + OBP + SLG + h_g + bb_g + x3b_g +
            x2b_g, data = train_hitting)

y_hat <- predict(fit, test_hitting)
y_hat

rmse_lm <- sqrt(mean((y_hat - test_hitting$WAA_162)^2))
rmse_lm

#RMSE is 13.29213. RBI is a bit noisy in this data set. Will try to take out x2b

fit <- lm(WAA_162 ~ hr_g + BA + OBP + SLG + h_g + bb_g + x3b_g,
          data = train_hitting)

y_hat <- predict(fit, test_hitting)
y_hat

rmse_lm <- sqrt(mean((y_hat - test_hitting$WAA_162)^2))
rmse_lm

#with an RMSE 13.38123 - it is still better to keep it in. Removing hits/game

fit <- lm(WAA_162 ~ hr_g + BA + OBP + SLG + x2b_g + bb_g + x3b_g, 
          data = train_hitting)

y_hat <- predict(fit, test_hitting)
y_hat

rmse_lm <- sqrt(mean((y_hat - test_hitting$WAA_162)^2))
rmse_lm

#RMSE of 13.36721 - better to include H/G ratio. Taking away SLG here

fit <- lm(WAA_162 ~ hr_g + BA + OBP + x2b_g + bb_g + x3b_g + h_g,
          data = train_hitting)

y_hat <- predict(fit, test_hitting)
y_hat

rmse_lm <- sqrt(mean((y_hat - test_hitting$WAA_162)^2))
rmse_lm

#RMSE of 13.37954 better to look at SLG. Taking away OBP

fit <- lm(WAA_162 ~ hr_g + BA + SLG + x2b_g + bb_g + x3b_g + h_g, 
          data = train_hitting)

y_hat <- predict(fit, test_hitting)
y_hat

rmse_lm <- sqrt(mean((y_hat - test_hitting$WAA_162)^2))
rmse_lm

#RMSE of 13.3203. Better to keep OBP. Taking away BA

fit <- lm(WAA_162 ~ hr_g + OBP + SLG + x2b_g + bb_g + x3b_g + h_g, 
          data = train_hitting)

y_hat <- predict(fit, test_hitting)
y_hat

rmse_lm <- sqrt(mean((y_hat - test_hitting$WAA_162)^2))
rmse_lm

#RMSE of 13.37988. Better to look at BA. Taking away HR

fit <- lm(WAA_162 ~ BA + OBP + SLG + x2b_g + bb_g + x3b_g + h_g, 
          data = train_hitting)

y_hat <- predict(fit, test_hitting)
y_hat

rmse_lm <- sqrt(mean((y_hat - test_hitting$WAA_162)^2))
rmse_lm

#RMSE of 13.35767. Better to look at HR/G. Here is the best that LM can do:

fit <- lm(WAA_162 ~ hr_g + BA + OBP + SLG + h_g + bb_g + x3b_g +
            x2b_g, data = train_hitting)

y_hat <- predict(fit, test_hitting)
y_hat

rmse_lm <- sqrt(mean((y_hat - test_hitting$WAA_162)^2))
rmse_lm

#RMSE of 13.29213

#Lets go beyond linear regression
#Will start with KNN 

knn_fit <- knn3(WAA_162 ~ hr_g + BA + OBP + SLG + h_g + bb_g + x3b_g +
                  x2b_g, data = train_hitting)

y_hat_knn <- predict(knn_fit, test_hitting, type = "prob")
y_hat_knn

rmse_knn <- sqrt(mean((y_hat_knn - test_hitting$WAA_162)^2))
rmse_knn

#RMSE is 16.70832. Can we make KNN better by choosing a better K?
#Due to boot strapping, we will allow the function to use the entire data set.

set.seed(1)
train_knn <- train(WAA_162 ~ hr_g + BA + OBP + SLG + h_g + bb_g + x3b_g + x2b_g, 
                   method = "knn", data = hitting,
                   tuneGrid = data.frame(k = seq(5, 99, 2)))

plot_knn <- ggplot(train_knn, highlight = TRUE)
plot_knn
which.min(train_knn$results$RMSE)
train_knn$results[29,]

#RMSE of 14.60331 - not an improvement from Linear Regression

#Perhaps Random Forests will help here?
#Again using the entire data set due to random boot strapping

if(!require(randomForest)) install.packages('randomForest', 
                                     repos = "http://cran.us.r-project.org")
library(randomForest)
set.seed(1)
fit_rf <- randomForest(WAA_162 ~ hr_g + BA + OBP + SLG + h_g + bb_g + x3b_g + 
                         x2b_g, data = hitting) 

rmse_rf <- sqrt(mean((fit_rf$predicted - hitting$WAA_162)^2))
rmse_rf

#We have some improvement from KNN here - RMSE of 13.98472
#Perhaps RF can do something with noisier predictors?

set.seed(1)
fit_rf_all <- randomForest(WAA_162 ~ hr_g + BA + OBP + SLG + h_g + x2b_g + 
                             x3b_g + rbi_g + bb_g, data = hitting) 

rmse_rf <- sqrt(mean((fit_rf_all$predicted - hitting$WAA_162)^2))
rmse_rf

#RMSE of 13.66735
#Although this does not help the heart of the project, we will include year and
#number of games to the equation

set.seed(1)
fit_rf_x <- randomForest(WAA_162 ~ hr_g + BA + OBP + SLG + h_g + x2b_g + 
                             x3b_g + rbi_g + bb_g + year + G, data = hitting) 

rmse_rf <- sqrt(mean((fit_rf_x$predicted - hitting$WAA_162)^2))
rmse_rf

#As expected, this results in the lowest RMSE of 12.84503