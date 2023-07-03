# Last Modified Date: 2023-07-03 15:00:00
# By: Rian Freitas da Silva
# IMPORTANT: This file is not meant to be run as a whole. It just describes the modelling used in the report.

# Usefull libraries for modelling
library(readxl)
library(tidyverse)

# Read the data from the excel file and convert it to a dataframe
df <- read_excel("airbnb_prices_europe.xlsx")  %>%  mutate(realSum = as.numeric(realSum))  %>%
    mutate(guest_satisfaction_overall = as.numeric(guest_satisfaction_overall)) %>% 
    mutate(cleanliness_rating = as.numeric(cleanliness_rating)) %>%
    mutate(dist = as.numeric(dist)) %>%
    mutate(room_type = as.factor(room_type)) %>%
    mutate(host_is_superhost = as.factor(host_is_superhost)) %>%
    mutate(metro_dist = as.numeric(metro_dist)) %>% as.data.frame()

# Split the data into training and testing sets
set.seed(123)

train_index <- sample(seq_len(nrow(df)), 0.7 * nrow(df))
train <- df[train_index, ]

test_index <- setdiff(seq_len(nrow(df)), train_index)
test <- df[test_index, ]

# First part: choose the best family of models for the data
# # Criando o modelo
model1 <- glm(guest_satisfaction_overall ~ ., data = train, family = gaussian(link = "identity"))

model2 <- glm(guest_satisfaction_overall ~ ., data = train, family = gaussian(link = "log"))

model3 <- glm(guest_satisfaction_overall ~ ., data = train, family = gaussian(link = "inverse"))

model4 <- glm(guest_satisfaction_overall ~ ., data = train, family = Gamma(link = "inverse"))

model5 <- glm(guest_satisfaction_overall ~ ., data = train, family = Gamma(link = "log"))

model6 <- glm(guest_satisfaction_overall ~ ., data = train, family = Gamma(link = "identity"))

model7 <- glm(guest_satisfaction_overall ~ ., data = train, family = poisson(link = "log"))

model8 <- glm(guest_satisfaction_overall ~ ., data = train, family = poisson(link = "identity"))

model9 <- glm(guest_satisfaction_overall ~ ., data = train, family = poisson(link = "inverse"))

model10 <- glm(guest_satisfaction_overall ~ ., data = train, family = inverse.gaussian(link = "identity"))

# To see the overall performance of the models, we use "summary(model)"

# State the function to calculate the MSE
mse <- function(model) {
    pred <- predict(model, test)
    mean((test$guest_satisfaction_overall - pred)^2)
}

# Calculate the MSE for each model
mses <- c(mse(model1),
    mse(model2),
    mse(model3),
    mse(model4),
    mse(model5),
    mse(model6),
    mse(model7),
    mse(model8),
    mse(model9),
    mse(model10))

# As you could see in the report, the best family of models is the gaussian(link = "identity") family

# Second part: choose the combination of variables that best fits the model

# Some of the possibilities of combinations of variables explored
df__ <- df %>% as.data.frame()
df__$iter1 <- df__$realSum * df__$cleanliness_rating
df__$iter2 <- df__$realSum * df__$cleanliness_rating * df__$dist
df__$iter3 <- df__$realSum * df__$cleanliness_rating * df__$dist * df__$metro_dist
df__$iter4 <- df__$dist * df__$metro_dist
df__$iter5 <- df__$realSum * df__$cleanliness_rating * df__$dist * df__$metro_dist
df__$iter6 <- df__$realSum / df__$cleanliness_rating
df__$iter7 <- df__$dist / df__$metro_dist
df__$iter8 <- df__$realSum ** 2

# To calculate the correlation between the variables, we use "cor(df__)"
# As said in the report, the variables chosen were: realSum, cleanliness_rating, iter7, isSharedRoom, host_is_superhost and city

# Third part: final model

# Adjusting the final dataset
df_final <- df %>% as.data.frame()
df_final$metroAndCenter <- - df_final$metro_dist / df_final$dist
df_final$isSharedRoom <- ifelse(df_final$room_type == "Shared room", 1, 0)
df_final <- df_final %>% select(-c(room_type, dist, metro_dist))

# Splitting the data into training and testing sets
set.seed(123)
train_final_index <- sample(seq_len(nrow(df_final)), 0.7 * nrow(df_final))
train_final <- df_final[train_final_index, ]

test_final_index <- setdiff(seq_len(nrow(df_final)), train_final_index)
test_final <- df_final[test_final_index, ]

# Fitting the final model
model_final <- glm(guest_satisfaction_overall ~ ., data = train_final, family = gaussian(link = "identity")) 

# Calculating the MSE for the final model
mse <- mse(test_final$guest_satisfaction_overall, predict(model_final, test_final))

