# Creation date: 2020-07-05
# By: Rian Freitas da Silva
# IMPORTANT: This file is not meant to be run as a whole. It just describes the plots used in the report.


# Useful libraries for organizing and plotting data
library(tidyverse)
library(ggplot2)
library(ggridges)

# Read the data from the excel file and convert it to a dataframe
df <- read_excel("airbnb_prices_europe.xlsx")  %>%
    mutate(realSum = as.numeric(realSum))  %>% # Convert to numeric values
    mutate(guest_satisfaction_overall =
    as.numeric(guest_satisfaction_overall)) %>%
    mutate(cleanliness_rating = as.numeric(cleanliness_rating)) %>%
    mutate(dist = as.numeric(dist)) %>%
    mutate(metro_dist = as.numeric(metro_dist)) %>%
    mutate(room_type = as.factor(room_type)) %>% # Convert to factors
    mutate(host_is_superhost = as.factor(host_is_superhost)) %>%
    as.data.frame()

# Plot 1: Violin plot of the price distribution for each satisfaction quartile

# Split the satisfaction into 4 categories (quartiles)
df1 <- df %>%  select(realSum, guest_satisfaction_overall) %>% as.data.frame()
df1 <- df1 %>% filter(realSum < 4000)
df1$satisfaction <- cut_interval(df1$guest_satisfaction_overall, n = 4)

# Plotting the violin plot
p <- ggplot(df1, aes(x = realSum, y = satisfaction)) +
    geom_violin(trim = FALSE, aes(fill = as.factor(satisfaction))) +
    geom_boxplot(width = 0.1, fill = '#c1c1c1') +
    labs(x = "Preço da diária (€)", y = "Satisfação do hóspede (%)") +
    theme(legend.position="none")
p <- p + scale_fill_brewer(palette = "BuPu")

# Plot 2: Density plot of the cleanliness rating for each satisfaction quartile

# Split the satisfaction into 4 categories (quartiles)
df2 <- df %>% select(cleanliness_rating, guest_satisfaction_overall) %>% as.data.frame()
df2$satisfaction <- cut_interval(df2$guest_satisfaction_overall, n = 4)

# Plotting the density plot
p1 <- ggplot(df2, aes(x = cleanliness_rating, y = satisfaction)) +
    geom_density_ridges(aes(fill = as.factor(satisfaction))) +
    labs(x = "Limpeza (%)", y = "Satisfação do hóspede (%)") +
    theme(legend.position="none") +
    scale_fill_brewer(palette = "RdGy")

# Plot 3 and 4: Density plot of the satisfaction for each room type and superhost

# Select the columns of interest
df3 <- df %>% select(room_type, host_is_superhost, guest_satisfaction_overall) %>% as.data.frame()

# Plotting the density plot for each room type
p3 <- ggplot(df3, aes(x=guest_satisfaction_overall, y=room_type)) +
    geom_density_ridges(aes(fill = as.factor(room_type))) +
    labs(x = "Satisfação do hóspede (%)", y = "Tipo de quarto") +
    scale_fill_brewer(palette = "Set1", name = "Tipo de quarto:", labels = c("Ambiente inteiro", "Quarto privado", "Quarto compartilhado")) +
    theme(legend.position="bottom")

# Plotting the density plot whether the host is a superhost or not
p4 <- ggplot(df3, aes(x=guest_satisfaction_overall, y=host_is_superhost)) +
    geom_density_ridges(aes(fill = as.factor(host_is_superhost))) +
    labs(x = "Satisfação do hóspede (%)", y = "Superhost") +
    scale_fill_brewer(palette = "Dark2", name = "Superhost:", labels = c("Não", "Sim")) +
    theme(legend.position="bottom")

# Plot 5: Density plot of the overall satisfaction

# Plotting the density plot
p5 <- ggplot(df, aes(guest_satisfaction_overall)) +
    geom_density(fill = "#69b3a2", color = "#e9ecef", alpha = 0.8) +
    labs(x = "Satisfação do hóspede (%)", y = "Densidade") +
    theme(legend.position = "none")

# Plot 6: Boxplot of the overall satisfaction distribution for each city

# Plotting the boxplot
p9 <- ggplot(df, aes(x = city, y = guest_satisfaction_overall)) +
    geom_boxplot(fill = "#4e4e4e", color = "#980000") +
    labs(x = "Cidade", y = "Satisfação do hóspede (%)") +
    theme(legend.position = "none") +
    theme_classic()

# Plot 7: Scatter plot of the residuals of the two linear models in comparison

# Model 1: Inverse Gaussian
# Model 2: Gaussian

# Obs.: The models were calculated in the file "src/modelling.r"

# Calculate the residuals of the two models
error1 <- test$guest_satisfaction_overall - predict(model10, test)
error2 <- test$guest_satisfaction_overall - predict(model1, test)

# Create a dataframe with the residuals for the first model
aval1 <- data.frame(test$guest_satisfaction_overall, error1)
colnames(aval1) <- c("satisfaction", "error")
aval1$method <- "Inversa Gaussiana"

# Create a dataframe with the residuals for the second model
aval2 <- data.frame(test$guest_satisfaction_overall, error2)
colnames(aval2) <- c("satisfaction", "error")
aval2$method <- "Gaussiana"

# Merge the two dataframes
aval <- rbind(aval1, aval2)

# Plotting the residuals of the two models
p14 <- ggplot(aval, aes(x = satisfaction, y = error, color = method, shape = method)) +
    geom_point() +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    labs(x = "Valores ajustados", y = "Resíduos")

# Plot 8: Violin plot of the real values, the standard predictions and the maximized predictions

# Obs.: The models were calculated in the file "src/modelling.r"

# Create a dataframe with the real values and the standard predictions
real <- new_data$guest_satisfaction_overall
pred1 <- predict(model_final, new_data)

# Obs.: The arguments of the model were calculated in the file "src/optimizer.r"

# Create a dataframe with the real values and the maximized predictions
new_data$realSum <- 500.88
new_data$cleanliness_rating <- 10.0
pred2 <- predict(model_final, new_data)

# Create a dataframe with the real values, the standard predictions and the maximized predictions
real <- as.data.frame(real)
pred1 <- as.data.frame(pred1)
pred2 <- as.data.frame(pred2)
real$model <- "Real"
pred1$model <- "Predição do modelo"
pred2$model <- "Predição máxima"
colnames(real) <- c("value", "model")
colnames(pred1) <- c("value", "model")
colnames(pred2) <- c("value", "model")
pred <- rbind(real, pred1, pred2)

# Plotting the violin plot
p18 <- ggplot(pred, aes(x = value, y = model)) +
    geom_violin(color = "darkgray", aes(fill = model), alpha = 0.8) +
    geom_boxplot(width = 0.1, color = "darkgray", alpha = 0.8) +
    labs(x = "Satisfação do hóspede (%)", y = "Modelo") +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.position = "bottom")
