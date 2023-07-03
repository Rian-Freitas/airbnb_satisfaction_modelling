# Last Modified Date: 2023-07-03 15:00:00
# By: Rian Freitas da Silva
# IMPORTANT: This file is not meant to be run as a whole. It just describes the optimizer developped for the maximization problem.

# State the optimizer function
pred_max <- function (realSum, cleanliness_rating, city, host_is_superhost, guest_satisfaction_overall, metroAndCenter, isSharedRoom) {

    f <- function(realSum, cleanliness_rating) {
        return(predict(model_final, data.frame(
            realSum = realSum,
            cleanliness_rating = cleanliness_rating,
            city = city,
            host_is_superhost = host_is_superhost,
            metroAndCenter = metroAndCenter,
            isSharedRoom = isSharedRoom
            )))
        }

    # calculando os argumentos que maximizam a função
    max <- optim(par = c(0, 0), fn = function(x) -f(x[1], x[2]), lower = c(q1_price, q1_clean), upper = c(q9_price, q9_clean), method = "L-BFGS-B")$par

    return(c(max[1], max[2]))
}

# Calculate the best parameters for each row of the dataframe
df <- df %>% mutate(max = pmap(list(realSum, cleanliness_rating, city, host_is_superhost, guest_satisfaction_overall, metroAndCenter, isSharedRoom), pred_max))
