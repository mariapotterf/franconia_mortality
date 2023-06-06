# RMSE calcuation

# relative RMSE

data <- data.frame(actual=rep(40,12 ), #c(40, 40, 40, 47, 48, 49, 46, 43, 42, 37, 36, 40),
                   predicted=c(37, 37, 43, 46, 46, 50, 45, 44, 43, 41, 32, 42))
data

windows()
plot(data$actual, data$predicted)

# own sqrt function
rmse = sqrt(mean((data$actual - data$predicted)^2))
target = mean(data$actual)
mean_pred = mean(data$predicted)

rmse/target # is this %??? 0.12
