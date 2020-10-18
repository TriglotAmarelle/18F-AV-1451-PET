# libraries needed
library(caret)
library(psych)

### TOTAL CORTICAL VALUES

# read in the data
#train1 <- train
head(train)
data_ctrl <- trainControl(method = "cv", number = 3)
model_caret <- train(av1451totalcort~PTAU + REALAGE + PTGENDER,   # model to fit
                     data = train,                        
                     trControl = data_ctrl,              # folds
                     method = "lm",                      # specifying regression model
                     na.action = na.pass)                # pass missing data to model - some models will handle this
model_caret
model_caret$finalModel
model_caret$resample
mean(model_caret$resample$Rsquared)
sd(model_caret$resample$Rsquared)
summary(model_caret)

pred3 <- predict(model_caret, newdata = test)
rmse <- sqrt(sum((exp(pred3) - test$av1451totalcort)^2)/length(test$av1451totalcort))
c(RMSE = rmse, R2=summary(model_caret)$r.squared)
par(mfrow=c(1,1))
plot(test$av1451totalcort, exp(pred3))

actuals_preds <- data.frame(cbind(actuals=data$av1451totalcort, predicteds=pred3))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)
data$model_caret <- actuals_preds$predicteds

ggplot(data, aes(x = PTAU, y = av1451totalcort)) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Plot regression slope
  geom_segment(aes(xend = PTAU, yend = model_caret), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model_caret), shape = 1) +
  theme_bw()  # Add theme for cleaner look

predict(model_caret$finalModel, train, interval = "confidence")



model_caret <- train(av1451totalcort~TAU + REALAGE + PTGENDER,   # model to fit
                     data = train,                        
                     trControl = data_ctrl,              # folds
                     method = "lm",                      # specifying regression model
                     na.action = na.pass)                # pass missing data to model - some models will handle this
model_caret
model_caret$finalModel
model_caret$resample
mean(model_caret$resample$Rsquared)
sd(model_caret$resample$Rsquared)
summary(model_caret)

pred3 <- predict(model_caret, newdata = test)
rmse <- sqrt(sum((exp(pred3) - test$av1451totalcort)^2)/length(test$av1451totalcort))
c(RMSE = rmse, R2=summary(model_caret)$r.squared)
par(mfrow=c(1,1))
plot(test$av1451totalcort, exp(pred3))

actuals_preds <- data.frame(cbind(actuals=data$av1451totalcort, predicteds=pred3))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)
data$model_caret <- actuals_preds$predicteds

ggplot(data, aes(x = TAU, y = av1451totalcort)) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Plot regression slope
  geom_segment(aes(xend = TAU, yend = model_caret), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model_caret), shape = 1) +
  theme_bw()  # Add theme for cleaner look





model_caret <- train(av1451totalcort~TAU + REALAGE + PTGENDER,   # model to fit
                     data = train,                        
                     trControl = data_ctrl,              # folds
                     method = "lm",                      # specifying regression model
                     na.action = na.pass)                # pass missing data to model - some models will handle this
model_caret
model_caret$finalModel
model_caret$resample
mean(model_caret$resample$Rsquared)
sd(model_caret$resample$Rsquared)
summary(model_caret)

pred3 <- predict(model_caret, newdata = test)
rmse <- sqrt(sum((exp(pred3) - test$av1451totalcort)^2)/length(test$av1451totalcort))
c(RMSE = rmse, R2=summary(model_caret)$r.squared)
par(mfrow=c(1,1))
plot(test$av1451totalcort, exp(pred3))

actuals_preds <- data.frame(cbind(actuals=data$av1451totalcort, predicteds=pred3))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)
data$model_caret <- actuals_preds$predicteds

ggplot(data, aes(x = TAU, y = av1451totalcort)) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Plot regression slope
  geom_segment(aes(xend = TAU, yend = model_caret), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model_caret), shape = 1) +
  theme_bw()  # Add theme for cleaner look




### ENTORHINAL VALUES

# read in the data
#train1 <- train
head(train)
data_ctrl <- trainControl(method = "cv", number = 3)
model_caret <- train(av1451entorhinal~PTAU + REALAGE + PTGENDER,   # model to fit
                     data = train,                        
                     trControl = data_ctrl,              # folds
                     method = "lm",                      # specifying regression model
                     na.action = na.pass)                # pass missing data to model - some models will handle this
model_caret
model_caret$finalModel
model_caret$resample
mean(model_caret$resample$Rsquared)
sd(model_caret$resample$Rsquared)
summary(model_caret)

pred3 <- predict(model_caret, newdata = test)
rmse <- sqrt(sum((exp(pred3) - test$av1451entorhinal)^2)/length(test$av1451entorhinal))
c(RMSE = rmse, R2=summary(model_caret)$r.squared)
par(mfrow=c(1,1))
plot(test$av1451entorhinal, exp(pred3))

actuals_preds <- data.frame(cbind(actuals=data$av1451entorhinal, predicteds=pred3))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)
data$model_caret <- actuals_preds$predicteds

ggplot(data, aes(x = PTAU, y = av1451entorhinal)) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Plot regression slope
  geom_segment(aes(xend = PTAU, yend = model_caret), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model_caret), shape = 1) +
  theme_bw()  # Add theme for cleaner look


model_caret <- train(av1451entorhinal~TAU + REALAGE + PTGENDER,   # model to fit
                     data = train,                        
                     trControl = data_ctrl,              # folds
                     method = "lm",                      # specifying regression model
                     na.action = na.pass)                # pass missing data to model - some models will handle this
model_caret
model_caret$finalModel
model_caret$resample
mean(model_caret$resample$Rsquared)
sd(model_caret$resample$Rsquared)
summary(model_caret)

pred3 <- predict(model_caret, newdata = test)
rmse <- sqrt(sum((exp(pred3) - test$av1451entorhinal)^2)/length(test$av1451entorhinal))
c(RMSE = rmse, R2=summary(model_caret)$r.squared)
par(mfrow=c(1,1))
plot(test$av1451entorhinal, exp(pred3))

actuals_preds <- data.frame(cbind(actuals=data$av1451entorhinal, predicteds=pred3))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)
data$model_caret <- actuals_preds$predicteds

ggplot(data, aes(x = TAU, y = av1451entorhinal)) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Plot regression slope
  geom_segment(aes(xend = TAU, yend = model_caret), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model_caret), shape = 1) +
  theme_bw()  # Add theme for cleaner look



model_caret <- train(av1451entorhinal~TAU + REALAGE + PTGENDER,   # model to fit
                     data = train,                        
                     trControl = data_ctrl,              # folds
                     method = "lm",                      # specifying regression model
                     na.action = na.pass)                # pass missing data to model - some models will handle this
model_caret
model_caret$finalModel
model_caret$resample
mean(model_caret$resample$Rsquared)
sd(model_caret$resample$Rsquared)
summary(model_caret)

pred3 <- predict(model_caret, newdata = test)
rmse <- sqrt(sum((exp(pred3) - test$av1451entorhinal)^2)/length(test$av1451entorhinal))
c(RMSE = rmse, R2=summary(model_caret)$r.squared)
par(mfrow=c(1,1))
plot(test$av1451entorhinal, exp(pred3))

actuals_preds <- data.frame(cbind(actuals=data$av1451entorhinal, predicteds=pred3))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)
data$model_caret <- actuals_preds$predicteds

ggplot(data, aes(x = TAU, y = av1451entorhinal)) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Plot regression slope
  geom_segment(aes(xend = TAU, yend = model_caret), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model_caret), shape = 1) +
  theme_bw()  # Add theme for cleaner look







### INFERIOR TEMPORAL VALUES

# read in the data
#train1 <- train
head(train)
data_ctrl <- trainControl(method = "cv", number = 3)
model_caret <- train(av1451inferiotemporal~PTAU + REALAGE + PTGENDER,   # model to fit
                     data = train,                        
                     trControl = data_ctrl,              # folds
                     method = "lm",                      # specifying regression model
                     na.action = na.pass)                # pass missing data to model - some models will handle this
model_caret
model_caret$finalModel
model_caret$resample
mean(model_caret$resample$Rsquared)
sd(model_caret$resample$Rsquared)
summary(model_caret)

pred3 <- predict(model_caret, newdata = test)
rmse <- sqrt(sum((exp(pred3) - test$av1451inferiotemporal)^2)/length(test$av1451inferiotemporal))
c(RMSE = rmse, R2=summary(model_caret)$r.squared)
par(mfrow=c(1,1))
plot(test$av1451inferiotemporal, exp(pred3))

actuals_preds <- data.frame(cbind(actuals=data$av1451inferiotemporal, predicteds=pred3))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)
data$model_caret <- actuals_preds$predicteds

ggplot(data, aes(x = PTAU, y = av1451inferiotemporal)) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Plot regression slope
  geom_segment(aes(xend = PTAU, yend = model_caret), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model_caret), shape = 1) +
  theme_bw()  # Add theme for cleaner look


model_caret <- train(av1451inferiotemporal~TAU + REALAGE + PTGENDER,   # model to fit
                     data = train,                        
                     trControl = data_ctrl,              # folds
                     method = "lm",                      # specifying regression model
                     na.action = na.pass)                # pass missing data to model - some models will handle this
model_caret
model_caret$finalModel
model_caret$resample
mean(model_caret$resample$Rsquared)
sd(model_caret$resample$Rsquared)
summary(model_caret)

pred3 <- predict(model_caret, newdata = test)
rmse <- sqrt(sum((exp(pred3) - test$av1451inferiotemporal)^2)/length(test$av1451inferiotemporal))
c(RMSE = rmse, R2=summary(model_caret)$r.squared)
par(mfrow=c(1,1))
plot(test$av1451inferiotemporal, exp(pred3))

actuals_preds <- data.frame(cbind(actuals=data$av1451inferiotemporal, predicteds=pred3))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)
data$model_caret <- actuals_preds$predicteds

ggplot(data, aes(x = TAU, y = av1451inferiotemporal)) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Plot regression slope
  geom_segment(aes(xend = TAU, yend = model_caret), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model_caret), shape = 1) +
  theme_bw()  # Add theme for cleaner look



model_caret <- train(av1451inferiotemporal~TAU + REALAGE + PTGENDER,   # model to fit
                     data = train,                        
                     trControl = data_ctrl,              # folds
                     method = "lm",                      # specifying regression model
                     na.action = na.pass)                # pass missing data to model - some models will handle this
model_caret
model_caret$finalModel
model_caret$resample
mean(model_caret$resample$Rsquared)
sd(model_caret$resample$Rsquared)
summary(model_caret)

pred3 <- predict(model_caret, newdata = test)
rmse <- sqrt(sum((exp(pred3) - test$av1451inferiotemporal)^2)/length(test$av1451inferiotemporal))
c(RMSE = rmse, R2=summary(model_caret)$r.squared)
par(mfrow=c(1,1))
plot(test$av1451inferiotemporal, exp(pred3))

actuals_preds <- data.frame(cbind(actuals=data$av1451inferiotemporal, predicteds=pred3))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)
data$model_caret <- actuals_preds$predicteds

ggplot(data, aes(x = TAU, y = av1451inferiotemporal)) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Plot regression slope
  geom_segment(aes(xend = TAU, yend = model_caret), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model_caret), shape = 1) +
  theme_bw()  # Add theme for cleaner look
