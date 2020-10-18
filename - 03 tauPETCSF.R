library(dplyr)
library(lsr)
library(ggplot2)


## Relationship between ptau and abeta, and ptau and tau


ggplot() + geom_point(data = fulldata, mapping = aes(x = PTAU, y = ABETA)) + 
  geom_smooth(data = fulldata, aes(x = PTAU, y = ABETA))
ggplot() + geom_point(data = fulldata, aes(x = PTAU, y = ABETA, color = DX))
ggplot() + geom_point(data = fulldata, mapping = aes(x = PTAU, y = TAU)) + 
  geom_smooth(data = fulldata, aes(x = PTAU, y = TAU))
ggplot() + geom_point(data = fulldata, aes(x = PTAU, y = TAU, color = DX))



## Histograms of CSF values


ggplot(data = fulldata, aes(x = ABETA)) + geom_histogram()
ggplot(data = fulldata, aes(x = PTAU)) + geom_histogram()
ggplot(data = fulldata, aes(x = TAU)) + geom_histogram()




## Barplots comparing Gender and distribution and DX


ggplot(data = fulldata, aes(x = DX, fill = PTGENDER)) + geom_bar(position = "dodge")
ggplot(data = fulldata, aes(x = REALAGE, fill = DX)) + geom_histogram(bins = 20)



## Checking the correlations, as in Chhatwal

ggplot() + geom_point(data = fulldata, mapping = aes(x = av1451totalcort, y = ABETA)) + 
  geom_smooth(method = lm, se = FALSE, data = fulldata, aes(x = av1451totalcort, y = ABETA)) + theme_bw()
ggplot() + geom_point(data = fulldata, mapping = aes(x = av1451totalcort, y = TAU)) + 
  geom_smooth(method = lm, se = FALSE, data = fulldata, aes(x = av1451totalcort, y = TAU)) + theme_bw()
ggplot() + geom_point(data = fulldata, mapping = aes(x = av1451totalcort, y = PTAU)) + 
  geom_smooth(method = lm, se = FALSE, data = fulldata, mapping = aes(x = av1451totalcort, y = PTAU)) + theme_bw()

ggplot() + geom_point(data = fulldata, mapping = aes(x = av1451entorhinal, y = ABETA)) + 
  geom_smooth(method = lm, se = FALSE, data = fulldata, aes(x = av1451entorhinal, y = ABETA)) + theme_bw()
ggplot() + geom_point(data = fulldata, mapping = aes(x = av1451entorhinal, y = TAU)) + 
  geom_smooth(method = lm, se = FALSE, data = fulldata, aes(x = av1451entorhinal, y = TAU)) + theme_bw()
ggplot() + geom_point(data = fulldata, mapping = aes(x = av1451entorhinal, y = PTAU)) + 
  geom_smooth(method = lm, se = FALSE, data = fulldata, mapping = aes(x = av1451entorhinal, y = PTAU)) + theme_bw()

ggplot() + geom_point(data = fulldata, mapping = aes(x = av1451inferiotemporal, y = ABETA)) + 
  geom_smooth(method = lm, se = FALSE, data = fulldata, aes(x = av1451inferiotemporal, y = ABETA)) + theme_bw()
ggplot() + geom_point(data = fulldata, mapping = aes(x = av1451inferiotemporal, y = TAU)) + 
  geom_smooth(method = lm, se = FALSE, data = fulldata, aes(x = av1451inferiotemporal, y = TAU)) + theme_bw()
ggplot() + geom_point(data = fulldata, mapping = aes(x = av1451inferiotemporal, y = PTAU)) + 
  geom_smooth(method = lm, se = FALSE, data = fulldata, mapping = aes(x = av1451inferiotemporal, y = PTAU)) + theme_bw()

# just normals

data <- fulldata[grep("CN", fulldata$DX) , ]

ggplot() + geom_point(data = data, mapping = aes(x = av1451totalcort, y = ABETA)) + 
  geom_smooth(method = lm, se = FALSE, data = data, aes(x = av1451totalcort, y = ABETA)) + theme_bw()
ggplot() + geom_point(data = data, mapping = aes(x = av1451totalcort, y = TAU)) + 
  geom_smooth(method = lm, se = FALSE, data = data, aes(x = av1451totalcort, y = TAU)) + theme_bw()
ggplot() + geom_point(data = data, mapping = aes(x = av1451totalcort, y = PTAU)) + 
  geom_smooth(method = lm, se = FALSE, data = data, mapping = aes(x = av1451totalcort, y = PTAU)) + theme_bw()

set.seed(123)
row.number <- sample(1:nrow(data), 0.8*nrow(data))
train = data[row.number,]
test = data[-row.number,]

model3 = lm(av1451totalcort~ABETA + REALAGE + PTGENDER, data=train)
summary(model3)
standardCoefs( model3 )
par(mfrow=c(2,2))
plot(model3)
pred3 <- predict(model3, newdata = test)
rmse <- sqrt(sum((exp(pred3) - test$av1451totalcort)^2)/length(test$av1451totalcort))
c(RMSE = rmse, R2=summary(model3)$r.squared)
par(mfrow=c(1,1))
plot(test$av1451totalcort, exp(pred3))

actuals_preds <- data.frame(cbind(actuals=data$av1451totalcort, predicteds=pred3))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)
data$model3 <- actuals_preds$predicteds

ggplot(data, aes(x = ABETA, y = av1451totalcort)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = ABETA, yend = model3), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model3), shape = 1) +
  theme_bw()  # Add theme for cleaner look

model6 = lm(av1451totalcort~TAU + REALAGE + PTGENDER, data=train)
summary(model6)
standardCoefs( model6 )
par(mfrow=c(2,2))
plot(model6)
pred6 <- predict(model6, newdata = test)
rmse <- sqrt(sum((exp(pred6) - test$av1451totalcort)^2)/length(test$av1451totalcort))
c(RMSE = rmse, R2=summary(model6)$r.squared)
par(mfrow=c(1,1))
plot(test$av1451totalcort, exp(pred6))

actuals_preds <- data.frame(cbind(actuals=data$av1451totalcort, predicteds=pred6))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)

data$model6 <- actuals_preds$predicteds

ggplot(data, aes(x = TAU, y = av1451totalcort)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = TAU, yend = model6), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model6), shape = 1) +
  theme_bw()  # Add theme for cleaner look

model8 = lm(av1451totalcort~PTAU + REALAGE + PTGENDER, data=train)
summary(model8)
standardCoefs( model8 )
par(mfrow=c(2,2))
plot(model8)
pred8 <- predict(model8, newdata = test)
rmse <- sqrt(sum((exp(pred8) - test$av1451totalcort)^2)/length(test$av1451totalcort))
c(RMSE = rmse, R2=summary(model8)$r.squared)
par(mfrow=c(1,1))
plot(test$av1451totalcort, exp(pred8))

actuals_preds <- data.frame(cbind(actuals=data$av1451totalcort, predicteds=pred8))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)

data$model8 <- actuals_preds$predicteds

ggplot(data, aes(x = PTAU, y = av1451totalcort)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = PTAU, yend = model8), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model8), shape = 1) +
  theme_bw()  # Add theme for cleaner look


# Determine all ages
ABETA <- data$ABETA
TAU <- data$TAU
PTAU <- data$PTAU


# Determine ages for each gender separately
av1451i <- data$av1451inferiotemporal
av1451total <- data$av1451totalcort
av1451e <- data$av1451entorhinal
xy <- data.frame(cbind(ABETA,av1451total))
xy

cor.test(ABETA, av1451total)

bootCorTest <- function(data, i){
  d <- data[i, ]
  res <- cor.test(d$ABETA, d$av1451total)
  c(stat = res$statistic, p.value = res$p.value, coeff = res$estimate,
    parameter = res$parameter, method = res$method, CI = res$conf.int)
}

b <- boot(xy, bootCorTest, R = 1000)

b$t0

xy <- data.frame(cbind(TAU,av1451total))
xy

cor.test(TAU, av1451total)

bootCorTest <- function(data, i){
  d <- data[i, ]
  res <- cor.test(d$TAU, d$av1451total)
  c(stat = res$statistic, p.value = res$p.value, coeff = res$estimate,
    parameter = res$parameter, method = res$method, CI = res$conf.int)
}

b <- boot(xy, bootCorTest, R = 1000)

b$t0

xy <- data.frame(cbind(PTAU,av1451total))
xy

cor.test(PTAU, av1451total)

bootCorTest <- function(data, i){
  d <- data[i, ]
  res <- cor.test(d$PTAU, d$av1451total)
  c(stat = res$statistic, p.value = res$p.value, coeff = res$estimate,
    parameter = res$parameter, method = res$method, CI = res$conf.int)
}

b <- boot(xy, bootCorTest, R = 1000)

b$t0


# just ad

data <- fulldata[grep("AD", fulldata$DX) , ]

ggplot() + geom_point(data = data, mapping = aes(x = av1451totalcort, y = ABETA)) + 
  geom_smooth(method = lm, se = FALSE, data = data, aes(x = av1451totalcort, y = ABETA)) + theme_bw()
ggplot() + geom_point(data = data, mapping = aes(x = av1451totalcort, y = TAU)) + 
  geom_smooth(method = lm, se = FALSE, data = data, aes(x = av1451totalcort, y = TAU)) + theme_bw()
ggplot() + geom_point(data = data, mapping = aes(x = av1451totalcort, y = PTAU)) + 
  geom_smooth(method = lm, se = FALSE, data = data, mapping = aes(x = av1451totalcort, y = PTAU)) + theme_bw()


set.seed(123)
row.number <- sample(1:nrow(data), 0.8*nrow(data))
train = data[row.number,]
test = data[-row.number,]

model3 = lm(av1451totalcort~ABETA + REALAGE + PTGENDER, data=train)
summary(model3)
standardCoefs( model3 )
par(mfrow=c(2,2))
plot(model3)
pred3 <- predict(model3, newdata = test)
rmse <- sqrt(sum((exp(pred3) - test$av1451totalcort)^2)/length(test$av1451totalcort))
c(RMSE = rmse, R2=summary(model3)$r.squared)
par(mfrow=c(1,1))
plot(test$av1451totalcort, exp(pred3))

actuals_preds <- data.frame(cbind(actuals=data$av1451totalcort, predicteds=pred3))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)
data$model3 <- actuals_preds$predicteds

ggplot(data, aes(x = ABETA, y = av1451totalcort)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = ABETA, yend = model3), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model3), shape = 1) +
  theme_bw()  # Add theme for cleaner look

model6 = lm(av1451totalcort~TAU + REALAGE + PTGENDER, data=train)
summary(model6)
standardCoefs( model6 )
par(mfrow=c(2,2))
plot(model6)
pred6 <- predict(model6, newdata = test)
rmse <- sqrt(sum((exp(pred6) - test$av1451totalcort)^2)/length(test$av1451totalcort))
c(RMSE = rmse, R2=summary(model6)$r.squared)
par(mfrow=c(1,1))
plot(test$av1451totalcort, exp(pred6))

actuals_preds <- data.frame(cbind(actuals=data$av1451totalcort, predicteds=pred6))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)

data$model6 <- actuals_preds$predicteds

ggplot(data, aes(x = TAU, y = av1451totalcort)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = TAU, yend = model6), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model6), shape = 1) +
  theme_bw()  # Add theme for cleaner look

model8 = lm(av1451totalcort~PTAU + REALAGE + PTGENDER, data=train)
summary(model8)
standardCoefs( model8 )
par(mfrow=c(2,2))
plot(model8)
pred8 <- predict(model8, newdata = test)
rmse <- sqrt(sum((exp(pred8) - test$av1451totalcort)^2)/length(test$av1451totalcort))
c(RMSE = rmse, R2=summary(model8)$r.squared)
par(mfrow=c(1,1))
plot(test$av1451totalcort, exp(pred8))

actuals_preds <- data.frame(cbind(actuals=data$av1451totalcort, predicteds=pred8))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)

data$model8 <- actuals_preds$predicteds

ggplot(data, aes(x = PTAU, y = av1451totalcort)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = PTAU, yend = model8), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model8), shape = 1) +
  theme_bw()  # Add theme for cleaner look


# Determine all ages
ABETA <- data$ABETA
TAU <- data$TAU
PTAU <- data$PTAU


# Determine ages for each gender separately
av1451i <- data$av1451inferiotemporal
av1451total <- data$av1451totalcort
av1451e <- data$av1451entorhinal
xy <- data.frame(cbind(ABETA,av1451total))
xy

cor.test(ABETA, av1451total)

bootCorTest <- function(data, i){
  d <- data[i, ]
  res <- cor.test(d$ABETA, d$av1451total)
  c(stat = res$statistic, p.value = res$p.value, coeff = res$estimate,
    parameter = res$parameter, method = res$method, CI = res$conf.int)
}

b <- boot(xy, bootCorTest, R = 1000)

b$t0

xy <- data.frame(cbind(TAU,av1451total))
xy

cor.test(TAU, av1451total)

bootCorTest <- function(data, i){
  d <- data[i, ]
  res <- cor.test(d$TAU, d$av1451total)
  c(stat = res$statistic, p.value = res$p.value, coeff = res$estimate,
    parameter = res$parameter, method = res$method, CI = res$conf.int)
}

b <- boot(xy, bootCorTest, R = 1000)

b$t0

xy <- data.frame(cbind(PTAU,av1451total))
xy

cor.test(PTAU, av1451total)

bootCorTest <- function(data, i){
  d <- data[i, ]
  res <- cor.test(d$PTAU, d$av1451total)
  c(stat = res$statistic, p.value = res$p.value, coeff = res$estimate,
    parameter = res$parameter, method = res$method, CI = res$conf.int)
}

b <- boot(xy, bootCorTest, R = 1000)

b$t0


# just mci

data <- fulldata[grep("MCI", fulldata$DX) , ]

ggplot() + geom_point(data = data, mapping = aes(x = av1451totalcort, y = ABETA)) + 
  geom_smooth(method = lm, se = FALSE, data = data, aes(x = av1451totalcort, y = ABETA)) + theme_bw()
ggplot() + geom_point(data = data, mapping = aes(x = av1451totalcort, y = TAU)) + 
  geom_smooth(method = lm, se = FALSE, data = data, aes(x = av1451totalcort, y = TAU)) + theme_bw()
ggplot() + geom_point(data = data, mapping = aes(x = av1451totalcort, y = PTAU)) + 
  geom_smooth(method = lm, se = FALSE, data = data, mapping = aes(x = av1451totalcort, y = PTAU)) + theme_bw()

set.seed(123)
row.number <- sample(1:nrow(data), 0.8*nrow(data))
train = data[row.number,]
test = data[-row.number,]

model3 = lm(av1451totalcort~ABETA + REALAGE + PTGENDER, data=train)
summary(model3)
standardCoefs( model3 )
par(mfrow=c(2,2))
plot(model3)
pred3 <- predict(model3, newdata = test)
rmse <- sqrt(sum((exp(pred3) - test$av1451totalcort)^2)/length(test$av1451totalcort))
c(RMSE = rmse, R2=summary(model3)$r.squared)
par(mfrow=c(1,1))
plot(test$av1451totalcort, exp(pred3))

actuals_preds <- data.frame(cbind(actuals=data$av1451totalcort, predicteds=pred3))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)
data$model3 <- actuals_preds$predicteds

ggplot(data, aes(x = ABETA, y = av1451totalcort)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = ABETA, yend = model3), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model3), shape = 1) +
  theme_bw()  # Add theme for cleaner look

model6 = lm(av1451totalcort~TAU + REALAGE + PTGENDER, data=train)
summary(model6)
standardCoefs( model6 )
par(mfrow=c(2,2))
plot(model6)
pred6 <- predict(model6, newdata = test)
rmse <- sqrt(sum((exp(pred6) - test$av1451totalcort)^2)/length(test$av1451totalcort))
c(RMSE = rmse, R2=summary(model6)$r.squared)
par(mfrow=c(1,1))
plot(test$av1451totalcort, exp(pred6))

actuals_preds <- data.frame(cbind(actuals=data$av1451totalcort, predicteds=pred6))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)

data$model6 <- actuals_preds$predicteds

ggplot(data, aes(x = TAU, y = av1451totalcort)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = TAU, yend = model6), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model6), shape = 1) +
  theme_bw()  # Add theme for cleaner look

model8 = lm(av1451totalcort~PTAU + REALAGE + PTGENDER, data=train)
summary(model8)
standardCoefs( model8 )
par(mfrow=c(2,2))
plot(model8)
pred8 <- predict(model8, newdata = test)
rmse <- sqrt(sum((exp(pred8) - test$av1451totalcort)^2)/length(test$av1451totalcort))
c(RMSE = rmse, R2=summary(model8)$r.squared)
par(mfrow=c(1,1))
plot(test$av1451totalcort, exp(pred8))

actuals_preds <- data.frame(cbind(actuals=data$av1451totalcort, predicteds=pred8))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)

data$model8 <- actuals_preds$predicteds

ggplot(data, aes(x = PTAU, y = av1451totalcort)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = PTAU, yend = model8), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model8), shape = 1) +
  theme_bw()  # Add theme for cleaner look


# Determine all ages
ABETA <- data$ABETA
TAU <- data$TAU
PTAU <- data$PTAU


# Determine ages for each gender separately
av1451i <- data$av1451inferiotemporal
av1451total <- data$av1451totalcort
av1451e <- data$av1451entorhinal
xy <- data.frame(cbind(ABETA,av1451total))
xy

cor.test(ABETA, av1451total)

bootCorTest <- function(data, i){
  d <- data[i, ]
  res <- cor.test(d$ABETA, d$av1451total)
  c(stat = res$statistic, p.value = res$p.value, coeff = res$estimate,
    parameter = res$parameter, method = res$method, CI = res$conf.int)
}

b <- boot(xy, bootCorTest, R = 1000)

b$t0

xy <- data.frame(cbind(TAU,av1451total))
xy

cor.test(TAU, av1451total)

bootCorTest <- function(data, i){
  d <- data[i, ]
  res <- cor.test(d$TAU, d$av1451total)
  c(stat = res$statistic, p.value = res$p.value, coeff = res$estimate,
    parameter = res$parameter, method = res$method, CI = res$conf.int)
}

b <- boot(xy, bootCorTest, R = 1000)

b$t0

xy <- data.frame(cbind(PTAU,av1451total))
xy

cor.test(PTAU, av1451total)

bootCorTest <- function(data, i){
  d <- data[i, ]
  res <- cor.test(d$PTAU, d$av1451total)
  c(stat = res$statistic, p.value = res$p.value, coeff = res$estimate,
    parameter = res$parameter, method = res$method, CI = res$conf.int)
}

b <- boot(xy, bootCorTest, R = 1000)

b$t0

# just mci and ad

data <- fulldata[!grepl("CN", fulldata$DX) , ]

ggplot() + geom_point(data = data, mapping = aes(x = av1451totalcort, y = ABETA)) + 
  geom_smooth(method = lm, se = FALSE, data = data, aes(x = av1451totalcort, y = ABETA)) + theme_bw()
ggplot() + geom_point(data = data, mapping = aes(x = av1451totalcort, y = TAU)) + 
  geom_smooth(method = lm, se = FALSE, data = data, aes(x = av1451totalcort, y = TAU)) + theme_bw()
ggplot() + geom_point(data = data, mapping = aes(x = av1451totalcort, y = PTAU)) + 
  geom_smooth(method = lm, se = FALSE, data = data, mapping = aes(x = av1451totalcort, y = PTAU)) + theme_bw()

set.seed(123)
row.number <- sample(1:nrow(data), 0.8*nrow(data))
train = data[row.number,]
test = data[-row.number,]

model3 = lm(av1451totalcort~ABETA + REALAGE + PTGENDER, data=train)
summary(model3)
standardCoefs( model3 )
par(mfrow=c(2,2))
plot(model3)
pred3 <- predict(model3, newdata = test)
rmse <- sqrt(sum((exp(pred3) - test$av1451totalcort)^2)/length(test$av1451totalcort))
c(RMSE = rmse, R2=summary(model3)$r.squared)
par(mfrow=c(1,1))
plot(test$av1451totalcort, exp(pred3))

actuals_preds <- data.frame(cbind(actuals=data$av1451totalcort, predicteds=pred3))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)
data$model3 <- actuals_preds$predicteds

ggplot(data, aes(x = ABETA, y = av1451totalcort)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = ABETA, yend = model3), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model3), shape = 1) +
  theme_bw()  # Add theme for cleaner look

model6 = lm(av1451totalcort~TAU + REALAGE + PTGENDER, data=train)
summary(model6)
standardCoefs( model6 )
par(mfrow=c(2,2))
plot(model6)
pred6 <- predict(model6, newdata = test)
rmse <- sqrt(sum((exp(pred6) - test$av1451totalcort)^2)/length(test$av1451totalcort))
c(RMSE = rmse, R2=summary(model6)$r.squared)
par(mfrow=c(1,1))
plot(test$av1451totalcort, exp(pred6))

actuals_preds <- data.frame(cbind(actuals=data$av1451totalcort, predicteds=pred6))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)

data$model6 <- actuals_preds$predicteds

ggplot(data, aes(x = TAU, y = av1451totalcort)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = TAU, yend = model6), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model6), shape = 1) +
  theme_bw()  # Add theme for cleaner look

model8 = lm(av1451totalcort~PTAU + REALAGE + PTGENDER, data=train)
summary(model8)
standardCoefs( model8 )
par(mfrow=c(2,2))
plot(model8)
pred8 <- predict(model8, newdata = test)
rmse <- sqrt(sum((exp(pred8) - test$av1451totalcort)^2)/length(test$av1451totalcort))
c(RMSE = rmse, R2=summary(model8)$r.squared)
par(mfrow=c(1,1))
plot(test$av1451totalcort, exp(pred8))

actuals_preds <- data.frame(cbind(actuals=data$av1451totalcort, predicteds=pred8))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)

data$model8 <- actuals_preds$predicteds

ggplot(data, aes(x = PTAU, y = av1451totalcort)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = PTAU, yend = model8), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model8), shape = 1) +
  theme_bw()  # Add theme for cleaner look


# Determine all ages
ABETA <- data$ABETA
TAU <- data$TAU
PTAU <- data$PTAU


# Determine ages for each gender separately
av1451i <- data$av1451inferiotemporal
av1451total <- data$av1451totalcort
av1451e <- data$av1451entorhinal
xy <- data.frame(cbind(ABETA,av1451total))
xy

cor.test(ABETA, av1451total)

bootCorTest <- function(data, i){
  d <- data[i, ]
  res <- cor.test(d$ABETA, d$av1451total)
  c(stat = res$statistic, p.value = res$p.value, coeff = res$estimate,
    parameter = res$parameter, method = res$method, CI = res$conf.int)
}

b <- boot(xy, bootCorTest, R = 1000)

b$t0

xy <- data.frame(cbind(TAU,av1451total))
xy

cor.test(TAU, av1451total)

bootCorTest <- function(data, i){
  d <- data[i, ]
  res <- cor.test(d$TAU, d$av1451total)
  c(stat = res$statistic, p.value = res$p.value, coeff = res$estimate,
    parameter = res$parameter, method = res$method, CI = res$conf.int)
}

b <- boot(xy, bootCorTest, R = 1000)

b$t0

xy <- data.frame(cbind(PTAU,av1451total))
xy

cor.test(PTAU, av1451total)

bootCorTest <- function(data, i){
  d <- data[i, ]
  res <- cor.test(d$PTAU, d$av1451total)
  c(stat = res$statistic, p.value = res$p.value, coeff = res$estimate,
    parameter = res$parameter, method = res$method, CI = res$conf.int)
}

b <- boot(xy, bootCorTest, R = 1000)

b$t0

# ENTORHINAL

# ALL

ggplot() + geom_point(data = fulldata, mapping = aes(x = av1451entorhinal, y = ABETA)) + 
  geom_smooth(method = lm, se = FALSE, data = fulldata, aes(x = av1451entorhinal, y = ABETA)) + theme_bw()
ggplot() + geom_point(data = fulldata, mapping = aes(x = av1451entorhinal, y = TAU)) + 
  geom_smooth(method = lm, se = FALSE, data = fulldata, aes(x = av1451entorhinal, y = TAU)) + theme_bw()
ggplot() + geom_point(data = fulldata, mapping = aes(x = av1451entorhinal, y = PTAU)) + 
  geom_smooth(method = lm, se = FALSE, data = fulldata, mapping = aes(x = av1451entorhinal, y = PTAU)) + theme_bw()

data <- fulldata

set.seed(123)
row.number <- sample(1:nrow(data), 0.8*nrow(data))
train = data[row.number,]
test = data[-row.number,]

model3 = lm(av1451entorhinal~ABETA + REALAGE + PTGENDER, data=train)
summary(model3)
standardCoefs( model3 )
par(mfrow=c(2,2))
plot(model3)
pred3 <- predict(model3, newdata = test)
rmse <- sqrt(sum((exp(pred3) - test$av1451entorhinal)^2)/length(test$av1451entorhinal))
c(RMSE = rmse, R2=summary(model3)$r.squared)
par(mfrow=c(1,1))
plot(test$av1451entorhinal, exp(pred3))

actuals_preds <- data.frame(cbind(actuals=data$av1451entorhinal, predicteds=pred3))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)
data$model3 <- actuals_preds$predicteds

ggplot(data, aes(x = ABETA, y = av1451entorhinal)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = ABETA, yend = model3), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model3), shape = 1) +
  theme_bw()  # Add theme for cleaner look

model6 = lm(av1451entorhinal~TAU + REALAGE + PTGENDER, data=train)
summary(model6)
standardCoefs( model6 )
par(mfrow=c(2,2))
plot(model6)
pred6 <- predict(model6, newdata = test)
rmse <- sqrt(sum((exp(pred6) - test$av1451entorhinal)^2)/length(test$av1451entorhinal))
c(RMSE = rmse, R2=summary(model6)$r.squared)
par(mfrow=c(1,1))
plot(test$av1451entorhinal, exp(pred6))

actuals_preds <- data.frame(cbind(actuals=data$av1451entorhinal, predicteds=pred6))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)

data$model6 <- actuals_preds$predicteds

ggplot(data, aes(x = TAU, y = av1451entorhinal)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = TAU, yend = model6), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model6), shape = 1) +
  theme_bw()  # Add theme for cleaner look

model8 = lm(av1451entorhinal~PTAU + REALAGE + PTGENDER, data=train)
summary(model8)
standardCoefs( model8 )
par(mfrow=c(2,2))
plot(model8)
pred8 <- predict(model8, newdata = test)
rmse <- sqrt(sum((exp(pred8) - test$av1451entorhinal)^2)/length(test$av1451entorhinal))
c(RMSE = rmse, R2=summary(model8)$r.squared)
par(mfrow=c(1,1))
plot(test$av1451entorhinal, exp(pred8))

actuals_preds <- data.frame(cbind(actuals=data$av1451entorhinal, predicteds=pred8))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)

data$model8 <- actuals_preds$predicteds

ggplot(data, aes(x = PTAU, y = av1451entorhinal)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = PTAU, yend = model8), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model8), shape = 1) +
  theme_bw()  # Add theme for cleaner look

# Determine all ages
ABETA <- data$ABETA
TAU <- data$TAU
PTAU <- data$PTAU


# Determine ages for each gender separately
av1451i <- data$av1451inferiotemporal
av1451total <- data$av1451totalcort
av1451e <- data$av1451entorhinal
xy <- data.frame(cbind(ABETA,av1451e))
xy

cor.test(ABETA, av1451e)

bootCorTest <- function(data, i){
  d <- data[i, ]
  res <- cor.test(d$ABETA, d$av1451e)
  c(stat = res$statistic, p.value = res$p.value, coeff = res$estimate,
    parameter = res$parameter, method = res$method, CI = res$conf.int)
}

b <- boot(xy, bootCorTest, R = 1000)

b$t0

xy <- data.frame(cbind(TAU,av1451e))
xy

cor.test(TAU, av1451e)

bootCorTest <- function(data, i){
  d <- data[i, ]
  res <- cor.test(d$TAU, d$av1451e)
  c(stat = res$statistic, p.value = res$p.value, coeff = res$estimate,
    parameter = res$parameter, method = res$method, CI = res$conf.int)
}

b <- boot(xy, bootCorTest, R = 1000)

b$t0

xy <- data.frame(cbind(PTAU,av1451e))
xy

cor.test(PTAU, av1451e)

bootCorTest <- function(data, i){
  d <- data[i, ]
  res <- cor.test(d$PTAU, d$av1451e)
  c(stat = res$statistic, p.value = res$p.value, coeff = res$estimate,
    parameter = res$parameter, method = res$method, CI = res$conf.int)
}

b <- boot(xy, bootCorTest, R = 1000)

b$t0



# just normals

data <- fulldata[grep("CN", fulldata$DX) , ]

ggplot() + geom_point(data = data, mapping = aes(x = av1451entorhinal, y = ABETA)) + 
  geom_smooth(method = lm, se = FALSE, data = data, aes(x = av1451entorhinal, y = ABETA)) + theme_bw()
ggplot() + geom_point(data = data, mapping = aes(x = av1451entorhinal, y = TAU)) + 
  geom_smooth(method = lm, se = FALSE, data = data, aes(x = av1451entorhinal, y = TAU)) + theme_bw()
ggplot() + geom_point(data = data, mapping = aes(x = av1451entorhinal, y = PTAU)) + 
  geom_smooth(method = lm, se = FALSE, data = data, mapping = aes(x = av1451entorhinal, y = PTAU)) + theme_bw()

set.seed(123)
row.number <- sample(1:nrow(data), 0.8*nrow(data))
train = data[row.number,]
test = data[-row.number,]

model3 = lm(av1451entorhinal~ABETA + REALAGE + PTGENDER, data=train)
summary(model3)
standardCoefs( model3 )
par(mfrow=c(2,2))
plot(model3)
pred3 <- predict(model3, newdata = test)
rmse <- sqrt(sum((exp(pred3) - test$av1451entorhinal)^2)/length(test$av1451entorhinal))
c(RMSE = rmse, R2=summary(model3)$r.squared)
par(mfrow=c(1,1))
plot(test$av1451entorhinal, exp(pred3))

actuals_preds <- data.frame(cbind(actuals=data$av1451entorhinal, predicteds=pred3))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)
data$model3 <- actuals_preds$predicteds

ggplot(data, aes(x = ABETA, y = av1451entorhinal)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = ABETA, yend = model3), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model3), shape = 1) +
  theme_bw()  # Add theme for cleaner look

model6 = lm(av1451entorhinal~TAU + REALAGE + PTGENDER, data=train)
summary(model6)
standardCoefs( model6 )
par(mfrow=c(2,2))
plot(model6)
pred6 <- predict(model6, newdata = test)
rmse <- sqrt(sum((exp(pred6) - test$av1451entorhinal)^2)/length(test$av1451entorhinal))
c(RMSE = rmse, R2=summary(model6)$r.squared)
par(mfrow=c(1,1))
plot(test$av1451entorhinal, exp(pred6))

actuals_preds <- data.frame(cbind(actuals=data$av1451entorhinal, predicteds=pred6))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)

data$model6 <- actuals_preds$predicteds

ggplot(data, aes(x = TAU, y = av1451entorhinal)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = TAU, yend = model6), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model6), shape = 1) +
  theme_bw()  # Add theme for cleaner look

model8 = lm(av1451entorhinal~PTAU + REALAGE + PTGENDER, data=train)
summary(model8)
standardCoefs( model8 )
par(mfrow=c(2,2))
plot(model8)
pred8 <- predict(model8, newdata = test)
rmse <- sqrt(sum((exp(pred8) - test$av1451entorhinal)^2)/length(test$av1451entorhinal))
c(RMSE = rmse, R2=summary(model8)$r.squared)
par(mfrow=c(1,1))
plot(test$av1451entorhinal, exp(pred8))

actuals_preds <- data.frame(cbind(actuals=data$av1451entorhinal, predicteds=pred8))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)

data$model8 <- actuals_preds$predicteds

ggplot(data, aes(x = PTAU, y = av1451entorhinal)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = PTAU, yend = model8), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model8), shape = 1) +
  theme_bw()  # Add theme for cleaner look

# Determine all ages
ABETA <- data$ABETA
TAU <- data$TAU
PTAU <- data$PTAU


# Determine ages for each gender separately
av1451i <- data$av1451inferiotemporal
av1451total <- data$av1451totalcort
av1451e <- data$av1451entorhinal
xy <- data.frame(cbind(ABETA,av1451e))
xy

cor.test(ABETA, av1451e)

bootCorTest <- function(data, i){
  d <- data[i, ]
  res <- cor.test(d$ABETA, d$av1451e)
  c(stat = res$statistic, p.value = res$p.value, coeff = res$estimate,
    parameter = res$parameter, method = res$method, CI = res$conf.int)
}

b <- boot(xy, bootCorTest, R = 1000)

b$t0

xy <- data.frame(cbind(TAU,av1451e))
xy

cor.test(TAU, av1451e)

bootCorTest <- function(data, i){
  d <- data[i, ]
  res <- cor.test(d$TAU, d$av1451e)
  c(stat = res$statistic, p.value = res$p.value, coeff = res$estimate,
    parameter = res$parameter, method = res$method, CI = res$conf.int)
}

b <- boot(xy, bootCorTest, R = 1000)

b$t0

xy <- data.frame(cbind(PTAU,av1451e))
xy

cor.test(PTAU, av1451e)

bootCorTest <- function(data, i){
  d <- data[i, ]
  res <- cor.test(d$PTAU, d$av1451e)
  c(stat = res$statistic, p.value = res$p.value, coeff = res$estimate,
    parameter = res$parameter, method = res$method, CI = res$conf.int)
}

b <- boot(xy, bootCorTest, R = 1000)

b$t0


# just ad

data <- fulldata[grep("AD", fulldata$DX) , ]

ggplot() + geom_point(data = data, mapping = aes(x = av1451entorhinal, y = ABETA)) + 
  geom_smooth(method = lm, se = FALSE, data = data, aes(x = av1451entorhinal, y = ABETA)) + theme_bw()
ggplot() + geom_point(data = data, mapping = aes(x = av1451entorhinal, y = TAU)) + 
  geom_smooth(method = lm, se = FALSE, data = data, aes(x = av1451entorhinal, y = TAU)) + theme_bw()
ggplot() + geom_point(data = data, mapping = aes(x = av1451entorhinal, y = PTAU)) + 
  geom_smooth(method = lm, se = FALSE, data = data, mapping = aes(x = av1451entorhinal, y = PTAU)) + theme_bw()

set.seed(123)
row.number <- sample(1:nrow(data), 0.8*nrow(data))
train = data[row.number,]
test = data[-row.number,]

model3 = lm(av1451entorhinal~ABETA + REALAGE + PTGENDER, data=train)
summary(model3)
standardCoefs( model3 )
par(mfrow=c(2,2))
plot(model3)
pred3 <- predict(model3, newdata = test)
rmse <- sqrt(sum((exp(pred3) - test$av1451entorhinal)^2)/length(test$av1451entorhinal))
c(RMSE = rmse, R2=summary(model3)$r.squared)
par(mfrow=c(1,1))
plot(test$av1451entorhinal, exp(pred3))

actuals_preds <- data.frame(cbind(actuals=data$av1451entorhinal, predicteds=pred3))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)
data$model3 <- actuals_preds$predicteds

ggplot(data, aes(x = ABETA, y = av1451entorhinal)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = ABETA, yend = model3), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model3), shape = 1) +
  theme_bw()  # Add theme for cleaner look

model6 = lm(av1451entorhinal~TAU + REALAGE + PTGENDER, data=train)
summary(model6)
standardCoefs( model6 )
par(mfrow=c(2,2))
plot(model6)
pred6 <- predict(model6, newdata = test)
rmse <- sqrt(sum((exp(pred6) - test$av1451entorhinal)^2)/length(test$av1451entorhinal))
c(RMSE = rmse, R2=summary(model6)$r.squared)
par(mfrow=c(1,1))
plot(test$av1451entorhinal, exp(pred6))

actuals_preds <- data.frame(cbind(actuals=data$av1451entorhinal, predicteds=pred6))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)

data$model6 <- actuals_preds$predicteds

ggplot(data, aes(x = TAU, y = av1451entorhinal)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = TAU, yend = model6), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model6), shape = 1) +
  theme_bw()  # Add theme for cleaner look

model8 = lm(av1451entorhinal~PTAU + REALAGE + PTGENDER, data=train)
summary(model8)
standardCoefs( model8 )
par(mfrow=c(2,2))
plot(model8)
pred8 <- predict(model8, newdata = test)
rmse <- sqrt(sum((exp(pred8) - test$av1451entorhinal)^2)/length(test$av1451entorhinal))
c(RMSE = rmse, R2=summary(model8)$r.squared)
par(mfrow=c(1,1))
plot(test$av1451entorhinal, exp(pred8))

actuals_preds <- data.frame(cbind(actuals=data$av1451entorhinal, predicteds=pred8))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)

data$model8 <- actuals_preds$predicteds

ggplot(data, aes(x = PTAU, y = av1451entorhinal)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = PTAU, yend = model8), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model8), shape = 1) +
  theme_bw()  # Add theme for cleaner look

# Determine all ages
ABETA <- data$ABETA
TAU <- data$TAU
PTAU <- data$PTAU


# Determine ages for each gender separately
av1451i <- data$av1451inferiotemporal
av1451total <- data$av1451totalcort
av1451e <- data$av1451entorhinal
xy <- data.frame(cbind(ABETA,av1451e))
xy

cor.test(ABETA, av1451e)

bootCorTest <- function(data, i){
  d <- data[i, ]
  res <- cor.test(d$ABETA, d$av1451e)
  c(stat = res$statistic, p.value = res$p.value, coeff = res$estimate,
    parameter = res$parameter, method = res$method, CI = res$conf.int)
}

b <- boot(xy, bootCorTest, R = 1000)

b$t0

xy <- data.frame(cbind(TAU,av1451e))
xy

cor.test(TAU, av1451e)

bootCorTest <- function(data, i){
  d <- data[i, ]
  res <- cor.test(d$TAU, d$av1451e)
  c(stat = res$statistic, p.value = res$p.value, coeff = res$estimate,
    parameter = res$parameter, method = res$method, CI = res$conf.int)
}

b <- boot(xy, bootCorTest, R = 1000)

b$t0

xy <- data.frame(cbind(PTAU,av1451e))
xy

cor.test(PTAU, av1451e)

bootCorTest <- function(data, i){
  d <- data[i, ]
  res <- cor.test(d$PTAU, d$av1451e)
  c(stat = res$statistic, p.value = res$p.value, coeff = res$estimate,
    parameter = res$parameter, method = res$method, CI = res$conf.int)
}

b <- boot(xy, bootCorTest, R = 1000)

b$t0


# just mci

data <- fulldata[grep("MCI", fulldata$DX) , ]

ggplot() + geom_point(data = data, mapping = aes(x = av1451entorhinal, y = ABETA)) + 
  geom_smooth(method = lm, se = FALSE, data = data, aes(x = av1451entorhinal, y = ABETA)) + theme_bw()
ggplot() + geom_point(data = data, mapping = aes(x = av1451entorhinal, y = TAU)) + 
  geom_smooth(method = lm, se = FALSE, data = data, aes(x = av1451entorhinal, y = TAU)) + theme_bw()
ggplot() + geom_point(data = data, mapping = aes(x = av1451entorhinal, y = PTAU)) + 
  geom_smooth(method = lm, se = FALSE, data = data, mapping = aes(x = av1451entorhinal, y = PTAU)) + theme_bw()

set.seed(123)
row.number <- sample(1:nrow(data), 0.8*nrow(data))
train = data[row.number,]
test = data[-row.number,]

model3 = lm(av1451entorhinal~ABETA + REALAGE + PTGENDER, data=train)
summary(model3)
standardCoefs( model3 )
par(mfrow=c(2,2))
plot(model3)
pred3 <- predict(model3, newdata = test)
rmse <- sqrt(sum((exp(pred3) - test$av1451entorhinal)^2)/length(test$av1451entorhinal))
c(RMSE = rmse, R2=summary(model3)$r.squared)
par(mfrow=c(1,1))
plot(test$av1451entorhinal, exp(pred3))

actuals_preds <- data.frame(cbind(actuals=data$av1451entorhinal, predicteds=pred3))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)
data$model3 <- actuals_preds$predicteds

ggplot(data, aes(x = ABETA, y = av1451entorhinal)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = ABETA, yend = model3), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model3), shape = 1) +
  theme_bw()  # Add theme for cleaner look

model6 = lm(av1451entorhinal~TAU + REALAGE + PTGENDER, data=train)
summary(model6)
standardCoefs( model6 )
par(mfrow=c(2,2))
plot(model6)
pred6 <- predict(model6, newdata = test)
rmse <- sqrt(sum((exp(pred6) - test$av1451entorhinal)^2)/length(test$av1451entorhinal))
c(RMSE = rmse, R2=summary(model6)$r.squared)
par(mfrow=c(1,1))
plot(test$av1451entorhinal, exp(pred6))

actuals_preds <- data.frame(cbind(actuals=data$av1451entorhinal, predicteds=pred6))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)

data$model6 <- actuals_preds$predicteds

ggplot(data, aes(x = TAU, y = av1451entorhinal)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = TAU, yend = model6), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model6), shape = 1) +
  theme_bw()  # Add theme for cleaner look

model8 = lm(av1451entorhinal~PTAU + REALAGE + PTGENDER, data=train)
summary(model8)
standardCoefs( model8 )
par(mfrow=c(2,2))
plot(model8)
pred8 <- predict(model8, newdata = test)
rmse <- sqrt(sum((exp(pred8) - test$av1451entorhinal)^2)/length(test$av1451entorhinal))
c(RMSE = rmse, R2=summary(model8)$r.squared)
par(mfrow=c(1,1))
plot(test$av1451entorhinal, exp(pred8))

actuals_preds <- data.frame(cbind(actuals=data$av1451entorhinal, predicteds=pred8))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)

data$model8 <- actuals_preds$predicteds

ggplot(data, aes(x = PTAU, y = av1451entorhinal)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = PTAU, yend = model8), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model8), shape = 1) +
  theme_bw()  # Add theme for cleaner look

# Determine all ages
ABETA <- data$ABETA
TAU <- data$TAU
PTAU <- data$PTAU


# Determine ages for each gender separately
av1451i <- data$av1451inferiotemporal
av1451total <- data$av1451totalcort
av1451e <- data$av1451entorhinal
xy <- data.frame(cbind(ABETA,av1451e))
xy

cor.test(ABETA, av1451e)

bootCorTest <- function(data, i){
  d <- data[i, ]
  res <- cor.test(d$ABETA, d$av1451e)
  c(stat = res$statistic, p.value = res$p.value, coeff = res$estimate,
    parameter = res$parameter, method = res$method, CI = res$conf.int)
}

b <- boot(xy, bootCorTest, R = 1000)

b$t0

xy <- data.frame(cbind(TAU,av1451e))
xy

cor.test(TAU, av1451e)

bootCorTest <- function(data, i){
  d <- data[i, ]
  res <- cor.test(d$TAU, d$av1451e)
  c(stat = res$statistic, p.value = res$p.value, coeff = res$estimate,
    parameter = res$parameter, method = res$method, CI = res$conf.int)
}

b <- boot(xy, bootCorTest, R = 1000)

b$t0

xy <- data.frame(cbind(PTAU,av1451e))
xy

cor.test(PTAU, av1451e)

bootCorTest <- function(data, i){
  d <- data[i, ]
  res <- cor.test(d$PTAU, d$av1451e)
  c(stat = res$statistic, p.value = res$p.value, coeff = res$estimate,
    parameter = res$parameter, method = res$method, CI = res$conf.int)
}

b <- boot(xy, bootCorTest, R = 1000)

b$t0

# just mci and ad

data <- fulldata[!grepl("CN", fulldata$DX) , ]

ggplot() + geom_point(data = data, mapping = aes(x = av1451entorhinal, y = ABETA)) + 
  geom_smooth(method = lm, se = FALSE, data = data, aes(x = av1451entorhinal, y = ABETA)) + theme_bw()
ggplot() + geom_point(data = data, mapping = aes(x = av1451entorhinal, y = TAU)) + 
  geom_smooth(method = lm, se = FALSE, data = data, aes(x = av1451entorhinal, y = TAU)) + theme_bw()
ggplot() + geom_point(data = data, mapping = aes(x = av1451entorhinal, y = PTAU)) + 
  geom_smooth(method = lm, se = FALSE, data = data, mapping = aes(x = av1451entorhinal, y = PTAU)) + theme_bw()

set.seed(123)
row.number <- sample(1:nrow(data), 0.8*nrow(data))
train = data[row.number,]
test = data[-row.number,]

model3 = lm(av1451entorhinal~ABETA + REALAGE + PTGENDER, data=train)
summary(model3)
standardCoefs( model3 )
par(mfrow=c(2,2))
plot(model3)
pred3 <- predict(model3, newdata = test)
rmse <- sqrt(sum((exp(pred3) - test$av1451entorhinal)^2)/length(test$av1451entorhinal))
c(RMSE = rmse, R2=summary(model3)$r.squared)
par(mfrow=c(1,1))
plot(test$av1451entorhinal, exp(pred3))

actuals_preds <- data.frame(cbind(actuals=data$av1451entorhinal, predicteds=pred3))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)
data$model3 <- actuals_preds$predicteds

ggplot(data, aes(x = ABETA, y = av1451entorhinal)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = ABETA, yend = model3), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model3), shape = 1) +
  theme_bw()  # Add theme for cleaner look

model6 = lm(av1451entorhinal~TAU + REALAGE + PTGENDER, data=train)
summary(model6)
standardCoefs( model6 )
par(mfrow=c(2,2))
plot(model6)
pred6 <- predict(model6, newdata = test)
rmse <- sqrt(sum((exp(pred6) - test$av1451entorhinal)^2)/length(test$av1451entorhinal))
c(RMSE = rmse, R2=summary(model6)$r.squared)
par(mfrow=c(1,1))
plot(test$av1451entorhinal, exp(pred6))

actuals_preds <- data.frame(cbind(actuals=data$av1451entorhinal, predicteds=pred6))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)

data$model6 <- actuals_preds$predicteds

ggplot(data, aes(x = TAU, y = av1451entorhinal)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = TAU, yend = model6), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model6), shape = 1) +
  theme_bw()  # Add theme for cleaner look

model8 = lm(av1451entorhinal~PTAU + REALAGE + PTGENDER, data=train)
summary(model8)
standardCoefs( model8 )
par(mfrow=c(2,2))
plot(model8)
pred8 <- predict(model8, newdata = test)
rmse <- sqrt(sum((exp(pred8) - test$av1451entorhinal)^2)/length(test$av1451entorhinal))
c(RMSE = rmse, R2=summary(model8)$r.squared)
par(mfrow=c(1,1))
plot(test$av1451entorhinal, exp(pred8))

actuals_preds <- data.frame(cbind(actuals=data$av1451entorhinal, predicteds=pred8))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)

data$model8 <- actuals_preds$predicteds

ggplot(data, aes(x = PTAU, y = av1451entorhinal)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = PTAU, yend = model8), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model8), shape = 1) +
  theme_bw()  # Add theme for cleaner look

# Determine all ages
ABETA <- data$ABETA
TAU <- data$TAU
PTAU <- data$PTAU


# Determine ages for each gender separately
av1451i <- data$av1451inferiotemporal
av1451total <- data$av1451totalcort
av1451e <- data$av1451entorhinal
xy <- data.frame(cbind(ABETA,av1451e))
xy

cor.test(ABETA, av1451e)

bootCorTest <- function(data, i){
  d <- data[i, ]
  res <- cor.test(d$ABETA, d$av1451e)
  c(stat = res$statistic, p.value = res$p.value, coeff = res$estimate,
    parameter = res$parameter, method = res$method, CI = res$conf.int)
}

b <- boot(xy, bootCorTest, R = 1000)

b$t0

xy <- data.frame(cbind(TAU,av1451e))
xy

cor.test(TAU, av1451e)

bootCorTest <- function(data, i){
  d <- data[i, ]
  res <- cor.test(d$TAU, d$av1451e)
  c(stat = res$statistic, p.value = res$p.value, coeff = res$estimate,
    parameter = res$parameter, method = res$method, CI = res$conf.int)
}

b <- boot(xy, bootCorTest, R = 1000)

b$t0

xy <- data.frame(cbind(PTAU,av1451e))
xy

cor.test(PTAU, av1451e)

bootCorTest <- function(data, i){
  d <- data[i, ]
  res <- cor.test(d$PTAU, d$av1451e)
  c(stat = res$statistic, p.value = res$p.value, coeff = res$estimate,
    parameter = res$parameter, method = res$method, CI = res$conf.int)
}

b <- boot(xy, bootCorTest, R = 1000)

b$t0

# INFERIOR TEMPORAL

# ALL

ggplot() + geom_point(data = fulldata, mapping = aes(x = av1451inferiotemporal, y = ABETA)) + 
  geom_smooth(method = lm, se = FALSE, data = fulldata, aes(x = av1451inferiotemporal, y = ABETA)) + theme_bw()
ggplot() + geom_point(data = fulldata, mapping = aes(x = av1451inferiotemporal, y = TAU)) + 
  geom_smooth(method = lm, se = FALSE, data = fulldata, aes(x = av1451inferiotemporal, y = TAU)) + theme_bw()
ggplot() + geom_point(data = fulldata, mapping = aes(x = av1451inferiotemporal, y = PTAU)) + 
  geom_smooth(method = lm, se = FALSE, data = fulldata, mapping = aes(x = av1451inferiotemporal, y = PTAU)) + theme_bw()

data <- fulldata

set.seed(123)
row.number <- sample(1:nrow(data), 0.8*nrow(data))
train = data[row.number,]
test = data[-row.number,]

model3 = lm(av1451inferiotemporal~ABETA + REALAGE + PTGENDER, data=train)
summary(model3)
standardCoefs( model3 )
par(mfrow=c(2,2))
plot(model3)
pred3 <- predict(model3, newdata = test)
rmse <- sqrt(sum((exp(pred3) - test$av1451inferiotemporal)^2)/length(test$av1451inferiotemporal))
c(RMSE = rmse, R2=summary(model3)$r.squared)
par(mfrow=c(1,1))
plot(test$av1451inferiotemporal, exp(pred3))

actuals_preds <- data.frame(cbind(actuals=data$av1451inferiotemporal, predicteds=pred3))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)
data$model3 <- actuals_preds$predicteds

ggplot(data, aes(x = ABETA, y = av1451inferiotemporal)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = ABETA, yend = model3), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model3), shape = 1) +
  theme_bw()  # Add theme for cleaner look

model6 = lm(av1451inferiotemporal~TAU + REALAGE + PTGENDER, data=train)
summary(model6)
standardCoefs( model6 )
par(mfrow=c(2,2))
plot(model6)
pred6 <- predict(model6, newdata = test)
rmse <- sqrt(sum((exp(pred6) - test$av1451inferiotemporal)^2)/length(test$av1451inferiotemporal))
c(RMSE = rmse, R2=summary(model6)$r.squared)
par(mfrow=c(1,1))
plot(test$av1451inferiotemporal, exp(pred6))

actuals_preds <- data.frame(cbind(actuals=data$av1451inferiotemporal, predicteds=pred6))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)

data$model6 <- actuals_preds$predicteds

ggplot(data, aes(x = TAU, y = av1451inferiotemporal)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = TAU, yend = model6), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model6), shape = 1) +
  theme_bw()  # Add theme for cleaner look

model8 = lm(av1451inferiotemporal~PTAU + REALAGE + PTGENDER, data=train)
summary(model8)
standardCoefs( model8 )
par(mfrow=c(2,2))
plot(model8)
pred8 <- predict(model8, newdata = test)
rmse <- sqrt(sum((exp(pred8) - test$av1451inferiotemporal)^2)/length(test$av1451inferiotemporal))
c(RMSE = rmse, R2=summary(model8)$r.squared)
par(mfrow=c(1,1))
plot(test$av1451inferiotemporal, exp(pred8))

actuals_preds <- data.frame(cbind(actuals=data$av1451inferiotemporal, predicteds=pred8))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)

data$model8 <- actuals_preds$predicteds

ggplot(data, aes(x = PTAU, y = av1451inferiotemporal)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = PTAU, yend = model8), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model8), shape = 1) +
  theme_bw()  # Add theme for cleaner look

# Determine all ages
ABETA <- data$ABETA
TAU <- data$TAU
PTAU <- data$PTAU


# Determine ages for each gender separately
av1451i <- data$av1451inferiotemporal
av1451total <- data$av1451totalcort
av1451e <- data$av1451entorhinal
xy <- data.frame(cbind(ABETA,av1451i))
xy

cor.test(ABETA, av1451i)

bootCorTest <- function(data, i){
  d <- data[i, ]
  res <- cor.test(d$ABETA, d$av1451i)
  c(stat = res$statistic, p.value = res$p.value, coeff = res$estimate,
    parameter = res$parameter, method = res$method, CI = res$conf.int)
}

b <- boot(xy, bootCorTest, R = 1000)

b$t0

xy <- data.frame(cbind(TAU,av1451i))
xy

cor.test(TAU, av1451i)

bootCorTest <- function(data, i){
  d <- data[i, ]
  res <- cor.test(d$TAU, d$av1451i)
  c(stat = res$statistic, p.value = res$p.value, coeff = res$estimate,
    parameter = res$parameter, method = res$method, CI = res$conf.int)
}

b <- boot(xy, bootCorTest, R = 1000)

b$t0

xy <- data.frame(cbind(PTAU,av1451i))
xy

cor.test(PTAU, av1451i)

bootCorTest <- function(data, i){
  d <- data[i, ]
  res <- cor.test(d$PTAU, d$av1451i)
  c(stat = res$statistic, p.value = res$p.value, coeff = res$estimate,
    parameter = res$parameter, method = res$method, CI = res$conf.int)
}

b <- boot(xy, bootCorTest, R = 1000)

b$t0



# just normals

data <- fulldata[grep("CN", fulldata$DX) , ]

ggplot() + geom_point(data = data, mapping = aes(x = av1451inferiotemporal, y = ABETA)) + 
  geom_smooth(method = lm, se = FALSE, data = data, aes(x = av1451inferiotemporal, y = ABETA)) + theme_bw()
ggplot() + geom_point(data = data, mapping = aes(x = av1451inferiotemporal, y = TAU)) + 
  geom_smooth(method = lm, se = FALSE, data = data, aes(x = av1451inferiotemporal, y = TAU)) + theme_bw()
ggplot() + geom_point(data = data, mapping = aes(x = av1451inferiotemporal, y = PTAU)) + 
  geom_smooth(method = lm, se = FALSE, data = data, mapping = aes(x = av1451inferiotemporal, y = PTAU)) + theme_bw()

set.seed(123)
row.number <- sample(1:nrow(data), 0.8*nrow(data))
train = data[row.number,]
test = data[-row.number,]

model3 = lm(av1451inferiotemporal~ABETA + REALAGE + PTGENDER, data=train)
summary(model3)
standardCoefs( model3 )
par(mfrow=c(2,2))
plot(model3)
pred3 <- predict(model3, newdata = test)
rmse <- sqrt(sum((exp(pred3) - test$av1451inferiotemporal)^2)/length(test$av1451inferiotemporal))
c(RMSE = rmse, R2=summary(model3)$r.squared)
par(mfrow=c(1,1))
plot(test$av1451inferiotemporal, exp(pred3))

actuals_preds <- data.frame(cbind(actuals=data$av1451inferiotemporal, predicteds=pred3))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)
data$model3 <- actuals_preds$predicteds

ggplot(data, aes(x = ABETA, y = av1451inferiotemporal)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = ABETA, yend = model3), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model3), shape = 1) +
  theme_bw()  # Add theme for cleaner look

model6 = lm(av1451inferiotemporal~TAU + REALAGE + PTGENDER, data=train)
summary(model6)
standardCoefs( model6 )
par(mfrow=c(2,2))
plot(model6)
pred6 <- predict(model6, newdata = test)
rmse <- sqrt(sum((exp(pred6) - test$av1451inferiotemporal)^2)/length(test$av1451inferiotemporal))
c(RMSE = rmse, R2=summary(model6)$r.squared)
par(mfrow=c(1,1))
plot(test$av1451inferiotemporal, exp(pred6))

actuals_preds <- data.frame(cbind(actuals=data$av1451inferiotemporal, predicteds=pred6))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)

data$model6 <- actuals_preds$predicteds

ggplot(data, aes(x = TAU, y = av1451inferiotemporal)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = TAU, yend = model6), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model6), shape = 1) +
  theme_bw()  # Add theme for cleaner look

model8 = lm(av1451inferiotemporal~PTAU + REALAGE + PTGENDER, data=train)
summary(model8)
standardCoefs( model8 )
par(mfrow=c(2,2))
plot(model8)
pred8 <- predict(model8, newdata = test)
rmse <- sqrt(sum((exp(pred8) - test$av1451inferiotemporal)^2)/length(test$av1451inferiotemporal))
c(RMSE = rmse, R2=summary(model8)$r.squared)
par(mfrow=c(1,1))
plot(test$av1451inferiotemporal, exp(pred8))

actuals_preds <- data.frame(cbind(actuals=data$av1451inferiotemporal, predicteds=pred8))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)

data$model8 <- actuals_preds$predicteds

ggplot(data, aes(x = PTAU, y = av1451inferiotemporal)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = PTAU, yend = model8), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model8), shape = 1) +
  theme_bw()  # Add theme for cleaner look

# Determine all ages
ABETA <- data$ABETA
TAU <- data$TAU
PTAU <- data$PTAU


# Determine ages for each gender separately
av1451i <- data$av1451inferiotemporal
av1451total <- data$av1451totalcort
av1451e <- data$av1451entorhinal
xy <- data.frame(cbind(ABETA,av1451i))
xy

cor.test(ABETA, av1451i)

bootCorTest <- function(data, i){
  d <- data[i, ]
  res <- cor.test(d$ABETA, d$av1451i)
  c(stat = res$statistic, p.value = res$p.value, coeff = res$estimate,
    parameter = res$parameter, method = res$method, CI = res$conf.int)
}

b <- boot(xy, bootCorTest, R = 1000)

b$t0

xy <- data.frame(cbind(TAU,av1451i))
xy

cor.test(TAU, av1451i)

bootCorTest <- function(data, i){
  d <- data[i, ]
  res <- cor.test(d$TAU, d$av1451i)
  c(stat = res$statistic, p.value = res$p.value, coeff = res$estimate,
    parameter = res$parameter, method = res$method, CI = res$conf.int)
}

b <- boot(xy, bootCorTest, R = 1000)

b$t0

xy <- data.frame(cbind(PTAU,av1451i))
xy

cor.test(PTAU, av1451i)

bootCorTest <- function(data, i){
  d <- data[i, ]
  res <- cor.test(d$PTAU, d$av1451i)
  c(stat = res$statistic, p.value = res$p.value, coeff = res$estimate,
    parameter = res$parameter, method = res$method, CI = res$conf.int)
}

b <- boot(xy, bootCorTest, R = 1000)

b$t0


# just ad

data <- fulldata[grep("AD", fulldata$DX) , ]

ggplot() + geom_point(data = data, mapping = aes(x = av1451inferiotemporal, y = ABETA)) + 
  geom_smooth(method = lm, se = FALSE, data = data, aes(x = av1451inferiotemporal, y = ABETA)) + theme_bw()
ggplot() + geom_point(data = data, mapping = aes(x = av1451inferiotemporal, y = TAU)) + 
  geom_smooth(method = lm, se = FALSE, data = data, aes(x = av1451inferiotemporal, y = TAU)) + theme_bw()
ggplot() + geom_point(data = data, mapping = aes(x = av1451inferiotemporal, y = PTAU)) + 
  geom_smooth(method = lm, se = FALSE, data = data, mapping = aes(x = av1451inferiotemporal, y = PTAU)) + theme_bw()

set.seed(123)
row.number <- sample(1:nrow(data), 0.8*nrow(data))
train = data[row.number,]
test = data[-row.number,]

model3 = lm(av1451inferiotemporal~ABETA + REALAGE + PTGENDER, data=train)
summary(model3)
standardCoefs( model3 )
par(mfrow=c(2,2))
plot(model3)
pred3 <- predict(model3, newdata = test)
rmse <- sqrt(sum((exp(pred3) - test$av1451inferiotemporal)^2)/length(test$av1451inferiotemporal))
c(RMSE = rmse, R2=summary(model3)$r.squared)
par(mfrow=c(1,1))
plot(test$av1451inferiotemporal, exp(pred3))

actuals_preds <- data.frame(cbind(actuals=data$av1451inferiotemporal, predicteds=pred3))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)
data$model3 <- actuals_preds$predicteds

ggplot(data, aes(x = ABETA, y = av1451inferiotemporal)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = ABETA, yend = model3), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model3), shape = 1) +
  theme_bw()  # Add theme for cleaner look

model6 = lm(av1451inferiotemporal~TAU + REALAGE + PTGENDER, data=train)
summary(model6)
standardCoefs( model6 )
par(mfrow=c(2,2))
plot(model6)
pred6 <- predict(model6, newdata = test)
rmse <- sqrt(sum((exp(pred6) - test$av1451inferiotemporal)^2)/length(test$av1451inferiotemporal))
c(RMSE = rmse, R2=summary(model6)$r.squared)
par(mfrow=c(1,1))
plot(test$av1451inferiotemporal, exp(pred6))

actuals_preds <- data.frame(cbind(actuals=data$av1451inferiotemporal, predicteds=pred6))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)

data$model6 <- actuals_preds$predicteds

ggplot(data, aes(x = TAU, y = av1451inferiotemporal)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = TAU, yend = model6), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model6), shape = 1) +
  theme_bw()  # Add theme for cleaner look

model8 = lm(av1451inferiotemporal~PTAU + REALAGE + PTGENDER, data=train)
summary(model8)
standardCoefs( model8 )
par(mfrow=c(2,2))
plot(model8)
pred8 <- predict(model8, newdata = test)
rmse <- sqrt(sum((exp(pred8) - test$av1451inferiotemporal)^2)/length(test$av1451inferiotemporal))
c(RMSE = rmse, R2=summary(model8)$r.squared)
par(mfrow=c(1,1))
plot(test$av1451inferiotemporal, exp(pred8))

actuals_preds <- data.frame(cbind(actuals=data$av1451inferiotemporal, predicteds=pred8))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)

data$model8 <- actuals_preds$predicteds

ggplot(data, aes(x = PTAU, y = av1451inferiotemporal)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = PTAU, yend = model8), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model8), shape = 1) +
  theme_bw()  # Add theme for cleaner look

# Determine all ages
ABETA <- data$ABETA
TAU <- data$TAU
PTAU <- data$PTAU


# Determine ages for each gender separately
av1451i <- data$av1451inferiotemporal
av1451total <- data$av1451totalcort
av1451e <- data$av1451entorhinal
xy <- data.frame(cbind(ABETA,av1451i))
xy

cor.test(ABETA, av1451i)

bootCorTest <- function(data, i){
  d <- data[i, ]
  res <- cor.test(d$ABETA, d$av1451i)
  c(stat = res$statistic, p.value = res$p.value, coeff = res$estimate,
    parameter = res$parameter, method = res$method, CI = res$conf.int)
}

b <- boot(xy, bootCorTest, R = 1000)

b$t0

xy <- data.frame(cbind(TAU,av1451i))
xy

cor.test(TAU, av1451i)

bootCorTest <- function(data, i){
  d <- data[i, ]
  res <- cor.test(d$TAU, d$av1451i)
  c(stat = res$statistic, p.value = res$p.value, coeff = res$estimate,
    parameter = res$parameter, method = res$method, CI = res$conf.int)
}

b <- boot(xy, bootCorTest, R = 1000)

b$t0

xy <- data.frame(cbind(PTAU,av1451i))
xy

cor.test(PTAU, av1451i)

bootCorTest <- function(data, i){
  d <- data[i, ]
  res <- cor.test(d$PTAU, d$av1451i)
  c(stat = res$statistic, p.value = res$p.value, coeff = res$estimate,
    parameter = res$parameter, method = res$method, CI = res$conf.int)
}

b <- boot(xy, bootCorTest, R = 1000)

b$t0


# just mci

data <- fulldata[grep("MCI", fulldata$DX) , ]

ggplot() + geom_point(data = data, mapping = aes(x = av1451inferiotemporal, y = ABETA)) + 
  geom_smooth(method = lm, se = FALSE, data = data, aes(x = av1451inferiotemporal, y = ABETA)) + theme_bw()
ggplot() + geom_point(data = data, mapping = aes(x = av1451inferiotemporal, y = TAU)) + 
  geom_smooth(method = lm, se = FALSE, data = data, aes(x = av1451inferiotemporal, y = TAU)) + theme_bw()
ggplot() + geom_point(data = data, mapping = aes(x = av1451inferiotemporal, y = PTAU)) + 
  geom_smooth(method = lm, se = FALSE, data = data, mapping = aes(x = av1451inferiotemporal, y = PTAU)) + theme_bw()

set.seed(123)
row.number <- sample(1:nrow(data), 0.8*nrow(data))
train = data[row.number,]
test = data[-row.number,]

model3 = lm(av1451inferiotemporal~ABETA + REALAGE + PTGENDER, data=train)
summary(model3)
standardCoefs( model3 )
par(mfrow=c(2,2))
plot(model3)
pred3 <- predict(model3, newdata = test)
rmse <- sqrt(sum((exp(pred3) - test$av1451inferiotemporal)^2)/length(test$av1451inferiotemporal))
c(RMSE = rmse, R2=summary(model3)$r.squared)
par(mfrow=c(1,1))
plot(test$av1451inferiotemporal, exp(pred3))

actuals_preds <- data.frame(cbind(actuals=data$av1451inferiotemporal, predicteds=pred3))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)
data$model3 <- actuals_preds$predicteds

ggplot(data, aes(x = ABETA, y = av1451inferiotemporal)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = ABETA, yend = model3), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model3), shape = 1) +
  theme_bw()  # Add theme for cleaner look

model6 = lm(av1451inferiotemporal~TAU + REALAGE + PTGENDER, data=train)
summary(model6)
standardCoefs( model6 )
par(mfrow=c(2,2))
plot(model6)
pred6 <- predict(model6, newdata = test)
rmse <- sqrt(sum((exp(pred6) - test$av1451inferiotemporal)^2)/length(test$av1451inferiotemporal))
c(RMSE = rmse, R2=summary(model6)$r.squared)
par(mfrow=c(1,1))
plot(test$av1451inferiotemporal, exp(pred6))

actuals_preds <- data.frame(cbind(actuals=data$av1451inferiotemporal, predicteds=pred6))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)

data$model6 <- actuals_preds$predicteds

ggplot(data, aes(x = TAU, y = av1451inferiotemporal)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = TAU, yend = model6), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model6), shape = 1) +
  theme_bw()  # Add theme for cleaner look

model8 = lm(av1451inferiotemporal~PTAU + REALAGE + PTGENDER, data=train)
summary(model8)
standardCoefs( model8 )
par(mfrow=c(2,2))
plot(model8)
pred8 <- predict(model8, newdata = test)
rmse <- sqrt(sum((exp(pred8) - test$av1451inferiotemporal)^2)/length(test$av1451inferiotemporal))
c(RMSE = rmse, R2=summary(model8)$r.squared)
par(mfrow=c(1,1))
plot(test$av1451inferiotemporal, exp(pred8))

actuals_preds <- data.frame(cbind(actuals=data$av1451inferiotemporal, predicteds=pred8))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)

data$model8 <- actuals_preds$predicteds

ggplot(data, aes(x = PTAU, y = av1451inferiotemporal)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = PTAU, yend = model8), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model8), shape = 1) +
  theme_bw()  # Add theme for cleaner look

# Determine all ages
ABETA <- data$ABETA
TAU <- data$TAU
PTAU <- data$PTAU


# Determine ages for each gender separately
av1451i <- data$av1451inferiotemporal
av1451total <- data$av1451totalcort
av1451e <- data$av1451entorhinal
xy <- data.frame(cbind(ABETA,av1451i))
xy

cor.test(ABETA, av1451i)

bootCorTest <- function(data, i){
  d <- data[i, ]
  res <- cor.test(d$ABETA, d$av1451i)
  c(stat = res$statistic, p.value = res$p.value, coeff = res$estimate,
    parameter = res$parameter, method = res$method, CI = res$conf.int)
}

b <- boot(xy, bootCorTest, R = 1000)

b$t0

xy <- data.frame(cbind(TAU,av1451i))
xy

cor.test(TAU, av1451i)

bootCorTest <- function(data, i){
  d <- data[i, ]
  res <- cor.test(d$TAU, d$av1451i)
  c(stat = res$statistic, p.value = res$p.value, coeff = res$estimate,
    parameter = res$parameter, method = res$method, CI = res$conf.int)
}

b <- boot(xy, bootCorTest, R = 1000)

b$t0

xy <- data.frame(cbind(PTAU,av1451i))
xy

cor.test(PTAU, av1451i)

bootCorTest <- function(data, i){
  d <- data[i, ]
  res <- cor.test(d$PTAU, d$av1451i)
  c(stat = res$statistic, p.value = res$p.value, coeff = res$estimate,
    parameter = res$parameter, method = res$method, CI = res$conf.int)
}

b <- boot(xy, bootCorTest, R = 1000)

b$t0

# just mci and ad

data <- fulldata[!grepl("CN", fulldata$DX) , ]

ggplot() + geom_point(data = data, mapping = aes(x = av1451inferiotemporal, y = ABETA)) + 
  geom_smooth(method = lm, se = FALSE, data = data, aes(x = av1451inferiotemporal, y = ABETA)) + theme_bw()
ggplot() + geom_point(data = data, mapping = aes(x = av1451inferiotemporal, y = TAU)) + 
  geom_smooth(method = lm, se = FALSE, data = data, aes(x = av1451inferiotemporal, y = TAU)) + theme_bw()
ggplot() + geom_point(data = data, mapping = aes(x = av1451inferiotemporal, y = PTAU)) + 
  geom_smooth(method = lm, se = FALSE, data = data, mapping = aes(x = av1451inferiotemporal, y = PTAU)) + theme_bw()

set.seed(123)
row.number <- sample(1:nrow(data), 0.8*nrow(data))
train = data[row.number,]
test = data[-row.number,]

model3 = lm(av1451inferiotemporal~ABETA + REALAGE + PTGENDER, data=train)
summary(model3)
standardCoefs( model3 )
par(mfrow=c(2,2))
plot(model3)
pred3 <- predict(model3, newdata = test)
rmse <- sqrt(sum((exp(pred3) - test$av1451inferiotemporal)^2)/length(test$av1451inferiotemporal))
c(RMSE = rmse, R2=summary(model3)$r.squared)
par(mfrow=c(1,1))
plot(test$av1451inferiotemporal, exp(pred3))

actuals_preds <- data.frame(cbind(actuals=data$av1451inferiotemporal, predicteds=pred3))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)
data$model3 <- actuals_preds$predicteds

ggplot(data, aes(x = ABETA, y = av1451inferiotemporal)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = ABETA, yend = model3), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model3), shape = 1) +
  theme_bw()  # Add theme for cleaner look

model6 = lm(av1451inferiotemporal~TAU + REALAGE + PTGENDER, data=train)
summary(model6)
standardCoefs( model6 )
par(mfrow=c(2,2))
plot(model6)
pred6 <- predict(model6, newdata = test)
rmse <- sqrt(sum((exp(pred6) - test$av1451inferiotemporal)^2)/length(test$av1451inferiotemporal))
c(RMSE = rmse, R2=summary(model6)$r.squared)
par(mfrow=c(1,1))
plot(test$av1451inferiotemporal, exp(pred6))

actuals_preds <- data.frame(cbind(actuals=data$av1451inferiotemporal, predicteds=pred6))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)

data$model6 <- actuals_preds$predicteds

ggplot(data, aes(x = TAU, y = av1451inferiotemporal)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = TAU, yend = model6), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model6), shape = 1) +
  theme_bw()  # Add theme for cleaner look

model8 = lm(av1451inferiotemporal~PTAU + REALAGE + PTGENDER, data=train)
summary(model8)
standardCoefs( model8 )
par(mfrow=c(2,2))
plot(model8)
pred8 <- predict(model8, newdata = test)
rmse <- sqrt(sum((exp(pred8) - test$av1451inferiotemporal)^2)/length(test$av1451inferiotemporal))
c(RMSE = rmse, R2=summary(model8)$r.squared)
par(mfrow=c(1,1))
plot(test$av1451inferiotemporal, exp(pred8))

actuals_preds <- data.frame(cbind(actuals=data$av1451inferiotemporal, predicteds=pred8))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)

data$model8 <- actuals_preds$predicteds

ggplot(data, aes(x = PTAU, y = av1451inferiotemporal)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = PTAU, yend = model8), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model8), shape = 1) +
  theme_bw()  # Add theme for cleaner look

# Determine all ages
ABETA <- data$ABETA
TAU <- data$TAU
PTAU <- data$PTAU


# Determine ages for each gender separately
av1451i <- data$av1451inferiotemporal
av1451total <- data$av1451totalcort
av1451e <- data$av1451entorhinal
xy <- data.frame(cbind(ABETA,av1451i))
xy

cor.test(ABETA, av1451i)

bootCorTest <- function(data, i){
  d <- data[i, ]
  res <- cor.test(d$ABETA, d$av1451i)
  c(stat = res$statistic, p.value = res$p.value, coeff = res$estimate,
    parameter = res$parameter, method = res$method, CI = res$conf.int)
}

b <- boot(xy, bootCorTest, R = 1000)

b$t0

xy <- data.frame(cbind(TAU,av1451i))
xy

cor.test(TAU, av1451i)

bootCorTest <- function(data, i){
  d <- data[i, ]
  res <- cor.test(d$TAU, d$av1451i)
  c(stat = res$statistic, p.value = res$p.value, coeff = res$estimate,
    parameter = res$parameter, method = res$method, CI = res$conf.int)
}

b <- boot(xy, bootCorTest, R = 1000)

b$t0

xy <- data.frame(cbind(PTAU,av1451i))
xy

cor.test(PTAU, av1451i)

bootCorTest <- function(data, i){
  d <- data[i, ]
  res <- cor.test(d$PTAU, d$av1451i)
  c(stat = res$statistic, p.value = res$p.value, coeff = res$estimate,
    parameter = res$parameter, method = res$method, CI = res$conf.int)
}

b <- boot(xy, bootCorTest, R = 1000)

b$t0

#################################################################################################

library(lsr) # for standardCoefs()

# Build our model
model <- lm( ABETA ~ av1451inferiotemporal, data = fulldata)
summary( model )
standardCoefs( model )
model <- lm( TAU ~ av1451inferiotemporal, data = fulldata)
summary( model )
standardCoefs( model )
model <- lm( PTAU ~ av1451inferiotemporal, data = fulldata)
summary( model )
standardCoefs( model )


# AVPET

cor.test(fulldata$ABETA, fulldata$av1451totalcort, method="pearson")
cor.test(fulldata$TAU, fulldata$av1451totalcort, method="pearson")
cor.test(fulldata$PTAU, fulldata$av1451totalcort, method="pearson")

cor.test(fulldata$ABETA, fulldata$av1451entorhinal, method="pearson")
cor.test(fulldata$TAU, fulldata$av1451entorhinal, method="pearson")
cor.test(fulldata$PTAU, fulldata$av1451entorhinal, method="pearson")

cor.test(fulldata$ABETA, fulldata$av1451inferiotemporal, method="pearson")
cor.test(fulldata$TAU, fulldata$av1451inferiotemporal, method="pearson")
cor.test(fulldata$PTAU, fulldata$av1451inferiotemporal, method="pearson")


set.seed(123)
row.number <- sample(1:nrow(fulldata), 0.8*nrow(fulldata))
train = fulldata[row.number,]
test = fulldata[-row.number,]
dim(train)
dim(test)
ggplot(train, aes(ABETA)) + geom_density(fill="blue")
ggplot(train, aes(log(ABETA))) + geom_density(fill="blue")
ggplot(train, aes(sqrt(ABETA))) + geom_density(fill="blue")
model1 = lm(ABETA~av1451entorhinal, data=train)
summary(model1)
par(mfrow=c(2,2))
plot(model1)
pred1 <- predict(model1, newdata = test)
rmse <- sqrt(sum((exp(pred1) - test$ABETA)^2)/length(test$ABETA))
c(RMSE = rmse, R2=summary(model1)$r.squared)
par(mfrow=c(1,1))
plot(test$ABETA, exp(pred1))

actuals_preds <- data.frame(cbind(actuals=fulldata$ABETA, predicteds=pred1))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)

ggplot(fulldata, aes(x = av1451entorhinal, y = ABETA)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = av1451entorhinal, yend = model1), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model1), shape = 1) +
  theme_bw()  # Add theme for cleaner look

model2 = lm(ABETA~av1451inferiotemporal, data=train)
summary(model2)
par(mfrow=c(2,2))
plot(model2)
pred2 <- predict(model2, newdata = test)
rmse <- sqrt(sum((exp(pred2) - test$ABETA)^2)/length(test$ABETA))
c(RMSE = rmse, R2=summary(model2)$r.squared)
par(mfrow=c(1,1))
plot(test$ABETA, exp(pred2))

actuals_preds <- data.frame(cbind(actuals=fulldata$ABETA, predicteds=pred2))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)
fulldata$model2 <- actuals_preds$predicteds

ggplot(fulldata, aes(x = av1451inferiotemporal, y = ABETA)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = av1451inferiotemporal, yend = model2), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model2), shape = 1) +
  theme_bw()  # Add theme for cleaner look

model3 = lm(ABETA~av1451totalcort, data=train)
summary(model3)
standardCoefs( model3 )
par(mfrow=c(2,2))
plot(model3)
pred3 <- predict(model3, newdata = test)
rmse <- sqrt(sum((exp(pred3) - test$ABETA)^2)/length(test$ABETA))
c(RMSE = rmse, R2=summary(model3)$r.squared)
par(mfrow=c(1,1))
plot(test$ABETA, exp(pred3))

actuals_preds <- data.frame(cbind(actuals=fulldata$ABETA, predicteds=pred3))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)
fulldata$model3 <- actuals_preds$predicteds

ggplot(fulldata, aes(x = av1451totalcort, y = ABETA)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = av1451totalcort, yend = model3), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model3), shape = 1) +
  theme_bw()  # Add theme for cleaner look


model4 = lm(TAU~av1451entorhinal, data=train)
summary(model4)
par(mfrow=c(2,2))
plot(model1)
pred4 <- predict(model4, newdata = test)
rmse <- sqrt(sum((exp(pred4) - test$TAU)^2)/length(test$TAU))
c(RMSE = rmse, R2=summary(model4)$r.squared)
par(mfrow=c(1,1))
plot(test$TAU, exp(pred4))

actuals_preds <- data.frame(cbind(actuals=fulldata$TAU, predicteds=pred4))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)

fulldata$model4 <- actuals_preds$predicteds

ggplot(fulldata, aes(x = av1451entorhinal, y = TAU)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = av1451entorhinal, yend = model4), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model4), shape = 1) +
  theme_bw()  # Add theme for cleaner look

model5 = lm(TAU~av1451inferiotemporal, data=train)
summary(model5)
par(mfrow=c(2,2))
plot(model5)
pred5 <- predict(model5, newdata = test)
rmse <- sqrt(sum((exp(pred5) - test$TAU)^2)/length(test$TAU))
c(RMSE = rmse, R2=summary(model5)$r.squared)
par(mfrow=c(1,1))
plot(test$TAU, exp(pred5))

actuals_preds <- data.frame(cbind(actuals=fulldata$TAU, predicteds=pred5))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)
fulldata$model5 <- actuals_preds$predicteds

ggplot(fulldata, aes(x = av1451inferiotemporal, y = TAU)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = av1451inferiotemporal, yend = model5), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model5), shape = 1) +
  theme_bw()  # Add theme for cleaner look


model6 = lm(TAU~av1451totalcort, data=train)
summary(model6)
standardCoefs( model6 )
par(mfrow=c(2,2))
plot(model6)
pred6 <- predict(model6, newdata = test)
rmse <- sqrt(sum((exp(pred6) - test$TAU)^2)/length(test$TAU))
c(RMSE = rmse, R2=summary(model6)$r.squared)
par(mfrow=c(1,1))
plot(test$TAU, exp(pred6))

actuals_preds <- data.frame(cbind(actuals=fulldata$TAU, predicteds=pred6))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)

fulldata$model6 <- actuals_preds$predicteds

ggplot(fulldata, aes(x = av1451totalcort, y = TAU)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = av1451totalcort, yend = model6), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model6), shape = 1) +
  theme_bw()  # Add theme for cleaner look


model7 = lm(PTAU~av1451entorhinal, data=train)
summary(model7)
par(mfrow=c(2,2))
plot(model1)
pred7 <- predict(model7, newdata = test)
rmse <- sqrt(sum((exp(pred7) - test$PTAU)^2)/length(test$PTAU))
c(RMSE = rmse, R2=summary(model7)$r.squared)
par(mfrow=c(1,1))
plot(test$PTAU, exp(pred7))

actuals_preds <- data.frame(cbind(actuals=fulldata$PTAU, predicteds=pred7))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)

fulldata$model7 <- actuals_preds$predicteds

ggplot(fulldata, aes(x = av1451entorhinal, y = PTAU)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = av1451entorhinal, yend = model7), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model7), shape = 1) +
  theme_bw()  # Add theme for cleaner look


model8 = lm(PTAU~av1451inferiotemporal, data=train)
summary(model8)
par(mfrow=c(2,2))
plot(model8)
pred8 <- predict(model8, newdata = test)
rmse <- sqrt(sum((exp(pred8) - test$PTAU)^2)/length(test$PTAU))
c(RMSE = rmse, R2=summary(model8)$r.squared)
par(mfrow=c(1,1))
plot(test$PTAU, exp(pred8))

actuals_preds <- data.frame(cbind(actuals=fulldata$PTAU, predicteds=pred8))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)
fulldata$model8 <- actuals_preds$predicteds

ggplot(fulldata, aes(x = av1451inferiotemporal, y = PTAU)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = av1451inferiotemporal, yend = model8), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model8), shape = 1) +
  theme_bw()  # Add theme for cleaner look


model8 = lm(PTAU~av1451totalcort, data=train)
summary(model8)
standardCoefs( model8 )
par(mfrow=c(2,2))
plot(model8)
pred8 <- predict(model8, newdata = test)
rmse <- sqrt(sum((exp(pred8) - test$PTAU)^2)/length(test$PTAU))
c(RMSE = rmse, R2=summary(model8)$r.squared)
par(mfrow=c(1,1))
plot(test$PTAU, exp(pred8))

actuals_preds <- data.frame(cbind(actuals=fulldata$PTAU, predicteds=pred8))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)

fulldata$model8 <- actuals_preds$predicteds

ggplot(fulldata, aes(x = av1451totalcort, y = PTAU)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = av1451totalcort, yend = model8), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = model8), shape = 1) +
  theme_bw()  # Add theme for cleaner look



# Determine all ages
ABETA <- fulldata$ABETA
TAU <- fulldata$TAU
PTAU <- fulldata$PTAU


# Determine ages for each gender separately
av1451i <- fulldata$av1451inferiotemporal
av1451total <- fulldata$av1451totalcort
av1451e <- fulldata$av1451entorhinal



# CORRELATION
library(boot)

xy <- data.frame(cbind(ABETA,av1451e))
xy

cor.test(ABETA, av1451e)

bootCorTest <- function(data, i){
  d <- data[i, ]
  res <- cor.test(d$ABETA, d$av1451e)
  c(stat = res$statistic, p.value = res$p.value, coeff = res$estimate,
    parameter = res$parameter, method = res$method, CI = res$conf.int)
}

b <- boot(xy, bootCorTest, R = 1000)

b$t0
#coeff of -6.668709e-01 is just -0.667

boot.ci(b)



xy <- data.frame(cbind(ABETA,av1451total))
xy

cor.test(ABETA, av1451total)

bootCorTest <- function(data, i){
  d <- data[i, ]
  res <- cor.test(d$ABETA, d$av1451total)
  c(stat = res$statistic, p.value = res$p.value, coeff = res$estimate,
    parameter = res$parameter, method = res$method, CI = res$conf.int)
}

b <- boot(xy, bootCorTest, R = 1000)

b$t0
#coeff of -6.668709e-01 is just -0.667

boot.ci(b)



xy <- data.frame(cbind(ABETA,av1451i))
xy

cor.test(ABETA, av1451i)

bootCorTest <- function(data, i){
  d <- data[i, ]
  res <- cor.test(d$ABETA, d$av1451i)
  c(stat = res$statistic, p.value = res$p.value, coeff = res$estimate,
    parameter = res$parameter, method = res$method, CI = res$conf.int)
}

b <- boot(xy, bootCorTest, R = 1000)

b$t0
#coeff of -6.668709e-01 is just -0.667

boot.ci(b)



xy <- data.frame(cbind(TAU,av1451total))
xy

cor.test(TAU, av1451total)

bootCorTest <- function(data, i){
  d <- data[i, ]
  res <- cor.test(d$TAU, d$av1451total)
  c(stat = res$statistic, p.value = res$p.value, coeff = res$estimate,
    parameter = res$parameter, method = res$method, CI = res$conf.int)
}

b <- boot(xy, bootCorTest, R = 1000)

b$t0
#coeff of -6.668709e-01 is just -0.667

boot.ci(b)


xy <- data.frame(cbind(TAU,av1451i))
xy

cor.test(TAU, av1451i)

bootCorTest <- function(data, i){
  d <- data[i, ]
  res <- cor.test(d$TAU, d$av1451i)
  c(stat = res$statistic, p.value = res$p.value, coeff = res$estimate,
    parameter = res$parameter, method = res$method, CI = res$conf.int)
}

b <- boot(xy, bootCorTest, R = 1000)

b$t0
#coeff of -6.668709e-01 is just -0.667

boot.ci(b)



xy <- data.frame(cbind(TAU,av1451e))
xy

cor.test(TAU, av1451e)

bootCorTest <- function(data, i){
  d <- data[i, ]
  res <- cor.test(d$TAU, d$av1451e)
  c(stat = res$statistic, p.value = res$p.value, coeff = res$estimate,
    parameter = res$parameter, method = res$method, CI = res$conf.int)
}

b <- boot(xy, bootCorTest, R = 1000)

b$t0
#coeff of -6.668709e-01 is just -0.667

boot.ci(b)



xy <- data.frame(cbind(PTAU,av1451total))
xy

cor.test(PTAU, av1451total)

bootCorTest <- function(data, i){
  d <- data[i, ]
  res <- cor.test(d$PTAU, d$av1451total)
  c(stat = res$statistic, p.value = res$p.value, coeff = res$estimate,
    parameter = res$parameter, method = res$method, CI = res$conf.int)
}

b <- boot(xy, bootCorTest, R = 1000)

b$t0
#coeff of -6.668709e-01 is just -0.667

boot.ci(b)



xy <- data.frame(cbind(PTAU,av1451i))
xy

cor.test(PTAU, av1451i)

bootCorTest <- function(data, i){
  d <- data[i, ]
  res <- cor.test(d$PTAU, d$av1451i)
  c(stat = res$statistic, p.value = res$p.value, coeff = res$estimate,
    parameter = res$parameter, method = res$method, CI = res$conf.int)
}

b <- boot(xy, bootCorTest, R = 1000)

b$t0
#coeff of -6.668709e-01 is just -0.667

boot.ci(b)



xy <- data.frame(cbind(PTAU,av1451e))
xy

cor.test(PTAU, av1451e)

bootCorTest <- function(data, i){
  d <- data[i, ]
  res <- cor.test(d$PTAU, d$av1451e)
  c(stat = res$statistic, p.value = res$p.value, coeff = res$estimate,
    parameter = res$parameter, method = res$method, CI = res$conf.int)
}

b <- boot(xy, bootCorTest, R = 1000)

b$t0
#coeff of -6.668709e-01 is just -0.667

boot.ci(b)




## REGRESSION

#set.seed(123)
N  <- 100
(fit <- lm(TAU ~ av1451e + av1451i + av1451total, data=fulldata))
sqrt(diag(vcov(fit)))
confint(fit)
getRegr <- function(dat, idx) {
  bsFit <- lm(TAU ~ av1451e + av1451i + av1451total, subset=idx, data=dat)
  c(coeff = bsFit$coefficients, fit = bsFit$fitted.values, residuals = bsFit$residuals,
    Df = bsFit$df.residual)
}
getRegr <- function(dat, idx) {
  bsFit <- lm(TAU ~ av1451e + av1451i + av1451total, subset=idx, data=dat)
  c(coeff = bsFit$coefficients,
    Df = bsFit$df.residual)
}
nR <- 999
(bsRegr <- boot(fulldata, statistic=getRegr, R=nR))
#boot.ci(bsRegr, conf=0.95, type="bca", index=4)$bca



N <- 1000
# Linear Model to Boostrap          
Model2Boot  <- lm(TAU ~ av1451e + av1451i + av1451total, fulldata)
# Values of the model coefficients
Betas       <- coefficients(Model2Boot)
# Number of coefficents to test against
M           <- length(Betas)
# Matrix of M columns to hold Bootstraping results
BtStrpRes   <- matrix( rep(0,M*N), ncol=M)


for (i in 1:N) {
  # Simulate data N times from the model we assume be true
  # and save the resulting coefficient in the i-th row of BtStrpRes
  BtStrpRes[i,] <-coefficients(lm(unlist(simulate(Model2Boot)) ~av1451e + av1451i + av1451total))
}

mean(BtStrpRes[,1])
mean(BtStrpRes[,2])
mean(BtStrpRes[,3])
mean(BtStrpRes[,4])

#Get the p-values for coefficient
P_val1 <-mean( abs(BtStrpRes[,1] - mean(BtStrpRes[,1]) )> abs( Betas[1]))
P_val2 <-mean( abs(BtStrpRes[,2] - mean(BtStrpRes[,2]) )> abs( Betas[2]))
P_val3 <-mean( abs(BtStrpRes[,3] - mean(BtStrpRes[,3]) )> abs( Betas[3]))
P_val4 <-mean( abs(BtStrpRes[,4] - mean(BtStrpRes[,4]) )> abs( Betas[4]))

#and some parametric bootstrap confidence intervals (2.5%, 97.5%) 
ConfInt1 <- quantile(BtStrpRes[,1], c(.025, 0.975))
ConfInt2 <- quantile(BtStrpRes[,2], c(.025, 0.975))
ConfInt3 <- quantile(BtStrpRes[,3], c(.025, 0.975))
ConfInt4 <- quantile(BtStrpRes[,4], c(.025, 0.975))





Model2Boot  <- lm(TAU ~ av1451total, fulldata)
# Values of the model coefficients
Betas       <- coefficients(Model2Boot)
# Number of coefficents to test against
M           <- length(Betas)
# Matrix of M columns to hold Bootstraping results
BtStrpRes   <- matrix( rep(0,M*N), ncol=M)


for (i in 1:N) {
  # Simulate data N times from the model we assume be true
  # and save the resulting coefficient in the i-th row of BtStrpRes
  BtStrpRes[i,] <-coefficients(lm(unlist(simulate(Model2Boot)) ~av1451total))
}

mean(BtStrpRes[,1])
mean(BtStrpRes[,2])

#Get the p-values for coefficient
P_val1 <-mean( abs(BtStrpRes[,1] - mean(BtStrpRes[,1]) )> abs( Betas[1]))
P_val2 <-mean( abs(BtStrpRes[,2] - mean(BtStrpRes[,2]) )> abs( Betas[2]))
P_val1
P_val2

#and some parametric bootstrap confidence intervals (2.5%, 97.5%) 
ConfInt1 <- quantile(BtStrpRes[,1], c(.025, 0.975))
ConfInt2 <- quantile(BtStrpRes[,2], c(.025, 0.975))
ConfInt1
ConfInt2





Model2Boot  <- lm(PTAU ~ av1451totalcort, fulldata)
# Values of the model coefficients
Betas       <- coefficients(Model2Boot)
# Number of coefficents to test against
M           <- length(Betas)
# Matrix of M columns to hold Bootstraping results
BtStrpRes   <- matrix( rep(0,M*N), ncol=M)


for (i in 1:N) {
  # Simulate data N times from the model we assume be true
  # and save the resulting coefficient in the i-th row of BtStrpRes
  BtStrpRes[i,] <-coefficients(lm(unlist(simulate(Model2Boot)) ~av1451totalcort, fulldata))
}

mean(BtStrpRes[,1])
mean(BtStrpRes[,2])

#Get the p-values for coefficient
P_val1 <-mean( abs(BtStrpRes[,1] - mean(BtStrpRes[,1]) )> abs( Betas[1]))
P_val2 <-mean( abs(BtStrpRes[,2] - mean(BtStrpRes[,2]) )> abs( Betas[2]))
P_val1
P_val2

#and some parametric bootstrap confidence intervals (2.5%, 97.5%) 
ConfInt1 <- quantile(BtStrpRes[,1], c(.025, 0.975))
ConfInt2 <- quantile(BtStrpRes[,2], c(.025, 0.975))
ConfInt1
ConfInt2


twosidep <- function(data) {
  p1 <- sum(data > 0)/length(data)
  p2 <- sum(data < 0)/length(data)
  p <- min(p1,p2)*2
  return(p)
}



#set.seed(123)
#N  <- 100
#(fit <- lm(TAU ~ av1451e + av1451i + av1451total, data=fulldata))
#sqrt(diag(vcov(fit)))
#confint(fit)
bs <- function(formula, data, indices) {
  d <- data[indices,]
  fit <- lm(formula, data=d)
  return(coef(fit))
}
extrema <- apply(X=results$t, MARGIN = 1, FUN = ">", results$t0)

results <- boot(data = fulldata, statistic = bs, R = 1000, formula = TAU ~ av1451total)
results

boot.ci(results, type="bca")
results$t0
results$t

R = 1000
pvals <- rowSums(extrema)/(R+1)
pvals

pvals <- mean(abs(results$t)>abs(results$t0))
