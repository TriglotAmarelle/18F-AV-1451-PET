correlation_subset <- fulldata[, -(18)]
correlation_subset <- correlation_subset[, -(9:11)]
correlation_subset <- correlation_subset[, -(1)]
correlation_subset <- correlation_subset[, -(4)]
#correlation_subset <- correlation_subset[, -(2:4)]

correlation_subset$DX <- ifelse(correlation_subset$DX == "CN", 0,
                                ifelse(correlation_subset$DX == "MCI", 2,
                                1))
correlation_subset$PTGENDER <- ifelse(correlation_subset$PTGENDER == "Male", 0,
                                      1)

cor <- cor(correlation_subset, use="pairwise.complete.obs")
cor <- data.frame(cor)
cor <- cor[order(abs(cor$DX)), ] #sort by correlation with Dx
myDF <- cbind(Row.Names = rownames(cor), cor)
## The variables which correlate best with the diagnosis are shown below
tail(myDF, 7)
myvars2 <- tail(myDF$Row.Names, n=10)

library(sparcl)

check <- correlation_subset
check <- check[!grepl("2", check$DX) , ]


################################# PCA without Braak regions

#check <- check[, -(42)]

data.classvar <- check[, 3]
#prcomp(~ ., data=log.data, na.action=na.omit, scale=TRUE)
#prcomp(na.omit(log.data), scale=TRUE)

log.data <- prcomp(scale(check[,10:68]))
plot(log.data$x[,1:2], col = data.classvar)

#log.data <- prcomp(check[,7:57], scale. = T)

#outputs the mean of variables
log.data$center

#outputs the standard deviation of variables
log.data$scale

log.data$rotation

pcacorr <- data.frame(log.data$rotation)

pcacorr <- log.data$rotation[order(abs(pcacorr$PC7)), ]
pcacorr[,7]

biplot(log.data, scale = 0)

#compute standard deviation of each principal component
std_dev <- log.data$sdev

#compute variance
pr_var <- std_dev^2

#check variance of first 10 components
pr_var[1:10]

#proportion of variance explained
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:20]
#PC1 variability explains 54% of the data, PC2 16% ...


#scree plot
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")

#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

nComp = 2
mu = colMeans(check[10:64])
Xhat = log.data$x[,1:nComp] %*% t(log.data$rotation[,1:nComp])
Xhat = scale(Xhat, center = -mu, scale = FALSE)
Xhat[1,]
sorted <- sort(Xhat[1,])

sorted
log.data
summary(log.data)

library(devtools)
library(ggbiplot)
g <- ggbiplot(log.data, obs.scale = 1, var.scale = 1, 
              groups = as.factor (data.classvar), ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
g <- g + theme_bw()
print(g)


check.new <- cbind(check, log.data$x[,1:9])

pcafile <- check.new[, -(7:68)]
pcafile <- pcafile[, -(4:6)]
normalize <- function(x){return((x-min(x))/(max(x)-min(x)))}
TAU <- check$TAU
PTAU <- check$PTAU
norm <- data.frame(TAU, PTAU)
norm <- as.data.frame(lapply(norm[1:2], normalize))
pcafile$TAU <- norm$TAU
pcafile$PTAU <- norm$PTAU
pcafile <- pcafile[,c(4:14,1:3)]

cor <- cor(pcafile, use="pairwise.complete.obs")
cor <- data.frame(cor)
cor <- cor[order(abs(cor$DX)), ] #sort by correlation with Dx
pcacorrregions <- cbind(Row.Names = rownames(cor), cor)



## Question 1. What part of the brain is best related to TAU CSF

cor_subset <- check[, -(6:9)]

corq1 <- cor(cor_subset[5:64], use="pairwise.complete.obs")
corq1 <- data.frame(corq1)
corq1 <- corq1[order(abs(corq1$TAU)), ] #sort by correlation with CSF TAU
myDF <- cbind(Row.Names = rownames(corq1), corq1)
## The variables which correlate best with the CSF TAU are shown below
tail(myDF, 7)
half <- length(myDF)/2
besttau <- tail(myDF$Row.Names, n=half)
besttau # this is really the answer to Question 1
worsttau <- head(myDF$Row.Names, n=half)
worsttau

p_subset <- check[,-(7:9)]
corq1 <- cor(p_subset[6:65], use="pairwise.complete.obs")
corq1 <- data.frame(corq1)
corq1 <- corq1[order(abs(corq1$PTAU)), ] #sort by correlation with CSF TAU
myDF <- cbind(Row.Names = rownames(corq1), corq1)
## The variables which correlate best with the CSF TAU are shown below
tail(myDF, 7)
half <- length(myDF)/2
bestp <- tail(myDF$Row.Names, n=half)
bestp # this is really the answer to Question 1
worstp <- head(myDF$Row.Names, n=half)
worstp

## Question 2. Is that region a better predictor of the diagnosis than the regions not related to TAU

### Best 10

var19 <- which( colnames(check)==besttau[19] )
var20 <- which( colnames(check)==besttau[20] )
var21 <- which( colnames(check)==besttau[21] )
var22 <- which( colnames(check)==besttau[22] )
var23 <- which( colnames(check)==besttau[23] )
var24 <- which( colnames(check)==besttau[24] )
var25 <- which( colnames(check)==besttau[25] )
var26 <- which( colnames(check)==besttau[26] )
var27 <- which( colnames(check)==besttau[27] )
var28 <- which( colnames(check)==besttau[28] )
besttauvect <- c(var19,var20,var21,var22,var23, 
  var24, var25, var26, var27, var28)
clust_df <- check[besttauvect]
clust_df$DX <- check$DX

library(RSSL)
library(SDMTools)
library(dplyr)
library(ggplot2)
library(tidyr)
set.seed(40)
clust_df$DX <- as.factor(clust_df$DX)
df <- clust_df %>% add_missinglabels_mar(DX~.,0.70)
g_s <- SVM(DX~.,df,C=1,scale=TRUE,x_center=TRUE)
g_s4 <- S4VM(DX~.,df,C1=1,C2=0.1,lambda_tradeoff = 3,scale=TRUE,x_center=TRUE)
labs <- g_s4@labelings[-c(1:40),]
colnames(labs) <- paste("DX",seq_len(ncol(g_s4@labelings)),sep="-")
# Plot the final labeling that was selected
# Note that this may not correspond to a linear classifier even if the linear kernel is used.
# The solution does not seem to make a lot of sense, but this is what the current implementation returns
df %>%
  filter(is.na(DX)) %>%
  mutate(prediction=g_s4@predictions) %>%
  ggplot(aes(x=av1451inferiorparietal,y=av1451parsopercularis,color=prediction)) +
  geom_point() +
  stat_classifier(color="black", classifiers=list(g_s))

df1 <- df
df1$actual <- clust_df$DX
delete <- which(!is.na(df1$DX))
df1<- df1[-c(delete), ]

df1$DX <- g_s4@predictions
tab <- table(df1$DX, df1$actual)
prop.table(tab)*100
n = sum(tab) # number of instances
nc = nrow(tab) # number of classes
diag = diag(tab) # number of correctly classified instances per class 
rowsums = apply(tab, 1, sum) # number of instances per class
colsums = apply(tab, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n
accuracy
library(caret)
sensitivity(tab)
specificity(tab)

### Worst

var1 <- which( colnames(check)==worsttau[1] )
var2 <- which( colnames(check)==worsttau[2] )
var3 <- which( colnames(check)==worsttau[3] )
var4 <- which( colnames(check)==worsttau[4] )
var5 <- which( colnames(check)==worsttau[5] )
var6 <- which( colnames(check)==worsttau[6] )
var7 <- which( colnames(check)==worsttau[7] )
var8 <- which( colnames(check)==worsttau[8] )
var9 <- which( colnames(check)==worsttau[9] )
var10 <- which( colnames(check)==worsttau[10] )
worsttauvec <- c(var1, var2,var3,var4,var5,var6,var7,var8,var9, var10)
clust_df <- check[worsttauvec]
clust_df$DX <- check$DX

set.seed(40)
clust_df$DX <- as.factor(clust_df$DX)
df <- clust_df %>% add_missinglabels_mar(DX~.,0.70)
g_s <- SVM(DX~.,df,C=1,scale=TRUE,x_center=TRUE)
g_s4 <- S4VM(DX~.,df,C1=1,C2=0.1,lambda_tradeoff = 3,scale=TRUE,x_center=TRUE)
labs <- g_s4@labelings[-c(1:40),]
colnames(labs) <- paste("DX",seq_len(ncol(g_s4@labelings)),sep="-")
# Plot the final labeling that was selected
# Note that this may not correspond to a linear classifier even if the linear kernel is used.
# The solution does not seem to make a lot of sense, but this is what the current implementation returns
df %>%
  filter(is.na(DX)) %>%
  mutate(prediction=g_s4@predictions) %>%
  ggplot(aes(x=av1451inferiorparietal,y=av1451parsopercularis,color=prediction)) +
  geom_point() +
  stat_classifier(color="black", classifiers=list(g_s))

df1 <- df
df1$actual <- clust_df$DX
delete <- which(!is.na(df1$DX))
df1<- df1[-c(delete), ]

df1$DX <- g_s4@predictions
tab <- table(df1$DX, df1$actual)
prop.table(tab)*100
n = sum(tab) # number of instances
nc = nrow(tab) # number of classes
diag = diag(tab) # number of correctly classified instances per class 
rowsums = apply(tab, 1, sum) # number of instances per class
colsums = apply(tab, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n
accuracy
sensitivity(tab)
specificity(tab)

# So far, with around 95% correct diagnoses for the best regions, and 0.875 for the worst regions, 
# neither group does a good job
# Let's reduce that to the highest ranked 5 variables for both

var24 <- which( colnames(check)==besttau[24] )
var25 <- which( colnames(check)==besttau[25] )
var26 <- which( colnames(check)==besttau[26] )
var27 <- which( colnames(check)==besttau[27] )
var28 <- which( colnames(check)==besttau[28] )
besttauvect <- c(var24, var25, var26, var27, var28)
clust_df <- check[besttauvect]
clust_df$DX <- check$DX

set.seed(40)
clust_df$DX <- as.factor(clust_df$DX)
df <- clust_df %>% add_missinglabels_mar(DX~.,0.70)
g_s <- SVM(DX~.,df,C=1,scale=TRUE,x_center=TRUE)
g_s4 <- S4VM(DX~.,df,C1=1,C2=0.1,lambda_tradeoff = 3,scale=TRUE,x_center=TRUE)
labs <- g_s4@labelings[-c(1:40),]
colnames(labs) <- paste("DX",seq_len(ncol(g_s4@labelings)),sep="-")
# Plot the final labeling that was selected
# Note that this may not correspond to a linear classifier even if the linear kernel is used.
# The solution does not seem to make a lot of sense, but this is what the current implementation returns
df %>%
  filter(is.na(DX)) %>%
  mutate(prediction=g_s4@predictions) %>%
  ggplot(aes(x=av1451middletemporal,y=av1451totalcort,color=prediction)) +
  geom_point() +
  stat_classifier(color="black", classifiers=list(g_s))

df1 <- df
df1$actual <- clust_df$DX
delete <- which(!is.na(df1$DX))
df1<- df1[-c(delete), ]

df1$DX <- g_s4@predictions
tab <- table(df1$DX, df1$actual)
prop.table(tab)*100
n = sum(tab) # number of instances
nc = nrow(tab) # number of classes
diag = diag(tab) # number of correctly classified instances per class 
rowsums = apply(tab, 1, sum) # number of instances per class
colsums = apply(tab, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n
accuracy
sensitivity(tab)
specificity(tab)

# The same! 95%

var1 <- which( colnames(check)==worsttau[1] )
var2 <- which( colnames(check)==worsttau[2] )
var3 <- which( colnames(check)==worsttau[3] )
var4 <- which( colnames(check)==worsttau[4] )
var5 <- which( colnames(check)==worsttau[5] )
worsttauvec <- c(var1, var2, var3, var4, var5)
clust_df <- check[worsttauvec]
clust_df$DX <- check$DX

set.seed(40)
clust_df$DX <- as.factor(clust_df$DX)
df <- clust_df %>% add_missinglabels_mar(DX~.,0.70)
g_s <- SVM(DX~.,df,C=1,scale=TRUE,x_center=TRUE)
g_s4 <- S4VM(DX~.,df,C1=1,C2=0.1,lambda_tradeoff = 3,scale=TRUE,x_center=TRUE)
labs <- g_s4@labelings[-c(1:40),]
colnames(labs) <- paste("DX",seq_len(ncol(g_s4@labelings)),sep="-")
# Plot the final labeling that was selected
# Note that this may not correspond to a linear classifier even if the linear kernel is used.
# The solution does not seem to make a lot of sense, but this is what the current implementation returns
df %>%
  filter(is.na(DX)) %>%
  mutate(prediction=g_s4@predictions) %>%
  ggplot(aes(x=ventraldc,y=pallidum,color=prediction)) +
  geom_point() +
  stat_classifier(color="black", classifiers=list(g_s))

df1 <- df
df1$actual <- clust_df$DX
delete <- which(!is.na(df1$DX))
df1<- df1[-c(delete), ]

df1$DX <- g_s4@predictions
tab <- table(df1$DX, df1$actual)
prop.table(tab)*100
n = sum(tab) # number of instances
nc = nrow(tab) # number of classes
diag = diag(tab) # number of correctly classified instances per class 
rowsums = apply(tab, 1, sum) # number of instances per class
colsums = apply(tab, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n
accuracy
sensitivity(tab)
specificity(tab)

# 89%

##PTAU

### Best 10

var19 <- which( colnames(check)==bestp[19] )
var20 <- which( colnames(check)==bestp[20] )
var21 <- which( colnames(check)==bestp[21] )
var22 <- which( colnames(check)==bestp[22] )
var23 <- which( colnames(check)==bestp[23] )
var24 <- which( colnames(check)==bestp[24] )
var25 <- which( colnames(check)==bestp[25] )
var26 <- which( colnames(check)==bestp[26] )
var27 <- which( colnames(check)==bestp[27] )
var28 <- which( colnames(check)==bestp[28] )
bestpvect <- c(var19, var20, var21, var22, var23, var24, var25, var26, var27, var28)
clust_df <- check[bestpvect]
clust_df$DX <- check$DX

set.seed(40)
clust_df$DX <- as.factor(clust_df$DX)
df <- clust_df %>% add_missinglabels_mar(DX~.,0.70)
g_s <- SVM(DX~.,df,C=1,scale=TRUE,x_center=TRUE)
g_s4 <- S4VM(DX~.,df,C1=1,C2=0.1,lambda_tradeoff = 3,scale=TRUE,x_center=TRUE)
labs <- g_s4@labelings[-c(1:40),]
colnames(labs) <- paste("DX",seq_len(ncol(g_s4@labelings)),sep="-")
# Plot the final labeling that was selected
# Note that this may not correspond to a linear classifier even if the linear kernel is used.
# The solution does not seem to make a lot of sense, but this is what the current implementation returns
df %>%
  filter(is.na(DX)) %>%
  mutate(prediction=g_s4@predictions) %>%
  ggplot(aes(x=av1451inferiorparietal,y=av1451parsopercularis,color=prediction)) +
  geom_point() +
  stat_classifier(color="black", classifiers=list(g_s))

df1 <- df
df1$actual <- clust_df$DX
delete <- which(!is.na(df1$DX))
df1<- df1[-c(delete), ]

df1$DX <- g_s4@predictions
tab <- table(df1$DX, df1$actual)
prop.table(tab)*100
n = sum(tab) # number of instances
nc = nrow(tab) # number of classes
diag = diag(tab) # number of correctly classified instances per class 
rowsums = apply(tab, 1, sum) # number of instances per class
colsums = apply(tab, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n
accuracy
sensitivity(tab)
specificity(tab)

### Worst

var1 <- which( colnames(check)==worstp[1] )
var2 <- which( colnames(check)==worstp[2] )
var3 <- which( colnames(check)==worstp[3] )
var4 <- which( colnames(check)==worstp[4] )
var5 <- which( colnames(check)==worstp[5] )
var6 <- which( colnames(check)==worstp[6] )
var7 <- which( colnames(check)==worstp[7] )
var8 <- which( colnames(check)==worstp[8] )
var9 <- which( colnames(check)==worstp[9] )
var10 <- which( colnames(check)==worstp[10] )
worstpvec <- c(var1, var2,var3,var4,var5,var6,var7,var8,var9, var10)
clust_df <- check[worstpvec]
clust_df$DX <- check$DX

set.seed(40)
clust_df$DX <- as.factor(clust_df$DX)
df <- clust_df %>% add_missinglabels_mar(DX~.,0.70)
g_s <- SVM(DX~.,df,C=1,scale=TRUE,x_center=TRUE)
g_s4 <- S4VM(DX~.,df,C1=1,C2=0.1,lambda_tradeoff = 3,scale=TRUE,x_center=TRUE)
labs <- g_s4@labelings[-c(1:40),]
colnames(labs) <- paste("DX",seq_len(ncol(g_s4@labelings)),sep="-")
# Plot the final labeling that was selected
# Note that this may not correspond to a linear classifier even if the linear kernel is used.
# The solution does not seem to make a lot of sense, but this is what the current implementation returns
df %>%
  filter(is.na(DX)) %>%
  mutate(prediction=g_s4@predictions) %>%
  ggplot(aes(x=av1451inferiorparietal,y=av1451parsopercularis,color=prediction)) +
  geom_point() +
  stat_classifier(color="black", classifiers=list(g_s))

df1 <- df
df1$actual <- clust_df$DX
delete <- which(!is.na(df1$DX))
df1<- df1[-c(delete), ]

df1$DX <- g_s4@predictions
tab <- table(df1$DX, df1$actual)
prop.table(tab)*100
n = sum(tab) # number of instances
nc = nrow(tab) # number of classes
diag = diag(tab) # number of correctly classified instances per class 
rowsums = apply(tab, 1, sum) # number of instances per class
colsums = apply(tab, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n
accuracy
sensitivity(tab)
specificity(tab)

# So far, with around 95% correct diagnoses for the best regions, and 86% for the worst regions, 
# neither group does a good job
# Let's reduce that to the highest ranked 5 variables for both

var24 <- which( colnames(check)==bestp[24] )
var25 <- which( colnames(check)==bestp[25] )
var26 <- which( colnames(check)==bestp[26] )
var27 <- which( colnames(check)==bestp[27] )
var28 <- which( colnames(check)==bestp[28] )
bestpvect <- c(var24, var25, var26, var27, var28)
clust_df <- check[bestpvect]

clust_df$DX <- check$DX

set.seed(100)
clust_df$DX <- as.factor(clust_df$DX)
df <- clust_df %>% add_missinglabels_mar(DX~.,0.70)
g_s <- SVM(DX~.,df,C=1,scale=TRUE,x_center=TRUE)
g_s4 <- S4VM(DX~.,df,C1=1,C2=0.1,lambda_tradeoff = 3,scale=TRUE,x_center=TRUE)
labs <- g_s4@labelings[-c(1:40),]
colnames(labs) <- paste("DX",seq_len(ncol(g_s4@labelings)),sep="-")
# Plot the final labeling that was selected
# Note that this may not correspond to a linear classifier even if the linear kernel is used.
# The solution does not seem to make a lot of sense, but this is what the current implementation returns
df %>%
  filter(is.na(DX)) %>%
  mutate(prediction=g_s4@predictions) %>%
  ggplot(aes(x=av1451lingual,y=av1451isthmuscin,color=prediction)) +
  geom_point() +
  stat_classifier(color="black", classifiers=list(g_s))

df1 <- df
df1$actual <- clust_df$DX
delete <- which(!is.na(df1$DX))
df1<- df1[-c(delete), ]

df1$DX <- g_s4@predictions
tab <- table(df1$DX, df1$actual)
prop.table(tab)*100
n = sum(tab) # number of instances
nc = nrow(tab) # number of classes
diag = diag(tab) # number of correctly classified instances per class 
rowsums = apply(tab, 1, sum) # number of instances per class
colsums = apply(tab, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n
accuracy
sensitivity(tab)
specificity(tab)

# Nope! 91%

var1 <- which( colnames(check)==worstp[1] )
var2 <- which( colnames(check)==worstp[2] )
var3 <- which( colnames(check)==worstp[3] )
var4 <- which( colnames(check)==worstp[4] )
var5 <- which( colnames(check)==worstp[5] )
worstpvec <- c(var1, var2,var3,var4,var5)
clust_df <- check[worstpvec]
clust_df$DX <- check$DX

set.seed(1)
clust_df$DX <- as.factor(clust_df$DX)
df <- clust_df %>% add_missinglabels_mar(DX~.,0.70)
g_s <- SVM(DX~.,df,C=1,scale=TRUE,x_center=TRUE)
g_s4 <- S4VM(DX~.,df,C1=1,C2=0.1,lambda_tradeoff = 3,scale=TRUE,x_center=TRUE)
labs <- g_s4@labelings[-c(1:40),]
colnames(labs) <- paste("DX",seq_len(ncol(g_s4@labelings)),sep="-")
# Plot the final labeling that was selected
# Note that this may not correspond to a linear classifier even if the linear kernel is used.
# The solution does not seem to make a lot of sense, but this is what the current implementation returns
df %>%
  filter(is.na(DX)) %>%
  mutate(prediction=g_s4@predictions) %>%
  ggplot(aes(x=anteriorcingcortex,y=ventricle3rd,color=prediction)) +
  geom_point() +
  stat_classifier(color="black", classifiers=list(g_s))

df1 <- df
df1$actual <- clust_df$DX
delete <- which(!is.na(df1$DX))
df1<- df1[-c(delete), ]

df1$DX <- g_s4@predictions
tab <- table(df1$DX, df1$actual)
prop.table(tab)*100
n = sum(tab) # number of instances
nc = nrow(tab) # number of classes
diag = diag(tab) # number of correctly classified instances per class 
rowsums = apply(tab, 1, sum) # number of instances per class
colsums = apply(tab, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n
accuracy
sensitivity(tab)
specificity(tab)

# 87.5%


## Question 3. What is the best way to predict the diagnosis?
justcsf <- check[, -(5:65)]
justcsf$TAU <- norm$TAU
justcsf$PTAU <- norm$PTAU
justcsf <- justcsf[, -(1:5)]
#justcsf$PTAUABETA <- norm$PTAUABETA
justcsf$DX <- check$DX

set.seed(40)
justcsf$DX <- as.factor(justcsf$DX)
df <- justcsf %>% add_missinglabels_mar(DX~.,0.70)
g_s <- SVM(DX~.,df,C=1,scale=TRUE,x_center=TRUE)
g_s4 <- S4VM(DX~.,df,C1=1,C2=0.1,lambda_tradeoff = 3,scale=TRUE,x_center=TRUE)
labs <- g_s4@labelings[-c(1:40),]
colnames(labs) <- paste("DX",seq_len(ncol(g_s4@labelings)),sep="-")
# Plot the final labeling that was selected
# Note that this may not correspond to a linear classifier even if the linear kernel is used.
# The solution does not seem to make a lot of sense, but this is what the current implementation returns
df %>%
  filter(is.na(DX)) %>%
  mutate(prediction=g_s4@predictions) %>%
  ggplot(aes(x=av1451inferiorparietal,y=av1451parsopercularis,color=prediction)) +
  geom_point() +
  stat_classifier(color="black", classifiers=list(g_s))

df1 <- df
df1$actual <- justcsf$DX
delete <- which(!is.na(df1$DX))
df1<- df1[-c(delete), ]

df1$DX <- g_s4@predictions
tab <- table(df1$DX, df1$actual)
prop.table(tab)*100
n = sum(tab) # number of instances
nc = nrow(tab) # number of classes
diag = diag(tab) # number of correctly classified instances per class 
rowsums = apply(tab, 1, sum) # number of instances per class
colsums = apply(tab, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n
accuracy
sensitivity(tab)
specificity(tab)


# CSF predicts just slightly above 87% with SSL

## PCA only

## Let's have a look at PCA variables

#pcaregions <- pcaregions[-(1)]
pcaregions <- pcafile[-(10:13)]

## Let's see how they correlate with DX

cor <- cor(pcaregions, use="pairwise.complete.obs")
cor <- data.frame(cor)
cor <- cor[order(abs(cor$DX)), ] #sort by correlation with Dx
regionsonlycorr <- cbind(Row.Names = rownames(cor), cor)

# Ok, PC1, PC2, PC3, PC4, age and gender
#pcaregions <- pcaregions[,c(10,11,1:9,12)]

# Check which variables are most useful

#1
set.seed(40)
pcaregions$DX <- as.factor(pcaregions$DX)
df <- pcaregions %>% add_missinglabels_mar(DX~.,0.70)
g_s <- SVM(DX~.,df[1:2],C=1,scale=TRUE,x_center=TRUE)
g_s4 <- S4VM(DX~.,df[1:2],C1=1,C2=0.1,lambda_tradeoff = 3,scale=TRUE,x_center=TRUE)
labs <- g_s4@labelings[-c(1:40),]
colnames(labs) <- paste("DX",seq_len(ncol(g_s4@labelings)),sep="-")
# Plot the final labeling that was selected
# Note that this may not correspond to a linear classifier even if the linear kernel is used.
# The solution does not seem to make a lot of sense, but this is what the current implementation returns
df %>%
  filter(is.na(DX)) %>%
  mutate(prediction=g_s4@predictions) %>%
  ggplot(aes(x=TAU,y=PTAU,color=prediction)) +
  geom_point() +
  stat_classifier(color="black", classifiers=list(g_s))

df1 <- df
df1$actual <- pcaregions$DX
delete <- which(!is.na(df1$DX))
df1<- df1[-c(delete), ]

df1$DX <- g_s4@predictions
tab <- table(df1$DX, df1$actual)
prop.table(tab)*100
n = sum(tab) # number of instances
nc = nrow(tab) # number of classes
diag = diag(tab) # number of correctly classified instances per class 
rowsums = apply(tab, 1, sum) # number of instances per class
colsums = apply(tab, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n
accuracy1 = accuracy
id1 = 1

#2
set.seed(40)
pcaregions$DX <- as.factor(pcaregions$DX)
df <- pcaregions %>% add_missinglabels_mar(DX~.,0.70)
g_s <- SVM(DX~.,df[1:3],C=1,scale=TRUE,x_center=TRUE)
g_s4 <- S4VM(DX~.,df[1:3],C1=1,C2=0.1,lambda_tradeoff = 3,scale=TRUE,x_center=TRUE)
labs <- g_s4@labelings[-c(1:40),]
colnames(labs) <- paste("DX",seq_len(ncol(g_s4@labelings)),sep="-")
# Plot the final labeling that was selected
# Note that this may not correspond to a linear classifier even if the linear kernel is used.
# The solution does not seem to make a lot of sense, but this is what the current implementation returns
df %>%
  filter(is.na(DX)) %>%
  mutate(prediction=g_s4@predictions) %>%
  ggplot(aes(x=TAU,y=PTAU,color=prediction)) +
  geom_point() +
  stat_classifier(color="black", classifiers=list(g_s))

df1 <- df
df1$actual <- pcaregions$DX
delete <- which(!is.na(df1$DX))
df1<- df1[-c(delete), ]

df1$DX <- g_s4@predictions
tab <- table(df1$DX, df1$actual)
prop.table(tab)*100
n = sum(tab) # number of instances
nc = nrow(tab) # number of classes
diag = diag(tab) # number of correctly classified instances per class 
rowsums = apply(tab, 1, sum) # number of instances per class
colsums = apply(tab, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n
accuracy2 = accuracy
id2 = 2

#3
set.seed(40)
pcaregions$DX <- as.factor(pcaregions$DX)
df <- pcaregions %>% add_missinglabels_mar(DX~.,0.70)
g_s <- SVM(DX~.,df[1:4],C=1,scale=TRUE,x_center=TRUE)
g_s4 <- S4VM(DX~.,df[1:4],C1=1,C2=0.1,lambda_tradeoff = 3,scale=TRUE,x_center=TRUE)
labs <- g_s4@labelings[-c(1:40),]
colnames(labs) <- paste("DX",seq_len(ncol(g_s4@labelings)),sep="-")
# Plot the final labeling that was selected
# Note that this may not correspond to a linear classifier even if the linear kernel is used.
# The solution does not seem to make a lot of sense, but this is what the current implementation returns
df %>%
  filter(is.na(DX)) %>%
  mutate(prediction=g_s4@predictions) %>%
  ggplot(aes(x=TAU,y=PTAU,color=prediction)) +
  geom_point() +
  stat_classifier(color="black", classifiers=list(g_s))

df1 <- df
df1$actual <- pcaregions$DX
delete <- which(!is.na(df1$DX))
df1<- df1[-c(delete), ]

df1$DX <- g_s4@predictions
tab <- table(df1$DX, df1$actual)
prop.table(tab)*100
n = sum(tab) # number of instances
nc = nrow(tab) # number of classes
diag = diag(tab) # number of correctly classified instances per class 
rowsums = apply(tab, 1, sum) # number of instances per class
colsums = apply(tab, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n
accuracy3 = accuracy
id3 = 3

#4
set.seed(40)
pcaregions$DX <- as.factor(pcaregions$DX)
df <- pcaregions %>% add_missinglabels_mar(DX~.,0.70)
g_s <- SVM(DX~.,df[1:5],C=1,scale=TRUE,x_center=TRUE)
g_s4 <- S4VM(DX~.,df[1:5],C1=1,C2=0.1,lambda_tradeoff = 3,scale=TRUE,x_center=TRUE)
labs <- g_s4@labelings[-c(1:40),]
colnames(labs) <- paste("DX",seq_len(ncol(g_s4@labelings)),sep="-")
# Plot the final labeling that was selected
# Note that this may not correspond to a linear classifier even if the linear kernel is used.
# The solution does not seem to make a lot of sense, but this is what the current implementation returns
df %>%
  filter(is.na(DX)) %>%
  mutate(prediction=g_s4@predictions) %>%
  ggplot(aes(x=TAU,y=PTAU,color=prediction)) +
  geom_point() +
  stat_classifier(color="black", classifiers=list(g_s))

df1 <- df
df1$actual <- pcaregions$DX
delete <- which(!is.na(df1$DX))
df1<- df1[-c(delete), ]

df1$DX <- g_s4@predictions
tab <- table(df1$DX, df1$actual)
prop.table(tab)*100
n = sum(tab) # number of instances
nc = nrow(tab) # number of classes
diag = diag(tab) # number of correctly classified instances per class 
rowsums = apply(tab, 1, sum) # number of instances per class
colsums = apply(tab, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n
accuracy4 = accuracy
id4 = 4

#5

set.seed(40)
pcaregions$DX <- as.factor(pcaregions$DX)
df <- pcaregions %>% add_missinglabels_mar(DX~.,0.70)
g_s <- SVM(DX~.,df[1:6],C=1,scale=TRUE,x_center=TRUE)
g_s4 <- S4VM(DX~.,df[1:6],C1=1,C2=0.1,lambda_tradeoff = 3,scale=TRUE,x_center=TRUE)
labs <- g_s4@labelings[-c(1:40),]
colnames(labs) <- paste("DX",seq_len(ncol(g_s4@labelings)),sep="-")
# Plot the final labeling that was selected
# Note that this may not correspond to a linear classifier even if the linear kernel is used.
# The solution does not seem to make a lot of sense, but this is what the current implementation returns
df %>%
  filter(is.na(DX)) %>%
  mutate(prediction=g_s4@predictions) %>%
  ggplot(aes(x=TAU,y=PTAU,color=prediction)) +
  geom_point() +
  stat_classifier(color="black", classifiers=list(g_s))

df1 <- df
df1$actual <- pcaregions$DX
delete <- which(!is.na(df1$DX))
df1<- df1[-c(delete), ]

df1$DX <- g_s4@predictions
tab <- table(df1$DX, df1$actual)
prop.table(tab)*100
n = sum(tab) # number of instances
nc = nrow(tab) # number of classes
diag = diag(tab) # number of correctly classified instances per class 
rowsums = apply(tab, 1, sum) # number of instances per class
colsums = apply(tab, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n
accuracy5 = accuracy
id5 = 5

#6

set.seed(40)
pcaregions$DX <- as.factor(pcaregions$DX)
df <- pcaregions %>% add_missinglabels_mar(DX~.,0.70)
g_s <- SVM(DX~.,df[1:7],C=1,scale=TRUE,x_center=TRUE)
g_s4 <- S4VM(DX~.,df[1:7],C1=1,C2=0.1,lambda_tradeoff = 3,scale=TRUE,x_center=TRUE)
labs <- g_s4@labelings[-c(1:40),]
colnames(labs) <- paste("DX",seq_len(ncol(g_s4@labelings)),sep="-")
# Plot the final labeling that was selected
# Note that this may not correspond to a linear classifier even if the linear kernel is used.
# The solution does not seem to make a lot of sense, but this is what the current implementation returns
df %>%
  filter(is.na(DX)) %>%
  mutate(prediction=g_s4@predictions) %>%
  ggplot(aes(x=TAU,y=PTAU,color=prediction)) +
  geom_point() +
  stat_classifier(color="black", classifiers=list(g_s))

df1 <- df
df1$actual <- pcaregions$DX
delete <- which(!is.na(df1$DX))
df1<- df1[-c(delete), ]

df1$DX <- g_s4@predictions
tab <- table(df1$DX, df1$actual)
prop.table(tab)*100
n = sum(tab) # number of instances
nc = nrow(tab) # number of classes
diag = diag(tab) # number of correctly classified instances per class 
rowsums = apply(tab, 1, sum) # number of instances per class
colsums = apply(tab, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n
accuracy6 = accuracy
id6 = 6

#7

set.seed(40)
pcaregions$DX <- as.factor(pcaregions$DX)
df <- pcaregions %>% add_missinglabels_mar(DX~.,0.70)
g_s <- SVM(DX~.,df[1:8],C=1,scale=TRUE,x_center=TRUE)
g_s4 <- S4VM(DX~.,df[1:8],C1=1,C2=0.1,lambda_tradeoff = 3,scale=TRUE,x_center=TRUE)
labs <- g_s4@labelings[-c(1:40),]
colnames(labs) <- paste("DX",seq_len(ncol(g_s4@labelings)),sep="-")
# Plot the final labeling that was selected
# Note that this may not correspond to a linear classifier even if the linear kernel is used.
# The solution does not seem to make a lot of sense, but this is what the current implementation returns
df %>%
  filter(is.na(DX)) %>%
  mutate(prediction=g_s4@predictions) %>%
  ggplot(aes(x=TAU,y=PTAU,color=prediction)) +
  geom_point() +
  stat_classifier(color="black", classifiers=list(g_s))

df1 <- df
df1$actual <- pcaregions$DX
delete <- which(!is.na(df1$DX))
df1<- df1[-c(delete), ]

df1$DX <- g_s4@predictions
tab <- table(df1$DX, df1$actual)
prop.table(tab)*100
n = sum(tab) # number of instances
nc = nrow(tab) # number of classes
diag = diag(tab) # number of correctly classified instances per class 
rowsums = apply(tab, 1, sum) # number of instances per class
colsums = apply(tab, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n
accuracy7 = accuracy
id7 = 7

#8

set.seed(40)
pcaregions$DX <- as.factor(pcaregions$DX)
df <- pcaregions %>% add_missinglabels_mar(DX~.,0.70)
g_s <- SVM(DX~.,df[1:9],C=1,scale=TRUE,x_center=TRUE)
g_s4 <- S4VM(DX~.,df[1:9],C1=1,C2=0.1,lambda_tradeoff = 3,scale=TRUE,x_center=TRUE)
labs <- g_s4@labelings[-c(1:40),]
colnames(labs) <- paste("DX",seq_len(ncol(g_s4@labelings)),sep="-")
# Plot the final labeling that was selected
# Note that this may not correspond to a linear classifier even if the linear kernel is used.
# The solution does not seem to make a lot of sense, but this is what the current implementation returns
df %>%
  filter(is.na(DX)) %>%
  mutate(prediction=g_s4@predictions) %>%
  ggplot(aes(x=TAU,y=PTAU,color=prediction)) +
  geom_point() +
  stat_classifier(color="black", classifiers=list(g_s))

df1 <- df
df1$actual <- pcaregions$DX
delete <- which(!is.na(df1$DX))
df1<- df1[-c(delete), ]

df1$DX <- g_s4@predictions
tab <- table(df1$DX, df1$actual)
prop.table(tab)*100
n = sum(tab) # number of instances
nc = nrow(tab) # number of classes
diag = diag(tab) # number of correctly classified instances per class 
rowsums = apply(tab, 1, sum) # number of instances per class
colsums = apply(tab, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n
accuracy8 = accuracy
id8 = 8

#9

set.seed(40)
pcaregions$DX <- as.factor(pcaregions$DX)
df <- pcaregions %>% add_missinglabels_mar(DX~.,0.70)
g_s <- SVM(DX~.,df[1:10],C=1,scale=TRUE,x_center=TRUE)
g_s4 <- S4VM(DX~.,df[1:10],C1=1,C2=0.1,lambda_tradeoff = 3,scale=TRUE,x_center=TRUE)
labs <- g_s4@labelings[-c(1:40),]
colnames(labs) <- paste("DX",seq_len(ncol(g_s4@labelings)),sep="-")
# Plot the final labeling that was selected
# Note that this may not correspond to a linear classifier even if the linear kernel is used.
# The solution does not seem to make a lot of sense, but this is what the current implementation returns
df %>%
  filter(is.na(DX)) %>%
  mutate(prediction=g_s4@predictions) %>%
  ggplot(aes(x=TAU,y=PTAU,color=prediction)) +
  geom_point() +
  stat_classifier(color="black", classifiers=list(g_s))

df1 <- df
df1$actual <- pcaregions$DX
delete <- which(!is.na(df1$DX))
df1<- df1[-c(delete), ]

df1$DX <- g_s4@predictions
tab <- table(df1$DX, df1$actual)
prop.table(tab)*100
n = sum(tab) # number of instances
nc = nrow(tab) # number of classes
diag = diag(tab) # number of correctly classified instances per class 
rowsums = apply(tab, 1, sum) # number of instances per class
colsums = apply(tab, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n
accuracy9 = accuracy
id9 = 9

#10

set.seed(40)
pcaregions$DX <- as.factor(pcaregions$DX)
df <- pcaregions %>% add_missinglabels_mar(DX~.,0.70)
g_s <- SVM(DX~.,df[1:11],C=1,scale=TRUE,x_center=TRUE)
g_s4 <- S4VM(DX~.,df[1:11],C1=1,C2=0.1,lambda_tradeoff = 3,scale=TRUE,x_center=TRUE)
labs <- g_s4@labelings[-c(1:40),]
colnames(labs) <- paste("DX",seq_len(ncol(g_s4@labelings)),sep="-")
# Plot the final labeling that was selected
# Note that this may not correspond to a linear classifier even if the linear kernel is used.
# The solution does not seem to make a lot of sense, but this is what the current implementation returns
df %>%
  filter(is.na(DX)) %>%
  mutate(prediction=g_s4@predictions) %>%
  ggplot(aes(x=TAU,y=PTAU,color=prediction)) +
  geom_point() +
  stat_classifier(color="black", classifiers=list(g_s))

df1 <- df
df1$actual <- pcaregions$DX
delete <- which(!is.na(df1$DX))
df1<- df1[-c(delete), ]

df1$DX <- g_s4@predictions
tab <- table(df1$DX, df1$actual)
prop.table(tab)*100
n = sum(tab) # number of instances
nc = nrow(tab) # number of classes
diag = diag(tab) # number of correctly classified instances per class 
rowsums = apply(tab, 1, sum) # number of instances per class
colsums = apply(tab, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n
accuracy10 = accuracy
id10 = 10

#11

set.seed(40)
pcaregions$DX <- as.factor(pcaregions$DX)
df <- pcaregions %>% add_missinglabels_mar(DX~.,0.70)
g_s <- SVM(DX~.,df[1:12],C=1,scale=TRUE,x_center=TRUE)
g_s4 <- S4VM(DX~.,df[1:12],C1=1,C2=0.1,lambda_tradeoff = 3,scale=TRUE,x_center=TRUE)
labs <- g_s4@labelings[-c(1:40),]
colnames(labs) <- paste("DX",seq_len(ncol(g_s4@labelings)),sep="-")
# Plot the final labeling that was selected
# Note that this may not correspond to a linear classifier even if the linear kernel is used.
# The solution does not seem to make a lot of sense, but this is what the current implementation returns
df %>%
  filter(is.na(DX)) %>%
  mutate(prediction=g_s4@predictions) %>%
  ggplot(aes(x=TAU,y=PTAU,color=prediction)) +
  geom_point() +
  stat_classifier(color="black", classifiers=list(g_s))

df1 <- df
df1$actual <- pcaregions$DX
delete <- which(!is.na(df1$DX))
df1<- df1[-c(delete), ]

df1$DX <- g_s4@predictions
tab <- table(df1$DX, df1$actual)
prop.table(tab)*100
n = sum(tab) # number of instances
nc = nrow(tab) # number of classes
diag = diag(tab) # number of correctly classified instances per class 
rowsums = apply(tab, 1, sum) # number of instances per class
colsums = apply(tab, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n
accuracy11 = accuracy
id11 = 11


accuracycol <- c(accuracy1,accuracy2,accuracy3,accuracy4,accuracy5,accuracy6,accuracy7,accuracy8,
                 accuracy9,accuracy10,accuracy11)
idcol <- c(id1,id2,id3,id4,id5,id6,id7,id8,
           id9,id10,id11)


acc<-data.frame(accuracycol,idcol)

ggplot(data = acc, aes(x = idcol, y = accuracycol)) + 
  scale_x_continuous(breaks = c(1,3,5,7,9,11)) + geom_line(col="red") + theme_bw()

## Okay, let us then only look at the first four PCs

set.seed(40)
pcaregions$DX <- as.factor(pcaregions$DX)
df <- pcaregions %>% add_missinglabels_mar(DX~.,0.70)
g_s <- SVM(DX~.,df[1:4],C=1,scale=TRUE,x_center=TRUE)
g_s4 <- S4VM(DX~.,df[1:4],C1=1,C2=0.1,lambda_tradeoff = 3,scale=TRUE,x_center=TRUE)
labs <- g_s4@labelings[-c(1:40),]
colnames(labs) <- paste("DX",seq_len(ncol(g_s4@labelings)),sep="-")
# Plot the final labeling that was selected
# Note that this may not correspond to a linear classifier even if the linear kernel is used.
# The solution does not seem to make a lot of sense, but this is what the current implementation returns
df %>%
  filter(is.na(DX)) %>%
  mutate(prediction=g_s4@predictions) %>%
  ggplot(aes(x=av1451inferiorparietal,y=av1451parsopercularis,color=prediction)) +
  geom_point() +
  stat_classifier(color="black", classifiers=list(g_s))

df1 <- df
df1$actual <- pcaregions$DX
delete <- which(!is.na(df1$DX))
df1<- df1[-c(delete), ]

df1$DX <- g_s4@predictions
tab <- table(df1$DX, df1$actual)
prop.table(tab)*100
n = sum(tab) # number of instances
nc = nrow(tab) # number of classes
diag = diag(tab) # number of correctly classified instances per class 
rowsums = apply(tab, 1, sum) # number of instances per class
colsums = apply(tab, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n
accuracy
sensitivity(tab)
specificity(tab)

# PCA elements predict 92.8% with SSL 
# WINNER

# PCA + CSF
pcacsf <- pcafile[,c(10:13,1:9,14)]


set.seed(40)
pcacsf$DX <- as.factor(pcacsf$DX)
df <- pcacsf %>% add_missinglabels_mar(DX~.,0.70)
g_s <- SVM(DX~.,df[1:5],C=1,scale=TRUE,x_center=TRUE)
g_s4 <- S4VM(DX~.,df[1:5],C1=1,C2=0.1,lambda_tradeoff = 3,scale=TRUE,x_center=TRUE)
labs <- g_s4@labelings[-c(1:40),]
colnames(labs) <- paste("DX",seq_len(ncol(g_s4@labelings)),sep="-")
# Plot the final labeling that was selected
# Note that this may not correspond to a linear classifier even if the linear kernel is used.
# The solution does not seem to make a lot of sense, but this is what the current implementation returns
df %>%
  filter(is.na(DX)) %>%
  mutate(prediction=g_s4@predictions) %>%
  ggplot(aes(x=av1451inferiorparietal,y=av1451parsopercularis,color=prediction)) +
  geom_point() +
  stat_classifier(color="black", classifiers=list(g_s))

df1 <- df
df1$actual <- pcacsf$DX
delete <- which(!is.na(df1$DX))
df1<- df1[-c(delete), ]

df1$DX <- g_s4@predictions
tab <- table(df1$DX, df1$actual)
prop.table(tab)*100
n = sum(tab) # number of instances
nc = nrow(tab) # number of classes
diag = diag(tab) # number of correctly classified instances per class 
rowsums = apply(tab, 1, sum) # number of instances per class
colsums = apply(tab, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n
accuracy
sensitivity(tab)
specificity(tab)

# Wow, 92.8%!!
# Ok, let's just look at the brain values ungrouped from pca

justtp <- check[, -(68)]
justtp <- justtp[, -(1:9)]
justtp$DX <- check$DX


set.seed(40)
justtp$DX <- as.factor(justtp$DX)
df <- justtp %>% add_missinglabels_mar(DX~.,0.70)
g_s <- SVM(DX~.,df,C=1,scale=TRUE,x_center=TRUE)
g_s4 <- S4VM(DX~.,df,C1=1,C2=0.1,lambda_tradeoff = 3,scale=TRUE,x_center=TRUE)
labs <- g_s4@labelings[-c(1:40),]
colnames(labs) <- paste("DX",seq_len(ncol(g_s4@labelings)),sep="-")
# Show the labelings that the algorithm is considering
df %>% filter(is.na(DX)) %>%
  bind_cols(data.frame(labs,check.names = FALSE)) %>%
  select(-DX) %>%
  gather(Classifier,Label,-PC1,-PC2) %>%
  ggplot(aes(x=PC1,y=PC2,color=Label)) +
  geom_point() +
  facet_wrap(~Classifier,ncol=5)
# Plot the final labeling that was selected
# Note that this may not correspond to a linear classifier even if the linear kernel is used.
# The solution does not seem to make a lot of sense, but this is what the current implementation returns
df %>%
  filter(is.na(DX)) %>%
  mutate(prediction=g_s4@predictions) %>%
  ggplot(aes(x=PC1,y=PC2,color=prediction)) +
  geom_point() +
  stat_classifier(color="black", classifiers=list(g_s))

df1 <- df
df1$actual <- justtp$DX
delete <- which(!is.na(df1$DX))
df1<- df1[-c(delete), ]

df1$DX <- g_s4@predictions
tab <- table(df1$DX, df1$actual)
prop.table(tab)*100
n = sum(tab) # number of instances
nc = nrow(tab) # number of classes
diag = diag(tab) # number of correctly classified instances per class 
rowsums = apply(tab, 1, sum) # number of instances per class
colsums = apply(tab, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n
accuracy
sensitivity(tab)
specificity(tab)

#92.8 % SSL


# Let us now use TAUPET and CSF. Brace yourself
taumaster <- justtp
taumaster$TAU <- norm$TAU
taumaster$PTAU <- norm$PTAU


set.seed(40)
taumaster$DX <- as.factor(taumaster$DX)
df <- taumaster %>% add_missinglabels_mar(DX~.,0.70)
g_s <- SVM(DX~.,df,C=1,scale=TRUE,x_center=TRUE)
g_s4 <- S4VM(DX~.,df,C1=1,C2=0.1,lambda_tradeoff = 3,scale=TRUE,x_center=TRUE)
labs <- g_s4@labelings[-c(1:40),]
colnames(labs) <- paste("DX",seq_len(ncol(g_s4@labelings)),sep="-")
# Plot the final labeling that was selected
# Note that this may not correspond to a linear classifier even if the linear kernel is used.
# The solution does not seem to make a lot of sense, but this is what the current implementation returns
df %>%
  filter(is.na(DX)) %>%
  mutate(prediction=g_s4@predictions) %>%
  ggplot(aes(x=TAU,y=av1451entorhinal,color=prediction)) +
  geom_point() +
  stat_classifier(color="black", classifiers=list(g_s))

df1 <- df
df1$actual <- taumaster$DX
delete <- which(!is.na(df1$DX))
df1<- df1[-c(delete), ]

df1$DX <- g_s4@predictions
tab <- table(df1$DX, df1$actual)
prop.table(tab)*100
n = sum(tab) # number of instances
nc = nrow(tab) # number of classes
diag = diag(tab) # number of correctly classified instances per class 
rowsums = apply(tab, 1, sum) # number of instances per class
colsums = apply(tab, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n
accuracy
sensitivity(tab)
specificity(tab)

# 92.8% predicted diagnoses



# Juste to verify, let us have a look at what the Braak regions are doing
braak <- check[, -(10:67)]
braak <- braak[, -(10)]
braak <- braak[, -(1:2)]

corbraak <- cor(braak, use="pairwise.complete.obs")
corbraak <- data.frame(corbraak)
corbraak <- corbraak[order(abs(corbraak$DX)), ] #sort by correlation with Dx
corbraakmatrix <- cbind(Row.Names = rownames(corbraak), corbraak)

#without csf
braakwocsf <- braak[, -(2:4)]

set.seed(40)
braakwocsf$DX <- as.factor(braakwocsf$DX)
df <- braakwocsf %>% add_missinglabels_mar(DX~.,0.70)
g_s <- SVM(DX~.,df,C=1,scale=TRUE,x_center=TRUE)
g_s4 <- S4VM(DX~.,df,C1=1,C2=0.1,lambda_tradeoff = 3,scale=TRUE,x_center=TRUE)
labs <- g_s4@labelings[-c(1:40),]
colnames(labs) <- paste("DX",seq_len(ncol(g_s4@labelings)),sep="-")
# Plot the final labeling that was selected
# Note that this may not correspond to a linear classifier even if the linear kernel is used.
# The solution does not seem to make a lot of sense, but this is what the current implementation returns
df %>%
  filter(is.na(DX)) %>%
  mutate(prediction=g_s4@predictions) %>%
  ggplot(aes(x=braak34,y=braak12,color=prediction)) +
  geom_point() +
  stat_classifier(color="black", classifiers=list(g_s))

df1 <- df
df1$actual <- braakwocsf$DX
delete <- which(!is.na(df1$DX))
df1<- df1[-c(delete), ]

df1$DX <- g_s4@predictions
tab <- table(df1$DX, df1$actual)
prop.table(tab)*100
n = sum(tab) # number of instances
nc = nrow(tab) # number of classes
diag = diag(tab) # number of correctly classified instances per class 
rowsums = apply(tab, 1, sum) # number of instances per class
colsums = apply(tab, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n
accuracy
sensitivity(tab)
specificity(tab)

# 95% predicted diagnoses


#with csf
braakwcsf <- braakwocsf
braakwcsf$TAU <- norm$TAU
braakwcsf$PTAU <- norm$PTAU
#braakwcsf <- braakwcsf[, -(7)]

set.seed(40)
braakwcsf$DX <- as.factor(braakwcsf$DX)
df <- braakwcsf %>% add_missinglabels_mar(DX~.,0.70)
g_s <- SVM(DX~.,df[1:6],C=1,scale=TRUE,x_center=TRUE)
g_s4 <- S4VM(DX~.,df[1:6],C1=1,C2=0.1,lambda_tradeoff = 3,scale=TRUE,x_center=TRUE)
labs <- g_s4@labelings[-c(1:40),]
colnames(labs) <- paste("DX",seq_len(ncol(g_s4@labelings)),sep="-")
# Plot the final labeling that was selected
# Note that this may not correspond to a linear classifier even if the linear kernel is used.
# The solution does not seem to make a lot of sense, but this is what the current implementation returns
df %>%
  filter(is.na(DX)) %>%
  mutate(prediction=g_s4@predictions) %>%
  ggplot(aes(x=braak34,y=braak12,color=prediction)) +
  geom_point() +
  stat_classifier(color="black", classifiers=list(g_s))

df1 <- df
df1$actual <- braakwcsf$DX
delete <- which(!is.na(df1$DX))
df1<- df1[-c(delete), ]

df1$DX <- g_s4@predictions
tab <- table(df1$DX, df1$actual)
prop.table(tab)*100
n = sum(tab) # number of instances
nc = nrow(tab) # number of classes
diag = diag(tab) # number of correctly classified instances per class 
rowsums = apply(tab, 1, sum) # number of instances per class
colsums = apply(tab, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n
accuracy
sensitivity(tab)
specificity(tab)

# Also 95 % SSL