library(dplyr)
library(tidyverse)
library(caret)
library(ggpubr)
library(StatMatch)  
library(scales)

# Downlod Data set from GitHub

dat <- read.csv(url("https://raw.githubusercontent.com/PaoloDePiante/Anomaly_Detection/master/Dataset_for_Bearing.csv"), sep=",", header = TRUE)
head(dat)
set.seed(2)

# Split data in "train set" for normal behaviour and "test set" for a failure 
train_set <- dat[which((as.POSIXct(dat$TS, origin="1970-01-01", tz="UTC") >= as.POSIXct('2004-03-04 09:27:46', origin="1970-01-01", tz="UTC")) & (as.POSIXct(dat$TS, origin="1970-01-01", tz="UTC") <= as.POSIXct('2004-04-14 20:51:57', origin="1970-01-01", tz="UTC"))),]
test_set <- dat[which(as.POSIXct(dat$TS, origin="1970-01-01", tz="UTC") >= as.POSIXct('2004-04-14 20:51:01', origin="1970-01-01", tz="UTC")),]

# Looking to Training Data for all Bearings 
par(mfrow=c(4,1))
plot(train_set$Bearing.1, type="l", col="dark red", main="Training Data from Bearing 1", ylab="Accelerometer Values ", xlab = "Number of data points")
plot(train_set$Bearing.2, type="l", col="blue", main="Training Data from Bearing 2", ylab="Accelerometer Values ", xlab = "Number of data points")
plot(train_set$Bearing.3, type="l", col="green", main="Training Data from Bearing 3", ylab="Accelerometer Values", xlab = "Number of data points")
plot(train_set$Bearing.4, type="l", col="purple", main="Training Data from Bearing 4", ylab="Accelerometer Values", xlab = "Number of data points")

# Normalize Data using Min Max Scaler to be in the range of [0,1]
min_max_train <- c(min(train_set$Bearing.1),max(train_set$Bearing.1), min(train_set$Bearing.2),max(train_set$Bearing.2), min(train_set$Bearing.3),max(train_set$Bearing.3), min(train_set$Bearing.4),max(train_set$Bearing.4))

Z_train_set <- train_set %>% mutate(Bearing.1 = ((train_set$Bearing.1-min_max_train[1])/(min_max_train[2]-min_max_train[1])),
                                    Bearing.2 = ((train_set$Bearing.2-min_max_train[3])/(min_max_train[4]-min_max_train[3])),
                                    Bearing.3 = ((train_set$Bearing.3-min_max_train[5])/(min_max_train[6]-min_max_train[5])),
                                    Bearing.4 = ((train_set$Bearing.4-min_max_train[7])/(min_max_train[8]-min_max_train[7])))

Z_test_set <- test_set %>% mutate(Bearing.1 = ((test_set$Bearing.1-min_max_train[1])/(min_max_train[2]-min_max_train[1])),
                                  Bearing.2 = ((test_set$Bearing.2-min_max_train[3])/(min_max_train[4]-min_max_train[3])),
                                  Bearing.3 = ((test_set$Bearing.3-min_max_train[5])/(min_max_train[6]-min_max_train[5])),
                                  Bearing.4 = ((test_set$Bearing.4-min_max_train[7])/(min_max_train[8]-min_max_train[7])))


par(mfrow=c(1,1))

# Reduce to two principal components the sensor readings for training data normalized
Train.pca <- prcomp(Z_train_set[2:5])
# Predict principal components based on Training ones using test set normalized
Test.pca <- predict(Train.pca, newdata = Z_test_set[2:5])

# Convert Matrix in Data Frame just for the first two principal components
Test.pca <- as.data.frame(Test.pca[,1:2])
Train.pca <- (Train.pca$x[,1:2])
Train.pca <- as.data.frame(Train.pca[,1:2])

# Since the unit of measure is the same the matrix of covariance can be used.
covariance_matrix <- cov(Train.pca)
inv_covariance_matrix <- cov(Train.pca)^-1
mean_distr <- colMeans(Train.pca)

# Mahalanobis distance calculation both for Train and Test Set
dist_train <- sqrt(mahalanobis(Train.pca, mean_distr, covariance_matrix, inverted = FALSE))
dist_test <- sqrt(mahalanobis(Test.pca, mean_distr, covariance_matrix, inverted = FALSE))

# Threshold calculation to identify outliers (anomalies) based on training data
MD_threshold <- function(dist, extreme){
  if (extreme) {k=3}
  else {k=2}
  threshold = mean(dist) * k
  return(threshold)
}


# The Hist to set the threshold to say when could be identified an anomaly 
d <- dist_train
hist(d, include.lowest = TRUE, right = TRUE, freq = FALSE,
     angle = 45, col = "LightBlue", 
     axes = TRUE, plot = TRUE, labels = FALSE,
     xlim = range(0,5), xlab ="Mahalanobis Distance", main = "Mahalanobis Distance Distribution")

# 3 sigma from mean distance has been used for upper threshold
threshold = MD_threshold(dist_train, extreme = TRUE)

# Threshold value
threshold

# Show the entire period observed joining Trainind and Testing Data
v_dist_train <-as.data.frame(dist_train)
v_dist_test <-as.data.frame(dist_test)
colnames(v_dist_train) <- "MD"
colnames(v_dist_test) <- "MD"
Train_Test <- rbind(v_dist_train, v_dist_test)

v_z_train_set <-as.data.frame(Z_train_set[1])
v_z_test_set <-as.data.frame(Z_test_set[1])
colnames(v_z_train_set) <- "Time"
colnames(v_z_test_set) <- "Time"
Ind_Train_Test <- rbind(v_z_train_set, v_z_test_set)

Train_Test <- log(Train_Test)

Anomaly <- as.data.frame(cbind(Ind_Train_Test,Train_Test))

df <- Anomaly %>% mutate(Time = as.POSIXct(Anomaly$Time, origin="1970-01-01", tz="UTC"))
p <- ggplot(df,aes(x=df$Time, y=df$MD))
p$labels$y <- "Gear Vibration"
p + scale_x_datetime(breaks = date_breaks("1 week"), name = "Time Monitoring") + 
  geom_line(col = "Blue") + 
  geom_hline(yintercept=log(threshold), col = "Red") +
  ggtitle("Engine failure long after crossing the red line")

# A zoom on the failure point

df <- Anomaly[5500:6324,] 
df <- df %>% mutate(Time = as.POSIXct(df$Time, origin="1970-01-01", tz="UTC"))

p <- ggplot(df,aes(x=df$Time, y=df$MD))
p$labels$y <- "Gear Vibration"
p + scale_x_datetime(breaks = date_breaks("1 day"), name = "Time Monitoring") + 
  geom_line(col = "Blue") + 
  geom_hline(yintercept=log(threshold), col = "Red") +
  ggtitle("Zoom on Engine failure")