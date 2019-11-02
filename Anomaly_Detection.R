library(dplyr)
library(tidyverse)
library(caret)
library(ggpubr)
library(StatMatch)  


library(scales)

#dat <- read.csv("C:/Lavoro/AnomalyDetect/merged_dataset_BearingTest_3.csv", sep=";", header = TRUE)
dat <- read.csv(url("https://raw.githubusercontent.com/PaoloDePiante/Anomaly_Detection/master/Dataset_for_Bearing.csv"), sep=",", header = TRUE)


head(dat)
set.seed(2)

train_set <- dat[which((as.POSIXct(dat$TS, origin="1970-01-01", tz="UTC") >= as.POSIXct('2004-03-04 09:27:46', origin="1970-01-01", tz="UTC")) & (as.POSIXct(dat$TS, origin="1970-01-01", tz="UTC") <= as.POSIXct('2004-04-14 20:51:57', origin="1970-01-01", tz="UTC"))),]
test_set <- dat[which(as.POSIXct(dat$TS, origin="1970-01-01", tz="UTC") >= as.POSIXct('2004-04-14 20:51:01', origin="1970-01-01", tz="UTC")),]

par(mfrow=c(4,1))
# esplora ggline e ggarrange
plot(train_set$Bearing.1, type="l", col="dark red")
plot(train_set$Bearing.2, type="l", col="blue")
plot(train_set$Bearing.3, type="l", col="green")
plot(train_set$Bearing.4, type="l", col="purple")

# Z_train_set <- train_set %>% mutate(Bearing.1 = scale(train_set$Bearing.1), Bearing.2 = scale(train_set$Bearing.2), Bearing3 = scale(train_set$Bearing3), Bearing.4 = scale(train_set$Bearing.4))
# il seguente esempio standardizza (ma non riduce a 0 e 1)
# X_train_scaled = scale(X_train)
# X_test_scaled = scale(X_test, center=attr(X_train_scaled, "scaled:center"), scale=attr(X_train_scaled, "scaled:scale"))
# si potrebbe usare la normalizzazione seguente ma non esporta il min e max da applicare poi al test set
# N_train_set <-normalize(train_set, range = c(0, 1), domain = range(x, na.rm = TRUE)

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


Train.pca <- prcomp(Z_train_set[2:5])
#summary(Train.pca)


Test.pca <- predict(Train.pca, newdata = Z_test_set[2:5])
#test.data <- as.data.frame(test.data)
Test.pca <- as.data.frame(Test.pca[,1:2])
Train.pca <- (Train.pca$x[,1:2])
Train.pca <- as.data.frame(Train.pca[,1:2])
#Test.pca <- prcomp(Z_test_set[2:5], center = TRUE, scale. = TRUE)
#summary(Test.pca)
#Test.pca <- Test.pca$x[,1:2]

#a_train_set <- train_set[,-1]

covariance_matrix <- cov(Train.pca)
inv_covariance_matrix <- cov(Train.pca)^-1
mean_distr <- colMeans(Train.pca)

dist_train <- sqrt(mahalanobis(Train.pca, mean_distr, covariance_matrix, inverted = FALSE))
dist_test <- sqrt(mahalanobis(Test.pca, mean_distr, covariance_matrix, inverted = FALSE))

MD_threshold <- function(dist, extreme){
  if (extreme) {k=3}
  else {k=2}
  threshold = mean(dist) * k
  return(threshold)
}

threshold = MD_threshold(dist_train, extreme = TRUE)

s <- (dist_train)^2
hist(s, include.lowest = TRUE, right = TRUE,
     angle = 45, col = "LightBlue", 
     axes = TRUE, plot = TRUE, labels = FALSE,
     ylim = range(0,80), xlim = range(0,14),
     warn.unused = TRUE, xlab = "Square of the Mahalanobis distance", main = "Square of the Mahalanobis distance Distribution")


d <- dist_train
hist(d, include.lowest = TRUE, right = TRUE, freq = FALSE,
     angle = 45, col = "LightBlue", 
     axes = TRUE, plot = TRUE, labels = FALSE,
     xlim = range(0,4), xlab ="Mahalanobis Distance", main = "Mahalanobis Distance Distribution")




# OK soglia ok 

threshold

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

write.table(Anomaly, file="C:/Lavoro/AnomalyDetect/paolo.csv", quote=T, sep=";", dec=".", na="NA", row.names=T, col.names=T)

plot(Anomaly)
title(main="Gear Vibration", col.main="blue", font.main="4")
lines(Anomaly, col = "Blue")
#abline(h=log(threshold))
abline(h=log(threshold), col = "Red")

df <- Anomaly %>% mutate(Time = as.POSIXct(Anomaly$Time, origin="1970-01-01", tz="UTC"))
p <- ggplot(df,aes(x=df$Time, y=df$MD))
p$labels$y <- "Gear Vibration"
p + scale_x_datetime(breaks = date_breaks("1 day"), name = "Time Monitoring") + geom_line(col = "Blue") + geom_hline(yintercept=log(threshold), col = "Red")
