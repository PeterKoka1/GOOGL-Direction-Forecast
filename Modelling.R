path <- "C:/Users/PeterKokalov/lpthw/Direction_Prediction/"
filename <- 'GOOGL-Final.csv'
get.csv <- function(path, csv) {
  df <- read.csv(paste0(path, csv), header = T)
  return(df)
}
df <- get.csv(path, filename)
date.col <- df['Date']
dir <- df$dir
dropcols <- c('X','Date','close.1','dir','Date.1','Date.2','Date.3')
preds <- df[, !(names(df) %in% dropcols)]
first <- preds[,1:7]
signals <- preds[,8:30]
stocks <- preds[,31:429]

par(mfrow=c(1,1))
plot(preds$close[100:600], type = "l", xlab = "600 Indexed Days", ylab = "Closing Prices USD", col = "darkblue")
lines(preds$SMA[100:600], type = "l", col = "darkgray")
lines(preds$EMA[100:600], type = "l", col = "red")
legend("topleft", legend=c("$GOOG", "SMA", "EMA"),
       col=c("darkblue","darkgray", "red"), lty=c(1,1,1), lwd=c(1.5,1.5,1.5), cex=0.8)

pr.out <- prcomp(preds, scale = TRUE)
pr.var <- pr.out$sdev^2
PVE <- pr.var / sum(pr.var)
PVE[1:7] # [1] 0.51780896 0.14742143 0.06471981 0.04605081 0.03212617 0.02244850 0.01945683
cum <- 0.0
for (i in 1:7) {
  cum <- cum + PVE[i]
}
cum # 85% of variance explained with 7 Principal Components

pve <- 100 * pr.out$sdev^2 / sum(pr.out$sdev^2)
par(mfrow = c(1,2))
plot(pve[1:20], type = "o", ylab = "Proportion of Variance Explained", xlab = "",col = "darkgray")
points(7, pve[7], pch=19, col = "darkblue")
plot(cumsum(pve)[1:20], type = "o", ylab = "Cumulative PVE", xlab = "", col = "darkgray")
points(7, cumsum(pve)[7], pch=19, col = "darkblue")

pcepreds <- data.frame(-pr.out$x[,1:7],dir)
attach(pcepreds)
glm.fit <- glm(dir ~., family="binomial", data=rets)
summary(glm.fit)
glm.probs <- predict(glm.fit, type="response")
glm.pred <- rep("False",dim(pcepreds)[1])
glm.pred[glm.probs>.5] <- "True" 
table(glm.pred,dir)
mean(glm.pred==dir) #60.2% TP Rate

###: TEST/TRAIN SPLIT
pcepreds <- data.frame(-pr.out$x[,1:7],dir)
preds.train <- pcepreds[1:2671,]
preds.test <- pcepreds[2672:3339,]
test.Direction <- dir[2672:3339]

###: LOGISTIC REGRESSION
attach(preds.train)
glm.fit <- glm(dir ~., data = preds.train, family = "binomial")
summary(glm.fit)$coef
glm.fit <- glm(dir ~ PC2 + PC5 + PC6 + PC7, data = preds.train, family = "binomial")
fitted.model.probs <- predict(glm.fit, newdata = preds.test, type = "response")
glm.preds <- rep('False', length(test.Direction))
glm.preds[fitted.model.probs > 0.5] = 'True'
table(glm.preds, test.Direction)
log.missclassError <- mean(glm.preds != test.Direction)
1 - log.missclassError #60.329% accuracy

###: QUADRATIC DISCRIMINANT ANALYSIS
library(MASS)
qda.fit <- qda(dir ~ PC2 + PC5 + PC6 + PC7, data=preds.train)
qda.class <- predict(qda.fit, newdata = preds.test)$class
table(qda.class, test.Direction)
qda.missclassError <- mean(qda.class != test.Direction) # 38.92% error
1 - qda.missclassError #61.07% accuracy

###: SUPPORT VECTOR MACHINE
library(e1071)
attach(pcepreds)
tune.out.rad <- tune(svm, dir ~ ., data = pcepreds, kernel = "radial", ranges = list(c(0.01, 0.1, 1, 10, 100, 100), 
                                                                                        gamma = c(0.1, 0.5, 1, 5)))
tune.out.rad$best.model
summary(tune.out.rad)

# 10-fold CV
# best parameters: cost: 1, gamma: 0.1
# best performance: 0.3944274
# number of support vectors: 2771

svm.fit <- svm(dir ~., data = preds.train, kernel = "radial", cost = 0.1, gamma = 0.1)
svm.preds <- predict(svm.fit, newdata = preds.test)
mean(svm.preds == test.Direction) # 61.07% accuracy
table(svm.preds,test.Direction)

###################
#### ALT. PCA #####
###################

pr.signals <- prcomp(signals, scale = TRUE)
pr.signals.var <- pr.signals$sdev^2
PVE.signals <- pr.signals.var / sum(pr.signals.var)
sum(PVE.signals[1:7]) #89.7%
pr.stocks <- prcomp(stocks, scale = TRUE)
pr.stocks.var <- pr.stocks$sdev^2
PVE.stocks <- pr.stocks.var / sum(pr.stocks.var)
sum(PVE.stocks[1:7]) #89%
pr.first <- prcomp(first, scale = TRUE)
pr.first.var <- pr.first$sdev^2
PVE.first <- pr.first.var / sum(pr.first.var)
sum(PVE.first[1:4]) #93%
principals <- data.frame(pr.stocks$x[,1:7], pr.signals$x[,1:7], pr.first$x[,1:4], dir)

attach(principals)
glm.fit <- glm(dir ~., data = principals, family = "binomial")
summary(glm.fit)
glm.probs <- predict(glm.fit, type = "response")
glm.pred <- rep("False",length(glm.probs))
glm.pred[glm.probs>.5] <- "True"
mean(glm.pred == dir) #69.48%

###: TEST/TRAIN SPLIT
train <- principals[1:2671,]
test <- principals[2672:3339,]
test.dir <- dir[2672:3339]

glm.fit <- glm(dir ~., data = train, family = "binomial")
frmla <- dir ~ PC5+PC1.1+PC2.1+PC4.1+PC5.1+PC7.1+PC1.2+PC2.2+PC3.2+PC4.2
glm.fit <- glm(frmla, data = train, family = "binomial")
fitted.model.probs <- predict(glm.fit, newdata = test, type = "response")
glm.preds <- rep('False', length(test.dir))
glm.preds[fitted.model.probs > 0.5] = 'True'
table(glm.preds, test.dir)
log.missclassError <- mean(glm.preds != test.dir)
1 - log.missclassError #67.365% accuracy

###: QUADRATIC DISCRIMINANT ANALYSIS
library(MASS)
qda.fit <- qda(frmla, data=train)
qda.class <- predict(qda.fit, newdata = test)$class
table(qda.class, test.dir)
qda.missclassError <- mean(qda.class != test.dir)
1 - qda.missclassError #62.724% accuracy

###: SUPPORT VECTOR MACHINE
library(e1071)
tune.out.rad <- tune(svm, frmla, data = data.frame(rbind(train,test)), kernel = "radial", ranges = list(c(0.01, 0.1, 1, 10, 100, 100), 
                                                                                     gamma = c(0.1, 0.5, 1, 5)))
tune.out.rad$best.model
summary(tune.out.rad)

# 10-fold CV
# best parameters: cost: 1, gamma: 0.1
# best performance: 0.332713

svm.fit <- svm(frmla, data = train, kernel = "radial", cost = 1, gamma = 0.1)
svm.preds <- predict(svm.fit, newdata = test)
mean(svm.preds == test.dir) # 70% accuracy
table(svm.preds,test.Direction)

write.csv(pcepreds, "totaldf.csv")
write.csv(preds.train, "train.csv")
write.csv(preds.test, "test.csv")
