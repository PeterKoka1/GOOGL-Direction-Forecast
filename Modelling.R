path <- "C:/Users/PeterKokalov/lpthw/Direction_Prediction/"
filename <- 'GOOGL-Final.csv'
setwd(path)
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

plt.goog <- function(dat) {
  par(mfrow=c(1,1))
  plot(dat$close[100:600], type = "l", xlab = "600 Indexed Days", ylab = "Closing Prices USD", col = "darkblue")
  lines(dat$SMA[100:600], type = "l", col = "darkgray")
  lines(dat$EMA[100:600], type = "l", col = "red")
  legend("topleft", legend=c("$GOOGL", "SMA", "EMA"),
         col=c("darkblue","darkgray", "red"), lty=c(1,1,1), lwd=c(1.5,1.5,1.5), cex=0.8)
}
plt.goog(preds)

##############
### NO PCA ###
##############

preds.train <- data.frame(preds[1:2700,], dir[1:2700])
preds.test <- data.frame(preds[2701:3338,], dir[2701:3338])
test.dir <- dir[2701:3338]

###: LOGISTIC REGRESSION
attach(preds.train)
glm.fit <- glm(preds.train$dir.1.2700. ~., data = preds.train, family = "binomial")
summary(glm.fit)
to.select <- summary(glm.fit)$coeff[-1,4] < 0.30
relevant.x <- names(to.select)[to.select == TRUE] 
frmla <- as.formula(paste("dir.1.2700. ~",relevant.x))  

glm.fit <- glm(frmla, data = preds.train, family = "binomial")
log.acc <- function(fit) {
  fitted.model.probs <- predict(glm.fit, newdata = preds.test, type = "response")
  glm.preds <- rep('False', length(test.dir))
  glm.preds[fitted.model.probs > 0.5] = 'True'
  print(table(glm.preds, test.dir))
  log.missclassError <- mean(glm.preds != test.dir)
  sprintf("%f accuracy", (1 - log.missclassError)) 
}
log.acc(glm.fit) #54.7% accuracy

###: QUADRATIC DISCRIMINANT ANALYSIS
library(MASS)
qda.fit <- qda(frmla, data=preds.train)
qda.acc <- function(fit) {
  qda.class <- predict(fit, newdata = preds.test)$class
  print(table(qda.class, test.dir))
  qda.missclassError <- mean(qda.class != test.dir) 
  sprintf("%f accuracy", (1 - qda.missclassError))   
}
qda.acc(qda.fit) #54.5% accuracy

###: SUPPORT VECTOR MACHINE
library(e1071)
tune.svm <- function(data) {
  df.svm <- data.frame(preds, dir)
  tune.out.rad <- tune(svm, dir ~ ., data = df.svm, kernel = "radial", ranges = list(c(0.01, 0.1, 1), gamma = c(0.1, 0.5, 1)))
  tune.out.rad$best.model
  summary(tune.out.rad)  
}

attach(preds.train)
names(preds.train)
svm.fit <- svm(frmla, data = preds.train, kernel = "radial", cost = 0.01, gamma = 1)
svm.acc <- function() {
  svm.preds <- predict(svm.fit, newdata = preds.test)
  print(table(svm.preds,test.dir))
  sprintf("%f accuracy", mean(svm.preds==test.dir))  
  return(svm.preds)
}
svm.acc() #54.2% accuracy - classifies all into True

#####################
### PCA Version 1 ###
#####################

pr.out <- prcomp(preds, scale = TRUE)
pr.var <- pr.out$sdev^2
PVE <- pr.var / sum(pr.var)
PVE[1:7] # [1] 0.51780896 0.14742143 0.06471981 0.04605081 0.03212617 0.02244850 0.01945683
cum <- 0.0
for (i in 1:7) {
  cum <- cum + PVE[i]
}
cum # 85% of variance explained with 7 Principal Components

pcaplots <- function() {
  pve <- 100 * pr.out$sdev^2 / sum(pr.out$sdev^2)
  par(mfrow = c(1,2))
  plot(pve[1:20], type = "o", ylab = "Proportion of Variance Explained", xlab = "",col = "darkgray")
  points(7, pve[7], pch=19, col = "darkblue")
  plot(cumsum(pve)[1:20], type = "o", ylab = "Cumulative PVE", xlab = "", col = "darkgray")
  points(7, cumsum(pve)[7], pch=19, col = "darkblue")  
}
pcaplots()

###: TEST/TRAIN SPLIT
pcepreds <- data.frame(-pr.out$x[,1:7],dir)
preds.train <- pcepreds[1:2700,]
preds.test <- pcepreds[2701:3338,]
test.Direction <- dir[2701:3338]

###: LOGISTIC REGRESSION
attach(preds.train)
glm.fit <- glm(dir ~., data = preds.train, family = "binomial")
summary(glm.fit)
glm.fit <- glm(dir ~ PC5, data = preds.train, family = "binomial")
log.acc <- function(fit) {
  fitted.model.probs <- predict(fit, newdata = preds.test, type = "response")
  glm.preds <- rep('False', length(test.Direction))
  glm.preds[fitted.model.probs > 0.5] = 'True'
  print(table(glm.preds, test.Direction))
  log.missclassError <- mean(glm.preds != test.Direction)
  sprintf("%f accuracy", (1 - log.missclassError)) 
}
log.acc(glm.fit) #50.8% accuracy

###: QUADRATIC DISCRIMINANT ANALYSIS
library(MASS)
qda.fit <- qda(dir ~ PC5, data=preds.train)
qda.acc <- function(fit) {
  qda.class <- predict(fit, newdata = preds.test)$class
  print(table(qda.class, test.Direction))
  qda.missclassError <- mean(qda.class != test.Direction) 
  sprintf("%f accuracy", (1 - qda.missclassError))  
}
qda.acc(qda.fit) #50.5% accuracy

###: SUPPORT VECTOR MACHINE
library(e1071)
attach(pcepreds)
tune.svm <- function(data) {
  tune.out.rad <- tune(svm, dir ~ ., data = pcepreds, kernel = "radial", ranges = list(c(0.01, 0.1, 1, 10, 100, 100), gamma = c(0.1, 0.5, 1, 5)))
  tune.out.rad$best.model
  summary(tune.out.rad)  
}

attach(preds.train)
svm.fit <- svm(dir ~., data = preds.train, kernel = "radial", cost = 1, gamma = 0.1)
svm.acc <- function(fit) {
  svm.preds <- predict(fit, newdata = preds.test)
  print(table(svm.preds,test.Direction))
  sprintf("%f accuracy", mean(svm.preds==test.Direction))  
}
svm.acc(svm.fit) #50% accuracy

###################
#### ALT. PCA #####
###################

alternative.pca <- function() {
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
  return(principals)
}
principals <- alternative.pca()

testerr <- data.frame(
  diff(principals$PC1, lag = 1, differences = 1),
  diff(principals$PC2, lag = 1, differences = 1),
  diff(principals$PC3, lag = 1, differences = 1),
  diff(principals$PC4, lag = 1, differences = 1),
  diff(principals$PC5, lag = 1, differences = 1),
  diff(principals$PC6, lag = 1, differences = 1),
  diff(principals$PC7, lag = 1, differences = 1),
  diff(principals$PC1.1, lag = 1, differences = 1),
  diff(principals$PC2.1, lag = 1, differences = 1),
  diff(principals$PC3.1, lag = 1, differences = 1),
  diff(principals$PC4.1, lag = 1, differences = 1),
  diff(principals$PC5.1, lag = 1, differences = 1),
  diff(principals$PC6.1, lag = 1, differences = 1),
  diff(principals$PC7.1, lag = 1, differences = 1),
  diff(principals$PC1.2, lag = 1, differences = 1),
  diff(principals$PC2.2, lag = 1, differences = 1),
  diff(principals$PC3.2, lag = 1, differences = 1),
  diff(principals$PC4.2, lag = 1, differences = 1)
)
principalC <- data.frame(na.omit(testerr), principals$dir[2:length(principals$dir)])

colnames(principalC) <- c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8",
                       "PC9","PC10","PC11","PC12","PC13","PC14","PC15","PC16",
                       "PC17","PC18","dir")

glm.fit <- glm(principalC$dir ~., data = principalC, family = "binomial")
summary(glm.fit)
glm.probs <- predict(glm.fit, type = "response")
glm.pred <- rep("False", length(glm.probs))
glm.pred[glm.probs>0.5] <- "True"
mean(glm.pred == principalC$dir) #55%

###: TEST/TRAIN SPLIT
dim(principalC)
train <- principalC[1:2700,]
test <- principalC[2701:3337,] # 2015-07-08 to 2018-01-16 == 2y8mo
test.dir <- principalC$dir[2701:3337]

frmla <- dir ~ PC1 + PC3 + PC7 + PC9
log.qda.fits <- function(frmla) {
  glm.fit <- glm(dir ~., data = train, family = "binomial")
  summary(glm.fit)
  glm.fit <- glm(frmla, data = train, family = "binomial")
  fitted.model.probs <- predict(glm.fit, newdata = test, type = "response")
  glm.preds <- rep('False', length(test.dir))
  glm.preds[fitted.model.probs > 0.5] = 'True'
  table(glm.preds, test.dir)
  log.missclassError <- mean(glm.preds != test.dir)
  library(MASS)
  qda.fit <- qda(frmla, data=train)
  qda.class <- predict(qda.fit, newdata = test)$class
  table(qda.class, test.dir)
  qda.missclassError <- mean(qda.class != test.dir)
  sprintf("Log Reg: %f, QDA: %f", 1 - log.missclassError, 1 - qda.missclassError)
}
log.qda.fits(frmla) #"Log Reg: 0.54.4, QDA: 0.53"

###: SUPPORT VECTOR MACHINE
tune.svm <- function(frlma) {
  library(e1071)
  tune.out.rad <- tune(svm, frmla, data = data.frame(rbind(train,test)), kernel = "radial", ranges = list(c(0.01, 0.1, 1, 10, 100, 100), 
                                                                                                          gamma = c(0.1, 0.5, 1, 5)))
  print(tune.out.rad$best.model)
  summary(tune.out.rad)
}

attach(principalC)
svm.fit <- svm(frmla, data = train, kernel = "radial", cost = 1, gamma = 1)
svm.acc <- function(fit) {
  svm.preds <- predict(svm.fit, newdata = test)
  table(svm.preds,test.dir)  
  sprintf("SVM: %f", mean(svm.preds == test.dir)) #53.2%
  return(svm.preds)
}

library(quantmod);library(tseries)
final.preds1 <- svm.preds 
export1 <- rep(1, length(final.preds1))
for (i in 1:length(final.preds1)) {
  if (final.preds1[i] == 'False') {
    export1[i] = -1
  }
}
final.preds2 <- glm.preds 
export2 <- rep(1, length(final.preds2))
for (i in 1:length(final.preds2)) {
  if (final.preds2[i] == 'False') {
    export2[i] = -1
  }
}
final.preds3 <- qda.class 
export3 <- rep(1, length(final.preds3))
for (i in 1:length(final.preds3)) {
  if (final.preds3[i] == 'False') {
    export3[i] = -1
  }
}
GOOGL.close <- preds$close[2701:3337]
export.final <- function(col1, col2, col3, col4, filename) {
  write.csv(data.frame(col1,col2,col3,col4,Delt(GOOGL.close, k = 1, type = "arithmetic")), filename)
}
name <- 'final.csv'
export.final(export1, export2[1:637], export3[1:637], GOOGL.close, name)

read.CR <- function(path) {
  cum.rets <- read.csv(path, header = TRUE)
}

cum.rets <- na.omit(read.CR(path = 'Returns.csv'))[2:5]
par(mfrow=c(1,1))
plot(cum.rets$SVM, type = "l", col = "darkblue", ylab = "$GOOGL Direction Forecast Portfolios", xlab = "2015-07-08 to 2018-01-16")
(cum.rets$SVM[636]) * 100
# 79.867% return

par(mfrow=c(1,1))
plot(cum.rets$SVM, type = "l", col = "darkblue", ylab = "Returns", xlab = "First 200 Days")
lines(cum.rets$daily_rets, type = "l", col = "darkgray")
lines(cum.rets$QDA, type = "l", col = "red")
lines(cum.rets$LogReg, type = "l", col = "blue")
legend("topleft", legend=c("SVM","QDA","Log Reg","$GOOGL"),
       col=c("darkblue","darkgray","red","blue"), lty=c(1,1,1,1), lwd=1.5, cex=0.8)

sharpe(cum.rets$SVM, r = 0) # 2.24
sharpe(cum.rets$QDA, r = 0) # 1.37
sharpe(cum.rets$LogReg, r = 0) # 1.54
maxdrawdown(cum.rets$SVM)$maxdrawdown * 100 # 39.31%
maxdrawdown(cum.rets$QDA)$maxdrawdown * 100 # 14.6%
maxdrawdown(cum.rets$LogReg)$maxdrawdown * 100 # 14.59%
maxdrawdown(cum.rets$daily_rets)$maxdrawdown * 100 # 14.04%
