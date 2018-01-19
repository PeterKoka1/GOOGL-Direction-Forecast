setwd("C:/Users/PeterKokalov/lpthw/Direction_Prediction")
library(tseries);library(quantmod)
df <- read.csv('GOOGL-Final.csv')
date.col <- df['Date']
dir <- df$dir
dropcols <- c('X','Date','close.1','dir','Date.1','Date.2','Date.3')
preds <- df[, !(names(df) %in% dropcols)]

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
plot(pve, type = "o", ylab = "PVE", xlab = "Principal Component",
     col = "darkblue")
plot(cumsum(pve), type = "o", ylab = "Cumulative PVE", xlab = "Principal Component",
     col = "darkgray")

pcepreds <- data.frame(-pr.out$x[,1:7],dir)
attach(pcepreds)
glm.fit <- glm(
  dir ~., family="binomial", data=pcepreds
)
summary(glm.fit)
summary(glm.fit)$coef[,4]
glm.probs <- predict(glm.fit, type="response")
glm.pred <- rep("False",dim(pcepreds)[1])
glm.pred[glm.probs>.5] <- "True" 
table(glm.pred,dir)
mean(glm.pred==dir) #60.2% TP Rate

##: t-kna vs PCA




preds.train <- rep(FALSE,dim(preds)[1])
preds.train[1:2600] <- TRUE
preds.test <- preds[(!preds.train),]
test.Direction <- as.factor(preds$dir.change[!train])
attach(preds)

###: LOGISTIC REGRESSION
glm.fit <- glm(dir.change ~ lag2 + lag5, data = preds, 
               family = binomial, subset = preds.train)
glm.probs <- predict(glm.fit, newdata = preds.test, type = "response")
glm.preds <- rep(0, length(test.Direction))
glm.preds[glm.probs > 0.5] = 1
table(glm.preds, test.Direction)
mean(glm.preds == test.Direction) # 52.71% accuracy

###: QUADRATIC DISCRIMINANT ANALYSIS
library(MASS)
qda.fit <- qda(dir.change ~ lag2 + lag5,
               data=preds,
               subset=preds.train)
qda.class <- predict(qda.fit, newdata = preds.test)$class
table(qda.class, test.Direction)
mean(qda.class == test.Direction) # 52.71% accuracy

###: SUPPORT VECTOR MACHINE
library(e1071)
tune.out.rad <- tune(svm, dir.change ~., data = preds, kernel = "radial", ranges = list(c(0.01, 0.1, 1, 10, 100, 100), 
                                                                                        gamma = c(0.1, 0.5, 1, 5)))

summary(tune.out.rad)

tune.out.poly <- tune(svm, dir.change ~., data = preds, kernel = "polynomial", ranges = list(c(0.001, 0.01, 0.1, 1, 10, 100, 100, 1000), 
                                                                                             gamma = c(0.1, 0.5, 1, 5, 50, 500, 5000)))
summary(tune.out.poly)

svm.fit <- svm(as.factor(dir.change) ~., data = preds, kernel = "linear", cost = 0.1)
preds <- predict(svm.fit, data = preds.test)
?tune
?svm
plot(svm.fit, dir.change) # all observations classified to one class



svm.rad <- svm(as.factor(y) ~., data = dat, kernel = "radial", gamma = 1, cost = 0.1)
preds <- predict(svm.rad, data = dat)
par(mfrow=c(1,1))
plot(svm.rad, dat)
plot(dat[preds == 1, "x1"], dat[preds == 1, "x2"], col = "darkblue")
points(dat[preds == 0, "x1"], dat[preds == 0, "x2"], col = "red")

