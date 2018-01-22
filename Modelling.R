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
qda.missclassError <- mean(qda.class != test.Direction) # 52.71% accuracy
1 - qda.missclassError #61.07% accuracy

###: RANDOM FOREST
fmla = dir ~.
library(randomForest)
?randomForest
fit.rf = randomForest(fmla, data=pcepreds)
print(fit.rf)
importance(fit.rf)
plot(fit.rf)
plot( importance(fit.rf), lty=2, pch=16)
lines(importance(fit.rf))
imp = importance(fit.rf)
impvar = rownames(imp)[order(imp[, 1], decreasing=TRUE)]
op = par(mfrow=c(1, 3))
for (i in seq_along(impvar)) {
  partialPlot(fit.rf, pcepreds, impvar[i], xlab=impvar[i],
              main=paste("Partial Dependence on", impvar[i]),
              ylim=c(0, 1))
}


###: SUPPORT VECTOR MACHINE
library(e1071)
attach(pcepreds)
tune.out.rad <- tune(svm, dir ~., data = pcepreds, kernel = "radial", ranges = list(c(0.01, 0.1, 1, 10, 100, 100), 
                                                                                        gamma = c(0.1, 0.5, 1, 5)))
summary(tune.out.rad)

tune.out.poly <- tune(svm, dir ~., data = preds, kernel = "polynomial", ranges = list(c(0.001, 0.01, 0.1, 1, 10, 100, 100, 1000), 
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

