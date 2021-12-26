# First regression in R
# Author: Amit Gunnika Karan Ravinder Sparsh
# Date: July22, 2019

# -----------------------------------------------------------------------
# Install relevant libraries
install.packages("caret")
install.packages("corrgram")
install.packages("car")
install.packages("carData")
install.packages("olsrr")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("magrittr") # only needed the first time you use it
install.packages("dplyr")    # alternative installation of the %>%
install.packages("Matrix")
install.packages("foreach")
install.packages("glmnet")
install.packages("ggplot2")
install.packages("forcats")
install.packages("reshape2")

# -----------------------------------------------------------------------
# Load in relevant libraries
library(caret)
library(corrgram)
library(carData)
library(car)
library(olsrr)
library(rpart)
library(rpart.plot)
library(magrittr)
library(dplyr)
library(Matrix)
library(foreach)
library(glmnet)
library(ggplot2)
library(forcats)
library(reshape2)

# -----------------------------------------------------------------------
# -----------------------------------------------------------------------
# Load in the data

rw <- read.csv("C:/Users/karan/OneDrive/Desktop/winequality-red.csv")
ww <- read.csv("C:/Users/karan/OneDrive/Desktop/winequality-white.csv")

# -----------------------------------------------------------------------
# Clean the data
# Cast the data objects into a data frame
rw <- data.frame(rw)
ww <- data.frame(ww)

rw$quality <- as.numeric(rw$quality) #confirm numeric

ww$quality <- as.numeric(ww$quality) #confirm numeric

# -----------------------------------------------------------------------
# Review the data
View(rw)
summary(rw) #Show point summary
str(rw) #structure of the data and allows us to see data types
dim(rw) # Dimensions of data
class(rw) #Numeric, matrix, data frame

View(ww)
summary(ww)
str(ww)
dim(ww)
class(ww)

#-----------------------------------------------------------------------
#Density Plots of Alcohol by wine Color
#-----------------------------------------------------------------------
dat = data.frame(count=c(1599, 4898), WineColor=c("Red Wine", "White Wine"))
fill <- c("tomato1","slategray1")



# Add addition columns, needed for drawing with geom_rect.
dat$fraction = dat$count / sum(dat$count)
dat = dat[order(dat$fraction), ]
dat$ymax = cumsum(dat$fraction)
dat$ymin = c(0, head(dat$ymax, n=-1))

p1 = ggplot(dat, aes(fill=WineColor, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(0, 4)) +
  theme(panel.grid=element_blank()) +
  theme(axis.text=element_blank()) +
  theme(axis.ticks=element_blank()) +
  annotate("text", x = 0, y = 0, label = "") +
  labs(title="") +
  scale_fill_manual(values=fill) +
  geom_label(aes(label=paste(round(fraction*100,2),"%"),x=3.5,y=(ymin+ymax)/2),inherit.aes = FALSE, show.legend = FALSE) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank())
p1


#-----------------------------------------------------------------------
#Density Plots of Alcohol by wine Color
#-----------------------------------------------------------------------
plot.new()
plot(density(rw$alcohol), main= "Density plot of Alcohol by Wine Color", frame.plot = FALSE,xlab="Alcohol Content", ylab="Density of Wine")
polygon(density(rw$alcohol),col=rgb(1,0.714,0.757,0.5),fillOddEven = TRUE,border = "pink")
polygon(density(ww$alcohol),col=rgb(0,1,1,0.2),fillOddEven = TRUE,border = "lightblue")
legend("topright",title="Wine Colour", 
       legend = c("Red wine", "White wine"), 
       col = c(rgb(1,0.714,0.757), 
               rgb(0,1,1,0.4)), 
       pch = c(15,15), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))

#-----------------------------------------------------------------------
#Checking Correlation
#-----------------------------------------------------------------------
red=rgb(1,0,0); green=rgb(0,1,0); blue=rgb(0,0,1); white=rgb(1,1,1)
RtoWrange<-colorRampPalette(c(red, white ) )
WtoGrange<-colorRampPalette(c(white, green) ) 

cordata <- round(cor(rw, method = "pearson"),2)
cordata1 <- round(cor(ww, method = "pearson"),2)

dd <- as.dist((1-cordata)/2)
hc <- hclust(dd)

dd1 <- as.dist((1-cordata1)/2)
hc1 <- hclust(dd1)

cordata <-cordata[hc$order, hc$order]
cordata[lower.tri(cordata)] <- NA
cordata <- melt(cordata,na.rm = TRUE)

cordata1 <-cordata1[hc1$order, hc1$order]
cordata1[lower.tri(cordata1)] <- NA
cordata1 <- melt(cordata1,na.rm = TRUE)


ggheatmap <- ggplot(data = cordata, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low=RtoWrange(100), mid=WtoGrange(100), high="gray") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1),
        axis.text.y = element_text(size = 12))+
  coord_fixed() + coord_flip()

ggheatmap <- ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.9, 0.1),
    legend.direction = "vertical")+
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 1))

print(ggheatmap)

ggheatmap1 <- ggplot(data = cordata1, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low=RtoWrange(100), mid=WtoGrange(100), high="gray") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1),
        axis.text.y = element_text(size = 12))+
  coord_fixed()

ggheatmap1 <- ggheatmap1 + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank())

print(ggheatmap1)
#-----------------------------------------------------------------------
# Regression Analysis
#-----------------------------------------------------------------------
# 80-20 train and test
set.seed(42)
n = nrow(rw)
trainIndex <- round(nrow(rw) * .80)
trainrw = rw[1:trainIndex ,]
testrw = rw[(trainIndex+1):n ,]

n = nrow(ww)
trainIndex <- round(nrow(ww) * .80)
trainww = ww[1:trainIndex ,]
testww = ww[(trainIndex+1):n ,]


#-----------------------------------------------------------------------
# Regression Analysis
fitrw <- lm(quality ~ ., data=trainrw)
vif(fitrw)
k <- ols_step_forward_aic(fitrw, details = TRUE)
plot(k)

fitww <- lm(quality ~ ., data=trainww)
vif(fitww)
k <- ols_step_forward_aic(fitww, details = TRUE)
plot(k)
#-----------------------------------------------------------------------
# Best model
fitrwbest <- lm(quality ~ alcohol + volatile.acidity + sulphates + total.sulfur.dioxide + 
                  chlorides + pH + free.sulfur.dioxide , data=trainrw)
vif(fitrwbest)
summary(fitrwbest)

mad(fitrwbest$residuals)

yhatcv <- predict(fitrwbest, testrw)
mean((testrw$quality - yhatcv)^2)

fitwwbest <- lm(quality ~ alcohol + volatile.acidity + residual.sugar+ fixed.acidity + free.sulfur.dioxide + 
                  sulphates + pH , data=trainww)
vif(fitwwbest)
summary(fitwwbest)

mad(residuals(fitwwbest))

yhatcv <- predict(fitwwbest, testww)
mean((testww$quality - yhatcv)^2)


-----------------------------------------------------------------------
  # Diagnostic plots 
layout(matrix(c(1,2,3,4),2,2))
plot(fitrwbest)
plot(fitwwbest)
par(mfrow=c(1,1))
#-----------------------------------------------------------------------
# Decission Tree Analysis
treeAnalysisrw <- rpart(fitrwbest,data=trainrw,cp=0.02)
rpart.plot(treeAnalysisrw, type=2,
           extra=100,
           box.palette="OrBu",
           branch.lty=1, 
           shadow.col="gray", 
           nn=TRUE,
           tweak = 1.2)

printcp(treeAnalysisrw)
plotcp(treeAnalysisrw) 
summary(treeAnalysisrw)
treeAnalysisrw$variable.importance


treeAnalysisww <- rpart(fitwwbest, data=trainww)
rpart.plot(treeAnalysisww, type=2,
           extra=100,
           box.palette="OrBu",
           branch.lty=1, 
           shadow.col="gray", 
           nn=TRUE,
           tweak = 0.9)

printcp(treeAnalysisww)
plotcp(treeAnalysisww) 
summary(treeAnalysisww)
treeAnalysisww$variable.importance


#-----------------------------------------------------------------------
#LASSO AND RIDGE
#-----------------------------------------------------------------------
#
# Red Wine Division y and x plus 80-20 train and test
#
#-----------------------------------------------------------------------
#data(rw)
# Center y, X will be standardized in the modelling function
rwy<- rw %>% select(quality) %>% scale(center = TRUE, scale = FALSE) %>% as.matrix()
rwX <- rw %>% select(-quality) %>% as.matrix()


set.seed(42)
n = nrow(rwX)
trainIndex <- round(nrow(rwX) * .80)

trainrwX = rwX[1:trainIndex ,]
testrwX = rwX[(trainIndex+1):n ,]

trainrwy = rwy[1:trainIndex ,]
testrwy = rwy[(trainIndex+1):n ,]

#-----------------------------------------------------------------------
#
# RIDGE REGRESSION FOR RED WINE
#
#-----------------------------------------------------------------------


# Perform 10-fold cross-validation to select lambda ---------------------------
# Setting alpha = 0 implements ridge regression
CV1 <- cv.glmnet(trainrwX, trainrwy, alpha = 0, nlambda = 100,
                      standardize = TRUE, nfolds = 10, )
# Plot cross-validation results
plot(CV1)

# Best cross-validated lambda
lambdaCV <- CV1$lambda.min
# Fit final model, get its sum of squared residuals and multiple R-squared
modelridge <- glmnet(trainrwX, trainrwy, alpha = 0, lambda = lambdaCV, standardize = TRUE)
yhatridge <- predict(modelridge, testrwX)
mean((testrwy - yhatridge)^2)

#-----------------------------------------------------------------------
#
# LASSO REGRESSION FOR RED WINE
#
#-----------------------------------------------------------------------

# Perform 10-fold cross-validation to select lambda ---------------------------
# Setting alpha = 1 implements lasso regression
CV2 <- cv.glmnet(trainrwX, trainrwy, alpha = 1, nlambda = 100,
                      standardize = TRUE, nfolds = 10)
# Plot cross-validation results
plot(CV2)


lambdaCV <- CV2$lambda.min
# Fit final model, get its sum of squared residuals and multiple R-squared
modelLasso <- glmnet(trainrwX, trainrwy, alpha = 1, lambda = lambdaCV, standardize = TRUE)
yhatlasso <- predict(modelLasso, testrwX)
rsqlasso <- cor(testrwy, yhatlasso)^2
mean((testrwy - yhatlasso)^2)
rsqlasso
modelLasso$beta[,1]
mad(residuals(modelLasso))

#######
fitrw <- lm(quality ~ volatile.acidity + chlorides + total.sulfur.dioxide + 
              pH + sulphates + alcohol,data=trainrw)
vif(fitrw)
summary(fitrw)

yhatcv <- predict(fitrw, testrw)
mean((testrw$quality - yhatcv)^2)

mad(fitrw$residuals, center = median(fitrw$residuals))


varImp(modelLasso, lambda = CV2$lambda.min)
#-----------------------------------------------------------------------
#
# White Wine Division y and x plus 80-20 train and test
#
#-----------------------------------------------------------------------
wwy <- ww %>% select(quality) %>% scale(center = TRUE, scale = FALSE) %>% as.matrix()
wwX <- ww %>% select(-quality) %>% as.matrix()

set.seed(42)
n = nrow(wwX)
trainIndex <- round(nrow(ww) * .80)

trainwwX = wwX[1:trainIndex ,]
testwwX = wwX[(trainIndex+1):n ,]

trainwwy = wwy[1:trainIndex ,]
testwwy = wwy[(trainIndex+1):n ,]

#-----------------------------------------------------------------------
#
# RIDGE REGRESSION FOR WHITE WINE
#
#-----------------------------------------------------------------------
# Perform 10-fold cross-validation to select lambda ---------------------------
# Setting alpha = 0 implements ridge regression
CV1 <- cv.glmnet(trainwwX, trainwwy, alpha = 0, nlambda = 100,
                 standardize = TRUE, nfolds = 10, )
# Plot cross-validation results
plot(CV1)

# Best cross-validated lambda
lambdaCV <- CV1$lambda.1se
# Fit final model, get its sum of squared residuals and multiple R-squared
modelridge <- glmnet(trainwwX, trainwwy, alpha = 0, lambda = lambdaCV, standardize = TRUE)
yhatridge <- predict(modelridge, testwwX)
mean((testwwy - yhatridge)^2)

#-----------------------------------------------------------------------
#
# LASSO REGRESSION FOR WHITE WINE
#
#-----------------------------------------------------------------------

# Perform 10-fold cross-validation to select lambda ---------------------------
# Setting alpha = 1 implements lasso regression
CV2 <- cv.glmnet(trainwwX, trainwwy, alpha = 1, nlambda = 100,
                 standardize = TRUE, nfolds = 10)
# Plot cross-validation results
plot(CV2)


lambdaCV <- CV2$lambda.1se
# Fit final model, get its sum of squared residuals and multiple R-squared
modelLasso <- glmnet(trainwwX, trainwwy, alpha = 1, lambda = lambdaCV, standardize = TRUE)
yhatlasso <- predict(modelLasso, testwwX)
rsqlasso <- cor(testwwy, yhatlasso)^2
mean((testwwy - yhatlasso)^2)
rsqlasso
modelLasso$beta[,1]

######
fitww <- lm(quality ~ chlorides + alcohol + volatile.acidity + fixed.acidity + residual.sugar +
              free.sulfur.dioxide + sulphates+ pH, data=trainww)
summary(fitww)
yhatcv <- predict(fittestww, testww)
mean((testww$quality - yhatcv)^2)

mad(fitww$residuals, center = median(fitww$residuals))
varImp(modelLasso, lambda = CV2$lambda.1se)
# ------------------------------------------------------------------------
# Descriptive Analysis
# ------------------------------------------------------------------------


spline.rw <- as.data.frame(spline(rw$alcohol, rw$quality))
spline.ww <- as.data.frame(spline(ww$alcohol, ww$quality))

ggplot(rw, aes(rw$alcohol, rw$quality))  + 
  scale_color_manual("Wine Color", values = c("Red Wine" = "#FF912C", "White Wine" = "#0B245B")) +
  geom_line(data = spline.ww, aes(x = x, y = y, color='White Wine'), size = 1.1) +
  geom_line(data = spline.rw, aes(x = x, y = y, color='Red Wine'), size = 1.1) +
  labs(x = "Alcohol", y = "Quality") +
  theme_bw() +
  theme(axis.title.x = element_text(size =12),
        axis.title.y = element_text(size =12),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.line = element_line(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#-------------------------------------------------------
#BarChart of Occupation 

winerw <- data.frame(quality = rw$quality)
wineww <- data.frame(quality = ww$quality)


winerw$color<- "red" 
wineww$color <- "white"

wine <- rbind(winerw,wineww)
wine$quality <- as.factor(wine$quality)
wine$color <- as.factor(wine$color)

fill <- c("#1F77B4", "#FF7F0E")
bp <- ggplot(wine, aes(x = fct_rev(fct_infreq(quality)), fill=color)) + 
  geom_bar(colour = "white") +
  theme_minimal()
bp <- bp + theme(
  axis.title.x = element_text(color = "black", size = 12, face = "bold"),
  axis.title.y = element_text(color = "black", size = 12, face = "bold"),
  axis.title = element_text(color="black", face="bold")) + 
  labs(title = "Occupation Categories " , x = "Occupation", y = "Number of Buyers") + 
  scale_fill_manual(values=fill) +
  theme_bw() +
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) + coord_flip()

bp


rw$quality <- as.numeric(rw$quality)
ww$quality <- as.numeric(ww$quality)

p <- ggplot(rw, aes(alcohol, quality,color="Red Wine"))
# Add regression line
p + scale_color_manual("Wine Color", values = c("Red Wine" = "red", "White Wine" = "lightblue")) +
  geom_smooth(data = ww, aes(alcohol, quality, color="White Wine"), size = 1.2,method = lm,se = F) +
  geom_smooth(method = lm, se = F,size = 1.2) + 
  theme_bw() +
  labs(x = "Alcohol", y = "Quality") +
  theme(axis.title.x = element_text(size =12),
        axis.title.y = element_text(size =12),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.line = element_line(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


p <- ggplot(rw, aes(quality, volatile.acidity,color="Red Wine"))
# Add regression line
p + scale_color_manual("Wine Color", values = c("Red Wine" = "red", "White Wine" = "lightblue")) +
   geom_smooth(method = lm, se = F,size = 1.2) + 
  theme_bw() +
  labs(x = "Quality", y = "Volatile Acidity") +
  theme(axis.title.x = element_text(size =12),
        axis.title.y = element_text(size =12),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.line = element_line(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


xp <- ggplot(rw, aes(chlorides, quality,color="Red Wine"))
# Add regression line
p + scale_color_manual("Wine Color", values = c("Red Wine" = "red", "White Wine" = "lightblue")) +
  geom_smooth(data = ww, aes(chlorides, quality, color="White Wine"), size = 1.2,method = lm,se = F) +
  geom_smooth(method = lm, se = F,size = 1.2) + 
  theme_bw() +
  theme(axis.line = element_line(),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank())

p <- ggplot(rw, aes(pH, quality,color="Red Wine"))
# Add regression line
p + scale_color_manual("Wine Color", values = c("Red Wine" = "red", "White Wine" = "lightblue")) +
  geom_smooth(data = ww, aes(pH, quality, color="White Wine"), size = 1.2,method = lm,se = F) +
  geom_smooth(method = lm, se = F,size = 1.2) + 
  theme_bw() +
  labs(x = "pH", y = "Quality") +
  theme(axis.title.x = element_text(size =12),
        axis.title.y = element_text(size =12),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.line = element_line(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
rw$quality <- as.factor(rw$quality)

ggplot(data=rw, aes(x=quality, y=volatile.acidity)) +
  geom_jitter( alpha = .5,color = "lightslateblue") +
  geom_boxplot(alpha = .5,color = 'red') +
  theme_bw() +
  theme(axis.line = element_line(),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank())
ww$quality <- as.factor(ww$quality)
ggplot(data=ww, aes(x=quality, y=volatile.acidity)) +
  geom_jitter( alpha = .5,color = "lightslateblue") +
  geom_boxplot(alpha = .5,color = 'red') +
  theme_bw() +
  theme(axis.line = element_line(),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank())

rw$quality <- as.factor(rw$quality)

ggplot(data=rw, aes(x=quality, y=pH)) +
  geom_jitter( alpha = .5,color = "lightslateblue") +
  geom_boxplot(alpha = .5,color = 'red') +
  theme_bw() +
  theme(axis.line = element_line(),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank())
ww$quality <- as.factor(ww$quality)
ggplot(data=ww, aes(x=quality, y=pH)) +
  geom_jitter( alpha = .5,color = "lightslateblue") +
  geom_boxplot(alpha = .5,color = 'red') +
  theme_bw() +
  theme(axis.line = element_line(),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank())



# -----------------------------------------------------------------------
# Create a new data frame, append it, and save it
aution_data_complete <- cbind(auction_data, fitted(fit))
View(aution_data_complete)
write.csv("c:/v....")