############################################################################################
####################################### OUTLINE ############################################
############################################################################################

#0) get the data
library(readxl)
data_players <- as.data.frame(read_excel("~/Desktop/R2/data_baseball.xlsx", sheet = "Players_info"))
data_log <- as.data.frame(read_excel("~/Desktop/R2/data_baseball.xlsx", sheet = "Pitchers_log"))

# the .xlsx file has a spreedsheat with only pitcher gaming log, so merging it with the players 
#info by names will result in only combining pitcher data.

data_pitchers<-merge(x = data_log, y = data_players, by.x = "PITCHERS", by.y = "name")

N = 407  #entries

#1) We are trying to predict if a team is going to win based on its pitcher statistic & salary
#      W = fct(IP, K, BB, ERA, WHIP, sal)
#             = t(beta) %*% [IP, K, BB, WHIP, sal, 1]
      
#H0: beta = [0 0 0 0 0 0]

#H1: beta != [0 0 0 0 0 0]

#2) random variable: salary, W, IP, K, BB, ERA, WHIP
#    distribution: Z- distribution
#    Z-transformation
zW<- (data_pitchers$W-mean(data_pitchers$W))/sd(data_pitchers$W, na.rm = TRUE)
zIP<- (data_pitchers$IP-mean(data_pitchers$IP))/sd(data_pitchers$IP, na.rm = TRUE)
zK<- (data_pitchers$K-mean(data_pitchers$K))/sd(data_pitchers$K, na.rm = TRUE)
zBB<- (data_pitchers$BB-mean(data_pitchers$BB))/sd(data_pitchers$BB, na.rm = TRUE)
zERA<- (data_pitchers$ERA-mean(data_pitchers$ERA))/sd(data_pitchers$ERA, na.rm = TRUE)
zWHIP<- (data_pitchers$WHIP-mean(data_pitchers$WHIP))/sd(data_pitchers$WHIP, na.rm = TRUE)
zsal<- (data_pitchers$sal-mean(data_pitchers$sal))/sd(data_pitchers$sal, na.rm = TRUE)

W<-data_pitchers$W
IP<-data_pitchers$IP
K<-data_pitchers$K
BB<-data_pitchers$BB
ERA<-data_pitchers$ERA
WHIP<-data_pitchers$WHIP
Salary<-data_pitchers$sal

plot(density(W))

# scatterplots & histograms
panel.hist <- function(x, ...)  {
  # Set user coordinates of plotting region
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5))
  par(new=TRUE)                    # Do not start new plot
  hist(x, prob=TRUE, axes=FALSE, xlab="", ylab="", 
       main="", col="lightgray")
  lines(density(x, na.rm=TRUE))    # Add density curve
}

pairs(~ W + IP + K + BB + ERA + WHIP + Salary, data=data_pitchers, 
      lower.panel = panel.smooth, upper.panel = NULL, 
      diag.panel = panel.hist)

# multiboxplots

par(mfrow=c(2, 3))
boxplot(data_pitchers$sal, main="Salary")
boxplot(data_pitchers$IP, main="IP")
boxplot(data_pitchers$K, main="K")
boxplot(data_pitchers$BB, main="BB")
boxplot(data_pitchers$ERA, main="ERA")
boxplot(data_pitchers$WHIP, main="WHIP")

#4) alpha = 0.05
#  non-directionnal   # 2*pnorm   alpha/2
  
#5)
#  linear model
  SmodelT<-lm(W ~ Salary + IP + K + BB + ERA + WHIP)
  Smodel<-lm(W ~ Salary + IP + K + BB)
  summary(Smodel)

# independence
  COVm<-vcov(SmodelT)
  1e3*COVm  

  # zIP ~ (zK, zBB)
  # zERA ~(zWHIP)
  
# variable selection BETA<-beta 
  VAR_BETA <- cbind(zsal, zK, zBB, zWHIP)
  
# performance_VAR<-t(BETA) %*% variables  

BETA1 <- c(coefficients(Smodel)[2], coefficients(Smodel)[4], coefficients(Smodel)[5], coefficients(Smodel)[7])
data_pitchers$performance_VAR <- as.matrix(VAR_BETA) %*% as.matrix(BETA)
    
 performance_MODEL<-lm(data_pitchers$sal ~ data_pitchers$performance_VAR)  
 summary(performance_MODEL)

BETA11 = coefficients(performance_MODEL)
data_pitchers$performance_MODEL<-  t(BETA11) %*% variables

# normal probability plots  
  par(mfrow=c(1, 1))
  performance_MODEL.stdres = rstandard(Smodel)
  qqnorm(performance_MODEL.stdres, ylab="Standardized Residuals", xlab="Theoretical Quantiles", main="Normal Q-Q")
  qqline(performance_MODEL.stdres)
  

  
#a<-data_pitchers$performance_MODEL[data_pitchers$performance_MODEL > 0.14 & data_pitchers$performance_MODEL < 0.25,]
  a<-data_pitchers$performance_MODEL
  qqnorm(a, ylab="Standardized Residuals", xlab="Theoretical Quantiles", main="Normal Q-Q")
qqline(a)  

#performance_MODEL<-lm(data_pitchers$sal ~ data_pitchers$performance_VAR)  
#summary(performance_MODEL)  
  
  
# the model incorrectly represents the distribution for the highest quantile   
  
  BETA2 <- c(coefficients(Smodel)[1], coefficients(Smodel)[2])
 # BETA21 = coefficients(performance_MODEL)
  
  
  data_pitchers$performance_MODEL<- cbind(1,data_pitchers$performance_VAR)  %*% BETA2
  
# Z-test
  Z_score = (mean(data_pitchers$performance_MODEL - mean(data_pitchers$W)))/(sd(data_pitchers$W)/sqrt(N))
  p_value = 2*pnorm(-abs(Z_score))
  
# chi2 test
chisq.test(data_pitchers$performance_VAR, data_pitchers$sal)

# k-fold cross validation
library(DAAG)
database<-as.data.frame(cbind(Salary, IP, W, K, BB))
cv.lm(data=database, Smodel, m=3, main = "cross-validation")

#6) 
library (car)
ncvTest(Smodel)
spreadLevelPlot(Smodel)

plot(Smodel) # Residuals vs Fitted / Normal Q-Q / Scale-Location / Residuals vs Leverage

#7) prospect




