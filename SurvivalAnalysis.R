#Samantha Maillie Survival Analysis Final Project
#The comments in this code utilize information cited in the 
#corresponding report
library(survival)
library(SurvRegCensCov)
library(forestplot)
data <- read.csv("https://query.data.world/s/JiORcJrs7jFb135GKFPgfbH1J3_c2H",
                 header=FALSE, stringsAsFactors=FALSE);
names(data) = c("SurvMonth", "Status0Dead", "age", "periEff", 
                "fracShort", "epss", "lvdd", "WMscore", "WMindex", "mult",
                "name", "group", "Deadin1stYear_0")
##There are some variables that are not needed and will prove of no value
##I will remove them before going further. Name is not relevant. 
##The creator of the data set put that group and mult are useless
##It was also stated to use the WM index in lieu of the WM score.
##Lastly the data contained in dead within a year is contained 
##within the survival months already so it is also irrelevant. 
##so I will not consider them as covariates. 

#Also note that the age is the age at the time of the heart attack

cardio <-data
cardio$name <- NULL
cardio$group <- NULL
cardio$mult <- NULL
cardio$WMscore <-NULL
cardio$Deadin1stYear_0 <- NULL
cardio

#All of the data is supposed to be numeric so I am going to set it
#as numeric to make some histograms to better examine the data

cardio$SurvMonth = as.numeric(cardio$SurvMonth)
cardio$Status0Dead = as.numeric(cardio$Status0Dead)
cardio$age = as.numeric(cardio$age)
cardio$periEff = as.numeric(cardio$periEff)
cardio$fracShort = as.numeric(cardio$fracShort)
cardio$epss = as.numeric(cardio$epss)
cardio$lvdd = as.numeric(cardio$lvdd)
cardio$WMindex = as.numeric(cardio$WMindex)

#Based on the American Heart Association's publications being over 
#the age of 65 years old is considered a high risk for a heart attack. 
#Someone under 65y/o is considered to be 'young' so I thought it would
#be interesting to look at the difference in survival post-heart attack 
#between below 65y/o and above. The histogram for ages is approximately 
#normal centered around 65. 0 = less than 65 1 = 65+

cardio$age[cardio$age< 65] <- 0 # <65
cardio$age[cardio$age>=65] <- 1 # >=65


#Fractional Shortening: if it fails to shorten by at least 28% then
#this indicates that the efficiency of the heart ejecting blood is 
#impaired. Therefore this will be split into patients that shorten
#by at least 28% and those that fail to do so. 0 will be impaired
#or less than .28 and 1 will be functionally normally or .28+

cardio$fracShort[cardio$fracShort< .28] <- 0
cardio$fracShort[cardio$fracShort>= .28] <- 1



#EPSS is a way of assessing the heart's ejection fraction or in other
#words the percentage of blood exiting the heart at each contraction. 
#A defining feature of systolic heart failure is a lower ejection
#fraction. An ejection fraction(EF) less than 30% is considered bad. 
#There is a negative linear relationship between EF and EPSS. 
#An EPSS of 7mm or above (corresponding to EF <= 30%) is considered
#to be a problem. 0 will be an EPSS less than 7mm 1 will be patients 
#with with EPSS 7mm or greater


cardio$epss[cardio$epss< 7] <- 0 #good 
cardio$epss[cardio$epss >= 7] <- 1 #bad


#It was noted in the documentation for the data set that 'large hearts
#tend to be sick hearts'. I found a baseline online that put the average
#range for LVDD (aka LVEDD a heart measurement) to be 47.86mm +/ 4.3mm
#The dataset did not specifically state that the collected data was in 
#cm however it appears clear that this is the case based on the provided
#data's range of values and mean. The mean is 4.765 with a range of 
#2.32 to 6.78. Assuming the measurements are in cm this data makes sense
#given the documented baseline information. 
#Therefore the average heart should be expected to have an LVDD 
#between 4.74 and 4.826cm. To start I will look at small, in range
#and large values. Though given the documentation's suggestion that
#large hearts tend to be sick hearts I may later consider large and
#not large for categories. Update: Based on further analysis this was
#changed from 3 factor levels to 2

#Previous code from 3 factor levels
#cardio$lvdd <- cut(cardio$lvdd, breaks=c(-Inf, 4.74, 4.826, Inf),
#                   labels=c(0,1,2))
#pie(table(cardio$lvdd), main = 'LVDD', 
#    labels = c('Small LVDD', 'In Range LVDD', 'Large LVDD'))


cardio$lvdd[cardio$lvdd< 4.826] <- 0 #good 
cardio$lvdd[cardio$lvdd >= 4.826] <- 1 #large

###The WM score index's calculation and explanation are provided
#within the report. The main take away is that normal is a score around
#1. Within this study the 3rd quantile is 1.51 for this score. 
#This score is an average of 16 assesmenets within the heart. 
#Based on papers I found that anything from 1 - 1.3 is normal
#1.4 - 1.9 is worrisome but you may not experience symptoms
#2 - 2.1 is when complications start and anything 2.2 and above is very
#bad. Given the scores in this data have a 3rd quantile of 1.51 I will
#put them into three groups 1.1 to 1.3 as normal and 1.4 to 1.9 as 
#worrysome WMSI and I will combine the problematic and very problamatic
#into one category with anything over 1.9 falling into this category. 
#The sample size would be too small to give 2 - 2.1 its own group

cardio$WMindex <- cut(cardio$WMindex, breaks=c(-Inf, 1.3, 1.9, Inf),
                      labels=c(0,1,2))
####################################################################
#Preliminary Analysis

#pericardial effusion is binary yet it has a 77, this point is changed
#to NA
cardio$periEff[49] <- NA

#Quick look at some plots
par(mfrow=c(3,3))
hist(cardio$SurvMonth, main = 'Survival (Months)')
pie(table(cardio$Status0Dead), main = "Dead(0) or Alive(1)")
pie(table(cardio$age), main = 'Age',
    labels = c('under 65', '65 and above'))
pie(table(cardio$periEff), main = 'Pericardial-Effusion')
pie(table(cardio$fracShort), main = 'Fractional Shortening', 
    labels = c('Less than 28%', '28% and above'))
pie(table(cardio$epss), main = 'EPSS', 
    labels = c('Less than 7', '7 and above'))
pie(table(cardio$lvdd), main = 'LVDD', 
    labels = c('Normal', 'Large'))
pie(table(cardio$WMindex), main = 'Wall Motion Index (aka WMSI)', 
    labels = c('Normal WMSI', 'Worrisome WMSI', 'Problematic WMSI'))


par(mfrow=c(1,1))

summary(cardio$SurvMonth)
#Survial times are missing data for 50, 51, and 96 so they will be thrown
#out of the dataset
cardio<-cardio[-50,]
cardio<-cardio[-50,]
cardio<-cardio[-94,]

#Preliminary fit with no covariates
survData1 = Surv(cardio$SurvMonth, cardio$Status0Dead,
                 type = c("right"), origin =0)
result.km1 <- survfit(survData1 ~ 1, conf.type = "log-log" )

result.km1
summary(result.km1)
barplot(result.km1$surv, width = 10, main = 'Survival Probabilities')
plot(result.km1, main = 'Kaplan Meir Curve Estimate', col = 'cadetblue4')

#function created by Dirk F. Moore
weibSurv <- function(t, shape, scale) pweibull(t, shape = shape, 
                                       scale = scale, lower.tail = F)
model.weibull <- survreg(Surv(cardio$SurvMonth, cardio$Status0Dead)~1, 
                         dist = "weibull")

summary(model.weibull)
mu.hat <- model.weibull$coefficients
sigma.hat <- model.weibull$scale
lamda.hat <- exp(-mu.hat)
alpha.hat <- 1/sigma.hat

weibull.curve<- curve(weibSurv(x, shape = alpha.hat, scale = 1/lamda.hat), from = 0, to = 100, 
                      ylim = c(0,1), ylab = 'Survival Probability', xlab = 'Time'
                      , main ='Weibull Fit')
lines(result.km1)

#Preliminary tests of significant differences between factor levels

#There is a function survdiff() contained in the package survival. 
#It uses a stratified log-rank test comparing the test statistic to a
#chi-square distribution with 1 degree of freedom. 

#indicates statistically significant difference between under and over
#65 year old survival (younger expected to live longer)
#p = .00128
#This is not entirely surprising, this data is not collected
#in a manner that controls for what type of death. In other words
#it is not a heart problem specific death. Also there is no crossing
#to make note of
survdiff(Surv(cardio$SurvMonth, cardio$Status0Dead)~cardio$age)
result.age <- survfit(survData1 ~ cardio$age,
                      conf.type = "log-log" )


#Coding note the factors are ordered numerically, so the color choices
#are ascending (ie green = 0, blue = 1)
plot(result.age, xlab = 'Time in Monthes', ylab = 'Survival Probability',
     col = c('green','blue'), main = 'Age')
legend("topright", legend = c("Less than 65 y/o", "65 and older"),
       col = c('green','blue'), lwd = 2)
result.age

#Indicates a statistically significany distance in survival 
#for periEff (only at .1 level not .05) The first few months appear 
#very similar but a clear diversion of the lines after that.
survdiff(Surv(cardio$SurvMonth, cardio$Status0Dead)~cardio$periEff)
result.periEff <- survfit(survData1 ~ cardio$periEff,
                          conf.type = "log-log" )

plot(result.periEff, xlab = 'Time in Monthes', ylab = 'Survival Probability',
     col = c('green','blue'), main = 'Pericardial Effusion')
legend("topright", legend = c("Fluid not Present", "Fluid Present"),
       col = c('green','blue'), lwd = 2)
result.periEff

#Indicates significant difference in survival between patients
#shortening by at least 28% (1) and those failing to do so (0)
#The one line has a very impressive survival rate, no crossing to note
survdiff(Surv(cardio$SurvMonth, cardio$Status0Dead)~cardio$fracShort)
result.fracShort <- survfit(survData1 ~ cardio$fracShort,
                            conf.type = "log-log" )

plot(result.fracShort,xlab = 'Time in Monthes', ylab = 'Survival Probability',
     col = c('green','blue'), main = 'Fractional Shortening')
legend("topright", legend = c("<28%", ">=28%"),
       col = c('green','blue'), lwd = 2)

#Indicates a siginifant survival difference in pateients with an EPSS
#greater than 7mm and those with an EPSS 7mm or less

survdiff(Surv(cardio$SurvMonth, cardio$Status0Dead)~cardio$epss)
result.epss <- survfit(survData1 ~ cardio$epss,
                       conf.type = "log-log" )

plot(result.epss, xlab = 'Time in Monthes', ylab = 'Survival Probability',
     col = c('green','blue'), main = 'EPSS')

legend("topright", legend = c("<7mm", ">=7mm"),
       col = c('green','blue'), lwd = 2)

#Indicates statistically significant different in survival times
#between the two factor levels
survdiff(Surv(cardio$SurvMonth, cardio$Status0Dead)~cardio$lvdd)
result.lvdd <- survfit(survData1 ~ cardio$lvdd,
                       conf.type = "log-log" )

plot(result.lvdd, xlab = 'Time in Monthes', ylab = 'Survival Probability',
     col = c('green','blue'),main = 'LVDD')
legend("topright", legend = c("Normal", "Large"),
       col = c('green','blue'), lwd = 2)

#Very low p-value, graph also shows significant difference
#this is unsurprising given when the WMSI is measuring 
#no crossing
survdiff(Surv(cardio$SurvMonth, cardio$Status0Dead)~cardio$WMindex)
result.WMindex <- survfit(survData1 ~ cardio$WMindex,
                          conf.type = "log-log" )

plot(result.WMindex, xlab = 'Time in Monthes', ylab = 'Survival Probability',
     col = c('green','blue', 'red'), main = 'Wall Motion Score Index (WMSI)')
legend("topright", legend =c('Normal WMSI', 'Worrisome WMSI', 
                             'Problematic WMSI'),
       col = c('green','blue', 'red'), lwd = 2)


####################################################################
## Main Analysis

summary(cardio$SurvMonth)

#Using an AFT Model
#Stepwise selection based on AIC 
model.full<-survreg(Surv(cardio$SurvMonth, 
                         cardio$Status0Dead)~ cardio$age 
                    + cardio$periEff 
                    + cardio$fracShort 
                    + cardio$epss
                    + cardio$lvdd 
                    +cardio$WMindex
                    ,dist = "weibull")
model.step.select<- step(model.full)
#The selected covariates to be in the model are fracShort, lvdd, age,
#epss and WMindex with an AIC 282.26

#The weibull regression function will produce hazard ratios and event 
#time ratios. Since the survreg() function is using the AFT estimates
#the ETRs are the ones to look at. 

#Using the predictors selected by the stepwise function 
#(everything but perieff)
WeibullReg(Surv(cardio$SurvMonth, 
                cardio$Status0Dead)~ cardio$age 
          + cardio$fracShort + cardio$epss
           + cardio$lvdd +cardio$WMindex, data = cardio, conf.level = 0.95)
#The following predictors have ETR's containing 1, suggesting 
#insignificant difference between factor levels: epss, lvdd and fracShort
#also note that the confidence interval for fracshort ETR is (.41, 241.12)
m<- survreg(Surv(cardio$SurvMonth, 
          cardio$Status0Dead)~ cardio$age 
          + cardio$fracShort 
          + cardio$epss
          + cardio$lvdd 
          +cardio$WMindex)
AIC(m) #Provides the same AIC which makes sense since weibull reg
#is using the survreg function
#By commenting out epss, lvdd, and fracShort one at a time AICS:
# w/o epss 291.7918
#w/o fracShort 282.9783
#w/o lvdd 284.6124
#The penalty in AIC is similar for all of these so I am electing to 
#remove fractional shortening. I wouldn't want to remove all three at 
#once just in case they are effecting each other. The fractional shortening
#has a low sample size and the overly large confidence interval makes
#it the best to remove in my opinion. Additionally it has the lowest
#penalty in AIC for removing it.

WeibullReg(Surv(cardio$SurvMonth, 
                cardio$Status0Dead)~ cardio$age 
            + cardio$epss
           + cardio$lvdd +cardio$WMindex, data = cardio, conf.level = 0.95)
#The following predictors have ETR's containing 1, suggesting 
#insignificant difference between factor levels: epss and lvdd 
m2<- survreg(Surv(cardio$SurvMonth, 
                 cardio$Status0Dead)~ cardio$age 
            + cardio$epss
           # + cardio$lvdd 
            +cardio$WMindex)
AIC(m2) #282.9783
#removing one at a time AIC:
#w/o epss 294.7191
#w/o lvdd 286.1626
#The confidence intervals are about the same length, the penalties for
#removing one are about the same for both. LVDD has a more balanced 
#sample size between groups than EPSS does. The effect sample size is
#having on the tests is evident in EPSS's km product estimate curve. 
#When the curve looks very 'blocky' it is a good way to spot low 
#sample size. The lvdd plot did not have as severe of a case of this. 
#LVDD also rejected the null at a lower alpha. Therefore I am 
#going to remove epss

WeibullReg(Surv(cardio$SurvMonth, 
                cardio$Status0Dead)~ cardio$age 
           + cardio$lvdd +cardio$WMindex, data = cardio, conf.level = 0.95)
#The following predictors have ETR's containing 1 suggesting 
#insignificant difference between factor levels: epss and lvdd 
m3<- survreg(Surv(cardio$SurvMonth, 
                  cardio$Status0Dead)~ cardio$age 
              + cardio$lvdd 
             +cardio$WMindex)
AIC(m3) #294.7191
#LVDD just barely contains 1 in its ETR confidence interval at this 
#point. Moving forward I want to consider the possiblity of keeping it
#in and taking it out. The reason being that the AIC jumps to 337.7526
#if LVDD is removed.

#Model Diagnostics
#This type of diagnostic plot evaluates using a weibull model
#If weibull is the right fit the lines should be approximately 
#linear and parrallel. As these appears to violate those assumptions
#by being curvilinear, I am going to try another type of fit for 
#the model
par(mfrow=c(2,2))
WeibullDiag(Surv(cardio$SurvMonth,cardio$Status0Dead)  ~ cardio$age  )
WeibullDiag(Surv(cardio$SurvMonth,cardio$Status0Dead)  ~ cardio$WMindex  )
WeibullDiag(Surv(cardio$SurvMonth,cardio$Status0Dead)  ~ cardio$lvdd  )
par(mfrow=c(1,1))


#Looking at these predictors with Cox Model
#survReg function was used before to fit to a Weibull
model.cox.full<- coxph(Surv(cardio$SurvMonth, 
                       cardio$Status0Dead)~  cardio$age 
                  + cardio$periEff 
                  + cardio$fracShort 
                  + cardio$epss
                  + cardio$lvdd 
                  +cardio$WMindex)
AIC(model.cox.full) #261.6518
#Using step 
#w/o periEFf 259.9825 is the only variable that lowers AIC by 
#its removal. By AIC the model will be
model.cox<- coxph(Surv(cardio$SurvMonth, 
                            cardio$Status0Dead)~  cardio$age 
                       + cardio$fracShort 
                       + cardio$epss
                        + cardio$lvdd 
                       +cardio$WMindex)
AIC(model.cox) #259.9925 Note: even though the previous model proved
#to be not ideal, this is the combination of predictors 
#that stepwise on the AFT weibull selected
BIC(model.cox) #268.777
model.cox

#Here I made age a strata variable

model.cox2<- coxph(Surv(cardio$SurvMonth, 
                       cardio$Status0Dead)~  strata(cardio$age) 
                  + cardio$fracShort 
                  + cardio$epss
                  + cardio$lvdd 
                  +cardio$WMindex, na.action="na.exclude")
AIC(model.cox2) #216.5862 which is a substantial drop
BIC(model.cox2) # 223.9148 the BIC also dropped. 
model.cox2
#The likelihood ratio test (goodness of fit)'s p-value went up 
#however, it is still capable of rejecting the null at the 
#alpha .001 level so I am inclined to keep this given the substantial
#decrease in AIC and BIC

#However, multiple predictors are insignificant. 

#By commenting out different variables I was able to 
#figure out that if you remove lvdd or epss the AIC and BIC do not change much
#however the p-value for the goodness of fit drops down quite a bit
#if you get rid of both of them the penalty is steeper in AIC and BIC
#and the p-value is about the same. Removing LVDD has 
#the better AIC/ BIC option. If you then remove fractional shortening
#there is next to no penalty in AIC or BIC, the p-value remains low
#and now all of the predictors are significant at at least the .1 level

model.cox4<- coxph(Surv(cardio$SurvMonth, 
                        cardio$Status0Dead)~  strata(cardio$age) 
                  + cardio$epss
                   +cardio$WMindex, na.action="na.exclude")
AIC(model.cox4) #222.9803
BIC(model.cox4) # 227.4698
model.cox4
#This is the model I will go with

#Here is the forest plot of the coefficients 
#code by Dirk F. Moore
coef.est<-c(NA, NA, 0, 1.084, NA, NA, NA, 0, 1.234, 1.701)
se.est<-c(NA, NA, 0, .610, NA, NA, NA, 0, .432, .467)
lower<- coef.est - 1.96*se.est
upper<- coef.est + 1.96*se.est
label.factors <- matrix(c("EPSS", "", "less than 7", "7+", "",
                          "WMSI", "", "good", "worrisome", "bad"), ncol=1)
forestplot(label.factors, coef.est, lower=lower, upper = upper,
           boxsize = 0.4, xticks = c(-0.5,0,0.5,1,1.5,2),
           txt_gp = fpTxtGp(label = gpar(cex=1.5)))

#Model Assesment

#In order to move forward with the proportional hazard model
#The assumptions must be verified. 
par(mfrow =c(1,2))
survData1 = Surv(cardio$SurvMonth, cardio$Status0Dead,
                 type = c("right"), origin =0)
plot(survfit(survData1 ~ cardio$WMindex), 
     col=c("blue", "green", "magenta"), fun="cloglog"
     , main = "Complementary Log-Log Plot WMSI")
plot(survfit(survData1 ~ cardio$epss), 
     col=c("blue", "green"), fun="cloglog"
     , main = "Complementary Log-Log Plot EPSS")
#Since the lines are approximately parrallel we can conclude the 
#assumptions have not been violated

#Residual Checking 
res <- residuals(model.cox4, type = "martingale")
par(mfrow =c(1,2))
plot(res~ as.factor(cardio$epss), main = 'Martingale Residuals vs EPSS')
plot(res~cardio$WMindex, main = 'Martingale Residuals vs WMSI')
par(mfrow =c(1,1))


#Unfourtanelty The residuals for these two groups are note evenly spread 
#One thing to note is that the sample sizes between different factor levels
#could be much better. Overall, this model is still better than the 
#weibull model


