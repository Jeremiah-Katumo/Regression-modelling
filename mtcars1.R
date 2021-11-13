# read in the mtcars data set
data(mtcars)
mtcars
am.glm <- glm(formula = am~hp+wt, data = mtcars, family = binomial)
am.glm

# Hypotheses test for individual covariates
summary(am.glm)
1-pchisq(43.230-10.059,31-29)

# do anova
am.glm0 <- glm(formula = am~1, data = mtcars, family = binomial)
am.glm0
anova(am.glm0, am.glm, test = 'Chisq')

# Prediction
newdata <- data.frame(hp=120, wt=2.8)
newdata
predict(am.glm, newdata, type = 'response')

# Probit Link
myprobit <- glm(am~hp+wt, data = mtcars, family = binomial(link = "probit"))
myprobit

# Logit Link (Logistic Regression model)
# fit log-linear model
glm.Logit <- glm(formula = am~hp+wt, data = mtcars, family = poisson())
glm.Logit

# Prediction of am of hp=120 and wt=2.8
newdata1 <- data.frame(hp=120, wt=2.8)
newdata1
predict(glm.Logit, newdata1, type = "response")

# Using likelihood ratio to check hypotheses, test statistics and p-value
summary(glm.Logit)
# Estimation: test model fitting
glm.LRatio <- glm(am~1, data = mtcars, family = poisson())
glm.LRatio
anova(glm.LRatio, glm.Logit, test = 'Chisq')

# plotting the probability for males and females
head(subset(mtcars, select = 'am'))
factor(mtcars$am)
w = table(mtcars$am)
w
t = as.data.frame(w)
t
# Red for male and yellow for female
h<- hist(t$Freq, breaks=5, col=c("red","yellow"), xlab="am",
         main="Histogram with Orvelay Line")
xfit<-seq(min(t$Freq),max(t$Freq),length=40)
yfit<-dnorm(xfit, mean = mean(t$Freq), sd=sd(t$Freq))
yfit<-yfit*diff(h$mids[1:2])*length(t$Freq)
lines(xfit, yfit, col="blue", lwd=2)
