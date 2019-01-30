
##########stargazer

install.packages(stargazer)
library(stargazer)

View(mtcars)
mydata <- data.frame(mtcars)     #data(mtcars)有误，格式是data.frame

###Statistic Description
stargazer(mydata, type = "text", title="Descriptive statistics", 
          digits=1, out="table1.txt")
stargazer(mydata, type = "text", title="Descriptive statistics", 
          digits=1, out="table2.txt", flip=TRUE)   #flip横排展示
stargazer(mydata, type = "text", title="Descriptive statistics", digits=1, out="table3.txt",
          covariate.labels=c("Miles/(US)gallon","No. of cylinders","Displacement (cu.in.)",
                             "Gross horsepower","Rear axle ratio","Weight (lb/1000)",
                             "1/4 mile time","V/S","Transmission (0=auto, 1=manual)",
                             "Number of forward gears","Number of carburetors"))

stargazer(mydata[c("mpg","hp","drat")], type = "text",
          title="Descriptive statistics/selected variables", digits=1, out="table4.txt", flip=TRUE,
          covariate.labels=c("Miles/(US)gallon","Gross horsepower","Rear axle ratio"))

stargazer(subset(mydata[c("mpg","hp","drat")], mydata$am==0),
          title="Automatic transmission", type = "text", digits=1, out="table-Automatic transmission.txt")
stargazer(subset(mydata[c("mpg","hp","drat")], mydata$am==1),
          title="Manual transmission", type = "text", digits=1, out="table-Manual transmission.txt")


###Regression

mydata$fast <- as.numeric((mydata$mpg > 20.1)) #Creating a dummy variable 1 = fast car
m1 <- lm(mpg ~ hp, data=mydata)
m2 <- lm(mpg ~ hp + drat, data=mydata)
m3 <- lm(mpg ~ hp + drat + factor(gear), data=mydata)
m4 <- glm(fast ~ hp + drat + am, family=binomial(link="logit"), data=mydata)
stargazer(m1, m2, m3, m4, type="text",
          dep.var.labels=c("Miles/(US) gallon","Fast car (=1)"),
          covariate.labels=c("Gross horsepower","Rear axle ratio","Four foward gears",
                             "Five forward gears","Type of transmission (manual=1)"), out="models.txt")
stargazer(m1, m2, m3, m4, type="html",
          dep.var.labels=c("Miles/(US) gallon","Fast car (=1)"),
          covariate.labels=c("Gross horsepower","Rear axle ratio","Four foward gears",
                             "Five forward gears","Type of transmission (manual=1)"), out="models.htm")


################outreg
install.packages("outreg")
library(outreg)
fitlist <- list(lm(mpg ~ cyl, data = mtcars),
                lm(mpg ~ cyl + wt + hp, data = mtcars),
                lm(mpg ~ cyl + wt + hp + drat, data = mtcars))
outreg(fitlist)

# with custom regression names
outreg(setNames(fitlist, c('small', 'medium', 'large')))
# star on standard errors, instead of estimate
outreg(fitlist, starred = 'se')
# include other stats
outreg(fitlist, pv = TRUE, tv = TRUE, se = FALSE)
?gl

counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)
fitlist2 <- list(glm(counts ~ outcome, family = poisson()),
                 glm(counts ~ outcome + treatment, family = poisson()))
outreg(fitlist2)


##########################texreg
install.packages("texreg")
library(texreg)

ctl <- c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14)
trt <- c(4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69)
group <- gl(2, 10, 20, labels = c("Ctl", "Trt"))
weight <- c(ctl, trt)
m1 <- lm(weight ~ group)
m2 <- lm(weight ~ group - 1)
summary(m2)
screenreg(list(m1, m2))            ##screenreg
