# Analysis of MBA SALARIES
# NAME: Aditya Sharma
# EMAIL: adityathc@gmail.com
# COLLEGE / COMPANY: NIT Jaipur


# Creating working directory
setwd("C:/Users/aditya/Downloads/Internship 2017/R directory")

# Reading csv file into R and creating a data frame "start"
start <- read.csv("MBA Starting Salaries Data.csv")

# Adding column mbavg which is average of spring and fall grades
start$mbavg <- (start$s_avg + start$f_avg)/2

# check for numbers of 0 (no placement) and 999,998 
table(start$salary==0)
table(start$salary==999)

# Creating a data frame "startplaced" which contains data of placed students only, removing unplaced and missing information
startplaced <- subset(start, salary!=0)
startplaced <- subset(startplaced, salary!=999)
startplaced <- subset(startplaced, salary!=998)

# Create data frame for unplaced students
startunplaced <- subset(start, salary==0)

# Summary statistics of "startplaced"
summary(startplaced)

# Boxplot visualization individually
boxplot(startplaced$age)
boxplot(startplaced$sex)
boxplot(startplaced$gmat_tot)
boxplot(startplaced$gmat_qpc)
boxplot(startplaced$gmat_vpc)
boxplot(startplaced$gmat_tpc)
boxplot(startplaced$s_avg)
boxplot(startplaced$f_avg)
boxplot(startplaced$quarter)
boxplot(startplaced$work_yrs)
boxplot(startplaced$frstlang)
boxplot(startplaced$salary)
boxplot(startplaced$mbavg)

# Pair-wise scatterplot visualization
plot(startplaced$age, startplaced$work_yrs)
plot(startplaced$gmat_tot, startplaced$gmat_tpc)
plot(startplaced$gmat_tot, startplaced$gmat_qpc)
plot(startplaced$gmat_qpc, startplaced$gmat_vpc)
plot(startplaced$gmat_tpc, startplaced$gmat_qpc)
plot(startplaced$gmat_tot, startplaced$s_avg)
plot(startplaced$gmat_tot, startplaced$work_yrs)
plot(startplaced$gmat_tot, startplaced$mbavg)
plot(startplaced$gmat_tot, startplaced$frstlang)
plot(startplaced$gmat_vpc, startplaced$frstlang)
plot(startplaced$salary, startplaced$work_yrs)
plot(startplaced$salary, startplaced$gmat_qpc)
plot(startplaced$salary, startplaced$gmat_vpc)
plot(startplaced$salary, startplaced$gmat_tpc)
plot(startplaced$salary, startplaced$s_avg)
plot(startplaced$salary, startplaced$mbavg)

# Plotting a Corrgram of each variable in "startplaced"
library(corrgram)
corrgram(startplaced, order = TRUE, lower.panel = panel.shade, upper.panel = panel.pie,text.panel = panel.txt,main="Corrgram of placed student intercorrelation")

# Correlation matrix
cor(startplaced)

# Creating few contigency tables and running chi-sq test, followed by t-test 
mytable1 <- xtabs(~ age + work_yrs, data = startplaced)
chisq.test(mytable1)
t.test(startplaced$age, startplaced$work_yrs)
cor.test(startplaced$age, startplaced$work_yrs)

mytable2 <- xtabs(~ gmat_tot + gmat_tpc, data = startplaced)
chisq.test(mytable2)
t.test(startplaced$gmat_tot, startplaced$gmat_tpc)

t.test(startplaced$salary, startplaced$work_yrs)
t.test(startplaced$salary, startplaced$gmat_tot)
t.test(startplaced$salary, startplaced$mbavg)
t.test(startplaced$salary, startplaced$satis)
t.test(startplaced$salary, startplaced$sex)
t.test(startplaced$salary, startplaced$frstlang)

# Fitting mutilple regression
# Step by Step regression (including many variables) , untill better adjusted r squared value is achived ( better fit)
# by removing least p-value variable step by step
model1 <- lm(salary ~ age + work_yrs + mbavg + gmat_tot + sex + satis + gmat_vpc + gmat_qpc + gmat_tpc + s_avg + f_avg, data = startplaced)
summary(model1)

model1A <- lm(salary ~ age + work_yrs + mbavg + sex + satis + gmat_vpc + gmat_qpc + gmat_tpc + s_avg, data = startplaced)
summary(model1A)
model1B <- lm(salary ~ age + work_yrs + sex + satis + gmat_vpc + gmat_qpc + gmat_tpc + s_avg, data = startplaced)
summary(model1B)
model1C <- lm(salary ~ age + sex + satis + gmat_vpc + gmat_qpc + gmat_tpc + s_avg, data = startplaced)
summary(model1C)
model1D <- lm(salary ~ age + sex + satis + gmat_vpc + gmat_qpc + gmat_tpc, data = startplaced)
summary(model1D)
model1E <- lm(salary ~ age + sex + gmat_vpc + gmat_qpc + gmat_tpc, data = startplaced)
summary(model1E)

model1F <- lm(salary ~ age + gmat_vpc + gmat_qpc + gmat_tpc, data = startplaced)
summary(model1F)

model1G <- lm(salary ~ age + gmat_qpc + gmat_tpc, data = startplaced)
summary(model1G)

# Alternate regression models- variables chosen according previous matrix and test

model2 <- lm(salary ~ age + work_yrs + gmat_tot + sex + satis + gmat_tpc + mbavg, data = startplaced)
summary(model2)
model2A <- lm(salary ~ age + work_yrs + gmat_tot + sex + satis + gmat_tpc, data = startplaced)
summary(model2A)
model2B <- lm(salary ~ age + gmat_tot + sex + satis + gmat_tpc, data = startplaced)
summary(model2B)


model3 <- lm(salary ~ age + work_yrs + gmat_tot + sex + gmat_tpc + mbavg, data = startplaced)
summary(model3)
model4 <- lm(salary ~ age + work_yrs + sex + gmat_tpc + mbavg, data = startplaced)
summary(model4)
model5 <- lm(salary ~ age + work_yrs + gmat_tot + mbavg, data = startplaced)
summary(model5)
model6 <- lm(salary ~ age  + gmat_tpc + mbavg, data = startplaced)
summary(model6)
model7 <- lm(salary ~ age  + gmat_tot + mbavg, data = startplaced)
summary(model7)

# Plotting best and fit ( model1f) and few others too for visualization of residual vs fitted line
plot(model1F)
plot(model1)
plot(model7)


## Part 2

# For visualization and analyzing jobs ( i.e. placed or not) 
# Adding new column (job) to "startplaced" and "startunplaced" ( "start" not chosen because of missing data)
# where in new columns 1 is  'placed' and 0 is 'unplaced'
startplaced$job <- 1
startunplaced$job <- 0

# Adding both data frames into one data frame called "jobdata" (row-wise) 
jobdata <- rbind(startplaced, startunplaced)

# Viewing structure of data set
str(jobdata)

# Making column 'job' a factor from numeric , making it catagorical (for logistic regression)
jobdata$job <- as.factor(jobdata$job)

# Creating few contigency tables and running chi-sq test and afterwards running t.tests ( checking variables with 'job')
mytable3 <- xtabs(~ job + gmat_tot,data = jobdata)
chisq.test(mytable3)
t.test(age ~ job, data = jobdata)
t.test(mbavg ~ job, data = jobdata)
t.test(gmat_tpc ~ job, data = jobdata)
t.test(gmat_tot ~ job, data = jobdata)
t.test(sex ~ job, data = jobdata)
t.test(quarter ~ job, data = jobdata)
t.test(s_avg ~ job, data = jobdata)
t.test(work_yrs ~ job, data = jobdata)
t.test(satis ~ job, data = jobdata)
t.test(frstlang ~ job, data = jobdata)

# Step by Step logistic model regression ( by removing least p-value variable step by step) untill best fit ( big residual deviance )
modelL1 <- glm(job ~ age + work_yrs + mbavg + gmat_tot + sex + satis + gmat_vpc + gmat_qpc + gmat_tpc + s_avg + f_avg, data = jobdata, family = binomial(link = logit))
summary(modelL1)
modelL2 <- glm(job ~ age + work_yrs + gmat_tot + sex + satis + gmat_vpc + gmat_qpc + gmat_tpc + s_avg, data = jobdata, family = binomial(link = logit))
summary(modelL2)
modelL3 <- glm(job ~ age + work_yrs + gmat_tot + satis + gmat_qpc + gmat_tpc + s_avg, data = jobdata, family = binomial(link = logit))
summary(modelL3)
modelL4 <- glm(job ~ age + work_yrs + gmat_tot + satis + gmat_tpc + s_avg, data = jobdata, family = binomial(link = logit))
summary(modelL4)
modelL5 <- glm(job ~ age + gmat_tot + satis + gmat_tpc + s_avg, data = jobdata, family = binomial(link = logit))
summary(modelL5)
modelL6 <- glm(job ~ age + gmat_tot + s_avg, data = jobdata, family = binomial(link = logit))
summary(modelL6)

modelL7 <- glm(job ~ age + s_avg, data = jobdata, family = binomial(link = logit))
summary(modelL7)

# Plotting the best fit model for residual vs fitted line visualization 
plot(modelL7)


