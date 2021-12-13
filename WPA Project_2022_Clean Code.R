# Sana Project 2022

setwd("C:/Users/cogps/Desktop/Greg Lab/Sana Project/HSLS_2017_PETS_SR_v1_0_CSV_Datasets")

Student<- read.csv("hsls_17_student_pets_sr_v1_0.csv")


Student1 <- subset(Student, select =c("X1RACE", "X1SEX", "X2POVERTY", "C2AVGACTREAD"))

## Subset for Black girls and white girls 
## 1 male and 2 female 

Student1$RaceSex <- with(Student1, ifelse(X1RACE == 3 & X1SEX == 2, "BlackFemale",
                                             ifelse(X1RACE==8 & X1SEX ==2, "whiteFemale", NA)) )

table(as.factor(Student1$RaceSex))
          # BlackFemale: 1168, white Female: 5941

## Dropping the ones that do not belong to either group 

Student2<-subset(Student1, !is.na(RaceSex))

table(as.factor(Student2$RaceSex))

          ## BFP: 148 v wFP:265

## linear regression

# C2AVGACTREAD ~ RaceSexIncome + X2POVERTY

## getting rid of categories not needed 
Student2[ Student2 == -9 ] <- NA
Student2[ Student2 == -8 ] <- NA
Student2[ Student2 == -6 ] <- NA

## Dummy Coding Categorical Variables 
levels(as.factor(Student2$RaceSex))

Student2$RaceSex <- ifelse(Student2$RaceSex == 'BlackFemale', 0, 1)
        # Black girls will be the refernce group and white girls the focal

Student2$X2POVERTY <- ifelse(Student2$X2POVERTY == 0, 1, 0)
        # reference category will be Below Poverty threshold, focal category 
        # will be at or above poverty threshold 


lmr<-lm(data = Student2, C2AVGACTREAD ~ RaceSex + X2POVERTY, na.action = na.exclude)

# lmrI<-lm(data = Student2, C2AVGACTREAD ~ RaceSex + X2POVERTY+ RaceSex*X2POVERTY)

summary(lmr)

# summary(lmrI)
        # significant at the .001 level 

plot(lmr)


## Checking whether the Interaction accounted for a signifincat amount of variance

Rsq_Delta <- summary(lmr)$r.squared - summary(lmrI)$r.squared

Rsq_Delta # .000216994

anova(lmr, lmrI, test = "F")

        # not significant 


## Running a robust linear regression
## https://stats.idre.ucla.edu/r/dae/robust-regression/


# install.packages("foreign")
library(foreign)
library(MASS)


summary(Rlm <- rlm(data = Student2, C2AVGACTREAD ~ RaceSex + X2POVERTY))

# save predicted Y value in multiple regression in R
# https://stackoverflow.com/questions/20907583/how-to-return-predicted-values-residuals-r-square-from-lm
# # https://stackoverflow.com/questions/19065279/in-r-how-to-add-the-fitted-value-column-to-the-original-dataframe

Student2$Pred  <- predict(lmr)

View(Student2$Pred)

# do a levene's test to check whether the variance of the groups is identical
# i forsure have unbalanced groups. 
# https://www.geeksforgeeks.org/levenes-test-in-r-programming/

# Using leveneTest()
library(car)
Student2$RaceSexF<-as.factor(Student2$RaceSex)
result = leveneTest(Pred ~ RaceSexF, Student2)

# print the result
print(result)
        # significant indicating its not homogenous

# robust ANOVA
# https://dornsife.usc.edu/assets/sites/239/docs/WRS2.pdf

library(WRS2)

# boot strapping
yuenbt(Pred ~ RaceSex, data = Student2)
# Test statistic: -27.1082 (df = NA), p-value = 0.01002
# 
# Trimmed mean difference:  -1.64516 
# 95 percent confidence interval:
#         -1.7675     -1.5228 

akp.effect(Pred ~ RaceSex, data = Student2, EQVAR = FALSE)

# AKPeffect
# [1] -1.601425
# 
# $AKPci
# [1] -1.641239 -1.594783
# 
# $alpha
# [1] 0.05


# non-robust ANOVA for groups based on predicted values 

one.way <- aov(Pred ~ RaceSex, data = Student2)

summary(one.way)

# Df Sum Sq Mean Sq F value Pr(>F)    
# RaceSex        1  829.3   829.3    3165 <2e-16 ***
#         Residuals   2414  632.6     0.3      

tapply(Student2$Pred, Student2$RaceSex, mean, na.rm=TRUE)
# 0        1 
# 20.42988 22.14033 
tapply(Student2$Pred, Student2$RaceSex, sd, na.rm=TRUE)
# 0         1 
# 0.6594696 0.4847524 
