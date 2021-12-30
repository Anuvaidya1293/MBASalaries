# Analysis of MBA SALARIES
# NAME: Anuradha Vaidya
# EMAIL: anuvaidya11@gmail.com
# COLLEGE: IIT, Madras

mba.df <- read.csv(paste("MBA Starting Salaries Data.csv", sep = ""))

View = mba.df
summary(mba.df)


library(psych)

plot( mba.df$salary, mba.df$sex)

plot( mba.df$salary, mba.df$frstlang)

plot( mba.df$salary, mba.df$gmat_tot)

plot( mba.df$salary, mba.df$work_yrs)

plot( mba.df$salary, mba.df$age)

library(car)

boxplot(mba.df$salary ~ mba.df$work_yrs, data=mba.df, horizontal=TRUE, yaxt="n",
        ylab="work_yrs", xlab="salary", 
        main="Factors affecting MBA Salaries") 



library(car)

scatterplot(salary ~ work_yrs, data=mba.df, 
            spread=FALSE, smoother.args=list(lty=2), 
            pch=19, main="Scatterplot of Salary vs. Work experience", 
            xlab="Work_yrs", ylab="Salary")


library(car)

scatterplot(salary ~ age, data=mba.df, 
            spread=FALSE, smoother.args=list(lty=2), 
            pch=19, main="Scatterplot of Salary vs. age", 
            xlab="age", ylab="Salary")

cor.test(mba.df[,"salary"], mba.df[,"work_yrs"])

library(car) 
scatterplotMatrix(mba.df[,c("salary","work_yrs","age")], 
                  spread=FALSE, smoother.args=list(lty=2),
                  main="Scatter Plot Matrix")


model <- lm(mba.df$salary ~ mba.df$work_yrs , data=mba.df) #the '.' means 'all' 
summary(model)

model <- lm(mba.df$salary ~ . , data=mba.df) #the '.' means 'all' 
summary(model)


model <- lm(mba.df$salary ~  mba.df$work_yrs  + mba.df$gender + 
              mba.df$gmat_tot, data=mba.df) #the '.' means 'all' 
summary(model)









