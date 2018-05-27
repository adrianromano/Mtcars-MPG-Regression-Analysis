## Load and read the data
library(datasets)
data(mtcars)
str(mtcars)
head(mtcars)

mtcars$cyl  <- factor(mtcars$cyl)
mtcars$vs   <- factor(mtcars$vs)
mtcars$am   <- factor(mtcars$am,labels = c("Automatic","Manual"))
mtcars$gear <- factor(mtcars$gear)
mtcars$carb <- factor(mtcars$carb)

summary(mtcars$mpg)

library(dplyr)
mtcars %>%
    group_by(am) %>%
    summarize(mean = mean(mpg))

boxplot(mpg ~ am, data = mtcars, xlab = "Transmission Type", ylab = "Miles per Gallor")
pairs(mpg ~ ., data = mtcars)

automatic <- mtcars[mtcars$am == "Automatic", ]
manual <- mtcars[mtcars$am == "Manual", ]
t.test(automatic$mpg, manual$mpg)

modelFit <- lm(mpg ~ am, data = mtcars)
summary(modelFit)

modelFit1 <- lm(mpg ~ am , data = mtcars) 
modelFit2 <- lm(mpg ~ am + wt, data = mtcars) 
modelFit3 <- lm(mpg ~ am + wt + hp , data = mtcars) 
modelFit4 <- lm(mpg ~ am + wt + hp + cyl, data = mtcars) 
modelFit5 <- lm(mpg ~ am + wt + hp + cyl + disp, data = mtcars) 
modelFit6 <- lm(mpg ~ ., data = mtcars) 

anova(modelFit1, modelFit2, modelFit3, modelFit4, modelFit5, modelFit6)

summary(modelFit3)
par(mfrow = c(2,2))
plot(modelFit3)