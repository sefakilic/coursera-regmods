
## ----warning=FALSE-------------------------------------------------------
# factor some variables
mtcars$cyl <- factor(mtcars$cyl)
mtcars$vs <- factor(mtcars$vs)
mtcars$am <- factor(mtcars$am)
mtcars$gear <- factor(mtcars$gear)
mtcars$carb <- factor(mtcars$carb)
str(mtcars)


## ------------------------------------------------------------------------
library(plyr)
library(ggplot2)
# Rename the levels of transmission types
transmission <- revalue(mtcars$am, c('0'="automatic", '1'="manual"))
ggplot(mtcars, aes(x=transmission, y=mpg, fill=transmission)) +
    geom_boxplot() +
    xlab("Transmission type") +
    ylab("Miles per gallon")


## ------------------------------------------------------------------------
fit1 <- lm(mpg ~ am, data=mtcars)
summary(fit1)


## ----, warning=FALSE, fig.width=8, fig.height=6--------------------------
pairs(mtcars, panel=function(x,y) {
    points(x, y)
    abline(lm(y ~ x), col="red")
})


## ------------------------------------------------------------------------
initial_model <- lm(mpg ~ ., data=mtcars)
best_model <- step(initial_model, direction="both", trace=0)
summary(best_model)
par(mfrow = c(2,2))
plot(best_model)


