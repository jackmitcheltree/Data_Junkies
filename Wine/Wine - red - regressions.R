#Wine regressions
install.packages("dplyr","tidyr", "ggplot2", "readr")
library(readr)
winequality_white <- read_csv("winequality-white.csv")

QualXAlch <- lm(quality ~ alcohol, data=winequality_white)
summary(QualXAlch)

QualXmany <- lm(quality ~ alcohol+`citric acid`+`residual sugar`+pH, 
                data = winequality_white)
summary(QualXmany)
head(winequality_white)

cor(winequality_white)

QualXAll <- lm(quality ~ `fixed acidity`+`volatile acidity`+`citric acid`+`residual sugar`
               +chlorides+`free sulfur dioxide`+`total sulfur dioxide`+density+pH
               +sulphates+ alcohol, data = winequality_white)
summary(QualXAll)


