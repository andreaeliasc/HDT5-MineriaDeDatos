library(dplyr)
library(tidyr)
library(rpart)
library(caret)
library(tree)
library(rpart.plot)
library(randomForest)
library(dplyr)
library(tidyr)
library(cluster)
library(e1071)
library(mclust)
library(fpc)
library(NbClust)
library(factoextra)
library(cluster)
library(e1071)
library(mclust)
library(fpc)
library(NbClust)
library(factoextra)
library(rpart)
library(corrplot)

# Analisis Exploratorio
train<- read.csv("train.csv", stringsAsFactors = FALSE)
test<- read.csv("test.csv", stringsAsFactors = FALSE)
train<-train[1:1460,]

glimpse(train[1:10,])

#separacion de datos para training y testing, donde se deja el 70% 
#para training y el 30% para testing
porcentaje<-0.7
datos<-read.csv("train.csv", stringsAsFactors = FALSE)
set.seed(123)

corte <- sample(nrow(datos),nrow(datos)*porcentaje)
train<-datos[corte,]
test<-datos[-corte,]
