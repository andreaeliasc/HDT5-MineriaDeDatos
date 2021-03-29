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

# Revisamos el tipo de datos del conjunto de entrenamiento
head(train)

# Revisamos el tipo de datos del conjunto de test
head(test)

glimpse(train[1:10,])
scatter.smooth(train$LotFrontage, train$SalePrice)
scatter.smooth(train$LotArea, train$SalePrice)
scatter.smooth(train$GrLivArea, train$SalePrice)
scatter.smooth(train$YearBuilt, train$SalePrice)
scatter.smooth(train$BsmtUnfSF, train$SalePrice)
scatter.smooth(train$TotalBsmtSF, train$SalePrice)
scatter.smooth(train$X1stFlrSF, train$SalePrice)
scatter.smooth(train$GarageYrBlt, train$SalePrice)
scatter.smooth(train$GarageArea, train$SalePrice)
scatter.smooth(train$YearRemodAdd, train$SalePrice)
scatter.smooth(train$TotRmsAbvGrd, train$SalePrice)
scatter.smooth(train$MoSold, train$SalePrice)
scatter.smooth(train$OverallQual, train$SalePrice)

## Clasificacion por Clustering 
datos <- train[,c("LotFrontage","LotArea","GrLivArea","YearBuilt","BsmtUnfSF","TotalBsmtSF","X1stFlrSF","GarageYrBlt","GarageArea","YearRemodAdd", "SalePrice")]

datos <- na.omit(datos)

# Grafico de codo 
wss <- (nrow(datos)-1)*sum(apply(datos,2,var))

for (i in 2:10) 
  wss[i] <- sum(kmeans(datos, centers=i)$withinss)

plot(1:10, wss, type="b", xlab="Number of Clusters",  ylab="Within groups sum of squares")

# Aplicacion del algoritmo K-Means
km<-kmeans(datos,3)
datos$grupo<-km$cluster

g1<- datos[datos$grupo==1,]
prop.table(table(g1$SalePrice))*100
g2<- datos[datos$grupo==2,]
prop.table(table(g2$SalePrice))*100
g3<- datos[datos$grupo==3,]
prop.table(table(g3$SalePrice))*100

# Graficos de cluster
plotcluster(datos,km$cluster)
fviz_cluster(km, data = datos,geom = "point", ellipse.type = "norm")

max(g1["SalePrice"])
min(g1["SalePrice"])
max(g2["SalePrice"])
min(g2["SalePrice"])
max(g3["SalePrice"])
min(g3["SalePrice"])

datos$grupo <- as.factor(datos$grupo)

##  modelo Naive Bayes 
modelo<-naiveBayes(grupo~.,data=datos)
modelo

## Eficiencia del modelo 
testing <- test[,c("LotFrontage","LotArea","GrLivArea","YearBuilt","BsmtUnfSF","TotalBsmtSF","X1stFlrSF","GarageYrBlt","GarageArea","YearRemodAdd", "SalePrice")]
testing <- na.omit(testing)
View(testing)

# Agregar la col
testing$grupo <- ifelse(testing$SalePrice<178000, "3", 
                        ifelse(testing$SalePrice<301000, "2",
                               ifelse(testing$SalePrice<756000,"1",NA)))

testing$grupo <- as.factor(testing$grupo)
testing

predBayes<-predict(modelo, newdata = testing[,1:11])
predBayes

# Se une la columna
testing['prediccion'] <- predBayes

# Cantidad de predicciones falladas
testing[which(testing[,12]!=testing[,13]),]
count(testing[which(testing[,12]!=testing[,13]),])

# Se elimna la columna extra de prediccion 
testing[,13]<- NULL

## Determinación de Eficiencia del modelo con Confusion Matrix
confusionMatrix(predBayes,testing$grupo)

matriz_cor <- cor(datos[,1:11])
matriz_cor
corrplot(matriz_cor)

## validacion cruzada
ct<-trainControl(method = "cv",datos[,1:11],number=10, verboseIter=T)
modeloCaret<-train(grupo~.,data=datos,method="nb",trControl = ct)
modeloCaret
prediccionCaret<-predict(modeloCaret,newdata = testing[,1:11])
confusionMatrix(prediccionCaret,testing$grupo)

