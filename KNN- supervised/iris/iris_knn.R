# We are going to work with the well-known supervised machine learning algorithm called k-NN or 
# k-Nearest Neighbors. For this exercise, we will use the Iris data set for classification. 
# The attribute `Species` of the data set will be the variable that we want to predict.

# Loading data

# First we need to load some libraries.

#install.packages('knitr')
#install.packages('class')
#install.packages('tidyverse')
#install.packages('GGally')
library(knitr)      # se usa para documentos escritos en R markdown, combina textos y análsis hacia otros formatos
library(class)      # Incluye funciones de clasificación incluído el de k vecinos más cercanos
library(tidyverse)  # colección de paquetes orientados a la manipulación, importación, exploración y visualización de datos
library(GGally)     # Este paquete está diseñado para poder graficar en forma de matriz un set de datos con múltiples variables, 
                      # el resultado es una correlación de las variables elegidas en dicho dataset.

mypath <- dirname(rstudioapi::getActiveDocumentContext()$path) 
setwd(mypath)

iris <- read.csv(file = 'Iris.csv')
iris$Id <- NULL  #### Elimina los ID de la base de datos, estos no aportan nada al análisis

# Let's get an idea of what we're working with.

head(iris)

# Last rows 
tail(iris)

# Summary
summary(iris)  # genera un recuento de todas las variables del dataset, medidas de tendencia central, clase de las variables cualitativas

# Structure 
str(iris) # indica la clase de todas las variables en el dataset, el número de observaciones y de variables existentes

# Some minor changes in the data set
iris2 <- iris %>%
  rename(`Sepal length`=SepalLengthCm,
         `Sepal width`=SepalWidthCm,
         `Petal length`=PetalLengthCm,
         `Petal width`=PetalWidthCm) %>%
  mutate(Species=fct_recode(Species, "setosa"="Iris-setosa",
                            "versicolor" = "Iris-versicolor",
                            "virginica"="Iris-virginica"))

# ------------------------------------ Analisis exploratorio de datos -----------------------------------------------------------------------------------


# gather() en el equivalente a  pivot_longer(), 
# que es más fácil de usar, tiene más funciones y aún está en desarrollo activo.

#     df %>% gather("key", "value", x, y, z)  es equivalente a 
#     df %>% pivot_longer(c(x, y, z), 
#                         names_to = "key", 
#                         values_to = "value")

# Histogram for each species --------------------------------
# número de frecuencias de un punto de datos específico.
iris2 %>%
  gather(Attributes, Value, 1:4) %>%      #gather() en el equivalente a  pivot_longer()
  ggplot(aes(x=Value, fill=Attributes)) +
  geom_histogram(colour="black") +        # geom_histogram() divide el eje x en varios contendores para poder gráficar los 3 histogramas
  facet_wrap(~Species) +                  # nos separa por especies
  theme_bw() +
  labs(x="Values", y="Frequency",
       title="Iris data set",
       subtitle="Histogram for each species") +
  theme(legend.title=element_blank(),
        legend.position="bottom")

# Density plot for each species--------------------------------
#  visualiza la distribución de datos en un intervalo
iris2 %>%
  gather(Attributes, value, 1:4) %>%
  ggplot(aes(x=value, fill=Species)) +
  geom_density(colour="black", alpha=0.5) +
  facet_wrap(~Attributes, scales="free_x") +
  labs(x="Values", y="Density",
       title="Iris data set",
       subtitle="Density plot for each attribute") +
  theme_bw() +
  theme(legend.position="bottom",
        legend.title=element_blank())

# Violin plot for each attribute--------------------------------
#  se utiliza para visualizar la distribución de los datos y su densidad de probabilidad
# Las partes anchas del diagrama indican que hay más obervaciones en esa posición y las más angostas indican que hay menos observaciones
iris2 %>%
  gather(Attributes, value, 1:4) %>%
  ggplot(aes(x=reorder(Attributes, value, FUN=median), y=value, fill=Attributes)) +
  geom_violin(show.legend=FALSE) +
  geom_boxplot(width=0.05, fill="white") +
  labs(title="Iris data set",
       subtitle="Violin plot for each attribute") +
  theme_bw() +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank())

# Boxplot for each attribute--------------------------------
# muestra un resumen de los datos en cinco medidas descriptivas
# (de abajo - arriba)
# min
# cuartil inferior
# mediana
# cuartil superior
# max
# valores atipicos
iris2 %>%
  gather(Attributes, value, 1:4) %>%
  ggplot(aes(x=reorder(Attributes, value, FUN=median), y=value, fill=Attributes)) +
  geom_boxplot(show.legend=FALSE) +
  labs(title="Iris data set",
       subtitle="Boxplot for each attribute") +
  theme_bw() +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank())

# Scatter plot and correlations --------------------------------
# Un diagrama de dispersión muestra la fuerza, la dirección 
# y la forma de la relación entre dos variables cuantitativas. 
# Un coeficiente de correlación mide la fuerza de esa relación 
ggpairs(cbind(iris2, Cluster=as.factor(iris2$Species)),
        columns=1:4, aes(colour=Cluster, alpha=0.5),
        lower=list(continuous="points"),
        axisLabels="none", switch="both") +
  theme_bw() 


# --------------------------------Data preparation--------------------------------

# We have to normalize the quantitative variables to express them in the same range of values. 

# Normalization of all columns except Species
dataNorm <- iris
dataNorm[, -5] <- scale(iris[, -5])

# Then we split the data set into two parts: a training set and a test set. The first is used to 
# train the system, while the second is used to evaluate the trained system.

# Reproducible results
# Fijar una semilla significa inicializar un generador de números pseudoaleatorios.
set.seed(1234)

# 70% train and 30% test
ind <- sample(2, nrow(dataNorm), replace=TRUE, prob=c(0.7, 0.3))
trainData <- dataNorm[ind==1,]
testData <- dataNorm[ind==2,]

# Once we have done the data analysis and the data set has been normalized and divided in two parts, 
# we can execute the k-NN algorithm. 

# -------------------------------------  ** k-NN execution ** ------------------------------------------------------------------------------

# The knn() function has the following main arguments:
  
# train: Matrix or data frame of training set cases.

# test: Matrix or data frame of test set cases. A vector will be interpreted as a row vector for a single case.

# cl: Factor of true classifications of training set.

# k: Number of neighbours considered.

# usage------------------------
# knn(train, test, 
#     cl, k = 1, l = 0, prob = FALSE, use.all = TRUE)

# Execution of k-NN with k=1
KnnTestPrediction_k1 <- knn(trainData[,-5], testData[,-5],
                            trainData$Species, k=1, prob=TRUE)

# Execution of k-NN with k=2
KnnTestPrediction_k2 <- knn(trainData[,-5], testData[,-5],
                            trainData$Species, k=2, prob=TRUE)

# Execution of k-NN with k=3
KnnTestPrediction_k3 <- knn(trainData[,-5], testData[,-5],
                            trainData$Species, k=3, prob=TRUE)

# Execution of k-NN with k=4
KnnTestPrediction_k4 <- knn(trainData[,-5], testData[,-5],
                            trainData$Species, k=4, prob=TRUE)

# ----------------------------- Evaluation ------------------------------------------------------------------------------------------
#  matriz de confusion y accuracy

# We can use the confusion matrix to evaluate the accuracy of the previous classifications with 
# different values of `k`, and study which one offers the best results. 

# Confusion matrix of KnnTestPrediction_k1
table(testData$Species, KnnTestPrediction_k1)

# How do we interpret this matrix? 
  
# The 10 observations in the test data corresponding to setosa species are correctly predicted as setosa. 

# The 12 observations in the test data corresponding to versicolor species are correctly predicted as versicolor. 

# 14 of the 16 observations in the test data corresponding to virginica species are correctly predicted as virginica. The other two are misclassified as versicolor. 

# We can calculate the classification accuracy as follows, 

# Classification accuracy of KnnTestPrediction_k1
sum(KnnTestPrediction_k1==testData$Species)/length(testData$Species)*100

# The results of the other classifications:
  
# Confusion matrix of KnnTestPrediction_k2
table(testData$Species, KnnTestPrediction_k2)

# Classification accuracy of KnnTestPrediction_k2
sum(KnnTestPrediction_k2==testData$Species)/length(testData$Species)*100

# Confusion matrix of KnnTestPrediction_k3
table(testData$Species, KnnTestPrediction_k3)

# Classification accuracy of KnnTestPrediction_k3
sum(KnnTestPrediction_k3==testData$Species)/length(testData$Species)*100

# Confusion matrix of KnnTestPrediction_k4
table(testData$Species, KnnTestPrediction_k4)

# Classification accuracy of KnnTestPrediction_k4
sum(KnnTestPrediction_k4==testData$Species)/length(testData$Species)*100

# To study graphically which value of `k` gives us the best classification, 
# --------------------------- we can plot Accuracy vs Choice of `k`. -------------------------------------------------------------------

#### Se procede a analizar cuáles son los mejores valores que K debería tomar, para ello realiza un ciclo donde K va tomar valores entre 1 y 100
###y va realizar la predicción con el modelo de entrenamiento luego se evalua la exactitud de la predicción con el modelo de prueba. En este caso
### se puede observar que los valores optimos de K se encuentran en un intervalo entre 4 y 24

# Empty variables
KnnTestPrediction <- list()
accuracy <- numeric()

# From k=1 to k=100...
for(k in 1:100){
  
  # KnnTestPrediction for each k
  KnnTestPrediction[[k]] <- knn(trainData[,-5], testData[,-5], trainData$Species, k, prob=TRUE)
  
  # Accuracy for each k   
  accuracy[k] <- sum(KnnTestPrediction[[k]]==testData$Species)/length(testData$Species)*100
  
}

# Accuracy vs Choice of k
plot(accuracy, type="b", col="dodgerblue", cex=1, pch=20,
     xlab="k, number of neighbors", ylab="Classification accuracy", 
     main="Accuracy vs Neighbors")

# Add lines indicating k with best accuracy
abline(v=which(accuracy==max(accuracy)), col="darkorange", lwd=1.5)

# Add line for max accuracy seen
abline(h=max(accuracy), col="grey", lty=2)

# Add line for min accuracy seen 
abline(h=min(accuracy), col="grey", lty=2)

# We see that 10 different values of `k` achieve the highest accuracy. Also notice that, 
################### as `k` increases, the accuracy decreases. ##################

# Summary

# In this kernel we have learned about the k-Nearest Neighbors algorithm, including the data preparation
# before we execute it (data normalization and division in two parts) and the evaluation of the results. 