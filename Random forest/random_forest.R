#Synopsis

# As the publisher of this dataset suggests; due to privacy and logistic issues, only physicochemical (inputs) and sensory (the output) variables are available (e.g. there is no data about grape types, wine brand, wine selling price, etc.).
# This specific dataset can be viewed as classification or a regression task. The classes are ordered and not balanced (e.g. there are much more normal wines than excellent or poor ones).
# As the publisher suggest, we will classify the wines by setting an arbitrary cutoff for our dependent variable (wine quality) at e.g. 7 or higher getting classified as 'good/1' and the remainder as 'not good/0'. For this specific kernel we will be doing some basic data explorations and will be doing classification utilizing a quick random forest as our baseline model. 

## Como sugiere el editor de este conjunto de datos; debido a cuestiones de privacidad y logística, solo se dispone de variables fisicoquímicas (entradas) y sensoriales (salida) (p. ej., no hay datos sobre tipos de uva, marca de vino, precio de venta del vino, etc.).
# Este conjunto de datos específico se puede ver como una clasificación o una tarea de regresión. Las clases están ordenadas y no equilibradas (por ejemplo, hay muchos más vinos normales que excelentes o malos).
# Como sugiere el editor, clasificaremos los vinos estableciendo un punto de corte arbitrario para nuestra variable dependiente (calidad del vino) en p. 7 o más siendo clasificado como 'bueno/1' y el resto como 'no bueno/0'. 
#Para este kernel específico, haremos algunas exploraciones de datos básicos y haremos una clasificación utilizando un bosque aleatorio rápido como nuestro modelo de referencia.


#Let's have a peek of our dataset


#Libraries needed
# install.packages('ggplot2')
# install.packages('ggthemes')
# install.packages('corrplot')
# install.packages('reshape2') ## Es un paquete de R que facilita transformación de datos entre los formatos Ancho y Largo.
# install.packages('dplyr')
# install.packages('randomForest') ## para hacer bosques aleatorios
library(ggplot2)
library(ggthemes)
library(corrplot)
library(reshape2)
library(dplyr)
library(randomForest)


setwd("C:/Users/jurbano/Desktop/Especializacion Analitica/Semestre 1/Aprendizaje automatico/Bosques aleatorios") ##definimos el escritorio de trabajo



#Load in our dataset
redwine<-read.csv("winequality-red.csv")

#Create a variable indicating if a wine is good or bad
##Crea una nueva variable para clasificar a los vinos buenos(1) y malos (0) si su calidad es superior a 6.1
redwine$good.wine<-ifelse(redwine$quality>6,1,0)

#Let's look at some summary statistics
str(redwine) ###realiza un chequeo de cuántas variables, qué tipo y algunas observaciones con las que cuenta el dataset
summary(redwine) ###Observamos medidas de tendencia central de todas las variables (porque son numéricas)


# As we can see, we will be working with an unbalanced dataset, wherein only around 13.57% out of 1599 wines is considered as a good wine.

#Exploratory Data Analysis

# In this section we will be doing some exploratory data analysis to have a better understanding of the data we are working with

##Correlation of Variables

##Realiza un análisis de dispersión entre todas las variables del data set

#Scatterplot Matrix of Variables
plot(redwine)
##De acuerdo con el resultado hay una aparente relación positiva entre la acidez fija y la densidad 
##también parece haber una relación inversa entre el PH y la acidez fija

#Correlation Heatmap of Variables
corrplot(cor(redwine))
## Efectivamente hay una correlación positiva entre la acidez y la densidad y una correlación negativa entre la acidez y el PH
##También se identifica una corrlación positiva entre el alcohol y la calidad del vino


##Estamos tratando de predecir la calidad del vino, por lo que nos preocupamos por las últimas 2 columnas/filas para saber cuál de las variables tiene la relación más fuerte con la calidad del vino. 
##Como sugiere el mapa de calor, el alcohol tiene la correlación más fuerte con la calidad del vino.

##Wine Quality


#Distribution of red wine quality ratings
ggplot(redwine,aes(x=quality))+geom_bar(stat = "count",position = "dodge")+
  scale_x_continuous(breaks = seq(3,8,1))+
  ggtitle("Distribution of Red Wine Quality Ratings")+
  geom_text(aes(label=..count..), stat='count',  
            position=position_dodge(0.9), 
            vjust=-0.5, 
            size=5.0
  ) + 
  theme_classic()

###Podemos observar que los buenos vinos solo son 217 de los 1599 que se estan analizando, ya que su score es superior a 6

#Distribution of good/bad red wines
ggplot(redwine,aes(x=good.wine,fill=factor(good.wine)))+geom_bar(stat = "count",position = "dodge")+
  scale_x_continuous(breaks = seq(0,1,1))+
  ggtitle("Distribution of Good/Bad Red Wines")+
  geom_text(aes(label=..count..), stat='count',  
            position=position_dodge(0.9), 
            vjust=-0.5, 
            size=5.0
  ) + 
  theme_classic()


# Above plot shows what we have inferred previously, that good wines were outnumbered by bad wines by a large margin. Most wines were mediocre (rated 5 or 6), but we could also see that there are some poor wines (3 or 4). A vast majority of good wines has a quality rating of 7.

###Physiochemical Properties and Wine Quality

# In this section we would be looking at the relationship between the physiochemical properties and whether a wine is good or not.

### Ahora se va a analizar las relaciones entre las propiedades fisicoquímicas del vino dependiendo de si es bueno o no

#Fixed Acidity and Wine Quality
ggplot(redwine,aes(x=fixed.acidity,fill=factor(good.wine)))+geom_density(alpha=0.25)+
  geom_vline(aes(xintercept=mean(fixed.acidity[good.wine==0],na.rm=T)),color="red",linetype="dashed",lwd=1)+
  geom_vline(aes(xintercept=mean(fixed.acidity[good.wine==1],na.rm=T)),color="blue",linetype="dashed",lwd=1)+
  scale_x_continuous(breaks = seq(4,16,1))+
  xlab(label = "Fixed Acidity Level")+
  ggtitle("Distribution of Fixed Acidity Levels")+
  theme_classic()

## En el gráfico se puede observar que los buenos vinos tienen una acidez promedio de 9, mientras que los malos vinos tienen menos acidez, en promedio de 8

#Volatile Acidity and Wine Quality
ggplot(redwine,aes(x=volatile.acidity,fill=factor(good.wine)))+geom_density(alpha=0.25)+
  geom_vline(aes(xintercept=mean(volatile.acidity[good.wine==0],na.rm=T)),color="red",linetype="dashed",lwd=1)+
  geom_vline(aes(xintercept=mean(volatile.acidity[good.wine==1],na.rm=T)),color="blue",linetype="dashed",lwd=1)+
  scale_x_continuous(breaks = seq(0,1.6,0.1))+
  xlab(label = "Volatile Acidity Level")+
  ggtitle("Distribution of Volatile Acidity Levels")+
  theme_classic()

## Los buenos vinos presentan una volatilidad de la acidez promedio de 0.4, mientras que los vinos malos presentan una 
##Volatilidad promedio de la acidez entre 0,5 y 0,6. Además se puede observar que hay menor dispersión en los vinos buenos frente a los vinos malos


#Citric Acid and Wine Quality
ggplot(redwine,aes(x=citric.acid,fill=factor(good.wine)))+geom_density(alpha=0.25)+
  geom_vline(aes(xintercept=mean(citric.acid[good.wine==0],na.rm=T)),color="red",linetype="dashed",lwd=1)+
  geom_vline(aes(xintercept=mean(citric.acid[good.wine==1],na.rm=T)),color="blue",linetype="dashed",lwd=1)+
  scale_x_continuous(breaks = seq(0,1,0.1))+
  xlab(label = "Citric Acid Level")+
  ggtitle("Distribution of Citric Acid Levels")+
  theme_classic()

##Se puede observar que los niveles de acído citrico promedio de los vinos buenos son cercanos a 4 y para los vinos malos cercanos a 3

#Residual Sugar and Wine Quality
ggplot(redwine,aes(x=residual.sugar,fill=factor(good.wine)))+geom_density(alpha=0.25)+
  geom_vline(aes(xintercept=mean(residual.sugar[good.wine==0],na.rm=T)),color="red",linetype="dashed",lwd=1)+
  geom_vline(aes(xintercept=mean(residual.sugar[good.wine==1],na.rm=T)),color="blue",linetype="dashed",lwd=1)+
  scale_x_continuous(breaks = seq(0.5,15.5,1))+
  xlab(label = "Residual Sugar Level")+
  ggtitle("Distribution of Residual Sugar Levels")+
  theme_classic()

###No hay grandes diferencias en los niveles promedio de azucar residual entre los vinos buenos y los malos
##Sin embargo, los vinos buenos presentan un nivel levemente más alto de azucar

#Chlorides and Wine Quality
ggplot(redwine,aes(x=chlorides,fill=factor(good.wine)))+geom_density(alpha=0.25)+
  geom_vline(aes(xintercept=mean(chlorides[good.wine==0],na.rm=T)),color="red",linetype="dashed",lwd=1)+
  geom_vline(aes(xintercept=mean(chlorides[good.wine==1],na.rm=T)),color="blue",linetype="dashed",lwd=1)+
  scale_x_continuous(breaks = seq(0.01,0.62,0.1))+
  xlab(label = "Chlorides Level")+
  ggtitle("Distribution of Chlorides Levels")+
  theme_classic()

###No hay grandes diferencias en los niveles promedio de cloruro entre los vinos buenos y los malos
##Sin embargo, los vinos buenos presentan un nivel levemente más bajo de cloruro

#Free Sulfur Dioxide and Wine Quality
ggplot(redwine,aes(x=free.sulfur.dioxide,fill=factor(good.wine)))+geom_density(alpha=0.25)+
  geom_vline(aes(xintercept=mean(free.sulfur.dioxide[good.wine==0],na.rm=T)),color="red",linetype="dashed",lwd=1)+
  geom_vline(aes(xintercept=mean(free.sulfur.dioxide[good.wine==1],na.rm=T)),color="blue",linetype="dashed",lwd=1)+
  scale_x_continuous(breaks = seq(0,72,8))+
  xlab(label = "Free Sulfur Dioxide Level")+
  ggtitle("Distribution of Free Sulfur Dioxide Levels")+
  theme_classic()

### Los vinos buenos presentan niveles más bajos de dioxido de azufre libre

#Total Sulfur Dioxide and Wine Quality
ggplot(redwine,aes(x=total.sulfur.dioxide,fill=factor(good.wine)))+geom_density(alpha=0.25)+
  geom_vline(aes(xintercept=mean(total.sulfur.dioxide[good.wine==0],na.rm=T)),color="red",linetype="dashed",lwd=1)+
  geom_vline(aes(xintercept=mean(total.sulfur.dioxide[good.wine==1],na.rm=T)),color="blue",linetype="dashed",lwd=1)+
  scale_x_continuous(breaks = seq(0,300,20))+
  xlab(label = "Total Sulfur Dioxide Level")+
  ggtitle("Distribution of Total Sulfur Dioxide Levels")+
  theme_classic()

##Los niveles totales promedio de dioxido de sulfuro de los vinos buenos son más bajos que los de los vinos malos
## Para los vinos buenos este estos niveles se encuentran entre 30 y 40 y para los malos entre 40 y 60


#Density and Wine Quality
ggplot(redwine,aes(x=density,fill=factor(good.wine)))+geom_density(alpha=0.25)+
  geom_vline(aes(xintercept=mean(density[good.wine==0],na.rm=T)),color="red",linetype="dashed",lwd=1)+
  geom_vline(aes(xintercept=mean(density[good.wine==1],na.rm=T)),color="blue",linetype="dashed",lwd=1)+
  scale_x_continuous(breaks = seq(0.9,1.1,0.05))+
  xlab(label = "Red Wine Density Level")+
  ggtitle("Distribution of Red Wine Density Levels")+
  theme_classic()

##Los vinos buenos presentan un nivel de densidad menor al de los vinos malos


#PH and Wine Quality
ggplot(redwine,aes(x=pH,fill=factor(good.wine)))+geom_density(alpha=0.25)+
  geom_vline(aes(xintercept=mean(pH[good.wine==0],na.rm=T)),color="red",linetype="dashed",lwd=1)+
  geom_vline(aes(xintercept=mean(pH[good.wine==1],na.rm=T)),color="blue",linetype="dashed",lwd=1)+
  scale_x_continuous(breaks = seq(2.5,5,0.5))+
  xlab(label = "Red Wine PH Level")+
  ggtitle("Distribution of Red Wine PH Levels")+
  theme_classic()

### Los vinos buenos prresentan un ph levemente más bajo que el de los vinos malos

#Sulphates and Wine Quality
ggplot(redwine,aes(x=sulphates,fill=factor(good.wine)))+geom_density(alpha=0.25)+
  geom_vline(aes(xintercept=mean(sulphates[good.wine==0],na.rm=T)),color="red",linetype="dashed",lwd=1)+
  geom_vline(aes(xintercept=mean(sulphates[good.wine==1],na.rm=T)),color="blue",linetype="dashed",lwd=1)+
  scale_x_continuous(breaks = seq(0,2,0.25))+
  xlab(label = "Sulphates Level")+
  ggtitle("Distribution of Sulphates Levels")+
  theme_classic()

###Los nievles promedio de sulfatos en los vinos buenos son mayores a los de los vinos malos, en promedio de 0,75

#Alcohol and Wine Quality
ggplot(redwine,aes(x=alcohol,fill=factor(good.wine)))+geom_density(alpha=0.25)+
  geom_vline(aes(xintercept=mean(alcohol[good.wine==0],na.rm=T)),color="red",linetype="dashed",lwd=1)+
  geom_vline(aes(xintercept=mean(alcohol[good.wine==1],na.rm=T)),color="blue",linetype="dashed",lwd=1)+
  scale_x_continuous(breaks = seq(8,15,1))+
  xlab(label = "Alcohol Level")+
  ggtitle("Distribution of Alcohol Levels")+
  theme_classic()

##Los vinos buenos presentan nievles de alcohol mucho más altos que los de los vinos malos
##En promedio los vinos buenos presentan un nivel de alcohol entre 11 y 12 y los malos entre 10 y 11

##Los gráficos exploratorios anteriores muestran que los vinos buenos y malos tienen una distribución muy similar de sus propiedades fisicoquímicas correspondientes. 
##Los atributos más discriminatorios que podemos observar son los sulfatos y el nivel de alcohol del vino.

##Predictive Modelling (Binary Classification)

##No se agregaran hiperparamétros a a la clasificación, se usarán por defecto las que trae la función randomforest
# As indicated in the synopsis, we would utilize a random forest as our baseline model in predicting the quality of a wine. We will not be using any hyper-parameter tuning and stick with the default of the randomForest function

table(redwine$good.wine)

#Baseline Random Forest Model
redwineRF<-randomForest(factor(good.wine)~.-quality,redwine,ntree=150)
redwineRF


# La precisión general de nuestro modelo es bastante buena, alrededor del 92 % en general ya que la tasa de error es del 8,19% 
##Sin embargo, pudimos ver claramente que es mucho mejor para predecir malos vinos que buenos.

###Variable Importance

### Se observa cuál o cuales son las variables más importantes para la clasificación de los vinos como buenos o malos

# Get importance

##Obtiene la variable importancia de la clasificación
importance    <- importance(redwineRF)

## La pasa de una lista a un data frame para poder ordenarla en un granking
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

##realiza el gráfico para ver cuáles son las variables más importantes en la clasificación
# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_classic()

##Se puede observar que los resultados de la clasificación son congruentes con los de los análisis exploratorios


# Aha! The results of our Random Forest Model coincides nicely with our previous observation from the density plots that alcohol and sulphates level are the most discriminating factor of wine quality.

##Wrapping it up

# To wrap it all up, this has been a good investigation of red wine quality. Further analysis can still be made with the data and better models can also be built. But for this particular kernel, we only utilized a simple random forest model as our baseline. That's all of it for now and will be updating this kernel when I have the time. Download the dataset do your own investigation or you may also fork this script if you liked it. Thanks and God Bless!