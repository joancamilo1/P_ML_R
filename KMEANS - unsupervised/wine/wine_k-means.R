# k-means is an unsupervised machine learning algorithm
# algoritmo basado en distancias
# used to find groups of observations (clusters) that share similar characteristics. 
# What is the meaning of unsupervised learning? It means that 
# the observations given in the data set are unlabeled, there is no outcome to be predicted. 

# We are going to use a Wine data set to cluster different types of wines. 
# This data set contains the results of a chemical analysis of wines grown in a specific area of Italy. 

# Loading data

# First we need to load some libraries and read the data set.

# Load libraries

#install.packages('tidyverse')
#install.packages('corrplot')
#install.packages('gridExtra')
#install.packages('GGally')
#install.packages('knitr')
library(tidyverse)  # manipulación, importación, exploración y visualización de datos
library(corrplot)   # Se usa para poder gráficar correlaciones 
library(gridExtra)  # Librería para realizar gráficos
library(GGally)     # graficar en forma de matriz un set de datos con múltiples variables, 
                      ##el resultado es una correlación de las variables elegidas en dicho dataset.
library(knitr)      # Esta librería se usa para documentos escritos en R markdown, combina textos y análsis hacia otros formatos

mypath <- dirname(rstudioapi::getActiveDocumentContext()$path) 
setwd(mypath)

# Read the stats
wines <- read.csv("Wine.csv")

# We don't need the `Customer_Segment` column. As we have said before, k-means is an unsupervised 
# machine learning algorithm and works with unlabeled data. 

# Remove the Type column
wines <- wines[, -14]    # eliminamos el etiquetado de los datos

# Let's get an idea of what we're working with.

# First rows 
head(wines)

# Last rows 
tail(wines)

# Summary
summary(wines)

# Structure 
str(wines) #  indica la clase de todas las variables en el dataset, el número de observaciones y de variables existentes

# ------------------------------------ Analisis exploratorio de datos -----------------------------------------------------------------------------------

# Data analysis

# First we have to explore and visualize the data.

# gather() en el equivalente a  pivot_longer(), 
# que es más fácil de usar, tiene más funciones y aún está en desarrollo activo.

#     df %>% gather("key", "value", x, y, z)  es equivalente a 
#     df %>% pivot_longer(c(x, y, z), 
#                         names_to = "key", 
#                         values_to = "value")


# Histogram for each Attribute------------------------------
# número de frecuencias de un punto de datos específico.
wines %>%
  gather(Attributes, value, 1:13) %>%
  ggplot(aes(x=value, fill=Attributes)) +
  geom_histogram(colour="black", show.legend=FALSE) +  # geom_histogram() divide el eje x en varios contendores para poder gráficar los 3 histogramas
  facet_wrap(~Attributes, scales="free_x") +
  labs(x="Values", y="Frequency",
       title="Wines Attributes - Histograms") +
  theme_bw()

# Density plot for each Attribute------------------------------
#  visualiza la distribución de datos en un intervalo
wines %>%
  gather(Attributes, value, 1:13) %>%
  ggplot(aes(x=value, fill=Attributes)) +
  geom_density(colour="black", alpha=0.5, show.legend=FALSE) +
  facet_wrap(~Attributes, scales="free_x") +
  labs(x="Values", y="Density",
       title="Wines Attributes - Density plots") +
  theme_bw()



# Boxplot for each Attribute  ------------------------------
# muestra un resumen de los datos en cinco medidas descriptivas

# min
# cuartil inferior
# mediana
# cuartil superior
# max
# valores atipicos
wines %>%
  gather(Attributes, values, c(1:4, 6:12)) %>%
  ggplot(aes(x=reorder(Attributes, values, FUN=median), y=values, fill=Attributes)) +
  geom_boxplot(show.legend=FALSE) +
  labs(title="Wines Attributes - Boxplots") +
  theme_bw() +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank()) +
  ylim(0, 35) +
  coord_flip()


# We haven't included magnesium and proline, since their values are very high and worsen the visualization.

# What is the relationship between the different attributes? We can use the `corrplot()` 
# function to create a graphical display of a correlation matrix. 

# Correlation matrix ------------------------------
# mide el grado de asociación entre las variables y el tipo de relación (positiva o negativa)
corrplot(cor(wines), type="upper", method="ellipse", tl.cex=0.9)


# There is a strong linear correlation between `Total_Phenols` and `Flavanoids`. We can model 
# the relationship between these two variables by fitting a linear equation.


# Relationship between Phenols and Flavanoids----------------------------
#identificamos que hay una correlación fuerte entre los fenoles y los flavanoides
ggplot(wines, aes(x=Total_Phenols, y=Flavanoids)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  labs(title="Wines Attributes",
       subtitle="Relationship between Phenols and Flavanoids") +
  theme_bw()
# Se puede observar que estas dos variables tienen una relación líneal positiva

# Now that we have done a exploratory data analysis, we can prepare the data in order to execute 
# the k-means algorithm. 

# --------------------------- Data preparation ---------------------------

# We have to normalize the variables to express them in the same range of values. 
# In other words, normalization means adjusting values measured on different scales to a common scale.

# Normalization ---------------------------
winesNorm <- as.data.frame(scale(wines))

# Original data
p1 <- ggplot(wines, aes(x=Alcohol, y=Malic_Acid)) +
  geom_point() +
  labs(title="Original data") +
  theme_bw()

# Normalized data 
p2 <- ggplot(winesNorm, aes(x=Alcohol, y=Malic_Acid)) +
  geom_point() +
  labs(title="Normalized data") +
  theme_bw()

# Subplot
grid.arrange(p1, p2, ncol=2)


# The points in the normalized data are the same as the original one. The only thing that changes 
# is the scale of the axis.

# -------------------------------- k-means execution  ------------------------------------------------------------------------------------------------

# In this section we are going to execute the k-means algorithm and analyze the main components that the function returns. 


set.seed(1234) # inicializar un generador de números pseudoaleatorios.

# Execution of k-means with k=2--------------------------------

# usage
# kmeans(x, centers, iter.max = 10, nstart = 1,
#        algorithm = c("Hartigan-Wong", "Lloyd", "Forgy",
#                      "MacQueen"), trace=FALSE)

wines_k2 <- kmeans(winesNorm, centers=2)


#The kmeans() function returns an object of class "kmeans" with information about the partition: 
  
# cluster: A vector of integers indicating the cluster to which each point is allocated.

# centers: A matrix of cluster centers.

# size: The number of points in each cluster.


# Cluster to which each point is allocated
wines_k2$cluster # Podemos observar en qué cluster clasificó a cada vino

# Cluster centers
wines_k2$centers  # coordenadas de los 2 centroides para las 13 dimensiones

# Cluster size
wines_k2$size # me indica el número de registros que quedaron en cada cluster


#Además, la función kmeans() devuelve algunas proporciones que nos permiten saber qué tan compacto es un clúster
# y cuán diferentes son varios grupos entre sí.

# (ENTRE)betweenss: La suma de cuadrados entre grupos. En una segmentación óptima, se espera que esta relación sea tan
#  ALTO POSIBLE, ya que nos gustaría tener grupos heterogéneos.

# (INTRA) withinss: Vector de suma de cuadrados dentro de un grupo, un componente por grupo. En una segmentación óptima,
# se espera que esta relación sea LO MAS BAJA POSIBLE para cada grupo, ya que nos gustaría tener homogeneidad
# dentro de los grupos.

# tot.withinss: suma total de cuadrados dentro del grupo.

# totss: La suma total de cuadrados.


# Between-cluster sum of squares
wines_k2$betweenss  # (ENTRE) lo más alejados posibles, para que sean muy heterogeneos

# Within-cluster sum of squares
wines_k2$withinss  # (INTRA) lo más pequeña posible, que sean muy homogeneos

# Total within-cluster sum of squares 
wines_k2$tot.withinss  ## es la suma de cuadrados dentro del conglomerado. Entonces da como resultado un vector con un número para cada grupo. 
##Se espera que esta relación sea lo más baja posible para cada conglomerado, 
##ya que nos gustaría tener homogeneidad dentro de los conglomerados.

# Total sum of squares
wines_k2$totss


# ----------------------------------------- How many clusters?  ------------------------------------------------

# To study graphically which value of `k` gives us the best partition, we can plot `betweenss` and `tot.withinss` 
# vs Choice of `k`. 

bss <- numeric()
wss <- numeric()

# Run the algorithm for different values of k 
set.seed(1234)

for(i in 1:10){
  
  # For each k, calculate betweenss and tot.withinss
  bss[i] <- kmeans(winesNorm, centers=i)$betweenss
  wss[i] <- kmeans(winesNorm, centers=i)$tot.withinss
  
}
# (ENTRE)
# Between-cluster sum of squares vs Choice of k
p3 <- qplot(1:10, bss, geom=c("point", "line"), 
            xlab="Number of clusters", ylab="Between-cluster sum of squares") +
  scale_x_continuous(breaks=seq(0, 10, 1)) +
  theme_bw()

# (INTRA)
# Total within-cluster sum of squares vs Choice of k
p4 <- qplot(1:10, wss, geom=c("point", "line"),
            xlab="Number of clusters", ylab="Total within-cluster sum of squares") +
  scale_x_continuous(breaks=seq(0, 10, 1)) +
  theme_bw()

# Subplot
grid.arrange(p3, p4, ncol=2)

# Gráficamente se puede observar que las mayores diatncias se dan en los 3 primeros puntos en ambos gráficos, 
# esto nos indica que el número óptimo de cluster deberían ser 3


# Which is the optimal value for `k`? One should choose a number of clusters so that adding another cluster 
# doesn't give much better partition of the data. At some point the gain will drop, giving an angle in the 
# graph (elbow criterion). The number of clusters is chosen at this point. In our case, it is clear that 3 
# is the appropriate value for `k`. 

# Results


# Execution of k-means with k=3  ------------------------------------------------------------------
set.seed(1234)

wines_k3 <- kmeans(winesNorm, centers=3)

# Mean values of each cluster
aggregate(wines, by=list(wines_k3$cluster), mean)

# ------------------- VISUALIZACION DE CLUSTERS --------------------
# Clustering 
ggpairs(cbind(wines, Cluster=as.factor(wines_k3$cluster)),
        columns=1:6, aes(colour=Cluster, alpha=0.5),
        lower=list(continuous="points"),
        upper=list(continuous="blank"),
        axisLabels="none", switch="both") +
        theme_bw()

##Al grficar se puede observar que las características de cada vino son homogeneas al interior del cluster y diferentes entre grupos

# Summary

# In this entry we have learned about the k-means algorithm, including the data normalization before we execute it, 
# the choice of the optimal number of clusters (elbow criterion) and the visualization of the clustering.
