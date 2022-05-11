# https://www.cienciadedatos.net/documentos/41_machine_learning_con_r_y_caret
# **Titanic Data Analysis & Prediction **

# Background & Introduction

# Titanic is definitely a great kickstart for beginning my journey at Kaggle, the largest playground for data scientists in the globe. As one of the well-known and largest Brithsh passenger liners in the early 20 century, RMS Titanic had experienced a huge trategy during its maiden voyage from Southampton to New York City. At April 15, 1912, the luxurious ocean palance sanked after striking an iceberg during its journey of passing through the North Altanic Ocean. This trategy had caused more than 1500 passangers and crews died in the ocean. It was seen as one of the most serious marine disasters in the world. During the following decades, there were a lot of books, atricles and movies trying to reappear that horrible night and some had made it an even romantic story such as the famous "Jack and Rose" film in 1997. This movie has also made Titanic known to almost everyone in the world.

# In the next sections, I will try to analyze the survival status data to figure out the features (e.g. age, gender, social class) that had affected if a passenger would survive during that horrible night at April 15th, 1912. Then, I will apply some machine learning models to predict the survival circumstances based on the personal characteriastics. The sequence of my Titanic data analysis are:

# 1. Problem Definition
# 2. Data Loading & Cleaning
# 3. Exploratory Data Analysis
# 4. Feature Engineering
# 4. Modeling
# 5. Summary

# **1. Problem Definition**

# Problem definition is always the first step of any data analysis project, 
# as it decides the direction and angle of the following technical task. 
# Going through the background and dataset, below are some questions and hypothesis that the data could answer:

# preguntas e hipótesis que los datos podrían responder:
  
  # - ¿Las personas que se hospedaron en las cabañas avanzadas tuvieron una mayor tasa de supervivencia?
  # - ¿Las personas mayores tenían menos posibilidades de sobrevivir?
  # - ¿Cómo el tamaño de la familia afectó el estado de supervivencia? (¿El tamaño de la familia más grande aumentó la posibilidad de sobrevivir?)
  # - ¿El género tuvo algún efecto sobre el resultado de supervivencia?
  # - ¿Existe alguna relación entre el título de una persona (p. ej., Sr., Sra.) y su tasa de supervivencia?

#After answering these questions and decide which factors are important to our problem, we could put them into machine learning models to make our predictions. Next, let's take a look at the dataset:

# **2. Data Loading & Cleaning**

# First let's load the required packages:

#install.packages('dplyr')
#install.packages('ggplot2')
#install.packages('tidyr')
#install.packages('caret')
#install.packages('stringr')
#install.packages('purrr')
#install.packages('corrplot')
#install.packages('fastDummies')
#install.packages('e1071')
#install.packages('MASS')
#install.packages('rpart')
#install.packages('rpart.plot')
#install.packages('ROCR')
#install.packages('pROC')
#install.packages('readr')
#install.packages('forcats')

# paquetes de tidyverse
library(dplyr)    # paquete de tidyverse, permite la manipulación de marcos de datos
library(ggplot2)  # es un sistema para crear gráficos declarativamente
library(tidyr)    # El objetivo de tidyr es ayudarlo a crear datos ordenados . 
# Los datos ordenados son datos donde: 
# Cada columna es variable.
# Cada fila es una observación.
# Cada celda es un valor único.
library(caret)    # (classification and regression training) incluye una serie de funciones que facilitan el uso de decenas de métodos complejos de clasificación y regresión. 
library(stringr)  # funciones diseñadas para hacer que trabajar con cadenas sea lo más fácil posible.
library(purrr)    # mejora en programación funcional (FP) de R al proporcionar herramientas para trabajar con funciones y vectores. 
library(corrplot) # herramienta de exploración visual en la matriz de correlación
library(fastDummies) # creación rápida de columnas y filas ficticias (binarias) a partir de variables categóricas
library(e1071)    # Funciones misceláneas del Departamento de Estadística para el análisis de clases latentes, transformada de Fourier de tiempo corto, 
                  # agrupamiento difuso, máquinas de vectores de soporte, cálculo de la ruta más corta, 
                  # agrupamiento en bolsas, clasificador de Bayes ingenuo, k-vecino más cercano generalizado ...
library(MASS)     # Funciones estadisticas y conjuntos de datos compatibles con Venables y Ripley, "Modern Applied Statistics with S" 
library(rpart)    # crea un árbol de decisión que puede usarse para pronosticar con la función predict,  a partir de un conjunto de datos, y de una fórmula de predicción
library(rpart.plot) # grafico ???
library(ROCR)     # es un paquete para evaluar y visualizar el rendimiento de los clasificadores de puntuación
library(pROC)     # Mostrar y analizar curvas ROC
library(readr)    #  proporciona una forma rápida y sencilla de leer datos rectangulares de archivos delimitados, como valores separados por comas (CSV) y valores separados por tabuladores (TSV). 

# R usa factores para manejar variables categóricas, variables que tienen un 
# conjunto fijo y conocido de valores posibles. Los factores también son útiles
# para reordenar los vectores de caracteres para mejorar la visualización. 
library(forcats)  #  proporcionar un conjunto de herramientas que resuelven problemas comunes con factores, incluido el cambio del orden de los niveles o los valores.

mypath <- dirname(rstudioapi::getActiveDocumentContext()$path) 
setwd(mypath) 

# Data Loading--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Load the training and testing data:  
train = read_csv('titanic_train.csv')
test = read_csv('titanic_test.csv')
# The read_csv function will show the read columns and their data types, it won't convert the character value into factor

# Create the survived variable to the test data for concatenation:
test$Survived = NA

# Concate the train and test into a new data "full" to better explore the whole story:
full = bind_rows(train, test)

# Take a look at the structure and basic info about the full data (similar to the str() function, with heading rows printed out):
glimpse(full) # estructura y la información básica sobre los datos completos

# Basic summary statistics:
summary(full)

# The train dataset has 891 rows and 12 columns, and the test data has 418 rows and 11 columns. There are 1309 rows in the combined data, with 12 variables. Next let's jump to the data cleaning step.

# Data Cleaning--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# First, let's check the status of missing values for each column:
#  map_int transforma su entrada aplicando una función a cada elemento y devolviendo un vector de la misma longitud que la entrada.
missing_values = map_int(full, function(x) {
  sum(is.na(x) | x == '')
})
print(missing_values)

# Let's visulize the missing results:
missing_values = data.frame(columns = factor(names(missing_values)), 
                            missing_values = missing_values)
ggplot(missing_values, 
       aes(x = fct_reorder(columns,missing_values), 
           y = missing_values)) + 
  geom_bar(stat = 'identity', fill = 'steelblue') + 
  coord_flip() + xlab("Features") + 
  ylab("Missing Values")

# 1. Los valores que faltan en la variable Survived son 418, lo cual es normal ya que pertenecen a los datos de prueba;
# 2. Hay 1014 valores faltantes en la variable Cabina, lo que dificulta su interpretación y compensación;
# 3. Hay 263 valores faltantes en la variable Edad. Dado que la edad podría ser un factor importante para nuestra pregunta, debemos cumplir con los valores faltantes tomando los valores medios o aleatorios o construir un modelo de regresión para predecir estos valores faltantes. Me ocuparé del tema del valor faltante en el tercer paso;
# 4. Faltan 2 valores en la variable Embarcado, lo que facilita la recuperación.
# 5. Falta 1 valor en la variable Tarifa, podríamos compensarlo asignando el valor medio

# Primero comencemos a tratar con los valores faltantes en la variable Embarcado
# Esta variable muestra qué puerto de embarque para el pasajero, 
    # C representa a Cherburgo, 
    # Q representa a Queenstown y
    # S representa a Southampton

# Vamos a asignar la moda a los valores que faltan ya que la mayoría de la gente se embarcó en Southampton.
table(full$Embarked)
full$Embarked = ifelse(is.na(full$Embarked) |
                         full$Embarked == '', "S", full$Embarked)

# Compense el valor faltante de la tarifa asignándole el valor medio:
full$Fare[is.na(full$Fare) |
            full$Fare == ''] = mean(full$Fare, na.rm = TRUE)

# Para la variable Edad, dado que tiene más valores perdidos, podríamos usar varias formas de crear los NAs, 
# como asignar valores medios o medianos, usar modelos de regresión o números aleatorios. 
# Aquí, usaré una forma relativamente simple, que consiste en asignar un valor de edad aleatorio a cada valor faltante.

# Primero, veamos si las distribuciones de la edad de los hombres y la edad de las mujeres son diferentes:
# Histogram of Age for both genders
hist(full$Age[!is.na(full$Age)],
     col = 'steelblue',
     xlab = "Age",
     main = "Age Distribution")

# Density plot of ages for male and female:
ggplot(data = full[!is.na(full$Age), ], aes(x = Age, col = Sex))+ geom_density(size = 1.2)

# Como podemos encontrar aquí, la altura de la curva de densidad de edad para los hombres 
# es ligeramente más alta que la de las mujeres en el medio, lo que significa que
# había más hombres en la mediana edad que mujeres. Así que llenaré las edades que faltan aquí por género.
# Ahora tratemos con los valores faltantes con números aleatorios de la variable Edad:

# Make two lists of random numbers for assigining the age values to the missings ones
table(full$Sex[is.na(full$Age)])

# Random numbers for male: ------------------------------------
# creamos los numeros de forma aleatoria con el tama;o de los faltantes (185)
rand_age_male = sample(full$Age[!is.na(full$Age) &
                                  full$Sex == "male"], size = 185, replace = TRUE)
# asiganmos edades aleatorias donde sean na 
full$Age[is.na(full$Age) & full$Sex == 'male'] = rand_age_male

# Random numbers for female: ------------------------------------
# creamos los numeros de forma aleatoria con el tama;o de los faltantes (78)
rand_age_female = sample(full$Age[!is.na(full$Age) &
                                    full$Sex == "female"], size = 78, replace = TRUE)
# asiganmos edades aleatorias donde sean na 
full$Age[is.na(full$Age) & full$Sex == 'female'] = rand_age_female

# Let's check the filled Age variable again:

# For the Ages of both male and female:
hist(full$Age,
     col = 'steelblue',
     xlab = "Age",
     main = "Age Distribution")

# Age by gender:
ggplot(data = full, aes(x = Age, col = Sex)) + geom_density(size = 1.2)

# Las distribuciones de edad se parecen a la que había antes de llenar los valores faltantes,
# por lo que nuestro método de números aleatorios hizo un trabajo decente. Luego,
# soltaremos algunas columnas que no se utilizarán en nuestros modelos de entrenamiento:

# Dado que faltan demasiados datos que dificultan el llenado, aquí descarté la variable Cabin:
full$Cabin = NULL

# También soltaré aquí la variable ID del pasajero ya que no tiene ningún significado para nuestra predicción.
# También soltaré la variable Ticket aquí para un análisis simple, aunque podría contener información importante si se hace un análisis más profundo.
full$PassengerId = NULL
full$Ticket = NULL

# Después de completar todos los valores faltantes y descartar las funciones no utilizadas, 
# verifiquemos de nuevo el estado de los valores faltantes:
colSums(is.na(full))

# Convert some variables into factors: ----------------------
# Los factores facilitan trabajar con variables categoricas en un modelo estadistico 
# porque las variables se codifican como numeros. Sin embargo, pueden llevar a confusion porque parecen strings y se comportan como integers.
full$Survived = factor(full$Survived)
full$Pclass = factor(full$Pclass)
full$Sex = factor(full$Sex)
full$Embarked = factor(full$Embarked)

# 3. Exploratory Data Analysis (EDA):  --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Primero, verifiquemos el estado general del estado de supervivencia (conjunto de datos de entrenamiento):
ggplot(data = full[!is.na(full$Survived), ], aes(Survived, fill = Survived)) + 
  stat_count(width = 0.6) + 
  labs(x = "Survived", y = "Count", title = "Survival Status") + 
  geom_label(aes(label = ..count..), stat = 'count', size = 7) + 
  theme_grey() + 
  ylim(c(0, 600))

# Hay 549 personas muertas y 342 sobrevivieron en el conjunto de datos de entrenamiento. Dado que hubo más de 1500 personas que no sobrevivieron durante el hundimiento, podría ser el mismo estado si nuestra muestra se selecciona al azar.

# Verifique las relaciones entre el estado de supervivencia y el género  --------------------
# (¿los hombres tenían más posibilidades de sobrevivir que las mujeres?)
ggplot(data = full[!is.na(full$Survived), ], aes(x = Sex, fill = Survived)) +
  geom_bar(position = 'dodge', width = 0.4) + 
  geom_label(aes(label = ..count..), stat = 'count', size = 6) + 
  labs(x = 'Survival status by gender', y = 'Count', title = 'Survival Status by Gender') + 
  theme_grey()

# Muestra que las mujeres tenían un mayor número de sobrevivientes y tenían mayores posibilidades de sobrevivir que los machos. Por lo tanto, el género jugaría un papel importante para decidir si una persona sobreviviría durante el desastre.

# Compruebe si la edad tiene un impacto en las posibilidades de supervivencia de una persona  ------------------
# (en su conjunto y en comparación por género)

ggplot(data = full[!is.na(full$Survived), ], aes(x = Age, col = Survived)) + 
  geom_freqpoly(size = 1.2, bins = 30)  + 
  labs(x = "Age", y = "Count", 
       title = "Survival VS Age")
# Parece que, en general, los niños menores de 10 años tienen una mayor tasa de supervivencia que otras etapas de edad. 
# Las personas en el grupo de personas mayores (edad >= 60) tuvieron una tasa de supervivencia mucho más baja.


ggplot(data = full[!is.na(full$Survived), ], aes(x = Age, col = Survived)) + 
  geom_freqpoly(size = 1.2, bins = 30) + 
  facet_wrap(. ~  Sex) +  # nos separa por sexo
  labs(x = "Age", y = "Count", title = "Survival VS Age VS Gender")
# En comparación con los machos, las hembras también tienen tasas de supervivencia mucho más altas
# en cada etapa de edad que los machos. A continuación, 

# echemos un vistazo a la variable de clase:
# Verifique las relaciones entre la Clase de Pasajero y su Estado de Supervivencia (también por género):------------------
ggplot(data = full[!is.na(full$Survived), ], aes(x = Pclass, fill = Survived)) +
  geom_bar(position = 'dodge', width = 0.6) + 
  labs(x = 'Passenger Class', y = 'Count', title = 'Survival status by Passenger Classes')


ggplot(data = full[!is.na(full$Survived), ], aes(x = Pclass, fill = Survived)) +
  geom_bar(position = 'dodge', width = 0.6) + facet_wrap(. ~ Sex) + labs(
    x = 'Passenger Class',
    y = 'Count',
    title = 'Survival status by Passenger Classes',
    subtitle = 'By gender'
  )

# Check the Age distribution by classes and sex:
ggplot(data = full[!is.na(full$Survived), ], aes(x = Age, y = "", col = Survived)) + 
  geom_jitter(size = 1.5, alpha = 0.75) + 
  facet_grid(Pclass ~ Sex) +
  labs(x = "Age", title = "Survival VS Age VS Gender VS Pclass") + ylab('')

# Está claro que los pasajeros de la primera clase tienen más posibilidades de supervivencia
# que las otras dos clases, es razonable ya que la primera clase podría tener mejores condiciones
# y ser más accesible para los barcos de salvamento que las otras dos clases. La mayoría de las
# hembras de la primera y segunda clase sobrevivieron, y los machos de las tres clases tienden a
# tener las menores posibilidades de supervivencia.


# echemos un vistazo al estado Embarcado (en qué puerto ingresar al barco) y Supervivencia: --------------
ggplot(data = full[!is.na(full$Survived), ], aes(x = Embarked, fill = Survived)) +
  geom_bar(position = 'dodge', width = 0.4) + 
  labs(x = 'Which port to onboarding?', y = 'Count', title = 'Survival Status by Embarked Port') + 
  geom_label(aes(label = ..count..), stat = 'count')

# Podemos encontrar que los pasajeros embarcados en el barco en Cherburgo tienen la mayor posibilidad de sobrevivir, mientras que las personas a bordo en Southampton tienen la menor posibilidad de sobrevivir.

# ¿También es interesante ver si la tarifa del boleto tiene relación con la probabilidad --------------
# de supervivencia y la clase (la clase más alta tiene un precio más alto)?
ggplot(data = full[!is.na(full$Survived), ], aes(x = Fare, col = Survived)) + 
  scale_x_log10() + geom_freqpoly(size = 1.25, bins = 30) + 
  labs(x = "Fare Amount", y = "Count", title = "Fare VS Survival") + 
  theme_grey()

ggplot(data = full[!is.na(full$Survived), ], aes(x = Pclass, y = Fare, fill = Survived)) + 
  geom_boxplot(color = 'steelblue') + ylim(c(0, 200)) + 
  labs(x = "Passenger Class", y = "Fare", title = "Fare VS Passenger Class") +
  theme_grey()

# Parece que una tarifa más alta podría conducir a una mayor probabilidad de supervivencia, 
# y está claro que la tarifa en la primera clase es más alta que en las otras dos clases. 
# Estas dos variables deben tener una fuerte correlación entre sí. También es interesante 
# ver que los que sobrevivieron en cada clase (especialmente en la primera clase) pagaron más que los que no sobrevivieron.

# Hay otras variables como SibSp (el número de hermanos o cónyuges) y Parch (el número de padres e hijos) 
# que podrían resumirse y combinarse en nuevas funciones. Los exploraré en el siguiente paso: 

# ingeniería de características. A continuación se muestra el gráfico de correlación de algunas
# variables con significados numéricos (Pclass aquí se identifica como numérico para encontrar 
# su relación con la variable tarifa):

# Extract the numeric variables:
num_vars = full[, c("Pclass", "Age", "SibSp", "Parch", "Fare")]
num_vars$Pclass = as.integer(num_vars$Pclass)

# Make the correlation plot. Here I used the Spearman correlation:
# SPEARMAN Este coeficiente es una medida de asociación lineal que utiliza los rangos, 
# números de orden, de cada grupo de sujetos y compara dichos rangos.
corrplot.mixed(corr = cor(num_vars, method = 'spearman'))

# 3. Ingeniería de características ------------------------------------------------------------------------------------------------------------------------------

# La primera parte de este paso es tratar con la variable Nombre.
# El título de cada pasajero (por ejemplo, Sr., Sra.) identifica su género,
# edad y clase social. Es importante tenerlo en cuenta
# al analizar nuestro problema de supervivencia, y muchos Kagglers han hecho mucho
# de exploración profunda a esta variable en sus cuadernos,
# por lo que es una característica popular para este caso. 

# Luego, usé la función str_remove_all
# en el paquete stringr para eliminar el contenido innecesario y dejar solo la parte del título.
# Luego guarde esto en una nueva variable "título".

# Hacer la variable "p_title" para obtener el título de la variable de nombre: 
# (la función gsub() es más popular, mientras que aquí trido la función str_remove_all)
full$title = str_remove_all(string = full$Name, pattern = '(.*, )|(\\..*)')

table(full$title)

ggplot(full[!is.na(full$Survived), ], aes(x = title, fill = Survived)) +
  geom_bar() + xlab("Title")

# Wow, tantos títulos, necesitamos hacer un resumen. Como muchos Kagglers hicieron
# en sus cuadernos, aquí resumiré estos títulos en cuatro partes: 
#  Mr, Mrs, Miss and Officer.
full$title = ifelse(full$title %in% c("Ms", "Mlle", "Dona"), "Miss", full$title)
full$title = ifelse(full$title %in% c("Mme", "Lady", "the Countess"),"Mrs",full$title)
full$title = ifelse(!full$title %in% c("Miss", "Mr", "Mrs"), "Officer", full$title)

full$title = factor(full$title) #factorizar titulo
table(full$title)                # conteo de cada titulo

# Echemos un vistazo al estado de supervivencia de los tres títulos después de agruparlos:
ggplot(full[!is.na(full$Survived), ], aes(x = title, fill = Survived)) + 
  geom_bar(width = 0.75) + xlab("Title")

# Look at the survival status by classes:
ggplot(full[!is.na(full$Survived), ], aes(x = title, fill = Survived)) + 
  geom_bar(width = 0.75, position = "fill") + 
  xlab("Title") + 
  facet_wrap(. ~ Pclass) +
  labs(title = "Survival Status by Title and Class") +
  ylab("Survival Percentage")

# Podemos encontrar aquí que las mujeres con título de señorita y señora tienen mayores posibilidades
# de supervivencia que las de señor y oficiales. 
# Esto coincide con el estado de supervivencia de la variable de género y podría usarse
# en los modelos de predicción. Cuando se muestra por clases, podemos ver que los pasajeros
# con título de señora y señorita en la primera y segunda clase tienen una tasa de supervivencia
# muy alta (> 0,8). 

# Aquí creé un nuevo grupo de ellos como "Female_High_Class":

full$female_high_class = ifelse((full$Pclass %in% c("1", "2")) &
                                  (full$title %in% c("Miss", "Mrs")), 1, 0)
full$female_high_class = factor(full$female_high_class)

# Check the number of this group:
table(full$female_high_class)

# Then, let's take a look at the SibSp and Parch variables:

# Eche un vistazo a la relación entre el número de hermanos o cónyuges de una persona y su probabilidad de supervivencia:
ggplot(full[!is.na(full$Survived), ], aes(x = SibSp, fill = Survived)) + 
  geom_bar(position = "fill") +
  ylab("Survival Percentage")

# Eche un vistazo a la relación entre el número de hijos y padres de una persona y su probabilidad de supervivencia:
ggplot(full[!is.na(full$Survived), ], aes(x = Parch, fill = Survived)) + 
  geom_bar(position = "fill") + ylab("Survival Percentage")

# Luego, sumando estas dos variables para ver la relación entre el tamaño de la familia y la tasa de supervivencia:
full$family = full$SibSp + full$Parch + 1

ggplot(full[!is.na(full$Survived), ], aes(x = factor(family), fill = Survived)) + 
  geom_bar(position = "fill") + 
  ylab("Survival Percentage")

# Podemos encontrar aquí que la tasa de supervivencia de una persona sería alta (más de 0,5) 
# si estuviera acompañada de 1 a 3 miembros de la familia.
# Para una persona sola o con más de 3 miembros en la familia, su tasa de supervivencia sería baja.
# Así que aquí creé tres contenedores para resumir el tamaño de la familia como "único", "pequeño", "grande":

# Haga una nueva variable para agrupar la variable del tamaño de la familia:

full$family_g = case_when(
  full$family == 1 ~ "single",
  full$family > 1 &
    full$family <= 4 ~ "small",
  full$family > 4 ~ "large"
)
full$family_g = factor(full$family_g)

# Let's take a look at these groups:
ggplot(full[!is.na(full$Survived), ], aes(x = family_g, fill = Survived)) + 
  geom_bar(position ="dodge", size = 0.75) + 
  geom_label(stat = 'count', aes(label = ..count..)) +
  xlab("Family Size")

# Podemos ver que el grupo "pequeño" tiene una tasa de supervivencia de alrededor del 60%, mientras que los grupos "único" y "grande" tienen tasas de supervivencia muy bajas (<30%).

# Ahora preparémonos para los conjuntos de datos de entrenamiento y prueba. Las variables que se pondrán en los modelos de predicción son Sobreviviente (Variable Y), Pclase, Sexo, Edad, Embarcado, Título, Mujer_clase_alta, Grupo familiar y Tarifa.

# Select the training and testing variables:
full = full[, c(
  "Survived",
  "Pclass",
  "Sex",
  "Age",
  "Embarked",
  "title",
  "female_high_class",
  "family_g",
  "Fare"
)]

# Divida los datos completos en archivos de entrenamiento y prueba:
# Aquí usaré el 80 % de los datos de entrenamiento (712) como train y 
# el 20 % (179) para comprobar el rendimiento de la predicción:

train = full[1:712, ]
test = full[713:891, ]

str(train)
str(test)

# 4. Modeling Process ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
#   Aquí usaré Regresión logística, 
#              Naive Bayes, 
#              Árbol de decisión, 
#              Máquina de vectores de soporte (SVM) y 
#              Análisis discriminante lineal (LDA)
#   \para predecir los resultados de supervivencia:

# ------------------------------------Logistics Regression--------------------------------------------------------------------------------------------------------------------------

# Build the Logistics Regression Classifier:
logit_model = glm(formula = Survived ~ .,
                  data = train,
                  family = "binomial")

# Use the step() function to choose the model with the lowest AIC score:
logit_model = step(logit_model)
summary(logit_model)

# Using the step AIC technique, the final model with the lowest AIC score contains these variables: passenger class, age, their title, female_high_class and family group. Next goes to the prediction step:

# Calculate the predicted survival probability using the test data:
prob_pred_logit = predict(logit_model, type = 'response', newdata = test)
# Create ROC plot to find out the optimal cut-off ratio:
pred_logit = prediction(predictions = prob_pred_logit, labels = test$Survived)
perf_logit = performance(pred_logit, "acc")
plot(perf_logit)

# It seems that 0.6 (60%) is supposed to be the optimal cut-off ratio, which would yield the highest accuracy ratio. Next, classifiy the prediction and check the prediction performance by building the confusion matrix (caret package):
y_pred = factor(ifelse(prob_pred_logit > 0.6, 1, 0), levels = c(0, 1))
confusionMatrix(test$Survived, y_pred)

# The logistics regression model yields 86.03% accuracy rate with cut-off ratio at 0.6.

# Make the ROC plot to visulize the model performance:
roc(test$Survived, prob_pred_logit, plot = T)

# As we can see from the ROC plot and confusion matrix, the logstics regression model did a pretty good job in predicting the survival status for the test data. Next, let's try to make another prediction task using the Naive Bayes Classifier.

# -------------------------------  Naive Bayes -----------------------------------------------------------------------------------

# Construya el clasificador Naive Bayes e imprima su resumen:
nb_model = naiveBayes(Survived ~., data = train)
nb_model
summary(nb_model)

# Make prediction using the classifier:

# Predecir la probabilidad de supervivencia y las etiquetas:
nb_prob_pred = predict(nb_model, test, type = 'raw')
nb_pred = predict(nb_model, test)

# Uso de la matriz de confusión para evaluar el rendimiento del modelo:
confusionMatrix(nb_pred, test$Survived)

# El clasificador Naive Bayes ofrece una precisión del 82,6 %, 
# ligeramente inferior al modelo de regresión logística. A continuación, 
# intentemos predecir usando el clasificador de árboles de decisión.

# -------------------------------- Decision Tree Classifier ------------------------------------------------------------------------------

# Build the decision tree classifier:
set.seed(100)
dt_model = rpart(Survived ~ ., data = train)
rpart.plot(dt_model, extra = 3, fallen.leaves = T)

# Make predictions based on the classifier:

dt_pred = predict(dt_model, newdata = test, type = "class")
confusionMatrix(dt_pred, test$Survived)


# The decision tree classifier gives an accuracy rate of 83.2%, slightly higher than the Naives Bayes model but lower than the logistic regression model.

# --------------------------------  Support Vector Machine (SVM) ----------------------------------------------------------------

# Make the SVM classifier using the Radial Kernal:

# Set seeds and build the model:
set.seed(123)
svm_model = svm(Survived ~ .,
                data = train,
                kernal = "radial",
                scale = TRUE)
summary(svm_model)

# Make predictions using the SVM classifier:

# Make predictions using the SVM model:
svm_pred = predict(svm_model, test)
confusionMatrix(svm_pred, test$Survived)

# The SVM predictor yields an accuracy rate of 87.15%, higher than the Logistic Regression, Naive Bayes and Decision Tree classifier. Last, let's try our prediction using Linear Discriminant Analysis (LDA):

#------------------------------- Linear Discriminant Analysis (LDA) ----------------------------------------------------------------

lda_model = lda(Survived ~ ., data = train)
print(lda_model)

# Make predictions using the test data:
lda_pred = predict(lda_model, newdata = test)
names(lda_pred)
confusionMatrix(lda_pred$class, test$Survived)

# The LDA classifier yields an accuracy rate of 83.8%, which is similar to the performance of the Logistics Regression, Decision Tree, Naive Bayes and Linear Discriminant Analysis (LDA) classifiers. Therefore, based on the prediction performance on the test data, the Support Vector Machine (SVM) did the best job, and I will use the SVM model to make the prediction for the next submission task.

# Create the output prediction file using the SVM classifier:
testing = full[c(892:1309), ]
testing$Survived = NULL
test_prediction = predict(svm_model, testing)
prediction = data.frame(PassengerId = c(892:1309), Survived = test_prediction)
write.csv(prediction, 'Submission.csv', row.names = FALSE)
# 6. Summary
# As a new comer to Kaggle and the data science field, I found it's critical to practise my data analytics skills and "data scientist mind" by applying the knowledge I have learned from academic courses and online materials. Kaggle is definitely a wonderful place for me to boost my ability to solve the real-world problems by cleaning, analyzing and modeling data from diverse industries. As my first tryout, this notebook must include some pieces that need to be improved, please leave any feedback below and upvote if you think my work is useful. Thank you for reading my journey of the Titanic data analysis! :)