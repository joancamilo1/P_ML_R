install.packages('tidyverse')
install.packages('plotly')
install.packages('rpart')
install.packages('rpart.plot')
install.packages('viridis')
install.packages('Metrics')
install.packages('glmnet')
install.packages('formattable')
library(tidyverse)
library(plotly)
library(rpart)
library(rpart.plot)
library(viridis)
library(Metrics)
library(glmnet)
library(formattable)


mypath <- dirname(rstudioapi::getActiveDocumentContext()$path) 
setwd(mypath) 

# cWe have some data out of a HR system, related to employees leaving the company. 
#Let's take a look!

# Data
data <- read_csv("HR_comma_sep.csv")

data %>% 
  head(5e2) %>% 
  formattable() %>% 
  as.datatable


# We can make a few observations about the columns --

# * `satisfaction_level`: Un indicador num�rico, presumiblemente (?) completado por el empleado.
# * `last_evaluacion`: Un indicador num�rico, presumiblemente completado por el gerente del empleado.
# * `number_project`: Un n�mero entero, quiz�s el n�mero de proyectos en los que ha trabajado el empleado. �Es esto algo as� como la construcci�n?
# * `average_monthly_hours`: El n�mero de horas que trabajan (�facturadas?) en el mes. �De media?
# * `time_spend_company`: Un valor entero, quiz�s a�os de servicio.
# * `Work_accident`: parece un valor booleano, probablemente haya tenido o no un accidente.
# * `left`: Parece un valor booleano, se deje o no.
# * `promoted_last_5years`: parece un valor booleano.
# * `ventas`: No estoy seguro de lo que esto significa. parte de la fuerza de ventas?
# * `salario`: parece un puntaje de salario de 3 niveles (`bajo`, `medio`, `alto`)

# Some missing things - 
#   
# * `user_id`: �Qu� pasa si un empleado se va y vuelve? �Est�n all� dos veces?

#   Well, let's investigate some of the columns.

##-------------------- Salary ----------------------

# �Cu�l es la distribuci�n del salario?
data %>% 
  count(salary) %>% 
  formattable(align = 'l')


# Como era de esperar, solo un peque�o n�mero de personas tiene un nivel de salario "alto".
# Es interesante que "medio" y "bajo" tengan aproximadamente el mismo tama�o.

##-------------------- sales --------------------
data %>% 
  count(sales) %>% 
  formattable(align = 'l')


# �Parece que tenemos gente de todos los departamentos! Eso es interesante: 
# no creo que `number_projects` realmente se aplique a, por ejemplo, **TI** o **contabilidad**.# 
# It also looks like **sales** is the largest category, which makes sense. 
 
# ---------- �C�mo interact�an `ventas` y `salario`? --------------------
data %>% 
  count(sales, salary) %>%  # contamos el numero de trabajadores en cada uno de los rangos de salario
  mutate( 
    salary = ordered(salary, c("low", "medium", "high")), # separamos por "low", "medium", "high"
    n = comma(n, 0)) %>%  # comma permite ver de una mejor forma los valores numericos
  spread(salary, n) %>%  # es equivalente a df %>% pivot_wider(names_from = key, values_from = value)
  formattable(list(area(T, 2:4) ~ color_tile("white", "orange")), align = 'l')
# Los marcos de datos formateables son marcos de datos que se representan como una tabla HTML


# Algunas cosas bastante obvias aqu�.

# * **management** tiende a tener salarios m�s altos.
# * Muy pocas personas en **sales** tienen salarios altos.
# * Hay _muchas_ personas con salarios m�s bajos en **support**, **sales**, and **technical**.

#  --------- Veamos el mismo gr�fico por porcentaje -------------------
data %>% 
  count(sales, salary) %>% 
  group_by(sales) %>% 
  mutate(
    salary = ordered(salary, c("low", "medium", "high")),
    n = percent(n, 0),  # damos formato de porcentaje
    n = n / sum(n)      # calculamos el porcentaje 
  ) %>% 
  spread(salary, n) %>%  # es equivalente a pivot_wider
  formattable(list(area(T, 2:4) ~ color_tile("white", "orange")), align = 'l')
# Los marcos de datos formateables son marcos de datos que se representan como una tabla HTML


# Eso hace que la distribuci�n de **administraci�n** sea realmente obvia, 
# y tambi�n se�ala que la segunda m�s alta es **contabilidad** 
# (lo cual no era obvio para m� en el gr�fico precioso).

# Tambi�n parece que mientras hay _mucha_ gente en algunos de los otros departamentos
# con salarios bajos/medios; como porcentaje de cada uno, todos son aproximadamente iguales. 
# Si quisi�ramos hacerlo realmente obvio, podr�amos indexar. Pero dada la peque�a variaci�n, no hay una necesidad real.

## Promovido
#  ------------------- Ahora veamos `promotion_last_5years`.  -------------------
data %>% 
  count(promotion_last_5years) %>% 
  formattable(align = 'l')

#"no muchas" personas fueron promovidas.

data %>% 
  count(promotion_last_5years, salary) %>%   # recuento de las personas promovidas
  group_by(salary) %>%                       # agrupamos por tipo de salario
  mutate(n = n / sum(n), n = percent(n)) %>% # calculamos el porcentaje
  spread(promotion_last_5years, n) %>%       # es equivalente a pivot_wider
  formattable(align = 'l')                   # formato de tabla html

# No sorprende notar que, de las personas que ganan un salario alto, 
#  casi el 6% de ellas fueron promovidas en los �ltimos 5 a�os.


## ---------- Tiempo en la compania  -------------------
data %>% 
  count(time_spend_company) %>% 
  ggplot(aes(time_spend_company, n)) +
  geom_line() ->
  g
ggplotly(g, width = 900)


# Siempre encuentro interesantes los picos: uno grande a los tres a�os y luego 
# una ca�da importante a partir del cuarto a�o.

# Es interesante que no est�n contando a las personas que han estado all� 
# por menos de dos a�os. �De qu� trata eso? �Hay demasiada rotaci�n?

# ------------ �C�mo se relaciona eso con el salario?  -------------------
data %>% 
  mutate(salary = ordered(salary, c("low", "medium", "high"))) %>% 
  count(time_spend_company, salary) %>% 
  group_by(time_spend_company) %>% 
  mutate(n = percent(n / sum(n))) %>% 
  ggplot(aes(time_spend_company, n)) +
  geom_area(aes(fill = salary)) ->
  g

ggplotly(g, width = 900)


# Idealmente, me gustar�a hacer una historia como "el salario es bastante estable 
# hasta alrededor de 5 o 6 a�os, y luego las personas que han estado aqu� por m�s tiempo
#  tienden a ganar m�s dinero". Pero no se parece a eso: tenga en cuenta 
#  el aumento en los niveles de salarios **bajos** a los ~8 a�os.

# A�n as�, la _proporci�n_ de personas con diferentes niveles salariales sigue
# aproximadamente esa distribuci�n.

# Tenga en cuenta que hay un n�mero decente de personas con salarios **altos** 
# con < 3 a�os de servicio. Apuesto a que hay un grupo de 
# gerentes/especialistas que se elevan en el nivel superior, luego solo 
# se quedan por unos pocos a�os. Las personas que permanecen pueden tener
# menos opciones, lo que explicar�a por qu� hay menos personas 
# con salarios **altos** en el a�o 5 m�s o menos.


# ------------------- Horas Mensuales -------------------
# Echemos un vistazo a las horas mensuales de todos los trabajadores
data %>% 
  ggplot(aes(average_montly_hours)) +
  geom_density(fill = "orange") ->
  g

ggplotly(g, width = 500)


# Es interesante que tengamos lo que parece ser una distribuci�n bimodal: 
# un pico en ~ 150 horas (una semana laboral de aproximadamente 40 horas) 
# y otro en 260 (una semana laboral loca de 65 horas).


# desglozamos por departamento y por nivel de salario -------------------
data %>% 
  mutate(salary = ordered(salary, c("low", "medium", "high"))) %>% 
  ggplot(aes(average_montly_hours)) +
  geom_density(fill = "orange") +
  facet_grid(sales ~ salary) -> 
  g

ggplotly(g, height = 1e3, width = 900)


# Sinceramente, estoy sorprendido de no ver m�s variaci�n. Sin embargo, 
# hay algunas cosas interesantes que podemos se�alar.

# * Casi todas las distribuciones siguen siendo bimodales. 
# �Hay alg�n otro factor en juego?
# * En **marketing** parece que las personas con salarios altos se toman
#  un descanso. Es m�s com�n trabajar menos horas.
# * En **soporte**, aparentemente se requiere trabajar m�s tiempo.
# * Parece que **hr** las personas con un salario medio no trabajan tanto
# como las personas con salarios altos o bajos.


# ------------------- N�mero de proyectos -------------------
# Veamos en cu�ntos proyectos trabaja la gente.
data %>% 
  count(number_project, sales) %>% 
  spread(number_project, n) %>% # es equivalente a pivot_wider
  formattable(align = 'l')


# Sinceramente, no estoy seguro de qu� significa esta variable 
# (n�mero de proyectos). �Significa p. clientes que tocan? 
# �O es "ID del proyecto" - hay seis proyectos? Eso es factible, supongo.

# Veamos el desglose porcentual.-------------------
data %>% 
  count(number_project, sales) %>% 
  group_by(number_project) %>% 
  mutate(n = percent(n / sum(n), 0)) %>% 
  spread(number_project, n) %>% # es equivalente a pivot_wider
  formattable(align = 'l')
 

# Hmm -- es relativamente similar en todos los proyectos.
# �Eso sugiere que no es "n�mero de proyectos" sino "ID del proyecto
# al que est�n asignados?" No necesariamente. Pero, 
# �por qu� no hay `number_project` == 1?

# La documentaci�n dice que es _N�mero de proyectos_, as� que lo dejaremos ah� por ahora.

data %>% 
  count(number_project, left) %>% 
  spread(left, n) %>%  # es equivalente a pivot_wider
  formattable(align = 'l')


# Es un poco sospechoso que las personas con `number_project == 7`
# hayan dejado la empresa...



#--------------- Satisfacci�n -------------------
data %>% 
  mutate(left = factor(left, labels = c("permanecen", "renunciaron"))) %>% 
  ggplot(aes(satisfaction_level)) +
  geom_density(fill = "orange") +
  facet_wrap( ~ left, ncol = 2) -> 
  g

ggplotly(g, width = 900)


# Bueno, eso est� bastante claro. Las personas con poca satisfacci�n rara vez
# se quedan en la empresa, y yo considerar�a que las personas con una
# satisfacci�n <0,5 est�n maduras para irse, y las que se van se dividen en
# tres categor�as:

# 1. Los "haters" con satisfacci�n < 0,2
# 2. Los "insatisfechos" con satisfacci�n < 0,5
# 3. El "bastante bien", con satisfacci�n > 0,6

#---------------  Calificaci�n -------------------
data %>% 
  mutate(left = factor(left, labels = c("permanecen", "renunciaron"))) %>% 
  ggplot(aes(last_evaluation)) +
  geom_density(fill = "orange") +
  facet_wrap( ~ left, ncol = 2) -> 
  g

ggplotly(g, width = 900)

# Quiero decir, eso es bastante asombroso. Sugiere que las personas que se van son:

# 1. Bastante mal en su trabajo. Tenga en cuenta que el extremo inferior de la escala es 0,4.
# 2. Excelente en su trabajo, probablemente se vaya por una promoci�n.

# Podemos ver tanto la satisfacci�n como la  calificacion -------------------
data %>% 
  mutate(left = factor(left, labels = c("permanecen", "renunciaron"))) %>% 
  ggplot(aes(last_evaluation, satisfaction_level)) +
  geom_point(alpha = 0.05, color = "orange") +
  facet_wrap( ~ left, ncol = 2) -> 
  g

# Use WebGL for speed
ggplotly(g) %>% 
  toWebGL()


# esa es una distribuci�n bastante ajustada para **renunciaron**. Loco apretado.

# ------------------- Leaving -----------------------------------------------------------------------------------------------

# Ahora veamos por qu� la gente se fue. Ya que estamos interesados 
# en _comprender_, podemos lograr esto de dos maneras: 
# modelos lineales
# o �rboles de decisi�n. 
# Seg�n algunos de los otros an�lisis realizados, parece que los bosques aleatorios ganan.

# -------------------�Probemos un solo �rbol de decisi�n! --------------------------------------
# Primero nos separaremos en prueba y entrenamiento.
n <- nrow(data)
idx <- sample(n, n * .66) # 66% del total de datos

# Make a few modications
data %>% 
  mutate(
    left = factor(left, labels = c("Remain", "Left")),
    salary = ordered(salary, c("low", "medium", "high"))
  ) -> 
  d

train <- d[idx, ] # tomamos el 66% del total de datos para entrenar
test <- d[-idx, ] # lo restante para validar


# Luego entrenaremos un solo �rbol de decisi�n usando `rpart`  -------------------
# y evaluaremos qu� tan bueno es nuestro ajuste.

tree <- rpart(left ~ ., data = train) # entrenamos

res <- predict(tree, test) # predicciones con datos de testeo

# AUC = probabilidad de que el modelo de ajuste
# obtenga una puntuaci�n m�s alta en una muestra positiva extra�da al azar 
# que en una muestra negativa extra�da al azar.
auc(as.numeric(test$left) - 1, res[, 2])


# Quiero decir, esa es una puntuaci�n AUC bastante incre�ble para un solo �rbol.
# Ech�mosle un vistazo.
rpart.plot(tree, type = 2, fallen.leaves = F, cex = 1, extra = 2)


# Entonces, �qu� podemos observar?

# 1. El nivel de satisfacci�n parece ser la pieza m�s importante. 
# Si est� por encima de 0,46, es mucho m�s probable que se quede 
# (que es lo que observamos anteriormente).

# 2. Si tiene baja satisfacci�n, la cantidad de proyectos se vuelve importante.
# Si est�s en m�s proyectos, es m�s probable que te quedes. 
# Si est� en menos proyectos, �quiz�s vea la escritura en la pared?

# 3. Si est� contento, ha estado en la empresa por menos de 4,5 a�os
# y obtuvo una puntuaci�n superior al 81% en su �ltima evaluaci�n, 
# es muy probable que se vaya. Y parece que el "decisivo" son las 
# horas mensuales sobre 216.


# En breve:
# 1. Si tienes �xito y est�s sobrecargado de trabajo, te vas.
# 2. Si no est�s contento y tienes exceso de trabajo, te vas.
# 3. Si no est�s contento y no tienes suficiente trabajo, te vas.
# 4. Si ha estado en la empresa durante m�s de 6,5 a�os, 
#    es m�s probable que se sienta feliz trabajando m�s horas.

# Limitemos las variables que entran en el modelo a ---------------------------------------------------------
# `satisfaction_level`, 
# `last_evaluatoin` y 
# `average_monthly_hours`.

tree <- rpart(left ~ satisfaction_level + last_evaluation + average_montly_hours, data = train)

res <- predict(tree, test)

auc(as.numeric(test$left) - 1, res[, 2])


# Still an excellent AUC score.

rpart.plot(tree, type = 2, fallen.leaves = F, cex = 1, extra = 2)


# Vemos m�s o menos la misma historia: 
# si eres bueno y est�s sobrecargado de trabajo, te vas; 
# si no est�s contento, tiendes a irte, especialmente si no tienes suficiente trabajo.

# ----------------- Promoci�n---------------------------------------------------
# �Qu� tal si la gente obtiene un ascenso?

# Repitamos nuestra divisi�n de datos.
n <- nrow(data)
idx <- sample(n, n * .66)

# Make a few modications
data %>% 
  mutate(
    left = factor(left, labels = c("Remain", "Left")),
    salary = ordered(salary, c("low", "medium", "high")),
    # nos enfocamos en los que fueron o no ascendidos
    promotion_last_5years = factor(promotion_last_5years, labels = c("Not Promoted", "Promoted"))
  ) -> 
  d

train <- d[idx, ]
test <- d[-idx, ]


# Now we can train and examine.-----------------

tree <- rpart(promotion_last_5years ~ ., data = train)

res <- predict(tree, test)

auc(as.numeric(test$promotion_last_5years) - 1, res[, 2])


# Huh: eso es horrible. �Realmente tenemos una capacidad tan limitada para 
# predecir una promoci�n?

rpart.plot(tree, type = 2, fallen.leaves = F, cex = 1, extra = 2)


# Entonces... esto sugiere que la mejor medida de "haber sido ascendido" 
# es "llevar un tiempo en la empresa". Interesante.

# Sin embargo, con un puntaje de AUC tan bajo, obviamente no es confiable.



# ----------------- regresi�n log�stica -------------------------------------------------------------------------------------
# Podemos intentar ver qu� caracter�sticas importan con una regresi�n log�stica--------------------------------------------------------------------
# penalizada con lazo. Echemos un vistazo r�pido.

X = model.matrix(promotion_last_5years ~ 0 + ., data) 
y = data$promotion_last_5years

lasso <- glmnet(X, y)


# All we're really interested in is the coefficient chart (from e.g. `plot(lasso)`). But I want the coefficient label and value to be available on hover, so alas we must deign to manipulate the `glmnet` object directly.


c = as.data.frame(as.matrix(coef(lasso)))
c %>% 
  mutate(var = rownames(c)) %>% 
  gather(lambda, estimate, -var) %>% 
  filter(var != "(Intercept)") %>% 
  group_by(lambda) %>% 
  mutate(l1_norm = sum(abs(estimate))) %>% 
  ggplot(aes(l1_norm, estimate)) +
  geom_line(aes(color = var)) +
  geom_hline(yintercept = 0) +
  theme(legend.position = "null") ->
  g

ggplotly(g, width = 900) %>%
  layout(showlegend = FALSE)


# Entonces, en el caso del modelo lineal, obtenemos una conclusi�n ligeramente diferente.
#
# 1. El mejor indicador de "haber sido ascendido" es "estar en la gerencia". Sorprendente, lo s�, aunque tiene sentido si las personas son promovidas _de_ un rol de CI _a_ la gerencia. Por lo tanto, las promociones podr�an indicar principalmente responsabilidad de gesti�n.
# 2. El siguiente mejor indicador es "salario bajo". Tiene sentido que si le pagan mal, podr�a obtener un ascenso.
# 3. Entonces hay muchas cosas que importan. Como era de esperar, si deja la empresa no puede ser promovido.

# Conclusi�n

# En general, dir�a que:
#
# * Averiguar por qu� la gente se va es bastante f�cil.
# * Identificar promociones parece muy dif�cil.
# * Recortar el salario en "alto", "medio" y "bajo" no es algo que me guste mucho.
#
# Adem�s, disfrut� mucho usando `plotly` para graficar aqu�. La conversi�n de `ggplot2` es sorprendentemente buena. Adem�s, el m�todo `toWebGL` (del que acabo de enterarme) hace factible trazar miles de puntos sin un rendimiento horrible.