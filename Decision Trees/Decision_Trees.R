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

# * `satisfaction_level`: Un indicador numérico, presumiblemente (?) completado por el empleado.
# * `last_evaluacion`: Un indicador numérico, presumiblemente completado por el gerente del empleado.
# * `number_project`: Un número entero, quizás el número de proyectos en los que ha trabajado el empleado. ¿Es esto algo así como la construcción?
# * `average_monthly_hours`: El número de horas que trabajan (¿facturadas?) en el mes. ¿De media?
# * `time_spend_company`: Un valor entero, quizás años de servicio.
# * `Work_accident`: parece un valor booleano, probablemente haya tenido o no un accidente.
# * `left`: Parece un valor booleano, se deje o no.
# * `promoted_last_5years`: parece un valor booleano.
# * `ventas`: No estoy seguro de lo que esto significa. parte de la fuerza de ventas?
# * `salario`: parece un puntaje de salario de 3 niveles (`bajo`, `medio`, `alto`)

# Some missing things - 
#   
# * `user_id`: ¿Qué pasa si un empleado se va y vuelve? ¿Están allí dos veces?

#   Well, let's investigate some of the columns.

##-------------------- Salary ----------------------

# ¿Cuál es la distribución del salario?
data %>% 
  count(salary) %>% 
  formattable(align = 'l')


# Como era de esperar, solo un pequeño número de personas tiene un nivel de salario "alto".
# Es interesante que "medio" y "bajo" tengan aproximadamente el mismo tamaño.

##-------------------- sales --------------------
data %>% 
  count(sales) %>% 
  formattable(align = 'l')


# ¡Parece que tenemos gente de todos los departamentos! Eso es interesante: 
# no creo que `number_projects` realmente se aplique a, por ejemplo, **TI** o **contabilidad**.# 
# It also looks like **sales** is the largest category, which makes sense. 
 
# ---------- ¿Cómo interactúan `ventas` y `salario`? --------------------
data %>% 
  count(sales, salary) %>%  # contamos el numero de trabajadores en cada uno de los rangos de salario
  mutate( 
    salary = ordered(salary, c("low", "medium", "high")), # separamos por "low", "medium", "high"
    n = comma(n, 0)) %>%  # comma permite ver de una mejor forma los valores numericos
  spread(salary, n) %>%  # es equivalente a df %>% pivot_wider(names_from = key, values_from = value)
  formattable(list(area(T, 2:4) ~ color_tile("white", "orange")), align = 'l')
# Los marcos de datos formateables son marcos de datos que se representan como una tabla HTML


# Algunas cosas bastante obvias aquí.

# * **management** tiende a tener salarios más altos.
# * Muy pocas personas en **sales** tienen salarios altos.
# * Hay _muchas_ personas con salarios más bajos en **support**, **sales**, and **technical**.

#  --------- Veamos el mismo gráfico por porcentaje -------------------
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


# Eso hace que la distribución de **administración** sea realmente obvia, 
# y también señala que la segunda más alta es **contabilidad** 
# (lo cual no era obvio para mí en el gráfico precioso).

# También parece que mientras hay _mucha_ gente en algunos de los otros departamentos
# con salarios bajos/medios; como porcentaje de cada uno, todos son aproximadamente iguales. 
# Si quisiéramos hacerlo realmente obvio, podríamos indexar. Pero dada la pequeña variación, no hay una necesidad real.

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
#  casi el 6% de ellas fueron promovidas en los últimos 5 años.


## ---------- Tiempo en la compania  -------------------
data %>% 
  count(time_spend_company) %>% 
  ggplot(aes(time_spend_company, n)) +
  geom_line() ->
  g
ggplotly(g, width = 900)


# Siempre encuentro interesantes los picos: uno grande a los tres años y luego 
# una caída importante a partir del cuarto año.

# Es interesante que no estén contando a las personas que han estado allí 
# por menos de dos años. ¿De qué trata eso? ¿Hay demasiada rotación?

# ------------ ¿Cómo se relaciona eso con el salario?  -------------------
data %>% 
  mutate(salary = ordered(salary, c("low", "medium", "high"))) %>% 
  count(time_spend_company, salary) %>% 
  group_by(time_spend_company) %>% 
  mutate(n = percent(n / sum(n))) %>% 
  ggplot(aes(time_spend_company, n)) +
  geom_area(aes(fill = salary)) ->
  g

ggplotly(g, width = 900)


# Idealmente, me gustaría hacer una historia como "el salario es bastante estable 
# hasta alrededor de 5 o 6 años, y luego las personas que han estado aquí por más tiempo
#  tienden a ganar más dinero". Pero no se parece a eso: tenga en cuenta 
#  el aumento en los niveles de salarios **bajos** a los ~8 años.

# Aún así, la _proporción_ de personas con diferentes niveles salariales sigue
# aproximadamente esa distribución.

# Tenga en cuenta que hay un número decente de personas con salarios **altos** 
# con < 3 años de servicio. Apuesto a que hay un grupo de 
# gerentes/especialistas que se elevan en el nivel superior, luego solo 
# se quedan por unos pocos años. Las personas que permanecen pueden tener
# menos opciones, lo que explicaría por qué hay menos personas 
# con salarios **altos** en el año 5 más o menos.


# ------------------- Horas Mensuales -------------------
# Echemos un vistazo a las horas mensuales de todos los trabajadores
data %>% 
  ggplot(aes(average_montly_hours)) +
  geom_density(fill = "orange") ->
  g

ggplotly(g, width = 500)


# Es interesante que tengamos lo que parece ser una distribución bimodal: 
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


# Sinceramente, estoy sorprendido de no ver más variación. Sin embargo, 
# hay algunas cosas interesantes que podemos señalar.

# * Casi todas las distribuciones siguen siendo bimodales. 
# ¿Hay algún otro factor en juego?
# * En **marketing** parece que las personas con salarios altos se toman
#  un descanso. Es más común trabajar menos horas.
# * En **soporte**, aparentemente se requiere trabajar más tiempo.
# * Parece que **hr** las personas con un salario medio no trabajan tanto
# como las personas con salarios altos o bajos.


# ------------------- Número de proyectos -------------------
# Veamos en cuántos proyectos trabaja la gente.
data %>% 
  count(number_project, sales) %>% 
  spread(number_project, n) %>% # es equivalente a pivot_wider
  formattable(align = 'l')


# Sinceramente, no estoy seguro de qué significa esta variable 
# (número de proyectos). ¿Significa p. clientes que tocan? 
# ¿O es "ID del proyecto" - hay seis proyectos? Eso es factible, supongo.

# Veamos el desglose porcentual.-------------------
data %>% 
  count(number_project, sales) %>% 
  group_by(number_project) %>% 
  mutate(n = percent(n / sum(n), 0)) %>% 
  spread(number_project, n) %>% # es equivalente a pivot_wider
  formattable(align = 'l')
 

# Hmm -- es relativamente similar en todos los proyectos.
# ¿Eso sugiere que no es "número de proyectos" sino "ID del proyecto
# al que están asignados?" No necesariamente. Pero, 
# ¿por qué no hay `number_project` == 1?

# La documentación dice que es _Número de proyectos_, así que lo dejaremos ahí por ahora.

data %>% 
  count(number_project, left) %>% 
  spread(left, n) %>%  # es equivalente a pivot_wider
  formattable(align = 'l')


# Es un poco sospechoso que las personas con `number_project == 7`
# hayan dejado la empresa...



#--------------- Satisfacción -------------------
data %>% 
  mutate(left = factor(left, labels = c("permanecen", "renunciaron"))) %>% 
  ggplot(aes(satisfaction_level)) +
  geom_density(fill = "orange") +
  facet_wrap( ~ left, ncol = 2) -> 
  g

ggplotly(g, width = 900)


# Bueno, eso está bastante claro. Las personas con poca satisfacción rara vez
# se quedan en la empresa, y yo consideraría que las personas con una
# satisfacción <0,5 están maduras para irse, y las que se van se dividen en
# tres categorías:

# 1. Los "haters" con satisfacción < 0,2
# 2. Los "insatisfechos" con satisfacción < 0,5
# 3. El "bastante bien", con satisfacción > 0,6

#---------------  Calificación -------------------
data %>% 
  mutate(left = factor(left, labels = c("permanecen", "renunciaron"))) %>% 
  ggplot(aes(last_evaluation)) +
  geom_density(fill = "orange") +
  facet_wrap( ~ left, ncol = 2) -> 
  g

ggplotly(g, width = 900)

# Quiero decir, eso es bastante asombroso. Sugiere que las personas que se van son:

# 1. Bastante mal en su trabajo. Tenga en cuenta que el extremo inferior de la escala es 0,4.
# 2. Excelente en su trabajo, probablemente se vaya por una promoción.

# Podemos ver tanto la satisfacción como la  calificacion -------------------
data %>% 
  mutate(left = factor(left, labels = c("permanecen", "renunciaron"))) %>% 
  ggplot(aes(last_evaluation, satisfaction_level)) +
  geom_point(alpha = 0.05, color = "orange") +
  facet_wrap( ~ left, ncol = 2) -> 
  g

# Use WebGL for speed
ggplotly(g) %>% 
  toWebGL()


# esa es una distribución bastante ajustada para **renunciaron**. Loco apretado.

# ------------------- Leaving -----------------------------------------------------------------------------------------------

# Ahora veamos por qué la gente se fue. Ya que estamos interesados 
# en _comprender_, podemos lograr esto de dos maneras: 
# modelos lineales
# o árboles de decisión. 
# Según algunos de los otros análisis realizados, parece que los bosques aleatorios ganan.

# -------------------¡Probemos un solo árbol de decisión! --------------------------------------
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


# Luego entrenaremos un solo árbol de decisión usando `rpart`  -------------------
# y evaluaremos qué tan bueno es nuestro ajuste.

tree <- rpart(left ~ ., data = train) # entrenamos

res <- predict(tree, test) # predicciones con datos de testeo

# AUC = probabilidad de que el modelo de ajuste
# obtenga una puntuación más alta en una muestra positiva extraída al azar 
# que en una muestra negativa extraída al azar.
auc(as.numeric(test$left) - 1, res[, 2])


# Quiero decir, esa es una puntuación AUC bastante increíble para un solo árbol.
# Echémosle un vistazo.
rpart.plot(tree, type = 2, fallen.leaves = F, cex = 1, extra = 2)


# Entonces, ¿qué podemos observar?

# 1. El nivel de satisfacción parece ser la pieza más importante. 
# Si está por encima de 0,46, es mucho más probable que se quede 
# (que es lo que observamos anteriormente).

# 2. Si tiene baja satisfacción, la cantidad de proyectos se vuelve importante.
# Si estás en más proyectos, es más probable que te quedes. 
# Si está en menos proyectos, ¿quizás vea la escritura en la pared?

# 3. Si está contento, ha estado en la empresa por menos de 4,5 años
# y obtuvo una puntuación superior al 81% en su última evaluación, 
# es muy probable que se vaya. Y parece que el "decisivo" son las 
# horas mensuales sobre 216.


# En breve:
# 1. Si tienes éxito y estás sobrecargado de trabajo, te vas.
# 2. Si no estás contento y tienes exceso de trabajo, te vas.
# 3. Si no estás contento y no tienes suficiente trabajo, te vas.
# 4. Si ha estado en la empresa durante más de 6,5 años, 
#    es más probable que se sienta feliz trabajando más horas.

# Limitemos las variables que entran en el modelo a ---------------------------------------------------------
# `satisfaction_level`, 
# `last_evaluatoin` y 
# `average_monthly_hours`.

tree <- rpart(left ~ satisfaction_level + last_evaluation + average_montly_hours, data = train)

res <- predict(tree, test)

auc(as.numeric(test$left) - 1, res[, 2])


# Still an excellent AUC score.

rpart.plot(tree, type = 2, fallen.leaves = F, cex = 1, extra = 2)


# Vemos más o menos la misma historia: 
# si eres bueno y estás sobrecargado de trabajo, te vas; 
# si no estás contento, tiendes a irte, especialmente si no tienes suficiente trabajo.

# ----------------- Promoción---------------------------------------------------
# ¿Qué tal si la gente obtiene un ascenso?

# Repitamos nuestra división de datos.
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


# Huh: eso es horrible. ¿Realmente tenemos una capacidad tan limitada para 
# predecir una promoción?

rpart.plot(tree, type = 2, fallen.leaves = F, cex = 1, extra = 2)


# Entonces... esto sugiere que la mejor medida de "haber sido ascendido" 
# es "llevar un tiempo en la empresa". Interesante.

# Sin embargo, con un puntaje de AUC tan bajo, obviamente no es confiable.



# ----------------- regresión logística -------------------------------------------------------------------------------------
# Podemos intentar ver qué características importan con una regresión logística--------------------------------------------------------------------
# penalizada con lazo. Echemos un vistazo rápido.

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


# Entonces, en el caso del modelo lineal, obtenemos una conclusión ligeramente diferente.
#
# 1. El mejor indicador de "haber sido ascendido" es "estar en la gerencia". Sorprendente, lo sé, aunque tiene sentido si las personas son promovidas _de_ un rol de CI _a_ la gerencia. Por lo tanto, las promociones podrían indicar principalmente responsabilidad de gestión.
# 2. El siguiente mejor indicador es "salario bajo". Tiene sentido que si le pagan mal, podría obtener un ascenso.
# 3. Entonces hay muchas cosas que importan. Como era de esperar, si deja la empresa no puede ser promovido.

# Conclusión

# En general, diría que:
#
# * Averiguar por qué la gente se va es bastante fácil.
# * Identificar promociones parece muy difícil.
# * Recortar el salario en "alto", "medio" y "bajo" no es algo que me guste mucho.
#
# Además, disfruté mucho usando `plotly` para graficar aquí. La conversión de `ggplot2` es sorprendentemente buena. Además, el método `toWebGL` (del que acabo de enterarme) hace factible trazar miles de puntos sin un rendimiento horrible.