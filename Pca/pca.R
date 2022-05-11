## Introduction

# The aim of this analysis is to find interensting relations between the academic performance and the private life of the students. It is structured in three section.
# 
# 1. Data exploration
# 2. Principal component analysis
# 3. Prediction

### 1. Load

# Let's load both datasets and merge them.

install.packages('knitr')
install.packages('pander')
install.packages('plotly')
install.packages('plyr')
install.packages('magrittr')
install.packages('readr')
install.packages('rpart')
install.packages('DMwR')
install.packages('dplyr')
install.packages('corrplot')
install.packages('randomForest')
library(knitr)
library(pander)
library(plotly)
library(plyr)
library(magrittr)
library(readr)
library(rpart)
library(DMwR)
library(dplyr)
library(corrplot)
library(randomForest)

options(digits=2)




student_mat <- read_csv("student-mat.csv")
student_mat$class <- "Mat"
student_por<-read_csv("student-por.csv")
student_por$class <- "Por"

students=rbind(student_mat,student_por)

students<-students %>% distinct(school,sex,age,address,famsize,Pstatus,
                                       Medu,Fedu,Mjob,Fjob,reason,
                                       guardian,traveltime,studytime,failures,
                                       schoolsup, famsup,activities,nursery,higher,internet,
                                       romantic,famrel,freetime,goout,Dalc,Walc,health,absences, .keep_all = TRUE)



# Here I introduce two new attribute that will be used later: **Gavg** and **Gintv**: the former is the average of the three scores, the latter is a discrete variable that defines three main score slots: `[0,10]`, `[11,16]` and `[17,20]`.


students$Gintv<-cut(students$G3, c(-1,10,16,20))
students$Gavg<-(students$G3+students$G2+students$G1)/3



  students_factorized<-students
  
  students_factorized$class<-as.numeric(factor(students_factorized$class,labels=c(-1,1)))
  students_factorized$famsize<-as.numeric(factor(students_factorized$famsize,labels=c(4,3)))
  students_factorized$Pstatus<-as.numeric(factor(students_factorized$Pstatus,labels=c(-1,1)))
  students_factorized$address<-as.numeric(factor(students_factorized$address,labels=c(-1,1)))
  students_factorized$sex<-as.numeric(factor(students_factorized$sex,labels=c(-1,1)))
  students_factorized$school<-as.numeric(factor(students_factorized$school,labels=c(2,1)))
  students_factorized$schoolsup<-as.numeric(factor(students_factorized$schoolsup, labels=c(0,1)))
  students_factorized$famsup<-as.numeric(factor(students_factorized$famsup, labels=c(0,1)))
  students_factorized$paid<-as.numeric(factor(students_factorized$paid, labels=c(0,1)))
  students_factorized$activities<-as.numeric(factor(students_factorized$activities, labels=c(0,1)))
  students_factorized$nursery<-as.numeric(factor(students_factorized$nursery, labels=c(0,1)))
  students_factorized$higher<-as.numeric(factor(students_factorized$higher, labels=c(0,1)))
  students_factorized$internet<-as.numeric(factor(students_factorized$internet, labels=c(0,1)))
  students_factorized$romantic<-as.numeric(factor(students_factorized$romantic, labels=c(0,1)))
  students_factorized$guardian<-as.numeric(factor(students_factorized$guardian))
  students_factorized$Mjob<-as.numeric(factor(students_factorized$Mjob))
  students_factorized$Fjob<-as.numeric(factor(students_factorized$Fjob))
  students_factorized$reason<-as.numeric(factor(students_factorized$reason))
  students_factorized$Gintv<-as.numeric(factor(students_factorized$Gintv))
  students_factorized$class<-as.numeric(factor(students_factorized$class, labels=c(-1,1)))
  
  students_factorized <- subset(students_factorized,select=-Gavg)

  
  ### 2. Exploration
  
  # In this first exploratory phase I use correlation matrix to find the main relations between attributes. 



M1<-cor(data.frame(model.matrix( ~.-1, data=subset(students_factorized, select=-c(reason,guardian,Fjob,Mjob)))))

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(M1, method="color", col=col(200),
         type="upper",order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation,
         
         tl.col="black", tl.srt=45, #Text label color and rotation
         tl.cex = 0.5,
         number.cex = 0.5,
         cl.cex = 0.5,
         # Combine with significance
         # hide correlation coefficient on the principal diagonal
         diag=FALSE
         
)


# As we can imagine there's a strong positive correlation among scores (G1,G2 and G3). Let's get into this.


linear_model<-lm( G3 ~ G2 + G1, data=students);



summ=summary(linear_model)

summ$sigma<-round(summ$sigma, digits=2)
summ$r.squared<-round(summ$r.squared, digits=2)
summ$adj.r.squared<-round(summ$adj.r.squared, digits=2)
f.statistic.value=round(as.numeric(summ$fstatistic[1]), digits=2)
f.statistic.numdf=round(as.numeric(summ$fstatistic[2]))
f.statistic.dendf=round(as.numeric(summ$fstatistic[3]))



pander(linear_model)


# All the outputs confirm that the relation between scores is well fitted by a linear model. In particular the F-Statisic is `r f.statistic.value`, that is greater then 1 and suggests us to refuse the null-hypothesis. Furthermore the R^2, that is a value between 0 and 1 that represents the proportion of variance explained, is `r summ$r.squared`, quite good.




# linear model : G3 = G2 + G1

plot_ly(students, x = ~G1, y = ~G2, z = ~G3, 
        marker = list(color = ~Dalc, colorscale = c('#FFE1A1', '#683531'),size = 3, showscale = TRUE)) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'G1'),
                      yaxis = list(title = 'G2'),
                      zaxis = list(title = 'G3')),
         annotations = list(
           x = 1.13,
           y = 1.05,
           text = 'Workday alcohol consumption',
           xref = 'paper',
           yref = 'paper',
           showarrow = FALSE
         ))



# The color in the previous graph represents the student's workday alcohol consumption, and we can start observing that students with high alcohol level tends to reach scores lower than 16.

# The correlation matrix seems not to highlight a strong correlation between alcohol consumption and scores. Let's deepen this.


plot_ly(data=students, x = ~Dalc, y = ~Gavg, type = "box", name="Dalc", color = 'Dalc') %>%
  add_trace(data=students, x = ~Walc, y = ~Gavg,  type = "box", name="Walc", color = 'Walc') %>%
  layout(boxmode = "group", xaxis=list(title="Alcohol level"))



freq=table(students$G3)
xdalc=array()
xwalc=array()
yD <- data.frame( x1=c(1:21),x2=c(1:21),x3=c(1:21),x4=c(1:21),x5=c(1:21) )
yW <- yD
for ( i in c(1:5)){
  
  xDalc <- table(students[students$Dalc==i,]$G3)
  xWalc <- table(students[students$Walc==i,]$G3)
  
  percD = array()
  percW = array()
  
  for ( j in c(1:21)){
    
    key=as.character(j-1)
    percD[j] <- xDalc[key]/freq[key]*100
    percW[j] <- xWalc[key]/freq[key]*100
    
    if (is.na(percD[j])){
      percD[j]<-0
    }
    if (is.na(percW[j])){
      percW[j]<-0
    }
    
  } 
  yD[i] <- percD
  yW[i] <- percW
  
}

G3=c(0:20)
p1<-plot_ly(yD, x = ~G3, y = ~x1, type = 'bar', name = 'Alcohol=1', marker = list(color = 'rgb(102,194,165)')) %>%
  add_trace(y = ~x2, name = 'Alcohol=2', marker = list(color = 'rgb(252,141,98)')) %>%
  add_trace(y = ~x3, name = 'Alcohol=3', marker = list(color = 'rgb(141,160,203)')) %>%
  add_trace(y = ~x4, name = 'Alcohol=4', marker = list(color = 'rgb(231,138,195)')) %>%
  add_trace(y = ~x5, name = 'Alcohol=5', marker = list(color = 'rgb(166,216,84)')) %>%
  layout(yaxis = list(title = 'Percentage of students'), barmode = 'stack',
         annotations=list(x = 0.35 , y = 1.05, text = "Daily consumption", showarrow = F, xref='paper', yref='paper'))

p2<-plot_ly(yW, x = ~G3, y = ~x1, type = 'bar', marker = list(color = 'rgb(102,194,165)'),showlegend=F) %>%
  add_trace(y = ~x2, marker = list(color = 'rgb(252,141,98)')) %>%
  add_trace(y = ~x3, marker = list(color = 'rgb(141,160,203)')) %>%
  add_trace(y = ~x4, marker = list(color = 'rgb(231,138,195)')) %>%
  add_trace(y = ~x5, marker = list(color = 'rgb(166,216,84)')) %>%
  layout(yaxis = list(title = 'Count'), barmode = 'stack',
         annotations=list(x = 0.75 , y = 1.05, text = "Weekend consumption", showarrow = F, xref='paper', yref='paper'))

subplot(p1,p2,shareY = TRUE, titleX = TRUE)




# There's an important difference between workday and weekend alcohol consumption: as expected students tend to drink more during the weekend, and those with the highest scores have an alcohol level equals or near to zero.

# Let's see now other predictors that mostly explain student's career. 


tb<-as.data.frame(table(Gintv=students$Gintv,failures=students$failures))
  
  for (i in c(0,1,2)){
    
    count<-0
    for(j in c(0,1,2,3)){
      count<- count+tb$Freq[i+3*j+1]
    }
    for(j in c(0,1,2,3)){
      tb$Freq[i+3*j+1] <- tb$Freq[i+3*j+1]/count*100; 
    }
    
  }
  
  colnames(tb)[colnames(tb) == 'Freq'] <- 'Percentage'
  
  tb %>%
    plot_ly(x = ~Gintv, y = ~Percentage, color=~failures, name="Failures") %>%
    add_annotations( text="Failures", xref="paper", yref="paper",
                  x=1.03, xanchor="left",
                  y=1, yanchor="bottom",    # Same y as legend below
                  legendtitle=TRUE, showarrow=FALSE ) %>%
    layout( yaxis=list(title="Percentage of students"), xaxis=list(title="Student's failures"))
  




tb<-as.data.frame(table(Gintv=students$Gintv,higher=students$higher))
t=table(students$Gintv)
tb$Percentage[c(1:6)] <- tb$Freq[c(1:6)]/(t[tb$Gintv[c(1:6)]])*100


plot_ly(data=tb[tb$higher == "yes",], x=~Gintv, y=~Percentage, type = "bar", name="higher=yes", 
        marker = list(color = "#66c2a5")) %>%
add_trace(data=tb[tb$higher == "no",], x=~Gintv, y=~Percentage, type = "bar", name="higher=no",
          marker = list(color = "#fc8d62")) %>%
  layout(barmode = "stack", yaxis=list(title="Percentage of students"))






tb<-as.data.frame(table(Gintv=students$Gintv,class=students$class))
t=table(students$Gintv)
tb$Percentage[c(1:3)] <- tb$Freq[c(1:3)]/sum(tb$Freq[c(1:3)])*100
tb$Percentage[c(4:6)] <- tb$Freq[c(4:6)]/sum(tb$Freq[c(4:6)])*100


plot_ly(data=tb[tb$class == "Por",], x=~Gintv, y=~Percentage, type = "bar", name="class=POR", 
        marker = list(color = "#66c2a5")) %>%
  add_trace(data=tb[tb$class == "Mat",], x=~Gintv, y=~Percentage, type = "bar", name="class=MAT",
            marker = list(color = "#fc8d62")) %>%
  layout(barmode = "group", yaxis=list(title="Percentage of students"))




## PCA

# In this second part I'll exploit the principal component analysis in order to lighten the dataset: the goal is to find few components that can explain the majority of the variance of the whole dataset.

# However, while PCA can handle real attributes and attributes with a natural order we must exclude discrete attributes that don't have a natural order: it is important to notice that since the attributes that match this condition are a lot (I had to exclude 15 attributes), even very good results will not be enough to balance the loss of information. 


tmp<-subset(students_factorized, select=c(age,Medu,Fedu,traveltime,studytime,failures,freetime,goout,Dalc,Walc,health,absences,G1,G2,G3))

pca_set=data.frame(model.matrix( ~.-1, data=tmp)) 
pca = prcomp(pca_set, scale. = T, center = T)



panderOptions("digits", 2)
pander(summary(pca)$importance[,1:15])
pca.var=pca$sdev^2
pve=pca.var/sum(pca.var)
pve=data.frame(x=c(1:length(names(tmp))), y=pve)
p2<-plot_ly(data=pve,x=~x, y=~cumsum(y), type="scatter", name="Cumulative PVE", mode="lines+markers", 
            marker = list(size = 6,color = 'rgba(141,160,203,1)'), 
            line = list(color = 'rgba(141,160,203, .5)')
) %>%
  layout(xaxis = list(title="Number of principal components"))

p1<-plot_ly(data=pve,x=~x, y=~y, type="scatter", name="PVE", mode="lines+markers", 
            marker = list(size = 6, color = 'rgba(102,194,165,1)'),
            line = list(color = 'rgba(102,194,165, .5)')
) %>%
  layout(xaxis = list(title="Number of principal components"))
subplot(p1,p2, shareX = TRUE)


# It's interesting to notice how the first two components correlates `G1,G2,G3`, `Fedu,Medu` and `Walc,Dalc,goout,freetime`.
# However, since a proportion of variance near 80% requires 8 components and given that to perform the PCA I excluded many attributes, I'll continue the analysis with the original dataset, otherwise we would lose a lot of information.

## Predictions

# Let's now try to do some predictions by means of bagging and random forest. The goal is to predict the score interval `Gintv` for each student.


students_factorized$Gintv = factor(as.numeric(students_factorized$Gintv))



smp_size <- floor(0.75 * nrow(students_factorized))
set.seed(123)
train_ind <- sample(seq_len(nrow(students_factorized)), size = smp_size)
test=students_factorized[-train_ind,]$Gintv





m_bagging = length(students_factorized)-2
m_rf = round(sqrt(m_bagging))

# Random forest on train set
rf.students=randomForest(Gintv~.-G3,data=students_factorized, importance =TRUE, subset=train_ind, mtry=m_rf )

# Bagging on train set
bagging.students=randomForest(Gintv~.-G3,data=students_factorized, importance =TRUE, subset=train_ind, mtry=m_bagging)



panderOptions("digits", 2)
rf.imp<-importance(rf.students)
bagging.imp<-importance(bagging.students)
l=dim(rf.imp)[1]
x1<-rf.imp[order(rf.imp[,5],decreasing = TRUE),][,5]
x1<-data.frame(x=names(x1),y=x1)
x2<-bagging.imp[order(bagging.imp[,5],decreasing = TRUE),][,5]
x2<-data.frame(x=names(x2),y=x2)

a <- list(
  title ="Attributes",
  showticklabels = TRUE,
  tickfont = list(size=10),
  exponentformat = "E"
)
b <- list(
  title ="Variable importance", 
  showticklabels = TRUE,
  tickfont = list(size=10),
  exponentformat = "E"
)

p1<- plot_ly(x1, type="bar", x=~y, y=~x, name="Random Forest",marker = list(color = 'rgb(252,141,98)')) %>% 
  layout(xaxis = b, yaxis=list(categoryarray = ~x,categoryorder = "array",tickfont = list(size=10), showticklabels=FALSE))
p2<- plot_ly( x2, type="bar", x=~y, y=~x, name="Bagging",marker = list(color ='rgb(142,160,203)' )) %>%
  layout(xaxis = b, yaxis=list(categoryarray = ~x,categoryorder = "array",tickfont = list(size=10)))

subplot(p1,p2, shareY = FALSE, titleX = TRUE, margin = 0.08 )




pander(rf.students$confusion, caption="Random forest confusion matrix")
pander(bagging.students$confusion, caption="Bagging confusion matrix")



# Test model on test set
yhat.students = predict(rf.students ,students_factorized[-train_ind,])

gintv1=c(0,0,0)
gintv2=c(0,0,0)
gintv3=c(0,0,0)

for (x in c(1:length(test))){
  
  if (test[x]==1)
    gintv1[ yhat.students[x] ]<-gintv1[ yhat.students[x] ]+1
  if (test[x]==2)
    gintv2[yhat.students[x]]<-gintv2[yhat.students[x]]+1
  if (test[x]==3)
    gintv3[yhat.students[x]]<-gintv3[yhat.students[x]]+1
}

gintv=as.data.frame(t(data.frame(gintv1=gintv1, gintv2=gintv2, gintv3=gintv3)))
for(i in c(1:3)){
  gintv[i,]=gintv[i,]/sum(gintv[i,])
}

Gintv=c(1:3)
p1<-plot_ly(gintv,x=~Gintv, y=~V1, name='Gintv=1', type='bar',marker = list(color = 'rgb(102,194,165)')) %>%
  add_trace(y=~V2,name='Gintv=2',marker = list(color = 'rgb(252,141,98)'))%>%
  add_trace(y=~V3,name='Gintv=3',marker = list(color = 'rgb(141,160,203)'))%>%
  layout(barmode = 'stack',annotations=list(x = 0.35 , y = 1.05, text = "Random Forest", showarrow = F, xref='paper', yref='paper'),yaxis=list(
  title = "Gintv",
  showticklabels = TRUE,
  exponentformat = "E"
))

yhat.students = predict(bagging.students ,students_factorized[-train_ind,])

gintv1=c(0,0,0)
gintv2=c(0,0,0)
gintv3=c(0,0,0)

for (x in c(1:length(test))){
  
  if (test[x]==1)
    gintv1[ yhat.students[x] ]<-gintv1[ yhat.students[x] ]+1
  if (test[x]==2)
    gintv2[yhat.students[x]]<-gintv2[yhat.students[x]]+1
  if (test[x]==3)
    gintv3[yhat.students[x]]<-gintv3[yhat.students[x]]+1
} 

gintv=as.data.frame(t(data.frame(gintv1=gintv1, gintv2=gintv2, gintv3=gintv3)))
for(i in c(1:3)){
  gintv[i,]=gintv[i,]/sum(gintv[i,])
}

labels=c(1:3)
p2<-plot_ly(gintv,x=~Gintv, y=~V1, type='bar',marker = list(color = 'rgb(102,194,165)'),showlegend=F) %>%
  add_trace(y=~V2,marker = list(color = 'rgb(252,141,98)'))%>%
  add_trace(y=~V3,marker = list(color = 'rgb(141,160,203)'))%>%
  layout(barmode = 'stack',annotations=list(x = 0.6 , y = 1.05, text = "Bagging", showarrow = F, xref='paper', yref='paper'))

  
subplot(p1,p2,shareY = TRUE, titleX = TRUE)
  


# Results are quite good, we're able to predict well the 90% of students with low scores, the 94% of students with intermediate scores, and the 80% of students with high scores.

# Both bagging and random forest produces the same results, the only difference is that while bagging uses `G2` as the most important predictor, random forest uses both `G1` and `G2`. However the previous graph highlights that all other predictors are ignored by both algorithms.

# Are we able to predict the student's final score even by removing `G2` and `G1`? Let's give it a try.



m_bagging = length(students_factorized)-4
m_rf = round(sqrt(m_bagging))

# Random forest on train set
rf.students=randomForest(Gintv~.-G3-G2-G1,data=students_factorized, importance =TRUE, subset=train_ind, mtry=m_rf )

# Bagging on train set
bagging.students=randomForest(Gintv~.-G3-G2-G1,data=students_factorized, importance =TRUE, subset=train_ind, mtry=m_bagging)



panderOptions("digits", 2)
rf.imp<-importance(rf.students)
bagging.imp<-importance(bagging.students)
l=dim(rf.imp)[1]
x1<-rf.imp[order(rf.imp[,5],decreasing = TRUE),][,5]
x1<-data.frame(x=names(x1),y=x1)
x2<-bagging.imp[order(bagging.imp[,5],decreasing = TRUE),][,5]
x2<-data.frame(x=names(x2),y=x2)

a <- list(
  title ="Attributes",
  showticklabels = TRUE,
  tickfont = list(size=10),
  exponentformat = "E"
)
b <- list(
  title ="Variable importance", 
  showticklabels = TRUE,
  tickfont = list(size=10),
  exponentformat = "E"
)

p1<- plot_ly(x1, type="bar", x=~y, y=~x, name="Random Forest",marker = list(color = 'rgb(252,141,98)')) %>% 
  layout(xaxis = b, yaxis=list(categoryarray = ~x,categoryorder = "array",tickfont = list(size=10),showticklabels=FALSE))
p2<- plot_ly( x2, type="bar", x=~y, y=~x, name="Bagging",marker = list(color ='rgb(142,160,203)' )) %>%
  layout(xaxis = b, yaxis=list(categoryarray = ~x,categoryorder = "array",tickfont = list(size=10)))

subplot(p1,p2, shareY = FALSE, titleX = TRUE, margin = 0.08 )




pander(rf.students$confusion, caption="Random forest confusion matrix")
pander(bagging.students$confusion, caption="Bagging confusion matrix")



# Test model on test set
yhat.students = predict(rf.students ,students_factorized[-train_ind,])

gintv1=c(0,0,0)
gintv2=c(0,0,0)
gintv3=c(0,0,0)

for (x in c(1:length(test))){
  
  if (test[x]==1)
    gintv1[ yhat.students[x] ]<-gintv1[ yhat.students[x] ]+1
  if (test[x]==2)
    gintv2[yhat.students[x]]<-gintv2[yhat.students[x]]+1
  if (test[x]==3)
    gintv3[yhat.students[x]]<-gintv3[yhat.students[x]]+1
  
}

gintv=as.data.frame(t(data.frame(gintv1=gintv1, gintv2=gintv2, gintv3=gintv3)))
for(i in c(1:3)){
  gintv[i,]=gintv[i,]/sum(gintv[i,])
}

Gintv=c(1:3)
p1<-plot_ly(gintv,x=~Gintv, y=~V1, name='Gintv=1', type='bar',marker = list(color = 'rgb(102,194,165)')) %>%
  add_trace(y=~V2,name='Gintv=2',marker = list(color = 'rgb(252,141,98)'))%>%
  add_trace(y=~V3,name='Gintv=3',marker = list(color = 'rgb(141,160,203)'))%>%
  layout(barmode = 'stack',annotations=list(x = 0.35 , y = 1.05, text = "Random Forest", showarrow = F, xref='paper', yref='paper'),yaxis=list(
    title = "Gintv",
    showticklabels = TRUE,
    exponentformat = "E"
  ))

yhat.students = predict(bagging.students ,students_factorized[-train_ind,])

gintv1=c(0,0,0)
gintv2=c(0,0,0)
gintv3=c(0,0,0)

for (x in c(1:length(test))){
  
  if (test[x]==1)
    gintv1[yhat.students[x]]<-gintv1[ yhat.students[x]]+1
  if (test[x]==2)
    gintv2[yhat.students[x]]<-gintv2[yhat.students[x]]+1
  if (test[x]==3)
    gintv3[yhat.students[x]]<-gintv3[yhat.students[x]]+1
} 

gintv=as.data.frame(t(data.frame(gintv1=gintv1, gintv2=gintv2, gintv3=gintv3)))
for(i in c(1:3)){
  gintv[i,]=gintv[i,]/sum(gintv[i,])
}

labels=c(1:3)
p2<-plot_ly(gintv,x=~Gintv, y=~V1, type='bar',marker = list(color = 'rgb(102,194,165)'),showlegend=F) %>%
  add_trace(y=~V2,marker = list(color = 'rgb(252,141,98)'))%>%
  add_trace(y=~V3,marker = list(color = 'rgb(141,160,203)'))%>%
  layout(barmode = 'stack',annotations=list(x = 0.6 , y = 1.05, text = "Bagging", showarrow = F, xref='paper', yref='paper'))


subplot(p1,p2,shareY = TRUE, titleX = TRUE)



# In this case the results are worse and even in this case there's no so much different between the two algorithms.
# We predict well the 56% of students with low scores, 88% of students with intermediate scores, and the 0% of students with high scores. 

# I can conclude that students who get high scores cannot be recognized by their habits or lifestyle: we have noticed weak tendencies, and maybe this dataset miss of some attributes, but in general there's not a strong rule that allows us to connect the background of a person to its academic performance.
