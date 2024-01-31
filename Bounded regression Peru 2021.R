
library(compositions)
library(Hmisc, pos=4)
library(foreign, pos=4)
library(corrplot)
library(ggplot2)
library(readxl)

#1.Dados eleições 1er turno 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
Electionsdata2021 <- read_excel("elections2021_1round.xlsx")
names(Electionsdata2021)
head(Electionsdata2021)

#2.Dados eleições 2do turno 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
Elections2021_2round <- read_excel("Elections2021_2round.xlsx")
Elections2021_2round

#3. Transformações
### Transformação em proporções

Matriz_2round = as.matrix(Elections2021_2round [,c(3,4,5)])
Matriz_2round_prop =matrix(numeric(25),nrow = 25,ncol =2)
for(i in 1:2){
  Matriz_2round_prop[,i] = Matriz_2round[,i]/Matriz_2round[,3]
}
colnames(Matriz_2round_prop) = c("Party1","Party2")

data2_2021_prop = as.data.frame(Matriz_2round_prop)
data2_2021_prop

parties=c("PL","FP","RP","AVP","ACCP","OTHERS","VALID VOTES")
matriz_votes_2021 = as.matrix(Electionsdata2021[,parties])
cor(matriz_votes_2021)


matriz_prop_2021 =matrix(numeric(25),nrow = 25,ncol = 6)
matriz_prop_2021 

for(i in 1:6){
  matriz_prop_2021[,i] = (matriz_votes_2021[,i]/matriz_votes_2021[,7])*100
}
matriz_prop_2021


### Transformação clr
parties1=c("PL","FP","RP","AVP","ACCP","OTHERS")
matriz_tr_2021 = matrix(numeric(25),nrow = 25,ncol = 6)
colnames(matriz_tr_2021)= parties1
matriz_tr_2021=clr(matriz_prop_2021)
matriz_tr_2021

# 4.Análise de componentes principais dos dados transformados

TransformedCPA<- princomp(matriz_tr_2021,cor =TRUE)

TransformedCPA$loadings
summary(TransformedCPA)
TransformedCPA$scores


biplot(TransformedCPA,xlab='First Component',
       ylab='Second Component',main ="Log contrast PCA scale1")

# 5. Normalização dos escores

#getAnywhere(biplot.princomp)
lam =((TransformedCPA$sdev))*sqrt(TransformedCPA$n.obs)
lam
#scale != 0 >> lam^scale 
#scale = 1 so, we say lam = lam, then 
lam = lam
score1 = TransformedCPA$scores[,1]/ lam[1]
score2 =TransformedCPA$scores[,2] / lam[2]
score1 #hist(score1, main = "Histogram Score 1",xlim=c(-0.6,0.6))
score2 #hist(score2, main = "Histogram Score 2",xlim=c(-0.6,0.6))



#par(mfrow=c(1,2))
#par(mar=c(2,2,2,2))



### escore 1 
score1_t1 <- (score1 - min(score1))/(max(score1) - min(score1))
score1_t1[15]= score1_t1[15]+0.0001
score1_t1[21] =score1_t1[21] -0.0001
score1_t1 #hist(score1_t1, main = "Histograma de z1",ylim = c(0, 6),xlim = c(0, 1.0),breaks = 10)

### escore 2
score2_t1 <- (score2 - min(score2))/(max(score2) - min(score2))
score2_t1[23]= score2_t1[23]+0.0001
score2_t1[1] =score2_t1[1] -0.0001
score2_t1 #hist(score2_t1, main = "Histograma de z2",ylim = c(0, 6),xlim = c(0, 1.0),breaks = 10)


par(mfrow=c(1,2))
par(asp = 0.5)
plot(density(score1_t1),main="Densidade de z1", xlab="", ylab="",col="lightgray", lwd=2)
polygon(density(score1_t1),main="Densidade de z1", xlab="", ylab="",col="lightgray", lwd=2)

plot(density(score2_t1),main="Densidade de z2", xlab="", ylab="",col="lightgray", lwd=2)
polygon(density(score2_t1),main="Densidade de z2", xlab="", ylab="",col="lightgray", lwd=2)


# 6. Dados para implementar o modelo

#indicators
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
indicators <- as.data.frame(read_excel("indicators.xlsx"))
indicators
indicators = indicators[,c(-1,-2)]
Data_indicators = cbind(indicators,score1_t1,score2_t1 )
Data_indicators 
summary(Data_indicators)



# 7. GAMLSS

#packageVersion("gamlss")

###Escore1
#####################################################################
library(gamlss)
gamlss_score1_t1_m1<- gamlss(score1_t1~HealthINEI+educationINEI1+educationINEI2+IncomeINEI,family=BE,data=Data_indicators)
summary(gamlss_score1_t1_m1)


gamlss_score1_t1_m2<- gamlss(score1_t1~HealthINEI+educationINEI1+IncomeINEI,family=BE,data=Data_indicators)
summary(gamlss_score1_t1_m2)

#library(xtable)
#xtable(gamlss_score1_t1_m2)
gamlss_score1_t1_m3<- gamlss(score1_t1~educationINEI1+IncomeINEI,family=BE,data=Data_indicators)
summary(gamlss_score1_t1_m3)

###Escore2
#####################################################################
gamlss_score2_t1_m1<- gamlss(score2_t1  ~ HealthINEI+educationINEI1+educationINEI2+IncomeINEI,family=BE,data=Data_indicators)
summary(gamlss_score2_t1_m1)

gamlss_score2_t1_m2<- gamlss(score2_t1  ~ educationINEI1+educationINEI2+IncomeINEI,family=BE,data=Data_indicators)
summary(gamlss_score2_t1_m2)

gamlss_score2_t1_m3<- gamlss(score2_t1  ~ educationINEI1,family=BE,data=Data_indicators)
summary(gamlss_score2_t1_m3)
fitted(gamlss_score2_t1_m3)


# 8. Diagnóstico

par(mfrow=c(2,2))
plot(fitted(gamlss_score1_t1_m3),resid(gamlss_score1_t1_m3),xlab="Fitted values",ylab="Residuals",main="a. Fitted values vs Residuals",ylim =c(-3,3))#### graph 1
plot(1:nrow(Data_indicators),resid(gamlss_score1_t1_m3),xlab="Index",ylab="Residuals",main="b. Index vs Residuals",ylim =c(-3,3))#### graph 2
plot(density(resid(gamlss_score1_t1_m3)),main="c. Density Estimate")#### graph 3
library(car)
qqPlot(resid(gamlss_score1_t1_m3), ylab = "Normalized quantile residuals", main ="d. Normal Q-Q Plot", ylim =c(-3,3),id=list(method="y", n=25, cex=0.5), )#### graph 4


par(mfrow=c(2,2))
plot(fitted(gamlss_score2_t1_m2),residuals(gamlss_score2_t1_m2),xlab="Fitted values",ylab="Residuals",main="a. Fitted values vs Residuals",ylim = c(-3,3))#### graph 1
plot(1:nrow(Data_indicators),resid(gamlss_score2_t1_m2),xlab="Index",ylab="Residuals",main="b. Index vs Residuals",ylim = c(-3,3))#### graph 2
plot(density(resid(gamlss_score2_t1_m2)),main="c. Density Estimate")#### graph 3
qqPlot(resid(gamlss_score2_t1_m2), ylab = "Normalized quantile residuals", main ="d. Normal Q-Q Plot", ylim =c(-3,3),id=list(method="y", n=25, cex=0.5), )#### graph 4









