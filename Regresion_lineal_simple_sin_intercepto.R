#install.packages("readxl",dependencies=TRUE)
library(readxl)
ls("package:readxl")
data<-read_excel("data_rls_uti.xlsx",sheet=1,na="")
str(data)
data <- read.table("data_rls_uti.txt", header = TRUE, dec=",", sep="\t")
str(data)
u1<-mean(data[,"Utilidad"])
u1
v1<-mean(data[,"Ventas"])
v1
u<-data[,"Utilidad"]-mean(u1)
v<-data[,"Ventas"]-mean(v1)

#Diagrama de dispersión
plot(u,v, main="Gráfico de Dispersión", col="red")
#Coeficiente de correlación
cor(u, v)

reg <- lm(u~v)
str(reg)
summary(reg)
#ANOVA
anova<-aov(reg)
summary(anova)
#Intervalos de confianza
confint(reg,level=0.95)
#Graficos residuales
names(reg)
str(reg)
res<-reg[["residuals"]]
hist(res,breaks=5,main="Grafico de Residuos",col="blue")
mean(res)
predic<-reg$fitted.values
predic
plot(res,u, main="Grafico de Residuos vs Utilidad", col="red")
plot(res,predic,main="Grafico de Residuos vs Pronostico de Utilidad", col="blue")
plot(res,v,main="Grafico de Residuos vs Ventas", col="green")
# Para la Normalidad
qqnorm(res,main="Grafico de Normalidad",col="blue")
qqline(res,col="red")

