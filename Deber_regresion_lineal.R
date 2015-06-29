#library(readxl)
library(ggplot2)
dir<-"C:/Users/TOSHIBA/Desktop"
setwd(dir)
list.files()
data<-read.table("data_rls_uti.txt",header=TRUE,dec=",",sep="\t")
#data<-read_excel("data_rls_uti.xlsx",sheet=1,na="")
Utilidad<-data[,1]-mean(data[,1])
Ventas<-data[,2]-mean(data[,2])
data_cen<-data.frame(Ventas,Utilidad)
View(data_cen)
#Graficos
ggplot(data_cen,aes(x=Utilidad,y=Ventas))+geom_point()
gra<-ggplot(data_cen,aes(x=Utilidad,y=Ventas))
gra+geom_point()+stat_smooth(method=lm)
#coeficiente de COrrelacion
cor(data_cen)

#regresion
regre<-lm(Utilidad~Ventas,data_cen)
regre
summary(regre)

#tabla ANOVA
tanova<-aov(regre)

summary(tanova)
F_alfa<-qf(0.95,df1=1,df2=38)

#Residuos
residuo<-regre[["residuals"]]

#Intervalos de Confianza, con nivel 0.95
confint(regre)

#Valores Ajustados por la regresion "yi techo"
ajuste<-regre[["fitted.values"]]

#creamos una nueva data frame aumentando las columnas residuos y valores ajustados
data2<-data.frame(data_cen,ajuste,residuo)
View(data2)

#Graficas mixtas
ggplot(data2,aes(x=residuo,y=..density..))+geom_histogram(fill="cornsilk",colour="grey60")+geom_density()


#normalidad de residuos
qqnorm(residuo)
qqline(residuo,col="blue")
ggplot(data2,aes(x=Ventas,y=residuo))+geom_point()
