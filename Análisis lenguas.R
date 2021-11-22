#-------------------------------------------------------------
#Título: Lenguas nativas colombianas en peligro de extinción |
#Fuente: www.datos.gov.co                                    |
#Fecha de publicación: 7 de oct 2021                         |  
#-------------------------------------------------------------

#Importación de datos y exploración previa

datosLenguas <- read.csv("Lenguas en Colombia - Copy.csv", header= T, sep=",")

class(datosLenguas)
names(datosLenguas)
head(datosLenguas)


#Subgrupos y limpieza de datos------------------------------------------------------

  porcentajes_subg <- datosLenguas$Porcentaje
  porcentajes_na <- as.numeric(porcentajes_subg)
  porcentajes <- na.omit(porcentajes_na)
    
  class(porcentajes)

#Estadísticos descriptivos (tendencia central y localización)
  
  mean(porcentajes)
  median(porcentajes)
  quantile(porcentajes)
  quantile(porcentajes, 0.65)
  summary(porcentajes)
  
  frecuencias <- data.frame(table(porcentajes))
  moda <- frecuencias[which.max(frecuencias$Freq),1]
  
  paste("La moda de la variable edad es", moda)

#Estadísticos de dispersión-----------------------------------------------------
  
  var(porcentajes) #Varianza del vector
  sd(porcentajes) #Desviación estándar
 
  x <- mean(porcentajes)
  y <- sd(porcentajes)
  
  cv <- (y/x)*100  #Coeficiente de variación
  cv
  
#Estadísticos de forma/simetría-------------------------------------------------
  
  library(psych)
  skew(porcentajes) # Coeficiente de asimetría
  
  kurtosi(porcentajes) # Curtosis
  
  library(modeest)
  mfv(porcentajes)   #Most frequent value
  
 
  
  
  
  

   
#Histograma y diagrama de cajas
  
  promedioh = mean(porcentajes)
  library(sfsmisc)
  histBxp(porcentajes, main="Histograma de lenguas indígenas en extinción", col="orange", 
          boxcol="lightcoral", medcol="indianred4", xlab="Hablantes por comunidad", 
          ylab="Frecuencias")

  histBxp(habitantes, col="orange", boxcol="lightcoral", medcol="indianred4")
  abline(v = mean(porcentajes), col="red")
  
  
  
  
#Visualizaciones
stem(porcentajes) #Diagrama de tallo y hoja
hist(porcentajes) #Histograma

hist(porcentajes, main="Histograma de lenguas en extinción", 
     xlab="Rangos", ylab="Frecuencias", col="pink", las=1,
     lwd=2,border="red", freq=F)
points(mean(porcentajes),1, pch=10, lwd=2, col="red")

dx <- density(porcentajes)

dy <- density(porcentajes)

plot(dx)
plot(dy)
polygon(dx, density=15, angle=90, lwd=2, col="red")

points(mean(porcentajes),1, pch=10, lwd=2)




