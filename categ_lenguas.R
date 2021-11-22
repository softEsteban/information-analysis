datosLenguas <- read.csv("Lenguas en Colombia - Copy2.csv", header= T, sep=",")

class(datosLenguas)
names(datosLenguas)
head(datosLenguas)

cat <- datosLenguas$nCat
cat_num <- as.numeric(cat)
ncategory <- na.omit(cat_num)

#Conteo
sum(ncategory == 1)
sum(ncategory == 2)
sum(ncategory == 3)
sum(ncategory == 4)


conteo <- c(17,19,18,13)
categorias <- c("1. Vulnerable", "2. Peligro", "3. Peligro de extincion", "4. Situación crítica")
colores <- c("gold1","darkorange1", "coral1", "firebrick1")

# Pie Chart with Percentages
slices <- c(17,19,18,13)
lbls <- c("1. Vulnerable", "2. Peligro", "3. Peligro de extincion",
          "4. Situación crítica")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls,
    main="Pie Chart of Vitality")

# 3D Exploded Pie Chart
library(plotrix)
pie3D(slices,labels=lbls,explode=0.1,
      main="Pie Chart of Language Vitality ", col=colores)









