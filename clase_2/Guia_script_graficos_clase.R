
#0. Borre todos los objetos existentes en el entorno de trabajo y establezca el directorio 
# de trabajo. Para saber en que directorio esta pues hacer getwd()

rm(list=ls()) 
# rm es el comando remove: al poner el list de esta forma borramos todo lo que pueda estar 
# almacenado 

setwd("C:\\Users\\Ana\\Dropbox\\lanueva\\listas\\Graficos") 
# establezco directorio de trabajo

#1. Lea el archivo StudentSurvey.txt y asignelo al data.frame Alumnos.

# El archvio Alumnos tiene en la primera linea el nombre de las variables: 
# Year, Gender, Smoke, etc.
# para que R entienda esto agrgamos header=T en la lectura.

Alumnos<- read.table("StudentSurvey.txt",header=T)

#2. Inspeccione los primeros casos de dicho data.frame.
head(Alumnos)

#Abra con el editor el data.frame y cierre el editor.
fix(Alumnos) # con esta instrucci\'on se abre un editor

# En R las variables de este archivo se identifican como Alumnos$Year, Alumnos$Gender, etc.  
# Si queremos nombrarlas como Year, Gender, Smoke, etc, debemos hacer un attach
# Se debe tener cuidado de que ya no existan otras variables con ese nombre.

#3. Inspeccione los nombres de las variables de Alumnos e identifique de que tipo 
# de variable se trata cada una de ellas.
names(Alumnos)

#4. Realice un attach de Alumnos.
attach(Alumnos)

#search() devuelve los data frames y paquetes atachados 

# 5. Realice un summary y un table de Gender. Hay diferencias entre estos comandos?
# Idem Smoke.

summary(Gender)
table(Gender)

summary(Smoke)
table(Smoke)

# el comano table aplicado a una variable categorica cuenta la frecuencia de cada categoria
# barplot realiza un diagrama de barras de una variable categorica, para la que hemos 
# calculado antes la tabla de frecuencia con table

#6. Realice un grafico de barras para Gender. Elija color y densidad de las rayas.
table(Gender)
counts.gender <- table(Gender)
barplot(counts.gender,col="blue",density=4)


#7. Realice un  grafico de tortas para Smoke, eligiendo colores y poniendo titulo.
table(Smoke)
counts.smoke <- table(Smoke)
pie(counts.smoke, col=c("blue","green"), main="Grafico de Torta de Smoke")
# aca arriba hemos elegido los colores de cada porcion

#8. Repita para Smoke con un grafico de tortas tridimensional, poniendo itulos y
# y asignando colores de su eleccion para las porciones de la torta.

library(plotrix) # Carga del paquete
pie3D(counts.smoke,col=c("blue","green"), main="Grafico de Torta 3D de Smoke")

#9. Realice una tabla de contingencia para Smoke y Gender de manera que Gender figure en 
# las filas de dicha tabla.

xtabs(~ Smoke+Gender)

#10. Realice barplot barplot para la tabla de contingencia de ambas variables Gender y Smoke.

counts <- table(Smoke,Gender)
barplot(counts,col=c("blue","red"),main="Smoke vs. Gender") 

#11. Realice un diagrama de barras apiladas. Elija colores rojo y azul.
plot(Gender,Smoke, col=c("blue","red")) 
# Compare con el anterior

#12. Realice una tabla de contingencia para Gender vs. Award y realice el barplot 
# correspondiente. Elija los colores de su agrado para las barras y coloque titulo.

xtabs(~ Gender+Award)
counts <- table(Gender, Award)
barplot(counts, names.arg=c("A","N","O"),main="Award vs. Gender", 
        col=c("blue","red"),xlab="Award", legend = rownames(counts),
        args.legend = list(x = "topleft", bty = "n"))

#13. Repita con diagrama de barras adyacentes
barplot(counts, names.arg=c("A","N","O"),main="Award vs. Gender",
        col=c("blue","red"),xlab="Award", legend = rownames(counts),
        args.legend = list(x = "topleft", bty = "n"),beside=TRUE)

#14. Repita los graficos anteriores disponiendo los dos graficos unos al lado del otro 
# en la misma ventana.

par(mfrow=c(1,2)) #asi dividimos la pantalla en dos mitades

barplot(counts, names.arg=c("A","N","O"),main="Award vs. Gender", 
        col=c("blue","red"),xlab="Award", legend = rownames(counts),
        args.legend = list(x = "topleft", bty = "n"))

barplot(counts, names.arg=c("A","N","O"),main="Award vs. Gender",
        col=c("blue","red"),xlab="Award", legend = rownames(counts),
        args.legend = list(x = "topleft", bty = "n"),beside=TRUE)

par(mfrow=c(1,1))  #asi restablecemos pantalla original

#15. Inspeccione primeros valore de GPA, calcule rango y promedio

head(GPA) # muestra los primeros 6 casos, si queremos los ultimos hacemos tail()
range(GPA) 
mean(GPA)

#16. Calcule la cantidad de valores ausentes o missings que tiene GPA.

is.na(GPA)
sum(is.na(GPA)) # cantidad de missings

# is.na() es una funcion logica que da FALSE o TRUE de acuerdo con que el valor 
# esta ausente o presente. Como TRUE y FALSE tienen una dualidad logica/numerica (1 y 0) 
# se pueden sumar y asi obtenemos la cantidad de TRUE, es decir la cantidad de NA.
  
#17. Calcule rango, promedio y summary omitiendo los valores missings.

range(GPA,na.rm=TRUE)             # la 1ra. componente es minimo, la 2da. el maximo
mean(GPA,na.rm=TRUE)
summary(GPA,na.rm=TRUE)

#18. Calcule la media, varianza y desvio standard de GPA. Calcule la mediana, la media podada 
# con porcentaje de poda 10% y 20% y la mad
# de dicha variable.

mean(GPA,na.rm=TRUE) # Promedio muestral
var(GPA,na.rm=TRUE)  # varianza muestral (divide por n-1)
sd(GPA,na.rm=TRUE)   # desvio standard muestral

median(GPA,na.rm=TRUE)
mean(GPA,na.rm=TRUE,trim=0.1) # trim=porcentaje de poda en cada cola
mean(GPA,na.rm=TRUE,trim=0.2)
mad(GPA,na.rm=TRUE)

#19. Arme una matriz con las variables Exercise,TV,Height, Weight, VerbalSAT, MathSAT, SAT, 
# GPA, Pulse y Piercings y calcule la media y el desvio standard de cada columna, 
# es decir de cada variable, mediante el comando apply.

SUB<-cbind(Exercise,TV,Height,Weight,VerbalSAT,MathSAT,SAT,GPA,Pulse,Piercings)
apply(SUB,2,mean,na.rm=TRUE)   # 2 indica que debe aplicar en la 2da. dimension,
                               # o sea por columna
apply(SUB,2,sd,na.rm=TRUE)

#20. Realice un histograma de densidad de VerbalSAT. 
#Que contenido tiene el objeto  hist(GPA,freq=F)?

hist(GPA,freq=F) # Calculamos histogramas de densidad

#21. Realice histogramas de densidad de VerbalSAT y MathSAT y dispongalos uno al lado del otro. 

par(mfrow=c(1,2))
hist(VerbalSAT,freq=F) 
hist(MathSAT,freq=F)
par(mfrow=c(1,1))

#22. Guarde grafico anterior en disco en formato pdf:
pdf ("histogramas.pdf ")
par(mfrow=c(1,2))
hist(VerbalSAT,freq=F)
hist(MathSAT,freq=F)
graphics.off()


#23. Idem en formato eps:
postscript("histogramas.eps")
par(mfrow=c(1,2))
hist(VerbalSAT,freq=F)
hist(MathSAT,freq=F)
graphics.off()


#24. Realice el histograma de densidad de VerbalSAT con una curva normal superpuesta

media<-mean(VerbalSAT)
desvio<-sd(VerbalSAT)

# creamos una secuencia entre el minimo y el maximo de 1000 puntos
grilla<-seq(range(VerbalSAT)[1],range(VerbalSAT)[2],length=100) 

# inspeccionamos los nombres del objeto creado por hist()
names(hist(VerbalSAT,freq=F))

# aumentamos los limites del eje vertical, asi entra la densidad normal
maximo<-max(hist(VerbalSAT,freq=F)$density)+0.0005
hist(VerbalSAT,ylim=c(0,maximo),freq=F,main="Histograma de Densidad de VerbalSAT")
lines(grilla,dnorm(grilla,media,desvio),col="blue")

#25. Realice un boxplot de VerbalSAT e identifique el indice de los outliers de VerbalSAT.
names(boxplot(VerbalSAT)) # devuelve nombres del objeto

names(VerbalSAT)<-1:length(VerbalSAT) # ponemos nombre a cada observacion
boxplot(VerbalSAT)$out

#26. Realice un boxplot para VerbalSAT. Al lado realice boxplots paralelos para VerbalSAT 
# clasificando a los estudiantes de acuerdo con su condicion de fumador. 

par(mfrow=c(1,2))
boxplot(VerbalSAT, main="VerbalSAT")
boxplot(VerbalSAT~Smoke, xlab="Smoke",main="VerbalSAT") #~ indica que variable clasifica
par(mfrow=c(1,1))

#27.  Grafique boxplots paralelos para VerbalSAT y MathSAT. 
#  Que pasa si ahora hacemos  boxplot(VerbalSAT,MathSAT)$out ??

boxplot(VerbalSAT,MathSAT,names=c("VerbalSAT","MathSAT"))

#28. Realice un bagplot para para Height y Weight. Realice sendos boxplots para Height y Weight y un bagplot para ambas variables. 
# Disponga en una misma pantalla.

#Ahora solo
library(aplpack)
bagplot(Height,Weight,xlab="Height",ylab="Weight",na.rm=T)

# Ahora juntos
m<-matrix(c(1:3, 3), 2, 2)
layout(m) # asi deividimos la pantalla en 3 partes
layout.show(3) # Veamos como queda

# Ahora grafico
library(aplpack) # ya estaba instalada: no seria necesario ponerlo otra vez
boxplot(Height,xlab="Height")
boxplot(Weight,xlab="Weight")
bagplot(Height,Weight,xlab="Height",ylab="Weight",na.rm=T) #ojo con outliers!
layout(c(1,1)) 


#29. Grafique un diagrama de dispersion de VerbalSAT vs. MathSAT. 
# Ponga labels en cada eje, titulo y elija un caracter para los puntos.

plot(VerbalSAT,MathSAT,xlab =" esta es VerbalSAT", ylab =" esta es  MathlSAT",
     main =" Plot of VerbalSAT vs. MathSAT",pch=16)

#30. Idem personalizando grafico

par(bg="lightgray",mar=c(4,2,3.5, 4)) 
#c(bottom, left, top, right)   defalut es c(5, 4, 4, 2) + 0.1.
plot(VerbalSAT,MathSAT,type="n",xlim=c(350,850),ylim=c(350,850),
     xlab="", ylab="",xaxt="n", yaxt="n")
#solo graficamos la caja
points(VerbalSAT,MathSAT,pch=20,col="magenta")
#solo graficamos los puntos con el simbolo deseado
#Ahora nos encargamos de los ejes
axis(1,c(400,600,800),cex=2)
mtext("VerbalMAT",side=1,cex=0.8,line=3)
axis(4,cex=0.8,col="blue",labels=FALSE)
mtext(c(400,500,600,700,800),side=4,at=c(400,500,600,700,800),col="blue",line=0.3)
mtext("MATHMAT",side=4,cex=0.8,line=2.5,col="blue")
#titulo
title("Diagrama Personalizado de VerbalSAT vs. MathSAT",cex.main=0.8)


#31. Realice diagrama de dispersion para todas las variables de SUB. Elija color de puntos y fondo 

par(mar=c(5, 4, 4, 2) + 0.1) # Valores de margenes de default
pairs(SUB,col="magenta")
par(bg="white")

#32. Realice mediante xyplot graficos bidimensionales de MathSAT respecto de VerbalSAT para 
# cada nivel de Award. Idem para Height vs. Weight  
# para cada nivel de Gender eligiendo tipo de caracter para los puntos y color.

library(lattice) #ojo lattice se sobreimpone a comandos existentes

xyplot(MathSAT~VerbalSAT|Award)

xyplot(Height~Weight|Gender,pch=15,col="darkblue")


##33. Idem anteriores con opcion groups.

# MathSAT vs. VerbalSAT por niveles de Award
xyplot(MathSAT~VerbalSAT,groups=Award, pch=c(1,2,3),type="p",
       col=c("red","blue","green"),key=list(columns=3,
       text=list(levels(Award)),points=list(col=c("red","blue","green"),
       pch=c(1,2,3))))

# Height vs. Weight por niveles de Gender.
xyplot(Height~Weight,groups=Gender, pch=c(16,16),type="p", col=c("red","blue"),
       key=list(columns=2,text=list(levels(Gender)),points=list(col=c("red","blue"),
       pch=c(16,16))))


#34. ## Graficos trisimensionales

#cortes de nivel
x<-seq(-pi,pi,length =50) # creamos la secuencia 50 puntos entre -pi y pi
y<-x
f<-outer(x,y, function(x,y) cos(y)/(1+x^2) ) # Hacemos una matriz de x por y

par(mfrow=c(1,3))
contour(x,y,f)              # graficamos cortes de nivel
contour(x,y,f,nlevels =15)
contour(x,y,f , nlevels =45)


#Ahora hacemos un grafico trisimensional variandoos angulos de vista y altura
par(mfrow=c(2,2),bg="white")
par(mar=c(1,1,1, 1))
persp (x,y,f)
persp (x,y,f , theta =30) #theta: angulo de direccion de vista
persp (x,y,f , theta =90) 
persp (x,y,f , theta =90, phi =45) # phi: angulo de altura de vista
par(mfrow=c(1,1),mar=c(5, 4, 4, 2) + 0.1)

