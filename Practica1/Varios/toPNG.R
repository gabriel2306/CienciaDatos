toPNG <- function(save,final) {

#Cargar libreria
library(png)

#Leer imagen blanca
img<-readPNG("blanco.png")

#Obtener tamaño de la imagen
h<-dim(img)[1]
w<-dim(img)[2]

#Crear 'final' para almacenar los datos
png(final, width=w, height=h)
par(mar=c(0,0,0,0), xpd=NA, mgp=c(0,0,0), oma=c(0,0,0,0), ann=F)
plot.new()
plot.window(0:1, 0:1)

#fill plot with image
usr<-par("usr")    
r<-rasterImage(img, usr[1], usr[3], usr[2], usr[4])

#Añadir 'save' a la imagen
t<-text(.2,.2, save, cex=2, col="black")

#Cerrar imagen
c<-dev.off()}