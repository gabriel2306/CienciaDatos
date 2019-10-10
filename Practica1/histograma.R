histograma <- function(var,name,ruta) {
    png(paste("./tmp/",ruta,sep=""))
 
    h<-hist(var, col='orange', breaks=40, xlab=name, 
            ylab="Frecuencia absoluta", main ="Histograma") 

    dev.off()
}