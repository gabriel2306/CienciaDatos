histograma <- function(var,name,ruta) {
    png(ruta)
 
    hist(var, col='orange', breaks=40, xlab=name, ylab="Frecuencia", main ="Histograma") 

    dev.off()
}