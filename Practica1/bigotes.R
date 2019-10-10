bigotes <- function(var,ruta) {
    png(ruta)
 
    boxplot(var) 

    dev.off()
}