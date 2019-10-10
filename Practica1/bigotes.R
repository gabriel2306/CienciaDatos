bigotes <- function(var,ruta) {
    png(ruta)
 
    boxplot(var, col='green', horizontal=T) 

    dev.off()
}