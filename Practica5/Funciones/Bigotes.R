bigotes <- function(var,ruta,rango) {
    png(paste("./tmp/",ruta,sep=""))
 
    boxplot(var, range=rango, col='red', horizontal=T) 

    dev.off()
}