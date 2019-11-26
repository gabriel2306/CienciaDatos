bigotes <- function(var,ruta,rango) {
    png(paste("./tmp/",ruta,sep=""))
 
    boxplot(var, range=rango, col='green', horizontal=T) 

    dev.off()
}