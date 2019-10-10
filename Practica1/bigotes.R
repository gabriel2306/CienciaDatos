bigotes <- function(var,ruta) {
    png(paste("./tmp/",ruta,sep=""))
 
    boxplot(var, col='green', horizontal=T) 

    dev.off()
}