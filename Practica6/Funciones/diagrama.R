diagrama <- function(ruta,sp) {
    png(paste("./tmp/",ruta,sep=""))
 
    plot(sp)

    dev.off()
}