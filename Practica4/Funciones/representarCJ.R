representarCJ <- function (cj, kP, ruta) {
    png(paste("./tmp/",ruta,sep=""))

    fviz_dend(x=cj, k=kP)   

    dev.off()
}