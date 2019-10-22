plotDisp <- function(data, regresion, xl, yl, ruta) {

    png(paste("./tmp/",ruta,sep=""))

    plot(data[,1], data[,2], xlab=xl, ylab=yl,main="Diagrama de Dispersion")
    abline(regresion, col="blue")

    dev.off()
}