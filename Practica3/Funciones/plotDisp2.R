plotDisp2 <- function(data, r1, r2, r3, r4, ruta) {

    png(paste("./tmp/",ruta,sep=""))

    par(mfrow = c(2, 2))

    plot(data[,1], data[,2], main="Muestra 1")
    abline(r1, col = "red")

    plot(data[,3], data[,4], main="Muestra 2")
    abline(r2, col = "blue")

    plot(data[,5], data[,6], main="Muestra 3")
    abline(r3, col = "green")

    plot(data[,7], data[,8], main="Muestra 4")
    abline(r4, col = "yellow")

    dev.off()
}