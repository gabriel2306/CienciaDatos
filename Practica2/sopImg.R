sopImg <- function(data, n, ruta) {

    install.packages("RColorBrewer")
    library(RColorBrewer)

    png(paste("./tmp/",ruta,sep=""))

    itemFrequencyPlot(data, topN=n, col=brewer.pal(8,'Set1'),
        type="relative",main="Soporte")

    dev.off()
}