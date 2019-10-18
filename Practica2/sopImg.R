sopImg <- function(data, ruta) {

    install.packages("RColorBrewer")
    library(RColorBrewer)

    png(paste("./tmp/",ruta,sep=""))

    itemFrequencyPlot(data, topN=10, col=brewer.pal(8,'Set1'),
        type="relative",main="Soporte")

    dev.off()
}