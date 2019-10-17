sopImg <- function(data, ruta) {

    png(paste("./tmp/",ruta,sep=""))

    itemFrequencyPlot(data, topN=10, type="relative", 
        main="Item Frequency")

    dev.off()
}