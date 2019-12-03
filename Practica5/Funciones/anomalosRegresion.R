anomalosRegresion <- function (residuos, limite, muestra) {
    tmp<-""

    for(i in 1:length(residuos)){
        if(residuos[i]>limite){
            tmp<-paste(tmp,"El suceso ", i, 
                " con valor (", muestra$r[i]," ",
                 muestra$d[i], ") es outlier - ",sep="")
        }
    }

    return(tmp)
}