sucesosAnomalos <- function (muestra, intervalo) {
    tmp<-""

    for(i in 1:length(muestra)){
        if(muestra[i] < intervalo[1] || muestra[i] > intervalo[2]){
            tmp<-paste(tmp, "El suceso ", i, " con valor ", muestra[i],
            " es un outlier - ", sep="")
        }
    }
    
    return(tmp)
}