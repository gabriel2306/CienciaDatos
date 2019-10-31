anovaR <- function(x,y) {
    mediaX<-mean(x)
    mediaY<-mean(y)

    recta<-calcularRecta(x,y,mediaX,mediaY)

    r2<-ssR(x,mediaY,recta)/ssY(y,mediaY)

    return(r2)
}

ssR <- function (x,mY,r) {
    sum<-0
    for (data in x){
        sum<-sum+((r[1]+r[2]*data)-mY)^2
    }

    return(sum)
}

ssY <- function (y, mY) {
    sum<-0
    for (data in y){
        sum<-sum+(y-mY)^2
    }

}

calcularRecta <- function (x,y,mX,mY) {
    b<-cov(x,y)/(desviacion(x,mX))^2
    a<-mY-b*mX

    return(list(a,b))
}

desviacion <- function (var,media) {
    num<-0
    for (data in var){
        num<-num+((data-media)^2)
    }

    s<-sqrt(num/length(var))

    return(s)
}

covarianza <- function(mX,My,y,x){
    sum<-0
    for(data1 in x,data2 in y){
        sum<-sum+(data1*data2)
    }

    cov <- (sum/length(y)) - mX*mY

    return(cov)
}
