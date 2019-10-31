anovaR <- function(x,y) {
    mediaX<-mean(x)
    mediaY<-mean(y)

    recta<-calcularRecta(x,y,mediaX,mediaY)

    r2<-(ssR(x,mediaY,recta))/(ssY(y,mediaY))

    return(r2)
}

ssR <- function (x,mY,r) {
    sum<-0
    for (data in x){
        sum<-sum+((r$a+r$b*data)-mY)^2
    }

    return(sum)
}

ssY <- function (y, mY) {
    sum<-0
    for (data in y){
        sum<-sum+(data-mY)^2
    }
    
    return(sum)
}

calcularRecta <- function (x,y,mX,mY) {
    b<-covarianza(x,y,mX,mY)/(desviacion(x))^2
    a<-mY-b*mX

    l<-list("a"=a, "b"=b)

    return(l)
}

desviacion <- function (var) {
    media<-mean(var)
    num<-0
    for (data in var){
        num<-num+((data-media)^2)
    }

    s<-sqrt(num/length(var))

    return(s)
}

covarianza <- function(x,y,mX,mY){
    sz<-length(x)
    sum<-0
    for(i in 1:sz){
        sum<-sum+(x[i]*y[i])
    }

    cov <- (sum/sz) - mX*mY

    return(cov)
}

correlacion <- function(cov,x,y){
    cr<-cov/((desviacion(x))*desviacion(y))

    return(cr)
}