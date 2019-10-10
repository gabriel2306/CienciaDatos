toTxt <- function(save,name,r) {
    write.table(save, paste("./tmp/",name,sep=""), sep="\t", row.names=r) 
}