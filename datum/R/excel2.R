###############
#Objective: Excel to R. UDAPE - IETA-DATUM
#author: Alvaro Chirino
###############
#library
library(xlsx)
###############
rm(list=ls())
setwd("/Users/iMac6/Desktop/outside/2018/Banco/1udape/htms/CAP01") #capitulo 1
###############
#xx<-dir()
#for(zz in xx){
  #print(zz)

  
dd<-read.xlsx("c010101.xls",1,header=F,stringsAsFactors=FALSE)
aux<-dim(dd)
nar<-apply(apply(dd,1,is.na),2,sum)
nac<-apply(apply(dd,2,is.na),2,sum)
dd<-dd[,nac!=aux[1]]
dd<-dd[nar!=aux[2],]
nar<-apply(apply(dd,1,is.na),2,sum)
nac<-apply(apply(dd,2,is.na),2,sum)
#nombres
for(i in 1:dim(dd)[1]){
  if(nar[i]==0){
    iname<-i
    break
  }
}
names(dd)<-dd[iname,]
dd<-dd[-4,]
#detalle de la base
det<-""
for(i in 1:dim(dd)[2]){
  for(j in 1:(iname-1)){
    if(!is.na(dd[j,i])){
      det<-paste(det,dd[j,i])
    }
  }
}
dd<-dd[-(1:(iname-1)),]
#fin de la base
nar<-apply(apply(dd,1,is.na),2,sum)
nac<-apply(apply(dd,2,is.na),2,sum)
for(i in dim(dd)[1]:1){
  if(nar[i]==0){
    iname<-i
    break
  }
}
#Pie de notas
pie<-""
for(i in 1:dim(dd)[2]){
  for(j in dim(dd)[1]:(iname+1)){
    if(!is.na(dd[j,i])){
      pie<-paste(pie,dd[j,i])
    }
  }
}
dd<-dd[-((iname+1):dim(dd)[1]),]

#valores perdidos en la primera columna
dd<-dd[!is.na(dd[,1]),]
#bases apiladas
fin<-dim(dd)
aux<-dd[fin[1],1]
auxvec<-NULL
for(i in 1:fin[1]){
  print(i)
  if(dd[i,1]==aux){
    auxvec<-c(auxvec,i)
  }
}
bd<-list()
zz<-1
for(i in length(auxvec)){
  bd[[i]]<-dd[1+auxvec[i]*(zz-1):auxvec[i],]
  if(i>2){zz<-2}
  zz<-zz+1
}
head(dd,18)
str(bd)
i<-1;zz<-1
head(dd[1+auxvec[i]*(zz-1):auxvec[i],])
#tipo de variables a numericas
for(i in 2:dim(dd)[2]){
  if(!is.numeric(dd[,i])){
    dd[,i]<-as.numeric(dd[,i])
  }
}
#csv
nn<-strsplit(zz,".xls")[[1]]
nn<-paste0(nn,".csv")
write.table(dd,nn,sep = ";")
}



