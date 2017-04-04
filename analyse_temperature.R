vartemp<-function(x,annee,temperature,stationame,n){
  ##Cette fonction caractérise la variabilité des températures annuelles
  ## x: la data.frame de départ, annee: la colonne de la
  ## data.frame contenant les années, temperature: colonne
  ## contenant les temperatures annuelles, stationame: nom de la
  ## station, n: ordre de la moyenne mobile
  # Calcul des anomalies centrées réduites
  x<-transform(x,annee=annee,temperature=temperature,
               u=(temperature-mean(temperature))/sd(
                 temperature),class=rep(NA,length(annee)),
               col=rep(NA,length(annee)))
  mm<-filter(x$u,filter=rep((1/n),n),sides=2)
  mm<-data.frame(x$annee,mm)
  colnames(mm)<-c("annee","mm")
  
  for (i in 1:length(annee)){
    if(x$u[i]>0) {
      
      x$class[i]="Année excédentaire"
      x$col[i]="gray30"
    }
    if(x$u[i]<0) {
      
      x$class[i]="Année déficitaire"
      x$col[i]="gray60"
    }
    
    
    
  }

  ## Graphiques
  ylim<-range(x$u)*c(1,1.5)
  bp<-barplot(x$u,names.arg = x$annee,beside=T,col=x$col,ylim=ylim,
          ylab="Anomalies centrées réduites",xlab="Années",
          main = paste("Station de ",stationame,sep=""))
  lines(bp,mm$mm,lty=1,lwd=2)
  typ<-c("Années excédentaires","Années déficitaires")
  legend("topleft",legend = typ,ncol=1,bty = "n",
         cex=1.2,fill=c("gray30","gray60"))
  par(bg="transparent")
  legend("topright",legend = c("Moyenne mobile"),ncol=1,bty = "n",
         cex=1.2,col="black",pch=NA,lty = 1,lwd=2)
}
