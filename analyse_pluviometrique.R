varpluie<-function(x,annee,pluviometrie,stationame,n){
  ##Cette fonction caract�rise la variabilit� pluviom�trique
  ## x: la data.frame de d�part, annee: la colonne de la
  ## data.frame contenant les ann�es, pluviometrie: colonne
  ## contenant les hauteurs de pluie annuelles, stationame: nom de la
  ## station, n: ordre de la moyenne mobile
  ## Calcul des anomalies et graphiques
  ## suivant la classification  de V.HOUSSOU et al.,2016
  ## u est la variable centr�e r�duite, ind la densit� de remplissage
  ## des diagrammes � bande, ang et ang2 sont les angles des lignes de
  ## remplissage et col est la couleur des bandes
  
  x<-transform(x,annee=annee,pluviometrie=pluviometrie,
               u=(pluviometrie-mean(pluviometrie))/sd(
                 pluviometrie),class=rep(NA,length(annee)),
               ind=rep(NA,length(annee)),ang=rep(NA,length(annee)), 
               ang2=rep(NA,length(annee)),col=rep(NA,length(annee)))
  
  # Moyennes mobiles
  
  mm<-filter(x$u,filter=rep((1/n),n),sides=2)
  mm<-data.frame(x$annee,mm)
  colnames(mm)<-c("annee","mm")
  
  # Classification et d�finition du remplissage des bandes
  
   for (i in 1:length(annee)){
    if(x$u[i]>2) {
      
      x$class[i]="Humidit� extr�me"
      x$ind[i]=10
      x$ang[i]=45
      x$ang2[i]=45
      x$col[i]="white"
    }
     if(x$u[i]<=2&x$u[i]>1) {
       
       x$class[i]="Tr�s pluvieuse"
       x$ind[i]=10
       x$ang[i]=45
       x$ang2[i]=-45
       x$col[i]="white"
     }
     if(x$u[i]<=1&x$u[i]>=0.25) {
       
       x$class[i]="Mod�r�ment pluvieuse"
       x$ind[i]=10
       x$ang[i]=45
       x$ang2[i]=0
       x$col[i]="white"
     }
     if(x$u[i]<0.25&x$u[i]>(-0.25)) {
       
       x$class[i]="Normale"
       x$ind[i]=0
       x$ang[i]=0
       x$ang2[i]=0
       x$col[i]="white"
     }
     if(x$u[i]<=(-0.25)&x$u[i]>=(-1)) {
       
       x$class[i]="Mod�r�ment s�che"
       x$ind[i]=0
       x$ang[i]=0
       x$ang2[i]=0
       x$col[i]="gray80"
       
     }
     if(x$u[i]<(-1)&x$u[i]>=(-2)) {
       
       x$class[i]="Tr�s s�che"
       x$ind[i]=0
       x$ang[i]=0
       x$ang2[i]=0
       x$col[i]="gray50"
     }
     if(x$u[i]<(-2)) {
       
       x$class[i]="S�cheresse extr�me"
       x$ind[i]=0
       x$ang[i]=0
       x$ang2[i]=0
       x$col[i]="gray30"
       
     }
   }
  
  # Cr�ation des couleurs et rayures (suivant les diff�rentes
  # classes identifi�es) � utiliser dans la l�gende
  
  ylim<-range(x$u)*c(1,2)
  leg<-c("Humidit� extr�me","Tr�s pluvieuse","Mod�r�ment pluvieuse",
         "Normale","Mod�r�ment s�che", "Tr�s s�che","S�cheresse extr�me")
  leg<-leg[leg%in%levels(factor(x$class))]
  le<-length(leg)
  ang<-rep(NA,7)
  ang2<-ang
  col<-ang
  dens<-ang

  for (i in 1:le){
    if (leg[i]=="Humidit� extr�me"){
      ang[i]=45
      ang2[i]=45
      dens[i]=10
      col[i]="white"
      # ang et ang2 : angles des traits devant remplir les bandes du graphique
      # dens : la densit� de remplissage
      # col : la couleur des bandes
    }
    if (leg[i]=="Tr�s pluvieuse"){
      ang[i]=45
      ang2[i]=-45
      dens[i]=10
      col[i]="white"
    }
    if (leg[i]=="Mod�r�ment pluvieuse"){
      ang[i]=45
      ang2[i]=0
      dens[i]=10
      col[i]="white"
    }
    if (leg[i]=="Normale"){
      ang[i]=0
      ang2[i]=0
      dens[i]=0
      col[i]="white"
      pos<-i
      # pos permet de savoir � partir de quel indice
      # on doit commencer � mettre des couleurs dans la l�gende
      # car une partie de la l�gende sera remplie de traits et une
      # autre sera remplie avec des couleurs
    }
    if (leg[i]=="Mod�r�ment s�che"){
      ang[i]=0
      ang2[i]=0
      dens[i]=0
      col[i]="gray80"
    }
    if (leg[i]=="Tr�s s�che"){
      ang[i]=0
      ang2[i]=0
      dens[i]=0
      col[i]="gray50"
    }
    if (leg[i]=="S�cheresse extr�me"){
      ang[i]=0
      ang2[i]=0
      dens[i]=0
      col[i]="gray30"
    }
  }
  ang<-na.exclude(ang)
  ang2<-na.exclude(ang2)
  dens<-na.exclude(dens)
  col<-na.exclude(col)
  
  ## Graphiques
  
  barplot(x$u,names.arg = x$annee,beside=T,col=x$col,ylim=ylim,
          ylab="Anomalies centr�es r�duites",xlab="Ann�es")
  bp<-barplot(x$u,names.arg = x$annee,beside=T,col="black",density = x$ind,
          angle = x$ang,ylim=ylim,main = paste("Station de ",stationame,sep=""),
          ylab="Anomalies centr�es r�duites",xlab="Ann�es",add=T)
  barplot(x$u,names.arg = x$annee,beside=T,col="black",density = x$ind,
          angle = x$ang2,ylim=ylim,add = T,
          ylab="Anomalies centr�es r�duites",xlab="Ann�es")
  lines(bp,mm$mm,lty=1,lwd=2)
  legend("topleft",legend = leg[1:pos],ncol=1,bty = "n",
         fill = T,cex=1.2,col=col[1:4],angle = ang[1:4],
         density =dens[1:4]*c(rep(2,2),rep(1,2)))
  par(bg="transparent")
  legend("topleft",legend = leg[1:pos],ncol=1,bty = "n",
         fill = T,cex=1.2,col=col[1:4],angle = ang2[1:4],
         density =dens[1:4]*c(rep(2,2),rep(1,2)))
  pos<-pos+1
  legend("topright",legend = leg[pos:le],ncol=1,bty = "n",
         cex=1.2,fill=col[pos:le])
  par(bg="transparent")
  legend("topright",legend = c(leg[pos:le],"Moyenne mobile"),ncol=1,bty = "n",
         cex=1.2,col=c(rep(NA,length(pos:le)),"black"),pch=NA,lty = 1,lwd=2)
  
}


