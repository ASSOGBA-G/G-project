## Cette fonction est l'application de la m�thode d�crite par Erpicum et al.,1988
## Selon ces auteurs, la saison des pluies commence au moment o� la probabilit� d'avoir un jour de pluie
## au cours d'une � pentade � (p�riode de cinq jours) d�termin�e est sup�rieure � celle d'avoir un jour sec
## appartenant � un �pisode de plus de sept jours.Selon le m�me calcul, la fin de la saison des pluies est fix�e
## d�s que la probabilit� d'avoir un jour sec appartenant � un �pisode de sept jours ou plus est sup�rieure 
## � celle d'avoir un jour de pluie au cours d'une � pentade �
## Il prend comme principal argument une colonne de date et une autre contenant les
## pluies journali�res

assogba.erpicum<-function(x,start,end,station,dest){
  
  # x est un tableau dont la premi�re colonne conient les dates
  # et la seconde les pluies journali�res, start=ann�e de d�part
  # et end = ann�e de fin, dest= dossier contenant les r�sultats
  
  # selection des donn�es
  x<-subset(x,subset = as.numeric(substr(x[,1],1,4))>=start & as.numeric(substr(x[,1],1,4)<=end))
  
  # Initialisations et cr�ation du tableau et vecteurs contenant les r�sultats + remplacement des NA
  # Certains NA ont �t� remplac�s par -999 ou 999 selon le type de comparaison � faire plus tard
  n.annee<-start:end
  n.pentade<-73*length(n.annee)
  p.pluie<-rep(-999,n.pentade)
  p.sec<-rep(-999,n.pentade)
  t.dest<-x
  t.dest[,2]<-rep(0,length(x[,2]))
  xx<-x
  xx2<-xx[,2]
  xx2[xx2%in%NA]<-rep(999,length(xx2[xx2%in%NA]))
  xx[,2]<-xx2
  
  xcor<-x
  xc2<-xcor[,2]
  xc2[xc2%in%NA]<-rep(-999,length(xc2[xc2%in%NA]))
  xcor[,2]<-xc2
  
  i<-1
  j<-1
  q<-1
  
  # Transformation des s�quences s�ches (pluviom�trie < 1 mm) de plus de 7 jours en s�ries constitu�es du chiffre 1
  nj<-length(xx[,2])
  while (i<=nj) {
    
    n.jsec<-0
    
      while (xx[j,2]<1) {
        
        n.jsec<-n.jsec+1
        j<-j+1
        
        if (j>nj) break
      }
    
    if (n.jsec>7){
      k<-j-1
      m<-k-i+1
      t.dest[i:k,2]<-rep(1,m)
      j<-j+1
      i<-j
    }
    else {
      
      j<-j+1
      i<-j
    }
    
  }
  
  
  # R�partion des donn�es par ann�e et calcul des proportions humides et s�ches
  j<-1
  
  for (o in 1:length(n.annee)) {
    
    year.data<-xcor[as.numeric(substr(xcor[,1],1,4))%in%n.annee[o],]
    year.data.sec<-t.dest[as.numeric(substr(t.dest[,1],1,4))%in%n.annee[o],]
    nj.an<-length(year.data[,1])
    
    # Calcul des proportions de jours humides (pluviom�trie >= 1 mm) de fa�on � obtenir 73 pentades
    # m�me en ann�e bissextile
    i<-1
    while (i<nj.an) {
      g<-i+4
      pentade<-year.data[i:g,2]
      p<-length(pentade[pentade>=1])/5
      p.pluie[j]<-round(p,3)
      i<-i+5
      
      if (i+9>nj.an){
        l<-nj.an-i
        pentade<-year.data[i:i+l,2]
        p<-length(pentade[pentade>=1])/(l+1)
        p.pluie[j+1]<-round(p,3)
        i<-nj.an
        j<-j+1
      }
      
      j<-j+1
    }
    
    
    i<-1
    
    # Calcul des proportions de jours secs (pluviom�trie < 1 mm) de fa�on � obtenir 73 pentades
    # m�me en ann�e bissextile
    while (i<nj.an) {
      f<-i+4
      pentade<-year.data.sec[i:f,2]
      p<-length(pentade[pentade%in%1])/5
      p.sec[q]<-round(p,3)
      i<-i+5
      
      if (i+9>nj.an){
        l<-nj.an-i
        h<-i+l
        pentade<-year.data.sec[i:h,2]
        p<-length(pentade[pentade%in%1])/(l+1)
        p.sec[q+1]<-round(p,3)
        i<-nj.an
        q<-q+1
      }
      
      q<-q+1
    }
    
    
    
    
  }
  
 
  # Cr�ation du tableau contenant les proportions de jours humides et secs etle num�ro des pentades
  # Il existe 73 pentades dans une ann�e
  
  num.pentade<-rep(1:73,length(n.annee))
  an<-num.pentade
  j<-1
  for (i in 1:length(n.annee)){
    k<-j+72
    an[j:k]<-rep(n.annee[i],73)
    j<-k+1
  }
  
  # details.prop contient les proportions par pentade sur toute la p�riode
  # d'�tude et final.prop contient les proportions moyenne par pentade sur 
  # la m�me p�riode
  
  details.prop<-data.frame(an,num.pentade,p.pluie,p.sec)
  colnames(details.prop)<-c("Ann�es","Pentades","Prop.jpluie","Prop.jsec")
  # Calcul des proportions moyennes par pentade
  
  prop_sec.moy<-tapply(details.prop[,4],as.factor(details.prop[,2]), mean)
  prop_humide.moy<-tapply(details.prop[,3],as.factor(details.prop[,2]), mean)
  
  final.prop<-data.frame(prop_sec.moy,prop_humide.moy)
  
  # Exportation des r�sultats
  chemin.details<-paste(paste(dest,paste("Prop.details",station,start,end,sep = "_"),sep="/"),".txt",sep="")
  chemin.final<-paste(paste(dest,paste("Prop.finalles",station,start,end,sep = "_"),sep="/"),".txt",sep="")
  write.table(details.prop,chemin.details,sep = "\t",dec=".",row.names = F)
  write.table(final.prop,chemin.final,sep = "\t",dec=".",row.names = F)
 
  # Graphiques
  nomstation<-paste("Station: ",station)
  plot(1:73,prop_sec.moy,type = "b",pch=0,col="gray30",main=nomstation,xlab="Pentades",ylab = "Proportions"
       ,sub=paste(start,end,sep=":"),ylim=c(0,1.6),col.sub="blue1")
  lines(1:73,prop_humide.moy,type = "b",pch=19,col="black")
  legend("top",legend = c("Proportions jours secs","Proportions jours pluvieux"),pch=c(0,19),col=c("gray30","black"),lty = 1)
}