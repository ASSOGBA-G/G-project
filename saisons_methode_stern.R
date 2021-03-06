## Cette fonction est une application de la m�thode d�crite par Stern et al.,1981
## pour l'identification du d�but et de la fin des saisons pluvieuses
## Selon Stern et al., (1981) la saison des pluies commence
## lorsque 20 mm de pluie sont recueillis en deux jours cons�cutifs
## sans p�riode s�che sup�rieure � dix jours dans les trente jours qui suivent,
## et prend fin lorsqu'il n'y a plus de pluie durant une p�riode de quinze jours.
## le terme pentade utilis� d�signe une p�riode 5 jours
## Il prend comme principal argument une colonne de date et une autre contenant les
## pluies journali�res
assogba.stern<-function(x,start,end,station,dest){
  
  # x est un tableau dont la premi�re colonne conient les dates
  # et la seconde les pluies journali�res, start=ann�e de d�part
  # et end ann�e de fin, dest= chemin d'acc�es au dossier contenant les r�sultats
  
  # chargement du package lubridate
  library(lubridate)
  
  # S�lection des donn�es
  
  x<-subset(x,subset = as.numeric(substr(x[,1],1,4))>=start & as.numeric(substr(x[,1],1,4)<=end))
  
  
  # Initialisations et cr�tion des vecteurs r�sultats et remplacement des NA
  
  nj<-length(x[,1])
  xx<-x
  xx2<-xx[,2]
  xx2[xx2%in%NA]<-rep(999,length(xx2[xx2%in%NA]))
  xx[,2]<-xx2
  t.dest<-x
  t.dest[,2]<-rep(0,length(x[,2]))
  n.annee<-end-start+1
  annee<-start:end
  
  # supposons que jusqu'� 4 saisons pluvieuses peuvent �tre d�tect�es
  # en raison des faux d�parts dans l'installation des pluies
  
  n4 <-4*n.annee
  debut.saison<-rep(NA,n4)
  fin.saison<-rep(NA,n4)
  an.repeat<-rep(NA,n4)
  i<-1
  j<-1
  e<-1
  
  
  
  # Transformation des s�quences s�ches (pluviom�trie < 1 mm) 
  # de plus de 10 jours en s�ries constitu�es du chiffre 1
  
  while (i<=nj) {
    
    n.jsec<-0
    
    while (xx[j,2]<1) {
      
      n.jsec<-n.jsec+1
      j<-j+1
      
      if (j>nj) {
        break
        }
    }
    
    if (n.jsec>=10){
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
  
  

  
 # R�partition des donn�es par ann�e et application de la m�thode
  
  for (h in 1:n.annee){
    i<-1
    year.data<-x[as.numeric(substr(x[,1],1,4))%in%annee[h],]
    year.data.sec<-t.dest[as.numeric(substr(t.dest[,1],1,4))%in%annee[h],]
    nj.an<-length(year.data[,1])
    year.data<-data.frame(year.data,1:nj.an)
    
    # Application de la m�thode de Stern et al.,1981 aux donn�es annuelles
    
    
    
    m<-i+1
    while (m<nj.an) {
      
      g<-i+1
      som<-sum(year.data[i,2],year.data[g,2],na.rm = T)

      if (som>=20){
        
        f<-g+29
        
        if (f<nj.an){
          trente<-year.data.sec[g:f,2]
          long<-length(trente[trente%in%1])
          
       
         if (long<=10){
          
           debut.saison[e]<-as.character(year.data[i,1])
           
           an.repeat[e]<-annee[h]
          
           d<-i+30
           c<-d+14
           while (c<=nj.an) {
             
             
             sec<-year.data.sec[d:c,2]
             long.sec<-length(sec[sec%in%1])
             
             
             if (long.sec<15){
               d<-d+1
               c<-d+14 
             }
             if (long.sec>=15) {
               fin.saison[e]<-as.character(year.data[d,1])
               
               c<-c+1
               d<-d+1
               e<-e+1
               break
             }
             
             
             
           }
           
           i<-c  
           m<-i+1
         }
        
        if (long>10){
          i<-i+1
          m<-i+1
        }
        }
        
        if (f>=nj.an){
          i<-i+1
          m<-i+1
        }
      }
      
      if (som<20){
        i<-i+1
        m<-i+1
      }
      
    }
    
    
    
  }
  
  debut.saison<-data.frame(debut.saison,yday(debut.saison))
  
  fin.saison<-data.frame(fin.saison,yday(fin.saison))
  pentade.fin<-rep(NA,length(fin.saison[,2]))
  pentade.debut<-rep(NA,length(debut.saison[,2]))
  f2<-fin.saison[,2]
  d2<-debut.saison[,2]
  
  # D�termination des pentades correspondantes aux dates identifi�es
  
  for (i in 1:length(fin.saison[,2])){
    if(is.na(f2[i])!=TRUE){
  if (f2[i]%%5>0){
    pentade.fin[i]<-as.integer(f2[i]/5)+1
  }
  else{
    pentade.fin[i]<-as.integer(f2[i]/5)
  }
      
      
    }
    
    
    if(is.na(d2[i])!=TRUE){
      if (d2[i]%%5>0){
        pentade.debut[i]<-as.integer(d2[i]/5)+1
      }
      else{
        pentade.debut[i]<-as.integer(d2[i]/5)
      }
      
      
    }
  }
  
  # Constitution du tableau final
  
  e<-e-1
  debut.saison<-data.frame(an.repeat,debut.saison,pentade.debut)
  debut.saison<-debut.saison[1:e,]
  fin.saison<-data.frame(an.repeat,fin.saison,pentade.fin)
  fin.saison<-fin.saison[1:e,]
  colnames(debut.saison)<-c("Ann�es","Dates de d�but","Jour","Pentades")
  colnames(fin.saison)<-c("Ann�es","Dates de fin","Jour","Pentades")
  
 
 # Exportation des r�sultats
  
  chemin.saison.debut<-paste(paste(dest,station,sep="/"),"Dates_debut",start,end,"txt",sep=".")
  chemin.saison.fin<-paste(paste(dest,station,sep="/"),"Dates_fin",start,end,"txt",sep=".")
  
  
  write.table(debut.saison,chemin.saison.debut,sep="\t",dec=".",row.names = F)
  write.table(fin.saison,chemin.saison.fin,sep="\t",dec=".",row.names = F)

  # Graphiques
  
  plot(debut.saison$Ann�es,debut.saison$Pentades,col="black",pch=19,sub=paste("Station", station,sep=":"),
       xlab="Ann�es",ylab="Pentades",main="Date de d�marrage de la saison pluvieuse",col.sub="blue1",lwd=2)

  plot(fin.saison$Ann�es,fin.saison$Pentades,col="black",pch=18,sub=paste("Station", station,sep=":"),
       xlab="Ann�es",ylab="Pentades",main="Date de fin de la saison pluvieuse",col.sub="blue1",lwd=2)
  
}