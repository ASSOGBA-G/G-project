## Cette fonction permet de faire l'analyse de la variabilit�
## des pluies et temp�ratures en g�n�ral
tropiclim<-function(Date,tmin,tmax,prec,start,end,stationname,dest){
  
  x<-data.frame(Date,tmin,tmax,prec)
  colnames(x)<-c("Date","tmin","tmax","prec")
  x<-subset(x,subset = as.numeric(substr(x$Date,1,4))>=start&as.numeric(substr(x$Date,1,4))<=end)

# Chargement des packages n�cessaires

library(iki.dataclim)
library(Kendall)
library(moments)

# Cr�ation d'un jeu de donn�es climatique et calcul des param�tres
# mensuels

clim.dat<-createDataclim(date=as.Date(x$Date),tmin=x$tmin,tmax=x$tmax,prec=x$prec,basePeriod = start:end)
rclim<-summary(clim.dat)
month.mean<-cbind(month.name,rclim$annualCycleAvg)
month.min<-cbind(month.name,rclim$annualCycleMin)
month.max<-cbind(month.name,rclim$annualCycleMax)
month.trend<-cbind(month.name,rclim$monthlyTrends$trend)
month.pval<-cbind(month.name,rclim$monthlyTrends$pval)
all.data<-data.frame(clim.dat@data)
all.data<-data.frame(row.names(all.data),all.data)
colnames(all.data)<-c("Date","tmin","tmax","prec")

# Calcul des indices
clim.climdex<-createClimdex(clim.dat, basePeriod = start:end)
clim.indices<-computeClimdex(clim.climdex)
annual.indices<-clim.indices$annual.climdex
month.indices<-clim.indices$monthly.climdex


# Diagramme ombro-thermique
plotWalterLieth(clim.dat,station = stationname)

# Donn�es annuelles
pan<-tapply(x$prec,substr(x$Date,1,4),sum,na.rm=T)
tmax_an<-tapply(x$tmax,substr(x$Date,1,4),mean,na.rm=T)
tmin_an<-tapply(x$tmin,substr(x$Date,1,4),mean,na.rm=T)
pan<-matrix(pan,ncol=1)
tmax_an<-matrix(tmax_an,ncol=1)
tmin_an<-matrix(tmin_an,ncol=1)
pan<-ts(pan,start = start,end = end,frequency = 1)
tmax_an<-ts(tmax_an,start = start,end = end,frequency = 1)
tmin_an<-ts(tmin_an,start = start,end = end,frequency = 1)


# Statistiques descriptives(pluies, tmax et tmin)
resume<-matrix(c(pan,tmax_an,tmin_an),ncol = 3)
moyenne<-apply(resume,2,mean,na.rm=T)
ecart_type<-apply(resume,2,sd,na.rm=T)
coef_variation<-ecart_type/moyenne
coef_asymetrie<-apply(resume,2,skewness,na.rm=T)
coef_aplatissement<-apply(resume,2,kurtosis,na.rm=T)
variables<-c("Pluie","Tmax","Tmin")
resume<-data.frame(variables,moyenne,ecart_type,coef_variation,coef_asymetrie,coef_aplatissement)


datclim<-cbind(start:end,pan,tmax_an,tmin_an)
colnames(datclim)<-c("annees","cumul_pluie_annuelle","tmax_moy_an","tmin_moy_an")



year.data<-split(all.data[,-1],as.numeric(substr(all.data[,1],1,4)))

o<-length(year.data)

# La variable miss contient les
# valeurs manquantes par ann�e exprim�es en %

miss<-matrix(rep(1:o,3),ncol=3,dimnames = list(c(start:end),c("tmin%","tmax%","prec%")))
for (j in 1:o){
  
  donnees<-year.data[[j]]
  m.tmin<-donnees[donnees$tmin%in%NA,1]
  m.tmax<-donnees[donnees$tmax%in%NA,2]
  m.prec<-donnees[donnees$prec%in%NA,3]
  
  n1<-100*round(length(m.tmin)/length(donnees[,1]),2)
  n2<-100*round(length(m.tmax)/length(donnees[,2]),2)
  n3<-100*round(length(m.prec)/length(donnees[,3]),2)
  
  miss[j,1]<-n1
  miss[j,2]<-n2
  miss[j,3]<-n3
}
miss<-cbind(start:end,miss)
colnames(miss)[1]<-"Annees"



## Exportation des fichiers
resultname<-c("statistiques_descriptives.txt","donnees_annuelles.txt","moyennes_mensuelles.txt","minima_mensuels.txt",
              "maxima_mensuels.txt","tendances_mensuelles.txt","pvalue_tendances_mensuels.txt","indices_annuels.txt",
              "indices_mensuels.txt","donnees_journalieres.txt","donnees_manquantes.txt")
resultdata<-list(resume,datclim,month.mean,month.min,month.max,month.trend
              ,month.pval,annual.indices,month.indices,all.data,miss)

nresult<-length(resultname)

for (l in 1:nresult){
  
  export<-paste(dest,paste(stationname,resultname[l],sep = "_"),sep = "/")
  write.table(resultdata[[l]],file=export,sep="\t",dec=".",row.names = F)
}


## Rupture, tendance et anomalies

for (j in 1:3) {
  titre<-c("Pluviom�trie","Temperature maximale moyenne","Temperature minimale moyenne")
  variable<-cbind(pan,tmax_an,tmin_an)
  variable<-variable[,j]
  titre<-titre[j]
  titr<-paste("Variabilit� inter-annuelle : ",stationname,sep=" ")
  plot(start:end,variable,type="b",pch=18,col="blue",lwd=1,xlab="Ann�es",ylab=titre,ylim = c(min(variable),max(variable)+2), main=titr)
  m<-mean(variable)
  m<-rep(m,length(variable))
  mts<-ts(m,start = start,end=end,frequency = 1)
  lines(mts,col="red",lwd=2)
  leg.text<-c(titre,"Moyenne")
  legend("top",legend = leg.text,col = c("blue","red"),pch =c(18,NA),lty = c(1,1), lwd=c(1,2))
  
  # Anomalies centr�es r�duites + moyenne mobile
  
  u<-(variable-m)/sd(variable)
  nex<-0
  ndef<-0
  t<-length(u)
  for (k in 1 : t){ if (u[k]>0) {nex=nex+1} else {ndef=ndef+1}}
  nex
  ndef
  u<-ts(u,start= start,end=end,frequency=1)
  titr<-paste("Anomalies centr�es r�duites : ",stationname,sep=" ")
  
  # Graphiques Anomalies centr�es r�duites + moyenne mobile
  
  plot(u,type="h",xlab="Ann�es",ylab=titre,lwd=2,col="blue",ylim=c(min(u),max(u)+2),main=titr)
  mm<-filter(u,filter=rep((1/5),5),sides=2)
  mm<-ts(mm,start= start,end=end,frequency=1)
  lines(mm,col="red",lwd=2)
  legend("top",legend = c("Valeurs centr�es r�duites","Moyenne mobile"),col = c("blue","red"),lty = c(1,1),lwd=c(2,2) )
  
  ## Test de tendance
  
  tendance<-MannKendall(variable)
  tendance<-cbind(tendance$tau[1],tendance$sl[1])
  colnames(tendance)<-c("Stat_kendall","Pvalue_kendall")
  
  ## Tests de rupture
  
  snt<-SNHtest(variable)
  bht<-BHRtest(variable)
  ptt<-PETtest(variable)
  vt<-VONtest(variable)
  
  rupt_snt<-start+snt$breakpoint-1
  rupt_bht<-start+bht$breakpoint-1
  rupt_ptt<-start+ptt$breakpoint-1
  
  snt<-data.frame(snt[c(1,3)],rupt_snt)
  colnames(snt)<-c("stat_SNH","significativit�_SNH","annee_rupture_SNH")
  bht<-data.frame(bht[c(1,3)],rupt_bht)
  colnames(bht)<-c("stat_Buishand","significativit�_Buishand","annee_rupture_Buishand")
  ptt<-data.frame(ptt[c(1,3)],rupt_ptt)
  colnames(ptt)<-c("stat_Pettitt","significativit�_Pettitt","annee_rupture_Pettitt")
  vt<-data.frame(vt[c(1,3)])
  colnames(vt)<-c("stat_VonNeumann","significativit�_VonNeumann")
  
  rupture<-cbind(snt,bht,ptt,vt)
  variability<-cbind(nex,ndef,tendance,rupture)
  colnames(variability)[c(1,2)]<-c("Ann�es exc�dentaires","Ann�es d�ficitaires")
  rownames(variability)<-titre
  obs<-paste(dest,paste(stationname,"tendance_rupture.txt",sep="_"),sep="/")
  write.table(variability,file=obs,sep="\t",dec = ".",row.names = TRUE,append = TRUE)
}

}