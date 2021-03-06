## Exemple de proc�dure
# Il faut au pr�alable charg� les fichiers exemples 
# et les codes dans R
f.b<-fichier_exemple_journalier
f.b.an<-fichier_exemple_annuel
# analyse climatique globale sur la p�riode 1971-2014
# avec r�sultats sur le disque C
tropiclim(Date=f.b$annees,tmin = f.b$tmin,tmax = f.b$tmax,
          prec = f.b$prec,dest = "C:",
          start = 1971,end = 2014,stationname = "Bohicon")

# analyse pluviom�trique 
varpluie(f.b.an,annee = f.b.an$annees,pluviometrie = f.b.an$cumul_pluie_annuelle,stationame = "Bohicon",n=5)

# analyse de temp�rature
vartemp(f.b.an,annee = f.b.an$annees,temperature =  f.b.an$tmax_moy_an,stationame = "Bohicon",n=5)

# analyse des saisons m�thode Erpicum sur la p�riode 1995-2002 et r�sultats dans le disque C
assogba.erpicum(x=f.b[,c("annees","prec")],start = 1995,end = 2002,station = "Bohicon",dest = "C:")

# analyse des saisons m�thode Stern sur la p�riode 1995-2002 et r�sultats dans le disque C
assogba.stern(x=f.b[,c("annees","prec")],start = 1995,end = 2002,station = "Bohicon",dest = "C:")
