## Exemple de procédure
# Il faut au préalable chargé les fichiers exemples 
# et les codes dans R
f.b<-fichier_exemple_journalier
f.b.an<-fichier_exemple_annuel
# analyse climatique globale sur la période 1971-2014
# avec résultats sur le disque C
tropiclim(Date=f.b$annees,tmin = f.b$tmin,tmax = f.b$tmax,
          prec = f.b$prec,dest = "C:",
          start = 1971,end = 2014,stationname = "Bohicon")

# analyse pluviométrique 
varpluie(f.b.an,annee = f.b.an$annees,pluviometrie = f.b.an$cumul_pluie_annuelle,stationame = "Bohicon",n=5)

# analyse de température
vartemp(f.b.an,annee = f.b.an$annees,temperature =  f.b.an$tmax_moy_an,stationame = "Bohicon",n=5)

# analyse des saisons méthode Erpicum sur la période 1995-2002 et résultats dans le disque C
assogba.erpicum(x=f.b[,c("annees","prec")],start = 1995,end = 2002,station = "Bohicon",dest = "C:")

# analyse des saisons méthode Stern sur la période 1995-2002 et résultats dans le disque C
assogba.stern(x=f.b[,c("annees","prec")],start = 1995,end = 2002,station = "Bohicon",dest = "C:")
