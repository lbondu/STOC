###########################################################################################
#                                                                                         #
#  Ce script sert à préparer les jeux de données qui seront utilisés pour les analyses    #
#                                                                                         #
###########################################################################################


#Charger les packages R utiles pour la suite ----------------------

library(dplyr) #permet d'utiliser %>%, filter, summarize et bien d'autres.
library(sf) #pour manipuler des objets spatiaux
library(ggplot2)  #pour les graphiques

linotte.bret = data.bret %>% filter(species=='Linaria cannabina')
alouette.bret = data.bret %>% filter(species=='Alauda arvensis')
charbo.bret = data.bret %>% filter(species=='Parus major')
bruantj.bret = data.bret %>% filter(species=='Emberiza citrinella')
tourt.bret = data.bret %>% filter(species=='Streptopelia turtur')
linotte.bret = data.bret %>% filter(species=='Linaria cannabina')
merle.bret = data.bret %>%  filter(species=='Turdus merula')

#importer les données STOC
mydat = read.csv2("data/0041794-241126133413365.csv", sep = '\t') # note pour Océane: pense à mettre le fichier de données dans le dossier STOC_Bret/data !

data.bret = read.csv2("data/data_bret.csv")
charbo.bret = read.csv2("data/charbo.bret.csv")
alouette.bret = read.csv2("data/alouette_bret.csv")
alouette_bret = read.csv2("data/alouette_bret.csv")

tourt.bret = read.csv2("data/tourt.bret.csv")
bruantj.bret = read.csv2("data/bruantj.bret.csv")

fauvette.bret = read.csv2("data/fauvette.bret.csv")
linotte.bret = read.csv2("data/linotte.bret.csv")

#stocker dans une variable les données concernant uniquement les espèces spécialistes : alouette des champs et bruant jaune
alouette = mydat %>%  filter(species == "Alauda arvensis")
bruantj = mydat %>% filter(species == "Emberiza citrinella")

#idem pour les espèces généralistes tourterelle des bois et mésange charbonnière
tourt = mydat %>% filter(species == "Streptopelia turtur")
charbo = mydat %>% filter(species == "Parus major")

fauvette = mydat %>% filter(species == "Linaria cannabina")
linotte = mydat %>% filter(species == "Sylvia commnuis")
merle = mydat %>% filter(species == "Turdus merula")

#importer une carte de l'Ille et Vilaine (polygone au format shapefile, donc un objet spatial)
#pour les objets spatiaux utiliser st_read
dep35 <- st_read("data/departement-35/admin-departement.shp")


# Représenter les points pour l'alouette des champs -----------------------

#convertir les donées STOC qui contiennent déjà des colonnes Longitude et Latitude
#en objet spatial aussi (les données Lon Lat sont reconnues grâce au système de coordonnées)
alouette2 = st_as_sf(alouette,coords = c("decimalLongitude","decimalLatitude"), crs = 4326)
#le crs est est le "Coordinate Reference System", ici WGS84, système géodésique mondial
plot(st_geometry(alouette)) #représente toutes les observations de l'alouette en France, selon leurs coordonnées (st_geometry pour ne représenter les points qu'une fois, selon la colonne geometry uniquement)


#représenter les observations d'alouettes en Bretagne
ggplot() + 
  geom_sf(data = dep35) + #trace le contour du département 35
  geom_sf(data = alouette) + #ajoute les données d'observation d'alouettes
  coord_sf(xlim = c(-6,0), ylim = c(46,49.5), expand = FALSE) #restreint ce qu'on voit de ces données à une zone en Bretagne (grâce aux degrés N et W)

#représenter uniquement les observations en Ille-et-Vilaine
#faire une intersection spatiale : extraire uniquement les points qui se trouvent dans le polygone d'intérêt (ici dpt 35)
alouette_in_dep35 <- st_intersection(alouette, dep35)

#Tracer
ggplot() + 
  geom_sf(data = dep35, fill = "white", color = "black") + # Contour du département35
  geom_sf(data = alouette_in_dep35, color = "blue", size = 1) + # Observations d'alouettes extraites
  theme_minimal() + #juste pour l'esthétique
  ggtitle("Observations (STOP-eps) d'Alouettes des champs en Ille-et-Vilaine") #ajout d'un titre

#Linaria cannabina linotte mélodieuse
linotte = mydat %>%  filter(species == "Linaria cannabina")
linotte.bret = data.bret %>% filter(species == "Linaria cannabina")
#convertir les donées STOC qui contiennent déjà des colonnes Longitude et Latitude
#en objet spatial aussi (les données Lon Lat sont reconnues grâce au système de coordonnées)
linotte = st_as_sf(linotte,coords = c("decimalLongitude","decimalLatitude"), crs = 4326)
#le crs est est le "Coordinate Reference System", ici WGS84, système géodésique mondial
plot(st_geometry(linotte)) #représente toutes les observations de l'linotte en France, selon leurs coordonnées (st_geometry pour ne représenter les points qu'une fois, selon la colonne geometry uniquement)
linotte.bret=st_as_sf(linotte.bret,coords = c("decimalLongitude","decimalLatitude"), crs = 4326)

#représenter les observations d'alouettes en Bretagne
ggplot() + 
  geom_sf(data = dep35) + #trace le contour du département 35
  geom_sf(data = linotte) + #ajoute les données d'observation d'alouettes
  coord_sf(xlim = c(-6,0), ylim = c(46,49.5), expand = FALSE) 

#Linaria cannabina linotte mélodieuse
fauvette = mydat %>%  filter(species == "Sylvia communis")
fauvette.bret = data.bret %>% filter(species=="Sylvia communis")
#convertir les donées STOC qui contiennent déjà des colonnes Longitude et Latitude
#en objet spatial aussi (les données Lon Lat sont reconnues grâce au système de coordonnées)
fauvette = st_as_sf(fauvette,coords = c("decimalLongitude","decimalLatitude"), crs = 4326)
#le crs est est le "Coordinate Reference System", ici WGS84, système géodésique mondial
plot(st_geometry(fauvette)) #représente toutes les observations de l'fauvette en France, selon leurs coordonnées (st_geometry pour ne représenter les points qu'une fois, selon la colonne geometry uniquement)
fauvette.brett = st_as_sf(fauvette.bret,coords = c("decimalLongitude","decimalLatitude"), crs = 4326)


#représenter les observations d'alouettes en Bretagne
ggplot() + 
  geom_sf(data = dep35) + #trace le contour du département 35
  geom_sf(data = fauvette) + #ajoute les données d'observation d'alouettes
  coord_sf(xlim = c(-6,0), ylim = c(46,49.5), expand = FALSE) 


#Linaria cannabina linotte mélodieuse
tarier = mydat %>%  filter(species == "Saxicola rubetra")
tarier.bret = data.bret %>% filter(species == "Saxicola ruberta")
#convertir les donées STOC qui contiennent déjà des colonnes Longitude et Latitude
#en objet spatial aussi (les données Lon Lat sont reconnues grâce au système de coordonnées)
tarier = st_as_sf(tarier,coords = c("decimalLongitude","decimalLatitude"), crs = 4326)
#le crs est est le "Coordinate Reference System", ici WGS84, système géodésique mondial
plot(st_geometry(tarier)) #représente toutes les observations de l'fauvette en France, selon leurs coordonnées (st_geometry pour ne représenter les points qu'une fois, selon la colonne geometry uniquement)
tarier.bret = st_as_sf(tarier.bret,coords = c("decimalLongitude","decimalLatitude"), crs = 4326)

#représenter les observations d'alouettes en Bretagne
ggplot() + 
  geom_sf(data = dep35) + #trace le contour du département 35
  geom_sf(data = tarier) + #ajoute les données d'observation d'alouettes
  coord_sf(xlim = c(-6,0), ylim = c(46,49.5), expand = FALSE) 


#passer montanus moineau friquet
moinfri = mydat %>%  filter(species == "Passer montanus")

linotte.bret = data.bret %>% filter(species=='Linaria cannabina')
#convertir les donées STOC qui contiennent déjà des colonnes Longitude et Latitude
#en objet spatial aussi (les données Lon Lat sont reconnues grâce au système de coordonnées)
moinfri = st_as_sf(moinfri,coords = c("decimalLongitude","decimalLatitude"), crs = 4326)
#le crs est est le "Coordinate Reference System", ici WGS84, système géodésique mondial
plot(st_geometry(moinfri)) #représente toutes les observations de l'moinfri en France, selon leurs coordonnées (st_geometry pour ne représenter les points qu'une fois, selon la colonne geometry uniquement)


#représenter les observations d'alouettes en Bretagne
ggplot() + 
  geom_sf(data = dep35) + #trace le contour du département 35
  geom_sf(data = moinfri) + #ajoute les données d'observation d'alouettes
  coord_sf(xlim = c(-6,0), ylim = c(46,49.5), expand = FALSE) 


# Même chose avec les autres espèces --------------------------------------

#conversion en spatial
bruantj = st_as_sf(bruantj,coords = c("decimalLongitude","decimalLatitude"), crs = 4326)
tourt = st_as_sf(tourt,coords = c("decimalLongitude","decimalLatitude"), crs = 4326)
charbo = st_as_sf(charbo,coords = c("decimalLongitude","decimalLatitude"), crs = 4326)

#représenter toutes les observations en Bretagne
ggplot() + 
  geom_sf(data = Bret) + #trace le contour du département 35
  geom_sf(data= charbo.bret, color = "purple")+
  geom_sf(data = alouette.bret, color = "red") + #ajoute les données d'observation d'alouette 
  geom_sf(data= bruantj.bret, color = "yellow")+
  geom_sf(data= tourt.bret, color = "blue")+
  geom_sf(data= linotte.bret, color = "green")+
  geom_sf(data= fauvette.bret, color = "pink")+
  coord_sf(xlim = c(-6,0), ylim = c(46,49.5), expand = FALSE) #restreint ce qu'on voit de ces données à une zone en Bretagne

#extraire les données en Ille-et-Vilaine
bruantj_in_dep35 <- st_intersection(bruantj, dep35)
tourt_in_dep35 <- st_intersection(tourt, dep35)
charbo_in_dep35 <- st_intersection(charbo, dep35)

#représneter
ggplot() + 
  geom_sf(data = Bret, fill = "white", color = "black") + # Contour du département
  geom_sf(data = charbo.bret, aes(color = "Great tit"), size = 1) + # Points pour mésange charbonnière
  geom_sf(data = merle.bret, aes(color = "Common blackbird"), size = 1)+
  geom_sf(data = alouette.bret, aes(color = "Eurasian skylark"), size = 1) + # Points pour alouette
  geom_sf(data = bruantj.bret, aes(color = "Yellowhamer"), size = 1) + # Points pour bruant jaune
  geom_sf(data = tourt.bret, aes(color = "European turtle dove"), size = 1)+
  geom_sf(data = linotte.bret, aes(color = "Common linnet"), size = 1)+
  geom_sf(data = fauvette.bret, aes(color = "Common whitethroat"), size = 1)+ # Points pour tourterelle
  scale_color_manual(values = c("Great tit" = "purple", #pour ajouter une légende manuellement
                                "Eurasian skylark" = "red", 
                                "Yellowhamer" = "orange", 
                                "European turtle dove" = "blue",
                                "Common linnet" = "green", 
                                "Common whitethroat" = "brown",
                                "Common blackbird" = "coral" )) + # Couleurs spécifiques pour chaque espèce
  theme_minimal() + #pour l'esthétique
  ggtitle("STOC programm observations in Brittany")+
  theme(legend.title = element_blank())+
  theme(plot.title = element_text(hjust = 0.8))# Enlever le titre de la légende



# Créer un polygone Bretagne --------------------------------------

#en rassemblant les différents polygines des départements
dep35 <- st_read("data/departement-35/admin-departement.shp")
dep22 <- st_read("data/departement-22/admin-departement.shp")
dep56 <- st_read("data/departement-56/admin-departement.shp")
dep29 <- st_read("data/departement-29/admin-departement.shp")

#rassembler les polygones deux à deux pour obtenir un seul polygone
dep35_22 = st_union(dep35,dep22)
plot(st_geometry(dep35_22))

dep56_29 = st_union(dep56,dep29)
plot(st_geometry(dep56_29))

#rassembler les deux polygones qui en résultent pour obtenir la Bretagne
Bret = st_union(dep35_22,dep56_29)
plot(st_geometry(Bret))


# Extraire les observations bretonnes  --------------------------------------

#pour l'alouette
alouette.bret = st_intersection(alouette,Bret)
ggplot() + 
  geom_sf(data = Bret, fill = "white", color = "black") + # Contour du département35
  geom_sf(data = alouette.bret, color = "blue", size = 1) + # Observations d'alouettes extraites
  theme_minimal() + #juste pour l'esthétique
  ggtitle("Observations (STOP-eps) d'alouettes des champs en Bretagne") #ajout d'un titre

#pour le bruant
bruantj.bret = st_intersection(bruantj,Bret)
ggplot() + 
  geom_sf(data = Bret, fill = "white", color = "black") + # Contour du département35
  geom_sf(data = bruantj.bret, color = "blue", size = 1) + # Observations d'alouettes extraites
  theme_minimal() + #juste pour l'esthétique
  ggtitle("Observations (STOP-eps) de Bruants jaunes en Bretagne") #ajout d'un titre

#pour la mésange
charbo.bret = st_intersection(charbo,Bret)
ggplot() + 
  geom_sf(data = Bret, fill = "white", color = "black") + # Contour du département35
  geom_sf(data = charbo.bret, color = "blue", size = 1) + # Observations d'alouettes extraites
  theme_minimal() + #juste pour l'esthétique
  ggtitle("Observations (STOP-eps) de mesange charbonnières en Bretagne") #ajout d'un titre

# Dataset avec toutes les espèces en Bretagne

data.bret = mydat %>% 
  st_as_sf(coords = c("decimalLongitude","decimalLatitude"), crs = 4326) %>% 
  st_intersection(Bret)

#Sauvegarder les données dans les documents
saveRDS(data.bret, "data/data.bret.RDS") 
#format utile en programmation R car compresse les données (prend moins de place), conserve les métadonnées (le type des données, classes, attributs...) et s'ouvre facilement
write.csv2(data.bret, "data/data.bret.csv") #pour ouvrir les données sous excel

saveRDS(alouette.bret, "data/alouette.bret.RDS")
write.csv2(alouette.bret, "data/alouette.bret.csv")

saveRDS(bruantj.bret, "data/bruantj.bret.RDS")
write.csv2(bruantj.bret, "data/bruantj.bret.csv")

saveRDS(linotte.bret, "data/linotte.bret.RDS")
write.csv2(linotte.bret, "data/linotte.bret.csv")

saveRDS(fauvette.bret, "data/fauvette.bret.RDS")
write.csv2(fauvette.bret, "data/fauvette.bret.csv")
write.csv2(charbo.bret, "data/charbo.bret.csv")
write.csv2(merle.bret, "data/merle.bret.csv")
write.csv2(tourt.bret, "data/tourt.bret.csv")

write.csv2(fauvette, "data/fauvette.csv")
write.csv2(alouette, "data/alouette.csv")
write.csv2(merle, "data/merle.csv")
write.csv2(linotte, "data/linotte.csv")
write.csv2(tourt, "data/tourt.csv")
write.csv2(charbo, "data/charbo.csv")
write.csv2(bruantj, "data/bruantj.csv")

write.csv2(ratio.tablLi, "data/ratio.tablLi.csv")
write.csv2(ratio.tabllinotteNat, "data/ratio.tabllinotteNat.csv")
write.csv2(ratio.merle, "data/ratio.merle.csv")
write.csv2(ratio.tablmerleNat, "data/ratio.tablmerleNat.csv")
write.csv2(ratio.alouettenbobse, "data/ratio.alouettenbobse.csv")
write.csv2(ratio.tablAlNat, "data/ratio.tablAlNat.csv")
write.csv2(ratio.BJobsparotot, "data/ratio.BJobsparotot.csv")
write.csv2(ratio.tablBJNat, "data/ratio.tablBJNat.csv")
write.csv2(ratio.tablFa, "data/ratio.tablFa.csv")
write.csv2(ratio.tablFaNat, "data/ratio.tablFaNat.csv")
write.csv2(ratio.tablCh, "data/ratio.tablCh.csv")
write.csv2(ratio.tablcharboNat, "data/ratio.tablcharboNat.csv")
write.csv2(ratio.tablTo, "data/ratio.tablTo.csv")
write.csv2(ratio.tabltourtNat, "data/ratio.tabltourtNat.csv")

write.csv2(sum.obs, "data/sum.obs.csv")
write.csv2(sum.observateurs, "data/sum.observateurs.csv")
write.csv2(sum.bret, "data/sum.bret.csv")
write.csv2(sum.obsNat, "data/sum.obsNat.csv")


#linotte et fauvette

# Pour le fun : créer une carte interactive -------------------------------

#nécessite un nouveau package
library(tmap)

#définir le mode interactif
tmap_mode("view")

tm_shape(charbo.bret) + 
  tm_dots(col = "grey", size = 0.1)+
  tm_shape(bruantj.bret) + 
  tm_dots(col = "yellow", size = 0.1)+ #trace le spoints, size définit leur taille
  tm_shape(alouette.bret) + 
  tm_dots(col = "brown", size = 0.1)+
  tm_layout(title = "Observations des espèces (STOC-EPS) en Bretagne",
            legend.show = TRUE,
            legend.outside = TRUE)




