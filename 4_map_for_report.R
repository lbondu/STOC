###########################################################################################
#                                                                                         #
#  Ce script sert à produire des cartes pour le rapport                                   #
#                                                                                         #
###########################################################################################


library(dplyr) #permet d'utiliser %>%, filter, summarize et bien d'autres.
library(sf) #pour manipuler des objets spatiaux
library(ggplot2)  #pour les graphiques


# ici mettre votre chemin d'accès jusqu'au dossier où se trouvent les fichiers .RDS 
# (penser à changer les \ en / )

mypath = "C:/Users/fameline/OneDrive - ENS RENNES/Documents/CPES/L2 24-25/suivi oiseaux communs/STOC_Bret/data"


#ré-importer les jeux de données enregistrés
alouette.bret = readRDS(paste0(mypath,"/alouette.bret.RDS"))
bruantj.bret = readRDS( paste0(mypath,"/bruantj.bret.RDS"))
charbo.bret = readRDS(paste0(mypath,"/charbo.bret.RDS"))
tourt.bret = readRDS( paste0(mypath,"/tourt.bret.RDS"))

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

alouette.bret = st_as_sf(alouette.bret,coords = geometry, crs = 4326)

#conversion - regelment de pb
alouette.bret <- alouette.bret %>%
  mutate(
    lon = as.numeric(sub("c\\(([^,]+),.*", "\\1", geometry)),
    lat = as.numeric(sub("c\\([^,]+,\\s*([^\\)]+)\\)", "\\1", geometry))
  )

# Étape 2 : Convertir en objet sf
alouette.bret <- st_as_sf(alouette.bret, coords = c("lon", "lat"), crs = 4326)

#conversion - regelment de pb
charbo.bret <- charbo.bret %>%
  mutate(
    lon = as.numeric(sub("c\\(([^,]+),.*", "\\1", geometry)),
    lat = as.numeric(sub("c\\([^,]+,\\s*([^\\)]+)\\)", "\\1", geometry))
  )

# Étape 2 : Convertir en objet sf
charbo.bret <- st_as_sf(charbo.bret, coords = c("lon", "lat"), crs = 4326)

#conversion - regelment de pb
bruantj.bret <- bruantj.bret %>%
  mutate(
    lon = as.numeric(sub("c\\(([^,]+),.*", "\\1", geometry)),
    lat = as.numeric(sub("c\\([^,]+,\\s*([^\\)]+)\\)", "\\1", geometry))
  )

# Étape 2 : Convertir en objet sf
bruantj.bret <- st_as_sf(bruantj.bret, coords = c("lon", "lat"), crs = 4326)

#conversion - regelment de pb
tourt.bret <- tourt.bret %>%
  mutate(
    lon = as.numeric(sub("c\\(([^,]+),.*", "\\1", geometry)),
    lat = as.numeric(sub("c\\([^,]+,\\s*([^\\)]+)\\)", "\\1", geometry))
  )

# Étape 2 : Convertir en objet sf
tourt.bret <- st_as_sf(tourt.bret, coords = c("lon", "lat"), crs = 4326)

#conversion - regelment de pb
linotte.bret <- linotte.bret %>%
  mutate(
    lon = as.numeric(sub("c\\(([^,]+),.*", "\\1", geometry)),
    lat = as.numeric(sub("c\\([^,]+,\\s*([^\\)]+)\\)", "\\1", geometry))
  )

# Étape 2 : Convertir en objet sf
linotte.bret <- st_as_sf(linotte.bret, coords = c("lon", "lat"), crs = 4326)

#conversion - regelment de pb
fauvette.bret <- fauvette.bret %>%
  mutate(
    lon = as.numeric(sub("c\\(([^,]+),.*", "\\1", geometry)),
    lat = as.numeric(sub("c\\([^,]+,\\s*([^\\)]+)\\)", "\\1", geometry))
  )

# Étape 2 : Convertir en objet sf
fauvette.bret <- st_as_sf(fauvette.bret, coords = c("lon", "lat"), crs = 4326)

#conversion - regelment de pb
merle.bret <- merle.bret %>%
  mutate(
    lon = as.numeric(sub("c\\(([^,]+),.*", "\\1", geometry)),
    lat = as.numeric(sub("c\\([^,]+,\\s*([^\\)]+)\\)", "\\1", geometry))
  )

# Étape 2 : Convertir en objet sf
merle.bret <- st_as_sf(merle.bret, coords = c("lon", "lat"), crs = 4326)



# carte des observations d'alouettes

ggplot() + 
  geom_sf(data = Bret, fill = "white", color = "black") + # Contour du département35
  geom_sf(data = alouette.bret, color = "blue", size = 1, alpha = 0.2) + # Observations d'alouettes extraites
  theme_minimal() + #juste pour l'esthétique
  ggtitle("Observations (STOC-eps) d'alouettes des champs en Bretagne") #ajout d'un titre


# couleur par année
ggplot() + 
  geom_sf(data = Bret, fill = "white", color = "black") + # Contour du département35
  geom_sf(data = alouette.bret, aes(color = year), size = 1) + # Observations d'alouettes extraites
  theme_minimal() + #juste pour l'esthétique
  ggtitle("Observations (STOC-eps) d'alouettes des champs en Bretagne") #ajout d'un titre

alouette.bretGeom

# Pour le fun : créer une carte interactive -------------------------------

#nécessite un nouveau package
library(tmap)

#définir le mode interactif
tmap_mode("view")

tm_shape(charbo.bret) + 
  tm_dots(col = "purple", size = 0.1)+
  tm_shape(bruantj.bret) + 
  tm_dots(col = "orange", size = 0.1)+ #trace les points, size définit leur taille
  tm_shape(alouette.bret) + 
  tm_dots(col = "red", size = 0.1)+
  tm_shape(tourt.bret) + 
  tm_dots(col = "blue", size = 0.1)+
  tm_shape(alouette.bret) + 
  tm_dots(col = "red", size = 0.1)+
  tm_shape(linotte.bret) + 
  tm_dots(col = "green", size = 0.1)+
  tm_layout(title = "Observations des espèces (STOC-EPS) en Bretagne",
            legend.show = TRUE,
            legend.outside = TRUE)
