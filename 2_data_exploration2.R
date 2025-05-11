###########################################################################################
#                                                                                         #
#  Ce script sert à faire des figures pour explorer les données                           #
#                                                                                         #
###########################################################################################


# import library and files ------------------------------------------------

library(dplyr) #permet d'utiliser %>%, filter, summarize et bien d'autres.
library(sf) #pour manipuler des objets spatiaux
library(ggplot2)  #pour les graphiques
library(ggpmisc)
library(ggpubr)
# ici mettre votre chemin d'accès jusqu'au dossier où se trouvent les fichiers .RDS 
# (penser à changer les \ en / )

mypath = "C:/Users/enfants/Documents/Loïc/CPES/2/STOC_Bret-main/data"
#mypath = "D:/Home/ocbegassat/Downloads/PIR-BREGER-BONDU-STOC/PIR-BREGER-BONDU-STOC"


#ré-importer les jeux de données enregistrés
alouette.bret = read.csv(paste0(mypath,"/alouette_bret.csv"), head = TRUE, sep=";")
#saveRDS(alouette.bret,file ="alouette_bret.RDS")
#alouette.bret = readRDS("alouette_bret.RDS")


bruantj.bret = read.csv( paste0(mypath,"/bruantj_bret.csv"), head = TRUE, sep=";")
#saveRDS(bruantj.bret,"bruantj.bret.RDS")
#bruantj.bret = readRDS("bruantj.bret.RDS")

charbo.bret = read.csv( paste0(mypath,"/charbo.bret.csv"), head = TRUE, sep=";")

tourt.bret = read.csv( paste0(mypath,"/tourt.bret.csv"), head = TRUE, sep=";")

data.bret = read.csv( paste0(mypath,"/data.bret.csv"), head = TRUE, sep=";")
data.bret = read.csv2(file = paste0(mypath,"/data.bret.csv"), head = TRUE, sep=";")
#"C:\Users\enfants\Documents\Loïc\CPES\2\STOC_Bret-main\data\data.bret.csv"
# Nombre d'observations et d'observateurs d'Alouettes ---------------------

#Somme des observateurs et observations d'alouettes par an et ratio de l'un sur l'autre
sum.alouette = alouette.bret %>% 
  st_drop_geometry() %>% #retire la partie spatial epour faciliter le traitement des données numériques
  group_by(year) %>%  #groupe par année (première colone = année)
  summarize(n.observations = n(), n.observateurs = n_distinct(recordedBy) ) %>% #crée deux nouvelles colonnes
#nombre d'observations = nombre de ligne alouette par an
#nombre d'observateurs = nombre de noms différents ayant fait ces observations d'alouettes par an
  mutate(ratio = n.observations/n.observateurs) #nouvelle colonne qui résume le ratio des deux

# représenter ce ratio selon les années
ggplot(sum.alouette) +
  geom_point(aes(x=year, y = ratio)) + theme_classic()



# Même chose pour le bruant jaune -----------------------------------------

sum.bruantj = bruantj.bret %>% 
  st_drop_geometry() %>% 
  group_by(year) %>% 
  summarize(n.observations = n(), n.observateurs = n_distinct(recordedBy) ) %>% 
  mutate(ratio = n.observations/n.observateurs)

ggplot(sum.bruantj) +
  geom_point(aes(x=year, y = ratio))

#evolution du nombre d'obervateurs par an en bretagne----------------------

sum.observateurs = data.bret %>% 
  st_drop_geometry() %>% 
  group_by(year) %>% 
  summarize( n.observateurs = n_distinct(recordedBy) ) 
 
ggplot(sum.obs) +
  geom_point(aes(x=year, y = n.observateurs)) +
  theme_classic()

#evolution du nombre d'obervations par observateurs------------------------

sum.bret = data.bret %>% 
  st_drop_geometry() %>% 
  group_by(year) %>% 
  summarize(n.observations = n(), n.observateurs = n_distinct(recordedBy) ) %>% 
  mutate(ratio = n.observations/n.observateurs)

ggplot(sum.bret) +
  geom_point(aes(x=year, y = ratio)) + theme_classic()


# evolution du nombre d'obervations par an en bretagne --------------------
sum.obs = data.bret %>% 
  st_drop_geometry() %>% 
  group_by(year) %>% 
  summarize( n.observationstot = n_distinct(occurrenceID) ) 

ggplot(sum.obs) +
  geom_point(aes(x=year, y = n.observations)) + theme_classic()

# I Nombre d'observations d'Alouettes par nombres d'obervation en bretagne---------------------

#créer une table avec le nombre d'observation par an
sum.obs = data.bret %>%
  st_drop_geometry() %>%
  group_by(year) %>%
  summarize( n.observationstot = n_distinct(occurrenceID))

#créer une table en fct années avec : 
#   n.observationsAl : le nombre d'observations d'alouette  
#   n.observateurs, le nombre d'observateru différents d'alouette
#   ratio1 : le nombre d'observations d'alouette par observateur
sum.alouette = alouette.bret %>% 
  st_drop_geometry() %>% #retire la partie spatial epour faciliter le traitement des données numériques
  group_by(year) %>%  #groupe par année (première colone = année)
  summarize(n.observationsAl = n(), n.observateurs = n_distinct(recordedBy) ) %>% #crée deux nouvelles colonnes
  #nombre d'observations = nombre de ligne alouette par an
  #nombre d'observateurs = nombre de noms différents ayant fait ces observations d'alouettes par an
  mutate(ratio1 = n.observationsAl/n.observateurs) #nouvelle colonne qui résume le ratio des deux


# représenter ce ratio AL selon les années avec graph points reliés ========

#créerun nv table combinant les 2 précédants
ratio.alouettenbobse = left_join(sum.obs,sum.alouette,by="year") %>% 
  mutate(ratio2 = n.observationsAl/n.observationstot) #%>% 
 #filter(year != 2014,year !=2001)

#extrait certaine donnée de ce tableau
ratio.alouettenbobse %>% 
  summarize(ratio2max=max(), ratio2min=min())

ggplot(ratio.alouettenbobse,aes(x=year, y = ratio2)) +
  geom_path(lineend = "butt",linejoin = "round", linemitre = 1) +
  #geom_point()
  labs(x = "Années", y = "nombre d'observation d'alouette sur le nombre d'observations totales",
                                                 title ="nombre d'observation d'alouette sur le nombre d'observations totales en fonction des années except 2001 et 2014",
                                                 caption = "PIR science Part") +
  geom_smooth(method = lm)

# représenter ce ratio AL selon les années avec graph points smooth ========


#créerun nv table combinant les 2 précédants
ratio.alouettenbobse = left_join(sum.obs,sum.alouette,by=NULL) %>% 
  mutate(ratio2 = n.observationsAl/n.observationstot) %>% 
  filter( year != 2014, year !=2001)

ggplot(ratio.alouettenbobse,aes(x=year, y = ratio2)) +
  #geom_path(lineend = "butt",linejoin = "round", linemitre = 1) +
  #geom_point()
  geom_smooth(method = lm) +
  labs(x = "Années", y = "nombre d'observation d'alouette sur le nombre d'observations totales",
       title ="nombre d'observation d'alouette sur le nombre d'observations totales en fonction des années except 2001 et 2014",
       caption = "PIR science Part") 
ggsave("plotAJ.png", width = 5, height = 5)




# II  Nombre d'observations de Bruant Jaune par nombres d'obervation e --------

#créer une table avec le nombre d'observation par an
sum.obs = data.bret %>%
  st_drop_geometry() %>%
  group_by(year) %>%
  summarize( n.observationstot = n_distinct(occurrenceID))

#créer une table en fct années avec : 
#   n.observationsBJ : le nombre d'observations de BJ 
#   n.observateurs, le nombre d'observateru différents d'alouette
#   ratio1 : le nombre d'observations de BJ par observateur
sum.bj = bruantj.bret %>% 
  st_drop_geometry() %>% #retire la partie spatial epour faciliter le traitement des données numériques
  group_by(year) %>%  #groupe par année (première colone = année)
  summarize(n.observationsBJ = n(), n.observateurs = n_distinct(recordedBy) ) %>% #crée deux nouvelles colonnes
  #nombre d'observations = nombre de ligne alouette par an
  #nombre d'observateurs = nombre de noms différents ayant fait ces observations d'alouettes par an
  mutate(ratio1 = n.observationsBJ/n.observateurs) #nouvelle colonne qui résume le ratio des deux


# représenter ce ratio BJ selon les années avec graph points reliés ========

#créerun nv table combinant les 2 précédants
ratio.BJobsparotot = left_join(sum.obs,sum.bj,by="year") %>% 
  mutate(ratio2 = n.observationsBJ/n.observationstot) #%>% 
#filter(year != 2014,year !=2001)

ggplot(ratio.BJobsparotot,aes(x=year, y = ratio2)) +
  geom_path(lineend = "butt",linejoin = "round", linemitre = 1) +
  #geom_point()
  labs(x = "Années", y = "nombre d'observation de BJ sur le nombre d'observations totales",
       title ="nombre d'observation de BJ sur le nombre d'observations totales en fonction des années except 2001 et 2014",
       caption = "PIR science Part") 


# représenter ce ratio BJ selon les années avec graph points smooth ========


#créerun nv table combinant les 2 précédants
ratio.BJobsparotot = left_join(sum.obs,sum.bj,by=NULL) %>% 
  mutate(ratio2 = n.observationsBJ/n.observationstot) %>% 
  filter( year !=2001)

ggplot(ratio.BJobsparotot,aes(x=year, y = ratio2)) +
  #geom_path(lineend = "butt",linejoin = "round", linemitre = 1) +
  #geom_point()
  geom_smooth(method = lm) +
  labs(x = "Années", y = "nombre d'observation de BJ sur le nombre d'observations totales",
       title ="nombre d'observation de BJ sur le nombre d'observations totales en fonction des années except 2001 et 2014",
       caption = "PIR science Part") 
ggsave("plotBJsmooth.png", width = 5, height = 5)

# III Nombre d'observations de charbo par nombres d'obervation en bretagne---------------------

#créer une table avec le nombre d'observation par an
sum.obs = data.bret %>%
  st_drop_geometry() %>%
  group_by(year) %>%
  summarize( n.observationstot = n_distinct(occurrenceID))

#créer une table en fct années avec : 
#   n.observationsAl : le nombre d'observations d'alouette  
#   n.observateurs, le nombre d'observateru différents d'alouette
#   ratio1 : le nombre d'observations d'alouette par observateur
sum.charbo = charbo.bret %>% 
  st_drop_geometry() %>% #retire la partie spatial epour faciliter le traitement des données numériques
  group_by(year) %>%  #groupe par année (première colone = année)
  summarize(n.observationsCh = n(), n.observateurs = n_distinct(recordedBy) ) %>% #crée deux nouvelles colonnes
  #nombre d'observations = nombre de ligne alouette par an
  #nombre d'observateurs = nombre de noms différents ayant fait ces observations d'alouettes par an
  mutate(ratio1 = n.observationsCh/n.observateurs) #nouvelle colonne qui résume le ratio des deux


# représenter ce ratio Ch selon les années avec graph points reliés ========

#créerun nv table combinant les 2 précédants
ratio.tablCh = left_join(sum.obs,sum.charbo,by="year") %>% 
  mutate(ratio2 = n.observationsCh/n.observationstot) 

ggplot(ratio.tablCh,aes(x=year, y = ratio2)) +
  geom_path(lineend = "butt",linejoin = "round", linemitre = 1) +
  #geom_point()
  labs(x = "Années", y = "nombre d'observation de Charbo sur le nombre d'observations totales",
       title ="nombre d'observation de charbo sur le nombre d'observations totales en fonction des années except 2001 et 2014",
       caption = "PIR science Part") 

# représenter ce ratio Ch selon les années avec graph points smooth ========


#créerun nv table combinant les 2 précédants
ratio.tablCh = left_join(sum.obs,sum.charbo,by=NULL) %>% 
  mutate(ratio2 = n.observationsCh/n.observationstot)

ggplot(ratio.tablCh,aes(x=year, y = ratio2)) +
  #geom_path(lineend = "butt",linejoin = "round", linemitre = 1) +
  #geom_point()
  geom_smooth(method = lm) +
  labs(x = "Années", y = "nombre d'observation de Charbo sur le nombre d'observations totales",
       title ="nombre d'observation de Charbo sur le nombre d'observations totales en fonction des années except 2001 et 2014",
       caption = "PIR science Part") 
ggsave("plotChsmoothsans2020200420162017.png", width = 5, height = 5)




# IV Nombre d'observations de tourt par nombres d'obervation en bretagne---------------------

#créer une table avec le nombre d'observation par an
sum.obs = data.bret %>%
  st_drop_geometry() %>%
  group_by(year) %>%
  summarize( n.observationstot = n_distinct(occurrenceID))

#créer une table en fct années avec : 
#   n.observationsAl : le nombre d'observations d'alouette  
#   n.observateurs, le nombre d'observateru différents d'alouette
#   ratio1 : le nombre d'observations d'alouette par observateur
sum.tourt = tourt.bret %>% 
  st_drop_geometry() %>% #retire la partie spatial epour faciliter le traitement des données numériques
  group_by(year) %>%  #groupe par année (première colone = année)
  summarize(n.observationsTo = n(), n.observateurs = n_distinct(recordedBy) ) %>% #crée deux nouvelles colonnes
  #nombre d'observations = nombre de ligne alouette par an
  #nombre d'observateurs = nombre de noms différents ayant fait ces observations d'alouettes par an
  mutate(ratio1 = n.observationsTo/n.observateurs) #nouvelle colonne qui résume le ratio des deux

# représenter ce ratio To selon les années avec graph points reliés ========

#créerun nv table combinant les 2 précédants
ratio.tablTo = left_join(sum.obs,sum.tourt,by="year") %>% 
  mutate(ratio2 = n.observationsTo/n.observationstot) #%>% 
#filter(year != 2014,year !=2001)

ggplot(ratio.tablTo,aes(x=year, y = ratio2)) +
  geom_path(lineend = "butt",linejoin = "round", linemitre = 1) +
  #geom_point()
  labs(x = "Années", y = "nombre d'observation de tourterelle sur le nombre d'observations totales",
       title ="nombre d'observation de tourterelle sur le nombre d'observations totales en fonction des années except 2001 et 2014",
       caption = "PIR science Part") 

# représenter ce ratio To selon les années avec graph points smooth ========


#créerun nv table combinant les 2 précédants
ratio.ratioTo = left_join(sum.obs,sum.tourt,by=NULL) %>% 
  mutate(ratio2 = n.observationsTo/n.observationstot) #%>% 
  #filter( year != 2014, year !=2001)

ggplot(ratio.ratioTo,aes(x=year, y = ratio2,)) +
  geom_smooth(method = lm,fill = 'blue',show.legend=TRUE ) +
  labs(x = "Années", y = "abondance relative",
       title ="Abondance relative de la tourterelle au cours des années",
       caption = "PIR science Participative") +
  theme_classic() +
  geom_path(lineend = "butt",linejoin = "round", linemitre = 20, colour = 'blue', size = 0.5)+
  theme(plot.title = element_text(hjust = 0.5))

ggsave("plotTosmooth.png", width = 5, height = 5)   





# Graphs beau pour chaque sp ----------------------------------------------

ratio.tablTo = left_join(sum.obs,sum.tourt,by="year") %>% 
  mutate(ratio2 = n.observationsTo/n.observationstot)

ggplot(ratio.tablTo,aes(x=year )) +
  geom_smooth(aes(y=ratio2), method = lm,fill='blue',color='blue' ) +
  labs(x = "Années", y = "abondance relative",
       title ="Abondance relative de la tourterelle au cours des années",
       caption = "PIR science Participative", subtitle = "Streptopelia turtur") +
  theme_classic() +
  geom_path(aes(y=ratio2),lineend = "butt",linejoin = "round", linemitre = 20, colour = 'blue', size = 0.5)+
  theme(plot.title = element_text(hjust = 0.5),,plot.subtitle = element_text(hjust = 0.5)) 

ggplot(NULL,aes(x=year )) +
  labs(x = "Années", y = "tout",
       title ="tout",
       caption = "PIR science Participative", subtitle = "tout") +
  theme_classic() +
  geom_path(data=ratio.tablTo, aes(x=year, y=ratio2),lineend = "butt",linejoin = "round", linemitre = 20, colour = 'blue', size = 1)+
  geom_path(data=ratio.alouettenbobse,aes(y=ratio2),lineend = "butt",linejoin = "round", linemitre = 20, colour = 'red', size = 1)+
  geom_path(data=ratio.BJobsparotot,aes(y=ratio2),lineend = "butt",linejoin = "round", linemitre = 20, colour = 'orange', size = 1)+
  geom_path(data=ratio.tablCh,aes(y=ratio2),lineend = "butt",linejoin = "round", linemitre = 20, colour = 'purple', size = 1)+
  theme(plot.title = element_text(hjust = 0.5),,plot.subtitle = element_text(hjust = 0.5)) 

ggplot(ratio.BJobsparotot,aes(x=year )) +
  geom_smooth(aes(y=ratio2), method = lm,fill='orange',color='orange' ) +
  labs(x = "Années", y = "abondance relative",
       title ="Abondance relative du Bruant Jaune au cours des années",
       caption = "PIR science Participative", subtitle = "Emberiza citrinella") +
  theme_classic() +
  geom_path(aes(y=ratio2),lineend = "butt",linejoin = "round", linemitre = 20, colour = 'orange', size = 0.5)+
  theme(plot.title = element_text(hjust = 0.5),,plot.subtitle = element_text(hjust = 0.5)) 

ggplot(ratio.tablCh,aes(x=year )) +
  geom_smooth(aes(y=ratio2), method = lm,fill='purple',color='purple' ) +
  labs(x = "Années", y = "abondance relative",
       title ="Abondance relative de la mésange charbonnière au cours des années",
       caption = "PIR science Participative", subtitle = "Parus major") +
  theme_classic() +
  geom_path(aes(y=ratio2),lineend = "butt",linejoin = "round", linemitre = 20, colour = 'purple', size = 0.5)+
  theme(plot.title = element_text(hjust = 0.5),,plot.subtitle = element_text(hjust = 0.5)) 

ggplot(ratio.alouettenbobse,aes(x=year )) +
  geom_smooth(aes(y=ratio2), method = lm,fill='red',color='red' ) +
  labs(x = "Années", y = "abondance relative",
       title ="Abondance relative de l'alouette des champs au cours des années",
       caption = "PIR science Participative", subtitle = "Alauda arvensis") +
  theme_classic() +
  geom_path(aes(y=ratio2),lineend = "butt",linejoin = "round", linemitre = 20, colour = 'red', size = 0.5)+
  theme(plot.title = element_text(hjust = 0.5),,plot.subtitle = element_text(hjust = 0.5)) 

#en geom point :

ggplot(NULL,aes(x=year )) +
  labs(x = "Années", y = "variation de l'abondance relative pour 4 espèces",
       title ="valeur d'abondance relative",
       caption = "PIR science Participative", subtitle = "") +
  theme_classic() +
  geom_point(data=ratio.tablTo, aes(x=year, y=ratio2), colour = 'blue', size = 1)+
  geom_point(data=ratio.alouettenbobse,aes(y=ratio2), colour = 'red', size = 1)+
  geom_point(data=ratio.BJobsparotot,aes(y=ratio2), colour = 'orange', size = 1)+
  geom_point(data=ratio.tablCh,aes(y=ratio2), colour = 'purple', size = 1)+
  theme(plot.title = element_text(hjust = 0.5),,plot.subtitle = element_text(hjust = 0.5)) 

#tout en geom smooth : 

ggplot(NULL,aes(x=year )) +
  labs(x = "Années", y = "modélisation de l'abondance relative pour 4 espèces",
       title ="valeur d'abondance relative",
       caption = "PIR science Participative", subtitle = "") +
  theme_classic() +
  geom_smooth(data=ratio.tablTo, aes(x=year, y=ratio2), method = lm,fill='blue',color='blue' ) +
  geom_smooth(data=ratio.alouettenbobse, aes(x=year, y=ratio2), method = lm,fill='red',color='red' ) +
  geom_smooth(data=ratio.BJobsparotot, aes(x=year, y=ratio2), method = lm,fill='orange',color='orange' ) +
  geom_smooth(data=ratio.tablCh, aes(x=year, y=ratio2), method = lm,fill='purple',color='purple' ) +
  theme(plot.title = element_text(hjust = 0.5),,plot.subtitle = element_text(hjust = 0.5)) +
  geom_point(data=ratio.tablTo, aes(x=year, y=ratio2), colour = 'blue', size = 1)+
  geom_point(data=ratio.alouettenbobse,aes(y=ratio2), colour = 'red', size = 1)+
  geom_point(data=ratio.BJobsparotot,aes(y=ratio2), colour = 'orange', size = 1)+
  geom_point(data=ratio.tablCh,aes(y=ratio2), colour = 'purple', size = 1)
  

ggplot(NULL,aes(x=year )) +
  labs(x = "Années", y = "modélisation de l'abondance relative pour 4 espèces",
       title ="valeur d'abondance relative",
       caption = "PIR science Participative", subtitle = "") +
  theme_classic() +
  geom_smooth(data=ratio.tablTo, aes(x=year, y=ratio2), method = lm,fill='blue',color='blue' ) +
  geom_smooth(data=ratio.alouettenbobse, aes(x=year, y=ratio2), method = lm,fill='red',color='red' ) +
  geom_smooth(data=ratio.BJobsparotot, aes(x=year, y=ratio2), method = lm,fill='orange',color='orange' ) +
  geom_smooth(data=ratio.tablCh, aes(x=year, y=ratio2), method = lm,fill='purple',color='purple' ) +
  theme(plot.title = element_text(hjust = 0.5),,plot.subtitle = element_text(hjust = 0.5)) 

#graphs individuels en geompoint et modelisation

ggplot(ratio.BJobsparotot,aes(x=year )) +
  geom_smooth(aes(y=ratio2), method = lm,fill='orange',color='orange' ) +
  labs(x = "Années", y = "abondance relative",
       title ="Abondance relative du Bruant Jaune au cours des années",
       caption = "PIR science Participative", subtitle = "Emberiza citrinella") +
  theme_classic() +
  geom_point(aes( y=ratio2), colour = 'orange', size = 1)+
  theme(plot.title = element_text(hjust = 0.5),,plot.subtitle = element_text(hjust = 0.5)) 

ggplot(ratio.tablCh,aes(x=year )) +
  geom_smooth(aes(y=ratio2), method = lm,fill='purple',color='purple' ) +
  labs(x = "Années", y = "abondance relative",
       title ="Abondance relative de la mésange charbonnière au cours des années",
       caption = "PIR science Participative", subtitle = "Parus major") +
  theme_classic() +
  geom_point(aes( y=ratio2), colour = 'purple', size = 1)+
  theme(plot.title = element_text(hjust = 0.5),,plot.subtitle = element_text(hjust = 0.5)) 

ggplot(ratio.alouettenbobse,aes(x=year )) +
  geom_smooth(aes(y=ratio2), method = lm,fill='red',color='red' ) +
  labs(x = "Années", y = "abondance relative",
       title ="Abondance relative de l'alouette des champs au cours des années",
       caption = "PIR science Participative", subtitle = "Alauda arvensis") +
  theme_classic() +
  geom_point(aes( y=ratio2), colour = 'red', size = 1)+
  theme(plot.title = element_text(hjust = 0.5),,plot.subtitle = element_text(hjust = 0.5)) 

ggplot(ratio.tablTo,aes(x=year )) +
  geom_smooth(aes(y=ratio2), method = lm,fill='blue',color='blue' ) +
  labs(x = "Années", y = "abondance relative",
       title ="Abondance relative de la tourterelle au cours des années",
       caption = "PIR science Participative", subtitle = "Streptopelia turtur") +
  theme_classic() +
  geom_point(aes( y=ratio2), colour = 'blue', size = 1)+
  theme(plot.title = element_text(hjust = 0.5),,plot.subtitle = element_text(hjust = 0.5)) +
  stat_cor(label.y = 35)


#affiche le r value --------------------
ggplot(data = ratio.alouettenbobse, aes(x = year, y = ratio2)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "adj.R2", "f", "p", "n", "R2.confint"))) +
  geom_point() +
  theme_classic()

p <- ggplot(data = df, aes(x = x, y = y)) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  geom_point()+
  stat_cor(label.y = 35)+ #this means at 35th unit in the y axis, the r squared and p value will be shown
  stat_regline_equation(label.y = 30)


#alouette######################################

#Proposition pour que ratio2 = 1 en 2001 et pour voir les autres années en fonction

ratio.alouettenbobse <- left_join(sum.obs, sum.alouette, by = "year") %>%
  mutate(ratio2 = n.observationsAl / n.observationstot) %>%
  mutate(ratio2_norm = ratio2 / ratio2[year == 2002])
#diviser les valeurs par celle de 2001

ggplot(ratio.alouettenbobse,aes(x=year, y = ratio2_norm)) +
  #geom_path(lineend = "butt",linejoin = "round", linemitre = 1) +
  #geom_point()
  geom_line()+
  geom_smooth(method = lm) +
  labs(x = "Années", y = "nombre d'observation d'alouette sur le nombre d'observations totales",
       title ="nombre d'observation d'alouette sur le nombre d'observations totales en fonction des années except 2001 et 2014",
       caption = "PIR science Part") 

ggplot(data = ratio.alouettenbobse, aes(x = year, y = ratio2_norm)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "adj.R2", "f", "p", "n", "R2.confint"))) +
  geom_point() +
  theme_classic()

# pour extrire les coefs de la régression linéaire

# Calcul de la régression linéaire
model <- lm(ratio2_norm ~ year, data = ratio.alouettenbobse)
summary_model <- summary(model)
coeffs <- summary_model$coefficients

# Extraction des valeurs importantes
a <- round(coeffs[1, 1], 4)  # Intercept
b <- round(coeffs[2, 1], 4)  # Pente
r2 <- round(summary_model$r.squared, 4)  # R²
p_value <- round(coeffs[2, 4], 4)  # p-value de la pente

# Calcul des intervalles de confiance à 95%
conf_int <- confint(model, level = 0.95)
conf_low <- round(conf_int[2, 1], 4)
conf_high <- round(conf_int[2, 2], 4)

# Création du texte de la légende
legend_text <- paste0("y = ", a, " + ", b, "x\n",
                      "R² = ", r2, "\n",
                      "p-value = ", p_value, "\n",
                      "IC 95%: [", conf_low, ", ", conf_high, "]")

# Graphique avec ggplot
ggplot(ratio.alouettenbobse, aes(x = year)) +
  geom_smooth(aes(y = ratio2_norm), method = lm, fill = 'red', color = 'red') +
  labs(x = "Années", y = "Abondance relative",
       title = "Abondance relative de l'Alouette des champsau cours des années",
       caption = "PIR Science Participative", subtitle = "Alauda arvensis") +
  theme_classic() +
  geom_point(aes(y = ratio2_norm), colour = 'red', size = 1) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  annotate("text", x = max(ratio.alouettenbobse$year) - 0,  # Déplacement à droite
           y = max(ratio.alouettenbobse$ratio2_norm) * 0.95,  # Déplacement vers le haut
           label = legend_text, hjust = 1, size = 3.5, color = "black")

###############################

#######################################

# Charbo ------------------------------------------------------------------


#Proposition pour que ratio2 = 1 en 2001 et pour voir les autres années en fonction

ratio.tablCh <- left_join(sum.obs, sum.charbo, by = "year") %>%
  mutate(ratio2 = n.observationsCh / n.observationstot) %>%
  mutate(ratio2_norm = ratio2 / ratio2[year == 2001])
#diviser les valeurs par celle de 2001

ggplot(ratio.tablCh,aes(x=year, y = ratio2_norm)) +
  #geom_path(lineend = "butt",linejoin = "round", linemitre = 1) +
  #geom_point()
  geom_line()+
  geom_smooth(method = lm) +
  labs(x = "Années", y = "nombre d'observation d'alouette sur le nombre d'observations totales",
       title ="nombre d'observation d'alouette sur le nombre d'observations totales en fonction des années except 2001 et 2014",
       caption = "PIR science Part") 

ggplot(data = ratio.tablCh, aes(x = year, y = ratio2_norm)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "adj.R2", "f", "p", "n", "R2.confint"))) +
  geom_point() +
  theme_classic()

# pour extrire les coefs de la régression linéaire

# Calcul de la régression linéaire
model <- lm(ratio2_norm ~ year, data = ratio.tablCh)
summary_model <- summary(model)
coeffs <- summary_model$coefficients

# Extraction des valeurs importantes
a <- round(coeffs[1, 1], 4)  # Intercept
b <- round(coeffs[2, 1], 4)  # Pente
r2 <- round(summary_model$r.squared, 4)  # R²
p_value <- round(coeffs[2, 4], 4)  # p-value de la pente

# Calcul des intervalles de confiance à 95%
conf_int <- confint(model, level = 0.95)
conf_low <- round(conf_int[2, 1], 4)
conf_high <- round(conf_int[2, 2], 4)

# Création du texte de la légende
legend_text <- paste0("y = ", a, " + ", b, "x\n",
                      "R² = ", r2, "\n",
                      "p-value = ", p_value, "\n",
                      "IC 95%: [", conf_low, ", ", conf_high, "]")

# Graphique avec ggplot
ggplot(ratio.tablCh, aes(x = year)) +
  geom_smooth(aes(y = ratio2_norm), method = lm, fill = 'purple', color = 'purple') +
  labs(x = "Années", y = "Abondance relative",
       title = "Abondance relative de la mésange charbonière au cours des années",
       caption = "PIR Science Participative", subtitle = "Parus major") +
  theme_classic() +
  geom_point(aes(y = ratio2_norm), colour = 'purple', size = 1) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  annotate("text", x = max(ratio.tablCh$year) - 6,  # Déplacement à droite
           y = min(ratio.tablCh$ratio2_norm) * 1.1,  # Déplacement vers le haut
           label = legend_text, hjust = 1, size = 3, color = "black")


# tourterelle -------------------------------------------------------------
ratio.tablTo <- left_join(sum.obs, sum.tourt, by = "year") %>%
  mutate(ratio2 = n.observationsTo / n.observationstot) %>%
  mutate(ratio2_norm = ratio2 / ratio2[year == 2001])
#diviser les valeurs par celle de 2001

ggplot(ratio.tablTo,aes(x=year, y = ratio2_norm)) +
  #geom_path(lineend = "butt",linejoin = "round", linemitre = 1) +
  #geom_point()
  geom_line()+
  geom_smooth(method = lm) +
  labs(x = "Années", y = "nombre d'observation d'alouette sur le nombre d'observations totales",
       title ="nombre d'observation d'alouette sur le nombre d'observations totales en fonction des années except 2001 et 2014",
       caption = "PIR science Part") 

ggplot(data = ratio.tablTo, aes(x = year, y = ratio2_norm)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "adj.R2", "f", "p", "n", "R2.confint"))) +
  geom_point() +
  theme_classic()

# pour extrire les coefs de la régression linéaire

# Calcul de la régression linéaire
model <- lm(ratio2_norm ~ year, data = ratio.tablTo)
summary_model <- summary(model)
coeffs <- summary_model$coefficients

# Extraction des valeurs importantes
a <- round(coeffs[1, 1], 4)  # Intercept
b <- round(coeffs[2, 1], 4)  # Pente
r2 <- round(summary_model$r.squared, 4)  # R²
p_value <- round(coeffs[2, 4], 4)  # p-value de la pente

# Calcul des intervalles de confiance à 95%
conf_int <- confint(model, level = 0.95)
conf_low <- round(conf_int[2, 1], 4)
conf_high <- round(conf_int[2, 2], 4)

# Création du texte de la légende
legend_text <- paste0("y = ", a, " + ", b, "x\n",
                      "R² = ", r2, "\n",
                      "p-value = ", p_value, "\n",
                      "IC 95%: [", conf_low, ", ", conf_high, "]")

# Graphique avec ggplot
ggplot(ratio.tablTo, aes(x = year)) +
  geom_smooth(aes(y = ratio2_norm), method = lm, fill = 'blue', color = 'blue') +
  labs(x = "Années", y = "Abondance relative",
         title = "Abondance relative de la Tourterelle des bois au cours des années",
       caption = "PIR Science Participative", subtitle = "Streptopelia turtur") +
  theme_classic() +
  geom_point(aes(y = ratio2_norm), colour = 'blue', size = 1) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  annotate("text", x = max(ratio.tablTo$year) - 0,  # Déplacement à droite
           y = max(ratio.tablTo$ratio2_norm) * 0.9,  # Déplacement vers le haut
           label = legend_text, hjust = 1, size = 3.5, color = "black")



# bruantj -----------------------------------------------------------------

ratio.BJobsparotot <- left_join(sum.obs, sum.bj, by = "year") %>%
  mutate(ratio2 = n.observationsBJ / n.observationstot) %>%
  mutate(ratio2_norm = ratio2 / ratio2[year == 2001])
#diviser les valeurs par celle de 2001


# Calcul de la régression linéaire
model <- lm(ratio2_norm ~ year, data = ratio.BJobsparotot)
summary_model <- summary(model)
coeffs <- summary_model$coefficients

# Extraction des valeurs importantes
a <- round(coeffs[1, 1], 4)  # Intercept
b <- round(coeffs[2, 1], 4)  # Pente
r2 <- round(summary_model$r.squared, 4)  # R²
p_value <- round(coeffs[2, 4], 4)  # p-value de la pente

# Calcul des intervalles de confiance à 95%
conf_int <- confint(model, level = 0.95)
conf_low <- round(conf_int[2, 1], 4)
conf_high <- round(conf_int[2, 2], 4)

# Création du texte de la légende
legend_text <- paste0("y = ", a, " + ", b, "x\n",
                      "R² = ", r2, "\n",
                      "p-value = ", p_value, "\n",
                      "IC 95%: [", conf_low, ", ", conf_high, "]")

# Graphique avec ggplot
ggplot(ratio.BJobsparotot, aes(x = year)) +
  geom_smooth(aes(y = ratio2_norm), method = lm, fill = 'orange', color = 'orange') +
  labs(x = "Années", y = "Abondance relative",
       title = "Abondance relative du Bruant jaune au cours des années",
       caption = "PIR Science Participative", subtitle = "Emberiza citrinella") +
  theme_classic() +
  geom_point(aes(y = ratio2_norm), colour = 'orange', size = 1) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  annotate("text", x = max(ratio.BJobsparotot$year) - 0,  # Déplacement à droite
           y = max(ratio.BJobsparotot$ratio2_norm) * 0.9,  # Déplacement vers le haut
           label = legend_text, hjust = 1, size = 3.5, color = "black")



# alouette nationale en pause car trop bizzarre : croissance ??? ------------------------------------------------------
sum.alouetteNat = alouette %>% 
  st_drop_geometry() %>% #retire la partie spatial epour faciliter le traitement des données numériques
  group_by(year) %>%  #groupe par année (première colone = année)
  summarize(n.observationsAlNat = n(), n.observateurs = n_distinct(recordedBy) ) %>% #crée deux nouvelles colonnes
  #nombre d'observations = nombre de ligne alouette par an
  #nombre d'observateurs = nombre de noms différents ayant fait ces observations d'alouettes par an
  mutate(ratio1 = n.observationsAlNat/n.observateurs) #nouvelle colonne qui résume le ratio des deux
ratio.tablAlNat = left_join(sum.obs,sum.alouetteNat,by="year") %>% 
  mutate(ratio2 = n.observationsAlNat/n.observationstot) #%>% 
#filter(year != 2014,year !=2001)


ratio.tablAlNat <- left_join(sum.obs, sum.alouetteNat, by = "year") %>%
  mutate(ratio2 = n.observationsAlNat / n.observationstot) %>%
  mutate(ratio2_norm = ratio2 / ratio2[year == 2001])
#diviser les valeurs par celle de 2001


# Calcul de la régression linéaire
model <- lm(ratio2_norm ~ year, data = ratio.tablAlNat)
summary_model <- summary(model)
coeffs <- summary_model$coefficients

# Extraction des valeurs importantes
a <- round(coeffs[1, 1], 4)  # Intercept
b <- round(coeffs[2, 1], 4)  # Pente
r2 <- round(summary_model$r.squared, 4)  # R²
p_value <- round(coeffs[2, 4], 4)  # p-value de la pente

# Calcul des intervalles de confiance à 95%
conf_int <- confint(model, level = 0.95)
conf_low <- round(conf_int[2, 1], 4)
conf_high <- round(conf_int[2, 2], 4)

# Création du texte de la légende
legend_text <- paste0("y = ", a, " + ", b, "x\n",
                      "R² = ", r2, "\n",
                      "p-value = ", p_value, "\n",
                      "IC 95%: [", conf_low, ", ", conf_high, "]")

# Graphique avec ggplot
ggplot(ratio.tablAlNat, aes(x = year)) +
  geom_smooth(aes(y = ratio2_norm), method = lm, fill = 'orange', color = 'orange') +
  labs(x = "Années", y = "Abondance relative",
       title = "Abondance relative de l'Alouette des champs à l'échelle nationale",
       caption = "PIR Science Participative", subtitle = "Alauda arvensis") +
  theme_classic() +
  geom_point(aes(y = ratio2_norm), colour = 'orange', size = 1) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  annotate("text", x = max(ratio.tablAlNat$year) - 0,  # Déplacement à droite
           y = max(ratio.tablAlNat$ratio2_norm) * 0.9,  # Déplacement vers le haut
           label = legend_text, hjust = 1, size = 3.5, color = "black")



# tourterelle nationale en pause car trop bizzarre : croissance ???---------------------------------------------------

sum.tourtNatio = tourt %>% 
  st_drop_geometry() %>% #retire la partie spatial epour faciliter le traitement des données numériques
  group_by(year) %>%  #groupe par année (première colone = année)
  summarize(n.observationsToNatio = n(), n.observateurs = n_distinct(recordedBy) ) %>% #crée deux nouvelles colonnes
  #nombre d'observations = nombre de ligne alouette par an
  #nombre d'observateurs = nombre de noms différents ayant fait ces observations d'alouettes par an
  mutate(ratio1 = n.observationsToNatio/n.observateurs) #nouvelle colonne qui résume le ratio des deux

ratio.tabltourtNat = left_join(sum.obs,sum.tourtNatio,by="year") %>% 
  mutate(ratio2 = n.observationsToNatio/n.observationstot) #%>% 
#filter(year != 2014,year !=2001)


ratio.tabltourtNat <- left_join(sum.obs, sum.tourtNatio, by = "year") %>%
  mutate(ratio2 = n.observationsToNatio / n.observationstot) %>%
  mutate(ratio2_norm = ratio2 / ratio2[year == 2001])
#diviser les valeurs par celle de 2001


# Calcul de la régression linéaire
model <- lm(ratio2_norm ~ year, data = ratio.tabltourtNat)
summary_model <- summary(model)
coeffs <- summary_model$coefficients

# Extraction des valeurs importantes
a <- round(coeffs[1, 1], 4)  # Intercept
b <- round(coeffs[2, 1], 4)  # Pente
r2 <- round(summary_model$r.squared, 4)  # R²
p_value <- round(coeffs[2, 4], 4)  # p-value de la pente

# Calcul des intervalles de confiance à 95%
conf_int <- confint(model, level = 0.95)
conf_low <- round(conf_int[2, 1], 4)
conf_high <- round(conf_int[2, 2], 4)

# Création du texte de la légende
legend_text <- paste0("y = ", a, " + ", b, "x\n",
                      "R² = ", r2, "\n",
                      "p-value = ", p_value, "\n",
                      "IC 95%: [", conf_low, ", ", conf_high, "]")

# Graphique avec ggplot
ggplot(ratio.tabltourtNat, aes(x = year)) +
  geom_smooth(aes(y = ratio2_norm), method = lm, fill = 'orange', color = 'orange') +
  labs(x = "Années", y = "Abondance relative",
       title = "Abondance relative de la tourterelle des bois à l'échelle nationale",
       caption = "PIR Science Participative", subtitle = "Streptopelia turtur") +
  theme_classic() +
  geom_point(aes(y = ratio2_norm), colour = 'orange', size = 1) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  annotate("text", x = max(ratio.tabltourtNat$year) - 0,  # Déplacement à droite
           y = max(ratio.tabltourtNat$ratio2_norm) * 0.9,  # Déplacement vers le haut
           label = legend_text, hjust = 1, size = 3.5, color = "black")




# linotte mélodieuse ------------------------------------------------------

sum.linotte.bret = linotte.bret %>% 
  st_drop_geometry() %>% #retire la partie spatial epour faciliter le traitement des données numériques
  group_by(year) %>%  #groupe par année (première colone = année)
  summarize(n.observationsLi = n(), n.observateurs = n_distinct(recordedBy) ) %>% #crée deux nouvelles colonnes
  #nombre d'observations = nombre de ligne alouette par an
  #nombre d'observateurs = nombre de noms différents ayant fait ces observations d'alouettes par an
  mutate(ratio1 = n.observationsLi/n.observateurs) #nouvelle colonne qui résume le ratio des deux

ratio.tablLi = left_join(sum.obs,sum.linotte.bret,by="year") %>% 
  mutate(ratio2 = n.observationsLi/n.observationstot) #%>% 
#filter(year != 2014,year !=2001)


ratio.tablLi <- left_join(sum.obs, sum.linotte.bret, by = "year") %>%
  mutate(ratio2 = n.observationsLi / n.observationstot) %>%
  mutate(ratio2_norm = ratio2 / ratio2[year == 2001])
#diviser les valeurs par celle de 2001


# Calcul de la régression linéaire
model <- lm(ratio2_norm ~ year, data = ratio.tablLi)
summary_model <- summary(model)
coeffs <- summary_model$coefficients

# Extraction des valeurs importantes
a <- round(coeffs[1, 1], 4)  # Intercept
b <- round(coeffs[2, 1], 4)  # Pente
r2 <- round(summary_model$r.squared, 4)  # R²
p_value <- round(coeffs[2, 4], 4)  # p-value de la pente

# Calcul des intervalles de confiance à 95%
conf_int <- confint(model, level = 0.95)
conf_low <- round(conf_int[2, 1], 4)
conf_high <- round(conf_int[2, 2], 4)

# Création du texte de la légende
legend_text <- paste0("y = ", a, " + ", b, "x\n",
                      "R² = ", r2, "\n",
                      "p-value = ", p_value, "\n",
                      "IC 95%: [", conf_low, ", ", conf_high, "]")

# Graphique avec ggplot
ggplot(ratio.tablLi, aes(x = year)) +
  geom_smooth(aes(y = ratio2_norm), method = lm, fill = 'green', color = 'green') +
  labs(x = "Années", y = "Abondance relative",
       title = "Abondance relative de la Linotte mélodieuse au ccours des années",
       caption = "PIR Science Participative", subtitle = "Linaria cannabina") +
  theme_classic() +
  geom_point(aes(y = ratio2_norm), colour = 'green', size = 1) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  annotate("text", x = max(ratio.tablLi$year) - 0,  # Déplacement à droite
           y = max(ratio.tablLi$ratio2_norm) * 0.9,  # Déplacement vers le haut
           label = legend_text, hjust = 1, size = 3.5, color = "black")


# fauvette grisette -------------------------------------------------------


sum.fauvette.bret = fauvette.bret %>% 
  st_drop_geometry() %>% #retire la partie spatial epour faciliter le traitement des données numériques
  group_by(year) %>%  #groupe par année (première colone = année)
  summarize(n.observationsFa = n(), n.observateurs = n_distinct(recordedBy) ) %>% #crée deux nouvelles colonnes
  #nombre d'observations = nombre de ligne alouette par an
  #nombre d'observateurs = nombre de noms différents ayant fait ces observations d'alouettes par an
  mutate(ratio1 = n.observationsFa/n.observateurs) #nouvelle colonne qui résume le ratio des deux

ratio.tablFa = left_join(sum.obs,sum.fauvette.bret,by="year") %>% 
  mutate(ratio2 = n.observationsFa/n.observationstot) #%>% 
#filter(year != 2014,year !=2001)


ratio.tablFa <- left_join(sum.obs, sum.fauvette.bret, by = "year") %>%
  mutate(ratio2 = n.observationsFa / n.observationstot) %>%
  mutate(ratio2_norm = ratio2 / ratio2[year == 2001])
#diviser les valeurs par celle de 2001


# Calcul de la régression linéaire
model <- lm(ratio2_norm ~ year, data = ratio.tablFa)
summary_model <- summary(model)
coeffs <- summary_model$coefficients

# Extraction des valeurs importantes
a <- round(coeffs[1, 1], 4)  # Intercept
b <- round(coeffs[2, 1], 4)  # Pente
r2 <- round(summary_model$r.squared, 4)  # R²
p_value <- round(coeffs[2, 4], 4)  # p-value de la pente

# Calcul des intervalles de confiance à 95%
conf_int <- confint(model, level = 0.95)
conf_low <- round(conf_int[2, 1], 4)
conf_high <- round(conf_int[2, 2], 4)

# Création du texte de la légende
legend_text <- paste0("y = ", a, " + ", b, "x\n",
                      "R² = ", r2, "\n",
                      "p-value = ", p_value, "\n",
                      "IC 95%: [", conf_low, ", ", conf_high, "]")

# Graphique avec ggplot
ggplot(ratio.tablFa, aes(x = year)) +
  geom_smooth(aes(y = ratio2_norm), method = lm, fill = 'brown', color = 'brown') +
  labs(x = "Years", y = "Relative abundance",
       title = "RElative abundance of the Common whitethroat dove over the years",
       caption = "", subtitle = "Sylvia communis") +
  theme_classic() +
  geom_point(aes(y = ratio2_norm), colour = 'brown', size = 1) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  annotate("text", x = max(ratio.tablFa$year) - 0,  # Déplacement à droite
           y = max(ratio.tablFa$ratio2_norm) * 0.9,  # Déplacement vers le haut
           label = legend_text, hjust = 1, size = 3.5, color = "black")




# merle noir -  Turdus merula - Common blackbird ------------------------------------------------
sum.merle.bret = merle.bret %>% 
  st_drop_geometry() %>% #retire la partie spatial epour faciliter le traitement des données numériques
  group_by(year) %>%  #groupe par année (première colone = année)
  summarize(n.observationsMe = n(), n.observateurs = n_distinct(recordedBy) ) %>% #crée deux nouvelles colonnes
  #nombre d'observations = nombre de ligne alouette par an
  #nombre d'observateurs = nombre de noms différents ayant fait ces observations d'alouettes par an
  mutate(ratio1 = n.observationsMe/n.observateurs) #nouvelle colonne qui résume le ratio des deux

ratio.tablLi = left_join(sum.obs,sum.merle.bret,by="year") %>% 
  mutate(ratio2 = n.observationsMe/n.observationstot) #%>% 
#filter(year != 2014,year !=2001)

ratio.merle <- left_join(sum.obs, sum.merle.bret, by = "year") %>%
  mutate(ratio2 = n.observationsMe / n.observationstot) %>%
  mutate(ratio2_norm = ratio2 / ratio2[year == 2001])
#diviser les valeurs par celle de 2001


# Calcul de la régression linéaire
model <- lm(ratio2_norm ~ year, data = ratio.merle)
summary_model <- summary(model)
coeffs <- summary_model$coefficients

# Extraction des valeurs importantes
a <- round(coeffs[1, 1], 4)  # Intercept
b <- round(coeffs[2, 1], 4)  # Pente
r2 <- round(summary_model$r.squared, 4)  # R²
p_value <- round(coeffs[2, 4], 4)  # p-value de la pente

# Calcul des intervalles de confiance à 95%
conf_int <- confint(model, level = 0.95)
conf_low <- round(conf_int[2, 1], 4)
conf_high <- round(conf_int[2, 2], 4)

# Création du texte de la légende
legend_text <- paste0("y = ", a, " + ", b, "x\n",
                      "R² = ", r2, "\n",
                      "p-value = ", p_value, "\n",
                      "IC 95%: [", conf_low, ", ", conf_high, "]")

# Graphique avec ggplot
ggplot(ratio.merle, aes(x = year)) +
  geom_smooth(aes(y = ratio2_norm), method = lm, fill = 'coral', color = 'coral') +
  labs(x = "Years", y = "Relative abundance",
       title = "Relative abundance of the Common blackbird over the years",
       caption = "", subtitle = "Turdus merula") +
  theme_classic() +
  geom_point(aes(y = ratio2_norm), colour = 'coral', size = 1) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  annotate("text", x = max(ratio.merle$year) - 0,  # Déplacement à droite
           y = max(ratio.merle$ratio2_norm) * 0.95,  # Déplacement vers le haut
           label = legend_text, hjust = 1, size = 3.5, color = "black")

# multi-espèces -----------------------------------------------------------
model <- lm(ratio2_norm ~ year, data = ratio.tablFa)
summary_model <- summary(model)
coeffs <- summary_model$coefficients

# Extraction des valeurs importantes
a <- round(coeffs[1, 1], 4)  # Intercept
b <- round(coeffs[2, 1], 4)  # Pente
r2 <- round(summary_model$r.squared, 4)  # R²
p_value <- round(coeffs[2, 4], 4)  # p-value de la pente

# Calcul des intervalles de confiance à 95%
conf_int <- confint(model, level = 0.95)
conf_low <- round(conf_int[2, 1], 4)
conf_high <- round(conf_int[2, 2], 4)

# Création du texte de la légende
legend_text <- paste0("y = ", a, " + ", b, "x\n",
                      "R² = ", r2, "\n",
                      "p-value = ", p_value, "\n",
                      "IC 95%: [", conf_low, ", ", conf_high, "]")

# Graphique avec ggplot
ggplot(ratio.tablFa, aes(x = year)) +
  geom_smooth(aes(y = ratio2_norm), method = lm, fill = 'yellow', color = 'yellow') +
  labs(x = "Années", y = "Abondance relative",
       title = "Abondance relative de la Fauvette grisette au ccours des années",
       caption = "PIR Science Participative", subtitle = "Sylvia communis") +
  theme_classic() +
  geom_point(aes(y = ratio2_norm), colour = 'yellow', size = 1) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  annotate("text", x = max(ratio.tablFa$year) - 0,  # Déplacement à droite
           y = max(ratio.tablFa$ratio2_norm) * 0.9,  # Déplacement vers le haut
           label = legend_text, hjust = 1, size = 3.5, color = "black")

ratio.tablMulti <- left_join(ratio.tablCh, ratio.alouettenbobse, by = "year") %>% 
  mutate(ratioCh=ratio2.x)
names(ratio.tablMulti)[names(ratio.tablMulti)== 'ratio2_norm.x'] <- "ratio2_normCh"
names(ratio.tablMulti)[names(ratio.tablMulti)== 'ratio2_norm.y'] <- "ratio2_normAl"
ratio.tablMulti <- left_join(ratio.tablMulti, ratio.tablTo, by = "year")
names(ratio.tablMulti)[names(ratio.tablMulti)== 'ratio2_norm'] <- "ratio2_normTo"
ratio.tablMulti <- left_join(ratio.tablMulti, ratio.BJobsparotot, by = "year")
names(ratio.tablMulti)[names(ratio.tablMulti)== 'ratio2_norm'] <- "ratio2_normBj"

ggplot(ratio.tablMulti, aes(x=year))+
  geom_point(aes(y = ratio2_normAl), colour = 'red', size = 1)+
  geom_point(aes(y = ratio2_normCh), colour = 'purple', size = 1)+
  geom_point(aes(y = ratio2_normTo), colour = 'blue', size = 1)+
  geom_point(aes(y = ratio2_normBj), colour = 'orange', size = 1)+
  theme_classic() +
  labs(x = "Années", y = "Abondance relative",
       title = "Abondance relative des 4 espèces au cours des années",
       caption = "PIR Science Participative", subtitle = "")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

ggplot(ratio.tablMulti, aes(x=year))+
  geom_point(aes(y = ratio2_normAl), colour = 'red', size = 1)+
  geom_point(aes(y = ratio2_normCh), colour = 'purple', size = 1)+
  geom_point(aes(y = ratio2_normTo), colour = 'blue', size = 1)+
  geom_point(aes(y = ratio2_normBj), colour = 'orange', size = 1)+
  geom_smooth(aes(y = ratio2_normAl), method = lm, fill = 'red', color = 'red') +
  geom_smooth(aes(y = ratio2_normCh), method = lm, fill = 'purple', color = 'purple') +
  geom_smooth(aes(y = ratio2_normTo), method = lm, fill = 'blue', color = 'blue') +
  geom_smooth(aes(y = ratio2_normBj), method = lm, fill = 'orange', color = 'orange') +
  theme_classic() +
  labs(x = "Années", y = "Abondance relative",
       title = "Abondance relative des 4 espèces au cours des années",
       caption = "PIR Science Participative", subtitle = "")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))


# national - intro  -------------------------------------------------------

alouette = mydat %>%  filter(species == "Alauda arvensis")
bruantj = mydat %>% filter(species == "Emberiza citrinella")

#idem pour les espèces généralistes tourterelle des bois et mésange charbonnière
tourt = mydat %>% filter(species == "Streptopelia turtur")
charbo = mydat %>% filter(species == "Parus major")

fauvette = mydat %>% filter(species == "Sylvia communis")
linotte = mydat %>% filter(species == "Linaria cannabina")
merle = mydat %>% filter(species == "Turdus merula")

sum.obsNat = mydat %>% 
  st_drop_geometry() %>% 
  group_by(year) %>% 
  summarize( n.observationstotnat = n_distinct(occurrenceID) ) 


# alouette nationale ------------------------------------------------------

sum.alouetteNat = alouette %>% 
  st_drop_geometry() %>% #retire la partie spatial epour faciliter le traitement des données numériques
  group_by(year) %>%  #groupe par année (première colone = année)
  summarize(n.observationsAlNat = n(), n.observateurs = n_distinct(recordedBy) ) %>% #crée deux nouvelles colonnes
  #nombre d'observations = nombre de ligne alouette par an
  #nombre d'observateurs = nombre de noms différents ayant fait ces observations d'alouettes par an
  mutate(ratio1 = n.observationsAlNat/n.observateurs) #nouvelle colonne qui résume le ratio des deux
ratio.tablAlNat = left_join(sum.obsNat,sum.alouetteNat,by="year") %>% 
  mutate(ratio2 = n.observationsAlNat/n.observationstotnat) #%>% 
#filter(year != 2014,year !=2001)


ratio.tablAlNat <- left_join(sum.obsNat, sum.alouetteNat, by = "year") %>%
  mutate(ratio2 = n.observationsAlNat / n.observationstotnat) %>%
  mutate(ratio2_norm = ratio2 / ratio2[year == 2001])
#diviser les valeurs par celle de 2001


# Calcul de la régression linéaire
model <- lm(ratio2_norm ~ year, data = ratio.tablAlNat)
summary_model <- summary(model)
coeffs <- summary_model$coefficients

# Extraction des valeurs importantes
a <- round(coeffs[1, 1], 4)  # Intercept
b <- round(coeffs[2, 1], 4)  # Pente
r2 <- round(summary_model$r.squared, 4)  # R²
p_value <- round(coeffs[2, 4], 4)  # p-value de la pente

# Calcul des intervalles de confiance à 95%
conf_int <- confint(model, level = 0.95)
conf_low <- round(conf_int[2, 1], 4)
conf_high <- round(conf_int[2, 2], 4)

# Création du texte de la légende
legend_text <- paste0("y = ", a, " + ", b, "x\n",
                      "R² = ", r2, "\n",
                      "p-value = ", p_value, "\n",
                      "IC 95%: [", conf_low, ", ", conf_high, "]")

# Graphique avec ggplot
ggplot(ratio.tablAlNat, aes(x = year)) +
  geom_smooth(aes(y = ratio2_norm), method = lm, fill = 'red', color = 'red') +
  labs(x = "Years", y = "relative abundance",
       title = "Relative abundance of the European skylark over the years at a national scale",
       caption = "", subtitle = "Alauda arvensis") +
  theme_classic() +
  geom_point(aes(y = ratio2_norm), colour = 'red', size = 1) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  annotate("text", x = max(ratio.tablAlNat$year) - 0,  # Déplacement à droite
           y = max(ratio.tablAlNat$ratio2_norm) * 0.9,  # Déplacement vers le haut
           label = legend_text, hjust = 1, size = 3.5, color = "black")



# tourterelle nationale ---------------------------------------------------
sum.tourtNatio = tourt %>% 
  st_drop_geometry() %>% #retire la partie spatial epour faciliter le traitement des données numériques
  group_by(year) %>%  #groupe par année (première colone = année)
  summarize(n.observationsToNatio = n(), n.observateurs = n_distinct(recordedBy) ) %>% #crée deux nouvelles colonnes
  #nombre d'observations = nombre de ligne alouette par an
  #nombre d'observateurs = nombre de noms différents ayant fait ces observations d'alouettes par an
  mutate(ratio1 = n.observationsToNatio/n.observateurs) #nouvelle colonne qui résume le ratio des deux

ratio.tabltourtNat = left_join(sum.obsNat,sum.tourtNatio,by="year") %>% 
  mutate(ratio2 = n.observationsToNatio/n.observationstotnat) #%>% 
#filter(year != 2014,year !=2001)


ratio.tabltourtNat <- left_join(sum.obsNat, sum.tourtNatio, by = "year") %>%
  mutate(ratio2 = n.observationsToNatio / n.observationstotnat) %>%
  mutate(ratio2_norm = ratio2 / ratio2[year == 2001])
#diviser les valeurs par celle de 2001


# Calcul de la régression linéaire
model <- lm(ratio2_norm ~ year, data = ratio.tabltourtNat)
summary_model <- summary(model)
coeffs <- summary_model$coefficients

# Extraction des valeurs importantes
a <- round(coeffs[1, 1], 4)  # Intercept
b <- round(coeffs[2, 1], 4)  # Pente
r2 <- round(summary_model$r.squared, 4)  # R²
p_value <- round(coeffs[2, 4], 4)  # p-value de la pente

# Calcul des intervalles de confiance à 95%
conf_int <- confint(model, level = 0.95)
conf_low <- round(conf_int[2, 1], 4)
conf_high <- round(conf_int[2, 2], 4)

# Création du texte de la légende
legend_text <- paste0("y = ", a, " + ", b, "x\n",
                      "R² = ", r2, "\n",
                      "p-value = ", p_value, "\n",
                      "IC 95%: [", conf_low, ", ", conf_high, "]")

# Graphique avec ggplot
ggplot(ratio.tabltourtNat, aes(x = year)) +
  geom_smooth(aes(y = ratio2_norm), method = lm, fill = 'blue', color = 'blue') +
  labs(x = "Years", y = "relative abundance",
       title = "Relative abundance of the European turtle dove over the years at a national scale",
       caption = "PIR Science Participative", subtitle = "Streptopelia turtur") +
  theme_classic() +
  geom_point(aes(y = ratio2_norm), colour = 'blue', size = 1) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  annotate("text", x = max(ratio.tabltourtNat$year) - 0,  # Déplacement à droite
           y = max(ratio.tabltourtNat$ratio2_norm) * 0.9,  # Déplacement vers le haut
           label = legend_text, hjust = 1, size = 3.5, color = "black")




# bruantj national --------------------------------------------------------
sum.BJNatio = bruantj %>% 
  st_drop_geometry() %>% #retire la partie spatial epour faciliter le traitement des données numériques
  group_by(year) %>%  #groupe par année (première colone = année)
  summarize(n.observationsBJnatio = n(), n.observateurs = n_distinct(recordedBy) ) %>% #crée deux nouvelles colonnes
  #nombre d'observations = nombre de ligne alouette par an
  #nombre d'observateurs = nombre de noms différents ayant fait ces observations d'alouettes par an
  mutate(ratio1 = n.observationsBJnatio/n.observateurs) #nouvelle colonne qui résume le ratio des deux

ratio.tablBJNat = left_join(sum.obsNat,sum.BJNatio,by="year") %>% 
  mutate(ratio2 = n.observationsBJnatio/n.observationstotnat) #%>% 
#filter(year != 2014,year !=2001)


ratio.tablBJNat <- left_join(sum.obsNat, sum.BJNatio, by = "year") %>%
  mutate(ratio2 = n.observationsBJnatio / n.observationstotnat) %>%
  mutate(ratio2_norm = ratio2 / ratio2[year == 2001])
#diviser les valeurs par celle de 2001


# Calcul de la régression linéaire
model <- lm(ratio2_norm ~ year, data = ratio.tablBJNat)
summary_model <- summary(model)
coeffs <- summary_model$coefficients

# Extraction des valeurs importantes
a <- round(coeffs[1, 1], 4)  # Intercept
b <- round(coeffs[2, 1], 4)  # Pente
r2 <- round(summary_model$r.squared, 4)  # R²
p_value <- round(coeffs[2, 4], 4)  # p-value de la pente

# Calcul des intervalles de confiance à 95%
conf_int <- confint(model, level = 0.95)
conf_low <- round(conf_int[2, 1], 4)
conf_high <- round(conf_int[2, 2], 4)

# Création du texte de la légende
legend_text <- paste0("y = ", a, " + ", b, "x\n",
                      "R² = ", r2, "\n",
                      "p-value = ", p_value, "\n",
                      "IC 95%: [", conf_low, ", ", conf_high, "]")

# Graphique avec ggplot
ggplot(ratio.tablBJNat, aes(x = year)) +
  geom_smooth(aes(y = ratio2_norm), method = lm, fill = 'orange', color = 'orange') +
  labs(x = "Years", y = "relative abundance",
       title = "Relative abundance of the Yellowhammer dove over the years at a national scale",
       caption = "", subtitle = "Emberiza citrinella") +
  theme_classic() +
  geom_point(aes(y = ratio2_norm), colour = 'orange', size = 1) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  annotate("text", x = max(ratio.tablBJNat$year) - 0,  # Déplacement à droite
           y = max(ratio.tablBJNat$ratio2_norm) * 0.9,  # Déplacement vers le haut
           label = legend_text, hjust = 1, size = 3.5, color = "black")




# charbo natio ------------------------------------------------------------
sum.charboNatio = charbo %>% 
  st_drop_geometry() %>% #retire la partie spatial epour faciliter le traitement des données numériques
  group_by(year) %>%  #groupe par année (première colone = année)
  summarize(n.observationscharbonatio = n(), n.observateurs = n_distinct(recordedBy) ) %>% #crée deux nouvelles colonnes
  #nombre d'observations = nombre de ligne alouette par an
  #nombre d'observateurs = nombre de noms différents ayant fait ces observations d'alouettes par an
  mutate(ratio1 = n.observationscharbonatio/n.observateurs) #nouvelle colonne qui résume le ratio des deux

ratio.tablcharboNat = left_join(sum.obsNat,sum.charboNatio,by="year") %>% 
  mutate(ratio2 = n.observationscharbonatio/n.observationstotnat) #%>% 
#filter(year != 2014,year !=2001)


ratio.tablcharboNat <- left_join(sum.obsNat, sum.charboNatio, by = "year") %>%
  mutate(ratio2 = n.observationscharbonatio / n.observationstotnat) %>%
  mutate(ratio2_norm = ratio2 / ratio2[year == 2001])
#diviser les valeurs par celle de 2001


# Calcul de la régression linéaire
model <- lm(ratio2_norm ~ year, data = ratio.tablcharboNat)
summary_model <- summary(model)
coeffs <- summary_model$coefficients

# Extraction des valeurs importantes
a <- round(coeffs[1, 1], 4)  # Intercept
b <- round(coeffs[2, 1], 4)  # Pente
r2 <- round(summary_model$r.squared, 4)  # R²
p_value <- round(coeffs[2, 4], 4)  # p-value de la pente

# Calcul des intervalles de confiance à 95%
conf_int <- confint(model, level = 0.95)
conf_low <- round(conf_int[2, 1], 4)
conf_high <- round(conf_int[2, 2], 4)

# Création du texte de la légende
legend_text <- paste0("y = ", a, " + ", b, "x\n",
                      "R² = ", r2, "\n",
                      "p-value = ", p_value, "\n",
                      "IC 95%: [", conf_low, ", ", conf_high, "]")

# Graphique avec ggplot
ggplot(ratio.tablcharboNat, aes(x = year)) +
  geom_smooth(aes(y = ratio2_norm), method = lm, fill = 'purple', color = 'purple') +
  labs(x = "Years", y = "relative abundance",
       title = "Relative abundance of the Great tit over the years at a national scale",
       caption = "", subtitle = "Parus major") +
  theme_classic() +
  geom_point(aes(y = ratio2_norm), colour = 'purple', size = 1) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  annotate("text", x = max(ratio.tablcharboNat$year) - 0,  # Déplacement à droite
           y = max(ratio.tablcharboNat$ratio2_norm) * 0.9,  # Déplacement vers le haut
           label = legend_text, hjust = 1, size = 3.5, color = "black")



# linoote natio -----------------------------------------------------------
sum.linotteNatio = linotte %>% 
  st_drop_geometry() %>% #retire la partie spatial epour faciliter le traitement des données numériques
  group_by(year) %>%  #groupe par année (première colone = année)
  summarize(n.observationslinottenatio = n(), n.observateurs = n_distinct(recordedBy) ) %>% #crée deux nouvelles colonnes
  #nombre d'observations = nombre de ligne alouette par an
  #nombre d'observateurs = nombre de noms différents ayant fait ces observations d'alouettes par an
  mutate(ratio1 = n.observationslinottenatio/n.observateurs) #nouvelle colonne qui résume le ratio des deux

ratio.tabllinotteNat = left_join(sum.obsNat,sum.linotteNatio,by="year") %>% 
  mutate(ratio2 = n.observationslinottenatio/n.observationstotnat) #%>% 
#filter(year != 2014,year !=2001)


ratio.tabllinotteNat <- left_join(sum.obsNat, sum.linotteNatio, by = "year") %>%
  mutate(ratio2 = n.observationslinottenatio / n.observationstotnat) %>%
  mutate(ratio2_norm = ratio2 / ratio2[year == 2001])
#diviser les valeurs par celle de 2001


# Calcul de la régression linéaire
model <- lm(ratio2_norm ~ year, data = ratio.tabllinotteNat)
summary_model <- summary(model)
coeffs <- summary_model$coefficients

# Extraction des valeurs importantes
a <- round(coeffs[1, 1], 4)  # Intercept
b <- round(coeffs[2, 1], 4)  # Pente
r2 <- round(summary_model$r.squared, 4)  # R²
p_value <- round(coeffs[2, 4], 4)  # p-value de la pente

# Calcul des intervalles de confiance à 95%
conf_int <- confint(model, level = 0.95)
conf_low <- round(conf_int[2, 1], 4)
conf_high <- round(conf_int[2, 2], 4)

# Création du texte de la légende
legend_text <- paste0("y = ", a, " + ", b, "x\n",
                      "R² = ", r2, "\n",
                      "p-value = ", p_value, "\n",
                      "IC 95%: [", conf_low, ", ", conf_high, "]")

# Graphique avec ggplot
ggplot(ratio.tabllinotteNat, aes(x = year)) +
  geom_smooth(aes(y = ratio2_norm), method = lm, fill = 'green', color = 'green') +
  labs(x = "Years", y = "relative abundance",
       title = "Relative abundance of the Common linnet over the years at a national scale",
       caption = "", subtitle = "Linaria cannabina") +
  theme_classic() +
  geom_point(aes(y = ratio2_norm), colour = 'green', size = 1) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  annotate("text", x = max(ratio.tabllinotteNat$year) - 0,  # Déplacement à droite
           y = max(ratio.tabllinotteNat$ratio2_norm) * 0.9,  # Déplacement vers le haut
           label = legend_text, hjust = 1, size = 3.5, color = "black")




# fauvette natio ----------------------------------------------------------
sum.FaNatio = fauvette %>% 
  st_drop_geometry() %>% #retire la partie spatial epour faciliter le traitement des données numériques
  group_by(year) %>%  #groupe par année (première colone = année)
  summarize(n.observationsFanatio = n(), n.observateurs = n_distinct(recordedBy) ) %>% #crée deux nouvelles colonnes
  #nombre d'observations = nombre de ligne alouette par an
  #nombre d'observateurs = nombre de noms différents ayant fait ces observations d'alouettes par an
  mutate(ratio1 = n.observationsFanatio/n.observateurs) #nouvelle colonne qui résume le ratio des deux

ratio.tablFaNat = left_join(sum.obsNat,sum.FaNatio,by="year") %>% 
  mutate(ratio2 = n.observationsFanatio/n.observationstotnat) #%>% 
#filter(year != 2014,year !=2001)


ratio.tablFaNat <- left_join(sum.obsNat, sum.FaNatio, by = "year") %>%
  mutate(ratio2 = n.observationsFanatio / n.observationstotnat) %>%
  mutate(ratio2_norm = ratio2 / ratio2[year == 2001])
#diviser les valeurs par celle de 2001


# Calcul de la régression linéaire
model <- lm(ratio2_norm ~ year, data = ratio.tablFaNat)
summary_model <- summary(model)
coeffs <- summary_model$coefficients

# Extraction des valeurs importantes
a <- round(coeffs[1, 1], 4)  # Intercept
b <- round(coeffs[2, 1], 4)  # Pente
r2 <- round(summary_model$r.squared, 4)  # R²
p_value <- round(coeffs[2, 4], 4)  # p-value de la pente

# Calcul des intervalles de confiance à 95%
conf_int <- confint(model, level = 0.95)
conf_low <- round(conf_int[2, 1], 4)
conf_high <- round(conf_int[2, 2], 4)

# Création du texte de la légende
legend_text <- paste0("y = ", a, " + ", b, "x\n",
                      "R² = ", r2, "\n",
                      "p-value = ", p_value, "\n",
                      "IC 95%: [", conf_low, ", ", conf_high, "]")

# Graphique avec ggplot
ggplot(ratio.tablFaNat, aes(x = year)) +
  geom_smooth(aes(y = ratio2_norm), method = lm, fill = 'brown', color = 'brown') +
  labs(x = "Years", y = "relative abundance",
       title = "Relative abundance of the Common whitethroat over the years at a national scale",
       caption = "", subtitle = "Sylvia communis") +
  theme_classic() +
  geom_point(aes(y = ratio2_norm), colour = 'brown', size = 1) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  annotate("text", x = max(ratio.tablFaNat$year) - 0,  # Déplacement à droite
           y = max(ratio.tablFaNat$ratio2_norm) * 0.9,  # Déplacement vers le haut
           label = legend_text, hjust = 1, size = 3.5, color = "black")




# merle nation ------------------------------------------------------------
sum.merleNatio = merle %>% 
  st_drop_geometry() %>% #retire la partie spatial epour faciliter le traitement des données numériques
  group_by(year) %>%  #groupe par année (première colone = année)
  summarize(n.observationsmerlenatio = n(), n.observateurs = n_distinct(recordedBy) ) %>% #crée deux nouvelles colonnes
  #nombre d'observations = nombre de ligne alouette par an
  #nombre d'observateurs = nombre de noms différents ayant fait ces observations d'alouettes par an
  mutate(ratio1 = n.observationsmerlenatio/n.observateurs) #nouvelle colonne qui résume le ratio des deux

ratio.tablmerleNat = left_join(sum.obsNat,sum.merleNatio,by="year") %>% 
  mutate(ratio2 = n.observationsmerlenatio/n.observationstotnat) #%>% 
#filter(year != 2014,year !=2001)


ratio.tablmerleNat <- left_join(sum.obsNat, sum.merleNatio, by = "year") %>%
  mutate(ratio2 = n.observationsmerlenatio / n.observationstotnat) %>%
  mutate(ratio2_norm = ratio2 / ratio2[year == 2001])
#diviser les valeurs par celle de 2001


# Calcul de la régression linéaire
model <- lm(ratio2_norm ~ year, data = ratio.tablmerleNat)
summary_model <- summary(model)
coeffs <- summary_model$coefficients

# Extraction des valeurs importantes
a <- round(coeffs[1, 1], 4)  # Intercept
b <- round(coeffs[2, 1], 4)  # Pente
r2 <- round(summary_model$r.squared, 4)  # R²
p_value <- round(coeffs[2, 4], 4)  # p-value de la pente

# Calcul des intervalles de confiance à 95%
conf_int <- confint(model, level = 0.95)
conf_low <- round(conf_int[2, 1], 4)
conf_high <- round(conf_int[2, 2], 4)

# Création du texte de la légende
legend_text <- paste0("y = ", a, " + ", b, "x\n",
                      "R² = ", r2, "\n",
                      "p-value = ", p_value, "\n",
                      "IC 95%: [", conf_low, ", ", conf_high, "]")

# Graphique avec ggplot
ggplot(ratio.tablmerleNat, aes(x = year)) +
  geom_smooth(aes(y = ratio2_norm), method = lm, fill = 'coral', color = 'coral') +
  labs(x = "Years", y = "relative abundance",
       title = "Relative abundance of the Common blackbird over the years at a national scale",
       caption = "", subtitle = "Turdus merula") +
  theme_classic() +
  geom_point(aes(y = ratio2_norm), colour = 'coral', size = 1) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  annotate("text", x = max(ratio.tablmerleNat$year) - 0,  # Déplacement à droite
           y = max(ratio.tablmerleNat$ratio2_norm) * 0.9,  # Déplacement vers le haut
           label = legend_text, hjust = 1, size = 3.5, color = "black")




# resultats IC ------------------------------------------------------------


# Prédictions avec la pente centrale
y2001 <- a + b * 2001
y2021 <- a + b * 2021
perc_decline <- (y2001 - y2021) / y2001 * 100

# Intervalle de confiance (min et max)
b_min <- conf_low
b_max <- conf_high

y2001_min <- a + b_min * 2001
y2021_min <- a + b_min * 2021
decline_min <- (y2001_min - y2021_min) / y2001_min * 100

y2001_max <- a + b_max * 2001
y2021_max <- a + b_max * 2021
decline_max <- (y2001_max - y2021_max) / y2001_max * 100

# Résumé
cat(sprintf("Baisse centrale : %.2f%%\n", perc_decline))
cat(sprintf("IC 95%% : [%.2f%%, %.2f%%]\n", decline_min, decline_max))
