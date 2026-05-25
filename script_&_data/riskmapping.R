#arbovirus-risk_mapping

#importe datset

#setwd(...) insérer votre chemin d'accès vers le dossier d'input/ouput

records_arbovirus = read.csv("Arbovirus_risk_mapping_simulated_dataset_1-2.csv", head=T)

#visualisation des données

head(records_arbovirus)
str(records_arbovirus)

#installation des pacakages nécessaires

if (!require(sp)) install.packages("sp") # to install the package if not installed
if (!require(raster)) install.packages("raster")
if (!require(colorspace)) install.packages("colorspace")
if (!require(MetBrewer)) install.packages("MetBrewer")
if (!require(RColorBrewer)) install.packages("RColorBrewer")
if (!require(raster)) install.packages("raster")
if (!require(sf)) install.packages("sf")
if (!require(blockCV)) install.packages("blockCV")
if (!require(dismo)) install.packages("dismo")
if (!require(gbm)) install.packages("gbm")
if (!require(gbm)) install.packages("ncf")
library(raster); library(sf); library(sp); library(RColorBrewer)
library(blockCV); library(dismo); library(gbm); library(ncf)
library(sp) 
library(colorspace); library(MetBrewer); 


#importation des shapefiles

borders = shapefile("Coasts_study_area_shapefile/Coasts_study_area_shapefile.shp")
envVariableNames = c(
  "human_pop_density_log10", 
  "croplands_all_categories", 
  "managed_pasture_and_rangeland",
  "precipitation_inFall", 
  "precipitation_spring", 
  "precipitation_summer", 
  "precipitation_winter",
  "primary_forest_areas", 
  "primary_non-forest_areas",
  "relative_humidity_inFall", 
  "relative_humidity_spring", 
  "relative_humidity_summer", 
  "relative_humidity_winter",
  "secondary_forest_areas", 
  "secondary_non-forest_areas",
  "temperature_inFall", 
  "temperature_spring", 
  "temperature_summer", 
  "temperature_winter"
)

envVariables = list() #  On crée une liste vide pour stocker les rasters

#  On lance une boucle pour importer chaque fichier
for (i in 1:length(envVariableNames)) {
  
  fileName = paste0("Environmental_raster_files/Raster_", envVariableNames[i], ".asc")
  envVariables[[i]] = raster(fileName)
  
  # On donne un nom propre (on remplace les éventuels tirets par des underscores pour éviter les bugs)
  names(envVariables[[i]]) = gsub("-", "_", envVariableNames[i])
}
envVariableNames <- gsub("-", "_", envVariableNames) #on remplace aussi les tirets dans envVariableNames pour eviter tout probleme lors de la BRT

#-------------------------------------------------------------------------
#affichage des données et des variables environnementales
#-------------------------------------------------------------------------

#affichage des infections
plot(borders, col=NA, border=NA)
plot(borders, col=NA, border="gray30", lwd=0.5, add=T) 

# couleurs transparentes
rouge_transparent <- adjustcolor("red", alpha.f = 0.3)
noir_transparent <- adjustcolor("black", alpha.f = 0.3)

# trace les points
for (i in dim(records_arbovirus)[1]:1) {
  points(records_arbovirus[i, c("longitude","latitude")], 
         pch=16, cex=0.7, col=rouge_transparent)
  points(records_arbovirus[i, c("longitude","latitude")], 
         pch=1, cex=0.7, lwd=0.4, col=noir_transparent)
}
#--------------------------------------------------------------------------
#affichage des variabes environnementales

par(mfrow=c(4,5), oma=c(0,0,1.5,0), mar=c(1,0,1,0), lwd=0.2,
    col="gray30", col.axis="gray30", fg="gray30")

#1) TEMPERATURES

Tlimites=c(-10,40)#bornes pour une légende uniforme entre les saisons

#Automne
cols = colorRampPalette(brewer.pal(9,"YlOrRd"))(150)[1:100]
plot(envVariables[[16]], col=cols,zlim=Tlimites, ann=F, legend=F, axes=F, box=F)

plot(borders, add=T, border="gray50", lwd=0.5)
mtext("Air temperature", side=3, line=0.3, cex=0.65, col="gray30")
mtext("in fall (°C)", side=3, line=-0.7, cex=0.65, col="gray30")
plot(envVariables[[16]], col=cols,zlim=Tlimites, legend.only=T, add=T, legend.width=0.5, legend.shrink=0.3,
     smallplot=c(0.10,0.80,0.09,0.12), adj=3, axis.args=list(cex.axis=0.7, lwd=0, lwd.tick=0.2,
                                                             col.tick="gray30", tck=-0.9, col="gray30", col.axis="gray30", line=0, mgp=c(0,0.1,0)),
     alpha=1, side=3, horizontal=T)

#pringtemps
plot(envVariables[[17]], col=cols,zlim=Tlimites, ann=F, legend=F, axes=F, box=F)
plot(borders, add=T, border="gray50", lwd=0.5)
mtext("Air temperature", side=3, line=0.3, cex=0.65, col="gray30")
mtext("in spring (°C)", side=3, line=-0.7, cex=0.65, col="gray30")
plot(envVariables[[17]], col=cols,zlim=Tlimites, legend.only=T, add=T, legend.width=0.5, legend.shrink=0.3,
     smallplot=c(0.10,0.80,0.09,0.12), adj=3, axis.args=list(cex.axis=0.7, lwd=0, lwd.tick=0.2,
                                                             col.tick="gray30", tck=-0.9, col="gray30", col.axis="gray30", line=0, mgp=c(0,0.1,0)),
     alpha=1, side=3, horizontal=T)

#été
plot(envVariables[[18]], col=cols,zlim=Tlimites, ann=F, legend=F, axes=F, box=F)
plot(borders, add=T, border="gray50", lwd=0.5)
mtext("Air temperature", side=3, line=0.3, cex=0.65, col="gray30")
mtext("in summer (°C)", side=3, line=-0.7, cex=0.65, col="gray30")
plot(envVariables[[18]], col=cols,zlim=Tlimites, legend.only=T, add=T, legend.width=0.5, legend.shrink=0.3,
     smallplot=c(0.10,0.80,0.09,0.12), adj=3, axis.args=list(cex.axis=0.7, lwd=0, lwd.tick=0.2,
                                                             col.tick="gray30", tck=-0.9, col="gray30", col.axis="gray30", line=0, mgp=c(0,0.1,0)),
     alpha=1, side=3, horizontal=T)

#Hiver
plot(envVariables[[19]], col=cols,zlim=Tlimites, ann=F, legend=F, axes=F, box=F)
plot(borders, add=T, border="gray50", lwd=0.5)
mtext("Air temperature", side=3, line=0.3, cex=0.65, col="gray30")
mtext("in winter (°C)", side=3, line=-0.7, cex=0.65, col="gray30")
plot(envVariables[[19]], col=cols,zlim=Tlimites, legend.only=T, add=T, legend.width=0.5, legend.shrink=0.3,
     smallplot=c(0.10,0.80,0.09,0.12), adj=3, axis.args=list(cex.axis=0.7, lwd=0, lwd.tick=0.2,
                                                             col.tick="gray30", tck=-0.9, col="gray30", col.axis="gray30", line=0, mgp=c(0,0.1,0)),
     alpha=1, side=3, horizontal=T)

#2) POPULATION

cols = colorRampPalette(brewer.pal(9,"BuPu"))(150)[1:100]
plot(envVariables[[1]], col=cols, ann=F, legend=F, axes=F, box=F)
plot(borders, add=T, border="gray50", lwd=0.5)
mtext("Human", side=3, line=0.3, cex=0.65, col="gray30")
mtext(expression("population (log"[10]*")"), side=3, line=-0.7, cex=0.65, col="gray30")
plot(envVariables[[1]], col=cols, legend.only=T, add=T, legend.width=0.5, legend.shrink=0.3,
     smallplot=c(0.10,0.80,0.09,0.12), adj=3, axis.args=list(cex.axis=0.7, lwd=0, lwd.tick=0.2,
                                                             col.tick="gray30", tck=-0.9, col="gray30", col.axis="gray30", line=0, mgp=c(0,0.1,0)),
     alpha=1, side=3, horizontal=T)

#3) PRECIPITATIONS 

Plimites=c(0,10)#bornes pour une légende uniforme entre les saisons

#Automne
cols = colorRampPalette(brewer.pal(9,"YlGnBu"))(100)
plot(envVariables[[4]], col=cols,zlim=Plimites, ann=F, legend=F, axes=F, box=F)
plot(borders, add=T, border="gray50", lwd=0.5)
mtext("Precipitation in fall", side=3, line=0.3, cex=0.65, col="gray30")
mtext(expression("(kg/m"^2*"/day)"), side=3, line=-0.7, cex=0.65, col="gray30")
mtext(expression("(kg/m"^2*"/day)"), side=3, line=-0.7, cex=0.65, col="gray30")
plot(envVariables[[4]], col=cols,zlim=Plimites, legend.only=T, add=T, legend.width=0.5, legend.shrink=0.3,
     smallplot=c(0.10,0.80,0.09,0.12), adj=3, axis.args=list(cex.axis=0.7, lwd=0, lwd.tick=0.2,
                                                             col.tick="gray30", tck=-0.9, col="gray30", col.axis="gray30", line=0, mgp=c(0,0.1,0)),
     alpha=1, side=3, horizontal=T)
                                                             
#Pringtemps
cols = colorRampPalette(brewer.pal(9,"YlGnBu"))(100)
plot(envVariables[[5]], col=cols,zlim=Plimites, ann=F, legend=F, axes=F, box=F)
plot(borders, add=T, border="gray50", lwd=0.5)
mtext("Precipitation in spring", side=3, line=0.3, cex=0.65, col="gray30")
mtext(expression("(kg/m"^2*"/day)"), side=3, line=-0.7, cex=0.65, col="gray30")
plot(envVariables[[5]], col=cols,zlim=Plimites, legend.only=T, add=T, legend.width=0.5, legend.shrink=0.3,
     smallplot=c(0.10,0.80,0.09,0.12), adj=3, axis.args=list(cex.axis=0.7, lwd=0, lwd.tick=0.2,
                                                             col.tick="gray30", tck=-0.9, col="gray30", col.axis="gray30", line=0, mgp=c(0,0.1,0)),
     alpha=1, side=3, horizontal=T)

#été
cols = colorRampPalette(brewer.pal(9,"YlGnBu"))(100)
plot(envVariables[[6]], col=cols,zlim=Plimites, ann=F, legend=F, axes=F, box=F)
plot(borders, add=T, border="gray50", lwd=0.5)
mtext("Precipitation in summer", side=3, line=0.3, cex=0.65, col="gray30")
mtext(expression("(kg/m"^2*"/day)"), side=3, line=-0.7, cex=0.65, col="gray30")
plot(envVariables[[6]], col=cols,zlim=Plimites, legend.only=T, add=T, legend.width=0.5, legend.shrink=0.3,
     smallplot=c(0.10,0.80,0.09,0.12), adj=3, axis.args=list(cex.axis=0.7, lwd=0, lwd.tick=0.2,
                                                             col.tick="gray30", tck=-0.9, col="gray30", col.axis="gray30", line=0, mgp=c(0,0.1,0)),
     alpha=1, side=3, horizontal=T)

#Hiver
cols = colorRampPalette(brewer.pal(9,"YlGnBu"))(100)
plot(envVariables[[7]], col=cols,zlim=Plimites, ann=F, legend=F, axes=F, box=F)
plot(borders, add=T, border="gray50", lwd=0.5)
mtext("Precipitation in winter", side=3, line=0.3, cex=0.65, col="gray30")
mtext(expression("(kg/m"^2*"/day)"), side=3, line=-0.7, cex=0.65, col="gray30")
plot(envVariables[[7]], col=cols,zlim=Plimites, legend.only=T, add=T, legend.width=0.5, legend.shrink=0.3,
     smallplot=c(0.10,0.80,0.09,0.12), adj=3, axis.args=list(cex.axis=0.7, lwd=0, lwd.tick=0.2,
                                                             col.tick="gray30", tck=-0.9, col="gray30", col.axis="gray30", line=0, mgp=c(0,0.1,0)),
     alpha=1, side=3, horizontal=T)

#4) TERRES AGRICOLES
cols = colorRampPalette(c("gray97","navajowhite4"),bias=1)(100)
plot(envVariables[[2]], col=cols, ann=F, legend=F, axes=F, box=F)
plot(borders, add=T, border="gray50", lwd=0.5)
mtext("Croplands", side=3, line=0.3, cex=0.65, col="gray30")
plot(envVariables[[2]], col=cols, legend.only=T, add=T, legend.width=0.5, legend.shrink=0.3,
     smallplot=c(0.10,0.80,0.09,0.12), adj=3, axis.args=list(cex.axis=0.7, lwd=0, lwd.tick=0.2,
                                                             col.tick="gray30", tck=-0.9, col="gray30", col.axis="gray30", line=0, mgp=c(0,0.1,0)),
     alpha=1, side=3, horizontal=T)


#5) FORETS

#forêts primaires
cols = colorRampPalette(c("gray97","chartreuse4"),bias=1)(100)
plot(envVariables[[8]], col=cols, ann=F, legend=F, axes=F, box=F)
plot(borders, add=T, border="gray50", lwd=0.5)
mtext("Forested", side=3, line=0.3, cex=0.65, col="gray30")
mtext("primary land", side=3, line=-0.7, cex=0.65, col="gray30")
plot(envVariables[[8]], col=cols, legend.only=T, add=T, legend.width=0.5, legend.shrink=0.3,
     smallplot=c(0.10,0.80,0.09,0.12), adj=3, axis.args=list(cex.axis=0.7, lwd=0, lwd.tick=0.2,
                                                             col.tick="gray30", tck=-0.9, col="gray30", col.axis="gray30", line=0, mgp=c(0,0.1,0)),
     alpha=1, side=3, horizontal=T)

#forêts secondaires
cols = colorRampPalette(c("gray97","olivedrab3"),bias=1)(100)
plot(envVariables[[14]], col=cols, ann=F, legend=F, axes=F, box=F)
plot(borders, add=T, border="gray50", lwd=0.5)
mtext("Forested", side=3, line=0.3, cex=0.65, col="gray30")
mtext("secondary land", side=3, line=-0.7, cex=0.65, col="gray30")
plot(envVariables[[14]], col=cols, legend.only=T, add=T, legend.width=0.5, legend.shrink=0.3,
     smallplot=c(0.10,0.80,0.09,0.12), adj=3, axis.args=list(cex.axis=0.7, lwd=0, lwd.tick=0.2,
                                                             col.tick="gray30", tck=-0.9, col="gray30", col.axis="gray30", line=0, mgp=c(0,0.1,0)),
     alpha=1, side=3, horizontal=T)

#6)ZONE NON FORESTIERES

#zone non forestières primaires
cols = colorRampPalette(c("gray97","navajowhite2"),bias=1)(100)
plot(envVariables[[8]], col=cols, ann=F, legend=F, axes=F, box=F)
plot(borders, add=T, border="gray50", lwd=0.5)
mtext("primary", side=3, line=0.3, cex=0.65, col="gray30")
mtext("non-forested areas", side=3, line=-0.7, cex=0.65, col="gray30")
plot(envVariables[[8]], col=cols, legend.only=T, add=T, legend.width=0.5, legend.shrink=0.3,
     smallplot=c(0.10,0.80,0.09,0.12), adj=3, axis.args=list(cex.axis=0.7, lwd=0, lwd.tick=0.2,
                                                             col.tick="gray30", tck=-0.9, col="gray30", col.axis="gray30", line=0, mgp=c(0,0.1,0)),
     alpha=1, side=3, horizontal=T)


#zone non forestières secondaires
cols = colorRampPalette(c("gray97","navajowhite3"),bias=1)(100)
plot(envVariables[[15]], col=cols, ann=F, legend=F, axes=F, box=F)
plot(borders, add=T, border="gray50", lwd=0.5)
mtext("secondary ", side=3, line=0.3, cex=0.65, col="gray30")
mtext("non-forested areas", side=3, line=-0.7, cex=0.65, col="gray30")
plot(envVariables[[15]], col=cols, legend.only=T, add=T, legend.width=0.5, legend.shrink=0.3,
     smallplot=c(0.10,0.80,0.09,0.12), adj=3, axis.args=list(cex.axis=0.7, lwd=0, lwd.tick=0.2,
                                                             col.tick="gray30", tck=-0.9, col="gray30", col.axis="gray30", line=0, mgp=c(0,0.1,0)),
     alpha=1, side=3, horizontal=T)


#7)PATURAGES/RANGE

cols = colorRampPalette(c("gray97","burlywood3"),bias=1)(100)
plot(envVariables[[3]], col=cols, ann=F, legend=F, axes=F, box=F)
plot(borders, add=T, border="gray50", lwd=0.5)
mtext("Pastures", side=3, line=0.3, cex=0.65, col="gray30")
mtext("and rangeland", side=3, line=-0.7, cex=0.65, col="gray30")
plot(envVariables[[3]], col=cols, legend.only=T, add=T, legend.width=0.5, legend.shrink=0.3,
     smallplot=c(0.10,0.80,0.09,0.12), adj=3, axis.args=list(cex.axis=0.7, lwd=0, lwd.tick=0.2,
                                                             col.tick="gray30", tck=-0.9, col="gray30", col.axis="gray30", line=0, mgp=c(0,0.1,0)),
     alpha=1, side=3, horizontal=T)

#8)HUMIDITE RELATIVE

RHlimites=c(0,100)

#Automne
cols = colorRampPalette(c("white","blue"),bias=1)(100)
plot(envVariables[[10]], col=cols,zlim=RHlimites, ann=F, legend=F, axes=F, box=F)
plot(borders, add=T, border="gray50", lwd=0.5)
mtext("Relative Humidity ", side=3, line=0.3, cex=0.65, col="gray30")
mtext("in fall", side=3, line=-0.7, cex=0.65, col="gray30")
plot(envVariables[[10]], col=cols,zlim=RHlimites, legend.only=T, add=T, legend.width=0.5, legend.shrink=0.3,
     smallplot=c(0.10,0.80,0.09,0.12), adj=3, axis.args=list(cex.axis=0.7, lwd=0, lwd.tick=0.2,
                                                             col.tick="gray30", tck=-0.9, col="gray30", col.axis="gray30", line=0, mgp=c(0,0.1,0)),
     alpha=1, side=3, horizontal=T)

#Pringtemps
cols = colorRampPalette(c("white","blue"),bias=1)(100)
plot(envVariables[[11]], col=cols,zlim=RHlimites, ann=F, legend=F, axes=F, box=F)
plot(borders, add=T, border="gray50", lwd=0.5)
mtext("Relative Humidity ", side=3, line=0.3, cex=0.65, col="gray30")
mtext("in spring", side=3, line=-0.7, cex=0.65, col="gray30")
plot(envVariables[[11]], col=cols,zlim=RHlimites, legend.only=T, add=T, legend.width=0.5, legend.shrink=0.3,
     smallplot=c(0.10,0.80,0.09,0.12), adj=3, axis.args=list(cex.axis=0.7, lwd=0, lwd.tick=0.2,
                                                             col.tick="gray30", tck=-0.9, col="gray30", col.axis="gray30", line=0, mgp=c(0,0.1,0)),
     alpha=1, side=3, horizontal=T)

#été
cols = colorRampPalette(c("white","blue"),bias=1)(100)
plot(envVariables[[12]], col=cols,zlim=RHlimites, ann=F, legend=F, axes=F, box=F)
plot(borders, add=T, border="gray50", lwd=0.5)
mtext("Relative Humidity ", side=3, line=0.3, cex=0.65, col="gray30")
mtext("in summer", side=3, line=-0.7, cex=0.65, col="gray30")
plot(envVariables[[12]], col=cols,zlim=RHlimites, legend.only=T, add=T, legend.width=0.5, legend.shrink=0.3,
     smallplot=c(0.10,0.80,0.09,0.12), adj=3, axis.args=list(cex.axis=0.7, lwd=0, lwd.tick=0.2,
                                                             col.tick="gray30", tck=-0.9, col="gray30", col.axis="gray30", line=0, mgp=c(0,0.1,0)),
     alpha=1, side=3, horizontal=T)

#Hiver
cols = colorRampPalette(c("white","blue"),bias=1)(100)
plot(envVariables[[13]], col=cols,zlim=RHlimites, ann=F, legend=F, axes=F, box=F)
plot(borders, add=T, border="gray50", lwd=0.5)
mtext("Relative Humidity ", side=3, line=0.3, cex=0.65, col="gray30")
mtext("in winter", side=3, line=-0.7, cex=0.65, col="gray30")
plot(envVariables[[13]], col=cols,zlim=RHlimites, legend.only=T, add=T, legend.width=0.5, legend.shrink=0.3,
     smallplot=c(0.10,0.80,0.09,0.12), adj=3, axis.args=list(cex.axis=0.7, lwd=0, lwd.tick=0.2,
                                                             col.tick="gray30", tck=-0.9, col="gray30", col.axis="gray30", line=0, mgp=c(0,0.1,0)),
     alpha=1, side=3, horizontal=T)


#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
#                         RISK MAPPING
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
#I.PSEUDO-ABSENCES
#-----------------------------------------------------------------------------

# 1. Préparation du background pour les pseudo-absences

# Création d'un template : Terre = 1, Océans = NA (pour éviter d'échantillonner en mer)
template = envVariables[[1]]
template[!is.na(template[])] = 1 

# Identification des indices des cellules contenant déjà une présence du virus
cells_arbo = unique(raster::extract(template, records_arbovirus, cellnumbers=T))[,"cells"]

background = template 

#plot(background) # Vérification visuelle (on devrait avoir l'Europe entière)

# Exclusion des cellules avec au moins une présence connues
background[(1:length(background[]))%in%cells_arbo] = NA 

#plot(background) # Vérification visuelle (on devrait avoir l'Europe entière sans les zones de présence)
cells_background = which(!is.na(background[])) 

#-------------------------------------------------------------
#II. création des pseudo-absences

number_of_replicates = 100 #on chosit 100 réplicats
dataframes = list() # the list object in each the data frame for each BRT analysis will be stored
for (i in 1:number_of_replicates) {
  presences = xyFromCell(template,cells_arbo)
  cells_pseudo_absence = sample(cells_background, length(cells_arbo), replace=F)
  pseudo_absences = xyFromCell(template, cells_pseudo_absence)
  data = rbind(cbind(rep(1,dim(presences)[1]),presences),
               cbind(rep(0,dim(pseudo_absences)[1]),pseudo_absences))
  colnames(data) = c("response", "longitude", "latitude") # names of three first columns
  data = cbind(data, raster::extract(stack(envVariables), data[,c(2:3)]))
  dataframes[[i]] = as.data.frame(data)
  if (i == 1) print(str(dataframes[[i]]))
}

# II. création des pseudo-absences

number_of_replicates = 100 # on choisit 100 réplicats
dataframes = list() # liste pour stocker chaque jeu de données (un par réplicat)

for (i in 1:number_of_replicates) {
  
  # Extraction des coordonnées des présences (cellules avec au moins 1 cas)
  presences = xyFromCell(template, cells_arbo)
  
  # Sélection aléatoire des pseudo-absences (même nombre que les présences)
  cells_pseudo_absence = sample(cells_background, length(cells_arbo), replace=F)
  pseudo_absences = xyFromCell(template, cells_pseudo_absence) # on récupère leurs coordonnées
  
  # Assemblage des présences (1) et pseudo-absences (0) dans un seul tableau
  data = rbind(cbind(rep(1,dim(presences)[1]), presences),
               cbind(rep(0,dim(pseudo_absences)[1]), pseudo_absences))
  colnames(data) = c("response", "longitude", "latitude") 
  
  # Ajout des valeurs environnementales extraites pour chaque point
  data = cbind(data, raster::extract(stack(envVariables), data[,c(2:3)]))
  
  # Conversion en dataframe et sauvegarde dans la liste
  dataframes[[i]] = as.data.frame(data)
  
  # Affichage du tableau pour le premier réplicat (vérification)
  if (i == 1) print(str(dataframes[[i]])) 
}

#III.visualisation présences/pseudo absences(sur 7réplicats)

par(mfrow=c(1,7), oma=c(0,0,1.5,0), mar=c(0.75,0,0,0))

# Boucle pour les 7 premiers jeux de données
for (i in 1:7) {
  
  # On extrait le jeu de données actuel
  data_visu <- dataframes[[i]]
  
  # On sépare les présences (1) et les pseudo-absences (0)
  presences <- data_visu[data_visu$response == 1, ]
  absences <- data_visu[data_visu$response == 0, ]
  
  # On trace la carte de fond 
  plot(borders, border="gray50", lwd=0.5)
  mtext(paste("Replicat", i), side=3, line=-1, cex=0.7)
  
  # On ajoute les pseudo-absences (en bleu) puis les présences (en rouge)
  points(absences$longitude, absences$latitude, col=adjustcolor("blue", alpha.f = 0.5), pch=16, cex=0.7)
  points(presences$longitude, presences$latitude, col= adjustcolor("red", alpha.f = 0.5), pch=16, cex=0.7)
}
#------------------------------------------------------------------------------
# III. Autocorrélation et corrélogramme spatiaux
#------------------------------------------------------------------------------

# Configuration de la fenêtre graphique
par(mfrow=c(1,1), oma=c(0,0,0,0), mar=c(2,2.2,0,0))

for (i in 1:number_of_replicates) {
  
  data = dataframes[[i]]
  
  # Calcul de l'autocorrélation spatiale (présence/absence selon la distance)
  correlogram = ncf::correlog(data[,"longitude"], data[,"latitude"], data[,"response"],
                              na.rm=T, increment=100, resamp=0, latlon=T)
  
  # Création du "fond" du graphique (exécuté uniquement au 1er réplicat(i=1))
  if (i == 1) { 
    plot(correlogram$mean.of.class, correlogram$correlation, ann=F, axes=F,
         lwd=0.2, cex=0.5, col=NA, ylim=c(-1,1.03), xlim=c(250,5700))
    
    # Ajout d'une ligne horizontale rouge pointillée à 0 (aucune corrélation)
    abline(h=0, lwd=0.5, col=rgb(222,67,39,255,maxColorValue=255), lty=2)
    
    # Paramétrage des axes X (Distance) et Y (Corrélation)
    axis(side=1, lwd.tick=0.2, cex.axis=0.7, lwd=0.2, tck=-0.03,
         col.axis="gray30", mgp=c(0,0.00,0), at=seq(0,9000,1000))
    axis(side=2, lwd.tick=0.2, cex.axis=0.7, lwd=0.2, tck=-0.03,
         col.axis="gray30", mgp=c(0,0.25,0), at=seq(-1.5,3,0.5))
    title(xlab="Distance (km)", cex.lab=0.9, mgp=c(0.9,0,0), col.lab="gray30")
    title(ylab="Correlation", cex.lab=0.9, mgp=c(1.3,0,0), col.lab="gray30")
  }
  
  # 4. Ajout de la courbe d'autocorrélation du réplicat actuel par-dessus
  lines(correlogram$mean.of.class[-1], correlogram$correlation[-1], lwd=0.1, col="gray60")
}

# Les courbes se croisent toutes un peu avant 2000 km --> on va prendre 2000 km
#------------------------------------------------------------------------------
# IV. Paramétrisation de l'algorithme BRT
#-------------------------------------------------------------------------------

theRanges = c(2000, 2000) * 1000 # Distance spatiale en mètres(donc *1000) issue du corrélogramme (2000 km)
gbm.x = envVariableNames         # variables explicatives 
gbm.y = "response"               # variable à prédire (1 = présence, 0 = absence)

offset = NULL
tree.complexity = 5              # Complexité des arbres (nombre de branchements/nœuds)
learning.rate = 0.005            # Taux d'apprentissage (influence de chaque nouvel arbre)
bag.fraction = 0.80              # Proportion des données utilisées à chaque itération (80%)
site.weights = rep(1, dim(data)[1])  # Poids égal pour tous les points géographiques
var.monotone = rep(0, length(gbm.x)) # On n'impose aucune contrainte de forme sur les courbes (0)

n.folds = 5                      # Nombre de blocs pour la validation croisée spatiale
prev.stratify = TRUE             # Ignoré ici car on fera nos blocs manuellement avec "cv_spatial"
family = "bernoulli"             # Loi mathématique adaptée aux données binaires (0 ou 1)

n.trees = 10                     # Nombre de départ pour les arbres
step.size = 5                    # Nombre d'arbres ajoutés à chaque étape d'évaluation
max.trees = 10000                # Sécurité : nombre maximum d'arbres calculés
tolerance.method = "auto"        # Méthode d'arrêt automatique de l'algorithme
tolerance = 0.001                # Seuil d'arrêt (s'arrête quand l'erreur ne baisse presque plus)

# Paramètres d'affichage et de gestion de la mémoire
plot.main = TRUE                 # Afficher le graphique d'évolution de l'erreur
plot.folds = FALSE               
verbose = TRUE                   # Afficher le texte de progression dans la console
silent = FALSE                   
keep.fold.models = FALSE         # Ne pas conserver les détails des blocs pour alléger la RAM
keep.fold.vector = FALSE         
keep.fold.fit = FALSE
#-------------------------------------------
# V. Lancement des modèles BRT (Boosted Regression Trees)
#-------------------------------------------


# 1. Préparation des listes pour stocker les résultats
brt_models = list()   # Pour stocker les 100 modèles entraînés
predictions = list()  # Pour stocker les 100 cartes de prédictions (rasters)
AUCs = matrix(nrow=number_of_replicates, ncol=1) # Matrice pour stocker le score de chaque modèle
colnames(AUCs) = c("AUC")

#-------------- Résolution de problème(problème des tirets dans envVariables résolu au début du code) -------------------------
# Test que nous avions fait pour trouver le problème
colonnes_demandees <- c("response", envVariableNames)
colonnes_existantes <- colnames(data)
colonnes_manquantes <- setdiff(colonnes_demandees, colonnes_existantes)

print("Voici la ou les colonnes introuvables :")
print(colonnes_manquantes)
# avant "primary_non-forest_areas" et "secondary_non-forest_areas" ressortaient, 
# c'est le problème des tirets ("-"). 
# Solutionné en amont par : envVariableNames <- gsub("-", "_", envVariableNames)
#--------------------------------------------------------------
# --- BOUCLE PRINCIPALE (Prend du temps à tourner,à ne lancer qu'une seule fois,on travaillera avec les fichiers sauvegardés!) ---

for (i in 1:number_of_replicates) {
  
  data = dataframes[[i]] # On prend le jeu de données de ce réplicat
  
  # Création de l'objet spatial requis pour la validation croisée
  spdf = SpatialPointsDataFrame(data[c("longitude","latitude")],
                                data[,c("response",envVariableNames)],
                                proj4string=crs(template))
  
  # Découpage de l'espace en folds pour tester le modèle
  myblocks = cv_spatial(spdf, column="response", k=n.folds, size=theRanges[1], selection="random")
  fold.vector = myblocks$folds_ids # On récupère le numéro de bloc pour chaque point
  
  # Lancement de l'algorithme BRT
  n.trees = 10 # On réinitialise le nombre d'arbres de départ à chaque tour
  brt_models[[i]] = gbm.step(data, gbm.x, gbm.y, offset, fold.vector, tree.complexity,
                             learning.rate, bag.fraction, site.weights, var.monotone,
                             n.folds, prev.stratify, family, n.trees, step.size,
                             max.trees, tolerance.method, tolerance, plot.main,
                             plot.folds, verbose, silent, keep.fold.models,
                             keep.fold.vector, keep.fold.fit)
  
  # On récupère et stocke la performance globale (AUC) de ce modèle
  AUCs[i,"AUC"] = brt_models[[i]]$cv.statistics$discrimination.mean
  
  # Prédiction sur toute la carte
  dataframe = as.data.frame(stack(envVariables)) # Transformation des rasters en tableau
  dataframe = dataframe[which(!is.na(rowMeans(dataframe))),] # Suppression des pixels vides (océans, etc.)
  
  n.trees = brt_models[[i]]$gbm.call$best.trees # On utilise le nombre optimal d'arbres trouvé
  type = "response"
  single.tree = FALSE 
  prediction = predict.gbm(brt_models[[i]], dataframe, n.trees, type, single.tree) # Calcul du risque
  
  # Reconversion des prédictions mathématiques en image (raster)
  buffer = envVariables[[1]]
  buffer[!is.na(buffer[])] = prediction
  predictions[[i]] = buffer
}

#SAUVEGARDE DES RESULTATS  
write.csv(AUCs, "AUC_value_replicates.csv", row.names=F, quote=F) # Sauvegarde des scores
saveRDS(brt_models, "BRT_model_replicates.rds") # Sauvegarde des modèles mathématiques
saveRDS(predictions, "BRT_model_predictions.rds") # Sauvegarde des cartes générées

#-------------------------------------------------------------------------------
#VI.visualisation CV_spatiale
#-------------------------------------------------------------------------------
par(mfrow=c(1,4), oma=c(0,0,1.5,0), mar=c(0.75,1,0,1))

# On prépare 5 couleurs pour les 5 folds
couleurs_folds <- c("red", "blue", "green", "orange", "purple")

#  Boucle pour les 4 premiers réplicats
for (i in 1:4) {
  data_visu <- dataframes[[i]]
  
  # On recrée l'objet spatial 
  spdf <- SpatialPointsDataFrame(data_visu[c("longitude", "latitude")],
                                 data_visu[, c("response", envVariableNames)],
                                 proj4string = crs(borders))
  # On lance cv_spatial 
  myblocks <- cv_spatial(spdf, column="response", k=n.folds, size=theRanges[1], 
                         selection="random", plot = FALSE)
  
  fold.vector <- myblocks$folds_ids
  
  # --- TRACÉ DES COUCHES  ---
  # Couche 1 : La carte de fond
  plot(borders, border="gray50", lwd=0.5)
  mtext(paste("Folds - Rep", i), side=3, line=-1, cex=0.7)
  
  # Couche 2 : Les hexagones
  # myblocks$blocks contient les polygones. On utilise st_geometry pour les extraire proprement.
  plot(st_geometry(myblocks$blocks), add=TRUE, border="gray30", lwd=0.5)
  
  # Couche 3 : Les points par-dessus
  points(data_visu$longitude, data_visu$latitude, col=couleurs_folds[fold.vector], pch=16, cex=0.5)
  
  #  LÉGENDE ---
  legend("bottomleft", 
         legend = c("Fold 1", "Fold 2", "Fold 3", "Fold 4", "Fold 5"),
         col = couleurs_folds, 
         pch = 16,            # Symbole des points
         cex = 0.6,           # Taille du texte (réduite pour que ça rentre)
         bty = "n",           # "n" pour ne pas dessiner de cadre autour de la légende
         inset = c(0.02, 0.05)) # Pour décaler légèrement la légende du bord
}


#------------------------------------------------------------------------------
# VII. Cartographie du risque : Prédictions individuelles, moyenne et incertitude
#------------------------------------------------------------------------------

# --- 1. Visualisation des 7 premiers réplicats ---

# Chargement des prédictions sauvegardées à l'étape précédente
predictions = readRDS("BRT_model_predictions.rds")

# Paramètres graphiques : 1 ligne, 7 colonnes
par(mfrow=c(1,7), oma=c(0,0,1.5,0), mar=c(0.75,0,0,0), lwd=0.2, col="gray30",
    col.axis="gray30", fg="gray30") 

# Définition de la palette de couleurs
cols = rev(colorRampPalette(brewer.pal(9,"RdYlBu"))(131)[11:121])

for (i in 1:7) { # On ne trace que les 7 premiers réplicats 
  
  # Affichage de la carte de prédiction (risque)
  plot(predictions[[i]], col=cols, ann=F, legend=F, axes=F, box=F) 
  plot(borders, add=T, border="gray50", lwd=0.5) # Contour de la zone d'étude
  
  # Ajout du titre en deux parties ("Ecological suitability" + "BRT replicate X")
  mtext("Ecological suitability", side=3, line=0.3, cex=0.65, col="gray30") 
  mtext(paste0("(BRT replicate ",i,")"), side=3, line=-0.7, cex=0.65, col="gray30") 
  
  # Ajout de la barre de légende
  plot(predictions[[i]], col=cols, legend.only=T, add=T, legend.width=0.5,
       legend.shrink=0.3, smallplot=c(0.10,0.80,0.09,0.12), adj=3,
       axis.args=list(cex.axis=0.65, lwd=0, lwd.tick=0.2, col.tick="gray30",
                      tck=-0.9, col="gray30", col.axis="gray30", line=0,
                      mgp=c(0,0.1,0)), alpha=1, side=3, horizontal=T) 
}

#------------------------------------------------------------------------------
# --- 2. Visualisation du modèle consensuel (Moyenne et Écart-type) ---

# On transforme notre liste de 100 prédictions en un stack
predictions_stack <- stack(predictions)

# On calcule la moyenne et l'écart-type (standard deviation) pixel par pixel
mean_map <- calc(predictions_stack, mean)
sd_map <- calc(predictions_stack, sd)

# Paramètres graphiques : 1 ligne, 2 colonnes pour mettre les cartes côte à côte
par(mfrow=c(1,2), oma=c(0,0,2,0), mar=c(1,1,2,5))

# On garde la même palette de couleurs que précédemment
cols <- rev(colorRampPalette(brewer.pal(9,"RdYlBu"))(131)[11:121])

# --- CARTE 1 : LA MOYENNE (Risque global) ---
plot(mean_map, col=cols,zlim=c(0,1), axes=FALSE, box=FALSE, 
     main="Moyenne 
     (Risque moyen des 100 modèles)", cex.main=0.7)
plot(borders, add=TRUE, border="gray50", lwd=0.5) 

# --- CARTE 2 : L'ÉCART-TYPE (Incertitude) ---
# 1) Option alternative (retenue) : avec échelle forcée de 0 à 1
plot(sd_map, 
     col=cols, 
     zlim=c(0, 1),       # force l'échelle de 0 à 1
     axes=FALSE, 
     box=FALSE, 
     main="Écart-type 
     (Incertitude entre les modèles)", 
     cex.main=0.7)

plot(borders, add=TRUE, border="gray50", lwd=0.5)

#2) Option avec échelle sur écart-type max (non retenue),permet de visualiser facilement l'ecart type maximal
par(mfrow=c(1,1))
cols <- rev(colorRampPalette(brewer.pal(9,"Purples"))(131)[121:11])

plot(sd_map, col=cols, axes=FALSE, box=FALSE, 
    main="Écart-type (Incertitude entre les modèles)", cex.main=0.8)
plot(borders, add=TRUE, border="gray50", lwd=0.5)


#------------------------------------------------------------------------------
# VII. Performance prédictive des modèles
#------------------------------------------------------------------------------

#  1. Évaluation par l'AUC (Area Under the Curve) 
# L'AUC mesure la capacité globale du modèle à bien séparer les présences des absences.
# (1 = modèle parfait, 0.5 = modèle équivalent au hasard)

AUCs = read.csv("AUC_value_replicates.csv", head=T)
cat("Mean AUC = ",round(mean(AUCs[,1]),2),
    ", AUC range = [",round(min(AUCs),2),", ",round(max(AUCs),2),"]","\n",sep="")

# Résultat obtenu: Mean AUC = 0.82, AUC range = [0.76, 0.88]

#2. Évaluation par l'Indice de Sørensen calibré (SI_ppc) 
# Le SI_ppc est une alternative  à l'AUC, spécialement(plus robuste ici)

brt_models = readRDS("BRT_model_replicates.rds") # Chargement des 100 modèles entraînés
SI_ppcs = rep(NA, length(brt_models))            # Vecteur vide pour stocker les scores SI_ppc
thresholds = rep(NA, length(brt_models))         # Vecteur vide pour stocker le seuil optimal de chaque modèle

# Boucle d'évaluation sur chaque modèle
for (i in 1:length(brt_models)) {
  
  # Matrice temporaire pour tester tous les seuils possibles (de 0 à 1, par pas de 0.01)
  tmp = matrix(nrow=101, ncol=2) 
  colnames(tmp) = c("threshold", "SIppc")
  tmp[, "threshold"] = seq(0, 1, 0.01)
  
  # Récupération des données utilisées par ce modèle spécifique
  dataframe = brt_models[[i]]$gbm.call$dataframe
  responses = dataframe$response
  data = dataframe[, 4:dim(dataframe)[2]] 
  
  # Extraction des prédictions mathématiques générées par ce modèle
  n.trees = brt_models[[i]]$gbm.call$best.trees
  type = "response"
  single.tree = FALSE
  prediction = predict.gbm(brt_models[[i]], data, n.trees, type, single.tree)
  
  # Calcul de la prévalence (proportion de présences par rapport au total)
  P = sum(responses == 1) # Nombre de présences réelles
  A = sum(responses == 0) # Nombre de pseudo-absences
  prev = P / (P + A) 
  x = (P / A) * ((1 - prev) / prev) # Facteur de correction mathématique
  SI_ppc = 0 
  
  # Sous-boucle pour trouver le seuil de probabilité optimal(seuil qui maximise le score de Sørensen)
  for (threshold in seq(0, 1, 0.01)) {
    TP = length(which((responses == 1) & (prediction >= threshold)))    # Vrais Positifs 
    FN = length(which((responses == 1) & (prediction < threshold)))     # Faux Négatifs
    FP_pa = length(which((responses == 0) & (prediction >= threshold))) # Faux Positifs 
    
    # Formule mathématique du SI_ppc
    SI_ppc_tmp = (2 * TP) / ((2 * TP) + (x * FP_pa) + FN) 
    tmp[which(tmp[, "threshold"] == threshold), "SIppc"] = SI_ppc_tmp
    
    # Si le score calculé est le meilleur trouvé jusqu'ici, on le sauvegarde avec son seuil
    if (SI_ppc < SI_ppc_tmp) { 
      SI_ppc = SI_ppc_tmp
      optimised_threshold = threshold
    }
  }
  
  # On stocke le résultat final et optimal pour ce modèle
  SI_ppcs[i] = SI_ppc
  thresholds[i] = optimised_threshold 
}

# résultats du SI_ppc 

mean_SIppc = round(mean(SI_ppcs), 2)
mean_thres = round(mean(thresholds), 2) # Seuil de probabilité moyen 

min_SIppc = round(min(SI_ppcs), 2)
min_thres = round(min(thresholds), 2) 

max_SIppc = round(max(SI_ppcs), 2)
max_thres = round(max(thresholds), 2) 

cat("Mean SIppc = ", mean_SIppc, ", SIppc range = [", min_SIppc, ", ", max_SIppc, "]", "\n", sep="")

# Résultat obtenu: Mean SIppc = 0.95, SIppc range = [0.91, 0.98]

#------------------------------------------------------------------------------
# VIII. importnce relatives
#------------------------------------------------------------------------------

brt_models = readRDS("BRT_model_replicates.rds")

relative_importances = matrix(nrow=length(brt_models), ncol=length(envVariables))
colnames(relative_importances) = envVariableNames

# Extraction de l'importance relative pour chaque variable dans chaque modèle
for (i in 1:length(brt_models)) { 
  for (j in 1:length(envVariables)) {
    relative_importances[i, j] = summary(brt_models[[i]])[names(envVariables[[j]]), "rel.inf"]
  }
}

# Sauvegarde dans un fichier CSV puis rechargement
write.table(relative_importances, "Relative_importances.csv", row.names=F, quote=F, sep=",") 
relative_importances = read.csv("Relative_importances.csv", head=T)

# Création du tableau de résumé (Moyenne et Intervalle de Confiance à 95%)
RI_summary = matrix(nrow=length(envVariables), ncol=1)
rownames(RI_summary) = envVariableNames
colnames(RI_summary) = c("mean RI [95% CI]")

for (i in 1:dim(relative_importances)[2]) {
  mean_RI = round(mean(relative_importances[, i]), 1)
  ci95_RI = round(t.test(relative_importances[, i])$conf.int[1:2], 1)
  RI_summary[i, 1] = paste0(mean_RI, " [", ci95_RI[1], ", ", ci95_RI[2], "]")
}

# 4. Affichage du résultat final dans la console
RI_summary
#Affichage(pour BRT lancé de notre coté):
#                             mean RI [95% CI]   
# human_pop_density_log10       "2.9 [2.6, 3.1]"   
# croplands_all_categories      "1.4 [1.2, 1.6]"   
# managed_pasture_and_rangeland "3.5 [3.2, 3.8]"   
# precipitation_inFall          "1.1 [1, 1.2]"     
# precipitation_spring          "7.7 [7.3, 8]"     
# precipitation_summer          "5.5 [5, 6]"       
# precipitation_winter          "2 [1.8, 2.3]"     
# primary_forest_areas          "0.3 [0.3, 0.4]"   
# primary_non_forest_areas      "0.9 [0.7, 1]"     
# relative_humidity_inFall      "27.2 [25.1, 29.3]"
# relative_humidity_spring      "1.9 [1.6, 2.1]"   
# relative_humidity_summer      "6.6 [5.6, 7.6]"   
# relative_humidity_winter      "14.5 [12.7, 16.3]"
# secondary_forest_areas        "3.4 [3.1, 3.7]"   
# secondary_non_forest_areas    "1.5 [1.2, 1.7]"   
# temperature_inFall            "2 [1.6, 2.4]"     
# temperature_spring            "5.3 [4.5, 6.2]"   
# temperature_summer            "10.1 [8.6, 11.6]" 
# temperature_winter            "2.2 [1.9, 2.4]" 


#------------------------------------------------------------------------------
# IX. Courbes de réponses (Effet individuel de chaque variable environnementale)
#------------------------------------------------------------------------------

#1. Préparation

brt_models = readRDS("BRT_model_replicates.rds") # Chargement des modèles

# Extraction des points géographiques où le virus a été détecté (response == 1)
sp = SpatialPoints(dataframes[[1]][which(dataframes[[1]][, "response"] == 1), 2:3])

envVariableValues = matrix(nrow=3, ncol=length(envVariables)) # Création de la matrice
colnames(envVariableValues) = envVariableNames
row.names(envVariableValues) = c("median", "minV", "maxV")

for (i in 1:length(envVariables)) {
  # On masque la carte pour ne garder que les pixels où le virus est présent
  points = rasterize(sp, envVariables[[i]])
  rast = envVariables[[i]]
  rast[is.na(points)] = NA
  
  # On calcule le minimum, le maximum et la médiane pour chaque variable
  minV = min(rast[], na.rm=T)
  maxV = max(rast[], na.rm=T)
  envVariableValues[, i] = cbind(median(rast[], na.rm=T), minV, maxV)
}

# Titres des graphiques 

envVariableTitles = c(
  "Human population", 
  "Croplands", 
  "Pastures",
  "Precipitation (Fall)", 
  "Precipitation (Spring)", 
  "Precipitation (Summer)", 
  "Precipitation (Winter)",
  "Primary forests", 
  "Primary non-forests",
  "Relative humidity (Fall)", 
  "Relative humidity (Spring)", 
  "Relative humidity (Summer)", 
  "Relative humidity (Winter)",
  "Secondary forests", 
  "Secondary non-forests",
  "Temperature (Fall)", 
  "Temperature (Spring)", 
  "Temperature (Summer)", 
  "Temperature (Winter)"
)

# 2.Création des 19 graphiques(pour chaque variable environnementale)

par(mfrow=c(4,5), oma=c(0,0,0,0), mar=c(2,1.2,0.5,0.5), lwd=0.2, col="gray30")

for (i in 1:19)  { # Boucle pour chaque variable environnementale
  
  # a. Création de 100 valeurs allant du Min au Max de la variable)
  valuesInterval = (envVariableValues["maxV", i] - envVariableValues["minV", i]) / 100 
  
  dataframe = data.frame(matrix(nrow=length(seq(envVariableValues["minV", i], 
                                                envVariableValues["maxV", i], valuesInterval)),
                                ncol=length(envVariables))) 
  colnames(dataframe) = envVariableNames
  
  # b. On fait varier la variable étudiée (i), et on bloque toutes les autres (j) sur leur médiane
  for (j in 1:length(envVariables)) {
    interval = (envVariableValues["maxV", j] - envVariableValues["minV", j]) / 100
    if (i == j) {
      dataframe[, envVariableNames[j]] = seq(envVariableValues["minV", j], 
                                             envVariableValues["maxV", j], interval)
    } else {
      dataframe[, envVariableNames[j]] = rep(envVariableValues["median", j], dim(dataframe)[1])
    }
  }
  
  # c. On demande à nos 100 modèles de prédire le risque pour ce scénario
  predictions = list() 
  
  for (j in 1:length(brt_models)) { 
    n.trees = brt_models[[j]]$gbm.call$best.trees
    type = "response"
    single.tree = FALSE
    prediction = predict.gbm(brt_models[[j]], newdata=dataframe, n.trees, type, single.tree)
    
    
    if (j == 1) { 
      minX = min(dataframe[, envVariableNames[i]])
      maxX = max(dataframe[, envVariableNames[i]])
    } else {
      if (minX > min(dataframe[, envVariableNames[i]])) minX = min(dataframe[, envVariableNames[i]])
      if (maxX < max(dataframe[, envVariableNames[i]])) maxX = max(dataframe[, envVariableNames[i]]) 
    }
    predictions[[j]] = prediction
  }
  
  # d. Dessin des courbes sur le graphique
  col = rgb(222, 67, 39, 255, maxColorValue=255) # rgb rouge
  
  for (j in 1:length(brt_models)) {
    if (j == 1) { # La 1ère courbe est crée avec le reste du graphique
      plot(dataframe[, envVariableNames[i]], predictions[[j]], col=col, ann=F,
           axes=F, lwd=0.2, type="l", xlim=c(minX, maxX), ylim=c(0, 1))
    } else { # Les 99 autres viennent se superposer 
      lines(dataframe[, envVariableNames[i]], predictions[[j]], col=col, lwd=0.2)
    }
  }
  axis(side=1, lwd.tick=0.2, cex.axis=0.7, lwd=0, tck=-0.040, col.axis="gray30", mgp=c(0,0.15,0))
  axis(side=2, lwd.tick=0.2, cex.axis=0.7, lwd=0, tck=-0.040, col.axis="gray30", mgp=c(0,0.4,0))
  title(xlab=envVariableTitles[i], cex.lab=0.9, mgp=c(1.0,0,0), col.lab="gray30")
  box(lwd=0.2, col="gray30")
}


#-----------------------------------------------------------------------------------------------
#X.Visualisation des courbes et des shapefiles selon leurs importances
#-----------------------------------------------------------------------------------------------
#Cette section n'ajoute aucun nouveau graphique mais permet de visualiser ceux deja créés,
#en fonction de l'importance des variables (on a choisi une seuil de 5% qui est modifiable)


#visualisation des courbes de réponses des  variables les plus improtantes(importances>5% dans le code), il est possible de modifier la limite 
#choisie en modifiant le 5 par un autre chiffre aux choix dans les 2 lignes suivantes:

#nb_graphiques= sum(toutes_les_moyennes > 5)
#if (moyenne_actuelle > 5) {

#-------------------------------------------------------------------------------
#1.Fenêtre graphique dynamique

#nous allors d'abord configurer la fenêtre graphique dynamique qui s'adapte
#au nombre de variables prises en compte (dépendant de la limite d'importance choisie)

#a. On calcule toutes les moyennes d'un coup
toutes_les_moyennes =colMeans(relative_importances)

#b. On compte combien de variables ont une moyenne supérieure à 5
nb_graphiques= sum(toutes_les_moyennes > 5)

#c. On calcule le nombre de lignes et de colonnes pour faire une belle grille
nb_colonnes = ceiling(sqrt(nb_graphiques))
nb_lignes= ceiling(nb_graphiques / nb_colonnes)

#d. On configure la fenêtre graphique (mfrow dynamique)

par(mfrow = c(nb_lignes, nb_colonnes))

#-------------------------------------------------------------------------------
#2.Boucle d'affichage des courbes de réponses
for (i in 1:19)  { # Boucle sur tes 19 variables
  
  # 1. On calcule la moyenne numérique directement à partir de ton tableau pour la variable 'i'
  moyenne_actuelle = mean(relative_importances[, i])
  
  # 2. On vérifie si cette moyenne est supérieure à 5
  if (moyenne_actuelle > 5) { 
    
    valuesInterval = (envVariableValues["maxV", i] - envVariableValues["minV", i]) / 100 
    
    dataframe = data.frame(matrix(nrow=length(seq(envVariableValues["minV", i], 
                                                  envVariableValues["maxV", i], valuesInterval)),
                                  ncol=length(envVariables))) 
    
    colnames(dataframe) = envVariableNames
    
    for (j in 1:length(envVariables)) {
      interval = (envVariableValues["maxV", j] - envVariableValues["minV", j]) / 100
      if (i == j) {
        dataframe[, envVariableNames[j]] = seq(envVariableValues["minV", j], 
                                               envVariableValues["maxV", j], interval)
      } else {
        dataframe[, envVariableNames[j]] = rep(envVariableValues["median", j], dim(dataframe)[1])
      }
    }
    
    predictions = list() 
    
    for (j in 1:length(brt_models)) { 
      n.trees = brt_models[[j]]$gbm.call$best.trees
      type = "response"
      single.tree = FALSE
      prediction = predict.gbm(brt_models[[j]], newdata=dataframe, n.trees, type, single.tree)
      
      if (j == 1) { 
        minX = min(dataframe[, envVariableNames[i]])
        maxX = max(dataframe[, envVariableNames[i]])
      } else {
        if (minX > min(dataframe[, envVariableNames[i]])) {
          minX = min(dataframe[, envVariableNames[i]])
        }
        if (maxX < max(dataframe[, envVariableNames[i]])) {
          maxX = max(dataframe[, envVariableNames[i]]) 
        }
      }
      predictions[[j]] = prediction
    }
    
    col = rgb(222, 67, 39, 255, maxColorValue=255) # red
    
    for (j in 1:length(brt_models)) {
      if (j == 1) {
        plot(dataframe[, envVariableNames[i]], predictions[[j]], col=col, ann=F,
             axes=F, lwd=0.2, type="l", xlim=c(minX, maxX), ylim=c(0, 1))
      } else {
        lines(dataframe[, envVariableNames[i]], predictions[[j]], col=col, lwd=0.2)
      }
    }
    
    axis(side=1, lwd.tick=0.2, cex.axis=0.7, lwd=0, tck=-0.040, col.axis="gray30", mgp=c(0,0.15,0))
    axis(side=2, lwd.tick=0.2, cex.axis=0.7, lwd=0, tck=-0.040, col.axis="gray30", mgp=c(0,0.4,0))
    title(xlab=envVariableTitles[i], cex.lab=0.9, mgp=c(1.0,0,0), col.lab="gray30")
    box(lwd=0.2, col="gray30")
    
  } 
} 
#----------------------------------------------------------------------------------
#code des figures
#-------------------------------------------------------------------
#FIGURE 1
# 1. Configuration de la fenêtre (1 ligne, 3 colonnes)
par(mfrow = c(1, 3), mar = c(1, 1, 3, 5))
# On fixe une taille commune pour tous les titres
taille_titre <- 1.2
# --- PANNEAU 1 : OCCURRENCES ---
#  On trace le raster 'mean_map' de manière INVISIBLE (col=NA, legend=FALSE).
#on force R à utiliser exactement le même cadre et les mêmes dimensions que les cartes 2 et 3 !
plot(mean_map, col=NA, legend=FALSE, axes=FALSE, box=FALSE,
     main="Données d'occurrences\n(Présences du virus)",
     cex.main=taille_titre)
# On ajoute les frontières par-dessus cette toile de fond invisible
plot(borders, add=TRUE, border="gray50", lwd=0.5)
# couleurs transparentes
rouge_transparent <- adjustcolor("red", alpha.f = 0.4)
noir_transparent <- adjustcolor("black", alpha.f = 0.2)
# Trace les points
points(records_arbovirus[, c("longitude","latitude")], pch=16, cex=0.7, col=rouge_transparent)
points(records_arbovirus[, c("longitude","latitude")], pch=1, cex=0.7, lwd=0.4, col=noir_transparent)
# --- PANNEAU 2 : CARTE MOYENNE (Risque global) ---
cols = rev(colorRampPalette(brewer.pal(9,"RdYlBu"))(131)[11:121])
plot(mean_map, col=cols, zlim=c(0,1), axes=FALSE, box=FALSE,
     main="Risque moyen\n(Moyenne des 100 modèles)",
     cex.main=taille_titre)
plot(borders, add=TRUE, border="gray50", lwd=0.5)
# --- PANNEAU 3 : L'ÉCART-TYPE (Incertitude) ---
plot(sd_map, col=cols, zlim=c(0, 1), axes=FALSE, box=FALSE,
     main="Écart-type\n(Incertitude entre les modèles)",
     cex.main=taille_titre)
plot(borders, add=TRUE, border="gray50", lwd=0.5)
#-------------------------------------------------------------------------------
#code pour la FIGURE 2 du RAPPORT
#-------------------------------------------------------------------------------
#on met les courbes de réponse (RI>5% avec les variables environnementales correspondantes)
par(mfrow = c(2,7),mar=c(3, 1, 2, 1));
#2.Boucle d'affichage des courbes de réponses
for (i in 1:19)  { # Boucle sur tes 19 variables
  # 1. On calcule la moyenne numérique directement à partir de ton tableau pour la variable 'i'
  moyenne_actuelle = mean(relative_importances[, i])
  # 2. On vérifie si cette moyenne est supérieure à 5
  if (moyenne_actuelle > 5) {
    valuesInterval = (envVariableValues["maxV", i] - envVariableValues["minV", i]) / 100
    dataframe = data.frame(matrix(nrow=length(seq(envVariableValues["minV", i],
                                                  envVariableValues["maxV", i], valuesInterval)),
                                  ncol=length(envVariables)))
    colnames(dataframe) = envVariableNames
    for (j in 1:length(envVariables)) {
      interval = (envVariableValues["maxV", j] - envVariableValues["minV", j]) / 100
      if (i == j) {
        dataframe[, envVariableNames[j]] = seq(envVariableValues["minV", j],
                                               envVariableValues["maxV", j], interval)
      } else {
        dataframe[, envVariableNames[j]] = rep(envVariableValues["median", j], dim(dataframe)[1])
      }
    }
    predictions = list()
    for (j in 1:length(brt_models)) {
      n.trees = brt_models[[j]]$gbm.call$best.trees
      type = "response"
      single.tree = FALSE
      prediction = predict.gbm(brt_models[[j]], newdata=dataframe, n.trees, type, single.tree)
      if (j == 1) {
        minX = min(dataframe[, envVariableNames[i]])
        maxX = max(dataframe[, envVariableNames[i]])
      } else {
        if (minX > min(dataframe[, envVariableNames[i]])) {
          minX = min(dataframe[, envVariableNames[i]])
        }
        if (maxX < max(dataframe[, envVariableNames[i]])) {
          maxX = max(dataframe[, envVariableNames[i]])
        }
      }
      predictions[[j]] = prediction
    }
    col = rgb(222, 67, 39, 255, maxColorValue=255) # red
    for (j in 1:length(brt_models)) {
      if (j == 1) {
        plot(dataframe[, envVariableNames[i]], predictions[[j]], col=col, ann=F,
             axes=F, lwd=0.2, type="l", xlim=c(minX, maxX), ylim=c(0, 1))
      } else {
        lines(dataframe[, envVariableNames[i]], predictions[[j]], col=col, lwd=0.2)
      }
    }
    axis(side=1, lwd.tick=0.2, cex.axis=0.7, lwd=0, tck=-0.040, col.axis="gray30", mgp=c(0,0.15,0))
    axis(side=2, lwd.tick=0.2, cex.axis=0.7, lwd=0, tck=-0.040, col.axis="gray30", mgp=c(0,0.4,0))
    #title(xlab=envVariableTitles[i], cex.lab=0.9, mgp=c(1.0,0,0), col.lab="gray30")
    box(lwd=0.2, col="gray30")
  }
}
Plimites=c(0,10)#bornes pour une légende uniforme entre les saisons
#Printemps
cols = colorRampPalette(brewer.pal(9,"YlGnBu"))(100)
plot(envVariables[[5]], col=cols,zlim=Plimites, ann=F, legend=F, axes=F, box=F)
plot(borders, add=T, border="gray50", lwd=0.5)
mtext("Précipitations au", side=3, line=2, cex=0.65, col="gray30")
mtext(expression(" printemps (kg/m"^2*"/day)"), side=3, line=1, cex=0.65, col="gray30")
plot(envVariables[[5]], col=cols,zlim=Plimites, legend.only=T, add=T, legend.width=0.5, legend.shrink=0.3,
     smallplot=c(0.10,0.80,0.09,0.12), adj=3, axis.args=list(cex.axis=0.7, lwd=0, lwd.tick=0.2,
                                                             col.tick="gray30", tck=-0.9, col="gray30", col.axis="gray30", line=0, mgp=c(0,0.1,0)),
     alpha=1, side=3, horizontal=T)
#été
cols = colorRampPalette(brewer.pal(9,"YlGnBu"))(100)
plot(envVariables[[6]], col=cols,zlim=Plimites, ann=F, legend=F, axes=F, box=F)
plot(borders, add=T, border="gray50", lwd=0.5)
mtext("Précipitations en été", side=3, line=2, cex=0.65, col="gray30")
mtext(expression("(kg/m"^2*"/day)"), side=3, line=1, cex=0.65, col="gray30")
plot(envVariables[[6]], col=cols,zlim=Plimites, legend.only=T, add=T, legend.width=0.5, legend.shrink=0.3,
     smallplot=c(0.10,0.80,0.09,0.12), adj=3, axis.args=list(cex.axis=0.7, lwd=0, lwd.tick=0.2,
                                                             col.tick="gray30", tck=-0.9, col="gray30", col.axis="gray30", line=0, mgp=c(0,0.1,0)),
     alpha=1, side=3, horizontal=T)
RHlimites=c(30,100)
#Automne
cols = colorRampPalette(c("white","blue"),bias=1)(100)
plot(envVariables[[10]], col=cols,zlim=RHlimites, ann=F, legend=F, axes=F, box=F)
plot(borders, add=T, border="gray50", lwd=0.5)
mtext("Humidité Relative ", side=3, line=2, cex=0.65, col="gray30")
mtext("en Automne", side=3, line=1, cex=0.65, col="gray30")
plot(envVariables[[10]], col=cols,zlim=RHlimites, legend.only=T, add=T, legend.width=0.5, legend.shrink=0.3,
     smallplot=c(0.10,0.80,0.09,0.12), adj=3, axis.args=list(cex.axis=0.7, lwd=0, lwd.tick=0.2,
                                                             col.tick="gray30", tck=-0.9, col="gray30", col.axis="gray30", line=0, mgp=c(0,0.1,0)),
     alpha=1, side=3, horizontal=T)
#été
cols = colorRampPalette(c("white","blue"),bias=1)(100)
plot(envVariables[[12]], col=cols,zlim=RHlimites, ann=F, legend=F, axes=F, box=F)
plot(borders, add=T, border="gray50", lwd=0.5)
mtext("Humidité Relative ", side=3, line=2, cex=0.65, col="gray30")
mtext("en été", side=3, line=1, cex=0.65, col="gray30")
plot(envVariables[[12]], col=cols,zlim=RHlimites, legend.only=T, add=T, legend.width=0.5, legend.shrink=0.3,
     smallplot=c(0.10,0.80,0.09,0.12), adj=3, axis.args=list(cex.axis=0.7, lwd=0, lwd.tick=0.2,
                                                             col.tick="gray30", tck=-0.9, col="gray30", col.axis="gray30", line=0, mgp=c(0,0.1,0)),
     alpha=1, side=3, horizontal=T)
#Hiver
cols = colorRampPalette(c("white","blue"),bias=1)(100)
plot(envVariables[[13]], col=cols,zlim=RHlimites, ann=F, legend=F, axes=F, box=F)
plot(borders, add=T, border="gray50", lwd=0.5)
mtext(" Humidité Relative ", side=3, line=2, cex=0.65, col="gray30")
mtext("en Hiver", side=3, line=1, cex=0.65, col="gray30")
plot(envVariables[[13]], col=cols,zlim=RHlimites, legend.only=T, add=T, legend.width=0.5, legend.shrink=0.3,
     smallplot=c(0.10,0.80,0.09,0.12), adj=3, axis.args=list(cex.axis=0.7, lwd=0, lwd.tick=0.2,
                                                             col.tick="gray30", tck=-0.9, col="gray30", col.axis="gray30", line=0, mgp=c(0,0.1,0)),
     alpha=1, side=3, horizontal=T)
Tlimites=c(-10,40)#bornes pour une légende uniforme entre les saisons
cols = colorRampPalette(brewer.pal(9,"YlOrRd"))(150)[1:100];
#printemps
plot(envVariables[[17]], col=cols,zlim=Tlimites, ann=F, legend=F, axes=F, box=F)


plot(borders, add=T, border="gray50", lwd=0.5)
mtext("Temperature de l'air", side=3, line=2, cex=0.65, col="gray30")
mtext("au Printemps (°C)", side=3, line=1, cex=0.65, col="gray30")
plot(envVariables[[17]], col=cols,zlim=Tlimites, legend.only=T, add=T, legend.width=0.5, legend.shrink=0.3,
     smallplot=c(0.10,0.80,0.09,0.12), adj=3, axis.args=list(cex.axis=0.7, lwd=0, lwd.tick=0.2,
                                                             col.tick="gray30", tck=-0.9, col="gray30", col.axis="gray30", line=0, mgp=c(0,0.1,0)),
     alpha=1, side=3, horizontal=T)
#été
plot(envVariables[[18]], col=cols,zlim=Tlimites, ann=F, legend=F, axes=F, box=F)
plot(borders, add=T, border="gray50", lwd=0.5)
mtext("Temperature de l'air", side=3, line=2, cex=0.65, col="gray30")
mtext("en été (°C)", side=3, line=1, cex=0.65, col="gray30")
plot(envVariables[[18]], col=cols,zlim=Tlimites, legend.only=T, add=T, legend.width=0.5, legend.shrink=0.3,
     smallplot=c(0.10,0.80,0.09,0.12), adj=3, axis.args=list(cex.axis=0.7, lwd=0, lwd.tick=0.2,
                                                             col.tick="gray30", tck=-0.9, col="gray30", col.axis="gray30", line=0, mgp=c(0,0.1,0)),
     alpha=1, side=3, horizontal=T)

