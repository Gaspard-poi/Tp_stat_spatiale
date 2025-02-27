url <- "https://minio.lab.sspcloud.fr/julienjamme/geographies/commune_francemetro_2021.gpkg"
file_name <- "commune_francemetro_2021.gpkg"
file_path <- "./fonds/"
library(dplyr)
library(units)
if(!dir.exists("fonds")) dir.create("fonds/", recursive = TRUE)

download.file(url = url, destfile = paste0(file_path, file_name))

install.packages("sf")
library(sf)
#1
Commune_fr_metro <- st_read("fonds/commune_francemetro_2021.gpkg")

#2
summary(Commune_fr_metro)

#3
View(Commune_fr_metro[1:10,])

#4
système_de_projection <- st_crs(Commune_fr_metro)

#5
Communes_PACA <- Commune_fr_metro %>%
  filter(reg == 93)%>%
  select(code, libelle, epc, dep, surf)
Communes_bretagne <- Commune_fr_metro %>%
  filter(reg == 53)%>%
  select(code, libelle, epc, dep, surf)
# Il y a toujours la variable geom

#6

str(Communes_bretagne)

#7
plot(Communes_PACA['dep'], lwd=0.5)

#8
plot(st_geometry(Communes_PACA, lwd = 0.5))

#9
Communes_PACA2 <- Communes_PACA %>%
  mutate(surf2 = st_area(Communes_PACA$geom))

#10
Communes_PACA2 <- Communes_PACA2 %>%
  mutate(surf2 = set_units(surf2, km*km))

#11
#oui
#12
dep_paca <- Communes_PACA2 %>%
  group_by(dep)%>%
  summarise(sum(surf))
plot(dep_paca['dep'])

#13

dep_paca2 <- Communes_PACA2 %>%
  group_by(dep) %>% 
  summarise(geom = st_union(geom))  

plot(dep_paca2)

#14

centroid_dep_paca <- st_centroid(dep_paca2)
plot(centroid_dep_paca, add = T)
plot(dep_paca2)

plot(st_geometry(centroid_dep_paca), add = T)
plot(st_geometry(dep_paca2))

centroid_dep_paca <- centroid_dep_paca %>%
  mutate(dep_lib = c("Alpes-De-Haute-Provence", "Hautes-Alpes", 'Alpes-maritimes', "Bouches-Du-Rhône", "Var", "Vaucluse"))

centroid_coords <- st_coordinates(centroid_dep_paca)
centroid_coords <- centroid_coords %>%
  bind_cols(
    centroid_dep_paca %>%
      select(dep, dep_lib)%>%
      st_drop_geometry()
  )

plot(st_geometry(dep_paca2))
plot(st_geometry(centroid_dep_paca), add = T)
text(x = centroid_coords$X, y = centroid_coords$Y, labels = centroid_coords$dep_lib, pos = 3, cex = 0.8,
     col = "orangered")

commune_centroid_paca <- st_intersects(centroid_dep_paca, Communes_PACA2)
typeof(commune_centroid_paca)

#16
st_intersection(centroid_dep_paca, Communes_PACA2)

Q16_within <- st_within(centroid_dep_paca, Communes_PACA2)
