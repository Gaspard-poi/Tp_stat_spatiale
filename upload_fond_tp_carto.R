
library(purrr)

fichiers <- data.frame(
  fichier = c(
  "Pop_legales_2019.xlsx",
  "Taux_pauvrete_2018.xlsx",
  "pop_region_2019.xlsx",
  "bpe20_sport_loisir_xy.csv",
  "base-cc-evol-struct-pop-2018_echantillon.csv",
  "commune_francemetro_2021.gpkg",
  "dep_francemetro_2021.gpkg",
  "merf_2021.gpkg",
  "reg_francemetro_2021.gpkg"
  ),
  type = c(rep("data",5),rep("fonds",4)),
  remote = c(rep("Data",5),rep("Fonds",4))
)

walk(
  unique(fichiers$type),
  \(t) if(!dir.exists(t)) dir.create(paste0(t, "/"), recursive = TRUE)
)

upload_fichier <- function(fichier, type, remote){
  
  base_url <- "https://minio.lab.sspcloud.fr/daudenaert"
  
  tryCatch(
    {
      download.file(url = paste0(base_url, "/", remote, "/", fichier),
                    destfile = paste0(type, "/", fichier))
    },
    error = function(e){print(paste0("pb de téléchargement pour le fichier ", fichier))}
  )
}

pwalk(fichiers, upload_fichier)

library(dplyr)
library(sf)
install.packages("mapsf")
library(mapsf)
library(classInt)
library(leaflet)


install.packages("readxl")
pop_com_2019 <- openxlsx::read.xlsx("data/Pop_legales_2019.xlsx")
communes_fm <- st_read("fonds/commune_francemetro_2021.gpkg")

pop_com_2019 <- pop_com_2019 %>%
  mutate(COM=if_else(substr(COM,1,3)=="751", "75056", COM))%>%
  group_by(code=COM) %>%
  summarise(pop=sum(PMUN19))

communes_fm <- communes_fm %>%
  left_join(pop_com_2019, by="code")%>%
  mutate(densite=pop/surf)

plot(communes_fm %>% st_geometry(), lwd = 0.1)
summary(communes_fm$densite)
hist(communes_fm$densite)

plot(communes_fm['densite'], border = F)
plot(communes_fm['densite'], breaks = 'quantile', main = "quantile", border = F)
plot(communes_fm['densite'], breaks = 'kmeans', main = "quantile", border = F)
plot(communes_fm['densite'], breaks = 'jenks', main = "quantile", border = F)
plot(communes_fm['densite_log'], breaks = "quantile", main = "Echelle logarithmique", border = FALSE)
communes_fm$densite_log <- log(communes_fm$densite + 1)

denspop_quant <- classIntervals(
  communes_fm$densite,
  style = 'quantile',
  n=5)
str(denspop_quant)
denspop_quant$brks

quantile(communes_fm$densite, probs = seq(0,1,0.1))
denspop_man_brks5 <- c(0,40,250,2000,15000, 27000)
popcomfm_sf <- communes_fm %>%
  mutate(
    densite_c = cut(
      densite,
      breaks = denspop_man_brks5,
      labels = paste0(denspop_man_brks5[1:5], "-", denspop_man_brks5[2:6]),
      include.lowest = T,
      right = F,
      ordered_result = T
    )
  )

plot(popcomfm_sf['densite_c'], border = F, pal = pa12)
pa12 <- c(
  RColorBrewer::brewer.pal(
    n=5,
    name = "Greens"
    )[4:3],
    RColorBrewer::brewer.pal(
    n=5,
    name="YlOrRd"
    )[c(2,4:5)]
  )
plot(popcomfm_sf['densite_c'], border = F, pal = pa12)


##################################
##### EXO 2 ######################
##################################

dep_francemetro_2021 <- st_read("fonds/dep_francemetro_2021.gpkg")
tx_pauvrete <- read_xlsx("data/Taux_pauvrete_2018.xlsx")
mer <- st_read("fonds/merf_2021.gpkg")

dep_francemetro_2018_pauv <- dep_francemetro_2021 %>%
  left_join(tx_pauvrete %>% select(-Dept), by = c("code"="Code"))

mf_map(x = dep_francemetro_2018_pauv,
       var = "Tx_pauvrete",
       type = "choro",
       nbreaks = 4,
       breaks = "jenks")

mf_map(x = dep_francemetro_2018_pauv,
       var = "Tx_pauvrete",
       type = "choro",
       breaks = c(0,13,17,25, max(dep_francemetro_2018_pauv$Tx_pauvrete)),
       pal = couleur,
       leg_pos = NA)

couleur <- rev(mf_get_pal(4, "Mint"))

mf_inset_on(x = dep_francemetro_2018_pauv, pos = "topright", cex = .2)

mf_init(dep_francemetro_2018_pauv %>%
          filter(code %in% c("75", "92", "93", "94")))

mf_map(x = dep_francemetro_2018_pauv %>%
         filter(code %in% c("75", "92", "93", "94")),
       var = "Tx_pauvrete",
       type = "choro",
       breaks = c(0,13,17,25, max(dep_francemetro_2018_pauv$Tx_pauvrete)),
       pal = couleur,
       leg_pos = NA,
       add = T)

coords <- st_coordinates(st_centroid(labels_data$geom))

mf_label(dep_francemetro_2018_pauv %>%
           filter(code %in% c("75", "92", "93", "94")),
         var = "code",
         col = "black")

mf_inset_off()

mf_legend(
  type = "choro",
  title = "Taux de pauvreté",
  val = c("", "Moins de 13", "De 13 à moins de 17", "De 17 à moins de 25", "25 ou plus"),
  pal = couleur,
  pos = "left"
)

mf_map(mer, add = T)

mf_layout(title = "Taux de pauvreté par département en 2018",
          credits = "Source : INSEE")


