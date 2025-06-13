#setwd("C:\\Users\\dagma\\OneDrive\\Pulpit\\PROJEKT_WIZUALIZACJA\\Wizualizacja")

packages <- c('RColorBrewer','viridis','sf', 'dplyr', 'tmap',
              'tidyverse','eurostat', 'patchwork', 'magick')

for(i in packages){
  if(!i %in% installed.packages()){
    install.packages(i)
  }
  library(i, character.only = T)
}
rm(i, packages)


CNTR_eu<-unlist(eu_countries %>%
                    mutate(geo=code) %>%
                    select(geo))

NUTS2_data <- st_read(dsn = "./NUTS Maps") %>%
  filter(LEVL_CODE == 2) %>%
  #filter(CNTR_CODE %in% c(eu_countries$code, "NO", "CH")) %>%
  filter(!CNTR_CODE == "RO") %>%
  filter(!str_detect(NUTS_ID, "FRY[0-9]+")) %>%
  filter(!str_detect(NUTS_ID, "ES7[0-9]+")) %>%
  filter(!str_detect(NUTS_ID, "PT2[0-9]+")) %>%
  filter(!str_detect(NUTS_ID, "PT3[0-9]+")) %>%
  filter(!NUTS_ID == "NO0B") %>%
  select(NUTS_ID, CNTR_CODE, NUTS_NAME, geometry)

#Sprawdzenie
tm_shape(NUTS2_data) +
  tm_polygons()

#Przygotowanie mapy dla krajów:
CNTR_data <- NUTS2_data %>%
  group_by(CNTR_CODE) %>%
  summarise(n = n()) %>%
  st_cast("MULTIPOLYGON") %>%
  mutate(NUTS_ID = CNTR_CODE) %>%
  select(NUTS_ID, geometry)

#Sprawdzenie
tm_shape(CNTR_data) +
  tm_polygons()
rm(NUTS2_data)

YEARS<-2018:2024
### Pobranie danych z eurostatu
#Pobieram dane: NEET
#jaki jest wskaznik neet w krajach ue
NEET<-get_eurostat(id="edat_lfse_23", time_format = "num", 
                   filters = list(
                     sex="T",
                     time=YEARS,
                     geo=CNTR_eu,
                     citizen = c("TOTAL"), 
                     age = c("Y15-29"),
                     freq = "A")) %>%
  select(geo, values, time)

NEET_data<-CNTR_data %>%
  left_join(NEET, by=join_by( "NUTS_ID" == "geo" ))

#Usunięcie braków danych
NEET_data<-NEET_data[!is.na(NEET_data$values),]


setwd("C:\\Users\\dagma\\OneDrive\\Pulpit\\PROJEKT_WIZUALIZACJA\\Wizualizacja\\MAPY_WIZ")

# for (i in 2018:2024){
# png(paste0("Mapa_",i, ".png"))
#   
# map <- tm_shape(NEET_data[NEET_data$time==i,]) +
#   
#   tm_polygons(title = "",
#               fill= "values",
#               palette = "viridis",
#               fill.legend = tm_legend(
#                 design= "standard",
#                 title.color = "black",
#                 bg.color = "#b2e2e9"),
#               style = "pretty",
#               fill.scale = tm_scale_intervals(breaks = c(0,5,10,15,20,25) )
#               ) +
#   
#   tm_layout(legend.position = c("left", "top"), 
#             frame = FALSE)+
#   tm_title(text = "NEET rate (%)")
# 
# print(map)
# dev.off() 
# }



range(NEET_data$values, na.rm = TRUE)
min_val <- min(NEET_data$values, na.rm = TRUE)
max_val <- max(NEET_data$values, na.rm = TRUE)



for (i in YEARS){
  png(filename = paste0("Mapa_",i, ".png"),
      #res = 300,
      width = 1000,
      height = 1000, 
      pointsize = 20
      )
  
  map <- ggplot(NEET_data[NEET_data$time==i,]) +
    labs(title = "Współczynnik NEET (%)", 
         subtitle = paste0("Osoby w wieku 15-29 lat\n", "Rok: ", i),
         caption = "Dane: edat_lfse_23, baza danych Eurostat")+
    
    
    
    geom_sf(aes(fill = values))+
    theme(plot.caption = element_text(hjust = 0, vjust = 0.5) )+
    theme_void() +
    
    scale_fill_viridis(
      option="B",
      breaks = c(0, 5, 10, 15, 20, 25),
      limits = c(min_val, max_val),
      name = "NEET rate (%)",
      guide = guide_colorbar(
        title = labs(title = "Współczynnik NEET"),
      #   keyheight = unit(10, units = "mm"),
      #   keywidth = unit(20, units = "mm"),
        label.position = "left",
        title.position = "top",
      #   theme = theme(legend.title =element_text(size = 15, colour = "red")),
      #   nrow = 10
        )
      )+
    
    theme(plot.caption = element_text(hjust = 0.1, size = 20),
          
          plot.title = element_text(hjust = 0.1, size = 32, vjust = -10),
          plot.subtitle = element_text(hjust = 0.1, size = 27, vjust = -15), 
          legend.justification = c("left", "top"), 
          legend.box.just = "left", 
          legend.position = c(.8, .8),
          legend.margin = margin(6, 8, 6, 6),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 18),
          legend.key.height = unit(15, "mm")
          )
  
  print(map)
  dev.off() 
}
rm(map, NEET, NEET_data, CNTR_data, CNTR_eu, i, max_val, min_val, YEARS)

# tworzenie animacji
png_files <- list.files(pattern = "Mapa_\\d+\\.png")
img_list <- lapply(png_files, image_read)
img_joined <- image_join(img_list)
#GIF
img_animated <- image_animate(img_joined, fps = 1)
image_write(img_animated, "Animacja.gif")

rm(img_animated, img_joined, img_list, png_files)

