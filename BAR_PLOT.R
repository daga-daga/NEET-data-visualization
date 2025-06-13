# Młodzi ludzie na rynku pracy w UE
#### Instalacja paczek
p<-c("eurostat", "ggplot2", "tidyr", "stringr", "knitr", "dplyr",
     "skimr", "ggfortify", "lmtest")
for (i in p){
  if(!require(i, character.only = TRUE)){install.packages(i)}
  library(i, character.only = TRUE)
}
rm(p, i)

#Przygotowanie zmiennych pod instalację danych 
CNTR_eu<-c(unlist(eu_countries %>%
                  mutate(geo=code) %>%
                  select(geo)), c("EU27_2020"))
CNTR_df<-(eu_countries %>%
                    mutate(geo=code) %>%
                    select(geo, name))
CNTR_df<-rbind(CNTR_df, c("EU27_2020", "European Union - 27 countries (from 2020)"))

AGE<-c("Y15-19", "Y20-24", "Y25-29", "Y30-34")
YEARS<-2018:2024

### Pobranie danych z eurostatu rok 2020
#Pobieram dane: NEET
#jak zmieniał się neet na przestrzeni lat w kracjahc ue
NEET<-get_eurostat(id="edat_lfse_23", time_format = "num", 
                   filters = list(
                     geo = CNTR_eu,
                     age=AGE,
                     sex="T",
                     time=YEARS,
                     citizen = c("TOTAL"), 
                     age = c("Y15-29"),
                     freq = "A")) %>%
  select(geo, values, time, sex, age)

NEET<- CNTR_df %>%
  left_join(x = NEET, by = c("geo" = "geo"), keep = FALSE )
# sum(is.na(NEET))
# colSums(is.na(NEET))

# NEET<-NEET %>%
#   pivot_wider(names_from = time, values_from = values)

COLORS<-c("#fdc086", "#7fc97f", "#8dd3c7", "#beaed4")
COLORS1<-c("#b2e2e2","#66c2a4", "#2ca25f", "#006d2c")
COLORS2<-c("#fdcc8a","#fc8d59","#e34a33", "#b30000")


AGE<-c("Y15-19", "Y20-24", "Y25-29", "Y30-34")

NEET$age <- factor(NEET$age,
                   levels = c("Y15-19", "Y20-24", "Y25-29", "Y30-34"),
                   labels = c("15–19", "20–24", "25–29", "30–34"))



#WYKRES ZE SŁUPKAMI I KROPKAMI

# png(filename = paste0("Mapa_", ".png"),
#     #res = 300,
#     width = 1000,
#     height = 1000, 
#     pointsize = 20)
# 
# 
# #####################UŻYWANE##################################
# 
# setwd("C:\\Users\\dagma\\OneDrive\\Pulpit\\PROJEKT_WIZUALIZACJA\\Wizualizacja\\WYKRES")
# kolory <- viridis(4, option = "B", begin = 0.1, end = 0.85)

ggplot() +
  labs(
    title = "Współczynniki NEET* wśród grup wiekowych dla Polski i średniej krajów UE (27 krajów - 2020) dla obu płci",
    subtitle = "*NEET - (ang. not in education, employment or training)\n nieuczęszczający do szkoły, niepracujący ani nieszkolący się",
    caption = "Dane: edat_lfse_23, baza danych Eurostat"
  ) +
  
  # BAR CHART DLA POLSKI
  geom_bar(
    data = NEET[NEET$geo == "PL", ],
    aes(x = time, y = values, fill = age),
    stat = "identity",
    position = position_dodge(),
    alpha = 0.65
  ) +
  
  # LINIA CELU UE2030
  geom_hline(yintercept = 9, linetype = "dashed", color = "red", ) +
  
  # LINE PLOT dla średniej UE27
  geom_point(
    shape=21,
    data = NEET[NEET$geo == "EU27_2020", ],
    aes(x = time, y = values, color = age),
    stat = "identity",
    size = 3,
    fill="white",
    stroke=1.7,
    #position=position_dodge()
    position=position_dodge(width = .9),
    #linewidth = 1.05,
  ) +
  
  # SKALE KOLORÓW BARPLOTU I POINTS
  scale_fill_manual(name = "Polska", values = kolory) +
  scale_color_manual(name = "Średnia dla\npaństw UE (EU27)", values = kolory) +
  
  #OKREŚLENIE RANGE DLA X i Y na wykresie
  coord_cartesian(
    xlim = c(2017.75, 2024.25),
    ylim = c(0, 20),
    clip = "off"
  ) +
  #DODANIE LAT DO OSI X
  scale_x_continuous(breaks = 2018:2024) +
  #NAZYW OSI
  labs(x = "Lata", y = "Odsetek NEET (%)") +
  #PODPIS LINI CELU UE2030
  annotate("text", x = 2023, y = 9.5, label = "Cel UE na rok 2030", size = 3.5) +
  
  # USTAWIENIA TŁA
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor.y = element_blank()
  )











# ggplot() +
#   labs(title = "Współczynniki NEET* wśród grup wiekowych dla Polski i średniej krajów UE(27 krajów - 2020)", 
#           subtitle = "*NEET - (ang. not in education, employment or training)\n nieuczęszczający do szkoły, niepracujący ani nieszkolący się ",
#        caption = "Dane: edat_lfse_23, baza danych Eurostat")+
#   #BAR CHART DLA POSLSKI
#   geom_bar(data = NEET[NEET$geo == "PL",],
#            aes(x = time, y = values, fill = age),
#            #color ="black" ,
#            stat="identity",
#            position=position_dodge(),
#            alpha = 0.5,
#            #position = 'identity', 
#            )+
#   scale_fill_viridis(option = "B", name="Polska")+
#   # LINIA CELU UE2030
#   geom_hline(yintercept=9, linetype="dashed", color = "red")+
#   
#   #LINE PLOT DLA SREDNIEJ UE27
#   geom_point(data = NEET[NEET$geo == "EU27_2020",],
#             aes(x = time, y = values, color = age),
#             stat="identity",
#             size = 3,
#             #position=position_dodge()
#             position=position_dodge(width = .9)
#             #linewidth = 1.05
#             )+
#   scale_color_viridis(option = "B", name="Średnia dla\npaństw UE (EU27)")+
#   
#   #OKREŚLENIE RANGE DLA X i Y na wykresie
#   coord_cartesian(xlim=c(2017.75, 2024.25),
#                   ylim = c(0, 20),
#                   clip = "off")+
#   
#   #WYSIWTLANIE WSZYTSKICH LAT NA OSI X
#   scale_x_continuous(breaks = 2018:2024)+
#   
#   # Tytuły legend 
#   guides(fill = guide_legend(title = c("Polska")), 
#          color = guide_legend(title = "Średnia dla\npaństw UE (EU27)"))+
#   labs(x="Lata", y="Odsetek NEET (%)") +
#   annotate("text", x=2023, y=9.5, label="Cel UE na rok 2030", size = 3.5) +
#   
#   #USTAWIENIA TŁA
#   theme_minimal() +
#   theme(
#     panel.grid.major.x = element_blank(), 
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.y = element_line(color = "grey80"),
#     panel.grid.minor.y = element_blank()  
#   )
# 
# dev.off() 
#   





