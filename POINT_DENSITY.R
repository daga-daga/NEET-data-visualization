p<-c("eurostat", "ggplot2", "tidyr", "stringr", "knitr", "dplyr",
     "skimr", "ggfortify", "lmtest","ggExtra", "cowplot", "htmlwidgets", 
     "ggrepel","plotly", "viridis", "patchwork")
for (i in p){
  if(!require(i, character.only = TRUE)){install.packages(i)}
  library(i, character.only = TRUE)
}
rm(p, i)



#Przygotowanie zmiennych pod instalację danych 
CNTR_eu<-c(unlist(eu_countries %>%
                    mutate(geo=code) %>%
                    select(geo)))
CNTR_df<-(eu_countries %>%
            mutate(geo=code) %>%
            select(geo, name))
CNTR_df<-rbind(CNTR_df)

YEARS<-c(2024)
AGE<-c("Y15-29")
EDUC<-c("ED0-2", "ED3_4", "ED5-8")
### Pobranie danych z eurostatu rok 2024
#Pobieram dane: NEET
#jak zmieniał się neet na przestrzeni lat w kracjahc ue
NEET<-get_eurostat(id="edat_lfse_21", time_format = "num", 
                   filters = list(
                     geo = CNTR_eu,
                     age=AGE,
                     sex="T",
                     time=YEARS,
                     isced11=EDUC,
                     age = AGE,
                     freq = "A")) %>%
  select(geo, values, isced11)
names(NEET)<-c("geo", "neet", "isced11")

NEET<- CNTR_df %>%
  left_join(x = NEET, by = c("geo" = "geo"), keep = FALSE )
# sum(is.na(NEET))
# colSums(is.na(NEET))
AGE<-c("Y15-74")
UNEMP<-get_eurostat(id="lfst_r_urgau", time_format = "num", 
                   filters = list(
                     geo = CNTR_eu,
                     age=AGE,
                     sex="T",
                     time=YEARS,
                     deg_urb=EDUC,
                     unit="PC_ACT",
                     age = AGE,
                     freq = "A")) %>%
  select(geo, values, isced11)
names(UNEMP)<-c("geo", "unemp", "isced11")


UNEMP_NEET <- NEET %>%
  left_join(UNEMP, by = c("geo", "isced11"))

#WYKRES ZE UNEMP I NEET
setwd("C:\\Users\\dagma\\OneDrive\\Pulpit\\PROJEKT_WIZUALIZACJA\\Wizualizacja\\WYKRES")
UNEMP_NEET <- UNEMP_NEET %>%
  mutate(isced11 = factor(isced11,
                          levels = c("ED0-2", "ED3_4", "ED5-8"),
                          labels = c("Podstawowe\nlub niższe", "Średnie", "Wyższe")))
  

kolory <- viridis(3, option = "B", begin = 0.1, end = 0.85)

p <- ggplot(UNEMP_NEET, aes(x = unemp, y = neet, color = isced11, text=name)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(
    x = "Stopa bezrobocia (%)",
    y = "Współczynnik NEET (%)",
    title = "Współczynnik NEET osób w wieku 15-29, a wskaźnik bezrobocia\n wśród osób aktywnych zawodowo w wieku 15-74 z podziałem\nna stopnie urbanizacji",
    subtitle = "Dane dla roku 2024",
    caption = "Dane: une_educ_a, baza danych Eurostat",
    color = "Poziom\nwykształcenia\n(ISCED11)"
  ) +
  scale_color_manual(values = kolory)+
  xlim(0, 40) + 
  ylim(0, 40) +
  theme_minimal()+
  theme(
    panel.grid.major.x = element_line(color = "grey80"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor.y = element_blank(),
    legend.position = "left"
  )

p_interactive <- ggplotly(p, tooltip = c("text", "x", "y", "color"))
saveWidget(p_interactive, "interactive_neet_unemp.html", selfcontained = TRUE)


p<-ggMarginal(p, type = "density", groupColour = TRUE, groupFill = TRUE, 
              margins = "both")
p













