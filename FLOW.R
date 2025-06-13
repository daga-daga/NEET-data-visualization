
p<-c("eurostat", "ggplot2", "tidyr", "stringr", "ggalluvial", "dplyr",
     "skimr", "ggfortify")
for (i in p){
  if(!require(i, character.only = TRUE)){install.packages(i)}
  library(i, character.only = TRUE)
}
rm(p, i)


CNTR_eu<-unlist(eu_countries %>%
                  mutate(geo=code) %>%
                  select(geo))

NEET <- get_eurostat("edat_lfse_34", time_format = "num",
                     filters = list(time = c(2024),
                                    geo="IT",
                                    freq = "A",
                                    age="Y20-34",
                                    sex=c("M", "F"),
                                    deg_urb=c("DEG1", "DEG2", "DEG3"),
                                    duration="TOTAL",
                                    isced11=c("ED0-2", "ED3_4", "ED5-8")
                                    ))%>%
  select(geo, sex, deg_urb, isced11, values) %>%
  filter(!is.na(values))

NEET <- NEET %>%
  mutate(isced11 = factor(isced11,
                          levels = c("ED0-2", "ED3_4", "ED5-8"),
                          labels = c("Podstawowe", "Średnie", "Wyższe"))) %>%
  mutate(sex = factor(sex,
                          levels = c("F", "M"),
                          labels = c("Kobiety", "Mężczyźni"))) %>%
  mutate(deg_urb = factor(deg_urb,
                          levels = c("DEG1", "DEG2", "DEG3"),
                          labels = c("Miasto", "Przedmieścia", "Wieś")))
  

kolory <- viridis(3, option = "B", begin = 0.1, end = 0.85)

ggplot(NEET,
       aes(axis1 = sex, axis2 = deg_urb, axis3 = isced11, y = values)) +
  geom_alluvium(aes(fill = isced11), width = 1/12, alpha = 0.75) +
  geom_stratum(width = 1/12, fill = "grey80", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
  
  scale_x_discrete(limits = c("Płeć", "Stopień urbanizacji", "Poziom wykształcenia"),
                   expand = c(0.1, .001)) +
  scale_fill_manual(values = kolory) +
  
  labs(
    title = "Przepływ NEET według płci, urbanizacji i poziomu wykształcenia dla roku 2024\n wśród osób w wieku 20-24",
    y = "Udział NEET",
    fill = "Poziom\nwykształcenia\n(ISCED11)"
  ) +
  
  theme_minimal() +
  theme(legend.position = "right",
        axis.text.y = element_blank())




