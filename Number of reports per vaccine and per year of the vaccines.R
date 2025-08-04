install.packages("tidyverse")
library(tidyverse)
library(readxl)

year_CAPVAXIVE <- read_excel("C:/Users/kuo/Desktop/vaers/1.PNEUMO/CAPVAXIVE.xlsx") %>% 
  mutate(vaccine = "CAPVAXIVE")

year_PNEUMOVAX <- read_excel("C:/Users/kuo/Desktop/vaers/1.PNEUMO/PNEUMOVAX.xlsx")%>% 
  mutate(vaccine = "PNEUMOVAX")

year_PNUIMUNE <- read_excel("C:/Users/kuo/Desktop/vaers/1.PNEUMO/PNUIMUNE.xlsx")%>% 
  mutate(vaccine = "PNU-IMUNE")

year_PREVNAR <- read_excel("C:/Users/kuo/Desktop/vaers/1.PNEUMO/PREVNAR.xlsx")%>% 
  mutate(vaccine = "PREVNAR")

year_PREVNAR13 <- read_excel("C:/Users/kuo/Desktop/vaers/1.PNEUMO/PREVNAR13.xlsx")%>% 
  mutate(vaccine = "PREVNAR13")

year_PREVNAR20 <- read_excel("C:/Users/kuo/Desktop/vaers/1.PNEUMO/PREVNAR20.xlsx")%>% 
  mutate(vaccine = "PREVNAR20")

year_SYNFLORIX <- read_excel("C:/Users/kuo/Desktop/vaers/1.PNEUMO/SYNFLORIX.xlsx")%>% 
  mutate(vaccine = "SYNFLORIX")

year_VAXNEUVANCE <- read_excel("C:/Users/kuo/Desktop/vaers/1.PNEUMO/VAXNEUVANCE.xlsx")%>% 
  mutate(vaccine = "VAXNEUVANCE")

year_NOBRANDNAME <- read_excel("C:/Users/kuo/Desktop/vaers/1.PNEUMO/NOBRANDNAME.xlsx")%>% 
  mutate(vaccine = "NO BRAND NAME")


year <- dplyr::bind_rows(year_CAPVAXIVE, year_PNEUMOVAX, year_PNUIMUNE, year_PREVNAR, year_PREVNAR13, 
                         year_PREVNAR20, year_SYNFLORIX, year_VAXNEUVANCE, year_NOBRANDNAME)

year$vaccine <- factor(year$vaccine, levels = c("CAPVAXIVE", "PNEUMOVAX", "PNU-IMUNE", "PREVNAR", "PREVNAR13",
                                                "PREVNAR20", "SYNFLORIX", "VAXNEUVANCE", "NO BRAND NAME"))

p <- ggplot(year, aes(x = GETDATAYEAR, y = n, fill = vaccine)) +
  geom_col(position = "stack") +
  #Set the theme
  theme_bw() +
  #Set the title
  labs(title = " ",x = "Years", y = "Number of reports") +
  #Remove legend title and center the main title
  theme(plot.title = element_text(hjust = 0.5) ,legend.title = element_blank()) +
  #Customize colors
  scale_fill_manual(values = c("CAPVAXIVE" = "#925E9FB2", "PNEUMOVAX" = "#ED0000B2", "PNU-IMUNE" = "#1B1919B2", 
                               "PREVNAR" = "#0099B4B2", "PREVNAR13" = "#00468BB2", "PREVNAR20" = "#FDAF91B2", 
                               "SYNFLORIX" = "#AD002AB2", "VAXNEUVANCE" = "#ADB6B6B2", "NO BRAND NAME" = "#42B540B2"
                               )) +
  #Add numerical tags
  #  geom_text(
  #    aes(label = n, group = vaccine), 
  #    position = position_stack(vjust = 0.5), 
  #    size = 3, 
  #    color = "black"
  #  ) +
  scale_x_continuous(breaks = seq(min(year$GETDATAYEAR), max(year$GETDATAYEAR), by = 1))

print(p)
ggsave('figure 1.png',dpi=600,width=16,height=10)


