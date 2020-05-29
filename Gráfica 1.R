# Titulo:   Analisis descriptivo datos encuesta condiciones laborales de repartidores
# Datos:    Datos Encuesta condiciones laborales de repartidores
# Fecha:    28-05-2020
# Autores:  Gatitos Contra la Desigualdad


# --- Directory and packages
rm(list = ls())
setwd("~/Documents/Encuestas/ECLR")
library(pacman)

p_load(survival, data.table, tidyverse, reshape2, gghighlight, RColorBrewer, wesanderson, 
       ggpubr, gridExtra, magrittr,ggrepel)

# ---  Importar los datos
datitos <- read.csv("datos_28052020.csv", header = T)





##Figure 1 
#####################
#Principal AES
ggplot(data =datitos,
       mapping  = aes(x = utilidades_dia  
                      #y = porc_viv,
                      #color=elec_gob_2020_21,
                      #size=hli,
                      #label = ent_corto
                      )
                      ) +
  #Geom_point
geom_density(fill="#dc5356", color="#dc5356", alpha=0.8)+
  #Theme    
  theme_minimal() +
  theme (text = element_text(family = "Verdana",color="#3a3a39"),
         panel.grid = element_blank(),
         plot.margin = unit(c(10,30,10,20), units = "point"),
         panel.background = element_rect(fill = "white", linetype = "blank"),
         plot.background = element_rect(fill = "white", linetype = "blank"),
         plot.title = element_text(face = "bold", color="#dc5356"),
         plot.subtitle = element_text(face = "bold", color="#3a3a39"),
         plot.caption = element_text(hjust = 0.5, lineheight = 0.9, color="#6f6f6f", size = 8),
         legend.title = element_text( size = 10),
         #legend.text = element_text( size = 10)
  )+
  #Scale Y  
  scale_y_continuous( 
                      #breaks = seq(0,70,10)
  ) +
  #Scale X  
  scale_x_continuous(labels =scales::dollar
                     #breaks = seq(0,90,10)
  ) +
  #Labels and titles
  labs ( x = "Utilidades por día",
         y = "Densidad",
         title = "Utilidades diarias generadas por repartidorxs de apps",
         subtitle = "Con corte al 28 de mayo",
         caption = "Fuente: Elaborado por @gatitosvsdesig, con datos de 'Encuesta condiciones laborales de repartidores'"
         #color = "¿Elecciones guberen 2020-21?",
         #size= "% habla lengua indígena"
         )+
    #Lineas de mediana y media
  #Linea 1:
  geom_vline(aes(xintercept=quantile(utilidades_dia, 0.25)),
             color="#3a3a39", linetype="dashed", size=0.5)+
  geom_text(aes(x=quantile(utilidades_dia, 0.25), label="25% con menos de $141\n", y=0.001), colour="#3a3a39", angle=90, text=element_text(size=11)
            )+
  #Linea 2:
geom_vline(aes(xintercept=median(utilidades_dia)),
           color="#3a3a39", linetype="dashed", size=0.5)+
geom_text(aes(x=median(utilidades_dia), label="Mediana: $213\n", y=0.001), colour="#3a3a39", angle=90, text=element_text(size=11))
#Save Figure 1
ggsave("Utilidades diarias_w.png", width = 7)



#Quitar outlier

datitos %<>%
  filter(utilidades_dia<345)



##Figure 2
#####################
#Principal AES
ggplot(data =datitos,
       mapping  = aes(x = utilidades_hora  
                      #y = porc_viv,
                      #color=elec_gob_2020_21,
                      #size=hli,
                      #label = ent_corto
       )
) +
  #Geom_point
  geom_density(fill="#dc5356", color="#dc5356", alpha=0.8)+
  #Theme    
  theme_minimal() +
  theme (text = element_text(family = "Verdana",color="#3a3a39"),
         panel.grid = element_blank(),
         plot.margin = unit(c(10,30,10,20), units = "point"),
         panel.background = element_rect(fill = "white", linetype = "blank"),
         plot.background = element_rect(fill = "white", linetype = "blank"),
         plot.title = element_text(face = "bold", color="#dc5356"),
         plot.subtitle = element_text(face = "bold", color="#3a3a39"),
         plot.caption = element_text(hjust = 0.5, lineheight = 0.9, color="#6f6f6f", size = 8),
         legend.title = element_text( size = 10),
         #legend.text = element_text( size = 10)
  )+
  #Scale Y  
  scale_y_continuous( 
    #breaks = seq(0,70,10)
  ) +
  #Scale X  
  scale_x_continuous(labels =scales::dollar
                     #breaks = seq(0,90,10)
  ) +
  #Labels and titles
  labs ( x = "Utilidades",
         y = "Densidad",
         title = "Utilidades por hora, generadas por repartidorxs de apps",
         subtitle = "Con corte al 28 de mayo",
         caption = "Fuente: Elaborado por @gatitosvsdesig, con datos de 'Encuesta condiciones laborales de repartidores'"
         #color = "¿Elecciones guberen 2020-21?",
         #size= "% habla lengua indígena"
  )+
  #Lineas de mediana y media
  #Linea 1:
  geom_vline(aes(xintercept=quantile(utilidades_hora, 0.25)),
             color="#3a3a39", linetype="dashed", size=0.5)+
  geom_text(aes(x=quantile(utilidades_hora, 0.25), label="25% inferior gana $26 por hora o menos \n", y=0.0085), colour="#3a3a39", angle=90, text=element_text(size=7)
  )+
  #Linea 3:
  geom_vline(aes(xintercept=quantile(utilidades_hora, 0.75)),
             color="#3a3a39", linetype="dashed", size=0.5)+
  geom_text(aes(x=quantile(utilidades_hora, 0.75), label="25% superior gana $54 por hora o más \n", y=0.008), colour="#3a3a39", angle=90, text=element_text(size=7)
  )+
  #Linea 2:
  geom_vline(aes(xintercept=median(utilidades_hora)),
             color="#3a3a39", linetype="dashed", size=0.5)+
  geom_text(aes(x=median(utilidades_hora), label="Mediana: $42 por hora \n", y=0.008), colour="#3a3a39", angle=90, text=element_text(size=11))
#Save Figure 1
ggsave("Utilidades por hora.png", width = 7)


