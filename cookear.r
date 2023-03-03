cookear<-function(x.y){
        
       olp <- c("A Coruña" = "steelblue", "Santa Eugenia de Ribeira" = "blue", "Cillero"="darkgreen",
          "Vigo" = "orange", "Avil?s-Gij?n" = "darkblue","Avil?s"="red", "Gij?n"="#00BFC4",
          "Santo?a" = "#7CAE00", "Cedeira"="forestgreen", "Finisterre"= "darkgoldenrod2",
          "Luarca" = "chartreuse4", "Muros"= "#619CFF", "Celeiro"="darkgreen", "Burela" ="yellowgreen",
          "Mar?n"= "mediumorchid", "San Vicente de la Barquera"= "tomato", "Isla Cristina" ="steelblue",
          "Punta Umbr?a" = "slateblue3", "Barbate"= "red3","Santander"= "red","Puerto de Santa Mar?a"="darkorchid2",
          "C?diz"="Chartreuse2", "Tarifa"= "coral1", "Ayamonte"= "coral3", "Sanl?car de Barrameda"= "darksalmon",
          "Castletown Bere" = "deeppink3", "Puerto de la Vega"="black", "Mux?a"="tomato2")
####PARA CARGAR LIBRERIAS
libs <- c('dplyr', 'tibble',
          "tidyr",# wrangling
          'stringr', "Hmisc"    # strings, scraping
          'knitr', 'kableExtra',  # table styling
          'lubridate', 'ggplot2', # time, plots
          'ggrepel')              # labels
invisible(lapply(libs, library, character.only = TRUE))
#cargamos la funcion cookear que nos servir? para hacer gr?ficos
source("https://raw.githubusercontent.com/pepeceb/Cookear/master/cookear.r")
#una funcion para hacer una cabecera corta y con data.frame
header<-function(x, y) {
  as.data.frame(head(x,2))
}
library(dplyr)
library (data.table)
library(ggplot2)
library(ggrepel)
install.packages("Hmisc")
library(Hmisc)
library(readr)
library(readxl)
library(stringr)
install.packages("lubridate")
library(lubridate)
muestreos_tallas<-fread("C:/Users/Usuario/Dropbox/IEO/2021/IEOUPMUETALSIRENO_2020.txt")
head (muestreos_tallas)

muestreos_tallas<-na.omit(muestreos_tallas, cols="EJEM_MEDIDOS")
muestreos_tallas$FECHA_MUE<-as.character (muestreos_tallas$FECHA_MUE)
muestreos_tallas$FECHA_MUE<-str_replace_all(muestreos_tallas$FECHA_MUE, "ENE", "JAN")
muestreos_tallas$FECHA_MUE<-str_replace_all(muestreos_tallas$FECHA_MUE, "ABR", "APR")
muestreos_tallas$FECHA_MUE<-str_replace_all(muestreos_tallas$FECHA_MUE, "AGO", "AUG")
muestreos_tallas$FECHA_MUE<-str_replace_all(muestreos_tallas$FECHA_MUE, "DIC", "DEC")
muestreos_tallas$FECHA<-dmy(muestreos_tallas$FECHA_MUE)
muestreos_tallas$QUARTER<-quarter(muestreos_tallas$FECHA)
header(muestreos_tallas)

colSums(is.na(muestreos_tallas))

tallas<-muestreos_tallas[,c("COD_ID", "FECHA","QUARTER","ESTRATO_RIM", "PUERTO","COD_TIPO_MUE","BARCO","ESP_MUE", "CATEGORIA",
                            "ESP_CAT","P_MUE_VIVO","P_VIVO", "TALLA", "EJEM_MEDIDOS", "SOP")]%>%as.data.frame()%>%
  distinct()
tallas<-tallas[complete.cases(tallas[c("EJEM_MEDIDOS", "P_MUE_VIVO")]),]
header (tallas)
substring2(tallas$PUERTO, "CILLERO") <- "CELEIRO"
substring2(tallas$PUERTO, "Cillero") <- "Celeiro"
pesos<-tallas%>%
  group_by(COD_TIPO_MUE,
           COD_ID, ESTRATO_RIM, PUERTO,FECHA,QUARTER,
           BARCO, TAXON=ESP_MUE,CATEGORIA, ESPECIE=ESP_CAT,P_VIVO) %>%
  summarise(
    EJEM_MEDIDOS_CAT=sum(EJEM_MEDIDOS),
    MUEST_SP_CAT= sum(SOP)
  )  %>%
  group_by(COD_ID, TAXON,ESPECIE) %>%
  mutate(
    MUEST_SP=sum(MUEST_SP_CAT)# este es el peso muestreado de la especie esa marea, de todas las categor?as
  )  %>%
  group_by(COD_ID, TAXON, CATEGORIA) %>%
  mutate(
    MUEST_CAT=sum(MUEST_SP_CAT)
  ) %>%
  group_by(COD_ID, ESPECIE) %>%
  mutate(
    PESO_SP_CAT=round((P_VIVO*MUEST_SP_CAT)/MUEST_CAT,2)
  )  %>%
  group_by(COD_TIPO_MUE,COD_ID,  ESTRATO_RIM, PUERTO,FECHA,
           BARCO , TAXON,ESPECIE)%>%
  mutate(
    EJEM_MEDIDOS_SP= sum(EJEM_MEDIDOS_CAT),#EJEMPLARES MEDIDOS DE LA SP EN LA MAREA
    PESO_SP=sum(PESO_SP_CAT), ##ESTE PESO DE LA ESPECIE EN LA MAREA
    PESO_SIRENO= sum(P_VIVO)) #PESO MAL PONDERADO DE SIRENO

header (pesos)
pesos1<-pesos[,c("COD_TIPO_MUE","COD_ID", "FECHA","QUARTER", "ESTRATO_RIM","PUERTO","BARCO",
                 "TAXON", "CATEGORIA", "ESPECIE",
                 "MUEST_SP_CAT", "PESO_SP_CAT", "MUEST_SP", "PESO_SP", "PESO_SIRENO")]
pesos1<-distinct(pesos1)

header(pesos1)
tallas1<-distinct(tallas[,c("COD_ID","FECHA", "QUARTER",  "ESTRATO_RIM","PUERTO", "COD_TIPO_MUE",
                            "ESP_MUE","CATEGORIA", "ESP_CAT","TALLA", "EJEM_MEDIDOS")])
header(tallas1)
colnames(tallas1)[colnames(tallas1) %in% c("ESP_MUE", "ESP_CAT")] <- c("TAXON", "ESPECIE")

head (as.data.frame(tallas2),10)
tallas1<-tallas1[complete.cases(tallas1[c("EJEM_MEDIDOS")]),]
as.data.frame(subset(tallas2,TAXON=="Lophius spp" & COD_ID =="201902609" & TALLA==42))
as.data.frame(subset(tallas2, COD_ID=="201902609" & CATEGORIA %in% c("Raspita", "Raspita negro" )))
tallas2<-full_join(pesos1, tallas1)%>%distinct()  %>%
  group_by(COD_ID,TALLA, ESPECIE)%>%
  mutate(
    EJEM_POND_CAT= round((PESO_SP_CAT*EJEM_MEDIDOS/MUEST_SP_CAT),2)
    
  )  %>% group_by(COD_ID,TALLA, ESPECIE)%>%
  mutate(
    EJEM_MED_TALLA=sum(EJEM_MEDIDOS),
    EJEM_POND_TALLA=sum(EJEM_POND_CAT),
    PESO_MUEST_TALLA= sum(MUEST_SP_CAT),
    PESO_DESEM_TALLA = sum (PESO_SP_CAT),
    EJEM_POND_METODOB= round((PESO_DESEM_TALLA*EJEM_MED_TALLA/PESO_MUEST_TALLA),2)
  )  %>%
  #<group_by( COD_ID, ESPECIE)  %>%
  
  #mutate(PONDERADOS=ifelse(COD_TIPO_MUE==2, EJEM_POND_METODOB, EJEM_PONDERADOS))%>%
  
  group_by( COD_ID, ESPECIE)  %>%
  mutate(
    EJEM_MED_MAREA=sum(EJEM_MEDIDOS),
    TALLA_MEDIA_MAREA= round (weighted.mean(TALLA, EJEM_POND_TALLA),2))%>%
  
  group_by(COD_ID, ESPECIE, CATEGORIA)%>%
  mutate(TALLA_MEDIA_CAT=round (weighted.mean(TALLA, EJEM_POND_CAT),2))
tallas2<-tallas2[,c(1,2,)]

tail (as.data.frame(tallas2))
as.data.frame(subset(tallas2,  CATEGORIA %in% c("Raspita", "Raspita negro" )))
prueba<- subset(tallas2, COD_ID=="202000093" & TAXON=="Lophius spp" & TALLA %in% c(45,53)
)%>% as.data.frame(); prueba


TALLAS<-tallas2[,c("COD_TIPO_MUE", "COD_ID","FECHA", "QUARTER", "ESTRATO_RIM","PUERTO","BARCO", "TAXON",
                   "ESPECIE", "TALLA_MEDIA_MAREA", "EJEM_MED_MAREA",
                   "PESO_SP")]%>% distinct()
as.data.frame(head (TALLAS))
subset(TALLAS, ESTRATO_RIM %in% c("RAPANTER_AC"))%>%as.data.frame()
TABLA<-as.data.frame(table(TALLAS$TAXON))%>% arrange(-Freq); head (TABLA,20)
subset(TALLAS, ESTRATO_RIM=="RAPANTER_AC")%>%as.data.frame()
mod <- lm(TALLA_MEDIA_MAREA~ESPECIE, data=TALLAS)
summary(mod)
cooksd <- cooks.distance(mod)
#CooksDist= as.data.frame(cooksd)

as.data.frame(head (tallas2))


#plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
#abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
#text(x=1:length(cooksd)+6, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),
#           names(cooksd),""), col="red")  # add labels


# influential observations.
#pero ahora lo hacemos con ggplot

cooksd2<-as.data.frame(cooksd)   #aÃ±adimos numero de observacion para cruzarlas


cooksd2$ObsNumber <- 1:length(cooksd)
TALLAS$ObsNumber <- 1:length(cooksd)
sp2<-full_join(TALLAS, cooksd2)%>%distinct()%>%arrange(ESTRATO_RIM, PUERTO, COD_ID)%>%
  arrange((ObsNumber))

subset(sp2, ESTRATO_RIM=="RAPANTER_AC")%>%as.data.frame()
sp2<-sp2[complete.cases(sp2[c("cooksd")]),]
dMean <- sp2 %>%
  group_by(ESPECIE, ESTRATO_RIM) %>%
  summarise(MN = mean(cooksd))%>%arrange(-MN)

dMean<-dMean[complete.cases(dMean[c("MN")]),]
sp3<-left_join(sp2, dMean)%>%distinct()%>%arrange(FECHA)


sp3<-sp3[complete.cases(sp3[c("MN")]),]
sp3<-sp3%>%group_by(ESTRATO_RIM,ESPECIE)%>%
  mutate(
    MAX=1.4*max(cooksd),
    t_max= max(TALLA_MEDIA_MAREA),
    t_min= min(TALLA_MEDIA_MAREA))%>%as.data.frame()


OUTLIERS<-subset(sp3, cooksd>4*MN & EJEM_MED_MAREA>1)%>%
  select(-c( MAX, num_cook, MN)); as.data.frame(OUTLIERS)
#write_xlsx(OUTLIERS,"Lophius.xlsx")




#sp3<-subset(sp3, ESTRATO_RIM %in% c("RAPANTER_AC"))
#sp3<-subset(sp3, num_cook>2 & ESPECIE %in% c("Chelidonichthys obscurus","Chelidonichthys cuculus","Trigla lyra", "Trigloporus lastoviza"))

library(ggbeeswarm)
sp3$PUERTO2<-ifelse(sp3$COD_TIPO_MUE==4, "A BORDO", sp3$PUERTO)
table(sp3$PUERTO2)
table(sp3$ESTRATO_RIM)
        AC<-subset(sp3, ESTRATO_RIM=="Merluccius merluccius")
uniq_species = unique(AC$ESPECIE)%>%as.data.frame()
        ggplot(data =sp3,
               mapping = aes(y = cooksd, x=PESO_SP, col=factor(PUERTO)))  +
                geom_point(data =distinct(AC),aes(size=PESO_SP))  +
                
                geom_hline(data = AC, aes(yintercept = 4*AC$MN),size=1.5, colour="red")  +
                guides(colour = guide_legend(override.aes = list(size=5,linetype=4))) +
                scale_size(range=c(2,5))  +
                facet_wrap(ESPECIE~ESTRATO_RIM, scales="free")   +
                geom_label(show.legend=FALSE,data=subset(sp3,cooksd>4*MN),aes(fontface="bold",
                                        PESO_SP,cooksd, label = paste( FECHA, "", "\n",                                                                                                                round(PESO_SP,2), "", "KG")),label.size = 1,segment.color="darkblue",
                                 arrow=arrow(length= unit(0.03,"npc"), type="closed",ends="first"),
                                 fill = "white" ,
                                 size=3.5, vjust=1, hjust=0.1)    +
                guides(colour = guide_legend(override.aes = list(size=4,linetype=4))) # +
        #theme(legend.position = "none")
}
       
