library(sf)
library(rnaturalearth)
library(dply)
library(gtable)
library(ggplot2)
library(raster)
library(rlang)
library(elevatr)
library(ggnewscale)
world        <- ne_countries(scale= "small", returnclass = "sf") # Continentes del mundo
world.SA     <- subset(world, subregion=="South America") 
Ecuador            <- getData('GADM', country='Ecuador', level=0) %>% st_as_sf()
Ecuado            <- getData('GADM', country='Ecuador', level=1) %>% st_as_sf()

Carchi         <- subset(Ecuado, NAME_1  == "Carchi")
Esmeralda     <- subset(Ecuado, NAME_1  == "Esmeraldas")
Imbabura     <- subset(Ecuado, NAME_1  == "Imbabura")
Imbabura_box     = st_as_sfc(st_bbox(Imbabura ))

elev = get_elev_raster(Imbabura, z=12)
Poligo_alt    <- crop(elev, Imbabura)                           #   
Poligo_alt   <- Poligo_alt <- mask(Poligo_alt, Imbabura)

slopee    = terrain(Poligo_alt  , opt = "slope") 
aspecte    = terrain(Poligo_alt, opt = "aspect")
hille     = hillShade(slopee, aspecte, angle = 40, direction = 270)

hill.p        <-  rasterToPoints(hille)
hill.pa_      <-  data.frame(hill.p)


A= ggplot()+
  geom_sf(data = world.SA, fill=NA, color="black", size=0.2)+
  geom_sf(data = Ecuador, fill="gray80", color="black")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotate(geom = "text", x = -50, y = -50, hjust = 0, vjust = 1, 
           label = "Suramerica",size = 3, family="serif", color = "black")

B= ggplot()+
  geom_sf(data = Ecuado, fill=NA, color="black", size=0.2)+
  geom_sf(data = Carchi , fill="gray80", color="black", size=0.2)+
  geom_sf(data = Esmeralda, fill="gray80", color="black", size=0.2)+
  geom_sf(data = Imbabura, fill="gray80", color="black", size=0.2)+
  geom_sf(data=Imbabura_box, fill=NA, color="black")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotate(geom = "text", x = -78, y = -5, hjust = 0, vjust = 1, 
           label = "Ecuador",size = 3, family="serif", color = "black")+
  coord_sf(xlim = c(-81.5 ,-75), ylim = c(-7  ,3),expand = FALSE)


C= ggplot()+
  geom_sf(data = Ecuado, fill=NA, color="black", size=0.2)+
  geom_sf(data = Carchi , fill="gray80", color="black")+
  geom_sf(data = Esmeralda, fill="gray80", color="black")+
  geom_sf(data = Imbabura, fill="gray80", color="black")+
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  coord_sf(xlim = c(-79 ,-77.5), ylim = c(-0.5  ,1.5))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  geom_sf_text(data = Ecuado , aes(label = NAME_1 ), 
               family="serif", color = "black",  fontface="italic", box.padding = unit(0.9, "lines"), 
               size = 3, face = "bold",color = 'black',
               point.padding = unit(0.9, "lines"))+
  annotate(geom = "text", x = -78.4, y = 1.3, hjust = 0, vjust = 1, 
           label = "COLOMBIA",size = 4, family="serif", color = "black")+
  ggspatial::annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book", text_col="black")+
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey10", "white"),
      line_col = "grey10", text_family = "ArcherPro Book" , text_col="black"))

library(ggpubr)
plots=  ggarrange(C,B,A, 
                 nrow = 1,ncol = 3)

plot= annotate_figure(plots,
                bottom = text_grob("Codigo desarrollado por: \n Ing. Gorky Florez Castillo", color = "black", family="serif",
                                   hjust = 1, x = 1, face = "italic", size = 10))

ggsave(plot=plot,"Mapa/ecuador.png",units = "cm",width = 17, # ancho
       height = 9, # alto
       dpi=1200)

