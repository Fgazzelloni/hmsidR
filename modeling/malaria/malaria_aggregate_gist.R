# Malaria Drugs
## Aggregate Values

# -   Article: <https://www.thelancet.com/journals/lanmic/article/PIIS2666-5247(23)00063-0/fulltext>
# -   GitHub repository: <https://github.com/hannaehrlich/maldrugres_SSA>
  
library(tidyverse)
library(sf)
library(gstat)
library(rnaturalearth)
library(patchwork)

url <- "https://raw.githubusercontent.com/hannaehrlich/maldrugres_SSA/main/Survey_MolecMarker_Data.csv"

maldrugres <- read.csv(url)
# names(maldrugres)

maldrugres_2 <- maldrugres %>%
  filter(!is.na(Lat),!is.na(Drug),!Drug=="") %>%
  janitor::clean_names()%>%
  select(country,site,lon,lat,present,drug)


africa <- ne_countries(continent = "Africa",
                       returnclass = 'sf')
crs_afr <- st_crs(africa)
crs<- "ESRI:102023"

new <- africa %>% st_transform(crs)
afr <- st_make_valid(new)

maldrugres_sf <- maldrugres_2%>%
  st_as_sf(coords = c(3,4),crs=crs_afr)%>%
  st_transform(crs)

maldrugres_sf_avg <- maldrugres_sf %>%
  group_by(country)%>%
  mutate(avg_present=mean(present))

p1 <- ggplot(africa)+
  geom_sf(aes(geometry=geometry))+
  geom_sf(data=maldrugres_sf_avg,
          aes(geometry=geometry,color=avg_present))+
  scale_color_gradientn(colors = sf.colors(20))+
  labs(title="Point Values",  color="present")


a <- aggregate(maldrugres_sf["present"], 
               by = afr, 
               FUN = mean)
a%>%class

p2 <- a |> 
  select(present) |>
  ggplot() + 
  geom_sf(mapping = aes(fill = present)) + 
  scale_fill_gradientn(colors = sf.colors(20),
                       na.value = "grey90")+
  labs(title="Aggregate values")


(p1 + p2) +
  patchwork::plot_layout(guides = "collect") +
  labs(caption = "Graphics: Federica Gazzelloni") &
  ggthemes::theme_map()+
  theme(legend.position = "bottom") 

ggsave("~/Documents/R/R_general_resources/Spatials/infectious_container/malaria/images/point_aggragate.png",
       bg="white")


