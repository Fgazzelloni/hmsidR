library(tidyverse) |> suppressPackageStartupMessages()
library(gstat)
no2 <- read_csv(system.file("external/no2.csv", 
                            package = "gstat"), show_col_types = FALSE)
library(sf)
# Linking to GEOS 3.11.1, GDAL 3.6.2, PROJ 9.1.1; sf_use_s2() is TRUE
crs <- st_crs("EPSG:32632")
st_as_sf(no2, crs = "OGC:CRS84", 
         coords = c("station_longitude_deg", 
                    "station_latitude_deg")) |>
  st_transform(crs) -> no2.sf
# no2.sf%>%View

# read_sf("data/de_nuts1.gpkg") |> st_transform(crs) -> de
"https://github.com/edzer/sdsr/raw/main/data/de_nuts1.gpkg" |> 
  read_sf() |> 
  st_transform(crs) -> de

ggplot() + 
  geom_sf(data = de) + 
  geom_sf(data = no2.sf, mapping = aes(col = NO2))

library(stars) |> suppressPackageStartupMessages()
st_bbox(de) |>
  st_as_stars(dx = 10000) |>
  st_crop(de) -> grd
grd

library(gstat)
i <- idw(NO2~1, no2.sf, grd)
i
# [inverse distance weighted interpolation]
p1 <- ggplot() + 
  geom_stars(data = i, 
                      aes(fill = var1.pred, x = x, y = y)) + 
  xlab(NULL) + ylab(NULL) +
  geom_sf(data = st_cast(de, "MULTILINESTRING")) + 
  geom_sf(data = no2.sf)

no2.sf%>%class

# the variogram considers the distance between pair points
# and calculate the mean of the values of the observed phenomenon
mean(no2.sf$NO2)
var(no2.sf$NO2)
lm(NO2~1, no2.sf)
#?gstat::variogram
v <- variogram(NO2~1, no2.sf)
v%>%class

options(scipen = 666)
tibble(dist=v$dist,gamma=v$gamma,point=v$np)%>%
  ggplot(aes(dist,gamma))+
  geom_point(shape=21,stroke=0.5,
             size=3,
             color="steelblue")+
  geom_smooth(se=F)+
  geom_text(aes(label=point),hjust=-0.5)+
  labs(x = "distance h [m]",y=expression(gamma(h)))+
  xlim(0, 1.055 * max(v$dist))+
  theme_bw()

plot(v, plot.numbers = TRUE, xlab = "distance h [m]",
     ylab = expression(gamma(h)),
     xlim = c(0, 1.055 * max(v$dist)))

v0 <- variogram(NO2~1, no2.sf, cutoff = 100000, width = 10000)
plot(v0, plot.numbers = TRUE, xlab = "distance h [m]",
     ylab = expression(gamma(h)),
     xlim = c(0, 1.055 * max(v0$dist)))
v.m <- fit.variogram(v, vgm(1, "Exp", 50000, 1))
v.m

#?gstat::krige
k <- krige(NO2~1, no2.sf, grd, v.m)
k


# [using ordinary kriging]
p2 <- ggplot() + 
  geom_stars(data = k, 
             aes(fill = var1.pred, x = x, y = y)) + 
  xlab(NULL) + ylab(NULL) +
  geom_sf(data = st_cast(de, "MULTILINESTRING")) + 
  geom_sf(data = no2.sf) +
  coord_sf(lims_method = "geometry_bbox")

library(patchwork)
p1|p2
