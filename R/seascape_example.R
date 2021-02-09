### Map latest seascape monthly value from erddap
### the source of data is `noaa_aoml_4729_9ee6_ab54` See https://cwcgom.aoml.noaa.gov/erddap/info/noaa_aoml_4729_9ee6_ab54/index.html
### data is extracted with `rerddap::griddap` for a selected region of interest.
### By E. Montes (emontesh@usf.edu)
### 2021-01-25

library(rerddap)
library(ggplot2)
library(mapdata)
library(tidyverse)
library(leaflet)
library(leafem)
library(raster)
library(viridis)
library(data.table)
library(glue)
library(raster)

# query params for seascapes
seascapeInfo <- info("noaa_aoml_4729_9ee6_ab54", url = "https://cwcgom.aoml.noaa.gov/erddap/")

# get the lastest monthly seascape classification
get_dates <- function(info){
  info$alldata$time %>%
    filter(attribute_name=="actual_range") %>%
    pull(value) %>%
    str_split(", ", simplify = T) %>%
    as.numeric() %>%
    as.POSIXct(origin = "1970-01-01", tz = "GMT")
}
d <- get_dates(seascapeInfo)[2]

# set lan/lon of site to center seascape map
site_lon <- -81.3
site_lat <- 24.5

# define the size of the domain
box <- 10

# render a map with the latest seascape
leaflet(
  options = leafletOptions(
    crs = leafletCRS(crsClass = "L.CRS.EPSG4326"))) %>%
  # basemap from GBIF in 4326
  addTiles("//tile.gbif.org/4326/omt/{z}/{x}/{y}@1x.png?style=gbif-geyser") %>%
  addWMSTiles(
    baseUrl = 'https://cwcgom.aoml.noaa.gov/erddap/wms/noaa_aoml_4729_9ee6_ab54/request?',
    layers = "noaa_aoml_4729_9ee6_ab54:CLASS",
    options = WMSTileOptions(
      version = "1.3.0", format = "image/png", transparent = T, opacity = 0.7,
      time = format(d,"%Y-%m-%dT00:00:00Z")))  %>%
  addMarkers(lng = site_lon, lat = site_lat)  %>%
  addMouseCoordinates() %>%
  fitBounds(site_lon - box, site_lat - box, site_lon + box, site_lat + box) %>%
  addLegend(
    position="bottomright",
    title = paste0("CLASS<br>", format(d,"%Y-%m-%d")),
    colorNumeric("Spectral", c(1,33), reverse=T), seq(1,33))

# define domain
n_lat <- site_lat + box
s_lat <- site_lat - box
e_lon <- site_lon + box
w_lon <- site_lon - box

# define time period (use same yyyy-mm for 8d seascapes)
t0 <- '2020-11'
tf <- '2020-11'

# define product P: probability or CLASS: seascape class
prod = "CLASS"

# download as csv
# weekly (8-day): https://cwcgom.aoml.noaa.gov/erddap/info/noaa_aoml_seascapes_8day/index.html
df_8d <- fread(glue("http://cwcgom.aoml.noaa.gov/erddap/griddap/noaa_aoml_seascapes_8day.csv?{prod}[({t0}-01):1:({tf}-01)][({s_lat}):1:({n_lat})][({w_lon}):1:({e_lon})]"))
# monthly: https://cwcgom.aoml.noaa.gov/erddap/info/noaa_aoml_4729_9ee6_ab54/index.html
df_mo <- fread(glue("http://cwcgom.aoml.noaa.gov/erddap/griddap/noaa_aoml_4729_9ee6_ab54.csv?{prod}[({t0}-01):1:({tf}-01)][({s_lat}):1:({n_lat})][({w_lon}):1:({e_lon})]"))

# delete first row
df_8d <- df_8d[-1, ]
df_mo <- df_mo[-1, ]

# extraction of class values within polygon (for now only works with a single time period)
df_mo_2 <- data.frame(cbind(as.numeric(df_mo$longitude), as.numeric(df_mo$latitude), as.numeric(df_mo$CLASS)))
coordinates(df_mo_2) <- ~ X1 + X2
gridded(df_mo_2) <- TRUE
rasterDF <- raster(df_mo_2)

# define polygon
e_pt <- -80
w_pt <- -82
n_pt <- 23
s_pt <- 21
poly <- spPolygons(rbind(c(w_pt, s_pt), c(e_pt, s_pt), c(e_pt, n_pt), c(w_pt, n_pt), c(w_pt, s_pt)))

# extract polygon seascape values
seas_poly <- extract(rasterDF, poly)

# create map with downloaded monthly data (for now only works with a single time period)
# generate map
names(df_mo) = c("dateTime", "lat", "lon", "SCclass")
rr_mo <- rasterFromXYZ(cbind(as.numeric(df_mo$lon), as.numeric(df_mo$lat), as.numeric(df_mo$SCclass)))
crs(rr_mo) <- CRS("+init=epsg:4326")
pal <- colorNumeric("Spectral", values(rr_mo), reverse=T,
                    na.color = "transparent") # other palette: c("#0C2C84", "#41B6C4", "#FFFFCC")
m_mo <- leaflet() %>% addTiles() %>% addRasterImage(rr_mo, colors = pal, opacity = 0.8) %>%
  addLegend(pal = pal, values = values(rr_mo),
            title = "CLASS")
m_mo

# create map with downloaded 8-day data (for now only works with a single time period)
# generate map
names(df_8d) = c("dateTime", "lat", "lon", "SCclass")
rr_8d <- rasterFromXYZ(cbind(as.numeric(df_8d$lon), as.numeric(df_8d$lat), as.numeric(df_8d$SCclass)))
crs(rr_8d) <- CRS("+init=epsg:4326")
pal <- colorNumeric("Spectral", values(rr_8d), reverse=T,
                    na.color = "transparent") # other palette: c("#0C2C84", "#41B6C4", "#FFFFCC")
m_8d <- leaflet() %>% addTiles() %>% addRasterImage(rr_8d, colors = pal, opacity = 0.8) %>%
  addLegend(pal = pal, values = values(rr_8d),
            title = "CLASS")
m_8d

# # download as netCDF
# download.file(url=glue("https://cwcgom.aoml.noaa.gov/thredds/ncss/SEASCAPE_MONTH/SEASCAPES.nc?var=CLASS&var=P&north={n_lat}&west={w_lon}&east={e_lon}&south={s_lat}&disableProjSubset=on&horizStride=1&time_start={t0}-15T12%3A00%3A00Z&time_end={tf}-15T12%3A00%3A00Z&timeStride=1&addLatLon=true&accept=netcdf"), destfile="~/ERDDAP-extractions/test.nc")
# nc_data <- nc_open(file="~/ERDDAP-extractions/test.nc")
#
# # lon <- ncvar_get(nc_data, "lon")
# lat <- ncvar_get(nc_data, "lat", verbose = F)
# t <- ncvar_get(nc_data, "time")
# CLASS <- ncvar_get(nc_data, "CLASS")
# last <- data.frame(CLASS[ , , 2])
