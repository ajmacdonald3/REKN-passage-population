################################################################################
# MAPS for MacDonald et al. JWM
# 3-Feb-2021
################################################################################

# load packages
library(tidyverse)
library(lubridate)
library(scales)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(cowplot)

windowsFonts(Times=windowsFont("TT Times New Roman"))

# set up basic things for maps 
theme_set(theme_bw())
world <- ne_countries(scale = "medium", returnclass = "sf")
#lakes <- ne_download(scale = "medium", type = 'lakes', category = 'physical',
#returnclass = "sf", destdir = "./map-data/lakes") # only need this first time downloading, saves shape file in folder called map-data in your working directory
lakes <- ne_load(type = "lakes", scale = "medium", category = 'physical',
                 returnclass = "sf",
                 destdir = paste0(getwd(), "./map-data/lakes")) # use this if already downloaded shapefiles

# get better resolution basemap
region <- ne_states(country = "Canada", returnclass = "sf") %>%
  filter(name %in% c("Ontario", "Québec", "Nunavut"))

# create dataframe with camp locations
camp_locs <- data.frame(long = c(-80.571, -80.692, -80.452),
                        lat = c(51.6557, 51.7989, 51.4844),
                        camp = c("Little Piskwamish Point", "Longridge Point", "Northbluff Point"))

# set map limits
xmin <- -82.7
xmax <- -78.1
ymin <- 51.1
ymax <- 53.5

# plot map
site_map <- ggplot(data = region) +
  geom_sf(fill = "grey70", colour = NA) +
  geom_sf(data = lakes, fill = "white", colour = NA) +
  geom_point(data = camp_locs,
             aes(x = long, y = lat),
                 size = 3) +
  geom_text(data = camp_locs,
            aes(x = long, y = lat, label = camp),
            size = 4, hjust = "right", nudge_x = -0.15, family = "Times") +
  annotate("text", label = "Akimiski\nIsland", family = "Times", x = -81.4, y = 53, size = 3) +
  annotation_scale(location = "br", line_width = 0.25,
                   text_cex = 0.5, height = unit(0.15, "cm")) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(1.25, "cm"), pad_y = unit(0.5, "cm"),
                         style = north_arrow_fancy_orienteering,
                         height = unit(0.75, "cm"), width = unit(0.75, "cm")) +
  coord_sf(
    xlim = c(xmin, xmax),
    ylim = c(ymin, ymax),
    expand = FALSE
  ) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none",
        plot.margin = margin(0, 0, 0, 0))

# inset map
region2 <- ne_states(country = c("Canada", "United States of America"),
                    returnclass = "sf")

# set map limits
xmin2 <- -97
xmax2 <- -60
ymin2 <- 41
ymax2 <- 57

inset <- ggplot(data = region2) +
  geom_sf(fill = "grey70", colour = "white", size = 0.2) +
  geom_sf(data = lakes, fill = "white", colour = NA) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = NA, colour = "black", size = 0.5) +
  geom_point(aes(x = -63.9, y = 50.2), colour = "black", size = 2) +
  geom_label(aes(x = -63.9, y = 52.2, label = "Mingan\nArchipelago"),
             family = "Times",  size = 2.5, fill = "grey70", label.size = 0) +
  annotate("text", label = "James\nBay", family = "Times", x = -84.7, y = 52.3, size = 2.5) +
  annotation_scale(location = "br", line_width = 0.25,
                   pad_x = unit(0.1, "cm"), pad_y = unit(0.1, "cm"),
                   text_cex = 0.5, height = unit(0.1, "cm")) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.1, "cm"), pad_y = unit(0.25, "cm"),
                         style = north_arrow_fancy_orienteering,
                         height = unit(0.5, "cm"), width = unit(0.5, "cm")) +
  coord_sf(
    xlim = c(xmin2, xmax2),
    ylim = c(ymin2, ymax2),
    expand = FALSE
  ) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.margin = margin(0, 0, 0, 0))

# plot map
png(filename = "figures/study_site.png",
    width=8, height=5, units="in", res=600)

ggdraw(site_map) + draw_plot(inset, x = 0.565, y = 0.54, width = 0.30, height = 0.60)

dev.off()

#### MAP RESIGHTS ####

bandedbirds_2017 <- readRDS("data-files/ALLCAMPS2017_bandedbirds_resights.rds")
bandedbirds_2018 <- readRDS("data-files/ALLCAMPS2018_bandedbirds_resights.rds")

# 2017
map_data_2017 <- bandedbirds_2017 %>% 
  filter(is.na(ResightCertainty) | ResightCertainty > 94 | ResightCertainty == 100) %>% 
  filter(Species %in% "REKN") %>% 
  filter(!str_detect(FlagCode, "Q")) %>%
  select(LocationID, Latitude, Longitude, FlagColor, FlagCode) %>%
  fill(LocationID, Latitude, Longitude) %>%
  mutate(FlagID = paste(FlagColor, FlagCode, sep = "(")) %>%
  select(LocationID, Latitude, Longitude, FlagID) %>% 
  mutate_at(vars(Latitude, Longitude), as.numeric) %>% 
  mutate_if(is.numeric, round, 2)

map_data_2017$FlagID <- paste0(map_data_2017$FlagID, ")")

map_resights_2017 <- map_data_2017 %>% 
  count(Latitude, Longitude, sort = TRUE) %>% 
  rename(Resights = n) %>% 
  mutate(Year = 2017)

# 2018
map_data_2018 <- bandedbirds_2018 %>% 
  filter(is.na(ResightCertainty) | ResightCertainty > 94 | ResightCertainty == 100) %>% 
  filter(Species %in% "REKN") %>% 
  filter(!str_detect(FlagCode, "Q")) %>%
  select(LocationID, Latitude, Longitude, FlagColor, FlagCode) %>%
  fill(LocationID, Latitude, Longitude) %>%
  mutate(FlagID = paste(FlagColor, FlagCode, sep = "(")) %>%
  select(LocationID, Latitude, Longitude, FlagID) %>% 
  mutate_at(vars(Latitude, Longitude), as.numeric) %>% 
  mutate_if(is.numeric, round, 2)

map_data_2018$FlagID <- paste0(map_data_2018$FlagID, ")")

map_resights_2018 <- map_data_2018 %>% 
  count(Latitude, Longitude, sort = TRUE) %>% 
  rename(Resights = n) %>% 
  mutate(Year = 2018)

# combine dataframes
map_resights <- bind_rows(map_resights_2017, map_resights_2018)

map_resights <- map_resights %>% 
  filter(!Latitude == "51.36") %>% 
  filter(!Longitude == "-80.23")

camp_locs <- data.frame(long = c(-80.571, -80.692, -80.452),
                        lat = c(51.6557, 51.7989, 51.4844),
                        camp = c("Little    \nPiskwamish Point ",
                                 "Longridge Point", "Northbluff Point"))

# set up map
region3 <- ne_states(country = "Canada", returnclass = "sf") %>%
  filter(name == "Ontario")

xmin <- -81.3
xmax <- -80
ymin <- 51.1
ymax <- 52

# map resights
png(filename = paste0("figures/resights_map.png"),
    width=6, height=4, units="in", res=600)

ggplot(data = region3) +
  geom_sf(colour = NA) +
  geom_sf(data = lakes, fill = "white", colour = NA) +
  geom_point(data=map_resights, aes(x=Longitude, y=Latitude, size=Resights),
             color="black", alpha=0.2) +
  facet_wrap( ~ Year, ncol = 2) +
  scale_size_continuous(name = "Number of\nresightings", range = c(1, 15)) +
  geom_text(data = camp_locs, aes(x = long, y = lat, label = camp),
            hjust = 1.15, family = "Times") +
  annotation_scale(location = "br", line_width = 0.25,
                   text_cex = 0.5, height = unit(0.15, "cm")) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.65, "cm"), pad_y = unit(0.5, "cm"),
                         style = north_arrow_fancy_orienteering,
                         height = unit(0.75, "cm"), width = unit(0.75, "cm")) +
  coord_sf(
    xlim = c(xmin, xmax),
    ylim = c(ymin, ymax),
    expand = FALSE
  ) +
  theme(text = element_text(family = "Times"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position="bottom",
        legend.direction="horizontal",
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 14, face = "bold"),
        strip.background = element_rect(size = 0.5, fill = "transparent", colour = "black"),
        legend.key = element_blank(),
        legend.margin=margin(t=-0.5, r=0, b=0, l=0, unit="cm"),
        panel.border = element_rect(fill = NA, colour = "black"),
        panel.grid = element_blank())

dev.off()
