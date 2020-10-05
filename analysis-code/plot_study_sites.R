library(ggmap)
library(ggsn)
library(tidyverse)
library(cowplot)

#### BLACK AND WHITE FOR THESIS ####

windowsFonts(Times=windowsFont("TT Times New Roman"))

# create dataframe with camp locations
camp_locs <- data.frame(long = c(-80.571, -80.692, -80.452),
                        lat = c(51.6557, 51.7989, 51.4844),
                        camp = c("Little Piskwamish Point", "Longridge Point", "Northbluff Point"))

# download basemap for study sites
map <- get_map(location = c(-82.7, 51.1, -78.1, 53.5), source = "stamen", maptype = "toner-lite")

# create bounding box df (for scale bar and north arrow)
bb <- attr(map, "bb")
bb2 <- data.frame(long = unlist(bb[c(2, 4)]), lat = unlist(bb[c(1,3)]))

bbx <- c(left = -82.7, bottom = 51.1, right = -78.1, top = 53.5)
x <- c(bbx["left"], bbx["left"], bbx["right"], bbx["right"])
y <- c(bbx["bottom"], bbx["top"], bbx["top"], bbx["bottom"])
bbx_df <- data.frame(x, y)


studysites_map <- ggmap(map) +
  geom_point(data=camp_locs, aes(x=long, y=lat), color="black", size = 4) +
  geom_text(data = camp_locs, aes(x = long, y = lat, label = camp),
            family = "Times", hjust = 1.1, size = 5) +
  annotate("text", label = "Akimiski\nIsland", family = "Times", x = -81.4, y = 53, size = 3) +
  scalebar(data = bb2, dist = 20, dist_unit = "km", transform = TRUE, model = "WGS84", 
           location = "bottomright", st.size = 3, box.fill = alpha("white", alpha = 0),
           box.color = "black", border.size = 1,
           anchor = c(x = -78.3, y = 51.3)) +
  north(data = bb2,
        location = "topleft", symbol = 10) +
  theme(text = element_text(family = "Times"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black"),
        #plot.margin = unit(c(0, 0, -1, -1), 'lines'),
        legend.position = "none")


# download basemap for inset map
map2 <- get_map(location = c(-97, 41, -60, 57), source = "stamen", maptype = "toner-background")

bb3 <- attr(map2, "bb")
bb4 <- data.frame(long = unlist(bb3[c(2, 4)]), lat = unlist(bb3[c(1,3)]))

inset_map <- ggmap(map2) +
  geom_polygon(aes(x = x, y = y),
               data = bbx_df, colour = "grey50", fill = NA, size = 1) +
  geom_point(aes(x = -63.9, y = 50.2), colour = "grey50", size = 4) +
  geom_label(aes(x = -63.9, y = 52, label = "Mingan\nArchipelago"), family = "Times",  size = 3, fill = "white", label.size = 0) +
  annotate("text", label = "James\nBay", family = "Times", x = -84.5, y = 52.3, size = 3) +
  scalebar(data = bb4, dist = 200, dist_unit = "km", transform = TRUE, model = "WGS84", 
           location = "bottomright", height = 0.03, st.dist = 0.03, st.size = 2, st.color = "white", box.fill = alpha("white", alpha = 0),
           box.color = "white", border.size = 0.5,
           anchor = c(x = -61.5, y = 42.3)) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black"),
        plot.margin = margin(0, 0, 0, 0))

# save and export plots
jpeg(filename = "figures/complete_map.jpg",
    width=6, height=5, units="in", res=600)

ggdraw(studysites_map) + draw_plot(inset_map, x = 0.45, y = 0.545, width = 0.52, height = 0.455)

dev.off()


ggsave(plot = studysites_map, "figures/REKN_studysites_bw.jpg", dpi = 600)
ggsave(plot = inset_map, "figures/REKN_inset_bw.jpg", dpi = 600)


#### COLOUR FOR PRESENTATIONS ####

# create dataframe with camp locations
camp_locs <- data.frame(long = c(-80.571, -80.692, -80.452),
                        lat = c(51.6557, 51.7989, 51.4844),
                        camp = c("Piskwamish", "Longridge", "Northbluff"))

# download basemap for study sites
map <- get_map(location = c(-82.7, 51.1, -78.1, 53.5), source = "stamen", maptype = "terrain")

# create bounding box df (for scale bar and north arrow)
bb <- attr(map, "bb")
bb2 <- data.frame(long = unlist(bb[c(2, 4)]), lat = unlist(bb[c(1,3)]))

bbx <- c(left = -82.7, bottom = 51.1, right = -78.1, top = 53.5)
x <- c(bbx["left"], bbx["left"], bbx["right"], bbx["right"])
y <- c(bbx["bottom"], bbx["top"], bbx["top"], bbx["bottom"])
bbx_df <- data.frame(x, y)


studysites_map <- ggmap(map) +
  geom_point(data=camp_locs, aes(x=long, y=lat), color="black", size = 5) +
  geom_text(data = camp_locs, aes(x = long, y = lat, label = camp), hjust = 1.3, size = 5) +
  scalebar(data = bb2, dist = 20, dist_unit = "km", transform = TRUE, model = "WGS84", 
           location = "bottomright", st.size = 3, box.fill = alpha("white", alpha = 0),
           box.color = "black", border.size = 1,
           anchor = c(x = -78.3, y = 51.3)) +
  north(data = bb2,
        location = "topleft", symbol = 10) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black"),
        #plot.margin = unit(c(0, 0, -1, -1), 'lines'),
        legend.position = "none")


# download basemap for inset map
map2 <- get_map(location = c(-97, 41, -72, 57), source = "stamen", maptype = "terrain")

inset_map <- ggmap(map2) +
  geom_polygon(aes(x = x, y = y),
               data = bbx_df, colour = "black", fill = NA, size = 2) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black"))
        #plot.margin = unit(c(0, 0, -1, -1), 'lines'))

png(filename = "figures/REKN_studysites.png")
#grid.newpage()
vp_b <- viewport(width = 1, height = 1, x = 0.5, y = 0.5)  # the larger map
vp_a <- viewport(width = 0.4, height = 0.4, x = 0.795, y = 0.8)  # the inset in upper left
print(studysites_map, vp = vp_b)
print(inset_map, vp = vp_a)
dev.off()

# save and export plots
ggsave(plot = studysites_map, "REKN_studysites.jpg")
ggsave(plot = inset_map, "REKN_inset.jpg")

#### modify north function so it works ####

north <- function(data = NULL, location = 'topright', scale = 0.1, symbol = 1, x.min, x.max, y.min, y.max, anchor = NULL) {
  if (is.null(data)) {
    if (is.null(x.min) | is.null(x.max) |
        is.null(y.min) | is.null(y.max) ) {
      stop('If data is not defined, x.min, x.max, y.min and y.max must be.')
    }
    data <- data.frame(long = c(x.min, x.max), lat = c(y.min, y.max))
  }
  if (any(class(data) %in% "sf")) {
    xmin <- sf::st_bbox(data)["xmin"]
    xmax <- sf::st_bbox(data)["xmax"]
    ymin <- sf::st_bbox(data)["ymin"]
    ymax <- sf::st_bbox(data)["ymax"]
    scale.x <- (xmax - xmin) * scale
    scale.y <- (ymax - ymin) * scale
  } else {
    xmin <- min(data$long)
    xmax <- max(data$long)
    ymin <- min(data$lat)
    ymax <- max(data$lat)
    scale.x <- (xmax - xmin) * scale
    scale.y <- (ymax - ymin) * scale
  }
  if (location == 'bottomleft') {
    if (is.null(anchor)) {
      x.min <- xmin
      y.min <- ymin
    } else {
      x.min <- anchor['x']
      y.min <- anchor['y']
    }
    x.max <- x.min + scale.x
    y.max <- y.min + scale.y
  }
  if (location == 'bottomright') {
    if (is.null(anchor)) {
      x.max <- xmax
      y.min <- ymin
    } else {
      x.max <- anchor['x']
      y.min <- anchor['y']
    }
    x.min <- x.max - scale.x
    y.max <- y.min + scale.y
  }
  if (location == 'topleft') {
    if (is.null(anchor)) {
      x.min <- xmin
      y.max <- ymax
    } else {
      x.min <- anchor['x']
      y.max <- anchor['y']
    }
    x.max <- x.min + scale.x
    y.min <- y.max - scale.y
  }
  if (location == 'topright') {
    if (is.null(anchor)) {
      x.max <- xmax
      y.max <- ymax
    } else {
      x.max <- anchor['x']
      y.max <- anchor['y']
    }
    x.min <- x.max - scale.x
    y.min <- y.max - scale.y
  }
  symbol <- sprintf("%02.f", symbol)
  symbol <- png::readPNG(paste0(system.file('symbols', package = 'ggsn'),
                                '/', symbol, '.png'))
  symbol <- grid::rasterGrob(symbol, interpolate = TRUE)
  return(inset(symbol, xmin = x.min, xmax = x.max, # changed annotation_custom to inset
               ymin = y.min, 
               ymax = y.max))
}
