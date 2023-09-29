#### Libraries ####

library(maps)
library(mapdata)
library(ggmap)
library(grid)
library(patchwork)
library(ggplot2)
library(dplyr) 
library(gghighlight)


#### Color palette ####
Precip_palette <- c("#BAD8F7", "#89B7E1", "#2E75B6", "#213964")

#Making table with coordinates for the sites

dat <- read.table(header = TRUE, text = "
siteID           Latitude Longitude Temperature Precipitation
Gudmedalen       60.8328    7.17561           1             3
Lavisdalen       60.8231    7.27596           1             2
Skjellingahaugen 60.9335    6.41504           1             4
Ulvhaugen        61.0243    8.12343           1             1
")

dat <- dat |>
  mutate(Precipitation = as.factor(Precipitation))

precipLab <- c("Driest", "Dry", "Wet", "Wettest")

xlim <- range(dat$Longitude) + c(-0.9, 0.5)
ylim <- range(dat$Latitude) + c(-0.4, 0.5)

#Get maps
norwaymap <- map_data("world", "Norway")
norwaymapHires <- map_data("worldHires", "Norway")


Scandinaviamap <- map_data("world", c("Norway", "Sweden", "Finland"))
norwaymapHires <- map_data("worldHires", "Norway")


Norway_map <- ggplot() +
  geom_map(data = Scandinaviamap, aes(x = long, y = lat, map_id = region, alpha = 0.2),
           map = Scandinaviamap, colour = "black", fill = "grey60") +
  geom_rect(data = data.frame(),
            aes(xmin = xlim[1], xmax = xlim[2], ymin = ylim[1], ymax = ylim[2]),
            colour = "black", fill = "#76B4FF", alpha = 0.5) +
  coord_map(xlim = c(4, 32), ylim = c(55, 71)) +
  labs(x = NULL, y = NULL) +
  guides(alpha = FALSE) +
  theme(
    axis.text = element_text(size = 9),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(fill = NA, colour = "black"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    legend.position="bottom")

maptheme <- function(...) {
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.position = "bottom",
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(fill = NA, colour = "black"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    ...
  )
}

Zoomed_in_map <- ggplot(dat, aes(x = Longitude, y = Latitude, fill = Precipitation)) +
  geom_map(aes(x = long, y = lat, map_id = region), data = norwaymapHires, map = norwaymapHires, color = NA, fill = "grey70") +
  geom_point(size = 2.5) +
  coord_map(xlim = xlim, ylim = ylim) +
  guides(fill = guide_legend(override.aes = list(shape = 21))) +
  scale_fill_manual(values = Precip_palette, labels = precipLab, aesthetics = "fill", breaks = 1:4) +
  scale_shape_manual(values = c(24, 21, 25), labels = tempLab, breaks = 1:3) +
  maptheme()
show(Zoomed_in_map)

plot <- Zoomed_in_map +
  inset_element(Norway_map, 0.6, 0.6, 1, 1, align_to = "full") +
  plot_layout(guides = 'collect', widths = 1, heights = 1) 

show(plot)

#ggsave(plot = plot, "SeedClim_climate_grid2.pdf", width = 34, height = 22, units = "cm")

