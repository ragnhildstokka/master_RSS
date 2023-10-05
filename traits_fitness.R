
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(devtools)
library(usethis)
library(LeafArea)

#importing traits
traits <- read_xlsx(path = "sp_va_traits.xlsx")

# adding LDMC to dataset
traits_clean <- traits %>%
  mutate(LDMC = dry_weight/wet_weight)

# adding SLA
# devtools::install_github("richardjtelford/LeafArea")

loop.files <-  function(files){
  
  file.copy(files, new.folder)
  if(grepl("-NA$", files)){
    newfile <- basename(files)
    file.rename(paste0(new.folder, "/", newfile), paste0(new.folder,
                                                         "/", gsub("-NA$", "", newfile)))
  }
  print(files)
  area <- try(run.ij(set.directory = new.folder, distance.pixel = 237, known.distance = 2, log = TRUE, low.size = 0.005, trim.pixel = 60, trim.pixel2 = 150, save.image = TRUE))
  # more cropping
  #area <- try(run.ij(set.directory = new.folder, distance.pixel = 237, known.distance = 2, log = TRUE, low.size = 0.005, trim.pixel = 200, trim.pixel2 = 0, save.image = TRUE))
  
  if(inherits(area, "try-error")){
    return(data.frame(LeafArea = NA))
  }
  file.copy(dir(new.folder, full.names = TRUE, pattern = "\\.tif"), output.folder)
  Sys.sleep(0.1)
  if(any(!file.remove(dir(new.folder, full.names = TRUE) ))) stop()
  res <- data.frame(dir = dirname(files), ID = names(unlist(area[[2]])), LeafArea = (unlist(area[[2]])))
  return(res)
}


list.of.files <- dir(path = paste0("data/leaves_sp_va/"), pattern = "jpeg|jpg", recursive = TRUE, full.names = TRUE)
new.folder <- "data/Temp/"
output.folder <- "data/output/"
LA <- plyr::ldply(list.of.files, loop.files)


# extract everything before point
leaf_area <- LA |>
  mutate(ID = sub("\\..*", "", ID)) |>
  group_by(dir, ID) |>
  summarise(n = n(),
            leaf_area = sum(LeafArea)) |>
  rename("sp_IDS" = "ID")
  
# adding leaf_area to dataset
traits_clean <- left_join(traits_clean, leaf_area, by = "sp_IDS")

#calculating SLA
traits_clean <- traits_clean %>%
  mutate(SLA = leaf_area/dry_weight)

### messing around
pl_1 <- ggplot(data = traits_clean, mapping = aes(SLA, LDMC)) +
  geom_point(aes(colour = species), size = 2)
show(pl_1)
