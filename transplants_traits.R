library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(devtools)
library(usethis)
library(LeafArea)

#importing traits
transplants_traits <- read_xlsx(path = "data/transplants_traits.xlsx")

# adding LDMC to dataset
transplants_traits_clean <- transplants_traits %>%
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
  area <- try(run.ij(set.directory = new.folder, distance.pixel = round(600/2.54), known.distance = 1, log = TRUE, low.size = 0.000, trim.pixel = 15, save.image = TRUE))
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


#scanning, 
list.of.files <- dir(path = paste0("data/leaves_transplants/"), pattern = "jpeg|jpg", recursive = TRUE, full.names = TRUE)
new.folder <- "data/Temp/"
output.folder <- "data/transplants_output/"
translants_scans <- plyr::ldply(list.of.files, loop.files)


# extract everything before point

leaf_area_transplants <- translants_scans |>
  mutate(ID = sub("\\..*", "", ID)) |>
  group_by(dir, ID) |>
  summarise(n = n(),
            leaf_area = sum(LeafArea)) |>
  rename("unique_IDS" = "ID")


# adding leaf_area to dataset

transplants_traits_clean <- left_join(transplants_traits_clean, leaf_area_transplants, by = "unique_IDS")

#calculating SLA
transplants_traits_clean <- transplants_traits_clean %>%
  mutate(SLA = leaf_area/dry_weight)

# 10 SLA values are missing, don't know why...

### messing around

ggplot(data = traits_clean, mapping = aes(leaf_area, dry_weight, label = sp_IDS)) +
  geom_point(aes(colour = siteID), size = 2)
#geom_point(aes(colour = siteID), size = 2)
#outlier here s_sp_4_2_11 is probably a juvenile

ggplot(data = traits_clean, mapping = aes(leaf_area, LDMC, label = sp_IDS)) +
  geom_point(aes(colour = siteID), size = 2)
#looks good

ggplot(data = traits_clean, mapping = aes(height, leaf_thickness, label = sp_IDS)) +
  geom_point(aes(colour = siteID), size = 2)
#looks fine

ggplot(data= traits_clean, mapping = aes(height, SLA))+
  geom_point(aes(colour = species), size = 2)


write.csv(transplants_traits_clean, file = "data/transplants_traits_2023.csv")
