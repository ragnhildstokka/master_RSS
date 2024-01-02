
library(ggplot2)
library(dplyr)
library(tidyr)
library(lme4)
library(lmerTest)
library(gridExtra)
library(cowplot)
library(tidyverse)

traits <- read.csv(file =  "data/cleaned_sp_va_traits_2023.csv") %>% 
  mutate(precipitation = if_else(siteID == "Skj", 3402,
                                 if_else(siteID == "Gud", 2130,
                                         if_else(siteID == "Lav", 1561,
                                                 if_else(siteID =="Ulv", 1226,NA_real_))))) %>% 
  select(-X) %>% 
  mutate(blockID = paste0(siteID, "_", blockID)) #changing block colon to include siteID

theme_set(theme_light())
site_order <- c("Skj", "Gud", "Lav", "Ulv")
custom_color_palette <- c("1226" = "#BCE6FF", "1561" = "#88CDF6", "2130" = "#2D82B5", "3402" = "#015C92")

# seperate datasets
sib_pro <- traits %>% 
  filter(species == "sib_pro")
ver_alp <- traits %>% 
  filter(species == "ver_alp")

#importing fitness data, and making long and wide version of datasets
sib_pro_growth_wide <- read.csv(file =  "data/INCLINE_Sib_pro_growth_fec_2018_2023.csv") %>%
  filter(!(unique_IDS == "Gud_2_1_2")) %>%
  filter(!(unique_IDS == "Gud_2_4_8")) %>% 
  left_join(sib_pro, by = "unique_IDS") %>% 
  mutate(FecundityCategory = ifelse(average_fec_2018_2023 > 0, 1, 0)) %>% 
  select(unique_IDS, sp_IDS, precipitation, FecundityCategory, siteID.x, blockID.x, plotID.x, OTC, fec, total_fec_2018_2023, average_fec_2018_2023, SLA, height, LDMC, leaf_thickness, mean_growth_2018_2023, growth_2018_2023, growth_2018_2019, growth_2019_2020, growth_2020_2021, growth_2021_2022, growth_2022_2023) %>% 
  distinct(sp_IDS, .keep_all = TRUE) %>%  #removing rows that are duplicate
  rename("siteID" = siteID.x, "blockID" = blockID.x, "plotID" = plotID.x)

sib_pro_growth_long <- sib_pro_growth_wide %>% 
  pivot_longer(cols = c(height, SLA, LDMC, leaf_thickness), names_to = "trait", values_to = "trait_value") #DO THIIIISSS

ver_alp_growth_wide <- read.csv(file =  "data/INCLINE_Ver_alp_growth_fec_2018_2023.csv") %>% 
  left_join(ver_alp, by = "unique_IDS") %>% 
  filter(!(unique_IDS == "u_va_3_4_8")) %>% #only one thickness measurement, and no dry weight
  mutate(FecundityCategory = ifelse(average_fec_2018_2023 > 0, 1, 0)) %>% 
  select(unique_IDS, sp_IDS, precipitation, FecundityCategory, siteID.x, blockID.x, plotID.x, OTC, fec, total_fec_2018_2023, average_fec_2018_2023, SLA, height, LDMC, leaf_thickness, mean_growth_2018_2023, growth_2018_2023, growth_2018_2019, growth_2019_2020, growth_2020_2021, growth_2021_2022, growth_2022_2023) %>% 
  distinct(sp_IDS, .keep_all = TRUE) %>% #removing rows that are duplicate
  rename("siteID" = siteID.x, "blockID" = blockID.x, "plotID" = plotID.x)

ver_alp_growth_long <-ver_alp_growth_wide %>% 
  pivot_longer(cols = c(height, SLA, LDMC, leaf_thickness), names_to = "trait", values_to = "trait_value")


# Combine the datasets
combined_data_long <- bind_rows(
  mutate(sib_pro_growth_long, species = "S. procumbens"),
  mutate(ver_alp_growth_long, species = "V. alpina")) 
combined_data_wide <- bind_rows(
  mutate(sib_pro_growth_wide, species = "S. procumbens"),
  mutate(ver_alp_growth_wide, species = "V. alpina")
)

#histogram traits
ggplot(combined_data_wide, aes(x = height)) +
  geom_histogram() +
  facet_wrap(~species)+
  theme_light()
ggplot(combined_data_wide, aes(x = SLA)) + #should I remove SLA here
  geom_histogram() +
  facet_wrap(~species)+
  theme_light()
ggplot(combined_data_wide, aes(x = LDMC)) +
  geom_histogram() +
  facet_wrap(~species) +
  theme_light()
ggplot(combined_data_wide, aes(x = leaf_thickness)) +
  geom_histogram() +
  facet_wrap(~species) +
  theme_light()

# mean annual growth and traits
ggplot(combined_data_long, aes(x= mean_growth_2018_2023, y = trait_value)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(species + trait ~ .,scales = "free" , nrow = 2)

### JUST TRAITS

ggplot(data = subset(sib_pro_growth_long, !is.na(precipitation)), aes(x = precipitation, y = trait_value, color = OTC)) +
  geom_jitter(width = 50) +
  labs(title = expression(italic("S. procumbens") ~ "Trait Distribution With and Without Warming"),
       y = "Trait Value",
       x = "Precipitation (mm)") +
  facet_wrap(~trait, scales = "free_y",nrow = 2,
             labeller = labeller(trait = c("height" = "Height (mm)",
                                           "LDMC" = "LDMC (mg)",
                                           "leaf_thickness" = "Leaf Thickness (mm)",
                                           "SLA" = "SLA (mm2/mg)")))+
  scale_color_brewer(palette = "Dark2") +
  geom_smooth(method = "lm") +
  scale_x_continuous(breaks = unique(sib_pro_growth_long$precipitation), labels = unique(sib_pro_growth_long$precipitation))

ggplot(data = subset(ver_alp_growth_long, !is.na(precipitation)), aes(x = precipitation, y = trait_value, color = OTC)) +
  geom_jitter(width = 50) +
  labs(title = expression(italic("V. alpina") ~ "Trait Distribution With and Without Warming"),
       y = "Trait Value",
       x = "Precipitation (mm)") +
  facet_wrap(~trait, scales = "free_y",nrow = 2,
             labeller = labeller(trait = c("height" = "Height (mm)",
                                           "LDMC" = "LDMC (mg)",
                                           "leaf_thickness" = "Leaf Thickness (mm)",
                                           "SLA" = "SLA (mm2/mg)"))) +
  scale_color_brewer(palette = "Dark2") +
  geom_smooth(method = "lm") +
  scale_x_continuous(breaks = unique(ver_alp_growth_long$precipitation), labels = unique(ver_alp_growth_long$precipitation))


# not this, dosen't match traits of the two species
ggplot(data = subset(combined_data_long, !is.na(precipitation)), aes(x = precipitation, y = trait_value, color = OTC)) +
  geom_jitter(width = 50) +
  labs(title = "Trait Distribution With and Without Warming",
       y = "Trait Value",
       x = "Precipitation (mm)") +
  facet_wrap(species + trait ~ ., scales = "free_y", nrow = 2, 
             labeller = labeller(trait = c("height" = "Height (mm)",
                                           "LDMC" = "LDMC (mg)",
                                           "leaf_thickness" = "Leaf Thickness (mm)",
                                           "SLA" = "SLA (mm2/mg)"))) +
  scale_color_brewer(palette = "Dark2") +
  geom_smooth(method = "lm") +
  scale_x_continuous(breaks = unique(combined_data_long$precipitation), labels = unique(combined_data_long$precipitation))


height_pl <- ggplot(traits, mapping = aes(x = precipitation, y = height, color = treatment)) +
  geom_jitter(width = 70) +
  geom_smooth(method = "lm") +
  labs(x = "Species / Treatment",
       y = "Height") +
  scale_color_brewer(palette = "Dark2") +
  facet_grid(. ~ species) +
  theme(legend.position = "none")
SLA_pl <- ggplot(traits, mapping = aes(x = precipitation, y = LDMC, color = treatment)) +
  geom_jitter(width = 70) +
  geom_smooth(method = "lm") +
  labs(x = "Species / Treatment",
       y = "LDMC") +
  scale_color_brewer(palette = "Dark2") +
  facet_grid(. ~ species) +
  theme(legend.position = "none")
LDMC_pl <- ggplot(traits, mapping = aes(x = precipitation, y = SLA, color = treatment)) +
  geom_jitter(width = 70) +
  geom_smooth(method = "lm") +
  labs(x = "Species / Treatment",
       y = "SLA") +
  scale_color_brewer(palette = "Dark2") +
  facet_grid(. ~ species) +
  theme(legend.position = "none")
LT_pl <- ggplot(traits, mapping = aes(x = precipitation, y = leaf_thickness, color = treatment)) +
  geom_jitter(width = 70) +
  geom_smooth(method = "lm") +
  labs(x = "Species / Treatment",
       y = "Leaf Thickness",) +
  scale_color_brewer(palette = "Dark2") +
  facet_grid(. ~ species) +
  theme(legend.position = "none")
plot_grid(height_pl, SLA_pl,LDMC_pl, LT_pl, ncol = 2) #combining the plots

#testing models

sp_traits <- lmer(height ~ scale(precipitation) * treatment + (1|blockID), data = sib_pro)
summary(sp_traits) # testing with height, SLA, leaf_thickness and LDMC

va_traits <- lmer(height ~ scale(precipitation) * treatment + (1|blockID), data = ver_alp) 
summary(va_traits) # testing with height, SLA, leaf_thickness and LDMC


# without precipitation gradient
#SLA
ggplot(traits, aes(x = treatment, y = SLA, fill = treatment)) +
  geom_boxplot() +
  labs(title = "SLA Distribution by Treatment and Species (All Sites)",
       x = "Species / Treatment",
       y = "SLA") +
  facet_grid(. ~ species)

# Height
ggplot(traits, aes(x = treatment, y = height, fill = treatment)) +
  geom_boxplot() +
  labs(title = "Height Distribution by Treatment and Species (All Sites)",
       x = "Species / Treatment",
       y = "Height") +
  facet_grid(. ~ species)
# LDMC
ggplot(traits, aes(x = treatment, y = LDMC, fill = treatment)) +
  geom_boxplot() +
  labs(title = "LDMC Distribution by Treatment and Species (All Sites)",
       x = "Species / Treatment",
       y = "LDMC") +
  facet_grid(. ~ species)

# Leaf thickness
ggplot(traits, aes(x = treatment, y = leaf_thickness, fill = treatment)) +
  geom_boxplot() +
  labs(title = "Leaf Thickness Distribution by Treatment and Species (All Sites)",
       x = "Species / Treatment",
       y = "Leaf thickness") +
  facet_grid(. ~ species)



#FECUNDITY

#SIB PRO
# fecundity with precipitation
ggplot(data = subset(sib_pro_growth_long, !is.na(precipitation)), aes(x = precipitation, y = trait_value, fill = FecundityCategory)) +
  geom_point(position = position_dodge(width = 100), aes(color = FecundityCategory)) +
  #geom_smooth(method = "lm", aes(group = FecundityCategory, color = FecundityCategory), position = position_dodge(width = 20)) +
  labs(title = expression(italic("S. procumbens") ~ "Traits by Fecundity and Precipitation"),
       x = "Precipitation (mm)",
       y = "Trait Value") +
  #scale_fill_manual(values = c(1 = "palevioletred2", 0 = "palegreen3")) +
  scale_color_manual(values = c(1 == "palevioletred2", 0 == "palegreen3")) +
  facet_wrap(~trait, scales = "free_y", nrow = 2,
             labeller = labeller(trait = c("height" = "Height (mm)",
                                           "LDMC" = "LDMC (mg)",
                                           "leaf_thickness" = "Leaf Thickness (mm)",
                                           "SLA" = "SLA (mm2/mg)"))) +
  scale_x_continuous(breaks = unique(ver_alp_growth_long$precipitation), labels = unique(ver_alp_growth_long$precipitation))

# Create a violin plot with numeric FecundityCategory values
ggplot(sib_pro_growth_long, aes(x = as.factor(FecundityCategory), y = trait_value, fill = as.factor(FecundityCategory))) +
  geom_violin() +
  labs(
    title = "Trait Values by Fecundity Category",
    x = "Fecundity Category",
    y = "Trait Value"
  ) +
  scale_x_discrete(labels = c("0" = "Nonflowering", "1" = "Flowering")) +
  scale_fill_manual(values = c("0" = "palegreen3", "1" = "palevioletred2")) +
  facet_wrap(~trait, scales = "free_y", nrow = 2,
             labeller = labeller(trait = c("height" = "Height (mm)",
                                           "LDMC" = "LDMC (mg)",
                                           "leaf_thickness" = "Leaf Thickness (mm)",
                                           "SLA" = "SLA (mm2/mg)"))) +
  theme_minimal()


# Create a violin plot... without precipitation
fec_height <- ggplot(combined_data_wide, aes(x = as.factor(FecundityCategory), y = height)) +
  geom_violin() +
  labs(x = "Fecundity Category", 
       y = "Height") +
  facet_wrap(~species)+
  geom_point(aes(fill = as.factor(FecundityCategory), color = as.factor(FecundityCategory)),
             alpha = 0.5, 
             position = position_jitter(height = 0.5, width = 0.2)) +
  geom_boxplot(alpha = 0, 
               width=0.2) +
  theme_light() +
  theme(legend.position = "none")
fec_LDMC <- ggplot(combined_data_wide, aes(x = as.factor(FecundityCategory), y = LDMC)) +
  geom_violin() +
  labs(x = "Fecundity Category",
       y = "LDMC") +
  facet_wrap(~species)+
  geom_point(aes(fill = as.factor(FecundityCategory), color = as.factor(FecundityCategory)),
             alpha = 0.5, 
             position = position_jitter(height = 0.5, width = 0.2)) +
  geom_boxplot(alpha = 0, 
               width=0.2) +
  theme_light() +
  theme(legend.position = "none")
fec_SLA <- ggplot(combined_data_wide, aes(x = as.factor(FecundityCategory), y = SLA)) +
  geom_violin() +
  labs(x = "Fecundity Category",
       y = "SLA") +
  facet_wrap(~species)+
  geom_point(aes(fill = as.factor(FecundityCategory), color = as.factor(FecundityCategory)),
             alpha = 0.5, 
             position = position_jitter(height = 0.5, width = 0.2)) +
  geom_boxplot(alpha = 0, 
               width=0.2) +
  theme_light() +
  theme(legend.position = "none")
fec_LT <- ggplot(combined_data_wide, aes(x = as.factor(FecundityCategory), y = leaf_thickness)) +
  geom_violin() +
  labs(x = "Fecundity Category",
       y = "Leaf Thickness") +
  facet_wrap(~species)+
  geom_point(aes(fill = as.factor(FecundityCategory), color = as.factor(FecundityCategory)),
             alpha = 0.5, 
             position = position_jitter(height = 0.5, width = 0.2)) +
  geom_boxplot(alpha = 0, 
               width=0.2) +
  theme_light() +
  theme(legend.position = "none")
plot_grid(fec_height, fec_SLA,fec_LDMC, fec_LT, ncol = 2) #combining the plots


#VER ALP
# fecundity with precipitation variability
ggplot(data = subset(ver_alp_growth_long, !is.na(precipitation)), aes(x = precipitation, y = trait_value, fill = factor(FecundityCategory))) +
  geom_point(position = position_dodge(width = 100), aes(color = factor(FecundityCategory))) +
  geom_smooth(method = "lm", aes(group = factor(FecundityCategory), color = factor(FecundityCategory)), position = position_dodge(width = 20)) +
  labs(title = expression(italic("V. alpina") ~ "Traits by Fecundity and Precipitation"),
       x = "Precipitation (mm)",
       y = "Trait Value") +
  scale_fill_manual(values = c("1" = "palevioletred2", "0" = "palegreen3")) +
  scale_color_manual(values = c("1" = "palevioletred2", "0" = "palegreen3")) +
  facet_wrap(~trait, scales = "free_y", nrow = 2,
             labeller = labeller(trait = c("height" = "Height (mm)",
                                           "LDMC" = "LDMC (mg)",
                                           "leaf_thickness" = "Leaf Thickness (mm)",
                                           "SLA" = "SLA (mm2/mg)"))) +
  scale_x_continuous(breaks = unique(ver_alp_growth_long$precipitation), labels = unique(ver_alp_growth_long$precipitation))

#Create a violin plot...without precipitation 
ggplot(ver_alp_growth_long, aes(x = factor(FecundityCategory), y = trait_value)) +
  geom_violin() +
  labs(title = expression(italic("V. alpina") ~ "Traits by Fecundity"),
       y = "Trait Value",
       x = "Fecundity Category") +
  facet_wrap(~trait, scales = "free_y", nrow = 2,
             labeller = labeller(trait = c("height" = "Height (mm)",
                                           "LDMC" = "LDMC (mg)",
                                           "leaf_thickness" = "Leaf Thickness (mm)",
                                           "SLA" = "SLA (mm2/mg)"))) +
  geom_point(aes(fill = factor(FecundityCategory), color = factor(FecundityCategory)),
             alpha = 0.7, 
             position = position_jitter(height = 0.5, width = 0.3)) +
  geom_boxplot(alpha = 0, 
               width=0.2) +
  theme_light()


#Binomial test fecundity

fec_sp_height_1 <- glmer(FecundityCategory ~ scale(height) * scale(precipitation) * 
                           OTC + (1|blockID) , data = sib_pro_growth_wide, family = binomial())
summary(fec_sp_height_1) 
fec_sp_height_2 <- glmer(FecundityCategory ~ scale(height) + scale(precipitation) + 
                           OTC +
                           scale(height):scale(precipitation) +
                           scale(height):OTC +
                           scale(precipitation):OTC +
                           (1|blockID), data = sib_pro_growth_wide, family = binomial())
summary(fec_sp_height_2)
anova(fec_sp_height_1, fec_sp_height_2)
# fec_sp_height_2 has a better fit than 1 and 3 (removed scale(height):OTC))

fec_sp_LDMC_1 <- glmer(FecundityCategory ~ scale(LDMC, scale = FALSE) * scale(precipitation) * 
                         OTC + (1|blockID), data = sib_pro_growth_wide, family = binomial())
summary(fec_sp_LDMC_1)

fec_sp_LDMC_2 <- glmer(FecundityCategory ~ scale(LDMC, scale = FALSE) + scale(precipitation) + 
                         OTC +
                         scale(height):scale(precipitation) +
                         scale(height):OTC +
                         scale(precipitation):OTC +
                         (1|blockID), data = sib_pro_growth_wide, family = binomial())
summary(fec_sp_LDMC_2)
anova(fec_sp_LDMC_1, fec_sp_LDMC_2)

fec_sp_SLA_1 <- glmer(FecundityCategory ~ scale(SLA) * scale(precipitation) * 
                         OTC + (1|blockID), data = sib_pro_growth_wide, family = binomial())
summary(fec_sp_SLA_1)

fec_sp_LT_1 <- glmer(FecundityCategory ~ scale(leaf_thickness, scale = FALSE) * scale(precipitation) * OTC + (1|blockID), data = sib_pro_growth_wide, family = binomial()) # scale = false --> sentrerer kun, skalerer ikke.
summary(fec_sp_LT_1)

# other analysis
# glmer( height  ~ precipitation * OTC , data = sib_pro_growth_wide, family = gamma()) kun for traitsene, hvis gaussian ikke går fint



# MEAN ANNUAL GROWTH

#sib pro
#Mean annual growth and traits, with and without warming 
ggplot(sib_pro_growth_long, aes(x = mean_growth_2018_2023, y = trait_value, color = OTC)) +
  geom_point() +
  labs(title = expression(italic("S. procumbens") ~ "Trait Distribution by Mean Annual Growth in 2023 With and Without Warming"),
       x = "Mean Annual Growth",
       y = "Trait Value") +
  geom_smooth(method = "lm") +
  scale_color_brewer(palette = "Dark2") +
  facet_wrap(~trait, scales = "free_y",
             labeller = labeller(trait = c("height" = "Height (mm)",
                                           "LDMC" = "LDMC (mg)",
                                           "leaf_thickness" = "Leaf Thickness (mm)",
                                           "SLA" = "SLA (mm2/mg)")))

# various yearly growth
ggplot(sib_pro_growth_long, aes(x = growth_2022_2023, y = trait_value, color = OTC)) +
  geom_point() +
  labs(title = expression(italic("S. procumbens") ~ "Trait Distribution by Mean Annual Growth in 2023 With and Without Warming"),
       x = "Mean Annual Growth",
       y = "Trait Value") +
  geom_smooth(method = "lm") +
  scale_color_brewer(palette = "Dark2") +
  facet_wrap(~trait, scales = "free_y",
             labeller = labeller(trait = c("height" = "Height (mm)",
                                           "LDMC" = "LDMC (mg)",
                                           "leaf_thickness" = "Leaf Thickness (mm)",
                                           "SLA" = "SLA (mm2/mg)")))


#Biomass and traits, precipitation. one NA because Gudmedalen was removed here (only two individuals)

ggplot(sib_pro_growth_long, aes(x = mean_growth_2018_2023, y = trait_value, color = as.factor(precipitation))) +
  geom_point() +
  labs(title = expression(italic("S. procumbens") ~ "Trait Distribution by Mean Annual Growth in 2023 With and Without Warming"),
       x = "Mean Annual Growth",
       y = "Trait Value") +
  geom_smooth(method = "lm") +
  scale_color_manual(values = custom_color_palette) +
  facet_wrap(~trait, scales = "free_y",nrow = 2,
             labeller = labeller(trait = c("height" = "Height (mm)",
                                           "LDMC" = "LDMC (mg)",
                                           "leaf_thickness" = "Leaf Thickness (mm)",
                                           "SLA" = "SLA (mm2/mg)")))


# analysis, using wider dataset
sp_growth <- lmer(mean_growth_2018_2023 ~ scale(leaf_thickness) * scale(precipitation) * OTC + (1|blockID), data = sib_pro_growth_wide)
summary(sp_growth)

sp_growth.2 <- update(sp_growth, -scale(leaf_thickness):scale(precipitation):OTC)
  
  
sp_growth.2 <- lmer(mean_growth_2018_2023 ~ leaf_thickness + scale(precipitation) + OTC +
                        leaf_thickness:scale(precipitation) +
                        leaf_thickness:OTC +
                        scale(precipitation):OTC +
                      (1|blockID.x), data = sib_pro_growth_wide)
summary(sp_growth.2)
anova(sp_growth, sp_growth.2)

# testing with height, SLA, leaf_thickness and LDMC

#both species
ggplot(combined_data_long, aes(x =  mean_growth_2018_2023, y = trait_value)) +
  geom_point() +
  labs(title = "Trait Distribution by Mean Annual Growth With and Without Warming",
       x = "Mean Annual Growth",
       y = "Trait Value") +
  geom_smooth(method = "lm") +
  scale_color_brewer(palette = "Dark2") +
  facet_wrap(species~trait, scales = "free_y", nrow = 2,
             labeller = labeller(trait = c("height" = "Height (mm)",
                                           "LDMC" = "LDMC (mg)",
                                           "leaf_thickness" = "Leaf Thickness (mm)",
                                           "SLA" = "SLA (mm2/mg)")))

# ver alp
ggplot(ver_alp_growth_long, aes(x = mean_growth_2018_2023, y = trait_value, color = OTC)) +
  geom_point() +
  labs(title = expression(italic("V. alpina") ~ "Trait Distribution by Mean Annual Growth in 2023 With and Without Warming"),
       x = "Mean Annual Growth",
       y = "Trait Value") +
  geom_smooth(method = "lm") +
  scale_color_brewer(palette = "Dark2") +
  facet_wrap(~trait, scales = "free_y",
             labeller = labeller(trait = c("height" = "Height (mm)",
                                           "LDMC" = "LDMC (mg)",
                                           "leaf_thickness" = "Leaf Thickness (mm)",
                                           "SLA" = "SLA (mm2/mg)")))
va_growth <- lmer(mean_growth_2018_2023 ~ height * scale(precipitation) * OTC + (1|blockID.x), data = ver_alp_growth_wide)
summary(va_growth)

# mean annual growth and precipitation
ggplot(combined_data_long, mapping = aes(x = precipitation, y = mean_growth_2018_2023, color = OTC)) +
  geom_point(position = position_dodge(width = 100), aes(color = OTC)) +
  geom_smooth(method = "lm") +
  labs(x = "Precipitation",
       y = "Mean Annual Growth") +
  scale_color_brewer(palette = "Dark2") +
  facet_grid(. ~ species)
va_just_growth <- lmer(mean_growth_2018_2023 ~ scale(precipitation) * OTC + (1|blockID.x), data = ver_alp_growth_wide)
summary(va_just_growth)

