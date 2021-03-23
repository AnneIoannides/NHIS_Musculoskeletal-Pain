#Anne Elizabeth Ioannides

#For the qualification of PhD (Physiology), University of the Witwatersrand, South Africa

#National Health Interview Survey (NHIS)

#This script is specifically to create choropleth maps of demographic distributions (age and sex) in the US by region, per year. It serves as an anchor for my results. 
#For convenience, I have uploaded the clean design objects that I used onto Dropbox so that I could easily create the figures using one script
#Although I have created these demographic maps for all years for which spatial data was analysed (1997 until 2018 except 2004), only years 1997, 2001, 2005, 2009, 2013, and 2017 are included within my thesis chapter; the rest are available on GitHub

#Packages
library(haven)
library(foreign)
library(tidyverse)
library(survey)
library(gdata)
library(ggplot2)
library(ggthemes)
library(sf)
library(sp)
library(spdep)

#Pull in the clean design objects

#Pull in clean design objects from Dropbox (generated from the cleaning script)
NHIS97_DO <- readRDS(url("https://www.dropbox.com/s/8grz5f017sb7yi0/NHIS97_DO.rds?dl=1", "rb"))
NHIS98_DO <- readRDS(url("https://www.dropbox.com/s/uai3ekyi4tlp7qh/NHIS98_DO.rds?dl=1", "rb"))
NHIS99_DO <- readRDS(url("https://www.dropbox.com/s/5jy8pbcomgzsb0g/NHIS99_DO.rds?dl=1", "rb"))
NHIS00_DO <- readRDS(url("https://www.dropbox.com/s/9lzdaljy42zwheg/NHIS00_DO.rds?dl=1", "rb"))
NHIS01_DO <- readRDS(url("https://www.dropbox.com/s/z3f2vr2j3e0eyrj/NHIS01_DO.rds?dl=1", "rb"))
NHIS02_DO <- readRDS(url("https://www.dropbox.com/s/a8c42ubsclpv7gi/NHIS02_DO.rds?dl=1", "rb"))
NHIS03_DO <- readRDS(url("https://www.dropbox.com/s/7eubymv8s71ryvn/NHIS03_DO.rds?dl=1", "rb"))
#no 2004
NHIS05_DO <- readRDS(url("https://www.dropbox.com/s/mtjdkrwhragzw2w/NHIS05_DO.rds?dl=1", "rb"))
NHIS06_DO <- readRDS(url("https://www.dropbox.com/s/u5g2lmuacf66exj/NHIS06_DO.rds?dl=1", "rb"))
NHIS07_DO <- readRDS(url("https://www.dropbox.com/s/i6y8pxl2jarzi3a/NHIS07_DO.rds?dl=1", "rb"))
NHIS08_DO <- readRDS(url("https://www.dropbox.com/s/mk6ra7suoqzqa6g/NHIS08_DO.rds?dl=1", "rb"))
NHIS09_DO <- readRDS(url("https://www.dropbox.com/s/rhqmownxr6n3mc9/NHIS09_DO.rds?dl=1", "rb"))
NHIS10_DO <- readRDS(url("https://www.dropbox.com/s/90km4k74bikub5r/NHIS10_DO.rds?dl=1", "rb"))
NHIS11_DO <- readRDS(url("https://www.dropbox.com/s/vec9zwu99u86qt3/NHIS11_DO.rds?dl=1", "rb"))
NHIS12_DO <- readRDS(url("https://www.dropbox.com/s/vf934xig2wam4g4/NHIS12_DO.rds?dl=1", "rb"))
NHIS13_DO <- readRDS(url("https://www.dropbox.com/s/4436ytfzrl34cza/NHIS13_DO.rds?dl=1", "rb"))
NHIS14_DO <- readRDS(url("https://www.dropbox.com/s/mqijhxdnm1ig0ou/NHIS14_DO.rds?dl=1", "rb"))
NHIS15_DO <- readRDS(url("https://www.dropbox.com/s/i9c52a5p3ow75de/NHIS15_DO.rds?dl=1", "rb"))
NHIS16_DO <- readRDS(url("https://www.dropbox.com/s/ep921ulg22spb22/NHIS16_DO.rds?dl=1", "rb"))
NHIS17_DO <- readRDS(url("https://www.dropbox.com/s/gjiqu3hr0t5ygsr/NHIS17_DO.rds?dl=1", "rb"))
NHIS18_DO <- readRDS(url("https://www.dropbox.com/s/c9zen5kd8k6bavj/NHIS18_DO.rds?dl=1", "rb"))


#I downloaded the shapefile from the US census bureau website, link: https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_region_500k.zip . However, due to http status not being modified, I had to download manually, and read the file in from my PC.

# *****IMPORTANT***** : When running this code, please remove my file path and enter yours after downloading the file from the link above. 

#The shape file folder and co is also available on my DropBox: https://www.dropbox.com/sh/rksa1sc3r2bjd2q/AACqQrmOqYFL5Mrn8rLTNQB6a?dl=0


#Pull shape file in
regions <- read_sf("C:\\Users\\Anne\\Desktop\\NHIS\\Regional shape files\\cb_2018_us_region_500k.shp")

plot(regions)
names(regions)
glimpse(regions)
as_tibble(regions)
class(regions)

#2018
#Proportions
N18_age <- svyby(~AGE_P, ~REGION, NHIS18_DO, svymean, na.rm = TRUE)
N18_sex <- svyby(~SEX, ~REGION, NHIS18_DO, svymean, na.rm = TRUE)

#Age
N18_age_sel <- N18_age %>%
  select(1:12) %>%
  setNames(c("Region", "18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 and above")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Sex
N18_sex_sel <- N18_sex %>%
  select(1, 3) %>%
  setNames(c("Region", "Male")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Change the region column to "NAME" to match sf
names(N18_age_sel)[names(N18_age_sel) == "Region"] <- "NAME"

N18.Age.joined <- regions %>%
  left_join(N18_age_sel)

N18.Age.joined$NAME <- as.factor(N18.Age.joined$NAME)

#Plot: 18 to 24
N18.Age1824.region.plot <- ggplot(data = N18.Age.joined) +
  geom_sf(aes(fill = `18 to 24`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 18- to 24-year-olds per region in 2018") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N18.Age1824.Reg.png", width = 5, height = 5)

#Plot: 25 to 29
N18.Age2529.region.plot <- ggplot(data = N18.Age.joined) +
  geom_sf(aes(fill = `25 to 29`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 25- to 29-year-olds per region in 2018") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N18.Age2529.Reg.png", width = 5, height = 5)

#Plot: 30 to 34
N18.Age3034.region.plot <- ggplot(data = N18.Age.joined) +
  geom_sf(aes(fill = `30 to 34`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 30- to 34-year-olds per region in 2018") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N18.Age3034.Reg.png", width = 5, height = 5)

#Plot: 35 to 39
N18.Age3539.region.plot <- ggplot(data = N18.Age.joined) +
  geom_sf(aes(fill = `35 to 39`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 35- to 39-year-olds per region in 2018") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N18.Age3539.Reg.png", width = 5, height = 5)

#Plot: 40 to 44
N18.Age4044.region.plot <- ggplot(data = N18.Age.joined) +
  geom_sf(aes(fill = `40 to 44`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 40- to 49-year-olds per region in 2018") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N18.Age4044.Reg.png", width = 5, height = 5)

#Plot: 45 to 49
N18.Age4549.region.plot <- ggplot(data = N18.Age.joined) +
  geom_sf(aes(fill = `45 to 49`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 45- to 49-year-olds per region in 2018") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N18.Age4549.Reg.png", width = 5, height = 5)

#Plot: 50 to 54
N18.Age5054.region.plot <- ggplot(data = N18.Age.joined) +
  geom_sf(aes(fill = `50 to 54`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 50- to 54-year-olds per region in 2018") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N18.Age5054.Reg.png", width = 5, height = 5)

#Plot: 55 to 59
N18.Age5559.region.plot <- ggplot(data = N18.Age.joined) +
  geom_sf(aes(fill = `55 to 59`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 55- to 59-year-olds per region in 2018") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N18.Age5559.Reg.png", width = 5, height = 5)

#Plot: 60 to 64
N18.Age6064.region.plot <- ggplot(data = N18.Age.joined) +
  geom_sf(aes(fill = `60 to 64`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 60- to 64-year-olds per region in 2018") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N18.Age6064.Reg.png", width = 5, height = 5)

#Plot: 65 to 69
N18.Age6569.region.plot <- ggplot(data = N18.Age.joined) +
  geom_sf(aes(fill = `65 to 69`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 65- to 69-year-olds per region in 2018") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N18.Age6569.Reg.png", width = 5, height = 5)

#Plot: 70 and above
N18.Age.70ab.region.plot <- ggplot(data = N18.Age.joined) +
  geom_sf(aes(fill = `70 and above`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 70-year-olds and above per region in 2018") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N18.Age70ab.Reg.png", width = 5, height = 5)


#Change the region column to "NAME" to match sf
names(N18_sex_sel)[names(N18_sex_sel) == "Region"] <- "NAME"

N18.Sex.joined <- regions %>%
  left_join(N18_sex_sel)

N18.Sex.joined$NAME <- as.factor(N18.Sex.joined$NAME)

#Plot: Male
N18.Sex.Male.region.plot <- ggplot(data = N18.Sex.joined) +
  geom_sf(aes(fill = `Male`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(45, 55), breaks = c(45, 50, 55), low = "#FF0000", high = "black") +
  ggtitle("Percentage of males per region in 2018") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N18.SexMale.Reg.png", width = 5, height = 5)


#2017
#Proportions
N17_age <- svyby(~AGE_P, ~REGION, NHIS17_DO, svymean, na.rm = TRUE)
N17_sex <- svyby(~SEX, ~REGION, NHIS17_DO, svymean, na.rm = TRUE)

#Age
N17_age_sel <- N17_age %>%
  select(1:12) %>%
  setNames(c("Region", "18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 and above")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Sex
N17_sex_sel <- N17_sex %>%
  select(1, 3) %>%
  setNames(c("Region", "Male")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))


#Change the region column to "NAME" to match sf
names(N17_age_sel)[names(N17_age_sel) == "Region"] <- "NAME"

N17.Age.joined <- regions %>%
  left_join(N17_age_sel)

N17.Age.joined$NAME <- as.factor(N17.Age.joined$NAME)

#Plot: 18 to 24
N17.Age1824.region.plot <- ggplot(data = N17.Age.joined) +
  geom_sf(aes(fill = `18 to 24`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 18- to 24-year-olds per region in 2017") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N17.Age1824.Reg.png", width = 5, height = 5)

#Plot: 25 to 29
N17.Age2529.region.plot <- ggplot(data = N17.Age.joined) +
  geom_sf(aes(fill = `25 to 29`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 25- to 29-year-olds per region in 2017") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N17.Age2529.Reg.png", width = 5, height = 5)

#Plot: 30 to 34
N17.Age3034.region.plot <- ggplot(data = N17.Age.joined) +
  geom_sf(aes(fill = `30 to 34`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 30- to 34-year-olds per region in 2017") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N17.Age3034.Reg.png", width = 5, height = 5)

#Plot: 35 to 39
N17.Age3539.region.plot <- ggplot(data = N17.Age.joined) +
  geom_sf(aes(fill = `35 to 39`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 35- to 39-year-olds per region in 2017") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N17.Age3539.Reg.png", width = 5, height = 5)

#Plot: 40 to 44
N17.Age4044.region.plot <- ggplot(data = N17.Age.joined) +
  geom_sf(aes(fill = `40 to 44`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 40- to 44-year-olds per region in 2017") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N17.Age4044.Reg.png", width = 5, height = 5)

#Plot: 45 to 49
N17.Age4549.region.plot <- ggplot(data = N17.Age.joined) +
  geom_sf(aes(fill = `45 to 49`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 45- to 49-year-olds per region in 2017") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N17.Age4549.Reg.png", width = 5, height = 5)

#Plot: 50 to 54
N17.Age5054.region.plot <- ggplot(data = N17.Age.joined) +
  geom_sf(aes(fill = `50 to 54`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 50- to 54-year-olds per region in 2017") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N17.Age5054.Reg.png", width = 5, height = 5)

#Plot: 55 to 59
N17.Age5559.region.plot <- ggplot(data = N17.Age.joined) +
  geom_sf(aes(fill = `55 to 59`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 55- to 59-year-olds per region in 2017") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N17.Age5559.Reg.png", width = 5, height = 5)

#Plot: 60 to 64
N17.Age6064.region.plot <- ggplot(data = N17.Age.joined) +
  geom_sf(aes(fill = `60 to 64`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 60- to 64-year-olds per region in 2017") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N17.Age6064.Reg.png", width = 5, height = 5)

#Plot: 65 to 69
N17.Age6569.region.plot <- ggplot(data = N17.Age.joined) +
  geom_sf(aes(fill = `65 to 69`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 65- to 69-year-olds per region in 2017") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N17.Age6569.Reg.png", width = 5, height = 5)

#Plot: 70 and above
N17.Age.70ab.region.plot <- ggplot(data = N17.Age.joined) +
  geom_sf(aes(fill = `70 and above`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 70-year-olds and above per region in 2017") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N17.Age70ab.Reg.png", width = 5, height = 5)


#Change the region column to "NAME" to match sf
names(N17_sex_sel)[names(N17_sex_sel) == "Region"] <- "NAME"

N17.Sex.joined <- regions %>%
  left_join(N17_sex_sel)

N17.Sex.joined$NAME <- as.factor(N17.Sex.joined$NAME)

#Plot: Male
N17.Sex.Male.region.plot <- ggplot(data = N17.Sex.joined) +
  geom_sf(aes(fill = `Male`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(45, 55), breaks = c(45, 50, 55), low = "#FF0000", high = "black") +
  ggtitle("Percentage of males per region in 2017") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N17.SexMale.Reg.png", width = 5, height = 5)


#2016
#Proportions
N16_age <- svyby(~AGE_P, ~REGION, NHIS16_DO, svymean, na.rm = TRUE)
N16_sex <- svyby(~SEX, ~REGION, NHIS16_DO, svymean, na.rm = TRUE)

#Age
N16_age_sel <- N16_age %>%
  select(1:12) %>%
  setNames(c("Region", "18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 and above")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Sex
N16_sex_sel <- N16_sex %>%
  select(1, 3) %>%
  setNames(c("Region", "Male")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Change the region column to "NAME" to match sf
names(N16_age_sel)[names(N16_age_sel) == "Region"] <- "NAME"

N16.Age.joined <- regions %>%
  left_join(N16_age_sel)

N16.Age.joined$NAME <- as.factor(N16.Age.joined$NAME)

#Plot: 18 to 24
N16.Age1824.region.plot <- ggplot(data = N16.Age.joined) +
  geom_sf(aes(fill = `18 to 24`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 18- to 24-year-olds per region in 2016") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N16.Age1824.Reg.png", width = 5, height = 5)

#Plot: 25 to 29
N16.Age2529.region.plot <- ggplot(data = N16.Age.joined) +
  geom_sf(aes(fill = `25 to 29`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 25- to 29-year-olds per region in 2016") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N16.Age2529.Reg.png", width = 5, height = 5)

#Plot: 30 to 34
N16.Age3034.region.plot <- ggplot(data = N16.Age.joined) +
  geom_sf(aes(fill = `30 to 34`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 30- to 34-year-olds per region in 2016") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N16.Age3034.Reg.png", width = 5, height = 5)

#Plot: 35 to 39
N16.Age3539.region.plot <- ggplot(data = N16.Age.joined) +
  geom_sf(aes(fill = `35 to 39`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 35- to 39-year-olds per region in 2016") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N16.Age3539.Reg.png", width = 5, height = 5)

#Plot: 40 to 44
N16.Age4044.region.plot <- ggplot(data = N16.Age.joined) +
  geom_sf(aes(fill = `40 to 44`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 40- to 44-year-olds per region in 2016") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N16.Age4044.Reg.png", width = 5, height = 5)

#Plot: 45 to 49
N16.Age4549.region.plot <- ggplot(data = N16.Age.joined) +
  geom_sf(aes(fill = `45 to 49`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 45- to 49-year-olds per region in 2016") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N16.Age4549.Reg.png", width = 5, height = 5)

#Plot: 50 to 54
N16.Age5054.region.plot <- ggplot(data = N16.Age.joined) +
  geom_sf(aes(fill = `50 to 54`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 50- to 54-year-olds per region in 2016") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N16.Age5054.Reg.png", width = 5, height = 5)

#Plot: 55 to 59
N16.Age5559.region.plot <- ggplot(data = N16.Age.joined) +
  geom_sf(aes(fill = `55 to 59`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 55- to 59-year-olds per region in 2016") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N16.Age5559.Reg.png", width = 5, height = 5)

#Plot: 60 to 64
N16.Age6064.region.plot <- ggplot(data = N16.Age.joined) +
  geom_sf(aes(fill = `60 to 64`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 60- to 64-year-olds per region in 2016") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N16.Age6064.Reg.png", width = 5, height = 5)

#Plot: 65 to 69
N16.Age6569.region.plot <- ggplot(data = N16.Age.joined) +
  geom_sf(aes(fill = `65 to 69`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 65- to 69-year-olds per region in 2016") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N16.Age6569.Reg.png", width = 5, height = 5)

#Plot: 70 and above
N16.Age.70ab.region.plot <- ggplot(data = N16.Age.joined) +
  geom_sf(aes(fill = `70 and above`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 70-year-olds and above per region in 2016") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N16.Age70ab.Reg.png", width = 5, height = 5)


#Change the region column to "NAME" to match sf
names(N16_sex_sel)[names(N16_sex_sel) == "Region"] <- "NAME"

N16.Sex.joined <- regions %>%
  left_join(N16_sex_sel)

N16.Sex.joined$NAME <- as.factor(N16.Sex.joined$NAME)

#Plot: Male
N16.Sex.Male.region.plot <- ggplot(data = N16.Sex.joined) +
  geom_sf(aes(fill = `Male`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(45, 55), breaks = c(45, 50, 55), low = "#FF0000", high = "black") +
  ggtitle("Percentage of males per region in 2016") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N16.SexMale.Reg.png", width = 5, height = 5)


#2015
#Proportions
N15_age <- svyby(~AGE_P, ~REGION, NHIS15_DO, svymean, na.rm = TRUE)
N15_sex <- svyby(~SEX, ~REGION, NHIS15_DO, svymean, na.rm = TRUE)

#Age
N15_age_sel <- N15_age %>%
  select(1:12) %>%
  setNames(c("Region", "18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 and above")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Sex
N15_sex_sel <- N15_sex %>%
  select(1, 3) %>%
  setNames(c("Region", "Male")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Change the region column to "NAME" to match sf
names(N15_age_sel)[names(N15_age_sel) == "Region"] <- "NAME"

N15.Age.joined <- regions %>%
  left_join(N15_age_sel)

N15.Age.joined$NAME <- as.factor(N15.Age.joined$NAME)

#Plot: 18 to 24
N15.Age1824.region.plot <- ggplot(data = N15.Age.joined) +
  geom_sf(aes(fill = `18 to 24`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 18- to 24-year-olds per region in 2015") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N15.Age1824.Reg.png", width = 5, height = 5)

#Plot: 25 to 29
N15.Age2529.region.plot <- ggplot(data = N15.Age.joined) +
  geom_sf(aes(fill = `25 to 29`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 25- to 29-year-olds per region in 2015") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N15.Age2529.Reg.png", width = 5, height = 5)

#Plot: 30 to 34
N15.Age3034.region.plot <- ggplot(data = N15.Age.joined) +
  geom_sf(aes(fill = `30 to 34`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 30- to 34-year-olds per region in 2015") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N15.Age3034.Reg.png", width = 5, height = 5)

#Plot: 35 to 39
N15.Age3539.region.plot <- ggplot(data = N15.Age.joined) +
  geom_sf(aes(fill = `35 to 39`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 35- to 39-year-olds per region in 2015") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N15.Age3539.Reg.png", width = 5, height = 5)

#Plot: 40 to 44
N15.Age4044.region.plot <- ggplot(data = N15.Age.joined) +
  geom_sf(aes(fill = `40 to 44`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 40- to 44-year-olds per region in 2015") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N15.Age4044.Reg.png", width = 5, height = 5)

#Plot: 45 to 49
N15.Age4549.region.plot <- ggplot(data = N15.Age.joined) +
  geom_sf(aes(fill = `45 to 49`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 45- to 49-year-olds per region in 2015") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N15.Age4549.Reg.png", width = 5, height = 5)

#Plot: 50 to 54
N15.Age5054.region.plot <- ggplot(data = N15.Age.joined) +
  geom_sf(aes(fill = `50 to 54`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 50- to 54-year-olds per region in 2015") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N15.Age5054.Reg.png", width = 5, height = 5)

#Plot: 55 to 59
N15.Age5559.region.plot <- ggplot(data = N15.Age.joined) +
  geom_sf(aes(fill = `55 to 59`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 55- to 59-year-olds per region in 2015") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N15.Age5559.Reg.png", width = 5, height = 5)

#Plot: 60 to 64
N15.Age6064.region.plot <- ggplot(data = N15.Age.joined) +
  geom_sf(aes(fill = `60 to 64`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 60- to 64-year-olds per region in 2015") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N15.Age6064.Reg.png", width = 5, height = 5)

#Plot: 65 to 69
N15.Age6569.region.plot <- ggplot(data = N15.Age.joined) +
  geom_sf(aes(fill = `65 to 69`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 65- to 69-year-olds per region in 2015") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N15.Age6569.Reg.png", width = 5, height = 5)

#Plot: 70 and above
N15.Age.70ab.region.plot <- ggplot(data = N15.Age.joined) +
  geom_sf(aes(fill = `70 and above`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 70-year-olds and above per region in 2015") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N15.Age70ab.Reg.png", width = 5, height = 5)


#Change the region column to "NAME" to match sf
names(N15_sex_sel)[names(N15_sex_sel) == "Region"] <- "NAME"

N15.Sex.joined <- regions %>%
  left_join(N15_sex_sel)

N15.Sex.joined$NAME <- as.factor(N15.Sex.joined$NAME)

#Plot: Male
N15.Sex.Male.region.plot <- ggplot(data = N15.Sex.joined) +
  geom_sf(aes(fill = `Male`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(45, 55), breaks = c(45, 50, 55), low = "#FF0000", high = "black") +
  ggtitle("Percentage of males per region in 2015") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N15.SexMale.Reg.png", width = 5, height = 5)


#Proportions
N14_age <- svyby(~AGE_P, ~REGION, NHIS14_DO, svymean, na.rm = TRUE)
N14_sex <- svyby(~SEX, ~REGION, NHIS14_DO, svymean, na.rm = TRUE)

#Age
N14_age_sel <- N14_age %>%
  select(1:12) %>%
  setNames(c("Region", "18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 and above")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Sex
N14_sex_sel <- N14_sex %>%
  select(1, 3) %>%
  setNames(c("Region", "Male")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Change the region column to "NAME" to match sf
names(N14_age_sel)[names(N14_age_sel) == "Region"] <- "NAME"

N14.Age.joined <- regions %>%
  left_join(N14_age_sel)

N14.Age.joined$NAME <- as.factor(N14.Age.joined$NAME)

#Plot: 18 to 24
N14.Age1824.region.plot <- ggplot(data = N14.Age.joined) +
  geom_sf(aes(fill = `18 to 24`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 18- to 24-year-olds per region in 2014") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N14.Age1824.Reg.png", width = 5, height = 5)

#Plot: 25 to 29
N14.Age2529.region.plot <- ggplot(data = N14.Age.joined) +
  geom_sf(aes(fill = `25 to 29`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 25- to 29-year-olds per region in 2014") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N14.Age2529.Reg.png", width = 5, height = 5)

#Plot: 30 to 34
N14.Age3034.region.plot <- ggplot(data = N14.Age.joined) +
  geom_sf(aes(fill = `30 to 34`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 30- to 34-year-olds per region in 2014") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N14.Age3034.Reg.png", width = 5, height = 5)

#Plot: 35 to 39
N14.Age3539.region.plot <- ggplot(data = N14.Age.joined) +
  geom_sf(aes(fill = `35 to 39`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 35- to 39-year-olds per region in 2014") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N14.Age3539.Reg.png", width = 5, height = 5)

#Plot: 40 to 44
N14.Age4044.region.plot <- ggplot(data = N14.Age.joined) +
  geom_sf(aes(fill = `40 to 44`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 40- to 44-year-olds per region in 2014") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N14.Age4044.Reg.png", width = 5, height = 5)

#Plot: 45 to 49
N14.Age4549.region.plot <- ggplot(data = N14.Age.joined) +
  geom_sf(aes(fill = `45 to 49`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 45- to 49-year-olds per region in 2014") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N14.Age4549.Reg.png", width = 5, height = 5)

#Plot: 50 to 54
N14.Age5054.region.plot <- ggplot(data = N14.Age.joined) +
  geom_sf(aes(fill = `50 to 54`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 50- to 54-year-olds per region in 2014") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N14.Age5054.Reg.png", width = 5, height = 5)

#Plot: 55 to 59
N14.Age5559.region.plot <- ggplot(data = N14.Age.joined) +
  geom_sf(aes(fill = `55 to 59`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 55- to 59-year-olds per region in 2014") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N14.Age5559.Reg.png", width = 5, height = 5)

#Plot: 60 to 64
N14.Age6064.region.plot <- ggplot(data = N14.Age.joined) +
  geom_sf(aes(fill = `60 to 64`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 60- to 64-year-olds per region in 2014") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N14.Age6064.Reg.png", width = 5, height = 5)

#Plot: 65 to 69
N14.Age6569.region.plot <- ggplot(data = N14.Age.joined) +
  geom_sf(aes(fill = `65 to 69`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 65- to 69-year-olds per region in 2014") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N14.Age6569.Reg.png", width = 5, height = 5)

#Plot: 70 and above
N14.Age.70ab.region.plot <- ggplot(data = N14.Age.joined) +
  geom_sf(aes(fill = `70 and above`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 70-year-olds and above per region in 2014") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N14.Age70ab.Reg.png", width = 5, height = 5)


#Change the region column to "NAME" to match sf
names(N14_sex_sel)[names(N14_sex_sel) == "Region"] <- "NAME"

N14.Sex.joined <- regions %>%
  left_join(N14_sex_sel)

N14.Sex.joined$NAME <- as.factor(N14.Sex.joined$NAME)

#Plot: Male
N14.Sex.Male.region.plot <- ggplot(data = N14.Sex.joined) +
  geom_sf(aes(fill = `Male`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(45, 55), breaks = c(45, 50, 55), low = "#FF0000", high = "black") +
  ggtitle("Percentage of males per region in 2014") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N14.SexMale.Reg.png", width = 5, height = 5)


#2013
#Proportions
N13_age <- svyby(~AGE_P, ~REGION, NHIS13_DO, svymean, na.rm = TRUE)
N13_sex <- svyby(~SEX, ~REGION, NHIS13_DO, svymean, na.rm = TRUE)

#Age
N13_age_sel <- N13_age %>%
  select(1:12) %>%
  setNames(c("Region", "18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 and above")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Sex
N13_sex_sel <- N13_sex %>%
  select(1, 3) %>%
  setNames(c("Region", "Male")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Change the region column to "NAME" to match sf
names(N13_age_sel)[names(N13_age_sel) == "Region"] <- "NAME"

N13.Age.joined <- regions %>%
  left_join(N13_age_sel)

N13.Age.joined$NAME <- as.factor(N13.Age.joined$NAME)

#Plot: 18 to 24
N13.Age1824.region.plot <- ggplot(data = N13.Age.joined) +
  geom_sf(aes(fill = `18 to 24`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 18- to 24-year-olds per region in 2013") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N13.Age1824.Reg.png", width = 5, height = 5)

#Plot: 25 to 29
N13.Age2529.region.plot <- ggplot(data = N13.Age.joined) +
  geom_sf(aes(fill = `25 to 29`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 25- to 29-year-olds per region in 2013") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N13.Age2529.Reg.png", width = 5, height = 5)

#Plot: 30 to 34
N13.Age3034.region.plot <- ggplot(data = N13.Age.joined) +
  geom_sf(aes(fill = `30 to 34`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 30- to 34-year-olds per region in 2013") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N13.Age3034.Reg.png", width = 5, height = 5)

#Plot: 35 to 39
N13.Age3539.region.plot <- ggplot(data = N13.Age.joined) +
  geom_sf(aes(fill = `35 to 39`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 35- to 39-year-olds per region in 2013") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N13.Age3539.Reg.png", width = 5, height = 5)

#Plot: 40 to 44
N13.Age4044.region.plot <- ggplot(data = N13.Age.joined) +
  geom_sf(aes(fill = `40 to 44`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 40- to 44-year-olds per region in 2013") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N13.Age4044.Reg.png", width = 5, height = 5)

#Plot: 45 to 49
N13.Age4549.region.plot <- ggplot(data = N13.Age.joined) +
  geom_sf(aes(fill = `45 to 49`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 45- to 49-year-olds per region in 2013") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N13.Age4549.Reg.png", width = 5, height = 5)

#Plot: 50 to 54
N13.Age5054.region.plot <- ggplot(data = N13.Age.joined) +
  geom_sf(aes(fill = `50 to 54`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 50- to 54-year-olds per region in 2013") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N13.Age5054.Reg.png", width = 5, height = 5)

#Plot: 55 to 59
N13.Age5559.region.plot <- ggplot(data = N13.Age.joined) +
  geom_sf(aes(fill = `55 to 59`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 55- to 59-year-olds per region in 2013") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N13.Age5559.Reg.png", width = 5, height = 5)

#Plot: 60 to 64
N13.Age6064.region.plot <- ggplot(data = N13.Age.joined) +
  geom_sf(aes(fill = `60 to 64`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 60- to 64-year-olds per region in 2013") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N13.Age6064.Reg.png", width = 5, height = 5)

#Plot: 65 to 69
N13.Age6569.region.plot <- ggplot(data = N13.Age.joined) +
  geom_sf(aes(fill = `65 to 69`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 65- to 69-year-olds per region in 2013") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N13.Age6569.Reg.png", width = 5, height = 5)

#Plot: 70 and above
N13.Age.70ab.region.plot <- ggplot(data = N13.Age.joined) +
  geom_sf(aes(fill = `70 and above`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 70-year-olds and above per region in 2013") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N13.Age70ab.Reg.png", width = 5, height = 5)


#Change the region column to "NAME" to match sf
names(N13_sex_sel)[names(N13_sex_sel) == "Region"] <- "NAME"

N13.Sex.joined <- regions %>%
  left_join(N13_sex_sel)

N13.Sex.joined$NAME <- as.factor(N13.Sex.joined$NAME)

#Plot: Male
N13.Sex.Male.region.plot <- ggplot(data = N13.Sex.joined) +
  geom_sf(aes(fill = `Male`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(45, 55), breaks = c(45, 50, 55), low = "#FF0000", high = "black") +
  ggtitle("Percentage of males per region in 2013") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N13.SexMale.Reg.png", width = 5, height = 5)


#2012
#Proportions
N12_age <- svyby(~AGE_P, ~REGION, NHIS12_DO, svymean, na.rm = TRUE)
N12_sex <- svyby(~SEX, ~REGION, NHIS12_DO, svymean, na.rm = TRUE)

#Age
N12_age_sel <- N12_age %>%
  select(1:12) %>%
  setNames(c("Region", "18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 and above")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Sex
N12_sex_sel <- N12_sex %>%
  select(1, 3) %>%
  setNames(c("Region", "Male")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Change the region column to "NAME" to match sf
names(N12_age_sel)[names(N12_age_sel) == "Region"] <- "NAME"

N12.Age.joined <- regions %>%
  left_join(N12_age_sel)

N12.Age.joined$NAME <- as.factor(N12.Age.joined$NAME)

#Plot: 18 to 24
N12.Age1824.region.plot <- ggplot(data = N12.Age.joined) +
  geom_sf(aes(fill = `18 to 24`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 18- to 24-year-olds per region in 2012") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N12.Age1824.Reg.png", width = 5, height = 5)

#Plot: 25 to 29
N12.Age2529.region.plot <- ggplot(data = N12.Age.joined) +
  geom_sf(aes(fill = `25 to 29`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 25- to 29-year-olds per region in 2012") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N12.Age2529.Reg.png", width = 5, height = 5)

#Plot: 30 to 34
N12.Age3034.region.plot <- ggplot(data = N12.Age.joined) +
  geom_sf(aes(fill = `30 to 34`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 30- to 34-year-olds per region in 2012") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N12.Age3034.Reg.png", width = 5, height = 5)

#Plot: 35 to 39
N12.Age3539.region.plot <- ggplot(data = N12.Age.joined) +
  geom_sf(aes(fill = `35 to 39`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 35- to 39-year-olds per region in 2012") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N12.Age3539.Reg.png", width = 5, height = 5)

#Plot: 40 to 44
N12.Age4044.region.plot <- ggplot(data = N12.Age.joined) +
  geom_sf(aes(fill = `40 to 44`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 40- to 44-year-olds per region in 2012") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N12.Age4044.Reg.png", width = 5, height = 5)

#Plot: 45 to 49
N12.Age4549.region.plot <- ggplot(data = N12.Age.joined) +
  geom_sf(aes(fill = `45 to 49`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 45- to 49-year-olds per region in 2012") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N12.Age4549.Reg.png", width = 5, height = 5)

#Plot: 50 to 54
N12.Age5054.region.plot <- ggplot(data = N12.Age.joined) +
  geom_sf(aes(fill = `50 to 54`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 50- to 54-year-olds per region in 2012") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N12.Age5054.Reg.png", width = 5, height = 5)

#Plot: 55 to 59
N12.Age5559.region.plot <- ggplot(data = N12.Age.joined) +
  geom_sf(aes(fill = `55 to 59`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 55- to 59-year-olds per region in 2012") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N12.Age5559.Reg.png", width = 5, height = 5)

#Plot: 60 to 64
N12.Age6064.region.plot <- ggplot(data = N12.Age.joined) +
  geom_sf(aes(fill = `60 to 64`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 60- to 64-year-olds per region in 2012") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N12.Age6064.Reg.png", width = 5, height = 5)

#Plot: 65 to 69
N12.Age6569.region.plot <- ggplot(data = N12.Age.joined) +
  geom_sf(aes(fill = `65 to 69`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 65- to 69-year-olds per region in 2012") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N12.Age6569.Reg.png", width = 5, height = 5)

#Plot: 70 and above
N12.Age.70ab.region.plot <- ggplot(data = N12.Age.joined) +
  geom_sf(aes(fill = `70 and above`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 70-year-olds and above per region in 2012") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N12.Age70ab.Reg.png", width = 5, height = 5)


#Change the region column to "NAME" to match sf
names(N12_sex_sel)[names(N12_sex_sel) == "Region"] <- "NAME"

N12.Sex.joined <- regions %>%
  left_join(N12_sex_sel)

N12.Sex.joined$NAME <- as.factor(N12.Sex.joined$NAME)

#Plot: Male
N12.Sex.Male.region.plot <- ggplot(data = N12.Sex.joined) +
  geom_sf(aes(fill = `Male`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(45, 55), breaks = c(45, 50, 55), low = "#FF0000", high = "black") +
  ggtitle("Percentage of males per region in 2012") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N12.SexMale.Reg.png", width = 5, height = 5)


#2011

#Proportions
N11_age <- svyby(~AGE_P, ~REGION, NHIS11_DO, svymean, na.rm = TRUE)
N11_sex <- svyby(~SEX, ~REGION, NHIS11_DO, svymean, na.rm = TRUE)

#Age
N11_age_sel <- N11_age %>%
  select(1:12) %>%
  setNames(c("Region", "18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 and above")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Sex
N11_sex_sel <- N11_sex %>%
  select(1, 3) %>%
  setNames(c("Region", "Male")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))


#Change the region column to "NAME" to match sf
names(N11_age_sel)[names(N11_age_sel) == "Region"] <- "NAME"

N11.Age.joined <- regions %>%
  left_join(N11_age_sel)

N11.Age.joined$NAME <- as.factor(N11.Age.joined$NAME)

#Plot: 18 to 24
N11.Age1824.region.plot <- ggplot(data = N11.Age.joined) +
  geom_sf(aes(fill = `18 to 24`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 18- to 24-year-olds per region in 2011") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N11.Age1824.Reg.png", width = 5, height = 5)

#Plot: 25 to 29
N11.Age2529.region.plot <- ggplot(data = N11.Age.joined) +
  geom_sf(aes(fill = `25 to 29`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 25- to 29-year-olds per region in 2011") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N11.Age2529.Reg.png", width = 5, height = 5)

#Plot: 30 to 34
N11.Age3034.region.plot <- ggplot(data = N11.Age.joined) +
  geom_sf(aes(fill = `30 to 34`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 30- to 34-year-olds per region in 2011") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N11.Age3034.Reg.png", width = 5, height = 5)

#Plot: 35 to 39
N11.Age3539.region.plot <- ggplot(data = N11.Age.joined) +
  geom_sf(aes(fill = `35 to 39`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 35- to 39-year-olds per region in 2011") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N11.Age3539.Reg.png", width = 5, height = 5)

#Plot: 40 to 44
N11.Age4044.region.plot <- ggplot(data = N11.Age.joined) +
  geom_sf(aes(fill = `40 to 44`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 40- to 44-year-olds per region in 2011") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N11.Age4044.Reg.png", width = 5, height = 5)

#Plot: 45 to 49
N11.Age4549.region.plot <- ggplot(data = N11.Age.joined) +
  geom_sf(aes(fill = `45 to 49`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 45- to 49-year-olds per region in 2011") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N11.Age4549.Reg.png", width = 5, height = 5)

#Plot: 50 to 54
N11.Age5054.region.plot <- ggplot(data = N11.Age.joined) +
  geom_sf(aes(fill = `50 to 54`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 50- to 54-year-olds per region in 2011") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N11.Age5054.Reg.png", width = 5, height = 5)

#Plot: 55 to 59
N11.Age5559.region.plot <- ggplot(data = N11.Age.joined) +
  geom_sf(aes(fill = `55 to 59`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 55- to 59-year-olds per region in 2011") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N11.Age5559.Reg.png", width = 5, height = 5)

#Plot: 60 to 64
N11.Age6064.region.plot <- ggplot(data = N11.Age.joined) +
  geom_sf(aes(fill = `60 to 64`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 60- to 64-year-olds per region in 2011") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N11.Age6064.Reg.png", width = 5, height = 5)

#Plot: 65 to 69
N11.Age6569.region.plot <- ggplot(data = N11.Age.joined) +
  geom_sf(aes(fill = `65 to 69`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 65- to 69-year-olds per region in 2011") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N11.Age6569.Reg.png", width = 5, height = 5)

#Plot: 70 and above
N11.Age.70ab.region.plot <- ggplot(data = N11.Age.joined) +
  geom_sf(aes(fill = `70 and above`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 70-year-olds and above per region in 2011") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N11.Age70ab.Reg.png", width = 5, height = 5)


#Change the region column to "NAME" to match sf
names(N11_sex_sel)[names(N11_sex_sel) == "Region"] <- "NAME"

N11.Sex.joined <- regions %>%
  left_join(N11_sex_sel)

N11.Sex.joined$NAME <- as.factor(N11.Sex.joined$NAME)

#Plot: Male
N11.Sex.Male.region.plot <- ggplot(data = N11.Sex.joined) +
  geom_sf(aes(fill = `Male`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(45, 55), breaks = c(45, 50, 55), low = "#FF0000", high = "black") +
  ggtitle("Percentage of males per region in 2011") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N11.SexMale.Reg.png", width = 5, height = 5)


#2010
#Proportions
N10_age <- svyby(~AGE_P, ~REGION, NHIS10_DO, svymean, na.rm = TRUE)
N10_sex <- svyby(~SEX, ~REGION, NHIS10_DO, svymean, na.rm = TRUE)

#Age
N10_age_sel <- N10_age %>%
  select(1:12) %>%
  setNames(c("Region", "18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 and above")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Sex
N10_sex_sel <- N10_sex %>%
  select(1, 3) %>%
  setNames(c("Region", "Male")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))


#Change the region column to "NAME" to match sf
names(N10_age_sel)[names(N10_age_sel) == "Region"] <- "NAME"

N10.Age.joined <- regions %>%
  left_join(N10_age_sel)

N10.Age.joined$NAME <- as.factor(N10.Age.joined$NAME)

#Plot: 18 to 24
N10.Age1824.region.plot <- ggplot(data = N10.Age.joined) +
  geom_sf(aes(fill = `18 to 24`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 18- to 24-year-olds per region in 2010") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N10.Age1824.Reg.png", width = 5, height = 5)

#Plot: 25 to 29
N10.Age2529.region.plot <- ggplot(data = N10.Age.joined) +
  geom_sf(aes(fill = `25 to 29`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 25- to 29-year-olds per region in 2010") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N10.Age2529.Reg.png", width = 5, height = 5)

#Plot: 30 to 34
N10.Age3034.region.plot <- ggplot(data = N10.Age.joined) +
  geom_sf(aes(fill = `30 to 34`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 30- to 34-year-olds per region in 2010") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N10.Age3034.Reg.png", width = 5, height = 5)

#Plot: 35 to 39
N10.Age3539.region.plot <- ggplot(data = N10.Age.joined) +
  geom_sf(aes(fill = `35 to 39`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 35- to 39-year-olds per region in 2010") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N10.Age3539.Reg.png", width = 5, height = 5)

#Plot: 40 to 44
N10.Age4044.region.plot <- ggplot(data = N10.Age.joined) +
  geom_sf(aes(fill = `40 to 44`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 40- to 44-year-olds per region in 2010") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N10.Age4044.Reg.png", width = 5, height = 5)

#Plot: 45 to 49
N10.Age4549.region.plot <- ggplot(data = N10.Age.joined) +
  geom_sf(aes(fill = `45 to 49`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 45- to 49-year-olds per region in 2010") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N10.Age4549.Reg.png", width = 5, height = 5)

#Plot: 50 to 54
N10.Age5054.region.plot <- ggplot(data = N10.Age.joined) +
  geom_sf(aes(fill = `50 to 54`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 50- to 54-year-olds per region in 2010") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N10.Age5054.Reg.png", width = 5, height = 5)

#Plot: 55 to 59
N10.Age5559.region.plot <- ggplot(data = N10.Age.joined) +
  geom_sf(aes(fill = `55 to 59`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 55- to 59-year-olds per region in 2010") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N10.Age5559.Reg.png", width = 5, height = 5)

#Plot: 60 to 64
N10.Age6064.region.plot <- ggplot(data = N10.Age.joined) +
  geom_sf(aes(fill = `60 to 64`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 60- to 64-year-olds per region in 2010") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N10.Age6064.Reg.png", width = 5, height = 5)

#Plot: 65 to 69
N10.Age6569.region.plot <- ggplot(data = N10.Age.joined) +
  geom_sf(aes(fill = `65 to 69`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 65- to 69-year-olds per region in 2010") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N10.Age6569.Reg.png", width = 5, height = 5)

#Plot: 70 and above
N10.Age.70ab.region.plot <- ggplot(data = N10.Age.joined) +
  geom_sf(aes(fill = `70 and above`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 70-year-olds and above per region in 2010") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N10.Age70ab.Reg.png", width = 5, height = 5)


#Change the region column to "NAME" to match sf
names(N10_sex_sel)[names(N10_sex_sel) == "Region"] <- "NAME"

N10.Sex.joined <- regions %>%
  left_join(N10_sex_sel)

N10.Sex.joined$NAME <- as.factor(N10.Sex.joined$NAME)

#Plot: Male
N10.Sex.Male.region.plot <- ggplot(data = N10.Sex.joined) +
  geom_sf(aes(fill = `Male`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(45, 55), breaks = c(45, 50, 55), low = "#FF0000", high = "black") +
  ggtitle("Percentage of males per region in 2010") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N10.SexMale.Reg.png", width = 5, height = 5)


#2009
#Proportions
N09_age <- svyby(~AGE_P, ~REGION, NHIS09_DO, svymean, na.rm = TRUE)
N09_sex <- svyby(~SEX, ~REGION, NHIS09_DO, svymean, na.rm = TRUE)

#Age
N09_age_sel <- N09_age %>%
  select(1:12) %>%
  setNames(c("Region", "18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 and above")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Sex
N09_sex_sel <- N09_sex %>%
  select(1, 3) %>%
  setNames(c("Region", "Male")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))


#Change the region column to "NAME" to match sf
names(N09_age_sel)[names(N09_age_sel) == "Region"] <- "NAME"

N09.Age.joined <- regions %>%
  left_join(N09_age_sel)

N09.Age.joined$NAME <- as.factor(N09.Age.joined$NAME)

#Plot: 18 to 24
N09.Age1824.region.plot <- ggplot(data = N09.Age.joined) +
  geom_sf(aes(fill = `18 to 24`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 18- to 24-year-olds per region in 2009") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N09.Age1824.Reg.png", width = 5, height = 5)

#Plot: 25 to 29
N09.Age2529.region.plot <- ggplot(data = N09.Age.joined) +
  geom_sf(aes(fill = `25 to 29`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 25- to 29-year-olds per region in 2009") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N09.Age2529.Reg.png", width = 5, height = 5)

#Plot: 30 to 34
N09.Age3034.region.plot <- ggplot(data = N09.Age.joined) +
  geom_sf(aes(fill = `30 to 34`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 30- to 34-year-olds per region in 2009") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N09.Age3034.Reg.png", width = 5, height = 5)

#Plot: 35 to 39
N09.Age3539.region.plot <- ggplot(data = N09.Age.joined) +
  geom_sf(aes(fill = `35 to 39`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 35- to 39-year-olds per region in 2009") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N09.Age3539.Reg.png", width = 5, height = 5)

#Plot: 40 to 44
N09.Age4044.region.plot <- ggplot(data = N09.Age.joined) +
  geom_sf(aes(fill = `40 to 44`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 40- to 44-year-olds per region in 2009") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N09.Age4044.Reg.png", width = 5, height = 5)

#Plot: 45 to 49
N09.Age4549.region.plot <- ggplot(data = N09.Age.joined) +
  geom_sf(aes(fill = `45 to 49`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 45- to 49-year-olds per region in 2009") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N09.Age4549.Reg.png", width = 5, height = 5)

#Plot: 50 to 54
N09.Age5054.region.plot <- ggplot(data = N09.Age.joined) +
  geom_sf(aes(fill = `50 to 54`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 50- to 54-year-olds per region in 2009") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N09.Age5054.Reg.png", width = 5, height = 5)

#Plot: 55 to 59
N09.Age5559.region.plot <- ggplot(data = N09.Age.joined) +
  geom_sf(aes(fill = `55 to 59`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 55- to 59-year-olds per region in 2009") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N09.Age5559.Reg.png", width = 5, height = 5)

#Plot: 60 to 64
N09.Age6064.region.plot <- ggplot(data = N09.Age.joined) +
  geom_sf(aes(fill = `60 to 64`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 60- to 64-year-olds per region in 2009") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N09.Age6064.Reg.png", width = 5, height = 5)

#Plot: 65 to 69
N09.Age6569.region.plot <- ggplot(data = N09.Age.joined) +
  geom_sf(aes(fill = `65 to 69`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(4, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 65- to 69-year-olds per region in 2009") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N09.Age6569.Reg.png", width = 5, height = 5)

#Plot: 70 and above
N09.Age.70ab.region.plot <- ggplot(data = N09.Age.joined) +
  geom_sf(aes(fill = `70 and above`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 70-year-olds and above per region in 2009") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N09.Age70ab.Reg.png", width = 5, height = 5)


#Change the region column to "NAME" to match sf
names(N09_sex_sel)[names(N09_sex_sel) == "Region"] <- "NAME"

N09.Sex.joined <- regions %>%
  left_join(N09_sex_sel)

N09.Sex.joined$NAME <- as.factor(N09.Sex.joined$NAME)

#Plot: Male
N09.Sex.Male.region.plot <- ggplot(data = N09.Sex.joined) +
  geom_sf(aes(fill = `Male`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(45, 55), breaks = c(45, 50, 55), low = "#FF0000", high = "black") +
  ggtitle("Percentage of males per region in 2009") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N09.SexMale.Reg.png", width = 5, height = 5)


#2008
#Proportions
N08_age <- svyby(~AGE_P, ~REGION, NHIS08_DO, svymean, na.rm = TRUE)
N08_sex <- svyby(~SEX, ~REGION, NHIS08_DO, svymean, na.rm = TRUE)

#Age
N08_age_sel <- N08_age %>%
  select(1:12) %>%
  setNames(c("Region", "18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 and above")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Sex
N08_sex_sel <- N08_sex %>%
  select(1, 3) %>%
  setNames(c("Region", "Male")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))


#Change the region column to "NAME" to match sf
names(N08_age_sel)[names(N08_age_sel) == "Region"] <- "NAME"

N08.Age.joined <- regions %>%
  left_join(N08_age_sel)

N08.Age.joined$NAME <- as.factor(N08.Age.joined$NAME)

#Plot: 18 to 24
N08.Age1824.region.plot <- ggplot(data = N08.Age.joined) +
  geom_sf(aes(fill = `18 to 24`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 18- to 24-year-olds per region in 2008") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N08.Age1824.Reg.png", width = 5, height = 5)

#Plot: 25 to 29
N08.Age2529.region.plot <- ggplot(data = N08.Age.joined) +
  geom_sf(aes(fill = `25 to 29`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 25- to 29-year-olds per region in 2008") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N08.Age2529.Reg.png", width = 5, height = 5)

#Plot: 30 to 34
N08.Age3034.region.plot <- ggplot(data = N08.Age.joined) +
  geom_sf(aes(fill = `30 to 34`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 30- to 34-year-olds per region in 2008") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N08.Age3034.Reg.png", width = 5, height = 5)

#Plot: 35 to 39
N08.Age3539.region.plot <- ggplot(data = N08.Age.joined) +
  geom_sf(aes(fill = `35 to 39`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 35- to 39-year-olds per region in 2008") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N08.Age3539.Reg.png", width = 5, height = 5)

#Plot: 40 to 44
N08.Age4044.region.plot <- ggplot(data = N08.Age.joined) +
  geom_sf(aes(fill = `40 to 44`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 40- to 44-year-olds per region in 2008") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N08.Age4044.Reg.png", width = 5, height = 5)

#Plot: 45 to 49
N08.Age4549.region.plot <- ggplot(data = N08.Age.joined) +
  geom_sf(aes(fill = `45 to 49`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 45- to 49-year-olds per region in 2008") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N08.Age4549.Reg.png", width = 5, height = 5)

#Plot: 50 to 54
N08.Age5054.region.plot <- ggplot(data = N08.Age.joined) +
  geom_sf(aes(fill = `50 to 54`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 50- to 54-year-olds per region in 2008") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N08.Age5054.Reg.png", width = 5, height = 5)

#Plot: 55 to 59
N08.Age5559.region.plot <- ggplot(data = N08.Age.joined) +
  geom_sf(aes(fill = `55 to 59`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 55- to 59-year-olds per region in 2008") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N08.Age5559.Reg.png", width = 5, height = 5)

#Plot: 60 to 64
N08.Age6064.region.plot <- ggplot(data = N08.Age.joined) +
  geom_sf(aes(fill = `60 to 64`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 60- to 64-year-olds per region in 2008") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N08.Age6064.Reg.png", width = 5, height = 5)

#Plot: 65 to 69
N08.Age6569.region.plot <- ggplot(data = N08.Age.joined) +
  geom_sf(aes(fill = `65 to 69`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(4, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 65- to 69-year-olds per region in 2008") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N08.Age6569.Reg.png", width = 5, height = 5)

#Plot: 70 and above
N08.Age.70ab.region.plot <- ggplot(data = N08.Age.joined) +
  geom_sf(aes(fill = `70 and above`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 70-year-olds and above per region in 2008") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N08.Age70ab.Reg.png", width = 5, height = 5)


#Change the region column to "NAME" to match sf
names(N08_sex_sel)[names(N08_sex_sel) == "Region"] <- "NAME"

N08.Sex.joined <- regions %>%
  left_join(N08_sex_sel)

N08.Sex.joined$NAME <- as.factor(N08.Sex.joined$NAME)

#Plot: Male
N08.Sex.Male.region.plot <- ggplot(data = N08.Sex.joined) +
  geom_sf(aes(fill = `Male`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(45, 55), breaks = c(45, 50, 55), low = "#FF0000", high = "black") +
  ggtitle("Percentage of males per region in 2008") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N08.SexMale.Reg.png", width = 5, height = 5)


#2007
#Proportions
N07_age <- svyby(~AGE_P, ~REGION, NHIS07_DO, svymean, na.rm = TRUE)
N07_sex <- svyby(~SEX, ~REGION, NHIS07_DO, svymean, na.rm = TRUE)

#Age
N07_age_sel <- N07_age %>%
  select(1:12) %>%
  setNames(c("Region", "18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 and above")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Sex
N07_sex_sel <- N07_sex %>%
  select(1, 3) %>%
  setNames(c("Region", "Male")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))


#Change the region column to "NAME" to match sf
names(N07_age_sel)[names(N07_age_sel) == "Region"] <- "NAME"

N07.Age.joined <- regions %>%
  left_join(N07_age_sel)

N07.Age.joined$NAME <- as.factor(N07.Age.joined$NAME)

#Plot: 18 to 24
N07.Age1824.region.plot <- ggplot(data = N07.Age.joined) +
  geom_sf(aes(fill = `18 to 24`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 18- to 24-year-olds per region in 2007") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N07.Age1824.Reg.png", width = 5, height = 5)

#Plot: 25 to 29
N07.Age2529.region.plot <- ggplot(data = N07.Age.joined) +
  geom_sf(aes(fill = `25 to 29`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 25- to 29-year-olds per region in 2007") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N07.Age2529.Reg.png", width = 5, height = 5)

#Plot: 30 to 34
N07.Age3034.region.plot <- ggplot(data = N07.Age.joined) +
  geom_sf(aes(fill = `30 to 34`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 30- to 34-year-olds per region in 2007") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N07.Age3034.Reg.png", width = 5, height = 5)

#Plot: 35 to 39
N07.Age3539.region.plot <- ggplot(data = N07.Age.joined) +
  geom_sf(aes(fill = `35 to 39`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 35- to 39-year-olds per region in 2007") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N07.Age3539.Reg.png", width = 5, height = 5)

#Plot: 40 to 44
N07.Age4044.region.plot <- ggplot(data = N07.Age.joined) +
  geom_sf(aes(fill = `40 to 44`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 40- to 44-year-olds per region in 2007") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N07.Age4044.Reg.png", width = 5, height = 5)

#Plot: 45 to 49
N07.Age4549.region.plot <- ggplot(data = N07.Age.joined) +
  geom_sf(aes(fill = `45 to 49`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 45- to 49-year-olds per region in 2007") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N07.Age4549.Reg.png", width = 5, height = 5)

#Plot: 50 to 54
N07.Age5054.region.plot <- ggplot(data = N07.Age.joined) +
  geom_sf(aes(fill = `50 to 54`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 50- to 54-year-olds per region in 2007") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N07.Age5054.Reg.png", width = 5, height = 5)

#Plot: 55 to 59
N07.Age5559.region.plot <- ggplot(data = N07.Age.joined) +
  geom_sf(aes(fill = `55 to 59`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 55- to 59-year-olds per region in 2007") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N07.Age5559.Reg.png", width = 5, height = 5)

#Plot: 60 to 64
N07.Age6064.region.plot <- ggplot(data = N07.Age.joined) +
  geom_sf(aes(fill = `60 to 64`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 60- to 64-year-olds per region in 2007") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N07.Age6064.Reg.png", width = 5, height = 5)

#Plot: 65 to 69
N07.Age6569.region.plot <- ggplot(data = N07.Age.joined) +
  geom_sf(aes(fill = `65 to 69`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(4, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 65- to 69-year-olds per region in 2007") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N07.Age6569.Reg.png", width = 5, height = 5)

#Plot: 70 and above
N07.Age.70ab.region.plot <- ggplot(data = N07.Age.joined) +
  geom_sf(aes(fill = `70 and above`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 70-year-olds and above per region in 2007") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N07.Age70ab.Reg.png", width = 5, height = 5)


#Change the region column to "NAME" to match sf
names(N07_sex_sel)[names(N07_sex_sel) == "Region"] <- "NAME"

N07.Sex.joined <- regions %>%
  left_join(N07_sex_sel)

N07.Sex.joined$NAME <- as.factor(N07.Sex.joined$NAME)

#Plot: Male
N07.Sex.Male.region.plot <- ggplot(data = N07.Sex.joined) +
  geom_sf(aes(fill = `Male`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(45, 55), breaks = c(45, 50, 55), low = "#FF0000", high = "black") +
  ggtitle("Percentage of males per region in 2007") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N07.SexMale.Reg.png", width = 5, height = 5)


#2006
#Proportions
N06_age <- svyby(~AGE_P, ~REGION, NHIS06_DO, svymean, na.rm = TRUE)
N06_sex <- svyby(~SEX, ~REGION, NHIS06_DO, svymean, na.rm = TRUE)

#Age
N06_age_sel <- N06_age %>%
  select(1:12) %>%
  setNames(c("Region", "18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 and above")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Sex
N06_sex_sel <- N06_sex %>%
  select(1, 3) %>%
  setNames(c("Region", "Male")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))


#Change the region column to "NAME" to match sf
names(N06_age_sel)[names(N06_age_sel) == "Region"] <- "NAME"

N06.Age.joined <- regions %>%
  left_join(N06_age_sel)

N06.Age.joined$NAME <- as.factor(N06.Age.joined$NAME)

#Plot: 18 to 24
N06.Age1824.region.plot <- ggplot(data = N06.Age.joined) +
  geom_sf(aes(fill = `18 to 24`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 18- to 24-year-olds per region in 2006") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N06.Age1824.Reg.png", width = 5, height = 5)

#Plot: 25 to 29
N06.Age2529.region.plot <- ggplot(data = N06.Age.joined) +
  geom_sf(aes(fill = `25 to 29`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 25- to 29-year-olds per region in 2006") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N06.Age2529.Reg.png", width = 5, height = 5)

#Plot: 30 to 34
N06.Age3034.region.plot <- ggplot(data = N06.Age.joined) +
  geom_sf(aes(fill = `30 to 34`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 30- to 34-year-olds per region in 2006") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N06.Age3034.Reg.png", width = 5, height = 5)

#Plot: 35 to 39
N06.Age3539.region.plot <- ggplot(data = N06.Age.joined) +
  geom_sf(aes(fill = `35 to 39`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 35- to 39-year-olds per region in 2006") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N06.Age3539.Reg.png", width = 5, height = 5)

#Plot: 40 to 44
N06.Age4044.region.plot <- ggplot(data = N06.Age.joined) +
  geom_sf(aes(fill = `40 to 44`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 40- to 44-year-olds per region in 2006") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N06.Age4044.Reg.png", width = 5, height = 5)

#Plot: 45 to 49
N06.Age4549.region.plot <- ggplot(data = N06.Age.joined) +
  geom_sf(aes(fill = `45 to 49`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 45- to 49-year-olds per region in 2006") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N06.Age4549.Reg.png", width = 5, height = 5)

#Plot: 50 to 54
N06.Age5054.region.plot <- ggplot(data = N06.Age.joined) +
  geom_sf(aes(fill = `50 to 54`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 50- to 54-year-olds per region in 2006") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N06.Age5054.Reg.png", width = 5, height = 5)

#Plot: 55 to 59
N06.Age5559.region.plot <- ggplot(data = N06.Age.joined) +
  geom_sf(aes(fill = `55 to 59`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 55- to 59-year-olds per region in 2006") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N06.Age5559.Reg.png", width = 5, height = 5)

#Plot: 60 to 64
N06.Age6064.region.plot <- ggplot(data = N06.Age.joined) +
  geom_sf(aes(fill = `60 to 64`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 60- to 64-year-olds per region in 2006") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N06.Age6064.Reg.png", width = 5, height = 5)

#Plot: 65 to 69
N06.Age6569.region.plot <- ggplot(data = N06.Age.joined) +
  geom_sf(aes(fill = `65 to 69`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(4, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 65- to 69-year-olds per region in 2006") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N06.Age6569.Reg.png", width = 5, height = 5)

#Plot: 70 and above
N06.Age.70ab.region.plot <- ggplot(data = N06.Age.joined) +
  geom_sf(aes(fill = `70 and above`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 70-year-olds and above per region in 2006") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N06.Age70ab.Reg.png", width = 5, height = 5)


#Change the region column to "NAME" to match sf
names(N06_sex_sel)[names(N06_sex_sel) == "Region"] <- "NAME"

N06.Sex.joined <- regions %>%
  left_join(N06_sex_sel)

N06.Sex.joined$NAME <- as.factor(N06.Sex.joined$NAME)

#Plot: Male
N06.Sex.Male.region.plot <- ggplot(data = N06.Sex.joined) +
  geom_sf(aes(fill = `Male`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(45, 55), breaks = c(45, 50, 55), low = "#FF0000", high = "black") +
  ggtitle("Percentage of males per region in 2006") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N06.SexMale.Reg.png", width = 5, height = 5)


#2005
#Proportions
N05_age <- svyby(~AGE_P, ~REGION, NHIS05_DO, svymean, na.rm = TRUE)
N05_sex <- svyby(~SEX, ~REGION, NHIS05_DO, svymean, na.rm = TRUE)

#Age
N05_age_sel <- N05_age %>%
  select(1:12) %>%
  setNames(c("Region", "18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 and above")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Sex
N05_sex_sel <- N05_sex %>%
  select(1, 3) %>%
  setNames(c("Region", "Male")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))


#Change the region column to "NAME" to match sf
names(N05_age_sel)[names(N05_age_sel) == "Region"] <- "NAME"

N05.Age.joined <- regions %>%
  left_join(N05_age_sel)

N05.Age.joined$NAME <- as.factor(N05.Age.joined$NAME)

#Plot: 18 to 24
N05.Age1824.region.plot <- ggplot(data = N05.Age.joined) +
  geom_sf(aes(fill = `18 to 24`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 18- to 24-year-olds per region in 2005") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N05.Age1824.Reg.png", width = 5, height = 5)

#Plot: 25 to 29
N05.Age2529.region.plot <- ggplot(data = N05.Age.joined) +
  geom_sf(aes(fill = `25 to 29`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 25- to 29-year-olds per region in 2005") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N05.Age2529.Reg.png", width = 5, height = 5)

#Plot: 30 to 34
N05.Age3034.region.plot <- ggplot(data = N05.Age.joined) +
  geom_sf(aes(fill = `30 to 34`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 30- to 34-year-olds per region in 2005") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N05.Age3034.Reg.png", width = 5, height = 5)

#Plot: 35 to 39
N05.Age3539.region.plot <- ggplot(data = N05.Age.joined) +
  geom_sf(aes(fill = `35 to 39`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 35- to 39-year-olds per region in 2005") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N05.Age3539.Reg.png", width = 5, height = 5)

#Plot: 40 to 44
N05.Age4044.region.plot <- ggplot(data = N05.Age.joined) +
  geom_sf(aes(fill = `40 to 44`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 40- to 44-year-olds per region in 2005") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N05.Age4044.Reg.png", width = 5, height = 5)

#Plot: 45 to 49
N05.Age4549.region.plot <- ggplot(data = N05.Age.joined) +
  geom_sf(aes(fill = `45 to 49`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 45- to 49-year-olds per region in 2005") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N05.Age4549.Reg.png", width = 5, height = 5)

#Plot: 50 to 54
N05.Age5054.region.plot <- ggplot(data = N05.Age.joined) +
  geom_sf(aes(fill = `50 to 54`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 50- to 54-year-olds per region in 2005") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N05.Age5054.Reg.png", width = 5, height = 5)

#Plot: 55 to 59
N05.Age5559.region.plot <- ggplot(data = N05.Age.joined) +
  geom_sf(aes(fill = `55 to 59`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 55- to 59-year-olds per region in 2005") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N05.Age5559.Reg.png", width = 5, height = 5)

#Plot: 60 to 64
N05.Age6064.region.plot <- ggplot(data = N05.Age.joined) +
  geom_sf(aes(fill = `60 to 64`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 60- to 64-year-olds per region in 2005") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N05.Age6064.Reg.png", width = 5, height = 5)

#Plot: 65 to 69
N05.Age6569.region.plot <- ggplot(data = N05.Age.joined) +
  geom_sf(aes(fill = `65 to 69`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(3.8, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 65- to 69-year-olds per region in 2005") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N05.Age6569.Reg.png", width = 5, height = 5)

#Plot: 70 and above
N05.Age.70ab.region.plot <- ggplot(data = N05.Age.joined) +
  geom_sf(aes(fill = `70 and above`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 70-year-olds and above per region in 2005") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N05.Age70ab.Reg.png", width = 5, height = 5)


#Change the region column to "NAME" to match sf
names(N05_sex_sel)[names(N05_sex_sel) == "Region"] <- "NAME"

N05.Sex.joined <- regions %>%
  left_join(N05_sex_sel)

N05.Sex.joined$NAME <- as.factor(N05.Sex.joined$NAME)

#Plot: Male
N05.Sex.Male.region.plot <- ggplot(data = N05.Sex.joined) +
  geom_sf(aes(fill = `Male`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(45, 55), breaks = c(45, 50, 55), low = "#FF0000", high = "black") +
  ggtitle("Percentage of males per region in 2005") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N05.SexMale.Reg.png", width = 5, height = 5)


#2003

#Proportions
N03_age <- svyby(~AGE_P, ~REGION, NHIS03_DO, svymean, na.rm = TRUE)
N03_sex <- svyby(~SEX, ~REGION, NHIS03_DO, svymean, na.rm = TRUE)

#Age
N03_age_sel <- N03_age %>%
  select(1:12) %>%
  setNames(c("Region", "18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 and above")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Sex
N03_sex_sel <- N03_sex %>%
  select(1, 3) %>%
  setNames(c("Region", "Male")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))


#Change the region column to "NAME" to match sf
names(N03_age_sel)[names(N03_age_sel) == "Region"] <- "NAME"

N03.Age.joined <- regions %>%
  left_join(N03_age_sel)

N03.Age.joined$NAME <- as.factor(N03.Age.joined$NAME)

#Plot: 18 to 24
N03.Age1824.region.plot <- ggplot(data = N03.Age.joined) +
  geom_sf(aes(fill = `18 to 24`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 18- to 24-year-olds per region in 2003") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N03.Age1824.Reg.png", width = 5, height = 5)

#Plot: 25 to 29
N03.Age2529.region.plot <- ggplot(data = N03.Age.joined) +
  geom_sf(aes(fill = `25 to 29`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 25- to 29-year-olds per region in 2003") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N03.Age2529.Reg.png", width = 5, height = 5)

#Plot: 30 to 34
N03.Age3034.region.plot <- ggplot(data = N03.Age.joined) +
  geom_sf(aes(fill = `30 to 34`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 30- to 34-year-olds per region in 2003") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N03.Age3034.Reg.png", width = 5, height = 5)

#Plot: 35 to 39
N03.Age3539.region.plot <- ggplot(data = N03.Age.joined) +
  geom_sf(aes(fill = `35 to 39`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 35- to 39-year-olds per region in 2003") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N03.Age3539.Reg.png", width = 5, height = 5)

#Plot: 40 to 44
N03.Age4044.region.plot <- ggplot(data = N03.Age.joined) +
  geom_sf(aes(fill = `40 to 44`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 40- to 44-year-olds per region in 2003") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N03.Age4044.Reg.png", width = 5, height = 5)

#Plot: 45 to 49
N03.Age4549.region.plot <- ggplot(data = N03.Age.joined) +
  geom_sf(aes(fill = `45 to 49`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 45- to 49-year-olds per region in 2003") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N03.Age4549.Reg.png", width = 5, height = 5)

#Plot: 50 to 54
N03.Age5054.region.plot <- ggplot(data = N03.Age.joined) +
  geom_sf(aes(fill = `50 to 54`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 50- to 54-year-olds per region in 2003") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N03.Age5054.Reg.png", width = 5, height = 5)

#Plot: 55 to 59
N03.Age5559.region.plot <- ggplot(data = N03.Age.joined) +
  geom_sf(aes(fill = `55 to 59`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 55- to 59-year-olds per region in 2003") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N03.Age5559.Reg.png", width = 5, height = 5)

#Plot: 60 to 64
N03.Age6064.region.plot <- ggplot(data = N03.Age.joined) +
  geom_sf(aes(fill = `60 to 64`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 60- to 64-year-olds per region in 2003") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N03.Age6064.Reg.png", width = 5, height = 5)

#Plot: 65 to 69
N03.Age6569.region.plot <- ggplot(data = N03.Age.joined) +
  geom_sf(aes(fill = `65 to 69`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(4, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 18- to 24-year-olds per region in 2003") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N03.Age6569.Reg.png", width = 5, height = 5)

#Plot: 70 and above
N03.Age.70ab.region.plot <- ggplot(data = N03.Age.joined) +
  geom_sf(aes(fill = `70 and above`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 70-year-olds and above per region in 2003") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N03.Age70ab.Reg.png", width = 5, height = 5)


#Change the region column to "NAME" to match sf
names(N03_sex_sel)[names(N03_sex_sel) == "Region"] <- "NAME"

N03.Sex.joined <- regions %>%
  left_join(N03_sex_sel)

N03.Sex.joined$NAME <- as.factor(N03.Sex.joined$NAME)

#Plot: Male
N03.Sex.Male.region.plot <- ggplot(data = N03.Sex.joined) +
  geom_sf(aes(fill = `Male`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(45, 55), breaks = c(45, 50, 55), low = "#FF0000", high = "black") +
  ggtitle("Percentage of males per region in 2003") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N03.SexMale.Reg.png", width = 5, height = 5)


#2002
#Proportions
N02_age <- svyby(~AGE_P, ~REGION, NHIS02_DO, svymean, na.rm = TRUE)
N02_sex <- svyby(~SEX, ~REGION, NHIS02_DO, svymean, na.rm = TRUE)

#Age
N02_age_sel <- N02_age %>%
  select(1:12) %>%
  setNames(c("Region", "18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 and above")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Sex
N02_sex_sel <- N02_sex %>%
  select(1, 3) %>%
  setNames(c("Region", "Male")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))


#Change the region column to "NAME" to match sf
names(N02_age_sel)[names(N02_age_sel) == "Region"] <- "NAME"

N02.Age.joined <- regions %>%
  left_join(N02_age_sel)

N02.Age.joined$NAME <- as.factor(N02.Age.joined$NAME)

#Plot: 18 to 24
N02.Age1824.region.plot <- ggplot(data = N02.Age.joined) +
  geom_sf(aes(fill = `18 to 24`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 18- to 24-year-olds per region in 2002") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N02.Age1824.Reg.png", width = 5, height = 5)

#Plot: 25 to 29
N02.Age2529.region.plot <- ggplot(data = N02.Age.joined) +
  geom_sf(aes(fill = `25 to 29`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 25- to 29-year-olds per region in 2002") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N02.Age2529.Reg.png", width = 5, height = 5)

#Plot: 30 to 34
N02.Age3034.region.plot <- ggplot(data = N02.Age.joined) +
  geom_sf(aes(fill = `30 to 34`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 30- to 34-year-olds per region in 2002") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N02.Age3034.Reg.png", width = 5, height = 5)

#Plot: 35 to 39
N02.Age3539.region.plot <- ggplot(data = N02.Age.joined) +
  geom_sf(aes(fill = `35 to 39`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 35- to 39-year-olds per region in 2002") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N02.Age3539.Reg.png", width = 5, height = 5)

#Plot: 40 to 44
N02.Age4044.region.plot <- ggplot(data = N02.Age.joined) +
  geom_sf(aes(fill = `40 to 44`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 40- to 44-year-olds per region in 2002") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N02.Age4044.Reg.png", width = 5, height = 5)

#Plot: 45 to 49
N02.Age4549.region.plot <- ggplot(data = N02.Age.joined) +
  geom_sf(aes(fill = `45 to 49`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 45- to 49-year-olds per region in 2002") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N02.Age4549.Reg.png", width = 5, height = 5)

#Plot: 50 to 54
N02.Age5054.region.plot <- ggplot(data = N02.Age.joined) +
  geom_sf(aes(fill = `50 to 54`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 50- to 54-year-olds per region in 2002") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N02.Age5054.Reg.png", width = 5, height = 5)

#Plot: 55 to 59
N02.Age5559.region.plot <- ggplot(data = N02.Age.joined) +
  geom_sf(aes(fill = `55 to 59`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 55- to 59-year-olds per region in 2002") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N02.Age5559.Reg.png", width = 5, height = 5)

#Plot: 60 to 64
N02.Age6064.region.plot <- ggplot(data = N02.Age.joined) +
  geom_sf(aes(fill = `60 to 64`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(4, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 60- to 64-year-olds per region in 2002") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N02.Age6064.Reg.png", width = 5, height = 5)

#Plot: 65 to 69
N02.Age6569.region.plot <- ggplot(data = N02.Age.joined) +
  geom_sf(aes(fill = `65 to 69`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(4, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 65- to 69-year-olds per region in 2002") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N02.Age6569.Reg.png", width = 5, height = 5)

#Plot: 70 and above
N02.Age.70ab.region.plot <- ggplot(data = N02.Age.joined) +
  geom_sf(aes(fill = `70 and above`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 70-year-olds and above per region in 2002") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N02.Age70ab.Reg.png", width = 5, height = 5)


#Change the region column to "NAME" to match sf
names(N02_sex_sel)[names(N02_sex_sel) == "Region"] <- "NAME"

N02.Sex.joined <- regions %>%
  left_join(N02_sex_sel)

N02.Sex.joined$NAME <- as.factor(N02.Sex.joined$NAME)

#Plot: Male
N02.Sex.Male.region.plot <- ggplot(data = N02.Sex.joined) +
  geom_sf(aes(fill = `Male`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(45, 55), breaks = c(45, 50, 55), low = "#FF0000", high = "black") +
  ggtitle("Percentage of males per region in 2002") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N02.SexMale.Reg.png", width = 5, height = 5)


#2001
#Proportions
N01_age <- svyby(~AGE_P, ~REGION, NHIS01_DO, svymean, na.rm = TRUE)
N01_sex <- svyby(~SEX, ~REGION, NHIS01_DO, svymean, na.rm = TRUE)

#Age
N01_age_sel <- N01_age %>%
  select(1:12) %>%
  setNames(c("Region", "18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 and above")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Sex
N01_sex_sel <- N01_sex %>%
  select(1, 3) %>%
  setNames(c("Region", "Male")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))


#Change the region column to "NAME" to match sf
names(N01_age_sel)[names(N01_age_sel) == "Region"] <- "NAME"

N01.Age.joined <- regions %>%
  left_join(N01_age_sel)

N01.Age.joined$NAME <- as.factor(N01.Age.joined$NAME)

#Plot: 18 to 24
N01.Age1824.region.plot <- ggplot(data = N01.Age.joined) +
  geom_sf(aes(fill = `18 to 24`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 18- to 24-year-olds per region in 2001") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N01.Age1824.Reg.png", width = 5, height = 5)

#Plot: 25 to 29
N01.Age2529.region.plot <- ggplot(data = N01.Age.joined) +
  geom_sf(aes(fill = `25 to 29`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 25- to 29-year-olds per region in 2001") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N01.Age2529.Reg.png", width = 5, height = 5)

#Plot: 30 to 34
N01.Age3034.region.plot <- ggplot(data = N01.Age.joined) +
  geom_sf(aes(fill = `30 to 34`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 30- to 34-year-olds per region in 2001") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N01.Age3034.Reg.png", width = 5, height = 5)

#Plot: 35 to 39
N01.Age3539.region.plot <- ggplot(data = N01.Age.joined) +
  geom_sf(aes(fill = `35 to 39`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 35- to 39-year-olds per region in 2001") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N01.Age3539.Reg.png", width = 5, height = 5)

#Plot: 40 to 44
N01.Age4044.region.plot <- ggplot(data = N01.Age.joined) +
  geom_sf(aes(fill = `40 to 44`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 40- to 44-year-olds per region in 2001") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N01.Age4044.Reg.png", width = 5, height = 5)

#Plot: 45 to 49
N01.Age4549.region.plot <- ggplot(data = N01.Age.joined) +
  geom_sf(aes(fill = `45 to 49`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 45- to 49-year-olds per region in 2001") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N01.Age4549.Reg.png", width = 5, height = 5)

#Plot: 50 to 54
N01.Age5054.region.plot <- ggplot(data = N01.Age.joined) +
  geom_sf(aes(fill = `50 to 54`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 50- to 54-year-olds per region in 2001") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N01.Age5054.Reg.png", width = 5, height = 5)

#Plot: 55 to 59
N01.Age5559.region.plot <- ggplot(data = N01.Age.joined) +
  geom_sf(aes(fill = `55 to 59`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 55- to 59-year-olds per region in 2001") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N01.Age5559.Reg.png", width = 5, height = 5)

#Plot: 60 to 64
N01.Age6064.region.plot <- ggplot(data = N01.Age.joined) +
  geom_sf(aes(fill = `60 to 64`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 60- to 64-year-olds per region in 2001") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N01.Age6064.Reg.png", width = 5, height = 5)

#Plot: 65 to 69
N01.Age6569.region.plot <- ggplot(data = N01.Age.joined) +
  geom_sf(aes(fill = `65 to 69`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(4, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 65- to 69-year-olds per region in 2001") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N01.Age6569.Reg.png", width = 5, height = 5)

#Plot: 70 and above
N01.Age.70ab.region.plot <- ggplot(data = N01.Age.joined) +
  geom_sf(aes(fill = `70 and above`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 70-year-olds and above per region in 2001") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N01.Age70ab.Reg.png", width = 5, height = 5)


#Change the region column to "NAME" to match sf
names(N01_sex_sel)[names(N01_sex_sel) == "Region"] <- "NAME"

N01.Sex.joined <- regions %>%
  left_join(N01_sex_sel)

N01.Sex.joined$NAME <- as.factor(N01.Sex.joined$NAME)

#Plot: Male
N01.Sex.Male.region.plot <- ggplot(data = N01.Sex.joined) +
  geom_sf(aes(fill = `Male`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(45, 55), breaks = c(45, 50, 55), low = "#FF0000", high = "black") +
  ggtitle("Percentage of males per region in 2001") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N01.SexMale.Reg.png", width = 5, height = 5)


#2000
#Proportions
N00_age <- svyby(~AGE_P, ~REGION, NHIS00_DO, svymean, na.rm = TRUE)
N00_sex <- svyby(~SEX, ~REGION, NHIS00_DO, svymean, na.rm = TRUE)

#Age
N00_age_sel <- N00_age %>%
  select(1:12) %>%
  setNames(c("Region", "18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 and above")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Sex
N00_sex_sel <- N00_sex %>%
  select(1, 3) %>%
  setNames(c("Region", "Male")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Change the region column to "NAME" to match sf
names(N00_age_sel)[names(N00_age_sel) == "Region"] <- "NAME"

N00.Age.joined <- regions %>%
  left_join(N00_age_sel)

N00.Age.joined$NAME <- as.factor(N00.Age.joined$NAME)

#Plot: 18 to 24
N00.Age1824.region.plot <- ggplot(data = N00.Age.joined) +
  geom_sf(aes(fill = `18 to 24`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 18- to 24-year-olds per region in 2000") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N00.Age1824.Reg.png", width = 5, height = 5)

#Plot: 25 to 29
N00.Age2529.region.plot <- ggplot(data = N00.Age.joined) +
  geom_sf(aes(fill = `25 to 29`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 25- to 29-year-olds per region in 2000") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N00.Age2529.Reg.png", width = 5, height = 5)

#Plot: 30 to 34
N00.Age3034.region.plot <- ggplot(data = N00.Age.joined) +
  geom_sf(aes(fill = `30 to 34`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 30- to 34-year-olds per region in 2000") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N00.Age3034.Reg.png", width = 5, height = 5)

#Plot: 35 to 39
N00.Age3539.region.plot <- ggplot(data = N00.Age.joined) +
  geom_sf(aes(fill = `35 to 39`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 35- to 39-year-olds per region in 2000") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N00.Age3539.Reg.png", width = 5, height = 5)

#Plot: 40 to 44
N00.Age4044.region.plot <- ggplot(data = N00.Age.joined) +
  geom_sf(aes(fill = `40 to 44`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 40- to 44-year-olds per region in 2000") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N00.Age4044.Reg.png", width = 5, height = 5)

#Plot: 45 to 49
N00.Age4549.region.plot <- ggplot(data = N00.Age.joined) +
  geom_sf(aes(fill = `45 to 49`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 45- to 49-year-olds per region in 2000") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N00.Age4549.Reg.png", width = 5, height = 5)

#Plot: 50 to 54
N00.Age5054.region.plot <- ggplot(data = N00.Age.joined) +
  geom_sf(aes(fill = `50 to 54`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 50- to 54-year-olds per region in 2000") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N00.Age5054.Reg.png", width = 5, height = 5)

#Plot: 55 to 59
N00.Age5559.region.plot <- ggplot(data = N00.Age.joined) +
  geom_sf(aes(fill = `55 to 59`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 55- to 59-year-olds per region in 2000") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N00.Age5559.Reg.png", width = 5, height = 5)

#Plot: 60 to 64
N00.Age6064.region.plot <- ggplot(data = N00.Age.joined) +
  geom_sf(aes(fill = `60 to 64`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(4, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 60- to 64-year-olds per region in 2000") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N00.Age6064.Reg.png", width = 5, height = 5)

#Plot: 65 to 69
N00.Age6569.region.plot <- ggplot(data = N00.Age.joined) +
  geom_sf(aes(fill = `65 to 69`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(4, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 65- to 69-year-olds per region in 2000") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N00.Age6569.Reg.png", width = 5, height = 5)

#Plot: 70 and above
N00.Age.70ab.region.plot <- ggplot(data = N00.Age.joined) +
  geom_sf(aes(fill = `70 and above`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 70-year-olds and above per region in 2000") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N00.Age70ab.Reg.png", width = 5, height = 5)


#Change the region column to "NAME" to match sf
names(N00_sex_sel)[names(N00_sex_sel) == "Region"] <- "NAME"

N00.Sex.joined <- regions %>%
  left_join(N00_sex_sel)

N00.Sex.joined$NAME <- as.factor(N00.Sex.joined$NAME)

#Plot: Male
N00.Sex.Male.region.plot <- ggplot(data = N00.Sex.joined) +
  geom_sf(aes(fill = `Male`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(45, 55), breaks = c(45, 50, 55), low = "#FF0000", high = "black") +
  ggtitle("Percentage of males per region in 2000") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N00.SexMale.Reg.png", width = 5, height = 5)


#1999
#Proportions
N99_age <- svyby(~AGE_P, ~REGION, NHIS99_DO, svymean, na.rm = TRUE)
N99_sex <- svyby(~SEX, ~REGION, NHIS99_DO, svymean, na.rm = TRUE)

#Age
N99_age_sel <- N99_age %>%
  select(1:12) %>%
  setNames(c("Region", "18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 and above")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Sex
N99_sex_sel <- N99_sex %>%
  select(1, 3) %>%
  setNames(c("Region", "Male")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))


#Change the region column to "NAME" to match sf
names(N99_age_sel)[names(N99_age_sel) == "Region"] <- "NAME"

N99.Age.joined <- regions %>%
  left_join(N99_age_sel)

N99.Age.joined$NAME <- as.factor(N99.Age.joined$NAME)

#Plot: 18 to 24
N99.Age1824.region.plot <- ggplot(data = N99.Age.joined) +
  geom_sf(aes(fill = `18 to 24`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 18- to 24-year-olds per region in 1999") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N99.Age1824.Reg.png", width = 5, height = 5)

#Plot: 25 to 29
N99.Age2529.region.plot <- ggplot(data = N99.Age.joined) +
  geom_sf(aes(fill = `25 to 29`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 25- to 29-year-olds per region in 1999") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N99.Age2529.Reg.png", width = 5, height = 5)

#Plot: 30 to 34
N99.Age3034.region.plot <- ggplot(data = N99.Age.joined) +
  geom_sf(aes(fill = `30 to 34`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 30- to 34-year-olds per region in 1999") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N99.Age3034.Reg.png", width = 5, height = 5)

#Plot: 35 to 39
N99.Age3539.region.plot <- ggplot(data = N99.Age.joined) +
  geom_sf(aes(fill = `35 to 39`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 35- to 39-year-olds per region in 1999") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N99.Age3539.Reg.png", width = 5, height = 5)

#Plot: 40 to 44
N99.Age4044.region.plot <- ggplot(data = N99.Age.joined) +
  geom_sf(aes(fill = `40 to 44`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 40- to 44-year-olds per region in 1999") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N99.Age4044.Reg.png", width = 5, height = 5)

#Plot: 45 to 49
N99.Age4549.region.plot <- ggplot(data = N99.Age.joined) +
  geom_sf(aes(fill = `45 to 49`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 45- to 49-year-olds per region in 1999") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N99.Age4549.Reg.png", width = 5, height = 5)

#Plot: 50 to 54
N99.Age5054.region.plot <- ggplot(data = N99.Age.joined) +
  geom_sf(aes(fill = `50 to 54`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 50- to 54-year-olds per region in 1999") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N99.Age5054.Reg.png", width = 5, height = 5)

#Plot: 55 to 59
N99.Age5559.region.plot <- ggplot(data = N99.Age.joined) +
  geom_sf(aes(fill = `55 to 59`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 55- to 59-year-olds per region in 1999") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N99.Age5559.Reg.png", width = 5, height = 5)

#Plot: 60 to 64
N99.Age6064.region.plot <- ggplot(data = N99.Age.joined) +
  geom_sf(aes(fill = `60 to 64`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(4, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 18- to 24-year-olds per region in 1999") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N99.Age6064.Reg.png", width = 5, height = 5)

#Plot: 65 to 69
N99.Age6569.region.plot <- ggplot(data = N99.Age.joined) +
  geom_sf(aes(fill = `65 to 69`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(4, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 65- to 69-year-olds per region in 1999") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N99.Age6569.Reg.png", width = 5, height = 5)

#Plot: 70 and above
N99.Age.70ab.region.plot <- ggplot(data = N99.Age.joined) +
  geom_sf(aes(fill = `70 and above`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 70-year-olds and above per region in 1999") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N99.Age70ab.Reg.png", width = 5, height = 5)


#Change the region column to "NAME" to match sf
names(N99_sex_sel)[names(N99_sex_sel) == "Region"] <- "NAME"

N99.Sex.joined <- regions %>%
  left_join(N99_sex_sel)

N99.Sex.joined$NAME <- as.factor(N99.Sex.joined$NAME)

#Plot: Male
N99.Sex.Male.region.plot <- ggplot(data = N99.Sex.joined) +
  geom_sf(aes(fill = `Male`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(45, 55), breaks = c(45, 50, 55), low = "#FF0000", high = "black") +
  ggtitle("Percentage of males per region in 1999") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N99.SexMale.Reg.png", width = 5, height = 5)


#1998
#Proportions
N98_age <- svyby(~AGE_P, ~REGION, NHIS98_DO, svymean, na.rm = TRUE)
N98_sex <- svyby(~SEX, ~REGION, NHIS98_DO, svymean, na.rm = TRUE)

#Age
N98_age_sel <- N98_age %>%
  select(1:12) %>%
  setNames(c("Region", "18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 and above")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Sex
N98_sex_sel <- N98_sex %>%
  select(1, 3) %>%
  setNames(c("Region", "Male")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Change the region column to "NAME" to match sf
names(N98_age_sel)[names(N98_age_sel) == "Region"] <- "NAME"

N98.Age.joined <- regions %>%
  left_join(N98_age_sel)

N98.Age.joined$NAME <- as.factor(N98.Age.joined$NAME)

#Plot: 18 to 24
N98.Age1824.region.plot <- ggplot(data = N98.Age.joined) +
  geom_sf(aes(fill = `18 to 24`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 18- to 24-year-olds per region in 1998") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N98.Age1824.Reg.png", width = 5, height = 5)

#Plot: 25 to 29
N98.Age2529.region.plot <- ggplot(data = N98.Age.joined) +
  geom_sf(aes(fill = `25 to 29`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 25- to 29-year-olds per region in 1998") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N98.Age2529.Reg.png", width = 5, height = 5)

#Plot: 30 to 34
N98.Age3034.region.plot <- ggplot(data = N98.Age.joined) +
  geom_sf(aes(fill = `30 to 34`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 30- to 34-year-olds per region in 1998") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N98.Age3034.Reg.png", width = 5, height = 5)

#Plot: 35 to 39
N98.Age3539.region.plot <- ggplot(data = N98.Age.joined) +
  geom_sf(aes(fill = `35 to 39`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 35- to 39-year-olds per region in 1998") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N98.Age3539.Reg.png", width = 5, height = 5)

#Plot: 40 to 44
N98.Age4044.region.plot <- ggplot(data = N98.Age.joined) +
  geom_sf(aes(fill = `40 to 44`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 40- to 44-year-olds per region in 1998") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N98.Age4044.Reg.png", width = 5, height = 5)

#Plot: 45 to 49
N98.Age4549.region.plot <- ggplot(data = N98.Age.joined) +
  geom_sf(aes(fill = `45 to 49`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 45- to 49-year-olds per region in 1998") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N98.Age4549.Reg.png", width = 5, height = 5)

#Plot: 50 to 54
N98.Age5054.region.plot <- ggplot(data = N98.Age.joined) +
  geom_sf(aes(fill = `50 to 54`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 50- to 54-year-olds per region in 1998") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N98.Age5054.Reg.png", width = 5, height = 5)

#Plot: 55 to 59
N98.Age5559.region.plot <- ggplot(data = N98.Age.joined) +
  geom_sf(aes(fill = `55 to 59`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 55- to 59-year-olds per region in 1998") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N98.Age5559.Reg.png", width = 5, height = 5)

#Plot: 60 to 64
N98.Age6064.region.plot <- ggplot(data = N98.Age.joined) +
  geom_sf(aes(fill = `60 to 64`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(4, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 60- to 64-year-olds per region in 1998") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N98.Age6064.Reg.png", width = 5, height = 5)

#Plot: 65 to 69
N98.Age6569.region.plot <- ggplot(data = N98.Age.joined) +
  geom_sf(aes(fill = `65 to 69`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(4, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 65- to 69-year-olds per region in 1998") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N98.Age6569.Reg.png", width = 5, height = 5)

#Plot: 70 and above
N98.Age.70ab.region.plot <- ggplot(data = N98.Age.joined) +
  geom_sf(aes(fill = `70 and above`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 70-year-olds and above per region in 1998") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N98.Age70ab.Reg.png", width = 5, height = 5)


#Change the region column to "NAME" to match sf
names(N98_sex_sel)[names(N98_sex_sel) == "Region"] <- "NAME"

N98.Sex.joined <- regions %>%
  left_join(N98_sex_sel)

N98.Sex.joined$NAME <- as.factor(N98.Sex.joined$NAME)

#Plot: Male
N98.Sex.Male.region.plot <- ggplot(data = N98.Sex.joined) +
  geom_sf(aes(fill = `Male`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(45, 55), breaks = c(45, 50, 55), low = "#FF0000", high = "black") +
  ggtitle("Percentage of males per region in 1998") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N98.SexMale.Reg.png", width = 5, height = 5)

#Proportions
N97_age <- svyby(~AGE_P, ~REGION, NHIS97_DO, svymean, na.rm = TRUE)
N97_sex <- svyby(~SEX, ~REGION, NHIS97_DO, svymean, na.rm = TRUE)

#Age
N97_age_sel <- N97_age %>%
  select(1:12) %>%
  setNames(c("Region", "18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 and above")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Sex
N97_sex_sel <- N97_sex %>%
  select(1, 3) %>%
  setNames(c("Region", "Male")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))


#Change the region column to "NAME" to match sf
names(N97_age_sel)[names(N97_age_sel) == "Region"] <- "NAME"

N97.Age.joined <- regions %>%
  left_join(N97_age_sel)

N97.Age.joined$NAME <- as.factor(N97.Age.joined$NAME)

#Plot: 18 to 24
N97.Age1824.region.plot <- ggplot(data = N97.Age.joined) +
  geom_sf(aes(fill = `18 to 24`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 18- to 24-year-olds per region in 1997") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N97.Age1824.Reg.png", width = 5, height = 5)

#Plot: 25 to 29
N97.Age2529.region.plot <- ggplot(data = N97.Age.joined) +
  geom_sf(aes(fill = `25 to 29`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 25- to 29-year-olds per region in 1997") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N97.Age2529.Reg.png", width = 5, height = 5)

#Plot: 30 to 34
N97.Age3034.region.plot <- ggplot(data = N97.Age.joined) +
  geom_sf(aes(fill = `30 to 34`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 30- to 34-year-olds per region in 1997") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N97.Age3034.Reg.png", width = 5, height = 5)

#Plot: 35 to 39
N97.Age3539.region.plot <- ggplot(data = N97.Age.joined) +
  geom_sf(aes(fill = `35 to 39`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 35- to 39-year-olds per region in 1997") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N97.Age3539.Reg.png", width = 5, height = 5)

#Plot: 40 to 44
N97.Age4044.region.plot <- ggplot(data = N97.Age.joined) +
  geom_sf(aes(fill = `40 to 44`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 40- to 44-year-olds per region in 1997") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N97.Age4044.Reg.png", width = 5, height = 5)

#Plot: 45 to 49
N97.Age4549.region.plot <- ggplot(data = N97.Age.joined) +
  geom_sf(aes(fill = `45 to 49`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 45- to 49-year-olds per region in 1997") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N97.Age4549.Reg.png", width = 5, height = 5)

#Plot: 50 to 54
N97.Age5054.region.plot <- ggplot(data = N97.Age.joined) +
  geom_sf(aes(fill = `50 to 54`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 50- to 54-year-olds per region in 1997") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N97.Age5054.Reg.png", width = 5, height = 5)

#Plot: 55 to 59
N97.Age5559.region.plot <- ggplot(data = N97.Age.joined) +
  geom_sf(aes(fill = `55 to 59`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 55- to 59-year-olds per region in 1997") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N97.Age5559.Reg.png", width = 5, height = 5)

#Plot: 60 to 64
N97.Age6064.region.plot <- ggplot(data = N97.Age.joined) +
  geom_sf(aes(fill = `60 to 64`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(4, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 60- to 64-year-olds per region in 1997") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N97.Age6064.Reg.png", width = 5, height = 5)

#Plot: 65 to 69
N97.Age6569.region.plot <- ggplot(data = N97.Age.joined) +
  geom_sf(aes(fill = `65 to 69`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(3.5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 65- to 69-year-olds per region in 1997") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N97.Age6569.Reg.png", width = 5, height = 5)

#Plot: 70 and above
N97.Age.70ab.region.plot <- ggplot(data = N97.Age.joined) +
  geom_sf(aes(fill = `70 and above`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") +
  ggtitle("Percentage of 70-year-olds and above per region in 1997") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N97.Age70ab.Reg.png", width = 5, height = 5)


#Change the region column to "NAME" to match sf
names(N97_sex_sel)[names(N97_sex_sel) == "Region"] <- "NAME"

N97.Sex.joined <- regions %>%
  left_join(N97_sex_sel)

N97.Sex.joined$NAME <- as.factor(N97.Sex.joined$NAME)

#Plot: Male
N97.Sex.Male.region.plot <- ggplot(data = N97.Sex.joined) +
  geom_sf(aes(fill = `Male`), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  labs(fill = "Percentage (%)") +
  scale_fill_continuous(limits = c(45, 55), breaks = c(45, 50, 55), low = "#FF0000", high = "black") +
  ggtitle("Percentage of males per region in 1997") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave("N97.SexMale.Reg.png", width = 5, height = 5)

