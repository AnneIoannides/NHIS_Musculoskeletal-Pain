#Anne Elizabeth Ioannides
#For the qualification of: PhD - Physiology, University of the Witwatersrand, South Africa

#National Health Interview Survey (NHIS) - Low back pain analysis

#The data cleaning methods are exactly the same as those used for joint symptoms, resulting in identical design objects used for both NHIS-based musculoskeletal pain analyses. One copy of this design object is available on Github

#Please note that the the script contains large data files, and this code may need to be run in pieces, depending on your computer's ram and processing power

#load packages
library(haven)
library(foreign)
library(SAScii)
library(tidyverse)
library(survey)
library(gdata)
library(ggplot2)
library(geofacet)
library(sf)
library(viridis)
library(viridisLite)

#download regional shape file for spatial analyses
#I downloaded the shapefile from the US census bureau website, link: https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_region_500k.zip . However, due to http status not being modified, I had to download manually, and read the file in from my PC.
#When running this code, please remove my file path and enter yours after downloading the file from the link above. The shape file itself is also available from my NHIS GitHub repository

# = Some quick admin = #


#download regional shape file for spatial analyses
#I downloaded the shapefile from the US census bureau website, link: https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_region_500k.zip . However, due to http status not being modified, I had to download manually, and read the file in from my PC.

# *****IMPORTANT***** : When running this code, please remove my file path and enter yours after downloading the file from the link above. 

#The shape file folder and co is also available on my DropBox: https://www.dropbox.com/sh/rksa1sc3r2bjd2q/AACqQrmOqYFL5Mrn8rLTNQB6a?dl=0

#Pull shape file in
regions <- read_sf("C:\\Users\\Anne\\Desktop\\NHIS\\Regional shape files\\cb_2018_us_region_500k.shp")

#some quick checks
names(regions)
glimpse(regions)
as_tibble(regions)
class(regions)

#Region dataframe for geofacets
USregions <- data.frame(
  name_regions = c("Northeast", "Midwest", "West", "South"),
  code_reg = c("Northeast", "Midwest", "West", "South"),
  row = c(1, 1, 1, 2),
  col = c(3, 2, 1, 2),
  stringsAsFactors = FALSE
)



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



#________________________________________________________________________________________

# = Begin analysis = #


# == 2018 == #

#1. Overall prevalence
N18_LBP <- svymean(~factor(PAINLB), 
                   NHIS18_DO, 
                   na.rm = TRUE)
N18_LBP.c <- N18_LBP %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N18_LBP_ci <- confint(N18_LBP) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
N18.LBP <- bind_cols(N18_LBP.c, N18_LBP_ci)
#remove LB = 0
N18.LBP <- N18.LBP[-c(1), ] #final proportion, se & 95% ci
#save
#write.csv(N18.LBP, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N18.LBP.csv")


#2. Spatial analysis, by region

#I)   Overall population
#i)   regional prevalence for the overall population
N18_LBP_reg <- svyby(formula = ~PAINLB,
                     by = ~REGION,
                     design = NHIS18_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N18_LBP_reg.c <- N18_LBP_reg %>%
  select(1, 3, 5) %>%
  setNames(c("Region", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N18_LBP_reg.ci <- confint(N18_LBP_reg) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N18_LBP_reg.ci <- N18_LBP_reg.ci[-c(1:4), ]
#join proportion and ci
N18.LBP.region <- bind_cols(N18_LBP_reg.c, N18_LBP_reg.ci) #final proportion, se & 95% ci
#save
#write.csv(N18.LBP.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N18.LBP.region.csv")

#Change the region column to "NAME" to match sf
names(N18.LBP.region)[names(N18.LBP.region) == "Region"] <- "NAME"

N18.LBP.joined <- regions %>%
  left_join(N18.LBP.region)

N18.LBP.joined$NAME <- as.factor(N18.LBP.joined$NAME)

#Plot
LBPregplot18 <- ggplot(data = N18.LBP.joined) +
  geom_sf(aes(fill = Proportion), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  scale_fill_viridis_c(name = "Prevalence (%)", option = "viridis", direction = -1, limits = c(25, 35), breaks = c(25, 30, 35)) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  ggtitle("Regional prevalence (%) of US residents with low back pain in 2018") +
  theme(plot.title = element_text(size = 10, hjust = 0.5))
ggsave("N18.LBP.regchor.png", width = 5, height = 5)


#ii)  logistic regression by region
N18.LBP.reg.glm <- svyglm(relevel(PAINLB, ref = "0")~ REGION + AGE_P,
                          family = quasibinomial,
                          design = NHIS18_DO)
summary(N18.LBP.reg.glm)
exp(cbind(OR=coef(N18.LBP.reg.glm), confint(N18.LBP.reg.glm)))


#II)  prevalence by region, by age

#i)  age-specific regional prevalance
N18_LBP_reg.age <- svyby(formula = ~PAINLB,
                         by = ~REGION + AGE_P,
                         design = NHIS18_DO,
                         FUN = svymean,
                         na.rm = TRUE)
N18_LBP_reg.age.c <- N18_LBP_reg.age %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Age", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Age = as.character(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N18_LBP_reg.age.ci <- confint(N18_LBP_reg.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N18_LBP_reg.age.ci <- N18_LBP_reg.age.ci[-c(1:44), ]
#join proportion and ci
N18.LBP.age.region <- bind_cols(N18_LBP_reg.age.c, N18_LBP_reg.age.ci) #final prevalence, se & 95% ci
#save
#write.csv(N18.LBP.age.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N18.LBP.region.age.csv")

#Geofacet - Age & Region
LBP.Age_Reg18 <- ggplot(N18.LBP.age.region, aes(Age, Proportion)) +
  geom_bar(colour = "black", stat = "identity", fill = "#6600CC") +
  coord_flip() +
  facet_geo(~Region,
            grid = USregions) +
  theme_bw() +
  theme(text = element_text(colour = "black", size = 20)) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 17)) +
  xlab("Age group (years)") +
  geom_errorbar(aes(ymin = Proportion-SE, ymax = Proportion+SE, width = 0.3)) +
  scale_y_continuous(name = "Prevalence (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 18)) +
  ggtitle("Regional prevalence (%) of US residents with low back pain by age in 2018") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N18.GF_AgeReg.LBP.png", width = 14, height = 9)


#ii) age-specific regional logistic regressions
#18 to 24
sub1824 <- subset(NHIS18_DO, AGE_P == "18 to 24")
N18.LBP.age1824.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub1824)
summary(N18.LBP.age1824.reg)
exp(cbind(OR = coef(N18.LBP.age1824.reg), confint(N18.LBP.age1824.reg)))

#25 to 29
sub2529 <- subset(NHIS18_DO, AGE_P == "25 to 29")
N18.LBP.age2529.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub2529)
summary(N18.LBP.age2529.reg)
exp(cbind(OR = coef(N18.LBP.age2529.reg), confint(N18.LBP.age2529.reg)))

#30 to 34
sub3034 <- subset(NHIS18_DO, AGE_P == "30 to 34")
N18.LB.age3034.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub3034)
summary(N18.LB.age3034.reg)
exp(cbind(OR = coef(N18.LB.age3034.reg), confint(N18.LB.age3034.reg)))

#35 to 39
sub3539 <- subset(NHIS18_DO, AGE_P == "35 to 39")
N18.LBP.age3539.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub3539)
summary(N18.LBP.age3539.reg)
exp(cbind(OR = coef(N18.LBP.age3539.reg), confint(N18.LBP.age3539.reg)))

#40 to 44
sub4044 <- subset(NHIS18_DO, AGE_P == "40 to 44")
N18.LBP.age4044.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub4044)
summary(N18.LBP.age4044.reg)
exp(cbind(OR = coef(N18.LBP.age4044.reg), confint(N18.LBP.age4044.reg)))

#45 to 49
sub4549 <- subset(NHIS18_DO, AGE_P == "45 to 49")
N18.LBP.age4549.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub4549)
summary(N18.LBP.age4549.reg)
exp(cbind(OR = coef(N18.LBP.age4549.reg), confint(N18.LBP.age4549.reg)))

#50 to 54
sub5054 <- subset(NHIS18_DO, AGE_P == "50 to 54")
N18.LBP.age5054.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub5054)
summary(N18.LBP.age5054.reg)
exp(cbind(OR = coef(N18.LBP.age5054.reg), confint(N18.LBP.age5054.reg)))

#55 to 59
sub5559 <- subset(NHIS18_DO, AGE_P == "55 to 59")
N18.LBP.age5559.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub5559)
summary(N18.LBP.age5559.reg)
exp(cbind(OR = coef(N18.LBP.age5559.reg), confint(N18.LBP.age5559.reg)))

#60 to 64
sub6064 <- subset(NHIS18_DO, AGE_P == "60 to 64")
N18.LBP.age6064.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub6064)
summary(N18.LBP.age6064.reg)
exp(cbind(OR = coef(N18.LBP.age6064.reg), confint(N18.LBP.age6064.reg)))

#65 to 69
sub6569 <- subset(NHIS18_DO, AGE_P == "65 to 69")
N18.LBP.age6569.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub6569)
summary(N18.LBP.age6569.reg)
exp(cbind(OR = coef(N18.LBP.age6569.reg), confint(N18.LBP.age6569.reg)))

#70 and above
sub70 <- subset(NHIS18_DO, AGE_P == "70 and above")
N18.LBP.age70.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                            family = quasibinomial,
                            design = sub70)
summary(N18.LBP.age70.reg)
exp(cbind(OR = coef(N18.LBP.age70.reg), confint(N18.LBP.age70.reg)))

#III) prevalence by region, by sex

#i)  sex-specific regional prevalence
N18_LBP_reg.sex <- svyby(formula = ~PAINLB,
                         by = ~REGION + SEX,
                         design = NHIS18_DO,
                         FUN = svymean,
                         na.rm = TRUE)
N18_LBP_reg.sex.c <- N18_LBP_reg.sex %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Sex", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N18_LBP_reg.sex.ci <- confint(N18_LBP_reg.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N18_LBP_reg.sex.ci <- N18_LBP_reg.sex.ci[-c(1:8), ]
#join proportion and ci
N18.LBP.sex.region <- bind_cols(N18_LBP_reg.sex.c, N18_LBP_reg.sex.ci) #final prevalence, se & 95% ci
#save
#write.csv(N18.LBP.sex.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N18.LBP.region.sex.csv")

#Geofacet - Sex & Region
LBP.Sex_Reg18 <- ggplot(N18.LBP.sex.region, aes(Sex, Proportion)) +
  geom_bar(colour = "black", stat = "identity", fill = "#6600CC") +
  coord_flip() +
  facet_geo(~Region,
            grid = USregions) +
  theme_bw() +
  theme(text = element_text(colour = "black", size = 20)) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 17)) +
  xlab("Sex") +
  geom_errorbar(aes(ymin = Proportion-SE, ymax = Proportion+SE, width = 0.3)) +
  scale_y_continuous(name = "Prevalence (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 18)) +
  ggtitle("Regional prevalence (%) of US residents with low back pain by sex in 2018") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N18.GF_SexReg.LBP.png", width = 14, height = 9)


#ii) sex-specific regional logistic regressions
#males
submales <- subset(NHIS18_DO, SEX == "Male")
N18.LBP.sexmale.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + AGE_P,
                              family = quasibinomial,
                              design = submales)
summary(N18.LBP.sexmale.reg)
exp(cbind(OR = coef(N18.LBP.sexmale.reg), confint(N18.LBP.sexmale.reg)))

#females
subfemales <- subset(NHIS18_DO, SEX == "Female")
N18.LBP.sexfemale.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + AGE_P,
                                family = quasibinomial,
                                design = subfemales)
summary(N18.LBP.sexfemale.reg)
exp(cbind(OR = coef(N18.LBP.sexfemale.reg), confint(N18.LBP.sexfemale.reg)))


#3. Demographic trends

#I)  Age trends

#i)  Prevalence by age
N18_LBP_age <- svyby(formula = ~PAINLB,
                     by = ~AGE_P,
                     design = NHIS18_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N18_LBP_age.c <- N18_LBP_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N18_LBP_age_ci <- confint(N18_LBP_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove LBP=0 ci
N18_LBP_age_ci <- N18_LBP_age_ci[-c(1:11), ]
#join proportion and ci
N18_LBP.age <- bind_cols(N18_LBP_age.c, N18_LBP_age_ci) #final proportion, se & 95% ci
#Save
#write.csv(N18_LBP.age, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N18.LBP.age.csv")

#ii) Logistic regression by age
N18_LBP.age_glm <- svyglm(PAINLB~AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS18_DO)
summary(N18_LBP.age_glm)
exp(cbind(OR=coef(N18_LBP.age_glm), confint(N18_LBP.age_glm)))


#II)  Sex trends

#i)  Prevalence by sex
N18_LBP_sex <- svyby(formula = ~PAINLB,
                     by = ~SEX,
                     design = NHIS18_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N18_LBP_sex.c <- N18_LBP_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N18_LBP_sex_ci <- confint(N18_LBP_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N18_LBP_sex_ci <- N18_LBP_sex_ci[-c(1:2), ]
#join proportion and ci
N18.LBP.sex <- bind_cols(N18_LBP_sex.c, N18_LBP_sex_ci) #final proportion, se & 95% ci
#save
#write.csv(N18.LBP.sex, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N18.LBP.sex.csv")

#ii) Logistic regression by sex
N18.LB.sex.glm.c <- svyglm(PAINLB~relevel(SEX, ref = "Male") + AGE_P,
                           family = quasibinomial,
                           design = NHIS18_DO)
summary(N18.LB.sex.glm.c)
exp(cbind(OR=coef(N18.LB.sex.glm.c), confint(N18.LB.sex.glm.c)))

#III) Socioeconomic trends (employment status only)

#i)   Prevalence by employment status
N18_LBP_emp <- svyby(formula = ~PAINLB,
                     by = ~DOINGLWA,
                     design = NHIS18_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N18_LBP_emp.c <- N18_LBP_emp %>%
  select(1, 3, 5) %>%
  setNames(c("Employment", "Proportion", "SE")) %>%
  mutate(Employment = as.factor(Employment)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N18_LBP_emp_ci <- confint(N18_LBP_emp) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for LBP = 0
N18_LBP_emp_ci <- N18_LBP_emp_ci[-c(1:2), ]
#join ci and proportion
N18.LBP.emp <- bind_cols(N18_LBP_emp.c, N18_LBP_emp_ci) #final proportion, se & 95% ci
#save
#write.csv(N18.LBP.emp, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N18.LBP.emp.csv")


#ii)  Logistic regression by employment status
N18_LBP.emp_glm <- svyglm(PAINLB~relevel(DOINGLWA, ref = "Working for pay at a job/business") + AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS18_DO)
summary(N18_LBP.emp_glm)
exp(cbind(OR=coef(N18_LBP.emp_glm), confint(N18_LBP.emp_glm)))

#4. Lifestyle trends

#I)  BMI
#logistic regression
N18.BMI.LBP.glm <- svyglm(PAINLB ~ BMI + AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS18_DO)
summary(N18.BMI.LBP.glm)
exp(cbind(OR=coef(N18.BMI.LBP.glm), confint(N18.BMI.LBP.glm)))

#II) Physical activity
#logistic regression
N18.PA.LBP.glm <- svyglm(PAINLB ~ relevel(factor(PAC, ordered = FALSE), ref = "Physically active") + AGE_P + SEX,
                         family = quasibinomial,
                         design = NHIS18_DO)
summary(N18.PA.LBP.glm)
exp(cbind(OR = coef(N18.PA.LBP.glm), confint(N18.PA.LBP.glm)))


#_____________________________________________________________________________________________________________________________________


# == 2017 == #

#1. Overall prevalence
N17_LBP <- svymean(~factor(PAINLB), 
                   NHIS17_DO, 
                   na.rm = TRUE)
N17_LBP.c <- N17_LBP %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N17_LBP_ci <- confint(N17_LBP) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
N17.LBP <- bind_cols(N17_LBP.c, N17_LBP_ci)
#remove LB = 0
N17.LBP <- N17.LBP[-c(1), ] #final proportion, se & 95% ci
#save
#write.csv(N17.LBP, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N17.LBP.csv")


#2. Spatial analysis, by region

#I)   Overall population
#i)   regional prevalence for the overall population
N17_LBP_reg <- svyby(formula = ~PAINLB,
                     by = ~REGION,
                     design = NHIS17_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N17_LBP_reg.c <- N17_LBP_reg %>%
  select(1, 3, 5) %>%
  setNames(c("Region", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N17_LBP_reg.ci <- confint(N17_LBP_reg) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N17_LBP_reg.ci <- N17_LBP_reg.ci[-c(1:4), ]
#join proportion and ci
N17.LBP.region <- bind_cols(N17_LBP_reg.c, N17_LBP_reg.ci) #final proportion, se & 95% ci
#save
#write.csv(N17.LBP.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N17.LBP.region.csv")

#Change the region column to "NAME" to match sf
names(N17.LBP.region)[names(N17.LBP.region) == "Region"] <- "NAME"

N17.LBP.joined <- regions %>%
  left_join(N17.LBP.region)

N17.LBP.joined$NAME <- as.factor(N17.LBP.joined$NAME)

#Plot
LBPregplot17 <- ggplot(data = N17.LBP.joined) +
  geom_sf(aes(fill = Proportion), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  scale_fill_viridis_c(name = "Prevalence (%)", option = "viridis", direction = -1, limits = c(25, 35), breaks = c(25, 30, 35)) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  ggtitle("Regional prevalence (%) of US residents with low back pain in 2017") +
  theme(plot.title = element_text(size = 10, hjust = 0.5))
ggsave("N17.LBP.regchor.png", width = 5, height = 5)


#ii)  logistic regression by region
N17.LBP.reg.glm <- svyglm(relevel(PAINLB, ref = "0")~ REGION + AGE_P,
                          family = quasibinomial,
                          design = NHIS17_DO)
summary(N17.LBP.reg.glm)
exp(cbind(OR=coef(N17.LBP.reg.glm), confint(N17.LBP.reg.glm)))


#II)  prevalence by region, by age

#i)  age-specific regional prevalance
N17_LBP_reg.age <- svyby(formula = ~PAINLB,
                         by = ~REGION + AGE_P,
                         design = NHIS17_DO,
                         FUN = svymean,
                         na.rm = TRUE)
N17_LBP_reg.age.c <- N17_LBP_reg.age %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Age", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Age = as.character(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N17_LBP_reg.age.ci <- confint(N17_LBP_reg.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N17_LBP_reg.age.ci <- N17_LBP_reg.age.ci[-c(1:44), ]
#join proportion and ci
N17.LBP.age.region <- bind_cols(N17_LBP_reg.age.c, N17_LBP_reg.age.ci) #final prevalence, se & 95% ci
#save
#write.csv(N17.LBP.age.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N17.LBP.region.age.csv")

#Geofacet - Age & Region
LBP.Age_Reg17 <- ggplot(N17.LBP.age.region, aes(Age, Proportion)) +
  geom_bar(colour = "black", stat = "identity", fill = "#6600CC") +
  coord_flip() +
  facet_geo(~Region,
            grid = USregions) +
  theme_bw() +
  theme(text = element_text(colour = "black", size = 20)) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 17)) +
  xlab("Age group (years)") +
  geom_errorbar(aes(ymin = Proportion-SE, ymax = Proportion+SE, width = 0.3)) +
  scale_y_continuous(name = "Prevalence (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 18)) +
  ggtitle("Regional prevalence (%) of US residents with low back pain by age in 2017") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N17.GF_AgeReg.LBP.png", width = 14, height = 9)


#ii) age-specific regional logistic regressions
#18 to 24
sub1824 <- subset(NHIS17_DO, AGE_P == "18 to 24")
N17.LBP.age1824.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub1824)
summary(N17.LBP.age1824.reg)
exp(cbind(OR = coef(N17.LBP.age1824.reg), confint(N17.LBP.age1824.reg)))

#25 to 29
sub2529 <- subset(NHIS17_DO, AGE_P == "25 to 29")
N17.LBP.age2529.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub2529)
summary(N17.LBP.age2529.reg)
exp(cbind(OR = coef(N17.LBP.age2529.reg), confint(N17.LBP.age2529.reg)))

#30 to 34
sub3034 <- subset(NHIS17_DO, AGE_P == "30 to 34")
N17.LB.age3034.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub3034)
summary(N17.LB.age3034.reg)
exp(cbind(OR = coef(N17.LB.age3034.reg), confint(N17.LB.age3034.reg)))

#35 to 39
sub3539 <- subset(NHIS17_DO, AGE_P == "35 to 39")
N17.LBP.age3539.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub3539)
summary(N17.LBP.age3539.reg)
exp(cbind(OR = coef(N17.LBP.age3539.reg), confint(N17.LBP.age3539.reg)))

#40 to 44
sub4044 <- subset(NHIS17_DO, AGE_P == "40 to 44")
N17.LBP.age4044.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub4044)
summary(N17.LBP.age4044.reg)
exp(cbind(OR = coef(N17.LBP.age4044.reg), confint(N17.LBP.age4044.reg)))

#45 to 49
sub4549 <- subset(NHIS17_DO, AGE_P == "45 to 49")
N17.LBP.age4549.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub4549)
summary(N17.LBP.age4549.reg)
exp(cbind(OR = coef(N17.LBP.age4549.reg), confint(N17.LBP.age4549.reg)))

#50 to 54
sub5054 <- subset(NHIS17_DO, AGE_P == "50 to 54")
N17.LBP.age5054.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub5054)
summary(N17.LBP.age5054.reg)
exp(cbind(OR = coef(N17.LBP.age5054.reg), confint(N17.LBP.age5054.reg)))

#55 to 59
sub5559 <- subset(NHIS17_DO, AGE_P == "55 to 59")
N17.LBP.age5559.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub5559)
summary(N17.LBP.age5559.reg)
exp(cbind(OR = coef(N17.LBP.age5559.reg), confint(N17.LBP.age5559.reg)))

#60 to 64
sub6064 <- subset(NHIS17_DO, AGE_P == "60 to 64")
N17.LBP.age6064.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub6064)
summary(N17.LBP.age6064.reg)
exp(cbind(OR = coef(N17.LBP.age6064.reg), confint(N17.LBP.age6064.reg)))

#65 to 69
sub6569 <- subset(NHIS17_DO, AGE_P == "65 to 69")
N17.LBP.age6569.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub6569)
summary(N17.LBP.age6569.reg)
exp(cbind(OR = coef(N17.LBP.age6569.reg), confint(N17.LBP.age6569.reg)))

#70 and above
sub70 <- subset(NHIS17_DO, AGE_P == "70 and above")
N17.LBP.age70.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                            family = quasibinomial,
                            design = sub70)
summary(N17.LBP.age70.reg)
exp(cbind(OR = coef(N17.LBP.age70.reg), confint(N17.LBP.age70.reg)))

#III) prevalence by region, by sex

#i)  sex-specific regional prevalence
N17_LBP_reg.sex <- svyby(formula = ~PAINLB,
                         by = ~REGION + SEX,
                         design = NHIS17_DO,
                         FUN = svymean,
                         na.rm = TRUE)
N17_LBP_reg.sex.c <- N17_LBP_reg.sex %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Sex", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N17_LBP_reg.sex.ci <- confint(N17_LBP_reg.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N17_LBP_reg.sex.ci <- N17_LBP_reg.sex.ci[-c(1:8), ]
#join proportion and ci
N17.LBP.sex.region <- bind_cols(N17_LBP_reg.sex.c, N17_LBP_reg.sex.ci) #final prevalence, se & 95% ci
#save
#write.csv(N17.LBP.sex.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N17.LBP.region.sex.csv")

#Geofacet - Sex & Region
LBP.Sex_Reg17 <- ggplot(N17.LBP.sex.region, aes(Sex, Proportion)) +
  geom_bar(colour = "black", stat = "identity", fill = "#6600CC") +
  coord_flip() +
  facet_geo(~Region,
            grid = USregions) +
  theme_bw() +
  theme(text = element_text(colour = "black", size = 20)) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 17)) +
  xlab("Sex") +
  geom_errorbar(aes(ymin = Proportion-SE, ymax = Proportion+SE, width = 0.3)) +
  scale_y_continuous(name = "Prevalence (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 18)) +
  ggtitle("Regional prevalence (%) of US residents with low back pain by sex in 2017") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N17.GF_SexReg.LBP.png", width = 14, height = 9)


#ii) sex-specific regional logistic regressions
#males
submales <- subset(NHIS17_DO, SEX == "Male")
N17.LBP.sexmale.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + AGE_P,
                              family = quasibinomial,
                              design = submales)
summary(N17.LBP.sexmale.reg)
exp(cbind(OR = coef(N17.LBP.sexmale.reg), confint(N17.LBP.sexmale.reg)))

#females
subfemales <- subset(NHIS17_DO, SEX == "Female")
N17.LBP.sexfemale.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + AGE_P,
                                family = quasibinomial,
                                design = subfemales)
summary(N17.LBP.sexfemale.reg)
exp(cbind(OR = coef(N17.LBP.sexfemale.reg), confint(N17.LBP.sexfemale.reg)))


#3. Demographic trends

#I)  Age trends

#i)  Prevalence by age
N17_LBP_age <- svyby(formula = ~PAINLB,
                     by = ~AGE_P,
                     design = NHIS17_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N17_LBP_age.c <- N17_LBP_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N17_LBP_age_ci <- confint(N17_LBP_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove LBP=0 ci
N17_LBP_age_ci <- N17_LBP_age_ci[-c(1:11), ]
#join proportion and ci
N17_LBP.age <- bind_cols(N17_LBP_age.c, N17_LBP_age_ci) #final proportion, se & 95% ci
#Save
#write.csv(N17_LBP.age, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N17.LBP.age.csv")

#ii) Logistic regression by age
N17_LBP.age_glm <- svyglm(PAINLB~AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS17_DO)
summary(N17_LBP.age_glm)
exp(cbind(OR=coef(N17_LBP.age_glm), confint(N17_LBP.age_glm)))


#II)  Sex trends

#i)  Prevalence by sex
N17_LBP_sex <- svyby(formula = ~PAINLB,
                     by = ~SEX,
                     design = NHIS17_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N17_LBP_sex.c <- N17_LBP_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N17_LBP_sex_ci <- confint(N17_LBP_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N17_LBP_sex_ci <- N17_LBP_sex_ci[-c(1:2), ]
#join proportion and ci
N17.LBP.sex <- bind_cols(N17_LBP_sex.c, N17_LBP_sex_ci) #final proportion, se & 95% ci
#save
#write.csv(N17.LBP.sex, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N17.LBP.sex.csv")

#ii) Logistic regression by sex
N17.LB.sex.glm.c <- svyglm(PAINLB~relevel(SEX, ref = "Male") + AGE_P,
                           family = quasibinomial,
                           design = NHIS17_DO)
summary(N17.LB.sex.glm.c)
exp(cbind(OR=coef(N17.LB.sex.glm.c), confint(N17.LB.sex.glm.c)))

#III) Socioeconomic trends (employment status only)

#i)   Prevalence by employment status
N17_LBP_emp <- svyby(formula = ~PAINLB,
                     by = ~DOINGLWA,
                     design = NHIS17_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N17_LBP_emp.c <- N17_LBP_emp %>%
  select(1, 3, 5) %>%
  setNames(c("Employment", "Proportion", "SE")) %>%
  mutate(Employment = as.factor(Employment)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N17_LBP_emp_ci <- confint(N17_LBP_emp) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for LBP = 0
N17_LBP_emp_ci <- N17_LBP_emp_ci[-c(1:2), ]
#join ci and proportion
N17.LBP.emp <- bind_cols(N17_LBP_emp.c, N17_LBP_emp_ci) #final proportion, se & 95% ci
#save
#write.csv(N17.LBP.emp, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N17.LBP.emp.csv")


#ii)  Logistic regression by employment status
N17_LBP.emp_glm <- svyglm(PAINLB~relevel(DOINGLWA, ref = "Working for pay at a job/business") + AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS17_DO)
summary(N17_LBP.emp_glm)
exp(cbind(OR=coef(N17_LBP.emp_glm), confint(N17_LBP.emp_glm)))

#4. Lifestyle trends

#I)  BMI
#logistic regression
N17.BMI.LBP.glm <- svyglm(PAINLB ~ BMI + AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS17_DO)
summary(N17.BMI.LBP.glm)
exp(cbind(OR=coef(N17.BMI.LBP.glm), confint(N17.BMI.LBP.glm)))

#II) Physical activity
#logistic regression
N17.PA.LBP.glm <- svyglm(PAINLB ~ relevel(factor(PAC, ordered = FALSE), ref = "Physically active") + AGE_P + SEX,
                         family = quasibinomial,
                         design = NHIS17_DO)
summary(N17.PA.LBP.glm)
exp(cbind(OR = coef(N17.PA.LBP.glm), confint(N17.PA.LBP.glm)))

#____________________________________________________________________________________________________________________________________________


# == 2016 == #

#1. Overall prevalence
N16_LBP <- svymean(~factor(PAINLB), 
                   NHIS16_DO, 
                   na.rm = TRUE)
N16_LBP.c <- N16_LBP %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N16_LBP_ci <- confint(N16_LBP) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
N16.LBP <- bind_cols(N16_LBP.c, N16_LBP_ci)
#remove LB = 0
N16.LBP <- N16.LBP[-c(1), ] #final proportion, se & 95% ci
#save
#write.csv(N16.LBP, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N16.LBP.csv")


#2. Spatial analysis, by region

#I)   Overall population
#i)   regional prevalence for the overall population
N16_LBP_reg <- svyby(formula = ~PAINLB,
                     by = ~REGION,
                     design = NHIS16_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N16_LBP_reg.c <- N16_LBP_reg %>%
  select(1, 3, 5) %>%
  setNames(c("Region", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N16_LBP_reg.ci <- confint(N16_LBP_reg) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N16_LBP_reg.ci <- N16_LBP_reg.ci[-c(1:4), ]
#join proportion and ci
N16.LBP.region <- bind_cols(N16_LBP_reg.c, N16_LBP_reg.ci) #final proportion, se & 95% ci
#save
#write.csv(N16.LBP.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N16.LBP.region.csv")

#Change the region column to "NAME" to match sf
names(N16.LBP.region)[names(N16.LBP.region) == "Region"] <- "NAME"

N16.LBP.joined <- regions %>%
  left_join(N16.LBP.region)

N16.LBP.joined$NAME <- as.factor(N16.LBP.joined$NAME)

#Plot
LBPregplot16 <- ggplot(data = N16.LBP.joined) +
  geom_sf(aes(fill = Proportion), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  scale_fill_viridis_c(name = "Prevalence (%)", option = "viridis", direction = -1, limits = c(25, 35), breaks = c(25, 30, 35)) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  ggtitle("Regional prevalence (%) of US residents with low back pain in 2016") +
  theme(plot.title = element_text(size = 10, hjust = 0.5))
ggsave("N16.LBP.regchor.png", width = 5, height = 5)


#ii)  logistic regression by region
N16.LBP.reg.glm <- svyglm(relevel(PAINLB, ref = "0")~ REGION + AGE_P,
                          family = quasibinomial,
                          design = NHIS16_DO)
summary(N16.LBP.reg.glm)
exp(cbind(OR=coef(N16.LBP.reg.glm), confint(N16.LBP.reg.glm)))


#II)  prevalence by region, by age

#i)  age-specific regional prevalance
N16_LBP_reg.age <- svyby(formula = ~PAINLB,
                         by = ~REGION + AGE_P,
                         design = NHIS16_DO,
                         FUN = svymean,
                         na.rm = TRUE)
N16_LBP_reg.age.c <- N16_LBP_reg.age %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Age", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Age = as.character(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N16_LBP_reg.age.ci <- confint(N16_LBP_reg.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N16_LBP_reg.age.ci <- N16_LBP_reg.age.ci[-c(1:44), ]
#join proportion and ci
N16.LBP.age.region <- bind_cols(N16_LBP_reg.age.c, N16_LBP_reg.age.ci) #final prevalence, se & 95% ci
#save
#write.csv(N16.LBP.age.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N16.LBP.region.age.csv")

#Geofacet - Age & Region
LBP.Age_Reg <- ggplot(N16.LBP.age.region, aes(Age, Proportion)) +
  geom_bar(colour = "black", stat = "identity", fill = "#6600CC") +
  coord_flip() +
  facet_geo(~Region,
            grid = USregions) +
  theme_bw() +
  theme(text = element_text(colour = "black", size = 20)) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 17)) +
  xlab("Age group (years)") +
  geom_errorbar(aes(ymin = Proportion-SE, ymax = Proportion+SE, width = 0.3)) +
  scale_y_continuous(name = "Prevalence (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 18)) +
  ggtitle("Regional prevalence of US residents with low back pain by age in 2016") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N16.GF_AgeReg.LBP.png", width = 14, height = 9)


#ii) age-specific regional logistic regressions
#18 to 24
sub1824 <- subset(NHIS16_DO, AGE_P == "18 to 24")
N16.LBP.age1824.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub1824)
summary(N16.LBP.age1824.reg)
exp(cbind(OR = coef(N16.LBP.age1824.reg), confint(N16.LBP.age1824.reg)))

#25 to 29
sub2529 <- subset(NHIS16_DO, AGE_P == "25 to 29")
N16.LBP.age2529.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub2529)
summary(N16.LBP.age2529.reg)
exp(cbind(OR = coef(N16.LBP.age2529.reg), confint(N16.LBP.age2529.reg)))

#30 to 34
sub3034 <- subset(NHIS16_DO, AGE_P == "30 to 34")
N16.LB.age3034.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub3034)
summary(N16.LB.age3034.reg)
exp(cbind(OR = coef(N16.LB.age3034.reg), confint(N16.LB.age3034.reg)))

#35 to 39
sub3539 <- subset(NHIS16_DO, AGE_P == "35 to 39")
N16.LBP.age3539.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub3539)
summary(N16.LBP.age3539.reg)
exp(cbind(OR = coef(N16.LBP.age3539.reg), confint(N16.LBP.age3539.reg)))

#40 to 44
sub4044 <- subset(NHIS16_DO, AGE_P == "40 to 44")
N16.LBP.age4044.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub4044)
summary(N16.LBP.age4044.reg)
exp(cbind(OR = coef(N16.LBP.age4044.reg), confint(N16.LBP.age4044.reg)))

#45 to 49
sub4549 <- subset(NHIS16_DO, AGE_P == "45 to 49")
N16.LBP.age4549.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub4549)
summary(N16.LBP.age4549.reg)
exp(cbind(OR = coef(N16.LBP.age4549.reg), confint(N16.LBP.age4549.reg)))

#50 to 54
sub5054 <- subset(NHIS16_DO, AGE_P == "50 to 54")
N16.LBP.age5054.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub5054)
summary(N16.LBP.age5054.reg)
exp(cbind(OR = coef(N16.LBP.age5054.reg), confint(N16.LBP.age5054.reg)))

#55 to 59
sub5559 <- subset(NHIS16_DO, AGE_P == "55 to 59")
N16.LBP.age5559.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub5559)
summary(N16.LBP.age5559.reg)
exp(cbind(OR = coef(N16.LBP.age5559.reg), confint(N16.LBP.age5559.reg)))

#60 to 64
sub6064 <- subset(NHIS16_DO, AGE_P == "60 to 64")
N16.LBP.age6064.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub6064)
summary(N16.LBP.age6064.reg)
exp(cbind(OR = coef(N16.LBP.age6064.reg), confint(N16.LBP.age6064.reg)))

#65 to 69
sub6569 <- subset(NHIS16_DO, AGE_P == "65 to 69")
N16.LBP.age6569.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub6569)
summary(N16.LBP.age6569.reg)
exp(cbind(OR = coef(N16.LBP.age6569.reg), confint(N16.LBP.age6569.reg)))

#70 and above
sub70 <- subset(NHIS16_DO, AGE_P == "70 and above")
N16.LBP.age70.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                            family = quasibinomial,
                            design = sub70)
summary(N16.LBP.age70.reg)
exp(cbind(OR = coef(N16.LBP.age70.reg), confint(N16.LBP.age70.reg)))

#III) prevalence by region, by sex

#i)  sex-specific regional prevalence
N16_LBP_reg.sex <- svyby(formula = ~PAINLB,
                         by = ~REGION + SEX,
                         design = NHIS16_DO,
                         FUN = svymean,
                         na.rm = TRUE)
N16_LBP_reg.sex.c <- N16_LBP_reg.sex %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Sex", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N16_LBP_reg.sex.ci <- confint(N16_LBP_reg.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N16_LBP_reg.sex.ci <- N16_LBP_reg.sex.ci[-c(1:8), ]
#join proportion and ci
N16.LBP.sex.region <- bind_cols(N16_LBP_reg.sex.c, N16_LBP_reg.sex.ci) #final prevalence, se & 95% ci
#save
#write.csv(N16.LBP.sex.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N16.LBP.region.sex.csv")

#Geofacet - Sex & Region
LBP.Sex_Reg16 <- ggplot(N16.LBP.sex.region, aes(Sex, Proportion)) +
  geom_bar(colour = "black", stat = "identity", fill = "#6600CC") +
  coord_flip() +
  facet_geo(~Region,
            grid = USregions) +
  theme_bw() +
  theme(text = element_text(colour = "black", size = 20)) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 17)) +
  xlab("Sex") +
  geom_errorbar(aes(ymin = Proportion-SE, ymax = Proportion+SE, width = 0.3)) +
  scale_y_continuous(name = "Prevalence (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 18)) +
  ggtitle("Regional prevalence (%) of US residents with low back pain by sex in 2016") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N16.GF_SexReg.LBP.png", width = 14, height = 9)


#ii) sex-specific regional logistic regressions
#males
submales <- subset(NHIS16_DO, SEX == "Male")
N16.LBP.sexmale.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + AGE_P,
                              family = quasibinomial,
                              design = submales)
summary(N16.LBP.sexmale.reg)
exp(cbind(OR = coef(N16.LBP.sexmale.reg), confint(N16.LBP.sexmale.reg)))

#females
subfemales <- subset(NHIS16_DO, SEX == "Female")
N16.LBP.sexfemale.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + AGE_P,
                                family = quasibinomial,
                                design = subfemales)
summary(N16.LBP.sexfemale.reg)
exp(cbind(OR = coef(N16.LBP.sexfemale.reg), confint(N16.LBP.sexfemale.reg)))


#3. Demographic trends

#I)  Age trends

#i)  Prevalence by age
N16_LBP_age <- svyby(formula = ~PAINLB,
                     by = ~AGE_P,
                     design = NHIS16_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N16_LBP_age.c <- N16_LBP_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N16_LBP_age_ci <- confint(N16_LBP_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove LBP=0 ci
N16_LBP_age_ci <- N16_LBP_age_ci[-c(1:11), ]
#join proportion and ci
N16_LBP.age <- bind_cols(N16_LBP_age.c, N16_LBP_age_ci) #final proportion, se & 95% ci
#Save
#write.csv(N16_LBP.age, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N16.LBP.age.csv")

#ii) Logistic regression by age
N16_LBP.age_glm <- svyglm(PAINLB~AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS16_DO)
summary(N16_LBP.age_glm)
exp(cbind(OR=coef(N16_LBP.age_glm), confint(N16_LBP.age_glm)))


#II)  Sex trends

#i)  Prevalence by sex
N16_LBP_sex <- svyby(formula = ~PAINLB,
                     by = ~SEX,
                     design = NHIS16_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N16_LBP_sex.c <- N16_LBP_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N16_LBP_sex_ci <- confint(N16_LBP_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N16_LBP_sex_ci <- N16_LBP_sex_ci[-c(1:2), ]
#join proportion and ci
N16.LBP.sex <- bind_cols(N16_LBP_sex.c, N16_LBP_sex_ci) #final proportion, se & 95% ci
#save
#write.csv(N16.LBP.sex, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N16.LBP.sex.csv")

#ii) Logistic regression by sex
N16.LB.sex.glm.c <- svyglm(PAINLB~relevel(SEX, ref = "Male") + AGE_P,
                           family = quasibinomial,
                           design = NHIS16_DO)
summary(N16.LB.sex.glm.c)
exp(cbind(OR=coef(N16.LB.sex.glm.c), confint(N16.LB.sex.glm.c)))

#III) Socioeconomic trends (employment status only)

#i)   Prevalence by employment status
N16_LBP_emp <- svyby(formula = ~PAINLB,
                     by = ~DOINGLWA,
                     design = NHIS16_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N16_LBP_emp.c <- N16_LBP_emp %>%
  select(1, 3, 5) %>%
  setNames(c("Employment", "Proportion", "SE")) %>%
  mutate(Employment = as.factor(Employment)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N16_LBP_emp_ci <- confint(N16_LBP_emp) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for LBP = 0
N16_LBP_emp_ci <- N16_LBP_emp_ci[-c(1:2), ]
#join ci and proportion
N16.LBP.emp <- bind_cols(N16_LBP_emp.c, N16_LBP_emp_ci) #final proportion, se & 95% ci
#save
#write.csv(N16.LBP.emp, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N16.LBP.emp.csv")


#ii)  Logistic regression by employment status
N16_LBP.emp_glm <- svyglm(PAINLB~relevel(DOINGLWA, ref = "Working for pay at a job/business") + AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS16_DO)
summary(N16_LBP.emp_glm)
exp(cbind(OR=coef(N16_LBP.emp_glm), confint(N16_LBP.emp_glm)))

#4. Lifestyle trends

#I)  BMI
#logistic regression
N16.BMI.LBP.glm <- svyglm(PAINLB ~ BMI + AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS16_DO)
summary(N16.BMI.LBP.glm)
exp(cbind(OR=coef(N16.BMI.LBP.glm), confint(N16.BMI.LBP.glm)))

#II) Physical activity
#logistic regression
N16.PA.LBP.glm <- svyglm(PAINLB ~ relevel(factor(PAC, ordered = FALSE), ref = "Physically active") + AGE_P + SEX,
                         family = quasibinomial,
                         design = NHIS16_DO)
summary(N16.PA.LBP.glm)
exp(cbind(OR = coef(N16.PA.LBP.glm), confint(N16.PA.LBP.glm)))

#_________________________________________________________________________________________________________________________________________________

# == 2015 == #

#1. Overall prevalence
N15_LBP <- svymean(~factor(PAINLB), 
                   NHIS15_DO, 
                   na.rm = TRUE)
N15_LBP.c <- N15_LBP %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N15_LBP_ci <- confint(N15_LBP) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
N15.LBP <- bind_cols(N15_LBP.c, N15_LBP_ci)
#remove LB = 0
N15.LBP <- N15.LBP[-c(1), ] #final proportion, se & 95% ci
#save
#write.csv(N15.LBP, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N15.LBP.csv")


#2. Spatial analysis, by region

#I)   Overall population
#i)   regional prevalence for the overall population
N15_LBP_reg <- svyby(formula = ~PAINLB,
                     by = ~REGION,
                     design = NHIS15_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N15_LBP_reg.c <- N15_LBP_reg %>%
  select(1, 3, 5) %>%
  setNames(c("Region", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N15_LBP_reg.ci <- confint(N15_LBP_reg) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N15_LBP_reg.ci <- N15_LBP_reg.ci[-c(1:4), ]
#join proportion and ci
N15.LBP.region <- bind_cols(N15_LBP_reg.c, N15_LBP_reg.ci) #final proportion, se & 95% ci
#save
#write.csv(N15.LBP.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N15.LBP.region.csv")

#Change the region column to "NAME" to match sf
names(N15.LBP.region)[names(N15.LBP.region) == "Region"] <- "NAME"

N15.LBP.joined <- regions %>%
  left_join(N15.LBP.region)

N15.LBP.joined$NAME <- as.factor(N15.LBP.joined$NAME)

#Plot
LBPregplot15 <- ggplot(data = N15.LBP.joined) +
  geom_sf(aes(fill = Proportion), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  scale_fill_viridis_c(name = "Prevalence (%)", option = "viridis", direction = -1, limits = c(25, 35), breaks = c(25, 30, 35)) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  ggtitle("Regional prevalence (%) of US residents with low back pain in 2015") +
  theme(plot.title = element_text(size = 10, hjust = 0.5))
ggsave("N15.LBP.regchor.png", width = 5, height = 5)


#ii)  logistic regression by region
N15.LBP.reg.glm <- svyglm(relevel(PAINLB, ref = "0")~ REGION + AGE_P,
                          family = quasibinomial,
                          design = NHIS15_DO)
summary(N15.LBP.reg.glm)
exp(cbind(OR=coef(N15.LBP.reg.glm), confint(N15.LBP.reg.glm)))


#II)  prevalence by region, by age

#i)  age-specific regional prevalance
N15_LBP_reg.age <- svyby(formula = ~PAINLB,
                         by = ~REGION + AGE_P,
                         design = NHIS15_DO,
                         FUN = svymean,
                         na.rm = TRUE)
N15_LBP_reg.age.c <- N15_LBP_reg.age %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Age", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Age = as.character(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N15_LBP_reg.age.ci <- confint(N15_LBP_reg.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N15_LBP_reg.age.ci <- N15_LBP_reg.age.ci[-c(1:44), ]
#join proportion and ci
N15.LBP.age.region <- bind_cols(N15_LBP_reg.age.c, N15_LBP_reg.age.ci) #final prevalence, se & 95% ci
#save
#write.csv(N15.LBP.age.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N15.LBP.region.age.csv")

#Geofacet - Age & Region
LBP.Age_Reg <- ggplot(N15.LBP.age.region, aes(Age, Proportion)) +
  geom_bar(colour = "black", stat = "identity", fill = "#6600CC") +
  coord_flip() +
  facet_geo(~Region,
            grid = USregions) +
  theme_bw() +
  theme(text = element_text(colour = "black", size = 20)) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 17)) +
  xlab("Age group (years)") +
  geom_errorbar(aes(ymin = Proportion-SE, ymax = Proportion+SE, width = 0.3)) +
  scale_y_continuous(name = "Prevalence (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 18)) +
  ggtitle("Regional prevalence (%) of US residents of low back pain by age in 2015") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N15.GF_AgeReg.LBP.png", width = 14, height = 9)


#ii) age-specific regional logistic regressions
#18 to 24
sub1824 <- subset(NHIS15_DO, AGE_P == "18 to 24")
N15.LBP.age1824.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub1824)
summary(N15.LBP.age1824.reg)
exp(cbind(OR = coef(N15.LBP.age1824.reg), confint(N15.LBP.age1824.reg)))

#25 to 29
sub2529 <- subset(NHIS15_DO, AGE_P == "25 to 29")
N15.LBP.age2529.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub2529)
summary(N15.LBP.age2529.reg)
exp(cbind(OR = coef(N15.LBP.age2529.reg), confint(N15.LBP.age2529.reg)))

#30 to 34
sub3034 <- subset(NHIS15_DO, AGE_P == "30 to 34")
N15.LB.age3034.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub3034)
summary(N15.LB.age3034.reg)
exp(cbind(OR = coef(N15.LB.age3034.reg), confint(N15.LB.age3034.reg)))

#35 to 39
sub3539 <- subset(NHIS15_DO, AGE_P == "35 to 39")
N15.LBP.age3539.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub3539)
summary(N15.LBP.age3539.reg)
exp(cbind(OR = coef(N15.LBP.age3539.reg), confint(N15.LBP.age3539.reg)))

#40 to 44
sub4044 <- subset(NHIS15_DO, AGE_P == "40 to 44")
N15.LBP.age4044.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub4044)
summary(N15.LBP.age4044.reg)
exp(cbind(OR = coef(N15.LBP.age4044.reg), confint(N15.LBP.age4044.reg)))

#45 to 49
sub4549 <- subset(NHIS15_DO, AGE_P == "45 to 49")
N15.LBP.age4549.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub4549)
summary(N15.LBP.age4549.reg)
exp(cbind(OR = coef(N15.LBP.age4549.reg), confint(N15.LBP.age4549.reg)))

#50 to 54
sub5054 <- subset(NHIS15_DO, AGE_P == "50 to 54")
N15.LBP.age5054.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub5054)
summary(N15.LBP.age5054.reg)
exp(cbind(OR = coef(N15.LBP.age5054.reg), confint(N15.LBP.age5054.reg)))

#55 to 59
sub5559 <- subset(NHIS15_DO, AGE_P == "55 to 59")
N15.LBP.age5559.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub5559)
summary(N15.LBP.age5559.reg)
exp(cbind(OR = coef(N15.LBP.age5559.reg), confint(N15.LBP.age5559.reg)))

#60 to 64
sub6064 <- subset(NHIS15_DO, AGE_P == "60 to 64")
N15.LBP.age6064.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub6064)
summary(N15.LBP.age6064.reg)
exp(cbind(OR = coef(N15.LBP.age6064.reg), confint(N15.LBP.age6064.reg)))

#65 to 69
sub6569 <- subset(NHIS15_DO, AGE_P == "65 to 69")
N15.LBP.age6569.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub6569)
summary(N15.LBP.age6569.reg)
exp(cbind(OR = coef(N15.LBP.age6569.reg), confint(N15.LBP.age6569.reg)))

#70 and above
sub70 <- subset(NHIS15_DO, AGE_P == "70 and above")
N15.LBP.age70.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                            family = quasibinomial,
                            design = sub70)
summary(N15.LBP.age70.reg)
exp(cbind(OR = coef(N15.LBP.age70.reg), confint(N15.LBP.age70.reg)))

#III) prevalence by region, by sex

#i)  sex-specific regional prevalence
N15_LBP_reg.sex <- svyby(formula = ~PAINLB,
                         by = ~REGION + SEX,
                         design = NHIS15_DO,
                         FUN = svymean,
                         na.rm = TRUE)
N15_LBP_reg.sex.c <- N15_LBP_reg.sex %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Sex", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N15_LBP_reg.sex.ci <- confint(N15_LBP_reg.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N15_LBP_reg.sex.ci <- N15_LBP_reg.sex.ci[-c(1:8), ]
#join proportion and ci
N15.LBP.sex.region <- bind_cols(N15_LBP_reg.sex.c, N15_LBP_reg.sex.ci) #final prevalence, se & 95% ci
#save
#write.csv(N15.LBP.sex.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N15.LBP.region.sex.csv")

#Geofacet - Sex & Region
LBP.Sex_Reg15 <- ggplot(N15.LBP.sex.region, aes(Sex, Proportion)) +
  geom_bar(colour = "black", stat = "identity", fill = "#6600CC") +
  coord_flip() +
  facet_geo(~Region,
            grid = USregions) +
  theme_bw() +
  theme(text = element_text(colour = "black", size = 20)) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 17)) +
  xlab("Sex") +
  geom_errorbar(aes(ymin = Proportion-SE, ymax = Proportion+SE, width = 0.3)) +
  scale_y_continuous(name = "Prevalence (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 18)) +
  ggtitle("Regional prevalence (%) of US residents with low back pain by sex in 2015") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N15.GF_SexReg.LBP.png", width = 14, height = 9)


#ii) sex-specific regional logistic regressions
#males
submales <- subset(NHIS15_DO, SEX == "Male")
N15.LBP.sexmale.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + AGE_P,
                              family = quasibinomial,
                              design = submales)
summary(N15.LBP.sexmale.reg)
exp(cbind(OR = coef(N15.LBP.sexmale.reg), confint(N15.LBP.sexmale.reg)))

#females
subfemales <- subset(NHIS15_DO, SEX == "Female")
N15.LBP.sexfemale.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + AGE_P,
                                family = quasibinomial,
                                design = subfemales)
summary(N15.LBP.sexfemale.reg)
exp(cbind(OR = coef(N15.LBP.sexfemale.reg), confint(N15.LBP.sexfemale.reg)))


#3. Demographic trends

#I)  Age trends

#i)  Prevalence by age
N15_LBP_age <- svyby(formula = ~PAINLB,
                     by = ~AGE_P,
                     design = NHIS15_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N15_LBP_age.c <- N15_LBP_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N15_LBP_age_ci <- confint(N15_LBP_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove LBP=0 ci
N15_LBP_age_ci <- N15_LBP_age_ci[-c(1:11), ]
#join proportion and ci
N15_LBP.age <- bind_cols(N15_LBP_age.c, N15_LBP_age_ci) #final proportion, se & 95% ci
#Save
#write.csv(N15_LBP.age, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N15.LBP.age.csv")

#ii) Logistic regression by age
N15_LBP.age_glm <- svyglm(PAINLB~AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS15_DO)
summary(N15_LBP.age_glm)
exp(cbind(OR=coef(N15_LBP.age_glm), confint(N15_LBP.age_glm)))


#II)  Sex trends

#i)  Prevalence by sex
N15_LBP_sex <- svyby(formula = ~PAINLB,
                     by = ~SEX,
                     design = NHIS15_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N15_LBP_sex.c <- N15_LBP_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N15_LBP_sex_ci <- confint(N15_LBP_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N15_LBP_sex_ci <- N15_LBP_sex_ci[-c(1:2), ]
#join proportion and ci
N15.LBP.sex <- bind_cols(N15_LBP_sex.c, N15_LBP_sex_ci) #final proportion, se & 95% ci
#save
#write.csv(N15.LBP.sex, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N15.LBP.sex.csv")

#ii) Logistic regression by sex
N15.LB.sex.glm.c <- svyglm(PAINLB~relevel(SEX, ref = "Male") + AGE_P,
                           family = quasibinomial,
                           design = NHIS15_DO)
summary(N15.LB.sex.glm.c)
exp(cbind(OR=coef(N15.LB.sex.glm.c), confint(N15.LB.sex.glm.c)))

#III) Socioeconomic trends (employment status only)

#i)   Prevalence by employment status
N15_LBP_emp <- svyby(formula = ~PAINLB,
                     by = ~DOINGLWA,
                     design = NHIS15_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N15_LBP_emp.c <- N15_LBP_emp %>%
  select(1, 3, 5) %>%
  setNames(c("Employment", "Proportion", "SE")) %>%
  mutate(Employment = as.factor(Employment)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N15_LBP_emp_ci <- confint(N15_LBP_emp) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for LBP = 0
N15_LBP_emp_ci <- N15_LBP_emp_ci[-c(1:2), ]
#join ci and proportion
N15.LBP.emp <- bind_cols(N15_LBP_emp.c, N15_LBP_emp_ci) #final proportion, se & 95% ci
#save
#write.csv(N15.LBP.emp, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N15.LBP.emp.csv")


#ii)  Logistic regression by employment status
N15_LBP.emp_glm <- svyglm(PAINLB~relevel(DOINGLWA, ref = "Working for pay at a job/business") + AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS15_DO)
summary(N15_LBP.emp_glm)
exp(cbind(OR=coef(N15_LBP.emp_glm), confint(N15_LBP.emp_glm)))

#4. Lifestyle trends

#I)  BMI
#logistic regression
N15.BMI.LBP.glm <- svyglm(PAINLB ~ BMI + AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS15_DO)
summary(N15.BMI.LBP.glm)
exp(cbind(OR=coef(N15.BMI.LBP.glm), confint(N15.BMI.LBP.glm)))

#II) Physical activity
#logistic regression
N15.PA.LBP.glm <- svyglm(PAINLB ~ relevel(factor(PAC, ordered = FALSE), ref = "Physically active") + AGE_P + SEX,
                         family = quasibinomial,
                         design = NHIS15_DO)
summary(N15.PA.LBP.glm)
exp(cbind(OR = coef(N15.PA.LBP.glm), confint(N15.PA.LBP.glm)))


#________________________________________________________________________________________________________________________________________________

# == 2014 == #

#1. Overall prevalence
N14_LBP <- svymean(~factor(PAINLB), 
                   NHIS14_DO, 
                   na.rm = TRUE)
N14_LBP.c <- N14_LBP %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N14_LBP_ci <- confint(N14_LBP) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
N14.LBP <- bind_cols(N14_LBP.c, N14_LBP_ci)
#remove LB = 0
N14.LBP <- N14.LBP[-c(1), ] #final proportion, se & 95% ci
#save
#write.csv(N14.LBP, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N14.LBP.csv")


#2. Spatial analysis, by region

#I)   Overall population
#i)   regional prevalence for the overall population
N14_LBP_reg <- svyby(formula = ~PAINLB,
                     by = ~REGION,
                     design = NHIS14_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N14_LBP_reg.c <- N14_LBP_reg %>%
  select(1, 3, 5) %>%
  setNames(c("Region", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N14_LBP_reg.ci <- confint(N14_LBP_reg) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N14_LBP_reg.ci <- N14_LBP_reg.ci[-c(1:4), ]
#join proportion and ci
N14.LBP.region <- bind_cols(N14_LBP_reg.c, N14_LBP_reg.ci) #final proportion, se & 95% ci
#save
#write.csv(N14.LBP.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N14.LBP.region.csv")

#Change the region column to "NAME" to match sf
names(N14.LBP.region)[names(N14.LBP.region) == "Region"] <- "NAME"

N14.LBP.joined <- regions %>%
  left_join(N14.LBP.region)

N14.LBP.joined$NAME <- as.factor(N14.LBP.joined$NAME)

#Plot
LBPregplot14 <- ggplot(data = N14.LBP.joined) +
  geom_sf(aes(fill = Proportion), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  scale_fill_viridis_c(name = "Prevalence (%)", option = "viridis", direction = -1, limits = c(25, 35), breaks = c(25, 30, 35)) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  ggtitle("Regional prevalence (%) of US residents with low back pain in 2014") +
  theme(plot.title = element_text(size = 10, hjust = 0.5))
ggsave("N14.LBP.regchor.png", width = 5, height = 5)


#ii)  logistic regression by region
N14.LBP.reg.glm <- svyglm(relevel(PAINLB, ref = "0")~ REGION + AGE_P,
                          family = quasibinomial,
                          design = NHIS14_DO)
summary(N14.LBP.reg.glm)
exp(cbind(OR=coef(N14.LBP.reg.glm), confint(N14.LBP.reg.glm)))


#II)  prevalence by region, by age

#i)  age-specific regional prevalance
N14_LBP_reg.age <- svyby(formula = ~PAINLB,
                         by = ~REGION + AGE_P,
                         design = NHIS14_DO,
                         FUN = svymean,
                         na.rm = TRUE)
N14_LBP_reg.age.c <- N14_LBP_reg.age %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Age", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Age = as.character(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N14_LBP_reg.age.ci <- confint(N14_LBP_reg.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N14_LBP_reg.age.ci <- N14_LBP_reg.age.ci[-c(1:44), ]
#join proportion and ci
N14.LBP.age.region <- bind_cols(N14_LBP_reg.age.c, N14_LBP_reg.age.ci) #final prevalence, se & 95% ci
#save
#write.csv(N14.LBP.age.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N14.LBP.region.age.csv")

#Geofacet - Age & Region
LBP.Age_Reg <- ggplot(N14.LBP.age.region, aes(Age, Proportion)) +
  geom_bar(colour = "black", stat = "identity", fill = "#6600CC") +
  coord_flip() +
  facet_geo(~Region,
            grid = USregions) +
  theme_bw() +
  theme(text = element_text(colour = "black", size = 20)) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 17)) +
  xlab("Age group (years)") +
  geom_errorbar(aes(ymin = Proportion-SE, ymax = Proportion+SE, width = 0.3)) +
  scale_y_continuous(name = "Prevalence (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 18)) +
  ggtitle("Regional prevalence (%) of US residents with low back pain by age in 2014") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N14.GF_AgeReg.LBP.png", width = 14, height = 9)


#ii) age-specific regional logistic regressions
#18 to 24
sub1824 <- subset(NHIS14_DO, AGE_P == "18 to 24")
N14.LBP.age1824.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub1824)
summary(N14.LBP.age1824.reg)
exp(cbind(OR = coef(N14.LBP.age1824.reg), confint(N14.LBP.age1824.reg)))

#25 to 29
sub2529 <- subset(NHIS14_DO, AGE_P == "25 to 29")
N14.LBP.age2529.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub2529)
summary(N14.LBP.age2529.reg)
exp(cbind(OR = coef(N14.LBP.age2529.reg), confint(N14.LBP.age2529.reg)))

#30 to 34
sub3034 <- subset(NHIS14_DO, AGE_P == "30 to 34")
N14.LB.age3034.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub3034)
summary(N14.LB.age3034.reg)
exp(cbind(OR = coef(N14.LB.age3034.reg), confint(N14.LB.age3034.reg)))

#35 to 39
sub3539 <- subset(NHIS14_DO, AGE_P == "35 to 39")
N14.LBP.age3539.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub3539)
summary(N14.LBP.age3539.reg)
exp(cbind(OR = coef(N14.LBP.age3539.reg), confint(N14.LBP.age3539.reg)))

#40 to 44
sub4044 <- subset(NHIS14_DO, AGE_P == "40 to 44")
N14.LBP.age4044.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub4044)
summary(N14.LBP.age4044.reg)
exp(cbind(OR = coef(N14.LBP.age4044.reg), confint(N14.LBP.age4044.reg)))

#45 to 49
sub4549 <- subset(NHIS14_DO, AGE_P == "45 to 49")
N14.LBP.age4549.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub4549)
summary(N14.LBP.age4549.reg)
exp(cbind(OR = coef(N14.LBP.age4549.reg), confint(N14.LBP.age4549.reg)))

#50 to 54
sub5054 <- subset(NHIS14_DO, AGE_P == "50 to 54")
N14.LBP.age5054.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub5054)
summary(N14.LBP.age5054.reg)
exp(cbind(OR = coef(N14.LBP.age5054.reg), confint(N14.LBP.age5054.reg)))

#55 to 59
sub5559 <- subset(NHIS14_DO, AGE_P == "55 to 59")
N14.LBP.age5559.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub5559)
summary(N14.LBP.age5559.reg)
exp(cbind(OR = coef(N14.LBP.age5559.reg), confint(N14.LBP.age5559.reg)))

#60 to 64
sub6064 <- subset(NHIS14_DO, AGE_P == "60 to 64")
N14.LBP.age6064.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub6064)
summary(N14.LBP.age6064.reg)
exp(cbind(OR = coef(N14.LBP.age6064.reg), confint(N14.LBP.age6064.reg)))

#65 to 69
sub6569 <- subset(NHIS14_DO, AGE_P == "65 to 69")
N14.LBP.age6569.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub6569)
summary(N14.LBP.age6569.reg)
exp(cbind(OR = coef(N14.LBP.age6569.reg), confint(N14.LBP.age6569.reg)))

#70 and above
sub70 <- subset(NHIS14_DO, AGE_P == "70 and above")
N14.LBP.age70.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                            family = quasibinomial,
                            design = sub70)
summary(N14.LBP.age70.reg)
exp(cbind(OR = coef(N14.LBP.age70.reg), confint(N14.LBP.age70.reg)))

#III) prevalence by region, by sex

#i)  sex-specific regional prevalence
N14_LBP_reg.sex <- svyby(formula = ~PAINLB,
                         by = ~REGION + SEX,
                         design = NHIS14_DO,
                         FUN = svymean,
                         na.rm = TRUE)
N14_LBP_reg.sex.c <- N14_LBP_reg.sex %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Sex", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N14_LBP_reg.sex.ci <- confint(N14_LBP_reg.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N14_LBP_reg.sex.ci <- N14_LBP_reg.sex.ci[-c(1:8), ]
#join proportion and ci
N14.LBP.sex.region <- bind_cols(N14_LBP_reg.sex.c, N14_LBP_reg.sex.ci) #final prevalence, se & 95% ci
#save
#write.csv(N14.LBP.sex.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N14.LBP.region.sex.csv")

#Geofacet - Sex & Region
LBP.Sex_Reg14 <- ggplot(N14.LBP.sex.region, aes(Sex, Proportion)) +
  geom_bar(colour = "black", stat = "identity", fill = "#6600CC") +
  coord_flip() +
  facet_geo(~Region,
            grid = USregions) +
  theme_bw() +
  theme(text = element_text(colour = "black", size = 20)) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 17)) +
  xlab("Sex") +
  geom_errorbar(aes(ymin = Proportion-SE, ymax = Proportion+SE, width = 0.3)) +
  scale_y_continuous(name = "Prevalence (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 18)) +
  ggtitle("Regional prevalence (%) of US residents with low back pain by sex by 2014") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N14.GF_SexReg.LBP.png", width = 14, height = 9)


#ii) sex-specific regional logistic regressions
#males
submales <- subset(NHIS14_DO, SEX == "Male")
N14.LBP.sexmale.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + AGE_P,
                              family = quasibinomial,
                              design = submales)
summary(N14.LBP.sexmale.reg)
exp(cbind(OR = coef(N14.LBP.sexmale.reg), confint(N14.LBP.sexmale.reg)))

#females
subfemales <- subset(NHIS14_DO, SEX == "Female")
N14.LBP.sexfemale.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + AGE_P,
                                family = quasibinomial,
                                design = subfemales)
summary(N14.LBP.sexfemale.reg)
exp(cbind(OR = coef(N14.LBP.sexfemale.reg), confint(N14.LBP.sexfemale.reg)))


#3. Demographic trends

#I)  Age trends

#i)  Prevalence by age
N14_LBP_age <- svyby(formula = ~PAINLB,
                     by = ~AGE_P,
                     design = NHIS14_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N14_LBP_age.c <- N14_LBP_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N14_LBP_age_ci <- confint(N14_LBP_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove LBP=0 ci
N14_LBP_age_ci <- N14_LBP_age_ci[-c(1:11), ]
#join proportion and ci
N14_LBP.age <- bind_cols(N14_LBP_age.c, N14_LBP_age_ci) #final proportion, se & 95% ci
#Save
#write.csv(N14_LBP.age, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N14.LBP.age.csv")

#ii) Logistic regression by age
N14_LBP.age_glm <- svyglm(PAINLB~AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS14_DO)
summary(N14_LBP.age_glm)
exp(cbind(OR=coef(N14_LBP.age_glm), confint(N14_LBP.age_glm)))


#II)  Sex trends

#i)  Prevalence by sex
N14_LBP_sex <- svyby(formula = ~PAINLB,
                     by = ~SEX,
                     design = NHIS14_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N14_LBP_sex.c <- N14_LBP_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N14_LBP_sex_ci <- confint(N14_LBP_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N14_LBP_sex_ci <- N14_LBP_sex_ci[-c(1:2), ]
#join proportion and ci
N14.LBP.sex <- bind_cols(N14_LBP_sex.c, N14_LBP_sex_ci) #final proportion, se & 95% ci
#save
#write.csv(N14.LBP.sex, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N14.LBP.sex.csv")

#ii) Logistic regression by sex
N14.LB.sex.glm.c <- svyglm(PAINLB~relevel(SEX, ref = "Male") + AGE_P,
                           family = quasibinomial,
                           design = NHIS14_DO)
summary(N14.LB.sex.glm.c)
exp(cbind(OR=coef(N14.LB.sex.glm.c), confint(N14.LB.sex.glm.c)))



#III) Socioeconomic trends (employment status only)

#i)   Prevalence by employment status
N14_LBP_emp <- svyby(formula = ~PAINLB,
                     by = ~DOINGLWA,
                     design = NHIS14_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N14_LBP_emp.c <- N14_LBP_emp %>%
  select(1, 3, 5) %>%
  setNames(c("Employment", "Proportion", "SE")) %>%
  mutate(Employment = as.factor(Employment)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N14_LBP_emp_ci <- confint(N14_LBP_emp) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for LBP = 0
N14_LBP_emp_ci <- N14_LBP_emp_ci[-c(1:2), ]
#join ci and proportion
N14.LBP.emp <- bind_cols(N14_LBP_emp.c, N14_LBP_emp_ci) #final proportion, se & 95% ci
#save
#write.csv(N14.LBP.emp, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N14.LBP.emp.csv")


#ii)  Logistic regression by employment status
N14_LBP.emp_glm <- svyglm(PAINLB~relevel(DOINGLWA, ref = "Working for pay at a job/business") + AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS14_DO)
summary(N14_LBP.emp_glm)
exp(cbind(OR=coef(N14_LBP.emp_glm), confint(N14_LBP.emp_glm)))

#4. Lifestyle trends

#I)  BMI
#logistic regression
N14.BMI.LBP.glm <- svyglm(PAINLB ~ BMI + AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS14_DO)
summary(N14.BMI.LBP.glm)
exp(cbind(OR=coef(N14.BMI.LBP.glm), confint(N14.BMI.LBP.glm)))

#II) Physical activity
#logistic regression
N14.PA.LBP.glm <- svyglm(PAINLB ~ relevel(factor(PAC, ordered = FALSE), ref = "Physically active") + AGE_P + SEX,
                         family = quasibinomial,
                         design = NHIS14_DO)
summary(N14.PA.LBP.glm)
exp(cbind(OR = coef(N14.PA.LBP.glm), confint(N14.PA.LBP.glm)))


#_______________________________________________________________________________________________________________________________________________


# == 2013 == #


#1. Overall prevalence
N13_LBP <- svymean(~factor(PAINLB), 
                   NHIS13_DO, 
                   na.rm = TRUE)
N13_LBP.c <- N13_LBP %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N13_LBP_ci <- confint(N13_LBP) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
N13.LBP <- bind_cols(N13_LBP.c, N13_LBP_ci)
#remove LB = 0
N13.LBP <- N13.LBP[-c(1), ] #final proportion, se & 95% ci
#save
#write.csv(N13.LBP, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N13.LBP.csv")


#2. Spatial analysis, by region

#I)   Overall population
#i)   regional prevalence for the overall population
N13_LBP_reg <- svyby(formula = ~PAINLB,
                     by = ~REGION,
                     design = NHIS13_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N13_LBP_reg.c <- N13_LBP_reg %>%
  select(1, 3, 5) %>%
  setNames(c("Region", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N13_LBP_reg.ci <- confint(N13_LBP_reg) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N13_LBP_reg.ci <- N13_LBP_reg.ci[-c(1:4), ]
#join proportion and ci
N13.LBP.region <- bind_cols(N13_LBP_reg.c, N13_LBP_reg.ci) #final proportion, se & 95% ci
#save
#write.csv(N13.LBP.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N13.LBP.region.csv")

#Change the region column to "NAME" to match sf
names(N13.LBP.region)[names(N13.LBP.region) == "Region"] <- "NAME"

N13.LBP.joined <- regions %>%
  left_join(N13.LBP.region)

N13.LBP.joined$NAME <- as.factor(N13.LBP.joined$NAME)

#Plot
LBPregplot13 <- ggplot(data = N13.LBP.joined) +
  geom_sf(aes(fill = Proportion), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  scale_fill_viridis_c(name = "Prevalence (%)", option = "viridis", direction = -1, limits = c(25, 35), breaks = c(25, 30, 35)) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  ggtitle("Regional prevalence (%) of US residents with low back pain in 2013") +
  theme(plot.title = element_text(size = 10, hjust = 0.5))
ggsave("N13.LBP.regchor.png", width = 5, height = 5)


#ii)  logistic regression by region
N13.LBP.reg.glm <- svyglm(relevel(PAINLB, ref = "0")~ REGION + AGE_P,
                          family = quasibinomial,
                          design = NHIS13_DO)
summary(N13.LBP.reg.glm)
exp(cbind(OR=coef(N13.LBP.reg.glm), confint(N13.LBP.reg.glm)))


#II)  prevalence by region, by age

#i)  age-specific regional prevalance
N13_LBP_reg.age <- svyby(formula = ~PAINLB,
                         by = ~REGION + AGE_P,
                         design = NHIS13_DO,
                         FUN = svymean,
                         na.rm = TRUE)
N13_LBP_reg.age.c <- N13_LBP_reg.age %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Age", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Age = as.character(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N13_LBP_reg.age.ci <- confint(N13_LBP_reg.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N13_LBP_reg.age.ci <- N13_LBP_reg.age.ci[-c(1:44), ]
#join proportion and ci
N13.LBP.age.region <- bind_cols(N13_LBP_reg.age.c, N13_LBP_reg.age.ci) #final prevalence, se & 95% ci
#save
#write.csv(N13.LBP.age.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N13.LBP.region.age.csv")

#Geofacet - Age & Region
LBP.Age_Reg <- ggplot(N13.LBP.age.region, aes(Age, Proportion)) +
  geom_bar(colour = "black", stat = "identity", fill = "#6600CC") +
  coord_flip() +
  facet_geo(~Region,
            grid = USregions) +
  theme_bw() +
  theme(text = element_text(colour = "black", size = 20)) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 17)) +
  xlab("Age group (years)") +
  geom_errorbar(aes(ymin = Proportion-SE, ymax = Proportion+SE, width = 0.3)) +
  scale_y_continuous(name = "Prevalence (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 18)) +
  ggtitle("Regional prevalence (%) of US residents of low back pain by age in 2013")
ggsave("N13.GF_AgeReg.LBP.png", width = 14, height = 9)


#ii) age-specific regional logistic regressions
#18 to 24
sub1824 <- subset(NHIS13_DO, AGE_P == "18 to 24")
N13.LBP.age1824.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub1824)
summary(N13.LBP.age1824.reg)
exp(cbind(OR = coef(N13.LBP.age1824.reg), confint(N13.LBP.age1824.reg)))

#25 to 29
sub2529 <- subset(NHIS13_DO, AGE_P == "25 to 29")
N13.LBP.age2529.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub2529)
summary(N13.LBP.age2529.reg)
exp(cbind(OR = coef(N13.LBP.age2529.reg), confint(N13.LBP.age2529.reg)))

#30 to 34
sub3034 <- subset(NHIS13_DO, AGE_P == "30 to 34")
N13.LB.age3034.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub3034)
summary(N13.LB.age3034.reg)
exp(cbind(OR = coef(N13.LB.age3034.reg), confint(N13.LB.age3034.reg)))

#35 to 39
sub3539 <- subset(NHIS13_DO, AGE_P == "35 to 39")
N13.LBP.age3539.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub3539)
summary(N13.LBP.age3539.reg)
exp(cbind(OR = coef(N13.LBP.age3539.reg), confint(N13.LBP.age3539.reg)))

#40 to 44
sub4044 <- subset(NHIS13_DO, AGE_P == "40 to 44")
N13.LBP.age4044.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub4044)
summary(N13.LBP.age4044.reg)
exp(cbind(OR = coef(N13.LBP.age4044.reg), confint(N13.LBP.age4044.reg)))

#45 to 49
sub4549 <- subset(NHIS13_DO, AGE_P == "45 to 49")
N13.LBP.age4549.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub4549)
summary(N13.LBP.age4549.reg)
exp(cbind(OR = coef(N13.LBP.age4549.reg), confint(N13.LBP.age4549.reg)))

#50 to 54
sub5054 <- subset(NHIS13_DO, AGE_P == "50 to 54")
N13.LBP.age5054.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub5054)
summary(N13.LBP.age5054.reg)
exp(cbind(OR = coef(N13.LBP.age5054.reg), confint(N13.LBP.age5054.reg)))

#55 to 59
sub5559 <- subset(NHIS13_DO, AGE_P == "55 to 59")
N13.LBP.age5559.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub5559)
summary(N13.LBP.age5559.reg)
exp(cbind(OR = coef(N13.LBP.age5559.reg), confint(N13.LBP.age5559.reg)))

#60 to 64
sub6064 <- subset(NHIS13_DO, AGE_P == "60 to 64")
N13.LBP.age6064.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub6064)
summary(N13.LBP.age6064.reg)
exp(cbind(OR = coef(N13.LBP.age6064.reg), confint(N13.LBP.age6064.reg)))

#65 to 69
sub6569 <- subset(NHIS13_DO, AGE_P == "65 to 69")
N13.LBP.age6569.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub6569)
summary(N13.LBP.age6569.reg)
exp(cbind(OR = coef(N13.LBP.age6569.reg), confint(N13.LBP.age6569.reg)))

#70 and above
sub70 <- subset(NHIS13_DO, AGE_P == "70 and above")
N13.LBP.age70.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                            family = quasibinomial,
                            design = sub70)
summary(N13.LBP.age70.reg)
exp(cbind(OR = coef(N13.LBP.age70.reg), confint(N13.LBP.age70.reg)))

#III) prevalence by region, by sex

#i)  sex-specific regional prevalence
N13_LBP_reg.sex <- svyby(formula = ~PAINLB,
                         by = ~REGION + SEX,
                         design = NHIS13_DO,
                         FUN = svymean,
                         na.rm = TRUE)
N13_LBP_reg.sex.c <- N13_LBP_reg.sex %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Sex", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N13_LBP_reg.sex.ci <- confint(N13_LBP_reg.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N13_LBP_reg.sex.ci <- N13_LBP_reg.sex.ci[-c(1:8), ]
#join proportion and ci
N13.LBP.sex.region <- bind_cols(N13_LBP_reg.sex.c, N13_LBP_reg.sex.ci) #final prevalence, se & 95% ci
#save
#write.csv(N13.LBP.sex.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N13.LBP.region.sex.csv")

#Geofacet - Sex & Region
LBP.Sex_Reg13 <- ggplot(N13.LBP.sex.region, aes(Sex, Proportion)) +
  geom_bar(colour = "black", stat = "identity", fill = "#6600CC") +
  coord_flip() +
  facet_geo(~Region,
            grid = USregions) +
  theme_bw() +
  theme(text = element_text(colour = "black", size = 20)) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 17)) +
  xlab("Sex") +
  geom_errorbar(aes(ymin = Proportion-SE, ymax = Proportion+SE, width = 0.3)) +
  scale_y_continuous(name = "Prevalence (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 18)) +
  ggtitle("Regional prevalence (%) of US residents with low back pain by sex in 2013") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N13.GF_SexReg.LBP.png", width = 14, height = 9)


#ii) sex-specific regional logistic regressions
#males
submales <- subset(NHIS13_DO, SEX == "Male")
N13.LBP.sexmale.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + AGE_P,
                              family = quasibinomial,
                              design = submales)
summary(N13.LBP.sexmale.reg)
exp(cbind(OR = coef(N13.LBP.sexmale.reg), confint(N13.LBP.sexmale.reg)))

#females
subfemales <- subset(NHIS13_DO, SEX == "Female")
N13.LBP.sexfemale.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + AGE_P,
                                family = quasibinomial,
                                design = subfemales)
summary(N13.LBP.sexfemale.reg)
exp(cbind(OR = coef(N13.LBP.sexfemale.reg), confint(N13.LBP.sexfemale.reg)))


#3. Demographic trends

#I)  Age trends

#i)  Prevalence by age
N13_LBP_age <- svyby(formula = ~PAINLB,
                     by = ~AGE_P,
                     design = NHIS13_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N13_LBP_age.c <- N13_LBP_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N13_LBP_age_ci <- confint(N13_LBP_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove LBP=0 ci
N13_LBP_age_ci <- N13_LBP_age_ci[-c(1:11), ]
#join proportion and ci
N13_LBP.age <- bind_cols(N13_LBP_age.c, N13_LBP_age_ci) #final proportion, se & 95% ci
#Save
#write.csv(N13_LBP.age, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N13.LBP.age.csv")

#ii) Logistic regression by age
N13_LBP.age_glm <- svyglm(PAINLB~AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS13_DO)
summary(N13_LBP.age_glm)
exp(cbind(OR=coef(N13_LBP.age_glm), confint(N13_LBP.age_glm)))


#II)  Sex trends

#i)  Prevalence by sex
N13_LBP_sex <- svyby(formula = ~PAINLB,
                     by = ~SEX,
                     design = NHIS13_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N13_LBP_sex.c <- N13_LBP_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N13_LBP_sex_ci <- confint(N13_LBP_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N13_LBP_sex_ci <- N13_LBP_sex_ci[-c(1:2), ]
#join proportion and ci
N13.LBP.sex <- bind_cols(N13_LBP_sex.c, N13_LBP_sex_ci) #final proportion, se & 95% ci
#save
#write.csv(N13.LBP.sex, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N13.LBP.sex.csv")

#ii) Logistic regression by sex
N13.LB.sex.glm.c <- svyglm(PAINLB~relevel(SEX, ref = "Male") + AGE_P,
                           family = quasibinomial,
                           design = NHIS13_DO)
summary(N13.LB.sex.glm.c)
exp(cbind(OR=coef(N13.LB.sex.glm.c), confint(N13.LB.sex.glm.c)))

#III) Socioeconomic trends (employment status only)

#i)   Prevalence by employment status
N13_LBP_emp <- svyby(formula = ~PAINLB,
                     by = ~DOINGLWA,
                     design = NHIS13_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N13_LBP_emp.c <- N13_LBP_emp %>%
  select(1, 3, 5) %>%
  setNames(c("Employment", "Proportion", "SE")) %>%
  mutate(Employment = as.factor(Employment)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N13_LBP_emp_ci <- confint(N13_LBP_emp) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for LBP = 0
N13_LBP_emp_ci <- N13_LBP_emp_ci[-c(1:2), ]
#join ci and proportion
N13.LBP.emp <- bind_cols(N13_LBP_emp.c, N13_LBP_emp_ci) #final proportion, se & 95% ci
#save
#write.csv(N13.LBP.emp, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N13.LBP.emp.csv")


#ii)  Logistic regression by employment status
N13_LBP.emp_glm <- svyglm(PAINLB~relevel(DOINGLWA, ref = "Working for pay at a job/business") + AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS13_DO)
summary(N13_LBP.emp_glm)
exp(cbind(OR=coef(N13_LBP.emp_glm), confint(N13_LBP.emp_glm)))

#4. Lifestyle trends

#I)  BMI
#logistic regression
N13.BMI.LBP.glm <- svyglm(PAINLB ~ BMI + AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS13_DO)
summary(N13.BMI.LBP.glm)
exp(cbind(OR=coef(N13.BMI.LBP.glm), confint(N13.BMI.LBP.glm)))

#II) Physical activity
#logistic regression
N13.PA.LBP.glm <- svyglm(PAINLB ~ relevel(factor(PAC, ordered = FALSE), ref = "Physically active") + AGE_P + SEX,
                         family = quasibinomial,
                         design = NHIS13_DO)
summary(N13.PA.LBP.glm)
exp(cbind(OR = coef(N13.PA.LBP.glm), confint(N13.PA.LBP.glm)))

#________________________________________________________________________________________________________________________________________________

# == 2012 == #

#1. Overall prevalence
N12_LBP <- svymean(~factor(PAINLB), 
                   NHIS12_DO, 
                   na.rm = TRUE)
N12_LBP.c <- N12_LBP %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N12_LBP_ci <- confint(N12_LBP) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
N12.LBP <- bind_cols(N12_LBP.c, N12_LBP_ci)
#remove LB = 0
N12.LBP <- N12.LBP[-c(1), ] #final proportion, se & 95% ci
#save
#write.csv(N12.LBP, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N12.LBP.csv")


#2. Spatial analysis, by region

#I)   Overall population
#i)   regional prevalence for the overall population
N12_LBP_reg <- svyby(formula = ~PAINLB,
                     by = ~REGION,
                     design = NHIS12_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N12_LBP_reg.c <- N12_LBP_reg %>%
  select(1, 3, 5) %>%
  setNames(c("Region", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N12_LBP_reg.ci <- confint(N12_LBP_reg) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N12_LBP_reg.ci <- N12_LBP_reg.ci[-c(1:4), ]
#join proportion and ci
N12.LBP.region <- bind_cols(N12_LBP_reg.c, N12_LBP_reg.ci) #final proportion, se & 95% ci
#save
#write.csv(N12.LBP.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N12.LBP.region.csv")

#Change the region column to "NAME" to match sf
names(N12.LBP.region)[names(N12.LBP.region) == "Region"] <- "NAME"

N12.LBP.joined <- regions %>%
  left_join(N12.LBP.region)

N12.LBP.joined$NAME <- as.factor(N12.LBP.joined$NAME)

#Plot
LBPregplot12 <- ggplot(data = N12.LBP.joined) +
  geom_sf(aes(fill = Proportion), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  scale_fill_viridis_c(name = "Prevalence (%)", option = "viridis", direction = -1, limits = c(25, 35), breaks = c(25, 30, 35)) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  ggtitle("Regional prevalence (%) of US residents with low back pain in 2012") +
  theme(plot.title = element_text(size = 10, hjust = 0.5))
ggsave("N12.LBP.regchor.png", width = 5, height = 5)


#ii)  logistic regression by region
N12.LBP.reg.glm <- svyglm(relevel(PAINLB, ref = "0")~ REGION + AGE_P,
                          family = quasibinomial,
                          design = NHIS12_DO)
summary(N12.LBP.reg.glm)
exp(cbind(OR=coef(N12.LBP.reg.glm), confint(N12.LBP.reg.glm)))


#II)  prevalence by region, by age

#i)  age-specific regional prevalance
N12_LBP_reg.age <- svyby(formula = ~PAINLB,
                         by = ~REGION + AGE_P,
                         design = NHIS12_DO,
                         FUN = svymean,
                         na.rm = TRUE)
N12_LBP_reg.age.c <- N12_LBP_reg.age %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Age", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Age = as.character(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N12_LBP_reg.age.ci <- confint(N12_LBP_reg.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N12_LBP_reg.age.ci <- N12_LBP_reg.age.ci[-c(1:44), ]
#join proportion and ci
N12.LBP.age.region <- bind_cols(N12_LBP_reg.age.c, N12_LBP_reg.age.ci) #final prevalence, se & 95% ci
#save
#write.csv(N12.LBP.age.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N12.LBP.region.age.csv")

#Geofacet - Age & Region
LBP.Age_Reg <- ggplot(N12.LBP.age.region, aes(Age, Proportion)) +
  geom_bar(colour = "black", stat = "identity", fill = "#6600CC") +
  coord_flip() +
  facet_geo(~Region,
            grid = USregions) +
  theme_bw() +
  theme(text = element_text(colour = "black", size = 20)) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 17)) +
  xlab("Age group (years)") +
  geom_errorbar(aes(ymin = Proportion-SE, ymax = Proportion+SE, width = 0.3)) +
  scale_y_continuous(name = "Prevalence (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 18)) +
  ggtitle("Regional prevalence (%) of US residents with low back pain by age in 2012") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N12.GF_AgeReg.LBP.png", width = 14, height = 9)


#ii) age-specific regional logistic regressions
#18 to 24
sub1824 <- subset(NHIS12_DO, AGE_P == "18 to 24")
N12.LBP.age1824.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub1824)
summary(N12.LBP.age1824.reg)
exp(cbind(OR = coef(N12.LBP.age1824.reg), confint(N12.LBP.age1824.reg)))

#25 to 29
sub2529 <- subset(NHIS12_DO, AGE_P == "25 to 29")
N12.LBP.age2529.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub2529)
summary(N12.LBP.age2529.reg)
exp(cbind(OR = coef(N12.LBP.age2529.reg), confint(N12.LBP.age2529.reg)))

#30 to 34
sub3034 <- subset(NHIS12_DO, AGE_P == "30 to 34")
N12.LB.age3034.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub3034)
summary(N12.LB.age3034.reg)
exp(cbind(OR = coef(N12.LB.age3034.reg), confint(N12.LB.age3034.reg)))

#35 to 39
sub3539 <- subset(NHIS12_DO, AGE_P == "35 to 39")
N12.LBP.age3539.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub3539)
summary(N12.LBP.age3539.reg)
exp(cbind(OR = coef(N12.LBP.age3539.reg), confint(N12.LBP.age3539.reg)))

#40 to 44
sub44044 <- subset(NHIS12_DO, AGE_P == "40 to 44")
N12.LBP.age4044.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub4044)
summary(N12.LBP.age4044.reg)
exp(cbind(OR = coef(N12.LBP.age4044.reg), confint(N12.LBP.age4044.reg)))

#45 to 49
sub4549 <- subset(NHIS12_DO, AGE_P == "45 to 49")
N12.LBP.age4549.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub4549)
summary(N12.LBP.age4549.reg)
exp(cbind(OR = coef(N12.LBP.age4549.reg), confint(N12.LBP.age4549.reg)))

#50 to 54
sub5054 <- subset(NHIS12_DO, AGE_P == "50 to 54")
N12.LBP.age5054.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub5054)
summary(N12.LBP.age5054.reg)
exp(cbind(OR = coef(N12.LBP.age5054.reg), confint(N12.LBP.age5054.reg)))

#55 to 59
sub5559 <- subset(NHIS12_DO, AGE_P == "55 to 59")
N12.LBP.age5559.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub5559)
summary(N12.LBP.age5559.reg)
exp(cbind(OR = coef(N12.LBP.age5559.reg), confint(N12.LBP.age5559.reg)))

#60 to 64
sub6064 <- subset(NHIS12_DO, AGE_P == "60 to 64")
N12.LBP.age6064.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub6064)
summary(N12.LBP.age6064.reg)
exp(cbind(OR = coef(N12.LBP.age6064.reg), confint(N12.LBP.age6064.reg)))

#65 to 69
sub6569 <- subset(NHIS12_DO, AGE_P == "65 to 69")
N12.LBP.age6569.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub6569)
summary(N12.LBP.age6569.reg)
exp(cbind(OR = coef(N12.LBP.age6569.reg), confint(N12.LBP.age6569.reg)))

#70 and above
sub70 <- subset(NHIS12_DO, AGE_P == "70 and above")
N12.LBP.age70.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                            family = quasibinomial,
                            design = sub70)
summary(N12.LBP.age70.reg)
exp(cbind(OR = coef(N12.LBP.age70.reg), confint(N12.LBP.age70.reg)))

#III) prevalence by region, by sex

#i)  sex-specific regional prevalence
N12_LBP_reg.sex <- svyby(formula = ~PAINLB,
                         by = ~REGION + SEX,
                         design = NHIS12_DO,
                         FUN = svymean,
                         na.rm = TRUE)
N12_LBP_reg.sex.c <- N12_LBP_reg.sex %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Sex", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N12_LBP_reg.sex.ci <- confint(N12_LBP_reg.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N12_LBP_reg.sex.ci <- N12_LBP_reg.sex.ci[-c(1:8), ]
#join proportion and ci
N12.LBP.sex.region <- bind_cols(N12_LBP_reg.sex.c, N12_LBP_reg.sex.ci) #final prevalence, se & 95% ci
#save
#write.csv(N12.LBP.sex.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N12.LBP.region.sex.csv")

#Geofacet - Sex & Region
LBP.Sex_Reg12 <- ggplot(N12.LBP.sex.region, aes(Sex, Proportion)) +
  geom_bar(colour = "black", stat = "identity", fill = "#6600CC") +
  coord_flip() +
  facet_geo(~Region,
            grid = USregions) +
  theme_bw() +
  theme(text = element_text(colour = "black", size = 20)) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 17)) +
  xlab("Sex") +
  geom_errorbar(aes(ymin = Proportion-SE, ymax = Proportion+SE, width = 0.3)) +
  scale_y_continuous(name = "Prevalence (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 18)) +
  ggtitle("Regional prevalence (%) of US residents with low back pain by sex in 2012") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N12.GF_SexReg.LBP.png", width = 14, height = 9)


#ii) sex-specific regional logistic regressions
#males
submales <- subset(NHIS12_DO, SEX == "Male")
N12.LBP.sexmale.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + AGE_P,
                              family = quasibinomial,
                              design = submales)
summary(N12.LBP.sexmale.reg)
exp(cbind(OR = coef(N12.LBP.sexmale.reg), confint(N12.LBP.sexmale.reg)))

#females
subfemales <- subset(NHIS12_DO, SEX == "Female")
N12.LBP.sexfemale.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + AGE_P,
                                family = quasibinomial,
                                design = subfemales)
summary(N12.LBP.sexfemale.reg)
exp(cbind(OR = coef(N12.LBP.sexfemale.reg), confint(N12.LBP.sexfemale.reg)))


#3. Demographic trends

#I)  Age trends

#i)  Prevalence by age
N12_LBP_age <- svyby(formula = ~PAINLB,
                     by = ~AGE_P,
                     design = NHIS12_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N12_LBP_age.c <- N12_LBP_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N12_LBP_age_ci <- confint(N12_LBP_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove LBP=0 ci
N12_LBP_age_ci <- N12_LBP_age_ci[-c(1:11), ]
#join proportion and ci
N12_LBP.age <- bind_cols(N12_LBP_age.c, N12_LBP_age_ci) #final proportion, se & 95% ci
#Save
#write.csv(N12_LBP.age, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N12.LBP.age.csv")

#ii) Logistic regression by age
N12_LBP.age_glm <- svyglm(PAINLB~AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS12_DO)
summary(N12_LBP.age_glm)
exp(cbind(OR=coef(N12_LBP.age_glm), confint(N12_LBP.age_glm)))


#II)  Sex trends

#i)  Prevalence by sex
N12_LBP_sex <- svyby(formula = ~PAINLB,
                     by = ~SEX,
                     design = NHIS12_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N12_LBP_sex.c <- N12_LBP_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N12_LBP_sex_ci <- confint(N12_LBP_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N12_LBP_sex_ci <- N12_LBP_sex_ci[-c(1:2), ]
#join proportion and ci
N12.LBP.sex <- bind_cols(N12_LBP_sex.c, N12_LBP_sex_ci) #final proportion, se & 95% ci
#save
#write.csv(N12.LBP.sex, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N12.LBP.sex.csv")

#ii) Logistic regression by sex
N12.LB.sex.glm.c <- svyglm(PAINLB~relevel(SEX, ref = "Male") + AGE_P,
                           family = quasibinomial,
                           design = NHIS12_DO)
summary(N12.LB.sex.glm.c)
exp(cbind(OR=coef(N12.LB.sex.glm.c), confint(N12.LB.sex.glm.c)))

#III) Socioeconomic trends (employment status only)

#i)   Prevalence by employment status
N12_LBP_emp <- svyby(formula = ~PAINLB,
                     by = ~DOINGLWA,
                     design = NHIS12_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N12_LBP_emp.c <- N12_LBP_emp %>%
  select(1, 3, 5) %>%
  setNames(c("Employment", "Proportion", "SE")) %>%
  mutate(Employment = as.factor(Employment)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N12_LBP_emp_ci <- confint(N12_LBP_emp) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for LBP = 0
N12_LBP_emp_ci <- N12_LBP_emp_ci[-c(1:2), ]
#join ci and proportion
N12.LBP.emp <- bind_cols(N12_LBP_emp.c, N12_LBP_emp_ci) #final proportion, se & 95% ci
#save
#write.csv(N12.LBP.emp, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N12.LBP.emp.csv")


#ii)  Logistic regression by employment status
N12_LBP.emp_glm <- svyglm(PAINLB~relevel(DOINGLWA, ref = "Working for pay at a job/business") + AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS12_DO)
summary(N12_LBP.emp_glm)
exp(cbind(OR=coef(N12_LBP.emp_glm), confint(N12_LBP.emp_glm)))

#4. Lifestyle trends

#I)  BMI
#logistic regression
N12.BMI.LBP.glm <- svyglm(PAINLB ~ BMI + AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS12_DO)
summary(N12.BMI.LBP.glm)
exp(cbind(OR=coef(N12.BMI.LBP.glm), confint(N12.BMI.LBP.glm)))

#II) Physical activity
#logistic regression
N12.PA.LBP.glm <- svyglm(PAINLB ~ relevel(factor(PAC, ordered = FALSE), ref = "Physically active") + AGE_P + SEX,
                         family = quasibinomial,
                         design = NHIS12_DO)
summary(N12.PA.LBP.glm)
exp(cbind(OR = coef(N12.PA.LBP.glm), confint(N12.PA.LBP.glm)))


#_______________________________________________________________________________________________________________________________________________

# == 2011 == #

#1. Overall prevalence
N11_LBP <- svymean(~factor(PAINLB), 
                   NHIS11_DO, 
                   na.rm = TRUE)
N11_LBP.c <- N11_LBP %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N11_LBP_ci <- confint(N11_LBP) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
N11.LBP <- bind_cols(N11_LBP.c, N11_LBP_ci)
#remove LB = 0
N11.LBP <- N11.LBP[-c(1), ] #final proportion, se & 95% ci
#save
#write.csv(N11.LBP, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N11.LBP.csv")


#2. Spatial analysis, by region

#I)   Overall population
#i)   regional prevalence for the overall population
N11_LBP_reg <- svyby(formula = ~PAINLB,
                     by = ~REGION,
                     design = NHIS11_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N11_LBP_reg.c <- N11_LBP_reg %>%
  select(1, 3, 5) %>%
  setNames(c("Region", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N11_LBP_reg.ci <- confint(N11_LBP_reg) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N11_LBP_reg.ci <- N11_LBP_reg.ci[-c(1:4), ]
#join proportion and ci
N11.LBP.region <- bind_cols(N11_LBP_reg.c, N11_LBP_reg.ci) #final proportion, se & 95% ci
#save
#write.csv(N11.LBP.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N11.LBP.region.csv")

#Change the region column to "NAME" to match sf
names(N11.LBP.region)[names(N11.LBP.region) == "Region"] <- "NAME"

N11.LBP.joined <- regions %>%
  left_join(N11.LBP.region)

N11.LBP.joined$NAME <- as.factor(N11.LBP.joined$NAME)

#Plot
LBPregplot11 <- ggplot(data = N11.LBP.joined) +
  geom_sf(aes(fill = Proportion), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  scale_fill_viridis_c(name = "Prevalence (%)", option = "viridis", direction = -1, limits = c(25, 35), breaks = c(25, 30, 35)) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  ggtitle("Regional prevalence (%) of US residents with low back pain in 2011") +
  theme(plot.title = element_text(size = 10, hjust = 0.5))
ggsave("N11.LBP.regchor.png", width = 5, height = 5)


#ii)  logistic regression by region
N11.LBP.reg.glm <- svyglm(relevel(PAINLB, ref = "0")~ REGION + AGE_P,
                          family = quasibinomial,
                          design = NHIS11_DO)
summary(N11.LBP.reg.glm)
exp(cbind(OR=coef(N11.LBP.reg.glm), confint(N11.LBP.reg.glm)))


#II)  prevalence by region, by age

#i)  age-specific regional prevalance
N11_LBP_reg.age <- svyby(formula = ~PAINLB,
                         by = ~REGION + AGE_P,
                         design = NHIS11_DO,
                         FUN = svymean,
                         na.rm = TRUE)
N11_LBP_reg.age.c <- N11_LBP_reg.age %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Age", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Age = as.character(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N11_LBP_reg.age.ci <- confint(N11_LBP_reg.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N11_LBP_reg.age.ci <- N11_LBP_reg.age.ci[-c(1:44), ]
#join proportion and ci
N11.LBP.age.region <- bind_cols(N11_LBP_reg.age.c, N11_LBP_reg.age.ci) #final prevalence, se & 95% ci
#save
#write.csv(N11.LBP.age.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N11.LBP.region.age.csv")

#Geofacet - Age & Region
LBP.Age_Reg <- ggplot(N11.LBP.age.region, aes(Age, Proportion)) +
  geom_bar(colour = "black", stat = "identity", fill = "#6600CC") +
  coord_flip() +
  facet_geo(~Region,
            grid = USregions) +
  theme_bw() +
  theme(text = element_text(colour = "black", size = 20)) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 17)) +
  xlab("Age group (years)") +
  geom_errorbar(aes(ymin = Proportion-SE, ymax = Proportion+SE, width = 0.3)) +
  scale_y_continuous(name = "Prevalence (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 18)) +
  ggtitle("Regional prevalence (%) of US residents of low back pain by age in 2011") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N11.GF_AgeReg.LBP.png", width = 14, height = 9)


#ii) age-specific regional logistic regressions
#18 to 24
sub1824 <- subset(NHIS11_DO, AGE_P == "18 to 24")
N11.LBP.age1824.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub1824)
summary(N11.LBP.age1824.reg)
exp(cbind(OR = coef(N11.LBP.age1824.reg), confint(N11.LBP.age1824.reg)))

#25 to 29
sub2529 <- subset(NHIS11_DO, AGE_P == "25 to 29")
N11.LBP.age2529.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub2529)
summary(N11.LBP.age2529.reg)
exp(cbind(OR = coef(N11.LBP.age2529.reg), confint(N11.LBP.age2529.reg)))

#30 to 34
sub3034 <- subset(NHIS11_DO, AGE_P == "30 to 34")
N11.LB.age3034.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub3034)
summary(N11.LB.age3034.reg)
exp(cbind(OR = coef(N11.LB.age3034.reg), confint(N11.LB.age3034.reg)))

#35 to 39
sub3539 <- subset(NHIS11_DO, AGE_P == "35 to 39")
N11.LBP.age3539.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub3539)
summary(N11.LBP.age3539.reg)
exp(cbind(OR = coef(N11.LBP.age3539.reg), confint(N11.LBP.age3539.reg)))

#40 to 44
sub4044 <- subset(NHIS11_DO, AGE_P == "40 to 44")
N11.LBP.age4044.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub4044)
summary(N11.LBP.age4044.reg)
exp(cbind(OR = coef(N11.LBP.age4044.reg), confint(N11.LBP.age4044.reg)))

#45 to 49
sub4549 <- subset(NHIS11_DO, AGE_P == "45 to 49")
N11.LBP.age4549.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub4549)
summary(N11.LBP.age4549.reg)
exp(cbind(OR = coef(N11.LBP.age4549.reg), confint(N11.LBP.age4549.reg)))

#50 to 54
sub5054 <- subset(NHIS11_DO, AGE_P == "50 to 54")
N11.LBP.age5054.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub5054)
summary(N11.LBP.age5054.reg)
exp(cbind(OR = coef(N11.LBP.age5054.reg), confint(N11.LBP.age5054.reg)))

#55 to 59
sub5559 <- subset(NHIS11_DO, AGE_P == "55 to 59")
N11.LBP.age5559.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub5559)
summary(N11.LBP.age5559.reg)
exp(cbind(OR = coef(N11.LBP.age5559.reg), confint(N11.LBP.age5559.reg)))

#60 to 64
sub6064 <- subset(NHIS11_DO, AGE_P == "60 to 64")
N11.LBP.age6064.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub6064)
summary(N11.LBP.age6064.reg)
exp(cbind(OR = coef(N11.LBP.age6064.reg), confint(N11.LBP.age6064.reg)))

#65 to 69
sub6569 <- subset(NHIS11_DO, AGE_P == "65 to 69")
N11.LBP.age6569.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub6569)
summary(N11.LBP.age6569.reg)
exp(cbind(OR = coef(N11.LBP.age6569.reg), confint(N11.LBP.age6569.reg)))

#70 and above
sub70 <- subset(NHIS11_DO, AGE_P == "70 and above")
N11.LBP.age70.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                            family = quasibinomial,
                            design = sub70)
summary(N11.LBP.age70.reg)
exp(cbind(OR = coef(N11.LBP.age70.reg), confint(N11.LBP.age70.reg)))

#III) prevalence by region, by sex

#i)  sex-specific regional prevalence
N11_LBP_reg.sex <- svyby(formula = ~PAINLB,
                         by = ~REGION + SEX,
                         design = NHIS11_DO,
                         FUN = svymean,
                         na.rm = TRUE)
N11_LBP_reg.sex.c <- N11_LBP_reg.sex %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Sex", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N11_LBP_reg.sex.ci <- confint(N11_LBP_reg.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N11_LBP_reg.sex.ci <- N11_LBP_reg.sex.ci[-c(1:8), ]
#join proportion and ci
N11.LBP.sex.region <- bind_cols(N11_LBP_reg.sex.c, N11_LBP_reg.sex.ci) #final prevalence, se & 95% ci
#save
#write.csv(N11.LBP.sex.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N11.LBP.region.sex.csv")

#Geofacet - Sex & Region
LBP.Sex_Reg11 <- ggplot(N11.LBP.sex.region, aes(Sex, Proportion)) +
  geom_bar(colour = "black", stat = "identity", fill = "#6600CC") +
  coord_flip() +
  facet_geo(~Region,
            grid = USregions) +
  theme_bw() +
  theme(text = element_text(colour = "black", size = 20)) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 17)) +
  xlab("Sex") +
  geom_errorbar(aes(ymin = Proportion-SE, ymax = Proportion+SE, width = 0.3)) +
  scale_y_continuous(name = "Prevalence (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 18)) +
  ggtitle("Regional prevalence (%) of US residents with low back pain by sex in 2011") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N11.GF_SexReg.LBP.png", width = 14, height = 9)


#ii) sex-specific regional logistic regressions
#males
submales <- subset(NHIS11_DO, SEX == "Male")
N11.LBP.sexmale.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + AGE_P,
                              family = quasibinomial,
                              design = submales)
summary(N11.LBP.sexmale.reg)
exp(cbind(OR = coef(N11.LBP.sexmale.reg), confint(N11.LBP.sexmale.reg)))

#females
subfemales <- subset(NHIS11_DO, SEX == "Female")
N11.LBP.sexfemale.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + AGE_P,
                                family = quasibinomial,
                                design = subfemales)
summary(N11.LBP.sexfemale.reg)
exp(cbind(OR = coef(N11.LBP.sexfemale.reg), confint(N11.LBP.sexfemale.reg)))


#3. Demographic trends

#I)  Age trends

#i)  Prevalence by age
N11_LBP_age <- svyby(formula = ~PAINLB,
                     by = ~AGE_P,
                     design = NHIS11_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N11_LBP_age.c <- N11_LBP_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N11_LBP_age_ci <- confint(N11_LBP_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove LBP=0 ci
N11_LBP_age_ci <- N11_LBP_age_ci[-c(1:11), ]
#join proportion and ci
N11_LBP.age <- bind_cols(N11_LBP_age.c, N11_LBP_age_ci) #final proportion, se & 95% ci
#Save
#write.csv(N11_LBP.age, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N11.LBP.age.csv")

#ii) Logistic regression by age
N11_LBP.age_glm <- svyglm(PAINLB~AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS11_DO)
summary(N11_LBP.age_glm)
exp(cbind(OR=coef(N11_LBP.age_glm), confint(N11_LBP.age_glm)))


#II)  Sex trends

#i)  Prevalence by sex
N11_LBP_sex <- svyby(formula = ~PAINLB,
                     by = ~SEX,
                     design = NHIS11_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N11_LBP_sex.c <- N11_LBP_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N11_LBP_sex_ci <- confint(N11_LBP_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N11_LBP_sex_ci <- N11_LBP_sex_ci[-c(1:2), ]
#join proportion and ci
N11.LBP.sex <- bind_cols(N11_LBP_sex.c, N11_LBP_sex_ci) #final proportion, se & 95% ci
#save
#write.csv(N11.LBP.sex, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N11.LBP.sex.csv")

#ii) Logistic regression by sex
N11.LB.sex.glm.c <- svyglm(PAINLB~relevel(SEX, ref = "Male") + AGE_P,
                           family = quasibinomial,
                           design = NHIS11_DO)
summary(N11.LB.sex.glm.c)
exp(cbind(OR=coef(N11.LB.sex.glm.c), confint(N11.LB.sex.glm.c)))


#III) Socioeconomic trends (employment status only)

#i)   Prevalence by employment status
N11_LBP_emp <- svyby(formula = ~PAINLB,
                     by = ~DOINGLWA,
                     design = NHIS11_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N11_LBP_emp.c <- N11_LBP_emp %>%
  select(1, 3, 5) %>%
  setNames(c("Employment", "Proportion", "SE")) %>%
  mutate(Employment = as.factor(Employment)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N11_LBP_emp_ci <- confint(N11_LBP_emp) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for LBP = 0
N11_LBP_emp_ci <- N11_LBP_emp_ci[-c(1:2), ]
#join ci and proportion
N11.LBP.emp <- bind_cols(N11_LBP_emp.c, N11_LBP_emp_ci) #final proportion, se & 95% ci
#save
#write.csv(N11.LBP.emp, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N11.LBP.emp.csv")


#ii)  Logistic regression by employment status
N11_LBP.emp_glm <- svyglm(PAINLB~relevel(DOINGLWA, ref = "Working for pay at a job/business") + AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS11_DO)
summary(N11_LBP.emp_glm)
exp(cbind(OR=coef(N11_LBP.emp_glm), confint(N11_LBP.emp_glm)))

#4. Lifestyle trends

#I)  BMI
#logistic regression
N11.BMI.LBP.glm <- svyglm(PAINLB ~ BMI + AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS11_DO)
summary(N11.BMI.LBP.glm)
exp(cbind(OR=coef(N11.BMI.LBP.glm), confint(N11.BMI.LBP.glm)))

#II) Physical activity
#logistic regression
N11.PA.LBP.glm <- svyglm(PAINLB ~ relevel(factor(PAC, ordered = FALSE), ref = "Physically active") + AGE_P + SEX,
                         family = quasibinomial,
                         design = NHIS11_DO)
summary(N11.PA.LBP.glm)
exp(cbind(OR = coef(N11.PA.LBP.glm), confint(N11.PA.LBP.glm)))


#________________________________________________________________________________________________________________________________________________

# == 2010 == #

#1. Overall prevalence
N10_LBP <- svymean(~factor(PAINLB), 
                   NHIS10_DO, 
                   na.rm = TRUE)
N10_LBP.c <- N10_LBP %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N10_LBP_ci <- confint(N10_LBP) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
N10.LBP <- bind_cols(N10_LBP.c, N10_LBP_ci)
#remove LB = 0
N10.LBP <- N10.LBP[-c(1), ] #final proportion, se & 95% ci
#save
#write.csv(N10.LBP, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N10.LBP.csv")


#2. Spatial analysis, by region

#I)   Overall population
#i)   regional prevalence for the overall population
N10_LBP_reg <- svyby(formula = ~PAINLB,
                     by = ~REGION,
                     design = NHIS10_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N10_LBP_reg.c <- N10_LBP_reg %>%
  select(1, 3, 5) %>%
  setNames(c("Region", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N10_LBP_reg.ci <- confint(N10_LBP_reg) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N10_LBP_reg.ci <- N10_LBP_reg.ci[-c(1:4), ]
#join proportion and ci
N10.LBP.region <- bind_cols(N10_LBP_reg.c, N10_LBP_reg.ci) #final proportion, se & 95% ci
#save
#write.csv(N10.LBP.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N10.LBP.region.csv")

#Change the region column to "NAME" to match sf
names(N10.LBP.region)[names(N10.LBP.region) == "Region"] <- "NAME"

N10.LBP.joined <- regions %>%
  left_join(N10.LBP.region)

N10.LBP.joined$NAME <- as.factor(N10.LBP.joined$NAME)

#Plot
LBPregplot10 <- ggplot(data = N10.LBP.joined) +
  geom_sf(aes(fill = Proportion), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  scale_fill_viridis_c(name = "Prevalence (%)", option = "viridis", direction = -1, limits = c(25, 35), breaks = c(25, 30, 35)) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  ggtitle("Regional prevalence (%) of US residents with low back pain in 2010") +
  theme(plot.title = element_text(size = 10, hjust = 0.5))
ggsave("N10.LBP.regchor.png", width = 5, height = 5)


#ii)  logistic regression by region
N10.LBP.reg.glm <- svyglm(relevel(PAINLB, ref = "0")~ REGION + AGE_P,
                          family = quasibinomial,
                          design = NHIS10_DO)
summary(N10.LBP.reg.glm)
exp(cbind(OR=coef(N10.LBP.reg.glm), confint(N10.LBP.reg.glm)))


#II)  prevalence by region, by age

#i)  age-specific regional prevalance
N10_LBP_reg.age <- svyby(formula = ~PAINLB,
                         by = ~REGION + AGE_P,
                         design = NHIS10_DO,
                         FUN = svymean,
                         na.rm = TRUE)
N10_LBP_reg.age.c <- N10_LBP_reg.age %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Age", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Age = as.character(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N10_LBP_reg.age.ci <- confint(N10_LBP_reg.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N10_LBP_reg.age.ci <- N10_LBP_reg.age.ci[-c(1:44), ]
#join proportion and ci
N10.LBP.age.region <- bind_cols(N10_LBP_reg.age.c, N10_LBP_reg.age.ci) #final prevalence, se & 95% ci
#save
#write.csv(N10.LBP.age.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N10.LBP.region.age.csv")

#Geofacet - Age & Region
LBP.Age_Reg <- ggplot(N10.LBP.age.region, aes(Age, Proportion)) +
  geom_bar(colour = "black", stat = "identity", fill = "#6600CC") +
  coord_flip() +
  facet_geo(~Region,
            grid = USregions) +
  theme_bw() +
  theme(text = element_text(colour = "black", size = 20)) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 17)) +
  xlab("Age group (years)") +
  geom_errorbar(aes(ymin = Proportion-SE, ymax = Proportion+SE, width = 0.3)) +
  scale_y_continuous(name = "Prevalence (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 18)) +
  ggtitle("Regional prevalence (%) of US residents with low back pain by age in 2010") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N10.GF_AgeReg.LBP.png", width = 14, height = 9)


#ii) age-specific regional logistic regressions
#18 to 24
sub1824 <- subset(NHIS10_DO, AGE_P == "18 to 24")
N10.LBP.age1824.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub1824)
summary(N10.LBP.age1824.reg)
exp(cbind(OR = coef(N10.LBP.age1824.reg), confint(N10.LBP.age1824.reg)))

#25 to 29
sub2529 <- subset(NHIS10_DO, AGE_P == "25 to 29")
N10.LBP.age2529.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub2529)
summary(N10.LBP.age2529.reg)
exp(cbind(OR = coef(N10.LBP.age2529.reg), confint(N10.LBP.age2529.reg)))

#30 to 34
sub3034 <- subset(NHIS10_DO, AGE_P == "30 to 34")
N10.LB.age3034.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub3034)
summary(N10.LB.age3034.reg)
exp(cbind(OR = coef(N10.LB.age3034.reg), confint(N10.LB.age3034.reg)))

#35 to 39
sub3539 <- subset(NHIS10_DO, AGE_P == "35 to 39")
N10.LBP.age3539.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub3539)
summary(N10.LBP.age3539.reg)
exp(cbind(OR = coef(N10.LBP.age3539.reg), confint(N10.LBP.age3539.reg)))

#40 to 44
sub4044 <- subset(NHIS10_DO, AGE_P == "40 to 44")
N10.LBP.age4044.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub4044)
summary(N10.LBP.age4044.reg)
exp(cbind(OR = coef(N10.LBP.age4044.reg), confint(N10.LBP.age4044.reg)))

#45 to 49
sub4549 <- subset(NHIS10_DO, AGE_P == "45 to 49")
N10.LBP.age4549.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub4549)
summary(N10.LBP.age4549.reg)
exp(cbind(OR = coef(N10.LBP.age4549.reg), confint(N10.LBP.age4549.reg)))

#50 to 54
sub5054 <- subset(NHIS10_DO, AGE_P == "50 to 54")
N10.LBP.age5054.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub5054)
summary(N10.LBP.age5054.reg)
exp(cbind(OR = coef(N10.LBP.age5054.reg), confint(N10.LBP.age5054.reg)))

#55 to 59
sub5559 <- subset(NHIS10_DO, AGE_P == "55 to 59")
N10.LBP.age5559.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub5559)
summary(N10.LBP.age5559.reg)
exp(cbind(OR = coef(N10.LBP.age5559.reg), confint(N10.LBP.age5559.reg)))

#60 to 64
sub6064 <- subset(NHIS10_DO, AGE_P == "60 to 64")
N10.LBP.age6064.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub6064)
summary(N10.LBP.age6064.reg)
exp(cbind(OR = coef(N10.LBP.age6064.reg), confint(N10.LBP.age6064.reg)))

#65 to 69
sub6569 <- subset(NHIS10_DO, AGE_P == "65 to 69")
N10.LBP.age6569.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub6569)
summary(N10.LBP.age6569.reg)
exp(cbind(OR = coef(N10.LBP.age6569.reg), confint(N10.LBP.age6569.reg)))

#70 and above
sub70 <- subset(NHIS10_DO, AGE_P == "70 and above")
N10.LBP.age70.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                            family = quasibinomial,
                            design = sub70)
summary(N10.LBP.age70.reg)
exp(cbind(OR = coef(N10.LBP.age70.reg), confint(N10.LBP.age70.reg)))

#III) prevalence by region, by sex

#i)  sex-specific regional prevalence
N10_LBP_reg.sex <- svyby(formula = ~PAINLB,
                         by = ~REGION + SEX,
                         design = NHIS10_DO,
                         FUN = svymean,
                         na.rm = TRUE)
N10_LBP_reg.sex.c <- N10_LBP_reg.sex %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Sex", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N10_LBP_reg.sex.ci <- confint(N10_LBP_reg.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N10_LBP_reg.sex.ci <- N10_LBP_reg.sex.ci[-c(1:8), ]
#join proportion and ci
N10.LBP.sex.region <- bind_cols(N10_LBP_reg.sex.c, N10_LBP_reg.sex.ci) #final prevalence, se & 95% ci
#save
#write.csv(N10.LBP.sex.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N10.LBP.region.sex.csv")

#Geofacet - Sex & Region
LBP.Sex_Reg10 <- ggplot(N10.LBP.sex.region, aes(Sex, Proportion)) +
  geom_bar(colour = "black", stat = "identity", fill = "#6600CC") +
  coord_flip() +
  facet_geo(~Region,
            grid = USregions) +
  theme_bw() +
  theme(text = element_text(colour = "black", size = 20)) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 17)) +
  xlab("Sex") +
  geom_errorbar(aes(ymin = Proportion-SE, ymax = Proportion+SE, width = 0.3)) +
  scale_y_continuous(name = "Prevalence (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 18)) +
  ggtitle("Regional prevalence (%) of US residents with low back pain by sex in 2010") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N10.GF_SexReg.LBP.png", width = 14, height = 9)


#ii) sex-specific regional logistic regressions
#males
submales <- subset(NHIS10_DO, SEX == "Male")
N10.LBP.sexmale.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + AGE_P,
                              family = quasibinomial,
                              design = submales)
summary(N10.LBP.sexmale.reg)
exp(cbind(OR = coef(N10.LBP.sexmale.reg), confint(N10.LBP.sexmale.reg)))

#females
subfemales <- subset(NHIS10_DO, SEX == "Female")
N10.LBP.sexfemale.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + AGE_P,
                                family = quasibinomial,
                                design = subfemales)
summary(N10.LBP.sexfemale.reg)
exp(cbind(OR = coef(N10.LBP.sexfemale.reg), confint(N10.LBP.sexfemale.reg)))


#3. Demographic trends

#I)  Age trends

#i)  Prevalence by age
N10_LBP_age <- svyby(formula = ~PAINLB,
                     by = ~AGE_P,
                     design = NHIS10_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N10_LBP_age.c <- N10_LBP_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N10_LBP_age_ci <- confint(N10_LBP_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove LBP=0 ci
N10_LBP_age_ci <- N10_LBP_age_ci[-c(1:11), ]
#join proportion and ci
N10_LBP.age <- bind_cols(N10_LBP_age.c, N10_LBP_age_ci) #final proportion, se & 95% ci
#Save
#write.csv(N10_LBP.age, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N10.LBP.age.csv")

#ii) Logistic regression by age
N10_LBP.age_glm <- svyglm(PAINLB~AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS10_DO)
summary(N10_LBP.age_glm)
exp(cbind(OR=coef(N10_LBP.age_glm), confint(N10_LBP.age_glm)))


#II)  Sex trends

#i)  Prevalence by sex
N10_LBP_sex <- svyby(formula = ~PAINLB,
                     by = ~SEX,
                     design = NHIS10_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N10_LBP_sex.c <- N10_LBP_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N10_LBP_sex_ci <- confint(N10_LBP_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N10_LBP_sex_ci <- N10_LBP_sex_ci[-c(1:2), ]
#join proportion and ci
N10.LBP.sex <- bind_cols(N10_LBP_sex.c, N10_LBP_sex_ci) #final proportion, se & 95% ci
#save
#write.csv(N10.LBP.sex, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N10.LBP.sex.csv")

#ii) Logistic regression by sex
N10.LB.sex.glm.c <- svyglm(PAINLB~relevel(SEX, ref = "Male") + AGE_P,
                           family = quasibinomial,
                           design = NHIS10_DO)
summary(N10.LB.sex.glm.c)
exp(cbind(OR=coef(N10.LB.sex.glm.c), confint(N10.LB.sex.glm.c)))

#III) Socioeconomic trends (employment status only)

#i)   Prevalence by employment status
N10_LBP_emp <- svyby(formula = ~PAINLB,
                     by = ~DOINGLWA,
                     design = NHIS10_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N10_LBP_emp.c <- N10_LBP_emp %>%
  select(1, 3, 5) %>%
  setNames(c("Employment", "Proportion", "SE")) %>%
  mutate(Employment = as.factor(Employment)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N10_LBP_emp_ci <- confint(N10_LBP_emp) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for LBP = 0
N10_LBP_emp_ci <- N10_LBP_emp_ci[-c(1:2), ]
#join ci and proportion
N10.LBP.emp <- bind_cols(N10_LBP_emp.c, N10_LBP_emp_ci) #final proportion, se & 95% ci
#save
#write.csv(N10.LBP.emp, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N10.LBP.emp.csv")


#ii)  Logistic regression by employment status
N10_LBP.emp_glm <- svyglm(PAINLB~relevel(DOINGLWA, ref = "Working for pay at a job/business") + AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS10_DO)
summary(N10_LBP.emp_glm)
exp(cbind(OR=coef(N10_LBP.emp_glm), confint(N10_LBP.emp_glm)))

#4. Lifestyle trends

#I)  BMI
#logistic regression
N10.BMI.LBP.glm <- svyglm(PAINLB ~ BMI + AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS10_DO)
summary(N10.BMI.LBP.glm)
exp(cbind(OR=coef(N10.BMI.LBP.glm), confint(N10.BMI.LBP.glm)))

#II) Physical activity
#logistic regression
N10.PA.LBP.glm <- svyglm(PAINLB ~ relevel(factor(PAC, ordered = FALSE), ref = "Physically active") + AGE_P + SEX,
                         family = quasibinomial,
                         design = NHIS10_DO)
summary(N10.PA.LBP.glm)
exp(cbind(OR = coef(N10.PA.LBP.glm), confint(N10.PA.LBP.glm)))


#_________________________________________________________________________________________________________________________________________________

# == 2009 == #

#1. Overall prevalence
N09_LBP <- svymean(~factor(PAINLB), 
                   NHIS09_DO, 
                   na.rm = TRUE)
N09_LBP.c <- N09_LBP %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N09_LBP_ci <- confint(N09_LBP) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
N09.LBP <- bind_cols(N09_LBP.c, N09_LBP_ci)
#remove LB = 0
N09.LBP <- N09.LBP[-c(1), ] #final proportion, se & 95% ci
#save
##write.csv(N09.LBP, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N09.LBP.csv")


#2. Spatial analysis, by region

#I)   Overall population
#i)   regional prevalence for the overall population
N09_LBP_reg <- svyby(formula = ~PAINLB,
                     by = ~REGION,
                     design = NHIS09_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N09_LBP_reg.c <- N09_LBP_reg %>%
  select(1, 3, 5) %>%
  setNames(c("Region", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N09_LBP_reg.ci <- confint(N09_LBP_reg) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N09_LBP_reg.ci <- N09_LBP_reg.ci[-c(1:4), ]
#join proportion and ci
N09.LBP.region <- bind_cols(N09_LBP_reg.c, N09_LBP_reg.ci) #final proportion, se & 95% ci
#save
#write.csv(N09.LBP.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N09.LBP.region.csv")

#Change the region column to "NAME" to match sf
names(N09.LBP.region)[names(N09.LBP.region) == "Region"] <- "NAME"

N09.LBP.joined <- regions %>%
  left_join(N09.LBP.region)

N09.LBP.joined$NAME <- as.factor(N09.LBP.joined$NAME)

#Plot
LBPregplot09 <- ggplot(data = N09.LBP.joined) +
  geom_sf(aes(fill = Proportion), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  scale_fill_viridis_c(name = "Prevalence (%)", option = "viridis", direction = -1, limits = c(25, 35), breaks = c(25, 30, 35)) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  ggtitle("Regional prevalence (%) of US residents with low back pain in 2009") +
  theme(plot.title = element_text(size = 10, hjust = 0.5))
ggsave("N09.LBP.regchor.png", width = 5, height = 5)


#ii)  logistic regression by region
N09.LBP.reg.glm <- svyglm(relevel(PAINLB, ref = "0")~ REGION + AGE_P,
                          family = quasibinomial,
                          design = NHIS09_DO)
summary(N09.LBP.reg.glm)
exp(cbind(OR=coef(N09.LBP.reg.glm), confint(N09.LBP.reg.glm)))


#II)  prevalence by region, by age

#i)  age-specific regional prevalance
N09_LBP_reg.age <- svyby(formula = ~PAINLB,
                         by = ~REGION + AGE_P,
                         design = NHIS09_DO,
                         FUN = svymean,
                         na.rm = TRUE)
N09_LBP_reg.age.c <- N09_LBP_reg.age %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Age", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Age = as.character(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N09_LBP_reg.age.ci <- confint(N09_LBP_reg.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N09_LBP_reg.age.ci <- N09_LBP_reg.age.ci[-c(1:44), ]
#join proportion and ci
N09.LBP.age.region <- bind_cols(N09_LBP_reg.age.c, N09_LBP_reg.age.ci) #final prevalence, se & 95% ci
#save
#write.csv(N09.LBP.age.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N09.LBP.region.age.csv")

#Geofacet - Age & Region
LBP.Age_Reg <- ggplot(N09.LBP.age.region, aes(Age, Proportion)) +
  geom_bar(colour = "black", stat = "identity", fill = "#6600CC") +
  coord_flip() +
  facet_geo(~Region,
            grid = USregions) +
  theme_bw() +
  theme(text = element_text(colour = "black", size = 20)) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 17)) +
  xlab("Age group (years)") +
  geom_errorbar(aes(ymin = Proportion-SE, ymax = Proportion+SE, width = 0.3)) +
  scale_y_continuous(name = "Prevalence (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 18)) + 
  ggtitle("Regional prevalence (%) of US residents with low back pain by age in 2009") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N09.GF_AgeReg.LBP.png", width = 14, height = 9)


#ii) age-specific regional logistic regressions
#18 to 24
sub1824 <- subset(NHIS09_DO, AGE_P == "18 to 24")
N09.LBP.age1824.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub1824)
summary(N09.LBP.age1824.reg)
exp(cbind(OR = coef(N09.LBP.age1824.reg), confint(N09.LBP.age1824.reg)))

#25 to 29
sub2529 <- subset(NHIS09_DO, AGE_P == "25 to 29")
N09.LBP.age2529.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub2529)
summary(N09.LBP.age2529.reg)
exp(cbind(OR = coef(N09.LBP.age2529.reg), confint(N09.LBP.age2529.reg)))

#30 to 34
sub3034 <- subset(NHIS09_DO, AGE_P == "30 to 34")
N09.LB.age3034.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub3034)
summary(N09.LB.age3034.reg)
exp(cbind(OR = coef(N09.LB.age3034.reg), confint(N09.LB.age3034.reg)))

#35 to 39
sub3539 <- subset(NHIS09_DO, AGE_P == "35 to 39")
N09.LBP.age3539.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub3539)
summary(N09.LBP.age3539.reg)
exp(cbind(OR = coef(N09.LBP.age3539.reg), confint(N09.LBP.age3539.reg)))

#40 to 44
sub4044 <- subset(NHIS09_DO, AGE_P == "40 to 44")
N09.LBP.age4044.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub4044)
summary(N09.LBP.age4044.reg)
exp(cbind(OR = coef(N09.LBP.age4044.reg), confint(N09.LBP.age4044.reg)))

#45 to 49
sub4549 <- subset(NHIS09_DO, AGE_P == "45 to 49")
N09.LBP.age4549.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub4549)
summary(N09.LBP.age4549.reg)
exp(cbind(OR = coef(N09.LBP.age4549.reg), confint(N09.LBP.age4549.reg)))

#50 to 54
sub5054 <- subset(NHIS09_DO, AGE_P == "50 to 54")
N09.LBP.age5054.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub5054)
summary(N09.LBP.age5054.reg)
exp(cbind(OR = coef(N09.LBP.age5054.reg), confint(N09.LBP.age5054.reg)))

#55 to 59
sub5559 <- subset(NHIS09_DO, AGE_P == "55 to 59")
N09.LBP.age5559.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub5559)
summary(N09.LBP.age5559.reg)
exp(cbind(OR = coef(N09.LBP.age5559.reg), confint(N09.LBP.age5559.reg)))

#60 to 64
sub6064 <- subset(NHIS09_DO, AGE_P == "60 to 64")
N09.LBP.age6064.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub6064)
summary(N09.LBP.age6064.reg)
exp(cbind(OR = coef(N09.LBP.age6064.reg), confint(N09.LBP.age6064.reg)))

#65 to 69
sub6569 <- subset(NHIS09_DO, AGE_P == "65 to 69")
N09.LBP.age6569.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub6569)
summary(N09.LBP.age6569.reg)
exp(cbind(OR = coef(N09.LBP.age6569.reg), confint(N09.LBP.age6569.reg)))

#70 and above
sub70 <- subset(NHIS09_DO, AGE_P == "70 and above")
N09.LBP.age70.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                            family = quasibinomial,
                            design = sub70)
summary(N09.LBP.age70.reg)
exp(cbind(OR = coef(N09.LBP.age70.reg), confint(N09.LBP.age70.reg)))

#III) prevalence by region, by sex

#i)  sex-specific regional prevalence
N09_LBP_reg.sex <- svyby(formula = ~PAINLB,
                         by = ~REGION + SEX,
                         design = NHIS09_DO,
                         FUN = svymean,
                         na.rm = TRUE)
N09_LBP_reg.sex.c <- N09_LBP_reg.sex %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Sex", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N09_LBP_reg.sex.ci <- confint(N09_LBP_reg.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N09_LBP_reg.sex.ci <- N09_LBP_reg.sex.ci[-c(1:8), ]
#join proportion and ci
N09.LBP.sex.region <- bind_cols(N09_LBP_reg.sex.c, N09_LBP_reg.sex.ci) #final prevalence, se & 95% ci
#save
#write.csv(N09.LBP.sex.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N09.LBP.region.sex.csv")

#Geofacet - Sex & Region
LBP.Sex_Reg09 <- ggplot(N09.LBP.sex.region, aes(Sex, Proportion)) +
  geom_bar(colour = "black", stat = "identity", fill = "#6600CC") +
  coord_flip() +
  facet_geo(~Region,
            grid = USregions) +
  theme_bw() +
  theme(text = element_text(colour = "black", size = 20)) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 17)) +
  xlab("Sex") +
  geom_errorbar(aes(ymin = Proportion-SE, ymax = Proportion+SE, width = 0.3)) +
  scale_y_continuous(name = "Prevalence (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 18)) +
  ggtitle("Regional prevalence (%) of US residents with low back pain by sex in 2009") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N09.GF_SexReg.LBP.png", width = 14, height = 9)


#ii) sex-specific regional logistic regressions
#males
submales <- subset(NHIS09_DO, SEX == "Male")
N09.LBP.sexmale.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + AGE_P,
                              family = quasibinomial,
                              design = submales)
summary(N09.LBP.sexmale.reg)
exp(cbind(OR = coef(N09.LBP.sexmale.reg), confint(N09.LBP.sexmale.reg)))

#females
subfemales <- subset(NHIS09_DO, SEX == "Female")
N09.LBP.sexfemale.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + AGE_P,
                                family = quasibinomial,
                                design = subfemales)
summary(N09.LBP.sexfemale.reg)
exp(cbind(OR = coef(N09.LBP.sexfemale.reg), confint(N09.LBP.sexfemale.reg)))


#3. Demographic trends

#I)  Age trends

#i)  Prevalence by age
N09_LBP_age <- svyby(formula = ~PAINLB,
                     by = ~AGE_P,
                     design = NHIS09_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N09_LBP_age.c <- N09_LBP_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N09_LBP_age_ci <- confint(N09_LBP_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove LBP=0 ci
N09_LBP_age_ci <- N09_LBP_age_ci[-c(1:11), ]
#join proportion and ci
N09_LBP.age <- bind_cols(N09_LBP_age.c, N09_LBP_age_ci) #final proportion, se & 95% ci
#Save
#write.csv(N09_LBP.age, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N09.LBP.age.csv")

#ii) Logistic regression by age
N09_LBP.age_glm <- svyglm(PAINLB~AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS09_DO)
summary(N09_LBP.age_glm)
exp(cbind(OR=coef(N09_LBP.age_glm), confint(N09_LBP.age_glm)))


#II)  Sex trends

#i)  Prevalence by sex
N09_LBP_sex <- svyby(formula = ~PAINLB,
                     by = ~SEX,
                     design = NHIS09_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N09_LBP_sex.c <- N09_LBP_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N09_LBP_sex_ci <- confint(N09_LBP_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N09_LBP_sex_ci <- N09_LBP_sex_ci[-c(1:2), ]
#join proportion and ci
N09.LBP.sex <- bind_cols(N09_LBP_sex.c, N09_LBP_sex_ci) #final proportion, se & 95% ci
#save
#write.csv(N09.LBP.sex, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N09.LBP.sex.csv")

#ii) Logistic regression by sex
N09.LB.sex.glm.c <- svyglm(PAINLB~relevel(SEX, ref = "Male") + AGE_P,
                           family = quasibinomial,
                           design = NHIS09_DO)
summary(N09.LB.sex.glm.c)
exp(cbind(OR=coef(N09.LB.sex.glm.c), confint(N09.LB.sex.glm.c)))

#III) Socioeconomic trends (employment status only)

#i)   Prevalence by employment status
N09_LBP_emp <- svyby(formula = ~PAINLB,
                     by = ~DOINGLWA,
                     design = NHIS09_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N09_LBP_emp.c <- N09_LBP_emp %>%
  select(1, 3, 5) %>%
  setNames(c("Employment", "Proportion", "SE")) %>%
  mutate(Employment = as.factor(Employment)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N09_LBP_emp_ci <- confint(N09_LBP_emp) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for LBP = 0
N09_LBP_emp_ci <- N09_LBP_emp_ci[-c(1:2), ]
#join ci and proportion
N09.LBP.emp <- bind_cols(N09_LBP_emp.c, N09_LBP_emp_ci) #final proportion, se & 95% ci
#save
#write.csv(N09.LBP.emp, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N09.LBP.emp.csv")


#ii)  Logistic regression by employment status
N09_LBP.emp_glm <- svyglm(PAINLB~relevel(DOINGLWA, ref = "Working for pay at a job/business") + AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS09_DO)
summary(N09_LBP.emp_glm)
exp(cbind(OR=coef(N09_LBP.emp_glm), confint(N09_LBP.emp_glm)))

#4. Lifestyle trends

#I)  BMI
#logistic regression
N09.BMI.LBP.glm <- svyglm(PAINLB ~ BMI + AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS09_DO)
summary(N09.BMI.LBP.glm)
exp(cbind(OR=coef(N09.BMI.LBP.glm), confint(N09.BMI.LBP.glm)))

#II) Physical activity
#logistic regression
N09.PA.LBP.glm <- svyglm(PAINLB ~ relevel(factor(PAC, ordered = FALSE), ref = "Physically active") + AGE_P + SEX,
                         family = quasibinomial,
                         design = NHIS09_DO)
summary(N09.PA.LBP.glm)
exp(cbind(OR = coef(N09.PA.LBP.glm), confint(N09.PA.LBP.glm)))


#________________________________________________________________________________________________________________________________________________

# == 2008 == #

#1. Overall prevalence
N08_LBP <- svymean(~factor(PAINLB), 
                   NHIS08_DO, 
                   na.rm = TRUE)
N08_LBP.c <- N08_LBP %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N08_LBP_ci <- confint(N08_LBP) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
N08.LBP <- bind_cols(N08_LBP.c, N08_LBP_ci)
#remove LB = 0
N08.LBP <- N08.LBP[-c(1), ] #final proportion, se & 95% ci
#save
#write.csv(N08.LBP, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N08.LBP.csv")


#2. Spatial analysis, by region

#I)   Overall population
#i)   regional prevalence for the overall population
N08_LBP_reg <- svyby(formula = ~PAINLB,
                     by = ~REGION,
                     design = NHIS08_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N08_LBP_reg.c <- N08_LBP_reg %>%
  select(1, 3, 5) %>%
  setNames(c("Region", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N08_LBP_reg.ci <- confint(N08_LBP_reg) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N08_LBP_reg.ci <- N08_LBP_reg.ci[-c(1:4), ]
#join proportion and ci
N08.LBP.region <- bind_cols(N08_LBP_reg.c, N08_LBP_reg.ci) #final proportion, se & 95% ci
#save
#write.csv(N08.LBP.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N08.LBP.region.csv")

#Change the region column to "NAME" to match sf
names(N08.LBP.region)[names(N08.LBP.region) == "Region"] <- "NAME"

N08.LBP.joined <- regions %>%
  left_join(N08.LBP.region)

N08.LBP.joined$NAME <- as.factor(N08.LBP.joined$NAME)

#Plot
LBPregplot08 <- ggplot(data = N08.LBP.joined) +
  geom_sf(aes(fill = Proportion), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  scale_fill_viridis_c(name = "Prevalence (%)", option = "viridis", direction = -1, limits = c(25, 35), breaks = c(25, 30, 35)) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  ggtitle("Regional prevalence (%) of US residents with low back pain in 2008") +
  theme(plot.title = element_text(size = 10, hjust = 0.5))
ggsave("N08.LBP.regchor.png", width = 5, height = 5)


#ii)  logistic regression by region
N08.LBP.reg.glm <- svyglm(relevel(PAINLB, ref = "0")~ REGION + AGE_P,
                          family = quasibinomial,
                          design = NHIS08_DO)
summary(N08.LBP.reg.glm)
exp(cbind(OR=coef(N08.LBP.reg.glm), confint(N08.LBP.reg.glm)))


#II)  prevalence by region, by age

#i)  age-specific regional prevalance
N08_LBP_reg.age <- svyby(formula = ~PAINLB,
                         by = ~REGION + AGE_P,
                         design = NHIS08_DO,
                         FUN = svymean,
                         na.rm = TRUE)
N08_LBP_reg.age.c <- N08_LBP_reg.age %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Age", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Age = as.character(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N08_LBP_reg.age.ci <- confint(N08_LBP_reg.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N08_LBP_reg.age.ci <- N08_LBP_reg.age.ci[-c(1:44), ]
#join proportion and ci
N08.LBP.age.region <- bind_cols(N08_LBP_reg.age.c, N08_LBP_reg.age.ci) #final prevalence, se & 95% ci
#save
#write.csv(N08.LBP.age.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N08.LBP.region.age.csv")

#Geofacet - Age & Region
LBP.Age_Reg <- ggplot(N08.LBP.age.region, aes(Age, Proportion)) +
  geom_bar(colour = "black", stat = "identity", fill = "#6600CC") +
  coord_flip() +
  facet_geo(~Region,
            grid = USregions) +
  theme_bw() +
  theme(text = element_text(colour = "black", size = 20)) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 17)) +
  xlab("Age group (years)") +
  geom_errorbar(aes(ymin = Proportion-SE, ymax = Proportion+SE, width = 0.3)) +
  scale_y_continuous(name = "Prevalence (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 18)) +
  ggtitle("Regional prevalence (%) of US residents with low back pain by age in 2008") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N08.GF_AgeReg.LBP.png", width = 14, height = 9)


#ii) age-specific regional logistic regressions
#18 to 24
sub1824 <- subset(NHIS08_DO, AGE_P == "18 to 24")
N08.LBP.age1824.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub1824)
summary(N08.LBP.age1824.reg)
exp(cbind(OR = coef(N08.LBP.age1824.reg), confint(N08.LBP.age1824.reg)))

#25 to 29
sub2529 <- subset(NHIS08_DO, AGE_P == "25 to 29")
N08.LBP.age2529.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub2529)
summary(N08.LBP.age2529.reg)
exp(cbind(OR = coef(N08.LBP.age2529.reg), confint(N08.LBP.age2529.reg)))

#30 to 34
sub3034 <- subset(NHIS08_DO, AGE_P == "30 to 34")
N08.LB.age3034.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub3034)
summary(N08.LB.age3034.reg)
exp(cbind(OR = coef(N08.LB.age3034.reg), confint(N08.LB.age3034.reg)))

#35 to 39
sub3539 <- subset(NHIS08_DO, AGE_P == "35 to 39")
N08.LBP.age3539.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub3539)
summary(N08.LBP.age3539.reg)
exp(cbind(OR = coef(N08.LBP.age3539.reg), confint(N08.LBP.age3539.reg)))

#40 to 44
sub4044 <- subset(NHIS08_DO, AGE_P == "40 to 44")
N08.LBP.age4044.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub4044)
summary(N08.LBP.age4044.reg)
exp(cbind(OR = coef(N08.LBP.age4044.reg), confint(N08.LBP.age4044.reg)))

#45 to 49
sub4549 <- subset(NHIS08_DO, AGE_P == "45 to 49")
N08.LBP.age4549.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub4549)
summary(N08.LBP.age4549.reg)
exp(cbind(OR = coef(N08.LBP.age4549.reg), confint(N08.LBP.age4549.reg)))

#50 to 54
sub5054 <- subset(NHIS08_DO, AGE_P == "50 to 54")
N08.LBP.age5054.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub5054)
summary(N08.LBP.age5054.reg)
exp(cbind(OR = coef(N08.LBP.age5054.reg), confint(N08.LBP.age5054.reg)))

#55 to 59
sub5559 <- subset(NHIS08_DO, AGE_P == "55 to 59")
N08.LBP.age5559.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub5559)
summary(N08.LBP.age5559.reg)
exp(cbind(OR = coef(N08.LBP.age5559.reg), confint(N08.LBP.age5559.reg)))

#60 to 64
sub6064 <- subset(NHIS08_DO, AGE_P == "60 to 64")
N08.LBP.age6064.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub6064)
summary(N08.LBP.age6064.reg)
exp(cbind(OR = coef(N08.LBP.age6064.reg), confint(N08.LBP.age6064.reg)))

#65 to 69
sub6569 <- subset(NHIS08_DO, AGE_P == "65 to 69")
N08.LBP.age6569.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub6569)
summary(N08.LBP.age6569.reg)
exp(cbind(OR = coef(N08.LBP.age6569.reg), confint(N08.LBP.age6569.reg)))

#70 and above
sub70 <- subset(NHIS08_DO, AGE_P == "70 and above")
N08.LBP.age70.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                            family = quasibinomial,
                            design = sub70)
summary(N08.LBP.age70.reg)
exp(cbind(OR = coef(N08.LBP.age70.reg), confint(N08.LBP.age70.reg)))

#III) prevalence by region, by sex

#i)  sex-specific regional prevalence
N08_LBP_reg.sex <- svyby(formula = ~PAINLB,
                         by = ~REGION + SEX,
                         design = NHIS08_DO,
                         FUN = svymean,
                         na.rm = TRUE)
N08_LBP_reg.sex.c <- N08_LBP_reg.sex %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Sex", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N08_LBP_reg.sex.ci <- confint(N08_LBP_reg.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N08_LBP_reg.sex.ci <- N08_LBP_reg.sex.ci[-c(1:8), ]
#join proportion and ci
N08.LBP.sex.region <- bind_cols(N08_LBP_reg.sex.c, N08_LBP_reg.sex.ci) #final prevalence, se & 95% ci
#save
#write.csv(N08.LBP.sex.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N08.LBP.region.sex.csv")

#Geofacet - Sex & Region
LBP.Sex_Reg08 <- ggplot(N08.LBP.sex.region, aes(Sex, Proportion)) +
  geom_bar(colour = "black", stat = "identity", fill = "#6600CC") +
  coord_flip() +
  facet_geo(~Region,
            grid = USregions) +
  theme_bw() +
  theme(text = element_text(colour = "black", size = 20)) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 17)) +
  xlab("Sex") +
  geom_errorbar(aes(ymin = Proportion-SE, ymax = Proportion+SE, width = 0.3)) +
  scale_y_continuous(name = "Prevalence (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 18)) +
  ggtitle("Regional prevalence (%) of US residents with low back pain by sex in 2008") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N08.GF_SexReg.LBP.png", width = 14, height = 9)


#ii) sex-specific regional logistic regressions
#males
submales <- subset(NHIS08_DO, SEX == "Male")
N08.LBP.sexmale.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + AGE_P,
                              family = quasibinomial,
                              design = submales)
summary(N08.LBP.sexmale.reg)
exp(cbind(OR = coef(N08.LBP.sexmale.reg), confint(N08.LBP.sexmale.reg)))

#females
subfemales <- subset(NHIS08_DO, SEX == "Female")
N08.LBP.sexfemale.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + AGE_P,
                                family = quasibinomial,
                                design = subfemales)
summary(N08.LBP.sexfemale.reg)
exp(cbind(OR = coef(N08.LBP.sexfemale.reg), confint(N08.LBP.sexfemale.reg)))


#3. Demographic trends

#I)  Age trends

#i)  Prevalence by age
N08_LBP_age <- svyby(formula = ~PAINLB,
                     by = ~AGE_P,
                     design = NHIS08_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N08_LBP_age.c <- N08_LBP_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N08_LBP_age_ci <- confint(N08_LBP_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove LBP=0 ci
N08_LBP_age_ci <- N08_LBP_age_ci[-c(1:11), ]
#join proportion and ci
N08_LBP.age <- bind_cols(N08_LBP_age.c, N08_LBP_age_ci) #final proportion, se & 95% ci
#Save
#write.csv(N08_LBP.age, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N08.LBP.age.csv")

#ii) Logistic regression by age
N08_LBP.age_glm <- svyglm(PAINLB~AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS08_DO)
summary(N08_LBP.age_glm)
exp(cbind(OR=coef(N08_LBP.age_glm), confint(N08_LBP.age_glm)))


#II)  Sex trends

#i)  Prevalence by sex
N08_LBP_sex <- svyby(formula = ~PAINLB,
                     by = ~SEX,
                     design = NHIS08_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N08_LBP_sex.c <- N08_LBP_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N08_LBP_sex_ci <- confint(N08_LBP_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N08_LBP_sex_ci <- N08_LBP_sex_ci[-c(1:2), ]
#join proportion and ci
N08.LBP.sex <- bind_cols(N08_LBP_sex.c, N08_LBP_sex_ci) #final proportion, se & 95% ci
#save
#write.csv(N08.LBP.sex, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N08.LBP.sex.csv")

#ii) Logistic regression by sex
N08.LB.sex.glm.c <- svyglm(PAINLB~relevel(SEX, ref = "Male") + AGE_P,
                           family = quasibinomial,
                           design = NHIS08_DO)
summary(N08.LB.sex.glm.c)
exp(cbind(OR=coef(N08.LB.sex.glm.c), confint(N08.LB.sex.glm.c)))

#III) Socioeconomic trends (employment status only)

#i)   Prevalence by employment status
N08_LBP_emp <- svyby(formula = ~PAINLB,
                     by = ~DOINGLWA,
                     design = NHIS08_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N08_LBP_emp.c <- N08_LBP_emp %>%
  select(1, 3, 5) %>%
  setNames(c("Employment", "Proportion", "SE")) %>%
  mutate(Employment = as.factor(Employment)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N08_LBP_emp_ci <- confint(N08_LBP_emp) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for LBP = 0
N08_LBP_emp_ci <- N08_LBP_emp_ci[-c(1:2), ]
#join ci and proportion
N08.LBP.emp <- bind_cols(N08_LBP_emp.c, N08_LBP_emp_ci) #final proportion, se & 95% ci
#save
#write.csv(N08.LBP.emp, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N08.LBP.emp.csv")


#ii)  Logistic regression by employment status
N08_LBP.emp_glm <- svyglm(PAINLB~relevel(DOINGLWA, ref = "Working for pay at a job/business") + AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS08_DO)
summary(N08_LBP.emp_glm)
exp(cbind(OR=coef(N08_LBP.emp_glm), confint(N08_LBP.emp_glm)))

#4. Lifestyle trends

#I)  BMI
#logistic regression
N08.BMI.LBP.glm <- svyglm(PAINLB ~ BMI + AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS08_DO)
summary(N08.BMI.LBP.glm)
exp(cbind(OR=coef(N08.BMI.LBP.glm), confint(N08.BMI.LBP.glm)))

#II) Physical activity
#logistic regression
N08.PA.LBP.glm <- svyglm(PAINLB ~ relevel(factor(PAC, ordered = FALSE), ref = "Physically active") + AGE_P + SEX,
                         family = quasibinomial,
                         design = NHIS08_DO)
summary(N08.PA.LBP.glm)
exp(cbind(OR = coef(N08.PA.LBP.glm), confint(N08.PA.LBP.glm)))

#________________________________________________________________________________________________________________________________________________

# == 2007 == #

#1. Overall prevalence
N07_LBP <- svymean(~factor(PAINLB), 
                   NHIS07_DO, 
                   na.rm = TRUE)
N07_LBP.c <- N07_LBP %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N07_LBP_ci <- confint(N07_LBP) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
N07.LBP <- bind_cols(N07_LBP.c, N07_LBP_ci)
#remove LB = 0
N07.LBP <- N07.LBP[-c(1), ] #final proportion, se & 95% ci
#save
#write.csv(N07.LBP, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N07.LBP.csv")


#2. Spatial analysis, by region

#I)   Overall population
#i)   regional prevalence for the overall population
N07_LBP_reg <- svyby(formula = ~PAINLB,
                     by = ~REGION,
                     design = NHIS07_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N07_LBP_reg.c <- N07_LBP_reg %>%
  select(1, 3, 5) %>%
  setNames(c("Region", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N07_LBP_reg.ci <- confint(N07_LBP_reg) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N07_LBP_reg.ci <- N07_LBP_reg.ci[-c(1:4), ]
#join proportion and ci
N07.LBP.region <- bind_cols(N07_LBP_reg.c, N07_LBP_reg.ci) #final proportion, se & 95% ci
#save
#write.csv(N07.LBP.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N07.LBP.region.csv")

#Change the region column to "NAME" to match sf
names(N07.LBP.region)[names(N07.LBP.region) == "Region"] <- "NAME"

N07.LBP.joined <- regions %>%
  left_join(N07.LBP.region)

N07.LBP.joined$NAME <- as.factor(N07.LBP.joined$NAME)

#Plot
LBPregplot07 <- ggplot(data = N07.LBP.joined) +
  geom_sf(aes(fill = Proportion), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  scale_fill_viridis_c(name = "Prevalence (%)", option = "viridis", direction = -1, limits = c(24, 35), breaks = c(25, 30, 35)) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  ggtitle("Regional prevalence (%) of US residents with low back pain in 2007") +
  theme(plot.title = element_text(size = 10, hjust = 0.5))
ggsave("N07.LBP.regchor.png", width = 5, height = 5)

#ii)  logistic regression by region
N07.LBP.reg.glm <- svyglm(relevel(PAINLB, ref = "0")~ REGION + AGE_P,
                          family = quasibinomial,
                          design = NHIS07_DO)
summary(N07.LBP.reg.glm)
exp(cbind(OR=coef(N07.LBP.reg.glm), confint(N07.LBP.reg.glm)))


#II)  prevalence by region, by age

#i)  age-specific regional prevalance
N07_LBP_reg.age <- svyby(formula = ~PAINLB,
                         by = ~REGION + AGE_P,
                         design = NHIS07_DO,
                         FUN = svymean,
                         na.rm = TRUE)
N07_LBP_reg.age.c <- N07_LBP_reg.age %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Age", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Age = as.character(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N07_LBP_reg.age.ci <- confint(N07_LBP_reg.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N07_LBP_reg.age.ci <- N07_LBP_reg.age.ci[-c(1:44), ]
#join proportion and ci
N07.LBP.age.region <- bind_cols(N07_LBP_reg.age.c, N07_LBP_reg.age.ci) #final prevalence, se & 95% ci
#save
#write.csv(N07.LBP.age.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N07.LBP.region.age.csv")

#Geofacet - Age & Region
LBP.Age_Reg <- ggplot(N07.LBP.age.region, aes(Age, Proportion)) +
  geom_bar(colour = "black", stat = "identity", fill = "#6600CC") +
  coord_flip() +
  facet_geo(~Region,
            grid = USregions) +
  theme_bw() +
  theme(text = element_text(colour = "black", size = 20)) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 17)) +
  xlab("Age group (years)") +
  geom_errorbar(aes(ymin = Proportion-SE, ymax = Proportion+SE, width = 0.3)) +
  scale_y_continuous(name = "Prevalence (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 18)) +
  ggtitle("Regional prevalence (%) of US residents with low back pain by age in 2007") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N07.GF_AgeReg.LBP.png", width = 14, height = 9)


#ii) age-specific regional logistic regressions
#18 to 24
sub1824 <- subset(NHIS07_DO, AGE_P == "18 to 24")
N07.LBP.age1824.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub1824)
summary(N07.LBP.age1824.reg)
exp(cbind(OR = coef(N07.LBP.age1824.reg), confint(N07.LBP.age1824.reg)))

#25 to 29
sub2529 <- subset(NHIS07_DO, AGE_P == "25 to 29")
N07.LBP.age2529.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub2529)
summary(N07.LBP.age2529.reg)
exp(cbind(OR = coef(N07.LBP.age2529.reg), confint(N07.LBP.age2529.reg)))

#30 to 34
sub3034 <- subset(NHIS07_DO, AGE_P == "30 to 34")
N07.LB.age3034.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub3034)
summary(N07.LB.age3034.reg)
exp(cbind(OR = coef(N07.LB.age3034.reg), confint(N07.LB.age3034.reg)))

#35 to 39
sub3539 <- subset(NHIS07_DO, AGE_P == "35 to 39")
N07.LBP.age3539.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub3539)
summary(N07.LBP.age3539.reg)
exp(cbind(OR = coef(N07.LBP.age3539.reg), confint(N07.LBP.age3539.reg)))

#40 to 44
sub4044 <- subset(NHIS07_DO, AGE_P == "40 to 44")
N07.LBP.age4044.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub4044)
summary(N07.LBP.age4044.reg)
exp(cbind(OR = coef(N07.LBP.age4044.reg), confint(N07.LBP.age4044.reg)))

#45 to 49
sub4549 <- subset(NHIS07_DO, AGE_P == "45 to 49")
N07.LBP.age4549.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub4549)
summary(N07.LBP.age4549.reg)
exp(cbind(OR = coef(N07.LBP.age4549.reg), confint(N07.LBP.age4549.reg)))

#50 to 54
sub5054 <- subset(NHIS07_DO, AGE_P == "50 to 54")
N07.LBP.age5054.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub5054)
summary(N07.LBP.age5054.reg)
exp(cbind(OR = coef(N07.LBP.age5054.reg), confint(N07.LBP.age5054.reg)))

#55 to 59
sub5559 <- subset(NHIS07_DO, AGE_P == "55 to 59")
N07.LBP.age5559.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub5559)
summary(N07.LBP.age5559.reg)
exp(cbind(OR = coef(N07.LBP.age5559.reg), confint(N07.LBP.age5559.reg)))

#60 to 64
sub6064 <- subset(NHIS07_DO, AGE_P == "60 to 64")
N07.LBP.age6064.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub6064)
summary(N07.LBP.age6064.reg)
exp(cbind(OR = coef(N07.LBP.age6064.reg), confint(N07.LBP.age6064.reg)))

#65 to 69
sub6569 <- subset(NHIS07_DO, AGE_P == "65 to 69")
N07.LBP.age6569.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub6569)
summary(N07.LBP.age6569.reg)
exp(cbind(OR = coef(N07.LBP.age6569.reg), confint(N07.LBP.age6569.reg)))

#70 and above
sub70 <- subset(NHIS07_DO, AGE_P == "70 and above")
N07.LBP.age70.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                            family = quasibinomial,
                            design = sub70)
summary(N07.LBP.age70.reg)
exp(cbind(OR = coef(N07.LBP.age70.reg), confint(N07.LBP.age70.reg)))

#III) prevalence by region, by sex

#i)  sex-specific regional prevalence
N07_LBP_reg.sex <- svyby(formula = ~PAINLB,
                         by = ~REGION + SEX,
                         design = NHIS07_DO,
                         FUN = svymean,
                         na.rm = TRUE)
N07_LBP_reg.sex.c <- N07_LBP_reg.sex %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Sex", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N07_LBP_reg.sex.ci <- confint(N07_LBP_reg.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N07_LBP_reg.sex.ci <- N07_LBP_reg.sex.ci[-c(1:8), ]
#join proportion and ci
N07.LBP.sex.region <- bind_cols(N07_LBP_reg.sex.c, N07_LBP_reg.sex.ci) #final prevalence, se & 95% ci
#save
#write.csv(N07.LBP.sex.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N07.LBP.region.sex.csv")

#Geofacet - Sex & Region
LBP.Sex_Reg07 <- ggplot(N07.LBP.sex.region, aes(Sex, Proportion)) +
  geom_bar(colour = "black", stat = "identity", fill = "#6600CC") +
  coord_flip() +
  facet_geo(~Region,
            grid = USregions) +
  theme_bw() +
  theme(text = element_text(colour = "black", size = 20)) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 17)) +
  xlab("Sex") +
  geom_errorbar(aes(ymin = Proportion-SE, ymax = Proportion+SE, width = 0.3)) +
  scale_y_continuous(name = "Prevalence (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 18)) +
  ggtitle("Regional prevalence (%) of US residents with low back pain by sex in 2007") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N07.GF_SexReg.LBP.png", width = 14, height = 9)


#ii) sex-specific regional logistic regressions
#males
submales <- subset(NHIS07_DO, SEX == "Male")
N07.LBP.sexmale.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + AGE_P,
                              family = quasibinomial,
                              design = submales)
summary(N07.LBP.sexmale.reg)
exp(cbind(OR = coef(N07.LBP.sexmale.reg), confint(N07.LBP.sexmale.reg)))

#females
subfemales <- subset(NHIS07_DO, SEX == "Female")
N07.LBP.sexfemale.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + AGE_P,
                                family = quasibinomial,
                                design = subfemales)
summary(N07.LBP.sexfemale.reg)
exp(cbind(OR = coef(N07.LBP.sexfemale.reg), confint(N07.LBP.sexfemale.reg)))


#3. Demographic trends

#I)  Age trends

#i)  Prevalence by age
N07_LBP_age <- svyby(formula = ~PAINLB,
                     by = ~AGE_P,
                     design = NHIS07_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N07_LBP_age.c <- N07_LBP_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N07_LBP_age_ci <- confint(N07_LBP_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove LBP=0 ci
N07_LBP_age_ci <- N07_LBP_age_ci[-c(1:11), ]
#join proportion and ci
N07_LBP.age <- bind_cols(N07_LBP_age.c, N07_LBP_age_ci) #final proportion, se & 95% ci
#Save
#write.csv(N07_LBP.age, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N07.LBP.age.csv")

#ii) Logistic regression by age
N07_LBP.age_glm <- svyglm(PAINLB~AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS07_DO)
summary(N07_LBP.age_glm)
exp(cbind(OR=coef(N07_LBP.age_glm), confint(N07_LBP.age_glm)))


#II)  Sex trends

#i)  Prevalence by sex
N07_LBP_sex <- svyby(formula = ~PAINLB,
                     by = ~SEX,
                     design = NHIS07_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N07_LBP_sex.c <- N07_LBP_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N07_LBP_sex_ci <- confint(N07_LBP_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N07_LBP_sex_ci <- N07_LBP_sex_ci[-c(1:2), ]
#join proportion and ci
N07.LBP.sex <- bind_cols(N07_LBP_sex.c, N07_LBP_sex_ci) #final proportion, se & 95% ci
#save
#write.csv(N07.LBP.sex, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N07.LBP.sex.csv")

#ii) Logistic regression by sex
N07.LB.sex.glm.c <- svyglm(PAINLB~relevel(SEX, ref = "Male") + AGE_P,
                           family = quasibinomial,
                           design = NHIS07_DO)
summary(N07.LB.sex.glm.c)
exp(cbind(OR=coef(N07.LB.sex.glm.c), confint(N07.LB.sex.glm.c)))

#III) Socioeconomic trends (employment status only)

#i)   Prevalence by employment status
N07_LBP_emp <- svyby(formula = ~PAINLB,
                     by = ~DOINGLWA,
                     design = NHIS07_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N07_LBP_emp.c <- N07_LBP_emp %>%
  select(1, 3, 5) %>%
  setNames(c("Employment", "Proportion", "SE")) %>%
  mutate(Employment = as.factor(Employment)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N07_LBP_emp_ci <- confint(N07_LBP_emp) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for LBP = 0
N07_LBP_emp_ci <- N07_LBP_emp_ci[-c(1:2), ]
#join ci and proportion
N07.LBP.emp <- bind_cols(N07_LBP_emp.c, N07_LBP_emp_ci) #final proportion, se & 95% ci
#save
#write.csv(N07.LBP.emp, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N07.LBP.emp.csv")


#ii)  Logistic regression by employment status
N07_LBP.emp_glm <- svyglm(PAINLB~relevel(DOINGLWA, ref = "Working for pay at a job/business") + AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS07_DO)
summary(N07_LBP.emp_glm)
exp(cbind(OR=coef(N07_LBP.emp_glm), confint(N07_LBP.emp_glm)))

#4. Lifestyle trends

#I)  BMI
#logistic regression
N07.BMI.LBP.glm <- svyglm(PAINLB ~ BMI + AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS07_DO)
summary(N07.BMI.LBP.glm)
exp(cbind(OR=coef(N07.BMI.LBP.glm), confint(N07.BMI.LBP.glm)))

#II) Physical activity
#logistic regression
N07.PA.LBP.glm <- svyglm(PAINLB ~ relevel(factor(PAC, ordered = FALSE), ref = "Physically active") + AGE_P + SEX,
                         family = quasibinomial,
                         design = NHIS07_DO)
summary(N07.PA.LBP.glm)
exp(cbind(OR = coef(N07.PA.LBP.glm), confint(N07.PA.LBP.glm)))


#_________________________________________________________________________________________________________________________________________________

# == 2006 == #

#1. Overall prevalence
N06_LBP <- svymean(~factor(PAINLB), 
                   NHIS06_DO, 
                   na.rm = TRUE)
N06_LBP.c <- N06_LBP %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N06_LBP_ci <- confint(N06_LBP) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
N06.LBP <- bind_cols(N06_LBP.c, N06_LBP_ci)
#remove LB = 0
N06.LBP <- N06.LBP[-c(1), ] #final proportion, se & 95% ci
#save
#write.csv(N06.LBP, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N06.LBP.csv")


#2. Spatial analysis, by region

#I)   Overall population
#i)   regional prevalence for the overall population
N06_LBP_reg <- svyby(formula = ~PAINLB,
                     by = ~REGION,
                     design = NHIS06_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N06_LBP_reg.c <- N06_LBP_reg %>%
  select(1, 3, 5) %>%
  setNames(c("Region", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N06_LBP_reg.ci <- confint(N06_LBP_reg) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N06_LBP_reg.ci <- N06_LBP_reg.ci[-c(1:4), ]
#join proportion and ci
N06.LBP.region <- bind_cols(N06_LBP_reg.c, N06_LBP_reg.ci) #final proportion, se & 95% ci
#save
#write.csv(N06.LBP.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N06.LBP.region.csv")

#Change the region column to "NAME" to match sf
names(N06.LBP.region)[names(N06.LBP.region) == "Region"] <- "NAME"

N06.LBP.joined <- regions %>%
  left_join(N06.LBP.region)

N06.LBP.joined$NAME <- as.factor(N06.LBP.joined$NAME)

#Plot
LBPregplot06 <- ggplot(data = N06.LBP.joined) +
  geom_sf(aes(fill = Proportion), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  scale_fill_viridis_c(name = "Prevalence (%)", option = "viridis", direction = -1, limits = c(25, 35), breaks = c(25, 30, 35)) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  ggtitle("Regional prevalence (%) of US residents with low back pain in 2006") +
  theme(plot.title = element_text(size = 10, hjust = 0.5))
ggsave("N06.LBP.regchor.png", width = 5, height = 5)


#ii)  logistic regression by region
N06.LBP.reg.glm <- svyglm(relevel(PAINLB, ref = "0")~ REGION + AGE_P,
                          family = quasibinomial,
                          design = NHIS06_DO)
summary(N06.LBP.reg.glm)
exp(cbind(OR=coef(N06.LBP.reg.glm), confint(N06.LBP.reg.glm)))


#II)  prevalence by region, by age

#i)  age-specific regional prevalance
N06_LBP_reg.age <- svyby(formula = ~PAINLB,
                         by = ~REGION + AGE_P,
                         design = NHIS06_DO,
                         FUN = svymean,
                         na.rm = TRUE)
N06_LBP_reg.age.c <- N06_LBP_reg.age %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Age", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Age = as.character(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N06_LBP_reg.age.ci <- confint(N06_LBP_reg.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N06_LBP_reg.age.ci <- N06_LBP_reg.age.ci[-c(1:44), ]
#join proportion and ci
N06.LBP.age.region <- bind_cols(N06_LBP_reg.age.c, N06_LBP_reg.age.ci) #final prevalence, se & 95% ci
#save
#write.csv(N06.LBP.age.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N06.LBP.region.age.csv")

#Geofacet - Age & Region
LBP.Age_Reg <- ggplot(N06.LBP.age.region, aes(Age, Proportion)) +
  geom_bar(colour = "black", stat = "identity", fill = "#6600CC") +
  coord_flip() +
  facet_geo(~Region,
            grid = USregions) +
  theme_bw() +
  theme(text = element_text(colour = "black", size = 20)) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 17)) +
  xlab("Age group (years)") +
  geom_errorbar(aes(ymin = Proportion-SE, ymax = Proportion+SE, width = 0.3)) +
  scale_y_continuous(name = "Prevalence (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 18)) +
  ggtitle("Regional prevalence (%) of US residents with low back pain by age in 2006") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N06.GF_AgeReg.LBP.png", width = 14, height = 9)


#ii) age-specific regional logistic regressions
#18 to 24
sub1824 <- subset(NHIS06_DO, AGE_P == "18 to 24")
N06.LBP.age1824.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub1824)
summary(N06.LBP.age1824.reg)
exp(cbind(OR = coef(N06.LBP.age1824.reg), confint(N06.LBP.age1824.reg)))

#25 to 29
sub2529 <- subset(NHIS06_DO, AGE_P == "25 to 29")
N06.LBP.age2529.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub2529)
summary(N06.LBP.age2529.reg)
exp(cbind(OR = coef(N06.LBP.age2529.reg), confint(N06.LBP.age2529.reg)))

#30 to 34
sub3034 <- subset(NHIS06_DO, AGE_P == "30 to 34")
N06.LB.age3034.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub3034)
summary(N06.LB.age3034.reg)
exp(cbind(OR = coef(N06.LB.age3034.reg), confint(N06.LB.age3034.reg)))

#35 to 39
sub3539 <- subset(NHIS06_DO, AGE_P == "35 to 39")
N06.LBP.age3539.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub3539)
summary(N06.LBP.age3539.reg)
exp(cbind(OR = coef(N06.LBP.age3539.reg), confint(N06.LBP.age3539.reg)))

#40 to 44
sub4044 <- subset(NHIS06_DO, AGE_P == "40 to 44")
N06.LBP.age4044.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub4044)
summary(N06.LBP.age4044.reg)
exp(cbind(OR = coef(N06.LBP.age4044.reg), confint(N06.LBP.age4044.reg)))

#45 to 49
sub4549 <- subset(NHIS06_DO, AGE_P == "45 to 49")
N06.LBP.age4549.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub4549)
summary(N06.LBP.age4549.reg)
exp(cbind(OR = coef(N06.LBP.age4549.reg), confint(N06.LBP.age4549.reg)))

#50 to 54
sub5054 <- subset(NHIS06_DO, AGE_P == "50 to 54")
N06.LBP.age5054.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub5054)
summary(N06.LBP.age5054.reg)
exp(cbind(OR = coef(N06.LBP.age5054.reg), confint(N06.LBP.age5054.reg)))

#55 to 59
sub5559 <- subset(NHIS06_DO, AGE_P == "55 to 59")
N06.LBP.age5559.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub5559)
summary(N06.LBP.age5559.reg)
exp(cbind(OR = coef(N06.LBP.age5559.reg), confint(N06.LBP.age5559.reg)))

#60 to 64
sub6064 <- subset(NHIS06_DO, AGE_P == "60 to 64")
N06.LBP.age6064.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub6064)
summary(N06.LBP.age6064.reg)
exp(cbind(OR = coef(N06.LBP.age6064.reg), confint(N06.LBP.age6064.reg)))

#65 to 69
sub6569 <- subset(NHIS06_DO, AGE_P == "65 to 69")
N06.LBP.age6569.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub6569)
summary(N06.LBP.age6569.reg)
exp(cbind(OR = coef(N06.LBP.age6569.reg), confint(N06.LBP.age6569.reg)))

#70 and above
sub70 <- subset(NHIS06_DO, AGE_P == "70 and above")
N06.LBP.age70.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                            family = quasibinomial,
                            design = sub70)
summary(N06.LBP.age70.reg)
exp(cbind(OR = coef(N06.LBP.age70.reg), confint(N06.LBP.age70.reg)))

#III) prevalence by region, by sex

#i)  sex-specific regional prevalence
N06_LBP_reg.sex <- svyby(formula = ~PAINLB,
                         by = ~REGION + SEX,
                         design = NHIS06_DO,
                         FUN = svymean,
                         na.rm = TRUE)
N06_LBP_reg.sex.c <- N06_LBP_reg.sex %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Sex", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N06_LBP_reg.sex.ci <- confint(N06_LBP_reg.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N06_LBP_reg.sex.ci <- N06_LBP_reg.sex.ci[-c(1:8), ]
#join proportion and ci
N06.LBP.sex.region <- bind_cols(N06_LBP_reg.sex.c, N06_LBP_reg.sex.ci) #final prevalence, se & 95% ci
#save
#write.csv(N06.LBP.sex.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N06.LBP.region.sex.csv")

#Geofacet - Sex & Region
LBP.Sex_Reg06 <- ggplot(N06.LBP.sex.region, aes(Sex, Proportion)) +
  geom_bar(colour = "black", stat = "identity", fill = "#6600CC") +
  coord_flip() +
  facet_geo(~Region,
            grid = USregions) +
  theme_bw() +
  theme(text = element_text(colour = "black", size = 20)) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 17)) +
  xlab("Sex") +
  geom_errorbar(aes(ymin = Proportion-SE, ymax = Proportion+SE, width = 0.3)) +
  scale_y_continuous(name = "Prevalence (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 18)) +
  ggtitle("Regional prevalence (%) of US residents with low back pain by sex in 2006") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N06.GF_SexReg.LBP.png", width = 14, height = 9)


#ii) sex-specific regional logistic regressions
#males
submales <- subset(NHIS06_DO, SEX == "Male")
N06.LBP.sexmale.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + AGE_P,
                              family = quasibinomial,
                              design = submales)
summary(N06.LBP.sexmale.reg)
exp(cbind(OR = coef(N06.LBP.sexmale.reg), confint(N06.LBP.sexmale.reg)))

#females
subfemales <- subset(NHIS06_DO, SEX == "Female")
N06.LBP.sexfemale.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + AGE_P,
                                family = quasibinomial,
                                design = subfemales)
summary(N06.LBP.sexfemale.reg)
exp(cbind(OR = coef(N06.LBP.sexfemale.reg), confint(N06.LBP.sexfemale.reg)))


#3. Demographic trends

#I)  Age trends

#i)  Prevalence by age
N06_LBP_age <- svyby(formula = ~PAINLB,
                     by = ~AGE_P,
                     design = NHIS06_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N06_LBP_age.c <- N06_LBP_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N06_LBP_age_ci <- confint(N06_LBP_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove LBP=0 ci
N06_LBP_age_ci <- N06_LBP_age_ci[-c(1:11), ]
#join proportion and ci
N06_LBP.age <- bind_cols(N06_LBP_age.c, N06_LBP_age_ci) #final proportion, se & 95% ci
#Save
#write.csv(N06_LBP.age, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N06.LBP.age.csv")

#ii) Logistic regression by age
N06_LBP.age_glm <- svyglm(PAINLB~AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS06_DO)
summary(N06_LBP.age_glm)
exp(cbind(OR=coef(N06_LBP.age_glm), confint(N06_LBP.age_glm)))


#II)  Sex trends

#i)  Prevalence by sex
N06_LBP_sex <- svyby(formula = ~PAINLB,
                     by = ~SEX,
                     design = NHIS06_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N06_LBP_sex.c <- N06_LBP_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N06_LBP_sex_ci <- confint(N06_LBP_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N06_LBP_sex_ci <- N06_LBP_sex_ci[-c(1:2), ]
#join proportion and ci
N06.LBP.sex <- bind_cols(N06_LBP_sex.c, N06_LBP_sex_ci) #final proportion, se & 95% ci
#save
#write.csv(N06.LBP.sex, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N06.LBP.sex.csv")

#ii) Logistic regression by sex
N06.LB.sex.glm.c <- svyglm(PAINLB~relevel(SEX, ref = "Male") + AGE_P,
                           family = quasibinomial,
                           design = NHIS06_DO)
summary(N06.LB.sex.glm.c)
exp(cbind(OR=coef(N06.LB.sex.glm.c), confint(N06.LB.sex.glm.c)))

#III) Socioeconomic trends (employment status only)

#i)   Prevalence by employment status
N06_LBP_emp <- svyby(formula = ~PAINLB,
                     by = ~DOINGLWA,
                     design = NHIS06_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N06_LBP_emp.c <- N06_LBP_emp %>%
  select(1, 3, 5) %>%
  setNames(c("Employment", "Proportion", "SE")) %>%
  mutate(Employment = as.factor(Employment)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N06_LBP_emp_ci <- confint(N06_LBP_emp) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for LBP = 0
N06_LBP_emp_ci <- N06_LBP_emp_ci[-c(1:2), ]
#join ci and proportion
N06.LBP.emp <- bind_cols(N06_LBP_emp.c, N06_LBP_emp_ci) #final proportion, se & 95% ci
#save
#write.csv(N06.LBP.emp, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N06.LBP.emp.csv")


#ii)  Logistic regression by employment status
N06_LBP.emp_glm <- svyglm(PAINLB~relevel(DOINGLWA, ref = "Working for pay at a job/business") + AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS06_DO)
summary(N06_LBP.emp_glm)
exp(cbind(OR=coef(N06_LBP.emp_glm), confint(N06_LBP.emp_glm)))

#4. Lifestyle trends

#I)  BMI
#logistic regression
N06.BMI.LBP.glm <- svyglm(PAINLB ~ BMI + AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS06_DO)
summary(N06.BMI.LBP.glm)
exp(cbind(OR=coef(N06.BMI.LBP.glm), confint(N06.BMI.LBP.glm)))

#II) Physical activity
#logistic regression
N06.PA.LBP.glm <- svyglm(PAINLB ~ relevel(factor(PAC, ordered = FALSE), ref = "Physically active") + AGE_P + SEX,
                         family = quasibinomial,
                         design = NHIS06_DO)
summary(N06.PA.LBP.glm)
exp(cbind(OR = coef(N06.PA.LBP.glm), confint(N06.PA.LBP.glm)))

#________________________________________________________________________________________________________________________________________________

# == 2005 == #

#1. Overall prevalence
N05_LBP <- svymean(~factor(PAINLB), 
                   NHIS05_DO, 
                   na.rm = TRUE)
N05_LBP.c <- N05_LBP %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N05_LBP_ci <- confint(N05_LBP) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
N05.LBP <- bind_cols(N05_LBP.c, N05_LBP_ci)
#remove LB = 0
N05.LBP <- N05.LBP[-c(1), ] #final proportion, se & 95% ci
#save
#write.csv(N05.LBP, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N05.LBP.csv")


#2. Spatial analysis, by region

#I)   Overall population
#i)   regional prevalence for the overall population
N05_LBP_reg <- svyby(formula = ~PAINLB,
                     by = ~REGION,
                     design = NHIS05_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N05_LBP_reg.c <- N05_LBP_reg %>%
  select(1, 3, 5) %>%
  setNames(c("Region", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N05_LBP_reg.ci <- confint(N05_LBP_reg) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N05_LBP_reg.ci <- N05_LBP_reg.ci[-c(1:4), ]
#join proportion and ci
N05.LBP.region <- bind_cols(N05_LBP_reg.c, N05_LBP_reg.ci) #final proportion, se & 95% ci
#save
#write.csv(N05.LBP.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N05.LBP.region.csv")

#Change the region column to "NAME" to match sf
names(N05.LBP.region)[names(N05.LBP.region) == "Region"] <- "NAME"

N05.LBP.joined <- regions %>%
  left_join(N05.LBP.region)

N05.LBP.joined$NAME <- as.factor(N05.LBP.joined$NAME)

#Plot
LBPregplot05 <- ggplot(data = N05.LBP.joined) +
  geom_sf(aes(fill = Proportion), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  scale_fill_viridis_c(name = "Prevalence (%)", option = "viridis", direction = -1, limits = c(25, 35), breaks = c(25, 30, 35)) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  ggtitle("Regional prevalence (%) of US residents with low back pain in 2005") +
  theme(plot.title = element_text(size = 10, hjust = 0.5))
ggsave("N05.LBP.regchor.png", width = 5, height = 5)


#ii)  logistic regression by region
N05.LBP.reg.glm <- svyglm(relevel(PAINLB, ref = "0")~ REGION + AGE_P,
                          family = quasibinomial,
                          design = NHIS05_DO)
summary(N05.LBP.reg.glm)
exp(cbind(OR=coef(N05.LBP.reg.glm), confint(N05.LBP.reg.glm)))


#II)  prevalence by region, by age

#i)  age-specific regional prevalance
N05_LBP_reg.age <- svyby(formula = ~PAINLB,
                         by = ~REGION + AGE_P,
                         design = NHIS05_DO,
                         FUN = svymean,
                         na.rm = TRUE)
N05_LBP_reg.age.c <- N05_LBP_reg.age %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Age", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Age = as.character(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N05_LBP_reg.age.ci <- confint(N05_LBP_reg.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N05_LBP_reg.age.ci <- N05_LBP_reg.age.ci[-c(1:44), ]
#join proportion and ci
N05.LBP.age.region <- bind_cols(N05_LBP_reg.age.c, N05_LBP_reg.age.ci) #final prevalence, se & 95% ci
#save
#write.csv(N05.LBP.age.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N05.LBP.region.age.csv")

#Geofacet - Age & Region
LBP.Age_Reg <- ggplot(N05.LBP.age.region, aes(Age, Proportion)) +
  geom_bar(colour = "black", stat = "identity", fill = "#6600CC") +
  coord_flip() +
  facet_geo(~Region,
            grid = USregions) +
  theme_bw() +
  theme(text = element_text(colour = "black", size = 20)) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 17)) +
  xlab("Age group (years)") +
  geom_errorbar(aes(ymin = Proportion-SE, ymax = Proportion+SE, width = 0.3)) +
  scale_y_continuous(name = "Prevalence (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 18)) +
  ggtitle("Regional prevalence (%) of US residents with low back pain by age in 2005") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N05.GF_AgeReg.LBP.png", width = 14, height = 9)


#ii) age-specific regional logistic regressions
#18 to 24
sub1824 <- subset(NHIS05_DO, AGE_P == "18 to 24")
N05.LBP.age1824.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub1824)
summary(N05.LBP.age1824.reg)
exp(cbind(OR = coef(N05.LBP.age1824.reg), confint(N05.LBP.age1824.reg)))

#25 to 29
sub2529 <- subset(NHIS05_DO, AGE_P == "25 to 29")
N05.LBP.age2529.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub2529)
summary(N05.LBP.age2529.reg)
exp(cbind(OR = coef(N05.LBP.age2529.reg), confint(N05.LBP.age2529.reg)))

#30 to 34
sub3034 <- subset(NHIS05_DO, AGE_P == "30 to 34")
N05.LB.age3034.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub3034)
summary(N05.LB.age3034.reg)
exp(cbind(OR = coef(N05.LB.age3034.reg), confint(N05.LB.age3034.reg)))

#35 to 39
sub3539 <- subset(NHIS05_DO, AGE_P == "35 to 39")
N05.LBP.age3539.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub3539)
summary(N05.LBP.age3539.reg)
exp(cbind(OR = coef(N05.LBP.age3539.reg), confint(N05.LBP.age3539.reg)))

#40 to 44
sub4044 <- subset(NHIS05_DO, AGE_P == "40 to 44")
N05.LBP.age4044.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub4044)
summary(N05.LBP.age4044.reg)
exp(cbind(OR = coef(N05.LBP.age4044.reg), confint(N05.LBP.age4044.reg)))

#45 to 49
sub4549 <- subset(NHIS05_DO, AGE_P == "45 to 49")
N05.LBP.age4549.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub4549)
summary(N05.LBP.age4549.reg)
exp(cbind(OR = coef(N05.LBP.age4549.reg), confint(N05.LBP.age4549.reg)))

#50 to 54
sub5054 <- subset(NHIS05_DO, AGE_P == "50 to 54")
N05.LBP.age5054.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub5054)
summary(N05.LBP.age5054.reg)
exp(cbind(OR = coef(N05.LBP.age5054.reg), confint(N05.LBP.age5054.reg)))

#55 to 59
sub5559 <- subset(NHIS05_DO, AGE_P == "55 to 59")
N05.LBP.age5559.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub5559)
summary(N05.LBP.age5559.reg)
exp(cbind(OR = coef(N05.LBP.age5559.reg), confint(N05.LBP.age5559.reg)))

#60 to 64
sub6064 <- subset(NHIS05_DO, AGE_P == "60 to 64")
N05.LBP.age6064.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub6064)
summary(N05.LBP.age6064.reg)
exp(cbind(OR = coef(N05.LBP.age6064.reg), confint(N05.LBP.age6064.reg)))

#65 to 69
sub6569 <- subset(NHIS05_DO, AGE_P == "65 to 69")
N05.LBP.age6569.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub6569)
summary(N05.LBP.age6569.reg)
exp(cbind(OR = coef(N05.LBP.age6569.reg), confint(N05.LBP.age6569.reg)))

#70 and above
sub70 <- subset(NHIS05_DO, AGE_P == "70 and above")
N05.LBP.age70.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                            family = quasibinomial,
                            design = sub70)
summary(N05.LBP.age70.reg)
exp(cbind(OR = coef(N05.LBP.age70.reg), confint(N05.LBP.age70.reg)))

#III) prevalence by region, by sex

#i)  sex-specific regional prevalence
N05_LBP_reg.sex <- svyby(formula = ~PAINLB,
                         by = ~REGION + SEX,
                         design = NHIS05_DO,
                         FUN = svymean,
                         na.rm = TRUE)
N05_LBP_reg.sex.c <- N05_LBP_reg.sex %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Sex", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N05_LBP_reg.sex.ci <- confint(N05_LBP_reg.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N05_LBP_reg.sex.ci <- N05_LBP_reg.sex.ci[-c(1:8), ]
#join proportion and ci
N05.LBP.sex.region <- bind_cols(N05_LBP_reg.sex.c, N05_LBP_reg.sex.ci) #final prevalence, se & 95% ci
#save
#write.csv(N05.LBP.sex.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N05.LBP.region.sex.csv")

#Geofacet - Sex & Region
LBP.Sex_Reg05 <- ggplot(N05.LBP.sex.region, aes(Sex, Proportion)) +
  geom_bar(colour = "black", stat = "identity", fill = "#6600CC") +
  coord_flip() +
  facet_geo(~Region,
            grid = USregions) +
  theme_bw() +
  theme(text = element_text(colour = "black", size = 20)) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 17)) +
  xlab("Sex") +
  geom_errorbar(aes(ymin = Proportion-SE, ymax = Proportion+SE, width = 0.3)) +
  scale_y_continuous(name = "Prevalence (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 18)) +
  ggtitle("Regional prevalence (%) of US residents with low back pain by sex in 2005") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N05.GF_SexReg.LBP.png", width = 14, height = 9)


#ii) sex-specific regional logistic regressions
#males
submales <- subset(NHIS05_DO, SEX == "Male")
N05.LBP.sexmale.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + AGE_P,
                              family = quasibinomial,
                              design = submales)
summary(N05.LBP.sexmale.reg)
exp(cbind(OR = coef(N05.LBP.sexmale.reg), confint(N05.LBP.sexmale.reg)))

#females
subfemales <- subset(NHIS05_DO, SEX == "Female")
N05.LBP.sexfemale.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + AGE_P,
                                family = quasibinomial,
                                design = subfemales)
summary(N05.LBP.sexfemale.reg)
exp(cbind(OR = coef(N05.LBP.sexfemale.reg), confint(N05.LBP.sexfemale.reg)))


#3. Demographic trends

#I)  Age trends

#i)  Prevalence by age
N05_LBP_age <- svyby(formula = ~PAINLB,
                     by = ~AGE_P,
                     design = NHIS05_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N05_LBP_age.c <- N05_LBP_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N05_LBP_age_ci <- confint(N05_LBP_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove LBP=0 ci
N05_LBP_age_ci <- N05_LBP_age_ci[-c(1:11), ]
#join proportion and ci
N05_LBP.age <- bind_cols(N05_LBP_age.c, N05_LBP_age_ci) #final proportion, se & 95% ci
#Save
#write.csv(N05_LBP.age, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N05.LBP.age.csv")

#ii) Logistic regression by age
N05_LBP.age_glm <- svyglm(PAINLB~AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS05_DO)
summary(N05_LBP.age_glm)
exp(cbind(OR=coef(N05_LBP.age_glm), confint(N05_LBP.age_glm)))


#II)  Sex trends

#i)  Prevalence by sex
N05_LBP_sex <- svyby(formula = ~PAINLB,
                     by = ~SEX,
                     design = NHIS05_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N05_LBP_sex.c <- N05_LBP_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N05_LBP_sex_ci <- confint(N05_LBP_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N05_LBP_sex_ci <- N05_LBP_sex_ci[-c(1:2), ]
#join proportion and ci
N05.LBP.sex <- bind_cols(N05_LBP_sex.c, N05_LBP_sex_ci) #final proportion, se & 95% ci
#save
#write.csv(N05.LBP.sex, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N05.LBP.sex.csv")

#ii) Logistic regression by sex
N05.LB.sex.glm.c <- svyglm(PAINLB~relevel(SEX, ref = "Male") + AGE_P,
                           family = quasibinomial,
                           design = NHIS05_DO)
summary(N05.LB.sex.glm.c)
exp(cbind(OR=coef(N05.LB.sex.glm.c), confint(N05.LB.sex.glm.c)))

#III) Socioeconomic trends (employment status only)

#i)   Prevalence by employment status
N05_LBP_emp <- svyby(formula = ~PAINLB,
                     by = ~DOINGLWA,
                     design = NHIS05_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N05_LBP_emp.c <- N05_LBP_emp %>%
  select(1, 3, 5) %>%
  setNames(c("Employment", "Proportion", "SE")) %>%
  mutate(Employment = as.factor(Employment)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N05_LBP_emp_ci <- confint(N05_LBP_emp) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for LBP = 0
N05_LBP_emp_ci <- N05_LBP_emp_ci[-c(1:2), ]
#join ci and proportion
N05.LBP.emp <- bind_cols(N05_LBP_emp.c, N05_LBP_emp_ci) #final proportion, se & 95% ci
#save
#write.csv(N05.LBP.emp, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N05.LBP.emp.csv")


#ii)  Logistic regression by employment status
N05_LBP.emp_glm <- svyglm(PAINLB~relevel(DOINGLWA, ref = "Working for pay at a job/business") + AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS05_DO)
summary(N05_LBP.emp_glm)
exp(cbind(OR=coef(N05_LBP.emp_glm), confint(N05_LBP.emp_glm)))

#4. Lifestyle trends

#I)  BMI
#logistic regression
N05.BMI.LBP.glm <- svyglm(PAINLB ~ BMI + AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS05_DO)
summary(N05.BMI.LBP.glm)
exp(cbind(OR=coef(N05.BMI.LBP.glm), confint(N05.BMI.LBP.glm)))

#II) Physical activity
#logistic regression
N05.PA.LBP.glm <- svyglm(PAINLB ~ relevel(factor(PAC, ordered = FALSE), ref = "Physically active") + AGE_P + SEX,
                         family = quasibinomial,
                         design = NHIS05_DO)
summary(N05.PA.LBP.glm)
exp(cbind(OR = coef(N05.PA.LBP.glm), confint(N05.PA.LBP.glm)))


#________________________________________________________________________________________________________________________________________________

# == no 2004 == #

#________________________________________________________________________________________________________________________________________________

# == 2003 == #

#1. Overall prevalence
N03_LBP <- svymean(~factor(PAINLB), 
                   NHIS03_DO, 
                   na.rm = TRUE)
N03_LBP.c <- N03_LBP %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N03_LBP_ci <- confint(N03_LBP) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
N03.LBP <- bind_cols(N03_LBP.c, N03_LBP_ci)
#remove LB = 0
N03.LBP <- N03.LBP[-c(1), ] #final proportion, se & 95% ci
#save
#write.csv(N03.LBP, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N03.LBP.csv")


#2. Spatial analysis, by region

#I)   Overall population
#i)   regional prevalence for the overall population
N03_LBP_reg <- svyby(formula = ~PAINLB,
                     by = ~REGION,
                     design = NHIS03_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N03_LBP_reg.c <- N03_LBP_reg %>%
  select(1, 3, 5) %>%
  setNames(c("Region", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N03_LBP_reg.ci <- confint(N03_LBP_reg) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N03_LBP_reg.ci <- N03_LBP_reg.ci[-c(1:4), ]
#join proportion and ci
N03.LBP.region <- bind_cols(N03_LBP_reg.c, N03_LBP_reg.ci) #final proportion, se & 95% ci
#save
#write.csv(N03.LBP.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N03.LBP.region.csv")

#Change the region column to "NAME" to match sf
names(N03.LBP.region)[names(N03.LBP.region) == "Region"] <- "NAME"

N03.LBP.joined <- regions %>%
  left_join(N03.LBP.region)

N03.LBP.joined$NAME <- as.factor(N03.LBP.joined$NAME)

#Plot
LBPregplot03 <- ggplot(data = N03.LBP.joined) +
  geom_sf(aes(fill = Proportion), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  scale_fill_viridis_c(name = "Prevalence (%)", option = "viridis", direction = -1, limits = c(25, 35), breaks = c(25, 30, 35)) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  ggtitle("Regional prevalence (%) of US residents with low back pain in 2003") +
  theme(plot.title = element_text(size = 10, hjust = 0.5))
ggsave("N03.LBP.regchor.png", width = 5, height = 5)


#ii)  logistic regression by region
N03.LBP.reg.glm <- svyglm(relevel(PAINLB, ref = "0")~ REGION + AGE_P,
                          family = quasibinomial,
                          design = NHIS03_DO)
summary(N03.LBP.reg.glm)
exp(cbind(OR=coef(N03.LBP.reg.glm), confint(N03.LBP.reg.glm)))


#II)  prevalence by region, by age

#i)  age-specific regional prevalance
N03_LBP_reg.age <- svyby(formula = ~PAINLB,
                         by = ~REGION + AGE_P,
                         design = NHIS03_DO,
                         FUN = svymean,
                         na.rm = TRUE)
N03_LBP_reg.age.c <- N03_LBP_reg.age %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Age", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Age = as.character(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N03_LBP_reg.age.ci <- confint(N03_LBP_reg.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N03_LBP_reg.age.ci <- N03_LBP_reg.age.ci[-c(1:44), ]
#join proportion and ci
N03.LBP.age.region <- bind_cols(N03_LBP_reg.age.c, N03_LBP_reg.age.ci) #final prevalence, se & 95% ci
#save
#write.csv(N03.LBP.age.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N03.LBP.region.age.csv")

#Geofacet - Age & Region
LBP.Age_Reg <- ggplot(N03.LBP.age.region, aes(Age, Proportion)) +
  geom_bar(colour = "black", stat = "identity", fill = "#6600CC") +
  coord_flip() +
  facet_geo(~Region,
            grid = USregions) +
  theme_bw() +
  theme(text = element_text(colour = "black", size = 20)) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 17)) +
  xlab("Age group (years)") +
  geom_errorbar(aes(ymin = Proportion-SE, ymax = Proportion+SE, width = 0.3)) +
  scale_y_continuous(name = "Prevalence (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 18)) +
  ggtitle("Regional prevalence (%) of US residents with low back pain by age in 2003") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N03.GF_AgeReg.LBP.png", width = 14, height = 9)


#ii) age-specific regional logistic regressions
#18 to 24
sub1824 <- subset(NHIS03_DO, AGE_P == "18 to 24")
N03.LBP.age1824.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub1824)
summary(N03.LBP.age1824.reg)
exp(cbind(OR = coef(N03.LBP.age1824.reg), confint(N03.LBP.age1824.reg)))

#25 to 29
sub2529 <- subset(NHIS03_DO, AGE_P == "25 to 29")
N03.LBP.age2529.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub2529)
summary(N03.LBP.age2529.reg)
exp(cbind(OR = coef(N03.LBP.age2529.reg), confint(N03.LBP.age2529.reg)))

#30 to 34
sub3034 <- subset(NHIS03_DO, AGE_P == "30 to 34")
N03.LB.age3034.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub3034)
summary(N03.LB.age3034.reg)
exp(cbind(OR = coef(N03.LB.age3034.reg), confint(N03.LB.age3034.reg)))

#35 to 39
sub3539 <- subset(NHIS03_DO, AGE_P == "35 to 39")
N03.LBP.age3539.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub3539)
summary(N03.LBP.age3539.reg)
exp(cbind(OR = coef(N03.LBP.age3539.reg), confint(N03.LBP.age3539.reg)))

#40 to 44
sub4044 <- subset(NHIS03_DO, AGE_P == "40 to 44")
N03.LBP.age4044.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub4044)
summary(N03.LBP.age4044.reg)
exp(cbind(OR = coef(N03.LBP.age4044.reg), confint(N03.LBP.age4044.reg)))

#45 to 49
sub4549 <- subset(NHIS03_DO, AGE_P == "45 to 49")
N03.LBP.age4549.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub4549)
summary(N03.LBP.age4549.reg)
exp(cbind(OR = coef(N03.LBP.age4549.reg), confint(N03.LBP.age4549.reg)))

#50 to 54
sub5054 <- subset(NHIS03_DO, AGE_P == "50 to 54")
N03.LBP.age5054.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub5054)
summary(N03.LBP.age5054.reg)
exp(cbind(OR = coef(N03.LBP.age5054.reg), confint(N03.LBP.age5054.reg)))

#55 to 59
sub5559 <- subset(NHIS03_DO, AGE_P == "55 to 59")
N03.LBP.age5559.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub5559)
summary(N03.LBP.age5559.reg)
exp(cbind(OR = coef(N03.LBP.age5559.reg), confint(N03.LBP.age5559.reg)))

#60 to 64
sub6064 <- subset(NHIS03_DO, AGE_P == "60 to 64")
N03.LBP.age6064.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub6064)
summary(N03.LBP.age6064.reg)
exp(cbind(OR = coef(N03.LBP.age6064.reg), confint(N03.LBP.age6064.reg)))

#65 to 69
sub6569 <- subset(NHIS03_DO, AGE_P == "65 to 69")
N03.LBP.age6569.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub6569)
summary(N03.LBP.age6569.reg)
exp(cbind(OR = coef(N03.LBP.age6569.reg), confint(N03.LBP.age6569.reg)))

#70 and above
sub70 <- subset(NHIS03_DO, AGE_P == "70 and above")
N03.LBP.age70.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                            family = quasibinomial,
                            design = sub70)
summary(N03.LBP.age70.reg)
exp(cbind(OR = coef(N03.LBP.age70.reg), confint(N03.LBP.age70.reg)))

#III) prevalence by region, by sex

#i)  sex-specific regional prevalence
N03_LBP_reg.sex <- svyby(formula = ~PAINLB,
                         by = ~REGION + SEX,
                         design = NHIS03_DO,
                         FUN = svymean,
                         na.rm = TRUE)
N03_LBP_reg.sex.c <- N03_LBP_reg.sex %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Sex", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N03_LBP_reg.sex.ci <- confint(N03_LBP_reg.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N03_LBP_reg.sex.ci <- N03_LBP_reg.sex.ci[-c(1:8), ]
#join proportion and ci
N03.LBP.sex.region <- bind_cols(N03_LBP_reg.sex.c, N03_LBP_reg.sex.ci) #final prevalence, se & 95% ci
#save
#write.csv(N03.LBP.sex.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N03.LBP.region.sex.csv")

#Geofacet - Sex & Region
LBP.Sex_Reg03 <- ggplot(N03.LBP.sex.region, aes(Sex, Proportion)) +
  geom_bar(colour = "black", stat = "identity", fill = "#6600CC") +
  coord_flip() +
  facet_geo(~Region,
            grid = USregions) +
  theme_bw() +
  theme(text = element_text(colour = "black", size = 20)) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 17)) +
  xlab("Sex") +
  geom_errorbar(aes(ymin = Proportion-SE, ymax = Proportion+SE, width = 0.3)) +
  scale_y_continuous(name = "Prevalence (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 18)) +
  ggtitle("Regional prevalence (%)of US residents with low back pain by sex in 2003") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N03.GF_SexReg.LBP.png", width = 14, height = 9)


#ii) sex-specific regional logistic regressions
#males
submales <- subset(NHIS03_DO, SEX == "Male")
N03.LBP.sexmale.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + AGE_P,
                              family = quasibinomial,
                              design = submales)
summary(N03.LBP.sexmale.reg)
exp(cbind(OR = coef(N03.LBP.sexmale.reg), confint(N03.LBP.sexmale.reg)))

#females
subfemales <- subset(NHIS03_DO, SEX == "Female")
N03.LBP.sexfemale.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + AGE_P,
                                family = quasibinomial,
                                design = subfemales)
summary(N03.LBP.sexfemale.reg)
exp(cbind(OR = coef(N03.LBP.sexfemale.reg), confint(N03.LBP.sexfemale.reg)))


#3. Demographic trends

#I)  Age trends

#i)  Prevalence by age
N03_LBP_age <- svyby(formula = ~PAINLB,
                     by = ~AGE_P,
                     design = NHIS03_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N03_LBP_age.c <- N03_LBP_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N03_LBP_age_ci <- confint(N03_LBP_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove LBP=0 ci
N03_LBP_age_ci <- N03_LBP_age_ci[-c(1:11), ]
#join proportion and ci
N03_LBP.age <- bind_cols(N03_LBP_age.c, N03_LBP_age_ci) #final proportion, se & 95% ci
#Save
#write.csv(N03_LBP.age, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N03.LBP.age.csv")

#ii) Logistic regression by age
N03_LBP.age_glm <- svyglm(PAINLB~AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS03_DO)
summary(N03_LBP.age_glm)
exp(cbind(OR=coef(N03_LBP.age_glm), confint(N03_LBP.age_glm)))


#II)  Sex trends

#i)  Prevalence by sex
N03_LBP_sex <- svyby(formula = ~PAINLB,
                     by = ~SEX,
                     design = NHIS03_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N03_LBP_sex.c <- N03_LBP_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N03_LBP_sex_ci <- confint(N03_LBP_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N03_LBP_sex_ci <- N03_LBP_sex_ci[-c(1:2), ]
#join proportion and ci
N03.LBP.sex <- bind_cols(N03_LBP_sex.c, N03_LBP_sex_ci) #final proportion, se & 95% ci
#save
#write.csv(N03.LBP.sex, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N03.LBP.sex.csv")

#ii) Logistic regression by sex
N03.LB.sex.glm.c <- svyglm(PAINLB~relevel(SEX, ref = "Male") + AGE_P,
                           family = quasibinomial,
                           design = NHIS03_DO)
summary(N03.LB.sex.glm.c)
exp(cbind(OR=coef(N03.LB.sex.glm.c), confint(N03.LB.sex.glm.c)))

#III) Socioeconomic trends (employment status only)

#i)   Prevalence by employment status
N03_LBP_emp <- svyby(formula = ~PAINLB,
                     by = ~ALL_SA,
                     design = NHIS03_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N03_LBP_emp.c <- N03_LBP_emp %>%
  select(1, 3, 5) %>%
  setNames(c("Employment", "Proportion", "SE")) %>%
  mutate(Employment = as.factor(Employment)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N03_LBP_emp_ci <- confint(N03_LBP_emp) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for LBP = 0
N03_LBP_emp_ci <- N03_LBP_emp_ci[-c(1:2), ]
#join ci and proportion
N03.LBP.emp <- bind_cols(N03_LBP_emp.c, N03_LBP_emp_ci) #final proportion, se & 95% ci
#save
#write.csv(N03.LBP.emp, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N03.LBP.emp.csv")


#ii)  Logistic regression by employment status
N03_LBP.emp_glm <- svyglm(PAINLB~relevel(ALL_SA, ref = "Working for pay at a job/business") + AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS03_DO)
summary(N03_LBP.emp_glm)
exp(cbind(OR=coef(N03_LBP.emp_glm), confint(N03_LBP.emp_glm)))

#4. Lifestyle trends

#I)  BMI
#logistic regression
N03.BMI.LBP.glm <- svyglm(PAINLB ~ BMI + AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS03_DO)
summary(N03.BMI.LBP.glm)
exp(cbind(OR=coef(N03.BMI.LBP.glm), confint(N03.BMI.LBP.glm)))

#II) Physical activity
#logistic regression
N03.PA.LBP.glm <- svyglm(PAINLB ~ relevel(factor(PAC, ordered = FALSE), ref = "Physically active") + AGE_P + SEX,
                         family = quasibinomial,
                         design = NHIS03_DO)
summary(N03.PA.LBP.glm)
exp(cbind(OR = coef(N03.PA.LBP.glm), confint(N03.PA.LBP.glm)))


#_______________________________________________________________________________________________________________________________________________

# == 2002 == #

#ANALYSIS

#1. Overall prevalence
N02_LBP <- svymean(~factor(PAINLB), 
                   NHIS02_DO, 
                   na.rm = TRUE)
N02_LBP.c <- N02_LBP %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N02_LBP_ci <- confint(N02_LBP) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
N02.LBP <- bind_cols(N02_LBP.c, N02_LBP_ci)
#remove LB = 0
N02.LBP <- N02.LBP[-c(1), ] #final proportion, se & 95% ci
#save
#write.csv(N02.LBP, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N02.LBP.csv")


#2. Spatial analysis, by region

#I)   Overall population
#i)   regional prevalence for the overall population
N02_LBP_reg <- svyby(formula = ~PAINLB,
                     by = ~REGION,
                     design = NHIS02_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N02_LBP_reg.c <- N02_LBP_reg %>%
  select(1, 3, 5) %>%
  setNames(c("Region", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N02_LBP_reg.ci <- confint(N02_LBP_reg) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N02_LBP_reg.ci <- N02_LBP_reg.ci[-c(1:4), ]
#join proportion and ci
N02.LBP.region <- bind_cols(N02_LBP_reg.c, N02_LBP_reg.ci) #final proportion, se & 95% ci
#save
#write.csv(N02.LBP.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N02.LBP.region.csv")

#Change the region column to "NAME" to match sf
names(N02.LBP.region)[names(N02.LBP.region) == "Region"] <- "NAME"

N02.LBP.joined <- regions %>%
  left_join(N02.LBP.region)

N02.LBP.joined$NAME <- as.factor(N02.LBP.joined$NAME)

#Plot
LBPregplot02 <- ggplot(data = N02.LBP.joined) +
  geom_sf(aes(fill = Proportion), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  scale_fill_viridis_c(name = "Prevalence (%)", option = "viridis", direction = -1, limits = c(24, 35), breaks = c(25, 30, 35)) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  ggtitle("Regional prevalence (%) of US residents with low back pain in 2002") +
  theme(plot.title = element_text(size = 10, hjust = 0.5))
ggsave("N02.LBP.regchor.png", width = 5, height = 5)


#ii)  logistic regression by region
N02.LBP.reg.glm <- svyglm(relevel(PAINLB, ref = "0")~ REGION + AGE_P,
                          family = quasibinomial,
                          design = NHIS02_DO)
summary(N02.LBP.reg.glm)
exp(cbind(OR=coef(N02.LBP.reg.glm), confint(N02.LBP.reg.glm)))


#II)  prevalence by region, by age

#i)  age-specific regional prevalance
N02_LBP_reg.age <- svyby(formula = ~PAINLB,
                         by = ~REGION + AGE_P,
                         design = NHIS02_DO,
                         FUN = svymean,
                         na.rm = TRUE)
N02_LBP_reg.age.c <- N02_LBP_reg.age %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Age", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Age = as.character(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N02_LBP_reg.age.ci <- confint(N02_LBP_reg.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N02_LBP_reg.age.ci <- N02_LBP_reg.age.ci[-c(1:44), ]
#join proportion and ci
N02.LBP.age.region <- bind_cols(N02_LBP_reg.age.c, N02_LBP_reg.age.ci) #final prevalence, se & 95% ci
#save
#write.csv(N02.LBP.age.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N02.LBP.region.age.csv")

#Geofacet - Age & Region
LBP.Age_Reg <- ggplot(N02.LBP.age.region, aes(Age, Proportion)) +
  geom_bar(colour = "black", stat = "identity", fill = "#6600CC") +
  coord_flip() +
  facet_geo(~Region,
            grid = USregions) +
  theme_bw() +
  theme(text = element_text(colour = "black", size = 20)) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 17)) +
  xlab("Age group (years)") +
  geom_errorbar(aes(ymin = Proportion-SE, ymax = Proportion+SE, width = 0.3)) +
  scale_y_continuous(name = "Prevalence (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 18)) +
  ggtitle("Regional prevalence (%) of US residents with low back pain by age in 2002") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N02.GF_AgeReg.LBP.png", width = 14, height = 9)


#ii) age-specific regional logistic regressions
#18 to 24
sub1824 <- subset(NHIS02_DO, AGE_P == "18 to 24")
N02.LBP.age1824.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub1824)
summary(N02.LBP.age1824.reg)
exp(cbind(OR = coef(N02.LBP.age1824.reg), confint(N02.LBP.age1824.reg)))

#25 to 29
sub2529 <- subset(NHIS02_DO, AGE_P == "25 to 29")
N02.LBP.age2529.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub2529)
summary(N02.LBP.age2529.reg)
exp(cbind(OR = coef(N02.LBP.age2529.reg), confint(N02.LBP.age2529.reg)))

#30 to 34
sub3034 <- subset(NHIS02_DO, AGE_P == "30 to 34")
N02.LB.age3034.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub3034)
summary(N02.LB.age3034.reg)
exp(cbind(OR = coef(N02.LB.age3034.reg), confint(N02.LB.age3034.reg)))

#35 to 39
sub3539 <- subset(NHIS02_DO, AGE_P == "35 to 39")
N02.LBP.age3539.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub3539)
summary(N02.LBP.age3539.reg)
exp(cbind(OR = coef(N02.LBP.age3539.reg), confint(N02.LBP.age3539.reg)))

#40 to 44
sub4044 <- subset(NHIS02_DO, AGE_P == "40 to 44")
N02.LBP.age4044.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub4044)
summary(N02.LBP.age4044.reg)
exp(cbind(OR = coef(N02.LBP.age4044.reg), confint(N02.LBP.age4044.reg)))

#45 to 49
sub4549 <- subset(NHIS02_DO, AGE_P == "45 to 49")
N02.LBP.age4549.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub4549)
summary(N02.LBP.age4549.reg)
exp(cbind(OR = coef(N02.LBP.age4549.reg), confint(N02.LBP.age4549.reg)))

#50 to 54
sub5054 <- subset(NHIS02_DO, AGE_P == "50 to 54")
N02.LBP.age5054.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub5054)
summary(N02.LBP.age5054.reg)
exp(cbind(OR = coef(N02.LBP.age5054.reg), confint(N02.LBP.age5054.reg)))

#55 to 59
sub5559 <- subset(NHIS02_DO, AGE_P == "55 to 59")
N02.LBP.age5559.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub5559)
summary(N02.LBP.age5559.reg)
exp(cbind(OR = coef(N02.LBP.age5559.reg), confint(N02.LBP.age5559.reg)))

#60 to 64
sub6064 <- subset(NHIS02_DO, AGE_P == "60 to 64")
N02.LBP.age6064.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub6064)
summary(N02.LBP.age6064.reg)
exp(cbind(OR = coef(N02.LBP.age6064.reg), confint(N02.LBP.age6064.reg)))

#65 to 69
sub6569 <- subset(NHIS02_DO, AGE_P == "65 to 69")
N02.LBP.age6569.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub6569)
summary(N02.LBP.age6569.reg)
exp(cbind(OR = coef(N02.LBP.age6569.reg), confint(N02.LBP.age6569.reg)))

#70 and above
sub70 <- subset(NHIS02_DO, AGE_P == "70 and above")
N02.LBP.age70.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                            family = quasibinomial,
                            design = sub70)
summary(N02.LBP.age70.reg)
exp(cbind(OR = coef(N02.LBP.age70.reg), confint(N02.LBP.age70.reg)))

#III) prevalence by region, by sex

#i)  sex-specific regional prevalence
N02_LBP_reg.sex <- svyby(formula = ~PAINLB,
                         by = ~REGION + SEX,
                         design = NHIS02_DO,
                         FUN = svymean,
                         na.rm = TRUE)
N02_LBP_reg.sex.c <- N02_LBP_reg.sex %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Sex", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N02_LBP_reg.sex.ci <- confint(N02_LBP_reg.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N02_LBP_reg.sex.ci <- N02_LBP_reg.sex.ci[-c(1:8), ]
#join proportion and ci
N02.LBP.sex.region <- bind_cols(N02_LBP_reg.sex.c, N02_LBP_reg.sex.ci) #final prevalence, se & 95% ci
#save
#write.csv(N02.LBP.sex.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N02.LBP.region.sex.csv")

#Geofacet - Sex & Region
LBP.Sex_Reg02 <- ggplot(N02.LBP.sex.region, aes(Sex, Proportion)) +
  geom_bar(colour = "black", stat = "identity", fill = "#6600CC") +
  coord_flip() +
  facet_geo(~Region,
            grid = USregions) +
  theme_bw() +
  theme(text = element_text(colour = "black", size = 20)) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 17)) +
  xlab("Sex") +
  geom_errorbar(aes(ymin = Proportion-SE, ymax = Proportion+SE, width = 0.3)) +
  scale_y_continuous(name = "Prevalence (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 18)) +
  ggtitle("Regional prevalence (%) of US residents with low back pain by sex in 2002") +
theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N02.GF_SexReg.LBP.png", width = 14, height = 9)


#ii) sex-specific regional logistic regressions
#males
submales <- subset(NHIS02_DO, SEX == "Male")
N02.LBP.sexmale.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + AGE_P,
                              family = quasibinomial,
                              design = submales)
summary(N02.LBP.sexmale.reg)
exp(cbind(OR = coef(N02.LBP.sexmale.reg), confint(N02.LBP.sexmale.reg)))

#females
subfemales <- subset(NHIS02_DO, SEX == "Female")
N02.LBP.sexfemale.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + AGE_P,
                                family = quasibinomial,
                                design = subfemales)
summary(N02.LBP.sexfemale.reg)
exp(cbind(OR = coef(N02.LBP.sexfemale.reg), confint(N02.LBP.sexfemale.reg)))


#3. Demographic trends

#I)  Age trends

#i)  Prevalence by age
N02_LBP_age <- svyby(formula = ~PAINLB,
                     by = ~AGE_P,
                     design = NHIS02_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N02_LBP_age.c <- N02_LBP_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N02_LBP_age_ci <- confint(N02_LBP_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove LBP=0 ci
N02_LBP_age_ci <- N02_LBP_age_ci[-c(1:11), ]
#join proportion and ci
N02_LBP.age <- bind_cols(N02_LBP_age.c, N02_LBP_age_ci) #final proportion, se & 95% ci
#Save
#write.csv(N02_LBP.age, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N02.LBP.age.csv")

#ii) Logistic regression by age
N02_LBP.age_glm <- svyglm(PAINLB~AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS02_DO)
summary(N02_LBP.age_glm)
exp(cbind(OR=coef(N02_LBP.age_glm), confint(N02_LBP.age_glm)))


#II)  Sex trends

#i)  Prevalence by sex
N02_LBP_sex <- svyby(formula = ~PAINLB,
                     by = ~SEX,
                     design = NHIS02_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N02_LBP_sex.c <- N02_LBP_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N02_LBP_sex_ci <- confint(N02_LBP_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N02_LBP_sex_ci <- N02_LBP_sex_ci[-c(1:2), ]
#join proportion and ci
N02.LBP.sex <- bind_cols(N02_LBP_sex.c, N02_LBP_sex_ci) #final proportion, se & 95% ci
#save
#write.csv(N02.LBP.sex, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N02.LBP.sex.csv")

#ii) Logistic regression by sex
N02.LB.sex.glm.c <- svyglm(PAINLB~relevel(SEX, ref = "Male") + AGE_P,
                           family = quasibinomial,
                           design = NHIS02_DO)
summary(N02.LB.sex.glm.c)
exp(cbind(OR=coef(N02.LB.sex.glm.c), confint(N02.LB.sex.glm.c)))

#III) Socioeconomic trends (employment status only)

#i)   Prevalence by employment status
N02_LBP_emp <- svyby(formula = ~PAINLB,
                     by = ~ALL_SA,
                     design = NHIS02_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N02_LBP_emp.c <- N02_LBP_emp %>%
  select(1, 3, 5) %>%
  setNames(c("Employment", "Proportion", "SE")) %>%
  mutate(Employment = as.factor(Employment)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N02_LBP_emp_ci <- confint(N02_LBP_emp) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for LBP = 0
N02_LBP_emp_ci <- N02_LBP_emp_ci[-c(1:2), ]
#join ci and proportion
N02.LBP.emp <- bind_cols(N02_LBP_emp.c, N02_LBP_emp_ci) #final proportion, se & 95% ci
#save
#write.csv(N02.LBP.emp, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N02.LBP.emp.csv")


#ii)  Logistic regression by employment status
N02_LBP.emp_glm <- svyglm(PAINLB~relevel(ALL_SA, ref = "Working for pay at a job/business") + AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS02_DO)
summary(N02_LBP.emp_glm)
exp(cbind(OR=coef(N02_LBP.emp_glm), confint(N02_LBP.emp_glm)))

#4. Lifestyle trends

#I)  BMI
#logistic regression
N02.BMI.LBP.glm <- svyglm(PAINLB ~ BMI + AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS02_DO)
summary(N02.BMI.LBP.glm)
exp(cbind(OR=coef(N02.BMI.LBP.glm), confint(N02.BMI.LBP.glm)))

#II) Physical activity
#logistic regression
N02.PA.LBP.glm <- svyglm(PAINLB ~ relevel(factor(PAC, ordered = FALSE), ref = "Physically active") + AGE_P + SEX,
                         family = quasibinomial,
                         design = NHIS02_DO)
summary(N02.PA.LBP.glm)
exp(cbind(OR = coef(N02.PA.LBP.glm), confint(N02.PA.LBP.glm)))

#________________________________________________________________________________________________________________________________________________

# == 2001 == #

#1. Overall prevalence
N01_LBP <- svymean(~factor(PAINLB), 
                   NHIS01_DO, 
                   na.rm = TRUE)
N01_LBP.c <- N01_LBP %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N01_LBP_ci <- confint(N01_LBP) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
N01.LBP <- bind_cols(N01_LBP.c, N01_LBP_ci)
#remove LB = 0
N01.LBP <- N01.LBP[-c(1), ] #final proportion, se & 95% ci
#save
#write.csv(N01.LBP, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N01.LBP.csv")


#2. Spatial analysis, by region

#I)   Overall population
#i)   regional prevalence for the overall population
N01_LBP_reg <- svyby(formula = ~PAINLB,
                     by = ~REGION,
                     design = NHIS01_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N01_LBP_reg.c <- N01_LBP_reg %>%
  select(1, 3, 5) %>%
  setNames(c("Region", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N01_LBP_reg.ci <- confint(N01_LBP_reg) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N01_LBP_reg.ci <- N01_LBP_reg.ci[-c(1:4), ]
#join proportion and ci
N01.LBP.region <- bind_cols(N01_LBP_reg.c, N01_LBP_reg.ci) #final proportion, se & 95% ci
#save
#write.csv(N01.LBP.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N01.LBP.region.csv")

#Change the region column to "NAME" to match sf
names(N01.LBP.region)[names(N01.LBP.region) == "Region"] <- "NAME"

N01.LBP.joined <- regions %>%
  left_join(N01.LBP.region)

N01.LBP.joined$NAME <- as.factor(N01.LBP.joined$NAME)

#Plot
LBPregplot01 <- ggplot(data = N01.LBP.joined) +
  geom_sf(aes(fill = Proportion), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  scale_fill_viridis_c(name = "Prevalence (%)", option = "viridis", direction = -1, limits = c(25, 35), breaks = c(25, 30, 35)) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  ggtitle("Regional prevalence (%) of US residents with low back pain in 2001") +
  theme(plot.title = element_text(size = 10, hjust = 0.5))
ggsave("N01.LBP.regchor.png", width = 5, height = 5)


#ii)  logistic regression by region
N01.LBP.reg.glm <- svyglm(relevel(PAINLB, ref = "0")~ REGION + AGE_P,
                          family = quasibinomial,
                          design = NHIS01_DO)
summary(N01.LBP.reg.glm)
exp(cbind(OR=coef(N01.LBP.reg.glm), confint(N01.LBP.reg.glm)))


#II)  prevalence by region, by age

#i)  age-specific regional prevalance
N01_LBP_reg.age <- svyby(formula = ~PAINLB,
                         by = ~REGION + AGE_P,
                         design = NHIS01_DO,
                         FUN = svymean,
                         na.rm = TRUE)
N01_LBP_reg.age.c <- N01_LBP_reg.age %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Age", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Age = as.character(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N01_LBP_reg.age.ci <- confint(N01_LBP_reg.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N01_LBP_reg.age.ci <- N01_LBP_reg.age.ci[-c(1:44), ]
#join proportion and ci
N01.LBP.age.region <- bind_cols(N01_LBP_reg.age.c, N01_LBP_reg.age.ci) #final prevalence, se & 95% ci
#save
#write.csv(N01.LBP.age.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N01.LBP.region.age.csv")

#Geofacet - Age & Region
LBP.Age_Reg <- ggplot(N01.LBP.age.region, aes(Age, Proportion)) +
  geom_bar(colour = "black", stat = "identity", fill = "#6600CC") +
  coord_flip() +
  facet_geo(~Region,
            grid = USregions) +
  theme_bw() +
  theme(text = element_text(colour = "black", size = 20)) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 17)) +
  xlab("Age group (years)") +
  geom_errorbar(aes(ymin = Proportion-SE, ymax = Proportion+SE, width = 0.3)) +
  scale_y_continuous(name = "Prevalence (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 18)) +
  ggtitle("Regional prevalence (%) of US residents with low back pain by age in 2001") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N01.GF_AgeReg.LBP.png", width = 14, height = 9)


#ii) age-specific regional logistic regressions
#18 to 24
sub1824 <- subset(NHIS01_DO, AGE_P == "18 to 24")
N01.LBP.age1824.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub1824)
summary(N01.LBP.age1824.reg)
exp(cbind(OR = coef(N01.LBP.age1824.reg), confint(N01.LBP.age1824.reg)))

#25 to 29
sub2529 <- subset(NHIS01_DO, AGE_P == "25 to 29")
N01.LBP.age2529.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub2529)
summary(N01.LBP.age2529.reg)
exp(cbind(OR = coef(N01.LBP.age2529.reg), confint(N01.LBP.age2529.reg)))

#30 to 34
sub3034 <- subset(NHIS01_DO, AGE_P == "30 to 34")
N01.LB.age3034.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub3034)
summary(N01.LB.age3034.reg)
exp(cbind(OR = coef(N01.LB.age3034.reg), confint(N01.LB.age3034.reg)))

#35 to 39
sub3539 <- subset(NHIS01_DO, AGE_P == "35 to 39")
N01.LBP.age3539.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub3539)
summary(N01.LBP.age3539.reg)
exp(cbind(OR = coef(N01.LBP.age3539.reg), confint(N01.LBP.age3539.reg)))

#40 to 44
sub4044 <- subset(NHIS01_DO, AGE_P == "40 to 44")
N01.LBP.age4044.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub4044)
summary(N01.LBP.age4044.reg)
exp(cbind(OR = coef(N01.LBP.age4044.reg), confint(N01.LBP.age4044.reg)))

#45 to 49
sub4549 <- subset(NHIS01_DO, AGE_P == "45 to 49")
N01.LBP.age4549.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub4549)
summary(N01.LBP.age4549.reg)
exp(cbind(OR = coef(N01.LBP.age4549.reg), confint(N01.LBP.age4549.reg)))

#50 to 54
sub5054 <- subset(NHIS01_DO, AGE_P == "50 to 54")
N01.LBP.age5054.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub5054)
summary(N01.LBP.age5054.reg)
exp(cbind(OR = coef(N01.LBP.age5054.reg), confint(N01.LBP.age5054.reg)))

#55 to 59
sub5559 <- subset(NHIS01_DO, AGE_P == "55 to 59")
N01.LBP.age5559.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub5559)
summary(N01.LBP.age5559.reg)
exp(cbind(OR = coef(N01.LBP.age5559.reg), confint(N01.LBP.age5559.reg)))

#60 to 64
sub6064 <- subset(NHIS01_DO, AGE_P == "60 to 64")
N01.LBP.age6064.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub6064)
summary(N01.LBP.age6064.reg)
exp(cbind(OR = coef(N01.LBP.age6064.reg), confint(N01.LBP.age6064.reg)))

#65 to 69
sub6569 <- subset(NHIS01_DO, AGE_P == "65 to 69")
N01.LBP.age6569.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub6569)
summary(N01.LBP.age6569.reg)
exp(cbind(OR = coef(N01.LBP.age6569.reg), confint(N01.LBP.age6569.reg)))

#70 and above
sub70 <- subset(NHIS01_DO, AGE_P == "70 and above")
N01.LBP.age70.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                            family = quasibinomial,
                            design = sub70)
summary(N01.LBP.age70.reg)
exp(cbind(OR = coef(N01.LBP.age70.reg), confint(N01.LBP.age70.reg)))

#III) prevalence by region, by sex

#i)  sex-specific regional prevalence
N01_LBP_reg.sex <- svyby(formula = ~PAINLB,
                         by = ~REGION + SEX,
                         design = NHIS01_DO,
                         FUN = svymean,
                         na.rm = TRUE)
N01_LBP_reg.sex.c <- N01_LBP_reg.sex %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Sex", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N01_LBP_reg.sex.ci <- confint(N01_LBP_reg.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N01_LBP_reg.sex.ci <- N01_LBP_reg.sex.ci[-c(1:8), ]
#join proportion and ci
N01.LBP.sex.region <- bind_cols(N01_LBP_reg.sex.c, N01_LBP_reg.sex.ci) #final prevalence, se & 95% ci
#save
#write.csv(N01.LBP.sex.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N01.LBP.region.sex.csv")

#Geofacet - Sex & Region
LBP.Sex_Reg01 <- ggplot(N01.LBP.sex.region, aes(Sex, Proportion)) +
  geom_bar(colour = "black", stat = "identity", fill = "#6600CC") +
  coord_flip() +
  facet_geo(~Region,
            grid = USregions) +
  theme_bw() +
  theme(text = element_text(colour = "black", size = 20)) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 17)) +
  xlab("Sex") +
  geom_errorbar(aes(ymin = Proportion-SE, ymax = Proportion+SE, width = 0.3)) +
  scale_y_continuous(name = "Prevalence (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 18)) +
  ggtitle("Regional prevalence (%) of US residents with low back pain by sex in 2001") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N01.GF_SexReg.LBP.png", width = 14, height = 9)


#ii) sex-specific regional logistic regressions
#males
submales <- subset(NHIS01_DO, SEX == "Male")
N01.LBP.sexmale.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + AGE_P,
                              family = quasibinomial,
                              design = submales)
summary(N01.LBP.sexmale.reg)
exp(cbind(OR = coef(N01.LBP.sexmale.reg), confint(N01.LBP.sexmale.reg)))

#females
subfemales <- subset(NHIS01_DO, SEX == "Female")
N01.LBP.sexfemale.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + AGE_P,
                                family = quasibinomial,
                                design = subfemales)
summary(N01.LBP.sexfemale.reg)
exp(cbind(OR = coef(N01.LBP.sexfemale.reg), confint(N01.LBP.sexfemale.reg)))


#3. Demographic trends

#I)  Age trends

#i)  Prevalence by age
N01_LBP_age <- svyby(formula = ~PAINLB,
                     by = ~AGE_P,
                     design = NHIS01_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N01_LBP_age.c <- N01_LBP_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N01_LBP_age_ci <- confint(N01_LBP_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove LBP=0 ci
N01_LBP_age_ci <- N01_LBP_age_ci[-c(1:11), ]
#join proportion and ci
N01_LBP.age <- bind_cols(N01_LBP_age.c, N01_LBP_age_ci) #final proportion, se & 95% ci
#Save
#write.csv(N01_LBP.age, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N01.LBP.age.csv")

#ii) Logistic regression by age
N01_LBP.age_glm <- svyglm(PAINLB~AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS01_DO)
summary(N01_LBP.age_glm)
exp(cbind(OR=coef(N01_LBP.age_glm), confint(N01_LBP.age_glm)))


#II)  Sex trends

#i)  Prevalence by sex
N01_LBP_sex <- svyby(formula = ~PAINLB,
                     by = ~SEX,
                     design = NHIS01_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N01_LBP_sex.c <- N01_LBP_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N01_LBP_sex_ci <- confint(N01_LBP_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N01_LBP_sex_ci <- N01_LBP_sex_ci[-c(1:2), ]
#join proportion and ci
N01.LBP.sex <- bind_cols(N01_LBP_sex.c, N01_LBP_sex_ci) #final proportion, se & 95% ci
#save
#write.csv(N01.LBP.sex, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N01.LBP.sex.csv")

#ii) Logistic regression by sex
N01.LB.sex.glm.c <- svyglm(PAINLB~relevel(SEX, ref = "Male") + AGE_P,
                           family = quasibinomial,
                           design = NHIS01_DO)
summary(N01.LB.sex.glm.c)
exp(cbind(OR=coef(N01.LB.sex.glm.c), confint(N01.LB.sex.glm.c)))

#4. Lifestyle trends

#I)  BMI
#logistic regression
N01.BMI.LBP.glm <- svyglm(PAINLB ~ BMI + AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS01_DO)
summary(N01.BMI.LBP.glm)
exp(cbind(OR=coef(N01.BMI.LBP.glm), confint(N01.BMI.LBP.glm)))

#II) Physical activity
#logistic regression
N01.PA.LBP.glm <- svyglm(PAINLB ~ relevel(factor(PAC, ordered = FALSE), ref = "Physically active") + AGE_P + SEX,
                         family = quasibinomial,
                         design = NHIS01_DO)
summary(N01.PA.LBP.glm)
exp(cbind(OR = coef(N01.PA.LBP.glm), confint(N01.PA.LBP.glm)))

#________________________________________________________________________________________________________________________________________________

# == 2000 == #

#1. Overall prevalence
N00_LBP <- svymean(~factor(PAINLB), 
                   NHIS00_DO, 
                   na.rm = TRUE)
N00_LBP.c <- N00_LBP %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N00_LBP_ci <- confint(N00_LBP) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
N00.LBP <- bind_cols(N00_LBP.c, N00_LBP_ci)
#remove LB = 0
N00.LBP <- N00.LBP[-c(1), ] #final proportion, se & 95% ci
#save
#write.csv(N00.LBP, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N00.LBP.csv")


#2. Spatial analysis, by region

#I)   Overall population
#i)   regional prevalence for the overall population
N00_LBP_reg <- svyby(formula = ~PAINLB,
                     by = ~REGION,
                     design = NHIS00_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N00_LBP_reg.c <- N00_LBP_reg %>%
  select(1, 3, 5) %>%
  setNames(c("Region", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N00_LBP_reg.ci <- confint(N00_LBP_reg) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N00_LBP_reg.ci <- N00_LBP_reg.ci[-c(1:4), ]
#join proportion and ci
N00.LBP.region <- bind_cols(N00_LBP_reg.c, N00_LBP_reg.ci) #final proportion, se & 95% ci
#save
#write.csv(N00.LBP.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N00.LBP.region.csv")

#Change the region column to "NAME" to match sf
names(N00.LBP.region)[names(N00.LBP.region) == "Region"] <- "NAME"

N00.LBP.joined <- regions %>%
  left_join(N00.LBP.region)

N00.LBP.joined$NAME <- as.factor(N00.LBP.joined$NAME)

#Plot
LBPregplot00 <- ggplot(data = N00.LBP.joined) +
  geom_sf(aes(fill = Proportion), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  scale_fill_viridis_c(name = "Prevalence (%)", option = "viridis", direction = -1, limits = c(25, 35), breaks = c(25, 30, 35)) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  ggtitle("Regional prevalence (%) of US residents with low back pain in 2000") +
  theme(plot.title = element_text(size = 10, hjust = 0.5))
ggsave("N00.LBP.regchor.png", width = 5, height = 5)


#ii)  logistic regression by region
N00.LBP.reg.glm <- svyglm(relevel(PAINLB, ref = "0")~ REGION + AGE_P,
                          family = quasibinomial,
                          design = NHIS00_DO)
summary(N00.LBP.reg.glm)
exp(cbind(OR=coef(N00.LBP.reg.glm), confint(N00.LBP.reg.glm)))


#II)  prevalence by region, by age

#i)  age-specific regional prevalance
N00_LBP_reg.age <- svyby(formula = ~PAINLB,
                         by = ~REGION + AGE_P,
                         design = NHIS00_DO,
                         FUN = svymean,
                         na.rm = TRUE)
N00_LBP_reg.age.c <- N00_LBP_reg.age %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Age", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Age = as.character(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N00_LBP_reg.age.ci <- confint(N00_LBP_reg.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N00_LBP_reg.age.ci <- N00_LBP_reg.age.ci[-c(1:44), ]
#join proportion and ci
N00.LBP.age.region <- bind_cols(N00_LBP_reg.age.c, N00_LBP_reg.age.ci) #final prevalence, se & 95% ci
#save
#write.csv(N00.LBP.age.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N00.LBP.region.age.csv")

#Geofacet - Age & Region
LBP.Age_Reg <- ggplot(N00.LBP.age.region, aes(Age, Proportion)) +
  geom_bar(colour = "black", stat = "identity", fill = "#6600CC") +
  coord_flip() +
  facet_geo(~Region,
            grid = USregions) +
  theme_bw() +
  theme(text = element_text(colour = "black", size = 20)) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 17)) +
  xlab("Age group (years)") +
  geom_errorbar(aes(ymin = Proportion-SE, ymax = Proportion+SE, width = 0.3)) +
  scale_y_continuous(name = "Prevalence (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 18)) +
  ggtitle("Regional prevalence (%) of US residents with low back pain by age in 2000") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N00.GF_AgeReg.LBP.png", width = 14, height = 9)


#ii) age-specific regional logistic regressions
#18 to 24
sub1824 <- subset(NHIS00_DO, AGE_P == "18 to 24")
N00.LBP.age1824.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub1824)
summary(N00.LBP.age1824.reg)
exp(cbind(OR = coef(N00.LBP.age1824.reg), confint(N00.LBP.age1824.reg)))

#25 to 29
sub2529 <- subset(NHIS00_DO, AGE_P == "25 to 29")
N00.LBP.age2529.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub2529)
summary(N00.LBP.age2529.reg)
exp(cbind(OR = coef(N00.LBP.age2529.reg), confint(N00.LBP.age2529.reg)))

#30 to 34
sub3034 <- subset(NHIS00_DO, AGE_P == "30 to 34")
N00.LB.age3034.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub3034)
summary(N00.LB.age3034.reg)
exp(cbind(OR = coef(N00.LB.age3034.reg), confint(N00.LB.age3034.reg)))

#35 to 39
sub3539 <- subset(NHIS00_DO, AGE_P == "35 to 39")
N00.LBP.age3539.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub3539)
summary(N00.LBP.age3539.reg)
exp(cbind(OR = coef(N00.LBP.age3539.reg), confint(N00.LBP.age3539.reg)))

#40 to 44
sub4044 <- subset(NHIS00_DO, AGE_P == "40 to 44")
N00.LBP.age4044.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub4044)
summary(N00.LBP.age4044.reg)
exp(cbind(OR = coef(N00.LBP.age4044.reg), confint(N00.LBP.age4044.reg)))

#45 to 49
sub4549 <- subset(NHIS00_DO, AGE_P == "45 to 49")
N00.LBP.age4549.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub4549)
summary(N00.LBP.age4549.reg)
exp(cbind(OR = coef(N00.LBP.age4549.reg), confint(N00.LBP.age4549.reg)))

#50 to 54
sub5054 <- subset(NHIS00_DO, AGE_P == "50 to 54")
N00.LBP.age5054.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub5054)
summary(N00.LBP.age5054.reg)
exp(cbind(OR = coef(N00.LBP.age5054.reg), confint(N00.LBP.age5054.reg)))

#55 to 59
sub5559 <- subset(NHIS00_DO, AGE_P == "55 to 59")
N00.LBP.age5559.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub5559)
summary(N00.LBP.age5559.reg)
exp(cbind(OR = coef(N00.LBP.age5559.reg), confint(N00.LBP.age5559.reg)))

#60 to 64
sub6064 <- subset(NHIS00_DO, AGE_P == "60 to 64")
N00.LBP.age6064.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub6064)
summary(N00.LBP.age6064.reg)
exp(cbind(OR = coef(N00.LBP.age6064.reg), confint(N00.LBP.age6064.reg)))

#65 to 69
sub6569 <- subset(NHIS00_DO, AGE_P == "65 to 69")
N00.LBP.age6569.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub6569)
summary(N00.LBP.age6569.reg)
exp(cbind(OR = coef(N00.LBP.age6569.reg), confint(N00.LBP.age6569.reg)))

#70 and above
sub70 <- subset(NHIS00_DO, AGE_P == "70 and above")
N00.LBP.age70.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                            family = quasibinomial,
                            design = sub70)
summary(N00.LBP.age70.reg)
exp(cbind(OR = coef(N00.LBP.age70.reg), confint(N00.LBP.age70.reg)))

#III) prevalence by region, by sex

#i)  sex-specific regional prevalence
N00_LBP_reg.sex <- svyby(formula = ~PAINLB,
                         by = ~REGION + SEX,
                         design = NHIS00_DO,
                         FUN = svymean,
                         na.rm = TRUE)
N00_LBP_reg.sex.c <- N00_LBP_reg.sex %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Sex", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N00_LBP_reg.sex.ci <- confint(N00_LBP_reg.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N00_LBP_reg.sex.ci <- N00_LBP_reg.sex.ci[-c(1:8), ]
#join proportion and ci
N00.LBP.sex.region <- bind_cols(N00_LBP_reg.sex.c, N00_LBP_reg.sex.ci) #final prevalence, se & 95% ci
#save
#write.csv(N00.LBP.sex.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N00.LBP.region.sex.csv")

#Geofacet - Sex & Region
LBP.Sex_Reg00 <- ggplot(N00.LBP.sex.region, aes(Sex, Proportion)) +
  geom_bar(colour = "black", stat = "identity", fill = "#6600CC") +
  coord_flip() +
  facet_geo(~Region,
            grid = USregions) +
  theme_bw() +
  theme(text = element_text(colour = "black", size = 20)) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 17)) +
  xlab("Sex") +
  geom_errorbar(aes(ymin = Proportion-SE, ymax = Proportion+SE, width = 0.3)) +
  scale_y_continuous(name = "Prevalence (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 18)) +
  ggtitle("Regional prevalence (%) of US residents with low back pain by sex in 2000") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N00.GF_SexReg.LBP.png", width = 14, height = 9)


#ii) sex-specific regional logistic regressions
#males
submales <- subset(NHIS00_DO, SEX == "Male")
N00.LBP.sexmale.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + AGE_P,
                              family = quasibinomial,
                              design = submales)
summary(N00.LBP.sexmale.reg)
exp(cbind(OR = coef(N00.LBP.sexmale.reg), confint(N00.LBP.sexmale.reg)))

#females
subfemales <- subset(NHIS00_DO, SEX == "Female")
N00.LBP.sexfemale.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + AGE_P,
                                family = quasibinomial,
                                design = subfemales)
summary(N00.LBP.sexfemale.reg)
exp(cbind(OR = coef(N00.LBP.sexfemale.reg), confint(N00.LBP.sexfemale.reg)))


#3. Demographic trends

#I)  Age trends

#i)  Prevalence by age
N00_LBP_age <- svyby(formula = ~PAINLB,
                     by = ~AGE_P,
                     design = NHIS00_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N00_LBP_age.c <- N00_LBP_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N00_LBP_age_ci <- confint(N00_LBP_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove LBP=0 ci
N00_LBP_age_ci <- N00_LBP_age_ci[-c(1:11), ]
#join proportion and ci
N00_LBP.age <- bind_cols(N00_LBP_age.c, N00_LBP_age_ci) #final proportion, se & 95% ci
#Save
#write.csv(N00_LBP.age, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N00.LBP.age.csv")

#ii) Logistic regression by age
N00_LBP.age_glm <- svyglm(PAINLB~AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS00_DO)
summary(N00_LBP.age_glm)
exp(cbind(OR=coef(N00_LBP.age_glm), confint(N00_LBP.age_glm)))


#II)  Sex trends

#i)  Prevalence by sex
N00_LBP_sex <- svyby(formula = ~PAINLB,
                     by = ~SEX,
                     design = NHIS00_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N00_LBP_sex.c <- N00_LBP_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N00_LBP_sex_ci <- confint(N00_LBP_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N00_LBP_sex_ci <- N00_LBP_sex_ci[-c(1:2), ]
#join proportion and ci
N00.LBP.sex <- bind_cols(N00_LBP_sex.c, N00_LBP_sex_ci) #final proportion, se & 95% ci
#save
#write.csv(N00.LBP.sex, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N00.LBP.sex.csv")

#ii) Logistic regression by sex
N00.LB.sex.glm.c <- svyglm(PAINLB~relevel(SEX, ref = "Male") + AGE_P,
                           family = quasibinomial,
                           design = NHIS00_DO)
summary(N00.LB.sex.glm.c)
exp(cbind(OR=coef(N00.LB.sex.glm.c), confint(N00.LB.sex.glm.c)))


#4. Lifestyle trends

#I)  BMI
#logistic regression
N00.BMI.LBP.glm <- svyglm(PAINLB ~ BMI + AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS00_DO)
summary(N00.BMI.LBP.glm)
exp(cbind(OR=coef(N00.BMI.LBP.glm), confint(N00.BMI.LBP.glm)))

#II) Physical activity
#logistic regression
N00.PA.LBP.glm <- svyglm(PAINLB ~ relevel(factor(PAC, ordered = FALSE), ref = "Physically active") + AGE_P + SEX,
                         family = quasibinomial,
                         design = NHIS00_DO)
summary(N00.PA.LBP.glm)
exp(cbind(OR = coef(N00.PA.LBP.glm), confint(N00.PA.LBP.glm)))

#_______________________________________________________________________________________________________________________________________________

# == 1999 == #

#1. Overall prevalence
N99_LBP <- svymean(~factor(PAINLB), 
                   NHIS99_DO, 
                   na.rm = TRUE)
N99_LBP.c <- N99_LBP %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N99_LBP_ci <- confint(N99_LBP) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
N99.LBP <- bind_cols(N99_LBP.c, N99_LBP_ci)
#remove LB = 0
N99.LBP <- N99.LBP[-c(1), ] #final proportion, se & 95% ci
#save
#write.csv(N99.LBP, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N99.LBP.csv")


#2. Spatial analysis, by region

#I)   Overall population
#i)   regional prevalence for the overall population
N99_LBP_reg <- svyby(formula = ~PAINLB,
                     by = ~REGION,
                     design = NHIS99_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N99_LBP_reg.c <- N99_LBP_reg %>%
  select(1, 3, 5) %>%
  setNames(c("Region", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N99_LBP_reg.ci <- confint(N99_LBP_reg) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N99_LBP_reg.ci <- N99_LBP_reg.ci[-c(1:4), ]
#join proportion and ci
N99.LBP.region <- bind_cols(N99_LBP_reg.c, N99_LBP_reg.ci) #final proportion, se & 95% ci
#save
#write.csv(N99.LBP.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N99.LBP.region.csv")

#Change the region column to "NAME" to match sf
names(N99.LBP.region)[names(N99.LBP.region) == "Region"] <- "NAME"

N99.LBP.joined <- regions %>%
  left_join(N99.LBP.region)

N99.LBP.joined$NAME <- as.factor(N99.LBP.joined$NAME)

#Plot
LBPregplot99 <- ggplot(data = N99.LBP.joined) +
  geom_sf(aes(fill = Proportion), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  scale_fill_viridis_c(name = "Prevalence (%)", option = "viridis", direction = -1, limits = c(25, 35), breaks = c(25, 30, 35)) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  ggtitle("Regional prevalence (%) of US residents with low back pain in 1999") +
  theme(plot.title = element_text(size = 10, hjust = 0.5))
ggsave("N99.LBP.regchor.png", width = 5, height = 5)


#ii)  logistic regression by region
N99.LBP.reg.glm <- svyglm(relevel(PAINLB, ref = "0")~ REGION + AGE_P,
                          family = quasibinomial,
                          design = NHIS99_DO)
summary(N99.LBP.reg.glm)
exp(cbind(OR=coef(N99.LBP.reg.glm), confint(N99.LBP.reg.glm)))


#II)  prevalence by region, by age

#i)  age-specific regional prevalance
N99_LBP_reg.age <- svyby(formula = ~PAINLB,
                         by = ~REGION + AGE_P,
                         design = NHIS99_DO,
                         FUN = svymean,
                         na.rm = TRUE)
N99_LBP_reg.age.c <- N99_LBP_reg.age %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Age", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Age = as.character(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N99_LBP_reg.age.ci <- confint(N99_LBP_reg.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N99_LBP_reg.age.ci <- N99_LBP_reg.age.ci[-c(1:44), ]
#join proportion and ci
N99.LBP.age.region <- bind_cols(N99_LBP_reg.age.c, N99_LBP_reg.age.ci) #final prevalence, se & 95% ci
#save
#write.csv(N99.LBP.age.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N99.LBP.region.age.csv")

#Geofacet - Age & Region
LBP.Age_Reg <- ggplot(N99.LBP.age.region, aes(Age, Proportion)) +
  geom_bar(colour = "black", stat = "identity", fill = "#6600CC") +
  coord_flip() +
  facet_geo(~Region,
            grid = USregions) +
  theme_bw() +
  theme(text = element_text(colour = "black", size = 20)) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 17)) +
  xlab("Age group (years)") +
  geom_errorbar(aes(ymin = Proportion-SE, ymax = Proportion+SE, width = 0.3)) +
  scale_y_continuous(name = "Prevalence (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 18)) +
  ggtitle("Regional prevalence (%) of US residents with low back pain by age in 1999") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N99.GF_AgeReg.LBP.png", width = 14, height = 9)


#ii) age-specific regional logistic regressions
#18 to 24
sub1824 <- subset(NHIS99_DO, AGE_P == "18 to 24")
N99.LBP.age1824.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub1824)
summary(N99.LBP.age1824.reg)
exp(cbind(OR = coef(N99.LBP.age1824.reg), confint(N99.LBP.age1824.reg)))

#25 to 29
sub2529 <- subset(NHIS99_DO, AGE_P == "25 to 29")
N99.LBP.age2529.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub2529)
summary(N99.LBP.age2529.reg)
exp(cbind(OR = coef(N99.LBP.age2529.reg), confint(N99.LBP.age2529.reg)))

#30 to 34
sub3034 <- subset(NHIS99_DO, AGE_P == "30 to 34")
N99.LB.age3034.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub3034)
summary(N99.LB.age3034.reg)
exp(cbind(OR = coef(N99.LB.age3034.reg), confint(N99.LB.age3034.reg)))

#35 to 39
sub3539 <- subset(NHIS99_DO, AGE_P == "35 to 39")
N99.LBP.age3539.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub3539)
summary(N99.LBP.age3539.reg)
exp(cbind(OR = coef(N99.LBP.age3539.reg), confint(N99.LBP.age3539.reg)))

#40 to 44
sub4044 <- subset(NHIS99_DO, AGE_P == "40 to 44")
N99.LBP.age4044.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub4044)
summary(N99.LBP.age4044.reg)
exp(cbind(OR = coef(N99.LBP.age4044.reg), confint(N99.LBP.age4044.reg)))

#45 to 49
sub4549 <- subset(NHIS99_DO, AGE_P == "45 to 49")
N99.LBP.age4549.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub4549)
summary(N99.LBP.age4549.reg)
exp(cbind(OR = coef(N99.LBP.age4549.reg), confint(N99.LBP.age4549.reg)))

#50 to 54
sub5054 <- subset(NHIS99_DO, AGE_P == "50 to 54")
N99.LBP.age5054.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub5054)
summary(N99.LBP.age5054.reg)
exp(cbind(OR = coef(N99.LBP.age5054.reg), confint(N99.LBP.age5054.reg)))

#55 to 59
sub5559 <- subset(NHIS99_DO, AGE_P == "55 to 59")
N99.LBP.age5559.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub5559)
summary(N99.LBP.age5559.reg)
exp(cbind(OR = coef(N99.LBP.age5559.reg), confint(N99.LBP.age5559.reg)))

#60 to 64
sub6064 <- subset(NHIS99_DO, AGE_P == "60 to 64")
N99.LBP.age6064.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub6064)
summary(N99.LBP.age6064.reg)
exp(cbind(OR = coef(N99.LBP.age6064.reg), confint(N99.LBP.age6064.reg)))

#65 to 69
sub6569 <- subset(NHIS99_DO, AGE_P == "65 to 69")
N99.LBP.age6569.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub6569)
summary(N99.LBP.age6569.reg)
exp(cbind(OR = coef(N99.LBP.age6569.reg), confint(N99.LBP.age6569.reg)))

#70 and above
sub70 <- subset(NHIS99_DO, AGE_P == "70 and above")
N99.LBP.age70.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                            family = quasibinomial,
                            design = sub70)
summary(N99.LBP.age70.reg)
exp(cbind(OR = coef(N99.LBP.age70.reg), confint(N99.LBP.age70.reg)))

#III) prevalence by region, by sex

#i)  sex-specific regional prevalence
N99_LBP_reg.sex <- svyby(formula = ~PAINLB,
                         by = ~REGION + SEX,
                         design = NHIS99_DO,
                         FUN = svymean,
                         na.rm = TRUE)
N99_LBP_reg.sex.c <- N99_LBP_reg.sex %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Sex", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N99_LBP_reg.sex.ci <- confint(N99_LBP_reg.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N99_LBP_reg.sex.ci <- N99_LBP_reg.sex.ci[-c(1:8), ]
#join proportion and ci
N99.LBP.sex.region <- bind_cols(N99_LBP_reg.sex.c, N99_LBP_reg.sex.ci) #final prevalence, se & 95% ci
#save
#write.csv(N99.LBP.sex.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N99.LBP.region.sex.csv")

#Geofacet - Sex & Region
LBP.Sex_Reg99 <- ggplot(N99.LBP.sex.region, aes(Sex, Proportion)) +
  geom_bar(colour = "black", stat = "identity", fill = "#6600CC") +
  coord_flip() +
  facet_geo(~Region,
            grid = USregions) +
  theme_bw() +
  theme(text = element_text(colour = "black", size = 20)) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 17)) +
  xlab("Sex") +
  geom_errorbar(aes(ymin = Proportion-SE, ymax = Proportion+SE, width = 0.3)) +
  scale_y_continuous(name = "Prevalence (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 18)) +
  ggtitle("Regional prevalence (%) of US residents with low back pain by sex in 1999") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N99.GF_SexReg.LBP.png", width = 14, height = 9)


#ii) sex-specific regional logistic regressions
#males
submales <- subset(NHIS99_DO, SEX == "Male")
N99.LBP.sexmale.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + AGE_P,
                              family = quasibinomial,
                              design = submales)
summary(N99.LBP.sexmale.reg)
exp(cbind(OR = coef(N99.LBP.sexmale.reg), confint(N99.LBP.sexmale.reg)))

#females
subfemales <- subset(NHIS99_DO, SEX == "Female")
N99.LBP.sexfemale.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + AGE_P,
                                family = quasibinomial,
                                design = subfemales)
summary(N99.LBP.sexfemale.reg)
exp(cbind(OR = coef(N99.LBP.sexfemale.reg), confint(N99.LBP.sexfemale.reg)))


#3. Demographic trends

#I)  Age trends

#i)  Prevalence by age
N99_LBP_age <- svyby(formula = ~PAINLB,
                     by = ~AGE_P,
                     design = NHIS99_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N99_LBP_age.c <- N99_LBP_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N99_LBP_age_ci <- confint(N99_LBP_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove LBP=0 ci
N99_LBP_age_ci <- N99_LBP_age_ci[-c(1:11), ]
#join proportion and ci
N99_LBP.age <- bind_cols(N99_LBP_age.c, N99_LBP_age_ci) #final proportion, se & 95% ci
#Save
#write.csv(N99_LBP.age, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N99.LBP.age.csv")

#ii) Logistic regression by age
N99_LBP.age_glm <- svyglm(PAINLB~AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS99_DO)
summary(N99_LBP.age_glm)
exp(cbind(OR=coef(N99_LBP.age_glm), confint(N99_LBP.age_glm)))


#II)  Sex trends

#i)  Prevalence by sex
N99_LBP_sex <- svyby(formula = ~PAINLB,
                     by = ~SEX,
                     design = NHIS99_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N99_LBP_sex.c <- N99_LBP_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N99_LBP_sex_ci <- confint(N99_LBP_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N99_LBP_sex_ci <- N99_LBP_sex_ci[-c(1:2), ]
#join proportion and ci
N99.LBP.sex <- bind_cols(N99_LBP_sex.c, N99_LBP_sex_ci) #final proportion, se & 95% ci
#save
#write.csv(N99.LBP.sex, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N99.LBP.sex.csv")

#ii) Logistic regression by sex
N99.LB.sex.glm.c <- svyglm(PAINLB~relevel(SEX, ref = "Male") + AGE_P,
                           family = quasibinomial,
                           design = NHIS99_DO)
summary(N99.LB.sex.glm.c)
exp(cbind(OR=coef(N99.LB.sex.glm.c), confint(N99.LB.sex.glm.c)))


#4. Lifestyle trends

#I)  BMI
#logistic regression
N99.BMI.LBP.glm <- svyglm(PAINLB ~ BMI + AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS99_DO)
summary(N99.BMI.LBP.glm)
exp(cbind(OR=coef(N99.BMI.LBP.glm), confint(N99.BMI.LBP.glm)))

#II) Physical activity
#logistic regression
N99.PA.LBP.glm <- svyglm(PAINLB ~ relevel(factor(PAC, ordered = FALSE), ref = "Physically active") + AGE_P + SEX,
                         family = quasibinomial,
                         design = NHIS99_DO)
summary(N99.PA.LBP.glm)
exp(cbind(OR = coef(N99.PA.LBP.glm), confint(N99.PA.LBP.glm)))


#________________________________________________________________________________________________________________________________________________

# == 1998 == #

#1. Overall prevalence
N98_LBP <- svymean(~factor(PAINLB), 
                   NHIS98_DO, 
                   na.rm = TRUE)
N98_LBP.c <- N98_LBP %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N98_LBP_ci <- confint(N98_LBP) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
N98.LBP <- bind_cols(N98_LBP.c, N98_LBP_ci)
#remove LB = 0
N98.LBP <- N98.LBP[-c(1), ] #final proportion, se & 95% ci
#save
#write.csv(N98.LBP, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N98.LBP.csv")


#2. Spatial analysis, by region

#I)   Overall population
#i)   regional prevalence for the overall population
N98_LBP_reg <- svyby(formula = ~PAINLB,
                     by = ~REGION,
                     design = NHIS98_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N98_LBP_reg.c <- N98_LBP_reg %>%
  select(1, 3, 5) %>%
  setNames(c("Region", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N98_LBP_reg.ci <- confint(N98_LBP_reg) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N98_LBP_reg.ci <- N98_LBP_reg.ci[-c(1:4), ]
#join proportion and ci
N98.LBP.region <- bind_cols(N98_LBP_reg.c, N98_LBP_reg.ci) #final proportion, se & 95% ci
#save
#write.csv(N98.LBP.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N98.LBP.region.csv")

#Change the region column to "NAME" to match sf
names(N98.LBP.region)[names(N98.LBP.region) == "Region"] <- "NAME"

N98.LBP.joined <- regions %>%
  left_join(N98.LBP.region)

N98.LBP.joined$NAME <- as.factor(N98.LBP.joined$NAME)

#Plot
LBPregplot98 <- ggplot(data = N98.LBP.joined) +
  geom_sf(aes(fill = Proportion), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  scale_fill_viridis_c(name = "Prevalence (%)", option = "viridis", direction = -1, limits = c(25, 35), breaks = c(25, 30, 35)) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  ggtitle("Regional prevalence (%) of US residents with low back pain in 1998") +
  theme(plot.title = element_text(size = 10, hjust = 0.5))
ggsave("N98.LBP.regchor.png", width = 5, height = 5)


#ii)  logistic regression by region
N98.LBP.reg.glm <- svyglm(relevel(PAINLB, ref = "0")~ REGION + AGE_P,
                          family = quasibinomial,
                          design = NHIS98_DO)
summary(N98.LBP.reg.glm)
exp(cbind(OR=coef(N98.LBP.reg.glm), confint(N98.LBP.reg.glm)))


#II)  prevalence by region, by age

#i)  age-specific regional prevalance
N98_LBP_reg.age <- svyby(formula = ~PAINLB,
                         by = ~REGION + AGE_P,
                         design = NHIS98_DO,
                         FUN = svymean,
                         na.rm = TRUE)
N98_LBP_reg.age.c <- N98_LBP_reg.age %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Age", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Age = as.character(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N98_LBP_reg.age.ci <- confint(N98_LBP_reg.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N98_LBP_reg.age.ci <- N98_LBP_reg.age.ci[-c(1:44), ]
#join proportion and ci
N98.LBP.age.region <- bind_cols(N98_LBP_reg.age.c, N98_LBP_reg.age.ci) #final prevalence, se & 95% ci
#save
#write.csv(N98.LBP.age.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N98.LBP.region.age.csv")

#Geofacet - Age & Region
LBP.Age_Reg <- ggplot(N98.LBP.age.region, aes(Age, Proportion)) +
  geom_bar(colour = "black", stat = "identity", fill = "#6600CC") +
  coord_flip() +
  facet_geo(~Region,
            grid = USregions) +
  theme_bw() +
  theme(text = element_text(colour = "black", size = 20)) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 17)) +
  xlab("Age group (years)") +
  geom_errorbar(aes(ymin = Proportion-SE, ymax = Proportion+SE, width = 0.3)) +
  scale_y_continuous(name = "Prevalence (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 18)) +
  ggtitle("Regional prevalence (%) of US residents with low back pain by age in 1998") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N98.GF_AgeReg.LBP.png", width = 14, height = 9)


#ii) age-specific regional logistic regressions
#18 to 24
sub1824 <- subset(NHIS98_DO, AGE_P == "18 to 24")
N98.LBP.age1824.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub1824)
summary(N98.LBP.age1824.reg)
exp(cbind(OR = coef(N98.LBP.age1824.reg), confint(N98.LBP.age1824.reg)))

#25 to 29
sub2529 <- subset(NHIS98_DO, AGE_P == "25 to 29")
N98.LBP.age2529.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub2529)
summary(N98.LBP.age2529.reg)
exp(cbind(OR = coef(N98.LBP.age2529.reg), confint(N98.LBP.age2529.reg)))

#30 to 34
sub3034 <- subset(NHIS98_DO, AGE_P == "30 to 34")
N98.LB.age3034.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub3034)
summary(N98.LB.age3034.reg)
exp(cbind(OR = coef(N98.LB.age3034.reg), confint(N98.LB.age3034.reg)))

#35 to 39
sub3539 <- subset(NHIS98_DO, AGE_P == "35 to 39")
N98.LBP.age3539.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub3539)
summary(N98.LBP.age3539.reg)
exp(cbind(OR = coef(N98.LBP.age3539.reg), confint(N98.LBP.age3539.reg)))

#40 to 44
sub4044 <- subset(NHIS98_DO, AGE_P == "40 to 44")
N98.LBP.age4044.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub4044)
summary(N98.LBP.age4044.reg)
exp(cbind(OR = coef(N98.LBP.age4044.reg), confint(N98.LBP.age4044.reg)))

#45 to 49
sub4549 <- subset(NHIS98_DO, AGE_P == "45 to 49")
N98.LBP.age4549.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub4549)
summary(N98.LBP.age4549.reg)
exp(cbind(OR = coef(N98.LBP.age4549.reg), confint(N98.LBP.age4549.reg)))

#50 to 54
sub5054 <- subset(NHIS98_DO, AGE_P == "50 to 54")
N98.LBP.age5054.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub5054)
summary(N98.LBP.age5054.reg)
exp(cbind(OR = coef(N98.LBP.age5054.reg), confint(N98.LBP.age5054.reg)))

#55 to 59
sub5559 <- subset(NHIS98_DO, AGE_P == "55 to 59")
N98.LBP.age5559.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub5559)
summary(N98.LBP.age5559.reg)
exp(cbind(OR = coef(N98.LBP.age5559.reg), confint(N98.LBP.age5559.reg)))

#60 to 64
sub6064 <- subset(NHIS98_DO, AGE_P == "60 to 64")
N98.LBP.age6064.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub6064)
summary(N98.LBP.age6064.reg)
exp(cbind(OR = coef(N98.LBP.age6064.reg), confint(N98.LBP.age6064.reg)))

#65 to 69
sub6569 <- subset(NHIS98_DO, AGE_P == "65 to 69")
N98.LBP.age6569.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub6569)
summary(N98.LBP.age6569.reg)
exp(cbind(OR = coef(N98.LBP.age6569.reg), confint(N98.LBP.age6569.reg)))

#70 and above
sub70 <- subset(NHIS98_DO, AGE_P == "70 and above")
N98.LBP.age70.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                            family = quasibinomial,
                            design = sub70)
summary(N98.LBP.age70.reg)
exp(cbind(OR = coef(N98.LBP.age70.reg), confint(N98.LBP.age70.reg)))

#III) prevalence by region, by sex

#i)  sex-specific regional prevalence
N98_LBP_reg.sex <- svyby(formula = ~PAINLB,
                         by = ~REGION + SEX,
                         design = NHIS98_DO,
                         FUN = svymean,
                         na.rm = TRUE)
N98_LBP_reg.sex.c <- N98_LBP_reg.sex %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Sex", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N98_LBP_reg.sex.ci <- confint(N98_LBP_reg.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N98_LBP_reg.sex.ci <- N98_LBP_reg.sex.ci[-c(1:8), ]
#join proportion and ci
N98.LBP.sex.region <- bind_cols(N98_LBP_reg.sex.c, N98_LBP_reg.sex.ci) #final prevalence, se & 95% ci
#save
#write.csv(N98.LBP.sex.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N98.LBP.region.sex.csv")

#Geofacet - Sex & Region
LBP.Sex_Reg98 <- ggplot(N98.LBP.sex.region, aes(Sex, Proportion)) +
  geom_bar(colour = "black", stat = "identity", fill = "#6600CC") +
  coord_flip() +
  facet_geo(~Region,
            grid = USregions) +
  theme_bw() +
  theme(text = element_text(colour = "black", size = 20)) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 17)) +
  xlab("Sex") +
  geom_errorbar(aes(ymin = Proportion-SE, ymax = Proportion+SE, width = 0.3)) +
  scale_y_continuous(name = "Prevalence (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 18)) +
  ggtitle("Regional prevalence (%) of US residents with low back pain by sex in 1998") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N98.GF_SexReg.LBP.png", width = 14, height = 9)


#ii) sex-specific regional logistic regressions
#males
submales <- subset(NHIS98_DO, SEX == "Male")
N98.LBP.sexmale.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + AGE_P,
                              family = quasibinomial,
                              design = submales)
summary(N98.LBP.sexmale.reg)
exp(cbind(OR = coef(N98.LBP.sexmale.reg), confint(N98.LBP.sexmale.reg)))

#females
subfemales <- subset(NHIS98_DO, SEX == "Female")
N98.LBP.sexfemale.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + AGE_P,
                                family = quasibinomial,
                                design = subfemales)
summary(N98.LBP.sexfemale.reg)
exp(cbind(OR = coef(N98.LBP.sexfemale.reg), confint(N98.LBP.sexfemale.reg)))


#3. Demographic trends

#I)  Age trends

#i)  Prevalence by age
N98_LBP_age <- svyby(formula = ~PAINLB,
                     by = ~AGE_P,
                     design = NHIS98_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N98_LBP_age.c <- N98_LBP_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N98_LBP_age_ci <- confint(N98_LBP_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove LBP=0 ci
N98_LBP_age_ci <- N98_LBP_age_ci[-c(1:11), ]
#join proportion and ci
N98_LBP.age <- bind_cols(N98_LBP_age.c, N98_LBP_age_ci) #final proportion, se & 95% ci
#Save
#write.csv(N98_LBP.age, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N98.LBP.age.csv")

#ii) Logistic regression by age
N98_LBP.age_glm <- svyglm(PAINLB~AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS98_DO)
summary(N98_LBP.age_glm)
exp(cbind(OR=coef(N98_LBP.age_glm), confint(N98_LBP.age_glm)))


#II)  Sex trends

#i)  Prevalence by sex
N98_LBP_sex <- svyby(formula = ~PAINLB,
                     by = ~SEX,
                     design = NHIS98_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N98_LBP_sex.c <- N98_LBP_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N98_LBP_sex_ci <- confint(N98_LBP_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N98_LBP_sex_ci <- N98_LBP_sex_ci[-c(1:2), ]
#join proportion and ci
N98.LBP.sex <- bind_cols(N98_LBP_sex.c, N98_LBP_sex_ci) #final proportion, se & 95% ci
#save
#write.csv(N98.LBP.sex, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N98.LBP.sex.csv")

#ii) Logistic regression by sex
N98.LB.sex.glm.c <- svyglm(PAINLB~relevel(SEX, ref = "Male") + AGE_P,
                           family = quasibinomial,
                           design = NHIS98_DO)
summary(N98.LB.sex.glm.c)
exp(cbind(OR=coef(N98.LB.sex.glm.c), confint(N98.LB.sex.glm.c)))


#4. Lifestyle trends

#I)  BMI
#logistic regression
N98.BMI.LBP.glm <- svyglm(PAINLB ~ BMI + AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS98_DO)
summary(N98.BMI.LBP.glm)
exp(cbind(OR=coef(N98.BMI.LBP.glm), confint(N98.BMI.LBP.glm)))

#II) Physical activity
#logistic regression
N98.PA.LBP.glm <- svyglm(PAINLB ~ relevel(factor(PAC, ordered = FALSE), ref = "Physically active") + AGE_P + SEX,
                         family = quasibinomial,
                         design = NHIS98_DO)
summary(N98.PA.LBP.glm)
exp(cbind(OR = coef(N98.PA.LBP.glm), confint(N98.PA.LBP.glm)))

#________________________________________________________________________________________________________________________________________________

# == 1997 == #

#1. Overall prevalence
N97_LBP <- svymean(~factor(PAINLB), 
                   NHIS97_DO, 
                   na.rm = TRUE)
N97_LBP.c <- N97_LBP %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N97_LBP_ci <- confint(N97_LBP) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
N97.LBP <- bind_cols(N97_LBP.c, N97_LBP_ci)
#remove LB = 0
N97.LBP <- N97.LBP[-c(1), ] #final proportion, se & 95% ci
#save
#write.csv(N97.LBP, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N97.LBP.csv")


#2. Spatial analysis, by region

#I)   Overall population
#i)   regional prevalence for the overall population
N97_LBP_reg <- svyby(formula = ~PAINLB,
                     by = ~REGION,
                     design = NHIS97_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N97_LBP_reg.c <- N97_LBP_reg %>%
  select(1, 3, 5) %>%
  setNames(c("Region", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N97_LBP_reg.ci <- confint(N97_LBP_reg) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N97_LBP_reg.ci <- N97_LBP_reg.ci[-c(1:4), ]
#join proportion and ci
N97.LBP.region <- bind_cols(N97_LBP_reg.c, N97_LBP_reg.ci) #final proportion, se & 95% ci
#save
#write.csv(N97.LBP.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N97.LBP.region.csv")

#Change the region column to "NAME" to match sf
names(N97.LBP.region)[names(N97.LBP.region) == "Region"] <- "NAME"

N97.LBP.joined <- regions %>%
  left_join(N97.LBP.region)

N97.LBP.joined$NAME <- as.factor(N97.LBP.joined$NAME)

#Plot
LBPregplot97 <- ggplot(data = N97.LBP.joined) +
  geom_sf(aes(fill = Proportion), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  scale_fill_viridis_c(name = "Prevalence (%)", option = "viridis", direction = -1, limits = c(25, 35), breaks = c(25, 30, 35)) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  ggtitle("Regional prevalence (%) of US residents with low back pain in 1997") +
  theme(plot.title = element_text(size = 10, hjust = 0.5))
ggsave("N97.LBP.regchor.png", width = 5, height = 5)


#ii)  logistic regression by region
N97.LBP.reg.glm <- svyglm(relevel(PAINLB, ref = "0")~ REGION + AGE_P,
                          family = quasibinomial,
                          design = NHIS97_DO)
summary(N97.LBP.reg.glm)
exp(cbind(OR=coef(N97.LBP.reg.glm), confint(N97.LBP.reg.glm)))


#II)  prevalence by region, by age

#i)  age-specific regional prevalance
N97_LBP_reg.age <- svyby(formula = ~PAINLB,
                         by = ~REGION + AGE_P,
                         design = NHIS97_DO,
                         FUN = svymean,
                         na.rm = TRUE)
N97_LBP_reg.age.c <- N97_LBP_reg.age %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Age", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Age = as.character(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N97_LBP_reg.age.ci <- confint(N97_LBP_reg.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N97_LBP_reg.age.ci <- N97_LBP_reg.age.ci[-c(1:44), ]
#join proportion and ci
N97.LBP.age.region <- bind_cols(N97_LBP_reg.age.c, N97_LBP_reg.age.ci) #final prevalence, se & 95% ci
#save
#write.csv(N97.LBP.age.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N97.LBP.region.age.csv")

#Geofacet - Age & Region
LBP.Age_Reg <- ggplot(N97.LBP.age.region, aes(Age, Proportion)) +
  geom_bar(colour = "black", stat = "identity", fill = "#6600CC") +
  coord_flip() +
  facet_geo(~Region,
            grid = USregions) +
  theme_bw() +
  theme(text = element_text(colour = "black", size = 20)) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 17)) +
  xlab("Age group (years)") +
  geom_errorbar(aes(ymin = Proportion-SE, ymax = Proportion+SE, width = 0.3)) +
  scale_y_continuous(name = "Prevalence (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 18)) +
  ggtitle("Regional prevalence (%) of US residents with low back pain by age in 1997") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N97.GF_AgeReg.LBP.png", width = 14, height = 9)


#ii) age-specific regional logistic regressions
#18 to 24
sub1824 <- subset(NHIS97_DO, AGE_P == "18 to 24")
N97.LBP.age1824.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub1824)
summary(N97.LBP.age1824.reg)
exp(cbind(OR = coef(N97.LBP.age1824.reg), confint(N97.LBP.age1824.reg)))

#25 to 29
sub2529 <- subset(NHIS97_DO, AGE_P == "25 to 29")
N97.LBP.age2529.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub2529)
summary(N97.LBP.age2529.reg)
exp(cbind(OR = coef(N97.LBP.age2529.reg), confint(N97.LBP.age2529.reg)))

#30 to 34
sub3034 <- subset(NHIS97_DO, AGE_P == "30 to 34")
N97.LB.age3034.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub3034)
summary(N97.LB.age3034.reg)
exp(cbind(OR = coef(N97.LB.age3034.reg), confint(N97.LB.age3034.reg)))

#35 to 39
sub3539 <- subset(NHIS97_DO, AGE_P == "35 to 39")
N97.LBP.age3539.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub3539)
summary(N97.LBP.age3539.reg)
exp(cbind(OR = coef(N97.LBP.age3539.reg), confint(N97.LBP.age3539.reg)))

#40 to 44
sub4044 <- subset(NHIS97_DO, AGE_P == "40 to 44")
N97.LBP.age4044.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub4044)
summary(N97.LBP.age4044.reg)
exp(cbind(OR = coef(N97.LBP.age4044.reg), confint(N97.LBP.age4044.reg)))

#45 to 49
sub4549 <- subset(NHIS97_DO, AGE_P == "45 to 49")
N97.LBP.age4549.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub4549)
summary(N97.LBP.age4549.reg)
exp(cbind(OR = coef(N97.LBP.age4549.reg), confint(N97.LBP.age4549.reg)))

#50 to 54
sub5054 <- subset(NHIS97_DO, AGE_P == "50 to 54")
N97.LBP.age5054.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub5054)
summary(N97.LBP.age5054.reg)
exp(cbind(OR = coef(N97.LBP.age5054.reg), confint(N97.LBP.age5054.reg)))

#55 to 59
sub5559 <- subset(NHIS97_DO, AGE_P == "55 to 59")
N97.LBP.age5559.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub5559)
summary(N97.LBP.age5559.reg)
exp(cbind(OR = coef(N97.LBP.age5559.reg), confint(N97.LBP.age5559.reg)))

#60 to 64
sub6064 <- subset(NHIS97_DO, AGE_P == "60 to 64")
N97.LBP.age6064.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub6064)
summary(N97.LBP.age6064.reg)
exp(cbind(OR = coef(N97.LBP.age6064.reg), confint(N97.LBP.age6064.reg)))

#65 to 69
sub6569 <- subset(NHIS97_DO, AGE_P == "65 to 69")
N97.LBP.age6569.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                              family = quasibinomial,
                              design = sub6569)
summary(N97.LBP.age6569.reg)
exp(cbind(OR = coef(N97.LBP.age6569.reg), confint(N97.LBP.age6569.reg)))

#70 and above
sub70 <- subset(NHIS97_DO, AGE_P == "70 and above")
N97.LBP.age70.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + SEX,
                            family = quasibinomial,
                            design = sub70)
summary(N97.LBP.age70.reg)
exp(cbind(OR = coef(N97.LBP.age70.reg), confint(N97.LBP.age70.reg)))

#III) prevalence by region, by sex

#i)  sex-specific regional prevalence
N97_LBP_reg.sex <- svyby(formula = ~PAINLB,
                         by = ~REGION + SEX,
                         design = NHIS97_DO,
                         FUN = svymean,
                         na.rm = TRUE)
N97_LBP_reg.sex.c <- N97_LBP_reg.sex %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Sex", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N97_LBP_reg.sex.ci <- confint(N97_LBP_reg.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for LBP = 0
N97_LBP_reg.sex.ci <- N97_LBP_reg.sex.ci[-c(1:8), ]
#join proportion and ci
N97.LBP.sex.region <- bind_cols(N97_LBP_reg.sex.c, N97_LBP_reg.sex.ci) #final prevalence, se & 95% ci
#save
#write.csv(N97.LBP.sex.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N97.LBP.region.sex.csv")

#Geofacet - Sex & Region
LBP.Sex_Reg97 <- ggplot(N97.LBP.sex.region, aes(Sex, Proportion)) +
  geom_bar(colour = "black", stat = "identity", fill = "#6600CC") +
  coord_flip() +
  facet_geo(~Region,
            grid = USregions) +
  theme_bw() +
  theme(text = element_text(colour = "black", size = 20)) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 17)) +
  xlab("Sex") +
  geom_errorbar(aes(ymin = Proportion-SE, ymax = Proportion+SE, width = 0.3)) +
  scale_y_continuous(name = "Prevalence (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 18)) +
  ggtitle("Regional prevalence (%) of US residents with low back pain by sex in 1997") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N97.GF_SexReg.LBP.png", width = 14, height = 9)


#ii) sex-specific regional logistic regressions
#males
submales <- subset(NHIS97_DO, SEX == "Male")
N97.LBP.sexmale.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + AGE_P,
                              family = quasibinomial,
                              design = submales)
summary(N97.LBP.sexmale.reg)
exp(cbind(OR = coef(N97.LBP.sexmale.reg), confint(N97.LBP.sexmale.reg)))

#females
subfemales <- subset(NHIS97_DO, SEX == "Female")
N97.LBP.sexfemale.reg <- svyglm(relevel(PAINLB, ref = "0")~REGION + AGE_P,
                                family = quasibinomial,
                                design = subfemales)
summary(N97.LBP.sexfemale.reg)
exp(cbind(OR = coef(N97.LBP.sexfemale.reg), confint(N97.LBP.sexfemale.reg)))


#3. Demographic trends

#I)  Age trends

#i)  Prevalence by age
N97_LBP_age <- svyby(formula = ~PAINLB,
                     by = ~AGE_P,
                     design = NHIS97_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N97_LBP_age.c <- N97_LBP_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N97_LBP_age_ci <- confint(N97_LBP_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove LBP=0 ci
N97_LBP_age_ci <- N97_LBP_age_ci[-c(1:11), ]
#join proportion and ci
N97_LBP.age <- bind_cols(N97_LBP_age.c, N97_LBP_age_ci) #final proportion, se & 95% ci
#Save
#write.csv(N97_LBP.age, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N97.LBP.age.csv")

#ii) Logistic regression by age
N97_LBP.age_glm <- svyglm(PAINLB~AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS97_DO)
summary(N97_LBP.age_glm)
exp(cbind(OR=coef(N97_LBP.age_glm), confint(N97_LBP.age_glm)))


#II)  Sex trends

#i)  Prevalence by sex
N97_LBP_sex <- svyby(formula = ~PAINLB,
                     by = ~SEX,
                     design = NHIS97_DO,
                     FUN = svymean,
                     na.rm = TRUE)
N97_LBP_sex.c <- N97_LBP_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N97_LBP_sex_ci <- confint(N97_LBP_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N97_LBP_sex_ci <- N97_LBP_sex_ci[-c(1:2), ]
#join proportion and ci
N97.LBP.sex <- bind_cols(N97_LBP_sex.c, N97_LBP_sex_ci) #final proportion, se & 95% ci
#save
#write.csv(N97.LBP.sex, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N97.LBP.sex.csv")

#ii) Logistic regression by sex
N97.LB.sex.glm.c <- svyglm(PAINLB~relevel(SEX, ref = "Male") + AGE_P,
                           family = quasibinomial,
                           design = NHIS97_DO)
summary(N97.LB.sex.glm.c)
exp(cbind(OR=coef(N97.LB.sex.glm.c), confint(N97.LB.sex.glm.c)))


#4. Lifestyle trends

#I)  BMI
#logistic regression
N97.BMI.LBP.glm <- svyglm(PAINLB ~ BMI + AGE_P + SEX,
                          family = quasibinomial,
                          design = NHIS97_DO)
summary(N97.BMI.LBP.glm)
exp(cbind(OR=coef(N97.BMI.LBP.glm), confint(N97.BMI.LBP.glm)))

#II) Physical activity
#logistic regression
N97.PA.LBP.glm <- svyglm(PAINLB ~ relevel(factor(PAC, ordered = FALSE), ref = "Physically active") + AGE_P + SEX,
                         family = quasibinomial,
                         design = NHIS97_DO)
summary(N97.PA.LBP.glm)
exp(cbind(OR = coef(N97.PA.LBP.glm), confint(N97.PA.LBP.glm)))


#______________________________________________________________________________________________________________________

#NHIS FIGURES

#1. Temporal figures for Chapter 3:

#make quick function for every nth year shown on x-axis
every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}


#Overall population annual prevalence

#Add years
N18.LBP$Year <- c("2018")
N17.LBP$Year <- c("2017")
N16.LBP$Year <- c("2016")
N15.LBP$Year <- c("2015")
N14.LBP$Year <- c("2014")
N13.LBP$Year <- c("2013")
N12.LBP$Year <- c("2012")
N11.LBP$Year <- c("2011")
N10.LBP$Year <- c("2010")
N09.LBP$Year <- c("2009")
N08.LBP$Year <- c("2008")
N07.LBP$Year <- c("2007")
N06.LBP$Year <- c("2006")
N05.LBP$Year <- c("2005")
N03.LBP$Year <- c("2003")
N02.LBP$Year <- c("2002")
N01.LBP$Year <- c("2001")
N00.LBP$Year <- c("2000")
N99.LBP$Year <- c("1999")
N98.LBP$Year <- c("1998")
N97.LBP$Year <- c("1997")


N.LBP <- Reduce(function(x, y) merge(x, y, all=TRUE), list(N97.LBP, N98.LBP, N99.LBP, N00.LBP, N01.LBP, N02.LBP, N03.LBP, N05.LBP, N06.LBP, N07.LBP, N08.LBP, N09.LBP, N10.LBP, N11.LBP, N12.LBP, N13.LBP, N14.LBP, N15.LBP, N16.LBP, N17.LBP, N18.LBP))

#Overall temporall plot
N.LBP.plot <- ggplot(data = N.LBP,
                    aes(x = Year, y = Proportion, group = 1)) +
  geom_line(colour = "#6600CC") +
  geom_point(colour = "#6600CC") + 
  geom_ribbon(aes(ymin = N.LBP$CI_Prop_low, ymax = N.LBP$CI_Prop_upp), alpha = 0.2, fill = "#6600CC") +
  scale_y_continuous(name = "Percentage (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  scale_x_discrete(breaks = every_nth(n = 2)) +
  theme_bw() +
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 18, angle = 90, hjust = 1)) +
  theme(axis.title.x = element_text(size = 21)) +
  theme(axis.title.y = element_text(size = 21)) +
  xlab("NHIS survey cycles (years)")
ggsave("N.LBP-time.png", width = 18, height = 11)


#Annual prevalence by age

#Add years
N18_LBP.age$Year <- c("2018", "2018", "2018", "2018", "2018", "2018", "2018", "2018", "2018", "2018", "2018")
N17_LBP.age$Year <- c("2017", "2017", "2017", "2017", "2017", "2017", "2017", "2017", "2017", "2017", "2017")
N16_LBP.age$Year <- c("2016", "2016", "2016", "2016", "2016", "2016", "2016", "2016", "2016", "2016", "2016")
N15_LBP.age$Year <- c("2015", "2015", "2015", "2015", "2015", "2015", "2015", "2015", "2015", "2015", "2015")
N14_LBP.age$Year <- c("2014", "2014", "2014", "2014", "2014", "2014", "2014", "2014", "2014", "2014", "2014")
N13_LBP.age$Year <- c("2013", "2013", "2013", "2013", "2013", "2013", "2013", "2013", "2013", "2013", "2013")
N12_LBP.age$Year <- c("2012", "2012", "2012", "2012", "2012", "2012", "2012", "2012", "2012", "2012", "2012")
N11_LBP.age$Year <- c("2011", "2011", "2011", "2011", "2011", "2011", "2011", "2011", "2011", "2011", "2011")
N10_LBP.age$Year <- c("2010", "2010", "2010", "2010", "2010", "2010", "2010", "2010", "2010", "2010", "2010")
N09_LBP.age$Year <- c("2009", "2009", "2009", "2009", "2009", "2009", "2009", "2009", "2009", "2009", "2009")
N08_LBP.age$Year <- c("2008", "2008", "2008", "2008", "2008", "2008", "2008", "2008", "2008", "2008", "2008")
N07_LBP.age$Year <- c("2007", "2007", "2007", "2007", "2007", "2007", "2007", "2007", "2007", "2007", "2007")
N06_LBP.age$Year <- c("2006", "2006", "2006", "2006", "2006", "2006", "2006", "2006", "2006", "2006", "2006")
N05_LBP.age$Year <- c("2005", "2005", "2005", "2005", "2005", "2005", "2005", "2005", "2005", "2005", "2005")
N03_LBP.age$Year <- c("2003", "2003", "2003", "2003", "2003", "2003", "2003", "2003", "2003", "2003", "2003")
N02_LBP.age$Year <- c("2002", "2002", "2002", "2002", "2002", "2002", "2002", "2002", "2002", "2002", "2002")
N01_LBP.age$Year <- c("2001", "2001", "2001", "2001", "2001", "2001", "2001", "2001", "2001", "2001", "2001")
N00_LBP.age$Year <- c("2000", "2000", "2000", "2000", "2000", "2000", "2000", "2000", "2000", "2000", "2000")
N99_LBP.age$Year <- c("1999", "1999", "1999", "1999", "1999", "1999", "1999", "1999", "1999", "1999", "1999")
N98_LBP.age$Year <- c("1998", "1998", "1998", "1998", "1998", "1998", "1998", "1998", "1998", "1998", "1998")
N97_LBP.age$Year <- c("1997", "1997", "1997", "1997", "1997", "1997", "1997", "1997", "1997", "1997", "1997")


#Merge
N.LBP.age <- Reduce(function(x, y) merge(x, y, all=TRUE), list(N18_LBP.age, N17_LBP.age, N16_LBP.age, N15_LBP.age, N14_LBP.age, N13_LBP.age, N12_LBP.age, N11_LBP.age, N10_LBP.age, N09_LBP.age, N08_LBP.age, N07_LBP.age, N06_LBP.age, N05_LBP.age, N03_LBP.age, N02_LBP.age, N01_LBP.age, N00_LBP.age, N99_LBP.age, N98_LBP.age, N97_LBP.age))

#Age temporal plot
N.LBP.age.plot <- ggplot(data = N.LBP.age,
                        aes(x = Year, y = Proportion, group = Age)) +
  geom_line(colour = "#6600CC") +
  geom_point(colour = "#6600CC") +
  geom_ribbon(aes(ymin = N.LBP.age$CI_Prop_low, ymax = N.LBP.age$CI_Prop_upp), alpha = 0.2, fill = "#6600CC") +
  facet_wrap(facets = ~Age) +
  scale_y_continuous(name = "Percentage (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  scale_x_discrete(breaks = every_nth(n = 3)) +
  theme_bw() +
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 18, angle = 90, hjust = 1)) +
  theme(axis.title.x = element_text(size = 21)) +
  theme(axis.title.y = element_text(size = 21)) +
  xlab("NHIS survey cycles (years)") +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 18))
ggsave("N.LBP.time.age.png", width = 18, height = 12)


#Annual prevalence by sex

#Add years
N18.LBP.sex$Year <- c("2018", "2018")
N17.LBP.sex$Year <- c("2017", "2017")
N16.LBP.sex$Year <- c("2016", "2016")
N15.LBP.sex$Year <- c("2015", "2015")
N14.LBP.sex$Year <- c("2014", "2014")
N13.LBP.sex$Year <- c("2013", "2013")
N12.LBP.sex$Year <- c("2012", "2012")
N11.LBP.sex$Year <- c("2011", "2011")
N10.LBP.sex$Year <- c("2010", "2010")
N09.LBP.sex$Year <- c("2009", "2009")
N08.LBP.sex$Year <- c("2008", "2008")
N07.LBP.sex$Year <- c("2007", "2007")
N06.LBP.sex$Year <- c("2006", "2006")
N05.LBP.sex$Year <- c("2005", "2005")
N03.LBP.sex$Year <- c("2003", "2003")
N02.LBP.sex$Year <- c("2002", "2002")
N01.LBP.sex$Year <- c("2001", "2001")
N00.LBP.sex$Year <- c("2000", "2000")
N99.LBP.sex$Year <- c("1999", "1999")
N98.LBP.sex$Year <- c("1998", "1998")
N97.LBP.sex$Year <- c("1997", "1997")

#Merge
N.LBP.sex <- Reduce(function(x, y) merge(x, y, all=TRUE), list(N18.LBP.sex, N17.LBP.sex, N16.LBP.sex, N15.LBP.sex, N14.LBP.sex, N13.LBP.sex, N12.LBP.sex, N10.LBP.sex, N09.LBP.sex, N08.LBP.sex, N07.LBP.sex, N06.LBP.sex, N05.LBP.sex, N03.LBP.sex, N02.LBP.sex, N01.LBP.sex, N00.LBP.sex, N99.LBP.sex, N98.LBP.sex, N97.LBP.sex))


#Sex temporal plot
N.LBP.sex.plot <- ggplot(data = N.LBP.sex,
                        aes(x = Year, y = Proportion, group = Sex)) +
  geom_line(colour = "#6600CC") +
  geom_point(colour = "#6600CC") +
  geom_ribbon(aes(ymin = N.LBP.sex$CI_Prop_low, ymax = N.LBP.sex$CI_Prop_upp), alpha = 0.2, fill = "#6600CC") +
  facet_wrap(facets = ~Sex) +
  scale_y_continuous(name = "Percentage (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  scale_x_discrete(breaks = every_nth(n = 2)) +
  theme_bw() +
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 18, angle = 90, hjust = 1)) +
  theme(axis.title.x = element_text(size = 21)) +
  theme(axis.title.y = element_text(size = 21)) +
  xlab("NHIS survey cycles (years)") +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 18))
ggsave("N.LBP.time.sex.png", width = 18, height = 12)


#Annual prevalence by employment

#Add years
N18.LBP.emp$Year <- c("2018", "2018")
N17.LBP.emp$Year <- c("2017", "2017")
N16.LBP.emp$Year <- c("2016", "2016")
N15.LBP.emp$Year <- c("2015", "2015")
N14.LBP.emp$Year <- c("2014", "2014")
N13.LBP.emp$Year <- c("2013", "2013")
N12.LBP.emp$Year <- c("2012", "2012")
N11.LBP.emp$Year <- c("2011", "2011")
N10.LBP.emp$Year <- c("2010", "2010")
N09.LBP.emp$Year <- c("2009", "2009")
N08.LBP.emp$Year <- c("2008", "2008")
N07.LBP.emp$Year <- c("2007", "2007")
N06.LBP.emp$Year <- c("2006", "2006")
N05.LBP.emp$Year <- c("2005", "2005")
N03.LBP.emp$Year <- c("2003", "2003")
N02.LBP.emp$Year <- c("2002", "2002")

#Merge
N.LBP.emp <- Reduce(function(x, y) merge(x, y, all=TRUE), list(N18.LBP.emp, N17.LBP.emp, N16.LBP.emp, N15.LBP.emp, N14.LBP.emp, N13.LBP.emp, N12.LBP.emp, N10.LBP.emp, N09.LBP.emp, N08.LBP.emp, N07.LBP.emp, N06.LBP.emp, N05.LBP.emp, N03.LBP.emp, N02.LBP.emp))


#emp temporal plot
N.LBP.emp.plot <- ggplot(data = N.LBP.emp,
                        aes(x = Year, y = Proportion, group = Employment)) +
  geom_line(colour = "#6600CC") +
  geom_point(colour = "#6600CC") +
  geom_ribbon(aes(ymin = N.LBP.emp$CI_Prop_low, ymax = N.LBP.emp$CI_Prop_upp), alpha = 0.2, fill = "#6600CC") +
  facet_wrap(facets = ~Employment) +
  scale_y_continuous(name = "Percentage (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  scale_x_discrete(breaks = every_nth(n = 2)) +
  theme_bw() +
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 18, angle = 90, hjust = 1)) +
  theme(axis.title.x = element_text(size = 21)) +
  theme(axis.title.y = element_text(size = 21)) +
  xlab("NHIS survey cycles (years)") +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 18))
ggsave("N.LBP.time.emp.png", width = 18, height = 12)


#2. Demographic figures for Chapter 5:


#Age-specific trends
N_LBP.age_plot <- ggplot(data = N.LBP.age,
                        aes(x = Age, y = Proportion)) +
  geom_bar(stat = "identity", fill = "#6600CC") +
  geom_errorbar(aes(ymin = Proportion-SE, ymax = Proportion+SE), colour = "black", size = 0.4, width = 0.3, position = position_dodge(0.9)) +
  theme_bw() +
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 15, angle = 90, hjust = 1)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  xlab("Age group (years)") +
  facet_wrap(facets = ~Year) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 16)) +
  scale_y_continuous(name = "Percentage (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100))
ggsave("N.LBP.demage.png", width = 18, height = 12)


#Sex-specific trends

N_LBP.sex_plot <- ggplot(data = N.LBP.sex,
                        aes(x = Sex, y = Proportion)) +
  geom_bar(stat = "identity", fill = "#6600CC") +
  geom_errorbar(aes(ymin = Proportion-SE, ymax = Proportion+SE), colour = "black", size = 0.4, width = 0.3, position = position_dodge(0.9)) +
  theme_bw() +
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 15, angle = 90, hjust = 1)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  xlab("Sex") +
  facet_wrap(facets = ~Year) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 16)) +
  scale_y_continuous(name = "Percentage (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100))
ggsave("N.LBP.demsex.png", width = 18, height = 11)

#Chapter 4 (spatial trends) images are done annually within analyses.
#No figures for chapter 6 (lifestyle trends)

# ------- END NHIS LOW BACK PAIN ANALYSIS HERE ------- #

