#Anne Elizabeth Ioannides
#For the qualification of: PhD - Physiology, University of the Witwatersrand, South Africa

#National Health Interview Survey (NHIS) - Joint symptoms analysis

#The data cleaning methods are exactly the same as those used for low back pain, resulting in identical design objects used for both NHIS-based musculoskeletal pain analyses. One copy of this design object is available on Github

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
library(R.utils)


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



#Pull in clean design objects from Dropbox (generated from the cleaning script)
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
N18_JS <- svymean(~factor(JNTSYMP), 
                  NHIS18_DO, 
                  na.rm = TRUE)

N18_JS.c <- N18_JS %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N18_JS_ci <- confint(N18_JS) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
N18.JS <- bind_cols(N18_JS.c, N18_JS_ci)
#remove js = 0
N18.JS <- N18.JS[-c(1), ] #final proportion, se & 95%
#save
##write.csv(N18.JS, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N18.JS.csv")

#2. Spatial analysis, by region

#I)   Overall population

#i)   regional prevalence for the overall population
N18_JS_reg <- svyby(formula = ~JNTSYMP,
                    by = ~REGION,
                    design = NHIS18_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N18_JS_reg.c <- N18_JS_reg %>%
  select(1, 3, 5) %>%
  setNames(c("Region", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N18_JS_reg.ci <- confint(N18_JS_reg) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for js = 0
N18_JS_reg.ci <- N18_JS_reg.ci[-c(1:4), ]
#join proportion and ci
N18.JS.region <- bind_cols(N18_JS_reg.c, N18_JS_reg.ci) #final proportion, se & 95% ci
#save
##write.csv(N18.JS.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N18.JS.region.csv")

#Create overall regional prevalence

#Change the region column to "NAME" to match sf
names(N18.JS.region)[names(N18.JS.region) == "Region"] <- "NAME"

N18.JS.joined <- regions %>%
  left_join(N18.JS.region)

N18.JS.joined$NAME <- as.factor(N18.JS.joined$NAME)

#Plot
JSregplot <- ggplot(data = N18.JS.joined) +
  geom_sf(aes(fill = Proportion), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  scale_fill_viridis_c(name = "Prevalence (%)", option = "viridis", direction = -1, limits = c(25, 40), breaks = c(25, 30, 35, 40)) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  ggtitle("Regional prevalence (%) of US residents with joint symptoms in 2018") +
 theme(plot.title = element_text(size = 10, hjust = 0.5))
ggsave("N18.JSregchor.png", width = 5, height = 5)


#ii)  logistic regression by region
N18.JS.reg.glm <- svyglm(relevel(JNTSYMP, ref = "0")~ REGION + AGE_P,
                         family = quasibinomial,
                         design = NHIS18_DO)
summary(N18.JS.reg.glm)
exp(cbind(OR=coef(N18.JS.reg.glm), confint(N18.JS.reg.glm)))

#II)  prevalence by region, by age

#i)  age-specific regional prevalance
N18_JS_reg.age <- svyby(formula = ~JNTSYMP,
                        by = ~REGION + AGE_P,
                        design = NHIS18_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N18_JS_reg.age.c <- N18_JS_reg.age %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Age", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Age = as.character(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N18_JS_reg.age.ci <- confint(N18_JS_reg.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for js = 0
N18_JS_reg.age.ci <- N18_JS_reg.age.ci[-c(1:44), ]
#join proportion and ci
N18.JS.age.region <- bind_cols(N18_JS_reg.age.c, N18_JS_reg.age.ci) #final proportion, se & 95% ci
#save
##write.csv(N18.JS.age.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N18.JS.region.age.csv")

#Geofacet plot - Age & Region
JS.Age_Reg <- ggplot(N18.JS.age.region, aes(Age, Proportion)) +
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
  ggtitle("Regional prevalence (%) of US residents with joint symptoms by age group in 2018") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N18.GF_AgeReg.JS.png", width = 14, height = 9)


#ii) age-specific regional logistic regressions
#18 to 24
sub1824 <- subset(NHIS18_DO, AGE_P == "18 to 24")
N18.JS.age1824.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub1824)
summary(N18.JS.age1824.reg)
exp(cbind(OR = coef(N18.JS.age1824.reg), confint(N18.JS.age1824.reg)))

#25 to 29
sub2529 <- subset(NHIS18_DO, AGE_P == "25 to 29")
N18.JS.age2529.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub2529)
summary(N18.JS.age2529.reg)
exp(cbind(OR = coef(N18.JS.age2529.reg), confint(N18.JS.age2529.reg)))

#30 to 34
sub3034 <- subset(NHIS18_DO, AGE_P == "30 to 34")
N18.JS.age3034.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub3034)
summary(N18.JS.age3034.reg)
exp(cbind(OR = coef(N18.JS.age3034.reg), confint(N18.JS.age3034.reg)))

#35 to 39
sub3539 <- subset(NHIS18_DO, AGE_P == "35 to 39")
N18.JS.age3539.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub3539)
summary(N18.JS.age3539.reg)
exp(cbind(OR = coef(N18.JS.age3539.reg), confint(N18.JS.age3539.reg)))

#40 to 44
sub4044 <- subset(NHIS18_DO, AGE_P == "40 to 44")
N18.JS.age4044.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub4044)
summary(N18.JS.age4044.reg)
exp(cbind(OR = coef(N18.JS.age4044.reg), confint(N18.JS.age4044.reg)))

#45 to 49
sub4549 <- subset(NHIS18_DO, AGE_P == "45 to 49")
N18.JS.age4549.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub4549)
summary(N18.JS.age4549.reg)
exp(cbind(OR = coef(N18.JS.age4549.reg), confint(N18.JS.age4549.reg)))

#50 to 54
sub5054 <- subset(NHIS18_DO, AGE_P =="50 to 54")
N18.JS.age5054.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub5054)
summary(N18.JS.age5054.reg)
exp(cbind(OR = coef(N18.JS.age5054.reg), confint(N18.JS.age5054.reg)))

#55 to 59
sub5559 <- subset(NHIS18_DO, AGE_P == "55 to 59")
N18.JS.age5559.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub5559)
summary(N18.JS.age5559.reg)
exp(cbind(OR = coef(N18.JS.age5559.reg), confint(N18.JS.age5559.reg)))

#60 to 64
sub6064 <- subset(NHIS18_DO, AGE_P == "60 to 64")
N18.JS.age6064.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub6064)
summary(N18.JS.age6064.reg)
exp(cbind(OR = coef(N18.JS.age6064.reg), confint(N18.JS.age6064.reg)))

#65 to 69
sub6569 <- subset(NHIS18_DO, AGE_P == "65 to 69")
N18.JS.age6569.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub6569)
summary(N18.JS.age6569.reg)
exp(cbind(OR = coef(N18.JS.age6569.reg), confint(N18.JS.age6569.reg)))

#70 and above
sub70 <- subset(NHIS18_DO, AGE_P == "70 and above")
N18.JS.age70.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                           family = quasibinomial,
                           design = sub70)
summary(N18.JS.age70.reg)
exp(cbind(OR = coef(N18.JS.age70.reg), confint(N18.JS.age70.reg)))

#III) prevalence by region, by sex

#i)  sex-specific regional prevalence
N18_JS_reg.sex <- svyby(formula = ~JNTSYMP,
                        by = ~REGION + SEX,
                        design = NHIS18_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N18_JS_reg.sex.c <- N18_JS_reg.sex %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Sex", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N18_JS_reg.sex.ci <- confint(N18_JS_reg.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for js = 0
N18_JS_reg.sex.ci <- N18_JS_reg.sex.ci[-c(1:8), ]
#join proportion and ci
N18.JS.sex.region <- bind_cols(N18_JS_reg.sex.c, N18_JS_reg.sex.ci) #final proprtion, se & 95% ci
#save
##write.csv(N18.JS.sex.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N18.JS.region.sex.csv")

#Geofacet - Sex & Region
JS.Sex_Reg18 <- ggplot(N18.JS.sex.region, aes(Sex, Proportion)) +
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
  ggtitle("Regional prevalence (%) of US residents with joint symptoms by sex in 2018") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N18.GF_SexReg.JS.png", width = 14, height = 9)


#ii) sex-specific regional logistic regressions
#males
submales <- subset(NHIS18_DO, SEX == "Male")
N18.JS.sexmale.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + AGE_P,
                             family = quasibinomial,
                             design = submales)
summary(N18.JS.sexmale.reg)
exp(cbind(OR = coef(N18.JS.sexmale.reg), confint(N18.JS.sexmale.reg)))

#females
subfemales <- subset(NHIS18_DO, SEX == "Female")
N18.JS.sexfemale.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + AGE_P,
                               family = quasibinomial,
                               design = subfemales)
summary(N18.JS.sexfemale.reg)
exp(cbind(OR = coef(N18.JS.sexfemale.reg), confint(N18.JS.sexfemale.reg)))


#3. Demographic trends

#I)  Age trends

#i)  Prevalence by age
N18_JS_age <- svyby(formula = ~JNTSYMP,
                    by = ~AGE_P,
                    design = NHIS18_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N18_JS_age.c <- N18_JS_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N18_JS_age_ci <- confint(N18_JS_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove js=0 ci
N18_JS_age_ci <- N18_JS_age_ci[-c(1:11), ]
#join proportion and ci
N18_JS.age <- bind_cols(N18_JS_age.c, N18_JS_age_ci) #final proportion, se & 95% ci
#Save
##write.csv(N18_JS.age, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N18.JS.age.csv")


#ii) Logistic regression by age
N18_JS.age_glm <- svyglm(JNTSYMP~AGE_P + SEX,
                         family = quasibinomial,
                         design = NHIS18_DO)
summary(N18_JS.age_glm)
exp(cbind(OR=coef(N18_JS.age_glm), confint(N18_JS.age_glm)))



#II)  Sex trends

#i)  Prevalence by sex
N18_JS_sex <- svyby(formula = ~JNTSYMP,
                    by = ~SEX,
                    design = NHIS18_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N18_JS_sex.c <- N18_JS_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N18_JS_sex_ci <- confint(N18_JS_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N18_JS_sex_ci <- N18_JS_sex_ci[-c(1:2), ]
#join proportion and ci
N18.JS.sex <- bind_cols(N18_JS_sex.c, N18_JS_sex_ci)
#save
##write.csv(N18.JS.sex, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N18.JS.sex.csv")


#ii) Logistic regression by sex
N18.JS.sex.glm.c <- svyglm(JNTSYMP~relevel(SEX, ref = "Male") + AGE_P,
                           family = quasibinomial,
                           design = NHIS18_DO)
summary(N18.JS.sex.glm.c)
exp(cbind(OR=coef(N18.JS.sex.glm.c), confint(N18.JS.sex.glm.c)))


#Prevalence of various age-sex groups and their joint pain locations

#1. Right shoulder
N18_JS.rtshoulder <- svyby(formula = ~JMTHP1,
                           by = ~AGE_P + SEX,
                           design = NHIS18_DO,
                           FUN = svymean,
                           na.rm = TRUE)
N18_JS.rtshoulder.c <- N18_JS.rtshoulder %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.RtShoulder")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N18_JS.rtshoulder.ci <- confint(N18_JS.rtshoulder) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N18_JS.rtshoulder.ci <- N18_JS.rtshoulder.ci[-c(23:44), ]
#join proportion and ci
N18.JS.rtshoulder <- bind_cols(N18_JS.rtshoulder.c, N18_JS.rtshoulder.ci)
#save
##write.csv(N18.JS.rtshoulder, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N18.rtshoulder.csv")

#2. Left Shoulder
N18_JS.ltshoulder <- svyby(formula = ~JMTHP2,
                           by = ~AGE_P + SEX,
                           design = NHIS18_DO,
                           FUN = svymean,
                           na.rm = TRUE)
N18_JS.ltshoulder.c <- N18_JS.ltshoulder %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltshoulder")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N18_JS.ltshoulder.ci <- confint(N18_JS.ltshoulder) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N18_JS.ltshoulder.ci <- N18_JS.ltshoulder.ci[-c(23:44), ]
#join proportion and ci
N18.JS.ltshoulder <- bind_cols(N18_JS.ltshoulder.c, N18_JS.ltshoulder.ci)
#save
##write.csv(N18.JS.ltshoulder, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N18.ltshoulder.csv")


#3. Right elbow
N18_JS.rtelbow <- svyby(formula = ~JMTHP3,
                        by = ~AGE_P + SEX,
                        design = NHIS18_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N18_JS.rtelbow.c <- N18_JS.rtelbow %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtelbow")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N18_JS.rtelbow.ci <- confint(N18_JS.rtelbow) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N18_JS.rtelbow.ci <- N18_JS.rtelbow.ci[-c(23:44), ]
#join proportion and ci
N18.JS.rtelbow <- bind_cols(N18_JS.rtelbow.c, N18_JS.rtelbow.ci)
#save
##write.csv(N18.JS.rtelbow, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N18.rtelbow.csv")

#4. Lt elbow
N18_JS.ltelbow <- svyby(formula = ~JMTHP4,
                        by = ~AGE_P + SEX,
                        design = NHIS18_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N18_JS.ltelbow.c <- N18_JS.ltelbow %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltelbow")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N18_JS.ltelbow.ci <- confint(N18_JS.ltelbow) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N18_JS.ltelbow.ci <- N18_JS.ltelbow.ci[-c(23:44), ]
#join proportion and ci
N18.JS.ltelbow <- bind_cols(N18_JS.ltelbow.c, N18_JS.ltelbow.ci)
#save
##write.csv(N18.JS.ltelbow, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N18.ltelbow.csv")

#5. Right hip
N18_JS.rthip <- svyby(formula = ~JMTHP5,
                      by = ~AGE_P + SEX,
                      design = NHIS18_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N18_JS.rthip.c <- N18_JS.rthip %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rthip")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N18_JS.rthip.ci <- confint(N18_JS.rthip) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N18_JS.rthip.ci <- N18_JS.rthip.ci[-c(23:44), ]
#join proportion and ci
N18.JS.rthip <- bind_cols(N18_JS.rthip.c, N18_JS.rthip.ci)
#save
##write.csv(N18.JS.rthip, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N18.rthip.csv")

#6. Left hip
N18_JS.lthip <- svyby(formula = ~JMTHP6,
                      by = ~AGE_P + SEX,
                      design = NHIS18_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N18_JS.lthip.c <- N18_JS.lthip %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.lthip")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N18_JS.lthip.ci <- confint(N18_JS.lthip) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N18_JS.lthip.ci <- N18_JS.lthip.ci[-c(23:44), ]
#join proportion and ci
N18.JS.lthip <- bind_cols(N18_JS.lthip.c, N18_JS.lthip.ci)
#save
##write.csv(N18.JS.lthip, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N18.lthip.csv")

#7. Right wrist
N18_JS.rtwrist <- svyby(formula = ~JMTHP7,
                        by = ~AGE_P + SEX,
                        design = NHIS18_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N18_JS.rtwrist.c <- N18_JS.rtwrist %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtwrist")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N18_JS.rtwrist.ci <- confint(N18_JS.rtwrist) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N18_JS.rtwrist.ci <- N18_JS.rtwrist.ci[-c(23:44), ]
#join proportion and ci
N18.JS.rtwrist <- bind_cols(N18_JS.rtwrist.c, N18_JS.rtwrist.ci)
#save
##write.csv(N18.JS.rtwrist, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N18.rtwrist.csv")

#8. Left wrist
N18_JS.ltwrist <- svyby(formula = ~JMTHP8,
                        by = ~AGE_P + SEX,
                        design = NHIS18_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N18_JS.ltwrist.c <- N18_JS.ltwrist %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltwrist")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N18_JS.ltwrist.ci <- confint(N18_JS.ltwrist) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N18_JS.ltwrist.ci <- N18_JS.ltwrist.ci[-c(23:44), ]
#join proportion and ci
N18.JS.ltwrist <- bind_cols(N18_JS.ltwrist.c, N18_JS.ltwrist.ci)
#save
##write.csv(N18.JS.ltwrist, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N18.ltwrist.csv")

#9. Right knee
N18_JS.rtknee <- svyby(formula = ~JMTHP9,
                       by = ~AGE_P + SEX,
                       design = NHIS18_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N18_JS.rtknee.c <- N18_JS.rtknee %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtknee")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N18_JS.rtknee.ci <- confint(N18_JS.rtknee) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N18_JS.rtknee.ci <- N18_JS.rtknee.ci[-c(23:44), ]
#join proportion and ci
N18.JS.rtknee <- bind_cols(N18_JS.rtknee.c, N18_JS.rtknee.ci)
#save
##write.csv(N18.JS.rtknee, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N18.rtknee.csv")

#10. Left knee
N18_JS.ltknee <- svyby(formula = ~JMTHP10,
                       by = ~AGE_P + SEX,
                       design = NHIS18_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N18_JS.ltknee.c <- N18_JS.ltknee %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltknee")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N18_JS.ltknee.ci <- confint(N18_JS.ltknee) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N18_JS.ltknee.ci <- N18_JS.ltknee.ci[-c(23:44), ]
#join proportion and ci
N18.JS.ltknee <- bind_cols(N18_JS.ltknee.c, N18_JS.ltknee.ci)
#save
##write.csv(N18.JS.ltknee, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N18.ltknee.csv")

#11. Right ankle
N18_JS.rtankle <- svyby(formula = ~JMTHP11,
                        by = ~AGE_P + SEX,
                        design = NHIS18_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N18_JS.rtankle.c <- N18_JS.rtankle %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtankle")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N18_JS.rtankle.ci <- confint(N18_JS.rtankle) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N18_JS.rtankle.ci <- N18_JS.rtankle.ci[-c(23:44), ]
#join proportion and ci
N18.JS.rtankle <- bind_cols(N18_JS.rtankle.c, N18_JS.rtankle.ci)
#save
##write.csv(N18.JS.rtankle, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N18.rtankle.csv")

#12. Left ankle
N18_JS.ltankle <- svyby(formula = ~JMTHP12,
                        by = ~AGE_P + SEX,
                        design = NHIS18_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N18_JS.ltankle.c <- N18_JS.ltankle %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltankle")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N18_JS.ltankle.ci <- confint(N18_JS.ltankle) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N18_JS.ltankle.ci <- N18_JS.ltankle.ci[-c(23:44), ]
#join proportion and ci
N18.JS.ltankle <- bind_cols(N18_JS.ltankle.c, N18_JS.ltankle.ci)
#save
##write.csv(N18.JS.ltankle, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N18.ltankle.csv")

#13. Right toes
N18_JS.rttoes <- svyby(formula = ~JMTHP13,
                       by = ~AGE_P + SEX,
                       design = NHIS18_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N18_JS.rttoes.c <- N18_JS.rttoes %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rttoes")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N18_JS.rttoes.ci <- confint(N18_JS.rttoes) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N18_JS.rttoes.ci <- N18_JS.rttoes.ci[-c(23:44), ]
#join proportion and ci
N18.JS.rttoes <- bind_cols(N18_JS.rttoes.c, N18_JS.rttoes.ci)
#save
##write.csv(N18.JS.rttoes, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N18.rttoes.csv")

#14. Left toes
N18_JS.lttoes <- svyby(formula = ~JMTHP14,
                       by = ~AGE_P + SEX,
                       design = NHIS18_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N18_JS.lttoes.c <- N18_JS.lttoes %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.lttoes")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N18_JS.lttoes.ci <- confint(N18_JS.lttoes) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N18_JS.lttoes.ci <- N18_JS.lttoes.ci[-c(23:44), ]
#join proportion and ci
N18.JS.lttoes <- bind_cols(N18_JS.lttoes.c, N18_JS.lttoes.ci)
#save
##write.csv(N18.JS.lttoes, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N18.lttoes.csv")

#15. Right fingers/thumb
N18_JS.rtfingthumb <- svyby(formula = ~JMTHP15,
                            by = ~AGE_P + SEX,
                            design = NHIS18_DO,
                            FUN = svymean,
                            na.rm = TRUE)
N18_JS.rtfingthumb.c <- N18_JS.rtfingthumb %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtfingthumb")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N18_JS.rtfingthumb.ci <- confint(N18_JS.rtfingthumb) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N18_JS.rtfingthumb.ci <- N18_JS.rtfingthumb.ci[-c(23:44), ]
#join proportion and ci
N18.JS.rtfingthumb <- bind_cols(N18_JS.rtfingthumb.c, N18_JS.rtfingthumb.ci)
#save
##write.csv(N18.JS.rtfingthumb, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N18.rtfingthumb.csv")

#16. Left fingers/thumb
N18_JS.ltfingthumb <- svyby(formula = ~JMTHP16,
                            by = ~AGE_P + SEX,
                            design = NHIS18_DO,
                            FUN = svymean,
                            na.rm = TRUE)
N18_JS.ltfingthumb.c <- N18_JS.ltfingthumb %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltfingthumb")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N18_JS.ltfingthumb.ci <- confint(N18_JS.ltfingthumb) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N18_JS.ltfingthumb.ci <- N18_JS.ltfingthumb.ci[-c(23:44), ]
#join proportion and ci
N18.JS.ltfingthumb <- bind_cols(N18_JS.ltfingthumb.c, N18_JS.ltfingthumb.ci)
#save
##write.csv(N18.JS.ltfingthumb, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N18.ltfingthumb.csv")

#17. Another
N18_JS.another <- svyby(formula = ~JMTHP17,
                        by = ~AGE_P + SEX,
                        design = NHIS18_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N18_JS.another.c <- N18_JS.another %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.another")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N18_JS.another.ci <- confint(N18_JS.another) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N18_JS.another.ci <- N18_JS.another.ci[-c(23:44), ]
#join proportion and ci
N18.JS.another <- bind_cols(N18_JS.another.c, N18_JS.another.ci)
#save
##write.csv(N18.JS.another, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N18.another.csv")

N18.JSloc <- Reduce(function(x, y) merge(x, y, all = TRUE), list(N18_JS.rtshoulder.c, N18_JS.ltshoulder.c, N18_JS.rtelbow.c, N18_JS.ltelbow.c, N18_JS.rthip.c, N18_JS.lthip.c, N18_JS.rtwrist.c, N18_JS.ltwrist.c, N18_JS.rtknee.c, N18_JS.ltknee.c, N18_JS.rtankle.c, N18_JS.ltankle.c, N18_JS.rttoes.c, N18_JS.lttoes.c, N18_JS.rtfingthumb.c, N18_JS.ltfingthumb.c, N18_JS.another.c))

#Wide to long
N18.JSlocation <- gather(N18.JSloc, Location, Proportion, Proportion.RtShoulder:Proportion.another, factor_key = TRUE)
N18.JSlocation <- N18.JSlocation %>% 
  unite("Demographic", Age:Sex)
N18.JSlocation$Location <- as.character(N18.JSlocation$Location)
N18.JSlocation$Location[N18.JSlocation$Location == "Proportion.RtShoulder"] <- "Right shoulder"
N18.JSlocation$Location[N18.JSlocation$Location == "Proportion.ltshoulder"] <- "Left shoulder"
N18.JSlocation$Location[N18.JSlocation$Location == "Proportion.rtelbow"] <- "Right elbow"
N18.JSlocation$Location[N18.JSlocation$Location == "Proportion.ltelbow"] <- "Left elbow"
N18.JSlocation$Location[N18.JSlocation$Location == "Proportion.rthip"] <- "Right hip"
N18.JSlocation$Location[N18.JSlocation$Location == "Proportion.lthip"] <- "Left hip"
N18.JSlocation$Location[N18.JSlocation$Location == "Proportion.rtwrist"] <- "Right wrist"
N18.JSlocation$Location[N18.JSlocation$Location == "Proportion.ltwrist"] <- "Left wrist"
N18.JSlocation$Location[N18.JSlocation$Location == "Proportion.rtknee"] <- "Right knee"
N18.JSlocation$Location[N18.JSlocation$Location == "Proportion.ltknee"] <- "Left knee"
N18.JSlocation$Location[N18.JSlocation$Location == "Proportion.rtankle"] <- "Right ankle"
N18.JSlocation$Location[N18.JSlocation$Location == "Proportion.ltankle"] <- "Left ankle"
N18.JSlocation$Location[N18.JSlocation$Location == "Proportion.rttoes"] <- "Right toes"
N18.JSlocation$Location[N18.JSlocation$Location == "Proportion.lttoes"] <- "Left toes"
N18.JSlocation$Location[N18.JSlocation$Location == "Proportion.rtfingthumb"] <- "Right fingers or thumb"
N18.JSlocation$Location[N18.JSlocation$Location == "Proportion.ltfingthumb"] <- "Left fingers or thumb"
N18.JSlocation$Location[N18.JSlocation$Location == "Proportion.another"] <- "Another joint"

#Heatmap of joint locations by demographic groups
N18.JSlocmap <- ggplot(N18.JSlocation,
                       aes(x = Demographic, y = Location, fill = Proportion)) +
  geom_tile() + 
  scale_fill_viridis(option = "viridis", direction = -1, limits = c(0, 60), breaks = c(10, 20, 30, 40, 50, 60), discrete = FALSE) +
  theme_bw() + 
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.text.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  xlab("Age-sex profile") +
  theme(legend.position = "right",
        strip.background = element_blank()) +
  theme(legend.title = element_text(size = 26)) +
  theme(legend.text = element_text(size = 22)) +
  labs(fill = "Prevalence (%)") +
  guides(fill = guide_colourbar(barwidth = 3, barheight = 25)) +
  scale_x_discrete(labels = c("Females, 18 to 24", "Males, 18 to 24", "Females, 25 to 29", "Males, 25 to 29", "Females, 30 to 34", "Males, 30 to 34", "Females, 35 to 39", "Males, 35 to 39", "Females, 40 to 44", "Males, 40 to 44", "Females, 45 to 49", "Males, 45 to 49", "Females, 50 to 54", "Males, 50 to 54", "Females, 55 to 59", "Males, 55 to 59", "Females, 60 to 64", "Males, 60 to 64", "Females, 65 to 69", "Males, 65 to 69", "Females, 70 and above", "Males, 70 and above")) +
  scale_y_discrete(limits = c("Another joint", "Right toes", "Left toes", "Right ankle", "Left ankle", "Right knee", "Left knee", "Right hip", "Left hip", "Right shoulder", "Left shoulder", "Right elbow", "Left elbow", "Right wrist", "Left wrist", "Right fingers or thumb", "Left fingers or thumb"))
ggsave("N18.age-sex.png", width = 20, height = 13)



#III) Socioeconomic trends (employment status only)

#i)   Prevalence by employment status
N18_JS_emp <- svyby(formula = ~JNTSYMP,
                    by = ~DOINGLWA,
                    design = NHIS18_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N18_JS_emp.c <- N18_JS_emp %>%
  select(1, 3, 5) %>%
  setNames(c("Employment", "Proportion", "SE")) %>%
  mutate(Employment = as.factor(Employment)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N18_JS_emp_ci <- confint(N18_JS_emp) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for js = 0
N18_JS_emp_ci <- N18_JS_emp_ci[-c(1:2), ]
#join ci and proportion
N18.JS.emp <- bind_cols(N18_JS_emp.c, N18_JS_emp_ci) #final proportion, se & 95% ci
#save
##write.csv(N18.JS.emp, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N18.JS.emp.csv")

#ii)  Logistic regression by employment status
N18_JS.emp_glm <- svyglm(JNTSYMP~relevel(DOINGLWA, ref = "Working for pay at a job/business") + AGE_P + SEX,
                         family = quasibinomial,
                         design = NHIS18_DO)
summary(N18_JS.emp_glm)
exp(cbind(OR=coef(N18_JS.emp_glm), confint(N18_JS.emp_glm)))

#4. Lifestyle trends

#I)  BMI
#logistic regression
N18.BMI.glm <- svyglm(JNTSYMP ~ BMI + AGE_P + SEX,
                      family = quasibinomial,
                      design = NHIS18_DO)
summary(N18.BMI.glm)
exp(cbind(OR=coef(N18.BMI.glm), confint(N18.BMI.glm)))

#II) Physical activity
#logistic regression
N18.PA.JS.glm <- svyglm(JNTSYMP ~ relevel(factor(PAC, ordered = FALSE), ref = "Physically active") + AGE_P + SEX,
                        family = quasibinomial,
                        design = NHIS18_DO)
summary(N18.PA.JS.glm)
exp(cbind(OR = coef(N18.PA.JS.glm), confint(N18.PA.JS.glm)))


#____________________________________________________________________________________________________________

# == 2017 == #

#1. Overall prevalence
N17_JS <- svymean(~factor(JNTSYMP), 
                  NHIS17_DO, 
                  na.rm = TRUE)

N17_JS.c <- N17_JS %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N17_JS_ci <- confint(N17_JS) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
N17.JS <- bind_cols(N17_JS.c, N17_JS_ci)
#remove js = 0
N17.JS <- N17.JS[-c(1), ] #final proportion, se & 95%
#save
##write.csv(N17.JS, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N17.JS.csv")

#2. Spatial analysis, by region

#I)   Overall population

#i)   regional prevalence for the overall population
N17_JS_reg <- svyby(formula = ~JNTSYMP,
                    by = ~REGION,
                    design = NHIS17_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N17_JS_reg.c <- N17_JS_reg %>%
  select(1, 3, 5) %>%
  setNames(c("Region", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N17_JS_reg.ci <- confint(N17_JS_reg) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for js = 0
N17_JS_reg.ci <- N17_JS_reg.ci[-c(1:4), ]
#join proportion and ci
N17.JS.region <- bind_cols(N17_JS_reg.c, N17_JS_reg.ci) #final proportion, se & 95% ci
#save
##write.csv(N17.JS.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N17.JS.region.csv")

#Create overall regional prevalence

#Change the region column to "NAME" to match sf
names(N17.JS.region)[names(N17.JS.region) == "Region"] <- "NAME"

N17.JS.joined <- regions %>%
  left_join(N17.JS.region)

N17.JS.joined$NAME <- as.factor(N17.JS.joined$NAME)

#Plot
JSregplot <- ggplot(data = N17.JS.joined) +
  geom_sf(aes(fill = Proportion), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  scale_fill_viridis_c(name = "Prevalence (%)", option = "viridis", direction = -1, limits = c(25, 40), breaks = c(25, 30, 35, 40)) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  ggtitle("Regional prevalence (%) of US residents with joint symptoms in 2017") +
  theme(plot.title = element_text(size = 10, hjust = 0.5))
ggsave("N17.JSregchor.png", width = 5, height = 5)


#ii)  logistic regression by region
N17.JS.reg.glm <- svyglm(relevel(JNTSYMP, ref = "0")~ REGION + AGE_P,
                         family = quasibinomial,
                         design = NHIS17_DO)
summary(N17.JS.reg.glm)
exp(cbind(OR=coef(N17.JS.reg.glm), confint(N17.JS.reg.glm)))

#II)  prevalence by region, by age

#i)  age-specific regional prevalance
N17_JS_reg.age <- svyby(formula = ~JNTSYMP,
                        by = ~REGION + AGE_P,
                        design = NHIS17_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N17_JS_reg.age.c <- N17_JS_reg.age %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Age", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Age = as.character(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N17_JS_reg.age.ci <- confint(N17_JS_reg.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for js = 0
N17_JS_reg.age.ci <- N17_JS_reg.age.ci[-c(1:44), ]
#join proportion and ci
N17.JS.age.region <- bind_cols(N17_JS_reg.age.c, N17_JS_reg.age.ci) #final proportion, se & 95% ci
#save
##write.csv(N17.JS.age.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N17.JS.region.age.csv")

#Geofacet plot - Age & Region
JS.Age_Reg <- ggplot(N17.JS.age.region, aes(Age, Proportion)) +
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
  ggtitle("Regional prevalence (%) of US residents with joint symptoms by age group in 2017") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N17.GF_AgeReg.JS.png", width = 14, height = 9)


#ii) age-specific regional logistic regressions
#18 to 24
sub1824 <- subset(NHIS17_DO, AGE_P == "18 to 24")
N17.JS.age1824.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub1824)
summary(N17.JS.age1824.reg)
exp(cbind(OR = coef(N17.JS.age1824.reg), confint(N17.JS.age1824.reg)))

#25 to 29
sub2529 <- subset(NHIS17_DO, AGE_P == "25 to 29")
N17.JS.age2529.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub2529)
summary(N17.JS.age2529.reg)
exp(cbind(OR = coef(N17.JS.age2529.reg), confint(N17.JS.age2529.reg)))

#30 to 34
sub3034 <- subset(NHIS17_DO, AGE_P == "30 to 34")
N17.JS.age3034.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub3034)
summary(N17.JS.age3034.reg)
exp(cbind(OR = coef(N17.JS.age3034.reg), confint(N17.JS.age3034.reg)))

#35 to 39
sub3539 <- subset(NHIS17_DO, AGE_P == "35 to 39")
N17.JS.age3539.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub3539)
summary(N17.JS.age3539.reg)
exp(cbind(OR = coef(N17.JS.age3539.reg), confint(N17.JS.age3539.reg)))

#40 to 44
sub4044 <- subset(NHIS17_DO, AGE_P == "40 to 44")
N17.JS.age4044.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub4044)
summary(N17.JS.age4044.reg)
exp(cbind(OR = coef(N17.JS.age4044.reg), confint(N17.JS.age4044.reg)))

#45 to 49
sub4549 <- subset(NHIS17_DO, AGE_P == "45 to 49")
N17.JS.age4549.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub4549)
summary(N17.JS.age4549.reg)
exp(cbind(OR = coef(N17.JS.age4549.reg), confint(N17.JS.age4549.reg)))

#50 to 54
sub5054 <- subset(NHIS17_DO, AGE_P =="50 to 54")
N17.JS.age5054.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub5054)
summary(N17.JS.age5054.reg)
exp(cbind(OR = coef(N17.JS.age5054.reg), confint(N17.JS.age5054.reg)))

#55 to 59
sub5559 <- subset(NHIS17_DO, AGE_P == "55 to 59")
N17.JS.age5559.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub5559)
summary(N17.JS.age5559.reg)
exp(cbind(OR = coef(N17.JS.age5559.reg), confint(N17.JS.age5559.reg)))

#60 to 64
sub6064 <- subset(NHIS17_DO, AGE_P == "60 to 64")
N17.JS.age6064.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub6064)
summary(N17.JS.age6064.reg)
exp(cbind(OR = coef(N17.JS.age6064.reg), confint(N17.JS.age6064.reg)))

#65 to 69
sub6569 <- subset(NHIS17_DO, AGE_P == "65 to 69")
N17.JS.age6569.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub6569)
summary(N17.JS.age6569.reg)
exp(cbind(OR = coef(N17.JS.age6569.reg), confint(N17.JS.age6569.reg)))

#70 and above
sub70 <- subset(NHIS17_DO, AGE_P == "70 and above")
N17.JS.age70.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                           family = quasibinomial,
                           design = sub70)
summary(N17.JS.age70.reg)
exp(cbind(OR = coef(N17.JS.age70.reg), confint(N17.JS.age70.reg)))

#III) prevalence by region, by sex

#i)  sex-specific regional prevalence
N17_JS_reg.sex <- svyby(formula = ~JNTSYMP,
                        by = ~REGION + SEX,
                        design = NHIS17_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N17_JS_reg.sex.c <- N17_JS_reg.sex %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Sex", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N17_JS_reg.sex.ci <- confint(N17_JS_reg.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for js = 0
N17_JS_reg.sex.ci <- N17_JS_reg.sex.ci[-c(1:8), ]
#join proportion and ci
N17.JS.sex.region <- bind_cols(N17_JS_reg.sex.c, N17_JS_reg.sex.ci) #final proprtion, se & 95% ci
#save
##write.csv(N17.JS.sex.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N17.JS.region.sex.csv")

#Geofacet - Sex & Region
JS.Sex_Reg17 <- ggplot(N17.JS.sex.region, aes(Sex, Proportion)) +
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
  ggtitle("Regional prevalence (%) of US residents with joint symptoms by sex in 2017") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N17.GF_SexReg.JS.png", width = 14, height = 9)


#ii) sex-specific regional logistic regressions
#males
submales <- subset(NHIS17_DO, SEX == "Male")
N17.JS.sexmale.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + AGE_P,
                             family = quasibinomial,
                             design = submales)
summary(N17.JS.sexmale.reg)
exp(cbind(OR = coef(N17.JS.sexmale.reg), confint(N17.JS.sexmale.reg)))

#females
subfemales <- subset(NHIS17_DO, SEX == "Female")
N17.JS.sexfemale.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + AGE_P,
                               family = quasibinomial,
                               design = subfemales)
summary(N17.JS.sexfemale.reg)
exp(cbind(OR = coef(N17.JS.sexfemale.reg), confint(N17.JS.sexfemale.reg)))


#3. Demographic trends

#I)  Age trends

#i)  Prevalence by age
N17_JS_age <- svyby(formula = ~JNTSYMP,
                    by = ~AGE_P,
                    design = NHIS17_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N17_JS_age.c <- N17_JS_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N17_JS_age_ci <- confint(N17_JS_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove js=0 ci
N17_JS_age_ci <- N17_JS_age_ci[-c(1:11), ]
#join proportion and ci
N17_JS.age <- bind_cols(N17_JS_age.c, N17_JS_age_ci) #final proportion, se & 95% ci
#Save
##write.csv(N17_JS.age, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N17.JS.age.csv")


#ii) Logistic regression by age
N17_JS.age_glm <- svyglm(JNTSYMP~AGE_P + SEX,
                         family = quasibinomial,
                         design = NHIS17_DO)
summary(N17_JS.age_glm)
exp(cbind(OR=coef(N17_JS.age_glm), confint(N17_JS.age_glm)))



#II)  Sex trends

#i)  Prevalence by sex
N17_JS_sex <- svyby(formula = ~JNTSYMP,
                    by = ~SEX,
                    design = NHIS17_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N17_JS_sex.c <- N17_JS_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N17_JS_sex_ci <- confint(N17_JS_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N17_JS_sex_ci <- N17_JS_sex_ci[-c(1:2), ]
#join proportion and ci
N17.JS.sex <- bind_cols(N17_JS_sex.c, N17_JS_sex_ci)
#save
##write.csv(N17.JS.sex, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N17.JS.sex.csv")


#ii) Logistic regression by sex
N17.JS.sex.glm.c <- svyglm(JNTSYMP~relevel(SEX, ref = "Male") + AGE_P,
                           family = quasibinomial,
                           design = NHIS17_DO)
summary(N17.JS.sex.glm.c)
exp(cbind(OR=coef(N17.JS.sex.glm.c), confint(N17.JS.sex.glm.c)))


#Prevalence of various age-sex groups and their joint pain locations

#1. Right shoulder
N17_JS.rtshoulder <- svyby(formula = ~JMTHP1,
                           by = ~AGE_P + SEX,
                           design = NHIS17_DO,
                           FUN = svymean,
                           na.rm = TRUE)
N17_JS.rtshoulder.c <- N17_JS.rtshoulder %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.RtShoulder")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N17_JS.rtshoulder.ci <- confint(N17_JS.rtshoulder) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N17_JS.rtshoulder.ci <- N17_JS.rtshoulder.ci[-c(23:44), ]
#join proportion and ci
N17.JS.rtshoulder <- bind_cols(N17_JS.rtshoulder.c, N17_JS.rtshoulder.ci)
#save
##write.csv(N17.JS.rtshoulder, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N17.rtshoulder.csv")

#2. Left Shoulder
N17_JS.ltshoulder <- svyby(formula = ~JMTHP2,
                           by = ~AGE_P + SEX,
                           design = NHIS17_DO,
                           FUN = svymean,
                           na.rm = TRUE)
N17_JS.ltshoulder.c <- N17_JS.ltshoulder %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltshoulder")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N17_JS.ltshoulder.ci <- confint(N17_JS.ltshoulder) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N17_JS.ltshoulder.ci <- N17_JS.ltshoulder.ci[-c(23:44), ]
#join proportion and ci
N17.JS.ltshoulder <- bind_cols(N17_JS.ltshoulder.c, N17_JS.ltshoulder.ci)
#save
##write.csv(N17.JS.ltshoulder, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N17.ltshoulder.csv")


#3. Right elbow
N17_JS.rtelbow <- svyby(formula = ~JMTHP3,
                        by = ~AGE_P + SEX,
                        design = NHIS17_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N17_JS.rtelbow.c <- N17_JS.rtelbow %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtelbow")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N17_JS.rtelbow.ci <- confint(N17_JS.rtelbow) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N17_JS.rtelbow.ci <- N17_JS.rtelbow.ci[-c(23:44), ]
#join proportion and ci
N17.JS.rtelbow <- bind_cols(N17_JS.rtelbow.c, N17_JS.rtelbow.ci)
#save
##write.csv(N17.JS.rtelbow, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N17.rtelbow.csv")

#4. Lt elbow
N17_JS.ltelbow <- svyby(formula = ~JMTHP4,
                        by = ~AGE_P + SEX,
                        design = NHIS17_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N17_JS.ltelbow.c <- N17_JS.ltelbow %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltelbow")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N17_JS.ltelbow.ci <- confint(N17_JS.ltelbow) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N17_JS.ltelbow.ci <- N17_JS.ltelbow.ci[-c(23:44), ]
#join proportion and ci
N17.JS.ltelbow <- bind_cols(N17_JS.ltelbow.c, N17_JS.ltelbow.ci)
#save
##write.csv(N17.JS.ltelbow, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N17.ltelbow.csv")

#5. Right hip
N17_JS.rthip <- svyby(formula = ~JMTHP5,
                      by = ~AGE_P + SEX,
                      design = NHIS17_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N17_JS.rthip.c <- N17_JS.rthip %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rthip")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N17_JS.rthip.ci <- confint(N17_JS.rthip) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N17_JS.rthip.ci <- N17_JS.rthip.ci[-c(23:44), ]
#join proportion and ci
N17.JS.rthip <- bind_cols(N17_JS.rthip.c, N17_JS.rthip.ci)
#save
##write.csv(N17.JS.rthip, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N17.rthip.csv")

#6. Left hip
N17_JS.lthip <- svyby(formula = ~JMTHP6,
                      by = ~AGE_P + SEX,
                      design = NHIS17_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N17_JS.lthip.c <- N17_JS.lthip %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.lthip")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N17_JS.lthip.ci <- confint(N17_JS.lthip) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N17_JS.lthip.ci <- N17_JS.lthip.ci[-c(23:44), ]
#join proportion and ci
N17.JS.lthip <- bind_cols(N17_JS.lthip.c, N17_JS.lthip.ci)
#save
##write.csv(N17.JS.lthip, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N17.lthip.csv")

#7. Right wrist
N17_JS.rtwrist <- svyby(formula = ~JMTHP7,
                        by = ~AGE_P + SEX,
                        design = NHIS17_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N17_JS.rtwrist.c <- N17_JS.rtwrist %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtwrist")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N17_JS.rtwrist.ci <- confint(N17_JS.rtwrist) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N17_JS.rtwrist.ci <- N17_JS.rtwrist.ci[-c(23:44), ]
#join proportion and ci
N17.JS.rtwrist <- bind_cols(N17_JS.rtwrist.c, N17_JS.rtwrist.ci)
#save
##write.csv(N17.JS.rtwrist, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N17.rtwrist.csv")

#8. Left wrist
N17_JS.ltwrist <- svyby(formula = ~JMTHP8,
                        by = ~AGE_P + SEX,
                        design = NHIS17_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N17_JS.ltwrist.c <- N17_JS.ltwrist %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltwrist")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N17_JS.ltwrist.ci <- confint(N17_JS.ltwrist) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N17_JS.ltwrist.ci <- N17_JS.ltwrist.ci[-c(23:44), ]
#join proportion and ci
N17.JS.ltwrist <- bind_cols(N17_JS.ltwrist.c, N17_JS.ltwrist.ci)
#save
##write.csv(N17.JS.ltwrist, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N17.ltwrist.csv")

#9. Right knee
N17_JS.rtknee <- svyby(formula = ~JMTHP9,
                       by = ~AGE_P + SEX,
                       design = NHIS17_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N17_JS.rtknee.c <- N17_JS.rtknee %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtknee")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N17_JS.rtknee.ci <- confint(N17_JS.rtknee) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N17_JS.rtknee.ci <- N17_JS.rtknee.ci[-c(23:44), ]
#join proportion and ci
N17.JS.rtknee <- bind_cols(N17_JS.rtknee.c, N17_JS.rtknee.ci)
#save
##write.csv(N17.JS.rtknee, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N17.rtknee.csv")

#10. Left knee
N17_JS.ltknee <- svyby(formula = ~JMTHP10,
                       by = ~AGE_P + SEX,
                       design = NHIS17_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N17_JS.ltknee.c <- N17_JS.ltknee %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltknee")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N17_JS.ltknee.ci <- confint(N17_JS.ltknee) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N17_JS.ltknee.ci <- N17_JS.ltknee.ci[-c(23:44), ]
#join proportion and ci
N17.JS.ltknee <- bind_cols(N17_JS.ltknee.c, N17_JS.ltknee.ci)
#save
##write.csv(N17.JS.ltknee, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N17.ltknee.csv")

#11. Right ankle
N17_JS.rtankle <- svyby(formula = ~JMTHP11,
                        by = ~AGE_P + SEX,
                        design = NHIS17_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N17_JS.rtankle.c <- N17_JS.rtankle %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtankle")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N17_JS.rtankle.ci <- confint(N17_JS.rtankle) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N17_JS.rtankle.ci <- N17_JS.rtankle.ci[-c(23:44), ]
#join proportion and ci
N17.JS.rtankle <- bind_cols(N17_JS.rtankle.c, N17_JS.rtankle.ci)
#save
##write.csv(N17.JS.rtankle, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N17.rtankle.csv")

#12. Left ankle
N17_JS.ltankle <- svyby(formula = ~JMTHP12,
                        by = ~AGE_P + SEX,
                        design = NHIS17_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N17_JS.ltankle.c <- N17_JS.ltankle %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltankle")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N17_JS.ltankle.ci <- confint(N17_JS.ltankle) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N17_JS.ltankle.ci <- N17_JS.ltankle.ci[-c(23:44), ]
#join proportion and ci
N17.JS.ltankle <- bind_cols(N17_JS.ltankle.c, N17_JS.ltankle.ci)
#save
##write.csv(N17.JS.ltankle, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N17.ltankle.csv")

#13. Right toes
N17_JS.rttoes <- svyby(formula = ~JMTHP13,
                       by = ~AGE_P + SEX,
                       design = NHIS17_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N17_JS.rttoes.c <- N17_JS.rttoes %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rttoes")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N17_JS.rttoes.ci <- confint(N17_JS.rttoes) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N17_JS.rttoes.ci <- N17_JS.rttoes.ci[-c(23:44), ]
#join proportion and ci
N17.JS.rttoes <- bind_cols(N17_JS.rttoes.c, N17_JS.rttoes.ci)
#save
##write.csv(N17.JS.rttoes, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N17.rttoes.csv")

#14. Left toes
N17_JS.lttoes <- svyby(formula = ~JMTHP14,
                       by = ~AGE_P + SEX,
                       design = NHIS17_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N17_JS.lttoes.c <- N17_JS.lttoes %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.lttoes")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N17_JS.lttoes.ci <- confint(N17_JS.lttoes) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N17_JS.lttoes.ci <- N17_JS.lttoes.ci[-c(23:44), ]
#join proportion and ci
N17.JS.lttoes <- bind_cols(N17_JS.lttoes.c, N17_JS.lttoes.ci)
#save
##write.csv(N17.JS.lttoes, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N17.lttoes.csv")

#15. Right fingers/thumb
N17_JS.rtfingthumb <- svyby(formula = ~JMTHP15,
                            by = ~AGE_P + SEX,
                            design = NHIS17_DO,
                            FUN = svymean,
                            na.rm = TRUE)
N17_JS.rtfingthumb.c <- N17_JS.rtfingthumb %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtfingthumb")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N17_JS.rtfingthumb.ci <- confint(N17_JS.rtfingthumb) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N17_JS.rtfingthumb.ci <- N17_JS.rtfingthumb.ci[-c(23:44), ]
#join proportion and ci
N17.JS.rtfingthumb <- bind_cols(N17_JS.rtfingthumb.c, N17_JS.rtfingthumb.ci)
#save
##write.csv(N17.JS.rtfingthumb, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N17.rtfingthumb.csv")

#16. Left fingers/thumb
N17_JS.ltfingthumb <- svyby(formula = ~JMTHP16,
                            by = ~AGE_P + SEX,
                            design = NHIS17_DO,
                            FUN = svymean,
                            na.rm = TRUE)
N17_JS.ltfingthumb.c <- N17_JS.ltfingthumb %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltfingthumb")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N17_JS.ltfingthumb.ci <- confint(N17_JS.ltfingthumb) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N17_JS.ltfingthumb.ci <- N17_JS.ltfingthumb.ci[-c(23:44), ]
#join proportion and ci
N17.JS.ltfingthumb <- bind_cols(N17_JS.ltfingthumb.c, N17_JS.ltfingthumb.ci)
#save
##write.csv(N17.JS.ltfingthumb, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N17.ltfingthumb.csv")

#17. Another
N17_JS.another <- svyby(formula = ~JMTHP17,
                        by = ~AGE_P + SEX,
                        design = NHIS17_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N17_JS.another.c <- N17_JS.another %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.another")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N17_JS.another.ci <- confint(N17_JS.another) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N17_JS.another.ci <- N17_JS.another.ci[-c(23:44), ]
#join proportion and ci
N17.JS.another <- bind_cols(N17_JS.another.c, N17_JS.another.ci)
#save
##write.csv(N17.JS.another, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N17.another.csv")

N17.JSloc <- Reduce(function(x, y) merge(x, y, all = TRUE), list(N17_JS.rtshoulder.c, N17_JS.ltshoulder.c, N17_JS.rtelbow.c, N17_JS.ltelbow.c, N17_JS.rthip.c, N17_JS.lthip.c, N17_JS.rtwrist.c, N17_JS.ltwrist.c, N17_JS.rtknee.c, N17_JS.ltknee.c, N17_JS.rtankle.c, N17_JS.ltankle.c, N17_JS.rttoes.c, N17_JS.lttoes.c, N17_JS.rtfingthumb.c, N17_JS.ltfingthumb.c, N17_JS.another.c))

#Wide to long
N17.JSlocation <- gather(N17.JSloc, Location, Proportion, Proportion.RtShoulder:Proportion.another, factor_key = TRUE)
N17.JSlocation <- N17.JSlocation %>% 
  unite("Demographic", Age:Sex)
N17.JSlocation$Location <- as.character(N17.JSlocation$Location)
N17.JSlocation$Location[N17.JSlocation$Location == "Proportion.RtShoulder"] <- "Right shoulder"
N17.JSlocation$Location[N17.JSlocation$Location == "Proportion.ltshoulder"] <- "Left shoulder"
N17.JSlocation$Location[N17.JSlocation$Location == "Proportion.rtelbow"] <- "Right elbow"
N17.JSlocation$Location[N17.JSlocation$Location == "Proportion.ltelbow"] <- "Left elbow"
N17.JSlocation$Location[N17.JSlocation$Location == "Proportion.rthip"] <- "Right hip"
N17.JSlocation$Location[N17.JSlocation$Location == "Proportion.lthip"] <- "Left hip"
N17.JSlocation$Location[N17.JSlocation$Location == "Proportion.rtwrist"] <- "Right wrist"
N17.JSlocation$Location[N17.JSlocation$Location == "Proportion.ltwrist"] <- "Left wrist"
N17.JSlocation$Location[N17.JSlocation$Location == "Proportion.rtknee"] <- "Right knee"
N17.JSlocation$Location[N17.JSlocation$Location == "Proportion.ltknee"] <- "Left knee"
N17.JSlocation$Location[N17.JSlocation$Location == "Proportion.rtankle"] <- "Right ankle"
N17.JSlocation$Location[N17.JSlocation$Location == "Proportion.ltankle"] <- "Left ankle"
N17.JSlocation$Location[N17.JSlocation$Location == "Proportion.rttoes"] <- "Right toes"
N17.JSlocation$Location[N17.JSlocation$Location == "Proportion.lttoes"] <- "Left toes"
N17.JSlocation$Location[N17.JSlocation$Location == "Proportion.rtfingthumb"] <- "Right fingers or thumb"
N17.JSlocation$Location[N17.JSlocation$Location == "Proportion.ltfingthumb"] <- "Left fingers or thumb"
N17.JSlocation$Location[N17.JSlocation$Location == "Proportion.another"] <- "Another joint"

#Heatmap of joint locations by demographic groups
N17.JSlocmap <- ggplot(N17.JSlocation,
                       aes(x = Demographic, y = Location, fill = Proportion)) +
  geom_tile() + 
  scale_fill_viridis(option = "viridis", direction = -1, limits = c(0, 60), breaks = c(10, 20, 30, 40, 50, 60), discrete = FALSE) +
  theme_bw() + 
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.text.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  xlab("Age-sex profile") +
  theme(legend.position = "right",
        strip.background = element_blank()) +
  theme(legend.title = element_text(size = 26)) +
  theme(legend.text = element_text(size = 22)) +
  labs(fill = "Prevalence (%)") +
  guides(fill = guide_colourbar(barwidth = 3, barheight = 25)) +
  scale_x_discrete(labels = c("Females, 18 to 24", "Males, 18 to 24", "Females, 25 to 29", "Males, 25 to 29", "Females, 30 to 34", "Males, 30 to 34", "Females, 35 to 39", "Males, 35 to 39", "Females, 40 to 44", "Males, 40 to 44", "Females, 45 to 49", "Males, 45 to 49", "Females, 50 to 54", "Males, 50 to 54", "Females, 55 to 59", "Males, 55 to 59", "Females, 60 to 64", "Males, 60 to 64", "Females, 65 to 69", "Males, 65 to 69", "Females, 70 and above", "Males, 70 and above")) +
  scale_y_discrete(limits = c("Another joint", "Right toes", "Left toes", "Right ankle", "Left ankle", "Right knee", "Left knee", "Right hip", "Left hip", "Right shoulder", "Left shoulder", "Right elbow", "Left elbow", "Right wrist", "Left wrist", "Right fingers or thumb", "Left fingers or thumb"))
ggsave("N17.age-sex.png", width = 20, height = 13)



#III) Socioeconomic trends (employment status only)

#i)   Prevalence by employment status
N17_JS_emp <- svyby(formula = ~JNTSYMP,
                    by = ~DOINGLWA,
                    design = NHIS17_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N17_JS_emp.c <- N17_JS_emp %>%
  select(1, 3, 5) %>%
  setNames(c("Employment", "Proportion", "SE")) %>%
  mutate(Employment = as.factor(Employment)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N17_JS_emp_ci <- confint(N17_JS_emp) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for js = 0
N17_JS_emp_ci <- N17_JS_emp_ci[-c(1:2), ]
#join ci and proportion
N17.JS.emp <- bind_cols(N17_JS_emp.c, N17_JS_emp_ci) #final proportion, se & 95% ci
#save
##write.csv(N17.JS.emp, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N17.JS.emp.csv")

#ii)  Logistic regression by employment status
N17_JS.emp_glm <- svyglm(JNTSYMP~relevel(DOINGLWA, ref = "Working for pay at a job/business") + AGE_P + SEX,
                         family = quasibinomial,
                         design = NHIS17_DO)
summary(N17_JS.emp_glm)
exp(cbind(OR=coef(N17_JS.emp_glm), confint(N17_JS.emp_glm)))

#4. Lifestyle trends

#I)  BMI
#logistic regression
N17.BMI.glm <- svyglm(JNTSYMP ~ BMI + AGE_P + SEX,
                      family = quasibinomial,
                      design = NHIS17_DO)
summary(N17.BMI.glm)
exp(cbind(OR=coef(N17.BMI.glm), confint(N17.BMI.glm)))

#II) Physical activity
#logistic regression
N17.PA.JS.glm <- svyglm(JNTSYMP ~ relevel(factor(PAC, ordered = FALSE), ref = "Physically active") + AGE_P + SEX,
                        family = quasibinomial,
                        design = NHIS17_DO)
summary(N17.PA.JS.glm)
exp(cbind(OR = coef(N17.PA.JS.glm), confint(N17.PA.JS.glm)))


#______________________________________________________________________________________________________________

# == 2016 == #

#1. Overall prevalence
N16_JS <- svymean(~factor(JNTSYMP), 
                  NHIS16_DO, 
                  na.rm = TRUE)

N16_JS.c <- N16_JS %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N16_JS_ci <- confint(N16_JS) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
N16.JS <- bind_cols(N16_JS.c, N16_JS_ci)
#remove js = 0
N16.JS <- N16.JS[-c(1), ] #final proportion, se & 95%
#save
##write.csv(N16.JS, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N16.JS.csv")

#2. Spatial analysis, by region

#I)   Overall population

#i)   regional prevalence for the overall population
N16_JS_reg <- svyby(formula = ~JNTSYMP,
                    by = ~REGION,
                    design = NHIS16_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N16_JS_reg.c <- N16_JS_reg %>%
  select(1, 3, 5) %>%
  setNames(c("Region", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N16_JS_reg.ci <- confint(N16_JS_reg) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for js = 0
N16_JS_reg.ci <- N16_JS_reg.ci[-c(1:4), ]
#join proportion and ci
N16.JS.region <- bind_cols(N16_JS_reg.c, N16_JS_reg.ci) #final proportion, se & 95% ci
#save
##write.csv(N16.JS.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N16.JS.region.csv")

#Create overall regional prevalence

#Change the region column to "NAME" to match sf
names(N16.JS.region)[names(N16.JS.region) == "Region"] <- "NAME"

N16.JS.joined <- regions %>%
  left_join(N16.JS.region)

N16.JS.joined$NAME <- as.factor(N16.JS.joined$NAME)

#Plot
JSregplot <- ggplot(data = N16.JS.joined) +
  geom_sf(aes(fill = Proportion), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  scale_fill_viridis_c(name = "Prevalence (%)", option = "viridis", direction = -1, limits = c(25, 40), breaks = c(25, 30, 35, 40)) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  ggtitle("Regional prevalence (%) of US residents with joint symptoms in 2016") +
  theme(plot.title = element_text(size = 10, hjust = 0.5))
ggsave("N16.JSregchor.png", width = 5, height = 5)


#ii)  logistic regression by region
N16.JS.reg.glm <- svyglm(relevel(JNTSYMP, ref = "0")~ REGION + AGE_P,
                         family = quasibinomial,
                         design = NHIS16_DO)
summary(N16.JS.reg.glm)
exp(cbind(OR=coef(N16.JS.reg.glm), confint(N16.JS.reg.glm)))

#II)  prevalence by region, by age

#i)  age-specific regional prevalance
N16_JS_reg.age <- svyby(formula = ~JNTSYMP,
                        by = ~REGION + AGE_P,
                        design = NHIS16_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N16_JS_reg.age.c <- N16_JS_reg.age %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Age", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Age = as.character(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N16_JS_reg.age.ci <- confint(N16_JS_reg.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for js = 0
N16_JS_reg.age.ci <- N16_JS_reg.age.ci[-c(1:44), ]
#join proportion and ci
N16.JS.age.region <- bind_cols(N16_JS_reg.age.c, N16_JS_reg.age.ci) #final proportion, se & 95% ci
#save
##write.csv(N16.JS.age.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N16.JS.region.age.csv")

#Geofacet plot - Age & Region
JS.Age_Reg <- ggplot(N16.JS.age.region, aes(Age, Proportion)) +
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
  ggtitle("Regional prevalence of US residnets with joint symptoms by age in 2016") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N16.GF_AgeReg.JS.png", width = 14, height = 9)


#ii) age-specific regional logistic regressions
#18 to 24
sub1824 <- subset(NHIS16_DO, AGE_P == "18 to 24")
N16.JS.age1824.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub1824)
summary(N16.JS.age1824.reg)
exp(cbind(OR = coef(N16.JS.age1824.reg), confint(N16.JS.age1824.reg)))

#25 to 29
sub2529 <- subset(NHIS16_DO, AGE_P == "25 to 29")
N16.JS.age2529.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub2529)
summary(N16.JS.age2529.reg)
exp(cbind(OR = coef(N16.JS.age2529.reg), confint(N16.JS.age2529.reg)))

#30 to 34
sub3034 <- subset(NHIS16_DO, AGE_P == "30 to 34")
N16.JS.age3034.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub3034)
summary(N16.JS.age3034.reg)
exp(cbind(OR = coef(N16.JS.age3034.reg), confint(N16.JS.age3034.reg)))

#35 to 39
sub3539 <- subset(NHIS16_DO, AGE_P == "35 to 39")
N16.JS.age3539.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub3539)
summary(N16.JS.age3539.reg)
exp(cbind(OR = coef(N16.JS.age3539.reg), confint(N16.JS.age3539.reg)))

#40 to 44
sub4044 <- subset(NHIS16_DO, AGE_P == "40 to 44")
N16.JS.age4044.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub4044)
summary(N16.JS.age4044.reg)
exp(cbind(OR = coef(N16.JS.age4044.reg), confint(N16.JS.age4044.reg)))

#45 to 49
sub4549 <- subset(NHIS16_DO, AGE_P == "45 to 49")
N16.JS.age4549.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub4549)
summary(N16.JS.age4549.reg)
exp(cbind(OR = coef(N16.JS.age4549.reg), confint(N16.JS.age4549.reg)))

#50 to 54
sub5054 <- subset(NHIS16_DO, AGE_P =="50 to 54")
N16.JS.age5054.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub5054)
summary(N16.JS.age5054.reg)
exp(cbind(OR = coef(N16.JS.age5054.reg), confint(N16.JS.age5054.reg)))

#55 to 59
sub5559 <- subset(NHIS16_DO, AGE_P == "55 to 59")
N16.JS.age5559.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub5559)
summary(N16.JS.age5559.reg)
exp(cbind(OR = coef(N16.JS.age5559.reg), confint(N16.JS.age5559.reg)))

#60 to 64
sub6064 <- subset(NHIS16_DO, AGE_P == "60 to 64")
N16.JS.age6064.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub6064)
summary(N16.JS.age6064.reg)
exp(cbind(OR = coef(N16.JS.age6064.reg), confint(N16.JS.age6064.reg)))

#65 to 69
sub6569 <- subset(NHIS16_DO, AGE_P == "65 to 69")
N16.JS.age6569.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub6569)
summary(N16.JS.age6569.reg)
exp(cbind(OR = coef(N16.JS.age6569.reg), confint(N16.JS.age6569.reg)))

#70 and above
sub70 <- subset(NHIS16_DO, AGE_P == "70 and above")
N16.JS.age70.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                           family = quasibinomial,
                           design = sub70)
summary(N16.JS.age70.reg)
exp(cbind(OR = coef(N16.JS.age70.reg), confint(N16.JS.age70.reg)))

#III) prevalence by region, by sex

#i)  sex-specific regional prevalence
N16_JS_reg.sex <- svyby(formula = ~JNTSYMP,
                        by = ~REGION + SEX,
                        design = NHIS16_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N16_JS_reg.sex.c <- N16_JS_reg.sex %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Sex", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N16_JS_reg.sex.ci <- confint(N16_JS_reg.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for js = 0
N16_JS_reg.sex.ci <- N16_JS_reg.sex.ci[-c(1:8), ]
#join proportion and ci
N16.JS.sex.region <- bind_cols(N16_JS_reg.sex.c, N16_JS_reg.sex.ci) #final proprtion, se & 95% ci
#save
##write.csv(N16.JS.sex.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N16.JS.region.sex.csv")

#Geofacet - Sex & Region
JS.Sex_Reg16 <- ggplot(N16.JS.sex.region, aes(Sex, Proportion)) +
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
  ggtitle("Regional prevalence (%) of US residents with joint symptoms by sex in 2016") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N16.GF_SexReg.JS.png", width = 14, height = 9)


#ii) sex-specific regional logistic regressions
#males
submales <- subset(NHIS16_DO, SEX == "Male")
N16.JS.sexmale.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + AGE_P,
                             family = quasibinomial,
                             design = submales)
summary(N16.JS.sexmale.reg)
exp(cbind(OR = coef(N16.JS.sexmale.reg), confint(N16.JS.sexmale.reg)))

#females
subfemales <- subset(NHIS16_DO, SEX == "Female")
N16.JS.sexfemale.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + AGE_P,
                               family = quasibinomial,
                               design = subfemales)
summary(N16.JS.sexfemale.reg)
exp(cbind(OR = coef(N16.JS.sexfemale.reg), confint(N16.JS.sexfemale.reg)))


#3. Demographic trends

#I)  Age trends

#i)  Prevalence by age
N16_JS_age <- svyby(formula = ~JNTSYMP,
                    by = ~AGE_P,
                    design = NHIS16_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N16_JS_age.c <- N16_JS_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N16_JS_age_ci <- confint(N16_JS_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove js=0 ci
N16_JS_age_ci <- N16_JS_age_ci[-c(1:11), ]
#join proportion and ci
N16_JS.age <- bind_cols(N16_JS_age.c, N16_JS_age_ci) #final proportion, se & 95% ci
#Save
##write.csv(N16_JS.age, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N16.JS.age.csv")


#ii) Logistic regression by age
N16_JS.age_glm <- svyglm(JNTSYMP~AGE_P + SEX,
                         family = quasibinomial,
                         design = NHIS16_DO)
summary(N16_JS.age_glm)
exp(cbind(OR=coef(N16_JS.age_glm), confint(N16_JS.age_glm)))



#II)  Sex trends

#i)  Prevalence by sex
N16_JS_sex <- svyby(formula = ~JNTSYMP,
                    by = ~SEX,
                    design = NHIS16_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N16_JS_sex.c <- N16_JS_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N16_JS_sex_ci <- confint(N16_JS_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N16_JS_sex_ci <- N16_JS_sex_ci[-c(1:2), ]
#join proportion and ci
N16.JS.sex <- bind_cols(N16_JS_sex.c, N16_JS_sex_ci)
#save
##write.csv(N16.JS.sex, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N16.JS.sex.csv")


#ii) Logistic regression by sex
N16.JS.sex.glm.c <- svyglm(JNTSYMP~relevel(SEX, ref = "Male") + AGE_P,
                           family = quasibinomial,
                           design = NHIS16_DO)
summary(N16.JS.sex.glm.c)
exp(cbind(OR=coef(N16.JS.sex.glm.c), confint(N16.JS.sex.glm.c)))

#Prevalence of various age-sex groups and their joint pain locations

#1. Right shoulder
N16_JS.rtshoulder <- svyby(formula = ~JMTHP1,
                           by = ~AGE_P + SEX,
                           design = NHIS16_DO,
                           FUN = svymean,
                           na.rm = TRUE)
N16_JS.rtshoulder.c <- N16_JS.rtshoulder %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.RtShoulder")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N16_JS.rtshoulder.ci <- confint(N16_JS.rtshoulder) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N16_JS.rtshoulder.ci <- N16_JS.rtshoulder.ci[-c(23:44), ]
#join proportion and ci
N16.JS.rtshoulder <- bind_cols(N16_JS.rtshoulder.c, N16_JS.rtshoulder.ci)
#save
##write.csv(N16.JS.rtshoulder, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N16.rtshoulder.csv")

#2. Left Shoulder
N16_JS.ltshoulder <- svyby(formula = ~JMTHP2,
                           by = ~AGE_P + SEX,
                           design = NHIS16_DO,
                           FUN = svymean,
                           na.rm = TRUE)
N16_JS.ltshoulder.c <- N16_JS.ltshoulder %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltshoulder")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N16_JS.ltshoulder.ci <- confint(N16_JS.ltshoulder) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N16_JS.ltshoulder.ci <- N16_JS.ltshoulder.ci[-c(23:44), ]
#join proportion and ci
N16.JS.ltshoulder <- bind_cols(N16_JS.ltshoulder.c, N16_JS.ltshoulder.ci)
#save
##write.csv(N16.JS.ltshoulder, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N16.ltshoulder.csv")


#3. Right elbow
N16_JS.rtelbow <- svyby(formula = ~JMTHP3,
                        by = ~AGE_P + SEX,
                        design = NHIS16_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N16_JS.rtelbow.c <- N16_JS.rtelbow %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtelbow")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N16_JS.rtelbow.ci <- confint(N16_JS.rtelbow) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N16_JS.rtelbow.ci <- N16_JS.rtelbow.ci[-c(23:44), ]
#join proportion and ci
N16.JS.rtelbow <- bind_cols(N16_JS.rtelbow.c, N16_JS.rtelbow.ci)
#save
##write.csv(N16.JS.rtelbow, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N16.rtelbow.csv")

#4. Lt elbow
N16_JS.ltelbow <- svyby(formula = ~JMTHP4,
                        by = ~AGE_P + SEX,
                        design = NHIS16_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N16_JS.ltelbow.c <- N16_JS.ltelbow %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltelbow")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N16_JS.ltelbow.ci <- confint(N16_JS.ltelbow) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N16_JS.ltelbow.ci <- N16_JS.ltelbow.ci[-c(23:44), ]
#join proportion and ci
N16.JS.ltelbow <- bind_cols(N16_JS.ltelbow.c, N16_JS.ltelbow.ci)
#save
##write.csv(N16.JS.ltelbow, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N16.ltelbow.csv")

#5. Right hip
N16_JS.rthip <- svyby(formula = ~JMTHP5,
                      by = ~AGE_P + SEX,
                      design = NHIS16_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N16_JS.rthip.c <- N16_JS.rthip %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rthip")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N16_JS.rthip.ci <- confint(N16_JS.rthip) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N16_JS.rthip.ci <- N16_JS.rthip.ci[-c(23:44), ]
#join proportion and ci
N16.JS.rthip <- bind_cols(N16_JS.rthip.c, N16_JS.rthip.ci)
#save
##write.csv(N16.JS.rthip, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N16.rthip.csv")

#6. Left hip
N16_JS.lthip <- svyby(formula = ~JMTHP6,
                      by = ~AGE_P + SEX,
                      design = NHIS16_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N16_JS.lthip.c <- N16_JS.lthip %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.lthip")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N16_JS.lthip.ci <- confint(N16_JS.lthip) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N16_JS.lthip.ci <- N16_JS.lthip.ci[-c(23:44), ]
#join proportion and ci
N16.JS.lthip <- bind_cols(N16_JS.lthip.c, N16_JS.lthip.ci)
#save
##write.csv(N16.JS.lthip, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N16.lthip.csv")

#7. Right wrist
N16_JS.rtwrist <- svyby(formula = ~JMTHP7,
                        by = ~AGE_P + SEX,
                        design = NHIS16_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N16_JS.rtwrist.c <- N16_JS.rtwrist %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtwrist")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N16_JS.rtwrist.ci <- confint(N16_JS.rtwrist) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N16_JS.rtwrist.ci <- N16_JS.rtwrist.ci[-c(23:44), ]
#join proportion and ci
N16.JS.rtwrist <- bind_cols(N16_JS.rtwrist.c, N16_JS.rtwrist.ci)
#save
##write.csv(N16.JS.rtwrist, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N16.rtwrist.csv")

#8. Left wrist
N16_JS.ltwrist <- svyby(formula = ~JMTHP8,
                        by = ~AGE_P + SEX,
                        design = NHIS16_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N16_JS.ltwrist.c <- N16_JS.ltwrist %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltwrist")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N16_JS.ltwrist.ci <- confint(N16_JS.ltwrist) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N16_JS.ltwrist.ci <- N16_JS.ltwrist.ci[-c(23:44), ]
#join proportion and ci
N16.JS.ltwrist <- bind_cols(N16_JS.ltwrist.c, N16_JS.ltwrist.ci)
#save
##write.csv(N16.JS.ltwrist, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N16.ltwrist.csv")

#9. Right knee
N16_JS.rtknee <- svyby(formula = ~JMTHP9,
                       by = ~AGE_P + SEX,
                       design = NHIS16_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N16_JS.rtknee.c <- N16_JS.rtknee %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtknee")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N16_JS.rtknee.ci <- confint(N16_JS.rtknee) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N16_JS.rtknee.ci <- N16_JS.rtknee.ci[-c(23:44), ]
#join proportion and ci
N16.JS.rtknee <- bind_cols(N16_JS.rtknee.c, N16_JS.rtknee.ci)
#save
##write.csv(N16.JS.rtknee, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N16.rtknee.csv")

#10. Left knee
N16_JS.ltknee <- svyby(formula = ~JMTHP10,
                       by = ~AGE_P + SEX,
                       design = NHIS16_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N16_JS.ltknee.c <- N16_JS.ltknee %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltknee")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N16_JS.ltknee.ci <- confint(N16_JS.ltknee) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N16_JS.ltknee.ci <- N16_JS.ltknee.ci[-c(23:44), ]
#join proportion and ci
N16.JS.ltknee <- bind_cols(N16_JS.ltknee.c, N16_JS.ltknee.ci)
#save
##write.csv(N16.JS.ltknee, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N16.ltknee.csv")

#11. Right ankle
N16_JS.rtankle <- svyby(formula = ~JMTHP11,
                        by = ~AGE_P + SEX,
                        design = NHIS16_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N16_JS.rtankle.c <- N16_JS.rtankle %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtankle")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N16_JS.rtankle.ci <- confint(N16_JS.rtankle) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N16_JS.rtankle.ci <- N16_JS.rtankle.ci[-c(23:44), ]
#join proportion and ci
N16.JS.rtankle <- bind_cols(N16_JS.rtankle.c, N16_JS.rtankle.ci)
#save
#write.csv(N16.JS.rtankle, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N16.rtankle.csv")

#12. Left ankle
N16_JS.ltankle <- svyby(formula = ~JMTHP12,
                        by = ~AGE_P + SEX,
                        design = NHIS16_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N16_JS.ltankle.c <- N16_JS.ltankle %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltankle")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N16_JS.ltankle.ci <- confint(N16_JS.ltankle) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N16_JS.ltankle.ci <- N16_JS.ltankle.ci[-c(23:44), ]
#join proportion and ci
N16.JS.ltankle <- bind_cols(N16_JS.ltankle.c, N16_JS.ltankle.ci)
#save
#write.csv(N16.JS.ltankle, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N16.ltankle.csv")

#13. Right toes
N16_JS.rttoes <- svyby(formula = ~JMTHP13,
                       by = ~AGE_P + SEX,
                       design = NHIS16_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N16_JS.rttoes.c <- N16_JS.rttoes %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rttoes")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N16_JS.rttoes.ci <- confint(N16_JS.rttoes) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N16_JS.rttoes.ci <- N16_JS.rttoes.ci[-c(23:44), ]
#join proportion and ci
N16.JS.rttoes <- bind_cols(N16_JS.rttoes.c, N16_JS.rttoes.ci)
#save
#write.csv(N16.JS.rttoes, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N16.rttoes.csv")

#14. Left toes
N16_JS.lttoes <- svyby(formula = ~JMTHP14,
                       by = ~AGE_P + SEX,
                       design = NHIS16_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N16_JS.lttoes.c <- N16_JS.lttoes %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.lttoes")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N16_JS.lttoes.ci <- confint(N16_JS.lttoes) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N16_JS.lttoes.ci <- N16_JS.lttoes.ci[-c(23:44), ]
#join proportion and ci
N16.JS.lttoes <- bind_cols(N16_JS.lttoes.c, N16_JS.lttoes.ci)
#save
#write.csv(N16.JS.lttoes, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N16.lttoes.csv")

#15. Right fingers/thumb
N16_JS.rtfingthumb <- svyby(formula = ~JMTHP15,
                            by = ~AGE_P + SEX,
                            design = NHIS16_DO,
                            FUN = svymean,
                            na.rm = TRUE)
N16_JS.rtfingthumb.c <- N16_JS.rtfingthumb %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtfingthumb")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N16_JS.rtfingthumb.ci <- confint(N16_JS.rtfingthumb) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N16_JS.rtfingthumb.ci <- N16_JS.rtfingthumb.ci[-c(23:44), ]
#join proportion and ci
N16.JS.rtfingthumb <- bind_cols(N16_JS.rtfingthumb.c, N16_JS.rtfingthumb.ci)
#save
#write.csv(N16.JS.rtfingthumb, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N16.rtfingthumb.csv")

#16. Left fingers/thumb
N16_JS.ltfingthumb <- svyby(formula = ~JMTHP16,
                            by = ~AGE_P + SEX,
                            design = NHIS16_DO,
                            FUN = svymean,
                            na.rm = TRUE)
N16_JS.ltfingthumb.c <- N16_JS.ltfingthumb %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltfingthumb")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N16_JS.ltfingthumb.ci <- confint(N16_JS.ltfingthumb) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N16_JS.ltfingthumb.ci <- N16_JS.ltfingthumb.ci[-c(23:44), ]
#join proportion and ci
N16.JS.ltfingthumb <- bind_cols(N16_JS.ltfingthumb.c, N16_JS.ltfingthumb.ci)
#save
#write.csv(N16.JS.ltfingthumb, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N16.ltfingthumb.csv")

#17. Another
N16_JS.another <- svyby(formula = ~JMTHP17,
                        by = ~AGE_P + SEX,
                        design = NHIS16_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N16_JS.another.c <- N16_JS.another %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.another")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N16_JS.another.ci <- confint(N16_JS.another) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N16_JS.another.ci <- N16_JS.another.ci[-c(23:44), ]
#join proportion and ci
N16.JS.another <- bind_cols(N16_JS.another.c, N16_JS.another.ci)
#save
#write.csv(N16.JS.another, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N16.another.csv")

N16.JSloc <- Reduce(function(x, y) merge(x, y, all = TRUE), list(N16_JS.rtshoulder.c, N16_JS.ltshoulder.c, N16_JS.rtelbow.c, N16_JS.ltelbow.c, N16_JS.rthip.c, N16_JS.lthip.c, N16_JS.rtwrist.c, N16_JS.ltwrist.c, N16_JS.rtknee.c, N16_JS.ltknee.c, N16_JS.rtankle.c, N16_JS.ltankle.c, N16_JS.rttoes.c, N16_JS.lttoes.c, N16_JS.rtfingthumb.c, N16_JS.ltfingthumb.c, N16_JS.another.c))

#Wide to long
N16.JSlocation <- gather(N16.JSloc, Location, Proportion, Proportion.RtShoulder:Proportion.another, factor_key = TRUE)
N16.JSlocation <- N16.JSlocation %>% 
  unite("Demographic", Age:Sex)
N16.JSlocation$Location <- as.character(N16.JSlocation$Location)
N16.JSlocation$Location[N16.JSlocation$Location == "Proportion.RtShoulder"] <- "Right shoulder"
N16.JSlocation$Location[N16.JSlocation$Location == "Proportion.ltshoulder"] <- "Left shoulder"
N16.JSlocation$Location[N16.JSlocation$Location == "Proportion.rtelbow"] <- "Right elbow"
N16.JSlocation$Location[N16.JSlocation$Location == "Proportion.ltelbow"] <- "Left elbow"
N16.JSlocation$Location[N16.JSlocation$Location == "Proportion.rthip"] <- "Right hip"
N16.JSlocation$Location[N16.JSlocation$Location == "Proportion.lthip"] <- "Left hip"
N16.JSlocation$Location[N16.JSlocation$Location == "Proportion.rtwrist"] <- "Right wrist"
N16.JSlocation$Location[N16.JSlocation$Location == "Proportion.ltwrist"] <- "Left wrist"
N16.JSlocation$Location[N16.JSlocation$Location == "Proportion.rtknee"] <- "Right knee"
N16.JSlocation$Location[N16.JSlocation$Location == "Proportion.ltknee"] <- "Left knee"
N16.JSlocation$Location[N16.JSlocation$Location == "Proportion.rtankle"] <- "Right ankle"
N16.JSlocation$Location[N16.JSlocation$Location == "Proportion.ltankle"] <- "Left ankle"
N16.JSlocation$Location[N16.JSlocation$Location == "Proportion.rttoes"] <- "Right toes"
N16.JSlocation$Location[N16.JSlocation$Location == "Proportion.lttoes"] <- "Left toes"
N16.JSlocation$Location[N16.JSlocation$Location == "Proportion.rtfingthumb"] <- "Right fingers or thumb"
N16.JSlocation$Location[N16.JSlocation$Location == "Proportion.ltfingthumb"] <- "Left fingers or thumb"
N16.JSlocation$Location[N16.JSlocation$Location == "Proportion.another"] <- "Another joint"

#Heatmap of joint locations by demographic groups
N16.JSlocmap <- ggplot(N16.JSlocation,
                       aes(x = Demographic, y = Location, fill = Proportion)) +
  geom_tile() + 
  scale_fill_viridis(option = "viridis", direction = -1, limits = c(0, 60), breaks = c(10, 20, 30, 40, 50, 60), discrete = FALSE) +
  theme_bw() + 
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.text.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  xlab("Age-sex profile") +
  theme(legend.position = "right",
        strip.background = element_blank()) +
  theme(legend.title = element_text(size = 26)) +
  theme(legend.text = element_text(size = 22)) +
  labs(fill = "Prevalence (%)") +
  guides(fill = guide_colourbar(barwidth = 3, barheight = 25)) +
  scale_x_discrete(labels = c("Females, 18 to 24", "Males, 18 to 24", "Females, 25 to 29", "Males, 25 to 29", "Females, 30 to 34", "Males, 30 to 34", "Females, 35 to 39", "Males, 35 to 39", "Females, 40 to 44", "Males, 40 to 44", "Females, 45 to 49", "Males, 45 to 49", "Females, 50 to 54", "Males, 50 to 54", "Females, 55 to 59", "Males, 55 to 59", "Females, 60 to 64", "Males, 60 to 64", "Females, 65 to 69", "Males, 65 to 69", "Females, 70 and above", "Males, 70 and above")) +
  scale_y_discrete(limits = c("Another joint", "Right toes", "Left toes", "Right ankle", "Left ankle", "Right knee", "Left knee", "Right hip", "Left hip", "Right shoulder", "Left shoulder", "Right elbow", "Left elbow", "Right wrist", "Left wrist", "Right fingers or thumb", "Left fingers or thumb"))
ggsave("N16.age-sex.png", width = 20, height = 13)

#III) Socioeconomic trends (employment status only)

#i)   Prevalence by employment status
N16_JS_emp <- svyby(formula = ~JNTSYMP,
                    by = ~DOINGLWA,
                    design = NHIS16_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N16_JS_emp.c <- N16_JS_emp %>%
  select(1, 3, 5) %>%
  setNames(c("Employment", "Proportion", "SE")) %>%
  mutate(Employment = as.factor(Employment)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N16_JS_emp_ci <- confint(N16_JS_emp) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for js = 0
N16_JS_emp_ci <- N16_JS_emp_ci[-c(1:2), ]
#join ci and proportion
N16.JS.emp <- bind_cols(N16_JS_emp.c, N16_JS_emp_ci) #final proportion, se & 95% ci
#save
#write.csv(N16.JS.emp, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N16.JS.emp.csv")

#ii)  Logistic regression by employment status
N16_JS.emp_glm <- svyglm(JNTSYMP~relevel(DOINGLWA, ref = "Working for pay at a job/business") + AGE_P + SEX,
                         family = quasibinomial,
                         design = NHIS16_DO)
summary(N16_JS.emp_glm)
exp(cbind(OR=coef(N16_JS.emp_glm), confint(N16_JS.emp_glm)))

#4. Lifestyle trends

#I)  BMI
#logistic regression
N16.BMI.glm <- svyglm(JNTSYMP ~ BMI + AGE_P + SEX,
                      family = quasibinomial,
                      design = NHIS16_DO)
summary(N16.BMI.glm)
exp(cbind(OR=coef(N16.BMI.glm), confint(N16.BMI.glm)))

#II) Physical activity
#logistic regression
N16.PA.JS.glm <- svyglm(JNTSYMP ~ relevel(factor(PAC, ordered = FALSE), ref = "Physically active") + AGE_P + SEX,
                        family = quasibinomial,
                        design = NHIS16_DO)
summary(N16.PA.JS.glm)
exp(cbind(OR = coef(N16.PA.JS.glm), confint(N16.PA.JS.glm)))


#___________________________________________________________________________________________________________________

# == 2015 == #

#1. Overall prevalence
N15_JS <- svymean(~factor(JNTSYMP), 
                  NHIS15_DO, 
                  na.rm = TRUE)

N15_JS.c <- N15_JS %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N15_JS_ci <- confint(N15_JS) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
N15.JS <- bind_cols(N15_JS.c, N15_JS_ci)
#remove js = 0
N15.JS <- N15.JS[-c(1), ] #final proportion, se & 95%
#save
#write.csv(N15.JS, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N15.JS.csv")

#2. Spatial analysis, by region

#I)   Overall population

#i)   regional prevalence for the overall population
N15_JS_reg <- svyby(formula = ~JNTSYMP,
                    by = ~REGION,
                    design = NHIS15_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N15_JS_reg.c <- N15_JS_reg %>%
  select(1, 3, 5) %>%
  setNames(c("Region", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N15_JS_reg.ci <- confint(N15_JS_reg) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for js = 0
N15_JS_reg.ci <- N15_JS_reg.ci[-c(1:4), ]
#join proportion and ci
N15.JS.region <- bind_cols(N15_JS_reg.c, N15_JS_reg.ci) #final proportion, se & 95% ci
#save
#write.csv(N15.JS.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N15.JS.region.csv")

#Create overall regional prevalence

#Change the region column to "NAME" to match sf
names(N15.JS.region)[names(N15.JS.region) == "Region"] <- "NAME"

N15.JS.joined <- regions %>%
  left_join(N15.JS.region)

N15.JS.joined$NAME <- as.factor(N15.JS.joined$NAME)

#Plot
JSregplot <- ggplot(data = N15.JS.joined) +
  geom_sf(aes(fill = Proportion), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  scale_fill_viridis_c(name = "Prevalence (%)", option = "viridis", direction = -1, limits = c(25, 40), breaks = c(25, 30, 35, 40)) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  ggtitle("Regional prevalence (%) of US residents with joint symptoms in 2015") +
  theme(plot.title = element_text(size = 10, hjust = 0.5))
ggsave("N15.JSregchor.png", width = 5, height = 5)


#ii)  logistic regression by region
N15.JS.reg.glm <- svyglm(relevel(JNTSYMP, ref = "0")~ REGION + AGE_P,
                         family = quasibinomial,
                         design = NHIS15_DO)
summary(N15.JS.reg.glm)
exp(cbind(OR=coef(N15.JS.reg.glm), confint(N15.JS.reg.glm)))

#II)  prevalence by region, by age

#i)  age-specific regional prevalance
N15_JS_reg.age <- svyby(formula = ~JNTSYMP,
                        by = ~REGION + AGE_P,
                        design = NHIS15_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N15_JS_reg.age.c <- N15_JS_reg.age %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Age", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Age = as.character(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N15_JS_reg.age.ci <- confint(N15_JS_reg.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for js = 0
N15_JS_reg.age.ci <- N15_JS_reg.age.ci[-c(1:44), ]
#join proportion and ci
N15.JS.age.region <- bind_cols(N15_JS_reg.age.c, N15_JS_reg.age.ci) #final proportion, se & 95% ci
#save
#write.csv(N15.JS.age.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N15.JS.region.age.csv")

#Geofacet plot - Age & Region
JS.Age_Reg <- ggplot(N15.JS.age.region, aes(Age, Proportion)) +
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
  ggtitle("Regional prevalence (%) of US residents with joint symptoms by age group in 2015") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N15.GF_AgeReg.JS.png", width = 14, height = 9)


#ii) age-specific regional logistic regressions
#18 to 24
sub1824 <- subset(NHIS15_DO, AGE_P == "18 to 24")
N15.JS.age1824.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub1824)
summary(N15.JS.age1824.reg)
exp(cbind(OR = coef(N15.JS.age1824.reg), confint(N15.JS.age1824.reg)))

#25 to 29
sub2529 <- subset(NHIS15_DO, AGE_P == "25 to 29")
N15.JS.age2529.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub2529)
summary(N15.JS.age2529.reg)
exp(cbind(OR = coef(N15.JS.age2529.reg), confint(N15.JS.age2529.reg)))

#30 to 34
sub3034 <- subset(NHIS15_DO, AGE_P == "30 to 34")
N15.JS.age3034.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub3034)
summary(N15.JS.age3034.reg)
exp(cbind(OR = coef(N15.JS.age3034.reg), confint(N15.JS.age3034.reg)))

#35 to 39
sub3539 <- subset(NHIS15_DO, AGE_P == "35 to 39")
N15.JS.age3539.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub3539)
summary(N15.JS.age3539.reg)
exp(cbind(OR = coef(N15.JS.age3539.reg), confint(N15.JS.age3539.reg)))

#40 to 44
sub4044 <- subset(NHIS15_DO, AGE_P == "40 to 44")
N15.JS.age4044.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub4044)
summary(N15.JS.age4044.reg)
exp(cbind(OR = coef(N15.JS.age4044.reg), confint(N15.JS.age4044.reg)))

#45 to 49
sub4549 <- subset(NHIS15_DO, AGE_P == "45 to 49")
N15.JS.age4549.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub4549)
summary(N15.JS.age4549.reg)
exp(cbind(OR = coef(N15.JS.age4549.reg), confint(N15.JS.age4549.reg)))

#50 to 54
sub5054 <- subset(NHIS15_DO, AGE_P =="50 to 54")
N15.JS.age5054.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub5054)
summary(N15.JS.age5054.reg)
exp(cbind(OR = coef(N15.JS.age5054.reg), confint(N15.JS.age5054.reg)))

#55 to 59
sub5559 <- subset(NHIS15_DO, AGE_P == "55 to 59")
N15.JS.age5559.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub5559)
summary(N15.JS.age5559.reg)
exp(cbind(OR = coef(N15.JS.age5559.reg), confint(N15.JS.age5559.reg)))

#60 to 64
sub6064 <- subset(NHIS15_DO, AGE_P == "60 to 64")
N15.JS.age6064.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub6064)
summary(N15.JS.age6064.reg)
exp(cbind(OR = coef(N15.JS.age6064.reg), confint(N15.JS.age6064.reg)))

#65 to 69
sub6569 <- subset(NHIS15_DO, AGE_P == "65 to 69")
N15.JS.age6569.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub6569)
summary(N15.JS.age6569.reg)
exp(cbind(OR = coef(N15.JS.age6569.reg), confint(N15.JS.age6569.reg)))

#70 and above
sub70 <- subset(NHIS15_DO, AGE_P == "70 and above")
N15.JS.age70.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                           family = quasibinomial,
                           design = sub70)
summary(N15.JS.age70.reg)
exp(cbind(OR = coef(N15.JS.age70.reg), confint(N15.JS.age70.reg)))

#III) prevalence by region, by sex

#i)  sex-specific regional prevalence
N15_JS_reg.sex <- svyby(formula = ~JNTSYMP,
                        by = ~REGION + SEX,
                        design = NHIS15_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N15_JS_reg.sex.c <- N15_JS_reg.sex %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Sex", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N15_JS_reg.sex.ci <- confint(N15_JS_reg.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for js = 0
N15_JS_reg.sex.ci <- N15_JS_reg.sex.ci[-c(1:8), ]
#join proportion and ci
N15.JS.sex.region <- bind_cols(N15_JS_reg.sex.c, N15_JS_reg.sex.ci) #final proprtion, se & 95% ci
#save
#write.csv(N15.JS.sex.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N15.JS.region.sex.csv")

#Geofacet - Sex & Region
JS.Sex_Reg15 <- ggplot(N15.JS.sex.region, aes(Sex, Proportion)) +
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
  ggtitle("Regional prevalence (%) of US residents with joint symptoms by sex in 2015")
ggsave("N15.GF_SexReg.JS.png", width = 14, height = 9)


#ii) sex-specific regional logistic regressions
#males
submales <- subset(NHIS15_DO, SEX == "Male")
N15.JS.sexmale.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + AGE_P,
                             family = quasibinomial,
                             design = submales)
summary(N15.JS.sexmale.reg)
exp(cbind(OR = coef(N15.JS.sexmale.reg), confint(N15.JS.sexmale.reg)))

#females
subfemales <- subset(NHIS15_DO, SEX == "Female")
N15.JS.sexfemale.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + AGE_P,
                               family = quasibinomial,
                               design = subfemales)
summary(N15.JS.sexfemale.reg)
exp(cbind(OR = coef(N15.JS.sexfemale.reg), confint(N15.JS.sexfemale.reg)))


#3. Demographic trends

#I)  Age trends

#i)  Prevalence by age
N15_JS_age <- svyby(formula = ~JNTSYMP,
                    by = ~AGE_P,
                    design = NHIS15_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N15_JS_age.c <- N15_JS_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N15_JS_age_ci <- confint(N15_JS_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove js=0 ci
N15_JS_age_ci <- N15_JS_age_ci[-c(1:11), ]
#join proportion and ci
N15_JS.age <- bind_cols(N15_JS_age.c, N15_JS_age_ci) #final proportion, se & 95% ci
#Save
#write.csv(N15_JS.age, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N15.JS.age.csv")


#ii) Logistic regression by age
N15_JS.age_glm <- svyglm(JNTSYMP~AGE_P + SEX,
                         family = quasibinomial,
                         design = NHIS15_DO)
summary(N15_JS.age_glm)
exp(cbind(OR=coef(N15_JS.age_glm), confint(N15_JS.age_glm)))



#II)  Sex trends

#i)  Prevalence by sex
N15_JS_sex <- svyby(formula = ~JNTSYMP,
                    by = ~SEX,
                    design = NHIS15_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N15_JS_sex.c <- N15_JS_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N15_JS_sex_ci <- confint(N15_JS_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N15_JS_sex_ci <- N15_JS_sex_ci[-c(1:2), ]
#join proportion and ci
N15.JS.sex <- bind_cols(N15_JS_sex.c, N15_JS_sex_ci)
#save
#write.csv(N15.JS.sex, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N15.JS.sex.csv")


#ii) Logistic regression by sex
N15.JS.sex.glm.c <- svyglm(JNTSYMP~relevel(SEX, ref = "Male") + AGE_P,
                           family = quasibinomial,
                           design = NHIS15_DO)
summary(N15.JS.sex.glm.c)
exp(cbind(OR=coef(N15.JS.sex.glm.c), confint(N15.JS.sex.glm.c)))


#Prevalence of various age-sex groups and their joint pain locations

#1. Right shoulder
N15_JS.rtshoulder <- svyby(formula = ~JMTHP1,
                           by = ~AGE_P + SEX,
                           design = NHIS15_DO,
                           FUN = svymean,
                           na.rm = TRUE)
N15_JS.rtshoulder.c <- N15_JS.rtshoulder %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.RtShoulder")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N15_JS.rtshoulder.ci <- confint(N15_JS.rtshoulder) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N15_JS.rtshoulder.ci <- N15_JS.rtshoulder.ci[-c(23:44), ]
#join proportion and ci
N15.JS.rtshoulder <- bind_cols(N15_JS.rtshoulder.c, N15_JS.rtshoulder.ci)
#save
#write.csv(N15.JS.rtshoulder, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N15.rtshoulder.csv")

#2. Left Shoulder
N15_JS.ltshoulder <- svyby(formula = ~JMTHP2,
                           by = ~AGE_P + SEX,
                           design = NHIS15_DO,
                           FUN = svymean,
                           na.rm = TRUE)
N15_JS.ltshoulder.c <- N15_JS.ltshoulder %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltshoulder")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N15_JS.ltshoulder.ci <- confint(N15_JS.ltshoulder) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N15_JS.ltshoulder.ci <- N15_JS.ltshoulder.ci[-c(23:44), ]
#join proportion and ci
N15.JS.ltshoulder <- bind_cols(N15_JS.ltshoulder.c, N15_JS.ltshoulder.ci)
#save
#write.csv(N15.JS.ltshoulder, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N15.ltshoulder.csv")


#3. Right elbow
N15_JS.rtelbow <- svyby(formula = ~JMTHP3,
                        by = ~AGE_P + SEX,
                        design = NHIS15_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N15_JS.rtelbow.c <- N15_JS.rtelbow %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtelbow")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N15_JS.rtelbow.ci <- confint(N15_JS.rtelbow) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N15_JS.rtelbow.ci <- N15_JS.rtelbow.ci[-c(23:44), ]
#join proportion and ci
N15.JS.rtelbow <- bind_cols(N15_JS.rtelbow.c, N15_JS.rtelbow.ci)
#save
#write.csv(N15.JS.rtelbow, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N15.rtelbow.csv")

#4. Lt elbow
N15_JS.ltelbow <- svyby(formula = ~JMTHP4,
                        by = ~AGE_P + SEX,
                        design = NHIS15_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N15_JS.ltelbow.c <- N15_JS.ltelbow %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltelbow")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N15_JS.ltelbow.ci <- confint(N15_JS.ltelbow) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N15_JS.ltelbow.ci <- N15_JS.ltelbow.ci[-c(23:44), ]
#join proportion and ci
N15.JS.ltelbow <- bind_cols(N15_JS.ltelbow.c, N15_JS.ltelbow.ci)
#save
#write.csv(N15.JS.ltelbow, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N15.ltelbow.csv")

#5. Right hip
N15_JS.rthip <- svyby(formula = ~JMTHP5,
                      by = ~AGE_P + SEX,
                      design = NHIS15_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N15_JS.rthip.c <- N15_JS.rthip %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rthip")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N15_JS.rthip.ci <- confint(N15_JS.rthip) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N15_JS.rthip.ci <- N15_JS.rthip.ci[-c(23:44), ]
#join proportion and ci
N15.JS.rthip <- bind_cols(N15_JS.rthip.c, N15_JS.rthip.ci)
#save
#write.csv(N15.JS.rthip, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N15.rthip.csv")

#6. Left hip
N15_JS.lthip <- svyby(formula = ~JMTHP6,
                      by = ~AGE_P + SEX,
                      design = NHIS15_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N15_JS.lthip.c <- N15_JS.lthip %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.lthip")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N15_JS.lthip.ci <- confint(N15_JS.lthip) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N15_JS.lthip.ci <- N15_JS.lthip.ci[-c(23:44), ]
#join proportion and ci
N15.JS.lthip <- bind_cols(N15_JS.lthip.c, N15_JS.lthip.ci)
#save
#write.csv(N15.JS.lthip, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N15.lthip.csv")

#7. Right wrist
N15_JS.rtwrist <- svyby(formula = ~JMTHP7,
                        by = ~AGE_P + SEX,
                        design = NHIS15_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N15_JS.rtwrist.c <- N15_JS.rtwrist %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtwrist")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N15_JS.rtwrist.ci <- confint(N15_JS.rtwrist) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N15_JS.rtwrist.ci <- N15_JS.rtwrist.ci[-c(23:44), ]
#join proportion and ci
N15.JS.rtwrist <- bind_cols(N15_JS.rtwrist.c, N15_JS.rtwrist.ci)
#save
#write.csv(N15.JS.rtwrist, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N15.rtwrist.csv")

#8. Left wrist
N15_JS.ltwrist <- svyby(formula = ~JMTHP8,
                        by = ~AGE_P + SEX,
                        design = NHIS15_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N15_JS.ltwrist.c <- N15_JS.ltwrist %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltwrist")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N15_JS.ltwrist.ci <- confint(N15_JS.ltwrist) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N15_JS.ltwrist.ci <- N15_JS.ltwrist.ci[-c(23:44), ]
#join proportion and ci
N15.JS.ltwrist <- bind_cols(N15_JS.ltwrist.c, N15_JS.ltwrist.ci)
#save
#write.csv(N15.JS.ltwrist, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N15.ltwrist.csv")

#9. Right knee
N15_JS.rtknee <- svyby(formula = ~JMTHP9,
                       by = ~AGE_P + SEX,
                       design = NHIS15_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N15_JS.rtknee.c <- N15_JS.rtknee %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtknee")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N15_JS.rtknee.ci <- confint(N15_JS.rtknee) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N15_JS.rtknee.ci <- N15_JS.rtknee.ci[-c(23:44), ]
#join proportion and ci
N15.JS.rtknee <- bind_cols(N15_JS.rtknee.c, N15_JS.rtknee.ci)
#save
#write.csv(N15.JS.rtknee, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N15.rtknee.csv")

#10. Left knee
N15_JS.ltknee <- svyby(formula = ~JMTHP10,
                       by = ~AGE_P + SEX,
                       design = NHIS15_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N15_JS.ltknee.c <- N15_JS.ltknee %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltknee")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N15_JS.ltknee.ci <- confint(N15_JS.ltknee) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N15_JS.ltknee.ci <- N15_JS.ltknee.ci[-c(23:44), ]
#join proportion and ci
N15.JS.ltknee <- bind_cols(N15_JS.ltknee.c, N15_JS.ltknee.ci)
#save
#write.csv(N15.JS.ltknee, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N15.ltknee.csv")

#11. Right ankle
N15_JS.rtankle <- svyby(formula = ~JMTHP11,
                        by = ~AGE_P + SEX,
                        design = NHIS15_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N15_JS.rtankle.c <- N15_JS.rtankle %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtankle")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N15_JS.rtankle.ci <- confint(N15_JS.rtankle) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N15_JS.rtankle.ci <- N15_JS.rtankle.ci[-c(23:44), ]
#join proportion and ci
N15.JS.rtankle <- bind_cols(N15_JS.rtankle.c, N15_JS.rtankle.ci)
#save
#write.csv(N15.JS.rtankle, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N15.rtankle.csv")

#12. Left ankle
N15_JS.ltankle <- svyby(formula = ~JMTHP12,
                        by = ~AGE_P + SEX,
                        design = NHIS15_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N15_JS.ltankle.c <- N15_JS.ltankle %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltankle")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N15_JS.ltankle.ci <- confint(N15_JS.ltankle) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N15_JS.ltankle.ci <- N15_JS.ltankle.ci[-c(23:44), ]
#join proportion and ci
N15.JS.ltankle <- bind_cols(N15_JS.ltankle.c, N15_JS.ltankle.ci)
#save
#write.csv(N15.JS.ltankle, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N15.ltankle.csv")

#13. Right toes
N15_JS.rttoes <- svyby(formula = ~JMTHP13,
                       by = ~AGE_P + SEX,
                       design = NHIS15_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N15_JS.rttoes.c <- N15_JS.rttoes %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rttoes")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N15_JS.rttoes.ci <- confint(N15_JS.rttoes) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N15_JS.rttoes.ci <- N15_JS.rttoes.ci[-c(23:44), ]
#join proportion and ci
N15.JS.rttoes <- bind_cols(N15_JS.rttoes.c, N15_JS.rttoes.ci)
#save
#write.csv(N15.JS.rttoes, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N15.rttoes.csv")

#14. Left toes
N15_JS.lttoes <- svyby(formula = ~JMTHP14,
                       by = ~AGE_P + SEX,
                       design = NHIS15_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N15_JS.lttoes.c <- N15_JS.lttoes %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.lttoes")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N15_JS.lttoes.ci <- confint(N15_JS.lttoes) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N15_JS.lttoes.ci <- N15_JS.lttoes.ci[-c(23:44), ]
#join proportion and ci
N15.JS.lttoes <- bind_cols(N15_JS.lttoes.c, N15_JS.lttoes.ci)
#save
#write.csv(N15.JS.lttoes, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N15.lttoes.csv")

#15. Right fingers/thumb
N15_JS.rtfingthumb <- svyby(formula = ~JMTHP15,
                            by = ~AGE_P + SEX,
                            design = NHIS15_DO,
                            FUN = svymean,
                            na.rm = TRUE)
N15_JS.rtfingthumb.c <- N15_JS.rtfingthumb %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtfingthumb")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N15_JS.rtfingthumb.ci <- confint(N15_JS.rtfingthumb) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N15_JS.rtfingthumb.ci <- N15_JS.rtfingthumb.ci[-c(23:44), ]
#join proportion and ci
N15.JS.rtfingthumb <- bind_cols(N15_JS.rtfingthumb.c, N15_JS.rtfingthumb.ci)
#save
#write.csv(N15.JS.rtfingthumb, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N15.rtfingthumb.csv")

#16. Left fingers/thumb
N15_JS.ltfingthumb <- svyby(formula = ~JMTHP16,
                            by = ~AGE_P + SEX,
                            design = NHIS15_DO,
                            FUN = svymean,
                            na.rm = TRUE)
N15_JS.ltfingthumb.c <- N15_JS.ltfingthumb %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltfingthumb")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N15_JS.ltfingthumb.ci <- confint(N15_JS.ltfingthumb) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N15_JS.ltfingthumb.ci <- N15_JS.ltfingthumb.ci[-c(23:44), ]
#join proportion and ci
N15.JS.ltfingthumb <- bind_cols(N15_JS.ltfingthumb.c, N15_JS.ltfingthumb.ci)
#save
#write.csv(N15.JS.ltfingthumb, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N15.ltfingthumb.csv")

#17. Another
N15_JS.another <- svyby(formula = ~JMTHP17,
                        by = ~AGE_P + SEX,
                        design = NHIS15_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N15_JS.another.c <- N15_JS.another %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.another")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N15_JS.another.ci <- confint(N15_JS.another) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N15_JS.another.ci <- N15_JS.another.ci[-c(23:44), ]
#join proportion and ci
N15.JS.another <- bind_cols(N15_JS.another.c, N15_JS.another.ci)
#save
#write.csv(N15.JS.another, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N15.another.csv")

N15.JSloc <- Reduce(function(x, y) merge(x, y, all = TRUE), list(N15_JS.rtshoulder.c, N15_JS.ltshoulder.c, N15_JS.rtelbow.c, N15_JS.ltelbow.c, N15_JS.rthip.c, N15_JS.lthip.c, N15_JS.rtwrist.c, N15_JS.ltwrist.c, N15_JS.rtknee.c, N15_JS.ltknee.c, N15_JS.rtankle.c, N15_JS.ltankle.c, N15_JS.rttoes.c, N15_JS.lttoes.c, N15_JS.rtfingthumb.c, N15_JS.ltfingthumb.c, N15_JS.another.c))

#Wide to long
N15.JSlocation <- gather(N15.JSloc, Location, Proportion, Proportion.RtShoulder:Proportion.another, factor_key = TRUE)
N15.JSlocation <- N15.JSlocation %>% 
  unite("Demographic", Age:Sex)
N15.JSlocation$Location <- as.character(N15.JSlocation$Location)
N15.JSlocation$Location[N15.JSlocation$Location == "Proportion.RtShoulder"] <- "Right shoulder"
N15.JSlocation$Location[N15.JSlocation$Location == "Proportion.ltshoulder"] <- "Left shoulder"
N15.JSlocation$Location[N15.JSlocation$Location == "Proportion.rtelbow"] <- "Right elbow"
N15.JSlocation$Location[N15.JSlocation$Location == "Proportion.ltelbow"] <- "Left elbow"
N15.JSlocation$Location[N15.JSlocation$Location == "Proportion.rthip"] <- "Right hip"
N15.JSlocation$Location[N15.JSlocation$Location == "Proportion.lthip"] <- "Left hip"
N15.JSlocation$Location[N15.JSlocation$Location == "Proportion.rtwrist"] <- "Right wrist"
N15.JSlocation$Location[N15.JSlocation$Location == "Proportion.ltwrist"] <- "Left wrist"
N15.JSlocation$Location[N15.JSlocation$Location == "Proportion.rtknee"] <- "Right knee"
N15.JSlocation$Location[N15.JSlocation$Location == "Proportion.ltknee"] <- "Left knee"
N15.JSlocation$Location[N15.JSlocation$Location == "Proportion.rtankle"] <- "Right ankle"
N15.JSlocation$Location[N15.JSlocation$Location == "Proportion.ltankle"] <- "Left ankle"
N15.JSlocation$Location[N15.JSlocation$Location == "Proportion.rttoes"] <- "Right toes"
N15.JSlocation$Location[N15.JSlocation$Location == "Proportion.lttoes"] <- "Left toes"
N15.JSlocation$Location[N15.JSlocation$Location == "Proportion.rtfingthumb"] <- "Right fingers or thumb"
N15.JSlocation$Location[N15.JSlocation$Location == "Proportion.ltfingthumb"] <- "Left fingers or thumb"
N15.JSlocation$Location[N15.JSlocation$Location == "Proportion.another"] <- "Another joint"

#Heatmap of joint locations by demographic groups
N15.JSlocmap <- ggplot(N15.JSlocation,
                       aes(x = Demographic, y = Location, fill = Proportion)) +
  geom_tile() + 
  scale_fill_viridis(option = "viridis", direction = -1, limits = c(0, 60), breaks = c(10, 20, 30, 40, 50, 60), discrete = FALSE) +
  theme_bw() + 
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.text.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  xlab("Age-sex profile") +
  theme(legend.position = "right",
        strip.background = element_blank()) +
  theme(legend.title = element_text(size = 26)) +
  theme(legend.text = element_text(size = 22)) +
  labs(fill = "Prevalence (%)") +
  guides(fill = guide_colourbar(barwidth = 3, barheight = 25)) +
  scale_x_discrete(labels = c("Females, 18 to 24", "Males, 18 to 24", "Females, 25 to 29", "Males, 25 to 29", "Females, 30 to 34", "Males, 30 to 34", "Females, 35 to 39", "Males, 35 to 39", "Females, 40 to 44", "Males, 40 to 44", "Females, 45 to 49", "Males, 45 to 49", "Females, 50 to 54", "Males, 50 to 54", "Females, 55 to 59", "Males, 55 to 59", "Females, 60 to 64", "Males, 60 to 64", "Females, 65 to 69", "Males, 65 to 69", "Females, 70 and above", "Males, 70 and above")) +
  scale_y_discrete(limits = c("Another joint", "Right toes", "Left toes", "Right ankle", "Left ankle", "Right knee", "Left knee", "Right hip", "Left hip", "Right shoulder", "Left shoulder", "Right elbow", "Left elbow", "Right wrist", "Left wrist", "Right fingers or thumb", "Left fingers or thumb"))
ggsave("N15.age-sex.png", width = 20, height = 13)



#III) Socioeconomic trends (employment status only)

#i)   Prevalence by employment status
N15_JS_emp <- svyby(formula = ~JNTSYMP,
                    by = ~DOINGLWA,
                    design = NHIS15_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N15_JS_emp.c <- N15_JS_emp %>%
  select(1, 3, 5) %>%
  setNames(c("Employment", "Proportion", "SE")) %>%
  mutate(Employment = as.factor(Employment)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N15_JS_emp_ci <- confint(N15_JS_emp) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for js = 0
N15_JS_emp_ci <- N15_JS_emp_ci[-c(1:2), ]
#join ci and proportion
N15.JS.emp <- bind_cols(N15_JS_emp.c, N15_JS_emp_ci) #final proportion, se & 95% ci
#save
#write.csv(N15.JS.emp, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N15.JS.emp.csv")

#ii)  Logistic regression by employment status
N15_JS.emp_glm <- svyglm(JNTSYMP~relevel(DOINGLWA, ref = "Working for pay at a job/business") + AGE_P + SEX,
                         family = quasibinomial,
                         design = NHIS15_DO)
summary(N15_JS.emp_glm)
exp(cbind(OR=coef(N15_JS.emp_glm), confint(N15_JS.emp_glm)))

#4. Lifestyle trends

#I)  BMI
#logistic regression
N15.BMI.glm <- svyglm(JNTSYMP ~ BMI + AGE_P + SEX,
                      family = quasibinomial,
                      design = NHIS15_DO)
summary(N15.BMI.glm)
exp(cbind(OR=coef(N15.BMI.glm), confint(N15.BMI.glm)))

#II) Physical activity
#logistic regression
N15.PA.JS.glm <- svyglm(JNTSYMP ~ relevel(factor(PAC, ordered = FALSE), ref = "Physically active") + AGE_P + SEX,
                        family = quasibinomial,
                        design = NHIS15_DO)
summary(N15.PA.JS.glm)
exp(cbind(OR = coef(N15.PA.JS.glm), confint(N15.PA.JS.glm)))

#___________________________________________________________________________________________________________________

#1. Overall prevalence
N14_JS <- svymean(~factor(JNTSYMP), 
                  NHIS14_DO, 
                  na.rm = TRUE)

N14_JS.c <- N14_JS %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N14_JS_ci <- confint(N14_JS) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
N14.JS <- bind_cols(N14_JS.c, N14_JS_ci)
#remove js = 0
N14.JS <- N14.JS[-c(1), ] #final proportion, se & 95%
#save
#write.csv(N14.JS, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N14.JS.csv")

#2. Spatial analysis, by region

#I)   Overall population

#i)   regional prevalence for the overall population
N14_JS_reg <- svyby(formula = ~JNTSYMP,
                    by = ~REGION,
                    design = NHIS14_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N14_JS_reg.c <- N14_JS_reg %>%
  select(1, 3, 5) %>%
  setNames(c("Region", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N14_JS_reg.ci <- confint(N14_JS_reg) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for js = 0
N14_JS_reg.ci <- N14_JS_reg.ci[-c(1:4), ]
#join proportion and ci
N14.JS.region <- bind_cols(N14_JS_reg.c, N14_JS_reg.ci) #final proportion, se & 95% ci
#save
#write.csv(N14.JS.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N14.JS.region.csv")

#Create overall regional prevalence

#Change the region column to "NAME" to match sf
names(N14.JS.region)[names(N14.JS.region) == "Region"] <- "NAME"

N14.JS.joined <- regions %>%
  left_join(N14.JS.region)

N14.JS.joined$NAME <- as.factor(N14.JS.joined$NAME)

#Plot
JSregplot <- ggplot(data = N14.JS.joined) +
  geom_sf(aes(fill = Proportion), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  scale_fill_viridis_c(name = "Prevalence (%)", option = "viridis", direction = -1, limits = c(25, 40), breaks = c(25, 30, 35, 40)) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  ggtitle("Regional prevalence (%) of US residents with joint symptoms in 2014") +
  theme(plot.title = element_text(size = 10, hjust = 0.5))
ggsave("N14.JSregchor.png", width = 5, height = 5)


#ii)  logistic regression by region
N14.JS.reg.glm <- svyglm(relevel(JNTSYMP, ref = "0")~ REGION + AGE_P,
                         family = quasibinomial,
                         design = NHIS14_DO)
summary(N14.JS.reg.glm)
exp(cbind(OR=coef(N14.JS.reg.glm), confint(N14.JS.reg.glm)))

#II)  prevalence by region, by age

#i)  age-specific regional prevalance
N14_JS_reg.age <- svyby(formula = ~JNTSYMP,
                        by = ~REGION + AGE_P,
                        design = NHIS14_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N14_JS_reg.age.c <- N14_JS_reg.age %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Age", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Age = as.character(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N14_JS_reg.age.ci <- confint(N14_JS_reg.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for js = 0
N14_JS_reg.age.ci <- N14_JS_reg.age.ci[-c(1:44), ]
#join proportion and ci
N14.JS.age.region <- bind_cols(N14_JS_reg.age.c, N14_JS_reg.age.ci) #final proportion, se & 95% ci
#save
#write.csv(N14.JS.age.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N14.JS.region.age.csv")

#Geofacet plot - Age & Region
JS.Age_Reg <- ggplot(N14.JS.age.region, aes(Age, Proportion)) +
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
  ggtitle("Regional prevalence (%) of US residents with joint symptoms by age group in 2014") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N14.GF_AgeReg.JS.png", width = 14, height = 9)


#ii) age-specific regional logistic regressions
#18 to 24
sub1824 <- subset(NHIS14_DO, AGE_P == "18 to 24")
N14.JS.age1824.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub1824)
summary(N14.JS.age1824.reg)
exp(cbind(OR = coef(N14.JS.age1824.reg), confint(N14.JS.age1824.reg)))

#25 to 29
sub2529 <- subset(NHIS14_DO, AGE_P == "25 to 29")
N14.JS.age2529.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub2529)
summary(N14.JS.age2529.reg)
exp(cbind(OR = coef(N14.JS.age2529.reg), confint(N14.JS.age2529.reg)))

#30 to 34
sub3034 <- subset(NHIS14_DO, AGE_P == "30 to 34")
N14.JS.age3034.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub3034)
summary(N14.JS.age3034.reg)
exp(cbind(OR = coef(N14.JS.age3034.reg), confint(N14.JS.age3034.reg)))

#35 to 39
sub3539 <- subset(NHIS14_DO, AGE_P == "35 to 39")
N14.JS.age3539.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub3539)
summary(N14.JS.age3539.reg)
exp(cbind(OR = coef(N14.JS.age3539.reg), confint(N14.JS.age3539.reg)))

#40 to 44
sub4044 <- subset(NHIS14_DO, AGE_P == "40 to 44")
N14.JS.age4044.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub4044)
summary(N14.JS.age4044.reg)
exp(cbind(OR = coef(N14.JS.age4044.reg), confint(N14.JS.age4044.reg)))

#45 to 49
sub4549 <- subset(NHIS14_DO, AGE_P == "45 to 49")
N14.JS.age4549.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub4549)
summary(N14.JS.age4549.reg)
exp(cbind(OR = coef(N14.JS.age4549.reg), confint(N14.JS.age4549.reg)))

#50 to 54
sub5054 <- subset(NHIS14_DO, AGE_P =="50 to 54")
N14.JS.age5054.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub5054)
summary(N14.JS.age5054.reg)
exp(cbind(OR = coef(N14.JS.age5054.reg), confint(N14.JS.age5054.reg)))

#55 to 59
sub5559 <- subset(NHIS14_DO, AGE_P == "55 to 59")
N14.JS.age5559.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub5559)
summary(N14.JS.age5559.reg)
exp(cbind(OR = coef(N14.JS.age5559.reg), confint(N14.JS.age5559.reg)))

#60 to 64
sub6064 <- subset(NHIS14_DO, AGE_P == "60 to 64")
N14.JS.age6064.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub6064)
summary(N14.JS.age6064.reg)
exp(cbind(OR = coef(N14.JS.age6064.reg), confint(N14.JS.age6064.reg)))

#65 to 69
sub6569 <- subset(NHIS14_DO, AGE_P == "65 to 69")
N14.JS.age6569.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub6569)
summary(N14.JS.age6569.reg)
exp(cbind(OR = coef(N14.JS.age6569.reg), confint(N14.JS.age6569.reg)))

#70 and above
sub70 <- subset(NHIS14_DO, AGE_P == "70 and above")
N14.JS.age70.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                           family = quasibinomial,
                           design = sub70)
summary(N14.JS.age70.reg)
exp(cbind(OR = coef(N14.JS.age70.reg), confint(N14.JS.age70.reg)))

#III) prevalence by region, by sex

#i)  sex-specific regional prevalence
N14_JS_reg.sex <- svyby(formula = ~JNTSYMP,
                        by = ~REGION + SEX,
                        design = NHIS14_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N14_JS_reg.sex.c <- N14_JS_reg.sex %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Sex", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N14_JS_reg.sex.ci <- confint(N14_JS_reg.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for js = 0
N14_JS_reg.sex.ci <- N14_JS_reg.sex.ci[-c(1:8), ]
#join proportion and ci
N14.JS.sex.region <- bind_cols(N14_JS_reg.sex.c, N14_JS_reg.sex.ci) #final proprtion, se & 95% ci
#save
#write.csv(N14.JS.sex.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N14.JS.region.sex.csv")

#Geofacet - Sex & Region
JS.Sex_Reg14 <- ggplot(N14.JS.sex.region, aes(Sex, Proportion)) +
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
  scale_y_continuous(name = "Prevalence (%)", breaks = c(0, 20, 40, 60), limits = c(0, 65)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 18)) +
  ggtitle("Regional prevalence (%) of US residents with joint symptoms by sex in 2014") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N14.GF_SexReg.JS.png", width = 14, height = 9)


#ii) sex-specific regional logistic regressions
#males
submales <- subset(NHIS14_DO, SEX == "Male")
N14.JS.sexmale.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + AGE_P,
                             family = quasibinomial,
                             design = submales)
summary(N14.JS.sexmale.reg)
exp(cbind(OR = coef(N14.JS.sexmale.reg), confint(N14.JS.sexmale.reg)))

#females
subfemales <- subset(NHIS14_DO, SEX == "Female")
N14.JS.sexfemale.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + AGE_P,
                               family = quasibinomial,
                               design = subfemales)
summary(N14.JS.sexfemale.reg)
exp(cbind(OR = coef(N14.JS.sexfemale.reg), confint(N14.JS.sexfemale.reg)))


#3. Demographic trends

#I)  Age trends

#i)  Prevalence by age
N14_JS_age <- svyby(formula = ~JNTSYMP,
                    by = ~AGE_P,
                    design = NHIS14_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N14_JS_age.c <- N14_JS_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N14_JS_age_ci <- confint(N14_JS_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove js=0 ci
N14_JS_age_ci <- N14_JS_age_ci[-c(1:11), ]
#join proportion and ci
N14_JS.age <- bind_cols(N14_JS_age.c, N14_JS_age_ci) #final proportion, se & 95% ci
#Save
#write.csv(N14_JS.age, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N14.JS.age.csv")


#ii) Logistic regression by age
N14_JS.age_glm <- svyglm(JNTSYMP~AGE_P + SEX,
                         family = quasibinomial,
                         design = NHIS14_DO)
summary(N14_JS.age_glm)
exp(cbind(OR=coef(N14_JS.age_glm), confint(N14_JS.age_glm)))



#II)  Sex trends

#i)  Prevalence by sex
N14_JS_sex <- svyby(formula = ~JNTSYMP,
                    by = ~SEX,
                    design = NHIS14_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N14_JS_sex.c <- N14_JS_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N14_JS_sex_ci <- confint(N14_JS_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N14_JS_sex_ci <- N14_JS_sex_ci[-c(1:2), ]
#join proportion and ci
N14.JS.sex <- bind_cols(N14_JS_sex.c, N14_JS_sex_ci)
#save
#write.csv(N14.JS.sex, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N14.JS.sex.csv")


#ii) Logistic regression by sex
N14.JS.sex.glm.c <- svyglm(JNTSYMP~relevel(SEX, ref = "Male") + AGE_P,
                           family = quasibinomial,
                           design = NHIS14_DO)
summary(N14.JS.sex.glm.c)
exp(cbind(OR=coef(N14.JS.sex.glm.c), confint(N14.JS.sex.glm.c)))


#Prevalence of various age-sex groups and their joint pain locations

#1. Right shoulder
N14_JS.rtshoulder <- svyby(formula = ~JMTHP1,
                           by = ~AGE_P + SEX,
                           design = NHIS14_DO,
                           FUN = svymean,
                           na.rm = TRUE)
N14_JS.rtshoulder.c <- N14_JS.rtshoulder %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.RtShoulder")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N14_JS.rtshoulder.ci <- confint(N14_JS.rtshoulder) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N14_JS.rtshoulder.ci <- N14_JS.rtshoulder.ci[-c(23:44), ]
#join proportion and ci
N14.JS.rtshoulder <- bind_cols(N14_JS.rtshoulder.c, N14_JS.rtshoulder.ci)
#save
#write.csv(N14.JS.rtshoulder, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N14.rtshoulder.csv")

#2. Left Shoulder
N14_JS.ltshoulder <- svyby(formula = ~JMTHP2,
                           by = ~AGE_P + SEX,
                           design = NHIS14_DO,
                           FUN = svymean,
                           na.rm = TRUE)
N14_JS.ltshoulder.c <- N14_JS.ltshoulder %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltshoulder")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N14_JS.ltshoulder.ci <- confint(N14_JS.ltshoulder) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N14_JS.ltshoulder.ci <- N14_JS.ltshoulder.ci[-c(23:44), ]
#join proportion and ci
N14.JS.ltshoulder <- bind_cols(N14_JS.ltshoulder.c, N14_JS.ltshoulder.ci)
#save
#write.csv(N14.JS.ltshoulder, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N14.ltshoulder.csv")


#3. Right elbow
N14_JS.rtelbow <- svyby(formula = ~JMTHP3,
                        by = ~AGE_P + SEX,
                        design = NHIS14_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N14_JS.rtelbow.c <- N14_JS.rtelbow %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtelbow")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N14_JS.rtelbow.ci <- confint(N14_JS.rtelbow) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N14_JS.rtelbow.ci <- N14_JS.rtelbow.ci[-c(23:44), ]
#join proportion and ci
N14.JS.rtelbow <- bind_cols(N14_JS.rtelbow.c, N14_JS.rtelbow.ci)
#save
#write.csv(N14.JS.rtelbow, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N14.rtelbow.csv")

#4. Lt elbow
N14_JS.ltelbow <- svyby(formula = ~JMTHP4,
                        by = ~AGE_P + SEX,
                        design = NHIS14_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N14_JS.ltelbow.c <- N14_JS.ltelbow %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltelbow")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N14_JS.ltelbow.ci <- confint(N14_JS.ltelbow) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N14_JS.ltelbow.ci <- N14_JS.ltelbow.ci[-c(23:44), ]
#join proportion and ci
N14.JS.ltelbow <- bind_cols(N14_JS.ltelbow.c, N14_JS.ltelbow.ci)
#save
#write.csv(N14.JS.ltelbow, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N14.ltelbow.csv")

#5. Right hip
N14_JS.rthip <- svyby(formula = ~JMTHP5,
                      by = ~AGE_P + SEX,
                      design = NHIS14_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N14_JS.rthip.c <- N14_JS.rthip %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rthip")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N14_JS.rthip.ci <- confint(N14_JS.rthip) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N14_JS.rthip.ci <- N14_JS.rthip.ci[-c(23:44), ]
#join proportion and ci
N14.JS.rthip <- bind_cols(N14_JS.rthip.c, N14_JS.rthip.ci)
#save
#write.csv(N14.JS.rthip, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N14.rthip.csv")

#6. Left hip
N14_JS.lthip <- svyby(formula = ~JMTHP6,
                      by = ~AGE_P + SEX,
                      design = NHIS14_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N14_JS.lthip.c <- N14_JS.lthip %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.lthip")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N14_JS.lthip.ci <- confint(N14_JS.lthip) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N14_JS.lthip.ci <- N14_JS.lthip.ci[-c(23:44), ]
#join proportion and ci
N14.JS.lthip <- bind_cols(N14_JS.lthip.c, N14_JS.lthip.ci)
#save
#write.csv(N14.JS.lthip, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N14.lthip.csv")

#7. Right wrist
N14_JS.rtwrist <- svyby(formula = ~JMTHP7,
                        by = ~AGE_P + SEX,
                        design = NHIS14_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N14_JS.rtwrist.c <- N14_JS.rtwrist %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtwrist")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N14_JS.rtwrist.ci <- confint(N14_JS.rtwrist) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N14_JS.rtwrist.ci <- N14_JS.rtwrist.ci[-c(23:44), ]
#join proportion and ci
N14.JS.rtwrist <- bind_cols(N14_JS.rtwrist.c, N14_JS.rtwrist.ci)
#save
#write.csv(N14.JS.rtwrist, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N14.rtwrist.csv")

#8. Left wrist
N14_JS.ltwrist <- svyby(formula = ~JMTHP8,
                        by = ~AGE_P + SEX,
                        design = NHIS14_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N14_JS.ltwrist.c <- N14_JS.ltwrist %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltwrist")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N14_JS.ltwrist.ci <- confint(N14_JS.ltwrist) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N14_JS.ltwrist.ci <- N14_JS.ltwrist.ci[-c(23:44), ]
#join proportion and ci
N14.JS.ltwrist <- bind_cols(N14_JS.ltwrist.c, N14_JS.ltwrist.ci)
#save
#write.csv(N14.JS.ltwrist, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N14.ltwrist.csv")

#9. Right knee
N14_JS.rtknee <- svyby(formula = ~JMTHP9,
                       by = ~AGE_P + SEX,
                       design = NHIS14_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N14_JS.rtknee.c <- N14_JS.rtknee %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtknee")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N14_JS.rtknee.ci <- confint(N14_JS.rtknee) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N14_JS.rtknee.ci <- N14_JS.rtknee.ci[-c(23:44), ]
#join proportion and ci
N14.JS.rtknee <- bind_cols(N14_JS.rtknee.c, N14_JS.rtknee.ci)
#save
#write.csv(N14.JS.rtknee, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N14.rtknee.csv")

#10. Left knee
N14_JS.ltknee <- svyby(formula = ~JMTHP10,
                       by = ~AGE_P + SEX,
                       design = NHIS14_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N14_JS.ltknee.c <- N14_JS.ltknee %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltknee")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N14_JS.ltknee.ci <- confint(N14_JS.ltknee) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N14_JS.ltknee.ci <- N14_JS.ltknee.ci[-c(23:44), ]
#join proportion and ci
N14.JS.ltknee <- bind_cols(N14_JS.ltknee.c, N14_JS.ltknee.ci)
#save
#write.csv(N14.JS.ltknee, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N14.ltknee.csv")

#11. Right ankle
N14_JS.rtankle <- svyby(formula = ~JMTHP11,
                        by = ~AGE_P + SEX,
                        design = NHIS14_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N14_JS.rtankle.c <- N14_JS.rtankle %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtankle")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N14_JS.rtankle.ci <- confint(N14_JS.rtankle) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N14_JS.rtankle.ci <- N14_JS.rtankle.ci[-c(23:44), ]
#join proportion and ci
N14.JS.rtankle <- bind_cols(N14_JS.rtankle.c, N14_JS.rtankle.ci)
#save
#write.csv(N14.JS.rtankle, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N14.rtankle.csv")

#12. Left ankle
N14_JS.ltankle <- svyby(formula = ~JMTHP12,
                        by = ~AGE_P + SEX,
                        design = NHIS14_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N14_JS.ltankle.c <- N14_JS.ltankle %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltankle")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N14_JS.ltankle.ci <- confint(N14_JS.ltankle) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N14_JS.ltankle.ci <- N14_JS.ltankle.ci[-c(23:44), ]
#join proportion and ci
N14.JS.ltankle <- bind_cols(N14_JS.ltankle.c, N14_JS.ltankle.ci)
#save
#write.csv(N14.JS.ltankle, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N14.ltankle.csv")

#13. Right toes
N14_JS.rttoes <- svyby(formula = ~JMTHP13,
                       by = ~AGE_P + SEX,
                       design = NHIS14_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N14_JS.rttoes.c <- N14_JS.rttoes %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rttoes")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N14_JS.rttoes.ci <- confint(N14_JS.rttoes) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N14_JS.rttoes.ci <- N14_JS.rttoes.ci[-c(23:44), ]
#join proportion and ci
N14.JS.rttoes <- bind_cols(N14_JS.rttoes.c, N14_JS.rttoes.ci)
#save
#write.csv(N14.JS.rttoes, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N14.rttoes.csv")

#14. Left toes
N14_JS.lttoes <- svyby(formula = ~JMTHP14,
                       by = ~AGE_P + SEX,
                       design = NHIS14_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N14_JS.lttoes.c <- N14_JS.lttoes %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.lttoes")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N14_JS.lttoes.ci <- confint(N14_JS.lttoes) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N14_JS.lttoes.ci <- N14_JS.lttoes.ci[-c(23:44), ]
#join proportion and ci
N14.JS.lttoes <- bind_cols(N14_JS.lttoes.c, N14_JS.lttoes.ci)
#save
#write.csv(N14.JS.lttoes, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N14.lttoes.csv")

#15. Right fingers/thumb
N14_JS.rtfingthumb <- svyby(formula = ~JMTHP15,
                            by = ~AGE_P + SEX,
                            design = NHIS14_DO,
                            FUN = svymean,
                            na.rm = TRUE)
N14_JS.rtfingthumb.c <- N14_JS.rtfingthumb %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtfingthumb")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N14_JS.rtfingthumb.ci <- confint(N14_JS.rtfingthumb) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N14_JS.rtfingthumb.ci <- N14_JS.rtfingthumb.ci[-c(23:44), ]
#join proportion and ci
N14.JS.rtfingthumb <- bind_cols(N14_JS.rtfingthumb.c, N14_JS.rtfingthumb.ci)
#save
#write.csv(N14.JS.rtfingthumb, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N14.rtfingthumb.csv")

#16. Left fingers/thumb
N14_JS.ltfingthumb <- svyby(formula = ~JMTHP16,
                            by = ~AGE_P + SEX,
                            design = NHIS14_DO,
                            FUN = svymean,
                            na.rm = TRUE)
N14_JS.ltfingthumb.c <- N14_JS.ltfingthumb %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltfingthumb")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N14_JS.ltfingthumb.ci <- confint(N14_JS.ltfingthumb) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N14_JS.ltfingthumb.ci <- N14_JS.ltfingthumb.ci[-c(23:44), ]
#join proportion and ci
N14.JS.ltfingthumb <- bind_cols(N14_JS.ltfingthumb.c, N14_JS.ltfingthumb.ci)
#save
#write.csv(N14.JS.ltfingthumb, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N14.ltfingthumb.csv")

#17. Another
N14_JS.another <- svyby(formula = ~JMTHP17,
                        by = ~AGE_P + SEX,
                        design = NHIS14_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N14_JS.another.c <- N14_JS.another %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.another")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N14_JS.another.ci <- confint(N14_JS.another) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N14_JS.another.ci <- N14_JS.another.ci[-c(23:44), ]
#join proportion and ci
N14.JS.another <- bind_cols(N14_JS.another.c, N14_JS.another.ci)
#save
#write.csv(N14.JS.another, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N14.another.csv")

N14.JSloc <- Reduce(function(x, y) merge(x, y, all = TRUE), list(N14_JS.rtshoulder.c, N14_JS.ltshoulder.c, N14_JS.rtelbow.c, N14_JS.ltelbow.c, N14_JS.rthip.c, N14_JS.lthip.c, N14_JS.rtwrist.c, N14_JS.ltwrist.c, N14_JS.rtknee.c, N14_JS.ltknee.c, N14_JS.rtankle.c, N14_JS.ltankle.c, N14_JS.rttoes.c, N14_JS.lttoes.c, N14_JS.rtfingthumb.c, N14_JS.ltfingthumb.c, N14_JS.another.c))

#Wide to long
N14.JSlocation <- gather(N14.JSloc, Location, Proportion, Proportion.RtShoulder:Proportion.another, factor_key = TRUE)
N14.JSlocation <- N14.JSlocation %>% 
  unite("Demographic", Age:Sex)
N14.JSlocation$Location <- as.character(N14.JSlocation$Location)
N14.JSlocation$Location[N14.JSlocation$Location == "Proportion.RtShoulder"] <- "Right shoulder"
N14.JSlocation$Location[N14.JSlocation$Location == "Proportion.ltshoulder"] <- "Left shoulder"
N14.JSlocation$Location[N14.JSlocation$Location == "Proportion.rtelbow"] <- "Right elbow"
N14.JSlocation$Location[N14.JSlocation$Location == "Proportion.ltelbow"] <- "Left elbow"
N14.JSlocation$Location[N14.JSlocation$Location == "Proportion.rthip"] <- "Right hip"
N14.JSlocation$Location[N14.JSlocation$Location == "Proportion.lthip"] <- "Left hip"
N14.JSlocation$Location[N14.JSlocation$Location == "Proportion.rtwrist"] <- "Right wrist"
N14.JSlocation$Location[N14.JSlocation$Location == "Proportion.ltwrist"] <- "Left wrist"
N14.JSlocation$Location[N14.JSlocation$Location == "Proportion.rtknee"] <- "Right knee"
N14.JSlocation$Location[N14.JSlocation$Location == "Proportion.ltknee"] <- "Left knee"
N14.JSlocation$Location[N14.JSlocation$Location == "Proportion.rtankle"] <- "Right ankle"
N14.JSlocation$Location[N14.JSlocation$Location == "Proportion.ltankle"] <- "Left ankle"
N14.JSlocation$Location[N14.JSlocation$Location == "Proportion.rttoes"] <- "Right toes"
N14.JSlocation$Location[N14.JSlocation$Location == "Proportion.lttoes"] <- "Left toes"
N14.JSlocation$Location[N14.JSlocation$Location == "Proportion.rtfingthumb"] <- "Right fingers or thumb"
N14.JSlocation$Location[N14.JSlocation$Location == "Proportion.ltfingthumb"] <- "Left fingers or thumb"
N14.JSlocation$Location[N14.JSlocation$Location == "Proportion.another"] <- "Another joint"

#Heatmap of joint locations by demographic groups
N14.JSlocmap <- ggplot(N14.JSlocation,
                       aes(x = Demographic, y = Location, fill = Proportion)) +
  geom_tile() + 
  scale_fill_viridis(option = "viridis", direction = -1, limits = c(0, 60), breaks = c(10, 20, 30, 40, 50, 60), discrete = FALSE) +
  theme_bw() + 
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.text.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  xlab("Age-sex profile") +
  theme(legend.position = "right",
        strip.background = element_blank()) +
  theme(legend.title = element_text(size = 26)) +
  theme(legend.text = element_text(size = 22)) +
  labs(fill = "Prevalence (%)") +
  guides(fill = guide_colourbar(barwidth = 3, barheight = 25)) +
  scale_x_discrete(labels = c("Females, 18 to 24", "Males, 18 to 24", "Females, 25 to 29", "Males, 25 to 29", "Females, 30 to 34", "Males, 30 to 34", "Females, 35 to 39", "Males, 35 to 39", "Females, 40 to 44", "Males, 40 to 44", "Females, 45 to 49", "Males, 45 to 49", "Females, 50 to 54", "Males, 50 to 54", "Females, 55 to 59", "Males, 55 to 59", "Females, 60 to 64", "Males, 60 to 64", "Females, 65 to 69", "Males, 65 to 69", "Females, 70 and above", "Males, 70 and above")) +
  scale_y_discrete(limits = c("Another joint", "Right toes", "Left toes", "Right ankle", "Left ankle", "Right knee", "Left knee", "Right hip", "Left hip", "Right shoulder", "Left shoulder", "Right elbow", "Left elbow", "Right wrist", "Left wrist", "Right fingers or thumb", "Left fingers or thumb"))
ggsave("N14.age-sex.png", width = 20, height = 13)



#III) Socioeconomic trends (employment status only)

#i)   Prevalence by employment status
N14_JS_emp <- svyby(formula = ~JNTSYMP,
                    by = ~DOINGLWA,
                    design = NHIS14_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N14_JS_emp.c <- N14_JS_emp %>%
  select(1, 3, 5) %>%
  setNames(c("Employment", "Proportion", "SE")) %>%
  mutate(Employment = as.factor(Employment)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N14_JS_emp_ci <- confint(N14_JS_emp) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for js = 0
N14_JS_emp_ci <- N14_JS_emp_ci[-c(1:2), ]
#join ci and proportion
N14.JS.emp <- bind_cols(N14_JS_emp.c, N14_JS_emp_ci) #final proportion, se & 95% ci
#save
#write.csv(N14.JS.emp, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N14.JS.emp.csv")

#ii)  Logistic regression by employment status
N14_JS.emp_glm <- svyglm(JNTSYMP~relevel(DOINGLWA, ref = "Working for pay at a job/business") + AGE_P + SEX,
                         family = quasibinomial,
                         design = NHIS14_DO)
summary(N14_JS.emp_glm)
exp(cbind(OR=coef(N14_JS.emp_glm), confint(N14_JS.emp_glm)))

#4. Lifestyle trends

#I)  BMI
#logistic regression
N14.BMI.glm <- svyglm(JNTSYMP ~ BMI + AGE_P + SEX,
                      family = quasibinomial,
                      design = NHIS14_DO)
summary(N14.BMI.glm)
exp(cbind(OR=coef(N14.BMI.glm), confint(N14.BMI.glm)))

#II) Physical activity
#logistic regression
N14.PA.JS.glm <- svyglm(JNTSYMP ~ relevel(factor(PAC, ordered = FALSE), ref = "Physically active") + AGE_P + SEX,
                        family = quasibinomial,
                        design = NHIS14_DO)
summary(N14.PA.JS.glm)
exp(cbind(OR = coef(N14.PA.JS.glm), confint(N14.PA.JS.glm)))


#____________________________________________________________________________________________________________________

# == 2013 == #

#1. Overall prevalence
N13_JS <- svymean(~factor(JNTSYMP), 
                  NHIS13_DO, 
                  na.rm = TRUE)

N13_JS.c <- N13_JS %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N13_JS_ci <- confint(N13_JS) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
N13.JS <- bind_cols(N13_JS.c, N13_JS_ci)
#remove js = 0
N13.JS <- N13.JS[-c(1), ] #final proportion, se & 95%
#save
#write.csv(N13.JS, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N13.JS.csv")

#2. Spatial analysis, by region

#I)   Overall population

#i)   regional prevalence for the overall population
N13_JS_reg <- svyby(formula = ~JNTSYMP,
                    by = ~REGION,
                    design = NHIS13_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N13_JS_reg.c <- N13_JS_reg %>%
  select(1, 3, 5) %>%
  setNames(c("Region", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N13_JS_reg.ci <- confint(N13_JS_reg) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for js = 0
N13_JS_reg.ci <- N13_JS_reg.ci[-c(1:4), ]
#join proportion and ci
N13.JS.region <- bind_cols(N13_JS_reg.c, N13_JS_reg.ci) #final proportion, se & 95% ci
#save
#write.csv(N13.JS.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N13.JS.region.csv")

#Create overall regional prevalence

#Change the region column to "NAME" to match sf
names(N13.JS.region)[names(N13.JS.region) == "Region"] <- "NAME"

N13.JS.joined <- regions %>%
  left_join(N13.JS.region)

N13.JS.joined$NAME <- as.factor(N13.JS.joined$NAME)

#Plot
JSregplot <- ggplot(data = N13.JS.joined) +
  geom_sf(aes(fill = Proportion), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  scale_fill_viridis_c(name = "Prevalence (%)", option = "viridis", direction = -1, limits = c(25, 40), breaks = c(25, 30, 35, 40)) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  ggtitle("Regional prevalence (%) of US residents with joint symptoms in 2013") +
  theme(plot.title = element_text(size = 10, hjust = 0.5))
ggsave("N13.JSregchor.png", width = 5, height = 5)


#ii)  logistic regression by region
N13.JS.reg.glm <- svyglm(relevel(JNTSYMP, ref = "0")~ REGION + AGE_P,
                         family = quasibinomial,
                         design = NHIS13_DO)
summary(N13.JS.reg.glm)
exp(cbind(OR=coef(N13.JS.reg.glm), confint(N13.JS.reg.glm)))

#II)  prevalence by region, by age

#i)  age-specific regional prevalance
N13_JS_reg.age <- svyby(formula = ~JNTSYMP,
                        by = ~REGION + AGE_P,
                        design = NHIS13_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N13_JS_reg.age.c <- N13_JS_reg.age %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Age", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Age = as.character(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N13_JS_reg.age.ci <- confint(N13_JS_reg.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for js = 0
N13_JS_reg.age.ci <- N13_JS_reg.age.ci[-c(1:44), ]
#join proportion and ci
N13.JS.age.region <- bind_cols(N13_JS_reg.age.c, N13_JS_reg.age.ci) #final proportion, se & 95% ci
#save
#write.csv(N13.JS.age.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N13.JS.region.age.csv")

#Geofacet plot - Age & Region
JS.Age_Reg <- ggplot(N13.JS.age.region, aes(Age, Proportion)) +
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
  ggtitle("Regional prevalence (%) of US residents with joint symptoms by age group in 2013") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N13.GF_AgeReg.JS.png", width = 14, height = 9)


#ii) age-specific regional logistic regressions
#18 to 24
sub1824 <- subset(NHIS13_DO, AGE_P == "18 to 24")
N13.JS.age1824.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub1824)
summary(N13.JS.age1824.reg)
exp(cbind(OR = coef(N13.JS.age1824.reg), confint(N13.JS.age1824.reg)))

#25 to 29
sub2529 <- subset(NHIS13_DO, AGE_P == "25 to 29")
N13.JS.age2529.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub2529)
summary(N13.JS.age2529.reg)
exp(cbind(OR = coef(N13.JS.age2529.reg), confint(N13.JS.age2529.reg)))

#30 to 34
sub3034 <- subset(NHIS13_DO, AGE_P == "30 to 34")
N13.JS.age3034.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub3034)
summary(N13.JS.age3034.reg)
exp(cbind(OR = coef(N13.JS.age3034.reg), confint(N13.JS.age3034.reg)))

#35 to 39
sub3539 <- subset(NHIS13_DO, AGE_P == "35 to 39")
N13.JS.age3539.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub3539)
summary(N13.JS.age3539.reg)
exp(cbind(OR = coef(N13.JS.age3539.reg), confint(N13.JS.age3539.reg)))

#40 to 44
sub4044 <- subset(NHIS13_DO, AGE_P == "40 to 44")
N13.JS.age4044.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub4044)
summary(N13.JS.age4044.reg)
exp(cbind(OR = coef(N13.JS.age4044.reg), confint(N13.JS.age4044.reg)))

#45 to 49
sub4549 <- subset(NHIS13_DO, AGE_P == "45 to 49")
N13.JS.age4549.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub4549)
summary(N13.JS.age4549.reg)
exp(cbind(OR = coef(N13.JS.age4549.reg), confint(N13.JS.age4549.reg)))

#50 to 54
sub5054 <- subset(NHIS13_DO, AGE_P =="50 to 54")
N13.JS.age5054.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub5054)
summary(N13.JS.age5054.reg)
exp(cbind(OR = coef(N13.JS.age5054.reg), confint(N13.JS.age5054.reg)))

#55 to 59
sub5559 <- subset(NHIS13_DO, AGE_P == "55 to 59")
N13.JS.age5559.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub5559)
summary(N13.JS.age5559.reg)
exp(cbind(OR = coef(N13.JS.age5559.reg), confint(N13.JS.age5559.reg)))

#60 to 64
sub6064 <- subset(NHIS13_DO, AGE_P == "60 to 64")
N13.JS.age6064.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub6064)
summary(N13.JS.age6064.reg)
exp(cbind(OR = coef(N13.JS.age6064.reg), confint(N13.JS.age6064.reg)))

#65 to 69
sub6569 <- subset(NHIS13_DO, AGE_P == "65 to 69")
N13.JS.age6569.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub6569)
summary(N13.JS.age6569.reg)
exp(cbind(OR = coef(N13.JS.age6569.reg), confint(N13.JS.age6569.reg)))

#70 and above
sub70 <- subset(NHIS13_DO, AGE_P == "70 and above")
N13.JS.age70.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                           family = quasibinomial,
                           design = sub70)
summary(N13.JS.age70.reg)
exp(cbind(OR = coef(N13.JS.age70.reg), confint(N13.JS.age70.reg)))

#III) prevalence by region, by sex

#i)  sex-specific regional prevalence
N13_JS_reg.sex <- svyby(formula = ~JNTSYMP,
                        by = ~REGION + SEX,
                        design = NHIS13_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N13_JS_reg.sex.c <- N13_JS_reg.sex %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Sex", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N13_JS_reg.sex.ci <- confint(N13_JS_reg.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for js = 0
N13_JS_reg.sex.ci <- N13_JS_reg.sex.ci[-c(1:8), ]
#join proportion and ci
N13.JS.sex.region <- bind_cols(N13_JS_reg.sex.c, N13_JS_reg.sex.ci) #final proprtion, se & 95% ci
#save
#write.csv(N13.JS.sex.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N13.JS.region.sex.csv")

#Geofacet - Sex & Region
JS.Sex_Reg13 <- ggplot(N13.JS.sex.region, aes(Sex, Proportion)) +
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
  ggtitle("Regional prevalence (%) of US residents with joint symptoms by sex in 2013") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N13.GF_SexReg.JS.png", width = 14, height = 9)


#ii) sex-specific regional logistic regressions
#males
submales <- subset(NHIS13_DO, SEX == "Male")
N13.JS.sexmale.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + AGE_P,
                             family = quasibinomial,
                             design = submales)
summary(N13.JS.sexmale.reg)
exp(cbind(OR = coef(N13.JS.sexmale.reg), confint(N13.JS.sexmale.reg)))

#females
subfemales <- subset(NHIS13_DO, SEX == "Female")
N13.JS.sexfemale.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + AGE_P,
                               family = quasibinomial,
                               design = subfemales)
summary(N13.JS.sexfemale.reg)
exp(cbind(OR = coef(N13.JS.sexfemale.reg), confint(N13.JS.sexfemale.reg)))


#3. Demographic trends

#I)  Age trends

#i)  Prevalence by age
N13_JS_age <- svyby(formula = ~JNTSYMP,
                    by = ~AGE_P,
                    design = NHIS13_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N13_JS_age.c <- N13_JS_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N13_JS_age_ci <- confint(N13_JS_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove js=0 ci
N13_JS_age_ci <- N13_JS_age_ci[-c(1:11), ]
#join proportion and ci
N13_JS.age <- bind_cols(N13_JS_age.c, N13_JS_age_ci) #final proportion, se & 95% ci
#Save
#write.csv(N13_JS.age, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N13.JS.age.csv")


#ii) Logistic regression by age
N13_JS.age_glm <- svyglm(JNTSYMP~AGE_P + SEX,
                         family = quasibinomial,
                         design = NHIS13_DO)
summary(N13_JS.age_glm)
exp(cbind(OR=coef(N13_JS.age_glm), confint(N13_JS.age_glm)))



#II)  Sex trends

#i)  Prevalence by sex
N13_JS_sex <- svyby(formula = ~JNTSYMP,
                    by = ~SEX,
                    design = NHIS13_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N13_JS_sex.c <- N13_JS_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N13_JS_sex_ci <- confint(N13_JS_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N13_JS_sex_ci <- N13_JS_sex_ci[-c(1:2), ]
#join proportion and ci
N13.JS.sex <- bind_cols(N13_JS_sex.c, N13_JS_sex_ci)
#save
#write.csv(N13.JS.sex, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N13.JS.sex.csv")


#ii) Logistic regression by sex
N13.JS.sex.glm.c <- svyglm(JNTSYMP~relevel(SEX, ref = "Male") + AGE_P,
                           family = quasibinomial,
                           design = NHIS13_DO)
summary(N13.JS.sex.glm.c)
exp(cbind(OR=coef(N13.JS.sex.glm.c), confint(N13.JS.sex.glm.c)))


#Prevalence of various age-sex groups and their joint pain locations

#1. Right shoulder
N13_JS.rtshoulder <- svyby(formula = ~JMTHP1,
                           by = ~AGE_P + SEX,
                           design = NHIS13_DO,
                           FUN = svymean,
                           na.rm = TRUE)
N13_JS.rtshoulder.c <- N13_JS.rtshoulder %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.RtShoulder")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N13_JS.rtshoulder.ci <- confint(N13_JS.rtshoulder) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N13_JS.rtshoulder.ci <- N13_JS.rtshoulder.ci[-c(23:44), ]
#join proportion and ci
N13.JS.rtshoulder <- bind_cols(N13_JS.rtshoulder.c, N13_JS.rtshoulder.ci)
#save
#write.csv(N13.JS.rtshoulder, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N13.rtshoulder.csv")

#2. Left Shoulder
N13_JS.ltshoulder <- svyby(formula = ~JMTHP2,
                           by = ~AGE_P + SEX,
                           design = NHIS13_DO,
                           FUN = svymean,
                           na.rm = TRUE)
N13_JS.ltshoulder.c <- N13_JS.ltshoulder %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltshoulder")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N13_JS.ltshoulder.ci <- confint(N13_JS.ltshoulder) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N13_JS.ltshoulder.ci <- N13_JS.ltshoulder.ci[-c(23:44), ]
#join proportion and ci
N13.JS.ltshoulder <- bind_cols(N13_JS.ltshoulder.c, N13_JS.ltshoulder.ci)
#save
#write.csv(N13.JS.ltshoulder, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N13.ltshoulder.csv")


#3. Right elbow
N13_JS.rtelbow <- svyby(formula = ~JMTHP3,
                        by = ~AGE_P + SEX,
                        design = NHIS13_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N13_JS.rtelbow.c <- N13_JS.rtelbow %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtelbow")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N13_JS.rtelbow.ci <- confint(N13_JS.rtelbow) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N13_JS.rtelbow.ci <- N13_JS.rtelbow.ci[-c(23:44), ]
#join proportion and ci
N13.JS.rtelbow <- bind_cols(N13_JS.rtelbow.c, N13_JS.rtelbow.ci)
#save
#write.csv(N13.JS.rtelbow, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N13.rtelbow.csv")

#4. Lt elbow
N13_JS.ltelbow <- svyby(formula = ~JMTHP4,
                        by = ~AGE_P + SEX,
                        design = NHIS13_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N13_JS.ltelbow.c <- N13_JS.ltelbow %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltelbow")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N13_JS.ltelbow.ci <- confint(N13_JS.ltelbow) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N13_JS.ltelbow.ci <- N13_JS.ltelbow.ci[-c(23:44), ]
#join proportion and ci
N13.JS.ltelbow <- bind_cols(N13_JS.ltelbow.c, N13_JS.ltelbow.ci)
#save
#write.csv(N13.JS.ltelbow, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N13.ltelbow.csv")

#5. Right hip
N13_JS.rthip <- svyby(formula = ~JMTHP5,
                      by = ~AGE_P + SEX,
                      design = NHIS13_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N13_JS.rthip.c <- N13_JS.rthip %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rthip")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N13_JS.rthip.ci <- confint(N13_JS.rthip) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N13_JS.rthip.ci <- N13_JS.rthip.ci[-c(23:44), ]
#join proportion and ci
N13.JS.rthip <- bind_cols(N13_JS.rthip.c, N13_JS.rthip.ci)
#save
#write.csv(N13.JS.rthip, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N13.rthip.csv")

#6. Left hip
N13_JS.lthip <- svyby(formula = ~JMTHP6,
                      by = ~AGE_P + SEX,
                      design = NHIS13_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N13_JS.lthip.c <- N13_JS.lthip %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.lthip")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N13_JS.lthip.ci <- confint(N13_JS.lthip) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N13_JS.lthip.ci <- N13_JS.lthip.ci[-c(23:44), ]
#join proportion and ci
N13.JS.lthip <- bind_cols(N13_JS.lthip.c, N13_JS.lthip.ci)
#save
#write.csv(N13.JS.lthip, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N13.lthip.csv")

#7. Right wrist
N13_JS.rtwrist <- svyby(formula = ~JMTHP7,
                        by = ~AGE_P + SEX,
                        design = NHIS13_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N13_JS.rtwrist.c <- N13_JS.rtwrist %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtwrist")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N13_JS.rtwrist.ci <- confint(N13_JS.rtwrist) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N13_JS.rtwrist.ci <- N13_JS.rtwrist.ci[-c(23:44), ]
#join proportion and ci
N13.JS.rtwrist <- bind_cols(N13_JS.rtwrist.c, N13_JS.rtwrist.ci)
#save
#write.csv(N13.JS.rtwrist, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N13.rtwrist.csv")

#8. Left wrist
N13_JS.ltwrist <- svyby(formula = ~JMTHP8,
                        by = ~AGE_P + SEX,
                        design = NHIS13_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N13_JS.ltwrist.c <- N13_JS.ltwrist %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltwrist")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N13_JS.ltwrist.ci <- confint(N13_JS.ltwrist) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N13_JS.ltwrist.ci <- N13_JS.ltwrist.ci[-c(23:44), ]
#join proportion and ci
N13.JS.ltwrist <- bind_cols(N13_JS.ltwrist.c, N13_JS.ltwrist.ci)
#save
#write.csv(N13.JS.ltwrist, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N13.ltwrist.csv")

#9. Right knee
N13_JS.rtknee <- svyby(formula = ~JMTHP9,
                       by = ~AGE_P + SEX,
                       design = NHIS13_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N13_JS.rtknee.c <- N13_JS.rtknee %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtknee")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N13_JS.rtknee.ci <- confint(N13_JS.rtknee) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N13_JS.rtknee.ci <- N13_JS.rtknee.ci[-c(23:44), ]
#join proportion and ci
N13.JS.rtknee <- bind_cols(N13_JS.rtknee.c, N13_JS.rtknee.ci)
#save
#write.csv(N13.JS.rtknee, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N13.rtknee.csv")

#10. Left knee
N13_JS.ltknee <- svyby(formula = ~JMTHP10,
                       by = ~AGE_P + SEX,
                       design = NHIS13_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N13_JS.ltknee.c <- N13_JS.ltknee %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltknee")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N13_JS.ltknee.ci <- confint(N13_JS.ltknee) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N13_JS.ltknee.ci <- N13_JS.ltknee.ci[-c(23:44), ]
#join proportion and ci
N13.JS.ltknee <- bind_cols(N13_JS.ltknee.c, N13_JS.ltknee.ci)
#save
#write.csv(N13.JS.ltknee, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N13.ltknee.csv")

#11. Right ankle
N13_JS.rtankle <- svyby(formula = ~JMTHP11,
                        by = ~AGE_P + SEX,
                        design = NHIS13_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N13_JS.rtankle.c <- N13_JS.rtankle %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtankle")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N13_JS.rtankle.ci <- confint(N13_JS.rtankle) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N13_JS.rtankle.ci <- N13_JS.rtankle.ci[-c(23:44), ]
#join proportion and ci
N13.JS.rtankle <- bind_cols(N13_JS.rtankle.c, N13_JS.rtankle.ci)
#save
#write.csv(N13.JS.rtankle, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N13.rtankle.csv")

#12. Left ankle
N13_JS.ltankle <- svyby(formula = ~JMTHP12,
                        by = ~AGE_P + SEX,
                        design = NHIS13_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N13_JS.ltankle.c <- N13_JS.ltankle %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltankle")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N13_JS.ltankle.ci <- confint(N13_JS.ltankle) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N13_JS.ltankle.ci <- N13_JS.ltankle.ci[-c(23:44), ]
#join proportion and ci
N13.JS.ltankle <- bind_cols(N13_JS.ltankle.c, N13_JS.ltankle.ci)
#save
#write.csv(N13.JS.ltankle, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N13.ltankle.csv")

#13. Right toes
N13_JS.rttoes <- svyby(formula = ~JMTHP13,
                       by = ~AGE_P + SEX,
                       design = NHIS13_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N13_JS.rttoes.c <- N13_JS.rttoes %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rttoes")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N13_JS.rttoes.ci <- confint(N13_JS.rttoes) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N13_JS.rttoes.ci <- N13_JS.rttoes.ci[-c(23:44), ]
#join proportion and ci
N13.JS.rttoes <- bind_cols(N13_JS.rttoes.c, N13_JS.rttoes.ci)
#save
#write.csv(N13.JS.rttoes, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N13.rttoes.csv")

#14. Left toes
N13_JS.lttoes <- svyby(formula = ~JMTHP14,
                       by = ~AGE_P + SEX,
                       design = NHIS13_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N13_JS.lttoes.c <- N13_JS.lttoes %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.lttoes")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N13_JS.lttoes.ci <- confint(N13_JS.lttoes) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N13_JS.lttoes.ci <- N13_JS.lttoes.ci[-c(23:44), ]
#join proportion and ci
N13.JS.lttoes <- bind_cols(N13_JS.lttoes.c, N13_JS.lttoes.ci)
#save
#write.csv(N13.JS.lttoes, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N13.lttoes.csv")

#15. Right fingers/thumb
N13_JS.rtfingthumb <- svyby(formula = ~JMTHP15,
                            by = ~AGE_P + SEX,
                            design = NHIS13_DO,
                            FUN = svymean,
                            na.rm = TRUE)
N13_JS.rtfingthumb.c <- N13_JS.rtfingthumb %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtfingthumb")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N13_JS.rtfingthumb.ci <- confint(N13_JS.rtfingthumb) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N13_JS.rtfingthumb.ci <- N13_JS.rtfingthumb.ci[-c(23:44), ]
#join proportion and ci
N13.JS.rtfingthumb <- bind_cols(N13_JS.rtfingthumb.c, N13_JS.rtfingthumb.ci)
#save
#write.csv(N13.JS.rtfingthumb, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N13.rtfingthumb.csv")

#16. Left fingers/thumb
N13_JS.ltfingthumb <- svyby(formula = ~JMTHP16,
                            by = ~AGE_P + SEX,
                            design = NHIS13_DO,
                            FUN = svymean,
                            na.rm = TRUE)
N13_JS.ltfingthumb.c <- N13_JS.ltfingthumb %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltfingthumb")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N13_JS.ltfingthumb.ci <- confint(N13_JS.ltfingthumb) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N13_JS.ltfingthumb.ci <- N13_JS.ltfingthumb.ci[-c(23:44), ]
#join proportion and ci
N13.JS.ltfingthumb <- bind_cols(N13_JS.ltfingthumb.c, N13_JS.ltfingthumb.ci)
#save
#write.csv(N13.JS.ltfingthumb, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N13.ltfingthumb.csv")

#17. Another
N13_JS.another <- svyby(formula = ~JMTHP17,
                        by = ~AGE_P + SEX,
                        design = NHIS13_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N13_JS.another.c <- N13_JS.another %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.another")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N13_JS.another.ci <- confint(N13_JS.another) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N13_JS.another.ci <- N13_JS.another.ci[-c(23:44), ]
#join proportion and ci
N13.JS.another <- bind_cols(N13_JS.another.c, N13_JS.another.ci)
#save
#write.csv(N13.JS.another, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N13.another.csv")

N13.JSloc <- Reduce(function(x, y) merge(x, y, all = TRUE), list(N13_JS.rtshoulder.c, N13_JS.ltshoulder.c, N13_JS.rtelbow.c, N13_JS.ltelbow.c, N13_JS.rthip.c, N13_JS.lthip.c, N13_JS.rtwrist.c, N13_JS.ltwrist.c, N13_JS.rtknee.c, N13_JS.ltknee.c, N13_JS.rtankle.c, N13_JS.ltankle.c, N13_JS.rttoes.c, N13_JS.lttoes.c, N13_JS.rtfingthumb.c, N13_JS.ltfingthumb.c, N13_JS.another.c))

#Wide to long
N13.JSlocation <- gather(N13.JSloc, Location, Proportion, Proportion.RtShoulder:Proportion.another, factor_key = TRUE)
N13.JSlocation <- N13.JSlocation %>% 
  unite("Demographic", Age:Sex)
N13.JSlocation$Location <- as.character(N13.JSlocation$Location)
N13.JSlocation$Location[N13.JSlocation$Location == "Proportion.RtShoulder"] <- "Right shoulder"
N13.JSlocation$Location[N13.JSlocation$Location == "Proportion.ltshoulder"] <- "Left shoulder"
N13.JSlocation$Location[N13.JSlocation$Location == "Proportion.rtelbow"] <- "Right elbow"
N13.JSlocation$Location[N13.JSlocation$Location == "Proportion.ltelbow"] <- "Left elbow"
N13.JSlocation$Location[N13.JSlocation$Location == "Proportion.rthip"] <- "Right hip"
N13.JSlocation$Location[N13.JSlocation$Location == "Proportion.lthip"] <- "Left hip"
N13.JSlocation$Location[N13.JSlocation$Location == "Proportion.rtwrist"] <- "Right wrist"
N13.JSlocation$Location[N13.JSlocation$Location == "Proportion.ltwrist"] <- "Left wrist"
N13.JSlocation$Location[N13.JSlocation$Location == "Proportion.rtknee"] <- "Right knee"
N13.JSlocation$Location[N13.JSlocation$Location == "Proportion.ltknee"] <- "Left knee"
N13.JSlocation$Location[N13.JSlocation$Location == "Proportion.rtankle"] <- "Right ankle"
N13.JSlocation$Location[N13.JSlocation$Location == "Proportion.ltankle"] <- "Left ankle"
N13.JSlocation$Location[N13.JSlocation$Location == "Proportion.rttoes"] <- "Right toes"
N13.JSlocation$Location[N13.JSlocation$Location == "Proportion.lttoes"] <- "Left toes"
N13.JSlocation$Location[N13.JSlocation$Location == "Proportion.rtfingthumb"] <- "Right fingers or thumb"
N13.JSlocation$Location[N13.JSlocation$Location == "Proportion.ltfingthumb"] <- "Left fingers or thumb"
N13.JSlocation$Location[N13.JSlocation$Location == "Proportion.another"] <- "Another joint"

#Heatmap of joint locations by demographic groups
N13.JSlocmap <- ggplot(N13.JSlocation,
                       aes(x = Demographic, y = Location, fill = Proportion)) +
  geom_tile() + 
  scale_fill_viridis(option = "viridis", direction = -1, limits = c(0, 60), breaks = c(10, 20, 30, 40, 50, 60), discrete = FALSE) +
  theme_bw() + 
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.text.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  xlab("Age-sex profile") +
  theme(legend.position = "right",
        strip.background = element_blank()) +
  theme(legend.title = element_text(size = 26)) +
  theme(legend.text = element_text(size = 22)) +
  labs(fill = "Prevalence (%)") +
  guides(fill = guide_colourbar(barwidth = 3, barheight = 25)) +
  scale_x_discrete(labels = c("Females, 18 to 24", "Males, 18 to 24", "Females, 25 to 29", "Males, 25 to 29", "Females, 30 to 34", "Males, 30 to 34", "Females, 35 to 39", "Males, 35 to 39", "Females, 40 to 44", "Males, 40 to 44", "Females, 45 to 49", "Males, 45 to 49", "Females, 50 to 54", "Males, 50 to 54", "Females, 55 to 59", "Males, 55 to 59", "Females, 60 to 64", "Males, 60 to 64", "Females, 65 to 69", "Males, 65 to 69", "Females, 70 and above", "Males, 70 and above")) +
  scale_y_discrete(limits = c("Another joint", "Right toes", "Left toes", "Right ankle", "Left ankle", "Right knee", "Left knee", "Right hip", "Left hip", "Right shoulder", "Left shoulder", "Right elbow", "Left elbow", "Right wrist", "Left wrist", "Right fingers or thumb", "Left fingers or thumb"))
ggsave("N13.age-sex.png", width = 20, height = 13)


#III) Socioeconomic trends (employment status only)

#i)   Prevalence by employment status
N13_JS_emp <- svyby(formula = ~JNTSYMP,
                    by = ~DOINGLWA,
                    design = NHIS13_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N13_JS_emp.c <- N13_JS_emp %>%
  select(1, 3, 5) %>%
  setNames(c("Employment", "Proportion", "SE")) %>%
  mutate(Employment = as.factor(Employment)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N13_JS_emp_ci <- confint(N13_JS_emp) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for js = 0
N13_JS_emp_ci <- N13_JS_emp_ci[-c(1:2), ]
#join ci and proportion
N13.JS.emp <- bind_cols(N13_JS_emp.c, N13_JS_emp_ci) #final proportion, se & 95% ci
#save
#write.csv(N13.JS.emp, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N13.JS.emp.csv")

#ii)  Logistic regression by employment status
N13_JS.emp_glm <- svyglm(JNTSYMP~relevel(DOINGLWA, ref = "Working for pay at a job/business") + AGE_P + SEX,
                         family = quasibinomial,
                         design = NHIS13_DO)
summary(N13_JS.emp_glm)
exp(cbind(OR=coef(N13_JS.emp_glm), confint(N13_JS.emp_glm)))

#4. Lifestyle trends

#I)  BMI
#logistic regression
N13.BMI.glm <- svyglm(JNTSYMP ~ BMI + AGE_P + SEX,
                      family = quasibinomial,
                      design = NHIS13_DO)
summary(N13.BMI.glm)
exp(cbind(OR=coef(N13.BMI.glm), confint(N13.BMI.glm)))

#II) Physical activity
#logistic regression
N13.PA.JS.glm <- svyglm(JNTSYMP ~ relevel(factor(PAC, ordered = FALSE), ref = "Physically active") + AGE_P + SEX,
                        family = quasibinomial,
                        design = NHIS13_DO)
summary(N13.PA.JS.glm)
exp(cbind(OR = coef(N13.PA.JS.glm), confint(N13.PA.JS.glm)))

#_____________________________________________________________________________________________________________________

# == 2012 == #

#1. Overall prevalence
N12_JS <- svymean(~factor(JNTSYMP), 
                  NHIS12_DO, 
                  na.rm = TRUE)

N12_JS.c <- N12_JS %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N12_JS_ci <- confint(N12_JS) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
N12.JS <- bind_cols(N12_JS.c, N12_JS_ci)
#remove js = 0
N12.JS <- N12.JS[-c(1), ] #final proportion, se & 95%
#save
#write.csv(N12.JS, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N12.JS.csv")

#2. Spatial analysis, by region

#I)   Overall population

#i)   regional prevalence for the overall population
N12_JS_reg <- svyby(formula = ~JNTSYMP,
                    by = ~REGION,
                    design = NHIS12_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N12_JS_reg.c <- N12_JS_reg %>%
  select(1, 3, 5) %>%
  setNames(c("Region", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N12_JS_reg.ci <- confint(N12_JS_reg) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for js = 0
N12_JS_reg.ci <- N12_JS_reg.ci[-c(1:4), ]
#join proportion and ci
N12.JS.region <- bind_cols(N12_JS_reg.c, N12_JS_reg.ci) #final proportion, se & 95% ci
#save
#write.csv(N12.JS.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N12.JS.region.csv")

#Create overall regional prevalence

#Change the region column to "NAME" to match sf
names(N12.JS.region)[names(N12.JS.region) == "Region"] <- "NAME"

N12.JS.joined <- regions %>%
  left_join(N12.JS.region)

N12.JS.joined$NAME <- as.factor(N12.JS.joined$NAME)

#Plot
JSregplot <- ggplot(data = N12.JS.joined) +
  geom_sf(aes(fill = Proportion), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  scale_fill_viridis_c(name = "Prevalence (%)", option = "viridis", direction = -1, limits = c(25, 40), breaks = c(25, 30, 35, 40)) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  ggtitle("Regional prevalence (%) of US residents with joint symptoms in 2012") +
  theme(plot.title = element_text(size = 10, hjust = 0.5))
ggsave("N12.JSregchor.png", width = 5, height = 5)


#ii)  logistic regression by region
N12.JS.reg.glm <- svyglm(relevel(JNTSYMP, ref = "0")~ REGION + AGE_P,
                         family = quasibinomial,
                         design = NHIS12_DO)
summary(N12.JS.reg.glm)
exp(cbind(OR=coef(N12.JS.reg.glm), confint(N12.JS.reg.glm)))

#II)  prevalence by region, by age

#i)  age-specific regional prevalance
N12_JS_reg.age <- svyby(formula = ~JNTSYMP,
                        by = ~REGION + AGE_P,
                        design = NHIS12_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N12_JS_reg.age.c <- N12_JS_reg.age %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Age", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Age = as.character(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N12_JS_reg.age.ci <- confint(N12_JS_reg.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for js = 0
N12_JS_reg.age.ci <- N12_JS_reg.age.ci[-c(1:44), ]
#join proportion and ci
N12.JS.age.region <- bind_cols(N12_JS_reg.age.c, N12_JS_reg.age.ci) #final proportion, se & 95% ci
#save
#write.csv(N12.JS.age.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N12.JS.region.age.csv")

#Geofacet plot - Age & Region
JS.Age_Reg <- ggplot(N12.JS.age.region, aes(Age, Proportion)) +
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
  ggtitle("Regional prevalence (%) of US residents with joint symptoms by age group in 2012") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N12.GF_AgeReg.JS.png", width = 14, height = 9)


#ii) age-specific regional logistic regressions
#18 to 24
sub1824 <- subset(NHIS12_DO, AGE_P == "18 to 24")
N12.JS.age1824.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub1824)
summary(N12.JS.age1824.reg)
exp(cbind(OR = coef(N12.JS.age1824.reg), confint(N12.JS.age1824.reg)))

#25 to 29
sub2529 <- subset(NHIS12_DO, AGE_P == "25 to 29")
N12.JS.age2529.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub2529)
summary(N12.JS.age2529.reg)
exp(cbind(OR = coef(N12.JS.age2529.reg), confint(N12.JS.age2529.reg)))

#30 to 34
sub3034 <- subset(NHIS12_DO, AGE_P == "30 to 34")
N12.JS.age3034.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub3034)
summary(N12.JS.age3034.reg)
exp(cbind(OR = coef(N12.JS.age3034.reg), confint(N12.JS.age3034.reg)))

#35 to 39
sub3539 <- subset(NHIS12_DO, AGE_P == "35 to 39")
N12.JS.age3539.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub3539)
summary(N12.JS.age3539.reg)
exp(cbind(OR = coef(N12.JS.age3539.reg), confint(N12.JS.age3539.reg)))

#40 to 44
sub4044 <- subset(NHIS12_DO, AGE_P == "40 to 44")
N12.JS.age4044.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub4044)
summary(N12.JS.age4044.reg)
exp(cbind(OR = coef(N12.JS.age4044.reg), confint(N12.JS.age4044.reg)))

#45 to 49
sub4549 <- subset(NHIS12_DO, AGE_P == "45 to 49")
N12.JS.age4549.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub4549)
summary(N12.JS.age4549.reg)
exp(cbind(OR = coef(N12.JS.age4549.reg), confint(N12.JS.age4549.reg)))

#50 to 54
sub5054 <- subset(NHIS12_DO, AGE_P =="50 to 54")
N12.JS.age5054.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub5054)
summary(N12.JS.age5054.reg)
exp(cbind(OR = coef(N12.JS.age5054.reg), confint(N12.JS.age5054.reg)))

#55 to 59
sub5559 <- subset(NHIS12_DO, AGE_P == "55 to 59")
N12.JS.age5559.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub5559)
summary(N12.JS.age5559.reg)
exp(cbind(OR = coef(N12.JS.age5559.reg), confint(N12.JS.age5559.reg)))

#60 to 64
sub6064 <- subset(NHIS12_DO, AGE_P == "60 to 64")
N12.JS.age6064.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub6064)
summary(N12.JS.age6064.reg)
exp(cbind(OR = coef(N12.JS.age6064.reg), confint(N12.JS.age6064.reg)))

#65 to 69
sub6569 <- subset(NHIS12_DO, AGE_P == "65 to 69")
N12.JS.age6569.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub6569)
summary(N12.JS.age6569.reg)
exp(cbind(OR = coef(N12.JS.age6569.reg), confint(N12.JS.age6569.reg)))

#70 and above
sub70 <- subset(NHIS12_DO, AGE_P == "70 and above")
N12.JS.age70.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                           family = quasibinomial,
                           design = sub70)
summary(N12.JS.age70.reg)
exp(cbind(OR = coef(N12.JS.age70.reg), confint(N12.JS.age70.reg)))

#III) prevalence by region, by sex

#i)  sex-specific regional prevalence
N12_JS_reg.sex <- svyby(formula = ~JNTSYMP,
                        by = ~REGION + SEX,
                        design = NHIS12_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N12_JS_reg.sex.c <- N12_JS_reg.sex %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Sex", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N12_JS_reg.sex.ci <- confint(N12_JS_reg.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for js = 0
N12_JS_reg.sex.ci <- N12_JS_reg.sex.ci[-c(1:8), ]
#join proportion and ci
N12.JS.sex.region <- bind_cols(N12_JS_reg.sex.c, N12_JS_reg.sex.ci) #final proprtion, se & 95% ci
#save
#write.csv(N12.JS.sex.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N12.JS.region.sex.csv")

#Geofacet - Sex & Region
JS.Sex_Reg12 <- ggplot(N12.JS.sex.region, aes(Sex, Proportion)) +
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
  ggtitle("Regional prevalence (%) of US residents with joint symptoms by sex in 2012") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N12.GF_SexReg.JS.png", width = 14, height = 9)


#ii) sex-specific regional logistic regressions
#males
submales <- subset(NHIS12_DO, SEX == "Male")
N12.JS.sexmale.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + AGE_P,
                             family = quasibinomial,
                             design = submales)
summary(N12.JS.sexmale.reg)
exp(cbind(OR = coef(N12.JS.sexmale.reg), confint(N12.JS.sexmale.reg)))

#females
subfemales <- subset(NHIS12_DO, SEX == "Female")
N12.JS.sexfemale.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + AGE_P,
                               family = quasibinomial,
                               design = subfemales)
summary(N12.JS.sexfemale.reg)
exp(cbind(OR = coef(N12.JS.sexfemale.reg), confint(N12.JS.sexfemale.reg)))


#3. Demographic trends

#I)  Age trends

#i)  Prevalence by age
N12_JS_age <- svyby(formula = ~JNTSYMP,
                    by = ~AGE_P,
                    design = NHIS12_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N12_JS_age.c <- N12_JS_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N12_JS_age_ci <- confint(N12_JS_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove js=0 ci
N12_JS_age_ci <- N12_JS_age_ci[-c(1:11), ]
#join proportion and ci
N12_JS.age <- bind_cols(N12_JS_age.c, N12_JS_age_ci) #final proportion, se & 95% ci
#Save
#write.csv(N12_JS.age, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N12.JS.age.csv")


#ii) Logistic regression by age
N12_JS.age_glm <- svyglm(JNTSYMP~AGE_P + SEX,
                         family = quasibinomial,
                         design = NHIS12_DO)
summary(N12_JS.age_glm)
exp(cbind(OR=coef(N12_JS.age_glm), confint(N12_JS.age_glm)))



#II)  Sex trends

#i)  Prevalence by sex
N12_JS_sex <- svyby(formula = ~JNTSYMP,
                    by = ~SEX,
                    design = NHIS12_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N12_JS_sex.c <- N12_JS_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N12_JS_sex_ci <- confint(N12_JS_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N12_JS_sex_ci <- N12_JS_sex_ci[-c(1:2), ]
#join proportion and ci
N12.JS.sex <- bind_cols(N12_JS_sex.c, N12_JS_sex_ci)
#save
#write.csv(N12.JS.sex, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N12.JS.sex.csv")


#ii) Logistic regression by sex
N12.JS.sex.glm.c <- svyglm(JNTSYMP~relevel(SEX, ref = "Male") + AGE_P,
                           family = quasibinomial,
                           design = NHIS12_DO)
summary(N12.JS.sex.glm.c)
exp(cbind(OR=coef(N12.JS.sex.glm.c), confint(N12.JS.sex.glm.c)))


#Prevalence of various age-sex groups and their joint pain locations

#1. Right shoulder
N12_JS.rtshoulder <- svyby(formula = ~JMTHP1,
                           by = ~AGE_P + SEX,
                           design = NHIS12_DO,
                           FUN = svymean,
                           na.rm = TRUE)
N12_JS.rtshoulder.c <- N12_JS.rtshoulder %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.RtShoulder")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N12_JS.rtshoulder.ci <- confint(N12_JS.rtshoulder) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N12_JS.rtshoulder.ci <- N12_JS.rtshoulder.ci[-c(23:44), ]
#join proportion and ci
N12.JS.rtshoulder <- bind_cols(N12_JS.rtshoulder.c, N12_JS.rtshoulder.ci)
#save
#write.csv(N12.JS.rtshoulder, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N12.rtshoulder.csv")

#2. Left Shoulder
N12_JS.ltshoulder <- svyby(formula = ~JMTHP2,
                           by = ~AGE_P + SEX,
                           design = NHIS12_DO,
                           FUN = svymean,
                           na.rm = TRUE)
N12_JS.ltshoulder.c <- N12_JS.ltshoulder %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltshoulder")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N12_JS.ltshoulder.ci <- confint(N12_JS.ltshoulder) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N12_JS.ltshoulder.ci <- N12_JS.ltshoulder.ci[-c(23:44), ]
#join proportion and ci
N12.JS.ltshoulder <- bind_cols(N12_JS.ltshoulder.c, N12_JS.ltshoulder.ci)
#save
#write.csv(N12.JS.ltshoulder, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N12.ltshoulder.csv")


#3. Right elbow
N12_JS.rtelbow <- svyby(formula = ~JMTHP3,
                        by = ~AGE_P + SEX,
                        design = NHIS12_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N12_JS.rtelbow.c <- N12_JS.rtelbow %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtelbow")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N12_JS.rtelbow.ci <- confint(N12_JS.rtelbow) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N12_JS.rtelbow.ci <- N12_JS.rtelbow.ci[-c(23:44), ]
#join proportion and ci
N12.JS.rtelbow <- bind_cols(N12_JS.rtelbow.c, N12_JS.rtelbow.ci)
#save
#write.csv(N12.JS.rtelbow, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N12.rtelbow.csv")

#4. Lt elbow
N12_JS.ltelbow <- svyby(formula = ~JMTHP4,
                        by = ~AGE_P + SEX,
                        design = NHIS12_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N12_JS.ltelbow.c <- N12_JS.ltelbow %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltelbow")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N12_JS.ltelbow.ci <- confint(N12_JS.ltelbow) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N12_JS.ltelbow.ci <- N12_JS.ltelbow.ci[-c(23:44), ]
#join proportion and ci
N12.JS.ltelbow <- bind_cols(N12_JS.ltelbow.c, N12_JS.ltelbow.ci)
#save
#write.csv(N12.JS.ltelbow, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N12.ltelbow.csv")

#5. Right hip
N12_JS.rthip <- svyby(formula = ~JMTHP5,
                      by = ~AGE_P + SEX,
                      design = NHIS12_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N12_JS.rthip.c <- N12_JS.rthip %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rthip")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N12_JS.rthip.ci <- confint(N12_JS.rthip) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N12_JS.rthip.ci <- N12_JS.rthip.ci[-c(23:44), ]
#join proportion and ci
N12.JS.rthip <- bind_cols(N12_JS.rthip.c, N12_JS.rthip.ci)
#save
#write.csv(N12.JS.rthip, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N12.rthip.csv")

#6. Left hip
N12_JS.lthip <- svyby(formula = ~JMTHP6,
                      by = ~AGE_P + SEX,
                      design = NHIS12_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N12_JS.lthip.c <- N12_JS.lthip %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.lthip")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N12_JS.lthip.ci <- confint(N12_JS.lthip) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N12_JS.lthip.ci <- N12_JS.lthip.ci[-c(23:44), ]
#join proportion and ci
N12.JS.lthip <- bind_cols(N12_JS.lthip.c, N12_JS.lthip.ci)
#save
#write.csv(N12.JS.lthip, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N12.lthip.csv")

#7. Right wrist
N12_JS.rtwrist <- svyby(formula = ~JMTHP7,
                        by = ~AGE_P + SEX,
                        design = NHIS12_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N12_JS.rtwrist.c <- N12_JS.rtwrist %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtwrist")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N12_JS.rtwrist.ci <- confint(N12_JS.rtwrist) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N12_JS.rtwrist.ci <- N12_JS.rtwrist.ci[-c(23:44), ]
#join proportion and ci
N12.JS.rtwrist <- bind_cols(N12_JS.rtwrist.c, N12_JS.rtwrist.ci)
#save
#write.csv(N12.JS.rtwrist, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N12.rtwrist.csv")

#8. Left wrist
N12_JS.ltwrist <- svyby(formula = ~JMTHP8,
                        by = ~AGE_P + SEX,
                        design = NHIS12_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N12_JS.ltwrist.c <- N12_JS.ltwrist %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltwrist")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N12_JS.ltwrist.ci <- confint(N12_JS.ltwrist) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N12_JS.ltwrist.ci <- N12_JS.ltwrist.ci[-c(23:44), ]
#join proportion and ci
N12.JS.ltwrist <- bind_cols(N12_JS.ltwrist.c, N12_JS.ltwrist.ci)
#save
#write.csv(N12.JS.ltwrist, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N12.ltwrist.csv")

#9. Right knee
N12_JS.rtknee <- svyby(formula = ~JMTHP9,
                       by = ~AGE_P + SEX,
                       design = NHIS12_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N12_JS.rtknee.c <- N12_JS.rtknee %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtknee")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N12_JS.rtknee.ci <- confint(N12_JS.rtknee) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N12_JS.rtknee.ci <- N12_JS.rtknee.ci[-c(23:44), ]
#join proportion and ci
N12.JS.rtknee <- bind_cols(N12_JS.rtknee.c, N12_JS.rtknee.ci)
#save
#write.csv(N12.JS.rtknee, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N12.rtknee.csv")

#10. Left knee
N12_JS.ltknee <- svyby(formula = ~JMTHP10,
                       by = ~AGE_P + SEX,
                       design = NHIS12_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N12_JS.ltknee.c <- N12_JS.ltknee %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltknee")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N12_JS.ltknee.ci <- confint(N12_JS.ltknee) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N12_JS.ltknee.ci <- N12_JS.ltknee.ci[-c(23:44), ]
#join proportion and ci
N12.JS.ltknee <- bind_cols(N12_JS.ltknee.c, N12_JS.ltknee.ci)
#save
#write.csv(N12.JS.ltknee, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N12.ltknee.csv")

#11. Right ankle
N12_JS.rtankle <- svyby(formula = ~JMTHP11,
                        by = ~AGE_P + SEX,
                        design = NHIS12_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N12_JS.rtankle.c <- N12_JS.rtankle %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtankle")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N12_JS.rtankle.ci <- confint(N12_JS.rtankle) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N12_JS.rtankle.ci <- N12_JS.rtankle.ci[-c(23:44), ]
#join proportion and ci
N12.JS.rtankle <- bind_cols(N12_JS.rtankle.c, N12_JS.rtankle.ci)
#save
#write.csv(N12.JS.rtankle, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N12.rtankle.csv")

#12. Left ankle
N12_JS.ltankle <- svyby(formula = ~JMTHP12,
                        by = ~AGE_P + SEX,
                        design = NHIS12_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N12_JS.ltankle.c <- N12_JS.ltankle %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltankle")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N12_JS.ltankle.ci <- confint(N12_JS.ltankle) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N12_JS.ltankle.ci <- N12_JS.ltankle.ci[-c(23:44), ]
#join proportion and ci
N12.JS.ltankle <- bind_cols(N12_JS.ltankle.c, N12_JS.ltankle.ci)
#save
#write.csv(N12.JS.ltankle, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N12.ltankle.csv")

#13. Right toes
N12_JS.rttoes <- svyby(formula = ~JMTHP13,
                       by = ~AGE_P + SEX,
                       design = NHIS12_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N12_JS.rttoes.c <- N12_JS.rttoes %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rttoes")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N12_JS.rttoes.ci <- confint(N12_JS.rttoes) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N12_JS.rttoes.ci <- N12_JS.rttoes.ci[-c(23:44), ]
#join proportion and ci
N12.JS.rttoes <- bind_cols(N12_JS.rttoes.c, N12_JS.rttoes.ci)
#save
#write.csv(N12.JS.rttoes, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N12.rttoes.csv")

#14. Left toes
N12_JS.lttoes <- svyby(formula = ~JMTHP14,
                       by = ~AGE_P + SEX,
                       design = NHIS12_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N12_JS.lttoes.c <- N12_JS.lttoes %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.lttoes")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N12_JS.lttoes.ci <- confint(N12_JS.lttoes) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N12_JS.lttoes.ci <- N12_JS.lttoes.ci[-c(23:44), ]
#join proportion and ci
N12.JS.lttoes <- bind_cols(N12_JS.lttoes.c, N12_JS.lttoes.ci)
#save
#write.csv(N12.JS.lttoes, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N12.lttoes.csv")

#15. Right fingers/thumb
N12_JS.rtfingthumb <- svyby(formula = ~JMTHP15,
                            by = ~AGE_P + SEX,
                            design = NHIS12_DO,
                            FUN = svymean,
                            na.rm = TRUE)
N12_JS.rtfingthumb.c <- N12_JS.rtfingthumb %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtfingthumb")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N12_JS.rtfingthumb.ci <- confint(N12_JS.rtfingthumb) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N12_JS.rtfingthumb.ci <- N12_JS.rtfingthumb.ci[-c(23:44), ]
#join proportion and ci
N12.JS.rtfingthumb <- bind_cols(N12_JS.rtfingthumb.c, N12_JS.rtfingthumb.ci)
#save
#write.csv(N12.JS.rtfingthumb, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N12.rtfingthumb.csv")

#16. Left fingers/thumb
N12_JS.ltfingthumb <- svyby(formula = ~JMTHP16,
                            by = ~AGE_P + SEX,
                            design = NHIS12_DO,
                            FUN = svymean,
                            na.rm = TRUE)
N12_JS.ltfingthumb.c <- N12_JS.ltfingthumb %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltfingthumb")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N12_JS.ltfingthumb.ci <- confint(N12_JS.ltfingthumb) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N12_JS.ltfingthumb.ci <- N12_JS.ltfingthumb.ci[-c(23:44), ]
#join proportion and ci
N12.JS.ltfingthumb <- bind_cols(N12_JS.ltfingthumb.c, N12_JS.ltfingthumb.ci)
#save
#write.csv(N12.JS.ltfingthumb, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N12.ltfingthumb.csv")

#17. Another
N12_JS.another <- svyby(formula = ~JMTHP17,
                        by = ~AGE_P + SEX,
                        design = NHIS12_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N12_JS.another.c <- N12_JS.another %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.another")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N12_JS.another.ci <- confint(N12_JS.another) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N12_JS.another.ci <- N12_JS.another.ci[-c(23:44), ]
#join proportion and ci
N12.JS.another <- bind_cols(N12_JS.another.c, N12_JS.another.ci)
#save
#write.csv(N12.JS.another, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N12.another.csv")

N12.JSloc <- Reduce(function(x, y) merge(x, y, all = TRUE), list(N12_JS.rtshoulder.c, N12_JS.ltshoulder.c, N12_JS.rtelbow.c, N12_JS.ltelbow.c, N12_JS.rthip.c, N12_JS.lthip.c, N12_JS.rtwrist.c, N12_JS.ltwrist.c, N12_JS.rtknee.c, N12_JS.ltknee.c, N12_JS.rtankle.c, N12_JS.ltankle.c, N12_JS.rttoes.c, N12_JS.lttoes.c, N12_JS.rtfingthumb.c, N12_JS.ltfingthumb.c, N12_JS.another.c))

#Wide to long
N12.JSlocation <- gather(N12.JSloc, Location, Proportion, Proportion.RtShoulder:Proportion.another, factor_key = TRUE)
N12.JSlocation <- N12.JSlocation %>% 
  unite("Demographic", Age:Sex)
N12.JSlocation$Location <- as.character(N12.JSlocation$Location)
N12.JSlocation$Location[N12.JSlocation$Location == "Proportion.RtShoulder"] <- "Right shoulder"
N12.JSlocation$Location[N12.JSlocation$Location == "Proportion.ltshoulder"] <- "Left shoulder"
N12.JSlocation$Location[N12.JSlocation$Location == "Proportion.rtelbow"] <- "Right elbow"
N12.JSlocation$Location[N12.JSlocation$Location == "Proportion.ltelbow"] <- "Left elbow"
N12.JSlocation$Location[N12.JSlocation$Location == "Proportion.rthip"] <- "Right hip"
N12.JSlocation$Location[N12.JSlocation$Location == "Proportion.lthip"] <- "Left hip"
N12.JSlocation$Location[N12.JSlocation$Location == "Proportion.rtwrist"] <- "Right wrist"
N12.JSlocation$Location[N12.JSlocation$Location == "Proportion.ltwrist"] <- "Left wrist"
N12.JSlocation$Location[N12.JSlocation$Location == "Proportion.rtknee"] <- "Right knee"
N12.JSlocation$Location[N12.JSlocation$Location == "Proportion.ltknee"] <- "Left knee"
N12.JSlocation$Location[N12.JSlocation$Location == "Proportion.rtankle"] <- "Right ankle"
N12.JSlocation$Location[N12.JSlocation$Location == "Proportion.ltankle"] <- "Left ankle"
N12.JSlocation$Location[N12.JSlocation$Location == "Proportion.rttoes"] <- "Right toes"
N12.JSlocation$Location[N12.JSlocation$Location == "Proportion.lttoes"] <- "Left toes"
N12.JSlocation$Location[N12.JSlocation$Location == "Proportion.rtfingthumb"] <- "Right fingers or thumb"
N12.JSlocation$Location[N12.JSlocation$Location == "Proportion.ltfingthumb"] <- "Left fingers or thumb"
N12.JSlocation$Location[N12.JSlocation$Location == "Proportion.another"] <- "Another joint"

#Heatmap of joint locations by demographic groups
N12.JSlocmap <- ggplot(N12.JSlocation,
                       aes(x = Demographic, y = Location, fill = Proportion)) +
  geom_tile() + 
  scale_fill_viridis(option = "viridis", direction = -1, limits = c(0, 60), breaks = c(10, 20, 30, 40, 50, 60), discrete = FALSE) +
  theme_bw() + 
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.text.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  xlab("Age-sex profile") +
  theme(legend.position = "right",
        strip.background = element_blank()) +
  theme(legend.title = element_text(size = 26)) +
  theme(legend.text = element_text(size = 22)) +
  labs(fill = "Prevalence (%)") +
  guides(fill = guide_colourbar(barwidth = 3, barheight = 25)) +
  scale_x_discrete(labels = c("Females, 18 to 24", "Males, 18 to 24", "Females, 25 to 29", "Males, 25 to 29", "Females, 30 to 34", "Males, 30 to 34", "Females, 35 to 39", "Males, 35 to 39", "Females, 40 to 44", "Males, 40 to 44", "Females, 45 to 49", "Males, 45 to 49", "Females, 50 to 54", "Males, 50 to 54", "Females, 55 to 59", "Males, 55 to 59", "Females, 60 to 64", "Males, 60 to 64", "Females, 65 to 69", "Males, 65 to 69", "Females, 70 and above", "Males, 70 and above")) +
  scale_y_discrete(limits = c("Another joint", "Right toes", "Left toes", "Right ankle", "Left ankle", "Right knee", "Left knee", "Right hip", "Left hip", "Right shoulder", "Left shoulder", "Right elbow", "Left elbow", "Right wrist", "Left wrist", "Right fingers or thumb", "Left fingers or thumb"))
ggsave("N12.age-sex.png", width = 20, height = 13)



#III) Socioeconomic trends (employment status only)

#i)   Prevalence by employment status
N12_JS_emp <- svyby(formula = ~JNTSYMP,
                    by = ~DOINGLWA,
                    design = NHIS12_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N12_JS_emp.c <- N12_JS_emp %>%
  select(1, 3, 5) %>%
  setNames(c("Employment", "Proportion", "SE")) %>%
  mutate(Employment = as.factor(Employment)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N12_JS_emp_ci <- confint(N12_JS_emp) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for js = 0
N12_JS_emp_ci <- N12_JS_emp_ci[-c(1:2), ]
#join ci and proportion
N12.JS.emp <- bind_cols(N12_JS_emp.c, N12_JS_emp_ci) #final proportion, se & 95% ci
#save
#write.csv(N12.JS.emp, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N12.JS.emp.csv")

#ii)  Logistic regression by employment status
N12_JS.emp_glm <- svyglm(JNTSYMP~relevel(DOINGLWA, ref = "Working for pay at a job/business") + AGE_P + SEX,
                         family = quasibinomial,
                         design = NHIS12_DO)
summary(N12_JS.emp_glm)
exp(cbind(OR=coef(N12_JS.emp_glm), confint(N12_JS.emp_glm)))

#4. Lifestyle trends

#I)  BMI
#logistic regression
N12.BMI.glm <- svyglm(JNTSYMP ~ BMI + AGE_P + SEX,
                      family = quasibinomial,
                      design = NHIS12_DO)
summary(N12.BMI.glm)
exp(cbind(OR=coef(N12.BMI.glm), confint(N12.BMI.glm)))

#II) Physical activity
#logistic regression
N12.PA.JS.glm <- svyglm(JNTSYMP ~ relevel(factor(PAC, ordered = FALSE), ref = "Physically active") + AGE_P + SEX,
                        family = quasibinomial,
                        design = NHIS12_DO)
summary(N12.PA.JS.glm)
exp(cbind(OR = coef(N12.PA.JS.glm), confint(N12.PA.JS.glm)))

#______________________________________________________________________________________________________________________

# == 2011 == #

#1. Overall prevalence
N11_JS <- svymean(~factor(JNTSYMP), 
                  NHIS11_DO, 
                  na.rm = TRUE)

N11_JS.c <- N11_JS %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N11_JS_ci <- confint(N11_JS) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
N11.JS <- bind_cols(N11_JS.c, N11_JS_ci)
#remove js = 0
N11.JS <- N11.JS[-c(1), ] #final proportion, se & 95%
#save
#write.csv(N11.JS, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N11.JS.csv")

#2. Spatial analysis, by region

#I)   Overall population

#i)   regional prevalence for the overall population
N11_JS_reg <- svyby(formula = ~JNTSYMP,
                    by = ~REGION,
                    design = NHIS11_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N11_JS_reg.c <- N11_JS_reg %>%
  select(1, 3, 5) %>%
  setNames(c("Region", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N11_JS_reg.ci <- confint(N11_JS_reg) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for js = 0
N11_JS_reg.ci <- N11_JS_reg.ci[-c(1:4), ]
#join proportion and ci
N11.JS.region <- bind_cols(N11_JS_reg.c, N11_JS_reg.ci) #final proportion, se & 95% ci
#save
#write.csv(N11.JS.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N11.JS.region.csv")

#Create overall regional prevalence

#Change the region column to "NAME" to match sf
names(N11.JS.region)[names(N11.JS.region) == "Region"] <- "NAME"

N11.JS.joined <- regions %>%
  left_join(N11.JS.region)

N11.JS.joined$NAME <- as.factor(N11.JS.joined$NAME)

#Plot
JSregplot <- ggplot(data = N11.JS.joined) +
  geom_sf(aes(fill = Proportion), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  scale_fill_viridis_c(name = "Prevalence (%)", option = "viridis", direction = -1, limits = c(25, 40), breaks = c(25, 30, 35, 40)) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  ggtitle("Regional prevalence (%) of US residents with joint symptoms in 2011") +
  theme(plot.title = element_text(size = 10, hjust = 0.5))
ggsave("N11.JSregchor.png", width = 5, height = 5)


#ii)  logistic regression by region
N11.JS.reg.glm <- svyglm(relevel(JNTSYMP, ref = "0")~ REGION + AGE_P,
                         family = quasibinomial,
                         design = NHIS11_DO)
summary(N11.JS.reg.glm)
exp(cbind(OR=coef(N11.JS.reg.glm), confint(N11.JS.reg.glm)))

#II)  prevalence by region, by age

#i)  age-specific regional prevalance
N11_JS_reg.age <- svyby(formula = ~JNTSYMP,
                        by = ~REGION + AGE_P,
                        design = NHIS11_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N11_JS_reg.age.c <- N11_JS_reg.age %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Age", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Age = as.character(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N11_JS_reg.age.ci <- confint(N11_JS_reg.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for js = 0
N11_JS_reg.age.ci <- N11_JS_reg.age.ci[-c(1:44), ]
#join proportion and ci
N11.JS.age.region <- bind_cols(N11_JS_reg.age.c, N11_JS_reg.age.ci) #final proportion, se & 95% ci
#save
#write.csv(N11.JS.age.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N11.JS.region.age.csv")

#Geofacet plot - Age & Region
JS.Age_Reg <- ggplot(N11.JS.age.region, aes(Age, Proportion)) +
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
  ggtitle("Regional prevalence (%) of US residents with joint symptoms by age group in 2011") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N11.GF_AgeReg.JS.png", width = 14, height = 9)


#ii) age-specific regional logistic regressions
#18 to 24
sub1824 <- subset(NHIS11_DO, AGE_P == "18 to 24")
N11.JS.age1824.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub1824)
summary(N11.JS.age1824.reg)
exp(cbind(OR = coef(N11.JS.age1824.reg), confint(N11.JS.age1824.reg)))

#25 to 29
sub2529 <- subset(NHIS11_DO, AGE_P == "25 to 29")
N11.JS.age2529.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub2529)
summary(N11.JS.age2529.reg)
exp(cbind(OR = coef(N11.JS.age2529.reg), confint(N11.JS.age2529.reg)))

#30 to 34
sub3034 <- subset(NHIS11_DO, AGE_P == "30 to 34")
N11.JS.age3034.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub3034)
summary(N11.JS.age3034.reg)
exp(cbind(OR = coef(N11.JS.age3034.reg), confint(N11.JS.age3034.reg)))

#35 to 39
sub3539 <- subset(NHIS11_DO, AGE_P == "35 to 39")
N11.JS.age3539.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub3539)
summary(N11.JS.age3539.reg)
exp(cbind(OR = coef(N11.JS.age3539.reg), confint(N11.JS.age3539.reg)))

#40 to 44
sub4044 <- subset(NHIS11_DO, AGE_P == "40 to 44")
N11.JS.age4044.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub4044)
summary(N11.JS.age4044.reg)
exp(cbind(OR = coef(N11.JS.age4044.reg), confint(N11.JS.age4044.reg)))

#45 to 49
sub4549 <- subset(NHIS11_DO, AGE_P == "45 to 49")
N11.JS.age4549.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub4549)
summary(N11.JS.age4549.reg)
exp(cbind(OR = coef(N11.JS.age4549.reg), confint(N11.JS.age4549.reg)))

#50 to 54
sub5054 <- subset(NHIS11_DO, AGE_P =="50 to 54")
N11.JS.age5054.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub5054)
summary(N11.JS.age5054.reg)
exp(cbind(OR = coef(N11.JS.age5054.reg), confint(N11.JS.age5054.reg)))

#55 to 59
sub5559 <- subset(NHIS11_DO, AGE_P == "55 to 59")
N11.JS.age5559.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub5559)
summary(N11.JS.age5559.reg)
exp(cbind(OR = coef(N11.JS.age5559.reg), confint(N11.JS.age5559.reg)))

#60 to 64
sub6064 <- subset(NHIS11_DO, AGE_P == "60 to 64")
N11.JS.age6064.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub6064)
summary(N11.JS.age6064.reg)
exp(cbind(OR = coef(N11.JS.age6064.reg), confint(N11.JS.age6064.reg)))

#65 to 69
sub6569 <- subset(NHIS11_DO, AGE_P == "65 to 69")
N11.JS.age6569.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub6569)
summary(N11.JS.age6569.reg)
exp(cbind(OR = coef(N11.JS.age6569.reg), confint(N11.JS.age6569.reg)))

#70 and above
sub70 <- subset(NHIS11_DO, AGE_P == "70 and above")
N11.JS.age70.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                           family = quasibinomial,
                           design = sub70)
summary(N11.JS.age70.reg)
exp(cbind(OR = coef(N11.JS.age70.reg), confint(N11.JS.age70.reg)))

#III) prevalence by region, by sex

#i)  sex-specific regional prevalence
N11_JS_reg.sex <- svyby(formula = ~JNTSYMP,
                        by = ~REGION + SEX,
                        design = NHIS11_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N11_JS_reg.sex.c <- N11_JS_reg.sex %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Sex", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N11_JS_reg.sex.ci <- confint(N11_JS_reg.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for js = 0
N11_JS_reg.sex.ci <- N11_JS_reg.sex.ci[-c(1:8), ]
#join proportion and ci
N11.JS.sex.region <- bind_cols(N11_JS_reg.sex.c, N11_JS_reg.sex.ci) #final proprtion, se & 95% ci
#save
#write.csv(N11.JS.sex.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N11.JS.region.sex.csv")

#Geofacet - Sex & Region
JS.Sex_Reg11 <- ggplot(N11.JS.sex.region, aes(Sex, Proportion)) +
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
  ggtitle("Regional prevalence (%) of US residents with joint symptoms by sex in 2011") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N11.GF_SexReg.JS.png", width = 14, height = 9)


#ii) sex-specific regional logistic regressions
#males
submales <- subset(NHIS11_DO, SEX == "Male")
N11.JS.sexmale.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + AGE_P,
                             family = quasibinomial,
                             design = submales)
summary(N11.JS.sexmale.reg)
exp(cbind(OR = coef(N11.JS.sexmale.reg), confint(N11.JS.sexmale.reg)))

#females
subfemales <- subset(NHIS11_DO, SEX == "Female")
N11.JS.sexfemale.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + AGE_P,
                               family = quasibinomial,
                               design = subfemales)
summary(N11.JS.sexfemale.reg)
exp(cbind(OR = coef(N11.JS.sexfemale.reg), confint(N11.JS.sexfemale.reg)))


#3. Demographic trends

#I)  Age trends

#i)  Prevalence by age
N11_JS_age <- svyby(formula = ~JNTSYMP,
                    by = ~AGE_P,
                    design = NHIS11_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N11_JS_age.c <- N11_JS_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N11_JS_age_ci <- confint(N11_JS_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove js=0 ci
N11_JS_age_ci <- N11_JS_age_ci[-c(1:11), ]
#join proportion and ci
N11_JS.age <- bind_cols(N11_JS_age.c, N11_JS_age_ci) #final proportion, se & 95% ci
#Save
#write.csv(N11_JS.age, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N11.JS.age.csv")


#ii) Logistic regression by age
N11_JS.age_glm <- svyglm(JNTSYMP~AGE_P + SEX,
                         family = quasibinomial,
                         design = NHIS11_DO)
summary(N11_JS.age_glm)
exp(cbind(OR=coef(N11_JS.age_glm), confint(N11_JS.age_glm)))



#II)  Sex trends

#i)  Prevalence by sex
N11_JS_sex <- svyby(formula = ~JNTSYMP,
                    by = ~SEX,
                    design = NHIS11_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N11_JS_sex.c <- N11_JS_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N11_JS_sex_ci <- confint(N11_JS_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N11_JS_sex_ci <- N11_JS_sex_ci[-c(1:2), ]
#join proportion and ci
N11.JS.sex <- bind_cols(N11_JS_sex.c, N11_JS_sex_ci)
#save
#write.csv(N11.JS.sex, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N11.JS.sex.csv")


#ii) Logistic regression by sex
N11.JS.sex.glm.c <- svyglm(JNTSYMP~relevel(SEX, ref = "Male") + AGE_P,
                           family = quasibinomial,
                           design = NHIS11_DO)
summary(N11.JS.sex.glm.c)
exp(cbind(OR=coef(N11.JS.sex.glm.c), confint(N11.JS.sex.glm.c)))


#Prevalence of various age-sex groups and their joint pain locations

#1. Right shoulder
N11_JS.rtshoulder <- svyby(formula = ~JMTHP1,
                           by = ~AGE_P + SEX,
                           design = NHIS11_DO,
                           FUN = svymean,
                           na.rm = TRUE)
N11_JS.rtshoulder.c <- N11_JS.rtshoulder %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.RtShoulder")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N11_JS.rtshoulder.ci <- confint(N11_JS.rtshoulder) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N11_JS.rtshoulder.ci <- N11_JS.rtshoulder.ci[-c(23:44), ]
#join proportion and ci
N11.JS.rtshoulder <- bind_cols(N11_JS.rtshoulder.c, N11_JS.rtshoulder.ci)
#save
#write.csv(N11.JS.rtshoulder, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N11.rtshoulder.csv")

#2. Left Shoulder
N11_JS.ltshoulder <- svyby(formula = ~JMTHP2,
                           by = ~AGE_P + SEX,
                           design = NHIS11_DO,
                           FUN = svymean,
                           na.rm = TRUE)
N11_JS.ltshoulder.c <- N11_JS.ltshoulder %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltshoulder")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N11_JS.ltshoulder.ci <- confint(N11_JS.ltshoulder) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N11_JS.ltshoulder.ci <- N11_JS.ltshoulder.ci[-c(23:44), ]
#join proportion and ci
N11.JS.ltshoulder <- bind_cols(N11_JS.ltshoulder.c, N11_JS.ltshoulder.ci)
#save
#write.csv(N11.JS.ltshoulder, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N11.ltshoulder.csv")


#3. Right elbow
N11_JS.rtelbow <- svyby(formula = ~JMTHP3,
                        by = ~AGE_P + SEX,
                        design = NHIS11_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N11_JS.rtelbow.c <- N11_JS.rtelbow %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtelbow")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N11_JS.rtelbow.ci <- confint(N11_JS.rtelbow) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N11_JS.rtelbow.ci <- N11_JS.rtelbow.ci[-c(23:44), ]
#join proportion and ci
N11.JS.rtelbow <- bind_cols(N11_JS.rtelbow.c, N11_JS.rtelbow.ci)
#save
#write.csv(N11.JS.rtelbow, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N11.rtelbow.csv")

#4. Lt elbow
N11_JS.ltelbow <- svyby(formula = ~JMTHP4,
                        by = ~AGE_P + SEX,
                        design = NHIS11_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N11_JS.ltelbow.c <- N11_JS.ltelbow %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltelbow")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N11_JS.ltelbow.ci <- confint(N11_JS.ltelbow) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N11_JS.ltelbow.ci <- N11_JS.ltelbow.ci[-c(23:44), ]
#join proportion and ci
N11.JS.ltelbow <- bind_cols(N11_JS.ltelbow.c, N11_JS.ltelbow.ci)
#save
#write.csv(N11.JS.ltelbow, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N11.ltelbow.csv")

#5. Right hip
N11_JS.rthip <- svyby(formula = ~JMTHP5,
                      by = ~AGE_P + SEX,
                      design = NHIS11_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N11_JS.rthip.c <- N11_JS.rthip %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rthip")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N11_JS.rthip.ci <- confint(N11_JS.rthip) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N11_JS.rthip.ci <- N11_JS.rthip.ci[-c(23:44), ]
#join proportion and ci
N11.JS.rthip <- bind_cols(N11_JS.rthip.c, N11_JS.rthip.ci)
#save
#write.csv(N11.JS.rthip, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N11.rthip.csv")

#6. Left hip
N11_JS.lthip <- svyby(formula = ~JMTHP6,
                      by = ~AGE_P + SEX,
                      design = NHIS11_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N11_JS.lthip.c <- N11_JS.lthip %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.lthip")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N11_JS.lthip.ci <- confint(N11_JS.lthip) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N11_JS.lthip.ci <- N11_JS.lthip.ci[-c(23:44), ]
#join proportion and ci
N11.JS.lthip <- bind_cols(N11_JS.lthip.c, N11_JS.lthip.ci)
#save
#write.csv(N11.JS.lthip, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N11.lthip.csv")

#7. Right wrist
N11_JS.rtwrist <- svyby(formula = ~JMTHP7,
                        by = ~AGE_P + SEX,
                        design = NHIS11_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N11_JS.rtwrist.c <- N11_JS.rtwrist %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtwrist")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N11_JS.rtwrist.ci <- confint(N11_JS.rtwrist) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N11_JS.rtwrist.ci <- N11_JS.rtwrist.ci[-c(23:44), ]
#join proportion and ci
N11.JS.rtwrist <- bind_cols(N11_JS.rtwrist.c, N11_JS.rtwrist.ci)
#save
#write.csv(N11.JS.rtwrist, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N11.rtwrist.csv")

#8. Left wrist
N11_JS.ltwrist <- svyby(formula = ~JMTHP8,
                        by = ~AGE_P + SEX,
                        design = NHIS11_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N11_JS.ltwrist.c <- N11_JS.ltwrist %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltwrist")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N11_JS.ltwrist.ci <- confint(N11_JS.ltwrist) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N11_JS.ltwrist.ci <- N11_JS.ltwrist.ci[-c(23:44), ]
#join proportion and ci
N11.JS.ltwrist <- bind_cols(N11_JS.ltwrist.c, N11_JS.ltwrist.ci)
#save
#write.csv(N11.JS.ltwrist, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N11.ltwrist.csv")

#9. Right knee
N11_JS.rtknee <- svyby(formula = ~JMTHP9,
                       by = ~AGE_P + SEX,
                       design = NHIS11_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N11_JS.rtknee.c <- N11_JS.rtknee %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtknee")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N11_JS.rtknee.ci <- confint(N11_JS.rtknee) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N11_JS.rtknee.ci <- N11_JS.rtknee.ci[-c(23:44), ]
#join proportion and ci
N11.JS.rtknee <- bind_cols(N11_JS.rtknee.c, N11_JS.rtknee.ci)
#save
#write.csv(N11.JS.rtknee, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N11.rtknee.csv")

#10. Left knee
N11_JS.ltknee <- svyby(formula = ~JMTHP10,
                       by = ~AGE_P + SEX,
                       design = NHIS11_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N11_JS.ltknee.c <- N11_JS.ltknee %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltknee")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N11_JS.ltknee.ci <- confint(N11_JS.ltknee) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N11_JS.ltknee.ci <- N11_JS.ltknee.ci[-c(23:44), ]
#join proportion and ci
N11.JS.ltknee <- bind_cols(N11_JS.ltknee.c, N11_JS.ltknee.ci)
#save
#write.csv(N11.JS.ltknee, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N11.ltknee.csv")

#11. Right ankle
N11_JS.rtankle <- svyby(formula = ~JMTHP11,
                        by = ~AGE_P + SEX,
                        design = NHIS11_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N11_JS.rtankle.c <- N11_JS.rtankle %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtankle")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N11_JS.rtankle.ci <- confint(N11_JS.rtankle) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N11_JS.rtankle.ci <- N11_JS.rtankle.ci[-c(23:44), ]
#join proportion and ci
N11.JS.rtankle <- bind_cols(N11_JS.rtankle.c, N11_JS.rtankle.ci)
#save
#write.csv(N11.JS.rtankle, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N11.rtankle.csv")

#12. Left ankle
N11_JS.ltankle <- svyby(formula = ~JMTHP12,
                        by = ~AGE_P + SEX,
                        design = NHIS11_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N11_JS.ltankle.c <- N11_JS.ltankle %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltankle")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N11_JS.ltankle.ci <- confint(N11_JS.ltankle) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N11_JS.ltankle.ci <- N11_JS.ltankle.ci[-c(23:44), ]
#join proportion and ci
N11.JS.ltankle <- bind_cols(N11_JS.ltankle.c, N11_JS.ltankle.ci)
#save
#write.csv(N11.JS.ltankle, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N11.ltankle.csv")

#13. Right toes
N11_JS.rttoes <- svyby(formula = ~JMTHP13,
                       by = ~AGE_P + SEX,
                       design = NHIS11_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N11_JS.rttoes.c <- N11_JS.rttoes %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rttoes")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N11_JS.rttoes.ci <- confint(N11_JS.rttoes) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N11_JS.rttoes.ci <- N11_JS.rttoes.ci[-c(23:44), ]
#join proportion and ci
N11.JS.rttoes <- bind_cols(N11_JS.rttoes.c, N11_JS.rttoes.ci)
#save
#write.csv(N11.JS.rttoes, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N11.rttoes.csv")

#14. Left toes
N11_JS.lttoes <- svyby(formula = ~JMTHP14,
                       by = ~AGE_P + SEX,
                       design = NHIS11_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N11_JS.lttoes.c <- N11_JS.lttoes %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.lttoes")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N11_JS.lttoes.ci <- confint(N11_JS.lttoes) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N11_JS.lttoes.ci <- N11_JS.lttoes.ci[-c(23:44), ]
#join proportion and ci
N11.JS.lttoes <- bind_cols(N11_JS.lttoes.c, N11_JS.lttoes.ci)
#save
#write.csv(N11.JS.lttoes, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N11.lttoes.csv")

#15. Right fingers/thumb
N11_JS.rtfingthumb <- svyby(formula = ~JMTHP15,
                            by = ~AGE_P + SEX,
                            design = NHIS11_DO,
                            FUN = svymean,
                            na.rm = TRUE)
N11_JS.rtfingthumb.c <- N11_JS.rtfingthumb %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtfingthumb")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N11_JS.rtfingthumb.ci <- confint(N11_JS.rtfingthumb) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N11_JS.rtfingthumb.ci <- N11_JS.rtfingthumb.ci[-c(23:44), ]
#join proportion and ci
N11.JS.rtfingthumb <- bind_cols(N11_JS.rtfingthumb.c, N11_JS.rtfingthumb.ci)
#save
#write.csv(N11.JS.rtfingthumb, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N11.rtfingthumb.csv")

#16. Left fingers/thumb
N11_JS.ltfingthumb <- svyby(formula = ~JMTHP16,
                            by = ~AGE_P + SEX,
                            design = NHIS11_DO,
                            FUN = svymean,
                            na.rm = TRUE)
N11_JS.ltfingthumb.c <- N11_JS.ltfingthumb %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltfingthumb")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N11_JS.ltfingthumb.ci <- confint(N11_JS.ltfingthumb) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N11_JS.ltfingthumb.ci <- N11_JS.ltfingthumb.ci[-c(23:44), ]
#join proportion and ci
N11.JS.ltfingthumb <- bind_cols(N11_JS.ltfingthumb.c, N11_JS.ltfingthumb.ci)
#save
#write.csv(N11.JS.ltfingthumb, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N11.ltfingthumb.csv")

#17. Another
N11_JS.another <- svyby(formula = ~JMTHP17,
                        by = ~AGE_P + SEX,
                        design = NHIS11_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N11_JS.another.c <- N11_JS.another %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.another")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N11_JS.another.ci <- confint(N11_JS.another) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N11_JS.another.ci <- N11_JS.another.ci[-c(23:44), ]
#join proportion and ci
N11.JS.another <- bind_cols(N11_JS.another.c, N11_JS.another.ci)
#save
#write.csv(N11.JS.another, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N11.another.csv")

N11.JSloc <- Reduce(function(x, y) merge(x, y, all = TRUE), list(N11_JS.rtshoulder.c, N11_JS.ltshoulder.c, N11_JS.rtelbow.c, N11_JS.ltelbow.c, N11_JS.rthip.c, N11_JS.lthip.c, N11_JS.rtwrist.c, N11_JS.ltwrist.c, N11_JS.rtknee.c, N11_JS.ltknee.c, N11_JS.rtankle.c, N11_JS.ltankle.c, N11_JS.rttoes.c, N11_JS.lttoes.c, N11_JS.rtfingthumb.c, N11_JS.ltfingthumb.c, N11_JS.another.c))

#Wide to long
N11.JSlocation <- gather(N11.JSloc, Location, Proportion, Proportion.RtShoulder:Proportion.another, factor_key = TRUE)
N11.JSlocation <- N11.JSlocation %>% 
  unite("Demographic", Age:Sex)
N11.JSlocation$Location <- as.character(N11.JSlocation$Location)
N11.JSlocation$Location[N11.JSlocation$Location == "Proportion.RtShoulder"] <- "Right shoulder"
N11.JSlocation$Location[N11.JSlocation$Location == "Proportion.ltshoulder"] <- "Left shoulder"
N11.JSlocation$Location[N11.JSlocation$Location == "Proportion.rtelbow"] <- "Right elbow"
N11.JSlocation$Location[N11.JSlocation$Location == "Proportion.ltelbow"] <- "Left elbow"
N11.JSlocation$Location[N11.JSlocation$Location == "Proportion.rthip"] <- "Right hip"
N11.JSlocation$Location[N11.JSlocation$Location == "Proportion.lthip"] <- "Left hip"
N11.JSlocation$Location[N11.JSlocation$Location == "Proportion.rtwrist"] <- "Right wrist"
N11.JSlocation$Location[N11.JSlocation$Location == "Proportion.ltwrist"] <- "Left wrist"
N11.JSlocation$Location[N11.JSlocation$Location == "Proportion.rtknee"] <- "Right knee"
N11.JSlocation$Location[N11.JSlocation$Location == "Proportion.ltknee"] <- "Left knee"
N11.JSlocation$Location[N11.JSlocation$Location == "Proportion.rtankle"] <- "Right ankle"
N11.JSlocation$Location[N11.JSlocation$Location == "Proportion.ltankle"] <- "Left ankle"
N11.JSlocation$Location[N11.JSlocation$Location == "Proportion.rttoes"] <- "Right toes"
N11.JSlocation$Location[N11.JSlocation$Location == "Proportion.lttoes"] <- "Left toes"
N11.JSlocation$Location[N11.JSlocation$Location == "Proportion.rtfingthumb"] <- "Right fingers or thumb"
N11.JSlocation$Location[N11.JSlocation$Location == "Proportion.ltfingthumb"] <- "Left fingers or thumb"
N11.JSlocation$Location[N11.JSlocation$Location == "Proportion.another"] <- "Another joint"

#Heatmap of joint locations by demographic groups
N11.JSlocmap <- ggplot(N11.JSlocation,
                       aes(x = Demographic, y = Location, fill = Proportion)) +
  geom_tile() + 
  scale_fill_viridis(option = "viridis", direction = -1, limits = c(0, 60), breaks = c(10, 20, 30, 40, 50, 60), discrete = FALSE) +
  theme_bw() + 
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.text.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  xlab("Age-sex profile") +
  theme(legend.position = "right",
        strip.background = element_blank()) +
  theme(legend.title = element_text(size = 26)) +
  theme(legend.text = element_text(size = 22)) +
  labs(fill = "Prevalence (%)") +
  guides(fill = guide_colourbar(barwidth = 3, barheight = 25)) +
  scale_x_discrete(labels = c("Females, 18 to 24", "Males, 18 to 24", "Females, 25 to 29", "Males, 25 to 29", "Females, 30 to 34", "Males, 30 to 34", "Females, 35 to 39", "Males, 35 to 39", "Females, 40 to 44", "Males, 40 to 44", "Females, 45 to 49", "Males, 45 to 49", "Females, 50 to 54", "Males, 50 to 54", "Females, 55 to 59", "Males, 55 to 59", "Females, 60 to 64", "Males, 60 to 64", "Females, 65 to 69", "Males, 65 to 69", "Females, 70 and above", "Males, 70 and above")) +
  scale_y_discrete(limits = c("Another joint", "Right toes", "Left toes", "Right ankle", "Left ankle", "Right knee", "Left knee", "Right hip", "Left hip", "Right shoulder", "Left shoulder", "Right elbow", "Left elbow", "Right wrist", "Left wrist", "Right fingers or thumb", "Left fingers or thumb"))
ggsave("N11.age-sex.png", width = 20, height = 13)


#III) Socioeconomic trends (employment status only)

#i)   Prevalence by employment status
N11_JS_emp <- svyby(formula = ~JNTSYMP,
                    by = ~DOINGLWA,
                    design = NHIS11_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N11_JS_emp.c <- N11_JS_emp %>%
  select(1, 3, 5) %>%
  setNames(c("Employment", "Proportion", "SE")) %>%
  mutate(Employment = as.factor(Employment)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N11_JS_emp_ci <- confint(N11_JS_emp) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for js = 0
N11_JS_emp_ci <- N11_JS_emp_ci[-c(1:2), ]
#join ci and proportion
N11.JS.emp <- bind_cols(N11_JS_emp.c, N11_JS_emp_ci) #final proportion, se & 95% ci
#save
#write.csv(N11.JS.emp, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N11.JS.emp.csv")

#ii)  Logistic regression by employment status
N11_JS.emp_glm <- svyglm(JNTSYMP~relevel(DOINGLWA, ref = "Working for pay at a job/business") + AGE_P + SEX,
                         family = quasibinomial,
                         design = NHIS11_DO)
summary(N11_JS.emp_glm)
exp(cbind(OR=coef(N11_JS.emp_glm), confint(N11_JS.emp_glm)))

#4. Lifestyle trends

#I)  BMI
#logistic regression
N11.BMI.glm <- svyglm(JNTSYMP ~ BMI + AGE_P + SEX,
                      family = quasibinomial,
                      design = NHIS11_DO)
summary(N11.BMI.glm)
exp(cbind(OR=coef(N11.BMI.glm), confint(N11.BMI.glm)))

#II) Physical activity
#logistic regression
N11.PA.JS.glm <- svyglm(JNTSYMP ~ relevel(factor(PAC, ordered = FALSE), ref = "Physically active") + AGE_P + SEX,
                        family = quasibinomial,
                        design = NHIS11_DO)
summary(N11.PA.JS.glm)
exp(cbind(OR = coef(N11.PA.JS.glm), confint(N11.PA.JS.glm)))

#_____________________________________________________________________________________________________________________

# == 2010 == #

#1. Overall prevalence
N10_JS <- svymean(~factor(JNTSYMP), 
                  NHIS10_DO, 
                  na.rm = TRUE)

N10_JS.c <- N10_JS %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N10_JS_ci <- confint(N10_JS) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
N10.JS <- bind_cols(N10_JS.c, N10_JS_ci)
#remove js = 0
N10.JS <- N10.JS[-c(1), ] #final proportion, se & 95%
#save
#write.csv(N10.JS, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N10.JS.csv")

#2. Spatial analysis, by region

#I)   Overall population

#i)   regional prevalence for the overall population
N10_JS_reg <- svyby(formula = ~JNTSYMP,
                    by = ~REGION,
                    design = NHIS10_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N10_JS_reg.c <- N10_JS_reg %>%
  select(1, 3, 5) %>%
  setNames(c("Region", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N10_JS_reg.ci <- confint(N10_JS_reg) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for js = 0
N10_JS_reg.ci <- N10_JS_reg.ci[-c(1:4), ]
#join proportion and ci
N10.JS.region <- bind_cols(N10_JS_reg.c, N10_JS_reg.ci) #final proportion, se & 95% ci
#save
#write.csv(N10.JS.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N10.JS.region.csv")

#Create overall regional prevalence

#Change the region column to "NAME" to match sf
names(N10.JS.region)[names(N10.JS.region) == "Region"] <- "NAME"

N10.JS.joined <- regions %>%
  left_join(N10.JS.region)

N10.JS.joined$NAME <- as.factor(N10.JS.joined$NAME)

#Plot
JSregplot <- ggplot(data = N10.JS.joined) +
  geom_sf(aes(fill = Proportion), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  scale_fill_viridis_c(name = "Prevalence (%)", option = "viridis", direction = -1, limits = c(25, 40), breaks = c(25, 30, 35, 40)) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  ggtitle("Regional prevalence (%) of US residents with joint symptoms in 2010") +
  theme(plot.title = element_text(size = 10, hjust = 0.5))
ggsave("N10.JSregchor.png", width = 5, height = 5)


#ii)  logistic regression by region
N10.JS.reg.glm <- svyglm(relevel(JNTSYMP, ref = "0")~ REGION + AGE_P,
                         family = quasibinomial,
                         design = NHIS10_DO)
summary(N10.JS.reg.glm)
exp(cbind(OR=coef(N10.JS.reg.glm), confint(N10.JS.reg.glm)))

#II)  prevalence by region, by age

#i)  age-specific regional prevalance
N10_JS_reg.age <- svyby(formula = ~JNTSYMP,
                        by = ~REGION + AGE_P,
                        design = NHIS10_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N10_JS_reg.age.c <- N10_JS_reg.age %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Age", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Age = as.character(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N10_JS_reg.age.ci <- confint(N10_JS_reg.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for js = 0
N10_JS_reg.age.ci <- N10_JS_reg.age.ci[-c(1:44), ]
#join proportion and ci
N10.JS.age.region <- bind_cols(N10_JS_reg.age.c, N10_JS_reg.age.ci) #final proportion, se & 95% ci
#save
#write.csv(N10.JS.age.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N10.JS.region.age.csv")

#Geofacet plot - Age & Region
JS.Age_Reg <- ggplot(N10.JS.age.region, aes(Age, Proportion)) +
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
  ggtitle("Regional prevalence (%) of US residents with joint symptoms by age group in 2010") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N10.GF_AgeReg.JS.png", width = 14, height = 9)


#ii) age-specific regional logistic regressions
#18 to 24
sub1824 <- subset(NHIS10_DO, AGE_P == "18 to 24")
N10.JS.age1824.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub1824)
summary(N10.JS.age1824.reg)
exp(cbind(OR = coef(N10.JS.age1824.reg), confint(N10.JS.age1824.reg)))

#25 to 29
sub2529 <- subset(NHIS10_DO, AGE_P == "25 to 29")
N10.JS.age2529.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub2529)
summary(N10.JS.age2529.reg)
exp(cbind(OR = coef(N10.JS.age2529.reg), confint(N10.JS.age2529.reg)))

#30 to 34
sub3034 <- subset(NHIS10_DO, AGE_P == "30 to 34")
N10.JS.age3034.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub3034)
summary(N10.JS.age3034.reg)
exp(cbind(OR = coef(N10.JS.age3034.reg), confint(N10.JS.age3034.reg)))

#35 to 39
sub3539 <- subset(NHIS10_DO, AGE_P == "35 to 39")
N10.JS.age3539.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub3539)
summary(N10.JS.age3539.reg)
exp(cbind(OR = coef(N10.JS.age3539.reg), confint(N10.JS.age3539.reg)))

#40 to 44
sub4044 <- subset(NHIS10_DO, AGE_P == "40 to 44")
N10.JS.age4044.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub4044)
summary(N10.JS.age4044.reg)
exp(cbind(OR = coef(N10.JS.age4044.reg), confint(N10.JS.age4044.reg)))

#45 to 49
sub4549 <- subset(NHIS10_DO, AGE_P == "45 to 49")
N10.JS.age4549.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub4549)
summary(N10.JS.age4549.reg)
exp(cbind(OR = coef(N10.JS.age4549.reg), confint(N10.JS.age4549.reg)))

#50 to 54
sub5054 <- subset(NHIS10_DO, AGE_P =="50 to 54")
N10.JS.age5054.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub5054)
summary(N10.JS.age5054.reg)
exp(cbind(OR = coef(N10.JS.age5054.reg), confint(N10.JS.age5054.reg)))

#55 to 59
sub5559 <- subset(NHIS10_DO, AGE_P == "55 to 59")
N10.JS.age5559.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub5559)
summary(N10.JS.age5559.reg)
exp(cbind(OR = coef(N10.JS.age5559.reg), confint(N10.JS.age5559.reg)))

#60 to 64
sub6064 <- subset(NHIS10_DO, AGE_P == "60 to 64")
N10.JS.age6064.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub6064)
summary(N10.JS.age6064.reg)
exp(cbind(OR = coef(N10.JS.age6064.reg), confint(N10.JS.age6064.reg)))

#65 to 69
sub6569 <- subset(NHIS10_DO, AGE_P == "65 to 69")
N10.JS.age6569.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub6569)
summary(N10.JS.age6569.reg)
exp(cbind(OR = coef(N10.JS.age6569.reg), confint(N10.JS.age6569.reg)))

#70 and above
sub70 <- subset(NHIS10_DO, AGE_P == "70 and above")
N10.JS.age70.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                           family = quasibinomial,
                           design = sub70)
summary(N10.JS.age70.reg)
exp(cbind(OR = coef(N10.JS.age70.reg), confint(N10.JS.age70.reg)))

#III) prevalence by region, by sex

#i)  sex-specific regional prevalence
N10_JS_reg.sex <- svyby(formula = ~JNTSYMP,
                        by = ~REGION + SEX,
                        design = NHIS10_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N10_JS_reg.sex.c <- N10_JS_reg.sex %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Sex", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N10_JS_reg.sex.ci <- confint(N10_JS_reg.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for js = 0
N10_JS_reg.sex.ci <- N10_JS_reg.sex.ci[-c(1:8), ]
#join proportion and ci
N10.JS.sex.region <- bind_cols(N10_JS_reg.sex.c, N10_JS_reg.sex.ci) #final proprtion, se & 95% ci
#save
#write.csv(N10.JS.sex.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N10.JS.region.sex.csv")

#Geofacet - Sex & Region
JS.Sex_Reg10 <- ggplot(N10.JS.sex.region, aes(Sex, Proportion)) +
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
  ggtitle("Regional prevalence (%) of US residents with joint symptoms by sex in 2010") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N10.GF_SexReg.JS.png", width = 14, height = 9)


#ii) sex-specific regional logistic regressions
#males
submales <- subset(NHIS10_DO, SEX == "Male")
N10.JS.sexmale.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + AGE_P,
                             family = quasibinomial,
                             design = submales)
summary(N10.JS.sexmale.reg)
exp(cbind(OR = coef(N10.JS.sexmale.reg), confint(N10.JS.sexmale.reg)))

#females
subfemales <- subset(NHIS10_DO, SEX == "Female")
N10.JS.sexfemale.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + AGE_P,
                               family = quasibinomial,
                               design = subfemales)
summary(N10.JS.sexfemale.reg)
exp(cbind(OR = coef(N10.JS.sexfemale.reg), confint(N10.JS.sexfemale.reg)))


#3. Demographic trends

#I)  Age trends

#i)  Prevalence by age
N10_JS_age <- svyby(formula = ~JNTSYMP,
                    by = ~AGE_P,
                    design = NHIS10_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N10_JS_age.c <- N10_JS_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N10_JS_age_ci <- confint(N10_JS_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove js=0 ci
N10_JS_age_ci <- N10_JS_age_ci[-c(1:11), ]
#join proportion and ci
N10_JS.age <- bind_cols(N10_JS_age.c, N10_JS_age_ci) #final proportion, se & 95% ci
#Save
#write.csv(N10_JS.age, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N10.JS.age.csv")


#ii) Logistic regression by age
N10_JS.age_glm <- svyglm(JNTSYMP~AGE_P + SEX,
                         family = quasibinomial,
                         design = NHIS10_DO)
summary(N10_JS.age_glm)
exp(cbind(OR=coef(N10_JS.age_glm), confint(N10_JS.age_glm)))



#II)  Sex trends

#i)  Prevalence by sex
N10_JS_sex <- svyby(formula = ~JNTSYMP,
                    by = ~SEX,
                    design = NHIS10_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N10_JS_sex.c <- N10_JS_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N10_JS_sex_ci <- confint(N10_JS_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N10_JS_sex_ci <- N10_JS_sex_ci[-c(1:2), ]
#join proportion and ci
N10.JS.sex <- bind_cols(N10_JS_sex.c, N10_JS_sex_ci)
#save
#write.csv(N10.JS.sex, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N10.JS.sex.csv")


#ii) Logistic regression by sex
N10.JS.sex.glm.c <- svyglm(JNTSYMP~relevel(SEX, ref = "Male") + AGE_P,
                           family = quasibinomial,
                           design = NHIS10_DO)
summary(N10.JS.sex.glm.c)
exp(cbind(OR=coef(N10.JS.sex.glm.c), confint(N10.JS.sex.glm.c)))



#Prevalence of various age-sex groups and their joint pain locations

#1. Right shoulder
N10_JS.rtshoulder <- svyby(formula = ~JMTHP1,
                           by = ~AGE_P + SEX,
                           design = NHIS10_DO,
                           FUN = svymean,
                           na.rm = TRUE)
N10_JS.rtshoulder.c <- N10_JS.rtshoulder %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.RtShoulder")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N10_JS.rtshoulder.ci <- confint(N10_JS.rtshoulder) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N10_JS.rtshoulder.ci <- N10_JS.rtshoulder.ci[-c(23:44), ]
#join proportion and ci
N10.JS.rtshoulder <- bind_cols(N10_JS.rtshoulder.c, N10_JS.rtshoulder.ci)
#save
#write.csv(N10.JS.rtshoulder, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N10.rtshoulder.csv")

#2. Left Shoulder
N10_JS.ltshoulder <- svyby(formula = ~JMTHP2,
                           by = ~AGE_P + SEX,
                           design = NHIS10_DO,
                           FUN = svymean,
                           na.rm = TRUE)
N10_JS.ltshoulder.c <- N10_JS.ltshoulder %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltshoulder")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N10_JS.ltshoulder.ci <- confint(N10_JS.ltshoulder) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N10_JS.ltshoulder.ci <- N10_JS.ltshoulder.ci[-c(23:44), ]
#join proportion and ci
N10.JS.ltshoulder <- bind_cols(N10_JS.ltshoulder.c, N10_JS.ltshoulder.ci)
#save
#write.csv(N10.JS.ltshoulder, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N10.ltshoulder.csv")


#3. Right elbow
N10_JS.rtelbow <- svyby(formula = ~JMTHP3,
                        by = ~AGE_P + SEX,
                        design = NHIS10_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N10_JS.rtelbow.c <- N10_JS.rtelbow %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtelbow")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N10_JS.rtelbow.ci <- confint(N10_JS.rtelbow) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N10_JS.rtelbow.ci <- N10_JS.rtelbow.ci[-c(23:44), ]
#join proportion and ci
N10.JS.rtelbow <- bind_cols(N10_JS.rtelbow.c, N10_JS.rtelbow.ci)
#save
#write.csv(N10.JS.rtelbow, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N10.rtelbow.csv")

#4. Lt elbow
N10_JS.ltelbow <- svyby(formula = ~JMTHP4,
                        by = ~AGE_P + SEX,
                        design = NHIS10_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N10_JS.ltelbow.c <- N10_JS.ltelbow %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltelbow")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N10_JS.ltelbow.ci <- confint(N10_JS.ltelbow) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N10_JS.ltelbow.ci <- N10_JS.ltelbow.ci[-c(23:44), ]
#join proportion and ci
N10.JS.ltelbow <- bind_cols(N10_JS.ltelbow.c, N10_JS.ltelbow.ci)
#save
#write.csv(N10.JS.ltelbow, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N10.ltelbow.csv")

#5. Right hip
N10_JS.rthip <- svyby(formula = ~JMTHP5,
                      by = ~AGE_P + SEX,
                      design = NHIS10_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N10_JS.rthip.c <- N10_JS.rthip %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rthip")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N10_JS.rthip.ci <- confint(N10_JS.rthip) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N10_JS.rthip.ci <- N10_JS.rthip.ci[-c(23:44), ]
#join proportion and ci
N10.JS.rthip <- bind_cols(N10_JS.rthip.c, N10_JS.rthip.ci)
#save
#write.csv(N10.JS.rthip, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N10.rthip.csv")

#6. Left hip
N10_JS.lthip <- svyby(formula = ~JMTHP6,
                      by = ~AGE_P + SEX,
                      design = NHIS10_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N10_JS.lthip.c <- N10_JS.lthip %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.lthip")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N10_JS.lthip.ci <- confint(N10_JS.lthip) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N10_JS.lthip.ci <- N10_JS.lthip.ci[-c(23:44), ]
#join proportion and ci
N10.JS.lthip <- bind_cols(N10_JS.lthip.c, N10_JS.lthip.ci)
#save
#write.csv(N10.JS.lthip, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N10.lthip.csv")

#7. Right wrist
N10_JS.rtwrist <- svyby(formula = ~JMTHP7,
                        by = ~AGE_P + SEX,
                        design = NHIS10_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N10_JS.rtwrist.c <- N10_JS.rtwrist %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtwrist")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N10_JS.rtwrist.ci <- confint(N10_JS.rtwrist) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N10_JS.rtwrist.ci <- N10_JS.rtwrist.ci[-c(23:44), ]
#join proportion and ci
N10.JS.rtwrist <- bind_cols(N10_JS.rtwrist.c, N10_JS.rtwrist.ci)
#save
#write.csv(N10.JS.rtwrist, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N10.rtwrist.csv")

#8. Left wrist
N10_JS.ltwrist <- svyby(formula = ~JMTHP8,
                        by = ~AGE_P + SEX,
                        design = NHIS10_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N10_JS.ltwrist.c <- N10_JS.ltwrist %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltwrist")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N10_JS.ltwrist.ci <- confint(N10_JS.ltwrist) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N10_JS.ltwrist.ci <- N10_JS.ltwrist.ci[-c(23:44), ]
#join proportion and ci
N10.JS.ltwrist <- bind_cols(N10_JS.ltwrist.c, N10_JS.ltwrist.ci)
#save
#write.csv(N10.JS.ltwrist, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N10.ltwrist.csv")

#9. Right knee
N10_JS.rtknee <- svyby(formula = ~JMTHP9,
                       by = ~AGE_P + SEX,
                       design = NHIS10_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N10_JS.rtknee.c <- N10_JS.rtknee %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtknee")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N10_JS.rtknee.ci <- confint(N10_JS.rtknee) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N10_JS.rtknee.ci <- N10_JS.rtknee.ci[-c(23:44), ]
#join proportion and ci
N10.JS.rtknee <- bind_cols(N10_JS.rtknee.c, N10_JS.rtknee.ci)
#save
#write.csv(N10.JS.rtknee, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N10.rtknee.csv")

#10. Left knee
N10_JS.ltknee <- svyby(formula = ~JMTHP10,
                       by = ~AGE_P + SEX,
                       design = NHIS10_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N10_JS.ltknee.c <- N10_JS.ltknee %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltknee")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N10_JS.ltknee.ci <- confint(N10_JS.ltknee) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N10_JS.ltknee.ci <- N10_JS.ltknee.ci[-c(23:44), ]
#join proportion and ci
N10.JS.ltknee <- bind_cols(N10_JS.ltknee.c, N10_JS.ltknee.ci)
#save
#write.csv(N10.JS.ltknee, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N10.ltknee.csv")

#11. Right ankle
N10_JS.rtankle <- svyby(formula = ~JMTHP11,
                        by = ~AGE_P + SEX,
                        design = NHIS10_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N10_JS.rtankle.c <- N10_JS.rtankle %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtankle")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N10_JS.rtankle.ci <- confint(N10_JS.rtankle) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N10_JS.rtankle.ci <- N10_JS.rtankle.ci[-c(23:44), ]
#join proportion and ci
N10.JS.rtankle <- bind_cols(N10_JS.rtankle.c, N10_JS.rtankle.ci)
#save
#write.csv(N10.JS.rtankle, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N10.rtankle.csv")

#12. Left ankle
N10_JS.ltankle <- svyby(formula = ~JMTHP12,
                        by = ~AGE_P + SEX,
                        design = NHIS10_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N10_JS.ltankle.c <- N10_JS.ltankle %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltankle")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N10_JS.ltankle.ci <- confint(N10_JS.ltankle) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N10_JS.ltankle.ci <- N10_JS.ltankle.ci[-c(23:44), ]
#join proportion and ci
N10.JS.ltankle <- bind_cols(N10_JS.ltankle.c, N10_JS.ltankle.ci)
#save
#write.csv(N10.JS.ltankle, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N10.ltankle.csv")

#13. Right toes
N10_JS.rttoes <- svyby(formula = ~JMTHP13,
                       by = ~AGE_P + SEX,
                       design = NHIS10_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N10_JS.rttoes.c <- N10_JS.rttoes %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rttoes")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N10_JS.rttoes.ci <- confint(N10_JS.rttoes) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N10_JS.rttoes.ci <- N10_JS.rttoes.ci[-c(23:44), ]
#join proportion and ci
N10.JS.rttoes <- bind_cols(N10_JS.rttoes.c, N10_JS.rttoes.ci)
#save
#write.csv(N10.JS.rttoes, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N10.rttoes.csv")

#14. Left toes
N10_JS.lttoes <- svyby(formula = ~JMTHP14,
                       by = ~AGE_P + SEX,
                       design = NHIS10_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N10_JS.lttoes.c <- N10_JS.lttoes %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.lttoes")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N10_JS.lttoes.ci <- confint(N10_JS.lttoes) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N10_JS.lttoes.ci <- N10_JS.lttoes.ci[-c(23:44), ]
#join proportion and ci
N10.JS.lttoes <- bind_cols(N10_JS.lttoes.c, N10_JS.lttoes.ci)
#save
#write.csv(N10.JS.lttoes, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N10.lttoes.csv")

#15. Right fingers/thumb
N10_JS.rtfingthumb <- svyby(formula = ~JMTHP15,
                            by = ~AGE_P + SEX,
                            design = NHIS10_DO,
                            FUN = svymean,
                            na.rm = TRUE)
N10_JS.rtfingthumb.c <- N10_JS.rtfingthumb %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtfingthumb")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N10_JS.rtfingthumb.ci <- confint(N10_JS.rtfingthumb) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N10_JS.rtfingthumb.ci <- N10_JS.rtfingthumb.ci[-c(23:44), ]
#join proportion and ci
N10.JS.rtfingthumb <- bind_cols(N10_JS.rtfingthumb.c, N10_JS.rtfingthumb.ci)
#save
#write.csv(N10.JS.rtfingthumb, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N10.rtfingthumb.csv")

#16. Left fingers/thumb
N10_JS.ltfingthumb <- svyby(formula = ~JMTHP16,
                            by = ~AGE_P + SEX,
                            design = NHIS10_DO,
                            FUN = svymean,
                            na.rm = TRUE)
N10_JS.ltfingthumb.c <- N10_JS.ltfingthumb %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltfingthumb")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N10_JS.ltfingthumb.ci <- confint(N10_JS.ltfingthumb) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N10_JS.ltfingthumb.ci <- N10_JS.ltfingthumb.ci[-c(23:44), ]
#join proportion and ci
N10.JS.ltfingthumb <- bind_cols(N10_JS.ltfingthumb.c, N10_JS.ltfingthumb.ci)
#save
#write.csv(N10.JS.ltfingthumb, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N10.ltfingthumb.csv")

#17. Another
N10_JS.another <- svyby(formula = ~JMTHP17,
                        by = ~AGE_P + SEX,
                        design = NHIS10_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N10_JS.another.c <- N10_JS.another %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.another")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N10_JS.another.ci <- confint(N10_JS.another) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N10_JS.another.ci <- N10_JS.another.ci[-c(23:44), ]
#join proportion and ci
N10.JS.another <- bind_cols(N10_JS.another.c, N10_JS.another.ci)
#save
#write.csv(N10.JS.another, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N10.another.csv")

N10.JSloc <- Reduce(function(x, y) merge(x, y, all = TRUE), list(N10_JS.rtshoulder.c, N10_JS.ltshoulder.c, N10_JS.rtelbow.c, N10_JS.ltelbow.c, N10_JS.rthip.c, N10_JS.lthip.c, N10_JS.rtwrist.c, N10_JS.ltwrist.c, N10_JS.rtknee.c, N10_JS.ltknee.c, N10_JS.rtankle.c, N10_JS.ltankle.c, N10_JS.rttoes.c, N10_JS.lttoes.c, N10_JS.rtfingthumb.c, N10_JS.ltfingthumb.c, N10_JS.another.c))

#Wide to long
N10.JSlocation <- gather(N10.JSloc, Location, Proportion, Proportion.RtShoulder:Proportion.another, factor_key = TRUE)
N10.JSlocation <- N10.JSlocation %>% 
  unite("Demographic", Age:Sex)
N10.JSlocation$Location <- as.character(N10.JSlocation$Location)
N10.JSlocation$Location[N10.JSlocation$Location == "Proportion.RtShoulder"] <- "Right shoulder"
N10.JSlocation$Location[N10.JSlocation$Location == "Proportion.ltshoulder"] <- "Left shoulder"
N10.JSlocation$Location[N10.JSlocation$Location == "Proportion.rtelbow"] <- "Right elbow"
N10.JSlocation$Location[N10.JSlocation$Location == "Proportion.ltelbow"] <- "Left elbow"
N10.JSlocation$Location[N10.JSlocation$Location == "Proportion.rthip"] <- "Right hip"
N10.JSlocation$Location[N10.JSlocation$Location == "Proportion.lthip"] <- "Left hip"
N10.JSlocation$Location[N10.JSlocation$Location == "Proportion.rtwrist"] <- "Right wrist"
N10.JSlocation$Location[N10.JSlocation$Location == "Proportion.ltwrist"] <- "Left wrist"
N10.JSlocation$Location[N10.JSlocation$Location == "Proportion.rtknee"] <- "Right knee"
N10.JSlocation$Location[N10.JSlocation$Location == "Proportion.ltknee"] <- "Left knee"
N10.JSlocation$Location[N10.JSlocation$Location == "Proportion.rtankle"] <- "Right ankle"
N10.JSlocation$Location[N10.JSlocation$Location == "Proportion.ltankle"] <- "Left ankle"
N10.JSlocation$Location[N10.JSlocation$Location == "Proportion.rttoes"] <- "Right toes"
N10.JSlocation$Location[N10.JSlocation$Location == "Proportion.lttoes"] <- "Left toes"
N10.JSlocation$Location[N10.JSlocation$Location == "Proportion.rtfingthumb"] <- "Right fingers or thumb"
N10.JSlocation$Location[N10.JSlocation$Location == "Proportion.ltfingthumb"] <- "Left fingers or thumb"
N10.JSlocation$Location[N10.JSlocation$Location == "Proportion.another"] <- "Another joint"

#Heatmap of joint locations by demographic groups
N10.JSlocmap <- ggplot(N10.JSlocation,
                       aes(x = Demographic, y = Location, fill = Proportion)) +
  geom_tile() + 
  scale_fill_viridis(option = "viridis", direction = -1, limits = c(0, 60), breaks = c(10, 20, 30, 40, 50, 60), discrete = FALSE) +
  theme_bw() + 
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.text.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  xlab("Age-sex profile") +
  theme(legend.position = "right",
        strip.background = element_blank()) +
  theme(legend.title = element_text(size = 26)) +
  theme(legend.text = element_text(size = 22)) +
  labs(fill = "Prevalence (%)") +
  guides(fill = guide_colourbar(barwidth = 3, barheight = 25)) +
  scale_x_discrete(labels = c("Females, 18 to 24", "Males, 18 to 24", "Females, 25 to 29", "Males, 25 to 29", "Females, 30 to 34", "Males, 30 to 34", "Females, 35 to 39", "Males, 35 to 39", "Females, 40 to 44", "Males, 40 to 44", "Females, 45 to 49", "Males, 45 to 49", "Females, 50 to 54", "Males, 50 to 54", "Females, 55 to 59", "Males, 55 to 59", "Females, 60 to 64", "Males, 60 to 64", "Females, 65 to 69", "Males, 65 to 69", "Females, 70 and above", "Males, 70 and above")) +
  scale_y_discrete(limits = c("Another joint", "Right toes", "Left toes", "Right ankle", "Left ankle", "Right knee", "Left knee", "Right hip", "Left hip", "Right shoulder", "Left shoulder", "Right elbow", "Left elbow", "Right wrist", "Left wrist", "Right fingers or thumb", "Left fingers or thumb"))
ggsave("N10.age-sex.png", width = 20, height = 13)



#III) Socioeconomic trends (employment status only)

#i)   Prevalence by employment status
N10_JS_emp <- svyby(formula = ~JNTSYMP,
                    by = ~DOINGLWA,
                    design = NHIS10_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N10_JS_emp.c <- N10_JS_emp %>%
  select(1, 3, 5) %>%
  setNames(c("Employment", "Proportion", "SE")) %>%
  mutate(Employment = as.factor(Employment)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N10_JS_emp_ci <- confint(N10_JS_emp) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for js = 0
N10_JS_emp_ci <- N10_JS_emp_ci[-c(1:2), ]
#join ci and proportion
N10.JS.emp <- bind_cols(N10_JS_emp.c, N10_JS_emp_ci) #final proportion, se & 95% ci
#save
#write.csv(N10.JS.emp, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N10.JS.emp.csv")

#ii)  Logistic regression by employment status
N10_JS.emp_glm <- svyglm(JNTSYMP~relevel(DOINGLWA, ref = "Working for pay at a job/business") + AGE_P + SEX,
                         family = quasibinomial,
                         design = NHIS10_DO)
summary(N10_JS.emp_glm)
exp(cbind(OR=coef(N10_JS.emp_glm), confint(N10_JS.emp_glm)))

#4. Lifestyle trends

#I)  BMI
#logistic regression
N10.BMI.glm <- svyglm(JNTSYMP ~ BMI + AGE_P + SEX,
                      family = quasibinomial,
                      design = NHIS10_DO)
summary(N10.BMI.glm)
exp(cbind(OR=coef(N10.BMI.glm), confint(N10.BMI.glm)))

#II) Physical activity
#logistic regression
N10.PA.JS.glm <- svyglm(JNTSYMP ~ relevel(factor(PAC, ordered = FALSE), ref = "Physically active") + AGE_P + SEX,
                        family = quasibinomial,
                        design = NHIS10_DO)
summary(N10.PA.JS.glm)
exp(cbind(OR = coef(N10.PA.JS.glm), confint(N10.PA.JS.glm)))

#____________________________________________________________________________________________________________________

# == 2009 == #

#1. Overall prevalence
N09_JS <- svymean(~factor(JNTSYMP), 
                  NHIS09_DO, 
                  na.rm = TRUE)

N09_JS.c <- N09_JS %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N09_JS_ci <- confint(N09_JS) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
N09.JS <- bind_cols(N09_JS.c, N09_JS_ci)
#remove js = 0
N09.JS <- N09.JS[-c(1), ] #final proportion, se & 95%
#save
#write.csv(N09.JS, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N09.JS.csv")

#2. Spatial analysis, by region

#I)   Overall population

#i)   regional prevalence for the overall population
N09_JS_reg <- svyby(formula = ~JNTSYMP,
                    by = ~REGION,
                    design = NHIS09_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N09_JS_reg.c <- N09_JS_reg %>%
  select(1, 3, 5) %>%
  setNames(c("Region", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N09_JS_reg.ci <- confint(N09_JS_reg) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for js = 0
N09_JS_reg.ci <- N09_JS_reg.ci[-c(1:4), ]
#join proportion and ci
N09.JS.region <- bind_cols(N09_JS_reg.c, N09_JS_reg.ci) #final proportion, se & 95% ci
#save
#write.csv(N09.JS.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N09.JS.region.csv")

#Create overall regional prevalence

#Change the region column to "NAME" to match sf
names(N09.JS.region)[names(N09.JS.region) == "Region"] <- "NAME"

N09.JS.joined <- regions %>%
  left_join(N09.JS.region)

N09.JS.joined$NAME <- as.factor(N09.JS.joined$NAME)

#Plot
JSregplot <- ggplot(data = N09.JS.joined) +
  geom_sf(aes(fill = Proportion), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  scale_fill_viridis_c(name = "Prevalence (%)", option = "viridis", direction = -1, limits = c(25, 40), breaks = c(25, 30, 35, 40)) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  ggtitle("Regional prevalence (%) of US residents with joint symptoms in 2009") +
  theme(plot.title = element_text(size = 10, hjust = 0.5))
ggsave("N09.JSregchor.png", width = 5, height = 5)


#ii)  logistic regression by region
N09.JS.reg.glm <- svyglm(relevel(JNTSYMP, ref = "0")~ REGION + AGE_P,
                         family = quasibinomial,
                         design = NHIS09_DO)
summary(N09.JS.reg.glm)
exp(cbind(OR=coef(N09.JS.reg.glm), confint(N09.JS.reg.glm)))

#II)  prevalence by region, by age

#i)  age-specific regional prevalance
N09_JS_reg.age <- svyby(formula = ~JNTSYMP,
                        by = ~REGION + AGE_P,
                        design = NHIS09_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N09_JS_reg.age.c <- N09_JS_reg.age %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Age", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Age = as.character(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N09_JS_reg.age.ci <- confint(N09_JS_reg.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for js = 0
N09_JS_reg.age.ci <- N09_JS_reg.age.ci[-c(1:44), ]
#join proportion and ci
N09.JS.age.region <- bind_cols(N09_JS_reg.age.c, N09_JS_reg.age.ci) #final proportion, se & 95% ci
#save
#write.csv(N09.JS.age.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N09.JS.region.age.csv")

#Geofacet plot - Age & Region
JS.Age_Reg <- ggplot(N09.JS.age.region, aes(Age, Proportion)) +
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
  ggtitle("Regional prevalence (%) of US residents with joint symptoms by age group in 2009") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N09.GF_AgeReg.JS.png", width = 14, height = 9)


#ii) age-specific regional logistic regressions
#18 to 24
sub1824 <- subset(NHIS09_DO, AGE_P == "18 to 24")
N09.JS.age1824.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub1824)
summary(N09.JS.age1824.reg)
exp(cbind(OR = coef(N09.JS.age1824.reg), confint(N09.JS.age1824.reg)))

#25 to 29
sub2529 <- subset(NHIS09_DO, AGE_P == "25 to 29")
N09.JS.age2529.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub2529)
summary(N09.JS.age2529.reg)
exp(cbind(OR = coef(N09.JS.age2529.reg), confint(N09.JS.age2529.reg)))

#30 to 34
sub3034 <- subset(NHIS09_DO, AGE_P == "30 to 34")
N09.JS.age3034.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub3034)
summary(N09.JS.age3034.reg)
exp(cbind(OR = coef(N09.JS.age3034.reg), confint(N09.JS.age3034.reg)))

#35 to 39
sub3539 <- subset(NHIS09_DO, AGE_P == "35 to 39")
N09.JS.age3539.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub3539)
summary(N09.JS.age3539.reg)
exp(cbind(OR = coef(N09.JS.age3539.reg), confint(N09.JS.age3539.reg)))

#40 to 44
sub4044 <- subset(NHIS09_DO, AGE_P == "40 to 44")
N09.JS.age4044.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub4044)
summary(N09.JS.age4044.reg)
exp(cbind(OR = coef(N09.JS.age4044.reg), confint(N09.JS.age4044.reg)))

#45 to 49
sub4549 <- subset(NHIS09_DO, AGE_P == "45 to 49")
N09.JS.age4549.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub4549)
summary(N09.JS.age4549.reg)
exp(cbind(OR = coef(N09.JS.age4549.reg), confint(N09.JS.age4549.reg)))

#50 to 54
sub5054 <- subset(NHIS09_DO, AGE_P =="50 to 54")
N09.JS.age5054.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub5054)
summary(N09.JS.age5054.reg)
exp(cbind(OR = coef(N09.JS.age5054.reg), confint(N09.JS.age5054.reg)))

#55 to 59
sub5559 <- subset(NHIS09_DO, AGE_P == "55 to 59")
N09.JS.age5559.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub5559)
summary(N09.JS.age5559.reg)
exp(cbind(OR = coef(N09.JS.age5559.reg), confint(N09.JS.age5559.reg)))

#60 to 64
sub6064 <- subset(NHIS09_DO, AGE_P == "60 to 64")
N09.JS.age6064.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub6064)
summary(N09.JS.age6064.reg)
exp(cbind(OR = coef(N09.JS.age6064.reg), confint(N09.JS.age6064.reg)))

#65 to 69
sub6569 <- subset(NHIS09_DO, AGE_P == "65 to 69")
N09.JS.age6569.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub6569)
summary(N09.JS.age6569.reg)
exp(cbind(OR = coef(N09.JS.age6569.reg), confint(N09.JS.age6569.reg)))

#70 and above
sub70 <- subset(NHIS09_DO, AGE_P == "70 and above")
N09.JS.age70.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                           family = quasibinomial,
                           design = sub70)
summary(N09.JS.age70.reg)
exp(cbind(OR = coef(N09.JS.age70.reg), confint(N09.JS.age70.reg)))

#III) prevalence by region, by sex

#i)  sex-specific regional prevalence
N09_JS_reg.sex <- svyby(formula = ~JNTSYMP,
                        by = ~REGION + SEX,
                        design = NHIS09_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N09_JS_reg.sex.c <- N09_JS_reg.sex %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Sex", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N09_JS_reg.sex.ci <- confint(N09_JS_reg.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for js = 0
N09_JS_reg.sex.ci <- N09_JS_reg.sex.ci[-c(1:8), ]
#join proportion and ci
N09.JS.sex.region <- bind_cols(N09_JS_reg.sex.c, N09_JS_reg.sex.ci) #final proprtion, se & 95% ci
#save
#write.csv(N09.JS.sex.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N09.JS.region.sex.csv")

#Geofacet - Sex & Region
JS.Sex_Reg09 <- ggplot(N09.JS.sex.region, aes(Sex, Proportion)) +
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
  ggtitle("Regional prevalence (%) of US residents with joint symptoms by sex in 2009") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N09.GF_SexReg.JS.png", width = 14, height = 9)


#ii) sex-specific regional logistic regressions
#males
submales <- subset(NHIS09_DO, SEX == "Male")
N09.JS.sexmale.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + AGE_P,
                             family = quasibinomial,
                             design = submales)
summary(N09.JS.sexmale.reg)
exp(cbind(OR = coef(N09.JS.sexmale.reg), confint(N09.JS.sexmale.reg)))

#females
subfemales <- subset(NHIS09_DO, SEX == "Female")
N09.JS.sexfemale.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + AGE_P,
                               family = quasibinomial,
                               design = subfemales)
summary(N09.JS.sexfemale.reg)
exp(cbind(OR = coef(N09.JS.sexfemale.reg), confint(N09.JS.sexfemale.reg)))


#3. Demographic trends

#I)  Age trends

#i)  Prevalence by age
N09_JS_age <- svyby(formula = ~JNTSYMP,
                    by = ~AGE_P,
                    design = NHIS09_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N09_JS_age.c <- N09_JS_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N09_JS_age_ci <- confint(N09_JS_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove js=0 ci
N09_JS_age_ci <- N09_JS_age_ci[-c(1:11), ]
#join proportion and ci
N09_JS.age <- bind_cols(N09_JS_age.c, N09_JS_age_ci) #final proportion, se & 95% ci
#Save
#write.csv(N09_JS.age, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N09.JS.age.csv")


#ii) Logistic regression by age
N09_JS.age_glm <- svyglm(JNTSYMP~AGE_P + SEX,
                         family = quasibinomial,
                         design = NHIS09_DO)
summary(N09_JS.age_glm)
exp(cbind(OR=coef(N09_JS.age_glm), confint(N09_JS.age_glm)))



#II)  Sex trends

#i)  Prevalence by sex
N09_JS_sex <- svyby(formula = ~JNTSYMP,
                    by = ~SEX,
                    design = NHIS09_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N09_JS_sex.c <- N09_JS_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N09_JS_sex_ci <- confint(N09_JS_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N09_JS_sex_ci <- N09_JS_sex_ci[-c(1:2), ]
#join proportion and ci
N09.JS.sex <- bind_cols(N09_JS_sex.c, N09_JS_sex_ci)
#save
#write.csv(N09.JS.sex, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N09.JS.sex.csv")


#ii) Logistic regression by sex
N09.JS.sex.glm.c <- svyglm(JNTSYMP~relevel(SEX, ref = "Male") + AGE_P,
                           family = quasibinomial,
                           design = NHIS09_DO)
summary(N09.JS.sex.glm.c)
exp(cbind(OR=coef(N09.JS.sex.glm.c), confint(N09.JS.sex.glm.c)))


#Prevalence of various age-sex groups and their joint pain locations

#1. Right shoulder
N09_JS.rtshoulder <- svyby(formula = ~JMTHP1,
                           by = ~AGE_P + SEX,
                           design = NHIS09_DO,
                           FUN = svymean,
                           na.rm = TRUE)
N09_JS.rtshoulder.c <- N09_JS.rtshoulder %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.RtShoulder")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N09_JS.rtshoulder.ci <- confint(N09_JS.rtshoulder) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N09_JS.rtshoulder.ci <- N09_JS.rtshoulder.ci[-c(23:44), ]
#join proportion and ci
N09.JS.rtshoulder <- bind_cols(N09_JS.rtshoulder.c, N09_JS.rtshoulder.ci)
#save
#write.csv(N09.JS.rtshoulder, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N09.rtshoulder.csv")

#2. Left Shoulder
N09_JS.ltshoulder <- svyby(formula = ~JMTHP2,
                           by = ~AGE_P + SEX,
                           design = NHIS09_DO,
                           FUN = svymean,
                           na.rm = TRUE)
N09_JS.ltshoulder.c <- N09_JS.ltshoulder %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltshoulder")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N09_JS.ltshoulder.ci <- confint(N09_JS.ltshoulder) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N09_JS.ltshoulder.ci <- N09_JS.ltshoulder.ci[-c(23:44), ]
#join proportion and ci
N09.JS.ltshoulder <- bind_cols(N09_JS.ltshoulder.c, N09_JS.ltshoulder.ci)
#save
#write.csv(N09.JS.ltshoulder, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N09.ltshoulder.csv")


#3. Right elbow
N09_JS.rtelbow <- svyby(formula = ~JMTHP3,
                        by = ~AGE_P + SEX,
                        design = NHIS09_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N09_JS.rtelbow.c <- N09_JS.rtelbow %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtelbow")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N09_JS.rtelbow.ci <- confint(N09_JS.rtelbow) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N09_JS.rtelbow.ci <- N09_JS.rtelbow.ci[-c(23:44), ]
#join proportion and ci
N09.JS.rtelbow <- bind_cols(N09_JS.rtelbow.c, N09_JS.rtelbow.ci)
#save
#write.csv(N09.JS.rtelbow, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N09.rtelbow.csv")

#4. Lt elbow
N09_JS.ltelbow <- svyby(formula = ~JMTHP4,
                        by = ~AGE_P + SEX,
                        design = NHIS09_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N09_JS.ltelbow.c <- N09_JS.ltelbow %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltelbow")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N09_JS.ltelbow.ci <- confint(N09_JS.ltelbow) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N09_JS.ltelbow.ci <- N09_JS.ltelbow.ci[-c(23:44), ]
#join proportion and ci
N09.JS.ltelbow <- bind_cols(N09_JS.ltelbow.c, N09_JS.ltelbow.ci)
#save
#write.csv(N09.JS.ltelbow, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N09.ltelbow.csv")

#5. Right hip
N09_JS.rthip <- svyby(formula = ~JMTHP5,
                      by = ~AGE_P + SEX,
                      design = NHIS09_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N09_JS.rthip.c <- N09_JS.rthip %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rthip")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N09_JS.rthip.ci <- confint(N09_JS.rthip) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N09_JS.rthip.ci <- N09_JS.rthip.ci[-c(23:44), ]
#join proportion and ci
N09.JS.rthip <- bind_cols(N09_JS.rthip.c, N09_JS.rthip.ci)
#save
#write.csv(N09.JS.rthip, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N09.rthip.csv")

#6. Left hip
N09_JS.lthip <- svyby(formula = ~JMTHP6,
                      by = ~AGE_P + SEX,
                      design = NHIS09_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N09_JS.lthip.c <- N09_JS.lthip %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.lthip")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N09_JS.lthip.ci <- confint(N09_JS.lthip) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N09_JS.lthip.ci <- N09_JS.lthip.ci[-c(23:44), ]
#join proportion and ci
N09.JS.lthip <- bind_cols(N09_JS.lthip.c, N09_JS.lthip.ci)
#save
#write.csv(N09.JS.lthip, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N09.lthip.csv")

#7. Right wrist
N09_JS.rtwrist <- svyby(formula = ~JMTHP7,
                        by = ~AGE_P + SEX,
                        design = NHIS09_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N09_JS.rtwrist.c <- N09_JS.rtwrist %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtwrist")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N09_JS.rtwrist.ci <- confint(N09_JS.rtwrist) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N09_JS.rtwrist.ci <- N09_JS.rtwrist.ci[-c(23:44), ]
#join proportion and ci
N09.JS.rtwrist <- bind_cols(N09_JS.rtwrist.c, N09_JS.rtwrist.ci)
#save
#write.csv(N09.JS.rtwrist, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N09.rtwrist.csv")

#8. Left wrist
N09_JS.ltwrist <- svyby(formula = ~JMTHP8,
                        by = ~AGE_P + SEX,
                        design = NHIS09_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N09_JS.ltwrist.c <- N09_JS.ltwrist %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltwrist")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N09_JS.ltwrist.ci <- confint(N09_JS.ltwrist) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N09_JS.ltwrist.ci <- N09_JS.ltwrist.ci[-c(23:44), ]
#join proportion and ci
N09.JS.ltwrist <- bind_cols(N09_JS.ltwrist.c, N09_JS.ltwrist.ci)
#save
#write.csv(N09.JS.ltwrist, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N09.ltwrist.csv")

#9. Right knee
N09_JS.rtknee <- svyby(formula = ~JMTHP9,
                       by = ~AGE_P + SEX,
                       design = NHIS09_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N09_JS.rtknee.c <- N09_JS.rtknee %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtknee")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N09_JS.rtknee.ci <- confint(N09_JS.rtknee) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N09_JS.rtknee.ci <- N09_JS.rtknee.ci[-c(23:44), ]
#join proportion and ci
N09.JS.rtknee <- bind_cols(N09_JS.rtknee.c, N09_JS.rtknee.ci)
#save
#write.csv(N09.JS.rtknee, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N09.rtknee.csv")

#10. Left knee
N09_JS.ltknee <- svyby(formula = ~JMTHP10,
                       by = ~AGE_P + SEX,
                       design = NHIS09_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N09_JS.ltknee.c <- N09_JS.ltknee %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltknee")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N09_JS.ltknee.ci <- confint(N09_JS.ltknee) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N09_JS.ltknee.ci <- N09_JS.ltknee.ci[-c(23:44), ]
#join proportion and ci
N09.JS.ltknee <- bind_cols(N09_JS.ltknee.c, N09_JS.ltknee.ci)
#save
#write.csv(N09.JS.ltknee, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N09.ltknee.csv")

#11. Right ankle
N09_JS.rtankle <- svyby(formula = ~JMTHP11,
                        by = ~AGE_P + SEX,
                        design = NHIS09_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N09_JS.rtankle.c <- N09_JS.rtankle %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtankle")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N09_JS.rtankle.ci <- confint(N09_JS.rtankle) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N09_JS.rtankle.ci <- N09_JS.rtankle.ci[-c(23:44), ]
#join proportion and ci
N09.JS.rtankle <- bind_cols(N09_JS.rtankle.c, N09_JS.rtankle.ci)
#save
#write.csv(N09.JS.rtankle, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N09.rtankle.csv")

#12. Left ankle
N09_JS.ltankle <- svyby(formula = ~JMTHP12,
                        by = ~AGE_P + SEX,
                        design = NHIS09_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N09_JS.ltankle.c <- N09_JS.ltankle %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltankle")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N09_JS.ltankle.ci <- confint(N09_JS.ltankle) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N09_JS.ltankle.ci <- N09_JS.ltankle.ci[-c(23:44), ]
#join proportion and ci
N09.JS.ltankle <- bind_cols(N09_JS.ltankle.c, N09_JS.ltankle.ci)
#save
#write.csv(N09.JS.ltankle, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N09.ltankle.csv")

#13. Right toes
N09_JS.rttoes <- svyby(formula = ~JMTHP13,
                       by = ~AGE_P + SEX,
                       design = NHIS09_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N09_JS.rttoes.c <- N09_JS.rttoes %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rttoes")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N09_JS.rttoes.ci <- confint(N09_JS.rttoes) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N09_JS.rttoes.ci <- N09_JS.rttoes.ci[-c(23:44), ]
#join proportion and ci
N09.JS.rttoes <- bind_cols(N09_JS.rttoes.c, N09_JS.rttoes.ci)
#save
#write.csv(N09.JS.rttoes, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N09.rttoes.csv")

#14. Left toes
N09_JS.lttoes <- svyby(formula = ~JMTHP14,
                       by = ~AGE_P + SEX,
                       design = NHIS09_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N09_JS.lttoes.c <- N09_JS.lttoes %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.lttoes")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N09_JS.lttoes.ci <- confint(N09_JS.lttoes) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N09_JS.lttoes.ci <- N09_JS.lttoes.ci[-c(23:44), ]
#join proportion and ci
N09.JS.lttoes <- bind_cols(N09_JS.lttoes.c, N09_JS.lttoes.ci)
#save
#write.csv(N09.JS.lttoes, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N09.lttoes.csv")

#15. Right fingers/thumb
N09_JS.rtfingthumb <- svyby(formula = ~JMTHP15,
                            by = ~AGE_P + SEX,
                            design = NHIS09_DO,
                            FUN = svymean,
                            na.rm = TRUE)
N09_JS.rtfingthumb.c <- N09_JS.rtfingthumb %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtfingthumb")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N09_JS.rtfingthumb.ci <- confint(N09_JS.rtfingthumb) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N09_JS.rtfingthumb.ci <- N09_JS.rtfingthumb.ci[-c(23:44), ]
#join proportion and ci
N09.JS.rtfingthumb <- bind_cols(N09_JS.rtfingthumb.c, N09_JS.rtfingthumb.ci)
#save
#write.csv(N09.JS.rtfingthumb, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N09.rtfingthumb.csv")

#16. Left fingers/thumb
N09_JS.ltfingthumb <- svyby(formula = ~JMTHP16,
                            by = ~AGE_P + SEX,
                            design = NHIS09_DO,
                            FUN = svymean,
                            na.rm = TRUE)
N09_JS.ltfingthumb.c <- N09_JS.ltfingthumb %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltfingthumb")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N09_JS.ltfingthumb.ci <- confint(N09_JS.ltfingthumb) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N09_JS.ltfingthumb.ci <- N09_JS.ltfingthumb.ci[-c(23:44), ]
#join proportion and ci
N09.JS.ltfingthumb <- bind_cols(N09_JS.ltfingthumb.c, N09_JS.ltfingthumb.ci)
#save
#write.csv(N09.JS.ltfingthumb, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N09.ltfingthumb.csv")

#17. Another
N09_JS.another <- svyby(formula = ~JMTHP17,
                        by = ~AGE_P + SEX,
                        design = NHIS09_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N09_JS.another.c <- N09_JS.another %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.another")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N09_JS.another.ci <- confint(N09_JS.another) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N09_JS.another.ci <- N09_JS.another.ci[-c(23:44), ]
#join proportion and ci
N09.JS.another <- bind_cols(N09_JS.another.c, N09_JS.another.ci)
#save
#write.csv(N09.JS.another, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N09.another.csv")

N09.JSloc <- Reduce(function(x, y) merge(x, y, all = TRUE), list(N09_JS.rtshoulder.c, N09_JS.ltshoulder.c, N09_JS.rtelbow.c, N09_JS.ltelbow.c, N09_JS.rthip.c, N09_JS.lthip.c, N09_JS.rtwrist.c, N09_JS.ltwrist.c, N09_JS.rtknee.c, N09_JS.ltknee.c, N09_JS.rtankle.c, N09_JS.ltankle.c, N09_JS.rttoes.c, N09_JS.lttoes.c, N09_JS.rtfingthumb.c, N09_JS.ltfingthumb.c, N09_JS.another.c))

#Wide to long
N09.JSlocation <- gather(N09.JSloc, Location, Proportion, Proportion.RtShoulder:Proportion.another, factor_key = TRUE)
N09.JSlocation <- N09.JSlocation %>% 
  unite("Demographic", Age:Sex)
N09.JSlocation$Location <- as.character(N09.JSlocation$Location)
N09.JSlocation$Location[N09.JSlocation$Location == "Proportion.RtShoulder"] <- "Right shoulder"
N09.JSlocation$Location[N09.JSlocation$Location == "Proportion.ltshoulder"] <- "Left shoulder"
N09.JSlocation$Location[N09.JSlocation$Location == "Proportion.rtelbow"] <- "Right elbow"
N09.JSlocation$Location[N09.JSlocation$Location == "Proportion.ltelbow"] <- "Left elbow"
N09.JSlocation$Location[N09.JSlocation$Location == "Proportion.rthip"] <- "Right hip"
N09.JSlocation$Location[N09.JSlocation$Location == "Proportion.lthip"] <- "Left hip"
N09.JSlocation$Location[N09.JSlocation$Location == "Proportion.rtwrist"] <- "Right wrist"
N09.JSlocation$Location[N09.JSlocation$Location == "Proportion.ltwrist"] <- "Left wrist"
N09.JSlocation$Location[N09.JSlocation$Location == "Proportion.rtknee"] <- "Right knee"
N09.JSlocation$Location[N09.JSlocation$Location == "Proportion.ltknee"] <- "Left knee"
N09.JSlocation$Location[N09.JSlocation$Location == "Proportion.rtankle"] <- "Right ankle"
N09.JSlocation$Location[N09.JSlocation$Location == "Proportion.ltankle"] <- "Left ankle"
N09.JSlocation$Location[N09.JSlocation$Location == "Proportion.rttoes"] <- "Right toes"
N09.JSlocation$Location[N09.JSlocation$Location == "Proportion.lttoes"] <- "Left toes"
N09.JSlocation$Location[N09.JSlocation$Location == "Proportion.rtfingthumb"] <- "Right fingers or thumb"
N09.JSlocation$Location[N09.JSlocation$Location == "Proportion.ltfingthumb"] <- "Left fingers or thumb"
N09.JSlocation$Location[N09.JSlocation$Location == "Proportion.another"] <- "Another joint"

#Heatmap of joint locations by demographic groups
N09.JSlocmap <- ggplot(N09.JSlocation,
                       aes(x = Demographic, y = Location, fill = Proportion)) +
  geom_tile() + 
  scale_fill_viridis(option = "viridis", direction = -1, limits = c(0, 60), breaks = c(10, 20, 30, 40, 50, 60), discrete = FALSE) +
  theme_bw() + 
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.text.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  xlab("Age-sex profile") +
  theme(legend.position = "right",
        strip.background = element_blank()) +
  theme(legend.title = element_text(size = 26)) +
  theme(legend.text = element_text(size = 22)) +
  labs(fill = "Prevalence (%)") +
  guides(fill = guide_colourbar(barwidth = 3, barheight = 25)) +
  scale_x_discrete(labels = c("Females, 18 to 24", "Males, 18 to 24", "Females, 25 to 29", "Males, 25 to 29", "Females, 30 to 34", "Males, 30 to 34", "Females, 35 to 39", "Males, 35 to 39", "Females, 40 to 44", "Males, 40 to 44", "Females, 45 to 49", "Males, 45 to 49", "Females, 50 to 54", "Males, 50 to 54", "Females, 55 to 59", "Males, 55 to 59", "Females, 60 to 64", "Males, 60 to 64", "Females, 65 to 69", "Males, 65 to 69", "Females, 70 and above", "Males, 70 and above")) +
  scale_y_discrete(limits = c("Another joint", "Right toes", "Left toes", "Right ankle", "Left ankle", "Right knee", "Left knee", "Right hip", "Left hip", "Right shoulder", "Left shoulder", "Right elbow", "Left elbow", "Right wrist", "Left wrist", "Right fingers or thumb", "Left fingers or thumb"))
ggsave("N09.age-sex.png", width = 20, height = 13)



#III) Socioeconomic trends (employment status only)

#i)   Prevalence by employment status
N09_JS_emp <- svyby(formula = ~JNTSYMP,
                    by = ~DOINGLWA,
                    design = NHIS09_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N09_JS_emp.c <- N09_JS_emp %>%
  select(1, 3, 5) %>%
  setNames(c("Employment", "Proportion", "SE")) %>%
  mutate(Employment = as.factor(Employment)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N09_JS_emp_ci <- confint(N09_JS_emp) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for js = 0
N09_JS_emp_ci <- N09_JS_emp_ci[-c(1:2), ]
#join ci and proportion
N09.JS.emp <- bind_cols(N09_JS_emp.c, N09_JS_emp_ci) #final proportion, se & 95% ci
#save
#write.csv(N09.JS.emp, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N09.JS.emp.csv")

#ii)  Logistic regression by employment status
N09_JS.emp_glm <- svyglm(JNTSYMP~relevel(DOINGLWA, ref = "Working for pay at a job/business") + AGE_P + SEX,
                         family = quasibinomial,
                         design = NHIS09_DO)
summary(N09_JS.emp_glm)
exp(cbind(OR=coef(N09_JS.emp_glm), confint(N09_JS.emp_glm)))

#4. Lifestyle trends

#I)  BMI
#logistic regression
N09.BMI.glm <- svyglm(JNTSYMP ~ BMI + AGE_P + SEX,
                      family = quasibinomial,
                      design = NHIS09_DO)
summary(N09.BMI.glm)
exp(cbind(OR=coef(N09.BMI.glm), confint(N09.BMI.glm)))

#II) Physical activity
#logistic regression
N09.PA.JS.glm <- svyglm(JNTSYMP ~ relevel(factor(PAC, ordered = FALSE), ref = "Physically active") + AGE_P + SEX,
                        family = quasibinomial,
                        design = NHIS09_DO)
summary(N09.PA.JS.glm)
exp(cbind(OR = coef(N09.PA.JS.glm), confint(N09.PA.JS.glm)))

#_____________________________________________________________________________________________________________________

# == 2008 == #

#1. Overall prevalence
N08_JS <- svymean(~factor(JNTSYMP), 
                  NHIS08_DO, 
                  na.rm = TRUE)

N08_JS.c <- N08_JS %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N08_JS_ci <- confint(N08_JS) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
N08.JS <- bind_cols(N08_JS.c, N08_JS_ci)
#remove js = 0
N08.JS <- N08.JS[-c(1), ] #final proportion, se & 95%
#save
#write.csv(N08.JS, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N08.JS.csv")

#2. Spatial analysis, by region

#I)   Overall population

#i)   regional prevalence for the overall population
N08_JS_reg <- svyby(formula = ~JNTSYMP,
                    by = ~REGION,
                    design = NHIS08_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N08_JS_reg.c <- N08_JS_reg %>%
  select(1, 3, 5) %>%
  setNames(c("Region", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N08_JS_reg.ci <- confint(N08_JS_reg) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for js = 0
N08_JS_reg.ci <- N08_JS_reg.ci[-c(1:4), ]
#join proportion and ci
N08.JS.region <- bind_cols(N08_JS_reg.c, N08_JS_reg.ci) #final proportion, se & 95% ci
#save
#write.csv(N08.JS.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N08.JS.region.csv")

#Create overall regional prevalence

#Change the region column to "NAME" to match sf
names(N08.JS.region)[names(N08.JS.region) == "Region"] <- "NAME"

N08.JS.joined <- regions %>%
  left_join(N08.JS.region)

N08.JS.joined$NAME <- as.factor(N08.JS.joined$NAME)

#Plot
JSregplot <- ggplot(data = N08.JS.joined) +
  geom_sf(aes(fill = Proportion), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  scale_fill_viridis_c(name = "Prevalence (%)", option = "viridis", direction = -1, limits = c(25, 40), breaks = c(25, 30, 35, 40)) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  ggtitle("Regional prevalence (%) of US residents with joint symptoms in 2008") +
  theme(plot.title = element_text(size = 10, hjust = 0.5))
ggsave("N08.JSregchor.png", width = 5, height = 5)


#ii)  logistic regression by region
N08.JS.reg.glm <- svyglm(relevel(JNTSYMP, ref = "0")~ REGION + AGE_P,
                         family = quasibinomial,
                         design = NHIS08_DO)
summary(N08.JS.reg.glm)
exp(cbind(OR=coef(N08.JS.reg.glm), confint(N08.JS.reg.glm)))

#II)  prevalence by region, by age

#i)  age-specific regional prevalance
N08_JS_reg.age <- svyby(formula = ~JNTSYMP,
                        by = ~REGION + AGE_P,
                        design = NHIS08_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N08_JS_reg.age.c <- N08_JS_reg.age %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Age", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Age = as.character(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N08_JS_reg.age.ci <- confint(N08_JS_reg.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for js = 0
N08_JS_reg.age.ci <- N08_JS_reg.age.ci[-c(1:44), ]
#join proportion and ci
N08.JS.age.region <- bind_cols(N08_JS_reg.age.c, N08_JS_reg.age.ci) #final proportion, se & 95% ci
#save
#write.csv(N08.JS.age.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N08.JS.region.age.csv")

#Geofacet plot - Age & Region
JS.Age_Reg <- ggplot(N08.JS.age.region, aes(Age, Proportion)) +
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
  ggtitle("Regional prevalence (%) of US residents with joint symptoms by age group in 2008") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N08.GF_AgeReg.JS.png", width = 14, height = 9)


#ii) age-specific regional logistic regressions
#18 to 24
sub1824 <- subset(NHIS08_DO, AGE_P == "18 to 24")
N08.JS.age1824.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub1824)
summary(N08.JS.age1824.reg)
exp(cbind(OR = coef(N08.JS.age1824.reg), confint(N08.JS.age1824.reg)))

#25 to 29
sub2529 <- subset(NHIS08_DO, AGE_P == "25 to 29")
N08.JS.age2529.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub2529)
summary(N08.JS.age2529.reg)
exp(cbind(OR = coef(N08.JS.age2529.reg), confint(N08.JS.age2529.reg)))

#30 to 34
sub3034 <- subset(NHIS08_DO, AGE_P == "30 to 34")
N08.JS.age3034.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub3034)
summary(N08.JS.age3034.reg)
exp(cbind(OR = coef(N08.JS.age3034.reg), confint(N08.JS.age3034.reg)))

#35 to 39
sub3539 <- subset(NHIS08_DO, AGE_P == "35 to 39")
N08.JS.age3539.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub3539)
summary(N08.JS.age3539.reg)
exp(cbind(OR = coef(N08.JS.age3539.reg), confint(N08.JS.age3539.reg)))

#40 to 44
sub4044 <- subset(NHIS08_DO, AGE_P == "40 to 44")
N08.JS.age4044.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub4044)
summary(N08.JS.age4044.reg)
exp(cbind(OR = coef(N08.JS.age4044.reg), confint(N08.JS.age4044.reg)))

#45 to 49
sub4549 <- subset(NHIS08_DO, AGE_P == "45 to 49")
N08.JS.age4549.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub4549)
summary(N08.JS.age4549.reg)
exp(cbind(OR = coef(N08.JS.age4549.reg), confint(N08.JS.age4549.reg)))

#50 to 54
sub5054 <- subset(NHIS08_DO, AGE_P =="50 to 54")
N08.JS.age5054.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub5054)
summary(N08.JS.age5054.reg)
exp(cbind(OR = coef(N08.JS.age5054.reg), confint(N08.JS.age5054.reg)))

#55 to 59
sub5559 <- subset(NHIS08_DO, AGE_P == "55 to 59")
N08.JS.age5559.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub5559)
summary(N08.JS.age5559.reg)
exp(cbind(OR = coef(N08.JS.age5559.reg), confint(N08.JS.age5559.reg)))

#60 to 64
sub6064 <- subset(NHIS08_DO, AGE_P == "60 to 64")
N08.JS.age6064.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub6064)
summary(N08.JS.age6064.reg)
exp(cbind(OR = coef(N08.JS.age6064.reg), confint(N08.JS.age6064.reg)))

#65 to 69
sub6569 <- subset(NHIS08_DO, AGE_P == "65 to 69")
N08.JS.age6569.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub6569)
summary(N08.JS.age6569.reg)
exp(cbind(OR = coef(N08.JS.age6569.reg), confint(N08.JS.age6569.reg)))

#70 and above
sub70 <- subset(NHIS08_DO, AGE_P == "70 and above")
N08.JS.age70.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                           family = quasibinomial,
                           design = sub70)
summary(N08.JS.age70.reg)
exp(cbind(OR = coef(N08.JS.age70.reg), confint(N08.JS.age70.reg)))

#III) prevalence by region, by sex

#i)  sex-specific regional prevalence
N08_JS_reg.sex <- svyby(formula = ~JNTSYMP,
                        by = ~REGION + SEX,
                        design = NHIS08_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N08_JS_reg.sex.c <- N08_JS_reg.sex %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Sex", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N08_JS_reg.sex.ci <- confint(N08_JS_reg.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for js = 0
N08_JS_reg.sex.ci <- N08_JS_reg.sex.ci[-c(1:8), ]
#join proportion and ci
N08.JS.sex.region <- bind_cols(N08_JS_reg.sex.c, N08_JS_reg.sex.ci) #final proprtion, se & 95% ci
#save
#write.csv(N08.JS.sex.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N08.JS.region.sex.csv")

#Geofacet - Sex & Region
JS.Sex_Reg08 <- ggplot(N08.JS.sex.region, aes(Sex, Proportion)) +
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
  ggtitle("Regional prevalence of US residents with joint symptoms by sex in 2008") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N08.GF_SexReg.JS.png", width = 14, height = 9)


#ii) sex-specific regional logistic regressions
#males
submales <- subset(NHIS08_DO, SEX == "Male")
N08.JS.sexmale.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + AGE_P,
                             family = quasibinomial,
                             design = submales)
summary(N08.JS.sexmale.reg)
exp(cbind(OR = coef(N08.JS.sexmale.reg), confint(N08.JS.sexmale.reg)))

#females
subfemales <- subset(NHIS08_DO, SEX == "Female")
N08.JS.sexfemale.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + AGE_P,
                               family = quasibinomial,
                               design = subfemales)
summary(N08.JS.sexfemale.reg)
exp(cbind(OR = coef(N08.JS.sexfemale.reg), confint(N08.JS.sexfemale.reg)))


#3. Demographic trends

#I)  Age trends

#i)  Prevalence by age
N08_JS_age <- svyby(formula = ~JNTSYMP,
                    by = ~AGE_P,
                    design = NHIS08_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N08_JS_age.c <- N08_JS_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N08_JS_age_ci <- confint(N08_JS_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove js=0 ci
N08_JS_age_ci <- N08_JS_age_ci[-c(1:11), ]
#join proportion and ci
N08_JS.age <- bind_cols(N08_JS_age.c, N08_JS_age_ci) #final proportion, se & 95% ci
#Save
#write.csv(N08_JS.age, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N08.JS.age.csv")


#ii) Logistic regression by age
N08_JS.age_glm <- svyglm(JNTSYMP~AGE_P + SEX,
                         family = quasibinomial,
                         design = NHIS08_DO)
summary(N08_JS.age_glm)
exp(cbind(OR=coef(N08_JS.age_glm), confint(N08_JS.age_glm)))



#II)  Sex trends

#i)  Prevalence by sex
N08_JS_sex <- svyby(formula = ~JNTSYMP,
                    by = ~SEX,
                    design = NHIS08_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N08_JS_sex.c <- N08_JS_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N08_JS_sex_ci <- confint(N08_JS_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N08_JS_sex_ci <- N08_JS_sex_ci[-c(1:2), ]
#join proportion and ci
N08.JS.sex <- bind_cols(N08_JS_sex.c, N08_JS_sex_ci)
#save
#write.csv(N08.JS.sex, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N08.JS.sex.csv")


#ii) Logistic regression by sex
N08.JS.sex.glm.c <- svyglm(JNTSYMP~relevel(SEX, ref = "Male") + AGE_P,
                           family = quasibinomial,
                           design = NHIS08_DO)
summary(N08.JS.sex.glm.c)
exp(cbind(OR=coef(N08.JS.sex.glm.c), confint(N08.JS.sex.glm.c)))


#Prevalence of various age-sex groups and their joint pain locations

#1. Right shoulder
N08_JS.rtshoulder <- svyby(formula = ~JMTHP1,
                           by = ~AGE_P + SEX,
                           design = NHIS08_DO,
                           FUN = svymean,
                           na.rm = TRUE)
N08_JS.rtshoulder.c <- N08_JS.rtshoulder %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.RtShoulder")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N08_JS.rtshoulder.ci <- confint(N08_JS.rtshoulder) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N08_JS.rtshoulder.ci <- N08_JS.rtshoulder.ci[-c(23:44), ]
#join proportion and ci
N08.JS.rtshoulder <- bind_cols(N08_JS.rtshoulder.c, N08_JS.rtshoulder.ci)
#save
#write.csv(N08.JS.rtshoulder, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N08.rtshoulder.csv")

#2. Left Shoulder
N08_JS.ltshoulder <- svyby(formula = ~JMTHP2,
                           by = ~AGE_P + SEX,
                           design = NHIS08_DO,
                           FUN = svymean,
                           na.rm = TRUE)
N08_JS.ltshoulder.c <- N08_JS.ltshoulder %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltshoulder")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N08_JS.ltshoulder.ci <- confint(N08_JS.ltshoulder) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N08_JS.ltshoulder.ci <- N08_JS.ltshoulder.ci[-c(23:44), ]
#join proportion and ci
N08.JS.ltshoulder <- bind_cols(N08_JS.ltshoulder.c, N08_JS.ltshoulder.ci)
#save
#write.csv(N08.JS.ltshoulder, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N08.ltshoulder.csv")


#3. Right elbow
N08_JS.rtelbow <- svyby(formula = ~JMTHP3,
                        by = ~AGE_P + SEX,
                        design = NHIS08_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N08_JS.rtelbow.c <- N08_JS.rtelbow %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtelbow")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N08_JS.rtelbow.ci <- confint(N08_JS.rtelbow) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N08_JS.rtelbow.ci <- N08_JS.rtelbow.ci[-c(23:44), ]
#join proportion and ci
N08.JS.rtelbow <- bind_cols(N08_JS.rtelbow.c, N08_JS.rtelbow.ci)
#save
#write.csv(N08.JS.rtelbow, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N08.rtelbow.csv")

#4. Lt elbow
N08_JS.ltelbow <- svyby(formula = ~JMTHP4,
                        by = ~AGE_P + SEX,
                        design = NHIS08_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N08_JS.ltelbow.c <- N08_JS.ltelbow %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltelbow")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N08_JS.ltelbow.ci <- confint(N08_JS.ltelbow) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N08_JS.ltelbow.ci <- N08_JS.ltelbow.ci[-c(23:44), ]
#join proportion and ci
N08.JS.ltelbow <- bind_cols(N08_JS.ltelbow.c, N08_JS.ltelbow.ci)
#save
#write.csv(N08.JS.ltelbow, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N08.ltelbow.csv")

#5. Right hip
N08_JS.rthip <- svyby(formula = ~JMTHP5,
                      by = ~AGE_P + SEX,
                      design = NHIS08_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N08_JS.rthip.c <- N08_JS.rthip %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rthip")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N08_JS.rthip.ci <- confint(N08_JS.rthip) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N08_JS.rthip.ci <- N08_JS.rthip.ci[-c(23:44), ]
#join proportion and ci
N08.JS.rthip <- bind_cols(N08_JS.rthip.c, N08_JS.rthip.ci)
#save
#write.csv(N08.JS.rthip, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N08.rthip.csv")

#6. Left hip
N08_JS.lthip <- svyby(formula = ~JMTHP6,
                      by = ~AGE_P + SEX,
                      design = NHIS08_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N08_JS.lthip.c <- N08_JS.lthip %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.lthip")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N08_JS.lthip.ci <- confint(N08_JS.lthip) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N08_JS.lthip.ci <- N08_JS.lthip.ci[-c(23:44), ]
#join proportion and ci
N08.JS.lthip <- bind_cols(N08_JS.lthip.c, N08_JS.lthip.ci)
#save
#write.csv(N08.JS.lthip, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N08.lthip.csv")

#7. Right wrist
N08_JS.rtwrist <- svyby(formula = ~JMTHP7,
                        by = ~AGE_P + SEX,
                        design = NHIS08_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N08_JS.rtwrist.c <- N08_JS.rtwrist %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtwrist")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N08_JS.rtwrist.ci <- confint(N08_JS.rtwrist) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N08_JS.rtwrist.ci <- N08_JS.rtwrist.ci[-c(23:44), ]
#join proportion and ci
N08.JS.rtwrist <- bind_cols(N08_JS.rtwrist.c, N08_JS.rtwrist.ci)
#save
#write.csv(N08.JS.rtwrist, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N08.rtwrist.csv")

#8. Left wrist
N08_JS.ltwrist <- svyby(formula = ~JMTHP8,
                        by = ~AGE_P + SEX,
                        design = NHIS08_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N08_JS.ltwrist.c <- N08_JS.ltwrist %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltwrist")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N08_JS.ltwrist.ci <- confint(N08_JS.ltwrist) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N08_JS.ltwrist.ci <- N08_JS.ltwrist.ci[-c(23:44), ]
#join proportion and ci
N08.JS.ltwrist <- bind_cols(N08_JS.ltwrist.c, N08_JS.ltwrist.ci)
#save
#write.csv(N08.JS.ltwrist, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N08.ltwrist.csv")

#9. Right knee
N08_JS.rtknee <- svyby(formula = ~JMTHP9,
                       by = ~AGE_P + SEX,
                       design = NHIS08_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N08_JS.rtknee.c <- N08_JS.rtknee %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtknee")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N08_JS.rtknee.ci <- confint(N08_JS.rtknee) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N08_JS.rtknee.ci <- N08_JS.rtknee.ci[-c(23:44), ]
#join proportion and ci
N08.JS.rtknee <- bind_cols(N08_JS.rtknee.c, N08_JS.rtknee.ci)
#save
#write.csv(N08.JS.rtknee, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N08.rtknee.csv")

#10. Left knee
N08_JS.ltknee <- svyby(formula = ~JMTHP10,
                       by = ~AGE_P + SEX,
                       design = NHIS08_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N08_JS.ltknee.c <- N08_JS.ltknee %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltknee")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N08_JS.ltknee.ci <- confint(N08_JS.ltknee) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N08_JS.ltknee.ci <- N08_JS.ltknee.ci[-c(23:44), ]
#join proportion and ci
N08.JS.ltknee <- bind_cols(N08_JS.ltknee.c, N08_JS.ltknee.ci)
#save
#write.csv(N08.JS.ltknee, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N08.ltknee.csv")

#11. Right ankle
N08_JS.rtankle <- svyby(formula = ~JMTHP11,
                        by = ~AGE_P + SEX,
                        design = NHIS08_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N08_JS.rtankle.c <- N08_JS.rtankle %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtankle")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N08_JS.rtankle.ci <- confint(N08_JS.rtankle) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N08_JS.rtankle.ci <- N08_JS.rtankle.ci[-c(23:44), ]
#join proportion and ci
N08.JS.rtankle <- bind_cols(N08_JS.rtankle.c, N08_JS.rtankle.ci)
#save
#write.csv(N08.JS.rtankle, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N08.rtankle.csv")

#12. Left ankle
N08_JS.ltankle <- svyby(formula = ~JMTHP12,
                        by = ~AGE_P + SEX,
                        design = NHIS08_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N08_JS.ltankle.c <- N08_JS.ltankle %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltankle")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N08_JS.ltankle.ci <- confint(N08_JS.ltankle) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N08_JS.ltankle.ci <- N08_JS.ltankle.ci[-c(23:44), ]
#join proportion and ci
N08.JS.ltankle <- bind_cols(N08_JS.ltankle.c, N08_JS.ltankle.ci)
#save
#write.csv(N08.JS.ltankle, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N08.ltankle.csv")

#13. Right toes
N08_JS.rttoes <- svyby(formula = ~JMTHP13,
                       by = ~AGE_P + SEX,
                       design = NHIS08_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N08_JS.rttoes.c <- N08_JS.rttoes %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rttoes")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N08_JS.rttoes.ci <- confint(N08_JS.rttoes) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N08_JS.rttoes.ci <- N08_JS.rttoes.ci[-c(23:44), ]
#join proportion and ci
N08.JS.rttoes <- bind_cols(N08_JS.rttoes.c, N08_JS.rttoes.ci)
#save
#write.csv(N08.JS.rttoes, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N08.rttoes.csv")

#14. Left toes
N08_JS.lttoes <- svyby(formula = ~JMTHP14,
                       by = ~AGE_P + SEX,
                       design = NHIS08_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N08_JS.lttoes.c <- N08_JS.lttoes %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.lttoes")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N08_JS.lttoes.ci <- confint(N08_JS.lttoes) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N08_JS.lttoes.ci <- N08_JS.lttoes.ci[-c(23:44), ]
#join proportion and ci
N08.JS.lttoes <- bind_cols(N08_JS.lttoes.c, N08_JS.lttoes.ci)
#save
#write.csv(N08.JS.lttoes, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N08.lttoes.csv")

#15. Right fingers/thumb
N08_JS.rtfingthumb <- svyby(formula = ~JMTHP15,
                            by = ~AGE_P + SEX,
                            design = NHIS08_DO,
                            FUN = svymean,
                            na.rm = TRUE)
N08_JS.rtfingthumb.c <- N08_JS.rtfingthumb %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtfingthumb")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N08_JS.rtfingthumb.ci <- confint(N08_JS.rtfingthumb) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N08_JS.rtfingthumb.ci <- N08_JS.rtfingthumb.ci[-c(23:44), ]
#join proportion and ci
N08.JS.rtfingthumb <- bind_cols(N08_JS.rtfingthumb.c, N08_JS.rtfingthumb.ci)
#save
#write.csv(N08.JS.rtfingthumb, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N08.rtfingthumb.csv")

#16. Left fingers/thumb
N08_JS.ltfingthumb <- svyby(formula = ~JMTHP16,
                            by = ~AGE_P + SEX,
                            design = NHIS08_DO,
                            FUN = svymean,
                            na.rm = TRUE)
N08_JS.ltfingthumb.c <- N08_JS.ltfingthumb %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltfingthumb")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N08_JS.ltfingthumb.ci <- confint(N08_JS.ltfingthumb) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N08_JS.ltfingthumb.ci <- N08_JS.ltfingthumb.ci[-c(23:44), ]
#join proportion and ci
N08.JS.ltfingthumb <- bind_cols(N08_JS.ltfingthumb.c, N08_JS.ltfingthumb.ci)
#save
#write.csv(N08.JS.ltfingthumb, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N08.ltfingthumb.csv")

#17. Another
N08_JS.another <- svyby(formula = ~JMTHP17,
                        by = ~AGE_P + SEX,
                        design = NHIS08_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N08_JS.another.c <- N08_JS.another %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.another")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N08_JS.another.ci <- confint(N08_JS.another) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N08_JS.another.ci <- N08_JS.another.ci[-c(23:44), ]
#join proportion and ci
N08.JS.another <- bind_cols(N08_JS.another.c, N08_JS.another.ci)
#save
#write.csv(N08.JS.another, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N08.another.csv")

N08.JSloc <- Reduce(function(x, y) merge(x, y, all = TRUE), list(N08_JS.rtshoulder.c, N08_JS.ltshoulder.c, N08_JS.rtelbow.c, N08_JS.ltelbow.c, N08_JS.rthip.c, N08_JS.lthip.c, N08_JS.rtwrist.c, N08_JS.ltwrist.c, N08_JS.rtknee.c, N08_JS.ltknee.c, N08_JS.rtankle.c, N08_JS.ltankle.c, N08_JS.rttoes.c, N08_JS.lttoes.c, N08_JS.rtfingthumb.c, N08_JS.ltfingthumb.c, N08_JS.another.c))

#Wide to long
N08.JSlocation <- gather(N08.JSloc, Location, Proportion, Proportion.RtShoulder:Proportion.another, factor_key = TRUE)
N08.JSlocation <- N08.JSlocation %>% 
  unite("Demographic", Age:Sex)
N08.JSlocation$Location <- as.character(N08.JSlocation$Location)
N08.JSlocation$Location[N08.JSlocation$Location == "Proportion.RtShoulder"] <- "Right shoulder"
N08.JSlocation$Location[N08.JSlocation$Location == "Proportion.ltshoulder"] <- "Left shoulder"
N08.JSlocation$Location[N08.JSlocation$Location == "Proportion.rtelbow"] <- "Right elbow"
N08.JSlocation$Location[N08.JSlocation$Location == "Proportion.ltelbow"] <- "Left elbow"
N08.JSlocation$Location[N08.JSlocation$Location == "Proportion.rthip"] <- "Right hip"
N08.JSlocation$Location[N08.JSlocation$Location == "Proportion.lthip"] <- "Left hip"
N08.JSlocation$Location[N08.JSlocation$Location == "Proportion.rtwrist"] <- "Right wrist"
N08.JSlocation$Location[N08.JSlocation$Location == "Proportion.ltwrist"] <- "Left wrist"
N08.JSlocation$Location[N08.JSlocation$Location == "Proportion.rtknee"] <- "Right knee"
N08.JSlocation$Location[N08.JSlocation$Location == "Proportion.ltknee"] <- "Left knee"
N08.JSlocation$Location[N08.JSlocation$Location == "Proportion.rtankle"] <- "Right ankle"
N08.JSlocation$Location[N08.JSlocation$Location == "Proportion.ltankle"] <- "Left ankle"
N08.JSlocation$Location[N08.JSlocation$Location == "Proportion.rttoes"] <- "Right toes"
N08.JSlocation$Location[N08.JSlocation$Location == "Proportion.lttoes"] <- "Left toes"
N08.JSlocation$Location[N08.JSlocation$Location == "Proportion.rtfingthumb"] <- "Right fingers or thumb"
N08.JSlocation$Location[N08.JSlocation$Location == "Proportion.ltfingthumb"] <- "Left fingers or thumb"
N08.JSlocation$Location[N08.JSlocation$Location == "Proportion.another"] <- "Another joint"

#Heatmap of joint locations by demographic groups
N08.JSlocmap <- ggplot(N08.JSlocation,
                       aes(x = Demographic, y = Location, fill = Proportion)) +
  geom_tile() + 
  scale_fill_viridis(option = "viridis", direction = -1, limits = c(0, 60), breaks = c(10, 20, 30, 40, 50, 60), discrete = FALSE) +
  theme_bw() + 
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.text.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  xlab("Age-sex profile") +
  theme(legend.position = "right",
        strip.background = element_blank()) +
  theme(legend.title = element_text(size = 26)) +
  theme(legend.text = element_text(size = 22)) +
  labs(fill = "Prevalence (%)") +
  guides(fill = guide_colourbar(barwidth = 3, barheight = 25)) +
  scale_x_discrete(labels = c("Females, 18 to 24", "Males, 18 to 24", "Females, 25 to 29", "Males, 25 to 29", "Females, 30 to 34", "Males, 30 to 34", "Females, 35 to 39", "Males, 35 to 39", "Females, 40 to 44", "Males, 40 to 44", "Females, 45 to 49", "Males, 45 to 49", "Females, 50 to 54", "Males, 50 to 54", "Females, 55 to 59", "Males, 55 to 59", "Females, 60 to 64", "Males, 60 to 64", "Females, 65 to 69", "Males, 65 to 69", "Females, 70 and above", "Males, 70 and above")) +
  scale_y_discrete(limits = c("Another joint", "Right toes", "Left toes", "Right ankle", "Left ankle", "Right knee", "Left knee", "Right hip", "Left hip", "Right shoulder", "Left shoulder", "Right elbow", "Left elbow", "Right wrist", "Left wrist", "Right fingers or thumb", "Left fingers or thumb"))
ggsave("N08.age-sex.png", width = 20, height = 13)



#III) Socioeconomic trends (employment status only)

#i)   Prevalence by employment status
N08_JS_emp <- svyby(formula = ~JNTSYMP,
                    by = ~DOINGLWA,
                    design = NHIS08_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N08_JS_emp.c <- N08_JS_emp %>%
  select(1, 3, 5) %>%
  setNames(c("Employment", "Proportion", "SE")) %>%
  mutate(Employment = as.factor(Employment)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N08_JS_emp_ci <- confint(N08_JS_emp) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for js = 0
N08_JS_emp_ci <- N08_JS_emp_ci[-c(1:2), ]
#join ci and proportion
N08.JS.emp <- bind_cols(N08_JS_emp.c, N08_JS_emp_ci) #final proportion, se & 95% ci
#save
#write.csv(N08.JS.emp, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N08.JS.emp.csv")

#ii)  Logistic regression by employment status
N08_JS.emp_glm <- svyglm(JNTSYMP~relevel(DOINGLWA, ref = "Working for pay at a job/business") + AGE_P + SEX,
                         family = quasibinomial,
                         design = NHIS08_DO)
summary(N08_JS.emp_glm)
exp(cbind(OR=coef(N08_JS.emp_glm), confint(N08_JS.emp_glm)))

#4. Lifestyle trends

#I)  BMI
#logistic regression
N08.BMI.glm <- svyglm(JNTSYMP ~ BMI + AGE_P + SEX,
                      family = quasibinomial,
                      design = NHIS08_DO)
summary(N08.BMI.glm)
exp(cbind(OR=coef(N08.BMI.glm), confint(N08.BMI.glm)))

#II) Physical activity
#logistic regression
N08.PA.JS.glm <- svyglm(JNTSYMP ~ relevel(factor(PAC, ordered = FALSE), ref = "Physically active") + AGE_P + SEX,
                        family = quasibinomial,
                        design = NHIS08_DO)
summary(N08.PA.JS.glm)
exp(cbind(OR = coef(N08.PA.JS.glm), confint(N08.PA.JS.glm)))

#_____________________________________________________________________________________________________________________

# == 2007 == #

#1. Overall prevalence
N07_JS <- svymean(~factor(JNTSYMP), 
                  NHIS07_DO, 
                  na.rm = TRUE)

N07_JS.c <- N07_JS %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N07_JS_ci <- confint(N07_JS) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
N07.JS <- bind_cols(N07_JS.c, N07_JS_ci)
#remove js = 0
N07.JS <- N07.JS[-c(1), ] #final proportion, se & 95%
#save
#write.csv(N07.JS, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N07.JS.csv")

#2. Spatial analysis, by region

#I)   Overall population

#i)   regional prevalence for the overall population
N07_JS_reg <- svyby(formula = ~JNTSYMP,
                    by = ~REGION,
                    design = NHIS07_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N07_JS_reg.c <- N07_JS_reg %>%
  select(1, 3, 5) %>%
  setNames(c("Region", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N07_JS_reg.ci <- confint(N07_JS_reg) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for js = 0
N07_JS_reg.ci <- N07_JS_reg.ci[-c(1:4), ]
#join proportion and ci
N07.JS.region <- bind_cols(N07_JS_reg.c, N07_JS_reg.ci) #final proportion, se & 95% ci
#save
#write.csv(N07.JS.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N07.JS.region.csv")

#Create overall regional prevalence

#Change the region column to "NAME" to match sf
names(N07.JS.region)[names(N07.JS.region) == "Region"] <- "NAME"

N07.JS.joined <- regions %>%
  left_join(N07.JS.region)

N07.JS.joined$NAME <- as.factor(N07.JS.joined$NAME)

#Plot
JSregplot <- ggplot(data = N07.JS.joined) +
  geom_sf(aes(fill = Proportion), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  scale_fill_viridis_c(name = "Prevalence (%)", option = "viridis", direction = -1, limits = c(25, 40), breaks = c(25, 30, 35, 40)) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  ggtitle("Regional prevalence (%) of US residents with joint symptoms in 2007") +
  theme(plot.title = element_text(size = 10, hjust = 0.5))
ggsave("N07.JSregchor.png", width = 5, height = 5)


#ii)  logistic regression by region
N07.JS.reg.glm <- svyglm(relevel(JNTSYMP, ref = "0")~ REGION + AGE_P,
                         family = quasibinomial,
                         design = NHIS07_DO)
summary(N07.JS.reg.glm)
exp(cbind(OR=coef(N07.JS.reg.glm), confint(N07.JS.reg.glm)))

#II)  prevalence by region, by age

#i)  age-specific regional prevalance
N07_JS_reg.age <- svyby(formula = ~JNTSYMP,
                        by = ~REGION + AGE_P,
                        design = NHIS07_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N07_JS_reg.age.c <- N07_JS_reg.age %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Age", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Age = as.character(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N07_JS_reg.age.ci <- confint(N07_JS_reg.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for js = 0
N07_JS_reg.age.ci <- N07_JS_reg.age.ci[-c(1:44), ]
#join proportion and ci
N07.JS.age.region <- bind_cols(N07_JS_reg.age.c, N07_JS_reg.age.ci) #final proportion, se & 95% ci
#save
#write.csv(N07.JS.age.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N07.JS.region.age.csv")

#Geofacet plot - Age & Region
JS.Age_Reg <- ggplot(N07.JS.age.region, aes(Age, Proportion)) +
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
  ggtitle("Regional prevalence (%) of US residents with joint symptoms by age group in 2007") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N07.GF_AgeReg.JS.png", width = 14, height = 9)


#ii) age-specific regional logistic regressions
#18 to 24
sub1824 <- subset(NHIS07_DO, AGE_P == "18 to 24")
N07.JS.age1824.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub1824)
summary(N07.JS.age1824.reg)
exp(cbind(OR = coef(N07.JS.age1824.reg), confint(N07.JS.age1824.reg)))

#25 to 29
sub2529 <- subset(NHIS07_DO, AGE_P == "25 to 29")
N07.JS.age2529.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub2529)
summary(N07.JS.age2529.reg)
exp(cbind(OR = coef(N07.JS.age2529.reg), confint(N07.JS.age2529.reg)))

#30 to 34
sub3034 <- subset(NHIS07_DO, AGE_P == "30 to 34")
N07.JS.age3034.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub3034)
summary(N07.JS.age3034.reg)
exp(cbind(OR = coef(N07.JS.age3034.reg), confint(N07.JS.age3034.reg)))

#35 to 39
sub3539 <- subset(NHIS07_DO, AGE_P == "35 to 39")
N07.JS.age3539.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub3539)
summary(N07.JS.age3539.reg)
exp(cbind(OR = coef(N07.JS.age3539.reg), confint(N07.JS.age3539.reg)))

#40 to 44
sub4044 <- subset(NHIS07_DO, AGE_P == "40 to 44")
N07.JS.age4044.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub4044)
summary(N07.JS.age4044.reg)
exp(cbind(OR = coef(N07.JS.age4044.reg), confint(N07.JS.age4044.reg)))

#45 to 49
sub4549 <- subset(NHIS07_DO, AGE_P == "45 to 49")
N07.JS.age4549.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub4549)
summary(N07.JS.age4549.reg)
exp(cbind(OR = coef(N07.JS.age4549.reg), confint(N07.JS.age4549.reg)))

#50 to 54
sub5054 <- subset(NHIS07_DO, AGE_P =="50 to 54")
N07.JS.age5054.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub5054)
summary(N07.JS.age5054.reg)
exp(cbind(OR = coef(N07.JS.age5054.reg), confint(N07.JS.age5054.reg)))

#55 to 59
sub5559 <- subset(NHIS07_DO, AGE_P == "55 to 59")
N07.JS.age5559.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub5559)
summary(N07.JS.age5559.reg)
exp(cbind(OR = coef(N07.JS.age5559.reg), confint(N07.JS.age5559.reg)))

#60 to 64
sub6064 <- subset(NHIS07_DO, AGE_P == "60 to 64")
N07.JS.age6064.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub6064)
summary(N07.JS.age6064.reg)
exp(cbind(OR = coef(N07.JS.age6064.reg), confint(N07.JS.age6064.reg)))

#65 to 69
sub6569 <- subset(NHIS07_DO, AGE_P == "65 to 69")
N07.JS.age6569.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub6569)
summary(N07.JS.age6569.reg)
exp(cbind(OR = coef(N07.JS.age6569.reg), confint(N07.JS.age6569.reg)))

#70 and above
sub70 <- subset(NHIS07_DO, AGE_P == "70 and above")
N07.JS.age70.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                           family = quasibinomial,
                           design = sub70)
summary(N07.JS.age70.reg)
exp(cbind(OR = coef(N07.JS.age70.reg), confint(N07.JS.age70.reg)))

#III) prevalence by region, by sex

#i)  sex-specific regional prevalence
N07_JS_reg.sex <- svyby(formula = ~JNTSYMP,
                        by = ~REGION + SEX,
                        design = NHIS07_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N07_JS_reg.sex.c <- N07_JS_reg.sex %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Sex", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N07_JS_reg.sex.ci <- confint(N07_JS_reg.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for js = 0
N07_JS_reg.sex.ci <- N07_JS_reg.sex.ci[-c(1:8), ]
#join proportion and ci
N07.JS.sex.region <- bind_cols(N07_JS_reg.sex.c, N07_JS_reg.sex.ci) #final proprtion, se & 95% ci
#save
#write.csv(N07.JS.sex.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N07.JS.region.sex.csv")

#Geofacet - Sex & Region
JS.Sex_Reg07 <- ggplot(N07.JS.sex.region, aes(Sex, Proportion)) +
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
  ggtitle("Regional prevalence (%) of US residents with joint symptoms by sex in 2007") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N07.GF_SexReg.JS.png", width = 14, height = 9)


#ii) sex-specific regional logistic regressions
#males
submales <- subset(NHIS07_DO, SEX == "Male")
N07.JS.sexmale.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + AGE_P,
                             family = quasibinomial,
                             design = submales)
summary(N07.JS.sexmale.reg)
exp(cbind(OR = coef(N07.JS.sexmale.reg), confint(N07.JS.sexmale.reg)))

#females
subfemales <- subset(NHIS07_DO, SEX == "Female")
N07.JS.sexfemale.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + AGE_P,
                               family = quasibinomial,
                               design = subfemales)
summary(N07.JS.sexfemale.reg)
exp(cbind(OR = coef(N07.JS.sexfemale.reg), confint(N07.JS.sexfemale.reg)))


#3. Demographic trends

#I)  Age trends

#i)  Prevalence by age
N07_JS_age <- svyby(formula = ~JNTSYMP,
                    by = ~AGE_P,
                    design = NHIS07_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N07_JS_age.c <- N07_JS_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N07_JS_age_ci <- confint(N07_JS_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove js=0 ci
N07_JS_age_ci <- N07_JS_age_ci[-c(1:11), ]
#join proportion and ci
N07_JS.age <- bind_cols(N07_JS_age.c, N07_JS_age_ci) #final proportion, se & 95% ci
#Save
#write.csv(N07_JS.age, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N07.JS.age.csv")


#ii) Logistic regression by age
N07_JS.age_glm <- svyglm(JNTSYMP~AGE_P + SEX,
                         family = quasibinomial,
                         design = NHIS07_DO)
summary(N07_JS.age_glm)
exp(cbind(OR=coef(N07_JS.age_glm), confint(N07_JS.age_glm)))



#II)  Sex trends

#i)  Prevalence by sex
N07_JS_sex <- svyby(formula = ~JNTSYMP,
                    by = ~SEX,
                    design = NHIS07_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N07_JS_sex.c <- N07_JS_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N07_JS_sex_ci <- confint(N07_JS_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N07_JS_sex_ci <- N07_JS_sex_ci[-c(1:2), ]
#join proportion and ci
N07.JS.sex <- bind_cols(N07_JS_sex.c, N07_JS_sex_ci)
#save
#write.csv(N07.JS.sex, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N07.JS.sex.csv")


#ii) Logistic regression by sex
N07.JS.sex.glm.c <- svyglm(JNTSYMP~relevel(SEX, ref = "Male") + AGE_P,
                           family = quasibinomial,
                           design = NHIS07_DO)
summary(N07.JS.sex.glm.c)
exp(cbind(OR=coef(N07.JS.sex.glm.c), confint(N07.JS.sex.glm.c)))

#Prevalence of various age-sex groups and their joint pain locations

#1. Right shoulder
N07_JS.rtshoulder <- svyby(formula = ~JMTHP1,
                           by = ~AGE_P + SEX,
                           design = NHIS07_DO,
                           FUN = svymean,
                           na.rm = TRUE)
N07_JS.rtshoulder.c <- N07_JS.rtshoulder %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.RtShoulder")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N07_JS.rtshoulder.ci <- confint(N07_JS.rtshoulder) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N07_JS.rtshoulder.ci <- N07_JS.rtshoulder.ci[-c(23:44), ]
#join proportion and ci
N07.JS.rtshoulder <- bind_cols(N07_JS.rtshoulder.c, N07_JS.rtshoulder.ci)
#save
#write.csv(N07.JS.rtshoulder, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N07.rtshoulder.csv")

#2. Left Shoulder
N07_JS.ltshoulder <- svyby(formula = ~JMTHP2,
                           by = ~AGE_P + SEX,
                           design = NHIS07_DO,
                           FUN = svymean,
                           na.rm = TRUE)
N07_JS.ltshoulder.c <- N07_JS.ltshoulder %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltshoulder")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N07_JS.ltshoulder.ci <- confint(N07_JS.ltshoulder) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N07_JS.ltshoulder.ci <- N07_JS.ltshoulder.ci[-c(23:44), ]
#join proportion and ci
N07.JS.ltshoulder <- bind_cols(N07_JS.ltshoulder.c, N07_JS.ltshoulder.ci)
#save
#write.csv(N07.JS.ltshoulder, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N07.ltshoulder.csv")


#3. Right elbow
N07_JS.rtelbow <- svyby(formula = ~JMTHP3,
                        by = ~AGE_P + SEX,
                        design = NHIS07_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N07_JS.rtelbow.c <- N07_JS.rtelbow %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtelbow")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N07_JS.rtelbow.ci <- confint(N07_JS.rtelbow) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N07_JS.rtelbow.ci <- N07_JS.rtelbow.ci[-c(23:44), ]
#join proportion and ci
N07.JS.rtelbow <- bind_cols(N07_JS.rtelbow.c, N07_JS.rtelbow.ci)
#save
#write.csv(N07.JS.rtelbow, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N07.rtelbow.csv")

#4. Lt elbow
N07_JS.ltelbow <- svyby(formula = ~JMTHP4,
                        by = ~AGE_P + SEX,
                        design = NHIS07_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N07_JS.ltelbow.c <- N07_JS.ltelbow %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltelbow")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N07_JS.ltelbow.ci <- confint(N07_JS.ltelbow) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N07_JS.ltelbow.ci <- N07_JS.ltelbow.ci[-c(23:44), ]
#join proportion and ci
N07.JS.ltelbow <- bind_cols(N07_JS.ltelbow.c, N07_JS.ltelbow.ci)
#save
#write.csv(N07.JS.ltelbow, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N07.ltelbow.csv")

#5. Right hip
N07_JS.rthip <- svyby(formula = ~JMTHP5,
                      by = ~AGE_P + SEX,
                      design = NHIS07_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N07_JS.rthip.c <- N07_JS.rthip %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rthip")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N07_JS.rthip.ci <- confint(N07_JS.rthip) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N07_JS.rthip.ci <- N07_JS.rthip.ci[-c(23:44), ]
#join proportion and ci
N07.JS.rthip <- bind_cols(N07_JS.rthip.c, N07_JS.rthip.ci)
#save
#write.csv(N07.JS.rthip, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N07.rthip.csv")

#6. Left hip
N07_JS.lthip <- svyby(formula = ~JMTHP6,
                      by = ~AGE_P + SEX,
                      design = NHIS07_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N07_JS.lthip.c <- N07_JS.lthip %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.lthip")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N07_JS.lthip.ci <- confint(N07_JS.lthip) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N07_JS.lthip.ci <- N07_JS.lthip.ci[-c(23:44), ]
#join proportion and ci
N07.JS.lthip <- bind_cols(N07_JS.lthip.c, N07_JS.lthip.ci)
#save
#write.csv(N07.JS.lthip, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N07.lthip.csv")

#7. Right wrist
N07_JS.rtwrist <- svyby(formula = ~JMTHP7,
                        by = ~AGE_P + SEX,
                        design = NHIS07_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N07_JS.rtwrist.c <- N07_JS.rtwrist %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtwrist")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N07_JS.rtwrist.ci <- confint(N07_JS.rtwrist) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N07_JS.rtwrist.ci <- N07_JS.rtwrist.ci[-c(23:44), ]
#join proportion and ci
N07.JS.rtwrist <- bind_cols(N07_JS.rtwrist.c, N07_JS.rtwrist.ci)
#save
#write.csv(N07.JS.rtwrist, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N07.rtwrist.csv")

#8. Left wrist
N07_JS.ltwrist <- svyby(formula = ~JMTHP8,
                        by = ~AGE_P + SEX,
                        design = NHIS07_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N07_JS.ltwrist.c <- N07_JS.ltwrist %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltwrist")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N07_JS.ltwrist.ci <- confint(N07_JS.ltwrist) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N07_JS.ltwrist.ci <- N07_JS.ltwrist.ci[-c(23:44), ]
#join proportion and ci
N07.JS.ltwrist <- bind_cols(N07_JS.ltwrist.c, N07_JS.ltwrist.ci)
#save
#write.csv(N07.JS.ltwrist, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N07.ltwrist.csv")

#9. Right knee
N07_JS.rtknee <- svyby(formula = ~JMTHP9,
                       by = ~AGE_P + SEX,
                       design = NHIS07_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N07_JS.rtknee.c <- N07_JS.rtknee %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtknee")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N07_JS.rtknee.ci <- confint(N07_JS.rtknee) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N07_JS.rtknee.ci <- N07_JS.rtknee.ci[-c(23:44), ]
#join proportion and ci
N07.JS.rtknee <- bind_cols(N07_JS.rtknee.c, N07_JS.rtknee.ci)
#save
#write.csv(N07.JS.rtknee, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N07.rtknee.csv")

#10. Left knee
N07_JS.ltknee <- svyby(formula = ~JMTHP10,
                       by = ~AGE_P + SEX,
                       design = NHIS07_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N07_JS.ltknee.c <- N07_JS.ltknee %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltknee")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N07_JS.ltknee.ci <- confint(N07_JS.ltknee) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N07_JS.ltknee.ci <- N07_JS.ltknee.ci[-c(23:44), ]
#join proportion and ci
N07.JS.ltknee <- bind_cols(N07_JS.ltknee.c, N07_JS.ltknee.ci)
#save
#write.csv(N07.JS.ltknee, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N07.ltknee.csv")

#11. Right ankle
N07_JS.rtankle <- svyby(formula = ~JMTHP11,
                        by = ~AGE_P + SEX,
                        design = NHIS07_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N07_JS.rtankle.c <- N07_JS.rtankle %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtankle")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N07_JS.rtankle.ci <- confint(N07_JS.rtankle) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N07_JS.rtankle.ci <- N07_JS.rtankle.ci[-c(23:44), ]
#join proportion and ci
N07.JS.rtankle <- bind_cols(N07_JS.rtankle.c, N07_JS.rtankle.ci)
#save
#write.csv(N07.JS.rtankle, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N07.rtankle.csv")

#12. Left ankle
N07_JS.ltankle <- svyby(formula = ~JMTHP12,
                        by = ~AGE_P + SEX,
                        design = NHIS07_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N07_JS.ltankle.c <- N07_JS.ltankle %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltankle")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N07_JS.ltankle.ci <- confint(N07_JS.ltankle) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N07_JS.ltankle.ci <- N07_JS.ltankle.ci[-c(23:44), ]
#join proportion and ci
N07.JS.ltankle <- bind_cols(N07_JS.ltankle.c, N07_JS.ltankle.ci)
#save
#write.csv(N07.JS.ltankle, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N07.ltankle.csv")

#13. Right toes
N07_JS.rttoes <- svyby(formula = ~JMTHP13,
                       by = ~AGE_P + SEX,
                       design = NHIS07_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N07_JS.rttoes.c <- N07_JS.rttoes %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rttoes")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N07_JS.rttoes.ci <- confint(N07_JS.rttoes) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N07_JS.rttoes.ci <- N07_JS.rttoes.ci[-c(23:44), ]
#join proportion and ci
N07.JS.rttoes <- bind_cols(N07_JS.rttoes.c, N07_JS.rttoes.ci)
#save
#write.csv(N07.JS.rttoes, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N07.rttoes.csv")

#14. Left toes
N07_JS.lttoes <- svyby(formula = ~JMTHP14,
                       by = ~AGE_P + SEX,
                       design = NHIS07_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N07_JS.lttoes.c <- N07_JS.lttoes %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.lttoes")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N07_JS.lttoes.ci <- confint(N07_JS.lttoes) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N07_JS.lttoes.ci <- N07_JS.lttoes.ci[-c(23:44), ]
#join proportion and ci
N07.JS.lttoes <- bind_cols(N07_JS.lttoes.c, N07_JS.lttoes.ci)
#save
#write.csv(N07.JS.lttoes, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N07.lttoes.csv")

#15. Right fingers/thumb
N07_JS.rtfingthumb <- svyby(formula = ~JMTHP15,
                            by = ~AGE_P + SEX,
                            design = NHIS07_DO,
                            FUN = svymean,
                            na.rm = TRUE)
N07_JS.rtfingthumb.c <- N07_JS.rtfingthumb %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtfingthumb")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N07_JS.rtfingthumb.ci <- confint(N07_JS.rtfingthumb) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N07_JS.rtfingthumb.ci <- N07_JS.rtfingthumb.ci[-c(23:44), ]
#join proportion and ci
N07.JS.rtfingthumb <- bind_cols(N07_JS.rtfingthumb.c, N07_JS.rtfingthumb.ci)
#save
#write.csv(N07.JS.rtfingthumb, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N07.rtfingthumb.csv")

#16. Left fingers/thumb
N07_JS.ltfingthumb <- svyby(formula = ~JMTHP16,
                            by = ~AGE_P + SEX,
                            design = NHIS07_DO,
                            FUN = svymean,
                            na.rm = TRUE)
N07_JS.ltfingthumb.c <- N07_JS.ltfingthumb %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltfingthumb")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N07_JS.ltfingthumb.ci <- confint(N07_JS.ltfingthumb) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N07_JS.ltfingthumb.ci <- N07_JS.ltfingthumb.ci[-c(23:44), ]
#join proportion and ci
N07.JS.ltfingthumb <- bind_cols(N07_JS.ltfingthumb.c, N07_JS.ltfingthumb.ci)
#save
#write.csv(N07.JS.ltfingthumb, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N07.ltfingthumb.csv")

#17. Another
N07_JS.another <- svyby(formula = ~JMTHP17,
                        by = ~AGE_P + SEX,
                        design = NHIS07_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N07_JS.another.c <- N07_JS.another %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.another")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N07_JS.another.ci <- confint(N07_JS.another) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N07_JS.another.ci <- N07_JS.another.ci[-c(23:44), ]
#join proportion and ci
N07.JS.another <- bind_cols(N07_JS.another.c, N07_JS.another.ci)
#save
#write.csv(N07.JS.another, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N07.another.csv")

N07.JSloc <- Reduce(function(x, y) merge(x, y, all = TRUE), list(N07_JS.rtshoulder.c, N07_JS.ltshoulder.c, N07_JS.rtelbow.c, N07_JS.ltelbow.c, N07_JS.rthip.c, N07_JS.lthip.c, N07_JS.rtwrist.c, N07_JS.ltwrist.c, N07_JS.rtknee.c, N07_JS.ltknee.c, N07_JS.rtankle.c, N07_JS.ltankle.c, N07_JS.rttoes.c, N07_JS.lttoes.c, N07_JS.rtfingthumb.c, N07_JS.ltfingthumb.c, N07_JS.another.c))

#Wide to long
N07.JSlocation <- gather(N07.JSloc, Location, Proportion, Proportion.RtShoulder:Proportion.another, factor_key = TRUE)
N07.JSlocation <- N07.JSlocation %>% 
  unite("Demographic", Age:Sex)
N07.JSlocation$Location <- as.character(N07.JSlocation$Location)
N07.JSlocation$Location[N07.JSlocation$Location == "Proportion.RtShoulder"] <- "Right shoulder"
N07.JSlocation$Location[N07.JSlocation$Location == "Proportion.ltshoulder"] <- "Left shoulder"
N07.JSlocation$Location[N07.JSlocation$Location == "Proportion.rtelbow"] <- "Right elbow"
N07.JSlocation$Location[N07.JSlocation$Location == "Proportion.ltelbow"] <- "Left elbow"
N07.JSlocation$Location[N07.JSlocation$Location == "Proportion.rthip"] <- "Right hip"
N07.JSlocation$Location[N07.JSlocation$Location == "Proportion.lthip"] <- "Left hip"
N07.JSlocation$Location[N07.JSlocation$Location == "Proportion.rtwrist"] <- "Right wrist"
N07.JSlocation$Location[N07.JSlocation$Location == "Proportion.ltwrist"] <- "Left wrist"
N07.JSlocation$Location[N07.JSlocation$Location == "Proportion.rtknee"] <- "Right knee"
N07.JSlocation$Location[N07.JSlocation$Location == "Proportion.ltknee"] <- "Left knee"
N07.JSlocation$Location[N07.JSlocation$Location == "Proportion.rtankle"] <- "Right ankle"
N07.JSlocation$Location[N07.JSlocation$Location == "Proportion.ltankle"] <- "Left ankle"
N07.JSlocation$Location[N07.JSlocation$Location == "Proportion.rttoes"] <- "Right toes"
N07.JSlocation$Location[N07.JSlocation$Location == "Proportion.lttoes"] <- "Left toes"
N07.JSlocation$Location[N07.JSlocation$Location == "Proportion.rtfingthumb"] <- "Right fingers or thumb"
N07.JSlocation$Location[N07.JSlocation$Location == "Proportion.ltfingthumb"] <- "Left fingers or thumb"
N07.JSlocation$Location[N07.JSlocation$Location == "Proportion.another"] <- "Another joint"

#Heatmap of joint locations by demographic groups
N07.JSlocmap <- ggplot(N07.JSlocation,
                       aes(x = Demographic, y = Location, fill = Proportion)) +
  geom_tile() + 
  scale_fill_viridis(option = "viridis", direction = -1, limits = c(0, 62), breaks = c(10, 20, 30, 40, 50, 60), discrete = FALSE) +
  theme_bw() + 
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.text.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  xlab("Age-sex profile") +
  theme(legend.position = "right",
        strip.background = element_blank()) +
  theme(legend.title = element_text(size = 26)) +
  theme(legend.text = element_text(size = 22)) +
  labs(fill = "Prevalence (%)") +
  guides(fill = guide_colourbar(barwidth = 3, barheight = 25)) +
  scale_x_discrete(labels = c("Females, 18 to 24", "Males, 18 to 24", "Females, 25 to 29", "Males, 25 to 29", "Females, 30 to 34", "Males, 30 to 34", "Females, 35 to 39", "Males, 35 to 39", "Females, 40 to 44", "Males, 40 to 44", "Females, 45 to 49", "Males, 45 to 49", "Females, 50 to 54", "Males, 50 to 54", "Females, 55 to 59", "Males, 55 to 59", "Females, 60 to 64", "Males, 60 to 64", "Females, 65 to 69", "Males, 65 to 69", "Females, 70 and above", "Males, 70 and above")) +
  scale_y_discrete(limits = c("Another joint", "Right toes", "Left toes", "Right ankle", "Left ankle", "Right knee", "Left knee", "Right hip", "Left hip", "Right shoulder", "Left shoulder", "Right elbow", "Left elbow", "Right wrist", "Left wrist", "Right fingers or thumb", "Left fingers or thumb"))
ggsave("N07.age-sex.png", width = 20, height = 13)


#III) Socioeconomic trends (employment status only)

#i)   Prevalence by employment status
N07_JS_emp <- svyby(formula = ~JNTSYMP,
                    by = ~DOINGLWA,
                    design = NHIS07_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N07_JS_emp.c <- N07_JS_emp %>%
  select(1, 3, 5) %>%
  setNames(c("Employment", "Proportion", "SE")) %>%
  mutate(Employment = as.factor(Employment)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N07_JS_emp_ci <- confint(N07_JS_emp) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for js = 0
N07_JS_emp_ci <- N07_JS_emp_ci[-c(1:2), ]
#join ci and proportion
N07.JS.emp <- bind_cols(N07_JS_emp.c, N07_JS_emp_ci) #final proportion, se & 95% ci
#save
#write.csv(N07.JS.emp, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N07.JS.emp.csv")

#ii)  Logistic regression by employment status
N07_JS.emp_glm <- svyglm(JNTSYMP~relevel(DOINGLWA, ref = "Working for pay at a job/business") + AGE_P + SEX,
                         family = quasibinomial,
                         design = NHIS07_DO)
summary(N07_JS.emp_glm)
exp(cbind(OR=coef(N07_JS.emp_glm), confint(N07_JS.emp_glm)))

#4. Lifestyle trends

#I)  BMI
#logistic regression
N07.BMI.glm <- svyglm(JNTSYMP ~ BMI + AGE_P + SEX,
                      family = quasibinomial,
                      design = NHIS07_DO)
summary(N07.BMI.glm)
exp(cbind(OR=coef(N07.BMI.glm), confint(N07.BMI.glm)))

#II) Physical activity
#logistic regression
N07.PA.JS.glm <- svyglm(JNTSYMP ~ relevel(factor(PAC, ordered = FALSE), ref = "Physically active") + AGE_P + SEX,
                        family = quasibinomial,
                        design = NHIS07_DO)
summary(N07.PA.JS.glm)
exp(cbind(OR = coef(N07.PA.JS.glm), confint(N07.PA.JS.glm)))

#____________________________________________________________________________________________________________________

# == 2006 == #

#1. Overall prevalence
N06_JS <- svymean(~factor(JNTSYMP), 
                  NHIS06_DO, 
                  na.rm = TRUE)

N06_JS.c <- N06_JS %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N06_JS_ci <- confint(N06_JS) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
N06.JS <- bind_cols(N06_JS.c, N06_JS_ci)
#remove js = 0
N06.JS <- N06.JS[-c(1), ] #final proportion, se & 95%
#save
#write.csv(N06.JS, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N06.JS.csv")

#2. Spatial analysis, by region

#I)   Overall population

#i)   regional prevalence for the overall population
N06_JS_reg <- svyby(formula = ~JNTSYMP,
                    by = ~REGION,
                    design = NHIS06_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N06_JS_reg.c <- N06_JS_reg %>%
  select(1, 3, 5) %>%
  setNames(c("Region", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N06_JS_reg.ci <- confint(N06_JS_reg) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for js = 0
N06_JS_reg.ci <- N06_JS_reg.ci[-c(1:4), ]
#join proportion and ci
N06.JS.region <- bind_cols(N06_JS_reg.c, N06_JS_reg.ci) #final proportion, se & 95% ci
#save
#write.csv(N06.JS.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N06.JS.region.csv")

#Create overall regional prevalence

#Change the region column to "NAME" to match sf
names(N06.JS.region)[names(N06.JS.region) == "Region"] <- "NAME"

N06.JS.joined <- regions %>%
  left_join(N06.JS.region)

N06.JS.joined$NAME <- as.factor(N06.JS.joined$NAME)

#Plot
JSregplot <- ggplot(data = N06.JS.joined) +
  geom_sf(aes(fill = Proportion), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  scale_fill_viridis_c(name = "Prevalence (%)", option = "viridis", direction = -1, limits = c(25, 40), breaks = c(25, 30, 35, 40)) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  ggtitle("Regional prevalence (%) of US residents with joint symptoms in 2006") +
  theme(plot.title = element_text(size = 10, hjust = 0.5))
ggsave("N06.JSregchor.png", width = 5, height = 5)


#ii)  logistic regression by region
N06.JS.reg.glm <- svyglm(relevel(JNTSYMP, ref = "0")~ REGION + AGE_P,
                         family = quasibinomial,
                         design = NHIS06_DO)
summary(N06.JS.reg.glm)
exp(cbind(OR=coef(N06.JS.reg.glm), confint(N06.JS.reg.glm)))

#II)  prevalence by region, by age

#i)  age-specific regional prevalance
N06_JS_reg.age <- svyby(formula = ~JNTSYMP,
                        by = ~REGION + AGE_P,
                        design = NHIS06_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N06_JS_reg.age.c <- N06_JS_reg.age %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Age", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Age = as.character(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N06_JS_reg.age.ci <- confint(N06_JS_reg.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for js = 0
N06_JS_reg.age.ci <- N06_JS_reg.age.ci[-c(1:44), ]
#join proportion and ci
N06.JS.age.region <- bind_cols(N06_JS_reg.age.c, N06_JS_reg.age.ci) #final proportion, se & 95% ci
#save
#write.csv(N06.JS.age.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N06.JS.region.age.csv")

#Geofacet plot - Age & Region
JS.Age_Reg <- ggplot(N06.JS.age.region, aes(Age, Proportion)) +
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
  ggtitle("Regional prevalence of US residents with joint symptoms by age in 2006") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N06.GF_AgeReg.JS.png", width = 14, height = 9)


#ii) age-specific regional logistic regressions
#18 to 24
sub1824 <- subset(NHIS06_DO, AGE_P == "18 to 24")
N06.JS.age1824.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub1824)
summary(N06.JS.age1824.reg)
exp(cbind(OR = coef(N06.JS.age1824.reg), confint(N06.JS.age1824.reg)))

#25 to 29
sub2529 <- subset(NHIS06_DO, AGE_P == "25 to 29")
N06.JS.age2529.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub2529)
summary(N06.JS.age2529.reg)
exp(cbind(OR = coef(N06.JS.age2529.reg), confint(N06.JS.age2529.reg)))

#30 to 34
sub3034 <- subset(NHIS06_DO, AGE_P == "30 to 34")
N06.JS.age3034.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub3034)
summary(N06.JS.age3034.reg)
exp(cbind(OR = coef(N06.JS.age3034.reg), confint(N06.JS.age3034.reg)))

#35 to 39
sub3539 <- subset(NHIS06_DO, AGE_P == "35 to 39")
N06.JS.age3539.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub3539)
summary(N06.JS.age3539.reg)
exp(cbind(OR = coef(N06.JS.age3539.reg), confint(N06.JS.age3539.reg)))

#40 to 44
sub4044 <- subset(NHIS06_DO, AGE_P == "40 to 44")
N06.JS.age4044.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub4044)
summary(N06.JS.age4044.reg)
exp(cbind(OR = coef(N06.JS.age4044.reg), confint(N06.JS.age4044.reg)))

#45 to 49
sub4549 <- subset(NHIS06_DO, AGE_P == "45 to 49")
N06.JS.age4549.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub4549)
summary(N06.JS.age4549.reg)
exp(cbind(OR = coef(N06.JS.age4549.reg), confint(N06.JS.age4549.reg)))

#50 to 54
sub5054 <- subset(NHIS06_DO, AGE_P =="50 to 54")
N06.JS.age5054.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub5054)
summary(N06.JS.age5054.reg)
exp(cbind(OR = coef(N06.JS.age5054.reg), confint(N06.JS.age5054.reg)))

#55 to 59
sub5559 <- subset(NHIS06_DO, AGE_P == "55 to 59")
N06.JS.age5559.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub5559)
summary(N06.JS.age5559.reg)
exp(cbind(OR = coef(N06.JS.age5559.reg), confint(N06.JS.age5559.reg)))

#60 to 64
sub6064 <- subset(NHIS06_DO, AGE_P == "60 to 64")
N06.JS.age6064.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub6064)
summary(N06.JS.age6064.reg)
exp(cbind(OR = coef(N06.JS.age6064.reg), confint(N06.JS.age6064.reg)))

#65 to 69
sub6569 <- subset(NHIS06_DO, AGE_P == "65 to 69")
N06.JS.age6569.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub6569)
summary(N06.JS.age6569.reg)
exp(cbind(OR = coef(N06.JS.age6569.reg), confint(N06.JS.age6569.reg)))

#70 and above
sub70 <- subset(NHIS06_DO, AGE_P == "70 and above")
N06.JS.age70.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                           family = quasibinomial,
                           design = sub70)
summary(N06.JS.age70.reg)
exp(cbind(OR = coef(N06.JS.age70.reg), confint(N06.JS.age70.reg)))

#III) prevalence by region, by sex

#i)  sex-specific regional prevalence
N06_JS_reg.sex <- svyby(formula = ~JNTSYMP,
                        by = ~REGION + SEX,
                        design = NHIS06_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N06_JS_reg.sex.c <- N06_JS_reg.sex %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Sex", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N06_JS_reg.sex.ci <- confint(N06_JS_reg.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for js = 0
N06_JS_reg.sex.ci <- N06_JS_reg.sex.ci[-c(1:8), ]
#join proportion and ci
N06.JS.sex.region <- bind_cols(N06_JS_reg.sex.c, N06_JS_reg.sex.ci) #final proprtion, se & 95% ci
#save
#write.csv(N06.JS.sex.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N06.JS.region.sex.csv")

#Geofacet - Sex & Region
JS.Sex_Reg06 <- ggplot(N06.JS.sex.region, aes(Sex, Proportion)) +
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
  ggtitle("Regional prevalence (%) of US residents with joint symptoms by sex in 2006") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N06.GF_SexReg.JS.png", width = 14, height = 9)


#ii) sex-specific regional logistic regressions
#males
submales <- subset(NHIS06_DO, SEX == "Male")
N06.JS.sexmale.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + AGE_P,
                             family = quasibinomial,
                             design = submales)
summary(N06.JS.sexmale.reg)
exp(cbind(OR = coef(N06.JS.sexmale.reg), confint(N06.JS.sexmale.reg)))

#females
subfemales <- subset(NHIS06_DO, SEX == "Female")
N06.JS.sexfemale.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + AGE_P,
                               family = quasibinomial,
                               design = subfemales)
summary(N06.JS.sexfemale.reg)
exp(cbind(OR = coef(N06.JS.sexfemale.reg), confint(N06.JS.sexfemale.reg)))


#3. Demographic trends

#I)  Age trends

#i)  Prevalence by age
N06_JS_age <- svyby(formula = ~JNTSYMP,
                    by = ~AGE_P,
                    design = NHIS06_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N06_JS_age.c <- N06_JS_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N06_JS_age_ci <- confint(N06_JS_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove js=0 ci
N06_JS_age_ci <- N06_JS_age_ci[-c(1:11), ]
#join proportion and ci
N06_JS.age <- bind_cols(N06_JS_age.c, N06_JS_age_ci) #final proportion, se & 95% ci
#Save
#write.csv(N06_JS.age, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N06.JS.age.csv")


#ii) Logistic regression by age
N06_JS.age_glm <- svyglm(JNTSYMP~AGE_P + SEX,
                         family = quasibinomial,
                         design = NHIS06_DO)
summary(N06_JS.age_glm)
exp(cbind(OR=coef(N06_JS.age_glm), confint(N06_JS.age_glm)))



#II)  Sex trends

#i)  Prevalence by sex
N06_JS_sex <- svyby(formula = ~JNTSYMP,
                    by = ~SEX,
                    design = NHIS06_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N06_JS_sex.c <- N06_JS_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N06_JS_sex_ci <- confint(N06_JS_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N06_JS_sex_ci <- N06_JS_sex_ci[-c(1:2), ]
#join proportion and ci
N06.JS.sex <- bind_cols(N06_JS_sex.c, N06_JS_sex_ci)
#save
#write.csv(N06.JS.sex, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N06.JS.sex.csv")


#ii) Logistic regression by sex
N06.JS.sex.glm.c <- svyglm(JNTSYMP~relevel(SEX, ref = "Male") + AGE_P,
                           family = quasibinomial,
                           design = NHIS06_DO)
summary(N06.JS.sex.glm.c)
exp(cbind(OR=coef(N06.JS.sex.glm.c), confint(N06.JS.sex.glm.c)))


#Prevalence of various age-sex groups and their joint pain locations

#1. Right shoulder
N06_JS.rtshoulder <- svyby(formula = ~JMTHP1,
                           by = ~AGE_P + SEX,
                           design = NHIS06_DO,
                           FUN = svymean,
                           na.rm = TRUE)
N06_JS.rtshoulder.c <- N06_JS.rtshoulder %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.RtShoulder")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N06_JS.rtshoulder.ci <- confint(N06_JS.rtshoulder) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N06_JS.rtshoulder.ci <- N06_JS.rtshoulder.ci[-c(23:44), ]
#join proportion and ci
N06.JS.rtshoulder <- bind_cols(N06_JS.rtshoulder.c, N06_JS.rtshoulder.ci)
#save
#write.csv(N06.JS.rtshoulder, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N06.rtshoulder.csv")

#2. Left Shoulder
N06_JS.ltshoulder <- svyby(formula = ~JMTHP2,
                           by = ~AGE_P + SEX,
                           design = NHIS06_DO,
                           FUN = svymean,
                           na.rm = TRUE)
N06_JS.ltshoulder.c <- N06_JS.ltshoulder %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltshoulder")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N06_JS.ltshoulder.ci <- confint(N06_JS.ltshoulder) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N06_JS.ltshoulder.ci <- N06_JS.ltshoulder.ci[-c(23:44), ]
#join proportion and ci
N06.JS.ltshoulder <- bind_cols(N06_JS.ltshoulder.c, N06_JS.ltshoulder.ci)
#save
#write.csv(N06.JS.ltshoulder, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N06.ltshoulder.csv")


#3. Right elbow
N06_JS.rtelbow <- svyby(formula = ~JMTHP3,
                        by = ~AGE_P + SEX,
                        design = NHIS06_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N06_JS.rtelbow.c <- N06_JS.rtelbow %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtelbow")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N06_JS.rtelbow.ci <- confint(N06_JS.rtelbow) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N06_JS.rtelbow.ci <- N06_JS.rtelbow.ci[-c(23:44), ]
#join proportion and ci
N06.JS.rtelbow <- bind_cols(N06_JS.rtelbow.c, N06_JS.rtelbow.ci)
#save
#write.csv(N06.JS.rtelbow, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N06.rtelbow.csv")

#4. Lt elbow
N06_JS.ltelbow <- svyby(formula = ~JMTHP4,
                        by = ~AGE_P + SEX,
                        design = NHIS06_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N06_JS.ltelbow.c <- N06_JS.ltelbow %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltelbow")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N06_JS.ltelbow.ci <- confint(N06_JS.ltelbow) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N06_JS.ltelbow.ci <- N06_JS.ltelbow.ci[-c(23:44), ]
#join proportion and ci
N06.JS.ltelbow <- bind_cols(N06_JS.ltelbow.c, N06_JS.ltelbow.ci)
#save
#write.csv(N06.JS.ltelbow, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N06.ltelbow.csv")

#5. Right hip
N06_JS.rthip <- svyby(formula = ~JMTHP5,
                      by = ~AGE_P + SEX,
                      design = NHIS06_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N06_JS.rthip.c <- N06_JS.rthip %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rthip")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N06_JS.rthip.ci <- confint(N06_JS.rthip) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N06_JS.rthip.ci <- N06_JS.rthip.ci[-c(23:44), ]
#join proportion and ci
N06.JS.rthip <- bind_cols(N06_JS.rthip.c, N06_JS.rthip.ci)
#save
#write.csv(N06.JS.rthip, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N06.rthip.csv")

#6. Left hip
N06_JS.lthip <- svyby(formula = ~JMTHP6,
                      by = ~AGE_P + SEX,
                      design = NHIS06_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N06_JS.lthip.c <- N06_JS.lthip %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.lthip")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N06_JS.lthip.ci <- confint(N06_JS.lthip) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N06_JS.lthip.ci <- N06_JS.lthip.ci[-c(23:44), ]
#join proportion and ci
N06.JS.lthip <- bind_cols(N06_JS.lthip.c, N06_JS.lthip.ci)
#save
#write.csv(N06.JS.lthip, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N06.lthip.csv")

#7. Right wrist
N06_JS.rtwrist <- svyby(formula = ~JMTHP7,
                        by = ~AGE_P + SEX,
                        design = NHIS06_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N06_JS.rtwrist.c <- N06_JS.rtwrist %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtwrist")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N06_JS.rtwrist.ci <- confint(N06_JS.rtwrist) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N06_JS.rtwrist.ci <- N06_JS.rtwrist.ci[-c(23:44), ]
#join proportion and ci
N06.JS.rtwrist <- bind_cols(N06_JS.rtwrist.c, N06_JS.rtwrist.ci)
#save
#write.csv(N06.JS.rtwrist, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N06.rtwrist.csv")

#8. Left wrist
N06_JS.ltwrist <- svyby(formula = ~JMTHP8,
                        by = ~AGE_P + SEX,
                        design = NHIS06_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N06_JS.ltwrist.c <- N06_JS.ltwrist %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltwrist")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N06_JS.ltwrist.ci <- confint(N06_JS.ltwrist) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N06_JS.ltwrist.ci <- N06_JS.ltwrist.ci[-c(23:44), ]
#join proportion and ci
N06.JS.ltwrist <- bind_cols(N06_JS.ltwrist.c, N06_JS.ltwrist.ci)
#save
#write.csv(N06.JS.ltwrist, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N06.ltwrist.csv")

#9. Right knee
N06_JS.rtknee <- svyby(formula = ~JMTHP9,
                       by = ~AGE_P + SEX,
                       design = NHIS06_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N06_JS.rtknee.c <- N06_JS.rtknee %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtknee")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N06_JS.rtknee.ci <- confint(N06_JS.rtknee) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N06_JS.rtknee.ci <- N06_JS.rtknee.ci[-c(23:44), ]
#join proportion and ci
N06.JS.rtknee <- bind_cols(N06_JS.rtknee.c, N06_JS.rtknee.ci)
#save
#write.csv(N06.JS.rtknee, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N06.rtknee.csv")

#10. Left knee
N06_JS.ltknee <- svyby(formula = ~JMTHP10,
                       by = ~AGE_P + SEX,
                       design = NHIS06_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N06_JS.ltknee.c <- N06_JS.ltknee %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltknee")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N06_JS.ltknee.ci <- confint(N06_JS.ltknee) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N06_JS.ltknee.ci <- N06_JS.ltknee.ci[-c(23:44), ]
#join proportion and ci
N06.JS.ltknee <- bind_cols(N06_JS.ltknee.c, N06_JS.ltknee.ci)
#save
#write.csv(N06.JS.ltknee, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N06.ltknee.csv")

#11. Right ankle
N06_JS.rtankle <- svyby(formula = ~JMTHP11,
                        by = ~AGE_P + SEX,
                        design = NHIS06_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N06_JS.rtankle.c <- N06_JS.rtankle %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtankle")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N06_JS.rtankle.ci <- confint(N06_JS.rtankle) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N06_JS.rtankle.ci <- N06_JS.rtankle.ci[-c(23:44), ]
#join proportion and ci
N06.JS.rtankle <- bind_cols(N06_JS.rtankle.c, N06_JS.rtankle.ci)
#save
#write.csv(N06.JS.rtankle, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N06.rtankle.csv")

#12. Left ankle
N06_JS.ltankle <- svyby(formula = ~JMTHP12,
                        by = ~AGE_P + SEX,
                        design = NHIS06_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N06_JS.ltankle.c <- N06_JS.ltankle %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltankle")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N06_JS.ltankle.ci <- confint(N06_JS.ltankle) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N06_JS.ltankle.ci <- N06_JS.ltankle.ci[-c(23:44), ]
#join proportion and ci
N06.JS.ltankle <- bind_cols(N06_JS.ltankle.c, N06_JS.ltankle.ci)
#save
#write.csv(N06.JS.ltankle, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N06.ltankle.csv")

#13. Right toes
N06_JS.rttoes <- svyby(formula = ~JMTHP13,
                       by = ~AGE_P + SEX,
                       design = NHIS06_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N06_JS.rttoes.c <- N06_JS.rttoes %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rttoes")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N06_JS.rttoes.ci <- confint(N06_JS.rttoes) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N06_JS.rttoes.ci <- N06_JS.rttoes.ci[-c(23:44), ]
#join proportion and ci
N06.JS.rttoes <- bind_cols(N06_JS.rttoes.c, N06_JS.rttoes.ci)
#save
#write.csv(N06.JS.rttoes, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N06.rttoes.csv")

#14. Left toes
N06_JS.lttoes <- svyby(formula = ~JMTHP14,
                       by = ~AGE_P + SEX,
                       design = NHIS06_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N06_JS.lttoes.c <- N06_JS.lttoes %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.lttoes")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N06_JS.lttoes.ci <- confint(N06_JS.lttoes) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N06_JS.lttoes.ci <- N06_JS.lttoes.ci[-c(23:44), ]
#join proportion and ci
N06.JS.lttoes <- bind_cols(N06_JS.lttoes.c, N06_JS.lttoes.ci)
#save
#write.csv(N06.JS.lttoes, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N06.lttoes.csv")

#15. Right fingers/thumb
N06_JS.rtfingthumb <- svyby(formula = ~JMTHP15,
                            by = ~AGE_P + SEX,
                            design = NHIS06_DO,
                            FUN = svymean,
                            na.rm = TRUE)
N06_JS.rtfingthumb.c <- N06_JS.rtfingthumb %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtfingthumb")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N06_JS.rtfingthumb.ci <- confint(N06_JS.rtfingthumb) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N06_JS.rtfingthumb.ci <- N06_JS.rtfingthumb.ci[-c(23:44), ]
#join proportion and ci
N06.JS.rtfingthumb <- bind_cols(N06_JS.rtfingthumb.c, N06_JS.rtfingthumb.ci)
#save
#write.csv(N06.JS.rtfingthumb, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N06.rtfingthumb.csv")

#16. Left fingers/thumb
N06_JS.ltfingthumb <- svyby(formula = ~JMTHP16,
                            by = ~AGE_P + SEX,
                            design = NHIS06_DO,
                            FUN = svymean,
                            na.rm = TRUE)
N06_JS.ltfingthumb.c <- N06_JS.ltfingthumb %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltfingthumb")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N06_JS.ltfingthumb.ci <- confint(N06_JS.ltfingthumb) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N06_JS.ltfingthumb.ci <- N06_JS.ltfingthumb.ci[-c(23:44), ]
#join proportion and ci
N06.JS.ltfingthumb <- bind_cols(N06_JS.ltfingthumb.c, N06_JS.ltfingthumb.ci)
#save
#write.csv(N06.JS.ltfingthumb, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N06.ltfingthumb.csv")

#17. Another
N06_JS.another <- svyby(formula = ~JMTHP17,
                        by = ~AGE_P + SEX,
                        design = NHIS06_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N06_JS.another.c <- N06_JS.another %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.another")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N06_JS.another.ci <- confint(N06_JS.another) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N06_JS.another.ci <- N06_JS.another.ci[-c(23:44), ]
#join proportion and ci
N06.JS.another <- bind_cols(N06_JS.another.c, N06_JS.another.ci)
#save
#write.csv(N06.JS.another, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N06.another.csv")

N06.JSloc <- Reduce(function(x, y) merge(x, y, all = TRUE), list(N06_JS.rtshoulder.c, N06_JS.ltshoulder.c, N06_JS.rtelbow.c, N06_JS.ltelbow.c, N06_JS.rthip.c, N06_JS.lthip.c, N06_JS.rtwrist.c, N06_JS.ltwrist.c, N06_JS.rtknee.c, N06_JS.ltknee.c, N06_JS.rtankle.c, N06_JS.ltankle.c, N06_JS.rttoes.c, N06_JS.lttoes.c, N06_JS.rtfingthumb.c, N06_JS.ltfingthumb.c, N06_JS.another.c))

#Wide to long
N06.JSlocation <- gather(N06.JSloc, Location, Proportion, Proportion.RtShoulder:Proportion.another, factor_key = TRUE)
N06.JSlocation <- N06.JSlocation %>% 
  unite("Demographic", Age:Sex)
N06.JSlocation$Location <- as.character(N06.JSlocation$Location)
N06.JSlocation$Location[N06.JSlocation$Location == "Proportion.RtShoulder"] <- "Right shoulder"
N06.JSlocation$Location[N06.JSlocation$Location == "Proportion.ltshoulder"] <- "Left shoulder"
N06.JSlocation$Location[N06.JSlocation$Location == "Proportion.rtelbow"] <- "Right elbow"
N06.JSlocation$Location[N06.JSlocation$Location == "Proportion.ltelbow"] <- "Left elbow"
N06.JSlocation$Location[N06.JSlocation$Location == "Proportion.rthip"] <- "Right hip"
N06.JSlocation$Location[N06.JSlocation$Location == "Proportion.lthip"] <- "Left hip"
N06.JSlocation$Location[N06.JSlocation$Location == "Proportion.rtwrist"] <- "Right wrist"
N06.JSlocation$Location[N06.JSlocation$Location == "Proportion.ltwrist"] <- "Left wrist"
N06.JSlocation$Location[N06.JSlocation$Location == "Proportion.rtknee"] <- "Right knee"
N06.JSlocation$Location[N06.JSlocation$Location == "Proportion.ltknee"] <- "Left knee"
N06.JSlocation$Location[N06.JSlocation$Location == "Proportion.rtankle"] <- "Right ankle"
N06.JSlocation$Location[N06.JSlocation$Location == "Proportion.ltankle"] <- "Left ankle"
N06.JSlocation$Location[N06.JSlocation$Location == "Proportion.rttoes"] <- "Right toes"
N06.JSlocation$Location[N06.JSlocation$Location == "Proportion.lttoes"] <- "Left toes"
N06.JSlocation$Location[N06.JSlocation$Location == "Proportion.rtfingthumb"] <- "Right fingers or thumb"
N06.JSlocation$Location[N06.JSlocation$Location == "Proportion.ltfingthumb"] <- "Left fingers or thumb"
N06.JSlocation$Location[N06.JSlocation$Location == "Proportion.another"] <- "Another joint"

#Heatmap of joint locations by demographic groups
N06.JSlocmap <- ggplot(N06.JSlocation,
                       aes(x = Demographic, y = Location, fill = Proportion)) +
  geom_tile() + 
  scale_fill_viridis(option = "viridis", direction = -1, limits = c(0, 60), breaks = c(10, 20, 30, 40, 50, 60), discrete = FALSE) +
  theme_bw() + 
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.text.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  xlab("Age-sex profile") +
  theme(legend.position = "right",
        strip.background = element_blank()) +
  theme(legend.title = element_text(size = 26)) +
  theme(legend.text = element_text(size = 22)) +
  labs(fill = "Prevalence (%)") +
  guides(fill = guide_colourbar(barwidth = 3, barheight = 25)) +
  scale_x_discrete(labels = c("Females, 18 to 24", "Males, 18 to 24", "Females, 25 to 29", "Males, 25 to 29", "Females, 30 to 34", "Males, 30 to 34", "Females, 35 to 39", "Males, 35 to 39", "Females, 40 to 44", "Males, 40 to 44", "Females, 45 to 49", "Males, 45 to 49", "Females, 50 to 54", "Males, 50 to 54", "Females, 55 to 59", "Males, 55 to 59", "Females, 60 to 64", "Males, 60 to 64", "Females, 65 to 69", "Males, 65 to 69", "Females, 70 and above", "Males, 70 and above")) +
  scale_y_discrete(limits = c("Another joint", "Right toes", "Left toes", "Right ankle", "Left ankle", "Right knee", "Left knee", "Right hip", "Left hip", "Right shoulder", "Left shoulder", "Right elbow", "Left elbow", "Right wrist", "Left wrist", "Right fingers or thumb", "Left fingers or thumb"))
ggsave("N06.age-sex.png", width = 20, height = 13)


#III) Socioeconomic trends (employment status only)

#i)   Prevalence by employment status
N06_JS_emp <- svyby(formula = ~JNTSYMP,
                    by = ~DOINGLWA,
                    design = NHIS06_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N06_JS_emp.c <- N06_JS_emp %>%
  select(1, 3, 5) %>%
  setNames(c("Employment", "Proportion", "SE")) %>%
  mutate(Employment = as.factor(Employment)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N06_JS_emp_ci <- confint(N06_JS_emp) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for js = 0
N06_JS_emp_ci <- N06_JS_emp_ci[-c(1:2), ]
#join ci and proportion
N06.JS.emp <- bind_cols(N06_JS_emp.c, N06_JS_emp_ci) #final proportion, se & 95% ci
#save
#write.csv(N06.JS.emp, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N06.JS.emp.csv")

#ii)  Logistic regression by employment status
N06_JS.emp_glm <- svyglm(JNTSYMP~relevel(DOINGLWA, ref = "Working for pay at a job/business") + AGE_P + SEX,
                         family = quasibinomial,
                         design = NHIS06_DO)
summary(N06_JS.emp_glm)
exp(cbind(OR=coef(N06_JS.emp_glm), confint(N06_JS.emp_glm)))

#4. Lifestyle trends

#I)  BMI
#logistic regression
N06.BMI.glm <- svyglm(JNTSYMP ~ BMI + AGE_P + SEX,
                      family = quasibinomial,
                      design = NHIS06_DO)
summary(N06.BMI.glm)
exp(cbind(OR=coef(N06.BMI.glm), confint(N06.BMI.glm)))

#II) Physical activity
#logistic regression
N06.PA.JS.glm <- svyglm(JNTSYMP ~ relevel(factor(PAC, ordered = FALSE), ref = "Physically active") + AGE_P + SEX,
                        family = quasibinomial,
                        design = NHIS06_DO)
summary(N06.PA.JS.glm)
exp(cbind(OR = coef(N06.PA.JS.glm), confint(N06.PA.JS.glm)))

#____________________________________________________________________________________________________________________

# == 2005 == #

#1. Overall prevalence
N05_JS <- svymean(~factor(JNTSYMP), 
                  NHIS05_DO, 
                  na.rm = TRUE)

N05_JS.c <- N05_JS %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N05_JS_ci <- confint(N05_JS) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
N05.JS <- bind_cols(N05_JS.c, N05_JS_ci)
#remove js = 0
N05.JS <- N05.JS[-c(1), ] #final proportion, se & 95%
#save
#write.csv(N05.JS, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N05.JS.csv")

#2. Spatial analysis, by region

#I)   Overall population

#i)   regional prevalence for the overall population
N05_JS_reg <- svyby(formula = ~JNTSYMP,
                    by = ~REGION,
                    design = NHIS05_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N05_JS_reg.c <- N05_JS_reg %>%
  select(1, 3, 5) %>%
  setNames(c("Region", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N05_JS_reg.ci <- confint(N05_JS_reg) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for js = 0
N05_JS_reg.ci <- N05_JS_reg.ci[-c(1:4), ]
#join proportion and ci
N05.JS.region <- bind_cols(N05_JS_reg.c, N05_JS_reg.ci) #final proportion, se & 95% ci
#save
#write.csv(N05.JS.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N05.JS.region.csv")

#Create overall regional prevalence

#Change the region column to "NAME" to match sf
names(N05.JS.region)[names(N05.JS.region) == "Region"] <- "NAME"

N05.JS.joined <- regions %>%
  left_join(N05.JS.region)

N05.JS.joined$NAME <- as.factor(N05.JS.joined$NAME)

#Plot
JSregplot <- ggplot(data = N05.JS.joined) +
  geom_sf(aes(fill = Proportion), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  scale_fill_viridis_c(name = "Prevalence (%)", option = "viridis", direction = -1, limits = c(25, 40), breaks = c(25, 30, 35, 40)) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  ggtitle("Regional prevalence (%) of US residents with joint symptoms in 2005") +
  theme(plot.title = element_text(size = 10, hjust = 0.5))
ggsave("N05.JSregchor.png", width = 5, height = 5)


#ii)  logistic regression by region
N05.JS.reg.glm <- svyglm(relevel(JNTSYMP, ref = "0")~ REGION + AGE_P,
                         family = quasibinomial,
                         design = NHIS05_DO)
summary(N05.JS.reg.glm)
exp(cbind(OR=coef(N05.JS.reg.glm), confint(N05.JS.reg.glm)))

#II)  prevalence by region, by age

#i)  age-specific regional prevalance
N05_JS_reg.age <- svyby(formula = ~JNTSYMP,
                        by = ~REGION + AGE_P,
                        design = NHIS05_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N05_JS_reg.age.c <- N05_JS_reg.age %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Age", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Age = as.character(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N05_JS_reg.age.ci <- confint(N05_JS_reg.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for js = 0
N05_JS_reg.age.ci <- N05_JS_reg.age.ci[-c(1:44), ]
#join proportion and ci
N05.JS.age.region <- bind_cols(N05_JS_reg.age.c, N05_JS_reg.age.ci) #final proportion, se & 95% ci
#save
#write.csv(N05.JS.age.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N05.JS.region.age.csv")

#Geofacet plot - Age & Region
JS.Age_Reg <- ggplot(N05.JS.age.region, aes(Age, Proportion)) +
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
  ggtitle("Regional prevalence (%) of US residents with joint symptoms by age in 2005") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N05.GF_AgeReg.JS.png", width = 14, height = 9)


#ii) age-specific regional logistic regressions
#18 to 24
sub1824 <- subset(NHIS05_DO, AGE_P == "18 to 24")
N05.JS.age1824.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub1824)
summary(N05.JS.age1824.reg)
exp(cbind(OR = coef(N05.JS.age1824.reg), confint(N05.JS.age1824.reg)))

#25 to 29
sub2529 <- subset(NHIS05_DO, AGE_P == "25 to 29")
N05.JS.age2529.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub2529)
summary(N05.JS.age2529.reg)
exp(cbind(OR = coef(N05.JS.age2529.reg), confint(N05.JS.age2529.reg)))

#30 to 34
sub3034 <- subset(NHIS05_DO, AGE_P == "30 to 34")
N05.JS.age3034.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub3034)
summary(N05.JS.age3034.reg)
exp(cbind(OR = coef(N05.JS.age3034.reg), confint(N05.JS.age3034.reg)))

#35 to 39
sub3539 <- subset(NHIS05_DO, AGE_P == "35 to 39")
N05.JS.age3539.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub3539)
summary(N05.JS.age3539.reg)
exp(cbind(OR = coef(N05.JS.age3539.reg), confint(N05.JS.age3539.reg)))

#40 to 44
sub4044 <- subset(NHIS05_DO, AGE_P == "40 to 44")
N05.JS.age4044.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub4044)
summary(N05.JS.age4044.reg)
exp(cbind(OR = coef(N05.JS.age4044.reg), confint(N05.JS.age4044.reg)))

#45 to 49
sub4549 <- subset(NHIS05_DO, AGE_P == "45 to 49")
N05.JS.age4549.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub4549)
summary(N05.JS.age4549.reg)
exp(cbind(OR = coef(N05.JS.age4549.reg), confint(N05.JS.age4549.reg)))

#50 to 54
sub5054 <- subset(NHIS05_DO, AGE_P =="50 to 54")
N05.JS.age5054.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub5054)
summary(N05.JS.age5054.reg)
exp(cbind(OR = coef(N05.JS.age5054.reg), confint(N05.JS.age5054.reg)))

#55 to 59
sub5559 <- subset(NHIS05_DO, AGE_P == "55 to 59")
N05.JS.age5559.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub5559)
summary(N05.JS.age5559.reg)
exp(cbind(OR = coef(N05.JS.age5559.reg), confint(N05.JS.age5559.reg)))

#60 to 64
sub6064 <- subset(NHIS05_DO, AGE_P == "60 to 64")
N05.JS.age6064.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub6064)
summary(N05.JS.age6064.reg)
exp(cbind(OR = coef(N05.JS.age6064.reg), confint(N05.JS.age6064.reg)))

#65 to 69
sub6569 <- subset(NHIS05_DO, AGE_P == "65 to 69")
N05.JS.age6569.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub6569)
summary(N05.JS.age6569.reg)
exp(cbind(OR = coef(N05.JS.age6569.reg), confint(N05.JS.age6569.reg)))

#70 and above
sub70 <- subset(NHIS05_DO, AGE_P == "70 and above")
N05.JS.age70.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                           family = quasibinomial,
                           design = sub70)
summary(N05.JS.age70.reg)
exp(cbind(OR = coef(N05.JS.age70.reg), confint(N05.JS.age70.reg)))

#III) prevalence by region, by sex

#i)  sex-specific regional prevalence
N05_JS_reg.sex <- svyby(formula = ~JNTSYMP,
                        by = ~REGION + SEX,
                        design = NHIS05_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N05_JS_reg.sex.c <- N05_JS_reg.sex %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Sex", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N05_JS_reg.sex.ci <- confint(N05_JS_reg.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for js = 0
N05_JS_reg.sex.ci <- N05_JS_reg.sex.ci[-c(1:8), ]
#join proportion and ci
N05.JS.sex.region <- bind_cols(N05_JS_reg.sex.c, N05_JS_reg.sex.ci) #final proprtion, se & 95% ci
#save
#write.csv(N05.JS.sex.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N05.JS.region.sex.csv")

#Geofacet - Sex & Region
JS.Sex_Reg05 <- ggplot(N05.JS.sex.region, aes(Sex, Proportion)) +
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
  ggtitle("Regional prevalence (%) of US residents with joint symptoms by sex in 2005") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N05.GF_SexReg.JS.png", width = 14, height = 9)


#ii) sex-specific regional logistic regressions
#males
submales <- subset(NHIS05_DO, SEX == "Male")
N05.JS.sexmale.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + AGE_P,
                             family = quasibinomial,
                             design = submales)
summary(N05.JS.sexmale.reg)
exp(cbind(OR = coef(N05.JS.sexmale.reg), confint(N05.JS.sexmale.reg)))

#females
subfemales <- subset(NHIS05_DO, SEX == "Female")
N05.JS.sexfemale.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + AGE_P,
                               family = quasibinomial,
                               design = subfemales)
summary(N05.JS.sexfemale.reg)
exp(cbind(OR = coef(N05.JS.sexfemale.reg), confint(N05.JS.sexfemale.reg)))


#3. Demographic trends

#I)  Age trends

#i)  Prevalence by age
N05_JS_age <- svyby(formula = ~JNTSYMP,
                    by = ~AGE_P,
                    design = NHIS05_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N05_JS_age.c <- N05_JS_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N05_JS_age_ci <- confint(N05_JS_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove js=0 ci
N05_JS_age_ci <- N05_JS_age_ci[-c(1:11), ]
#join proportion and ci
N05_JS.age <- bind_cols(N05_JS_age.c, N05_JS_age_ci) #final proportion, se & 95% ci
#Save
#write.csv(N05_JS.age, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N05.JS.age.csv")


#ii) Logistic regression by age
N05_JS.age_glm <- svyglm(JNTSYMP~AGE_P + SEX,
                         family = quasibinomial,
                         design = NHIS05_DO)
summary(N05_JS.age_glm)
exp(cbind(OR=coef(N05_JS.age_glm), confint(N05_JS.age_glm)))



#II)  Sex trends

#i)  Prevalence by sex
N05_JS_sex <- svyby(formula = ~JNTSYMP,
                    by = ~SEX,
                    design = NHIS05_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N05_JS_sex.c <- N05_JS_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N05_JS_sex_ci <- confint(N05_JS_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N05_JS_sex_ci <- N05_JS_sex_ci[-c(1:2), ]
#join proportion and ci
N05.JS.sex <- bind_cols(N05_JS_sex.c, N05_JS_sex_ci)
#save
#write.csv(N05.JS.sex, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N05.JS.sex.csv")


#ii) Logistic regression by sex
N05.JS.sex.glm.c <- svyglm(JNTSYMP~relevel(SEX, ref = "Male") + AGE_P,
                           family = quasibinomial,
                           design = NHIS05_DO)
summary(N05.JS.sex.glm.c)
exp(cbind(OR=coef(N05.JS.sex.glm.c), confint(N05.JS.sex.glm.c)))


#Prevalence of various age-sex groups and their joint pain locations

#1. Right shoulder
N05_JS.rtshoulder <- svyby(formula = ~JMTHP1,
                           by = ~AGE_P + SEX,
                           design = NHIS05_DO,
                           FUN = svymean,
                           na.rm = TRUE)
N05_JS.rtshoulder.c <- N05_JS.rtshoulder %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.RtShoulder")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N05_JS.rtshoulder.ci <- confint(N05_JS.rtshoulder) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N05_JS.rtshoulder.ci <- N05_JS.rtshoulder.ci[-c(23:44), ]
#join proportion and ci
N05.JS.rtshoulder <- bind_cols(N05_JS.rtshoulder.c, N05_JS.rtshoulder.ci)
#save
#write.csv(N05.JS.rtshoulder, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N05.rtshoulder.csv")

#2. Left Shoulder
N05_JS.ltshoulder <- svyby(formula = ~JMTHP2,
                           by = ~AGE_P + SEX,
                           design = NHIS05_DO,
                           FUN = svymean,
                           na.rm = TRUE)
N05_JS.ltshoulder.c <- N05_JS.ltshoulder %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltshoulder")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N05_JS.ltshoulder.ci <- confint(N05_JS.ltshoulder) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N05_JS.ltshoulder.ci <- N05_JS.ltshoulder.ci[-c(23:44), ]
#join proportion and ci
N05.JS.ltshoulder <- bind_cols(N05_JS.ltshoulder.c, N05_JS.ltshoulder.ci)
#save
#write.csv(N05.JS.ltshoulder, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N05.ltshoulder.csv")


#3. Right elbow
N05_JS.rtelbow <- svyby(formula = ~JMTHP3,
                        by = ~AGE_P + SEX,
                        design = NHIS05_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N05_JS.rtelbow.c <- N05_JS.rtelbow %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtelbow")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N05_JS.rtelbow.ci <- confint(N05_JS.rtelbow) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N05_JS.rtelbow.ci <- N05_JS.rtelbow.ci[-c(23:44), ]
#join proportion and ci
N05.JS.rtelbow <- bind_cols(N05_JS.rtelbow.c, N05_JS.rtelbow.ci)
#save
#write.csv(N05.JS.rtelbow, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N05.rtelbow.csv")

#4. Lt elbow
N05_JS.ltelbow <- svyby(formula = ~JMTHP4,
                        by = ~AGE_P + SEX,
                        design = NHIS05_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N05_JS.ltelbow.c <- N05_JS.ltelbow %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltelbow")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N05_JS.ltelbow.ci <- confint(N05_JS.ltelbow) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N05_JS.ltelbow.ci <- N05_JS.ltelbow.ci[-c(23:44), ]
#join proportion and ci
N05.JS.ltelbow <- bind_cols(N05_JS.ltelbow.c, N05_JS.ltelbow.ci)
#save
#write.csv(N05.JS.ltelbow, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N05.ltelbow.csv")

#5. Right hip
N05_JS.rthip <- svyby(formula = ~JMTHP5,
                      by = ~AGE_P + SEX,
                      design = NHIS05_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N05_JS.rthip.c <- N05_JS.rthip %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rthip")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N05_JS.rthip.ci <- confint(N05_JS.rthip) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N05_JS.rthip.ci <- N05_JS.rthip.ci[-c(23:44), ]
#join proportion and ci
N05.JS.rthip <- bind_cols(N05_JS.rthip.c, N05_JS.rthip.ci)
#save
#write.csv(N05.JS.rthip, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N05.rthip.csv")

#6. Left hip
N05_JS.lthip <- svyby(formula = ~JMTHP6,
                      by = ~AGE_P + SEX,
                      design = NHIS05_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N05_JS.lthip.c <- N05_JS.lthip %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.lthip")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N05_JS.lthip.ci <- confint(N05_JS.lthip) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N05_JS.lthip.ci <- N05_JS.lthip.ci[-c(23:44), ]
#join proportion and ci
N05.JS.lthip <- bind_cols(N05_JS.lthip.c, N05_JS.lthip.ci)
#save
#write.csv(N05.JS.lthip, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N05.lthip.csv")

#7. Right wrist
N05_JS.rtwrist <- svyby(formula = ~JMTHP7,
                        by = ~AGE_P + SEX,
                        design = NHIS05_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N05_JS.rtwrist.c <- N05_JS.rtwrist %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtwrist")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N05_JS.rtwrist.ci <- confint(N05_JS.rtwrist) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N05_JS.rtwrist.ci <- N05_JS.rtwrist.ci[-c(23:44), ]
#join proportion and ci
N05.JS.rtwrist <- bind_cols(N05_JS.rtwrist.c, N05_JS.rtwrist.ci)
#save
#write.csv(N05.JS.rtwrist, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N05.rtwrist.csv")

#8. Left wrist
N05_JS.ltwrist <- svyby(formula = ~JMTHP8,
                        by = ~AGE_P + SEX,
                        design = NHIS05_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N05_JS.ltwrist.c <- N05_JS.ltwrist %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltwrist")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N05_JS.ltwrist.ci <- confint(N05_JS.ltwrist) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N05_JS.ltwrist.ci <- N05_JS.ltwrist.ci[-c(23:44), ]
#join proportion and ci
N05.JS.ltwrist <- bind_cols(N05_JS.ltwrist.c, N05_JS.ltwrist.ci)
#save
#write.csv(N05.JS.ltwrist, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N05.ltwrist.csv")

#9. Right knee
N05_JS.rtknee <- svyby(formula = ~JMTHP9,
                       by = ~AGE_P + SEX,
                       design = NHIS05_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N05_JS.rtknee.c <- N05_JS.rtknee %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtknee")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N05_JS.rtknee.ci <- confint(N05_JS.rtknee) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N05_JS.rtknee.ci <- N05_JS.rtknee.ci[-c(23:44), ]
#join proportion and ci
N05.JS.rtknee <- bind_cols(N05_JS.rtknee.c, N05_JS.rtknee.ci)
#save
#write.csv(N05.JS.rtknee, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N05.rtknee.csv")

#10. Left knee
N05_JS.ltknee <- svyby(formula = ~JMTHP10,
                       by = ~AGE_P + SEX,
                       design = NHIS05_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N05_JS.ltknee.c <- N05_JS.ltknee %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltknee")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N05_JS.ltknee.ci <- confint(N05_JS.ltknee) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N05_JS.ltknee.ci <- N05_JS.ltknee.ci[-c(23:44), ]
#join proportion and ci
N05.JS.ltknee <- bind_cols(N05_JS.ltknee.c, N05_JS.ltknee.ci)
#save
#write.csv(N05.JS.ltknee, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N05.ltknee.csv")

#11. Right ankle
N05_JS.rtankle <- svyby(formula = ~JMTHP11,
                        by = ~AGE_P + SEX,
                        design = NHIS05_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N05_JS.rtankle.c <- N05_JS.rtankle %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtankle")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N05_JS.rtankle.ci <- confint(N05_JS.rtankle) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N05_JS.rtankle.ci <- N05_JS.rtankle.ci[-c(23:44), ]
#join proportion and ci
N05.JS.rtankle <- bind_cols(N05_JS.rtankle.c, N05_JS.rtankle.ci)
#save
#write.csv(N05.JS.rtankle, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N05.rtankle.csv")

#12. Left ankle
N05_JS.ltankle <- svyby(formula = ~JMTHP12,
                        by = ~AGE_P + SEX,
                        design = NHIS05_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N05_JS.ltankle.c <- N05_JS.ltankle %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltankle")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N05_JS.ltankle.ci <- confint(N05_JS.ltankle) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N05_JS.ltankle.ci <- N05_JS.ltankle.ci[-c(23:44), ]
#join proportion and ci
N05.JS.ltankle <- bind_cols(N05_JS.ltankle.c, N05_JS.ltankle.ci)
#save
#write.csv(N05.JS.ltankle, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N05.ltankle.csv")

#13. Right toes
N05_JS.rttoes <- svyby(formula = ~JMTHP13,
                       by = ~AGE_P + SEX,
                       design = NHIS05_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N05_JS.rttoes.c <- N05_JS.rttoes %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rttoes")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N05_JS.rttoes.ci <- confint(N05_JS.rttoes) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N05_JS.rttoes.ci <- N05_JS.rttoes.ci[-c(23:44), ]
#join proportion and ci
N05.JS.rttoes <- bind_cols(N05_JS.rttoes.c, N05_JS.rttoes.ci)
#save
#write.csv(N05.JS.rttoes, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N05.rttoes.csv")

#14. Left toes
N05_JS.lttoes <- svyby(formula = ~JMTHP14,
                       by = ~AGE_P + SEX,
                       design = NHIS05_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N05_JS.lttoes.c <- N05_JS.lttoes %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.lttoes")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N05_JS.lttoes.ci <- confint(N05_JS.lttoes) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N05_JS.lttoes.ci <- N05_JS.lttoes.ci[-c(23:44), ]
#join proportion and ci
N05.JS.lttoes <- bind_cols(N05_JS.lttoes.c, N05_JS.lttoes.ci)
#save
#write.csv(N05.JS.lttoes, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N05.lttoes.csv")

#15. Right fingers/thumb
N05_JS.rtfingthumb <- svyby(formula = ~JMTHP15,
                            by = ~AGE_P + SEX,
                            design = NHIS05_DO,
                            FUN = svymean,
                            na.rm = TRUE)
N05_JS.rtfingthumb.c <- N05_JS.rtfingthumb %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtfingthumb")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N05_JS.rtfingthumb.ci <- confint(N05_JS.rtfingthumb) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N05_JS.rtfingthumb.ci <- N05_JS.rtfingthumb.ci[-c(23:44), ]
#join proportion and ci
N05.JS.rtfingthumb <- bind_cols(N05_JS.rtfingthumb.c, N05_JS.rtfingthumb.ci)
#save
#write.csv(N05.JS.rtfingthumb, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N05.rtfingthumb.csv")

#16. Left fingers/thumb
N05_JS.ltfingthumb <- svyby(formula = ~JMTHP16,
                            by = ~AGE_P + SEX,
                            design = NHIS05_DO,
                            FUN = svymean,
                            na.rm = TRUE)
N05_JS.ltfingthumb.c <- N05_JS.ltfingthumb %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltfingthumb")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N05_JS.ltfingthumb.ci <- confint(N05_JS.ltfingthumb) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N05_JS.ltfingthumb.ci <- N05_JS.ltfingthumb.ci[-c(23:44), ]
#join proportion and ci
N05.JS.ltfingthumb <- bind_cols(N05_JS.ltfingthumb.c, N05_JS.ltfingthumb.ci)
#save
#write.csv(N05.JS.ltfingthumb, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N05.ltfingthumb.csv")

#17. Another
N05_JS.another <- svyby(formula = ~JMTHP17,
                        by = ~AGE_P + SEX,
                        design = NHIS05_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N05_JS.another.c <- N05_JS.another %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.another")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N05_JS.another.ci <- confint(N05_JS.another) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N05_JS.another.ci <- N05_JS.another.ci[-c(23:44), ]
#join proportion and ci
N05.JS.another <- bind_cols(N05_JS.another.c, N05_JS.another.ci)
#save
#write.csv(N05.JS.another, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N05.another.csv")

N05.JSloc <- Reduce(function(x, y) merge(x, y, all = TRUE), list(N05_JS.rtshoulder.c, N05_JS.ltshoulder.c, N05_JS.rtelbow.c, N05_JS.ltelbow.c, N05_JS.rthip.c, N05_JS.lthip.c, N05_JS.rtwrist.c, N05_JS.ltwrist.c, N05_JS.rtknee.c, N05_JS.ltknee.c, N05_JS.rtankle.c, N05_JS.ltankle.c, N05_JS.rttoes.c, N05_JS.lttoes.c, N05_JS.rtfingthumb.c, N05_JS.ltfingthumb.c, N05_JS.another.c))

#Wide to long
N05.JSlocation <- gather(N05.JSloc, Location, Proportion, Proportion.RtShoulder:Proportion.another, factor_key = TRUE)
N05.JSlocation <- N05.JSlocation %>% 
  unite("Demographic", Age:Sex)
N05.JSlocation$Location <- as.character(N05.JSlocation$Location)
N05.JSlocation$Location[N05.JSlocation$Location == "Proportion.RtShoulder"] <- "Right shoulder"
N05.JSlocation$Location[N05.JSlocation$Location == "Proportion.ltshoulder"] <- "Left shoulder"
N05.JSlocation$Location[N05.JSlocation$Location == "Proportion.rtelbow"] <- "Right elbow"
N05.JSlocation$Location[N05.JSlocation$Location == "Proportion.ltelbow"] <- "Left elbow"
N05.JSlocation$Location[N05.JSlocation$Location == "Proportion.rthip"] <- "Right hip"
N05.JSlocation$Location[N05.JSlocation$Location == "Proportion.lthip"] <- "Left hip"
N05.JSlocation$Location[N05.JSlocation$Location == "Proportion.rtwrist"] <- "Right wrist"
N05.JSlocation$Location[N05.JSlocation$Location == "Proportion.ltwrist"] <- "Left wrist"
N05.JSlocation$Location[N05.JSlocation$Location == "Proportion.rtknee"] <- "Right knee"
N05.JSlocation$Location[N05.JSlocation$Location == "Proportion.ltknee"] <- "Left knee"
N05.JSlocation$Location[N05.JSlocation$Location == "Proportion.rtankle"] <- "Right ankle"
N05.JSlocation$Location[N05.JSlocation$Location == "Proportion.ltankle"] <- "Left ankle"
N05.JSlocation$Location[N05.JSlocation$Location == "Proportion.rttoes"] <- "Right toes"
N05.JSlocation$Location[N05.JSlocation$Location == "Proportion.lttoes"] <- "Left toes"
N05.JSlocation$Location[N05.JSlocation$Location == "Proportion.rtfingthumb"] <- "Right fingers or thumb"
N05.JSlocation$Location[N05.JSlocation$Location == "Proportion.ltfingthumb"] <- "Left fingers or thumb"
N05.JSlocation$Location[N05.JSlocation$Location == "Proportion.another"] <- "Another joint"

#Heatmap of joint locations by demographic groups
N05.JSlocmap <- ggplot(N05.JSlocation,
                       aes(x = Demographic, y = Location, fill = Proportion)) +
  geom_tile() + 
  scale_fill_viridis(option = "viridis", direction = -1, limits = c(0, 60), breaks = c(10, 20, 30, 40, 50, 60), discrete = FALSE) +
  theme_bw() + 
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.text.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  xlab("Age-sex profile") +
  theme(legend.position = "right",
        strip.background = element_blank()) +
  theme(legend.title = element_text(size = 26)) +
  theme(legend.text = element_text(size = 22)) +
  labs(fill = "Prevalence (%)") +
  guides(fill = guide_colourbar(barwidth = 3, barheight = 25)) +
  scale_x_discrete(labels = c("Females, 18 to 24", "Males, 18 to 24", "Females, 25 to 29", "Males, 25 to 29", "Females, 30 to 34", "Males, 30 to 34", "Females, 35 to 39", "Males, 35 to 39", "Females, 40 to 44", "Males, 40 to 44", "Females, 45 to 49", "Males, 45 to 49", "Females, 50 to 54", "Males, 50 to 54", "Females, 55 to 59", "Males, 55 to 59", "Females, 60 to 64", "Males, 60 to 64", "Females, 65 to 69", "Males, 65 to 69", "Females, 70 and above", "Males, 70 and above")) +
  scale_y_discrete(limits = c("Another joint", "Right toes", "Left toes", "Right ankle", "Left ankle", "Right knee", "Left knee", "Right hip", "Left hip", "Right shoulder", "Left shoulder", "Right elbow", "Left elbow", "Right wrist", "Left wrist", "Right fingers or thumb", "Left fingers or thumb"))
ggsave("N05.age-sex.png", width = 20, height = 13)


#III) Socioeconomic trends (employment status only)

#i)   Prevalence by employment status
N05_JS_emp <- svyby(formula = ~JNTSYMP,
                    by = ~DOINGLWA,
                    design = NHIS05_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N05_JS_emp.c <- N05_JS_emp %>%
  select(1, 3, 5) %>%
  setNames(c("Employment", "Proportion", "SE")) %>%
  mutate(Employment = as.factor(Employment)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N05_JS_emp_ci <- confint(N05_JS_emp) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for js = 0
N05_JS_emp_ci <- N05_JS_emp_ci[-c(1:2), ]
#join ci and proportion
N05.JS.emp <- bind_cols(N05_JS_emp.c, N05_JS_emp_ci) #final proportion, se & 95% ci
#save
#write.csv(N05.JS.emp, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N05.JS.emp.csv")

#ii)  Logistic regression by employment status
N05_JS.emp_glm <- svyglm(JNTSYMP~relevel(DOINGLWA, ref = "Working for pay at a job/business") + AGE_P + SEX,
                         family = quasibinomial,
                         design = NHIS05_DO)
summary(N05_JS.emp_glm)
exp(cbind(OR=coef(N05_JS.emp_glm), confint(N05_JS.emp_glm)))

#4. Lifestyle trends

#I)  BMI
#logistic regression
N05.BMI.glm <- svyglm(JNTSYMP ~ BMI + AGE_P + SEX,
                      family = quasibinomial,
                      design = NHIS05_DO)
summary(N05.BMI.glm)
exp(cbind(OR=coef(N05.BMI.glm), confint(N05.BMI.glm)))

#II) Physical activity
#logistic regression
N05.PA.JS.glm <- svyglm(JNTSYMP ~ relevel(factor(PAC, ordered = FALSE), ref = "Physically active") + AGE_P + SEX,
                        family = quasibinomial,
                        design = NHIS05_DO)
summary(N05.PA.JS.glm)
exp(cbind(OR = coef(N05.PA.JS.glm), confint(N05.PA.JS.glm)))

#_____________________________________________________________________________________________________________________

# == 2004 == #

# - no 2004 analysis - #

#_____________________________________________________________________________________________________________________

# == 2003 == #

#1. Overall prevalence
N03_JS <- svymean(~factor(JNTSYMP), 
                  NHIS03_DO, 
                  na.rm = TRUE)

N03_JS.c <- N03_JS %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N03_JS_ci <- confint(N03_JS) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
N03.JS <- bind_cols(N03_JS.c, N03_JS_ci)
#remove js = 0
N03.JS <- N03.JS[-c(1), ] #final proportion, se & 95%
#save
#write.csv(N03.JS, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N03.JS.csv")

#2. Spatial analysis, by region

#I)   Overall population

#i)   regional prevalence for the overall population
N03_JS_reg <- svyby(formula = ~JNTSYMP,
                    by = ~REGION,
                    design = NHIS03_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N03_JS_reg.c <- N03_JS_reg %>%
  select(1, 3, 5) %>%
  setNames(c("Region", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N03_JS_reg.ci <- confint(N03_JS_reg) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for js = 0
N03_JS_reg.ci <- N03_JS_reg.ci[-c(1:4), ]
#join proportion and ci
N03.JS.region <- bind_cols(N03_JS_reg.c, N03_JS_reg.ci) #final proportion, se & 95% ci
#save
#write.csv(N03.JS.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N03.JS.region.csv")

#Create overall regional prevalence

#Change the region column to "NAME" to match sf
names(N03.JS.region)[names(N03.JS.region) == "Region"] <- "NAME"

N03.JS.joined <- regions %>%
  left_join(N03.JS.region)

N03.JS.joined$NAME <- as.factor(N03.JS.joined$NAME)

#Plot
JSregplot <- ggplot(data = N03.JS.joined) +
  geom_sf(aes(fill = Proportion), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  scale_fill_viridis_c(name = "Prevalence (%)", option = "viridis", direction = -1, limits = c(25, 40), breaks = c(25, 30, 35, 40)) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  ggtitle("Regional prevalence (%) of US residents with joint symptoms in 2003") +
  theme(plot.title = element_text(size = 10, hjust = 0.5))
ggsave("N03.JSregchor.png", width = 5, height = 5)


#ii)  logistic regression by region
N03.JS.reg.glm <- svyglm(relevel(JNTSYMP, ref = "0")~ REGION + AGE_P,
                         family = quasibinomial,
                         design = NHIS03_DO)
summary(N03.JS.reg.glm)
exp(cbind(OR=coef(N03.JS.reg.glm), confint(N03.JS.reg.glm)))

#II)  prevalence by region, by age

#i)  age-specific regional prevalance
N03_JS_reg.age <- svyby(formula = ~JNTSYMP,
                        by = ~REGION + AGE_P,
                        design = NHIS03_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N03_JS_reg.age.c <- N03_JS_reg.age %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Age", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Age = as.character(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N03_JS_reg.age.ci <- confint(N03_JS_reg.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for js = 0
N03_JS_reg.age.ci <- N03_JS_reg.age.ci[-c(1:44), ]
#join proportion and ci
N03.JS.age.region <- bind_cols(N03_JS_reg.age.c, N03_JS_reg.age.ci) #final proportion, se & 95% ci
#save
#write.csv(N03.JS.age.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N03.JS.region.age.csv")

#Geofacet plot - Age & Region
JS.Age_Reg <- ggplot(N03.JS.age.region, aes(Age, Proportion)) +
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
  ggtitle("Regional prevalence (%) of US residents with joint symptoms by age in 2003") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N03.GF_AgeReg.JS.png", width = 14, height = 9)


#ii) age-specific regional logistic regressions
#18 to 24
sub1824 <- subset(NHIS03_DO, AGE_P == "18 to 24")
N03.JS.age1824.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub1824)
summary(N03.JS.age1824.reg)
exp(cbind(OR = coef(N03.JS.age1824.reg), confint(N03.JS.age1824.reg)))

#25 to 29
sub2529 <- subset(NHIS03_DO, AGE_P == "25 to 29")
N03.JS.age2529.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub2529)
summary(N03.JS.age2529.reg)
exp(cbind(OR = coef(N03.JS.age2529.reg), confint(N03.JS.age2529.reg)))

#30 to 34
sub3034 <- subset(NHIS03_DO, AGE_P == "30 to 34")
N03.JS.age3034.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub3034)
summary(N03.JS.age3034.reg)
exp(cbind(OR = coef(N03.JS.age3034.reg), confint(N03.JS.age3034.reg)))

#35 to 39
sub3539 <- subset(NHIS03_DO, AGE_P == "35 to 39")
N03.JS.age3539.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub3539)
summary(N03.JS.age3539.reg)
exp(cbind(OR = coef(N03.JS.age3539.reg), confint(N03.JS.age3539.reg)))

#40 to 44
sub4044 <- subset(NHIS03_DO, AGE_P == "40 to 44")
N03.JS.age4044.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub4044)
summary(N03.JS.age4044.reg)
exp(cbind(OR = coef(N03.JS.age4044.reg), confint(N03.JS.age4044.reg)))

#45 to 49
sub4549 <- subset(NHIS03_DO, AGE_P == "45 to 49")
N03.JS.age4549.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub4549)
summary(N03.JS.age4549.reg)
exp(cbind(OR = coef(N03.JS.age4549.reg), confint(N03.JS.age4549.reg)))

#50 to 54
sub5054 <- subset(NHIS03_DO, AGE_P =="50 to 54")
N03.JS.age5054.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub5054)
summary(N03.JS.age5054.reg)
exp(cbind(OR = coef(N03.JS.age5054.reg), confint(N03.JS.age5054.reg)))

#55 to 59
sub5559 <- subset(NHIS03_DO, AGE_P == "55 to 59")
N03.JS.age5559.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub5559)
summary(N03.JS.age5559.reg)
exp(cbind(OR = coef(N03.JS.age5559.reg), confint(N03.JS.age5559.reg)))

#60 to 64
sub6064 <- subset(NHIS03_DO, AGE_P == "60 to 64")
N03.JS.age6064.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub6064)
summary(N03.JS.age6064.reg)
exp(cbind(OR = coef(N03.JS.age6064.reg), confint(N03.JS.age6064.reg)))

#65 to 69
sub6569 <- subset(NHIS03_DO, AGE_P == "65 to 69")
N03.JS.age6569.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub6569)
summary(N03.JS.age6569.reg)
exp(cbind(OR = coef(N03.JS.age6569.reg), confint(N03.JS.age6569.reg)))

#70 and above
sub70 <- subset(NHIS03_DO, AGE_P == "70 and above")
N03.JS.age70.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                           family = quasibinomial,
                           design = sub70)
summary(N03.JS.age70.reg)
exp(cbind(OR = coef(N03.JS.age70.reg), confint(N03.JS.age70.reg)))

#III) prevalence by region, by sex

#i)  sex-specific regional prevalence
N03_JS_reg.sex <- svyby(formula = ~JNTSYMP,
                        by = ~REGION + SEX,
                        design = NHIS03_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N03_JS_reg.sex.c <- N03_JS_reg.sex %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Sex", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N03_JS_reg.sex.ci <- confint(N03_JS_reg.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for js = 0
N03_JS_reg.sex.ci <- N03_JS_reg.sex.ci[-c(1:8), ]
#join proportion and ci
N03.JS.sex.region <- bind_cols(N03_JS_reg.sex.c, N03_JS_reg.sex.ci) #final proprtion, se & 95% ci
#save
#write.csv(N03.JS.sex.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N03.JS.region.sex.csv")

#Geofacet - Sex & Region
JS.Sex_Reg03 <- ggplot(N03.JS.sex.region, aes(Sex, Proportion)) +
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
  ggtitle("Regional prevalence (%) of US residents with joint symptoms by sex in 2003") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N03.GF_SexReg.JS.png", width = 14, height = 9)


#ii) sex-specific regional logistic regressions
#males
submales <- subset(NHIS03_DO, SEX == "Male")
N03.JS.sexmale.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + AGE_P,
                             family = quasibinomial,
                             design = submales)
summary(N03.JS.sexmale.reg)
exp(cbind(OR = coef(N03.JS.sexmale.reg), confint(N03.JS.sexmale.reg)))

#females
subfemales <- subset(NHIS03_DO, SEX == "Female")
N03.JS.sexfemale.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + AGE_P,
                               family = quasibinomial,
                               design = subfemales)
summary(N03.JS.sexfemale.reg)
exp(cbind(OR = coef(N03.JS.sexfemale.reg), confint(N03.JS.sexfemale.reg)))


#3. Demographic trends

#I)  Age trends

#i)  Prevalence by age
N03_JS_age <- svyby(formula = ~JNTSYMP,
                    by = ~AGE_P,
                    design = NHIS03_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N03_JS_age.c <- N03_JS_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N03_JS_age_ci <- confint(N03_JS_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove js=0 ci
N03_JS_age_ci <- N03_JS_age_ci[-c(1:11), ]
#join proportion and ci
N03_JS.age <- bind_cols(N03_JS_age.c, N03_JS_age_ci) #final proportion, se & 95% ci
#Save
#write.csv(N03_JS.age, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N03.JS.age.csv")


#ii) Logistic regression by age
N03_JS.age_glm <- svyglm(JNTSYMP~AGE_P + SEX,
                         family = quasibinomial,
                         design = NHIS03_DO)
summary(N03_JS.age_glm)
exp(cbind(OR=coef(N03_JS.age_glm), confint(N03_JS.age_glm)))



#II)  Sex trends

#i)  Prevalence by sex
N03_JS_sex <- svyby(formula = ~JNTSYMP,
                    by = ~SEX,
                    design = NHIS03_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N03_JS_sex.c <- N03_JS_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N03_JS_sex_ci <- confint(N03_JS_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N03_JS_sex_ci <- N03_JS_sex_ci[-c(1:2), ]
#join proportion and ci
N03.JS.sex <- bind_cols(N03_JS_sex.c, N03_JS_sex_ci)
#save
#write.csv(N03.JS.sex, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N03.JS.sex.csv")


#ii) Logistic regression by sex
N03.JS.sex.glm.c <- svyglm(JNTSYMP~relevel(SEX, ref = "Male") + AGE_P,
                           family = quasibinomial,
                           design = NHIS03_DO)
summary(N03.JS.sex.glm.c)
exp(cbind(OR=coef(N03.JS.sex.glm.c), confint(N03.JS.sex.glm.c)))


#Prevalence of various age-sex groups and their joint pain locations

#1. Right shoulder
N03_JS.rtshoulder <- svyby(formula = ~JMTHP1,
                           by = ~AGE_P + SEX,
                           design = NHIS03_DO,
                           FUN = svymean,
                           na.rm = TRUE)
N03_JS.rtshoulder.c <- N03_JS.rtshoulder %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.RtShoulder")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N03_JS.rtshoulder.ci <- confint(N03_JS.rtshoulder) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N03_JS.rtshoulder.ci <- N03_JS.rtshoulder.ci[-c(23:44), ]
#join proportion and ci
N03.JS.rtshoulder <- bind_cols(N03_JS.rtshoulder.c, N03_JS.rtshoulder.ci)
#save
#write.csv(N03.JS.rtshoulder, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N03.rtshoulder.csv")

#2. Left Shoulder
N03_JS.ltshoulder <- svyby(formula = ~JMTHP2,
                           by = ~AGE_P + SEX,
                           design = NHIS03_DO,
                           FUN = svymean,
                           na.rm = TRUE)
N03_JS.ltshoulder.c <- N03_JS.ltshoulder %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltshoulder")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N03_JS.ltshoulder.ci <- confint(N03_JS.ltshoulder) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N03_JS.ltshoulder.ci <- N03_JS.ltshoulder.ci[-c(23:44), ]
#join proportion and ci
N03.JS.ltshoulder <- bind_cols(N03_JS.ltshoulder.c, N03_JS.ltshoulder.ci)
#save
#write.csv(N03.JS.ltshoulder, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N03.ltshoulder.csv")


#3. Right elbow
N03_JS.rtelbow <- svyby(formula = ~JMTHP3,
                        by = ~AGE_P + SEX,
                        design = NHIS03_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N03_JS.rtelbow.c <- N03_JS.rtelbow %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtelbow")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N03_JS.rtelbow.ci <- confint(N03_JS.rtelbow) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N03_JS.rtelbow.ci <- N03_JS.rtelbow.ci[-c(23:44), ]
#join proportion and ci
N03.JS.rtelbow <- bind_cols(N03_JS.rtelbow.c, N03_JS.rtelbow.ci)
#save
#write.csv(N03.JS.rtelbow, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N03.rtelbow.csv")

#4. Lt elbow
N03_JS.ltelbow <- svyby(formula = ~JMTHP4,
                        by = ~AGE_P + SEX,
                        design = NHIS03_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N03_JS.ltelbow.c <- N03_JS.ltelbow %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltelbow")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N03_JS.ltelbow.ci <- confint(N03_JS.ltelbow) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N03_JS.ltelbow.ci <- N03_JS.ltelbow.ci[-c(23:44), ]
#join proportion and ci
N03.JS.ltelbow <- bind_cols(N03_JS.ltelbow.c, N03_JS.ltelbow.ci)
#save
#write.csv(N03.JS.ltelbow, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N03.ltelbow.csv")

#5. Right hip
N03_JS.rthip <- svyby(formula = ~JMTHP5,
                      by = ~AGE_P + SEX,
                      design = NHIS03_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N03_JS.rthip.c <- N03_JS.rthip %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rthip")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N03_JS.rthip.ci <- confint(N03_JS.rthip) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N03_JS.rthip.ci <- N03_JS.rthip.ci[-c(23:44), ]
#join proportion and ci
N03.JS.rthip <- bind_cols(N03_JS.rthip.c, N03_JS.rthip.ci)
#save
#write.csv(N03.JS.rthip, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N03.rthip.csv")

#6. Left hip
N03_JS.lthip <- svyby(formula = ~JMTHP6,
                      by = ~AGE_P + SEX,
                      design = NHIS03_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N03_JS.lthip.c <- N03_JS.lthip %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.lthip")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N03_JS.lthip.ci <- confint(N03_JS.lthip) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N03_JS.lthip.ci <- N03_JS.lthip.ci[-c(23:44), ]
#join proportion and ci
N03.JS.lthip <- bind_cols(N03_JS.lthip.c, N03_JS.lthip.ci)
#save
#write.csv(N03.JS.lthip, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N03.lthip.csv")

#7. Right wrist
N03_JS.rtwrist <- svyby(formula = ~JMTHP7,
                        by = ~AGE_P + SEX,
                        design = NHIS03_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N03_JS.rtwrist.c <- N03_JS.rtwrist %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtwrist")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N03_JS.rtwrist.ci <- confint(N03_JS.rtwrist) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N03_JS.rtwrist.ci <- N03_JS.rtwrist.ci[-c(23:44), ]
#join proportion and ci
N03.JS.rtwrist <- bind_cols(N03_JS.rtwrist.c, N03_JS.rtwrist.ci)
#save
#write.csv(N03.JS.rtwrist, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N03.rtwrist.csv")

#8. Left wrist
N03_JS.ltwrist <- svyby(formula = ~JMTHP8,
                        by = ~AGE_P + SEX,
                        design = NHIS03_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N03_JS.ltwrist.c <- N03_JS.ltwrist %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltwrist")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N03_JS.ltwrist.ci <- confint(N03_JS.ltwrist) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N03_JS.ltwrist.ci <- N03_JS.ltwrist.ci[-c(23:44), ]
#join proportion and ci
N03.JS.ltwrist <- bind_cols(N03_JS.ltwrist.c, N03_JS.ltwrist.ci)
#save
#write.csv(N03.JS.ltwrist, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N03.ltwrist.csv")

#9. Right knee
N03_JS.rtknee <- svyby(formula = ~JMTHP9,
                       by = ~AGE_P + SEX,
                       design = NHIS03_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N03_JS.rtknee.c <- N03_JS.rtknee %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtknee")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N03_JS.rtknee.ci <- confint(N03_JS.rtknee) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N03_JS.rtknee.ci <- N03_JS.rtknee.ci[-c(23:44), ]
#join proportion and ci
N03.JS.rtknee <- bind_cols(N03_JS.rtknee.c, N03_JS.rtknee.ci)
#save
#write.csv(N03.JS.rtknee, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N03.rtknee.csv")

#10. Left knee
N03_JS.ltknee <- svyby(formula = ~JMTHP10,
                       by = ~AGE_P + SEX,
                       design = NHIS03_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N03_JS.ltknee.c <- N03_JS.ltknee %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltknee")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N03_JS.ltknee.ci <- confint(N03_JS.ltknee) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N03_JS.ltknee.ci <- N03_JS.ltknee.ci[-c(23:44), ]
#join proportion and ci
N03.JS.ltknee <- bind_cols(N03_JS.ltknee.c, N03_JS.ltknee.ci)
#save
#write.csv(N03.JS.ltknee, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N03.ltknee.csv")

#11. Right ankle
N03_JS.rtankle <- svyby(formula = ~JMTHP11,
                        by = ~AGE_P + SEX,
                        design = NHIS03_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N03_JS.rtankle.c <- N03_JS.rtankle %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtankle")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N03_JS.rtankle.ci <- confint(N03_JS.rtankle) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N03_JS.rtankle.ci <- N03_JS.rtankle.ci[-c(23:44), ]
#join proportion and ci
N03.JS.rtankle <- bind_cols(N03_JS.rtankle.c, N03_JS.rtankle.ci)
#save
#write.csv(N03.JS.rtankle, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N03.rtankle.csv")

#12. Left ankle
N03_JS.ltankle <- svyby(formula = ~JMTHP12,
                        by = ~AGE_P + SEX,
                        design = NHIS03_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N03_JS.ltankle.c <- N03_JS.ltankle %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltankle")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N03_JS.ltankle.ci <- confint(N03_JS.ltankle) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N03_JS.ltankle.ci <- N03_JS.ltankle.ci[-c(23:44), ]
#join proportion and ci
N03.JS.ltankle <- bind_cols(N03_JS.ltankle.c, N03_JS.ltankle.ci)
#save
#write.csv(N03.JS.ltankle, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N03.ltankle.csv")

#13. Right toes
N03_JS.rttoes <- svyby(formula = ~JMTHP13,
                       by = ~AGE_P + SEX,
                       design = NHIS03_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N03_JS.rttoes.c <- N03_JS.rttoes %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rttoes")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N03_JS.rttoes.ci <- confint(N03_JS.rttoes) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N03_JS.rttoes.ci <- N03_JS.rttoes.ci[-c(23:44), ]
#join proportion and ci
N03.JS.rttoes <- bind_cols(N03_JS.rttoes.c, N03_JS.rttoes.ci)
#save
#write.csv(N03.JS.rttoes, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N03.rttoes.csv")

#14. Left toes
N03_JS.lttoes <- svyby(formula = ~JMTHP14,
                       by = ~AGE_P + SEX,
                       design = NHIS03_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N03_JS.lttoes.c <- N03_JS.lttoes %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.lttoes")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N03_JS.lttoes.ci <- confint(N03_JS.lttoes) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N03_JS.lttoes.ci <- N03_JS.lttoes.ci[-c(23:44), ]
#join proportion and ci
N03.JS.lttoes <- bind_cols(N03_JS.lttoes.c, N03_JS.lttoes.ci)
#save
#write.csv(N03.JS.lttoes, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N03.lttoes.csv")

#15. Right fingers/thumb
N03_JS.rtfingthumb <- svyby(formula = ~JMTHP15,
                            by = ~AGE_P + SEX,
                            design = NHIS03_DO,
                            FUN = svymean,
                            na.rm = TRUE)
N03_JS.rtfingthumb.c <- N03_JS.rtfingthumb %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtfingthumb")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N03_JS.rtfingthumb.ci <- confint(N03_JS.rtfingthumb) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N03_JS.rtfingthumb.ci <- N03_JS.rtfingthumb.ci[-c(23:44), ]
#join proportion and ci
N03.JS.rtfingthumb <- bind_cols(N03_JS.rtfingthumb.c, N03_JS.rtfingthumb.ci)
#save
#write.csv(N03.JS.rtfingthumb, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N03.rtfingthumb.csv")

#16. Left fingers/thumb
N03_JS.ltfingthumb <- svyby(formula = ~JMTHP16,
                            by = ~AGE_P + SEX,
                            design = NHIS03_DO,
                            FUN = svymean,
                            na.rm = TRUE)
N03_JS.ltfingthumb.c <- N03_JS.ltfingthumb %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltfingthumb")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N03_JS.ltfingthumb.ci <- confint(N03_JS.ltfingthumb) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N03_JS.ltfingthumb.ci <- N03_JS.ltfingthumb.ci[-c(23:44), ]
#join proportion and ci
N03.JS.ltfingthumb <- bind_cols(N03_JS.ltfingthumb.c, N03_JS.ltfingthumb.ci)
#save
#write.csv(N03.JS.ltfingthumb, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N03.ltfingthumb.csv")

#17. Another
N03_JS.another <- svyby(formula = ~JMTHP17,
                        by = ~AGE_P + SEX,
                        design = NHIS03_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N03_JS.another.c <- N03_JS.another %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.another")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N03_JS.another.ci <- confint(N03_JS.another) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N03_JS.another.ci <- N03_JS.another.ci[-c(23:44), ]
#join proportion and ci
N03.JS.another <- bind_cols(N03_JS.another.c, N03_JS.another.ci)
#save
#write.csv(N03.JS.another, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N03.another.csv")

N03.JSloc <- Reduce(function(x, y) merge(x, y, all = TRUE), list(N03_JS.rtshoulder.c, N03_JS.ltshoulder.c, N03_JS.rtelbow.c, N03_JS.ltelbow.c, N03_JS.rthip.c, N03_JS.lthip.c, N03_JS.rtwrist.c, N03_JS.ltwrist.c, N03_JS.rtknee.c, N03_JS.ltknee.c, N03_JS.rtankle.c, N03_JS.ltankle.c, N03_JS.rttoes.c, N03_JS.lttoes.c, N03_JS.rtfingthumb.c, N03_JS.ltfingthumb.c, N03_JS.another.c))

#Wide to long
N03.JSlocation <- gather(N03.JSloc, Location, Proportion, Proportion.RtShoulder:Proportion.another, factor_key = TRUE)
N03.JSlocation <- N03.JSlocation %>% 
  unite("Demographic", Age:Sex)
N03.JSlocation$Location <- as.character(N03.JSlocation$Location)
N03.JSlocation$Location[N03.JSlocation$Location == "Proportion.RtShoulder"] <- "Right shoulder"
N03.JSlocation$Location[N03.JSlocation$Location == "Proportion.ltshoulder"] <- "Left shoulder"
N03.JSlocation$Location[N03.JSlocation$Location == "Proportion.rtelbow"] <- "Right elbow"
N03.JSlocation$Location[N03.JSlocation$Location == "Proportion.ltelbow"] <- "Left elbow"
N03.JSlocation$Location[N03.JSlocation$Location == "Proportion.rthip"] <- "Right hip"
N03.JSlocation$Location[N03.JSlocation$Location == "Proportion.lthip"] <- "Left hip"
N03.JSlocation$Location[N03.JSlocation$Location == "Proportion.rtwrist"] <- "Right wrist"
N03.JSlocation$Location[N03.JSlocation$Location == "Proportion.ltwrist"] <- "Left wrist"
N03.JSlocation$Location[N03.JSlocation$Location == "Proportion.rtknee"] <- "Right knee"
N03.JSlocation$Location[N03.JSlocation$Location == "Proportion.ltknee"] <- "Left knee"
N03.JSlocation$Location[N03.JSlocation$Location == "Proportion.rtankle"] <- "Right ankle"
N03.JSlocation$Location[N03.JSlocation$Location == "Proportion.ltankle"] <- "Left ankle"
N03.JSlocation$Location[N03.JSlocation$Location == "Proportion.rttoes"] <- "Right toes"
N03.JSlocation$Location[N03.JSlocation$Location == "Proportion.lttoes"] <- "Left toes"
N03.JSlocation$Location[N03.JSlocation$Location == "Proportion.rtfingthumb"] <- "Right fingers or thumb"
N03.JSlocation$Location[N03.JSlocation$Location == "Proportion.ltfingthumb"] <- "Left fingers or thumb"
N03.JSlocation$Location[N03.JSlocation$Location == "Proportion.another"] <- "Another joint"

#Heatmap of joint locations by demographic groups
N03.JSlocmap <- ggplot(N03.JSlocation,
                       aes(x = Demographic, y = Location, fill = Proportion)) +
  geom_tile() + 
  scale_fill_viridis(option = "viridis", direction = -1, limits = c(0, 60), breaks = c(10, 20, 30, 40, 50, 60), discrete = FALSE) +
  theme_bw() + 
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.text.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  xlab("Age-sex profile") +
  theme(legend.position = "right",
        strip.background = element_blank()) +
  theme(legend.title = element_text(size = 26)) +
  theme(legend.text = element_text(size = 22)) +
  labs(fill = "Prevalence (%)") +
  guides(fill = guide_colourbar(barwidth = 3, barheight = 25)) +
  scale_x_discrete(labels = c("Females, 18 to 24", "Males, 18 to 24", "Females, 25 to 29", "Males, 25 to 29", "Females, 30 to 34", "Males, 30 to 34", "Females, 35 to 39", "Males, 35 to 39", "Females, 40 to 44", "Males, 40 to 44", "Females, 45 to 49", "Males, 45 to 49", "Females, 50 to 54", "Males, 50 to 54", "Females, 55 to 59", "Males, 55 to 59", "Females, 60 to 64", "Males, 60 to 64", "Females, 65 to 69", "Males, 65 to 69", "Females, 70 and above", "Males, 70 and above")) +
  scale_y_discrete(limits = c("Another joint", "Right toes", "Left toes", "Right ankle", "Left ankle", "Right knee", "Left knee", "Right hip", "Left hip", "Right shoulder", "Left shoulder", "Right elbow", "Left elbow", "Right wrist", "Left wrist", "Right fingers or thumb", "Left fingers or thumb"))
ggsave("N03.age-sex.png", width = 20, height = 13)



#III) Socioeconomic trends (employment status only)

#i)   Prevalence by employment status
N03_JS_emp <- svyby(formula = ~JNTSYMP,
                    by = ~ALL_SA,
                    design = NHIS03_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N03_JS_emp.c <- N03_JS_emp %>%
  select(1, 3, 5) %>%
  setNames(c("Employment", "Proportion", "SE")) %>%
  mutate(Employment = as.factor(Employment)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N03_JS_emp_ci <- confint(N03_JS_emp) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for js = 0
N03_JS_emp_ci <- N03_JS_emp_ci[-c(1:2), ]
#join ci and proportion
N03.JS.emp <- bind_cols(N03_JS_emp.c, N03_JS_emp_ci) #final proportion, se & 95% ci
#save
#write.csv(N03.JS.emp, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N03.JS.emp.csv")

#ii)  Logistic regression by employment status
N03_JS.emp_glm <- svyglm(JNTSYMP~relevel(ALL_SA, ref = "Working for pay at a job/business") + AGE_P + SEX,
                         family = quasibinomial,
                         design = NHIS03_DO)
summary(N03_JS.emp_glm)
exp(cbind(OR=coef(N03_JS.emp_glm), confint(N03_JS.emp_glm)))

#4. Lifestyle trends

#I)  BMI
#logistic regression
N03.BMI.glm <- svyglm(JNTSYMP ~ BMI + AGE_P + SEX,
                      family = quasibinomial,
                      design = NHIS03_DO)
summary(N03.BMI.glm)
exp(cbind(OR=coef(N03.BMI.glm), confint(N03.BMI.glm)))

#II) Physical activity
#logistic regression
N03.PA.JS.glm <- svyglm(JNTSYMP ~ relevel(factor(PAC, ordered = FALSE), ref = "Physically active") + AGE_P + SEX,
                        family = quasibinomial,
                        design = NHIS03_DO)
summary(N03.PA.JS.glm)
exp(cbind(OR = coef(N03.PA.JS.glm), confint(N03.PA.JS.glm)))

#____________________________________________________________________________________________________________________

# == 2002 == #

#1. Overall prevalence
N02_JS <- svymean(~factor(JNTSYMP), 
                  NHIS02_DO, 
                  na.rm = TRUE)

N02_JS.c <- N02_JS %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N02_JS_ci <- confint(N02_JS) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
N02.JS <- bind_cols(N02_JS.c, N02_JS_ci)
#remove js = 0
N02.JS <- N02.JS[-c(1), ] #final proportion, se & 95%
#save
#write.csv(N02.JS, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N02.JS.csv")

#2. Spatial analysis, by region

#I)   Overall population

#i)   regional prevalence for the overall population
N02_JS_reg <- svyby(formula = ~JNTSYMP,
                    by = ~REGION,
                    design = NHIS02_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N02_JS_reg.c <- N02_JS_reg %>%
  select(1, 3, 5) %>%
  setNames(c("Region", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N02_JS_reg.ci <- confint(N02_JS_reg) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for js = 0
N02_JS_reg.ci <- N02_JS_reg.ci[-c(1:4), ]
#join proportion and ci
N02.JS.region <- bind_cols(N02_JS_reg.c, N02_JS_reg.ci) #final proportion, se & 95% ci
#save
#write.csv(N02.JS.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N02.JS.region.csv")

#Create overall regional prevalence

#Change the region column to "NAME" to match sf
names(N02.JS.region)[names(N02.JS.region) == "Region"] <- "NAME"

N02.JS.joined <- regions %>%
  left_join(N02.JS.region)

N02.JS.joined$NAME <- as.factor(N02.JS.joined$NAME)

#Plot
JSregplot <- ggplot(data = N02.JS.joined) +
  geom_sf(aes(fill = Proportion), colour = "white", size = 0.2) +
  xlim(125, 60) +
  ylim(25, 55) +
  theme_void() +
  scale_fill_viridis_c(name = "Prevalence (%)", option = "viridis", direction = -1, limits = c(25, 40), breaks = c(25, 30, 35, 40)) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  ggtitle("Regional prevalence (%) of US residents with joint symptoms in 2002") +
  theme(plot.title = element_text(size = 10, hjust = 0.5))
ggsave("N02.JSregchor.png", width = 5, height = 5)


#ii)  logistic regression by region
N02.JS.reg.glm <- svyglm(relevel(JNTSYMP, ref = "0")~ REGION + AGE_P,
                         family = quasibinomial,
                         design = NHIS02_DO)
summary(N02.JS.reg.glm)
exp(cbind(OR=coef(N02.JS.reg.glm), confint(N02.JS.reg.glm)))

#II)  prevalence by region, by age

#i)  age-specific regional prevalance
N02_JS_reg.age <- svyby(formula = ~JNTSYMP,
                        by = ~REGION + AGE_P,
                        design = NHIS02_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N02_JS_reg.age.c <- N02_JS_reg.age %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Age", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Age = as.character(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N02_JS_reg.age.ci <- confint(N02_JS_reg.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for js = 0
N02_JS_reg.age.ci <- N02_JS_reg.age.ci[-c(1:44), ]
#join proportion and ci
N02.JS.age.region <- bind_cols(N02_JS_reg.age.c, N02_JS_reg.age.ci) #final proportion, se & 95% ci
#save
#write.csv(N02.JS.age.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N02.JS.region.age.csv")

#Geofacet plot - Age & Region
JS.Age_Reg <- ggplot(N02.JS.age.region, aes(Age, Proportion)) +
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
  ggtitle("Regional prevalence (%) of US residents with joint symptoms by age group in 2002") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N02.GF_AgeReg.JS.png", width = 14, height = 9)


#ii) age-specific regional logistic regressions
#18 to 24
sub1824 <- subset(NHIS02_DO, AGE_P == "18 to 24")
N02.JS.age1824.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub1824)
summary(N02.JS.age1824.reg)
exp(cbind(OR = coef(N02.JS.age1824.reg), confint(N02.JS.age1824.reg)))

#25 to 29
sub2529 <- subset(NHIS02_DO, AGE_P == "25 to 29")
N02.JS.age2529.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub2529)
summary(N02.JS.age2529.reg)
exp(cbind(OR = coef(N02.JS.age2529.reg), confint(N02.JS.age2529.reg)))

#30 to 34
sub3034 <- subset(NHIS02_DO, AGE_P == "30 to 34")
N02.JS.age3034.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub3034)
summary(N02.JS.age3034.reg)
exp(cbind(OR = coef(N02.JS.age3034.reg), confint(N02.JS.age3034.reg)))

#35 to 39
sub3539 <- subset(NHIS02_DO, AGE_P == "35 to 39")
N02.JS.age3539.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub3539)
summary(N02.JS.age3539.reg)
exp(cbind(OR = coef(N02.JS.age3539.reg), confint(N02.JS.age3539.reg)))

#40 to 44
sub4044 <- subset(NHIS02_DO, AGE_P == "40 to 44")
N02.JS.age4044.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub4044)
summary(N02.JS.age4044.reg)
exp(cbind(OR = coef(N02.JS.age4044.reg), confint(N02.JS.age4044.reg)))

#45 to 49
sub4549 <- subset(NHIS02_DO, AGE_P == "45 to 49")
N02.JS.age4549.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub4549)
summary(N02.JS.age4549.reg)
exp(cbind(OR = coef(N02.JS.age4549.reg), confint(N02.JS.age4549.reg)))

#50 to 54
sub5054 <- subset(NHIS02_DO, AGE_P =="50 to 54")
N02.JS.age5054.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub5054)
summary(N02.JS.age5054.reg)
exp(cbind(OR = coef(N02.JS.age5054.reg), confint(N02.JS.age5054.reg)))

#55 to 59
sub5559 <- subset(NHIS02_DO, AGE_P == "55 to 59")
N02.JS.age5559.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub5559)
summary(N02.JS.age5559.reg)
exp(cbind(OR = coef(N02.JS.age5559.reg), confint(N02.JS.age5559.reg)))

#60 to 64
sub6064 <- subset(NHIS02_DO, AGE_P == "60 to 64")
N02.JS.age6064.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub6064)
summary(N02.JS.age6064.reg)
exp(cbind(OR = coef(N02.JS.age6064.reg), confint(N02.JS.age6064.reg)))

#65 to 69
sub6569 <- subset(NHIS02_DO, AGE_P == "65 to 69")
N02.JS.age6569.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                             family = quasibinomial,
                             design = sub6569)
summary(N02.JS.age6569.reg)
exp(cbind(OR = coef(N02.JS.age6569.reg), confint(N02.JS.age6569.reg)))

#70 and above
sub70 <- subset(NHIS02_DO, AGE_P == "70 and above")
N02.JS.age70.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + SEX,
                           family = quasibinomial,
                           design = sub70)
summary(N02.JS.age70.reg)
exp(cbind(OR = coef(N02.JS.age70.reg), confint(N02.JS.age70.reg)))

#III) prevalence by region, by sex

#i)  sex-specific regional prevalence
N02_JS_reg.sex <- svyby(formula = ~JNTSYMP,
                        by = ~REGION + SEX,
                        design = NHIS02_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N02_JS_reg.sex.c <- N02_JS_reg.sex %>%
  select(1, 2, 4, 6) %>%
  setNames(c("Region", "Sex", "Proportion", "SE")) %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N02_JS_reg.sex.ci <- confint(N02_JS_reg.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for js = 0
N02_JS_reg.sex.ci <- N02_JS_reg.sex.ci[-c(1:8), ]
#join proportion and ci
N02.JS.sex.region <- bind_cols(N02_JS_reg.sex.c, N02_JS_reg.sex.ci) #final proprtion, se & 95% ci
#save
#write.csv(N02.JS.sex.region, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N02.JS.region.sex.csv")

#Geofacet - Sex & Region
JS.Sex_Reg02 <- ggplot(N02.JS.sex.region, aes(Sex, Proportion)) +
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
  ggtitle("Regional prevalence (%) of US residents with joint symptoms by sex in 2002") +
  theme(plot.title = element_text(size = 22, hjust = 0))
ggsave("N02.GF_SexReg.JS.png", width = 14, height = 9)


#ii) sex-specific regional logistic regressions
#males
submales <- subset(NHIS02_DO, SEX == "Male")
N02.JS.sexmale.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + AGE_P,
                             family = quasibinomial,
                             design = submales)
summary(N02.JS.sexmale.reg)
exp(cbind(OR = coef(N02.JS.sexmale.reg), confint(N02.JS.sexmale.reg)))

#females
subfemales <- subset(NHIS02_DO, SEX == "Female")
N02.JS.sexfemale.reg <- svyglm(relevel(JNTSYMP, ref = "0")~REGION + AGE_P,
                               family = quasibinomial,
                               design = subfemales)
summary(N02.JS.sexfemale.reg)
exp(cbind(OR = coef(N02.JS.sexfemale.reg), confint(N02.JS.sexfemale.reg)))


#3. Demographic trends

#I)  Age trends

#i)  Prevalence by age
N02_JS_age <- svyby(formula = ~JNTSYMP,
                    by = ~AGE_P,
                    design = NHIS02_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N02_JS_age.c <- N02_JS_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N02_JS_age_ci <- confint(N02_JS_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove js=0 ci
N02_JS_age_ci <- N02_JS_age_ci[-c(1:11), ]
#join proportion and ci
N02_JS.age <- bind_cols(N02_JS_age.c, N02_JS_age_ci) #final proportion, se & 95% ci
#Save
#write.csv(N02_JS.age, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N02.JS.age.csv")


#ii) Logistic regression by age
N02_JS.age_glm <- svyglm(JNTSYMP~AGE_P + SEX,
                         family = quasibinomial,
                         design = NHIS02_DO)
summary(N02_JS.age_glm)
exp(cbind(OR=coef(N02_JS.age_glm), confint(N02_JS.age_glm)))



#II)  Sex trends

#i)  Prevalence by sex
N02_JS_sex <- svyby(formula = ~JNTSYMP,
                    by = ~SEX,
                    design = NHIS02_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N02_JS_sex.c <- N02_JS_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N02_JS_sex_ci <- confint(N02_JS_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N02_JS_sex_ci <- N02_JS_sex_ci[-c(1:2), ]
#join proportion and ci
N02.JS.sex <- bind_cols(N02_JS_sex.c, N02_JS_sex_ci)
#save
#write.csv(N02.JS.sex, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N02.JS.sex.csv")


#ii) Logistic regression by sex
N02.JS.sex.glm.c <- svyglm(JNTSYMP~relevel(SEX, ref = "Male") + AGE_P,
                           family = quasibinomial,
                           design = NHIS02_DO)
summary(N02.JS.sex.glm.c)
exp(cbind(OR=coef(N02.JS.sex.glm.c), confint(N02.JS.sex.glm.c)))


#Prevalence of various age-sex groups and their joint pain locations

#1. Right shoulder
N02_JS.rtshoulder <- svyby(formula = ~JMTHP1,
                           by = ~AGE_P + SEX,
                           design = NHIS02_DO,
                           FUN = svymean,
                           na.rm = TRUE)
N02_JS.rtshoulder.c <- N02_JS.rtshoulder %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.RtShoulder")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N02_JS.rtshoulder.ci <- confint(N02_JS.rtshoulder) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N02_JS.rtshoulder.ci <- N02_JS.rtshoulder.ci[-c(23:44), ]
#join proportion and ci
N02.JS.rtshoulder <- bind_cols(N02_JS.rtshoulder.c, N02_JS.rtshoulder.ci)
#save
#write.csv(N02.JS.rtshoulder, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N02.rtshoulder.csv")

#2. Left Shoulder
N02_JS.ltshoulder <- svyby(formula = ~JMTHP2,
                           by = ~AGE_P + SEX,
                           design = NHIS02_DO,
                           FUN = svymean,
                           na.rm = TRUE)
N02_JS.ltshoulder.c <- N02_JS.ltshoulder %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltshoulder")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N02_JS.ltshoulder.ci <- confint(N02_JS.ltshoulder) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N02_JS.ltshoulder.ci <- N02_JS.ltshoulder.ci[-c(23:44), ]
#join proportion and ci
N02.JS.ltshoulder <- bind_cols(N02_JS.ltshoulder.c, N02_JS.ltshoulder.ci)
#save
#write.csv(N02.JS.ltshoulder, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N02.ltshoulder.csv")


#3. Right elbow
N02_JS.rtelbow <- svyby(formula = ~JMTHP3,
                        by = ~AGE_P + SEX,
                        design = NHIS02_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N02_JS.rtelbow.c <- N02_JS.rtelbow %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtelbow")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N02_JS.rtelbow.ci <- confint(N02_JS.rtelbow) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N02_JS.rtelbow.ci <- N02_JS.rtelbow.ci[-c(23:44), ]
#join proportion and ci
N02.JS.rtelbow <- bind_cols(N02_JS.rtelbow.c, N02_JS.rtelbow.ci)
#save
#write.csv(N02.JS.rtelbow, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N02.rtelbow.csv")

#4. Lt elbow
N02_JS.ltelbow <- svyby(formula = ~JMTHP4,
                        by = ~AGE_P + SEX,
                        design = NHIS02_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N02_JS.ltelbow.c <- N02_JS.ltelbow %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltelbow")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N02_JS.ltelbow.ci <- confint(N02_JS.ltelbow) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N02_JS.ltelbow.ci <- N02_JS.ltelbow.ci[-c(23:44), ]
#join proportion and ci
N02.JS.ltelbow <- bind_cols(N02_JS.ltelbow.c, N02_JS.ltelbow.ci)
#save
#write.csv(N02.JS.ltelbow, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N02.ltelbow.csv")

#5. Right hip
N02_JS.rthip <- svyby(formula = ~JMTHP5,
                      by = ~AGE_P + SEX,
                      design = NHIS02_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N02_JS.rthip.c <- N02_JS.rthip %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rthip")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N02_JS.rthip.ci <- confint(N02_JS.rthip) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N02_JS.rthip.ci <- N02_JS.rthip.ci[-c(23:44), ]
#join proportion and ci
N02.JS.rthip <- bind_cols(N02_JS.rthip.c, N02_JS.rthip.ci)
#save
#write.csv(N02.JS.rthip, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N02.rthip.csv")

#6. Left hip
N02_JS.lthip <- svyby(formula = ~JMTHP6,
                      by = ~AGE_P + SEX,
                      design = NHIS02_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N02_JS.lthip.c <- N02_JS.lthip %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.lthip")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N02_JS.lthip.ci <- confint(N02_JS.lthip) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N02_JS.lthip.ci <- N02_JS.lthip.ci[-c(23:44), ]
#join proportion and ci
N02.JS.lthip <- bind_cols(N02_JS.lthip.c, N02_JS.lthip.ci)
#save
#write.csv(N02.JS.lthip, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N02.lthip.csv")

#7. Right wrist
N02_JS.rtwrist <- svyby(formula = ~JMTHP7,
                        by = ~AGE_P + SEX,
                        design = NHIS02_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N02_JS.rtwrist.c <- N02_JS.rtwrist %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtwrist")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N02_JS.rtwrist.ci <- confint(N02_JS.rtwrist) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N02_JS.rtwrist.ci <- N02_JS.rtwrist.ci[-c(23:44), ]
#join proportion and ci
N02.JS.rtwrist <- bind_cols(N02_JS.rtwrist.c, N02_JS.rtwrist.ci)
#save
#write.csv(N02.JS.rtwrist, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N02.rtwrist.csv")

#8. Left wrist
N02_JS.ltwrist <- svyby(formula = ~JMTHP8,
                        by = ~AGE_P + SEX,
                        design = NHIS02_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N02_JS.ltwrist.c <- N02_JS.ltwrist %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltwrist")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N02_JS.ltwrist.ci <- confint(N02_JS.ltwrist) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N02_JS.ltwrist.ci <- N02_JS.ltwrist.ci[-c(23:44), ]
#join proportion and ci
N02.JS.ltwrist <- bind_cols(N02_JS.ltwrist.c, N02_JS.ltwrist.ci)
#save
#write.csv(N02.JS.ltwrist, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N02.ltwrist.csv")

#9. Right knee
N02_JS.rtknee <- svyby(formula = ~JMTHP9,
                       by = ~AGE_P + SEX,
                       design = NHIS02_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N02_JS.rtknee.c <- N02_JS.rtknee %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtknee")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N02_JS.rtknee.ci <- confint(N02_JS.rtknee) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N02_JS.rtknee.ci <- N02_JS.rtknee.ci[-c(23:44), ]
#join proportion and ci
N02.JS.rtknee <- bind_cols(N02_JS.rtknee.c, N02_JS.rtknee.ci)
#save
#write.csv(N02.JS.rtknee, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N02.rtknee.csv")

#10. Left knee
N02_JS.ltknee <- svyby(formula = ~JMTHP10,
                       by = ~AGE_P + SEX,
                       design = NHIS02_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N02_JS.ltknee.c <- N02_JS.ltknee %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltknee")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N02_JS.ltknee.ci <- confint(N02_JS.ltknee) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N02_JS.ltknee.ci <- N02_JS.ltknee.ci[-c(23:44), ]
#join proportion and ci
N02.JS.ltknee <- bind_cols(N02_JS.ltknee.c, N02_JS.ltknee.ci)
#save
#write.csv(N02.JS.ltknee, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N02.ltknee.csv")

#11. Right ankle
N02_JS.rtankle <- svyby(formula = ~JMTHP11,
                        by = ~AGE_P + SEX,
                        design = NHIS02_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N02_JS.rtankle.c <- N02_JS.rtankle %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtankle")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N02_JS.rtankle.ci <- confint(N02_JS.rtankle) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N02_JS.rtankle.ci <- N02_JS.rtankle.ci[-c(23:44), ]
#join proportion and ci
N02.JS.rtankle <- bind_cols(N02_JS.rtankle.c, N02_JS.rtankle.ci)
#save
#write.csv(N02.JS.rtankle, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N02.rtankle.csv")

#12. Left ankle
N02_JS.ltankle <- svyby(formula = ~JMTHP12,
                        by = ~AGE_P + SEX,
                        design = NHIS02_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N02_JS.ltankle.c <- N02_JS.ltankle %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltankle")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N02_JS.ltankle.ci <- confint(N02_JS.ltankle) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N02_JS.ltankle.ci <- N02_JS.ltankle.ci[-c(23:44), ]
#join proportion and ci
N02.JS.ltankle <- bind_cols(N02_JS.ltankle.c, N02_JS.ltankle.ci)
#save
#write.csv(N02.JS.ltankle, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N02.ltankle.csv")

#13. Right toes
N02_JS.rttoes <- svyby(formula = ~JMTHP13,
                       by = ~AGE_P + SEX,
                       design = NHIS02_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N02_JS.rttoes.c <- N02_JS.rttoes %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rttoes")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N02_JS.rttoes.ci <- confint(N02_JS.rttoes) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N02_JS.rttoes.ci <- N02_JS.rttoes.ci[-c(23:44), ]
#join proportion and ci
N02.JS.rttoes <- bind_cols(N02_JS.rttoes.c, N02_JS.rttoes.ci)
#save
#write.csv(N02.JS.rttoes, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N02.rttoes.csv")

#14. Left toes
N02_JS.lttoes <- svyby(formula = ~JMTHP14,
                       by = ~AGE_P + SEX,
                       design = NHIS02_DO,
                       FUN = svymean,
                       na.rm = TRUE)
N02_JS.lttoes.c <- N02_JS.lttoes %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.lttoes")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N02_JS.lttoes.ci <- confint(N02_JS.lttoes) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N02_JS.lttoes.ci <- N02_JS.lttoes.ci[-c(23:44), ]
#join proportion and ci
N02.JS.lttoes <- bind_cols(N02_JS.lttoes.c, N02_JS.lttoes.ci)
#save
#write.csv(N02.JS.lttoes, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N02.lttoes.csv")

#15. Right fingers/thumb
N02_JS.rtfingthumb <- svyby(formula = ~JMTHP15,
                            by = ~AGE_P + SEX,
                            design = NHIS02_DO,
                            FUN = svymean,
                            na.rm = TRUE)
N02_JS.rtfingthumb.c <- N02_JS.rtfingthumb %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.rtfingthumb")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N02_JS.rtfingthumb.ci <- confint(N02_JS.rtfingthumb) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N02_JS.rtfingthumb.ci <- N02_JS.rtfingthumb.ci[-c(23:44), ]
#join proportion and ci
N02.JS.rtfingthumb <- bind_cols(N02_JS.rtfingthumb.c, N02_JS.rtfingthumb.ci)
#save
#write.csv(N02.JS.rtfingthumb, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N02.rtfingthumb.csv")

#16. Left fingers/thumb
N02_JS.ltfingthumb <- svyby(formula = ~JMTHP16,
                            by = ~AGE_P + SEX,
                            design = NHIS02_DO,
                            FUN = svymean,
                            na.rm = TRUE)
N02_JS.ltfingthumb.c <- N02_JS.ltfingthumb %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.ltfingthumb")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N02_JS.ltfingthumb.ci <- confint(N02_JS.ltfingthumb) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N02_JS.ltfingthumb.ci <- N02_JS.ltfingthumb.ci[-c(23:44), ]
#join proportion and ci
N02.JS.ltfingthumb <- bind_cols(N02_JS.ltfingthumb.c, N02_JS.ltfingthumb.ci)
#save
#write.csv(N02.JS.ltfingthumb, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N02.ltfingthumb.csv")

#17. Another
N02_JS.another <- svyby(formula = ~JMTHP17,
                        by = ~AGE_P + SEX,
                        design = NHIS02_DO,
                        FUN = svymean,
                        na.rm = TRUE)
N02_JS.another.c <- N02_JS.another %>%
  select(1:3) %>%
  setNames(c("Age", "Sex", "Proportion.another")) %>%
  mutate(Age = as.character(Age)) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N02_JS.another.ci <- confint(N02_JS.another) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
N02_JS.another.ci <- N02_JS.another.ci[-c(23:44), ]
#join proportion and ci
N02.JS.another <- bind_cols(N02_JS.another.c, N02_JS.another.ci)
#save
#write.csv(N02.JS.another, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N02.another.csv")

N02.JSloc <- Reduce(function(x, y) merge(x, y, all = TRUE), list(N02_JS.rtshoulder.c, N02_JS.ltshoulder.c, N02_JS.rtelbow.c, N02_JS.ltelbow.c, N02_JS.rthip.c, N02_JS.lthip.c, N02_JS.rtwrist.c, N02_JS.ltwrist.c, N02_JS.rtknee.c, N02_JS.ltknee.c, N02_JS.rtankle.c, N02_JS.ltankle.c, N02_JS.rttoes.c, N02_JS.lttoes.c, N02_JS.rtfingthumb.c, N02_JS.ltfingthumb.c, N02_JS.another.c))

#Wide to long
N02.JSlocation <- gather(N02.JSloc, Location, Proportion, Proportion.RtShoulder:Proportion.another, factor_key = TRUE)
N02.JSlocation <- N02.JSlocation %>% 
  unite("Demographic", Age:Sex)
N02.JSlocation$Location <- as.character(N02.JSlocation$Location)
N02.JSlocation$Location[N02.JSlocation$Location == "Proportion.RtShoulder"] <- "Right shoulder"
N02.JSlocation$Location[N02.JSlocation$Location == "Proportion.ltshoulder"] <- "Left shoulder"
N02.JSlocation$Location[N02.JSlocation$Location == "Proportion.rtelbow"] <- "Right elbow"
N02.JSlocation$Location[N02.JSlocation$Location == "Proportion.ltelbow"] <- "Left elbow"
N02.JSlocation$Location[N02.JSlocation$Location == "Proportion.rthip"] <- "Right hip"
N02.JSlocation$Location[N02.JSlocation$Location == "Proportion.lthip"] <- "Left hip"
N02.JSlocation$Location[N02.JSlocation$Location == "Proportion.rtwrist"] <- "Right wrist"
N02.JSlocation$Location[N02.JSlocation$Location == "Proportion.ltwrist"] <- "Left wrist"
N02.JSlocation$Location[N02.JSlocation$Location == "Proportion.rtknee"] <- "Right knee"
N02.JSlocation$Location[N02.JSlocation$Location == "Proportion.ltknee"] <- "Left knee"
N02.JSlocation$Location[N02.JSlocation$Location == "Proportion.rtankle"] <- "Right ankle"
N02.JSlocation$Location[N02.JSlocation$Location == "Proportion.ltankle"] <- "Left ankle"
N02.JSlocation$Location[N02.JSlocation$Location == "Proportion.rttoes"] <- "Right toes"
N02.JSlocation$Location[N02.JSlocation$Location == "Proportion.lttoes"] <- "Left toes"
N02.JSlocation$Location[N02.JSlocation$Location == "Proportion.rtfingthumb"] <- "Right fingers or thumb"
N02.JSlocation$Location[N02.JSlocation$Location == "Proportion.ltfingthumb"] <- "Left fingers or thumb"
N02.JSlocation$Location[N02.JSlocation$Location == "Proportion.another"] <- "Another joint"

#Heatmap of joint locations by demographic groups
N02.JSlocmap <- ggplot(N02.JSlocation,
                       aes(x = Demographic, y = Location, fill = Proportion)) +
  geom_tile() + 
  scale_fill_viridis(option = "viridis", direction = -1, limits = c(0, 60), breaks = c(10, 20, 30, 40, 50, 60), discrete = FALSE) +
  theme_bw() + 
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.text.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  xlab("Age-sex profile") +
  theme(legend.position = "right",
        strip.background = element_blank()) +
  theme(legend.title = element_text(size = 26)) +
  theme(legend.text = element_text(size = 22)) +
  labs(fill = "Prevalence (%)") +
  guides(fill = guide_colourbar(barwidth = 3, barheight = 25)) +
  scale_x_discrete(labels = c("Females, 18 to 24", "Males, 18 to 24", "Females, 25 to 29", "Males, 25 to 29", "Females, 30 to 34", "Males, 30 to 34", "Females, 35 to 39", "Males, 35 to 39", "Females, 40 to 44", "Males, 40 to 44", "Females, 45 to 49", "Males, 45 to 49", "Females, 50 to 54", "Males, 50 to 54", "Females, 55 to 59", "Males, 55 to 59", "Females, 60 to 64", "Males, 60 to 64", "Females, 65 to 69", "Males, 65 to 69", "Females, 70 and above", "Males, 70 and above")) +
  scale_y_discrete(limits = c("Another joint", "Right toes", "Left toes", "Right ankle", "Left ankle", "Right knee", "Left knee", "Right hip", "Left hip", "Right shoulder", "Left shoulder", "Right elbow", "Left elbow", "Right wrist", "Left wrist", "Right fingers or thumb", "Left fingers or thumb"))
ggsave("N02.age-sex.png", width = 20, height = 13)


#III) Socioeconomic trends (employment status only)

#i)   Prevalence by employment status
N02_JS_emp <- svyby(formula = ~JNTSYMP,
                    by = ~ALL_SA,
                    design = NHIS02_DO,
                    FUN = svymean,
                    na.rm = TRUE)
N02_JS_emp.c <- N02_JS_emp %>%
  select(1, 3, 5) %>%
  setNames(c("Employment", "Proportion", "SE")) %>%
  mutate(Employment = as.factor(Employment)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N02_JS_emp_ci <- confint(N02_JS_emp) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for js = 0
N02_JS_emp_ci <- N02_JS_emp_ci[-c(1:2), ]
#join ci and proportion
N02.JS.emp <- bind_cols(N02_JS_emp.c, N02_JS_emp_ci) #final proportion, se & 95% ci
#save
#write.csv(N02.JS.emp, "C:\\Users\\Anne\\Desktop\\Backup csv's\\NHIS\\N02.JS.emp.csv")

#ii)  Logistic regression by employment status
N02_JS.emp_glm <- svyglm(JNTSYMP~relevel(ALL_SA, ref = "Working for pay at a job/business") + AGE_P + SEX,
                         family = quasibinomial,
                         design = NHIS02_DO)
summary(N02_JS.emp_glm)
exp(cbind(OR=coef(N02_JS.emp_glm), confint(N02_JS.emp_glm)))

#4. Lifestyle trends

#I)  BMI
#logistic regression
N02.BMI.glm <- svyglm(JNTSYMP ~ BMI + AGE_P + SEX,
                      family = quasibinomial,
                      design = NHIS02_DO)
summary(N02.BMI.glm)
exp(cbind(OR=coef(N02.BMI.glm), confint(N02.BMI.glm)))

#II) Physical activity
#logistic regression
N02.PA.JS.glm <- svyglm(JNTSYMP ~ relevel(factor(PAC, ordered = FALSE), ref = "Physically active") + AGE_P + SEX,
                        family = quasibinomial,
                        design = NHIS02_DO)
summary(N02.PA.JS.glm)
exp(cbind(OR = coef(N02.PA.JS.glm), confint(N02.PA.JS.glm)))

#______________________________________________________________________________________________________________________

#NHIS FIGURES

#make quick function for every nth year shown on x-axis
every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}


#1. Temporal figures for Chapter 3:

#Overall population annual prevalence

#Add years
N18.JS$Year <- c("2018")
N17.JS$Year <- c("2017")
N16.JS$Year <- c("2016")
N15.JS$Year <- c("2015")
N14.JS$Year <- c("2014")
N13.JS$Year <- c("2013")
N12.JS$Year <- c("2012")
N11.JS$Year <- c("2011")
N10.JS$Year <- c("2010")
N09.JS$Year <- c("2009")
N08.JS$Year <- c("2008")
N07.JS$Year <- c("2007")
N06.JS$Year <- c("2006")
N05.JS$Year <- c("2005")
N03.JS$Year <- c("2003")
N02.JS$Year <- c("2002")

N.JS <- Reduce(function(x, y) merge(x, y, all=TRUE), list(N02.JS, N03.JS, N05.JS, N06.JS, N07.JS, N08.JS, N09.JS, N10.JS, N11.JS, N12.JS, N13.JS, N14.JS, N15.JS, N16.JS, N17.JS, N18.JS))

#Overall temporall plot
N.JS.plot <- ggplot(data = N.JS,
                    aes(x = Year, y = Proportion, group = 1)) +
  geom_line(colour = "#6600CC") +
  geom_point(colour = "#6600CC") + 
  geom_ribbon(aes(ymin = N.JS$CI_Prop_low, ymax = N.JS$CI_Prop_upp), alpha = 0.2, fill = "#6600CC") +
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
ggsave("N.JS-time.png", width = 18, height = 11)


#Annual prevalence by age

#Add years
N18_JS.age$Year <- c("2018", "2018", "2018", "2018", "2018", "2018", "2018", "2018", "2018", "2018", "2018")
N17_JS.age$Year <- c("2017", "2017", "2017", "2017", "2017", "2017", "2017", "2017", "2017", "2017", "2017")
N16_JS.age$Year <- c("2016", "2016", "2016", "2016", "2016", "2016", "2016", "2016", "2016", "2016", "2016")
N15_JS.age$Year <- c("2015", "2015", "2015", "2015", "2015", "2015", "2015", "2015", "2015", "2015", "2015")
N14_JS.age$Year <- c("2014", "2014", "2014", "2014", "2014", "2014", "2014", "2014", "2014", "2014", "2014")
N13_JS.age$Year <- c("2013", "2013", "2013", "2013", "2013", "2013", "2013", "2013", "2013", "2013", "2013")
N12_JS.age$Year <- c("2012", "2012", "2012", "2012", "2012", "2012", "2012", "2012", "2012", "2012", "2012")
N11_JS.age$Year <- c("2011", "2011", "2011", "2011", "2011", "2011", "2011", "2011", "2011", "2011", "2011")
N10_JS.age$Year <- c("2010", "2010", "2010", "2010", "2010", "2010", "2010", "2010", "2010", "2010", "2010")
N09_JS.age$Year <- c("2009", "2009", "2009", "2009", "2009", "2009", "2009", "2009", "2009", "2009", "2009")
N08_JS.age$Year <- c("2008", "2008", "2008", "2008", "2008", "2008", "2008", "2008", "2008", "2008", "2008")
N07_JS.age$Year <- c("2007", "2007", "2007", "2007", "2007", "2007", "2007", "2007", "2007", "2007", "2007")
N06_JS.age$Year <- c("2006", "2006", "2006", "2006", "2006", "2006", "2006", "2006", "2006", "2006", "2006")
N05_JS.age$Year <- c("2005", "2005", "2005", "2005", "2005", "2005", "2005", "2005", "2005", "2005", "2005")
N03_JS.age$Year <- c("2003", "2003", "2003", "2003", "2003", "2003", "2003", "2003", "2003", "2003", "2003")
N02_JS.age$Year <- c("2002", "2002", "2002", "2002", "2002", "2002", "2002", "2002", "2002", "2002", "2002")

#Merge
N.JS.age <- Reduce(function(x, y) merge(x, y, all=TRUE), list(N18_JS.age, N17_JS.age, N16_JS.age, N15_JS.age, N14_JS.age, N13_JS.age, N12_JS.age, N11_JS.age, N10_JS.age, N09_JS.age, N08_JS.age, N07_JS.age, N06_JS.age, N05_JS.age, N03_JS.age, N02_JS.age))

#Age temporal plot
N.JS.age.plot <- ggplot(data = N.JS.age,
                        aes(x = Year, y = Proportion, group = Age)) +
  geom_line(colour = "#6600CC") +
  geom_point(colour = "#6600CC") +
  geom_ribbon(aes(ymin = N.JS.age$CI_Prop_low, ymax = N.JS.age$CI_Prop_upp), alpha = 0.2, fill = "#6600CC") +
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
ggsave("N.JS.time.age.png", width = 18, height = 11)


#Annual prevalence by sex

#Add years
N18.JS.sex$Year <- c("2018", "2018")
N17.JS.sex$Year <- c("2017", "2017")
N16.JS.sex$Year <- c("2016", "2016")
N15.JS.sex$Year <- c("2015", "2015")
N14.JS.sex$Year <- c("2014", "2014")
N13.JS.sex$Year <- c("2013", "2013")
N12.JS.sex$Year <- c("2012", "2012")
N11.JS.sex$Year <- c("2011", "2011")
N10.JS.sex$Year <- c("2010", "2010")
N09.JS.sex$Year <- c("2009", "2009")
N08.JS.sex$Year <- c("2008", "2008")
N07.JS.sex$Year <- c("2007", "2007")
N06.JS.sex$Year <- c("2006", "2006")
N05.JS.sex$Year <- c("2005", "2005")
N03.JS.sex$Year <- c("2003", "2003")
N02.JS.sex$Year <- c("2002", "2002")

#Merge
N.JS.sex <- Reduce(function(x, y) merge(x, y, all=TRUE), list(N18.JS.sex, N17.JS.sex, N16.JS.sex, N15.JS.sex, N14.JS.sex, N13.JS.sex, N12.JS.sex, N10.JS.sex, N09.JS.sex, N08.JS.sex, N07.JS.sex, N06.JS.sex, N05.JS.sex, N03.JS.sex, N02.JS.sex))


#Sex temporal plot
N.JS.sex.plot <- ggplot(data = N.JS.sex,
                        aes(x = Year, y = Proportion, group = Sex)) +
  geom_line(colour = "#6600CC") +
  geom_point(colour = "#6600CC") +
  geom_ribbon(aes(ymin = N.JS.sex$CI_Prop_low, ymax = N.JS.sex$CI_Prop_upp), alpha = 0.2, fill = "#6600CC") +
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
ggsave("N.JS.time.sex.png", width = 18, height = 11)


#Annual prevalence by employment

#Add years
N18.JS.emp$Year <- c("2018", "2018")
N17.JS.emp$Year <- c("2017", "2017")
N16.JS.emp$Year <- c("2016", "2016")
N15.JS.emp$Year <- c("2015", "2015")
N14.JS.emp$Year <- c("2014", "2014")
N13.JS.emp$Year <- c("2013", "2013")
N12.JS.emp$Year <- c("2012", "2012")
N11.JS.emp$Year <- c("2011", "2011")
N10.JS.emp$Year <- c("2010", "2010")
N09.JS.emp$Year <- c("2009", "2009")
N08.JS.emp$Year <- c("2008", "2008")
N07.JS.emp$Year <- c("2007", "2007")
N06.JS.emp$Year <- c("2006", "2006")
N05.JS.emp$Year <- c("2005", "2005")
N03.JS.emp$Year <- c("2003", "2003")
N02.JS.emp$Year <- c("2002", "2002")

#Merge
N.JS.emp <- Reduce(function(x, y) merge(x, y, all=TRUE), list(N18.JS.emp, N17.JS.emp, N16.JS.emp, N15.JS.emp, N14.JS.emp, N13.JS.emp, N12.JS.emp, N10.JS.emp, N09.JS.emp, N08.JS.emp, N07.JS.emp, N06.JS.emp, N05.JS.emp, N03.JS.emp, N02.JS.emp))


#emp temporal plot
N.JS.emp.plot <- ggplot(data = N.JS.emp,
                        aes(x = Year, y = Proportion, group = Employment)) +
  geom_line(colour = "#6600CC") +
  geom_point(colour = "#6600CC") +
  geom_ribbon(aes(ymin = N.JS.emp$CI_Prop_low, ymax = N.JS.emp$CI_Prop_upp), alpha = 0.2, fill = "#6600CC") +
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
ggsave("N.JS.time.emp.png", width = 18, height = 11)


#2. Demographic figures for Chapter 5:

#Age-specific trends
N_JS.age_plot <- ggplot(data = N.JS.age,
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
ggsave("N.JS.demage.png", width = 18, height = 12)


#Sex-specific trends

N_JS.sex_plot <- ggplot(data = N.JS.sex,
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
ggsave("N.JS.demsex.png", width = 18, height = 11)

#Chapter 4 (spatial trends) images are done annually within analyses.
#No figures for chapter 6 (lifestyle trends)

# ------- END NHIS JOINT SYMPTOMS ANALYSIS HERE ------- #
