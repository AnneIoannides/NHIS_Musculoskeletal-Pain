#Anne Elizabeth Ioannides
#For the qualification of: PhD - Physiology, University of the Witwatersrand, South Africa

#National Health Interview Survey (NHIS) - Downloading data, cleaning data, and creating design objects (no analysis in this script)

#These design ojects are available in the NHIS Github repo. The same design objects were used to analyse both joint symptoms and low back pain. 

#load packages
library(haven)
library(foreign)
library(SAScii)
library(tidyverse)
library(survey)
library(gdata)


# == 2018 == #

#DOWNLOAD

tempn18 <- tempfile()
download.file("ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/2018/samadultcsv.zip",
              tempn18)
NHIS18_SA <- read.csv(unz(tempn18, "samadult.csv"))
unlink(tempn18)
NHIS18_SA
#Save as RDS file
saveRDS(NHIS18_SA,
        file = "NHIS18_SA.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

#CLEANING

str(NHIS18_SA)
tail(NHIS18_SA)
glimpse(NHIS18_SA)
colnames(NHIS18_SA)


#Select variables
NHIS18 <- select(NHIS18_SA,
                 HHX, FMX, FPX,
                 REGION,
                 PSTRAT, PPSU, SRVY_YR, WTFA_SA,
                 SEX, AGE_P,
                 DOINGLWA,
                 JNTSYMP, JMTHP1, JMTHP2, JMTHP3, JMTHP4, JMTHP5, JMTHP6, JMTHP7, JMTHP8, JMTHP9, JMTHP10, JMTHP11, JMTHP12, JMTHP13, JMTHP14, JMTHP15, JMTHP16, JMTHP17,
                 PAINLB,
                 VIGFREQW, VIGMIN, MODFREQW, MODMIN,
                 BMI,
                 HYPEV, COPDEV)

str(NHIS18)
tail(NHIS18)
glimpse(NHIS18)
colnames(NHIS18)

#joint symptoms recode
table(NHIS18$JNTSYMP)
#recode
NHIS18$JNTSYMP <- recode(NHIS18$JNTSYMP,
                         "1" = "1",
                         "2" = "0",
                         "7" = "7",
                         "9" = "9")
(NHIS18$JNTSYMP <- unknownToNA(NHIS18$JNTSYMP, unknown = c("7", "9")))
table(NHIS18$JNTSYMP)
NHIS18$JNTSYMP <- as.factor(NHIS18$JNTSYMP)

#low back pain recode
table(NHIS18$PAINLB)
#recode
NHIS18$PAINLB <- recode(NHIS18$PAINLB,
                        "1" = "1",
                        "2" = "0")
table(NHIS18$PAINLB)
NHIS18$PAINLB <- as.factor(NHIS18$PAINLB)

#region
table(NHIS18$REGION)
NHIS18$REGION <- recode(NHIS18$REGION,
                        "1" = "Northeast",
                        "2" = "Midwest",
                        "3" = "South",
                        "4" = "West")
table(NHIS18$REGION)

#age
table(NHIS18$AGE_P)
NHIS18$AGE_P <- recode(NHIS18$AGE_P,
                       "18" = "18 to 24",
                       "19" = "18 to 24",
                       "20" = "18 to 24",
                       "21" = "18 to 24",
                       "22" = "18 to 24",
                       "23" = "18 to 24",
                       "24" = "18 to 24",
                       "25" = "25 to 29",
                       "26" = "25 to 29",
                       "27" = "25 to 29",
                       "28" = "25 to 29",
                       "29" = "25 to 29",
                       "30" = "30 to 34",
                       "31" = "30 to 34",
                       "32" = "30 to 34",
                       "33" = "30 to 34",
                       "34" = "30 to 34",
                       "35" = "35 to 39",
                       "36" = "35 to 39",
                       "37" = "35 to 39",
                       "38" = "35 to 39", 
                       "39" = "35 to 39",
                       "40" = "40 to 44",
                       "41" = "40 to 44",
                       "42" = "40 to 44",
                       "43" = "40 to 44",
                       "44" = "40 to 44",
                       "45" = "45 to 49",
                       "46" = "45 to 49",
                       "47" = "45 to 49",
                       "48" = "45 to 49",
                       "49" = "45 to 49",
                       "50" = "50 to 54",
                       "51" = "50 to 54",
                       "52" = "50 to 54",
                       "53" = "50 to 54",
                       "54" = "50 to 54",
                       "55" = "55 to 59",
                       "56" = "55 to 59",
                       "57" = "55 to 59",
                       "58" = "55 to 59",
                       "59" = "55 to 59",
                       "60" = "60 to 64",
                       "61" = "60 to 64",
                       "62" = "60 to 64",
                       "63" = "60 to 64",
                       "64" = "60 to 64",
                       "65" = "65 to 69",
                       "66" = "65 to 69",
                       "67" = "65 to 69",
                       "68" = "65 to 69",
                       "69" = "65 to 69",
                       "70" = "70 and above",
                       "71" = "70 and above",
                       "72" = "70 and above",
                       "73" = "70 and above",
                       "74" = "70 and above",
                       "75" = "70 and above",
                       "76" = "70 and above",
                       "77" = "70 and above",
                       "78" = "70 and above",
                       "79" = "70 and above",
                       "80" = "70 and above",
                       "81" = "70 and above",
                       "82" = "70 and above",
                       "83" = "70 and above",
                       "84" = "70 and above",
                       "85" = "70 and above")
table(NHIS18$AGE_P)
NHIS18$AGE_P <- as.factor(NHIS18$AGE_P)

#sex
NHIS18$SEX <- recode(NHIS18$SEX,
                     "1" = "Male",
                     "2" = "Female")
table(NHIS18$SEX)
NHIS18$SEX <- as.factor(NHIS18$SEX)

#employment status
NHIS18$DOINGLWA <- recode(NHIS18$DOINGLWA,
                          "1" = "Working for pay at a job/business",
                          "2" = "2",
                          "3" = "3",
                          "4" = "4",
                          "5" = "Not working")
#Change unused values to NA
(NHIS18$DOINGLWA <- unknownToNA(NHIS18$DOINGLWA, unknown = c("9", "2", "3", "4")))
table(NHIS18$DOINGLWA)
NHIS18$DOINGLWA <- as.factor(NHIS18$DOINGLWA)

#affected joing areas quick clean
NHIS18$JMTHP1 <- unknownToNA(NHIS18$JMTHP1, unknown = 7:9)
NHIS18$JMTHP1 <- as.factor(NHIS18$JMTHP1)

NHIS18$JMTHP2 <- unknownToNA(NHIS18$JMTHP2, unknown = 7:9)
NHIS18$JMTHP2 <- as.factor(NHIS18$JMTHP2)

NHIS18$JMTHP3 <- unknownToNA(NHIS18$JMTHP3, unknown = 7:9)
NHIS18$JMTHP3 <- as.factor(NHIS18$JMTHP3)

NHIS18$JMTHP4 <- unknownToNA(NHIS18$JMTHP4, unknown = 7:9)
NHIS18$JMTHP4 <- as.factor(NHIS18$JMTHP4)

NHIS18$JMTHP5 <- unknownToNA(NHIS18$JMTHP5, unknown = 7:9)
NHIS18$JMTHP5 <- as.factor(NHIS18$JMTHP5)

NHIS18$JMTHP6 <- unknownToNA(NHIS18$JMTHP6, unknown = 7:9)
NHIS18$JMTHP6 <- as.factor(NHIS18$JMTHP6)

NHIS18$JMTHP7 <- unknownToNA(NHIS18$JMTHP7, unknown = 7:9)
NHIS18$JMTHP7 <- as.factor(NHIS18$JMTHP7)

NHIS18$JMTHP8 <- unknownToNA(NHIS18$JMTHP8, unknown = 7:9)
NHIS18$JMTHP8 <- as.factor(NHIS18$JMTHP8)

NHIS18$JMTHP9 <- unknownToNA(NHIS18$JMTHP9, unknown = 7:9)
NHIS18$JMTHP9 <- as.factor(NHIS18$JMTHP9)

NHIS18$JMTHP10 <- unknownToNA(NHIS18$JMTHP10, unknown = 7:9)
NHIS18$JMTHP10 <- as.factor(NHIS18$JMTHP10)

NHIS18$JMTHP11 <- unknownToNA(NHIS18$JMTHP11, unknown = 7:9)
NHIS18$JMTHP11 <- as.factor(NHIS18$JMTHP11)

NHIS18$JMTHP12 <- unknownToNA(NHIS18$JMTHP12, unknown = 7:9)
NHIS18$JMTHP12 <- as.factor(NHIS18$JMTHP12)

NHIS18$JMTHP13 <- unknownToNA(NHIS18$JMTHP13, unknown = 7:9)
NHIS18$JMTHP13 <- as.factor(NHIS18$JMTHP13)

NHIS18$JMTHP14 <- unknownToNA(NHIS18$JMTHP14, unknown = 7:9)
NHIS18$JMTHP14 <- as.factor(NHIS18$JMTHP14)

NHIS18$JMTHP15 <- unknownToNA(NHIS18$JMTHP15, unknown = 7:9)
NHIS18$JMTHP15 <- as.factor(NHIS18$JMTHP15)

NHIS18$JMTHP16 <- unknownToNA(NHIS18$JMTHP16, unknown = 7:9)
NHIS18$JMTHP16 <- as.factor(NHIS18$JMTHP16)

NHIS18$JMTHP17 <- unknownToNA(NHIS18$JMTHP17, unknown = 7:9)
NHIS18$JMTHP17 <- as.factor(NHIS18$JMTHP17)

#physical activity
#vigorous PA frequency
table(NHIS18$VIGFREQW)
NHIS18$VIGFREQW <- as.numeric(NHIS18$VIGFREQW)

#vigorous PA duration
table(NHIS18$VIGMIN)
NHIS18$VIGMIN <- as.numeric(NHIS18$VIGMIN)

#moderate pa frequency
table(NHIS18$MODFREQW)
NHIS18$MODFREQW <- as.numeric(NHIS18$MODFREQW)

#moderate PA duration
table(NHIS18$MODMIN)
NHIS18$MODMIN <- as.numeric(NHIS18$MODMIN)


#Make PA
#Physicially active = light/moderate pa >=5x/wk & >=30min OR Vig >=3x/wk & >=20min, else not
NHIS18$PAC <- ifelse(NHIS18$MODFREQW >= 5 & NHIS18$MODFREQW <= 28 & NHIS18$MODMIN >= 30 & NHIS18$MODMIN < 997, "Physically active",
                     ifelse(NHIS18$VIGFREQW >= 3 & NHIS18$VIGFREQW <= 28 & NHIS18$VIGMIN >= 20 & NHIS18$VIGMIN < 997, "Physically active", "Not physically active"))

table(NHIS18$PAC)

#BMI
NHIS18$BMI
#implied 2 decimal places, therefore divide all values by 100 to get the BMI value
NHIS18 <- NHIS18 %>%
  mutate(BMI = BMI/100)
#change all 99.99 (unknown values) to NA
NHIS18$BMI <- unknownToNA(NHIS18$BMI, unknown = 99.99)
#quick check to be sure that there are no more potential unknowns
NHIS18 %>% top_n(10, BMI)

#copd
NHIS18$COPDEV <- recode(NHIS18$COPDEV,
                        "1" = "1",
                        "2" = "0")
table(NHIS18$COPDEV)
NHIS18$COPDEV <- as.factor(NHIS18$COPDEV)

#ht
NHIS18$HYPEV <- recode(NHIS18$HYPEV,
                       "1" = "1",
                       "2" = "0")
table(NHIS18$HYPEV)
NHIS18$HYPEV <- as.factor(NHIS18$HYPEV)

#________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#for establishing unweighted sample size only, not for design object creation
NHIS18_JS <- subset(NHIS18,
                    !is.na(WTFA_SA) &
                      !is.na(PSTRAT) &
                      !is.na(PPSU) &
                      !is.na(JNTSYMP))

table(subset(NHIS18_JS, AGE_P == "18 to 24")$SEX)
table(subset(NHIS18_JS, AGE_P == "25 to 29")$SEX)
table(subset(NHIS18_JS, AGE_P == "30 to 34")$SEX)
table(subset(NHIS18_JS, AGE_P == "35 to 39")$SEX)
table(subset(NHIS18_JS, AGE_P == "40 to 44")$SEX)
table(subset(NHIS18_JS, AGE_P == "45 to 49")$SEX)
table(subset(NHIS18_JS, AGE_P == "50 to 54")$SEX)
table(subset(NHIS18_JS, AGE_P == "55 to 59")$SEX)
table(subset(NHIS18_JS, AGE_P == "60 to 64")$SEX)
table(subset(NHIS18_JS, AGE_P == "65 to 69")$SEX)
table(subset(NHIS18_JS, AGE_P == "70 and above")$SEX)


#for establishing unweighted sample size only, not for design object creation
NHIS18_LBP <- subset(NHIS18,
                     !is.na(WTFA_SA) &
                       !is.na(PSTRAT) &
                       !is.na(PPSU) &
                       !is.na(PAINLB))

table(subset(NHIS18_LBP, AGE_P == "18 to 24")$SEX)
table(subset(NHIS18_LBP, AGE_P == "25 to 29")$SEX)
table(subset(NHIS18_LBP, AGE_P == "30 to 34")$SEX)
table(subset(NHIS18_LBP, AGE_P == "35 to 39")$SEX)
table(subset(NHIS18_LBP, AGE_P == "40 to 44")$SEX)
table(subset(NHIS18_LBP, AGE_P == "45 to 49")$SEX)
table(subset(NHIS18_LBP, AGE_P == "50 to 54")$SEX)
table(subset(NHIS18_LBP, AGE_P == "55 to 59")$SEX)
table(subset(NHIS18_LBP, AGE_P == "60 to 64")$SEX)
table(subset(NHIS18_LBP, AGE_P == "65 to 69")$SEX)
table(subset(NHIS18_LBP, AGE_P == "70 and above")$SEX)


#make sure all complex survey requirements are available for design object
NHIS18_SA_dataset <- subset(NHIS18,
                            !is.na(WTFA_SA) &
                              !is.na(PSTRAT) &
                              !is.na(PPSU))

#Check that the sum of the weights is equal to the US population
sum(NHIS18_SA_dataset$WTFA_SA)
#The sum of the weights is 249 455 533, which is acceptable

#Check the number of people (unique PSU's)
length(unique(NHIS18_SA_dataset[["PPSU"]]))
#The number of unique PSU's in the data is 100

#Check the number of unique strata
length(unique(NHIS18_SA_dataset[["PSTRAT"]]))
#The number of unique strata is 52

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHIS18_DO <- svydesign(ids = ~1,
                       weights = ~WTFA_SA,
                       strata = ~PSTRAT,
                       nest = TRUE,
                       data = NHIS18_SA_dataset)

#Save design object as RDS file for ease of retrieval for analysis, thereby avoiding running the data download and the design object creation code multiple times
saveRDS(NHIS18_DO,
        file = "NHIS18_DO.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

#________________________________________________________________________________________________________
#________________________________________________________________________________________________________
#________________________________________________________________________________________________________


# == 2017 == #

#DOWNLOADS

tempn17 <- tempfile()
download.file("ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/2017/samadultcsv.zip",
              tempn17)
NHIS17_SA <- read.csv(unz(tempn17, "samadult.csv"))
NHIS17_SA
#Save as RDS file
saveRDS(NHIS17_SA,
        file = "NHIS17_SA.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

#CLEANING

#Some exploration
str(NHIS17_SA)
tail(NHIS17_SA)
glimpse(NHIS17_SA)
colnames(NHIS17_SA)

#Select variables
NHIS17 <- select(NHIS17_SA,
                 HHX, FMX, FPX,
                 REGION,
                 PSTRAT, PPSU, SRVY_YR, WTFA_SA,
                 SEX, AGE_P,
                 DOINGLWA,
                 JNTSYMP, JMTHP1, JMTHP2, JMTHP3, JMTHP4, JMTHP5, JMTHP6, JMTHP7, JMTHP8, JMTHP9, JMTHP10, JMTHP11, JMTHP12, JMTHP13, JMTHP14, JMTHP15, JMTHP16, JMTHP17,
                 PAINLB,
                 VIGFREQW, VIGMIN, MODFREQW, MODMIN,
                 BMI,
                 HYPEV, COPDEV)

str(NHIS17)
tail(NHIS17)
glimpse(NHIS17)
colnames(NHIS17)

#joint symptoms recode
table(NHIS17$JNTSYMP)
#recode
NHIS17$JNTSYMP <- recode(NHIS17$JNTSYMP,
                         "1" = "1",
                         "2" = "0",
                         "7" = "7",
                         "9" = "9")
(NHIS17$JNTSYMP <- unknownToNA(NHIS17$JNTSYMP, unknown = c("7", "9")))
table(NHIS17$JNTSYMP)
NHIS17$JNTSYMP <- as.factor(NHIS17$JNTSYMP)

#low back pain recode
table(NHIS17$PAINLB)
#recode
NHIS17$PAINLB <- recode(NHIS17$PAINLB,
                        "1" = "1",
                        "2" = "0")
table(NHIS17$PAINLB)
NHIS17$PAINLB <- as.factor(NHIS17$PAINLB)

#region
table(NHIS17$REGION)
NHIS17$REGION <- recode(NHIS17$REGION,
                        "1" = "Northeast",
                        "2" = "Midwest",
                        "3" = "South",
                        "4" = "West")
table(NHIS17$REGION)

#age
table(NHIS17$AGE_P)



NHIS17$AGE_P <- recode(NHIS17$AGE_P,
                       "18" = "18 to 24",
                       "19" = "18 to 24",
                       "20" = "18 to 24",
                       "21" = "18 to 24",
                       "22" = "18 to 24",
                       "23" = "18 to 24",
                       "24" = "18 to 24",
                       "25" = "25 to 29",
                       "26" = "25 to 29",
                       "27" = "25 to 29",
                       "28" = "25 to 29",
                       "29" = "25 to 29",
                       "30" = "30 to 34",
                       "31" = "30 to 34",
                       "32" = "30 to 34",
                       "33" = "30 to 34",
                       "34" = "30 to 34",
                       "35" = "35 to 39",
                       "36" = "35 to 39",
                       "37" = "35 to 39",
                       "38" = "35 to 39", 
                       "39" = "35 to 39",
                       "40" = "40 to 44",
                       "41" = "40 to 44",
                       "42" = "40 to 44",
                       "43" = "40 to 44",
                       "44" = "40 to 44",
                       "45" = "45 to 49",
                       "46" = "45 to 49",
                       "47" = "45 to 49",
                       "48" = "45 to 49",
                       "49" = "45 to 49",
                       "50" = "50 to 54",
                       "51" = "50 to 54",
                       "52" = "50 to 54",
                       "53" = "50 to 54",
                       "54" = "50 to 54",
                       "55" = "55 to 59",
                       "56" = "55 to 59",
                       "57" = "55 to 59",
                       "58" = "55 to 59",
                       "59" = "55 to 59",
                       "60" = "60 to 64",
                       "61" = "60 to 64",
                       "62" = "60 to 64",
                       "63" = "60 to 64",
                       "64" = "60 to 64",
                       "65" = "65 to 69",
                       "66" = "65 to 69",
                       "67" = "65 to 69",
                       "68" = "65 to 69",
                       "69" = "65 to 69",
                       "70" = "70 and above",
                       "71" = "70 and above",
                       "72" = "70 and above",
                       "73" = "70 and above",
                       "74" = "70 and above",
                       "75" = "70 and above",
                       "76" = "70 and above",
                       "77" = "70 and above",
                       "78" = "70 and above",
                       "79" = "70 and above",
                       "80" = "70 and above",
                       "81" = "70 and above",
                       "82" = "70 and above",
                       "83" = "70 and above",
                       "84" = "70 and above",
                       "85" = "70 and above")
table(NHIS17$AGE_P)
NHIS17$AGE_P <- as.factor(NHIS17$AGE_P)

#sex
NHIS17$SEX <- recode(NHIS17$SEX,
                     "1" = "Male",
                     "2" = "Female")
table(NHIS17$SEX)
NHIS17$SEX <- as.factor(NHIS17$SEX)

#employment status
NHIS17$DOINGLWA <- recode(NHIS17$DOINGLWA,
                          "1" = "Working for pay at a job/business",
                          "2" = "2",
                          "3" = "3",
                          "4" = "4",
                          "5" = "Not working")
#Change unused values to NA
(NHIS17$DOINGLWA <- unknownToNA(NHIS17$DOINGLWA, unknown = c("9", "2", "3", "4")))
table(NHIS17$DOINGLWA)
NHIS17$DOINGLWA <- as.factor(NHIS17$DOINGLWA)

#affected joing areas quick clean
NHIS17$JMTHP1 <- unknownToNA(NHIS17$JMTHP1, unknown = 7:9)
NHIS17$JMTHP1 <- as.factor(NHIS17$JMTHP1)

NHIS17$JMTHP2 <- unknownToNA(NHIS17$JMTHP2, unknown = 7:9)
NHIS17$JMTHP2 <- as.factor(NHIS17$JMTHP2)

NHIS17$JMTHP3 <- unknownToNA(NHIS17$JMTHP3, unknown = 7:9)
NHIS17$JMTHP3 <- as.factor(NHIS17$JMTHP3)

NHIS17$JMTHP4 <- unknownToNA(NHIS17$JMTHP4, unknown = 7:9)
NHIS17$JMTHP4 <- as.factor(NHIS17$JMTHP4)

NHIS17$JMTHP5 <- unknownToNA(NHIS17$JMTHP5, unknown = 7:9)
NHIS17$JMTHP5 <- as.factor(NHIS17$JMTHP5)

NHIS17$JMTHP6 <- unknownToNA(NHIS17$JMTHP6, unknown = 7:9)
NHIS17$JMTHP6 <- as.factor(NHIS17$JMTHP6)

NHIS17$JMTHP7 <- unknownToNA(NHIS17$JMTHP7, unknown = 7:9)
NHIS17$JMTHP7 <- as.factor(NHIS17$JMTHP7)

NHIS17$JMTHP8 <- unknownToNA(NHIS17$JMTHP8, unknown = 7:9)
NHIS17$JMTHP8 <- as.factor(NHIS17$JMTHP8)

NHIS17$JMTHP9 <- unknownToNA(NHIS17$JMTHP9, unknown = 7:9)
NHIS17$JMTHP9 <- as.factor(NHIS17$JMTHP9)

NHIS17$JMTHP10 <- unknownToNA(NHIS17$JMTHP10, unknown = 7:9)
NHIS17$JMTHP10 <- as.factor(NHIS17$JMTHP10)

NHIS17$JMTHP11 <- unknownToNA(NHIS17$JMTHP11, unknown = 7:9)
NHIS17$JMTHP11 <- as.factor(NHIS17$JMTHP11)

NHIS17$JMTHP12 <- unknownToNA(NHIS17$JMTHP12, unknown = 7:9)
NHIS17$JMTHP12 <- as.factor(NHIS17$JMTHP12)

NHIS17$JMTHP13 <- unknownToNA(NHIS17$JMTHP13, unknown = 7:9)
NHIS17$JMTHP13 <- as.factor(NHIS17$JMTHP13)

NHIS17$JMTHP14 <- unknownToNA(NHIS17$JMTHP14, unknown = 7:9)
NHIS17$JMTHP14 <- as.factor(NHIS17$JMTHP14)

NHIS17$JMTHP15 <- unknownToNA(NHIS17$JMTHP15, unknown = 7:9)
NHIS17$JMTHP15 <- as.factor(NHIS17$JMTHP15)

NHIS17$JMTHP16 <- unknownToNA(NHIS17$JMTHP16, unknown = 7:9)
NHIS17$JMTHP16 <- as.factor(NHIS17$JMTHP16)

NHIS17$JMTHP17 <- unknownToNA(NHIS17$JMTHP17, unknown = 7:9)
NHIS17$JMTHP17 <- as.factor(NHIS17$JMTHP17)

#physical activity
#vigorous PA frequency
table(NHIS17$VIGFREQW)
NHIS17$VIGFREQW <- as.numeric(NHIS17$VIGFREQW)

#vigorous PA duration
table(NHIS17$VIGMIN)
NHIS17$VIGMIN <- as.numeric(NHIS17$VIGMIN)

#moderate pa frequency
table(NHIS17$MODFREQW)
NHIS17$MODFREQW <- as.numeric(NHIS17$MODFREQW)

#moderate PA duration
table(NHIS17$MODMIN)
NHIS17$MODMIN <- as.numeric(NHIS17$MODMIN)


#Make PA
#Physicially active = light/moderate pa >=5x/wk & >=30min OR Vig >=3x/wk & >=20min, else not
NHIS17$PAC <- ifelse(NHIS17$MODFREQW >= 5 & NHIS17$MODFREQW <= 28 & NHIS17$MODMIN >= 30 & NHIS17$MODMIN < 997, "Physically active",
                     ifelse(NHIS17$VIGFREQW >= 3 & NHIS17$VIGFREQW <= 28 & NHIS17$VIGMIN >= 20 & NHIS17$VIGMIN < 997, "Physically active", "Not physically active"))


#BMI
#implied 2 decimal places, therefore divide all values by 100 to get the BMI value
NHIS17 <- NHIS17 %>%
  mutate(BMI = BMI/100)
#change all 99.99 (unknown values) to NA
NHIS17$BMI <- unknownToNA(NHIS17$BMI, unknown = 99.99)
#quick check to be sure that there are no more potential unknowns
NHIS17 %>% top_n(10, BMI)

#copd
NHIS17$COPDEV <- recode(NHIS17$COPDEV,
                        "1" = "1",
                        "2" = "0")
table(NHIS17$COPDEV)
NHIS17$COPDEV <- as.factor(NHIS17$COPDEV)

#ht
NHIS17$HYPEV <- recode(NHIS17$HYPEV,
                       "1" = "1",
                       "2" = "0")
table(NHIS17$HYPEV)
NHIS17$HYPEV <- as.factor(NHIS17$HYPEV)

#________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")


#for establishing unweighted sample size only, not for design object creation
NHIS17_JS <- subset(NHIS17,
                    !is.na(WTFA_SA) &
                      !is.na(PSTRAT) &
                      !is.na(PPSU) &
                      !is.na(JNTSYMP))

table(subset(NHIS17_JS, AGE_P == "18 to 24")$SEX)
table(subset(NHIS17_JS, AGE_P == "25 to 29")$SEX)
table(subset(NHIS17_JS, AGE_P == "30 to 34")$SEX)
table(subset(NHIS17_JS, AGE_P == "35 to 39")$SEX)
table(subset(NHIS17_JS, AGE_P == "40 to 44")$SEX)
table(subset(NHIS17_JS, AGE_P == "45 to 49")$SEX)
table(subset(NHIS17_JS, AGE_P == "50 to 54")$SEX)
table(subset(NHIS17_JS, AGE_P == "55 to 59")$SEX)
table(subset(NHIS17_JS, AGE_P == "60 to 64")$SEX)
table(subset(NHIS17_JS, AGE_P == "65 to 69")$SEX)
table(subset(NHIS17_JS, AGE_P == "70 and above")$SEX)


#for establishing unweighted sample size only, not for design object creation
NHIS17_LBP <- subset(NHIS17,
                     !is.na(WTFA_SA) &
                       !is.na(PSTRAT) &
                       !is.na(PPSU) &
                       !is.na(PAINLB))

table(subset(NHIS17_LBP, AGE_P == "18 to 24")$SEX)
table(subset(NHIS17_LBP, AGE_P == "25 to 29")$SEX)
table(subset(NHIS17_LBP, AGE_P == "30 to 34")$SEX)
table(subset(NHIS17_LBP, AGE_P == "35 to 39")$SEX)
table(subset(NHIS17_LBP, AGE_P == "40 to 44")$SEX)
table(subset(NHIS17_LBP, AGE_P == "45 to 49")$SEX)
table(subset(NHIS17_LBP, AGE_P == "50 to 54")$SEX)
table(subset(NHIS17_LBP, AGE_P == "55 to 59")$SEX)
table(subset(NHIS17_LBP, AGE_P == "60 to 64")$SEX)
table(subset(NHIS17_LBP, AGE_P == "65 to 69")$SEX)
table(subset(NHIS17_LBP, AGE_P == "70 and above")$SEX)


#make sure all complex survey requirements are available
NHIS17_SA_dataset <- subset(NHIS17,
                            !is.na(WTFA_SA) &
                              !is.na(PSTRAT) &
                              !is.na(PPSU))

#Check that the sum of the weights is equal to the US population
sum(NHIS17_SA_dataset$WTFA_SA)
#The sum of the weights is 246 657 271, which is acceptable

#Check the number of people (unique PSU's)
length(unique(NHIS17_SA_dataset[["PPSU"]]))
#The number of unique PSU's in the data is 100

#Check the number of unique strata
length(unique(NHIS17_SA_dataset[["PSTRAT"]]))
#The number of unique strata is 52

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHIS17_DO <- svydesign(ids = ~1,
                       weights = ~WTFA_SA,
                       strata = ~PSTRAT,
                       nest = TRUE,
                       data = NHIS17_SA_dataset)

#Save design object as RDS file for ease of retrieval for analysis, thereby avoiding running the data download and the design object creation code multiple times
saveRDS(NHIS17_DO,
        file = "NHIS17_DO.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

#________________________________________________________________________________________________________
#________________________________________________________________________________________________________
#________________________________________________________________________________________________________


# == 2016 == #

#DOWNLOADS

tempn16 <- tempfile()
download.file("ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/2016/samadultcsv.zip",
              tempn16)
NHIS16_SA <- read.csv(unz(tempn16, "samadult.csv"))
NHIS16_SA
#Save as RDS file
saveRDS(NHIS16_SA,
        file = "NHIS16_SA.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

#CLEANING

#Some exploration
str(NHIS16_SA)
tail(NHIS16_SA)
glimpse(NHIS16_SA)
colnames(NHIS16_SA)

#Select variables
NHIS16 <- select(NHIS16_SA,
                 HHX, FMX, FPX,
                 REGION,
                 PSTRAT, PPSU, SRVY_YR, WTFA_SA,
                 SEX, AGE_P,
                 DOINGLWA,
                 JNTSYMP, JMTHP1, JMTHP2, JMTHP3, JMTHP4, JMTHP5, JMTHP6, JMTHP7, JMTHP8, JMTHP9, JMTHP10, JMTHP11, JMTHP12, JMTHP13, JMTHP14, JMTHP15, JMTHP16, JMTHP17,
                 PAINLB,
                 VIGFREQW, VIGMIN, MODFREQW, MODMIN,
                 BMI,
                 HYPEV, COPDEV)

str(NHIS16)
tail(NHIS16)
glimpse(NHIS16)
colnames(NHIS16)

#joint symptoms recode
table(NHIS16$JNTSYMP)
#recode
NHIS16$JNTSYMP <- recode(NHIS16$JNTSYMP,
                         "1" = "1",
                         "2" = "0",
                         "7" = "7",
                         "9" = "9")
(NHIS16$JNTSYMP <- unknownToNA(NHIS16$JNTSYMP, unknown = c("7", "9")))
table(NHIS16$JNTSYMP)
NHIS16$JNTSYMP <- as.factor(NHIS16$JNTSYMP)

#low back pain recode
table(NHIS16$PAINLB)
#recode
NHIS16$PAINLB <- recode(NHIS16$PAINLB,
                        "1" = "1",
                        "2" = "0")
table(NHIS16$PAINLB)
NHIS16$PAINLB <- as.factor(NHIS16$PAINLB)

#region
table(NHIS16$REGION)
NHIS16$REGION <- recode(NHIS16$REGION,
                        "1" = "Northeast",
                        "2" = "Midwest",
                        "3" = "South",
                        "4" = "West")
table(NHIS16$REGION)

#age
table(NHIS16$AGE_P)



NHIS16$AGE_P <- recode(NHIS16$AGE_P,
                       "18" = "18 to 24",
                       "19" = "18 to 24",
                       "20" = "18 to 24",
                       "21" = "18 to 24",
                       "22" = "18 to 24",
                       "23" = "18 to 24",
                       "24" = "18 to 24",
                       "25" = "25 to 29",
                       "26" = "25 to 29",
                       "27" = "25 to 29",
                       "28" = "25 to 29",
                       "29" = "25 to 29",
                       "30" = "30 to 34",
                       "31" = "30 to 34",
                       "32" = "30 to 34",
                       "33" = "30 to 34",
                       "34" = "30 to 34",
                       "35" = "35 to 39",
                       "36" = "35 to 39",
                       "37" = "35 to 39",
                       "38" = "35 to 39", 
                       "39" = "35 to 39",
                       "40" = "40 to 44",
                       "41" = "40 to 44",
                       "42" = "40 to 44",
                       "43" = "40 to 44",
                       "44" = "40 to 44",
                       "45" = "45 to 49",
                       "46" = "45 to 49",
                       "47" = "45 to 49",
                       "48" = "45 to 49",
                       "49" = "45 to 49",
                       "50" = "50 to 54",
                       "51" = "50 to 54",
                       "52" = "50 to 54",
                       "53" = "50 to 54",
                       "54" = "50 to 54",
                       "55" = "55 to 59",
                       "56" = "55 to 59",
                       "57" = "55 to 59",
                       "58" = "55 to 59",
                       "59" = "55 to 59",
                       "60" = "60 to 64",
                       "61" = "60 to 64",
                       "62" = "60 to 64",
                       "63" = "60 to 64",
                       "64" = "60 to 64",
                       "65" = "65 to 69",
                       "66" = "65 to 69",
                       "67" = "65 to 69",
                       "68" = "65 to 69",
                       "69" = "65 to 69",
                       "70" = "70 and above",
                       "71" = "70 and above",
                       "72" = "70 and above",
                       "73" = "70 and above",
                       "74" = "70 and above",
                       "75" = "70 and above",
                       "76" = "70 and above",
                       "77" = "70 and above",
                       "78" = "70 and above",
                       "79" = "70 and above",
                       "80" = "70 and above",
                       "81" = "70 and above",
                       "82" = "70 and above",
                       "83" = "70 and above",
                       "84" = "70 and above",
                       "85" = "70 and above")
table(NHIS16$AGE_P)
NHIS16$AGE_P <- as.factor(NHIS16$AGE_P)

#sex
NHIS16$SEX <- recode(NHIS16$SEX,
                     "1" = "Male",
                     "2" = "Female")
table(NHIS16$SEX)
NHIS16$SEX <- as.factor(NHIS16$SEX)

#employment status
NHIS16$DOINGLWA <- recode(NHIS16$DOINGLWA,
                          "1" = "Working for pay at a job/business",
                          "2" = "2",
                          "3" = "3",
                          "4" = "4",
                          "5" = "Not working")
#Change unused values to NA
(NHIS16$DOINGLWA <- unknownToNA(NHIS16$DOINGLWA, unknown = c("9", "2", "3", "4")))
table(NHIS16$DOINGLWA)
NHIS16$DOINGLWA <- as.factor(NHIS16$DOINGLWA)

#affected joing areas quick clean
NHIS16$JMTHP1 <- unknownToNA(NHIS16$JMTHP1, unknown = 7:9)
NHIS16$JMTHP1 <- as.factor(NHIS16$JMTHP1)

NHIS16$JMTHP2 <- unknownToNA(NHIS16$JMTHP2, unknown = 7:9)
NHIS16$JMTHP2 <- as.factor(NHIS16$JMTHP2)

NHIS16$JMTHP3 <- unknownToNA(NHIS16$JMTHP3, unknown = 7:9)
NHIS16$JMTHP3 <- as.factor(NHIS16$JMTHP3)

NHIS16$JMTHP4 <- unknownToNA(NHIS16$JMTHP4, unknown = 7:9)
NHIS16$JMTHP4 <- as.factor(NHIS16$JMTHP4)

NHIS16$JMTHP5 <- unknownToNA(NHIS16$JMTHP5, unknown = 7:9)
NHIS16$JMTHP5 <- as.factor(NHIS16$JMTHP5)

NHIS16$JMTHP6 <- unknownToNA(NHIS16$JMTHP6, unknown = 7:9)
NHIS16$JMTHP6 <- as.factor(NHIS16$JMTHP6)

NHIS16$JMTHP7 <- unknownToNA(NHIS16$JMTHP7, unknown = 7:9)
NHIS16$JMTHP7 <- as.factor(NHIS16$JMTHP7)

NHIS16$JMTHP8 <- unknownToNA(NHIS16$JMTHP8, unknown = 7:9)
NHIS16$JMTHP8 <- as.factor(NHIS16$JMTHP8)

NHIS16$JMTHP9 <- unknownToNA(NHIS16$JMTHP9, unknown = 7:9)
NHIS16$JMTHP9 <- as.factor(NHIS16$JMTHP9)

NHIS16$JMTHP10 <- unknownToNA(NHIS16$JMTHP10, unknown = 7:9)
NHIS16$JMTHP10 <- as.factor(NHIS16$JMTHP10)

NHIS16$JMTHP11 <- unknownToNA(NHIS16$JMTHP11, unknown = 7:9)
NHIS16$JMTHP11 <- as.factor(NHIS16$JMTHP11)

NHIS16$JMTHP12 <- unknownToNA(NHIS16$JMTHP12, unknown = 7:9)
NHIS16$JMTHP12 <- as.factor(NHIS16$JMTHP12)

NHIS16$JMTHP13 <- unknownToNA(NHIS16$JMTHP13, unknown = 7:9)
NHIS16$JMTHP13 <- as.factor(NHIS16$JMTHP13)

NHIS16$JMTHP14 <- unknownToNA(NHIS16$JMTHP14, unknown = 7:9)
NHIS16$JMTHP14 <- as.factor(NHIS16$JMTHP14)

NHIS16$JMTHP15 <- unknownToNA(NHIS16$JMTHP15, unknown = 7:9)
NHIS16$JMTHP15 <- as.factor(NHIS16$JMTHP15)

NHIS16$JMTHP16 <- unknownToNA(NHIS16$JMTHP16, unknown = 7:9)
NHIS16$JMTHP16 <- as.factor(NHIS16$JMTHP16)

NHIS16$JMTHP17 <- unknownToNA(NHIS16$JMTHP17, unknown = 7:9)
NHIS16$JMTHP17 <- as.factor(NHIS16$JMTHP17)

#physical activity
#vigorous PA frequency
table(NHIS16$VIGFREQW)
NHIS16$VIGFREQW <- as.numeric(NHIS16$VIGFREQW)

#vigorous PA duration
table(NHIS16$VIGMIN)
NHIS16$VIGMIN <- as.numeric(NHIS16$VIGMIN)

#moderate pa frequency
table(NHIS16$MODFREQW)
NHIS16$MODFREQW <- as.numeric(NHIS16$MODFREQW)

#moderate PA duration
table(NHIS16$MODMIN)
NHIS16$MODMIN <- as.numeric(NHIS16$MODMIN)


#Make PA
#Physicially active = light/moderate pa >=5x/wk & >=30min OR Vig >=3x/wk & >=20min, else not
NHIS16$PAC <- ifelse(NHIS16$MODFREQW >= 5 & NHIS16$MODFREQW <= 28 & NHIS16$MODMIN >= 30 & NHIS16$MODMIN < 997, "Physically active",
                     ifelse(NHIS16$VIGFREQW >= 3 & NHIS16$VIGFREQW <= 28 & NHIS16$VIGMIN >= 20 & NHIS16$VIGMIN < 997, "Physically active", "Not physically active"))


#BMI
#implied 2 decimal places, therefore divide all values by 100 to get the BMI value
NHIS16 <- NHIS16 %>%
  mutate(BMI = BMI/100)
#change all 99.99 (unknown values) to NA
NHIS16$BMI <- unknownToNA(NHIS16$BMI, unknown = 99.99)
#quick check to be sure that there are no more potential unknowns
NHIS16 %>% top_n(10, BMI)

#copd
NHIS16$COPDEV <- recode(NHIS16$COPDEV,
                        "1" = "1",
                        "2" = "0")
table(NHIS16$COPDEV)
NHIS16$COPDEV <- as.factor(NHIS16$COPDEV)

#ht
NHIS16$HYPEV <- recode(NHIS16$HYPEV,
                       "1" = "1",
                       "2" = "0")
table(NHIS16$HYPEV)
NHIS16$HYPEV <- as.factor(NHIS16$HYPEV)

#________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")


#for establishing unweighted sample size only, not for design object creation
NHIS16_JS <- subset(NHIS16,
                    !is.na(WTFA_SA) &
                      !is.na(PSTRAT) &
                      !is.na(PPSU) &
                      !is.na(JNTSYMP))

table(subset(NHIS16_JS, AGE_P == "18 to 24")$SEX)
table(subset(NHIS16_JS, AGE_P == "25 to 29")$SEX)
table(subset(NHIS16_JS, AGE_P == "30 to 34")$SEX)
table(subset(NHIS16_JS, AGE_P == "35 to 39")$SEX)
table(subset(NHIS16_JS, AGE_P == "40 to 44")$SEX)
table(subset(NHIS16_JS, AGE_P == "45 to 49")$SEX)
table(subset(NHIS16_JS, AGE_P == "50 to 54")$SEX)
table(subset(NHIS16_JS, AGE_P == "55 to 59")$SEX)
table(subset(NHIS16_JS, AGE_P == "60 to 64")$SEX)
table(subset(NHIS16_JS, AGE_P == "65 to 69")$SEX)
table(subset(NHIS16_JS, AGE_P == "70 and above")$SEX)


#for establishing unweighted sample size only, not for design object creation
NHIS16_LBP <- subset(NHIS16,
                     !is.na(WTFA_SA) &
                       !is.na(PSTRAT) &
                       !is.na(PPSU) &
                       !is.na(PAINLB))

table(subset(NHIS16_LBP, AGE_P == "18 to 24")$SEX)
table(subset(NHIS16_LBP, AGE_P == "25 to 29")$SEX)
table(subset(NHIS16_LBP, AGE_P == "30 to 34")$SEX)
table(subset(NHIS16_LBP, AGE_P == "35 to 39")$SEX)
table(subset(NHIS16_LBP, AGE_P == "40 to 44")$SEX)
table(subset(NHIS16_LBP, AGE_P == "45 to 49")$SEX)
table(subset(NHIS16_LBP, AGE_P == "50 to 54")$SEX)
table(subset(NHIS16_LBP, AGE_P == "55 to 59")$SEX)
table(subset(NHIS16_LBP, AGE_P == "60 to 64")$SEX)
table(subset(NHIS16_LBP, AGE_P == "65 to 69")$SEX)
table(subset(NHIS16_LBP, AGE_P == "70 and above")$SEX)


#make sure all complex survey requirements are available
NHIS16_SA_dataset <- subset(NHIS16,
                            !is.na(WTFA_SA) &
                              !is.na(PSTRAT) &
                              !is.na(PPSU))

#Check that the sum of the weights is equal to the US population
sum(NHIS16_SA_dataset$WTFA_SA)
#The sum of the weights is 245 142 225, which is acceptable

#Check the number of people (unique PSU's)
length(unique(NHIS16_SA_dataset[["PPSU"]]))
#The number of unique PSU's in the data is 111

#Check the number of unique strata
length(unique(NHIS16_SA_dataset[["PSTRAT"]]))
#The number of unique strata is 52

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHIS16_DO <- svydesign(ids = ~1,
                       weights = ~WTFA_SA,
                       strata = ~PSTRAT,
                       nest = TRUE,
                       data = NHIS16_SA_dataset)

#Save design object as RDS file for ease of retrieval for analysis, thereby avoiding running the data download and the design object creation code multiple times
saveRDS(NHIS16_DO,
        file = "NHIS16_DO.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

#________________________________________________________________________________________________________
#________________________________________________________________________________________________________
#________________________________________________________________________________________________________


# == 2015 == #

#DOWNLOADS

tempn15 <- tempfile()
download.file("ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/2015/samadult.zip",
              tempn15)
NHIS15_SA <- read.csv(unz(tempn15, "samadult.csv"))
NHIS15_SA
#Save as RDS file
saveRDS(NHIS15_SA,
        file = "NHIS15_SA.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

#CLEANING

#Some exploration
str(NHIS15_SA)
tail(NHIS15_SA)
glimpse(NHIS15_SA)
colnames(NHIS15_SA)

#Select variables
NHIS15 <- select(NHIS15_SA,
                 HHX, FMX, FPX,
                 REGION,
                 STRAT_P, PSU_P, SRVY_YR, WTFA_SA,
                 SEX, AGE_P,
                 DOINGLWA,
                 JNTSYMP, JMTHP1, JMTHP2, JMTHP3, JMTHP4, JMTHP5, JMTHP6, JMTHP7, JMTHP8, JMTHP9, JMTHP10, JMTHP11, JMTHP12, JMTHP13, JMTHP14, JMTHP15, JMTHP16, JMTHP17,
                 PAINLB,
                 VIGFREQW, VIGMIN, MODFREQW, MODMIN,
                 BMI,
                 HYPEV, COPDEV)

str(NHIS15)
tail(NHIS15)
glimpse(NHIS15)
colnames(NHIS15)

#joint symptoms recode
table(NHIS15$JNTSYMP)
#recode
NHIS15$JNTSYMP <- recode(NHIS15$JNTSYMP,
                         "1" = "1",
                         "2" = "0",
                         "7" = "7",
                         "9" = "9")
(NHIS15$JNTSYMP <- unknownToNA(NHIS15$JNTSYMP, unknown = c("7", "9")))
table(NHIS15$JNTSYMP)
NHIS15$JNTSYMP <- as.factor(NHIS15$JNTSYMP)

#low back pain recode
table(NHIS15$PAINLB)
#recode
NHIS15$PAINLB <- recode(NHIS15$PAINLB,
                        "1" = "1",
                        "2" = "0")
table(NHIS15$PAINLB)
NHIS15$PAINLB <- as.factor(NHIS15$PAINLB)

#region
table(NHIS15$REGION)
NHIS15$REGION <- recode(NHIS15$REGION,
                        "1" = "Northeast",
                        "2" = "Midwest",
                        "3" = "South",
                        "4" = "West")
table(NHIS15$REGION)

#age
table(NHIS15$AGE_P)



NHIS15$AGE_P <- recode(NHIS15$AGE_P,
                       "18" = "18 to 24",
                       "19" = "18 to 24",
                       "20" = "18 to 24",
                       "21" = "18 to 24",
                       "22" = "18 to 24",
                       "23" = "18 to 24",
                       "24" = "18 to 24",
                       "25" = "25 to 29",
                       "26" = "25 to 29",
                       "27" = "25 to 29",
                       "28" = "25 to 29",
                       "29" = "25 to 29",
                       "30" = "30 to 34",
                       "31" = "30 to 34",
                       "32" = "30 to 34",
                       "33" = "30 to 34",
                       "34" = "30 to 34",
                       "35" = "35 to 39",
                       "36" = "35 to 39",
                       "37" = "35 to 39",
                       "38" = "35 to 39", 
                       "39" = "35 to 39",
                       "40" = "40 to 44",
                       "41" = "40 to 44",
                       "42" = "40 to 44",
                       "43" = "40 to 44",
                       "44" = "40 to 44",
                       "45" = "45 to 49",
                       "46" = "45 to 49",
                       "47" = "45 to 49",
                       "48" = "45 to 49",
                       "49" = "45 to 49",
                       "50" = "50 to 54",
                       "51" = "50 to 54",
                       "52" = "50 to 54",
                       "53" = "50 to 54",
                       "54" = "50 to 54",
                       "55" = "55 to 59",
                       "56" = "55 to 59",
                       "57" = "55 to 59",
                       "58" = "55 to 59",
                       "59" = "55 to 59",
                       "60" = "60 to 64",
                       "61" = "60 to 64",
                       "62" = "60 to 64",
                       "63" = "60 to 64",
                       "64" = "60 to 64",
                       "65" = "65 to 69",
                       "66" = "65 to 69",
                       "67" = "65 to 69",
                       "68" = "65 to 69",
                       "69" = "65 to 69",
                       "70" = "70 and above",
                       "71" = "70 and above",
                       "72" = "70 and above",
                       "73" = "70 and above",
                       "74" = "70 and above",
                       "75" = "70 and above",
                       "76" = "70 and above",
                       "77" = "70 and above",
                       "78" = "70 and above",
                       "79" = "70 and above",
                       "80" = "70 and above",
                       "81" = "70 and above",
                       "82" = "70 and above",
                       "83" = "70 and above",
                       "84" = "70 and above",
                       "85" = "70 and above")
table(NHIS15$AGE_P)
NHIS15$AGE_P <- as.factor(NHIS15$AGE_P)

#sex
NHIS15$SEX <- recode(NHIS15$SEX,
                     "1" = "Male",
                     "2" = "Female")
table(NHIS15$SEX)
NHIS15$SEX <- as.factor(NHIS15$SEX)

#employment status
NHIS15$DOINGLWA <- recode(NHIS15$DOINGLWA,
                          "1" = "Working for pay at a job/business",
                          "2" = "2",
                          "3" = "3",
                          "4" = "4",
                          "5" = "Not working")
#Change unused values to NA
(NHIS15$DOINGLWA <- unknownToNA(NHIS15$DOINGLWA, unknown = c("9", "2", "3", "4")))
table(NHIS15$DOINGLWA)
NHIS15$DOINGLWA <- as.factor(NHIS15$DOINGLWA)

#affected joing areas quick clean
NHIS15$JMTHP1 <- unknownToNA(NHIS15$JMTHP1, unknown = 7:9)
NHIS15$JMTHP1 <- as.factor(NHIS15$JMTHP1)

NHIS15$JMTHP2 <- unknownToNA(NHIS15$JMTHP2, unknown = 7:9)
NHIS15$JMTHP2 <- as.factor(NHIS15$JMTHP2)

NHIS15$JMTHP3 <- unknownToNA(NHIS15$JMTHP3, unknown = 7:9)
NHIS15$JMTHP3 <- as.factor(NHIS15$JMTHP3)

NHIS15$JMTHP4 <- unknownToNA(NHIS15$JMTHP4, unknown = 7:9)
NHIS15$JMTHP4 <- as.factor(NHIS15$JMTHP4)

NHIS15$JMTHP5 <- unknownToNA(NHIS15$JMTHP5, unknown = 7:9)
NHIS15$JMTHP5 <- as.factor(NHIS15$JMTHP5)

NHIS15$JMTHP6 <- unknownToNA(NHIS15$JMTHP6, unknown = 7:9)
NHIS15$JMTHP6 <- as.factor(NHIS15$JMTHP6)

NHIS15$JMTHP7 <- unknownToNA(NHIS15$JMTHP7, unknown = 7:9)
NHIS15$JMTHP7 <- as.factor(NHIS15$JMTHP7)

NHIS15$JMTHP8 <- unknownToNA(NHIS15$JMTHP8, unknown = 7:9)
NHIS15$JMTHP8 <- as.factor(NHIS15$JMTHP8)

NHIS15$JMTHP9 <- unknownToNA(NHIS15$JMTHP9, unknown = 7:9)
NHIS15$JMTHP9 <- as.factor(NHIS15$JMTHP9)

NHIS15$JMTHP10 <- unknownToNA(NHIS15$JMTHP10, unknown = 7:9)
NHIS15$JMTHP10 <- as.factor(NHIS15$JMTHP10)

NHIS15$JMTHP11 <- unknownToNA(NHIS15$JMTHP11, unknown = 7:9)
NHIS15$JMTHP11 <- as.factor(NHIS15$JMTHP11)

NHIS15$JMTHP12 <- unknownToNA(NHIS15$JMTHP12, unknown = 7:9)
NHIS15$JMTHP12 <- as.factor(NHIS15$JMTHP12)

NHIS15$JMTHP13 <- unknownToNA(NHIS15$JMTHP13, unknown = 7:9)
NHIS15$JMTHP13 <- as.factor(NHIS15$JMTHP13)

NHIS15$JMTHP14 <- unknownToNA(NHIS15$JMTHP14, unknown = 7:9)
NHIS15$JMTHP14 <- as.factor(NHIS15$JMTHP14)

NHIS15$JMTHP15 <- unknownToNA(NHIS15$JMTHP15, unknown = 7:9)
NHIS15$JMTHP15 <- as.factor(NHIS15$JMTHP15)

NHIS15$JMTHP16 <- unknownToNA(NHIS15$JMTHP16, unknown = 7:9)
NHIS15$JMTHP16 <- as.factor(NHIS15$JMTHP16)

NHIS15$JMTHP17 <- unknownToNA(NHIS15$JMTHP17, unknown = 7:9)
NHIS15$JMTHP17 <- as.factor(NHIS15$JMTHP17)

#physical activity
#vigorous PA frequency
table(NHIS15$VIGFREQW)
NHIS15$VIGFREQW <- as.numeric(NHIS15$VIGFREQW)

#vigorous PA duration
table(NHIS15$VIGMIN)
NHIS15$VIGMIN <- as.numeric(NHIS15$VIGMIN)

#moderate pa frequency
table(NHIS15$MODFREQW)
NHIS15$MODFREQW <- as.numeric(NHIS15$MODFREQW)

#moderate PA duration
table(NHIS15$MODMIN)
NHIS15$MODMIN <- as.numeric(NHIS15$MODMIN)


#Make PA
#Physicially active = light/moderate pa >=5x/wk & >=30min OR Vig >=3x/wk & >=20min, else not
NHIS15$PAC <- ifelse(NHIS15$MODFREQW >= 5 & NHIS15$MODFREQW <= 28 & NHIS15$MODMIN >= 30 & NHIS15$MODMIN < 997, "Physically active",
                     ifelse(NHIS15$VIGFREQW >= 3 & NHIS15$VIGFREQW <= 28 & NHIS15$VIGMIN >= 20 & NHIS15$VIGMIN < 997, "Physically active", "Not physically active"))


#BMI
#implied 2 decimal places, therefore divide all values by 100 to get the BMI value
NHIS15 <- NHIS15 %>%
  mutate(BMI = BMI/100)
#change all 99.99 (unknown values) to NA
NHIS15$BMI <- unknownToNA(NHIS15$BMI, unknown = 99.99)
#quick check to be sure that there are no more potential unknowns
NHIS15 %>% top_n(10, BMI)

#copd
NHIS15$COPDEV <- recode(NHIS15$COPDEV,
                        "1" = "1",
                        "2" = "0")
table(NHIS15$COPDEV)
NHIS15$COPDEV <- as.factor(NHIS15$COPDEV)

#ht
NHIS15$HYPEV <- recode(NHIS15$HYPEV,
                       "1" = "1",
                       "2" = "0")
table(NHIS15$HYPEV)
NHIS15$HYPEV <- as.factor(NHIS15$HYPEV)

#________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")


#for establishing unweighted sample size only, not for design object creation
NHIS15_JS <- subset(NHIS15,
                    !is.na(WTFA_SA) &
                      !is.na(STRAT_P) &
                      !is.na(PSU_P) &
                      !is.na(JNTSYMP))

table(subset(NHIS15_JS, AGE_P == "18 to 24")$SEX)
table(subset(NHIS15_JS, AGE_P == "25 to 29")$SEX)
table(subset(NHIS15_JS, AGE_P == "30 to 34")$SEX)
table(subset(NHIS15_JS, AGE_P == "35 to 39")$SEX)
table(subset(NHIS15_JS, AGE_P == "40 to 44")$SEX)
table(subset(NHIS15_JS, AGE_P == "45 to 49")$SEX)
table(subset(NHIS15_JS, AGE_P == "50 to 54")$SEX)
table(subset(NHIS15_JS, AGE_P == "55 to 59")$SEX)
table(subset(NHIS15_JS, AGE_P == "60 to 64")$SEX)
table(subset(NHIS15_JS, AGE_P == "65 to 69")$SEX)
table(subset(NHIS15_JS, AGE_P == "70 and above")$SEX)


#for establishing unweighted sample size only, not for design object creation
NHIS15_LBP <- subset(NHIS15,
                     !is.na(WTFA_SA) &
                       !is.na(STRAT_P) &
                       !is.na(PSU_P) &
                       !is.na(PAINLB))

table(subset(NHIS15_LBP, AGE_P == "18 to 24")$SEX)
table(subset(NHIS15_LBP, AGE_P == "25 to 29")$SEX)
table(subset(NHIS15_LBP, AGE_P == "30 to 34")$SEX)
table(subset(NHIS15_LBP, AGE_P == "35 to 39")$SEX)
table(subset(NHIS15_LBP, AGE_P == "40 to 44")$SEX)
table(subset(NHIS15_LBP, AGE_P == "45 to 49")$SEX)
table(subset(NHIS15_LBP, AGE_P == "50 to 54")$SEX)
table(subset(NHIS15_LBP, AGE_P == "55 to 59")$SEX)
table(subset(NHIS15_LBP, AGE_P == "60 to 64")$SEX)
table(subset(NHIS15_LBP, AGE_P == "65 to 69")$SEX)
table(subset(NHIS15_LBP, AGE_P == "70 and above")$SEX)


#make sure all complex survey requirements are available
NHIS15_SA_dataset <- subset(NHIS15,
                            !is.na(WTFA_SA) &
                              !is.na(STRAT_P) &
                              !is.na(PSU_P))

#Check that the sum of the weights is equal to the US population
sum(NHIS15_SA_dataset$WTFA_SA)
#The sum of the weights is 242 500 657, which is acceptable

#Check the number of people (unique PSU's)
length(unique(NHIS15_SA_dataset[["PSU_P"]]))
#The number of unique PSU's in the data is 2

#Check the number of unique strata
length(unique(NHIS15_SA_dataset[["STRAT_P"]]))
#The number of unique strata is 300

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHIS15_DO <- svydesign(ids = ~1,
                       weights = ~WTFA_SA,
                       strata = ~STRAT_P,
                       nest = TRUE,
                       data = NHIS15_SA_dataset)

#Save design object as RDS file for ease of retrieval for analysis, thereby avoiding running the data download and the design object creation code multiple times
saveRDS(NHIS15_DO,
        file = "NHIS15_DO.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


#________________________________________________________________________________________________________
#________________________________________________________________________________________________________
#________________________________________________________________________________________________________

# == 2014 == #

#DOWNLOAD

NHIS14.instructions <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Program_Code/NHIS/2014/samadult.sas"
NHIS14.fileloc <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/2014/samadult.zip"
#store as df
NHIS14_SA <- read.SAScii(NHIS14.fileloc, NHIS14.instructions, zipped = TRUE)
#Save as RDS file
saveRDS(NHIS14_SA,
        file = "NHIS14_SA.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

#CLEANING

#Some exploration
str(NHIS14_SA)
tail(NHIS14_SA)
glimpse(NHIS14_SA)
colnames(NHIS14_SA)

#Select variables
NHIS14 <- select(NHIS14_SA,
                 HHX, FMX, FPX,
                 REGION,
                 STRAT_P, PSU_P, SRVY_YR, WTFA_SA,
                 SEX, AGE_P,
                 DOINGLWA,
                 JNTSYMP, JMTHP1, JMTHP2, JMTHP3, JMTHP4, JMTHP5, JMTHP6, JMTHP7, JMTHP8, JMTHP9, JMTHP10, JMTHP11, JMTHP12, JMTHP13, JMTHP14, JMTHP15, JMTHP16, JMTHP17,
                 PAINLB,
                 VIGFREQW, VIGMIN, MODFREQW, MODMIN,
                 BMI,
                 HYPEV, COPDEV)


str(NHIS14)
tail(NHIS14)
glimpse(NHIS14)
colnames(NHIS14)

#joint symptoms recode
table(NHIS14$JNTSYMP)
#recode
NHIS14$JNTSYMP <- recode(NHIS14$JNTSYMP,
                         "1" = "1",
                         "2" = "0",
                         "7" = "7",
                         "9" = "9")
(NHIS14$JNTSYMP <- unknownToNA(NHIS14$JNTSYMP, unknown = c("7", "9")))
table(NHIS14$JNTSYMP)
NHIS14$JNTSYMP <- as.factor(NHIS14$JNTSYMP)

#low back pain recode
table(NHIS14$PAINLB)
#recode
NHIS14$PAINLB <- recode(NHIS14$PAINLB,
                        "1" = "1",
                        "2" = "0")
table(NHIS14$PAINLB)
NHIS14$PAINLB <- as.factor(NHIS14$PAINLB)

#region
table(NHIS14$REGION)
NHIS14$REGION <- recode(NHIS14$REGION,
                        "1" = "Northeast",
                        "2" = "Midwest",
                        "3" = "South",
                        "4" = "West")
table(NHIS14$REGION)

#age
table(NHIS14$AGE_P)



NHIS14$AGE_P <- recode(NHIS14$AGE_P,
                       "18" = "18 to 24",
                       "19" = "18 to 24",
                       "20" = "18 to 24",
                       "21" = "18 to 24",
                       "22" = "18 to 24",
                       "23" = "18 to 24",
                       "24" = "18 to 24",
                       "25" = "25 to 29",
                       "26" = "25 to 29",
                       "27" = "25 to 29",
                       "28" = "25 to 29",
                       "29" = "25 to 29",
                       "30" = "30 to 34",
                       "31" = "30 to 34",
                       "32" = "30 to 34",
                       "33" = "30 to 34",
                       "34" = "30 to 34",
                       "35" = "35 to 39",
                       "36" = "35 to 39",
                       "37" = "35 to 39",
                       "38" = "35 to 39", 
                       "39" = "35 to 39",
                       "40" = "40 to 44",
                       "41" = "40 to 44",
                       "42" = "40 to 44",
                       "43" = "40 to 44",
                       "44" = "40 to 44",
                       "45" = "45 to 49",
                       "46" = "45 to 49",
                       "47" = "45 to 49",
                       "48" = "45 to 49",
                       "49" = "45 to 49",
                       "50" = "50 to 54",
                       "51" = "50 to 54",
                       "52" = "50 to 54",
                       "53" = "50 to 54",
                       "54" = "50 to 54",
                       "55" = "55 to 59",
                       "56" = "55 to 59",
                       "57" = "55 to 59",
                       "58" = "55 to 59",
                       "59" = "55 to 59",
                       "60" = "60 to 64",
                       "61" = "60 to 64",
                       "62" = "60 to 64",
                       "63" = "60 to 64",
                       "64" = "60 to 64",
                       "65" = "65 to 69",
                       "66" = "65 to 69",
                       "67" = "65 to 69",
                       "68" = "65 to 69",
                       "69" = "65 to 69",
                       "70" = "70 and above",
                       "71" = "70 and above",
                       "72" = "70 and above",
                       "73" = "70 and above",
                       "74" = "70 and above",
                       "75" = "70 and above",
                       "76" = "70 and above",
                       "77" = "70 and above",
                       "78" = "70 and above",
                       "79" = "70 and above",
                       "80" = "70 and above",
                       "81" = "70 and above",
                       "82" = "70 and above",
                       "83" = "70 and above",
                       "84" = "70 and above",
                       "85" = "70 and above")
table(NHIS14$AGE_P)
NHIS14$AGE_P <- as.factor(NHIS14$AGE_P)

#sex
NHIS14$SEX <- recode(NHIS14$SEX,
                     "1" = "Male",
                     "2" = "Female")
table(NHIS14$SEX)
NHIS14$SEX <- as.factor(NHIS14$SEX)

#employment status
NHIS14$DOINGLWA <- recode(NHIS14$DOINGLWA,
                          "1" = "Working for pay at a job/business",
                          "2" = "2",
                          "3" = "3",
                          "4" = "4",
                          "5" = "Not working")
#Change unused values to NA
(NHIS14$DOINGLWA <- unknownToNA(NHIS14$DOINGLWA, unknown = c("9", "2", "3", "4")))
table(NHIS14$DOINGLWA)
NHIS14$DOINGLWA <- as.factor(NHIS14$DOINGLWA)

#affected joing areas quick clean
NHIS14$JMTHP1 <- unknownToNA(NHIS14$JMTHP1, unknown = 7:9)
NHIS14$JMTHP1 <- as.factor(NHIS14$JMTHP1)

NHIS14$JMTHP2 <- unknownToNA(NHIS14$JMTHP2, unknown = 7:9)
NHIS14$JMTHP2 <- as.factor(NHIS14$JMTHP2)

NHIS14$JMTHP3 <- unknownToNA(NHIS14$JMTHP3, unknown = 7:9)
NHIS14$JMTHP3 <- as.factor(NHIS14$JMTHP3)

NHIS14$JMTHP4 <- unknownToNA(NHIS14$JMTHP4, unknown = 7:9)
NHIS14$JMTHP4 <- as.factor(NHIS14$JMTHP4)

NHIS14$JMTHP5 <- unknownToNA(NHIS14$JMTHP5, unknown = 7:9)
NHIS14$JMTHP5 <- as.factor(NHIS14$JMTHP5)

NHIS14$JMTHP6 <- unknownToNA(NHIS14$JMTHP6, unknown = 7:9)
NHIS14$JMTHP6 <- as.factor(NHIS14$JMTHP6)

NHIS14$JMTHP7 <- unknownToNA(NHIS14$JMTHP7, unknown = 7:9)
NHIS14$JMTHP7 <- as.factor(NHIS14$JMTHP7)

NHIS14$JMTHP8 <- unknownToNA(NHIS14$JMTHP8, unknown = 7:9)
NHIS14$JMTHP8 <- as.factor(NHIS14$JMTHP8)

NHIS14$JMTHP9 <- unknownToNA(NHIS14$JMTHP9, unknown = 7:9)
NHIS14$JMTHP9 <- as.factor(NHIS14$JMTHP9)

NHIS14$JMTHP10 <- unknownToNA(NHIS14$JMTHP10, unknown = 7:9)
NHIS14$JMTHP10 <- as.factor(NHIS14$JMTHP10)

NHIS14$JMTHP11 <- unknownToNA(NHIS14$JMTHP11, unknown = 7:9)
NHIS14$JMTHP11 <- as.factor(NHIS14$JMTHP11)

NHIS14$JMTHP12 <- unknownToNA(NHIS14$JMTHP12, unknown = 7:9)
NHIS14$JMTHP12 <- as.factor(NHIS14$JMTHP12)

NHIS14$JMTHP13 <- unknownToNA(NHIS14$JMTHP13, unknown = 7:9)
NHIS14$JMTHP13 <- as.factor(NHIS14$JMTHP13)

NHIS14$JMTHP14 <- unknownToNA(NHIS14$JMTHP14, unknown = 7:9)
NHIS14$JMTHP14 <- as.factor(NHIS14$JMTHP14)

NHIS14$JMTHP15 <- unknownToNA(NHIS14$JMTHP15, unknown = 7:9)
NHIS14$JMTHP15 <- as.factor(NHIS14$JMTHP15)

NHIS14$JMTHP16 <- unknownToNA(NHIS14$JMTHP16, unknown = 7:9)
NHIS14$JMTHP16 <- as.factor(NHIS14$JMTHP16)

NHIS14$JMTHP17 <- unknownToNA(NHIS14$JMTHP17, unknown = 7:9)
NHIS14$JMTHP17 <- as.factor(NHIS14$JMTHP17)

#physical activity
#vigorous PA frequency
table(NHIS14$VIGFREQW)
NHIS14$VIGFREQW <- as.numeric(NHIS14$VIGFREQW)

#vigorous PA duration
table(NHIS14$VIGMIN)
NHIS14$VIGMIN <- as.numeric(NHIS14$VIGMIN)

#moderate pa frequency
table(NHIS14$MODFREQW)
NHIS14$MODFREQW <- as.numeric(NHIS14$MODFREQW)

#moderate PA duration
table(NHIS14$MODMIN)
NHIS14$MODMIN <- as.numeric(NHIS14$MODMIN)


#Make PA
#Physicially active = light/moderate pa >=5x/wk & >=30min OR Vig >=3x/wk & >=20min, else not
NHIS14$PAC <- ifelse(NHIS14$MODFREQW >= 5 & NHIS14$MODFREQW <= 28 & NHIS14$MODMIN >= 30 & NHIS14$MODMIN < 997, "Physically active",
                     ifelse(NHIS14$VIGFREQW >= 3 & NHIS14$VIGFREQW <= 28 & NHIS14$VIGMIN >= 20 & NHIS14$VIGMIN < 997, "Physically active", "Not physically active"))


#BMI
NHIS14$BMI <- unknownToNA(NHIS14$BMI, unknown = c("99.95", "99.99"))
#quick check to be sure that there are no more potential unknowns
NHIS14 %>% top_n(10, BMI)


#copd
NHIS14$COPDEV <- recode(NHIS14$COPDEV,
                        "1" = "1",
                        "2" = "0")
table(NHIS14$COPDEV)
NHIS14$COPDEV <- as.factor(NHIS14$COPDEV)

#ht
NHIS14$HYPEV <- recode(NHIS14$HYPEV,
                       "1" = "1",
                       "2" = "0")
table(NHIS14$HYPEV)
NHIS14$HYPEV <- as.factor(NHIS14$HYPEV)

#________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")


#for establishing unweighted sample size only, not for design object creation
NHIS14_JS <- subset(NHIS14,
                    !is.na(WTFA_SA) &
                      !is.na(STRAT_P) &
                      !is.na(PSU_P) &
                      !is.na(JNTSYMP))

table(subset(NHIS14_JS, AGE_P == "18 to 24")$SEX)
table(subset(NHIS14_JS, AGE_P == "25 to 29")$SEX)
table(subset(NHIS14_JS, AGE_P == "30 to 34")$SEX)
table(subset(NHIS14_JS, AGE_P == "35 to 39")$SEX)
table(subset(NHIS14_JS, AGE_P == "40 to 44")$SEX)
table(subset(NHIS14_JS, AGE_P == "45 to 49")$SEX)
table(subset(NHIS14_JS, AGE_P == "50 to 54")$SEX)
table(subset(NHIS14_JS, AGE_P == "55 to 59")$SEX)
table(subset(NHIS14_JS, AGE_P == "60 to 64")$SEX)
table(subset(NHIS14_JS, AGE_P == "65 to 69")$SEX)
table(subset(NHIS14_JS, AGE_P == "70 and above")$SEX)


#for establishing unweighted sample size only, not for design object creation
NHIS14_LBP <- subset(NHIS14,
                     !is.na(WTFA_SA) &
                       !is.na(STRAT_P) &
                       !is.na(PSU_P) &
                       !is.na(PAINLB))

table(subset(NHIS14_LBP, AGE_P == "18 to 24")$SEX)
table(subset(NHIS14_LBP, AGE_P == "25 to 29")$SEX)
table(subset(NHIS14_LBP, AGE_P == "30 to 34")$SEX)
table(subset(NHIS14_LBP, AGE_P == "35 to 39")$SEX)
table(subset(NHIS14_LBP, AGE_P == "40 to 44")$SEX)
table(subset(NHIS14_LBP, AGE_P == "45 to 49")$SEX)
table(subset(NHIS14_LBP, AGE_P == "50 to 54")$SEX)
table(subset(NHIS14_LBP, AGE_P == "55 to 59")$SEX)
table(subset(NHIS14_LBP, AGE_P == "60 to 64")$SEX)
table(subset(NHIS14_LBP, AGE_P == "65 to 69")$SEX)
table(subset(NHIS14_LBP, AGE_P == "70 and above")$SEX)


#make sure all complex survey requirements are available
NHIS14_SA_dataset <- subset(NHIS14,
                            !is.na(WTFA_SA) &
                              !is.na(STRAT_P) &
                              !is.na(PSU_P))

#Check that the sum of the weights is equal to the US population
sum(NHIS14_SA_dataset$WTFA_SA)
#The sum of the weights is 239 688 457, which is acceptable

#Check the number of people (unique PSU's)
length(unique(NHIS14_SA_dataset[["PSU_P"]]))
#The number of unique PSU's in the data is 2

#Check the number of unique strata
length(unique(NHIS14_SA_dataset[["STRAT_P"]]))
#The number of unique strata is 300

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHIS14_DO <- svydesign(ids = ~1,
                       weights = ~WTFA_SA,
                       strata = ~STRAT_P,
                       nest = TRUE,
                       data = NHIS14_SA_dataset)

#Save design object as RDS file for ease of retrieval for analysis, thereby avoiding running the data download and the design object creation code multiple times
saveRDS(NHIS14_DO,
        file = "NHIS14_DO.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

#_________________________________________________________________________________________
#_________________________________________________________________________________________
#_________________________________________________________________________________________

# == 2013 == #

#DOWNLOAD

NHIS13.instructions <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Program_Code/NHIS/2013/samadult.sas"
NHIS13.fileloc <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/2013/samadult.zip"
#store as df
NHIS13_SA <- read.SAScii(NHIS13.fileloc, NHIS13.instructions, zipped = TRUE)
#Save as RDS file
saveRDS(NHIS13_SA,
        file = "NHIS13_SA.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

#CLEANING

#Some exploration
str(NHIS13_SA)
tail(NHIS13_SA)
glimpse(NHIS13_SA)
colnames(NHIS13_SA)

#Select variables
NHIS13 <- select(NHIS13_SA,
                 HHX, FMX, FPX,
                 REGION,
                 STRAT_P, PSU_P, SRVY_YR, WTFA_SA,
                 SEX, AGE_P,
                 DOINGLWA,
                 JNTSYMP, JMTHP1, JMTHP2, JMTHP3, JMTHP4, JMTHP5, JMTHP6, JMTHP7, JMTHP8, JMTHP9, JMTHP10, JMTHP11, JMTHP12, JMTHP13, JMTHP14, JMTHP15, JMTHP16, JMTHP17,
                 PAINLB,
                 VIGFREQW, VIGMIN, MODFREQW, MODMIN, 
                 BMI,
                 HYPEV, COPDEV)

str(NHIS13)
tail(NHIS13)
glimpse(NHIS13)
colnames(NHIS13)

#joint symptoms recode
table(NHIS13$JNTSYMP)
#recode
NHIS13$JNTSYMP <- recode(NHIS13$JNTSYMP,
                         "1" = "1",
                         "2" = "0",
                         "7" = "7",
                         "9" = "9")
(NHIS13$JNTSYMP <- unknownToNA(NHIS13$JNTSYMP, unknown = c("7", "9")))
table(NHIS13$JNTSYMP)
NHIS13$JNTSYMP <- as.factor(NHIS13$JNTSYMP)

#low back pain recode
table(NHIS13$PAINLB)
#recode
NHIS13$PAINLB <- recode(NHIS13$PAINLB,
                        "1" = "1",
                        "2" = "0")
table(NHIS13$PAINLB)
NHIS13$PAINLB <- as.factor(NHIS13$PAINLB)

#region
table(NHIS13$REGION)
NHIS13$REGION <- recode(NHIS13$REGION,
                        "1" = "Northeast",
                        "2" = "Midwest",
                        "3" = "South",
                        "4" = "West")
table(NHIS13$REGION)

#age
table(NHIS13$AGE_P)



NHIS13$AGE_P <- recode(NHIS13$AGE_P,
                       "18" = "18 to 24",
                       "19" = "18 to 24",
                       "20" = "18 to 24",
                       "21" = "18 to 24",
                       "22" = "18 to 24",
                       "23" = "18 to 24",
                       "24" = "18 to 24",
                       "25" = "25 to 29",
                       "26" = "25 to 29",
                       "27" = "25 to 29",
                       "28" = "25 to 29",
                       "29" = "25 to 29",
                       "30" = "30 to 34",
                       "31" = "30 to 34",
                       "32" = "30 to 34",
                       "33" = "30 to 34",
                       "34" = "30 to 34",
                       "35" = "35 to 39",
                       "36" = "35 to 39",
                       "37" = "35 to 39",
                       "38" = "35 to 39", 
                       "39" = "35 to 39",
                       "40" = "40 to 44",
                       "41" = "40 to 44",
                       "42" = "40 to 44",
                       "43" = "40 to 44",
                       "44" = "40 to 44",
                       "45" = "45 to 49",
                       "46" = "45 to 49",
                       "47" = "45 to 49",
                       "48" = "45 to 49",
                       "49" = "45 to 49",
                       "50" = "50 to 54",
                       "51" = "50 to 54",
                       "52" = "50 to 54",
                       "53" = "50 to 54",
                       "54" = "50 to 54",
                       "55" = "55 to 59",
                       "56" = "55 to 59",
                       "57" = "55 to 59",
                       "58" = "55 to 59",
                       "59" = "55 to 59",
                       "60" = "60 to 64",
                       "61" = "60 to 64",
                       "62" = "60 to 64",
                       "63" = "60 to 64",
                       "64" = "60 to 64",
                       "65" = "65 to 69",
                       "66" = "65 to 69",
                       "67" = "65 to 69",
                       "68" = "65 to 69",
                       "69" = "65 to 69",
                       "70" = "70 and above",
                       "71" = "70 and above",
                       "72" = "70 and above",
                       "73" = "70 and above",
                       "74" = "70 and above",
                       "75" = "70 and above",
                       "76" = "70 and above",
                       "77" = "70 and above",
                       "78" = "70 and above",
                       "79" = "70 and above",
                       "80" = "70 and above",
                       "81" = "70 and above",
                       "82" = "70 and above",
                       "83" = "70 and above",
                       "84" = "70 and above",
                       "85" = "70 and above")
table(NHIS13$AGE_P)
NHIS13$AGE_P <- as.factor(NHIS13$AGE_P)

#sex
NHIS13$SEX <- recode(NHIS13$SEX,
                     "1" = "Male",
                     "2" = "Female")
table(NHIS13$SEX)
NHIS13$SEX <- as.factor(NHIS13$SEX)

#employment status
NHIS13$DOINGLWA <- recode(NHIS13$DOINGLWA,
                          "1" = "Working for pay at a job/business",
                          "2" = "2",
                          "3" = "3",
                          "4" = "4",
                          "5" = "Not working")
#Change unused values to NA
(NHIS13$DOINGLWA <- unknownToNA(NHIS13$DOINGLWA, unknown = c("9", "2", "3", "4")))
table(NHIS13$DOINGLWA)
NHIS13$DOINGLWA <- as.factor(NHIS13$DOINGLWA)

#affected joing areas quick clean
NHIS13$JMTHP1 <- unknownToNA(NHIS13$JMTHP1, unknown = 7:9)
NHIS13$JMTHP1 <- as.factor(NHIS13$JMTHP1)

NHIS13$JMTHP2 <- unknownToNA(NHIS13$JMTHP2, unknown = 7:9)
NHIS13$JMTHP2 <- as.factor(NHIS13$JMTHP2)

NHIS13$JMTHP3 <- unknownToNA(NHIS13$JMTHP3, unknown = 7:9)
NHIS13$JMTHP3 <- as.factor(NHIS13$JMTHP3)

NHIS13$JMTHP4 <- unknownToNA(NHIS13$JMTHP4, unknown = 7:9)
NHIS13$JMTHP4 <- as.factor(NHIS13$JMTHP4)

NHIS13$JMTHP5 <- unknownToNA(NHIS13$JMTHP5, unknown = 7:9)
NHIS13$JMTHP5 <- as.factor(NHIS13$JMTHP5)

NHIS13$JMTHP6 <- unknownToNA(NHIS13$JMTHP6, unknown = 7:9)
NHIS13$JMTHP6 <- as.factor(NHIS13$JMTHP6)

NHIS13$JMTHP7 <- unknownToNA(NHIS13$JMTHP7, unknown = 7:9)
NHIS13$JMTHP7 <- as.factor(NHIS13$JMTHP7)

NHIS13$JMTHP8 <- unknownToNA(NHIS13$JMTHP8, unknown = 7:9)
NHIS13$JMTHP8 <- as.factor(NHIS13$JMTHP8)

NHIS13$JMTHP9 <- unknownToNA(NHIS13$JMTHP9, unknown = 7:9)
NHIS13$JMTHP9 <- as.factor(NHIS13$JMTHP9)

NHIS13$JMTHP10 <- unknownToNA(NHIS13$JMTHP10, unknown = 7:9)
NHIS13$JMTHP10 <- as.factor(NHIS13$JMTHP10)

NHIS13$JMTHP11 <- unknownToNA(NHIS13$JMTHP11, unknown = 7:9)
NHIS13$JMTHP11 <- as.factor(NHIS13$JMTHP11)

NHIS13$JMTHP12 <- unknownToNA(NHIS13$JMTHP12, unknown = 7:9)
NHIS13$JMTHP12 <- as.factor(NHIS13$JMTHP12)

NHIS13$JMTHP13 <- unknownToNA(NHIS13$JMTHP13, unknown = 7:9)
NHIS13$JMTHP13 <- as.factor(NHIS13$JMTHP13)

NHIS13$JMTHP14 <- unknownToNA(NHIS13$JMTHP14, unknown = 7:9)
NHIS13$JMTHP14 <- as.factor(NHIS13$JMTHP14)

NHIS13$JMTHP15 <- unknownToNA(NHIS13$JMTHP15, unknown = 7:9)
NHIS13$JMTHP15 <- as.factor(NHIS13$JMTHP15)

NHIS13$JMTHP16 <- unknownToNA(NHIS13$JMTHP16, unknown = 7:9)
NHIS13$JMTHP16 <- as.factor(NHIS13$JMTHP16)

NHIS13$JMTHP17 <- unknownToNA(NHIS13$JMTHP17, unknown = 7:9)
NHIS13$JMTHP17 <- as.factor(NHIS13$JMTHP17)

#physical activity
#vigorous PA frequency
#NHIS13$VIGFREQW <- unknownToNA(NHIS13$VIGFREQW, unknown = c(97:99))
table(NHIS13$VIGFREQW)
NHIS13$VIGFREQW <- as.numeric(NHIS13$VIGFREQW)

#vigorous PA duration
#NHIS13$VIGMIN <- unknownToNA(NHIS13$VIGMIN, unknown = c(997:999))
table(NHIS13$VIGMIN)
NHIS13$VIGMIN <- as.numeric(NHIS13$VIGMIN)

#moderate pa frequency
#NHIS13$MODFREQW <- unknownToNA(NHIS13$MODFREQW, unknown = c(97:99))
table(NHIS13$MODFREQW)
NHIS13$MODFREQW <- as.numeric(NHIS13$MODFREQW)

#moderate PA duration
#NHIS13$MODMIN <- unknownToNA(NHIS13$MODMIN, unknown = c(997:999))
table(NHIS13$MODMIN)
NHIS13$MODMIN <- as.numeric(NHIS13$MODMIN)

#Make PA
#Physicially active = light/moderate pa >=5x/wk & >=30min OR Vig >=3x/wk & >=20min, else not
NHIS13$PAC <- ifelse(NHIS13$MODFREQW >= 5 & NHIS13$MODFREQW <= 28 & NHIS13$MODMIN >= 30 & NHIS13$MODMIN < 997, "Physically active",
                     ifelse(NHIS13$VIGFREQW >= 3 & NHIS13$VIGFREQW <= 28 & NHIS13$VIGMIN >= 20 & NHIS13$VIGMIN < 997, "Physically active", "Not physically active"))


#BMI
#change all 99.99 (unknown values) to NA
NHIS13$BMI <- unknownToNA(NHIS13$BMI, unknown = c("99.99"))
#quick check to be sure that there are no more potential unknowns
NHIS13 %>% top_n(10, BMI)

#copd
NHIS13$COPDEV <- recode(NHIS13$COPDEV,
                        "1" = "1",
                        "2" = "0")
table(NHIS13$COPDEV)
NHIS13$COPDEV <- as.factor(NHIS13$COPDEV)

#ht
NHIS13$HYPEV <- recode(NHIS13$HYPEV,
                       "1" = "1",
                       "2" = "0")
table(NHIS13$HYPEV)
NHIS13$HYPEV <- as.factor(NHIS13$HYPEV)

#________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")



#for establishing unweighted sample size only, not for design object creation
NHIS13_JS <- subset(NHIS13,
                    !is.na(WTFA_SA) &
                      !is.na(STRAT_P) &
                      !is.na(PSU_P) &
                      !is.na(JNTSYMP))

table(subset(NHIS13_JS, AGE_P == "18 to 24")$SEX)
table(subset(NHIS13_JS, AGE_P == "25 to 29")$SEX)
table(subset(NHIS13_JS, AGE_P == "30 to 34")$SEX)
table(subset(NHIS13_JS, AGE_P == "35 to 39")$SEX)
table(subset(NHIS13_JS, AGE_P == "40 to 44")$SEX)
table(subset(NHIS13_JS, AGE_P == "45 to 49")$SEX)
table(subset(NHIS13_JS, AGE_P == "50 to 54")$SEX)
table(subset(NHIS13_JS, AGE_P == "55 to 59")$SEX)
table(subset(NHIS13_JS, AGE_P == "60 to 64")$SEX)
table(subset(NHIS13_JS, AGE_P == "65 to 69")$SEX)
table(subset(NHIS13_JS, AGE_P == "70 and above")$SEX)


#for establishing unweighted sample size only, not for design object creation
NHIS13_LBP <- subset(NHIS13,
                     !is.na(WTFA_SA) &
                       !is.na(STRAT_P) &
                       !is.na(PSU_P) &
                       !is.na(PAINLB))

table(subset(NHIS13_LBP, AGE_P == "18 to 24")$SEX)
table(subset(NHIS13_LBP, AGE_P == "25 to 29")$SEX)
table(subset(NHIS13_LBP, AGE_P == "30 to 34")$SEX)
table(subset(NHIS13_LBP, AGE_P == "35 to 39")$SEX)
table(subset(NHIS13_LBP, AGE_P == "40 to 44")$SEX)
table(subset(NHIS13_LBP, AGE_P == "45 to 49")$SEX)
table(subset(NHIS13_LBP, AGE_P == "50 to 54")$SEX)
table(subset(NHIS13_LBP, AGE_P == "55 to 59")$SEX)
table(subset(NHIS13_LBP, AGE_P == "60 to 64")$SEX)
table(subset(NHIS13_LBP, AGE_P == "65 to 69")$SEX)
table(subset(NHIS13_LBP, AGE_P == "70 and above")$SEX)


#make sure all complex survey requirements are available
NHIS13_SA_dataset <- subset(NHIS13,
                            !is.na(WTFA_SA) &
                              !is.na(STRAT_P) &
                              !is.na(PSU_P))

#Check that the sum of the weights is equal to the US population
sum(NHIS13_SA_dataset$WTFA_SA)
#The sum of the weights is 237 394 354, which is acceptable

#Check the number of people (unique PSU's)
length(unique(NHIS13_SA_dataset[["PSU_P"]]))
#The number of unique PSU's in the data is 2

#Check the number of unique strata
length(unique(NHIS13_SA_dataset[["STRAT_P"]]))
#The number of unique strata is 300

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHIS13_DO <- svydesign(ids = ~1,
                       weights = ~WTFA_SA,
                       strata = ~STRAT_P,
                       nest = TRUE,
                       data = NHIS13_SA_dataset)

#Save design object as RDS file for ease of retrieval for analysis, thereby avoiding running the data download and the design object creation code multiple times
saveRDS(NHIS13_DO,
        file = "NHIS13_DO.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

#________________________________________________________________________________
#________________________________________________________________________________
#________________________________________________________________________________


# == 2012 == #

#DOWNLOAD

NHIS12.instructions <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Program_Code/NHIS/2012/samadult.sas"
NHIS12.fileloc <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/2012/samadult.zip"
#store as df
NHIS12_SA <- read.SAScii(NHIS12.fileloc, NHIS12.instructions, zipped = TRUE)
#save as RDS file
saveRDS(NHIS12_SA,
        file = "NHIS12_SA.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

#CLEANING

#Some exploration
str(NHIS12_SA)
tail(NHIS12_SA)
glimpse(NHIS12_SA)
colnames(NHIS12_SA)

#Select variables
NHIS12 <- select(NHIS12_SA,
                 HHX, FMX, FPX,
                 REGION,
                 STRAT_P, PSU_P, SRVY_YR, WTFA_SA,
                 SEX, AGE_P,
                 DOINGLWA,
                 JNTSYMP, JMTHP1, JMTHP2, JMTHP3, JMTHP4, JMTHP5, JMTHP6, JMTHP7, JMTHP8, JMTHP9, JMTHP10, JMTHP11, JMTHP12, JMTHP13, JMTHP14, JMTHP15, JMTHP16, JMTHP17,
                 PAINLB,
                 VIGFREQW, VIGMIN, MODFREQW, MODMIN,
                 BMI,
                 HYPEV, COPDEV)

str(NHIS12)
tail(NHIS12)
glimpse(NHIS12)
colnames(NHIS12)

#joint symptoms recode
table(NHIS12$JNTSYMP)
#recode
NHIS12$JNTSYMP <- recode(NHIS12$JNTSYMP,
                         "1" = "1",
                         "2" = "0",
                         "7" = "7",
                         "9" = "9")
(NHIS12$JNTSYMP <- unknownToNA(NHIS12$JNTSYMP, unknown = c("7", "9")))
table(NHIS12$JNTSYMP)
NHIS12$JNTSYMP <- as.factor(NHIS12$JNTSYMP)

#low back pain recode
table(NHIS12$PAINLB)
#recode
NHIS12$PAINLB <- recode(NHIS12$PAINLB,
                        "1" = "1",
                        "2" = "0")
table(NHIS12$PAINLB)
NHIS12$PAINLB <- as.factor(NHIS12$PAINLB)

#region
table(NHIS12$REGION)
NHIS12$REGION <- recode(NHIS12$REGION,
                        "1" = "Northeast",
                        "2" = "Midwest",
                        "3" = "South",
                        "4" = "West")
table(NHIS12$REGION)

#age
table(NHIS12$AGE_P)



NHIS12$AGE_P <- recode(NHIS12$AGE_P,
                       "18" = "18 to 24",
                       "19" = "18 to 24",
                       "20" = "18 to 24",
                       "21" = "18 to 24",
                       "22" = "18 to 24",
                       "23" = "18 to 24",
                       "24" = "18 to 24",
                       "25" = "25 to 29",
                       "26" = "25 to 29",
                       "27" = "25 to 29",
                       "28" = "25 to 29",
                       "29" = "25 to 29",
                       "30" = "30 to 34",
                       "31" = "30 to 34",
                       "32" = "30 to 34",
                       "33" = "30 to 34",
                       "34" = "30 to 34",
                       "35" = "35 to 39",
                       "36" = "35 to 39",
                       "37" = "35 to 39",
                       "38" = "35 to 39", 
                       "39" = "35 to 39",
                       "40" = "40 to 44",
                       "41" = "40 to 44",
                       "42" = "40 to 44",
                       "43" = "40 to 44",
                       "44" = "40 to 44",
                       "45" = "45 to 49",
                       "46" = "45 to 49",
                       "47" = "45 to 49",
                       "48" = "45 to 49",
                       "49" = "45 to 49",
                       "50" = "50 to 54",
                       "51" = "50 to 54",
                       "52" = "50 to 54",
                       "53" = "50 to 54",
                       "54" = "50 to 54",
                       "55" = "55 to 59",
                       "56" = "55 to 59",
                       "57" = "55 to 59",
                       "58" = "55 to 59",
                       "59" = "55 to 59",
                       "60" = "60 to 64",
                       "61" = "60 to 64",
                       "62" = "60 to 64",
                       "63" = "60 to 64",
                       "64" = "60 to 64",
                       "65" = "65 to 69",
                       "66" = "65 to 69",
                       "67" = "65 to 69",
                       "68" = "65 to 69",
                       "69" = "65 to 69",
                       "70" = "70 and above",
                       "71" = "70 and above",
                       "72" = "70 and above",
                       "73" = "70 and above",
                       "74" = "70 and above",
                       "75" = "70 and above",
                       "76" = "70 and above",
                       "77" = "70 and above",
                       "78" = "70 and above",
                       "79" = "70 and above",
                       "80" = "70 and above",
                       "81" = "70 and above",
                       "82" = "70 and above",
                       "83" = "70 and above",
                       "84" = "70 and above",
                       "85" = "70 and above")
table(NHIS12$AGE_P)
NHIS12$AGE_P <- as.factor(NHIS12$AGE_P)

#sex
NHIS12$SEX <- recode(NHIS12$SEX,
                     "1" = "Male",
                     "2" = "Female")
table(NHIS12$SEX)
NHIS12$SEX <- as.factor(NHIS12$SEX)

#employment status
NHIS12$DOINGLWA <- recode(NHIS12$DOINGLWA,
                          "1" = "Working for pay at a job/business",
                          "2" = "2",
                          "3" = "3",
                          "4" = "4",
                          "5" = "Not working")
#Change unused values to NA
(NHIS12$DOINGLWA <- unknownToNA(NHIS12$DOINGLWA, unknown = c("9", "2", "3", "4")))
table(NHIS12$DOINGLWA)
NHIS12$DOINGLWA <- as.factor(NHIS12$DOINGLWA)

#affected joing areas quick clean
NHIS12$JMTHP1 <- unknownToNA(NHIS12$JMTHP1, unknown = 7:9)
NHIS12$JMTHP1 <- as.factor(NHIS12$JMTHP1)

NHIS12$JMTHP2 <- unknownToNA(NHIS12$JMTHP2, unknown = 7:9)
NHIS12$JMTHP2 <- as.factor(NHIS12$JMTHP2)

NHIS12$JMTHP3 <- unknownToNA(NHIS12$JMTHP3, unknown = 7:9)
NHIS12$JMTHP3 <- as.factor(NHIS12$JMTHP3)

NHIS12$JMTHP4 <- unknownToNA(NHIS12$JMTHP4, unknown = 7:9)
NHIS12$JMTHP4 <- as.factor(NHIS12$JMTHP4)

NHIS12$JMTHP5 <- unknownToNA(NHIS12$JMTHP5, unknown = 7:9)
NHIS12$JMTHP5 <- as.factor(NHIS12$JMTHP5)

NHIS12$JMTHP6 <- unknownToNA(NHIS12$JMTHP6, unknown = 7:9)
NHIS12$JMTHP6 <- as.factor(NHIS12$JMTHP6)

NHIS12$JMTHP7 <- unknownToNA(NHIS12$JMTHP7, unknown = 7:9)
NHIS12$JMTHP7 <- as.factor(NHIS12$JMTHP7)

NHIS12$JMTHP8 <- unknownToNA(NHIS12$JMTHP8, unknown = 7:9)
NHIS12$JMTHP8 <- as.factor(NHIS12$JMTHP8)

NHIS12$JMTHP9 <- unknownToNA(NHIS12$JMTHP9, unknown = 7:9)
NHIS12$JMTHP9 <- as.factor(NHIS12$JMTHP9)

NHIS12$JMTHP10 <- unknownToNA(NHIS12$JMTHP10, unknown = 7:9)
NHIS12$JMTHP10 <- as.factor(NHIS12$JMTHP10)

NHIS12$JMTHP11 <- unknownToNA(NHIS12$JMTHP11, unknown = 7:9)
NHIS12$JMTHP11 <- as.factor(NHIS12$JMTHP11)

NHIS12$JMTHP12 <- unknownToNA(NHIS12$JMTHP12, unknown = 7:9)
NHIS12$JMTHP12 <- as.factor(NHIS12$JMTHP12)

NHIS12$JMTHP13 <- unknownToNA(NHIS12$JMTHP13, unknown = 7:9)
NHIS12$JMTHP13 <- as.factor(NHIS12$JMTHP13)

NHIS12$JMTHP14 <- unknownToNA(NHIS12$JMTHP14, unknown = 7:9)
NHIS12$JMTHP14 <- as.factor(NHIS12$JMTHP14)

NHIS12$JMTHP15 <- unknownToNA(NHIS12$JMTHP15, unknown = 7:9)
NHIS12$JMTHP15 <- as.factor(NHIS12$JMTHP15)

NHIS12$JMTHP16 <- unknownToNA(NHIS12$JMTHP16, unknown = 7:9)
NHIS12$JMTHP16 <- as.factor(NHIS12$JMTHP16)

NHIS12$JMTHP17 <- unknownToNA(NHIS12$JMTHP17, unknown = 7:9)
NHIS12$JMTHP17 <- as.factor(NHIS12$JMTHP17)

#physical activity
#vigorous PA frequency
table(NHIS12$VIGFREQW)
NHIS12$VIGFREQW <- as.numeric(NHIS12$VIGFREQW)

#vigorous PA duration
table(NHIS12$VIGMIN)
NHIS12$VIGMIN <- as.numeric(NHIS12$VIGMIN)

#moderate pa frequency
table(NHIS12$MODFREQW)
NHIS12$MODFREQW <- as.numeric(NHIS12$MODFREQW)

#moderate PA duration
table(NHIS12$MODMIN)
NHIS12$MODMIN <- as.numeric(NHIS12$MODMIN)

#Make PA
#Physicially active = light/moderate pa >=5x/wk & >=30min OR Vig >=3x/wk & >=20min, else not
NHIS12$PAC <- ifelse(NHIS12$MODFREQW >= 5 & NHIS12$MODFREQW <= 28 & NHIS12$MODMIN >= 30 & NHIS12$MODMIN < 997, "Physically active",
                     ifelse(NHIS12$VIGFREQW >= 3 & NHIS12$VIGFREQW <= 28 & NHIS12$VIGMIN >= 20 & NHIS12$VIGMIN < 997, "Physically active", "Not physically active"))


#BMI
#change all 99.99 (unknown values) to NA
NHIS12$BMI <- unknownToNA(NHIS12$BMI, unknown = c("99.99"))
#quick check to be sure that there are no more potential unknowns
NHIS12 %>% top_n(10, BMI)

#copd
NHIS12$COPDEV <- recode(NHIS12$COPDEV,
                        "1" = "1",
                        "2" = "0")
table(NHIS12$COPDEV)
NHIS12$COPDEV <- as.factor(NHIS12$COPDEV)

#ht
NHIS12$HYPEV <- recode(NHIS12$HYPEV,
                       "1" = "1",
                       "2" = "0")
table(NHIS12$HYPEV)
NHIS12$HYPEV <- as.factor(NHIS12$HYPEV)

#________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#for establishing unweighted sample size only, not for design object creation
NHIS12_JS <- subset(NHIS12,
                    !is.na(WTFA_SA) &
                      !is.na(STRAT_P) &
                      !is.na(PSU_P) &
                      !is.na(JNTSYMP))

table(subset(NHIS12_JS, AGE_P == "18 to 24")$SEX)
table(subset(NHIS12_JS, AGE_P == "25 to 29")$SEX)
table(subset(NHIS12_JS, AGE_P == "30 to 34")$SEX)
table(subset(NHIS12_JS, AGE_P == "35 to 39")$SEX)
table(subset(NHIS12_JS, AGE_P == "40 to 44")$SEX)
table(subset(NHIS12_JS, AGE_P == "45 to 49")$SEX)
table(subset(NHIS12_JS, AGE_P == "50 to 54")$SEX)
table(subset(NHIS12_JS, AGE_P == "55 to 59")$SEX)
table(subset(NHIS12_JS, AGE_P == "60 to 64")$SEX)
table(subset(NHIS12_JS, AGE_P == "65 to 69")$SEX)
table(subset(NHIS12_JS, AGE_P == "70 and above")$SEX)


#for establishing unweighted sample size only, not for design object creation
NHIS12_LBP <- subset(NHIS12,
                     !is.na(WTFA_SA) &
                       !is.na(STRAT_P) &
                       !is.na(PSU_P) &
                       !is.na(PAINLB))

table(subset(NHIS12_LBP, AGE_P == "18 to 24")$SEX)
table(subset(NHIS12_LBP, AGE_P == "25 to 29")$SEX)
table(subset(NHIS12_LBP, AGE_P == "30 to 34")$SEX)
table(subset(NHIS12_LBP, AGE_P == "35 to 39")$SEX)
table(subset(NHIS12_LBP, AGE_P == "40 to 44")$SEX)
table(subset(NHIS12_LBP, AGE_P == "45 to 49")$SEX)
table(subset(NHIS12_LBP, AGE_P == "50 to 54")$SEX)
table(subset(NHIS12_LBP, AGE_P == "55 to 59")$SEX)
table(subset(NHIS12_LBP, AGE_P == "60 to 64")$SEX)
table(subset(NHIS12_LBP, AGE_P == "65 to 69")$SEX)
table(subset(NHIS12_LBP, AGE_P == "70 and above")$SEX)


#make sure all complex survey requirements are available
NHIS12_SA_dataset <- subset(NHIS12,
                            !is.na(WTFA_SA) &
                              !is.na(STRAT_P) &
                              !is.na(PSU_P))

#Check that the sum of the weights is equal to the US population
sum(NHIS12_SA_dataset$WTFA_SA)
#The sum of the weights is 234 920 670, which is acceptable

#Check the number of people (unique PSU's)
length(unique(NHIS12_SA_dataset[["PSU_P"]]))
#The number of unique PSU's in the data is 2

#Check the number of unique strata
length(unique(NHIS12_SA_dataset[["STRAT_P"]]))
#The number of unique strata is 300

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHIS12_DO <- svydesign(ids = ~1,
                       weights = ~WTFA_SA,
                       strata = ~STRAT_P,
                       nest = TRUE,
                       data = NHIS12_SA_dataset)

#Save design object as RDS file for ease of retrieval for analysis, thereby avoiding running the data download and the design object creation code multiple times
saveRDS(NHIS12_DO,
        file = "NHIS12_DO.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

#__________________________________________________________________________
#__________________________________________________________________________
#__________________________________________________________________________

# == 2011 == #

#DOWNLOAD

NHIS11.instructions <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Program_Code/NHIS/2011/samadult.sas"
NHIS11.fileloc <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/2011/samadult.zip"
#store as df
NHIS11_SA <- read.SAScii(NHIS11.fileloc, NHIS11.instructions, zipped = TRUE)
#save as RDS file
saveRDS(NHIS11_SA,
        file = "NHIS11_SA.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


#CLEANING

#Some exploration
str(NHIS11_SA)
tail(NHIS11_SA)
glimpse(NHIS11_SA)
colnames(NHIS11_SA)

#Select variables
NHIS11 <- select(NHIS11_SA,
                 HHX, FMX, FPX,
                 REGION,
                 STRAT_P, PSU_P, SRVY_YR, WTFA_SA,
                 SEX, AGE_P,
                 DOINGLWA,
                 JNTSYMP, JMTHP1, JMTHP2, JMTHP3, JMTHP4, JMTHP5, JMTHP6, JMTHP7, JMTHP8, JMTHP9, JMTHP10, JMTHP11, JMTHP12, JMTHP13, JMTHP14, JMTHP15, JMTHP16, JMTHP17,
                 PAINLB,
                 VIGFREQW, VIGMIN, MODFREQW, MODMIN,
                 BMI,
                 HYPEV)

str(NHIS11)
tail(NHIS11)
glimpse(NHIS11)
colnames(NHIS11)

#joint symptoms recode
table(NHIS11$JNTSYMP)
#recode
NHIS11$JNTSYMP <- recode(NHIS11$JNTSYMP,
                         "1" = "1",
                         "2" = "0",
                         "7" = "7",
                         "9" = "9")
(NHIS11$JNTSYMP <- unknownToNA(NHIS11$JNTSYMP, unknown = c("7", "9")))
table(NHIS11$JNTSYMP)
NHIS11$JNTSYMP <- as.factor(NHIS11$JNTSYMP)

#low back pain recode
table(NHIS11$PAINLB)
#recode
NHIS11$PAINLB <- recode(NHIS11$PAINLB,
                        "1" = "1",
                        "2" = "0")
table(NHIS11$PAINLB)
NHIS11$PAINLB <- as.factor(NHIS11$PAINLB)

#region
table(NHIS11$REGION)
NHIS11$REGION <- recode(NHIS11$REGION,
                        "1" = "Northeast",
                        "2" = "Midwest",
                        "3" = "South",
                        "4" = "West")
table(NHIS11$REGION)

#age
table(NHIS11$AGE_P)



NHIS11$AGE_P <- recode(NHIS11$AGE_P,
                       "18" = "18 to 24",
                       "19" = "18 to 24",
                       "20" = "18 to 24",
                       "21" = "18 to 24",
                       "22" = "18 to 24",
                       "23" = "18 to 24",
                       "24" = "18 to 24",
                       "25" = "25 to 29",
                       "26" = "25 to 29",
                       "27" = "25 to 29",
                       "28" = "25 to 29",
                       "29" = "25 to 29",
                       "30" = "30 to 34",
                       "31" = "30 to 34",
                       "32" = "30 to 34",
                       "33" = "30 to 34",
                       "34" = "30 to 34",
                       "35" = "35 to 39",
                       "36" = "35 to 39",
                       "37" = "35 to 39",
                       "38" = "35 to 39", 
                       "39" = "35 to 39",
                       "40" = "40 to 44",
                       "41" = "40 to 44",
                       "42" = "40 to 44",
                       "43" = "40 to 44",
                       "44" = "40 to 44",
                       "45" = "45 to 49",
                       "46" = "45 to 49",
                       "47" = "45 to 49",
                       "48" = "45 to 49",
                       "49" = "45 to 49",
                       "50" = "50 to 54",
                       "51" = "50 to 54",
                       "52" = "50 to 54",
                       "53" = "50 to 54",
                       "54" = "50 to 54",
                       "55" = "55 to 59",
                       "56" = "55 to 59",
                       "57" = "55 to 59",
                       "58" = "55 to 59",
                       "59" = "55 to 59",
                       "60" = "60 to 64",
                       "61" = "60 to 64",
                       "62" = "60 to 64",
                       "63" = "60 to 64",
                       "64" = "60 to 64",
                       "65" = "65 to 69",
                       "66" = "65 to 69",
                       "67" = "65 to 69",
                       "68" = "65 to 69",
                       "69" = "65 to 69",
                       "70" = "70 and above",
                       "71" = "70 and above",
                       "72" = "70 and above",
                       "73" = "70 and above",
                       "74" = "70 and above",
                       "75" = "70 and above",
                       "76" = "70 and above",
                       "77" = "70 and above",
                       "78" = "70 and above",
                       "79" = "70 and above",
                       "80" = "70 and above",
                       "81" = "70 and above",
                       "82" = "70 and above",
                       "83" = "70 and above",
                       "84" = "70 and above",
                       "85" = "70 and above")
table(NHIS11$AGE_P)
NHIS11$AGE_P <- as.factor(NHIS11$AGE_P)

#sex
NHIS11$SEX <- recode(NHIS11$SEX,
                     "1" = "Male",
                     "2" = "Female")
table(NHIS11$SEX)
NHIS11$SEX <- as.factor(NHIS11$SEX)

#employment status
NHIS11$DOINGLWA <- recode(NHIS11$DOINGLWA,
                          "1" = "Working for pay at a job/business",
                          "2" = "2",
                          "3" = "3",
                          "4" = "4",
                          "5" = "Not working")
#Change unused values to NA
(NHIS11$DOINGLWA <- unknownToNA(NHIS11$DOINGLWA, unknown = c("9", "2", "3", "4")))
table(NHIS11$DOINGLWA)
NHIS11$DOINGLWA <- as.factor(NHIS11$DOINGLWA)

#affected joing areas quick clean
NHIS11$JMTHP1 <- unknownToNA(NHIS11$JMTHP1, unknown = 7:9)
NHIS11$JMTHP1 <- as.factor(NHIS11$JMTHP1)

NHIS11$JMTHP2 <- unknownToNA(NHIS11$JMTHP2, unknown = 7:9)
NHIS11$JMTHP2 <- as.factor(NHIS11$JMTHP2)

NHIS11$JMTHP3 <- unknownToNA(NHIS11$JMTHP3, unknown = 7:9)
NHIS11$JMTHP3 <- as.factor(NHIS11$JMTHP3)

NHIS11$JMTHP4 <- unknownToNA(NHIS11$JMTHP4, unknown = 7:9)
NHIS11$JMTHP4 <- as.factor(NHIS11$JMTHP4)

NHIS11$JMTHP5 <- unknownToNA(NHIS11$JMTHP5, unknown = 7:9)
NHIS11$JMTHP5 <- as.factor(NHIS11$JMTHP5)

NHIS11$JMTHP6 <- unknownToNA(NHIS11$JMTHP6, unknown = 7:9)
NHIS11$JMTHP6 <- as.factor(NHIS11$JMTHP6)

NHIS11$JMTHP7 <- unknownToNA(NHIS11$JMTHP7, unknown = 7:9)
NHIS11$JMTHP7 <- as.factor(NHIS11$JMTHP7)

NHIS11$JMTHP8 <- unknownToNA(NHIS11$JMTHP8, unknown = 7:9)
NHIS11$JMTHP8 <- as.factor(NHIS11$JMTHP8)

NHIS11$JMTHP9 <- unknownToNA(NHIS11$JMTHP9, unknown = 7:9)
NHIS11$JMTHP9 <- as.factor(NHIS11$JMTHP9)

NHIS11$JMTHP10 <- unknownToNA(NHIS11$JMTHP10, unknown = 7:9)
NHIS11$JMTHP10 <- as.factor(NHIS11$JMTHP10)

NHIS11$JMTHP11 <- unknownToNA(NHIS11$JMTHP11, unknown = 7:9)
NHIS11$JMTHP11 <- as.factor(NHIS11$JMTHP11)

NHIS11$JMTHP12 <- unknownToNA(NHIS11$JMTHP12, unknown = 7:9)
NHIS11$JMTHP12 <- as.factor(NHIS11$JMTHP12)

NHIS11$JMTHP13 <- unknownToNA(NHIS11$JMTHP13, unknown = 7:9)
NHIS11$JMTHP13 <- as.factor(NHIS11$JMTHP13)

NHIS11$JMTHP14 <- unknownToNA(NHIS11$JMTHP14, unknown = 7:9)
NHIS11$JMTHP14 <- as.factor(NHIS11$JMTHP14)

NHIS11$JMTHP15 <- unknownToNA(NHIS11$JMTHP15, unknown = 7:9)
NHIS11$JMTHP15 <- as.factor(NHIS11$JMTHP15)

NHIS11$JMTHP16 <- unknownToNA(NHIS11$JMTHP16, unknown = 7:9)
NHIS11$JMTHP16 <- as.factor(NHIS11$JMTHP16)

NHIS11$JMTHP17 <- unknownToNA(NHIS11$JMTHP17, unknown = 7:9)
NHIS11$JMTHP17 <- as.factor(NHIS11$JMTHP17)

#physical activity
#vigorous PA frequency
table(NHIS11$VIGFREQW)
NHIS11$VIGFREQW <- as.numeric(NHIS11$VIGFREQW)

#vigorous PA duration
table(NHIS11$VIGMIN)
NHIS11$VIGMIN <- as.numeric(NHIS11$VIGMIN)

#moderate pa frequency
table(NHIS11$MODFREQW)
NHIS11$MODFREQW <- as.numeric(NHIS11$MODFREQW)

#moderate PA duration
table(NHIS11$MODMIN)
NHIS11$MODMIN <- as.numeric(NHIS11$MODMIN)

#Make PA
#Physicially active = light/moderate pa >=5x/wk & >=30min OR Vig >=3x/wk & >=20min, else not
NHIS11$PAC <- ifelse(NHIS11$MODFREQW >= 5 & NHIS11$MODFREQW <= 28 & NHIS11$MODMIN >= 30 & NHIS11$MODMIN < 997, "Physically active",
                     ifelse(NHIS11$VIGFREQW >= 3 & NHIS11$VIGFREQW <= 28 & NHIS11$VIGMIN >= 20 & NHIS11$VIGMIN < 997, "Physically active", "Not physically active"))


#BMI
#change all 99.99 (unknown values) to NA
NHIS11$BMI <- unknownToNA(NHIS11$BMI, unknown = c("99.99"))
#quick check to be sure that there are no more potential unknowns
NHIS11 %>% top_n(10, BMI)


#no copd

#ht
NHIS11$HYPEV <- recode(NHIS11$HYPEV,
                       "1" = "1",
                       "2" = "0")
table(NHIS11$HYPEV)
NHIS11$HYPEV <- as.factor(NHIS11$HYPEV)

#________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#for establishing unweighted sample size only, not for design object creation
NHIS11_JS <- subset(NHIS11,
                    !is.na(WTFA_SA) &
                      !is.na(STRAT_P) &
                      !is.na(PSU_P) &
                      !is.na(JNTSYMP))

table(subset(NHIS11_JS, AGE_P == "18 to 24")$SEX)
table(subset(NHIS11_JS, AGE_P == "25 to 29")$SEX)
table(subset(NHIS11_JS, AGE_P == "30 to 34")$SEX)
table(subset(NHIS11_JS, AGE_P == "35 to 39")$SEX)
table(subset(NHIS11_JS, AGE_P == "40 to 44")$SEX)
table(subset(NHIS11_JS, AGE_P == "45 to 49")$SEX)
table(subset(NHIS11_JS, AGE_P == "50 to 54")$SEX)
table(subset(NHIS11_JS, AGE_P == "55 to 59")$SEX)
table(subset(NHIS11_JS, AGE_P == "60 to 64")$SEX)
table(subset(NHIS11_JS, AGE_P == "65 to 69")$SEX)
table(subset(NHIS11_JS, AGE_P == "70 and above")$SEX)


#for establishing unweighted sample size only, not for design object creation
NHIS11_LBP <- subset(NHIS11,
                     !is.na(WTFA_SA) &
                       !is.na(STRAT_P) &
                       !is.na(PSU_P) &
                       !is.na(PAINLB))

table(subset(NHIS11_LBP, AGE_P == "18 to 24")$SEX)
table(subset(NHIS11_LBP, AGE_P == "25 to 29")$SEX)
table(subset(NHIS11_LBP, AGE_P == "30 to 34")$SEX)
table(subset(NHIS11_LBP, AGE_P == "35 to 39")$SEX)
table(subset(NHIS11_LBP, AGE_P == "40 to 44")$SEX)
table(subset(NHIS11_LBP, AGE_P == "45 to 49")$SEX)
table(subset(NHIS11_LBP, AGE_P == "50 to 54")$SEX)
table(subset(NHIS11_LBP, AGE_P == "55 to 59")$SEX)
table(subset(NHIS11_LBP, AGE_P == "60 to 64")$SEX)
table(subset(NHIS11_LBP, AGE_P == "65 to 69")$SEX)
table(subset(NHIS11_LBP, AGE_P == "70 and above")$SEX)


#make sure all complex survey requirements are available
NHIS11_SA_dataset <- subset(NHIS11,
                            !is.na(WTFA_SA) &
                              !is.na(STRAT_P) &
                              !is.na(PSU_P))

#Check that the sum of the weights is equal to the US population
sum(NHIS11_SA_dataset$WTFA_SA)
#The sum of the weights is 231 375 657, which is acceptable

#Check the number of people (unique PSU's)
length(unique(NHIS11_SA_dataset[["PSU_P"]]))
#The number of unique PSU's in the data is 2

#Check the number of unique strata
length(unique(NHIS11_SA_dataset[["STRAT_P"]]))
#The number of unique strata is 300

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHIS11_DO <- svydesign(ids = ~1,
                       weights = ~WTFA_SA,
                       strata = ~STRAT_P,
                       nest = TRUE,
                       data = NHIS11_SA_dataset)

#Save design object as RDS file for ease of retrieval for analysis, thereby avoiding running the data download and the design object creation code multiple times
saveRDS(NHIS11_DO,
        file = "NHIS11_DO.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

#______________________________________________________________________________
#______________________________________________________________________________
#______________________________________________________________________________


# == 2010 == #

#DOWNLOAD

NHIS10.instructions <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Program_Code/NHIS/2010/samadult.sas"
NHIS10.fileloc <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/2010/samadult.zip"
#store as df
NHIS10_SA <- read.SAScii(NHIS10.fileloc, NHIS10.instructions, zipped = TRUE)
#save as RDS file
saveRDS(NHIS10_SA,
        file = "NHIS10_SA.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

#CLEANING

#Some exploration
str(NHIS10_SA)
tail(NHIS10_SA)
glimpse(NHIS10_SA)
colnames(NHIS10_SA)

#Select variables
NHIS10 <- select(NHIS10_SA,
                 HHX, FMX, FPX,
                 REGION,
                 STRAT_P, PSU_P, SRVY_YR, WTFA_SA,
                 SEX, AGE_P,
                 DOINGLWA,
                 JNTSYMP, JMTHP1, JMTHP2, JMTHP3, JMTHP4, JMTHP5, JMTHP6, JMTHP7, JMTHP8, JMTHP9, JMTHP10, JMTHP11, JMTHP12, JMTHP13, JMTHP14, JMTHP15, JMTHP16, JMTHP17,
                 PAINLB,
                 VIGFREQW, VIGMIN, MODFREQW, MODMIN,
                 BMI,
                 HYPEV)

str(NHIS10)
tail(NHIS10)
glimpse(NHIS10)
colnames(NHIS10)

#joint symptoms recode
table(NHIS10$JNTSYMP)
#recode
NHIS10$JNTSYMP <- recode(NHIS10$JNTSYMP,
                         "1" = "1",
                         "2" = "0",
                         "7" = "7",
                         "9" = "9")
(NHIS10$JNTSYMP <- unknownToNA(NHIS10$JNTSYMP, unknown = c("7", "9")))
table(NHIS10$JNTSYMP)
NHIS10$JNTSYMP <- as.factor(NHIS10$JNTSYMP)

#low back pain recode
table(NHIS10$PAINLB)
#recode
NHIS10$PAINLB <- recode(NHIS10$PAINLB,
                        "1" = "1",
                        "2" = "0")
table(NHIS10$PAINLB)
NHIS10$PAINLB <- as.factor(NHIS10$PAINLB)

#region
table(NHIS10$REGION)
NHIS10$REGION <- recode(NHIS10$REGION,
                        "1" = "Northeast",
                        "2" = "Midwest",
                        "3" = "South",
                        "4" = "West")
table(NHIS10$REGION)

#age
table(NHIS10$AGE_P)


NHIS10$AGE_P <- recode(NHIS10$AGE_P,
                       "18" = "18 to 24",
                       "19" = "18 to 24",
                       "20" = "18 to 24",
                       "21" = "18 to 24",
                       "22" = "18 to 24",
                       "23" = "18 to 24",
                       "24" = "18 to 24",
                       "25" = "25 to 29",
                       "26" = "25 to 29",
                       "27" = "25 to 29",
                       "28" = "25 to 29",
                       "29" = "25 to 29",
                       "30" = "30 to 34",
                       "31" = "30 to 34",
                       "32" = "30 to 34",
                       "33" = "30 to 34",
                       "34" = "30 to 34",
                       "35" = "35 to 39",
                       "36" = "35 to 39",
                       "37" = "35 to 39",
                       "38" = "35 to 39", 
                       "39" = "35 to 39",
                       "40" = "40 to 44",
                       "41" = "40 to 44",
                       "42" = "40 to 44",
                       "43" = "40 to 44",
                       "44" = "40 to 44",
                       "45" = "45 to 49",
                       "46" = "45 to 49",
                       "47" = "45 to 49",
                       "48" = "45 to 49",
                       "49" = "45 to 49",
                       "50" = "50 to 54",
                       "51" = "50 to 54",
                       "52" = "50 to 54",
                       "53" = "50 to 54",
                       "54" = "50 to 54",
                       "55" = "55 to 59",
                       "56" = "55 to 59",
                       "57" = "55 to 59",
                       "58" = "55 to 59",
                       "59" = "55 to 59",
                       "60" = "60 to 64",
                       "61" = "60 to 64",
                       "62" = "60 to 64",
                       "63" = "60 to 64",
                       "64" = "60 to 64",
                       "65" = "65 to 69",
                       "66" = "65 to 69",
                       "67" = "65 to 69",
                       "68" = "65 to 69",
                       "69" = "65 to 69",
                       "70" = "70 and above",
                       "71" = "70 and above",
                       "72" = "70 and above",
                       "73" = "70 and above",
                       "74" = "70 and above",
                       "75" = "70 and above",
                       "76" = "70 and above",
                       "77" = "70 and above",
                       "78" = "70 and above",
                       "79" = "70 and above",
                       "80" = "70 and above",
                       "81" = "70 and above",
                       "82" = "70 and above",
                       "83" = "70 and above",
                       "84" = "70 and above",
                       "85" = "70 and above")
table(NHIS10$AGE_P)
NHIS10$AGE_P <- as.factor(NHIS10$AGE_P)

#sex
NHIS10$SEX <- recode(NHIS10$SEX,
                     "1" = "Male",
                     "2" = "Female")
table(NHIS10$SEX)
NHIS10$SEX <- as.factor(NHIS10$SEX)

#employment status
NHIS10$DOINGLWA <- recode(NHIS10$DOINGLWA,
                          "1" = "Working for pay at a job/business",
                          "2" = "2",
                          "3" = "3",
                          "4" = "4",
                          "5" = "Not working")
#Change unused values to NA
(NHIS10$DOINGLWA <- unknownToNA(NHIS10$DOINGLWA, unknown = c("9", "2", "3", "4")))
table(NHIS10$DOINGLWA)
NHIS10$DOINGLWA <- as.factor(NHIS10$DOINGLWA)

#affected joing areas quick clean
NHIS10$JMTHP1 <- unknownToNA(NHIS10$JMTHP1, unknown = 7:9)
NHIS10$JMTHP1 <- as.factor(NHIS10$JMTHP1)

NHIS10$JMTHP2 <- unknownToNA(NHIS10$JMTHP2, unknown = 7:9)
NHIS10$JMTHP2 <- as.factor(NHIS10$JMTHP2)

NHIS10$JMTHP3 <- unknownToNA(NHIS10$JMTHP3, unknown = 7:9)
NHIS10$JMTHP3 <- as.factor(NHIS10$JMTHP3)

NHIS10$JMTHP4 <- unknownToNA(NHIS10$JMTHP4, unknown = 7:9)
NHIS10$JMTHP4 <- as.factor(NHIS10$JMTHP4)

NHIS10$JMTHP5 <- unknownToNA(NHIS10$JMTHP5, unknown = 7:9)
NHIS10$JMTHP5 <- as.factor(NHIS10$JMTHP5)

NHIS10$JMTHP6 <- unknownToNA(NHIS10$JMTHP6, unknown = 7:9)
NHIS10$JMTHP6 <- as.factor(NHIS10$JMTHP6)

NHIS10$JMTHP7 <- unknownToNA(NHIS10$JMTHP7, unknown = 7:9)
NHIS10$JMTHP7 <- as.factor(NHIS10$JMTHP7)

NHIS10$JMTHP8 <- unknownToNA(NHIS10$JMTHP8, unknown = 7:9)
NHIS10$JMTHP8 <- as.factor(NHIS10$JMTHP8)

NHIS10$JMTHP9 <- unknownToNA(NHIS10$JMTHP9, unknown = 7:9)
NHIS10$JMTHP9 <- as.factor(NHIS10$JMTHP9)

NHIS10$JMTHP10 <- unknownToNA(NHIS10$JMTHP10, unknown = 7:9)
NHIS10$JMTHP10 <- as.factor(NHIS10$JMTHP10)

NHIS10$JMTHP11 <- unknownToNA(NHIS10$JMTHP11, unknown = 7:9)
NHIS10$JMTHP11 <- as.factor(NHIS10$JMTHP11)

NHIS10$JMTHP12 <- unknownToNA(NHIS10$JMTHP12, unknown = 7:9)
NHIS10$JMTHP12 <- as.factor(NHIS10$JMTHP12)

NHIS10$JMTHP13 <- unknownToNA(NHIS10$JMTHP13, unknown = 7:9)
NHIS10$JMTHP13 <- as.factor(NHIS10$JMTHP13)

NHIS10$JMTHP14 <- unknownToNA(NHIS10$JMTHP14, unknown = 7:9)
NHIS10$JMTHP14 <- as.factor(NHIS10$JMTHP14)

NHIS10$JMTHP15 <- unknownToNA(NHIS10$JMTHP15, unknown = 7:9)
NHIS10$JMTHP15 <- as.factor(NHIS10$JMTHP15)

NHIS10$JMTHP16 <- unknownToNA(NHIS10$JMTHP16, unknown = 7:9)
NHIS10$JMTHP16 <- as.factor(NHIS10$JMTHP16)

NHIS10$JMTHP17 <- unknownToNA(NHIS10$JMTHP17, unknown = 7:9)
NHIS10$JMTHP17 <- as.factor(NHIS10$JMTHP17)

#physical activity
#vigorous PA frequency
table(NHIS10$VIGFREQW)
NHIS10$VIGFREQW <- as.numeric(NHIS10$VIGFREQW)

#vigorous PA duration
table(NHIS10$VIGMIN)
NHIS10$VIGMIN <- as.numeric(NHIS10$VIGMIN)

#moderate pa frequency
table(NHIS10$MODFREQW)
NHIS10$MODFREQW <- as.numeric(NHIS10$MODFREQW)

#moderate PA duration
table(NHIS10$MODMIN)
NHIS10$MODMIN <- as.numeric(NHIS10$MODMIN)

#Make PA
#Physicially active = light/moderate pa >=5x/wk & >=30min OR Vig >=3x/wk & >=20min, else not
NHIS10$PAC <- ifelse(NHIS10$MODFREQW >= 5 & NHIS10$MODFREQW <= 28 & NHIS10$MODMIN >= 30 & NHIS10$MODMIN < 997, "Physically active",
                     ifelse(NHIS10$VIGFREQW >= 3 & NHIS10$VIGFREQW <= 28 & NHIS10$VIGMIN >= 20 & NHIS10$VIGMIN < 997, "Physically active", "Not physically active"))


#BMI
#change all 99.99 (unknown values) to NA
NHIS10$BMI <- unknownToNA(NHIS10$BMI, unknown = c("99.99"))
#quick check to be sure that there are no more potential unknowns
NHIS10 %>% top_n(10, BMI)


#no copd

#ht
NHIS10$HYPEV <- recode(NHIS10$HYPEV,
                       "1" = "1",
                       "2" = "0")
table(NHIS10$HYPEV)
NHIS10$HYPEV <- as.factor(NHIS10$HYPEV)

#________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#for establishing unweighted sample size only, not for design object creation
NHIS10_JS <- subset(NHIS10,
                    !is.na(WTFA_SA) &
                      !is.na(STRAT_P) &
                      !is.na(PSU_P) &
                      !is.na(JNTSYMP))

table(subset(NHIS10_JS, AGE_P == "18 to 24")$SEX)
table(subset(NHIS10_JS, AGE_P == "25 to 29")$SEX)
table(subset(NHIS10_JS, AGE_P == "30 to 34")$SEX)
table(subset(NHIS10_JS, AGE_P == "35 to 39")$SEX)
table(subset(NHIS10_JS, AGE_P == "40 to 44")$SEX)
table(subset(NHIS10_JS, AGE_P == "45 to 49")$SEX)
table(subset(NHIS10_JS, AGE_P == "50 to 54")$SEX)
table(subset(NHIS10_JS, AGE_P == "55 to 59")$SEX)
table(subset(NHIS10_JS, AGE_P == "60 to 64")$SEX)
table(subset(NHIS10_JS, AGE_P == "65 to 69")$SEX)
table(subset(NHIS10_JS, AGE_P == "70 and above")$SEX)


#for establishing unweighted sample size only, not for design object creation
NHIS10_LBP <- subset(NHIS10,
                     !is.na(WTFA_SA) &
                       !is.na(STRAT_P) &
                       !is.na(PSU_P) &
                       !is.na(PAINLB))

table(subset(NHIS10_LBP, AGE_P == "18 to 24")$SEX)
table(subset(NHIS10_LBP, AGE_P == "25 to 29")$SEX)
table(subset(NHIS10_LBP, AGE_P == "30 to 34")$SEX)
table(subset(NHIS10_LBP, AGE_P == "35 to 39")$SEX)
table(subset(NHIS10_LBP, AGE_P == "40 to 44")$SEX)
table(subset(NHIS10_LBP, AGE_P == "45 to 49")$SEX)
table(subset(NHIS10_LBP, AGE_P == "50 to 54")$SEX)
table(subset(NHIS10_LBP, AGE_P == "55 to 59")$SEX)
table(subset(NHIS10_LBP, AGE_P == "60 to 64")$SEX)
table(subset(NHIS10_LBP, AGE_P == "65 to 69")$SEX)
table(subset(NHIS10_LBP, AGE_P == "70 and above")$SEX)

#make sure all complex survey requirements are available
NHIS10_SA_dataset <- subset(NHIS10,
                            !is.na(WTFA_SA) &
                              !is.na(STRAT_P) &
                              !is.na(PSU_P))

#Check that the sum of the weights is equal to the US population
sum(NHIS10_SA_dataset$WTFA_SA)
#The sum of the weights is 229 505 094, which is acceptable

#Check the number of people (unique PSU's)
length(unique(NHIS10_SA_dataset[["PSU_P"]]))
#The number of unique PSU's in the data is 2

#Check the number of unique strata
length(unique(NHIS10_SA_dataset[["STRAT_P"]]))
#The number of unique strata is 300

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHIS10_DO <- svydesign(ids = ~1,
                       weights = ~WTFA_SA,
                       strata = ~STRAT_P,
                       nest = TRUE,
                       data = NHIS10_SA_dataset)

#Save design object as RDS file for ease of retrieval for analysis, thereby avoiding running the data download and the design object creation code multiple times
saveRDS(NHIS10_DO,
        file = "NHIS10_DO.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

#_________________________________________________________________________________
#_________________________________________________________________________________
#_________________________________________________________________________________


# == 2009 == #

#DOWNLOAD

NHIS09.instructions <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Program_Code/NHIS/2009/samadult.sas"
NHIS09.fileloc <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/2009/samadult.exe"
#store as df
NHIS09_SA <- read.SAScii(NHIS09.fileloc, NHIS09.instructions, zipped = TRUE)
#save as RDS file
saveRDS(NHIS09_SA,
        file = "NHIS09_SA.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

#CLEANING

#Some exploration
str(NHIS09_SA)
tail(NHIS09_SA)
glimpse(NHIS09_SA)
colnames(NHIS09_SA)

#Select variables
NHIS09 <- select(NHIS09_SA,
                 HHX, FMX, FPX,
                 REGION,
                 STRAT_P, PSU_P, SRVY_YR, WTFA_SA,
                 SEX, AGE_P,
                 DOINGLWA,
                 JNTSYMP, JMTHP1, JMTHP2, JMTHP3, JMTHP4, JMTHP5, JMTHP6, JMTHP7, JMTHP8, JMTHP9, JMTHP10, JMTHP11, JMTHP12, JMTHP13, JMTHP14, JMTHP15, JMTHP16, JMTHP17,
                 PAINLB,
                 VIGFREQW, VIGMIN, MODFREQW, MODMIN,
                 BMI,
                 HYPEV)

str(NHIS09)
tail(NHIS09)
glimpse(NHIS09)
colnames(NHIS09)

#joint symptoms recode
table(NHIS09$JNTSYMP)
#recode
NHIS09$JNTSYMP <- recode(NHIS09$JNTSYMP,
                         "1" = "1",
                         "2" = "0",
                         "7" = "7",
                         "9" = "9")
(NHIS09$JNTSYMP <- unknownToNA(NHIS09$JNTSYMP, unknown = c("7", "9")))
table(NHIS09$JNTSYMP)
NHIS09$JNTSYMP <- as.factor(NHIS09$JNTSYMP)

#low back pain recode
table(NHIS09$PAINLB)
#recode
NHIS09$PAINLB <- recode(NHIS09$PAINLB,
                        "1" = "1",
                        "2" = "0")
table(NHIS09$PAINLB)
NHIS09$PAINLB <- as.factor(NHIS09$PAINLB)

#region
table(NHIS09$REGION)
NHIS09$REGION <- recode(NHIS09$REGION,
                        "1" = "Northeast",
                        "2" = "Midwest",
                        "3" = "South",
                        "4" = "West")
table(NHIS09$REGION)

#age
table(NHIS09$AGE_P)



NHIS09$AGE_P <- recode(NHIS09$AGE_P,
                       "18" = "18 to 24",
                       "19" = "18 to 24",
                       "20" = "18 to 24",
                       "21" = "18 to 24",
                       "22" = "18 to 24",
                       "23" = "18 to 24",
                       "24" = "18 to 24",
                       "25" = "25 to 29",
                       "26" = "25 to 29",
                       "27" = "25 to 29",
                       "28" = "25 to 29",
                       "29" = "25 to 29",
                       "30" = "30 to 34",
                       "31" = "30 to 34",
                       "32" = "30 to 34",
                       "33" = "30 to 34",
                       "34" = "30 to 34",
                       "35" = "35 to 39",
                       "36" = "35 to 39",
                       "37" = "35 to 39",
                       "38" = "35 to 39", 
                       "39" = "35 to 39",
                       "40" = "40 to 44",
                       "41" = "40 to 44",
                       "42" = "40 to 44",
                       "43" = "40 to 44",
                       "44" = "40 to 44",
                       "45" = "45 to 49",
                       "46" = "45 to 49",
                       "47" = "45 to 49",
                       "48" = "45 to 49",
                       "49" = "45 to 49",
                       "50" = "50 to 54",
                       "51" = "50 to 54",
                       "52" = "50 to 54",
                       "53" = "50 to 54",
                       "54" = "50 to 54",
                       "55" = "55 to 59",
                       "56" = "55 to 59",
                       "57" = "55 to 59",
                       "58" = "55 to 59",
                       "59" = "55 to 59",
                       "60" = "60 to 64",
                       "61" = "60 to 64",
                       "62" = "60 to 64",
                       "63" = "60 to 64",
                       "64" = "60 to 64",
                       "65" = "65 to 69",
                       "66" = "65 to 69",
                       "67" = "65 to 69",
                       "68" = "65 to 69",
                       "69" = "65 to 69",
                       "70" = "70 and above",
                       "71" = "70 and above",
                       "72" = "70 and above",
                       "73" = "70 and above",
                       "74" = "70 and above",
                       "75" = "70 and above",
                       "76" = "70 and above",
                       "77" = "70 and above",
                       "78" = "70 and above",
                       "79" = "70 and above",
                       "80" = "70 and above",
                       "81" = "70 and above",
                       "82" = "70 and above",
                       "83" = "70 and above",
                       "84" = "70 and above",
                       "85" = "70 and above")
table(NHIS09$AGE_P)
NHIS09$AGE_P <- as.factor(NHIS09$AGE_P)

#sex
NHIS09$SEX <- recode(NHIS09$SEX,
                     "1" = "Male",
                     "2" = "Female")
table(NHIS09$SEX)
NHIS09$SEX <- as.factor(NHIS09$SEX)

#employment status
NHIS09$DOINGLWA <- recode(NHIS09$DOINGLWA,
                          "1" = "Working for pay at a job/business",
                          "2" = "2",
                          "3" = "3",
                          "4" = "4",
                          "5" = "Not working")
#Change unused values to NA
(NHIS09$DOINGLWA <- unknownToNA(NHIS09$DOINGLWA, unknown = c("9", "2", "3", "4")))
table(NHIS09$DOINGLWA)
NHIS09$DOINGLWA <- as.factor(NHIS09$DOINGLWA)

#affected joing areas quick clean
NHIS09$JMTHP1 <- unknownToNA(NHIS09$JMTHP1, unknown = 7:9)
NHIS09$JMTHP1 <- as.factor(NHIS09$JMTHP1)

NHIS09$JMTHP2 <- unknownToNA(NHIS09$JMTHP2, unknown = 7:9)
NHIS09$JMTHP2 <- as.factor(NHIS09$JMTHP2)

NHIS09$JMTHP3 <- unknownToNA(NHIS09$JMTHP3, unknown = 7:9)
NHIS09$JMTHP3 <- as.factor(NHIS09$JMTHP3)

NHIS09$JMTHP4 <- unknownToNA(NHIS09$JMTHP4, unknown = 7:9)
NHIS09$JMTHP4 <- as.factor(NHIS09$JMTHP4)

NHIS09$JMTHP5 <- unknownToNA(NHIS09$JMTHP5, unknown = 7:9)
NHIS09$JMTHP5 <- as.factor(NHIS09$JMTHP5)

NHIS09$JMTHP6 <- unknownToNA(NHIS09$JMTHP6, unknown = 7:9)
NHIS09$JMTHP6 <- as.factor(NHIS09$JMTHP6)

NHIS09$JMTHP7 <- unknownToNA(NHIS09$JMTHP7, unknown = 7:9)
NHIS09$JMTHP7 <- as.factor(NHIS09$JMTHP7)

NHIS09$JMTHP8 <- unknownToNA(NHIS09$JMTHP8, unknown = 7:9)
NHIS09$JMTHP8 <- as.factor(NHIS09$JMTHP8)

NHIS09$JMTHP9 <- unknownToNA(NHIS09$JMTHP9, unknown = 7:9)
NHIS09$JMTHP9 <- as.factor(NHIS09$JMTHP9)

NHIS09$JMTHP10 <- unknownToNA(NHIS09$JMTHP10, unknown = 7:9)
NHIS09$JMTHP10 <- as.factor(NHIS09$JMTHP10)

NHIS09$JMTHP11 <- unknownToNA(NHIS09$JMTHP11, unknown = 7:9)
NHIS09$JMTHP11 <- as.factor(NHIS09$JMTHP11)

NHIS09$JMTHP12 <- unknownToNA(NHIS09$JMTHP12, unknown = 7:9)
NHIS09$JMTHP12 <- as.factor(NHIS09$JMTHP12)

NHIS09$JMTHP13 <- unknownToNA(NHIS09$JMTHP13, unknown = 7:9)
NHIS09$JMTHP13 <- as.factor(NHIS09$JMTHP13)

NHIS09$JMTHP14 <- unknownToNA(NHIS09$JMTHP14, unknown = 7:9)
NHIS09$JMTHP14 <- as.factor(NHIS09$JMTHP14)

NHIS09$JMTHP15 <- unknownToNA(NHIS09$JMTHP15, unknown = 7:9)
NHIS09$JMTHP15 <- as.factor(NHIS09$JMTHP15)

NHIS09$JMTHP16 <- unknownToNA(NHIS09$JMTHP16, unknown = 7:9)
NHIS09$JMTHP16 <- as.factor(NHIS09$JMTHP16)

NHIS09$JMTHP17 <- unknownToNA(NHIS09$JMTHP17, unknown = 7:9)
NHIS09$JMTHP17 <- as.factor(NHIS09$JMTHP17)

#physical activity
#vigorous PA frequency
table(NHIS09$VIGFREQW)
NHIS09$VIGFREQW <- as.numeric(NHIS09$VIGFREQW)

#vigorous PA duration
table(NHIS09$VIGMIN)
NHIS09$VIGMIN <- as.numeric(NHIS09$VIGMIN)

#moderate pa frequency
table(NHIS09$MODFREQW)
NHIS09$MODFREQW <- as.numeric(NHIS09$MODFREQW)

#moderate PA duration
table(NHIS09$MODMIN)
NHIS09$MODMIN <- as.numeric(NHIS09$MODMIN)

#Make PA
#Physicially active = light/moderate pa >=5x/wk & >=30min OR Vig >=3x/wk & >=20min, else not
NHIS09$PAC <- ifelse(NHIS09$MODFREQW >= 5 & NHIS09$MODFREQW <= 28 & NHIS09$MODMIN >= 30 & NHIS09$MODMIN < 997, "Physically active",
                     ifelse(NHIS09$VIGFREQW >= 3 & NHIS09$VIGFREQW <= 28 & NHIS09$VIGMIN >= 20 & NHIS09$VIGMIN < 997, "Physically active", "Not physically active"))

#BMI
#change all 99.99 (unknown values) to NA
NHIS09$BMI <- unknownToNA(NHIS09$BMI, unknown = c("99.99"))
#quick check to be sure that there are no more potential unknowns
NHIS09 %>% top_n(10, BMI)


#no copd

#ht
NHIS09$HYPEV <- recode(NHIS09$HYPEV,
                       "1" = "1",
                       "2" = "0")
table(NHIS09$HYPEV)
NHIS09$HYPEV <- as.factor(NHIS09$HYPEV)

#________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#for establishing unweighted sample size only, not for design object creation
NHIS09_JS <- subset(NHIS09,
                    !is.na(WTFA_SA) &
                      !is.na(STRAT_P) &
                      !is.na(PSU_P) &
                      !is.na(JNTSYMP))

table(subset(NHIS09_JS, AGE_P == "18 to 24")$SEX)
table(subset(NHIS09_JS, AGE_P == "25 to 29")$SEX)
table(subset(NHIS09_JS, AGE_P == "30 to 34")$SEX)
table(subset(NHIS09_JS, AGE_P == "35 to 39")$SEX)
table(subset(NHIS09_JS, AGE_P == "40 to 44")$SEX)
table(subset(NHIS09_JS, AGE_P == "45 to 49")$SEX)
table(subset(NHIS09_JS, AGE_P == "50 to 54")$SEX)
table(subset(NHIS09_JS, AGE_P == "55 to 59")$SEX)
table(subset(NHIS09_JS, AGE_P == "60 to 64")$SEX)
table(subset(NHIS09_JS, AGE_P == "65 to 69")$SEX)
table(subset(NHIS09_JS, AGE_P == "70 and above")$SEX)


#for establishing unweighted sample size only, not for design object creation
NHIS09_LBP <- subset(NHIS09,
                     !is.na(WTFA_SA) &
                       !is.na(STRAT_P) &
                       !is.na(PSU_P) &
                       !is.na(PAINLB))

table(subset(NHIS09_LBP, AGE_P == "18 to 24")$SEX)
table(subset(NHIS09_LBP, AGE_P == "25 to 29")$SEX)
table(subset(NHIS09_LBP, AGE_P == "30 to 34")$SEX)
table(subset(NHIS09_LBP, AGE_P == "35 to 39")$SEX)
table(subset(NHIS09_LBP, AGE_P == "40 to 44")$SEX)
table(subset(NHIS09_LBP, AGE_P == "45 to 49")$SEX)
table(subset(NHIS09_LBP, AGE_P == "50 to 54")$SEX)
table(subset(NHIS09_LBP, AGE_P == "55 to 59")$SEX)
table(subset(NHIS09_LBP, AGE_P == "60 to 64")$SEX)
table(subset(NHIS09_LBP, AGE_P == "65 to 69")$SEX)
table(subset(NHIS09_LBP, AGE_P == "70 and above")$SEX)

#make sure all complex survey requirements are available
NHIS09_SA_dataset <- subset(NHIS09,
                            !is.na(WTFA_SA) &
                              !is.na(STRAT_P) &
                              !is.na(PSU_P))

#Check that the sum of the weights is equal to the US population
sum(NHIS09_SA_dataset$WTFA_SA)
#The sum of the weights is 227 371 068, which is acceptable

#Check the number of people (unique PSU's)
length(unique(NHIS09_SA_dataset[["PSU_P"]]))
#The number of unique PSU's in the data is 2

#Check the number of unique strata
length(unique(NHIS09_SA_dataset[["STRAT_P"]]))
#The number of unique strata is 300

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHIS09_DO <- svydesign(ids = ~1,
                       weights = ~WTFA_SA,
                       strata = ~STRAT_P,
                       nest = TRUE,
                       data = NHIS09_SA_dataset)

#Save design object as RDS file for ease of retrieval for analysis, thereby avoiding running the data download and the design object creation code multiple times
saveRDS(NHIS09_DO,
        file = "NHIS09_DO.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

#_______________________________________________________________________________
#_______________________________________________________________________________
#_______________________________________________________________________________

# == 2008 == #

#DOWNLOAD

NHIS08.instructions <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Program_Code/NHIS/2008/samadult.sas"
NHIS08.fileloc <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/2008/samadult.exe"
#store as df
NHIS08_SA <- read.SAScii(NHIS08.fileloc, NHIS08.instructions, zipped = TRUE)
#save as RDS file
saveRDS(NHIS08_SA,
        file = "NHIS08_SA.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

#CLEANING

#Some exploration
str(NHIS08_SA)
tail(NHIS08_SA)
glimpse(NHIS08_SA)
colnames(NHIS08_SA)

#Select variables
NHIS08 <- select(NHIS08_SA,
                 HHX, FMX, FPX,
                 REGION,
                 STRAT_P, PSU_P, SRVY_YR, WTFA_SA,
                 SEX, AGE_P,
                 DOINGLWA,
                 JNTSYMP, JMTHP1, JMTHP2, JMTHP3, JMTHP4, JMTHP5, JMTHP6, JMTHP7, JMTHP8, JMTHP9, JMTHP10, JMTHP11, JMTHP12, JMTHP13, JMTHP14, JMTHP15, JMTHP16, JMTHP17,
                 PAINLB,
                 VIGFREQW, VIGMIN, MODFREQW, MODMIN,
                 BMI,
                 HYPEV)

str(NHIS08)
tail(NHIS08)
glimpse(NHIS08)
colnames(NHIS08)

#joint symptoms recode
table(NHIS08$JNTSYMP)
#recode
NHIS08$JNTSYMP <- recode(NHIS08$JNTSYMP,
                         "1" = "1",
                         "2" = "0",
                         "7" = "7",
                         "9" = "9")
(NHIS08$JNTSYMP <- unknownToNA(NHIS08$JNTSYMP, unknown = c("7", "9")))
table(NHIS08$JNTSYMP)
NHIS08$JNTSYMP <- as.factor(NHIS08$JNTSYMP)

#low back pain recode
table(NHIS08$PAINLB)
#recode
NHIS08$PAINLB <- recode(NHIS08$PAINLB,
                        "1" = "1",
                        "2" = "0")
table(NHIS08$PAINLB)
NHIS08$PAINLB <- as.factor(NHIS08$PAINLB)

#region
table(NHIS08$REGION)
NHIS08$REGION <- recode(NHIS08$REGION,
                        "1" = "Northeast",
                        "2" = "Midwest",
                        "3" = "South",
                        "4" = "West")
table(NHIS08$REGION)

#age
table(NHIS08$AGE_P)



NHIS08$AGE_P <- recode(NHIS08$AGE_P,
                       "18" = "18 to 24",
                       "19" = "18 to 24",
                       "20" = "18 to 24",
                       "21" = "18 to 24",
                       "22" = "18 to 24",
                       "23" = "18 to 24",
                       "24" = "18 to 24",
                       "25" = "25 to 29",
                       "26" = "25 to 29",
                       "27" = "25 to 29",
                       "28" = "25 to 29",
                       "29" = "25 to 29",
                       "30" = "30 to 34",
                       "31" = "30 to 34",
                       "32" = "30 to 34",
                       "33" = "30 to 34",
                       "34" = "30 to 34",
                       "35" = "35 to 39",
                       "36" = "35 to 39",
                       "37" = "35 to 39",
                       "38" = "35 to 39", 
                       "39" = "35 to 39",
                       "40" = "40 to 44",
                       "41" = "40 to 44",
                       "42" = "40 to 44",
                       "43" = "40 to 44",
                       "44" = "40 to 44",
                       "45" = "45 to 49",
                       "46" = "45 to 49",
                       "47" = "45 to 49",
                       "48" = "45 to 49",
                       "49" = "45 to 49",
                       "50" = "50 to 54",
                       "51" = "50 to 54",
                       "52" = "50 to 54",
                       "53" = "50 to 54",
                       "54" = "50 to 54",
                       "55" = "55 to 59",
                       "56" = "55 to 59",
                       "57" = "55 to 59",
                       "58" = "55 to 59",
                       "59" = "55 to 59",
                       "60" = "60 to 64",
                       "61" = "60 to 64",
                       "62" = "60 to 64",
                       "63" = "60 to 64",
                       "64" = "60 to 64",
                       "65" = "65 to 69",
                       "66" = "65 to 69",
                       "67" = "65 to 69",
                       "68" = "65 to 69",
                       "69" = "65 to 69",
                       "70" = "70 and above",
                       "71" = "70 and above",
                       "72" = "70 and above",
                       "73" = "70 and above",
                       "74" = "70 and above",
                       "75" = "70 and above",
                       "76" = "70 and above",
                       "77" = "70 and above",
                       "78" = "70 and above",
                       "79" = "70 and above",
                       "80" = "70 and above",
                       "81" = "70 and above",
                       "82" = "70 and above",
                       "83" = "70 and above",
                       "84" = "70 and above",
                       "85" = "70 and above")
table(NHIS08$AGE_P)
NHIS08$AGE_P <- as.factor(NHIS08$AGE_P)

#sex
NHIS08$SEX <- recode(NHIS08$SEX,
                     "1" = "Male",
                     "2" = "Female")
table(NHIS08$SEX)
NHIS08$SEX <- as.factor(NHIS08$SEX)

#employment status
NHIS08$DOINGLWA <- recode(NHIS08$DOINGLWA,
                          "1" = "Working for pay at a job/business",
                          "2" = "2",
                          "3" = "3",
                          "4" = "4",
                          "5" = "Not working")
#Change unused values to NA
(NHIS08$DOINGLWA <- unknownToNA(NHIS08$DOINGLWA, unknown = c("9", "2", "3", "4")))
table(NHIS08$DOINGLWA)
NHIS08$DOINGLWA <- as.factor(NHIS08$DOINGLWA)

#affected joing areas quick clean
NHIS08$JMTHP1 <- unknownToNA(NHIS08$JMTHP1, unknown = 7:9)
NHIS08$JMTHP1 <- as.factor(NHIS08$JMTHP1)

NHIS08$JMTHP2 <- unknownToNA(NHIS08$JMTHP2, unknown = 7:9)
NHIS08$JMTHP2 <- as.factor(NHIS08$JMTHP2)

NHIS08$JMTHP3 <- unknownToNA(NHIS08$JMTHP3, unknown = 7:9)
NHIS08$JMTHP3 <- as.factor(NHIS08$JMTHP3)

NHIS08$JMTHP4 <- unknownToNA(NHIS08$JMTHP4, unknown = 7:9)
NHIS08$JMTHP4 <- as.factor(NHIS08$JMTHP4)

NHIS08$JMTHP5 <- unknownToNA(NHIS08$JMTHP5, unknown = 7:9)
NHIS08$JMTHP5 <- as.factor(NHIS08$JMTHP5)

NHIS08$JMTHP6 <- unknownToNA(NHIS08$JMTHP6, unknown = 7:9)
NHIS08$JMTHP6 <- as.factor(NHIS08$JMTHP6)

NHIS08$JMTHP7 <- unknownToNA(NHIS08$JMTHP7, unknown = 7:9)
NHIS08$JMTHP7 <- as.factor(NHIS08$JMTHP7)

NHIS08$JMTHP8 <- unknownToNA(NHIS08$JMTHP8, unknown = 7:9)
NHIS08$JMTHP8 <- as.factor(NHIS08$JMTHP8)

NHIS08$JMTHP9 <- unknownToNA(NHIS08$JMTHP9, unknown = 7:9)
NHIS08$JMTHP9 <- as.factor(NHIS08$JMTHP9)

NHIS08$JMTHP10 <- unknownToNA(NHIS08$JMTHP10, unknown = 7:9)
NHIS08$JMTHP10 <- as.factor(NHIS08$JMTHP10)

NHIS08$JMTHP11 <- unknownToNA(NHIS08$JMTHP11, unknown = 7:9)
NHIS08$JMTHP11 <- as.factor(NHIS08$JMTHP11)

NHIS08$JMTHP12 <- unknownToNA(NHIS08$JMTHP12, unknown = 7:9)
NHIS08$JMTHP12 <- as.factor(NHIS08$JMTHP12)

NHIS08$JMTHP13 <- unknownToNA(NHIS08$JMTHP13, unknown = 7:9)
NHIS08$JMTHP13 <- as.factor(NHIS08$JMTHP13)

NHIS08$JMTHP14 <- unknownToNA(NHIS08$JMTHP14, unknown = 7:9)
NHIS08$JMTHP14 <- as.factor(NHIS08$JMTHP14)

NHIS08$JMTHP15 <- unknownToNA(NHIS08$JMTHP15, unknown = 7:9)
NHIS08$JMTHP15 <- as.factor(NHIS08$JMTHP15)

NHIS08$JMTHP16 <- unknownToNA(NHIS08$JMTHP16, unknown = 7:9)
NHIS08$JMTHP16 <- as.factor(NHIS08$JMTHP16)

NHIS08$JMTHP17 <- unknownToNA(NHIS08$JMTHP17, unknown = 7:9)
NHIS08$JMTHP17 <- as.factor(NHIS08$JMTHP17)

#physical activity
#vigorous PA frequency
table(NHIS08$VIGFREQW)
NHIS08$VIGFREQW <- as.numeric(NHIS08$VIGFREQW)

#vigorous PA duration
table(NHIS08$VIGMIN)
NHIS08$VIGMIN <- as.numeric(NHIS08$VIGMIN)

#moderate pa frequency
table(NHIS08$MODFREQW)
NHIS08$MODFREQW <- as.numeric(NHIS08$MODFREQW)

#moderate PA duration
table(NHIS08$MODMIN)
NHIS08$MODMIN <- as.numeric(NHIS08$MODMIN)

#Make PA
#Physicially active = light/moderate pa >=5x/wk & >=30min OR Vig >=3x/wk & >=20min, else not
NHIS08$PAC <- ifelse(NHIS08$MODFREQW >= 5 & NHIS08$MODFREQW <= 28 & NHIS08$MODMIN >= 30 & NHIS08$MODMIN < 997, "Physically active",
                     ifelse(NHIS08$VIGFREQW >= 3 & NHIS08$VIGFREQW <= 28 & NHIS08$VIGMIN >= 20 & NHIS08$VIGMIN < 997, "Physically active", "Not physically active"))


#BMI
#change all 99.99 (unknown values) to NA
NHIS08$BMI <- unknownToNA(NHIS08$BMI, unknown = c("99.99"))
#quick check to be sure that there are no more potential unknowns
NHIS08 %>% top_n(10, BMI)


#no copd

#ht
NHIS08$HYPEV <- recode(NHIS08$HYPEV,
                       "1" = "1",
                       "2" = "0")
table(NHIS08$HYPEV)
NHIS08$HYPEV <- as.factor(NHIS08$HYPEV)

#________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#for establishing unweighted sample size only, not for design object creation
NHIS08_JS <- subset(NHIS08,
                    !is.na(WTFA_SA) &
                      !is.na(STRAT_P) &
                      !is.na(PSU_P) &
                      !is.na(JNTSYMP))

table(subset(NHIS08_JS, AGE_P == "18 to 24")$SEX)
table(subset(NHIS08_JS, AGE_P == "25 to 29")$SEX)
table(subset(NHIS08_JS, AGE_P == "30 to 34")$SEX)
table(subset(NHIS08_JS, AGE_P == "35 to 39")$SEX)
table(subset(NHIS08_JS, AGE_P == "40 to 44")$SEX)
table(subset(NHIS08_JS, AGE_P == "45 to 49")$SEX)
table(subset(NHIS08_JS, AGE_P == "50 to 54")$SEX)
table(subset(NHIS08_JS, AGE_P == "55 to 59")$SEX)
table(subset(NHIS08_JS, AGE_P == "60 to 64")$SEX)
table(subset(NHIS08_JS, AGE_P == "65 to 69")$SEX)
table(subset(NHIS08_JS, AGE_P == "70 and above")$SEX)


#for establishing unweighted sample size only, not for design object creation
NHIS08_LBP <- subset(NHIS08,
                     !is.na(WTFA_SA) &
                       !is.na(STRAT_P) &
                       !is.na(PSU_P) &
                       !is.na(PAINLB))

table(subset(NHIS08_LBP, AGE_P == "18 to 24")$SEX)
table(subset(NHIS08_LBP, AGE_P == "25 to 29")$SEX)
table(subset(NHIS08_LBP, AGE_P == "30 to 34")$SEX)
table(subset(NHIS08_LBP, AGE_P == "35 to 39")$SEX)
table(subset(NHIS08_LBP, AGE_P == "40 to 44")$SEX)
table(subset(NHIS08_LBP, AGE_P == "45 to 49")$SEX)
table(subset(NHIS08_LBP, AGE_P == "50 to 54")$SEX)
table(subset(NHIS08_LBP, AGE_P == "55 to 59")$SEX)
table(subset(NHIS08_LBP, AGE_P == "60 to 64")$SEX)
table(subset(NHIS08_LBP, AGE_P == "65 to 69")$SEX)
table(subset(NHIS08_LBP, AGE_P == "70 and above")$SEX)

#make sure all complex survey requirements are available
NHIS08_SA_dataset <- subset(NHIS08,
                            !is.na(WTFA_SA) &
                              !is.na(STRAT_P) &
                              !is.na(PSU_P))

#Check that the sum of the weights is equal to the US population
sum(NHIS08_SA_dataset$WTFA_SA)
#The sum of the weights is 225 227 316, which is acceptable

length(unique(NHIS08_SA_dataset[["PSU_P"]]))
#The number of unique PSU's in the data is 2

#Check the number of unique strata
length(unique(NHIS08_SA_dataset[["STRAT_P"]]))
#The number of unique strata is 300

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHIS08_DO <- svydesign(ids = ~1,
                       weights = ~WTFA_SA,
                       strata = ~STRAT_P,
                       nest = TRUE,
                       data = NHIS08_SA_dataset)

#Save design object as RDS file for ease of retrieval for analysis, thereby avoiding running the data download and the design object creation code multiple times
saveRDS(NHIS08_DO,
        file = "NHIS08_DO.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

#_______________________________________________________________________________
#_______________________________________________________________________________
#_______________________________________________________________________________

# == 2007 == #

#DOWNLOAD

NHIS07.instructions <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Program_Code/NHIS/2007/samadult.sas"
NHIS07.fileloc <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/2007/samadult.exe"
#store as a df
NHIS07_SA <- read.SAScii(NHIS07.fileloc, NHIS07.instructions, zipped = TRUE)
#save as RDS file
saveRDS(NHIS07_SA,
        file = "NHIS07_SA.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

#CLEANING

#Some exploration
str(NHIS07_SA)
tail(NHIS07_SA)
glimpse(NHIS07_SA)
colnames(NHIS07_SA)

#Select variables
NHIS07 <- select(NHIS07_SA,
                 HHX, FMX, FPX,
                 REGION,
                 STRAT_P, PSU_P, SRVY_YR, WTFA_SA,
                 SEX, AGE_P,
                 DOINGLWA,
                 JNTSYMP, JMTHP1, JMTHP2, JMTHP3, JMTHP4, JMTHP5, JMTHP6, JMTHP7, JMTHP8, JMTHP9, JMTHP10, JMTHP11, JMTHP12, JMTHP13, JMTHP14, JMTHP15, JMTHP16, JMTHP17,
                 PAINLB,
                 VIGFREQW, VIGMIN, MODFREQW, MODMIN,
                 BMI,
                 HYPEV)

str(NHIS07)
tail(NHIS07)
glimpse(NHIS07)
colnames(NHIS07)

#joint symptoms recode
table(NHIS07$JNTSYMP)
#recode
NHIS07$JNTSYMP <- recode(NHIS07$JNTSYMP,
                         "1" = "1",
                         "2" = "0",
                         "7" = "7",
                         "9" = "9")
(NHIS07$JNTSYMP <- unknownToNA(NHIS07$JNTSYMP, unknown = c("7", "9")))
table(NHIS07$JNTSYMP)
NHIS07$JNTSYMP <- as.factor(NHIS07$JNTSYMP)

#low back pain recode
table(NHIS07$PAINLB)
#recode
NHIS07$PAINLB <- recode(NHIS07$PAINLB,
                        "1" = "1",
                        "2" = "0")
table(NHIS07$PAINLB)
NHIS07$PAINLB <- as.factor(NHIS07$PAINLB)

#region
table(NHIS07$REGION)
NHIS07$REGION <- recode(NHIS07$REGION,
                        "1" = "Northeast",
                        "2" = "Midwest",
                        "3" = "South",
                        "4" = "West")
table(NHIS07$REGION)

#age
table(NHIS07$AGE_P)



NHIS07$AGE_P <- recode(NHIS07$AGE_P,
                       "18" = "18 to 24",
                       "19" = "18 to 24",
                       "20" = "18 to 24",
                       "21" = "18 to 24",
                       "22" = "18 to 24",
                       "23" = "18 to 24",
                       "24" = "18 to 24",
                       "25" = "25 to 29",
                       "26" = "25 to 29",
                       "27" = "25 to 29",
                       "28" = "25 to 29",
                       "29" = "25 to 29",
                       "30" = "30 to 34",
                       "31" = "30 to 34",
                       "32" = "30 to 34",
                       "33" = "30 to 34",
                       "34" = "30 to 34",
                       "35" = "35 to 39",
                       "36" = "35 to 39",
                       "37" = "35 to 39",
                       "38" = "35 to 39", 
                       "39" = "35 to 39",
                       "40" = "40 to 44",
                       "41" = "40 to 44",
                       "42" = "40 to 44",
                       "43" = "40 to 44",
                       "44" = "40 to 44",
                       "45" = "45 to 49",
                       "46" = "45 to 49",
                       "47" = "45 to 49",
                       "48" = "45 to 49",
                       "49" = "45 to 49",
                       "50" = "50 to 54",
                       "51" = "50 to 54",
                       "52" = "50 to 54",
                       "53" = "50 to 54",
                       "54" = "50 to 54",
                       "55" = "55 to 59",
                       "56" = "55 to 59",
                       "57" = "55 to 59",
                       "58" = "55 to 59",
                       "59" = "55 to 59",
                       "60" = "60 to 64",
                       "61" = "60 to 64",
                       "62" = "60 to 64",
                       "63" = "60 to 64",
                       "64" = "60 to 64",
                       "65" = "65 to 69",
                       "66" = "65 to 69",
                       "67" = "65 to 69",
                       "68" = "65 to 69",
                       "69" = "65 to 69",
                       "70" = "70 and above",
                       "71" = "70 and above",
                       "72" = "70 and above",
                       "73" = "70 and above",
                       "74" = "70 and above",
                       "75" = "70 and above",
                       "76" = "70 and above",
                       "77" = "70 and above",
                       "78" = "70 and above",
                       "79" = "70 and above",
                       "80" = "70 and above",
                       "81" = "70 and above",
                       "82" = "70 and above",
                       "83" = "70 and above",
                       "84" = "70 and above",
                       "85" = "70 and above")
table(NHIS07$AGE_P)
NHIS07$AGE_P <- as.factor(NHIS07$AGE_P)

#sex
NHIS07$SEX <- recode(NHIS07$SEX,
                     "1" = "Male",
                     "2" = "Female")
table(NHIS07$SEX)
NHIS07$SEX <- as.factor(NHIS07$SEX)

#employment status
NHIS07$DOINGLWA <- recode(NHIS07$DOINGLWA,
                          "1" = "Working for pay at a job/business",
                          "2" = "2",
                          "3" = "3",
                          "4" = "4",
                          "5" = "Not working")
#Change unused values to NA
(NHIS07$DOINGLWA <- unknownToNA(NHIS07$DOINGLWA, unknown = c("9", "2", "3", "4")))
table(NHIS07$DOINGLWA)
NHIS07$DOINGLWA <- as.factor(NHIS07$DOINGLWA)

#affected joing areas quick clean
NHIS07$JMTHP1 <- unknownToNA(NHIS07$JMTHP1, unknown = 7:9)
NHIS07$JMTHP1 <- as.factor(NHIS07$JMTHP1)

NHIS07$JMTHP2 <- unknownToNA(NHIS07$JMTHP2, unknown = 7:9)
NHIS07$JMTHP2 <- as.factor(NHIS07$JMTHP2)

NHIS07$JMTHP3 <- unknownToNA(NHIS07$JMTHP3, unknown = 7:9)
NHIS07$JMTHP3 <- as.factor(NHIS07$JMTHP3)

NHIS07$JMTHP4 <- unknownToNA(NHIS07$JMTHP4, unknown = 7:9)
NHIS07$JMTHP4 <- as.factor(NHIS07$JMTHP4)

NHIS07$JMTHP5 <- unknownToNA(NHIS07$JMTHP5, unknown = 7:9)
NHIS07$JMTHP5 <- as.factor(NHIS07$JMTHP5)

NHIS07$JMTHP6 <- unknownToNA(NHIS07$JMTHP6, unknown = 7:9)
NHIS07$JMTHP6 <- as.factor(NHIS07$JMTHP6)

NHIS07$JMTHP7 <- unknownToNA(NHIS07$JMTHP7, unknown = 7:9)
NHIS07$JMTHP7 <- as.factor(NHIS07$JMTHP7)

NHIS07$JMTHP8 <- unknownToNA(NHIS07$JMTHP8, unknown = 7:9)
NHIS07$JMTHP8 <- as.factor(NHIS07$JMTHP8)

NHIS07$JMTHP9 <- unknownToNA(NHIS07$JMTHP9, unknown = 7:9)
NHIS07$JMTHP9 <- as.factor(NHIS07$JMTHP9)

NHIS07$JMTHP10 <- unknownToNA(NHIS07$JMTHP10, unknown = 7:9)
NHIS07$JMTHP10 <- as.factor(NHIS07$JMTHP10)

NHIS07$JMTHP11 <- unknownToNA(NHIS07$JMTHP11, unknown = 7:9)
NHIS07$JMTHP11 <- as.factor(NHIS07$JMTHP11)

NHIS07$JMTHP12 <- unknownToNA(NHIS07$JMTHP12, unknown = 7:9)
NHIS07$JMTHP12 <- as.factor(NHIS07$JMTHP12)

NHIS07$JMTHP13 <- unknownToNA(NHIS07$JMTHP13, unknown = 7:9)
NHIS07$JMTHP13 <- as.factor(NHIS07$JMTHP13)

NHIS07$JMTHP14 <- unknownToNA(NHIS07$JMTHP14, unknown = 7:9)
NHIS07$JMTHP14 <- as.factor(NHIS07$JMTHP14)

NHIS07$JMTHP15 <- unknownToNA(NHIS07$JMTHP15, unknown = 7:9)
NHIS07$JMTHP15 <- as.factor(NHIS07$JMTHP15)

NHIS07$JMTHP16 <- unknownToNA(NHIS07$JMTHP16, unknown = 7:9)
NHIS07$JMTHP16 <- as.factor(NHIS07$JMTHP16)

NHIS07$JMTHP17 <- unknownToNA(NHIS07$JMTHP17, unknown = 7:9)
NHIS07$JMTHP17 <- as.factor(NHIS07$JMTHP17)

#physical activity
#vigorous PA frequency
table(NHIS07$VIGFREQW)
NHIS07$VIGFREQW <- as.numeric(NHIS07$VIGFREQW)

#vigorous PA duration
table(NHIS07$VIGMIN)
NHIS07$VIGMIN <- as.numeric(NHIS07$VIGMIN)

#moderate pa frequency
table(NHIS07$MODFREQW)
NHIS07$MODFREQW <- as.numeric(NHIS07$MODFREQW)

#moderate PA duration
table(NHIS07$MODMIN)
NHIS07$MODMIN <- as.numeric(NHIS07$MODMIN)

#Make PA
#Physicially active = light/moderate pa >=5x/wk & >=30min OR Vig >=3x/wk & >=20min, else not
NHIS07$PAC <- ifelse(NHIS07$MODFREQW >= 5 & NHIS07$MODFREQW <= 28 & NHIS07$MODMIN >= 30 & NHIS07$MODMIN < 997, "Physically active",
                     ifelse(NHIS07$VIGFREQW >= 3 & NHIS07$VIGFREQW <= 28 & NHIS07$VIGMIN >= 20 & NHIS07$VIGMIN < 997, "Physically active", "Not physically active"))



#BMI
#change all 99.99 (unknown values) to NA
NHIS07$BMI <- unknownToNA(NHIS07$BMI, unknown = c("99.99"))
#quick check to be sure that there are no more potential unknowns
NHIS07 %>% top_n(10, BMI)


#no copd

#ht
NHIS07$HYPEV <- recode(NHIS07$HYPEV,
                       "1" = "1",
                       "2" = "0")
table(NHIS07$HYPEV)
NHIS07$HYPEV <- as.factor(NHIS07$HYPEV)

#________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#for establishing unweighted sample size only, not for design object creation
NHIS07_JS <- subset(NHIS07,
                    !is.na(WTFA_SA) &
                      !is.na(STRAT_P) &
                      !is.na(PSU_P) &
                      !is.na(JNTSYMP))

table(subset(NHIS07_JS, AGE_P == "18 to 24")$SEX)
table(subset(NHIS07_JS, AGE_P == "25 to 29")$SEX)
table(subset(NHIS07_JS, AGE_P == "30 to 34")$SEX)
table(subset(NHIS07_JS, AGE_P == "35 to 39")$SEX)
table(subset(NHIS07_JS, AGE_P == "40 to 44")$SEX)
table(subset(NHIS07_JS, AGE_P == "45 to 49")$SEX)
table(subset(NHIS07_JS, AGE_P == "50 to 54")$SEX)
table(subset(NHIS07_JS, AGE_P == "55 to 59")$SEX)
table(subset(NHIS07_JS, AGE_P == "60 to 64")$SEX)
table(subset(NHIS07_JS, AGE_P == "65 to 69")$SEX)
table(subset(NHIS07_JS, AGE_P == "70 and above")$SEX)


#for establishing unweighted sample size only, not for design object creation
NHIS07_LBP <- subset(NHIS07,
                     !is.na(WTFA_SA) &
                       !is.na(STRAT_P) &
                       !is.na(PSU_P) &
                       !is.na(PAINLB))

table(subset(NHIS07_LBP, AGE_P == "18 to 24")$SEX)
table(subset(NHIS07_LBP, AGE_P == "25 to 29")$SEX)
table(subset(NHIS07_LBP, AGE_P == "30 to 34")$SEX)
table(subset(NHIS07_LBP, AGE_P == "35 to 39")$SEX)
table(subset(NHIS07_LBP, AGE_P == "40 to 44")$SEX)
table(subset(NHIS07_LBP, AGE_P == "45 to 49")$SEX)
table(subset(NHIS07_LBP, AGE_P == "50 to 54")$SEX)
table(subset(NHIS07_LBP, AGE_P == "55 to 59")$SEX)
table(subset(NHIS07_LBP, AGE_P == "60 to 64")$SEX)
table(subset(NHIS07_LBP, AGE_P == "65 to 69")$SEX)
table(subset(NHIS07_LBP, AGE_P == "70 and above")$SEX)

#make sure all complex survey requirements are available
NHIS07_SA_dataset <- subset(NHIS07,
                            !is.na(WTFA_SA) &
                              !is.na(STRAT_P) &
                              !is.na(PSU_P))

#Check that the sum of the weights is equal to the US population
sum(NHIS07_SA_dataset$WTFA_SA)
#The sum of the weights is 223 180 965, which is acceptable

length(unique(NHIS07_SA_dataset[["PSU_P"]]))
#The number of unique PSU's in the data is 2

#Check the number of unique strata
length(unique(NHIS07_SA_dataset[["STRAT_P"]]))
#The number of unique strata is 300

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHIS07_DO <- svydesign(ids = ~1,
                       weights = ~WTFA_SA,
                       strata = ~STRAT_P,
                       nest = TRUE,
                       data = NHIS07_SA_dataset)

#Save design object as RDS file for ease of retrieval for analysis, thereby avoiding running the data download and the design object creation code multiple times
saveRDS(NHIS07_DO,
        file = "NHIS07_DO.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


#_________________________________________________________________________
#_________________________________________________________________________
#_________________________________________________________________________

# == 2006 == #

#DOWNLOAD

NHIS06.instructions <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Program_Code/NHIS/2006/samadult.sas"
NHIS06.fileloc <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/2006/samadult.exe"
#store as a df
NHIS06_SA <- read.SAScii(NHIS06.fileloc, NHIS06.instructions, zipped = TRUE)
#save as RDS file
saveRDS(NHIS06_SA,
        file = "NHIS06_SA.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

#CLEANING

#Some exploration
str(NHIS06_SA)
tail(NHIS06_SA)
glimpse(NHIS06_SA)
colnames(NHIS06_SA)

#Select variables
NHIS06 <- select(NHIS06_SA,
                 HHX, FMX, FPX,
                 REGION,
                 STRAT_P, PSU_P, SRVY_YR, WTFA_SA,
                 SEX, AGE_P,
                 DOINGLWA,
                 JNTSYMP, JMTHP1, JMTHP2, JMTHP3, JMTHP4, JMTHP5, JMTHP6, JMTHP7, JMTHP8, JMTHP9, JMTHP10, JMTHP11, JMTHP12, JMTHP13, JMTHP14, JMTHP15, JMTHP16, JMTHP17,
                 PAINLB,
                 VIGFREQW, VIGMIN, MODFREQW, MODMIN,
                 BMI,
                 HYPEV)

str(NHIS06)
tail(NHIS06)
glimpse(NHIS06)
colnames(NHIS06)

#joint symptoms recode
table(NHIS06$JNTSYMP)
#recode
NHIS06$JNTSYMP <- recode(NHIS06$JNTSYMP,
                         "1" = "1",
                         "2" = "0",
                         "7" = "7",
                         "9" = "9")
(NHIS06$JNTSYMP <- unknownToNA(NHIS06$JNTSYMP, unknown = c("7", "9")))
table(NHIS06$JNTSYMP)
NHIS06$JNTSYMP <- as.factor(NHIS06$JNTSYMP)

#low back pain recode
table(NHIS06$PAINLB)
#recode
NHIS06$PAINLB <- recode(NHIS06$PAINLB,
                        "1" = "1",
                        "2" = "0")
table(NHIS06$PAINLB)
NHIS06$PAINLB <- as.factor(NHIS06$PAINLB)

#region
table(NHIS06$REGION)
NHIS06$REGION <- recode(NHIS06$REGION,
                        "1" = "Northeast",
                        "2" = "Midwest",
                        "3" = "South",
                        "4" = "West")
table(NHIS06$REGION)

#age
table(NHIS06$AGE_P)



NHIS06$AGE_P <- recode(NHIS06$AGE_P,
                       "18" = "18 to 24",
                       "19" = "18 to 24",
                       "20" = "18 to 24",
                       "21" = "18 to 24",
                       "22" = "18 to 24",
                       "23" = "18 to 24",
                       "24" = "18 to 24",
                       "25" = "25 to 29",
                       "26" = "25 to 29",
                       "27" = "25 to 29",
                       "28" = "25 to 29",
                       "29" = "25 to 29",
                       "30" = "30 to 34",
                       "31" = "30 to 34",
                       "32" = "30 to 34",
                       "33" = "30 to 34",
                       "34" = "30 to 34",
                       "35" = "35 to 39",
                       "36" = "35 to 39",
                       "37" = "35 to 39",
                       "38" = "35 to 39", 
                       "39" = "35 to 39",
                       "40" = "40 to 44",
                       "41" = "40 to 44",
                       "42" = "40 to 44",
                       "43" = "40 to 44",
                       "44" = "40 to 44",
                       "45" = "45 to 49",
                       "46" = "45 to 49",
                       "47" = "45 to 49",
                       "48" = "45 to 49",
                       "49" = "45 to 49",
                       "50" = "50 to 54",
                       "51" = "50 to 54",
                       "52" = "50 to 54",
                       "53" = "50 to 54",
                       "54" = "50 to 54",
                       "55" = "55 to 59",
                       "56" = "55 to 59",
                       "57" = "55 to 59",
                       "58" = "55 to 59",
                       "59" = "55 to 59",
                       "60" = "60 to 64",
                       "61" = "60 to 64",
                       "62" = "60 to 64",
                       "63" = "60 to 64",
                       "64" = "60 to 64",
                       "65" = "65 to 69",
                       "66" = "65 to 69",
                       "67" = "65 to 69",
                       "68" = "65 to 69",
                       "69" = "65 to 69",
                       "70" = "70 and above",
                       "71" = "70 and above",
                       "72" = "70 and above",
                       "73" = "70 and above",
                       "74" = "70 and above",
                       "75" = "70 and above",
                       "76" = "70 and above",
                       "77" = "70 and above",
                       "78" = "70 and above",
                       "79" = "70 and above",
                       "80" = "70 and above",
                       "81" = "70 and above",
                       "82" = "70 and above",
                       "83" = "70 and above",
                       "84" = "70 and above",
                       "85" = "70 and above")
table(NHIS06$AGE_P)
NHIS06$AGE_P <- as.factor(NHIS06$AGE_P)

#sex
NHIS06$SEX <- recode(NHIS06$SEX,
                     "1" = "Male",
                     "2" = "Female")
table(NHIS06$SEX)
NHIS06$SEX <- as.factor(NHIS06$SEX)

#employment status
NHIS06$DOINGLWA <- recode(NHIS06$DOINGLWA,
                          "1" = "Working for pay at a job/business",
                          "2" = "2",
                          "3" = "3",
                          "4" = "4",
                          "5" = "Not working")
#Change unused values to NA
(NHIS06$DOINGLWA <- unknownToNA(NHIS06$DOINGLWA, unknown = c("9", "2", "3", "4")))
table(NHIS06$DOINGLWA)
NHIS06$DOINGLWA <- as.factor(NHIS06$DOINGLWA)

#affected joing areas quick clean
NHIS06$JMTHP1 <- unknownToNA(NHIS06$JMTHP1, unknown = 7:9)
NHIS06$JMTHP1 <- as.factor(NHIS06$JMTHP1)

NHIS06$JMTHP2 <- unknownToNA(NHIS06$JMTHP2, unknown = 7:9)
NHIS06$JMTHP2 <- as.factor(NHIS06$JMTHP2)

NHIS06$JMTHP3 <- unknownToNA(NHIS06$JMTHP3, unknown = 7:9)
NHIS06$JMTHP3 <- as.factor(NHIS06$JMTHP3)

NHIS06$JMTHP4 <- unknownToNA(NHIS06$JMTHP4, unknown = 7:9)
NHIS06$JMTHP4 <- as.factor(NHIS06$JMTHP4)

NHIS06$JMTHP5 <- unknownToNA(NHIS06$JMTHP5, unknown = 7:9)
NHIS06$JMTHP5 <- as.factor(NHIS06$JMTHP5)

NHIS06$JMTHP6 <- unknownToNA(NHIS06$JMTHP6, unknown = 7:9)
NHIS06$JMTHP6 <- as.factor(NHIS06$JMTHP6)

NHIS06$JMTHP7 <- unknownToNA(NHIS06$JMTHP7, unknown = 7:9)
NHIS06$JMTHP7 <- as.factor(NHIS06$JMTHP7)

NHIS06$JMTHP8 <- unknownToNA(NHIS06$JMTHP8, unknown = 7:9)
NHIS06$JMTHP8 <- as.factor(NHIS06$JMTHP8)

NHIS06$JMTHP9 <- unknownToNA(NHIS06$JMTHP9, unknown = 7:9)
NHIS06$JMTHP9 <- as.factor(NHIS06$JMTHP9)

NHIS06$JMTHP10 <- unknownToNA(NHIS06$JMTHP10, unknown = 7:9)
NHIS06$JMTHP10 <- as.factor(NHIS06$JMTHP10)

NHIS06$JMTHP11 <- unknownToNA(NHIS06$JMTHP11, unknown = 7:9)
NHIS06$JMTHP11 <- as.factor(NHIS06$JMTHP11)

NHIS06$JMTHP12 <- unknownToNA(NHIS06$JMTHP12, unknown = 7:9)
NHIS06$JMTHP12 <- as.factor(NHIS06$JMTHP12)

NHIS06$JMTHP13 <- unknownToNA(NHIS06$JMTHP13, unknown = 7:9)
NHIS06$JMTHP13 <- as.factor(NHIS06$JMTHP13)

NHIS06$JMTHP14 <- unknownToNA(NHIS06$JMTHP14, unknown = 7:9)
NHIS06$JMTHP14 <- as.factor(NHIS06$JMTHP14)

NHIS06$JMTHP15 <- unknownToNA(NHIS06$JMTHP15, unknown = 7:9)
NHIS06$JMTHP15 <- as.factor(NHIS06$JMTHP15)

NHIS06$JMTHP16 <- unknownToNA(NHIS06$JMTHP16, unknown = 7:9)
NHIS06$JMTHP16 <- as.factor(NHIS06$JMTHP16)

NHIS06$JMTHP17 <- unknownToNA(NHIS06$JMTHP17, unknown = 7:9)
NHIS06$JMTHP17 <- as.factor(NHIS06$JMTHP17)

#physical activity
#vigorous PA frequency
table(NHIS06$VIGFREQW)
NHIS06$VIGFREQW <- as.numeric(NHIS06$VIGFREQW)

#vigorous PA duration
table(NHIS06$VIGMIN)
NHIS06$VIGMIN <- as.numeric(NHIS06$VIGMIN)

#moderate pa frequency
table(NHIS06$MODFREQW)
NHIS06$MODFREQW <- as.numeric(NHIS06$MODFREQW)

#moderate PA duration
table(NHIS06$MODMIN)
NHIS06$MODMIN <- as.numeric(NHIS06$MODMIN)

#Make PA
#Physicially active = light/moderate pa >=5x/wk & >=30min OR Vig >=3x/wk & >=20min, else not
NHIS06$PAC <- ifelse(NHIS06$MODFREQW >= 5 & NHIS06$MODFREQW <= 28 & NHIS06$MODMIN >= 30 & NHIS06$MODMIN < 997, "Physically active",
                     ifelse(NHIS06$VIGFREQW >= 3 & NHIS06$VIGFREQW <= 28 & NHIS06$VIGMIN >= 20 & NHIS06$VIGMIN < 997, "Physically active", "Not physically active"))

#BMI
#change all 99.99 (unknown values) to NA
NHIS06$BMI <- unknownToNA(NHIS06$BMI, unknown = c("99.99"))
#quick check to be sure that there are no more potential unknowns
NHIS06 %>% top_n(10, BMI)


#no copd

#ht
NHIS06$HYPEV <- recode(NHIS06$HYPEV,
                       "1" = "1",
                       "2" = "0")
table(NHIS06$HYPEV)
NHIS06$HYPEV <- as.factor(NHIS06$HYPEV)

#________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#for establishing unweighted sample size only, not for design object creation
NHIS06_JS <- subset(NHIS06,
                    !is.na(WTFA_SA) &
                      !is.na(STRAT_P) &
                      !is.na(PSU_P) &
                      !is.na(JNTSYMP))

table(subset(NHIS06_JS, AGE_P == "18 to 24")$SEX)
table(subset(NHIS06_JS, AGE_P == "25 to 29")$SEX)
table(subset(NHIS06_JS, AGE_P == "30 to 34")$SEX)
table(subset(NHIS06_JS, AGE_P == "35 to 39")$SEX)
table(subset(NHIS06_JS, AGE_P == "40 to 44")$SEX)
table(subset(NHIS06_JS, AGE_P == "45 to 49")$SEX)
table(subset(NHIS06_JS, AGE_P == "50 to 54")$SEX)
table(subset(NHIS06_JS, AGE_P == "55 to 59")$SEX)
table(subset(NHIS06_JS, AGE_P == "60 to 64")$SEX)
table(subset(NHIS06_JS, AGE_P == "65 to 69")$SEX)
table(subset(NHIS06_JS, AGE_P == "70 and above")$SEX)


#for establishing unweighted sample size only, not for design object creation
NHIS06_LBP <- subset(NHIS06,
                     !is.na(WTFA_SA) &
                       !is.na(STRAT_P) &
                       !is.na(PSU_P) &
                       !is.na(PAINLB))

table(subset(NHIS06_LBP, AGE_P == "18 to 24")$SEX)
table(subset(NHIS06_LBP, AGE_P == "25 to 29")$SEX)
table(subset(NHIS06_LBP, AGE_P == "30 to 34")$SEX)
table(subset(NHIS06_LBP, AGE_P == "35 to 39")$SEX)
table(subset(NHIS06_LBP, AGE_P == "40 to 44")$SEX)
table(subset(NHIS06_LBP, AGE_P == "45 to 49")$SEX)
table(subset(NHIS06_LBP, AGE_P == "50 to 54")$SEX)
table(subset(NHIS06_LBP, AGE_P == "55 to 59")$SEX)
table(subset(NHIS06_LBP, AGE_P == "60 to 64")$SEX)
table(subset(NHIS06_LBP, AGE_P == "65 to 69")$SEX)
table(subset(NHIS06_LBP, AGE_P == "70 and above")$SEX)

#make sure all complex survey requirements are available
NHIS06_SA_dataset <- subset(NHIS06,
                            !is.na(WTFA_SA) &
                              !is.na(STRAT_P) &
                              !is.na(PSU_P))

#Check that the sum of the weights is equal to the US population
sum(NHIS06_SA_dataset$WTFA_SA)
#The sum of the weights is 220 266 693, which is acceptable

length(unique(NHIS06_SA_dataset[["PSU_P"]]))
#The number of unique PSU's in the data is 2

#Check the number of unique strata
length(unique(NHIS06_SA_dataset[["STRAT_P"]]))
#The number of unique strata is 300

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHIS06_DO <- svydesign(ids = ~1,
                       weights = ~WTFA_SA,
                       strata = ~STRAT_P,
                       nest = TRUE,
                       data = NHIS06_SA_dataset)

#Save design object as RDS file for ease of retrieval for analysis, thereby avoiding running the data download and the design object creation code multiple times
saveRDS(NHIS06_DO,
        file = "NHIS06_DO.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


#____________________________________________________________________
#____________________________________________________________________
#____________________________________________________________________

# == 2005 == #

#DOWNLOAD

NHIS05.instructions <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Program_Code/NHIS/2005/samadult.sas"
NHIS05.fileloc <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/2005/samadult.exe"
#store as df
NHIS05_SA <- read.SAScii(NHIS05.fileloc, NHIS05.instructions, zipped = TRUE)
#save as RDS file
saveRDS(NHIS05_SA,
        file = "NHIS05_SA.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

#CLEANING

#Some exploration
str(NHIS05_SA)
tail(NHIS05_SA)
glimpse(NHIS05_SA)
colnames(NHIS05_SA)

#Select variables
NHIS05 <- select(NHIS05_SA,
                 HHX, FMX, FPX,
                 REGION,
                 STRATUM, PSU, SRVY_YR, WTFA_SA,
                 SEX, AGE_P,
                 DOINGLWA,
                 JNTSYMP, JMTHP1, JMTHP2, JMTHP3, JMTHP4, JMTHP5, JMTHP6, JMTHP7, JMTHP8, JMTHP9, JMTHP10, JMTHP11, JMTHP12, JMTHP13, JMTHP14, JMTHP15, JMTHP16, JMTHP17,
                 PAINLB,
                 VIGFREQW, VIGMIN, MODFREQW, MODMIN,
                 BMI,
                 HYPEV)

str(NHIS05)
tail(NHIS05)
glimpse(NHIS05)
colnames(NHIS05)

#joint symptoms recode
table(NHIS05$JNTSYMP)
#recode
NHIS05$JNTSYMP <- recode(NHIS05$JNTSYMP,
                         "1" = "1",
                         "2" = "0",
                         "7" = "7",
                         "9" = "9")
(NHIS05$JNTSYMP <- unknownToNA(NHIS05$JNTSYMP, unknown = c("7", "9")))
table(NHIS05$JNTSYMP)
NHIS05$JNTSYMP <- as.factor(NHIS05$JNTSYMP)

#low back pain recode
table(NHIS05$PAINLB)
#recode
NHIS05$PAINLB <- recode(NHIS05$PAINLB,
                        "1" = "1",
                        "2" = "0")
table(NHIS05$PAINLB)
NHIS05$PAINLB <- as.factor(NHIS05$PAINLB)

#region
table(NHIS05$REGION)
NHIS05$REGION <- recode(NHIS05$REGION,
                        "1" = "Northeast",
                        "2" = "Midwest",
                        "3" = "South",
                        "4" = "West")
table(NHIS05$REGION)

#age
table(NHIS05$AGE_P)



NHIS05$AGE_P <- recode(NHIS05$AGE_P,
                       "18" = "18 to 24",
                       "19" = "18 to 24",
                       "20" = "18 to 24",
                       "21" = "18 to 24",
                       "22" = "18 to 24",
                       "23" = "18 to 24",
                       "24" = "18 to 24",
                       "25" = "25 to 29",
                       "26" = "25 to 29",
                       "27" = "25 to 29",
                       "28" = "25 to 29",
                       "29" = "25 to 29",
                       "30" = "30 to 34",
                       "31" = "30 to 34",
                       "32" = "30 to 34",
                       "33" = "30 to 34",
                       "34" = "30 to 34",
                       "35" = "35 to 39",
                       "36" = "35 to 39",
                       "37" = "35 to 39",
                       "38" = "35 to 39", 
                       "39" = "35 to 39",
                       "40" = "40 to 44",
                       "41" = "40 to 44",
                       "42" = "40 to 44",
                       "43" = "40 to 44",
                       "44" = "40 to 44",
                       "45" = "45 to 49",
                       "46" = "45 to 49",
                       "47" = "45 to 49",
                       "48" = "45 to 49",
                       "49" = "45 to 49",
                       "50" = "50 to 54",
                       "51" = "50 to 54",
                       "52" = "50 to 54",
                       "53" = "50 to 54",
                       "54" = "50 to 54",
                       "55" = "55 to 59",
                       "56" = "55 to 59",
                       "57" = "55 to 59",
                       "58" = "55 to 59",
                       "59" = "55 to 59",
                       "60" = "60 to 64",
                       "61" = "60 to 64",
                       "62" = "60 to 64",
                       "63" = "60 to 64",
                       "64" = "60 to 64",
                       "65" = "65 to 69",
                       "66" = "65 to 69",
                       "67" = "65 to 69",
                       "68" = "65 to 69",
                       "69" = "65 to 69",
                       "70" = "70 and above",
                       "71" = "70 and above",
                       "72" = "70 and above",
                       "73" = "70 and above",
                       "74" = "70 and above",
                       "75" = "70 and above",
                       "76" = "70 and above",
                       "77" = "70 and above",
                       "78" = "70 and above",
                       "79" = "70 and above",
                       "80" = "70 and above",
                       "81" = "70 and above",
                       "82" = "70 and above",
                       "83" = "70 and above",
                       "84" = "70 and above",
                       "85" = "70 and above")
table(NHIS05$AGE_P)
NHIS05$AGE_P <- as.factor(NHIS05$AGE_P)

#sex
NHIS05$SEX <- recode(NHIS05$SEX,
                     "1" = "Male",
                     "2" = "Female")
table(NHIS05$SEX)
NHIS05$SEX <- as.factor(NHIS05$SEX)

#employment status
NHIS05$DOINGLWA <- recode(NHIS05$DOINGLWA,
                          "1" = "Working for pay at a job/business",
                          "2" = "2",
                          "3" = "3",
                          "4" = "4",
                          "5" = "Not working")
#Change unused values to NA
(NHIS05$DOINGLWA <- unknownToNA(NHIS05$DOINGLWA, unknown = c("9", "2", "3", "4")))
table(NHIS05$DOINGLWA)
NHIS05$DOINGLWA <- as.factor(NHIS05$DOINGLWA)

#affected joing areas quick clean
NHIS05$JMTHP1 <- unknownToNA(NHIS05$JMTHP1, unknown = 7:9)
NHIS05$JMTHP1 <- as.factor(NHIS05$JMTHP1)

NHIS05$JMTHP2 <- unknownToNA(NHIS05$JMTHP2, unknown = 7:9)
NHIS05$JMTHP2 <- as.factor(NHIS05$JMTHP2)

NHIS05$JMTHP3 <- unknownToNA(NHIS05$JMTHP3, unknown = 7:9)
NHIS05$JMTHP3 <- as.factor(NHIS05$JMTHP3)

NHIS05$JMTHP4 <- unknownToNA(NHIS05$JMTHP4, unknown = 7:9)
NHIS05$JMTHP4 <- as.factor(NHIS05$JMTHP4)

NHIS05$JMTHP5 <- unknownToNA(NHIS05$JMTHP5, unknown = 7:9)
NHIS05$JMTHP5 <- as.factor(NHIS05$JMTHP5)

NHIS05$JMTHP6 <- unknownToNA(NHIS05$JMTHP6, unknown = 7:9)
NHIS05$JMTHP6 <- as.factor(NHIS05$JMTHP6)

NHIS05$JMTHP7 <- unknownToNA(NHIS05$JMTHP7, unknown = 7:9)
NHIS05$JMTHP7 <- as.factor(NHIS05$JMTHP7)

NHIS05$JMTHP8 <- unknownToNA(NHIS05$JMTHP8, unknown = 7:9)
NHIS05$JMTHP8 <- as.factor(NHIS05$JMTHP8)

NHIS05$JMTHP9 <- unknownToNA(NHIS05$JMTHP9, unknown = 7:9)
NHIS05$JMTHP9 <- as.factor(NHIS05$JMTHP9)

NHIS05$JMTHP10 <- unknownToNA(NHIS05$JMTHP10, unknown = 7:9)
NHIS05$JMTHP10 <- as.factor(NHIS05$JMTHP10)

NHIS05$JMTHP11 <- unknownToNA(NHIS05$JMTHP11, unknown = 7:9)
NHIS05$JMTHP11 <- as.factor(NHIS05$JMTHP11)

NHIS05$JMTHP12 <- unknownToNA(NHIS05$JMTHP12, unknown = 7:9)
NHIS05$JMTHP12 <- as.factor(NHIS05$JMTHP12)

NHIS05$JMTHP13 <- unknownToNA(NHIS05$JMTHP13, unknown = 7:9)
NHIS05$JMTHP13 <- as.factor(NHIS05$JMTHP13)

NHIS05$JMTHP14 <- unknownToNA(NHIS05$JMTHP14, unknown = 7:9)
NHIS05$JMTHP14 <- as.factor(NHIS05$JMTHP14)

NHIS05$JMTHP15 <- unknownToNA(NHIS05$JMTHP15, unknown = 7:9)
NHIS05$JMTHP15 <- as.factor(NHIS05$JMTHP15)

NHIS05$JMTHP16 <- unknownToNA(NHIS05$JMTHP16, unknown = 7:9)
NHIS05$JMTHP16 <- as.factor(NHIS05$JMTHP16)

NHIS05$JMTHP17 <- unknownToNA(NHIS05$JMTHP17, unknown = 7:9)
NHIS05$JMTHP17 <- as.factor(NHIS05$JMTHP17)

#physical activity
#vigorous PA frequency
table(NHIS05$VIGFREQW)
NHIS05$VIGFREQW <- as.numeric(NHIS05$VIGFREQW)

#vigorous PA duration
table(NHIS05$VIGMIN)
NHIS05$VIGMIN <- as.numeric(NHIS05$VIGMIN)

#moderate pa frequency
table(NHIS05$MODFREQW)
NHIS05$MODFREQW <- as.numeric(NHIS05$MODFREQW)

#moderate PA duration
table(NHIS05$MODMIN)
NHIS05$MODMIN <- as.numeric(NHIS05$MODMIN)

#Make PA
#Physicially active = light/moderate pa >=5x/wk & >=30min OR Vig >=3x/wk & >=20min, else not
NHIS05$PAC <- ifelse(NHIS05$MODFREQW >= 5 & NHIS05$MODFREQW <= 28 & NHIS05$MODMIN >= 30 & NHIS05$MODMIN < 997, "Physically active",
                     ifelse(NHIS05$VIGFREQW >= 3 & NHIS05$VIGFREQW <= 28 & NHIS05$VIGMIN >= 20 & NHIS05$VIGMIN < 997, "Physically active", "Not physically active"))

#BMI
#change all 99.99 (unknown values) to NA
NHIS05$BMI <- unknownToNA(NHIS05$BMI, unknown = c("99.99"))
#quick check to be sure that there are no more potential unknowns
NHIS05 %>% top_n(10, BMI)


#no copd

#ht
NHIS05$HYPEV <- recode(NHIS05$HYPEV,
                       "1" = "1",
                       "2" = "0")
table(NHIS05$HYPEV)
NHIS05$HYPEV <- as.factor(NHIS05$HYPEV)

#________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#for establishing unweighted sample size only, not for design object creation
NHIS05_JS <- subset(NHIS05,
                    !is.na(WTFA_SA) &
                      !is.na(STRATUM) &
                      !is.na(PSU) &
                      !is.na(JNTSYMP))

table(subset(NHIS05_JS, AGE_P == "18 to 24")$SEX)
table(subset(NHIS05_JS, AGE_P == "25 to 29")$SEX)
table(subset(NHIS05_JS, AGE_P == "30 to 34")$SEX)
table(subset(NHIS05_JS, AGE_P == "35 to 39")$SEX)
table(subset(NHIS05_JS, AGE_P == "40 to 44")$SEX)
table(subset(NHIS05_JS, AGE_P == "45 to 49")$SEX)
table(subset(NHIS05_JS, AGE_P == "50 to 54")$SEX)
table(subset(NHIS05_JS, AGE_P == "55 to 59")$SEX)
table(subset(NHIS05_JS, AGE_P == "60 to 64")$SEX)
table(subset(NHIS05_JS, AGE_P == "65 to 69")$SEX)
table(subset(NHIS05_JS, AGE_P == "70 and above")$SEX)


#for establishing unweighted sample size only, not for design object creation
NHIS05_LBP <- subset(NHIS05,
                     !is.na(WTFA_SA) &
                       !is.na(STRATUM) &
                       !is.na(PSU) &
                       !is.na(PAINLB))

table(subset(NHIS05_LBP, AGE_P == "18 to 24")$SEX)
table(subset(NHIS05_LBP, AGE_P == "25 to 29")$SEX)
table(subset(NHIS05_LBP, AGE_P == "30 to 34")$SEX)
table(subset(NHIS05_LBP, AGE_P == "35 to 39")$SEX)
table(subset(NHIS05_LBP, AGE_P == "40 to 44")$SEX)
table(subset(NHIS05_LBP, AGE_P == "45 to 49")$SEX)
table(subset(NHIS05_LBP, AGE_P == "50 to 54")$SEX)
table(subset(NHIS05_LBP, AGE_P == "55 to 59")$SEX)
table(subset(NHIS05_LBP, AGE_P == "60 to 64")$SEX)
table(subset(NHIS05_LBP, AGE_P == "65 to 69")$SEX)
table(subset(NHIS05_LBP, AGE_P == "70 and above")$SEX)

#make sure all complex survey requirements are available
NHIS05_SA_dataset <- subset(NHIS05,
                            !is.na(WTFA_SA) &
                              !is.na(STRATUM) &
                              !is.na(PSU))

#Check that the sum of the weights is equal to the US population
sum(NHIS05_SA_dataset$WTFA_SA)
#The sum of the weights is 217 773 755, which is acceptable

length(unique(NHIS05_SA_dataset[["PSU"]]))
#The number of unique PSU's in the data is 2

#Check the number of unique strata
length(unique(NHIS05_SA_dataset[["STRATUM"]]))
#The number of unique strata is 339

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHIS05_DO <- svydesign(ids = ~1,
                       weights = ~WTFA_SA,
                       strata = ~STRATUM,
                       nest = TRUE,
                       data = NHIS05_SA_dataset)

#Save design object as RDS file for ease of retrieval for analysis, thereby avoiding running the data download and the design object creation code multiple times
saveRDS(NHIS05_DO,
        file = "NHIS05_DO.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

#_________________________________________________________________________
#_________________________________________________________________________
#_________________________________________________________________________

# == 2004 == #

#DOWNLOAD

NHIS04.instructions <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Program_Code/NHIS/2004/sampleadult/samadult.sas"
NHIS04.fileloc <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/2004/sampleadult/samadult.exe"
#store as df
NHIS04_SA <- read.SAScii(NHIS04.fileloc, NHIS04.instructions, zipped = TRUE)
#save as RDS file
saveRDS(NHIS04_SA,
        file = "NHIS04_SA.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

#CLEANING

#Some exploration
str(NHIS04_SA)
tail(NHIS04_SA)
glimpse(NHIS04_SA)
colnames(NHIS04_SA)

#no stratum

## -- end 2004 here -- ##

#_____________________________________________________________________________
#_____________________________________________________________________________
#_____________________________________________________________________________


# == 2003 == #

#DOWNLOAD

NHIS03.instructions <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Program_Code/NHIS/2003/samadult.sas"
NHIS03.fileloc <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/2003/samadult.exe"
#store as df
NHIS03_SA <- read.SAScii(NHIS03.fileloc, NHIS03.instructions, zipped = TRUE)
#save as RDS file
saveRDS(NHIS03_SA,
        file = "NHIS03_SA.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

#CLEANING

#Some exploration
str(NHIS03_SA)
tail(NHIS03_SA)
glimpse(NHIS03_SA)
colnames(NHIS03_SA)

#Select variables
NHIS03 <- select(NHIS03_SA,
                 HHX, FMX, PX,
                 REGION,
                 STRATUM, PSU, SRVY_YR, WTFA_SA,
                 SEX, AGE_P,
                 ALL_SA,
                 JNTSYMP, JMTHP1, JMTHP2, JMTHP3, JMTHP4, JMTHP5, JMTHP6, JMTHP7, JMTHP8, JMTHP9, JMTHP10, JMTHP11, JMTHP12, JMTHP13, JMTHP14, JMTHP15, JMTHP16, JMTHP17,
                 PAINLB,
                 VIGFREQW, VIGMIN, MODFREQW, MODMIN,
                 BMI,
                 HYPEV)

str(NHIS03)
tail(NHIS03)
glimpse(NHIS03)
colnames(NHIS03)

#joint symptoms recode
table(NHIS03$JNTSYMP)
#recode
NHIS03$JNTSYMP <- recode(NHIS03$JNTSYMP,
                         "1" = "1",
                         "2" = "0",
                         "7" = "7",
                         "9" = "9")
(NHIS03$JNTSYMP <- unknownToNA(NHIS03$JNTSYMP, unknown = c("7", "9")))
table(NHIS03$JNTSYMP)
NHIS03$JNTSYMP <- as.factor(NHIS03$JNTSYMP)

#low back pain recode
table(NHIS03$PAINLB)
#recode
NHIS03$PAINLB <- recode(NHIS03$PAINLB,
                        "1" = "1",
                        "2" = "0")
table(NHIS03$PAINLB)
NHIS03$PAINLB <- as.factor(NHIS03$PAINLB)

#region
table(NHIS03$REGION)
NHIS03$REGION <- recode(NHIS03$REGION,
                        "1" = "Northeast",
                        "2" = "Midwest",
                        "3" = "South",
                        "4" = "West")
table(NHIS03$REGION)

#age
table(NHIS03$AGE_P)



NHIS03$AGE_P <- recode(NHIS03$AGE_P,
                       "18" = "18 to 24",
                       "19" = "18 to 24",
                       "20" = "18 to 24",
                       "21" = "18 to 24",
                       "22" = "18 to 24",
                       "23" = "18 to 24",
                       "24" = "18 to 24",
                       "25" = "25 to 29",
                       "26" = "25 to 29",
                       "27" = "25 to 29",
                       "28" = "25 to 29",
                       "29" = "25 to 29",
                       "30" = "30 to 34",
                       "31" = "30 to 34",
                       "32" = "30 to 34",
                       "33" = "30 to 34",
                       "34" = "30 to 34",
                       "35" = "35 to 39",
                       "36" = "35 to 39",
                       "37" = "35 to 39",
                       "38" = "35 to 39", 
                       "39" = "35 to 39",
                       "40" = "40 to 44",
                       "41" = "40 to 44",
                       "42" = "40 to 44",
                       "43" = "40 to 44",
                       "44" = "40 to 44",
                       "45" = "45 to 49",
                       "46" = "45 to 49",
                       "47" = "45 to 49",
                       "48" = "45 to 49",
                       "49" = "45 to 49",
                       "50" = "50 to 54",
                       "51" = "50 to 54",
                       "52" = "50 to 54",
                       "53" = "50 to 54",
                       "54" = "50 to 54",
                       "55" = "55 to 59",
                       "56" = "55 to 59",
                       "57" = "55 to 59",
                       "58" = "55 to 59",
                       "59" = "55 to 59",
                       "60" = "60 to 64",
                       "61" = "60 to 64",
                       "62" = "60 to 64",
                       "63" = "60 to 64",
                       "64" = "60 to 64",
                       "65" = "65 to 69",
                       "66" = "65 to 69",
                       "67" = "65 to 69",
                       "68" = "65 to 69",
                       "69" = "65 to 69",
                       "70" = "70 and above",
                       "71" = "70 and above",
                       "72" = "70 and above",
                       "73" = "70 and above",
                       "74" = "70 and above",
                       "75" = "70 and above",
                       "76" = "70 and above",
                       "77" = "70 and above",
                       "78" = "70 and above",
                       "79" = "70 and above",
                       "80" = "70 and above",
                       "81" = "70 and above",
                       "82" = "70 and above",
                       "83" = "70 and above",
                       "84" = "70 and above",
                       "85" = "70 and above")
table(NHIS03$AGE_P)
NHIS03$AGE_P <- as.factor(NHIS03$AGE_P)

#sex
NHIS03$SEX <- recode(NHIS03$SEX,
                     "1" = "Male",
                     "2" = "Female")
table(NHIS03$SEX)
NHIS03$SEX <- as.factor(NHIS03$SEX)

#employment status
NHIS03$ALL_SA <- recode(NHIS03$ALL_SA,
                        "1" = "Working for pay at a job/business",
                        "2" = "2",
                        "3" = "Not working",
                        "4" = "Not working")
NHIS03$ALL_SA <- unknownToNA(NHIS03$ALL_SA, unknown = c("2", "9"))
table(NHIS03$ALL_SA)
NHIS03$ALL_SA <- as.factor(NHIS03$ALL_SA)

#affected joing areas quick clean
NHIS03$JMTHP1 <- unknownToNA(NHIS03$JMTHP1, unknown = 7:9)
NHIS03$JMTHP1 <- as.factor(NHIS03$JMTHP1)

NHIS03$JMTHP2 <- unknownToNA(NHIS03$JMTHP2, unknown = 7:9)
NHIS03$JMTHP2 <- as.factor(NHIS03$JMTHP2)

NHIS03$JMTHP3 <- unknownToNA(NHIS03$JMTHP3, unknown = 7:9)
NHIS03$JMTHP3 <- as.factor(NHIS03$JMTHP3)

NHIS03$JMTHP4 <- unknownToNA(NHIS03$JMTHP4, unknown = 7:9)
NHIS03$JMTHP4 <- as.factor(NHIS03$JMTHP4)

NHIS03$JMTHP5 <- unknownToNA(NHIS03$JMTHP5, unknown = 7:9)
NHIS03$JMTHP5 <- as.factor(NHIS03$JMTHP5)

NHIS03$JMTHP6 <- unknownToNA(NHIS03$JMTHP6, unknown = 7:9)
NHIS03$JMTHP6 <- as.factor(NHIS03$JMTHP6)

NHIS03$JMTHP7 <- unknownToNA(NHIS03$JMTHP7, unknown = 7:9)
NHIS03$JMTHP7 <- as.factor(NHIS03$JMTHP7)

NHIS03$JMTHP8 <- unknownToNA(NHIS03$JMTHP8, unknown = 7:9)
NHIS03$JMTHP8 <- as.factor(NHIS03$JMTHP8)

NHIS03$JMTHP9 <- unknownToNA(NHIS03$JMTHP9, unknown = 7:9)
NHIS03$JMTHP9 <- as.factor(NHIS03$JMTHP9)

NHIS03$JMTHP10 <- unknownToNA(NHIS03$JMTHP10, unknown = 7:9)
NHIS03$JMTHP10 <- as.factor(NHIS03$JMTHP10)

NHIS03$JMTHP11 <- unknownToNA(NHIS03$JMTHP11, unknown = 7:9)
NHIS03$JMTHP11 <- as.factor(NHIS03$JMTHP11)

NHIS03$JMTHP12 <- unknownToNA(NHIS03$JMTHP12, unknown = 7:9)
NHIS03$JMTHP12 <- as.factor(NHIS03$JMTHP12)

NHIS03$JMTHP13 <- unknownToNA(NHIS03$JMTHP13, unknown = 7:9)
NHIS03$JMTHP13 <- as.factor(NHIS03$JMTHP13)

NHIS03$JMTHP14 <- unknownToNA(NHIS03$JMTHP14, unknown = 7:9)
NHIS03$JMTHP14 <- as.factor(NHIS03$JMTHP14)

NHIS03$JMTHP15 <- unknownToNA(NHIS03$JMTHP15, unknown = 7:9)
NHIS03$JMTHP15 <- as.factor(NHIS03$JMTHP15)

NHIS03$JMTHP16 <- unknownToNA(NHIS03$JMTHP16, unknown = 7:9)
NHIS03$JMTHP16 <- as.factor(NHIS03$JMTHP16)

NHIS03$JMTHP17 <- unknownToNA(NHIS03$JMTHP17, unknown = 7:9)
NHIS03$JMTHP17 <- as.factor(NHIS03$JMTHP17)

#physical activity
#vigorous PA frequency
table(NHIS03$VIGFREQW)
NHIS03$VIGFREQW <- as.numeric(NHIS03$VIGFREQW)

#vigorous PA duration
table(NHIS03$VIGMIN)
NHIS03$VIGMIN <- as.numeric(NHIS03$VIGMIN)

#moderate pa frequency
table(NHIS03$MODFREQW)
NHIS03$MODFREQW <- as.numeric(NHIS03$MODFREQW)

#moderate PA duration
table(NHIS03$MODMIN)
NHIS03$MODMIN <- as.numeric(NHIS03$MODMIN)

#Make PA
#Physicially active = light/moderate pa >=5x/wk & >=30min OR Vig >=3x/wk & >=20min, else not
NHIS03$PAC <- ifelse(NHIS03$MODFREQW >= 5 & NHIS03$MODFREQW <= 28 & NHIS03$MODMIN >= 30 & NHIS03$MODMIN < 997, "Physically active",
                     ifelse(NHIS03$VIGFREQW >= 3 & NHIS03$VIGFREQW <= 28 & NHIS03$VIGMIN >= 20 & NHIS03$VIGMIN < 997, "Physically active", "Not physically active"))

#BMI
#change all 99.99 (unknown values) to NA
NHIS03$BMI <- unknownToNA(NHIS03$BMI, unknown = c("99.99"))
#quick check to be sure that there are no more potential unknowns
NHIS03 %>% top_n(10, BMI)


#no copd

#ht
NHIS03$HYPEV <- recode(NHIS03$HYPEV,
                       "1" = "1",
                       "2" = "0")
table(NHIS03$HYPEV)
NHIS03$HYPEV <- as.factor(NHIS03$HYPEV)

#________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#for establishing unweighted sample size only, not for design object creation
NHIS03_JS <- subset(NHIS03,
                    !is.na(WTFA_SA) &
                      !is.na(STRATUM) &
                      !is.na(PSU) &
                      !is.na(JNTSYMP))

table(subset(NHIS03_JS, AGE_P == "18 to 24")$SEX)
table(subset(NHIS03_JS, AGE_P == "25 to 29")$SEX)
table(subset(NHIS03_JS, AGE_P == "30 to 34")$SEX)
table(subset(NHIS03_JS, AGE_P == "35 to 39")$SEX)
table(subset(NHIS03_JS, AGE_P == "40 to 44")$SEX)
table(subset(NHIS03_JS, AGE_P == "45 to 49")$SEX)
table(subset(NHIS03_JS, AGE_P == "50 to 54")$SEX)
table(subset(NHIS03_JS, AGE_P == "55 to 59")$SEX)
table(subset(NHIS03_JS, AGE_P == "60 to 64")$SEX)
table(subset(NHIS03_JS, AGE_P == "65 to 69")$SEX)
table(subset(NHIS03_JS, AGE_P == "70 and above")$SEX)


#for establishing unweighted sample size only, not for design object creation
NHIS03_LBP <- subset(NHIS03,
                     !is.na(WTFA_SA) &
                       !is.na(STRATUM) &
                       !is.na(PSU) &
                       !is.na(PAINLB))

table(subset(NHIS03_LBP, AGE_P == "18 to 24")$SEX)
table(subset(NHIS03_LBP, AGE_P == "25 to 29")$SEX)
table(subset(NHIS03_LBP, AGE_P == "30 to 34")$SEX)
table(subset(NHIS03_LBP, AGE_P == "35 to 39")$SEX)
table(subset(NHIS03_LBP, AGE_P == "40 to 44")$SEX)
table(subset(NHIS03_LBP, AGE_P == "45 to 49")$SEX)
table(subset(NHIS03_LBP, AGE_P == "50 to 54")$SEX)
table(subset(NHIS03_LBP, AGE_P == "55 to 59")$SEX)
table(subset(NHIS03_LBP, AGE_P == "60 to 64")$SEX)
table(subset(NHIS03_LBP, AGE_P == "65 to 69")$SEX)
table(subset(NHIS03_LBP, AGE_P == "70 and above")$SEX)


#make sure all complex survey requirements are available
NHIS03_SA_dataset <- subset(NHIS03,
                            !is.na(WTFA_SA) &
                              !is.na(STRATUM) &
                              !is.na(PSU))

#Check that the sum of the weights is equal to the US population
sum(NHIS03_SA_dataset$WTFA_SA)
#The sum of the weights is 213 042 220, which is acceptable

length(unique(NHIS03_SA_dataset[["PSU"]]))
#The number of unique PSU's in the data is 2

#Check the number of unique strata
length(unique(NHIS03_SA_dataset[["STRATUM"]]))
#The number of unique strata is 339

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHIS03_DO <- svydesign(ids = ~1,
                       weights = ~WTFA_SA,
                       strata = ~STRATUM,
                       nest = TRUE,
                       data = NHIS03_SA_dataset)

#Save design object as RDS file for ease of retrieval for analysis, thereby avoiding running the data download and the design object creation code multiple times
saveRDS(NHIS03_DO,
        file = "NHIS03_DO.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

#_____________________________________________________________________________
#_____________________________________________________________________________
#_____________________________________________________________________________

# == 2002 == #

#DOWNLOAD

NHIS02.instructions <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Program_Code/NHIS/2002/samadult.sas"
NHIS02.fileloc <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/2002/samadult.exe"
#store as df
NHIS02_SA <- read.SAScii(NHIS02.fileloc, NHIS02.instructions, zipped = TRUE)
#save as RDS file
saveRDS(NHIS02_SA,
        file = "NHIS02_SA.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

#CLEANING

#Some exploration
str(NHIS02_SA)
tail(NHIS02_SA)
glimpse(NHIS02_SA)
colnames(NHIS02_SA)

#Select variables
NHIS02 <- select(NHIS02_SA,
                 HHX, FMX, PX,
                 REGION,
                 STRATUM, PSU, SRVY_YR, WTFA_SA,
                 SEX, AGE_P,
                 ALL_SA,
                 JNTSYMP, JMTHP1, JMTHP2, JMTHP3, JMTHP4, JMTHP5, JMTHP6, JMTHP7, JMTHP8, JMTHP9, JMTHP10, JMTHP11, JMTHP12, JMTHP13, JMTHP14, JMTHP15, JMTHP16, JMTHP17,
                 PAINLB,
                 VIGFREQW, VIGMIN, MODFREQW, MODMIN,
                 BMI,
                 HYPEV)

str(NHIS02)
tail(NHIS02)
glimpse(NHIS02)
colnames(NHIS02)

#joint symptoms recode
table(NHIS02$JNTSYMP)
#recode
NHIS02$JNTSYMP <- recode(NHIS02$JNTSYMP,
                         "1" = "1",
                         "2" = "0",
                         "7" = "7",
                         "9" = "9")
(NHIS02$JNTSYMP <- unknownToNA(NHIS02$JNTSYMP, unknown = c("7", "9")))
table(NHIS02$JNTSYMP)
NHIS02$JNTSYMP <- as.factor(NHIS02$JNTSYMP)

#low back pain recode
table(NHIS02$PAINLB)
#recode
NHIS02$PAINLB <- recode(NHIS02$PAINLB,
                        "1" = "1",
                        "2" = "0")
table(NHIS02$PAINLB)
NHIS02$PAINLB <- as.factor(NHIS02$PAINLB)

#region
table(NHIS02$REGION)
NHIS02$REGION <- recode(NHIS02$REGION,
                        "1" = "Northeast",
                        "2" = "Midwest",
                        "3" = "South",
                        "4" = "West")
table(NHIS02$REGION)

#age
table(NHIS02$AGE_P)



NHIS02$AGE_P <- recode(NHIS02$AGE_P,
                       "18" = "18 to 24",
                       "19" = "18 to 24",
                       "20" = "18 to 24",
                       "21" = "18 to 24",
                       "22" = "18 to 24",
                       "23" = "18 to 24",
                       "24" = "18 to 24",
                       "25" = "25 to 29",
                       "26" = "25 to 29",
                       "27" = "25 to 29",
                       "28" = "25 to 29",
                       "29" = "25 to 29",
                       "30" = "30 to 34",
                       "31" = "30 to 34",
                       "32" = "30 to 34",
                       "33" = "30 to 34",
                       "34" = "30 to 34",
                       "35" = "35 to 39",
                       "36" = "35 to 39",
                       "37" = "35 to 39",
                       "38" = "35 to 39", 
                       "39" = "35 to 39",
                       "40" = "40 to 44",
                       "41" = "40 to 44",
                       "42" = "40 to 44",
                       "43" = "40 to 44",
                       "44" = "40 to 44",
                       "45" = "45 to 49",
                       "46" = "45 to 49",
                       "47" = "45 to 49",
                       "48" = "45 to 49",
                       "49" = "45 to 49",
                       "50" = "50 to 54",
                       "51" = "50 to 54",
                       "52" = "50 to 54",
                       "53" = "50 to 54",
                       "54" = "50 to 54",
                       "55" = "55 to 59",
                       "56" = "55 to 59",
                       "57" = "55 to 59",
                       "58" = "55 to 59",
                       "59" = "55 to 59",
                       "60" = "60 to 64",
                       "61" = "60 to 64",
                       "62" = "60 to 64",
                       "63" = "60 to 64",
                       "64" = "60 to 64",
                       "65" = "65 to 69",
                       "66" = "65 to 69",
                       "67" = "65 to 69",
                       "68" = "65 to 69",
                       "69" = "65 to 69",
                       "70" = "70 and above",
                       "71" = "70 and above",
                       "72" = "70 and above",
                       "73" = "70 and above",
                       "74" = "70 and above",
                       "75" = "70 and above",
                       "76" = "70 and above",
                       "77" = "70 and above",
                       "78" = "70 and above",
                       "79" = "70 and above",
                       "80" = "70 and above",
                       "81" = "70 and above",
                       "82" = "70 and above",
                       "83" = "70 and above",
                       "84" = "70 and above",
                       "85" = "70 and above")
table(NHIS02$AGE_P)
NHIS02$AGE_P <- as.factor(NHIS02$AGE_P)

#sex
NHIS02$SEX <- recode(NHIS02$SEX,
                     "1" = "Male",
                     "2" = "Female")
table(NHIS02$SEX)
NHIS02$SEX <- as.factor(NHIS02$SEX)

#employment status
NHIS02$ALL_SA <- recode(NHIS02$ALL_SA,
                        "1" = "Working for pay at a job/business",
                        "2" = "2",
                        "3" = "Not working",
                        "4" = "Not working")
NHIS02$ALL_SA <- unknownToNA(NHIS02$ALL_SA, unknown = c("2", "9"))
table(NHIS02$ALL_SA)
NHIS02$ALL_SA <- as.factor(NHIS02$ALL_SA)

#affected joing areas quick clean
NHIS02$JMTHP1 <- unknownToNA(NHIS02$JMTHP1, unknown = 7:9)
NHIS02$JMTHP1 <- as.factor(NHIS02$JMTHP1)

NHIS02$JMTHP2 <- unknownToNA(NHIS02$JMTHP2, unknown = 7:9)
NHIS02$JMTHP2 <- as.factor(NHIS02$JMTHP2)

NHIS02$JMTHP3 <- unknownToNA(NHIS02$JMTHP3, unknown = 7:9)
NHIS02$JMTHP3 <- as.factor(NHIS02$JMTHP3)

NHIS02$JMTHP4 <- unknownToNA(NHIS02$JMTHP4, unknown = 7:9)
NHIS02$JMTHP4 <- as.factor(NHIS02$JMTHP4)

NHIS02$JMTHP5 <- unknownToNA(NHIS02$JMTHP5, unknown = 7:9)
NHIS02$JMTHP5 <- as.factor(NHIS02$JMTHP5)

NHIS02$JMTHP6 <- unknownToNA(NHIS02$JMTHP6, unknown = 7:9)
NHIS02$JMTHP6 <- as.factor(NHIS02$JMTHP6)

NHIS02$JMTHP7 <- unknownToNA(NHIS02$JMTHP7, unknown = 7:9)
NHIS02$JMTHP7 <- as.factor(NHIS02$JMTHP7)

NHIS02$JMTHP8 <- unknownToNA(NHIS02$JMTHP8, unknown = 7:9)
NHIS02$JMTHP8 <- as.factor(NHIS02$JMTHP8)

NHIS02$JMTHP9 <- unknownToNA(NHIS02$JMTHP9, unknown = 7:9)
NHIS02$JMTHP9 <- as.factor(NHIS02$JMTHP9)

NHIS02$JMTHP10 <- unknownToNA(NHIS02$JMTHP10, unknown = 7:9)
NHIS02$JMTHP10 <- as.factor(NHIS02$JMTHP10)

NHIS02$JMTHP11 <- unknownToNA(NHIS02$JMTHP11, unknown = 7:9)
NHIS02$JMTHP11 <- as.factor(NHIS02$JMTHP11)

NHIS02$JMTHP12 <- unknownToNA(NHIS02$JMTHP12, unknown = 7:9)
NHIS02$JMTHP12 <- as.factor(NHIS02$JMTHP12)

NHIS02$JMTHP13 <- unknownToNA(NHIS02$JMTHP13, unknown = 7:9)
NHIS02$JMTHP13 <- as.factor(NHIS02$JMTHP13)

NHIS02$JMTHP14 <- unknownToNA(NHIS02$JMTHP14, unknown = 7:9)
NHIS02$JMTHP14 <- as.factor(NHIS02$JMTHP14)

NHIS02$JMTHP15 <- unknownToNA(NHIS02$JMTHP15, unknown = 7:9)
NHIS02$JMTHP15 <- as.factor(NHIS02$JMTHP15)

NHIS02$JMTHP16 <- unknownToNA(NHIS02$JMTHP16, unknown = 7:9)
NHIS02$JMTHP16 <- as.factor(NHIS02$JMTHP16)

NHIS02$JMTHP17 <- unknownToNA(NHIS02$JMTHP17, unknown = 7:9)
NHIS02$JMTHP17 <- as.factor(NHIS02$JMTHP17)

#physical activity
#vigorous PA frequency
table(NHIS02$VIGFREQW)
NHIS02$VIGFREQW <- as.numeric(NHIS02$VIGFREQW)

#vigorous PA duration
table(NHIS02$VIGMIN)
NHIS02$VIGMIN <- as.numeric(NHIS02$VIGMIN)

#moderate pa frequency
table(NHIS02$MODFREQW)
NHIS02$MODFREQW <- as.numeric(NHIS02$MODFREQW)

#moderate PA duration
table(NHIS02$MODMIN)
NHIS02$MODMIN <- as.numeric(NHIS02$MODMIN)

#Make PA
#Physicially active = light/moderate pa >=5x/wk & >=30min OR Vig >=3x/wk & >=20min, else not
NHIS02$PAC <- ifelse(NHIS02$MODFREQW >= 5 & NHIS02$MODFREQW <= 28 & NHIS02$MODMIN >= 30 & NHIS02$MODMIN < 997, "Physically active",
                     ifelse(NHIS02$VIGFREQW >= 3 & NHIS02$VIGFREQW <= 28 & NHIS02$VIGMIN >= 20 & NHIS02$VIGMIN < 997, "Physically active", "Not physically active"))

#BMI
#change all 99.99 (unknown values) to NA
NHIS02$BMI <- unknownToNA(NHIS02$BMI, unknown = c("99.99"))
#quick check to be sure that there are no more potential unknowns
NHIS02 %>% top_n(10, BMI)


#no copd

#ht
NHIS02$HYPEV <- recode(NHIS02$HYPEV,
                       "1" = "1",
                       "2" = "0")
table(NHIS02$HYPEV)
NHIS02$HYPEV <- as.factor(NHIS02$HYPEV)

#________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#for establishing unweighted sample size only, not for design object creation
NHIS02_JS <- subset(NHIS02,
                    !is.na(WTFA_SA) &
                      !is.na(STRATUM) &
                      !is.na(PSU) &
                      !is.na(JNTSYMP))

table(subset(NHIS02_JS, AGE_P == "18 to 24")$SEX)
table(subset(NHIS02_JS, AGE_P == "25 to 29")$SEX)
table(subset(NHIS02_JS, AGE_P == "30 to 34")$SEX)
table(subset(NHIS02_JS, AGE_P == "35 to 39")$SEX)
table(subset(NHIS02_JS, AGE_P == "40 to 44")$SEX)
table(subset(NHIS02_JS, AGE_P == "45 to 49")$SEX)
table(subset(NHIS02_JS, AGE_P == "50 to 54")$SEX)
table(subset(NHIS02_JS, AGE_P == "55 to 59")$SEX)
table(subset(NHIS02_JS, AGE_P == "60 to 64")$SEX)
table(subset(NHIS02_JS, AGE_P == "65 to 69")$SEX)
table(subset(NHIS02_JS, AGE_P == "70 and above")$SEX)


#for establishing unweighted sample size only, not for design object creation
NHIS02_LBP <- subset(NHIS02,
                     !is.na(WTFA_SA) &
                       !is.na(STRATUM) &
                       !is.na(PSU) &
                       !is.na(PAINLB))

table(subset(NHIS02_LBP, AGE_P == "18 to 24")$SEX)
table(subset(NHIS02_LBP, AGE_P == "25 to 29")$SEX)
table(subset(NHIS02_LBP, AGE_P == "30 to 34")$SEX)
table(subset(NHIS02_LBP, AGE_P == "35 to 39")$SEX)
table(subset(NHIS02_LBP, AGE_P == "40 to 44")$SEX)
table(subset(NHIS02_LBP, AGE_P == "45 to 49")$SEX)
table(subset(NHIS02_LBP, AGE_P == "50 to 54")$SEX)
table(subset(NHIS02_LBP, AGE_P == "55 to 59")$SEX)
table(subset(NHIS02_LBP, AGE_P == "60 to 64")$SEX)
table(subset(NHIS02_LBP, AGE_P == "65 to 69")$SEX)
table(subset(NHIS02_LBP, AGE_P == "70 and above")$SEX)


#make sure all complex survey requirements are available
NHIS02_SA_dataset <- subset(NHIS02,
                            !is.na(WTFA_SA) &
                              !is.na(STRATUM) &
                              !is.na(PSU))

#Check that the sum of the weights is equal to the US population
sum(NHIS02_SA_dataset$WTFA_SA)
#The sum of the weights is 205 825 095, which is acceptable

length(unique(NHIS02_SA_dataset[["PSU"]]))
#The number of unique PSU's in the data is 2

#Check the number of unique strata
length(unique(NHIS02_SA_dataset[["STRATUM"]]))
#The number of unique strata is 339

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHIS02_DO <- svydesign(ids = ~1,
                       weights = ~WTFA_SA,
                       strata = ~STRATUM,
                       nest = TRUE,
                       data = NHIS02_SA_dataset)

#Save design object as RDS file for ease of retrieval for analysis, thereby avoiding running the data download and the design object creation code multiple times
saveRDS(NHIS02_DO,
        file = "NHIS02_DO.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

#____________________________________________________________________________
#____________________________________________________________________________
#____________________________________________________________________________

# == 2001 == #

#DOWNLOAD

NHIS01.instructions <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Program_Code/NHIS/2001/samadult.sas"
NHIS01.fileloc <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/2001/samadult.exe"
#store as df
NHIS01_SA <- read.SAScii(NHIS01.fileloc, NHIS01.instructions, zipped = TRUE)
#save as RDS file
saveRDS(NHIS01_SA,
        file = "NHIS01_SA.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

#CLEANING

#From 2001 and back, only data on low back pain due to the joint symptom question defintion changing to 1 year time frame
#Furthermore, the employment question in Sample Adult questionnaire also changes time frame

#Some exploration
str(NHIS01_SA)
tail(NHIS01_SA)
glimpse(NHIS01_SA)
colnames(NHIS01_SA)

#Select variables
NHIS01 <- select(NHIS01_SA,
                 HHX, FMX, PX,
                 REGION,
                 STRATUM, PSU, SRVY_YR, WTFA_SA,
                 SEX, AGE_P,
                 ALL_SA,
                 PAINLB,
                 VIGFREQW, VIGMIN, MODFREQW, MODMIN,
                 BMI,
                 HYPEV)

str(NHIS01)
tail(NHIS01)
glimpse(NHIS01)
colnames(NHIS01)

#low back pain recode
table(NHIS01$PAINLB)
#recode
NHIS01$PAINLB <- recode(NHIS01$PAINLB,
                        "1" = "1",
                        "2" = "0")
table(NHIS01$PAINLB)
NHIS01$PAINLB <- as.factor(NHIS01$PAINLB)

#region
table(NHIS01$REGION)
NHIS01$REGION <- recode(NHIS01$REGION,
                        "1" = "Northeast",
                        "2" = "Midwest",
                        "3" = "South",
                        "4" = "West")
table(NHIS01$REGION)

#age
table(NHIS01$AGE_P)



NHIS01$AGE_P <- recode(NHIS01$AGE_P,
                       "18" = "18 to 24",
                       "19" = "18 to 24",
                       "20" = "18 to 24",
                       "21" = "18 to 24",
                       "22" = "18 to 24",
                       "23" = "18 to 24",
                       "24" = "18 to 24",
                       "25" = "25 to 29",
                       "26" = "25 to 29",
                       "27" = "25 to 29",
                       "28" = "25 to 29",
                       "29" = "25 to 29",
                       "30" = "30 to 34",
                       "31" = "30 to 34",
                       "32" = "30 to 34",
                       "33" = "30 to 34",
                       "34" = "30 to 34",
                       "35" = "35 to 39",
                       "36" = "35 to 39",
                       "37" = "35 to 39",
                       "38" = "35 to 39", 
                       "39" = "35 to 39",
                       "40" = "40 to 44",
                       "41" = "40 to 44",
                       "42" = "40 to 44",
                       "43" = "40 to 44",
                       "44" = "40 to 44",
                       "45" = "45 to 49",
                       "46" = "45 to 49",
                       "47" = "45 to 49",
                       "48" = "45 to 49",
                       "49" = "45 to 49",
                       "50" = "50 to 54",
                       "51" = "50 to 54",
                       "52" = "50 to 54",
                       "53" = "50 to 54",
                       "54" = "50 to 54",
                       "55" = "55 to 59",
                       "56" = "55 to 59",
                       "57" = "55 to 59",
                       "58" = "55 to 59",
                       "59" = "55 to 59",
                       "60" = "60 to 64",
                       "61" = "60 to 64",
                       "62" = "60 to 64",
                       "63" = "60 to 64",
                       "64" = "60 to 64",
                       "65" = "65 to 69",
                       "66" = "65 to 69",
                       "67" = "65 to 69",
                       "68" = "65 to 69",
                       "69" = "65 to 69",
                       "70" = "70 and above",
                       "71" = "70 and above",
                       "72" = "70 and above",
                       "73" = "70 and above",
                       "74" = "70 and above",
                       "75" = "70 and above",
                       "76" = "70 and above",
                       "77" = "70 and above",
                       "78" = "70 and above",
                       "79" = "70 and above",
                       "80" = "70 and above",
                       "81" = "70 and above",
                       "82" = "70 and above",
                       "83" = "70 and above",
                       "84" = "70 and above",
                       "85" = "70 and above")
table(NHIS01$AGE_P)
NHIS01$AGE_P <- as.factor(NHIS01$AGE_P)

#sex
NHIS01$SEX <- recode(NHIS01$SEX,
                     "1" = "Male",
                     "2" = "Female")
table(NHIS01$SEX)
NHIS01$SEX <- as.factor(NHIS01$SEX)

#employment status - changed time frame and phrasing 

#physical activity
#vigorous PA frequency
table(NHIS01$VIGFREQW)
NHIS01$VIGFREQW <- as.numeric(NHIS01$VIGFREQW)

#vigorous PA duration
table(NHIS01$VIGMIN)
NHIS01$VIGMIN <- as.numeric(NHIS01$VIGMIN)

#moderate pa frequency
table(NHIS01$MODFREQW)
NHIS01$MODFREQW <- as.numeric(NHIS01$MODFREQW)

#moderate PA duration
table(NHIS01$MODMIN)
NHIS01$MODMIN <- as.numeric(NHIS01$MODMIN)

#Make PA
#Physicially active = light/moderate pa >=5x/wk & >=30min OR Vig >=3x/wk & >=20min, else not
NHIS01$PAC <- ifelse(NHIS01$MODFREQW >= 5 & NHIS01$MODFREQW <= 28 & NHIS01$MODMIN >= 30 & NHIS01$MODMIN < 997, "Physically active",
                     ifelse(NHIS01$VIGFREQW >= 3 & NHIS01$VIGFREQW <= 28 & NHIS01$VIGMIN >= 20 & NHIS01$VIGMIN < 997, "Physically active", "Not physically active"))


#BMI
#change all 99.99 (unknown values) to NA
NHIS01$BMI <- unknownToNA(NHIS01$BMI, unknown = c("99.99"))
#quick check to be sure that there are no more potential unknowns
NHIS01 %>% top_n(10, BMI)


#no copd

#ht
NHIS01$HYPEV <- recode(NHIS01$HYPEV,
                       "1" = "1",
                       "2" = "0")
table(NHIS01$HYPEV)
NHIS01$HYPEV <- as.factor(NHIS01$HYPEV)

#________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#for establishing unweighted sample size only, not for design object creation
NHIS01_LBP <- subset(NHIS01,
                     !is.na(WTFA_SA) &
                       !is.na(STRATUM) &
                       !is.na(PSU) &
                       !is.na(PAINLB))

table(subset(NHIS01_LBP, AGE_P == "18 to 24")$SEX)
table(subset(NHIS01_LBP, AGE_P == "25 to 29")$SEX)
table(subset(NHIS01_LBP, AGE_P == "30 to 34")$SEX)
table(subset(NHIS01_LBP, AGE_P == "35 to 39")$SEX)
table(subset(NHIS01_LBP, AGE_P == "40 to 44")$SEX)
table(subset(NHIS01_LBP, AGE_P == "45 to 49")$SEX)
table(subset(NHIS01_LBP, AGE_P == "50 to 54")$SEX)
table(subset(NHIS01_LBP, AGE_P == "55 to 59")$SEX)
table(subset(NHIS01_LBP, AGE_P == "60 to 64")$SEX)
table(subset(NHIS01_LBP, AGE_P == "65 to 69")$SEX)
table(subset(NHIS01_LBP, AGE_P == "70 and above")$SEX)


#make sure all complex survey requirements are available
NHIS01_SA_dataset <- subset(NHIS01,
                            !is.na(WTFA_SA) &
                              !is.na(STRATUM) &
                              !is.na(PSU))

#Check that the sum of the weights is equal to the US population
sum(NHIS01_SA_dataset$WTFA_SA)
#The sum of the weights is 203 831 923, which is acceptable

length(unique(NHIS01_SA_dataset[["PSU"]]))
#The number of unique PSU's in the data is 2

#Check the number of unique strata
length(unique(NHIS01_SA_dataset[["STRATUM"]]))
#The number of unique strata is 339

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHIS01_DO <- svydesign(ids = ~1,
                       weights = ~WTFA_SA,
                       strata = ~STRATUM,
                       nest = TRUE,
                       data = NHIS01_SA_dataset)

#Save design object as RDS file for ease of retrieval for analysis, thereby avoiding running the data download and the design object creation code multiple times
saveRDS(NHIS01_DO,
        file = "NHIS01_DO.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

#_______________________________________________________________________________
#_______________________________________________________________________________
#_______________________________________________________________________________


# == 2000 == #

#DOWNLOAD

NHIS00.instructions <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Program_Code/NHIS/2000/samadult.sas"
NHIS00.fileloc <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/2000/samadult.exe"
#store as df
NHIS00_SA <- read.SAScii(NHIS00.fileloc, NHIS00.instructions, zipped = TRUE)
#save as RDS file
saveRDS(NHIS00_SA,
        file = "NHIS00_SA.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

#CLEANING

#Some exploration
str(NHIS00_SA)
tail(NHIS00_SA)
glimpse(NHIS00_SA)
colnames(NHIS00_SA)

#Select variables
NHIS00 <- select(NHIS00_SA,
                 HHX, FMX, PX,
                 REGION,
                 STRATUM, PSU, SRVY_YR, WTFA_SA,
                 SEX, AGE_P,
                 PAINLB,
                 VIGFREQW, VIGMIN, MODFREQW, MODMIN,
                 BMI,
                 HYPEV)

str(NHIS00)
tail(NHIS00)
glimpse(NHIS00)
colnames(NHIS00)

#low back pain recode
table(NHIS00$PAINLB)
#recode
NHIS00$PAINLB <- recode(NHIS00$PAINLB,
                        "1" = "1",
                        "2" = "0")
table(NHIS00$PAINLB)
NHIS00$PAINLB <- as.factor(NHIS00$PAINLB)

#region
table(NHIS00$REGION)
NHIS00$REGION <- recode(NHIS00$REGION,
                        "1" = "Northeast",
                        "2" = "Midwest",
                        "3" = "South",
                        "4" = "West")
table(NHIS00$REGION)

#age
table(NHIS00$AGE_P)



NHIS00$AGE_P <- recode(NHIS00$AGE_P,
                       "18" = "18 to 24",
                       "19" = "18 to 24",
                       "20" = "18 to 24",
                       "21" = "18 to 24",
                       "22" = "18 to 24",
                       "23" = "18 to 24",
                       "24" = "18 to 24",
                       "25" = "25 to 29",
                       "26" = "25 to 29",
                       "27" = "25 to 29",
                       "28" = "25 to 29",
                       "29" = "25 to 29",
                       "30" = "30 to 34",
                       "31" = "30 to 34",
                       "32" = "30 to 34",
                       "33" = "30 to 34",
                       "34" = "30 to 34",
                       "35" = "35 to 39",
                       "36" = "35 to 39",
                       "37" = "35 to 39",
                       "38" = "35 to 39", 
                       "39" = "35 to 39",
                       "40" = "40 to 44",
                       "41" = "40 to 44",
                       "42" = "40 to 44",
                       "43" = "40 to 44",
                       "44" = "40 to 44",
                       "45" = "45 to 49",
                       "46" = "45 to 49",
                       "47" = "45 to 49",
                       "48" = "45 to 49",
                       "49" = "45 to 49",
                       "50" = "50 to 54",
                       "51" = "50 to 54",
                       "52" = "50 to 54",
                       "53" = "50 to 54",
                       "54" = "50 to 54",
                       "55" = "55 to 59",
                       "56" = "55 to 59",
                       "57" = "55 to 59",
                       "58" = "55 to 59",
                       "59" = "55 to 59",
                       "60" = "60 to 64",
                       "61" = "60 to 64",
                       "62" = "60 to 64",
                       "63" = "60 to 64",
                       "64" = "60 to 64",
                       "65" = "65 to 69",
                       "66" = "65 to 69",
                       "67" = "65 to 69",
                       "68" = "65 to 69",
                       "69" = "65 to 69",
                       "70" = "70 and above",
                       "71" = "70 and above",
                       "72" = "70 and above",
                       "73" = "70 and above",
                       "74" = "70 and above",
                       "75" = "70 and above",
                       "76" = "70 and above",
                       "77" = "70 and above",
                       "78" = "70 and above",
                       "79" = "70 and above",
                       "80" = "70 and above",
                       "81" = "70 and above",
                       "82" = "70 and above",
                       "83" = "70 and above",
                       "84" = "70 and above",
                       "85" = "70 and above")
table(NHIS00$AGE_P)
NHIS00$AGE_P <- as.factor(NHIS00$AGE_P)

#sex
NHIS00$SEX <- recode(NHIS00$SEX,
                     "1" = "Male",
                     "2" = "Female")
table(NHIS00$SEX)
NHIS00$SEX <- as.factor(NHIS00$SEX)

#physical activity
#vigorous PA frequency
table(NHIS00$VIGFREQW)
NHIS00$VIGFREQW <- as.numeric(NHIS00$VIGFREQW)

#vigorous PA duration
table(NHIS00$VIGMIN)
NHIS00$VIGMIN <- as.numeric(NHIS00$VIGMIN)

#moderate pa frequency
table(NHIS00$MODFREQW)
NHIS00$MODFREQW <- as.numeric(NHIS00$MODFREQW)

#moderate PA duration
table(NHIS00$MODMIN)
NHIS00$MODMIN <- as.numeric(NHIS00$MODMIN)

#Make PA
#Physicially active = light/moderate pa >=5x/wk & >=30min OR Vig >=3x/wk & >=20min, else not
NHIS00$PAC <- ifelse(NHIS00$MODFREQW >= 5 & NHIS00$MODFREQW <= 28 & NHIS00$MODMIN >= 30 & NHIS00$MODMIN < 997, "Physically active",
                     ifelse(NHIS00$VIGFREQW >= 3 & NHIS00$VIGFREQW <= 28 & NHIS00$VIGMIN >= 20 & NHIS00$VIGMIN < 997, "Physically active", "Not physically active"))


#BMI
#change all 99.99 (unknown values) to NA
NHIS00$BMI <- unknownToNA(NHIS00$BMI, unknown = c("99.99"))
#quick check to be sure that there are no more potential unknowns
NHIS00 %>% top_n(10, BMI)


#no copd

#ht
NHIS00$HYPEV <- recode(NHIS00$HYPEV,
                       "1" = "1",
                       "2" = "0")
table(NHIS00$HYPEV)
NHIS00$HYPEV <- as.factor(NHIS00$HYPEV)

#________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#for establishing unweighted sample size only, not for design object creation
NHIS00_LBP <- subset(NHIS00,
                     !is.na(WTFA_SA) &
                       !is.na(STRATUM) &
                       !is.na(PSU) &
                       !is.na(PAINLB))

table(subset(NHIS00_LBP, AGE_P == "18 to 24")$SEX)
table(subset(NHIS00_LBP, AGE_P == "25 to 29")$SEX)
table(subset(NHIS00_LBP, AGE_P == "30 to 34")$SEX)
table(subset(NHIS00_LBP, AGE_P == "35 to 39")$SEX)
table(subset(NHIS00_LBP, AGE_P == "40 to 44")$SEX)
table(subset(NHIS00_LBP, AGE_P == "45 to 49")$SEX)
table(subset(NHIS00_LBP, AGE_P == "50 to 54")$SEX)
table(subset(NHIS00_LBP, AGE_P == "55 to 59")$SEX)
table(subset(NHIS00_LBP, AGE_P == "60 to 64")$SEX)
table(subset(NHIS00_LBP, AGE_P == "65 to 69")$SEX)
table(subset(NHIS00_LBP, AGE_P == "70 and above")$SEX)


#make sure all complex survey requirements are available
NHIS00_SA_dataset <- subset(NHIS00,
                            !is.na(WTFA_SA) &
                              !is.na(STRATUM) &
                              !is.na(PSU))
#Check that the sum of the weights is equal to the US population
sum(NHIS00_SA_dataset$WTFA_SA)
#The sum of the weights is 201 698 125, which is acceptable

length(unique(NHIS00_SA_dataset[["PSU"]]))
#The number of unique PSU's in the data is 2

#Check the number of unique strata
length(unique(NHIS00_SA_dataset[["STRATUM"]]))
#The number of unique strata is 339

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHIS00_DO <- svydesign(ids = ~1,
                       weights = ~WTFA_SA,
                       strata = ~STRATUM,
                       nest = TRUE,
                       data = NHIS00_SA_dataset)

#Save design object as RDS file for ease of retrieval for analysis, thereby avoiding running the data download and the design object creation code multiple times
saveRDS(NHIS00_DO,
        file = "NHIS00_DO.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


#____________________________________________________________________________
#____________________________________________________________________________
#____________________________________________________________________________


# == 1999 == #

#DOWNLOAD

NHIS99.instructions <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Program_Code/NHIS/1999/samadult.sas"
NHIS99.fileloc <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/1999/SAMADULT.EXE"
#store as df
NHIS99_SA <- read.SAScii(NHIS99.fileloc, NHIS99.instructions, zipped = TRUE)
#save as RDS file
saveRDS(NHIS99_SA,
        file = "NHIS99_SA.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

#CLEANING

#Some exploration
str(NHIS99_SA)
tail(NHIS99_SA)
glimpse(NHIS99_SA)
colnames(NHIS99_SA)

#Select variables
NHIS99 <- select(NHIS99_SA,
                 HHX, FMX, PX,
                 REGION,
                 STRATUM, PSU, SRVY_YR, WTFA_SA,
                 SEX, AGE_P,
                 PAINLB,
                 VIGFREQW, VIGMIN, MODFREQW, MODMIN,
                 BMI,
                 HYPEV)

str(NHIS99)
tail(NHIS99)
glimpse(NHIS99)
colnames(NHIS99)

#low back pain recode
table(NHIS99$PAINLB)
#recode
NHIS99$PAINLB <- recode(NHIS99$PAINLB,
                        "1" = "1",
                        "2" = "0")
table(NHIS99$PAINLB)
NHIS99$PAINLB <- as.factor(NHIS99$PAINLB)

#region
table(NHIS99$REGION)
NHIS99$REGION <- recode(NHIS99$REGION,
                        "1" = "Northeast",
                        "2" = "Midwest",
                        "3" = "South",
                        "4" = "West")
table(NHIS99$REGION)

#age
table(NHIS99$AGE_P)



NHIS99$AGE_P <- recode(NHIS99$AGE_P,
                       "18" = "18 to 24",
                       "19" = "18 to 24",
                       "20" = "18 to 24",
                       "21" = "18 to 24",
                       "22" = "18 to 24",
                       "23" = "18 to 24",
                       "24" = "18 to 24",
                       "25" = "25 to 29",
                       "26" = "25 to 29",
                       "27" = "25 to 29",
                       "28" = "25 to 29",
                       "29" = "25 to 29",
                       "30" = "30 to 34",
                       "31" = "30 to 34",
                       "32" = "30 to 34",
                       "33" = "30 to 34",
                       "34" = "30 to 34",
                       "35" = "35 to 39",
                       "36" = "35 to 39",
                       "37" = "35 to 39",
                       "38" = "35 to 39", 
                       "39" = "35 to 39",
                       "40" = "40 to 44",
                       "41" = "40 to 44",
                       "42" = "40 to 44",
                       "43" = "40 to 44",
                       "44" = "40 to 44",
                       "45" = "45 to 49",
                       "46" = "45 to 49",
                       "47" = "45 to 49",
                       "48" = "45 to 49",
                       "49" = "45 to 49",
                       "50" = "50 to 54",
                       "51" = "50 to 54",
                       "52" = "50 to 54",
                       "53" = "50 to 54",
                       "54" = "50 to 54",
                       "55" = "55 to 59",
                       "56" = "55 to 59",
                       "57" = "55 to 59",
                       "58" = "55 to 59",
                       "59" = "55 to 59",
                       "60" = "60 to 64",
                       "61" = "60 to 64",
                       "62" = "60 to 64",
                       "63" = "60 to 64",
                       "64" = "60 to 64",
                       "65" = "65 to 69",
                       "66" = "65 to 69",
                       "67" = "65 to 69",
                       "68" = "65 to 69",
                       "69" = "65 to 69",
                       "70" = "70 and above",
                       "71" = "70 and above",
                       "72" = "70 and above",
                       "73" = "70 and above",
                       "74" = "70 and above",
                       "75" = "70 and above",
                       "76" = "70 and above",
                       "77" = "70 and above",
                       "78" = "70 and above",
                       "79" = "70 and above",
                       "80" = "70 and above",
                       "81" = "70 and above",
                       "82" = "70 and above",
                       "83" = "70 and above",
                       "84" = "70 and above",
                       "85" = "70 and above")
table(NHIS99$AGE_P)
NHIS99$AGE_P <- as.factor(NHIS99$AGE_P)

#sex
NHIS99$SEX <- recode(NHIS99$SEX,
                     "1" = "Male",
                     "2" = "Female")
table(NHIS99$SEX)
NHIS99$SEX <- as.factor(NHIS99$SEX)

#physical activity
#vigorous PA frequency
table(NHIS99$VIGFREQW)
NHIS99$VIGFREQW <- as.numeric(NHIS99$VIGFREQW)

#vigorous PA duration
table(NHIS99$VIGMIN)
NHIS99$VIGMIN <- as.numeric(NHIS99$VIGMIN)

#moderate pa frequency
table(NHIS99$MODFREQW)
NHIS99$MODFREQW <- as.numeric(NHIS99$MODFREQW)

#moderate PA duration
table(NHIS99$MODMIN)
NHIS99$MODMIN <- as.numeric(NHIS99$MODMIN)

#Make PA
#Physicially active = light/moderate pa >=5x/wk & >=30min OR Vig >=3x/wk & >=20min, else not
NHIS99$PAC <- ifelse(NHIS99$MODFREQW >= 5 & NHIS99$MODFREQW <= 28 & NHIS99$MODMIN >= 30 & NHIS99$MODMIN < 997, "Physically active",
                     ifelse(NHIS99$VIGFREQW >= 3 & NHIS99$VIGFREQW <= 28 & NHIS99$VIGMIN >= 20 & NHIS99$VIGMIN < 997, "Physically active", "Not physically active"))

#BMI
#change all 99.99 (unknown values) to NA
NHIS99$BMI <- unknownToNA(NHIS99$BMI, unknown = c("99.99"))
#quick check to be sure that there are no more potential unknowns
NHIS99 %>% top_n(10, BMI)


#no copd

#ht
NHIS99$HYPEV <- recode(NHIS99$HYPEV,
                       "1" = "1",
                       "2" = "0")
table(NHIS99$HYPEV)
NHIS99$HYPEV <- as.factor(NHIS99$HYPEV)

#________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#for establishing unweighted sample size only, not for design object creation
NHIS99_LBP <- subset(NHIS99,
                     !is.na(WTFA_SA) &
                       !is.na(STRATUM) &
                       !is.na(PSU) &
                       !is.na(PAINLB))

table(subset(NHIS99_LBP, AGE_P == "18 to 24")$SEX)
table(subset(NHIS99_LBP, AGE_P == "25 to 29")$SEX)
table(subset(NHIS99_LBP, AGE_P == "30 to 34")$SEX)
table(subset(NHIS99_LBP, AGE_P == "35 to 39")$SEX)
table(subset(NHIS99_LBP, AGE_P == "40 to 44")$SEX)
table(subset(NHIS99_LBP, AGE_P == "45 to 49")$SEX)
table(subset(NHIS99_LBP, AGE_P == "50 to 54")$SEX)
table(subset(NHIS99_LBP, AGE_P == "55 to 59")$SEX)
table(subset(NHIS99_LBP, AGE_P == "60 to 64")$SEX)
table(subset(NHIS99_LBP, AGE_P == "65 to 69")$SEX)
table(subset(NHIS99_LBP, AGE_P == "70 and above")$SEX)


#make sure all complex survey requirements are available
NHIS99_SA_dataset <- subset(NHIS99,
                            !is.na(WTFA_SA) &
                              !is.na(STRATUM) &
                              !is.na(PSU))


#Check that the sum of the weights is equal to the US population
sum(NHIS99_SA_dataset$WTFA_SA)
#The sum of the weights is 199 617 483, which is acceptable

length(unique(NHIS99_SA_dataset[["PSU"]]))
#The number of unique PSU's in the data is 2

#Check the number of unique strata
length(unique(NHIS99_SA_dataset[["STRATUM"]]))
#The number of unique strata is 339

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHIS99_DO <- svydesign(ids = ~1,
                       weights = ~WTFA_SA,
                       strata = ~STRATUM,
                       nest = TRUE,
                       data = NHIS99_SA_dataset)

#Save design object as RDS file for ease of retrieval for analysis, thereby avoiding running the data download and the design object creation code multiple times
saveRDS(NHIS99_DO,
        file = "NHIS99_DO.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

#___________________________________________________________________________________
#___________________________________________________________________________________
#___________________________________________________________________________________


# == 1998 == #

#DOWNLOAD

NHIS98.instructions <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Program_Code/NHIS/1998/samadult.sas"
NHIS98.fileloc <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/1998/samadult.exe"
#store as df
NHIS98_SA <- read.SAScii(NHIS98.fileloc, NHIS98.instructions, zipped = TRUE)
#save as RDS file
saveRDS(NHIS98_SA,
        file = "NHIS98_SA.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

#CLEANING

#Some exploration
str(NHIS98_SA)
tail(NHIS98_SA)
glimpse(NHIS98_SA)
colnames(NHIS98_SA)

#Select variables
NHIS98 <- select(NHIS98_SA,
                 HHX, FMX, PX,
                 REGION,
                 STRATUM, PSU, SRVY_YR, WTFA_SA,
                 SEX, AGE_P,
                 PAINLB,
                 VIGFREQW, VIGMIN, MODFREQW, MODMIN,
                 BMI,
                 HYPEV)

str(NHIS98)
tail(NHIS98)
glimpse(NHIS98)
colnames(NHIS98)

#low back pain recode
table(NHIS98$PAINLB)
#recode
NHIS98$PAINLB <- recode(NHIS98$PAINLB,
                        "1" = "1",
                        "2" = "0")
table(NHIS98$PAINLB)
NHIS98$PAINLB <- as.factor(NHIS98$PAINLB)

#region
table(NHIS98$REGION)
NHIS98$REGION <- recode(NHIS98$REGION,
                        "1" = "Northeast",
                        "2" = "Midwest",
                        "3" = "South",
                        "4" = "West")
table(NHIS98$REGION)

#age
table(NHIS98$AGE_P)



NHIS98$AGE_P <- recode(NHIS98$AGE_P,
                       "18" = "18 to 24",
                       "19" = "18 to 24",
                       "20" = "18 to 24",
                       "21" = "18 to 24",
                       "22" = "18 to 24",
                       "23" = "18 to 24",
                       "24" = "18 to 24",
                       "25" = "25 to 29",
                       "26" = "25 to 29",
                       "27" = "25 to 29",
                       "28" = "25 to 29",
                       "29" = "25 to 29",
                       "30" = "30 to 34",
                       "31" = "30 to 34",
                       "32" = "30 to 34",
                       "33" = "30 to 34",
                       "34" = "30 to 34",
                       "35" = "35 to 39",
                       "36" = "35 to 39",
                       "37" = "35 to 39",
                       "38" = "35 to 39", 
                       "39" = "35 to 39",
                       "40" = "40 to 44",
                       "41" = "40 to 44",
                       "42" = "40 to 44",
                       "43" = "40 to 44",
                       "44" = "40 to 44",
                       "45" = "45 to 49",
                       "46" = "45 to 49",
                       "47" = "45 to 49",
                       "48" = "45 to 49",
                       "49" = "45 to 49",
                       "50" = "50 to 54",
                       "51" = "50 to 54",
                       "52" = "50 to 54",
                       "53" = "50 to 54",
                       "54" = "50 to 54",
                       "55" = "55 to 59",
                       "56" = "55 to 59",
                       "57" = "55 to 59",
                       "58" = "55 to 59",
                       "59" = "55 to 59",
                       "60" = "60 to 64",
                       "61" = "60 to 64",
                       "62" = "60 to 64",
                       "63" = "60 to 64",
                       "64" = "60 to 64",
                       "65" = "65 to 69",
                       "66" = "65 to 69",
                       "67" = "65 to 69",
                       "68" = "65 to 69",
                       "69" = "65 to 69",
                       "70" = "70 and above",
                       "71" = "70 and above",
                       "72" = "70 and above",
                       "73" = "70 and above",
                       "74" = "70 and above",
                       "75" = "70 and above",
                       "76" = "70 and above",
                       "77" = "70 and above",
                       "78" = "70 and above",
                       "79" = "70 and above",
                       "80" = "70 and above",
                       "81" = "70 and above",
                       "82" = "70 and above",
                       "83" = "70 and above",
                       "84" = "70 and above",
                       "85" = "70 and above")
table(NHIS98$AGE_P)
NHIS98$AGE_P <- as.factor(NHIS98$AGE_P)

#sex
NHIS98$SEX <- recode(NHIS98$SEX,
                     "1" = "Male",
                     "2" = "Female")
table(NHIS98$SEX)
NHIS98$SEX <- as.factor(NHIS98$SEX)

#physical activity
#vigorous PA frequency
table(NHIS98$VIGFREQW)
NHIS98$VIGFREQW <- as.numeric(NHIS98$VIGFREQW)

#vigorous PA duration
table(NHIS98$VIGMIN)
NHIS98$VIGMIN <- as.numeric(NHIS98$VIGMIN)

#moderate pa frequency
table(NHIS98$MODFREQW)
NHIS98$MODFREQW <- as.numeric(NHIS98$MODFREQW)

#moderate PA duration
table(NHIS98$MODMIN)
NHIS98$MODMIN <- as.numeric(NHIS98$MODMIN)

#Make PA
#Physicially active = light/moderate pa >=5x/wk & >=30min OR Vig >=3x/wk & >=20min, else not
NHIS98$PAC <- ifelse(NHIS98$MODFREQW >= 5 & NHIS98$MODFREQW <= 28 & NHIS98$MODMIN >= 30 & NHIS98$MODMIN < 997, "Physically active",
                     ifelse(NHIS98$VIGFREQW >= 3 & NHIS98$VIGFREQW <= 28 & NHIS98$VIGMIN >= 20 & NHIS98$VIGMIN < 997, "Physically active", "Not physically active"))

#BMI
#change all 99.99 (unknown values) to NA
NHIS98$BMI <- unknownToNA(NHIS98$BMI, unknown = c("99.99"))
#quick check to be sure that there are no more potential unknowns
NHIS98 %>% top_n(10, BMI)


#no copd

#ht
NHIS98$HYPEV <- recode(NHIS98$HYPEV,
                       "1" = "1",
                       "2" = "0")
table(NHIS98$HYPEV)
NHIS98$HYPEV <- as.factor(NHIS98$HYPEV)

#________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#for establishing unweighted sample size only, not for design object creation
NHIS98_LBP <- subset(NHIS98,
                     !is.na(WTFA_SA) &
                       !is.na(STRATUM) &
                       !is.na(PSU) &
                       !is.na(PAINLB))

table(subset(NHIS98_LBP, AGE_P == "18 to 24")$SEX)
table(subset(NHIS98_LBP, AGE_P == "25 to 29")$SEX)
table(subset(NHIS98_LBP, AGE_P == "30 to 34")$SEX)
table(subset(NHIS98_LBP, AGE_P == "35 to 39")$SEX)
table(subset(NHIS98_LBP, AGE_P == "40 to 44")$SEX)
table(subset(NHIS98_LBP, AGE_P == "45 to 49")$SEX)
table(subset(NHIS98_LBP, AGE_P == "50 to 54")$SEX)
table(subset(NHIS98_LBP, AGE_P == "55 to 59")$SEX)
table(subset(NHIS98_LBP, AGE_P == "60 to 64")$SEX)
table(subset(NHIS98_LBP, AGE_P == "65 to 69")$SEX)
table(subset(NHIS98_LBP, AGE_P == "70 and above")$SEX)

#make sure all complex survey requirements are available
NHIS98_SA_dataset <- subset(NHIS98,
                            !is.na(WTFA_SA) &
                              !is.na(STRATUM) &
                              !is.na(PSU))
#Check that the sum of the weights is equal to the US population
sum(NHIS98_SA_dataset$WTFA_SA)
#The sum of the weights is 197 303 607, which is acceptable

length(unique(NHIS98_SA_dataset[["PSU"]]))
#The number of unique PSU's in the data is 2

#Check the number of unique strata
length(unique(NHIS98_SA_dataset[["STRATUM"]]))
#The number of unique strata is 339

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHIS98_DO <- svydesign(ids = ~1,
                       weights = ~WTFA_SA,
                       strata = ~STRATUM,
                       nest = TRUE,
                       data = NHIS98_SA_dataset)

#Save design object as RDS file for ease of retrieval for analysis, thereby avoiding running the data download and the design object creation code multiple times
saveRDS(NHIS98_DO,
        file = "NHIS98_DO.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

#____________________________________________________________________
#____________________________________________________________________
#____________________________________________________________________


# == 1997 == #

#DOWNLOAD

NHIS97.instructions <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Program_Code/NHIS/1997/samadult.sas"
NHIS97.fileloc <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/1997/samadult.exe"
#store as df
NHIS97_SA <- read.SAScii(NHIS97.fileloc, NHIS97.instructions, zipped = TRUE)
#save as RDS file
saveRDS(NHIS97_SA,
        file = "NHIS97_SA.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

#CLEANING

#Some exploration
str(NHIS97_SA)
tail(NHIS97_SA)
glimpse(NHIS97_SA)
colnames(NHIS97_SA)

#Select variables
NHIS97 <- select(NHIS97_SA,
                 HHX, FMX, PX,
                 REGION,
                 STRATUM, PSU, SRVY_YR, WTFA_SA,
                 SEX, AGE_P,
                 PAINLB,
                 VIGFREQW, VIGMIN, MODFREQW, MODMIN,
                 BMI,
                 HYPEV)

str(NHIS97)
tail(NHIS97)
glimpse(NHIS97)
colnames(NHIS97)

#low back pain recode
table(NHIS97$PAINLB)
#recode
NHIS97$PAINLB <- recode(NHIS97$PAINLB,
                        "1" = "1",
                        "2" = "0")
table(NHIS97$PAINLB)
NHIS97$PAINLB <- as.factor(NHIS97$PAINLB)

#region
table(NHIS97$REGION)
NHIS97$REGION <- recode(NHIS97$REGION,
                        "1" = "Northeast",
                        "2" = "Midwest",
                        "3" = "South",
                        "4" = "West")
table(NHIS97$REGION)

#age
table(NHIS97$AGE_P)



NHIS97$AGE_P <- recode(NHIS97$AGE_P,
                       "18" = "18 to 24",
                       "19" = "18 to 24",
                       "20" = "18 to 24",
                       "21" = "18 to 24",
                       "22" = "18 to 24",
                       "23" = "18 to 24",
                       "24" = "18 to 24",
                       "25" = "25 to 29",
                       "26" = "25 to 29",
                       "27" = "25 to 29",
                       "28" = "25 to 29",
                       "29" = "25 to 29",
                       "30" = "30 to 34",
                       "31" = "30 to 34",
                       "32" = "30 to 34",
                       "33" = "30 to 34",
                       "34" = "30 to 34",
                       "35" = "35 to 39",
                       "36" = "35 to 39",
                       "37" = "35 to 39",
                       "38" = "35 to 39", 
                       "39" = "35 to 39",
                       "40" = "40 to 44",
                       "41" = "40 to 44",
                       "42" = "40 to 44",
                       "43" = "40 to 44",
                       "44" = "40 to 44",
                       "45" = "45 to 49",
                       "46" = "45 to 49",
                       "47" = "45 to 49",
                       "48" = "45 to 49",
                       "49" = "45 to 49",
                       "50" = "50 to 54",
                       "51" = "50 to 54",
                       "52" = "50 to 54",
                       "53" = "50 to 54",
                       "54" = "50 to 54",
                       "55" = "55 to 59",
                       "56" = "55 to 59",
                       "57" = "55 to 59",
                       "58" = "55 to 59",
                       "59" = "55 to 59",
                       "60" = "60 to 64",
                       "61" = "60 to 64",
                       "62" = "60 to 64",
                       "63" = "60 to 64",
                       "64" = "60 to 64",
                       "65" = "65 to 69",
                       "66" = "65 to 69",
                       "67" = "65 to 69",
                       "68" = "65 to 69",
                       "69" = "65 to 69",
                       "70" = "70 and above",
                       "71" = "70 and above",
                       "72" = "70 and above",
                       "73" = "70 and above",
                       "74" = "70 and above",
                       "75" = "70 and above",
                       "76" = "70 and above",
                       "77" = "70 and above",
                       "78" = "70 and above",
                       "79" = "70 and above",
                       "80" = "70 and above",
                       "81" = "70 and above",
                       "82" = "70 and above",
                       "83" = "70 and above",
                       "84" = "70 and above",
                       "85" = "70 and above")
table(NHIS97$AGE_P)
NHIS97$AGE_P <- as.factor(NHIS97$AGE_P)

#sex
NHIS97$SEX <- recode(NHIS97$SEX,
                     "1" = "Male",
                     "2" = "Female")
table(NHIS97$SEX)
NHIS97$SEX <- as.factor(NHIS97$SEX)

#physical activity
#vigorous PA frequency
table(NHIS97$VIGFREQW)
NHIS97$VIGFREQW <- as.numeric(NHIS97$VIGFREQW)

#vigorous PA duration
table(NHIS97$VIGMIN)
NHIS97$VIGMIN <- as.numeric(NHIS97$VIGMIN)

#moderate pa frequency
table(NHIS97$MODFREQW)
NHIS97$MODFREQW <- as.numeric(NHIS97$MODFREQW)

#moderate PA duration
table(NHIS97$MODMIN)
NHIS97$MODMIN <- as.numeric(NHIS97$MODMIN)

#Make PA
#Physicially active = light/moderate pa >=5x/wk & >=30min OR Vig >=3x/wk & >=20min, else not
NHIS97$PAC <- ifelse(NHIS97$MODFREQW >= 5 & NHIS97$MODFREQW <= 28 & NHIS97$MODMIN >= 30 & NHIS97$MODMIN < 997, "Physically active",
                     ifelse(NHIS97$VIGFREQW >= 3 & NHIS97$VIGFREQW <= 28 & NHIS97$VIGMIN >= 20 & NHIS97$VIGMIN < 997, "Physically active", "Not physically active"))

#BMI
#change all 99.99 (unknown values) to NA
NHIS97$BMI <- unknownToNA(NHIS97$BMI, unknown = c("99.99"))
#quick check to be sure that there are no more potential unknowns
NHIS97 %>% top_n(10, BMI)


#no copd

#ht
NHIS97$HYPEV <- recode(NHIS97$HYPEV,
                       "1" = "1",
                       "2" = "0")
table(NHIS97$HYPEV)
NHIS97$HYPEV <- as.factor(NHIS97$HYPEV)

#________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#for establishing unweighted sample size only, not for design object creation
NHIS97_LBP <- subset(NHIS97,
                     !is.na(WTFA_SA) &
                       !is.na(STRATUM) &
                       !is.na(PSU) &
                       !is.na(PAINLB))

table(subset(NHIS97_LBP, AGE_P == "18 to 24")$SEX)
table(subset(NHIS97_LBP, AGE_P == "25 to 29")$SEX)
table(subset(NHIS97_LBP, AGE_P == "30 to 34")$SEX)
table(subset(NHIS97_LBP, AGE_P == "35 to 39")$SEX)
table(subset(NHIS97_LBP, AGE_P == "40 to 44")$SEX)
table(subset(NHIS97_LBP, AGE_P == "45 to 49")$SEX)
table(subset(NHIS97_LBP, AGE_P == "50 to 54")$SEX)
table(subset(NHIS97_LBP, AGE_P == "55 to 59")$SEX)
table(subset(NHIS97_LBP, AGE_P == "60 to 64")$SEX)
table(subset(NHIS97_LBP, AGE_P == "65 to 69")$SEX)
table(subset(NHIS97_LBP, AGE_P == "70 and above")$SEX)

#make sure all complex survey requirements are available
NHIS97_SA_dataset <- subset(NHIS97,
                            !is.na(WTFA_SA) &
                              !is.na(STRATUM) &
                              !is.na(PSU))

#Check that the sum of the weights is equal to the US population
sum(NHIS97_SA_dataset$WTFA_SA)
#The sum of the weights is 195 276 321, which is acceptable

length(unique(NHIS97_SA_dataset[["PSU"]]))
#The number of unique PSU's in the data is 2

#Check the number of unique strata
length(unique(NHIS97_SA_dataset[["STRATUM"]]))
#The number of unique strata is 339

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHIS97_DO <- svydesign(ids = ~1,
                       weights = ~WTFA_SA,
                       strata = ~STRATUM,
                       nest = TRUE,
                       data = NHIS97_SA_dataset)

#Save design object as RDS file for ease of retrieval for analysis, thereby avoiding running the data download and the design object creation code multiple times
saveRDS(NHIS97_DO,
        file = "NHIS97_DO.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

#___________________________________________________________________________________
#__________________ END NHIS DOWNLOADS & CLEANING HERE______________________________
#___________________________________________________________________________________
