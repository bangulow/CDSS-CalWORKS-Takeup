library(tidyverse)
library(readxl)
library(ggplot2)
library(dplyr)
library(ipumsr)
library(sf)
library(spData)
library(stringr)
library(openxlsx)
library(scales)

##California counties shape file, (https://data.ca.gov/)
setwd("~/Documents/Coding LAB/California_Counties")

cali_geo <- st_read("Counties.shp")

ggplot(data = cali_geo) +
  geom_sf()

#ACS Data from 2019 (1 year) IPUMS-California (STATECODE:6) 
setwd("~/Documents/Coding LAB")
ddi <- read_ipums_ddi("usa_00004.xml")
ACS_data <- read_ipums_micro(ddi)

##ACS data shows 39.5 million population in 2019-- correct.
ACS_data %>%
  summarise(sum(PERWT, na.rm = TRUE))

##clean to decrease population to children and 80 below poverty level
ACS_chld <- ACS_data %>%
  filter(AGE < 18 & POVERTY <= 80)

ACS_chld %>%
  summarise(sum(PERWT, na.rm = TRUE))
##1,127,323

##filter for unique SERIAL numbers to capture households without repetition (i.e. siblings)
ACS_chld_dstnct <- ACS_chld %>% distinct(SERIAL, .keep_all = TRUE)

ACS_chld_dstnct %>% 
  summarise(sum(HHWT, na.rm = TRUE))
##483,225

#create data set to capture only households w/ adults below poverty using unique SERIAL
ACS_adlts <- ACS_data %>%
  filter(AGE >=18 & POVERTY <= 80)

ACS_adlts_dstnct <- ACS_adlts %>% distinct(SERIAL, .keep_all = TRUE)

ACS_adlts_dstnct %>% 
  summarise(sum(HHWT, na.rm = TRUE))
  ##2,159,624

##join adult and child poverty data sets for all households with children below poverty
ACS_join <- left_join(ACS_chld, ACS_adlts, by = "SERIAL")

ACS_join_dstnct <- ACS_join %>% distinct(SERIAL, .keep_all = TRUE)

ACS_join_dstnct %>% 
  summarise(sum(HHWT.x, na.rm = TRUE))
##483,225

ACS_join %>% 
  summarise(sum(PERWT.x, na.rm = TRUE))
##1,939,110  
  

##ACS new with citizenship variable (3 is non citizen, we assume .55 of non citizens are LPR, according to DHS 2015)
###___________________Includes Citizenship status 
#ACS Data from 2019 IPUMS California 
setwd("~/Documents/Coding LAB")
ddi <- read_ipums_ddi("usa_00005.xml")
ACS_data_ctzn <- read_ipums_micro(ddi)

##ACS data shows 39.5 million population in 2019-- correct.
ACS_data_ctzn %>%
  summarise(sum(PERWT, na.rm = TRUE))

##clean to decrease household with children and 80 below poverty level
ACS_chld_ctzn <- ACS_data_ctzn %>%
  filter(AGE < 18 & POVERTY <= 80)  %>%
  select(YEAR, SERIAL, COUNTYFIP, HHWT, PERWT, HHINCOME, FAMSIZE, NCHILD, POVERTY, CITIZEN)

##total children in poverty
ACS_chld_ctzn %>%
  summarise(sum(PERWT, na.rm = TRUE))
##1,127,323  

##adults in poverty 
ACS_adlts_ctzn <- ACS_data_ctzn %>%
  filter(AGE >=18 & POVERTY <= 80)

##join data sets for children and adults households <80 FPL
ACS_join_ctzn <- left_join(ACS_chld_ctzn, ACS_adlts_ctzn, by = "SERIAL")

###create .55 random sample of 1 citizen, 0 not citizen/not eligible
nonctzn <- ACS_join_ctzn %>%
  filter(CITIZEN.x == 3 | CITIZEN.y == 3)

nonctzn <- nonctzn %>% distinct(SERIAL, .keep_all = TRUE)

nonctzn %>%
  summarise(sum(HHWT.y, na.rm = TRUE))
##172,287

nonctzn %>%
  summarise(sum(HHWT.x, na.rm = TRUE))
##176,812

##about 174,000 HH are by noncitizens
174000 * .55
##95,700 are LPR

##subtract non citizen from total HH pop under poverty, add back in LPR
(483225 - 174000) + 95700
##404925

####-------------------------------------------------------------------------------

##CDSS Data from fiscal year 2019-2020
##https://www.cdss.ca.gov/inforesources/research-and-data/calworks-data-tables/ca-237-cw 
CDSS <- read.xlsx("CDSS_data.xlsx", colNames = F, na.strings="*")

###Clean data sheet for all caseload info only
CDSS_clean <- CDSS[-1, ]

##Cases (all caseload) receiving cash grants (section 8A)
CDSS_clean <- CDSS_clean %>%
  select(2:6, 70:74)

##Clean df
##rename values for new header
CDSS_clean$X70[CDSS_clean$X70 == '64'] <- 'Two Parent'
CDSS_clean$X71[CDSS_clean$X71 == '65'] <- 'Zero Parent'
CDSS_clean$X72[CDSS_clean$X72 == '66'] <- 'All Other'
CDSS_clean$X73[CDSS_clean$X73 == '67'] <- 'TANF  Timed-Out'
CDSS_clean$X74[CDSS_clean$X74 == '68'] <- 'SN/FF/LTS'

##make  row the header
names(CDSS_clean) <- CDSS_clean[4, ]

CDSS_clean <- CDSS_clean[-(1:4),]

##re-evalue name and number changes from header change
CDSS_clean$'Two Parent'[CDSS_clean$'Two Parent' == 'Two Parent'] <- '64' 
CDSS_clean$'Zero Parent'[CDSS_clean$'Zero Parent' == 'Zero Parent'] <- '65' 
CDSS_clean$'All Other'[CDSS_clean$'All Other' == 'All Other'] <- '66' 
CDSS_clean$'TANF  Timed-Out'[CDSS_clean$'TANF  Timed-Out' == 'TANF  Timed-Out'] <- '67'
CDSS_clean$'SN/FF/LTS'[CDSS_clean$'SN/FF/LTS' == 'SN/FF/LTS'] <- '68'


##create a total column, first create integers and NAs
CDSS_clean$'Two Parent' <- as.integer(CDSS_clean$'Two Parent')
CDSS_clean$'Zero Parent' <- as.integer(CDSS_clean$'Zero Parent')
CDSS_clean$'All Other' <- as.integer(CDSS_clean$'All Other')
CDSS_clean$'TANF  Timed-Out' <- as.integer(CDSS_clean$'TANF  Timed-Out')
CDSS_clean$'SN/FF/LTS' <- as.integer(CDSS_clean$'SN/FF/LTS')

##Create a total for all situations column per county & statewide
CDSS_clean <- replace(CDSS_clean,is.na(CDSS_clean),0)

CDSS_clean$Total <- rowSums(CDSS_clean[,6:10])


##total cash assistance CASELOADS for 2019, July, end of fiscal year (i.e. numerator)
CDSS_clean %>% filter(Month == "7" & Year == "2019" & `County Name` == "Statewide") %>%
  select(Total)
##366,281
##--------------------------------------------------------------------------------------
##total cash assistance PERSONS(adults and children) for 2019, July, 
##end of fiscal year (i.e. numerator)

CDSS_PERWT <- CDSS[-1, ]

##Cases (all caseload) receiving cash grants (section 8A)
CDSS_PERWT <- CDSS_PERWT %>%
  select(2:6, 75:82)

##Clean df
##rename values for new header
CDSS_PERWT$X75[CDSS_PERWT$X75 == '69'] <- 'chld_Two Parent'
CDSS_PERWT$X76[CDSS_PERWT$X76 == '70'] <- 'chld_Zero Parent'
CDSS_PERWT$X77[CDSS_PERWT$X77 == '71'] <- 'chld_All Other'
CDSS_PERWT$X78[CDSS_PERWT$X78 == '72'] <- 'chld_TANF  Timed-Out'
CDSS_PERWT$X79[CDSS_PERWT$X79 == '73'] <- 'chld_SN/FF/LTS'
CDSS_PERWT$X80[CDSS_PERWT$X80 == '74'] <- 'adlt_Two Parent'
CDSS_PERWT$X81[CDSS_PERWT$X81 == '75'] <- 'adlt_All Other'
CDSS_PERWT$X82[CDSS_PERWT$X82 == '76'] <- 'adlt_TANF  Timed-Out'

##make  row the header
names(CDSS_PERWT) <- CDSS_PERWT[4, ]

CDSS_PERWT <- CDSS_PERWT[-(1:4),]

###make NAs zeros to add cases 
CDSS_PERWT <- replace(CDSS_PERWT,is.na(CDSS_PERWT),0)

##re-value name and number changes from header change
CDSS_PERWT$'chld_Two Parent'[CDSS_PERWT$'chld_Two Parent' == 'chld_Two Parent'] <- '69' 
CDSS_PERWT$'chld_Zero Parent'[CDSS_PERWT$'chld_Zero Parent' == 'chld_Zero Parent'] <- '70' 
CDSS_PERWT$'chld_All Other'[CDSS_PERWT$'chld_All Other' == 'chld_All Other'] <- '71' 
CDSS_PERWT$'chld_TANF  Timed-Out'[CDSS_PERWT$'chld_TANF  Timed-Out' == 'chld_TANF  Timed-Out'] <- '72'
CDSS_PERWT$'chld_SN/FF/LTS'[CDSS_PERWT$'chld_SN/FF/LTS' == 'chld_SN/FF/LTS'] <- '73'
CDSS_PERWT$'adlt_Two Parent'[CDSS_PERWT$'adlt_Two Parent' == 'adltTwo Parent'] <- '74'
CDSS_PERWT$'adlt_All Other'[CDSS_PERWT$'adlt_All Other' == 'adlt_All Other'] <- '75'
CDSS_PERWT$'adlt_TANF  Timed-Out'[CDSS_PERWT$'adlt_TANF  Timed-Out' == 'adlt_TANF  Timed-Out'] <- '76'


##create a total column, first change to integers from characters

CDSS_PERWT$'chld_Two Parent' <- as.integer(CDSS_PERWT$'chld_Two Parent')
CDSS_PERWT$'chld_Zero Parent' <- as.integer(CDSS_PERWT$'chld_Zero Parent')
CDSS_PERWT$'chld_All Other' <- as.integer(CDSS_PERWT$'chld_All Other') 
CDSS_PERWT$'chld_TANF  Timed-Out'  <- as.integer(CDSS_PERWT$'chld_TANF  Timed-Out')
CDSS_PERWT$'chld_SN/FF/LTS'  <- as.integer(CDSS_PERWT$'chld_SN/FF/LTS')
CDSS_PERWT$'adlt_Two Parent'  <- as.integer(CDSS_PERWT$'adlt_Two Parent')
CDSS_PERWT$'adlt_All Other'  <- as.integer(CDSS_PERWT$'adlt_All Other')
CDSS_PERWT$'adlt_TANF  Timed-Out' <- as.integer(CDSS_PERWT$'adlt_TANF  Timed-Out')

##Create total row for all children and adults
CDSS_PERWT$Total <- rowSums(CDSS_PERWT[,6:13])

##total cash assistance PARTICIPANTS for 2019, July, end of fiscal year (i.e. numerator)
CDSS_PERWT %>% filter(Month == "7" & Year == "2019" & `County Name` == "Statewide") %>%
  select(Total)
##875,131


##---------------------------------------------------------------------------------------
##maps
##join CDSS with shapefile
CDSS_clean$CountyName <- CDSS_clean$'County Name'

cntys_geo <- right_join(cali_geo, CDSS_clean, by = "CountyName")

cntys_geo$COUNTYFIP <- cntys_geo$GeoID

cntys_geo <- cntys_geo %>% 
  filter(Month == 7)
##remove statewide info
cntys_geo <- cntys_geo[-59, ]

##Cali Counties map of count of cash grant recipients
ggplot(data = cali_geo) +
  geom_sf(data = cntys_geo, aes(fill = Total)) +
  labs(title = "Total Caseloads Receiving Cash Grants By County",
       subtitle = "*Some counties expressed as 0 for NA data in July.",
       fill = "Total Cases
(366,281)",
       caption = "Source: CDSS 2019-2020 Fiscal Year") + 
  scale_fill_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme_void() 

##select counties with 0, to show which ones
zero_cnts <- cntys_geo %>% select(Total, CountyName)


########Cases fallout rate per county ______________________________________________________________
CDSS_fallout <- read.xlsx("CDSS_data.xlsx", colNames = F, na.strings="*")

###Clean data sheet for all caseload info only
CDSS_fallout<- CDSS_fallout[-1, ]

##Cases (all caseload) receiving cash grants (section 8A)
CDSS_fallout <- CDSS_fallout %>%
  select(2:6, 13:16)

##rename values for new header
CDSS_fallout$X13[CDSS_fallout$X13 == '7'] <- 'Total Apps'
CDSS_fallout$X14[CDSS_fallout$X14 == '8'] <- 'Total Disposed'
CDSS_fallout$X15[CDSS_fallout$X15 == '9'] <- 'Approved'
CDSS_fallout$X16[CDSS_fallout$X16 == '10'] <- 'Denied'

##make  row the header
names(CDSS_fallout) <- CDSS_fallout[4, ]
CDSS_fallout <- CDSS_fallout[-(1:4),]

##get rid of NA
CDSS_fallout <- replace(CDSS_fallout, is.na(CDSS_fallout), 0)


##create a total column, first create integers and NAs
CDSS_fallout$'Total Apps' <- as.integer(CDSS_fallout$'Total Apps')
CDSS_fallout$'Total Disposed' <- as.integer(CDSS_fallout$'Total Disposed')
CDSS_fallout$'Approved' <- as.integer(CDSS_fallout$'Approved')
CDSS_fallout$'Denied' <- as.integer(CDSS_fallout$'Denied')

##Percent denied
CDSS_fallout$Perc_Denied <- (CDSS_fallout$'Denied' / CDSS_fallout$'Total Apps') * 100

##Percent denied by 58 counties
CDSS_fallout <- CDSS_fallout %>% 
  filter(Month == 7)

##remove statewide info
CDSS_fallout <- CDSS_fallout[-1, ]

##top 10 counties that deny apps the most
top_10_deny <- CDSS_fallout %>% 
  select("County Name", Perc_Denied) %>% 
  arrange(desc(Perc_Denied)) %>%
  head(10)
