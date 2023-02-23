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

##California counties shape file
setwd("~/Documents/Coding LAB/California_Counties")

cali_geo <- st_read("Counties.shp")

ggplot(data = cali_geo) +
  geom_sf()

#ACS Data from 2019 IPUMS California 
setwd("~/Documents/Coding LAB")
ddi <- read_ipums_ddi("usa_00004.xml")
ACS_data <- read_ipums_micro(ddi)

summary(ACS_data)
##ACS data shows 39.5 million population in 2019-- coorect.
ACS_data %>%
  summarise(sum(PERWT, na.rm = TRUE))

##clean to decrease household with children and 80 below poverty level
ACS_clean <- ACS_data %>%
  filter(NCHILD > 0 & POVERTY < 81)  %>%
  select(YEAR, COUNTYFIP, HHWT, PERWT, HHINCOME, FAMSIZE, NCHILD, POVERTY)

##Household count -- in poverty with children population (i.e. eligible)
ACS_clean %>%
  summarise(sum(HHWT, na.rm = TRUE))
##737086 House Holds (multi-family homes)

##Person weight-- in poverty with children population (i.e. eligible)
ACS_clean %>%
  summarise(sum(PERWT, na.rm = TRUE))
##758243 People

##Income per person
ACS_clean$Poverty <- ACS_clean$HHINCOME / ACS_clean$PERWT

##populations mean income per person living in poverty. ACS only has 35 counties
ACS_cnty_pvrty <- ACS_clean %>% 
  group_by(COUNTYFIP) %>% 
  summarise(Pct_Poverty = mean(Poverty))

##create FIP to match with CDSS County codes
ACS_cnty_pvrty$COUNTYFIP <- as.integer(ACS_cnty_pvrty$COUNTYFIP)

ACS_cnty_pvrty$COUNTYFIP <- paste0("060", ACS_cnty_pvrty$COUNTYFIP) 

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
CDSS_clean$Total<-apply(cbind(CDSS_clean$'Zero Parent',
                              CDSS_clean$'Two Parent', 
                              CDSS_clean$'All Other',
                              CDSS_clean$'TANF Timed-Out',
                              CDSS_clean$'SN/FF/LTS'),1,
                        function(x) ifelse(all(is.na(x)),NA,sum(x,na.rm=T)))

##ALL CASELOAD for July 2019-July 2020 receiving cash grants
view(CDSS_clean)

##total cash assistance participants for 2020, july, end of fiscal year (i.e. numerator)
CDSS_clean %>% filter(Month == "6" & Year == "2020" & `County Name` == "Statewide") %>%
  select(Total)
##348,220

##select month of July 2019, for all 58 counties
CDSS_July_Cntys <- CDSS_clean %>% 
  filter(Month == 7)
##remove statewide info
CDSS_July_Cntys <- CDSS_July_Cntys[-1, ]

## Merge Cali_geo shapefile with July County caseload
CDSS_July_Cntys$CountyName <- CDSS_July_Cntys$'County Name'

jly_cntys_geo <- right_join(cali_geo, CDSS_July_Cntys, by = "CountyName")

jly_cntys_geo$COUNTYFIP <- jly_cntys_geo$GeoID



##Cali Counties map of count of cash grant recipients
ggplot(data = cali_geo) +
  geom_sf(data = jly_cntys_geo, aes(fill = Total)) +
  labs(title = "Total Caseloads Receiving Cash Grants By County",
       subtitle = "*Gray scale counties are missing in data collected.",
       fill = "Total Cases
(348,220)",
       caption = "Source: CDSS 2019-2020 Fiscal Year") + 
  scale_fill_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme_void() 

###data frame with county population below poverty (mean income per person = Pct_poverty
##only 35 counties have Pct_poverty, the rest are NA 
CDSS_ACS <- left_join(jly_cntys_geo, ACS_cnty_pvrty, by = "COUNTYFIP")

##map, but only 26 counties show because FIPS codes don't match
ggplot(data = cali_geo) +
  geom_sf(data = CDSS_ACS, aes(fill = Pct_Poverty)) +
  labs(title = "Mean Income Per Person Living in Poverty",
       subtitle = "ACS 2019 data only captures data from 35/58 counties.
Some counties are missing from mismatched geographical data points.",
       fill = "Avg. Income
Per Person",
       caption = "Source: IPUMS USA, ACS 2019") + 
  scale_fill_continuous(labels = dollar_format( )) +
  theme_void() 


####Total Eligible population 

##person count in eligible population
ACS_cnty_pop <- ACS_clean %>% 
  group_by(COUNTYFIP) %>% 
  summarise(Ppl_Pov = sum(PERWT))

##create FIP to match with CDSS County codes
ACS_cnty_pop$COUNTYFIP <- as.integer(ACS_cnty_pop$COUNTYFIP)

ACS_cnty_pop$COUNTYFIP <- paste0("060", ACS_cnty_pop$COUNTYFIP) 

x_CDSS_ACS <- left_join(jly_cntys_geo, ACS_cnty_pop, by = "COUNTYFIP")

ggplot(data = cali_geo) +
  geom_sf(data = x_CDSS_ACS, aes(fill = Ppl_Pov)) +
  labs(title = "Population 80% below poverty, with children",
       subtitle = "ACS 2019 data only captures data from 35/58 counties.
Some counties are missing from mismatched geographical data points.",
       fill = "Total People
(758,243)",
       caption = "Source: IPUMS USA, ACS 2019") + 
  scale_fill_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme_void() 



########Cases fallout rate per county 
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





