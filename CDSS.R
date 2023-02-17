library(tidyverse)
library(readxl)
library(ggplot2)
library(dplyr)
library(ipumsr)
library(sf)
library(spData)
library(stringr)
library(openxlsx)

##California county shapefile
setwd("~/Documents/Coding LAB/California_Counties")

cali_geo <- st_read("Counties.shp")

ggplot(data = cali_geo) +
  geom_sf()

#ACS Data from 2019 IPUMS California 
setwd("~/Documents/Coding LAB")
ddi <- read_ipums_ddi("usa_00004.xml")
ACS_data <- read_ipums_micro(ddi)

summary(ACS_data)

##decrease to household with children
ACS_cnty <- ACS_data %>%
  filter(NCHILD > 0 & POVERTY < 81)  %>%
  select(YEAR, COUNTYFIP, HHINCOME, FAMSIZE, NCHILD, POVERTY)

##get a population count
ACS_indv <- sum(ACS_cnty$FAMSIZE)
ACS_all <- sum(ACS_data$FAMSIZE)

ACS_cnty %>%
  summarise(sum(FAMSIZE, na.rm = TRUE))

#26,871 total members of household in 2019 with children at poverty threshold 
#create poverty per household member
ACS_cnty$Poverty <- ACS_cnty$HHINCOME / ACS_cnty$FAMSIZE

cnty_pvrty <- ACS_cnty %>% 
  group_by(COUNTYFIP) %>% 
  summarise(Pct_Poverty = mean(Poverty))

all_cnty_wlfr <- ACS_data %>% 
  group_by(COUNTYICP) %>% 
  summarise(Poverty = mean(POVERTY))

##create FIP to match with CDSS County codes
cnty_pvrty$COUNTYFIP <- as.integer(cnty_pvrty$COUNTYFIP)

cnty_pvrty$COUNTYFIP <- paste0("060", cnty_pvrty$COUNTYFIP) 

####-------------------------------------------------------------------------------


##CDSS Data from fiscal year 2019-2020
##https://www.cdss.ca.gov/inforesources/research-and-data/calworks-data-tables/ca-237-cw 
CDSS_columnsNames <- read.xlsx("CDSS_data.xlsx", colNames = F, na.strings="*")

###Clean data sheet for all caseload info only
CDSS_clean <- CDSS_columnsNames[-1, ]

##Cases (all caseload) receiving cash grants
CDSS_clean <- CDSS_clean %>%
  select(2:6, 70:74)

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

##ALL CASELOAD for July 2019-July 2020 receiving cash grants
view(CDSS_clean)

##create a total column, first create intergers and NAs
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

##total cash assistance participants for 2020, july, end of fiscal year
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



##Cali Map in Counties
ggplot(data = cali_geo) +
  geom_sf(data = jly_cntys_geo, aes(fill = Total)) +
  labs(title = "Total Caseloads Receiving CashGrants By County",
       subtitle = "Data for July 2019",
       fill = element_blank(),
       caption = "Source: CDSS.gov") +
  theme_void()

aaaa <- CDSS_clean %>% 
  group_by("County Name") %>% 
  summarise(mean(is.na("Total")))


###Need to create a DF with fiscal year
CDSS_ACS <- left_join(jly_cntys_geo, cnty_pvrty, by = "COUNTYFIP")

ggplot(data = cali_geo) +
  geom_sf(data = CDSS_ACS, aes(fill = Pct_Poverty)) +
  labs(title = "County's Mean Poverty Level",
       subtitle = "Data for July 2019",
       fill = element_blank(),
       caption = "Source: CDSS.gov & ACS") +
  theme_void()


#####Removed
setwd("~/Documents/Coding LAB")

cnty_nm_cd <- read.csv("fips2county.tsv", sep = "\t")

cnty_nm_cd <- cnty_nm_cd %>%
  filter(StateAbbr == "CA") %>%
  select(CountyName, CountyFIPS)

##
cali_geo$COUNTY <- sub( ".", "", cali_geo$GeoID) 

cali_geo$COUNTY <- as.character(cali_geo$COUNTY)

data$COUNTY <- as.character(data$COUNTY)

cali_joined <- right_join(cali_geo, data, by = "COUNTY")

cali_joined_X <- cali_joined %>%
  group_by(COUNTY) %>%
  summarise(mean_inc = mean(poverty), n = n( ))

####get data. down to 58 counties
cali_58 <- cali_joined %>% left_join(cnty_nm_cd, cali_joined, by = "COUNTY")

cnty_nm_cd$COUNTY <- as.integer(cnty_nm_cd$CountyFIPS)
cali_joined$COUNTY <- as.integer(cali_joined$COUNTY)

cnty_nm_cd$COUNTY <- cnty_nm_cd$CountyFIPS
 



####removals 

