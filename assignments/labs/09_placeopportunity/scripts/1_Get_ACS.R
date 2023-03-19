library(tidycensus)
library(tidyverse)

#B01001: Age, Population ----
#https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_14_5YR_B01001&prodType=table

B01001_Vars<-c("B01001_001",
               "B01001_002",
               "B01001_003",
               "B01001_004",
               "B01001_005",
               "B01001_006",
               "B01001_020",
               "B01001_021",
               "B01001_022",
               "B01001_023",
               "B01001_024",
               "B01001_025",
               "B01001_027",
               "B01001_028",
               "B01001_029",
               "B01001_030",
               "B01001_044",
               "B01001_045",
               "B01001_046",
               "B01001_047",
               "B01001_048",
               "B01001_049")

B01001 <- get_acs(geography = "tract", state = state, variables = B01001_Vars, survey = survey, year = DL_Year, output = "wide")

B01001 <- B01001 |> 
  mutate(under18 = (B01001_003E
                   +B01001_004E
                   +B01001_005E
                   +B01001_006E
                   +B01001_027E
                   +B01001_028E
                   +B01001_029E
                   +B01001_030E)/B01001_001E,
         over65 = (B01001_020E
                   +B01001_021E
                   +B01001_022E
                   +B01001_023E
                   +B01001_024E
                   +B01001_025E
                   +B01001_044E
                   +B01001_045E
                   +B01001_046E
                   +B01001_047E
                   +B01001_048E
                   +B01001_049E)/B01001_001E,
         Pop = B01001_001E,
         P_Female = (B01001_001E-B01001_002E)/B01001_001E
  )

B01001 <- B01001 |> 
  mutate(across(under18:P_Female, ~na_if(., "NaN")))

B01001<-B01001 |>  select(GEOID, under18, over65, Pop, P_Female)


#B02001: Race ----
#https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_16_5YR_B02001&prodType=table
B02001_Vars<-c("B02001_001",
               "B02001_002",
               "B02001_003",
               "B02001_004",
               "B02001_005",
               "B02001_006",
               "B02001_007",
               "B02001_008")

B02001 <- get_acs(geography = "tract", state = state, variables = B02001_Vars, survey = survey, year = DL_Year, output = "wide")

B02001$PWhite<-B02001$B02001_002E/B02001$B02001_001E
B02001$PBlack<-B02001$B02001_003E/B02001$B02001_001E
B02001$PAIAN<-B02001$B02001_004E/B02001$B02001_001E
B02001$PAsian<-B02001$B02001_005E/B02001$B02001_001E
B02001$PNonwhite<-(B02001$B02001_001E-B02001$B02001_002E)/B02001$B02001_001E

B02001$PWhite[B02001$PWhite == "NaN"]<-NA
B02001$PBlack[B02001$PBlack == "NaN"]<-NA
B02001$PAIAN[B02001$PAIAN == "NaN"]<-NA
B02001$PAsian[B02001$PAsian == "NaN"]<-NA
B02001$PNonwhite[B02001$PNonwhite == "NaN"]<-NA

B02001<-B02001 %>% select(GEOID, PWhite, PBlack, PAIAN, PAsian, PNonwhite)

#B03001: Ethnicity ----
#https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_16_5YR_B03001&prodType=table
B03001_Vars<-c("B03001_001",
               "B03001_003")

B03001 <- get_acs(geography = "tract", state = state, variables = B03001_Vars, survey = survey, year = DL_Year, output = "wide")

B03001$PLatino<-B03001$B03001_003E/B03001$B03001_001E

B03001$PLatino[B03001$PLatino == "NaN"]<-NA

B03001<-B03001 %>% select(GEOID, PLatino)

#B05002: Foreign Born ----
#https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_16_5YR_B05002&prodType=table
B05002_Vars<-c("B05002_001",
               "B05002_013")

B05002 <- get_acs(geography = "tract", state = state, variables = B05002_Vars, survey = survey, year = DL_Year, output = "wide")

B05002$PForeignborn<-B05002$B05002_013E/B05002$B05002_001E

B05002$PForeignborn[B05002$PForeignborn == "NaN"]<-NA

B05002<-B05002 %>% select(GEOID, PForeignborn)

#B19013: Median Household Income ----
#https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_16_5YR_B19013&prodType=table
B19013_Vars<-c("B19013_001")

B19013 <- get_acs(geography = "tract", state = state, variables = B19013_Vars, survey = survey, year = DL_Year, output = "wide")

B19013$MHHI<-B19013$B19013_001E

B19013$MHHI[B19013$MHHI == "NaN"]<-NA

B19013<-B19013 %>% select(GEOID, MHHI)

#B11001: Female Headed Household ----
#https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_16_5YR_B11001&prodType=table
B11001_Vars<-c("B11001_001", "B11001_006")
B11001 <- get_acs(geography = "tract", state = state, variables = B11001_Vars, survey = survey, year = DL_Year, output = "wide")
B11001$P_FHHH <- B11001$B11001_006E /B11001$B11001_001E

B11001$P_FHHH[B11001$P_FHHH == "NaN"]<-NA

B11001<-B11001 %>% select(GEOID, P_FHHH)


#B17001: Poverty Rate ----
#https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_16_5YR_B17001&prodType=table
B17001_Vars<-c("B17001_001",
               "B17001_002")

B17001 <- get_acs(geography = "tract", state = state, variables = B17001_Vars, survey = survey, year = DL_Year, output = "wide")

B17001$Pov<-B17001$B17001_002E / B17001$B17001_001E

B17001$Pov[B17001$Pov == "NaN"]<-NA

B17001<-B17001 %>% select(GEOID, Pov)


#B25003: Tenure (Owner Occupied) ----
#https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_16_5YR_B25003&prodType=table

B25003_Vars <- c("B25003_001", "B25003_002")
B25003 <- get_acs(geography = "tract", state = state, variables = B25003_Vars, survey = survey, year = DL_Year, output = "wide")

B25003$P_Own <- B25003$B25003_002E / B25003$B25003_001E
B25003$P_Own[B25003$P_Own == "NaN"]<-NA

B25003<-B25003 %>% select(GEOID, P_Own)

#B25077: Median Home Value (Owner Occupied Housing Units) ----
B25077_Vars <- c("B25077_001")
B25077 <- get_acs(geography = "tract", state = state, variables = B25077_Vars, survey = survey, year = DL_Year, output = "wide")

names(B25077)[3]<-"MHV"
B25077$MHV[B25077$MHV == "NaN"]<-NA

B25077 <- B25077 %>%  select(GEOID, MHV)
# B25106: Housing Cost Burden ----
B25106_Vars <- c("B25106_001",
                 "B25106_006",
                 "B25106_010",
                 "B25106_014", 
                 "B25106_018", 
                 "B25106_022", 
                 "B25106_028", 
                 "B25106_032",
                 "B25106_036",
                 "B25106_040",
                 "B25106_044")

B25106 <- get_acs(geography = "tract", state = state, variables = B25106_Vars, survey = survey, year = DL_Year, output = "wide")

B25106$CostBurden<-(B25106$B25106_006E+
                      B25106$B25106_010E+
                      B25106$B25106_014E+
                      B25106$B25106_018E+
                      B25106$B25106_022E+
                      B25106$B25106_028E+
                      B25106$B25106_032E+
                      B25106$B25106_036E+
                      B25106$B25106_040E+
                      B25106$B25106_044E)/B25106$B25106_001E
B25106$CostBurden[B25106$CostBurden == "NaN"]<-NA

B25106 <- B25106 %>%  select(GEOID, CostBurden)

# Residential Vacancy Rate ----
B25002_Vars<-c("B25002_001", "B25002_003")
B25002 <- get_acs(geography = "tract", state = state, variables = B25002_Vars, survey = survey, year = DL_Year, output = "wide")
B25002$Rvac<-B25002$B25002_003E/B25002$B25002_001E
B25002$Rvac[B25002$Rvac == "NaN"]<-NA

B25002 <- B25002 %>% select(GEOID, Rvac)

rm(B01001_Vars, 
   B02001_Vars, 
   B03001_Vars, 
   B05002_Vars, 
   B11001_Vars,
   B17001_Vars, 
   B19013_Vars, 
   B25002_Vars, 
   B25003_Vars, 
   B25077_Vars, 
   B25106_Vars)

