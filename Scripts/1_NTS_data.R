#-------------------------------------------------------------------------------
#----------------------------------NTS DATASET----------------------------------
#-------------------------------------------------------------------------------
#This script loads, cleans and combines several NTS datasets to produce a master 
# dataframe of key travel behaviours that we want to explore.

# The output is a dataframe called "car_only_nts" which contains diary entries from
# those who used a car during the survey week and those who are aged 17 or above
# because they can legally drive.



#--------------------------------GETTING STARTED--------------------------------

#---------------------------------LOAD THE DATA---------------------------------
#Load the relevant NTS datasets.

household_nts <- fread("NTS_datasets/householdeul2015_2017.csv")
household_nts <- as.data.frame(household_nts)
individual_nts <- fread("NTS_datasets/individualeul2015_2017.csv")
individual_nts <- as.data.frame(individual_nts)
stage_nts <- fread("NTS_datasets/stageeul2015_2017.csv")
stage_nts <- as.data.frame(stage_nts)
trip_nts <- fread("NTS_datasets/tripeul2015_2017.csv")
trip_nts <- as.data.frame(trip_nts)
vehicle_nts <- fread("NTS_datasets/vehicleeul2015_2017.csv")
vehicle_nts <- as.data.frame(vehicle_nts)

#---------------------------------SELECT COLUMNS--------------------------------
#Create a series of vectors that hold the name of the attributes we want to 
# explore from each dataset.

household_att <- c("HouseholdID", 
                   "SurveyYear", 
                   "HHoldOAClass2011_B03ID",
                   "HHoldGOR_B02ID", 
                   "HHoldNumPeople",
                   "HHIncome2002_B02ID",
                   "NumBike", 
                   "NumCar",
                   "NumVanLorry", 
                   "NumCarVan", 
                   "NumMCycle",
                   "NumVehicles",
                   "AddressType_B01ID",
                   "Ten1_B02ID",
                   "Settlement2011EW_B03ID")

individual_att <- c("IndividualID", 
                    "SurveyYear", 
                    "HouseholdID", 
                    "Age_B04ID", 
                    "Sex_B01ID",
                    "OwnCycle_B01ID", 
                    "LiveWith_B02ID", 
                    "CarAccess_B02ID",
                    "NSSec_B03ID")

stage_att <- c("IndividualID",
               "HouseholdID",
               "TripID",
               "SurveyYear",
               "ParkingCost",
               "StageCost",
               "StageFareCost",
               "StagePassCost",
               "StageID")

trip_att <- c("SurveyYear", 
              "TripID", 
              "IndividualID", 
              "HouseholdID",
              "MainMode_B04ID", 
              "TripPurpose_B01ID",
              "TripStartHours",
              "TripTotalTime",
              "TripDisIncSW")

vehicle_att <- c("SurveyYear",
                 "VehicleID",
                 "HouseholdID",
                 "IndividualID",
                 "VehNo",
                 "VehType_B03ID",
                 "EngineCap",
                 "VehAnMileage",
                 "VehPropTypeTS_B01ID",
                 "WhyVehNotUsed_B01ID",
                 "VehWeekMileage_B01ID")


#Create new dataframes with the selected columns.
individual_new_nts <- select(individual_nts, individual_att)

household_new_nts <- select(household_nts, household_att)

vehicle_new_nts <- select(vehicle_nts, vehicle_att)

trip_new_nts <- select(trip_nts, trip_att)

stage_new_nts <- select(stage_nts, stage_att)

#-----------------------------COMBINE THE DATAFRAMES----------------------------
#Combine all of the dataframes one by one with left_joins, start with the 
# individual dataset and end with the stage dataset. 

combine_all_nts <- individual_new_nts %>% 
  left_join(household_new_nts, by = c("SurveyYear","HouseholdID")) %>%
  left_join(vehicle_new_nts, by = c("IndividualID","HouseholdID","SurveyYear")) %>%
  left_join(trip_new_nts, by = c("IndividualID","HouseholdID","SurveyYear")) %>%
  left_join(stage_new_nts, by = c("IndividualID","HouseholdID","TripID", "SurveyYear"))


#-------------------------------RELABEL THE COLUMNS-----------------------------
#Change name of each column in the dataframe so it is clear what that column describes.

colnames(combine_all_nts) <-  c("IndividualID",
                                "SurveyYear",
                                "HouseholdID",
                                "Age_B04", 
                                "Sex", 
                                "OwnCycle",
                                "LiveWith",
                                "CarAccess",
                                "NSSEC",
                                "OAClass2011",
                                "GOR",
                                "HHoldNumPeople",
                                "Income",
                                "NumBike",
                                "NumCar",
                                "NumVanLorry",
                                "NumCarVan",
                                "NumMotorcycle",
                                "NumVehicles",
                                "AddressType",
                                "TenancyType",
                                "ONSRuralUrban",
                                "VehicleID",
                                "VehNo",
                                "VehType",
                                "EngineCapacity",
                                "AnnualMileage",
                                "TypeOfFuel",
                                "WhyVehNotUsed",
                                "WeeklyMileage",
                                "TripID",
                                "MainMode",
                                "TripPurpose",
                                "TripStartHours",
                                "TripTotalTime",
                                "TripDistance",
                                "ParkingCost",
                                "StageCost",
                                "StageFareCost",
                                "StagePassCost",
                                "StageID")


#---------------------------------CLEAN THE DATA--------------------------------
#These lines of code go through each column in the dataframe that have a negative 
# number coding system to signify an unknown in the survey, according to the NTS 
# documents, and replaces the number with NA. It will make it easier to do
# calculations with the dataframe if the negative numbers are replaced with NA. 

nts_test <- combine_all_nts %>%
  mutate(OAClass2011 = replace(OAClass2011, OAClass2011 == -8, NA)) %>%
  mutate(GOR = replace(GOR, GOR == -8, NA)) %>%
  mutate(GOR = replace(GOR, GOR == -9, NA)) %>%
  mutate(Income = replace(Income, Income == -8, NA)) %>%
  mutate(AddressType = replace(AddressType, AddressType == -8, -2)) %>% #same NA code as SPENSER
  mutate(TenancyType = replace(TenancyType, TenancyType == -8, -2)) %>% #same NA code as SPENSER
  mutate(TenancyType = replace(TenancyType, TenancyType == -9, -2)) %>% #same NA code as SPENSER
  mutate(ONSRuralUrban = replace(ONSRuralUrban, ONSRuralUrban == -8, NA)) %>%
  mutate(Age_B04 = replace(Age_B04, Age_B04 == -8, NA)) %>%
  mutate(Sex = replace(Sex, Sex == -8, NA)) %>%
  mutate(OwnCycle = replace(OwnCycle, OwnCycle == -8, NA)) %>% 
  mutate(OwnCycle = replace(OwnCycle, OwnCycle == -9, NA)) %>% 
  mutate(LiveWith = replace(LiveWith, LiveWith == -8, NA)) %>%
  mutate(LiveWith = replace(LiveWith, LiveWith == -9, NA)) %>% 
  mutate(CarAccess = replace(CarAccess, CarAccess == -8, NA)) %>%
  mutate(NSSEC = replace(NSSEC, NSSEC == -9, -1)) %>%
  mutate(MainMode = replace(MainMode, MainMode == -8, NA)) %>%
  mutate(TripPurpose = replace(TripPurpose, TripPurpose == -8, NA)) %>%
  mutate(VehType = replace(VehType, VehType == -8, NA)) %>%
  mutate(TypeOfFuel = replace(TypeOfFuel, TypeOfFuel == -8, NA)) %>%
  mutate(TypeOfFuel = replace(TypeOfFuel, TypeOfFuel == -9, NA)) %>%
  mutate(WhyVehNotUsed = replace(WhyVehNotUsed, WhyVehNotUsed == -8, NA)) %>%
  mutate(WhyVehNotUsed = replace(WhyVehNotUsed, WhyVehNotUsed == -9, NA)) %>%
  mutate(WhyVehNotUsed = replace(WhyVehNotUsed, WhyVehNotUsed == 97, 6)) %>%
  mutate(WeeklyMileage = replace(WeeklyMileage, WeeklyMileage == -8, NA)) %>%
  mutate(WeeklyMileage = replace(WeeklyMileage, WeeklyMileage == -9, NA)) 


#---------------------------------SPLIT THE DATA--------------------------------
#Because individuals, who are aged 16 and under are not legally allowed to drive
# in the UK, we want to create dataframe that does not contain those individuals.
#Therefore we want to remove age categories 1 (0-4 years), 2 (5-10 years) and 
# 3 (11-16) years.

over_17_nts <- nts_test[!nts_test$Age_B04 %in% c(1,2,3), ]

#Futhermore we are only interested trips that are taken by car/van, so we will only
# include MainMode 3(Car/van driver) and 4 (Car/van passenger)
car_only_nts <- over_17_nts[over_17_nts$MainMode %in% c(3,4), ]


#-------------------------------CREATE NEW GROUPS-------------------------------
#Use the following script to load the necessary functions that cleans the nts
# dataframe to ensure that the number of 0s in the eventual constraint table
# are reduced and to match constraint variables with those found in the SPENSER
# dataset.

source("Functions/1_new_NTS_groups.R")

#--------------------------------DISTANCE GROUPS--------------------------------
# Convert distance travelled for a particular trip from miles into kms and then
# create new distance travlled in km groups.
#The new column is "TripDistance_km"
car_only_nts <- new_distance_km(car_only_nts)

#----------------------------------TRIP PURPOSE---------------------------------
#This function replaces the original trip purpose column in the NTS dataset with new 
# values and trip purpose groups. It will also reduce the number of 0s in the 
# constraint table.
car_only_nts <- new_tripPurpose(car_only_nts)


#----------------------------------NEW REGIONS----------------------------------
#Create new government operational regions groups so that there are less constraint
# variables and to reduce the amount of 0s in the constraint table.
#The new column is "GOR_new"
car_only_nts <- new_GOR(car_only_nts)


#--------------------------------NEW MAIN MODES---------------------------------
#Create new main mode for a trip groups.
#The new column is "MainMode_new".
#over_17_nts <- new_mainMode(over_17_nts)


#----------------------------------TENANCY TYPE---------------------------------
#Create a new tenancy type group that match the tenancy type groups of the SPENSER
# dataset.
#For this one, just need to replace Other (including rent free) coded 3 with 2.

car_only_nts <- car_only_nts %>%
  mutate(TenancyType = replace(TenancyType, TenancyType == 3, 2))


#-----------------------------NUMBER OF PEOPLE IN HH----------------------------
#Create new HHoldNumPeople codes that match the HHoldNumPeople codes of the SPENSER
# dataset.

#The easiest way to do this is to replace values greater than 4 with 4.
car_only_nts$HHoldNumPeople <- ifelse(car_only_nts$HHoldNumPeople > 4, 4, car_only_nts$HHoldNumPeople)


#----------------------------------ADDRESS TYPE---------------------------------
#Create new address type groups that match the address type groups of the SPENSER
# dataset.
#It replaces the original "AddressType" column.
car_only_nts <- new_addressType(car_only_nts)

#------------------------------NUMBER OF CARS/VANS------------------------------
#This function creates a new column that counts the number of cars and vans
# per a household, excluding motorcycles. Then recodes this column accordingly 
# to mirror the codes from the SPENSER dataset that relate to the number of cars 
# or vans that are available for each household.
#The new column is "CarsNo".

car_only_nts <- new_carNo(car_only_nts)


#---------------------------------REMOVE VALUES---------------------------------
#remove unnecessary dataframes and values.
remove(household_att, 
       household_new_nts, 
       household_nts, 
       individual_att, 
       individual_new_nts, 
       individual_nts,
       stage_att, 
       stage_new_nts, 
       stage_nts, 
       trip_att, 
       trip_new_nts, 
       trip_nts, 
       vehicle_att, 
       vehicle_new_nts,
       vehicle_nts, 
       combine_all_nts,
       nts_test,
       over_17_nts,
       new_addressType,
       new_carNo,
       new_distance_km,
       new_mainMode,
       new_tripPurpose)

