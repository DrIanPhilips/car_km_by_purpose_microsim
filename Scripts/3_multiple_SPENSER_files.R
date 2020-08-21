#-------------------------------------------------------------------------------
#-------------------------------SPENSER DATASETS--------------------------------
#-------------------------------------------------------------------------------


#--------------------------------GETTING STARTED--------------------------------
library(tidyverse)
library(data.table)


#---------------------------------LOAD THE DATA---------------------------------
#Use the ONS_data frame that contains a column which lists the Local Authority 
# District (LAD) names and codes for England.
#This will be used to find the SPENSER datasets in the "Assign_population_to_hh"
# folder and to name each dataframe. 

#Store each unique LAD code as a vector in alphabetical order.
LADvec <- sort(unique(as.character(ONS_data[, paste0("LAD19CD")]))) #Length is 326

#Store the LADs whose populations were projected for the year 2017.
LADvec2017 <- LADvec[LADvec != "E09000001"]
LADvec2017 <- LADvec2017[LADvec2017 != "E06000053"]

#This LADs have a very small population, hence the SPENSER programme is not able to
# project these populations for years after 2011
small_pop <- c("E06000053", "E09000001")


#-------------------------------LOAD THE FUNCTIONS------------------------------
#This script loads two new functions new_age() and new_NSSEC(), which create new
# age and NSSEC groups that corresponds with those found in the NTS dataset.

source("Functions/3_new_SPENSER_groups.R")


#-------------------------------LOAD SPENSER DATA-------------------------------
#A for loop that loads each Local Authority District SPENSER population and household 
# data files. 
#It joins these two dataframes by household ID (HID) to create a master dataframe.
# It the cleans the dataframe and replaces values for Age and NSSEC to match those
# for the NTS dataset.
#Finally, each dataframe is stored in a list and the name of the list variable is 
# the dataframe's respective Local Authority District code.
dfList <- list()

namesvec <- c()

#Get the populations for 2017.
for (i in 1:length(LADvec2017)){
  spenser_pop <- paste0("Assign_population_to_hh/ass_",LADvec2017[i],"_MSOA11_2017.csv")
  
  spenser_hh <- paste0("Assign_population_to_hh/ass_hh_",LADvec2017[i],"_OA11_2017.csv")
  
  pop_df <- fread(spenser_pop)
  pop_df <- as.data.frame(pop_df)
  
  hh_df <- fread(spenser_hh)
  hh_df <- as.data.frame(hh_df)
  
  combine_spenser <- pop_df %>% 
    left_join(hh_df, by = "HID")
  
  #Rename the columns.
  colnames(combine_spenser) <- c("PID",
                                 "MSOA",
                                 "Sex",
                                 "Age_B04",
                                 "Ethnicity",
                                 "HID",
                                 "OA",
                                 "AddressType",
                                 "QS420",
                                 "TenancyType",
                                 "HouseholdType",
                                 "CommunalSize",
                                 "HHoldNumPeople",
                                 "NumRooms",
                                 "NumBedrooms",
                                 "PersonsPerBedroom",
                                 "CentralHeating",
                                 "NSSEC",
                                 "Ethnicity_2",
                                 "CarsNo",
                                 "HRPID",
                                 "Filled")
  
  #Join the ONS_data dataframe to the SPENSER dataframe. 
  combine_spenser <- combine_spenser %>% 
    left_join(ONS_data, by = c("OA" = "OA11CD"))
  
  #Select the columns required for the project.
  combine_spenser <- select(combine_spenser, c("PID", 
                                               "MSOA", 
                                               "Sex", 
                                               "Age_B04", 
                                               "HID", 
                                               "OA", 
                                              #"AddressType",
                                              #"TenancyType", 
                                              #"HHoldNumPeople", 
                                               "NSSEC", 
                                              #"CarsNo",
                                              "LAD19CD",
                                              #"LAD19NM",
                                              #"GOR",
                                               "Region",
                                               "LSOA11CD",
                                               "ONSRuralUrban",
                                               "GOR_new"))
  
  #Subset the data to only include those who are aged 17 or above.
  combine_spenser <- combine_spenser[!combine_spenser$Age_B04 %in% c(0:16), ]

  #In the SPENSER data, some of the values under HHoldNumPeople are -1. By logic 
  # there surely must be at least one person in these households and the individual
  # counts as that one person. Therefore, need to replace -1 with 1.
  
  # combine_spenser <- combine_spenser %>%
    # mutate(HHoldNumPeople = replace(HHoldNumPeople, HHoldNumPeople == -1, 1))
  
  #Create age and NSSEC groups of the spenser data that match the age and NSSEC
  # groups of the NTS dataset.
  combine_spenser <- new_age(combine_spenser)
  
  combine_spenser <- new_NSSEC(combine_spenser)
  
  #write.csv(combine_spenser, paste0("pop_hh_",LADvec[i],".csv"))

  dfList[[i]] <- combine_spenser
  
  namesvec <- c(namesvec,paste0(LADvec2017[i]))
  
  names(dfList) <- namesvec
  
  
} 

#To get the populations for 2011
for (j in 1:2){
  spenser_pop <- paste0("Assign_population_to_hh/ass_",small_pop[j],"_MSOA11_2011.csv")
  
  spenser_hh <- paste0("Assign_population_to_hh/ass_hh_",small_pop[j],"_OA11_2011.csv")
  
  pop_df <- fread(spenser_pop)
  pop_df <- as.data.frame(pop_df)
  
  hh_df <- fread(spenser_hh)
  hh_df <- as.data.frame(hh_df)
  
  combine_spenser <- pop_df %>% 
    left_join(hh_df, by = "HID")
  
  #Rename the columns.
  colnames(combine_spenser) <- c("PID",
                                 "MSOA",
                                 "Sex",
                                 "Age_B04",
                                 "Ethnicity",
                                 "HID",
                                 "OA",
                                 "AddressType",
                                 "QS420",
                                 "TenancyType",
                                 "HouseholdType",
                                 "CommunalSize",
                                 "HHoldNumPeople",
                                 "NumRooms",
                                 "NumBedrooms",
                                 "PersonsPerBedroom",
                                 "CentralHeating",
                                 "NSSEC",
                                 "Ethnicity_2",
                                 "CarsNo",
                                 "HRPID",
                                 "Filled")
  
  #Join the ONS_data dataframe to the SPENSER dataframe. 
  combine_spenser <- combine_spenser %>% 
    left_join(ONS_data, by = c("OA" = "OA11CD"))
  
  #Select the columns required for the project.
  combine_spenser <- select(combine_spenser, c("PID", 
                                               "MSOA", 
                                               "Sex", 
                                               "Age_B04", 
                                               "HID", 
                                               "OA", 
                                               #"AddressType",
                                               #"TenancyType", 
                                               #"HHoldNumPeople", 
                                               "NSSEC", 
                                               #"CarsNo",
                                               "LAD19CD",
                                               #"LAD19NM",
                                               #"GOR",
                                               "Region",
                                               "LSOA11CD",
                                               "ONSRuralUrban",
                                               "GOR_new"))
  
  #Subset the data to only include those who are aged 17 or above.
  combine_spenser <- combine_spenser[!combine_spenser$Age_B04 %in% c(0:16), ]
  
  #In the SPENSER data, some of the values under HHoldNumPeople are -1. By logic 
  # there surely must be at least one person in these households and the individual
  # counts as that one person. Therefore, need to replace -1 with 1.
  
  # combine_spenser <- combine_spenser %>%
  # mutate(HHoldNumPeople = replace(HHoldNumPeople, HHoldNumPeople == -1, 1))
  
  #Create age and NSSEC groups of the spenser data that match the age and NSSEC
  # groups of the NTS dataset.
  combine_spenser <- new_age(combine_spenser)
  
  combine_spenser <- new_NSSEC(combine_spenser)
  
  #write.csv(combine_spenser, paste0("pop_hh_",LADvec[i],".csv"))
  
  dfList[[324+j]] <- combine_spenser
  
  namesvec <- c(namesvec,paste0(small_pop[j]))
  
  names(dfList) <- namesvec
  
  
} 


#---------------------------------REMOVE VALUES---------------------------------
remove(spenser_hh,
       spenser_pop,
       hh_df,
       pop_df,
       combine_spenser,
       namesvec,
       i,
       j,
       new_age,
       new_NSSEC,
       LADvec2017,
       small_pop)

