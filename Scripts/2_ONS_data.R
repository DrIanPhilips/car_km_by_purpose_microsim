#-------------------------------------------------------------------------------
#---------------------------------ONS DATASETS----------------------------------
#-------------------------------------------------------------------------------

#-----------------------------------LOAD DATA-----------------------------------
#Load the following datasets:
# i) Local Authority District names and the Government Operational region they 
#    belong to, where the codes match those of the NTS dataset.
# ii) All OA, LSOA, MSOA and LAD regions for the UK.
# iii) Rural/urban classification for each LSOA in England and Wales. The 
#      classification matches the Settlement2011EW_B03ID code from the NTS dataset.
LADs_GOR <- fread("ONS_datasets/LAD_Names_and_GOR.csv")
LADs_GOR <- as.data.frame(LADs_GOR)

ONS_codes <- fread("ONS_datasets/OA_LSOA_MSOA_LAD_2017.csv")
ONS_codes <- as.data.frame(ONS_codes)

LSOA_RuralUrban <- fread("ONS_datasets/Rural_Urban_Classification_2011_LSOA.csv")
LSOA_RuralUrban <- as.data.frame(LSOA_RuralUrban)


#-----------------------------COMBINE THE DATAFRAMES----------------------------
#Combine all of the dataframes one by one with left_joins.
#Then select the most relevant columns for this project.

ONS_data <- LADs_GOR %>% 
  left_join(ONS_codes, by = c("LAD19CD" = "LAD17CD")) %>%
  left_join(LSOA_RuralUrban, by = c("LSOA11CD" = "LSOA11CD")) %>%
  select(c("LAD19CD", 
           "LAD19NM", 
           "GOR",
           "Region",
           "OA11CD", 
           "LSOA11CD", 
           "MSOA11CD", 
           "RUC11CD", 
           "ONSRuralUrban"))


#----------------------------------NEW REGIONS----------------------------------
#Create new government operational regions groups so that there are less constraint
# variables and to reduce the amount of 0s in the constraint table.
#The GOR codes are allocated in the exact same way as the NTS data. Therefore, we
# can use the function that was previously loaded previous new_GOR().
#The name of this new column is again "GOR_new".

ONS_data <- new_GOR(ONS_data)


#---------------------------------REMOVE VALUES---------------------------------
#remove unnecessary dataframes and values.
remove(LADs_GOR, 
       LSOA_RuralUrban, 
       ONS_codes)


