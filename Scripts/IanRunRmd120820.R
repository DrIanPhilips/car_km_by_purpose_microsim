#script of HPCspenserrmd 


library(tidyverse)
library(data.table)
library(tmap)
library(tmaptools) 
library(sf)
#library(OpenStreetMap) #you have to have java runtime environment for this package to work 
library(shinyjs)


### 1.2 Date and time
#Get the date and the time of the starting point for this Rmarkdown file to track the files created and saved in this document.

datetime <- gsub(" ", "_",Sys.time())
datetime <- gsub(":","-",datetime)



## 2 Load the data

### 2.1 NTS Datasets
# The following script joins several National Travel Survey datasets together for the years 2015, 2016 and 2017.
# It cleans the datasets and replaces attribute values with those found in SPENSER (ie the population dataset). This seamlessly links the two datasets together and reduces the number of the 0s in the constraint table.
# The script outputs one dataframe "car_only_nts" which contains respondents who are aged 17 and above, and trips were the individual is either the passenger or the driver of a car or van. 

#```{r}
source("Scripts/1_NTS_data.R")
#```




### 2.2 Constraint Table
#The function "constraint_table_5(dataframe, Con1, Con2, Con3, Con4, Con5, Var1)" 
#creates a probability constraint table of a particular attribute based on five constraints. 
#In this model, the constraints are "Sex", "Age_B04", "NSSEC", "GOR_new" and "ONSRuralUrban" 
#and the travel behaviour we want to look at is "TripPurpose". 
#The five constraints are important factors that influences a person's travel purpose and distance. 
#This will be useful for the Monte Carlo sampling process because the constraint table displays 
#the probability distribution for the behaviour to occur for all 600 constraint combinations. 

#Pass the "car_only_nts" dataframe through the function constraint_table_5().
#It is imperative that the constraints and attribute are in quotations "".

#```{r}
source("Functions/2_constraint_table_5.R")

trip_purpose_ct <- constraint_table_5(car_only_nts, "Sex", "Age_B04", "NSSEC", "GOR_new", "ONSRuralUrban", "TripPurpose")
#```

#Currently, we do not require a the "p%" columns 
#from the constraint table so these can be removed. Also, it will reduce the size of the dataframe.

#```{r}
delete <- grep("p%", colnames(trip_purpose_ct))
trip_purpose_ct <- select(trip_purpose_ct, -c(delete))

remove(delete)
#```



### 2.3 ONS DATASET
#This script loads Office for National Statistics data and stores it in a dataframe. The output is a dataframe called "ONS_data". This includes the Local Authority District name and code, Government Operational Region, Output Area code, Lower Layer Super Output Area (LSOA) code, Middle Layer Super Output Area (MSOA), and Rural Urban Classification.

#```{r}
source("Scripts/2_ONS_data.R")
#`




### 2.4 SPENSER DATASET
#Load the 2017 population and household data for every Local Authority district in England. This synthetic population was created by the programme SPENSER. Each dataframe is cleaned and the Age and NSSEC variables are recoded to match the codes found in the NTS dataset. Next, the dataframe is joined to "ONS_data", thus the synthetic population has an LSOA, Rural/Urban classification and GOR attribute too. These dataframes are stored in a list called "dfList", and the name of each dataframe in the list is named after the Local Authority District Code for the dataframe.

#This process takes about 3 minutes.

#```{r}
source("Scripts/3_multiple_SPENSER_files.R")

dfList <- dfList[order(names(dfList))]
#```


### 2.5 Split the dataframes by Age and NSSEC group
#The number of trips an individudal takes during a week depends on the age 
#and the NSSEC group they belong to. For more information look at script "Scripts/test_count_trips.R".

##Therefore, it is justified to split the synthetic population data by 
#each age and NSSEC group combination to reasonable allocate indiviudals a number of trips.
#This gives an output of 30 dataframes which are stored in a list called "ageNSSEClist". 
#It names the elements after its representive age/NSSEC combination.

#!!The following script takes 1 hour to run.

#```{r}
source("Scripts/4_binding_dfs_by_age_NSSEC.R")

gc()
#remove(dfList)
#```


### 2.6 Combine the Constraint Table with SPENSER
#Join the constraint table with each ageNSSEC dataframe from "ageNSSEClist" to create a master table of the population and the probability distrubtion for each NTS trip purpose based on the individual's age, gender, National Statistics Socio-economic Classification, adjusted Government Operational Region and Rural/Urban Classification.
#The output is another list called "masterTableList", which stores each master table dataframe and names that element after the dataframe's respective age/NSSEC combination. 


#```{r}
source("Scripts/5_masterTable.R")
#remove(ageNSSEClist)
#```
