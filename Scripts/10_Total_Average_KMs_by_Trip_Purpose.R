#-------------------------------------------------------------------------------
#-----------------TOTAL AND AVERAGE YEARLY KMs BY TRIP PURPOSE------------------
#-------------------------------------------------------------------------------

#This script calculates yearly and average km travelled at the LSOA level and 
# using the raw microsimulated data for each trip purpose type.

#It also considers how these km estimates will change because not everyone in 
# population will have access to a car. It uses the dataframe "difference" that 
# contains the proportionaility constant to reduce an individual's car kms 
# travelled based on their Age, Sex, NSSEC, GOR and Rural/Urban. This can be 
# found in script "Validation/2_No_Car_Access.R". These columns are indicated by
# using "new_" at the beginning of the column name.

#We require the "Trip_Kms" list by loading the "Trip_Kms" csvs from the 
# "Trip_Counts" folder.

#These dataframes are save in the "Weekly_Yearly_Kms" folder.

#-------------------------------------------------------------------------------
#--------------------------------GETTING STARTED--------------------------------
#-------------------------------------------------------------------------------

#Create a list for each region of the simulated kms travelled per a week for 
# the total population of England.

Trip_Kms <- list()

for(i in 1:length(region_names)){
  
  test <- fread(paste0("Trip_Counts/",datetime,"_",(region_names)[i],"_Trip_Kms.csv"))
  test <- as.data.frame(test)
  
  test <- test %>%
    select(c(PID, 
             Age_B04,
             Sex,
             GOR_new,
             NSSEC,
             ONSRuralUrban,
             LSOA11CD, 
             OA, 
             Total_kms_TP_1,
             Total_kms_TP_2,
             Total_kms_TP_3,
             Total_kms_TP_4,
             Total_kms_TP_5,
             Total_kms_TP_6))
  
  Trip_Kms[[i]] <- test
  
}

names(Trip_Kms) <- region_names
remove(i, test)


#--------------------------TOTAL KM TRAVELLED PER YEAR--------------------------
#This function will convert the weekly estimates of distance travelled per each 
# purpose and convert that into a yearly estimate.
#This will simply multiple each "Total_kms_TP" column by 52 and append these 
# additional yearly distance travelled estimates to the end of each column. 

yearly_km <- function(dataframe){
  
  km_post <- grep(paste0("kms"), colnames(dataframe))
  
  for(i in 1:length(km_post)){
    
    dataframe[, paste0("Yearly_kms_TP_", i)] <- dataframe[, km_post[i]] * 52
    
  }
  
  return(dataframe)
  
}

#Pass the function through each dataframe of the "Trip_Kms" list
for(i in 1:length(Trip_Kms)){
  
  Trip_Kms[[i]] <- yearly_km(Trip_Kms[[i]])
  
}


#----------------------------ACCOUNT FOR CAR ACCESS-----------------------------
#Now join the "difference" dataframe to the each dataframe in the list "Trip_Kms"
# and multiple the simulated yearly km column by the column "proportion_have_car" 
# ie the proportion of people in the NTS that used a car at least once during 
# the NTS survey week. 

#Load the "difference" dataframe from the following script
source("Validation/2_No_Car_Access.R")

#For loop to join the trip kms data to the difference dataframe.
for(i in 1:length(Trip_Kms)){
  
  Trip_Kms[[i]] <- Trip_Kms[[i]] %>%
    left_join(difference, by = c("Sex", "Age_B04", "NSSEC", "GOR_new", "ONSRuralUrban"))
  
}

#Create a new function to multiple the yearly kms per a person by the value in
# the proportion column.

new_yearly_kms <- function(dataframe){
  
  km_post <- grep(paste0("Yearly_kms_TP_"), colnames(dataframe))
  
  for(i in 1:length(km_post)){
    
    dataframe[, paste0("new_Yearly_kms_TP_", i)] <- dataframe[, km_post[i]] * dataframe$proportion_have_car
    
  }

  return(dataframe)
}

for(i in 1:length(Trip_Kms)){
  
  Trip_Kms[[i]] <- new_yearly_kms(Trip_Kms[[i]])
  
}


#------------------------LSOA TOTAL KM TRAVELLED PER YEAR-----------------------
#This functions takes a List which contains dataframes of yearly distances 
# travelled for each individual, it then sums these distances to get an estimate
# of the total kms travelled per an LSOA for each trip purpose.
#It saves the dataframes in the folder "Weekly_Yearly_Kms/".

sumYearlyKmsLSOA <- function(List){
  
  #Create an empty list
  YearlyTripKms <- list()
  
  
  for(i in 1:length(List)){
    
    dataframe <- List[[i]]
    
    #Get the columns that contain the yearly kms travelled for each individual
    count1 <- grep(paste0("Yearly"), colnames(dataframe))
    
    #Sum the columns to get total kms travelled per LSOA.
    sum_by_LSOA <- dataframe %>%
      group_by(LSOA11CD) %>%
      summarise_at(c(colnames(dataframe)[count1]), funs(sum(., na.rm=FALSE)))
    
    #Save the data as a csv file.
    write.csv(sum_by_LSOA, paste0("Weekly_Yearly_Kms/",datetime,"_",names(List)[i],"_Yearly_sum_kms_LSOA.csv"))
    
    #Store the dataframe in the list
    YearlyTripKms[[i]] <- sum_by_LSOA 
    
  }
  
  names(YearlyTripKms) <- names(List)
  
  return(YearlyTripKms)
  
}


sumYearlyKmsLSOA(Trip_Kms)


#------------------------LSOA MEAN KM TRAVELLED PER YEAR------------------------
#This functions takes a List which contains dataframes of yearly distances 
# travelled for each individual, it then calculates the mean kms travelled by 
# individuals living in the LSOA for each trip purpose type.
#It saves the dataframes in the folder "Weekly_Yearly_Kms/".

meanYearlyKmsLSOA <- function(List){
  
  #Create an empty list
  YearlyTripKms <- list()
  
  
  for(i in 1:length(List)){
    
    dataframe <- List[[i]]
    
    #Get the columns that contain the yearly kms travelled for each individual
    count1 <- grep(paste0("Yearly"), colnames(dataframe))
    
    #Find the mean distance travelled for each trip purpose per LSOA.
    mean_by_LSOA <- dataframe %>%
      group_by(LSOA11CD) %>%
      summarise_at(c(colnames(dataframe)[count1]), funs(mean(., na.rm=FALSE)))
    
    #Save the data as a csv file.
    write.csv(mean_by_LSOA, paste0("Weekly_Yearly_Kms/",datetime,"_",names(List)[i],"_Yearly_mean_kms_LSOA.csv"))
    
    #Store the dataframe in the list
    YearlyTripKms[[i]] <- mean_by_LSOA 
    
  }
  
  names(YearlyTripKms) <- names(List)
  
  return(YearlyTripKms)
  
}

meanYearlyKmsLSOA(Trip_Kms)


#Remove unnecessary values
remove(yearly_km, new_yearly_kms, sumYearlyKmsLSOA, meanYearlyKmsLSOA, Trip_Kms, i)

