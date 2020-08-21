#-------------------------------------------------------------------------------
#-------------------------TOTAL AND AVERAGE YEARLY KMs--------------------------
#-------------------------------------------------------------------------------
#This script calculates yearly and average km travelled at the LSOA level and 
# the OA level using the raw microsimulated data. 

#It also considers how these km estimates will change because not everyone in 
# population will have access to a car. It uses the dataframe "difference" that 
# contains the proportionaility constant to reduce an individual's car kms 
# travelled based on their Age, Sex, NSSEC, GOR and Rural/Urban. This can be 
# found in script "Validation/2_No_Car_Access.R". These columns are indicated by
# using "new_" at the beginning of the column name.

#We require the "Trip_Kms" list by loading the "Trip_Kms" csvs from the 
# "Trip_Counts" folder.

#The output is two dataframes "all_YearlyKms_LSOA" and "all_YearlyKms_OA".
#These dataframes are save in the "Weekly_Yearly_Kms" folder.


#-------------------------------------------------------------------------------
#--------------------------------GETTING STARTED--------------------------------
#-------------------------------------------------------------------------------

#--------------------------------TOTAL TRIP KMS---------------------------------
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

#Function to read in the total weekly kms each person travels in the population
total_weekly_kms <- function(dataframe){
  
  #Get the position of the simulated kms for each trip purpose type
  count1 <- grep(paste0("Total_kms"), colnames(dataframe))
  
  #Create an empty vector and a vector of the column GOR.
  dataframe$TotalWeeklyKms <- rowSums(dataframe[, c(count1)])
  
  return(dataframe)
}

#Pass the function through each dataframe of the "Trip_Kms" list
for(i in 1:length(Trip_Kms)){
  
  Trip_Kms[[i]] <- total_weekly_kms(Trip_Kms[[i]])
  
}

#Function to get the yearly total kms travelled per person
yearly_kms <- function(dataframe){
  
  #Create an empty vector and a vector of the column GOR.
  dataframe$YearlyKms <- dataframe$TotalWeeklyKms * 52
  
  return(dataframe)
}

#Again pass the function through each dataframe of the Trip_Kms list
for(i in 1:length(Trip_Kms)){
  
  Trip_Kms[[i]] <- yearly_kms(Trip_Kms[[i]])
  
}

remove(i, total_weekly_kms, yearly_kms)


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
  
  #Create an empty vector and a vector of the column GOR.
  dataframe$new_YearlyKms <- dataframe$YearlyKms * dataframe$proportion_have_car
  
  return(dataframe)
}

for(i in 1:length(Trip_Kms)){
  
  Trip_Kms[[i]] <- new_yearly_kms(Trip_Kms[[i]])
  
}

#Remove unused columns
for(i in 1:length(Trip_Kms)){
  
  Trip_Kms[[i]] <- Trip_Kms[[i]] %>%
    select(c(PID, 
             Age_B04,
             Sex,
             GOR_new,
             NSSEC,
             ONSRuralUrban,
             LSOA11CD, 
             OA, 
             YearlyKms,
             proportion_have_car,
             new_YearlyKms))
  
}

remove(i, new_yearly_kms)

#-------------------------------------------------------------------------------
#------------------------------------BY LSOA------------------------------------
#-------------------------------------------------------------------------------
#The following section creates the dataframe "privateKm_LSOA" which contains the
# total yearly kms travelled per LSOA and the average yearly kms travelled per 
# LSOA from the raw microsimulation data (YearlyKms, microsim_kmAv). It also
# includes these columns for the yearly kms estimates that consider the total
# population has access to a car (new_YearlyKms, new_microsim_kmAv).


#-------------------------------TOTAL YEARLY KMs--------------------------------

#Function to sum total kms travelled by LSOA and pass the list "Trip_Kms" 
# through it.
sumYearlyKms_LSOA <- function(List){
  
  #Create an empty list
  YearlyKms <- list()
  
  
  for(i in 1:length(List)){
    
    dataframe <- List[[i]]
    
    #Get the columns that contain the yearly kms travelled for each individual
    count1 <- grep(paste0("Yearly"), colnames(dataframe))
    
    #Sum the columns to get total kms travelled per OA.
    sum_by_LSOA <- dataframe %>%
      group_by(LSOA11CD) %>%
      summarise_at(c(colnames(dataframe)[count1]), funs(sum(., na.rm=FALSE)))
    
    #Store the dataframe in the list
    YearlyKms[[i]] <- sum_by_LSOA 
    
  }
  
  names(YearlyKms) <- names(List)
  
  return(YearlyKms)
  
}

YearlyKms_LSOA <- sumYearlyKms_LSOA(Trip_Kms)

#Sum the total number of people who live in an LSOA.
#This is done by caluclating the number of times the LSOA appeared in the 
# simulated data as each row indicates a different person.
#Pass the function through the "Trip_Kms" list.
population_LSOA <- function(List){
  
  #Create an empty list
  pop_LSOA <- list()
  
  for(i in 1:length(List)){
    
    dataframe <- List[[i]]
    
    #Store the Output Area column as a vector
    LSOA11CD <- c(dataframe[,paste0("LSOA11CD")])
    
    #Freq is the number of times the individual ID occurred
    LSOA_count <- (data.frame(table(LSOA11CD)))
    
    pop_LSOA[[i]] <- LSOA_count
    
  }
  
  names(pop_LSOA) <- names(List)
  
  return(pop_LSOA)
  
}

microsim_pop_LSOA <- population_LSOA(Trip_Kms)

#Create a dataframe of the elements in the lists "microsim_pop_LSOA" and 
# "YearlyKms_LSOA"
ALLmicrosim_pop_LSOA <- data.frame()
ALLYearlyKms_LSOA <- data.frame()
for(i in 1:length(region_names)){
  
  ALLmicrosim_pop_LSOA <- rbind(ALLmicrosim_pop_LSOA, microsim_pop_LSOA[[i]])
  
  ALLYearlyKms_LSOA <- rbind(ALLYearlyKms_LSOA, YearlyKms_LSOA[[i]])
  
}

all_YearlyKms_LSOA <- ALLmicrosim_pop_LSOA %>%
  left_join(ALLYearlyKms_LSOA, by = c("LSOA11CD" = "LSOA11CD"))

remove(population_LSOA, sumYearlyKms_LSOA, microsim_pop_LSOA, YearlyKms_LSOA, 
       ALLmicrosim_pop_LSOA, ALLYearlyKms_LSOA)


#------------------------------AVERAGE YEARLY KMS-------------------------------
#Find the average number of Kms per a person in an LSOA for the raw Yearly Km
# estimates and the new Yearly Km estimates
all_YearlyKms_LSOA$microsim_kmAv <- all_YearlyKms_LSOA$YearlyKms / all_YearlyKms_LSOA$Freq
all_YearlyKms_LSOA$new_microsim_kmAv <- all_YearlyKms_LSOA$new_YearlyKms / all_YearlyKms_LSOA$Freq

fwrite(all_YearlyKms_LSOA, paste0("Weekly_Yearly_Kms/",datetime,"_Yearly_Total_Average_Kms_Travelled_by_LSOA.csv"))

#-------------------------------------------------------------------------------
#-------------------------------------BY OA-------------------------------------
#-------------------------------------------------------------------------------
#The following section creates the dataframe "privateKm_OA" which contains the
# total yearly kms travelled per OA and the average yearly kms travelled per OA 
# from the raw microsimulation data (YearlyKms, microsim_kmAv). It also includes 
# these columns for the yearly kms estimates that consider the total population 
# has access to a car (new_YearlyKms, new_microsim_kmAv).


#-------------------------------TOTAL YEARLY KMs--------------------------------
#Sum total kms travelled by OA
sumYearlyKms_OA <- function(List){
  
  #Create an empty list
  YearlyKms <- list()
  
  
  for(i in 1:length(List)){
    
    dataframe <- List[[i]]
    
    #Get the columns that contain the yearly kms travelled for each individual
    count1 <- grep(paste0("Yearly"), colnames(dataframe))
    
    #Sum the columns to get total kms travelled per OA.
    sum_by_OA <- dataframe %>%
      group_by(OA) %>%
      summarise_at(c(colnames(dataframe)[count1]), funs(sum(., na.rm=FALSE)))
    
    #Store the dataframe in the list
    YearlyKms[[i]] <- sum_by_OA 
    
  }
  
  names(YearlyKms) <- names(List)
  
  return(YearlyKms)
  
}

#Run the "sumYearlyKmsOA(List)" function through the Trip_Kms list
YearlyKms_OA <- sumYearlyKms_OA(Trip_Kms)

#Sum the total number of people who live in an OA. To do this we caluclate 
# the number of times the OA appeared in the simulated data.

population_OA <- function(List){
  
  #Create an empty list
  pop_OA <- list()
  
  for(i in 1:length(List)){
    
    dataframe <- List[[i]]
    
    #Store the Output Area column as a vector
    OA <- c(dataframe[,paste0("OA")])
    
    #Freq is the number of times the individual ID occurred
    OA_count <- (data.frame(table(OA)))
    
    pop_OA[[i]] <- OA_count
    
  }
  
  names(pop_OA) <- names(List)
  
  return(pop_OA)
  
}


microsim_pop_OA <- population_OA(Trip_Kms)

#Create a dataframe of the elements in the lists "microsim_pop_OA" and 
# "YearlyKms_OA"
ALLmicrosim_pop_OA <- data.frame()
ALLYearlyKms_OA <- data.frame()
for(i in 1:length(region_names)){
  
  ALLmicrosim_pop_OA <- rbind(ALLmicrosim_pop_OA, microsim_pop_OA[[i]])
  
  ALLYearlyKms_OA <- rbind(ALLYearlyKms_OA, YearlyKms_OA[[i]])
  
}

all_YearlyKms_OA <- ALLmicrosim_pop_OA %>%
  left_join(ALLYearlyKms_OA, by = c("OA" = "OA"))

remove(population_OA, sumYearlyKms_OA, microsim_pop_OA, YearlyKms_OA, 
       ALLmicrosim_pop_OA, ALLYearlyKms_OA)


#------------------------------AVERAGE YEARLY KMS-------------------------------
#Find the average number of Kms per a person in an OA for the raw Yearly Km
# estimates and the new Yearly Km estimates
all_YearlyKms_OA$microsim_kmAv <- all_YearlyKms_OA$YearlyKms / all_YearlyKms_OA$Freq
all_YearlyKms_OA$new_microsim_kmAv <- all_YearlyKms_OA$new_YearlyKms / all_YearlyKms_OA$Freq

fwrite(all_YearlyKms_OA, paste0("Weekly_Yearly_Kms/",datetime,"_Yearly_Total_Average_Kms_Travelled_by_OA.csv"))

#Remove unnessercary variables
remove(Trip_Kms, i)





