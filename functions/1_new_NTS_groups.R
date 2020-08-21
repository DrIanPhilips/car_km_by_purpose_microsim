#-----------------------------CREATE NEW GROUPS NTS-----------------------------
#First this script loads several new functions that create new groups for NTS 
# data variables. These include: new_distance_km(), new_tripPurpose(), new_GOR() 
# and new_mainMode().
#Secondly, this script loads functions that match NTS variables to those that 
# the SPENSER data has, these include: new_addressType() and new_carNo().


#--------------------------------DISTANCE GROUPS--------------------------------
#Convert the distance travelled from miles to kms, then create the following 
# distance groups for the new km value.

# Group 1: x <= 2km
# Group 2: 2 < x <= 5km
# Group 3: 5 < x <= 10km
# Group 4: 10 < x <= 25km
# Group 5: 25 < x <= 50km
# Group 6: x > 50km

new_distance_km <- function(dataframe){
  #The trip distance is measured in miles. Convert this unit of measurement into km.
  dataframe$TripDistance_km <- (dataframe$TripDistance)*1.60934
  
  #Create an empty vector and a vector of the column TripDistance_km
  assignmentvec <- vector(mode = "numeric", length = nrow(dataframe))  
  dist_vec <-  dataframe[, paste0("TripDistance_km")]
  
  #This for loop numerically codes TripDistance_km if the value falls within 
  # a particular group. It also considers whether the value is NA.
  for(i in 1:length(assignmentvec)){
    
    if (is.na(dist_vec[i]) == TRUE){
      assignmentvec[i] <- NA
    } else if (dist_vec[i] <= 2){
      assignmentvec[i] <- 1
    } else if (dist_vec[i] > 2 && dist_vec[i] <= 5){
      assignmentvec[i] <- 2
    } else if (dist_vec[i] > 5 && dist_vec[i] <= 10){
      assignmentvec[i] <- 3
    } else if (dist_vec[i] > 10 && dist_vec[i] <= 25){
      assignmentvec[i] <- 4
    } else if (dist_vec[i] > 25 && dist_vec[i] <= 50){
      assignmentvec[i] <- 5
    } else {assignmentvec[i] <- 6}
    
  }
  
  #Now replace the orginal TripDistance_km column with assignmentvec  
  dataframe[, paste0("TripDistance_km")] <- assignmentvec
  
  return(dataframe)
}


#----------------------------------TRIP PURPOSE---------------------------------
#Create new trip purpose for the NTS data to reduce the number of 0s in the 
# constraint table.

# NTS Code -> new NTS code
# Commuting 1 -> Commute 1
# Business 2 -> Business 2
# Other work 3 -> Business 2
# Education 4 -> Education 3
# Food shopping 5 -> Shop personal business 4
# Non food shopping 6 -> Shop personal business 4
# Personal business medical 7 -> Shop personal business 4
# Personal business eat / drink 8 -> Shop personal business 4
# Personal business other 9 -> Shop personal business 4
# Visit friends at private home 10 -> Social Interaction 5
# Eat / drink with friends 11 -> Social Interaction 5
# Other social 12 -> Social Interaction 5
# Entertain / public activity 13 -> Leisure 6
# Sport: participate 14 -> Leisure 6
# Holiday: base 15 -> Leisure 6
# Day trip 16 -> Leisure 6
# Just walk 17 -> Leisure 6
# Other non-escort 18 -> Leisure 6
# Escort commuting 19 -> Commute 1
# Escort business and other work 20 -> Business 2
# Escort education 21 -> Education 3
# Escort shopping / personal business 22 -> Shop personal business 4
# Escort home (not own) and other escort 23 -> Leisure 6
# NA -8 -> NA

new_tripPurpose <- function(dataframe){
  #Create an empty vector and a vector of the column NSSEC
  assignmentvec <- vector(mode = "numeric", length = nrow(dataframe))  
  tripPurpose <-  dataframe[, paste0("TripPurpose")]
  
  #This for loop numerically codes for NSSEC and allocates it a new numeric group
  # based on the NTS data. Any other code it is replaces with -1.
  for(k in 1:length(assignmentvec)){
    if (is.na(tripPurpose[k]) == TRUE){
      assignmentvec[k] <- NA
    } else if (tripPurpose[k] == 1){
      assignmentvec[k] <- 1
    } else if (tripPurpose[k] == 2){
      assignmentvec[k] <- 2
    } else if (tripPurpose[k] == 3){
      assignmentvec[k] <- 2
    } else if (tripPurpose[k] == 4){
      assignmentvec[k] <- 3
    } else if (tripPurpose[k] == 5){
      assignmentvec[k] <- 4
    } else if (tripPurpose[k] == 6){
      assignmentvec[k] <- 4  
    } else if (tripPurpose[k] == 7){
      assignmentvec[k] <- 4 
    } else if (tripPurpose[k] == 8){
      assignmentvec[k] <- 4 
    } else if (tripPurpose[k] == 9){
      assignmentvec[k] <- 4 
    } else if (tripPurpose[k] == 10){
      assignmentvec[k] <- 5 
    } else if (tripPurpose[k] == 11){
      assignmentvec[k] <- 5 
    } else if (tripPurpose[k] == 12){
      assignmentvec[k] <- 5 
    } else if (tripPurpose[k] == 13){
      assignmentvec[k] <- 6 
    } else if (tripPurpose[k] == 14){
      assignmentvec[k] <- 6 
    } else if (tripPurpose[k] == 15){
      assignmentvec[k] <- 6 
    } else if (tripPurpose[k] == 16){
      assignmentvec[k] <- 6 
    } else if (tripPurpose[k] == 17){
      assignmentvec[k] <- 6 
    } else if (tripPurpose[k] == 18){
      assignmentvec[k] <- 6 
    } else if (tripPurpose[k] == 19){
      assignmentvec[k] <- 1 
    } else if (tripPurpose[k] == 20){
      assignmentvec[k] <- 2 
    } else if (tripPurpose[k] == 21){
      assignmentvec[k] <- 3 
    } else if (tripPurpose[k] == 22){
      assignmentvec[k] <- 4 
    } else if (tripPurpose[k] == 23){
      assignmentvec[k] <- 6 
    } else {assignmentvec[k] <- NA}
    
  }
  
  #Now replace the orginal NSSEC column with assignmentvec  
  dataframe[, paste0("TripPurpose")] <- assignmentvec
  
  return(dataframe)
}

#------------------------------------REGIONS------------------------------------
#Create new government office regions groups so that there are less constraint
# variables and to reduce the amount of 0s in the constraint table.

# NTS codes -> New NTS codes
# North East 1 -> North and Yorkshire 1
# North West 2 -> North and Yorkshire 1
# Yorkshire and the Humber 3 -> North and Yorkshire 1
# East Midlands 4 -> Midlands 2
# West Midlands 5 -> Midlands 2
# East of England 6 -> East and South East 3
# London 7 -> London 4
# South East 8 -> East and South East 3
# South West 9 -> South West 5

new_GOR <- function(dataframe){
  #Create an empty vector and a vector of the column GOR.
  assignmentvec <- vector(mode = "numeric", length = nrow(dataframe))  
  GOR <-  dataframe[, paste0("GOR")]
  
  #This for loop numerically codes for GOR and allocates it a new numeric 
  # group as shown above.
  for(i in 1:length(assignmentvec)){
    if (GOR[i] == 1){
      assignmentvec[i] <- 1
    } else if (GOR[i] == 2){
      assignmentvec[i] <- 1
    } else if (GOR[i] == 3){
      assignmentvec[i] <- 1
    } else if (GOR[i] == 4){
      assignmentvec[i] <- 2
    } else if (GOR[i] == 5){
      assignmentvec[i] <- 2
    } else if (GOR[i] == 6){
      assignmentvec[i] <- 3  
    } else if (GOR[i] == 7){
      assignmentvec[i] <- 4 
    } else if (GOR[i] == 8){
      assignmentvec[i] <- 3 
    } else {assignmentvec[i] <- 5}
    
  }
  
  #Now add the assignmentvec to the dataframe as a new GOR column
  dataframe[, paste0("GOR_new")] <- assignmentvec
  
  return(dataframe)
}


#--------------------------------NEW MAIN MODES---------------------------------
#Create new main mode for a trip groups.

# NTS codes -> New NTS codes
# Walk 1 -> Walk and Cycle 1
# Bicycle 2 -> Walk and Cycle 1
# Car / van driver 3 -> Car or van driver 2
# Car / van passenger 4 -> Car or van passenger 3
# Motorcycle 5 -> Other private transport 4
# Other private transport 6 -> Other private transport 4
# Bus in London 7 -> Local and non-local buses 5
# Other local bus 8 -> Local and non-local buses 5
# Non-local bus 9 -> Local and non-local buses 5
# London Underground 10 -> Rail 6
# Surface Rail 11 -> Rail 6
# Taxi / minicab 12 -> Other private transport 4
# Other public transport 13 -> Other public transport 7

new_mainMode <- function(dataframe){
  #Create an empty vector and a vector of the column MainMode.
  assignmentvec <- vector(mode = "numeric", length = nrow(dataframe))  
  mainmode <-  dataframe[, paste0("MainMode")]
  
  #This for loop numerically codes for AddressType and allocates it a new numeric 
  # group based on the SPENSER data. Any other code it is replaces with -2.
  for(i in 1:length(assignmentvec)){
    if (is.na(mainmode[i]) == TRUE){
      assignmentvec[i] <- NA
    } else if (mainmode[i] == 1){
      assignmentvec[i] <- 1
    } else if (mainmode[i] == 2){
      assignmentvec[i] <- 1
    } else if (mainmode[i] == 3){
      assignmentvec[i] <- 2
    } else if (mainmode[i] == 4){
      assignmentvec[i] <- 3
    } else if (mainmode[i] == 5){
      assignmentvec[i] <- 4
    } else if (mainmode[i] == 6){
      assignmentvec[i] <- 4  
    } else if (mainmode[i] == 7){
      assignmentvec[i] <- 5 
    } else if (mainmode[i] == 8){
      assignmentvec[i] <- 5 
    } else if (mainmode[i] == 9){
      assignmentvec[i] <- 5
    } else if (mainmode[i] == 10){
      assignmentvec[i] <- 6 
    } else if (mainmode[i] == 11){
      assignmentvec[i] <- 6 
    } else if (mainmode[i] == 12){
      assignmentvec[i] <- 3
    } else {assignmentvec[i] <- 7}
    
  }
  
    #Now add the assignmentvec to the dataframe as a new MainMode column
  dataframe[, paste0("MainMode_new")] <- assignmentvec
  
  return(dataframe)
}


#----------------------------------ADDRESS TYPE---------------------------------
#Create new address type groups that match the address type groups of the SPENSER
# dataset.

# NTS codes -> SPENSER code
# House / bungalow (detached) 1 -> Whole house or bungalow: Total 2
# House / bungalow (semi-detached) 2 -> Whole house or bungalow: Semi-detached 3
# House / bungalow (terrace / end terrace) 3 -> Whole house or bungalow: Terrace (including end-terrace) 4
# House / bungalow (type unknown) 4 -> Housetype Unknown -2
# Flat / maisonette (purpose built) 5 -> Flat, maisonette or apartment, or mobile/temporary accommodation 5
# Flat / maisonette (non-purpose built) 6 -> Flat, maisonette or apartment, or mobile/temporary accommodation 5
# Flat / maisonette (type unknown) 7 -> Flat, maisonette or apartment, or mobile/temporary accommodation 5
# Other accomodation type 8 -> Flat, maisonette or apartment, or mobile/temporary accommodation 5
# Address type unknown -8 -> NA/DNA -2

new_addressType <- function(dataframe){
  #Create an empty vector and a vector of the column AddressType.
  assignmentvec <- vector(mode = "numeric", length = nrow(dataframe))  
  address <-  dataframe[, paste0("AddressType")]
  
  #This for loop numerically codes for AddressType and allocates it a new numeric 
  # group based on the SPENSER data. Any other code it is replaces with -2.
  for(i in 1:length(assignmentvec)){
    if (address[i] == 1){
      assignmentvec[i] <- 2
    } else if (address[i] == 2){
      assignmentvec[i] <- 3
    } else if (address[i] == 3){
      assignmentvec[i] <- 4
    } else if (address[i] == 4){
      assignmentvec[i] <- -2
    } else if (address[i] == 5){
      assignmentvec[i] <- 5
    } else if (address[i] == 6){
      assignmentvec[i] <- 5  
    } else if (address[i] == 7){
      assignmentvec[i] <- 5 
    } else if (address[i] == 8){
      assignmentvec[i] <- 5 
    } else {assignmentvec[i] <- -2}
    
  }
  
  #Now replace the orginal AddressType column with assignmentvec  
  dataframe[, paste0("AddressType")] <- assignmentvec
  
  return(dataframe)
  
}


#------------------------------NUMBER OF CARS/VANS------------------------------
#This function creates a new column that counts the number of cars and vans
# per a household, excluding motorcycles. Then recodes this column accordingly 
# to mirror the codes from the SPENSER dataset that relate to the number of cars 
# or vans that are available for each household.

# NTS codes -> SPENSER code
# No vehicles per a household 0 -> No cars or vans in household 1
# One vehicle per a household 1 -> One car or van in household 2
# Two vehicles per a household -> Two or more cars or vans in household 3
# Three vehicles per a household -> Two or more cars or vans in household 3
# Four vehicles per a household -> Two or more cars or vans in household 3
# etc

new_carNo <- function(dataframe){
  #Create a new column that counts the number of cars and vans per a household, 
  # excluding motorcycles.
  dataframe$CarsNo <- dataframe$NumVehicles - dataframe$NumMotorcycle

  #Create an empty vector and a vector of the column number of cars.
  assignmentvec <- vector(mode = "numeric", length = nrow(dataframe))  
  cars_num <-  dataframe[, paste0("CarsNo")]
  
  #This for loop numerically codes CarNos with  a new numeric code based on the 
  # codes from the SPENSER data. 
  for(i in 1:length(assignmentvec)){
    if (is.na(cars_num[i]) == TRUE){
      assignmentvec[i] <- NA
    } else if (cars_num[i] == 0){
      assignmentvec[i] <- 1  
    } else if (cars_num[i] == 1){
      assignmentvec[i] <- 2
    } else if (cars_num[i] == 2){
      assignmentvec[i] <- 3
    } else {assignmentvec[i] <- 3}
    
  }
  
  #Now replace the orginal CarsNo column with assignmentvec  
  dataframe[, paste0("CarsNo")] <- assignmentvec
  
  return(dataframe)
}








