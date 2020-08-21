#-------------------------------------------------------------------------------
#-----------------------------COUNT NUMBER OF TRIPS-----------------------------
#-------------------------------------------------------------------------------
#First count the number of trips an indiviudal completes in a week.

#This is done by caluclating the number of times the IndiviudalId 
# appeared in the NTS data, as each line of the dataframe indicates 
# a different trip.
IndividualID <- c(car_only_nts[,paste0("IndividualID")])

#Freq is the number of times the individual ID occurred
count_id <- (data.frame(table(IndividualID )))

count_id$IndividualID <- as.numeric(as.character(count_id$IndividualID))

count_id <- count_id %>%
  left_join(car_only_nts, by = c("IndividualID" = "IndividualID"))

count_id <- select(count_id, c("IndividualID",
                               "Freq",
                               "Age_B04",
                               "Sex",
                               "NSSEC",
                               "GOR_new",
                               "ONSRuralUrban"))

#Removes repeated rows
count_id <- unique(count_id[,1:7])


#Assumed that number of trips per a week is influenced by individual attributes.
#It was found that age and NSSEC groups had the largest range of trips per a week.
# This will be useful for ensuring individual's in the population are allocated
# a reasonable amount of trips depending on their age and NSSEC.

#Calculate the mean number of trips between each age/NSSEC group combination.
#Range is 12, smallest sample size 871
age_nssec_count <- count_id %>%
  group_by(Age_B04,NSSEC) %>%
  summarise_at(c("Freq"), funs(mean(., na.rm=FALSE)))

#Round the mean value to the nearest whole number.
age_nssec_count[,3] <- round(age_nssec_count[,3],digits=0)

age_nssec_count <- as.data.frame(age_nssec_count)

remove(count_id, IndividualID)


#-----------------------------VECTOR OF TRIP COUNT------------------------------
#Get the trip counts for each age/NSSEC combination. 

#Create an empty vector that will store trip counts for each age/NSSEC combination.
countvec <- c()

#Loop through to find when age and NSSEC in age_nssec_count equal that of the name 
# of the dataframe in the "masterTableList". If they are equal, the value of the 
# row's "Freq" column will be placed in the empty vector in accordance to the
# position the age/NSSEC dataframe is in the List.

for(k in 4:9){
  
  for(l in 1:5){
    
    for(i in 1:nrow(age_nssec_count)){
      
      if(age_nssec_count[i,1] == k && age_nssec_count[i,2] == l){
        countvec[grep(paste0("age",k,"NSSEC",l), names(masterTableList))] <- age_nssec_count$Freq[i]
      } else{}
    }
  }
}

remove(i,k,l)

