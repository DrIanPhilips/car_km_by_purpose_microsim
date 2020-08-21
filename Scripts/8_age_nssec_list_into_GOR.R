#-------------------------------------------------------------------------------
#-------------------------OFFICE REGION DISTRICT COUNTS-------------------------
#-------------------------------------------------------------------------------

#Join all of the age/NSSEC dataframes together from "CountList" by Government 
# Office Regions (GOR). This can be achieved because all of the count dataframes
# have the same number of columns.
#This will be useful for count in total each purpose/distance combination per an
# LSOA. Once we know the count for each distance group we can then approximate the
# total kms travelled per a trip purpose per an LSOA.

#The output is a list "GORCountTrip" which contains 5 dataframes for each GOR.

GORCountTrip <- list()

for(i in 1:5){
  
  test_1 <- data.frame()
  
  for(j in 1:length(CountTrip)){
    
    test_1 <- rbind(test_1, CountTrip[[j]][CountTrip[[j]]$GOR_new %in% i, ])
    
    CountTrip[[j]] <- CountTrip[[j]][!CountTrip[[j]]$GOR_new %in% i, ]
    
    gc()

  }
  
  GORCountTrip[[i]] <- test_1

}  

names(GORCountTrip) <- c("North_and_Yorkshire", "Midlands", "East_and_South_East", "London", "South_West")

remove(test_1, i, j, CountTrip)


