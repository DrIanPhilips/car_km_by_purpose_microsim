#-------------------------------------------------------------------------------
#----------------------INDIVIDUAL PLOTS PURPOSE / DISTANCE----------------------
#-------------------------------------------------------------------------------

#This functions produces interactive html maps of the mean distance travelled per 
# person per year for the each of the 6 trip purpose types. Hence, 6 maps are 
# produced in total. These maps are saved in what you have get to be the current 
# working directory.

#A reminder of what the trip purpose groups are:
# Commute 1
# Business 2
# Education 3
# Shop personal trips 4
# Social Interaction 5
# Leisure 6


plotHTMLMeanLSOA <- function(dataframe, region, travel_purpose){
  
  #-------------------------------READ DATA INTO R--------------------------------
  #Read in the non spatial data, in this case it will be the yearly mean kms 
  # travelled for each of the 6 trip purpose types by LSOA.
  non_spatial_data <- dataframe
  non_spatial_data$LSOA11CD <- as.character(non_spatial_data$LSOA11CD)
  
  non_spatial_data <- non_spatial_data %>%
    select(-c(Yearly_kms_TP_1,
              Yearly_kms_TP_2,
              Yearly_kms_TP_3,
              Yearly_kms_TP_4,
              Yearly_kms_TP_5,
              Yearly_kms_TP_6))
  
  #Relabel the columns for trip purpose as follows.
  colnames(non_spatial_data) <- c("number",
                                  "LSOA11CD",
                                  "Commuting",
                                  "Business",
                                  "Education",
                                  "Shopping Personal Trips",
                                  "Social Interaction",
                                  "Leisure")
  
  #Read in spatial data to get the ONS co-ordinates for each LSOA in the UK.
  #This data can be found using the link below using the generalised clipped 
  # boundaries (BGC). 
  #http://geoportal.statistics.gov.uk/datasets/lower-layer-super-output-areas-december-2011-boundaries-ew-bgc?geometry=-12.660%2C52.318%2C8.246%2C54.607
  
  #If you click the down arrow next to API and copy the GeoJSON link. 
  #"st-read" is from the sf package
  spatial_data <- st_read("https://opendata.arcgis.com/datasets/e993add3f1944437bc91ec7c76100c63_0.geojson")
  spatial_data$LSOA11CD <- as.character(spatial_data$LSOA11CD)
  
  
  
  #----------------------JOIN THE TWO SPATIAL SETS TOGETHER-----------------------
  non_spatial_data$check = 1
  
  #Join the non-sptail and spatial together via the MSOA.
  lsoa <- left_join(spatial_data,non_spatial_data,by= c("LSOA11CD"= "LSOA11CD"))
  
  #Only keep the MSOAs that are in the current non_spatial dataset.  
  lsoa <- lsoa %>% filter(check == 1)
  
  #-------------------------------EXPLORE PALETTES--------------------------------
  #This tmaptools package shows a varied of colours to use for a map.
  palette_explorer()
  
  
  #------------------------MAP TRIP PURPOSE WITH A BACKDROP-----------------------
  #By using OpenStreetMap, tmap and tmaptools this gives the maps a basemap that 
  # look like Google maps.
  
  #This makes maps that appear in a webbrowser window and you can pan around it. 
  tmap_mode('view')
  
  #!!as.symbol() lets you put a string variable name into the select statement.
  #This filters out any 0 or NA values in the mean distance column for the 
  # inputted "travel_purpose" out of the 6 listed above.
  lsoa2 <- lsoa %>% 
    select((!!as.symbol(travel_purpose))) %>%
    filter((!!as.symbol(travel_purpose)) >= 0)
  
  m <-  tm_shape(lsoa2) +
    tm_fill(travel_purpose,
            lty = 0, 
            lwd = 0,
            border.alpha = 0,
            style= 'fisher', # change to fixed if you want to manually set class boundaries
            n = 10, #number of classes
            alpha = 0.6,
            #breaks = 'fisher', #set your break values in this vector if you change to style = 'fixed'
            title = "Distance (km)",
            #legend.hist = F, #change to F if you don't want a histogram
            palette = "PRGn", #"Blues", "Greens" and "YlOrBr" are good for choropleth
            midpoint = NA) +
    tm_layout(paste0(travel_purpose),
              title.size = 1.5)

  #This saves the map into an empty folder called Maps_TripPurpose
  tmap_save(m,paste0("Mean_Distance_Travelled_by_Car_per_Person_per_Year_by_LSOA_for_",travel_purpose,"_in_", region,".html"))
  
  
  
    
  #For alternative map colours use these hexcode colours.
  #yello/orange/red "#ffffcc","#ffeda0","#fed976","#feb24c","#fd8d3c","#fc4e2a","#e31a1c","#bd0026","#800026"
  #yellow/blue "#ffffd9","#edf8b1","#c7e9b4","#7fcdbb","#41b6c4","#1d91c0","#225ea8","#253494","#081d58"
  #PRGn - purple green 
  
  return(m)
  
}




