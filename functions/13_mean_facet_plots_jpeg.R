#-------------------------------------------------------------------------------
#-------------------------FACET PLOT PURPOSE / DISTANCE-------------------------
#-------------------------------------------------------------------------------
#Code blocks to use as templates for making maps in tmap.tmap tutorials include:
#'https://cran.r-project.org/web/packages/tmap/vignettes/tmap-getstarted.html'
#https://mran.microsoft.com/snapshot/2014-11-17/web/packages/tmap/vignettes/tmap-nutshell.html

#This functions produces a .jpg of 6 maps arrange by 3 x 2 of the mean distance 
# travelled per person per year by LSOA for the each of the 6 trip purpose types
# using "tm_facets". As a result, there is only one colour legend to show 
# differences in distance travelled between LSOAs additionally also between trip 
# purpose types too. 
#The facet of the six maps is saved in the folder "Plots_Purpose_Distance/". 

#A reminder of what the trip purpose groups are:
# Commute 1
# Business 2
# Education 3
# Shop personal trips 4
# Social Interaction 5
# Leisure 6

plotFACETMeanLSOA <- function(dataframe, region){
  
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
  tripvec <- c("Commuting",
               "Business",
               "Education",
               "Shopping Personal Trips",
               "Social Interaction",
               "Leisure")
  
  colnames(non_spatial_data) <- c("number",
                                  "LSOA11CD",
                                  tripvec)
  
  
  #We need to place each of the mean distance travelled for a trip purpose
  # columns underneath each other for the "tm_facets" to work.
  
  #First create an empty dataframe.
  test_2 <- data.frame()
  
  for(i in 1:length(tripvec)){
    
    #Select the LSOA and the mean distance for trip purpose i columns
    test <- select(non_spatial_data, c(LSOA11CD, tripvec[i]))
    
    #Create a new column which holds the name of the trip purpose types.
    test[, paste0("TripPurpose")] <- tripvec[i]
    
    #Bind test underneath
    test_2 <- rbind(test_2, test, use.names = FALSE)
    
  }
  
  #Rename the columns suitably
  colnames(test_2) <- c("LSOA11CD", "MeanDistance", "TripPurpose")
  
  #Read in spatial data to get the ONS co-ordinates for each LSOA in the UK.
  #This data can be found using the link below using the generalised clipped 
  # boundaries (BGC).
  #http://geoportal.statistics.gov.uk/datasets/lower-layer-super-output-areas-december-2011-boundaries-ew-bgc?geometry=-12.660%2C52.318%2C8.246%2C54.607
  
  #If you click the down arrow next to API and copy the GeoJSON link. 
  #"st-read" is from the sf package
  spatial_data <- st_read("https://opendata.arcgis.com/datasets/e993add3f1944437bc91ec7c76100c63_0.geojson")
  spatial_data$LSOA11CD <- as.character(spatial_data$LSOA11CD)
  
  
  #----------------------JOIN THE TWO SPATIAL SETS TOGETHER-----------------------
  test_2$check = 1
  
  #Join the non-sptail and spatial together via the MSOA.
  lsoa <- right_join(spatial_data, test_2, by= c("LSOA11CD" = "LSOA11CD"))
  
  #Only keep the MSOAs that are in the current non_spatial dataset.  
  lsoa <- lsoa %>% filter(check == 1)
  
  #-------------------------------EXPLORE PALETTES--------------------------------
  #This tmaptools package shows a varied of colours to use for a map.
  #palette_explorer()
  
  
  #------------------------MAP TRIP PURPOSE WITH A BACKDROP-----------------------
  #By using OpenStreetMap, tmap and tmaptools this gives the maps a basemap that 
  # look like Google maps.
  
  #This makes maps that you can save as jpegs etc.
  tmap_mode('plot')

  #Get the osm maptiles a little bigger than the extent of the lsoas you are plotting.
  c_osm <- read_osm(bb(lsoa, ext = 1.05))
  
  #!!as.symbol() lets you put a string variable name into the select statement.
  #This helps if you want to set up a loop to make many maps.
  #Filter out any rows where the MeanDistance is NA or 0.
  lsoa2 <- lsoa %>% 
    select((!!as.symbol("MeanDistance"))) %>%
    filter((!!as.symbol("MeanDistance")) >= 0)
  
  m <-  tm_shape(c_osm) +
    tm_rgb()+
    tm_shape(lsoa) +
    tm_facets(by = "TripPurpose", nrow = 2, ncol = 3, free.coords = FALSE) +
    tm_fill("MeanDistance", 
            lty = 0, 
            lwd = 0,
            border.alpha = 0,
            style= 'fisher', # change to fixed if you want to manually set class boundaries
            n = 10, #number of classes
            alpha = 0.6,
            #breaks = 'fisher', #set your break values in this vector if you change to style = 'fixed'
            title = "Distance (km)",
            legend.hist = F, #change to F if you don't want a histogram
            palette = "PRGn", #"Blues", "Greens" and "YlOrBr" are good for choropleth
            midpoint = NA) +
    tm_layout(paste0("Mean distance travelled by car per person per year by LSOA"),
              legend.outside = TRUE,
              legend.outside.position = "right",
              legend.bg.color = "white",
              legend.title.size = 1.5,
              legend.text.size = 1)
  #tm_scale_bar(position  = c("right", "bottom"))
  
  #This saves the map into an empty folder called Maps_TripPurpose
  tmap_save(m, paste0("Plots_Purpose_Distance/Facet_",datetime," Mean Distance Travelled by Car per Person per Year by LSOA in ", region, ".jpg"), height = 29.7,width = 21, unit = "cm",dpi = 300)
  
  #For alternative map colours use these hexcode colours.
  #yello/orange/red "#ffffcc","#ffeda0","#fed976","#feb24c","#fd8d3c","#fc4e2a","#e31a1c","#bd0026","#800026"
  #yellow/blue "#ffffd9","#edf8b1","#c7e9b4","#7fcdbb","#41b6c4","#1d91c0","#225ea8","#253494","#081d58"
  #PRGn - purple green 
  
  return(m)
  
}






  












