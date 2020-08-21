#-------------------------------------------------------------------------------
#------------------------ARRANGE PLOT PURPOSE / DISTANCE------------------------
#-------------------------------------------------------------------------------

#This functions produces a .jpg of 6 maps arrange by 3 x 2 of the mean distance 
# travelled per person per year for the each of the 6 trip purpose types. Each 
# map has its own legend table that describes, using colour, the differences in 
# distance travelled between LSOAs. 
#The image of the six maps is saved in the folder "Plots_Purpose_Distance/". 

#A reminder of what the trip purpose groups are:
# Commute 1
# Business 2
# Education 3
# Shop personal trips 4
# Social Interaction 5
# Leisure 6


plotARRANGEMeanLSOA <- function(dataframe, region){
  
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
  lsoa <- right_join(spatial_data,non_spatial_data,by= c("LSOA11CD"= "LSOA11CD"))
  
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
  
  m3 <-  tm_shape(c_osm) +
    tm_rgb()+
    tm_shape(lsoa) +
    tm_fill(tripvec,
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
    tm_layout(tripvec,
              legend.position = c("right","top"),
              legend.bg.color = "white",
              legend.title.size = 1,
              legend.text.size = 0.6) +
    tm_scale_bar(position  = c("right", "bottom"))
  
  tmap_save(m3, paste0("Plots_Purpose_Distance/Arrange_",datetime," Mean Distance Travelled by Car per Person per Year by LSOA in ",region,".jpg"),dpi = 300)
  
  return(m3)
  
}



