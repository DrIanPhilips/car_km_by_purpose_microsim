This folder contains R functions that are used in the project.

1_new_NTS_groups.R
Match the NTS response codes to those found in SPENSER

2_constraint_table_5.R
Used to create a table of probability distributions for a particular attribute dependant 
on 5 constraints

3_new_SPENSER_groups.R
Match the SPENSER codes to those found in the NTS survey data

4_distance_assignment.R
Assign a trip purpose then a trip distance group to each individual in the dataframe x 
number of times.

5_purpose_distance_count.R
Counts the number of times each purpose and distance combination occurs for every 
individual in the population

6_count_to_kms.R
Converts the count columns into distance travelled per a trip purpose.


--------MAPS---------
Following functions map the mean distance travelled for each trip purpose by LSOA.
It takes the mean distance that considers who has access to a car denoted by the columns
"new_Yearly_kms". For info: see Validation/2_No_Car_Access.R

10_mean_individual_plots_jpeg.R
Produces .jpg maps of the mean distance travelled per person per year by LSOA for the 
each of the 6 trip purpose types.

11_mean_individual_plots_html.R
Produces interactive html maps of the mean distance travelled per person per year by LSOA 
for the each of the 6 trip purpose types.

12_mean_arrange_plots_jpeg.R
Produces a .jpg of 6 maps arrange by 3 x 2 of the mean distance travelled per person per 
year by LSOA for the each of the 6 trip purpose types.

13_mean_facet_plots_jpeg.R
Produces a .jpg of 6 maps arrange by 3 x 2 of the mean distance travelled per person per 
year by LSOA for the each of the 6 trip purpose types using "tm_facets"

