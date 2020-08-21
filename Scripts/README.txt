This folder contains the scripts needed to run the microsimulation and analysis

1_NTS_data.R
    - It loads, joins and cleans the NTS data
    - The output is a dataframe called "car_only_nts" which contains diary entries from those
      who used a car during the survey week and those who are aged 17 or above because they
      can legally drive.

2_ONS_data.R
    - Creates the dataframe "ONS_data" which contains the following columns: LAD, GOR, OA,
      LSOA, MSOA and ONSRuralUrban.
    - Uses csv files from the ONS website.

3_multiple_SPENSER_files.R
    - Joins the household and individual files from SPENSER together for each of the LAD
      regions in England.
    - Note regions E06000053 (Isles of Scilly) and E09000001 (City of London) have very small
      populations so could not run SPENSER for the year 2017 only the census year 2011
      (see line 24).
    - It inputs the dataframes into a list and the list elements are named after the LAD code.
    - Secondly the data is cleaned and matches codes age and NSSEC as those found in the NTS.

4_binding_dfs_by_age_NSSEC.R
    - Found that age and NSSEC had the greatest impact on how many trips an indiviudal took
      in a car.
more info(Philips_ShadboltLIDAinternshare/writings/Justifying variable groups in NTS.docx)
    - Therefore splits the population based on what age/NSSEC group they belong to.
    - The output is a list of 30 elements (6 age groups and 5 NSSEC groups -> 5*6 = 30)

5_masterTable.R
    - Joins each age/NSSEC dataframe to the trip purpose constraint
    - It joins by the following elements: Sex, Age, NSSEC, GOR and ONSRuralUrban


6_distanceTravelled_ct.R
    - This script creates 6 distance travelled constraint tables stored in a list for each of 
      the six trip purpose types: Commuting, Business, Education, Shopping, personal trips and
      leisure.
    - The other constraints are again: Age, Sex, NSSEC, GOR and ONSUrbanRural.
    - Each of the constraint table have different values for different combinations of the
      constraints.
    - These tables are used after the individual has been allocated a trip purpose in the Monte
      Carlo process.

7_count_average_trips.R
    - Calculates the average number of trips each age/NSSEC group took during the weekly travel
      survey (NTS) for the years 2015, 2016 and 2017.
    - Gives a total of 30 average for each age/NSSEC group combination.
    - This is used to determine how many times to repeat the Monte Carlo trip purpose/distance
      allocation process for an individual.

8_age_NSSEC_list_into_GOR.R
    - After the Monte Carlo process and after counting how many each trip purpose and distance 
      combination occurs for the population, this script sorts the individuals back in to their
      government office region.
    - Now have 5 dataframes in a list for each of the 5 GORs.

9_Yearly_Total_Average_Kms_Travelled.R
    - Calculates the total and average yearly mileage for each GOR at the LSOA and OA level.
    - These dataframes are saved in the folder "Weekly_Yearly_Kms/".
    - The "new_Yearly_kms" column is the raw kms multiplied by the proportional constant that
      accounts for those who might not have access to a car. See: "Validation/2_No_Car_Access.R"

10_Total_Average_KMS_by_Trip_Purpose.R
    - Calculates the mean and sum total yearly distance travelled for each trip purpose type 
      by LSOA.
    - These dataframes are saved in the "Weekly_Yearly_Kms/" folder.
    - The "new_Yearly_kms" column is the raw kms multiplied by the proportional constant that
      accounts for those who might not have access to a car. See: "Validation/2_No_Car_Access.R"






















