#This script modifies publicly available data sets and combines them with email data resulting from the correspondence study
#Because manually cleaning some data was involved (e.g., finding missing ori9 values and processing email data)
#Some data cleaning steps are completed before this script is to be used
#Additionally, some publicly available data are downloaded using R (e.g., Census data) whereas some data are downloaded manually (e.g., LEMAS)
#Finally, the fully processed resulting data set is included in the replication files so this script can be skipped for those wishing to use the cleaned data

#Setup: load packages and data ----

library(pacman)

p_load(data.table, here, tidyverse, magrittr, stringr, tidycensus, xlsx, janitor )

#load data

ucr_leoka_data =
  here( '002 Data',
        '01 intermediate data',
        'agency data',
        'leoka_yearly_1960_2020.rds') %>%
  read_rds() %>%
  setDT()

lemas_2020 <- haven::read_dta( here(
  '002 Data', 
  '01 intermediate data',
  'agency data',
  'LEMAS-Data-2020.dta')) %>% 
  clean_names()

lemas_2016 <- haven::read_dta( here(
  '002 Data', 
  '01 intermediate data',
  'agency data',
  'LEMAS-Data-2016.dta')) %>% 
  clean_names()

rushin_2016_cba_data =
  here('002 Data',
       '01 intermediate data',
       'agency data',
       'rushin_2016.xlsx') %>%
  readxl::read_xlsx() %>%
  clean_names()

florida_cba_data = 
  here('002 Data',
       '01 intermediate data',
       'agency data',
       'Agency-Info-PD-2021.xlsx') %>%
  readxl::read_xlsx(skip = 1) %>%
  clean_names()

email_data =
  here('002 Data',
       '01 intermediate data',
       'experiment data',
       'email_data.csv') |>
  read_csv()

SES_data =
  here('002 Data',
       '01 intermediate data',
       'experiment data',
       'SES.csv') |>
  read_csv()

#Step 1: download and modify Census data using `tidycensus` ----
#we are interested in income, poverty status, and racial/ethnic composition at the county level
#you will need a Census API to run this step

#get variable codes for ACS5 2019
v19 <- load_variables(2019, "acs5", cache = TRUE)

acs_data <- get_acs(geography = "county", 
                    variables = c(total_pop = 'B01003_001',
                                  white_pop = 'B03002_003',
                                  black_pop = 'B03002_004',
                                  hispanic_pop = 'B03002_012',
                                  
                                  median_hh_income = 'B19013_001',
                                  median_hh_income_white = 'B19013H_001',
                                  median_hh_income_hispanic = 'B19013I_001',
                                  median_hh_income_black = 'B19013B_001',
                                  
                                  poverty_status_past_year          = 'B17020_002',
                                  poverty_status_hispanic_past_year = 'B17020I_002',
                                  poverty_status_black_past_year = 'B17020B_002',
                                  poverty_status_white = 'B17020H_002'),
                    output = 'wide',
                    year = 2019)

#drop NAME
acs_data %<>% select(-NAME)

#remove margin of error columns
acs_data %<>% select(!ends_with('M'))


#rename GEOID to fips_county_state

acs_data %<>% rename(fips_county_state = GEOID)


#remove E (for estimate) from column names

acs_data %<>% rename_with(~str_remove(., 'E'))

##create percentage variables for all non-median variables 

acs_data %<>% group_by(fips_county_state) %>%
  mutate( 
          percent_black_hisp_pop                                      = round((black_pop + hispanic_pop)/total_pop, 3),
          percent_black_pop                                           = round(black_pop/ total_pop, 3),
          
          percent_hispanic_pop                                        = round( hispanic_pop/total_pop, 3),
          
          percent_poverty_status                                      = round( poverty_status_past_year/total_pop, 3),
          percent_poverty_status_black                                = round( poverty_status_black_past_year/black_pop, 3),
          percent_poverty_status_hispanic                             = round( poverty_status_hispanic_past_year/hispanic_pop, 3),
          percent_poverty_status_white                                = round( poverty_status_white/white_pop, 3),
          percent_white_pop                                           = round( white_pop/total_pop, 3)
          )


#we will combine this df with our other data later in the script

#Step 2: Clean Jacob Kaplan’s Concatenated Files: ----
# Uniform Crime Reporting Program Data: Law Enforcement Officers Killed and Assaulted (LEOKA)
# We will use these data for employee counts for agencies

ucr_leoka_data %<>% select(ori, ori9, agency_name, year, agency_type, male_employees_officers: total_employees_total )

# keep only 'local police department' and 'constable/marshal'
# have to fill in a few missing values for missing documentation for agencies included in study
ucr_leoka_data %<>% mutate(agency_type = case_when(agency_name == 'fulshear' & is.na(agency_type) == T ~ 'local police department',
                                                   agency_name == 'upper macungie township' & is.na(agency_type) == T ~ 'local police department',
                                                   agency_name == 'cecil' & is.na(agency_type) == T ~ 'local police department',
                                                   TRUE ~ agency_type
))

ucr_leoka_data %<>% filter(agency_type == 'constable/marshal' | agency_type == 'local police department') %>% setDT()

# some departments do not have an ori9, replace NA with ori

ucr_leoka_data = ucr_leoka_data[ , ori9 := if_else(is.na(ori9) == T, ori, ori9)]

# we want agency size (total_employees_total), but some places do not report or report 0 for 2019/2020
# so we want to grab the most recent year after 2014 that a zero is not reported

# create a "keep_year" which indicates if there is non-zero data after 2014
ucr_leoka_data = ucr_leoka_data[ , keep_year := case_when(
  year == 2020 & total_employees_total > 0 ~ 2020,
  year == 2019 & total_employees_total > 0 ~ 2019,
  year == 2018 & total_employees_total > 0 ~ 2018,
  year == 2017 & total_employees_total > 0 ~ 2017,
  year == 2016 & total_employees_total > 0 ~ 2016,
  year == 2015 & total_employees_total > 0 ~ 2015,
  #we use year -2014 so that when we filter on max keep year it only picks one observation for each ori9
  TRUE ~  year - 2014),  
  by = ori9]

# keep the max keep_year so that we have the most recent non-zero data (or just one entry for an agency with missing data)
ucr_leoka_data %<>% group_by(ori9) %>% filter(keep_year == max(keep_year)) %>%ungroup()

# convert all values to NA if the first non-zero entry for an ori9 is before 2015
ucr_leoka_data %<>% 
  mutate( across( male_employees_officers:total_employees_total, 
                  ~if_else(keep_year < 2015, 
                           'NA', 
                           as.character(.x))))

# covert columns back to numeric, 'NA' --> NA
ucr_leoka_data %<>% mutate(across( male_employees_officers:total_employees_total, ~as.numeric(.x)))

# keep an indicator for what year the data came from. if before 2015 then just 0 
ucr_leoka_data %<>% mutate(agency_size_year = if_else(keep_year >= 2015, year, 0))

# remove year and keep_year so we just have 'agency_size_year'

ucr_leoka_data %<>% select(-keep_year, -year, -agency_type)

#Step 3: personnel demographics  ----
#clean the DOJ's Law Enforcement Management and Administrative Statistics (LEMAS) data for 2016 and 2020
#LEMAS provides many variables, but we will use them (2016, 2020) for
## officer demographics, complaint policies, number of complaints, and collective bargaining
## Both surveys contain: officer demographics, complaint policies
## 2016 survey contains: collective bargaining, which we will augment with Florida data and Rushin 2016 data
## 2020 survey contains: complaint count and outcomes 
## If both surveys have the variable for the same agency, we will defer to 2020
# we will begin by collecting the race/ethnicity and gender variables from the LEMAS data

##1 First we will remove incomplete surveys and replace missing values with NA. This only applies to the 2020 data as the incompletes were already removed from
## the 2016 survey by the data source

### filter out -8 and 0 for complete

lemas_2020  %<>% filter(complete == 1)

### replace -9 and -8 with NA

lemas_2016[lemas_2016 == -9] = NA
lemas_2016[lemas_2016 == -8] = NA

lemas_2020[lemas_2020 == -9] = NA
lemas_2020[lemas_2020 == -8] = NA

### add an indicator for which LEMAS survey the data are associated with

lemas_2016 %<>% mutate(lemas_year = '2016')
lemas_2020 %<>% mutate(lemas_year = '2020')

#refine 2016 and 2020 lemas into shared variables and then combine

lemas_race_gender_2016 <- lemas_2016 %>% 
  select(
    ori9,
    agencyname,
    lemas_year,
    totftemp, totptemp, ftsworn,
    pers_white_fem, pers_white_male, pers_black_fem, pers_black_male,
    pers_hisp_fem, pers_hisp_male, pers_male, pers_female,
    pers_sup_intm_wh, pers_sup_intm_bk, pers_sup_intm_hs, pers_sup_intm_totr,
    pers_sup_intm_male, pers_sup_intm_fem, pers_sup_intm_tots,
    pers_sup_sgt_wh, pers_sup_sgt_bk, pers_sup_sgt_hs, pers_sup_sgt_totr,
    pers_sup_sgt_male, pers_sup_sgt_fem, pers_sup_sgt_tots,
    pers_chf_sex,
    pers_chf_race)

#lemas 2016 has all chief race variables combined (long format) so need to change to wide format

lemas_race_gender_2016 %<>% 
  mutate( pers_chf_hisp_or = if_else(pers_chf_race == 3, 1, 0),
          pers_chf_race_wht = if_else(pers_chf_race == 1, 1, 0), 
          pers_chf_race_bk = if_else(pers_chf_race == 2, 1, 0),
          chief_poc = case_when(pers_chf_race != 1 & pers_chf_race != 8 ~ 1,
                                pers_chf_race == 1 ~ 0,
                                pers_chf_race == 8 ~ NA)) %>% 
  select(-pers_chf_race)

# lemas 2020
lemas_race_gender_2020 <- lemas_2020 %>% 
  select(
    ori9,
    agencyname,
    lemas_year,
    totftemp, totptemp, ftsworn,
    pers_white_fem, pers_white_male, pers_black_fem, pers_black_male,
    pers_hisp_fem, pers_hisp_male, pers_male, pers_female,
    pers_sup_intm_wh, pers_sup_intm_bk, pers_sup_intm_hs, pers_sup_intm_totr,
    pers_sup_intm_male, pers_sup_intm_fem, pers_sup_intm_tots,
    pers_sup_sgt_wh, pers_sup_sgt_bk, pers_sup_sgt_hs, pers_sup_sgt_totr,
    pers_sup_sgt_male, pers_sup_sgt_fem, pers_sup_sgt_tots,
    pers_chf_sex, pers_chf_hisp_or, pers_chf_race_wht, pers_chf_race_bk) %>%
  mutate(chief_poc = case_when(pers_chf_race_wht != 1 ~ 1,
                               pers_chf_race_wht == 1 ~ 0,
                               pers_chf_race_wht == 8 ~ NA),
         pers_chf_hisp_or = case_when(pers_chf_hisp_or == 1 ~ 1,
                                      pers_chf_hisp_or == 2 ~ 0,
                                      is.na(pers_chf_hisp_or) == T ~ NA))

#combine
lemas_race_gender_all = rbind(lemas_race_gender_2016, lemas_race_gender_2020)

#remove duplicates and defer to 2020
lemas_race_gender_all %<>% 
  group_by(ori9) %>%
  add_count()

lemas_race_gender_all %<>%
  filter(n == 1 | lemas_year == '2020' & n == 2)
## create a few more variables

lemas_race_gender_all %<>%
  mutate( black_sworn_employees = pers_black_fem + pers_black_male,
          hisp_sworn_employees = pers_hisp_fem + pers_hisp_male,
          white_sworn_employees = pers_white_fem + pers_white_male,
          percent_sgt_white = pers_sup_sgt_wh/pers_sup_sgt_totr,
          percent_sgt_black = pers_sup_sgt_bk/pers_sup_sgt_totr,
          percent_sgt_hispanic = pers_sup_sgt_hs/pers_sup_sgt_totr,
          percent_sgt_female = pers_sup_sgt_fem/pers_sup_sgt_tots,
          percent_sgt_male = pers_sup_sgt_male/pers_sup_sgt_tots,
          percent_intm_white = pers_sup_intm_wh/pers_sup_intm_totr,
          percent_intm_black = pers_sup_intm_bk/pers_sup_intm_totr,
          percent_intm_hispanic = pers_sup_intm_hs/pers_sup_intm_totr,
          percent_intm_female = pers_sup_intm_fem/pers_sup_intm_tots,
          percent_intm_male = pers_sup_intm_male/pers_sup_intm_tots)

lemas_race_gender_all %<>% mutate( percent_sworn_black = black_sworn_employees/ftsworn,
                                                   percent_sworn_white = white_sworn_employees/ftsworn,
                                                   percent_sworn_hispanic= hisp_sworn_employees/ftsworn,
                                                   percent_sworn_female = pers_female/ftsworn,
                                                   percent_sworn_male = pers_male/ftsworn,
                                                   all_white_sgt = if_else(percent_sgt_white == 1, 1, 0))

#after inspecting the data it appears a number of agencies did not fill out survey correctly
#because percents > 1 for some agencies. We will remove these observations
lemas_race_gender_all %<>% filter(percent_sworn_black <= 1 & 
                                 percent_sworn_white <= 1 &
                                 percent_sworn_hispanic <= 1 &
                                 percent_sworn_female <= 1 &
                                 percent_sworn_male <= 1)

#remove n column
lemas_race_gender_all %<>% select(-n)
#Step 4: Collective Bargaining Unit ----
#clean the DOJ's Law Enforcement Management and Administrative Statistics (LEMAS) data for 2016
## 2016 survey contains: collective bargaining, which we will augment with Florida data and Rushin 2016 data

## lemas data
lemas_cba_2016 <- 
  lemas_2016 %>% 
  select( ori9,
          pers_colbar_swn
  )

## need to modify the variable
## currently 1=All, 2=Some, 3=None and -9 missing (should already be NA)

lemas_cba_2016 %<>%
  mutate(pers_colbar_swn = case_when(pers_colbar_swn == 1 ~ 1,
                                     pers_colbar_swn == 2 ~ 1,
                                     pers_colbar_swn == 3 ~ 0)) %>%
  rename(collective_bargaining_unit = pers_colbar_swn)

#Rushin 2016
# ori9 numbers have been added in for depts in the experiment
# only want ori9 and collective bargaining unit variable (which we need to add)

rushin_2016_cba_data %<>%
  select(ori9) %>%
  mutate(collective_bargaining_unit = 1)

#Florida data
# ori9 numbers have been added in for depts in the experiment
# only want ori9 and collective bargaining unit variables

florida_cba_data %<>% select(ori9, collective_bargaining_unit)

#covert Yes/No to 1/0

florida_cba_data %<>% mutate(collective_bargaining_unit = if_else(collective_bargaining_unit == 'Yes', 1, 0))

## check for inconsistencies in overlap. i.e., does lemas_2016 and rushin 2016 have different codes for the same agency

temp = inner_join(rushin_2016_cba_data, lemas_cba_2016, 'ori9')

which(temp$collective_bargaining_unit.x != temp$collective_bargaining_unit.y) #two inconsistencies

temp[42,1] #Denton, manually verified == 1 defer to Rushin 2016
temp[41,1] #Dallas, manually verified == 1 defer to Rushin 2016

#remove Denton and Dallas from Lemas

lemas_cba_2016 %<>% filter(ori9 != 'TX0610200' & ori9 != 'TXDPD0000')

## check for inconsistencies in overlap. i.e., does lemas_2016 and FDLE have different codes for the same agency

temp = inner_join(florida_cba_data, lemas_cba_2016, 'ori9')

which(temp$collective_bargaining_unit.x != temp$collective_bargaining_unit.y) #no inconsistencies

#combine all cba data

cba_data =
  rbind(florida_cba_data, rushin_2016_cba_data, lemas_cba_2016) %>%
  group_by(ori9) 

cba_data %<>%
  distinct(ori9, .keep_all = T)

##Step 5: LEMAS 2020 survey contains: complaint count and outcomes ----

lemas_complaint =
  lemas_2020 %>%
  select(ori9,
         compl_all_sust,
         compl_all_pend,
         compl_all_tot,
         ftsworn, ptsworn)
#replace -9 with NA except for ptsworn, for ptsworn, replace with 0

lemas_complaint %<>%
  mutate(across(ori9:ftsworn,
                ~if_else(.x == -9,
                         NA,
                         .)))

lemas_complaint %<>%
  mutate(ptsworn = ifelse(ptsworn == -9,
                          0,
                          ptsworn))
#remove agenices with NA for all complaint statistics and agencies where compl_all_tot is missing

lemas_complaint %<>% 
  filter(!(is.na(compl_all_sust) & is.na(compl_all_pend) & is.na(compl_all_tot) | is.na(compl_all_tot)))

# create a few variables
# 1: any complaints
# 2: sustained rate
# 3: total complaints per officers

lemas_complaint %<>%
  mutate(complaint_any = if_else(compl_all_tot > 0, 1, 0), #if an agency had any complaints (binary outcome)
         complaint_sust_rate = compl_all_sust/(compl_all_tot - compl_all_pend), #the rate of completed complaints that were sustained
         complaint_per_officer = compl_all_tot/(ftsworn + ptsworn )) #the number of complaints per officer for a given agency

#remove very improbable outliers

lemas_complaint %<>% filter(complaint_per_officer < 8)

#remove columns that appear in the lemas_race_gender_all df

lemas_complaint %<>% select(-ftsworn, -ptsworn)
#Step 6: combine public data with email data----
# we have:
#acs_data (census demographics)
#ucr_leoka_data (agency size)
#lemas_race_gender_all (agency personnel race/ethnicity and gender demographics)
#cba_data (which agencies have collective bargaining units)
#lemas_complaint (statistics on complaints for sampled agencies)
#we want to join these data with the email_data (the results from the audit)

#join acs data with acs data using the fips_county_state variable
full_data = left_join(email_data, acs_data, 'fips_county_state')

#join ucr_leoka_data using ori9

full_data = left_join(full_data, ucr_leoka_data, 'ori9')

#join lemas gender/race/ethnicity data using ori9

full_data = left_join(full_data, lemas_race_gender_all, 'ori9')

#join cba_data using ori9

full_data = left_join(full_data, cba_data, 'ori9')

#join lemas_complaint data using ori9

full_data = left_join(full_data, lemas_complaint, 'ori9')

#Step 7: add census----
#add Census regions and divisions using state codes

# add census region/division
full_data %<>%
  mutate( census_division = case_when( state_code == '09' ~ 'New England',
                                       state_code == '23' ~ 'New England',
                                       state_code == '25' ~ 'New England',
                                       state_code == '33' ~ 'New England',
                                       state_code == '44' ~ 'New England',
                                       state_code == '50' ~ 'New England',
                                       
                                       state_code == '34' ~ 'Mid Atlantic',
                                       state_code == '36' ~ 'Mid Atlantic',
                                       state_code == '42' ~ 'Mid Atlantic',
                                       
                                       state_code == '18' ~ 'East North Central',
                                       state_code == '17' ~ 'East North Central',
                                       state_code == '26' ~ 'East North Central',
                                       state_code == '39' ~ 'East North Central',
                                       state_code == '55' ~ 'East North Central',
                                       
                                       state_code == '19' ~ 'West North Central',
                                       state_code == '20' ~ 'West North Central',
                                       state_code == '27' ~ 'West North Central',
                                       state_code == '29' ~ 'West North Central',
                                       state_code == '31' ~ 'West North Central',
                                       state_code == '38' ~ 'West North Central',
                                       state_code == '46' ~ 'West North Central',
                                       
                                       state_code == '10' ~ 'South Atlantic',
                                       state_code == '11' ~ 'South Atlantic',
                                       state_code == '12' ~ 'South Atlantic',
                                       state_code == '13' ~ 'South Atlantic',
                                       state_code == '24' ~ 'South Atlantic',
                                       state_code == '37' ~ 'South Atlantic',
                                       state_code == '45' ~ 'South Atlantic',
                                       state_code == '51' ~ 'South Atlantic',
                                       state_code == '54' ~ 'South Atlantic',
                                       
                                       state_code == '01' ~ 'East South Central',
                                       state_code == '21' ~ 'East South Central',
                                       state_code == '28' ~ 'East South Central',
                                       state_code == '47' ~ 'East South Central',
                                       
                                       state_code == '05' ~ 'West South Central',
                                       state_code == '22' ~ 'West South Central',
                                       state_code == '40' ~ 'West South Central',
                                       state_code == '48' ~ 'West South Central',
                                       
                                       state_code == '04' ~ 'Mountain',
                                       state_code == '08' ~ 'Mountain',
                                       state_code == '16' ~ 'Mountain',
                                       state_code == '35' ~ 'Mountain',
                                       state_code == '30' ~ 'Mountain',
                                       state_code == '49' ~ 'Mountain',
                                       state_code == '32' ~ 'Mountain',
                                       state_code == '56' ~ 'Mountain',
                                       
                                       state_code == '02' ~ 'Pacific',
                                       state_code == '06' ~ 'Pacific',
                                       state_code == '15' ~ 'Pacific',
                                       state_code == '41' ~ 'Pacific',
                                       state_code == '53' ~ 'Pacific'))

full_data %<>% mutate( census_region  = case_when( census_division == 'New England' | census_division == 'Mid Atlantic' ~ 'Northeast',
                                                         census_division == 'East North Central'| census_division == 'West North Central' ~ 'Midwest',
                                                         census_division == 'South Atlantic'| census_division =='East South Central' | census_division == 'West South Central'~ 'South',
                                                         census_division == 'Mountain'| census_division == 'Pacific' ~ 'West'))

#Step 8: write file ----

write_csv(full_data,
          here('002 Data',
               '02 final data',
               '02_all_department_data.csv'))
#Step 8 distinct departments----
#for some of our analysis, we want to a file that has all local police departments
#but we want distinct ori9 to remove departments that appear multiple times
#for example, a department could appear multiple times in the data because they responded multiple times
#and each response is its own row
#or the case of the Dallas PD where the department's various branches were all contacted

distinct_depts = full_data %>% 
  distinct(ori9, .keep_all = T)

write_csv( distinct_depts,
           here('002 Data',
                '02 final data',
                '03_distinct_departments_data.csv'))

#Step 9a: ----
#we will also create a smaller dataset that we will use for the bulk of the analysis
#this will be restricted to departments that were both selected for the study
#and the emails did not bounce ("denied" and "failed")
#we will also remove the extra responses from departments, that is we will only keep a department's
#first response (some departments sent multiple emails)
#we will also add:
#(1) an implied age variable 
#(2) add the SES associated with first names (Black and White) from Gaddis (2017). See appendix for details.

main_email = full_data %>%
  filter(searched == 1 & has_email_address == 1 & denied != 1 & failed != 1) %>%
  #the first line indicates that the dept. (1) was selected for the study
  #(2) had a publicly available email address
  #(3) the contact email didn't bounce
  filter(!(response == 0 & not_with_agency == 1 )) %>%
  #the next line removes agencies that responded with an automatic response of "no longer with dept"
  filter(response <= 1)
  #finally, we only want the first response (or no response) from a department

#Step 9b: implied age ----
#use the email address of the sender to create an implied age 
main_email %<>% 
  mutate( implied_age = case_when(sender_email == 'banksss.1991@gmail.com' ~ 2022 -1991,
                                sender_email == 'gonzzalezzz.1994@gmail.com' ~ 2022 - 1994,
                                sender_email == 'h3rnand3z.1973@gmail.com' ~ 2022 - 1973,
                                sender_email == 'hans3n.1972@gmail.com' ~ 2022 - 1972,
                                sender_email == 'jacksonnn.1990@gmail.com' ~ 2022 - 1990, 
                                sender_email == 'jeffersn78@gmail.com' ~ 2022 - 1978,
                                sender_email == 'joe.seph1952@gmail.com' ~ 2022 - 1952,
                                sender_email == 'larsonn.1952@gmail.com' ~ 2022 - 1952,
                                sender_email == 'lopezjr.0621@gmail.com' ~ 0,
                                sender_email == 'm.ramirezz.1986@gmail.com' ~ 2022 - 1986,
                                sender_email == 'martin3z.1978@gmail.com' ~ 2022 - 1978,
                                sender_email == 'meyerrr.1982@gmail.com' ~ 2022 - 1982,
                                sender_email == 'olson2292@gmail.com' ~ 0,
                                sender_email == 'schmidt.04513@gmail.com' ~ 0,
                                sender_email == 'snyderrr.1992@gmail.com' ~ 2022 -  1992,
                                sender_email == 'the.rodriguezzz1992@gmail.com' ~ 2022 - 1992,
                                sender_email == 'the.williams.1956@gmail.com' ~ 2022 - 1956,
                                sender_email == 'washingtonn.1962@gmail.com' ~ 2022 - 1962
))

main_email %<>% mutate(age_dummy = if_else(implied_age == 0, 0, 1))

#Step 9c: SES ----
#just keep name and educational attainment from SES

SES_data %<>% select(name, hs, college)

main_email = left_join(main_email, SES_data, by = c('sender_first_name' = 'name'))

#Step 10----
#write file

write_csv( main_email,
           here('002 Data',
                '02 final data',
                '04_main_data.csv'))

