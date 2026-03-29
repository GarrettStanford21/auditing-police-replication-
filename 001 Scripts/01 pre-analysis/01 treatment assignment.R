# load packages ----

library(pacman)

p_load(xml2, data.table, here, tidyverse, magrittr, XML, stringr, 
       tidycensus, xlsx, janitor, purrr, openxlsx, stringi, randomizr, RCT, ggplot2, lutz)


# load data ----

#police departments selected for study.
#Includes: dept. name, email address, dept. state, lat and lon.
police_departments =
  here('002 Data',
       '01 intermediate data',
       'experiment data',
       'selected_police_departments.csv') |>
  read_csv()


#identities used for study.
#Includes: first name, last name, full name, race/ethnicity (White, Black, Hispanic) and gender (male, female)
identities =
  here('002 Data', 
       '01 intermediate data',
       'experiment data',
       'audit_identities.csv') |>
  read_csv()

# Step 1: ----
# Assign the week that the email should be sent out. This experiment is rolled out over 10 weeks.
# This is done to minimize temporal bias (e.g., a high-profile officer-involved incident).
# It also lowers the logistical burden on the researcher's part.

#split into state specific dataframes
split_data <- split.data.frame( x = police_departments, f= police_departments$dept_address_state)

week_assignment_function <- function(state){
  df <- split_data[state] %>% as.data.frame() #grab state
  week_index_total <- nrow(df)/10 # how many full sets of ten 
  week_index_total %<>% floor() # round down to the number of full sets of ten
  week_index <- rep( 1:10, each = week_index_total) #create 1:10 index for each full set of ten
  week_index_extra <- sample(1:10, nrow(df) - week_index_total*10, replace = F) # sample from 1:10 for remainder occurrences
  week_index_full <- c(week_index, week_index_extra) # combine full sets of 10 and remainder occurrences indexes
  df <- df %>% mutate( week = sample(week_index_full, size =  nrow(df), replace = F)) #assign by state to weeks 1-10
  names(df) <- c("dept_name", "dept_email", "dept_address_state", "lat" , "lon", 'week')    # convert names back to original
  return(df)
}

#we will call the dataframe that we slowly build "experiment data"
experiment_data <- do.call(rbind, lapply(c(1:49), week_assignment_function)) # bind together all of the state dfs 

#check to see that the function worked properly

# number of emails by week
temp = 
  experiment_data |>
  group_by(week) |>
  count()

# number of emails by week by state

temp = 
  experiment_data |>
  group_by(week, dept_address_state) |>
  count() |>
  arrange(dept_address_state)

# Step 2: ----
# assign race/ethnicity 1/3 each. Group by state. Dept is already randomized into week bins. 
# missfits are assigned by global to keep treatment balanced as a total (versus within week)
# as a result race should be evenly spread across weeks as a result 

#use `treatment_assign` from RCT package


treatment_data_list_race <- treatment_assign( data = experiment_data,
                                              share_control = (1/3), #evenly assign Black, White, and Hispanic
                                              n_t = 2, #treatment is n-1
                                              strata_varlist = dplyr::vars(dept_address_state), #we will use the dept_email as the unique ID (key)
                                              missfits = "global", #keep treatment balanced as a total (versus within week)
                                              seed = 3707,
                                              key = 'dept_email') #we will stratify at the state level

#take a look at the summary of treatment assignment
treatment_data_race_summary <- treatment_data_list_race$summary_strata

#the actual treatment assignment lives in the 'data' list produced by `treatment_assign`
treatment_data_race <- treatment_data_list_race$data

#rename treat (our treatment assignment) as race and remove the missfit identifier and strata
treatment_data_race %<>%
  rename( race = treat) %>%
  ungroup() %>%
  select( -missfit, -strata)

# Note: our treatment assignment will be designated as follows (arbitrary choice, but important to keep track of)
# white    = 0
# black    = 1
# hispanic = 2

#combine the race/ethnicity treatment assignment with our experiment_data using email address

experiment_data <- left_join(experiment_data,
                             treatment_data_race,
                             by = 'dept_email') 

# Step 3: ----
# repeat exercise for gender
# add an additional strata of "race" so that the gender assignment is roughly equal across Black, White, Hispanic
treatment_data_list_gender <- treatment_assign( data = experiment_data,
                                              share_control = (1/2),
                                              n_t = 1,
                                              strata_varlist = dplyr::vars(race, dept_address_state),
                                              missfits = "global",
                                              seed = 3707,
                                              key = 'dept_email')


#take a look at the summary of treatment assignment
treatment_data_gender_summary <- treatment_data_list_gender$summary_strata

#the actual treatment assignment lives in the 'data' list produced by `treatment_assign`
treatment_data_gender <- treatment_data_list_gender$data

#rename treat (our treatment assignment) as gender and remove the missfit identifier and strata
treatment_data_gender %<>%
  rename( gender = treat) %>%
  ungroup() %>%
  select( -missfit, -strata)

# Note: our treatment assignment will be designated as follows (arbitrary choice, but important to keep track of)
# female    = 0
# male      = 1


#combine the gender treatment assignment with our experiment_data using email address

experiment_data <- left_join(experiment_data,
                             treatment_data_gender,
                             by = 'dept_email') 

#Step 4: ----
#we will now assign names from our list of idenitites based on the race/ethnicity & gender treatment assignment
#there are 60 names for each identity (e.g., Black male) and there are six identities 
#there are only 354 names in the `identities` data frame (run: nrow(identities) ) because six names were removed
#as they coincided with celebrity names (e.g., "Tyra Banks")


#we will assign a name index (a number) using treatment assign, and then convert that index to an actual name

treatment_data_list_names <- treatment_assign( data = experiment_data,
                                               share_control = (1/60),
                                               n_t = 59,
                                               strata_varlist = dplyr::vars(race, gender), #stratify by indentity 
                                               missfits = "global",
                                               seed = 5621,
                                               key = 'dept_email')

#inspect data  
treatment_data_names_summary <- treatment_data_list_names$summary_strata

#extract treatment assignment
treatment_data_names <- treatment_data_list_names$data  

#modify dataframe
treatment_data_names %<>%
  rename( name = treat) %>%
  ungroup() %>%
  select(-missfit, -strata)

#modify name index to run from 1:60 instead of 0:59
treatment_data_names %<>% mutate( name = name + 1)

#combine the name index treatment assignment with our experiment_data using email address

experiment_data <- left_join(experiment_data, treatment_data_names, by = 'dept_email')


#Step 5: ----
# now that we have assigned name indexes, we will add the actual names to the experiment_data dataframe

#first we will create lists of names for the six identities which we will use later

hispanic_female_names <- identities %>%
  filter(hispanic == 1 & female == 1) %>% 
  select(full_name) %>% as.list()

hispanic_female_names$full_name %<>% as.list()

hispanic_male_names <- identities %>%
  filter(hispanic == 1 & male == 1) %>% 
  select(full_name) %>% as.list()

hispanic_male_names$full_name %<>% as.list()

black_female_names <- identities %>%
  filter(black == 1 & female == 1) %>% 
  select(full_name) %>% as.list()

black_female_names$full_name %<>% as.list()

black_male_names <- identities %>%
  filter(black == 1 & male == 1) %>% 
  select(full_name) %>% as.list()

black_male_names$full_name %<>% as.list()

white_female_names <- identities %>%
  filter(white == 1 & female == 1) %>% 
  select(full_name) %>% as.list()

white_female_names$full_name %<>% as.list()

white_male_names <- identities %>%
  filter(white == 1 & male == 1) %>% 
  select(full_name) 

white_male_names$full_name %<>% as.list()


# before we assign names, we need to remove a few name indices
# Black males are missing 2 names (58 total)
# Black females are missing 1 names (59 total)
# White males are missing 1 name (59 total)
# White females are missing 1 name (59 total)
# Hispanic males are missing 1 name (59 total)

# replace the out of bounds indices with a random draw
experiment_data %<>% 
  mutate( name = if_else(race == 0 & gender == 0 & name == 60, #replace white female name index 60 
                         sample(c(1:59), size = 2135, replace = T), name), #with random draws 1:59
          name = if_else(race == 0 & gender == 1 & name == 60, #replace white male name index 60 
                         sample(c(1:59), size = 2135, replace = T), name), #with random draws 1:59
          name = if_else(race == 1 & gender == 0 & name == 60, #replace black female name index 60 
                         sample(c(1:59), size = 2135, replace = T), name), #with random draws 1:59
          name = if_else(race == 1 & gender == 1 & name == 60|race == 1 & gender == 1 & name == 59, #replace black male name indices 59 & 60
                        sample(c(1:58), size = 2135, replace = T), name), #with random draws 1:58
         name = if_else(race == 2 & gender == 1 & name == 60, #replace Hispanic male name index 60
                        sample(c(1:59), size = 2135, replace = T), name)#with random draws 1:59
         )

#we will now assign the name from our identities data frame using the assigned name indices
#we will create a new variable called "sender_name" which will be the full name (e.g., "Claire Olson")

experiment_data %<>%
  mutate( sender_name = 
            case_when( race == 0 & gender == 0 ~ white_female_names$full_name[experiment_data$name],
                       race == 0 & gender == 1 ~ white_male_names$full_name[experiment_data$name],
                       race == 1 & gender == 0 ~ black_female_names$full_name[experiment_data$name],
                       race == 1 & gender == 1 ~ black_male_names$full_name[experiment_data$name],
                       race == 2 & gender == 0 ~ hispanic_female_names$full_name[experiment_data$name],
                       race == 2 & gender == 1 ~ hispanic_male_names$full_name[experiment_data$name]))

#we will also create variables for the first and last name
experiment_data %<>%
  mutate( sender_last_name = word(sender_name, 2),
          sender_first_name = word(sender_name, 1))


#Step 6: ----
#We will also assign as treatment the day of week the email is to be sent
#All emails are sent M,T, or W to avoid late-week effects
#Assignment needs to be stratified by sender_last_name and week. 
#This is done to minimize use of single researcher email account per day (this can cause administrative issues)

treatment_data_list_day <- treatment_assign( data = experiment_data,
                                               share_control = (1/3), #Monday, Tuesday, Wednesday
                                               n_t = 2, #n-1
                                               strata_varlist = dplyr::vars(sender_last_name, week),
                                               missfits = "strata",
                                               seed = 5621,
                                               key = 'dept_email')
#check summary
treatment_data_day_summary <- treatment_data_list_day$summary_strata

#extract data
treatment_data_day <- treatment_data_list_day$data

# rename treat to day_of_week
treatment_data_day %<>%
  rename( day_of_week = treat) %>%
  ungroup() %>%
  select( -missfit, -strata)

#convert treatment to day of week
treatment_data_day %<>% 
  mutate( day_of_week = case_when(day_of_week == 0 ~ 'Monday',
                                  day_of_week == 1 ~ 'Tuesday',
                                  day_of_week == 2 ~ 'Wednesday'))

# join the day of week treatment to data table

experiment_data = left_join(experiment_data, treatment_data_day, by = 'dept_email')


#Step 7: ----
#We will assign an additional treatment that is the email sign-off
#this varies between "sincerely," (a cool/neutral tone) and "Thank you!" (a warm tone)
#we will stratify by race, gender, and state

treatment_data_list_signoff <- treatment_assign( data = experiment_data,
                                                share_control = (1/2),
                                                n_t = 1,
                                                strata_varlist = dplyr::vars(race, 
                                                                             gender,
                                                                             dept_address_state),
                                                missfits = "global",
                                                seed = 3707,
                                                key = 'dept_email')

#check summary
treatment_data_signoff_summary <- treatment_data_list_signoff$summary_strata

#extract data
treatment_data_signoff <- treatment_data_list_signoff$data

#modify data
treatment_data_signoff %<>% 
  rename( sign_off = treat) %>% 
  ungroup() %>%
  select(-missfit, -strata)

#convert sign_off into the two sign-offs 

treatment_data_signoff %<>% 
  mutate( sign_off = if_else(sign_off == 1, "Thank you!", 'Sincerely,'))

#add to experimental_data
experiment_data = left_join(experiment_data, treatment_data_signoff, by = 'dept_email') 

#Step 8: ----
#We will assign a time for sending the email.
#The goal is to have emails sent at roughly the same time every day to keep treatment consistent.
#This time needs to be consistent with the department's time zone which we can extract from the lat/lon for dept's

#we need timezone for each department
#first we will use lat/lon to get the name of the timezone and create a variable called `timezone`

experiment_data %<>% 
  mutate(timezone = tz_lookup_coords( lat = round(lat, digits = 3) ,
                                      lon = round(lon, digits = 3),
                                      method = 'accurate'))

#we will use the timezone to get a UTC-offset so that we can adjust department times to PST
#get list for converting timezone name into utc offset

tz_list <- tz_list() %>%
  filter(is_dst == T) %>%
  select(tz_name, utc_offset_h)

#join tz_list to main data

experiment_data <- left_join(experiment_data, tz_list, by = c('timezone' = 'tz_name'))

#for some reason (possibly because it does not observe DST), America/Phoenix is missing from the tz_list
#we will manually fix that by replacing NA with -7
#fix phoenix
experiment_data %<>% 
  mutate( utc_offset_h = if_else( is.na(utc_offset_h) == T, -7, utc_offset_h)) 

#we will normalize utc offset to PST
#for example, New York will be +3

experiment_data %<>% mutate( utc_offset_h = utc_offset_h + 7)

#we want to space out the sending of the emails (which we can automate)
#to reduce the chances the email accounts are flagged for suspicious activity (i.e., spamming)
#create send time (hour and minute) index by grouping by day, week, sender_last_name
#this way if the same email account is supposed to send multiple emails at the same time on the same day
#they are spaced apart

#index for time
experiment_data %<>% 
  group_by(week, day_of_week, sender_last_name, utc_offset_h) %>%
  mutate( time_index = row_number()) %>%
  ungroup()

#convert index into time. Baseline is 9am (add 2 minutes to make it seem less automated),
#then add 5 minutes in between for each sender_last_name on same day of same week (0.05*(time_index -1)). 
#Adjust by UTC offset
experiment_data %<>% 
  mutate( time_to_send = 9.02 + (.05*(time_index-1)) - utc_offset_h )

#Step 9: ----
#reorder data and write file

experiment_data %<>%
  select(week, day_of_week, time_to_send,
         sign_off, sender_last_name, sender_first_name, sender_name,
         dept_email,
         race, gender, name,
         everything())

experiment_data %<>% arrange(week, day_of_week, time_to_send)

#write the data as excel or csv depending on how email automation will work

write_csv(experiment_data, 
          here( '002 Data',
                '02 final data',
                '01_outgoing_email_data.csv'))

write.xlsx(experiment_data, 
          here( '002 Data',
                '02 final data',
                '01_outgoing_email_data.xlsx'))


