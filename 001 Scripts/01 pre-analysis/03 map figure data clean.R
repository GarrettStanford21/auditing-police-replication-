#This script creates data sets used to create the map figures (Figure 1 and A3)
#The majority of data cleaning is done in `02 general data clean.R`
#This script begins with a product of that script: 02_all_department_data.csv
#which can be found in: '002 Data' -> 'final data'

###Setup: load packages and data ----

library(pacman)
p_load(fastverse, here, stringr, ggplot2, magrittr, tidyverse, data.table, janitor)


# data
department_data =
  here('002 Data',
       '02 final data',
       '03_distinct_departments_data.csv') |>
  read_csv()


population_data =
  here('002 Data',
       '01 intermediate data',
       'figure data',
       'census_pop_2020.csv') |>
  read_csv() |>
  clean_names()


#1 select columns of interest. Not required but helps keep operation clean ----

department_data %<>%
  select(dept_name, state_name, state, has_email_address, searched, population)

#2 modify has_email_address variable ----
#currently has_email_address == 2 if the department was not selected for inclusion in the study
#convert 2's to 0's

department_data %<>%
  mutate(has_email_address = if_else(searched == 0, 
                                     0,
                                     has_email_address))

#3 filter population data to state level and select variables of interest ----

population_data =
population_data |>
  filter(sumlev == '040') |>
  select(state, stname, popestimate2020)

names(population_data) = c('state_fips', 'state_name', 'state_population_2020')

#4 join state population data with department data ----

department_map_data =
  left_join(department_data, population_data, by = c('state_name')) |>
  setDT()

#5 we need to create four variables at the state level the describe the reprensativeness of the study----
#1 % pop represented for each state (sum of pop served by departments in a state),
#2 % of departments in each state contacted,
#3 % of departments serving \geq 7,500 
#4 raw # of departments contacted for each state

#get the number of departments in each state
department_map_data[ , state_dept_count := .N, state_name]

#get the number of departments contacted in each state

department_map_data[ , contacted_dept_count := sum(has_email_address), state_name]

#calculate the sum of the state population served by contacted departments
department_map_data[ , 
                     pop_served := sum(population*has_email_address),
                     by = .(state_name)]

#repeat process for 7500 pop departments
#we can filter out departments as we only need one entry per state
department_map_data %<>% filter(population >= 7500)

department_map_data[ , state_dept_count_7500 := .N, state_name]

department_map_data[ , contacted_dept_count_7500 := sum(has_email_address), state_name]


#6 reduce data to one row per state ----
department_map_data %<>% distinct(state_name, .keep_all = T)

#7 create percents ----

department_map_data %<>% mutate(per_contacted = contacted_dept_count/ state_dept_count,
                      per_contacted_7500 = contacted_dept_count_7500/ state_dept_count_7500,
                      per_pop = pop_served/state_population_2020)


#8 create bins for proportions ----
# we want to use a heat map but the figure cannot use color so we only use 4 bins

#find max and mins divide by 4
#per_contacted 7:59
seq(7, 59, ((59-7)/4)) %>% round()
#per_contacted_7500 28:95
seq(28, 95, ((95-28)/4)) %>% round()
#per_pop 11:84
seq(11, 84, ((84-11)/4)) %>% round()
#count 6:138
seq(6, 138, ((138-6)/4)) %>% round()

department_map_data %<>% 
  mutate( prop_contacted_bin = 
            case_when(per_contacted < .20 ~ '7-20%',
                      per_contacted < .33 ~ '20-33%',
                      per_contacted < .46 ~ '33-46%', #note there are no states in this range 
                      TRUE ~ '46-59%'),
          prop_contacted_7500_bin =
            case_when(per_contacted_7500 < .45 ~ '28-45%',
                      per_contacted_7500 < .62 ~ '45-62%',
                      per_contacted_7500 < .78 ~ '62-78%',
                      TRUE ~ '78-95%'),
          prop_pop_bin =
            case_when(per_pop < .29 ~ '11-29%',
                      per_pop < .48 ~ '29-48%',
                      per_pop < .66 ~ '48-66%',
                      TRUE ~ '66-84%'),
          dept_count_bin =
            case_when(contacted_dept_count < 39 ~ '6-39',
                      contacted_dept_count < 72 ~ '39-72',
                      contacted_dept_count < 105 ~ '72-105',
                      TRUE ~ '105-138'),
                                  ) 
#9 write file

write_csv(department_map_data, here('002 Data', '02 final data', 'department_map_data.csv'))
