#This script creates the tables shown in the article requiring statistical analysis (A.3, A.4 and, A.9 are not included)
#Tables are organized by the order in which they appear in the paper
#The data set is a combination of publicly available data and the results of the correspondence study
#Three different data files: 02_all_department_data, 03_distinct_departments, & 04_main_data
#have been created to minimize data manipulation in this script
#To see how these data files were created see `02 data clean.R` (001 Scripts -> pre-analysis folder) of the R scripts
###Setup: load packages and data ----
library(pacman)

p_load(data.table, here, tidyverse, magrittr, stringr, tidycensus, xtable, stargazer,
       fixest, lmtest, car, marginaleffects, scales, kableExtra, modelsummary, janitor
       )

#load data

#all local police departments in the U.S. joined with correspondence study results, includes duplicated agencies

all_departments =
  here('002 Data',
       '02 final data',
       '02_all_department_data.csv') |>
  read_csv()

#all local police departments in the U.S. joined with correspondence study results
distinct_departments =
  here('002 Data',
       '02 final data',
       '03_distinct_departments_data.csv') |>
  read_csv()

#primary analysis data

main_data =
  here('002 Data',
       '02 final data',
       '04_main_data.csv') |>
  read_csv()

### Table 1 ----
# create a table that breaks down emails sent in study by outcome

temp = tibble(
'Email Outcome' = c( 
  'Single response',
  'Multiple response',
  'No response',
  'Denied',
  'Failed'),
Total = c(
single_response = 
  all_departments |>
  filter(response == 1) |>
  nrow() - (all_departments |>
  filter( response == 2) |>
  nrow()),

multiple_response = 
  all_departments |>
  filter( response == 2) |>
  nrow(),

no_response = 
  all_departments |>
  filter(has_email_address == 1 & response == 0)|>
  filter(!(failed == 1 | denied == 1| not_with_agency == 1)) |>
  nrow(),

denied = 
  all_departments |>
  filter(denied == 1) |>
  nrow(),

failed = 
  all_departments |>
  filter(failed == 1 | response == 0 & not_with_agency == 1) |>
  nrow()

)
)

temp %<>% 
  mutate('Percent of Total' = round(Total/(sum(Total))*100, 2))

temp_table = kableExtra::kbl(temp,
                col.names = NULL,
                format = 'latex',
                booktabs = T) %>%
  kable_styling() 

#write as file

write(temp_table, file=here('003 Output', 'Tables', "table_1.tex"))


#additional editing done in LaTex
rm(temp, temp_table)

### Table 2 ----

####modify data for table
#create "contacted variable." 1 if was sent an email and 0 if not

distinct_departments %<>%
  mutate(contacted = if_else(has_email_address == 1, 1, 0)) #takes on 0 if no email address or not selected for study

#create data subsets for analysis and table 
department_eligible = distinct_departments %>% filter(population >= 7500) #pop needs to be >7500 to be eligible 

department_included = distinct_departments %>% filter(contacted == 1) #several departments with populations <7,500 were incidentally included

department_response = main_data %>% filter(response == 1)

department_no_response = main_data %>% filter(response == 0)

####regression table function
table_reg_function =
  function(var1, var2, name, data, scale, sample){
    
    # Build the formula from string
    fml <- as.formula(paste(var1, "~ ", var2))
    
    # Run regression
    model_temp <- feols(fml, data = data)
    
    temp = model_temp$coeftable[, 1:2] %>%
      mutate(label = c('mean', 'difference'),
             variable = name) %>%
      clean_names()
    
    temp %<>% pivot_wider(names_from = 'label', values_from = c('estimate', 'std_error')) %>%
      select( c(1:3, 5))
    
    temp %<>% mutate(across(2:4, ~round(.x/scale, 2)))
    
    temp[ , 4] = paste0('(', temp[,4], ')')
    
    names(temp) = c(paste(sample, 'Variable', sep = '_'),
                    paste(sample, 'Mean', sep = '_'),
                    paste(sample, 'Difference', sep = '_'),
                    paste(sample, 'se', sep = '_'))
    
    temp
  }


#### run regression function for all departments, departments with pop > 7,500, and searched departments
dept_eligible = rbind(
  table_reg_function('median_hh_income', 'contacted', 'Median income all HH ($1,000s)', department_eligible, 1000, 'all_departments'),
  table_reg_function('median_hh_income_black', 'contacted','Median income Black HH ($1,000s)', department_eligible, 1000, 'all_departments'),
  table_reg_function('median_hh_income_hispanic', 'contacted', 'Median income Hispanic HH ($1,000s)', department_eligible, 1000, 'all_departments'),
  table_reg_function('median_hh_income_white', 'contacted', 'Median income White HH ($1,000s)', department_eligible, 1000, 'all_departments'),
  
  table_reg_function('percent_poverty_status', 'contacted', '% Pop. in poverty', department_eligible, .01, 'all_departments'),
  table_reg_function('percent_poverty_status_black', 'contacted', '% Black pop. in poverty', department_eligible, .01, 'all_departments'),
  table_reg_function('percent_poverty_status_hispanic','contacted', '% Hispanic pop. in poverty', department_eligible, .01, 'all_departments'),
  table_reg_function('percent_poverty_status_white','contacted', '% White pop. in poverty', department_eligible, .01, 'all_departments'),
  
  table_reg_function('population','contacted', 'Local population (1000s)', department_eligible, 1000, 'all_departments'),
  table_reg_function('percent_black_pop','contacted', 'Pop. % Black (county-level)', department_eligible, .01, 'all_departments'),
  table_reg_function('percent_hispanic_pop','contacted', 'Pop. % Hispanic (county-level)', department_eligible, .01, 'all_departments'),
  table_reg_function('percent_white_pop','contacted', 'Pop. % White (county-level)', department_eligible, .01, 'all_departments'),
  
  table_reg_function('total_employees_total','contacted', 'Total employees', department_eligible, 1, 'all_departments'),
  table_reg_function('total_employees_officers','contacted', 'Total officers', department_eligible, 1, 'all_departments'),
  table_reg_function('total_employees_civilians','contacted', 'Total civilian employees', department_eligible, 1, 'all_departments')
)

dept_response = rbind(
  table_reg_function('median_hh_income', 'response', 'Median income all HH ($1,000s)', main_data, 1000, 'departments_no_resp'),
  table_reg_function('median_hh_income_black', 'response', 'Median income Black HH ($1,000s)', main_data, 1000, 'departments_no_resp'),
  table_reg_function('median_hh_income_hispanic', 'response', 'Median income Hispanic HH ($1,000s)', main_data, 1000, 'departments_no_resp'),
  table_reg_function('median_hh_income_white', 'response', 'Median income White HH ($1,000s)', main_data, 1000, 'departments_no_resp'),
  
  table_reg_function('percent_poverty_status', 'response', '% Pop. in poverty', main_data, .01, 'departments_no_resp'),
  table_reg_function('percent_poverty_status_black', 'response', '% Black pop. in poverty', main_data, .01, 'departments_no_resp'),
  table_reg_function('percent_poverty_status_hispanic', 'response', '% Hispanic pop. in poverty', main_data, .01, 'departments_no_resp'),
  table_reg_function('percent_poverty_status_white', 'response', '% White pop. in poverty', main_data, .01, 'departments_no_resp'),
  
  table_reg_function('population', 'response', 'Local population (1000s)', main_data, 1000, 'departments_no_resp'),
  table_reg_function('percent_black_pop', 'response', 'Pop. % Black (county-level)', main_data, .01, 'departments_no_resp'),
  table_reg_function('percent_hispanic_pop', 'response', 'Pop. % Hispanic (county-level)', main_data, .01, 'departments_no_resp'),
  table_reg_function('percent_white_pop', 'response', 'Pop. % White (county-level)', main_data, .01, 'departments_no_resp'),
  
  table_reg_function('total_employees_total', 'response', 'Total employees', main_data, 1, 'departments_no_resp'),
  table_reg_function('total_employees_officers', 'response', 'Total officers', main_data, 1, 'departments_no_resp'),
  table_reg_function('total_employees_civilians', 'response', 'Total civilian employees', main_data, 1, 'departments_no_resp')
)



#### create baseline value for missing email departments

contact_baseline =
  data.frame(
    
    contact_baseline = 
      c((mean(department_included$median_hh_income)/1000) %>% round(2),
        (mean(department_included$median_hh_income_black, na.rm = T)/1000) %>% round(2),
        (mean(department_included$median_hh_income_hispanic , na.rm = T)/1000) %>% round(2),
        (mean(department_included$median_hh_income_white , na.rm = T)/1000) %>% round(2),
        (mean(department_included$percent_poverty_status , na.rm = T)/.01) %>% round(2),
        (mean(department_included$percent_poverty_status_black , na.rm = T)/.01) %>% round(2),
        (mean(department_included$percent_poverty_status_hispanic , na.rm = T)/.01) %>% round(2),
        (mean(department_included$percent_poverty_status_white , na.rm = T)/.01) %>% round(2),
        (mean(department_included$population , na.rm = T)/1000) %>% round(2),
        (mean(department_included$percent_black_pop , na.rm = T)/.01) %>% round(2),
        (mean(department_included$percent_hispanic_pop , na.rm = T)/.01) %>% round(2),
        (mean(department_included$percent_white_pop , na.rm = T)/.01) %>% round(2),
        mean(department_included$total_employees_total , na.rm = T) %>% round(2),
        mean(department_included$total_employees_officers , na.rm = T) %>% round(2),
        mean(department_included$total_employees_civilians , na.rm = T) %>% round(2))) 

response_baseline =
  data.frame(
    
    response_baseline = 
      c((mean(department_response$median_hh_income)/1000) %>% round(2),
        (mean(department_response$median_hh_income_black, na.rm = T)/1000) %>% round(2),
        (mean(department_response$median_hh_income_hispanic , na.rm = T)/1000) %>% round(2),
        (mean(department_response$median_hh_income_white , na.rm = T)/1000) %>% round(2),
        (mean(department_response$percent_poverty_status , na.rm = T)/.01) %>% round(2),
        (mean(department_response$percent_poverty_status_black , na.rm = T)/.01) %>% round(2),
        (mean(department_response$percent_poverty_status_hispanic , na.rm = T)/.01) %>% round(2),
        (mean(department_response$percent_poverty_status_white , na.rm = T)/.01) %>% round(2),
        (mean(department_response$population , na.rm = T)/1000) %>% round(2),
        (mean(department_response$percent_black_pop , na.rm = T)/.01) %>% round(2),
        (mean(department_response$percent_hispanic_pop , na.rm = T)/.01) %>% round(2),
        (mean(department_response$percent_white_pop , na.rm = T)/.01) %>% round(2),
        mean(department_response$total_employees_total , na.rm = T) %>% round(2),
        mean(department_response$total_employees_officers , na.rm = T) %>% round(2),
        mean(department_response$total_employees_civilians , na.rm = T) %>% round(2))) 

#### count by census region


census_contacted_df = 
  department_included %>%
  group_by(census_region) %>%
  count() %>%
  as.data.frame()

census_contacted_df %<>%
  mutate(n = ((n/nrow(department_included))/.01) %>% round(2))

##
census_eligible_df = 
  department_eligible %>%
  group_by(census_region) %>%
  count() %>%
  as.data.frame() %>%
  mutate()

census_eligible_df %<>%
  mutate(n = ((n/nrow(department_eligible))/.01) %>% round(2),
         census_region = paste('%', census_region))

##
census_response_df = 
  department_response %>%
  group_by(census_region) %>%
  count() %>%
  as.data.frame() %>%
  mutate()

census_response_df %<>%
  mutate(n = ((n/nrow(department_response))/.01) %>% round(2))

##
census_no_response_df = 
  department_no_response %>%
  group_by(census_region) %>%
  count() %>%
  as.data.frame() %>%
  mutate()


census_no_response_df %<>%
  mutate(n = ((n/nrow(department_no_response))/.01) %>% round(2))

##combine

census_table =
  cbind(census_contacted_df, census_eligible_df[, 2], census_response_df[ , 2], census_no_response_df[, 2])

#difference
census_table %<>%
  mutate(
    diff_contacted = round(.[[2]] - .[[3]], 2),
    dif_responded = round(.[[4]] - .[[5]], 2)
  )

#reorder

census_table %<>%
  select(1, 2, 3, 6, 4:7)

census_table %<>% mutate(census_region = paste('%', census_region))

#### create full table

table = 
  cbind(dept_eligible, contact_baseline, dept_response[2:4], response_baseline)

table %<>%
  mutate(dept_eligible_diff = paste(all_departments_Difference, all_departments_se),
         dept_7500_diff = paste(departments_no_resp_Difference, departments_no_resp_se))

table %<>%
  select(1, 5, 2, 10, 9, 6, 11)

names(table) =
  c('variable',
    'mean contacted',
    'mean eligible',
    'diff eligible',
    'mean response',
    'mean no response',
    'diff no response')

names(census_table) = names(table)

table = rbind(table, census_table)

#### prepare for LaTex

temp_table =
kableExtra::kbl(table,
                col.names = NULL,
                format = 'latex', #turn on for LaTex output
                booktabs = T) %>%
  kable_styling() %>%
  add_header_above(c('',
                     'Mean' = 1,
                     'Mean' = 1,
                     'Difference' = 1,
                     'Mean' = 1,
                     'Mean' = 1,
                     'Difference' =1)) %>%
  add_header_above(c('', 
                     'Contacted' = 1,
                     'Uncontacted' = 2,
                     'Responsive' = 1,
                     'Unresponsive' = 2),
                   italic = T
  ) %>%
  add_header_above(c( '',
                      'Panel A' = 3,
                      'Panel B' = 3), bold = T) %>%
  pack_rows('Income (county-level)', 1, 8, bold = T) %>%
  pack_rows('Population', 9, 12, bold = T) %>%
  pack_rows('Department size (# of employees)', 13, 15, bold = T) %>%
  pack_rows('Census region', 16, 19, bold = T)

#write as file

write(temp_table, file=here('003 Output', 'Tables', "table_2.tex"))

#clean up work space

rm(census_contacted_df,
   census_eligible_df,
   census_no_response_df,
   census_response_df,
   census_table,
   contact_baseline,
   department_eligible,
   department_included,
   department_no_response,
   department_response,
   dept_eligible,
   dept_response,
   response_baseline,
   table,
   temp_all,
   temp_table,
   table_reg_function)

### Table 3 ----

## overall statistics

overall_rr = mean(main_data$response)

race_rr = main_data %>%
  group_by(race_name) %>%
  summarise(mean(response))

gender_rr = main_data %>%
  group_by(gender_name) %>%
  summarise(mean(response))


##1 NO WEIGHTS
model_simple <- feols( response ~ black + hispanic + female| week + state,
                       cluster =  c('week', 'state'), 
                       data = main_data)

summary(model_simple)

confint.default(model_simple)

## 2 WEIGHTS

model_weighted <- feols( response ~ black + hispanic + female| week + state,
                         cluster =  c('week', 'state'), 
                         weights = ~sqrt(population),
                         data = main_data)
summary(model_weighted)


confint.default(model_weighted)



### create table
## estimates, standard errors, p.value for 4 models


etable(list(model_simple,
            model_weighted),
       tex = F)

extra_rows = data.frame( rbind(c('Weights', 'Standard OLS','', 'Sqrt. local pop', ''),
                               c('Reference mean', paste('White:', round(race_rr[3,2],3)), '', '', '' ),
                               c( '', paste('Men:', round(gender_rr[2,2],3)), '', '', ''),
                         c( 'Num.Obs', '2086', '', '2086', '')))



#attr(extra_rows, 'position') = c(5,6,7,8)
options(modelsummary_format_numeric_latex = "plain")

temp_table=
modelsummary(
  list("(1)" = model_simple, '(2)' = model_weighted),
  fmt = getOption("modelsummary_fmt", 4),
  statistic = c("p.value"),  # show p-values
  shape = term ~ model + statistic,
  estimate = "{estimate} \n ({std.error})",
  statistic_map = c( p.value = "P-value"),
  coef_rename = c("black" = "Black", "hispanic" = "Hispanic", 'female' = 'Women'),
  add_rows = extra_rows,
  gof_map = c("nobs"),
  output = "latex_tabular", #turn on for LaTex output
  theme = 'spacing'
)

#write as file

write(temp_table, file=here('003 Output', 'Tables', "table_3.tex"))

#cleanup work space

rm(extra_rows, gender_rr, model_simple, model_weighted, race_rr, overall_rr, temp_table)

### Table 4 ----
#### Overall Statistics

##1 reference mean

mean_responses <- main_data %>%
  group_by(identity) %>%
  summarise( mean(response)) %>%
  clean_names()

## 1 NO WEIGHTS
model_fe_interact <- feols( response ~ black + hispanic + female + black:female + hispanic:female|
                              week + state,
                            cluster =  c('week', 'state'), 
                            data = main_data)


summary(model_fe_interact)


confint.default(model_fe_interact)


## 2 WEIGHTS
model_fe_interact_weighted <- feols( response ~black + hispanic + female + black:female + hispanic:female |
                                       week + state,
                                     weights = ~sqrt(population),
                                     cluster =  c('week', 'state'), 
                                     data = main_data)
summary(model_fe_interact_weighted)

confint.default(model_fe_interact_weighted)


etable( model_fe_interact,  model_fe_interact_weighted)
## create Table

extra_rows = data.frame( rbind(c('Weights', 'Standard OLS','', 'Sqrt. local pop', ''),
                               c('Reference mean', paste('White men', round(mean_responses[6,2],3)), '', '', ''),
                               c('Num.Obs.', '2086', '', '2086', '')))



#attr(extra_rows, 'position') = c(6, 7)
options(modelsummary_format_numeric_latex = "plain")

temp_table = 
modelsummary(
  list("(1)" = model_fe_interact, '(2)' = model_fe_interact_weighted),
  fmt = getOption("modelsummary_fmt", 4),
  statistic = c("p.value"),  # show p-values
  shape = term ~ model + statistic,
  estimate = "{estimate} \n ({std.error})",
  statistic_map = c( p.value = "P-value"),
  coef_rename = c('black:female' = 'Black women',
                  'black' = 'Black',
                  'hispanic:female' = 'Hispanic women',
                  "hispanic" = "Hispanic",
                  "female" = "Women"),
  add_rows = extra_rows,
  gof_map = c("nobs"),
  output = "latex_tabular", #turn on for LaTex
  theme = 'spacing'
) 

#write file
write(temp_table, file=here('003 Output', 'Tables', "table_4.tex"))

#clean up workspace 

rm(extra_rows,mean_responses, model_fe_interact, model_fe_interact_weighted, temp_table)

### Table 5 ----
## create two additional data sets:
## (1) sample removing departments with online contact forms
## (2) sample with only departments with online contact forms

no_form_data = 
  main_data %>%
  filter(has_contact_form_on_website == 'No')

yes_form_data = 
  main_data %>%
  filter(has_contact_form_on_website == 'Yes')

## full sample race pooled
mean_responses_all_race <- main_data %>%
  group_by(race_name) %>%
  summarise( mean(response)) %>%
  clean_names()

model_all_race <- feols( response ~ black + hispanic| week + state,
                         cluster =  c('week', 'state'), 
                         data = main_data)

summary(model_all_race)

linearHypothesis(model_all_race, 
                 c('black = hispanic'))

## no form sample race pooled
mean_responses_nf_race <- no_form_data %>%
  group_by(race_name) %>%
  summarise( mean(response)) %>%
  clean_names()

model_nf_race <- feols( response ~ black + hispanic| week + state,
                        cluster =  c('week', 'state'), 
                        data = no_form_data)

summary(model_nf_race)

linearHypothesis(model_nf_race, 
                 c('black = hispanic'))

## yes form sample race pooled
mean_responses_yf_race <- yes_form_data %>%
  group_by(race_name) %>%
  summarise( mean(response)) %>%
  clean_names()

model_yf_race <- feols( response ~ black + hispanic| week + state,
                        cluster =  c('week', 'state'), 
                        data = yes_form_data)

summary(model_yf_race)

linearHypothesis(model_yf_race, 
                 c('black = hispanic'))



#Make table

etable(model_all_race, model_nf_race, model_yf_race)

extra_rows = data.frame( rbind(
  c('Online portal use', 'All depts','', 'No portal', '', 'Portal', ''),
  c('Reference mean',
    paste('White:', round(mean_responses_all_race[3,2], 3)), '',
    paste('White:', round(mean_responses_nf_race[3,2], 3)), '',
    paste('White:', round(mean_responses_yf_race[3,2], 3)), ''),
  c('Overall response rate',
    round(mean(main_data$response), 3),
    '',
    round(mean(no_form_data$response), 3),
    '',
    round(mean(yes_form_data$response), 3),
    ''
  ),
  c('Num.Obs.', '2,086', '', '1,816', '', '270', '')
)
)

options(modelsummary_format_numeric_latex = "plain")

#attr(extra_rows, 'position') = c(3, 3, 3)

temp_table = 
modelsummary(
  list("(1)" = model_all_race, '(2)' = model_nf_race, "(3)" = model_yf_race),
  statistic = c("p.value"),  # show p-values
  shape = term ~ model + statistic,
  estimate = "{estimate} \n ({std.error})",
  statistic_map = c( p.value = "P-value"),
  coef_rename = c("black" = "Black", "hispanic" = "Hispanic"),
  add_rows = extra_rows,
  gof_map = c("nobs"),
  output = "latex_tabular",
  theme = 'spacing'
) 

#write file
write(temp_table, file=here('003 Output', 'Tables', "table_5.tex"))


#clean up workspace
rm(extra_rows, mean_responses_all_race, mean_responses_nf_race, mean_responses_yf_race,
   model_all_race, model_nf_race, model_yf_race,
   no_form_data, yes_form_data, temp_table)

### Table 6 ----

# add additional 'non-white' population variable
main_data %<>% mutate( percent_poc_pop = 1 - percent_white_pop)

# identity race interacted with racial composition

model_percent_b_h<- feols( response ~ black + black*percent_black_pop + hispanic + hispanic*percent_hispanic_pop| 
                             week + state ,
                           cluster =  c('week', 'state'), 
                           data = main_data)
summary(model_percent_b_h)

# identity race interacted with racial composition and income controls

model_percent_b_h_i<- feols( response ~ black + black*percent_black_pop + hispanic + hispanic*percent_hispanic_pop| 
                               week + state + median_hh_income,
                             cluster =  c('week', 'state'), 
                             data = main_data)
summary(model_percent_b_h_i)

# match black identity with hispanic pop and hispanic identity with black pop 

model_percent_hb_bh<- feols( response ~ black + black*percent_hispanic_pop + hispanic + hispanic*percent_black_pop| 
                               week + state,
                             cluster =  c('week', 'state'), 
                             data = main_data)
summary(model_percent_hb_bh)

# match black identity with hispanic pop and hispanic identity with black pop with income controls

model_percent_hb_bh_i<- feols( response ~ black + black*percent_hispanic_pop + hispanic + hispanic*percent_black_pop| 
                                 week + state + median_hh_income,
                               cluster =  c('week', 'state'), 
                               data = main_data)
summary(model_percent_hb_bh_i)

# use POC (non-white) instead of hispanic and black

model_percent_poc <- feols( response ~ black + black*percent_poc_pop + hispanic + hispanic*percent_poc_pop| 
                              week + state,
                            cluster =  c('week', 'state'), 
                            data = main_data)
summary(model_percent_poc)

#6 use POC (non-white) instead of hispanic and black with income controls 

model_percent_poc_i <- feols( response ~ black + black*percent_poc_pop + hispanic + hispanic*percent_poc_pop| 
                                week + state + median_hh_income,
                              cluster =  c('week', 'state'), 
                              data = main_data)

etable(model_percent_b_h, model_percent_b_h_i, model_percent_hb_bh, model_percent_hb_bh_i, model_percent_poc, model_percent_poc_i)

## make table

extra_rows = data.frame( rbind(
  c('Income controls', 'No','', 'Yes', '', 'No', '', 'Yes', '', 'No', '', 'Yes', ''),
  c( 'Num.Obs', '2086', '', '2086', '','2086', '', '2086', '','2086', '', '2086', '' )
))

options(modelsummary_format_numeric_latex = "plain")

attr(extra_rows, 'position') = c(12)

temp_table =
modelsummary(
  list("(1)" = model_percent_b_h,
       '(2)' = model_percent_b_h_i,
       "(3)" = model_percent_hb_bh,
       '(4)' = model_percent_hb_bh_i,
       '(5)' = model_percent_poc,
       '(6)' = model_percent_poc_i),
  statistic = c("p.value"),  # show p-values
  shape = term ~ model + statistic,
  estimate = "{estimate} \n ({std.error})",
  statistic_map = c( p.value = "P-value"),
  coef_rename = c("black" = "Black",
                  "hispanic" = "Hispanic",
                  'percent_black_pop' = 'Pop% Black',
                  'percent_hispanic_pop' = 'Pop% Hispanic',
                  'percent_poc_pop' = 'Pop% POC'),
  add_rows = extra_rows,
  gof_map = c("nobs"),
  output = "latex_tabular",
  theme = 'spacing'
) 

#write file
write(temp_table, file=here('003 Output', 'Tables', "table_6.tex"))

#clean up workspace

rm(extra_rows, 
   model_percent_b_h, model_percent_b_h_i, model_percent_hb_bh,
   model_percent_hb_bh_i, model_percent_poc, model_percent_poc_i,
   temp_table)
### Table 7 ----

##chief race 

model_chf_bh_all = feols(response ~ black*pers_chf_race_bk + hispanic*pers_chf_hisp_or|
                     week + state,
                   main_data)

model_chf_bh_2020 = feols(response ~ black*pers_chf_race_bk + hispanic*pers_chf_hisp_or|
                      week + state,
                    filter(main_data, lemas_year == '2020'))


model_chf_white_all = feols(response ~ black*pers_chf_race_wht + hispanic*pers_chf_race_wht|
                        week + state,
                      main_data)

model_chf_white_2020 = feols(response ~ black*pers_chf_race_wht + hispanic*pers_chf_race_wht|
                         week + state,
                       filter(main_data, lemas_year == '2020'))

etable(model_chf_bh_all, model_chf_bh_2020, model_chf_white_all, model_chf_white_2020)

#make table

extra_rows = data.frame( rbind(
  c('LEMAS survey', 'Both','', '2020', '','Both','', '2020', ''),
  c('Num.Obs.', '724', '', '491', '', '727', '', '494', '')))

options(modelsummary_format_numeric_latex = "plain")

#attr(extra_rows, 'position') = c(10)

temp_table = 
modelsummary(
  list("(1)" = model_chf_bh_all, '(2)' = model_chf_bh_2020,
       "(3)" = model_chf_white_all, '(4)' = model_chf_white_2020),
  statistic = c("p.value"),  # show p-values
  shape = term ~ model + statistic,
  estimate = "{estimate} \n ({std.error})",
  statistic_map = c( p.value = "P-value"),
  coef_rename = c("black" = "Black",
                  "hispanic" = "Hispanic",
                  'pers_chf_race_bk' = 'Chief Black',
                  'pers_chf_hisp_or' = 'Chief Hispanic',
                  'pers_chf_race_wht' = 'Chief White'),
  add_rows = extra_rows,
  gof_map = c("nobs"),
  output = "latex_tabular",
  theme = 'spacing'
) 

#write file
write(temp_table, file=here('003 Output', 'Tables', "table_7.tex"))

#clean up workspace

rm(extra_rows, temp_table,
   model_chf_bh_2020, model_chf_bh_all, model_chf_white_2020, model_chf_white_all)
### Table 8 ----
#### Need to modify data
# we will create bins for total employees and employees per capita served

agency_size = main_data %>%
 filter(is.na(total_employees_total) == F & total_employees_total != 0)

# indicator for big or small (absolute terms)
dept_size_median = 
  median(agency_size$total_employees_total)

agency_size %<>% 
  mutate(abs_dept_size = as.factor(if_else(total_employees_total < dept_size_median, 0, 1)))

# indicator for big small (ratio)

agency_size %<>% 
  mutate( per_capita_total_law_enforcement = total_employees_total / population,
          per_capita_law_enforcement = total_employees_officers / population,
          officer_to_civilian_ratio = if_else(total_employees_civilians > 0,
                                              (total_employees_officers)/(total_employees_civilians),
                                              0))
# indicator for big or small (per capita)
dept_per_size_median = median(agency_size$per_capita_total_law_enforcement)

agency_size %<>% 
  mutate(per_dept_size = 
           as.factor(if_else(per_capita_total_law_enforcement < dept_per_size_median,
                             0,
                             1)))


#### run analysis and make table

#abs_dept_size = 1/0 around the median (41 employees)
model_abs_dept <- feols( response ~ black + hispanic + abs_dept_size + black:abs_dept_size + hispanic:abs_dept_size|
                           week + state,
                         cluster =  c('week', 'state'), 
                         data = agency_size)



#per_dept_size = 1/0 around the median of employees/population ( 22:10,000)
model_per_dept <- feols( response ~ black + hispanic + per_dept_size + black:per_dept_size + hispanic:per_dept_size|
                           week + state,
                         cluster =  c('week', 'state'), 
                         data = agency_size)

etable(model_abs_dept, model_per_dept)

### make table

extra_rows = data.frame(rbind(
  c('Num.Obs.', '2060','', '2060', '')))

options(modelsummary_format_numeric_latex = "plain")

temp_table = 
modelsummary(
  list("(1)" = model_abs_dept, '(2)' = model_per_dept),
  statistic = c("p.value"),  # show p-values
  shape = term ~ model + statistic,
  estimate = "{estimate} \n ({std.error})",
  statistic_map = c( p.value = "P-value"),
  coef_rename = c("black" = "Black",
                  "hispanic" = "Hispanic",
                  'abs_dept_size1' = 'Dept. size',
                  'per_dept_size1' = 'Dept. size per'),
  add_rows = extra_rows,
  gof_map = c("nobs"),
  output = "latex_tabular",
  theme = 'spacing'
) 

#write file
write(temp_table, file=here('003 Output', 'Tables', "table_8.tex"))

#clean up workspace

rm(extra_rows, agency_size,
   model_abs_dept, model_per_dept,
   dept_size_median, dept_per_size_median,
   temp_table)
### Table 9 ----
#any complaint in 2020
model_comp_any = feols(response ~ black + hispanic + complaint_any + black*complaint_any + hispanic*complaint_any|
                   week + state,
                 main_data)
#the sustained complained rate by department (sustained/(total-pending))
model_comp_sust_rate = feols(response ~ black + hispanic + complaint_sust_rate + black*complaint_sust_rate + hispanic*complaint_sust_rate |
                         week + state,
                       main_data)

#the number of complaints per officer (by agency)
model_comp_per_officer = feols(response ~ black + hispanic + complaint_per_officer + black*complaint_per_officer + hispanic*complaint_per_officer |
                           week + state,
                         main_data)


etable(model_comp_any, model_comp_sust_rate, model_comp_per_officer)

#make table

extra_rows = data.frame( rbind(
  c('LEMAS survey', '2020','', '2020', '', '2020', ''),
  c('Num.Obs.', '499', '', '435', '', '499', '')
))

options(modelsummary_format_numeric_latex = "plain")

#attr(extra_rows, 'position') = c(12)

temp_table = 
modelsummary(
  list("(1)" = model_comp_any, '(2)' = model_comp_sust_rate, "(3)" = model_comp_per_officer),
  statistic = c("p.value"),  # show p-values
  shape = term ~ model + statistic,
  estimate = "{estimate} \n ({std.error})",
  statistic_map = c( p.value = "P-value"),
  coef_rename = c("black" = "Black",
                  "hispanic" = "Hispanic"#,
                  #'complaint_any' = 'compl. ind.',
                  #'complaint_sust_rate ' = '',
                  #'complaint_per_officer ' = ''
  ),
  add_rows = extra_rows,
  gof_map = c("nobs"),
  output = "latex_tabular",
  theme = 'spacing'
) 

#write file
write(temp_table, file=here('003 Output', 'Tables', "table_9.tex"))

#clean up workspace

rm(extra_rows,
   model_comp_any,
   model_comp_per_officer,
   model_comp_sust_rate,
   temp_table)

### Table 10 ----

model_cbu = feols(response ~ black + hispanic + collective_bargaining_unit + black*collective_bargaining_unit + hispanic*collective_bargaining_unit|
              week + state,
            main_data)

etable(model_cbu)

#make table

extra_rows = data.frame(rbind(c('Num.Obs',
                        '568',
                        ''
                        )))

options(modelsummary_format_numeric_latex = "plain")

#attr(extra_rows, 'position') = c(12)

temp_table = 
modelsummary(
  list(model_cbu),
  statistic = c("p.value"),  # show p-values
  shape = term ~ model + statistic,
  estimate = "{estimate} \n ({std.error})",
  statistic_map = c( p.value = "P-value"),
  coef_rename = c("black" = "Black",
                  "hispanic" = "Hispanic",
                  'collective_bargaining_unit' = 'CBU'
  ),
  add_rows = extra_rows,
  gof_map = c("nobs"),
  output = "latex_tabular",
  theme = 'spacing'
) 
#write file
write(temp_table, file=here('003 Output', 'Tables', "table_10.tex"))

#cleanup

rm(extra_rows, model_cbu, temp_table)

### Appendix Table A.1 ----

####modify data for table
#create "contacted variable." 1 if was sent an email and 0 if not

distinct_departments %<>%
  mutate(contacted = if_else(has_email_address == 1, 1, 0),#takes on 0 if no email address or not selected for study
         department = 1) 

#create data subsets for analysis and table 
department_eligible = distinct_departments %>% filter(population >= 7500) #pop needs to be >7500 to be eligible 

department_included = distinct_departments %>% filter(contacted == 1) #several departments with populations <7,500 were incidentally included


####regression table function
table_reg_function =
  function(var1, var2, name, data, scale, sample){
    
    # Build the formula from string
    fml <- as.formula(paste(var1, "~ ", var2))
    
    # Run regression
    model_temp <- feols(fml, data = data)
    
    temp = model_temp$coeftable[, 1:2] %>%
      mutate(label = c('mean', 'difference'),
             variable = name) %>%
      clean_names()
    
    temp %<>% pivot_wider(names_from = 'label', values_from = c('estimate', 'std_error')) %>%
      select( c(1:3, 5))
    
    temp %<>% mutate(across(2:4, ~round(.x/scale, 2)))
    
    temp[ , 4] = paste0('(', temp[,4], ')')
    
    names(temp) = c(paste(sample, 'Variable', sep = '_'),
                    paste(sample, 'Mean', sep = '_'),
                    paste(sample, 'Difference', sep = '_'),
                    paste(sample, 'se', sep = '_'))
    
    temp
  }


#### run regression function for all departments, departments with pop > 7,500, and searched departments
dept_eligible = rbind(
  table_reg_function('median_hh_income', 'contacted', 'Median income all HH ($1,000s)', department_eligible, 1000, 'eligible_departments'),
  table_reg_function('median_hh_income_black', 'contacted','Median income Black HH ($1,000s)', department_eligible, 1000, 'eligible_departments'),
  table_reg_function('median_hh_income_hispanic', 'contacted', 'Median income Hispanic HH ($1,000s)', department_eligible, 1000, 'eligible_departments'),
  table_reg_function('median_hh_income_white', 'contacted', 'Median income White HH ($1,000s)', department_eligible, 1000, 'eligible_departments'),
  
  table_reg_function('percent_poverty_status', 'contacted', '% Pop. in poverty', department_eligible, .01, 'eligible_departments'),
  table_reg_function('percent_poverty_status_black', 'contacted', '% Black pop. in poverty', department_eligible, .01, 'eligible_departments'),
  table_reg_function('percent_poverty_status_hispanic','contacted', '% Hispanic pop. in poverty', department_eligible, .01, 'eligible_departments'),
  table_reg_function('percent_poverty_status_white','contacted', '% White pop. in poverty', department_eligible, .01, 'eligible_departments'),
  
  table_reg_function('population','contacted', 'Local population (1000s)', department_eligible, 1000, 'eligible_departments'),
  table_reg_function('percent_black_pop','contacted', 'Pop. % Black (county-level)', department_eligible, .01, 'eligible_departments'),
  table_reg_function('percent_hispanic_pop','contacted', 'Pop. % Hispanic (county-level)', department_eligible, .01, 'eligible_departments'),
  table_reg_function('percent_white_pop','contacted', 'Pop. % White (county-level)', department_eligible, .01, 'eligible_departments'),
  
  table_reg_function('total_employees_total','contacted', 'Total employees', department_eligible, 1, 'eligible_departments'),
  table_reg_function('total_employees_officers','contacted', 'Total officers', department_eligible, 1, 'eligible_departments'),
  table_reg_function('total_employees_civilians','contacted', 'Total civilian employees', department_eligible, 1, 'eligible_departments')
)

dept_all = rbind(
  table_reg_function('median_hh_income', 'contacted', 'Median income all HH ($1,000s)', distinct_departments, 1000, 'all_departments'),
  table_reg_function('median_hh_income_black', 'contacted','Median income Black HH ($1,000s)', distinct_departments, 1000, 'all_departments'),
  table_reg_function('median_hh_income_hispanic', 'contacted', 'Median income Hispanic HH ($1,000s)', distinct_departments, 1000, 'all_departments'),
  table_reg_function('median_hh_income_white', 'contacted', 'Median income White HH ($1,000s)', distinct_departments, 1000, 'all_departments'),
  
  table_reg_function('percent_poverty_status', 'contacted', '% Pop. in poverty', distinct_departments, .01, 'all_departments'),
  table_reg_function('percent_poverty_status_black', 'contacted', '% Black pop. in poverty', distinct_departments, .01, 'all_departments'),
  table_reg_function('percent_poverty_status_hispanic','contacted', '% Hispanic pop. in poverty', distinct_departments, .01, 'all_departments'),
  table_reg_function('percent_poverty_status_white','contacted', '% White pop. in poverty', distinct_departments, .01, 'all_departments'),
  
  table_reg_function('population','contacted', 'Local population (1000s)', distinct_departments, 1000, 'all_departments'),
  table_reg_function('percent_black_pop','contacted', 'Pop. % Black (county-level)', distinct_departments, .01, 'all_departments'),
  table_reg_function('percent_hispanic_pop','contacted', 'Pop. % Hispanic (county-level)', distinct_departments, .01, 'all_departments'),
  table_reg_function('percent_white_pop','contacted', 'Pop. % White (county-level)', distinct_departments, .01, 'all_departments'),
  
  table_reg_function('total_employees_total','contacted', 'Total employees', distinct_departments, 1, 'all_departments'),
  table_reg_function('total_employees_officers','contacted', 'Total officers', distinct_departments, 1, 'all_departments'),
  table_reg_function('total_employees_civilians','contacted', 'Total civilian employees', distinct_departments, 1, 'all_departments')
)


#### create baseline value for missing email departments

contact_baseline =
  data.frame(
    
    contact_baseline = 
      c((mean(department_included$median_hh_income)/1000) %>% round(2),
        (mean(department_included$median_hh_income_black, na.rm = T)/1000) %>% round(2),
        (mean(department_included$median_hh_income_hispanic , na.rm = T)/1000) %>% round(2),
        (mean(department_included$median_hh_income_white , na.rm = T)/1000) %>% round(2),
        (mean(department_included$percent_poverty_status , na.rm = T)/.01) %>% round(2),
        (mean(department_included$percent_poverty_status_black , na.rm = T)/.01) %>% round(2),
        (mean(department_included$percent_poverty_status_hispanic , na.rm = T)/.01) %>% round(2),
        (mean(department_included$percent_poverty_status_white , na.rm = T)/.01) %>% round(2),
        (mean(department_included$population , na.rm = T)/1000) %>% round(2),
        (mean(department_included$percent_black_pop , na.rm = T)/.01) %>% round(2),
        (mean(department_included$percent_hispanic_pop , na.rm = T)/.01) %>% round(2),
        (mean(department_included$percent_white_pop , na.rm = T)/.01) %>% round(2),
        mean(department_included$total_employees_total , na.rm = T) %>% round(2),
        mean(department_included$total_employees_officers , na.rm = T) %>% round(2),
        mean(department_included$total_employees_civilians , na.rm = T) %>% round(2))) 



#### create full table

table = 
  cbind(dept_eligible, contact_baseline, dept_all[2:4])

table %<>%
  mutate(dept_eligible_diff = paste(eligible_departments_Difference, eligible_departments_se),
         dept_all_diff = paste(all_departments_Difference, all_departments_se))

table %<>%
  select(1, 5, 2, 9, 6, 10)

names(table) =
  c('variable',
    'mean contacted',
    'mean eligible',
    'diff eligible',
    'mean all departments',
    'diff all departments')



#### prepare for LaTex

temp_table=
kableExtra::kbl(table,
                col.names = NULL,
                format = 'latex', #turn on for LaTex output
                booktabs = T) %>%
  kable_styling() %>%
  add_header_above(c('',
                     'Mean' = 1,
                     'Mean' = 1,
                     'Difference' = 1,
                     'Mean' = 1,
                     'Difference' =1)) %>%
  add_header_above(c('', 
                     'Contacted' = 1,
                     'Eligible' = 2,
                     'All' = 2),
                   italic = T
  ) %>%
  #add_header_above(c( '',
  #                    'Panel A' = 3,
  #                    'Panel B' = 3), bold = T) %>%
  pack_rows('Income (county-level)', 1, 8, bold = T) %>%
  pack_rows('Population', 9, 12, bold = T) %>%
  pack_rows('Department size (# of employees)', 13, 15, bold = T) 

#write file
write(temp_table, file=here('003 Output', 'Tables', "table_A1.tex"))


#clean up work space
rm(contact_baseline,
   department_eligible,
   department_included,
   department_all,
   dept_eligible,
   dept_all,
   table,
   table_reg_function,
   temp_table)

### Appendix Table A.2 ----
#add variable indicating whether a department has an email address (vs. no email found or not selected for study)

department_data =
  distinct_departments |>
  mutate(no_email = if_else(has_email_address == 0, 1, 0))

#### create subsets of data
department_7500_data = department_data %>% filter(population >= 7500)

searched_department_data = department_data %>% filter(searched == 1)

missing_department_data = searched_department_data %>% filter(has_email_address == 0)

#### regression table function
no_email_function =
  function(var, name, data, scale, sample){
    
    # Build the formula from string
    fml <- as.formula(paste(var, "~ no_email"))
    
    # Run regression
    model_temp <- feols(fml, data = data)
    
    temp = model_temp$coeftable[, 1:2] %>%
      mutate(label = c('mean', 'difference'),
             variable = name) %>%
      clean_names()
    
    temp %<>% pivot_wider(names_from = 'label', values_from = c('estimate', 'std_error')) %>%
      select( c(1:3, 5))
    
    temp %<>% mutate(across(2:4, ~round(.x/scale, 2)))
    
    temp[ , 4] = paste0('(', temp[,4], ')')
    
    names(temp) = c(paste(sample, 'Variable', sep = '_'),
                    paste(sample, 'Mean', sep = '_'),
                    paste(sample, 'Difference', sep = '_'),
                    paste(sample, 'se', sep = '_'))
    
    temp
  }


#### run regression function for all departments, departments with pop > 7,500, and searched departments 
dept_all = rbind(
  no_email_function('median_hh_income', 'Median income all HH ($1,000s)', department_data, 1000, 'all_departments'),
  no_email_function('median_hh_income_black', 'Median income Black HH ($1,000s)', department_data, 1000, 'all_departments'),
  no_email_function('median_hh_income_hispanic', 'Median income Hispanic HH ($1,000s)', department_data, 1000, 'all_departments'),
  no_email_function('median_hh_income_white', 'Median income White HH ($1,000s)', department_data, 1000, 'all_departments'),
  
  no_email_function('percent_poverty_status', '% Pop. in poverty', department_data, .01, 'all_departments'),
  no_email_function('percent_poverty_status_black', '% Black pop. in poverty', department_data, .01, 'all_departments'),
  no_email_function('percent_poverty_status_hispanic', '% Hispanic pop. in poverty', department_data, .01, 'all_departments'),
  no_email_function('percent_poverty_status_white', '% White pop. in poverty', department_data, .01, 'all_departments'),
  
  no_email_function('population', 'Local population (1000s)', department_data, 1000, 'all_departments'),
  no_email_function('percent_black_pop', 'Pop. % Black (county-level)', department_data, .01, 'all_departments'),
  no_email_function('percent_hispanic_pop', 'Pop. % Hispanic (county-level)', department_data, .01, 'all_departments'),
  no_email_function('percent_white_pop', 'Pop. % White (county-level)', department_data, .01, 'all_departments'),
  
  no_email_function('total_employees_total', 'Total employees', department_data, 1, 'all_departments'),
  no_email_function('total_employees_officers', 'Total officers', department_data, 1, 'all_departments'),
  no_email_function('total_employees_civilians', 'Total civilian employees', department_data, 1, 'all_departments')
)

dept_7500 = rbind(
  no_email_function('median_hh_income', 'Median income all HH ($1,000s)', department_7500_data, 1000, 'departments_7500'),
  no_email_function('median_hh_income_black', 'Median income Black HH ($1,000s)', department_7500_data, 1000, 'departments_7500'),
  no_email_function('median_hh_income_hispanic', 'Median income Hispanic HH ($1,000s)', department_7500_data, 1000, 'departments_7500'),
  no_email_function('median_hh_income_white', 'Median income White HH ($1,000s)', department_7500_data, 1000, 'departments_7500'),
  
  no_email_function('percent_poverty_status', '% Pop. in poverty', department_7500_data, .01, 'departments_7500'),
  no_email_function('percent_poverty_status_black', '% Black pop. in poverty', department_7500_data, .01, 'departments_7500'),
  no_email_function('percent_poverty_status_hispanic', '% Hispanic pop. in poverty', department_7500_data, .01, 'departments_7500'),
  no_email_function('percent_poverty_status_white', '% White pop. in poverty', department_7500_data, .01, 'departments_7500'),
  
  no_email_function('population', 'Local population (1000s)', department_7500_data, 1000, 'departments_7500'),
  no_email_function('percent_black_pop', 'Pop. % Black (county-level)', department_7500_data, .01, 'departments_7500'),
  no_email_function('percent_hispanic_pop', 'Pop. % Hispanic (county-level)', department_7500_data, .01, 'departments_7500'),
  no_email_function('percent_white_pop', 'Pop. % White (county-level)', department_7500_data, .01, 'departments_7500'),
  
  no_email_function('total_employees_total', 'Total employees', department_7500_data, 1, 'departments_7500'),
  no_email_function('total_employees_officers', 'Total officers', department_7500_data, 1, 'departments_7500'),
  no_email_function('total_employees_civilians', 'Total civilian employees', department_7500_data, 1, 'departments_7500')
)

dept_searched = rbind(
  no_email_function('median_hh_income', 'Median income all HH ($1,000s)', searched_department_data, 1000, 'searched_dept'),
  no_email_function('median_hh_income_black', 'Median income Black HH ($1,000s)', searched_department_data, 1000, 'searched_dept'),
  no_email_function('median_hh_income_hispanic', 'Median income Hispanic HH ($1,000s)', searched_department_data, 1000, 'searched_dept'),
  no_email_function('median_hh_income_white', 'Median income White HH ($1,000s)', searched_department_data, 1000, 'searched_dept'),
  
  no_email_function('percent_poverty_status', '% Pop. in poverty', searched_department_data, .01, 'searched_dept'),
  no_email_function('percent_poverty_status_black', '% Black pop. in poverty', searched_department_data, .01, 'searched_dept'),
  no_email_function('percent_poverty_status_hispanic', '% Hispanic pop. in poverty', searched_department_data, .01, 'searched_dept'),
  no_email_function('percent_poverty_status_white', '% White pop. in poverty', searched_department_data, .01, 'searched_dept'),
  
  no_email_function('population', 'Local population (1000s)', searched_department_data, 1000, 'searched_dept'),
  no_email_function('percent_black_pop', 'Pop. % Black (county-level)', searched_department_data, .01, 'searched_dept'),
  no_email_function('percent_hispanic_pop', 'Pop. % Hispanic (county-level)', searched_department_data, .01, 'searched_dept'),
  no_email_function('percent_white_pop', 'Pop. % White (county-level)', searched_department_data, .01, 'searched_dept'),
  
  no_email_function('total_employees_total', 'Total employees', searched_department_data, 1, 'searched_dept'),
  no_email_function('total_employees_officers', 'Total officers', searched_department_data, 1, 'searched_dept'),
  no_email_function('total_employees_civilians', 'Total civilian employees', searched_department_data, 1, 'searched_dept')
)

#### create baseline value for missing email departments

baseline =
  data.frame(
    
    baseline = 
      c((mean(missing_department_data$median_hh_income)/1000) %>% round(2),
        (mean(missing_department_data$median_hh_income_black, na.rm = T)/1000) %>% round(2),
        (mean(missing_department_data$median_hh_income_hispanic , na.rm = T)/1000) %>% round(2),
        (mean(missing_department_data$median_hh_income_white , na.rm = T)/1000) %>% round(2),
        (mean(missing_department_data$percent_poverty_status , na.rm = T)/.01) %>% round(2),
        (mean(missing_department_data$percent_poverty_status_black , na.rm = T)/.01) %>% round(2),
        (mean(missing_department_data$percent_poverty_status_hispanic , na.rm = T)/.01) %>% round(2),
        (mean(missing_department_data$percent_poverty_status_white , na.rm = T)/.01) %>% round(2),
        (mean(missing_department_data$population , na.rm = T)/1000) %>% round(2),
        (mean(missing_department_data$percent_black_pop , na.rm = T)/.01) %>% round(2),
        (mean(missing_department_data$percent_hispanic_pop , na.rm = T)/.01) %>% round(2),
        (mean(missing_department_data$percent_white_pop , na.rm = T)/.01) %>% round(2),
        mean(missing_department_data$total_employees_total , na.rm = T) %>% round(2),
        mean(missing_department_data$total_employees_officers , na.rm = T) %>% round(2),
        mean(missing_department_data$total_employees_civilians , na.rm = T) %>% round(2)))      
#### count by census region


census_missing_df = 
  missing_department_data %>%
  group_by(census_region) %>%
  count() %>%
  as.data.frame()

census_missing_df %<>%
  mutate(n = ((n/nrow(missing_department_data))/.01) %>% round(2))

##
census_all_df = 
  department_data %>%
  group_by(census_region) %>%
  count() %>%
  as.data.frame() %>%
  mutate()

census_all_df %<>%
  mutate(n = ((n/nrow(department_data))/.01) %>% round(2),
         census_region = paste('%', census_region))

##
census_7500_df = 
  department_7500_data %>%
  group_by(census_region) %>%
  count() %>%
  as.data.frame() %>%
  mutate()

census_7500_df %<>%
  mutate(n = ((n/nrow(department_7500_data))/.01) %>% round(2))

##
census_srchd_df = 
  searched_department_data %>%
  group_by(census_region) %>%
  count() %>%
  as.data.frame() %>%
  mutate()


census_srchd_df %<>%
  mutate(n = ((n/nrow(searched_department_data))/.01) %>% round(2))

##combine

census_table =
  cbind(census_missing_df, census_all_df[, 2], census_7500_df[ , 2], census_srchd_df[, 2])

#difference
census_table %<>%
  mutate(
    diff_all = round(.[[2]] - .[[3]], 2),
    dif_7500 = round(.[[2]] - .[[4]], 2),
    diff_srchd = round(.[[2]] - .[[5]], 2)
  )

#reorder

census_table %<>%
  select(1, 2, 3, 6, 4, 7, 5, 8)

census_table %<>% mutate(census_region = paste('%', census_region))

#### create full table

table = 
  cbind(dept_all, dept_7500[,2:4], dept_searched[2:4], baseline)

table %<>%
  mutate(dept_all_diff = paste(all_departments_Difference, all_departments_se),
         dept_7500_diff = paste(departments_7500_Difference, departments_7500_se),
         dept_searched_diff = paste(searched_dept_Difference, searched_dept_se))

table %<>%
  select(1, 11, 2, 12, 5, 13, 8, 14)

names(table) =
  c('variable',
    'mean missing',
    'mean all',
    'diff all',
    'mean 7500',
    'diff 7500',
    'mean searched',
    'diff searched')

names(census_table) = names(table)

table = rbind(table, census_table)

#prepare for LaTex

temp_table =
kableExtra::kbl(table,
                col.names = NULL,
                format = 'latex',
                booktabs = T) %>%
  kable_styling() %>%
  add_header_above(c('',
                     'Mean' = 1,
                     'Mean' = 1,
                     'Difference' = 1,
                     'Mean' = 1,
                     'Difference' = 1,
                     'Mean' = 1,
                     'Difference' =1)) %>%
  add_header_above(c('', 
                     'Missing Email Departments' = 1,
                     'All Departments' = 2,
                     'Departments > 7,500' = 2,
                     'Searched Departmens' = 2),
                   italic = T
  ) %>%
  pack_rows('Income (county-level)', 1, 8, bold = T) %>%
  pack_rows('Population', 9, 12, bold = T) %>%
  pack_rows('Department size (# of employees)', 13, 15, bold = T) %>%
  pack_rows('Census region', 16, 19, bold = T)

#write file
write(temp_table, file=here('003 Output', 'Tables', "table_A2.tex"))

#clean up workspace 
rm(baseline, census_7500_df, census_all_df, census_missing_df, census_srchd_df, census_table,
   department_7500_data, department_data, dept_7500, dept_all, dept_searched,
   missing_department_data, searched_department_data, table, no_email_function, temp_table)


### Appendix Table A.5 ----
#modify data to include all departments contacted but only one entry per department 

contacted_departments_data = 
  all_departments |>
  filter(has_email_address == 1 & response <= 1)

####regression table function
table_reg_function =
  function(var1, name, data, scale){
    
    # Build the formula from string
    fml <- as.formula(paste(var1, "~ white:female + hispanic:female + hispanic:male + black:female + black:male"))
    
    # Run regression
    model_temp <- feols(fml, data = contacted_departments_data)
    
    temp = model_temp$coeftable[, 1:2] %>%
      mutate(label = c('mean', 'wf_difference', 'hf_difference', 'hm_difference', 'bf_difference', 'bm_difference'),
             variable = name) |>clean_names()
    
    temp %<>% pivot_wider(names_from = 'label', values_from = c('estimate', 'std_error')) %>%
      select( c(1:3, 9, 4, 10, 5, 11, 6, 12, 7, 13))
    
    temp %<>% mutate(across(2:12, ~round(.x/scale, 2)))
    
    
    temp[ , 4] = paste0('(', temp[,4], ')')
    temp[ , 6] = paste0('(', temp[,6], ')')
    temp[ , 8] = paste0('(', temp[,8], ')')
    temp[ , 10] = paste0('(', temp[,10], ')')
    temp[ , 12] = paste0('(', temp[,12], ')')
    
    
    names(temp) = c('variable',
                    'mean',
                    'wf_e',
                    'wf_sd',
                    'hf_e',
                    'hf_sd',
                    'hm_e',
                    'hm_sd',
                    'bf_e',
                    'bf_sd',
                    'bm_e',
                    'bm_sd')
    
    temp
    
  }


#### run regression function for all departments, departments with pop > 7,500, and searched departments
table = rbind(
  table_reg_function('median_hh_income',  'Median income all HH ($1,000s)', contacted_departments_data, 1000 ),
  table_reg_function('median_hh_income_black', 'Median income Black HH ($1,000s)', contacted_departments_data, 1000 ),
  table_reg_function('median_hh_income_hispanic',  'Median income Hispanic HH ($1,000s)', contacted_departments_data, 1000 ),
  table_reg_function('median_hh_income_white',  'Median income White HH ($1,000s)', contacted_departments_data, 1000 ),
  
  table_reg_function('percent_poverty_status',  '% Pop. in poverty', contacted_departments_data, .01 ),
  table_reg_function('percent_poverty_status_black',  '% Black pop. in poverty', contacted_departments_data, .01 ),
  table_reg_function('percent_poverty_status_hispanic', '% Hispanic pop. in poverty', contacted_departments_data, .01 ),
  table_reg_function('percent_poverty_status_white', '% White pop. in poverty', contacted_departments_data, .01 ),
  
  table_reg_function('population', 'Local population (1000s)', contacted_departments_data, 1000 ),
  table_reg_function('percent_black_pop', 'Pop. % Black (county-level)', contacted_departments_data, .01 ),
  table_reg_function('percent_hispanic_pop', 'Pop. % Hispanic (county-level)', contacted_departments_data, .01 ),
  table_reg_function('percent_white_pop', 'Pop. % White (county-level)', contacted_departments_data, .01 ),
  
  table_reg_function('total_employees_total', 'Total employees', contacted_departments_data, 1 ),
  table_reg_function('total_employees_officers', 'Total officers', contacted_departments_data, 1 ),
  table_reg_function('total_employees_civilians', 'Total civilian employees', contacted_departments_data, 1 )
)

#combine estimate and standard errors

table %<>%
  mutate(white_female = paste(wf_e, wf_sd),
         hispanic_male = paste(hm_e, hm_sd),
         hispanic_female = paste(hf_e, hf_sd),
         black_male = paste(bm_e, bm_sd),
         black_female = paste(bf_e, bf_sd)
  ) %>%
  select(variable, mean, white_female, hispanic_male, hispanic_female, black_male, black_female)


#### create table

temp_table= 
kableExtra::kbl(table,
                col.names = NULL,
                format = 'latex',
                booktabs = T) %>%
  kable_styling() %>%
  
  add_header_above(c('', 
                     '(1) White man' = 1,
                     '(2) White woman' = 1,
                     '(3) Hispanic man' = 1,
                     '(4) Hispanic woman' = 1,
                     '(5) Black man' = 1,
                     '(6) Black woman' = 1
  ),
  italic = T
  ) %>%
  add_header_above(c(' ' = 1,
                     'mean' = 1,
                     'Differential' = 5
  )) %>%
  add_header_above(c(' ' = 1,
                     'Identity' = 6
  )) %>%
  pack_rows('Income (county-level)', 1, 8, bold = T) %>%
  pack_rows('Population', 9, 12, bold = T) %>%
  pack_rows('Department size (# of employees)', 13, 15, bold = T) 

#write file
write(temp_table, file=here('003 Output', 'Tables', "table_A5.tex"))

#Note: additional editing done in LaTex

#clean up workspace

rm(contacted_departments_data, table, table_reg_function, temp_table)

### Appendix Table A.6 ----

#racial composition of departments' sworn officers

model_officer_bh_all = feols(response ~ black*percent_sworn_black + hispanic*percent_sworn_hispanic|
                         week + state,
                       main_data)

model_officer_bh_2020 = feols(response ~ black*percent_sworn_black + hispanic*percent_sworn_hispanic|
                          week + state,
                        filter(main_data, lemas_year == '2020'))

model_officer_white_all = feols(response ~ black*percent_sworn_white + hispanic*percent_sworn_white|
                            week + state,
                          main_data)

model_officer_white_2020 = feols(response ~ black*percent_sworn_white + hispanic*percent_sworn_white|
                             week + state,
                           filter(main_data, lemas_year == '2020'))



etable(model_officer_bh_all, model_officer_bh_2020, model_officer_white_all, model_officer_white_2020)

#make table

extra_rows = data.frame( rbind(
  c('LEMAS survey', 'Both','', '2020', '','Both','', '2020', ''),
  c('Num.Obs.', '728', '', '495', '', '728', '', '495', '' )
))

options(modelsummary_format_numeric_latex = "plain")

#attr(extra_rows, 'position') = c(10)

temp_table=
modelsummary(
  list("(1)" = model_officer_bh_all, '(2)' = model_officer_bh_2020, "(3)" = model_officer_white_all, '(4)' = model_officer_white_2020),
  statistic = c("p.value"),  # show p-values
  shape = term ~ model + statistic,
  estimate = "{estimate} \n ({std.error})",
  statistic_map = c( p.value = "P-value"),
  coef_rename = c("black" = "Black",
                  "hispanic" = "Hispanic",
                  'percent_sworn_black' = 'Dept % Black',
                  'percent_sworn_hispanic' = 'Dept % Hispanic',
                  'percent_sworn_white' = 'Dept % White'),
  add_rows = extra_rows,
  gof_map = c("nobs"),
  output = "latex_tabular",
  theme = 'spacing'
) 

#write file
write(temp_table, file=here('003 Output', 'Tables', "table_A6.tex"))

#clean up workspace

rm(extra_rows, model_officer_bh_2020, model_officer_bh_all, 
   model_officer_white_2020, model_officer_white_all, temp_table)
### Appendix Table A.7 ----
#Northeast 


model_northeast <- feols(response ~ black + hispanic + female |
                           week+ state ,
                         data = main_data %>% filter(census_region == 'Northeast'))

#Midwest


model_midwest <- feols(response ~ black + hispanic + female |
                         week + state ,
                       data = main_data %>% filter(census_region == 'Midwest'))

#South 


model_south <- feols(response ~ black + hispanic + female |
                       week + state ,
                     data = main_data %>% filter(census_region == 'South'))

#West 
model_west <- feols(response ~ black + hispanic + female |
                      week + state ,
                    data = main_data %>% filter(census_region == 'West'))

#inspect

etable(model_northeast, model_south, model_midwest, model_west)

#create reference means

main_data %>% 
  filter(race_name == 'White') %>%
  group_by(census_region, race_name) %>%
  summarise(mean(response))

white_reference = c('White', '0.747', '', '0.717', '', '0.790', '', '0.775', '')

main_data %>% 
  filter(gender_name == 'Male') %>%
  group_by(census_region, gender_name) %>%
  summarise(mean(response))

man_reference = c('Man', '0.633', '', '0.643', '', '0.686', '', '0.707', '')

#make table

extra_rows = data.frame(rbind( white_reference, man_reference))

#may need to add nobs ,
#c('Num.Obs.', '547', '', '572', '', '621', '', '346', '')

attr(extra_rows, 'position') = c(4,5)

options(modelsummary_format_numeric_latex = "plain")

temp_table=
modelsummary(
  list("Northeast" = model_northeast, 'South' = model_south, 'Midwest' = model_midwest, 'West' = model_west),
  fmt = getOption("modelsummary_fmt", 4),
  statistic = c("p.value"),  # show p-values
  shape = term ~ model + statistic,
  estimate = "{estimate} \n ({std.error})",
  statistic_map = c( p.value = "P-value"),
  coef_rename = c("black" = "Black", "hispanic" = "Hispanic", 'female' = 'Women'),
  add_rows = extra_rows,
  gof_map = c("nobs"),
  output = "latex_tabular", #turn on for LaTex output
  theme = 'spacing'
)
#write file
write(temp_table, file=here('003 Output', 'Tables', "table_A7.tex"))

#clean up workspace

rm(extra_rows, model_midwest, model_northeast, model_south, model_west,
   man_reference, white_reference, temp_table)

### Appendix Table A.8 ----
#run analysis
model_sign_off = feols(response ~ sign_off + black + hispanic + female +
                         black:sign_off + hispanic:sign_off + female:sign_off|
                         week + state,
                       main_data)

#inspect

etable(model_sign_off)

#make table
temp_table=
modelsummary(
  list("1" = model_sign_off),
  fmt = getOption("modelsummary_fmt", 4),
  statistic = c("p.value"),  # show p-values
  shape = term ~ model + statistic,
  estimate = "{estimate} \n ({std.error})",
  statistic_map = c( p.value = "P-value"),
  coef_rename = c("black" = "Black", "hispanic" = "Hispanic", 'female' = 'Women',
                  'sign_offThank you!' = 'Sign-off'),
  #add_rows = extra_rows,
  gof_map = c("nobs"),
  output = "latex_tabular", #turn on for LaTex output
  theme = 'spacing'
)

#write file
write(temp_table, file=here('003 Output', 'Tables', "table_A8.tex"))
#clean up workspace

rm(model_sign_off, temp_table)
### Appendix Table A.10 ----

#run analysis
model_age = feols(response ~ implied_age + black + hispanic + female +
                    black:implied_age + hispanic:implied_age + female:implied_age|
                    week + state,
                  filter(main_data, age_dummy ==1))#make sure only identities with implied ages are in analysis


#inspect

etable(model_age)

#make table


temp_table =
modelsummary(
  list("1" = model_age),
  fmt = getOption("modelsummary_fmt", 4),
  statistic = c("p.value"),  # show p-values
  shape = term ~ model + statistic,
  estimate = "{estimate} \n ({std.error})",
  statistic_map = c( p.value = "P-value"),
  coef_rename = c("black" = "Black", "hispanic" = "Hispanic", 'female' = 'Women',
                  'implied_age' = 'Age'),
  #add_rows = extra_rows,
  gof_map = c("nobs"),
  output = "latex_tabular", #turn on for LaTex output
  theme = 'spacing'
)
#write file
write(temp_table, file=here('003 Output', 'Tables', "table_A10.tex"))

#clean up workspace

rm(model_age, temp_table)
### Appendix Table A.11 ----
#run analysis
model_word_count <- feols( word_count ~ black + hispanic + female |
                             week + state,
                           cluster =  c('week', 'state'),
                           
                           data = filter(main_data, word_count > 0 ))


#response time 


model_response_time_days <- feols( response_time_hours ~ black + hispanic + female |
                                     week + state,
                                   cluster =  c('week', 'state'),
                                   data = main_data)


#inspect

etable(model_word_count, model_response_time_days)

#make table
temp_table =
modelsummary(
  list("Word count" = model_word_count, 'Response time' = model_response_time_days),
  fmt = getOption("modelsummary_fmt", 4),
  statistic = c("p.value"),  # show p-values
  shape = term ~ model + statistic,
  estimate = "{estimate} \n ({std.error})",
  statistic_map = c( p.value = "P-value"),
  coef_rename = c("black" = "Black", "hispanic" = "Hispanic", 'female' = 'Women',
                  'implied_age' = 'Age'),
  #add_rows = extra_rows,
  gof_map = c("nobs"),
  output = "latex_tabular", #turn on for LaTex output
  theme = 'spacing'
)
#write file
write(temp_table, file=here('003 Output', 'Tables', "table_A11.tex"))

#cleanup
rm(model_response_time_days, model_word_count, temp_table)

### Appendix Table A.12 ----
#remove departments serving less than 7,500 population

main_data_7500 =
  main_data |>
  filter(population >= 7500)

## overall statistics

overall_rr = mean(main_data_7500$response)

race_rr = main_data_7500 %>%
  group_by(race_name) %>%
  summarise(mean(response))

gender_rr = main_data_7500 %>%
  group_by(gender_name) %>%
  summarise(mean(response))


##1 NO WEIGHTS
model_simple <- feols( response ~ black + hispanic + female| week + state,
                       cluster =  c('week', 'state'), 
                       data = main_data_7500)

summary(model_simple)

confint.default(model_simple)

## 2 WEIGHTS

model_weighted <- feols( response ~ black + hispanic + female| week + state,
                         cluster =  c('week', 'state'), 
                         weights = ~sqrt(population),
                         data = main_data_7500)
summary(model_weighted)


confint.default(model_weighted)



### create table
## estimates, standard errors, p.value for 4 models


etable(list(model_simple,
            model_weighted),
       tex = F)

extra_rows = data.frame( rbind(c('Weights', 'Standard OLS','', 'Sqrt. local pop', ''),
                               c('Reference mean', paste('White:', round(race_rr[3,2],3)), '', '', '' ),
                               c( '', paste('Men:', round(gender_rr[2,2],3)), '', '', ''),
                               c( 'Num.Obs', '2086', '', '2086', '')))



#attr(extra_rows, 'position') = c(5,6,7,8)
options(modelsummary_format_numeric_latex = "plain")

temp_table =
modelsummary(
  list("(1)" = model_simple, '(2)' = model_weighted),
  fmt = getOption("modelsummary_fmt", 4),
  statistic = c("p.value"),  # show p-values
  shape = term ~ model + statistic,
  estimate = "{estimate} \n ({std.error})",
  statistic_map = c( p.value = "P-value"),
  coef_rename = c("black" = "Black", "hispanic" = "Hispanic", 'female' = 'Women'),
  add_rows = extra_rows,
  gof_map = c("nobs"),
  output = "latex_tabular", #turn on for LaTex output
  theme = 'spacing'
)

#write file
write(temp_table, file=here('003 Output', 'Tables', "table_A12.tex"))

#cleanup work space

rm(extra_rows, gender_rr, model_simple, model_weighted, race_rr, overall_rr, main_data_7500, temp_table)
