#This script creates data sets used to create the map figures (Figure 1 and A3)
#The majority of data cleaning is done in `02 general data clean.R`
#This script begins with a product of that script: 02_all_department_data.csv
#which can be found in: '002 Data' -> 'final data'

###Setup: load packages and data ----

library(pacman)
p_load(fastverse, here, stringr, ggplot2, magrittr, tidyverse, data.table, janitor)


# data
main_data =
  here('002 Data', '02 final data', '04_main_data.csv') |>
  read_csv()

# distribution of word count and time of response

temp <- main_data %>% 
  mutate( word_count_bin = cut(word_count,
                               breaks =  c(0, 25, 50,  75, 100, 125, 
                                           150, 175, 200, max(main_data$word_count, na.rm = T)),
                               include.lowest = T,
                               labels = FALSE) |> as.factor(),
          response_time_bin = cut( response_time_hours,
                                   breaks = c(0, 3, 6, 9, 12, 24, 48, 168, 336,
                                              672, max(main_data$response_time_hours, na.rm = T)),
                                   include.lowest = T, 
                                   labels = FALSE)|> as.factor()) 

temp %<>% group_by(word_count_bin) %>% 
  mutate( word_count_bin_n = n()) %>%
  group_by(response_time_bin) %>%
  mutate( response_time_bin_n = n()) %>%
  ungroup()


word_count_data = temp %>% 
  distinct(word_count_bin, .keep_all = T) %>%
  filter(is.na(word_count_bin) == F) %>%
  select(word_count_bin, word_count_bin_n)

response_time_data = temp %>% 
  distinct(response_time_bin, .keep_all = T) %>%
  filter(is.na(response_time_bin) == F) %>%
  select(response_time_bin, response_time_bin_n)


write_csv(word_count_data, here('002 Data', '02 final data', 'word_count_data.csv'))
write_csv(response_time_data, here('002 Data', '02 final data', 'response_time_data.csv'))
