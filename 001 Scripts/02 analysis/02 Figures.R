#This script creates the figures shown in the article for figures requiring statistical analysis (figure 1 is excluded)
#Tables are organized by the order in which they appear in the paper
#The data set is a combination of publicly available data and the results of the correspondence study
#Three different data files: 02_all_department_data, 03_distinct_departments, & 04_main_data
#have been created to minimize data manipulation in this script
#To see how these data files were created see `02 data clean.R` (001 Scripts -> pre-analysis folder) of the R scripts
###Setup: load packages and data ----
library(pacman)
p_load(fastverse, here, stringr, ggplot2, magrittr, tidyverse, data.table, ggpattern, extrafont, cowplot,
       patchwork, ggpubr)

# Load fonts for R/ggplot
loadfonts(device = "pdf") 

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

#data for maps

department_map_data =
  here('002 Data', '02 final data', 'department_map_data.csv') |>
  read_csv()


###Figure 2 ----
### A prepare HEX data
# Function that creates hexagons from data.table of points
multi_hex <- function(DT, x, y, id, size) {
  # Function that calculates coordinates for hexagons
  hex_coords <- function(x, y, size) {
    # Create the coordinates
    the_coords <- rbind(
      c(x - size/2, y - size * 0.375),
      c(x - size/2, y + size * 0.375),
      c(x, y + size * 0.625),
      c(x + size/2, y + size * 0.375),
      c(x + size/2, y - size * 0.375),
      c(x, y - size * 0.625)
    ) %>% data.table()
    # Set the names
    setnames(the_coords, c('x', 'y'))
    # Return the coordinates
    return(the_coords)
  }
  # Calculate hexagonal coordinates for each row in DT
  tmp <- mapply(
    FUN = hex_coords,
    x = DT[[x]],
    y = DT[[y]],
    size = size,
    SIMPLIFY = FALSE
  ) %>% rbindlist()
  # Add ID column
  tmp[, id := rep(DT[[id]], each = 6)]
  # Return new data.table
  return(tmp)
}

# Read coordinate file
coord_dt = file.path('002 Data',
                     '01 intermediate data',
                     'figure data',
                     'state-hex-square-coords.csv') %>% fread()


# Capitalize state abbreviations (all caps)
coord_dt[, abbr := abbr %>% str_to_upper()]

#remove DC and HI

coord_dt %<>% filter(abbr != 'DC' & abbr != 'HI')

# Run hexagon function
hex_dt = multi_hex(coord_dt, 'hex_x', 'hex_y', 'abbr', 2)


### join HEX with item of interest


hex_dt %<>% merge(
  y = department_map_data,
  by.x = 'id', by.y = 'state',
  all.x = TRUE, all.y = FALSE, sort = FALSE
)

#remove DC and HI

hex_dt %<>% filter(id != 'DC' & id != 'HI')

### Top panel
#proportion of the population served

temp = 
ggplot(
  data = hex_dt,
  aes(x,y)
) +
  geom_polygon_pattern(
    aes(group = id,
        pattern = prop_pop_bin,
    ),
    fill = 'white',
    color = "black",
    pattern_color = "black",
    pattern_density = 0.1,
    pattern_spacing = 0.02,
    size = .5
  ) +
  geom_label(
    data = coord_dt,
    aes(x = hex_x, y = hex_y, label = abbr),
    color = 'black', size = 5,
    family = "Times New Roman",
  ) +
  scale_pattern_manual(
    name = "Proportion of population served by contacted departments",
    values = c(
      'none',
      "circle",
      "stripe",
      "crosshatch"
    ),
    
  )+
  
  coord_equal() +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    plot.background = element_rect(color = 'white'),
    panel.grid = element_line(color = 'white'),
    legend.position = 'bottom',
    legend.title.position = 'top',
    legend.title = element_text(size = 20),
    legend.key.height = unit(1, 'cm'),
    legend.key.width = unit(1, 'cm'),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.text = element_text(colour = 'grey20', size = 20),
    plot.margin=grid::unit(c(0,0,0,0), 'mm'),
    legend.margin=margin(0,0,0,0),
    legend.box.margin=margin(-10,-10,10,-10)
  ) +
  guides(
    pattern = guide_legend(
      override.aes = list(
        pattern_density = 0.1,
        pattern_spacing = 0.02,
        fill = "white",
        color = "black"
      )
    )
  )

# Save plot
ggsave(
  plot = temp,
  path = here('003 Output', 'figures'),
  filename = 'fig_2_A.png',
  device = ragg::agg_png,
  width = 10,
  height = 7
)
rm(temp)

### Bottom panel 
#create dept contacted 7500 map 
temp =
  ggplot(
    data = hex_dt,
    aes(x,y)
  ) +
  geom_polygon_pattern(
    aes(group = id,
        pattern = prop_contacted_7500_bin,
    ),
    fill = 'white',
    color = "black",
    pattern_color = "black",
    pattern_density = 0.1,
    pattern_spacing = 0.02,
    size = .5
  ) +
  geom_label(
    data = coord_dt,
    aes(x = hex_x, y = hex_y, label = abbr),
    color = 'black', size = 5,
    family = "Times New Roman",
  ) +
  scale_pattern_manual(
    name = "Proportion of departments (serving >7,500) contacted",
    values = c(
      'none',
      "circle",
      "stripe",
      "crosshatch"
    ),
    
  )+
  
  coord_equal() +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    plot.background = element_rect(color = 'white'),
    panel.grid = element_line(color = 'white'),
    legend.position = 'bottom',
    legend.title.position = 'top',
    legend.title = element_text(size = 20),
    legend.key.height = unit(1, 'cm'),
    legend.key.width = unit(1, 'cm'),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.text = element_text(colour = 'grey20', size = 20),
    plot.margin=grid::unit(c(0,0,0,0), 'mm'),
    legend.margin=margin(0,0,0,0),
    legend.box.margin=margin(-10,-10,10,-10)
  ) +
  guides(
    pattern = guide_legend(
      override.aes = list(
        pattern_density = 0.1,
        pattern_spacing = 0.02,
        fill = "white",
        color = "black"
      )
    )
  )

# Save plot
ggsave(
  plot = temp,
  path = here('003 Output', 'figures'),
  filename = 'fig_2_B.png',
  device = ragg::agg_png,
  width = 10,
  height = 7
)
rm(temp, hex_dt, coord_dt, multi_hex)

#NOTE:panels are combined outside of R

###Figure A1 ----
#prepare data
black_ses =
  main_data %>%
  mutate() %>%
  group_by(sender_first_name) %>%
  mutate(response_rate_race = mean(response))%>%
  distinct(sender_first_name, .keep_all = T)%>%
  filter(race_name == 'Black') %>%
  select(sender_first_name, response_rate_race, college)

#plot
temp =
  ggplot(black_ses, aes(x = college, y = response_rate_race)) +
  geom_point( size = 3) +
  geom_smooth(method = 'glm', se = F, color = 'black') +
  theme_bw()+
  theme(text = element_text(family = "Times New Roman"),
        axis.text = element_text(size = 20),
        plot.background = element_rect(color = 'white'),
        plot.title = element_text(size = 40, hjust = .15),
        axis.title = element_text(size = 25)
  ) +
  xlab('Mother\'s education: % some college or higher') +
  ylab('Response rate by first name') 

# Save plot
ggsave(
  plot = temp,
  path = here('003 Output', 'figures'),
  filename = 'fig_A1.png',
  device = ragg::agg_png,
  width = 10,
  height = 7
)
rm(temp, black_ses)
###Figure A2 ----
#prepare data
white_ses =
  main_data %>%
  mutate() %>%
  group_by(sender_first_name) %>%
  mutate(response_rate_race = mean(response))%>%
  distinct(sender_first_name, .keep_all = T)%>%
  filter(race_name == 'White') %>%
  select(sender_first_name, response_rate_race, college)

#plot
temp =
  ggplot(white_ses, aes(x = college, y = response_rate_race)) +
  geom_point( size = 3) +
  geom_smooth(method = 'glm', se = F, color = 'black') +
  theme_bw()+
  theme(text = element_text(family = "Times New Roman"),
        axis.text = element_text(size = 20),
        plot.background = element_rect(color = 'white'),
        plot.title = element_text(size = 40, hjust = .15),
        axis.title = element_text(size = 25)
  ) +
  xlab('Mother\'s education: % some college or higher') +
  ylab('Response rate by first name') 

# Save plot
ggsave(
  plot = temp,
  path = here('003 Output', 'figures'),
  filename = 'fig_A2.png',
  device = ragg::agg_png,
  width = 10,
  height = 7
)
rm(temp, white_ses)

###Figure A3 ----
### A prepare HEX data
# Function that creates hexagons from data.table of points
multi_hex <- function(DT, x, y, id, size) {
  # Function that calculates coordinates for hexagons
  hex_coords <- function(x, y, size) {
    # Create the coordinates
    the_coords <- rbind(
      c(x - size/2, y - size * 0.375),
      c(x - size/2, y + size * 0.375),
      c(x, y + size * 0.625),
      c(x + size/2, y + size * 0.375),
      c(x + size/2, y - size * 0.375),
      c(x, y - size * 0.625)
    ) %>% data.table()
    # Set the names
    setnames(the_coords, c('x', 'y'))
    # Return the coordinates
    return(the_coords)
  }
  # Calculate hexagonal coordinates for each row in DT
  tmp <- mapply(
    FUN = hex_coords,
    x = DT[[x]],
    y = DT[[y]],
    size = size,
    SIMPLIFY = FALSE
  ) %>% rbindlist()
  # Add ID column
  tmp[, id := rep(DT[[id]], each = 6)]
  # Return new data.table
  return(tmp)
}

# Read coordinate file
coord_dt = file.path('002 Data',
                     '01 intermediate data',
                     'figure data',
                     'state-hex-square-coords.csv') %>% fread()


# Capitalize state abbreviations (all caps)
coord_dt[, abbr := abbr %>% str_to_upper()]

#remove DC and HI

coord_dt %<>% filter(abbr != 'DC' & abbr != 'HI')

# Run hexagon function
hex_dt = multi_hex(coord_dt, 'hex_x', 'hex_y', 'abbr', 2)


### join HEX with item of interest


hex_dt %<>% merge(
  y = department_map_data,
  by.x = 'id', by.y = 'state',
  all.x = TRUE, all.y = FALSE, sort = FALSE
)

#remove DC and HI

hex_dt %<>% filter(id != 'DC' & id != 'HI')

### Top panel
#number of depts contacted

temp = 
  ggplot(
    data = hex_dt,
    aes(x,y)
  ) +
  geom_polygon_pattern(
    aes(group = id,
        pattern = dept_count_bin,
    ),
    fill = 'white',
    color = "black",
    pattern_color = "black",
    pattern_density = 0.1,
    pattern_spacing = 0.02,
    size = .5
  ) +
  geom_label(
    data = coord_dt,
    aes(x = hex_x, y = hex_y, label = abbr),
    color = 'black', size = 5,
    family = "Times New Roman",
  ) +
  scale_pattern_manual(
    name = "Number of contacted departments",
    values = c(
      'none',
      "circle",
      "stripe",
      "crosshatch"
    ),
    breaks = c('6-39', '39-72', '72-105', '105-138')
    
  )+
  
  coord_equal() +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    plot.background = element_rect(color = 'white'),
    panel.grid = element_line(color = 'white'),
    legend.position = 'bottom',
    legend.title.position = 'top',
    legend.title = element_text(size = 20),
    legend.key.height = unit(1, 'cm'),
    legend.key.width = unit(1, 'cm'),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.text = element_text(colour = 'grey20', size = 20),
    plot.margin=grid::unit(c(0,0,0,0), 'mm'),
    legend.margin=margin(0,0,0,0),
    legend.box.margin=margin(-10,-10,10,-10)
  ) +
  guides(
    pattern = guide_legend(
      override.aes = list(
        pattern_density = 0.1,
        pattern_spacing = 0.02,
        fill = "white",
        color = "black"
      )
    )
  )

# Save plot
ggsave(
  plot = temp,
  path = here('003 Output', 'figures'),
  filename = 'fig_A3_A.png',
  device = ragg::agg_png,
  width = 10,
  height = 7
)
rm(temp)

### Bottom panel
# proportion of all depts contacted
temp =
  ggplot(
    data = hex_dt,
    aes(x,y)
  ) +
  geom_polygon_pattern(
    aes(group = id,
        pattern = prop_contacted_bin,
    ),
    fill = 'white',
    color = "black",
    pattern_color = "black",
    pattern_density = 0.1,
    pattern_spacing = 0.02,
    size = .5
  ) +
  geom_label(
    data = coord_dt,
    aes(x = hex_x, y = hex_y, label = abbr),
    color = 'black', size = 5,
    family = "Times New Roman",
  ) +
  scale_pattern_manual(
    name = "Proportion of all police departments contacted",
    values = c(
      'none',
      "circle",
      "stripe",
      "crosshatch"
    ),
    breaks = c('7-20%', '20-33%','33-46%','46-59%')
  )+
  
  coord_equal() +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    plot.background = element_rect(color = 'white'),
    panel.grid = element_line(color = 'white'),
    legend.position = 'bottom',
    legend.title.position = 'top',
    legend.title = element_text(size = 20),
    legend.key.height = unit(1, 'cm'),
    legend.key.width = unit(1, 'cm'),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.text = element_text(colour = 'grey20', size = 20),
    plot.margin=grid::unit(c(0,0,0,0), 'mm'),
    legend.margin=margin(0,0,0,0),
    legend.box.margin=margin(-10,-10,10,-10)
  ) +
  guides(
    pattern = guide_legend(
      override.aes = list(
        pattern_density = 0.1,
        pattern_spacing = 0.02,
        fill = "white",
        color = "black"
      )
    )
  )

# Save plot
ggsave(
  plot = temp,
  path = here('003 Output', 'figures'),
  filename = 'fig_A3_B.png',
  device = ragg::agg_png,
  width = 10,
  height = 7
)
rm(temp, hex_dt, coord_dt, multi_hex)

#NOTE:panels are combined outside of R
###Figure A4 ----
#plot the population density 
p1 = ggplot( data = main_data) + 
  geom_density(aes(x = population), size = 1, alpha = .5, color = 'black', fill = NA) +
  scale_x_continuous(breaks = c( 100000, 2000000, 4000000), labels = c('100',  '2,000',  '4,000') ) +
  #scale_y_continuous(labels = label_number()) +
  scale_y_continuous( breaks =  c( 0.00003), labels = c( '0.00003'))+
  theme( 
    text = element_text(family = "Times New Roman"),
    panel.background = element_rect(fill = 'white'),
    plot.background = element_rect(fill = 'white'),
    panel.grid.major.x  = element_blank(),
    strip.background = element_rect(fill = 'white'),
    strip.text = element_text(size = 15),
    axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0, 'mm'),
                                size = 30),
    axis.text = element_text(size = 20, color = 'black'),
    axis.ticks = element_blank(),
    
    #axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, 'mm'),
    #                            size = 25),
    axis.title.y = element_blank(),
    plot.title.position = 'panel',
    plot.title = element_text(margin = margin(t = 3, r = 0, b = 5, l = 0, 'mm'),
                              size = 30, hjust = 0.5),
    legend.background = element_rect(colour = 'white'),
    legend.key.size = unit(1, 'cm'),
    legend.key.spacing.y = unit(5, 'mm'),
    legend.title = element_blank(),
    legend.text = element_text(size = 25),
    plot.margin = margin(t=1, b=1, r=5, l=1 , 'cm')
    
  )+
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = min(main_data$population)) +
  xlab('Raw count')



#using log
p2 = ggplot( data = main_data) + 
  geom_density(aes(x = log(population)), size = 1, alpha = .5, color = 'black', fill = NA) +
  scale_x_continuous(breaks = c(5.6, 10.0, 15.2)) +
  scale_y_continuous( breaks =  c(0.5), labels = c( '0.5' ))+
  theme( 
    text = element_text(family = "Times New Roman"),
    panel.background = element_rect(fill = 'white'),
    plot.background = element_rect(fill = 'white'),
    panel.grid.major.x  = element_blank(),
    strip.background = element_rect(fill = 'white'),
    strip.text = element_text(size = 20),
    axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0, 'mm'),
                                size = 30),
    axis.text = element_text(size = 20, color = 'black'),
    axis.ticks = element_blank(),
    
    #axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, 'mm'),
    #                            size = 25),
    axis.title.y = element_blank(),
    plot.title.position = 'panel',
    plot.title = element_text(margin = margin(t = 3, r = 0, b = 5, l = 0, 'mm'),
                              size = 30, hjust = 0.5),
    legend.background = element_rect(colour = 'white'),
    legend.key.size = unit(1, 'cm'),
    legend.key.spacing.y = unit(5, 'mm'),
    legend.title = element_blank(),
    legend.text = element_text(size = 25),
    plot.margin = margin(t=0.5, b=0.5, r=0.5, l= 0.5, 'cm')
    
  )+
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = min(log(main_data$population))) +
  xlab('Natural log')

#using square root 
p3 = ggplot( data = main_data) + 
  geom_density(aes(x = sqrt(population)), size = 1, alpha = .5, color = 'black', fill = NA) +
  scale_x_continuous(breaks = c(17, 1000, 2004), labels = c('17', '1,000', '2,004')) +
  scale_y_continuous( breaks =  c(0.009), labels = c( '0.009'))+
  theme( 
    text = element_text(family = "Times New Roman"),
    panel.background = element_rect(fill = 'white'),
    plot.background = element_rect(fill = 'white'),
    panel.grid.major.x  = element_blank(),
    strip.background = element_rect(fill = 'white'),
    strip.text = element_text(size = 20),
    axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0, 'mm'),
                                size = 30),
    axis.text = element_text(size = 20, color = 'black'),
    axis.ticks = element_blank(),
    
    #axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, 'mm'),
    #                            size = 25),
    axis.title.y = element_blank(),
    plot.title.position = 'panel',
    plot.title = element_text(margin = margin(t = 3, r = 0, b = 5, l = 0, 'mm'),
                              size = 30, hjust = 0.5),
    legend.background = element_rect(colour = 'white'),
    legend.key.size = unit(1, 'cm'),
    legend.key.spacing.y = unit(5, 'mm'),
    legend.title = element_blank(),
    legend.text = element_text(size = 25),
    plot.margin = margin(t=.5, b=0.5, r=0.5, l=0.5, 'cm')
    
  )+
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = min(sqrt(main_data$population))) +
  xlab('Square root')




p2 = p2 + p3
final_plot <- plot_grid(
  # top row
  p1,
  p2,
  ncol = 1
)
final_plot
# Save plot
ggsave(
  plot = final_plot,
  path = here('003 Output', 'figures'),
  filename = 'fig_A4.png',
  device = ragg::agg_png,
  width = 10,
  height = 7
)
rm(p1, p2, p3, final_plot)
###Figure A5 ----
# response time plot (top panel)

temp =
  ggplot( data = response_time_data, 
          aes( x = as.factor(response_time_bin), 
               y = response_time_bin_n,
          ),
          color= 'black') +
  geom_col(width = .85,
           color= 'black',
           fill = 'white') +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_discrete( labels = c('0-3 H', '3-6 H', '6-9 H', '9-12 H',
                               '12-24 H', '1-2 D', '3-7 D','1-2 W', '2-4 W', '>4 W')) +
  
  theme(text = element_text(family = "Times New Roman"),
    axis.text.x = element_text(size = 35, colour = 'black'),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_blank(),
        legend.position = 'none',
        plot.margin=grid::unit(c(1,1,1,1), 'cm'),
        plot.title = element_text(size = 40, hjust = .5)
  ) +
  geom_vline(xintercept = 9.5,
             color = 'black',
             linetype = 2,
  ) +
  geom_hline(yintercept = 850, color = 'white')+
  geom_text(aes(label = response_time_bin_n),
            colour = 'black', 
            family = "Times New Roman",
            vjust = -.5,
            size = 15) 

# Save plot
ggsave(
  plot = temp,
  path = here('003 Output', 'figures'),
  filename = 'fig_A5_A.png',
  device = ragg::agg_png,
  width = 20,
  height = 14
)
rm(temp)

## Word count

temp = 
  ggplot( data = word_count_data, 
          aes( x = as.factor(word_count_bin), 
               y = word_count_bin_n,
          ),
          color= 'black') +
  geom_col(width = .85,
           color= 'black',
           fill = 'white') +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_discrete( labels = c('<25', '<50', '<75', '<100',
                               '<125', '<150', '<175', '<200',
                               '>200'
  )) +
  theme(text = element_text(family = "Times New Roman"),
    axis.text.x = element_text(size = 35, colour = 'black'),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_blank(),
        legend.position = 'none',
        plot.margin=grid::unit(c(1,1,1,1), 'cm'),
        plot.title = element_text(size = 40, hjust = .5)
  ) +
  geom_hline(yintercept = 500, color = 'white')+
  geom_text(aes(label = word_count_bin_n),
            family = "Times New Roman",
            colour = 'black', 
            vjust = -.5,
            size = 15) 
# Save plot
ggsave(
  plot = temp,
  path = here('003 Output', 'figures'),
  filename = 'fig_A5_B.png',
  device = ragg::agg_png,
  width = 20,
  height = 14
)
rm(temp)
###Figure A6 ----
#data prep
# Distribution of Black, White, Hispanic populations (and same across identities)
race_data <- main_data %>% select(race_name, 
                                  percent_black_pop, percent_hispanic_pop, percent_white_pop)

#reformat data
race_data_wide <- race_data %>% pivot_longer(cols = starts_with('percent'),
                                             names_to = 'racial_comp',
                                             values_to = 'racial_comp_value')

#plot

p1 = 
  ggplot( data = filter(race_data_wide, race_name == 'Black')) + 
  geom_density(aes(x = racial_comp_value,
                   linetype = racial_comp), size = 1) +
  geom_hline(yintercept = 0, color = 'lightgray') +
  scale_x_continuous(breaks = seq(0,1,.20),
                     labels = c('0%', '20%','40%', '60%','80%','100%')) +
  scale_linetype_manual(
    name = "Population Race",
    values = c(
      "percent_black_pop"    = "solid",
      "percent_hispanic_pop" = "dashed",
      "percent_white_pop"    = "dotted"
    ),
    labels =
      c("Pop. Black", 'Pop. Hispanic', 'Pop. White')
  )+
  theme( 
    text = element_text(family = "Times New Roman"),
    panel.background = element_rect(fill = 'white'),
    plot.background = element_rect(fill = 'white'),
    panel.grid.major.x  = element_line( color = 'lightgray'),
    strip.background = element_rect(fill = 'white'),
    strip.text = element_text(size = 20),
    axis.text.x = element_text(size = 20, color = 'black', hjust = .7),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(margin = margin(t = 3, r = 0, b = 5, l = 0, 'mm'),
                              size = 30, hjust = 0.5),
    legend.position = 'none',
    plot.margin = margin(t=0.5, b=0.5, r=0, l= .5, 'cm')
    
  )+
  ggtitle('Black email')

p2 = 
  ggplot( data = filter(race_data_wide, race_name == 'Hispanic')) + 
  geom_density(aes(x = racial_comp_value,
                   linetype = racial_comp), size = 1) +
  geom_hline(yintercept = 0, color = 'lightgray') +
  scale_x_continuous(breaks = seq(0,1,.20),
                     labels = c('0%', '20%','40%', '60%','80%','100%')) +
  scale_linetype_manual(
    name = "Population Race",
    values = c(
      "percent_black_pop"    = "solid",
      "percent_hispanic_pop" = "dashed",
      "percent_white_pop"    = "dotted"
    ),
    labels =
      c("Pop. Black", 'Pop. Hispanic', 'Pop. White')
  )+
  theme( 
    text = element_text(family = "Times New Roman"),
    panel.background = element_rect(fill = 'white'),
    plot.background = element_rect(fill = 'white'),
    panel.grid.major.x  = element_line( color = 'lightgray'),
    strip.background = element_rect(fill = 'white'),
    strip.text = element_text(size = 20),
    axis.text.x = element_text(size = 20, color = 'black', hjust = .7),
    axis.ticks.y = element_blank(),
    
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(margin = margin(t = 3, r = 0, b = 5, l = 0, 'mm'),
                              size = 30, hjust = 0.5),
    
    legend.position = 'none',
    plot.margin = margin(t=.5, b=.5, r=.5, l= 1, 'cm')
  ) +
  ggtitle('Hispanic email')
p3 = 
  ggplot( data = filter(race_data_wide, race_name == 'White')) + 
  geom_density(aes(x = racial_comp_value,
                   linetype = racial_comp), size = 1) +
  geom_hline(yintercept = 0, color = 'lightgray') +
  scale_x_continuous(breaks = seq(0,1,.20),
                     labels = c('0%', '20%','40%', '60%','80%','100%')) +
  scale_linetype_manual(
    name = "Population Race",
    values = c(
      "percent_black_pop"    = "solid",
      "percent_hispanic_pop" = "dashed",
      "percent_white_pop"    = "dotted"
    ),
    labels =
      c("Pop. Black", 'Pop. Hispanic', 'Pop. White')
  )+
  guides(linetype = 'legend') +
  theme( 
    text = element_text(family = "Times New Roman"),
    panel.background = element_rect(fill = 'white'),
    plot.background = element_rect(fill = 'white'),
    panel.grid.major.x  = element_line( color = 'lightgray'),
    strip.background = element_rect(fill = 'white'),
    strip.text = element_text(size = 20),
    axis.title.x = element_text(margin = margin(t = 3, r = 0, b = 0, l = 0, 'mm'),
                                size = 35,
                                hjust = 4),
    axis.text.x = element_text(size = 20, color = 'black', hjust = .7),
    axis.ticks.y = element_blank(),
    
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    plot.title.position = 'panel',
    plot.title = element_text(margin = margin(t = 3, r = 0, b = 5, l = 0, 'mm'),
                              size = 30, hjust = 0.5),
    legend.background = element_rect(colour = 'white'),
    legend.key.size = unit(1, 'cm'),
    legend.key.spacing.y = unit(5, 'mm'),
    legend.title = element_blank(),
    legend.text = element_text(size = 25),
    legend.position = 'right',
    legend.margin = margin(t=0, b=1, r=0, l= 2, 'cm'),
    plot.margin = margin(t=1, b=.5, r=4.5, l= 0.5, 'cm')
    
  ) +
  xlab('County population') +
  ggtitle('White email')


p1 = p1 + p2
final_plot <- plot_grid(
  # top row
  p1,
  p3,
  ncol = 1
)
final_plot

# Save plot
ggsave(
  plot = final_plot,
  path = here('003 Output', 'figures'),
  filename = 'fig_A6.png',
  device = ragg::agg_png,
  width = 10,
  height = 7
)
rm(final_plot, p1, p2, p3, race_data, race_data_wide)
