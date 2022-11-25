library(tidyverse)
library(usdata)
library(usmap)
library(cowplot)

# read and data

all_df <- data.frame()

for(year in c(2016,2017,2018,2019)){
  
  input <- paste0('~/Google Drive/My Drive/Code_scripts/state_migration/State_to_State_Migrations_Table_', year,'.xls')
  raw <- readxl::read_xls(input, skip = 6, na = "N/A")
  
  raw_nona <- raw %>% rename(current = `...1`) %>% 
    filter(is.na(current) == FALSE) %>% 
    filter(is.na(`U.S. Island Area`) == FALSE) %>% 
    select(starts_with(c('c',LETTERS))) %>% 
    select(-starts_with("Total"), -`U.S. Island Area`) %>% 
    filter(current != "United States2") 
  
  df_year <- raw_nona %>% pivot_longer(!current, names_to = 'left', values_to = 'count')
  
  # add state abbreviations
  df_year$current_abb <- state2abbr(df_year$current)
  df_year$left_abb <- state2abbr(df_year$left)
  df_year$count <- as.numeric(df_year$count)
  df_year$year <- rep(year,nrow(df_year))
  
  all_df <- rbind(all_df, df_year)
}


sum_all_df <- all_df %>% group_by(current,left, current_abb, left_abb) %>% 
  summarise(count = sum(count))

sum_all_df$year <- rep("Total 2016-2019",nrow(sum_all_df))
all_df$year <- as.character(all_df$year)
all_df <- rbind(sum_all_df, all_df) 

write_csv(all_df,'~/Google Drive/My Drive/Code_scripts/state_migration/migration_dataframe.csv')

# Function
migmap <- function(states, years){
  
  
  test <- all_df %>% filter(current == states, year == years) %>% 
  rename(state = left) %>% 
  mutate(log_count = log(count))
  
atitle <- paste("Came to", states)
  
p1 <- plot_usmap(data = test, values = "count", color = "gray75") + 
  scale_fill_viridis_c(option = 'inferno', direction = 1) +
  theme(legend.position = "bottom",legend.key.width = unit(1.6,"cm"),
        legend.key.height = unit(.4,"cm")) + 
  labs(title = atitle, subtitle = years)

p2 <- plot_usmap(data = test, values = "log_count", color = "gray75") + 
  scale_fill_viridis_c(option = 'inferno', direction = 1) +
  theme(legend.position = "bottom",legend.key.width = unit(1.6,"cm"),
        legend.key.height = unit(.4,"cm"))+ 
  labs(title = paste("(log)",atitle),  subtitle = years)



test2 <- all_df %>% filter(left == states, year == years) %>% 
  rename(state = current) %>% 
  mutate(log_count = log(count))

ltitle <- paste("Left", states)
p3 <- plot_usmap(data = test2, values = "count", color = "gray75") + 
  scale_fill_viridis_c(option = 'inferno', direction = 1) +
  theme(legend.position = "bottom",legend.key.width = unit(1.6,"cm"),
        legend.key.height = unit(.4,"cm")) + 
  labs(title = ltitle, subtitle = years)

p4 <- plot_usmap(data = test2, values = "log_count", color = "gray75") + 
  scale_fill_viridis_c(option = 'inferno', direction = 1) +
  theme(legend.position = "bottom",legend.key.width = unit(1.6,"cm"),
        legend.key.height = unit(.4,"cm")) + 
  labs(title = paste("(log)",ltitle), subtitle = years)


mp <- plot_grid(p1, p2, p3, p4, nrow = 2)

return(mp)

}

migmap("Florida","Total 2016-2019") 




