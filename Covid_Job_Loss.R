install.packages('ggmap')
install.packages('usmap')
install.packages('maps')
install.packages('RColorBrewer')
install.packages('tidycensus')
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggmap)
library(usmap)
library(maps)
library(gridExtra)
library(RColorBrewer)
library(tidycensus)
install.packages('stringi')
library(stringi)

#import and rename
#### start ####
#use countypop dataset in R for 2015 population stats
#use countypov dataset for 2014 poverty data

#pad 0s to make fips consistent between datasets
countypop_df <- countypop %>% rename(county_fips = fips) 

translation_codes <- read.csv('lehd_types_s3.csv')
job_loss_county_raw <- read.csv('sum_job_loss_county.csv') 
oldnames <- colnames(job_loss_county[4:24])
newnames <- as.vector(translation_codes[,2])

job_loss_county_raw$county_fips <- stri_pad_left(str = job_loss_county_raw$county_fips, 5, pad = "0")
#### end ####

demographic <- read.csv('cc-est2018-alldata.csv')

job_loss_county <- job_loss_county_raw %>% 
  rename_at(vars(X000:X20), ~ newnames) %>%
  merge(., countypop_df) %>%
  mutate(Job_Loss_Per_Capita = `Total Job Loss Index`/pop_2015)

head(job_loss_county_raw)
unique(countypop_df$abbr)
unique(job_loss_county_raw$state_name)

job_loss_NY <- job_loss_county %>% filter(state_name == 'New York')

job_loss_by_state <- job_loss_county %>% group_by(state_name) %>% 
  summarise_each(funs(if(is.numeric(.)) sum(., na.rm = TRUE) else "")) %>%
  select(state = state_name, Job_Loss = `Total Job Loss Index`, pop_2015) %>%
  mutate(Job_Loss_Per_100Capita = 100*Job_Loss/pop_2015) %>%
  arrange(-Job_Loss)

#Plot job loss from each state
plot.new()
plot_usmap(data = job_loss_by_state, values = "Job_Loss", color = 'black') +
  labs(title = "Total State Job Loss") +
  scale_fill_continuous(low = "green", high = "red", name = "Job Loss (Covid-19)", label = scales::comma) + 
  theme(legend.position = "right")

#Same as before, but now account for state population (per 100 people)
plot.new()
plot_usmap(data = job_loss_by_state, values = "Job_Loss_Per_100Capita", color = 'black') +
  labs(title = "US State Job Loss per 100 people") +
  scale_fill_continuous(low = "green", high = "red", name = "Job Loss (Covid-19)", label = scales::comma) + 
  theme(legend.position = "right")

plot.new()
NY_counties <- job_loss_county %>% filter(abbr == 'NY') %>% 
  select(fips = county_fips, county_name, Job_Loss_Per_Capita) %>%
  mutate(high_job_loss = if_else(Job_Loss_Per_Capita > 0.06, county_name, NA))

NY_Plot <- plot_usmap(data = NY_counties, regions = 'counties', include = 'NY', 
           values = "Job_Loss_Per_Capita", color = 'black') + 
  scale_fill_binned(low = 'white', high = 'red', name = "Job Loss", label = scales::comma,
                    guide = guide_colorsteps(even.steps = F), breaks = c(0,0.02,0.04,0.06,0.08)) +
  theme(legend.position = 'right')

NY_counties %>% filter(Job_Loss_Per_Capita == max(Job_Loss_Per_Capita))

postscript('NY_plot_binned.eps')
NY_Plot
dev.off()

###CO
#### start ####
CO_counties <- job_loss_county %>% filter(abbr == 'CO') %>% 
  select(fips = county_fips, county_name, Job_Loss_Per_Capita)

CO_Plot <- plot_usmap(data = CO_counties, regions = 'counties', include = 'CO', 
                      values = "Job_Loss_Per_Capita", color = 'black') + 
  scale_fill_continuous(low = 'green', high = 'red', name = "Job Loss", label = scales::comma) +
  theme(legend.position = 'right')

CO_counties %>% filter(Job_Loss_Per_Capita == max(Job_Loss_Per_Capita))

postscript('CO_Plot.eps')
CO_Plot
dev.off
#### end ####

###MI
#### start ####
MI_counties <- job_loss_county %>% filter(abbr == 'MI') %>% 
  select(fips = county_fips, county_name, Job_Loss_Per_Capita)

MI_Plot <- plot_usmap(data = CO_counties, regions = 'counties', include = 'MI', 
                      values = "Job_Loss_Per_Capita", color = 'black') + 
  scale_fill_continuous(low = 'green', high = 'red', name = "Job Loss", label = scales::comma) +
  theme(legend.position = 'right')

MI_counties %>% filter(Job_Loss_Per_Capita == max(Job_Loss_Per_Capita))

postscript('MI_Plot.eps')
MI_Plot
dev.off
#### end ####

###CA
#### start #### 
CA_counties <- job_loss_county %>% filter(abbr == 'CA') %>% 
  select(fips = county_fips, county_name, Job_Loss_Per_Capita)

CA_Plot <- plot_usmap(data = CA_counties, regions = 'counties', include = 'CA', 
                      values = "Job_Loss_Per_Capita", color = 'black') + 
  scale_fill_binned(low = 'white', high = 'red', name = "Job Loss", label = scales::comma,
                    breaks = c(0, 0.02, 0.04, 0.06, 0.08)) +
  theme(legend.position = 'right')

CA_counties %>% filter(Job_Loss_Per_Capita == max(Job_Loss_Per_Capita))

postscript('CA_Plot_binned.eps')
CA_Plot
dev.off()

#### end ####

###ND
#### start ####
ND_counties <- job_loss_county %>% filter(abbr == 'ND') %>% 
  select(fips = county_fips, pop_2015, county_name, Job_Loss_Per_Capita)

ND_Plot <- plot_usmap(data = ND_counties, regions = 'counties', include = 'ND', 
                      values = "Job_Loss_Per_Capita", color = 'black') + 
  scale_fill_continuous(low = 'green', high = 'red', name = "Job Loss", label = scales::comma) +
  theme(legend.position = 'right')

ND_counties %>% filter(Job_Loss_Per_Capita == max(Job_Loss_Per_Capita))

ND_Pop_Loss_Plot <- ND_counties %>%
  ggplot() +
  geom_point(aes(x = pop_2015, y = Job_Loss_Per_Capita))

postscript('ND_Plot_final.eps')
ND_Plot
dev.off()

#### end ####

###
#Eliminate 2 SD outliers to look at trends + add regions
#regions large strokes
#### start ####
Northeast <- c('Maine', 'New York', 'New Jersey', 'Vermont', 'Massachusetts', 'Rhode Island',
               'Connecticut', 'New Hampshire', 'Pennsylvania')
Southeast <- c('Alabama', 'Florida', 'Georgia', 'Kentucky', 'Mississippi', 'North Carolina', 
               'South Carolina', 'Tennessee', 'Maryland', 'Virginia', 'West Virginia')
West <- c('Alaska', 'Arizona', 'California', 'Colorado', 'Hawaii', 'Idaho', 'Montana',
          'Nevada', 'New Mexico', 'Oregon', 'Utah', 'Washington', 'Wyoming')
Midwest <- c('Illinois', 'Indiana', 'Iowa', 'Kansas', 'Michigan', 'Minnesota', 'Missouri', 'Nebraska', 
             'North Dakota', 'Ohio', 'South Dakota', 'Wisconsin')
Southwest <- c('Oklahoma', 'Texas')
#### end ####

#regions as defined by densus
#### start ####
PW <- c('WA', 'OR', 'CA','HI','AK')
Mountain <- c('AZ','CO','ID','MT','NM','NV','UT','WY')
MA <- c('NY', 'PA')
WNC <- c('MO','NE', 'MN', 'IA','KS', 'ND','SD')
WSC <- c('AR','LA','OK','TX')
ENC <- c('IL','IN','MI','OH','WI')
NE <- c('CT','MA','ME','NH','NJ', 'RI','VT')
ESC <- c('AL','KY','MS','TN')
SA <- c('DE','FL','GA','NC','MD','SC','VA', 'WV')

state.abb %in% all_states

job_loss_model_df <- job_loss_county %>%
  filter(!(abs(Job_Loss_Per_Capita - median(Job_Loss_Per_Capita)) > 2*sd(Job_Loss_Per_Capita))) %>%
  mutate(region = case_when(
    abbr %in% PW ~ 'PW',
    abbr %in% Mountain ~ 'Mountain',
    abbr %in% WNC ~ 'WNC',
    abbr %in% WSC ~ 'WSC',
    abbr %in% ENC ~ 'ENC',
    abbr %in% NE ~ 'NE',
    abbr %in% ESC ~ 'ESC',
    abbr %in% SA ~ 'SA',
    abbr %in% MA ~ 'MA')) %>%
  mutate(density = if_else(pop_2015 < median(pop_2015), 'Rural', 'Urban'))
#median(job_loss_county$pop_2015) 25698
#### end ####

#Rural vs Urban
rural_urban_job_loss <- ggplot(job_loss_model_df, aes(x = density, y = Job_Loss_Per_Capita, fill = density)) +
  geom_boxplot() +
  theme_bw() +
  theme(legend.title = element_blank()) 

postscript('regional_job_loss2.eps')
regional_job_loss
dev.off()
NE_Job_Loss <- job_loss_model_df %>% filter(region == 'NE')

#regional analysis - which us region has been affected most byc covid-19 job loss?
regional_job_loss <- job_loss_model_df %>%
  ggplot(aes(x = reorder(region, -Job_Loss_Per_Capita, median), y = Job_Loss_Per_Capita, fill = region)) +
  geom_boxplot() +
  theme_bw() +
  theme(legend.title = element_blank()) 


postscript('rural_urban_fin.eps')
rural_urban_job_loss
dev.off()

lm(NE_Job_Loss$Job_Loss_Per_Capita ~ NE_Job_Loss$pop_2015)
cor(NE_Job_Loss$Job_Loss_Per_Capita, NE_Job_Loss$pop_2015)^2

#################################################################################################
#sector analysis - analyze top 5 hit industries
#### start ####
hard_hit_names <- sector_analysis %>% group_by(Industry) %>% summarise(med_sector_loss = median(count)) %>%
  top_n(med_sector_loss, n = 5) %>% pull(Industry)

sector_analysis <- job_loss_model_df %>%
  rename(`Health + Assistance` = `Health Care and Social Assistance`,
         `Arts + Entertainment`= `Arts, Entertainment, and Recreation`,
         Education = `Educational Services`) %>%
  pivot_longer(cols = `Agriculture, Forestry, Fishing, and Hunting`:`Public Administration`, names_to = "Industry", values_to = 'count') %>%
  select(county_name, county_fips, state_name, pop_2015, Industry, count) %>%
  mutate(hard_hit_sector = if_else(Industry %in% hard_hit_names, 'Yes', 'No'))

sector_analysis_top10percent_pop <- sector_analysis %>% top_frac(pop_2015, n = 0.1)
sector_analysis_bottom90percent_pop <- sector_analysis %>% top_frac(-pop_2015, n = 0.9)

top5_job_loss <- sector_analysis %>% filter(hard_hit_sector == 'Yes') %>% pull(count) %>% sum()
all_job_loss <- sector_analysis %>% pull(count) %>% sum()
accomodation_jobloss <- sector_analysis %>% filter(Industry == 'Accomodation and Food Services') %>% pull(count) %>% sum()

#The top 5 sectors account for 73% of total job loss and accomodation and food services accounts for 39%
top5_job_loss/all_job_loss*100
accomodation_jobloss/all_job_loss*100
#examine top5 hit industries (accomodation, arts, education, health care, retail)

sector_analysis_plot <- sector_analysis_top10percent_pop %>% filter(hard_hit_sector == 'Yes') %>%
ggplot(aes(x = reorder(Industry, -count, median), y = count, fill = Industry)) +
  ylim(c(0, 5*10^4)) + 
  geom_boxplot() +
  theme_bw() +
  theme(legend.title = element_blank())
        

postscript(file = "CovidJobloss_EDA2.eps")
regional_job_loss
dev.off()
### end ####

#Each state of interest population vs job loss
abbr_interest <- c('CO', 'CA','ND','NY', 'SD','VT', 'NV', 'MI')
job_loss_county %>%
  filter(abbr %in% abbr_interest) %>%
  ggplot() +
  geom_point(aes(x = pop_2015, y = Job_Loss_Per_Capita, fill = as.factor(state_name)), 
             pch = 21, color = 'black') +
  xlim(c(0, 0.5*10^6))

ggplot(job_loss_by_state) %>%
job_loss_by_state

