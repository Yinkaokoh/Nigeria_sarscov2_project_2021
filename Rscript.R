#   SARS-CoV2 Research Project in Nigeria using data from GISAID, and covid19.ncndc.gov.ng NCDC
#   In collaboration with Nidia (USA), Nicholas and Ann (Ghana) and Elijah et al (Nigeria)
#   script written by Olayinka Sunday OKOH 
#   21st April, 2021
#   The data were downloaded on 19th April, 2021 from gisaid and covid19.ncdc.gov.ng

library(readr) # To read in the tsv file
library(dplyr) # For data manipulation and wrangling
library(ggplot2) # For Visualization
library(readxl) # For reading in excel files
library(tmap) # For Maps
library(tmaptools) #  For Maps
library(sf) # For Maps



#read in data from covid19.ncdc.gov.ng
updated_nigeria_ncdc <- read_excel("raw_data/nig_covid19_data_wk_14_2021.xlsx")
nigeria_ncdc <- as.data.frame(updated_nigeria_ncdc)

# Merge the data from gisaid and ncdc together
nigeria_cases_and_sequences <- left_join(nigeria_ncdc, nigeria_sequences, by = "State")





#    ====================== MAPS ================================

# Read in the Nigeria shape file which was downloaded from gadm.org
nigeria_sh <- st_read("raw_data/gadm36_NGA_shp/gadm36_NGA_1.shp")

#Let COVID-19 data in Nigeria be nigeria_data and rename State as NAME_1 since that is what is used in Tmap
nigeria_data <- nigeria_cases_and_sequences %>% rename(NAME_1 = State)

#Join the map data and covid-19 data and name it as nigeria
nigeria <- left_join(nigeria_sh, nigeria_data, by = "NAME_1")

##Map of COVID-19 Cases as reported by NCDC
nigeria_cases_map <- tm_shape(nigeria) +
  tm_fill(col = "Confirmed", style = "cont", palette = "Reds", title = "COVID-19 cases") +
  tm_borders(col = "black", lwd = 1) +
  tm_text("NAME_1") 
#tm_credits("COVID-19 cases \nacross Nigeria", size = 0.8, position = c(0.6, 0.01))
#tm_layout(frame = FALSE, legend.width = 0.7, legend.position = c(0.85, 0.8), legend.title.size = 0.7) 
tmap_save(nigeria_cases_map, filename = "outputs/Nigeria_Covid_19_cases_map.pdf")
tmap_save(nigeria_cases_map, filename = "outputs/Nigeria_Covid_19_cases_map.jpg")

##Map  of Nigeria Recoveries
nigeria_Recoveries_map <- tm_shape(nigeria) +
  tm_fill(col = "Recoveries", style = "cont", palette = "Blues", title = "Recoveries") +
  tm_borders(col = "black", lwd = 1) +
  tm_text("NAME_1") 
#tm_credits("COVID-19 Recoveries \nacross Nigeria", size = 0.8, position = c(0.6, 0.01))
#tm_layout(frame = FALSE, legend.width = 0.7, legend.position = c(0.85, 0.8), legend.title.size = 0.7) 
tmap_save(nigeria_Recoveries_map, filename = "outputs/Nigeria_Covid_19_Recoveries_map.pdf")
tmap_save(nigeria_Recoveries_map, filename = "outputs/Nigeria_Covid_19_Recoveries_map.jpg")


##Map  of Nigeria Deaths
nigeria_Deaths_map <- tm_shape(nigeria) +
  tm_fill(col = "Deaths", style = "cont", palette = "Reds", title = "Deaths") +
  tm_borders(col = "black", lwd = 1) +
  tm_text("NAME_1") 
#tm_credits("COVID-19 Deaths \nacross Nigeria", size = 0.8, position = c(0.6, 0.01))
#tm_layout(frame = FALSE, legend.width = 0.7, legend.position = c(0.85, 0.8), legend.title.size = 0.7) 
tmap_save(nigeria_Deaths_map, filename = "outputs/Nigeria_Covid_19_Deaths_map.pdf")
tmap_save(nigeria_Deaths_map, filename = "outputs/Nigeria_Covid_19_Deaths_map.jpg")


##Map  of Nigeria Testing
nigeria_Test_map <- tm_shape(nigeria) +
  tm_fill(col = "Tests", style = "cont", palette = "Blues", title = "COVID-19 tests") +
  tm_borders(col = "black", lwd = 1) +
  tm_text("NAME_1") 
#tm_credits("COVID-19 Testing \nacross Nigeria", size = 0.8, position = c(0.6, 0.01))
#tm_layout(frame = FALSE, legend.width = 0.7, legend.position = c(0.85, 0.8), legend.title.size = 0.7) 
tmap_save(nigeria_Test_map, filename = "outputs/Nigeria_Covid_19_Testing_map.pdf")
tmap_save(nigeria_Test_map, filename = "outputs/Nigeria_Covid_19_Testing_map.jpg")



##Map  of Nigeria sequences
nigeria_sequences_map <- tm_shape(nigeria) +
  tm_fill(col = "sequences", style = "cont", palette = "Blues", title = "Sequences in GISAID") +
  tm_borders(col = "black", lwd = 1) +
  tm_text("NAME_1") 
#tm_credits("COVID-19 sequences \nacross Nigeria", size = 0.8, position = c(0.6, 0.01))
#tm_layout(frame = FALSE, legend.width = 0.7, legend.position = c(0.85, 0.8), legend.title.size = 0.7) 
tmap_save(nigeria_sequences_map, filename = "outputs/Nigeria_Covid_19_sequences_map.pdf")
tmap_save(nigeria_sequences_map, filename = "outputs/Nigeria_Covid_19_sequences_map.jpg")



##Map  of Nigeria Cases_1M
nigeria_Cases_1M_map <- tm_shape(nigeria) +
  tm_fill(col = "Cases_1M", style = "cont", palette = "Reds", title = "Cases per million") +
  tm_borders(col = "black", lwd = 1) +
  tm_text("NAME_1") 
#tm_credits("COVID-19 cases per 1M \npopulation across Nigeria", size = 0.8, position = c(0.6, 0.01))
#tm_layout(frame = FALSE, legend.width = 0.7, legend.position = c(0.85, 0.8), legend.title.size = 0.7) 
tmap_save(nigeria_Cases_1M_map, filename = "outputs/Nigeria_Covid_19_Cases_1M_map.pdf")
tmap_save(nigeria_Cases_1M_map, filename = "outputs/Nigeria_Covid_19_Cases_1M_map.jpg")



##Map  of Nigeria Tests_1M
nigeria_Tests_1M_map <- tm_shape(nigeria) +
  tm_fill(col = "Test_1M", style = "cont", palette = "Blues", title = "Tests per million") +
  tm_borders(col = "black", lwd = 1) +
  tm_text("NAME_1") 
#tm_credits("COVID-19 Tests per 1M \npopulation across Nigeria", size = 0.8, position = c(0.6, 0.01))
#tm_layout(frame = FALSE, legend.width = 0.7, legend.position = c(0.85, 0.8), legend.title.size = 0.7) 
tmap_save(nigeria_Tests_1M_map, filename = "outputs/Nigeria_Covid_19_Tests_1M_map.pdf")
tmap_save(nigeria_Tests_1M_map, filename = "outputs/Nigeria_Covid_19_Tests_1M_map.jpg")



##Map  of Nigeria Positive_1KTests
nigeria_Positive_1KTests_map <- tm_shape(nigeria) +
  tm_fill(col = "Positive_1KTests", style = "cont", palette = "Blues", title = "Positive per 1,000 tests") +
  tm_borders(col = "black", lwd = 1) +
  tm_text("NAME_1") 
#tm_credits("COVID-19 Tests per 1M \npopulation across Nigeria", size = 0.8, position = c(0.6, 0.01))
#tm_layout(frame = FALSE, legend.width = 0.7, legend.position = c(0.85, 0.8), legend.title.size = 0.7) 
tmap_save(nigeria_Positive_1KTests_map, filename = "outputs/Nigeria_Covid_19_Positive_1KTests_map.pdf")
tmap_save(nigeria_Positive_1KTests_map, filename = "outputs/Nigeria_Covid_19_Positive_1KTests_map.jpg")


##Map  of Nigeria Sequences_1HCases
nigeria_Sequences_1HCases_map <- tm_shape(nigeria) +
  tm_fill(col = "Sequences_1HCases", style = "cont", palette = "Blues", title = "Sequences per 100 cases") +
  tm_borders(col = "black", lwd = 1) +
  tm_text("NAME_1") 
#tm_credits("COVID-19 Tests per 1M \npopulation across Nigeria", size = 0.8, position = c(0.6, 0.01))
#tm_layout(frame = FALSE, legend.width = 0.7, legend.position = c(0.85, 0.8), legend.title.size = 0.7) 
tmap_save(nigeria_Sequences_1HCases_map, filename = "outputs/Nigeria_Covid_19_Sequences_1HCases_map.pdf")
tmap_save(nigeria_Sequences_1HCases_map, filename = "outputs/Nigeria_Covid_19_Sequences_1HCases_map.jpg")


####Map  of Nigeria Recovery_1HCases
nigeria_Recovery_1HCases_map <- tm_shape(nigeria) +
  tm_fill(col = "Recovery_1HCases", style = "cont", palette = "Blues", title = "Recoveries per 100 cases") +
  tm_borders(col = "black", lwd = 1) +
  tm_text("NAME_1") 
#tm_credits("COVID-19 Tests per 1M \npopulation across Nigeria", size = 0.8, position = c(0.6, 0.01))
#tm_layout(frame = FALSE, legend.width = 0.7, legend.position = c(0.85, 0.8), legend.title.size = 0.7) 
tmap_save(nigeria_Recovery_1HCases_map, filename = "outputs/Nigeria_Covid_19_Recovery_1HCases_map.pdf")
tmap_save(nigeria_Recovery_1HCases_map, filename = "outputs/Nigeria_Covid_19_Recovery_1HCases_map.jpg")


##Map  of Nigeria Deaths_1HCases
nigeria_Deaths_1HCases_map <- tm_shape(nigeria) +
  tm_fill(col = "Deaths_1HCases", style = "cont", palette = "Reds", title = "Deaths per 100 cases") +
  tm_borders(col = "black", lwd = 1) +
  tm_text("NAME_1") 
#tm_credits("COVID-19 Tests per 1M \npopulation across Nigeria", size = 0.8, position = c(0.6, 0.01))
#tm_layout(frame = FALSE, legend.width = 0.7, legend.position = c(0.85, 0.8), legend.title.size = 0.7) 
tmap_save(nigeria_Deaths_1HCases_map, filename = "outputs/Nigeria_Covid_19_Deaths_1HCases_map.pdf")
tmap_save(nigeria_Deaths_1HCases_map, filename = "outputs/Nigeria_Covid_19_Deaths_1HCases_map.jpg")

##Projected COVID-19 Cases in Nigeria based on no of positive tests per 1000 tests

nigeria_projected_cases_map <- tm_shape(nigeria) + 
  tm_fill(col = "Projected_cases", style = "cont", palette = "Reds", title = "Projected COVID-19 Cases in Nigeria") +
  tm_borders(col = "black", lwd = 1) +
  tm_text("NAME_1") 
tmap_save(nigeria_projected_cases_map, filename = "outputs/nigeria_projected_cases_map.pdf")
tmap_save(nigeria_projected_cases_map, filename = "outputs/nigeria_projected_cases_map.jpg")


#=============== END of MAPS ========================================


#================== Bar charts and other plots
##States that submitted sequences
ggplot(nigeria_sequences, aes(y = reorder(State, sequences), x = sequences)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(Percentage, "%")), position = position_dodge(width = 1), vjust = 0.5) +
  labs(title = "SARS-CoV-2 sequences submitted in GISAID from Nigeria", y = "States", x = "Sequences")
ggsave("outputs/nigeria_seq_submitted_bar.jpg", height =  120, width = 200, units = "mm")
ggsave("outputs/nigeria_seq_submitted_bar.pdf", height =  120, width = 200, units = "mm")


##Sequences submitted per 100 cases
##States that submitted sequences
ggplot(na.omit(nigeria_cases_and_sequences), aes(y = reorder(State, Sequences_1HCases), x = Sequences_1HCases)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(round(Sequences_1HCases,2), "%")), position = position_dodge(width = 1), vjust = 0.5) +
  labs(title = "Sequences submitted per 100 COVID-19 cases from Nigeria", y = "States", x = "%")
ggsave("outputs/nigeria_seq_submitted_per1H_bar.jpg", height =  120, width = 200, units = "mm")
ggsave("outputs/nigeria_seq_submitted_per1H_bar.pdf", height =  120, width = 200, units = "mm")


##Lineages circulating in Nigeria 
ggplot(Lineages, aes(y = reorder(Lineage, N), x = N)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(Percentage, "%")), position = position_dodge(width = 1), vjust = 0.5) +
  labs(title = "SARS-CoV-2 lineages circulating in Nigeria", y = "Lineages", x = "Number of sequences")
ggsave("outputs/nigeria_pango_lineages_bar.jpg", height =  120, width = 200, units = "mm")
ggsave("outputs/nigeria_pango_lineages_bar.pdf", height =  120, width = 200, units = "mm")



##  GISAID Clade diversity
ggplot(GISAID_clade, aes(y = reorder(Clade, N), x = N)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(Percentage, "%")), position = position_dodge(width = 1), vjust = 0.5) +
  labs(title = "GISAID clade diversity in Nigeria", y = "GISAID clades", x = "Number of sequences")
ggsave("outputs/nigeria_GISAID_bar.jpg", height =  120, width = 200, units = "mm")
ggsave("outputs/nigeria_GISAID_bar.pdf", height =  120, width = 200, units = "mm")

# =========================== END OF SCRIPT=================================================



###For manuscript writing purpose
cases <- nigeria_cases_and_sequences %>% select(State, Confirmed) %>% mutate (Perc = round((Confirmed/sum(Confirmed))*100,2))

ggplot(cases, aes(y = reorder(State, Confirmed), x = Confirmed))+
  geom_text(aes(label = paste0(Perc, "%")), position = position_dodge(width = 1), vjust = 0.5, color = "red") 

cases_1M <- nigeria_cases_and_sequences %>% select(State, Cases_1M) %>% arrange(Cases_1M) %>%  View ()

tests <- nigeria_cases_and_sequences %>% select(State, Testing) %>% arrange(Testing) %>%  View ()

tests_1M <- nigeria_cases_and_sequences %>% select(State, Test_1M) %>% arrange(Test_1M) %>%  View ()

Positive_1KTests <- nigeria_cases_and_sequences %>% select(State, Positive_1KTests) %>% arrange(Positive_1KTests) %>%  View ()

Projected_cases <- nigeria_cases_and_sequences %>% select(State, Projected_cases) %>% arrange(Projected_cases) %>%  View ()

Recoveries <- nigeria_cases_and_sequences %>% select(State, Recoveries) %>% arrange(Recoveries) %>%  View ()

Recovery_1HCases <- nigeria_cases_and_sequences %>% select(State, Recovery_1HCases) %>% arrange(Recovery_1HCases) %>%  View ()

Deaths <- nigeria_cases_and_sequences %>% select(State, Deaths) %>% arrange(Deaths) %>%  View ()

Deaths_1HCases <- nigeria_cases_and_sequences %>% select(State, Deaths_1HCases) %>% arrange(Deaths_1HCases) %>%  View ()





























































# Evolutionary Dynamics of SARS-Cov2 circulating in Nigeria.
# A Collaborative work - Nidia (NIH - USA), Nicholas and Anna (Ghana); Elijah et al (Nigeria)
# Script writtten by Okoh Olayinka Sunday
# May 16th, 2021



# Load the libraries/packages needed for the analysis
library(readr) # to use read_tsv()
library(dplyr) # for data manipulation
library(stringr) # for string manipulation
library(ggplot2) # for data visualization

# Load the meta data
# All the data from gisaid is named gisaid_data
# Nigeria data from gisaid is named nig_gisaid
gisaid_data <- read_tsv("/home/olayinka/Documents/R_Analysis/Nigeria_sarscov2_project_2021/raw_data/metadata.tsv")

# Filter Nigeria data
nig_gisaid <- gisaid_data[str_detect(gisaid_data$Location, "Nigeria"), ] # I used this because I could not use dplyr consequent of the inconsistencies in the naming in column

# alternate method is the use of library(data.table) with the code below
# nig_gisaid <- gisaid_data[gisaid_data$Location %like% "Nigeria", ]

# Select relevant columns for the Nigeria data and save as dataframe in the Nigeria 
nig_gisaid <- nig_gisaid %>% select(Location, `Collection date`, `Sequence length`, `Patient age`, Gender, Clade, `Pango lineage`, `Submission date`, `GC-Content`)

dim(nig_gisaid) # 704 sequences submitted to gisaid from Nigeria
nig_gisaid <- as.data.frame(nig_gisaid) # To ensure the data is in data frame format

# Sequence length
range(nig_gisaid$`Sequence length`)

# Gender distribution 
gender <- table(nig_gisaid$Gender) # Female 238 | Male 323 | Unknown 143

# Plot the gender distribution into a barplot with ggplot2
sex <- c("Male", "Female", "NA")
patients <- as.data.frame(sex)
patients$N <- c(323, 238, 143)
patients <- patients %>% mutate(Percentage = round((N/sum(N))*100, 1))

ggplot(patients, aes(x = reorder(sex, N), y = N)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(Percentage, "%")), position = position_dodge(width = 1), vjust = 0.5)+
  labs(y = "Number of patients", x = "Sex of patients")
ggsave("outputs/nigeria_sex_bar.jpg", height =  120, width = 200, units = "mm")
ggsave("outputs/nigeria_sex_bar.pdf", height =  120, width = 200, units = "mm")


# Age distribution
sum(is.na(as.numeric(nig_gisaid$`Patient age`))) # 216 patients ages not declared
nig_age <- na.omit(as.numeric(nig_gisaid$`Patient age`)) # ensure the data is numeric
patients_ages <- as.data.frame(nig_age) # set the ages as data frame to plot ggplot 2 graph 
range(nig_age) 
mean_age <- mean(na.omit(as.numeric(nig_gisaid$`Patient age`))) #40.06  
median_age <- median(na.omit(as.numeric(nig_gisaid$`Patient age`))) # 38

# Plot a density plot of age distribution
ggplot(patients_ages, aes(x = nig_age)) +
  geom_density(fill = "grey") +
  geom_vline(xintercept = (mean_age), color = "red") # to add the mean age  +
  labs(x = "Patients age", y = "Density")
ggsave("outputs/nigeria_age.jpg", height =  120, width = 200, units = "mm")
ggsave("outputs/nigeria_age_bar.pdf", height =  120, width = 200, units = "mm")

# histogram of ages. 
# Histogram and bar plots were made for the group to select the one preferred
# plot it in pdf
pdf("outputs/Age_hist.pdf")
hist(nig_age, main = NULL, xlab = "Age") 
abline(v = mean_age, col = "red", lwd = 4)
dev.off()

# Plot it also in jpg
jpeg("outputs/Age_hist.jpg")
hist(nig_age, main = NULL, xlab = "Age") 
abline(v = mean_age, col = "red", lwd = 4)
dev.off()
#####################

# Get the number sequences submitted from each state

nig_sequences <- nig_gisaid %>% select(Location) %>% group_by(Location) %>% summarize (sequences = n()) %>% mutate(Percentage = round((sequences/sum(sequences)*100),1))

# The naming of the Location is inconsistent, so create a new column named State for consistency
nig_sequences$State<- c("", "Federal Capital Territory", "Federal Capital Territory", "Adamawa", "Akwa Ibom", "Akwa Ibom", "Bauchi", "Benue", "Borno", "Delta", "Ebonyi", "Ebonyi", "Edo", "Edo", "Ekiti", "Ekiti", "Enugu", "Federal Capital Territory", "Federal Capital Territory", "Imo", "Kaduna", "Kano", "Katsina", "Kogi", "Kwara", "Lagos", "Lagos", "Nassarawa", "Niger", "Ogun", "Ondo", "Ondo", "Osun", "Osun", "Oyo", "Oyo", "Plateau", "Rivers", "Sokoto", "Zamfara")

# 3 sequences could not be traced to any State. They are just tagged Nigeria, so they should be excluded
nig_sequences <- nig_sequences[-1,]

nigeria_sequences <- nig_sequences %>% select(State, sequences) %>% group_by(State) %>% summarise(Sequences = sum(sequences)) %>% mutate(Percentage =round((Sequences/sum(Sequences))*100, 2)) %>% arrange(desc(Sequences))

# How many states submitted sequences to gisaid?
dim(nigeria_sequences)  # Only 28 States submitted sequences to gisaid from Nigeria

# Export the tables as Sequences submitted to gisaid from each Nigeria State
write.csv(nigeria_sequences, "outputs/Submitted_Sequences.csv")



# How many Pango lineages are in Nigeria?
length(unique(nig_gisaid$`Pango lineage`)) # 37 Pango lineages in Nigeria

# Pangolin lineages in Nigeria
nigeria_pango_lineages <- nig_gisaid %>% group_by(`Pango lineage`) %>% summarize(N = n()) %>% mutate (Percentage = round((N/sum(N))*100,1)) %>% arrange(desc(N))

# Export the table of Lineages circulating in Nigeria
write.csv(nigeria_pango_lineages, "outputs/Lineages_in_Nigeria.csv")


# Nextstrain clades in Nigeria
nigeria_gisaid_clades <- nig_gisaid %>% group_by(Clade) %>% summarize(N = n()) %>% mutate (Percentage = round((N/sum(N))*100,1)) %>% arrange(desc(N))

# Export the clades in Nigeria
write.csv(nigeria_gisaid_clades, "outputs/Clades_in_Nigeria.csv")


# To plot the incidence of SARS-CoV2 in Nigeria using submitted sequences

# extract needed data with date
nigeria_date <- nig_gisaid %>% select(`Pango lineage`, `Collection date`) %>% group_by(`Collection date`) %>% summarize(N = n())
nigeria_date$`Collection date` <- as.Date(nigeria_date$`Collection date`)
nigeria_date <- nigeria_date[-1,]

# 2 sequences does not belong to a specific date rather just 2020. Exclude them

# Plot a chart of cumulative sequences submitted with lock down dates
 ggplot(nigeria_date, aes(x = as.Date(`Collection date`), y = cumsum(N), group = 1)) +
 geom_line(color = 'red') +
geom_vline(xintercept = as.Date(c("2020-03-30","2020-07-01", "2020-10-01"))) +
 labs( y = "Number of sequences", x = "Date")
 ggsave("outputs/nigeria_cumulative.jpg", height =  120, width = 200, units = "mm")
 ggsave("outputs/nigeria_cumulative_bar.pdf", height =  120, width = 200, units = "mm")


 
 
# To plot incidence graph of the top 5 lineages
# The top 6 lineages in Nigeria are "B.1.525" (160), "B.1.1.7" (137), "B.1" (80), "B.1.1" (79),"L.3" (67), and "B.1.1.487" (41)
 Top_lineages <- nig_gisaid %>% select(`Pango lineage`, `Collection date`)  %>% filter(`Pango lineage` == c("B.1.525", "B.1.1.7", "B.1", "B.1.1", "L.3", "B.1.1.487")) %>% group_by(`Collection date`, `Pango lineage`) %>% summarise (N = n()) %>% mutate(Percentage = N/sum(N))

ggplot(Top_lineages, aes(x = as.Date(`Collection date`), y = Percentage, 
                       fill = `Pango lineage`)) +
  geom_area(size = 0.3, color = 'white') + 
  labs(y = "Percentage", x = "Date")
   ggsave("outputs/nigeria_Top6_incidence.jpg", height =  120, width = 200, units = "mm")
   ggsave("outputs/nigeria_Top6_incidence_bar.pdf", height =  120, width = 200, units = "mm")

# Pango lineages circulating in Nigeria
   ggplot(nigeria_pango_lineages, aes(y = reorder(`Pango lineage`, N), x = N)) +
     geom_bar(stat = "identity", fill = "steelblue") +
     geom_text(aes(label = paste0(Percentage, "%")), position = position_dodge(width = 1), vjust = 0.5) +
     labs(y = "Pango lineages", x = "Number of sequences")
   ggsave("outputs/nigeria_pango_lineages_bar.jpg", height =  120, width = 200, units = "mm")
   ggsave("outputs/nigeria_pango_lineages_bar.pdf", height =  120, width = 200, units = "mm")
   
  
   
   ##GISAID Clade diversity
   ggplot(nigeria_gisaid_clades, aes(y = reorder(Clade, N), x = N)) +
     geom_bar(stat = "identity", fill = "blue") +
     geom_text(aes(label = paste0(Percentage, "%")), position = position_dodge(width = 1), vjust = 0.5) +
     labs(y = "GISAID clades", x = "Number of sequences")
   ggsave("outputs/nigeria_GISAID_clades_bar.jpg", height =  120, width = 200, units = "mm")
   ggsave("outputs/nigeria_GISAID_bar.pdf", height =  120, width = 200, units = "mm")
   
#  States that submitted sequences
   ggplot(nigeria_sequences, aes(y = reorder(State, Sequences), x = Sequences)) +
     geom_bar(stat = "identity", fill = "steelblue") +
     geom_text(aes(label = paste0(Percentage, "%")), position = position_dodge(width = 1), vjust = 0.5) +
     labs(y = "States", x = "Sequences")
   ggsave("outputs/nigeria_seq_submitted_bar.jpg", height =  120, width = 200, units = "mm")
   ggsave("outputs/nigeria_seq_submitted_bar.pdf", height =  120, width = 200, units = "mm")
   
   
   # STOPPED HERE ON 19TH MAY 2021 22:30PM    
   
   
     
#####################



#read in data from ncdc.gov.ng
updated_nigeria_ncdc <- read_excel("C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/updated_nigeria_covid19_ncdc.xlsx")
nigeria_ncdc <- as.data.frame(updated_nigeria_ncdc)
nigeria_cases_and_sequences <- left_join(nigeria_ncdc, nigeria_sequences, by = "State")

#Manipulate to include some calculated data
nigeria_cases_and_sequences <- nigeria_cases_and_sequences %>% mutate(Cases_1M = Confirmed/Population, Test_1M = Testing/Population, Positive_1KTests = (Confirmed/Testing)*1000, Sequences_1HCases = (sequences/Confirmed)*100, Recovery_1HCases = (Recoveries/Confirmed)*100, Deaths_1HCases = (Deaths/Confirmed)*100, Projected_cases = (Positive_1KTests/1000)*(Population*1000000))

#for GISAID data set = nigeria_gisaid
#for ncdc data set and sequences = nigeria_cases_and_sequences

map_ng(show.text = TRUE) #to draw Nigeria Map with States boundaries map_np(NULL) give map without State boundaries
#map_ng(states("sw"), show.text = TRUE, col = 4) gives map of South West

#This is to draw Nigeria maps for spatial analysis


#break the cases into categories
bb <- c(0,50, 100,500, 1000, 5000, 10000, 20000, 40000) 
ab <- c(0, 10, 100, 500, 1000, 5000, 10000, 20000, 30000)
dd <- c(0, 5, 10, 50, 100, 200, 260)
pb <- c(0, 2000, 5000, 10000, 25000, 50000, 100000, 200000, 270000) ##breaks for positive cases
ac <- c(0, 5, 50, 100, 1000, 3000, 4000)
ad <- c(0, 500, 1000, 5000, 10000, 20000, 42000)
ae <- c(0, 5, 10, 25, 50, 100, 150) #breaks for Positive per 1K tests
af <- c(0, 25, 50, 75, 100)
ag <- c(0, 1, 3, 5, 10, 20, 30, 40)
pmb <- c(0, 50, 100, 500, 1000, 2000) ##breaks for per million infected 
ppb <- c(0, 2, 5, 10, 15, 20) ##breaks for percentage positive cases
ppt <- c(0, 0.25, 0.5,0.75, 1.0, 3)



#Map showing the distribution of Covid 19 across States in Nigeria as reported by NCDC
pdf("updated_plots/Nigeria_Covid_Cases.pdf")
map_ng(region = nigeria_cases_and_sequences$State, x = nigeria_cases_and_sequences$Confirmed, breaks = bb, col = "blue", title = "COVID-19 distribution in Nigeria ") 
dev.off()

jpeg("updated_plots/Nigeria_Covid_Cases.jpeg")
map_ng(region = nigeria_cases_and_sequences$State, x = nigeria_cases_and_sequences$Confirmed, breaks = bb, col = "blue", title = "COVID-19 distribution in Nigeria ") 
dev.off()
######

#Cases per milion
#Map showing the distribution of Covid 19 across States in Nigeria as reported by NCDC
pdf("updated_plots/Nigeria_Covid_Cases_1M.pdf")
map_ng(region = nigeria_cases_and_sequences$State, x = nigeria_cases_and_sequences$Cases_1M, breaks = ac, col = "red", title = "COVID-19 cases per 1 million population in Nigeria ") 
dev.off()

jpeg("updated_plots/Nigeria_Covid_Cases_1M.jpeg")
map_ng(region = nigeria_cases_and_sequences$State, x = nigeria_cases_and_sequences$Cases_1M, breaks = ac, col = "red", title = "COVID-19 cases per 1 million population in Nigeria ") 
dev.off()
######

##Recoveries
pdf("updated_plots/Nigeria_Covid_recoveries.pdf")
map_ng(region = nigeria_cases_and_sequences$State, x = nigeria_cases_and_sequences$Recoveries, breaks = ab, col = "green", title = "Recoveries from COVID-19 across Nigeria ") 
dev.off()

jpeg("updated_plots/Nigeria_Covid_recoveries.jpeg")
map_ng(region = nigeria_cases_and_sequences$State, x = nigeria_cases_and_sequences$Recoveries, breaks = ab, col = "green", title = "Recoveries from COVID-19 across Nigeria ") 
dev.off()
######

###Recovery per 100 case
##Recoveries
pdf("updated_plots/Nigeria_Covid_recoveries_1Hcases.pdf")
map_ng(region = nigeria_cases_and_sequences$State, x = nigeria_cases_and_sequences$Recovery_1HCases, breaks = af, col = "green", title = "Recoveries from COVID-19 per 100 cases across Nigeria ") 
dev.off()

jpeg("updated_plots/Nigeria_Covid_recoveries_1Hcases.jpeg")
map_ng(region = nigeria_cases_and_sequences$State, x = nigeria_cases_and_sequences$Recovery_1HCases, breaks = af, col = "green", title = "Recoveries from COVID-19 per 100 cases across Nigeria ") 
dev.off()
######

#####Deaths
aj <- c(0, 2, 10, 20, 50, 100, 200, 260)
pdf("updated_plots/Nigeria_Covid_Deaths.pdf")
map_ng(region = nigeria_cases_and_sequences$State, x = nigeria_cases_and_sequences$Deaths, breaks = aj, col = "red", title = "Deaths from COVID-19 across Nigeria ") 
dev.off()

jpeg("updated_plots/Nigeria_Covid_Deaths.jpeg")
map_ng(region = nigeria_cases_and_sequences$State, x = nigeria_cases_and_sequences$Deaths, breaks = aj, col = "red", title = "Deaths from COVID-19 across Nigeria ") 
dev.off()
######
#####Deaths per 100 cases
pdf("updated_plots/Nigeria_Covid_Deaths_1HCases.pdf")
map_ng(region = nigeria_cases_and_sequences$State, x = nigeria_cases_and_sequences$Deaths_1HCases, breaks = ag, col = "red", title = "Deaths per 100 COVID-19 cases across Nigeria ") 
dev.off()

jpeg("updated_plots/Nigeria_Covid_Deaths_1HCases.jpeg")
map_ng(region = nigeria_cases_and_sequences$State, x = nigeria_cases_and_sequences$Deaths_1HCases, breaks = ag, col = "red", title = "Deaths per 100 COVID-19 cases across Nigeria ") 
dev.off()
######

#### Tests
pdf("updated_plots/Nigeria_Covid_Tests.pdf")
map_ng(region = nigeria_cases_and_sequences$State, x = nigeria_cases_and_sequences$Testing, breaks = pb, col = "green", title = "COVID-19 tests conducted across Nigeria ") 
dev.off()

jpeg("updated_plots/Nigeria_Covid_Tests.jpeg")
map_ng(region = nigeria_cases_and_sequences$State, x = nigeria_cases_and_sequences$Testing, breaks = pb, col = "green", title = "COVID-19 tests conducted across Nigeria ") 
dev.off()
######
###Tests per 1 Million population
pdf("updated_plots/Nigeria_Covid_Tests_1M.pdf")
map_ng(region = nigeria_cases_and_sequences$State, x = nigeria_cases_and_sequences$Test_1M, breaks = ad, col = "green", title = "COVID-19 tests per 1 million population across Nigeria ") 
dev.off()

jpeg("updated_plots/Nigeria_Covid_Tests_1M.jpeg")
map_ng(region = nigeria_cases_and_sequences$State, x = nigeria_cases_and_sequences$Test_1M, breaks = ad, col = "green", title = "COVID-19 tests per 1 million population across Nigeria ") 
dev.off()

### Sequences submitted
pdf("updated_plots/Nigeria_Covid_Sequences.pdf")
map_ng(region = nigeria_cases_and_sequences$State, x = nigeria_cases_and_sequences$Testing, breaks = pb, col = "blue", title = "COVID-19 Sequences conducted across Nigeria ") 
dev.off()

jpeg("updated_plots/Nigeria_Covid_Sequences.jpeg")
map_ng(region = nigeria_cases_and_sequences$State, x = nigeria_cases_and_sequences$Testing, breaks = pb, col = "blue", title = "COVID-19 Sequences conducted across Nigeria ") 
dev.off()
#####

##Positive per 1K tests

pdf("updated_plots/Nigeria_Positive_1KTests.pdf")
map_ng(region = nigeria_cases_and_sequences$State, x = nigeria_cases_and_sequences$Positive_1KTests, breaks = ae, col = "red", title = "Positive COVID-19 tests per 1000 tests conducted across Nigeria ") 
dev.off()

jpeg("updated_plots/Nigeria_Positive_1KTests.jpeg")
map_ng(region = nigeria_cases_and_sequences$State, x = nigeria_cases_and_sequences$Positive_1KTests, breaks = ae, col = "red", title = "Positive COVID-19 tests per 1000 tests conducted across Nigeria ") 
dev.off()


##States that submitted sequences
ggplot(nigeria_sequences, aes(y = reorder(State, sequences), x = sequences)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(Percentage, "%")), position = position_dodge(width = 1), vjust = 0.5) +
  labs(title = "SARS-CoV-2 sequences submitted in GISAID from Nigeria", y = "States", x = "Sequences")
ggsave("C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/updated_plots/nigeria_seq_submitted_bar.jpg", height =  120, width = 200, units = "mm")
ggsave("C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/updated_plots/nigeria_seq_submitted_bar.pdf", height =  120, width = 200, units = "mm")


##Sequences submitted per 100 cases
##States that submitted sequences
ggplot(na.omit(nigeria_cases_and_sequences), aes(y = reorder(State, Sequences_1HCases), x = Sequences_1HCases)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = paste0(round(Sequences_1HCases,2), "%")), position = position_dodge(width = 1), vjust = 0.5) +
  labs(title = "Sequences submitted per 100 COVID-19 cases from Nigeria", y = "States", x = "%")
ggsave("C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/updated_plots/nigeria_seq_submitted_per1H_bar.jpg", height =  120, width = 200, units = "mm")
ggsave("C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/updated_plots/nigeria_seq_submitted_per1H_bar.pdf", height =  120, width = 200, units = "mm")



##To plot incidence graph of the top 5 lineages


####Incidences of all the lineages in Nigeria
lineages <- nigeria_gisaid %>% select(pangolin_lineage, date)  %>% group_by(date, pangolin_lineage) %>% summarise (N = n()) %>% mutate(percentage = N/sum(N))

ggplot(lineages, aes(x = date, y = percentage, 
                     fill = pangolin_lineage)) +
  geom_area(size = 0.3, color = 'white') + 
  labs(title = "Incidence of all pango lineages circulating in Nigeria", y = "Percentage", x = "Date")
ggsave("C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/updated_plots/nigeria_incidence_all.jpg", height =  120, width = 200, units = "mm")
ggsave("C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/updated_plots/nigeria_incidence_all.pdf", height =  120, width = 200, units = "mm")


#####

ggplot(nigeria_gisaid, aes(x = age)) + geom_density(fill = "grey") +
  geom_vline(xintercept = (mean_age)) 
ggsave("C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/updated_plots/nigeria_agehist.jpg", height =  120, width = 200, units = "mm")
ggsave("C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/updated_plots/nigeria_agehist.pdf", height =  120, width = 200, units = "mm")


###### Spatial analysis in Nigeria with Tmap
#load the libraries
library(tmap)
library(tmaptools)
library(sf)
library(readxl)
library(dplyr)

# Download the shapefiles for Nigeria from gadm.org
# Read in Nigeria shape file
nigeria_sh <- st_read("C:/Users/YinkaOkoh/Desktop/Workshops/LISA 2020 OAU/gadm36_NGA_shp/gadm36_NGA_1.shp")


#Let Nigeria data be nigeria_data and rename State as NAME_1 since that is what is used in Tmap
nigeria_data <- nigeria_cases_and_sequences %>% rename(NAME_1 = State)

#Join the map data and covid-19 data
nigeria <- left_join(nigeria_sh, nigeria_data, by = "NAME_1")

##Map of COVID-19 Cases as reported by NCDC
nigeria_cases_map <- tm_shape(nigeria) +
  tm_fill(col = "Confirmed", style = "cont", palette = "Reds", title = "COVID-19 cases") +
  tm_borders(col = "black", lwd = 1) +
  tm_text("NAME_1") +
  tm_credits("COVID-19 cases \nacross Nigeria", size = 0.8, position = c(0.6, 0.01))
#tm_layout(frame = FALSE, legend.width = 0.7, legend.position = c(0.85, 0.8), legend.title.size = 0.7) 
tmap_save(nigeria_cases_map, filename = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/updated_plots/Nigeria_Covid_19_cases_map.pdf")
tmap_save(nigeria_cases_map, filename = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/updated_plots/Nigeria_Covid_19_cases_map.jpg")

##Map  of Nigeria Recoveries
nigeria_Recoveries_map <- tm_shape(nigeria) +
  tm_fill(col = "Recoveries", style = "cont", palette = "Blues", title = "Recoveries") +
  tm_borders(col = "black", lwd = 1) +
  tm_text("NAME_1") 
#tm_credits("COVID-19 Recoveries \nacross Nigeria", size = 0.8, position = c(0.6, 0.01))
#tm_layout(frame = FALSE, legend.width = 0.7, legend.position = c(0.85, 0.8), legend.title.size = 0.7) 
tmap_save(nigeria_Recoveries_map, filename = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/updated_plots/Nigeria_Covid_19_Recoveries_map.pdf")
tmap_save(nigeria_Recoveries_map, filename = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/updated_plots/Nigeria_Covid_19_Recoveries_map.jpg")


##Map  of Nigeria Deaths
nigeria_Deaths_map <- tm_shape(nigeria) +
  tm_fill(col = "Deaths", style = "cont", palette = "Reds", title = "Deaths") +
  tm_borders(col = "black", lwd = 1) +
  tm_text("NAME_1") 
#tm_credits("COVID-19 Deaths \nacross Nigeria", size = 0.8, position = c(0.6, 0.01))
#tm_layout(frame = FALSE, legend.width = 0.7, legend.position = c(0.85, 0.8), legend.title.size = 0.7) 
tmap_save(nigeria_Deaths_map, filename = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/updated_plots/Nigeria_Covid_19_Deaths_map.pdf")
tmap_save(nigeria_Deaths_map, filename = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/updated_plots/Nigeria_Covid_19_Deaths_map.jpg")


##Map  of Nigeria Testing
nigeria_Test_map <- tm_shape(nigeria) +
  tm_fill(col = "Testing", style = "cont", palette = "Blues", title = "COVID-19 tests") +
  tm_borders(col = "black", lwd = 1) +
  tm_text("NAME_1") 
#tm_credits("COVID-19 Testing \nacross Nigeria", size = 0.8, position = c(0.6, 0.01))
#tm_layout(frame = FALSE, legend.width = 0.7, legend.position = c(0.85, 0.8), legend.title.size = 0.7) 
tmap_save(nigeria_Test_map, filename = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/updated_plots/Nigeria_Covid_19_Testing_map.pdf")
tmap_save(nigeria_Test_map, filename = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/updated_plots/Nigeria_Covid_19_Testing_map.jpg")



##Map  of Nigeria sequences
nigeria_sequences_map <- tm_shape(nigeria) +
  tm_fill(col = "sequences", style = "cont", palette = "Blues", title = "Sequences in GISAID") +
  tm_borders(col = "black", lwd = 1) +
  tm_text("NAME_1") 
#tm_credits("COVID-19 sequences \nacross Nigeria", size = 0.8, position = c(0.6, 0.01))
#tm_layout(frame = FALSE, legend.width = 0.7, legend.position = c(0.85, 0.8), legend.title.size = 0.7) 
tmap_save(nigeria_sequences_map, filename = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/updated_plots/Nigeria_Covid_19_sequences_map.pdf")
tmap_save(nigeria_sequences_map, filename = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/updated_plots/Nigeria_Covid_19_sequences_map.jpg")



##Map  of Nigeria Cases_1M
nigeria_Cases_1M_map <- tm_shape(nigeria) +
  tm_fill(col = "Cases_1M", style = "cont", palette = "Reds", title = "Cases per million") +
  tm_borders(col = "black", lwd = 1) +
  tm_text("NAME_1") 
#tm_credits("COVID-19 cases per 1M \npopulation across Nigeria", size = 0.8, position = c(0.6, 0.01))
#tm_layout(frame = FALSE, legend.width = 0.7, legend.position = c(0.85, 0.8), legend.title.size = 0.7) 
tmap_save(nigeria_Cases_1M_map, filename = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/updated_plots/Nigeria_Covid_19_Cases_1M_map.pdf")
tmap_save(nigeria_Cases_1M_map, filename = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/updated_plots/Nigeria_Covid_19_Cases_1M_map.jpg")



##Map  of Nigeria Tests_1M
nigeria_Tests_1M_map <- tm_shape(nigeria) +
  tm_fill(col = "Test_1M", style = "cont", palette = "Blues", title = "Tests per million") +
  tm_borders(col = "black", lwd = 1) +
  tm_text("NAME_1") 
#tm_credits("COVID-19 Tests per 1M \npopulation across Nigeria", size = 0.8, position = c(0.6, 0.01))
#tm_layout(frame = FALSE, legend.width = 0.7, legend.position = c(0.85, 0.8), legend.title.size = 0.7) 
tmap_save(nigeria_Tests_1M_map, filename = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/updated_plots/Nigeria_Covid_19_Tests_1M_map.pdf")
tmap_save(nigeria_Tests_1M_map, filename = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/updated_plots/Nigeria_Covid_19_Tests_1M_map.jpg")



##Map  of Nigeria Positive_1KTests
nigeria_Positive_1KTests_map <- tm_shape(nigeria) +
  tm_fill(col = "Positive_1KTests", style = "cont", palette = "Blues", title = "Positive per 1,000 tests") +
  tm_borders(col = "black", lwd = 1) +
  tm_text("NAME_1") 
#tm_credits("COVID-19 Tests per 1M \npopulation across Nigeria", size = 0.8, position = c(0.6, 0.01))
#tm_layout(frame = FALSE, legend.width = 0.7, legend.position = c(0.85, 0.8), legend.title.size = 0.7) 
tmap_save(nigeria_Positive_1KTests_map, filename = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/updated_plots/Nigeria_Covid_19_Positive_1KTests_map.pdf")
tmap_save(nigeria_Positive_1KTests_map, filename = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/updated_plots/Nigeria_Covid_19_Positive_1KTests_map.jpg")


##Map  of Nigeria Sequences_1HCases
nigeria_Sequences_1HCases_map <- tm_shape(nigeria) +
  tm_fill(col = "Sequences_1HCases", style = "cont", palette = "Blues", title = "Sequences per 100 cases") +
  tm_borders(col = "black", lwd = 1) +
  tm_text("NAME_1") 
#tm_credits("COVID-19 Tests per 1M \npopulation across Nigeria", size = 0.8, position = c(0.6, 0.01))
#tm_layout(frame = FALSE, legend.width = 0.7, legend.position = c(0.85, 0.8), legend.title.size = 0.7) 
tmap_save(nigeria_Sequences_1HCases_map, filename = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/updated_plots/Nigeria_Covid_19_Sequences_1HCases_map.pdf")
tmap_save(nigeria_Sequences_1HCases_map, filename = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/updated_plots/Nigeria_Covid_19_Sequences_1HCases_map.jpg")


####Map  of Nigeria Recovery_1HCases
nigeria_Recovery_1HCases_map <- tm_shape(nigeria) +
  tm_fill(col = "Recovery_1HCases", style = "cont", palette = "Blues", title = "Recoveries per 100 cases") +
  tm_borders(col = "black", lwd = 1) +
  tm_text("NAME_1") 
#tm_credits("COVID-19 Tests per 1M \npopulation across Nigeria", size = 0.8, position = c(0.6, 0.01))
#tm_layout(frame = FALSE, legend.width = 0.7, legend.position = c(0.85, 0.8), legend.title.size = 0.7) 
tmap_save(nigeria_Recovery_1HCases_map, filename = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/updated_plots/Nigeria_Covid_19_Recovery_1HCases_map.pdf")
tmap_save(nigeria_Recovery_1HCases_map, filename = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/updated_plots/Nigeria_Covid_19_Recovery_1HCases_map.jpg")


##Map  of Nigeria Deaths_1HCases
nigeria_Deaths_1HCases_map <- tm_shape(nigeria) +
  tm_fill(col = "Deaths_1HCases", style = "cont", palette = "Reds", title = "Deaths per 100 cases") +
  tm_borders(col = "black", lwd = 1) +
  tm_text("NAME_1") 
#tm_credits("COVID-19 Tests per 1M \npopulation across Nigeria", size = 0.8, position = c(0.6, 0.01))
#tm_layout(frame = FALSE, legend.width = 0.7, legend.position = c(0.85, 0.8), legend.title.size = 0.7) 
tmap_save(nigeria_Deaths_1HCases_map, filename = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/updated_plots/Nigeria_Covid_19_Deaths_1HCases_map.pdf")
tmap_save(nigeria_Deaths_1HCases_map, filename = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/updated_plots/Nigeria_Covid_19_Deaths_1HCases_map.jpg")

##Projected COVID-19 Cases in Nigeria based on no of positive tests per 1000 tests

nigeria_projected_cases_map <- tm_shape(nigeria) + 
  tm_fill(col = "Projected_cases", style = "cont", palette = "Reds", title = "Projected COVID-19 Cases in Nigeria") +
  tm_borders(col = "black", lwd = 1) +
  tm_text("NAME_1") 
tmap_save(nigeria_projected_cases_map, filename = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/updated_plots/nigeria_projected_cases_map.pdf")
tmap_save(nigeria_projected_cases_map, filename = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/updated_plots/nigeria_projected_cases_map.jpg")






#####
#Trying out new data downloaded on 22nd Feb 2021 from ourworldindata

new_cases_data <- read.csv("C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/covid-19-global-cases_220221.csv", header = TRUE)
new_cases <- as.data.frame(new_cases_data)
#Select Nigeria data and some required fields
new_cases_nigeria <- new_cases %>% filter (location == "Nigeria") %>% select(location, date, total_cases)

ggplot(new_cases_nigeria, aes(x = as.Date(date), y = total_cases)) +
  geom_line(color = 'red') +
  geom_vline(xintercept = as.Date(c("2020-03-30","2020-07-01", "2020-10-01"))) +
  labs(title = "Cumulative COVID-19 cases in Nigeria", y = "Number of cases", x = "Date")
ggsave("C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/updated_plots/nigeria_cumulative_cases.jpg", height =  120, width = 200, units = "mm")
ggsave("C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/updated_plots/nigeria_cumulative_bar_cases.pdf", height =  120, width = 200, units = "mm")


###For manuscript writing purpose
cases <- nigeria_cases_and_sequences %>% select(State, Confirmed) %>% mutate (Perc = round((Confirmed/sum(Confirmed))*100,2))

ggplot(cases, aes(y = reorder(State, Confirmed), x = Confirmed))+
  geom_text(aes(label = paste0(Perc, "%")), position = position_dodge(width = 1), vjust = 0.5, color = "red") 

cases_1M <- nigeria_cases_and_sequences %>% select(State, Cases_1M) %>% arrange(Cases_1M) %>%  View ()

tests <- nigeria_cases_and_sequences %>% select(State, Testing) %>% arrange(Testing) %>%  View ()

tests_1M <- nigeria_cases_and_sequences %>% select(State, Test_1M) %>% arrange(Test_1M) %>%  View ()

Positive_1KTests <- nigeria_cases_and_sequences %>% select(State, Positive_1KTests) %>% arrange(Positive_1KTests) %>%  View ()

Projected_cases <- nigeria_cases_and_sequences %>% select(State, Projected_cases) %>% arrange(Projected_cases) %>%  View ()

Recoveries <- nigeria_cases_and_sequences %>% select(State, Recoveries) %>% arrange(Recoveries) %>%  View ()

Recovery_1HCases <- nigeria_cases_and_sequences %>% select(State, Recovery_1HCases) %>% arrange(Recovery_1HCases) %>%  View ()

Deaths <- nigeria_cases_and_sequences %>% select(State, Deaths) %>% arrange(Deaths) %>%  View ()

Deaths_1HCases <- nigeria_cases_and_sequences %>% select(State, Deaths_1HCases) %>% arrange(Deaths_1HCases) %>%  View ()

                  


















