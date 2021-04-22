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


#   Read in  the file as raw_data
raw_data<- read_tsv("raw_data/Nigeria_gisaid_hcov-19_2021_04_19_15.tsv", col_names = TRUE,
                    col_types = cols(strain ="f", virus = "c", gisaid_epi_isl = "-",
                                     genbank_accession = "-", date = "D", region = "f",
                                     country = "c",  division = "c", location = "c",
                                     region_exposure = "c", country_exposure = "c", division_exposure = "c",
                                     segment = "c", host = "f", age = "i", sex = "f",
                                     Nextstrain_clade = "f", pangolin_lineage = "f", GISAID_clade = "f",
                                     originating_lab = "c", submitting_lab = "c", authors = "-", url = "-", title = "-",
                                     paper_url = "-",date_submitted = "D" 
                    ))


#Change file to nigeria_gisaid and select some columns to use for the analysis
nigeria_gisaid <- raw_data %>% select(-c(`Virus name`, `Accession ID`, Comment, `Comment type`, `Additional host information`, Passage, Host)) %>% as.data.frame()


# Exploratory analysis
dim(nigeria_gisaid) # 469 sequence submitted

# Get the number of sequences submitted from each state
nigeria_sequences <- nigeria_gisaid %>% select(Location) %>% group_by(Location) %>% summarize (sequences = n()) %>% mutate(Percentage = round((sequences/sum(sequences)*100),1))

# There is no consistency in the naming of the states, so add a new column state and group by the new column
nigeria_sequences$State<- c("Unknown", "Federal Capital Territory", "Adamawa", "Akwa Ibom", "Akwa Ibom", "Benue", "Borno", "Delta", "Ebonyi", "Ebonyi", "Edo", "Edo", "Ekiti", "Ekiti", "Enugu", "Federal Capital Territory",  "Federal Capital Territory", "Kaduna", "Kano", "Kogi", "Kwara", "Lagos","Lagos", "Nassarawa", "Niger", "Ogun", "Ondo", "Ondo", "Osun", "Osun", "Oyo", "Oyo", "Plateau", "Rivers", "Zamfara")
nigeria_sequences <- nigeria_sequences %>% group_by(State) %>% summarise(sequences = sum(sequences)) %>% mutate(Percentage = round((sequences/sum(sequences))*100, 2)) %>% select(State, sequences, Percentage) %>% arrange(desc(sequences))

#   Export the tables as Sequences submitted to gisaid from each Nigeria State
write.csv(nigeria_sequences, "outputs/Submitted_Sequences.csv")

#   Of the 469 sequences submitted from Nigeria, 2 could not be traced to any state. 

#read in data from covid19.ncdc.gov.ng
updated_nigeria_ncdc <- read_excel("raw_data/nig_covid19_data_wk_14_2021.xlsx")
nigeria_ncdc <- as.data.frame(updated_nigeria_ncdc)

# Merge the data from gisaid and ncdc together
nigeria_cases_and_sequences <- left_join(nigeria_ncdc, nigeria_sequences, by = "State")

# Manipulate to include some calculated data
nigeria_cases_and_sequences <- nigeria_cases_and_sequences %>% mutate(Cases_1M = Confirmed/Population, Test_1M = Tests/Population, Positive_1KTests = (Confirmed/Tests)*1000, Sequences_1HCases = (sequences/Confirmed)*100, Recovery_1HCases = (Recoveries/Confirmed)*100, Deaths_1HCases = (Deaths/Confirmed)*100, Projected_cases = (Positive_1KTests/1000)*(Population*1000000))

#  GISAID dataset = nigeria_gisaid
#  ncdc dataset and sequences = nigeria_cases_and_sequences

#State without sequences in gisaid 
sum(is.na(nigeria_cases_and_sequences$sequences)) # 13 = Abia, Anambra, Bauchi, Bayelsa, C. Rivers, Gombe, Imo, Jigawa, Katsina, Kebbi, Sokoto, Taraba, Yobe 

#lineages circulating in Nigeria are 25
length(unique(nigeria_gisaid$Lineage)) # They are 25

# Analyze the the SARS-CoV2 lineages in Nigeria
Lineages <- nigeria_gisaid %>% group_by(Lineage) %>% summarize(N = n()) %>% mutate (Percentage = round((N/sum(N))*100,1)) %>% arrange(desc(N))
write.csv(Lineages, "outputs/Lineages.csv")

# Analyze the GISAID clades in Nigeria
GISAID_clade <- nigeria_gisaid %>% group_by(Clade) %>% summarize(N = n()) %>% mutate (Percentage = round((N/sum(N))*100,1)) %>% arrange(desc(N))
write.csv(GISAID_clade, "outputs/Clades.csv")

# Analyze the Sequencing Technology used in Nigeria
Sequencing_Technology <- nigeria_gisaid %>% group_by(`Sequencing technology`) %>% summarize(N = n()) %>% mutate (Percentage = round((N/sum(N))*100,1)) %>% arrange(desc(N))
write.csv(Sequencing_Technology, "outputs/Sequencing_Tech.csv")


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



#   To plot lock down date
#   Date not included in ncdc data. It is only included in owid data which is not available yet
# nigeria_date <- nigeria_gisaid %>% select(strain, date) %>% group_by(date) %>% summarize(N = n())

#Plot a chart 
# ggplot(nigeria_date, aes(x = date, y = cumsum(N))) +
#  geom_line(color = 'red') +
#  geom_vline(xintercept = as.Date(c("2020-03-30","2020-07-01", "2020-10-01"))) +
#  labs(title = "Cumulative SARS-CoV-2 sequences in Nigeria", y = "Number of sequences", x = "Date")
# ggsave("outputs/nigeria_cumulative.jpg", height =  120, width = 200, units = "mm")
# ggsave("outputs/nigeria_cumulative_bar.pdf", height =  120, width = 200, units = "mm")


# To plot incidence graph of the top 5 lineages
# No need for this any more
# Top_lineages <- nigeria_gisaid %>% select(pangolin_lineage, date)  %>% filter(pangolin_lineage == c("B.1", "B.1.1", "B.1.246", "B.1.247")) %>% group_by(date, pangolin_lineage) %>% summarise (N = n()) %>% mutate(percentage = N/sum(N))

# ggplot(Top_lineages, aes(x = date, y = percentage, 
#                       fill = pangolin_lineage)) +
#  geom_area(size = 0.3, color = 'white') + 
#  labs(title = "Incidence of top pango lineages circulating in Nigeria", y = "Percentage", x = "Date")
#   ggsave("outputs/nigeria_incidence.jpg", height =  120, width = 200, units = "mm")
#   ggsave("outputs/nigeria_incidence_bar.pdf", height =  120, width = 200, units = "mm")

####Incidences of all the lineages in Nigeria
# lineages <- nigeria_gisaid %>% select(pangolin_lineage, date)  %>% group_by(date, pangolin_lineage) %>% summarise (N = n()) %>% mutate(percentage = N/sum(N))

# ggplot(lineages, aes(x = date, y = percentage, 
#                    fill = pangolin_lineage)) +
#  geom_area(size = 0.3, color = 'white') + 
#  labs(title = "Incidence of all pango lineages circulating in Nigeria", y = "Percentage", x = "Date")
# ggsave("outputs/nigeria_incidence_all.jpg", height =  120, width = 200, units = "mm")
# ggsave("outputs/nigeria_incidence_all.pdf", height =  120, width = 200, units = "mm")




#   OWID Data

# raw_data<- read_tsv("C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/updatedSARCoV2_project/metadata_2021-01-06_18-26.tsv.zip", col_names = TRUE,
#                  col_types = cols(strain ="f", virus = "c", gisaid_epi_isl = "-",
#                                  genbank_accession = "-", date = "D", region = "f",
#                                 country = "c",  division = "c", location = "c",
#                                region_exposure = "c", country_exposure = "c", division_exposure = "c",
#                               segment = "c", host = "f", age = "i", sex = "f",
#                              Nextstrain_clade = "f", pangolin_lineage = "f", GISAID_clade = "f",
#                             originating_lab = "c", submitting_lab = "c", authors = "-", url = "-", title = "-",
#                            paper_url = "-",date_submitted = "D" 
#          ))

#filter Nigeria data
#   nigeria_gisaid <- raw_data %>% filter(country == "Nigeria") %>% select(-c(virus, region, country, location, region_exposure, country_exposure, division_exposure, segment, host, originating_lab, submitting_lab)) %>% as.data.frame()

#Exploratory analysis
#dim(nigeria_gisaid) # 223 sequence submitted
#table(nigeria_gisaid$sex) # 109 males, 57 females, 57 persons undeclared sex
#sum(is.na(nigeria_gisaid$age)) # 69 persons without age
#range(na.omit(nigeria_gisaid$age)) # 4 - 98
#summary(nigeria_gisaid$age) # Min 4.0, 1st qd 30.0, Median 40.50, Mean 41.69, 3rd qd 51.75, Max 98, NA 
#mean_age <- mean(na.omit(nigeria_gisaid$age)) #41.69
#median_age <- median(na.omit(nigeria_gisaid$age)) #41.69
#range(nigeria_gisaid$length) #28,259 - 29,903 is the range of the sequence length
#get the number of sequences submitted from each state
#nigeria_sequences <- nigeria_gisaid %>% select(division, strain) %>% group_by(division) %>% summarize (sequences = n()) %>% mutate(Percentage = round((sequences/sum(sequences)*100),1))
#rename the state for consistency
#nigeria_sequences$State<- c("Federal Capital Territory", "Akwa Ibom", "Benue", "Borno", "Delta", "Ebonyi", "Edo", "Ekiti", "Enugu", "Federal Capital Territory", "Kaduna", "Kano", "Katsina", "Kogi", "Kwara", "Lagos", "Nassarawa", "Niger", "", "Ogun", "Ondo", "Osun", "Oyo", "Rivers")
# 2 sequences were tagged Nigeria. The actual state is/are unknown
#remove the two state sequences without state, also remove the column name Federal Capital Territory, since we also have Abuja
#nigeria_sequences <- nigeria_sequences[-c(10, 19),]

#add the sequence of FCT to Abua
#nigeria_sequences[nigeria_sequences$State == "Federal Capital Territory", "sequences"] <- 25
#nigeria_sequences[nigeria_sequences$division == "Abuja", "State"] <- "Federal Capital Territory"

#remove the division column
#nigeria_sequences <- nigeria_sequences %>% select(-division)



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




