# blog post - Using R to analyse the German Federal Election

# libraries needed

library(tidyverse) # load packages related to data cleaning (e.g. dplyr) and data visualization(ggplot2)
library(rgdal) # load shape files
library(broom) # transform shape files in data frames
library(readxl) # load excel files
library(here) # cretes a path to the current directory
library(extrafont) # add new fonts

# load file
germany_elections_untidy <- read_excel(here("btw17_kerg.xlsx"), 
                                       skip = 5) 

# check file
glimpse(germany_elections_untidy)

# create two new vectors
even_columns<-seq(20,190,2) 
odd_columns<-seq(3,87,2) 

# clean the German Federal Election data frame

germany_tidy <- germany_elections_untidy %>%
  # select columns of interest
  select(1:2, even_columns) %>%
  # select rows that correspond to the 16 States of Germany
  slice(c(14, 22, 30, 62, 66, 78, 89, 103, 169, 187, 211, 221, 238, 286, 326, 332)) %>%
  # rename columns' names - "new name" = "old name"
  rename(state = Gebiet, CDU = X__14, SPD = X__17, DieLinke = X__20, 
         Gruenen = X__23, CSU = X__26, FDP = X__29, AfD = X__32, 
         Piraten = X__35, NPD = X__38, FW = X__41, PMUT = X__44, 
         ODP = X__47, PArbeit = X__50, BP = X__53, ADV = X__56,
         PdV = X__59, MLPD = X__62, BS = X__65, SP = X__68, 
         DR = X__71, ADD = X__74, AMTS = X__77, BergP = X__80, 
         BG = X__83, DBewe = X__86, DKP = X__89, DMitte = X__92, 
         Grauen = X__95, UrbaneHipHop = X__98, MPartei = X__101, 
         MWelt = X__104, PH = X__107, PGesund = X__110, 
         VPartei = X__113, BCD = X__116, Einheit = X__119, 
         Violetten = X__122, FamilienP = X__125, 
         FPDF = X__128, Mieter = X__131, NLiberale = X__134, 
         UBD = X__137, U = X__140) %>%
  # delete columns
  select(-odd_columns) %>%
  # mutate variables to numeric from the 3rd to the 45th column 
  mutate_at(vars(3:45), as.numeric) %>%
  # mutate 1st column to character
  mutate_at(vars(1), as.character) %>%
  # create the CDU/CSU variable and all the percentage values of each party for each state 
  mutate(CDU_CSU = rowSums(.[c("CDU", "CSU")], na.rm = TRUE),
         CDU_CSU_perc = CDU_CSU / rowSums(.[3:45], na.rm = TRUE) * 100,
         SPD_perc = SPD / rowSums(.[3:45], na.rm = TRUE) * 100,
         DieLinke_perc = DieLinke / rowSums(.[3:45], na.rm = TRUE) * 100,
         Gruenen_perc = Gruenen / rowSums(.[3:45], na.rm = TRUE) * 100,
         FDP_perc = FDP / rowSums(.[3:45], na.rm = TRUE) * 100,
         AfD_perc = AfD / rowSums(.[3:45], na.rm = TRUE) * 100) %>%
  # reorder and delete columns 10 to 45
  select(Nr, state, CDU_CSU_perc,SPD_perc,
         DieLinke_perc, Gruenen_perc, FDP_perc, 
         AfD_perc, CDU_CSU, everything(), -10:-45) 

# recode variable NR - "old value" = "new value"
Nr_recode <- c("10" = "11", "9" = "1", "1" = "14", "4" = "4", 
               "13" = "7", "8" = "0","5" = "9", "6" = "6", "12" = "3", 
               "7" = "10", "14" = "12", "11" = "2", "15" = "13",
               "2" = "5", "16" = "15", "3" = "8") 

# assign new values to the Nr variable
germany_tidy$Nr <- Nr_recode[germany_tidy$Nr] 

# load shape file
germany <- readOGR(here(layer = "DEU_adm1.shp"), use_iconv = TRUE, encoding = "UTF-8") 

plot(germany)

# prepare the shapefile with the map of germany to make it possible to join the dataframe of german elections
germany_states <- germany@data$NAME_1
germany@data$germany_states <- germany_states 

# filter data for germany states
elections_data <- germany_tidy %>% 
  filter(state %in% germany_states) 

# join datasets
germany@data <- germany@data %>% 
  left_join(elections_data,by = c("germany_states" = "state")) 

# important step for the indexes to join
rownames(germany@data) <- germany@data$germany_states 

# transform the previous object in a dataframe
germany_dataframe <- tidy(germany) %>% 
  left_join(., germany@data, by = c("id" = "Nr")) 

#------------plots-----------------#

# plot
plot_basic_CDU_CSU <- germany_dataframe %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  # i need to add this filter so that the Berlin 
  # and Bremen results can be mapped and 
  # distinguished from the Brandeburg and Niedersachsen results, respectively
  geom_polygon(aes(fill=CDU_CSU_perc), color = "white", 
               data =filter(germany_dataframe, !NAME_1 %in% c("Berlin", "Bremen"))) + 
  geom_polygon(aes(fill=CDU_CSU_perc), color = "white", 
               data =filter(germany_dataframe, NAME_1 %in%  c("Berlin", "Bremen"))) + 
  theme_minimal() +
  coord_map()

plot_basic_CDU_CSU

# creation of a common theme for all plots
common_theme <- theme(title = element_text(family = "Cambria", size = 12, hjust = 0.5),
                      plot.title = element_text(family = "Cambria", 
                                                color = "#22211d", 
                                                size = 20, 
                                                hjust = 0),
                      plot.subtitle = element_text(family = "Cambria", 
                                                   face = "italic", 
                                                   color = "#22211d", 
                                                   size = 14, 
                                                   hjust = 0), 
                      plot.caption = element_text(family = "Cambria", 
                                                  color = "#22211d", 
                                                  size = 10, 
                                                  hjust = 0),
                      plot.background = element_rect(fill = "#f5f5f2", 
                                                     color = NA), 
                      panel.background = element_rect(fill = "#f5f5f2", 
                                                      color = NA),
                      legend.background = element_rect(fill = "#f5f5f2", 
                                                       color = NA),
                      legend.text = element_text(family = "Cambria", 
                                                 size = 10),
                      axis.line = element_blank(), 
                      axis.ticks = element_blank(),
                      panel.grid = element_blank(), 
                      axis.title = element_blank(),
                      axis.text = element_blank(),
                      legend.position = "right")

# create a common legend design - given that the dependent variable is continous, I'll use guide_colorbar
guide_legend <- guide_colorbar(
  direction = "horizontal",
  barheight = unit(2, units = "mm"),
  barwidth = unit(50, units = "mm"),
  draw.ulim = F,
  title.position = 'top',
  # adjust the position of the label around
  title.hjust = 0.5,
  label.hjust = 0.5)

#---plot cdu_csu---#
plot_CDU_CSU <- germany_dataframe %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill=CDU_CSU_perc), 
               color = "#7F7F7F", 
               data =filter(germany_dataframe, 
                            !NAME_1 %in% c("Berlin", "Bremen"))) + 
  geom_polygon(aes(fill=CDU_CSU_perc), 
               color = "#7F7F7F", 
               data =filter(germany_dataframe, 
                            NAME_1 %in%  c("Berlin", "Bremen"))) +
  coord_map() +
  labs(title = "German Federal Election 2017", 
       subtitle = "Share of the CDU/CSU Vote (%)", 
       caption = "Source: Federal Returning Office" ) + 
  common_theme +
  scale_fill_gradient2(
    low = "#F2F2F2",
    mid= "#A4A4A4",
    high = "#000000",
    midpoint = 30.5,
    name = "Share of Vote (%)",
    limits = c(22,40), 
    breaks = c(22, 25, 28, 31, 34, 37, 40),
    guide = guide_legend) 

plot_CDU_CSU

#---plot spd---#
plot_SPD <- germany_dataframe %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill=SPD_perc), 
               color = "#7F7F7F", 
               data =filter(germany_dataframe, 
                            !NAME_1 %in% c("Berlin", "Bremen"))) + 
  geom_polygon(aes(fill=SPD_perc), 
               color = "#7F7F7F", 
               data =filter(germany_dataframe, 
                            NAME_1 %in%  c("Berlin", "Bremen"))) + 
  coord_map() +
  labs(title = "German Federal Election 2017", 
       subtitle = "Share of the SPD Vote (%)", 
       caption = "Source: Federal Returning Office" ) + 
  common_theme +
  scale_fill_gradient2(
    low = "#FBEFEF",
    mid= "#F78181",
    high = "#E3000F",
    midpoint = 19,
    name = "Share of Vote (%)",
    limits = c(10,28), 
    breaks = c(10, 13, 16, 19, 22, 25, 28),
    guide = guide_legend) 

plot_SPD

#---plot fdp---#
plot_FDP <- germany_dataframe %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill=FDP_perc), 
               color = "#7F7F7F", 
               data =filter(germany_dataframe, 
                            !NAME_1 %in% c("Berlin", "Bremen"))) + 
  geom_polygon(aes(fill=FDP_perc), 
               color = "#7F7F7F", 
               data =filter(germany_dataframe, 
                            NAME_1 %in%  c("Berlin", "Bremen"))) + 
  coord_map() +
  labs(title = "German Federal Election 2017", 
       subtitle = "Share of the FDP Vote (%)", 
       caption = "Source: Federal Returning Office" ) + 
  common_theme +
  scale_fill_gradient2(
    low = "#F5F6CE",
    mid= "#F3F781",
    high = "#FFC300",
    midpoint = 9,
    name = "Share of Vote (%)",
    limits = c(6,14), 
    breaks = c(6, 8, 10, 12, 14),
    guide = guide_legend) 

plot_FDP 

#---plot gruenen---#
plot_Gruenen <- germany_dataframe %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill=Gruenen_perc), 
               color = "#7F7F7F", 
               data =filter(germany_dataframe, 
                            !NAME_1 %in% c("Berlin", "Bremen"))) + 
  geom_polygon(aes(fill=Gruenen_perc), 
               color = "#7F7F7F", 
               data =filter(germany_dataframe, 
                            NAME_1 %in%  c("Berlin", "Bremen"))) + 
  coord_map() +
  labs(title = "German Federal Election 2017", 
       subtitle = "Share of the Gruenen Vote (%)", 
       caption = "Source: Federal Returning Office" ) + 
  common_theme +
  scale_fill_gradient2(
    low = "#D8F6CE",
    mid= "#82FA58",
    high = "#19A229",
    midpoint = 8,
    name = "Share of Vote (%)",
    limits = c(3,14), 
    breaks = c(3, 5, 7, 9, 11, 14),
    guide = guide_legend) 

plot_Gruenen

#---plot dielinke---#
plot_DieLinke <- germany_dataframe %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill=DieLinke_perc), 
               color = "#7F7F7F", 
               data =filter(germany_dataframe, 
                            !NAME_1 %in% c("Berlin", "Bremen"))) + 
  geom_polygon(aes(fill=DieLinke_perc), 
               color = "#7F7F7F", 
               data =filter(germany_dataframe, 
                            NAME_1 %in%  c("Berlin", "Bremen"))) +
  coord_map() +
  labs(title = "German Federal Election 2017", 
       subtitle = "Share of the Die Linke  Vote (%)", 
       caption = "Source: Federal Returning Office" ) + 
  common_theme +
  theme(legend.position = "right") +
  scale_fill_gradient2(
    low = "#ECCEF5",
    mid= "#DA81F5",
    high = "#B3315D",
    midpoint = 12.5,
    name = "Share of Vote (%)",
    limits = c(6,19), 
    breaks = c(5, 7, 9, 11, 13, 15, 17, 19),
    guide = guide_legend) 

plot_DieLinke 

#---plot afd---#
plot_AfD <- germany_dataframe %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill=AfD_perc), 
               color = "#7F7F7F", 
               data =filter(germany_dataframe, !NAME_1 %in% c("Berlin", "Bremen"))) + 
  geom_polygon(aes(fill=AfD_perc), 
               color = "#7F7F7F", 
               data =filter(germany_dataframe, NAME_1 %in%  c("Berlin", "Bremen"))) + 
  coord_map() +
  labs(title = "German Federal Election 2017", 
       subtitle = "Share of the AfD Vote (%)", 
       caption = "Source: Federal Returning Office" ) + 
  common_theme +
  scale_fill_gradient2(
    low = "#EFFBFB",
    mid= "#A9D0F5",
    high = "#009DE0",
    midpoint = 17.5,
    name = "Share of the AfD Vote(%)",
    limits = c(7,28), 
    breaks = c(7, 10, 13, 16, 19, 22, 25, 28),
    guide = guide_legend)

plot_AfD