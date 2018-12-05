
# load libraries
 
library(rgdal) # open shape file of Germany
library(tidyverse) # data manipulation and visualization
library(purrr) # iterative operations
library(viridis) # color pallete
library(here) # create a file directory
library(extrafont) # load fonts
```

# Opening and Tidying the Elections Files 

path <- here()

files <- list.files(path, pattern = "*_kerg.txt", full.names = TRUE)

all_elections <- map_df(files, read_delim,
                        delim = ";", 
                        skip = 5, # delete the first five rows of each file
                        quote = "\"",
                        col_names = FALSE, 
                        .id = "kerg")


#----tidy elections 2002----#
# create a new vector
even_columns02<-seq(12,60,2) 
# filter by elections 2002
elections_2002 <- all_elections %>%
  filter(kerg == 1) %>%
  dplyr::select(1:3, even_columns02) %>%
  slice(-c(1:3, 15, 16, 24, 25, 32, 33, 63, 64, 67, 68, 79, 80, 91, 92, 105, 106,
           171, 172, 190, 191, 213, 214, 225, 226, 242, 243, 288, 289, 327, 328, 333:335)) %>%
  rename(id = X1, district = X2, SPD = X11, CDU = X13, CSU = X15, 
         Gruenen = X17, FDP = X19) %>%
  # mutate variables to numeric from the 3rd to the 45th column 
  mutate_at(vars(4:19), as.numeric) %>%
  # mutate 2nd column to character
  mutate_at(vars(2), as.integer) %>%
  # create new variables - percentage of the major parties
  mutate(CDU_CSU = rowSums(.[c(5,6)], na.rm = TRUE),
         CDU_CSU_perc = CDU_CSU / rowSums(.[4:19], na.rm = TRUE) * 100,
         SPD_perc = SPD / rowSums(.[4:19], na.rm = TRUE) * 100,
         Gruenen_perc = Gruenen / rowSums(.[4:19], na.rm = TRUE) * 100,
         FDP_perc = FDP / rowSums(.[4:19], na.rm = TRUE) * 100,
         DieLinke_perc = NA,
         AfD_perc = NA) %>%
  # select columns
  dplyr::select(CDU_CSU_perc,SPD_perc, 
                Gruenen_perc, FDP_perc, DieLinke_perc, AfD_perc,
                kerg, id, district)
  
#----tidy elections 2005----#
# create a new vector
odd_columns05<-seq(23,147,4) 
# filter by elections 2005
elections_2005 <- all_elections %>%
  filter(kerg == 2) %>%
  dplyr::select(1:3, odd_columns05) %>%
  slice(-c(1:3, 15, 16, 24, 25, 32, 33, 63, 
           64, 67, 68, 79, 80, 91, 92, 105, 106,
           171, 172, 190, 191, 213, 214, 224, 225, 
           241, 242, 288, 289, 327, 328, 333:335)) %>%
  rename(id = X1, district = X2, SPD = X22, CDU = X26, CSU = X30, 
         Gruenen = X34, FDP = X38, DieLinke = X42) %>%
  # mutate variables to numeric from the 3rd to the 45th column 
  mutate_at(vars(4:35), as.numeric) %>%
  # mutate 2nd column to character
  mutate_at(vars(2), as.integer) %>%
  # create new variables - percentage of the major parties
  mutate(CDU_CSU = rowSums(.[c("CDU", "CSU")], na.rm = TRUE),
         CDU_CSU_perc = CDU_CSU / rowSums(.[4:35], na.rm = TRUE) * 100,
         SPD_perc = SPD / rowSums(.[4:35], na.rm = TRUE) * 100,
         Gruenen_perc = Gruenen / rowSums(.[4:35], na.rm = TRUE) * 100,
         FDP_perc = FDP / rowSums(.[4:35], na.rm = TRUE) * 100,
         DieLinke_perc = NA,
         AfD_perc = NA) %>%
  # select columns
  dplyr::select(CDU_CSU_perc,SPD_perc, 
                Gruenen_perc, FDP_perc, DieLinke_perc, AfD_perc,
                kerg, id, district) 

#----tidy elections 2009----#
# create a new vector
odd_columns09<-seq(23,135,4) 
# filter by elections 2009
elections_2009 <- all_elections %>%
  filter(kerg == 3) %>%
  dplyr::select(1:3, odd_columns09) %>%
  slice(-c(1:3, 15, 16, 24, 25, 32, 33, 64, 
           65, 68, 69, 80, 81, 91, 92, 105, 106,
           171, 172, 189, 190, 212, 213, 223, 224, 
           240, 241, 287, 288, 327, 328, 333:335)) %>%
  rename(id = X1, district = X2, SPD = X22, CDU = X26, FDP = X30, 
         DieLinke = X34, Gruenen = X38, CSU = X42) %>%
  # mutate variables to numeric from the 3rd to the 45th column 
  mutate_at(vars(4:32), as.numeric) %>%
  # mutate 2nd column to character
  mutate_at(vars(2), as.integer) %>%
  # create new variables - percentage of the major parties
  mutate(CDU_CSU = rowSums(.[c("CDU", "CSU")], na.rm = TRUE),
         CDU_CSU_perc = CDU_CSU / rowSums(.[4:32], na.rm = TRUE) * 100,
         SPD_perc = SPD / rowSums(.[4:32], na.rm = TRUE) * 100,
         Gruenen_perc = Gruenen / rowSums(.[4:32], na.rm = TRUE) * 100,
         FDP_perc = FDP / rowSums(.[4:32], na.rm = TRUE) * 100,
         DieLinke_perc = DieLinke / rowSums(.[4:32], na.rm = TRUE) * 100,
         AfD_perc = NA) %>%
  # select columns
  dplyr::select(CDU_CSU_perc,SPD_perc, 
                Gruenen_perc, FDP_perc, DieLinke_perc, AfD_perc,
                kerg, id, district) 




#----tidy elections 2013----#
# create a new vector
odd_columns13<-seq(23,159,4) 
# filter by elections 2005
elections_2013 <- all_elections %>%
  filter(kerg == 4) %>%
  dplyr::select(1:3, odd_columns13) %>%
  slice(-c(1:3, 15, 16, 23, 24, 55, 56, 59, 60, 125, 126, 149, 150,
           166, 167, 206, 207, 253, 254, 259, 260, 273, 274, 285, 286, 293, 294, 311, 312, 
           322, 323, 333:335)) %>%
  rename(id = X1, district = X2, CDU = X22, SPD = X26, FDP = X30, 
         DieLinke = X34, Gruenen = X38, CSU = X42, AfD = X106) %>%
  # mutate variables to numeric from the 3rd to the 45th column 
  mutate_at(vars(4:38), as.numeric) %>%
  # mutate 2nd column to character
  mutate_at(vars(2), as.integer) %>%
  # create new variables - percentage of the major parties
  mutate(CDU_CSU = rowSums(.[c("CDU", "CSU")], na.rm = TRUE),
         CDU_CSU_perc = CDU_CSU / rowSums(.[4:38], na.rm = TRUE) * 100,
         SPD_perc = SPD / rowSums(.[4:38], na.rm = TRUE) * 100,
         Gruenen_perc = Gruenen / rowSums(.[4:38], na.rm = TRUE) * 100,
         FDP_perc = FDP / rowSums(.[4:38], na.rm = TRUE) * 100,
         DieLinke_perc = DieLinke / rowSums(.[4:38], na.rm = TRUE) * 100,
         AfD_perc = AfD / rowSums(.[4:38], na.rm = TRUE) * 100) %>%
  # select columns
  dplyr::select(CDU_CSU_perc,SPD_perc, 
                Gruenen_perc, FDP_perc, DieLinke_perc, AfD_perc,
                kerg, id, district) 

#----tidy elections 2017----#
# create a new vector
odd_columns2017<-seq(23,191,4)
# filter by elections 2017
elections_2017 <- all_elections %>%
  filter(kerg == 5) %>%
  dplyr::select(1:3, odd_columns2017) %>%
  slice(-c(1:3, 15, 16, 23, 24, 31, 32, 63, 64, 67, 68, 79, 80, 90, 91, 104, 105,
           170, 171, 188, 189, 212, 213, 222, 223, 239, 240, 287, 288, 327, 328, 333:335)) %>%
  rename(id = X1, district = X2, CDU = X22, SPD = X26, DieLinke = X30, 
         Gruenen = X34, CSU = X38, FDP = X42, AfD = X46) %>%
  # mutate variables to numeric from the 3rd to the 45th column 
  mutate_at(vars(4:46), as.numeric) %>%
  # mutate 2nd column to character
  mutate_at(vars(2), as.integer) %>%
  # create the CDU/CSU variable and all the percentage values of each party for each state 
  mutate(CDU_CSU = rowSums(.[c("CDU", "CSU")], na.rm = TRUE),
         CDU_CSU_perc = CDU_CSU / rowSums(.[4:46], na.rm = TRUE) * 100,
         SPD_perc = SPD / rowSums(.[4:46], na.rm = TRUE) * 100,
         DieLinke_perc = DieLinke / rowSums(.[4:46], na.rm = TRUE) * 100,
         Gruenen_perc = Gruenen / rowSums(.[4:46], na.rm = TRUE) * 100,
         FDP_perc = FDP / rowSums(.[4:46], na.rm = TRUE) * 100,
         AfD_perc = AfD / rowSums(.[4:46], na.rm = TRUE) * 100) %>%
  # reorder and delete columns 10 to 45
  dplyr::select(CDU_CSU_perc,SPD_perc, 
                Gruenen_perc, FDP_perc, DieLinke_perc, AfD_perc,
                kerg, id, district) 


#------Bind Rows of all elections----
elections_21st <- do.call("rbind", list(elections_2002,
                                        elections_2005,
                                        elections_2009,
                                        elections_2013,
                                        elections_2017))



# create variable year
elections_21st <- elections_21st %>%
  mutate(year = case_when(kerg == 1 ~ 2002,
                          kerg == 2 ~ 2005,
                          kerg == 3 ~ 2009,
                          kerg == 4 ~ 2013,
                          kerg == 5 ~ 2017))

# explore dataset
elections_21st %>%
  group_by(year) %>%
  summarize(CDU = mean(CDU_CSU_perc, na.rm = TRUE),
            SPD = mean(SPD_perc, na.rm = TRUE),
            Gruenen = mean(Gruenen_perc, na.rm = TRUE),
            FDP = mean(FDP_perc, na.rm = TRUE),
            DieLinke = mean(DieLinke_perc, na.rm = TRUE),
            AfD = mean(AfD_perc, na.rm = TRUE))



## Spatial Data of Germany
# map of germany
# open shape file germany with districts

germany_map <- readOGR(here("geometrie_Wahlkreise_19DBT_geo.shp"),
                       use_iconv = TRUE, 
                       encoding = "UTF-8")

# plot file
plot(germany_map)

# transform in a dataframe and create column id
germany_new <- germany_map@data%>% 
  as.data.frame() %>% 
  rownames_to_column("id")

# fortify the dataframe
germany_map <- fortify(germany_map) %>%
  left_join(germany_new, by = "id")


# join map dataframe with elections dataframe by district variable
germany_df <- elections_21st %>%
  left_join(germany_map, by = c("id" = "wkr_nr")) %>% 
  glimpse()
```

## Plot the Results of the German Elections

for(i in 1:6){
  colsnames <- c("CDU", "SPD", "GRUENEN",
                 "FDP", "Die Linke", "AfD")
  plot <- ggplot(germany_df, aes(x = long, y = lat, 
                                 group = group)) +
    geom_polygon(aes(fill = germany_df[[i]])) + 
    scale_fill_viridis(
      option = "magma",
      direction = -1,
      name = paste(colsnames[i], "(%)", sep = ""),
      guide = guide_colorbar(
        direction = "horizontal",
        barheight = unit(2, units = "mm"),
        barwidth = unit(50, units = "mm"),
        draw.ulim = F,
        title.position = 'top',
        title.hjust = 0.5,
        label.hjust = 0.5)) + 
    theme(plot.title = element_text(family = "Agency FB", 
                                    color = "black", 
                                    size = 24, 
                                    hjust = 0),
          plot.subtitle = element_text(family ="Agency FB", color = "black", 
                                       size = 20, 
                                       hjust = 0), 
          plot.caption = element_text(family = "Agency FB", 
                                      color = "black", 
                                      size = 14, 
                                      hjust = 0),
          plot.background = element_rect(fill = "#D8D8D8", 
                                         color = NA), 
          panel.background = element_rect(fill = "#D8D8D8", 
                                          color = NA),
          legend.background = element_rect(fill = "#D8D8D8", 
                                           color = NA),
          legend.text = element_text(family = "Agency FB", 
                                     size = 16, color = "black"),
          legend.title = element_text(family = "Agency FB", 
                                      size = 16, color = "black"),
          axis.line = element_blank(), 
          axis.ticks = element_blank(),
          panel.grid = element_blank(), 
          axis.title = element_blank(),
          axis.text = element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(color = "black", size = 16),
          legend.position = "bottom") +
    coord_map() +
    facet_grid(.~year) +
    labs(title = "German Federal Election", 
         subtitle = paste("Share of the ",
                          colsnames[i], " Vote (%)", sep=""),
         caption = "Source: Federal Returning Office")
  
  print(plot)
  
}


