library(tidyverse)
data <- read_csv("2021-06-30Book.csv")
summary(data)
data <- data %>% group_by(Station, Polymer, Depth_label, Timepoint) %>% mutate(co2_average =  mean(CO2_in_bottle_umol)) %>% 
                 mutate(co2_stdev = sd(CO2_in_bottle_umol) ) %>% 
                 mutate(mean_delta_13C = mean(delta_13C)) %>% 
                 mutate(sd_delta_13c = sd(delta_13C)) %>% ungroup()
write_excel_csv(data, file = "./book_avg_sd.csv")
data <- read_csv("./book_avg_sd.csv")
data_1 <- data %>% select(Station, Polymer, Depth_label, Timepoint, co2_average, 
                        co2_stdev, mean_delta_13C, sd_delta_13c) %>% distinct() 

write_excel_csv(data_1, file = "./book_avg_sd_no_replicates.csv")

t <- read_csv("book_avg_sd_no_replicates.csv")

Fraction_13C <- function(x) (0.0111796016761313*((x/(1000)+1)))/((0.0111796016761313*((x/(1000)+1)))+1)
RVPDB_13 <- 0.011056/0.988944 
  
t <- t %>% mutate(F_13C = Fraction_13C(mean_delta_13C)) %>% mutate(CO2_mmol = co2_average/1000) %>% 
  mutate( atom_perct_13C = F_13C*100 )
background_t <- t %>% filter(Polymer == "background")

t <- read_csv("book_avg_sd_no_replicates_some_calc.csv")

stations <- unique(background_t$Station)
timepoints <- unique(background_t$Timepoint)

t <- t %>% add_column(C13O2_atom_BG = NA)

for (i in 1:nrow(t)) {
  for (y in 1:nrow(background_t)) { 
    if ( t[["Station"]][[i]]==background_t[["Station"]][[y]] && t[["Timepoint"]][[i]]==background_t[["Timepoint"]][[y]]) 
      {t[["C13O2_atom_BG"]][[i]] <- background_t[["atom_perct_13C"]][[y]]}
    }
}

t <- t %>% add_column(D13C_CO2_perct = NA)

for (i in 1:nrow(t)) {
  t[["D13C_CO2_perct"]][[i]] <- (t[["atom_perct_13C"]][[i]]-t[["C13O2_atom_BG"]][[i]])
}

t <- t %>% add_column(prod_13CO2_mmol= NA)


for (i in 1:nrow(t)) {
  t[["prod_13CO2_mmol"]][[i]] <- (t[["CO2_mmol"]][[i]]/100*t[["D13C_CO2_perct"]][[i]])
}


t1 <- t %>% filter(Polymer %in% c("PP","PE")) %>% filter(Station %in% c("st04")) %>% add_column(net_prod_13CO2_mmol= NA)
write_excel_csv(t1,"t1.csv")

for( i in 1:nrow(t1)){
  for (y in unique(t1$Station) ) {
    for (z in unique(t1$Polymer)) {
        if (length(which(t1[["Station"]][[i]]==y & t1[["Polymer"]][[i]]==z &  t1[["Timepoint"]]=="t0" ))>0 && !is.na(t1[["prod_13CO2_mmol"]])) {

              t1[["net_prod_13CO2_mmol"]][[i]] <- 
              t1[["prod_13CO2_mmol"]][[i]]-t1[["prod_13CO2_mmol"]][[which(t1[["Station"]]==y & t1[["Polymer"]]==z & t1[["Timepoint"]]=="t0")]]        
      
         
        }
      }
    } 
  }

