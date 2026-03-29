library(growthrates)
library(ggplot2)
library(tidyr)
library(lubridate)
library(magick)
library(dplyr)
library(patchwork)
library(ggpubr)

setwd("/Users/lishijia/Documents/simulations/mono-species cultivation/ODs")
no_bac = read.csv(file = "NO-bacteria_ODs.csv",header = T,sep = ',',check.names = F)
no_bac_od = rowMeans(no_bac[,2:ncol(no_bac)],na.rm = T)

B.fragilis = read.csv(file = "B.fragilis_raw_OD.csv",header = T,sep = ",",check.names = F) %>%
  mutate(across(everything(),~ . - first(.)))
B.fragilis = as.data.frame(lapply(B.fragilis,function(x) gsub(",",".",x)),check.names = F)
B.fragilis = as.data.frame(lapply(B.fragilis,as.numeric),check.names = F)
B.fragilis[B.fragilis <= 0] = 1e-4
B.fragilis_long = B.fragilis %>%
  pivot_longer(cols = -`Time (h)`,names_to = 'Condition',values_to = "OD") %>%
  mutate(Carbon = sub("-\\d+$","",Condition)) %>%
  group_by(`Time (h)`,Carbon) %>%
  summarise(mean_OD = mean(OD,na.rm = TRUE),
            sd_OD = sd(OD,na.rm = TRUE),
            .groups = "drop")

B.breve = read.csv(file = "B.breve_raw_OD.csv",header = T,sep = ",",check.names = F) %>%
  mutate(across(everything(),~ . - first(.)))
B.breve = as.data.frame(lapply(B.breve,function(x) gsub(",",".",x)),check.names = F)
B.breve = as.data.frame(lapply(B.breve,as.numeric),check.names = F)
B.breve[B.breve <= 0] = 1e-4
B.breve_long = B.breve %>%
  pivot_longer(cols = -`Time (h)`,names_to = 'Condition',values_to = "OD") %>%
  mutate(Carbon = sub("-\\d+$","",Condition)) %>%
  group_by(`Time (h)`,Carbon) %>%
  summarise(mean_OD = mean(OD,na.rm = TRUE),
            sd_OD = sd(OD,na.rm = TRUE),
            .groups = "drop")

B.bifidum = read.csv(file = "B.bifidum_raw_OD.csv",header = T,sep = ",",check.names = F) %>%
  mutate(across(everything(),~ . - first(.)))
B.bifidum = as.data.frame(lapply(B.bifidum,function(x) gsub(",",".",x)),check.names = F)
B.bifidum = as.data.frame(lapply(B.bifidum,as.numeric),check.names = F)
B.bifidum[B.bifidum <= 0] = 1e-4
B.bifidum_long = B.bifidum %>%
  pivot_longer(cols = -`Time (h)`,names_to = 'Condition',values_to = "OD") %>%
  mutate(Carbon = sub("-\\d+$","",Condition)) %>%
  group_by(`Time (h)`,Carbon) %>%
  summarise(mean_OD = mean(OD,na.rm = TRUE),
            sd_OD = sd(OD,na.rm = TRUE),
            .groups = "drop")

B.infantis = read.csv(file = "B.infantis_raw_OD.csv",header = T,sep = ",",check.names = F) %>%
  mutate(across(everything(),~ . - first(.)))
B.infantis = as.data.frame(lapply(B.infantis,function(x) gsub(",",".",x)),check.names = F)
B.infantis = as.data.frame(lapply(B.infantis,as.numeric),check.names = F)
B.infantis[B.infantis <= 0] = 1e-4
B.infantis_long = B.infantis %>%
  pivot_longer(cols = -`Time (h)`,names_to = 'Condition',values_to = "OD") %>%
  mutate(Carbon = sub("-\\d+$","",Condition)) %>%
  group_by(`Time (h)`,Carbon) %>%
  summarise(mean_OD = mean(OD,na.rm = TRUE),
            sd_OD = sd(OD,na.rm = TRUE),
            .groups = "drop")

B.luti = read.csv(file = "B.luti_raw_OD.csv",header = T,sep = ",",check.names = F) %>%
  mutate(across(everything(),~ . - first(.)))
B.luti = as.data.frame(lapply(B.luti,function(x) gsub(",",".",x)),check.names = F)
B.luti = as.data.frame(lapply(B.luti,as.numeric),check.names = F)
B.luti[B.luti <= 0] = 1e-4
B.luti_long = B.luti %>%
  pivot_longer(cols = -`Time (h)`,names_to = 'Condition',values_to = "OD") %>%
  mutate(Carbon = sub("-\\d+$","",Condition)) %>%
  group_by(`Time (h)`,Carbon) %>%
  summarise(mean_OD = mean(OD,na.rm = TRUE),
            sd_OD = sd(OD,na.rm = TRUE),
            .groups = "drop")

B.pseudo = read.csv(file = "B.pseudo_raw_OD.csv",header = T,sep = ",",check.names = F) %>%
  mutate(across(everything(),~ . - first(.)))
B.pseudo = as.data.frame(lapply(B.pseudo,function(x) gsub(",",".",x)),check.names = F)
B.pseudo = as.data.frame(lapply(B.pseudo,as.numeric),check.names = F)
B.pseudo[B.pseudo <= 0] = 1e-4
B.pseudo_long = B.pseudo %>%
  pivot_longer(cols = -`Time (h)`,names_to = 'Condition',values_to = "OD") %>%
  mutate(Carbon = sub("-\\d+$","",Condition)) %>%
  group_by(`Time (h)`,Carbon) %>%
  summarise(mean_OD = mean(OD,na.rm = TRUE),
            sd_OD = sd(OD,na.rm = TRUE),
            .groups = "drop")

C.aerofaciens = read.csv(file = "C.aero_raw_OD.csv",header = T,sep = ",",check.names = F) %>%
  mutate(across(everything(),~ . - first(.)))
C.aerofaciens = as.data.frame(lapply(C.aerofaciens,function(x) gsub(",",".",x)),check.names = F)
C.aerofaciens = as.data.frame(lapply(C.aerofaciens,as.numeric),check.names = F)
C.aerofaciens[C.aerofaciens <= 0] = 1e-4
C.aerofaciens_long = C.aerofaciens %>%
  pivot_longer(cols = -`Time (h)`,names_to = 'Condition',values_to = "OD") %>%
  mutate(Carbon = sub("-\\d+$","",Condition)) %>%
  group_by(`Time (h)`,Carbon) %>%
  summarise(mean_OD = mean(OD,na.rm = TRUE),
            sd_OD = sd(OD,na.rm = TRUE),
            .groups = "drop")

A.caccae = read.csv(file = "A.caccae_raw_OD.csv",header = T,sep = ",",check.names = F) %>%
  mutate(across(everything(),~ . - first(.)))
A.caccae = as.data.frame(lapply(A.caccae,function(x) gsub(",",".",x)),check.names = F)
A.caccae = as.data.frame(lapply(A.caccae,as.numeric),check.names = F)
A.caccae[A.caccae <= 0] = 1e-4
A.caccae_long = A.caccae %>%
  pivot_longer(cols = -`Time (h)`,names_to = 'Condition',values_to = "OD") %>%
  mutate(Carbon = sub("-\\d+$","",Condition)) %>%
  group_by(`Time (h)`,Carbon) %>%
  summarise(mean_OD = mean(OD,na.rm = TRUE),
            sd_OD = sd(OD,na.rm = TRUE),
            .groups = "drop")


growth_curves = function(long_data,species_name) {
  p = ggplot(long_data,aes(x = `Time (h)`,y = mean_OD,color = Carbon, fill = Carbon))+
    geom_line(size = 1)+
    geom_ribbon(aes(ymin = mean_OD - sd_OD,
                    ymax = mean_OD + sd_OD),
                alpha = 0.15,color = NA)+
    labs(x = "Time (h)",y = "Optical density (600nm)",title = species_name)+
    theme_classic(base_size = 15)+
    theme(legend.position = "none",
          legend.title = element_blank(),
          plot.title = element_text(face = "italic"))+
    scale_color_manual(values = c(
      "2FL" = "#D55E00",
      "3FL" = "#0072B2",
      "3SL" = "#E69F00",
      "6SL" = "#009E73",
      "DFL" = "#CC79A7",
      "LNT" = "#56B4E9",
      "GMC" = "#000000",
      "LNnT" = "#CC7"
    )) +
    scale_fill_manual(values = c(
      "2FL" = "#D55E00",
      "3FL" = "#0072B2",
      "3SL" = "#E69F00",
      "6SL" = "#009E73",
      "DFL" = "#CC79A7",
      "LNT" = "#56B4E9",
      "GMC" = "#000000",
      "LNnT" = "#CC7"
    ))
  return(p)
}
Bfr_growth = growth_curves(long_data = B.fragilis_long,species_name = "B.fragilis")
Bbr_growth = growth_curves(long_data = B.breve_long,species_name = "B.breve")
Bbi_growth = growth_curves(long_data = B.bifidum_long,species_name = "B.bifidum")
Bin_growth = growth_curves(long_data = B.infantis_long,species_name = "B.infantis")
Blu_growth = growth_curves(long_data = B.luti_long,species_name = "B.luti")
Bps_growth = growth_curves(long_data = B.pseudo_long,species_name = "B.pseudocatenulatum")
Cae_growth = growth_curves(long_data = C.aerofaciens_long,species_name = "C.aerofaciens")
Aca_growth = growth_curves(long_data = A.caccae_long,species_name = "A.caccae")

DatapointsinH = 24

MUandLag = function(Species_OD, Carbon, Species_name) {
  
  Species = data.frame(Time = Species_OD[,1])
  
  for (i in Carbon) {
    species_HMO = Species_OD[,grep(i, colnames(Species_OD))]
    Species = cbind(Species, species_HMO)
  }
  
  t <- Species$Time
  fitfun <- function(x) {coef(fit_easylinear(t, x, h = DatapointsinH))}
  Growthrates <- sapply(Species[,-1], fitfun)
  
  for (j in colnames(Species)[2:length(Species)]) {
    fit <- fit_easylinear(t, Species[,j], h = DatapointsinH)
    #par(mfrow = c(1, 2))
    #plot(fit, log = 'y', main = paste(j, "(log)", sep = ""))
    #plot(fit, main = j)
  }
  
  results = as.data.frame(t(Growthrates))
  
  HMO = vector()
  for (i in 1:nrow(results)) {
    HMO[i] = strsplit(rownames(results)[i], "-")[[1]][1]
  }
  results$HMO = HMO
  results$HMO = factor(results$HMO,levels = c("GMC","2FL","3FL","DFL","3SL","6SL","LNT","LNnT"))
  
  mu_mean_sd = results %>%
    group_by(HMO) %>%
    summarise(mumax_mean = mean(mumax, na.rm = TRUE),
              mumax_sd = sd(mumax, na.rm = TRUE))
  mu_mean_sd = as.data.frame(mu_mean_sd)
  
  lag_mean_sd = results %>%
    group_by(HMO) %>%
    summarise(lag_mean = mean(lag, na.rm = TRUE),
              lag_sd = sd(lag, na.rm = TRUE))
  lag_mean_sd = as.data.frame(lag_mean_sd)
  
  max_OD = Species %>%
    gather(key = "Sample", value = "OD", -Time) %>%
    group_by(Sample) %>%
    summarise(max_OD_value = max(OD, na.rm = TRUE)) %>%
    mutate(HMO = sapply(Sample, function(x) strsplit(x, "-")[[1]][1])) %>%
    group_by(HMO) %>%
    summarise(max_OD_mean = mean(max_OD_value, na.rm = TRUE),
              max_OD_sd = sd(max_OD_value, na.rm = TRUE))
  
  muandlag = merge(mu_mean_sd, lag_mean_sd, by = "HMO")
  muandlag = merge(muandlag, max_OD, by = "HMO")
  muandlag[muandlag < 0] = 0
  
  p = ggplot(data = results, aes(x = HMO, y = mumax)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = paste("Maximum growthrate of", Species_name, "on different Carbon sources", sep = " "), 
         x = "Carbons", y = "mumax") +
    geom_point(aes(x = HMO, y = mumax_mean), color = "blue", size = 3, data = mu_mean_sd) +
    geom_text(aes(x = HMO, y = mumax_mean, label = round(mumax_mean, 3)), 
              vjust = 0.5, hjust = 1.1, size = 3.5, data = mu_mean_sd) +
    theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14), 
          title = element_text(size = 16))+
    geom_jitter(aes(x = HMO, y = mumax), color = "black", size = 2, data = results)+
    geom_boxplot(aes(x = HMO, y = mumax), data = results, fill = "grey", alpha = 0.5)
  
  result = list('results' = results,
                'mu_and_lag' = muandlag,
                'mu_plot' = p)
  
  return(result)
}

Bfr_results= MUandLag(Species_OD = B.fragilis, Carbon = c("2FL","3FL","3SL","6SL","DFL","LNT","LNnT","GMC"), Species_name = "B.fragilis")
Bfr_results$mu_and_lag$Species = "B.fragilis"
Bbr_results= MUandLag(Species_OD = B.breve, Carbon = c("LNT","LNnT","GMC"),Species_name = "B.breve")
Bbr_results$mu_and_lag$Species = "B.breve"
Bbi_results= MUandLag(Species_OD = B.bifidum, Carbon = c("GMC"),Species_name = "B.bifidum")
Bbi_results$mu_and_lag$Species = "B.bifidum"
Aca_results= MUandLag(Species_OD = A.caccae, Carbon = c("GMC"),Species_name = "A.caccae")
Aca_results$mu_and_lag$Species = "A.caccae"
Bps_results= MUandLag(Species_OD = B.pseudo,Carbon = c("GMC","LNT","3FL"),Species_name = "B.pseudo")
Bps_results$mu_and_lag$Species = "B.pseudo"
Blu_results= MUandLag(Species_OD = B.luti,Carbon = c("GMC"),Species_name = "B.luti")
Blu_results$mu_and_lag$Species = "B.luti"
Cae_results= MUandLag(Species_OD = C.aerofaciens,Carbon = c("GMC"),Species_name = "C.aerofaciens")
Cae_results$mu_and_lag$Species = "C.aerofaciens"
Bin_results= MUandLag(Species_OD = B.infantis,Carbon = c("2FL","3FL","DFL","LNT","LNnT","GMC"),Species_name = "B.infantis")
Bin_results$mu_and_lag$Species = "B.infantis"
measured_para = rbind(Bfr_results[['mu_and_lag']],Bin_results[['mu_and_lag']],
                      Bbr_results[['mu_and_lag']],Bps_results[['mu_and_lag']],
                      Blu_results[['mu_and_lag']],Bbi_results[['mu_and_lag']],
                      Cae_results[['mu_and_lag']],Aca_results[['mu_and_lag']])
write.table(measured_para,file = '~/Documents/simulations/Butyrate project/measured_parameters.txt',
            row.names = F,col.names = T,sep = '\t')

library(tidyverse)
setwd("~/Documents/simulations/Butyrate project/")
Bin_hplc = read.table(file = 'Bin_hplc.csv',header = TRUE,row.names = 1,sep = ',',check.names = F) %>% mutate(across(where(is.numeric), ~ ifelse(. < 0, 0, .)))
Cae_hplc = read.table(file = 'Cae_hplc.csv',header = TRUE,row.names = 1,sep = ',',check.names = F) %>% mutate(across(where(is.numeric), ~ ifelse(. < 0, 0, .)))
Blu_hplc = read.table(file = 'Blu_hplc.csv',header = TRUE,row.names = 1,sep = ',',check.names = F) %>% mutate(across(where(is.numeric), ~ ifelse(. < 0, 0, .)))
Bps_hplc = read.table(file = 'Bps_hplc.csv',header = TRUE,row.names = 1,sep = ',',check.names = F) %>% mutate(across(where(is.numeric), ~ ifelse(. < 0, 0, .)))
Aca_hplc = read.table(file = 'Aca_hplc.csv',header = TRUE,row.names = 1,sep = ',',check.names = F) %>% mutate(across(where(is.numeric), ~ ifelse(. < 0, 0, .)))
Bbi_hplc = read.table(file = 'Bbi_hplc.csv',header = TRUE,row.names = 1,sep = ',',check.names = F) %>% mutate(across(where(is.numeric), ~ ifelse(. < 0, 0, .)))
Bfr_hplc = read.table(file = 'Bfr_hplc.csv',header = TRUE,row.names = 1,sep = ',',check.names = F) %>% mutate(across(where(is.numeric), ~ ifelse(. < 0, 0, .)))
Bbr_hplc = read.table(file = 'Bbr_hplc.csv',header = TRUE,row.names = 1,sep = ',',check.names = F) %>% mutate(across(where(is.numeric), ~ ifelse(. < 0, 0, .)))

hplc_plot = function(df){
  df = as.data.frame(t(df)) %>% rownames_to_column(var = "Carbon")
  df_long = df %>% pivot_longer(
    cols = -Carbon,
    names_to = 'SCFA',
    values_to = "Concentration"
  )
  
  p = ggplot(df_long,aes(x = SCFA,y = Carbon,fill = Concentration))+
    geom_tile(color = 'black')+
    scale_fill_gradient(low = 'white',high = 'red')+
    theme_minimal(base_size = 15)+
    theme(axis.text.x = element_text(angle = 30,hjust = 1))+
    labs(x = NULL,
         y = NULL,
         fill = 'mM')
  return(p)
}

Bin_heat = hplc_plot(Bin_hplc)
Cae_heat = hplc_plot(Cae_hplc)
Blu_heat = hplc_plot(Blu_hplc)
Bps_heat = hplc_plot(Bps_hplc)
Aca_heat = hplc_plot(Aca_hplc)
Bbi_heat = hplc_plot(Bbi_hplc)
Bfr_heat = hplc_plot(Bfr_hplc)
Bbr_heat = hplc_plot(Bbr_hplc)

library(patchwork)
final_plot <- (
  (Bfr_growth | Bfr_heat | Bin_growth | Bin_heat) /
    (Bbr_growth | Bbr_heat | Bps_growth | Bps_heat) /
    (Blu_growth | Blu_heat | Bbi_growth | Bbi_heat) /
    (Cae_growth | Cae_heat | Aca_growth | Aca_heat)
)+plot_annotation(tag_levels = 'A')

library(cowplot)
hmo_colors <- c(
  "2FL"  = "#D55E00", 
  "3FL"  = "#0072B2", 
  "3SL"  = "#E69F00", 
  "6SL"  = "#009E73", 
  "DFL"  = "#CC79A7", 
  "LNT"  = "#56B4E9", 
  "LNnT" = "#CC7",
  "GMC"  = "#000000"
)

p_for_legend <- ggplot(data.frame(x=1, y=1, Carbon=names(hmo_colors)), 
                       aes(x, y, color=Carbon)) +
  geom_line(linewidth = 2) +
  scale_color_manual(values = hmo_colors) +
  labs(color = "Carbon Sources") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 18, face = "bold")
  ) +
  guides(color = guide_legend(nrow = 1))

shared_legend <- get_legend(p_for_legend)

final_output <- plot_grid(
  final_plot, 
  shared_legend, 
  ncol = 1,
  rel_heights = c(20, 1)
)

print(final_output)

ggsave(plot = final_output,filename = 'EXP_measure_results_new.jpeg',height = 42,width = 40,dpi = 1300,units = 'cm')