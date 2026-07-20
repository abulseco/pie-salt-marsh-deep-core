# Fig. 2: Bulk Organic Matter
# Bulk density, LOI, C:N, del 13 

library(ggplot2); library(plyr); library(viridis)
library(patchwork); 

theme.geochem <- function(){
  theme_bw() +
    theme(axis.text.x=element_text(size = 14, color="black"),
          axis.text.y=element_text(size = 14, color="black"),
          # panel.grid.major = element_blank(),
          # panel.grid.minor = element_blank(),
          text=element_text(size=14),
          panel.background = element_blank(),
          panel.border = element_rect(fill=NA),
          axis.line = element_line(colour = "black"), 
          legend.position = "none")
}

theme.geochem.legend <- function(){
  theme_bw() +
    theme(axis.text.x=element_text(size = 14, color="black"),
          axis.text.y=element_text(size = 14, color="black"),
          # panel.grid.major = element_blank(),
          # panel.grid.minor = element_blank(),
          text=element_text(size=20),
          panel.background = element_blank(),
          panel.border = element_rect(fill=NA),
          axis.line = element_line(colour = "black"))
}

theme.geochem.angle <- function(){
  theme_bw() +
    theme(axis.text.x=element_text(size=14, color = "black", angle = 45, hjust = 1),
          axis.text.y=element_text(size = 14, color="black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          text=element_text(size=14),
          panel.background = element_blank(),
          panel.border = element_rect(fill=NA),
          axis.line = element_line(colour = "black"), 
          legend.position = "none")
}

pretty.theme <- function(){
  theme_bw()+
    theme(axis.text.x=element_text(size=14, color = "black", angle = 45, hjust = 1),
          axis.text.y=element_text(size=14, color = "black"),
          axis.title.x=element_text(size=20, color = "black"),             
          axis.title.y=element_text(size=20, color = "black"),             
          panel.grid.major.x=element_blank(),                                          
          panel.grid.minor.x=element_blank(),
          panel.grid.minor.y=element_blank(),
          panel.grid.major.y=element_blank(),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          plot.title = element_text(size=20))
}

# upload necessary data files----
# Use Dropbox > DEEPCORE-AMPLICON-PROJ > NEW_ANALYSES
## BD, LOI----
bulk_LOI <- read.csv("08_Input_Data/deepcore_bd_LOI_every10.csv", header = T)

## %C, %N, C:N----
deep_all_geochem <- read.csv("08_Input_Data/deepcore_geochem_ALL.csv") 

## C-13
carbon13 <- read.csv("08_Input_Data/13C_deepcore.csv")

# Plots----
## Bulk density----
BD_summary <- bulk_LOI %>%
  group_by(Site, Depth) %>%
  summarize(
    n = sum(!is.na(Bulk_Density)),
    mean_BD = mean(Bulk_Density, na.rm = TRUE),
    se_BD = ifelse(
      n > 1,
      sd(Bulk_Density, na.rm = TRUE) / sqrt(n),
      0
    ),
    .groups = "drop"
  )

BD_plot <- ggplot(BD_summary,aes(x = mean_BD, y = Depth, fill = "black")) +
  geom_path(linewidth = 0.8) +
  geom_errorbarh(aes(xmin = mean_BD - se_BD,
                     xmax = mean_BD + se_BD),
                 height = 0) +
  geom_point(size = 2) +
  scale_y_reverse(
    limits = c(300, 0),
    breaks = seq(0, 300, 50)) +
  facet_wrap(~Site, nrow = 1) +
  labs(x = "Bulk Density",
       y = "Depth (cm)", 
       tag = "A") +
  theme.geochem.angle() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 14))
BD_plot

## LOI----
LOI_summary <- bulk_LOI %>%
  group_by(Site, Depth) %>%
  summarize(
    n = sum(!is.na(LOI)),
    mean_LOI = mean(LOI, na.rm = TRUE),
    se_LOI = ifelse(
      n > 1,
      sd(LOI, na.rm = TRUE) / sqrt(n),
      0
    ),
    .groups = "drop"
  )

LOI_plot <- ggplot(LOI_summary,aes(x = mean_LOI, y = Depth, fill = "black")) +
  geom_path(linewidth = 0.8) +
  geom_errorbarh(aes(xmin = mean_LOI - se_LOI,
                     xmax = mean_LOI + se_LOI),
                 height = 0) +
  geom_point(size = 2) +
  scale_y_reverse(
    limits = c(300, 0),
    breaks = seq(0, 300, 50)) +
  facet_wrap(~Site, nrow = 1) +
  labs(x = "Loss-on-Ignition",
       y = "Depth (cm)", 
       tag = "B") +
  theme.geochem.angle() +
  theme(axis.title.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 14))
LOI_plot

## del C----
delC13_summary <- carbon13 %>%
  group_by(Site, Depth) %>%
  summarize(
    n = sum(!is.na(C13)),
    mean_C13 = mean(C13, na.rm = TRUE),
    se_C13 = ifelse(
      n > 1,
      sd(C13, na.rm = TRUE) / sqrt(n),
      0
    ),
    .groups = "drop"
  )

C13_plot <- ggplot(delC13_summary,aes(x = mean_C13, y = Depth, fill = "black")) +
  geom_path(linewidth = 0.8) +
  geom_errorbarh(aes(xmin = mean_C13 - se_C13,
                     xmax = mean_C13 + se_C13),
                 height = 0) +
  geom_point(size = 2) +
  scale_y_reverse(
    limits = c(300, 0),
    breaks = seq(0, 300, 50)) +
  facet_wrap(~Site, nrow = 1) +
  labs(x = expression(delta^13 * "C"),
       y = "Depth (cm)", 
       tag = "D") +
  theme.geochem.angle() +
  theme(axis.title.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 14))
C13_plot

## C:N----
CN_summary <- deep_all_geochem %>%
  group_by(Site, Depth, Years_CE) %>%
  summarize(
    n = sum(!is.na(CN)),
    mean_CN = mean(CN, na.rm = TRUE),
    se_CN = ifelse(
      n > 1,
      sd(CN, na.rm = TRUE) / sqrt(n),
      0
    ),
    .groups = "drop"
  )

CN_plot <- ggplot(CN_summary,aes(x = mean_CN, y = Depth, fill = "black")) +
  geom_path(linewidth = 0.8) +
  geom_errorbarh(aes(xmin = mean_CN - se_CN,
                     xmax = mean_CN + se_CN),
                 height = 0) +
  geom_point(size = 2) +
  scale_y_reverse(
    limits = c(300, 0),
    breaks = seq(0, 300, 50)) +
  facet_wrap(~Site, nrow = 1) +
  labs(x = "Molar C:N",
       y = "Depth (cm)", 
       tag = "C") +
  theme.geochem.angle() +
  theme(axis.title.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 14))
CN_plot

# Combine plots
combined_OM <- BD_plot | LOI_plot | CN_plot | C13_plot
combined_OM

