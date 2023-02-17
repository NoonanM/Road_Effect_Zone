## Data import and error calibration

#Load in the necessary packages
library(ctmm)
library(rgeos)
library(ggplot2)
library(raster)
library(rgdal)
library(gridExtra)

#Import the GPS dataset (Gets imported as an R objected called data)
load("~/Anteater_GPS_Data.Rda")

#Drop the outliers
data_clean <- data[which(data$OUT == 0),]

#Convert the clean dataset to a telemetry object
DATA <- as.telemetry(data_clean)

#Estimating the UERE
UERE <- uere.fit(DATA[[43]])

#Applying the UERE to the two datasets
uere(DATA) <-UERE

## Loading in fitted movement models

#Extract a pair of example individuals from the cleaned dataset
jenn <- DATA[[23]]
load("~/Scripts/Results/Fits/Fits_Jennifer 262.rda")
fit_jenn <- FIT

scott <- DATA[[39]]
load("~/Scripts/Results/Fits/Fits_Scott.rda")
fit_scott <- FIT

# Load the road data
BR262 <- readOGR(dsn = "~/Scripts/Roads/BR262")

#Reproject the roads to match the tracking data
ant_proj <- jenn@info$projection
BR262 <- spTransform(BR262,ant_proj)

#buffer the road by 10m
BR262_buff <- gBuffer(BR262, width = 10)


## Estimate the home range areas

# calculate the AKDEs based on the tracking data and fitted models
# Res is bumped up here to help get a smoother PDF, does not impact the estimate
jenn.akde <- akde(jenn, fit_jenn, res = 50)
scott.akde <- akde(scott, fit_scott, res = 50)

## Road Effect

### Probability of encountering a vehicle

#Currently assuming it's directly proportional to time spent on road.

#Convert HR PDFs to rasters
PDF_jenn <- raster(jenn.akde,DF="PDF")
PDF_scott <- raster(scott.akde,DF="PDF")

#Renormalise
PDF_jenn <- PDF_jenn/cellStats(PDF_jenn, stat='sum')
PDF_scott <- PDF_scott/cellStats(PDF_scott, stat='sum')

#probability of being on road
P_jenn <- as.numeric(extract(PDF_jenn,
                             BR262_buff,
                             fun=sum,
                             na.rm=TRUE))
P_scott <- as.numeric(extract(PDF_scott,
                              BR262_buff,
                              fun=sum,
                              na.rm=TRUE))

### Road effect zone

#Probability of being within some distance window from a road

# First for Jenn
results_jenn <- data.frame(distance = NA,
                           probability = NA)

bins <- seq(10,4000,by = 10)
for(i in 2:length(bins)){
  
  #Create two road buffers to identify the parts of the UD that
  # are in the slice of interest
  road_buff_min <- gBuffer(BR262, width = bins[[i-1]])
  road_buff_max <- gBuffer(BR262, width = bins[[i]])
  
  #Clip to only the slice of interest
  HR_crop <- mask(PDF_jenn, road_buff_min, inverse=TRUE)
  HR_crop <- mask(HR_crop, road_buff_max)
  
  #Store results for the current "slice"
  results_jenn[i-1,] <- c(distance = bins[[i]],
                          probability = cellStats(HR_crop,
                                                  sum))
}

#Joint probability
results_jenn$joint_probability <- results_jenn$probability*P_jenn


# Then for scott
results_scott <- data.frame(distance = NA,
                            probability = NA)

bins <- seq(10,4000,by = 10)
for(i in 2:length(bins)){
  
  #Create two road buffers to identify the parts of the UD that
  # are in the slice of interest
  road_buff_min <- gBuffer(BR262, width = bins[[i-1]])
  road_buff_max <- gBuffer(BR262, width = bins[[i]])
  
  #Clip to only the slice of interest
  HR_crop <- mask(PDF_scott, road_buff_min, inverse=TRUE)
  HR_crop <- mask(HR_crop, road_buff_max)
  
  #Store results for the current "slice"
  results_scott[i-1,] <- c(distance = bins[[i]],
                           probability = cellStats(HR_crop,
                                                   sum))
}

#Joint probability
results_scott$joint_probability <- results_scott$probability*P_scott


### Visualise the results


#Data carpentry for Jenn
jenn_points <- SpatialPoints.telemetry(jenn)
jenn_HR<- SpatialPolygonsDataFrame.UD(jenn.akde)
jenn_HR <- fortify(jenn_HR)
PDF_jenn2 <- as(PDF_jenn, "SpatialPixelsDataFrame")
PDF_jenn2 <- as.data.frame(PDF_jenn2)
colnames(PDF_jenn2) <- c("value", "x", "y")

#Data carpentry for Scott
scott_points <- SpatialPoints.telemetry(scott)
scott_HR<- SpatialPolygonsDataFrame.UD(scott.akde)
scott_HR <- fortify(scott_HR)
PDF_scott2 <- as(PDF_scott, "SpatialPixelsDataFrame")
PDF_scott2 <- as.data.frame(PDF_scott2)
colnames(PDF_scott2) <- c("value", "x", "y")

#Road 
BR262_2 <- fortify(BR262)

JENN_FIG <- 
  ggplot() +
  ggtitle("A)") +
  geom_tile(data=PDF_jenn2, aes(x=x, y=y, fill=value), alpha=0.8) +
  scale_fill_gradient(low = "transparent", high = "#669543") +
  
  geom_path(data = jenn_HR, aes(x=long, y=lat,  group = group, alpha = group)) +
  
  geom_path(data = BR262_2, aes(x=long, y=lat,  group = group), col = "#6c757d", size = 2) +
  geom_path(data = BR262_2, aes(x=long, y=lat,  group = group), col = "#ffb703", size = 0.2, linetype = "dashed") +
  
  geom_point(data = as.data.frame(coordinates(jenn_points)), aes(x,y), alpha = 0.4, size = 0.07, col = '#046C9A')  +
  
  scale_alpha_manual(values = c(0,0.7,0)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_text(size=10, family = "sans", face = "bold"),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "none",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm")) +
  ylab("Y (m)") +
  xlab("X (m)") +
  coord_cartesian(xlim = c(-102000, -94000),
                  ylim = c(24000, 32000))



SCOTT_FIG <- 
  ggplot() +
  ggtitle("B)") +
  geom_tile(data=PDF_scott2, aes(x=x, y=y, fill=value), alpha=0.8) +
  scale_fill_gradient(low = "transparent", high = "#669543") +
  
  geom_path(data = scott_HR, aes(x=long, y=lat,  group = group, alpha = group)) +
  geom_path(data = BR262_2, aes(x=long, y=lat,  group = group), col = "#6c757d", size = 2) +
    geom_path(data = BR262_2, aes(x=long, y=lat,  group = group), col = "#ffb703", size = 0.2, linetype = "dashed") +
  
  geom_point(data = as.data.frame(coordinates(scott_points)), aes(x,y), alpha = 0.4, size = 0.07, col = '#046C9A')  +
  
  scale_alpha_manual(values = c(0,0.7,0.7,0)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_text(size=10, family = "sans", face = "bold"),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "none",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm")) +
  ylab("Y (m)") +
  xlab("X (m)") +
  coord_cartesian(xlim = c(-100500, -96000),
                  ylim = c(11000, 15500))


JENN_FIG_2 <- 
ggplot(results_jenn) +
  ggtitle("C)") +
  geom_smooth(aes(y=joint_probability, x=distance), col = '#FF0000', span = 0.2, se = F) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_text(size=10, family = "sans", face = "bold"),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "none",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm")) +
  scale_y_continuous(limits = c(0, 0.00015), expand = c(0,0.000001)) +
  xlab("Distance from road (m)") +
  ylab("Probability of a road effect")
  ylab("Road Effect ")


SCOTT_FIG_2 <- 
  ggplot(results_scott) +
  ggtitle("D)") +
  geom_smooth(aes(y=joint_probability, x=distance), col = '#FF0000', span = 0.1, se = F) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_text(size=10, family = "sans", face = "bold"),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "none",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm")) +
  #scale_y_continuous(limits = c(0, 0.00015), expand = c(0,0.000001)) +
  xlab("Distance from road (m)") +
  ylab("Probability of a road effect")
  ylab("Road Effect")


FIG <-
  grid.arrange(JENN_FIG,SCOTT_FIG,
               JENN_FIG_2,SCOTT_FIG_2,
               ncol=2,
               nrow=2)

ggsave(FIG,
       width = 6.86, height = 6, units = "in",
       dpi = 600,
       bg = "transparent",
       file="Figures/Anteater_PRE.png")
