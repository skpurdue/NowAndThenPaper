library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(ggpattern)
library(ggfittext)
library(ggfx)
library(ggrepel)
set.seed(98765)

source("MakeDatasetsReliablyv4.R")

#FILL IN PATH FOR SAVING DATA TO - WE WOULD RECOMMEND A FOLDER
path <- ""

situatedTheme <- theme_minimal() + 
  theme(plot.background = element_rect(fill = "white"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray97"),
        strip.text.x = element_text(size = 16, margin = margin(t = 15, r = 0, b = 12, l = 0, unit = "pt")),
        axis.line = element_line(color = "gray50"),
        axis.text = element_text(size = 10),
        axis.ticks = element_line(color = "gray50"),
        axis.ticks.length = unit(5,"pt"),
        axis.text.x = element_text(vjust = -.5, margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
        axis.text.y = element_text(hjust = 1, margin = margin(t = 0, r = 5, b = 0, l = 10, unit = "pt")),
        plot.margin = margin(t = 0,
                             r = 20, 
                             b = 15,
                             l = 5))

set.seed(98765)
holdMachineData <- data.frame()

for (i in 1:40) {
  machineData <- GenerateData(c("smallRise","smallFall","largeFall","largeRise",
                                "smallRise","largeFall","exponential","smallFall",
                                "largeRise","largeFall","smallPeak"), 
                              numberOfMachines = 16, numberOfReadings = 15, baseline = 49,
                              stanDev = 6, addHighLow = c(0, 0))
  machineData <- machineData %>% 
    select(-c(thresholdGroup, thresholdGroupCurrent, slope)) %>% 
    mutate(iteration = i)
  holdMachineData <- bind_rows(holdMachineData, machineData)
}

GenerateMachinePlot <- function(machineData, trendType, plotType, numberOfMachines = 6, numberOfReadings = 10, baseline = 50, stanDev = 2.5, addHighLow = c(3, 5)) {
  machinePlotBase <- ggplot(data = machineData,
                            aes(index, rand)) + 
    situatedTheme + 
    facet_wrap(vars(facet), scales = "free") + 
    scale_y_continuous(limits = c(0,100), breaks = seq(0,100,by = 10), labels = seq(0,100,by = 10),
                       expand = expansion(mult = c(0, 0)),
                       name = NULL) +
    scale_x_continuous(breaks = c(1:numberOfReadings), labels = c(1:numberOfReadings),
                       expand = expansion(mult = c(0, .001)),
                       name = NULL) +
    coord_cartesian(clip = "off")
  
  if (plotType == "standard") {
    machinePlot <- machinePlotBase + 
      geom_line(color = "steelblue") + 
      geom_point(color = "steelblue",
                 size = 3, shape = 19, show.legend = F) + 
      geom_text(data = machineData %>% filter(last == 1),
                aes(x = index + 1.25, y = rand + 13, label = rand), size = 6) +
      scale_y_continuous(limits = c(0,100), breaks = seq(0,100,by = 10), labels = seq(0,100,by = 10),
                         expand = expansion(mult = c(0, 0)),
                         name = NULL, oob = scales::oob_keep) + 
      scale_x_continuous(breaks = c(1:15), labels = c(1:numberOfReadings),
                         expand = expansion(mult = c(0, 0)),
                         name = NULL)
  } else if (plotType == "emphasis") {
    machinePlot <- machinePlotBase + 
      geom_line(alpha = 0.5, color = "steelblue") + 
      geom_point(data = machineData %>% filter(last == 0),
                 color = "steelblue",
                 size = 3, shape = 19, show.legend = F) +
      geom_label(data = machineData %>% filter(last == 1), 
                 aes(x = index, y = rand, label = rand),
                 fill = "steelblue", hjust = 0.1,
                 size = 6, show.legend = F, color = "white") + 
      scale_x_continuous(breaks = c(1:15), labels = c(1:numberOfReadings),
                         expand = expansion(mult = c(0, .145)),
                         name = NULL)
  } else if (plotType == "integratedSize") {
    machinePlot <- machinePlotBase + 
      geom_line(color = "steelblue") + 
      geom_point(data = machineData %>% filter(last == 0),
                 color = "steelblue",
                 size = 3, shape = 19, show.legend = F) + 
      geom_label(data = machineData %>% filter(last == 1), 
                 aes(x = index, y = rand, label = rand, size = rand), fill = "steelblue", hjust = 0.1,
                 show.legend = F, color = "white") + 
      scale_y_continuous(limits = c(0,100), breaks = seq(0,100,by = 10), labels = seq(0,100,by = 10),
                         expand = expansion(mult = c(0, 0)),
                         name = NULL, oob = scales::oob_keep) + 
      scale_x_continuous(breaks = c(1:15), labels = c(1:numberOfReadings),
                         expand = expansion(mult = c(0, .145)),
                         name = NULL) + 
      scale_size_continuous(range = c(3,9))
  } else if (plotType == "integratedPosition") {
    machinePlot <- machinePlotBase +
      geom_line(color = "steelblue") + 
      geom_point(color = "steelblue",
                 size = 3, shape = 19, show.legend = F) + 
      geom_col(data = machineData %>% filter(last == 1), 
               aes(x = index + 0.85, y = rand), fill = "steelblue",
               show.legend = F) + 
      geom_label(data = machineData %>% filter(last == 1), 
                 aes(x = index + 0.85, y = rand, label = rand), fill = "steelblue",
                 size = 6, show.legend = F, color = "white") + 
      scale_y_continuous(limits = c(0,100), breaks = seq(0,100,by = 10), labels = seq(0,100,by = 10),
                         expand = expansion(mult = c(0, 0)),
                         name = NULL, oob = scales::oob_keep) + 
      scale_x_continuous(breaks = c(1:14, 15 + 0.85), labels = c(1:numberOfReadings),
                         expand = expansion(mult = c(0, .075)),
                         name = NULL)
  } else if (plotType == "separatedSize") {
    machinePlot <- machinePlotBase + 
      geom_line(color = "steelblue") + 
      geom_point(color = "steelblue",
                 size = 3, shape = 19, show.legend = F) + 
      geom_label(data = machineData %>% filter(last == 1),
                 aes(x = 14.5, y = 122, label = rand, size = rand),
                 fill = "steelblue",color = "white", show.legend = F) +
      scale_y_continuous(limits = c(0,100), breaks = seq(0,100,by = 10), labels = seq(0,100,by = 10),
                         expand = expansion(mult = c(0, 0)),
                         name = NULL, oob = scales::oob_keep) + 
      scale_x_continuous(breaks = c(1:15), labels = c(1:numberOfReadings),
                         expand = expansion(mult = c(0, .145)),
                         name = NULL) +
      scale_size_continuous(range = c(3,9))
  } else if (plotType == "separatedPositionPoint") {
    machinePlot <- machinePlotBase + 
      geom_line(color = "steelblue") + 
      geom_point(color = "steelblue",
                 size = 3, shape = 19, show.legend = F) + 
      geom_label(data = machineData %>% filter(last == 1),
                 aes(x = 15.15, y = 122, label = rand), size = 6,
                 fill = "steelblue", show.legend = F, color = "white") +
      annotate(geom = "segment", x = 13.25, xend = 13.75, y = 135, yend = 135, color = "gray50") +
      annotate(geom = "segment", x = 13.5, xend = 13.5, y = 110, yend = 135, color = "gray50") +
      annotate(geom = "segment", x = 13.25, xend = 13.75, y = 110, yend = 110, color = "gray50") +
      geom_point(data = machineData %>% filter(last == 1),
                 aes(x = 13.5, y = rand/4 + 110),
                 size = 2, color = "steelblue") + 
      scale_y_continuous(limits = c(0,100), breaks = seq(0,100,by = 10), labels = seq(0,100,by = 10),
                         expand = expansion(mult = c(0, 0)),
                         name = NULL, oob = scales::oob_keep) + 
      scale_x_continuous(breaks = c(1:15), labels = c(1:numberOfReadings),
                         expand = expansion(mult = c(0, .12)),
                         name = NULL)
  } else if (plotType == "separatedPositionBar") {
    machinePlot <- machinePlotBase + 
      geom_line(color = "steelblue") + 
      geom_point(color = "steelblue",
                 size = 3, shape = 19, show.legend = F) + 
      geom_label(data = machineData %>% filter(last == 1),
                 aes(x = 15.75, y = 122, label = rand), size = 6,
                 fill = "steelblue", show.legend = F, color = "white") +
      annotate(geom = "segment", x = 13.25, xend = 14.25, y = 135, yend = 135, color = "gray50") +
      annotate(geom = "segment", x = 13.75, xend = 13.75, y = 110, yend = 135, color = "gray50") +
      annotate(geom = "segment", x = 13.25, xend = 14.25, y = 110, yend = 110, color = "gray50") +
      geom_rect(data = machineData %>% filter(last == 1),
                aes(xmin = 13.4, xmax = 14.10, ymin = 110, ymax = rand/4 + 110),
                fill = "steelblue") + 
      scale_y_continuous(limits = c(0,100), breaks = seq(0,100,by = 10), labels = seq(0,100,by = 10),
                         expand = expansion(mult = c(0, 0)),
                         name = NULL, oob = scales::oob_keep) + 
      scale_x_continuous(breaks = c(1:15), labels = c(1:numberOfReadings),
                         expand = expansion(mult = c(0, .12)),
                         name = NULL)
  }
  return(machinePlot)
}


GeneratePlotsEnMasse <- function(machineDataAll, trendType, plotType, numberOfMachines = 6, numberOfReadings = 10, baseline = 50, stanDev = 2.5, addHighLow = c(3, 5)) {
  for (plot in 1:40) {
    # if (plot == numberOfPlots - 2) {
    #   plot <- "training"
    # } else if (plot == numberOfPlots - 1) {
    #   plot <- "training2"
    # } else if (plot == numberOfPlots) {
    #   plot <- "evaluation"
    # }
    machineData <- machineDataAll %>% 
      filter(iteration == plot)
    madePlot <- GenerateMachinePlot(machineData, trendType, plotType, numberOfMachines, numberOfReadings, baseline, stanDev, addHighLow)
    if (plotType == "integratedPosition") {
      plotTypeNew = "intPosition"
    } else if (plotType == "separatedPositionBar") {
      plotTypeNew = "sepPosBar"
    } else if (plotType == "separatedPositionPoint") {
      plotTypeNew = "sepPosPoint"
    } else {
      plotTypeNew = plotType
    }
    ggsave(paste0(path, plotTypeNew, "_", plot,".png"), madePlot, width = 14, height = 9, dpi = "retina")
  }
}

variationList <- c("standard", "emphasis", 
                   "integratedSize", "integratedPosition",
                   "separatedSize", "separatedPositionBar")

set.seed(98765)
for (variation in variationList) {
  GeneratePlotsEnMasse(holdMachineData, 
                       c("smallRise","smallFall","largeFall","largeRise",
                         "smallRise","largeFall","exponential","smallFall",
                         "largeRise","largeFall","smallPeak"),
                       variation,
                       numberOfMachines = 16, numberOfReadings = 15, baseline = 49,
                       stanDev = 6, addHighLow = c(0, 0))
}



set.seed(65891)
getAnswerOptions <- holdMachineData %>% 
  distinct(facet, currentValue, iteration) %>% 
  group_by(iteration) %>% 
  slice_sample(n=4) %>% 
  filter(iteration >= 13 & iteration <= 18)



set.seed(12345)
#Training question randomization
#1: 
sample(1:4, 14, replace = T)
#Training number randomization
sample(1:16, 28, replace = T) 
#Question number randomization2
sample(1:16, 28, replace = T) 

#Training question machine randomization for est cur val
sample(1:16, 4, replace = F) 




#New random number
set.seed(94750)
sample(1:16, 20, replace = T) 
