library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(ggpattern)
library(ggfittext)
library(ggfx)
library(ggrepel)
set.seed(98765)

source("MakeDatasetsReliably_Threshold.R")

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
        plot.margin = margin(t = 8,
                             r = 20, 
                             b = 15,
                             l = 5))

holdMachineData <- data.frame()

set.seed(98765)
for (i in 1:40) {
  machineData <- GenerateData(c("smallRise","smallFall","largeFall","largeRise",
                                "smallRise","largeFall","exponential","smallFall",
                                "largeRise","largeFall","smallPeak"), 
                              numberOfMachines = 16, numberOfReadings = 15, baseline = 49,
                              stanDev = 6, addHighLow = c(0, 0))
  machineData <- machineData %>% 
    select(-c(thresholdGroup, thresholdGroupCurrent, slope)) %>% 
    mutate(iteration = i)
  holdMachineData <- bind_rows(holdMachineData2, machineData)
}

holdMachineData <- holdMachineData %>% 
  mutate(rand = if_else(iteration == 8 & facet == "Machine 11" & index == 13, 72, rand),
         rand = if_else(iteration == 13 & facet == "Machine 3" & index == 11, 28, rand),
         rand = if_else(iteration == 18 & facet == "Machine 14" & index == 13, 71, rand))

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
  
  if (plotType == "standard_color") {
    machinePlot <- machinePlotBase + 
      geom_line(color = "gray55") + 
      geom_point(aes(color = I(ifelse(rand < 30, "deepskyblue2", ifelse(rand > 70, "coral","gray55")))),
                 size = 3, shape = 19, show.legend = F) + 
      geom_text(data = machineData %>% filter(last == 1),
                aes(x = index + 1.25, y = rand + 13, label = rand), size = 6) +
      scale_y_continuous(limits = c(0,100), breaks = seq(0,100,by = 10), labels = seq(0,100,by = 10),
                         expand = expansion(mult = c(0, 0)),
                         name = NULL, oob = scales::oob_keep) + 
      scale_x_continuous(breaks = c(1:15), labels = c(1:numberOfReadings),
                         expand = expansion(mult = c(0, 0)),
                         name = NULL)
  } else if (plotType == "standard_band") {
    machinePlot <- machinePlotBase + 
      geom_rect(aes(xmin = 1, xmax = numberOfReadings+1.55, ymin = 30, ymax = 70),
                fill = "gray90", alpha = 0.025) +
      geom_line(color = "gray55") + 
      geom_point(color = "gray55",
                 size = 3, shape = 19, show.legend = F) + 
      geom_text(data = machineData %>% filter(last == 1),
                aes(x = index + 1.25, y = rand + 13, label = rand), size = 6) +
      scale_y_continuous(limits = c(0,100), breaks = seq(0,100,by = 10), labels = seq(0,100,by = 10),
                         expand = expansion(mult = c(0, 0)),
                         name = NULL, oob = scales::oob_keep) + 
      scale_x_continuous(breaks = c(1:15), labels = c(1:numberOfReadings),
                         expand = expansion(mult = c(0, 0)),
                         name = NULL)
  } else if (plotType == "integratedSize_color") {
    machinePlot <- machinePlotBase + 
      geom_line(color = "gray55") + 
      geom_point(data = machineData %>% filter(last == 0),
                 aes(color = I(ifelse(rand < 30, "deepskyblue2", ifelse(rand > 70, "coral","gray55")))),
                 size = 3, shape = 19, show.legend = F) + 
      geom_label(data = machineData %>% filter(last == 1), 
                 aes(x = index, y = rand, label = rand, size = rand, 
                     fill = I(ifelse(rand < 30, "deepskyblue2", ifelse(rand > 70, "coral","gray55")))),
                 hjust = 0.1,
                 show.legend = F, color = "white") + 
      scale_y_continuous(limits = c(0,100), breaks = seq(0,100,by = 10), labels = seq(0,100,by = 10),
                         expand = expansion(mult = c(0, 0)),
                         name = NULL, oob = scales::oob_keep) + 
      scale_x_continuous(breaks = c(1:15), labels = c(1:numberOfReadings),
                         expand = expansion(mult = c(0, .145)),
                         name = NULL) + 
      scale_size_continuous(range = c(3,9))
  } else if (plotType == "integratedSize_band") {
    machinePlot <- machinePlotBase + 
      geom_rect(aes(xmin = 1, xmax = numberOfReadings+2.45, ymin = 30, ymax = 70),
                fill = "gray90", alpha = 0.025) +
      geom_line(color = "gray55") + 
      geom_point(data = machineData %>% filter(last == 0),
                 color = "gray55",
                 size = 3, shape = 19, show.legend = F) + 
      geom_label(data = machineData %>% filter(last == 1), 
                 aes(x = index, y = rand, label = rand, size = rand), fill = "gray55", hjust = 0.1,
                 show.legend = F, color = "white") + 
      # geom_point(data = machineData %>% filter(last == 1),
      #            color = "gray5",
      #            size = 1.25, shape = 19, show.legend = F) + 
      scale_y_continuous(limits = c(0,100), breaks = seq(0,100,by = 10), labels = seq(0,100,by = 10),
                         expand = expansion(mult = c(0, 0)),
                         name = NULL, oob = scales::oob_keep) + 
      scale_x_continuous(breaks = c(1:15), labels = c(1:numberOfReadings),
                         expand = expansion(mult = c(0, 0)),
                         name = NULL) + 
      scale_size_continuous(range = c(3,9))
  } else if (plotType == "separatedSize_color") {
    machinePlot <- machinePlotBase + 
      geom_line(color = "gray55") + 
      geom_point(aes(color = I(ifelse(rand < 30, "deepskyblue2", ifelse(rand > 70, "coral","gray55")))),
                 size = 3, shape = 19, show.legend = F) + 
      geom_label(data = machineData %>% filter(last == 1),
                 aes(x = 14.5, y = 128, label = rand, size = rand,
                     fill = I(ifelse(rand < 30, "deepskyblue2", ifelse(rand > 70, "coral","gray55")))),
                 color = "white", show.legend = F) +
      scale_y_continuous(limits = c(0,100), breaks = seq(0,100,by = 10), labels = seq(0,100,by = 10),
                         expand = expansion(mult = c(0, 0)),
                         name = NULL, oob = scales::oob_keep) + 
      scale_x_continuous(breaks = c(1:15), labels = c(1:numberOfReadings),
                         expand = expansion(mult = c(0, .145)),
                         name = NULL) +
      scale_size_continuous(range = c(3,9))
  } else if (plotType == "separatedSize_band") {
    machinePlot <- machinePlotBase +
      geom_rect(aes(xmin = 1, xmax = numberOfReadings+1.75, ymin = 30, ymax = 70),
                fill = "gray90", alpha = .2) +
      geom_rect(data = machineData %>% filter(last == 1),
                aes(xmin = 12.5, 
                    xmax = numberOfReadings+1.5, 
                    ymin = if_else(currentValue < 30, 138, ifelse(currentValue > 70, 101, 110)), 
                    ymax = if_else(currentValue < 30, 145, ifelse(currentValue > 70, 107, 145))),
                fill = "gray90") +
      geom_line(color = "gray55") +
      geom_point(color = "gray55",
                 size = 3, shape = 19, show.legend = F) +
      geom_label(data = machineData %>% filter(last == 1),
                 aes(x = 14.5, y = 128, label = rand, size = rand),
                 fill = "gray55", color = "white", show.legend = F) +
      scale_y_continuous(limits = c(0,100), breaks = seq(0,100,by = 10), labels = seq(0,100,by = 10),
                         expand = expansion(mult = c(0, 0)),
                         name = NULL, oob = scales::oob_keep) +
      scale_x_continuous(breaks = c(1:15), labels = c(1:numberOfReadings),
                         expand = expansion(mult = c(0, 0)),
                         name = NULL) +
      scale_size_continuous(range = c(3,9))
  }
  return(machinePlot)
}

GeneratePlotsEnMasse <- function(machineDataAll, trendType, plotType, numberOfMachines = 6, numberOfReadings = 10, baseline = 50, stanDev = 2.5, addHighLow = c(3, 5)) {
  for (plot in 1:40) {
    # print(paste("PLOT", plot))
    # if (plot == numberOfPlots - 1) {
    #   plot <- "training"
    # # } else if (plot == numberOfPlots - 2) {
    # #   plot <- "training2"
    # } else if (plot == numberOfPlots) {
    #   plot <- "evaluation"
    # }
    machineData <- machineDataAll %>% 
      filter(iteration == plot)
    madePlot <- GenerateMachinePlot(machineData, trendType, plotType, numberOfMachines, numberOfReadings, baseline, stanDev, addHighLow)
    if (plotType == "integratedSize_band") {
      plotTypeNew = "intSize_band"
    } else if (plotType == "integratedSize_color") {
      plotTypeNew = "intSize_color"
    } else if (plotType == "separatedSize_color") {
      plotTypeNew = "sepSize_color"
    } else if (plotType == "separatedSize_band") {
      plotTypeNew = "sepSize_band"
    } else {
      plotTypeNew = plotType
    }
    ggsave(paste0(path, plotTypeNew, "_", plot,".png"), madePlot, width = 14, height = 9, dpi = "retina")
  }
}

variationList <- c("standard_color", "standard_band", 
                   "integratedSize_color", "integratedSize_band", 
                   "separatedSize_color", "separatedSize_band")

set.seed(45678)
#FILL IN PATH FOR SAVING DATA TO - WE WOULD RECOMMEND A FOLDER
path <- ""

for (variation in variationList) {
  print(variation)
  GeneratePlotsEnMasse(holdMachineData, c("smallRise","smallFall","exponential_inverse","largeRise",
                         "smallRise","largeFall","exponential","smallFall",
                         "smallPeak"),
                       variation,
                       numberOfMachines = 16, numberOfReadings = 15, baseline = 49,
                       stanDev = 6, addHighLow = c(0, 0))
  
}


set.seed(63578)
#Training number randomization
sample(1:16, 6, replace = T) 


set.seed(06792)
datasetFiltered.answers <- datasetFiltered %>% group_by(file) %>% slice_sample(n=4)

set.seed(19283)
#Training number randomization
sample(1:16, 2, replace = T) 

#Question number randomization2
sample(1:16, 28, replace = T) 

#Training question machine randomization for est cur val
sample(1:16, 4, replace = F) 


#new selections
set.seed(47529)
sample(1:16, 36, replace = T) 
