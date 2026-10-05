library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(ggpattern)
set.seed(98765)

'%!in%' <- function(x,y)!('%in%'(x,y))

GenerateData <- function(trendType, numberOfMachines = 6, numberOfReadings = 10, baseline = 49, stanDev = 2.5, addHighLow = c(3, 5)) {
  howManyOutliers <- length(trendType)
  #whichOneIsTheOutlier <- sample(setdiff(1:numberOfMachines,addHighLow), howManyOutliers, replace = F)
  whichOneIsTheOutlier <- sample(1:numberOfMachines, howManyOutliers, replace = F)
  names(whichOneIsTheOutlier) <- trendType
  high <- addHighLow[1]
  if (is.null(high)) {high = 0}
  low <- addHighLow[2]
  if (is.null(low)) {low = 0}
  createdData <- data.frame(index = integer(), rand = integer(), facet = character())
  noise <- rnorm(n = 10000, mean = 1, sd = stanDev)
  for (i in 1:numberOfMachines) {
    noise.sample <- round(sample(noise, numberOfReadings))
    if (i == high) {
      baselineGraph = baseline + 20
    } else if (i == low) {
      baselineGraph = baseline - 20
    } else {
      baselineGraph = baseline 
    }
    if (i %!in% whichOneIsTheOutlier) {
      temp <- data.frame(index = 1:numberOfReadings, temp = rep(baselineGraph, numberOfReadings), noise = noise.sample) %>% 
        mutate(rand = temp + noise.sample) %>% 
        select(index, rand)
      createdData <- bind_rows(createdData, bind_cols(temp, facet = paste0("Machine ", i)))
    } else {
      if (names(whichOneIsTheOutlier)[whichOneIsTheOutlier == i] == "smallPeak") {
        temp <- data.frame(index = 1:numberOfReadings, temp = round(c(seq(baselineGraph - 15,baselineGraph + 15, by = 30/(numberOfReadings%/%2)),
                                                                      rev(seq(baselineGraph - 15,baselineGraph + 15, by = 30/(numberOfReadings%/%2)))))[1:numberOfReadings],
                           noise = noise.sample) %>% 
          mutate(rand = temp + noise.sample) %>% 
          select(index, rand)
      } else if (names(whichOneIsTheOutlier)[whichOneIsTheOutlier == i] == "largePeak") {
        temp <- data.frame(index = 1:numberOfReadings, temp = round(c(seq(baselineGraph - 25, baselineGraph + 25, by = 50/(numberOfReadings%/%2)),
                                                                      rev(seq(baselineGraph - 25, baselineGraph + 25, by = 50/(numberOfReadings%/%2)))))[1:numberOfReadings], noise = noise.sample) %>% 
          mutate(rand = temp + noise.sample) %>% 
          select(index, rand)
      } else if (names(whichOneIsTheOutlier)[whichOneIsTheOutlier == i] == "smallValley") {
        temp <- data.frame(index = 1:numberOfReadings, temp = round(c(rev(seq(baselineGraph - 15,baselineGraph + 15, by = 30/(numberOfReadings%/%2))),
                                                                      seq(baselineGraph - 15,baselineGraph + 15, by = 30/(numberOfReadings%/%2))))[1:numberOfReadings], noise = noise.sample) %>% 
          mutate(rand = temp + noise.sample) %>% 
          select(index, rand)
      } else if (names(whichOneIsTheOutlier)[whichOneIsTheOutlier == i] == "largeValley") {
        temp <- data.frame(index = 1:numberOfReadings, temp = round(c(rev(seq(baselineGraph - 25, baselineGraph + 25, by = 50/(numberOfReadings%/%2))),
                                                                      seq(baselineGraph - 25, baselineGraph + 25, by = 50/(numberOfReadings%/%2))))[1:numberOfReadings], noise = noise.sample) %>% 
          mutate(rand = temp + noise.sample) %>% 
          select(index, rand)
      } else if (names(whichOneIsTheOutlier)[whichOneIsTheOutlier == i] == "smallRise") {
        temp <- data.frame(index = 1:numberOfReadings, temp = seq(baselineGraph - 15,baselineGraph + 15, by = 30/numberOfReadings)[1:numberOfReadings], noise = noise.sample) %>% 
          mutate(rand = temp + noise.sample) %>% 
          select(index, rand)
      } else if (names(whichOneIsTheOutlier)[whichOneIsTheOutlier == i] == "largeRise") {
        temp <- data.frame(index = 1:numberOfReadings, temp = seq(baselineGraph - 25, baselineGraph + 25, by = 50/numberOfReadings)[1:numberOfReadings], noise = noise.sample) %>% 
          mutate(rand = temp + noise.sample) %>% 
          select(index, rand)
      } else if (names(whichOneIsTheOutlier)[whichOneIsTheOutlier == i] == "smallFall") {
        temp <- data.frame(index = 1:numberOfReadings, temp = rev(seq(baselineGraph - 15,baselineGraph + 15, by = 30/numberOfReadings)[1:numberOfReadings]), noise = noise.sample) %>% 
          mutate(rand = temp + noise.sample) %>% 
          select(index, rand)
      } else if (names(whichOneIsTheOutlier)[whichOneIsTheOutlier == i] == "largeFall") {
        temp <- data.frame(index = 1:numberOfReadings, temp = rev(seq(baselineGraph - 25, baselineGraph + 25, by = 50/numberOfReadings)[1:numberOfReadings]), noise = noise.sample) %>% 
          mutate(rand = temp + noise.sample) %>% 
          select(index, rand)
      } else if (names(whichOneIsTheOutlier)[whichOneIsTheOutlier == i] == "exponential") {
        pickVariation = sample(1:5, 1)
        temp <- data.frame(index = 1:numberOfReadings, temp = 2^(seq(1, numberOfReadings, by = 1))/25 + baselineGraph, noise = noise.sample)
        if (pickVariation == 1) {
          temp <- temp %>% mutate(temp = rev(temp))
        } else if (pickVariation == 2) {
          temp <- temp %>% mutate(temp = 2^(c(seq(1, numberOfReadings/2 + 1, by = 1), rev(seq(1, numberOfReadings/2, by = 1))))/2 + baselineGraph)
        } else if (pickVariation == 3) {
          temp <- data.frame(index = 1:numberOfReadings, temp = c(2^(seq(4, 11, by = 0.5))/25 + baselineGraph), noise = noise.sample)
        } else if (pickVariation == 4) {
          temp <- data.frame(index = 1:numberOfReadings, temp = c(2^(seq(4, 11, by = 0.5))/25 + baselineGraph), noise = noise.sample) %>% 
            mutate(temp = rev(temp))
        }
        temp <- temp %>% 
          mutate(temp = if_else(temp > 90, 90, temp)) %>%
          mutate(temp = if_else(temp < 10, 10, temp)) %>%
          mutate(noise.sample = if_else(noise > 10, 10, noise)) %>% 
          mutate(noise.sample = if_else(noise.sample < -9, -9, noise.sample)) %>% 
          mutate(rand = round(temp + noise.sample)) %>% 
          select(index, rand)
      } else if (names(whichOneIsTheOutlier)[whichOneIsTheOutlier == i] == "exponential_inverse") {
        pickVariation = sample(1:5, 1)
        temp <- data.frame(index = 1:numberOfReadings, temp = 2^(seq(1, numberOfReadings, by = 1))/25 + baselineGraph, noise = noise.sample)
        if (pickVariation == 1) {
          temp <- temp %>% mutate(temp = rev(temp))
        } else if (pickVariation == 2) {
          temp <- temp %>% mutate(temp = 2^(c(seq(1, numberOfReadings/2 + 1, by = 1), rev(seq(1, numberOfReadings/2, by = 1))))/2 + baselineGraph)
        } else if (pickVariation == 3) {
          temp <- data.frame(index = 1:numberOfReadings, temp = c(2^(seq(4, 11, by = 0.5))/25 + baselineGraph), noise = noise.sample)
        } else if (pickVariation == 4) {
          temp <- data.frame(index = 1:numberOfReadings, temp = c(2^(seq(4, 11, by = 0.5))/25 + baselineGraph), noise = noise.sample) %>% 
            mutate(temp = rev(temp))
        }
        temp <- temp %>% 
          mutate(temp = if_else(temp > 90, 90, temp)) %>%
          mutate(temp = if_else(temp < 10, 10, temp)) %>%
          mutate(noise.sample = if_else(noise > 10, 10, noise)) %>% 
          mutate(noise.sample = if_else(noise.sample < -9, -9, noise.sample)) %>% 
          mutate(rand = round(temp + noise.sample)) %>% 
          select(index, rand) %>% 
          mutate(rand = 100 - rand)
      } else if (names(whichOneIsTheOutlier)[whichOneIsTheOutlier == i] == "high") {
        temp <- data.frame(index = 1:numberOfReadings, temp = rep(min(baselineGraph + 20, 90), numberOfReadings), noise = noise.sample) %>% 
          mutate(rand = temp + noise.sample) %>% 
          select(index, rand)
      } else if (names(whichOneIsTheOutlier)[whichOneIsTheOutlier == i] == "low") {
        temp <- data.frame(index = 1:numberOfReadings, temp = rep(max(baselineGraph - 20, 10), numberOfReadings), noise = noise.sample) %>% 
          mutate(rand = temp + noise.sample) %>% 
          select(index, rand)
      }
      createdData <- bind_rows(createdData, bind_cols(temp, facet = paste0("Machine ", i)))
    }
  }
  facetOrder <- paste("Machine", seq(1, numberOfMachines))
  createdData <- createdData %>% 
    mutate(last = if_else(index == numberOfReadings, 1, 0),
           facet = factor(facet, levels = facetOrder),
           rand = if_else(rand < 10, sample(0:10, 1), rand),
           rand = if_else(rand > 90, sample(90:99, 1), rand),
           rand = round(rand))
  top5 <- createdData %>%
    filter(index == 15) %>% 
    arrange(-rand) %>%
    head(5) %>% select(facet, index, rand) %>% mutate(top5 = 1:n())
  bot5 <- createdData %>%
    filter(index == 15) %>% 
    arrange(rand) %>%
    head(5) %>% select(facet, index, rand) %>% mutate(bot5 = 1:n())
  createdData <- createdData %>% 
    left_join(top5) %>% 
    left_join(bot5) %>% 
    mutate(top5 = if_else(is.na(top5), 0, top5), 
           bot5 = if_else(is.na(bot5), 0, bot5)) %>% 
    mutate(rand = if_else((rand <= 70 & index == 15 & top5 <= 2 & top5 != 0), sample(71:75, 1), rand), 
           rand = if_else((rand >= 30 & index == 15 & bot5 <= 2 & bot5 != 0), sample(25:29, 1), rand),
           rand = if_else((rand > 70 & index == 15 & top5 > 2), sample(66:70, 1), rand), 
           rand = if_else((rand < 30 & index == 15 & bot5 > 2), sample(30:34, 1), rand))
  countCheck <- createdData %>% 
    mutate(outsideThreshold = if_else((rand < 30 | rand > 70),1,0)) %>% 
    group_by(facet) %>% 
    summarise(n = sum(outsideThreshold)) %>% 
    arrange(-n) %>% 
    head(2)
  conflict <- countCheck$n[1] == countCheck$n[2]
  if (conflict == T) {
    print("CONFLICT")
    machineToFix <- countCheck$facet[2]
    idFix <- createdData %>% 
      filter(facet == machineToFix) %>% 
      filter(rand < 30 | rand > 70) %>% 
      mutate(distance = abs(rand - 50)) %>% 
      arrange(distance)
    whichIndex <- idFix %>% 
      pull(index)
    if (1 %in% whichIndex) {
      indexToEdit <- max(whichIndex[whichIndex<8])
    } else if (15 %in% whichIndex) {
      indexToEdit <- min(whichIndex[whichIndex>7])
    } else {
      indexToEdit <- idFix %>% 
        mutate(minDistance = min(distance)) %>% 
        filter(minDistance == distance) %>% 
        head(1) %>% pull(index)
    }
    createdData <- createdData %>% 
      mutate(rand = if_else(indexToEdit == index & facet == machineToFix,
                            if_else(rand > 70, sample(66:70, 1), 
                                    if_else(rand < 30, sample(30:35, 1), rand)), rand))
  }
  createdData <- createdData %>% 
    mutate(thresholdGroup = factor(cut(rand,breaks = c(0, 20, 40, 60, 80, 100), 
                                       include.lowest = T, labels = F), ordered = T)) %>% 
    group_by(facet) %>% 
    mutate(peak = if_else(rand == max(rand), index, 0),
           peak = if_else(peak == max(peak),1,0)) %>%
    mutate(currentValue = last(rand)) %>% 
    mutate(slope = lm(rand ~ index)$coeff[2]) %>% 
    ungroup() %>% 
    mutate(thresholdGroupCurrent = factor(cut(currentValue,breaks = c(0, 20, 40, 60, 80, 100), 
                                              include.lowest = T, labels = F), ordered = T))
  print(whichOneIsTheOutlier)
  return(createdData)
}