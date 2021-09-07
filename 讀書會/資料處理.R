library(dplyr)
library(magrittr)
library(ggplot2)
library(readxl)

LearningData = read.csv("C:/Users/Lai/Desktop/LearningData_202001_08.csv",header = T)
VideoData = read.csv("C:/Users/Lai/Desktop/Daya/VideoData_202001_08.csv",header = T)

VideoData = VideoData %>%
  filter(影片長度 != "") %>%
  filter(動作名稱 != "") %>%
  filter(!(知識架構 %in% levels(as.factor(VideoData$知識架構))[1:3]))


VideoData$知識架構 = gsub("\\[|\\]","",VideoData$知識架構)

VideoData$影片名稱 %>% table()

x = VideoData$影片長度 %>% strsplit("H")  %>% unlist()
x = matrix(x,ncol=2,byrow=T)


min = x[,2] %>% strsplit("M") %>% unlist()
time = matrix(min,ncol = 2,byrow = T)

a = as.numeric(time[,1])*60 

b = time[,2] %>% strsplit("S") %>%unlist() %>% as.numeric() 
VideoData$影片長度 = as.numeric(a+b)



VideoData %>%
  group_by(知識架構) %>%
  summarise(Mean = round(mean(影片長度),2))




