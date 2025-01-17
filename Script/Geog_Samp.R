###############################################################################
###############################################################################
###############################################################################
#loading packages
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(cowplot)
###############################################################################
#loading files
  DATA <- read_excel(
    "Data/IOtrap野外监测数据汇总_2020_7_8_9.xlsx",
    sheet="01.GZ-Field-Band",
    col_types = c(
      "text", "text", "date", "text", "numeric", "numeric"))
###############################################################################
#data
  DATA <- unite(DATA,Site,ID,col="Label",sep="_",remove=F)
  Data <- subset(DATA,Count!="NA")
  Data$Count <- as.numeric(Data$Count)
#data  
  Data_SMU <- subset(Data, Site=="SMU")
  Data_HPX <- subset(Data, Site=="HPX")
  Data_HHG <- subset(Data, Site=="HHG")
  Data_TAJ <- subset(Data, Site=="TAJ")
  Data_XJH <- subset(Data, Site=="XJH")
###############################################################################
#test  
  Result_Summary <- data.frame(
    SampleNumber=numeric(),Date=character(),re=numeric(),cv=numeric())
#test
  for (i in unique(Data_SMU$Date)) {
    Test <- subset(Data_SMU, Date==i)
    for (SampleNumber in 1:19) {
      Test_sampled <- data.frame(
        Label=sample(Test$Label, SampleNumber, replace=F, prob=NULL)) 
      Test_sampled <- left_join(Test_sampled, Test)
      Test_sampled_result <- data.frame(
        SampleNumber = SampleNumber,
        Date = i,
        re = abs(mean(Test$Count)-mean(Test_sampled$Count) )/mean(Test_sampled$Count),
        cv = sd(Test_sampled$Count)/mean(Test_sampled$Count) )
      Result_Summary <- rbind(Result_Summary,Test_sampled_result)
      Test_sampled <- data.frame(
        Label=sample(Test$Label, SampleNumber, replace=F, prob=NULL)) 
      Test_sampled <- left_join(Test_sampled, Test)
      Test_sampled_result <- data.frame(
        SampleNumber = SampleNumber,
        Date = i,
        re = abs(mean(Test$Count)-mean(Test_sampled$Count) )/mean(Test_sampled$Count),
        cv = sd(Test_sampled$Count)/mean(Test_sampled$Count) )
      Result_Summary <- rbind(Result_Summary,Test_sampled_result)
      Test_sampled <- data.frame(
        Label=sample(Test$Label, SampleNumber, replace=F, prob=NULL)) 
      Test_sampled <- left_join(Test_sampled, Test)
      Test_sampled_result <- data.frame(
        SampleNumber = SampleNumber,
        Date = i,
        re = abs(mean(Test$Count)-mean(Test_sampled$Count) )/mean(Test_sampled$Count),
        cv = sd(Test_sampled$Count)/mean(Test_sampled$Count) )
      Result_Summary <- rbind(Result_Summary,Test_sampled_result)
    }
  }
  write.csv(Result_Summary,"Result/Geog_Samp_SMU.csv")
###############################################################################
#test
  Result_Summary <- data.frame(
    SampleNumber=numeric(),Date=character(),re=numeric(),cv=numeric())
#test
  for (i in unique(Data_HHG$Date)) {
    Test <- subset(Data_HHG, Date==i)
    for (SampleNumber in 1:11) {
      Test_sampled <- data.frame(
        Label=sample(Test$Label, SampleNumber, replace=F, prob=NULL)) 
      Test_sampled <- left_join(Test_sampled, Test)
      Test_sampled_result <- data.frame(
        SampleNumber = SampleNumber,
        Date = i,
        re = abs(mean(Test$Count)-mean(Test_sampled$Count) )/mean(Test_sampled$Count),
        cv = sd(Test_sampled$Count)/mean(Test_sampled$Count) )
      Result_Summary <- rbind(Result_Summary,Test_sampled_result)
      Test_sampled <- data.frame(
        Label=sample(Test$Label, SampleNumber, replace=F, prob=NULL)) 
      Test_sampled <- left_join(Test_sampled, Test)
      Test_sampled_result <- data.frame(
        SampleNumber = SampleNumber,
        Date = i,
        re = abs(mean(Test$Count)-mean(Test_sampled$Count) )/mean(Test_sampled$Count),
        cv = sd(Test_sampled$Count)/mean(Test_sampled$Count) )
      Result_Summary <- rbind(Result_Summary,Test_sampled_result)
      Test_sampled <- data.frame(
        Label=sample(Test$Label, SampleNumber, replace=F, prob=NULL)) 
      Test_sampled <- left_join(Test_sampled, Test)
      Test_sampled_result <- data.frame(
        SampleNumber = SampleNumber,
        Date = i,
        re = abs(mean(Test$Count)-mean(Test_sampled$Count) )/mean(Test_sampled$Count),
        cv = sd(Test_sampled$Count)/mean(Test_sampled$Count) )
      Result_Summary <- rbind(Result_Summary,Test_sampled_result)
    }
  }
  write.csv(Result_Summary,"Result/Geog_Samp_HHG.csv")
###############################################################################
#test  
  Result_Summary <- data.frame(
    SampleNumber=numeric(),Date=character(),re=numeric(),cv=numeric())
  #test
  for (i in unique(Data_HPX$Date)) {
    Test <- subset(Data_HPX, Date==i)
    for (SampleNumber in 1:13) {
      Test_sampled <- data.frame(
        Label=sample(Test$Label, SampleNumber, replace=F, prob=NULL)) 
      Test_sampled <- left_join(Test_sampled, Test)
      Test_sampled_result <- data.frame(
        SampleNumber = SampleNumber,
        Date = i,
        re = abs(mean(Test$Count)-mean(Test_sampled$Count) )/mean(Test_sampled$Count),
        cv = sd(Test_sampled$Count)/mean(Test_sampled$Count) )
      Result_Summary <- rbind(Result_Summary,Test_sampled_result)
      Test_sampled <- data.frame(
        Label=sample(Test$Label, SampleNumber, replace=F, prob=NULL)) 
      Test_sampled <- left_join(Test_sampled, Test)
      Test_sampled_result <- data.frame(
        SampleNumber = SampleNumber,
        Date = i,
        re = abs(mean(Test$Count)-mean(Test_sampled$Count) )/mean(Test_sampled$Count),
        cv = sd(Test_sampled$Count)/mean(Test_sampled$Count) )
      Result_Summary <- rbind(Result_Summary,Test_sampled_result)
      Test_sampled <- data.frame(
        Label=sample(Test$Label, SampleNumber, replace=F, prob=NULL)) 
      Test_sampled <- left_join(Test_sampled, Test)
      Test_sampled_result <- data.frame(
        SampleNumber = SampleNumber,
        Date = i,
        re = abs(mean(Test$Count)-mean(Test_sampled$Count) )/mean(Test_sampled$Count),
        cv = sd(Test_sampled$Count)/mean(Test_sampled$Count) )
      Result_Summary <- rbind(Result_Summary,Test_sampled_result)
    }
  }
  write.csv(Result_Summary,"Result/Geog_Samp_HPX.csv")
###############################################################################
#test
  Result_Summary <- data.frame(
    SampleNumber=numeric(),Date=character(),re=numeric(),cv=numeric())
#test
  for (i in unique(Data_TAJ$Date)) {
    Test <- subset(Data_TAJ, Date==i)
    for (SampleNumber in 1:9) {
      Test_sampled <- data.frame(
        Label=sample(Test$Label, SampleNumber, replace=F, prob=NULL)) 
      Test_sampled <- left_join(Test_sampled, Test)
      Test_sampled_result <- data.frame(
        SampleNumber = SampleNumber,
        Date = i,
        re = abs(mean(Test$Count)-mean(Test_sampled$Count) )/mean(Test_sampled$Count),
        cv = sd(Test_sampled$Count)/mean(Test_sampled$Count) )
      Result_Summary <- rbind(Result_Summary,Test_sampled_result)
      Test_sampled <- data.frame(
        Label=sample(Test$Label, SampleNumber, replace=F, prob=NULL)) 
      Test_sampled <- left_join(Test_sampled, Test)
      Test_sampled_result <- data.frame(
        SampleNumber = SampleNumber,
        Date = i,
        re = abs(mean(Test$Count)-mean(Test_sampled$Count) )/mean(Test_sampled$Count),
        cv = sd(Test_sampled$Count)/mean(Test_sampled$Count) )
      Result_Summary <- rbind(Result_Summary,Test_sampled_result)
      Test_sampled <- data.frame(
        Label=sample(Test$Label, SampleNumber, replace=F, prob=NULL)) 
      Test_sampled <- left_join(Test_sampled, Test)
      Test_sampled_result <- data.frame(
        SampleNumber = SampleNumber,
        Date = i,
        re = abs(mean(Test$Count)-mean(Test_sampled$Count) )/mean(Test_sampled$Count),
        cv = sd(Test_sampled$Count)/mean(Test_sampled$Count) )
      Result_Summary <- rbind(Result_Summary,Test_sampled_result)
    }
  }
  write.csv(Result_Summary,"Result/Geog_Samp_TAJ.csv")
###############################################################################
#test
  Result_Summary <- data.frame(
    SampleNumber=numeric(),Date=character(),re=numeric(),cv=numeric())
#test
  for (i in unique(Data_XJH$Date)) {
    Test <- subset(Data_XJH, Date==i)
    for (SampleNumber in 1:9) {
      Test_sampled <- data.frame(
        Label=sample(Test$Label, SampleNumber, replace=F, prob=NULL)) 
      Test_sampled <- left_join(Test_sampled, Test)
      Test_sampled_result <- data.frame(
        SampleNumber = SampleNumber,
        Date = i,
        re = abs(mean(Test$Count)-mean(Test_sampled$Count) )/mean(Test_sampled$Count),
        cv = sd(Test_sampled$Count)/mean(Test_sampled$Count) )
      Result_Summary <- rbind(Result_Summary,Test_sampled_result)
      Test_sampled <- data.frame(
        Label=sample(Test$Label, SampleNumber, replace=F, prob=NULL)) 
      Test_sampled <- left_join(Test_sampled, Test)
      Test_sampled_result <- data.frame(
        SampleNumber = SampleNumber,
        Date = i,
        re = abs(mean(Test$Count)-mean(Test_sampled$Count) )/mean(Test_sampled$Count),
        cv = sd(Test_sampled$Count)/mean(Test_sampled$Count) )
      Result_Summary <- rbind(Result_Summary,Test_sampled_result)
      Test_sampled <- data.frame(
        Label=sample(Test$Label, SampleNumber, replace=F, prob=NULL)) 
      Test_sampled <- left_join(Test_sampled, Test)
      Test_sampled_result <- data.frame(
        SampleNumber = SampleNumber,
        Date = i,
        re = abs(mean(Test$Count)-mean(Test_sampled$Count) )/mean(Test_sampled$Count),
        cv = sd(Test_sampled$Count)/mean(Test_sampled$Count) )
      Result_Summary <- rbind(Result_Summary,Test_sampled_result)
    }
  }
  write.csv(Result_Summary,"Result/Geog_Samp_XJH.csv")

