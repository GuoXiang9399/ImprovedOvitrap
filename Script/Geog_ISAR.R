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
  DATA_HHG <- read_excel("Data/AeSpatWeek_HHG20-7-9_1127.xlsx", 
                     col_types = c("text", "date", "numeric", 
                                   "text", "numeric", "numeric", "text", 
                                   "numeric", "numeric", "numeric"))
  DATA_HPX <- read_excel("Data/AeSpatWeek_HPX20-7-9_1127.xlsx", 
                         col_types = c("text", "date", "numeric", 
                                       "text", "numeric", "numeric", "text", 
                                       "numeric", "numeric", "numeric"))
  DATA_SMU <- read_excel("Data/AeSpatWeek_SMU20-7-9_1127.xlsx", 
                         col_types = c("text", "date", "numeric", 
                                       "text", "numeric", "numeric", "text", 
                                       "numeric", "numeric", "numeric"))
  DATA_TAJ <- read_excel("Data/AeSpatWeek_TAJ20-7-9_1127.xlsx", 
                         col_types = c("text", "date", "numeric", 
                                       "text", "numeric", "numeric", "text", 
                                       "numeric", "numeric", "numeric"))
  DATA_XJH <- read_excel("Data/AeSpatWeek_XJH20-7-9_1127.xlsx", 
                         col_types = c("text", "date", "numeric", 
                                       "text", "numeric", "numeric", "text", 
                                       "numeric", "numeric", "numeric"))
  DATA_HHG$Site <- "HHG"
  DATA_HPX$Site <- "HPX"
  DATA_SMU$Site <- "SMU"
  DATA_TAJ$Site <- "TAJ"
  DATA_XJH$Site <- "XJH"
  DATA <- rbind(DATA_HHG, DATA_HPX, DATA_SMU, DATA_TAJ, DATA_XJH)
  DATA <- subset(DATA, Dist <= 250) 
  DATA <- subset(DATA, Moran_I_statistic_standard_deviate!="Inf")
###############################################################################
#data
  DATAm1 <- subset(DATA, Moran_I_statistic_standard_deviate>=1.65)
  DATAm2 <- subset(DATA, Moran_I_statistic_standard_deviate>=1.96)
#data  
  DATAm1 <- subset(DATA, p_value <=0.1)
  DATAm2 <- subset(DATA, p_value <=0.05)
#data  
  DATAm <- subset(DATA,p_value <=0.1 )
  DATAn <- subset(DATA,p_value >0.1 )
###############################################################################
#plot  
  p1 <- ggplot(DATAm1,aes(Dist,Moran_I_statistic_standard_deviate))+
    stat_smooth(method="loess",color="#DB291A",fill="gray90",
                linewidth=0.75)+
    geom_point(size=0.3,alpha=0.6)+
    scale_x_continuous(expand = c(0,0),limits = c(50,250),
                       breaks = c(seq(50,250,by=25)))+
    scale_y_continuous(expand = c(0,0),limits = c(1.5,4.0))+
    ylab("Moran_I_statistic_standard_deviate")+
    theme_classic()+
    theme(axis.text.x = element_text(size=6),
          axis.text.y = element_text(size=6),
          axis.title = element_text(size=7),
          legend.key.size = unit(0.25,"cm"),
          legend.text=element_text(size=4),
          legend.title = element_text(size = 4),
          legend.text.align = 0,
          legend.position=c(.1, .8))
  p4 <- ggplot(DATAm2,aes(Dist,Moran_I_statistic_standard_deviate))+
    stat_smooth(method="loess",color="#DB291A",fill="gray90",
                linewidth=0.75)+
    geom_point(size=0.3,alpha=0.6)+
    scale_x_continuous(expand = c(0,0),limits = c(50,250),
                       breaks = c(seq(50,250,by=25)))+
    scale_y_continuous(expand = c(0,0),limits = c(1.5,4.0))+
    ylab("Moran_I_statistic_standard_deviate")+
    theme_classic()+
    theme(axis.text.x = element_text(size=6),
          axis.text.y = element_text(size=6),
          axis.title = element_text(size=7),
          legend.key.size = unit(0.25,"cm"),
          legend.text=element_text(size=4),
          legend.title = element_text(size = 4),
          legend.text.align = 0,
          legend.position=c(.1, .8))
  p2 <- ggplot(DATAm1,aes(Dist,p_value))+
    stat_smooth(method="loess",color="#DB291A",fill="gray90",
                linewidth=0.75)+
    geom_point(size=0.3,alpha=0.6)+
    scale_x_continuous(expand = c(0,0),limits = c(50,250),
                       breaks = c(seq(50,250,by=25)))+
    scale_y_continuous(expand = c(0,0))+
    ylab("p_value")+
    theme_classic()+
    theme(axis.text.x = element_text(size=6),
          axis.text.y = element_text(size=6),
          axis.title = element_text(size=7),
          legend.key.size = unit(0.25,"cm"),
          legend.text=element_text(size=4),
          legend.title = element_text(size = 4),
          legend.text.align = 0,
          legend.position=c(.1, .8))
  p5 <- ggplot(DATAm2,aes(Dist,p_value))+
    stat_smooth(method="loess",color="#DB291A",fill="gray90",
                linewidth=0.75)+
    geom_point(size=0.3,alpha=0.6)+
    scale_x_continuous(expand = c(0,0),limits = c(50,250),
                       breaks = c(seq(50,250,by=25)))+
    scale_y_continuous(expand = c(0,0))+
    ylab("p_value")+
    theme_classic()+
    theme(axis.text.x = element_text(size=6),
          axis.text.y = element_text(size=6),
          axis.title = element_text(size=7),
          legend.key.size = unit(0.25,"cm"),
          legend.text=element_text(size=4),
          legend.title = element_text(size = 4),
          legend.text.align = 0,
          legend.position=c(.1, .8))
  p3 <- ggplot(DATAm1,aes(x = Dist,y = Moran_I_statistic))+
    geom_hline(yintercept = 0.2)+
    stat_smooth(method="loess",color="#00A559",fill="gray90",
                linewidth=0.75)+
    geom_point(size=0.3,alpha=0.6)+
    scale_x_continuous(expand = c(0,0),limits = c(50,250),
                       breaks = c(seq(50,250,by=25)))+
    scale_y_continuous(expand = c(0,0),limits = c(-0.1,0.5),
                       breaks = c(seq(-0.1,0.5,by=0.1)))+
    ylab("Moran_I_statistic")+
    theme_classic()+
    theme(axis.text.x = element_text(size=6),
          axis.text.y = element_text(size=6),
          axis.title = element_text(size=7),
          legend.key.size = unit(0.25,"cm"),
          legend.text=element_text(size=4),
          legend.title = element_text(size = 4),
          legend.text.align = 0,
          legend.position=c(.1, .8))
  p6 <- ggplot(DATAm2,aes(x = Dist,y = Moran_I_statistic))+
    geom_hline(yintercept = 0.2)+
    stat_smooth(method="loess",color="#00A559",fill="gray90",
                linewidth=0.75)+
    geom_point(size=0.3,alpha=0.6)+
    scale_x_continuous(expand = c(0,0),limits = c(50,250),
                       breaks = c(seq(50,250,by=25)))+
    # scale_y_continuous(expand = c(0,0),limits = c(-0.1,0.5),
   #                    breaks = c(seq(-0.1,0.5,by=0.1)))+
    ylab("Moran_I_statistic")+
    theme_classic()+
    theme(axis.text.x = element_text(size=6),
          axis.text.y = element_text(size=6),
          axis.title = element_text(size=7),
          legend.key.size = unit(0.25,"cm"),
          legend.text=element_text(size=4),
          legend.title = element_text(size = 4),
          legend.text.align = 0,
          legend.position=c(.1, .8))
  plot_grid(p1,p2,p3,p4,p5,p6,ncol=3)
  ggsave("Plot/Geog_Samp.pdf",width=17,height=10,units="cm")
  
  
  
