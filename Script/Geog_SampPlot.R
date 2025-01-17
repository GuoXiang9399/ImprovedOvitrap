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
  Data_SMU <- read.csv("Result/Geog_Samp_SMU.csv")
  Data_HHG <- read.csv("Result/Geog_Samp_HHG.csv")
  Data_HPX <- read.csv("Result/Geog_Samp_HPX.csv")
  Data_TAJ <- read.csv("Result/Geog_Samp_TAJ.csv")
  Data_XJH <- read.csv("Result/Geog_Samp_XJH.csv")
###############################################################################
#plot
    p1 <- ggplot(Data_HHG)+
      geom_jitter(aes(SampleNumber,re),color="black",size=0.2,alpha=0.55)+
      scale_y_continuous(expand = c(0,0))+
      scale_x_continuous(expand = c(0,0),
                         breaks = c(seq(0,19,by=1)))+
      theme_classic()+
      ylab("Relatie errors (RE)")+
      xlab("Sampling fraction (%)")+
      theme(axis.title = element_text(size=7),
            axis.text.x = element_text(size=6),
            axis.text.y = element_text(size=6),
            legend.key.size = unit(0.25,"cm"),
            legend.text=element_text(size=4),
            legend.title = element_text(size = 4),
            legend.text.align = 0,
            legend.position=c(.1, .8))
    p2 <- ggplot(Data_HHG)+
      stat_smooth(aes(SampleNumber,re),method="loess",color="#00A2D2",
                  fill="gray90",linewidth=0.75)+
      scale_y_continuous(expand = c(0,0))+
      scale_x_continuous(expand = c(0,0),
                         breaks = c(seq(0,19,by=1)))+
      theme_classic()+
      ylab("Relatie errors (RE)")+
      xlab("Sampling fraction (%)")+
      theme(axis.title = element_text(size=7),
            axis.text.x = element_text(size=6),
            axis.text.y = element_text(size=6),
            legend.key.size = unit(0.25,"cm"),
            legend.text=element_text(size=4),
            legend.title = element_text(size = 4),
            legend.text.align = 0,
            legend.position=c(.1, .8))
    p3 <- ggplot(Data_HPX)+
      geom_jitter(aes(SampleNumber,re),color="black",size=0.2,alpha=0.55)+
      scale_y_continuous(expand = c(0,0))+
      scale_x_continuous(expand = c(0,0),
                         breaks = c(seq(0,19,by=1)))+
      theme_classic()+
      ylab("Relatie errors (RE)")+
      xlab("Sampling fraction (%)")+
    theme(axis.title = element_text(size=7),
          axis.text.x = element_text(size=6),
          axis.text.y = element_text(size=6),
          legend.key.size = unit(0.25,"cm"),
          legend.text=element_text(size=4),
          legend.title = element_text(size = 4),
          legend.text.align = 0,
          legend.position=c(.1, .8))
    p4 <- ggplot(Data_HPX)+
      stat_smooth(aes(SampleNumber,re),method="loess",color="#FDB439",
                  fill="gray90",linewidth=0.75)+
      scale_y_continuous(expand = c(0,0))+
      scale_x_continuous(expand = c(0,0),
                         breaks = c(seq(0,19,by=1)))+
      theme_classic()+
      ylab("Relatie errors (RE)")+
      xlab("Sampling fraction (%)")+
    theme(axis.title = element_text(size=7),
          axis.text.x = element_text(size=6),
          axis.text.y = element_text(size=6),
          legend.key.size = unit(0.25,"cm"),
          legend.text=element_text(size=4),
          legend.title = element_text(size = 4),
          legend.text.align = 0,
          legend.position=c(.1, .8))
    p5 <- ggplot(Data_SMU)+
      geom_jitter(aes(SampleNumber,re),color="black",size=0.2,alpha=0.55)+
      scale_y_continuous(expand = c(0,0))+
      scale_x_continuous(expand = c(0,0),
                         breaks = c(seq(0,19,by=1)))+
      theme_classic()+
      ylab("Relatie errors (RE)")+
      xlab("Sampling fraction (%)")+
    theme(axis.title = element_text(size=7),
          axis.text.x = element_text(size=6),
          axis.text.y = element_text(size=6),
          legend.key.size = unit(0.25,"cm"),
          legend.text=element_text(size=4),
          legend.title = element_text(size = 4),
          legend.text.align = 0,
          legend.position=c(.1, .8))
    p6 <- ggplot(Data_SMU)+
      stat_smooth(aes(SampleNumber,re),method="loess",color="#00A559",
                  fill="gray90",linewidth=0.75)+
      scale_y_continuous(expand = c(0,0))+
      scale_x_continuous(expand = c(0,0),
                         breaks = c(seq(0,19,by=1)))+
      theme_classic()+
      ylab("Relatie errors (RE)")+
      xlab("Sampling fraction (%)")+
    theme(axis.title = element_text(size=7),
          axis.text.x = element_text(size=6),
          axis.text.y = element_text(size=6),
          legend.key.size = unit(0.25,"cm"),
          legend.text=element_text(size=4),
          legend.title = element_text(size = 4),
          legend.text.align = 0,
          legend.position=c(.1, .8))
    p7 <- ggplot(Data_TAJ)+
      geom_jitter(aes(SampleNumber,re),color="black",size=0.2,alpha=0.55)+
      scale_y_continuous(expand = c(0,0))+
      scale_x_continuous(expand = c(0,0),
                         breaks = c(seq(0,19,by=1)))+
      theme_classic()+
      ylab("Relatie errors (RE)")+
      xlab("Sampling fraction (%)")+
    theme(axis.title = element_text(size=7),
          axis.text.x = element_text(size=6),
          axis.text.y = element_text(size=6),
          legend.key.size = unit(0.25,"cm"),
          legend.text=element_text(size=4),
          legend.title = element_text(size = 4),
          legend.text.align = 0,
          legend.position=c(.1, .8))
    p8 <- ggplot(Data_TAJ)+
      stat_smooth(aes(SampleNumber,re),method="loess",color="#A17FBB",
                  fill="gray90",linewidth=0.75)+
      scale_y_continuous(expand = c(0,0))+
      scale_x_continuous(expand = c(0,0),
                         breaks = c(seq(0,19,by=1)))+
      theme_classic()+
      ylab("Relatie errors (RE)")+
      xlab("Sampling fraction (%)")+
    theme(axis.title = element_text(size=7),
          axis.text.x = element_text(size=6),
          axis.text.y = element_text(size=6),
          legend.key.size = unit(0.25,"cm"),
          legend.text=element_text(size=4),
          legend.title = element_text(size = 4),
          legend.text.align = 0,
          legend.position=c(.1, .8))
    p9 <- ggplot(Data_XJH)+
      geom_jitter(aes(SampleNumber,re),color="black",size=0.2,alpha=0.55)+
      scale_y_continuous(expand = c(0,0))+
      scale_x_continuous(expand = c(0,0),
                         breaks = c(seq(0,19,by=1)))+
      theme_classic()+
      ylab("Relatie errors (RE)")+
      xlab("Sampling fraction (%)")+
    theme(axis.title = element_text(size=7),
          axis.text.x = element_text(size=6),
          axis.text.y = element_text(size=6),
          legend.key.size = unit(0.25,"cm"),
          legend.text=element_text(size=4),
          legend.title = element_text(size = 4),
          legend.text.align = 0,
          legend.position=c(.1, .8))
    p10 <- ggplot(Data_XJH)+
      stat_smooth(aes(SampleNumber,re),method="loess",color="#DD336C",
                  fill="gray90",linewidth=0.75)+
      scale_y_continuous(expand = c(0,0))+
      scale_x_continuous(expand = c(0,0),
                         breaks = c(seq(0,19,by=1)))+
      theme_classic()+
      ylab("Relatie errors (RE)")+
      xlab("Sampling fraction (%)")+
    theme(axis.title = element_text(size=7),
          axis.text.x = element_text(size=6),
          axis.text.y = element_text(size=6),
          legend.key.size = unit(0.25,"cm"),
          legend.text=element_text(size=4),
          legend.title = element_text(size = 4),
          legend.text.align = 0,
          legend.position=c(.1, .8))
###############################################################################
#plot 
  plot_grid(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,ncol=4,align="vh" )
  ggsave("Plot/Spa_Samp.pdf",width=17,height=12,units="cm")
  
    
