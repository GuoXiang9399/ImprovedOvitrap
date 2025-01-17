###############################################################################   
###############################################################################   
###############################################################################   
#loading packages
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(cowplot)
  library(ggpmisc)
  library(ggrepel)
  library(broom)
###############################################################################   
#loading files
  DATA <- read_excel("Data/Summary_MOIvsNOI.xlsx")
  DATA_SMU <- subset(DATA,Site=="SMU")
###############################################################################   
#data  
  DATA_Com <- pivot_longer(DATA_SMU,
    cols = c("EggNumberPerDayPerOvitrap","EggNumberPerDayPerMOT"),
     values_to = "EggNumber")
  DATA_Com$Date <- as.character(DATA_Com$Date)
  DATA_Com <- subset(DATA_Com, Date!="NA")
#plot
  p1 <- ggplot(DATA_Com)+
    geom_bar(aes(Date,EggNumber,fill=name),
             alpha=0.75,color="black",linewidth=0.5,width = 0.7,
             stat = "identity",position = position_dodge())+
    scale_y_continuous(expand = c(0,0),limits = c(0,70),
                       breaks = c(seq(0,100,by=10)))+
    scale_fill_manual(values=c("#CCCCCC","#A784B2"))+
    ylab("Egg number of each trap")+xlab("Date")+
    theme_classic()+
    theme(axis.title = element_text(size=6.5),
          axis.text.x = element_text(size=6),
          axis.text.y = element_text(size=6),
          legend.position = c(0.8,0.8),
          legend.key.size = unit(0.25,"cm"),
          legend.text=element_text(size=4),
          legend.title = element_blank(),
          legend.text.align = 0)
#plot
  p2 <- ggplot(DATA_Com)+
    geom_boxplot(aes(name,EggNumber,fill=name),
                 width=0.4,outlier.shape = NA,color="black",linewidth=0.5)+
    geom_jitter(aes(name,EggNumber),alpha=0.75,size=0.8)+
    scale_x_discrete(labels=c("MOT","IMT"))+
    scale_y_continuous(limits = c(0,70),
                       breaks = c(seq(0,100,by=10)))+
    scale_fill_manual(values=c("#CCCCCC","#A784B2"))+
    ylab("Egg number of each trap")+xlab("Trap type")+
    theme_classic()+
    theme(legend.position = "none",
          axis.title = element_text(size=6.5),
          axis.text.x = element_text(size=6),
          axis.text.y = element_text(size=6),
          legend.key.size = unit(0.25,"cm"),
          legend.text=element_text(size=4),
          legend.title = element_text(size = 4),
          legend.text.align = 0)
###############################################################################   
#plot
  formula <- y ~ I(x)
#plot
  p4 <- ggplot(DATA_SMU,aes(x = EggNumberPerDayPerOvitrap,y = MOI))+
    geom_hline(yintercept = 5,linewidth=0.25)+
    geom_hline(yintercept = 10,linewidth=0.25)+
    geom_hline(yintercept = 20,linewidth=0.25)+
    scale_x_continuous(breaks = c(seq(0,100,by=10)))+
    scale_y_continuous(breaks = c(seq(0,100,by=10)))+
    #stat_fit_deviations(formula = formula,colour="black",alpha=0.95) +
    stat_poly_line(formula = formula,color="#DB291A",fill="gray90")+
    stat_poly_eq(use_label(c("eq", "adj.R2", "P")),
                 formula = formula, size=2)+
    geom_point(color="black",alpha=0.75,size=0.6)+
        scale_y_continuous(expand = c(0,0))+
ylab("MOI")+xlab("Egg number per day per IMT")+
    theme_classic()+
    theme(legend.position = "none",
          axis.title = element_text(size=6.5),
          axis.text.x = element_text(size=6),
          axis.text.y = element_text(size=6),
          legend.key.size = unit(0.25,"cm"),
          legend.text=element_text(size=4),
          legend.title = element_text(size = 4),
          legend.text.align = 0)
#plot
  p5 <- ggplot(DATA_SMU,aes(x = EggNumberPerDayPerMOT,y = MOI))+
    geom_hline(yintercept = 5,linewidth=0.25)+
    geom_hline(yintercept = 10,linewidth=0.25)+
    geom_hline(yintercept = 20,linewidth=0.25)+
    scale_x_continuous(breaks = c(seq(0,100,by=2)))+
    scale_y_continuous(breaks = c(seq(0,100,by=10)))+
    #stat_fit_deviations(formula = formula,color="black",alpha=0.95) +
    stat_poly_line(formula = formula,color="#00A2D2",fill="gray90")+
    stat_poly_eq(use_label(c("eq", "adj.R2", "P")),
                 formula = formula, size=2)+
    geom_point(color="black",alpha=0.75,size=0.6)+
    ylab("MOI")+xlab("Egg number per day per MOT")+
    theme_classic()+
    theme(legend.position = "none",
          axis.title = element_text(size=6.5),
          axis.text.x = element_text(size=6),
          axis.text.y = element_text(size=6),
          legend.key.size = unit(0.25,"cm"),
          legend.text=element_text(size=4),
          legend.title = element_text(size = 4),
          legend.text.align = 0)
#plot
  p6 <- ggplot(DATA_SMU,aes(x = EggNumberPerDayPerMOT,y = EggNumberPerDayPerOvitrap))+
    geom_hline(yintercept = 5,linewidth=0.25)+
    geom_hline(yintercept = 10,linewidth=0.25)+
    geom_hline(yintercept = 20,linewidth=0.25)+
    scale_x_continuous(breaks = c(seq(0,100,by=2)))+
    scale_y_continuous(breaks = c(seq(0,100,by=10)))+
    #stat_fit_deviations(formula = formula,color="black",alpha=0.95) +
    stat_poly_line(formula = formula,color="#00A559",fill="gray90")+
    stat_poly_eq(use_label(c("eq", "adj.R2", "P")),
                 formula = formula, size=2)+
    geom_point(color="black",alpha=0.75,size=0.6)+
    ylab("Egg number per day per MOT")+xlab("Egg number per day per IMT")+
    theme_classic()+
    theme(legend.position = "none",
          axis.title = element_text(size=6.5),
          axis.text.x = element_text(size=6),
          axis.text.y = element_text(size=6),
          legend.key.size = unit(0.25,"cm"),
          legend.text=element_text(size=4),
          legend.title = element_text(size = 4),
          legend.text.align = 0)
###############################################################################   
#plot  
  pI <- plot_grid(p1,p2,rel_widths=c(0.65,0.35),ncol=2,
                  labels = c("a","b"),label_size = 12)
  pII <- plot_grid(p4,p5,p6,ncol=3,
                   labels = c("c","d","e"),label_size = 12)
  plot_grid(pI,pII,ncol=1)
  ggsave("Plot/MOIvsOvi.pdf",width=17,height=10,units="cm" )
  
  