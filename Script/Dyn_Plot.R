###############################################################################
###############################################################################
###############################################################################
#loading packages
  library(readxl)
  library(tidyr)
  library(dplyr)
  library(ggplot2)
  library(ggsci)
###############################################################################
#loading files
  DATA <- read_excel(
    "Data/IOtrap野外监测数据汇总_2020_7_8_9.xlsx",
    sheet="01.GZ-Field-Band",
    col_types = c(
      "text", "text", "date", "text", "numeric", "numeric"))
###############################################################################
#data
  Data <- DATA[,c("Site","ID","Date","Count")]
  Data <- unite(Data,Site,ID,col="Label",sep="-",remove=F)
  Data <- subset(Data,Count!="NA")
  Data$Count <- as.numeric(Data$Count)
  Data$Date <- as.character(Data$Date)
  #Data <- subset(Data, Site!="HHG")
#plot  
  pI <- ggplot(Data)+theme_classic()+
    geom_boxplot(aes(Date,Count,group = Date,fill=Site),
                 color="black",outlier.shape = NA,
                 linewidth=0.25)+
    geom_jitter(aes(Date,Count,group = Date),
                 color="black",
                 size=0.05,alpha=0.75)+
    #scale_x_discrete(expand=c(0,0))+
    scale_fill_manual(values = c(
      "#00A2D2","#FDB439","#00A559","#A17FBB","#DD336C"
    ))+
    scale_y_continuous(expand=c(0,0),limits=c(0,2000),
                       breaks=c(seq(0,6000,by=500)))+
    facet_wrap(vars(Site),ncol=1,scales="free")+
    theme(panel.background = element_blank(),
          axis.ticks = element_line(linewidth = 0.25),
          axis.line = element_line(linewidth = 0.25),
          axis.ticks.length = unit(0.1,"lines"),
          axis.title = element_text(size=6),
          axis.text.x = element_text(size=4,angle = 90),
          axis.text.y = element_text(size=6),
          legend.position = "none",
          legend.key.size = unit(0.2,"cm"),
          legend.text = element_text(size=4),
          strip.text = element_text(size=8),
          strip.background = element_blank()
            )
###############################################################################
#data  
  SiteCoord <- read_excel("Data/SiteCoord.xlsx")
  Data <- unite(DATA, Site, ID, col="Label",sep="_",remove = F)
  Data$Count <- as.numeric(Data$Count)
  Data <- left_join(Data,SiteCoord)
  Data_SMU <- subset(Data, Site=="SMU")
  Data_HHG <- subset(Data, Site=="HHG")
  Data_HPX <- subset(Data, Site=="HPX")
  Data_TAJ <- subset(Data, Site=="TAJ")
  Data_XJH <- subset(Data, Site=="XJH")
#plot
  p3 <- ggplot(Data_SMU,aes(Long,Lat))+
    geom_point(aes(color=Count),size=0.15)+
    facet_wrap(~Date,nrow=2)+
    scale_color_gradientn(limits=c(0,2500),na.value = "white",
                          colours=c("#FEE0D9","#F4A582","#EF8A62","#D6604D","#CA0020","#B2182B"))+
    scale_x_continuous(breaks = c(seq(113,114,by=0.001)))+
    theme(panel.background = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_rect(size=0.1,fill=NA),
          strip.background = element_blank(),
          strip.text = element_text(size=4),
          legend.background = element_blank(),
          legend.key.size = unit(0.15,"cm"),
          legend.text=element_text(size=2),
          legend.text.align = 0,
          legend.title = element_text(size=4),
          legend.position = c(0.97,0.20),
          axis.ticks = element_line(size=0.1),
          axis.ticks.length = unit(0.025,"cm"),
          axis.text.x = element_text(size=3),
          axis.text.y = element_text(size=3),
          axis.title = element_text(size=5))
  p1 <- ggplot(Data_HHG,aes(Long,Lat))+
    geom_point(aes(color=Count),size=0.15)+
    facet_wrap(~Date,nrow=2)+
    scale_color_gradientn(limits=c(0,2500),na.value = "white",
                          colours=c("#FEE0D9","#F4A582","#EF8A62","#D6604D","#CA0020","#B2182B"))+
    scale_x_continuous(breaks = c(seq(113,114,by=0.0002)))+
    theme(panel.background = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_rect(size=0.1,fill=NA),
          strip.background = element_blank(),
          strip.text = element_text(size=4),
          legend.background = element_blank(),
          legend.key.size = unit(0.15,"cm"),
          legend.text=element_text(size=2),
          legend.text.align = 0,
          legend.title = element_text(size=4),
          legend.position = c(0.97,0.20),
          axis.ticks = element_line(size=0.1),
          axis.ticks.length = unit(0.025,"cm"),
          axis.text.x = element_text(size=3),
          axis.text.y = element_text(size=3),
          axis.title = element_text(size=5))
  p3 <- ggplot(Data_HPX,aes(Long,Lat))+
    geom_point(aes(color=Count),size=0.15)+
    facet_wrap(~Date,nrow=2)+
    scale_color_gradientn(limits=c(0,2500),na.value = "white",
                          colours=c("#FEE0D9","#F4A582","#EF8A62","#D6604D","#CA0020","#B2182B"))+
    scale_x_continuous(breaks = c(seq(113,114,by=0.001)))+
    theme(panel.background = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_rect(size=0.1,fill=NA),
          strip.background = element_blank(),
          strip.text = element_text(size=4),
          legend.background = element_blank(),
          legend.key.size = unit(0.15,"cm"),
          legend.text=element_text(size=2),
          legend.text.align = 0,
          legend.title = element_text(size=4),
          legend.position = c(0.97,0.20),
          axis.ticks = element_line(size=0.1),
          axis.ticks.length = unit(0.025,"cm"),
          axis.text.x = element_text(size=3),
          axis.text.y = element_text(size=3),
          axis.title = element_text(size=5))
  p4 <- ggplot(Data_TAJ,aes(Long,Lat))+
    geom_point(aes(color=Count),size=0.15)+
    facet_wrap(~Date,nrow=2)+
    scale_color_gradientn(limits=c(0,2500),na.value = "white",
                          colours=c("#FEE0D9","#F4A582","#EF8A62","#D6604D","#CA0020","#B2182B"))+
    scale_x_continuous(breaks = c(seq(113,114,by=0.001)))+
    theme(panel.background = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_rect(size=0.1,fill=NA),
          strip.background = element_blank(),
          strip.text = element_text(size=4),
          legend.background = element_blank(),
          legend.key.size = unit(0.15,"cm"),
          legend.text=element_text(size=2),
          legend.text.align = 0,
          legend.title = element_text(size=4),
          legend.position = c(0.97,0.20),
          axis.ticks = element_line(size=0.1),
          axis.ticks.length = unit(0.025,"cm"),
          axis.text.x = element_text(size=3),
          axis.text.y = element_text(size=3),
          axis.title = element_text(size=5))
  p5 <- ggplot(Data_XJH,aes(Long,Lat))+
    geom_point(aes(color=Count),size=0.15)+
    facet_wrap(~Date,nrow=2)+
    scale_color_gradientn(limits=c(0,2500),na.value = "white",
                          colours=c("#FEE0D9","#F4A582","#EF8A62","#D6604D","#CA0020","#B2182B"))+
    scale_x_continuous(breaks = c(seq(113,114,by=0.001)))+
    theme(panel.background = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_rect(size=0.1,fill=NA),
          strip.background = element_blank(),
          strip.text = element_text(size=4),
          legend.background = element_blank(),
          legend.key.size = unit(0.15,"cm"),
          legend.text=element_text(size=2),
          legend.text.align = 0,
          legend.title = element_text(size=4),
          legend.position = c(0.97,0.20),
          axis.ticks = element_line(size=0.1),
          axis.ticks.length = unit(0.025,"cm"),
          axis.text.x = element_text(size=3),
          axis.text.y = element_text(size=3),
          axis.title = element_text(size=5))
  pII <- plot_grid(p1,p2,p3,p4,p5,ncol=1)
###############################################################################
#plot  
  plot_grid(pI,pII,ncol=2,rel_widths = c(0.3,0.7))
  ggsave("Plot/Ovi.Dyna.pdf",width=17,height=20,units="cm")
  
  