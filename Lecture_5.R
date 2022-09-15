rm(list=ls())
getwd()

################################################
###########   Data Exploration  ################
################################################


PC_continuous <- data.frame(read.csv("./Lecture_5/PC_continuous.csv", header=T, sep=","))
head(PC_continuous)

library(dplyr)     #data manipulation package
library(ggplot2)   #graph generation package
library(scales)    #graph generation package
library(gridExtra) #graph generation package


#Exploration of individual time-concentration profile (linear scale)
Individual_graph_linear <- ggplot(data=PC_continuous, aes(x = TIME , y = CONC, group=SUBJECT , colour=as.factor(DOSE_GRP))) + 
  theme_bw() +
  geom_point() + 
  geom_line() +
  ggtitle("A) Individual time-concentration profile (linear scale)")

#Exploration of individual time-concentration profile (Semi-log scale)
Individual_graph_log <- Individual_graph_linear +
  scale_y_continuous(limits=c(5, 10000), breaks=c(0.1, 1, 10, 1000, 10000), trans=log10_trans())+
  ggtitle("B) Individual time-concentration profile (Semi-log scale)")



tiff("./Lecture_5/Time-concentration_graph.tiff", height=8.27, width=11.69, units = "in",res = 600, compression = 'lzw')
grid.arrange(Individual_graph_linear, Individual_graph_log, nrow=2)
dev.off()


################################################
#######  Noncompartment analysis  ##############
################################################

library(NonCompart) #Noncompartment analysis package
library(ncar)

#partial AUC
iAUC = data.frame(Name=c("AUC0_10h","AUC10_32h"), Start=c(0,10), End=c(10,32)) ; iAUC

NCA_lowdose <-tblNCA(PC_continuous[PC_continuous$DOSE_GRP==335,], key="SUBJECT", colTime="TIME", colConc="CONC", dose=335,
       iAUC=iAUC, adm="Infusion", dur=10, doseUnit="mg", concUnit="ug/L") %>%  
       select(SUBJECT, CMAX, LAMZHL, TMAX, AUCLST, AUCIFO, AUC0_10h, AUC10_32h, CLO, VZO) %>%
       mutate(Css=AUC0_10h/10)


NCA_highdose <-tblNCA(PC_continuous[PC_continuous$DOSE_GRP==670,], key="SUBJECT", colTime="TIME", colConc="CONC", dose=670,
       iAUC=iAUC, adm="Infusion", dur=10, doseUnit="mg", concUnit="ug/L") %>%  
      select(SUBJECT, CMAX, LAMZHL, TMAX, AUCLST, AUCIFO, AUC0_10h, AUC10_32h, CLO, VZO) %>%
  mutate(Css=AUC0_10h/10)


write.csv(NCA_lowdose, "./Lecture_5/NCA_lowdose.csv") 
write.csv(NCA_highdose, "./Lecture_5/NCA_highdose.csv") 

C10 <- PC_continuous %>% filter(TIME==10)

write.csv(C10, "./Lecture_5/C10.csv") 

