rm(list=ls())
#install.packages(c('NonCompart','ncar','pkr','dplyr', 'ggplot2', 'scales', 'gridExtra'))


#Indomethacin data
#https://cran.r-project.org/web/packages/medicaldata/medicaldata.pdf

################################################
###########   Data Exploration  ################
################################################


head(Indometh)
View(Indometh)

library(dplyr)     #data manipulation package
library(ggplot2)   #graph generation package
library(scales)    #graph generation package
library(gridExtra) #graph generation package


#Exploration of individual time-concentration profile (linear scale)
Individual_graph_linear <- ggplot(data=Indometh, aes(x = time , y = conc, group=Subject, colour=Subject)) + 
  theme_bw() +
  geom_point() + 
  geom_line() +
  ggtitle("A) Individual time-concentration profile (linear scale)")

#Exploration of individual time-concentration profile (Semi-log scale)
Individual_graph_log <- Individual_graph_linear +
  scale_y_continuous(limits=c(0.05, 10), breaks=c(0.1, 1, 10), trans=log10_trans())+
  ggtitle("B) Individual time-concentration profile (Semi-log scale)")


#Descriptive summary statistics for time concentration data
Indometh_summary <- Indometh %>% 
  group_by(time) %>%  
  summarise(conc.n = length(conc),
            conc.mean = mean(conc), 
            conc.sd = sd(conc))


#Exploration of mean time-concentration profile (linear scale)
Mean_graph_linear <-ggplot(data=Indometh_summary, aes(x = time , y = conc.mean)) + 
  theme_bw() +
  geom_point() + 
  geom_line()+
  geom_errorbar(aes(ymin=conc.mean, ymax=conc.mean+conc.sd), width=.1)+
  ggtitle("C) Mean time-concentration profile (Semi-log scale)")


#Exploration of mean time-concentration profile (Semi-log scale)
Mean_graph_log <- Mean_graph_linear +
  scale_y_continuous(limits=c(0.05, 10), breaks=c(0.1, 1, 10), trans=log10_trans())+
  ggtitle("D) Mean time-concentration profile (Semi-log scale)")



tiff("./Lecture_2/Time-concentration_graph.tiff", height=8.27, width=11.69, units = "in",res = 600, compression = 'lzw')
grid.arrange(Individual_graph_linear,Individual_graph_log, Mean_graph_linear, Mean_graph_log, ncol=2, nrow=2)
dev.off()


#Autogeneration of time-concentration profiles
setwd("./Lecture_2/")

library(pkr)    #graph generation package

plotPK(Indometh, "Subject", "time", "conc", 
            unitTime = "hr", unitConc = "mg/L", dose = 25)

detach("package:pkr", unload=TRUE)
setwd("../")

################################################
#######  Noncompartment analysis  ##############
################################################

library(NonCompart) #Noncompartment analysis package
library(ncar)

#Auto-fitting
Indometh_NCA_auto <-tblNCA(Indometh, key="Subject", colTime="time", colConc="conc", dose=25,
       adm="Infusion", dur=0.5, doseUnit="mg", concUnit="mg/L")

#Manual-fitting
Indometh_NCA_manual <- tblNCA(Indometh, key="Subject", colTime="time", colConc="conc", dose=25,
                        adm="Infusion", dur=0.5, doseUnit="mg", concUnit="mg/L", R2ADJ=1)


write.csv(Indometh_NCA_auto, "./Lecture_2/Indometh_NCA_auto_result.csv") 
write.csv(Indometh_NCA_manual, "./Lecture_2/Indometh_NCA_manual_result.csv") 


