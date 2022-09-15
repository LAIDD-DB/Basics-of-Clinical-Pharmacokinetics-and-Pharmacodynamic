rm(list=ls())
getwd()


################################################
################   Data Import  ################
################################################

library(dplyr)     #data manipulation package

PK_result_S <- data.frame(read.csv("./Lecture_4/PC_single.csv", header=T, sep=","))
head(PK_result_S)

PK_result_M <- data.frame(read.csv("./Lecture_4/PC_multiple.csv", header=T, sep=","))
head(PK_result_M)

PK_result_M_prep <-PK_result_M %>% mutate(NTIME=TIME-168)
head(PK_result_M_prep)


################################################
###########   Data Exploration  ################
################################################

library(ggplot2)   #graph generation package
library(gridExtra) #graph generation package


CP_plot_single <- ggplot(data=PK_result_S, aes(x = TIME, y = CP, group=as.factor(SUBJECT), colour=as.factor(SUBJECT))) + theme_bw() +
  geom_line(size=1.5) +
  xlab ("Time after dose (hour)") +
  ylab ("Plasma concentration (μg/L)")

CP_plot_multiple <- ggplot(data=PK_result_M_prep, aes(x = TIME, y = CP, group=as.factor(SUBJECT), colour=as.factor(SUBJECT))) + theme_bw() +
  geom_line(size=1.5) +
  xlab ("Time after dose (hour)") +
  ylab ("Plasma concentration (μg/L)")

CP_plot_multiple2 <-ggplot(data=PK_result_M_prep, aes(x = NTIME, y = CP, group=as.factor(SUBJECT), colour=as.factor(SUBJECT))) + theme_bw() +
  geom_line(size=1.5) +
  xlab ("Time after dose (hour)") +
  ylab ("Plasma concentration (μg/L)")



tiff("./Lecture_4/Time-concentration_graph.tiff", height=8.27, width=11.69, units = "in",res = 600, compression = 'lzw')
grid.arrange(CP_plot_single, CP_plot_multiple2, nrow=2)
dev.off()



################################################
#######  Noncompartment analysis  ##############
################################################

library(PKNCA)
library(NonCompart)

NCA_S_auto<-tblNCA(PK_result_S, key="SUBJECT", colTime="TIME",
                  colConc="CP", dose=400, adm="Extravascular", doseUnit="mg", concUnit="ug/L") %>% select(SUBJECT, CMAX, LAMZHL, TMAX, AUCLST, AUCIFO, CLFO, VZFO) 



NCA_M_auto<-tblNCA(PK_result_M_prep, key="SUBJECT", colTime="NTIME",
                   colConc="CP", dose=400, adm="Extravascular", doseUnit="mg", concUnit="ug/L") %>% select(SUBJECT, CMAX, LAMZHL, TMAX, AUCLST, AUCIFO, CLFO, VZFO) 


write.csv(NCA_S_auto, "./Lecture_4/NCA_result_single.csv") 
write.csv(NCA_S_auto, "./Lecture_4/NCA_result_multiple.csv") 
