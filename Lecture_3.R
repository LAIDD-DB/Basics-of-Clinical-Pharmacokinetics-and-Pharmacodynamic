rm(list=ls())
setwd("./Lecture_3/")

#Oral Theophylline data
#https://cran.r-project.org/web/packages/medicaldata/medicaldata.pdf


################################################
###########   Data Exploration  ################
################################################
library(pkr)        #graph generation package


plotPK(Theoph, "Subject", "Time", "conc", 
       unitTime = "hr", unitConc = "mg/L", dose = 320)

detach("package:pkr", unload=TRUE)



################################################
#######  Noncompartment analysis  ##############
################################################
library(NonCompart) #Noncompartment analysis package
library(ncar)       #Noncompartment analysis package

#Auto-fitting
Theoph_NCA_auto <-tblNCA(Theoph, key="Subject", colTime="Time", colConc="conc", dose=320,
                           adm="Extravascular", doseUnit="mg", concUnit="mg/L")

#Manual-fitting
Theoph_NCA <- tblNCA(Theoph, key="Subject", colTime="Time", colConc="conc", dose=320,
                     adm="Extravascular", doseUnit="mg", concUnit="mg/L", R2ADJ=1)

write.csv(Indometh_NCA_auto, "Indometh_NCA_result.csv") 




#Report generation
pdfNCA(fileName="pdfNCA-Indometh.pdf", Indometh, key="Subject", colTime="time", colConc="conc", dose=25,
       adm="Infusion", dur=0.5, doseUnit="mg", concUnit="mg/L")

rtfNCA(fileName="rtfNCA-Indometh.rtf", Indometh, key="Subject", colTime="time", colConc="conc", dose=25,
       adm="Infusion", dur=0.5, doseUnit="mg", concUnit="mg/L")



pdfNCA(fileName="pdfNCA-Theoph.pdf", Theoph, key="Subject", colTime="Time", colConc="conc", dose=320,
       adm="Extravascular", doseUnit="mg", concUnit="mg/L")

rtfNCA(fileName="rtfNCA-Theoph.rtf", Theoph, key="Subject", colTime="Time", colConc="conc", dose=320,
       adm="Extravascular", doseUnit="mg", concUnit="mg/L")


