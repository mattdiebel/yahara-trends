# Flow Separation model

#Set working directory
wd = "C:/LWRD/yahara-trends/QSM/"
setwd(wd)

# Load packages and functions
library(EGRET)
library(dataRetrieval)
library(lubridate)
library(EcoHydRology)
library(plotly)
library(ggplot2)
source("runQSepReg.R")
source("makeeList.R")
source("buildQSM.R")
source("plotDaily.R")
source("plotlyFitObs.R")
source("plotlyAnnual.R")

# Set site, parameter, and period
site = "05427910"
pcode = "00665"
start_date = as.Date("2012-10-01")
end_date = as.Date("2019-09-30")
INFO = readNWISInfo(site, pcode, interactive=FALSE)
INFO$shortName = "Sixmile Creek"
INFO$staAbbrev = "SXM"
INFO$paramShortName = "Total Phosphorus"
INFO$constitAbbrev = "TP"

# Set window widths
windowY=3
windowS=0.25
windowQ=1
windowQq = 1
windowQb = 0.3
minNumObs = 100

eList = makeeList(wd, site, pcode, start_date, end_date)
eList = modelEstimation(eList, windowY=windowY, windowQ=windowQ, windowS=windowS)
eList = buildQSM(eList)
plotlyFitObs(eList)
plotlyAnnual(eList)
plotDaily(eList)





