makeeList <- function(wd, site, pcode, start_date, end_date) {
  Sample <- readNWISSample(site, pcode)
  Sample <- Sample[Sample$Date>=start_date,]
  Sample <- Sample[Sample$Date<=end_date,]
  Sample_day_mean <- aggregate(Sample[,c("Date","ConcAve")], by = list(Sample$Date), mean)
  Sample_day_mean <- data.frame(date = Sample_day_mean$Date, remark = "", result = Sample_day_mean$ConcAve)
  write.csv(Sample_day_mean, file = "temp.csv", row.names = FALSE)
  Sample <- readUserSample(filePath = wd, fileName = "temp.csv")
  Daily <- readNWISDaily(site, "00060", start_date, end_date)
  eList <- mergeReport(INFO, Daily, Sample)
	return(eList)
}