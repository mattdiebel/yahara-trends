runQSepReg <- function(estPtYear,estPtLQb,estPtQq,DecLow,DecHigh,localSample,windowY=7,windowQb=1,windowQq=1,windowS=0.5,minNumObs=100){
	localSample <- Sample
	numSamples <- length(localSample$DecYear)
	numEstPt <- length(estPtYear)
	resultBaseReg <- array(0,c(numEstPt,3))
	for ( i in 1:numEstPt){
		tempWindowY <- windowY
		tempWindowQq <- windowQq
		tempWindowQb <- windowQb
		tempWindowS <- windowS
		estY <- estPtYear[i]
		distLow <- estY - DecLow
		distHigh <- DecHigh - estY
		distTime <- min(distLow,distHigh)
		if(distTime <= tempWindowY) {tempWindowY <- 2*tempWindowY - distTime}
		estLQb <- estPtLQb[i]
		estLQq <- estPtLQq[i]
		k <- 1
		repeat {
			Sam <- localSample[abs(localSample$DecYear - estY) <= tempWindowY,]
			diffY <- abs(Sam$DecYear - estY)
			weightY <- triCube(diffY,tempWindowY)
			weightQb <- triCube(Sam$LogQb - estLQb,tempWindowQb)
			weightQq <- triCube(Sam$LogQq - estLQq,tempWindowQq)
			diffUpper <- ceiling(diffY)
			diffLower <- floor(diffY)
			diffSeason <- pmin(abs(diffUpper-diffY),abs(diffY-diffLower))
			weightS <- triCube(diffSeason,tempWindowS)
			Sam$weight <- weightY * weightQb * weightQq * weightS
			Sam <- subset(Sam,weight>0)
			numPosWt <- length(Sam$weight)
			tempWindowY <- tempWindowY * 1.1
			tempWindowQb <- tempWindowQb * 1.1
			tempWindowQq <- tempWindowQq * 1.1
			k <- k + 1
			if(k > 10000)
				message("Problems converging")
			tempWindowS <- if(windowS <= 0.5)
				min(tempWindowS * 1.1,0.5)
			else windowS
			if(numPosWt >= minNumObs | k > 10000)
				break
		}
		weight <- Sam$weight
		aveWeight <- sum(weight)/numPosWt
		weight <- weight / aveWeight
		Sam <- data.frame(Sam)
		model <- lm(log(ConcAve)~DecYear+LogQb+LogQq+SinDY+CosDY,data=Sam,weights=weight)
		term1 <- 1 / model$df.residual
		sumTerm <- sum(weight*(model$residual^2))
		SE <- sqrt(term1 * sumTerm)
		newdf <- data.frame(DecYear = estY, LogQb = estLQb, LogQq = estLQq, SinDY = sin(2 * pi * estY), CosDY = cos(2 * pi * estY))
		yHat <- predict(model,newdf)
		bias <- exp((SE^2)/2)
		resultBaseReg[i,1] <- yHat
		resultBaseReg[i,2] <- SE
		resultBaseReg[i,3] <- bias * exp(yHat)
	}
	return(resultBaseReg)
}