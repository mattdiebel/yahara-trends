runBaseReg <- function(estPtYear,estPtLQb,estPtQp,DecLow,DecHigh,Sample,windowY=7,windowQb=1,windowQp=30,windowS=0.5,minNumObs=100){
	localSample <- Sample
	numSamples <- length(localSample$DecYear)
	numEstPt <- length(estPtYear)
	resultBaseReg <- array(0,c(numEstPt,3))
	for ( i in 1:numEstPt){
		tempWindowY <- windowY
		tempWindowQp <- windowQp
		tempWindowQb <- windowQb
		tempWindowS <- windowS
		estY <- estPtYear[i]
		distLow <- estY - DecLow
		distHigh <- DecHigh - estY
		distTime <- min(distLow,distHigh)
		tempWindowY <- if(distTime > tempWindowY)
			tempWindowY
			else((2*tempWindowY) - distTime)
		estLQb <- estPtLQb[i]
		estQp <- estPtQp[i]
		k <- 1
		repeat {
			Sam <- localSample[abs(localSample$DecYear - estY) <= tempWindowY,]
			diffY <- abs(Sam$DecYear - estY)
			weightY <- triCube(diffY,tempWindowY)
			weightQb <- triCube(Sam$LogQb - estLQb,tempWindowQb)
			weightQp <- triCube(Sam$Qp - estQp,tempWindowQp)
			diffUpper <- ceiling(diffY)
			diffLower <- floor(diffY)
			diffSeason <- pmin(abs(diffUpper-diffY),abs(diffY-diffLower))
			weightS <- triCube(diffSeason,tempWindowS)
			Sam$weight <- weightY * weightQb * weightQp * weightS
			Sam <- subset(Sam,weight>0)
			numPosWt <- length(Sam$weight)
			tempWindowY <- tempWindowY * 1.1
			tempWindowQb <- tempWindowQb * 1.1
			tempWindowQp <- tempWindowQp * 1.1
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
		model <- lm(log(Conc)~DecYear+LogQb+Qp+SinDY+CosDY,data=Sam,weights=weight)
		term1 <- 1 / model$df.residual
		sumTerm <- sum(weight*(model$residual^2))
		SE <- sqrt(term1 * sumTerm)
		newdf <- data.frame(DecYear = estY, LogQb = estLQb, Qp = estQp, SinDY = sin(2 * pi * estY), CosDY = cos(2 * pi * estY))
		yHat <- predict(model,newdf)
		bias <- exp((SE^2)/2)
		resultBaseReg[i,1] <- yHat
		resultBaseReg[i,2] <- SE
		resultBaseReg[i,3] <- bias * exp(yHat)
	}
	return(resultBaseReg)
}