estSurfaces_QSM <- function (eList, surfaceStart = NA, surfaceEnd = NA, localSample = NA, 
          windowY = 7, windowQ = 2, windowS = 0.5, minNumObs = 100, 
          minNumUncen = 50, edgeAdjust = TRUE, verbose = TRUE, interactive = NULL, 
          run.parallel = FALSE) 
{
  if (!is.null(interactive)) {
    warning("The argument 'interactive' is deprecated. Please use 'verbose' instead")
    verbose <- interactive
  }
  if (!is.egret(eList)) {
    stop("Please check eList argument")
  }
  localINFO <- getInfo(eList)
  localDaily <- getDaily(eList)
  if (all(is.na(localSample))) {
    localSample <- eList$Sample
  }
  highLow <- decimalHighLow(localSample)
  DecHigh <- highLow[["DecHigh"]]
  DecLow <- highLow[["DecLow"]]
  surfaceInfo <- surfaceIndex(localDaily)
  vectorYear <- surfaceInfo[["vectorYear"]]
  vectorLogQ <- surfaceInfo[["vectorLogQ"]]
  LogQ <- seq(surfaceInfo[["bottomLogQ"]], by = surfaceInfo[["stepLogQ"]], 
              length.out = surfaceInfo[["nVectorLogQ"]])
  if (is.na(surfaceStart) && is.na(surfaceEnd)) {
    nVectorYear <- length(vectorYear)
    estPtYear <- rep(vectorYear, each = 14)
    Year <- seq(surfaceInfo[["bottomYear"]], by = surfaceInfo[["stepYear"]], 
                length.out = surfaceInfo[["nVectorYear"]])
  }
  else {
    sliceIndex <- which(vectorYear >= decimalDate(as.Date(surfaceStart)) & 
                          vectorYear <= decimalDate(as.Date(surfaceEnd)))
    Year <- vectorYear[c(sliceIndex[1] - 1, sliceIndex, tail(sliceIndex, 
                                                             n = 1) + 1)]
    nVectorYear <- length(Year)
    estPtYear <- rep(Year, each = 14)
  }
  estPtLogQ <- rep(vectorLogQ, nVectorYear)
  resultSurvReg <- runSurvReg(estPtYear, estPtLogQ, DecLow, 
                              DecHigh, localSample, windowY, windowQ, windowS, minNumObs, 
                              minNumUncen, edgeAdjust = edgeAdjust, verbose = verbose, 
                              run.parallel = run.parallel)
  surfaces <- array(0, dim = c(14, nVectorYear, 3))
  for (iQ in 1:14) {
    for (iY in 1:nVectorYear) {
      k <- (iY - 1) * 14 + iQ
      surfaces[iQ, iY, ] <- resultSurvReg[k, ]
    }
  }
  attr(surfaces, "surfaceIndex") <- surfaceInfo
  attr(surfaces, "LogQ") <- LogQ
  attr(surfaces, "Year") <- Year
  return(surfaces)
}