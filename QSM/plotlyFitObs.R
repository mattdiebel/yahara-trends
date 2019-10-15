plotlyFitObs <- function(eList) {

  Sample <- eList$Sample
  Sample$y <- log(Sample$ConcAve)
  
  axis_range <- c(min(Sample[,c("ConcHat","ConcHat_QSM","ConcAve")]),
                 max(Sample[,c("ConcHat","ConcHat_QSM","ConcAve")]))
  
  Sample$residual_WRTDS <- Sample$y - Sample$yHat
  RMSE_WRTDS <- round((mean(Sample$residual_WRTDS^2))^0.5,2)
  lm_WRTDS <- lm(y~yHat, Sample)
  R2_WRTDS <- round(summary(lm_WRTDS)$r.squared,2)
  title_WRTDS <- paste0(INFO$shortName, " WRTDS", "\nR-squared  =  ", R2_WRTDS, ", RMSE  =  ", RMSE_WRTDS)
  
  Sample$residual_QSM <- Sample$y - Sample$yHat_QSM
  RMSE_QSM <- round((mean(Sample$residual_QSM^2))^0.5,2)
  lm_QSM <- lm(y~yHat_QSM, Sample)
  R2_QSM <- round(summary(lm_QSM)$r.squared,2)
  title_QSM <- paste0(INFO$shortName, " QSM", "\nR-squared  =  ", R2_QSM, ", RMSE  =  ", RMSE_QSM)
  
  t1 <- list(
    text = title_WRTDS,
    font = list(size = 14),
    xref = "paper",
    yref = "paper",
    yanchor = "bottom",
    xanchor = "center",
    align = "center",
    x = 0.5,
    y = 1,
    showarrow = FALSE
  )
  
t2 <- t1
t2$text <- title_QSM
  
  p1 <- plot_ly(type="scatter", mode="markers", showlegend=FALSE)
  p1 <- add_trace(p1,
                  x = axis_range,
                  y = axis_range,
                  mode = "lines",
                  line = list(
                    color = "black",
                    width = 1
                  ))
  p1 <- add_trace(p1,
                  data = Sample,
                  x = ~ConcHat,
                  y = ~ConcAve,
                  marker = list(
                    color = "rgba(0,0,0,0.2)",
                    line = list(
                      color = "rgba(0,0,0,0.6)",
                      width = 1
                    )),
                  hoverlabel=list(bgcolor="white"),
                  hoverinfo="text",
                  text=paste0(format(Sample$Date, "%B %d, %Y"), "<br>", 
                              "Estimated: ", signif(Sample$ConcHat,3), " mg/L", "<br>",
                              "Observed: ", signif(Sample$ConcAve,3), " mg/L"))
  
  p1 <- layout(p1,
               xaxis = list(title = "Estimated Concentration (mg/L)", type = "log"),
               yaxis = list(title = "Observed Concentration (mg/L)", type = "log"),
               annotations = t1,
               margin = list(t = 60))
  p2 <- plot_ly(type="scatter", mode="markers", showlegend=FALSE)
  p2 <- add_trace(p2,
                  x = axis_range,
                  y = axis_range,
                  mode = "lines",
                  line = list(
                    color = "black",
                    width = 1
                  ))
  p2 <- add_trace(p2,
                  data = Sample,
                  x = ~ConcHat_QSM,
                  y = ~ConcAve,
                  marker = list(
                    color = "rgba(0,0,0,0.2)",
                    line = list(
                      color = "rgba(0,0,0,0.6)",
                      width = 1
                    )),
                  hoverlabel=list(bgcolor="white"),
                  hoverinfo="text",
                  text=paste0(format(Sample$Date, "%B %d, %Y"), "<br>", 
                              "Estimated: ", signif(Sample$ConcHat_QSM,3), " mg/L", "<br>",
                              "Observed: ", signif(Sample$ConcAve,3), " mg/L"))
  
  p2 <- layout(p2,
               xaxis = list(title = "Estimated Concentration (mg/L)", type = "log"),
               yaxis = list(title = "", type = "log"),
               annotations = t2,
               margin = list(t = 60))
  subplot(p1, p2, titleX = TRUE, titleY = TRUE)

}