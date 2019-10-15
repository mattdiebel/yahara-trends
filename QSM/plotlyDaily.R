plotlyDaily <- function(eList, FN = FALSE, Samples = TRUE) {
  if (FN==TRUE) {
    pdata = eList$Daily[,c("Date","FNConc","FNConc_QSM")]
  } else {
    pdata = eList$Daily[,c("Date","ConcDay","ConcDay_QSM")]
  }
  colnames(pdata) <- c("Date","WRTDS","QSM")
  p1 <- plot_ly(type="scatter", mode="lines")
  p1 <- add_trace(p1,
                 data = pdata,
                 x = ~Date,
                 y = ~WRTDS,
                 name = "WRTDS",
                 line = list(width = 1, color = "#e3550e"),
                 hoverlabel = list(bgcolor = "white"),
                 hoverinfo = "text", 
                 text = paste0("WRTDS", "<br>", 
                             format(pdata$Date, "%B %d, %Y"), "<br>", 
                             "TP: ", signif(pdata$WRTDS,3), " mg/L"))
  p1 <- add_trace(p1,
                 data = pdata,
                 x = ~Date,
                 y = ~QSM,
                 name = "QSM",
                 line = list(width = 1, color = "blue"),
                 hoverlabel = list(bgcolor = "white"),
                 hoverinfo = "text", 
                 text = paste0("QSM", "<br>", 
                             format(pdata$Date, "%B %d, %Y"), "<br>", 
                             "TP: ", signif(pdata$QSM,3), " mg/L"))
  if (Samples==TRUE) {
    p1 <- add_trace(p1,
                   data = eList$Sample,
                   x = ~Date,
                   y = ~ConcAve,
                   mode = "markers",
                   name = "Measured",
                   marker = list(
                     color = "black",
                     line = list(
                       color = "black",
                       width = 1
                     )),
                   hoverlabel = list(bgcolor = "black"),
                   hoverinfo = "text",
                   text = paste0("Measured", "<br>",
                               format(eList$Sample$Date, "%B %d, %Y"), "<br>",
                               "TP: ", signif(eList$Sample$ConcAve,3), " mg/L"))
  }
  p1 <- layout(p1,
              yaxis = list(title = "TP (mg/L)", type = "log"),
              xaxis = list(title = "", rangeslider = list(type = "date", thickness = 0.05)))
  p1
}
