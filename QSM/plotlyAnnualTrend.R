plotlyAnnual <- function(eList) {
  WY <- eList$WY
  
  p1 <- plot_ly(type="scatter",
                mode="markers",
                hoverinfo = "text",
                hoverlabel = list(bgcolor="white"),
                showlegend = FALSE)
  p1 <- add_trace(p1,
                  data = WY,
                  x = ~waterYear,
                  y = ~Conc,
                  marker = list(color = "black"),
                  text = paste0(WY$waterYear, " Conc: ", signif(WY$Conc,3), " mg/L"))
  p1 <- add_trace(p1,
                  data = WY,
                  x = ~waterYear,
                  y = ~FNConc,
                  mode = "lines",
                  line = list(color = "black", width = 1),
                  text = paste0(WY$waterYear, " FN Conc: ", signif(WY$FNConc,3), " mg/L"))
  p1 <- layout(p1,
               xaxis = list(title = ""),
               yaxis = list(title = "Concentration (mg/L)"),
               margin = list(t = 60))
  
  p2 <- plot_ly(type="scatter",
                mode="markers",
                hoverinfo = "text",
                hoverlabel = list(bgcolor="white"),
                showlegend = FALSE)
  p2 <- add_trace(p2,
                  data = WY,
                  x = ~waterYear,
                  y = ~Conc_QSM,
                  marker = list(color = "black"),
                  text = paste0(WY$waterYear, " Conc: ", signif(WY$Conc_QSM,3), " mg/L"))
  p2 <- add_trace(p2,
                  data = WY,
                  x = ~waterYear,
                  y = ~FNConc_QSM,
                  mode = "lines",
                  line = list(color = "black", width = 1),
                  text = paste0(WY$waterYear, " FN Conc: ", signif(WY$FNConc_QSM,3), " mg/L"))
  p2 <- layout(p2,
               xaxis = list(title = ""),
               yaxis = list(title = ""),
               margin = list(t = 60))
  
  subplot(p1, p2, titleX = TRUE, titleY = TRUE)

}