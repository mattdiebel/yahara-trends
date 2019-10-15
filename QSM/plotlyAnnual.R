plotlyAnnual <- function(eList) {
  
  WY <- eList$WY

  conc_range <- c(min(WY[,c("Conc","Conc_QSM")]),
                  max(WY[,c("Conc","Conc_QSM")]))
  
  flux_range <- c(min(WY[,c("Flux","Flux_QSM")]),
                  max(WY[,c("Flux","Flux_QSM")]))
  
  t1 <- list(
    text = "Concentration (mg/L)",
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
  t2$text <- "Flux (kg)"

  p1 <- plot_ly(type = "scatter",
                mode = "markers",
                showlegend = FALSE)
  p1 <- add_trace(p1,
                  x = conc_range,
                  y = conc_range,
                  mode = "lines",
                  line = list(
                    color = "black",
                    width = 1
                  ),
                  hoverinfo="text",
                  text = "1:1 line")
  p1 <- add_trace(p1,
                  data = WY,
                  x = ~Conc_QSM,
                  y = ~Conc,
                  marker = list(
                    color = "rgba(0,0,0,0.2)",
                    line = list(
                      color = "rgba(0,0,0,0.6)",
                      width = 1
                    )),
                  hoverlabel = list(bgcolor="white"),
                  hoverinfo = "text",
                  text = paste0(WY$waterYear, "<br>", 
                              "WRTDS: ", signif(WY$Conc,3), " mg/L", "<br>",
                              "QSM: ", signif(WY$Conc_QSM,3), " mg/L"))
  
  p1 <- layout(p1,
               xaxis = list(title = "QSM"),
               yaxis = list(title = "WRTDS"),
               annotations = t1,
               margin = list(t = 60))
  
  p2 <- plot_ly(type = "scatter",
                mode = "markers",
                showlegend = FALSE)
  p2 <- add_trace(p2,
                  x = flux_range,
                  y = flux_range,
                  mode = "lines",
                  line = list(
                    color = "black",
                    width = 1
                  ),
                  hoverinfo="text",
                  text = "1:1 line")
  p2 <- add_trace(p2,
                  data = WY,
                  x = ~Flux_QSM,
                  y = ~Flux,
                  marker = list(
                    color = "rgba(0,0,0,0.2)",
                    line = list(
                      color = "rgba(0,0,0,0.6)",
                      width = 1
                    )),
                  hoverlabel = list(bgcolor="white"),
                  hoverinfo = "text",
                  text = paste0(WY$waterYear, "<br>", 
                              "WRTDS: ", signif(WY$Flux,3), " kg", "<br>",
                              "QSM: ", signif(WY$Flux_QSM,3), " kg"))
  
  p2 <- layout(p2,
               xaxis = list(title = "QSM"),
               yaxis = list(title = ""),
               annotations = t2,
               margin = list(t = 60))
  
  subplot(p1, p2, titleX = TRUE, titleY = TRUE)

}