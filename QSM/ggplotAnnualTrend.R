ggplotAnnualTrend <- function(eList) {
  require(ggplot2)
  require(gridExtra)
  WY <- eList$WY
  
  p1 = ggplot() +
    geom_point(data = WY[WY$FN=="Annual" & WY$measure=="Concentration",], aes(waterYear, value)) +
    geom_line(data = WY[WY$FN=="Flow-Normalized" & WY$measure=="Concentration",], aes(waterYear, value)) +
    facet_grid( . ~ model) +
    ylab("Concentration (mg/L)") +
    theme_light()
  p1
  
  p2 = ggplot() +
    geom_point(data = WY[WY$FN=="Annual" & WY$measure=="Flux",], aes(waterYear, value)) +
    geom_line(data = WY[WY$FN=="Flow-Normalized" & WY$measure=="Flux",], aes(waterYear, value)) +
    facet_grid( . ~ model) +
    ylab("Flux (kg)") +
    theme_light()
  p2

  grid.arrange(p1, p2, nrow=2)
}