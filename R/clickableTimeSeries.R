#' Interactive time series plot for linelist data with a clickable feature.
#'
#' This code will later be merged with driver.R to create one super duper dynamic time series.
#' To be made 'functionable' at a more reasonable hour.
#'
#' @param
#' @return
#' @author Rolina D. van Gaalen
#' @export

library(dygraphs)
lungDeaths <- cbind(mdeaths, fdeaths)
dimnames(lungDeaths)[[2]] <- c("male deaths", "female deaths")
x = dygraph(lungDeaths)

ui = shinyUI(fluidPage(
  dygraphOutput("plot"),
#   textOutput("text")
  plotOutput("plot2")
))

server = shinyServer(function(input, output, session){
  output$plot = renderDygraph({
     x$x$attrs$zoomCallback = htmlwidgets::JS(
#        "function(minDate, maxDate, yRanges){
#          Shiny.onInputChange('mydata', JSON.stringify([minDate/(24*60*60*1000), maxDate/(24*60*60*1000)]))
#        }"
       "function(minDate, maxDate, yRanges){
         Shiny.onInputChange('mydata', JSON.stringify([minDate/(24*60*60*1000), maxDate/(24*60*60*1000)]))
       }"
      )
     x
    })
#   output$text = renderText(
#     #input$mydata
#       if (!is.null(input$mydata)) {input$mydata}
#       else {"no input"}
  output$plot2 = renderPlot(
      if (!is.null(input$mydata)) {
        range <- unique(na.omit(as.numeric(unlist(strsplit(unlist(input$mydata), "[^0-9.]+")))))
        date.data <- as.numeric(seq(as.Date("1974-01-01"),as.Date("1979-12-01"), by="month"))
        minRange <- which(abs(date.data-range[1])==min(abs(date.data-range[1])))
        maxRange <- which(abs(date.data-range[2])==min(abs(date.data-range[2])))
        data <- melt(lungDeaths, varnames=c("time","deaths"))

        ggplot(data[data$time %in% minRange:maxRange,], aes(x=value)) +
          facet_wrap(~deaths) +
          geom_histogram(binwidth=50)
          #geom_density()
        #hist(lungDeaths[minRange:maxRange,1], breaks=20, main=paste("Data at ", as.Date(minRange, origin="1970-01-01"))) #toString(input$mydata)))
      } #else plot(1:10, main="")
    )
})

runApp(list(ui = ui, server = server))
