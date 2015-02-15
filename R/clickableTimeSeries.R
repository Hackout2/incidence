#' Interactive time series plot for linelist data with a clickable feature.
#'
#' This code will later be merged with driver.R to create one super duper dynamic time series.
#' To be made 'functionable' at a more reasonable hour.
#'
#' @param
#' @return
#' @author Rolina D. van Gaalen
#' @export

# To do: merge with driver.R
# Click and zoom
# Click but don't zoom
# Point click

# --- Define required libraries
library(dygraphs)
library(reshape2)

# --- Load the data and make it suitable for the program
data(hagelloch.obk)
df <- hagelloch.obk@individuals
names(df)[names(df)=="SEX"] <- "Sex"
names(df)[names(df)=="AGE"] <- "Age"
names(df)[names(df)=="CL"] <- "Class"
names(df)[names(df)=="C"] <- "Complications"

# --- Define the main plot
df_temp <- data.frame(table(factor(as.character(df$ERU), levels=as.character(seq(min(df$ERU), max(df$ERU), by=1)))))
names(df_temp) <- c("ERU", "total.incidence")
df_temp$date <- as.Date(df_temp$ERU, origin="1970-01-01")
x = dygraph(xts(df_temp$total.incidence, as.Date(df_temp$ERU, format='%m/%d/%Y')), main = "")

# --- USER INFERFACE
ui = shinyUI(fluidPage(
  title = "Hagelloch cases",
  tabsetPanel(
    tabPanel("Non-interactive histogram", plotOutput("plot")),
    tabPanel("Interactive DY Graph",
            dygraphOutput("dyplot"),
            hr(),
            fluidRow(
              column(3,
                     h4("Hagelloch cases"),
                     selectInput("x",
                                 label = "X axis",
                                 choices = c("Sex", "Age", "Class", "Complications"),
                                 selected = "Age"),
                     selectInput("facet_col",
                                 label = "Facet Column",
                                 choices = c("None", "Sex", "Age", "Class", "Complications"),
                                 selected = "None"),
                     selectInput("facet_row",
                                 label = "Facet Row",
                                 choices = c("None", "Sex", "Age", "Class", "Complications"),
                                 selected = "None")),
              column(6, offset = 1,
                     plotOutput("plot2")))))
))

# --- SERVER
server = shinyServer(function(input, output, session){

  output$plot = renderPlot(
    hist(df[,"ERU"], start.on.monday=FALSE, breaks="days", col = 'skyblue', border = 'white', freq=TRUE,
         xlab=paste("Daily ","incidence",sep=" "), main=""))

  output$dyplot = renderDygraph({
     x$x$attrs$zoomCallback = htmlwidgets::JS(
       "function(minDate, maxDate, yRanges){
         Shiny.onInputChange('mydata', JSON.stringify([minDate/(24*60*60*1000), maxDate/(24*60*60*1000)]))}")
     x })

  output$plot2 = renderPlot(

    if (!is.null(input$mydata)) {
      range <- unique(na.omit(as.numeric(unlist(strsplit(unlist(input$mydata), "[^0-9.-]+")))))

     facet.row <- input$facet_row
     facet.col <- input$facet_col

     if (input$facet_row == "None") facet.row <- "."
     if (input$facet_col == "None") facet.col <- "."

     if ((input$facet_col != "None") | (input$facet_row != "None")) {
       ggplot(df[df$ERU>range[1] & df$ERU<range[2],], aes_string(x=input$x)) +
         facet_grid(as.formula(paste(facet.row, "~", facet.col))) +
         geom_histogram(binwidth=1) +
         labs(title = " ", y="Number of cases")
       # geom_density()
     } else {
       ggplot(df.subset[df$ERU>range[1] & df$ERU<range[2],], aes_string(x=input$x)) +
         geom_histogram(binwidth=1) +
         labs(title = " ", y = "Number of cases")}}
    #else plot(1:10, main="")
  )
})

runApp(list(ui = ui, server = server))
