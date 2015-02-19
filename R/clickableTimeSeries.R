#' Interactive time series plot for linelist data with a clickable feature.
#'
#' This code will later be merged with driver.R to create one super duper dynamic time series.
#' To be made 'functionable' at a more reasonable hour.
#'
#' @param df - data frame; contains the dates and other variables (for demo, type the string "demo")
#' @param date.variable - string; define which variable in the data frame is the date variable
#' @param x.axis.choices - vector of strings; define the variable(s) that may be selected for
#' the x axis
#' @param x.axis.selected.choice - string; define the variable that is selected by default;
#' default is "None"
#' @param facets.col.choices -
#' @param facets.col.selected.choice -
#' @param facets.row.choices -
#' @param facets.row.selected.choice -
#' @param main.title -
#' @param main.description -
#' @return
#' @author Rolina D. van Gaalen
#' @import dygraphs reshape2 plyr
#' @export

# To do:
# Include maps


# --- Define required libraries
#library(dygraphs)
#library(reshape2)
#library(plyr)

interactive <- function(df, date.variable, x.axis.choices, x.axis.selected.choice,
                        facets.col.choices, facets.row.choices,
                        main.title=NULL, main.description=NULL,
                        facets.col.selected.choice="None",
                        facets.row.selected.choice="None") {

  if (df == "demo") {
    data(hagelloch.obk)
    df <- hagelloch.obk@individuals
    df <- rename(df, replace=c("SEX"="Sex", "AGE"="Age", "CL"="Class", "C"="Complications"))
  }

  names(df)[names(df)==date.variable] <- "date"

  # --- USER INFERFACE
  ui = shinyUI(fluidPage(
    tabsetPanel(
      tabPanel("Interactive Time Series - Region",
               if (exists("main.title")) h3(main.title),
               if (exists("main.description")) h5(main.description),
               dygraphOutput("dyplot", height=300),
               hr(),
               fluidRow(
                 column(4,
                        h4("Summary statistics on selected region"),
                        if (length(x.axis.choices) > 1) {
                          selectInput("x",
                                      label = "X axis",
                                      choices = x.axis.choices,
                                      selected = x.axis.selected.choice)},
                        selectInput("facet_col",
                                      label = "Facet Column",
                                      choices = c("None", facets.col.choices),
                                      selected = facets.col.selected.choice),
                        selectInput("facet_row",
                                      label = "Facet Row",
                                      choices = c("None", facets.row.choices),
                                      selected = facets.row.selected.choice)),
                 column(8,
                        plotOutput("plot2")))),
      tabPanel("Interactive Time Series - Point",
               if (exists("main.title")) h3(main.title),
               if (exists("main.description")) h5(main.description),
               dygraphOutput("dyplot_point", height=300),
               hr(),
               fluidRow(
                 column(4,
                        h4("Summary statistics on selected region"),
                        if (length(x.axis.choices) > 1) {
                          selectInput("x_p",
                                      label = "X axis",
                                      choices = x.axis.choices,
                                      selected = x.axis.selected.choice)},
                        selectInput("facet_col_p",
                                    label = "Facet Column",
                                    choices = c("None", facets.col.choices),
                                    selected = facets.col.selected.choice),
                        selectInput("facet_row_p",
                                    label = "Facet Row",
                                    choices = c("None", facets.row.choices),
                                    selected = facets.row.selected.choice)),
                 column(8,
                        plotOutput("plot_point")))),
              # plotOutput("plot_point")),
      tabPanel("Non-interactive histogram", plotOutput("plot")))
  ))

# --- SERVER
server = shinyServer(function(input, output, session){

  # --- Define the main plots
  df_temp <- data.frame(table(factor(as.character(df$date), levels=as.character(seq(min(df$date), max(df$date), by=1)))))
  names(df_temp) <- c("date", "total.incidence")
  df_temp$date <- as.Date(df_temp$date, origin="1970-01-01")

  x <- x2 <-  dygraph(xts(df_temp$total.incidence, as.Date(df_temp$date, format='%m/%d/%Y')), main = "") %>%
    dyRangeSelector(dateWindow = c(as.character(min(df_temp$date)), as.character(max(df_temp$date))))
#   x2 = dygraph(xts(df_temp$total.incidence, as.Date(df_temp$date, format='%m/%d/%Y')), main = "") %>%
#     dyRangeSelector(dateWindow = c(as.character(min(df_temp$date)), as.character(max(df_temp$date))))

  # This is the non-interactive histogram
  output$plot = renderPlot(
    hist(df$date, start.on.monday=FALSE, breaks="days", col = 'skyblue', border = 'white', freq=TRUE,
         xlab=paste("Daily ","incidence",sep=" "), main=""))

  # This is for the interactive region plot
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
      if (length(x.axis.choices) > 1) input.x <- input$x
      if (length(x.axis.choices) == 1) input.x <- x.axis.choices
      if (length(x.axis.choices) < 1) print("Please choose one variable for the x axis")

      if ((input$facet_col != "None") | (input$facet_row != "None")) {
        ggplot(df[df$date>range[1] & df$date<range[2],], aes_string(x=input.x)) +
          facet_grid(as.formula(paste(facet.row, "~", facet.col))) +
          geom_histogram(binwidth=1) +
          #geom_density() +
          labs(title = " ", y="Number of cases")
      } else {
        ggplot(df[df$date>=range[1] & df$date<=range[2],], aes_string(x=input.x)) +
          geom_histogram(binwidth=1) +
          labs(title = " ", y = "Number of cases")}}
  )

  output$dyplot_point = renderDygraph({
    x2$x$attrs$clickCallback = htmlwidgets::JS(
      "function(e, x, point){
         Shiny.onInputChange('mydata_point', JSON.stringify(point))}")
    x2 })

#   output$plot_point = renderPlot(
#     if (!is.null(input$mydata_point)) {
#
#       ggplot(df[df$ERU == point,], aes_string(x="Age")) +
#         facet_grid(as.formula(paste("~", "Sex"))) +
#         geom_histogram(binwidth=1) +
#         labs(title = " ", y = "Number of cases")
#
#     })


  output$plot_point = renderPlot(

    if (!is.null(input$mydata_point)) {
      point <- unique(na.omit(as.numeric(unlist(strsplit(unlist(input$mydata_point), "[^0-9.-]+")))))
      point <- round(point[3]/(24*60*60*1000))

      facet.row.p <- input$facet_row_p
      facet.col.p <- input$facet_col_p

      if (input$facet_row_p == "None") facet.row.p <- "."
      if (input$facet_col_p == "None") facet.col.p <- "."
      if (length(x.axis.choices) > 1) input.x.p <- input$x_p
      if (length(x.axis.choices) == 1) input.x.p <- x.axis.choices
      if (length(x.axis.choices) < 1) print("Please choose one variable for the x axis")

      if ((input$facet_col_p != "None") | (input$facet_row_p != "None")) {
        ggplot(df[df$date == point,], aes_string(x=input.x.p)) +
          facet_grid(as.formula(paste(facet.row.p, "~", facet.col.p))) +
          geom_histogram(binwidth=1) +
          #geom_density() +
          labs(title = " ", y="Number of cases")
      } else {
        ggplot(df[df$date == point,], aes_string(x=input.x.p)) +
          geom_histogram(binwidth=1) +
          labs(title = " ", y = "Number of cases")}}
  )


})

runApp(list(ui = ui, server = server))
}

# interactive(df=df, date.variable=date.variable, x.axis.choices=x.axis.choices,
#             x.axis.selected.choice=x.axis.selected.choice, facets.col.choices=facets.col.choices,
#             facets.row.choices=facets.row.choices, main.title="Interactive time series plot",
#             main.description="Plot description.", facets.col.selected.choice="None",
#             facets.row.selected.choice="None")


#demo
interactive(df="demo", date.variable="ERU", x.axis.choices=c("Sex", "Age", "Class", "Complications"),
            x.axis.selected.choice="Sex", facets.col.choices=c("Sex", "Age", "Class", "Complications"),
            facets.row.choices=c("Sex", "Age", "Class", "Complications"),
            main.title="Interactive time series plot", main.description="Plot description.",
            facets.col.selected.choice="None", facets.row.selected.choice="None")




