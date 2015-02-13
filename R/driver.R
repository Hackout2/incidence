# --------------------------------------------------------------------------
# PRELIMINARIES
# --------------------------------------------------------------------------

# --- Set working directory
setwd("/Users/rolinavangaalen/Documents/Hackout/time/")

# --- Define required libraries
library(shiny)
library(dygraphs)
library("linelist2ts")

# --- Load the data
data(hagelloch.obk)
df <- hagelloch.obk@individuals
names(df)[names(df)=="SEX"] <- "Sex"
names(df)[names(df)=="AGE"] <- "Age"
df$Age <- as.factor(df$Age)

# --- Set options for the function
main.title <- "Interactive time series"

# --------------------------------------------------------------------------
# THE FUNCTION
# --------------------------------------------------------------------------

driver <- function(main.title, case.times, dropdown) {

  drop_ = function(i){
    selectInput(dropdown[i],
                label = dropdown[i],
                choices = c("all", levels(df[,dropdown[i]])),
                selected = "all")
  }

  # --- USER INTERFACE
  ui = shinyUI(fluidPage(

    # Application title
    titlePanel(main.title),

    # Sidebar with a slider input for the number of bins
    sidebarLayout(
      sidebarPanel(

        do.call(tagList, lapply(seq_along(dropdown), drop_)),

        selectInput("var.2",
                    label = "Break by:",
                    choices = c("days", "weeks", "months"),
                    selected = "days"),

        dateRangeInput("range", "Dates",
                       start = min(df$ERU), end = max(df$ERU),
                       min = min(df$ERU), max = max(df$ERU),
                       format = "yyyy-mm-dd", startview = "month",
                       weekstart = 0, language = "en", separator = " to ")

      ),

      # Show a plot of the generated distribution
      mainPanel(
        h3(plotOutput("distPlot"),
           dygraphOutput("dygraph"))
      )
    )
  ))

  # --- SERVER
  server = shinyServer(function(input, output) {

    output$distPlot <- renderPlot({

      date.min <- as.Date(input$range[1], origin = "1970-01-01")
      date.max <- as.Date(input$range[2], origin = "1970-01-01")

      #Subset the data
      df.subset <- (date.min<=df$ERU & df$ERU<=date.max)

      for (i in seq_along(dropdown)) {
        if ((input[[dropdown[i]]] != "all") & (!is.null(dropdown))) df.subset <- df.subset & (df[,dropdown[i]] %in% input[[dropdown[i]]])
        if ((input[[dropdown[i]]] == "all") & (!is.null(dropdown))) df.subset <- df.subset & (df[,dropdown[i]] %in% levels(df[,dropdown[i]]))
      }

      if (input$var.2 == "days") incidence.title = "Daily"
      if (input$var.2 == "weeks") incidence.title = "Weekly"
      if (input$var.2 == "months") incidence.title = "Monthly"

      # Draw the histogram
      hist(df[df.subset,case.times], breaks = input$var.2, start.on.monday=FALSE, col = 'skyblue', border = 'white', freq=TRUE,
           xlab=paste(incidence.title,"incidence",sep=" "), main="")
    })

    output$dygraph <- renderDygraph({

      df.subset <- rep(TRUE, dim(df)[1])
      for (i in seq_along(dropdown)) {
        if ((input[[dropdown[i]]] != "all") & (!is.null(dropdown))) df.subset <- df.subset & (df[,dropdown[i]] %in% input[[dropdown[i]]])
        if ((input[[dropdown[i]]] == "all") & (!is.null(dropdown))) df.subset <- df.subset & (df[,dropdown[i]] %in% levels(df[,dropdown[i]]))
      }
      df.subset <- df[df.subset,]

      if (input$var.2 == "days") incidence.title = "Daily"
      if (input$var.2 == "weeks") incidence.title = "Weekly"
      if (input$var.2 == "months") incidence.title = "Monthly"

      df_temp <- data.frame(table(factor(as.character(df.subset$ERU), levels=as.character(seq(min(df$ERU), max(df$ERU), by=1)))))
      names(df_temp) <- c("date", "incidence")
      df_temp$date <- as.Date(df_temp$date, origin="1970-01-01")

      #dygraph(predicted(), main = "Predicted Deaths/Month") %>%
      dygraph(xts(df_temp$incidence, as.Date(df_temp$date, format='%m/%d/%Y')), main = "") %>%
        dyOptions(drawGrid = input$showgrid) %>%
        dyRangeSelector(dateWindow = c(as.character(min(df_temp$date)), as.character(max(df_temp$date))))

    })

  })

  runApp(list(ui = ui, server = server), launch.browser = getOption("shiny.launch.browser"))
  #runApp("shiny_timeseries", display.mode = "showcase")

}

driver(main.title, case.times="ERU", dropdown=c("Sex", "Age"))
driver(main.title, case.times="ERU", dropdown=NULL)
