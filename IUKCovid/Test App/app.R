#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

session <- "server-setup" # it is assumed the code run at server start-up, until session is overwritten.

reportLine <- function(message, session = NULL, print = T){

    msgSplit <- unlist(str_split(message,"\n"))

    session <- ifelse(is.character(session), session, session$token)

    doWrite <- !is.null(session) && exists("logFilename") && file.exists(logFilename)

    lapply(msgSplit, function(msg){
        line = paste0(now(), "\t", session, "\t", msg)

        if (doWrite) write(line, file = logFilename, append = TRUE)

        if (print) cat(line, "\n")
    })

    invisible()
}

foo <- function(x = "foo") reportLine(x, session)

foo()

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    foo()

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        foo(x = input$bins)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application
shinyApp(ui = ui, server = server)
