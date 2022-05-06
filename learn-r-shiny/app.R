library(shiny)
library(ggplot2)
library(RColorBrewer)

# Parameters
font_size = 15
line_size = 1
MAX_X = 12
MAX_Y = 10

# Define UI for application that draws a point graph
ui <- fluidPage(
    # Sidebar with a slider input for number of value 
    sidebarLayout(
        sidebarPanel(
            sliderInput("value",
                        "On a scale of 1 to 10 (10 = super excited), how excited are you to learn R?",
                        min = 1,
                        max = MAX_Y,
                        value = MAX_Y)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot", height = 300, width = 350)
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # Make data frame
        df=data.frame(x=as.factor(1:MAX_X))
        df$y <- c(rep(input$value, times=input$value+(MAX_X-11)), input$value:MAX_Y)
        
        # Plot
        ggplot(df, aes(x, y))+
            geom_point(aes(fill=y), colour="black",pch=21, size=5, alpha=0.8)+
            labs(y="Your Excitement Level",
                 x="Time Taken to Read Book")+
            theme_classic()+    
            scale_y_continuous(breaks=1:MAX_Y, limits=c(0,MAX_Y))+ 
            # scale_fill_brewer(aes(fill=y), palette = "Dark2")
            theme(
                panel.background = element_blank(), # remove background color and lines
                axis.line = element_line(colour = "black", size = line_size), # increase the axis-line thickness and change the color to blac
                # Axes ticks
                axis.ticks = element_line(colour = "black", size = line_size), # increase the tick thickness)
                axis.ticks.length = unit(.25, "cm"),
                # Axes labels
                axis.text = element_text(colour = "black", size = font_size),
                axis.text.x = element_blank(), #element_text(margin = margin(t = 7, r = 0, b = 0, l = 0)),#, hjust = 1, vjust = 1, angle = 45), # increase space between x axis title and labels
                axis.text.y = element_text(margin = margin(t = 0, r = 7, b = 0, l = 0)),
                # Axes titles
                axis.title = element_text(colour = "black", size = font_size, face = "bold"), # axes title labels
                axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)), # increase space between x axis title and labels
                axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
                # Legend
                legend.position = "none",
            )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
