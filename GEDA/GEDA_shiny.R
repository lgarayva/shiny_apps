#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library("readr")
library("stringr")
library("tidyr")
library("dplyr")
library("ggplot2")
library("ggthemes")
library("DiagrammeR")
library("patchwork")
library("purrr")
library("shiny")
library("plotly")
library("GGally")

# Si no se han cargado en la sesión los datos, es necesario descomentar la siguiente instrucción
#datos <- readRDS("../autos_limpios.rds")%>% readr::type_convert()
#Si no se cargaron los datos, es necesario comentar la siguiente instrucción
datos <- iris
numericas <- select_if(datos, is.numeric) %>% names()
categoricas <- select(datos, -numericas) %>% names()
cols_multi <- map_dbl(numericas, ~which(colnames(datos) == .x))


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Graphical Exploratory Data Analysis"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            #selectizeInput("numeric", "Variables Numéricas", numericas),
            #selectizeInput("categoric", "Variables Categóricas", categoricas),
            conditionalPanel(condition = "input.tabs == 'Histograma'",
                             sliderInput("bins",
                                         "Number of bins:",
                                         min = 1,
                                         max = 50,
                                         value = 30)
                             , selectInput("numeric", "Variables Numéricas", numericas)),
            
            conditionalPanel(condition = "input.tabs == 'Barras'", #| input.tabs == 'Multivariado'",
                             selectInput("categoric", "Variables Categóricas", categoricas)),
            
            conditionalPanel(condition = "input.tabs == 'Boxplot' | input.tabs == 'Summary'"
                             , selectInput("nombre", "Característica", names(datos))),
            
            #conditionalPanel(condition = "input.tabs == 'Boxplot2'"
            #                 , selectInput("categoric_bp2", "Variables Categóricas", categoricas)
            #                 , selectInput("numeric_bp2", "Variables Numéricas", numericas)),
            #
            conditionalPanel(condition = "input.tabs == 'Scatter'"#| input.tabs == 'Boxplot2'"
                             , selectInput("variable_1", "Característica", names(datos))
                             , selectInput("variable_2", "Característica", names(datos))),
            
            conditionalPanel(condition = "input.tabs == 'Boxplot Bivariado'"
                             , selectInput("variable_3", "Categóricas", categoricas)
                             , selectInput("variable_4", "Numéricas", numericas)),
            
            conditionalPanel(condition = "input.tabs == 'Multivariado'",
                             selectInput("categoric_multi", "Grupo", categoricas)),
            
            conditionalPanel(condition = "input.tabs == 'Multivariado Scatter'"
                             , selectInput("variable_num1", "Característica", numericas)
                             , selectInput("variable_num2", "Característica", numericas)
                             , selectInput("grupo", "Grupo", categoricas))
            
            # conditionalPanel(condition = "input.tabs == 'Multivariado'"
            #                  , selectInput("variable_5", "Categóricas", categoricas)
            #                  , selectInput("variable_6", "Numéricas", numericas))
            # 
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs", id="tabs",
                        #tabPanel("Resumen", verbatimTextOutput("resumen")),
                        tabPanel("Histograma", plotlyOutput("distPlot")),
                        tabPanel("Barras", plotlyOutput("BarPlot")),
                        tabPanel("Summary", verbatimTextOutput("summary")),
                        tabPanel("Boxplot", plotlyOutput("boxplot")),
                        tabPanel("Boxplot Bivariado", plotlyOutput("boxplot2")),
                        tabPanel("Scatter", plotlyOutput("scatter")),
                        tabPanel("Multivariado",plotlyOutput("multivariado")),
                        tabPanel("Multivariado Scatter",plotlyOutput("multivariado2"))
            )
        )        
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$distPlot <- renderPlotly({
        # generate bins based on input$bins from ui.R
        x    <- datos[[input$numeric]]
        bins <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        p <-ggplot(datos)+
            geom_histogram(mapping=aes_string(input$numeric, na.rm = TRUE), breaks = bins)+
            ylab("Total")+
            theme_classic()
        ggplotly(p)
    })
    
    output$BarPlot <- renderPlotly({
        #p <-ggplot(datos)+geom_histogram(mapping=aes_string(input$numeric), breaks = bins)
        
        p <- ggplot(datos, aes_string(input$categoric, color = input$categoric))+
            geom_bar()+ theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
            xlab(input$numeric)+ylab("Total")
        
        ggplotly(p)
    })
    
    
    # Generate a summary of the data ----
    output$summary <- renderPrint({
        summary(datos[input$nombre])
        #, glimpse(mtcars)
    })
    
    
    output$boxplot <- renderPlotly({
        variable <- input$nombre
        
        p <-ggplot(datos)+
            geom_boxplot(mapping=aes_string(y=variable))
        
        ggplotly(p)
    })
    
    
    output$boxplot2 <- renderPlotly({
        variable_1 <- input$variable_3
        variable_2 <- input$variable_4
        p <-ggplot(datos)+
            geom_boxplot(mapping=aes_string(x=variable_1,y=variable_2, color= variable_1))
        
        ggplotly(p)
    })
    
    
    output$scatter <- renderPlotly({
        variable_1 <- input$variable_1
        variable_2 <- input$variable_2
        p <-ggplot(datos)+
            geom_point(mapping=aes_string(x=variable_1,y=variable_2))
        
        ggplotly(p)
    })
    
    output$multivariado <- renderPlotly({
        
        ggparcoord(data = datos, columns = cols_multi, groupColumn = input$categoric_multi, scale="uniminmax") + 
            xlab("") + ylab("")
    })
    
    output$multivariado2 <- renderPlotly({
        
        p <- ggplot(datos, aes_string(input$variable_num1,input$variable_num2))+
            geom_point()+
            geom_smooth(method = "lm")+
            facet_wrap(facets=input$grupo)
        
        ggplotly(p)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
