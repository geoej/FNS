# Define UI for application that draws a histogram
library(shiny)
library(ggplot2)
library(reshape2)
library(plotly)

nutdata <- read.csv("Data1.csv",header = T)
nutdata$new = 1
dataM <- melt(nutdata[,2:12], id.vars = "new")
dataN <- data.frame(dataM, crop=nutdata$crop)

ui <- fluidPage(theme = "bootstrap.css",
  
  # Application title
 titlePanel("Underutilised Food Crops for Sustainable Nutrition Security"),
  
 
  shinyUI(navbarPage("",theme="/bootstrap/css/bootstrap.css",
          navbarMenu("HOW DO THEY COMPARE?",
          tabPanel("All Nutrients", 
                   sidebarLayout(
                     sidebarPanel(
                       tags$br(""),
                       img(src='Capture.PNG', align="middle",height = 250, width = 250),
                       tags$br(""),
                       tags$br(""),
                      "A visualisation made by the",
                       "Crops For the Future", img(src='logo.png', align="right",height = 50, width = 50)
                     ),
                     mainPanel (plotOutput("allplot"))
                     
                   )
                   
                   ),                   
          tabPanel("Individual Nutrient Element", 
                   sidebarLayout(
                   sidebarPanel(
                   selectInput(inputId="variable",
                               label = "Select Element", 
                               choices = c("Energy" = "Energy_kcal",
                                           "Protein" = "Protein_g",
                                           "Fat" = "Fat_g", 
                                           "Calcium" = "Calcium_mg",   
                                           "Iron" = "Iron_mg", 
                                           "Zinc" = "Zinc_mg",
                                           "Thiamine" = "Thiamine_mg",
                                           "Ribioflavin" = "Riboflavin_mg",
                                           "Folic Acid" = "Folic_acid_mg",
                                           "Fiber" = "Fiber_g")
                   ),
                   tags$br("")
                   ),
                   mainPanel (plotOutput("ggplot"))
                   
                   )
                   
                  )),
          tabPanel("PREVALANCE OF MALNUTRITION", 
                   sidebarLayout(
                     sidebarPanel(
                       selectInput(inputId="malnut",
                                   label = "Select Type of Malnutrition", 
                                   choices = c("Food insecure households" =  "Food.insecure.households",
                                               "Underweight women age 14-49" =  "underweight.womeny15.49y",                    
                                               "Staunting" =  "Stunting",                                    
                                               "Wasting" =  "Wasting",                                     
                                               "Underweight" =  "Underweight",                                 
                                               "Calcium Deficiency non pregnant women age 14-49" =  "Calcium.deficiency.Non.pregnant.women.15.49y",
                                               "Calcium Deficiency pregnant women age 14-49" =  "Calcium.deficiency.pregnant.women.15.49y")
                                   )),
                      mainPanel(plotlyOutput("mymap"),tableOutput("maldata") )
                   )
                   ),
          tabPanel("Nutrient Data", tableOutput("data"))
        )
 ),
 tags$br("Data from:"),
 tags$em("Adhikari, L., Hussain, A., & Rasul, G. (2017). Tapping the Potential of Neglected and Underutilized Food Crops for Sustainable Nutrition Security in the Mountains of Pakistan and Nepal. Sustainability, 9(2), 291.") 
)

# Define server logic 
server <- function(input, output) {
  
  output$ggplot <- renderPlot({
    # print(ggplot(data=nutdata, aes_string(x = factor(nutdata$crop,levels=unique(nutdata$crop)),
    #                                     y = "Protein_g", fill=nutdata$crop))+
    #    geom_bar(stat="identity")) #+ 
    #    theme(legend.position="none") + 
    #     ylab(input$variable)+ 
    #    xlab("Crops")+ 
    #     coord_flip()
    
    p <- ggplot(data=as.data.frame(nutdata), aes_string(x = factor(nutdata$crop,levels=unique(nutdata$crop)),
                                                         y = input$variable, fill="crop"))+
            geom_bar(stat="identity")+
            coord_flip()+ xlab("")+
            theme(axis.text=element_text(size=16),
                  axis.title=element_text(size=16,face="bold"),legend.position="none") 
  
    #ggplotly(p)
    print(p)
    })
  
  output$allplot <- renderPlot({
    p <- ggplot(dataN, aes(factor(crop,levels=unique(crop)), 
                      value, fill = variable, color= variable)) + 
      geom_bar(stat="identity",na.rm = T) +
      coord_flip()+ xlab("")+
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"),legend.position="bottom") 
    
      #ggplotly(p)
    print(p)
    
    })
  
  
  output$data <- renderTable({
    nutdata[]
  })

  
#####################  
  malnut <- read.csv("malnut4.csv",header = T)
  
  # light grey boundaries
  l <- list(color = toRGB("grey"), width = 0.5)
  
  # specify map projection/options
  g <- list(
    scope = "asia",
    showframe = TRUE,
    showcoastlines = TRUE,
    locationmode = "country names",
    showcountries = T,
    projection = list(type = 'Mercator'),
    showland = T
  )
  
  output$mymap <- renderPlotly({
    plot_geo(malnut) %>%
      add_trace(
        z = malnut[,input$malnut], locations = ~code)  %>%
      colorbar(title = "National Average") %>%
      layout(
        title = input$malnut,
        geo = g
      )
  })
  
  
  
 
  
  output$maldata <- renderTable({
    malnut[,c( "country",input$malnut)]
  })
  
  
 ##################### 
  
  
  
  }

shinyApp(ui, server)
