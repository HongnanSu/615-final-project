library(shiny)
library(shinydashboard)
library(leaflet)
library(shinyjs)

# UI 
ui <- dashboardPage(
  dashboardHeader(title = "New Zealand"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Welcome", tabName = "welcome", icon = icon("home")),
      menuItem("New Zealand", tabName = "new_zealand", icon = icon("globe")),
      menuItem("Māori Culture", tabName = "maori_culture", icon = icon("users")),
      menuItem("Data Collection", tabName = "data_collection", icon = icon("table")),
      menuItem("Model Fitting", tabName = "model_fitting", icon = icon("chart-line")),
      menuItem("Prediction", tabName = "prediction", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    tabItems(
      # Welcome page
      tabItem(tabName = "welcome", 
              h2("Welcome to the New Zealand Dashboard"),
              p("This is Hongnan Su's 615 final project"),
              p("predict New Zealand GDP using previous dataset."),
              tags$img(src = "nz.png", width = "80%", height = "auto")
      ),
      
      # New Zealand Introduction with Map
      tabItem(tabName = "new_zealand",
              h3("Introduction of New Zealand"),
              tabsetPanel(
                id = "nz_subpages",
                type = "tabs",
                tabPanel("Map and Overview",
                         p("New Zealand is a beautiful island nation located in the southwestern Pacific Ocean. Known for its rich natural landscapes and unique culture, it is a popular destination for tourists and adventurers."),
                         p("**Geography**: New Zealand consists of two main islands—the North Island and the South Island—along with numerous smaller islands."),
                         fluidRow(
                           column(6,
                                  sliderInput("zoom_level", "Zoom Level:",
                                              min = 1, max = 18, value = 6, step = 1)
                           ),
                           column(6,
                                  div(id = "mapContainer", 
                                      leafletOutput("map", width = "100%", height = "500px"))
                           )
                         )
                ),
                tabPanel("History and Culture",
                         p("New Zealand has a rich history shaped by its indigenous Māori people and European settlers. The Māori, who are the indigenous Polynesian people of New Zealand, arrived from eastern Polynesia around the 13th century."),
                         p("- **Māori Culture**: Māori culture is rich in traditions, including the famous haka (a ceremonial dance), intricate tattooing (tā moko), and wood carving."),
                         p("- **European Colonization**: In the 18th century, European explorers, notably James Cook, arrived in New Zealand. The Treaty of Waitangi, signed in 1840, established British sovereignty."),
                         tags$img(src = "nz1.jpg", width = "80%", height = "auto")
                ),
                tabPanel("Natural Beauty and Economy",
                         p("New Zealand is renowned for its stunning natural beauty, including mountains, lakes, beaches, and geothermal areas."),
                         p("- **Southern Alps**: Home to the highest peak in New Zealand, Mount Cook (Aoraki)."),
                         p("- **Geothermal Areas**: Rotorua is famous for its geothermal activity, including hot springs, geysers, and mud pools."),
                         p("- **Fiords**: Milford Sound and Doubtful Sound, located on the South Island, are stunning fjords surrounded by dramatic cliffs, waterfalls, and lush forests."),
                         p("**Economy**: New Zealand's economy is driven by agriculture, tourism, and manufacturing. The country is a major global exporter of dairy products, wool, and meat."),
                         tags$img(src = "nz2.jpg", width = "80%", height = "auto")
                ),
                tabPanel("Language and Features",
                         p("New Zealand has three official languages: English, Māori, and New Zealand Sign Language. While English is the primary language spoken, Māori has experienced a revival."),
                         p("- **English**: English is the dominant language used in business, education, and day-to-day communication."),
                         p("- **Māori**: Māori is the language of the indigenous people, and there has been a strong push to preserve and promote its use."),
                         p("**Unique Features**: New Zealand is known for its love of sports, particularly rugby. The All Blacks rugby team is one of the most successful in the world."),
                         p("**Wildlife**: The kiwi, a flightless bird, is New Zealand's national symbol."),
                         tags$img(src = "nz3.jpg", width = "80%", height = "auto")
                )
              )
      ),
      
      # Māori Culture Page
      tabItem(tabName = "maori_culture", 
              h3("Introduction of Māori Culture"),
              p("The Māori people are the indigenous people of New Zealand, with a rich cultural heritage, history, and traditions. They migrated from Polynesia to New Zealand around the 13th century, and their culture continues to play a central role in the identity of New Zealand today."),
              tabsetPanel(
                id = "maori_subpages",
                type = "tabs", 
                tabPanel("Origins and Migration",
                         h4("Origins and Migration"),
                         p("The Māori are believed to have originated from eastern Polynesia. According to Māori oral tradition, they migrated to New Zealand in a series of waves from the 9th to the 13th century, navigating vast distances across the Pacific Ocean in large canoes known as waka."),
                         tags$img(src = "maoli.jpg", width = "80%", height = "auto")
                         ),
                
                tabPanel("Language",
                         h4("Māori Language"),
                         p("Māori is one of New Zealand’s official languages and holds cultural significance. Efforts to preserve and promote the language have been supported through initiatives such as Māori-language television, radio stations, and schools."),
                         tags$img(src = "maori1.webp", width = "80%", height = "auto")
                ),
                tabPanel("Social Structure and Tribal System",
                         h4("Social Structure and Tribal System"),
                         p("The Māori people traditionally lived in tribal groups known as iwi, which are made up of smaller sub-groups called hapū. These tribes were governed by chiefs, or rangatira, and the social structure was hierarchical."),
                         tags$img(src = "maori2.webp", width = "80%", height = "auto")
                ),
                tabPanel("Māori Religion and Beliefs",
                         h4("Māori Religion and Beliefs"),
                         p("The Māori worldview is based on a deep connection to the land (whenua) and nature. They believe in spiritual forces that inhabit the environment, including the land, sea, and sky."),
                         tags$img(src = "maori3.jpg", width = "80%", height = "auto")
                )
              )
      ),
      
      # Data Collection Page
      tabItem(tabName = "data_collection",
              h3("Data Collection"),
              h4("Dataset"),
              tableOutput("data_nz"),
              plotOutput("data_plot")
      ),
      
      # Model Fitting Page
      tabItem(tabName = "model_fitting", 
              h3("Model Fitting"),
              p("The method I used is `stan_glm`."),
              p("regression <- stan_glm(gdp ~ income2 + population2 + input + output + inflation2, data_nz, family = gaussian)"),
              p("MCMC showed that Rhat is 1, meaning most variables fit well."),
              tags$img(src = "MCMC.png", width = "80%", height = "auto")
      ),
      
      # Prediction Page
      tabItem(tabName = "prediction", 
              h3("Prediction"),
              p("In this section, we will make prediction of 2024 New Zealand GDP using the fitted model."),
              p("predicted_gdp_2024<- posterior_predict(regression2, newdata = data_2024_nz)
GDP_2024<-mean(predicted_gdp_2024)
GDP_2024
"),
              textOutput("prediction_output")
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 174.7762, lat = -36.8509, zoom = input$zoom_level) 
  })
  
  observe({
    leafletProxy("map", data = NULL) %>%
      setView(lng = 174.7762, lat = -36.8509, zoom = input$zoom_level) 
  })
  
  # Data Collection
  data_nz <- read.csv("data_nz.csv")
  
  output$data_nz <- renderTable({
    data_nz
  })
  
  output$data_plot <- renderPlot({
    library(ggplot2)
    library(tidyr) 
    df_long <- data_nz %>%
      gather(key = "Variable", value = "Value", gdp, income2, population2, input, output, visitor2, inflation2)
    
    ggplot(df_long, aes(x = year, y = Value, color = Variable)) +
      geom_line(size = 1) + 
      theme_minimal() + 
      labs(title = "Time Series of Variables", x = "Year", y = "Value") +
      theme(legend.title = element_blank())
  })
  
  # Model Fitting 
  output$model_plot <- renderPlot({
    plot(1:10, 1:10, main = "Model Fitting Example", xlab = "X", ylab = "Y")
  })
  
  # Prediction
  output$prediction_output <- renderText({
    
    paste("Predicted Value:",  243930143788)  
  })
}



shinyApp(ui, server)