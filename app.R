
library(shiny)
library(shinythemes)


# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                withMathJax(),
                navbarPage(
                  # theme = "cerulean",  # <--- To use a theme, uncomment this
                  "Diabetes app",
                  tabPanel("delete",
                           sidebarPanel(
                             tags$h3("Input:"),
                             textInput("txt1", "Given Name:", ""),
                             textInput("txt2", "Surname:", ""),
                             
                           ), # sidebarPanel
                           mainPanel(
                             h1("Header 1"),
                             
                             h4("Output 1"),
                             verbatimTextOutput("txtout"),
                             
                           ) # mainPanel
                           
                  ), # Navbar 1, tabPanel
                  tabPanel("About", 
                           mainPanel(
                             h3("Purpose of App"),
                             htmlOutput("text1"),
                             h3("Datasets"),
                             htmlOutput("text2"),
                             h3("Link to Data Files on Kaggle"),
                             htmlOutput("htmlLink"),
                             h3("Purpose of each tab (page)"),
                             htmlOutput("txt2"),
                             tags$img(src="NIH image.png")
                           )
                           
                           
                           ),
                  
                  tabPanel("Navbar 3", "This panel is intentionally left blank")
                  
                ) # navbarPage
) # fluidPage


# Define server function  
server <- function(input, output) {
  
  output$txtout <- renderText({
    paste( input$txt1, input$txt2, sep = " " )
  })
  
  output$htmlLink <- renderUI({
    a("Diabetes Dataset", href = "https://www.kaggle.com/datasets/mathchi/diabetes-data-set") 
  })
  
  output$text1 <- renderText({
    p1 <- ("The purpose of the app is to ....")
  })
  
  output$text2 <- renderText({
    p1 <- ("This dataset is originally from the National Institute of Diabetes and Digestive and Kidney Diseases. The objective is to predict based on diagnostic measurements whether a patient has diabetes. In particular, all patients here are females at least 21 years old of Pima Indian heritage. The variables are as follows;")
    HTML(paste(p1, sep = '<br/> <br/>'), "<br/> <br/>",
         "<ul>
        <li>Pregnancies: Number of times pregnant</li>
        <li>Glucose: Plasma glucose concentration a 2 hours in an oral glucose tolerance test>
        <li>BloodPressure: Diastolic blood pressure (mm Hg)</li>
        <li>SkinThickness: Triceps skin fold thickness (mm)</li>
        <li>Insulin: 2-Hour serum insulin (mu U/ml)</li>
        <li>BMI: Body mass index (weight in kg/(height in m)^2)</li>
        <li>DiabetesPedigreeFunction: Diabetes pedigree function</li>
        <li>Age: Age (years)</li>
        <li>Outcome: Class variable (0 or 1)</li>
        </ul>")
  })
  
  output$txt2 <- renderText({
    p1 <- ("There are four main tabs as follows.")
    
    HTML(paste(p1, sep = '<br/> <br/>'), "<br/> <br/>",
         "<ul>
        <li>An About page which describe the purpose of the app, briefly discuss the data and its source and Include a picture related to the data.</li>
        
        <li>A Data Exploration page which Creates numerical and graphical summaries, Change the type of plot shown and type of summary reported, Change the variables and filter the rows to change the data used in the plots/summaries and Change the variables and filter the rows to change the data used in the plots/summaries.
</li>
        
        <li>A Modeling page that fits three supervised learning models</li>
        
        <li>A Data page that allows for Scrolling through the data set, subsetting this data set (rows and columns) and Saving the (possibly subsetted) data as a file
</li>
         
        </ul>")
    
    
    
    
#            
# ∗ Describe the purpose of the app
# 
# 
# ∗ Briefly discuss the data and its source - providing a link to more information about the data
# ∗ Tell the user the purpose of each tab (page) of the app
# ∗ Include a picture related to the data")
#     HTML(paste(p1, sep = '<br/> <br/>'), "<br/> <br/>",
#          "<ul>
#         <li>Poverty Rate</li>
#         <li>High School Graduation Rate</li>
#         <li>High School Graduation Rate</li>
#         <li>Median Household Income</li>
#         <li>Racial Demographics</li>
#         <li>Police Killings in the US</li>
#         </ul>"
  })
  
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)