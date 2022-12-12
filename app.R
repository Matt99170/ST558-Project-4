
#Matthew Sookoo
#ST558 Final Project 4

library(shiny)
library(shinythemes)
library(tidyverse)
library(caret)
library(randomForest)


# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                withMathJax(),
                navbarPage(
                  
                  "Diabetes App",
                  
                  
#About tab
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
                           )),

#Data Exploration tab
                  
                  tabPanel("Data Exploration", 
                           
                           sidebarLayout(
                             sidebarPanel(
                               selectizeInput("varX", 
                                              h5("Select x-axis variable"), 
                                              selected = "BMI", 
                                              choices = c("Pregnancies", 
                                                          "Glucose", 
                                                          "BloodPressure",
                                                          "SkinThickness",
                                                          "Insulin",
                                                          "BMI",
                                                    "DiabetesPedigreeFunction"
                               )),
                               
                      selectizeInput("varY", 
                                     h5("Select y-axis variable"), 
                                    selected = "BloodPressure", 
                                    choices = c("Pregnancies", 
                                                "Glucose", 
                                                "BloodPressure",
                                                "SkinThickness",
                                                "Insulin",
                                                "BMI",
                                                "DiabetesPedigreeFunction"
                                              )),
                              ),
                             
                             mainPanel(
                               plotOutput("corPlot1"),
                               br(),
                               h5(textOutput("text31")),
                              
                              
                               h4(tableOutput("text3"), align = "center"),
                               br(),
                               h5(textOutput("text32")),
                               
                               h2(verbatimTextOutput("textt3"), align = "center")
                             ),
                           )),



#Modelling tab
                  tabPanel("Modeling", 
                  
                           
                  #Modelling information subtab
                           tabsetPanel(
                             tabPanel("Model Information", " You should explain these three modeling approaches, the benefits of each,
and the drawbacks of each. You should include some type of math type in the explanation
(you’ll need to include mathJax)."),
                             
                             
    # Model fitting subtab                         
    tabPanel("Model Fitting"
             ,h3("Percentage of Training and Test split"),
             br(),
             sidebarPanel(
             sliderInput("bins1",
                         "Select percentage of training set:",
                                                    min = 0,
                                                    max = 1,
                                                    value = 0.7)
                                      ),
                                      
                                      
                                      h3("Predictors for models"),
                                      
                                      
                                    sidebarLayout(
                                    sidebarPanel(
                                    selectizeInput("pred1", 
                                    h5("Select first predictor variable"), 
                                    selected = "BMI", 
                                    choices = c("Pregnancies", 
                                                "Glucose", 
                                                "BloodPressure",
                                                "SkinThickness",
                                                "Insulin",
                                                "BMI",
                                                "DiabetesPedigreeFunction"
                                                         )),
                                          
                                    selectizeInput("pred2", 
                                    h5("Select second predictor variable"), 
                                    selected = "BloodPressure", 
                                    choices = c("Pregnancies", 
                                                "Glucose", 
                                                "BloodPressure",
                                                "SkinThickness",
                                                "Insulin",
                                                "BMI",
                                                "DiabetesPedigreeFunction"
                                                         )),
                                    
                                    selectizeInput("pred3", 
                                    h5("Select third predictor variable"), 
                                    selected = "Insulin", 
                                    choices = c("Pregnancies", 
                                                "Glucose", 
                                                "BloodPressure",
                                                "SkinThickness",
                                                "Insulin",
                                                "BMI",
                                                "DiabetesPedigreeFunction"
                                                   )),
                                     actionButton("submitbutton", "Get Model Fit Stats", class= "btn btn-primary")     
                                          
                                        ),
                                        
                                        mainPanel(
                                          textOutput("logit"),
                                          br(),
                                          verbatimTextOutput("logit2"),
                                          br(),
                                          textOutput("classtree"),
                                          br(),
                                          verbatimTextOutput("classtree2"),
                                          br(),
                        
                                          textOutput("randomforest"),
                                          br(),
                                          verbatimTextOutput("randomforest2"),
                                          br(),
                                          plotOutput("varImpPlot")
                                        ),
                                      )),
                  
    
                      #Logistic Regression Predictor subtab
                             tabPanel("Logistic Regression Predictor"
                                      
                                      ,h3("Values For Prediction"),
                                    br(),
                                      sidebarPanel(
                                        sliderInput("bins11",
                                                    "Select value of BMI:",
                                                    min = 0,
                                                    max = 100,
                                                    value = 40)
                                      ), 
                                    
                                    sidebarPanel(
                                      sliderInput("bins12",
                                                  "Select value of BloodPressure:",
                                                  min = 0,
                                                  max = 200,
                                                  value = 90)
                                    ),
                                    
                                    sidebarPanel(
                                      sliderInput("bins13",
                                                  "Select value of Glucose:",
                                                  min = 0,
                                                  max = 300,
                                                  value = 150)
                                    ),
                                    mainPanel(
                                      textOutput("logitpred"),
                                      br(),
                                      tableOutput("logitpred2"),
                                      
                                      
                                    )))),
    
                  
#Data tab
                  tabPanel("Data", 
                           h3("Dataset"),
                           
                           selectizeInput("dataset", 
                                          h5("Select the variable to subset the data and download"), 
                                          selected = "BMI", 
                                          choices = c("Pregnancies", 
                                                      "Glucose", 
                                                      "BloodPressure",
                                                      "SkinThickness",
                                                      "Insulin",
                                                      "BMI",
                                                      "DiabetesPedigreeFunction"
                                          )),
                           # Button
                           downloadButton("downloadData", "Download"),
                          
                           mainPanel(tableOutput("entiretable"),
                                     
                                     tableOutput("subsettable"))
                           
                           )
                  
                ) # navbarPage
) # fluidPage


# Define server function  
server <- function(input, output, session) {
  
  #outputs
  
  output$txtout <- renderText({
    paste( input$txt1, input$txt2, sep = " " )
  })
  
  output$htmlLink <- renderUI({
    a("Diabetes Dataset", href = "https://www.kaggle.com/datasets/mathchi/diabetes-data-set") 
  })
  
  output$text1 <- renderText({
    p1 <- ("The purpose of the app is to is to create a nice looking shiny app that can be used to explore data and model it.")
  })
  
  output$text2 <- renderText({
    p1 <- ("This dataset is originally from the National Institute of Diabetes and Digestive and Kidney Diseases. The objective is to predict based on diagnostic measurements whether a patient has diabetes. In particular, all patients here are females at least 21 years old of Pima Indian heritage. The variables are as follows;")
    HTML(paste(p1, sep = '<br/> <br/>'), "<br/> <br/>",
         "<ul>
        <li>Pregnancies: Number of times pregnant</li>
        <li>Glucose: Plasma glucose concentration a 2 hours in an oral glucose tolerance test
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
    

  })
  
  
  
  output$textt1 <- renderText({
    "There are four main tabs as follows."
    
    
  })
  
  
  diabetes_original <- read_csv(file = "./diabetes.csv")
  
  output$corPlot1 <- renderPlot({
    
     
    
    PlotGraph1<- diabetes_original%>% select(xvar=input$varX, yvar=input$varY, Age)
  
    
    ggplot(data = PlotGraph1, aes(x = xvar, y = yvar)) +
        geom_point(aes(color = Age)) +
        geom_smooth(method = "lm") +
        ggtitle("Trend showing correlation between the x-axis and the y-axis variables selected")+
        labs(x = "x-axis variable selected" , y = "y-axis variable selected")

  
  })
  
  
  output$text31 <- renderText({
   "Table show mean and standard deviation for the x-axis variable selected"
    
  })
  
  
  
  output$text3 <- renderTable({
    PlotGraph1<- diabetes_original%>% select(xvar=input$varX, yvar=input$varY, Age)
    data.frame(mean=mean(PlotGraph1$xvar), std_dev=sd(PlotGraph1$xvar))
    
  })
  
  output$text32 <- renderText({
    "Summary of y-axis variable selected"
    
  })  
  
  output$textt3 <- renderPrint({
    PlotGraph1<- diabetes_original%>% select(xvar=input$varX, yvar=input$varY, Age)
    
    summary(PlotGraph1$yvar)
    
  })
  
  ########################################################################
  diabetes_original <- read_csv(file = "./diabetes.csv")
  

  
  
  diabetes_original <- diabetes_original %>% mutate(Outcome = as.factor(Outcome))
  
  
  mydata <- reactiveValues()
  
  observeEvent(input$submitbutton,{
 
  output$logit <- renderText({ "Logistic Regression Model"
   
  })

  
  
  output$logit2 <- renderPrint({
    
    #########################
    set.seed(123)
    #splitting into train and test set
    #indices to split on
    diabetesIndex <- createDataPartition(diabetes_original$Outcome, p = input$bins1, list = FALSE)
    #subset
    trainingSet <- diabetes_original[diabetesIndex, ]
    testSet  <- diabetes_original[-diabetesIndex, ]
    
    
    ########################
    
    
    log1<- trainingSet%>% select(xvar1=input$pred1, yvar1=input$pred2, zvar1=input$pred3, Outcome)
    
    log2<- testSet%>% select(xvar1=input$pred1, yvar1=input$pred2, zvar1=input$pred3, Outcome)
    
    #logistic regression
    l_poly_1 <- train(Outcome ~ xvar1 + yvar1 + zvar1,
                      data = log1,
                      trControl = trainControl(method = "cv", number = 10),
                      preProcess = c("center", "scale"),
                      method = "glmnet")
    
    
    l_poly_1Prediction <- predict(l_poly_1, newdata = log2)
    m1 <- postResample(l_poly_1Prediction, log2$Outcome)
    m1
    
  })
  
  #################################################classification tree
  
  output$classtree <- renderText({ "Classification Tree"
    
  })
  
  
  
  output$classtree2 <- renderPrint({
    
    ######################
    set.seed(123)
    #splitting into train and test set
    #indices to split on
    diabetesIndex <- createDataPartition(diabetes_original$Outcome, p = input$bins1, list = FALSE)
    #subset
    trainingSet <- diabetes_original[diabetesIndex, ]
    testSet  <- diabetes_original[-diabetesIndex, ]
    #####################
    
    log11<- trainingSet%>% select(xvar11=input$pred1, yvar11=input$pred2, zvar11=input$pred3, Outcome)
    
    log22<- testSet%>% select(xvar11=input$pred1, yvar11=input$pred2, zvar11=input$pred3, Outcome)
    
    
    class_tree = train(Outcome ~ xvar11 + yvar11 + zvar11, 
                       data = log11, 
                       method="rpart", 
                       trControl = trainControl(method = "cv"))
    
    test_pred_tree <- predict(class_tree, newdata = log22)
    m2 <- postResample(test_pred_tree, log22$Outcome)
    #calling m3 object
    m2
  })
  
  
  #########################################################################
  #random forest model
  
  output$randomforest <- renderText({ "Random Forest Model"
    
  })
  
  
  
  output$randomforest2 <- renderPrint({
    
    ############################
    set.seed(123)
    #splitting into train and test set
    #indices to split on
    diabetesIndex <- createDataPartition(diabetes_original$Outcome, p = input$bins1, list = FALSE)
    #subset
    trainingSet <- diabetes_original[diabetesIndex, ]
    testSet  <- diabetes_original[-diabetesIndex, ]
    ###########################
    
    log111<- trainingSet%>% select(xvar111=input$pred1, yvar111=input$pred2, zvar111=input$pred3, Outcome)
    
    log222<- testSet%>% select(xvar111=input$pred1, yvar111=input$pred2, zvar111=input$pred3, Outcome)
    
    
    r_f <- train(Outcome ~ xvar111 + yvar111 + zvar111, 
                 data = log111,                  
                 method = "rf",
                 trControl=trainControl(method = "cv", number = 5),
                 preProcess = c("center", "scale"),
                 tuneGrid = data.frame(mtry = 1:3))
    #calling r_f object
    r_f
    
    test_pred_r_f <- predict(r_f, newdata = log222)
    m3 <- postResample(test_pred_r_f, log222$Outcome)
    #calling m3 object
    m3
  })
  #################################### varplotoutput for rf
  
  output$varImpPlot <- renderPlot({
    
    ################################
    set.seed(123)
    #splitting into train and test set
    #indices to split on
    diabetesIndex <- createDataPartition(diabetes_original$Outcome, p = input$bins1, list = FALSE)
    #subset
    trainingSet <- diabetes_original[diabetesIndex, ]
    testSet  <- diabetes_original[-diabetesIndex, ]
    ###############################
    
    r_f2 <- randomForest(trainingSet, data=trainingSet, ntree=5
                         , keep.forest=FALSE,
                         importance=TRUE)
    
    varImpPlot(r_f2, sort=FALSE, main="Variable Importance Plot From Random Forest")
  })
  
  })
  
  
  ###################################################
  output$logitpred <- renderText({"A value of '1' indicates that person may have diabetes and similarly a value of '0' indiccates that the person may not have diabetes"
    
  })
  
  output$logitpred2 <- renderTable({
    
    
    
    
    set.seed(123)
    
    diabetes_original <- diabetes_original %>% mutate(Outcome = as.factor(Outcome))
    
    #indices to split on
    diabetesIndex <- createDataPartition(diabetes_original$Outcome, p = 0.70, list = FALSE)
    #subset
    trainingSet <- diabetes_original[diabetesIndex, ]
    testSet  <- diabetes_original[-diabetesIndex, ]
    
    ###################### Logistic regression for predictor
    
    l_poly_1 <- train(Outcome ~ BMI + BloodPressure + Glucose,
                      data = trainingSet,
                      trControl = trainControl(method = "cv", number = 10),
                      preProcess = c("center", "scale"),
                      method = "glmnet")
    
    ################
    l_poly_1Prediction1 <- predict(
      l_poly_1, newdata = data.frame(BMI=input$bins11, BloodPressure=input$bins12, Glucose=input$bins13))
    
    l_poly_1Prediction1
    #####
    
  })
  
  ################## data tab

  output$entiretable <- renderTable({
    #get data
     diabetes_original 
  })
  
  ##################################################
  # Reactive value for selected dataset ----
  
  
  
  datasetInput <- reactive({
   
   
    
    switch(input$dataset,
           "Pregnancies" = Pregnancies,
           "Glucose"= Glucose,
           "BloodPressure" = BloodPressure,
           "SkinThickness" = SkinThickness,
           "Insulin" = Insulin,
           "BMI" = BMI)
  })
  
  
  
  # Table of selected dataset ----
  output$subsettable <- renderTable({
     datasetInput()
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
  
  ########################################################################
} 


# Create Shiny object
shinyApp(ui = ui, server = server)