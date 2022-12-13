# ST558-Project-4

The goal of this App is to create a nice looking shiny app that can be used to explore data and model it.

This dataset is originally from the National Institute of Diabetes and Digestive and Kidney Diseases. The objective is to predict based on diagnostic measurements whether a patient has diabetes. In particular, all patients here are females at least 21 years old of Pima Indian heritage. The variables are as follows;

* Pregnancies: Number of times pregnant
* Glucose: Plasma glucose concentration a 2 hours in an oral glucose tolerance test
* BloodPressure: Diastolic blood pressure (mm Hg)
* SkinThickness: Triceps skin fold thickness (mm)
* Insulin: 2-Hour serum insulin (mu U/ml)
* BMI: Body mass index (weight in kg/(height in m)^2)
* DiabetesPedigreeFunction: Diabetes pedigree function
* Age: Age (years)
* Outcome: Class variable (0 or 1)

There are four main tabs as follows

* An About page which describe the purpose of the app, briefly discuss the data and its source and Include a picture related to the data.
* A Data Exploration page which Creates numerical and graphical summaries, Change the type of plot shown and type of summary reported, Change the variables and filter the rows to change the data used in the plots/summaries and Change the variables and filter the rows to change the data used in the plots/summaries.
* A Modeling page that fits three supervised learning models.
* Data page that allows for Scrolling through the data set, subsetting this data set (rows and columns) and Saving the (possibly subsetted) data as a file
  
 
## Required packages

* shiny
* shinythemes
* tidyverse
* caret
* randomForest

The following code can be used to install the above packages:

install.packages("shiny", "shinythemes", "tidyverse", "caret", "randomForest")

## Code to run the App

library(shiny)

library(shinythemes)

library(tidyverse)

library(caret)

library(randomForest)

runGitHub(repo = "ST558-Project-4", username = "matt99170", ref = "main")






