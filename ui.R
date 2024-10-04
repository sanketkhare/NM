library(shiny)
library(shinydashboard)
library(tibble)
library(reshape2)
library(ggplot2)
library(data.table)
library(DT)
library(dplyr)
library(tibble)
library(plotly)
library(vctrs)
library(scales)
library(shinyWidgets)
library(shinythemes)
library(shinyjs)
library(shinyalert)
library(MASS)
library(knitr)
library(shinythemes)
library(V8)
library(kableExtra)
library(openxlsx)
library(tidyr)
library(shinycssloaders)
library(plotly)
library(stringdist)
library(tm)
library(tidytext)
library(stringr)
library(readr)
library(plumber)

jsResetCode <- "shinyjs.reset = function() {history.go(0)}"

ui<-fluidPage(
  useShinyjs(),
  extendShinyjs(text = jsResetCode, functions = c("winprint")),
  tags$head(
    tags$style(HTML("
      .nav-tabs > li > a {
        background-color: #FFFFFF; 
        color: #000000; 
      }
      .logo {
            background-color: #000033 !important;
            }
      .navbar {
              background-color: #000033 !important;
              }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        background-color: #000033;
        color: #FFFFFF;
      }
      .dashboard-title {
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
        max-width: 100%;
      }
      .btn{
          background-color: #000033;
          color: white;
        }
    "))
  ),
  dashboardPage(
    dashboardHeader(title="Name Match",titleWidth = "100%"),
    dashboardSidebar(collapsed = F,
                     sidebarMenu(id="tabs",
                                 menuItem("Tab 1",tabName = "tab1")
                     )
                     ),
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "tab1",
          fluidRow(
            box(id="box1",title = "Input Policy No",width = 4,solidHeader = T,
                status = "info",textInput("polno","",value = " ")
             ),
            box(id="box2",title = "Input Insured Name",width = 4,solidHeader = T,
                status = "info",textInput("insname","",value = " ")
            ),
            box(id="box3",title = "Input KYC Name",width = 4,solidHeader = T,
                status = "info",textInput("kycname","",value = " ")
            )
            
            
          ),
          fluidRow(
            box(id="box4",title = "Data",width = 6,solidHeader = T,
                status = "info",tableOutput("trialdata")
            )
            
            
          )
          
        )
      ))
  )
)
