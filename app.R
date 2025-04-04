#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(activityinfo)
library(bslib)
library(gridlayout)
library(DT)
#source("functions.R")
credentials <- Sys.getenv("ACTIVITYINFOTOKEN")
print(credentials)
activityInfoToken(credentials, prompt = FALSE)
shinyApp(ui = ui, server = server)
