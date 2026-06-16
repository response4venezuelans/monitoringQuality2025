library(shiny)
library(activityinfo)
library(bslib)
library(gridlayout)
library(DT)
source("functions.R")

#Define Custom Theme
# Define custom theme using the provided color palette
R4Vtheme <- bs_theme(
  bg = "#f0f2f5",  # Light gray background
  fg = "#132A3E",  # Dark blue foreground
  primary = "#00AAAD",  # Teal primary color
  secondary = "#902857",  # Dark pink secondary color
  success = "#72BF44",  # Green success color
  info = "#129ABF",  # Light blue info color
  warning = "#FEBE10",  # Yellow warning color
  danger = "#e83f54",  # Red danger color
  base_font = font_google("Inter"),
  code_font = font_google("JetBrains Mono")
)

activityInfoToken(Sys.getenv("ACTIVITYINFOTOKEN"), prompt = FALSE)
# Get from Activity Info data Lists
# Monitoring framework
# Admin list
# Organization list
# Country list

countryListDF <- queryTable("cnkrge1m07falxuu7o",
                 "Country" = "c8u26b8kxeqpy0k4",
                 "Admin1" = "c3ns3zikxeqq4h95",
                 "Admin1ISOCode" = "cl3sspjkxeqq8yq6",
                 "countryISO" = "c1u8kphm4vqtemz2")

countryList <-  unique(countryListDF$Country)
# Include All to get all data
countryList <- c(countryList, "All")



df_partnersDF <- queryTable("cukzlnfm66k09v73",
                          "IPID" = "cladozxm66k0duc4",
                          "AOIDORG" = "c7any2um66k0duf5",
                          "Name" = "crsqq8vm66k0dug6",
                          "Nombre" = "cycv8tum66k0dug7",
                          "Nome" = "cwq2vmdm66k0duh8",
                          "Acronym/Short Name" = "c5jh080m66k0duh9",
                          "Type" = "cjdhnb0m66k0duha",
                          "Regional" = "c91cd1dm66k0dujm",
                          "Platform" = "cnn13w6m66k0dukp",
                          "Refugees and Migrants lead" = "cw7tb4hm66k0dulz",
                          "Link EN" = "cl2tilvm66k0dul12",
                          "Link ES" = "cakk6mm66k0dum13")
partnerList<- unique(df_partnersDF$Name)
partnerList <- c(partnerList, "All")

indicatorDF <- queryTable("cwk3g8tm07falxuu7j",
                 "CODE" = "cdhugiblctco28h3",
                 "Sector" = "cagw22hlctcp2vu5",
                 "Sector Objective" = "cyyqv3alctcperj6",
                 "Indicator" = "c1oo0eclctcqtjp8",
                 "Rationale" = "cpaq2z8m07feolz3",
                 "Indicator Type" = "cuskmf7lctcszoga",
                 "Definitions" = "cmfe24fm07ff9y24",
                 "Sector SP" = "cviy6p0lctcud9xc",
                 "Objetivo" = "c4ut4hjlctcunimd",
                 "Indicador" = "c9ck1g2lctcuxg4e",
                 "Descripcion" = "chilz3cm07fgc9g5",
                 "Tipo de indicador" = "cw1sv7hlctd1pk5g",
                 "Definiciones" = "cyznpw1m07fgv8l6")

activities_icon <-
  HTML(
    '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-bar-chart-line-fill" viewBox="0 0 16 16">
  <path d="M11 2a1 1 0 0 1 1-1h2a1 1 0 0 1 1 1v12h.5a.5.5 0 0 1 0 1H.5a.5.5 0 0 1 0-1H1v-3a1 1 0 0 1 1-1h2a1 1 0 0 1 1 1v3h1V7a1 1 0 0 1 1-1h2a1 1 0 0 1 1 1v7h1z"/>
</svg>'
  )
error_icon <-
  HTML(
    '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-bandaid" viewBox="0 0 16 16">
  <path d="M14.121 1.879a3 3 0 0 0-4.242 0L8.733 3.026l4.261 4.26 1.127-1.165a3 3 0 0 0 0-4.242M12.293 8 8.027 3.734 3.738 8.031 8 12.293zm-5.006 4.994L3.03 8.737 1.879 9.88a3 3 0 0 0 4.241 4.24l.006-.006 1.16-1.121ZM2.679 7.676l6.492-6.504a4 4 0 0 1 5.66 5.653l-1.477 1.529-5.006 5.006-1.523 1.472a4 4 0 0 1-5.653-5.66l.001-.002 1.505-1.492z"/>
  <path d="M5.56 7.646a.5.5 0 1 1-.706.708.5.5 0 0 1 .707-.708Zm1.415-1.414a.5.5 0 1 1-.707.707.5.5 0 0 1 .707-.707M8.39 4.818a.5.5 0 1 1-.708.707.5.5 0 0 1 .707-.707Zm0 5.657a.5.5 0 1 1-.708.707.5.5 0 0 1 .707-.707ZM9.803 9.06a.5.5 0 1 1-.707.708.5.5 0 0 1 .707-.707Zm1.414-1.414a.5.5 0 1 1-.706.708.5.5 0 0 1 .707-.708ZM6.975 9.06a.5.5 0 1 1-.707.708.5.5 0 0 1 .707-.707ZM8.39 7.646a.5.5 0 1 1-.708.708.5.5 0 0 1 .707-.708Zm1.413-1.414a.5.5 0 1 1-.707.707.5.5 0 0 1 .707-.707"/>
</svg>'
  )
percent_icon <-
  HTML(
    '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-percent" viewBox="0 0 16 16">
  <path d="M13.442 2.558a.625.625 0 0 1 0 .884l-10 10a.625.625 0 1 1-.884-.884l10-10a.625.625 0 0 1 .884 0M4.5 6a1.5 1.5 0 1 1 0-3 1.5 1.5 0 0 1 0 3m0 1a2.5 2.5 0 1 0 0-5 2.5 2.5 0 0 0 0 5m7 6a1.5 1.5 0 1 1 0-3 1.5 1.5 0 0 1 0 3m0 1a2.5 2.5 0 1 0 0-5 2.5 2.5 0 0 0 0 5"/>
</svg>'
  )
