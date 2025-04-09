library(shiny)
library(activityinfo)
library(bslib)
library(gridlayout)
library(DT)
library(purrr)


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
