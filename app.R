# PBPK-QSTS shiny v0.8
# 
# 
# Copyright (C) 2018 
# 
# Authors: Zofia Tylutki, Jakub Szlęk
# 
# 
# Affiliation: 
# Jagiellonian University Medical College,
# Faculty of Pharmacy,
# Medyczna 9 st.,
# 30-688 Kraków
# Poland
# 
# Bugs, issues, please e-mail to maintainer
# Jakub Szlęk: j.szlek@uj.edu.pl
#
# Copyright (C) 2018 
# 
# This program is free software: you can redistribute it and/or modify it under the terms of the GNU General 
# Public License as published by the Free Software Foundation, either version 3 of the License,
# or (at your option) any later version.
# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
# without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with this program.
# If not, see <http://www.gnu.org/licenses/>.
# 
#


# Package requirements

require(shiny)
require(shinyjs)
require(DT)
require(shinyBS)
require(dplyr)
require(plyr)
require(ggplot2)
require(ggthemes)
require(distr)
require(data.table)
require(deSolve)


# Source other R scripts

source("log_normal_transf.R")
source("stat_func.R")

# Turn off warnings
options(warn=-1)

# Create dummy obj
data_vals <- NULL

#' busyIndicator START
#' 
#' busyIndicator from https://github.com/AnalytixWare/ShinySky/blob/master/R/busy-indicator.r by  xiaodaigh
#' 
#' A busy indicator
#' 
#' @param text The text to show
#' @param img An anitmated gif
#' @param wait The amount of time to wait before showing the busy indicator. The
#'   default is 1000 which is 1 second.
#'   
#' @export

busyIndicator <- function(text = "Calculation in progress..",img = "ajax-loader.gif", wait=1000) {
  tagList(
    singleton(tags$head(
      tags$link(rel="stylesheet", type="text/css",href="busyIndicator.css")
    ))
    ,div(class="busy-indicator",tags$p(text),tags$img(src=img))
    ,tags$script(sprintf(
      "	setInterval(function(){
      if ($('html').hasClass('shiny-busy')) {
      setTimeout(function() {
      if ($('html').hasClass('shiny-busy')) {
      $('div.busy-indicator').show()
      }
      }, %d)  		    
      } else {
      $('div.busy-indicator').hide()
      }
},100)
      ",wait)
    )
  )	
}

#'
#'
#' busyIndicator END
#' 



# Define UI for application
ui <- navbarPage(


  
# Application title
  "PBPK-QSTS v0.8",
  
  tabPanel("Welcome",
           shinyjs::useShinyjs(),
           mainPanel(
             tags$br(),
             includeHTML("Help.html"),
             busyIndicator()
           )
  ),
  
  navbarMenu("Population",
             tabPanel("Population settings", fluid=TRUE,
                      tags$h4("Population specific parameters"),
                      tags$hr(),
                      fluidPage(
                        fluidRow(
                          column(4,
                                 tags$h4(
                                   tags$b(
                                     "General parameters:"
                                   )
                                 ),
                                 tags$hr(),
                                 actionButton("reset_population_defaults_general","Default settings"), # Reset population general settings  button
                                 tags$hr(),
                                 numericInput(inputId = "seed",
                                              label = "Set seed:",
                                              value = 1),
                                 # Alerts
                                 bsAlert("seed_alert"),
                                 
                                 numericInput(inputId = "individual_count",
                                              label = "Number of individuals:",
                                              value = 15,
                                              min = 1,
                                              step = 1),
                                 # Alerts
                                 bsAlert("individual_count_alert"),
                                 
                                 sliderInput(inputId = "female_count",
                                             label = "Percentage of females [%]:",
                                             value = 0,
                                             min = 0,
                                             max = 100),
                                 sliderInput(inputId = "age_range",
                                             label = "Age range:",
                                             min = 18,
                                             max = 70,
                                             value = c(18, 70)),
                                 
                                 numericInput(inputId = "scale_M",
                                              label = "Scale parameter for males' age distribution", # [Simcyp Simulator v.16]
                                              value = 24.7,
                                              min = .Machine$double.xmin),
                                 # Alerts
                                 bsAlert("scale_M_alert"),
                                 
                                 numericInput(inputId = "shape_M",
                                              label = "Shape parameter for males' age distribution", # [Simcyp Simulator v.16]
                                              value = 2.1,
                                              min = .Machine$double.xmin),
                                 # Alerts
                                 bsAlert("shape_M_alert"),
                                 
                                 numericInput(inputId = "scale_F",
                                              label = "Scale parameter for females' age distribution", # [Simcyp Simulator v.16]
                                              value = 24.7,
                                              min = .Machine$double.xmin),
                                 # Alerts
                                 bsAlert("scale_F_alert"),
                                 
                                 numericInput(inputId = "shape_F",
                                              label = "Shape parameter for females' age distribution", # [Simcyp Simulator v.16]
                                              value = 1.9,
                                              min = .Machine$double.xmin),
                                 # Alerts
                                 bsAlert("shape_F_alert"),
                                 
                                 numericInput(inputId = "liver_density",
                                              label = "Liver density [g/L]",
                                              value = 1080),
                                 # bsAlert
                                 bsAlert("liver_density_alert"),
                                 
                                 numericInput(inputId = "heart_density",
                                              label = "Heart density [g/L]",
                                              value = 1050),
                                 # bsAlert
                                 bsAlert("heart_density_alert"),
                                 
                                 numericInput(inputId = "Qpf",
                                              label = "Pericaridum blood flow", 
                                              value = 0.011),
                                 # bsAlert
                                 bsAlert("Qpf_alert"),
                                 
                                 
                                 #Tooltips 
                                 bsTooltip("seed", "Random Number Generation (seed). A single value, interpreted as an integer.", placement = "bottom", trigger = "hover",
                                           options = NULL),
                                 bsTooltip("individual_count", "Number of individuals used in the study during simulation process.", placement = "bottom", trigger = "hover",
                                           options = NULL),
                                 bsTooltip("female_count", "Percentage [%] of females participants.", placement = "bottom", trigger = "hover",
                                           options = NULL),
                                 bsTooltip("age_range", "Age of participants between 18 and 70 years.", placement = "bottom", trigger = "hover",
                                           options = NULL),
                                 bsTooltip("reset_population_defaults_general", "Reset all settings of \\'General parameters\\' to defaults.", placement = "bottom", trigger = "hover",
                                           options = NULL),
                                 bsTooltip("scale_M", "Scale parameter for males\\' age distribution (Weibull).", placement = "bottom", trigger = "hover",
                                           options = NULL),
                                 bsTooltip("shape_M", "Shape parameter for males\\' age distribution (Weibull).", placement = "bottom", trigger = "hover",
                                           options = NULL),
                                 bsTooltip("scale_F", "Scale parameter for females\\' age distribution (Weibull).", placement = "bottom", trigger = "hover",
                                           options = NULL),
                                 bsTooltip("shape_F", "Shape parameter for females\\' age distribution (Weibull).", placement = "bottom", trigger = "hover",
                                           options = NULL)
                                 

                          ),
                          column(4,
                                 tags$h4(
                                   tags$b(
                                     "Absorption parameters:"
                                   )
                                 ),
                                 tags$hr(),
                                 actionButton("reset_population_defaults_pharmacokinetics_absorption", "Default settings"), # Reset population pharmacokinetics settings button
                                 tags$hr(),
                                 
                                 numericInput(inputId = "ka",
                                              label = "ka", # [Simcyp Simulator v.16]
                                              value = 0.2434986),
                                 # Alerts
                                 bsAlert("ka_alert"),
                                 
                                 numericInput(inputId = "tlag_m_lognorm_m",
                                              label = "Lag time m_lognormal 1st param (m)", # [Simcyp Simulator v.16]
                                              value = 1.3321865),
                                 # Alerts
                                 bsAlert("tlag_m_lognorm_m_alert"),
                                 
                                 numericInput(inputId = "tlag_m_lognorm_cv",
                                              label = "Lag time m_lognormal 2nd param (cv)", # [Simcyp Simulator v.16]
                                              value = 0.3),
                                 # Alerts
                                 bsAlert("tlag_m_lognorm_cv_alert"),
                                 
                                 numericInput(inputId = "F_mean",
                                              label = "Oral bioavailability [%]", # [Simcyp Simulator v.16]
                                              value = 45.9),
                                 # Alerts
                                 bsAlert("F_mean_alert"),
                                 
                                 numericInput(inputId = "F_sd",
                                              label = "Oral bioavailability standard deviation", # [Simcyp Simulator v.16]
                                              value = 9.3),
                                 # Alerts
                                 bsAlert("F_sd_alert"),
                                 
                                 numericInput(inputId = "F_lower",
                                              label = "F lower oral bioavailability of API [%]", # [Simcyp Simulator v.16]
                                              value = 33),
                                 # Alerts
                                 bsAlert("F_lower_alert"),
                                 
                                 numericInput(inputId = "F_upper",
                                              label = "F upper oral bioavailability of API [%]", # [Simcyp Simulator v.16]
                                              value = 62),
                                 # Alerts
                                 bsAlert("F_upper_alert"),
                                 
                                 numericInput(inputId = "FaFg_m_lognorm_m",
                                              label = "FaFg m_lognormal 1st param (m) [h]", # [Simcyp Simulator v.16]
                                              value = 0.832),
                                 # Alerts
                                 bsAlert("FaFg_m_lognorm_m_alert"),
                                 
                                 numericInput(inputId = "FaFg_m_lognorm_cv",
                                              label = "FaFg m_lognormal 2nd param (cv) [h]", # [Simcyp Simulator v.16]
                                              value = 0.131),
                                 # Alerts
                                 bsAlert("FaFg_m_lognorm_cv_alert"),
                                 
                                 # Tooltips
                                 bsTooltip("reset_population_defaults_pharmacokinetics_absorption", "Reset all settings of \\'Absorption parameters\\' to defaults.",
                                           placement = "bottom", trigger = "hover",
                                           options = NULL),
                                 bsTooltip("ka", "first order absorption rate [1/h]",
                                           placement = "bottom", trigger = "hover",
                                           options = NULL),
                                 bsTooltip("tlag_m_lognorm_m", "mean lag time of drug absorption [h]",
                                           placement = "bottom", trigger = "hover",
                                           options = NULL),
                                 bsTooltip("tlag_m_lognorm_cv", "coefficient of variation of lag time of drug absorption",
                                           placement = "bottom", trigger = "hover",
                                           options = NULL),
                                 bsTooltip("F_mean", "mean oral bioavailability of API [%]",
                                           placement = "bottom", trigger = "hover",
                                           options = NULL),
                                 bsTooltip("F_sd", "standard deviation of API\\'s oral bioavailability [%]",
                                           placement = "bottom", trigger = "hover",
                                           options = NULL),
                                 bsTooltip("F_lower", "lower bound of the range of API\\'s oral bioavailability if known [%]",
                                           placement = "bottom", trigger = "hover",
                                           options = NULL),
                                 bsTooltip("F_upper", "upper bound of the range of API\\'s oral bioavailability if known [%]",
                                           placement = "bottom", trigger = "hover",
                                           options = NULL),
                                 bsTooltip("FaFg_m_lognorm_m", "mean fraction of administered dose of API absorbed to enterocytes, escaping gut wall metabolism and entering portal vein",
                                           placement = "bottom", trigger = "hover",
                                           options = NULL),
                                 bsTooltip("FaFg_m_lognorm_cv", "coefficient of variation of fraction of administered dose of API absorbed to enterocytes, escaping gut wall metabolism and entering portal vein",
                                           placement = "bottom", trigger = "hover",
                                           options = NULL)
                                 
                                 
                          ),
                          column(4,
                                 tags$h4(
                                   tags$b(
                                     "Distribution parameters:"
                                   )
                                 ),
                                 
                                 tags$hr(),
                                 actionButton("reset_population_defaults_pharmacokinetics_distribution", "Default settings"), # Reset population pharmacokinetics settings button
                                 tags$hr(),
                                 
                                 numericInput(inputId = "fup_m_lognorm_2_m",
                                              label = "fup m_lognormal_2 and s_lognormal_2 1st param (m)", # [Simcyp Simulator v.16]
                                              value = 0.060),
                                 
                                 # Alerts
                                 bsAlert("fup_m_lognorm_2_m_alert"),
                                 
                                 numericInput(inputId = "fup_m_lognorm_2_cv",
                                              label = "fup m_lognormal_2 and s_lognormal_2 2nd param (cv)", # [Simcyp Simulator v.16]
                                              value = 0.018),
                                 # Alerts
                                 bsAlert("fup_m_lognorm_2_cv_alert"),
                                 
                                 numericInput(inputId = "BP_m_lognorm_2_m",
                                              label = "BP m_lognormal_2 and s_lognormal_2 1st param (m)", # [Simcyp Simulator v.16]
                                              value = 0.877),
                                 # Alerts
                                 bsAlert("BP_m_lognorm_2_m_alert"),
                                 
                                 numericInput(inputId = "BP_m_lognorm_2_cv",
                                              label = "BP m_lognormal_2 and s_lognormal_2 2nd param (cv)", # [Simcyp Simulator v.16]
                                              value = 0.168),
                                 # Alerts
                                 bsAlert("BP_m_lognorm_2_cv_alert"),
                                 
                                 numericInput(inputId = "BP_metab_m_lognorm_2_m",
                                              label = "BP_metab m_lognormal_2 and s_lognormal_2 1st param (m)", # [Simcyp Simulator v.16]
                                              value = 1.97),
                                 # Alerts
                                 bsAlert("BP_metab_m_lognorm_2_m_alert"),
                                 
                                 numericInput(inputId = "BP_metab_m_lognorm_2_cv",
                                              label = "BP_metab m_lognormal_2 and s_lognormal_2 2nd param (cv)", # [Simcyp Simulator v.16]
                                              value = 0.22),
                                 # Alerts
                                 bsAlert("BP_metab_m_lognorm_2_cv_alert"),
                                 
                                 numericInput(inputId = "fup_metab_1",
                                              label = "fup_metab [a] param (y = a*x +b)", # [Simcyp Simulator v.16]
                                              value = 0.8231),
                                 # Alerts
                                 bsAlert("fup_metab_1_alert"),
                                 
                                 numericInput(inputId = "fup_metab_2",
                                              label = "fup_metab [b] param (y = a*x +b)", # [Simcyp Simulator v.16]
                                              value = 0.0394),
                                 # Alerts
                                 bsAlert("fup_metab_2_alert"),
                                 
                                 
                                 # Tooltips
                                 bsTooltip("reset_population_defaults_pharmacokinetics_distribution", "Reset all settings of \\'Distribution parameters\\' to defaults.",
                                           placement = "bottom", trigger = "hover",
                                           options = NULL),
                                 bsTooltip("fup_m_lognorm_2_m", "mean of fraction of API unbound in plasma",
                                           placement = "bottom", trigger = "hover",
                                           options = NULL),
                                 bsTooltip("fup_m_lognorm_2_cv", "coefficient of variation of fraction of API unbound in plasma",
                                           placement = "bottom", trigger = "hover",
                                           options = NULL),
                                 bsTooltip("BP_m_lognorm_2_m","mean of blood to plasma partition coefficient for API",
                                           placement = "bottom", trigger = "hover",
                                           options = NULL),
                                 bsTooltip("BP_m_lognorm_2_cv","coefficient of variation of blood to plasma partition coefficient for API",
                                           placement = "bottom", trigger = "hover",
                                           options = NULL),
                                 bsTooltip("BP_metab_m_lognorm_2_m","mean of blood to plasma partition coefficient for API\\'s metabolite",
                                           placement = "bottom", trigger = "hover",
                                           options = NULL),
                                 bsTooltip("BP_metab_m_lognorm_2_cv","coefficient of variation of blood to plasma partition coefficient for API\\'s metabolite",
                                           placement = "bottom", trigger = "hover",
                                           options = NULL),
                                 bsTooltip("fup_metab_1","fraction of unbound metabolite in plasma 1",
                                           placement = "bottom", trigger = "hover",
                                           options = NULL),
                                 bsTooltip("fup_metab_2", "fraction of unbound metabolite in plasma 2",
                                           placement = "bottom", trigger = "hover",
                                           options = NULL)
                                 
                                 
                          )
                        ),
                        mainPanel(
                          
                          busyIndicator()
                          
                        )
                        
                      )
                     
                      ),
             tabPanel("Population dataset", fluid=TRUE,
                      sidebarLayout(
                        sidebarPanel(
                          titlePanel(h4(
                            "Save population dataset as csv")),
                          tags$hr(),
                          # Input: Checkbox if file has column name ----
                          checkboxInput("pop_data_colnames", "Column names", TRUE),
                          
                          # Input: Checkbox if file has row name ----
                          checkboxInput("pop_data_rownames", "Row names", TRUE),
                          
                          # Input: Select separator ----
                          radioButtons("pop_data_sep", "Separator",
                                       choices = c(Comma = ",",
                                                   Semicolon = ";",
                                                   Tab = "\t"),
                                       selected = ","),
                          
                          downloadButton(outputId="downloadData", label="Download Data")
                        ),
                        mainPanel(
                          
                          dataTableOutput("table_pop"),
                          busyIndicator()
                          
                          ) 
                        )
                      ),
             tabPanel("Population Plots",fluid=TRUE,
                      tags$h4("Download plots"),
                      tags$hr(),
                      sidebarLayout(
                          sidebarPanel(
                          tags$h5("Age plot settings"),
                          numericInput("downloadPlot_pop_age_width", "Width [mm]", value = 120, min = 10, max = 1200),
                          numericInput("downloadPlot_pop_age_height", "Height [mm]", value = 90, min = 30, max = 900),
                          numericInput("downloadPlot_pop_age_dpi", "Resolution [dpi]", value = 600, min = 72, max = 1200),
                          radioButtons("downloadPlot_pop_age_device", "Format", choices = c("png","pdf","jpeg","eps"), selected = "png", inline = TRUE),
                          downloadButton("downloadPlot_pop_age","Download age plot"),
                          tags$hr(),
                          tags$h5("Weight plot settings"),
                          numericInput("downloadPlot_pop_weight_width", "Width [mm]", value = 120, min = 10, max = 1200),
                          numericInput("downloadPlot_pop_weight_height", "Height [mm]", value = 90, min = 30, max = 900),
                          numericInput("downloadPlot_pop_weight_dpi", "Resolution [dpi]", value = 600, min = 72, max = 1200),
                          radioButtons("downloadPlot_pop_weight_device", "Format", choices = c("png","pdf","jpeg","eps"), selected = "png", inline = TRUE),
                          downloadButton("downloadPlot_pop_weight","Download weight plot"),
                          tags$hr(),
                          tags$h5("Height plot settings"),
                          numericInput("downloadPlot_pop_height_width", "Width [mm]", value = 120, min = 10, max = 1200),
                          numericInput("downloadPlot_pop_height_height", "Height [mm]", value = 90, min = 30, max = 900),
                          numericInput("downloadPlot_pop_height_dpi", "Resolution [dpi]", value = 600, min = 72, max = 1200),
                          radioButtons("downloadPlot_pop_height_device", "Format", choices = c("png","pdf","jpeg","eps"), selected = "png", inline = TRUE),
                          downloadButton("downloadPlot_pop_height","Download height plot"),
                          tags$hr(),
                          tags$h5("Mean CO plot settings"),
                          numericInput("downloadPlot_pop_mean_CO_Tanner_width", "Width [mm]", value = 120, min = 10, max = 1200),
                          numericInput("downloadPlot_pop_mean_CO_Tanner_height", "Height [mm]", value = 90, min = 30, max = 900),
                          numericInput("downloadPlot_pop_mean_CO_Tanner_dpi", "Resolution [dpi]", value = 600, min = 72, max = 1200),
                          radioButtons("downloadPlot_pop_mean_CO_Tanner_device", "Format", choices = c("png","pdf","jpeg","eps"), selected = "png", inline = TRUE),
                          downloadButton("downloadPlot_pop_mean_CO_Tanner","Download Mean CO plot")

                          ),
                          mainPanel(
                            busyIndicator(),
                            # plotOutput("pop_age"),
                            plotOutput("pop_age"),
                            plotOutput("pop_weight"),
                            plotOutput("pop_height"),
                            plotOutput("pop_mean_CO_Tanner"),
                            busyIndicator()
                          )
                        )
                      )
             ),

  tabPanel("Study",
           tags$h4("Study specific parameters"),
           tags$hr(),
           fluidPage(
             fluidRow(
               column(4,
                      tags$h4(
                        tags$b(
                          "Oral dose parameters:"
                        )
                      ),
                      tags$hr(),
                      actionButton("reset_study_specific_defaults_oral", "Default settings"), # Reset study specific parameters settings button
                      tags$hr(),
                      numericInput(inputId = "dose",
                                   label = "Oral dose [mg]", 
                                   value = 22), # calculated as neutral entity
                      # bsAlert
                      bsAlert("dose_alert"),
                      
                      numericInput(inputId = "no_doses",
                                   label = "No of doses", 
                                   value = 3),
                      # bsAlert
                      bsAlert("no_doses_alert"),
                      
                      numericInput(inputId = "dose_every",
                                   label = "Dosing interval [h]", 
                                   value = 8),
                      # bsAlert
                      bsAlert("dose_every_alert"),
                      
                      numericInput(inputId = "inf_dose",
                                   label = "Intravenous dose [mg]",
                                   value = 0),
                      # bsAlert
                      bsAlert("inf_dose_alert"),
                      
                      numericInput(inputId = "inf_time",
                                   label = "Infusion time [h]", 
                                   value = 2),
                      # bsAlert
                      bsAlert("inf_time_alert"),
                      
                      numericInput(inputId = "t_end",
                                   label = "Duration of simulation [h]", # [Simcyp Simulator v.16]
                                   value = 84),
                      # bsAlert
                      bsAlert("t_end_alert"),
                      

                      #Tooltips 
                      bsTooltip("dose", "dose of API as neutral entity. E.g Amitryptyline hydrochloride MW = 313.9 and dose 25 mg, amitryptyline (neutral) MW = 277.4 and dose 22 mg.",
                                placement = "bottom", trigger = "hover",
                                options = NULL),
                      bsTooltip("no_doses", "number of doses in all study period",
                                placement = "bottom", trigger = "hover",
                                options = NULL),
                      bsTooltip("dose_every", "time interval between doses [h]",
                                placement = "bottom", trigger = "hover",
                                options = NULL),
                      bsTooltip("inf_dose", "intravenous dose of API as neutral entity [mg]",
                                placement = "bottom", trigger = "hover",
                                options = NULL),
                      bsTooltip("inf_time", "duration of infusion [h]",
                                placement = "bottom", trigger = "hover",
                                options = NULL),
                      bsTooltip("t_end", "duration of simulated study [h]",
                                placement = "bottom", trigger = "hover",
                                options = NULL)
                      
               ),
               column(4,
                      tags$h4(
                        tags$b(
                          "Scaling factors:"
                        )
                      ),
                      tags$hr(),
                      actionButton("reset_scaling_factors_defaults", "Default settings"), # Reset scaling factors settings button
                      tags$hr(),
                      numericInput(inputId = "ISEF1A2",
                                   label = "ISEF1A2", # [Simcyp Simulator v.16]
                                   value = 11.1),
                      # bsAlert
                      bsAlert("ISEF1A2_alert"),
                      
                      numericInput(inputId = "ISEF2C19",
                                   label = "ISEF2C19", # [Simcyp Simulator v.16]
                                   value = 3.07),
                      # bsAlert
                      bsAlert("ISEF2C19_alert"),
                      
                      numericInput(inputId = "ISEF2D6",
                                   label = "ISEF2D6", # [Simcyp Simulator v.16]
                                   value = 0.74),
                      # bsAlert
                      bsAlert("ISEF2D6_alert"),
                      
                      numericInput(inputId = "ISEF2C9",
                                   label = "ISEF2C9", # [Simcyp Simulator v.16]
                                   value = 5.73),
                      # bsAlert
                      bsAlert("ISEF2C9_alert"),
                      
                      numericInput(inputId = "ISEF3A4",
                                   label = "ISEF3A4", # [Simcyp Simulator v.16]
                                   value = 3.92),
                      # bsAlert
                      bsAlert("ISEF3A4_alert"),
                      
                      numericInput(inputId = "ISEF2B6",
                                   label = "ISEF2B6", # [Simcyp Simulator v.16]
                                   value = 3.7),
                      # bsAlert
                      bsAlert("ISEF2B6_alert"),
                      
                      numericInput(inputId = "ISEF2C8",
                                   label = "ISEF2C8", # [Simcyp Simulator v.16]
                                   value = 3.7),
                      # bsAlert
                      bsAlert("ISEF2C8_alert"),
                      
                      # Tooltips
                      bsTooltip("ISEF1A2", "intersystem Extrapolation Factor (source of data: Simcyp Simulator v.17.1) for cytochrome CYP1A2",
                                placement = "bottom", trigger = "hover",
                                options = NULL),
                      bsTooltip("ISEF2C19", "intersystem Extrapolation Factor (source of data: Simcyp Simulator v.17.1) for cytochrome CYP2C19",
                                placement = "bottom", trigger = "hover",
                                options = NULL),
                      bsTooltip("ISEF2D6", "intersystem Extrapolation Factor (source of data: Simcyp Simulator v.17.1) for cytochrome CYP2D6",
                                placement = "bottom", trigger = "hover",
                                options = NULL),
                      bsTooltip("ISEF2C9", "intersystem Extrapolation Factor (source of data: Simcyp Simulator v.17.1) for cytochrome CYP2C9",
                                placement = "bottom", trigger = "hover",
                                options = NULL),
                      bsTooltip("ISEF3A4", "intersystem Extrapolation Factor (source of data: Simcyp Simulator v.17.1) for cytochrome CYP3A4",
                                placement = "bottom", trigger = "hover",
                                options = NULL),
                      bsTooltip("ISEF2B6", "intersystem Extrapolation Factor (source of data: Simcyp Simulator v.17.1) for cytochrome CYP2B6",
                                placement = "bottom", trigger = "hover",
                                options = NULL),
                      bsTooltip("ISEF2C8", "intersystem Extrapolation Factor (source of data: Simcyp Simulator v.17.1) for cytochrome CYP2C8",
                                placement = "bottom", trigger = "hover",
                                options = NULL)
               ),
               column(4,
                      tags$h4(
                        tags$b(
                          "Fraction of unbound API in vitro:"
                        )
                      ),
                      tags$hr(),
                      actionButton("reset_study_specific_defaults_fumic", "Default settings"), # Reset study specific parameters settings button
                      tags$hr(),
                      numericInput(inputId = "fumic",
                                   label = "fumic", # [Simcyp Simulator v.16]
                                   value = 0.82),
                      # bsAlert
                      bsAlert("fumic_alert"),
                      
                      numericInput(inputId = "fumic_metab",
                                   label = "fumic_metab", # [Simcyp Simulator v.16]
                                   value = 0.82),
                      # bsAlert
                      bsAlert("fumic_metab_alert"),
                      
                      
                      # Tooltips
                      bsTooltip("fumic", "fraction of API unbound in an in vitro microsomal preparation",
                                placement = "bottom", trigger = "hover",
                                options = NULL),
                      bsTooltip("fumic_metab", "fraction of API's metabolite unbound in an in vitro microsomal preparation",
                                placement = "bottom", trigger = "hover",
                                options = NULL)

                      )
               
             ),
             mainPanel(
               busyIndicator()
             )
           )
           
           ),
  tabPanel("API",
           
           tags$h4("Drug specific parameters"),
           tags$hr(),
           fluidPage(
             fluidRow(
               column(4,
                      tags$h4(
                        tags$b(
                          "Physico-chemical parameters:"
                        )
                      ),
                      tags$hr(),
                      actionButton("reset_API_defaults_phys_chem_param", "Default settings"), # Reset API phys-chem parameters settings button
                      
                      tags$hr(),
                      tags$h6("METABOLITE"),
                      tags$hr(),
                      
                      checkboxInput("METAB_present","Calculate concentration of metabolite",FALSE),
                      conditionalPanel(condition = "input.METAB_present==true",
                                       numericInput(inputId = "MW_metab",
                                                    label = "Metabolite's molecular weight [g/mol]", #
                                                    value = 263.384,
                                                    min=1),
                                       # bsAlert
                                       bsAlert("MW_metab_alert"),
                                       
                                       numericInput(inputId = "pKa_metab",
                                                    label = "Metabolite's pKa", #
                                                    value = 10.1,
                                                    min=0),
                                       # bsAlert
                                       bsAlert("pKa_metab_alert"),
                                       
                                       numericInput(inputId = "Kpre_metab",
                                                    label = "Kpre_metab # 28.73022 # x[1]", # [Simcyp Simulator v.16]
                                                    value = 28.73022),
                                       # bsAlert
                                       bsAlert("Kpre_metab_alert"),
                                       
                                       
                                       # Tooltips
                                       bsTooltip("MW_metab", "API\\'s metabolite molecular weight [g/mol] as neutral entity",
                                                 placement = "bottom", trigger = "hover",
                                                 options = NULL),
                                       bsTooltip("pKa_metab", "the negative base-10 logarithm of the acid dissociation constant of API's metabolite as neutral entity",
                                                 placement = "bottom", trigger = "hover",
                                                 options = NULL),
                                       bsTooltip("Kpre_metab", "rest of the body partition coefficient for API\\'s metabolite",
                                                 placement = "bottom", trigger = "hover",
                                                 options = NULL)
                                       ),
                      tags$hr(),
                      tags$h6("API"),
                      tags$hr(),
                      numericInput(inputId = "MW_api",
                                   label = "API's molecular weight [g/mol]", #
                                   value = 277.4,
                                   min=1),
                      # bsAlert
                      bsAlert("MW_api_alert"),
                      
                      numericInput(inputId = "pKa_api",
                                   label = "API's pKa", #
                                   value = 9.41,
                                   min=0),
                      # bsAlert
                      bsAlert("pKa_api_alert"),
                      
                      numericInput(inputId = "PAMPA",
                                   label = "Parallel artificial membrane permeability [cm/s]", #
                                   value = 12.3*10^(-6),
                                   min=0),
                      # bsAlert
                      bsAlert("PAMPA_alert"),
                      
                      # Tooltips
                      bsTooltip("MW_api", "API\\'s molecular weight [g/mol] as neutral entity",
                                placement = "bottom", trigger = "hover",
                                options = NULL),
                      bsTooltip("pKa_api", "the negative base-10 logarithm of the acid dissociation constant of API as neutral entity",
                                placement = "bottom", trigger = "hover",
                                options = NULL),
                      bsTooltip("PAMPA", "in vitro parallel artificial membrane permeability (PAMPA) assay result [cm/s] for API",
                                placement = "bottom", trigger = "hover",
                                options = NULL)

               ),
               
               column(4,
                      tags$h4(
                        tags$b(
                          "Distribution:"
                        )
                      ),
                      tags$hr(),
                      actionButton("reset_tissue_partition_coefficients_defaults_param", "Default settings"), # Reset tissue part coef parameters settings button
                      tags$hr(),
                      tags$h6("API"),
                      tags$hr(),
                      
                      numericInput(inputId = "fu_ht_api",
                                   label = "Fraction of API unbound in heart tissue", # [Simcyp Simulator v.16]
                                   value = 0.0012),
                      # bsAlert
                      bsAlert("fu_ht_api_alert"),
                      
                      numericInput(inputId = "CLefflux",
                                   label = "CLefflux", # [Simcyp Simulator v.16]
                                   value = 2.996919),
                      # bsAlert
                      bsAlert("CLefflux_alert"),
                      
                      numericInput(inputId = "CLuptake",
                                   label = "CLuptake", # [Simcyp Simulator v.16]
                                   value = 2.177201),
                      # bsAlert
                      bsAlert("CLuptake_alert"),

                      tags$hr(),
                      
                      checkboxInput("Kpad_API","Adipose partition coefficient",TRUE),
                      conditionalPanel(condition = "input.Kpad_API==true",
                        numericInput(inputId = "Kpad",
                                     label = "Adipose partition coefficient", #
                                     value = 4.10,
                                     min=0),
                        # bsAlert
                        bsAlert("Kpad_alert")
                        ),
                      
                      checkboxInput("Kpbo_API","Bone partition coefficient",TRUE),
                      conditionalPanel(condition = "input.Kpbo_API==true",
                        numericInput(inputId = "Kpbo",
                                     label = "Bone partition coefficient", #
                                     value = 4.14,
                                     min=0),
                        # bsAlert
                        bsAlert("Kpbo_alert")
                      ),
                      
                      checkboxInput("Kpbr_API","Brain partition coefficient",TRUE),
                      conditionalPanel(condition = "input.Kpbr_API==true",
                        numericInput(inputId = "Kpbr",
                                     label = "Brain partition coefficient", #
                                     value = 3.05,
                                     min=0),
                        # bsAlert
                        bsAlert("Kpbr_alert")
                      ),
                      
                      checkboxInput("Kpgu_API","Gut partition coefficient",TRUE),
                      conditionalPanel(condition = "input.Kpgu_API==true",
                        numericInput(inputId = "Kpgu",
                                     label = "Gut partition coefficient", #
                                     value = 11.73,
                                     min=0),
                        # bsAlert
                        bsAlert("Kpgu_alert")
                      ),
                      
                      checkboxInput("Kphe_API","Heart partition coefficient",TRUE),
                      conditionalPanel(condition = "input.Kphe_API==true",
                        numericInput(inputId = "Kphe",
                                     label = "Heart partition coefficient", #
                                     value = 11.77,
                                     min=0),
                        # bsAlert
                        bsAlert("Kphe_alert")
                      ),
                      
                      checkboxInput("Kppf_API","Pericardial fluid partition coefficient",TRUE),
                      conditionalPanel(condition = "input.Kppf_API==true",
                        numericInput(inputId = "Kppf",
                                     label = "Pericardial fluid partition coefficient", #
                                     value = 2.6,
                                     min=0),
                        # bsAlert
                        bsAlert("Kppf_alert")
                      ),
                      
                      checkboxInput("Kpec_API","Extracellular partition coefficient",TRUE),
                      conditionalPanel(condition = "input.Kpec_API==true",
                        numericInput(inputId = "Kpec",
                                     label = "Extracellular  partition coefficient", #
                                     value = 1,
                                     min=0),
                        # bsAlert
                        bsAlert("Kpec_alert")
                      ),
                      
                      checkboxInput("Kpki_API","Kidney partition coefficient",TRUE),
                      conditionalPanel(condition = "input.Kpki_API==true",
                        numericInput(inputId = "Kpki",
                                     label = "Kidney  partition coefficient", #
                                     value = 9.79,
                                     min=0),
                        # bsAlert
                        bsAlert("Kpki_alert")
                      ),
                      
                      checkboxInput("Kpli_API","Liver partition coefficient",TRUE),
                      conditionalPanel(condition = "input.Kpli_API==true",
                        numericInput(inputId = "Kpli",
                                     label = "Liver  partition coefficient", #
                                     value = 19.80,
                                     min=0),
                        # bsAlert
                        bsAlert("Kpli_alert")
                      ),
                      
                      checkboxInput("Kplu_API","Lung partition coefficient",TRUE),
                      conditionalPanel(condition = "input.Kplu_API==true",
                        numericInput(inputId = "Kplu",
                                     label = "Lung  partition coefficient", #
                                     value = 2.05,
                                     min=0),
                        # bsAlert
                        bsAlert("Kplu_alert")
                      ),
                      
                      checkboxInput("Kpmu_API","Muscle partition coefficient",TRUE),
                      conditionalPanel(condition = "input.Kpmu_API==true",
                        numericInput(inputId = "Kpmu",
                                     label = "Muscle  partition coefficient", #
                                     value = 9.85,
                                     min=0),
                        # bsAlert
                        bsAlert("Kpmu_alert")
                      ),
                      
                      checkboxInput("Kpsk_API","Skin partition coefficient",TRUE),
                      conditionalPanel(condition = "input.Kpsk_API==true",
                        numericInput(inputId = "Kpsk",
                                     label = "Skin  partition coefficient", #
                                     value = 5.61,
                                     min=0),
                        # bsAlert
                        bsAlert("Kpsk_alert")
                      ),
                      
                      checkboxInput("Kpsp_API","Spleen partition coefficient",TRUE),
                      conditionalPanel(condition = "input.Kpsp_API==true",
                        numericInput(inputId = "Kpsp",
                                     label = "Spleen  partition coefficient", #
                                     value = 11.02,
                                     min=0),
                        # bsAlert
                        bsAlert("Kpsp_alert")
                      ),
                      
                      checkboxInput("Kpre_API","Rest of the body partition coefficient",TRUE),
                      conditionalPanel(condition = "input.Kpre_API==true",
                        numericInput(inputId = "Kpre",
                                     label = "Rest of the body  partition coefficient", #
                                     value = 1,
                                     min=0),
                        # bsAlert
                        bsAlert("Kpre_alert")
                      ),
                      
                      
                      # Tooltips
                      
                      bsTooltip("Kppf_API", "pericardial fluid partition coefficient (postmortem data)",
                                placement = "bottom", trigger = "hover",
                                options = NULL),
                      bsTooltip("Kppf", "pericardial fluid partition coefficient (postmortem data)",
                                placement = "bottom", trigger = "hover",
                                options = NULL),
                      bsTooltip("Kpec_API", "partition coefficient between plasma and heart extracellular water",
                                placement = "bottom", trigger = "hover",
                                options = NULL),
                      bsTooltip("Kpec", "partition coefficient between plasma and heart extracellular water",
                                placement = "bottom", trigger = "hover",
                                options = NULL),
                      bsTooltip("CLefflux", "the active efflux transport of API at the membranes of cardiomyocyte (intracellular) compartments [L/h]",
                                placement = "bottom", trigger = "hover",
                                options = NULL),
                      bsTooltip("CLuptake", "the active uptake transport of API at the membranes of cardiomyocyte (intracellular) compartments [L/h]",
                                placement = "bottom", trigger = "hover",
                                options = NULL),
                      
                      tags$hr(),
                      tags$h6("METABOLITE"),
                      tags$hr(),
                      
                      checkboxInput("Kpli_METAB","Liver partition coefficient",FALSE),
                      conditionalPanel(condition = "input.Kpli_METAB==true & input.METAB_present==true",
                        numericInput(inputId = "Kpli_metab",
                                     label = "Metabolite's liver partition coefficient", #
                                     value = 59.08,
                                     min=0),
                        # bsAlert
                        bsAlert("Kpli_metab_alert")
                      ),
                      
                      checkboxInput("Kphe_METAB","Heart partition coefficient",FALSE),
                      conditionalPanel(condition = "input.Kphe_METAB==true & input.METAB_present==true",
                        numericInput(inputId = "Kphe_metab",
                                     label = "Metabolite's kidney partition coefficient", #
                                     value = 35.63,
                                     min=0),
                        # bsAlert
                        bsAlert("Kphe_metab_alert")
                      ),
                      # Tooltips
                      
                      bsTooltip("Kpli_METAB", "metabolite\\'s liver partition coefficient",
                                placement = "bottom", trigger = "hover",
                                options = NULL),
                      bsTooltip("Kphe_METAB", "metabolite\\'s heart partition coefficient",
                                placement = "bottom", trigger = "hover",
                                options = NULL)
                      
               ),
               
               column(4,
                      tags$h4(
                        tags$b(
                          "Elimination:"
                        )
                      ),
                      tags$hr(),
                      actionButton("reset_elimination_defaults_param", "Default settings"), # Reset metabolism kinetics settings button
                      tags$hr(),
                      tags$h6("API"),
                      tags$hr(),
                      
                      numericInput(inputId = "fuha",
                                   label = "fuha", # [Simcyp Simulator v.16]
                                   value = 0.1230352),
                      # bsAlert
                      bsAlert("fuha_alert"),
                      
                      numericInput(inputId = "CLrenal",
                                   label = "CLrenal [L/h]", # [Simcyp Simulator v.16]
                                   value = 0.09),
                      # bsAlert
                      bsAlert("CLrenal_alert"),

                      tags$hr(),
                      
                      tags$h5("Pathway 1"),
                      
                      checkboxInput("CYP1A2_API_dm","CYP1A2_API", TRUE),
                      conditionalPanel(condition = "input.CYP1A2_API_dm==true",
                        numericInput(inputId = "Vmax_1A2_api_dm",
                                     label = "Vmax for API [pmol/min/pmol] CYP1A2", #
                                     value = 59.08,
                                     min=0),
                        # bsAlert
                        bsAlert("Vmax_1A2_api_dm_alert"),
                        
                        numericInput(inputId = "Km_1A2_api_dm",
                                     label = "Km for API [µM] CYP1A2", #
                                     value = 35.63,
                                     min=0),
                        # bsAlert
                        bsAlert("Km_1A2_api_dm_alert")
                        
                      ),
                      
                      checkboxInput("CYP2B6_API_dm","CYP2B6_API", TRUE),
                      conditionalPanel(condition = "input.CYP2B6_API_dm==true",
                        numericInput(inputId = "Vmax_2B6_api_dm",
                                     label = "Vmax for API [pmol/min/pmol] CYP2B6", #
                                     value = 0.25,
                                     min=0),
                        # bsAlert
                        bsAlert("Vmax_2B6_api_dm_alert"), 
                        
                        numericInput(inputId = "Km_2B6_api_dm",
                                     label = "Km for API [µM] CYP2B6", #
                                     value = 56.7,
                                     min=0),
                        # bsAlert
                        bsAlert("Km_2B6_api_dm_alert")
                        
                      ),
                      
                      checkboxInput("CYP2C8_API_dm","CYP2C8_API", TRUE),
                      conditionalPanel(condition = "input.CYP2C8_API_dm==true",
                        numericInput(inputId = "Vmax_2C8_api_dm",
                                     label = "Vmax for API [pmol/min/pmol] CYP2C8", #
                                     value = 0.7,
                                     min=0),
                        # bsAlert
                        bsAlert("Vmax_2C8_api_dm_alert"),
                        
                        numericInput(inputId = "Km_2C8_api_dm",
                                     label = "Km for API [µM] CYP2C8", #
                                     value = 9.74,
                                     min=0),
                        # bsAlert
                        bsAlert("Km_2C8_api_dm_alert")
                        
                      ),
                      
                      checkboxInput("CYP2C9_API_dm","CYP2C9_API", TRUE),
                      conditionalPanel(condition = "input.CYP2C9_API_dm==true",
                        numericInput(inputId = "Vmax_2C9_api_dm",
                                     label = "Vmax for API [pmol/min/pmol] CYP2C9", #
                                     value = 3.97,
                                     min=0),
                        # bsAlert
                        bsAlert("Vmax_2C9_api_dm_alert"),
                        
                        numericInput(inputId = "Km_2C9_api_dm",
                                     label = "Km for API [µM] CYP2C9", #
                                     value = 50.5,
                                     min=0),
                        # bsAlert
                        bsAlert("Km_2C9_api_dm_alert")
                        
                      ),
                      
                      checkboxInput("CYP2C19_API_dm","CYP2C19_API", TRUE),
                      conditionalPanel(condition = "input.CYP2C19_API_dm==true",
                        numericInput(inputId = "Vmax_2C19_api_dm",
                                     label = "Vmax for API [µM] CYP2C19", #
                                     value = 4.22,
                                     min=0),
                        # bsAlert
                        bsAlert("Vmax_2C19_api_dm_alert"),
                        
                        numericInput(inputId = "Km_2C19_api_dm",
                                     label = "Km for API [µM] CYP2C19", #
                                     value = 8.52,
                                     min=0),
                        # bsAlert
                        bsAlert("Km_2C19_api_dm_alert")
                        
                      ),
                      
                      checkboxInput("CYP2D6_API_dm","CYP2D6_API", TRUE),
                      conditionalPanel(condition = "input.CYP2D6_API_dm==true",
                        numericInput(inputId = "Vmax_2D6_api_dm",
                                     label = "Vmax for API [µM] CYP2D6", #
                                     value = 1.49,
                                     min=0),
                        # bsAlert
                        bsAlert("Vmax_2D6_api_dm_alert"),
                        
                        numericInput(inputId = "Km_2D6_api_dm",
                                     label = "Km for API [µM] CYP2C9", #
                                     value = 7.12,
                                     min=0),
                        # bsAlert
                        bsAlert("Km_2D6_api_dm_alert")
                        
                      ),
                      
                      checkboxInput("CYP3A4_API_dm","CYP3A4_API", TRUE),
                      conditionalPanel(condition = "input.CYP3A4_API_dm==true",
                        numericInput(inputId = "Vmax_3A4_api_dm",
                                     label = "Vmax for API [µM] CYP3A4", #
                                     value = 3.37,
                                     min=0),
                        # bsAlert
                        bsAlert("Vmax_3A4_api_dm_alert"),
                        
                        numericInput(inputId = "Km_3A4_api_dm",
                                     label = "Km for API [µM] CYP3A4", #
                                     value = 213.8,
                                     min=0),
                        # bsAlert
                        bsAlert("Km_3A4_api_dm_alert")
                        
                      ),
                      
                      tags$h5("Pathway 2"),
                      
                      checkboxInput("CYP2B6_API_h","CYP2B6_API_hydroxylation", TRUE),
                      conditionalPanel(condition = "input.CYP2B6_API_h==true",
                        numericInput(inputId = "Vmax_2B6_api_h",
                                     label = "Vmax for API [µM] CYP2B6 (hydroxylation)", #
                                     value = 0.13,
                                     min=0),
                        # bsAlert
                        bsAlert("Vmax_2B6_api_h_alert"),
                        
                        numericInput(inputId = "Km_2B6_api_h",
                                     label = "Km for API [µM] CYP2B6  (hydroxylation)", #
                                     value = 98,
                                     min=0),
                        # bsAlert
                        bsAlert("Km_2B6_api_h_alert")
                        
                      ),
                      
                      checkboxInput("CYP2D6_API_h","CYP2D6_API_hydroxylation", TRUE),
                      conditionalPanel(condition = "input.CYP2D6_API_h==true",
                        numericInput(inputId = "Vmax_2D6_api_h",
                                     label = "Vmax for API [µM] CYP2D6  (hydroxylation)", #
                                     value = 2.71,
                                     min=0),
                        # bsAlert
                        bsAlert("Vmax_2D6_api_h_alert"),
                        
                        numericInput(inputId = "Km_2D6_api_h",
                                     label = "Km for API [µM] CYP2D6  (hydroxylation)", #
                                     value = 4.75,
                                     min=0),
                        # bsAlert
                        bsAlert("Km_2D6_api_h_alert")
                        
                      ),
                      
                      checkboxInput("CYP3A4_API_h","CYP3A4_API_hydroxylation", TRUE),
                      conditionalPanel(condition = "input.CYP3A4_API_h==true",
                        numericInput(inputId = "Vmax_3A4_api_h",
                                     label = "Vmax for API [µM] CYP3A4  (hydroxylation)", #
                                     value = 0.4,
                                     min=0),
                        # bsAlert
                        bsAlert("Vmax_3A4_api_h_alert"),
                        
                        numericInput(inputId = "Km_3A4_api_h",
                                     label = "Km for API [µM] CYP3A4  (hydroxylation)", #
                                     value = 69.3,
                                     min=0),
                        # bsAlert
                        bsAlert("Km_3A4_api_h_alert")
                        
                      ),
                      
                      
                      tags$hr(),
                      tags$h6("METABOLITE"),
                      tags$hr(),
                      
                      numericInput(inputId = "fuhn",
                                   label = "fuhn", # [Simcyp Simulator v.16]
                                   value = 0.1575158),
                      # bsAlert
                      bsAlert("fuhn_alert"),
                      
                      numericInput(inputId = "CLrenal_metab",
                                   label = "CLrenal_metab [L/h]", # [Simcyp Simulator v.16]
                                   value = 0.39),
                      # bsAlert
                      bsAlert("CLrenal_metab_alert"),

                      tags$hr(),
                      
                      tags$h5("Pathway 1"),
                      
                      checkboxInput("CYP1A2_METAB_dm","CYP1A2_METAB",FALSE),
                      conditionalPanel(condition = "input.CYP1A2_METAB_dm==true & input.METAB_present==true",
                        numericInput(inputId = "Vmax_1A2_metab_dm",
                                     label = "Vmax for METABOLITE [µM] CYP1A2", #
                                     value = 6.8,
                                     min=0),
                        # bsAlert
                        bsAlert("Vmax_1A2_metab_dm_alert"),
                        
                        numericInput(inputId = "Km_1A2_metab_dm",
                                     label = "Km for METABOLITE [µM] CYP1A2", #
                                     value = 54.2,
                                     min=0),
                        # bsAlert
                        bsAlert("Km_1A2_metab_dm_alert")
                        
                      ),
                      
                      checkboxInput("CYP2C19_METAB_dm","CYP2C19_METAB",FALSE),
                      conditionalPanel(condition = "input.CYP2C19_METAB_dm==true & input.METAB_present==true",
                        numericInput(inputId = "Vmax_2C19_metab_dm",
                                     label = "Vmax for METABOLITE [µM] CYP2C19", #
                                     value = 93.1,
                                     min=0),
                        # bsAlert
                        bsAlert("Vmax_2C19_metab_dm_alert"),
                        
                        numericInput(inputId = "Km_2C19_metab_dm",
                                     label = "Km for METABOLITE [µM] CYP2C19", #
                                     value = 118,
                                     min=0),
                        # bsAlert
                        bsAlert("Km_2C19_metab_dm_alert")
                        
                      ),
                      
                      checkboxInput("CYP2D6_METAB_dm","CYP2D6_METAB",FALSE),
                      conditionalPanel(condition = "input.CYP2D6_METAB_dm==true & input.METAB_present==true",
                        numericInput(inputId = "Vmax_2D6_metab_dm",
                                     label = "Vmax for METABOLITE [µM] CYP2D6", #
                                     value = 19.4,
                                     min=0),
                        # bsAlert
                        bsAlert("Vmax_2D6_metab_dm_alert"),
                        
                        numericInput(inputId = "Km_2D6_metab_dm",
                                     label = "Km for METABOLITE [µM] CYP2D6", #
                                     value = 0.48,
                                     min=0),
                        # bsAlert
                        bsAlert("Km_2D6_metab_dm_alert")
                        
                      ),
                      
                      tags$h5("Pathway 2"),
                      checkboxInput("CYP2D6_METAB_h","CYP2D6_METAB_hydroxylation",FALSE),
                      conditionalPanel(condition = "input.CYP2D6_METAB_h==true & input.METAB_present==true",
                        numericInput(inputId = "Vmax_2D6_metab_h",
                                     label = "Vmax for METABOLITE [µM] CYP2D6  (hydroxylation)", #
                                     value = 130,
                                     min=0),
                        # bsAlert
                        bsAlert("Vmax_2D6_metab_h_alert"),
                        
                        numericInput(inputId = "Km_2D6_metab_h",
                                     label = "Km for METABOLITE [µM] CYP2D6  (hydroxylation)", #
                                     value = 0.74,
                                     min=0),
                        # bsAlert
                        bsAlert("Km_2D6_metab_h_alert")
                        
                      ),
                      
                      # Tooltips
                      bsTooltip("CLrenal", "renal clearance of API [L/h]",
                                placement = "bottom", trigger = "hover",
                                options = NULL),
                      bsTooltip("CLrenal_metab", "renal clearance of API\\'s metabolite [L/h]",
                                placement = "bottom", trigger = "hover",
                                options = NULL),
                      bsTooltip("fuha", "the fraction of API actively transported to hepatocytes",
                                placement = "bottom", trigger = "hover",
                                options = NULL),
                      bsTooltip("fuhn", "the fraction of API\\'s metabolite actively transported to hepatocytes",
                                placement = "bottom", trigger = "hover",
                                options = NULL)

                      
                      )
               
             ),
             mainPanel(
               busyIndicator()
             )
           )
           ),
  
  tabPanel("Run",
           tags$h4("Start simulation"),
           tags$hr(),
           sidebarLayout(
             sidebarPanel(width = 3,
                      h4("Simulation"),
                      actionButton("run_sim","Run"),
                      tags$hr(),
                      textInput("api_plot_caption", "API name", value = "Amitryptyline"),
                      tags$hr(),
                      conditionalPanel(condition = "input.METAB_present==true",
                        textInput("metab_plot_caption", "Metabolite name", value = "Nortryptyline")
                      ),
                      tags$hr(),
                      # Input: Select a file ----
                      h4("Add data to API plots"),
                      fileInput("in_external_data", "Choose CSV File",
                                multiple = TRUE,
                                accept = c("text/csv",
                                           "text/comma-separated-values,text/plain",
                                           ".csv")),
                      
                      # Input: Checkbox if file has header ----
                      checkboxInput("in_external_header", "Header", TRUE),
                      
                      # Input: Select separator ----
                      radioButtons("in_external_sep", "Separator",
                                   choices = c(Comma = ",",
                                               Semicolon = ";",
                                               Tab = "\t"),
                                   selected = ","),
                      
                      # Input: Select quotes ----
                      radioButtons("in_external_quote", "Quote",
                                   choices = c(None = "",
                                               "Double Quote" = '"',
                                               "Single Quote" = "'"),
                                   selected = '"'),
                      conditionalPanel(condition = "input.METAB_present==true",
                                       tags$hr(),
                                       # Input: Select a file ----
                                       h4("Add data to metabolite plots"),
                                       fileInput("in_external_data_metab", "Choose CSV File",
                                                 multiple = TRUE,
                                                 accept = c("text/csv",
                                                            "text/comma-separated-values,text/plain",
                                                            ".csv")),
                                       
                                       # Input: Checkbox if file has header ----
                                       checkboxInput("in_external_metab_header", "Header", TRUE),
                                       
                                       # Input: Select separator ----
                                       radioButtons("in_external_metab_sep", "Separator",
                                                    choices = c(Comma = ",",
                                                                Semicolon = ";",
                                                                Tab = "\t"),
                                                    selected = ","),
                                       
                                       # Input: Select quotes ----
                                       radioButtons("in_external_metab_quote", "Quote",
                                                    choices = c(None = "",
                                                                "Double Quote" = '"',
                                                                "Single Quote" = "'"),
                                                    selected = '"')
                                       ),
                      tags$hr(),
                      h4("Save results dataset as csv"),
                      tags$hr(),
                      # Input: Checkbox if file has column name ----
                      checkboxInput("res_data_colnames", "Column names", TRUE),
                       
                      # Input: Checkbox if file has row name ----
                      checkboxInput("res_data_rownames", "Row names", TRUE),
                       
                      # Input: Select separator ----
                      radioButtons("res_data_sep", "Separator",
                                    choices = c(Comma = ",",
                                                Semicolon = ";",
                                                Tab = "\t"),
                                    selected = ","),
                      downloadButton(outputId="downloadResults", label="Download Results"),
                      tags$hr(),
                      tags$h4("Log conc plot settings in plasma"),
                      numericInput("downloadPlot_res_log_conc_in_venous_plasma_width", "Width [mm]", value = 120, min = 10, max = 1200),
                      numericInput("downloadPlot_res_log_conc_in_venous_plasma_height", "Height [mm]", value = 90, min = 30, max = 900),
                      numericInput("downloadPlot_res_log_conc_in_venous_plasma_dpi", "Resolution [dpi]", value = 600, min = 72, max = 1200),
                      radioButtons("downloadPlot_res_log_conc_in_venous_plasma_device", "Format", choices = c("png","pdf","jpeg","eps"), selected = "png", inline = TRUE),
                      downloadButton("downloadPlot_res_log_conc_in_venous_plasma", "Download plot"),
                      tags$hr(),
                      tags$h4("Conc plot settings in plasma"),
                      numericInput("downloadPlot_res_conc_in_venous_plasma_width", "Width [mm]", value = 120, min = 10, max = 1200),
                      numericInput("downloadPlot_res_conc_in_venous_plasma_height", "Height [mm]", value = 90, min = 30, max = 900),
                      numericInput("downloadPlot_res_conc_in_venous_plasma_dpi", "Resolution [dpi]", value = 600, min = 72, max = 1200),
                      radioButtons("downloadPlot_res_conc_in_venous_plasma_device", "Format", choices = c("png","pdf","jpeg","eps"), selected = "png", inline = TRUE),
                      downloadButton("downloadPlot_res_conc_in_venous_plasma", "Download plot"),
                      tags$hr(),
                      conditionalPanel(condition = "input.METAB_present==true",
                        tags$h4("Metabolite conc plot settings in plasma"),
                        numericInput("downloadPlot_res_conc_metab_in_venous_plasma_width", "Width [mm]", value = 120, min = 10, max = 1200),
                        numericInput("downloadPlot_res_conc_metab_in_venous_plasma_height", "Height [mm]", value = 90, min = 30, max = 900),
                        numericInput("downloadPlot_res_conc_metab_in_venous_plasma_dpi", "Resolution [dpi]", value = 600, min = 72, max = 1200),
                        radioButtons("downloadPlot_res_conc_metab_in_venous_plasma_device", "Format", choices = c("png","pdf","jpeg","eps"), selected = "png", inline = TRUE),
                        downloadButton("downloadPlot_res_conc_metab_in_venous_plasma", "Download plot"),
                        tags$hr(),
                        tags$h4("Stats metabolite conc plot settings in plasma"),
                        sliderInput(inputId = "downloadPlot_stats_res_conc_metab_in_venous_plasma_CI",
                                    label = "Confidence interval [%]:",
                                    value = 95,
                                    min = 0,
                                    max = 100),
                        numericInput("downloadPlot_stats_res_conc_metab_in_venous_plasma_width", "Width [mm]", value = 120, min = 10, max = 1200),
                        numericInput("downloadPlot_stats_res_conc_metab_in_venous_plasma_height", "Height [mm]", value = 90, min = 30, max = 900),
                        numericInput("downloadPlot_stats_res_conc_metab_in_venous_plasma_dpi", "Resolution [dpi]", value = 600, min = 72, max = 1200),
                        radioButtons("downloadPlot_stats_res_conc__metab_in_venous_plasma_device", "Format", choices = c("png","pdf","jpeg","eps"), selected = "png", inline = TRUE),
                        downloadButton("downloadPlot_stats_res_conc_metab_in_venous_plasma", "Download plot")
                      ),
                      tags$hr(),
                      tags$h4("Conc plot settings in heart"),
                      numericInput("downloadPlot_res_conc_in_heart_width", "Width [mm]", value = 120, min = 10, max = 1200),
                      numericInput("downloadPlot_res_conc_in_heart_height", "Height [mm]", value = 90, min = 30, max = 900),
                      numericInput("downloadPlot_res_conc_in_heart_dpi", "Resolution [dpi]", value = 600, min = 72, max = 1200),
                      radioButtons("downloadPlot_res_conc_in_heart_device", "Format", choices = c("png","pdf","jpeg","eps"), selected = "png", inline = TRUE),
                      downloadButton("downloadPlot_res_conc_in_heart", "Download plot"),
                      tags$hr(),
                      conditionalPanel(condition = "input.METAB_present==true",
                        tags$h4("Metabolite conc plot settings in heart"),
                        numericInput("downloadPlot_res_conc_metab_in_heart_width", "Width [mm]", value = 120, min = 10, max = 1200),
                        numericInput("downloadPlot_res_conc_metab_in_heart_height", "Height [mm]", value = 90, min = 30, max = 900),
                        numericInput("downloadPlot_res_conc_metab_in_heart_dpi", "Resolution [dpi]", value = 600, min = 72, max = 1200),
                        radioButtons("downloadPlot_res_conc_metab_in_heart_device", "Format", choices = c("png","pdf","jpeg","eps"), selected = "png", inline = TRUE),
                        downloadButton("downloadPlot_res_conc_metab_in_heart", "Download plot"),
                        tags$hr(),
                        tags$h4("Stats metabolite conc plot settings in heart"),
                        sliderInput(inputId = "downloadPlot_stats_res_conc_metab_in_heart_CI",
                                    label = "Confidence interval [%]:",
                                    value = 95,
                                    min = 0,
                                    max = 100),
                        numericInput("downloadPlot_stats_res_conc_metab_in_heart_width", "Width [mm]", value = 120, min = 10, max = 1200),
                        numericInput("downloadPlot_stats_res_conc_metab_in_heart_height", "Height [mm]", value = 90, min = 30, max = 900),
                        numericInput("downloadPlot_stats_res_conc_metab_in_heart_dpi", "Resolution [dpi]", value = 600, min = 72, max = 1200),
                        radioButtons("downloadPlot_stats_res_conc_metab_in_heart_device", "Format", choices = c("png","pdf","jpeg","eps"), selected = "png", inline = TRUE),
                        downloadButton("downloadPlot_stats_res_conc_metab_in_heart", "Download plot")
                      ),
                      tags$hr(),
                      tags$h4("Stats plot settings conc in plasma"),
                      sliderInput(inputId = "downloadPlot_stats_res_conc_in_venous_plasma_CI",
                                  label = "Confidence interval [%]:",
                                  value = 95,
                                  min = 0,
                                  max = 100),
                      numericInput("downloadPlot_stats_res_conc_in_venous_plasma_width", "Width [mm]", value = 120, min = 10, max = 1200),
                      numericInput("downloadPlot_stats_res_conc_in_venous_plasma_height", "Height [mm]", value = 90, min = 30, max = 900),
                      numericInput("downloadPlot_stats_res_conc_in_venous_plasma_dpi", "Resolution [dpi]", value = 600, min = 72, max = 1200),
                      radioButtons("downloadPlot_stats_res_conc_in_venous_plasma_device", "Format", choices = c("png","pdf","jpeg","eps"), selected = "png", inline = TRUE),
                      downloadButton("downloadPlot_stats_res_conc_in_venous_plasma", "Download plot"),
                      tags$hr(),
                      tags$h4("Stats plot settings log 10 conc in plasma"),
                      sliderInput(inputId = "downloadPlot_stats_res_log_conc_in_venous_plasma_CI",
                                  label = "Confidence interval [%]:",
                                  value = 95,
                                  min = 0,
                                  max = 100),
                      numericInput("downloadPlot_stats_res_log_conc_in_venous_plasma_width", "Width [mm]", value = 120, min = 10, max = 1200),
                      numericInput("downloadPlot_stats_res_log_conc_in_venous_plasma_height", "Height [mm]", value = 90, min = 30, max = 900),
                      numericInput("downloadPlot_stats_res_log_conc_in_venous_plasma_dpi", "Resolution [dpi]", value = 600, min = 72, max = 1200),
                      radioButtons("downloadPlot_stats_res_log_conc_in_venous_plasma_device", "Format", choices = c("png","pdf","jpeg","eps"), selected = "png", inline = TRUE),
                      downloadButton("downloadPlot_stats_res_log_conc_in_venous_plasma", "Download plot"),
                      tags$hr(),
                      tags$h4("Stats plot settings conc in heart"),
                      sliderInput(inputId = "downloadPlot_stats_res_conc_in_heart_CI",
                                  label = "Confidence interval [%]:",
                                  value = 95,
                                  min = 0,
                                  max = 100),
                      numericInput("downloadPlot_stats_res_conc_in_heart_width", "Width [mm]", value = 120, min = 10, max = 1200),
                      numericInput("downloadPlot_stats_res_conc_in_heart_height", "Height [mm]", value = 90, min = 30, max = 900),
                      numericInput("downloadPlot_stats_res_conc_in_heart_dpi", "Resolution [dpi]", value = 600, min = 72, max = 1200),
                      radioButtons("downloadPlot_stats_res_conc_in_heart_device", "Format", choices = c("png","pdf","jpeg","eps"), selected = "png", inline = TRUE),
                      downloadButton("downloadPlot_stats_res_conc_in_heart", "Download plot"),
                      
                      
                      
                      bsTooltip("downloadPlot_stats_res_conc_in_venous_plasma_CI", "confidence interval, default = 95%",
                                placement = "bottom", trigger = "hover",
                                options = NULL),
                      bsTooltip("downloadPlot_stats_res_log_conc_in_venous_plasma_CI", "confidence interval, default = 95%",
                                placement = "bottom", trigger = "hover",
                                options = NULL)
                      
                      
             ),

               mainPanel(
                      tags$h4("Concentration of API plots"),
                      plotOutput("res_log_conc_in_venous_plasma"),
                      tags$br(),
                      plotOutput("res_conc_in_venous_plasma"),
                      tags$br(),
                      plotOutput("res_conc_in_heart"),
                      conditionalPanel(condition = "input.METAB_present==true",
                                      tags$br(),
                                      tags$h4("Concentration of metabolite plots"),
                                      plotOutput("res_conc_metab_in_heart"),
                                      tags$br(),
                                      plotOutput("res_conc_metab_in_venous_plasma")
                                      ),
                      tags$br(),
                      busyIndicator(),
                      tags$h4("Results data table"),
                      DT::dataTableOutput("res_datatable"),
                      busyIndicator(),
                      tags$br(),
                      tags$h4("Stats plots of API concentration in plasma"),
                      plotOutput("stats_res_conc_in_venous_plasma"),
                      busyIndicator(),
                      tags$br(),
                      tags$h4("Stats plots of API log 10 concentration in plasma"),
                      plotOutput("stats_res_log_conc_in_venous_plasma"),
                      busyIndicator(),
                      tags$br(),
                      tags$h4("Stats plots of API concentration in heart"),
                      plotOutput("stats_res_conc_in_heart"),
                      conditionalPanel(condition = "input.METAB_present==true",
                                       tags$br(),
                                       tags$h4("Stats plots of metabolite concentration in venous plasma"),
                                       plotOutput("stats_res_conc_metab_in_venous_plasma"),
                                       tags$br(),
                                       tags$h4("Stats plots of metabolite concentration in heart"),
                                       plotOutput("stats_res_conc_metab_in_heart")
                      )
                      
                      
                )
             
              )
           ),
  
  tabPanel("About",
           pre(includeText("README.TXT"))
           ),
  
  mainPanel(
    busyIndicator()
  )
  
  )
  


# Define server logic required to draw a histogram
server <- function(input, output, session) {

  #
  # Population tab logic (start) -----------------------------------------------
  #

  # Object to store reactive values for population
  
  pop_vals <- reactiveValues()
  
  observe({

    # observe reactive values of tab Population

    pop_vals$min_age <- input$age_range[1]
    pop_vals$max_age <- input$age_range[2]

    #### Checking population values with conditions
    
    if((typeof(input$seed) != "integer")) {
      ## Conditions
      createAlert(session, "seed_alert", "exampleAlert1",
                  content = "Seed must be integer", append = FALSE)
      pop_vals$seed <- NULL
    } else {
      closeAlert(session, "exampleAlert1")
      pop_vals$seed <- input$seed
    }
    
    if(!is.numeric(input$scale_M) | input$scale_M <= 0) {
      ## Conditions
      createAlert(session, "scale_M_alert", "exampleAlert2",
                  content = "male's scale must be numeric > 0", append = FALSE)
      pop_vals$scale_M <- NULL
    } else {
      closeAlert(session, "exampleAlert2")
      pop_vals$scale_M <- input$scale_M
    }
    
    if(!is.numeric(input$shape_M) | input$shape_M <= 0) {
      ## Conditions
      createAlert(session, "shape_M_alert", "exampleAlert3",
                  content = "male's shape must be numeric > 0", append = FALSE)
      pop_vals$shape_M <- NULL
    } else {
      closeAlert(session, "exampleAlert3")
      pop_vals$shape_M <- input$shape_M
    }
    
    if(!is.numeric(input$scale_F) | input$scale_F <= 0) {
      ## Conditions
      createAlert(session, "scale_F_alert", "exampleAlert4",
                  content = "female's scale must be numeric > 0", append = FALSE)
      pop_vals$scale_F <- NULL
    } else {
      closeAlert(session, "exampleAlert4")
      pop_vals$scale_F <- input$scale_F
    }
    
    if(!is.numeric(input$shape_F) | input$shape_F <= 0) {
      ## Conditions
      createAlert(session, "shape_F_alert", "exampleAlert5",
                  content = "female's shape must be numeric > 0", append = FALSE)
      pop_vals$shape_F <- NULL
    } else {
      closeAlert(session, "exampleAlert5")
      pop_vals$shape_F <- input$shape_F
    }
    
    if(!is.numeric(input$individual_count) | input$individual_count < 1) {
      ## Conditions
      createAlert(session, "individual_count_alert", "exampleAlert6",
                  content = "Individual count must be numeric > 1", append = FALSE)
      pop_vals$individual_count <- NULL
      pop_vals$males_count <- NULL
      pop_vals$females_count <- NULL
      
    } else {
      closeAlert(session, "exampleAlert6")
      pop_vals$individual_count <- input$individual_count
      pop_vals$female_count <- round(input$female_count / 100 * input$individual_count)
      pop_vals$males_count <- input$individual_count - pop_vals$female_count

    }
    
    
    if(!is.numeric(input$ka) | input$ka <= 0) {
      ## Conditions
      createAlert(session, "ka_alert", "exampleAlert7",
                  content = "ka must be numeric > 0", append = FALSE)
      pop_vals$ka <- NULL
      
    } else {
      closeAlert(session, "exampleAlert7")
      pop_vals$ka <- input$ka
      
    }
    
    
    if(!is.numeric(input$tlag_m_lognorm_m) | input$tlag_m_lognorm_m <= 0) {
      ## Conditions
      createAlert(session, "tlag_m_lognorm_m_alert", "exampleAlert8",
                  content = "Lag time mean must be numeric => 0", append = FALSE)
      pop_vals$tlag_m_lognorm_m <- NULL
      
    } else {
      closeAlert(session, "exampleAlert8")
      pop_vals$tlag_m_lognorm_m <- input$tlag_m_lognorm_m
      
    }
    
    
    if(!is.numeric(input$tlag_m_lognorm_cv) | input$tlag_m_lognorm_cv <= 0) {
      ## Conditions
      createAlert(session, "tlag_m_lognorm_cv_alert", "exampleAlert9",
                  content = "Lag time cv must be numeric => 0", append = FALSE)
      pop_vals$tlag_m_lognorm_cv <- NULL
      
    } else {
      closeAlert(session, "exampleAlert9")
      pop_vals$tlag_m_lognorm_cv <- input$tlag_m_lognorm_cv
      
    }
    
    
    if(!is.numeric(input$F_mean) | input$F_mean <= 0 | input$F_mean > 100) {
      ## Conditions
      createAlert(session, "F_mean_alert", "exampleAlert10",
                  content = "F mean must be numeric => 0 and < 100", append = FALSE)
      pop_vals$F_mean <- NULL
      
    } else {
      closeAlert(session, "exampleAlert10")
      pop_vals$F_mean <- input$F_mean
      
    }
    
    
    if(!is.numeric(input$F_sd) | input$F_sd <= 0) {
      ## Conditions
      createAlert(session, "F_sd_alert", "exampleAlert11",
                  content = "F sd must be numeric => 0", append = FALSE)
      pop_vals$F_sd <- NULL
      
    } else {
      closeAlert(session, "exampleAlert11")
      pop_vals$F_sd <- input$F_sd
      
    }
    
    
    if(!is.numeric(input$F_lower) | input$F_lower > input$F_mean) {
      ## Conditions
      createAlert(session, "F_lower_alert", "exampleAlert12",
                  content = "F lower must be numeric < F mean", append = FALSE)
      pop_vals$F_lower <- NULL
      
    } else {
      closeAlert(session, "exampleAlert12")
      pop_vals$F_lower <- input$F_lower
      
    }
    
    if(!is.numeric(input$F_upper) | input$F_upper < input$F_mean) {
      ## Conditions
      createAlert(session, "F_upper_alert", "exampleAlert13",
                  content = "F upper must be numeric > F mean", append = FALSE)
      pop_vals$F_upper <- NULL
      
    } else {
      closeAlert(session, "exampleAlert13")
      pop_vals$F_upper <- input$F_upper
      
    }
    
    
    if(!is.numeric(input$FaFg_m_lognorm_m) | (input$FaFg_m_lognorm_m <= 0) | (input$FaFg_m_lognorm_m > 1)){
      ## Conditions
      createAlert(session, "FaFg_m_lognorm_m_alert", "exampleAlert14",
                  content = "FaFg mean must be numeric > 0 and <= 1", append = FALSE)
      pop_vals$FaFg_m_lognorm_m <- NULL
      
    } else {
      closeAlert(session, "exampleAlert14")
      pop_vals$FaFg_m_lognorm_m <- input$FaFg_m_lognorm_m
      
    }
    
    
    if(!is.numeric(input$FaFg_m_lognorm_cv) | (input$FaFg_m_lognorm_cv < 0)){
      ## Conditions
      createAlert(session, "FaFg_m_lognorm_cv_alert", "exampleAlert15",
                  content = "FaFg cv must be numeric >= 0 ", append = FALSE)
      pop_vals$FaFg_m_lognorm_cv <- NULL
      
    } else {
      closeAlert(session, "exampleAlert15")
      pop_vals$FaFg_m_lognorm_cv <- input$FaFg_m_lognorm_cv
      
    }
    
    
    if(!is.numeric(input$fup_m_lognorm_2_m) | (input$fup_m_lognorm_2_m <= 0) | (input$fup_m_lognorm_2_m > 1)){
      ## Conditions
      createAlert(session, "fup_m_lognorm_2_m_alert", "exampleAlert16",
                  content = "fup mean must be numeric > 0 and <= 1", append = FALSE)
      pop_vals$fup_m_lognorm_2_m <- NULL
      
    } else {
      closeAlert(session, "exampleAlert16")
      pop_vals$fup_m_lognorm_2_m <- input$fup_m_lognorm_2_m
      
    }
    
    
    if(!is.numeric(input$fup_m_lognorm_2_cv) | (input$fup_m_lognorm_2_cv < 0)){
      ## Conditions
      createAlert(session, "fup_m_lognorm_2_cv_alert", "exampleAlert17",
                  content = "fup cv must be numeric >= 0", append = FALSE)
      pop_vals$fup_m_lognorm_2_cv <- NULL
      
    } else {
      closeAlert(session, "exampleAlert17")
      pop_vals$fup_m_lognorm_2_cv <- input$fup_m_lognorm_2_cv
      
    }
    
    if(!is.numeric(input$BP_m_lognorm_2_m) | (input$BP_m_lognorm_2_m <= 0)){
      ## Conditions
      createAlert(session, "BP_m_lognorm_2_m_alert", "exampleAlert18",
                  content = "BP mean must be numeric > 0", append = FALSE)
      pop_vals$BP_m_lognorm_2_m <- NULL
      
    } else {
      closeAlert(session, "exampleAlert18")
      pop_vals$BP_m_lognorm_2_m <- input$BP_m_lognorm_2_m
      
    }
    
    if(!is.numeric(input$BP_m_lognorm_2_cv) | (input$BP_m_lognorm_2_cv < 0)){
      ## Conditions
      createAlert(session, "BP_m_lognorm_2_cv_alert", "exampleAlert19",
                  content = "BP cv must be numeric >= 0", append = FALSE)
      pop_vals$BP_m_lognorm_2_cv <- NULL
      
    } else {
      closeAlert(session, "exampleAlert19")
      pop_vals$BP_m_lognorm_2_cv <- input$BP_m_lognorm_2_cv
      
    }
    
    
    if(!is.numeric(input$BP_metab_m_lognorm_2_m) | (input$BP_metab_m_lognorm_2_m <= 0)){
      ## Conditions
      createAlert(session, "BP_metab_m_lognorm_2_m_alert", "exampleAlert20",
                  content = "BP metabolite mean must be numeric > 0", append = FALSE)
      pop_vals$BP_metab_m_lognorm_2_m <- NULL
      
    } else {
      closeAlert(session, "exampleAlert20")
      pop_vals$BP_metab_m_lognorm_2_m <- input$BP_metab_m_lognorm_2_m
      
    }
    
    
    if(!is.numeric(input$BP_metab_m_lognorm_2_cv) | (input$BP_metab_m_lognorm_2_cv <= 0)){
      ## Conditions
      createAlert(session, "BP_metab_m_lognorm_2_cv_alert", "exampleAlert21",
                  content = "BP metabolite cv must be numeric > 0", append = FALSE)
      pop_vals$BP_metab_m_lognorm_2_cv <- NULL
      
    } else {
      closeAlert(session, "exampleAlert21")
      pop_vals$BP_metab_m_lognorm_2_cv <- input$BP_metab_m_lognorm_2_cv
      
    }
    
    
    if(!is.numeric(input$fup_metab_1) | (input$fup_metab_1 <= 0) | (input$fup_metab_1 > 1)){
      ## Conditions
      createAlert(session, "fup_m_lognorm_2_m_alert", "exampleAlert22",
                  content = "fup mean must be numeric > 0 and <= 1", append = FALSE)
      pop_vals$fup_metab_1 <- NULL
      
    } else {
      closeAlert(session, "exampleAlert22")
      pop_vals$fup_metab_1 <- input$fup_metab_1
      
    }
    
    
    if(!is.numeric(input$fup_metab_2) | (input$fup_metab_2 < 0)){
      ## Conditions
      createAlert(session, "fup_metab_2_alert", "exampleAlert23",
                  content = "fup cv must be numeric >= 0", append = FALSE)
      pop_vals$fup_metab_2 <- NULL
      
    } else {
      closeAlert(session, "exampleAlert23")
      pop_vals$fup_metab_2 <- input$fup_metab_2
      
    }
    
    
  })    
  
  a <- c(0.2434986, 1.3321865) #Olek 16.02.2018
  
  M_age <- reactive({

    #M_age require
    req(pop_vals$seed, pop_vals$shape_M, pop_vals$scale_M, pop_vals$shape_F, pop_vals$scale_F, pop_vals$males_count)
    
    set.seed(pop_vals$seed)
    
    if (pop_vals$min_age != pop_vals$max_age ) {
      d_age_M <-
        Weibull(shape = pop_vals$shape_M, scale = pop_vals$scale_M) #distribution of males age
      d_age_M_truncate <-
        Truncate(d_age_M, lower = pop_vals$min_age, upper = pop_vals$max_age)
      assign('M_age', round(d_age_M_truncate@r(pop_vals$males_count)))
    } else {
      assign('M_age', rep(pop_vals$min_age, pop_vals$males_count ))
    }
  })
  
  F_age <- reactive({
    
    # F_age require
    req(pop_vals$seed,
        pop_vals$shape_F,
        pop_vals$scale_F
        )
    
    set.seed(pop_vals$seed)
    
    if (pop_vals$min_age != pop_vals$max_age) {
      d_age_M <-
        Weibull(shape = pop_vals$shape_M, scale = pop_vals$scale_M) #distribution of males age
      d_age_F <-
        Weibull(shape = pop_vals$shape_F, scale = pop_vals$scale_F) #distribution of females age
      d_age_F_truncate <-
        Truncate(d_age_F, lower = pop_vals$min_age, upper = pop_vals$max_age)
      assign('F_age', round(d_age_F_truncate@r(pop_vals$female_count )))
    } else {
      assign('F_age', rep(pop_vals$min_age, pop_vals$female_count))
    }
  })
  
  M_height <- reactive({
    
    # M_height require
    req(pop_vals$seed)
    
    set.seed(pop_vals$seed)
    category_M_height <- function(x) {
      if (x <= 29)
        round(rnorm(1, 175.8, 6.8), digits = 1)
      else if (x >= 30 &
               x <= 39)
        round(rnorm(1, 174.8, 6.7), digits = 1)
      else if (x >= 40 &
               x <= 49)
        round(rnorm(1, 173.6, 7.4), digits = 1)
      else if (x >= 50 &
               x <= 59)
        round(rnorm(1, 172.9, 6.4), digits = 1)
      else if (x >= 60)
        round(rnorm(1, 171.2, 6.4), digits = 1)
      else
        round(rnorm(1, 174.1, 7), digits = 1)
    }
    assign('M_height', as.numeric(lapply(M_age(), category_M_height)))
  })
  
  F_height <- reactive({
    
    # F_height require
    req(pop_vals$seed)
    
    set.seed(pop_vals$seed)
    category_F_height <- function(x)
    {
      if (x <= 29)
        round(rnorm(1, 162, 6.4), digits = 1)
      else if (x >= 30 &
               x <= 39)
        round(rnorm(1, 161.4, 6.3), digits = 1)
      else if (x >= 40 &
               x <= 49)
        round(rnorm(1, 160.9, 6.5), digits = 1)
      else if (x >= 50 &
               x <= 59)
        round(rnorm(1, 159.7, 6.4), digits = 1)
      else if (x >= 60)
        round(rnorm(1, 157.6, 6.4), digits = 1)
      else
        round(rnorm(1, 160.7, 6.6), digits = 1)
    }
    assign('F_height', as.numeric(lapply(F_age(), category_F_height)))
  })
  
  M_weight <- reactive({
    
    # M_weight require
    req(pop_vals$seed)
    
    set.seed(pop_vals$seed)
    category_M_weight <-
      function(x) {
        if (x <= 29)
          round(Truncate(Norm(73.9, 11.3), lower = 30)@r(1), digits = 1)
        else if (x >= 30 &
                 x <= 39)
          round(Truncate(Norm(77.1, 12.4), lower = 30)@r(1), digits = 1)
        else if (x >= 40 &
                 x <= 49)
          round(Truncate(Norm(78.8, 12.9), lower = 30)@r(1), digits = 1)
        else if (x >= 50 &
                 x <= 59)
          round(Truncate(Norm(78.3, 11.2), lower = 30)@r(1), digits = 1)
        else if (x >= 60)
          round(Truncate(Norm(78.1, 11.9), lower = 30)@r(1), digits = 1)
        else
          round(Truncate(Norm(76.7, 12.1), lower = 30)@r(1), digits = 1)
      }
    assign('M_weight', as.numeric(lapply(M_age(), category_M_weight)))
  })
  
  F_weight <- reactive({
    
    # F_weight require
    req(pop_vals$seed)
    
    set.seed(pop_vals$seed)
    category_F_weight <-
      function(x) {
        if (x <= 29)
          round(Truncate(Norm(57.2, 8.8), lower = 30)@r(1), digits = 1)
        else if (x >= 30 &
                 x <= 39)
          round(Truncate(Norm(61.2, 11.3), lower = 30)@r(1), digits = 1)
        else if (x >= 40 &
                 x <= 49)
          round(Truncate(Norm(63.7, 11.9), lower = 30)@r(1), digits = 1)
        else if (x >= 50 &
                 x <= 59)
          round(Truncate(Norm(65.2, 11.2), lower = 30)@r(1), digits = 1)
        else if (x >= 60)
          round(Truncate(Norm(64, 10.8), lower = 30)@r(1), digits = 1)
        else
          round(Truncate(Norm(61.5, 11.1), lower = 30)@r(1), digits = 1)
      }
    assign('F_weight', as.numeric(lapply(F_age(), category_F_weight)))
  })
  
  BSA <- reactive({
    ( c(M_weight(), F_weight()) ^ 0.425 * c(M_height(), F_height()) ^ 0.725 ) * 0.007184 #Body surface area according to [DuBois-DuBois 1916]
  })
  
  
  CO <- reactive({
    
    # CO require
    req(pop_vals$males_count,
        pop_vals$female_count,
        pop_vals$individual_count
        )
    
    if (pop_vals$female_count==0)
      (1.1 * BSA() - 0.05 * M_age() + 5.5) * 60
    else if (pop_vals$males_count == 0)
      (1.7 * BSA() - 0.04 * F_age() + 3.5) * 60
    else
      (c(1.1 * BSA()[1:length(M_weight())] - 0.05 * M_age() + 5.5, 
         1.7 * BSA()[(length(M_weight())+1):pop_vals$individual_count] - 0.04 * F_age() + 3.5)) * 60 
  })
  
  Qad <- reactive({
    # Qad require
    req(pop_vals$males_count,
        pop_vals$female_count,
        pop_vals$individual_count
    )
    
    if (pop_vals$female_count==0)
      CO() * 0.05
    else if (pop_vals$males_count == 0)
      CO() * 0.085
    else
      (c(CO()[1:length(M_weight())] * 0.05 , 
         CO()[(length(M_weight())+1):pop_vals$individual_count] * 0.085)) 
  })
  
  Qbo <- reactive({
    CO() * 0.05
  })
  
  Qbr <- reactive({
    CO() * 0.12
  })
  
  Qgu <- reactive({
    # Qgu require
    req(pop_vals$males_count,
        pop_vals$female_count,
        pop_vals$individual_count
    )
    
    if (pop_vals$female_count == 0)
      CO() * 0.16
    else if (pop_vals$males_count == 0)
      CO() * 0.17
    else
      (c(CO()[1:length(M_weight())] * 0.16 , 
         CO()[(length(M_weight())+1):pop_vals$individual_count] * 0.17)) 
  })
  
  Qhe <- reactive({
    # Qhe require
    req(pop_vals$males_count,
        pop_vals$female_count,
        pop_vals$individual_count
    )
    
    if (pop_vals$female_count==0)
      CO() * 0.04
    else if (pop_vals$males_count == 0)
      CO() * 0.05
    else
      (c(CO()[1:length(M_weight())] * 0.04, 
         CO()[(length(M_weight())+1):pop_vals$individual_count] * 0.05)) 
  })
  
  Qki <- reactive({
    # Qki require
    req(pop_vals$males_count,
        pop_vals$female_count,
        pop_vals$individual_count
    )
    
    if (pop_vals$female_count==0)
      CO() * 0.19
    else if (pop_vals$males_count == 0)
      CO() * 0.17
    else
      (c(CO()[1:length(M_weight())] * 0.19, 
         CO()[(length(M_weight())+1):pop_vals$individual_count] * 0.17)) 
  })
  
  Qh <- reactive({
    # Qh require
    req(pop_vals$males_count,
        pop_vals$female_count,
        pop_vals$individual_count
    )
    
    if (pop_vals$female_count==0)
      CO() * 0.19
    else if (pop_vals$males_count == 0)
      CO() * 0.215
    else
      (c(CO()[1:length(M_weight())] * 0.19, 
         CO()[(length(M_weight())+1):pop_vals$individual_count] * 0.215)) 
  })
  
  Qlu <- reactive({
    CO()
  })
  
  Qmu <- reactive({
    # Qmu require
    req(pop_vals$males_count,
        pop_vals$female_count,
        pop_vals$individual_count
    )
    
    if (pop_vals$female_count==0)
      CO() * 0.17
    else if (pop_vals$males_count == 0)
      CO() * 0.12
    else
      (c(CO()[1:length(M_weight())] * 0.17, 
         CO()[(length(M_weight())+1):pop_vals$individual_count] * 0.12)) 
  })
  
  Qsk <- reactive({
    CO() * 0.05
  })
  
  Qsp <- reactive({
    # Qsp require
    req(pop_vals$males_count,
        pop_vals$female_count,
        pop_vals$individual_count
    )
    
    if (pop_vals$female_count==0)
      CO() * 0.02
    else if (pop_vals$males_count == 0)
      CO() * 0.03
    else
      (c(CO()[1:length(M_weight())] * 0.02, 
         CO()[(length(M_weight())+1):pop_vals$individual_count] * 0.03)) 
  })
  
  Qre <- reactive({
    # Qre require
    req(pop_vals$males_count,
        pop_vals$female_count,
        pop_vals$individual_count
    )
    
    if (pop_vals$female_count==0)
      CO() * 0.14
    else if (pop_vals$males_count == 0)
      CO() * 0.14
    else
      (c(CO()[1:length(M_weight())] * 0.14, 
         CO()[(length(M_weight())+1):pop_vals$individual_count] * 0.14))
  })
  
  
  BW <- reactive({
    c(M_weight(), F_weight())
  })
  
  Vad <- reactive({
    0.259 * BW() / 0.923
  })
  
  Vbo <- reactive({
    0.090 * BW() / 1.850
  })
  
  Vbr <- reactive({
    0.017 * BW() / 1.04
  })
  
  Vgu <- reactive({
    0.016 * BW() / 1.04
  })
  
  Vhe <- reactive({
    0.005 * BW() / 1.04
  })
  
  Vki <- reactive({
    0.004 * BW() / 1.05
  })
  
  Vli <- reactive({
    0.022 * BW() / 1.08
  })
  
  Vlu <- reactive({
    0.007 * BW() / 1.05
  })
  
  Vmu <- reactive({
    0.403 * BW() / 1.04
  })
  
  Vsk <- reactive({
    0.043 * BW() / 1.1
  })
  
  Vsp <- reactive({
    0.002 * BW() / 1.06
  })
  
  Vre <- reactive({
    0.057 * BW() / 1.05
  })
  
  Vpl <- reactive({
    0.044 * BW() / 1.025
  })
  
  Vrb <- reactive({
    0.031 * BW() / 1.125
  })
  
  Vbl <- reactive({
    Vpl() + Vrb()
  })
  
  mean_MPPGL <- reactive({
    10 ^ (
      1.407 + 0.0158 * c(M_age(), F_age()) - 0.00038 * (c(M_age(), F_age()) ^ 2) + 0.0000024 *
        (c(M_age(), F_age()) ^ 3)
    )
  })
  
  MPPGL <- reactive({
    # MPPGL require
    req(pop_vals$seed)
    
    set.seed(pop_vals$seed)
    
    RANDOM_MPPGL <- function(x) {
      Lnorm(m_lognormal(x, 0.269), s_lognormal(x, 0.269))@r(1)
    }  #CV=26.9%
    as.numeric(lapply(mean_MPPGL(), RANDOM_MPPGL))
  })
  
  CYP2C8_H <- reactive({
    # CYP require
    req(pop_vals$seed,
        pop_vals$individual_count
        )
    
    set.seed(pop_vals$seed)
    cbind(Lnorm(m_lognormal_2(0.2, 0.02), s_lognormal_2(0.2, 0.02))@r(pop_vals$individual_count))
  })
  
  CYP2C9_H <- reactive({
    # CYP require
    req(pop_vals$seed,
        pop_vals$individual_count
    )
    
    set.seed(pop_vals$seed)
    cbind(Lnorm(m_lognormal(5.5, 0.3), s_lognormal(5.5, 0.3))@r(pop_vals$individual_count)) #CV=30% assumption
  })
  
  CYP2J2_H <- reactive({
    # CYP require
    req(pop_vals$seed,
        pop_vals$individual_count
    )
    
    set.seed(pop_vals$seed)
    cbind(Lnorm(m_lognormal_2(0.17, 0.05), s_lognormal_2(0.17, 0.05))@r(pop_vals$individual_count)) #CV=30% assumption
  })

  CYP1A2_L <- reactive({
    # CYP require
    req(pop_vals$seed,
        pop_vals$individual_count
    )
    
    set.seed(pop_vals$seed)
    cbind(Lnorm(m_lognormal(52, 0.67), s_lognormal(52, 0.67))@r(pop_vals$individual_count))
  })
  
  CYP2B6_L <- reactive({
    # CYP require
    req(pop_vals$seed,
        pop_vals$individual_count
    )
    
    set.seed(pop_vals$seed)
    cbind(Lnorm(m_lognormal(17, 1.22), s_lognormal(17, 1.22))@r(pop_vals$individual_count))
  })
  
  CYP2C8_L <- reactive({
    # CYP require
    req(pop_vals$seed,
        pop_vals$individual_count
    )
    
    set.seed(pop_vals$seed)
    cbind(Lnorm(m_lognormal(24, 0.81), s_lognormal(24, 0.81))@r(pop_vals$individual_count))
  })
  
  CYP2C9_L <- reactive({
    # CYP require
    req(pop_vals$seed,
        pop_vals$individual_count
    )
    
    set.seed(pop_vals$seed)
    cbind(Lnorm(m_lognormal(73, 0.54), s_lognormal(73, 0.54))@r(pop_vals$individual_count))
  })
  
  CYP2C19_L <- reactive({
    # CYP require
    req(pop_vals$seed,
        pop_vals$individual_count
    )
    
    set.seed(pop_vals$seed)
    cbind(Lnorm(m_lognormal(14, 1.06), s_lognormal(14, 1.06))@r(pop_vals$individual_count))
  })
  
  CYP2D6_L <- reactive({
    # CYP require
    req(pop_vals$seed,
        pop_vals$individual_count
    )
    
    set.seed(pop_vals$seed)
    cbind(Lnorm(m_lognormal(8, 0.61), s_lognormal(8, 0.61))@r(pop_vals$individual_count))
  })
  
  CYP3A4_L <- reactive({
    # CYP require
    req(pop_vals$seed,
        pop_vals$individual_count
    )
    
    set.seed(pop_vals$seed)
    cbind(Lnorm(m_lognormal(137, 0.41), s_lognormal(137, 0.41))@r(pop_vals$individual_count))
  })
  
  tlag <- reactive({
    # tlag require
    req(pop_vals$seed,
        pop_vals$individual_count,
        pop_vals$tlag_m_lognorm_m,
        pop_vals$tlag_m_lognorm_cv,
        pop_vals$tlag_m_lognorm_m,
        pop_vals$tlag_m_lognorm_cv
        )
    
    set.seed(pop_vals$seed)
    cbind(Lnorm(m_lognormal(pop_vals$tlag_m_lognorm_m, pop_vals$tlag_m_lognorm_cv), s_lognormal(pop_vals$tlag_m_lognorm_m, pop_vals$tlag_m_lognorm_cv))@r(pop_vals$individual_count)) #time of gastric emptying
  })

  Bioavailability <- reactive({
    # BA require
    req(
      pop_vals$seed,
      pop_vals$F_mean,
      pop_vals$F_sd,
      pop_vals$F_lower,
      pop_vals$F_upper,
      pop_vals$individual_count
      )
    
    set.seed(pop_vals$seed)
    cbind((Truncate(
      Norm(pop_vals$F_mean, pop_vals$F_sd), lower = pop_vals$F_lower, upper = pop_vals$F_upper
    )@r(pop_vals$individual_count)) / 100)
  })
  
  FaFg <- reactive({
    # FaFg require
    req(
      pop_vals$seed,
      pop_vals$FaFg_m_lognorm_m,
      pop_vals$FaFg_m_lognorm_cv,
      pop_vals$individual_count
      )
    
    set.seed(pop_vals$seed)
    cbind(Lnorm(m_lognormal(pop_vals$FaFg_m_lognorm_m, pop_vals$FaFg_m_lognorm_cv))@r(pop_vals$individual_count))
    #[h]
  })

  fup <- reactive({
    # fup require
    req(
      pop_vals$seed,
      pop_vals$fup_m_lognorm_2_m,
      pop_vals$fup_m_lognorm_2_cv,
      pop_vals$fup_m_lognorm_2_m, 
      pop_vals$fup_m_lognorm_2_cv,
      pop_vals$individual_count
    )
    
    set.seed(pop_vals$seed)
    cbind(Lnorm(m_lognormal_2(pop_vals$fup_m_lognorm_2_m, pop_vals$fup_m_lognorm_2_cv), s_lognormal_2(pop_vals$fup_m_lognorm_2_m, pop_vals$fup_m_lognorm_2_cv))@r(pop_vals$individual_count))
  })
  
  BP <- reactive({
    # BP require
    req(
      pop_vals$seed,
      pop_vals$BP_m_lognorm_2_m,
      pop_vals$BP_m_lognorm_2_cv,
      pop_vals$BP_m_lognorm_2_m,
      pop_vals$BP_m_lognorm_2_cv,
      pop_vals$individual_count
    )
    
    set.seed(pop_vals$seed)
    cbind(Lnorm(m_lognormal_2(pop_vals$BP_m_lognorm_2_m, pop_vals$BP_m_lognorm_2_cv), s_lognormal_2(pop_vals$BP_m_lognorm_2_m, pop_vals$BP_m_lognorm_2_cv))@r(pop_vals$individual_count))
  })
  
  BP_metab <- reactive({
    # BP_metab require
    req(
      pop_vals$seed,
      pop_vals$BP_metab_m_lognorm_2_m,
      pop_vals$BP_metab_m_lognorm_2_cv,
      pop_vals$BP_metab_m_lognorm_2_m,
      pop_vals$BP_metab_m_lognorm_2_cv,
      pop_vals$individual_count
    )
    set.seed(pop_vals$seed)
    cbind(Lnorm(m_lognormal_2(pop_vals$BP_metab_m_lognorm_2_m, pop_vals$BP_metab_m_lognorm_2_cv), s_lognormal_2(pop_vals$BP_metab_m_lognorm_2_m, pop_vals$BP_metab_m_lognorm_2_cv))@r(pop_vals$individual_count)) #distribution according to [Rollins 1980]
  })
  
  fup_metab <- reactive({
    # fup_metab require
    req(
      pop_vals$seed,
      pop_vals$fup_metab_1,
      pop_vals$fup_metab_2,
      pop_vals$individual_count
    )
    
    set.seed(pop_vals$seed)
    pop_vals$fup_metab_1 * fup() + pop_vals$fup_metab_2
  })
  
  MV_SE <- reactive({
    # MV_SE require
    req(pop_vals$seed)
    
    set.seed(pop_vals$seed)
    MV <- function(x) {
      #cr inline or give methods meaningful names
      sqrt(0.29602 ^ 2 + (c(1, x) %*% matrix(
        c(38.12228, -0.66728, -0.66728, 0.01639),
        nrow = 2,
        ncol = 2
      ) %*% t(t(c(
        1, x
      ))) / 1000))
    }
    cbind(mapply(MV, c(M_age(), F_age())))
  })
  
  MV <- reactive({
    # MV require
    req(pop_vals$seed,
        pop_vals$individual_count
        )
    
    set.seed(pop_vals$seed)
    cbind(exp(rnorm(
      pop_vals$individual_count,
      (c(M_age(), F_age()) * 0.04551 + 7.36346),
      MV_SE()
    )))
  })
  
  MSA <- reactive({
    # MSA require
    req(pop_vals$seed,
        pop_vals$individual_count
    )
    
    set.seed(pop_vals$seed)
    cbind(exp(rnorm(
      pop_vals$individual_count, 0.860 * log(MV()),
      ((sqrt(
        0.102 ^ 2 + (log(MV())) ^ 2 * 0.002939 ^ 2
      )))
    )))
  })
  
  SA_pf <- reactive({
    cbind(2 * (0.87 * c(M_height(), F_height()) + 0.34 * BW() - 63.8))
  })

  Population <- reactive({
    # Population require
    req(pop_vals$males_count,
        pop_vals$female_count,
        pop_vals$fup_m_lognorm_2_m,
        pop_vals$fup_m_lognorm_2_cv,
        pop_vals$BP_m_lognorm_2_m,
        pop_vals$BP_m_lognorm_2_cv,
        pop_vals$BP_metab_m_lognorm_2_m,
        pop_vals$BP_metab_m_lognorm_2_cv,
        pop_vals$fup_metab_1,
        pop_vals$fup_metab_2
        )

    data.frame(
      sex = c(rep("M", pop_vals$males_count ), rep("F", pop_vals$female_count)),
      age = c(M_age(), F_age()),
      height = c(M_height(), F_height()),
      weight = c(M_weight(), F_weight()),
      BSA = BSA(),
      "mean CO Tanner formula [L/h]" = CO(),
      'Qad' = Qad(),
      'Qbo' = Qbo(),
      'Qbr' = Qbr(),
      'Qgu' = Qgu(),
      'Qhe' = Qhe(),
      'Qki' = Qki(),
      'Qh' = Qh(),
      'Qlu' = Qlu(),
      'Qmu' = Qmu(),
      'Qsk' = Qsk(),
      'Qsp' = Qsp(),
      'Qre' = Qre(),
      "CO-Qsum" = round(CO()-(Qad() + Qbo() + Qbr() + Qki() + Qh() + Qmu() + Qsk() + Qre() + Qhe())),
      "Vad" = Vad(),
      "Vbo" = Vbo(),
      "Vbr" = Vbr(),
      "Vgu" = Vgu(),
      "Vhe" = Vhe(),
      "Vli" = Vli(),
      "Vlu" = Vlu(),
      "Vki" = Vki(),
      "Vmu" = Vmu(),
      "Vsk" = Vsk(),
      "Vsp" = Vsp(),
      "Vre" = Vre(),
      "Vpl" = Vpl(),
      "Vrb" = Vrb(),
      "Vbl" = Vbl(),
      "MPPGL" = MPPGL(),
      "CYP2C8_H" = CYP2C8_H(),
      "CYP2C9_H" = CYP2C9_H(),
      "CYP2J2_H" = CYP2J2_H(),
      "CYP1A2_L" = CYP1A2_L(),
      "CYP2B6_L" = CYP2B6_L(),
      "CYP2C8_L" = CYP2C8_L(),
      "CYP2C9_L" = CYP2C9_L(),
      "CYP2C19_L" = CYP2C19_L(),
      "CYP2D6_L" = CYP2D6_L(),
      "CYP3A4_L" = CYP3A4_L(),
      "tlag" = tlag(),
      "Bioavailability" = Bioavailability(),
      "FaFg" = FaFg(),
      "fup" = fup(),
      "BP" = BP(),
      "BP_metab" = BP_metab(),
      "fup_metab" = fup_metab(),
      "MV" = MV(),
      "MSA" = MSA(),
      "SA_pf" = SA_pf()
      
    )
  })
  
  Vpf <- reactive({
    # Vpf require
    req(pop_vals$seed,
        pop_vals$individual_count
    )
    
    set.seed(pop_vals$seed)
    sort((rnorm(pop_vals$individual_count, 0.03, (0.3 * 0.03))))
  })
  
  pop2 <- reactive({
    
    # pop2 require
    req(pop_vals$males_count,
        pop_vals$female_count,
        pop_vals$fup_m_lognorm_2_m,
        pop_vals$fup_m_lognorm_2_cv,
        pop_vals$BP_m_lognorm_2_m,
        pop_vals$BP_m_lognorm_2_cv,
        pop_vals$BP_metab_m_lognorm_2_m,
        pop_vals$BP_metab_m_lognorm_2_cv,
        pop_vals$fup_metab_1,
        pop_vals$fup_metab_2
    )

    cbind(
      Population()[order(Population()$weight),], Vpf=Vpf()
    )
  })
  
  
  pop3 <-reactive({
    a <-  setDT(pop2()[order(as.numeric(row.names(pop2()))),], #rearranging the table with virtual population data according to individual numbers AND format cols 6:56 to 4 digits without thousand mark
                  keep.rownames = TRUE # Population()
                  ) %>% datatable(options = list(
                    pageLength = 20,
                    lengthMenu = list(c(10, 15, -1), c('10', '15', 'All')),
                    scrollX = TRUE) ) %>% formatRound(columns=c(6:56), digits=4, mark = "")
    
    #### Check if FaFg is not smaller than Bioavialability, and substitute any FaFg < Bioav, by FaFg = Bioavail
    
    a$x$data$FaFg[which(a$x$data$Bioavailability > a$x$data$FaFg)] <- a$x$data$Bioavailability[which(a$x$data$Bioavailability > a$x$data$FaFg)]
    
    
    a$rn <- as.numeric(as.character(a$rn))
    return(a)
  })
  
  
  output$table_pop <- DT::renderDataTable( 
    pop3()
    ) 
    # 
  
  output$downloadData <- downloadHandler(
    filename = function(){
      paste("Population",input$seed, ".csv",sep=".")
    },content = function(file) {
      write.table(pop3()$x$data, row.names=input$pop_data_rownames, col.names=input$pop_data_colnames, sep=paste(input$pop_data_sep), file=file)
    },
    contentType="csv"
  )

  #
  # Population tab logic (end) -----------------------------------------------
  #
  
  #
  # Study tab logic (start) ----------------------------------------------------
  #
  
  study_vals <- reactiveValues()
  
  observe({

    #### Checking study values with conditions within observe({})
    
    if((!is.numeric(input$dose) | input$dose <= 0)) {
      ## Conditions
      createAlert(session, "dose_alert", "exampleAlert24",
                  content = "Dose must be > 0", append = FALSE)
      study_vals$dose <- NULL
    } else {
      closeAlert(session, "exampleAlert24")
      study_vals$dose <- input$dose
    }
    
    if((!is.numeric(input$no_doses) | input$no_doses < 1 | !is.integer(input$no_doses))) {
      ## Conditions
      createAlert(session, "no_doses_alert", "exampleAlert25",
                  content = "No of doses must be integer >= 1", append = FALSE)
      study_vals$no_doses <- NULL
    } else {
      closeAlert(session, "exampleAlert25")
      study_vals$no_doses <- input$no_doses
    }
    
    
    if((!is.numeric(input$dose_every) | input$dose_every <= 0 )) {
      ## Conditions
      createAlert(session, "dose_every_alert", "exampleAlert26",
                  content = "Dose interval must be > 0", append = FALSE)
      study_vals$dose_every <- NULL
    } else {
      closeAlert(session, "exampleAlert26")
      study_vals$dose_every <- input$dose_every
    }
    
    if((!is.numeric(input$inf_dose) | input$inf_dose < 0 )) {
      ## Conditions
      createAlert(session, "inf_dose_alert", "exampleAlert27",
                  content = "Intravenous dose must be => 0", append = FALSE)
      study_vals$inf_dose <- NULL
    } else {
      closeAlert(session, "exampleAlert27")
      study_vals$inf_dose <- input$inf_dose
    }

    if((!is.numeric(input$inf_time) | input$inf_time <= 0 )) {
      ## Conditions
      createAlert(session, "inf_time_alert", "exampleAlert28",
                  content = "Intravenous dose time must be > 0", append = FALSE)
      study_vals$inf_time <- NULL
    } else {
      closeAlert(session, "exampleAlert28")
      study_vals$inf_time <- input$inf_time
    }

    if((!is.numeric(input$t_end) | input$t_end <= 0 )) {
      ## Conditions
      createAlert(session, "t_end_alert", "exampleAlert29",
                  content = "Simulation time must be > 0", append = FALSE)
      study_vals$t_end <- NULL
    } else {
      closeAlert(session, "exampleAlert29")
      study_vals$t_end <- input$t_end
    }
    
    if((!is.numeric(input$ISEF1A2) | input$ISEF1A2 <= 0 )) {
      ## Conditions
      createAlert(session, "ISEF1A2_alert", "exampleAlert30",
                  content = "ISEF1A2 must be > 0", append = FALSE)
      study_vals$ISEF1A2 <- NULL
    } else {
      closeAlert(session, "exampleAlert30")
      study_vals$ISEF1A2 <- input$ISEF1A2
    }
    
    if((!is.numeric(input$ISEF2C19) | input$ISEF2C19 <= 0 )) {
      ## Conditions
      createAlert(session, "ISEF2C19_alert", "exampleAlert31",
                  content = "ISEF2C19 must be > 0", append = FALSE)
      study_vals$ISEF2C19 <- NULL
    } else {
      closeAlert(session, "exampleAlert31")
      study_vals$ISEF2C19 <- input$ISEF2C19
    }
    
    if((!is.numeric(input$ISEF2D6) | input$ISEF2D6 <= 0 )) {
      ## Conditions
      createAlert(session, "ISEF2D6_alert", "exampleAlert32",
                  content = "ISEF2D6 must be > 0", append = FALSE)
      study_vals$ISEF2D6 <- NULL
    } else {
      closeAlert(session, "exampleAlert32")
      study_vals$ISEF2D6 <- input$ISEF2D6
    }
    
    if((!is.numeric(input$ISEF2C9) | input$ISEF2C9 <= 0 )) {
      ## Conditions
      createAlert(session, "ISEF2C9_alert", "exampleAlert33",
                  content = "ISEF2C9 must be > 0", append = FALSE)
      study_vals$ISEF2C9 <- NULL
    } else {
      closeAlert(session, "exampleAlert33")
      study_vals$ISEF2C9 <- input$ISEF2C9
    }
    
    if((!is.numeric(input$ISEF3A4) | input$ISEF3A4 <= 0 )) {
      ## Conditions
      createAlert(session, "ISEF3A4_alert", "exampleAlert34",
                  content = "ISEF3A4 must be > 0", append = FALSE)
      study_vals$ISEF3A4 <- NULL
    } else {
      closeAlert(session, "exampleAlert34")
      study_vals$ISEF3A4 <- input$ISEF3A4
    }
    
    if((!is.numeric(input$ISEF2B6) | input$ISEF2B6 <= 0 )) {
      ## Conditions
      createAlert(session, "ISEF2B6_alert", "exampleAlert35",
                  content = "ISEF2B6 must be > 0", append = FALSE)
      study_vals$ISEF2B6 <- NULL
    } else {
      closeAlert(session, "exampleAlert35")
      study_vals$ISEF2B6 <- input$ISEF2B6
    }
    
    if((!is.numeric(input$ISEF2C8) | input$ISEF2C8 <= 0 )) {
      ## Conditions
      createAlert(session, "ISEF2C8_alert", "exampleAlert36",
                  content = "ISEF2C8 must be > 0", append = FALSE)
      study_vals$ISEF2C8 <- NULL
    } else {
      closeAlert(session, "exampleAlert36")
      study_vals$ISEF2C8 <- input$ISEF2C8
    }
    
    if((!is.numeric(input$fumic) | input$fumic <= 0 | input$fumic > 1)) {
      ## Conditions
      createAlert(session, "fumic_alert", "exampleAlert37",
                  content = "fumic must be > 0 and <= 1", append = FALSE)
      study_vals$fumic <- NULL
    } else {
      closeAlert(session, "exampleAlert37")
      study_vals$fumic <- input$fumic
    }
    
    if((!is.numeric(input$fumic_metab) | input$fumic_metab <= 0 | input$fumic_metab > 1)) {
      ## Conditions
      createAlert(session, "fumic_metab_alert", "exampleAlert38",
                  content = "fumic must be > 0 and < 1", append = FALSE)
      study_vals$fumic_metab <- NULL
    } else {
      closeAlert(session, "exampleAlert38")
      study_vals$fumic_metab <- input$fumic_metab
    }
    
    if((!is.numeric(input$CLrenal) | input$CLrenal < 0)) {
      ## Conditions
      createAlert(session, "CLrenal_alert", "exampleAlert39",
                  content = "CLrenal must be => 0", append = FALSE)
      study_vals$CLrenal <- NULL
    } else {
      closeAlert(session, "exampleAlert39")
      study_vals$CLrenal <- input$CLrenal
    }
    
    if((!is.numeric(input$CLrenal_metab) | input$CLrenal_metab < 0)) {
      ## Conditions
      createAlert(session, "CLrenal_metab_alert", "exampleAlert40",
                  content = "CLrenal_metab must be => 0", append = FALSE)
      study_vals$CLrenal_metab <- NULL
    } else {
      closeAlert(session, "exampleAlert40")
      study_vals$CLrenal_metab <- input$CLrenal_metab
    }
    
    if((!is.numeric(input$liver_density) | input$liver_density <= 0)) {
      ## Conditions
      createAlert(session, "liver_density_alert", "exampleAlert41",
                  content = "Liver density must be > 0", append = FALSE)
      study_vals$liver_density <- NULL
    } else {
      closeAlert(session, "exampleAlert41")
      study_vals$liver_density <- input$liver_density
    }
    
    if((!is.numeric(input$heart_density) | input$heart_density <= 0)) {
      ## Conditions
      createAlert(session, "heart_density_alert", "exampleAlert42",
                  content = "Heart density must be > 0", append = FALSE)
      study_vals$heart_density <- NULL
    } else {
      closeAlert(session, "exampleAlert42")
      study_vals$heart_density <- input$heart_density
    }
    
    if((!is.numeric(input$Qpf) | input$Qpf <= 0)) {
      ## Conditions
      createAlert(session, "Qpf_alert", "exampleAlert43",
                  content = "Pericardium bloood flow must be > 0", append = FALSE)
      study_vals$Qpf <- NULL
    } else {
      closeAlert(session, "exampleAlert43")
      study_vals$Qpf <- input$Qpf
    }
    
    if((!is.numeric(input$CLefflux) | input$CLefflux < 0)) {
      ## Conditions
      createAlert(session, "CLefflux_alert", "exampleAlert44",
                  content = "CLefflux must be => 0", append = FALSE)
      study_vals$CLefflux <- NULL
    } else {
      closeAlert(session, "exampleAlert44")
      study_vals$CLefflux <- input$CLefflux
    }
    
    if((!is.numeric(input$CLuptake) | input$CLuptake < 0)) {
      ## Conditions
      createAlert(session, "CLuptake_alert", "exampleAlert45",
                  content = "CLuptake must be => 0", append = FALSE)
      study_vals$CLuptake <- NULL
    } else {
      closeAlert(session, "exampleAlert45")
      study_vals$CLuptake <- input$CLuptake
    }
    
    
  })
  
  #
  # Study tab logic (end)
  #
  

  #
  # API tab logic (start) ------------------------------------------------------
  #

  api_vals <- reactiveValues()

  # observe checkboxes - if unchecked assign values to appropriate numeric inputs 


  # 1st column

  observeEvent(input$METAB_present, {
    if(input$METAB_present == FALSE){
      print("Metabolite physico-chemical parameters OFF")
      updateNumericInput(session, "MW_metab", value = input$MW_api)
      updateNumericInput(session, "pKa_metab", value = input$pKa_api)
      
    } else {
      print("Metabolite physico-chemical parameters ON")
      updateNumericInput(session, inputId = "MW_metab", value = 263.384)
      updateNumericInput(session, inputId = "pKa_metab", value = 10.1)
      
    }
  })
  
  # 2nd column
  
  observeEvent(input$Kpad_API, {
    if(input$Kpad_API == FALSE){
      print("Adipose partition coefficient OFF")
      updateNumericInput(session, "Kpad", value = 1)
    } else {
      print("Adipose partition coefficient ON")
      updateNumericInput(session, inputId = "Kpad", value = 4.10)
    }
  })
  
  observeEvent(input$Kpbo_API, {
    if(input$Kpbo_API == FALSE){
      print("Bone partition coefficient OFF")
      updateNumericInput(session, "Kpbo", value = 1)
    } else {
      print("Bone partition coefficient ON")
      updateNumericInput(session, inputId = "Kpbo", value = 4.14)
    }
  })
  
  observeEvent(input$Kpbr_API, {
    if(input$Kpbr_API == FALSE){
      print("Brain partition coefficient OFF")
      updateNumericInput(session, "Kpbr", value = 1)
    } else {
      print("Brain partition coefficient ON")
      updateNumericInput(session, inputId = "Kpbr", value = 3.05)
    }
  })
  
  observeEvent(input$Kpgu_API, {
    if(input$Kpgu_API == FALSE){
      print("Gut partition coefficient OFF")
      updateNumericInput(session, "Kpgu", value = 1)
    } else {
      print("Gut partition coefficient ON")
      updateNumericInput(session, inputId = "Kpgu", value = 11.73)
    }
  })
  
  observeEvent(input$Kphe_API, {
    if(input$Kphe_API == FALSE){
      print("Heart partition coefficient OFF")
      updateNumericInput(session, "Kphe", value = 1)
    } else {
      print("Heart partition coefficient ON")
      updateNumericInput(session, inputId = "Kphe", value = 11.77)
    }
  })
  
  observeEvent(input$Kppf_API, {
    if(input$Kppf_API == FALSE){
      print("Postmortem partition coefficient OFF")
      updateNumericInput(session, "Kppf", value = 1)
    } else {
      print("Postmortem partition coefficient ON")
      updateNumericInput(session, inputId = "Kppf", value = 2.6)
    }
  })
  
  observeEvent(input$Kpec_API, {
    if(input$Kpec_API == FALSE){
      print("Extracellular partition coefficient OFF")
      updateNumericInput(session, "Kpec", value = 1)
    } else {
      print("Extracellular partition coefficient ON")
      updateNumericInput(session, inputId = "Kpec", value = 1)
    }
  })
  
  observeEvent(input$Kpki_API, {
    if(input$Kpki_API == FALSE){
      print("Kidney partition coefficient OFF")
      updateNumericInput(session, "Kpki", value = 1)
    } else {
      print("Kidney partition coefficient ON")
      updateNumericInput(session, inputId = "Kpki", value = 9.79)
    }
  })
  
  observeEvent(input$Kpli_API, {
    if(input$Kpli_API == FALSE){
      print("Liver partition coefficient OFF")
      updateNumericInput(session, "Kpli", value = 1)
    } else {
      print("Liver partition coefficient ON")
      updateNumericInput(session, inputId = "Kpli", value = 19.80)
    }
  })
  
  observeEvent(input$Kplu_API, {
    if(input$Kplu_API == FALSE){
      print("Lung partition coefficient OFF")
      updateNumericInput(session, "Kplu", value = 1)
    } else {
      print("Lung partition coefficient ON")
      updateNumericInput(session, inputId = "Kplu", value = 2.05)
    }
  })
  
  observeEvent(input$Kpmu_API, {
    if(input$Kpmu_API == FALSE){
      print("Muscle partition coefficient OFF")
      updateNumericInput(session, "Kpmu", value = 1)
    } else {
      print("Muscle partition coefficient ON")
      updateNumericInput(session, inputId = "Kpmu", value = 9.85)
    }
  })
  
  observeEvent(input$Kpsk_API, {
    if(input$Kpsk_API == FALSE){
      print("Skin partition coefficient OFF")
      updateNumericInput(session, "Kpsk", value = 1)
    } else {
      print("Skin partition coefficient ON")
      updateNumericInput(session, inputId = "Kpsk", value = 5.61)
    }
  })
  
  observeEvent(input$Kpsp_API, {
    if(input$Kpsp_API == FALSE){
      print("Spleen partition coefficient OFF")
      updateNumericInput(session, "Kpsp", value = 1)
    } else {
      print("Spleen partition coefficient ON")
      updateNumericInput(session, inputId = "Kpsp", value = 11.02)
    }
  })
  
  observeEvent(input$Kpre_API, {
    if(input$Kpre_API == FALSE){
      print("Rest of the body partition coefficient OFF")
      updateNumericInput(session, "Kpre", value = 1)
    } else {
      print("Rest of the body partition coefficient ON")
      updateNumericInput(session, inputId = "Kpre", value = 1)
    }
  })
  
  observeEvent(input$Kpli_METAB, {
    if(input$Kpli_METAB == FALSE){
      print("Liver metabolite partition coefficient OFF")
      updateNumericInput(session, "Kpli_metab", value = 1)
    } else {
      print("Liver metabolite partition coefficient ON")
      updateNumericInput(session, inputId = "Kpli_metab", value = 59.08)
    }
  })
  
  observeEvent(input$Kphe_METAB, {
    if(input$Kphe_METAB == FALSE){
      print("Heart metabolite partition coefficient OFF")
      updateNumericInput(session, "Kphe_metab", value = 1)
    } else {
      print("Heart metabolite partition coefficient ON")
      updateNumericInput(session, inputId = "Kphe_metab", value = 35.63)
    }
  })
  
  # 3rd column
    
  observeEvent(input$CYP1A2_API_dm, {
    if(input$CYP1A2_API_dm == FALSE){
      print("CYP1A2 not selected")
      updateNumericInput(session, "Vmax_1A2_api_dm", value = 0)
      updateNumericInput(session, "Km_1A2_api_dm", value = 1)
    } else {
      print("CYP1A2 selected")
      updateNumericInput(session, "Vmax_1A2_api_dm", value = 59.08)
      updateNumericInput(session, "Km_1A2_api_dm", value = 35.63)

    }
    
  })
  
  observeEvent(input$CYP2B6_API_dm, {
    if(input$CYP2B6_API_dm == FALSE){
      print("CYP2B6 not selected")
      updateNumericInput(session, inputId = "Vmax_2B6_api_dm", value = 0)
      updateNumericInput(session, inputId = "Km_2B6_api_dm", value = 1)
      
    } else {
      print("CYP2B6 selected")
      updateNumericInput(session, inputId = "Vmax_2B6_api_dm", value = 0.25)
      updateNumericInput(session, inputId = "Km_2B6_api_dm", value = 56.7)
    }
  })
  
  observeEvent(input$CYP2C8_API_dm, {
    if(input$CYP2C8_API_dm == FALSE){
      print("CYP2C8 not selected")
      updateNumericInput(session, inputId = "Vmax_2C8_api_dm", value = 0)
      updateNumericInput(session, inputId = "Km_2C8_api_dm", value = 1)
    } else {
      print("CYP2C8 selected")
      updateNumericInput(session, inputId = "Vmax_2C8_api_dm", value = 0.7)
      updateNumericInput(session, inputId = "Km_2C8_api_dm", value = 9.74)
    }
  })

  observeEvent(input$CYP2C9_API_dm, {
    if(input$CYP2C9_API_dm == FALSE){
      print("CYP2C9 not selected")
      updateNumericInput(session, inputId = "Vmax_2C9_api_dm", value = 0)
      updateNumericInput(session, inputId = "Km_2C9_api_dm", value = 1)
    } else {
      print("CYP2C9 selected")
      updateNumericInput(session, inputId = "Vmax_2C9_api_dm", value = 3.97)
      updateNumericInput(session, inputId = "Km_2C9_api_dm", value = 50.5)
    }
  })

  observeEvent(input$CYP2C19_API_dm, {
    if(input$CYP2C19_API_dm == FALSE){
      print("CYP2C19 not selected")
      updateNumericInput(session, inputId = "Vmax_2C19_api_dm", value = 0)
      updateNumericInput(session, inputId = "Km_2C19_api_dm", value = 1)
    } else {
      print("CYP2C19 selected")
      updateNumericInput(session, inputId = "Vmax_2C19_api_dm", value = 4.22)
      updateNumericInput(session, inputId = "Km_2C19_api_dm", value = 8.52)

    }
  })
  
  observeEvent(input$CYP2D6_API_dm, {
    if(input$CYP2D6_API_dm == FALSE){
      print("CYP2D6 not selected")
      updateNumericInput(session, inputId = "Vmax_2D6_api_dm", value = 0)
      updateNumericInput(session, inputId = "Km_2D6_api_dm", value = 1)
    } else {
      print("CYP2D6 selected")
      updateNumericInput(session, inputId = "Vmax_2D6_api_dm", value = 1.49)
      updateNumericInput(session, inputId = "Km_2D6_api_dm", value = 7.12)
    }
  })
  
  observeEvent(input$CYP3A4_API_dm, {
    if(input$CYP3A4_API_dm == FALSE){
      print("CYP3A4 not selected")
      updateNumericInput(session, inputId = "Vmax_3A4_api_dm", value = 0)
      updateNumericInput(session, inputId = "Km_3A4_api_dm", value = 1)
    } else {
      print("CYP3A4 selected")
      updateNumericInput(session, inputId = "Vmax_3A4_api_dm", value = 3.37)
      updateNumericInput(session, inputId = "Km_3A4_api_dm", value = 213.8)
    }
  })
  
  observeEvent(input$CYP2B6_API_h, {
    if(input$CYP2B6_API_h == FALSE){
      print("CYP2B6_hydroxylation not selected")
      updateNumericInput(session, inputId = "Vmax_2B6_api_h", value = 0)
      updateNumericInput(session, inputId = "Km_2B6_api_h", value = 1)
    } else {
      print("CYP2B6_hydroxylation selected")
      updateNumericInput(session, inputId = "Vmax_2B6_api_h", value = 0.13)
      updateNumericInput(session, inputId = "Km_2B6_api_h", value = 98)
    }
  })
  
  observeEvent(input$CYP2D6_API_h, {
    if(input$CYP2D6_API_h == FALSE){
      print("CYP2D6_hydroxylation not selected")
      updateNumericInput(session, inputId = "Vmax_2D6_api_h", value = 0)
      updateNumericInput(session, inputId = "Km_2D6_api_h", value = 1)
    } else {
      print("CYP2D6_hydroxylation selected")
      updateNumericInput(session, inputId = "Vmax_2D6_api_h", value = 2.71)
      updateNumericInput(session, inputId = "Km_2D6_api_h", value = 4.75)
    }
  })
  
  observeEvent(input$CYP3A4_API_h, {
    if(input$CYP3A4_API_h == FALSE){
      print("CYP2D6_hydroxylation not selected")
      updateNumericInput(session, inputId = "Vmax_3A4_api_h", value = 0)
      updateNumericInput(session, inputId = "Km_3A4_api_h", value = 1)
    } else {
      print("CYP2D6_hydroxylation selected")
      updateNumericInput(session, inputId = "Vmax_3A4_api_h", value = 0.4)
      updateNumericInput(session, inputId = "Km_3A4_api_h", value = 69.3)
    }
  })
  
  observeEvent(input$CYP1A2_METAB_dm, {
    if(input$CYP1A2_METAB_dm == FALSE){
      print("CYP1A2 metabolite not selected")
      updateNumericInput(session, inputId = "Vmax_1A2_metab_dm", value = 0)
      updateNumericInput(session, inputId = "Km_1A2_metab_dm", value = 1)
    } else {
      print("CYP1A2 metabolite selected")
      updateNumericInput(session, inputId = "Vmax_1A2_metab_dm", value = 6.8)
      updateNumericInput(session, inputId = "Km_1A2_metab_dm", value = 54.2)
    }
  })
  
  observeEvent(input$CYP2C19_METAB_dm, {
    if(input$CYP2C19_METAB_dm == FALSE){
      print("CYP2C19 metabolite not selected")
      updateNumericInput(session, inputId = "Vmax_2C19_metab_dm", value = 0)
      updateNumericInput(session, inputId = "Km_2C19_metab_dm", value = 1)
    } else {
      print("CYP2C19 metabolite selected")
      updateNumericInput(session, inputId = "Vmax_2C19_metab_dm", value = 93.1)
      updateNumericInput(session, inputId = "Km_2C19_metab_dm", value = 118)
    }
  })
  
  observeEvent(input$CYP2D6_METAB_dm, {
    if(input$CYP2D6_METAB_dm == FALSE){
      print("CYP2D6 metabolite not selected")
      updateNumericInput(session, inputId = "Vmax_2D6_metab_dm", value = 0)
      updateNumericInput(session, inputId = "Km_2D6_metab_dm", value = 1)
    } else {
      print("CYP2D6 metabolite selected")
      updateNumericInput(session, inputId = "Vmax_2D6_metab_dm", value = 19.4)
      updateNumericInput(session, inputId = "Km_2D6_metab_dm", value = 0.48)
    }
  })
  
  observeEvent(input$CYP2D6_METAB_h, {
    if(input$CYP2D6_METAB_h == FALSE){
      print("CYP2D6_hydroxylation metabolite not selected")
      updateNumericInput(session, inputId = "Vmax_2D6_metab_h", value = 0)
      updateNumericInput(session, inputId = "Km_2D6_metab_h", value = 1)
    } else {
      print("CYP2D6_hydroxylation metabolite selected")
      updateNumericInput(session, inputId = "Vmax_2D6_metab_h", value = 130)
      updateNumericInput(session, inputId = "Km_2D6_metab_h", value = 0.74)
    }
    
    
  })
  
  # observe changes in values of API tab START 
  
  observe({
    
    ###### Checking API values with conditions within observe({})
    ###### 1st column
      
      if((!is.numeric(input$MW_api) | input$MW_api <= 0)) {
        ## Conditions
        createAlert(session, "MW_api_alert", "exampleAlert46",
                    content = "API mol. weight must be > 0", append = FALSE)
        api_vals$MW_api <- NULL
      } else {
        closeAlert(session, "exampleAlert46")
        api_vals$MW_api <- input$MW_api
      }
    
    if((!is.numeric(input$pKa_api) | input$pKa_api <= 0)) {
      ## Conditions
      createAlert(session, "pKa_api_alert", "exampleAlert47",
                  content = "pKa must be > 0", append = FALSE)
      api_vals$pKa_api <- NULL
    } else {
      closeAlert(session, "exampleAlert47")
      api_vals$pKa_api <- input$pKa_api
    }
    
    if((!is.numeric(input$PAMPA) | input$PAMPA < 0)) {
      ## Conditions
      createAlert(session, "PAMPA_alert", "exampleAlert48",
                  content = "PAMPA must be > 0", append = FALSE)
      api_vals$PAMPA <- NULL
    } else {
      closeAlert(session, "exampleAlert48")
      api_vals$PAMPA <- input$PAMPA
    }
    
    if((!is.numeric(input$MW_metab) | input$MW_metab <= 0)) {
      ## Conditions
      createAlert(session, "MW_metab_alert", "exampleAlert49",
                  content = "MW_metab must be > 0", append = FALSE)
      api_vals$MW_metab <- NULL
    } else {
      closeAlert(session, "exampleAlert49")
      api_vals$MW_metab <- input$MW_metab
    }
    
    if((!is.numeric(input$pKa_metab) | input$pKa_metab <= 0)) {
      ## Conditions
      createAlert(session, "pKa_metab_alert", "exampleAlert50",
                  content = "pKa metab must be > 0", append = FALSE)
      api_vals$pKa_metab <- NULL
    } else {
      closeAlert(session, "exampleAlert50")
      api_vals$pKa_metab <- input$pKa_metab
    }
    
    
    # 2nd column
    
    if((!is.numeric(input$Kpad) | input$Kpad <= 0)) {
      ## Conditions
      createAlert(session, "Kpad_alert", "exampleAlert51",
                  content = "Adipose partition coefficient must be > 0", append = FALSE)
      api_vals$Kpad <- NULL
    } else {
      closeAlert(session, "exampleAlert51")
      api_vals$Kpad <- input$Kpad
    }
    
    if((!is.numeric(input$Kpbo) | input$Kpbo <= 0)) {
      ## Conditions
      createAlert(session, "Kpbo_alert", "exampleAlert52",
                  content = "Bone partition coefficient must be > 0", append = FALSE)
      api_vals$Kpbo <- NULL
    } else {
      closeAlert(session, "exampleAlert52")
      api_vals$Kpbo <- input$Kpbo
    }
    
    if((!is.numeric(input$Kpbr) | input$Kpbr <= 0)) {
      ## Conditions
      createAlert(session, "Kpbr_alert", "exampleAlert53",
                  content = "Brain partition coefficient must be > 0", append = FALSE)
      api_vals$Kpbr <- NULL
    } else {
      closeAlert(session, "exampleAlert53")
      api_vals$Kpbr <- input$Kpbr
    }
    
    if((!is.numeric(input$Kpgu) | input$Kpgu <= 0)) {
      ## Conditions
      createAlert(session, "Kpgu_alert", "exampleAlert54",
                  content = "Gut partition coefficient must be > 0", append = FALSE)
      api_vals$Kpgu <- NULL
    } else {
      closeAlert(session, "exampleAlert54")
      api_vals$Kpgu <- input$Kpgu
    }
    
    if((!is.numeric(input$Kphe) | input$Kphe <= 0)) {
      ## Conditions
      createAlert(session, "Kphe_alert", "exampleAlert55",
                  content = "Heart partition coefficient must be > 0", append = FALSE)
      api_vals$Kphe <- NULL
    } else {
      closeAlert(session, "exampleAlert55")
      api_vals$Kphe <- input$Kphe
    }
    
    if((!is.numeric(input$Kppf) | input$Kppf <= 0)) {
      ## Conditions
      createAlert(session, "Kppf_alert", "exampleAlert56",
                  content = "Pericardial fluid partition coefficient must be > 0", append = FALSE)
      api_vals$Kppf <- NULL
    } else {
      closeAlert(session, "exampleAlert56")
      api_vals$Kppf <- input$Kppf
    }
    
    if((!is.numeric(input$Kpec) | input$Kpec <= 0)) {
      ## Conditions
      createAlert(session, "Kpec_alert", "exampleAlert57",
                  content = "Extracellular partition coefficient must be > 0", append = FALSE)
      api_vals$Kpec <- NULL
    } else {
      closeAlert(session, "exampleAlert57")
      api_vals$Kpec <- input$Kpec
    }
    
    if((!is.numeric(input$Kpki) | input$Kpki <= 0)) {
      ## Conditions
      createAlert(session, "Kpki_alert", "exampleAlert58",
                  content = "Kidney partition coefficient must be > 0", append = FALSE)
      api_vals$Kpki <- NULL
    } else {
      closeAlert(session, "exampleAlert58")
      api_vals$Kpki <- input$Kpki
    }
    
    if((!is.numeric(input$Kpli) | input$Kpli <= 0)) {
      ## Conditions
      createAlert(session, "Kpli_alert", "exampleAlert59",
                  content = "Liver partition coefficient must be > 0", append = FALSE)
      api_vals$Kpli <- NULL
    } else {
      closeAlert(session, "exampleAlert59")
      api_vals$Kpli <- input$Kpli
    }
    
    if((!is.numeric(input$Kplu) | input$Kplu <= 0)) {
      ## Conditions
      createAlert(session, "Kplu_alert", "exampleAlert60",
                  content = "Lung partition coefficient must be > 0", append = FALSE)
      api_vals$Kplu <- NULL
    } else {
      closeAlert(session, "exampleAlert60")
      api_vals$Kplu <- input$Kplu
    }
    
    if((!is.numeric(input$Kpmu) | input$Kpmu <= 0)) {
      ## Conditions
      createAlert(session, "Kpmu_alert", "exampleAlert61",
                  content = "Muscle partition coefficient must be > 0", append = FALSE)
      api_vals$Kpmu <- NULL
    } else {
      closeAlert(session, "exampleAlert61")
      api_vals$Kpmu <- input$Kpmu
    }
    
    if((!is.numeric(input$Kpsk) | input$Kpsk <= 0)) {
      ## Conditions
      createAlert(session, "Kpsk_alert", "exampleAlert62",
                  content = "Skin partition coefficient must be > 0", append = FALSE)
      api_vals$Kpsk <- NULL
    } else {
      closeAlert(session, "exampleAlert62")
      api_vals$Kpsk <- input$Kpsk
    }
    
    if((!is.numeric(input$Kpsp) | input$Kpsp <= 0)) {
      ## Conditions
      createAlert(session, "Kpsp_alert", "exampleAlert63",
                  content = "Spleen partition coefficient must be > 0", append = FALSE)
      api_vals$Kpsp <- NULL
    } else {
      closeAlert(session, "exampleAlert63")
      api_vals$Kpsp <- input$Kpsp
    }
    
    if((!is.numeric(input$Kpre) | input$Kpre <= 0)) {
      ## Conditions
      createAlert(session, "Kpre_alert", "exampleAlert64",
                  content = "Rest of the body partition coefficient must be > 0", append = FALSE)
      api_vals$Kpre <- NULL
    } else {
      closeAlert(session, "exampleAlert64")
      api_vals$Kpre <- input$Kpre
    }
    
    if((!is.numeric(input$Kpli_metab) | input$Kpli_metab <= 0)) {
      ## Conditions
      createAlert(session, "Kpli_metab_alert", "exampleAlert65",
                  content = "Metabolite's liver partition coefficient must be > 0", append = FALSE)
      api_vals$Kpli_metab <- NULL
    } else {
      closeAlert(session, "exampleAlert65")
      api_vals$Kpli_metab <- input$Kpli_metab
    }
    
    if((!is.numeric(input$Kphe_metab) | input$Kphe_metab <= 0)) {
      ## Conditions
      createAlert(session, "Kphe_metab_alert", "exampleAlert66",
                  content = "Metabolite's heart partition coefficient must be > 0", append = FALSE)
      api_vals$Kphe_metab <- NULL
    } else {
      closeAlert(session, "exampleAlert66")
      api_vals$Kphe_metab <- input$Kphe_metab
    }
    
    if((!is.numeric(input$fu_ht_api) | input$fu_ht_api <= 0)) {
      ## Conditions
      createAlert(session, "fu_ht_api_alert", "exampleAlert67",
                  content = "Unbound fraction of API in heart tissue must be > 0", append = FALSE)
      api_vals$fu_ht_api <- NULL
    } else {
      closeAlert(session, "exampleAlert67")
      api_vals$fu_ht_api <- input$fu_ht_api
    }
    
    if((!is.numeric(input$fuha) | input$fuha <= 0)) {
      ## Conditions
      createAlert(session, "fuha_alert", "exampleAlert68",
                  content = "Unbound fraction (fuha) must be > 0", append = FALSE)
      api_vals$fuha <- NULL
    } else {
      closeAlert(session, "exampleAlert68")
      api_vals$fuha <- input$fuha
    }
    
    if((!is.numeric(input$fuhn) | input$fuhn <= 0)) {
      ## Conditions
      createAlert(session, "fuhn_alert", "exampleAlert69",
                  content = "Unbound fraction (fuhn) must be > 0", append = FALSE)
      api_vals$fuhn <- NULL
    } else {
      closeAlert(session, "exampleAlert69")
      api_vals$fuhn <- input$fuhn
    }
    
    if((!is.numeric(input$Kpre_metab) | input$Kpre_metab <= 0)) {
      ## Conditions
      createAlert(session, "Kpre_metab_alert", "exampleAlert70",
                  content = "Metabolite's rest of the body partition coefficient must be > 0", append = FALSE)
      api_vals$Kpre_metab <- NULL
    } else {
      closeAlert(session, "exampleAlert70")
      api_vals$Kpre_metab <- input$Kpre_metab
    }
    
    
    ### 3rd column
    
    if((!is.numeric(input$Vmax_1A2_api_dm) | input$Vmax_1A2_api_dm < 0)) {
      ## Conditions
      createAlert(session, "Vmax_1A2_api_dm_alert", "exampleAlert71",
                  content = "Vmax CYP1A2 must be => 0", append = FALSE)
      api_vals$Vmax_1A2_api_dm <- NULL
    } else {
      closeAlert(session, "exampleAlert71")
      api_vals$Vmax_1A2_api_dm <- input$Vmax_1A2_api_dm
    }
    
    if((!is.numeric(input$Km_1A2_api_dm) | input$Km_1A2_api_dm <= 0)) {
      ## Conditions
      createAlert(session, "Km_1A2_api_dm_alert", "exampleAlert72",
                  content = "Km CYP1A2 must be > 0", append = FALSE)
      api_vals$Km_1A2_api_dm <- NULL
    } else {
      closeAlert(session, "exampleAlert72")
      api_vals$Km_1A2_api_dm <- input$Km_1A2_api_dm
    }
    
    if((!is.numeric(input$Vmax_2B6_api_dm) | input$Vmax_2B6_api_dm < 0)) {
      ## Conditions
      createAlert(session, "Vmax_2B6_api_dm_alert", "exampleAlert73",
                  content = "Vmax CYP2B6 must be => 0", append = FALSE)
      api_vals$Vmax_2B6_api_dm <- NULL
    } else {
      closeAlert(session, "exampleAlert73")
      api_vals$Vmax_2B6_api_dm <- input$Vmax_2B6_api_dm
    }
    
    if((!is.numeric(input$Km_2B6_api_dm) | input$Km_2B6_api_dm <= 0)) {
      ## Conditions
      createAlert(session, "Km_2B6_api_dm_alert", "exampleAlert74",
                  content = "Km CYP2B6 must be > 0", append = FALSE)
      api_vals$Km_2B6_api_dm <- NULL
    } else {
      closeAlert(session, "exampleAlert74")
      api_vals$Km_2B6_api_dm <- input$Km_2B6_api_dm
    }
    
    if((!is.numeric(input$Vmax_2C8_api_dm) | input$Vmax_2C8_api_dm < 0)) {
      ## Conditions
      createAlert(session, "Vmax_2C8_api_dm_alert", "exampleAlert75",
                  content = "Vmax CYP2C8 must be => 0", append = FALSE)
      api_vals$Vmax_2C8_api_dm <- NULL
    } else {
      closeAlert(session, "exampleAlert75")
      api_vals$Vmax_2C8_api_dm <- input$Vmax_2C8_api_dm
    }
    
    if((!is.numeric(input$Km_2C8_api_dm) | input$Km_2C8_api_dm <= 0)) {
      ## Conditions
      createAlert(session, "Km_2C8_api_dm_alert", "exampleAlert76",
                  content = "Km CYP2C8 must be > 0", append = FALSE)
      api_vals$Km_2C8_api_dm <- NULL
    } else {
      closeAlert(session, "exampleAlert76")
      api_vals$Km_2C8_api_dm <- input$Km_2C8_api_dm
    }
    
    if((!is.numeric(input$Vmax_2C9_api_dm) | input$Vmax_2C9_api_dm < 0)) {
      ## Conditions
      createAlert(session, "Vmax_2C9_api_dm_alert", "exampleAlert77",
                  content = "Vmax CYP2C9 must be => 0", append = FALSE)
      api_vals$Vmax_2C9_api_dm <- NULL
    } else {
      closeAlert(session, "exampleAlert77")
      api_vals$Vmax_2C9_api_dm <- input$Vmax_2C9_api_dm
    }
    
    if((!is.numeric(input$Km_2C9_api_dm) | input$Km_2C9_api_dm <= 0)) {
      ## Conditions
      createAlert(session, "Km_2C9_api_dm_alert", "exampleAlert78",
                  content = "Km CYP2C9 must be > 0", append = FALSE)
      api_vals$Km_2C9_api_dm <- NULL
    } else {
      closeAlert(session, "exampleAlert78")
      api_vals$Km_2C9_api_dm <- input$Km_2C9_api_dm
    }
    
    if((!is.numeric(input$Vmax_2C19_api_dm) | input$Vmax_2C19_api_dm < 0)) {
      ## Conditions
      createAlert(session, "Vmax_2C19_api_dm_alert", "exampleAlert79",
                  content = "Vmax CYP2C19 must be => 0", append = FALSE)
      api_vals$Vmax_2C19_api_dm <- NULL
    } else {
      closeAlert(session, "exampleAlert79")
      api_vals$Vmax_2C19_api_dm <- input$Vmax_2C19_api_dm
    }
    
    if((!is.numeric(input$Km_2C19_api_dm) | input$Km_2C19_api_dm <= 0)) {
      ## Conditions
      createAlert(session, "Km_2C19_api_dm_alert", "exampleAlert80",
                  content = "Km CYP2C19 must be > 0", append = FALSE)
      api_vals$Km_2C19_api_dm <- NULL
    } else {
      closeAlert(session, "exampleAlert80")
      api_vals$Km_2C19_api_dm <- input$Km_2C19_api_dm
    }
    
    if((!is.numeric(input$Vmax_2D6_api_dm) | input$Vmax_2D6_api_dm < 0)) {
      ## Conditions
      createAlert(session, "Vmax_2D6_api_dm_alert", "exampleAlert81",
                  content = "Vmax CYP2D6 must be => 0", append = FALSE)
      api_vals$Vmax_2D6_api_dm <- NULL
    } else {
      closeAlert(session, "exampleAlert81")
      api_vals$Vmax_2D6_api_dm <- input$Vmax_2D6_api_dm
    }
    
    if((!is.numeric(input$Km_2D6_api_dm) | input$Km_2D6_api_dm <= 0)) {
      ## Conditions
      createAlert(session, "Km_2D6_api_dm_alert", "exampleAlert82",
                  content = "Km CYP2D6 must be > 0", append = FALSE)
      api_vals$Km_2D6_api_dm <- NULL
    } else {
      closeAlert(session, "exampleAlert82")
      api_vals$Km_2D6_api_dm <- input$Km_2D6_api_dm
    }
    
    if((!is.numeric(input$Vmax_3A4_api_dm) | input$Vmax_3A4_api_dm < 0)) {
      ## Conditions
      createAlert(session, "Vmax_3A4_api_dm_alert", "exampleAlert83",
                  content = "Vmax CYP3A4 must be => 0", append = FALSE)
      api_vals$Vmax_3A4_api_dm <- NULL
    } else {
      closeAlert(session, "exampleAlert83")
      api_vals$Vmax_3A4_api_dm <- input$Vmax_3A4_api_dm
    }
    
    if((!is.numeric(input$Km_3A4_api_dm) | input$Km_3A4_api_dm <= 0)) {
      ## Conditions
      createAlert(session, "Km_3A4_api_dm_alert", "exampleAlert84",
                  content = "Km CYP3A4 must be > 0", append = FALSE)
      api_vals$Km_3A4_api_dm <- NULL
    } else {
      closeAlert(session, "exampleAlert84")
      api_vals$Km_3A4_api_dm <- input$Km_3A4_api_dm
    }
    
    if((!is.numeric(input$Vmax_2B6_api_h) | input$Vmax_2B6_api_h < 0)) {
      ## Conditions
      createAlert(session, "Vmax_2B6_api_h_alert", "exampleAlert85",
                  content = "Vmax CYP2B6 hydroxylation must be => 0", append = FALSE)
      api_vals$Vmax_2B6_api_h <- NULL
    } else {
      closeAlert(session, "exampleAlert85")
      api_vals$Vmax_2B6_api_h <- input$Vmax_2B6_api_h
    }
    
    if((!is.numeric(input$Km_2B6_api_h) | input$Km_2B6_api_h <= 0)) {
      ## Conditions
      createAlert(session, "Km_2B6_api_h_alert", "exampleAlert86",
                  content = "Km CYP2B6 hydroxylation must be > 0", append = FALSE)
      api_vals$Km_2B6_api_h <- NULL
    } else {
      closeAlert(session, "exampleAlert86")
      api_vals$Km_2B6_api_h <- input$Km_2B6_api_h
    }
    
    if((!is.numeric(input$Vmax_2D6_api_h) | input$Vmax_2D6_api_h < 0)) {
      ## Conditions
      createAlert(session, "Vmax_2D6_api_h_alert", "exampleAlert87",
                  content = "Vmax CYP2D6 hydroxylation must be => 0", append = FALSE)
      api_vals$Vmax_2D6_api_h <- NULL
    } else {
      closeAlert(session, "exampleAlert87")
      api_vals$Vmax_2D6_api_h <- input$Vmax_2D6_api_h
    }
    
    if((!is.numeric(input$Km_2D6_api_h) | input$Km_2D6_api_h <= 0)) {
      ## Conditions
      createAlert(session, "Km_2D6_api_h_alert", "exampleAlert88",
                  content = "Km CYP2D6 hydroxylation must be > 0", append = FALSE)
      api_vals$Km_2D6_api_h <- NULL
    } else {
      closeAlert(session, "exampleAlert88")
      api_vals$Km_2D6_api_h <- input$Km_2D6_api_h
    }
    
    if((!is.numeric(input$Vmax_3A4_api_h) | input$Vmax_3A4_api_h < 0)) {
      ## Conditions
      createAlert(session, "Vmax_3A4_api_h_alert", "exampleAlert89",
                  content = "Vmax CYP3A4 hydroxylation must be => 0", append = FALSE)
      api_vals$Vmax_3A4_api_h <- NULL
    } else {
      closeAlert(session, "exampleAlert89")
      api_vals$Vmax_3A4_api_h <- input$Vmax_3A4_api_h
    }
    
    if((!is.numeric(input$Km_3A4_api_h) | input$Km_3A4_api_h <= 0)) {
      ## Conditions
      createAlert(session, "Km_3A4_api_h_alert", "exampleAlert90",
                  content = "Km CYP3A4 hydroxylation must be > 0", append = FALSE)
      api_vals$Km_3A4_api_h <- NULL
    } else {
      closeAlert(session, "exampleAlert90")
      api_vals$Km_3A4_api_h <- input$Km_3A4_api_h
    }
    
    if((!is.numeric(input$Vmax_1A2_metab_dm) | input$Vmax_1A2_metab_dm < 0)) {
      ## Conditions
      createAlert(session, "Vmax_1A2_metab_dm_alert", "exampleAlert91",
                  content = "Vmax CYP1A2 for metabolite must be => 0", append = FALSE)
      api_vals$Vmax_1A2_metab_dm <- NULL
    } else {
      closeAlert(session, "exampleAlert91")
      api_vals$Vmax_1A2_metab_dm <- input$Vmax_1A2_metab_dm
    }
    
    if((!is.numeric(input$Km_1A2_metab_dm) | input$Km_1A2_metab_dm <= 0)) {
      ## Conditions
      createAlert(session, "Km_1A2_metab_dm_alert", "exampleAlert92",
                  content = "Km CYP1A2 for metabolite must be > 0", append = FALSE)
      api_vals$Km_1A2_metab_dm <- NULL
    } else {
      closeAlert(session, "exampleAlert92")
      api_vals$Km_1A2_metab_dm <- input$Km_1A2_metab_dm
    }
    
    if((!is.numeric(input$Vmax_2C19_metab_dm) | input$Vmax_2C19_metab_dm < 0)) {
      ## Conditions
      createAlert(session, "Vmax_2C19_metab_dm_alert", "exampleAlert93",
                  content = "Vmax CYP2C19 for metabolite must be => 0", append = FALSE)
      api_vals$Vmax_2C19_metab_dm <- NULL
    } else {
      closeAlert(session, "exampleAlert93")
      api_vals$Vmax_2C19_metab_dm <- input$Vmax_2C19_metab_dm
    }
    
    if((!is.numeric(input$Km_2C19_metab_dm) | input$Km_2C19_metab_dm <= 0)) {
      ## Conditions
      createAlert(session, "Km_2C19_metab_dm_alert", "exampleAlert94",
                  content = "Km CYP2C19 for metabolite must be > 0", append = FALSE)
      api_vals$Km_2C19_metab_dm <- NULL
    } else {
      closeAlert(session, "exampleAlert94")
      api_vals$Km_2C19_metab_dm <- input$Km_2C19_metab_dm
    }
    
    if((!is.numeric(input$Vmax_2D6_metab_dm) | input$Vmax_2D6_metab_dm < 0)) {
      ## Conditions
      createAlert(session, "Vmax_2D6_metab_dm_alert", "exampleAlert95",
                  content = "Vmax CYP2D6 for metabolite must be => 0", append = FALSE)
      api_vals$Vmax_2D6_metab_dm <- NULL
    } else {
      closeAlert(session, "exampleAlert95")
      api_vals$Vmax_2D6_metab_dm <- input$Vmax_2D6_metab_dm
    }
    
    if((!is.numeric(input$Km_2D6_metab_dm) | input$Km_2D6_metab_dm <= 0)) {
      ## Conditions
      createAlert(session, "Km_2D6_metab_dm_alert", "exampleAlert96",
                  content = "Km CYP2D6 for metabolite must be > 0", append = FALSE)
      api_vals$Km_2D6_metab_dm <- NULL
    } else {
      closeAlert(session, "exampleAlert96")
      api_vals$Km_2D6_metab_dm <- input$Km_2D6_metab_dm
    }
    
    if((!is.numeric(input$Vmax_2D6_metab_h) | input$Vmax_2D6_metab_h < 0)) {
      ## Conditions
      createAlert(session, "Vmax_2D6_metab_h_alert", "exampleAlert97",
                  content = "Vmax CYP2D6 hydroxylation for metabolite must be => 0", append = FALSE)
      api_vals$Vmax_2D6_metab_h <- NULL
    } else {
      closeAlert(session, "exampleAlert97")
      api_vals$Vmax_2D6_metab_h <- input$Vmax_2D6_metab_h
    }
    
    if((!is.numeric(input$Km_2D6_metab_h) | input$Km_2D6_metab_h <= 0)) {
      ## Conditions
      createAlert(session, "Km_2D6_metab_h_alert", "exampleAlert98",
                  content = "Km CYP2D6 hydroxylation for metabolite must be > 0", append = FALSE)
      api_vals$Km_2D6_metab_h <- NULL
    } else {
      closeAlert(session, "exampleAlert98")
      api_vals$Km_2D6_metab_h <- input$Km_2D6_metab_h
    }


    
    })
  
  
  
  
  
  # observe changes in values of API tab END
  

  #
  # API tab logic (end) ------------------------------------------------------
  #
  

  #
  # MODEL logic (start)  ------------------------------------------------------
  #
  
# Run simulation button logic  
observeEvent(input$run_sim, {
  
  # requirements tu run simulation
  req(study_vals$dose,
      study_vals$dose_every,
      study_vals$inf_dose,
      study_vals$inf_time,
      study_vals$t_end,
      study_vals$ISEF1A2,
      study_vals$ISEF2C19,
      study_vals$ISEF2D6,
      study_vals$ISEF2C9,
      study_vals$ISEF3A4,
      study_vals$ISEF2B6,
      study_vals$ISEF2C8,
      study_vals$fumic,
      study_vals$fumic_metab,
      study_vals$CLrenal,
      study_vals$CLrenal_metab,
      study_vals$liver_density,
      study_vals$heart_density,
      study_vals$Qpf,
      study_vals$CLefflux,
      study_vals$CLuptake)
  
  req(api_vals$MW_api,
      api_vals$pKa_api,
      api_vals$PAMPA,
      api_vals$MW_metab,
      api_vals$pKa_metab,
      api_vals$Kpad,
      api_vals$Kpbo,
      api_vals$Kpbr,
      api_vals$Kpgu,
      api_vals$Kphe,
      api_vals$Kppf,
      api_vals$Kpec,
      api_vals$Kpki,
      api_vals$Kpli,
      api_vals$Kplu,
      api_vals$Kpmu,
      api_vals$Kpsk,
      api_vals$Kpsp,
      api_vals$Kpre,
      api_vals$Kpli_metab,
      api_vals$Kphe_metab,
      api_vals$fu_ht_api,
      api_vals$Kpre_metab,
      api_vals$fuha,
      api_vals$fuhn,
      api_vals$Vmax_1A2_api_dm,
      api_vals$Km_1A2_api_dm,
      api_vals$Vmax_2B6_api_dm,
      api_vals$Km_2B6_api_dm,
      api_vals$Vmax_2C8_api_dm,
      api_vals$Km_2C8_api_dm,
      api_vals$Vmax_2C9_api_dm,
      api_vals$Km_2C9_api_dm,
      api_vals$Vmax_2C19_api_dm,
      api_vals$Km_2C19_api_dm,
      api_vals$Vmax_2D6_api_dm,
      api_vals$Km_2D6_api_dm,
      api_vals$Vmax_3A4_api_dm,
      api_vals$Km_3A4_api_dm,
      api_vals$Vmax_2B6_api_h,
      api_vals$Km_2B6_api_h,
      api_vals$Vmax_2D6_api_h,
      api_vals$Km_2D6_api_h,
      api_vals$Vmax_3A4_api_h,
      api_vals$Km_3A4_api_h,
      api_vals$Vmax_1A2_metab_dm,
      api_vals$Km_1A2_metab_dm,
      api_vals$Vmax_2C19_metab_dm,
      api_vals$Km_2C19_metab_dm,
      api_vals$Vmax_2D6_metab_dm,
      api_vals$Km_2D6_metab_dm,
      api_vals$Vmax_2D6_metab_h,
      api_vals$Km_2D6_metab_h)
  
  # Disable all buttons/sliders/checkboxes/textinput etc. when running simulations
  shinyjs::disable("reset_population_defaults_general")
  shinyjs::disable("reset_population_defaults_pharmacokinetics_absorption")
  shinyjs::disable("reset_study_specific_defaults_oral")
  shinyjs::disable("reset_scaling_factors_defaults")
  shinyjs::disable("reset_study_specific_defaults_fumic")
  shinyjs::disable("reset_API_defaults_phys_chem_param")
  shinyjs::disable("reset_tissue_partition_coefficients_defaults_param")
  shinyjs::disable("reset_elimination_defaults_param")
  shinyjs::disable("seed")
  shinyjs::disable("individual_count")
  shinyjs::disable("female_count")
  shinyjs::disable("age_range")
  shinyjs::disable("scale_M")
  shinyjs::disable("shape_M")
  shinyjs::disable("scale_F")
  shinyjs::disable("shape_F")
  shinyjs::disable("ka")
  shinyjs::disable("tlag_m_lognorm_m")
  shinyjs::disable("tlag_m_lognorm_cv")
  shinyjs::disable("F_mean")
  shinyjs::disable("F_sd")
  shinyjs::disable("F_lower")
  shinyjs::disable("F_upper")
  shinyjs::disable("FaFg_m_lognorm_m")
  shinyjs::disable("FaFg_m_lognorm_cv")
  shinyjs::disable("fup_m_lognorm_2_m")
  shinyjs::disable("fup_m_lognorm_2_cv")
  shinyjs::disable("BP_m_lognorm_2_m")
  shinyjs::disable("BP_m_lognorm_2_cv")
  shinyjs::disable("BP_metab_m_lognorm_2_m")
  shinyjs::disable("BP_metab_m_lognorm_2_cv")
  shinyjs::disable("fup_metab_1")
  shinyjs::disable("fup_metab_2")
  shinyjs::disable("pop_data_colnames")
  shinyjs::disable("pop_data_rownames")
  shinyjs::disable("pop_data_sep")
  shinyjs::disable("dose")
  shinyjs::disable("no_doses")
  shinyjs::disable("dose_every")
  shinyjs::disable("inf_dose")
  shinyjs::disable("inf_time")
  shinyjs::disable("t_end")
  shinyjs::disable("ISEF1A2")
  shinyjs::disable("ISEF2C19")
  shinyjs::disable("ISEF2D6")
  shinyjs::disable("ISEF2C9")
  shinyjs::disable("ISEF3A4")
  shinyjs::disable("ISEF2B6")
  shinyjs::disable("ISEF2C8")
  shinyjs::disable("fumic")
  shinyjs::disable("fumic_metab")
  shinyjs::disable("CLrenal")
  shinyjs::disable("CLrenal_metab")
  shinyjs::disable("liver_density")
  shinyjs::disable("heart_density")
  shinyjs::disable("Qpf")
  shinyjs::disable("CLefflux")
  shinyjs::disable("CLuptake")
  shinyjs::disable("MW_api")
  shinyjs::disable("pKa_api")
  shinyjs::disable("PAMPA")
  shinyjs::disable("fu_ht_api")
  shinyjs::disable("Kpre_metab")
  shinyjs::disable("fuha")
  shinyjs::disable("fuhn")
  shinyjs::disable("METAB_present")
  shinyjs::disable("MW_metab")
  shinyjs::disable("pKa_metab")
  shinyjs::disable("Kpad_API")
  shinyjs::disable("Kpad")
  shinyjs::disable("Kpbo_API")
  shinyjs::disable("Kpbo")
  shinyjs::disable("Kpbr_API")
  shinyjs::disable("Kpbr")
  shinyjs::disable("Kpgu_API")
  shinyjs::disable("Kpgu")
  shinyjs::disable("Kphe_API")
  shinyjs::disable("Kphe")
  shinyjs::disable("Kppf_API")
  shinyjs::disable("Kppf")
  shinyjs::disable("Kpec_API")
  shinyjs::disable("Kpec")
  shinyjs::disable("Kpki_API")
  shinyjs::disable("Kpki")
  shinyjs::disable("Kpli_API")
  shinyjs::disable("Kpli")
  shinyjs::disable("Kplu_API")
  shinyjs::disable("Kplu")
  shinyjs::disable("Kpmu_API")
  shinyjs::disable("Kpmu")
  shinyjs::disable("Kpsk_API")
  shinyjs::disable("Kpsk")
  shinyjs::disable("Kpsp_API")
  shinyjs::disable("Kpsp")
  shinyjs::disable("Kpre_API")
  shinyjs::disable("Kpre")
  shinyjs::disable("Kpli_METAB")
  shinyjs::disable("Kpli_metab")
  shinyjs::disable("Kphe_METAB")
  shinyjs::disable("Kphe_metab")
  shinyjs::disable("CYP1A2_API_dm")
  shinyjs::disable("Vmax_1A2_api_dm")
  shinyjs::disable("Km_1A2_api_dm")
  shinyjs::disable("CYP2B6_API_dm")
  shinyjs::disable("Vmax_2B6_api_dm")
  shinyjs::disable("Km_2B6_api_dm")
  shinyjs::disable("CYP2C8_API_dm")
  shinyjs::disable("Vmax_2C8_api_dm")
  shinyjs::disable("Km_2C8_api_dm")
  shinyjs::disable("CYP2C9_API_dm")
  shinyjs::disable("Vmax_2C9_api_dm")
  shinyjs::disable("Km_2C9_api_dm")
  shinyjs::disable("CYP2C19_API_dm")
  shinyjs::disable("Vmax_2C19_api_dm")
  shinyjs::disable("Km_2C19_api_dm")
  shinyjs::disable("CYP2D6_API_dm")
  shinyjs::disable("Vmax_2D6_api_dm")
  shinyjs::disable("Km_2D6_api_dm")
  shinyjs::disable("CYP3A4_API_dm")
  shinyjs::disable("Vmax_3A4_api_dm")
  shinyjs::disable("Km_3A4_api_dm")
  shinyjs::disable("CYP2B6_API_h")
  shinyjs::disable("Vmax_2B6_api_h")
  shinyjs::disable("Km_2B6_api_h")
  shinyjs::disable("CYP2D6_API_h")
  shinyjs::disable("Vmax_2D6_api_h")
  shinyjs::disable("Km_2D6_api_h")
  shinyjs::disable("CYP3A4_API_h")
  shinyjs::disable("Vmax_3A4_api_h")
  shinyjs::disable("Km_3A4_api_h")
  shinyjs::disable("CYP1A2_METAB_dm")
  shinyjs::disable("Vmax_1A2_metab_dm")
  shinyjs::disable("Km_1A2_metab_dm")
  shinyjs::disable("CYP2C19_METAB_dm")
  shinyjs::disable("Vmax_2C19_metab_dm")
  shinyjs::disable("Km_2C19_metab_dm")
  shinyjs::disable("CYP2D6_METAB_dm")
  shinyjs::disable("Vmax_2D6_metab_dm")
  shinyjs::disable("Km_2D6_metab_dm")
  shinyjs::disable("CYP2D6_METAB_h")
  shinyjs::disable("Vmax_2D6_metab_h")
  shinyjs::disable("Km_2D6_metab_h")
  shinyjs::disable("run_sim")
  shinyjs::disable("api_plot_caption")
  shinyjs::disable("metab_plot_caption")
  shinyjs::disable("res_data_colnames")
  shinyjs::disable("res_data_rownames")
  shinyjs::disable("res_data_sep")
  shinyjs::disable("downloadResults")
  shinyjs::disable("in_external_data")
  shinyjs::disable("in_external_quote")
  shinyjs::disable("in_external_sep")
  shinyjs::disable("in_external_header")
  shinyjs::disable("in_external_data_metab")
  shinyjs::disable("in_external_metab_quote")
  shinyjs::disable("in_external_metab_sep")
  shinyjs::disable("in_external_metab_header")
  shinyjs::disable("downloadData")
  shinyjs::disable("downloadPlot_pop_age")
  shinyjs::disable("downloadPlot_pop_age_height")
  shinyjs::disable("downloadPlot_pop_age_width")
  shinyjs::disable("downloadPlot_pop_age_dpi")
  shinyjs::disable("downloadPlot_pop_device")
  shinyjs::disable("downloadPlot_pop_weight")
  shinyjs::disable("downloadPlot_pop_weight_height")
  shinyjs::disable("downloadPlot_pop_weight_width")
  shinyjs::disable("downloadPlot_pop_weight_dpi")
  shinyjs::disable("downloadPlot_pop_weight_device")
  shinyjs::disable("downloadPlot_pop_height")
  shinyjs::disable("downloadPlot_pop_height_height")
  shinyjs::disable("downloadPlot_pop_height_width")
  shinyjs::disable("downloadPlot_pop_height_dpi")
  shinyjs::disable("downloadPlot_pop_height_device")
  shinyjs::disable("downloadPlot_pop_mean_CO_Tanner")
  shinyjs::disable("downloadPlot_pop_mean_CO_Tanner_height")
  shinyjs::disable("downloadPlot_pop_mean_CO_Tanner_width")
  shinyjs::disable("downloadPlot_pop_mean_CO_Tanner_dpi")
  shinyjs::disable("downloadPlot_pop_mean_CO_Tanner_device")
  
  shinyjs::disable("downloadPlot_res_log_conc_in_venous_plasma")
  shinyjs::disable("downloadPlot_res_log_conc_in_venous_plasma_height")
  shinyjs::disable("downloadPlot_res_log_conc_in_venous_plasma_width")
  shinyjs::disable("downloadPlot_res_log_conc_in_venous_plasma_dpi")
  shinyjs::disable("downloadPlot_res_log_conc_in_venous_plasma_device")
  
  shinyjs::disable("downloadPlot_res_conc_in_venous_plasma")
  shinyjs::disable("downloadPlot_res_conc_in_venous_plasma_height")
  shinyjs::disable("downloadPlot_res_conc_in_venous_plasma_width")
  shinyjs::disable("downloadPlot_res_conc_in_venous_plasma_dpi")
  shinyjs::disable("downloadPlot_res_conc_in_venous_plasma_device")
  
  shinyjs::disable("downloadPlot_res_conc_metab_in_venous_plasma")
  shinyjs::disable("downloadPlot_res_conc_metab_in_venous_plasma_height")
  shinyjs::disable("downloadPlot_res_conc_metab_in_venous_plasma_width")
  shinyjs::disable("downloadPlot_res_conc_metab_in_venous_plasma_dpi")
  shinyjs::disable("downloadPlot_res_conc_metab_in_venous_plasma_device")
  
  shinyjs::disable("downloadPlot_res_conc_in_heart")
  shinyjs::disable("downloadPlot_res_conc_in_heart_height")
  shinyjs::disable("downloadPlot_res_conc_in_heart_width")
  shinyjs::disable("downloadPlot_res_conc_in_heart_dpi")
  shinyjs::disable("downloadPlot_res_conc_in_heart_device")
  
  shinyjs::disable("downloadPlot_res_conc_metab_in_heart")
  shinyjs::disable("downloadPlot_res_conc_metab_in_heart_height")
  shinyjs::disable("downloadPlot_res_conc_metab_in_heart_width")
  shinyjs::disable("downloadPlot_res_conc_metab_in_heart_dpi")
  shinyjs::disable("downloadPlot_res_conc_metab_in_heart_device")
  
  shinyjs::disable("downloadPlot_stats_res_conc_metab_in_venous_plasma_CI")
  shinyjs::disable("downloadPlot_stats_res_conc_metab_in_venous_plasma_width")
  shinyjs::disable("downloadPlot_stats_res_conc_metab_in_venous_plasma_height")
  shinyjs::disable("downloadPlot_stats_res_conc_metab_in_venous_plasma_dpi")
  shinyjs::disable("downloadPlot_stats_res_conc__metab_in_venous_plasma_device")
  shinyjs::disable("downloadPlot_stats_res_conc_metab_in_venous_plasma")
  
  shinyjs::disable("downloadPlot_stats_res_conc_metab_in_heart_CI")
  shinyjs::disable("downloadPlot_stats_res_conc_metab_in_heart_width")
  shinyjs::disable("downloadPlot_stats_res_conc_metab_in_heart_height")
  shinyjs::disable("downloadPlot_stats_res_conc_metab_in_heart_dpi")
  shinyjs::disable("downloadPlot_stats_res_conc_metab_in_heart_device")
  shinyjs::disable("downloadPlot_stats_res_conc_metab_in_heart")
  
  shinyjs::disable("downloadPlot_stats_res_log_conc_in_venous_plasma_CI")
  shinyjs::disable("downloadPlot_stats_res_log_conc_in_venous_plasma_width")
  shinyjs::disable("downloadPlot_stats_res_log_conc_in_venous_plasma_height")
  shinyjs::disable("downloadPlot_stats_res_log_conc_in_venous_plasma_dpi")
  shinyjs::disable("downloadPlot_stats_res_log_conc_in_venous_plasma_device")
  shinyjs::disable("downloadPlot_stats_res_log_conc_in_venous_plasma")
                   
  shinyjs::disable("downloadPlot_stats_res_conc_in_venous_plasma_CI")
  shinyjs::disable("downloadPlot_stats_res_conc_in_venous_plasma_width")
  shinyjs::disable("downloadPlot_stats_res_conc_in_venous_plasma_height")
  shinyjs::disable("downloadPlot_stats_res_conc_in_venous_plasma_dpi")
  shinyjs::disable("downloadPlot_stats_res_conc_in_venous_plasma_device")
  shinyjs::disable("downloadPlot_stats_res_conc_in_venous_plasma")
                     
  shinyjs::disable("downloadPlot_stats_res_conc_in_heart_CI")
  shinyjs::disable("downloadPlot_stats_res_conc_in_heart_width")
  shinyjs::disable("downloadPlot_stats_res_conc_in_heart_height")
  shinyjs::disable("downloadPlot_stats_res_conc_in_heart_dpi")
  shinyjs::disable("downloadPlot_stats_res_conc_in_heart_device")
  shinyjs::disable("downloadPlot_stats_res_conc_in_heart")
  # Disable all buttons/sliders/checkboxes/textinput etc. when running simulations (END)
  
  pop3 <- as.data.frame(pop3()$x$data)
  
  outputCurry <- list()
  
  for (i in pop3$rn) {
    print(pop3[i,])
    # nam <- paste("outputCurry", i, sep = "")
    #assign (
    #  nam,
      outputCurry[[i]] <- ModelVar(
        pop3[i,]$rn,
        pop3[i,]$weight,
        pop3[i,]$mean.CO.Tanner.formula..L.h.,
        pop3[i,]$MPPGL,
        pop3[i,]$Vad,
        pop3[i,]$Vbl,
        pop3[i,]$Vrb,
        pop3[i,]$Vbo,
        pop3[i,]$Vbr,
        pop3[i,]$Vgu,
        pop3[i,]$Vhe,
        pop3[i,]$Vki,
        pop3[i,]$Vli,
        pop3[i,]$Vlu,
        pop3[i,]$Vpl,
        pop3[i,]$Vmu,
        pop3[i,]$Vsk,
        pop3[i,]$Vsp,
        pop3[i,]$Qad,
        pop3[i,]$Qbo,
        pop3[i,]$Qbr,
        pop3[i,]$Qgu,
        pop3[i,]$Qhe,
        pop3[i,]$Qki,
        pop3[i,]$Qh,
        pop3[i,]$Qlu,
        pop3[i,]$Qmu,
        pop3[i,]$Qsk,
        pop3[i,]$Qsp,
        pop3[i,]$Vpf,
        pop3[i,]$CYP2C8_H,
        pop3[i,]$CYP2C9_H,
        pop3[i,]$CYP2J2_H,
        pop3[i,]$Vre,
        pop3[i,]$Qre,
        pop3[i,]$CYP1A2_L,
        pop3[i,]$CYP2B6_L,
        pop3[i,]$CYP2C8_L,
        pop3[i,]$CYP2C9_L,
        pop3[i,]$CYP2C19_L,
        pop3[i,]$CYP2D6_L,
        pop3[i,]$CYP3A4_L,
        #pop3$`CYP2C9_I [nmol/small intestine]`[i],
        #pop3$`CYP2C19_I [nmol/small intestine]`[i],
        #pop3$`CYP2D6_I [nmol/small intestine]`[i],
        #pop3$`CYP3A4_I [nmol/small intestine]`[i],
        pop3[i,]$tlag,
        pop3[i,]$Bioavailability,
        #pop3$T_si[i],
        pop3[i,]$FaFg,
        study_vals$t_end,  # need to explain
        study_vals$dose,   # need to explain
        pop3[i,]$fup,
        pop3[i,]$fup_metab,
        pop3[i,]$BP,
        pop3[i,]$BP_metab,
        pop3[i,]$MV,
        pop3[i,]$MSA,
        pop3[i,]$SA_pf#,
        #eventdat
        )
     # )

    }
  
  newDF <- do.call(function(...) rbind(..., make.row.names=FALSE), outputCurry) # save calculated data to newDF

  # print(newDF)
  
  
  #
  # Render results plots START --------------------------
  #
  
  output$res_log_conc_in_venous_plasma <- renderPlot({

        ggplot(as.data.frame(newDF), aes(time, BL, colour = rn)) +
        geom_line(size = 1) +
        theme_classic() +
        scale_y_log10() + # correct way of presenting the log10 values?
        theme(
          panel.grid.major = element_line(colour="grey",size = rel(0.5)),
          panel.grid.minor = element_blank(),
          axis.title = element_text(size = 13),
          axis.text = element_text(size = 13),
          legend.title = element_text(size = 11),
          legend.position = "right"
        ) +
        ggtitle(paste("Log 10 concentration vs. time for ",input$api_plot_caption, sep="")) +
        labs(
            x = "Time [h]",
            y = "log 10 concentration in venous plasma [mg/L]",
            colour = "Individuals"
        )
    

  })
  
  output$res_conc_in_venous_plasma <- renderPlot({
    ggplot(newDF, aes(time, BL, colour = rn)) +
      geom_line(size = 1) +
      theme_classic() +
      theme(
        panel.grid.major = element_line(colour="grey",size = rel(0.5)),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 13),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 11),
        legend.position = "right"
      ) +
      ggtitle(paste("Concentration vs. time for ",input$api_plot_caption, sep="")) +
      labs(
          x = "Time [h]",
          y = "Concentration in venous plasma [mg/L]",
          colour ="Individuals"
      )
    
  })
  
  
  output$res_conc_in_heart <- renderPlot({
    ggplot(newDF, aes(time, HT, colour = rn)) +
     geom_line(size = 1) +
     theme_classic() +
     theme(
       panel.grid.major = element_line(colour="grey",size = rel(0.5)),
       panel.grid.minor = element_blank(),
       axis.title = element_text(size = 13),
       axis.text = element_text(size = 13),
       legend.text = element_text(size = 11),
       legend.title = element_text(size = 11),
       legend.position = "right"
     ) +
     ggtitle(paste("Concentration vs. time for ",input$api_plot_caption, sep="")) +
     labs(
         x = "Time [h]",
         y = "Concentration in heart tissue [mg/L]",
         colour = "Individuals"
     )
  })
  
if(input$METAB_present == TRUE){
  
  output$res_conc_metab_in_venous_plasma <- renderPlot({
    ggplot(newDF, aes(time, BL_METAB, colour = rn)) +
      geom_line(size = 1) +
      theme_classic() +
      theme(
        panel.grid.major = element_line(colour="grey",size = rel(0.5)),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 13),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 11),
        legend.position = "right"
      ) +
      ggtitle(paste("Concentration vs. time for ",input$metab_plot_caption, sep="")) +
      labs(
        x = "Time [h]",
        y = "Concentration in venous plasma [mg/L]",
        colour = "Individuals"
      )
  })
  
  
  output$res_conc_metab_in_heart <- renderPlot({
    ggplot(newDF, aes(time, HT_METAB, colour = rn)) +
      geom_line(size = 1) +
      theme_classic() +
      theme(
        panel.grid.major = element_line(colour="grey",size = rel(0.5)),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 13),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 11),
        legend.position = "right"
      ) +
      ggtitle(paste("Concentration vs. time for ",input$metab_plot_caption, sep="")) +
      labs(
          x = "Time [h]",
          y = "Concentration in heart tissue [mg/L]",
          colour = "Individuals"
        )
  })
  
  output$stats_res_conc_metab_in_venous_plasma <- renderPlot({
    
    newDF <- as.data.frame(newDF)
    
    subset <- newDF[is.finite(rowSums(newDF[,2:ncol(newDF)])),]
    
    
    CI_level <- input$downloadPlot_stats_res_conc_metab_in_venous_plasma_CI
    
    CI_low <- (1-(CI_level/100))/2
    CI_high <- 1 - ((1- (CI_level/100))/2)
    
    stats_iv <- plyr::ddply(subset, .(time), function(subset) statFunc(subset$BL_METAB, CI_low, CI_high))
    
    ggplot(data=stats_iv, aes(x = time, y = median), colour = "red", alpha = 0.8) +
      geom_line(size = 1) +
      theme_classic() +
      theme(
        panel.grid.major = element_line(colour="grey",size = rel(0.5)),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 13),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 11),
        legend.position = "right"
      ) +
      ggtitle(paste("Concentration vs. time for ",input$metab_plot_caption, sep="")) +
      labs(
        x = "Time [h]",
        y = "Concentration of metabolite in venous plasma [mg/L]",
        colour ="Individuals"
      ) + geom_ribbon(data= stats_iv, aes(x = time, ymin = lower_CI, ymax = higher_CI), fill="red", color = "red", alpha = 0.3)
    
  })
  
  output$stats_res_conc_metab_in_heart <- renderPlot({
    
    newDF <- as.data.frame(newDF)
    
    subset <- newDF[is.finite(rowSums(newDF[,2:ncol(newDF)])),]
    
    
    CI_level <- input$downloadPlot_stats_res_conc_metab_in_heart_CI
    
    CI_low <- (1-(CI_level/100))/2
    CI_high <- 1 - ((1- (CI_level/100))/2)
    
    stats_iv <- plyr::ddply(subset, .(time), function(subset) statFunc(subset$HT_METAB, CI_low, CI_high))
    
    ggplot(data=stats_iv, aes(x = time, y = median), colour = "red", alpha = 0.8) +
      geom_line(size = 1) +
      theme_classic() +
      theme(
        panel.grid.major = element_line(colour="grey",size = rel(0.5)),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 13),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 11),
        legend.position = "right"
      ) +
      ggtitle(paste("Concentration vs. time for ",input$metab_plot_caption, sep="")) +
      labs(
          x = "Time [h]",
          y = "Concentration of metabolite in heart [mg/L]",
          colour ="Individuals"
      ) + geom_ribbon(data= stats_iv, aes(x = time, ymin = lower_CI, ymax = higher_CI), fill="red", color = "red", alpha = 0.3)
    
  })
  
}
  
  output$stats_res_conc_in_venous_plasma <- renderPlot({
    
    newDF <- as.data.frame(newDF)

    subset <- newDF[is.finite(rowSums(newDF[,2:ncol(newDF)])),]


      CI_level <- input$downloadPlot_stats_res_conc_in_venous_plasma_CI

      CI_low <- (1-(CI_level/100))/2
      CI_high <- 1 - ((1- (CI_level/100))/2)

      stats_iv <- plyr::ddply(subset, .(time), function(subset) statFunc(subset$BL,CI_low, CI_high))

      ggplot(data=stats_iv, aes(x = time, y = median), colour = "red", alpha = 0.8) +
        geom_line(size = 1) +
        theme_classic() +
        theme(
          panel.grid.major = element_line(colour="grey",size = rel(0.5)),
          panel.grid.minor = element_blank(),
          axis.title = element_text(size = 13),
          axis.text = element_text(size = 13),
          legend.text = element_text(size = 11),
          legend.title = element_text(size = 11),
          legend.position = "right"
        ) +
        ggtitle(paste("Concentration vs. time for ",input$api_plot_caption, sep="")) +
        labs(
            x = "Time [h]",
            y = "Concentration in venous plasma [mg/L]",
            colour = "Individuals"
        ) + geom_ribbon(data= stats_iv, aes(x = time, ymin = lower_CI, ymax = higher_CI), fill="red", color = "red", alpha = 0.3)

  })
  
  
  output$stats_res_log_conc_in_venous_plasma <- renderPlot({
    
    newDF <- as.data.frame(newDF)
    
    subset <- newDF[is.finite(rowSums(newDF[,2:ncol(newDF)])),]
    
    
    CI_level <- input$downloadPlot_stats_res_log_conc_in_venous_plasma_CI
    
    CI_low <- (1-(CI_level/100))/2
    CI_high <- 1 - ((1- (CI_level/100))/2)
    
    stats_iv <- plyr::ddply(subset, .(time), function(subset) statFunc(subset$BL,CI_low, CI_high))
    
    newDF$median <- with(newDF, ave(BL, time, FUN=function(x) median(x, na.rm = TRUE)))
    
    ggplot(data=stats_iv, aes(x = time, y = median), colour = "red", alpha = 0.8) +
      geom_line(size = 1) +
      theme_classic() +
      scale_y_log10() + # correct way of presenting the log10 values
      theme(
        panel.grid.major = element_line(colour="grey",size = rel(0.5)),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 13),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 11),
        legend.position = "right"
      ) +
      ggtitle(paste("Concentration vs. time for ",input$api_plot_caption, sep="")) +
      labs(
          x = "Time [h]",
          y = "Log 10 Concentration in venous plasma [mg/L]",
          colour = "Individuals"
      ) + geom_ribbon(data= stats_iv, aes(x = time, ymin = lower_CI, ymax = higher_CI), fill="red", color = "red", alpha = 0.3)
    
  })
  
  output$stats_res_conc_in_heart <- renderPlot({
    
    newDF <- as.data.frame(newDF)
    
    subset <- newDF[is.finite(rowSums(newDF[,2:ncol(newDF)])),]
    
    
    CI_level <- input$downloadPlot_stats_res_conc_in_heart_CI
    
    CI_low <- (1-(CI_level/100))/2
    CI_high <- 1 - ((1- (CI_level/100))/2)
    
    stats_iv <- plyr::ddply(subset, .(time), function(subset) statFunc(subset$HT, CI_low, CI_high))
    
    newDF$median <- with(newDF, ave(HT, time, FUN=function(x) median(x, na.rm = TRUE)))
    
    ggplot(data=stats_iv, aes(x = time, y = median), colour = "red", alpha = 0.8) +
      geom_line(size = 1) +
      theme_classic() +
      theme(
        panel.grid.major = element_line(colour="grey",size = rel(0.5)),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 13),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 11),
        legend.position = "right"
      ) +
      ggtitle(paste("Concentration vs. time for ",input$api_plot_caption, sep="")) +
      labs(
          x = "Time [h]",
          y = "Concentration in heart [mg/L]",
          colour = "Individuals"
      ) + geom_ribbon(data= stats_iv, aes(x = time, ymin = lower_CI, ymax = higher_CI), fill="red", color = "red", alpha = 0.3)
    
  })

  #
  # Render results plots inside run_sim button observeEvent END ----------------------------
  #
  
  
  
  output$res_datatable <- DT::renderDataTable(  # render newDF in the tab Run
    newDF
  )
  
  
  #
  # Download handlers of data table and plots inside run_sim button observe event START ----------
  #
  
  output$downloadResults <- downloadHandler(
    filename = function(){
      paste("Population",input$seed, "_res_1.csv",sep=".")
    },content = function(file) {
      write.table(newDF,  file=file, row.names=input$res_data_rownames, col.names=input$res_data_colnames, sep=paste(input$res_data_sep))
    },
    contentType="csv"
  )
  
  
  output$downloadPlot_res_log_conc_in_venous_plasma <- downloadHandler(
    filename = function() { paste(input$downloadPlot_res_log_conc_in_venous_plasma, '_plot_1.png', sep='') },
    content = function(file) {
      ggsave(file, plot = plot_res_log_conc_in_venous_plasma(newDF), units = "mm", width = input$downloadPlot_res_log_conc_in_venous_plasma_width,
             height = input$downloadPlot_res_log_conc_in_venous_plasma_height, dpi = input$downloadPlot_res_log_conc_in_venous_plasma_dpi,
             device = input$downloadPlot_res_log_conc_in_venous_plasma_device)
    }
  )
  
  output$downloadPlot_res_conc_in_venous_plasma <- downloadHandler(
    filename = function() { paste(input$downloadPlot_res_conc_in_venous_plasma, '_plot_2.png', sep='') },
    content = function(file) {
      ggsave(file, plot = plot_res_conc_in_venous_plasma(newDF), units = "mm", width = input$downloadPlot_res_conc_in_venous_plasma_width,
             height = input$downloadPlot_res_conc_in_venous_plasma_height, dpi = input$downloadPlot_res_conc_in_venous_plasma_dpi,
             device = input$downloadPlot_res_conc_in_venous_plasma_device)
    }
  )
  
  output$downloadPlot_res_conc_metab_in_venous_plasma <- downloadHandler(
    filename = function() { paste(input$downloadPlot_res_conc_metab_in_venous_plasma, '_plot_3.png', sep='') },
    content = function(file) {
      ggsave(file, plot = plot_res_conc_metab_in_venous_plasma(newDF), units = "mm", width = input$downloadPlot_res_conc_metab_in_venous_plasma_width,
             height = input$downloadPlot_res_conc_metab_in_venous_plasma_height, dpi = input$downloadPlot_res_conc_metab_in_venous_plasma_dpi,
             device = input$downloadPlot_res_conc_metab_in_venous_plasma_device)
    }
  )
  
  output$downloadPlot_res_conc_in_heart <- downloadHandler(
    filename = function() { paste(input$downloadPlot_res_conc_in_heart, '_plot_4.png', sep='') },
    content = function(file) {
      ggsave(file, plot = plot_res_conc_in_heart(newDF), units = "mm", width = input$downloadPlot_res_conc_in_heart_width,
             height = input$downloadPlot_res_conc_in_heart_height, dpi = input$downloadPlot_res_conc_in_heart_dpi,
             device = input$downloadPlot_res_conc_in_heart_device)
    }
  )
  
  output$downloadPlot_res_conc_metab_in_heart <- downloadHandler(
    filename = function() { paste(input$downloadPlot_res_conc_metab_in_heart, '_plot_5.png', sep='') },
    content = function(file) {
      ggsave(file, plot = plot_res_conc_metab_in_heart(newDF), units = "mm", width = input$downloadPlot_res_conc_metab_in_heart_width,
             height = input$downloadPlot_res_conc_metab_in_heart_height, dpi = input$downloadPlot_res_conc_metab_in_heart_dpi,
             device = input$downloadPlot_res_conc_metab_in_heart_device)
    }
  )
  
  output$downloadPlot_stats_res_conc_in_venous_plasma <- downloadHandler(
    filename = function() { paste(input$downloadPlot_stats_res_conc_in_venous_plasma, '_plot_6.png', sep='') },
    content = function(file) {
      ggsave(file, plot = plot_stats_res_conc_in_venous_plasma(newDF), units = "mm", width = input$downloadPlot_stats_res_conc_in_venous_plasma_width,
             height = input$downloadPlot_stats_res_conc_in_venous_plasma_height, dpi = input$downloadPlot_stats_res_conc_in_venous_plasma_dpi,
             device = input$downloadPlot_stats_res_conc_in_venous_plasma_device)
    }
  )
  
  output$downloadPlot_stats_res_log_conc_in_venous_plasma <- downloadHandler(
    filename = function() { paste(input$downloadPlot_stats_res_log_conc_in_venous_plasma, '_plot_7.png', sep='') },
    content = function(file) {
      ggsave(file, plot = plot_stats_res_log_conc_in_venous_plasma(newDF), units = "mm", width = input$downloadPlot_stats_res_log_conc_in_venous_plasma_width,
             height = input$downloadPlot_stats_res_log_conc_in_venous_plasma_height, dpi = input$downloadPlot_stats_res_log_conc_in_venous_plasma_dpi,
             device = input$downloadPlot_stats_res_log_conc_in_venous_plasma_device)
    }
  )
  
  output$downloadPlot_stats_res_conc_in_heart <- downloadHandler(
    filename = function() { paste(input$downloadPlot_stats_res_conc_in_heart, '_plot_8.png', sep='') },
    content = function(file) {
      ggsave(file, plot = plot_stats_res_conc_in_heart(newDF), units = "mm", width = input$downloadPlot_stats_res_conc_in_heart_width,
             height = input$downloadPlot_stats_res_conc_in_heart_height, dpi = input$downloadPlot_stats_res_conc_in_heart_dpi,
             device = input$downloadPlot_stats_res_conc_in_heart_device)
    }
  )
  
  output$downloadPlot_stats_res_conc_metab_in_venous_plasma <- downloadHandler(
    filename = function() { paste(input$downloadPlot_stats_res_conc_metab_in_venous_plasma, '_plot_9.png', sep='') },
    content = function(file) {
      ggsave(file, plot = plot_stats_res_conc_metab_in_venous_plasma(newDF), units = "mm", width = input$downloadPlot_stats_res_conc_metab_in_venous_plasma_width,
             height = input$downloadPlot_stats_res_conc_metab_in_venous_plasma_height, dpi = input$downloadPlot_stats_res_conc_metab_in_venous_plasma_dpi,
             device = input$downloadPlot_stats_res_conc_metab_in_venous_plasma_device)
    }
  )
  
  output$downloadPlot_stats_res_conc_metab_in_heart <- downloadHandler(
    filename = function() { paste(input$downloadPlot_stats_res_conc_metab_in_heart, '_plot_10.png', sep='') },
    content = function(file) {
      ggsave(file, plot = plot_stats_res_conc_metab_in_heart(newDF), units = "mm", width = input$downloadPlot_stats_res_conc_metab_in_heart_width,
             height = input$downloadPlot_stats_res_conc_metab_in_heart_height, dpi = input$downloadPlot_stats_res_conc_metab_in_heart_dpi,
             device = input$downloadPlot_stats_res_conc_metab_in_heart_device)
    }
  )
  
  #
  # Download handlers of data table and plots inside run_sim button observe event END ----------
  #
  

  
  # Enable all buttons/sliders/checkboxes/textinput etc. when simulations is finished (START)
  shinyjs::enable("reset_population_defaults_general")
  shinyjs::enable("reset_population_defaults_pharmacokinetics_absorption")
  shinyjs::enable("reset_population_defaults_pharmacokinetics_distribution")
  shinyjs::enable("reset_study_specific_defaults_oral")
  shinyjs::enable("reset_scaling_factors_defaults")
  shinyjs::enable("reset_study_specific_defaults_fumic")
  shinyjs::enable("reset_API_defaults_phys_chem_param")
  shinyjs::enable("reset_tissue_partition_coefficients_defaults_param")
  shinyjs::enable("reset_elimination_defaults_param")
  shinyjs::enable("seed")
  shinyjs::enable("individual_count")
  shinyjs::enable("female_count")
  shinyjs::enable("age_range")
  shinyjs::enable("scale_M")
  shinyjs::enable("shape_M")
  shinyjs::enable("scale_F")
  shinyjs::enable("shape_F")
  shinyjs::enable("ka")
  shinyjs::enable("tlag_m_lognorm_m")
  shinyjs::enable("tlag_m_lognorm_cv")
  shinyjs::enable("F_mean")
  shinyjs::enable("F_sd")
  shinyjs::enable("F_lower")
  shinyjs::enable("F_upper")
  shinyjs::enable("FaFg_m_lognorm_m")
  shinyjs::enable("FaFg_m_lognorm_cv")
  shinyjs::enable("fup_m_lognorm_2_m")
  shinyjs::enable("fup_m_lognorm_2_cv")
  shinyjs::enable("BP_m_lognorm_2_m")
  shinyjs::enable("BP_m_lognorm_2_cv")
  shinyjs::enable("BP_metab_m_lognorm_2_m")
  shinyjs::enable("BP_metab_m_lognorm_2_cv")
  shinyjs::enable("fup_metab_1")
  shinyjs::enable("fup_metab_2")
  shinyjs::enable("pop_data_colnames")
  shinyjs::enable("pop_data_rownames")
  shinyjs::enable("pop_data_sep")
  shinyjs::enable("dose")
  shinyjs::enable("no_doses")
  shinyjs::enable("dose_every")
  shinyjs::enable("inf_dose")
  shinyjs::enable("inf_time")
  shinyjs::enable("t_end")
  shinyjs::enable("ISEF1A2")
  shinyjs::enable("ISEF2C19")
  shinyjs::enable("ISEF2D6")
  shinyjs::enable("ISEF2C9")
  shinyjs::enable("ISEF3A4")
  shinyjs::enable("ISEF2B6")
  shinyjs::enable("ISEF2C8")
  shinyjs::enable("fumic")
  shinyjs::enable("fumic_metab")
  shinyjs::enable("CLrenal")
  shinyjs::enable("CLrenal_metab")
  shinyjs::enable("liver_density")
  shinyjs::enable("heart_density")
  shinyjs::enable("Qpf")
  shinyjs::enable("CLefflux")
  shinyjs::enable("CLuptake")
  shinyjs::enable("MW_api")
  shinyjs::enable("pKa_api")
  shinyjs::enable("PAMPA")
  shinyjs::enable("fu_ht_api")
  shinyjs::enable("Kpre_metab")
  shinyjs::enable("fuha")
  shinyjs::enable("fuhn")
  shinyjs::enable("METAB_present")
  shinyjs::enable("MW_metab")
  shinyjs::enable("pKa_metab")
  shinyjs::enable("Kpad_API")
  shinyjs::enable("Kpad")
  shinyjs::enable("Kpbo_API")
  shinyjs::enable("Kpbo")
  shinyjs::enable("Kpbr_API")
  shinyjs::enable("Kpbr")
  shinyjs::enable("Kpgu_API")
  shinyjs::enable("Kpgu")
  shinyjs::enable("Kphe_API")
  shinyjs::enable("Kphe")
  shinyjs::enable("Kppf_API")
  shinyjs::enable("Kppf")
  shinyjs::enable("Kpec_API")
  shinyjs::enable("Kpec")
  shinyjs::enable("Kpki_API")
  shinyjs::enable("Kpki")
  shinyjs::enable("Kpli_API")
  shinyjs::enable("Kpli")
  shinyjs::enable("Kplu_API")
  shinyjs::enable("Kplu")
  shinyjs::enable("Kpmu_API")
  shinyjs::enable("Kpmu")
  shinyjs::enable("Kpsk_API")
  shinyjs::enable("Kpsk")
  shinyjs::enable("Kpsp_API")
  shinyjs::enable("Kpsp")
  shinyjs::enable("Kpre_API")
  shinyjs::enable("Kpre")
  shinyjs::enable("Kpli_METAB")
  shinyjs::enable("Kpli_metab")
  shinyjs::enable("Kphe_METAB")
  shinyjs::enable("Kphe_metab")
  shinyjs::enable("CYP1A2_API_dm")
  shinyjs::enable("Vmax_1A2_api_dm")
  shinyjs::enable("Km_1A2_api_dm")
  shinyjs::enable("CYP2B6_API_dm")
  shinyjs::enable("Vmax_2B6_api_dm")
  shinyjs::enable("Km_2B6_api_dm")
  shinyjs::enable("CYP2C8_API_dm")
  shinyjs::enable("Vmax_2C8_api_dm")
  shinyjs::enable("Km_2C8_api_dm")
  shinyjs::enable("CYP2C9_API_dm")
  shinyjs::enable("Vmax_2C9_api_dm")
  shinyjs::enable("Km_2C9_api_dm")
  shinyjs::enable("CYP2C19_API_dm")
  shinyjs::enable("Vmax_2C19_api_dm")
  shinyjs::enable("Km_2C19_api_dm")
  shinyjs::enable("CYP2D6_API_dm")
  shinyjs::enable("Vmax_2D6_api_dm")
  shinyjs::enable("Km_2D6_api_dm")
  shinyjs::enable("CYP3A4_API_dm")
  shinyjs::enable("Vmax_3A4_api_dm")
  shinyjs::enable("Km_3A4_api_dm")
  shinyjs::enable("CYP2B6_API_h")
  shinyjs::enable("Vmax_2B6_api_h")
  shinyjs::enable("Km_2B6_api_h")
  shinyjs::enable("CYP2D6_API_h")
  shinyjs::enable("Vmax_2D6_api_h")
  shinyjs::enable("Km_2D6_api_h")
  shinyjs::enable("CYP3A4_API_h")
  shinyjs::enable("Vmax_3A4_api_h")
  shinyjs::enable("Km_3A4_api_h")
  shinyjs::enable("CYP1A2_METAB_dm")
  shinyjs::enable("Vmax_1A2_metab_dm")
  shinyjs::enable("Km_1A2_metab_dm")
  shinyjs::enable("CYP2C19_METAB_dm")
  shinyjs::enable("Vmax_2C19_metab_dm")
  shinyjs::enable("Km_2C19_metab_dm")
  shinyjs::enable("CYP2D6_METAB_dm")
  shinyjs::enable("Vmax_2D6_metab_dm")
  shinyjs::enable("Km_2D6_metab_dm")
  shinyjs::enable("CYP2D6_METAB_h")
  shinyjs::enable("Vmax_2D6_metab_h")
  shinyjs::enable("Km_2D6_metab_h")
  shinyjs::enable("run_sim")
  shinyjs::enable("api_plot_caption")
  shinyjs::enable("metab_plot_caption")
  shinyjs::enable("res_data_colnames")
  shinyjs::enable("res_data_rownames")
  shinyjs::enable("res_data_sep")
  shinyjs::enable("downloadResults")
  shinyjs::enable("in_external_data")
  shinyjs::enable("in_external_quote")
  shinyjs::enable("in_external_sep")
  shinyjs::enable("in_external_header")
  shinyjs::enable("in_external_data_metab")
  shinyjs::enable("in_external_metab_quote")
  shinyjs::enable("in_external_metab_sep")
  shinyjs::enable("in_external_metab_header")
  shinyjs::enable("downloadData")
  shinyjs::enable("downloadPlot_pop_age")
  shinyjs::enable("downloadPlot_pop_age_height")
  shinyjs::enable("downloadPlot_pop_age_width")
  shinyjs::enable("downloadPlot_pop_dpi")
  shinyjs::enable("downloadPlot_pop_device")
  shinyjs::enable("downloadPlot_pop_weight")
  shinyjs::enable("downloadPlot_pop_weight_height")
  shinyjs::enable("downloadPlot_pop_weight_width")
  shinyjs::enable("downloadPlot_pop_weight_dpi")
  shinyjs::enable("downloadPlot_pop_weight_device")
  shinyjs::enable("downloadPlot_pop_height")
  shinyjs::enable("downloadPlot_pop_height_height")
  shinyjs::enable("downloadPlot_pop_height_width")
  shinyjs::enable("downloadPlot_pop_height_dpi")
  shinyjs::enable("downloadPlot_pop_height_device")
  
  shinyjs::enable("downloadPlot_pop_mean_CO_Tanner")
  shinyjs::enable("downloadPlot_pop_mean_CO_Tanner_height")
  shinyjs::enable("downloadPlot_pop_mean_CO_Tanner_width")
  shinyjs::enable("downloadPlot_pop_mean_CO_Tanner_dpi")
  shinyjs::enable("downloadPlot_pop_mean_CO_Tanner_device")
  
  shinyjs::enable("downloadPlot_res_log_conc_in_venous_plasma")
  shinyjs::enable("downloadPlot_res_log_conc_in_venous_plasma_height")
  shinyjs::enable("downloadPlot_res_log_conc_in_venous_plasma_width")
  shinyjs::enable("downloadPlot_res_log_conc_in_venous_plasma_dpi")
  shinyjs::enable("downloadPlot_res_log_conc_in_venous_plasma_device")
  
  shinyjs::enable("downloadPlot_res_conc_in_venous_plasma")
  shinyjs::enable("downloadPlot_res_conc_in_venous_plasma_height")
  shinyjs::enable("downloadPlot_res_conc_in_venous_plasma_width")
  shinyjs::enable("downloadPlot_res_conc_in_venous_plasma_dpi")
  shinyjs::enable("downloadPlot_res_conc_in_venous_plasma_device")
  
  shinyjs::enable("downloadPlot_res_conc_metab_in_venous_plasma")
  shinyjs::enable("downloadPlot_res_conc_metab_in_venous_plasma_height")
  shinyjs::enable("downloadPlot_res_conc_metab_in_venous_plasma_width")
  shinyjs::enable("downloadPlot_res_conc_metab_in_venous_plasma_dpi")
  shinyjs::enable("downloadPlot_res_conc_metab_in_venous_plasma_device")
  
  shinyjs::enable("downloadPlot_res_conc_in_heart")
  shinyjs::enable("downloadPlot_res_conc_in_heart_height")
  shinyjs::enable("downloadPlot_res_conc_in_heart_width")
  shinyjs::enable("downloadPlot_res_conc_in_heart_dpi")
  shinyjs::enable("downloadPlot_res_conc_in_heart_device")
  
  shinyjs::enable("downloadPlot_res_conc_metab_in_heart")
  shinyjs::enable("downloadPlot_res_conc_metab_in_heart_height")
  shinyjs::enable("downloadPlot_res_conc_metab_in_heart_width")
  shinyjs::enable("downloadPlot_res_conc_metab_in_heart_dpi")
  shinyjs::enable("downloadPlot_res_conc_metab_in_heart_device")
  
  shinyjs::enable("downloadPlot_stats_res_conc_metab_in_venous_plasma_CI")
  shinyjs::enable("downloadPlot_stats_res_conc_metab_in_venous_plasma_width")
  shinyjs::enable("downloadPlot_stats_res_conc_metab_in_venous_plasma_height")
  shinyjs::enable("downloadPlot_stats_res_conc_metab_in_venous_plasma_dpi")
  shinyjs::enable("downloadPlot_stats_res_conc__metab_in_venous_plasma_device")
  shinyjs::enable("downloadPlot_stats_res_conc_metab_in_venous_plasma")
  
  shinyjs::enable("downloadPlot_stats_res_conc_metab_in_heart_CI")
  shinyjs::enable("downloadPlot_stats_res_conc_metab_in_heart_width")
  shinyjs::enable("downloadPlot_stats_res_conc_metab_in_heart_height")
  shinyjs::enable("downloadPlot_stats_res_conc_metab_in_heart_dpi")
  shinyjs::enable("downloadPlot_stats_res_conc_metab_in_heart_device")
  shinyjs::enable("downloadPlot_stats_res_conc_metab_in_heart")
  
  shinyjs::enable("downloadPlot_stats_res_log_conc_in_venous_plasma_CI")
  shinyjs::enable("downloadPlot_stats_res_log_conc_in_venous_plasma_width")
  shinyjs::enable("downloadPlot_stats_res_log_conc_in_venous_plasma_height")
  shinyjs::enable("downloadPlot_stats_res_log_conc_in_venous_plasma_dpi")
  shinyjs::enable("downloadPlot_stats_res_log_conc_in_venous_plasma_device")
  shinyjs::enable("downloadPlot_stats_res_log_conc_in_venous_plasma")
  
  shinyjs::enable("downloadPlot_stats_res_conc_in_venous_plasma_CI")
  shinyjs::enable("downloadPlot_stats_res_conc_in_venous_plasma_width")
  shinyjs::enable("downloadPlot_stats_res_conc_in_venous_plasma_height")
  shinyjs::enable("downloadPlot_stats_res_conc_in_venous_plasma_dpi")
  shinyjs::enable("downloadPlot_stats_res_conc_in_venous_plasma_device")
  shinyjs::enable("downloadPlot_stats_res_conc_in_venous_plasma")
  
  shinyjs::enable("downloadPlot_stats_res_conc_in_heart_CI")
  shinyjs::enable("downloadPlot_stats_res_conc_in_heart_width")
  shinyjs::enable("downloadPlot_stats_res_conc_in_heart_height")
  shinyjs::enable("downloadPlot_stats_res_conc_in_heart_dpi")
  shinyjs::enable("downloadPlot_stats_res_conc_in_heart_device")
  shinyjs::enable("downloadPlot_stats_res_conc_in_heart")
  # Enable all buttons/sliders/checkboxes/textinput etc. when simulations is finished (END)
  
  
  
  #
  # Store newDF - results in data_vals object for further processing
  # 
  
  data_vals <<- newDF
  

  }
)

  #
  # MODEL logic (end)  ------------------------------------------------------
  #
  
  # MODEL -------------------------------------------------------------------
  
  #Arguments of the function are the model parameters that vary
  ModelVar <- function (rn,
                        BW,
                        CO,
                        MPPGL,
                        Vad,
                        Vbl,
                        Vrb,
                        Vbo,
                        Vbr,
                        Vgu,
                        Vheart,
                        Vki,
                        Vli,
                        Vlu,
                        Vpl,
                        Vmu,
                        Vsk,
                        Vsp,
                        Qad,
                        Qbo,
                        Qbr,
                        Qgu,
                        Qheart,
                        Qki,
                        Qh,
                        Qlu,
                        Qmu,
                        Qsk,
                        Qsp,
                        Vpf,
                        CYP2C8_H,
                        CYP2C9_H,
                        CYP2J2_H,
                        Vre,
                        Qre,
                        CYP1A2_L,
                        CYP2B6_L,
                        CYP2C8_L,
                        CYP2C9_L,
                        CYP2C19_L,
                        CYP2D6_L,
                        CYP3A4_L,
                        #CYP2C9_I,
                        #CYP2C19_I,
                        #CYP2D6_I,
                        #CYP3A4_I,
                        tlag,
                        Bioavailability,
                        #T_si,
                        FaFg,
                        t_end,
                        oral_dose, # as input$dose
                        fup,
                        fup_metab,
                        BP,
                        BP_metab,
                        MV,
                        MSA,
                        SA_pf){
    
    times <- seq(0, t_end, by = 0.1)

    # optimized parameters: Kpre_metab, fuha, fuhn, CLefflux, CLuptake
    Kpre_metab <- api_vals$Kpre_metab
    fuha <- api_vals$fuha
    fuhn <- api_vals$fuhn
    CLefflux <- study_vals$CLefflux
    CLuptake <- study_vals$CLuptake
    
    # PHYSICO-CHEMICAL PARAMETERS OF API AND ITS METABOLITE ------------------------------------------------------------------
    # API - parent compound
    MW_api <- api_vals$MW_api #[g/mol] Molecular weight
    pKa_api <- api_vals$MW_api # pKa [Cantu 2005]

    #METABOLITE
    MW_metab <- api_vals$MW_metab #[g/mol] [PubChem Compound Database; CID = 4543]
    pKa_metab <- api_vals$pKa_metab #pKa [Cantu 2005]
    #Absorption:
    PAMPA <- api_vals$PAMPA #[cm/s] [Oja 2015]
    #Distribution
    # TISSUE TO PLASMA PARTITION COEFFICIENT ----------------------------------
    #Calculated in Simcyp Simulator v.16, Method 2
    # API:
    Kpad = api_vals$Kpad# adipose
    Kpbo = api_vals$Kpbo# bone
    Kpbr = api_vals$Kpbr# brain
    Kpgu = api_vals$Kpgu# gut
    Kphe = api_vals$Kphe # heart
    Kppf = api_vals$Kppf #[Moriya 2000]; postmortem data
    Kpec = api_vals$Kpec #partition coefficient between plasma and extracellular fluid
    Kpki = api_vals$Kpki# kidney
    Kpli = api_vals$Kpli# liver
    Kplu = api_vals$Kplu# lung
    Kpmu = api_vals$Kpmu# muscle
    Kpsk = api_vals$Kpsk# skin
    Kpsp = api_vals$Kpsp# spleen
    Kpre = api_vals$Kpre #rest of the body
    # Metabolite TISSUE TO BLOOD PARTITION COEFFICIENT:
    Kpli_metab = api_vals$Kpli_metab / BP_metab# liver
    Kphe_metab = api_vals$Kphe_metab / BP_metab# heart
    #Drug binding
    # API:
    #Assumed. Possible to enter values if known.
    fu_pf = fup #fraction unbound in pericardial fluid; equality assumed. Unknown. proteins in PF < proteins in blood;
    fu_ht_api = api_vals$fu_ht_api #fraction of API unbound in heart tissue; Default - amitryptyline [Mikkelsen 2017]
    fu_ec = fu_pf #fraction unbound in extracellular fluid assumed to equal free fraction in pericardial fluid
    
    #Metabolism
    #metabolism kinetics
    #API (API)
    #LIVER (L)
    #N-demethylation
    #Vmax for API [pmol/min/pmol CYP] Default - amitryptyline
    #Km for API [mcM] Default - amitryptyline
    # 1.CYP1A2
    Vmax_1A2_api_dm <- api_vals$Vmax_1A2_api_dm 
    Km_1A2_api_dm <- api_vals$Km_1A2_api_dm 
    # 2.CYP2B6
    Vmax_2B6_api_dm <- api_vals$Vmax_2B6_api_dm 
    Km_2B6_api_dm <- api_vals$Km_2B6_api_dm 
    # 3.CYP2C8
    Vmax_2C8_api_dm <- api_vals$Vmax_2C8_api_dm 
    Km_2C8_api_dm <- api_vals$Km_2C8_api_dm 
    # 4.CYP2C9
    Vmax_2C9_api_dm <- api_vals$Vmax_2C9_api_dm 
    Km_2C9_api_dm <- api_vals$Km_2C9_api_dm 
    # 5.CYP2C19
    Vmax_2C19_api_dm <- api_vals$Vmax_2C19_api_dm 
    Km_2C19_api_dm <- api_vals$Km_2C19_api_dm 
    # 6.CYP2D6
    Vmax_2D6_api_dm <- api_vals$Vmax_2D6_api_dm 
    Km_2D6_api_dm <- api_vals$Km_2D6_api_dm 
    # 7.CYP3A4
    Vmax_3A4_api_dm <- api_vals$Vmax_3A4_api_dm 
    Km_3A4_api_dm <- api_vals$Km_3A4_api_dm 
    #E-10-hydroxylation:
    #1.CYP2B6
    Vmax_2B6_api_h <- api_vals$Vmax_2B6_api_h 
    Km_2B6_api_h <- api_vals$Km_2B6_api_h 
    #2.CYP2D6
    Vmax_2D6_api_h <- api_vals$Vmax_2D6_api_h 
    Km_2D6_api_h <- api_vals$Km_2D6_api_h 
    #3.CYP3A4
    Vmax_3A4_api_h <- api_vals$Vmax_3A4_api_h 
    Km_3A4_api_h <- api_vals$Km_3A4_api_h 
    
    
    # Metabolite (metab)
    #LIVER (L)
    #Vmax for Default nortryptyline after [Olesen and Linnet 1997] [mol/h/mol CYP]
    #Km for Default nortryptyline [mcmol/L]
    #demethylation
    # 1.CYP1A2
    Vmax_1A2_metab_dm <- api_vals$Vmax_1A2_metab_dm 
    Km_1A2_metab_dm <- api_vals$Km_1A2_metab_dm 
    # 2.CYP2C19
    Vmax_2C19_metab_dm <- api_vals$Vmax_2C19_metab_dm 
    Km_2C19_metab_dm <- api_vals$Km_2C19_metab_dm 
    # 3.CYP2D6
    Vmax_2D6_metab_dm <- api_vals$Vmax_2D6_metab_dm 
    Km_2D6_metab_dm <- api_vals$Km_2D6_metab_dm 
    #(E)-10-hydroxylation
    #1. CYP2D6
    Vmax_2D6_metab_h <- api_vals$Vmax_2D6_metab_h 
    Km_2D6_metab_h <- api_vals$Km_2D6_metab_h 
    
    
    # DOSING and ABSORPTION ------------------------------------------------------------------
    #IV Bolus Dose [mg]
    IVDOSE = 0
    #IV INFUSION RATE
    r <- study_vals$inf_dose# [mg]
    t <- study_vals$inf_time #time of infusion [h]
    inf = r / t #infusion rate [mg/h]
    #Oral Bolus Dose [mg]
    PODOSE <- study_vals$dose
    # DOSE REGIMENT
    no_doses <- study_vals$no_doses
    dose_every <- study_vals$dose_every
    dose <- study_vals$dose
    
    
    # ELIMINATION - Metabolite --------------------------------------------------------------
    #1) LIVER
    #ISEF
    #values from Simcyp. rCYP system: Lymph B
    ISEF1A2 <- study_vals$ISEF1A2
    ISEF2C19 <- study_vals$ISEF2C19
    ISEF2D6 <- study_vals$ISEF2D6
    ISEF2C9 <- study_vals$ISEF2C9
    ISEF3A4 <- study_vals$ISEF3A4
    ISEF2B6 <- study_vals$ISEF2B6
    ISEF2C8 <- study_vals$ISEF2C8

    #fumic
    fumic <- study_vals$fumic# [Venkatakrishnan 2001]
    fumic_metab <- study_vals$fumic_metab # the same value as for ami -> [McLure 2002]: api and metab the same ranges 0.3-0.7
    #2) KIDNEY
    CLrenal <- study_vals$CLrenal #CL renal [L/h]; [Karkkainen 1986]
    CLnonhep_metab <- study_vals$CLrenal_metab #[L/h] [Jornil 2011]
    
    
    # PHYSIOLOGICAL PARAMETERS ------------------------------------------------------------------
    #Tissue densities:
    liver_density <- study_vals$liver_density #[g/L]
    heart_density <- study_vals$heart_density #[g/L] [Kim 2005, Yan 2006]
    #Tissue volumes [L]
    #Full PBPK:
    Vhe = Vheart - Vpf #heart
    Vendo = 0.5 * Vhe #endocardium
    Vmid = 0.3 * Vhe #midmyocaridum
    Vepi = 0.2 * Vhe #epicardium
    Vendo_ic = 87.5 / 100 * Vendo #intracellular space of endocardium [Sjogaard 1982] -> data for skeletal muscle; calculated the proportion of extracellular water to total water in skeletal muscle
    Vepi_ic = 87.5 / 100 * Vepi #intracellular space of epicardium
    Vmid_ic = 87.5 / 100 * Vmid #intracellular space of midmyocardium
    Vhe_ec = 12.5 / 100 * Vhe #extracellular space of heart tissue
    Vve = (2 / 3) * Vbl		#venous blood; assumed 2/3 of total blood according to volmues published in CPT. Regarding the distribution of blood volume within the circulation, the greatest volume resides in the venous vasculature, where 70-80% of the blood volume is found. -> http://www.cvphysiology.com/Blood%20Pressure/BP019
    Var = Vbl - Vve		#arterial blood
    Vplas_ven = Vpl * (Vve / (Vve + Var))  #venous plasma
    Vplas_art = Vpl * (Var / (Vve + Var)) 	#arterial plasma
    #myocyte volume
    ML <- 134 #[mcm] myocyte length [Tracy 2011, Gerdes 1995]
    MB <- ML / 7 #=2r [mcm] myocyte breadth ML:MB = 7:1 [Gerdes 1995]
    MVol <-
      MV * (10 ^ -15) #[L] random age dependent myocate volume in mcm3 -> changing to liters
    #cells amounts
    cell_amount_epi <- Vepi_ic / MVol #epicardium
    cell_amount_mid <- Vmid_ic / MVol #midmyocardium
    cell_amount_endo <- Vendo_ic / MVol #endocardium
    #surface area
    SA_epi <- (cell_amount_epi * MSA) / (10 ^ 8) #[cm^2]
    SA_mid <- (cell_amount_mid * MSA) / (10 ^ 8) #[cm^2]
    SA_endo <- (cell_amount_endo * MSA) / (10 ^ 8) #[cm^2]
    
    #Minimum-PBPK:
    V1 = BW - (Var + Vve) - Vli - Vhe #rest of the body compartment
    # Tissue blood flows
    Qpf = study_vals$Qpf # pericardium: fitted in [Tylutki 2017]
    Qhe = Qheart - Qpf     #heart
    Qha = Qh - Qgu - Qsp #hepatic artery
    QC  = CO - Qh - Qhe #remaining compartment in minimum-PBPK
    # PARAMETERS FOR ICF and ECF in heart tissue
    pH_ic <- 7.2 #[Vaugha-Jones 2009, Zheng 2005] # Add to Tab STUDY
    pH_ec <- 7.4 #[Vaugha-Jones 2009, Zheng 2005] # Add to Tab STUDY
    pH_pf <- 7.57 #Hutchin 1971; SD=0.11 #11 patients with heart lesions; no pericardial effusion # Add to Tab STUDY
    #API - base;
    #Henderson_Hasselbalch equation for base compound -> fraction of un-ionized base in heart compartments:
    funionized_ic <- 1 / (1 + 10 ^ (pKa_api - pH_ic)) 
    funionized_ec <- 1 / (1 + 10 ^ (pKa_api - pH_ec))
    funionized_pf <- 1 / (1 + 10 ^ (pKa_api - pH_pf))
    
    
    # OTHER PARAMS ------------------------------------------------------------------
    
    #Absorption:
    #PAMPA <- 12.3 * 10 ^ -6 #[cm/s] [Oja 2015]
    Pdiff_api <- api_vals$PAMPA #[cm/s]

    #metabolism kinetics
    # API (A)
    # LIVER (L)
    #N-demethylation
    #Vmax for API after (Default for amitryptyline)[Venkatakrishnan 2001] [pmol/min/pmol CYP]
    #Km for API (Default for amitryptyline) [mcM]
    # 1.CYP1A2
    V_1A2_api <- Vmax_1A2_api_dm * MW_api * 10 ^ -9 #[mg/min/pmol CYP]
    K_1A2_api <- Km_1A2_api_dm * MW_api * 10 ^ -3 #[mg/L]
    # 2.CYP2B6
    V_2B6_api <- Vmax_2B6_api_dm * MW_api * 10 ^ -9 #[mg/min/pmol CYP]
    K_2B6_api <- Km_2B6_api_dm * MW_api * 10 ^ -3 #[mg/L]
    # 3.CYP2C8
    V_2C8_api <- Vmax_2C8_api_dm * MW_api * 10 ^ -9 #[mg/min/pmol CYP]
    K_2C8_api <- Km_2C8_api_dm * MW_api * 10 ^ -3 #[mg/L]
    # 4.CYP2C9
    V_2C9_api <- Vmax_2C9_api_dm * MW_api * 10 ^ -9 #[mg/min/pmol CYP]
    K_2C9_api <- Km_2C9_api_dm * MW_api * 10 ^ -3 #[mg/L]
    # 5.CYP2C19
    V_2C19_api <- Vmax_2C19_api_dm * MW_api * 10 ^ -9 #[mg/min/pmol CYP]
    K_2C19_api <- Km_2C19_api_dm * MW_api * 10 ^ -3 #[mg/L]
    # 6.CYP2D6
    V_2D6_api <- Vmax_2D6_api_dm * MW_api * 10 ^ -9 #[mg/min/pmol CYP]
    K_2D6_api <- Km_2D6_api_dm * MW_api * 10 ^ -3 #[mg/L]
    # 7.CYP3A4
    V_3A4_api <- Vmax_3A4_api_dm * MW_api * 10 ^ -9 #[mg/min/pmol CYP]#[Ghahramani 1997]
    K_3A4_api <- Km_3A4_api_dm * MW_api * 10 ^ -3 #[mg/L]
    #E-10-hydroxylation:
    #1.CYP2B6
    V_2B6_api_h <- Vmax_2B6_api_h * MW_api * 10 ^ -9 #[mg/min/pmol CYP]
    K_2B6_api_h <- Km_2B6_api_h * MW_api * 10 ^ -3 #[mg/L]
    #2.CYP2D6
    V_2D6_api_h <- Vmax_2D6_api_h * MW_api * 10 ^ -9 #[mg/min/pmol CYP]
    K_2D6_api_h <- Km_2D6_api_h * MW_api * 10 ^ -3 #[mg/L]
    #3.CYP3A4
    V_3A4_api_h <- Vmax_3A4_api_h * MW_api * 10 ^ -9 #[mg/min/pmol CYP]
    K_3A4_api_h <- Vmax_3A4_api_h * MW_api * 10 ^ -3 #[mg/L]

    # DISTRIBUTION ---------------------------------------------------------
    #Drug binding

    #passive permeability surface area product in heart tissue
    PSA_pf = Pdiff_api * SA_pf * (10 ^ -3) * 60 * 60 #[L/h] passive permeability surface area product
    PSA_epi = Pdiff_api * SA_epi * (10 ^ -3) * 60 * 60 #[L/h] passive permeability surface area product
    PSA_mid = Pdiff_api * SA_mid * (10 ^ -3) * 60 * 60 #[L/h] passive permeability surface area product
    PSA_endo = Pdiff_api * SA_endo * (10 ^ -3) * 60 * 60 #[L/h] passive permeability surface area product

    #3) HEART
    CYP3A4_H <- 0 #CYP 3A4 abundance in average human heart [pmol/mg tissue][Thum 2000]

    # MODEL -------------------------------------------------------------------
    parameters <- c(
      BP = BP,
      Kpad = Kpad,
      Kpbo = Kpbo,
      Kpbr = Kpbr,
      Kpgu = Kpgu,
      Vendo = Vendo,
      Vmid = Vmid,
      Vepi = Vepi,
      Kppf = Kppf,
      Kpki = Kpki,
      CLrenal = CLrenal,
      fup = fup,
      Qgu = Qgu,
      Kpgu = Kpgu,
      Kpsp = Kpsp,
      Kpli = Kpli,
      Kplu = Kplu,
      Kpmu = Kpmu,
      Kpsk = Kpsk,
      PODOSE = PODOSE,
      funionized_ic = funionized_ic,
      funionized_ec = funionized_ec,
      funionized_pf = funionized_pf,
      fu_ec = fu_ec,
      PSA_pf = PSA_pf,
      PSA_epi = PSA_epi,
      PSA_mid = PSA_mid,
      PSA_endo = PSA_endo,
      Kpec = Kpec
    )
    
    # State variables
    state <- c(
      INFUSION = r,
      Aad = 0,
      Abo = 0,
      Abr = 0,
      Agu = 0,
      Ahe_ec = 0,
      Aepi = 0,
      Amid = 0,
      Aendo = 0,
      Apf = 0,
      Aki = 0,
      Ali = 0,
      Alu = 0,
      Amu = 0,
      Ask = 0,
      Asp = 0,
      Ave = IVDOSE,
      Aar = 0,
      Are = 0,
      D = PODOSE * Bioavailability,
      METAB = PODOSE * (1 - Bioavailability/FaFg) * MW_metab/MW_api * FaFg,
      Ali_metab = 0,
      Cbl_metab = 0,
      Cre_metab = 0,
      Che_metab = 0
    )
    ###Differential equations - mg/hr
    PBPKModel = function(times, state, parm) {
      with(as.list(c(state, parm)), {
        inf <- ifelse(times <= t, inf, 0)

        F_Dose <-
          ifelse(times < tlag, 0, pop_vals$ka * D ) #
        
        Metab_formed <- 
          ifelse(times < tlag, 0, pop_vals$ka * METAB)

        # API concentrations:
        Cadipose <- Aad / Vad    #adipose
        Cbone <- Abo / Vbo		   #bone
        Cbrain <- Abr / Vbr		   #brain
        Cgut <- Agu / Vgu			   #gut
        Cheart_ec <- Ahe_ec / Vhe_ec #cardiac extracellular fluid
        Cendo <- Aendo / Vendo   #endocardium
        Cmid <- Amid / Vmid      #miocardium
        Cepi <- Aepi / Vepi      #epicardium
        Cpf <- Apf / Vpf         #pericardial fluid
        Ckidney <- Aki / Vki	   #kidney
        Cliver <- Ali / Vli		   #liver
        Clung <- Alu / Vlu		   #lung
        Cmuscle <- Amu / Vmu	   #muscle
        Cskin <- Ask / Vsk		   #skin
        Cspleen <- Asp / Vsp	   #spleen
        Cvenous <- Ave / Vve     #venous blood
        Carterial <- Aar / Var	 #arterial blood
        Crest <- Are / Vre 			#rest of body
        Cplasmavenous <- Cvenous / BP	#venous plasma concentration
        Cliverfree <- Cliver * (fup / BP) * fuha  #liver - free concentration
        Ckidneyfree <- Ckidney * (fup / BP)	 #kidney - free concentration
        #METABTRIPTYLINE Concentrations
        Cliver_metab <- Ali_metab / Vli #liver
        Cliverfree_metab = Cliver_metab * (fup_metab / BP_metab) * fuhn #liver - free concentration for nortriptyline
        Cplasma_metab = Cbl_metab / BP_metab #venous plasma nortriptyline concentration
        
        
        # metabolism
        # API (A)
        # LIVER (L)
        # N-demethylation
        # Vmax for API (Default amitriptyline) after [Venkatakrishnan 2001] [pmol/min/pmol CYP]
        # Km for API (Default amitriptyline) [mcM]
        # 1.CYP1A2
        CLint_1A2 <- (ISEF1A2 * (V_1A2_api / (K_1A2_api + Cliverfree)) * CYP1A2_L) / fumic  #[L/min/mg of microsomal protein]
        # 2.CYP2B6
        CLint_2B6 <- (ISEF2B6 * (V_2B6_api / (K_2B6_api + Cliverfree)) * CYP2B6_L) / fumic  #[L/min/mg of microsomal protein]
        # 3.CYP2C8
        CLint_2C8 <- (ISEF2C8 * (V_2C8_api / (K_2C8_api + Cliverfree)) * CYP2C8_L) / fumic  #[L/min/mg of microsomal protein]
        # 4.CYP2C9
        CLint_2C9 <- (ISEF2C9 * (V_2C9_api / (K_2C9_api + Cliverfree)) * CYP2C9_L) / fumic  #[L/min/mg of microsomal protein]
        # 5.CYP2C19
        CLint_2C19 <- (ISEF2C19 * (V_2C19_api / (K_2C19_api + Cliverfree)) * CYP2C19_L) / fumic  #[L/min/mg of microsomal protein]
        # 6.CYP2D6
        CLint_2D6 <- (ISEF2D6 * (V_2D6_api / (K_2D6_api + Cliverfree)) * CYP2D6_L) / fumic  #[L/min/mg of microsomal protein]
        # 7.CYP3A4
        CLint_3A4 <- (ISEF3A4 * (V_3A4_api / (K_3A4_api + Cliverfree)) * CYP3A4_L) / fumic  #[L/min/mg of microsomal protein]
        #sum of intrinsic clearances for demethylation for all CYPs isoforms
        CLint_demethylation_L <- (CLint_1A2 + CLint_2B6 + CLint_2C8 + CLint_2C9 + CLint_2C19 + CLint_2D6 + CLint_3A4)*60 #[L/h/mg of microsomal protein]
        
        #E-10-hydroxylation:
        #1.CYP2B6
        CLint_2B6_api_h <- (ISEF2B6 * (V_2B6_api_h / (K_2B6_api_h + Cliverfree)) * CYP2B6_L) / fumic  #[L/min/mg of microsomal protein]
        #2.CYP2D6
        CLint_2D6_api_h <- (ISEF2D6 * (V_2D6_api_h / (K_2D6_api_h + Cliverfree)) * CYP2D6_L) / fumic  #[L/min/mg of microsomal protein]
        #3.CYP3A4
        CLint_3A4_api_h <- (ISEF3A4 * (V_3A4_api_h / (K_3A4_api_h + Cliverfree)) * CYP3A4_L) / fumic  #[L/min/mg of microsomal protein]
        #sum of intrinsic clearances for demethylation for all CYPs isoforms
        CLint_hydroxylation_L <- (CLint_2B6_api_h + CLint_2D6_api_h + CLint_3A4_api_h) * 60 #[L/h/mg of microsomal protein]
        
        #Hepatic intrinsic clearance:
        CLint_api_L <- (CLint_demethylation_L + CLint_hydroxylation_L) * MPPGL * Vli * liver_density #[L/h]
        
        #HEART
        #no scalar assumed in heart tissue
        #Vmax and Km for liver microsomes
        # 1.CYP2C8
        CLint_2C8_H <- ((V_2C8_api / K_2C8_api) * CYP2C8_H) / fumic  #[L/min/mg tissue]
        # 2.CYP2C9
        CLint_2C9_H <- ((V_2C9_api / K_2C9_api) * CYP2C9_H) / fumic  #[L/min/mg tissue]
        # 3.CYP2J2
        CLint_2J2_H <-  0 * CYP2J2_H #[L/min/mg tissue] - why 0?
        # 4.CYP3A4
        CLint_3A4_H <-  ((V_3A4_api / K_3A4_api) * CYP3A4_H) / fumic
        #sum of intrinsic clearances for demethylation for all CYPs isoforms
        CLint_demethylation_H <- (CLint_2C8_H + CLint_2C9_H + CLint_2J2_H + CLint_3A4_H) * 60 #[L/h/mg tissue]
        #Cardiac intrinsic clearance:
        CLint_api_H <- CLint_demethylation_H * Vhe * heart_density * 10 ^ 3  #[L/h]
        
        
        # Metabolite (metab)
        # LIVER (L)
        # Vmax (Default for Nortryptyline) after [Olesen and Linnet 1997] [mol/h/mol CYP]
        # Km (Default for Nortryptyline) [mcmol/L]
        # demethylation
        # 1.CYP1A2
        V_1A2_metab <- Vmax_1A2_metab_dm * MW_metab * 10 ^ -9 #[mg/h/pmol CYP]
        K_1A2_metab <- Km_1A2_metab_dm * MW_metab * 10 ^ -3 #[mg/L]
        CLint_1A2_metab <-
          (ISEF1A2 * (V_1A2_metab / (K_1A2_metab + Cliverfree_metab)) * CYP1A2_L) / fumic_metab  #[L/h/mg of microsomal protein]
        # 2.CYP2C19
        V_2C19_metab <- Vmax_2C19_metab_dm * MW_metab * 10 ^ -9 #[mg/h/pmol CYP]
        K_2C19_metab <- Km_2C19_metab_dm * MW_metab * 10 ^ -3 #[mg/L]
        CLint_2C19_metab <- (ISEF2B6 * (V_2C19_metab / (K_2C19_metab + Cliverfree_metab)) * CYP2C19_L)/fumic_metab  #[L/h/mg of microsomal protein]
        # 3.CYP2D6
        V_2D6_metab_d <- Vmax_2D6_metab_dm * MW_metab * 10 ^ -9 #[mg/h/pmol CYP]
        K_2D6_metab_d <- Km_2D6_metab_dm * MW_metab * 10 ^ -3 #[mg/L]
        CLint_2D6_metab_d <- (ISEF2C8 * (V_2D6_metab_d / (K_2D6_metab_d + Cliverfree_metab)) * CYP2D6_L)/fumic_metab  #[L/h/mg of microsomal protein]
        #sum of intrinsic clearances for demethylation for all CYPs isoforms
        CLint_demethylation_metab_L <- CLint_1A2_metab + CLint_2C19_metab + CLint_2D6_metab_d #[L/h/mg of microsomal protein]
        #(E)-10-hydroxylation
        #1. CYP2D6
        V_2D6_metab_h <- Vmax_2D6_metab_h * MW_metab * 10 ^ -9 #[mg/h/pmol CYP]
        K_2D6_metab_h <- Km_2D6_metab_h * MW_metab * 10 ^ -3 #[mg/L]
        CLint_2D6_metab_h <- (ISEF2C8 * (V_2D6_metab_h / (K_2D6_metab_h + Cliverfree_metab)) * CYP2D6_L)/fumic_metab  #[L/h/mg of microsomal protein]
        #sum of intrinsic clearances for hydroxylation for all CYPs isoforms
        CLint_hydroxylation_metab_L <- CLint_2D6_metab_h #[L/h/mg of microsomal protein]
        
        #Hepatic intrinsic clearance:
        CLint_metab_L <- (CLint_demethylation_metab_L + CLint_hydroxylation_metab_L) * MPPGL * Vli * liver_density #[L/h]
        
        #HEART (H)
        #assumed no metabolism of nortriptyline in heart tissue
        
        
        Venous <-  F_Dose + Qpf * (Cpf / Kppf) + Qad * (Cadipose / Kpad * BP) + Qbo * (Cbone / Kpbo * BP) + Qbr * (Cbrain / Kpbr * BP) + Qhe * (Cheart_ec / Kpec * BP) + Qki * (Ckidney / Kpki * BP) + Qh * (Cliver / Kpli * BP)  + Qmu * (Cmuscle / Kpmu * BP) + Qsk * (Cskin / Kpsk * BP) + Qre * (Crest / Kpre * BP)
        ## rates of changes
        dINFUSION <- -inf
        dAad <- Qad * (Carterial - Cadipose / Kpad * BP) #adipose
        dAbo <- Qbo * (Carterial - Cbone / Kpbo * BP) #bone
        dAbr <- Qbr * (Carterial - Cbrain / Kpbr * BP) #brain
        dAgu <-  Qgu * (Carterial - Cgut / Kpgu * BP) #gut
        #
        #heart tissue:
        dAhe_ec <- Qhe * (Carterial - Cheart_ec / Kpec * BP) + PSA_pf * ((Cpf * fu_pf * funionized_pf) - (Cheart_ec * fu_ec * funionized_ec)) + PSA_epi * ((Cepi * fu_ht_api * funionized_ic) - (Cheart_ec * fu_ec * funionized_ec)) + PSA_mid * ((Cmid * fu_ht_api * funionized_ic) - (Cheart_ec * fu_ec * funionized_ec)) + PSA_endo * ((Cendo * fu_ht_api * funionized_ic) - (Cheart_ec * fu_ec * funionized_ec)) + (CLefflux * fu_ht_api * Cepi) - (CLuptake * fu_ec * Cheart_ec) + (CLefflux * fu_ht_api * Cmid) - (CLuptake * fu_ec * Cheart_ec) + (CLefflux * fu_ht_api * Cendo) - (CLuptake * fu_ec * Cheart_ec)
        dAepi <- PSA_epi * ((Cheart_ec * fu_ec * funionized_ec) - (Cepi * fu_ht_api * funionized_ic)) + (CLuptake * fu_ec * Cheart_ec) - (CLefflux * fu_ht_api * Cepi) - (Cepi * fu_ht_api  * (Vepi /
                                                                                                                                                                                  Vhe) * CLint_api_H)
        dAmid <- PSA_mid * ((Cheart_ec * fu_ec * funionized_ec) - (Cmid * fu_ht_api * funionized_ic)) + (CLuptake * fu_ec * Cheart_ec) - (CLefflux * fu_ht_api * Cmid) - (Cmid * fu_ht_api  * (Vmid /
                                                                                                                                                                                  Vhe) * CLint_api_H)
        dAendo <- PSA_endo * ((Cheart_ec * fu_ec * funionized_ec) - (Cendo * fu_ht_api * funionized_ic)) + (CLuptake * fu_ec * Cheart_ec) - (CLefflux * fu_ht_api * Cendo) - (Cendo * fu_ht_api * (Vendo /
                                                                                                                                                                                     Vhe) * CLint_api_H)
        dApf <- Qpf * (Carterial - Cpf / Kppf) + PSA_pf * ((Cheart_ec * fu_ec * funionized_ec) - (Cpf * fu_pf * funionized_pf))
        #
        dAki <- Qki * (Carterial - Ckidney / Kpki * BP) - CLrenal * Ckidneyfree  #kidney

        dAli <- Qha * Carterial + Qgu * (Cgut / Kpgu * BP) + Qsp * (Cspleen / Kpsp * BP) - Qh * (Cliver / Kpli * BP) - Cliverfree * CLint_api_L #liver
        dAlu <- Qlu * Cvenous - Qlu * (Clung / Kplu * BP) #lung
        dAmu <- Qmu * (Carterial - Cmuscle / Kpmu * BP)   #muscle
        dAsk <- Qsk * (Carterial - Cskin / Kpsk * BP)  		#skin
        dAsp <- Qsp * (Carterial - Cspleen / Kpsp * BP)  	#spleen
        dAve <- inf + Venous - Qlu * Cvenous   				#venous blood
        dAar <- Qlu * (Clung / Kplu * BP) - Qlu * Carterial  		#arterial blood
        dAre <- Qre * (Carterial - Crest / Kpre * BP)  			#rest of body
        dD <- -F_Dose  			#oral dosing
        dMETAB <- -Metab_formed

        #mPBPK
        dAli_metab <- Metab_formed + (MW_metab / MW_api) * (Cliverfree * CLint_demethylation_L * MPPGL * Vli * liver_density) + Qh *  Cbl_metab - Cliver_metab / Kpli_metab * Qh - Cliverfree_metab * CLint_metab_L
        dCbl_metab <- (((Cliver_metab / Kpli_metab) * Qh + QC * Cre_metab / Kpre_metab + Qhe * Che_metab / Kphe_metab - (QC + Qhe + Qh + CLnonhep_metab) * Cbl_metab)) / (Var + Vve)
        dCre_metab <- ((QC * Cbl_metab - QC * Cre_metab / Kpre_metab)) / V1
        # dCre_metab <- ((MW_metab/MW_api)*(Cgut*fugut_api*CLuint_gut) +  (QC * Cbl_metab - QC * Cre_metab / x[1])) / V1
        dChe_metab <- ((MW_metab / MW_api) * ((Cepi * fu_ht_api * (Vepi / Vhe) * CLint_api_H) + (Cmid * fu_ht_api * (Vmid / Vhe) * CLint_api_H) + (Cendo * fu_ht_api * (Vendo / Vhe) * CLint_api_H)) + (Qhe * Cbl_metab - Qhe * Che_metab / Kphe_metab)) / Vhe
        
        #return the rate of changes
        res <- list(
          c(
            dINFUSION,
            dAad,
            dAbo,
            dAbr,
            dAgu,
            dAhe_ec,
            dAepi,
            dAmid,
            dAendo,
            dApf,
            dAki,
            dAli,
            dAlu,
            dAmu,
            dAsk,
            dAsp,
            dAve,
            dAar,
            dAre,
            dD,
            dMETAB,
            dAli_metab,
            dCbl_metab,
            dCre_metab,
            dChe_metab
          ),
          BL = Cplasmavenous,
          BL_METAB = Cplasma_metab,
          HT = (Ahe_ec + Aepi + Amid + Aendo) / Vhe,
          MID = (Amid / Vmid_ic),
          ENDO = (Aendo) / Vendo_ic,
          EPI = (Aepi) / Vepi_ic,
          EC = Cheart_ec,
          HT_METAB = Che_metab,
          logHT = log10((Ahe_ec + Aepi + Amid + Aendo) / Vhe),
          logBL = log10(Cplasmavenous),
          logBL_METAB = log10(Cplasma_metab),
          DOSE = D,
          A_METAB = METAB
        )
        
        return(res)
      })
    }
    

    eventdat <- data.frame(
      var = c(rep("D", times = no_doses), rep("METAB", times = no_doses)),
      time = c(seq(0 + tlag, (no_doses - 1) * dose_every + tlag, by = dose_every),
               seq(0 + tlag, (no_doses - 1) * dose_every + tlag, by = dose_every)),
      value = c(
        rep(dose * Bioavailability, times = no_doses),
        rep(dose * (1 - Bioavailability / FaFg) * FaFg * MW_metab / MW_api, times = no_doses)
      ),
      method = rep("add", times = no_doses * 2)
    )
  
    out <-
      ode(
        y = state,
        times = times,
        func = PBPKModel,
        parm = parameters,
        events = list(data = eventdat)
      )

    results <- data.frame(rn=rn,out)
    
    return(results)
  } 

  #
  # Population plots START ---------------------------------
  #

  observeEvent(pop3(),{
    pop_data_table <- pop3()$x$data
    
    print(pop_data_table)
    
    output$pop_age <- renderPlot({
      
      ggplot(pop_data_table, aes(x=age, colour=sex)) +
        geom_histogram(fill="lightgrey",alpha=.5, position = "identity", binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3))) +
        theme_classic() +
        theme(
          panel.grid.major = element_line(colour="grey",size = rel(0.5)),
          panel.grid.minor = element_blank(),
          axis.title = element_text(size = 13),
          axis.text = element_text(size = 13),
          legend.text = element_text(size = 11),
          legend.title = element_text(size = 11),
          legend.position = "right"
        )
      
    })
    
    output$pop_weight <- renderPlot({
      
      ggplot(pop_data_table, aes(x=weight, colour=sex)) +
        geom_histogram(fill="lightgrey",alpha=.5, position = "identity", binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3))) +
        theme_classic() +
        theme(
          panel.grid.major = element_line(colour="grey",size = rel(0.5)),
          panel.grid.minor = element_blank(),
          axis.title = element_text(size = 13),
          axis.text = element_text(size = 13),
          legend.text = element_text(size = 11),
          legend.title = element_text(size = 11),
          legend.position = "right"
        )
      
    })
    
    output$pop_height <- renderPlot({
      
      ggplot(pop_data_table, aes(x=height, colour=sex)) +
        geom_histogram(fill="lightgrey",alpha=.5, position = "identity", binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3))) +
        theme_classic() +
        theme(
          panel.grid.major = element_line(colour="grey",size = rel(0.5)),
          panel.grid.minor = element_blank(),
          axis.title = element_text(size = 13),
          axis.text = element_text(size = 13),
          legend.text = element_text(size = 11),
          legend.title = element_text(size = 11),
          legend.position = "right"
        )

    })
    
    output$pop_mean_CO_Tanner <- renderPlot({
      
      ggplot(pop_data_table, aes(x=mean.CO.Tanner.formula..L.h., colour=sex)) +
        geom_histogram(fill="lightgrey",alpha=.5, position = "identity", binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3))) +
        theme_classic() +
        theme(
          panel.grid.major = element_line(colour="grey",size = rel(0.5)),
          panel.grid.minor = element_blank(),
          axis.title = element_text(size = 13),
          axis.text = element_text(size = 13),
          legend.text = element_text(size = 11),
          legend.title = element_text(size = 11),
          legend.position = "right"
        )
      
    })

  })

  #
  # Population plots END ------------------------------------
  #
  
  #
  # Reactive population plots START -------------------------
  #
  
  plot_pop_age <- reactive({
    df <- pop3()$x$data
    ggplot(df, aes(x=age, colour=sex)) +
      geom_histogram(fill="lightgrey",alpha=.5, position = "identity", binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3))) +
      theme_classic() +
      theme(
        panel.grid.major = element_line(colour="grey",size = rel(0.5)),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 13),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 11),
        legend.position = "right"
      )
    
  })
  
  plot_pop_weight <- reactive({
    df <- pop3()$x$data
    ggplot(df, aes(x=weight, colour=sex)) +
      geom_histogram(fill="lightgrey",alpha=.5, position = "identity", binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3))) +
      theme_classic() +
      theme(
        panel.grid.major = element_line(colour="grey",size = rel(0.5)),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 13),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 11),
        legend.position = "right"
      )
    
  })
  
  plot_pop_height <- reactive({
    df <- pop3()$x$data
    ggplot(df, aes(x=height, colour=sex)) +
      geom_histogram(fill="lightgrey",alpha=.5, position = "identity", binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3))) +
      theme_classic() +
      theme(
        panel.grid.major = element_line(colour="grey",size = rel(0.5)),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 13),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 11),
        legend.position = "right"
      )
    
  })
  
  plot_pop_mean_CO_Tanner <- reactive({
    df <- pop3()$x$data
    ggplot(df, aes(x=mean.CO.Tanner.formula..L.h., colour=sex)) +
      geom_histogram(fill="lightgrey",alpha=.5, position = "identity", binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3))) +
      theme_classic() +
      theme(
        panel.grid.major = element_line(colour="grey",size = rel(0.5)),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 13),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 11),
        legend.position = "right"
      )
    
  })
  
  #
  # Reactive population plots END -------------------------
  #
  
  #
  # Functions results plots START --------------------------
  #
  
  plot_res_log_conc_in_venous_plasma <- function(data_to_plot){
    
    ggplot(data_to_plot, aes(time, BL, colour = rn)) +
      geom_line(size = 1) +
      theme_classic() +
      scale_y_log10() + # correct way of presenting the log10 values?
     theme(
      panel.grid.major = element_line(colour="grey",size = rel(0.5)),
      panel.grid.minor = element_blank(),
      axis.title = element_text(size = 13),
      axis.text = element_text(size = 13),
      legend.text = element_text(size = 11),
      legend.title = element_text(size = 11),
      legend.position = "right"
    ) +
      ggtitle(paste("Log 10 concentration vs. time for ",input$api_plot_caption, sep="")) +
      labs(
          x = "Time [h]",
          y = "log 10 concentration in venous plasma [mg/L]",
          colour = "Individuals"
      )
  }
  
  plot_res_conc_in_venous_plasma <- function(data_to_plot){
    ggplot(data_to_plot, aes(time, BL, colour = rn)) +
      geom_line(size = 1) +
      theme_classic() +
      theme(
        panel.grid.major = element_line(colour="grey",size = rel(0.5)),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 13),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 11),
        legend.position = "right"
      ) +
      ggtitle(paste("Concentration vs. time for ",input$api_plot_caption, sep="")) +
      labs(
          x = "Time [h]",
          y = "Concentration in venous plasma [mg/L]",
          colour = "Individuals"
      )
  }
  
  plot_res_conc_metab_in_venous_plasma <- function(data_to_plot){
    ggplot(data_to_plot, aes(time, BL_METAB, colour = rn)) +
      geom_line(size = 1) +
      theme_classic() +
      theme(
        panel.grid.major = element_line(colour="grey",size = rel(0.5)),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 13),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 11),
        legend.position = "right"
      ) +
      ggtitle(paste("Concentration vs. time for ",input$metab_plot_caption, sep="")) +
      labs(
          x = "Time [h]",
          y = "Concentration in venous plasma [mg/L]",
          colour = "Individuals"
      )
  }
  
  plot_res_conc_in_heart <- function(data_to_plot){
    ggplot(data_to_plot, aes(time, HT, colour = rn)) +
      geom_line(size = 1) +
      theme_classic() +
      theme(
        panel.grid.major = element_line(colour="grey",size = rel(0.5)),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 13),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 11),
        legend.position = "right"
      ) +
      ggtitle(paste("Concentration vs. time for ",input$api_plot_caption, sep="")) +
      labs(
          x = "Time [h]",
          y = "Concentration in heart tissue [mg/L]",
          colour = "Individuals"
      )
  }
  
  plot_res_conc_metab_in_heart <- function(data_to_plot){
    ggplot(data_to_plot, aes(time, HT_METAB, colour = rn)) +
      geom_line(size = 1) +
      theme_classic() +
      theme(
        panel.grid.major = element_line(colour="grey",size = rel(0.5)),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 13),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 11),
        legend.position = "right"
      ) +
      ggtitle(paste("Concentration vs. time for ",input$metab_plot_caption, sep="")) +
      labs(
          x = "Time [h]",
          y = "Concentration in heart tissue [mg/L]",
          colour = "Individuals"
      )
  }
  
  
  ######## Stats res plots
  
  plot_stats_res_conc_in_venous_plasma <- function(data_to_plot){
    
    CI_level <- input$downloadPlot_stats_res_conc_in_venous_plasma_CI
    
    CI_low <- (1-(CI_level/100))/2
    CI_high <- 1 - ((1- (CI_level/100))/2)
    
    stats_iv <- plyr::ddply(data_to_plot, .(time), function(data_to_plot) statFunc(data_to_plot$BL,CI_low, CI_high))
    
    ggplot(data=stats_iv, aes(x = time, y = median), colour = "red", alpha = 0.8) +
      geom_line(size = 1) +
      theme_classic() +
      theme(
        panel.grid.major = element_line(colour="grey",size = rel(0.5)),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 13),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 11),
        legend.position = "right"
      ) +
      ggtitle(paste("Concentration vs. time for ",input$api_plot_caption, sep="")) +
      labs(
          x = "Time [h]",
          y = "Concentration in venous plasma [mg/L]",
          colour = "Individuals"
      ) + geom_ribbon(data= stats_iv, aes(x = time, ymin = lower_CI, ymax = higher_CI), fill="red", color = "red", alpha = 0.3)
    
  }
  
  
  plot_stats_res_conc_metab_in_venous_plasma <- function(data_to_plot){
    
    CI_level <- input$downloadPlot_stats_res_conc_metab_in_venous_plasma_CI
    
    CI_low <- (1-(CI_level/100))/2
    CI_high <- 1 - ((1- (CI_level/100))/2)
    
    stats_iv <- plyr::ddply(data_to_plot, .(time), function(data_to_plot) statFunc(data_to_plot$BL_METAB,CI_low, CI_high))
    
    ggplot(data=stats_iv, aes(x = time, y = median), colour = "red", alpha = 0.8) +
      geom_line(size = 1) +
      theme_classic() +
      theme(
        panel.grid.major = element_line(colour="grey",size = rel(0.5)),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 13),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 11),
        legend.position = "right"
      ) +
      ggtitle(paste("Concentration vs. time for ",input$metab_plot_caption, sep="")) +
      labs(
          x = "Time [h]",
          y = "Concentration in venous plasma [mg/L]",
          colour = "Individuals"
      ) + geom_ribbon(data= stats_iv, aes(x = time, ymin = lower_CI, ymax = higher_CI), fill="red", color = "red", alpha = 0.3)
  }
  
  plot_stats_res_log_conc_in_venous_plasma <- function(data_to_plot){
    
    CI_level <- input$downloadPlot_stats_res_log_conc_in_venous_plasma_CI
    
    CI_low <- (1-(CI_level/100))/2
    CI_high <- 1 - ((1- (CI_level/100))/2)
    
    stats_iv <- plyr::ddply(data_to_plot, .(time), function(data_to_plot) statFunc(data_to_plot$BL,CI_low, CI_high))
    
    ggplot(data=stats_iv, aes(x = time, y = median), colour = "red", alpha = 0.8) +
      geom_line(size = 1) +
      theme_classic() +
      scale_y_log10() + # correct way of presenting the log10 values
      theme(
        panel.grid.major = element_line(colour="grey",size = rel(0.5)),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 13),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 11),
        legend.position = "right"
      ) +
      ggtitle(paste("Concentration vs. time for ",input$api_plot_caption, sep="")) +
      labs(
          x = "Time [h]",
          y = "Log 10 Concentration in venous plasma [mg/L]",
          colour = "Individuals"
      ) + geom_ribbon(data= stats_iv, aes(x = time, ymin = lower_CI, ymax = higher_CI), fill="red", color = "red", alpha = 0.3)
  }
  
  plot_stats_res_conc_in_heart <- function(data_to_plot){
    
    CI_level <- input$downloadPlot_stats_res_conc_in_heart_CI
    
    CI_low <- (1-(CI_level/100))/2
    CI_high <- 1 - ((1- (CI_level/100))/2)
    
    stats_iv <- plyr::ddply(data_to_plot, .(time), function(data_to_plot) statFunc(data_to_plot$HT,CI_low, CI_high))
    
    ggplot(data=stats_iv, aes(x = time, y = median), colour = "red", alpha = 0.8) +
      geom_line(size = 1) +
      theme_classic() +
      theme(
        panel.grid.major = element_line(colour="grey",size = rel(0.5)),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 13),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 11),
        legend.position = "right"
      ) +
      ggtitle(paste("Concentration vs. time for ",input$api_plot_caption, sep="")) +
      labs(
          x = "Time [h]",
          y = "Concentration in heart [mg/L]",
          colour = "Individuals"
      ) + geom_ribbon(data= stats_iv, aes(x = time, ymin = lower_CI, ymax = higher_CI), fill="red", color = "red", alpha = 0.3)
  }
  
  
  plot_stats_res_conc_metab_in_heart <- function(data_to_plot){
    
    CI_level <- input$downloadPlot_stats_res_conc_metab_in_heart_CI
    
    CI_low <- (1-(CI_level/100))/2
    CI_high <- 1 - ((1- (CI_level/100))/2)
    
    stats_iv <- plyr::ddply(data_to_plot, .(time), function(data_to_plot) statFunc(data_to_plot$HT_METAB, CI_low, CI_high))
    
    ggplot(data=stats_iv, aes(x = time, y = median), colour = "red", alpha = 0.8) +
      geom_line(size = 1) +
      theme_classic() +
      theme(
        panel.grid.major = element_line(colour="grey",size = rel(0.5)),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 13),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 11),
        legend.position = "right"
      ) +
      ggtitle(paste("Concentration vs. time for ",input$metab_plot_caption, sep="")) +
      labs(
          x = "Time [h]",
          y = "Concentration of metabolite in heart [mg/L]",
          colour = "Individuals"
      ) + geom_ribbon(data= stats_iv, aes(x = time, ymin = lower_CI, ymax = higher_CI), fill="red", color = "red", alpha = 0.3)
  }
  
  #
  # Functions results plots END ----------------------------
  #
  

  
  #
  # Download plots helper functions  START---------------
  #
  
  output$downloadPlot_pop_age <- downloadHandler(
    filename = function() { paste(input$pop_age, 'pop_plot_1.png', sep='') },
    content = function(file) {
      ggsave(file, plot = plot_pop_age(), units = "mm", width = input$downloadPlot_pop_age_width,
             height = input$downloadPlot_pop_age_height, dpi = input$downloadPlot_pop_age_dpi, device = input$downloadPlot_pop_age_device)
    }
  )
  
  output$downloadPlot_pop_height <- downloadHandler(

    filename = function() { paste(input$pop_height, 'pop_plot_2.png', sep='') },
    content = function(file) {
      ggsave(file, plot = plot_pop_height(), units = "mm", width = input$downloadPlot_pop_height_width,
             height = input$downloadPlot_pop_height_height, dpi = input$downloadPlot_pop_height_dpi, device = input$downloadPlot_pop_height_device)
    }
  )
  
  output$downloadPlot_pop_weight <- downloadHandler(
    filename = function() { paste(input$pop_weight, 'pop_plot_3.png', sep='') },
    content = function(file) {
      ggsave(file, plot = plot_pop_weight(),units = "mm", width = input$downloadPlot_pop_weight_width,
             height = input$downloadPlot_pop_weight_height, dpi = input$downloadPlot_pop_weight_dpi, device = input$downloadPlot_pop_weight_device)
    }
  )
  
  output$downloadPlot_pop_mean_CO_Tanner <- downloadHandler(
    filename = function() { paste(input$pop_mean_CO_Tanner, 'pop_plot_4.png', sep='') },
    content = function(file) {
      ggsave(file, plot = plot_pop_mean_CO_Tanner(), units = "mm", width = input$downloadPlot_mean_CO_Tanner_width,
             height = input$downloadPlot_pop_mean_CO_Tanner_height, dpi = input$downloadPlot_pop_mean_CO_Tanner_dpi,
             device = input$downloadPlot_pop_mean_CO_Tanner_device)
    }
  )
  

  #
  # Download plots helper functions  (END) -----------------
  #
  
  #
  # Reset to defaults START ---------------------------------
  #
  
  observeEvent(input$reset_population_defaults_general, {
    
    updateNumericInput(session, inputId = "seed", value = 1)
    updateNumericInput(session, inputId = "individual_count",value = 15)
    updateSliderInput(session, inputId = "female_count",value = 0)
    updateSliderInput(session, inputId = "age_range",value = c(18, 70))
    updateNumericInput(session, inputId = "scale_M", value = 24.7)
    updateNumericInput(session, inputId = "shape_M", value = 2.1)
    updateNumericInput(session, inputId = "scale_F", value = 24.7)
    updateNumericInput(session, inputId = "shape_F", value = 1.9)
    updateNumericInput(session, inputId = "liver_density", value = 1080)
    updateNumericInput(session, inputId = "heart_density", value = 1050)
    updateNumericInput(session, inputId = "Qpf", value = 0.011)
    
  })
  
  
  observeEvent(input$reset_population_defaults_pharmacokinetics_absorption,{
    
    updateNumericInput(session, inputId = "ka",value = 0.2434986)
    updateNumericInput(session, inputId = "tlag_m_lognorm_m",value = 1.3321865)
    updateNumericInput(session, inputId = "tlag_m_lognorm_cv",value = 0.3)
    updateNumericInput(session, inputId = "F_mean",value = 45.9)
    updateNumericInput(session, inputId = "F_sd",value = 9.3)
    updateNumericInput(session, inputId = "F_lower",value = 33)
    updateNumericInput(session, inputId = "F_upper",value = 62)
    updateNumericInput(session, inputId = "FaFg_m_lognorm_m",value = 0.832)
    updateNumericInput(session, inputId = "FaFg_m_lognorm_cv",value = 0.131)
    
  })
  
  observeEvent(input$reset_population_defaults_pharmacokinetics_distribution,{
    
    updateNumericInput(session, inputId = "fup_m_lognorm_2_m",value = 0.060)
    updateNumericInput(session, inputId = "fup_m_lognorm_2_cv",value = 0.018)
    updateNumericInput(session, inputId = "BP_m_lognorm_2_m",value = 0.877)
    updateNumericInput(session, inputId = "BP_m_lognorm_2_cv",value = 0.168)
    updateNumericInput(session, inputId = "BP_metab_m_lognorm_2_m",value = 1.97)
    updateNumericInput(session, inputId = "BP_metab_m_lognorm_2_cv",value = 0.22)
    updateNumericInput(session, inputId = "fup_metab_1",value = 0.8231)
    updateNumericInput(session, inputId = "fup_metab_2",value = 0.0394)
    
  })
  
  observeEvent(input$reset_study_specific_defaults_oral , {
    
    updateNumericInput(session, inputId = "dose", value = 22)
    updateNumericInput(session, inputId = "inf_dose", value = 0)
    updateNumericInput(session, inputId = "inf_time", value = 2)
    updateNumericInput(session, inputId = "t_end", value = 84)
    
  })
  
  observeEvent(input$reset_scaling_factors_defaults , {
    
    updateNumericInput(session, inputId = "ISEF1A2", value = 11.1)
    updateNumericInput(session, inputId = "ISEF2C19", value = 3.07)
    updateNumericInput(session, inputId = "ISEF2D6", value = 0.74)
    updateNumericInput(session, inputId = "ISEF2C9", value = 5.73)
    updateNumericInput(session, inputId = "ISEF3A4", value = 3.92)
    updateNumericInput(session, inputId = "ISEF2B6", value = 3.7)
    updateNumericInput(session, inputId = "ISEF2C8", value = 3.7)
    
  })
  
  observeEvent(input$reset_study_specific_defaults_fumic , {
    
    updateNumericInput(session, inputId = "fumic", value = 0.82)
    updateNumericInput(session, inputId = "fumic_metab", value = 0.82)
    updateNumericInput(session, inputId = "CLefflux", value = 2.996919)
    updateNumericInput(session, inputId = "CLuptake", value = 2.177201)
    
  })
  
  observeEvent(input$reset_API_defaults_phys_chem_param, {
    
    updateNumericInput(session, inputId = "MW_api", value = 277.4)
    updateNumericInput(session, inputId = "pKa_api", value = 9.41)
    updateNumericInput(session, inputId = "PAMPA", value = 12.3*10^(-6))
    
    updateNumericInput(session, inputId = "Kpre_metab", value = 28.73022)
    updateCheckboxInput(session, inputId = "METAB_present",value = FALSE)
    updateNumericInput(session, inputId = "MW_metab", value = 263.384)
    updateNumericInput(session, inputId = "pKa_metab", value = 10.1)

  })
  
  observeEvent(input$reset_tissue_partition_coefficients_defaults_param , {
    
    updateNumericInput(session, inputId = "fu_ht_api", value = 0.0012)
    updateCheckboxInput(session, "Kpad_API", value = TRUE)
    updateNumericInput(session, inputId = "Kpad", value = 4.10)
    updateCheckboxInput(session, "Kpbo_API", value = TRUE)
    updateNumericInput(session, inputId = "Kpbo", value = 4.14)
    updateCheckboxInput(session, "Kpbr_API", value = TRUE)
    updateNumericInput(session, inputId = "Kpbr", value = 3.05)
    updateCheckboxInput(session, "Kpgu_API", value = TRUE)
    updateNumericInput(session, inputId = "Kpgu", value = 11.73)
    updateCheckboxInput(session, "Kphe_API", value = TRUE)
    updateNumericInput(session, inputId = "Kphe", value = 11.77)
    updateCheckboxInput(session, "Kppf_API", value = TRUE)
    updateNumericInput(session, inputId = "Kppf", value = 2.6)
    updateCheckboxInput(session, "Kpec_API", value = TRUE)
    updateNumericInput(session, inputId = "Kpec", value = 1)
    updateCheckboxInput(session, "Kpki_API",value = TRUE)
    updateNumericInput(session, inputId = "Kpki", value = 9.79)
    updateCheckboxInput(session, "Kpli_API", value = TRUE)
    updateNumericInput(session, inputId = "Kpli", value = 19.80)
    updateCheckboxInput(session, "Kplu_API", value = TRUE)
    updateNumericInput(session, inputId = "Kplu", value = 2.05)
    updateCheckboxInput(session, "Kpmu_API", value = TRUE)
    updateNumericInput(session, inputId = "Kpmu", value = 9.85)
    updateCheckboxInput(session, "Kpsk_API", value = TRUE)
    updateNumericInput(session, inputId = "Kpsk", value = 5.61)
    updateCheckboxInput(session, "Kpsp_API", value = TRUE)
    updateNumericInput(session, inputId = "Kpsp", value = 11.02)
    updateCheckboxInput(session, "Kpre_API", value = TRUE)
    updateNumericInput(session, inputId = "Kpre", value = 1)
    updateCheckboxInput(session, "Kpli_METAB", value = FALSE)
    updateNumericInput(session, inputId = "Kpli_metab", value = 59.08)
    updateCheckboxInput(session, "Kphe_METAB", value = FALSE)
    updateNumericInput(session, inputId = "Kphe_metab", value = 35.63)
    
  })
  
  observeEvent(input$reset_elimination_defaults_param, {
    
    updateNumericInput(session, inputId = "CLrenal", value = 0.09)
    updateNumericInput(session, inputId = "CLrenal_metab", value = 0.39)
    updateNumericInput(session, inputId = "fuha", value = 0.1230352)
    updateNumericInput(session, inputId = "fuhn", value = 0.1575158)
    updateCheckboxInput(session, "CYP1A2_API_dm", value = TRUE)
    updateNumericInput(session, inputId = "Vmax_1A2_api_dm", value = 59.08)
    updateNumericInput(session, inputId = "Km_1A2_api_dm", value = 35.63)
    updateCheckboxInput(session, "CYP2B6_API_dm", value = TRUE)
    updateNumericInput(session, inputId = "Vmax_2B6_api_dm", value = 0.25)
    updateNumericInput(session, inputId = "Km_2B6_api_dm", value = 56.7)
    updateCheckboxInput(session, "CYP2C8_API_dm", value = TRUE)
    updateNumericInput(session, inputId = "Vmax_2C8_api_dm", value = 0.7)
    updateNumericInput(session, inputId = "Km_2C8_api_dm", value = 9.74)
    updateCheckboxInput(session, "CYP2C9_API_dm", value = TRUE)
    updateNumericInput(session, inputId = "Vmax_2C9_api_dm", value = 3.97)
    updateNumericInput(session, inputId = "Km_2C9_api_dm", value = 50.5)
    updateCheckboxInput(session, "CYP2C19_API_dm", value = TRUE)
    updateNumericInput(session, inputId = "Vmax_2C19_api_dm", value = 4.22)
    updateNumericInput(session, inputId = "Km_2C19_api_dm", value = 8.52)
    updateCheckboxInput(session, "CYP2D6_API_dm", value = TRUE)
    updateNumericInput(session, inputId = "Vmax_2D6_api_dm", value = 1.49)
    updateNumericInput(session, inputId = "Km_2D6_api_dm", value = 7.12)
    updateCheckboxInput(session, "CYP3A4_API_dm", value = TRUE)
    updateNumericInput(session, inputId = "Vmax_3A4_api_dm", value = 3.37)
    updateNumericInput(session, inputId = "Km_3A4_api_dm", value = 213.8)
    updateCheckboxInput(session, "CYP2B6_API_h", value = TRUE)
    updateNumericInput(session, inputId = "Vmax_2B6_api_h", value = 0.13)
    updateNumericInput(session, inputId = "Km_2B6_api_h", value = 98)
    updateCheckboxInput(session, "CYP2D6_API_h", value = TRUE)
    updateNumericInput(session, inputId = "Vmax_2D6_api_h", value = 2.71)
    updateNumericInput(session, inputId = "Km_2D6_api_h", value = 4.75)
    updateCheckboxInput(session, "CYP3A4_API_h", value = TRUE)
    updateNumericInput(session, inputId = "Vmax_3A4_api_h", value = 0.4)
    updateNumericInput(session, inputId = "Km_3A4_api_h", value = 69.3)
    updateCheckboxInput(session, "CYP1A2_METAB_dm", value = FALSE)
    updateNumericInput(session, inputId = "Vmax_1A2_metab_dm", value = 6.8)
    updateNumericInput(session, inputId = "Km_1A2_metab_dm", value = 54.2)
    updateCheckboxInput(session, "CYP2C19_METAB_dm", value = FALSE)
    updateNumericInput(session, inputId = "Vmax_2C19_metab_dm", value = 93.1)
    updateNumericInput(session, inputId = "Km_2C19_metab_dm", value = 118)
    updateCheckboxInput(session, "CYP2D6_METAB_dm", value = FALSE)
    updateNumericInput(session, inputId = "Vmax_2D6_metab_dm", value = 19.4)
    updateNumericInput(session, inputId = "Km_2D6_metab_dm", value = 0.48)
    updateCheckboxInput(session, "CYP2D6_METAB_h", value = FALSE)
    updateNumericInput(session, inputId = "Vmax_2D6_metab_h", value = 130)
    updateNumericInput(session, inputId = "Km_2D6_metab_h", value = 0.74)
  })
  
  
  #
  # Reset to defaults END ------------------------------------
  #
  
  # 
  # Check Calculate metabolite concentration checkbox (START) ----------------
  # 
  
  observeEvent(input$METAB_present, {
    
    if(input$METAB_present == TRUE){
        
        shinyjs::enable("fuhn")
        shinyjs::enable("BP_metab_m_lognorm_2_m")
        shinyjs::enable("BP_metab_m_lognorm_2_cv")
        shinyjs::enable("fup_metab_1")
        shinyjs::enable("fup_metab_2")
        shinyjs::enable("fumic_metab")
        shinyjs::enable("CLrenal_metab")
        shinyjs::enable("Kpre_metab")
        shinyjs::enable("MW_metab")
        shinyjs::enable("pKa_metab")
        shinyjs::enable("Kpli_METAB")
        shinyjs::enable("Kpli_metab")
        shinyjs::enable("Kphe_METAB")
        shinyjs::enable("Kphe_metab")
        shinyjs::enable("CYP1A2_METAB_dm")
        shinyjs::enable("Vmax_1A2_metab_dm")
        shinyjs::enable("Km_1A2_metab_dm")
        shinyjs::enable("CYP2C19_METAB_dm")
        shinyjs::enable("Vmax_2C19_metab_dm")
        shinyjs::enable("Km_2C19_metab_dm")
        shinyjs::enable("CYP2D6_METAB_dm")
        shinyjs::enable("Vmax_2D6_metab_dm")
        shinyjs::enable("Km_2D6_metab_dm")
        shinyjs::enable("CYP2D6_METAB_h")
        shinyjs::enable("Vmax_2D6_metab_h")
        shinyjs::enable("Km_2D6_metab_h")
        shinyjs::enable("metab_plot_caption")
        shinyjs::enable("downloadPlot_res_conc_metab_in_venous_plasma")
        shinyjs::enable("downloadPlot_res_conc_metab_in_venous_plasma_height")
        shinyjs::enable("downloadPlot_res_conc_metab_in_venous_plasma_width")
        shinyjs::enable("downloadPlot_res_conc_metab_in_venous_plasma_dpi")
        shinyjs::enable("downloadPlot_res_conc_metab_in_venous_plasma_device")
        shinyjs::enable("downloadPlot_res_conc_metab_in_heart")
        shinyjs::enable("downloadPlot_res_conc_metab_in_heart_height")
        shinyjs::enable("downloadPlot_res_conc_metab_in_heart_width")
        shinyjs::enable("downloadPlot_res_conc_metab_in_heart_dpi")
        shinyjs::enable("downloadPlot_res_conc_metab_in_heart_device")
        
    } else {
      
      shinyjs::disable("fuhn")
      shinyjs::disable("BP_metab_m_lognorm_2_m")
      shinyjs::disable("BP_metab_m_lognorm_2_cv")
      shinyjs::disable("fup_metab_1")
      shinyjs::disable("fup_metab_2")
      shinyjs::disable("fumic_metab")
      shinyjs::disable("CLrenal_metab")
      shinyjs::disable("Kpre_metab")
      shinyjs::disable("MW_metab")
      shinyjs::disable("pKa_metab")
      shinyjs::disable("Kpli_METAB")
      shinyjs::disable("Kpli_metab")
      shinyjs::disable("Kphe_METAB")
      shinyjs::disable("Kphe_metab")
      shinyjs::disable("CYP1A2_METAB_dm")
      shinyjs::disable("Vmax_1A2_metab_dm")
      shinyjs::disable("Km_1A2_metab_dm")
      shinyjs::disable("CYP2C19_METAB_dm")
      shinyjs::disable("Vmax_2C19_metab_dm")
      shinyjs::disable("Km_2C19_metab_dm")
      shinyjs::disable("CYP2D6_METAB_dm")
      shinyjs::disable("Vmax_2D6_metab_dm")
      shinyjs::disable("Km_2D6_metab_dm")
      shinyjs::disable("CYP2D6_METAB_h")
      shinyjs::disable("Vmax_2D6_metab_h")
      shinyjs::disable("Km_2D6_metab_h")
      shinyjs::disable("metab_plot_caption")
      shinyjs::disable("downloadPlot_res_conc_metab_in_venous_plasma")
      shinyjs::disable("downloadPlot_res_conc_metab_in_venous_plasma_height")
      shinyjs::disable("downloadPlot_res_conc_metab_in_venous_plasma_width")
      shinyjs::disable("downloadPlot_res_conc_metab_in_venous_plasma_dpi")
      shinyjs::disable("downloadPlot_res_conc_metab_in_venous_plasma_device")
      shinyjs::disable("downloadPlot_res_conc_metab_in_heart")
      shinyjs::disable("downloadPlot_res_conc_metab_in_heart_height")
      shinyjs::disable("downloadPlot_res_conc_metab_in_heart_width")
      shinyjs::disable("downloadPlot_res_conc_metab_in_heart_dpi")
      shinyjs::disable("downloadPlot_res_conc_metab_in_heart_device")
        
    }
    
  # 
  # Check Calculate metabolite concentration checkbox (END) ----------------
  # 

  })
  
  
  #
  # Check for providing external data
  #
  
  observeEvent(input$in_external_data,{
    
    ##### if external data API is added plot with points
    
    if(!is.null(input$in_external_data)){
      
      tbl_in_external_data <- read.csv(input$in_external_data$datapath, header = input$in_external_header,
                                       sep = input$in_external_sep, quote = input$in_external_quote)
      
      names(tbl_in_external_data) <- c("rn","time","conc")
      
      output$res_conc_in_venous_plasma <- renderPlot({
        ggplot(data_vals, aes(time, BL, colour = rn)) +
          geom_line(size = 1) +
          theme_classic() +
          theme(
            panel.grid.major = element_line(colour="grey",size = rel(0.5)),
            panel.grid.minor = element_blank(),
            axis.title = element_text(size = 13),
            axis.text = element_text(size = 13),
            legend.text = element_text(size = 11),
            legend.title = element_text(size = 11),
            legend.position = "right"
          ) +
          ggtitle(paste("Concentration vs. time for ",input$api_plot_caption, sep="")) +
          labs(
              x = "Time [h]",
              y = "Concentration in venous plasma [mg/L]",
              colour = "Individuals"
          ) +
          geom_point(shape = 21, colour = "black", fill = "#56B4E9", size = 1.5, stroke = 0.9,
                     data = tbl_in_external_data, mapping = aes(x = time, y = conc, colour = "external data"))
      })
      
      output$res_log_conc_in_venous_plasma <- renderPlot({
        
        ggplot(data_vals, aes(time, BL, colour = rn)) +
          geom_line(size = 1) +
          theme_classic() +
          scale_y_log10() +
          ggtitle(paste("Log 10 concentration vs. time for ",input$api_plot_caption, sep="")) +
          labs(
              x = "Time [h]",
              y = "log 10 concentration in venous plasma [mg/L]",
              legend = "Individuals"
            ) +
          geom_point(shape = 21, colour = "black", fill = "#56B4E9", size = 1.5, stroke = 0.9,
                     data = tbl_in_external_data, mapping = aes(x = time, y = conc, colour = "external data")) +
          theme(
            panel.grid.major = element_line(colour="grey",size = rel(0.5)),
            panel.grid.minor = element_blank(),
            axis.title = element_text(size = 13),
            axis.text = element_text(size = 13),
            legend.text = element_text(size = 11),
            legend.title = element_text(size = 11),
            legend.position = "right"
          )
        
        
      })

      output$stats_res_conc_in_venous_plasma <- renderPlot({
        
        newDF <- as.data.frame(data_vals)
        
        subset <- newDF[is.finite(rowSums(newDF[,2:ncol(newDF)])),]
        
        
        CI_level <- input$downloadPlot_stats_res_conc_in_venous_plasma_CI
        
        CI_low <- (1-(CI_level/100))/2
        CI_high <- 1 - ((1- (CI_level/100))/2)
        
        stats_iv <- plyr::ddply(subset, .(time), function(subset) statFunc(subset$BL,CI_low, CI_high))
        
        ggplot(data=stats_iv, aes(x = time, y = median), colour = "red", alpha = 0.8) +
          geom_line(size = 1) +
          theme_classic() +
          theme(
            panel.grid.major = element_line(colour="grey",size = rel(0.5)),
            panel.grid.minor = element_blank(),
            axis.title = element_text(size = 13),
            axis.text = element_text(size = 13),
            legend.text = element_text(size = 11),
            legend.title = element_text(size = 11),
            legend.position = "right"
          ) +
          ggtitle(paste("Concentration vs. time for ",input$api_plot_caption, sep="")) +
          labs(
              x = "Time [h]",
              y = "Concentration in venous plasma [mg/L]",
              colour = "Individuals"
          ) + geom_ribbon(data= stats_iv, aes(x = time, ymin = lower_CI, ymax = higher_CI), fill="red", color = "red", alpha = 0.3)+
          geom_point(shape = 21, colour = "black", fill = "#56B4E9", size = 1.5, stroke = 0.9,
                     data = tbl_in_external_data, mapping = aes(x = time, y = conc, colour = "external data"))
        
      })
      
      output$stats_res_log_conc_in_venous_plasma <- renderPlot({
        
        newDF <- as.data.frame(data_vals)
        
        subset <- newDF[is.finite(rowSums(newDF[,2:ncol(newDF)])),]
        
        
        CI_level <- input$downloadPlot_stats_res_log_conc_in_venous_plasma_CI
        
        CI_low <- (1-(CI_level/100))/2
        CI_high <- 1 - ((1- (CI_level/100))/2)
        
        stats_iv <- plyr::ddply(subset, .(time), function(subset) statFunc(subset$BL,CI_low, CI_high))
        
        newDF$median <- with(newDF, ave(BL, time, FUN=function(x) median(x, na.rm = TRUE)))
        
        ggplot(data=stats_iv, aes(x = time, y = median), colour = "red", alpha = 0.8) +
          geom_line(size = 1) +
          theme_classic() +
          scale_y_log10() + # correct way of presenting the log10 values
          theme(
            panel.grid.major = element_line(colour="grey",size = rel(0.5)),
            panel.grid.minor = element_blank(),
            axis.title = element_text(size = 13),
            axis.text = element_text(size = 13),
            legend.text = element_text(size = 11),
            legend.title = element_text(size = 11),
            legend.position = "right"
          ) +
          ggtitle(paste("Concentration vs. time for ",input$api_plot_caption, sep="")) +
          labs(
              x = "Time [h]",
              y = "Log 10 Concentration in venous plasma [mg/L]",
              colour = "Individuals"
          ) + geom_ribbon(data= stats_iv, aes(x = time, ymin = lower_CI, ymax = higher_CI), fill="red", color = "red", alpha = 0.3)+
          geom_point(shape = 21, colour = "black", fill = "#56B4E9", size = 1.5, stroke = 0.9,
                     data = tbl_in_external_data, mapping = aes(x = time, y = conc, colour = "external data"))
        
      })
      
      ### Helper functions to downloadHandler
      
      plot_res_log_conc_in_venous_plasma <- function(data_to_plot, external_data){
        
        tbl_in_external_data <- external_data
        
        ggplot(data_to_plot, aes(time, BL, colour = rn)) +
          geom_line(size = 1) +
          theme_classic() +
          scale_y_log10() + # correct way of presenting the log10 values?
          theme(
            panel.grid.major = element_line(colour="grey",size = rel(0.5)),
            panel.grid.minor = element_blank(),
            axis.title = element_text(size = 13),
            axis.text = element_text(size = 13),
            legend.text = element_text(size = 11),
            legend.title = element_text(size = 11),
            legend.position = "right"
          ) +
          ggtitle(paste("Log 10 concentration vs. time for ",input$api_plot_caption, sep="")) +
          labs(
            x = "Time [h]",
            y = "log 10 concentration in venous plasma [mg/L]",
            colour = "Individuals"
          ) +
          geom_point(shape = 21, colour = "black", fill = "#56B4E9", size = 1.5, stroke = 0.9,
                     data = tbl_in_external_data, mapping = aes(x = time, y = conc, colour = "external data"))
      }
      
      plot_res_conc_in_venous_plasma <- function(data_to_plot, external_data){
        
        tbl_in_external_data <- external_data
        
        ggplot(data_to_plot, aes(time, BL, colour = rn)) +
          geom_line(size = 1) +
          theme_classic() +
          theme(
            panel.grid.major = element_line(colour="grey",size = rel(0.5)),
            panel.grid.minor = element_blank(),
            axis.title = element_text(size = 13),
            axis.text = element_text(size = 13),
            legend.text = element_text(size = 11),
            legend.title = element_text(size = 11),
            legend.position = "right"
          ) +
          ggtitle(paste("Concentration vs. time for ",input$api_plot_caption, sep="")) +
          labs(
            x = "Time [h]",
            y = "Concentration in venous plasma [mg/L]",
            colour = "Individuals"
          )+
          geom_point(shape = 21, colour = "black", fill = "#56B4E9", size = 1.5, stroke = 0.9,
                     data = tbl_in_external_data, mapping = aes(x = time, y = conc, colour = "external data"))
      }
      
      ######## Stats res plots
      
      plot_stats_res_conc_in_venous_plasma <- function(data_to_plot, external_data){
        
        tbl_in_external_data <- external_data
        
        CI_level <- input$downloadPlot_stats_res_conc_in_venous_plasma_CI
        
        CI_low <- (1-(CI_level/100))/2
        CI_high <- 1 - ((1- (CI_level/100))/2)
        
        stats_iv <- plyr::ddply(data_to_plot, .(time), function(data_to_plot) statFunc(data_to_plot$BL,CI_low, CI_high))
        
        ggplot(data=stats_iv, aes(x = time, y = median), colour = "red", alpha = 0.8) +
          geom_line(size = 1) +
          theme_classic() +
          theme(
            panel.grid.major = element_line(colour="grey",size = rel(0.5)),
            panel.grid.minor = element_blank(),
            axis.title = element_text(size = 13),
            axis.text = element_text(size = 13),
            legend.text = element_text(size = 11),
            legend.title = element_text(size = 11),
            legend.position = "right"
          ) +
          ggtitle(paste("Concentration vs. time for ",input$api_plot_caption, sep="")) +
          labs(
            x = "Time [h]",
            y = "Concentration in venous plasma [mg/L]",
            colour = "Individuals"
          ) + geom_ribbon(data= stats_iv, aes(x = time, ymin = lower_CI, ymax = higher_CI), fill="red", color = "red", alpha = 0.3) +
          geom_point(shape = 21, colour = "black", fill = "#56B4E9", size = 1.5, stroke = 0.9,
                     data = tbl_in_external_data, mapping = aes(x = time, y = conc, colour = "external data"))
        
      }
      
      plot_stats_res_log_conc_in_venous_plasma <- function(data_to_plot, external_data){
        
        tbl_in_external_data <- external_data
        
        CI_level <- input$downloadPlot_stats_res_log_conc_in_venous_plasma_CI
        
        CI_low <- (1-(CI_level/100))/2
        CI_high <- 1 - ((1- (CI_level/100))/2)
        
        stats_iv <- plyr::ddply(data_to_plot, .(time), function(data_to_plot) statFunc(data_to_plot$BL,CI_low, CI_high))
        
        ggplot(data=stats_iv, aes(x = time, y = median), colour = "red", alpha = 0.8) +
          geom_line(size = 1) +
          theme_classic() +
          scale_y_log10() + # correct way of presenting the log10 values
          theme(
            panel.grid.major = element_line(colour="grey",size = rel(0.5)),
            panel.grid.minor = element_blank(),
            axis.title = element_text(size = 13),
            axis.text = element_text(size = 13),
            legend.text = element_text(size = 11),
            legend.title = element_text(size = 11),
            legend.position = "right"
          ) +
          ggtitle(paste("Concentration vs. time for ",input$api_plot_caption, sep="")) +
          labs(
            x = "Time [h]",
            y = "Log 10 Concentration in venous plasma [mg/L]",
            colour = "Individuals"
          ) + geom_ribbon(data= stats_iv, aes(x = time, ymin = lower_CI, ymax = higher_CI), fill="red", color = "red", alpha = 0.3) +
          geom_point(shape = 21, colour = "black", fill = "#56B4E9", size = 1.5, stroke = 0.9,
                     data = tbl_in_external_data, mapping = aes(x = time, y = conc, colour = "external data"))
      }
      
      
      output$downloadPlot_res_log_conc_in_venous_plasma <- downloadHandler(
        filename = function() { paste(input$downloadPlot_res_log_conc_in_venous_plasma, '_plot_1.png', sep='') },
        content = function(file) {
          ggsave(file, plot = plot_res_log_conc_in_venous_plasma(data_vals, tbl_in_external_data), units = "mm", width = input$downloadPlot_res_log_conc_in_venous_plasma_width,
                 height = input$downloadPlot_res_log_conc_in_venous_plasma_height, dpi = input$downloadPlot_res_log_conc_in_venous_plasma_dpi,
                 device = input$downloadPlot_res_log_conc_in_venous_plasma_device)
        }
      )
      
      output$downloadPlot_res_conc_in_venous_plasma <- downloadHandler(
        filename = function() { paste(input$downloadPlot_res_conc_in_venous_plasma, '_plot_2.png', sep='') },
        content = function(file) {
          ggsave(file, plot = plot_res_conc_in_venous_plasma(data_vals, tbl_in_external_data), units = "mm", width = input$downloadPlot_res_conc_in_venous_plasma_width,
                 height = input$downloadPlot_res_conc_in_venous_plasma_height, dpi = input$downloadPlot_res_conc_in_venous_plasma_dpi,
                 device = input$downloadPlot_res_conc_in_venous_plasma_device)
        }
      )
      
      output$downloadPlot_stats_res_conc_in_venous_plasma <- downloadHandler(
        filename = function() { paste(input$downloadPlot_stats_res_conc_in_venous_plasma, '_plot_6.png', sep='') },
        content = function(file) {
          ggsave(file, plot = plot_stats_res_conc_in_venous_plasma(data_vals, tbl_in_external_data), units = "mm", width = input$downloadPlot_stats_res_conc_in_venous_plasma_width,
                 height = input$downloadPlot_stats_res_conc_in_venous_plasma_height, dpi = input$downloadPlot_stats_res_conc_in_venous_plasma_dpi,
                 device = input$downloadPlot_stats_res_conc_in_venous_plasma_device)
        }
      )
      
      output$downloadPlot_stats_res_log_conc_in_venous_plasma <- downloadHandler(
        filename = function() { paste(input$downloadPlot_stats_res_log_conc_in_venous_plasma, '_plot_7.png', sep='') },
        content = function(file) {
          ggsave(file, plot = plot_stats_res_log_conc_in_venous_plasma(data_vals, tbl_in_external_data), units = "mm", width = input$downloadPlot_stats_res_log_conc_in_venous_plasma_width,
                 height = input$downloadPlot_stats_res_log_conc_in_venous_plasma_height, dpi = input$downloadPlot_stats_res_log_conc_in_venous_plasma_dpi,
                 device = input$downloadPlot_stats_res_log_conc_in_venous_plasma_device)
        }
      )
      
      
      
    }
    
  })
  
  observeEvent(input$in_external_data_metab,{
    
    ##### if external data METAB is added plot with points
    
    if(!is.null(input$in_external_data_metab)){
      
      tbl_in_external_data_metab <- read.csv(input$in_external_data_metab$datapath, header = input$in_external_metab_header,
                                             sep = input$in_external_metab_sep, quote = input$in_external_metab_quote)
      
      names(tbl_in_external_data_metab) <- c("rn","time","conc")
      
      if(input$METAB_present == TRUE){
        
        output$res_conc_metab_in_venous_plasma <- renderPlot({
          ggplot(data_vals, aes(time, BL_METAB, colour = rn)) +
            geom_line(size = 1) +
            theme_classic() +
            theme(
              panel.grid.major = element_line(colour="grey",size = rel(0.5)),
              panel.grid.minor = element_blank(),
              axis.title = element_text(size = 13),
              axis.text = element_text(size = 13),
              legend.text = element_text(size = 11),
              legend.title = element_text(size = 11),
              legend.position = "right"
            ) +
            ggtitle(paste("Concentration vs. time for ",input$metab_plot_caption, sep="")) +
            labs(
              x = "Time [h]",
              y = "Concentration in venous plasma [mg/L]",
              colour = "Individuals"
            ) +
            geom_point(shape = 21, colour = "black", fill = "#56B4E9", size = 1.5, stroke = 0.9,
                       data = tbl_in_external_data_metab, mapping = aes(x = time, y = conc, colour = "external data"))

        })
        
        
        output$stats_res_conc_metab_in_venous_plasma <- renderPlot({
          
          newDF <- as.data.frame(data_vals)
          
          subset <- newDF[is.finite(rowSums(newDF[,2:ncol(newDF)])),]
          
          
          CI_level <- input$downloadPlot_stats_res_conc_metab_in_venous_plasma_CI
          
          CI_low <- (1-(CI_level/100))/2
          CI_high <- 1 - ((1- (CI_level/100))/2)
          
          stats_iv <- plyr::ddply(subset, .(time), function(subset) statFunc(subset$BL_METAB, CI_low, CI_high))
          
          ggplot(data=stats_iv, aes(x = time, y = median), colour = "red", alpha = 0.8) +
            geom_line(size = 1) +
            theme_classic() +
            theme(
              panel.grid.major = element_line(colour="grey",size = rel(0.5)),
              panel.grid.minor = element_blank(),
              axis.title = element_text(size = 13),
              axis.text = element_text(size = 13),
              legend.text = element_text(size = 11),
              legend.title = element_text(size = 11),
              legend.position = "right"
            ) +
            ggtitle(paste("Concentration vs. time for ",input$metab_plot_caption, sep="")) +
            labs(
              x = "Time [h]",
              y = "Concentration in venous plasma [mg/L]",
              colour = "Individuals"
            ) + geom_ribbon(data= stats_iv, aes(x = time, ymin = lower_CI, ymax = higher_CI), fill="red", color = "red", alpha = 0.3)+
            geom_point(shape = 21, colour = "black", fill = "#56B4E9", size = 1.5, stroke = 0.9,
                       data = tbl_in_external_data_metab, mapping = aes(x = time, y = conc, colour = "external data"))
        })
        
        ### Helper functions to downloadHandler
        
        plot_res_metab_conc_in_venous_plasma <- function(data_to_plot, external_data){
          
          tbl_in_external_data_metab <- external_data
          
          ggplot(data_to_plot, aes(time, BL_METAB, colour = rn)) +
            geom_line(size = 1) +
            theme_classic() +
            theme(
              panel.grid.major = element_line(colour="grey",size = rel(0.5)),
              panel.grid.minor = element_blank(),
              axis.title = element_text(size = 13),
              axis.text = element_text(size = 13),
              legend.text = element_text(size = 11),
              legend.title = element_text(size = 11),
              legend.position = "right"
            ) +
            ggtitle(paste("Concentration vs. time for ",input$metab_plot_caption, sep="")) +
            labs(
              x = "Time [h]",
              y = "Concentration in venous plasma [mg/L]",
              colour = "Individuals"
            )+
            geom_point(shape = 21, colour = "black", fill = "#56B4E9", size = 1.5, stroke = 0.9,
                       data = tbl_in_external_data_metab, mapping = aes(x = time, y = conc, colour = "external data"))
        }
        
        ######## Stats res plots
        
        plot_stats_res_metab_conc_in_venous_plasma <- function(data_to_plot, external_data){
          
          tbl_in_external_data_metab <- external_data
          
          CI_level <- input$downloadPlot_stats_res_metab_conc_in_venous_plasma_CI
          
          CI_low <- (1-(CI_level/100))/2
          CI_high <- 1 - ((1- (CI_level/100))/2)
          
          stats_iv <- plyr::ddply(data_to_plot, .(time), function(data_to_plot) statFunc(data_to_plot$BL_METAB,CI_low, CI_high))
          
          ggplot(data=stats_iv, aes(x = time, y = median), colour = "red", alpha = 0.8) +
            geom_line(size = 1) +
            theme_classic() +
            theme(
              panel.grid.major = element_line(colour="grey",size = rel(0.5)),
              panel.grid.minor = element_blank(),
              axis.title = element_text(size = 13),
              axis.text = element_text(size = 13),
              legend.text = element_text(size = 11),
              legend.title = element_text(size = 11),
              legend.position = "right"
            ) +
            ggtitle(paste("Concentration vs. time for ",input$metab_plot_caption, sep="")) +
            labs(
              x = "Time [h]",
              y = "Concentration in venous plasma [mg/L]",
              colour = "Individuals"
            ) + geom_ribbon(data= stats_iv, aes(x = time, ymin = lower_CI, ymax = higher_CI), fill="red", color = "red", alpha = 0.3) +
            geom_point(shape = 21, colour = "black", fill = "#56B4E9", size = 1.5, stroke = 0.9,
                       data = tbl_in_external_data_metab, mapping = aes(x = time, y = conc, colour = "external data"))

        }
        
        output$downloadPlot_res_conc_metab_in_venous_plasma <- downloadHandler(
          filename = function() { paste(input$downloadPlot_res_conc_metab_in_venous_plasma, '_plot_2.png', sep='') },
          content = function(file) {
            ggsave(file, plot = plot_res_conc_in_venous_plasma(data_vals, tbl_in_external_data_metab), units = "mm", width = input$downloadPlot_res_conc_metab_in_venous_plasma_width,
                   height = input$downloadPlot_res_conc_metab_in_venous_plasma_height, dpi = input$downloadPlot_res_conc_metab_in_venous_plasma_dpi,
                   device = input$downloadPlot_res_conc_metab_in_venous_plasma_device)
          }
        )
        
        output$downloadPlot_stats_res_conc_metab_in_venous_plasma <- downloadHandler(
          filename = function() { paste(input$downloadPlot_stats_res_conc_metab_in_venous_plasma, '_plot_6.png', sep='') },
          content = function(file) {
            ggsave(file, plot = plot_stats_res_conc_metab_in_venous_plasma(data_vals, tbl_in_external_data_metab), units = "mm", width = input$downloadPlot_stats_res_conc_metab_in_venous_plasma_width,
                   height = input$downloadPlot_stats_res_conc_metab_in_venous_plasma_height, dpi = input$downloadPlot_stats_res_conc_metab_in_venous_plasma_dpi,
                   device = input$downloadPlot_stats_res_conc_metab_in_venous_plasma_device)
          }
        )
        
      }
      
    }
    
  })
  

}  

# Run the application 
shinyApp(ui = ui, server = server)
