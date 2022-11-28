#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(dplyr)
library(lubridate)
library(EpiEstim)
library(stringr)
library(tools)
library(xlsx)
library(googledrive)
library(readr)
library(shiny.i18n)
library(ggplot2)
library(rio)
library(future)
library(promises)
library(shinyStore)
library(uuid)

#install.packages("JuliaConnectoR")
#devtools::install_github('bakaburg1/JuliaConnectoR@dev_juliaOpts') # to change once my fork is integrated in the main package
library(JuliaConnectoR)

library(forecast)
#library(shinylogs)
library(Cairo)
DEBUG <- FALSE

source("PlottingFunctions.R")
source("ExtraStats.R")
source("DataFunctions.R")
source("VaccFunctions.R")
source("NowcastFunctions.R")
source("REstimationFunctions.R")
source("incidenceModelFunctions.R")
source("CarePathFunctions.R")

begrenzer<-TRUE
use_future<-TRUE

logFilename<-"/var/www/public_files/CustomLogFiles.txt"
i18n <- Translator$new(translation_json_path = "./Resources/translation.json")
JuliaSessionBackup <- file.path('IUKCovid', 'julia_session.so')

languages <- c("Deutsch", "English")
flags <- c(
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/de.svg",
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/gb.svg"
)

RestrictCores<-4
detectedCores<-parallel::detectCores()
if(detectedCores<RestrictCores) {RestrictCores<-detectedCores}

# General functions (to put into a file) -------------------------------------------------

session <- "server-setup" # it is assumed the code run at server start-up, until session is overwritten.

reportLine <- function(message, session = NULL, uuid="-1",print = T, debug_only = FALSE){
  
  if (isTRUE(debug_only) & isFALSE(DEBUG)) return(invisible())
  
  msgSplit <- unlist(str_split(message,"\n"))
  
  session <- ifelse(is.character(session), session, session$token)
  
  doWrite <- !is.null(session) && exists("logFilename") && file.exists(logFilename)
  
  lapply(msgSplit, function(msg){
    line = paste0(format(now(), "%Y-%m-%d %H:%M:%OS3"), "\t", Sys.getpid(), '\t', session, "\t",uuid, "\t", msg)
    
    if (doWrite) write(line, file = logFilename, append = TRUE)
    
    if (print) cat(line, "\n")
  })
  
  invisible()
}

formatTimeDiff <- function(time) {
  if (is.POSIXct(time)) {
    time <- Sys.time() - time
  }
  
  unit <- attr(time, 'units')
  
  paste(signif(time, 3), unit)
}

# Julia helpers (to put into a file) -------------------------------------------------



getJuliaPort <- function() {
  # No need to cache the port. It's already stored here!
  port <- str_extract(Sys.getenv("JULIACONNECTOR_SERVER"), "\\d+$")
  if (is.na(port)) NULL else port
}

isJuliaPrecompiled <- function(ses = session) {
  check <- file.exists(JuliaSessionBackup)
  
  if (check) {
    reportLine('Julia session backup is available', ses)
  } else reportLine('Julia session backup is not present', ses)
  
  check
}

isJuliaSessionBackupValid <- function(ses = session) {
  msg <- suppressWarnings(system2("julia", c(paste0("-J", JuliaSessionBackup), '-e "exit()"'), stderr = TRUE, stdout = TRUE))
  
  isValid <- length(msg) == 0 || (
    !str_detect(msg[1], fixed("ERROR: could not load library")) &
      !any(str_detect(msg, fixed("image not found")))
  )
  
  if (isValid) {
    reportLine('Julia session backup is valid', ses)
  } else {
    reportLine('Julia session backup could not be used, with errors:', ses)
    reportLine(msg, 'julia-error')
    reportLine('Removing the .so file', ses)
    
    file.remove(JuliaSessionBackup)
  }
  
  isValid
}

startJuliaCompilation <- function(ses = session) {
  procFile <- file.path('IUKCovid', list.files('IUKCovid', pattern = 'compiling_\\d'))
  errFile <- file.path('IUKCovid', 'compilingErr.txt')
  
  if (length(procFile) == 0) {
    
    procFile <- file.path('IUKCovid', paste0('compiling_', str_replace_all(now(), c(' ' = 'T', ':' = '.')), '.txt'))
    
    pID <- sys::exec_background('julia', args = c(file.path('IUKCovid', 'precompile_package.jl'), JuliaSessionBackup, procFile, errFile),
                                std_out = procFile, std_err = errFile)
    
    reportLine(sprintf('Compilation of Julia session started in background (pID: %s) ...)', pID), ses)
    
    # pID <- sys::exec_background('nohup', args = c('julia', file.path('IUKCovid', 'precompile_package.jl'), procFile, errFile, '&'),
    #                             std_out = procFile, std_err = errFile)
    
    #browser()
    write(pID, procFile)
  } else {
    reportLine(paste(
      'Julia session compilation already started on',
      str_remove(basename(procFile), 'compiling_') %>% str_remove('\\.txt'),
      'with pID',
      readLines(procFile)), ses)
  }
}

checkJuliaCompilationErrors <- function(ses = session) {
  errFile <- file.path('IUKCovid', 'compilingErr.txt')
  procFile <- file.path('IUKCovid', list.files('IUKCovid', pattern = 'compiling_\\d'))[1]
  
  if (file.exists(errFile)) {
    errMsg <- readLines(errFile)
    
    if (any(str_detect(errMsg, 'ERROR'))) {
      
      reportLine("The compilation of Julia code in background failed:", ses)
      reportLine(errMsg, sprintf("%s (julia-error)", ifelse(is.character(ses), ses, ses$token)))
      
      pID <- str_extract(readLines(procFile), '\\d+')
      
      pIDname <- system(sprintf("ps -p %s -o command=", pID), intern = T)
      
      if (str_detect(pIDname, 'julia')) {
        reportLine(paste("Killing left-over Julia process with pID:", pID), ses)
        
        tools::pskill(pID) # kill the julia process if still alive
      }
      
      file.remove(procFile)
      file.remove(errFile)
    }
  }
}

startupJulia <- function(ses) {
  
  reportLine("Julia init ...", ses)
  
  tstart <- Sys.time()
  
  juliaPort <- suppressWarnings(startJuliaServer())
  
  # It would be better to catch the julia pID but I wouldn't know how
  reportLine(sprintf("Julia server started in: %s (port: %s)", formatTimeDiff(tstart), juliaPort),ses)
  
  # Double-checking if everything's ok
  if (is.null(getJuliaPort())) {
    showModal(modalDialog(
      title = "An error occurred!",
      "The IUK Forecast dashboard could not start up.\n Please write a message to iuk.forecast@uniklinik-freiburg.de.",
      easyClose = FALSE,
      footer = NULL
    ))
    # TODO: Some logic to manage the problem
    
    reportLine("Could not start the Julia server", ses)
    stop("Could not start the Julia server")
  }
  
  juliaPort
}

initializeJuliaFunctions <- function(IUKCovid, ses) {
  tstart <- Sys.time()
  packageRefFile <- "IUKCovid/IUKCovidRef.rds"
  
  if (class(try(IUKCovid, silent = T)) != "try-error") {
    
    reportLine("Using existing Julia function object ...", ses)
    
  } else if (file.exists(packageRefFile)) {
    
    reportLine("Load existing Julia function object ...", ses)
    IUKCovid <- read_rds(packageRefFile)
    
  } else {
    
    IUKCovid <- list(
      testConnection = function() FALSE
    )
  }
  
  
  test <- try(IUKCovid$testConnection(), silent = TRUE)
  
  if (!isTRUE(test)) {
    reportLine("Precompiled Julia function object not valid anymore:", ses)
    reportLine(test, sprintf("%s (julia-error)", ifelse(is.character(ses), ses, ses$token)))
    
    file.remove(packageRefFile)
    
    if (!isJuliaPrecompiled(ses)) {
      
      reportLine("Forcing Julia functions precompilation ...", ses)
      
      juliaEval('import Pkg; Pkg.activate("IUKCovid")')         # Import our package
      
      juliaEval("Pkg.Registry.update()")
      
      juliaEval('Pkg.resolve(); Pkg.instantiate()') 						# should install not up to date dependencies
      
      IUKCovid <- juliaImport('IUKCovid')
      
      if (DEBUG) IUKCovid$activateDebugging(TRUE)               # Activate a flag in the module that triggers extra logging
      
      numRuns <- 2L
      mrres <- data.frame(runNum = sample(1:numRuns, 10, replace = T), underlying = sample(1:33994, 10, replace = T))
      thisAdmProps = runif(10)
      directICUProp = 0L
      propGW2IC = 40L; propIC2SD = 40L
      losGWT = "Exponential"; losGWM = 11.5; losGWS = 11.5
      losICT = "Weibull";  losICM = 16.5; losICS = 14.5
      losSDT = "Weibull";  losSDM = 22; losSDS = 17
      numGW = 20L; numICU = 10L; startOffset = 526L
      
      IUKCovid$createInHRunWSD(
        numRuns,
        select(mrres, runNum, underlying) %>% mutate_all(as.integer),
        thisAdmProps, directICUProp,
        propGW2IC, propIC2SD,
        losGWT, losGWM, losGWS,
        losICT, losICM, losICS,
        losSDT, losSDM, losSDS,
        numGW, numICU, startOffset
      )
      
    } else {
      reportLine("Taking the Julia function object from stored session ...", ses)
      IUKCovid <- juliaImport('.IUKCovid') # The dot is important
    }
  }
  
  reportLine(paste("Julia function object initialization completed in:", formatTimeDiff(tstart)), ses)
  write_rds(IUKCovid, packageRefFile)
  
  IUKCovid
}

# Julia server-wide setup -------------------------------------------------

stopJulia()                                     # should (hopefully) kill leftover Julia connections, silently

Sys.setenv(JULIA_NUM_THREADS = RestrictCores)   # Allow multithreading Julia
Sys.setenv(JULIACONNECTOR_JULIAOPTS = "")       # Reset Julia options

if (isJuliaPrecompiled() && isJuliaSessionBackupValid()) {
  Sys.setenv(JULIACONNECTOR_JULIAOPTS = paste('-J', JuliaSessionBackup))
} else {
  startJuliaCompilation()
}

# -----------

plan(multisession,workers=RestrictCores)



languageButton_UI <- function(id, i18n) {
  ns <- NS(id)
  tagList( usei18n(i18n), pickerInput(ns("langChoice"),NULL, multiple = F,
                                      choices = languages,
                                      selected = "Deutsch",
                                      choicesOpt = list(content =
                                                          mapply(languages, flags, FUN = function(country, flagUrl) {
                                                            HTML(paste(
                                                              tags$img(src=flagUrl, width=20, height=15),
                                                              country
                                                            ))
                                                          }, SIMPLIFY = T, USE.NAMES = FALSE)
                                                        
                                      ))
  )
}

languageButton_Server <- function(id, global_session) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id)
      observeEvent(input$langChoice,{
        if(DEBUG) print(input$langChoice)
        if((input$langChoice=="Deutsch")){
          update_lang(global_session, "de")
        } else {
          update_lang(global_session, "en")
        }
      })
    }
  )
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  shiny.i18n::usei18n(i18n),
  useShinyjs(),
  # Application title
  tags$head(HTML("<title>IUK Forecast</title>")),
  titlePanel(i18n$t("Analysis and Prediction Tool for COVID-19 Cases")),
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      initStore("store", "shinyStore-iukdashboard"), # Namespace must be unique to this application!
      # Horizontal line ----
      #tags$hr(),
      #splitLayout(cellWidths = c("50%", "50%"),
      #            cellArgs = list(style = "padding: 6px"),
      uiOutput("StateOptions"),
      checkboxInput("allStates", i18n$t("Select all states"), FALSE),
      pickerInput("DistrictsMulti", i18n$t("Districts (RKI data):"), NULL , selected = NULL, multiple = TRUE),
      checkboxInput("allDistricts", i18n$t("Select all districts"), FALSE),
      splitLayout(
        htmlOutput("populationsize", inline= TRUE),
        htmlOutput("latestICUtext", inline= TRUE)
      ),
      tags$head(),
      tags$head(tags$style(HTML("
                            #populationsize {
                              font-size: 14px;
                              font-weight: bold;
                            }
                            ")),
                tags$style(HTML("
                            #latestICUtext {
                              font-size: 14px;
                              font-weight: bold;
                            }
                            ")),
                tags$script('$(document).on("shiny:connected", function(e) {
                              Shiny.onInputChange("innerSideWidth", window.innerWidth);
                              });
                              $(window).resize(function(e) {
                              Shiny.onInputChange("innerSideWidth", window.innerWidth);
                              });
                            ')
      ),
      #),
      tags$hr(),
      fluidRow(column(7, 
                      disabled(selectInput("selectDataPoint", i18n$t("Select start data point"), c("../../.... - . - . "), multiple = FALSE)) ),
               column(5, 
                      numericInput(inputId = "runLength",
                                   label =  i18n$t("Forecast length (days)"),
                                   value = 30,
                                   max = 90,
                                   min=5),) ),

      fluidRow(column(7, numericInput(inputId = "nrofruns",
                                label =  i18n$t("Number of Runs"),
                                value = 5,
                                min=1,
                                max=100),),
         column(5)),

      br(),
      fluidRow(column(7, languageButton_UI("language_button", i18n = i18n)),column(5)),
    
      br(),
      tags$hr(),
      tags$br(),
      imageOutput("logo"),
      tags$style("#copyrightiuk {
                                    width:100%;
                                    border-radius:15px;
                                    padding:6%; margin:6%;
                                    justify-content:center;
                                    font-size:14px; font-family: Calibri;
                                    color:black;
                                     }"),
      htmlOutput("copyrightiuk"),
      textOutput("versionnumber")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(i18n$t("Incidence"),
                 column(12,
                        plotOutput("inciDataPlot"),
                        sliderInput("viewRangeRKI", NULL,
                                    min = as.Date("2020-01-01"),
                                    max = (today()-10), value = c(as.Date("2020-01-01"),today()-10), width='100%'
                        )
                 ),
                 tags$hr(),
                 fluidRow(
                   column(4,
                          tags$style("#InciValue {
                                    width:80%;
                                    border-radius:15px;
                                    padding:6%; margin:2%;
                                    justify-content:center;
                                    font-size:15px; font-family: Calibri;
                                    color:black;
                                    background-color:#d9fbff;
                                    display:block; }"),
                          htmlOutput("InciValue")),
                   column(4,
                          tags$style("#SevendayInciValue {
                                    width:80%;
                                    border-radius:15px;
                                    padding:6%; margin:2%;
                                    justify-content:center;
                                    font-size:15px; font-family: Calibri;
                                    color:black;
                                    background-color:#ced1da;
                                     }"),
                          htmlOutput("SevendayInciValue")
                   )
                   
                 ),
                 tableOutput("ecdfsmall")
                 
        ),
        
        tabPanel(i18n$t("Eff. R (EpiEstim)"),
                 # Horizontal line ----
                 # column(12,
                 plotOutput("epiestim2"),
                 sliderInput("viewRangeRt", NULL,
                             min = as.Date("2020-01-01"), max = today()+30, value = c(as.Date("2020-01-01"),today()-1), width='100%'
                 ),
                 #  ),
                 downloadButton("exportButtonRt", "Export results"),
                 tags$hr(),
                 fluidRow(
                   column(4,
                          tags$style("#RValue {
                            width:80%;
                            border-radius:15px;
                            padding:6%; margin:2%;
                            justify-content:center;
                            font-size:15px; font-family: Calibri;
                            color:black;
                            background-color:#749dae;
                             }"),
                          htmlOutput("RValue")),
                   column(4,
                          tags$style("#RValueSim {
                            width:80%;
                            border-radius:15px;
                            padding:6%; margin:2%;
                            justify-content:center;
                            font-size:15px; font-family: Calibri;
                            color:black;
                            background-color:#f3c483;
                             }"),
                          htmlOutput("RValueSim")
                   ),
                   column(4,
                          checkboxInput("useETS", i18n$t("Use exponential smoothing"), FALSE),
                          checkboxInput("inclVOC", i18n$t("Include effect of Variant of Concern (VoC)"), FALSE),
                          #refDateVars<-as.Date("2021-02-01")+7
                          #advantage<-0.895
                          #startProp<-0.1182
                          disabled(
                            dateInput("startVOCdate",
                                      i18n$t("On this date"),
                                      min = as.Date("2020-01-01"),
                                      max = today(),
                                      value = as.Date("2021-12-30"))),
                          disabled(numericInputIcon("startVOCprop",
                                                    i18n$t("a VoC was present at this percentage"),
                                                    min = 0,
                                                    max = 100,
                                                    step = 0.01,
                                                    value = 52.6,
                                                    icon = list(NULL, icon("percent")))),
                          disabled(numericInputIcon("advanVOC",
                                                    i18n$t("with an additional transmissibility of"),
                                                    min = 0,
                                                    step = 0.1,
                                                    value = 10,
                                                    icon = list(NULL, icon("percent")))),
                          disabled(numericInputIcon("immEvasionVOC",
                                                    i18n$t("reducing immune/vaccine efficacy by"),
                                                    min = 0,
                                                    max= 100,
                                                    step = 0.1,
                                                    value = 85,
                                                    icon = list(NULL, icon("percent")))),
                          htmlOutput("advanCombi", inline= TRUE),
                          
                   )
                 )
        ),
        tabPanel(i18n$t("Vaccination"),
                 plotOutput("resultVacc"),
                 sliderInput("viewRangeVacc", NULL,
                             min = as.Date("2020-12-01")-30, max = today()+30, value = c(as.Date("2020-12-01")-30,today()+30), width='100%'
                 ),
                 downloadButton("exportButtonVacc", "Export results"),
                 tags$hr(),
                 fluidRow(
                   column(4,
                          numericInput(inputId = "vaccPop",
                                       label =  i18n$t("Number of started immunisations per day (first doses)"),
                                       value = 0,
                                       step = 1,
                                       min=0),
                          numericInput(inputId = "vaccPopBooster",
                                       label =  i18n$t("Number of booster immunisations per day"),
                                       value = 0,
                                       step = 1,
                                       min=0),
                          numericInput(inputId = "vaccDelayBooster",
                                       label =  i18n$t("Minimum delay between 2nd dose and booster (Days)"),
                                       value = 152,
                                       step = 1,
                                       min=0)
                          #htmlOutput("vacctext",inline=TRUE),
                   ),
                   column(4,
                          tags$style("#vaccSingle {
                                    width:80%;
                                    border-radius:15px;
                                    padding:6%; margin:2%;
                                    justify-content:center;
                                    font-size:15px; font-family: Calibri;
                                    color:black;
                                    background-color:#00990055;
                                     }"),
                          htmlOutput("vaccSingle")
                   ),
                   column(4,
                          tags$style("#vaccFull {
                                    width:80%;
                                    border-radius:15px;
                                    padding:6%; margin:2%;
                                    justify-content:center;
                                    font-size:15px; font-family: Calibri;
                                    color:black;
                                    background-color:#009900AA;
                                     }"),
                          htmlOutput("vaccFull")
                   )
                 )
                 
        ),
        tabPanel(i18n$t("Incidence Forecast"),
                 fluidRow(
                   column(11,
                     plotOutput("resultIncidence"),
                   ),
                   column(1,
                          noUiSliderInput(
                            inputId = "yZoomSlider", label = "Zoom",
                            min = 0, max = 500000, step = 5,
                            value = c(0, 0),
                            orientation = "vertical",
                            direction = 'rtl',
                            width = "100%", height = "400px"
                          ),
                   ),
                 ),
                 sliderInput("viewRangeInci", NULL,
                             min = as.Date("2020-01-01")-30, max = today()+30, value = c(as.Date("2020-01-01")-30,today()+30), width='100%'
                 ),
                 
                 downloadButton("exportButtonInci", "Export results"),
                 tags$hr(),
                 checkboxInput("wklyPattern", i18n$t("Simulate weekly reporting pattern"), FALSE)
        ),
        tabPanel(i18n$t("Bed Forecast"),

                 fluidRow(
                   column(11,
                          plotOutput("resultBeds"),
                   ),
                   column(1,
                          noUiSliderInput(
                            inputId = "yZoomSliderBeds", label = "Zoom",
                            min = 0, max = 500000, step = 5,
                            value = c(0, 0), 
                            orientation = "vertical",
                            direction = 'rtl',
                            width = "100%", height = "400px"
                          ),
                   ),
                 ),

                 sliderInput("viewRangeBeds", NULL,
                             min = as.Date("2020-01-01")-30, max = today()+30, value = c(as.Date("2020-01-01")-30,today()+30), width='100%'
                 ),
                 downloadButton("exportButton", "Export results"),
                 tags$hr(),
                 fluidRow(
                   column(4,
                          tags$style("#admHosp {
                                    width:80%;
                                    border-radius:15px;
                                    padding:6%; margin:2%;
                                    justify-content:center;
                                    font-size:15px; font-family: Calibri;
                                    color:black;
                                    background-color: #f3be9c;
                           }"),
                          htmlOutput("admHosp")
                   ),
                   column(4,
                          splitLayout(
                            numericInput("inputNumICU", i18n$t("Intensive care"), 10,
                                         min = 0),
                            numericInput("inputNumGW", i18n$t("General ward"), 20,
                                         min = 0)
                          ),
                          tags$hr(),
                          checkboxInput("onlyICU", i18n$t("Show only ICU results"), FALSE),
                          tags$hr(),
                          #checkboxInput("vaccAge", i18n$t("Include age-specific hospitalisation risk reduction by vaccination. (Vaccination distributed old to young)"), FALSE),
                          checkboxInput("manualDirect", i18n$t("Manually set direct ICU proportion"), FALSE),
                          disabled(sliderInput("directICUval",
                                               i18n$t("Direct ICU proportion"),
                                               min = 0,
                                               max = 1,
                                               step = 0.01,
                                               value = 0))
                   ),
                   
                   column(4,
                          fileInput("bedfile1", i18n$t("Choose Bed Occupancy File"),
                                    multiple = FALSE,
                                    accept = c(".xlsx", ".csv")),
                        
                          
                   )
                 )
        ),
        tabPanel(i18n$t("Parameter choices"),
                 tags$hr(),
                 
                 fluidRow(
                   strong(column(8, align="center", offset = 2,"Serial interval distribution"))
                 ),
                 tags$br(),
                 tags$br(),
                 fluidRow(
                   
                   column(width = 3,tags$br()," Serial interval"),
                   column(width = 3,selectInput("SIDT", "Distribution", c("Exponential","Gamma","Weibull"), selected="Weibull",multiple = FALSE)),
                   column(width = 3,numericInput("SIDM",#"disGW1" #P1M
                                                 "Mean",
                                                 min = 0,
                                                 max = 1000,
                                                 step = 0.01,
                                                 value = 14.37)),
                   column(width = 3,numericInput("SIDS",
                                                 "St Dev.",
                                                 min = 0.01,
                                                 max = 1000,
                                                 step = 0.01,
                                                 value = 11.39))
                 ),
                 tags$hr(),
                 
                 fluidRow(
                   strong(column(8, align="center", offset = 2,"Vaccination effect parameters"))
                 ),
                 tags$br(),
                 tags$br(),
                 fluidRow(
                   column(width = 3,tags$br()," Efficacy first dose preventing transmission"),
                   column(width = 3,numericInput("protEffac1",
                                                 "Efficacy",
                                                 min = 0.01,
                                                 max = 100,
                                                 step = 0.01,
                                                 value =25))
                 ),
                 fluidRow(
                   column(width = 3,tags$br()," Delay until protection after first dose"),
                   column(width = 3,selectInput("protDelay1T", "Distribution", c("Exponential","Gamma","Weibull"),selected="Gamma", multiple = FALSE)),
                   column(width = 3,numericInput("protDelay1M",
                                                 "Mean",
                                                 min = 0,
                                                 max = 1000,
                                                 step = 0.01,
                                                 value = 15)),
                   column(width = 3,numericInput("protDelay1S",
                                                 "St Dev.",
                                                 min = 0.01,
                                                 max = 1000,
                                                 step = 0.01,
                                                 value =3.8))
                 ),
                 fluidRow(
                   column(width = 3,tags$br()," Efficacy first and second dose preventing transmission"),
                   column(width = 3,numericInput("protEffac2",
                                                 "Efficacy",
                                                 min = 0.01,
                                                 max = 100,
                                                 step = 0.01,
                                                 value = 50))
                 ),
                 fluidRow(
                   column(width = 3,tags$br()," Delay until protection after second dose"),
                   column(width = 3,selectInput("protDelay2T", "Distribution", c("Exponential","Gamma","Weibull"),selected="Gamma", multiple = FALSE)),
                   column(width = 3,numericInput("protDelay2M",
                                                 "Mean",
                                                 min = 0,
                                                 max = 1000,
                                                 step = 0.01,
                                                 value = 15)),
                   column(width = 3,numericInput("protDelay2S",
                                                 "St Dev.",
                                                 min = 0.01,
                                                 max = 1000,
                                                 step = 0.01,
                                                 value =6.5))
                 ),

                 fluidRow(
                   column(width = 3,tags$br()," Efficacy first, second, and booster dose preventing transmission"),
                   column(width = 3,numericInput("protEffac3",
                                                 "Efficacy",
                                                 min = 0.01,
                                                 max = 100,
                                                 step = 0.01,
                                                 value = 78))
                 ),
                 fluidRow(
                   column(width = 3,tags$br()," Delay until protection after booster dose"),
                   column(width = 3,selectInput("protDelay3T", "Distribution", c("Exponential","Gamma","Weibull"),selected="Gamma", multiple = FALSE)),
                   column(width = 3,numericInput("protDelay3M",
                                                 "Mean",
                                                 min = 0,
                                                 max = 1000,
                                                 step = 0.01,
                                                 value = 7)),
                   column(width = 3,numericInput("protDelay3S",
                                                 "St Dev.",
                                                 min = 0.01,
                                                 max = 1000,
                                                 step = 0.01,
                                                 value =3.8))

                 ),
                 fluidRow(

                   column(width = 3,tags$br()," Relative hospitalisation risk by any immunity (vaccination and/or infection)"),
                   column(width = 3,numericInputIcon("relHospRisk",
                                                 "Relative risk",
                                                 min = 0.01,
                                                 max = 200,
                                                 step = 0.01,
                                                 value = 10, 
                                                 icon = list(NULL, icon("percent"))))
                  ),
                 ##############
                 tags$hr(),
                 
                 fluidRow(
                   strong(column(8, align="center", offset = 2,"Within hospital (care-path) parameters"))
                 ),
                 tags$br(),
                 tags$br(),
                 
                 fluidRow(
                   column(width = 3,tags$br()," Length of stay, general ward"),
                   column(width = 3,selectInput("losGWT", "Distribution", c("Exponential","Gamma","Weibull"),selected="Exponential", multiple = FALSE)),
                   column(width = 3,numericInput("losGWM",
                                                 "Mean",
                                                 min = 0,
                                                 max = 1000,
                                                 step = 0.01,
                                                 value = 11.5)),
                   column(width = 3,numericInput("losGWS",
                                                 "St Dev.",
                                                 min = 0.01,
                                                 max = 1000,
                                                 step = 0.01,
                                                 value =11.5))
                 ), fluidRow(
                   column(width = 3,tags$br()," Proportion GW admitted to ICU"),
                   column(width = 3,numericInput("propGW2IC",#"disGW2"
                                                 "Percentage",
                                                 min = 0,
                                                 max = 100,
                                                 step = 0.1,
                                                 value = 7))

                 ),
                 fluidRow(
                   column(width = 3,tags$br()," Length of stay, ICU"),
                   column(width = 3,selectInput("losICT", "Distribution", c("Exponential","Gamma","Weibull"),selected="Exponential", multiple = FALSE)),
                   column(width = 3,numericInput("losICM",#"disGW2"
                                                 "Mean",
                                                 min = 0,
                                                 max = 100,
                                                 step = 0.01,
                                                 value = 8.5)),
                   column(width = 3,numericInput("losICS",
                                                 "St Dev.",
                                                 min = 0.01,
                                                 max = 100,
                                                 step = 0.01,
                                                 value = 8.5))
                 ),
                 fluidRow(
                   column(width = 3,tags$br()," Proportion ICU admitted to SD"),
                   column(width = 3,numericInput("propIC2SD",
                                                 "Percentage",
                                                 min = 0.01,
                                                 max = 100,
                                                 step = 0.1,
                                                 value = 70))
                   
                 ),
                 fluidRow(
                   column(width = 3,tags$br()," Length of stay, step-down unit"),
                   column(width = 3,selectInput("losSDT", "Distribution", c("Exponential","Gamma","Weibull"),selected="Weibull", multiple = FALSE)),
                   column(width = 3,numericInput("losSDM",#"disGW2"
                                                 "Mean",
                                                 min = 0,
                                                 max = 100,
                                                 step = 0.01,
                                                 value = 22)),
                   column(width = 3,numericInput("losSDS",
                                                 "St Dev.",
                                                 min = 0.01,
                                                 max = 100,
                                                 step = 0.01,
                                                 value = 17.1))
                 ),
                 
                 tags$br(),
                 tags$hr(),
                 fluidRow(
                   strong(column(8, align="center", offset = 2,"Exponential smoothing (ETS) model"))
                 ),
                 tags$br(),
                 tags$br(),
                 fluidRow(
                   column(width = 3,tags$br()," ETS model parameters"),
                   column(width = 3,numericInput("ETSlength",#"disGW2"
                                                 "Length of fit (days)",
                                                 min = 0,
                                                 max = 500,
                                                 step = 11,
                                                 value = 100)),

                   column(width = 3,numericInput("ETSalpha",#"disGW2"
                                                 "alpha",
                                                 min = 0,
                                                 max = 1,
                                                 step = 0.01,
                                                 value = 0.25)),
                   column(width = 3,numericInput("ETSbeta",#"disGW2"
                                                 "beta",
                                                 min = 0,
                                                 max = 1,
                                                 step = 0.01,
                                                 value = 0.15))
                 ),

                 tags$br(),
                 tags$hr(),
                 actionButton("resetButton", "Reset parameters"),
                 tags$hr(),
                 fluidRow(
                   column(width = 6,
                          
                          strong(column(8, align="center", offset = 2,"Serial interval distribution")),
                          tags$head(tags$script('$(document).on("shiny:connected", function(e) {
                              Shiny.onInputChange("innerWidth", window.innerWidth);
                              });
                              $(window).resize(function(e) {
                              Shiny.onInputChange("innerWidth", window.innerWidth);
                              });
                            ')),
                          plotOutput("SerialIntervalPlot")
                   ),
                   column(width = 6,
                          
                          strong(column(8, align="center", offset = 2,"Resulting ward stay")),
                          tags$head(tags$script('$(document).on("shiny:connected", function(e) {
                              Shiny.onInputChange("innerWidth", window.innerWidth);
                              });
                              $(window).resize(function(e) {
                              Shiny.onInputChange("innerWidth", window.innerWidth);
                              });
                            ')),
                          plotOutput("losDistrPlot")
                   )
                 )
        ),
        tabPanel(i18n$t("Manual"),
                 tags$style("#manualtext {
                                    width:95%;
                                    border-radius:15px;
                                    padding:2%; margin:2%;
                                    justify-content:center;
                                    font-size:15px; font-family: Calibri;
                                    color:black;
                             }"),
                 htmlOutput("manualtext"),
                 tags$hr()
        ),
        tabPanel(i18n$t("About"),
                 htmlOutput("acknowledgetext"),
                 tags$hr(),
                 HTML("See details on our <a href=https://iuk-forecast.uniklinik-freiburg.de/DSGVO.html>cookie policy</a> (in German)<br>"),
                 actionButton("removeBiscuit","Remove locally stored user ID (Cookie)")
        )
      )
    )
  )
)

useSweetAlert()

#source("../EarlyIncidencePrediction.R")

onStop(function() { 
  JuliaConnectoR::stopJulia() # kill julia if server disconnects
})          

# Data loading outside server block 
#incidenceDataGlobal<-getInciData(getDataLocation())
#populationDataGlobal<-getDistrictPops()
#vaccinationDataGlobal<-getVaccTable(getDataLocation(),populationDataGlobal)
#bedDataGlobal<-readHistoricalBedInfo(getDataLocation(),populationDataGlobal,maxDate=max(incidenceDataGlobal$Date))

# Server start ------------------------------------------------------------

server <- function(input, output, session) {
  myLocalUUID<-reactiveVal("-1")
  
  onStop(function() {
    dev.off(which=dev.cur())
    reportLine("Stopping session",session,isolate(myLocalUUID()))
  })
  
  Cairo(file='/dev/null')
  reportLine(sprintf("Starting new session (location: %s)", isolate(getwd())),session,isolate(myLocalUUID()))
  
  
  # Julia Init --------------------------------------------------------------
  
  pleaseWait_init <- modalDialog(
    title = "Initialising dashboard",
    "The IUK Forecast dashboard is starting up. This could take a while.\n Please wait.",
    easyClose = FALSE,
    footer = NULL
  )
  
  checkJuliaCompilationErrors(session) # Since the errors can take time to happen, we check every time a user log in
  
  if (is.null(getJuliaPort())) {
    showModal(pleaseWait_init)
    isModalShown <- TRUE
    
    juliaPort <- startupJulia(session)
  }
  
  IUKCovid <- initializeJuliaFunctions(IUKCovid, session)
  removeModal()
  isModalShown = FALSE
  

  # Store local variables --------------------------------------------------
  if(getDataLocation()=="server"){
    versionnrs_file <- file.path('/home/ukfadmin/autoDashboard/', 'currentRelease.txt')
    versionnrs <- readLines(versionnrs_file)
    currver <- versionnrs[length(versionnrs)]
  } else {
    currver<-"v0000.02F"
  }
  lastver <- isolate(input$store)$lastreleaseversion
  relnotes <-  readLines(file.path('./Resources/','ReleaseNotes.txt'))

  wantCookie<-FALSE

  pleaseWait_relnotes_cookie <- modalDialog(
    title = "New version",
    paste("The dashboard changed to a new version. \n"),
    HTML(relnotes),
    "\n\nIMPORTANT: Do you accept a cookie that locally stores the last seen version number and an anonymous id?\n",
    HTML("See details on our <a href=https://iuk-forecast.uniklinik-freiburg.de/DSGVO.html>cookie policy</a> (in German)<br>"),
    easyClose = FALSE,
    footer = tagList(
      actionButton("declineBiscuits", "Decline cookies and close"),
      actionButton("acceptBiscuits", "Accept cookies and close")
    )
  )
  observeEvent(input$declineBiscuits,{
    wantCookie<-FALSE
    removeModal()
  })
  
  observeEvent(input$acceptBiscuits,{
    wantCookie<-TRUE
    myLocalUUID(uuid::UUIDgenerate(use.time = TRUE, n = 1L))
    updateStore(session, "localuuid",  isolate(myLocalUUID()) )
    print(paste0("Created local UUID: ",isolate(myLocalUUID())))
    
    updateStore(session, "lastreleaseversion", currver)
    print(paste0("Stored local version number ",currver))
    removeModal()
  })
  
 #updateStore(session, "lastreleaseversion", currver)
  
  observeEvent(input$removeBiscuit,{
    wantCookie<-FALSE
    myLocalUUID("-1")
    updateStore(session, "localuuid",  NULL )
    reportLine("Removed cookie",session,isolate(myLocalUUID()))
    updateStore(session, "lastreleaseversion", NULL)
  })
  

  pleaseWait_relnotes <- modalDialog(
    title = "New version",
    paste("The dashboard changed to a new version.\n"),
    HTML(relnotes),
    "",
    easyClose = FALSE,
    footer = tagList(
      modalButton("OK")
    )
  )
  
  myLocalUUID(isolate(input$store)$localuuid)
  
  if (is.null(isolate(myLocalUUID()))){
      print("No information from cookie, new user or declined cookie")
      myLocalUUID(-1)
      cookieInfo<-FALSE
  } else {
     #myLocalUUID<-isolate(input$store)$localuuid
     print(paste0("Read local UUID: ",isolate(myLocalUUID())))
     wantCookie<-TRUE
     cookieInfo<-TRUE
  }
  
  if (!is.null(lastver) ) {
      print('Version number is outdated')
      if (currver != lastver){
        updateStore(session, "lastreleaseversion", currver) 
        showModal(pleaseWait_relnotes)
      }
  } else {
    if(cookieInfo) {
      showModal(pleaseWait_relnotes)
    } else {
      showModal(pleaseWait_relnotes_cookie)
    }
  }

  languageButton_Server("language_button", global_session = session)
  
  numberOfRuns<-reactiveVal(as.integer(5)) #Note that we have to manually set this value to the standard value of the input field (I don't think you can do this dynamically)
  bedPlot <- reactiveVal()
  parameterVersion <- reactiveVal(0)
  valueMemory <- reactiveValues(
    startSimDate=today()-1,
    runLength=30
  )
  dataLoc<-reactiveVal(getDataLocation())
  automatedBedNumber<-reactiveVal(TRUE)
  
  
  output$logo <- renderImage({
    return(list(
      src = "Resources/logoUKF.png",
      contentType = "image/png",
      width = "80%",
      height=reactive(ifelse(!is.null(input$innerSideWidth),input$innerSideWidth*1/5,0)),
      alt = "Uniklinikum Freiburg"
    ))
  }, deleteFile = FALSE)
  
  output$versionnumber <- renderText(
    paste('Version:',input$store$lastreleaseversion, '    uuid:',isolate(input$store)$localuuid)
  )
  
  output$StateOptions <- renderUI({
    if(DEBUG) print("StateOptions")
    
    withProgress(message = i18n$t('Fetching RKI data') , value = 0, {
      resetToUKF()
      incProgress(1/3, detail = "...")
      resetToUKF()
      temp<-inciData()
      # Increment the progress bar, and update the detail text.
      incProgress(2/3, detail = "Bundeslaender")
      districts <- (districtPops())
      incProgress(1, detail = "Finished")
      
      #Sys.sleep(0.1)
    })
    
    if (input$allStates) {
      selStates <- unique(districts$stateName)
      return(pickerInput("StateMulti",  i18n$t("State:"), unique(districts$stateName), selected = selStates, multiple = TRUE))
      
    } else {
      selStates <- NULL
      return(pickerInput("StateMulti",  i18n$t("State:"), unique(districts$stateName),  multiple = TRUE))
    }
    if(DEBUG) print("StateOptions - Done")
  })
  
  resetRunParams<-function(){
    updateNumericInput(session,"nrofruns",value=5,min=1,max=100)
    updateNumericInput(session,"runLength",value=30)
  }
  
  ShowBegrenzer<-function(){
    showModal(modalDialog(
      title = "Limiter",
      HTML(paste0(
        "The chosen parameters may cause a long calculation time, and are therefore reset to the minimum values.<br><br>",
        "To enable longer simulations, use fewer runs.<br>To enable more runs, set a shorter simulation.<br><br>",
        "Alternatively, download the dashboard's source code from github, run locally with the limiter disabled.<br>"
      )
      ),
      footer=modalButton("OK, sorry")
    ))
  }
  
  checkBegrenzerPassed<-function(runs,runlength){
    return((selectedInci()[[1,5]]/100000)*runs*runlength<(900*5*18200))
  }
  
  districtsMultiVal<-reactiveVal()
  
  observeEvent(input$DistrictsMulti, {
    districtsMultiVal(input$DistrictsMulti)
    automatedBedNumber(TRUE)
    reportLine(paste0("observeEvent: Selected ",length(input$DistrictsMulti)," districts"),session,isolate(myLocalUUID()))
  })
  
  observeEvent(districtsMultiVal(), {
    if(is.null(bedFileName() )){ #If no bed occupancy file is given, load an initial number if beds from the DIVI data
      bedObservations <-autoSelectedBedNums()
      nObs<-nrow(bedObservations)
      print(paste0("nObs: ",nObs))
      endDate<-bedObservations[[nObs,"Date"]]
      
      enable("selectDataPoint")
      if(nObs==1){
        choiceStrings<-c(paste0(as.character(bedObservations[[1,"Date"]])," (",i18n$t("ICU"),": ",bedObservations[[1,"ICU"]],", ",i18n$t("GW"),": ",bedObservations[[1,"Normal"]],")"))
      } else {
        choiceStrings<-unlist(lapply((nObs):1,
                                     function(x) paste0(as.character(bedObservations[[x,"Date"]])," (",i18n$t("ICU"),": ",bedObservations[[x,"ICU"]],", ",i18n$t("GW"),": ",bedObservations[[x,"Normal"]],")")))
      }
      
      print("updating ICU/GW numbers place 1")
      isolate(updateNumericInput(session, "inputNumICU", value = bedObservations[nrow(bedObservations),"ICU"]))
      isolate(updateNumericInput(session, "inputNumGW", value = bedObservations[nrow(bedObservations),"Normal"]))
      updateSelectInput(session, "selectDataPoint",choices=choiceStrings,selected=choiceStrings[1])
      #updateSliderInput(session,"directICUval",value=0)
    }
    updateNumericInput(session, "vaccPop", value = vaccRate())
    updateNumericInput(session, "vaccPopBooster", value = vaccRateBooster())
    automatedBedNumber(T)
  })
  
  observeEvent(input$StateMulti, {
    
    reportLine(paste0("observeEvent: Selected ",length(input$StateMulti)," states"),session,isolate(myLocalUUID()))
    
    if(DEBUG) print("StateMulti")
    origSelDistricts<-districtsMultiVal()
    
    districts=unique(districtPops()[,c("districtName","idDistrict","stateName")])
    districts<-(districts[!is.na(districts$idDistrict),])
    
    allDistricts <- districts[districts$stateName %in% input$StateMulti  ,]
    
    origSelDistrictsincl<-districts[districts$districtName %in% origSelDistricts,]
    origSelDistrictsincl<-origSelDistrictsincl[origSelDistrictsincl$stateName %in% input$StateMulti,]
    
    if (input$allDistricts) {
      selDistricts <- unique(allDistricts$districtName)
      
    } else {
      selDistricts <- unique(origSelDistrictsincl$districtName) #the original selected
    }
    updatePickerInput(session,"DistrictsMulti", choices=allDistricts$districtName , selected = selDistricts)
    if(DEBUG) print("StateMulti-Done")
  })
  
  observeEvent(input$allStates, {
    if(input$allStates==FALSE){
      updatePickerInput(session,"DistrictsMulti",selected = 0)
    }
  })
  
  observeEvent(input$allDistricts, {
    if(input$allDistricts==TRUE){
      allDistrictsAllStates=unique(districtPops()[,c("districtName","idDistrict","stateName")])
      allDistrictsAllStates<-(allDistrictsAllStates[!is.na(allDistrictsAllStates$idDistrict),])
      updatePickerInput(session,"DistrictsMulti", selected = allDistrictsAllStates[allDistrictsAllStates$stateName %in% input$StateMulti,"districtName"])
      
    } else {
      updatePickerInput(session,"DistrictsMulti", selected = 0)
      districtsMultiVal(NULL)
    }
  })
  
  observeEvent(input$bedfile1, {
    if(DEBUG) print("Loaded file!")
    reportLine('observeEvent: Loaded bed file',session,isolate(myLocalUUID()))
    bedObservations <- obsData()
    nObs<-nrow(bedObservations)
    endDate<-bedObservations[[nObs,"Date"]]
    #update input to the last record in the file (nObs-1), because nObs is the original value in the inputs.
    enable("selectDataPoint")
    choiceStrings<-unlist(lapply((nObs):1,
                                 function(x) paste0(as.character(bedObservations[[x,"Date"]])," (",i18n$t("ICU"),": ",bedObservations[[x,"ICU"]],", ",i18n$t("GW"),": ",bedObservations[[x,"Normal"]],")")))
    
    updateSelectInput(session, "selectDataPoint",choices=choiceStrings)
  })
  
  observeEvent(input$manualDirect, {
    reportLine('observeEvent: manualDirect',session,isolate(myLocalUUID()))
    if(input$manualDirect==FALSE) {
      disable("directICUval")
    } else {
      enable("directICUval")
    }
  })
  
  observeEvent(input$inclVOC, {
    reportLine('observeEvent: inclVOC',session,isolate(myLocalUUID()))
    if(input$inclVOC==FALSE) {
      disable("startVOCdate")
      disable("advanVOC")
      disable("startVOCprop")
      disable("immEvasionVOC")
    } else {
      enable("startVOCdate")
      enable("advanVOC")
      enable("startVOCprop")
      enable("immEvasionVOC")
    }
  })
  
  observeEvent(input$inputNumICU, {
    # 
    ni <- isolate(input$inputNumICU)
    #      print(paste0("Observed set to ",ni," ICU beds"))
    #       
    if (!isTruthy(ni)){
      print('numICU not truthy')
      ni<-1
      #  isolate(updateNumericInput(session, "inputNumICU", value = 1))
    }
    if((numICU())!= ni) {
      #         print(paste0("Set numICU from ", isolate(numICU())," to ", ni))
      reportLine(paste0("Set numICU from ", isolate(numICU())," to ", ni),session,isolate(myLocalUUID()))
      numICU(ni)
      #         isolate(updateNumericInput(session, "inputNumICU", value = ni))
    }
    automatedBedNumber(F)
  })
  
  
  observeEvent(input$inputNumGW, {
    ngw <- isolate(input$inputNumGW)
    # print(paste0("Observed set to ",ngw," GW beds"))
    # 
    if (!isTruthy(ngw)){
      print('numGW not truthy')
      ngw<-1
      #   isolate(updateNumericInput(session, "inputNumGW", value = 1))
    }
    # 
    if((numGW())!= ngw) {
      reportLine(paste0("Set numGW from ", isolate(numGW())," to ", ngw),session,isolate(myLocalUUID()))
      #   isolate(updateNumericInput(session, "inputNumGW", value = ngw))
      numGW(ngw)
    }
    automatedBedNumber(F)
  })
  
  vaccinPop <- reactiveVal(0)
  
  observeEvent(input$vaccPop, {
    print('vaccPop, vp:')
    vp <- input$vaccPop
    print(vp)
    
    if (!isTruthy(vp)){
      print('vaccinPop not truthy')
      vp<-vaccRate()
      # updateNumericInput(session, "vaccPop", value = vaccRate())
    }
    
    pop <- selectedInci()[1,5]
    if (vp > pop) {
      print('vaccinPop greater than population')
      vp <- pop
      #  isolate(updateNumericInput(session, "vaccPop", value = pop))
    }
    
    print(vaccinPop())
    if((vaccinPop())!= vp) {
      reportLine(paste0("Set vaccination pop from ", isolate(vaccinPop())," to ", vp),session,isolate(myLocalUUID()))
      vaccinPop(vp)
      #  isolate(updateNumericInput(session, "vaccPop", value = vp))
    }
  })

  vaccinDelayBooster <- reactiveVal(152)
  
  observeEvent(input$vaccDelayBooster, {
    print('vaccDelayBooster, vp:')
    vp <- input$vaccDelayBooster
    print(vp)
    
    if (!isTruthy(vp)){
      print('vaccinPop not truthy')
      vp<-vaccinDelayBooster()
    }
    if(vp<0) vp<-vaccinDelayBooster()
    
    print(vaccinDelayBooster())
    if((vaccinDelayBooster())!= vp) {
      reportLine(paste0("Set vaccination delay from ", isolate(vaccinDelayBooster())," to ", vp),session,isolate(myLocalUUID()))
      vaccinDelayBooster(vp)
    }
  })
  
  vaccinPopBooster <- reactiveVal(0)
  
  observeEvent(input$vaccPopBooster, {
    print('vaccPopBooster, vp:')
    vp <- input$vaccPopBooster
    print(vp)
    
    if (!isTruthy(vp)){
      print('vaccinPop not truthy')
      vp<-vaccRateBooster()
    }
    
    pop <- selectedInci()[1,5]
    if (vp > pop) {
      print('vaccinPop greater than population')
      vp <- pop
    }
    
    print(vaccinPopBooster())
    if((vaccinPopBooster())!= vp) {
      reportLine(paste0("Set vaccination pop from ", isolate(vaccinPopBooster())," to ", vp),session,isolate(myLocalUUID()))
      vaccinPopBooster(vp)
    }
  })
  
  startDateSim <- reactiveVal(today()-1 )
  
  # observeEvent(input$selectDataPoint, {
  #   #if(DEBUG)
  #   print('observeEvent: Change in Input value startSimDate')
  #   ssd <- input$startSimDate
  # 
  #   if (!isTruthy(ssd)){
  #     ssd<-today()-1
  #     #isolate(updateDateInput(session,"startSimDate",value = ssd))
  #     startDateSim(ssd)
  #     if(isolate(valueMemory$startSimDate)==as.Date("2035-01-01")){
  #       #isolate(updateDateInput(session,"startSimDate",value = ssd))
  #       startDateSim(ssd)
  #     }
  #   }
  # 
  #   if(is.na(ssd)){
  #     #isolate(updateDateInput(session,"startSimDate",value = today()-1))
  #     startDateSim(ssd)
  #     ssd<-today()-1
  #   }
  # 
  # 
  #   # if(ssd<as.Date("2020-01-01")){
  #   #   isolate(updateDateInput(session,"startSimDate",value = as.Date("2020-01-01")))
  #   #   ssd<-as.Date("2020-01-01")
  #   # }
  #   # if(ssd> today()-1){
  #   #   isolate(updateDateInput(session,"nrofruns",value =  today()-1))
  #   #   ssd<- today()-1
  #   # }
  # 
  #   if((startDateSim())!=ssd) {
  # 
  #     reportLine(paste0("setting startDateSim from ", isolate(startDateSim())," to ", ssd),session,isolate(myLocalUUID()))
  # 
  #     startDateSim(as.Date(ssd))
  #     isolate(updateNumericInput(session, "vaccPop", value = vaccRate()))
  #     isolate(updateNumericInput(session, "vaccPopBooster", value = vaccRateBooster()))
  #   }
  # 
  # 
  #   shiftPlotBy<-ssd-isolate(valueMemory$startSimDate)
  #   if(isolate(valueMemory$startSimDate)==as.Date("2035-01-01")) shiftPlotBy<-0
  # 
  # 
  #   if(DEBUG) print(paste0("Slide sprinkleplot by ",shiftPlotBy," days"))
  #   valueMemory$startSimDate<-ssd
  # 
  # 
  #   viewRangeInciVals<-input$viewRangeInci+shiftPlotBy
  #   updateSliderInput(session,"viewRangeInci",
  #                     max=max(ssd+round(simLength()*1.01),today()+1),
  #                     value=viewRangeInciVals
  #   )
  # 
  #   viewRangeBedsVals<-input$viewRangeBeds+shiftPlotBy
  #   updateSliderInput(session,"viewRangeBeds",
  #                     max=max(ssd+round(simLength()*1.01),today()+1),
  #                     value=viewRangeBedsVals
  # 
  #   )
  # 
  #   viewviewRangeRt<-input$viewRangeRt+shiftPlotBy
  #   updateSliderInput(session,"viewRangeRt",
  #                     max=max(ssd+round(simLength()*1.01),today()+1),
  #                     value=viewviewRangeRt
  #   )
  # 
  #   viewviewRangeVacc<-input$viewRangeVacc+shiftPlotBy
  #   updateSliderInput(session,"viewRangeVacc",
  #                     max=max(ssd+round(simLength()*1.01),today()+1),
  #                     value=viewviewRangeVacc
  #   )
  # 
  #   print("Done here")
  # 
  # })
  
  observeEvent(input$runLength, {
    #if(DEBUG)
    reportLine("input$runLength changed",session,isolate(myLocalUUID()))
    #if(input$runLength<7) updateNumericInput(session,"runLength",value = 7)
    #Checking for a minimum of 7 is not possible in this way, because slowly
    #typing "60" will trigger the observeEvent at "6", and set it to "7" after
    #the user types the finishing "0".
    #This only works for a minimum of 1.
    
    readInputLength<-simLength()
    #if(is.na(readInputLength)) {
    #  readInputLength<-1
    #  updateNumericInput(session,"runLength",value = 1)
    #}
    if(readInputLength<1) updateNumericInput(session,"runLength",value = 1)
    #if(readInputLength>365) updateNumericInput(session,"runLength",value = 365)

    if(readInputLength>90) updateNumericInput(session,"runLength",value = 90)

    #readInputLength<-simLength()
    print(readInputLength)
    print(valueMemory$runLength)
    shiftPlotBy<-readInputLength-isolate(valueMemory$runLength)
    # if(DEBUG)
    print(paste0("extend plots by ",shiftPlotBy," days"))
    valueMemory$runLength<-readInputLength
    
    viewRangeInciVals<-input$viewRangeInci+c(0,shiftPlotBy)
    updateSliderInput(session,"viewRangeInci",
                      max=max(startDateSim()+round(readInputLength*1.01),today()+1),
                      value=viewRangeInciVals
    )
    
    viewRangeBedsVals<-input$viewRangeBeds+c(0,shiftPlotBy)
    updateSliderInput(session,"viewRangeBeds",
                      max=max(startDateSim()+round(readInputLength*1.01),today()+1),
                      value=viewRangeBedsVals
    )
    
    viewRangeRtVals<-input$viewRangeRt+c(0,shiftPlotBy)
    updateSliderInput(session,"viewRangeRt",
                      max=max(startDateSim()+round(readInputLength*1.01),today()+1),
                      value=viewRangeRtVals
    )
    
    viewviewRangeVacc<-input$viewRangeVacc+c(0,shiftPlotBy)
    updateSliderInput(session,"viewRangeVacc",
                      max=max(startDateSim()+round(simLength()*1.01),today()+1),
                      value=viewviewRangeVacc
    )
  })
  
  bedFileName<-function(){##TODO: reactive?
    isolate(bfn<-input$bedfile1)
    return(bfn)
  }
  
  observeEvent(input$selectDataPoint, {
    automatedBedNumber(T)
    reportLine(paste0("selectDataPoint: ",input$selectDataPoint),session,isolate(myLocalUUID()))
    if(input$selectDataPoint=="../../.... - . - . "){
      if(DEBUG) print("Nothing to see here")
    } else {
      selectedDate<-as.Date(str_sub(input$selectDataPoint,1,10))
      bedObservations <- obsData()
      print(bedObservations[nrow(bedObservations)-(10:0),])
      nObs=which(bedObservations[,"Date"]==selectedDate)[[1]]
      if(DEBUG) print(paste0("nObs = ",nObs))
      print("updating ICU/GW numbers place 2")
      isolate(updateNumericInput(session, "inputNumGW", value = bedObservations[[nObs,"Normal"]]))
      isolate(updateNumericInput(session, "inputNumICU", value = bedObservations[[nObs,"ICU"]]))
      
      isolate(updateNumericInput(session, "vaccPop", value = vaccRate()))
      #updateDateInput(session,"startSimDate",value=bedObservations[[nObs,"Date"]])
      startDateSim(bedObservations[[nObs,"Date"]])
      print(paste0("Going for ",bedObservations[nObs,]))
      
      
      ssd<-bedObservations[[nObs,"Date"]]
      # if((startDateSim())!=ssd) {
      #   
      #   reportLine(paste0("setting startDateSim from ", isolate(startDateSim())," to ", ssd),session,isolate(myLocalUUID()))
      #   
      #   startDateSim(as.Date(ssd))
      #   isolate(updateNumericInput(session, "vaccPop", value = vaccRate()))
      #   isolate(updateNumericInput(session, "vaccPopBooster", value = vaccRateBooster()))
      # }
      
      
      shiftPlotBy<-ssd-isolate(valueMemory$startSimDate)
      if(isolate(valueMemory$startSimDate)==as.Date("2035-01-01")) shiftPlotBy<-0
      
      
      if(DEBUG) print(paste0("Slide sprinkleplot by ",shiftPlotBy," days"))
      valueMemory$startSimDate<-ssd
      
      
      viewRangeInciVals<-input$viewRangeInci+shiftPlotBy
      updateSliderInput(session,"viewRangeInci",
                        max=max(ssd+round(simLength()*1.01),today()+1),
                        value=viewRangeInciVals
      )
      
      viewRangeBedsVals<-input$viewRangeBeds+shiftPlotBy
      updateSliderInput(session,"viewRangeBeds",
                        max=max(ssd+round(simLength()*1.01),today()+1),
                        value=viewRangeBedsVals
                        
      )
      
      viewviewRangeRt<-input$viewRangeRt+shiftPlotBy
      updateSliderInput(session,"viewRangeRt",
                        max=max(ssd+round(simLength()*1.01),today()+1),
                        value=viewviewRangeRt
      )
      
      viewviewRangeVacc<-input$viewRangeVacc+shiftPlotBy
      updateSliderInput(session,"viewRangeVacc",
                        max=max(ssd+round(simLength()*1.01),today()+1),
                        value=viewviewRangeVacc
      )
      
    }
    automatedBedNumber(T)
  })
  
  observeEvent(input$P1T, {
    if(input$P1T=="Exponential"){
      disable("P1S")
    } else {
      enable("P1S")
    }
  })
  observeEvent(input$P2T, {
    if(input$P2T=="Exponential"){
      disable("P2S")
    } else {
      enable("P2S")
    }
  })
  observeEvent(input$P3T, {
    if(input$P3T=="Exponential"){
      disable("P3S")
    } else {
      enable("P3S")
    }
  })
  observeEvent(input$P4T, {
    if(input$P4T=="Exponential"){
      disable("P4S")
    } else {
      enable("P4S")
    }
  })
  observeEvent(input$P5T, {
    if(input$P5T=="Exponential"){
      disable("P5S")
    } else {
      enable("P5S")
    }
  })
  observeEvent(input$SIDT, {
    if(input$SIDT=="Exponential"){
      disable("SIDS")
    } else {
      enable("SIDS")
    }
  })
  
  resetToUKF<-function(){
    updateSelectInput(session,"SIDT",selected="Gamma")
    updateNumericInput(session,"SIDM",value=5.0) #round(1.87/0.28,digits=2))
    updateNumericInput(session,"SIDS",value=round(sqrt(1.87*((1/0.28)^2)),digits=2))
    
    updateSelectInput(session,"losGWT",selected="Exponential")
    updateNumericInput(session,"losGWM",value=11.5)
    updateNumericInput(session,"losGWS",value=11.5)
    updateNumericInput(session,"propGW2IC",value=7)
    
    
    #updateSelectInput(session,"losICT",selected="Weibull")
    #updateNumericInput(session,"losICM",value=16.5)
    #updateNumericInput(session,"losICS",value=14.5)
    
    updateSelectInput(session,"losICT",selected="Exponential")
    updateNumericInput(session,"losICM",value=8.5)
    updateNumericInput(session,"losICS",value=8.5)
    
    
    updateNumericInput(session,"propIC2SD",value=40)
    
    updateSelectInput(session,"losSDT",selected="Weibull")
    updateNumericInput(session,"losSDM",value=22)
    updateNumericInput(session,"losSDS",value=17)
  }
  
  observeEvent(input$resetButton, {
    resetToUKF()
  })
  
  observeEvent(input$useETS, {
    reportLine(paste0("useETS set to ",input$useETS),session,isolate(myLocalUUID()))
    
  })
  
  output$exportButtonInci <- downloadHandler(
    filename = function() {
      paste("Exported_Incidence_",Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      reportLine("exportButtonInci",session,isolate(myLocalUUID()))
      if(input$wklyPattern){
        plotVar<-"wklyReported"
      } else {
        plotVar<-"reportedEpi"
      }
      runResults <- mrRes()
      summaryRes<-runResults %>%
        group_by(time) %>%
        summarise(origRep=min(report),
                  med = quantile(get(plotVar),probs=0.5),
                  #  mea = mean(reportedRaw),
                  Q05 = quantile(get(plotVar),probs=0.05),
                  Q25 = quantile(get(plotVar),probs=0.25),
                  Q75 = quantile(get(plotVar),probs=0.75),
                  Q95 = quantile(get(plotVar),probs=0.95)
        ) %>%
        as.data.frame()
      
      summaryRes[summaryRes$time<=startDateSim(),c("med","Q05","Q25","Q75","Q95")]<-0
      
      #print(summaryRes)
      write.csv(summaryRes
                , file
                , row.names=F
      )
    }
  )
  
  output$exportButton <- downloadHandler(
    # EXPORT THE BED DATA
    filename = function() {
      paste("Exported_BedOccupancy_",Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      reportLine("exportButton",session,isolate(myLocalUUID()))
      runResults <- mrRes()
      bedplotdata<-bedPlot()
      bedplotdata$time<-min(runResults$time)+bedplotdata$time
      bedplotdata<-subset(bedplotdata, time >= startDateSim())
      write.csv(bedplotdata, file, row.names = FALSE)
    }
  )
  
  output$exportButtonRt <- downloadHandler(
    
    # EXPORT THE BED DATA
    filename = function() {
      paste("Exported_Rt_",Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      reportLine("exportButtonRt",session,isolate(myLocalUUID()))
      erres<-as.data.frame(epi_result())
      write.csv(erres, file, row.names = FALSE)
    }
  )
  
  output$exportButtonVacc <- downloadHandler(
    # EXPORT THE BED DATA
    filename = function() {
      paste("Exported_Vaccinations_",Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      reportLine("exportButtonVacc",session,isolate(myLocalUUID()))
      vaccData <- vaccTimeSeries()
      vaccData$isForecast<-"N"
      vaccData[vaccData$date>startDateSim(),"isForecast"]<-"Y"
      write.csv(vaccData, file, row.names = FALSE)
    }
  )
  
  observeEvent(input$nrofruns, {
    readInputRuns<-input$nrofruns
    if(is.na(readInputRuns)){
      isolate(updateNumericInput(session,"nrofruns",value = 1))
      readInputRuns<-1
    }
    if(readInputRuns<1){
      isolate(updateNumericInput(session,"nrofruns",value = 1))
      readInputRuns<-1
    }
    if(readInputRuns>100){
      isolate(updateNumericInput(session,"nrofruns",value = 100))
      readInputRuns<-100
    }
    
    if((numberOfRuns())!=readInputRuns) {
      reportLine(paste0("setting numberOfRuns from ", isolate(numberOfRuns())," to ", readInputRuns),session,isolate(myLocalUUID()))
      numberOfRuns(as.integer(readInputRuns))
    }
  })
  
  simLength<-reactive({
    reportLine(paste0("simLength reacted, asked for ",input$runLength),session,isolate(myLocalUUID()))
    
    readInputLength<-input$runLength
    if(is.na(readInputLength)){
      updateNumericInput(session,"runLength",value = 1)
      return(1)
    } else {
      if(readInputLength<1){
        return(1)
      } else {
        if(checkBegrenzerPassed(numberOfRuns(),readInputLength)) {
          return(readInputLength)
        } else {
          resetRunParams()
          ShowBegrenzer()
          return(1) # the bare minimum
        }
      }
    }
  })
  
  SerialIntervalDistr<-reactive({
    if(DEBUG) print("SerialIntervalDistr")
    validate(
      need(input$SIDM, 'Please check your input at the serial interval Mean parameter, it seems to be invalid'),
      need(input$SIDS, 'Please check your input at the serial interval St. Dev. parameter, it seems to be invalid')
    )
    return(getDistr(input$SIDT,input$SIDM,input$SIDS))
  })
  
  inciData<- reactive({
    reportLine("inciData",session,isolate(myLocalUUID()))
    
    runLength<-isolate(input$runLength)
    
    temp<-getInciData(dataLoc())
    
    #temp<-incidenceDataGlobal
    
    if(DEBUG) print(paste0("startSimDate ",isolate(startDateSim())))
    
    if(isolate(startDateSim())>(max(temp[,"Updated"])-1)){
      
      if(DEBUG) print(paste("Need to adjust"))
      # updateDateInput(session,"startSimDate",
      #                 value=max(temp[,"Updated"])-1,
      #                 max=max(temp[,"Updated"])-1,
      #                 min=min(temp[,"Date"])+14)
      startDateSim(max(temp[,"Updated"])-1)
    }
    
    #Is this following updateDateInput still needed?
    print("Adjusting zooming")
    isolate({
     # updateDateInput(session,"startSimDate",max=max(temp[,"Updated"])-1,min=min(temp[,"Date"])+14)
      updateSliderInput(session,"viewRangeRKI",max=max(temp[,"Updated"])-1,min=min(temp[,"Date"])+14,value=c(min(temp[,"Date"])+14,max(temp[,"Updated"])-1))
      updateSliderInput(session,"viewRangeRt",max=max(temp[,"Updated"])+runLength,min=min(temp[,"Date"])+14,value=c(max(temp[,"Updated"])-runLength,max(temp[,"Updated"])))
      updateSliderInput(session,"viewRangeBeds",max=max(temp[,"Updated"])+runLength,min=min(temp[,"Date"])+14,value=c(max(temp[,"Updated"])-runLength,max(temp[,"Updated"])+runLength))
      updateSliderInput(session,"viewRangeVacc",max=max(temp[,"Updated"])+runLength,min=min(temp[,"Date"])+14,value=c(as.Date("2021-01-01"),max(temp[,"Updated"])+runLength))
      updateSliderInput(session,"viewRangeInci",max=max(temp[,"Updated"])+runLength,min=min(temp[,"Date"])+14,value=c(max(temp[,"Updated"])-runLength,max(temp[,"Updated"])+runLength))
    }
    )
    
    return(temp)
  })
  
  districtPops<-reactive({
    #print("Triggered districtPops")
    #return(populationDataGlobal)
    return(getDistrictPops())
  })
  
  vaccDelay<-reactive({
    ret<-getVaccDelay(vaccTable(),districtsMultiVal(),districtPops(),startDateSim())
    return(ret)
  })
  
  vaccRate<-reactive({
    #print("Triggered vaccRate")
    vdel<-vaccDelay()
    
    myVaccTable<-vaccTable()
    
    selDistrictIDs<-getDistrictCodes(districtsMultiVal(),districtPops())
    selectedPops<-myVaccTable[myVaccTable$idDistrict %in% selDistrictIDs,]
    
    TSdf<-selectedPops%>%
      group_by(Date)%>%
      summarise(Pop=sum(Pop),
                peopleFirstTotal=sum(peopleFirstTotalDistrict),
                peopleFullTotal=sum(peopleFullTotalDistrict),
                peopleBoosterTotal=sum(peopleBoosterTotalDistrict)
      )%>%as.data.frame()
    #Insert missing dates
    TSdf<-merge(data.frame(Date=min(TSdf$Date)+(0:as.numeric(max(TSdf$Date)-min(TSdf$Date)))),TSdf,by="Date",all=T)
    
    #browser()
    TSdf$Pop<-extraInterPolate(TSdf$Pop)
    TSdf$peopleFirstTotal<-round(extraInterPolate(TSdf$peopleFirstTotal))
    TSdf$peopleFullTotal<-round(extraInterPolate(TSdf$peopleFullTotal))
    TSdf$peopleBoosterTotal<-round(extraInterPolate(TSdf$peopleBoosterTotal))
    
    return(round(((TSdf[TSdf$Date==startDateSim(),"peopleFirstTotal"])-
                    TSdf[TSdf$Date==(startDateSim()-7),"peopleFirstTotal"])
                   / 7))
  })
  

  vaccRateBooster<-reactive({
    
    myVaccTable<-vaccTable()
    
    selDistrictIDs<-getDistrictCodes(districtsMultiVal(),districtPops())
    selectedPops<-myVaccTable[myVaccTable$idDistrict %in% selDistrictIDs,]
    
    TSdf<-selectedPops%>%
      group_by(Date)%>%
      summarise(Pop=sum(Pop),
                peopleFirstTotal=sum(peopleFirstTotalDistrict),
                peopleFullTotal=sum(peopleFullTotalDistrict),
                peopleBoosterTotal=sum(peopleBoosterTotalDistrict)
      )%>%as.data.frame()
    #Insert missing dates
    TSdf<-merge(data.frame(Date=min(TSdf$Date)+(0:as.numeric(max(TSdf$Date)-min(TSdf$Date)))),TSdf,by="Date",all=T)
    
    #browser()
    TSdf$Pop<-extraInterPolate(TSdf$Pop)
    TSdf$peopleFirstTotal<-round(extraInterPolate(TSdf$peopleFirstTotal))
    TSdf$peopleFullTotal<-round(extraInterPolate(TSdf$peopleFullTotal))
    TSdf$peopleBoosterTotal<-round(extraInterPolate(TSdf$peopleBoosterTotal))
    
    return(round(((TSdf[TSdf$Date==startDateSim(),"peopleBoosterTotal"])-
                    (TSdf[TSdf$Date==(startDateSim()-7),"peopleBoosterTotal"])
    ) / 7))
  })


  vaccTable<-reactive({
    #print("Triggered vaccTable")
    #reportLine("Triggered vaccTable",session,isolate(myLocalUUID()))
    vt<-getVaccTable(dataLoc(),districtPops()) #TODO: modify for selected selDistrictIDs
    #vt<-vaccinationDataGlobal
    return(vt)
  })
  
  weightedMeanVacc<-reactive({
    # print("Triggered weightedMeanVacc")
    # reportLine("weightedMeanVacc",session,isolate(myLocalUUID()))
    req(districtsMultiVal())
    myVaccTable<-vaccTable()
    myVaccTable<-myVaccTable[myVaccTable$date==startDateSim(),]
    
    selDistrictIDs<-getDistrictCodes(districtsMultiVal(),districtPops())
    selectedPops<-myVaccTable[myVaccTable$idDistrict %in% selDistrictIDs,]
    
    return(sum(selectedPops$peopleFullTotalDistrict)/sum(selectedPops$Pop))
  })
  
  bedNumbersData<-reactive({
    #return(bedDataGlobal)
    reportLine("bedNumbersData",session,isolate(myLocalUUID()))
    return(readHistoricalBedInfo(dataLoc(),districtPops(),maxDate=max(inciData()$Date)))
  })
  
  autoSelectedBedNums<-reactive({
    reportLine("autoSelectedBedNums",session,isolate(myLocalUUID()))
    req(districtsMultiVal())
    dp<-districtPops()
    bnd<-bedNumbersData()
    print(bnd)
    theBeds<-getSelectedBedNumbers(bnd,districtsMultiVal(),dp)
    return(theBeds)
  })
  
  selectedInci <- reactive({
    reportLine("Triggered selectedInci",session,isolate(myLocalUUID()))
    req(districtsMultiVal())
    dateandinc<-getSelectedInci(inciData(), districtsMultiVal(),districtPops())
    return(dateandinc)
  })
  
  obsDataInput<-reactive({
    return(data.frame(
      Date=startDateSim(),
      ICU=numICU(),
      Normal=numGW(),
      Hosp=numGW()+numICU(),
      automated=FALSE
    ))
  })
  
  obsData <- reactive({#TODO Input here a general data frame for the downloaded bed data.
    # print("Triggered obsData")
    reportLine("obsData triggered",session,isolate(myLocalUUID()))
    
    if(is.null(input$bedfile1)){
      beds<-autoSelectedBedNums()
      obsData<-beds
    } else {
      if(DEBUG) print('OBSDATA')
      #Data loading should be done separately.  TODO
      
      req(input$bedfile1)
      print(paste0("input file is ",input$bedfile1[[4]]))
      if(file_ext(input$bedfile1[[4]]) %in% c("xls","xlsx")) {
        if(DEBUG)print("This is an excel file")
        obsData <- as.data.frame(readxl::read_excel(paste0(input$bedfile1[[4]]), 1))
      }
      if(file_ext(input$bedfile1[[4]]) %in% c("csv","tsv")) {
        if(DEBUG)print("This is an delimited text file")
        obsData<-read.csv(input$bedfile1$datapath,sep=";")
        if(ncol(obsData)<3) {#Guess the separator
          obsData<-readcsvguessed(input$bedfile1$datapath)
        }
      }
      obsData$Date<-as.Date(parse_date_time(obsData$Date,c("Ymd","dmY","dmy", "ymd","ydm","mdy")))
      obsData$Hosp<-obsData$ICU+obsData$Normal
      obsData$automated<-T
    }
    #Remove the input date from loaded data and replace with input
    obsData$automated=TRUE
    obsData<-rbind(obsDataInput(),obsData)
    obsData<-obsData[order(obsData$Date),]
    print(obsData[nrow(obsData)-10:0,])
    
    if(automatedBedNumber()) obsData<-obsData[obsData$automated,] else
      obsData<-obsData[
        ((obsData$Date%in%obsDataInput()$Date)&(!obsData$automated))|((!(obsData$Date%in%obsDataInput()$Date))&(obsData$automated))
        ,]
    
    print(obsData[nrow(obsData)-10:0,])
    
    obsData<-unique(obsData)
    obsData<-obsData[!is.na(obsData$Hosp),]
    obsData<-obsData[obsData$Date<today(),] #TODO should be the Updated
    #}
    
    
    #if(DEBUG) print
    print("returning ObsData")
    return(obsData)
  })
  
  numICUdata<-reactive({
    return(obsData()[obsData()$Date==startDateSim(),"ICU"][1])
  })
  
  numGWdata<-reactive({
    return(obsData()[obsData()$Date==startDateSim(),"Normal"][1])
  })
  
  output$bedfile1Uploaded <- reactive({
    return(!is.null(input$bedfile1))
  })
  
  outputOptions(output, 'bedfile1Uploaded', suspendWhenHidden=FALSE)
  numGW<-reactiveVal(20)
  numICU<-reactiveVal(10)
  
  LOSdistr<-reactive({
    return(getLOSdistr(withinHospPara()))
  })
  
  propVaccPerDay<-reactive({
    popsize <-  selectedInci()[1,5]
    propPerDay<-round(vaccinPop())/(popsize*2) # Assuming two dose immunity
    return(propPerDay)
  })
  
  vaccIntersect<-reactive({
    return((today()-1)-round(weightedMeanVacc()/propVaccPerDay()))
  })
  
  endRunVaccProp<-reactive({
    alreadyVaccAtRunbegin<-(startDateSim()-vaccIntersect())*propVaccPerDay()
    addedDuringRun<-propVaccPerDay()*simLength()
    return(max(min(1,(alreadyVaccAtRunbegin+addedDuringRun)),0))
  })
  
  withinHospPara=reactive({
    paraList<-list(
      propGW2IC=input$propGW2IC,
      propIC2SD=input$propIC2SD,
      losGWT=input$losGWT,
      losGWM=input$losGWM,
      losGWS=input$losGWS,
      losICT=input$losICT,
      losICM=input$losICM,
      losICS=input$losICS,
      losSDT=input$losSDT,
      losSDM=input$losSDM,
      losSDS=input$losSDS,
      propManual=input$manualDirect,
      propDirects=input$directICUval
    )
    return(paraList)
  })
  
  replacementPatients<-reactive({
    return(createReplacementPatients(withinHospPara()))
  })

  getParameters<-reactive({
    mrres<-mrRes()
    whp<-withinHospPara()
    admRateEsti<-admRateEstimation()
    endDate <- startDateSim() 
    directICUProp<-getPropDirect(startDateSim(),admRateEsti,whp)
    numRuns<-numberOfRuns() 
    startOffset=as.numeric(startDateSim()-min(mrres[,"Date"]))#-100 #The -100 is for double checking if the estimated admission rate & proportion direct to ICU get to the correct initial values

    #thisAdmProps<-admRateEsti[admRateEsti$Date==startDateSim(),"admProps"]
    thisAdmProps<-admRateEsti[,"admProps"]
    startAdmProp<-admRateEsti[admRateEsti$Date==startDateSim(),"admProps"][[1]]
    startReinf<-admRateEsti[admRateEsti$Date==startDateSim(),"wPropReinf"][[1]]
    primaryHrisk<-startAdmProp / ((1-startReinf)+((input$relHospRisk/100)*startReinf))
    
    adjAdmProps<-(admRateEsti[(admRateEsti$Date<=(startDateSim()+simLength())),"propReinf"]*primaryHrisk*(input$relHospRisk/100))+((1-admRateEsti[(admRateEsti$Date<=(startDateSim()+simLength())),"propReinf"])*primaryHrisk)
    adjAdmProps<-pmax(pmin(adjAdmProps,1),0)
    adjAdmProps[is.na(adjAdmProps)]<-0

    if(is.na(directICUProp)|is.null(numGWdata())|is.null(numICUdata())){
      args<-NULL
    } else {
      args <- list(
        thisParaVersion=isolate(parameterVersion()+1),
        numRuns = as.integer(numRuns),
        mrres = mrres[mrres$Date<startDateSim()+simLength(),c("runNum","underlying")],
        observedBeds <- obsData(),
        thisAdmProps = adjAdmProps,
        directICUProp = directICUProp,
        propGW2IC = whp$propGW2IC,
        propIC2SD = whp$propIC2SD,
        losGWT = whp$losGWT,
        losGWM = whp$losGWM,
        losGWS = whp$losGWS,
        losICT = whp$losICT,
        losICM = whp$losICM,
        losICS = whp$losICS,
        losSDT = whp$losSDT,
        losSDM = whp$losSDM,
        losSDS = whp$losSDS,
        replacements = (replacementPatients()),
        numGW = as.integer(numGWdata()),
        numICU = as.integer(numICUdata()),
        startOffset = startOffset,
        tzero=startDateSim()-startOffset
      )
    }
    return(args)
  })
  
  parameters<-reactive({
    print("Requesting parameters")
    args<-getParameters()
    if(!is.null(args)) parameterVersion(args$thisParaVersion)
    return(args)
  })
  
  observeEvent({
    parameterVersion() # this can be made more efficient by listening only to relevant stuff
  },
  if(parameterVersion()>0){
    #print("Parameter version changed.")
    #print('Start julia implementation')
    reportLine("Parameter version changed. Start julia implementation",session,isolate(myLocalUUID()))
    args <- parameters()
    #print("Parameters ready")
    if(!is.null(args)) temp<-produceBedPlotFunction(args)
  } else {
    print("Initial parameter version change trigger, ignore.")
    
  }
  )
  
  #####################################
  #     Major simulation triggers     #
  #####################################
  
  nowcasted<-reactive({
    reportLine("Nowcasting",session,isolate(myLocalUUID()))
    withProgress(message = "Nowcasting " , value = 0, {
      incProgress(0, detail = "...")
      res<-getNowcast(selectedInci(),numberOfRuns(),SerialIntervalDistr())
      setProgress(1,detail = "Finished")
    })
    return(res)
  })
  
  
  epi_result <- reactive({
    req(selectedInci(),nowcasted(),SerialIntervalDistr(),input$startVOCdate,input$advanVOC,input$startVOCprop)
    nc<-nowcasted()
    reportLine("Calculating Historical R",session,isolate(myLocalUUID()))
    return(get_Historical_R(selectedInci(),
                            nc,
                            vaccTimeSeries(),
                            SerialIntervalDistr(),
                            input$startVOCdate,
                            #(1+(input$advanVOC/100))*ImmEvasionAdvantage(),
                            (1+(input$advanVOC/100)),
                            ImmEvasionAdvantage(),
                            input$startVOCprop,
                            serialInt=input$SIDM,
                            crossIm=1-(input$immEvasionVOC/100)
                            
                            )
           )
  })
  
  ImmEvasionAdvantage<-reactive({
    nc<-nowcasted()
    vts<-vaccTimeSeries()
    epicurve<-nc$ncMean$epiCurveNowcast
    inci=selectedInci()
    popSize<-max(inci$Pop)
    immEv<-input$immEvasionVOC/100
    if(sum(vts$Date==input$startVOCdate)==0){
      vProtect<-0
    } else {
      vProtect<-vts[vts$Date==input$startVOCdate,"devVaccProtect"][[1]]
    }
    immune1<-popSize-round((popSize-sum(inci[inci$Date<=input$startVOCdate,"Cases"]))*(1-vProtect))
    immEvAdv<-(1-((immune1/popSize)*(1-immEv)))/(1-(immune1/popSize))
    return(immEvAdv)
  })
  
  RdevCurves<-reactive({
    er<-epi_result()
    immEvAdv<-ImmEvasionAdvantage()
    reportLine("Forecasting R",session,isolate(myLocalUUID()))
    Rdevs<-getRdevCurves(er,
                         startDateSim(),
                         numberOfRuns(),
                         simLength(),
                         input$startVOCdate,
                         (1+(input$advanVOC/100)),
                         immEvAdv,
                         input$startVOCprop,
                         serialInt=input$SIDM,
                         etsAlpha=input$ETSalpha,#0.25,
                         etsBeta=input$ETSbeta,#0.15,
                         etsLength=input$ETSlength#100
                         )
    return(Rdevs)
  })
  
  vaccTimeSeries<-reactive({
    reportLine("Estimating / forecasting vaccination coverage",session,isolate(myLocalUUID()))
    # validate(
    #   need(input$protDelay1M, 'Your parameter input for the mean delay until protection after first dose seems to be invalid'),
    #   need(input$protDelay1S, 'Your parameter input for the St.Dev. delay until protection after first dose seems to be invalid'),
    #   need(input$protDelay2M, 'Your parameter input for the mean delay until protection after second dose seems to be invalid'),
    #   need(input$protDelay1S, 'Your parameter input for the St.Dev. delay until protection after second dose seems to be invalid'),
    #   need(input$protEffac1, 'Your parameter input for the first dose efficacy seems to be invalid'),
    #   need(input$protEffac2, 'Your parameter input for the second dose efficacy seems to be invalid'),
    # )
    TSdf<-getVaccTimeSeries(selectedInci(),
                            vaccTable(),
                            districtPops(),
                            startDateSim(),vaccinPop(),vaccinPopBooster(),vaccinDelayBooster(),simLength(),districtsMultiVal(),
                            input$protDelay1T,input$protDelay1M,input$protDelay1S,input$protEffac1,
                            input$protDelay2T,input$protDelay2M,input$protDelay2S,input$protEffac2,
                            input$protDelay3T,input$protDelay3M,input$protDelay3S,input$protEffac3)
    return(TSdf)
  })
  
  mrRes <- reactive({
    nc<-nowcasted()
    rdc<-RdevCurves()
    vts<-vaccTimeSeries()
    reportLine("Running incidence model",session,isolate(myLocalUUID()))
    mrRes<-incidenceModel(
                          selectedInci(),
                          Rdevs=rdc,
                          vaccTimeSeries=vts,
                          startDateSim(),
                          simLength(),
                          numberOfRuns(),
                          SerialIntervalDistr(),
                          input$inclVOC,
                          input$useETS,
                          (1+(input$advanVOC/100)),
                          ImmEvasionAdvantage(),
                          crossIm=1-(input$immEvasionVOC/100)
    )
    
    return(mrRes)
  })
  
  
  summaryRes <- reactive({
    plotVar<-"reportedEpi"
    summaryRes<-mrRes() %>%
      group_by(time) %>%
      summarise(origRep=min(report),
                med = quantile(get(plotVar),probs=0.5,na.rm=T),
                #  mea = mean(reportedRaw),
                Q05 = quantile(get(plotVar),probs=0.05,na.rm=T),
                Q25 = quantile(get(plotVar),probs=0.25,na.rm=T),
                Q75 = quantile(get(plotVar),probs=0.75,na.rm=T),
                Q95 = quantile(get(plotVar),probs=0.95,na.rm=T)
      ) %>%
      as.data.frame()
    #print('recalculate summary:')
    #print(summaryRes$Q95)
    return(summaryRes)
  })
  
  yzoomValues <- reactive({
    summaryRes <- summaryRes()
    
    xlimits<-input$viewRangeInci
    
    subRunResults<-summaryRes[summaryRes$time>=xlimits[1]&summaryRes$time<=xlimits[2],]
    
    minval <- 0
    maxval <-max(1, max(subRunResults$Q95,na.rm = T))
    extmaxval <- maxval*1.1

    if (input$yZoomSlider[1] == 0 && input$yZoomSlider[2] == 0  ) {
      ylimits=c(minval,extmaxval)
      isolate(updateNoUiSliderInput(session, "yZoomSlider", value=c(minval,extmaxval) ))
    } else {
      ylimits=c(max(input$yZoomSlider[1],0),min(input$yZoomSlider[2],extmaxval))
      isolate(updateNoUiSliderInput(session, "yZoomSlider", range=c(minval,extmaxval) ))
    }
    
    print(ylimits)
    return(ylimits)
  })

  yzoomBedsValues <- reactive({#getBedShadedPlot
    summaryRes <- bedPlot()
    
    startD=min(mrRes()$time)
    summaryRes$Date<-startD+summaryRes$time
    xlimits<-input$viewRangeBeds
    
    subRunResults<-summaryRes[summaryRes$Date>=xlimits[1]&summaryRes$Date<=xlimits[2],]
    
    beds<-isolate(obsData())
    
    minval <- 0
   
    if(input$onlyICU){
      maxval=max(c(subRunResults$ICUQ95,
                        subRunResults$ICUQ50*1.1,
                        beds$ICU))
    }else{
      maxval=max(c(subRunResults$ICUQ95,
                        subRunResults$GWQ95,
                        subRunResults$GWQ50*1.1,
                        subRunResults$ICUQ50*1.1,
                        beds$ICU,
                        beds$Normal))
    }    
    
    if (input$yZoomSliderBeds[1] == 0 && input$yZoomSliderBeds[2] == 0 ) {
      ylimits=c(minval,maxval)
      isolate(updateNoUiSliderInput(session, "yZoomSliderBeds", value=c(minval,maxval) ))
    } else {
      ylimits=c(max(input$yZoomSliderBeds[1],0),min(input$yZoomSliderBeds[2],maxval))
      isolate(updateNoUiSliderInput(session, "yZoomSliderBeds", range=c(minval,maxval) ))
    }
    
    print(ylimits)
    return(ylimits)
  })
  
  admRateEstimation <- reactive({
    reportLine("Estimating admission rate",session,isolate(myLocalUUID()))
    obsDataTemp<-obsData()
    selInci<-selectedInci()
    mrr<-mrRes()
    losDis<-LOSdistr()
    resDF<-admRateEstimator(mrr,withinHospsPara(),obsData=obsDataTemp,losDistr=losDis,DEBUG=FALSE)
    return(resDF)
  })
  
  
  produceBedPlotFunction<-function(args){
    #initstart <- Sys.time()
    bedPlot(NULL)
    
    pleaseWait_bedPlot <- modalDialog(
      title = "Running care path model",
      "Please wait...",
      easyClose = FALSE,
      footer = NULL
    )
    
    showModal(pleaseWait_bedPlot)
    
    if(use_future){
      #print('Start julia promise')
      reportLine("Running care path model (in future/promise)",session,isolate(myLocalUUID()))
      catch(
        future_promise({
          
          Sys.setenv("JULIACONNECTOR_SERVER" = paste0("localhost:", juliaPort)) # Necessary since Julia was started after plan(multissession) (S. Lenz is smart indeed!)
          #Sys.sleep(15) # Remove in production
          start <- Sys.time()
          #browser()
          args$mrres$runNum<-as.integer(args$mrres$runNum)
          args$mrres$underlying<-as.integer(args$mrres$underlying)
          print("Running care path model (inside the future/promise)")
          res <- as.data.frame(
            with(args, {IUKCovid$createInHRunWSD(
              numRuns,
              mrres,
              thisAdmProps,
              directICUProp,
              propGW2IC, propIC2SD,
              losGWT, losGWM, losGWS,
              losICT,  losICM, losICS,
              losSDT, losSDM, losSDS,
              numGW, numICU, as.integer(startOffset)
            )
            })
          )
          list(
            thisParaVersion=args$thisParaVersion,
            start = start,
            res = res
          )
          
        },globals = list(juliaPort = getJuliaPort(), args = args, IUKCovid = IUKCovid)) %>%
          then(
            onFulfilled = function(value) {
              #print(paste('Julia promise completed in:', signif(Sys.time() - value$start, 3), "s"))
              reportLine(paste('Julia promise completed in:', signif(Sys.time() - value$start, 3), "s"),session,isolate(myLocalUUID()))
              print(paste0("Checking parameter versions. Simulation: ",value$thisParaVersion,", current: ",isolate(parameterVersion())))
              if(value$thisParaVersion==isolate(parameterVersion())) bedPlot(value$res)
              removeModal()
              print("Done here")
            },
            onRejected = function(err) {
              print(paste0("Checking parameter versions. Simulation: ",args$thisParaVersion,", current: ",isolate(parameterVersion())))
              
              if(args$thisParaVersion==isolate(parameterVersion())) bedPlot(NULL)
              warning("Julia promise failed!")
              #warning(err)
              reportLine("Julia promise failed!",session,isolate(myLocalUUID()))
              reportLine(err,session,isolate(myLocalUUID()))
              
              removeModal()
              showModal(modalDialog(
                title = "An error occurred!",
                "The bed occupancy prediction could not be computed.\n Please write a message to iuk.forecast@uniklinik-freiburg.de.", #put an email
                easyClose = TRUE,
                footer = modalButton("OK")
              ))
            }
          ),
        function(e){
          print(e$message)
          bedPlot(NULL)
        })
    } else {
      reportLine("Running care path model (straightforward)",session,isolate(myLocalUUID()))
      args$mrres$runNum<-as.integer(args$mrres$runNum)
      args$mrres$underlying<-as.integer(args$mrres$underlying)
      res <- as.data.frame(
        with(args, {IUKCovid$createInHRunWSD(
          #"MOOHAHAHA",
          numRuns,
          mrres,
          #select(mrres, runNum, underlying) %>% mutate_all(as.integer),
          thisAdmProps, directICUProp,
          propGW2IC, propIC2SD,
          losGWT, losGWM, losGWS,
          losICT,  losICM, losICS,
          losSDT, losSDM, losSDS,
          numGW, numICU, as.integer(startOffset)
        )
        })
      )
      reportLine("Done care path model (straightforward)",session,isolate(myLocalUUID()))
      bedPlot(res)
    }
    
    #print(paste('Julia promise initialisation completed in:' ,Sys.time() - initstart))
    return(1)
  }
  
  #####################################
  #     figure plot rendering         #
  #####################################
  
  output$inciDataPlot <- renderPlot({
    reportLine("Plot reported incidence",session,isolate(myLocalUUID()))
    reportedIncidencePlot(selectedInci(),nowcasted(),input$viewRangeRKI,i18n)
  })
  
  output$epiestim2 <- renderPlot({
    reportLine("Plot R estimate",session,isolate(myLocalUUID()))
    epiestim2plot(selectedInci(),
                  epi_result(),
                  nowcasted(),
                  Rdevs=RdevCurves(),
                  startDateSim(),
                  simLength(),
                  input$viewRangeRt,
                  input$inclVOC,
                  input$useETS,
                  i18n)
  })
  
  output$resultVacc <- renderPlot({
    reportLine("Plot Vaccinations",session,isolate(myLocalUUID()))
    getVaccPlot(vaccTimeSeries(),
                xlimits=input$viewRangeVacc,
                cutDate=startDateSim(),
                i18n=i18n)
  })
  
  output$resultIncidence <- renderPlot({
    reportLine("Plot incidence forecast",session,isolate(myLocalUUID()))
    ylimits <- yzoomValues()
    getSprinklePlotIQR(mrRes(),
                       xlimits=input$viewRangeInci,
                       ylimits=ylimits,
                       plotWeeklyPattern=input$wklyPattern,
                       i18n=i18n
    )
  })
  
  
  output$resultBeds <- renderPlot({
    reportLine("Plot resultBeds",session,isolate(myLocalUUID()))
    print("rendering...")
    args <- parameters()
    
    if(is.null(bedPlot())) {
      return(ggplot() + theme_minimal())
    } else {
      ylimits=yzoomBedsValues()
      return(getBedShadedPlot(bedPlot(),
                              bothObs=isolate(obsData()),
                              startD=min(mrRes()$time),
                              xlimits=input$viewRangeBeds,
                              #xlimits=c(startDateSim()-round(simLength()*1.05),startDateSim()+round(simLength()*1.05)),
                              ylimits=ylimits,
                              cutDate=startDateSim(),
                              runLength = simLength(),
                              todayDate=startDateSim(),
                              onlyICU = input$onlyICU, i18n=i18n))
    }
  })
  
  
  
  output$SerialIntervalPlot<- renderPlot({
    reportLine("Plot Serial interval (Parameter choices)",session,isolate(myLocalUUID()))
    losDistr<-data.frame(
      Days=1:length(SerialIntervalDistr()),
      Prob=SerialIntervalDistr()
      
    )
    
    ggplot(losDistr)+
      geom_line(aes(x=Days,y=Prob),color="black")+
      scale_x_continuous(limits=c(0,61))+
      labs(x="Time between cases",
           y="Density")+
      theme_minimal(base_size=18)
  },
  height=reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*1.3/5,0))
  )
  
  output$losDistrPlot <- renderPlot({
    losDistr<-LOSdistr()
    ## S3 method for class 'estimate_R'
    ggplot(losDistr)+
      geom_line(aes(x=Days,y=Normal,linetype="Current model"),color="blue")+
      geom_line(aes(x=Days,y=ICU,linetype="Current model"),color="red")+
      scale_x_continuous(limits=c(0,61))+
      scale_linetype_manual(limits=c("Current model","UKF reference"),values=c(1,3))+
      labs(x="Time since admission (days)",
           y="Proportion of patients present"
      )+
      theme_minimal(base_size=18)+
      theme(legend.position = "bottom")
  },
  height=reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*1.5/5,0))#,
  )
  
  
  
  
  #####################################
  #     Text fields rendering         #
  #####################################
  output$populationsize <- renderText({
    popsize <-  selectedInci()[1,5]
    
    str1 <- paste(i18n$t("Population size"))
    str2 <- paste("<b style='font-size:30px;'>",popsize,"</b>")
    HTML(paste(str1, str2, sep = '<br/>'))
    
  })
  
  output$advanCombi <- renderText({
    immEvAdv<-ImmEvasionAdvantage()
    totAdvan<-(1+(input$advanVOC/100))*immEvAdv
    
    str1 <- paste(i18n$t("Combined advantage:"))
    str2 <- paste("<b>",(totAdvan-1)*100,"% </b>")
    HTML(paste(str1, str2, sep = '<br/>'))
    
  })
  
  output$latestICUtext <- renderText({
    beds<-autoSelectedBedNums()
    latestICU<-beds[nrow(beds),"ICU"]
    str1 <- paste(i18n$t("ICU bed occupancy (DIVI registry)"))
    str2 <- paste("<b style='font-size:30px;'>",latestICU,"</b>")
    HTML(paste(str1, str2, sep = '<br/>'))
    
  })
  
  output$admHosp <- renderText({
    admh <- admRateEstimation()
    
    estiDirect<-1-(admh[admh$Date==startDateSim(),"admProps"]/
                     admh[admh$Date==startDateSim(),"admPropsICU"])
    if(length(estiDirect)>1)estiDirect<-estiDirect[length(estiDirect)]
    
    admRate<-(admh[admh$Date==startDateSim(),"admProps"])
    if(length(admRate)>1)admRate<-admRate[length(admRate)]
    
    str1 <- paste(i18n$t("Proportion cases admitted to hospital"))
    str2 <- paste("<b style='font-size:30px;'>", round(100*admRate,digits = 2) ,"% </b>")
    str3 <- paste("<b>", startDateSim(), "</b>")
    
    HTML(paste(str1, str2,str3, sep = '<br/>'))
  })
  
  output$InciValue <- renderText({
    inc <- selectedInci()
    nowcast <- nowcasted()
    ll <- length(inc[,2])
    
    for (i in 0:ll) {
      temp <- inc[ll-i,2]
      if (temp > 0){
        incval <- temp
        incNowcast <- nowcast$ncMean$epiCurveNowcast$I[ll-i]
        incdate <- inc[ll-i,1]
        break
      }
    }
    
    str1 <- paste(i18n$t("Last non-zero RKI incidence (per day)"))
    str2 <- paste("<b style='font-size:30px;'>",incval,"</b>")
    str3 <- paste(i18n$t("Compensated for reporting delay (nowcast)"))
    str4 <- paste("<b style='font-size:30px;'>",incNowcast,"</b>")
    str5 <- paste(i18n$t("from"),"<b>",incdate,"</b>")
    HTML(paste(str1, str2, str3, str4, str5, sep = '<br/>'))
    
  })
  
  output$SevendayInciValue <- renderText({
    inc <- selectedInci()
    nowcast <- nowcasted()
    ll <- length(inc[,2])
    
    for (i in 0:ll) {
      temp <- inc[ll-i,2]
      if (temp > 0){
        incval <- inc[ll-i,4]
        incval <- format(round(incval, 2), nsmall = 2)
        incNowcast <- 100000*(sum(nowcast$ncMean$epiCurveNowcast$I[(ll-i-6):ll-i])/inc$Pop[ll-i])
        incNowcast <- format(round(incNowcast, 2), nsmall = 2)
        incdate <- inc[ll-i,1]
        break
      }
    }
    
    str1 <- paste( i18n$t("Last useful 7-day RKI incidence (per 100 000)") )
    str2 <- paste("<b style='font-size:30px;'>",incval,"</b>")
    str3 <- paste(i18n$t("Compensated for reporting delay (nowcast)"))
    str4 <- paste("<b style='font-size:30px;'>",incNowcast,"</b>")
    str5 <- paste(i18n$t("from"),"<b>",incdate,"</b>")
    HTML(paste(str1, str2, str3, str4, str5, sep = '<br/>'))
  })
  
  
  output$RValue <- renderText({
    inc <- selectedInci()
    datenstand<-max(inc$Date)
    er<-epi_result()
    rvalue=format(round(er[er$Date==datenstand,"R_mean_raw"], 2), nsmall = 2)
    rvalueNowcast<-format(round(er[er$Date==datenstand,"R_mean_nc"], 2), nsmall = 2)
    latestDate<- (er[er$Date==datenstand,"Date"])
    
    str1 <- paste(i18n$t("Last useful eff. R Value based on RKI data"))
    str2 <- paste("<b style='font-size:30px;'>",rvalue,"</b>")
    str3 <- paste(i18n$t("Compensated for reporting delay (nowcast)"))
    str4 <- paste("<b style='font-size:30px;'>",rvalueNowcast,"</b>")
    str5 <- paste(i18n$t("from"),"<b>",latestDate,"</b>")
    HTML(paste(str1, str2, str3,str4,str5, sep = '<br/>'))
  })
  
  output$RValueSim <- renderText({
    er<-epi_result()
    rvalue <- format(round(er[er$Date==startDateSim(),"R_mean_raw"],2), nsmall = 2)
    rvalueNowcast <- format(round(er[er$Date==startDateSim(),"R_mean_nc"],2), nsmall = 2)
    str1 <- paste(i18n$t("Forecast R value based on RKI data"))
    str2 <- paste("<b style='font-size:30px;'>",rvalue,"</b>")
    str3 <- paste(i18n$t("Compensated for reporting delay (nowcast)"))
    str4 <- paste("<b style='font-size:30px;'>",rvalueNowcast,"</b>")
    str5 <- paste(i18n$t("from"),"<b>",(startDateSim()),"</b>")
    HTML(paste(str1, str2, str3,str4,str5, sep = '<br/>'))
  })
  
  
  output$vaccBegin <- renderText({
    req(districtsMultiVal())
    vts<-vaccTimeSeries()
    idx<-nrow(vts)-simLength()
    str1 <- paste(i18n$t("Vaccination coverage at start of the simulation"),"<br/>")
    #str2 <- paste("<b style='font-size:30px;'>",format(round(weightedMeanVacc()*100, 2), nsmall = 2),"% </b>")
    str2 <- paste0("\t",i18n$t("Single dose"),": ")
    str3 <- paste0("<b style='font-size:30px;'>",round(vts[idx,]$peopleFirstProp*100,digits=2),"% </b>")
    str4 <- paste0("\t",i18n$t("Two doses"),": ")
    str5 <- paste0("<b style='font-size:30px;'>",round(vts[idx,]$peopleFullProp*100,digits=2),"% </b>")
    HTML(paste(str1, str2, str3,str4, str5, sep = '<br/>'))
  })
  
  output$vaccEnd <- renderText({
    req(districtsMultiVal())
    vts<-vaccTimeSeries()
    idx<-nrow(vts)
    str1 <- paste(i18n$t("Vaccination coverage at end of the simulation"),"<br/>")
    #str2 <- paste("<b style='font-size:30px;'>",format(round(weightedMeanVacc()*100, 2), nsmall = 2),"% </b>")
    str2 <- paste0("\t",i18n$t("Single dose"),": ")
    str3 <- paste0("<b style='font-size:30px;'>",round(vts[idx,]$peopleFirstProp*100,digits=2),"% </b>")
    str4 <- paste0("\t",i18n$t("Two doses"),": ")
    str5 <- paste0("<b style='font-size:30px;'>",round(vts[idx,]$peopleFullProp*100,digits=2),"% </b>")
    HTML(paste(str1, str2, str3,str4, str5, sep = '<br/>'))
  })
  
  output$vaccSingle <- renderText({
    req(districtsMultiVal())
    vts<-vaccTimeSeries()
    idx1<-nrow(vts)-simLength()
    idx2<-nrow(vts)
    str1 <- paste(i18n$t("Proportion of the population who receive single vaccination dose"),":<br/>")
    str2 <- paste0("\t",i18n$t("Start of the simulation"),": ")
    str3 <- paste0("<b style='font-size:30px;'>",round(vts[idx1,]$peopleFirstProp*100,digits=2),"% </b>")
    str4 <- paste0("\t",i18n$t("End of the simulation"),": ")
    str5 <- paste0("<b style='font-size:30px;'>",round(vts[idx2,]$peopleFirstProp*100,digits=2),"% </b>")
    HTML(paste(str1, str2, str3,str4, str5, sep = '<br/>'))
  })
  
  output$vaccFull <- renderText({
    req(districtsMultiVal())
    vts<-vaccTimeSeries()
    idx1<-nrow(vts)-simLength()
    idx2<-nrow(vts)
    str1 <- paste(i18n$t("Proportion of the population fully vaccinated"),":<br/>")
    str2 <- paste0("\t",i18n$t("Start of the simulation"),": ")
    str3 <- paste0("<b style='font-size:30px;'>",round(vts[idx1,]$peopleFullProp*100,digits=2),"% </b>")
    str4 <- paste0("\t",i18n$t("End of the simulation"),": ")
    str5 <- paste0("<b style='font-size:30px;'>",round(vts[idx2,]$peopleFullProp*100,digits=2),"% </b>")
    HTML(paste(str1, str2, str3,str4, str5, sep = '<br/>'))
  })
  
  output$vacctext <- renderText({
    #What does it need to say?
    # - percentage vaccinated per day
    # - percentage vaccinated at the end of the simulation
    # - Date when entire population is vaccinated
    vaccBeginDate<-today()-1
    
    vts<-vaccTimeSeries()
    str1 <- paste0("<b>",i18n$t("Vaccination results (double dose)"),"</b>")
    str2 <- paste0(i18n$t("Vaccinated at the end of the simulation"),": ",round(endRunVaccProp()*100,digits=2))
    str3 <- paste0("\t",i18n$t("Single dose"),": ",round(vts[nrow(vts),]$peopleFirstProp*100,digits=2))
    str4 <- paste0("\t",i18n$t("Two doses"),": ",round(vts[nrow(vts),]$peopleFullProp*100,digits=2))
    HTML(paste(str1, str2,str3,str4, sep = '<br/>'))
    
  })
  
  output$copyrightiuk <- renderText({
    str1 <- paste("&#169;&nbsp;Copyright:<b> Institut&nbsp;f&uuml;r&nbsp;Infektionspr&auml;vention&nbsp;<br>und&nbsp;Krankenhaushygiene </b>")
    HTML(paste(str1, sep = '<br/>'))
  })
  
  datenstandenText<-function(){
    # str2 <- paste0("Datenstand RKI incidence data: ", max(inciData()$Datenstand))
    str1 <- paste0("<i><b>Incidence data</b></i>",
                   "<br> - Source: <a href=https://www.rki.de>Robert Koch Institut (RKI)</a>",
                   "<br> - Direct link to the <a href=https://opendata.arcgis.com/datasets/9644cad183f042e79fb6ad00eadc4ecf_0.csv>data</a>",
                   "<br> - Updated: ",
                   max(inciData()$Updated)
    )
    
    if(is.na(max(inciData()$dwnldtime))){
      str2 <- paste0("<br> - Cache time unavailable")
    } else {
      str2 <- paste0("<br> - Cached: ",
                     as.character(max(inciData()$dwnldtime)),"<br>")
    }
    str3 <- paste0("<br><br><i><b>Intensive care bed occupancy data</b></i>",
                   "<br> - Source: <a href=https://www.divi.de/register/tagesreport>Deutsche Interdisziplinäre Vereinigung für Intensiv- und Notfallmedizin (DIVI-Intensivregister)</a>",
                   "<br> - Direct link to the <a href=https://diviexchange.blob.core.windows.net/%24web/DIVI_Intensivregister_Auszug_pro_Landkreis.csv>data</a>",
                   "<br> - Datenstand: ",
                   max(bedNumbersData()$daten_stand)
    )
    if(is.na(max(bedNumbersData()$dwnldtime))){
      str4 <- paste0("<br> - Cache time unavailable")
    } else {
      str4<-paste0("<br> - Cached: ",
                   as.character(max(bedNumbersData()$dwnldtime)),"<br>")
    }
    
    str5 <- paste0("<br><br><i><b>Vaccination data</b></i>",
                   "<br> - Source: <a href=https://impfdashboard.de>Bundesministerium für Gesundheit</a> and <a href=https://www.rki.de>Robert Koch Institut (RKI)</a>",
                   "<br> - Direct link to the <a href=https://impfdashboard.de/static/data/germany_vaccinations_by_state.tsv>data</a>",
                   "<br> - Datenstand: <i>Currently unavailable</i>"#,
                   #                   max(vaccTable()$daten_stand)
    )
    if(is.na(max(vaccTable()$dwnldtime))){
      str6 <- paste0("<br> - Cache time unavailable")
    } else {
      str6<-paste0("<br> - Cached: ",
                   as.character(max(vaccTable()$dwnldtime)),"<br>")
    }
    return(paste0(str1,str2,str3,str4,str5,str6))
    
  }
  
  output$manualtext <- renderText({
    reportLine("Manual",session,isolate(myLocalUUID()))
    fileName <- i18n$t("Resources/Manual_EN.html")
    fromFileTxt<-readChar(fileName, file.info(fileName)$size)
    str1 <- paste0(fromFileTxt)
    str2<-""
    HTML(paste(str1, str2, sep = '<br/>'))
  })
  
  output$acknowledgetext <- renderText({
    reportLine("Acknowledgetext",session,isolate(myLocalUUID()))
    fileName <- 'Resources/acktext-en.txt'
    fromFileTxt<-readChar(fileName, file.info(fileName)$size)
    str1 <- paste0(fromFileTxt)
    str2 <- datenstandenText()
    HTML(paste(str1, str2, sep = '<br/>'))
  })
}


# Run the application
shinyApp(ui = ui, server = server)
