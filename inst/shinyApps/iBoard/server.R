options(shiny.maxRequestSize=30*1024^2)

tablist0<-c("exampleTab")

tablist4<-c("searchTab")

tablist <-c("dfChrTab", "dfMarkTab", "dfMSTab")

tablist2<-c("paramTab", "logTab","codeTab")

menulist<-c( "examplesMenu"
             ,"nuccoreMenu"
             ,"DFsMenu"
             ,"parameterPlotMenu"
             ,"aboutMenu"
             )

server <- function(input, output, session) {

  print(paste0("Running in: ",isolate(session$clientData$url_hostname),":",isolate(session$clientData$url_port)) )

  #
  # values

  source(file.path("presetVals.R"),         local = TRUE, chdir = TRUE)

  source(file.path("reactiveV.R"),          local = TRUE, chdir = TRUE)

  #
  # outputs

  source(file.path("outputMenu.R"),         local = TRUE, chdir = TRUE)

  source(file.path("outputTabsetpanels.R"), local = TRUE, chdir = TRUE)

  source(file.path("outputTabs.R"),         local = TRUE, chdir = TRUE)

  source(file.path("outputPanels.R"),       local = TRUE, chdir = TRUE)

  source(file.path("outputNuccore.R"),      local = TRUE, chdir = TRUE)

  source(file.path("outputDataframes.R"),   local = TRUE, chdir = TRUE)

  source(file.path("outputPlot.R"),         local = TRUE, chdir = TRUE)

  source(file.path("outputLogCode.R"),      local = TRUE, chdir = TRUE)

  #
  # observers
  source(file.path("obserPosButtons.R"),    local = TRUE, chdir = TRUE)

  source(file.path("obserPresets.R"),       local = TRUE, chdir = TRUE)

  source(file.path("obserNuccore.R"),       local = TRUE, chdir = TRUE)

  source(file.path("obserDataframes.R"),    local = TRUE, chdir = TRUE)

  source(file.path("obserSwap.R"),          local = TRUE, chdir = TRUE)

  source(file.path("obserColumnMod.R"),     local = TRUE, chdir = TRUE)

  source(file.path("obserPlot.R"),          local = TRUE, chdir = TRUE)

  outputOptions(output, "tabsetpanel1UI", suspendWhenHidden = FALSE)
  outputOptions(output, "tabsetpanel2UI", suspendWhenHidden = FALSE)
  outputOptions(output, "tabsetpanel4UI", suspendWhenHidden = FALSE)

  outputOptions(output, "presetUI",       suspendWhenHidden = FALSE)

  outputOptions(output, "dfchrpanel",     suspendWhenHidden = FALSE)
  outputOptions(output, "out",            suspendWhenHidden = FALSE)

  outputOptions(output, "dfmarkpanel",    suspendWhenHidden = FALSE)
  outputOptions(output, "outMark",        suspendWhenHidden = FALSE)

  outputOptions(output, "dfMStylepanel",  suspendWhenHidden = FALSE)
  outputOptions(output, "outMStyle",      suspendWhenHidden = FALSE)

  outputOptions(output, "parameterPanel", suspendWhenHidden = FALSE)
  outputOptions(output, "logpanel",       suspendWhenHidden = FALSE)
  outputOptions(output, "strpanel",       suspendWhenHidden = FALSE)

  outputOptions(output, "searchPanel",    suspendWhenHidden = FALSE)
  outputOptions(output, "searchUI",       suspendWhenHidden = FALSE)
  outputOptions(output, "markColumnUI",   suspendWhenHidden = FALSE)
  outputOptions(output, "authorsUI",      suspendWhenHidden = FALSE)

}
