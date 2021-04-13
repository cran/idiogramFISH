
values <- reactiveValues(number=0
                         , mylistAllClass="numeric"
                         , df1=data.frame()
                         , df1Mark=data.frame()
                         , termButtonVal=0
                         , entrez_titles=NA
                         , entrez_selected=NA
                         , counter=0
                         , decision=""
                         , state="ready"
                         , pngorsvg="svg"
                         , plot=TRUE
                         , stop=FALSE
)

#
#   first tab

Current <- reactiveValues(Tab = "dfChrTab", Tab2 = "paramTab", Tab4 = "searchTab")

#
# first submenu, see local

CurrentM <- reactiveValues(
  menu = "parameterPlotMenu"
)

scriptR <- reactive({
  df <- values[["strFun"]]
})
