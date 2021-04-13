#
#   left submenus
#

output$mysidebar <- renderUI({
  sidebarMenu(
    id = "tabs",
    sidebarMenuOutput("menu1")
  )# menu
})

output$menu1 <- renderMenu({

  sidebarMenu(
    menuItem("Examples (presets)", tabName="examplesMenu" #, icon = icon("equalizer", lib="glyphicon")
    )
    ,menuItem("Nucleotides", tabName="nuccoreMenu", badgeLabel = "new", badgeColor = "orange" )

    ,menuItem("data.frames", tabName="DFsMenu" #, icon = icon("equalizer", lib="glyphicon")
    ) # menuitem
    ,menuItem("Parameters & Plot", tabName="parameterPlotMenu"   #, icon = icon("equalizer", lib="glyphicon")
              ,selected = TRUE
    ) #menuitem
    ,menuItem("About", tabName="aboutMenu"
    ) #menuitem
  )
}) #output
