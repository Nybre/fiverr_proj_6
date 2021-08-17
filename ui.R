
shinyUI(dashboardPage(title= "Performance ranking tool", 
                      #--------------------------------------------------------------------
                      ## HEADERBAR
                      #------------------------------------------------------------------
                      #logo can be used here
                      dashboardHeader(  
                                      tags$li(class = "dropdown", 
                                              popify(
                                                tags$a(img(height = "18px", 
                                                           src = "images/refresh.png"),href = "javascript:history.go(0)"),
                                                title = "Refresh",  
                                                placement = "left"
                                              )
                                      )     
                      ), 
                      #--------------------------------------------------------------------
                      ## SIDE NAVIGATION BAR
                      #--------------------------------------------------------------------
                      dashboardSidebar(
                        collapsed = TRUE,
                        sidebarMenu(
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(), 
                          menuItem("Perfomance rating", tabName = "performance_rating", icon = icon("search",lib = "font-awesome")),    
                          br(),
                          br(),
                          br() 
                        )), 
                      #--------------------------------------------------------------------
                      ## MAINBODY
                      #--------------------------------------------------------------------
                      dashboardBody(  
                        setShadow(class = "box"),
                        tags$head(
                          tags$link(rel = "icon", type = "image/png", href = "OISweb.png"), 
                          tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
                          tags$style(HTML(".main-sidebar { font-size: 13px; }")),
                          tags$style(HTML(".main-sidebar {  font-weight:bold; }")),
                          tags$style(HTML(".skin-blue .main-sidebar {background-color: #0d1919;} ")) 
                        ),
                        tabItems( 
                          #--------------------------------------------------------------------
                          ### SCRIPT UI 
                          #--------------------------------------------------------------------
                          tabItem(tabName = "performance_rating",  
                                  verticalLayout( 
                                    tabsetPanel(   
                                      tabPanel( "Perfomance Rating App",value=1, 
                                                performance_rating_UI("performance_rating-module")
                                      ), 
                                      #usefull if you have want multiple tabs ran on different scripts 
                                      id = "conditionedPanels"
                                    )
                                  )) 
                        ))  
)) 