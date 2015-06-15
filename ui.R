library(shiny)
library(shinythemes)
library(networkD3)
      
buildTabAboutMe = function() {
	tabPanel("About Me", class = "div1",
						fluidRow( 
                      fluidRow(column(3, img(src = "sivan_profile.jpg", class = "circular")),
                               column(7, h3("- about me - ", class = "bio"))),
                                
                      fluidRow(column(9, offset = 2, p("I'm a data analyst working mainly with operational data and user data. I like to play with new and known technologies. 
                                                          My tools of choice are R and SQL. I also work with Excel, Tableau & SAS."),
                                                       p("I collaborate with small and midsized businesses to integrate feasible and low-cost
                                                         data solutions from data storage to frequent reporting to dashboarding."))),
                      fluidRow(column(3,img(src = "membership_icon.png", class = "circular")), 
                               column(7, h3("- commitments -", class = "bio"))),
                      fluidRow(column(9, offset = 2, p("I'm an active member of the Bay Area UseR Group and attend the monthly meetings.
                                                        R is an open source statistics and programming language, supported by a large community with libraries and 
                                                        add-ons. 
                                                        Working with R allows me to stay within one framework when scripting, exploring, 
                                                        performing advanced analytics, and dashboarding."),
                                                     p("In my free time I build electronic gadgets."),
                                                     p("My current project is a wifi connected barrel smoker that I built from scratch together with two friends."),
                                                     p("Originally we built the barrel oven to bake Bavarian Pretzels. Currently, we are 
                                                        transforming the Pretzel Barrel into a wifi controlled Barrel Smoker with an external 
                                                        smoke box and lots of sensors to measure and control temperature and air intake remotely."))),
                      fluidRow(column(3,
                                      img(src = "contact_icon.png", class = "circular")), 
                               column(7, h3("- contact -", class = "bio"))),
                      fluidRow(column(9, offset = 2, p("For suggestions or services, please text or call via Google Voice:"), 
                                                         p("415.868.5846") 
                                                         ))))
}

click_create = function() {
  singleton(tags$head(HTML(
'
  <script type="text/javascript">
    $(document).ready(function() {
      // disable download at startup. data_file is the id of the downloadButton
      setTimeout( function() {
          $("#Create").click();
          setTimeout(function() { $("#Compute").click();  }, 100)},
        200 );
    })
  </script>
'
  )))
}

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("cosmo"),
                  click_create(),
                  tags$head(
                    tags$link(rel = "stylesheet", type = "text/css", href = "sivan.css")
                  ),
  
#######################################################
##################   PayR   ###########################
#######################################################

   navbarPage("Sivan's website",
              tabPanel("PayR",  
                       tabsetPanel(id = "PayR_Steps",
                                     tabPanel(title = "Project", class = "div1",value="Projectpanel",
                                              fluidRow(column(6, 
                                                              fluidRow(
                                                                column(7, class = "cols1",
                                                                       textInput("Project", "Project:", value = "Easter Camping 2015")
                                                                )),
                                                              fluidRow(
                                                                column(7, class = "cols1",
                                                                       textInput("TMember", "Trip Members:", value = "Simon, Sivan, Joe")), 
                                                                column(1, img(src = "info.png", class = "smallicon", alt="Enter all names of the members of this trip, separated by comma.", title="Enter all names of the members of this trip, separated by comma."))
                                                              ), 
                                                              fluidRow(
                                                                column(7, class = "cols1",
                                                                actionButton("Create", "Create"))
                                                              )
                                     ))),
                                     tabPanel(title = "Transactions", class = "div1",value="Transactionpanel",
                                              fluidRow(column(4, 
                                                       fluidRow(
                                                          column(12,
                                                          uiOutput("ui") 
                                                                  )),
                                                       fluidRow(
                                                                  column(6, class = "buttonleft",
                                                                         actionButton("Add", "Add"),
                                                                         actionButton("Delete", "Delete") 
                                                                         #actionButton("Reset", "Reset")
                                                                         )     
                                                                  ),
                                                         
                                                     fluidRow(
                                                                column(7,class = "cols1",
                                                                       textInput("SpentFor", "Spent for:", value = "Grocery")),
                                                                column(1, img(src = "info.png", class = "smallicon", alt="Enter a discriptive name for the cost item so that everyone knows what this was spent for.", 
                                                                              title = "Enter a discriptive name for the cost item so that everyone knows what this was spent for."))), 
                                                     fluidRow(
                                                                column(7,class = "cols1",
                                                                       textInput("Amount", "Amount:", value = 50))),
                                                     fluidRow(
                                                                column(7, class = "cols1", 
                                                                       textInput("Payer", "Payer:", value = "Sivan")),
                                                                column(1, img(src = "info.png", class = "smallicon", alt = "Enter name of person who paid.", title = "Enter name of person who paid."))), 
                                                     fluidRow(
                                                                column(6,class = "buttonleft", 
                                                                actionButton("Compute", "Compute")))
                                                              ),
                                             column(8,        
                                             fluidRow(column(12, dataTableOutput("sumPary")))))),
                                                      
                                   tabPanel(title = "Results",value="Results",
                                            #fluidRow(class = "div1", 
                                                      #column(5,
                                                       #      textInput("CloseTo", "enter groups of people that meet often:", value = "Sivan, Joe, Simon"), 
                                                        #     fluidRow(column(12, actionButton("AddGroup", "Add Group"), 
                                                         #             actionButton("Optimize", "Optimize"), 
                                                          #            actionButton("Delete2", "Delete")))),
                                                      #column(3, 
                                                      #       verbatimTextOutput("CloseTo"))
                                                      #column(4, img(src = "Group_icon.png", class = "circular"))
                                                     #),
                                            fluidRow(class = "div1", 
                                                column(12, 
                                                       dataTableOutput("Logic"))),
                                            fluidRow(class = "div1", 
                                                column(12, class = "graph", 
                                                       #forceNetworkOutput("PaymentPlot")
                                                       plotOutput("PaymentPlot")
                                                       )) 
                                            ), 
                                  tabPanel("About PayR", class = "div1", 
                                          fluidRow(column(2, img(src = "PayR_logo.png", class = "circular")),
                                                   column(9, 
                                                           p(id = "about", "Traveling in groups is fun. Typically some organize and pay. Splitting the costs afterwards can be 
                                                                   a complicated calculation, because not everyone participates in all activities. After I ran into this
                                                                   issue several times, I came up with PayR, an app to facilitate the process of balancing out group expenses. 
                                                                   "),  
                                                            p(id = "about", "PayR works in a few steps:"),
                                                            tags$ul("1. First enter the trip name and all members."),  
                                                            tags$ul("2. Enter all expenses, by whom they were paid and who participated in them."), 
                                                            tags$ul("3. When you press the Compute Button PayR will run a Monte Carlo simulation of balancing out the
                                                                        costs with the least possible number of transactions.")  
                                                            #tags$ul("4. "),  
                                                            #tags$ul("5. Solutions are scored according to the number of transaction needed and according to closeness of the 
                                                            #         people exchanging money. The solution with the least score wins and is provided. "), 
                                                            #p(id = "about", "PayR provides the easiest solution to pay debts and at the same time 
                                                            #prefers the solution with the least number of transactions between people.")))
                                                            ))),
 #######################################################
 ##################   StRaw   #########################
 #######################################################    
 
#c/p content of function here
 
 #######################################################
 ##################   AboutMe  #########################
 #######################################################
                buildTabAboutMe())
             ))))
   
                      




  