library(shiny)
library(shinythemes)
library(networkD3)
      
buildTabAboutMe = function() {
	tabPanel("About Me", class = "div1",
						fluidRow(column(3, 
                                    fluidRow(img(src = "sivan_profile.jpg", class = "circular"))),
                                column(7,h3("- about me - ", class = "bio"))),
                                
                        fluidRow(column(9, offset = 3, p("I'm a data analyst for user experience, marketing 
                                                         and operations data. I like to play with new and known technologies. 
                                                         My every-day tools of choice are MySQL and R. I also work with Excel, Tableau 
                                                         & Weka."),
                                                       p("I collaborate with small businesses to integrate feasible and low-cost
                                                         data solutions. From data storage over frequent reporting to dashboarding, I offer end-to-end solutions
                                                         to enhance customer service, marketing effectiveness, and conversion rate."))),
                      fluidRow(column(3,
                                      img(src = "membership_icon.png", class = "circular")), 
                               column(7, h3("- commitments -", class = "bio"))),
                      fluidRow(column(9, offset = 3, p("I'm an active member of the Bay Area UseR Group and attend the monthly meetings regularly.
                                                        R is an open source statistics and programming language, supported by a large community with libraries and 
                                                        add-ons. 
                                                        Working with R allows me to stay within one framework when scripting, exploring, 
                                                        performing advanced analytics, and dashboarding."),
                                                     p("In my free time I build electronic gadgets."),
                                                     p("My current project is a wifi connected barrel smoker that I built 
                                                         from scratch with two friends. We built it as an enormous Pretzel oven to serve
                                                         fresh Pretzels every morning on Burning Man 2014."),
                                                     p("Now we are transforming the Pretzel Barrel into a wifi controlled Barrel Smoker with an external 
                                                        smoke box and lots of sensors to measure and control all kinds of temperatures and air intake."))),
                      fluidRow(column(3,
                                      img(src = "contact_icon.png", class = "circular")), 
                               column(7, h3("- contact -", class = "bio"))),
                      fluidRow(column(9, offset = 3, p("For suggestions or services, please text or call via Google Voice:"), 
                                                         p("415.868.5846") 
                                                         )))
}

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("cosmo"),
                  tags$head(
                    tags$link(rel = "stylesheet", type = "text/css", href = "sivan.css")
                  ),
  
#######################################################
##################   PayR   ###########################
#######################################################

   navbarPage("Sivan's Projects",
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
                                                           p(id = "about", "Traveling in groups is fun. Typically some organize and prepay all expenses and afterwards the costs
                                                            per person are added up for each individual. 
                                                            Depending on the variety of activities this can be a fairly complex task. After having done several 
                                                            of these calculations, I came up with PayR, a small app written in R
                                                            to facilitate the process of balancing out traveling expenses."),  
                                                            p(id = "about", "PayR works in a few steps:"),
                                                            tags$ul("1. It balances costs in all subgroups. The subgroups determine who participates in which costs."),  
                                                            tags$ul("2. The category 'all' is balanced. This is for costs concerning the whole group as groceries."), 
                                                            tags$ul("3. Groups of people that see each other daily or all use the same money tranfer technology can be added."),  
                                                            tags$ul("4. From this information PayR computes 1000 different possiblities to make transactions."),  
                                                            tags$ul("5. Solutions are scored according to the number of transaction needed and according to closeness of the 
                                                                     people exchanging money. The solution with the least score wins and is provided. "), 
                                                            p(id = "about", "The PayR solution provides the easiest solution to pay debts and at the same time 
                                                            prefers the solution with the least number of transactions between people.")))))),
 #######################################################
 ##################   StRaw   #########################
 #######################################################    
 
 
              tabPanel("StRaw", 
                   tabsetPanel(id = "StRaw_Steps",
                       tabPanel(title = "StRaw",
                            fluidRow( class = "div1", 
                                    column(6, 
                                         fluidRow(column(12, class = "cols1", 
                                                         tags$textarea(id="UserText", "Paste your text:"))),
                                         fluidRow(column(2, class="button", 
                                                         actionButton("Analyze", "Analyze"))) 
                                          
                                                         
                                       ),
                                    column(6, class = "cols1",
                                           fluidRow(column(12, plotOutput("WordCloud"))) 
                                    )               
                            ), 
                            fluidRow(class = "div1",
                                    column(6, class = "cols1", plotOutput("PosNegCount")), 
                                    column(6, class = "Sentiment", p("Sentiment:")), 
                                    column(6, p("Sentiment is the percentage of positive words minus the percentage
                                                of negative words of the absolute number of recognized
                                                positive and negative values in the text."),
                                    column(6, class = "Sentiment", textOutput("Sentiment")))
                                            
                      )),
                      
                      tabPanel(title = "About StRaw", 
                               fluidRow(class = "div1", column(2, offset= 1, img(src = "StRaw_icon.png", class = "icon")),
                                        column(9, class = "cols1", p(id = "about","StRaw allows you to paste a text or a url into the textfield provided."), 
                                                                 p(id = "about", "The content that is made available is transformed into a corpus and 
                                                                 matched with the Harvard Inquirer Dictionary. As a result you will get 
                                                                 a summary of the text, whether it is more positive or negative, whether
                                                                 it is strongly opinionated and which words occur most often."),
                                                                 p(id = "about", "The wordcloud function builds a word cloud out of the 50 most occuring words")))
                  ))), 
 
 #######################################################
 ##################   AboutMe  #########################
 #######################################################
                buildTabAboutMe())
             ))
   
                      




  