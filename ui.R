library(shiny)
library(shinyTree)
library(shinythemes)
library(ggplot2)
#library(reshape)
library(plyr)
library(gridExtra)

############################necessary graphical functions

wrap_gtable <- function(g){
  class(g) <- c("arrange", class(g))
  g
}

print.arrange = function(x){
  if(is.ggplot(x))  x <- ggplot2::ggplotGrob(x)
  grid::grid.draw(x)
}


###############################################################################

classif.index<- readRDS("./data/ClassificationIndex.RDS")


###########

shinyUI(fluidPage(
  
    theme = shinytheme("cerulean"),
    
    titlePanel("CANJEM job exposure matrix "),
    
    sidebarLayout(
    
      
    ####################################################### SIDEBAR
    sidebarPanel(
      
      
    
      
        h3("Activity, agent and time periods"),
      
        
        ####activity and agent
        br(),
        p(strong("Selected activity classification system:"),textOutput("classification.name.1",inline=TRUE)),
        br(),
        p(strong("Selected activity:"),textOutput("activname.2",inline=TRUE)),
        br(),
        p(strong("Data availability per period")),  
        
        
        ##############data availability
        
        uiOutput("ui1"),
        br(),
        
     
       #########agent
       br(),
       p(strong("Selected agent:"),textOutput("agentname.2",inline=TRUE)), 
       br(),
    
      ##############Parameter section
      h3("Parameter selection"),
      br(),
      p("The parameters below are used to define which cells and which exposure records will be used to calculate CANJEM according to your choices. The default values for the parameters are recommended for general use."),
      br(),
      
      #############criteria for jobs
      h4("Selection of exposure records"),
      p("The following criteria pertain to defining what conditions must be met for a single job to be deemed exposed to a specific agent"),
      selectInput("job.min.reliability", "Minimum confidence:",
                  c("Possible" = 1,
                    "Probable" = 2,
                    "Definite" = 3)),
      selectInput("job.min.concentration", "Minimum intensity:",
                  c("Low" = 1,
                    "Medium" = 2,
                    "High" = 3)),
      sliderInput("job.min.frequency", "Minimum frequency (hours per week)", 0, 40, value=0.5, step = 0.5),
      numericInput("job.min.FWI", "Minimum frequency weighted intensity (FWI)", value=0.05, min = 0, max = 25),
      br(),
      
      #############criteria for cells
      h4("Selection of JEM cells"),
      p("The following criteria pertain to defining what conditions much be met for a CANJEM cell to be displayed"),
      sliderInput("cell.min.njob", "Minimum number of jobs", 1, 100, value=10, step = 1),
      sliderInput("cell.min.nsubjects", "Minimum number of subjects", 0, 20, value=3, step = 1),
      sliderInput("cell.min.probability", "Minimum probability of exposure (%)", 0, 100, value=5, step = 1),
      numericInput("cell.min.FWImedian", "Minimum median FWI", value=0.05, min = 0, max = 25)
      
      
    ),
    
    
    
    ############################################### MAIN PANEL
    mainPanel(
      
      
      ####waiting message
      
      tags$head(tags$style(type="text/css", "
                           #waitmessage {
                           position: fixed;
                           top: 0px;
                           left: 0px;
                           width: 100%;
                           padding: 5px 0px 5px 0px;
                           text-align: center;
                           font-weight: bold;
                           font-size: 100%;
                           color: #000000;
                           background-color: #CCFF66;
                           z-index: 105;
                           }
                          
                           ")),
      
      
      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                       tags$div("Calculations running...",id="waitmessage")),  
      
      
      
      
    
    tabsetPanel(
      
      ######################################ACTIVITYSELECTION VERSION 2
      tabPanel("Classification system selection",
               
               br(),
               p("In this tab you first have to select a classification of activity (CANJEM includes 4 occupation and 3 industry classifications)."),
               br(),
               radioButtons("type.of.classification","Select a type of activity classification",choices=c("Occupation","Industry"),selected = "Occupation"),
               uiOutput("list.of.activityclassif.ui"),
               br(),
               p(strong("Activity classification name: ")),
               textOutput("classification.name"),
               br(),
               br(),
               h5("Warning : Once you have gone to the",strong("activity selection")," tab, you won't be able to change the classification system. Although you will be able to see the updated hierarchy, you won't be able to select a category in the new system. If you wish to change to a new classification system, refresh your browser to re-initialize the app.")
               
               
      ),
    
    
      ######################################ACTIVITYSELECTION
      tabPanel("Activity selection",
               
               br(),
               p("In this tab you can can select the activity of interest within the chosen classification in the hierarchical tree below. The search field can be used to find activities based on full or partial name (expect slowdown if used)."),
               br(),
               h5("Warning : because of the complexity of the hierarchical activity trees, it might take some time (no more than a few seconds though), for the activity you clicked on to actually be processed internally. You will know it worked when the title and definitions appear, or are updated, just below this warning."),
               br(),
               p(strong("Activity title: "),textOutput("activname",inline=TRUE)),
               p(strong("Activity definition")),
               textOutput("activdef"),
               br(),
               h4("Activity hierarchy"),
               br(),
               uiOutput("activity.tree.ui"),
               br()
               
               
      ),
      
      

      
      ######################################AGENT SELECTION
      tabPanel("Agent selection",
               br(),
               p("In this tab you can select the agent of interest in the hierarchical tree below. The search field can be used to quickly find agents based on full or partial name. THe full name and definition of the selected agents can be found at the bottom of the tree."),
               br(),
               p(strong("Agent name: "),textOutput("agentname",inline=TRUE)),
               p(strong("Agent definition")),
               textOutput("agentdef"),
               br(),
               p(strong("In the CANJEM database")),
               p("",textOutput("n.job.exposed",inline = TRUE )," jobs are exposed to this agent according to the selected criterias, representing ",textOutput("n.job.exposed.perc",inline = TRUE )," of the jobs in the CANJEM databbase."),
               br(),
               h4("Agent hierarchy"),
               br(),
               p("search field"),
               shinyTree("tree", search=TRUE),
               br()
               
               
               
      ),
      
##################################### CANJEM results
      tabPanel("CANJEM results",
               
###################################PER CELL     
          tabsetPanel(     
               
               
               tabPanel("By combination agent-activity",
               br(),
               br(),
               p("In this tab you will find the CANJEM information for the combination of agent and activity selected in the respitective tabs. The availability of information will depend on the criterion selected in the sidebar panel"),
              br(),
               #######parameters
              
               h4("Selected activity:",strong(textOutput("activname.1",inline=TRUE))),
               br(),
               h4("selected agent:",strong(textOutput("agentname.1",inline=TRUE))),
               br(),
               h4("selected time period",strong(textOutput("period.1",inline=TRUE))),
               br(),
               uiOutput("period.list"), 
               br(),
               uiOutput("ui3"),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               h4(strong("Frequency weighted intensity")),
               br(),
               uiOutput("ui4")
               
               ),
      ################################## PER AGENT         
       tabPanel("By agent",
                br(),
                br(),
                p("In this tab you will find a summary CANJEM information for the selected agent across all activities. The availability of information will depend on the criterion selected in the sidebar panel. We present activities with exposure, and those with no exposure."),
                
                ###############################
                tabsetPanel(
                  
                  ############################  EXPOSED ACTIVITIES          
                  tabPanel("Exposed activities",          
                    p("In this tab you will find a summary CANJEM information for the selected agent across all activities with exposure according to the criterias in the sidebar. Results can be sorted by probability of exposure, intensity, confidence, frequency and FWI. You can select below the period of interest and the activity resolution."),
                    br(),
                    h4("selected agent:",strong(textOutput("agentname.3",inline=TRUE))),
                    br(),
                    h4("selected time period",strong(textOutput("period.2",inline=TRUE))),
                    br(),
                    uiOutput("period.list.2"), 
                    uiOutput("select.resolution"),
                    selectInput(inputId="selected.sort.order",label="Select a sorting order",
                                choices=list("Highest probability of exposure"=1,
                                        "Highest intensity of exposure"=2,
                                        "Highest frequency of exposure"=3,
                                        "Highest confidence in ratings"=4,
                                        "Highest FWI"=5)),
                    br(),
                    h4(strong("CANJEM RESULTS")),
                    br(),
                    uiOutput("by.agent.exposed.ui"),
                    br()),
                  
                  ############################  UNEXPOSED ACTIVITIES          
                  tabPanel("Unexposed activities",          
                           p("In this tab you will find a summary CANJEM information for the selected agent across all activities with no exposure according to the criterias in the sidebar."),
                           br(),
                           h4("selected agent:",strong(textOutput("agentname.4",inline=TRUE))),
                           br(),
                           h4("selected time period",strong(textOutput("period.3",inline=TRUE))),
                           br(),
                           uiOutput("period.list.3"), 
                           uiOutput("select.resolution.1"),
                           br(),
                           h4(strong("CANJEM RESULTS")),
                           br(),
                           uiOutput("by.agent.unexposed.ui"),
                           br())
            )),
       
       ########################### PER ACTIVITY
       tabPanel("By activity",
                br(),  
                br(),
                p("In this tab you will find a summary CANJEM information for the selected activity across all agents with exposure according to the criterias in the sidebar. Results can be sorted by probability of exposure, intensity, confidence, frequency and FWI. You can select below the period of interest and the activity resolution."),
                br(),
                h4("selected activity:",strong(textOutput("activname.3",inline=TRUE))),
                br(),
                h4("selected time period",strong(textOutput("period.4",inline=TRUE))),
                br(),
                uiOutput("period.list.4"), 
                br(),
                selectInput(inputId="selected.sort.order.1",label="Select a sorting order",
                            choices=list("Highest probability of exposure"=1,
                                         "Highest intensity of exposure"=2,
                                         "Highest frequency of exposure"=3,
                                         "Highest confidence in ratings"=4,
                                         "Highest FWI"=5)),
                br(),
                h4(strong("CANJEM RESULTS")),
                br(),
                uiOutput("by.activity.ui"),
                br()
               )
               
          )
      )
      
      
      
  )))))