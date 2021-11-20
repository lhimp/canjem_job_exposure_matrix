library(shiny)
library(shinyTree)
library(ggplot2)
#library(reshape)
library(plyr)
library(gridExtra)

#######################################################################
#
############################necessary graphical functions
#
######################################################################

wrap_gtable <- function(g){
  class(g) <- c("arrange", class(g))
  g
}

print.arrange = function(x){
  if(is.ggplot(x))  x <- ggplot2::ggplotGrob(x)
  grid::grid.draw(x)
}


###########################################################
#
############ FUNCTIONS
#
#############################################################




############################################################
#
##########################preliminary data preparation and loading
#
#################################################################

agent.tree <-readRDS("./data/AgentTree.RDS")

isco.tree <-readRDS("./data/ISCOTree.RDS")

canjem.wk <-readRDS("./data/canjem.wk.prepped.RDS")

agents.def <-readRDS("./data/agents.def.RDS")

canjem.jdb123 <-readRDS("./data/canjem.jdb123.prepped.RDS")

canjem.jdb23<-readRDS("./data/canjem.jdb23.prepped.RDS")

isco68 <-readRDS("./data/isco68.prepped.RDS")

periods <-readRDS("./data/periods.RDS")

classifications<-readRDS("./data/ClassificationList.RDS")
  
classif.index<- readRDS("./data/ClassificationIndex.RDS")


########################################################################
#
###### SHINY STUFF
#
#######################################################################


shinyServer(function(input, output, session) {
 

  #########################################   MANAGEMENT OF TREES  
  
        #####agent tree
        output$tree <- renderTree({
          agent.tree
        })  
        
        
  
        ########### activity tree
        
        output$tree1 <- renderTree({
          tree.act <-selected.tree()
        })
        
        
        #####conditional panel for activity TREE
        
        
        output$activity.tree.ui <-renderUI({
          
          classification <- input$select.classification
          
          if (is.null(classification) ) return("No activity classification selected") else
          
          shinyTree("tree1", search=TRUE)
          
        })
        
        
        
        
       ###selected agent 
       agent.choice <-reactive({
         
         tree.in <- input$tree
         
         agent.selected <-unlist(get_selected(tree.in)) 
         
         agent.selected
       }) 
        
        
       ###selected activity
       
       activity.choice <-reactive({
         
         tree1.in <- input$tree1
         

         
         activity.selected <-unlist(get_selected(tree1.in)) 
         
         activity.selected
         
       }) 

       
       ###rest selected activity when another classification is seleted
       
   
         v <- reactiveValues(rows = NULL)
        
          observeEvent(activity.choice(), {
           v$activity.choice <- activity.choice()
         })
         
          observeEvent(input$select.classification, {
           v$activity.choice <- NULL
         })
        
 
################################### CALCULTATIONS 
 
       
       ######selected activity cclassification
       
       list.of.activityclassif <-reactive({
         
         type <- input$type.of.classification
         
         activityclassif.list <-classif.index$name[classif.index$type==input$type.of.classification] 
                                            
       }) 
       
       
       ######TREE for the selected activity cclassification
       
       selected.tree <-reactive({
         
         selected.classification <- input$select.classification
         
         tree.selected <-classifications[[classif.index$index[classif.index$name==selected.classification]]]$tree
         
       }) 
       
       
       ######resetting the tree when classification is changed
       

       
       
         #########################tableau de synthèse pour période  
         
         period.availability <-reactive({
           
           
           #####selected classification
           
           classification <- input$select.classification
           
           index <-classif.index$index[classif.index$name==classification]
           
           
           selected.classification <-classifications[[index]]
           
           
           ###making the table
           
           activity <-v$activity.choice
           
           if(!is.null(activity)) {
           
           results <-periods
           
           ####identifying the level of the title
           
           #restrict.activity<-grepl(activity,selected.classification$description$label.show,fixed=TRUE)
           restrict.activity<-substring(selected.classification$description$label.show,1,min(10,nchar(selected.classification$description$label.show)))==substring(activity,1,min(10,nchar(activity)))
           
           level.activity <-selected.classification$description$level[restrict.activity]
           
           for (i in 1:7) {
             
             restrict <-!is.na(canjem.wk[,level.activity]) & canjem.wk[,level.activity]==selected.classification$description$CODE_PONCTUE[restrict.activity] &
               canjem.wk[13+i]!=0
             
             temp <-canjem.wk$ID[restrict]
             
             results$n.jobs[i] <- length(temp)
             
             results$n.subjects[i] <-length(unique(temp))  
             
             
             
           }
           
           
           results$ok<-results$n.jobs>=input$cell.min.njob & results$n.subjects>=input$cell.min.nsubjects 
           
           results }
           
         })
         
         
         
         ################NUMBER OF JOBS EXPOSED PER AGENT
         
         n.job.exposedperagent <-reactive({
           
           agent<-agent.choice()
           
           if (input$job.min.reliability==1) result <- length(canjem.jdb123[canjem.jdb123$IDCHEM==agents.def$idchem[agents.def$label==agent] & canjem.jdb123$R_FINAL>=input$job.min.reliability & canjem.jdb123$F_FINAL>=input$job.min.frequency & canjem.jdb123$C_FINAL>=input$job.min.concentration & canjem.jdb123$F_FINAL*canjem.jdb123$C_FINAL/40>=input$job.min.FWI,1]) else result <- length(canjem.jdb23[canjem.jdb23$IDCHEM==agents.def$idchem[agents.def$label==agent] &
                                                                                                                                                                                       canjem.jdb23$R_FINAL>=input$job.min.reliability &
                                                                                                                                                                                       canjem.jdb23$F_FINAL>=input$job.min.frequency &
                                                                                                                                                                                       canjem.jdb23$C_FINAL>=input$job.min.concentration &
                                                                                                                                                                                       canjem.jdb23$F_FINAL*canjem.jdb23$C_FINAL/40>=input$job.min.FWI,1])
                                                                              
                                                                              result
                                                                              
         })
         
         
    ######### JEM CALCULATION FOR ALL PERIODS (CELL TAB)
         
         jem.calc <-reactive({
           
               
           classification <- input$select.classification
           
           index <-classif.index$index[classif.index$name==classification]
           
           
           selected.classification <-classifications[[index]]
           
           period.temp <-period.availability()
           
           activity <-  v$activity.choice
           
           agent<-agent.choice()
           
           #agent <-"Engine emissions"
           
           #activity <-"0-11.20 : Organic Chemist."
           
           
           #######selecting the occupational histories
           
           
           level.activity <-selected.classification$description$level[selected.classification$description$label.show==activity]
           
             restrict <-!is.na(canjem.wk[,level.activity]) & canjem.wk[,level.activity]==selected.classification$description$CODE_PONCTUE[selected.classification$description$label.show==activity] 
             
             wk <-canjem.wk[restrict,]
           
             activity.type <-level.activity
           
           
         
           
           
           #######selecting the exposure data
           
           if (input$job.min.reliability==1) jdb <-canjem.jdb123 else jdb <-canjem.jdb23
           
           jdb <-jdb[jdb$IDCHEM==agents.def$idchem[agents.def$label==agent],]
           
           jdb <-jdb[jdb$R_FINAL>=input$job.min.reliability,]
           
           
           #######matrix calculation
           mat <-list()
           
           for (i in 1:7) {
             
             if(period.temp$ok[i]) mat[[i]]<- matrix.fun(jdb,wk,vec.dim=activity.type,
                                                         agents=agents.def$idchem[agents.def$label==agent],
                                                         type='R1expo',
                                                         Dmin=input$job.min.FWI,
                                                         Fmin=input$job.min.frequency ,
                                                         Cmin=input$job.min.concentration,
                                                         Nmin=1,Nmin.s=1,
                                                         c(period.temp$yearin[i],period.temp$yearout[i]),
                                                         Fcut= c(0, 2, 12, 40, 250)) else mat[[i]] <- NULL
                                                         
           }
           
           return(mat)
           
         })
 
         
        #############################JEM CALCULATION BY AGENT - EXPOSED
         
        jem.calc.agent.exposed <- reactive({
          
          ###inpout
          
          classification <- input$select.classification
          
          index <-classif.index$index[classif.index$name==classification]
          
          
          selected.classification <-classifications[[index]]
          
          
          agent <-agent.choice()
          
          period <-input$select.period.2
          
          i<-periods$code[periods$label==period]
          
          level.activity <-input$selected.resolution
          
          sort.order <-input$selected.sort.order
          
          ##criteria for individual jobs
          
          job.min.reliability <-input$job.min.reliability
          job.min.concentration <-input$job.min.concentration
          job.min.frequency <-input$job.min.frequency
          job.min.FWI <-input$job.min.FWI
          
          ##criteria for cells
          
          cell.min.njob <-input$cell.min.njob
          cell.min.nsubjects <-input$cell.min.nsubjects
          cell.min.probability <-input$cell.min.probability
          cell.min.FWImedian <-input$cell.min.FWImedian
          
        
          #######selecting the occupational histories
          
          restrict <-!is.na(canjem.wk[,level.activity])  
          
          wk <-canjem.wk[restrict,]
          
          activity.type <-level.activity
          
          
          #######selecting the exposure data
          
          if (job.min.reliability==1) jdb <-canjem.jdb123 else jdb <-canjem.jdb23
          
          jdb <-jdb[jdb$IDCHEM==agents.def$idchem[agents.def$label==agent],]
          
          jdb <-jdb[jdb$R_FINAL>=job.min.reliability,]
          
          ######################  matrix calculation for the selected period
          
          mat <-matrix.fun(jdb,wk,vec.dim=activity.type,
                           agents=agents.def$idchem[agents.def$label==agent],
                           type='R1expo',
                           Dmin=job.min.FWI,
                           Fmin=job.min.frequency ,
                           Cmin=job.min.concentration,
                           Nmin=1,Nmin.s=1,
                           c(periods$yearin[i],periods$yearout[i]),
                           Fcut= c(0, 2, 12, 40, 250)) 
          
          
          ###################formating mat
          
          
          ######most frequent categories     
          
          mat$most.freq.confidence <- ddply(mat ,~ cell , function(x){c(3:1)[which.max(c(x$n.R3[1],x$n.R2[1],x$n.R1[1]))]})$V1
          
          mat$most.freq.intensity <- ddply(mat ,~ cell , function(x){c(3:1)[which.max(c(x$n.C3[1],x$n.C2[1],x$n.C1[1]))]})$V1
          
          mat$most.freq.frequency <- ddply(mat ,~ cell , function(x){c(4:1)[which.max(c(x$n.F4,x$n.F3[1],x$n.F2[1],x$n.F1[1]))]})$V1
          
          ######variables of interest
          
          
          mat <-mat[,c(54,4:7,56:58,50)]
          
          
          
          ######exclusion criterias
          
          list.exposed <-mat[mat$ntot>=cell.min.njob &
                                           mat$nsub>=cell.min.nsubjects &
                                           mat$Dmed.5>=cell.min.FWImedian &
                                           mat$p>=cell.min.probability,activity.type]
          
          list.unexposed <-mat[mat$ntot>=cell.min.njob &
                                             mat$nsub>=cell.min.nsubjects &
                                             (mat$Dmed.5<cell.min.FWImedian |
                                                mat$p<cell.min.probability),activity.type]
          
          
          #####final sorting
          
          if (sort.order==1) mat<-mat[order(mat$p,decreasing = TRUE),]
          if (sort.order==2) mat<-mat[order(mat$most.freq.intensity ,decreasing = TRUE),]
          if (sort.order==3) mat<-mat[order(mat$most.freq.frequency ,decreasing = TRUE),]
          if (sort.order==4) mat<-mat[order(mat$most.freq.confidence ,decreasing = TRUE),]
          if (sort.order==5) mat<-mat[order(mat$Dmed.5 ,decreasing = TRUE),]
          
          
          
          ######nicer labels
          
          mat$most.freq.confidence <-revalue(as.character(mat$most.freq.confidence), c("1"="Possible","2"="Probable","3"="Definite"))
          
          mat$most.freq.intensity <- revalue(as.character(mat$most.freq.intensity), c("1"="Low","2"="Medium","3"="High"))
          
          mat$most.freq.frequency <- revalue(as.character(mat$most.freq.frequency), c("1"="[0-2h[","2"="[2-12h[","3"="[12-40h[","4"="[40h+]"))
          
          mat$Activity.title<-selected.classification$description$LIB_PRINCIPAL[match(mat[,1],selected.classification$description$CODE_PONCTUE)]
          
          mat$p <-as.character(signif(mat$p,2))
          mat$Dmed.5 <-as.character(signif(mat$Dmed.5,2))
          
          names(mat)[6:9] <-c("Confidence","Intensity","Frequency","FWI")
          
         
          ################separating the mats
          
          mat.exposed <-mat[is.element(mat[,activity.type],list.exposed),]
          
          mat.unexposed <-mat[is.element(mat[,activity.type],list.unexposed),]
          
          mat.unexposed <-mat.unexposed[,c(1,2,4,5,10)]
          
          
          result <-data.frame(Activity.code=mat.exposed[,1])
          
          result <-cbind(result,mat.exposed[,-1])
          
          return(result)
          
        }) 
         
         
        #############################JEM CALCULATION BY AGENT - UNEXPOSED
        
        jem.calc.agent.unexposed <- reactive({
          
          ###inpout
          
          classification <- input$select.classification
          
          index <-classif.index$index[classif.index$name==classification]
          
          
          selected.classification <-classifications[[index]]
          
          
          agent <-agent.choice()
          
          period <-input$select.period.3
          
          i<-periods$code[periods$label==period]
          
          level.activity <-input$selected.resolution.1
          
      
          
          ##criteria for individual jobs
          
          job.min.reliability <-input$job.min.reliability
          job.min.concentration <-input$job.min.concentration
          job.min.frequency <-input$job.min.frequency
          job.min.FWI <-input$job.min.FWI
          
          ##criteria for cells
          
          cell.min.njob <-input$cell.min.njob
          cell.min.nsubjects <-input$cell.min.nsubjects
          cell.min.probability <-input$cell.min.probability
          cell.min.FWImedian <-input$cell.min.FWImedian
          
          
          #######selecting the occupational histories
          
          
          restrict <-!is.na(canjem.wk[,level.activity])  
          
          wk <-canjem.wk[restrict,]
          
          activity.type <-level.activity
          
          
          
          #######selecting the exposure data
          
          if (job.min.reliability==1) jdb <-canjem.jdb123 else jdb <-canjem.jdb23
          
          jdb <-jdb[jdb$IDCHEM==agents.def$idchem[agents.def$label==agent],]
          
          jdb <-jdb[jdb$R_FINAL>=job.min.reliability,]
          
          ######################  matrix calculation for the selected period
          
          mat <-matrix.fun(jdb,wk,vec.dim=activity.type,
                           agents=agents.def$idchem[agents.def$label==agent],
                           type='R1expo',
                           Dmin=job.min.FWI,
                           Fmin=job.min.frequency ,
                           Cmin=job.min.concentration,
                           Nmin=1,Nmin.s=1,
                           c(periods$yearin[i],periods$yearout[i]),
                           Fcut= c(0, 2, 12, 40, 250)) 
          
          
          ###################formating mat
          
          
          ######most frequent categories     
          
          mat$most.freq.confidence <- ddply(mat ,~ cell , function(x){c(3:1)[which.max(c(x$n.R3[1],x$n.R2[1],x$n.R1[1]))]})$V1
          
          mat$most.freq.intensity <- ddply(mat ,~ cell , function(x){c(3:1)[which.max(c(x$n.C3[1],x$n.C2[1],x$n.C1[1]))]})$V1
          
          mat$most.freq.frequency <- ddply(mat ,~ cell , function(x){c(4:1)[which.max(c(x$n.F4,x$n.F3[1],x$n.F2[1],x$n.F1[1]))]})$V1
          
          ######variables of interest
          
          
          mat <-mat[,c(54,4:7,56:58,50)]
          
          
          
          ######exclusion criterias
          
          list.exposed <-mat[mat$ntot>=cell.min.njob &
                               mat$nsub>=cell.min.nsubjects &
                               mat$Dmed.5>=cell.min.FWImedian &
                               mat$p>=cell.min.probability,activity.type]
          
          list.unexposed <-mat[mat$ntot>=cell.min.njob &
                                 mat$nsub>=cell.min.nsubjects &
                                 (mat$Dmed.5<cell.min.FWImedian |
                                    mat$p<cell.min.probability),activity.type]
          
          
      
          
          
          
          ######nicer labels
          
          mat$most.freq.confidence <-revalue(as.character(mat$most.freq.confidence), c("1"="Possible","2"="Probable","3"="Definite"))
          
          mat$most.freq.intensity <- revalue(as.character(mat$most.freq.intensity), c("1"="Low","2"="Medium","3"="High"))
          
          mat$most.freq.frequency <- revalue(as.character(mat$most.freq.frequency), c("1"="[0-2h[","2"="[2-12h[","3"="[12-40h[","4"="[40h+]"))
          
          mat$Activity.title<-selected.classification$description$LIB_PRINCIPAL[match(mat[,1],selected.classification$description$CODE_PONCTUE)]
          
          mat$p <-as.character(signif(mat$p,2))
          mat$Dmed.5 <-as.character(signif(mat$Dmed.5,2))
          
          names(mat)[6:9] <-c("Confidence","Intensity","Frequency","FWI")
          
          
          ################separating the mats
          
          mat.exposed <-mat[is.element(mat[,activity.type],list.exposed),]
          
          mat.unexposed <-mat[is.element(mat[,activity.type],list.unexposed),]
          
          mat.unexposed <-mat.unexposed[,c(1,2,4,5,10)]
          
          
          result <-data.frame(Activity.code=mat.unexposed[,1])
          
          result <-cbind(result,mat.unexposed[,-1])
          
          return(result)
          
        }) 
        
    #############################JEM CALCULATION BY ACTIVITY
        
        jem.calc.activity <- reactive({
          
          ###inpout
          
          classification <- input$select.classification
          
          index <-classif.index$index[classif.index$name==classification]
          
          
          selected.classification <-classifications[[index]]
          
          activity <-v$activity.choice
          
          period <-input$select.period.4
          
          i<-periods$code[periods$label==period]
          
          sort.order <-input$selected.sort.order.1
          
          ##criteria for individual jobs
          
          job.min.reliability <-input$job.min.reliability
          job.min.concentration <-input$job.min.concentration
          job.min.frequency <-input$job.min.frequency
          job.min.FWI <-input$job.min.FWI
          
          ##criteria for cells
          
          cell.min.njob <-input$cell.min.njob
          cell.min.nsubjects <-input$cell.min.nsubjects
          cell.min.probability <-input$cell.min.probability
          cell.min.FWImedian <-input$cell.min.FWImedian
          
          
          #######selecting the occupational histories
          
          #######selecting the occupational histories
          
          
          level.activity <-selected.classification$description$level[selected.classification$description$label.show==activity]
          
          restrict <-!is.na(canjem.wk[,level.activity]) & canjem.wk[,level.activity]==selected.classification$description$CODE_PONCTUE[selected.classification$description$label.show==activity] 
          
          wk <-canjem.wk[restrict,]
          
          activity.type <-level.activity
          
          #######selecting the exposure data
          
          if (job.min.reliability==1) jdb <-canjem.jdb123 else jdb <-canjem.jdb23
          
          jdb <-jdb[jdb$R_FINAL>=job.min.reliability,]
          
          ######################  matrix calculation for the selected period
          
          mat <-matrix.fun(jdb,wk,vec.dim=activity.type,
                           agents=unique(jdb$IDCHEM),
                           type='R1expo',
                           Dmin=job.min.FWI,
                           Fmin=job.min.frequency ,
                           Cmin=job.min.concentration,
                           Nmin=1,Nmin.s=1,
                           c(periods$yearin[i],periods$yearout[i]),
                           Fcut= c(0, 2, 12, 40, 250)) 
          
          
          ###################formating mat
          
          
          ######most frequent categories     
          
          mat$most.freq.confidence <- ddply(mat ,~ cell , function(x){c(3:1)[which.max(c(x$n.R3[1],x$n.R2[1],x$n.R1[1]))]})$V1
          
          mat$most.freq.intensity <- ddply(mat ,~ cell , function(x){c(3:1)[which.max(c(x$n.C3[1],x$n.C2[1],x$n.C1[1]))]})$V1
          
          mat$most.freq.frequency <- ddply(mat ,~ cell , function(x){c(4:1)[which.max(c(x$n.F4,x$n.F3[1],x$n.F2[1],x$n.F1[1]))]})$V1
          
        ######exclusion criterias
          
          list.exposed <-mat$cell[mat$ntot>=cell.min.njob &
                               mat$nsub>=cell.min.nsubjects &
                               mat$Dmed.5>=cell.min.FWImedian &
                               mat$p>=cell.min.probability]
          

          
          
          #####final sorting
          
          if (sort.order==1) mat<-mat[order(mat$p,decreasing = TRUE),]
          if (sort.order==2) mat<-mat[order(mat$most.freq.intensity ,decreasing = TRUE),]
          if (sort.order==3) mat<-mat[order(mat$most.freq.frequency ,decreasing = TRUE),]
          if (sort.order==4) mat<-mat[order(mat$most.freq.confidence ,decreasing = TRUE),]
          if (sort.order==5) mat<-mat[order(mat$Dmed.5 ,decreasing = TRUE),]
          
          
          
          ######nicer labels
          
          mat$most.freq.confidence <-revalue(as.character(mat$most.freq.confidence), c("1"="Possible","2"="Probable","3"="Definite"))
          
          mat$most.freq.intensity <- revalue(as.character(mat$most.freq.intensity), c("1"="Low","2"="Medium","3"="High"))
          
          mat$most.freq.frequency <- revalue(as.character(mat$most.freq.frequency), c("1"="[0-2h[","2"="[2-12h[","3"="[12-40h[","4"="[40h+]"))
          
          mat$Agent.name<-agents.def$label[match(mat$idchem,agents.def$idchem)]
          
          mat$p <-as.character(signif(mat$p,2))
          mat$Dmed.5 <-as.character(signif(mat$Dmed.5,2))
          
         
          ################exclusions
          
          mat <-mat[is.element(mat$cell,list.exposed),]
          
         
          ###final
          
          results <-data.frame(Agent=mat$Agent.name,
                               p=mat$p,
                               ntot=mat$ntot,
                               n.sub=mat$nsub,
                               nexp=mat$nexp,
                               nexp.s=mat$nexp.s,
                               Confidence=mat$most.freq.confidence,
                               Intensity=mat$most.freq.intensity,
                               Frequency=mat$most.freq.frequency,
                               FWI=mat$Dmed.5)
          
          return(results)
          
        }) 
        
#################################### TAB FOR PARAMETERS
  
         
        #Nom de la classification sélectionnée
        
        
        output$classification.name.1 <- renderText({
          
          classification <- input$select.classification
          
          if (is.null(classification)) "None selected" else 
          
          as.character(classif.index$full.name[classif.index$name==classification])
          
        })
        
        
        
        
         #agent name
          output$agentname.2 <- renderText({
            
             agent.selected <-agent.choice()
            
            
            if (is.null(agent.selected)){
              "None selected"
            } else{
              
              
              agent.selected
            }
          })
          
          #activity name
          output$activname.2 <- renderText({
            
            activity.selected <-v$activity.choice
            
            
            if (is.null(activity.selected)){
              "None selected"
            } else{
              
              
              activity.selected
            }
            
          })
          
          ###########conditional display for the activity table
          
          output$ui1 <-renderUI({
            
            activity <-v$activity.choice
            
            if (is.null(activity) ) return("No activity selected")
            
            tableOutput("activ.numbers")
            
          })
          
          
          output$activ.numbers <-renderTable({
            
            results <-period.availability()
            
            results[,c(2,5,6)]
            
          },include.rownames = FALSE) 
          
          
######################################################tTAB FOR AGENT SELECTION
  
      
        output$agentdef <- renderText({
        
        agent.selected <-agent.choice()
       
        if (is.null(agent.selected)){
          "None selected"
        } else{
          
          agents.def$definition[agents.def$label==agent.selected]
        }
      })
      
      output$agentname <- renderText({
        
        agent.selected <-agent.choice()
        
        
        if (is.null(agent.selected)){
          "None selected"
        } else{
          
          
          agent.selected
        }
      })
      
      output$n.job.exposed <- renderText({
        
        results <- n.job.exposedperagent()
        
        results
      })
      
      
      output$n.job.exposed.perc <- renderText({
        
        results <- n.job.exposedperagent()
        
        results <-paste(signif(100*results/length(canjem.wk[,1]),2)," %",sep="")
        
        results
      })
      
      
      
########################################################### TAB FOR classification system SELECTION 
  
        
        #############type de classification d'activité
      
      output$list.of.activityclassif.ui <- renderUI({
        
        list.class <-list.of.activityclassif()
        
         
        selectInput("select.classification","Available classifications",list.class,selected="classif.index$name[1]")
        
        
      })
      
      
      #Nom de la classification sélectionnée
      
      
      output$classification.name <- renderText({
        
        classification <- input$select.classification
        
        as.character(classif.index$full.name[classif.index$name==classification])
        
      })
      
########################################################### TAB FOR ACTIVITY SELECTION 
      
          
        #########nom et description
          
          output$activdef <- renderText({
            
            
            #####selected classification
            
            classification <- input$select.classification
            
            index <-classif.index$index[classif.index$name==classification]
            
            
            selected.classification <-classifications[[index]]
            
            
            ###### selected activity
            
            activity.selected <-v$activity.choice
            
            
            restrict.activity<-substring(selected.classification$description$label.show,1,min(10,nchar(selected.classification$description$label.show)))==substring(activity.selected,1,min(10,nchar(activity.selected)))
            
         
            selected.classification$description$DESCRIPTION[restrict.activity]
          
          })
          
          
          ##########
          

          
          output$activname <- renderText({
            
            #####selected classification
            
            classification <- input$select.classification
            
            index <-classif.index$index[classif.index$name==classification]
            
            
            selected.classification <-classifications[[index]]
            
            
            ###### selected activity
            
            activity.selected <-v$activity.choice
            
            
            restrict.activity<-substring(selected.classification$description$label.show,1,min(10,nchar(selected.classification$description$label.show)))==substring(activity.selected,1,min(10,nchar(activity.selected)))
            
            
            selected.classification$description$LIB_PRINCIPAL[restrict.activity]
            
          })
          
        
           
   
   
   ###########################################################TAB FOR CANJEM CELL 
   
   #agent name
   output$agentname.1 <- renderText({
     
     result <-agent.choice()
     
     result
     
   })
   
   #activity name
   output$activname.1 <- renderText({
     
     activity.selected <-v$activity.choice
     
     if (is.null(activity.selected)){
       "None selected"
     } else{
       
       
       activity.selected
     }
     
   })
   
   #period name
    

   output$period.1 <- renderText({
     
     period <- input$select.period.1
     
     as.character(period)
     
   })
   
   

   
   ###list of available periods
   
   output$period.list <- renderUI({
     
     activity <-v$activity.choice
     
     agent <-agent.choice
     
     if (is.null(activity) | is.null(agent)) return("")
     
     period.object <-period.availability()
     
     avail.list <-period.object$label[period.object$ok]
     
     if (length(avail.list)==0) return ("According to the chosen criteria of minimal number of jobs and subjects per cell, no period has sufficient information for calculation.")
     
     selectInput("select.period.1","Available periods",avail.list)
     
     
   })
   
   
   #####graph with grid arrange
   
       #### definition of graphs
       plot_vt_reactive <- reactive({
         
         period <- input$select.period.1
         
         i<-periods$code[periods$label==period]
         
         mat <-jem.calc()
     
           # Prevalence - using table
           
           prev.df <- data.frame(rnames = c("Number of jobs","number of subjects", "Number of exposed jobs","Number of exposed subjects","Probability of exposure based on jobs"),
                                 data = c(mat[[i]]$ntot, mat[[i]]$nsub, mat[[i]]$nexp,mat[[i]]$nexp.s, paste(signif(mat[[i]]$p,2),"%",sep="")))
           
           if(is.na(prev.df$data[4])) prev.df$data[4] <-0 
           
           # Frequency
           
           freq.df <- data.frame(ratings =c("[0-2h[", "[2-12h[","[12-40h[", "[40h+]"),
                                 p = as.numeric(100*mat[[i]][,c(19:22)]/mat[[i]]$nexp))
           
           freq.df$ratings<- ordered(freq.df$ratings,levels=c("[0-2h[", "[2-12h[","[12-40h[", "[40h+]"))
           
           
           p.freq <- ggplot(freq.df, aes(x=factor(ratings), y=p, fill=factor(ratings)))+
             geom_bar(stat="identity", position="dodge", alpha=.8,fill=c('steelblue1','royalblue2',"royalblue4","navyblue"))+
             xlab("Frequency category (hours per week)") + ylab("Proportion (%)")+ 
             ggtitle("Distribution of frequency of exposure categories")+
             theme(legend.position="none")+
             scale_y_continuous(limits=c(0,100), expand=c(0,0))
           
           
           # Intensity
           
           conc.df <- data.frame(ratings =c("Low", "Medium","High"),
                                 p = as.numeric(100*mat[[i]][,c(15:17)]/mat[[i]]$nexp))
           
           conc.df$ratings<- ordered(conc.df$ratings,levels=c("Low", "Medium","High"))
           
           
           p.conc <- ggplot(conc.df, aes(x=factor(ratings), y=p, fill=factor(ratings)))+
             geom_bar(stat="identity", position="dodge", alpha=.8,fill=c('palegreen2','seagreen3',"springgreen4"))+
             xlab("Intensity category") + ylab("Proportion (%)")+ 
             ggtitle("Distribution of intensity of exposure categories")+
             theme(legend.position="none")+
             scale_y_continuous(limits=c(0,100), expand=c(0,0))
           
           #reliability
           
           reliab.df <- data.frame(ratings =c("Possible", "Probable","Definite"),
                                   p = as.numeric(100*mat[[i]][,c(11:13)]/mat[[i]]$nexp))
           
           reliab.df$ratings<- ordered(reliab.df$ratings,levels=c("Possible", "Probable","Definite"))
           
           
           p.reliab <- ggplot(reliab.df, aes(x=factor(ratings), y=p, fill=factor(ratings)))+
             geom_bar(stat="identity", position="dodge", alpha=.8,fill=c('pink','hotpink2',"maroon4"))+
             xlab("Confidence category") + ylab("Proportion (%)")+ 
             ggtitle("Distribution of confidence categories")+
             theme(legend.position="none")+
             scale_y_continuous(limits=c(0,100), expand=c(0,0))
           
           
                       ################making the summary plot
                       
          
                  
                      wrap_gtable(arrangeGrob(tableGrob(prev.df, rows = rep("", nrow(prev.df)), cols = c("Probability of exposure", "")),
                                              p.freq, p.conc,p.reliab) )
             
          
                     
           })
         
   
             ##########################plotting the plot
             
             output$plot_vt <- renderPlot({
               #print("plot output")
               p <-plot_vt_reactive()
               
               print(p)
             }, width=800, height=600)
   
   
   
   
   #############################the FWI plot
   
   
   output$plot_FWI <-renderPlot({
     
     period <- input$select.period.1
     
     i<-periods$code[periods$label==period]
     
     #i<-1
     
     mat <-jem.calc()
     
     FWI <-mat[[i]]$Dmed.5
     
     ###############graph parameter
     
     slope <-5/(log(25)-log(0.05))
     
     intercept1 <-10-slope*log(0.05)
     intercept2 <-5+slope*log(0.05)
     
     
     #data.frame with x, y ID
     
     n.poly <-1000
     
     initial.positions <-data.frame(x=seq(from=log(0.05),to=log(25),length.out = n.poly))
     
     initial.positions$y1 <-slope*initial.positions$x+intercept1
     
     initial.positions$y2 <-(-slope)*initial.positions$x+intercept2
     
     initial.positions$ID <-factor(1:(n.poly))
     
     
     #########with segment
     
     
     s.positions <- data.frame(
       xstart = initial.positions$x,
       xend = initial.positions$x,
       ystart = initial.positions$y2,
       yend = initial.positions$y1)  
     
     
     s.positions$value <-1:(n.poly)
     
     
     p2 <- ggplot(s.positions)
     p2 <-p2 + geom_segment(aes(x=xstart,xend=xend,y=ystart,yend=yend,color=value))
     p2<- p2 + scale_color_gradient2(limits=c(0, n.poly), low = "seagreen4", mid="yellow3",high = "coral3",midpoint = n.poly/2)+xlim(c(log(0.03),log(50)))+ylim(c(-2,17))
     
     ###formatting
     
     p2 <-p2+xlab('Frequency weighted intensity')+
       theme(aspect.ratio = 1/3)  +
       theme(axis.line=element_blank(),
             axis.text.x=element_blank(),
             axis.text.y=element_blank(),
             axis.ticks=element_blank(),
             axis.title.x=element_blank(),
             axis.title.y=element_blank(),
             legend.position="none",
             panel.background=element_blank(),
             panel.border=element_blank(),
             panel.grid.major=element_blank(),
             panel.grid.minor=element_blank(),
             plot.background=element_blank())
     
     ########## adding the ref bars
     
     ref <-data.frame(xstart=log(c(0.05,1,5,25)))
     ref$xend <-ref$xstart
     ref$ystart <-(-slope)*ref$xstart+intercept2
     ref$yend <-(slope)*ref$xstart+intercept1
     
     
     ref$explanation <-c("FWI=0.05\n(2h at low)","FWI=1\n40h at low",
                         "FWI=5\n40h at medium\n 8h at high",
                         "FWI=25\n40h at high")
     
     for (i in 1:length(ref[,1])) {
       p2 <-p2+ geom_segment(x=ref$xstart[i],xend=ref$xend[i],y=ref$ystart[i],yend=ref$yend[i],color="black",size=1)
       
       if ((i %% 2)==0) p2 <-p2+ annotate("text", x = ref$xstart[i], y = ref$ystart[i]-0.7, label = ref$explanation[i]) else p2 <-p2+ annotate("text", x = ref$xstart[i], y = ref$yend[i]+1+0.25*i, label = ref$explanation[i])
       
     }
     
     
     ######adding the real value
     
     p2 <- p2+ geom_segment(x=log(FWI),xend=log(FWI),y=(-slope)*log(FWI)+intercept2,yend=(slope)*log(FWI)+intercept1,color="blue",size=2)+
       annotate("text",x=log(FWI)+0.2,y=(intercept2+intercept1)/2,color="blue",size=5,label=paste("FWI\n",signif(FWI,2),sep=""))
     
     #######final graph
     
     print(p2)
     
     
   }, width=900, height=300)
   
   
 
       

   #####################conditional display for the multiple graphs
   
   output$ui3 <-renderUI({
     
     activity <-  v$activity.choice
     
     agent <- agent.choice()
     
     if (is.null(activity) | is.null(agent)) return("")
     
     period.object <-period.availability()
     
     avail.list <-period.object$label[period.object$ok]
     
     if (length(avail.list)==0) return ("")
     
     plotOutput("plot_vt")
     
   })  
   
   
   #####################conditional display for the multiple graphs
   
   output$ui4 <-renderUI({
     
     activity <-  v$activity.choice
     
     agent <- agent.choice()
     
    if (is.null(activity) | is.null(agent)) return("")
     
     period.object <-period.availability()
     
     avail.list <-period.object$label[period.object$ok]
     
     if (length(avail.list)==0) return ("")
     
     
     plotOutput("plot_FWI")
     
   })
   
   
   
   
######################################### TAB FOR AGENT - EXPOSED ACTIVITIES   
   
   #agent name
   output$agentname.3 <- renderText({
     
     agent <- agent.choice()
     
     if (is.null(agent)) return("No agent selected")
     
     agent
     
   })
   

   #period name
   
   output$period.2 <- renderText({
     
     period <- input$select.period.2
     
     as.character(period)
     
   })
   
   
   #activity resolution
   
   output$activity.resolution <- renderText({
     
     resolution <- input$selected.resolution
     
     as.character(resolution)
     
   })
   
   ###list of available periods
   
   output$period.list.2 <- renderUI({
     
     avail.list <-periods$label
     
     selectInput("select.period.2","Select a period",avail.list)
     
     
   })   
   
   
   ###list of available resolutions
   
   output$select.resolution <- renderUI({
     
     classification <- input$select.classification
     
     index <-classif.index$index[classif.index$name==classification]
     
     avail.list.reso <-classifications[[index]]$level.labels
     
     selectInput("selected.resolution","Select an activity resolution",avail.list.reso)
     
     
   }) 
   
   
   
   ##########conditional panel for the results
   output$by.agent.exposed.ui <-renderUI({
     
      agent <- agent.choice()
     
     if (is.null(agent)) return("No agent selected")
     
        tableOutput("by.agent.exposed")
     
   })  
   
   ####results table
   
   output$by.agent.exposed <-renderTable({
     
     results <-jem.calc.agent.exposed()
     
     results
     
   },include.rownames = FALSE)
   
##########################################TAB FOR AGENT - UNEXPOSED JOBS 
   
   #agent name
   output$agentname.4 <- renderText({
     
     agent <- agent.choice()
     
     if (is.null(agent)) return("No agent selected")
     
     agent
     
   })
   
   
   #period name
   
   output$period.3 <- renderText({
     
     period <- input$select.period.3
     
     as.character(period)
     
   })
   
   
   #activity resolution
   
   output$activity.resolution.1 <- renderText({
     
     resolution <- input$selected.resolution.1
     
     as.character(resolution)
     
   })
   
   ###list of available periods
   
   output$period.list.3 <- renderUI({
     
     avail.list <-periods$label
     
     selectInput("select.period.3","Select a period",avail.list)
     
     
   })   
   
   
   ###list of available resolutions
   
   output$select.resolution.1 <- renderUI({
     
     classification <- input$select.classification
     
     index <-classif.index$index[classif.index$name==classification]
     
     avail.list.reso <-classifications[[index]]$level.labels
     
     selectInput("selected.resolution.1","Select an activity resolution",avail.list.reso)
     
     
   }) 
   
   
   
   ##########conditional panel for the results
   output$by.agent.unexposed.ui <-renderUI({
     
     agent <- agent.choice()
     
     if (is.null(agent)) return("No agent selected")
     
     tableOutput("by.agent.unexposed")
     
   })  
   
   ####results table
   
   output$by.agent.unexposed <-renderTable({
     
     results <-jem.calc.agent.unexposed()
     
     results
     
   },include.rownames = FALSE)
   
   
   ######################################### TAB FOR ACTIVITY
   
   #activity name
   output$activname.3 <- renderText({
     
     activity <- v$activity.choice
     
     if (is.null(activity)) return("No activity selected")
     
     activity
     
   })
   
   
   #period name
   
   output$period.4 <- renderText({
     
     period <- input$select.period.4
     
     as.character(period)
     
   })
   
   
     ###list of available periods
   
   output$period.list.4 <- renderUI({
     
     activity <-v$activity.choice
     
     if (is.null(activity)) return("")
     
     period.object <-period.availability()
     
     avail.list <-period.object$label[period.object$ok]
     
     if (length(avail.list)==0) return ("According to the chosen criteria of minimal number of jobs and subjects per cell, no period has sufficient information for calculation.")
     
     selectInput("select.period.4","Available periods",avail.list)
     
     
   })   
   
   

   ##########conditional panel for the results
   output$by.activity.ui <-renderUI({
     
     activity <- v$activity.choice
     
     if (is.null(activity)) return("No activity selected")
     
     tableOutput("by.activity")
     
   })  
   
   ####results table
   
   output$by.activity <-renderTable({
     
     activity <-v$activity.choice
     
     if (is.null(activity)) return("")
     
     period.object <-period.availability()
     
     avail.list <-period.object$label[period.object$ok]
     
     if (length(avail.list)==0) return ("According to the chosen criteria of minimal number of jobs and subjects per cell, no period has sufficient information for calculation.")
     
     
     results <-jem.calc.activity()
     
     results
     
   },include.rownames = FALSE)
   
   
   
   
}) 


############################################################
#
##########################big matrix function
#
#################################################################


######################################################################################################################################
#Fonction de cr?ation des matrice de pr?valence : Version Juillet 2014
## --- MODIFI?E PAR JF JUL03 2015 pour corriger restriction sur Nmin/Nmin.s (~ligne 244)
##	-- + Modification DEC04 2015 pour attribution des p?riodes aux jobs
## MODIFIED JAN25 2016 to add categorical frequency
## MODIFIED MAY10 2016 to add "R12Nexpo" and "R12nouse"
#######################################
#entr?e : Une BD de type JDB, une BD de type OccInd, les param?tres suivants
#
# 
# typeR : type de matrice : 'R1expo' R1=expos?, 'R1Nexpo' R1=non expos?, 'R1nouse' R1 non consid?r?, retir? des donn?es incluant workoccind
#           'R12Nexpo' R1+R2=non expos?, 'R12nouse' R1+R2 non consid?r?, retir? des donn?es incluant workoccind
# Nmin : nombre Jobs minimal pour une estimation
# Nmin.s : nombre de sujet minmal pour une estimation
# Dmin : Dose minimale pour inclure un job dans le calcul
# Cmin : Concentration minimale pour inclure un job dans le calcul
# Fmin : Fr?quence minimale pour inclure un job dans le calcul
#
# dimension : vector of variables in the OccInd data.frame to act as stratification variables
#
# agents : list of agents (idchem) of interest
#
# time breaks : vector of year points for making time periods
#
#
#############################################################################################
#sortie : 1 matrice de pr?valence avce les cellules comme combinaison des variables s?lectionn?es
#
# 
#
# quantit?es estim?es
# p : pr?valence
# ntot : total de jobs
# nsub : total de sujets
# nexp : jobs exposed
# nexps : sujets expos?s
# n.Ri : nombre de jobs avec fiabilit? i (0 ? 3)
# n.Ci : nombre de jobs avec concentration i (0 ? 3)
# n.Fi : nombre de jobs avec fr?quence i (0 ? i)  # Default = 4
# p.Ri : proportion de jobs avec fiabilit? i (0 ? 3)
# p.Ci : proportion de jobs avec concentration i (0 ? 3)
# p.Fi : proportion de jobs avec fr?quence i (0 ? i) # Default = 4
#
#exposition metrics
#
# Cmoyi  : concentration moyenne pour les ?chelles 123,139,1525,110100
# Dmoyi  : : concentration moyenne pour les ?chelles 123,139,1525,110100
# Cmed    concentration mediane (1525)
# Dmed   dose m?diane
# Fmoy    Fr?quence moyenne
# Fmed    Fr?quenc m?diane
#
#
#
#
#Variables obligatoires dans JDB : ID, JOB, IDCHEM, YEARIN, YEAROUT, R_FINAL, F_FINAL, C_FINAL, C_FINAL_CAT
#
#Variables obligatoires dans workoccind : ID, JOB, YEARIN, YEAROUT
##
############################################################################################################

# 
# ### Add-on JF - AUG14 2014 - Debugging information to go inside function
# 
# ## Load data files
# setwd("C:\\CRCHUM\\CANJEM\\Data files")
# ## Chemical coding
# canjem.jdb <-read.csv('C:\\CRCHUM\\CANJEM\\Data files\\canjem.jdb.123.D.csv',stringsAsFactors=F)
# ## Job information
# canjem.wk <-read.csv('C:\\CRCHUM\\CANJEM\\Data files\\canjem.workoccind.csv',stringsAsFactors=F)
# # Change names of Year in/out variables
# names(canjem.wk) <- gsub("YEARIN",'YEARIN', names(canjem.wk))
# names(canjem.wk) <- gsub("YEAROUT",'YEAROUT', names(canjem.wk))
# names(canjem.jdb) <- gsub("YEARIN",'YEARIN', names(canjem.jdb))
# names(canjem.jdb) <- gsub("YEAROUT",'YEAROUT', names(canjem.jdb))
# # Extract 4-digit CCDO code from 7-digit variable
# canjem.wk$ccdo4d <- substr(gsub("-", "", canjem.wk$CCDP1971), 1, 4)
# canjem.wk$ccdo3d <- substr(gsub("-", "", canjem.wk$CCDP1971), 1, 3)
# ## Agent codes/names
# agents <-read.csv('AGENTS CODES.csv',sep=';',stringsAsFactors=F)
# agents.label <-read.csv('AGENTS LABELS.csv',sep=';',stringsAsFactors=F)
# agents$label<-agents.label$LblEn[match(agents$idchem,agents.label$Code)]
# agent <-c('110016', '460003')
# 
# #### Input for the function itself
# jdb=canjem.jdb
# workoccind=canjem.wk
# vec.dim=c('ccdo4d', 'ccdo3d')
# agents=agent
# Nmin=5
# Nmin.s=5
# Dmin=0.05
# Fmin=0.5
# Cmin=1
# Rmin=1
# time.breaks=c(min(canjem.wk$YEARIN),1950, 1970, 1985, max(canjem.wk$YEAROUT))
# type='R12nouse'
# Fcut = c(0, 2, 12, 40, max(canjem.jdb$F_FINAL)) # As  [0,2)   [2,12)  [12,40) [40,MAX]
# 
# 
# # RUN IT
# mat.samp <- matrix.fun(jdb=canjem.jdb,workoccind=canjem.wk,
# vec.dim=c('ccdo4d'),
# agents=agent
# ,Nmin=1, Nmin.s=1, Dmin=0.05,Fmin=0.5,Cmin=1,type='R1expo',
# time.breaks=c(min(canjem.wk$YEARIN),max(canjem.wk$YEAROUT)))


matrix.fun <- function(jdb,workoccind,vec.dim,agents,type='R1expo',Dmin,Fmin,Cmin,Nmin,Nmin.s,time.breaks,
                       Fcut= c(0, 2, 12, 40, max(canjem.jdb$F_FINAL)))		{
  
  
  
  #jdb <-jdb  
  #workoccind <-wk
  #vec.dim<-"CITP1968" 
  #agents<- 
  
  ###creating the Y axis : combinations of values of variables of vec.dim
  
  n.dim <-length(vec.dim)
  
  ##creating the time periods
  
  period.vec <-character(length(time.breaks)-1)
  for (i in 1:(length(time.breaks)-1)) period.vec[i] <-paste(time.breaks[i],time.breaks[i+1]-1*ifelse(i==(length(time.breaks)-1),0,1),sep='-')
  
  
  ######################################################
  ###creating a base exposure database
  ########################################################
  
  jdb <-jdb[is.element(jdb$IDCHEM,agents),]
  
  jdb$D_FINAL <-jdb$C_FINAL*jdb$F_FINAL/40
  
  jdb <-jdb[jdb$C_FINAL>=Cmin &
              jdb$D_FINAL>=Dmin &
              jdb$F_FINAL>=Fmin,]
  
  ######################################################
  ### Categorize continuous frequency values (added JAN26 2016)
  ########################################################
  
  # Note - apparently (from help of "cut" function) "findInterval" is faster
  nlevsFreq <- length(Fcut)-1
  jdb$F_FINAL_CAT <- factor(findInterval(x=jdb$F_FINAL, vec=Fcut, rightmost.closed = T,  all.inside = T),
                            levels=1:nlevsFreq)
  # tapply(jdb$F_FINAL, jdb$F_FINAL_CAT, range) # To check that correct categorization has been applied
  
  
  
  ##################################################################################
  #Creating a base prevalence database
  ##################################################################################
  
  bd.prev.1 <-workoccind
  
  bd.prev.1$id.job <-paste(bd.prev.1$ID,bd.prev.1$JOB,sep='-')
  
  bd.prev.1 <-bd.prev.1[,is.element(names(bd.prev.1),c(c('ID','JOB','YEARIN','YEAROUT'),
                                                       vec.dim))]
  
  ###formattage des p?riodes
  
  
  ##### retrait des emplois <a l'ext?rieur de la p?riode d?finie par time.breaks
  
  bd.prev.1<-bd.prev.1[bd.prev.1$YEARIN<=max(time.breaks),]
  bd.prev.1<-bd.prev.1[bd.prev.1$YEAROUT>=min(time.breaks),]
  
  bd.prev.1$YEAROUT[bd.prev.1$YEAROUT>max(time.breaks)] <-max(time.breaks)
  bd.prev.1$YEARIN[bd.prev.1$YEARIN<min(time.breaks)] <-min(time.breaks)
  
  #####ann?es dans chaque p?riode ####on d?cide toujours d'exclure YEAROUT
  
  bd.prev.1$yearspan <- paste(bd.prev.1$YEARIN,bd.prev.1$YEAROUT,sep='-')
  ## Modified approach to attribute periods in the JEM
  ##  Old version with "fun.yrs" was done using loop across periods
  ##  For each job/period combination, two sequences were created each time
  ## NEW APPROACH
  # step 1 - create list of years for each time period
  period.list <- lapply(as.list(period.vec), function(x) {
    seq(from=as.numeric(substr(x, 1, 4)), to=as.numeric(substr(x, 6,9)), 1) })
  # step 2 - new "fun.yrs" function that does all time periods at once for each job
  fun.yrs2 <- function(x,period.list){
    y.in <-as.numeric(substring(x,1,4)) ; y.out <-as.numeric(substring(x,6,9))
    ifelse(y.in==y.out, yrs <- y.in, yrs <-seq(y.in,y.out,by=1))
    res <- as.numeric(unlist(lapply(period.list, function(y) length(which(yrs%in%y)))))
    return(res)}
  #fun.yrs2(bd.prev.1$yearspan[1], period.list) ### TEST IT
  # step 3 - run it using lapply across all jobs
  # 3a) output is a list of length "njobs", each element is a vector of length "period.vec"
  # 3b) Collapse list into a table of dim(njobs, nperiods) using "rbind"
  # 3c) Combine with bd.prev.1 using cbind as in previous versions.
  bd.prev.1 <- cbind(bd.prev.1, 
                     do.call("rbind", lapply(bd.prev.1$yearspan, function(x) fun.yrs2(x, period.list))))
  dimnames(bd.prev.1)[[2]][(1+ncol(bd.prev.1)-length(period.vec)):ncol(bd.prev.1)] <- period.vec
  
  
  ###fabriCATion de la matrice complete de pr?valence
  
  #index de repliCATion
  
  if (type=='R1nouse'|type=='R12nouse') {
    if(type=='R1nouse'){
      ##chaque agent possede une longeur de workoccind sp?cifique
      
      bd.prev.2 <-bd.prev.1[0,]
      bd.prev.2$IDCHEM <-numeric(0)
      
      for (i in 1:length(agents))  {
        restrict <-jdb$R_FINAL==1 & jdb$IDCHEM==agents[i]
        r1nouse <-paste(jdb$ID[restrict],jdb$JOB[restrict],sep='-')
        
        restrict.1 <-!is.element(paste(bd.prev.1$ID,bd.prev.1$JOB,sep='-'),r1nouse)
        temp <-bd.prev.1[restrict.1,]
        temp$IDCHEM <-agents[i]
        bd.prev.2<-rbind(bd.prev.2,temp)
        
        
      }
    } else {		
      
      bd.prev.2 <-bd.prev.1[0,]
      bd.prev.2$IDCHEM <-numeric(0)
      
      for (i in 1:length(agents))  {
        restrict <-(jdb$R_FINAL==1|jdb$R_FINAL==2) & jdb$IDCHEM==agents[i]
        r1nouse <-paste(jdb$ID[restrict],jdb$JOB[restrict],sep='-')
        
        restrict.1 <-!is.element(paste(bd.prev.1$ID,bd.prev.1$JOB,sep='-'),r1nouse)
        temp <-bd.prev.1[restrict.1,]
        temp$IDCHEM <-agents[i]
        bd.prev.2<-rbind(bd.prev.2,temp)
        
        
      }					  
    }
  } else {						  
    
    ##for anything but R1nouse, same bd.prev.1 for any agent
    
    index <-rep(1:length(bd.prev.1$ID),rep(length(agents),length(bd.prev.1$ID)))
    
    bd.prev.2 <-bd.prev.1[index,]
    
    bd.prev.2$IDCHEM <-c(rep(agents,length(bd.prev.1[,1])))
    
  }
  
  ####################################creation de la matrice pr?valence ################################################
  
  ##creation des snips
  
  bd.prev.2$snip <-paste(bd.prev.2$ID, bd.prev.2$JOB,bd.prev.2$IDCHEM,sep='-')
  
  jdb$snip <-paste(jdb$ID, jdb$JOB,jdb$IDCHEM,sep='-')
  
  ##variable d'exposition LOGIK  ###d?pend du type
  
  bd.prev.2$expo.1 <-rep(F,length(bd.prev.2[,1]))
  
  if (type=='R1Nexpo'|type=='R12Nexpo'){ 
    if (type=='R1Nexpo'){
      bd.prev.2$expo.1[is.element(bd.prev.2$snip,jdb$snip[jdb$R_FINAL!=1])] <-T #valable pour R1Nexpo
    }else{
      bd.prev.2$expo.1[is.element(bd.prev.2$snip,jdb$snip[jdb$R_FINAL!=1&jdb$R_FINAL!=2])] <-T #valable pour R12Nexpo
    }
    
  }else{
    bd.prev.2$expo.1[is.element(bd.prev.2$snip,jdb$snip)] <-T #valable pour R1expo et R1nouse
  }
  
  #Variable de cellule (hors p?riode)
  
  bd.prev.2$cell.noperiod <-bd.prev.2[,match(vec.dim[1],names(bd.prev.2))]
  
  if (n.dim>1) for (i in 2:n.dim) bd.prev.2$cell.noperiod <-paste(bd.prev.2$cell.noperiod,bd.prev.2[,match(vec.dim[i],names(bd.prev.2))],sep='-')
  
  bd.prev.2$cell.noperiod <-paste(bd.prev.2$cell.noperiod,bd.prev.2$IDCHEM,sep='-')
  
  #####ann?es expos?es dans chaque p?riode 
  
  for (i in 1:length(period.vec)) {
    
    bd.prev.2 <-cbind(bd.prev.2,bd.prev.2[,match(period.vec[i],names(bd.prev.2))])
    bd.prev.2[bd.prev.2$expo.1==F,dim(bd.prev.2)[2]] <-0
    names(bd.prev.2)[dim(bd.prev.2)[2]] <-paste('EXP',period.vec[i],sep='-') }
  
  ##ajout des information sur l'exposition sur chaque snip  ####d?pend du type
  
  ######pour R1Nexpo
  
  if (type=='R1Nexpo'|type=='R12Nexpo'){ 
    if (type=='R1Nexpo'){
      jdb2 <-jdb[jdb$R_FINAL!=1,]	####jdb sans R1
    }else{
      jdb2 <-jdb[jdb$R_FINAL!=1&jdb$R_FINAL!=2,]
    }
    
    
    bd.prev.2$R_FINAL<-jdb2$R_FINAL[match(bd.prev.2$snip,jdb2$snip)]
    bd.prev.2$F_FINAL<-jdb2$F_FINAL[match(bd.prev.2$snip,jdb2$snip)]
    bd.prev.2$F_FINAL_CAT<-jdb2$F_FINAL_CAT[match(bd.prev.2$snip,jdb2$snip)]
    bd.prev.2$C_FINAL<-jdb2$C_FINAL[match(bd.prev.2$snip,jdb2$snip)]	
    bd.prev.2$C_FINAL_CAT<-jdb2$C_FINAL_CAT[match(bd.prev.2$snip,jdb2$snip)]
    bd.prev.2$D_FINAL<-jdb2$D_FINAL[match(bd.prev.2$snip,jdb2$snip)]
    
  } else{  ###pour R1expo et R1nouse tout est d?j? pr?t
    
    
    bd.prev.2$R_FINAL<-jdb$R_FINAL[match(bd.prev.2$snip,jdb$snip)]
    bd.prev.2$F_FINAL<-jdb$F_FINAL[match(bd.prev.2$snip,jdb$snip)]
    bd.prev.2$F_FINAL_CAT<-jdb$F_FINAL_CAT[match(bd.prev.2$snip,jdb$snip)]
    bd.prev.2$C_FINAL<-jdb$C_FINAL[match(bd.prev.2$snip,jdb$snip)]	
    bd.prev.2$C_FINAL_CAT<-jdb$C_FINAL_CAT[match(bd.prev.2$snip,jdb$snip)]
    bd.prev.2$D_FINAL<-jdb$D_FINAL[match(bd.prev.2$snip,jdb$snip)]
    
  }								
  
  
  ######## matrices de r?sultats
  
  ###tableau : creation sequentielle pour chaque periode
  
  
  mat <-data.frame(cell.noperiod=character(0),period=character(0),idchem=numeric(0),p=numeric(0),ntot=numeric(0),
                   nsub=numeric(0),
                   nexp=numeric(0),
                   nexp.s=numeric(0),
                   nyrs=numeric(0),
                   n.R0=numeric(0),n.R1=numeric(0),n.R2=numeric(0),n.R3=numeric(0),	#number of jobs per reliability level
                   n.C0=numeric(0),n.C1=numeric(0),n.C2=numeric(0),n.C3=numeric(0),	#number of jobs per concentration level
                   matrix(0, ncol=(nlevsFreq+1), nrow=0),                            #number of jobs per frequency level
                   p.R0=numeric(0),p.R1=numeric(0),p.R2=numeric(0),n.R3=numeric(0),	#proportion of jobs per reliability level
                   p.C0=numeric(0),p.C1=numeric(0),p.C2=numeric(0),n.C3=numeric(0),	#proportion of jobs per concentration level
                   matrix(0, ncol=(nlevsFreq+1), nrow=0),                            #proportion of jobs per frequency level
                   Cmoy.1=numeric(0),Cmoy.3=numeric(0),Cmoy.5=numeric(0),Cmoy.10=numeric(0),  #mean concentration with scales 1-2-3,1-3-9,1-5-25,1-10-100
                   Dmoy.1=numeric(0),Dmoy.3=numeric(0),Dmoy.5=numeric(0),Dmoy.10=numeric(0),  #mean concentration with scales 1-2-3,1-3-9,1-5-25,1-10-100
                   Cmed.1=numeric(0),Cmed.3=numeric(0),Cmed.5=numeric(0),Cmed.10=numeric(0),
                   Dmed.1=numeric(0),Dmed.3=numeric(0),Dmed.5=numeric(0),Dmed.10=numeric(0),
                   Fmoy=numeric(0),Fmed=numeric(0),cell=character(0),stringsAsFactors=F)
  
  # Ajout des noms de variables pour fr?quence
  names(mat)[grep("^X", names(mat))][1:(nlevsFreq+1)] <- paste0("n.F", seq(0, nlevsFreq))
  names(mat)[grep("^X", names(mat))][1:(nlevsFreq+1)] <- paste0("p.F", seq(0, nlevsFreq))
  
  
  for (i in 1:n.dim) {mat<-cbind(mat,character(0))
  names(mat)[dim(mat)[2]]<-vec.dim[i]}				
  
  
  
  for (i in 1:length(period.vec)) {
    
    #selecting at first cells with Nmin jobs and Nmin.s subjects
    
    ##table of all cells with njobs et nsubjects
    
    restrict <-bd.prev.2[,match(period.vec[i],names(bd.prev.2))]!=0
    bla <-data.frame(table(bd.prev.2$cell.noperiod[restrict]),stringsAsFactors=F)
    names(bla) <-c('nom','n')
    bla$n.s <-tapply(bd.prev.2$ID[restrict],bd.prev.2$cell.noperiod[restrict],function(x){length(unique(x))})
    
    cell.list <-bla$nom[bla$n>=Nmin & bla$n.s>=Nmin.s] # Corrig? JUL 03 2015
    
    n.cell <-length(cell.list)
    
    #initializing the period specific JEM
    mat.temp <-data.frame(cell.noperiod=cell.list,period=character(n.cell),idchem=numeric(n.cell),p=numeric(n.cell),
                          ntot=numeric(n.cell),
                          nsub=numeric(n.cell),
                          nexp=numeric(n.cell),
                          nexp.s=numeric(n.cell),
                          nyrs=numeric(n.cell),
                          n.R0=numeric(n.cell),n.R1=numeric(n.cell),n.R2=numeric(n.cell),n.R3=numeric(n.cell),	#number of jobs per reliability level
                          n.C0=numeric(n.cell),n.C1=numeric(n.cell),n.C2=numeric(n.cell),n.C3=numeric(n.cell),	#number of jobs per concentration level
                          matrix(0, ncol=(nlevsFreq+1), nrow=n.cell),                            #number of jobs per frequency level
                          p.R0=numeric(n.cell),p.R1=numeric(n.cell),p.R2=numeric(n.cell),p.R3=numeric(n.cell),	#proportion of jobs per reliability level
                          p.C0=numeric(n.cell),p.C1=numeric(n.cell),p.C2=numeric(n.cell),p.C3=numeric(n.cell),	#proportion of jobs per concentration level
                          matrix(0, ncol=(nlevsFreq+1), nrow=n.cell),                            #proportion of jobs per frequency level
                          Cmoy.1=numeric(n.cell),Cmoy.3=numeric(n.cell),Cmoy.5=numeric(n.cell),Cmoy.10=numeric(n.cell),  #mean concentration with scales 1-2-3,1-3-9,1-5-25,1-10-100
                          Dmoy.1=numeric(n.cell),Dmoy.3=numeric(n.cell),Dmoy.5=numeric(n.cell),Dmoy.10=numeric(n.cell),  #mean concentration with scales 1-2-3,1-3-9,1-5-25,1-10-100
                          Cmed.1=numeric(n.cell),Cmed.3=numeric(n.cell),Cmed.5=numeric(n.cell),Cmed.10=numeric(n.cell),
                          Dmed.1=numeric(n.cell),Dmed.3=numeric(n.cell),Dmed.5=numeric(n.cell),Dmed.10=numeric(n.cell),
                          Fmoy=numeric(n.cell),Fmed=numeric(n.cell),stringsAsFactors=F)
    
    # Ajout des noms de variables pour fr?quence
    names(mat.temp)[grep("^X", names(mat.temp))][1:(nlevsFreq+1)] <- paste0("n.F", seq(0, nlevsFreq))
    names(mat.temp)[grep("^X", names(mat.temp))][1:(nlevsFreq+1)] <- paste0("p.F", seq(0, nlevsFreq))
    
    
    for (j in 1:n.dim) {mat.temp<-cbind(mat.temp,character(n.cell))
    names(mat.temp)[dim(mat.temp)[2]]<-vec.dim[j]}
    
    
    
    #filling the period, idchem and vec.dim identifiCATion variables
    
    mat.temp$period <-rep(period.vec[i],n.cell)
    
    restrict <-is.element(bd.prev.2$cell.noperiod,cell.list)
    
    temp.idchem <-tapply(bd.prev.2$IDCHEM[restrict],bd.prev.2$cell.noperiod[restrict],function(x){x[1]})
    temp.idchem <-data.frame(idchem=unname(temp.idchem),cell=names(temp.idchem),stringsAsFactors=F)
    mat.temp$idchem<-temp.idchem$idchem[match(mat.temp$cell.noperiod,temp.idchem$cell)]
    
    for (j in 1:n.dim) {
      
      temp.dim <-tapply(bd.prev.2[restrict,match(vec.dim[j],names(bd.prev.2))],bd.prev.2$cell.noperiod[restrict],function(x){x[1]})
      temp.dim <-data.frame(dim=unname(temp.dim),cell=names(temp.dim),stringsAsFactors=F)
      mat.temp[,match(vec.dim[j],names(mat.temp))]<-temp.dim$dim[match(mat.temp$cell.noperiod,temp.dim$cell)]
      
    }	
    
    ##filling the exposure metrics		
    
    restrict <-is.element(bd.prev.2$cell.noperiod,cell.list) & bd.prev.2[,match(period.vec[i],names(bd.prev.2))]!=0	
    
    #nombre de jobs dans bd.prev.2
    temp <-tapply(bd.prev.2$IDCHEM[restrict],bd.prev.2$cell.noperiod[restrict],length)
    temp <-data.frame(metric=unname(temp),cell=names(temp),stringsAsFactors=F)
    mat.temp$ntot<-temp$metric[match(mat.temp$cell.noperiod,temp$cell)]
    
    #nombre de sujets dans bd.prev.2
    temp <-tapply(bd.prev.2$ID[restrict],bd.prev.2$cell.noperiod[restrict],function(x) {length(unique(x))})
    temp <-data.frame(metric=unname(temp),cell=names(temp),stringsAsFactors=F)
    mat.temp$nsub<-temp$metric[match(mat.temp$cell.noperiod,temp$cell)]
    
    #nombre d'ann?es dans bd.prev.2
    temp <-tapply(bd.prev.2[restrict,match(period.vec[i],names(bd.prev.2))],bd.prev.2$cell[restrict],sum)
    temp <-data.frame(metric=unname(temp),cell=names(temp),stringsAsFactors=F)
    mat.temp$nyrs<-temp$metric[match(mat.temp$cell.noperiod,temp$cell)]
    
    #nombre de jobs expos?s
    temp <-tapply(bd.prev.2$expo.1[restrict],bd.prev.2$cell[restrict],sum)
    temp <-data.frame(metric=unname(temp),cell=names(temp),stringsAsFactors=F)
    mat.temp$nexp<-temp$metric[match(mat.temp$cell.noperiod,temp$cell)]
    
    #nombre de sujet expos?s
    temp <-tapply(bd.prev.2$ID[restrict & bd.prev.2$expo.1==1],bd.prev.2$cell[restrict & bd.prev.2$expo.1==1],function(x) {length(unique(x))})
    temp <-data.frame(metric=unname(temp),cell=names(temp),stringsAsFactors=F)
    mat.temp$nexp.s<-temp$metric[match(mat.temp$cell.noperiod,temp$cell)]
    
    #prevalence nombre de jobs
    
    mat.temp$p <-100*mat.temp$nexp/mat.temp$ntot
    
    
    ###exposure level metric - mean
    
    #C5
    temp <-tapply(bd.prev.2$C_FINAL[restrict],bd.prev.2$cell[restrict],mean,na.rm=T)
    temp <-data.frame(metric=unname(temp),cell=names(temp),stringsAsFactors=F)
    mat.temp$Cmoy.5<-temp$metric[match(mat.temp$cell.noperiod,temp$cell)]
    
    #C1
    temp <-tapply(1+log(bd.prev.2$C_FINAL[restrict])/log(5),bd.prev.2$cell[restrict],mean,na.rm=T)
    temp <-data.frame(metric=unname(temp),cell=names(temp),stringsAsFactors=F)
    mat.temp$Cmoy.1<-temp$metric[match(mat.temp$cell.noperiod,temp$cell)]
    
    #C3
    temp <-tapply(3^(log(bd.prev.2$C_FINAL[restrict])/log(5)),bd.prev.2$cell[restrict],mean,na.rm=T)
    temp <-data.frame(metric=unname(temp),cell=names(temp),stringsAsFactors=F)
    mat.temp$Cmoy.3<-temp$metric[match(mat.temp$cell.noperiod,temp$cell)]
    
    #C10
    temp <-tapply(10^(log(bd.prev.2$C_FINAL[restrict])/log(5)),bd.prev.2$cell[restrict],mean,na.rm=T)
    temp <-data.frame(metric=unname(temp),cell=names(temp),stringsAsFactors=F)
    mat.temp$Cmoy.10<-temp$metric[match(mat.temp$cell.noperiod,temp$cell)]
    
    ###exposure level metric - median
    
    #C5
    temp <-tapply(bd.prev.2$C_FINAL[restrict],bd.prev.2$cell[restrict],median,na.rm=T)
    temp <-data.frame(metric=unname(temp),cell=names(temp),stringsAsFactors=F)
    mat.temp$Cmed.5<-temp$metric[match(mat.temp$cell.noperiod,temp$cell)]
    
    #C1
    temp <-tapply(1+log(bd.prev.2$C_FINAL[restrict])/log(5),bd.prev.2$cell[restrict],median,na.rm=T)
    temp <-data.frame(metric=unname(temp),cell=names(temp),stringsAsFactors=F)
    mat.temp$Cmed.1<-temp$metric[match(mat.temp$cell.noperiod,temp$cell)]
    
    #C3
    temp <-tapply(3^(log(bd.prev.2$C_FINAL[restrict])/log(5)),bd.prev.2$cell[restrict],median,na.rm=T)
    temp <-data.frame(metric=unname(temp),cell=names(temp),stringsAsFactors=F)
    mat.temp$Cmed.3<-temp$metric[match(mat.temp$cell.noperiod,temp$cell)]
    
    #C10
    temp <-tapply(10^(log(bd.prev.2$C_FINAL[restrict])/log(5)),bd.prev.2$cell[restrict],median,na.rm=T)
    temp <-data.frame(metric=unname(temp),cell=names(temp),stringsAsFactors=F)
    mat.temp$Cmed.10<-temp$metric[match(mat.temp$cell.noperiod,temp$cell)]
    
    ##exposure dose - mean
    
    #D5
    temp <-tapply(bd.prev.2$C_FINAL[restrict]*bd.prev.2$F_FINAL[restrict]/40,bd.prev.2$cell[restrict],mean,na.rm=T)
    temp <-data.frame(metric=unname(temp),cell=names(temp),stringsAsFactors=F)
    mat.temp$Dmoy.5<-temp$metric[match(mat.temp$cell.noperiod,temp$cell)]
    
    #D1
    temp <-tapply((1+log(bd.prev.2$C_FINAL[restrict])/log(5))*bd.prev.2$F_FINAL[restrict]/40,bd.prev.2$cell[restrict],mean,na.rm=T)
    temp <-data.frame(metric=unname(temp),cell=names(temp),stringsAsFactors=F)
    mat.temp$Dmoy.1<-temp$metric[match(mat.temp$cell.noperiod,temp$cell)]
    
    #D3
    temp <-tapply((3^(log(bd.prev.2$C_FINAL[restrict])/log(5)))*bd.prev.2$F_FINAL[restrict]/40,bd.prev.2$cell[restrict],mean,na.rm=T)
    temp <-data.frame(metric=unname(temp),cell=names(temp),stringsAsFactors=F)
    mat.temp$Dmoy.3<-temp$metric[match(mat.temp$cell.noperiod,temp$cell)]
    
    #D10
    temp <-tapply((10^(log(bd.prev.2$C_FINAL[restrict])/log(5)))*bd.prev.2$F_FINAL[restrict]/40,bd.prev.2$cell[restrict],mean,na.rm=T)
    temp <-data.frame(metric=unname(temp),cell=names(temp),stringsAsFactors=F)
    mat.temp$Dmoy.10<-temp$metric[match(mat.temp$cell.noperiod,temp$cell)]
    
    ##exposure dose - meddian
    
    #D5
    temp <-tapply(bd.prev.2$C_FINAL[restrict]*bd.prev.2$F_FINAL[restrict]/40,bd.prev.2$cell[restrict],median,na.rm=T)
    temp <-data.frame(metric=unname(temp),cell=names(temp),stringsAsFactors=F)
    mat.temp$Dmed.5<-temp$metric[match(mat.temp$cell.noperiod,temp$cell)]
    
    #D1
    temp <-tapply((1+log(bd.prev.2$C_FINAL[restrict])/log(5))*bd.prev.2$F_FINAL[restrict]/40,bd.prev.2$cell[restrict],median,na.rm=T)
    temp <-data.frame(metric=unname(temp),cell=names(temp),stringsAsFactors=F)
    mat.temp$Dmed.1<-temp$metric[match(mat.temp$cell.noperiod,temp$cell)]
    
    #D3
    temp <-tapply((3^(log(bd.prev.2$C_FINAL[restrict])/log(5)))*bd.prev.2$F_FINAL[restrict]/40,bd.prev.2$cell[restrict],median,na.rm=T)
    temp <-data.frame(metric=unname(temp),cell=names(temp),stringsAsFactors=F)
    mat.temp$Dmed.3<-temp$metric[match(mat.temp$cell.noperiod,temp$cell)]
    
    #D10
    temp <-tapply((10^(log(bd.prev.2$C_FINAL[restrict])/log(5)))*bd.prev.2$F_FINAL[restrict]/40,bd.prev.2$cell[restrict],median,na.rm=T)
    temp <-data.frame(metric=unname(temp),cell=names(temp),stringsAsFactors=F)
    mat.temp$Dmed.10<-temp$metric[match(mat.temp$cell.noperiod,temp$cell)]
    
    #mean frequency
    
    temp <-tapply(bd.prev.2$F_FINAL[restrict],bd.prev.2$cell[restrict],mean,na.rm=T)
    temp <-data.frame(metric=unname(temp),cell=names(temp),stringsAsFactors=F)
    mat.temp$Fmoy<-temp$metric[match(mat.temp$cell.noperiod,temp$cell)]
    
    #median frequency
    
    temp <-tapply(bd.prev.2$F_FINAL[restrict],bd.prev.2$cell[restrict],median,na.rm=T)
    temp <-data.frame(metric=unname(temp),cell=names(temp),stringsAsFactors=F)
    mat.temp$Fmed<-temp$metric[match(mat.temp$cell.noperiod,temp$cell)]
    
    ########frequency of exposure CATegories
    
    #concentration
    
    temp <-tapply(bd.prev.2$C_FINAL_CAT[restrict],bd.prev.2$cell[restrict],function(x){length(x[is.na(x)])})
    temp <-data.frame(metric=unname(temp),cell=names(temp),stringsAsFactors=F)
    mat.temp$n.C0<-temp$metric[match(mat.temp$cell.noperiod,temp$cell)]
    mat.temp$p.C0<-100*mat.temp$n.C0/mat.temp$ntot
    
    temp <-tapply(bd.prev.2$C_FINAL_CAT[restrict],bd.prev.2$cell[restrict],function(x){length(x[!is.na(x)& x==1])})
    temp <-data.frame(metric=unname(temp),cell=names(temp),stringsAsFactors=F)
    mat.temp$n.C1<-temp$metric[match(mat.temp$cell.noperiod,temp$cell)]
    mat.temp$p.C1<-100*mat.temp$n.C1/mat.temp$ntot
    
    temp <-tapply(bd.prev.2$C_FINAL_CAT[restrict],bd.prev.2$cell[restrict],function(x){length(x[!is.na(x)& x==2])})
    temp <-data.frame(metric=unname(temp),cell=names(temp),stringsAsFactors=F)
    mat.temp$n.C2<-temp$metric[match(mat.temp$cell.noperiod,temp$cell)]
    mat.temp$p.C2<-100*mat.temp$n.C2/mat.temp$ntot
    
    temp <-tapply(bd.prev.2$C_FINAL_CAT[restrict],bd.prev.2$cell[restrict],function(x){length(x[!is.na(x)& x==3])})
    temp <-data.frame(metric=unname(temp),cell=names(temp),stringsAsFactors=F)
    mat.temp$n.C3<-temp$metric[match(mat.temp$cell.noperiod,temp$cell)]
    mat.temp$p.C3<-100*mat.temp$n.C3/mat.temp$ntot
    
    #frequency
    # - number of jobs in each rating per cell
    temp <- do.call("rbind", tapply(bd.prev.2$F_FINAL_CAT[restrict],bd.prev.2$cell[restrict],function(x){
      tbl <- table(x, useNA="always")
      names(tbl)[is.na(names(tbl))] <- "0"
      tbl <- tbl[order(names(tbl))] # reorder to have 0 first
      tbl }))
    # - add into "mat.temp"
    mat.temp[,grep("^n.F[0-9]", names(mat.temp))] <- temp[match(mat.temp$cell.noperiod,rownames(temp)),]
    # proportions 
    temp.p <- t(apply(temp, 1, function(x) 100*x/(sum(x))))
    mat.temp[,grep("^p.F[0-9]", names(mat.temp))] <-temp.p[match(mat.temp$cell.noperiod,rownames(temp.p)),]
    
    
    
    #fiability
    
    temp <-tapply(bd.prev.2$R_FINAL[restrict],bd.prev.2$cell[restrict],function(x){length(x[is.na(x)])})
    temp <-data.frame(metric=unname(temp),cell=names(temp),stringsAsFactors=F)
    mat.temp$n.R0<-temp$metric[match(mat.temp$cell.noperiod,temp$cell)]
    mat.temp$p.R0<-100*mat.temp$n.R0/mat.temp$ntot
    
    temp <-tapply(bd.prev.2$R_FINAL[restrict],bd.prev.2$cell[restrict],function(x){length(x[!is.na(x)& x==1])})
    temp <-data.frame(metric=unname(temp),cell=names(temp),stringsAsFactors=F)
    mat.temp$n.R1<-temp$metric[match(mat.temp$cell.noperiod,temp$cell)]
    mat.temp$p.R1<-100*mat.temp$n.R1/mat.temp$ntot
    
    temp <-tapply(bd.prev.2$R_FINAL[restrict],bd.prev.2$cell[restrict],function(x){length(x[!is.na(x)& x==2])})
    temp <-data.frame(metric=unname(temp),cell=names(temp),stringsAsFactors=F)
    mat.temp$n.R2<-temp$metric[match(mat.temp$cell.noperiod,temp$cell)]
    mat.temp$p.R2<-100*mat.temp$n.R2/mat.temp$ntot
    
    temp <-tapply(bd.prev.2$R_FINAL[restrict],bd.prev.2$cell[restrict],function(x){length(x[!is.na(x)& x==3])})
    temp <-data.frame(metric=unname(temp),cell=names(temp),stringsAsFactors=F)
    mat.temp$n.R3<-temp$metric[match(mat.temp$cell.noperiod,temp$cell)]
    mat.temp$p.R3<-100*mat.temp$n.R3/mat.temp$ntot
    
    #adding the FINAL cell variable
    
    mat.temp$cell <-paste(mat.temp$cell.noperiod,mat.temp$period,sep='-')
    
    #joining with mat
    
    mat <-rbind(mat,mat.temp)
    
  }
  
  
  return(mat)
  
}	



