library(IsolationForest)
library(rworldmap)
library(shiny)

setwd("C:/Users/zhaoch/Desktop/Project/R Shiny")

# Load Tickets data (ticket.data)
# load("tickets.RData")
# attach(ticket.data)


shinyServer(function(input, output, session) {
  
  # Load Messages data
  Messages<-read.csv(file="Messages.csv")
  numMsg<-dim(Messages)
  
  # Document the various problem location scenarios
  LocS<-c("India","Australia","China","Brazil","Argentina","Mexico","Germany","France","Italy")
  CitS<-c("New Delhi","Sydney","Beijing","Rio","Santa Fe","Mexico City","Berlin","Paris","Rome")
  
  # Create the Novelty Scoring Models
  ifSKYPE<-IsolationTrees(as.data.frame(SKYPE_TO),rFactor=0)
  ifOUTL<-IsolationTrees(as.data.frame(OUTL_TO),rFactor=0)
  ifEOL<-IsolationTrees(as.data.frame(EOL_TO),rFactor=0)
  
  # Create a reactiveValues object
  vals <- reactiveValues(counter = 1,
                         
                         SPR = 2, OPR = 2, EPR = 3,
                         
                         SKYPE_SCORE = AnomalyScore(t(2),ifSKYPE)$outF,
                         OUTL_SCORE = AnomalyScore(t(1),ifOUTL)$outF,
                         EOL_SCORE = AnomalyScore(t(3),ifEOL)$outF,
                         
                         skype_labels = "", outlook_labels = "", eol_labels = "",
                         
                         skype_message = "", outlook_message = "", eol_message = "",
                         
                         skype_row = "", outlook_row = "", eol_row = "",
                         
                         city_skype = "", city_outlook = "", city_eol = ""
  )
  
  # Action Button to Start the Demo
  StartDemo <- eventReactive(input$go,{
    Threshold.num <- as.numeric(input$Threshold)
  })
  
  observeEvent(input$go, {
    
    # The number of iterations to perform
    maxIter <- 50
    
    # Update the percentage complete
    output$percentage <- renderText({
      paste0(round((vals$counter - 1)/maxIter * 100, 1), "%")
    })
    
    # # Track the start and elapsed time
    # startTime <- Sys.time()
    # output$elapsed <- renderText({
    #   vals$counter
    #   round(difftime(Sys.time(), startTime, units = "secs"))
    # })
    
  observe({
    
    isolate({
      
      i <- vals$counter
      
      date <- as.Date("2016-01-01")
      
      #Plot parameters
      xl<-NULL
      xlower<-1
      xhigher<-i
      
      if (i>9){
        xlower<-i-9
        xhigher<-i
      }
      
      # Plot the Skype Problem Chart.
      output$SkypePlot <- renderPlot({

        # par(mfrow=c(1, 2))
        
        barplot(vals$SPR[xlower:xhigher],main="SKYPE Ticket Rate Monitor",ylab="Ticket Rate")
        grid()
        a
        # bcols<-c("grey","red")[(vals$SKYPE_SCORE[xlower:xhigher]>StartDemo())+1]
        # barplot(vals$SKYPE_SCORE[xlower:xhigher],main="SKYPE Monitor",ylab="Novelty Score",ylim=(0:1),
        #         xlab=xl,col=bcols,names.arg=paste(vals$skype_labels[xlower:xhigher]))
        # abline(StartDemo(),0,col="red")
        # grid()
      })
      
      # Plot the Outlook Problem Chart.
      output$OutlookPlot <- renderPlot({
        
        # par(mfrow=c(1, 2))
        
        barplot(vals$OPR[xlower:xhigher],main="OUTLOOK Ticket Rate Monitor",ylab="Ticket Rate")
        grid()
        
        # bcols<-c("grey","red")[(vals$OUTL_SCORE[xlower:xhigher]>StartDemo())+1]
        # barplot(vals$OUTL_SCORE[xlower:xhigher],main="OUTLOOK Monitor",ylab="Novelty Score",ylim=(0:1),
        #         xlab=yl,col=bcols,names.arg=paste(vals$outlook_labels[xlower:xhigher]))
        # abline(StartDemo(),0,col="red")
        # grid()
      })
      
      # Plot the EOL Problem Chart.
      output$EOLPlot <- renderPlot({
        
        # par(mfrow=c(1, 2))
        
        barplot(vals$EPR[xlower:xhigher],main="EOL Ticket Rate Monitor",ylab="Ticket Rate")
        grid()
        
        # bcols<-c("grey","red")[(vals$EOL_SCORE[xlower:xhigher]>StartDemo())+1]
        # barplot(vals$EOL_SCORE[xlower:xhigher],main="EOL ACCESS Monitor",ylab="Novelty Score",ylim=(0:1),
        #         xlab=zl,col=bcols,names.arg=paste(vals$eol_labels[xlower:xhigher]))
        # abline(StartDemo(),0,col="red")
        # grid()
      })
      
      # Create Text Messages.
      output$SkypeText <- renderTable({
        
        vals$skype_message
        
      },
      include.rownames=FALSE
      )
      
      output$OutlookText <- renderTable({
        
        vals$outlook_message
        
      },
      include.rownames=FALSE
      )
      
      output$EOLText <- renderTable({
        
        vals$eol_message
        
      },
      include.rownames=FALSE
      )
      
      
      # Increment the reactivevalues
      rs<-round(runif(3,1,9000))
      vals$SPR <- c(vals$SPR,sum(SKYPE_IN[rs[1]],SKYPE_MA[rs[2]],SKYPE_CH[rs[3]]))
      vals$SKYPE_SCORE <- c(vals$SKYPE_SCORE,AnomalyScore(t(vals$SPR[i]),ifSKYPE)$outF)
      
      rs<-round(runif(3,1,9000))
      vals$OPR <- c(vals$OPR,sum(OUTL_IN[rs[1]],OUTL_MA[rs[2]],OUTL_CH[rs[3]]))
      vals$OUTL_SCORE <- c(vals$OUTL_SCORE,AnomalyScore(t(vals$OPR[i]),ifOUTL)$outF)
      
      rs<-round(runif(3,1,9000))
      vals$EPR <- c(vals$EPR,sum(EOL_IN[rs[1]],EOL_MA[rs[2]],EOL_CH[rs[3]]))
      vals$EOL_SCORE <- c(vals$EOL_SCORE,AnomalyScore(t(vals$EPR[i]),ifEOL)$outF)
      
      #cat(vals$SKYPE_SCORE[i],vals$OUTL_SCORE[i],vals$EOL_SCORE[i],'\n')
      
      # Increment the Skype labels
      if (i < 2) {
        if (vals$SKYPE_SCORE[i]>input$Threshold) {
          n <- floor(runif(1,1,3.9))
          token <- LocS[n]
          vals$city_skype <- paste(paste(CitS[n], "Data Center", sep=" "), date+i, sep=" ; ")
          
          skype_lc <- token
          xl<-paste(token,"Subsystem: SKYPE")
          vals$skype_labels <- token
          
          row <- round(runif(1,1,numMsg[1]))
          
          vals$skype_row <- row
          vals$skype_message <- Messages[c(vals$skype_row),1:(numMsg[2]-2)]
          vals$skype_message$"Locs & Time" <- vals$city_skype
          skype_col <- as.character(Messages[row,numMsg[2]])
          
          alert1 <- Messages[row,numMsg[2]-1]
        } else {
          vals$city_skype <- CitS[0]
          skype_lc <- ""
          vals$skype_labels <- ""
          vals$skype_row <- 0
          vals$skype_message <- Messages[c(vals$skype_row),1:(numMsg[2]-2)]
          vals$skype_message$"Locs & Time" <- vals$city_skype
          skype_col <-""
          alert1 <- "transparent"
        }
        
      } else {
        if (vals$SKYPE_SCORE[i]>input$Threshold) {
          n <- floor(runif(1,1,3.9))
          token <- LocS[n]
          
          vals$city_skype <- c(paste(paste(CitS[n],"Data Center",sep=" "),date+i,sep=" ; "),vals$city_skype)
          skype_lc <- token
          xl<-paste(token,"Subsystem: SKYPE")
          vals$skype_labels <- c(vals$skype_labels, token)
          
          row <- round(runif(1,1,numMsg[1]))
          
          vals$skype_row <- c(row,vals$skype_row)
          vals$skype_message <- Messages[c(vals$skype_row),1:(numMsg[2]-2)]
          vals$skype_message$"Locs & Time" <- vals$city_skype
          skype_col <- as.character(Messages[row,numMsg[2]])
          
          alert1 <- Messages[row,numMsg[2]-1]
        } else {
          vals$city_skype <- vals$city_skype
          skype_lc <- ""
          vals$skype_labels <- c(vals$skype_labels, "")
          vals$skype_row <- vals$skype_row
          vals$skype_message <- Messages[c(vals$skype_row),1:(numMsg[2]-2)]
          vals$skype_message$"Locs & Time" <- vals$city_skype
          skype_col <-""
          alert1 <- "transparent"
        }
      }
      
      # Increment the Outlook labels
      if (i < 2) {
        if (vals$OUTL_SCORE[i]>input$Threshold) {
          n <- floor(runif(1,4,6.9))
          token <- LocS[n]
          vals$city_outlook <- paste(paste(CitS[n],"Data Center",sep=" "), date+i, sep=" ; ")
          
          outlook_lc <- token
          yl<-paste(token,"Subsystem: OUTLOOK")
          vals$outlook_labels <- token
          
          row <- round(runif(1,1,numMsg[1]))
          
          vals$outlook_row <- row
          vals$outlook_message <- Messages[c(vals$outlook_row),1:(numMsg[2]-2)]
          vals$outlook_message$"Locs & Time" <- vals$city_outlook
          outlook_col <- as.character(Messages[row,numMsg[2]])
          
          alert2 <- Messages[row,numMsg[2]-1]
        } else {
          vals$city_outlook <- CitS[0]
          outlook_lc <- ""
          vals$outlook_labels <- ""
          vals$outlook_row <- 0
          vals$outlook_message <- Messages[c(vals$outlook_row),1:(numMsg[2]-2)]
          vals$outlook_message$"Locs & Time" <- vals$city_outlook
          outlook_col <-""
          alert2 <- "transparent"
        }
        
      } else {
        if (vals$OUTL_SCORE[i]>input$Threshold) {
          n <- floor(runif(1,4,6.9))
          token <- LocS[n]
          vals$city_outlook <- c(paste(paste(CitS[n],"Data Center",sep=" "),
                                       date+i,sep=" ; "),vals$city_outlook)
          
          outlook_lc <- token
          yl<-paste(token,"Subsystem: OUTLOOK")
          vals$outlook_labels <- c(vals$outlook_labels, token)
          
          row <- round(runif(1,1,numMsg[1]))
          
          vals$outlook_row <- c(row,vals$outlook_row)
          vals$outlook_message <- Messages[c(vals$outlook_row),1:(numMsg[2]-2)]
          vals$outlook_message$"Locs & Time" <- vals$city_outlook
          outlook_col <- as.character(Messages[row,numMsg[2]])
          
          alert2 <- Messages[row,numMsg[2]-1]
        } else {
          vals$city_outlook <- vals$city_outlook
          outlook_lc <- ""
          vals$outlook_labels <- c(vals$outlook_labels, "")
          vals$outlook_row <- vals$outlook_row
          vals$outlook_message <- Messages[c(vals$outlook_row),1:(numMsg[2]-2)]
          vals$outlook_message$"Locs & Time" <- vals$city_outlook
          outlook_col <-""
          alert2 <- "transparent"
        }
      }
      
      # Increment the EOL labels
      if (i < 2) {
        if (vals$EOL_SCORE[i]>input$Threshold) {
          n <- floor(runif(1,7,9.9))
          token <- LocS[n]
          vals$city_eol <- paste(paste(CitS[n],"Data Center",sep=" "), date+i, sep=" ; ")
          
          eol_lc <- token
          zl<-paste(token,"Subsystem: Expense Online")
          vals$eol_labels <- token
          
          row <- round(runif(1,1,numMsg[1]))
          
          vals$eol_row <- row
          vals$eol_message <- Messages[c(vals$eol_row),1:(numMsg[2]-2)]
          vals$eol_message$"Locs & Time" <- vals$city_eol
          eol_col <- as.character(Messages[row,numMsg[2]])
          
          alert3 <- Messages[row,numMsg[2]-1]
        } else {
          vals$city_eol <- CitS[0]
          eol_lc <- ""
          vals$eol_labels <-""
          vals$eol_row <- 0
          vals$eol_message <- Messages[c(vals$eol_row),1:(numMsg[2]-2)]
          vals$eol_message$"Locs & Time" <- vals$city_eol
          eol_col <- ""
          alert3 <- "transparent"
        }
        
      } else {
        if (vals$EOL_SCORE[i]>input$Threshold) {
          n <- floor(runif(1,7,9.9))
          token <- LocS[n]
          vals$city_eol <- c(paste(paste(CitS[n],"Data Center",sep=" "), date+i, sep=" ; "),vals$city_eol)
          
          eol_lc <- token
          zl<-paste(token,"Subsystem: Expense Online")
          vals$eol_labels <- c(vals$eol_labels, token)
          
          row <- round(runif(1,1,numMsg[1]))
          
          vals$eol_row <- c(row,vals$eol_row)
          vals$eol_message <- Messages[c(vals$eol_row),1:(numMsg[2]-2)]
          vals$eol_message$"Locs & Time" <- vals$city_eol
          eol_col <- as.character(Messages[row,numMsg[2]])
          
          alert3 <- Messages[row,numMsg[2]-1]
        } else {
          vals$city_eol <- vals$city_eol
          eol_lc <- ""
          vals$eol_labels <- c(vals$eol_labels, "")
          vals$eol_row <- vals$eol_row
          vals$eol_message <- Messages[c(vals$eol_row),1:(numMsg[2]-2)]
          vals$eol_message$"Locs & Time" <- vals$city_eol
          eol_col <- ""
          alert3 <- "transparent"
        }
      }
      
        
      # World Map Parameters
      location <- c(skype_lc, outlook_lc, eol_lc)
      location <- location[location!=""]
      
      color <- c(skype_col, outlook_col, eol_col)
      color <- color[color!=""]
      
      
      # Draw World Map using rworldmap package based on dynamic paramters
      if (length(color) == 0) {
        
        color <- ""
        location <- ""
        categ <- ""
        
        
        malDF <- data.frame(country = location, Customer.Incident.Map = categ)
        
        capture.output(
          malMap <- joinCountryData2Map(malDF, joinCode = "NAME", nameJoinColumn = "country")
        , file='NUL')
                
        output$map1<- renderPlot({
          
          mapParams <- 
            mapCountryData(malMap, nameColumnToPlot="Customer.Incident.Map", catMethod="categorical", 
                           missingCountryCol=gray(0.8), colourPalette="heat", addLegend='FALSE',
                           mapTitle = "Global incident map for clients: RDS, BMSQ, MONDELEZ")
          
        legend("left", title="Current Status", leg="No Anomaly Detected", col="green", horiz=FALSE, pch=19)
        })
        
      } else if (length(unique(color)) == 1) {
        
        categ <- color
        
        for (ii in 1:length(color)) {
          switch(color[ii], 
                 red={
                   categ[ii] <- "Likely"
                 },
                 yellow={
                   categ[ii] <- "Not Likely" 
                 }
          )}
        
        color <- unique(color)
        
        malDF <- data.frame(country = c(location,"Canada"), Customer.Incident.Map = c(categ,"ZZZ"))
        legend.categ <- unique(categ)
        
        capture.output(
          malMap <- joinCountryData2Map(malDF, joinCode = "NAME",nameJoinColumn = "country")
          , file='NUL')
        
        output$map1<- renderPlot({
          
          mapParams <- 
            mapCountryData(malMap, nameColumnToPlot="Customer.Incident.Map", catMethod="categorical", 
                           missingCountryCol=gray(0.8), colourPalette=c(color,"#CCCCCC"), addLegend='FALSE',
                           mapTitle = "Global incident map for clients: RDS, BMSQ, MONDELEZ")
          
          legend("left", title="Current Status", leg=c(legend.categ,"No Anomaly Detected"),
                 fill=c(color,"gray"), horiz=FALSE)
        })
        
      } else {
        
        categ <- color
        
        for (ii in 1:length(color)) {
          switch(color[ii], 
                 red={
                   categ[ii] <- "Likely"
                 },
                 yellow={
                   categ[ii] <- "Not Likely" 
                 }
          )}
        
        color <- unique(color)
        
        malDF <- data.frame(country = location, Customer.Incident.Map = categ)
        legend.categ <- unique(categ)
        
        capture.output(
          malMap <- joinCountryData2Map(malDF, joinCode = "NAME",nameJoinColumn = "country")
        , file='NUL')
        
        output$map1<- renderPlot({
            
          mapParams <- 
            mapCountryData(malMap, nameColumnToPlot="Customer.Incident.Map", catMethod="categorical", 
                           missingCountryCol=gray(0.8), colourPalette=color, addLegend='FALSE',
                           mapTitle = "Global incident map for clients: RDS, BMSQ, MONDELEZ")
          
          legend("left", title="Current Status", leg=c(legend.categ,"No Anomaly Detected"), 
                 fill=c(color,"gray"), horiz=FALSE)
          })
      }
      
      
      # Create alert picture messages when Anomaly detected
      if (alert1 != "transparent") {
        alertpic <- alert1
        
      } else if (alert2 != "transparent") {
        alertpic <- alert2
        
      } else if (alert3 != "transparent") {
        alertpic <- alert3
        
      } else alertpic <- "transparent"
      
      
      output$alert<- renderImage({
          {
            return(list(
              src = paste("www/", paste(alertpic,".png",sep=""),sep=""),
              contentType = "image/png",
              width = 400,
              height = 200,
              alt = "Face"
            ))
          }
        }, deleteFile = FALSE)
        
      
      # Increment the counter
      vals$counter <- vals$counter + 1
    })
    
      # # Add logic for Resume and Restart Button
      # if(input$iStop== input$go){
      #   maxIter <- 0
      # }
      # 
      # if(input$iResume == input$iStop){
      #   maxIter <- 500
      # }
      # 
      # if(input$iStop > input$iResume){
      #   maxIter <- 0
      # }
      
      # If we're not done yet, then schedule this block to execute again in 3 seconds.
      if (isolate(vals$counter) < maxIter + 1){

        if (alertpic != "transparent") {
          invalidateLater(6000, session)

        } else {
          invalidateLater(3000, session)
        }
      }
    
   })
  })
})
