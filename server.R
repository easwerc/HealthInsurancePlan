shinyServer(function(input, output) {
  
  aYearPremium <- reactive({  
    if(input$aContrib==1) 
      return(input$aPremium * 26)
    return(input$aPremium * 12)
  })
  
  bYearPremium <- reactive({  
    if(input$bContrib==1) 
      return(input$bPremium * 26)
    return(input$bPremium * 12)
  })
  
  output$aYearPremium <- renderText({ 
    paste("$",  aYearPremium())
  })
  
  output$bYearPremium <- renderText({ 
    paste("$",  bYearPremium())
  })
  
  output$aVisitExpenses <- renderText({ 
    paste("$", input$nClinicVisit * input$nClinicCost)
  })
  
  output$bVisitExpenses <- renderText({ 
    paste("$", input$nClinicVisit * input$nClinicCost)
  })
  
  aYouPay <- reactive({
    if(input$nClinicVisit * input$nClinicCost < input$aDeduct) 
      return(input$nClinicVisit * 30) # Pay avg $30 copay per visit
    
    if(input$nClinicVisit * input$nClinicCost > input$aOOPMax) 
      return(input$aOOPMax)
    
    input$aDeduct + ((input$nClinicVisit * input$nClinicCost)-input$aDeduct) * 0.2
  })
  
  bYouPay <- reactive({
    if(input$nClinicVisit * input$nClinicCost < input$bDeduct) 
      return(input$nClinicVisit * input$nClinicCost) # Pay until deductible is met
    
    if(input$nClinicVisit * input$nClinicCost > input$bOOPMax) 
      return(input$bOOPMax)
    
    input$bDeduct + ((input$nClinicVisit * input$nClinicCost)-input$bDeduct) * 0.2
  })
  
  output$aYouPay <- renderText({ 
    paste("$", aYouPay())
  })
  
  output$bYouPay <- renderText({ 
    paste("$", bYouPay())
  })
  
  output$aAllExpenses <- renderText({ 
    paste("$", aYearPremium() + aYouPay())
  })
  
  output$bAllExpenses <- renderText({ 
    paste("$", bYearPremium() + bYouPay())
  })
  
  output$conclusion <- renderText({ 
    if((aYearPremium()+aYouPay()) > (bYearPremium()+bYouPay())) 
       return(paste("Conclusion: Plan B would give a cost savings of $", 
                    aYearPremium()+aYouPay()-bYearPremium()-bYouPay(),
                    " over Plan A"))
    paste("Conclusion: Plan A would give a cost savings of $", 
                    bYearPremium()+bYouPay()-aYearPremium()-aYouPay(),
                    " over Plan B")
  })
  
  getClaimsData <- reactive({  
    read.csv("data/ClaimsData.csv")
  })
  
  getPredictedVisit <- reactive({
    input$goButton
    claimsData <- getClaimsData()
    fitVisit <- lm(Visits ~ EE_Type+Age_EE0+Age_EE1+Age_EE2+Age_EE3+Hlevel, data=claimsData)
    
    EE_Type <- factor(x=isolate(input$insWho), c(1,2,3,4),labels=c("EE0","EE1","EE2","EE3"))
    newdata <- data.frame(EE_Type=EE_Type, 
                          Age_EE0=isolate(input$dep0Age), 
                          Age_EE1=isolate(input$dep1Age), 
                          Age_EE2=isolate(input$dep2Age), 
                          Age_EE3=isolate(input$dep3Age), 
                          Hlevel=integer(isolate(input$hlevel)))
    abs(round(predict(fitVisit, newdata)[1],0))
    
  })
  
  getPredictedAvgCost <- reactive({
    input$goButton
    claimsData <- getClaimsData()
    fitCost <- lm(TotalCost ~ EE_Type+Age_EE0+Age_EE1+Age_EE2+Age_EE3+Hlevel, data=claimsData)
    
    EE_Type <- factor(x=isolate(input$insWho), c(1,2,3,4),labels=c("EE0","EE1","EE2","EE3"))
    newdata <- data.frame(EE_Type=EE_Type, 
                          Age_EE0=isolate(input$dep0Age), 
                          Age_EE1=isolate(input$dep1Age), 
                          Age_EE2=isolate(input$dep2Age), 
                          Age_EE3=isolate(input$dep3Age), 
                          Hlevel=as.integer(isolate(input$hlevel)))
    avgCost <- abs(round(predict(fitCost, newdata)[1], 2))
    
    if(getPredictedVisit()>0) 
      return(avgCost/getPredictedVisit())
    return(avgCost)
  })
  
  output$plot1 <- renderPlot({
    input$goButton
    
    claimsData <- getClaimsData()
    par(mar = c(5.1, 4.1, 0, 1))
    plot(claimsData$EE_Type, claimsData$TotalCost/claimsData$Visits,
         xlab="Who is insured", ylab="Avg cost per visit",
         pch = 20, cex = 3)
    
    points(isolate(input$insWho), as.integer(getPredictedAvgCost()), 
           pch = 4, cex = 4, lwd = 4)
    # Use isolate() to avoid dependency on input$*
    
  })
  
  output$predictText <- renderText({
    # Take a dependency on input$goButton
    input$goButton
    
    if(getPredictedVisit()==0) 
      paste("Not enough data to predict for the given values")
    else
      paste("The model has predicted ", getPredictedVisit(), 
          "visits with a cost of $", round(getPredictedAvgCost(),2), 
          "per visit")
  })
})

