# rsamreen_peer
library(datasets)
library(shiny)
##
mpgData <- mtcars
mpgData$am <- factor(mpgData$am, labels = c("utomatic", "Manuual"))
##
shinyServer(function(ipt, oupt) {
  
  formulaText <- reactive({
    paste("mpg ~", ipt$variable)
  })  
  formulaTextPoint <- reactive({
    paste("mpg ~", "as.integer(", ipt$variable, ")")
  })  
  ##
  fit <- reactive({
    lm(as.formula(formulaTextPoint()), data=mpgData)
  })
  ##
  
  oupt$caption <- renderText({
    formulaText()
  })
  ##
  oupt$mpgBoxPlot <- renderPlot({
    boxplot(as.formula(formulaText()), 
            data = mpgData,
            outline = ipt$outliers)
  })
  ##
  oupt$fit <- renderPrint({
    summary(fit())
  })  
  oupt$mpgPlot <- renderPlot({
    with(mpgData, {
      plot(as.formula(formulaTextPoint()))
      abline(fit(), col=2)
    })
  })
  
})
