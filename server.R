pts <- seq(from = -5, to = 5, length = 700)

shinyServer(function(input, output, session) {

  output$plot1 <- renderPlot({
    
    ## Evaluate probabilities and plot
    mu <- input$mu
    
    # vector of outcome values to plot
    jvec <- 0:(4*mu + 2)
    
    poissonprobs <- dpois(jvec, mu, log = FALSE)
    
    barplot(poissonprobs, names.arg = jvec, ylab = "Poisson Pr(Y=j|mu)", xlab = "j", main = "Poisson PMF, no explanatory variables")
    
    
    # probs1 <- exp(alpha_1 - b_1* pts) / (1 + exp(alpha_1 - b_1* pts))
    # probs2 <- exp(alpha_2 - b_1* pts) / (1 + exp(alpha_2 - b_1* pts)) - exp(alpha_1 - b_1* pts) / (1 + exp(alpha_1 - b_1* pts))
    # probs3 <- exp(alpha_3 - b_1* pts) / (1 + exp(alpha_3 - b_1* pts)) - exp(alpha_2 - b_1* pts) / (1 + exp(alpha_2 - b_1* pts))
    # probs4 <- exp(alpha_4 - b_1* pts) / (1 + exp(alpha_4 - b_1* pts)) - exp(alpha_3 - b_1* pts) / (1 + exp(alpha_3 - b_1* pts))
    # probs5 <- 1 - exp(alpha_4 - b_1* pts) / (1 + exp(alpha_4 - b_1* pts))
    

    # plot(pts, probs1, xlim = c(-5, 5), ylim = c(0, 1), lwd = 2, col = "#88CCEE", type = "l",
    #      xlab = "x", ylab = "Prob(Y = j|X = x)")
    # lines(pts, probs2, #xlim = c(-5, 5), ylim = c(0, 1), lwd = 2, 
    #      col = "#CC6677", type = "l"#,
    #      # xlab = "X", ylab = "Prob(Y = 2|X = x)"
    #      )
    # lines(pts, probs3, col = "#DDCC77", type = "l")
    # lines(pts, probs4, col = "#117733", type = "l")
    # lines(pts, probs5, col = "#332288", type = "l")
    
    
  })
  
  output$plot2 <- renderPlot({
    
    ## Evaluate probabilities and plot
    mu <- input$mu
    alpha <- input$alpha
    
    # vector of outcome values to plot
    jvec <- 0:(4*mu + 5)
    # p_params <- mu/(mu+1/alpha)
    
    nbprobs <- dnbinom(jvec, size = 1/alpha, mu = mu, log = FALSE)
    
    barplot(nbprobs, names.arg = jvec, ylab = "Poisson Pr(Y=j|mu)", xlab = "j", main = "Poisson PMF, no explanatory variables")
    
    
    
  })
  
  
  output$plot3 <- renderPlot({
    
    ## Evaluate probabilities and plot
    b_0 <- input$b_0
    b_1 <- input$b_1
    x <- input$x
    mu <- exp(b_0+b_1*x)
    
    # vector of outcome values to plot
    jvec <- 0:(4*mu + 2)
    
    poissonprobs <- dpois(jvec, mu, log = FALSE)
    
    barplot(poissonprobs, names.arg = jvec, ylab = "Poisson Pr(Y=j|X=x)", xlab = "j", main = "Poisson PMF, mu=exp(b_0 + x*b_1)")
    
  })
  

  output$plot4 <- renderPlot({
    
    ## Evaluate probabilities and plot
    b_0 <- input$b_0
    b_1 <- input$b_1
    x <- input$x
    mu <- exp(b_0+b_1*x)
    alpha <- input$alpha
    
    # vector of outcome values to plot
    jvec <- 0:(4*mu + 2)
    
    nbprobs <- dnbinom(jvec, size = 1/alpha, mu = mu, log = FALSE)
    
    barplot(nbprobs, names.arg = jvec, ylab = "Poisson Pr(Y=j|X=x)", xlab = "j", main = "Poisson PMF, mu=exp(b_0 + x*b_1)")
    
  })

    
  output$plot5 <- renderPlot({
    
    ## Evaluate probabilities and plot
    b_0 <- input$b_0
    b_1 <- input$b_1
    x <- input$x
    mu <- exp(b_0+b_1*pts)
    
    # vector of outcome values to plot
    ymin <- 0
    ymax = max(mu)
    
    plot(pts,mu,  ylab = "Poisson E(Y|X=x)",     lwd = 2, col = "#88CCEE", type = "l",        
         xlim = c(-5, 5), ylim = c(0, ymax),
         xlab = "x", main = "Poisson E(Y|X=x)")
    lines(pts,mu*b_1, col = "#CC6677", type = "l")
    legend(-5, ymax, legend=c("E(Y|X=x)", "PE of E(Y|X=x) on x"),
           col=c("#88CCEE", "#CC6677"), lty= c(1,1), cex=0.8,
           box.lty=2, box.lwd=2, box.col="green")
  })
  
  # output$plot6 <- renderPlot({
  #   
  #   ## Evaluate probabilities and plot
  #   b_0 <- input$b_0
  #   b_1 <- input$b_1
  #   # x <- input$x
  #   mu <- exp(b_0+b_1*pts)
  #   
  #   # vector of outcome values to plot
  #   ymin <- 0
  #   ymax = max(mu)
  #   
  #   plot(pts,mu*b_1  , ylab = "Poisson E(Y|X=x)",            
  #        xlim = c(-5, 5), ylim = c(0, ymax), lwd = 2, col = "#88CCEE", type = "l",
  #        xlab = "x", main = "Poisson Partial Effect conditional on X=x")
  # })
  
  output$plot6 <- renderPlot({
    
    ## Evaluate probabilities and plot
    mu <- input$mu
    pi_Hurdle_ZI <- input$pi_Hurdle_ZI
    
    # vector of outcome values to plot
    jvec <- 0:(4*mu + 2)
    
    poissonprobs <- dpois(jvec, mu, log = FALSE)
    probs <- (1-pi_Hurdle_ZI)* poissonprobs/(1 - exp(-mu))
    probs[1] <- pi_Hurdle_ZI
    barplot(probs, names.arg = jvec, ylab = "Pr(Y=j|mu)", xlab = "j", main = "Hurdle Poisson PMF, no explanatory variables")
    
    
  })
  
  output$plot7 <- renderPlot({
    
    ## Evaluate probabilities and plot
    mu <- input$mu
    pi_Hurdle_ZI <- input$pi_Hurdle_ZI
    
    # vector of outcome values to plot
    jvec <- 0:(4*mu + 2)
    
    poissonprobs <- dpois(jvec, mu, log = FALSE)
    probs <- (1-pi_Hurdle_ZI)* poissonprobs
    probs[1] <- pi_Hurdle_ZI + probs[1]
    barplot(probs, names.arg = jvec, ylab = "Pr(Y=j|mu)", xlab = "j", main = "Zero-Inflated Poisson PMF, no explanatory variables")
    
    
    
    
  })
  
})
