library(shiny)


# static model values - outside function, visible across all sessions

s1_fare_elas <- -0.4
s1_mile_elas <- 0.4
s2_fare_elas <- -0.2
s2_mile_elas <- 0.6
passenger_loss_rate <- .01
inflation_rate <- .01
tax_rate <- 1
interest_rate <- 0.05
bankrupt_level <- -50


server <- function(input, output) {
  # Define reactive values
  
  values <- reactiveValues()
  
  # company level reactive values
  
  values$year <- 0
  values$id <- 1
  values$unitcost <- 1
  values$fixedcost <- 50
  values$userfeedback <-
    "Costs and revenues currently balance exactly. Set your desired change in fare and mileage for each service next year and click the 'Simulate next year' button. Good luck!"
  values$bankrupt <- "no"
  
  # define company dataframe
  
  values$cdf <- data.frame(
    id = as.integer(1),
    year = as.integer(0),
    total_revenue = 250,
    fixed_costs = 50,
    unit_cost = 1,
    total_costs = 250,
    interest_paid = 0,
    gross_profit = 0,
    tax = 0,
    net_profit = 0,
    interest_due = 0,
    tax_todate = 0,
    interest_todate = 0,
    balance = 0,
    stringsAsFactors = FALSE
  )
  
  
  # define service 1 dataframe
  
  values$s1df <- data.frame(
    id = as.integer(1),
    year = as.integer(0),
    fare_increase = 0,
    mileage_increase = 0,
    miles = 100,
    fare = 1,
    passengers = 150,
    variable_cost = 100,
    revenue = 150
  )
  
  
  # define service 2 dataframe
  
  values$s2df <- data.frame(
    id = as.integer(1),
    year = as.integer(0),
    fare_increase = 0,
    mileage_increase = 0,
    miles = 100,
    fare = 1,
    passengers = 100,
    variable_cost = 100,
    revenue = 100
  )
  
  
  
  # observe event - respond to simulate button
  observeEvent(input$simulate, {
    if (input$simulate > 0 & values$bankrupt == "no") {
      # increase unit cost by inflation
      values$unitcost <-
        (values$unitcost + (inflation_rate * values$unitcost))
      
      # increase fixed cost by inflation
      values$fixedcost <-
        (values$fixedcost + (inflation_rate * values$fixedcost))
      
      # calculate service 1 values
      
      s1_prior_miles <- values$s1df$miles[values$id]
      s1_prior_fare <- values$s1df$fare[values$id]
      s1_prior_passengers <- values$s1df$passengers[values$id]
      s1_prior_variable_cost <- values$s1df$variable_cost[values$id]
      
      s1_new_miles <-
        s1_prior_miles * (1 + (input$s1_mileage_increase / 100))
      s1_new_fare <-
        s1_prior_fare * (1 + (input$s1_fare_increase / 100))
      s1_new_passengers <-
        (s1_prior_passengers - (passenger_loss_rate * s1_prior_passengers)) * ((s1_new_fare / s1_prior_fare) ^
                                                                                 s1_fare_elas) * ((s1_new_miles / s1_prior_miles) ^ s1_mile_elas)
      s1_new_variable_cost <- values$unitcost * s1_new_miles
      s1_revenue <- s1_new_fare * s1_new_passengers
      
      # calculate service 2 values
      
      s2_prior_miles <- values$s2df$miles[values$id]
      s2_prior_fare <- values$s2df$fare[values$id]
      s2_prior_passengers <- values$s2df$passengers[values$id]
      s2_prior_variable_cost <- values$s2df$variable_cost[values$id]
      
      s2_new_miles <-
        s2_prior_miles * (1 + (input$s2_mileage_increase / 100))
      s2_new_fare <-
        s2_prior_fare * (1 + (input$s2_fare_increase / 100))
      s2_new_passengers <-
        (s2_prior_passengers - (passenger_loss_rate * s2_prior_passengers)) * ((s2_new_fare / s2_prior_fare) ^
                                                                                 s2_fare_elas) * ((s2_new_miles / s2_prior_miles) ^ s2_mile_elas)
      s2_new_variable_cost <- values$unitcost * s2_new_miles
      s2_revenue <- s2_new_fare * s2_new_passengers
      
      # calculate company values
      
      prior_interest_due <- values$cdf$interest_due[values$id]
      prior_tax_todate <- values$cdf$tax_todate[values$id]
      prior_interest_todate <- values$cdf$interest_todate[values$id]
      prior_balance <- values$cdf$balance[values$id]
      
      revenue <- s1_revenue + s2_revenue
      cost <-
        s1_new_variable_cost + s2_new_variable_cost + values$fixedcost
      grprofit <- revenue - cost - prior_interest_due
      
      if (grprofit > 10) {
        tax <- (grprofit - 10) * tax_rate
      } else {
        tax <- 0
      }
      
      if (grprofit < 0) {
        new_interest_due <- abs(grprofit * interest_rate)
      } else {
        new_interest_due <- 0
      }
      
      netprofit <- grprofit - tax
      new_tax_todate <- prior_tax_todate + tax
      new_interest_todate <-
        prior_interest_todate + prior_interest_due
      new_balance <- prior_balance + netprofit
      
      # check for bankruptcy
      
      if (new_balance < bankrupt_level) {
        values$bankrupt <- "yes"
      }
      
      
      # set user feedback strings
      
      if (values$bankrupt == "yes") {
        values$userfeedback <-
          "You have gone BANKRUPT! Better luck next time ..."
      } else {
        if (netprofit < 0) {
          values$userfeedback <-
            "You have made a LOSS this year! You will pay interest on borrowings next year."
        }
        
        if (netprofit > 0 & tax == 0) {
          values$userfeedback <- "You have made a PROFIT this year!"
        }
        
        if (netprofit > 0 & tax > 0) {
          values$userfeedback <-
            "You have made EXCESSIVE PROFITS this year and have been taxed!"
        }
      }
      
      
      # Increment id and year
      values$year = values$year + 1
      values$id = values$id + 1
      
      
      # Update dataframes
      
      # Update s1 df
      newLine <- c(
        values$id,
        values$year,
        input$s1_fare_increase,
        input$s1_mileage_increase,
        s1_new_miles,
        s1_new_fare,
        s1_new_passengers,
        s1_new_variable_cost,
        s1_revenue
      )
      values$s1df[nrow(values$s1df) + 1, ] <- newLine
      values$s1df$id <- as.integer(values$s1df$id)
      values$s1df$year <- as.integer(values$s1df$year)
      
      # Update s2 df
      newLine <- c(
        values$id,
        values$year,
        input$s2_fare_increase,
        input$s2_mileage_increase,
        s2_new_miles,
        s2_new_fare,
        s2_new_passengers,
        s2_new_variable_cost,
        s2_revenue
      )
      values$s2df[nrow(values$s2df) + 1, ] <- newLine
      values$s2df$id <- as.integer(values$s2df$id)
      values$s2df$year <- as.integer(values$s2df$year)
      
      # Update company df
      newLine <- c(
        values$id,
        values$year,
        revenue,
        values$fixedcost,
        values$unitcost,
        cost,
        prior_interest_due,
        grprofit,
        tax,
        netprofit,
        new_interest_due,
        new_tax_todate,
        new_interest_todate,
        new_balance
      )
      values$cdf[nrow(values$cdf) + 1, ] <- newLine
      values$cdf$id <- as.integer(values$cdf$id)
      values$cdf$year <- as.integer(values$cdf$year)
      
    }
  })
  
  
  # define outputs
  
  # passengers plot
  
  output$passengers <- renderPlot({
    plot(
      values$s1df$year,
      values$s1df$passengers,
      type = 'o',
      xaxt = "n",
      ylim = c(0, max(
        values$s1df$passengers + values$s2df$passengers
      )),
      xlim = c(0, max(values$s1df$year)),
      xlab = "Year",
      ylab = "Number",
      main = "Passengers",
      col = '#4d4dff',
      lwd = 1,
      pch = 3
    )
    axis(side = 1, at = c(0:max(values$s1df$year)))
    lines(
      values$s1df$year,
      values$s2df$passengers,
      type = "o",
      col = "#ff4d4d",
      lwd = 1,
      pch = 4
    )
    lines(
      values$s1df$year,
      (values$s1df$passengers + values$s2df$passengers),
      type = "o",
      col = "black",
      lwd = 1,
      pch = 1
    )
    legend(
      "bottomleft",
      bty = "n",
      c("s1", "s2", "co"),
      lty = c(1, 1, 1),
      lwd = c(1, 1, 1),
      pch = c(3, 4, 1),
      col = c("#4d4dff", "#ff4d4d", "black")
    )
    
  })
  
  # miles plot
  output$miles <- renderPlot({
    plot(
      values$s1df$year,
      values$s1df$miles,
      type = 'o',
      xaxt = "n",
      ylim = c(0, max(
        values$s1df$miles + values$s2df$miles
      )),
      xlim = c(0, max(values$s1df$year)),
      xlab = "Year",
      ylab = "Number",
      main = "Miles",
      col = '#4d4dff',
      lwd = 1,
      pch = 3
    )
    axis(side = 1, at = c(0:max(values$s1df$year)))
    lines(
      values$s1df$year,
      values$s2df$miles,
      type = "o",
      col = "#ff4d4d",
      lwd = 1,
      pch = 4
    )
    lines(
      values$s1df$year,
      (values$s1df$miles + values$s2df$miles),
      type = "o",
      col = "black",
      lwd = 1,
      pch = 1
    )
    legend(
      "bottomleft",
      bty = "n",
      c("s1", "s2", "co"),
      lty = c(1, 1, 1),
      lwd = c(1, 1, 1),
      pch = c(3, 4, 1),
      col = c("#4d4dff", "#ff4d4d", "black")
    )
    
  })
  
  # revenue plot
  output$revenue <- renderPlot({
    plot(
      values$s1df$year,
      values$s1df$revenue,
      type = 'o',
      xaxt = "n",
      ylim = c(0, max(
        values$s1df$revenue + values$s2df$revenue
      )),
      xlim = c(0, max(values$s1df$year)),
      xlab = "Year",
      ylab = "Amount",
      main = "Revenue",
      col = '#4d4dff',
      lwd = 1,
      pch = 3
    )
    axis(side = 1, at = c(0:max(values$s1df$year)))
    lines(
      values$s1df$year,
      values$s2df$revenue,
      type = "o",
      col = "#ff4d4d",
      lwd = 1,
      pch = 4
    )
    lines(
      values$s1df$year,
      (values$s1df$revenue + values$s2df$revenue),
      type = "o",
      col = "black",
      lwd = 1,
      pch = 1
    )
    legend(
      "bottomleft",
      bty = "n",
      c("s1", "s2", "co"),
      lty = c(1, 1, 1),
      lwd = c(1, 1, 1),
      pch = c(3, 4, 1),
      col = c("#4d4dff", "#ff4d4d", "black")
    )
    
  })
  
  
  # costs plot
  output$costs <- renderPlot({
    plot(
      values$s1df$year,
      values$s1df$variable_cost,
      type = 'o',
      xaxt = "n",
      ylim = c(0, max(values$cdf$total_costs)),
      xlim = c(0, max(values$s1df$year)),
      xlab = "Year",
      ylab = "Amount",
      main = "Costs",
      col = '#4d4dff',
      lwd = 1,
      pch = 3
    )
    axis(side = 1, at = c(0:max(values$s1df$year)))
    lines(
      values$s1df$year,
      values$s2df$variable_cost,
      type = "o",
      col = "#ff4d4d",
      lwd = 1,
      pch = 4
    )
    lines(
      values$s1df$year,
      (values$cdf$total_costs),
      type = "o",
      col = "black",
      lwd = 1,
      pch = 1
    )
    legend(
      "bottomleft",
      bty = "n",
      c("s1", "s2", "co"),
      lty = c(1, 1, 1),
      lwd = c(1, 1, 1),
      pch = c(3, 4, 1),
      col = c("#4d4dff", "#ff4d4d", "black")
    )
    
  })
  
  
  # net profit plot
  output$profit <- renderPlot({
    plot(
      values$cdf$year,
      values$cdf$net_profit,
      type = 'o',
      xaxt = "n",
      ylim = c(min(values$cdf$net_profit), max(values$cdf$net_profit)),
      xlim = c(0, max(values$cdf$year)),
      xlab = "Year",
      ylab = "Amount",
      main = "Group net profit",
      col = 'black',
      lwd = 1,
      pch = 1
    )
    axis(side = 1, at = c(0:max(values$s1df$year)))
    
  })
  
  
  # balance plot
  output$cbalance <- renderPlot({
    plot(
      values$cdf$year,
      values$cdf$balance,
      type = 'o',
      xaxt = "n",
      ylim = c(min(values$cdf$balance), max(values$cdf$balance)),
      xlim = c(0, max(values$cdf$year)),
      xlab = "Year",
      ylab = "Amount",
      main = "Group balance (to date)",
      col = 'black',
      lwd = 1,
      pch = 1
    )
    axis(side = 1, at = c(0:max(values$cdf$year)))
    
  })
  
  # Data table output and download function
  
  output$c_data_out  <-
    renderTable(values$cdf[, c(2:14)], striped = TRUE, spacing = "xs")
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('dominobus', 'csv', sep = ".")
    },
    content = function(file) {
      # Write to a file specified by the 'file' argument
      write.csv(values$cdf[, c(2:14)], file,
                row.names = FALSE)
    }
  )
  
  # Dashboard output
  
  output$year <- renderText(paste({
    values$year
  }))
  output$grossprofit <-
    renderText(paste({
      round(values$cdf$gross_profit[values$id], 2)
    }))
  output$netprofit <-
    renderText(paste({
      round(values$cdf$net_profit[values$id], 2)
    }))
  output$taxpaid <-
    renderText(paste({
      round(values$cdf$tax[values$id], 2)
    }))
  output$interestdue <-
    renderText(paste({
      round(values$cdf$interest_due[values$id], 2)
    }))
  output$balance <-
    renderText(paste({
      round(values$cdf$balance[values$id], 2)
    }))
  output$feedback <- renderText(paste({
    values$userfeedback
  }))
  
  # Note this is used with conditional panel to deactivate simulation button once bankrupt
  output$bankrupt <- renderText(paste({
    values$bankrupt
  }))
  
  
  # This line is needed as output$bankrupt is not shown on the app, to prevent it being suspended
  outputOptions(output, "bankrupt", suspendWhenHidden = FALSE)
  
}