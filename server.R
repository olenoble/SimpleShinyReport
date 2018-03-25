library(shiny)
library(plyr)
library(reshape)

shinyServer(function(input, output) {

	  # You can access the value of the widget with input$select, e.g.
  	output$stratselect = renderPrint({input$stratselect})
 
  	## get data
  	strategy_output = reactive({
  		if (is.null(input$stratselect))
  			return(NULL)                

  	  ## Extract Vol Data
  		inFile = paste("./data/", input$stratselect, "_gedi.csv", sep = "")
  		data = read.csv(inFile, stringsAsFactors = FALSE)
    	data$Date = as.Date(data$Date, "%d-%b-%y")
    	data$Expiry = as.Date(data$Expiry, "%d-%b-%y")
    	data = data[order(data$Date, data$Expiry, decreasing = TRUE),]

    	## Get R2 statistics
    	r2_avg = ddply(data, .(Date), function(x){mean(x$R2)})
    	r2_min = ddply(data, .(Date), function(x){min(x$R2)})
    	
    	N = dim(r2_avg)[1]
    	metrics_avg = data.frame(average=mean(r2_avg$V1), dcount=N, check1=sum(r2_avg$V1>0.99)/N, check2=sum(r2_avg$V1>0.95)/N, check3=sum(r2_avg$V1>0.90)/N, check4=sum(r2_avg$V1>0.50)/N)
    	metrics_min = data.frame(average=mean(r2_min$V1), dcount=N, check1=sum(r2_min$V1>0.99)/N, check2=sum(r2_min$V1>0.95)/N, check3=sum(r2_min$V1>0.90)/N, check4=sum(r2_min$V1>0.50)/N)
    	metrics_all = rbind(metrics_avg, metrics_min)

  		names(metrics_all) = c("Average", "Count", "Above 99%", "Above 95%", "Above 90%", "Above 50%")
		  metrics = data.frame(t(metrics_all))
		  names(metrics) = c("Average Daily R2", "Min Daily R2")
		  
		  ## Breakdown by Year/Month
		  r2_avg_ym = r2_avg
		  r2_avg_ym$Year = format(r2_avg_ym$Date, '%Y')
		  r2_avg_ym$Month = format(r2_avg_ym$Date, '%b')
		  r2_avg_breakdown = ddply(r2_avg_ym, c(.(Year), .(Month)), function(X){data.frame(startdate=min(X$Date), statsout=mean(X$V1))})
		  r2_avg_breakdown = r2_avg_breakdown[order(r2_avg_breakdown$startdate),]
		  
		  r2_avg_breakdown_df = cast(r2_avg_breakdown, Year~Month, value='statsout')
		  collist = c('Year', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
		  r2_breakdown_metrics = r2_avg_breakdown_df[collist[collist %in% names(r2_avg_breakdown_df)]]
		  
		  ## Vols
		  historical_vol = interpolated_volbyFwdMoneyness(data, input$moneyness, input$maturity)
		  
		  list(undldata=data, r2_avg=r2_avg, r2_min=r2_min, metrics=metrics, metrics_ym=r2_breakdown_metrics, historicalvol = historical_vol)
		})

  	
  	output$contents = renderDataTable({
  		strategy_output()$undldata
  	  })

  	
	  output$metrics = renderTable({
	    strategy_output()$metrics
	    }, striped=TRUE, hover=TRUE, bordered=TRUE, rownames=TRUE
	    )

	  
	  output$metrics_ym = renderTable({
	    strategy_output()$metrics_ym
	    }, striped=TRUE, hover=TRUE, bordered=TRUE, rownames=FALSE
	    )
	  
	
  	output$plot = renderPlot({
  		if(!is.null(strategy_output)){
  		  par(mfrow=c(2,1))
  		  plot(strategy_output()$r2_avg, pch=18, xlab='Date', 'ylab'='R2', main='Average Daily R2')
  		  plot(strategy_output()$r2_min, pch=18, xlab='Date', 'ylab'='R2', main='Miniumun Daily R2')
  		  }
  	  })

  	
    output$YoYplot = renderPlot({
  		if(!is.null(strategy_output)){
  			X = strategy_output()$undldata
  			X$year = format(X$Date, "%Y")
  			yoy = ddply(X, .(year), function(y){y$Close[1] / y$Close[dim(y)[1]] - 1})
  			barplot(yoy$V1 * 100, name = yoy$year)
  		}
  	})

    
    output$HistoricalVol = renderPlot({
      if(!is.null(strategy_output)){
        X = strategy_output()$historicalvol
        titleGraph = paste(input$maturity, 'M ', input$moneyness, '% Volatility', sep = '')
        plot(X$Date, X$Vol * 100, xlab='Date', ylab='Volatility', type='l', main=titleGraph)
      }
    })
    
    
  	output$downloadData = downloadHandler(
    	filename = function(){paste(input$stratselect, '.csv', sep='')},
    	content = function(file) {write.csv(strategy_output()$undldata, file)}
    )

})


interpolated_volbyFwdMoneyness = function(x, m, tenor){
  y = gedi_volbyFwdMoneyness(x, m)
  y$vsq = y$vol * y$vol * y$vttx
  interpvol = ddply(y, .(Date), function(X,mat){
    if(dim(X)[1] < 2){
      return(NA)
    } else{
      return(approx(X$vttx, X$vsq, mat, rule=1)$y)
    }}, tenor/12)
  names(interpvol) = c('Date', 'Vol')
  return(interpvol)
}


gedi_volbyFwdMoneyness = function(x, m){
  x$vttx = as.numeric(x$Expiry - x$Date) / 365
  N = -log(m/100) /(10 * sqrt(x$vttx))
  wing = ifelse(N < 0, x$Putwing, x$CallWing)
  A = wing * atan(N/wing) * 10
  x$vol = x$ATM - x$Skew * A + x$Kurtosis * A * A 
  return(x)
  }
