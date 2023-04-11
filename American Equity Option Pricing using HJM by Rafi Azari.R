#HJM Stock Option Pricer by Rafi Azari
#American Stock Option Pricing via Optimal Stopping using the HJM Model 

##### Instructions #####
########################

#Ensure all functions are run for initialization first prior to triggering the main
#pricing function HJM_Stock_Option_Pricer included at the bottom
#There is a test command provided at the bottom of each function in case testing 
#each component is necessary
#Note that WRDS access is required
#In case of missing WRDS setup, refer to the below link:
#https://wrds-www.wharton.upenn.edu/pages/support/programming-wrds/programming-r/r-from-your-computer/

#As a first step, log into WRDS by replacing your WRDS username in the "user" field below and running the commands :
if (!require(RPostgres)){
  install.packages('RPostgres')
  library(RPostgres)
} 
wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  dbname='wrds',
                  sslmode='require',
                  user="enter WRDS username inside these brackets")


##### Start of Code #####
#########################

#Retrieve the secid for the given ticker
Get_secid <- function(ticker){
  query <- paste0("SELECT secid FROM optionm.optionmnames where ticker='", ticker,"'")
  res <- dbSendQuery(wrds, query)
  cell <- dbFetch(res, n=1)
  dbClearResult(res)
  secid=as.numeric(cell)
  return(cell)
}

test=Get_secid(ticker="IBM")
test

#Retrieve the daily spot prices based on ATM options for a given maturity and option type
Get_Historical_Spot_Prices <- function(ticker, type, maturity){
  if (type == "C"){
    delta = 50
  } else if (type == "P"){
    delta = -50
  }
  secid=Get_secid(ticker=ticker)
  query <- paste0("SELECT * FROM optionm.vsurfd2021  
                where secid='", secid, "'
                and cp_flag='", type, "' 
                and days='", maturity, "'
                and delta='", delta, "'
                ORDER BY date ASC")
  res <- dbSendQuery(wrds, query)
  optvolsurface_data <- dbFetch(res)
  dbClearResult(res)
  historical_spot_prices <- matrix(nrow=length(optvolsurface_data[,"date"]), ncol=5)
  colnames(historical_spot_prices) <- c("type", "date", "maturity", "delta", "spot_price")
  for(i in 1:length(optvolsurface_data[,"date"])){
    historical_spot_prices[i,"type"] = toString(optvolsurface_data[i,"cp_flag"])
    historical_spot_prices[i,"date"] = toString(optvolsurface_data[i,"date"])
    historical_spot_prices[i,"maturity"] = optvolsurface_data[i,"days"]
    historical_spot_prices[i,"delta"] = optvolsurface_data[i,"delta"]
    historical_spot_prices[i,"spot_price"] = optvolsurface_data[i,"impl_strike"]
  }
  
  return(historical_spot_prices)
}

test=Get_Historical_Spot_Prices(ticker="IBM", type="P", maturity=30)
head(test,5)

#Retrieve the daily option prices for a given maturity and delta
Get_Historical_Option_Prices <- function(ticker, type, maturity, delta){
  secid=Get_secid(ticker=ticker)
  query <- paste0("SELECT * FROM optionm.vsurfd2021  
                where secid='", secid, "'
                and cp_flag='", type, "' 
                and days='", maturity, "'
                and delta='", delta, "'
                ORDER BY date ASC")
  res <- dbSendQuery(wrds, query)
  optvolsurface_data <- dbFetch(res)
  dbClearResult(res)
  historical_option_prices <- matrix(nrow=length(optvolsurface_data[,"date"]), ncol=7)
  colnames(historical_option_prices) <- c("type", "date", "maturity", "delta", "option_price", "strike_price", "vol")
  for(i in 1:length(optvolsurface_data[,"date"])){
    historical_option_prices[i,"type"] = toString(optvolsurface_data[i,"cp_flag"])
    historical_option_prices[i,"date"] = toString(optvolsurface_data[i,"date"])
    historical_option_prices[i,"maturity"] = optvolsurface_data[i,"days"]
    historical_option_prices[i,"delta"] = optvolsurface_data[i,"delta"]
    historical_option_prices[i,"option_price"] = optvolsurface_data[i,"impl_premium"]
    historical_option_prices[i,"strike_price"] = optvolsurface_data[i,"impl_strike"]
    historical_option_prices[i,"vol"] = optvolsurface_data[i,"impl_volatility"]
  }
  
  return(historical_option_prices)
}

test=Get_Historical_Option_Prices(ticker="IBM", type="C", maturity=30, delta=50)
head(test,5)

#Retrieve closing prices from Yahoo Finance
Daily_Closing_Prices<-function(ticker){
  
  #Download and install the necessary libraries to pull closing stock prices from Yahoo finance
  if (!require(quantmod)){
    install.packages('quantmod')
    library(quantmod)
  } 
  
  if (!require(TTR)){
    install.packages('TTR')
    library(TTR)
  } 
  
  #Retrieve the full dataset from Yahoo Finance
  prices_complete=suppressWarnings(getSymbols(ticker,src='yahoo',auto.assign=FALSE))
  
  #Omit any NA values in the data for days which the bond market was closed
  prices_complete_clean=na.omit(prices_complete)
  
  #Retrieve the daily closing prices
  ticker <- gsub('[^[:alnum:] ]','',ticker)
  close_price_column_name= paste(ticker, "Close", sep=".")
  closing_prices = prices_complete_clean[,close_price_column_name]
  closing_prices_clean=as.matrix(data.frame(date=index(closing_prices), coredata(closing_prices)))
  
  return(closing_prices_clean)
}

test=Daily_Closing_Prices(ticker="IBM")
tail(test,1)

#Calculate the yearly risk-free rate on each day based on 3-month treasury bill closing prices
Get_Yearly_Risk_Free_Rates<-function(){
  
  #Retrieve prices for the 3-month Treasury Bill from Yahoo Finance
  closing_prices=Daily_Closing_Prices(ticker = '^IRX')
  
  #Omit any NA values in the data for days which the bond market was closed
  closing_prices_clean=na.omit(closing_prices)
  
  #Convert the data to a matrix and initialize the matrix used to convert 
  #Treasury bill prices to risk-free rates
  risk_free_rate_prices=as.matrix(closing_prices_clean)
  
  risk_free_rate_adjusted=matrix(NA,nrow=nrow(risk_free_rate_prices),ncol=2)
  colnames(risk_free_rate_adjusted) = c("date", "yearly_interest_rate")
  
  #Convert Treasury bill prices to yearly risk-free rates
  for(i in 1:nrow(risk_free_rate_prices)){
    risk_free_rate_adjusted[i,"date"]=risk_free_rate_prices[i,1]
    risk_free_rate_adjusted[i,"yearly_interest_rate"]=as.numeric(risk_free_rate_prices[i,2])/100
  }
  
  return(risk_free_rate_adjusted)
}

test=Get_Yearly_Risk_Free_Rates()
head(test,5)

#Add daily spot prices and interest rates to the corresponding option prices for each day
Get_Full_Daily_Option_Prices <- function(ticker, type, maturity, delta){
  
  #Retrieve daily spot prices for that maturity and option type
  Daily_Spot_Prices = Get_Historical_Spot_Prices(ticker=ticker, type=type, maturity=maturity)
  
  #Retrieve the daily interest rates
  Yearly_Risk_Free_Rates = Get_Yearly_Risk_Free_Rates()
  
  #Retrieve daily option prices for that maturity, option type, and delta
  Daily_Option_Prices = Get_Historical_Option_Prices(ticker=ticker, type=type, maturity=maturity, delta=delta)
  
  #Iterate through and add the spot price to the corresponding option price for each day
  Full_Option_Price_History = matrix(nrow = nrow(Daily_Option_Prices), ncol=9)
  colnames(Full_Option_Price_History) <- c("type", "date", "maturity", "delta", "option_price", "strike_price", "spot_price", "interest_rate", "vol")
  for( i in 1:nrow(Daily_Option_Prices)){
    Full_Option_Price_History[i,"type"] = Daily_Option_Prices[i,"type"]
    Full_Option_Price_History[i,"date"] = Daily_Option_Prices[i,"date"]
    Full_Option_Price_History[i,"maturity"] = Daily_Option_Prices[i,"maturity"]
    Full_Option_Price_History[i,"delta"] = Daily_Option_Prices[i,"delta"]
    Full_Option_Price_History[i,"option_price"] = Daily_Option_Prices[i,"option_price"]
    Full_Option_Price_History[i,"strike_price"] = Daily_Option_Prices[i,"strike_price"]
    Full_Option_Price_History[i,"spot_price"] = Daily_Spot_Prices[Daily_Spot_Prices[,"date"] == Full_Option_Price_History[i,"date"],"spot_price"]
    if (Full_Option_Price_History[i,"date"] %in% Yearly_Risk_Free_Rates[,"date"]){
      Full_Option_Price_History[i,"interest_rate"] = Yearly_Risk_Free_Rates[Yearly_Risk_Free_Rates[,"date"] == Full_Option_Price_History[i,"date"],"yearly_interest_rate"]
    } else {
      Full_Option_Price_History[i,"interest_rate"] = Full_Option_Price_History[i-1,"interest_rate"]
    }
    Full_Option_Price_History[i,"vol"] = Daily_Option_Prices[i,"vol"]
  }
  
  return(Full_Option_Price_History)
}

test=Get_Full_Daily_Option_Prices(ticker="IBM", type="C", maturity=365, delta=60)
tail(test,10)

#Calculate the forward drift for a given maturity and delta
Get_Daily_Forward_Drift <- function(ticker, type, maturity, delta){
  
  #Retrieve full option price history for the given maturity and delta, including the spot price for that day
  Daily_Option_Prices = Get_Full_Daily_Option_Prices(ticker=ticker, type=type, maturity=maturity, delta=delta)
  
  Daily_Forward_Drfit = matrix(nrow=nrow(Daily_Option_Prices), ncol=11)
  colnames(Daily_Forward_Drfit) <- c("type", "date", "maturity", "delta", "option_price", "strike_price", "spot_price", "interest_rate", "payoff", "drift", "vol")
  for(i in 1:nrow(Daily_Option_Prices)){
    Daily_Forward_Drfit[i,"type"] = Daily_Option_Prices[i,"type"]
    Daily_Forward_Drfit[i,"date"] = Daily_Option_Prices[i,"date"]
    Daily_Forward_Drfit[i,"maturity"] = as.numeric(Daily_Option_Prices[i,"maturity"])
    Daily_Forward_Drfit[i,"delta"] = Daily_Option_Prices[i,"delta"]
    Daily_Forward_Drfit[i,"option_price"] = as.numeric(Daily_Option_Prices[i,"option_price"])
    Daily_Forward_Drfit[i,"strike_price"] = as.numeric(Daily_Option_Prices[i,"strike_price"])
    Daily_Forward_Drfit[i,"spot_price"] = as.numeric(Daily_Option_Prices[i,"spot_price"])
    Daily_Forward_Drfit[i,"interest_rate"] = as.numeric(Daily_Option_Prices[i,"interest_rate"])
    if(Daily_Forward_Drfit[i,"type"] == "C"){
      Daily_Forward_Drfit[i,"payoff"] = max((as.numeric(Daily_Forward_Drfit[i,"spot_price"]) - as.numeric(Daily_Option_Prices[i,"strike_price"]) ),0)
    } else if(Daily_Forward_Drfit[i,"type"] == "P") {
      Daily_Forward_Drfit[i,"payoff"] = max((as.numeric(Daily_Option_Prices[i,"strike_price"]) - as.numeric(Daily_Forward_Drfit[i,"spot_price"]) ),0)
    }
    Daily_Forward_Drfit[i,"drift"] = (as.numeric(Daily_Forward_Drfit[i,"option_price"]) - as.numeric(Daily_Forward_Drfit[i,"payoff"])) / (as.numeric(Daily_Forward_Drfit[i,"maturity"]))
    Daily_Forward_Drfit[i,"vol"] = as.numeric(Daily_Option_Prices[i,"vol"])
  }
  
  return(Daily_Forward_Drfit)
}

test=Get_Daily_Forward_Drift(ticker="IBM", type="C", maturity=365, delta=60)
tail(test,5)

#Create the daily forward drift change time series for each delta and maturity
Get_Daily_Forward_Drift_Change_TimeSeries <- function(ticker, type, maturity, delta) {
  Daily_Drifts = Get_Daily_Forward_Drift(ticker=ticker, type=type, maturity=maturity, delta=delta)
  
  Daily_Drift_Change = matrix(nrow=(nrow(Daily_Drifts) - 1), ncol=5)
  colnames(Daily_Drift_Change) <- c("observation","type", "maturity", "delta", "drfit_change")
  for(i in 1:nrow(Daily_Drift_Change)){
    Daily_Drift_Change[i,"observation"] = i
    Daily_Drift_Change[i,"type"] = Daily_Drifts[i,"type"]
    Daily_Drift_Change[i,"maturity"] = Daily_Drifts[i,"maturity"]
    Daily_Drift_Change[i,"delta"] = Daily_Drifts[i,"delta"]
    Daily_Drift_Change[i,"drfit_change"] = as.numeric(Daily_Drifts[i+1,"drift"]) - as.numeric(Daily_Drifts[i,"drift"])
  }
  
  return(Daily_Drift_Change)
}

test=Get_Daily_Forward_Drift_Change_TimeSeries(ticker="IBM", type="C", maturity=30, delta=70)
tail(test,10)

#Create a matrix with daily forward drift changes for various maturities given a delta
Get_Forward_Drfit_Change_for_All_Maturities <- function(ticker, type, delta){
  
  #Get the forward drift changes per observation for each standard maturity on the surface for the given delta
  print("Retrieving 1m forward drift time series")
  Forward_Drfits_1m = Get_Daily_Forward_Drift_Change_TimeSeries(ticker=ticker, type=type, maturity=30, delta=delta)
  print("Retrieving 2m forward drift time series")
  Forward_Drfits_2m = Get_Daily_Forward_Drift_Change_TimeSeries(ticker=ticker, type=type, maturity=60, delta=delta)
  print("Retrieving 3m forward drift time series")
  Forward_Drfits_3m = Get_Daily_Forward_Drift_Change_TimeSeries(ticker=ticker, type=type, maturity=91, delta=delta)
  print("Retrieving 4m forward drift time series")
  Forward_Drfits_4m = Get_Daily_Forward_Drift_Change_TimeSeries(ticker=ticker, type=type, maturity=122, delta=delta)
  print("Retrieving 5m forward drift time series")
  Forward_Drfits_5m = Get_Daily_Forward_Drift_Change_TimeSeries(ticker=ticker, type=type, maturity=152, delta=delta)
  print("Retrieving 6m forward drift time series")
  Forward_Drfits_6m = Get_Daily_Forward_Drift_Change_TimeSeries(ticker=ticker, type=type, maturity=182, delta=delta)
  print("Retrieving 9m forward drift time series")
  Forward_Drfits_9m = Get_Daily_Forward_Drift_Change_TimeSeries(ticker=ticker, type=type, maturity=273, delta=delta)
  print("Retrieving 1y forward drift time series")
  Forward_Drfits_1y = Get_Daily_Forward_Drift_Change_TimeSeries(ticker=ticker, type=type, maturity=365, delta=delta)
  print("Retrieving 18m forward drift time series")
  Forward_Drfits_18m = Get_Daily_Forward_Drift_Change_TimeSeries(ticker=ticker, type=type, maturity=547, delta=delta)
  print("Retrieving 2y forward drift time series")
  Forward_Drfits_2y = Get_Daily_Forward_Drift_Change_TimeSeries(ticker=ticker, type=type, maturity=730, delta=delta)
  
  #Check to ensure we have the same set of observations for each maturity and if not terminate the program
  print("Verifying there is no missing data from the surface across all maturities")
  Y_DQ_Test <- matrix(nrow=10, ncol=1)
  colnames(Y_DQ_Test) <- c("observation_count")
  rownames(Y_DQ_Test) <- c("1m", "2m", "3m", "4m", "5m", "6m", "9m", "1y", "18m", "2y")
  for(i in 1:nrow(Y_DQ_Test)){
    Y_DQ_Test[i,1] = nrow(Forward_Drfits_1m)
    Y_DQ_Test[i,1] = nrow(Forward_Drfits_2m)
    Y_DQ_Test[i,1] = nrow(Forward_Drfits_3m)
    Y_DQ_Test[i,1] = nrow(Forward_Drfits_4m)
    Y_DQ_Test[i,1] = nrow(Forward_Drfits_5m)
    Y_DQ_Test[i,1] = nrow(Forward_Drfits_6m)
    Y_DQ_Test[i,1] = nrow(Forward_Drfits_9m)
    Y_DQ_Test[i,1] = nrow(Forward_Drfits_1y)
    Y_DQ_Test[i,1] = nrow(Forward_Drfits_18m)
    Y_DQ_Test[i,1] = nrow(Forward_Drfits_2y)
  }
  
  if (!require(quantmod)){
    install.packages('DescTools')
    library(DescTools)
  } 
  
  common_obs_count <- Mode(Y_DQ_Test[,"observation_count"])
  DQ_Test_Result <- matrix(nrow=2, ncol=1)
  DQ_Test_Result[1,1] = "Common Observation"
  DQ_Test_Result[2,1] = common_obs_count
  for(i in 1:nrow(Y_DQ_Test)){
    test_overall_status = "PASS"
    if(Y_DQ_Test[i,"observation_count"] != common_obs_count){
      DQ_Test_Result <- cbind(DQ_Test_Result, c(rowname(Y_DQ_Test)[i], Y_DQ_Test[i,"observation_count"]))
      test_overall_status = "FAIL"
    }
  }
  
  if(test_overall_status =="FAIL"){
    print("Missing data in the volatility surface; below are the expected and tenors with missing data")
    print(Y_DQ_Test)
    stop()
  }
  
  print("Surface data is complete; generating the complete daily forward change matrix across all maturities")
  #Now that we made sure there is no missing data, construct the daily drift change matrix Y that will be used in the PCA
  Y = Forward_Drfits_1m[,"drfit_change"]
  Y = cbind(Y, Forward_Drfits_2m[,"drfit_change"])
  Y = cbind(Y, Forward_Drfits_3m[,"drfit_change"])
  Y = cbind(Y, Forward_Drfits_4m[,"drfit_change"])
  Y = cbind(Y, Forward_Drfits_5m[,"drfit_change"])
  Y = cbind(Y, Forward_Drfits_6m[,"drfit_change"])
  Y = cbind(Y, Forward_Drfits_9m[,"drfit_change"])
  Y = cbind(Y, Forward_Drfits_1y[,"drfit_change"])
  Y = cbind(Y, Forward_Drfits_18m[,"drfit_change"])
  Y = cbind(Y, Forward_Drfits_2y[,"drfit_change"])
  colnames(Y) <- c("1m", "2m", "3m", "4m", "5m", "6m", "9m", "1y", "18m", "2y")
  
  return(Y)
}

test=Get_Forward_Drfit_Change_for_All_Maturities(ticker="IBM", type="C", delta=70)
head(test,5)

#Get forward drift volatilities for a given delta class based on top three PCA variances
Get_Forward_Drift_Variances_for_One_Delta_Class <- function(ticker, type, delta){
  
  #Retrieve the daily forward drift time series for all standard maturities on the volatility surface
  print("Retrieving daily forward drift change for all standard maturities on the surface")
  Forward_Drift_Change_TimeSeries <- Get_Forward_Drfit_Change_for_All_Maturities(ticker=ticker, type=type, delta=delta)
  class(Forward_Drift_Change_TimeSeries) <- "numeric"
  print("Completed retrieving the daily forward change across all maturities")
  
  print("Performing PCA on the matrix of the daily forward change")
  PCA_Result_Raw <- prcomp(na.omit(Forward_Drift_Change_TimeSeries), center = FALSE,scale. = FALSE)
  PCA_Result = summary(PCA_Result_Raw)$importance[,c(1:3)]
  PCA_Matrix = matrix(nrow=4,ncol=3)
  rownames(PCA_Matrix) = c("Standard Deviation", "Variance", "Proportion of Variance", "Total Variance Explained")
  colnames(PCA_Matrix) = c("PC1", "PC2", "PC3")
  for(i in 1:ncol(PCA_Matrix)){
    PCA_Matrix["Standard Deviation",i] = PCA_Result["Standard deviation",i]
    PCA_Matrix["Variance",i] = (PCA_Result["Standard deviation",i])^2
    PCA_Matrix["Proportion of Variance",i] = PCA_Result["Proportion of Variance",i]
    PCA_Matrix["Total Variance Explained",i] = PCA_Result["Cumulative Proportion",i]
  }
  
  return(PCA_Matrix)
}

test=Get_Forward_Drift_Variances_for_One_Delta_Class(ticker="IBM", type="C", delta=70)
test

#Get forward drift variances for all delta classes
Get_Forward_Drift_Variances_for_All_Delta_Class <- function(ticker, type){
  
  print(">>>>>Retrieving top three forward drift variances for each delta class<<<<<")
  
  #Determine the delta sign based on the option tyo
  if(type == "C"){
    delta_sign = 1
  } else if(type == "P"){
    delta_sign = -1
  }
  deltas <- matrix(c(30, 50, 70), nrow=3, ncol=1)
  deltas = deltas * delta_sign
  colnames(deltas) <- c("delta")
  
  #Set up the matrix to be used for summarizing the final variances
  Variance_Summary = matrix(nrow=nrow(deltas), ncol=3)
  colnames(Variance_Summary) <- c("PC1", "PC2", "PC3")
  row_names = matrix(nrow=nrow(deltas), ncol=1)
  for(i in 1:nrow(deltas)){
    row_names[i,1] = paste0("", deltas[i,"delta"], " delta")
  }
  rownames(Variance_Summary) <- row_names
  
  #Get top three variances using PCA for each delta class
  for(i in 1:nrow(deltas)){
    message <- paste0("-----Retrieving top three forward drift variances for the ", deltas[i,"delta"], " delta moneyness class-----")
    print(message)
    Forward_Drift_Variances = Get_Forward_Drift_Variances_for_One_Delta_Class(ticker = ticker, type = type, delta = deltas[i,"delta"])
    message <- paste0("Successfully retrieved top three forward drift variances for the ", deltas[i,"delta"], " delta moneyness class with these results:")
    print(Forward_Drift_Variances)
    
    message <- paste0("Final PCA result for the ", deltas[i,"delta"], " delta moneyness class is the following:")
    print(message)
    Variance_Summary[i,"PC1"] = Forward_Drift_Variances["Variance", "PC1"]
    Variance_Summary[i,"PC2"] = Forward_Drift_Variances["Variance", "PC2"]
    Variance_Summary[i,"PC3"] = Forward_Drift_Variances["Variance", "PC3"]
    print(Variance_Summary[i,])
  }
  
  #Print the consolidated PCA results across all delta classes
  print("-----The consolidated PCA results for all delta classes is the following:-----")
  print(Variance_Summary)
  
  #Average the variances for each PCA type across all deltas to arrive at the final variance for each PCA type
  print("-----Calculating the average variance for each PCA across all deltas-----")
  Final_Forward_Drift_Variances = as.matrix(colMeans(Variance_Summary))
  colnames(Final_Forward_Drift_Variances) = c("Forward Drift Variance")
  print(Final_Forward_Drift_Variances)
  
  return(Final_Forward_Drift_Variances)
}

test=Get_Forward_Drift_Variances_for_All_Delta_Class(ticker="IBM", type="C")
test

#Calculate American option price using the binomial lattice used as part of the calculation for the initial f(0) vector given OptionMetrics through WRDS only provides historical data
Binomial_American_Option_Pricer <- function(ticker, type, delta, Stock_Volatility, maturity, Risk_Free_Rate, Stock_Spot_Price, Strike_Price, time_steps){
  
  if(type == "C"){
    type = "call"
  } else if (type == "P"){
    type = "put"
  }
  #The binomial option pricer code is retrieved from FinanceTrain given it is quite standard
  #Source = https://financetrain.com/binomial-option-pricing-model-in-r
  #Start of code sourced from FinanceTrain
  #Build stock tree function
  build_stock_tree_binomial <- function(S, sigma, delta_t, N) {
    tree = matrix(0, nrow=N+1, ncol=N+1)
    U = exp(sigma*sqrt(delta_t))
    D = exp(-sigma*sqrt(delta_t))
    for (i in 1:(N+1)) {
      for (j in 1:i) {
        tree[i, j] = S * U^(j-1) * D^((i-1)-(j-1))
      }  }
    return(tree)
  }
  
  #Determine the up/down risk-neutral probabilities
  q_prob_binomial <- function(r, delta_t, sigma) {
    u = exp(sigma*sqrt(delta_t))
    d = exp(-sigma*sqrt(delta_t))
    return((exp(r*delta_t) - d)/(u-d))
  }
  
  value_binomial_stock_option <- function(tree, sigma, delta_t, r, X, type) {
    q = q_prob_binomial(r, delta_t, sigma)
    option_tree = matrix(0, nrow=nrow(tree), ncol=ncol(tree))
    if(type == 'put') {
      option_tree[nrow(option_tree),] = pmax(X - tree[nrow(tree),], 0)
    } else {  option_tree[nrow(option_tree),] = pmax(tree[nrow(tree),] - X, 0)
    }
    for (i in (nrow(tree)-1):1) {
      for(j in 1:i) {
        option_tree[i,j]=((1-q)*option_tree[i+1,j] + q*option_tree[i+1,j+1])/exp(r*delta_t)
      }
    }
    return(option_tree)
  }
  
  binomial_option_pricer <- function(type, sigma, T, r, X, S, N) {
    q <- q_prob_binomial(r=r, delta_t=T/N, sigma=sigma)
    tree <- build_stock_tree_binomial(S=S, sigma=sigma, delta_t=T/N, N=N)
    option <- value_binomial_stock_option(tree, sigma=sigma, delta_t=T/N, r=r, X=X, type=type)
    return(list(q=q, stock=tree, option=option, price=option[1,1]))
  }
  #End of source code from FinanceTrain
  
  
  #Determine spot price, volatility, risk_free_rate, spot_price, and strike_price from delta if delta provided
  
  #Determine the market risk-free rate if missing from the latest closing price of the three-month treasury T-bill
  if(missing(Risk_Free_Rate)){
    Risk_Free_Rate = Market_Risk_Free_Rate()
    Risk_Free_Rate = Risk_Free_Rate
  }
  
  #Determine the volatility if missing from historical volatility based on Yahoo Finance closing prices and using Hull's approach
  #Calculate Historical Stock Price Volatility based on Hull page 325
  Yearly_Historical_Volatility<-function(stock_ticker, lookback){
    
    #Retrieve daily closing prices for the stock
    if(missing(lookback)){
      lookback = 0
      stock_closing_prices = Closing_Prices(ticker = stock_ticker)
    } else {
      stock_closing_prices = head(Closing_Prices(ticker = stock_ticker),lookback)
    }
    
    #Initialize the matrices for log and squared log returns used for the calculation
    stock_log_returns=matrix(NA,nrow=nrow(stock_closing_prices)-1,ncol=1)
    stock_log_returns_squared=matrix(NA,nrow=nrow(stock_closing_prices)-1,ncol=1)
    
    #Calculate daily log and squared log returns
    for(i in 1:(nrow(stock_closing_prices)-1)){
      stock_log_returns[i,1]=log((stock_closing_prices[i+1,1]/stock_closing_prices[i,1]))
      stock_log_returns_squared[i,1]=(log((stock_closing_prices[i+1,1]/stock_closing_prices[i,1])))^2
    }
    
    #Keep the past year's data, based on 252 trading days in one year
    daily_stock_returns=tail(stock_log_returns,252)
    daily_stock_returns_squared=tail(stock_log_returns_squared,252)
    
    #Calculate daily stock volatility
    daily_volatility=sqrt((sum(daily_stock_returns_squared)/251)-((sum(daily_stock_returns)^2)/(252*251)))
    
    #Convert daily volatility to yearly volatility
    yearly_volatility=round(daily_volatility*sqrt(252),digits=4)
    
    return(yearly_volatility)
  }
  
  if(missing(Stock_Volatility)){
    Stock_Volatility = Yearly_Historical_Volatility(stock_ticker = ticker)
  }
  
  #Determine the market spot price from Yahoo Finance if missing
  if(missing(Stock_Spot_Price)){
    Stock_Bid_Ask = Market_Spot_Rate(ticker = ticker)
    if (type == "call"){
      Stock_Spot_Price = as.numeric(Stock_Bid_Ask["Ask",1])
    } else if (type == "put"){
      Stock_Spot_Price = as.numeric(Stock_Bid_Ask["Bid",1])
    }
  }
  
  #Determine the strike price if missing from the provided delta using the market convention, which is Black-Scholes-Merton for all exercise types
  if(missing(Strike_Price)){
    if(missing(delta)){
      print("Either delta or strike price needs to be provided; terminating.")
    } else {
      if (type == "call"){
        Strike_Price = Stock_Spot_Price * exp(-1*((Stock_Volatility*sqrt(maturity)*qnorm(exp(Risk_Free_Rate*maturity)*delta))-(Risk_Free_Rate+((Stock_Volatility)^2)/2)*maturity))
      } else if (type == "put"){
        Strike_Price = Stock_Spot_Price * exp(-1*((Stock_Volatility*sqrt(maturity)*qnorm((exp(Risk_Free_Rate*maturity)*delta)+1))-(Risk_Free_Rate+((Stock_Volatility)^2)/2)*maturity))
      }
    }
  }
  
  #Round the maturity if provided in days
  maturity = round(maturity,2)
  
  binomial_option_pricer_result = binomial_option_pricer(type=type, sigma=Stock_Volatility, T=maturity, r=Risk_Free_Rate, X=Strike_Price, S=Stock_Spot_Price, N=time_steps)
  
  binomial_option_price = binomial_option_pricer_result$price
  
  Final_Result=matrix(nrow=8,ncol=1)
  rownames(Final_Result) <- c("ticker", "type", "maturity", "spot_price", "strike_price", "volatility", "risk_free_rate", "option_price")
  colnames(Final_Result) <- c("value")
  Final_Result["ticker","value"] = toString(ticker)
  Final_Result["type","value"] = toString(type)
  Final_Result["maturity","value"] = maturity
  Final_Result["spot_price","value"] = Stock_Spot_Price
  Final_Result["strike_price","value"] = round(Strike_Price,2)
  Final_Result["volatility","value"] = Stock_Volatility
  Final_Result["risk_free_rate","value"] = Risk_Free_Rate
  Final_Result["option_price","value"] = binomial_option_price
  
  return(Final_Result)
}

test=Binomial_American_Option_Pricer(ticker="IBM", type="call", delta=0.5, maturity=1, time_steps=150)
test

#Get the initial drifts for all standard maturities to be used as starting points for the forward drift process
Get_Initial_Forward_Drfits_for_All_Maturities <- function(ticker, type, delta){
  
  print("<<<<<Retrieving the initial forward drift for all standard maturities for the given delta class as of the last available date in the market data<<<<<")
  
  maturities <- c(30, 60, 91, 122, 152, 182, 273, 365, 547, 730)
  row_names <- c("1m", "2m", "3m", "4m", "5m", "6m", "9m", "1y", "18m", "2y")
  Initial_Forward_Drifts <- matrix(nrow=10, ncol=5)
  rownames(Initial_Forward_Drifts) <- row_names
  colnames(Initial_Forward_Drifts) <- c("ticker", "type", "maturity", "delta", "drift")
  for(i in 1:10){
    message <- paste0("Retrieving the ", row_names[i], " American option forward drift change")
    print(message)
    Option_Price_Result =  tail(Get_Daily_Forward_Drift_Change_TimeSeries(ticker=ticker, type=type, maturity=maturities[i], delta=delta),1)
    Initial_Forward_Drifts[i,"ticker"] = ticker
    Initial_Forward_Drifts[i,"type"] = type
    Initial_Forward_Drifts[i,"maturity"] = maturities[i]
    Initial_Forward_Drifts[i,"delta"] = Option_Price_Result[1,"delta"]
    Initial_Forward_Drifts[i,"drift"] = Option_Price_Result[1,"drfit_change"]
  }
  
  print("-----Aggregating the forward drift result for the given delta class-----")
  print(Initial_Forward_Drifts)
  
  print("-----Returning the final result showing the initial drift for all standard maturities for the given delta class-----")
  Final_Result = matrix(nrow=10, ncol=2)
  rownames(Final_Result) <- c("1m", "2m", "3m", "4m", "5m", "6m", "9m", "1y", "18m", "2y")
  colnames(Final_Result) <- c("maturity", "drift")
  for(i in 1:10){
    Final_Result[i,"maturity"] = Initial_Forward_Drifts[i,"maturity"]
    Final_Result[i,"drift"] = Initial_Forward_Drifts[i,"drift"]
  }
  print(Final_Result)
  
  
  return(Final_Result)
}

test=Get_Initial_Forward_Drfits_for_All_Maturities(ticker="IBM", type="C", delta=50)

#Generic 3-component Brownian motion generator
Brownian_Motion_Generator<-function(beta1, beta2, beta3, mean, sd, starting_value, paths, days) {
  paths <- paths
  count <- days
  interval <- 1/days
  sample<-matrix(0,nrow=(count+1),ncol=paths)
  for(i in 1:paths)
  {
    sample[1,i]<- starting_value
    for(j in 2:(count+1))
    {
      sample[j,i]<-sample[j-1,i]+sqrt(beta1)*((interval)^.5)*rnorm(n=1, mean=mean, sd=sd)+sqrt(beta2)*((interval)^.5)*rnorm(n=1, mean=mean, sd=sd)+sqrt(beta3)*((interval)^.5)*rnorm(n=1, mean=mean, sd=sd)
    }
  }	
  
  matplot(sample,main="Forward Drift Process for each Maturity",xlab="Time",ylab="Path",type="l")
  
  return(sample)
}

test=Brownian_Motion_Generator(beta1=0.00006, beta2=0.0000037, beta3=0.0000009, mean=0, sd=1, starting_value=0.0, paths=100, days=365)
test

#Generate the forward drift process
Forward_Drift_Process_Generator <- function(ticker, type, delta, paths, days){
  
  #Calculate the forward drift variances to be passed to the forward drift process generator
  Forward_Drift_Variances = Get_Forward_Drift_Variances_for_All_Delta_Class(ticker=ticker, type=type)
  class(Forward_Drift_Variances) <- "numeric"
  
  #Calculate the initial forward drifts based on today's active market data
  Initial_Forward_Drifts = Get_Initial_Forward_Drfits_for_All_Maturities(ticker=ticker, type=type, delta=delta)
  class(Initial_Forward_Drifts) <- "numeric"
  
  #Generating the forward drift process for one starting point
  print("<<<<<Generating the forward drift process>>>>>")
  #1m Forward Drift Process
  Forward_Drfit_Process_1m = Brownian_Motion_Generator(beta1=Forward_Drift_Variances["PC1","Forward Drift Variance"], beta2=Forward_Drift_Variances["PC2","Forward Drift Variance"], beta3=Forward_Drift_Variances["PC3","Forward Drift Variance"], mean=0, sd=1, starting_value=Initial_Forward_Drifts["1m","drift"], paths=paths, days=days)

  #2m Forward Drift Process
  Forward_Drfit_Process_2m = Brownian_Motion_Generator(beta1=Forward_Drift_Variances["PC1","Forward Drift Variance"], beta2=Forward_Drift_Variances["PC2","Forward Drift Variance"], beta3=Forward_Drift_Variances["PC3","Forward Drift Variance"], mean=0, sd=1, starting_value=Initial_Forward_Drifts["2m","drift"], paths=paths, days=days)

  #3m Forward Drift Process
  Forward_Drfit_Process_3m = Brownian_Motion_Generator(beta1=Forward_Drift_Variances["PC1","Forward Drift Variance"], beta2=Forward_Drift_Variances["PC2","Forward Drift Variance"], beta3=Forward_Drift_Variances["PC3","Forward Drift Variance"], mean=0, sd=1, starting_value=Initial_Forward_Drifts["3m","drift"], paths=paths, days=days)

  #4m Forward Drift Process
  Forward_Drfit_Process_4m = Brownian_Motion_Generator(beta1=Forward_Drift_Variances["PC1","Forward Drift Variance"], beta2=Forward_Drift_Variances["PC2","Forward Drift Variance"], beta3=Forward_Drift_Variances["PC3","Forward Drift Variance"], mean=0, sd=1, starting_value=Initial_Forward_Drifts["4m","drift"], paths=paths, days=days)

  #5m Forward Drift Process
  Forward_Drfit_Process_5m = Brownian_Motion_Generator(beta1=Forward_Drift_Variances["PC1","Forward Drift Variance"], beta2=Forward_Drift_Variances["PC2","Forward Drift Variance"], beta3=Forward_Drift_Variances["PC3","Forward Drift Variance"], mean=0, sd=1, starting_value=Initial_Forward_Drifts["5m","drift"], paths=paths, days=days)

  #6m Forward Drift Process
  Forward_Drfit_Process_6m = Brownian_Motion_Generator(beta1=Forward_Drift_Variances["PC1","Forward Drift Variance"], beta2=Forward_Drift_Variances["PC2","Forward Drift Variance"], beta3=Forward_Drift_Variances["PC3","Forward Drift Variance"], mean=0, sd=1, starting_value=Initial_Forward_Drifts["6m","drift"], paths=paths, days=days)

  #9m Forward Drift Process
  Forward_Drfit_Process_9m = Brownian_Motion_Generator(beta1=Forward_Drift_Variances["PC1","Forward Drift Variance"], beta2=Forward_Drift_Variances["PC2","Forward Drift Variance"], beta3=Forward_Drift_Variances["PC3","Forward Drift Variance"], mean=0, sd=1, starting_value=Initial_Forward_Drifts["9m","drift"], paths=paths, days=days)

  #1y Forward Drift Process
  Forward_Drfit_Process_1y = Brownian_Motion_Generator(beta1=Forward_Drift_Variances["PC1","Forward Drift Variance"], beta2=Forward_Drift_Variances["PC2","Forward Drift Variance"], beta3=Forward_Drift_Variances["PC3","Forward Drift Variance"], mean=0, sd=1, starting_value=Initial_Forward_Drifts["1y","drift"], paths=paths, days=days)

  #18m Forward Drift Process
  Forward_Drfit_Process_18m = Brownian_Motion_Generator(beta1=Forward_Drift_Variances["PC1","Forward Drift Variance"], beta2=Forward_Drift_Variances["PC2","Forward Drift Variance"], beta3=Forward_Drift_Variances["PC3","Forward Drift Variance"], mean=0, sd=1, starting_value=Initial_Forward_Drifts["18m","drift"], paths=paths, days=days)

  #2y Forward Drift Process
  Forward_Drfit_Process_2y = Brownian_Motion_Generator(beta1=Forward_Drift_Variances["PC1","Forward Drift Variance"], beta2=Forward_Drift_Variances["PC2","Forward Drift Variance"], beta3=Forward_Drift_Variances["PC3","Forward Drift Variance"], mean=0, sd=1, starting_value=Initial_Forward_Drifts["1y","drift"], paths=paths, days=days)
  
  Forward_Drfit_Process <- cbind(Forward_Drfit_Process_1m, Forward_Drfit_Process_2m, Forward_Drfit_Process_3m, Forward_Drfit_Process_4m, Forward_Drfit_Process_5m, Forward_Drfit_Process_6m, Forward_Drfit_Process_9m, Forward_Drfit_Process_1y, Forward_Drfit_Process_18m, Forward_Drfit_Process_2y)
  
  matplot(Forward_Drfit_Process,main="Forward Drift Process",xlab="Time",ylab="Path",type="l")
  
  return(Forward_Drfit_Process)
}

test_process=Forward_Drift_Process_Generator(ticker="IBM", type="C", delta=60, paths=10, days=365)
head(test_process,5)

#Calculate the optimal stopping time
Optimal_Stopping_Time_Calculator <- function(forward_drift_process){
  
  print("<<<<<Calculating the optimal stopping time>>>>>")
  paths = ncol(forward_drift_process)
  path_names = seq(1:paths)
  intervals = nrow(forward_drift_process)
  x <- (0:(intervals-1))
  result = matrix(nrow=paths, ncol=1)
  rownames(result) <- path_names
  colnames(result) <- c("days")
  for(i in 1:paths){
    y = forward_drift_process[,i]
    drift_plot =cbind(x,y)
    class(drift_plot) <- "numeric"
    final_period = nrow(drift_plot)
    for (j in 1:nrow(drift_plot)){
      drift_integral = drift_plot[j,"y"] * (drift_plot[final_period,"x"] - drift_plot[j,"x"])
      if(drift_integral > 0){
        optimal_stopping_time = drift_plot[final_period,"x"]
        next
      } else if(drift_integral == 0){
        optimal_stopping_time = drift_plot[j,"x"]
        break
      } else if(drift_integral < 0){
        if (j==1){
          optimal_stopping_time = 0
          break
        }
        optimal_stopping_time = drift_plot[j-1,"x"]
        break
      }
    }
    result[i,"days"] = optimal_stopping_time
  }
  
  averages = matrix(nrow=1, ncol=1)
  rownames(averages) = c("optimal_stopping_time")
  averages[1,1] = round(colMeans(result)[1],0)
  result = rbind(result,averages)
  average_optimal_stopping_time = tail(result,1)
  
  print("The optimal stopping time is the following:")
  print(average_optimal_stopping_time)
  
  return(average_optimal_stopping_time)
}

test=Optimal_Stopping_Time_Calculator(forward_drift_process = test_process)
test

#This is the main function for pricing the option using the HJM model
HJM_Stock_Option_Pricer <- function(ticker, type, delta, paths, days, contracts){
  
  #Converting the user-friendly optiont type to OptionMetrics format
  if (type == "call"){
    type = "C"
  } else if (type == "put"){
    type = "P"
  }
  
  print("+++++Pricing the Option Using HJM Model+++++")
  #Generate the forward drift process
  Forward_Drfit_Process = Forward_Drift_Process_Generator(ticker=ticker, type=type, delta=delta, paths=paths, days=days)
  
  #Calculate the optimal stopping time
  Optimal_Stopping_Time_Result = Optimal_Stopping_Time_Calculator(forward_drift_process=Forward_Drfit_Process)
  Optimal_Stopping_Time <- as.numeric(Optimal_Stopping_Time_Result["optimal_stopping_time","days"])
  
  #Price the option using the HJM model
  print("<<<<<Calculating the HJM stock option price>>>>>")
  Option_Data=tail(Get_Full_Daily_Option_Prices(ticker="IBM", type="C", maturity=365, delta=60),1)
  strike_price <- as.numeric(Option_Data[1,"strike_price"])
  initial_stock_price <- as.numeric(Option_Data[1,"spot_price"])
  risk_free_rate <- 0
  volatility <- as.numeric(Option_Data[1,"vol"])
  start_time <- 0
  optimal_stopping_time <- Optimal_Stopping_Time
  
  if (type=="C"){
    integrand <- function(t){
      exp(-1*risk_free_rate*(t-start_time)) * pnorm(((log(initial_stock_price/strike_price)) + (risk_free_rate + ((volatility)^2)*(1/2) )* (t-start_time))/(volatility*sqrt(t-start_time)))
    }
    R <- integrate(integrand,start_time,optimal_stopping_time)$value
    HJM_Price = format(round(contracts * 100 * (max((initial_stock_price-strike_price),0) + risk_free_rate * strike_price * R),2), big.mark=",")
  } else if (type=="P"){
    integrand <- function(t){
      exp(-1*risk_free_rate*(t-start_time)) * pnorm(((log(initial_stock_price/strike_price)) + (risk_free_rate - ((volatility)^2)*(1/2) )* (t-start_time))/(volatility*sqrt(t-start_time)))
    }
    R <- integrate(integrand,start_time,optimal_stopping_time)$value
    HJM_Price = format(round(contracts * 100 * (max((initial_stock_price-strike_price),0) - risk_free_rate * strike_price * R),2), big.mark=",")
  }
  
  print("Below is the option price using the HJM model:")
  print(HJM_Price)
  
  print("+++++Pricing the Option Using the Binomial Model+++++")
  binomial_option_pricer_result=Binomial_American_Option_Pricer(ticker=ticker, type=type, Risk_Free_Rate=risk_free_rate, Stock_Spot_Price =initial_stock_price, Stock_Volatility=volatility, Strike_Price=strike_price, maturity=days/365, time_steps=100)
  Binomial_Price = format(round(contracts * 100 * as.numeric(binomial_option_pricer_result["option_price","value"]),2), big.mark=",")
  print("Below is the option price using the binomial model:")
  print(Binomial_Price)
  
  print("+++++Generating the Result Summary+++++")
  result_summary = matrix(nrow=1, ncol=2)
  rownames(result_summary) <- c("price")
  colnames(result_summary) <- c("HJM Model", "Binomial Model")
  result_summary["price","HJM Model"] = HJM_Price
  result_summary["price","Binomial Model"] = Binomial_Price
  print(result_summary)
  
  print("+++++Pricing Completed+++++")
  
  return(HJM_Price)
}

price_test=HJM_Stock_Option_Pricer(ticker="IBM", type="call", delta=60, paths=10, days=365, contracts=1)





