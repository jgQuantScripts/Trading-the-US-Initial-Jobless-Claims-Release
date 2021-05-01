require("pdftools"); require("tm");require("stringr");require("IBrokers");require("httr");require("rvest")
# https://www.investing.com/economic-calendar
# https://oui.doleta.gov/press/2021/040821.pdf
# ***************************************************************************
# function to open position / send OPEN order to IB
openPOS = function(ticker, shares, action){
# PLACE ORDER FUNCTION
require("IBrokers")
tws = twsConnect(port=7497)
ac <- reqAccountUpdates(tws)
# create tws CFutures Contract
contract <- twsContract()
contract$symbol <- paste(ticker)
contract$currency <- "USD"
contract$sectype <- "FUT"

CONTRACT <- reqContractDetails(tws,contract)
CONTRACT <- CONTRACT[[which.min(as.numeric(lapply(CONTRACT,"[[",c("contractMonth"))))]]
security = twsFUT(CONTRACT$contract$symbol,CONTRACT$contract$exch,CONTRACT$contract$expiry)

securityShrs = as.character(shares)
ACT          = as.character(action) # "BUY" / "SELL"
orderId0      = as.numeric(reqIds(tws))
myorder      = twsOrder(orderId0, orderType = "MKT",outsideRTH = "1",action=ACT, 
                        totalQuantity = securityShrs, transmit=TRUE)
placeOrder(tws,security, myorder)
}
# function to open position / send OPEN order to IB
closePOS = function(ticker, action,take_profit){
  Sys.sleep(5)
  # PLACE ORDER FUNCTION
  require("IBrokers")
  tws = twsConnect(port=7497)
  ac <- reqAccountUpdates(tws)
  # # check if Position exists
  positions = twsPortfolioValue(ac,zero.pos=FALSE)       # get current positions
  tickers = str_sub(positions$local,1,-3)
  loc = which(tickers == ticker)
  if(length(which(tickers == ticker)) != 0){
    pos = positions[loc,]
    ENTRY_PRC = round((pos$averageCost/5 + 0.25) * 2) / 2 - 0.25
    EXIT_PRC = ifelse(action=="BUY",as.character(abs(ENTRY_PRC) - take_profit),as.character(abs(ENTRY_PRC) + take_profit))
    contract <- twsContract()
    contract$symbol <- paste(ticker)
    contract$currency <- "USD"
    contract$sectype <- "FUT"
    
    CONTRACT <- reqContractDetails(tws,contract)
    CONTRACT <- CONTRACT[[which.min(as.numeric(lapply(CONTRACT,"[[",c("contractMonth"))))]]
    security = twsFUT(CONTRACT$contract$symbol,CONTRACT$contract$exch,CONTRACT$contract$expiry)
    #securityShrs = as.character(shares)
    securityShrs = as.character(1)
    ACT          = as.character(action) # "BUY" / "SELL"
    orderId      = as.numeric(reqIds(tws))
    myorder      = twsOrder(orderId, orderType = "LMT",lmtPrice=EXIT_PRC,outsideRTH = "1",
                            action=ACT, totalQuantity = securityShrs, transmit=TRUE)
    placeOrder(tws,security, myorder)
  }else{
    cat("\nSecurity Not Found!\n")
  }
}
### SLEEP UNTIL THE SCHEDULED NEWS RELEASE
SLEEEP = function(RELEASE_DATETIME){
  ttt <- RELEASE_DATETIME - Sys.time()
  HMS <- attr(ttt,"units")
  tt <- as.numeric(ttt)
  if(HMS == "hours")
  {
    print(paste0("Will now sleep for: ",tt , " hours"));cat("\n")
    print(paste0("STARTING AT: ",RELEASE_DATETIME));cat("\n")
    Sys.sleep(tt*60*60)
  }
  if(HMS == "mins")
  {
    print(paste0("Will now sleep for: ",tt , " minutes"));cat("\n")
    print(paste0("STARTING AT: ",RELEASE_DATETIME));cat("\n")
    Sys.sleep(tt*60)
  }
  if(HMS == "secs")
  {
    print(paste0("Will now sleep for: ",tt , " seconds"));cat("\n")
    print(paste0("STARTING AT: ",RELEASE_DATETIME));cat("\n")
    Sys.sleep(tt)
  }  
}
## FIX TZ
START = function(dte)
{
  # difference between current time zone & New York Time
  tmDIFF = round(as.numeric(difftime(Sys.time(),
                                     lubridate::force_tz(with_tz(Sys.time(),tz="America/New_York")),
                                     units = "hours")),0)
  # round to the nearest 30 minutes
  START <- as.POSIXct(round_date(Sys.time(), paste0(30," minutes")))
  # time adjustment
  START  <- as.POSIXct(paste0(dte," 08:30:00"), format="%m%d%y %H:%M:%S") + hours(tmDIFF)   
  START
}
# "forecast" initial weekly claims | must be entered manually
est = 549000
# get the very next release (every Thursday @ 8:30 AM EST)
thisWK <- seq.Date(Sys.Date(), Sys.Date()+7, "1 day")
dte    <- thisWK[base::which(weekdays(thisWK) == "Thursday")]
# change the release date to match naming convention on Bureau of Labor (BOL)
dte    <- format(dte[1], format="%m%d%y")
file <- paste0(dte,".pdf")
# Timestamp of when Algo should start... ideally on time!
RELEASE_DATETIME = START(dte)
# ***********************************************
#     ALGO INSTRUCTIONS: 'weeklyClaimsAlgo'
# ***********************************************
weeklyClaimsAlgo = function(Symbol, Shares)
{
  # check to see if the new PDF posted
  url = "https://oui.doleta.gov/press/2021/"
  web = read_html(url)
  web <- web %>% html_text() %>% str_split(pattern= "\n")
  
  # if the PDF is posted on the site then run the following
  if(suppressWarnings(str_detect(string = web, pattern = file)))
  {
    # file = "020421.pdf"
    # file = "031121.pdf"
    # file = "040821.pdf"
    # store time to see how long the process takes
    IN = Sys.time()
    # read in the PDF
    tmp <- pdftools::pdf_text(pdf = paste0("https://oui.doleta.gov/press/2021/",file))
    # break the PDF with new-line delimiter... easier to work with in chunks
    tmp = tmp[1] %>% str_split(pattern="\n", n=25)
    # locate where in the PDF reads "In the week ending..." all releases have been the same
    loc = tmp[[1]] %>% str_detect(pattern = "In the week ending ") %>% base::which()
    # subset location (its in the 1st paragraph somewhere)
    initClaims = tmp[[1]][loc]
    # replace commas with empty space in orde to convert to numeric values
    initClaims<- gsub("\\,","",initClaims)
    # split the vector of words after "adjusted initial claims was"
    initClaims <- str_split(initClaims,pattern = "adjusted initial claims was")
    # get the second item in the list (this is where the number is stored)
    tmp <- initClaims[[1]][2]
    # don't care about the words just numbers... pull all instances of numerical values
    tmp <- regmatches(tmp, gregexpr("[[:digit:]]+", tmp))
    # convert to numeric for comparison
    tmp <- tmp %>% unlist %>% as.numeric
    # initial weekly claims number
    tmp <- tmp[1]
    # System out time
    OUT = Sys.time()
    # usually takes between 2-3 seconds to run the above
    OUT-IN
    # Prints current time
    cat("\nTime: ",paste(Sys.time()))
    # if the number is above estimates then bearish... 
    if(tmp > est){cat("\nBearish: ", tmp, " VS ",est,"\n"); ACT = "SELL"}
    # if the number is below estimates then bullish.. 
    if(tmp < est){cat("\nBullish: ", tmp, " VS ",est,"\n"); ACT = "BUY"}
    # send order to IB.. trading 1 share 
    openPOS(ticker = Symbol, shares = Shares, action = ACT)
    #openPOS2(ticker = Symbol, shares = Shares, action = ACT, take_profit = .50, stop=0.50)
    nuACT = ifelse(ACT == "SELL","BUY","SELL")
    cat("\nOPENING ORDER SENT...\n")
    closePOS(ticker=Symbol,action = nuACT, take_profit = 2.00)
    cat("\nCLOSING ORDER SENT...\n")
    return(cat("\nORDER SENT!\n"))
  }else{
    # if the release is not available yet then the system will sleep for 5 seconds and..
    # it will then call the `weeklyClaimsAlgo` function again essentially loop until we get 
    # data
    cat("\nNo DATA YET!\n")
    Sys.sleep(5)
    weeklyClaimsAlgo()
  }
}
# ***************************************************************************
#                           Initial Weekly Claims Algo
# ***************************************************************************
# Ran the night before the release.. will sleep until the Release time
SLEEEP(RELEASE_DATETIME);weeklyClaimsAlgo(Symbol="MES", Shares = 1)








# 
# # function to open position / send OPEN order to IB
# closePOS = function(ticker, action,take_profit, stop){
#   Sys.sleep(5)
#   # PLACE ORDER FUNCTION
#   require("IBrokers")
#   tws = twsConnect(port=7497)
#   ac <- reqAccountUpdates(tws)
#   # # check if Position exists
#   positions = twsPortfolioValue(ac,zero.pos=FALSE)       # get current positions
#   tickers = str_sub(positions$local,1,-3)
#   loc = which(tickers == ticker)
#   if(length(which(tickers == ticker)) != 0){
#   pos = positions[loc,]
#   ENTRY_PRC = round((pos$averageCost/5 + 0.25) * 2) / 2 - 0.25
#   EXIT_PRC = ifelse(action == "BUY",as.character(abs(ENTRY_PRC) + take_profit),
#                     as.character(abs(ENTRY_PRC) - take_profit))
#   STOP_PRC = ifelse(action == "BUY", as.character(abs(ENTRY_PRC) - stop), 
#                     as.character(abs(ENTRY_PRC) + stop))
#   # #NROWS  = ifelse(length(positions)==0,0,nrow(positions))
#   # # *********************************************************************
#   # security     = twsSTK(paste(ticker),exch="SMART",primary="BATS",currency="USD")
#   # securityShrs = as.character(abs(pos$position))
#   # ACT          = as.character(action)
#   # orderId      = as.numeric(reqIds(tws))
#   # # get quote
#   # midPoint = IBrokers::reqMktData(tws, Contract = security, snapshot=TRUE)
#   # lmtPRC       = round(as.numeric((midPoint$bidPrice + midPoint$askPrice)/2),2)
#   # myorder      = twsOrder(orderId, orderType = "LMT",lmtPrice = lmtPRC ,outsideRTH = "1",
#   #                         action=ACT, totalQuantity = securityShrs, transmit=FALSE)
#   # placeOrder(tws,security, myorder)
#   # }else{
#   #   cat("\nNothing to Close!\n")
#   # }
#   contract <- twsContract()
#   contract$symbol <- paste(ticker)
#   contract$currency <- "USD"
#   contract$sectype <- "FUT"
# 
#   CONTRACT <- reqContractDetails(tws,contract)
#   CONTRACT <- CONTRACT[[which.min(as.numeric(lapply(CONTRACT,"[[",c("contractMonth"))))]]
#   security = twsFUT(CONTRACT$contract$symbol,CONTRACT$contract$exch,CONTRACT$contract$expiry)
#   #securityShrs = as.character(shares)
#   securityShrs = as.character(1)
#   ACT          = as.character(action) # "BUY" / "SELL"
#   orderId      = as.numeric(reqIds(tws))
#   myorder      = twsOrder(orderId, orderType = "LMT",lmtPrice=EXIT_PRC,outsideRTH = "1",
#                           action=ACT, totalQuantity = securityShrs, transmit=TRUE,ocaType = "1",
#                           ocaGroup = paste0("OCA100",orderId))
#   placeOrder(tws,security, myorder)
# 
#   orderId2      = as.numeric(reqIds(tws))
#   myorder      = twsOrder(orderId2, orderType = "STPLMT", outsideRTH = "1",auxPrice  = STOP_PRC,
#                           lmtPrice  = STOP_PRC,action=ACT, totalQuantity = securityShrs,
#                           transmit=TRUE, ocaGroup = paste0("OCA100",orderId),
#                           ocaType = "1")
#   placeOrder(tws,security, myorder)
# 
#   }else{
#     cat("\nSecurity Not Found!\n")
#   }
# }
# 
