# get reit index data
require (Rbbg)
require (zoo)
require (lubridate)
require (XLConnect)
#load("reitmfdata.rdata")
conn=blpConnect()

djusreit=bdh(conn,"DJUSRET Index","PX_LAST",Sys.Date()-25*366,
             option_names="periodicitySelection",option_value="MONTHLY")
zdjusreit=zoo(djusreit$PX_LAST,as.Date(djusreit$date))
mu.djusreit=zoo(diff(log(coredata(zdjusreit))),time(zdjusreit)[-1])
nareit=bdh(conn,"FNERTR Index","PX_LAST",Sys.Date()-25*366,
           option_names="periodicitySelection",option_value="MONTHLY")
znareit=zoo(nareit$PX_LAST,as.Date(nareit$date))
mu.nareit=zoo(diff(log(coredata(znareit))),time(znareit)[-1])
djglblre=bdh(conn,"DWGRST Index","PX_LAST",Sys.Date()-25*366,
        option_names="periodicitySelection",option_value="MONTHLY")
zdjglblre=zoo(djglblre$PX_LAST,as.Date(djglblre$date))
mu.djglblre=zoo(diff(log(coredata(zdjglblre))),time(zdjglblre)[-1])
djxusre=bdh(conn,"DWXRST Index","PX_LAST",Sys.Date()-25*366,
           option_names="periodicitySelection",option_value="MONTHLY")
zdjxusre=zoo(djxusre$PX_LAST,as.Date(djxusre$date))
mu.djxusre=zoo(diff(log(coredata(zdjxusre))),time(zdjxusre)[-1])
tickers=read.csv('reit mf tickers.csv',stringsAsFactors=FALSE)
tick=tickers$Ticker
tick=sub(" US","",tick)
tick=paste(tick,"US Equity")
mfret=list()
first=as.Date("1994-1-1")
last=Sys.Date()-1
ticker.vec=tick
mfname=tickers$Name
for (i in 1:length(ticker.vec)) {
  hold=bdh(conn,tick[i],"TOT_RETURN_INDEX_NET_DVDS",Sys.Date()-25*366,
              option_names="periodicitySelection",option_value="MONTHLY")
  hold=zoo(hold$TOT_RETURN_INDEX_NET_DVDS,as.Date(hold$date))
  mfret[[i]]=zoo(diff(log(coredata(hold))),time(hold)[-1])  
}
names(mfret)=tick
blpDisconnect(conn)
plot(exp(cumsum(mu.djusreit)),type='l',col='blue')
lines(exp(cumsum(mu.djglblre)),type='l',col='red')
lines(exp(cumsum(mu.djxusre)),type='l',col='green')
save.image("reitbbgdata.rdata")
