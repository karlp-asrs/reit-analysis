# get reit index data
require (Rbbg)
require (zoo)
require (lubridate)
require (XLConnect)
load("reitmfdata.rdata")
conn=blpConnect()

djusreit=bdh(conn,"DJUSRET Index","PX_LAST",Sys.Date()-25*366,
             option_names="periodicitySelection",option_value="MONTHLY")
zdjusreit=zoo(djusreit$PX_LAST,as.Date(djusreit$date))
mu.djusreit=zoo(diff(log(coredata(zdjusreit))),time(zdjusreit)[-1])
djglblre=bdh(conn,"DWGRST Index","PX_LAST",Sys.Date()-25*366,
        option_names="periodicitySelection",option_value="MONTHLY")
zdjglblre=zoo(djglblre$PX_LAST,as.Date(djglblre$date))
mu.djglblre=zoo(diff(log(coredata(zdjglblre))),time(zdjglblre)[-1])
djxusre=bdh(conn,"DWXRST Index","PX_LAST",Sys.Date()-25*366,
           option_names="periodicitySelection",option_value="MONTHLY")
zdjxusre=zoo(djxusre$PX_LAST,as.Date(djxusre$date))
mu.djxusre=zoo(diff(log(coredata(zdjxusre))),time(zdjxusre)[-1])

blpDisconnect(conn)
plot(exp(cumsum(mu.djusreit)),type='l',col='blue')
lines(exp(cumsum(mu.djglblre)),type='l',col='red')
lines(exp(cumsum(mu.djxusre)),type='l',col='green')
save.image("reitmfdata.rdata")
