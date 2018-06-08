## reit analysis
setwd("P:/IMD/Karl/R projects/reit r")
require(zoo)
require(Rbbg)
require(lubridate)
source("../basic financial.r",echo=FALSE)
#get bbg data
conn=blpConnect()
sec.name.reit="FNERTR Index"
field.div="EQY_DVD_YLD_12M"
field.tr="TOT_RETURN_INDEX_NET_DVDS"
#field.3yr.grow="EQY_DPS_GROSS_3YR_GROWTH"
#field.1yr.grow="EQY_DPS_NET_1YR_GROWTH"
sec.name.5yr="GT05 Govt"
field.5yr="YLD_YTM_MID"
field.ptobk="PX_TO_BOOK_RATIO"
start="20010101"
end="20141130"
reitdat=bdh(conn, 
              sec.name.reit,
              c(field.tr,field.div,field.ptobk),
              start, 
              end)
t5yr=bdh(conn,sec.name.5yr,field.5yr,start,end)
spdat=bdh(conn, 
          "SPX Index",
          c(field.tr,field.div),
          start, 
          end)
blpDisconnect(conn)
save.image("dailyreit.rdata")
#oganize data
reitprice=zoo(reitdat[,2],as.Date(reitdat[,1]))
reitdiv=zoo(reitdat[,3],as.Date(reitdat[,1]))
#reit3yrgrow=zoo(reitdat[,4],as.Date(reitdat[,1]))
#reit1yrgrow=zoo(reitdat[,5],as.Date(reitdat[,1]))
t5yr.yld=zoo(t5yr[,2],as.Date(t5yr[,1]))
reitprice=reitprice[!is.na(reitprice)]
reitdiv=reitdiv[!is.na(reitdiv)]
#reit3yrgrow=reit3yrgrow[!is.na(reit3yrgrow)]
#reit1yrgrow=reit1yrgrow[!is.na(reit1yrgrow)]
t5yr.yld=t5yr.yld[!is.na(t5yr.yld)]
first=as.Date(max(time(reitprice[1]),time(reitdiv[1]),
                  time(t5yr.yld[1])))
last=as.Date(min(lastinvec(time(reitprice)),lastinvec(time(reitdiv)),
         lastinvec(time(t5yr.yld))))
days365=zooreg(rep(0,1+last-first),start=first,end=last)
reitprice=reitprice[time(reitprice)>=first&time(reitprice)<=last]
reitdiv=reitdiv[time(reitdiv)>=first&time(reitdiv)<=last]
#reit3yrgrow=reit3yrgrow[time(reit3yrgrow)>=first&time(reit3yrgrow)<=last]
#reit1yrgrow=reit1yrgrow[time(reit1yrgrow)>=first&time(reit1yrgrow)<=last]
t5yr.yld=t5yr.yld[time(t5yr.yld)>=first&time(t5yr.yld)<=last]
hold=merge(days365,reitprice)
reitprice=na.approx(hold[,2],na.rm=FALSE)
hold=merge(days365,reitdiv)
reitdiv=na.approx(hold[,2],na.rm=FALSE)
#hold=merge(days365,reit3yrgrow)
#reit3yrgrow=na.approx(hold[,2],na.rm=FALSE)
#hold=merge(days365,reit1yrgrow)
#reit1yrgrow=na.approx(hold[,2],na.rm=FALSE)
hold=merge(days365,t5yr.yld)
t5yr.yld=na.approx(hold[,2],na.rm=FALSE)
reitprice.num=as.numeric(reitprice)
fwdsix=100*(-1+reitprice.num[183:length(reitprice.num)]
            /reitprice.num[1:(-182+length(reitprice.num))])
fwdsix=zoo(fwdsix,time(reitprice)[1:length(fwdsix)])
fwdtwelve=100*(-1+reitprice.num[366:length(reitprice.num)]
            /reitprice.num[1:(-365+length(reitprice.num))])
fwdtwelve=zoo(fwdtwelve,time(reitprice)[1:length(fwdtwelve)])
yspread=reitdiv-t5yr.yld
div=reitdiv*reitprice*.01
divnum=as.numeric(div)
divlen=length(div)
div1yrgrow=-1+(divnum[366:divlen]/divnum[1:(divlen-365)])
div3yrgrow=-1+((divnum[1096:divlen]/divnum[1:(divlen-1095)])^(1/3))
div1yrgrow=zoo(div1yrgrow,time(div)[366:divlen])
div3yrgrow=zoo(div3yrgrow,time(div)[(1096):divlen])
#plot
par(mfrow=c(2,1))
plot(reitdiv,ylim=c(-10,20),ylab='percent')
lines(t5yr.yld,col='blue')
lines(yspread,col='red')
lines(fwdsix,col='green')
plot(reitprice,ylab="Price")
par(mfrow=c(1,1))
reitdaily=100*(-1+(reitprice.num[2:length(reitprice.num)]
                  /reitprice.num[1:(-1+length(reitprice.num))]))
reitdaily=zoo(reitdaily,time(reitprice)[1:length(reitdaily)])
plot(reitdaily,col='blue')





# download.file("http://returns.reit.com/returns/Industrial-Office.xls","industrial-office.xls",method="internal")
# data=loadWorkbook("industrial-office.xls")
# data2=readWorksheet(data,sheet="Index Data",startRow=8,endRow=0,startCol=0,endCol=0,header=FALSE)
