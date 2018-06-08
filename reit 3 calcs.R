## @knitr setup
setwd("P:/IMD/Karl/R projects/reit r")
require(zoo)
require(lubridate)
source("../basic financial.r",echo=FALSE)
load ("dailyreit.rdata")
#oganize data
reitprice=zoo(reitdat[,2],as.Date(reitdat[,1]))
reitptobk=zoo(reitdat[,4],as.Date(reitdat[,1]))
reitdiv=zoo(reitdat[,3],as.Date(reitdat[,1]))
#reit3yrgrow=zoo(reitdat[,4],as.Date(reitdat[,1]))
#reit1yrgrow=zoo(reitdat[,5],as.Date(reitdat[,1]))
t5yr.yld=zoo(t5yr[,2],as.Date(t5yr[,1]))
spdiv=zoo(spdat[,3],as.Date(spdat[,1]))
reitprice=reitprice[!is.na(reitprice)]
reitdiv=reitdiv[!is.na(reitdiv)]
reitptobk=reitptobk[!is.na(reitptobk)]
#reit3yrgrow=reit3yrgrow[!is.na(reit3yrgrow)]
#reit1yrgrow=reit1yrgrow[!is.na(reit1yrgrow)]
t5yr.yld=t5yr.yld[!is.na(t5yr.yld)]
spdiv=spdiv[!is.na(spdiv)]
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
spdiv=spdiv[time(spdiv)>=first&time(spdiv)<=last]
# hold=merge(days365,reitprice)
# reitprice=na.approx(hold[,2],na.rm=FALSE)
# hold=merge(days365,reitdiv)
# reitdiv=na.approx(hold[,2],na.rm=FALSE)
# #hold=merge(days365,reit3yrgrow)
# #reit3yrgrow=na.approx(hold[,2],na.rm=FALSE)
# #hold=merge(days365,reit1yrgrow)
# #reit1yrgrow=na.approx(hold[,2],na.rm=FALSE)
# hold=merge(days365,t5yr.yld)
# t5yr.yld=na.approx(hold[,2],na.rm=FALSE)
reitprice.num=as.numeric(reitprice)
fwdsix=100*(-1+reitprice.num[125:length(reitprice.num)]
            /reitprice.num[1:(-124+length(reitprice.num))])
fwdsix=zoo(fwdsix,time(reitprice)[1:length(fwdsix)])
fwdtwelve=100*(-1+reitprice.num[250:length(reitprice.num)]
               /reitprice.num[1:(-249+length(reitprice.num))])
fwdtwelve=zoo(fwdtwelve,time(reitprice)[1:length(fwdtwelve)])
fwdone=100*(-1+reitprice.num[21:length(reitprice.num)]
               /reitprice.num[1:(-20+length(reitprice.num))])
fwdone=zoo(fwdone,time(reitprice)[1:length(fwdone)])

yspread=reitdiv-t5yr.yld
spyspread=reitdiv-spdiv
div=reitdiv*reitprice*.01
divnum=as.numeric(div)
divlen=length(div)
# div1yrgrow=-1+(divnum[366:divlen]/divnum[1:(divlen-365)])
# div3yrgrow=-1+((divnum[1096:divlen]/divnum[1:(divlen-1095)])^(1/3))
# div1yrgrow=zoo(div1yrgrow,time(div)[366:divlen])
# div3yrgrow=zoo(div3yrgrow,time(div)[(1096):divlen])
#plot
# par(mfrow=c(2,1))
# plot(reitdiv,ylim=c(-10,20),ylab='percent')
# lines(t5yr.yld,col='blue')
# lines(yspread,col='red')
# lines(fwdsix,col='green')
# plot(reitprice,ylab="Price")
# par(mfrow=c(1,1))
# reitdaily=100*(-1+(reitprice.num[2:length(reitprice.num)]
#                    /reitprice.num[1:(-1+length(reitprice.num))]))
# reitdaily=zoo(reitdaily,time(reitprice)[1:length(reitdaily)])
# plot(reitdaily,col='blue')
# plot(yspread,fwdsix,pch=19,cex=.3)
# regres=lm(fwdsix~yspread[time(fwdsix)])
# abline(reg=regres,col='red')
# summary(regres)
# plot(yspread,fwdone,pch=19,cex=.3)
# regres=lm(fwdone~yspread[time(fwdone)])
# abline(reg=regres,col='red')
# summary(regres)
# plot(yspread,col='blue')
# abline(h=c(0,2,4,6,8,10),lty='dotted')
# plot(spyspread,col='blue')
# abline(h=c(0,2,4,6,8,10),lty='dotted')
# plot(spyspread,fwdsix,pch=19,cex=.3)
# x=merge(spyspread,fwdsix)
# goodrow=(!is.na(x[,1]))&(!is.na(x[,2]))
# x=x[goodrow,]
# regres=lm(x[,1]~x[,2])
# plot(x[,1],x[,2],pch==19,cex=.3)
# abline(reg=regres,col='red')
# summary(regres)
# plot(spyspread,fwdone,pch=19,cex=.3)
# x=merge(spyspread,fwdone)
# goodrow=(!is.na(x[,1]))&(!is.na(x[,2]))
# x=x[goodrow,]
# regres=lm(x[,1]~x[,2])
# abline(reg=regres,col='red')
# summary(regres)
# plot(spyspread,ylim=c(-20,20))
# lines(fwdone,col='green')
# plot(reitptobk,fwdone,pch=19,cex=.3)
require(ggplot2)
plotpred=function(xx,yy,xname,yname) {
  x=merge(xx,yy)
  goodrow=(!is.na(x[,1]))&(!is.na(x[,2]))
  x=x[goodrow,]
  dat=data.frame(yspread=x[,1],fwdone=x[,2],yr=year(time(x)))
  ggplot(dat,aes(x=yspread,y=fwdone,color=yr)) +
    geom_point(shape=1) +
    geom_smooth() +
    xlab(xname) +
    ylab(yname)
  
}
## @knitr ptobook
require(zoo)
par(mfrow=c(2,1))
plot(reitptobk,col='blue',xlab='',ylab='Reit price to book')
abline(h=mean(reitptobk),lty='dotted')
plot(reitprice,col='blue',ylab='Reit Total Return Index',xlab='')
## @knitr ptobook1
require(ggplot2)
require(zoo)
require(lubridate)
plotpred(reitptobk,fwdone,'Price to Book','Next 1 month return')

## @knitr ptobook6
require(ggplot2)
require(zoo)
require(lubridate)
plotpred(reitptobk,fwdsix,'Price to Book','Next 6 mos. return')

## @knitr ptobook12
require(ggplot2)
require(zoo)
require(lubridate)
plotpred(reitptobk,fwdtwelve,'Price to Book','Next 1 yr. return')

## @knitr spyspread
require(zoo)
par(mfrow=c(2,1))
plot(spyspread,col='blue',xlab='',ylab='Spread to SP500 div yld')
abline(h=mean(spyspread),lty='dotted')
plot(reitprice,col='blue',ylab='Reit Total Return Index',xlab='')

## @knitr spyspread1
require(ggplot2)
require(zoo)
require(lubridate)
plotpred(spyspread,fwdone,'Spread to SP500 div yld','Next 1 month return')

## @knitr spyspread6
require(ggplot2)
require(zoo)
require(lubridate)
plotpred(spyspread,fwdsix,'Spread to SP500 div yld','Next 6 mos. return')

## @knitr spyspread12
require(ggplot2)
require(zoo)
require(lubridate)
plotpred(spyspread,fwdtwelve,'Spread to SP500 div yld','Next 1 yr. return')

## @knitr yspread
require(zoo)
par(mfrow=c(2,1))
plot(yspread,col='blue',xlab='',ylab='Spread to 5 yr treas')
abline(h=mean(yspread),lty='dotted')
plot(reitprice,col='blue',ylab='Reit Total Return Index',xlab='')

## @knitr yspread1
require(ggplot2)
require(zoo)
require(lubridate)
plotpred(yspread,fwdone,'Spread to 5 yr treas','Next 1 month return')

## @knitr yspread6
require(ggplot2)
require(zoo)
require(lubridate)
plotpred(yspread,fwdsix,'Spread to 5 yr treas','Next 6 mos. return')

## @knitr yspread12
require(ggplot2)
require(zoo)
require(lubridate)
plotpred(yspread,fwdtwelve,'Spread to 5 yr treas','Next 1 yr. return')
