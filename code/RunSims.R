try(stopCluster(cl),silent=TRUE)
library(bayesm)
library(dplyr)
library(ggplot2)
library(scales)
library(ggthemes)
library(parallel)
rm(list=ls())
library(xtable)
library(tidyr)
library(tables)
#library(grid)
#library(gridExtra)


library(compiler)
cmpfile("./code/MonteCarlo.R")
loadcmp("./code/MonteCarlo.Rc")

simpath <- "./data"

#profiler <- profvis(prof_output="barg_calibrate.Rprof",{
#profiler <- microbenchmark::microbenchmark(times=1,{
#Rprof(filename = "Rprof.out", append = FALSE, interval = 2, line.profiling=TRUE)

cl <- makeCluster(mc <- getOption("cl.cores", 45))
## to make this reproducible
clusterSetRNGStream(cl, 3141519)

clusterEvalQ(cl,{
             if(require(RevoUtilsMath)) setMKLthreads(1)
})

samples <- 1e3
ndfirms <- 2:5
nufirms <- ndfirms
nvfirms <- 0:4
nestParm <- c(0)
type=c("1st")
merger=c("up","down", "vertical","both")
bargparm <- seq(.1,.9,.1)
mc=c("constant","linprod"#, "quadprod","linquad","quadlin","lincons","conslin"#,"linfirm"
     )
relleveragePre <-  (1-bargparm)/bargparm



genMkts <- function(x,y,z,t,m,n, b,c, large=TRUE, bargpreset="none"){





  thismkt <- try(market(supply.down=t,
                                                      nfirms.down = y,
                                                      nfirms.up = x,
                                                      nfirms.vert = as.numeric(as.character(z)),
                                                      #outMargin = runif(1,2,9),
                                                      outMargin=5,
                                                      nestParm = n,
                                                      shareOutDown = .15 ,
                                                      mcshare.up =rep(.25,x*y),
                                                      mcshare.down = rep(.1,x*y),
                                                      bargparm = rep(b,x*y),
                                                      ownerPost = m,
                                                      M=1,cost_type=as.character(c),
                                                      largeMerger = large,
                                                      bargpreset= bargpreset)
                 ,silent=TRUE)


  if(any(class(thismkt)=="try-error")){return(list(
    summary=thismkt,
    res=thismkt))}

isVert <- length(thismkt$vertical)>0


shareOutDown <- thismkt$down$shareOut
M <- thismkt$down$M
outMargin <- thismkt$down$outMargin
#thismkt <- calcSlopes(thismkt,constrain="global");


mcParmUp <- thismkt$up$mcParm
mcParmDown <- thismkt$down$mcParm

thissum <-try( summary(thismkt),silent=TRUE)
if(class(thissum)=="try-error"){return(list(
  summary=thissum,
  res=thissum))}
isMarket=try(HypoMonTest(thismkt),silent=TRUE)
if(class(isMarket)=="try-error") isMarket=NA
thissum$idVert <- thissum$idDown
thissum$idVert[thissum$idUp == 1] <- 1

sumshares.pre <- sum(thissum$shares.pre,na.rm=TRUE)
sumshares.post <- sum(thissum$shares.post,na.rm=TRUE)

mktrev.pre = sum(thissum$downPricePre*thissum$shares.pre,na.rm=TRUE)*M

thisres <- with(thissum,data.frame(
  type =t,
  down=y,
  up=x,
  vert=z,
  nestParm=n,
  merger=m,
  barg=b,
  mc=c,
  M=M,
  outMargin=outMargin,
  mktElast = mktElastPre[1],
  cv       =  cv[1],
  isMarket=isMarket,
  mktrev.pre = mktrev.pre,
  relmarginPre=relmarginPre[1],
  relmarginPartyPre=relmarginPartyPre[1],
  upPSPre=sum(upPSPre,na.rm=TRUE),
  upPSPost=sum(upPSPost,na.rm=TRUE),
  downPSPre=sum(downPSPre, na.rm=TRUE),
  downPSPost=sum(downPSPost,na.rm=TRUE),
  isProfitable.up=ifelse(isVert,
                         sum( upPSPost[idUp %in% 1:2], downPSPost[idDown %in% 2],  - upPSPre[ idUp %in% 1:2],- downPSPre[idDown %in% 2],na.rm=TRUE) >0,
                         sum( upPSPost[idUp %in% 1:2],  - upPSPre[ idUp %in% 1:2],na.rm=TRUE) >0),
  isProfitable.down=ifelse(isVert,
                           sum( upPSPost[idUp %in% 2],downPSPost[idDown %in% 1:2],  -upPSPre[idUp %in% 2],- downPSPre[ idDown %in% 1:2],na.rm=TRUE) >0,
                           sum( downPSPost[idDown %in% 1:2],  - downPSPre[ idDown %in% 1:2],na.rm=TRUE) >0),
  isProfitable.vert=sum( downPSPost[idDown==1], upPSPost[idUp==1],  - downPSPre[idDown==1] , - upPSPre[idUp==1],na.rm=TRUE) >0,
  isProfitable.both=sum( downPSPost[idDown%in% 1:2], upPSPost[idUp%in% 1:2],  - downPSPre[idDown%in% 1:2] , - upPSPre[idUp%in% 1:2],na.rm=TRUE) >0,
  avgpricepre.up = sum(thissum$upPricePre*shares.pre/sumshares.pre,na.rm=TRUE),
  avgpricepre.down = sum(thissum$downPricePre*shares.pre/sumshares.pre,na.rm=TRUE),
  # avgPartyCostParmUp.up = weighted.mean(mcParmUp[idUp %in% 1:2],shares.pre[idUp %in% 1:2] ),
  # avgPartyCostParmDown.down = weighted.mean(mcParmDown[idDown %in% 1:2],shares.pre[idDown %in% 1:2]),
  # avgPartyCostParmUp.vert = weighted.mean(mcParmUp[idUp ==1],shares.pre[idUp == 1]),
  # avgPartyCostParmDown.vert = weighted.mean(mcParmDown[idDown ==1],shares.pre[idDown == 1]),
  #avgpricedelta = sum(thissum$downPricePost*shares.post,na.rm=TRUE) + outMargin*(1-sumshares.post ) - sum(thissum$downPricePre*shares.pre/sumshares.pre,na.rm=TRUE) - outMargin*(1-sumshares.pre ),
  avgpricedelta = sum(thissum$downPricePost*shares.post/sumshares.post,na.rm=TRUE) - sum(thissum$downPricePre*shares.pre/sumshares.pre,na.rm=TRUE) ,
  hhidelta.down = 2 * prod(tapply(shares.pre/sumshares.pre * 100,idDown, sum,na.rm=TRUE)[1:2]),
  hhipre.down = sum(tapply(shares.pre/sumshares.pre * 100,idDown, sum)^2,na.rm=TRUE),
  hhidelta.up = 2 * prod(tapply(shares.pre/sumshares.pre * 100,idUp, sum,na.rm=TRUE)[1:2]),
  hhipre.up = sum(tapply(shares.pre/sumshares.pre * 100,idUp, sum)^2,na.rm=TRUE),
  hhipost.vert = sum(tapply(shares.pre/sumshares.pre * 100,idVert, sum)^2,na.rm=TRUE)
))

thisres$hhidelta <- with(thisres, ifelse(merger == "up", hhidelta.up,
                                         ifelse(merger == "vertical", hhipost.vert - hhipre.down,hhidelta.down)))
thisres$hhipre <- with(thisres, ifelse(merger != "up", hhipre.down,hhipre.up))
thisres$hhipost <- with(thisres, ifelse(merger == "vertical", hhipost.vert, hhipre + hhidelta))
thisres$hhipost.vert <-thisres$hhidelta.up <- thisres$hhidelta.down <- thisres$hhipre.up <- thisres$hhipre.down <- NULL

return(list(
  #mkt=thismkt,
  summary=thissum,
  res=thisres))}

repMkts <- function(x,y,z,t,m,n,b,c,large=TRUE){replicate(samples, genMkts(x,y,z,t,m,n,b,c,large),simplify = "list")}

clusterExport(cl,varlist = setdiff(ls(),c("cl")))





## Monte Carlos fixing the number of wholesalers and retailers, but changing the bargaining parameters

res.nests <- expand.grid(up=ndfirms,
                         down =nufirms ,
                         vert=nvfirms,
                         nestParm=nestParm,
                         barg = bargparm,
                         type=type,
                         merger=merger,
                         mc=mc)

#res.nests <- (filter(res.nests, down==1))
res.nests <- mutate(res.nests, type=as.character(type),
                    merger=as.character(merger)) %>%
  filter(!(merger %in% c("up") & (up ==1))) %>%
  filter(!(merger %in% c("down") & (down ==1))) %>%
  filter(vert < up & vert<down) %>%
  filter(!(merger %in% c("vertical") & (down == 1 & up ==1)))


mkts.nests <- clusterMap(cl, repMkts,
                         res.nests$up,res.nests$down,res.nests$vert, res.nests$type,res.nests$merger,res.nests$nestParm,
                         res.nests$barg,res.nests$mc, SIMPLIFY = FALSE,
                         .scheduling = "dynamic"
)

names(mkts.nests) <- with(res.nests,as.character(interaction(up,down,vert,type,merger,mc,barg,nestParm)))


res.nests <-  lapply(mkts.nests, function(x){
  x <- x["res",!sapply(x["res",],class) %in% "try-error"]
  if(length(x)>0) data.table::rbindlist(x)})


res.nests <- data.table::rbindlist(res.nests)
res.nests <- mutate(res.nests, up=factor(up), down=factor(down), vert=factor(vert),nestParm=factor(nestParm),
                    relleveragePre = (1- barg)/barg,barg=factor(barg),
                    type= ifelse(type == "1st", "Bertrand","2nd"),type=factor(type, levels=c("Bertrand","2nd")),
                    Retailers = down)

save(mkts.nests, file=file.path(simpath,"SimsNests.RData"))


rm(mkts.nests)


res.nests <- ungroup(res.nests) %>% group_by(merger,up,down,vert,type,nestParm,mc) %>%
  mutate(upPSDelta=(upPSPost- upPSPre),
         downPSDelta=(downPSPost - downPSPre),
         cv= -cv,
         totalDelta= cv + upPSDelta + downPSDelta,
         avgpricedelta=avgpricedelta,
         isProfitable=ifelse(merger=="up", isProfitable.up,
                             ifelse(merger=="down",isProfitable.down,
                                    ifelse(merger=="vert",isProfitable.vert,isProfitable.both))),
         relmarginPreCut=cut(relmarginPre,quantile(relmarginPre, probs=seq(0,1,.1),na.rm=TRUE), dig.lab=1, include.lowest = TRUE),
         cv001=quantile(cv,.05,na.rm=TRUE),
         cv999=quantile(cv,.95,na.rm=TRUE),
         cvtrun =ifelse(cv< cv001,cv001,cv))
res.nests$cvtrun <- with(res.nests, ifelse(cvtrun>cv999,cv999,cvtrun))

##eliminate markets where the hhipre is 0, isMarket==0
sink("isMarketSummary.txt")
print(table(res.nests$isMarket,useNA="ifany"))
print(table(res.nests$isMarket,res.nests$type,useNA="ifany"))
print(table(res.nests$isProfitable,res.nests$merger,useNA="ifany"))
print(with(res.nests[is.na(res.nests$isMarket) |! res.nests$isMarket,], table(up,down,useNA="ifany")))
sink()

## correlation between relmargin and relative bargaining by nest
#group_by(res.nests,nestParm) %>% mutate(barg=as.character(barg),relabarg=(1-as.numeric(barg))/as.numeric(barg)) %>%
#dplyr::summarize(rho=cor(relmarginPre,as.numeric(relabarg)))



res.nests <- filter(as.data.frame(res.nests),hhipre>0 #& as.logical(isMarket)
                    ) %>%
  filter(#nestParm =="0"  &
         #barg != "0.1" &
         isProfitable &
         avgpricepre.up>0 & up!="1" & down !="1"
         ) %>%
  mutate_if(is.factor,droplevels)

res.nest_all <- res.nests

res.nests <- filter(res.nests,nestParm =="0")

res.nests.long <- select(res.nests,type,down,up,vert,merger,barg,nestParm,mc,cv,upPSDelta,downPSDelta,totalDelta,relleveragePre,relmarginPreCut,
                        avgpricedelta,mktrev.pre) %>%
  rename(Consumer=cv, `Wholesaler`=upPSDelta, `Retailer`=downPSDelta,`Total`=totalDelta,  Wholesalers=up,
         Retailers=down) %>%
  gather(key="Outcome",value="Outcome_value",Consumer,`Wholesaler`,`Retailer`,`Total`) %>%
  mutate(Outcome=factor(Outcome,levels=c("Consumer","Retailer","Wholesaler","Total")))

res.nest_all.long <- select(res.nest_all,type,down,up,merger,barg,nestParm,mc,cv,upPSDelta,downPSDelta,totalDelta,relleveragePre,relmarginPreCut,
                            avgpricedelta,mktrev.pre) %>%
  rename(Consumer=cv, `Wholesaler`=upPSDelta, `Retailer`=downPSDelta,`Total`=totalDelta,  Wholesalers=up,
         Retailers=down) %>%
  gather(key="Outcome",value="Outcome_value",Consumer,`Wholesaler`,`Retailer`,`Total`) %>%
  mutate(Outcome=factor(Outcome,levels=c("Consumer","Retailer","Wholesaler","Total")))



sumtable <- data.frame(res.nests)



sumtable <- select(sumtable,merger,down, up,vert,barg,nestParm, hhipre, hhipost, hhidelta,  avgpricepre.up, avgpricepre.down, avgpricedelta,
                    mktElast) %>% distinct()  %>% mutate(
                                                                                         #mktCnt=length(up),
                                                                                         up = as.numeric(as.character(up)),
                                                                                         down=as.numeric(as.character(down)),
                                                                                         barg=as.numeric(as.character(barg)),
                                                                                         nestParm=as.numeric(as.character(nestParm)))#%>% select(-up,-down)


sumall <- select(sumtable,down, up,vert,barg,nestParm, avgpricepre.up, avgpricepre.down,mktElast) %>%
  gather(variable, value, factor_key = TRUE) %>% mutate(value=as.numeric(value)) %>% group_by(variable)%>%
  summarise(Min=quantile(value,probs=0,na.rm=TRUE),
            p25=quantile(value,probs=.25,na.rm=TRUE),
            p50=quantile(value,probs=.5,na.rm=TRUE),
            p75=quantile(value,probs=.75,na.rm=TRUE),
            Max=quantile(value,probs=1,na.rm=TRUE),
            Markets=length(value)) %>%
  mutate(merger="all")


sumtable <- select(sumtable , hhipre,hhipost, hhidelta, merger) %>%
  gather(variable, value, -merger, factor_key = TRUE) %>%
  group_by(merger,variable) %>%
  summarise(Min=quantile(value,probs=0,na.rm=TRUE),
                                                       p25=quantile(value,probs=.25,na.rm=TRUE),
                                                       p50=quantile(value,probs=.5,na.rm=TRUE),
                                                       p75=quantile(value,probs=.75,na.rm=TRUE),
                                                       Max=quantile(value,probs=1,na.rm=TRUE),
                                                       Markets=length(value)) %>% ungroup()

sumtable <- dplyr::bind_rows(sumall,sumtable)
sumtable <- gather(sumtable, quant, val, Min:Markets)
sumtable <- mutate(sumtable, val = ifelse(!variable %in% c("mktElast","barg","nestParm","avgpricepre.up","avgpricepre.down","cv","avgpricedelta", "upPSDelta","downPSDelta", "totalDelta"),round(val),round(val,2)))
sumtable <- spread(sumtable, quant, val) %>% select(-Max,Max) %>%
  mutate(
    #set=factor(set,levels=c("firm","bargaining"),labels=c("Firm Count","Bargaining Power")),
    merger=factor(merger, labels=c("All","Both","Upstream","Downstream","Vertical")),
    variable = factor(variable, levels=c("up","down","barg","nestParm",
                                         "avgpricepre.up","avgpricepre.down","mktElast",
                                         "hhipre","hhipost","hhidelta"),
                      labels = c("\\# Wholesalers","\\# Retailers","Bargaining Power","Nesting Parameter","Avg. Upstream Price ()","Avg. Downstream Price ()","Market Elasticity",
                                 "Pre-Merger HHI",
                                 "Post-Merger HHI","Delta HHI"
                      )
    )) %>%
  mutate(across(.fns=prettyNum, digits=2,big.mark=","))


#sumtable <- filter(sumtable, quant == "p50") %>% spread( merger, val) %>% mutate_all(funs(prettyNum(., digits=2,big.mark=",")))

sumout <- tabular(merger*Markets*variable*AllObs(sumtable)~p50+Min+p25+p75+Max, data=sumtable)
#sumout <- tabular(AllObs(sumtable)~ variable+up+down+vertical, data=sumtable)
latex(sumout)



#witholdfreq <- with(res.barg[merger=="vertical",],table(type,nsDown,useNA = "always"))
#colnames(witholdfreq) <- c("noUp","NoDown","All")


try(stopCluster(cl),silent=TRUE)
save.image(file.path(simpath,"AllSims.RData"))

