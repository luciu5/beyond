rm(list=ls())
library(readr)
library(data.table)
library(kableExtra)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpmisc)

batchdir <- "./data/batch"
simpath <- "./data"

mktlevel <- list.files(path=batchdir ,pattern="ResMkt.*\\d.csv",full.names = TRUE)


mkts_flat <- lapply(mktlevel,fread,verbose=FALSE)
names(mkts_flat) <- gsub("./data/batch/|.csv","",mktlevel)
res_mkt <- rbindlist(mkts_flat,idcol="file")




res_mkt <- ungroup(res_mkt) %>%# group_by(merger,up,down,vert,type,preset,nestParm,mc) %>%
  mutate(upPSDelta=(upPSPost- upPSPre),
         downPSDelta=(downPSPost - downPSPre),
         cv= -cv,
         totalDelta= cv + upPSDelta + downPSDelta,
         avgpricedelta=avgpricedelta,
         isProfitable=ifelse(merger=="up", isProfitable.up,
                             ifelse(merger=="down",isProfitable.down,
                                    ifelse(merger=="vertical",isProfitable.vert,isProfitable.both)))
         #relmarginPreCut=cut(relmarginPre,quantile(relmarginPre, probs=seq(0,1,.1),na.rm=TRUE), dig.lab=2, include.lowest = TRUE)
  )


profitableMarkets <- filter(res_mkt,isMarket) %>% group_by(merger,vert,mc,preset,barg) %>% summarize(n=n(),isprofitable=sum(isProfitable))

res_mkt <- filter(as.data.frame(res_mkt),hhipre>0 & as.logical(isMarket)
) %>%
  filter(#nestParm =="0"  &
    #barg != "0.1" &
    isProfitable &
      avgpricepre.up>=0 & up!="1" & down !="1"
  ) %>%
  mutate_if(is.factor,droplevels)


firmlevel <- list.files(path=batchdir ,pattern="ResFirm.*\\d.csv",full.names = TRUE)


firms_flat <- lapply(firmlevel,fread,verbose=FALSE)
names(firms_flat) <- gsub("./data/batch/|.csv","",firmlevel)
res_firms <- rbindlist(firms_flat,idcol="file")


save(res_mkt,file="./data/resultsMkt.RData")
save(res_firms,file="./data/resultsFirm.RData")


rm(firms_flat,mkts_flat)
rm(res_firms)




res_mkt_all <- res_mkt


res_mkt <- filter(res_mkt,nestParm ==0 & preset=="none")

res_mkt.long <- select(res_mkt,type,down,up,vert,merger,barg,nestParm,mc,cv,upPSDelta,downPSDelta,totalDelta,relleveragePre,#relmarginPreCut,
                       avgpricedelta,mktrev.pre) %>%
  rename(Consumer=cv, `Wholesaler`=upPSDelta, `Retailer`=downPSDelta,`Total`=totalDelta,  Wholesalers=up,
         Retailers=down) %>%
  gather(key="Outcome",value="Outcome_value",Consumer,`Wholesaler`,`Retailer`,`Total`) %>%
  mutate(Outcome=factor(Outcome,levels=c("Consumer","Retailer","Wholesaler","Total")))

res_mkt_all.long <- select(res_mkt_all,type,down,up,vert,merger,barg,nestParm,mc,preset,cv,upPSDelta,downPSDelta,totalDelta,relleveragePre,#relmarginPreCut,
                           avgpricedelta,mktrev.pre) %>%
  rename(Consumer=cv, `Wholesaler`=upPSDelta, `Retailer`=downPSDelta,`Total`=totalDelta,  Wholesalers=up,
         Retailers=down) %>%
  gather(key="Outcome",value="Outcome_value",Consumer,`Wholesaler`,`Retailer`,`Total`) %>%
  mutate(Outcome=factor(Outcome,levels=c("Consumer","Retailer","Wholesaler","Total")))


## only summarize constant costs
sumtable <- data.frame(res_mkt) %>% filter(mc=="constant")



sumtable <- select(sumtable,merger,down, up,vert,barg,nestParm, hhipre, hhipost, hhidelta,  avgpricepre.up, avgpricepre.down, avgpricedelta,
                   mktElast)  %>% mutate(
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

options(scipen = 999)
sumtable <- spread(sumtable, quant, val) %>% select(-Max,Max) %>%
  mutate(
    #set=factor(set,levels=c("firm","bargaining"),labels=c("Firm Count","Bargaining Power")),
    merger=factor(merger, levels=c("all","vertical","up","down","both"),labels=c("All","Vertical","Upstream","Downstream","Integrated")),
    variable = factor(variable, levels=c("up","down","vert","barg",#"nestParm",
                                         "avgpricepre.up","avgpricepre.down","mktElast",
                                         "hhipre","hhipost","hhidelta"),
                      labels = c("# Wholesalers","# Retailers","# Integrated","Bargaining Power",#"Nesting Parameter",
                                 "Avg. Upstream Price ($)","Avg. Downstream Price ($)","Market Elasticity",
                                 "Pre-Merger HHI",
                                 "Post-Merger HHI","Delta HHI"
                      )
    ),
    Markets=as.numeric(Markets)) %>%
  mutate(across(where(is.numeric),.fns=prettyNum, digits=2,big.mark=",")) %>%
  rename(Variable=variable,Merger=merger,`50th`=p50,`25th`=p25,`75th`=p75) %>%arrange(Variable,Merger)

sumtable <- filter(sumtable,!is.na(Variable))
sink("./doc/sumtable.tex")
print(kable(sumtable,format="latex",digits=0) %>% collapse_rows(columns = 1:3, latex_hline = "major", valign = "middle"))
sink()


save.image(file.path(simpath,"AllSims.RData"))


