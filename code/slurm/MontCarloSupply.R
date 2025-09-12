## Code For Generating Fake
## Data for Monte Carlo Experiments



library(bayesm,quietly = TRUE)
library(tmvtnorm,quietly = TRUE)
library(dplyr,quietly = TRUE)
library(tidyr,quietly = TRUE)
library(cubature,quietly = TRUE)
library(pracma,quietly = TRUE)

## Create constructor function to generate markets
## For a given demand system
market<-function(supply=c("bertrand","cournot","2nd","barg","bargaining2nd"),
                 demand=c("logit","ces","log","linear","aids"),
                 nMarkets=2, 
                 nFirms=3,
                 M=1,
                 outPrice=100,
                 outMargin=  outPrice*runif(nMarkets,0.1,0.9 ),
                 #outCDF=c("pbeta","punif","pexp","pweibull","pgumbel","pfrechet"),
                 ownerPre=rep(1:nFirms,nMarkets),
                 ownerPost=c("global","partial"),
                 debtDelta=0,
                 nOverlaps=ifelse(ownerPost=="global",nMarkets,nMarkets-1),
                 shareOutParm=c("uniform","skew_left","skew_right","extreme"), # uniform # skew beta towards 0
                 shareOut=matrix(rbeta(nMarkets*nFirms,shape1 = shareOutParm[1], shape2 = shareOutParm[2]),nrow=nMarkets,ncol=nFirms), # mean of .5
                 #shareOut=runif(nMarkets,.1,.6),
                 sharesIn=t(replicate(nMarkets,{bayesm::rdirichlet(rep(2.5,nFirms))},simplify = "matrix")) , # dirichlet (share) distribution
                 Vslope = rep(0,nMarkets), # variance of slope parameter
                 Vsd = matrix(rep(0,4), nrow=2,ncol=2), # supply demand variance-covariance
                 mc = (outPrice - outMargin)*matrix(runif(nFirms*nMarkets,0.8,1.2),nrow=nMarkets,ncol=nFirms), #random  marginal costs centered at outside mc
                 ...){
  

  
  demand<-match.arg(demand)
  supply<-match.arg(supply)
  ownerPost <- match.arg(ownerPost)
  shareOutParm <- match.arg(shareOutParm)
  mergeType <- ownerPost
  
  
  
  
  shareOutParm <- switch(shareOutParm,
                         uniform=c(1,1),
                         skew_right=c(3,6),
                         skew_left=c(6,3),
                         extreme=c(.7,.7))
 
  
  hFocalIntegral <- function(u,s1=shareOutParm[1],s2=shareOutParm[2]){
    beta(s1+1,s2+1)/beta(s1,s2)*
      pbeta(u,shape1=s1+1,shape2=s2+1,lower.tail = TRUE)
    }

  gFocalIntegral <- function(u,s1=shareOutParm[1],s2=shareOutParm[2]){
    beta(s1,s2+1)/beta(s1,s2)*
      pbeta(u,shape1=s1,shape2=s2+1,lower.tail = TRUE)
  }
  
  #outCDF <- match.arg(outCDF)
  #lower.tail    <- TRUE
  #outPDF <- match.fun(paste("d",substring(outCDF,2),sep=""))
  
  #if(identical(outCDF,"punif")){outUpper <- 10}
  #else if (identical(outCDF,"pbeta")){outUpper <- 1}
  #else{outUpper <- Inf}
  #if(outCDF %in% c("pgumbel","pfrechet")){ lower.tail <- FALSE}
  
  if(nFirms<3){stop("'nFirms'>=3")}
  if(nOverlaps>nMarkets){stop("'nOverlaps' > 'nMarkets'")}
  
  ownerPost=c(1,1)
  if(nFirms>2) ownerPost<-c(ownerPost,3:nFirms)
  ownerPost <- rep(ownerPost, nMarkets)
  
  
  isParty<-ownerPost == 1
  
 
  if(mergeType!="global"){
    assign <- as.logical(rep(0:1,nMarkets)[1:nMarkets])
    fudge= 1e-3
    nNoOverlaps <- nMarkets-nOverlaps
    if(nOverlaps>0) assign[-(1:nNoOverlaps)] <- FALSE
    
    
    #Put Party 2 only in even and overlap markets and redistribute shares equally to 3rd parties
    sharesIn[assign,3:nFirms] <- sharesIn[assign,3:nFirms] + (sharesIn[assign,2] - fudge)/length(3:nFirms)
    sharesIn[assign,2] <- fudge
    #Remove Party 1 from market 2 and redistribute shares equally to 3rd parties
    if(nOverlaps>0) assign[-(1:nNoOverlaps)] <- TRUE
    sharesIn[!assign,3:nFirms] <- sharesIn[!assign,3:nFirms] + (sharesIn[!assign,1] - fudge)/length(3:nFirms)
    sharesIn[!assign,1] <- fudge
    
  }
  
  
  
  ## create a container to store upstream and downstream data
  mkt<-list(mergeType=mergeType,nMarkets = nMarkets,nOverlaps=nOverlaps,nFirms=nFirms,M=M,outMargin=outMargin,outPrice=outPrice,debtDelta=debtDelta,shareOutParm=shareOutParm,
            #outCDF=outCDF,outPDF=outPDF,lower.tail=lower.tail,outUpper=outUpper,
            Vslope=Vslope,Vsd=Vsd,isParty=isParty,ownerPre=ownerPre,ownerPost=ownerPost,
            shareOut=shareOut,sharesIn=sharesIn,mc=mc,hFocalIntegral=hFocalIntegral,gFocalIntegral=gFocalIntegral)
  
  theseclasses <- append(class(mkt),c(demand,supply))
  class(mkt)<- theseclasses
  
  mkt<-simMarket(mkt,...)
  class(mkt) <- theseclasses
  
  return(mkt)
  
}

## Define generic methods
simMarket <- function (mkt,...)  {
  UseMethod("simMarket", mkt)
}

FOC <- function (mkt,...)  {
  UseMethod("FOC", mkt)
}
summary <- function (mkt,...)  {
  UseMethod("summary", mkt)
}

calcShares <- function (mkt,...)  {
  UseMethod("calcShares", mkt)
}

calcQuantities <- function (mkt,...)  {
  UseMethod("calcQuantities", mkt)
}


calcPrices <- function (mkt,...)  {
  UseMethod("calcPrices", mkt)
}


elast <- function (mkt,...)  {
  UseMethod("elast", mkt)
}



CV <- function (mkt,...)  {
  UseMethod("CV", mkt)
}



simMarket.bertrand <- function(mkt){
  
  sharesIn=mkt$sharesIn
  Vslope=mkt$Vslope
  Vsd=mkt$Vsd
  shareOut=mkt$shareOut
  outMargin=mkt$outMargin
  outPrice=mkt$outPrice
  nMarkets <- mkt$nMarkets
  nFirms <- mkt$nFirms
  mc <- mkt$mc
  M <- mkt$M
  
  
  
  shareOutParm <- mkt$shareOutParm
  
  sharesBE <- sharesIn*(1-shareOut)

  
  
  demand <-intersect(class(mkt),c("logit","ces","aids") ) 

  
  if(isTRUE(demand =="logit")){
    
    slopes_mean <- -1/outMargin
    slopes <-  rnorm(n=nMarkets, mean=slopes_mean, sd= ifelse(Vslope ==0, 0,sqrt(Vslope)))
    if(any(slopes>0,na.rm=TRUE)) warning("some slopes are positive!")
  
      
    marginsBE <- -1/(slopes*(1-sharesBE))
    priceBE <- marginsBE+mc
    debtPre <-  colSums(M*marginsBE*sharesBE)
    
    mval <- log(sharesIn)- log(sharesIn[,1]) -slopes*(priceBE - priceBE[,1])
    z_crit <- log(shareOut) - log(sharesBE) -slopes*(outPrice - priceBE) + mval
  }
    mkt <- append(mkt, list(mval=mval,z_crit=z_crit,debtPre=debtPre,pricesBE=priceBE,
                            sharesBE=sharesBE,marginsBE=marginsBE,slopes=slopes) )
  
  return(mkt)
}
  
##Define system of FOC as a function of candidate Prices

FOC.default <- function(objCand,mkt,preMerger=TRUE,BE=FALSE,
                        sOnly=FALSE,
                        debtDelta=0,
                        method_foc=ifelse(mkt$nMarkets<2,"pcubature","hcubature"),
                        nVec=1024L,
                        relTol_foc=1e-5,
                        absTol_foc=relTol_foc*2,
                        maxEval_foc=10^6,
                        ...){
  
  nFirms <- mkt$nFirms 
  nMarkets <- mkt$nMarkets
  slopes <- mkt$slopes
  mval <- mkt$mval
  mc <- mkt$mc
  M <- mkt$M
  shareOutParm <- mkt$shareOutParm
  outPrice=mkt$outPrice
  focal=1 # set focal market to be market #1
  z_crit=mkt$z_crit
  debt=mkt$debtPre
  
  lowerB <- rep(0,nMarkets-1)
  upperB <- rep(1,nMarkets-1)
  
  if(preMerger){
    firmVec <- 1:nFirms
  }
  else{firmVec <- 2:nFirms
       debt[2] <- debt[2]*(1+debtDelta) #assign extra debt to firm2
  }
  
  
  bDens <- function(u,...) {dbeta(u, shape1 = shareOutParm[1],shape2 = shareOutParm[2],...)}
  bProb <- function(u,...) {pbeta(u, shape1 = shareOutParm[1],lower.tail=TRUE,shape2 = shareOutParm[2],...)}
  
  integrateDerivOtherFun <- function(u,f,pre=preMerger){s0=s0Focal(u,f,pre); u*(1-u)*bProb(s0)}
  integrateInsideOtherFun <- function(u,f,pre=preMerger){s0=s0Focal(u,f,pre); (1-u)*bProb(s0)}
  
  s0Focal <- function(sOut=rep(NA,nMarkets),i,pre=preMerger,n=nMarkets,l=focal){
    ## allow firm index f to be a vector of length>1.
    ## choose firm 2's product in market 1 to be the reference good
    if(!pre && any(i %in% 1:2)){i=2:1}
    if(n==1){ res <- 1- sum(debt[i])/sum(profitsCond[,i]); return(res)}
    res <- (sum(profitsCond[,i,drop=FALSE]) - sum(debt[i]))/profitsCond[l,i[1],drop=TRUE] - 
      sum(as.vector(sOut) * profitsCond[-l,i,drop=FALSE])/profitsCond[l,i[1],drop=TRUE]
    
  
    
    return(res)
  }
  
  hFocalIntegral <- mkt$hFocalIntegral
  gFocalIntegral <- mkt$gFocalIntegral
  

  
  
  
  
    p <- matrix(objCand,nrow=nMarkets,byrow=FALSE)
    margin <- p - mc
    
    sCond <- exp(mval + slopes * p)
    sOutBE <-exp(z_crit + slopes * outPrice)
    #sOutBE <- sOutBE/(sOutBE+rowSums(sCond))
    sOutBE <- sOutBE/(sOutBE+rowSums(sOutBE))
    sCond <- sCond/rowSums(sCond)
    profitsCond <- M*margin*sCond
  
    

    if(BE){
      
      sharesCand <- sCond*(1-sOutBE)
      if(sOnly){return(as.numeric(sharesCand))}
      
      
      if(!preMerger){
        if(nMarkets>1){sharesCand[,1:2] <- rowSums(sharesCand[,1:2])}
        else{sharesCand[,1:2] <- sum(sharesCand[,1:2])}
      }
 
      marginsCand <- -1/(slopes*(1-sharesCand))
      res <- as.vector(margin - marginsCand)
      
      return(res)
      
      
    }
      
    
    
    
    if(nMarkets ==1){hFun <- sapply(firmVec,function(q){s0_focal=s0Focal(NA,q);hFocalIntegral(s0_focal)})  }
    else if(shareOutParm[1] ==1 && shareOutParm[2] ==1){
      
      theseStates <-  expand.grid(lapply(1:(nMarkets-1),function(x){1:0}))
      
      hFun <- sapply(firmVec,function(q,pre=preMerger) {
        s0_focal <- apply(theseStates,1,s0Focal,i=q)
        if(!pre && q==2){q <- 2:1}
        
        betaStates <- 6/factorial(nMarkets+1)*s0_focal^(nMarkets+1)
        betaStates <- beta(2,2)*(betaStates - 12/factorial(nMarkets+2)*s0_focal^(nMarkets+2))
        
        
  
        
        
        st=ncol(theseStates)
        while(st>=1){
          lastState <- theseStates[,st]
          betaStates <- betaStates[lastState==1]-betaStates[lastState==0]
          theseStates <- unique(theseStates[,-st,drop=FALSE])
          st <- st-1
        }
        #betaStates <- rev((-1)^(1:length(betaStates)))*betaStates
        #betaStates <- sum(betaStates)
        thisProfits <- rowSums(profitsCond[,q,drop=FALSE])
        
        res <- (-1)^(nMarkets+1)*(prod(thisProfits[-1])/(thisProfits[1]^(nMarkets-1)))*betaStates
        
        return(res)
      })
      
    }
   
    else{
    hFun <- 
      sapply(firmVec,function(g) {cubintegrate(function(x,f=g){
        x <- as.matrix(x)
        matrix(apply(x, 2, function(s){
          s0_focal=s0Focal(s,f)
          res <- hFocalIntegral(s0_focal)*prod(bDens(s))
          return(res)
          }),ncol=col(x)
        )}
        ,lower=lowerB,upper=upperB,nVec=nVec,method= method_foc,relTol = relTol_foc, absTol = absTol_foc,maxEval = maxEval_foc)$integral
      })
    
    }
    
    if(nMarkets ==1){gFun <- sapply(firmVec,function(q){s0_focal=s0Focal(NA,q);gFocalIntegral(s0_focal)})  }
    else if(nMarkets >1 && shareOutParm[1] ==1 && shareOutParm[2] ==1){
      
      gFun <- sapply(firmVec,function(q,pre=preMerger) {
        s0_focal <- apply(theseStates,1,s0Focal,i=q)
        if(!pre && q==2){q <- 2:1}
  
        betaStates <- 2/factorial(nMarkets)*s0_focal^(nMarkets)
        betaStates <-  beta(1,2)*(betaStates - 2/factorial(nMarkets+1)*s0_focal^(nMarkets+1))
        
        st=ncol(theseStates)
        while(st>=1){
          lastState <- theseStates[,st]
          betaStates <- betaStates[lastState==1]-betaStates[lastState==0]
          theseStates <- unique(theseStates[,-st,drop=FALSE])
          st <- st-1
        }
        #betaStates <- rev((-1)^(1:length(betaStates)))*betaStates
        #betaStates <- sum(betaStates)
        thisProfits <- rowSums(profitsCond[,q,drop=FALSE])
        res <- (-1)^(nMarkets-1)*(prod(thisProfits[-1])/(thisProfits[1]^(nMarkets-1)))*betaStates
        
        return(res)
      })

    }
  
    else{
    gFun <- 
      sapply(firmVec,function(g) {cubintegrate(function(x,f=g){
        x <- as.matrix(x)
        matrix(apply(x, 2, function(s){
          s0_focal=s0Focal(s,f)
          #if(s0_focal==1){return(0)}
          gFocalIntegral(s0_focal)*prod(bDens(s))}),ncol=col(x)
        )}
        ,lower=lowerB,upper=upperB,nVec=nVec,method= method_foc,relTol = relTol_foc,absTol = absTol_foc, maxEval = maxEval_foc)$integral
      })
    }
    
 if(nMarkets >1){
   if(
     #nMarkets ==2 && 
     shareOutParm[1] ==1 && shareOutParm[2] ==1){
    
     
   
       theseStates <-theseStates[theseStates$Var1==1,,drop=FALSE]
       
     
     otherFun <- function(q,wgt=.5,pre=preMerger) {
       s0_focal <- apply(theseStates,1,s0Focal,i=q)
       if(!pre && q==2){q <- 2:1}
       thisProfits <- rowSums(profitsCond[,q,drop=FALSE])
       betaStates <- s0_focal^(nMarkets - 1)/factorial(nMarkets - 1)
     
       st=ncol(theseStates)
       while(st>=2){
         lastState <- theseStates[,st]
         betaStates <- betaStates[lastState==1]-betaStates[lastState==0]
         theseStates <- unique(theseStates[,-st,drop=FALSE])
         st <- st-1
       }
      
       if(nMarkets==2){res <- betaStates}
       else{res <- (-1)^(nMarkets-1)*(prod(thisProfits[-(1:2)])/(thisProfits[1]^(nMarkets-2)))*betaStates}
       
       res <- res + wgt*(thisProfits[2]/thisProfits[1])
       return(res)
     }
     
     tFun <- beta(2,2)*sapply(firmVec,otherFun,wgt= 1/2)
     rFun <- beta(1,2)*sapply(firmVec,otherFun,wgt= 2/3)
     
   
     
     
   }
     
     
   
   else{
    tFun <- 
      sapply(firmVec,function(g) {cubintegrate(function(x,f=g){
        x <- as.matrix(x)
        matrix(apply(x, 2, function(s){
          
          integrateDerivOtherFun(s,f)*prod(bDens(s))}),ncol=col(x)
        )}
        ,fDim=1,lower=lowerB,upper=upperB,nVec=nVec,method= method_foc,relTol = relTol_foc, absTol = absTol_foc,maxEval = maxEval_foc)$integral
      })
    
    rFun <- 
      sapply(firmVec,function(g) {cubintegrate(function(x,f=g){
        x <- as.matrix(x)
        matrix(apply(x, 2, function(s){
          
          integrateInsideOtherFun(s,f)*prod(bDens(s))}),ncol=col(x)
        )}
        ,fDim=1,lower=lowerB,upper=upperB,nVec=nVec,method= method_foc,relTol = relTol_foc,absTol = absTol_foc, maxEval = maxEval_foc)$integral
      })
    
   }
    
    #rFun[rFun==0] <- 1e-6
    shareOutOther <- tFun/rFun
    shareOutOther[shareOutOther>1] <- 1
    shareOutOther[shareOutOther<0] <- 0
 }

    #gFun[gFun==0] <- 1e-6
    shareOutFocal <- matrix(hFun/gFun,nrow=1)
    shareOutFocal[shareOutFocal > 1] <- 1
    shareOutFocal[shareOutFocal < 0] <- 0
    #if(nMarkets==2){shareOutOther <- matrix(shareOutOther,nrow=nMarkets - 1)}
  
    
     if(!preMerger){
       shareOutFocal <- c(shareOutFocal[1],shareOutFocal)
       if(nMarkets>1) shareOutOther <- c(shareOutOther[1],shareOutOther)
       #if(nMarkets>1) shareOutOther <- cbind(shareOutOther[,1],shareOutOther)
     }
    
    SharesOutInt <- matrix(NA,nrow=nMarkets,ncol=nFirms)
    SharesOutInt[focal,]=shareOutFocal
    if(nMarkets>1) SharesOutInt[-focal,]=shareOutOther
    
    
    if(sOnly){
      return(as.numeric(sCond*(1-SharesOutInt)))
    }
    if(!preMerger){
      if(nMarkets>1)  sCond[,1:2] <- rowSums(sCond[,1:2])
      else{ sCond[,1:2] <- sum(sCond[,1:2])}
      }
    marginCand <- -1/(slopes*(1 - sCond*(1-SharesOutInt) ))
    
    
    #return(sum(as.vector((marginCand - margin)^2)))
    return(as.vector(marginCand - margin))
    
  }
  
  

## Compute equilibrium prices

calcPrices.default <- function(mkt,
                               preMerger=TRUE,BE=FALSE,sOnly=FALSE,
                               pricesStart=as.vector(mkt$pricesBE)*(1+debtDelta),
                               method_foc=ifelse(mkt$nMarkets<2,"pcubature","hcubature"),
                               relTol_foc=1e-5,
                               absTol_foc=relTol_foc*2,
                               maxEval_foc=10^6,
                               debtDelta=0,
                               ...){
  
  
  #res <- BB::BBsolve(optStart,FOC,mkt=mkt,mktid=mktid,preMerger=preMerger,BE=TRUE,...)
  res <- try(nleqslv::nleqslv(pricesStart,FOC,mkt=mkt,preMerger=preMerger,method_foc=method_foc,absTol_foc = absTol_foc,BE=BE,debtDelta=debtDelta,...),silent=FALSE)
  
  if(class(res)=="try-error"){
    if(method_foc=="pcubature"){ res <- try(nleqslv::nleqslv(pricesStart,FOC,mkt=mkt,preMerger=preMerger,method_foc="hcubature",absTol_foc = absTol_foc,BE=BE,debtDelta=debtDelta,...),silent=FALSE)
    }
    
    if(class(res)=="try-error") {return(rep(NA,mkt$nFirms*mkt$nMarkets))}
  }
  
  # if(res$termcd %in% 1:2 ||
  #    res$termcd %in% 3 & max(res$fvec)<1e-3
  # ){
  #   
     return(res$x)
  #   
  #   }
  
  #else{return(rep(NA,length(optStart)))}
  
 
  
  return(res)
}


summary.bertrand <- function(mkt,market=FALSE,debtDelta=mkt$debtDelta,...){
  



  nMarkets <- mkt$nMarkets
  nFirms <- mkt$nFirms
  

  priceBEPre <- calcPrices(mkt,preMerger=TRUE,BE=TRUE,...)
  priceBEPost <- calcPrices(mkt,preMerger=FALSE,BE=TRUE,...)
  
  
  vecErr <- rep(NA,nMarkets*nFirms)
  
  pricePre <- calcPrices(mkt,preMerger=TRUE,BE=FALSE,...)
  pricePost <- sapply(debtDelta, function(d) {calcPrices(mkt,preMerger=FALSE,
                                                         BE=FALSE,
                                                         pricesStart=as.vector(priceBEPost),
                                                         debtDelta=d,...)},simplify="matrix")
  
  postMissing <- as.logical(apply(pricePost,2, function(x){any(is.na(x))}))
  if(any(postMissing)){
  pricePost[,postMissing] <- sapply(debtDelta[postMissing], function(d) {calcPrices(mkt,preMerger=FALSE,
                                                                       BE=FALSE,
                                                                       pricesStart=as.vector(priceBEPost)*(1+d),
                                                                       debtDelta=d,...)},simplify="matrix")
  }
  if(length(debtDelta)>1){
    colnames(pricePost) <- paste0("pricePost",debtDelta*100)
  }
  if(any(is.na(pricePre))){sharesPre <- vecErr}
  else{sharesPre <- FOC(pricePre,mkt,preMerger=TRUE,BE=FALSE,sOnly=TRUE,...)}

  #if(any(is.na(pricePost))){sharesPost <- vecErr}

  
  #else{
    if(length(debtDelta)>1){
    sharesPost <- sapply(1:length(debtDelta), function(d){
      if(any(is.na(pricePost[,d]))){return(rep(NA,length(pricePost[,d])))}
      FOC(pricePost[,d],mkt,preMerger=FALSE,BE=FALSE,sOnly=TRUE,debtDelta=debtDelta[d],...)},simplify="matrix")      
    colnames(sharesPost) <- paste0("sharesPost",debtDelta*100)
    }
    else{sharesPost <- FOC(as.vector(pricePost),mkt,preMerger=FALSE,BE=FALSE,sOnly=TRUE,debtDelta=debtDelta,...)
    }
  #}
  
    
    
  
  sharesBEPre <- FOC(priceBEPre,mkt,preMerger=TRUE,BE=TRUE,sOnly=TRUE)
  sharesBEPost <- FOC(priceBEPost,mkt,preMerger=FALSE,BE=TRUE,sOnly=TRUE)
  
  debtPost <-  sapply(1:length(debtDelta), function(d){debt <- mkt$debtPre;debt[2] <- debt[2]*(1+debtDelta[d]);return(debt)},simplify = "matrix")

      debtPost <- debtPost[rep(1:nrow(debtPost),each=nMarkets),,drop=FALSE]
       
      if(ncol(debtPost)>1) colnames(debtPost) <- paste0("debtPost",debtDelta*100)
  
      
  

  outAll <- cbind.data.frame(type=mkt$mergeType,
                             M=mkt$M,
                             nOverlaps=mkt$nOverlaps,
                             mkt=rep(1:nMarkets,nFirms),
                             firmid=rep(1:nFirms,each=nMarkets),
                             shape1=mkt$shareOutParm[1],
                             shape2=mkt$shareOutParm[2],
                             pricePre,pricePost,sharesPre,sharesPost,
                             priceBEPre,priceBEPost,sharesBEPre,sharesBEPost,
                             sharesIn=as.numeric(mkt$sharesIn),
                             mc=as.numeric(mkt$mc),
                             debtPre=rep(mkt$debtPre,each=nMarkets),debtPost)
 outAll <- pivot_longer(outAll,starts_with(c("pricePost","sharesPost","debtPost")),names_to = "out",values_to = "value") %>%
             mutate(debtDelta=gsub("debtPost|pricePost|sharesPost","",out,perl=TRUE),out=gsub("-?\\d+","",out,perl=TRUE)) %>% pivot_wider(names_from="out")
 
 outAll$debtDelta[outAll$debtDelta==""]=debtDelta*100
 
  outAll <- relocate(outAll,debtDelta,.after="shape2") %>% relocate(debtPre,.before=debtPost)
  
  if(market){
    debtsum <- filter(outAll,firmid %in% 1:2) %>% 
      group_by(type,M,nOverlaps,mkt,debtDelta,shape1,shape2) %>% 
      dplyr::summarize(debt=sum(debtPost)) %>% ungroup()
    
    profitable <- filter(ungroup(outAll),firmid %in% 1:2) %>% 
      #group_by(type,M,nOverlaps,mkt,shape1,shape2) %>% 
      dplyr::summarise(profitsPost= sum(M*sharesPost * (pricePost-mc)),
                profitsPre= sum(M*sharesPre * (pricePre-mc)),
                profitsBEPost= sum(M*sharesBEPost * (priceBEPost-mc)),
                profitsBEPre= sum(M*sharesBEPre * (priceBEPre-mc))
                ) %>%
      mutate(leverage=profitsPre,
             isProfitable=(profitsPost-profitsPre)>0,
             isProfitableBE=(profitsBEPost-profitsBEPre)>0) %>% 
      select(starts_with("isProfit"),leverage) %>% ungroup()
    hhisum <- group_by(outAll,M,nOverlaps,mkt,debtDelta,shape1,shape2)  %>% dplyr::summarize(nFirms=n(),
                                              AvgPrePrice=sum(sharesIn*pricePre),
                                               AvgPrePriceBE=sum(sharesIn*priceBEPre),
                                               paasche=sum(sharesPost * pricePost)/sum(sharesPost * pricePre) - 1,
                                               paascheBE=sum(sharesBEPost * priceBEPost)/sum(sharesBEPost * priceBEPre) - 1,
                                               `Pre-Merger HHI` =sum((sharesIn*100)^2),
                                               `HHI Change`=2*prod(sharesIn[firmid %in% 1:2]*100),
                                              `Pre-Merger Debt HHI` =sum((debtPre/sum(debtPre)*100)^2),
                                              `HHI Debt Change`=2*prod((debtPre/sum(debtPre))[firmid %in% 1:2]*100)) %>%
    
    mutate( `Post-Merger HHI`= `Pre-Merger HHI`+`HHI Change`,
            `Post-Merger Debt HHI`= `Pre-Merger Debt HHI`+`HHI Debt Change`) %>% ungroup()
    
    outAll <- inner_join(debtsum,hhisum,by = c("M", "nOverlaps", "mkt", "debtDelta", "shape1", "shape2"))
    outAll <- bind_cols(outAll,profitable) %>% ungroup() %>% mutate(leverage=leverage/debt)
    
  } 
  
   
   return(outAll) 
   
}

PS.logit <- function(mkt,prices){
  ## compute producer surplus given model parameters
  ## and user-supplied prices
  ps <- (prices - mkt$mc)*calcShares(mkt,prices)
  return(ps)
}




