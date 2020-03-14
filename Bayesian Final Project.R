
#Import CSV
naep <- read.csv("naep.csv", colClasses = c("integer", "factor", "character", "factor", "integer" ))
str(naep)

naep <- subset(naep, AVG_SCORE != "—")
naep <- subset(naep, AVG_SCORE != "‡")
str(naep)
summary(naep)

naep$AVG_SCORE <- as.numeric(naep$AVG_SCORE)
summary(naep)
str(naep)

states <- read.csv("states.csv", header=TRUE)
summary(states)
states$perpupil <- states$TOTAL_EXPENDITURE/states$ENROLL
states$bin <- cut(states$perpupil, 3, include.lowest=TRUE, labels=c("Low", "Med", "High"))
summary(states)


#Combine into one DataFrame

data <- merge(naep,states)
data <- subset(data, YEAR >= 2003)
data <- subset(data, TEST_YEAR == 4)
summary(data)
#Deal With Missing Values


datamath <- (subset(data, TEST_SUBJECT == "Mathematics"))
datareading <- (subset(data, TEST_SUBJECT == "Reading" ))
summary(datamath)
summary(datareading)
summary(data)

#Seperate Data Into Bins
datamath$proficiency <- NA

for (i in 1:nrow(datamath)){
  if (datamath[i,"AVG_SCORE"] >= 240){
    datamath[i,"proficiency"] = 1}
  else {
    datamath[i,"proficiency"] = 0}}
summary(datamath)

datareading$proficiency <- NA
for (i in 1:nrow(datareading)){
  if (datareading[i,"AVG_SCORE"] >= 220){
    datareading[i,"proficiency"] = 1}
  else {
    datareading[i,"proficiency"] = 0}}
summary(datareading)



#Create Analysis Data Frames
#Math Data Set 1

datamath1 <- c("bin", "proficiency")
datamath1 <- datamath[datamath1]
summary(datamath1)
names(datamath1) <- c("s", "y")
summary(datamath1)


datamath2 <- c("bin", "proficiency", "STATE")
datamath2 <- datamath[datamath2]
summary(datamath2)
names(datamath2) <- c("s", "y", "z")
summary(datamath2)

datareading1 <- c("bin", "proficiency")
datareading1 <- datareading[datareading1]
summary(datareading1)
names(datareading1) <- c("s", "y")
summary(datareading1)


datareading2 <- c("bin", "proficiency", "STATE")
datareading2 <- datareading[datareading2]
summary(datareading2)
names(datareading2) <- c("s", "y", "z")
summary(datareading2)

#Data Analysis

source("DBDA2E-utilities.R")

fileNameRoot = "fergusonproject_" 
graphFileType = "pdf"

#===============================================================================
genMCMC = function( datamath1, numSavedSteps=50000 , saveName=NULL , thinSteps=1 ,
                    runjagsMethod=runjagsMethodDefault , 
                    nChains=nChainsDefault ) { 
  require(rjags)
  require(runjags)
  
  #-----------------------------------------------------------------------------
  # THE DATA.
  yA = as.matrix(subset(datamath1, s=="Low", select=y)) 
  yB = as.matrix(subset(datamath1, s=="Low-Med", select=y))
  yC = as.matrix(subset(datamath1, s=="Med-High", select=y)) 

  
  # Specify the data in a list, for later shipment to JAGS:
  dataList = list(
    yA = yA,
    yB = yB,
    yC = yC,
    zA = sum(yA),
    zB = sum(yB),
    zC = sum(yC))
  
  #-----------------------------------------------------------------------------
  # THE MODEL.
  modelString = "
  model {
    zA ~ dbin( thetaA , length(yA) )
    zB ~ dbin( thetaB , length(yB) )
    zC ~ dbin( thetaC , length(yC) )

    thetaA ~ dbeta(1,1)
    thetaB ~ dbeta(1,1)
    thetaC ~ dbeta(1,1)
  }
  " # close quote for modelString
  writeLines( modelString , con="TEMPmodel.txt" )
  
  #-----------------------------------------------------------------------------
  # INTIALIZE THE CHAINS.
  # Initial values of MCMC chains based on data:
  initsList = function() {
    resampledZA = rbinom(1, size=length(yA) , prob=sum(yA)/length(yA) )
    resampledZB = rbinom(1, size=length(yB) , prob=sum(yB)/length(yB) )
    resampledZC = rbinom(1, size=length(yC) , prob=sum(yC)/length(yC) )
    
    thetaInitA = resampledZA/length(yA)
    thetaInitB = resampledZB/length(yB)
    thetaInitC = resampledZC/length(yC)
    
    
    return( list( thetaA=thetaInitA,
                  thetaB=thetaInitB,
                  thetaC=thetaInitC
                  
    ))
  }
  
  #-----------------------------------------------------------------------------
  # RUN THE CHAINS
  parameters = c( "thetaA", "thetaB", "thetaC") 
  adaptSteps = 500             # Number of steps to adapt the samplers
  burnInSteps = 500            # Number of steps to burn-in the chains
  
  useRunjags = TRUE
  if ( useRunjags ) {
    runJagsOut <- run.jags( method=runjagsMethod ,
                            model="TEMPmodel.txt" , 
                            monitor=parameters , 
                            data=dataList ,  
                            inits=initsList , 
                            n.chains=nChains ,
                            adapt=adaptSteps ,
                            burnin=burnInSteps , 
                            sample=ceiling(numSavedSteps/nChains) ,
                            thin=thinSteps ,
                            summarise=FALSE ,
                            plots=FALSE )
    codaSamples = as.mcmc.list( runJagsOut )
  } else {
    # Create, initialize, and adapt the model:
    jagsModel = jags.model( "TEMPmodel.txt" , data=dataList , inits=initsList , 
                            n.chains=nChains , n.adapt=adaptSteps )
    # Burn-in:
    cat( "Burning in the MCMC chain...\n" )
    update( jagsModel , n.iter=burnInSteps )
    # The saved MCMC chain:
    cat( "Sampling final MCMC chain...\n" )
    codaSamples = coda.samples( jagsModel , variable.names=parameters , 
                                n.iter=ceiling(numSavedSteps*thinSteps/nChains), 
                                thin=thinSteps )
  }  
  
  # resulting codaSamples object has these indices: 
  #   codaSamples[[ chainIdx ]][ stepIdx , paramIdx ]
  if ( !is.null(saveName) ) {
    save( codaSamples , file=paste(saveName,"Mcmc.Rdata",sep="") )
  }
  return( codaSamples )
}

#===============================================================================
plotMCMC = function( codaSamples , data ,
                     compVal=0.5 , rope=NULL , 
                     diffSList=NULL , diffCList=NULL , 
                     compValDiff=0.0 , ropeDiff=NULL , 
                     saveName=NULL , saveType="jpg" ) {
  # Now plot the posterior:
  mcmcMat = as.matrix(codaSamples,chains=TRUE)
  chainLength = NROW( mcmcMat )
  
  
  
  # Plot individual theta's and differences:
  if ( !is.null(diffSList) ) {
    for ( compIdx in 1:length(diffSList) ) {
      diffSVec = diffSList[[compIdx]]
      Nidx = length(diffSVec)
      openGraph(width=2.5*Nidx,height=2.0*Nidx)
      par( mfrow=c(Nidx,Nidx) )
      xLim = range(c( compVal, rope,
                      mcmcMat[,diffSVec] ))
      for ( t1Idx in 1:Nidx ) {
        for ( t2Idx in 1:Nidx ) {
          parName1 = diffSVec[t1Idx]
          parName2 = diffSVec[t2Idx]
          if ( t1Idx > t2Idx) {  
            # plot.new() # empty plot, advance to next
            par( mar=c(3,3,3,1) , mgp=c(2.0,0.7,0) , pty="s" )
            nToPlot = 700
            ptIdx = round(seq(1,chainLength,length=nToPlot))
            plot ( mcmcMat[ptIdx,parName2] , mcmcMat[ptIdx,parName1] , 
                   cex.main=1.25 , cex.lab=1.25 , 
                   xlab=diffSVec[t2Idx] , 
                   ylab=diffSVec[t1Idx] , 
                   col="skyblue" )
            abline(0,1,lty="dotted")
          } else if ( t1Idx == t2Idx ) {
            par( mar=c(3,1.5,3,1.5) , mgp=c(2.0,0.7,0) , pty="m" )
            postInfo = plotPost( mcmcMat[,parName1] , 
                                 compVal=compVal , ROPE=rope , 
                                 cex.main=1.25 , cex.lab=1.25 , 
                                 xlab=bquote(.(parName1)) ,
                                 main=diffSVec[t1Idx] ,  
                                 xlim=xLim )
          } else if ( t1Idx < t2Idx ) {
            par( mar=c(3,1.5,3,1.5) , mgp=c(2.0,0.7,0) , pty="m" )
            postInfo = plotPost( mcmcMat[,parName1]-mcmcMat[,parName2] , 
                                 compVal=compValDiff , ROPE=ropeDiff , 
                                 cex.main=1.25 , cex.lab=1.25 , 
                                 xlab=bquote("Difference of "*omega*"'s") , 
                                 main=paste( diffSVec[t1Idx] ,
                                             "-",diffSVec[t2Idx] ) )
          }
        }
      }
      if ( !is.null(saveName) ) {
        saveGraph( file=paste0(saveName,"OmegaDiff",compIdx), type=saveType)
      }
    }
  }
}




#------------------------------------------------------------------------------- 
# Generate the MCMC chain:
mcmcCoda = genMCMC( data=datamath1 , numSavedSteps=11000 , saveName=fileNameRoot ,
                    thinSteps=20 )

summary(datamath1)
View(datamath1)
#------------------------------------------------------------------------------- 
# Display diagnostics of chain, for specified parameters:
for ( parName in c( "thetaA","thetaB","thetaC","thetaD") ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName ,
            saveName=fileNameRoot , saveType=graphFileType )
}

#------------------------------------------------------------------------------- 
# Display posterior information:
plotMCMC( mcmcCoda , data=myData , 
          compVal=NULL ,
          diffSList=list( c("thetaA","thetaB") ,
                          c("thetaA","thetaC") , 
                          c("thetaB","thetaC"),
                          c("thetaC","thetaD")) ,
          compValDiff=0.0,
          saveName=fileNameRoot , saveType=graphFileType )
#------------------------------------------------------------------------------- 

