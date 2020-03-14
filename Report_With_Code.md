Using Bayesian Analysis to Examine the Impact of State Spending on
Student Acheivement
================
Taisha Ferguson
June 30, 2019

# Introduction and Motivation

The goal of this project is to examine the relationship between
individual state spending in the United States on education and the
achievement of students in each state. The data set for the project
comes from
[Kaggle.com](https://www.kaggle.com/noriuk/us-educational-finances#naep.csv)\[1\].
The data on state spending comes from the United States Census Bureau
Annual Survey of School System Finances. This annual survey includes
statistics on revenue, debt, and assets for all states, including the
District of Columbia\[2\]. The National Assesment of Educational
Progress (NAEP) is a national assessment that measures student
achievement in various subjects\[3\].

The following variables will be used to measure the relationship with
state spending and student achievement:

  - Average test scores for each state (obtained from NAEP)
  - Per student spending on education (obtained from Census Bureau
    Annual Survey of School System
Finances)

# Data and Data Preprossing

### Import Data

``` r
naep <- read.csv("naep.csv", colClasses = c("integer", "factor", "character", "factor", "integer" ))
str(naep)
```

    ## 'data.frame':    2305 obs. of  5 variables:
    ##  $ YEAR        : int  2017 2017 2017 2017 2017 2017 2017 2017 2017 2017 ...
    ##  $ STATE       : Factor w/ 52 levels "Alabama","Alaska",..: 1 2 3 4 5 6 7 8 9 11 ...
    ##  $ AVG_SCORE   : chr  "232.170687741509" "230.456277558902" "234.435788152091" "233.848143678937" ...
    ##  $ TEST_SUBJECT: Factor w/ 2 levels "Mathematics",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ TEST_YEAR   : int  4 4 4 4 4 4 4 4 4 4 ...

``` r
naep <- subset(naep, AVG_SCORE != "—")
naep <- subset(naep, AVG_SCORE != "‡")
naep$AVG_SCORE <- as.numeric(naep$AVG_SCORE)
summary(naep)
```

    ##       YEAR                       STATE        AVG_SCORE    
    ##  Min.   :1990   Alabama             :  45   Min.   :178.6  
    ##  1st Qu.:2003   Arizona             :  45   1st Qu.:224.9  
    ##  Median :2007   Arkansas            :  45   Median :246.5  
    ##  Mean   :2007   California          :  45   Mean   :248.3  
    ##  3rd Qu.:2013   Connecticut         :  45   3rd Qu.:269.2  
    ##  Max.   :2017   District of Columbia:  45   Max.   :300.6  
    ##                 (Other)             :1905                  
    ##       TEST_SUBJECT    TEST_YEAR   
    ##  Mathematics:1104   Min.   :4.00  
    ##  Reading    :1071   1st Qu.:4.00  
    ##                     Median :4.00  
    ##                     Mean   :5.96  
    ##                     3rd Qu.:8.00  
    ##                     Max.   :8.00  
    ## 

``` r
states <- read.csv("states.csv", header=TRUE)
summary(states)
```

    ##         STATE           YEAR          ENROLL        TOTAL_REVENUE     
    ##  Alabama   :  25   Min.   :1992   Min.   :  43866   Min.   :  465650  
    ##  Alaska    :  25   1st Qu.:1998   1st Qu.: 264514   1st Qu.: 2189504  
    ##  Arizona   :  25   Median :2004   Median : 649934   Median : 5085826  
    ##  Arkansas  :  25   Mean   :2004   Mean   : 917542   Mean   : 9102045  
    ##  California:  25   3rd Qu.:2010   3rd Qu.:1010532   3rd Qu.:10845163  
    ##  Colorado  :  25   Max.   :2016   Max.   :6307022   Max.   :89217262  
    ##  (Other)   :1125                  NA's   :51                          
    ##  FEDERAL_REVENUE   STATE_REVENUE      LOCAL_REVENUE     
    ##  Min.   :  31020   Min.   :       0   Min.   :   22093  
    ##  1st Qu.: 189958   1st Qu.: 1165776   1st Qu.:  715121  
    ##  Median : 403548   Median : 2537754   Median : 2058996  
    ##  Mean   : 767780   Mean   : 4223743   Mean   : 4110522  
    ##  3rd Qu.: 827932   3rd Qu.: 5055548   3rd Qu.: 4755293  
    ##  Max.   :9990221   Max.   :50904567   Max.   :36105265  
    ##                                                         
    ##  TOTAL_EXPENDITURE  INSTRUCTION_EXPENDITURE SUPPORT_SERVICES_EXPENDITURE
    ##  Min.   :  481665   Min.   :  265549        Min.   :  139963            
    ##  1st Qu.: 2170404   1st Qu.: 1171336        1st Qu.:  638076            
    ##  Median : 5242672   Median : 2658253        Median : 1525471            
    ##  Mean   : 9206242   Mean   : 4768010        Mean   : 2682587            
    ##  3rd Qu.:10744202   3rd Qu.: 5561959        3rd Qu.: 3222924            
    ##  Max.   :85320133   Max.   :43964520        Max.   :26058021            
    ##                                                                         
    ##  OTHER_EXPENDITURE CAPITAL_OUTLAY_EXPENDITURE
    ##  Min.   :  11541   Min.   :   12708          
    ##  1st Qu.: 103449   1st Qu.:  181507          
    ##  Median : 271704   Median :  510428          
    ##  Mean   : 429951   Mean   :  903467          
    ##  3rd Qu.: 517222   3rd Qu.:  966148          
    ##  Max.   :3995951   Max.   :10223657          
    ##  NA's   :51

### Format Data

NAEP Data:

  - Remove years before 2003
  - Remove tests years other than 4th Grade

Census Data:

  - Create variable for per student speding by dividing total speding by
    number of enrolled students
  - Divide pers student spending into bins (Low, Medium and High)

### Merge NAEP and Census Data

Merge NAEP and Census Data. The R merge function automatically removes
cenus data that does not align with NAEP testing years. The remaining
analysis years are (2003, 2005, 2007, 2009, 2011, 2013, and 2015)

``` r
states$perpupil <- states$TOTAL_EXPENDITURE/states$ENROLL
states$bin <- cut(states$perpupil, 3, include.lowest=TRUE, labels=c("Low", "Med", "High"))
data <- merge(naep,states)
data <- subset(data, YEAR >= 2003)
data <- subset(data, TEST_YEAR == 4)
summary(data)
```

    ##       YEAR             STATE       AVG_SCORE          TEST_SUBJECT
    ##  Min.   :2003   Alabama   : 14   Min.   :188.4   Mathematics:357  
    ##  1st Qu.:2005   Alaska    : 14   1st Qu.:221.1   Reading    :357  
    ##  Median :2009   Arizona   : 14   Median :228.7                    
    ##  Mean   :2009   Arkansas  : 14   Mean   :229.4                    
    ##  3rd Qu.:2013   California: 14   3rd Qu.:239.8                    
    ##  Max.   :2015   Colorado  : 14   Max.   :253.4                    
    ##                 (Other)   :630                                    
    ##    TEST_YEAR     ENROLL        TOTAL_REVENUE      FEDERAL_REVENUE  
    ##  Min.   :4   Min.   :  44179   Min.   :  801008   Min.   :  75856  
    ##  1st Qu.:4   1st Qu.: 272070   1st Qu.: 2778564   1st Qu.: 295968  
    ##  Median :4   Median : 665109   Median : 6706922   Median : 636436  
    ##  Mean   :4   Mean   : 945126   Mean   :11154793   Mean   :1022119  
    ##  3rd Qu.:4   3rd Qu.:1028259   3rd Qu.:12999357   3rd Qu.:1116670  
    ##  Max.   :4   Max.   :6307022   Max.   :78248042   Max.   :9990221  
    ##                                                                    
    ##  STATE_REVENUE      LOCAL_REVENUE      TOTAL_EXPENDITURE 
    ##  Min.   :       0   Min.   :   35182   Min.   :  805362  
    ##  1st Qu.: 1546600   1st Qu.: 1013658   1st Qu.: 2708776  
    ##  Median : 3232013   Median : 2523155   Median : 6715011  
    ##  Mean   : 5121749   Mean   : 5010925   Mean   :11266757  
    ##  3rd Qu.: 6339782   3rd Qu.: 5903450   3rd Qu.:12757599  
    ##  Max.   :42360470   Max.   :34941513   Max.   :78365958  
    ##                                                          
    ##  INSTRUCTION_EXPENDITURE SUPPORT_SERVICES_EXPENDITURE OTHER_EXPENDITURE
    ##  Min.   :  330698        Min.   :  239435             Min.   :  27608  
    ##  1st Qu.: 1431522        1st Qu.:  852145             1st Qu.: 131177  
    ##  Median : 3383909        Median : 2067538             Median : 356389  
    ##  Mean   : 5824046        Mean   : 3301162             Mean   : 502583  
    ##  3rd Qu.: 6899346        3rd Qu.: 3750687             3rd Qu.: 577867  
    ##  Max.   :41954260        Max.   :23501848             Max.   :3759373  
    ##                                                                        
    ##  CAPITAL_OUTLAY_EXPENDITURE    perpupil        bin     
    ##  Min.   :   20070           Min.   : 6.123   Low :448  
    ##  1st Qu.:  248505           1st Qu.: 9.596   Med :244  
    ##  Median :  618284           Median :11.238   High: 22  
    ##  Mean   : 1087787           Mean   :12.135             
    ##  3rd Qu.: 1086722           3rd Qu.:13.552             
    ##  Max.   :10223657           Max.   :29.617             
    ## 

### Seperate Data into Math and Reading Scores

``` r
datamath <- (subset(data, TEST_SUBJECT == "Mathematics"))
datareading <- (subset(data, TEST_SUBJECT == "Reading" ))
```

### Create Student Proficiency Variable

In this section the reading and math scores, the dependent variables,
are transformed into binomial variables based on each states annual
level of proficiency. The levels are as follows:

  - Math scores at 240 or above are labeled as 1 and below that is
    labeled as 0
  - Reading scores at 220 or above are labeled as 1 and below that is
    labeled as 0

These scores were chosen because they are a little above the median
value for each data set. NAEP has different thresholds for proficiency
for reading(238) and math(249) scores. The NAEP levels were not chosen
for this analysis due to thefact that the majority of data set is below
their proficiency cut off.

``` r
datamath$proficiency <- NA
for (i in 1:nrow(datamath)){
  if (datamath[i,"AVG_SCORE"] >= 240){
    datamath[i,"proficiency"] = 1}
  else {
    datamath[i,"proficiency"] = 0}}

datamath1 <- c("bin", "proficiency")
datamath1 <- datamath[datamath1]
summary(datamath1)
```

    ##    bin       proficiency    
    ##  Low :224   Min.   :0.0000  
    ##  Med :122   1st Qu.:0.0000  
    ##  High: 11   Median :0.0000  
    ##             Mean   :0.4874  
    ##             3rd Qu.:1.0000  
    ##             Max.   :1.0000

``` r
names(datamath1) <- c("s", "y")


datareading$proficiency <- NA
for (i in 1:nrow(datareading)){
  if (datareading[i,"AVG_SCORE"] >= 220){
    datareading[i,"proficiency"] = 1}
  else {
    datareading[i,"proficiency"] = 0}}

datareading1 <- c("bin", "proficiency")
datareading1 <- datareading[datareading1]
summary(datareading1)
```

    ##    bin       proficiency   
    ##  Low :224   Min.   :0.000  
    ##  Med :122   1st Qu.:0.000  
    ##  High: 11   Median :1.000  
    ##             Mean   :0.577  
    ##             3rd Qu.:1.000  
    ##             Max.   :1.000

``` r
names(datareading1) <- c("s", "y")
```

# Bayesian Analysis and Data Methods

The Bayesian Analysis method Markov chain Monte Carlo(MCMC) was used to
analyze the different distributions of student proficiency in math and
reading based on amount per student spending on education. Jags software
will be used to implement to the MCMC chains. Jags is a program that
automatically runs MCMC chains using Gibbs
sampling\[4\].

### Import Jags and Bayesian Functions from *Doing Bayesian Analysis* textbook.

Some of the functions used in the analysis below are imported to from
*Doing Bayesian Analysis* by John Kruschke. You can use this
[link](https://sites.google.com/site/doingbayesiandataanalysis/software-installation)
to download the files below and import functions.

``` r
source("DBDA2E-utilities.R")
```

    ## 
    ## *********************************************************************
    ## Kruschke, J. K. (2015). Doing Bayesian Data Analysis, Second Edition:
    ## A Tutorial with R, JAGS, and Stan. Academic Press / Elsevier.
    ## *********************************************************************

    ## Loading required package: coda

    ## Linked to JAGS 4.3.0

    ## Loaded modules: basemod,bugs

### Create Function to Generate MCMC Chain

In order to generate the MCMC chain, you have to specify the model that
will be used in the MCMC chains to derive the posterior distributions.
Because the dependent variable student proficiency is binary, we can use
the Binomial Distribution to model the likeihoood function. The
likelihood function gives the probability of proficiency with the
parameter theta. In this project theta follows a beta distribution with
alpha and beta equal to 1. Alpha and beta equal to one is a uniform
distribution that indicates that we do not have strong prior knowledge
about the bias of theta.

``` r
genMCMC = function( data , sName="s" , yName="y" ,  
                    numSavedSteps=50000 , saveName=NULL , thinSteps=1 ,
                    runjagsMethod=runjagsMethodDefault , 
                    nChains=nChainsDefault ) { 
  require(rjags)
  require(runjags)
  
  #The Data
  y = data[,yName]
  s = as.numeric(data[,sName]) 
  if ( any( y!=0 & y!=1 ) ) { stop("All y values must be 0 or 1.") }
  z = aggregate( y , by=list(s) , FUN=sum )$x
  N = aggregate( rep(1,length(y)) , by=list(s) , FUN=sum )$x
  Nsubj = length(unique(s))
  
  # Data in a list, for later shipment to JAGS:
  dataList = list(
    z = z ,
    N = N ,
    Nsubj = Nsubj
  )
  
  # The Model
  modelString = "
  model {
    for ( s in 1:Nsubj ) {
      z[s] ~ dbin( theta[s] , N[s] )
      theta[s] ~ dbeta(1,1) 
    }
    
  }
  " # close quote for modelString
  writeLines( modelString , con="TEMPmodel.txt" )
 
  
  # Intialize the Chains
  initsList = function() {
    thetaInit = rep(0,Nsubj)
    for ( sIdx in 1:Nsubj ) { 
      includeRows = ( s == sIdx ) 
      yThisSubj = y[includeRows]  
      resampledY = sample( yThisSubj , replace=TRUE ) 
      thetaInit[sIdx] = sum(resampledY)/length(resampledY) 
    }
    thetaInit = 0.001+0.998*thetaInit 
    return( list( theta=thetaInit ) )
  }
  
  # Run the Chains
  parameters = c( "theta") 
  adaptSteps = 500             
  burnInSteps = 500            
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
    jagsModel = jags.model( "TEMPmodel.txt" , data=dataList , inits=initsList , 
                            n.chains=nChains , n.adapt=adaptSteps )
    #Burn-in:
    cat( "Burning in the MCMC chain...\n" )
    update( jagsModel , n.iter=burnInSteps )
    # Saved MCMC chain:
    cat( "Sampling final MCMC chain...\n" )
    codaSamples = coda.samples( jagsModel , variable.names=parameters , 
                                n.iter=ceiling(numSavedSteps*thinSteps/nChains), 
                                thin=thinSteps )
  }  
  
  if ( !is.null(saveName) ) {
    save( codaSamples , file=paste(saveName,"Mcmc.Rdata",sep="") )
  }
  return( codaSamples )
}
```

### Create Function to Summarize the Result of the Chains

``` r
smryMCMC = function(  codaSamples , compVal=0.5 , rope=NULL , 
                      compValDiff=0.0 , ropeDiff=NULL , saveName=NULL ) {
  mcmcMat = as.matrix(codaSamples,chains=TRUE)
  Ntheta = length(grep("theta",colnames(mcmcMat)))
  summaryInfo = NULL
  rowIdx = 0
  for ( tIdx in 1:Ntheta ) {
    parName = paste0("theta[",tIdx,"]")
    summaryInfo = rbind( summaryInfo , 
                         summarizePost( mcmcMat[,parName] , compVal=compVal , ROPE=rope ) )
    rowIdx = rowIdx+1
    rownames(summaryInfo)[rowIdx] = parName
  }
  for ( t1Idx in 1:(Ntheta-1) ) {
    for ( t2Idx in (t1Idx+1):Ntheta ) {
      parName1 = paste0("theta[",t1Idx,"]")
      parName2 = paste0("theta[",t2Idx,"]")
      summaryInfo = rbind( summaryInfo , 
                           summarizePost( mcmcMat[,parName1]-mcmcMat[,parName2] ,
                                          compVal=compValDiff , ROPE=ropeDiff ) )
      rowIdx = rowIdx+1
      rownames(summaryInfo)[rowIdx] = paste0(parName1,"-",parName2)
    }
  }
  if ( !is.null(saveName) ) {
    write.csv( summaryInfo , file=paste(saveName,"SummaryInfo.csv",sep="") )
  }
  show( summaryInfo )
  return( summaryInfo )
}
```

### Create Funtion Plot the Results of MCMC Chain

``` r
plotMCMC = function( codaSamples , data , compVal=0.5 , rope=NULL , 
                     compValDiff=0.0 , ropeDiff=NULL , 
                     saveName=NULL , saveType="jpg" ) {
  y = data$y
  s = as.numeric(data$s) 
  #Plot Posteriors
  mcmcMat = as.matrix(codaSamples,chains=TRUE)
  chainLength = NROW( mcmcMat )
  Ntheta = length(grep("theta",colnames(mcmcMat)))
  openGraph(width=2.5*Ntheta,height=2.0*Ntheta)
  par( mfrow=c(Ntheta,Ntheta) )
  for ( t1Idx in 1:(Ntheta) ) {
    for ( t2Idx in (1):Ntheta ) {
      parName1 = paste0("theta[",t1Idx,"]")
      parName2 = paste0("theta[",t2Idx,"]")
      if ( t1Idx > t2Idx) {  
        par( mar=c(3.5,3.5,1,1) , mgp=c(2.0,0.7,0) )
        nToPlot = 700
        ptIdx = round(seq(1,chainLength,length=nToPlot))
        plot ( mcmcMat[ptIdx,parName2] , mcmcMat[ptIdx,parName1] , cex.lab=1.75 ,
               xlab=parName2 , ylab=parName1 , col="skyblue" )
      } else if ( t1Idx == t2Idx ) {
        par( mar=c(3.5,1,1,1) , mgp=c(2.0,0.7,0) )
        postInfo = plotPost( mcmcMat[,parName1] , cex.lab = 1.75 , 
                             compVal=compVal , ROPE=rope , cex.main=1.5 ,
                             xlab=parName1 , main="" )
        includeRows = ( s == t1Idx ) 
        dataPropor = sum(y[includeRows])/sum(includeRows) 
        points( dataPropor , 0 , pch="+" , col="red" , cex=3 )
      } else if ( t1Idx < t2Idx ) {
        par( mar=c(3.5,1,1,1) , mgp=c(2.0,0.7,0) )
        postInfo = plotPost(mcmcMat[,parName1]-mcmcMat[,parName2] , cex.lab = 1.75 , 
                            compVal=compValDiff , ROPE=ropeDiff , cex.main=1.5 ,
                            xlab=paste0(parName1,"-",parName2) , main="" )
        includeRows1 = ( s == t1Idx ) 
        dataPropor1 = sum(y[includeRows1])/sum(includeRows1) 
        includeRows2 = ( s == t2Idx ) 
        dataPropor2 = sum(y[includeRows2])/sum(includeRows2) 
        points( dataPropor1-dataPropor2 , 0 , pch="+" , col="red" , cex=3 )
      }
    }
  }
  if ( !is.null(saveName) ) {
    saveGraph( file=paste(saveName,"Post",sep=""), type=saveType)
  }
}
```

### Apply Functions to the Math Data Set

``` r
fileNameRoot = "Ferguson-" 
graphFileType = "pdf" 
#Generate MCMC Chain
mcmcCoda = genMCMC( data=datamath1 , numSavedSteps=50000 , saveName=fileNameRoot )
```

    ## Calling 3 simulations using the parallel method...
    ## Following the progress of chain 1 (the program will wait for all
    ## chains to finish before continuing):
    ## Welcome to JAGS 4.3.0 on Sat Mar 14 18:03:17 2020
    ## JAGS is free software and comes with ABSOLUTELY NO WARRANTY
    ## Loading module: basemod: ok
    ## Loading module: bugs: ok
    ## . . Reading data file data.txt
    ## . Compiling model graph
    ##    Resolving undeclared variables
    ##    Allocating nodes
    ## Graph information:
    ##    Observed stochastic nodes: 3
    ##    Unobserved stochastic nodes: 3
    ##    Total graph size: 11
    ## . Reading parameter file inits1.txt
    ## . Initializing model
    ## . Adapting 500
    ## -------------------------------------------------| 500
    ## ++++++++++++++++++++++++++++++++++++++++++++++++++ 100%
    ## Adaptation successful
    ## . Updating 500
    ## -------------------------------------------------| 500
    ## ************************************************** 100%
    ## . . Updating 16667
    ## -------------------------------------------------| 16650
    ## ************************************************** 100%
    ## * 100%
    ## . . . . Updating 0
    ## . Deleting model
    ## . 
    ## All chains have finished
    ## Simulation complete.  Reading coda files...
    ## Coda files loaded successfully
    ## Finished running the simulation

``` r
parameterNames = varnames(mcmcCoda) 
for ( parName in parameterNames ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName , 
            saveName=fileNameRoot , saveType=graphFileType )
}
#Get Summary Info
summaryInfo = smryMCMC( mcmcCoda , compVal=NULL ,
                        compValDiff=0.0 , saveName=fileNameRoot )
```

![](Bayesian-2_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

    ##                          Mean    Median        Mode     ESS HDImass
    ## theta[1]           0.38053157  0.380255  0.38244439 31038.7    0.95
    ## theta[2]           0.68548727  0.686555  0.68558353 29811.7    0.95
    ## theta[3]           0.46224575  0.461090  0.46685385 32399.8    0.95
    ## theta[1]-theta[2] -0.30495570 -0.305568 -0.30089000 30842.9    0.95
    ## theta[1]-theta[3] -0.08171417 -0.080153 -0.07235791 32306.8    0.95
    ## theta[2]-theta[3]  0.22324152  0.224413  0.21744984 32136.1    0.95
    ##                      HDIlow   HDIhigh CompVal PcntGtCompVal ROPElow
    ## theta[1]           0.318155  0.443347      NA            NA      NA
    ## theta[2]           0.601725  0.764815      NA            NA      NA
    ## theta[3]           0.204906  0.715698      NA            NA      NA
    ## theta[1]-theta[2] -0.407690 -0.203298       0       0.00000      NA
    ## theta[1]-theta[3] -0.341264  0.185956       0      28.59543      NA
    ## theta[2]-theta[3] -0.052872  0.482209       0      94.28611      NA
    ##                   ROPEhigh PcntLtROPE PcntInROPE PcntGtROPE
    ## theta[1]                NA         NA         NA         NA
    ## theta[2]                NA         NA         NA         NA
    ## theta[3]                NA         NA         NA         NA
    ## theta[1]-theta[2]       NA         NA         NA         NA
    ## theta[1]-theta[3]       NA         NA         NA         NA
    ## theta[2]-theta[3]       NA         NA         NA         NA

``` r
#Plot Posteriors
plotMCMC( mcmcCoda , data=datamath1 , compVal=NULL ,
          compValDiff=0.0 , 
          saveName=fileNameRoot , saveType=graphFileType )
#------------------------------------------------------------------------------- 
```

![](Bayesian-2_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

# Observations from Math Score Results

The results of the summary are described in terms of theta\[1\],
theta\[2\], theta\[3\]. These values correspond to the following per
student funding groups:

  - theta\[1\] = Low Per Student Funding Group ($6,000- $14,000 per
    student)
  - theta\[2\] = Medium Per Student Funding Group ($14,000 - $22,000 per
    student)
  - theta\[3\] = High Per Student Funding Group ($22,000-$30,000 per
    student)

The graphs of the MCMC summary chains show the accucuary of MCMC chains.
The param value, the ESS score, and the shrink factor all indicate that
the all of the samples chains were similar to each other and were
representative of the full sample space.

The graphs of the posteriors of theta\[1\], theta\[2\], and theta\[3\],
and the differences between the 3 indicate that there are in factor
major differences in student acheivement based on spending per student.
In particular, the change in probability of proficiency changes from a
mode of .38 to a mode of .69 when moving from low to moderate funding.
Interestingly, there is a decrease in the mode when moving from moderate
to high spending. The decrease from moderate to high is an indication of
other factors that may contribute to student acheivement beyond spending
per student.

## Apply Functions to Reading Data Set

``` r
fileNameRoot = "Ferguson2-" 
graphFileType = "pdf" 

#Generate MCMC Chain
mcmcCoda = genMCMC( data=datareading1 , numSavedSteps=50000 , saveName=fileNameRoot )
```

    ## Calling 3 simulations using the parallel method...
    ## Following the progress of chain 1 (the program will wait for all
    ## chains to finish before continuing):
    ## Welcome to JAGS 4.3.0 on Sat Mar 14 18:03:31 2020
    ## JAGS is free software and comes with ABSOLUTELY NO WARRANTY
    ## Loading module: basemod: ok
    ## Loading module: bugs: ok
    ## . . Reading data file data.txt
    ## . Compiling model graph
    ##    Resolving undeclared variables
    ##    Allocating nodes
    ## Graph information:
    ##    Observed stochastic nodes: 3
    ##    Unobserved stochastic nodes: 3
    ##    Total graph size: 11
    ## . Reading parameter file inits1.txt
    ## . Initializing model
    ## . Adapting 500
    ## -------------------------------------------------| 500
    ## ++++++++++++++++++++++++++++++++++++++++++++++++++ 100%
    ## Adaptation successful
    ## . Updating 500
    ## -------------------------------------------------| 500
    ## ************************************************** 100%
    ## . . Updating 16667
    ## -------------------------------------------------| 16650
    ## ************************************************** 100%
    ## * 100%
    ## . . . . Updating 0
    ## . Deleting model
    ## . 
    ## All chains have finished
    ## Simulation complete.  Reading coda files...
    ## Coda files loaded successfully
    ## Finished running the simulation

``` r
parameterNames = varnames(mcmcCoda)
for ( parName in parameterNames ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName , 
            saveName=fileNameRoot , saveType=graphFileType )
}

#Get Summary of Chains
summaryInfo = smryMCMC( mcmcCoda , compVal=NULL ,
                        compValDiff=0.0 , 
                        saveName=fileNameRoot )
```

![](Bayesian-2_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

    ##                         Mean    Median       Mode     ESS HDImass
    ## theta[1]           0.4645584  0.464444  0.4633705 32127.6    0.95
    ## theta[2]           0.7740160  0.775295  0.7715132 28719.1    0.95
    ## theta[3]           0.6149810  0.621395  0.6410906 31086.2    0.95
    ## theta[1]-theta[2] -0.3094576 -0.310413 -0.3121591 29688.5    0.95
    ## theta[1]-theta[3] -0.1504227 -0.156391 -0.1790795 31513.5    0.95
    ## theta[2]-theta[3]  0.1590349  0.153201  0.1270646 30877.5    0.95
    ##                      HDIlow   HDIhigh CompVal PcntGtCompVal ROPElow
    ## theta[1]           0.398494  0.528106      NA            NA      NA
    ## theta[2]           0.699037  0.845116      NA            NA      NA
    ## theta[3]           0.360509  0.857102      NA            NA      NA
    ## theta[1]-theta[2] -0.407244 -0.211558       0       0.00000      NA
    ## theta[1]-theta[3] -0.400914  0.114754       0      14.14172      NA
    ## theta[2]-theta[3] -0.092751  0.425979       0      87.74025      NA
    ##                   ROPEhigh PcntLtROPE PcntInROPE PcntGtROPE
    ## theta[1]                NA         NA         NA         NA
    ## theta[2]                NA         NA         NA         NA
    ## theta[3]                NA         NA         NA         NA
    ## theta[1]-theta[2]       NA         NA         NA         NA
    ## theta[1]-theta[3]       NA         NA         NA         NA
    ## theta[2]-theta[3]       NA         NA         NA         NA

``` r
#Plot Posteriors
plotMCMC( mcmcCoda , data=datareading1 , compVal=NULL , 
          compValDiff=0.0 , 
          saveName=fileNameRoot , saveType=graphFileType )
#------------------------------------------------------------------------------- 
```

![](Bayesian-2_files/figure-gfm/unnamed-chunk-13-2.png)<!-- -->

# Observations from Reading Score Results

The results of the reading data set are very similar to the results of
the math data set. The change in probability of proficiency changes from
a mode of .47 to a mode of .78 when moving from low to moderate funding
but decrease to a mode of .69 when moving to high spending.

# Conclusion

The goal of this project was to examine the relationship between
individual state spending in the United States on education and the
achievement of students in each state. Using the Bayesian analysis
method MCMC and Jags software, it was shown that inceasing spending per
student from a low range of $6,000-$14,000 per student to a moderate
range of $14,000-$22,000 is correlated with a significantly higher
probablilty of student proficiency in both math and reading. Suprisingly
however, increased spedinding from the moderate range to high range of
$22,000-$30,000 per student is correlated with a lower probabliltiy of
student proficiency in both math and reading.

# References

1.  Kruschke, J. K. (2015). Doing bayesian data analysis: A tutorial
    with R and BUGS. Beijing: Ji xie gong ye chu ban she.
2.  Garrard, R. (2018, August 29). U.S. Educational Finances. Retrieved
    from
    <https://www.kaggle.com/noriuk/us-educational-finances#naep.csv>
3.  NDE Core Web. (n.d.). Retrieved from
    <https://www.nationsreportcard.gov/ndecore/landing> 4.US Census
    Bureau. (2017, May 04). Annual Survey of School System Finances
    Tables. Retrieved from
    <https://www.census.gov/programs-surveys/school-finances/data/tables.html>

<!-- end list -->

1.  Garrard, R. (2018, August 29). U.S. Educational Finances. Retrieved
    from
    <https://www.kaggle.com/noriuk/us-educational-finances#naep.csv>

2.  NDE Core Web. (n.d.). Retrieved from
    <https://www.nationsreportcard.gov/ndecore/landing>

3.  US Census Bureau. (2017, May 04). Annual Survey of School System
    Finances Tables. Retrieved from
    <https://www.census.gov/programs-surveys/school-finances/data/tables.html>

4.  Kruschke, J. K. (2015). Doing bayesian data analysis: A tutorial
    with R and BUGS. Beijing:Ji xie gong ye chu ban she.
