##
runSimulation <- function(mu, sigma, n, R=100, M=1, monitor=10, returnRaw=TRUE)
{
  ## mu mean of Normal distribution from which samples are drawn
  ## sigma standard deviation of Normal distribution from which samples are drawn
  ## n sample size for each of the generated datasets
  ## R number of generated datasets within a simulation
  ## M number of simulations
  ## monitor keep a track of how the simulation is coming along
  ## returnRaw indicator of whether or not the raw output (i.e the sample mean and median)
  ## is returned for all MxR datasets
  ##
  rawOutput <- array(NA, dim=c(R, 2, M))
  simResults <- matrix(NA, nrow=M, ncol=4)
  rownames(simResults) <- paste("Simulation", 1:M)
  colnames(simResults) <- c("% Bias - muH", "% Bias - muT", "Rel. eff", "Rel. unc")
  ##
  for(m in 1:M)
  {
    ##
    if((m %% monitor) == 0) print(c(M, m))
    ##
    simData <- matrix(rnorm(R*n, mu, sigma), nrow=n)
    
    #Means of each of the R normal samples of size n
    rawOutput[,1,m] <- apply(simData, 2, mean)
    #Medians of each of the R normal samples of size n
    rawOutput[,2,m] <- apply(simData, 2, median)
    
    ##Operating Characteristics
    #(Empirical) Percent bias of sample mean
    simResults[m,1] <- (mean(rawOutput[,1,m]) - mu) / mu * 100
    #(Empirical) Percent bias of sample median
    simResults[m,2] <- (mean(rawOutput[,2,m]) - mu) / mu * 100
    #(Empirical) Relative efficiency: ratio of sample variances of median & mean
    simResults[m,3] <- var(rawOutput[,2,m]) / var(rawOutput[,1,m]) * 100
    #(Empirical) Relative uncertainty: ratio of sample standard deviations (square root of variances) of median & mean
    simResults[m,4] <- sd(rawOutput[,2,m]) / sd(rawOutput[,1,m]) * 100
  }
  ##
  #Since this output is a list, when you store output in variable simOutputs (e.g. or any other name works), 
  #you will extract raw output of R means and medians as simOutputs$rawOutput, and the matrix of 4 operating 
  #characteristics as simOutputs#$simResults
  if(returnRaw == TRUE) value <- list(rawOutput=rawOutput, simResults=simResults)
  if(returnRaw == FALSE) value <- simResults
  return(value)
}
