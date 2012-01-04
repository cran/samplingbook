stratasamp <-
function(n, Nh, Sh = NULL, Ch = NULL, type = 'prop')
{
  if (n < length(Nh))
    stop("Invalid input: the number of samples ", sQuote("n"), " has to be positiv.")
  if (!(is.numeric(Nh) && is.vector(Nh)))
    stop("Invalid input: ", sQuote("Nh"), " has to be a numeric vector.")
  if (any(Nh < 1))
    stop("Invalid input: population in ", sQuote("Nh"), " has to be positiv.")
  type <- match.arg(type,c('prop', 'opt', 'costopt'))  
  nh <- NA
  N <- sum(Nh)
  S <- sum(Sh)
  wh <- Nh/N  
  if (type == 'opt'){
    if ( length(Sh) != length(Nh) )
      stop("Invalid input: ", sQuote("Sh"), " and ", sQuote("Nh"), " must have the same length.")
    if (!(is.numeric(Sh) && is.vector(Sh)))
      stop("Invalid input: ", sQuote("Sh"), " has to be a numeric vector.")
    if (any(Sh < 0))
      stop("Invalid input: standard diviation in ", sQuote("Sh"), " can not be negativ.")
    for (i in 1:length(Nh))
      wh[i] <- (Nh[i] * Sh[i]) / sum(Nh*Sh)
  } 
  if(type == 'costopt'){
    if ( (length(Ch) != length(Nh)) || (length(Sh) != length(Nh)) )
      stop("Invalid input: ", sQuote("Ch"), ", ", sQuote("Sh"), " and ", sQuote("Nh"), " must have the same length.")
    if (!(is.numeric(Sh) && is.vector(Sh)))
      stop("Invalid input: ", sQuote("Ch"), " has to be a numeric vector.")
    if (any(Ch < 1))
      stop("Invalid input: cost in ", sQuote("Ch"), " can not be negativ.")
    for (i in 1:length(Nh))
    wh[[i]] <- (Nh[i] * Sh[i] / sqrt(Ch[i])) / sum(Nh*Sh/sqrt(Ch))
  }
  for (i in 1:length(Nh))
    nh[i] <- round(n * wh[i])
  if(any(nh < 2))
    warning("Warning: Observation less than 2 in a stratum is not recommended!") 
  res <- rbind(1:length(Nh), nh)
  rownames(res) <- c("Stratum","Size")
  colnames(res) <- c(rep("",length(Nh)))
  return(res)
}
