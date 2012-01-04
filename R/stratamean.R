stratamean <-
function(y, h, Nh, wh, level = 0.95, eae = FALSE, data = NULL)
{
  if (!is.null(data)){
    target = y
    strata = h
  }
  else{
    if (!(is.numeric(y) && is.vector(y)))
      stop("Invalid input: ", sQuote("y"), " has to be a numeric vector.")
    if (!is.vector(h))
      stop("Invalid input: ", sQuote("h"), " has to be a vector.")
    if ( length(y) != length(h) )
      stop("Invalid input: ", sQuote("y"), " and ", sQuote("h"), " must have the same length.")    
    data <- as.data.frame(cbind(target=y,strata=h))
    target = names(data)[1]
    strata = names(data)[2]
  }
  n <- length(unique(data[, strata]))    
  if ( missing(Nh) == missing(wh) )  
    stop("Invalid input: only ", sQuote("Nh"), " or ", sQuote("wh"), " valid inputs." )   
  if (missing(Nh)){
    if( n != length(wh) )
      stop("Invalid input: the number of strata given in ", sQuote("wh"), " is not valid according the sample data.")
    if ( sum(wh) != 1)
      stop("Invalid input: the sum of ", sQuote("wh"), " must equal 1.")
    Nh = NA
  }
  else{
    if ( n != length(Nh) )
      stop("Invalid input: the number of strata given in ", sQuote("Nh"), " is not correct according the sample data.")
  }
  if ( level < 0 || level > 1 )
  	stop("Invalid input: ", sQuote("level"), " has to be probability between 0 and 1.")
  if (!is.data.frame(data))
    stop("Invalid input: ", sQuote("data"), " has to be in data.frame-format.") 
  if (any(table(data[, strata]) < 2))
		stop("Invalid input: stratum with only one observation was entered.")
  if (any(is.na(data))){
		data <- na.omit(data)
    warning("NA imported, these will be ignored in calculations.")
  }
	q <- qnorm((1 + level) / 2)
	fpc <- FALSE 
	splitted <- split(data, data[, strata])	
	if (missing(wh)){
		N <- sum(Nh)
		wh <- Nh/N
		fpc <- TRUE
	}
	CIuh <- CIoh <- Varh <- Meanh <- vector(length = length(splitted)) 	
	for( i in 1:n ) {
		if (fpc){
			size = Nh[i]
		}
		else{
			size = Inf
		}
    if (length(splitted[[i]][, target]) > size)
      stop("Invalid input: the population of a stratum can not be less than the number of observations of this stratum")
		Smean.i <- Smean(y = as.numeric(as.vector(splitted[[i]][, target])), N = size)
		Meanh[i] <- Smean.i$mean
		Varh[i] <- Smean.i$se^2
		CIuh[i] <- Smean.i$ci[1]
		CIoh[i] <- Smean.i$ci[2]
  }  	
	Mean <- sum(Meanh*wh)
	Var <- sum(Varh*((wh)^2))
	CIo <- Mean + q*sqrt(Var)
	CIu <- Mean - q*sqrt(Var)
	eaes <- cbind(Mean = Meanh, SE = sqrt(Varh), CIu = CIuh, CIo = CIoh)
	rownames(eaes) <- sort(unique(data[, strata]))
  overall <- c(Mean, sqrt(Var), CIu, CIo)
  res <- rbind(eaes,overall)
	if (eae==FALSE){
		ret <- list()
    ret$call <- list(y = y, h = h, Nh = Nh, wh = wh, fpc = fpc, level = level)
    ret$mean <- Mean
    ret$se <- sqrt(Var)
    ret$ci <- c(CIu, CIo)
    structure(ret, class = "stratamean")   
	}
	else{
		return(res) 
	}
}
