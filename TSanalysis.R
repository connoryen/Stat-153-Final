if (!require(astsa)) install.packages('astsa')
library(astsa)
if (!require(forecast)) install.packages('forecast')
library(forecast)
if (!require(FitAR)) install.packages('FitAR')
library(FitAR)
if (!require(Metrics)) install.packages('Metrics')
library(Metrics)
if (!require(pracma)) install.packages('pracma')
library(pracma)


localMaxima <- function(x) {
    'Find indecies of local maxima of a sequential list.
    Returns first index if x contains consecutive maxima. 

    Parameters
    ----------
    first: list of floats
        sequential y-vals of curve

    Returns
    -------
    list
        indecies of local maxima
    '

    if (length(x) == 0) return(c())

    y <- diff(c(-Inf, x)) > 0L
    rle(y)$lengths
    y <- cumsum(rle(y)$lengths)
    y <- y[seq.int(1L, length(y), 2L)]
    if (x[[1]] == x[[2]]) {
        y <- y[-1]
    }

    if (length(y) == 0) return(c())
    return(y)
}

pgram <- function(x){
    'Discrete periodigram for a time series via FFT. 
    index/length(x) gives the sinusoid frequencies 
    from the fourier frequencies. 

    Parameters
    ----------
    first: list of floats
        time series

    Returns
    -------
    list
        periodigram of time series and list of periodigram values.
    '
    m = floor(length(x)/2)
    pgram = abs(fft(x)[2:(m+1)])^2/length(x)
    plot(pgram, type = "h")
    abline(h=0)
    return(pgram)
}

specMax <- function(x) {
    'Compute the spectral density of a time series x. 
    Return frequencies of spectral maxima in descending order.
    Recall that spectral frequencies are the same as sinusoid frequencies. 

    Parameters
    ----------
    first: list of floats
        time series data

    Returns
    -------
    vector
        frequencies of spectral maxima
    '

    s <- mvspec(x)
    maxs.index <- localMaxima(s$spec)  # find indecies of local maxima
    maxs <- s$freq[maxs.index]  # frequency values of local maxima
    maxs.val <- s$spec[maxs.index]  # spec values of local maxima
    
    rv <- maxs[order(-maxs.val)]  # put maxs in decreasing order
    
    return(rv)
}

backshiftPolynomialCoefficients <- function(coefs, p=0,q=0, P=0,Q=0,S=1) {
    'Backshift polynomial coefficients for an ARMA expression. 
    Designed to be used with the SARIMA() function.

    Parameters
    ----------
    first: double matrix
        AR and MA coefficients. $fit$coef output of the SARIMA() 
        function. 
    second: double
        AR order
    third: double
        MA order
    fourth: double
        SAR order
    fifth: double
        SMA order
    sixth: double
        seasonality period

    Returns
    -------
    list
        first: AR
            coefficients of the backshift AR polynomial
        second: MA
            coefficients of the backshift MA polynomial
    '
    ARMAcoefficients <- list("AR" = c(), "MA" = c())
    ARpoly <- list("little" = c(), "big" = c())
    MApoly <- list("little" = c(), "big" = c())
    
    # non-seasonal coefficient polynomial. 
    # Polynomials for "little phis" and "little thetas":
    if(p > 0){
        for(i in c(1:p)){
            ARpoly$little <- c(ARpoly$little, coefs[i])
        }
    }
    ARpoly$little <- c(1, ARpoly$little)
    if(q > 0){
        for(i in c(1:q)){
            MApoly$little <- c(MApoly$little, coefs[i+p])
        }
    }
    MApoly$little <- c(1, MApoly$little)
    
    # seasonal coefficient polynomial. 
    # Polynomials for "big Phis" and "Big Thetas":
    if(P > 0){
        for(i in c(1:P)){
            ARpoly$big <- c(ARpoly$big, 
                            rep(0, (S-1)), coefs[i+p+q])
        }
    }
    ARpoly$big <- c(1, ARpoly$big)
    if(Q > 0){
        for(i in c(1:Q)){
            MApoly$big <- c(MApoly$big, 
                            rep(0, (S-1)), coefs[i+p+q+P])
        }
    }
    MApoly$big <- c(1, MApoly$big)
    
    # Full ARMA coefficients:
    ARcoef <- rev(polymul(rev(ARpoly$little), rev(ARpoly$big)))
    MAcoef <- rev(polymul(rev(MApoly$little), rev(MApoly$big)))
    
    RV <- list("AR" = ARcoef[-1], "MA" = MAcoef[-1])
    return(RV)
}