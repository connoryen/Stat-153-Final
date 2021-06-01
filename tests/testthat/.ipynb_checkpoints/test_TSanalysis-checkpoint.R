if (!require(testthat)) install.packages('testthat')
library(testthat)

source("../../TSanalysis.R")

test_that(
    "localMaxima", 
    {
        x <- c(1,2,3,4,5)
        expect_equal(localMaxima(x), c(5))
        
        x <- c()
        expect_equal(localMaxima(x), c())
        
        x <- c(1,1,1,1,1)
        expect_equal(localMaxima(x), c())
        
        x <- c(0.1, 0.2, 0.3, 0.3, 0.4, 0.9, -0.1, -0.05, -0.9)
        expect_equal(localMaxima(x), c(3,6,8))
        
        x <- c(1, 1 + 10**-10, 1)
        expect_equal(localMaxima(x), c(2))
    }
)

test_that(
    "pgram",
    {
        
    }
)

