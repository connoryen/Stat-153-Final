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
        n=100; t = 1:n; 
        x = cos(2*pi*t*(5/n))
        x_pgram <- pgram(x, FALSE)
        expect_equal(which.max(x_pgram)/100, 5/100)
        
        n=100; t = 1:n; 
        x = cos(2*pi*t*(1/3)) + sin(2*pi*t*(1/3))
        x_pgram <- pgram(x, FALSE)
        expect_equal(which.max(x_pgram), 33)
        
        n=100; t = 1:n; 
        x = cos(2*pi*t*(1/6)) + sin(2*pi*t*(1/3))
        x_pgram <- pgram(x, FALSE)
        ordered_pgram = sort(x_pgram, decreasing = TRUE)
        peaks <- c(which(x_pgram == ordered_pgram[1]), 
                   which(x_pgram == ordered_pgram[2]))
        expect_equal(peaks, c(17, 33))
    }
)

