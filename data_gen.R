rm(list=ls())

library(ordinal)

#set.seed(122229)
set.seed(422229)

k_dim <- 3000 # Number of engines
j_dim <- 3 # Number of gears in each engine
m_dim <- 3 # Number of repeated measurements
p_dim <- 3 # Number of covariates

gear_names <- c("IDLER", "LOX", "FUEL")
gear_type <- rep(gear_names, k_dim)
engine_number <- rep(1:k_dim, each=3)
dat <- data.frame(gear_type=gear_type, engine_number=engine_number, stringsAsFactors=FALSE)

x_star_0 <- runif(k_dim*j_dim, min=-1, max=1)
x_star_25 <- runif(k_dim*j_dim, min=-1, max=1)
x_star_50 <- runif(k_dim*j_dim, min=-1, max=1)

# Engine effects
# In the future, maybe incorporate some kind of covariate (e.g. for vib frequency) into this
# However, it would need to be separated by gear
# kappa <- rep(rnorm(k_dim, sd=.3), each=j_dim)
kappa <- 0 # For now remove the engine effects

dat <- cbind(dat, x_star_0, x_star_25, x_star_50, kappa)

true_beta <- matrix(rnorm((p_dim + 1)*j_dim, mean=2), nrow=(p_dim + 1), ncol=j_dim)
colnames(true_beta) <- gear_names

construct_eta <- function(dat) {
    eta <- vector(length=nrow(dat))
    for(b in 1:nrow(dat)) {
        dat_row <- dat[b,]
        beta <- true_beta[,dat_row$gear_type, drop=FALSE]
        x <- simplify2array(c(1, dat_row[,grepl("^x_star", names(dat_row))]))
        eta[b] <- x %*% beta
    }

    return(eta)
}
dat$eta <- construct_eta(dat)
dat$y_star <- dat$eta + rlogis(nrow(dat))

c1 <- quantile(dat$y_star, prob=.2)
c2 <- quantile(dat$y_star, prob=.66)
dat$y <- 3
dat$y[dat$y_star < c1] <- 1
dat$y[(dat$y_star > c1) & (dat$y_star < c2)] <- 2

dat_fuel <- dat[dat$gear_type=="FUEL",]
fuel_model <- clm(factor(y) ~ x_star_0 + x_star_25 + x_star_50, data=dat_fuel)
summary(fuel_model)

fmodel <- clm(factor(y) ~ x_star_0*gear_type + x_star_25*gear_type + x_star_50*gear_type, data=dat)
summary(fmodel)
