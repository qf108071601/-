library(depmixS4)
data("speed")
set.seed(1)

mod <- depmix(response = rt ~ 1, 
              data = speed,
              nstates = 2,
              trstart = runif(4))

fm <- fit(mod, emc=em.control(rand=FALSE))
fm
summary(fm,which = "transition")


set.seed(1)
mod <- depmix(rt ~ 1,
              data = speed, 
              nstates = 2, 
              family = gaussian(),
              transition = ~ scale(Pacc), 
              instart = runif(2))

fm <- fit(mod, 
          verbose = FALSE,
          emc=em.control(rand=FALSE))

summary(fm, which = "transition")
summary(fm, which = "response")




set.seed(1)
mod <- depmix(list(rt ~ 1,corr ~ 1), data = speed, nstates = 2,
              family = list(gaussian(), multinomial("identity")),
              transition = ~ scale(Pacc), instart = runif(2))

fm <- fit(mod, verbose = FALSE, emc=em.control(rand=FALSE))

summary(fm, which = "response")
summary(fm, which = "transition")


setpars(mod, value = 1:npar(mod))
setpars(mod, getpars(mod, which = "fixed"))

trst <- c(0.9, 0.1, 0, 0, 0.1, 0.9, 0, 0)

mod <- depmix(list(rt ~ 1,corr ~ 1), data = speed, transition = ~ Pacc,
              nstates = 2, 
              family = list(gaussian(), multinomial("identity")),
              trstart = trst, instart = c(0.99, 0.01))

fm1 <- fit(mod,verbose = FALSE, emc=em.control(rand=FALSE))


pars <- c(unlist(getpars(fm1)))
pars[6] <- pars[10] <- 11
pars[1] <- 0
pars[2] <- 1
pars[13] <- pars[14] <- 0.5
fm1 <- setpars(mod, pars)
conpat <- c(0, 0, rep(c(0, 1), 4), 1, 1, 0, 0, 1, 1, 1, 1)
conpat[6] <- conpat[10] <- 2

fm2 <- fit(fm1, equal = conpat)




data("balance")
set.seed(1)
mod <- mix(list(d1 ~ 1, d2 ~ 1, d3 ~ 1, d4 ~ 1), data = balance,
           nstates = 3, family = list(multinomial("identity"),
                                      multinomial("identity"),
                                      multinomial("identity"),
                                      multinomial("identity")), 
           respstart = runif(24),
           prior = ~ age,
           initdata = balance)

fm <- fit(mod, verbose = FALSE, emc=em.control(rand=FALSE))
fm
summary(fm, which = "response")
summary(fm, which = "prior")










