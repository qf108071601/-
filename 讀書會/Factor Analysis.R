library(psych)

url <- "https://assets.datacamp.com/production/repositories/2136/datasets/869615371e66021e97829feb7e19e38037ed0c14/GCBS_data.rds"
gcbs <- readRDS(gzcon(url(url)))

# Conduct a single-factor EFA
EFA_model <- fa(gcbs,nfactors = 2,rotate = "varimax",fm = "pa")

# View the results
print(EFA_model)

plot(EFA_model)

fa.diagram(EFA_model)

plot(density(EFA_model$scores, na.rm = TRUE), 
     main = "Factor Scores")


error.dots(gcbs)
error.bars(gcbs)


data(ability.cov)
cov = ability.cov$cov
