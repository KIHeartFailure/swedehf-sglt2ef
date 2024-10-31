source(here::here("setup/setup.R"))

# load data
load(here("data/clean-data/rsdata.RData"))

dataass <- mice::complete(imprsdata, 3)
dataass <- mice::complete(imprsdata, 6)

# For log reg models
ormod <- glm(formula(paste0("sos_lm_sglt2 == 'Yes' ~ shf_ef_cat + ", paste0(modvars, collapse = " + "))),
  family = binomial(link = "logit"), data = dataass
)

# vif
print(car::vif(ormod))

# outliers
cooks <- cooks.distance(ormod)
plot(cooks)
abline(h = 4 / nrow(dataass), lty = 2, col = "red") # add cutoff line
