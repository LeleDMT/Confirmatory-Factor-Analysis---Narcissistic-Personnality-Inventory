
# 0. INSTALL & LOAD PACKAGES

install.packages(c("lavaan", "semPlot", "semTools", "psych", "dplyr"))

library(lavaan)
library(semTools)
library(semPlot)
library(psych)
library(dplyr)


# 1. LOAD DATA

data <- read.csv("data.csv")


# 2. DEFINE THE MODEL

npi_model <- '
  Authority        =~ Q1  + Q8  + Q10 + Q11 + Q12 + Q32 + Q33 + Q36
  SelfSufficiency  =~ Q17 + Q21 + Q22 + Q31 + Q34 + Q39
  Superiority      =~ Q4  + Q9  + Q26 + Q37 + Q40
  Exhibitionism    =~ Q2  + Q3  + Q7  + Q20 + Q28 + Q30 + Q38
  Exploitativeness =~ Q6  + Q13 + Q16 + Q23 + Q35
  Vanity           =~ Q15 + Q19 + Q29
  Entitlement      =~ Q5  + Q14 + Q18 + Q24 + Q25 + Q27
'


# 3. FIT THE MODEL

fit_cfa <- lavaan::cfa(
  model     = npi_model,
  data      = data,
  estimator = "WLSMV",  # for binary / ordinal items
  ordered   = TRUE
)


# 4. FIT INDICES

summary(fit_cfa, fit.measures = TRUE, standardized = TRUE)

lavaan::fitMeasures(fit_cfa, c(
  "chisq", "df", "pvalue",
  "cfi",   "tli",
  "rmsea", "rmsea.ci.lower", "rmsea.ci.upper",
  "srmr"
))


# 5. FACTOR LOADINGS

lavaan::parameterEstimates(fit_cfa, standardized = TRUE) |>
  filter(op == "=~") |>
  select(Factor = lhs, Item = rhs,
         Lambda = std.all, SE = se, p = pvalue) |>
  mutate(across(where(is.numeric), ~ round(., 3)))



# 6. PATH DIAGRAM 

semPaths(
  fit_cfa,
  what           = "std",      # standardized loadings on arrows
  layout         = "tree2",    # factors left, items right
  rotation       = 2,
  edge.label.cex = 0.55,
  node.label.cex = 0.65,
  residuals      = FALSE,      # cleaner without residual loops
  curvePivot     = TRUE,
  fade           = FALSE,      # uniform arrow opacity
  nCharNodes     = 0,          # full variable names
  mar            = c(4, 4, 4, 4),
  style          = "lisrel",   # classic SEM look
  color          = list(
    lat = "#4A90D9",           # blue for latent factors
    man = "#F5F5F5"            # light grey for observed items
  ),
  border.width   = 1.5,
  edge.color     = "#333333"
)



