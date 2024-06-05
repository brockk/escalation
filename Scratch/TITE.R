
library(dfcrm)
library(escalation)

prior <- c(0.1, 0.25, 0.4, 0.8)
df <- parse_phase1_outcomes("1NNN 2NNN", as_list = FALSE)
df$weight <- c(1, 1, 1, 0.1, 0.01, 0.1)
x0 <- crm(
  prior = prior, target = 0.25, tox = df$tox, level = df$dose,
  n = nrow(df)
)
x0

x1 <- get_dfcrm(skeleton = prior, target = 0.25) %>%
  fit(df)
x1
x1$dfcrm_fit

? crm
# crm(prior, target, tox, level, n = length(level), dosename = NULL,
#     include = 1:n, pid = 1:n, conf.level = 0.9, method = "bayes",
#     model = "empiric", intcpt = 3, scale = sqrt(1.34), model.detail = TRUE,
#     patient.detail = TRUE, var.est = TRUE)
? titecrm
# titecrm(prior, target, tox, level, n = length(level), weights = NULL,
#         followup = NULL, entry = NULL, exit = NULL, obswin = NULL,
#         scheme = "linear", conf.level = 0.9, dosename = NULL, include = 1:n,
#         pid = 1:n, method = "bayes", model = "empiric", var.est = TRUE,
#         scale = sqrt(1.34), intcpt = 3, model.detail = TRUE, patient.detail = TRUE,
#         tite = TRUE)

x2 <- titecrm(
  prior = prior, target = 0.25, tox = df$tox, level = df$dose,
  n = nrow(df), weights = df$weight
)
x2$weights
