## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(survex)
library(survival)

cph <- coxph(Surv(time, status) ~ ., data = veteran, model = TRUE, x = TRUE)

auto_cph_explainer <- explain(cph)

## -----------------------------------------------------------------------------
cph <- coxph(Surv(time, status) ~ ., data=veteran)

# data must not include the target columns
veteran_data <- veteran[, -c(3,4)]
veteran_y <- Surv(veteran$time, veteran$status)

# set prediction functions of the required format
risk_pred <- function(model, newdata) predict(model, newdata, type = "risk")
surv_pred <- function(model, newdata, times) pec::predictSurvProb(model, newdata, times)
chf_pred <- function(model, newdata, times) -log(surv_pred(model, newdata, times))

manual_cph_explainer <- explain_survival(model = cph,
                                         data = veteran_data,
                                         y = veteran_y,
                                         predict_function = risk_pred,
                                         predict_survival_function = surv_pred,
                                         predict_cumulative_hazard_function = chf_pred,
                                         label="manual coxph")

## -----------------------------------------------------------------------------
surv_pred_rsf <- transform_to_stepfunction(predict,
                                           type="survival",
                                           prediction_element = "survival",
                                           times_element = "time.interest")

## -----------------------------------------------------------------------------
# would also work 
# chf_pred_rsf <- transform_to_stepfunction(predict,
#                                           type="chf",
#                                           prediction_element = "chf",
#                                           times_element = "time.interest")

chf_pred_rsf <- function(model, newdata, times) {
  survival_to_cumulative_hazard(surv_pred_rsf(model, newdata, times))
}

## -----------------------------------------------------------------------------
times <- unique(veteran$times)
risk_pred_rsf <- risk_from_chf(chf_pred_rsf, times)

## -----------------------------------------------------------------------------
library(randomForestSRC)
rsf <- rfsrc(Surv(time, status) ~ ., data = veteran)

manual_rsf_explainer <- explain_survival(model = rsf,
                                         data = veteran_data,
                                         y = veteran_y,
                                         predict_function = risk_pred_rsf,
                                         predict_survival_function = surv_pred_rsf,
                                         predict_cumulative_hazard_function = chf_pred_rsf,
                                         label = "manual rsf")

