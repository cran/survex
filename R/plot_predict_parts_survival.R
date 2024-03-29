#' Plot Predict Parts for Survival Models
#'
#' This function plots objects of class `"predict_parts_survival"` - local explanations
#' for survival models created using the `predict_parts()` function.
#'
#' @param x an object of class  `"predict_parts_survival"` to be plotted
#' @param ... additional parameters passed to the `plot.surv_shap` or `plot.surv_lime` functions
#'
#' @return An object of the class `ggplot`.
#'
#' @section Plot options:
#'
#' ## `plot.surv_shap`
#'
#' * `x` - an object of class `"surv_shap"` to be plotted
#' * `...` - additional objects of class `surv_shap` to be plotted together
#' * `title` - character, title of the plot
#' * `subtitle` - character, subtitle of the plot, `'default'` automatically generates "created for XXX, YYY models", where XXX and YYY are the explainer labels
#' * `max_vars` - maximum number of variables to be plotted (least important variables are ignored)
#' * `colors` - character vector containing the colors to be used for plotting variables (containing either hex codes "#FF69B4", or names "blue")
#' * `rug` - character, one of `"all"`, `"events"`, `"censors"`, `"none"` or `NULL`. Which times to mark on the x axis in `geom_rug()`.
#' * `rug_colors` - character vector containing two colors (containing either hex codes "#FF69B4", or names "blue"). The first color (red by default) will be used to mark event times, whereas the second (grey by default) will be used to mark censor times.
#'
#' ## `plot.surv_lime`
#'
#' * `x` -  an object of class `"surv_lime"` to be plotted
#' * `type` -  character, either "coefficients" or "local_importance", selects the type of plot
#' * `show_survival_function` -  logical, if the survival function of the explanations should be plotted next to the barplot
#' * `...` -  other parameters currently ignored
#' * `title` -  character, title of the plot
#' * `subtitle` -  character, subtitle of the plot, `'default'` automatically generates "created for XXX, YYY models", where XXX and YYY are the explainer labels
#' * `max_vars` - maximum number of variables to be plotted (least important variables are ignored)
#' * `colors` -  character vector containing the colors to be used for plotting variables (containing either hex codes "#FF69B4", or names "blue")
#'
#' @family functions for plotting 'predict_parts_survival' objects
#'
#' @examples
#' \donttest{
#' library(survival)
#' library(survex)
#'
#' model <- randomForestSRC::rfsrc(Surv(time, status) ~ ., data = veteran)
#' exp <- explain(model)
#'
#' p_parts_shap <- predict_parts(exp, veteran[1, -c(3, 4)], type = "survshap")
#' plot(p_parts_shap)
#'
#' p_parts_lime <- predict_parts(exp, veteran[1, -c(3, 4)], type = "survlime")
#' plot(p_parts_lime)
#' }
#' @export
plot.predict_parts_survival <- function(x, ...) {
    class(x) <- class(x)[-1]
    plot(x, ...)
}
