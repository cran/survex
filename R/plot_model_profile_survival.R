#' Plot Model Profile for Survival Models
#'
#' This function plots objects of class `"model_profile_survival"` created
#' using the `model_profile()` function.
#'
#' @param x an object of class `model_profile_survival` to be plotted
#' @param ... additional parameters, unused, currently ignored
#' @param variables character, names of the variables to be plotted
#' @param variable_type character, either `"numerical"`, `"categorical"` or `NULL` (default), select only one type of variable for plotting, or leave `NULL` for all
#' @param facet_ncol number of columns for arranging subplots
#' @param numerical_plot_type character, either `"lines"`, or `"contours"` selects the type of numerical variable plots
#' @param title character, title of the plot
#' @param subtitle character, subtitle of the plot, `'default'` automatically generates "created for XXX, YYY models", where XXX and YYY are the explainer labels
#' @param colors character vector containing the colors to be used for plotting variables (containing either hex codes "#FF69B4", or names "blue")
#'
#' @return A grid of `ggplot` objects arranged with the `gridExtra::grid.arrange` function.
#'
#' @examples
#' \donttest{
#' library(survival)
#' library(survex)
#'
#' model <- randomForestSRC::rfsrc(Surv(time, status) ~ ., data = veteran)
#' exp <- explain(model)
#'
#' m_prof <- model_profile(exp, categorical_variables = "trt")
#'
#' plot(m_prof)
#'
#' plot(m_prof, numerical_plot_type = "contours")
#'
#' plot(m_prof, variables = c("trt", "age"), facet_ncol = 1)
#' }
#'
#' @export
plot.model_profile_survival <- function(x,
                                        ...,
                                        variables = NULL,
                                        variable_type = NULL,
                                        facet_ncol = NULL,
                                        numerical_plot_type = "lines",
                                        title = "Partial dependence survival profile",
                                        subtitle = "default",
                                        colors = NULL)
{


    aggregated_profiles <- x$result
    class(aggregated_profiles) <- "data.frame"

    all_variables <- unique(aggregated_profiles$`_vname_`)
    if (!is.null(variables)) {
        all_variables <- intersect(all_variables, variables)
        if (length(all_variables) == 0)
            stop(paste0(
                "variables do not overlap with ",
                paste(all_variables, collapse = ", ")
            ))
    }
    aggregated_profiles <-
        aggregated_profiles[aggregated_profiles$`_vname_` %in% all_variables, ]


    if (!is.null(subtitle) && subtitle == "default") {
        labels <-
            paste0(unique(aggregated_profiles$`_label_`), collapse = ", ")
        subtitle <- paste0("created for the ", labels, " model")
    }

    if (is.null(variables)) {
        variables <- unique(aggregated_profiles$`_vname_`)}

    if (!is.null(variable_type) && variable_type == "numerical") {
        aggregated_profiles <- aggregated_profiles[aggregated_profiles$`_vtype_` == "numerical", ]
    }

    if (!is.null(variable_type) && variable_type == "categorical") {
        aggregated_profiles <- aggregated_profiles[aggregated_profiles$`_vtype_` == "categorical", ]
    }

    variables <- intersect(variables, unique(aggregated_profiles$`_vname_`))
    n_colors <- length(unique(aggregated_profiles$`_x_`))

    aggregated_profiles$`_real_point_` <- FALSE

    pl <- plot_individual_ceteris_paribus_survival(aggregated_profiles, variables, colors, numerical_plot_type)

    patchwork::wrap_plots(pl, ncol = facet_ncol) +
        patchwork::plot_annotation(title = title,
                                   subtitle = subtitle) & DALEX::theme_drwhy()

}
