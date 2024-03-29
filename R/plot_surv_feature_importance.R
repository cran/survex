#' Plot Permutational Feature Importance for Survival Models
#'
#' This function plots feature importance objects created for survival models using the
#' `model_parts()` function with a time-dependent metric, that is `loss_one_minus_cd_auc()` or
#' `loss_brier_score()`.
#'
#' @param x an object of class `"surv_feature_importance"` to be plotted
#' @param ... additional objects of class `"surv_feature_importance"` to be plotted together
#' @param title character, title of the plot
#' @param subtitle character, subtitle of the plot, `'default'` automatically generates "created for XXX, YYY models", where XXX and YYY are the explainer labels
#' @param max_vars maximum number of variables to be plotted (least important variables are ignored)
#' @param colors character vector containing the colors to be used for plotting variables (containing either hex codes "#FF69B4", or names "blue")
#' @param rug character, one of `"all"`, `"events"`, `"censors"`, `"none"` or `NULL`. Which times to mark on the x axis in `geom_rug()`.
#' @param rug_colors character vector containing two colors (containing either hex codes "#FF69B4", or names "blue"). The first color (red by default) will be used to mark event times, whereas the second (grey by default) will be used to mark censor times.
#'
#' @return An object of the class `ggplot`.
#'
#' @family functions for plotting 'model_parts_survival' objects
#'
#' @examples
#' \donttest{
#' library(survival)
#' library(survex)
#'
#' model <- coxph(Surv(time, status) ~ ., data = veteran, x = TRUE, model = TRUE, y = TRUE)
#' model_rf <- randomForestSRC::rfsrc(Surv(time, status) ~ ., data = veteran)
#' explainer <- explain(model)
#' explainer_rf <- explain(model_rf)
#'
#' mp <- model_parts(explainer)
#' mp_rf <- model_parts(explainer_rf)
#'
#' plot(mp, mp_rf)
#' }
#'
#' @export
plot.surv_feature_importance <- function(x, ...,
                                         title = "Time-dependent feature importance",
                                         subtitle = "default",
                                         max_vars = 7,
                                         colors = NULL,
                                         rug = "all",
                                         rug_colors = c("#dd0000", "#222222")) {
    df_list <- c(list(x), list(...))

    transformed_dfs <- lapply(df_list, function(x) {
        x <- x$result
        label <- unique(x$label)
        x <- x[x$`_permutation_` == 0, !colnames(x) %in% c("_permutation_", "label", "_baseline_")]
        plotting_df <- with(x, cbind(x[1], stack(x, select = -`_times_`), label, row.names = NULL))
    })

    transformed_rug_dfs <- lapply(df_list, function(x) {
        rug_df <- data.frame(times = x$event_times, statuses = as.character(x$event_statuses), label = unique(x$result$label))
    })

    plotting_df <- do.call(rbind, transformed_dfs)
    rug_df <- do.call(rbind, transformed_rug_dfs)

    labels <- unique(plotting_df$label)

    subs <- aggregate(plotting_df$value, by = list(var = plotting_df$ind), function(x) sum(abs(x)))

    subs <- subs[order(subs$x, decreasing = TRUE), ]
    plotting_df <- plotting_df[plotting_df$ind %in% c("_full_model_", as.character(head(subs$var, max_vars))), ]

    num_vars <- length(unique(plotting_df$ind)) - 1 # remove full_model; note that num_vars <= max_vars

    additional_info <- switch(attr(x, "type"),
        "raw" = "",
        "ratio" = "\ndivided by the loss of full model",
        "difference" = "\nwith loss of full model subtracted"
    )

    if (!is.null(attr(x, "loss_name"))) {
        y_lab <- paste0(paste(attr(x, "loss_name")[1], "loss after permutations"), additional_info)
    } else {
        y_lab <- paste0("Loss function after variable's permutations", additional_info)
    }

    if (!is.null(subtitle) && subtitle == "default") {
        endword <- ifelse(length(labels) > 1, " models", " model")
        subtitle <- paste0("created for the ", paste0(labels, collapse = ", "), endword)
    }

    base_plot <- with(plotting_df, {
        ggplot(data = plotting_df, aes(x = `_times_`, y = values, color = ind, label = ind)) +
            geom_line(linewidth = 0.8) +
            theme_default_survex() +
            labs(x = "time", y = y_lab, title = title, subtitle = subtitle) +
            xlim(c(0, NA)) +
            scale_color_manual(name = "Variable", values = c("#000000", generate_discrete_color_scale(num_vars, colors))) +
            facet_wrap(~label)
    })

    return_plot <- add_rug_to_plot(base_plot, rug_df, rug, rug_colors)

    return(return_plot)
}
