#' Segment Profiler with Automatic Segmentation
#'
#' Automatically segments customers using RFM + K-means (if no segment column provided)
#' and generates a comprehensive profile with summary table and visualizations.
#'
#' @param data A data frame. If segment_col is NULL, must contain columns:
#'        customer_id, transaction_date (Date), amount (numeric). Otherwise,
#'        must contain a segment column.
#' @param customer_id Unquoted column name for customer identifier (required for auto-segmentation).
#' @param segment_col Unquoted column name for existing segment labels (optional).
#'        If provided, segmentation is skipped.
#' @param metrics Character vector of column names to profile (optional). If NULL,
#'        all columns except segment and customer_id are profiled.
#' @param weight_col Unquoted column name for weighting (optional).
#' @param plot_3d Logical, whether to produce 3D scatter plot of centroids.
#' @param palette Color palette name from `scale_fill_brewer` or `scale_color_brewer`.
#' @param verbose Logical, print messages.
#' @param k Number of clusters for K-means (default 4).
#' @param analysis_date Date to use as "today" for recency calculation (default: max transaction date).
#'
#' @return A list with components:
#'   \item{summary}{data frame with segment statistics}
#'   \item{plots}{list of ggplot/plotly objects: size, radar, scatter2d, scatter3d (optional)}
#' @export
#'
#' @examples
#' \dontrun{
#' # Auto-segmentation example
#' trans <- data.frame(
#'   customer_id = c(1,1,2,2,3,3),
#'   transaction_date = as.Date(c("2024-01-01","2024-02-01","2024-01-15",
#'                                "2024-03-01","2024-02-10","2024-04-01")),
#'   amount = c(100,150,200,50,300,80),
#'   category = c("A","B","A","A","B","B")
#' )
#' result <- seg_prof(trans, customer_id = customer_id, plot_3d = TRUE)
#' result$summary
#' result$plots$size
#' result$plots$scatter2d
#' }
seg_prof <- function(data,
                     customer_id = NULL,
                     segment_col = NULL,
                     metrics = NULL,
                     weight_col = NULL,
                     plot_3d = FALSE,
                     palette = "Set2",
                     verbose = TRUE,
                     k = 4,
                     analysis_date = NULL) {
  
  # ---------- 1. Input validation ----------
  if (!is.data.frame(data)) stop("data must be a data frame")
  
  # ---------- 2. Auto-segmentation if segment_col not provided ----------
  if (is.null(segment_col)) {
    if (verbose) message("No segment column provided. Performing automatic RFM + K-means segmentation...")
    
    # Check customer_id
    if (is.null(customer_id)) stop("If no segment column, you must provide customer_id column for segmentation.")
    cust_id <- enquo(customer_id)
    cust_name <- as_name(cust_id)
    if (!(cust_name %in% names(data))) stop("customer_id column not found.")
    
    # Check required columns for RFM
    required <- c("transaction_date", "amount")
    missing <- setdiff(required, names(data))
    if (length(missing) > 0) {
      stop("For automatic segmentation, data must contain columns: ",
           paste(required, collapse=", "), ". Missing: ", paste(missing, collapse=", "))
    }
    
    # Ensure transaction_date is Date
    if (!inherits(data$transaction_date, "Date")) {
      data$transaction_date <- as.Date(data$transaction_date)
    }
    
    # Determine analysis date
    if (is.null(analysis_date)) analysis_date <- max(data$transaction_date, na.rm = TRUE)
    
    # Compute RFM
    rfm_data <- data %>%
      group_by(!!cust_id) %>%
      summarise(
        recency = as.numeric(analysis_date - max(transaction_date, na.rm = TRUE)),
        frequency = n(),
        monetary = sum(amount, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Standardize and cluster
    rfm_scaled <- rfm_data %>% select(recency, frequency, monetary) %>% scale()
    set.seed(456)
    km <- kmeans(rfm_scaled, centers = k, nstart = 25)
    rfm_data$segment <- paste0("Segment_", km$cluster)
    
    # Merge segment back to original data (keep all original columns)
    data_for_prof <- data %>%
      left_join(rfm_data %>% select(!!cust_id, segment), by = cust_name)
    
    segment_col <- sym("segment")
    if (verbose) message("Auto-segmentation completed with k = ", k, " clusters.")
  } else {
    # Use provided segment column
    data_for_prof <- data
    segment_col <- enquo(segment_col)
    if (verbose) message("Using existing segment column: ", as_name(segment_col))
  }
  
  # ---------- 3. Profiling (robust handling of mixed types) ----------
  seg_name <- as_name(segment_col)
  if (!(seg_name %in% names(data_for_prof))) stop("Segment column not found after segmentation.")
  
  # Prepare metrics
  all_cols <- names(data_for_prof)
  if (is.null(metrics)) {
    metrics <- setdiff(all_cols, seg_name)
    # Exclude customer_id if present (to avoid double counting)
    if (!is.null(customer_id)) {
      cust_name <- as_name(enquo(customer_id))
      metrics <- setdiff(metrics, cust_name)
    }
  } else {
    metrics <- as.character(metrics)
    missing_metrics <- setdiff(metrics, all_cols)
    if (length(missing_metrics) > 0) stop("Metrics not found: ", paste(missing_metrics, collapse=", "))
  }
  
  # Subset data
  data_sub <- data_for_prof %>% select(!!segment_col, all_of(metrics))
  if (nrow(data_sub) == 0) stop("No data after selecting columns")
  if (any(is.na(data_sub))) warning("Missing values detected. They will be handled appropriately.")
  
  # Clean data types
  data_sub <- data_sub %>%
    mutate(across(where(is.factor), as.character)) %>%
    mutate(across(where(is.logical), as.character)) %>%
    mutate(across(where(is.numeric), ~ ifelse(is.infinite(.), NA, .)))
  
  if (is.factor(data_sub[[seg_name]])) data_sub[[seg_name]] <- droplevels(data_sub[[seg_name]])
  
  # Identify column types
  col_types <- sapply(data_sub, function(x) {
    if (is.numeric(x)) "numeric"
    else if (is.character(x) || is.factor(x)) "categorical"
    else "other"
  })
  numeric_cols <- setdiff(names(col_types[col_types == "numeric"]), seg_name)
  categorical_cols <- setdiff(names(col_types[col_types == "categorical"]), seg_name)
  other_cols <- names(col_types[col_types == "other"])
  
  if (verbose && length(other_cols) > 0) {
    message("Ignoring non-numeric/non-categorical columns: ", paste(other_cols, collapse=", "))
  }
  
  # ---- Numeric summary ----
  num_summary <- NULL
  if (length(numeric_cols) > 0) {
    if (!is.null(weight_col)) {
      weight <- enquo(weight_col)
      if (!(as_name(weight) %in% names(data_sub))) stop("weight_col not found in data")
      num_summary <- data_sub %>%
        group_by(!!segment_col) %>%
        summarise(across(all_of(numeric_cols),
                         list(
                           n = ~sum(!is.na(.)),
                           mean = ~weighted.mean(., w = !!weight, na.rm = TRUE),
                           median = ~median(., na.rm = TRUE),
                           sd = ~sd(., na.rm = TRUE)
                         ), .names = "{col}_{fn}"),
                  .groups = "drop")
    } else {
      num_summary <- data_sub %>%
        group_by(!!segment_col) %>%
        summarise(across(all_of(numeric_cols),
                         list(
                           n = ~sum(!is.na(.)),
                           mean = ~mean(., na.rm = TRUE),
                           median = ~median(., na.rm = TRUE),
                           sd = ~sd(., na.rm = TRUE)
                         ), .names = "{col}_{fn}"),
                  .groups = "drop")
    }
  }
  
  # ---- Categorical summary ----
  cat_summary <- NULL
  if (length(categorical_cols) > 0) {
    cat_summary <- data_sub %>%
      group_by(!!segment_col) %>%
      summarise(across(all_of(categorical_cols),
                       list(
                         mode = ~{
                           tbl <- table(., useNA = "ifany")
                           if (length(tbl) == 0) NA_character_ else names(tbl)[which.max(tbl)]
                         },
                         pct_mode = ~{
                           tbl <- table(., useNA = "ifany")
                           if (length(tbl) == 0) NA_real_ else max(tbl) / sum(tbl) * 100
                         }
                       ), .names = "{col}_{fn}"),
                .groups = "drop")
  }
  
  # Combine summaries
  if (!is.null(num_summary) && !is.null(cat_summary)) {
    summary_table <- full_join(num_summary, cat_summary, by = seg_name)
  } else if (!is.null(num_summary)) {
    summary_table <- num_summary
  } else if (!is.null(cat_summary)) {
    summary_table <- cat_summary
  } else {
    stop("No numeric or categorical columns to summarize.")
  }
  
  # Add segment size and share
  seg_counts <- data_sub %>%
    group_by(!!segment_col) %>%
    summarise(n_segments = n(), .groups = "drop") %>%
    mutate(share = n_segments / sum(n_segments) * 100)
  
  summary_table <- summary_table %>%
    left_join(seg_counts, by = seg_name) %>%
    select(!!segment_col, n_segments, share, everything())
  
  # ---- Visualizations ----
  plots <- list()
  
  # 1. Bar chart segment size
  p_size <- ggplot(seg_counts, aes(x = reorder(!!segment_col, -n_segments), 
                                   y = n_segments, 
                                   fill = !!segment_col)) +
    geom_bar(stat = "identity") +
    scale_fill_brewer(palette = palette) +
    labs(title = "Segment Size", x = "Segment", y = "Number of Customers") +
    theme_minimal() +
    theme(legend.position = "none")
  plots$size <- p_size
  
  # 2. Radar chart (if at least 2 numeric metrics)
  if (length(numeric_cols) >= 2) {
    radar_data <- data_sub %>%
      group_by(!!segment_col) %>%
      summarise(across(all_of(numeric_cols), ~mean(., na.rm = TRUE)), .groups = "drop") %>%
      tidyr::pivot_longer(cols = -!!segment_col, names_to = "metric", values_to = "value")
    
    radar_norm <- radar_data %>%
      group_by(metric) %>%
      mutate(
        min_val = min(value, na.rm = TRUE),
        max_val = max(value, na.rm = TRUE),
        value_norm = ifelse(min_val == max_val, 0.5, (value - min_val) / (max_val - min_val))
      ) %>%
      ungroup()
    
    p_radar <- ggplot(radar_norm, aes(x = metric, y = value_norm, 
                                      color = !!segment_col, group = !!segment_col)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      coord_polar() +
      scale_color_brewer(palette = palette) +
      labs(title = "Segment Profiles (Normalized Means)", x = "", y = "") +
      theme_minimal() +
      theme(axis.text.x = element_text(size = 8))
    plots$radar <- p_radar
  }
  
  # 3. 2D Scatter plot with ellipses (if at least 2 numeric metrics)
  if (length(numeric_cols) >= 2) {
    x_metric <- numeric_cols[1]
    y_metric <- numeric_cols[2]
    p_scatter2d <- ggplot(data_sub, aes(x = .data[[x_metric]], 
                                        y = .data[[y_metric]], 
                                        color = !!segment_col)) +
      geom_point(alpha = 0.6, size = 1.5) +
      stat_ellipse(level = 0.95, linetype = "dashed") +
      scale_color_brewer(palette = palette) +
      labs(title = paste("Actual Data Distribution (", x_metric, " vs ", y_metric, ")", sep = ""),
           x = x_metric, y = y_metric, color = "Segment") +
      theme_minimal()
    plots$scatter2d <- p_scatter2d
  }
  
  # 4. 3D scatter (if requested and at least 3 numeric metrics)
  if (plot_3d && length(numeric_cols) >= 3) {
    centroids <- data_sub %>%
      group_by(!!segment_col) %>%
      summarise(across(all_of(numeric_cols), ~mean(., na.rm = TRUE)), .groups = "drop")
    
    x_metric <- numeric_cols[1]
    y_metric <- numeric_cols[2]
    z_metric <- numeric_cols[3]
    
    p_3d <- plot_ly(centroids,
                    x = as.formula(paste0("~", x_metric)),
                    y = as.formula(paste0("~", y_metric)),
                    z = as.formula(paste0("~", z_metric)),
                    color = as.formula(paste0("~", seg_name)),
                    colors = viridis(length(unique(centroids[[seg_name]]))),
                    type = "scatter3d",
                    mode = "markers",
                    marker = list(size = 8),
                    text = ~paste(seg_name, "<br>",
                                  x_metric, ":", round(get(x_metric), 2), "<br>",
                                  y_metric, ":", round(get(y_metric), 2), "<br>",
                                  z_metric, ":", round(get(z_metric), 2))) %>%
      layout(title = paste("3D Segment Centroids (", x_metric, ", ", y_metric, ", ", z_metric, ")", sep = ""),
             scene = list(xaxis = list(title = x_metric),
                          yaxis = list(title = y_metric),
                          zaxis = list(title = z_metric)))
    plots$scatter3d <- p_3d
  }
  
  # Return result
  result <- list(
    summary = summary_table,
    plots = plots
  )
  class(result) <- "seg_prof"
  return(result)
}
