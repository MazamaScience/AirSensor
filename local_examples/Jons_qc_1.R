
# ===== TEST CASES =============================================================

if ( FALSE ) {
  
  scsg_15 <- pat_load("SCSG_15", "2019-06-13", "2019-06-20")
  pat_multiplot(scsg_15)
  jons_qc_1(scsg_15)
  
  scap_14 <- pat_load("SCAP_14", "2019-06-13", "2019-06-20")
  pat_multiplot(scap_14)
  jons_qc_1(scap_14)
  
  scem_05 <- pat_load("SCEM_05", "2019-06-13", "2019-06-20")
  pat_multiplot(scem_05)
  jons_qc_1(scem_05)
  
  # And now, 12 months worth of data
  scsb_02 <- pat_load("SCSB_02", "2018-01-01", "2018-12-31")
  pat_multiplot(scsb_02)
  jons_qc_1(scsb_02)
  
}


jons_qc_1 <- function(
  pat
) {
  
  # Set up default colors
  colors <- c(rgb(0.9, 0.25, 0.2), rgb(0.2, 0.25, 0.9))
  
  # ----- Prepare data ---------------------------------------------------------
  
  hourlyData <-
    pat %>%
    pat_qc() %>%
    pat_aggregate(period = "1 hour") %>%
    # Create pm25 by averaging the A and B channel aggregation means
    dplyr::mutate(pm25 = (pm25_A_mean + pm25_B_mean) / 2) %>%
    # Calculate min_count and mean_diff for use in QC
    dplyr::mutate(min_count = pmin(pm25_A_count, pm25_B_count, na.rm = TRUE)) %>%
    dplyr::mutate(mean_diff = abs(pm25_A_mean - pm25_B_mean)) %>%
    # JONS_QC_1 follows
    dplyr::mutate(pm25_qc = pm25) %>%
    # When only 1/3 of data are reporting, something is wrong.
    # Invalidate data where:  (min_count < 10)
    dplyr::mutate(pm25_qc = replace(
      pm25_qc, 
      which(min_count < 10), 
      NA) 
    ) %>%
    # When the means are significantly differnt AND 'large', something is wrong.
    # Invalidate data where:  (p-value < 1e-4) & (mean_diff > 10)
    dplyr::mutate(pm25_qc = replace(
      pm25_qc,
      which( (pm25_p < 1e-4) & (mean_diff > 10) ),
      NA)
    ) %>% 
    # A difference of 20 ug/m3 should only be seen at very high levels.
    # Invalidate data where:  (mean < 100) & (mean_diff > 20)
    dplyr::mutate(pm25_qc = replace(
      pm25_qc,
      which( (pm25 < 100) & (mean_diff > 20) ),
      NA)
    )
  
  # ----- Create plots ---------------------------------------------------------
  
  # min count plot
  gg_min_count <- 
    ggplot(hourlyData, aes(datetime, min_count)) +
    geom_point(shape = 15) +
    scale_y_continuous() + 
    ggtitle("A/B minimum count")
  
  # mean difference plot
  gg_mean_diff <- 
    ggplot(hourlyData, aes(datetime, mean_diff)) +
    geom_point(shape = 15) +
    scale_y_log10() + 
    ggtitle("A/B difference")
  
  # p-value plot
  gg_pm25_p <- 
    ggplot(hourlyData, aes(datetime, pm25_p)) +
    geom_point(shape = 15) +
    scale_y_log10() + 
    ggtitle("t-test p-value")
  
  # A B mean plot
  gg_pm25 <- 
    ggplot(hourlyData, aes(datetime, pm25)) +
    geom_point(shape = 15) +
    scale_y_continuous() + 
    ggtitle("merged pm25 with no QC")
  
  # Jons_qc_1 plot
  gg_pm25_qc <- 
    ggplot(hourlyData, aes(datetime, pm25_qc)) +
    geom_point(shape = 15) +
    scale_y_continuous() + 
    ggtitle("merged pm25 with Jons QC 1")
  
  # A B means
  gg_AB_means <- 
    hourlyData %>%
    dplyr::select(datetime, pm25_A_mean, pm25_B_mean) %>%
    tidyr::gather("channel", "value", -datetime) %>%
    
    ggplot(aes(datetime, value, color = channel)) +
    geom_point(shape = 15) +
    scale_color_manual(values=colors) +
    scale_y_continuous() + 
    theme(legend.position = "none") +
    ggtitle("A/B separate")
  
  # ----- Assemmble full plot --------------------------------------------------
  
  multi_ggplot(
    gg_min_count,
    gg_mean_diff,
    gg_pm25_p,
    gg_AB_means,
    gg_pm25,
    gg_pm25_qc
  )
  
}
