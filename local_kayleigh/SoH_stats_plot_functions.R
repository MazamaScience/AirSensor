# Create a plot showing the range of temperature_pctReporting for every sensor
singleMonthlySohMetric_plot <- function(
  monthlySoH_tbl = NULL,
  SoHMetric = NULL
) {
ggplot(monthlySoH_tbl, aes(x = reorder(deviceDeploymentID, SoHMetric, mean), 
                y = SoHMetric)) +
  geom_boxplot() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
}
  
  
  
  
  
  
  