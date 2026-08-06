library(ggrepel)

plot_data <- mpa_results %>%
  select(MPA_Name, SST_Anomaly_SSP245, SST_Anomaly_SSP585) %>%
  tidyr::pivot_longer(
    cols = c(SST_Anomaly_SSP245, SST_Anomaly_SSP585),
    names_to = "Scenario", values_to = "Anomaly"
  ) %>%
  mutate(
    Scenario = gsub("SST_Anomaly_", "", Scenario),
    Flag_1p5 = Anomaly > 1.5,
    Flag_2p0 = Anomaly > 2.0,
    Label = case_when(
      Flag_2p0 ~ paste0(MPA_Name, " (", round(Anomaly, 1), "°C)"),
      Flag_1p5 ~ paste0(MPA_Name, " (", round(Anomaly, 1), "°C)"),
      TRUE ~ NA_character_
    )
  )

# Subset Mafia
mafia_data <- plot_data %>%
  filter(MPA_Name == "Mafia Island") %>%
  mutate(Label = paste0(MPA_Name, " (", round(Anomaly, 1), "°C)"))

p <- ggplot(plot_data, aes(x = Scenario, y = Anomaly, fill = Scenario)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  # normal labels
  geom_text_repel(
    aes(label = Label),
    na.rm = TRUE,
    size = 3,
    max.overlaps = 20,
    segment.color = NA,
    color = "black"
  ) +
  # Mafia's red point
  geom_point(
    data = mafia_data,
    aes(x = Scenario, y = Anomaly),
    color = "red", size = 3
  ) +
  # Mafia's red label (always visible)
  geom_text_repel(
    data = mafia_data,
    aes(label = Label),
    size = 3.5,
    color = "red",
    segment.color = NA,
    max.overlaps = Inf
  ) +
  labs(
    title = "SST Anomalies by Climate Scenario",
    subtitle = "MPAs labelled if exceeding 1.5°C or 2°C targets\n(Mafia Island always highlighted in red)",
    x = "Scenario", y = "Anomaly (°C)"
  ) +
  theme_minimal()

print(p)

ggsave("output/sst_anomalies_mpa.png", plot = p, bg = 'white',width = 10, height = 10, dpi = 300)
