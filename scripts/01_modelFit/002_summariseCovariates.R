# ==============================================================================
# Plot predictor rasters
# ==============================================================================

library(terra)
library(dplyr)
library(tidyr)
library(ggplot2)

predictors_df <- terra::as.data.frame(
  predictors,
  xy = TRUE,
  na.rm = FALSE
) %>%
  tidyr::pivot_longer(
    cols = -c(x, y),
    names_to = "predictor",
    values_to = "value"
  )


predictor_plot <- ggplot(
  predictors_df,
  aes(
    x = x,
    y = y,
    fill = value
  )
) +
  geom_raster() +
  facet_wrap(
    ~ predictor,
    ncol = 5
  ) +
  coord_equal() +
  scale_fill_viridis_c(
    na.value = "transparent"
  ) +
  labs(
    x = " ",
    y = " ",
    fill = "Scaled value"#,
    #title = "Spatial distribution of model predictors"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    panel.grid = element_blank()
  )

predictor_plot

ggsave(
  filename = file.path(
    "predictors.png"
  ),
  plot = predictor_plot,
  width = 12,
  height = 12,
  units = "in",
  dpi = 300
)
