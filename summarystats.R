library(maps)
library(reshape2)
library(ggrepel)

## Setting a theme
mytheme <- theme(axis.ticks = element_blank(),
                 panel.grid.major = element_line(color = "gray"),
                 panel.grid.minor = element_blank(),
                 panel.background = element_blank(),
                 axis.line = element_line(colour = "black"),
                 text = element_text(family = "Times New Roman"))

#############################
### Making figures of crop locations
#############################

acrebreaks <- c(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000)

ggplot(data = summarystats) + 
  geom_sf(aes(fill = apricots), colour = NA) + theme_void() + 
  scale_fill_viridis_c(breaks = acrebreaks, label = acrebreaks,
                       trans = scales::pseudo_log_trans()) +
  labs(fill = "Acres Per County") +
  theme(text = element_text(family = "Times New Roman"),
        plot.margin = margin(5, 5, 5, 5, "pt"),
        plot.title = element_text(hjust = 0.5)) +
  coord_sf(expand = FALSE)
ggsave("Output/Figures/apricots_1978.png", width = 6, height = 4)

ggplot(data = summarystats) + 
  geom_sf(aes(fill = wheat), colour = NA) + theme_void() + 
  scale_fill_viridis_c(breaks = acrebreaks, label = acrebreaks,
                       trans = scales::pseudo_log_trans()) +
  labs(fill = "Acres Per County") +
  theme(text = element_text(family = "Times New Roman"),
        plot.margin = margin(5, 5, 5, 5, "pt"),
        plot.title = element_text(hjust = 0.5)) +
  coord_sf(expand = FALSE)
ggsave("Output/Figures/wheat_1978.png", width = 6, height = 4)

ggplot(data = summarystats) + 
  geom_sf(aes(fill = rice), colour = NA) + theme_void() + 
  scale_fill_viridis_c(breaks = acrebreaks, label = acrebreaks,
                       trans = scales::pseudo_log_trans()) +
  labs(fill = "Acres Per County", title = "Rice") +
  theme(text = element_text(family = "Times New Roman"),
        plot.margin = margin(5, 5, 5, 5, "pt"),
        plot.title = element_text(hjust = 0.5)) +
  coord_sf(expand = FALSE)
ggsave("Output/Figures/rice_1978.png", width = 6, height = 4)

######################
### Getting some statistics on total production
######################

nhgis_crs <- st_crs(shapes1970)

states <- st_as_sf(map("state", plot = FALSE, fill = TRUE)) |>
  st_transform(states, crs = nhgis_crs)

summarystats_bycrop <- summarystats |>
  st_drop_geometry() |>
  pivot_longer(cols = !c(X_CENTROID, Y_CENTROID, statefip, counfip, countycode, 
                         NHGISST, NHGISCTY, pointcentroid),
               names_to = "crop", values_to = "production") |>
  select(-c(NHGISST, NHGISCTY, statefip, countycode, counfip)) |>
  group_by(crop) |>
  summarize(totalprod = sum(production),
            meanlong = weighted.mean(X_CENTROID, production, na.rm = TRUE),
            meanlat = weighted.mean(Y_CENTROID, production, na.rm = TRUE)) |>
  filter(totalprod != 0 & !is.nan(meanlong) & !is.nan(meanlat)) |>
  st_as_sf(coords = c("meanlong", "meanlat"),
           crs = nhgis_crs)

ggplot() + geom_sf(data = states, fill = "beige") + 
  geom_sf(data = summarystats_bycrop, aes(color = totalprod)) + theme_void() +
  scale_color_viridis_c(breaks = acrebreaks, label = acrebreaks,
                       trans = scales::pseudo_log_trans(),
                       end = 0.9) +
  labs(color = "Acres, 1978") +
  theme(text = element_text(family = "Times New Roman"),
        plot.margin = margin(5, 5, 5, 5, "pt"),
        plot.title = element_text(hjust = 0.5)) +
  coord_sf(expand = FALSE)
ggsave("Output/Figures/cropcentroids.png", width = 6, height = 4)

summarystats_bycrop_small <- summarystats_bycrop |>
  arrange(desc(totalprod)) |>
  slice_head(n = 12) |>
  mutate(crop = str_to_sentence(crop),
         crop = if_else(crop == "Graincorn", "Grain Corn", crop))

ggplot() + geom_sf(data = states, fill = "beige") + 
  geom_sf(data = summarystats_bycrop_small, aes(color = totalprod)) + theme_void() +
  geom_text_repel(data = summarystats_bycrop_small,
                  aes(label = crop, geometry = geometry),
                  stat = "sf_coordinates", size = 3,
                  family = "Times New Roman") +
  scale_color_viridis_c(end = 0.9) +
  labs(color = "Acres, 1978") +
  theme(text = element_text(family = "Times New Roman"),
        plot.margin = margin(5, 5, 5, 5, "pt"),
        plot.title = element_text(hjust = 0.5)) +
  coord_sf(expand = FALSE)
ggsave("Output/Figures/cropcentroids_small.png", width = 6, height = 4)

###############################
### Getting county-level correlations
##############################

countycorr <- countylevel_all |>
  filter(croplandgrowth_2012_1978 <= 100 & growth_2012_1978 <= 300)

pearsonr_countycorr <- cor(countycorr$croplandgrowth_2012_1978, countycorr$growth_2012_1978)

ggplot(data = countycorr) + geom_point(aes(x = croplandgrowth_2012_1978, y = growth_2012_1978)) +
  mytheme + labs(x = "% Growth in Cropland, 1978 to 2012", y = "% Growth in Employment, 1978 to 2012") +
  annotate("text", x = 50, y = 375,
           label = paste("r =", round(pearsonr_countycorr, 3)),
           family = "Times New Roman",
           size = 5)
ggsave("Output/Figures/growthcroplandcorr.png")

#####################################
### Making a correlation matrix
#####################################

corr_farmvars <- countylevel_all |>
  select("# of Farms" = farmsgrowth_2012_1978, "Acres Farmland" = farmlandgrowth_2012_1978,
         "Avg. Acres per Farm" = farmsizegrowth_2012_1978, "Avg. Farm Value" = farmvaluegrowth_2012_1978,
         "Avg. Acre Farmland Value" = farmacrevaluegrowth_2012_1978,
         "Acres Cropland" = croplandgrowth_2012_1978, "Irrigated Acres" = irrlandgrowth_2012_1978,
         "Products Sold" = prodvalgrowth_2012_1978,
         "People Employed" = growth_2012_1978) |>
  mutate(across(everything(), as.numeric)) |>
  dplyr::filter(if_all(everything(), is.finite)) |>
  cor() |>
  round(3)
corr_farmvars[upper.tri(corr_farmvars)] <- NA
corr_farmvars <- melt(corr_farmvars)

ggplot(data = corr_farmvars, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() + labs(x = "Variable 1", y = "Variable 2", fill = "Correlation") +
  theme_minimal() +
  coord_fixed() + scale_fill_viridis_c(begin = 0.5, end = 1) +
  geom_text(aes(Var1, Var2, label = value), color = "black", size = 3) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.56, 0.7),
    legend.direction = "horizontal",
    axis.text.x = element_text(angle = 45, vjust = 1, 
                               hjust = 1),
    text = element_text(family = "Times New Roman")) +
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))
ggsave("Output/Figures/corr_farmvars.png")

## With fewer variables for the slides
corr_farmvars_small <- countylevel_all |>
  select("# of Farms" = farmsgrowth_2012_1978,
         "Avg. Acres per Farm" = farmsizegrowth_2012_1978,
         "Avg. Acre Farmland Value" = farmacrevaluegrowth_2012_1978,
         "Acres Cropland" = croplandgrowth_2012_1978,
         "People Employed" = growth_2012_1978) |>
  mutate(across(everything(), as.numeric)) |>
  filter(if_all(everything(), is.finite)) |>
  cor() |>
  round(3)
corr_farmvars_small[upper.tri(corr_farmvars_small)] <- NA
corr_farmvars_small <- melt(corr_farmvars_small)

ggplot(data = corr_farmvars_small, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() + labs(x = "Variable 1", y = "Variable 2", fill = "Correlation, 1978-2012") +
  theme_minimal() +
  coord_fixed() + scale_fill_viridis_c(begin = 0.5, end = 1) +
  geom_text(aes(Var1, Var2, label = value), color = "black", size = 3) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.56, 0.7),
    legend.direction = "horizontal",
    axis.text.x = element_text(angle = 45, vjust = 1, 
                               hjust = 1),
    text = element_text(family = "Times New Roman")) +
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))
ggsave("Output/Figures/corr_farmvars_small.png")

## For growth variables
corr_1980_1990 <- stats_1980_1990 |>
  select("Population" = popgrowth, "Number of Establishments" = estabsgrowth, 
         "Total Payroll" = payrollgrowth, "Number of Households" = householdgrowth, 
         "Housing Stock" = housinggrowth, "Per Capita Income" = incgrowth, 
         "Number of Employed" = empgrowth) |>
  mutate(across(everything(), as.numeric)) |>
  filter(if_all(everything(), is.finite)) |>
  cor() |>
  round(3)
corr_1980_1990[upper.tri(corr_1980_1990)] <- NA
corr_1980_1990 <- melt(corr_1980_1990)

ggplot(data = corr_1980_1990, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() + labs(x = "Variable 1", y = "Variable 2", fill = "Correlation") +
  theme_minimal() +
  coord_fixed() + scale_fill_viridis_c(begin = 0.5, end = 1) +
  geom_text(aes(Var1, Var2, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.56, 0.7),
    legend.direction = "horizontal",
    axis.text.x = element_text(angle = 45, vjust = 1, 
                               hjust = 1),
    text = element_text(family = "Times New Roman")) +
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))
ggsave("Output/Figures/corr_1980_1990.png")

## Making a plot of cropland
ggplot(data = cropland, aes(x = year, y = totalcropland)) +
  geom_line() + geom_point(color = "red") + 
  mytheme + labs(x = "Year", y = "Total Acres Cropland") +
  scale_y_continuous(labels = scales::comma)
ggsave("Output/Figures/totalcropland.png", width = 6, height = 4)

## And one of total farms
ggplot(data = totfarms, aes(x = year, y = totalfarms)) +
  geom_line() + geom_point(color = "red") + 
  mytheme + labs(x = "Year", y = "Total Farms") +
  scale_y_continuous(labels = scales::comma)
ggsave("Output/Figures/totalfarms.png", width = 6, height = 4)

####################################
### Getting a correlation matrix for precipitation
####################################

corr_precip <- countylevel_all |>
  select("Norm. Employment Growth, 1978-2012" = norm_growth_2012_1978,
         "Norm. Employment Growth, 1978-2007" = norm_growth_2007_1978,
         "Norm. Employment Growth, 1978-1982" = norm_growth_1982_1978,
         "Precipitation" = precip,
         "Drought, 1978-2012" = drought_1978_2012,
         "Norm. Cropland Growth, 1978-2012" = norm_croplandgrowth_2012_1978) |>
  mutate(across(everything(), as.numeric)) |>
  filter(if_all(everything(), is.finite)) |>
  cor() |>
  round(3)
corr_precip[upper.tri(corr_precip)] <- NA
corr_precip <- melt(corr_precip)

ggplot(data = corr_precip, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() + labs(x = "Variable 1", y = "Variable 2", fill = "Correlation") +
  theme_minimal() +
  coord_fixed() + scale_fill_viridis_c(begin = 0.5, end = 1) +
  geom_text(aes(Var1, Var2, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.56, 0.7),
    legend.direction = "horizontal",
    axis.text.x = element_text(angle = 45, vjust = 1, 
                               hjust = 1),
    text = element_text(family = "Times New Roman")) +
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))
ggsave("Output/Figures/corr_precip.png")