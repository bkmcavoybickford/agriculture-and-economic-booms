

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

## Getting some reasonable breaks for a log scale
acrebreaks <- c(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000)

## Apricots as an example crop
ggplot(data = summarystats) + 
  geom_sf(aes(fill = Apricots), colour = NA) + theme_void() + 
  scale_fill_viridis_c(breaks = acrebreaks, label = acrebreaks,
                       trans = scales::pseudo_log_trans()) +
  labs(fill = "Acres Per County") +
  theme(text = element_text(family = "Times New Roman"),
        plot.margin = margin(5, 5, 5, 5, "pt"),
        plot.title = element_text(hjust = 0.5)) +
  coord_sf(expand = FALSE)
ggsave("Output/Figures/apricots_1978.png", width = 6, height = 4)

## Wheat as another example
ggplot(data = summarystats) + 
  geom_sf(aes(fill = Wheat), colour = NA) + theme_void() + 
  scale_fill_viridis_c(breaks = acrebreaks, label = acrebreaks,
                       trans = scales::pseudo_log_trans()) +
  labs(fill = "Acres Per County") +
  theme(text = element_text(family = "Times New Roman"),
        plot.margin = margin(5, 5, 5, 5, "pt"),
        plot.title = element_text(hjust = 0.5)) +
  coord_sf(expand = FALSE)
ggsave("Output/Figures/wheat_1978.png", width = 6, height = 4)

## Rice as my third example
ggplot(data = summarystats) + 
  geom_sf(aes(fill = Rice), colour = NA) + theme_void() + 
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

## Getting a map of states
nhgis_crs <- st_crs(shapes1970)

states <- states(cb = TRUE, year = 2020) |>
  filter(!STATEFP %in% c("02", "15", "60", "66", "69", "72", "78", "11")) |>
  st_transform(crs = nhgis_crs)

## Getting summary statistics by crop
summarystats_bycrop <- summarystats |>
  st_drop_geometry() |>
  pivot_longer(cols = !c(X_CENTROID, Y_CENTROID, statefip, counfip, countycode, 
                         NHGISST, NHGISCTY, pointcentroid, name, level, year),
               names_to = "crop", values_to = "production") |>
  select(-c(NHGISST, NHGISCTY, statefip, countycode, counfip)) |>
  group_by(crop) |>
  summarize(totalprod = sum(production),
            meanlong = weighted.mean(X_CENTROID, production, na.rm = TRUE),
            meanlat = weighted.mean(Y_CENTROID, production, na.rm = TRUE)) |>
  filter(totalprod != 0 & !is.nan(meanlong) & !is.nan(meanlat)) |>
  st_as_sf(coords = c("meanlong", "meanlat"),
           crs = nhgis_crs)

## Plotting them all
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

## Plotting just the ten biggest crops
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

countylevel_winsor <- countylevel_all |>
  mutate(croplandgrowth = winsorize(croplandgrowth_2012_1978),
         growth = winsorize(growth_2012_1978)) |>
  filter(!is.na(croplandgrowth) & !is.na(growth))

## Calculating the r
pearsonr_countycorr <- cor(countylevel_winsor$croplandgrowth, countylevel_winsor$growth)

## Making a plot
ggplot(data = countylevel_winsor) + geom_point(aes(x = croplandgrowth, y = growth)) +
  mytheme + labs(x = "% Growth in Cropland, 1978 to 2012", y = "% Growth in Employment, 1978 to 2012") +
  annotate("text", x = 0, y = 300,
           label = paste("r =", round(pearsonr_countycorr, 3)),
           family = "Times New Roman",
           size = 5)
ggsave("Output/Figures/growthcroplandcorr.png")

#####################################
### Making a correlation matrix
#####################################

## Doing it for farm variables
corr_farmvars <- countylevel_all |>
  select("# of Farms" = farmsgrowth_2012_1978, "Acres Farmland" = farmlandgrowth_2012_1978,
         "Avg. Acres per Farm" = farmsizegrowth_2012_1978, "Avg. Farm Value" = farmvaluegrowth_2012_1978,
         "Avg. Acre Farmland Value" = farmacrevaluegrowth_2012_1978,
         "Acres Cropland" = croplandgrowth_2012_1978, "Irrigated Acres" = irrlandgrowth_2012_1978,
         "Production Value" = prodvalgrowth_2012_1978,
         "People Employed" = growth_2012_1978) |>
  mutate(across(everything(), as.numeric)) |>
  dplyr::filter(if_all(everything(), is.finite)) |>
  cor() |>
  round(3)
corr_farmvars[upper.tri(corr_farmvars)] <- NA
corr_farmvars <- melt(corr_farmvars)

## Actually plotting the matrix
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

###############################
### Plots of national level variables over time
##############################

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

########################################
### Getting some choropleths for the total county-level data
########################################

## Making the data choroplethable
countylevel_choro <- countylevel_merged |>
  right_join(shapes1970, by = "countycode") |>
  st_as_sf() |>
  filter(NHGISST != "020" & NHGISST != "150")

## Making a choropleth of 1978-2012 employed pop growth
ggplot(data = countylevel_choro) + 
  geom_sf(aes(fill = norm_growth_2012_1978), colour = NA) + theme_void() + 
  scale_fill_viridis_c(breaks = c(-1,0,1), label = c(-1,0,1),
                       trans = scales::pseudo_log_trans(),
                       na.value = "grey") +
  labs(fill = "") +
  theme(text = element_text(family = "Times New Roman"),
        plot.margin = margin(5, 5, 5, 5, "pt"),
        plot.title = element_text(hjust = 0.5)) +
  coord_sf(expand = FALSE)
ggsave("Output/Figures/normgrowth_1978_2012.png", width = 6, height = 4)

## Doing the same for the shift share
ggplot(data = countylevel_choro) + 
  geom_sf(aes(fill = norm_shiftshare_2012_1978), colour = NA) + theme_void() + 
  scale_fill_viridis_c(breaks = c(-1,0,1), label = c(-1,0,1),
                       trans = scales::pseudo_log_trans(),
                       na.value = "grey") +
  labs(fill = "") +
  theme(text = element_text(family = "Times New Roman"),
        plot.margin = margin(5, 5, 5, 5, "pt"),
        plot.title = element_text(hjust = 0.5)) +
  coord_sf(expand = FALSE)
ggsave("Output/Figures/normshiftshare_1978_2012.png", width = 6, height = 4)

## Doing the same for cropland
ggplot(data = countylevel_choro) + 
  geom_sf(aes(fill = norm_croplandgrowth_2012_1978), colour = NA) + theme_void() + 
  scale_fill_viridis_c(breaks = c(-1,0,1), label = c(-1,0,1),
                       trans = scales::pseudo_log_trans(),
                       na.value = "grey") +
  labs(fill = "") +
  theme(text = element_text(family = "Times New Roman"),
        plot.margin = margin(5, 5, 5, 5, "pt"),
        plot.title = element_text(hjust = 0.5)) +
  coord_sf(expand = FALSE)
ggsave("Output/Figures/normcroplandgrowth_1978_2012.png", width = 6, height = 4)

## And irrigated land in case that's interesting
ggplot(data = countylevel_choro) + 
  geom_sf(aes(fill = norm_irrlandgrowth_2012_1978), colour = NA) + theme_void() + 
  scale_fill_viridis_c(breaks = c(-1,0,1), label = c(-1,0,1),
                       trans = scales::pseudo_log_trans(),
                       na.value = "grey") +
  labs(fill = "") +
  theme(text = element_text(family = "Times New Roman"),
        plot.margin = margin(5, 5, 5, 5, "pt"),
        plot.title = element_text(hjust = 0.5)) +
  coord_sf(expand = FALSE)
ggsave("Output/Figures/normirrlandgrowth_1978_2012.png", width = 6, height = 4)

## And farms
ggplot(data = countylevel_choro) + 
  geom_sf(aes(fill = norm_farmsgrowth_2012_1978), colour = NA) + theme_void() + 
  scale_fill_viridis_c(breaks = c(-1,0,1), label = c(-1,0,1),
                       trans = scales::pseudo_log_trans(),
                       na.value = "grey") +
  labs(fill = "") +
  theme(text = element_text(family = "Times New Roman"),
        plot.margin = margin(5, 5, 5, 5, "pt"),
        plot.title = element_text(hjust = 0.5)) +
  coord_sf(expand = FALSE)
ggsave("Output/Figures/normfarmsgrowth_1978_2012.png", width = 6, height = 4)

## And farm acre value
ggplot(data = countylevel_choro) + 
  geom_sf(aes(fill = norm_farmacrevaluegrowth_2012_1978), colour = NA) + theme_void() + 
  scale_fill_viridis_c(breaks = c(-1,0,1), label = c(-1,0,1),
                       trans = scales::pseudo_log_trans(),
                       na.value = "grey") +
  labs(fill = "") +
  theme(text = element_text(family = "Times New Roman"),
        plot.margin = margin(5, 5, 5, 5, "pt"),
        plot.title = element_text(hjust = 0.5)) +
  coord_sf(expand = FALSE)
ggsave("Output/Figures/normfarmacrevaluegrowth_1978_2012.png", width = 6, height = 4)

## And farmland
ggplot(data = countylevel_choro) + 
  geom_sf(aes(fill = norm_farmlandgrowth_2012_1978), colour = NA) + theme_void() + 
  scale_fill_viridis_c(breaks = c(-1,0,1), label = c(-1,0,1),
                       trans = scales::pseudo_log_trans(),
                       na.value = "grey") +
  labs(fill = "") +
  theme(text = element_text(family = "Times New Roman"),
        plot.margin = margin(5, 5, 5, 5, "pt"),
        plot.title = element_text(hjust = 0.5)) +
  coord_sf(expand = FALSE)
ggsave("Output/Figures/normfarmlandgrowth_1978_2012.png", width = 6, height = 4)

## And farm size
ggplot(data = countylevel_choro) + 
  geom_sf(aes(fill = norm_farmsizegrowth_2012_1978), colour = NA) + theme_void() + 
  scale_fill_viridis_c(breaks = c(-1,0,1), label = c(-1,0,1),
                       trans = scales::pseudo_log_trans(),
                       na.value = "grey") +
  labs(fill = "") +
  theme(text = element_text(family = "Times New Roman"),
        plot.margin = margin(5, 5, 5, 5, "pt"),
        plot.title = element_text(hjust = 0.5)) +
  coord_sf(expand = FALSE)
ggsave("Output/Figures/normfarmsizegrowth_1978_2012.png", width = 6, height = 4)

## And farm value
ggplot(data = countylevel_choro) + 
  geom_sf(aes(fill = norm_farmvaluegrowth_2012_1978), colour = NA) + theme_void() + 
  scale_fill_viridis_c(breaks = c(-1,0,1), label = c(-1,0,1),
                       trans = scales::pseudo_log_trans(),
                       na.value = "grey") +
  labs(fill = "") +
  theme(text = element_text(family = "Times New Roman"),
        plot.margin = margin(5, 5, 5, 5, "pt"),
        plot.title = element_text(hjust = 0.5)) +
  coord_sf(expand = FALSE)
ggsave("Output/Figures/normfarmvaluegrowth_1978_2012.png", width = 6, height = 4)

## And production value
ggplot(data = countylevel_choro) + 
  geom_sf(aes(fill = norm_prodvalgrowth_2012_1978), colour = NA) + theme_void() + 
  scale_fill_viridis_c(breaks = c(-1,0,1), label = c(-1,0,1),
                       trans = scales::pseudo_log_trans(),
                       na.value = "grey") +
  labs(fill = "") +
  theme(text = element_text(family = "Times New Roman"),
        plot.margin = margin(5, 5, 5, 5, "pt"),
        plot.title = element_text(hjust = 0.5)) +
  coord_sf(expand = FALSE)
ggsave("Output/Figures/normprodvalgrowth_1978_2012.png", width = 6, height = 4)

##########################
### Checking correlations between the quintile measures
##########################

quintcorr <- countylevel_all |>
  group_by(quintile_emppercap, quintile_agshare) |>
  filter(!is.na(quintile_emppercap) & !is.na(quintile_agshare))

quintcorr_sum <- quintcorr |>
  summarize(counties = n())
quintcorr_tot <- cor(quintcorr$quintile_agshare, quintcorr$quintile_emppercap)

ggplot(quintcorr_sum, aes(x = quintile_emppercap,
                      y = quintile_agshare,
                      fill = counties)) + geom_tile() +
  geom_text(aes(label = round(counties, 2)),
            family = "Times New Roman") +
  theme(axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        text = element_text(family = "Times New Roman")) +
  labs(x = "Employment/Area Quintile",
       y = "Ag. Employment Share Quintile",
       fill = "# of Counties",
       subtitle = paste0("Correlation = ", quintcorr_tot)) + scale_fill_viridis_c(begin = 0.4)
ggsave("Output/Figures/quintcorr.png")