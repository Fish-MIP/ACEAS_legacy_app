# Plot figures from per-year summary CSVs
# Creates biomass and yield time-series PNGs from the summary CSVs

library(ggplot2)
library(dplyr)
library(scales)

outdir <- "monte_carlo_2111_summaries"
biomass_csv <- file.path(outdir, "biomass_timeseries_summary_per_year_tonnes.csv")
yield_csv <- file.path(outdir, "yield_timeseries_summary_per_year_tonnes.csv")

if (!file.exists(biomass_csv) || !file.exists(yield_csv)) stop("Summary CSVs not found. Run extract_timeseries_summary.R first.")

biomass <- read.csv(biomass_csv, stringsAsFactors = FALSE)
yield <- read.csv(yield_csv, stringsAsFactors = FALSE)

# species order (smallest to largest) - ensure matches earlier ordering
species_order <- c(
  "mesozooplankton","other krill","other macrozooplankton","antarctic krill",
  "salps","mesopelagic fishes","bathypelagic fishes","shelf and coastal fishes",
  "flying birds","small divers","squids","toothfishes","leopard seals",
  "medium divers","large divers","minke whales","orca","sperm whales","baleen whales"
)

biomass$Species <- factor(biomass$Species, levels=species_order)

# Identify species with actual yields (filter to species with any max_t > 0)
species_with_yield <- unique(yield$Species[yield$max_t > 0])
cat("Species with yields:", paste(species_with_yield, collapse=", "), "\n")

# Filter yield data to only those species and build indicator for observed positives
yield <- yield %>% filter(Species %in% species_with_yield)
yield$Species <- factor(yield$Species, levels=species_order)

# Build indicator: has_obs_pos = TRUE when ObsYield_t > 0 for that Year/Species
yield <- yield %>%
  mutate(has_obs_pos = !is.na(ObsYield_t) & ObsYield_t > 0)

# Separate frames for ribbons and line (filter out model zeros when no observed positive)
yield_ribbon <- yield %>%
  filter(!(has_obs_pos == FALSE & (is.na(q95_t) | q95_t <= 0))) %>%
  mutate(
    q05_t = ifelse(q05_t > 0, q05_t, NA_real_),
    q25_t = ifelse(q25_t > 0, q25_t, NA_real_),
    q75_t = ifelse(q75_t > 0, q75_t, NA_real_),
    q95_t = ifelse(q95_t > 0, q95_t, NA_real_)
  ) %>%
  filter(!(is.na(q25_t) & is.na(q75_t) & is.na(q05_t) & is.na(q95_t)))

yield_line <- yield %>%
  filter(!(has_obs_pos == FALSE & (is.na(median_t) | median_t <= 0))) %>%
  mutate(median_t = ifelse(median_t > 0, median_t, NA_real_))

# Observed yield points: only positive values
yield_obs <- yield %>% filter(has_obs_pos) %>% select(Year, Species, ObsYield_t)

# Number of simulations: read from 'n' column (each row represents one Year/Species, n is count of sims)
n_sims <- max(yield$n, na.rm=TRUE)

# Option A: min-max ribbons (min_t, max_t)
cat('Creating biomass plot with min-max ribbons...\n')
p_biomass_minmax <- ggplot(biomass, aes(x=Year, y=median_t, color=Species, fill=Species)) +
  geom_ribbon(aes(ymin=min_t, ymax=max_t), alpha=0.15, color=NA) +
  geom_ribbon(aes(ymin=q25_t, ymax=q75_t), alpha=0.25, color=NA) +
  geom_line(size=0.8) +
  geom_point(data = biomass %>% filter(!is.na(ObsBiomass_t) & Year >= 2010 & Year <= 2020), aes(x=Year, y=ObsBiomass_t), inherit.aes=FALSE, color='black', size=1) +
  facet_wrap(~Species, scales='free_y') +
  scale_y_continuous(labels = function(x) scales::comma(x)) +
  theme_bw() +
  theme(legend.position='none', strip.text = element_text(face='bold')) +
  labs(title = sprintf('Monte Carlo: Biomass vs Observations ( %d sims )', n_sims), x='Year', y='Biomass [t]')

# Save biomass plot
ggsave('mc_analysis_output/biomass_timeseries_from_summary_minmax.png', p_biomass_minmax, width=12, height=10, dpi=300)
cat('Saved biomass_timeseries_from_summary_minmax.png\n')

# Option B: dual ribbons 5-95 and 25-75 (as original)
cat('Creating biomass plot with 5-95 and 25-75 ribbons...\n')
p_biomass_q <- ggplot(biomass, aes(x=Year, y=median_t, color=Species, fill=Species)) +
  geom_ribbon(aes(ymin=q05_t, ymax=q95_t), alpha=0.15, color=NA) +
  geom_ribbon(aes(ymin=q25_t, ymax=q75_t), alpha=0.25, color=NA) +
  geom_line(size=0.8) +
  geom_point(data = biomass %>% filter(!is.na(ObsBiomass_t) & Year >= 2010 & Year <= 2020), aes(x=Year, y=ObsBiomass_t), inherit.aes=FALSE, color='black', size=1) +
  facet_wrap(~Species, scales='free_y') +
  scale_y_continuous(labels = function(x) scales::comma(x)) +
  theme_bw() +
  theme(legend.position='none', strip.text = element_text(face='bold')) +
  labs(title = sprintf('Monte Carlo: Biomass vs Observations ( %d sims )', n_sims), x='Year', y='Biomass [t]')

ggsave('mc_analysis_output/biomass_timeseries_from_summary_q.png', p_biomass_q, width=12, height=10, dpi=300)
cat('Saved biomass_timeseries_from_summary_q.png\n')

# Now yield plots. Y-axis log scale with smart labels
label_dynamic_decimals <- function(x) {
  sci <- scales::label_scientific(digits = 1)
  vapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    if (v <= 0) return("0")
    vt <- v
    if (vt >= 1) {
      paste0(formatC(vt, format = "f", digits = 0, big.mark = ","), " t")
    } else if (vt < 1e-6) {
      paste0(sci(vt), " t")
    } else {
      dp <- max(1, ceiling(-log10(vt)))
      dp <- min(dp, 6)
      paste0(formatC(vt, format = "f", digits = dp), " t")
    }
  }, character(1))
}

# Determine x-axis start year (use baleen start if available, else global min observed)
baleen_start_obs <- yield_obs %>%
  filter(Species == "baleen whales") %>%
  summarise(min_year = min(Year, na.rm = TRUE)) %>%
  pull(min_year)
global_start_obs <- yield_obs %>%
  summarise(min_year = min(Year, na.rm = TRUE)) %>%
  pull(min_year)
start_year <- if (length(baleen_start_obs) == 1 && is.finite(baleen_start_obs)) {
  baleen_start_obs
} else if (length(global_start_obs) == 1 && is.finite(global_start_obs)) {
  global_start_obs
} else {
  1930
}
end_year <- max(yield$Year, na.rm = TRUE)

# Create yield plot with dual ribbons (5-95 & 25-75)
cat('Creating yield plot with dual ribbons (5-95 & 25-75)...\n')
p_yield_q <- ggplot() +
  geom_ribbon(data = yield_ribbon,
              aes(x = Year, ymin = q05_t, ymax = q95_t, fill = Species),
              alpha = 0.2) +
  geom_ribbon(data = yield_ribbon,
              aes(x = Year, ymin = q25_t, ymax = q75_t, fill = Species),
              alpha = 0.3) +
  geom_line(data = yield_line,
            aes(x = Year, y = median_t, color = Species),
            linewidth = 1.0, linetype = "solid", lineend = "round") +
  geom_point(data = yield_obs,
             aes(x = Year, y = ObsYield_t, colour = Species),
             size = 1) +
  geom_point(data = yield_obs,
             aes(x = Year, y = ObsYield_t),
             shape = 1, size = 1, colour = "black") +
  geom_vline(xintercept = 1961, linetype = "dashed") +
  geom_vline(xintercept = 2010, linetype = "dashed") +
  scale_y_log10(breaks = scales::log_breaks(n = 6), labels = label_dynamic_decimals) +
  coord_cartesian(xlim = c(start_year, end_year)) +
  facet_wrap(~Species, scales = "free_y") +
  theme_bw() +
  theme(legend.position = "none", strip.text = element_text(face = "bold")) +
  labs(
    x = "Year", 
    y = "Yield [t/year]", 
    title = sprintf("Monte Carlo: Yield vs Observations (%d sims)", n_sims)
  )

ggsave('mc_analysis_output/yield_timeseries_from_summary_q.png', p_yield_q, width=12, height=10, dpi=300)
cat('Saved yield_timeseries_from_summary_q.png\n')

cat('Plotting complete.\n')
