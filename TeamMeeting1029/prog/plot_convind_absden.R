#!/usr/bin/env Rscript
#==============================================================================
# Team meeting 2026/10/29: convergence / discrepancy indicators with an absolute-value denominator
#   variable: Emi_CO2_Ene_and_Ind_Pro (World), all scenarios of DEF_list.txt
#
# Indicators (Fujimori et al.; prog_misc/labseminar1021/plot_labseminar1021.R), t = 2020, 2025, ..., 2100
#   ConvInd_{m,n} = sum_t |X_{m,t,n} - X_{m,t,n-1}| / D      m = AIM-Hub or AIM-Tech, n = iteration 1..5
#   DiscInd_n     = sum_t |X_{Hub,t,n} - X_{Tech,t,n}| / D    n = iteration 0..5
#   denominator D
#     original : sum_t (X_a + X_b) / 2          (NA if D <= 0, as in labseminar1021)
#     abs      : sum_t (|X_a| + |X_b|) / 2      (always > 0, so series around zero / negative are kept)
#   with net-negative emissions the original D shrinks (or turns negative), so the original indicator
#   is larger than the abs version; both are equal when X >= 0 in all years.
#
# Data (read only)
#   AIM-Hub  : Model/AIMHub/output/iiasa_database/txt/global_17_IAMC.csv
#              "Emissions|CO2|Energy and Industrial Processes", <Hub scenario>_DEF_global{n} (n = 0: _No)
#   AIM-Tech : output/gdx/DEF_global{n}Ite2Hub.gdx, EMFtemp2 (n >= 1)
#              Model/AIMTechnology/output/DEF_global0/cons/main/merged_output.gdx, data_all (n = 0)
#   i0 of DiscInd is AIM-Hub stand-alone vs AIM-Tech run 0 (labseminar1021 used AIM-Tech run 1 for i0)
#
# Usage (single-threaded, about 1 min):
#   OMP_NUM_THREADS=1 /home/rhan/R-4.3.2/bin/Rscript plot_convind_absden.R
# Output (../output)
#   ConvInd_DiscInd_absden_EIP.png
#==============================================================================
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(scales)
})
options(readr.show_col_types = FALSE, warn = 1)

#---------------- settings ----------------
ITE     <- 0:5
REGION  <- "World"
YEARS   <- seq(2020, 2100, 5)
RUN     <- "DEF_global"
CONVIND_TOL <- 0.05                        # same reference lines as labseminar1021
ZERO_TOL    <- 1e-10                       # indicators below this (machine precision) are not drawn on the log axis
DISCIND_TOL <- 0.10
# IntTool scenario -> AIM-Hub scenario / AIM-Tech scenario (data/ScenarioSet/DEF/DEF_list.txt)
SCEN_MAP <- tribble(
  ~Scenario,  ~Hub,                                  ~Tech,
  "NoPOL",    "SSP2_BaU_NoCC",                       "NoPOL",
  "Opt1.5C",  "SSP2_500C_2020NDC-low_NoCC",          "500C",
  "ZF2050",   "SSP2_500C_2020NDC-low-Sus-a_NoCC",    "500C-DEF3050",
  "ZF2060",   "SSP2_500C_2020NDC-low-Sus-b_NoCC",    "500C-DEF3060",
  "ZF2070",   "SSP2_500C_2020NDC-low-Sus-c_NoCC",    "500C-DEF3070",
  "ZF2080",   "SSP2_500C_2020NDC-low-Sus-d_NoCC",    "500C-DEF3080",
  "ZF2090",   "SSP2_500C_2020NDC-low-Sus-e_NoCC",    "500C-DEF3090",
  "ZF2100",   "SSP2_500C_2020NDC-low-Sus-f_NoCC",    "500C-DEF3000"
)
GDXDUMP <- Sys.getenv("GDXDUMP", "/home/rhan/gams44.4_linux_x64_64_sfx/gdxdump")

#---------------- paths ----------------
file_arg   <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
SCRIPT_DIR <- if (length(file_arg)) dirname(normalizePath(sub("^--file=", "", file_arg))) else getwd()
INTTOOL_DIR <- normalizePath(file.path(SCRIPT_DIR, "../../.."))
HUB_CSV <- file.path(INTTOOL_DIR, "Model/AIMHub/output/iiasa_database/txt/global_17_IAMC.csv")
GDX_DIR <- file.path(INTTOOL_DIR, "output/gdx")
TECH_OUT_DIR <- file.path(INTTOOL_DIR, "Model/AIMTechnology/output")
FIG_DIR <- normalizePath(file.path(SCRIPT_DIR, "../output"), mustWork = FALSE)
dir.create(FIG_DIR, showWarnings = FALSE)

#---------------- AIM-Hub ----------------
# grep for the exact variable field: a plain pattern would also match "Gross Emissions|CO2|..."
hub_cmd <- sprintf("(head -1 '%s'; grep -F ',\"%s\",\"Emissions|CO2|Energy and Industrial Processes\",' '%s')",
                   HUB_CSV, REGION, HUB_CSV)
hub <- read_csv(pipe(hub_cmd), col_types = cols(.default = col_character())) %>%
  mutate(SCENARIO = sub("_No$", paste0("_", RUN, "0"), SCENARIO)) %>%   # "_No" = AIM-Hub stand-alone (iteration 0)
  filter(grepl(paste0("_", RUN, "[0-9]+$"), SCENARIO)) %>%
  pivot_longer(matches("^[0-9]{4}$"), names_to = "Year", values_to = "Value") %>%
  mutate(Year = as.integer(Year), Value = as.numeric(Value),
         Hub = sub(paste0("_", RUN, "[0-9]+$"), "", SCENARIO),
         Iteration = as.integer(sub(paste0(".*_", RUN), "", SCENARIO))) %>%
  inner_join(SCEN_MAP %>% select(Scenario, Hub), by = "Hub") %>%
  filter(Iteration %in% ITE, Year %in% YEARS) %>%
  transmute(Model = "AIM-Hub", Scenario, Iteration, Year, Value)

#---------------- AIM-Technology ----------------
read_tech <- function(k) {
  if (k == 0) {
    gdx <- file.path(TECH_OUT_DIR, sprintf("%s0/cons/main/merged_output.gdx", RUN)); sym <- "data_all"
  } else {
    gdx <- file.path(GDX_DIR, sprintf("%s%dIte2Hub.gdx", RUN, k)); sym <- "EMFtemp2"
  }
  if (!file.exists(gdx)) { warning("not found: ", gdx); return(NULL) }
  cmd <- sprintf("'%s' '%s' symb=%s format=csv | grep -F ',\"%s\",\"Emi_CO2_Ene_and_Ind_Pro\",'",
                 GDXDUMP, gdx, sym, REGION)
  read_csv(pipe(cmd), col_names = c("Tech", "Region", "Variable", "Year", "Value"),
           col_types = "cccid") %>%
    mutate(Iteration = k)
}
tech <- bind_rows(lapply(ITE, read_tech)) %>%
  inner_join(SCEN_MAP %>% select(Scenario, Tech), by = "Tech") %>%
  filter(Year %in% YEARS) %>%
  transmute(Model = "AIM-Technology", Scenario, Iteration, Year, Value)

#---------------- indicators ----------------
# a, b: the two series compared (missing record = 0 in GDX)
ind_calc <- function(d) {
  d %>%
    mutate(a = coalesce(a, 0), b = coalesce(b, 0)) %>%
    summarise(num = sum(abs(a - b)),
              den_original = sum((a + b) / 2),
              den_abs = sum((abs(a) + abs(b)) / 2),
              n_year = n(), .groups = "drop") %>%
    mutate(original = ifelse(den_original > 0, num / den_original, NA_real_),
           abs = ifelse(den_abs > 0, num / den_abs, NA_real_))
}

conv <- bind_rows(hub, tech) %>%
  group_by(Model, Scenario, Year) %>%
  arrange(Iteration, .by_group = TRUE) %>%
  mutate(b = lag(Value), prev_it = lag(Iteration)) %>%
  ungroup() %>%
  filter(!is.na(prev_it), prev_it == Iteration - 1) %>%
  rename(a = Value) %>%
  group_by(Model, Scenario, Iteration) %>%
  ind_calc() %>%
  mutate(Indicator = paste0("ConvInd: ", Model))

disc <- full_join(hub %>% select(Scenario, Iteration, Year, a = Value),
                  tech %>% select(Scenario, Iteration, Year, b = Value),
                  by = c("Scenario", "Iteration", "Year")) %>%
  group_by(Scenario, Iteration) %>%
  ind_calc() %>%
  mutate(Indicator = "DiscInd: AIM-Hub vs AIM-Tech")

res <- bind_rows(conv, disc) %>%
  select(Indicator, Scenario, Iteration, num, den_original, den_abs, original, abs, n_year) %>%
  arrange(Indicator, factor(Scenario, levels = SCEN_MAP$Scenario), Iteration)
nbad <- sum(is.na(res$original))
if (nbad > 0) message(sprintf("original denominator <= 0 in %d cases (set to NA)", nbad))

#---------------- figure ----------------
ROWS <- c("ConvInd: AIM-Hub", "ConvInd: AIM-Technology", "DiscInd: AIM-Hub vs AIM-Tech")
DEN_LABEL <- c(original = "original: sum (X_a + X_b)/2", abs = "abs: sum (|X_a| + |X_b|)/2")
d <- res %>%
  pivot_longer(c(original, abs), names_to = "Denominator", values_to = "Val") %>%
  filter(!is.na(Val), Val > ZERO_TOL) %>%
  mutate(Indicator = factor(Indicator, levels = ROWS),
         Scenario = factor(Scenario, levels = SCEN_MAP$Scenario),
         Denominator = factor(DEN_LABEL[Denominator], levels = DEN_LABEL))
tol <- tibble(Indicator = factor(ROWS, levels = ROWS), tol = c(CONVIND_TOL, CONVIND_TOL, DISCIND_TOL) * 100)

p <- ggplot(d, aes(x = Iteration, y = Val * 100, colour = Denominator)) +
  geom_hline(data = tol, aes(yintercept = tol), linetype = "dashed", colour = "grey40") +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.3) +
  facet_grid(Indicator ~ Scenario, scales = "free_y") +
  scale_y_log10(labels = label_number(drop0trailing = TRUE)) +
  scale_x_continuous(breaks = ITE) +
  scale_colour_manual(values = c("#D55E00", "#0072B2")) +
  labs(x = "Iteration (ConvInd: n-1 -> n, DiscInd: Hub n vs Tech n)", y = "Indicator (%, log scale)", colour = "Denominator",
       title = sprintf("Emi_CO2_Ene_and_Ind_Pro (%s): ConvInd / DiscInd with original and absolute-value denominators", REGION),
       subtitle = sprintf("t = %d-%d (5-year steps); dashed line = %g%% (ConvInd), %g%% (DiscInd)",
                          min(YEARS), max(YEARS), CONVIND_TOL * 100, DISCIND_TOL * 100)) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        strip.text.y = element_text(size = 8))
# no X11 on the compute nodes (and ragg cannot be loaded), so the base cairo device is used
ggsave(file.path(FIG_DIR, "ConvInd_DiscInd_absden_EIP.png"), p, width = 18, height = 8, dpi = 200,
       bg = "white", device = grDevices::png, type = "cairo")

cat("figure:", file.path(FIG_DIR, "ConvInd_DiscInd_absden_EIP.png"), "\n")
