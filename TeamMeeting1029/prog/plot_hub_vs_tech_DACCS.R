#!/usr/bin/env Rscript
#==============================================================================
# Team meeting 2026/10/29: AIM-Hub vs AIM-Technology, Car_Rem_Dir_Air_Cap_wit_CCS (World)
#
# One panel per iteration (columns) and scenario (rows); each panel has two lines,
# AIM-Hub and AIM-Technology, of the same iteration.
#   AIM-Hub  : Model/AIMHub/output/iiasa_database/txt/global_17_IAMC.csv
#              "Carbon Removal|Geological Storage|Direct Air Capture", scenario <Hub scenario>_DEF_global{k}
#              (= Car_Rem_Dir_Air_Cap_wit_CCS in Model/AIMHub/output/iiasa_database/gdx/global_17_IAMC.gdx,
#               which Hub2Tech.gms reads; the CSV is used because dumping the gdx takes minutes)
#              (k = 0: <Hub scenario>_No, AIM-Hub stand-alone)
#   AIM-Tech : output/gdx/DEF_global{k}Ite2Hub.gdx, EMFtemp2(SCENARIO,RCGE,VEMF,Y)
#              AIM-Hub iteration k is solved with DEF_global{k}Ite2Hub.gdx, so both are iteration k.
#              k = 0 has no Ite2Hub.gdx: Model/AIMTechnology/output/DEF_global0/cons/main/merged_output.gdx,
#              data_all (AIM-Tech run without AIM-Hub input)
#
# Usage (single-threaded, about 1 min):
#   OMP_NUM_THREADS=1 /home/rhan/R-4.3.2/bin/Rscript plot_hub_vs_tech_DACCS.R
# Output (../output)
#   DACCS_hub_vs_tech_by_iteration.png
#==============================================================================
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
})
options(readr.show_col_types = FALSE, warn = 1)

#---------------- settings ----------------
SCEN    <- c("Opt1.5C", "ZF2050")          # IntTool scenario names (data/ScenarioSet/DEF/DEF_list.txt)
ITE     <- 0:5                             # iterations (DEF.sh: LastIte=5; 0 = before data exchange)
REGION  <- "World"
YEARS   <- seq(2020, 2100, 5)              # AIM-Tech values before 2017 are set to zero in Tech2Hub.gms
RUN     <- "DEF_global"
# IntTool scenario -> AIM-Hub scenario / AIM-Tech scenario
HUBSCEN  <- c("Opt1.5C" = "SSP2_500C_2020NDC-low_NoCC", "ZF2050" = "SSP2_500C_2020NDC-low-Sus-a_NoCC")
TECHSCEN <- c("Opt1.5C" = "500C",                       "ZF2050" = "500C-DEF3050")
HUBVAR   <- "Carbon Removal|Geological Storage|Direct Air Capture"
TECHVAR  <- "Car_Rem_Dir_Air_Cap_wit_CCS"
GDXDUMP  <- Sys.getenv("GDXDUMP", "/home/rhan/gams44.4_linux_x64_64_sfx/gdxdump")

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
# grep for the exact variable field (a plain pattern would also match sub-variables)
hub_cmd <- sprintf("(head -1 '%s'; grep -F ',\"%s\",\"%s\",' '%s')",
                   HUB_CSV, REGION, HUBVAR, HUB_CSV)
hub <- read_csv(pipe(hub_cmd), col_types = cols(.default = col_character())) %>%
  mutate(SCENARIO = sub("_No$", paste0("_", RUN, "0"), SCENARIO)) %>%   # "_No" = AIM-Hub stand-alone (iteration 0)
  filter(grepl(paste0("_", RUN, "[0-9]+$"), SCENARIO)) %>%
  pivot_longer(matches("^[0-9]{4}$"), names_to = "Year", values_to = "Value") %>%
  mutate(Year = as.integer(Year), Value = as.numeric(Value),
         Hub = sub(paste0("_", RUN, "[0-9]+$"), "", SCENARIO),
         Iteration = as.integer(sub(paste0(".*_", RUN), "", SCENARIO))) %>%
  filter(Hub %in% HUBSCEN, Iteration %in% ITE, Year %in% YEARS) %>%
  mutate(Scenario = names(HUBSCEN)[match(Hub, HUBSCEN)], Model = "AIM-Hub") %>%
  select(Model, Scenario, Iteration, Year, Value)

#---------------- AIM-Technology ----------------
read_tech <- function(k) {
  if (k == 0) {
    gdx <- file.path(TECH_OUT_DIR, sprintf("%s0/cons/main/merged_output.gdx", RUN)); sym <- "data_all"
  } else {
    gdx <- file.path(GDX_DIR, sprintf("%s%dIte2Hub.gdx", RUN, k)); sym <- "EMFtemp2"
  }
  if (!file.exists(gdx)) { warning("not found: ", gdx); return(NULL) }
  cmd <- sprintf("'%s' '%s' symb=%s format=csv | grep -F ',\"%s\",\"%s\",'",
                 GDXDUMP, gdx, sym, REGION, TECHVAR)
  # GDX does not store zeros, so years without capture are filled with 0
  read_csv(pipe(cmd), col_names = c("Tech", "Region", "Variable", "Year", "Value"),
           col_types = "cccid") %>%
    filter(Tech %in% TECHSCEN, Year %in% YEARS) %>%
    complete(Tech = unname(TECHSCEN), Year = YEARS, fill = list(Value = 0)) %>%
    mutate(Iteration = k)
}
tech <- bind_rows(lapply(ITE, read_tech)) %>%
  filter(Tech %in% TECHSCEN, Year %in% YEARS) %>%
  mutate(Scenario = names(TECHSCEN)[match(Tech, TECHSCEN)], Model = "AIM-Technology") %>%
  select(Model, Scenario, Iteration, Year, Value)

#---------------- data ----------------
df <- bind_rows(hub, tech) %>%
  mutate(Scenario = factor(Scenario, levels = SCEN),
         Model = factor(Model, levels = c("AIM-Hub", "AIM-Technology")))

#---------------- figure ----------------
p <- ggplot(df, aes(x = Year, y = Value, colour = Model)) +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 0.9) +
  facet_grid(Scenario ~ Iteration, labeller = labeller(Iteration = function(x) paste0("Iteration ", x))) +
  scale_colour_manual(values = c("AIM-Hub" = "#D55E00", "AIM-Technology" = "#0072B2")) +
  scale_x_continuous(breaks = seq(2020, 2100, 20)) +
  labs(x = "Year", y = paste0(TECHVAR, " (Mt CO2/yr)"), colour = NULL,
       title = sprintf("Carbon removal by direct air capture with CCS (%s)", REGION)) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank())
# no X11 on the compute nodes (and ragg cannot be loaded), so the base cairo device is used
ggsave(file.path(FIG_DIR, "DACCS_hub_vs_tech_by_iteration.png"), p, width = 15, height = 6, dpi = 200,
       bg = "white", device = grDevices::png, type = "cairo")

cat("figure:", file.path(FIG_DIR, "DACCS_hub_vs_tech_by_iteration.png"), "\n")
