## ---- code/1-main.R ----------------------------------------------------------
# =========================================================
# 0. Beginn Logfile
# =========================================================

log_dir <- here::here("output", "logs")
if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)

logfile <- file.path(log_dir, paste0("run_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".log"))

sink(logfile, split = TRUE)   


# =========================================================
# 1. Rohdaten laden (SOEP .rds-Dateien)
# =========================================================

pequiv   <- readRDS(here::here("data", "raw", "pequiv.rds"))
pgen     <- readRDS(here::here("data", "raw", "pgen.rds"))
ppathl   <- readRDS(here::here("data", "raw", "ppathl.rds"))
hbrutto  <- readRDS(here::here("data", "raw", "hbrutto.rds"))
pl       <- readRDS(here::here("data", "raw", "pl.rds"))


# =========================================================
# 2. Datensätze reduzieren (auf nur relevante Spalten)
# =========================================================

pequiv_red <- pequiv %>% select(cid, hid, pid, syear, d11101, d11102ll, d11107)

pgen_red <- pgen %>% select(cid, hid, pid, syear, pgtatzeit, pgnace, pgnace2, 
            pglabgro, pgbilzeit, pgemplst, pgfamstd, pgexpft, pgexppt)

ppathl_red <- ppathl %>% select(cid, hid, pid, syear, phrf, phrf0, phrf1)

pl_red <- pl %>% select(cid, hid, pid, syear, ple0008, plh0178, plh0182)

hbrutto_red <- hbrutto %>% select(cid, hid, syear, bula_h)


# =================================================================
# 3. Merge-Vorbereitung: Duplikate in ID-Jahr-Kombinationen prüfen
# =================================================================

pequiv_red %>% count(cid, hid, pid, syear) %>% filter(n > 1)
pgen_red %>% count(cid, hid, pid, syear) %>% filter(n > 1)
ppathl_red %>% count(cid, hid, pid, syear) %>% filter(n > 1)
pl_red %>% count(cid, hid, pid, syear) %>% filter(n > 1)
hbrutto_red %>% count(cid, hid, syear) %>% filter (n > 1)


# =========================================================
# 4. ID-Variablen harmonisieren 
# =========================================================

id_vars <- c("cid", "hid", "pid", "syear")

norm_ids <- function(df) {df %>% 
    mutate(across(any_of(id_vars), ~ .x %>% zap_labels() %>% as.integer()))}

pequiv_red <- norm_ids(pequiv_red)
pgen_red <- norm_ids(pgen_red)
ppathl_red <- norm_ids(ppathl_red)
pl_red <- norm_ids(pl_red)
hbrutto_red <- norm_ids(hbrutto_red)


# =========================================================
# 5. Mergen der Einzeldatensätze in kombiniertes Panel
# =========================================================

combined <- pgen_red %>%
  left_join(pl_red, by = c("cid", "hid", "pid", "syear")) %>%
  left_join(pequiv_red, by = c("cid", "hid", "pid", "syear")) %>%
  left_join(ppathl_red, by = c("cid", "hid", "pid", "syear")) %>%
  left_join(hbrutto_red, by = c("cid", "hid", "syear"))


# =========================================================
# 6. Branchenharmonisierung nach NACE-2008 (destatis)
# =========================================================

combined <- combined %>%
  mutate(nacestub = coalesce(
      if_else(pgnace >= 0, zap_labels(pgnace), NA_integer_),
      if_else(pgnace2 >= 0, zap_labels(pgnace2), NA_integer_)),
    nacestub = as.integer(nacestub), branche_section = case_when(
      nacestub %in%  1:3   ~ "A", nacestub %in%  5:9   ~ "B",
      nacestub %in% 10:33  ~ "C", nacestub ==   35     ~ "D",
      nacestub %in% 36:39  ~ "E", nacestub %in% 41:43  ~ "F",
      nacestub %in% 45:47  ~ "G", nacestub %in% 49:53  ~ "H",
      nacestub %in% 55:56  ~ "I", nacestub %in% 58:63  ~ "J",
      nacestub %in% 64:66  ~ "K", nacestub ==   68     ~ "L",
      nacestub %in% 69:75  ~ "M", nacestub %in% 77:82  ~ "N",
      nacestub ==   84     ~ "O", nacestub ==   85     ~ "P",
      nacestub %in% 86:88  ~ "Q", nacestub %in% 90:93  ~ "R",
      nacestub %in% 94:96  ~ "S", nacestub %in% 97:98  ~ "T",
      nacestub ==   99     ~ "U", TRUE ~ NA_character_)) %>%
  select(-pgnace, -pgnace2)


# =========================================================
# 7. Berufserfahrung kombinieren (Vollzeit + 0.5xTeilzeit)
# =========================================================

combined$exp <- combined$pgexpft + 0.5*combined$pgexppt
combined$pgexppt <- NULL
combined$pgexpft <- NULL


# =========================================================
# 8. Entfernen von negativen Werten
# =========================================================

combined <- combined %>%
  filter(pgtatzeit >= 0, bula_h >= 0, d11101 >= 0, d11102ll >= 0, pgfamstd >= 0,
    pglabgro > 0, d11107 >= 0, exp > 0, pgbilzeit >= 0)


# =========================================================
# 9. Entfernen von NA in relevanten Variablen
# =========================================================

combined <- drop_na(combined, pgtatzeit, pglabgro, bula_h, d11101, 
            branche_section, d11102ll, pgfamstd, d11107, exp, pgbilzeit, 
            pgemplst, cid, hid, pid, syear, nacestub)


# =========================================================
# 10. Filtern nach Analysestichprobe
# =========================================================

combined <- combined %>%
  filter(pgemplst < 3, # reguläre Erwerbstätigkeit
    d11101 >= 18, d11101 <= 67, # arbeitsfähiges Alter
    pgtatzeit >= 20, pgtatzeit <= 60, # Zwischen Teilzeit und 60 Stunden
    (d11101 - exp) >= 12,           # keine unplausible Berufslaufbahn
    pglabgro > 100, pglabgro < 25000)


# =========================================================
# 11. Negativwerte in Outcomes durch NA ersetzen
# =========================================================

vars <- c("ple0008", "plh0182", "plh0178")
combined[vars] <- lapply(combined[vars], function(x) ifelse(x < 0, NA, x))


# =========================================================
# 12. Gesundheitszustand auf 0–10 umskalieren
# =========================================================

combined$ple0008<- (5 - combined$ple0008) * (10/4)


# ===============================================================
# 13. Variablen Geschlecht & Beschäftigung als Faktor definieren
# ===============================================================

combined <- combined %>%
  mutate(gender = case_when(d11102ll == 1 ~ "Male", d11102ll == 2 ~ "Female",
      TRUE ~ NA_character_),
    gender = factor(gender, levels = c("Male", "Female")))
combined$d11102ll <- NULL
combined <- combined %>%
  mutate(emplst_num = as.numeric(pgemplst),
    emplst = factor(pgemplst, levels = c(1, 2), labels = c("1","2")))
combined$pgemplst <- NULL


# =========================================================
# 14. Variablentypen harmonisieren
# =========================================================

combined <- combined %>%
  mutate(across(c(pgtatzeit, ple0008, plh0178, plh0182, d11101, pgfamstd, 
                  d11107, exp, pglabgro, pgbilzeit), as.numeric))


# =========================================================
# 15. Variablen umbenennen
# =========================================================

combined <- combined %>% rename(year = syear, tatzeit = pgtatzeit, 
            income = pglabgro, bilzeit = pgbilzeit, famstd = pgfamstd, 
            health = ple0008, lifesat = plh0182, leissat = plh0178, 
            age = d11101, children = d11107, bula = bula_h, 
            branche_count = nacestub)

# =========================================================
# 16. Reihenfolge der Variablen festlegen
# =========================================================

combined <- combined %>% select(cid, hid, pid, year, bula, branche_count, 
            branche_section, tatzeit, health, leissat, lifesat, gender, age, 
            famstd, children, exp, income, bilzeit, emplst, emplst_num, phrf, phrf0, phrf1)


# =========================================================
# 17. Datensatz speichern und optional laden
# =========================================================

saveRDS(combined, here::here("data", "derived", "preinstrument.rds"), compress = "xz")
combined <- readRDS(here::here("data", "derived", "preinstrument.rds"))


# =========================================================
# 18. Kopie des Analysepanels zur Instrument-Konstruktion
# =========================================================

df_iv <- combined


# =========================================================
# 19. Setzen der relevanten Zeitparameter
# =========================================================

base_year <- 1991:1993   # Basiszeitraum für Branchenstruktur
first_year <- 1994        # Start des Schockzeitraums
last_year  <- 2022        # Ende der Analyseperiode


# =========================================================
# 20. "Share"-Komponente: Branchenanteile pro Bundesland
# =========================================================

shares <- df_iv %>%
  filter(year %in% base_year) %>%
  group_by(bula, branche_section) %>%
  summarise(emp = n(), .groups = "drop") %>%
  group_by(bula) %>%
  mutate(share = emp / sum(emp)) %>%
  ungroup()


# =========================================================
# 21. "Shift"-Komponente: Leave-One-Out-Branchenschocks
# =========================================================

# Arbeitsstunden je (branche, year, bula)
bl_level <- df_iv %>%
  filter(year >= first_year - 1, year <= last_year) %>%
  group_by(branche_section, year, bula) %>%
  summarise(sum_hours = sum(tatzeit, na.rm = TRUE), n_obs = n(), 
            .groups = "drop")

# Nationale Summe je (branche, year)
nat_total <- bl_level %>%
  group_by(branche_section, year) %>%
  summarise(sum_nat = sum(sum_hours), n_nat = sum(n_obs), .groups = "drop")

# Leave-One-Out Durchschnitt
bl_loo <- bl_level %>%
  left_join(nat_total, by = c("branche_section", "year")) %>%
  mutate(mean_nat_loo = (sum_nat - sum_hours) / (n_nat - n_obs)) %>%
  select(branche_section, year, bula, mean_nat_loo)


# =========================================================
# 22. Berechnung der Schocks (Δx_it)
# =========================================================

shift <- bl_loo %>%
  group_by(branche_section, bula) %>%
  arrange(year) %>%
  mutate(shift = mean_nat_loo - lag(mean_nat_loo)) %>%
  ungroup() %>%
  filter(year >= first_year, !is.na(shift)) %>%
  select(bula, branche_section, year, shift)


# =========================================================
# 23. Berechnung des Instruments
# =========================================================

bartik <- shares %>%
  inner_join(shift, by = c("bula", "branche_section")) %>%
  mutate(component = share * shift) %>%
  group_by(bula, year) %>%
  summarise(bartik_iv = sum(component, na.rm = TRUE), .groups = "drop")


# =========================================================
# 24. Hinzufügen des Instruments zum Panel
# =========================================================

df_iv <- df_iv %>%
  filter(year >= first_year, year <= last_year) %>%
  left_join(bartik, by = c("bula", "year"))


# =========================================================
# 25. Variablenreihenfolge mit Instrument neu sortieren
# =========================================================

df_iv <- df_iv %>%
  select(cid, hid, pid, year, bula, branche_count, branche_section, bartik_iv, 
    tatzeit, health, leissat, lifesat, gender, age, famstd, children, exp, 
    income, bilzeit, emplst, emplst_num, phrf, phrf0, phrf1)


# =========================================================
# 26. Beobachtungen ohne Instrument entfernen
# =========================================================

df_iv <- drop_na(df_iv, bartik_iv)


# =========================================================
# 27. Speichern und Laden des finalen Instrumentsamples
# =========================================================

saveRDS(df_iv, here::here("data", "derived", "postinstrument.rds"), compress = "xz")
df <- readRDS(here::here("data", "derived", "postinstrument.rds"))


# =========================================================
# 28. Überblick: Anzahl Beobachtungen und Individuen
# =========================================================

n_obs <- nrow(df)
n_ind <- n_distinct(df$pid)


# =========================================================
# 29. Erstellung des Survey-Designs mit Panelgewichten
# =========================================================

des <- survey::svydesign(ids = ~pid, weights = ~phrf1, data = df, nest = TRUE)


# =========================================================
# 30. Deskriptivstatistik für metrische Variablen
# =========================================================

vars_metrisch <- c("bartik_iv", "tatzeit", "health", "leissat", "lifesat", 
                   "age", "children", "exp", "income", "bilzeit")

results <- purrr::map_dfr(vars_metrisch, function(var) {tmp <- df %>%
    select(pid, phrf1, x = all_of(var)) %>% 
  filter(!is.na(x))
  tmp_des <- svydesign(ids = ~pid, weights = ~phrf1, data = tmp)
  data.frame(Variable = var,
    Mean     = round(as.numeric(svymean(~x, tmp_des)), 2),
    SD       = round(sd(tmp$x), 4),
    Min      = round(min(tmp$x), 2),
    Max      = round(max(tmp$x), 2),
    N        = nrow(tmp),
    Category = NA)})


# =========================================================
# 31. Deskriptivstatistik für binäre Variablen
# =========================================================

vars_binär <- c("gender", "emplst")

results_binär <- purrr::map_dfr(vars_binär, function(var) {
  tmp_rows <- !is.na(df[[var]])
  tmp <- data.frame(
    pid   = df$pid[tmp_rows],
    phrf1 = df$phrf1[tmp_rows],
    x     = df[[var]][tmp_rows])
  tmp_des <- svydesign(ids = ~pid, weights = ~phrf1, data = tmp)
  m <- svymean(~x, tmp_des)
  purrr::map_dfr(seq_along(m), function(i) {data.frame(
      Variable = var,
      Category = sub("^x", "", names(m)[i]),
      Mean     = round(as.numeric(m[i]), 3),
      SD       = NA, Min = NA, Max = NA,
      N        = nrow(tmp))})})


# =========================================================
# 32. Kombination der Ergebnis-Tabellen und Export
# =========================================================

results_all <- bind_rows(results[, c("Variable", "Category", "Mean", "SD", 
                                     "Min", "Max", "N")], results_binär)

# Unterordner erstellen
dir.create(here::here("output", "tables", "table1"), recursive = TRUE)

# Export als LaTeX-Tabelle in table1/
write_tex_rows(results_all, file_base = file.path("table1", "deskriptiv"))


# =========================================================
# 33. Verteilungen von Bundesländern und Branchen
# =========================================================

# Bundesland- und Branchenlabels definieren
bundesland_labels <- c("1" = "Schleswig-Holstein", "2" = "Hamburg", 
  "3" = "Niedersachsen", "4" = "Bremen", "5" = "Nordrhein-Westfalen", 
  "6" = "Hessen", "7" = "Rheinland-Pfalz / Saarland",
  "8" = "Baden-Württemberg", "9" = "Bayern", "10" = "Saarland", "11" = "Berlin",
  "12" = "Brandenburg", "13" = "Mecklenburg-Vorpommern", "14" = "Sachsen",
  "15" = "Sachsen-Anhalt", "16" = "Thüringen")

branche_labels <- c(
  "A" = "Land- und Forstwirtschaft, Fischerei",
  "B" = "Bergbau und Gewinnung von Steinen und Erden",
  "C" = "Verarbeitendes Gewerbe",
  "D" = "Energieversorgung",
  "E" = "Wasserversorgung; Abwasser- und Abfallentsorgung und Beseitigung von Umweltverschmutzungen",
  "F" = "Baugewerbe",
  "G" = "Handel; Instandhaltung und Reparatur von Kraftfahrzeugen",
  "H" = "Verkehr und Lagerei",
  "I" = "Gastgewerbe",
  "J" = "Information und Kommunikation",
  "K" = "Erbringung von Finanz- und Versicherungsdienstleistungen",
  "L" = "Grundstücks- und Wohnungswesen",
  "M" = "Erbringung von freiberuflichen, wissenschaftlichen und technischen Dienstleistungen",
  "N" = "Erbringung von sonstigen wirtschaftlichen Dienstleistungen",
  "O" = "Öffentliche Verwaltung, Verteidigung; Sozialversicherung",
  "P" = "Erziehung und Unterricht",
  "Q" = "Gesundheits- und Sozialwesen",
  "R" = "Kunst, Unterhaltung und Erholung",
  "S" = "Erbringung von sonstigen Dienstleistungen",
  "T" = "Private Haushalte mit Hauspersonal; Herstellung von Waren und Erbringung von Dienstleistungen durch private Haushalte für den Eigenbedarf ohne ausgeprägten Schwerpunkt",
  "U" = "Exterritoriale Organisationen und Körperschaften")

# Funktion für gewichtete Anteile
w_share_full <- function(data, var_cat, var_w) {
  v <- enquo(var_cat); w <- enquo(var_w)
  df <- data %>%
    filter(!is.na(!!v), !is.na(!!w))
  sum_w <- sum(pull(df, !!w), na.rm = TRUE)
  df %>%
    group_by(!!v) %>%
    summarise(Anteil_pct = 100 * sum(!!w, na.rm = TRUE) / sum_w, .groups = "drop") %>%
    arrange(desc(Anteil_pct)) %>%
    mutate(Anteil_pct = round(Anteil_pct, 1))}

# Tabellen berechnen
bula_tab_raw    <- w_share_full(df, bula, phrf1)
branche_tab_raw <- w_share_full(df, branche_section, phrf1)

# Bundesland-Labels anwenden
bula_tab <- w_share_full(df, bula, phrf1) %>%
  mutate(Bundesland = bundesland_labels[as.character(bula)]) %>%
  select(Bundesland, Anteil_pct)

# Branchen-Labels anwenden
branche_tab <- w_share_full(df, branche_section, phrf1) %>%
  mutate(Branche = branche_labels[as.character(branche_section)]) %>%
  select(Branche, Anteil_pct)

# Ordner erstellen
dir.create(here::here("output", "tables", "tableA2"), recursive = TRUE, showWarnings = FALSE)
dir.create(here::here("output", "tables", "tableA3"), recursive = TRUE, showWarnings = FALSE)

# Exportiere nur die Zeilen im LaTeX-Format
write_tex_rows(bula_tab,    file_base = file.path("tableA2", "bula_share"))
write_tex_rows(branche_tab, file_base = file.path("tableA3", "branche_share"))


# =========================================================
# 34. Vorbereitung für Plot der wichtigsten Verteilungen
# =========================================================

var_labels_axis <- c(
  "bartik_iv" = "Bartik-Instrument",
  "tatzeit"   = "Wochenarbeitszeit",
  "health"    = "Gesundheitszustand",
  "lifesat"   = "Lebenszufriedenheit",
  "leissat"   = "Freizeitzufriedenheit")

var_labels_title <- c(
  "bartik_iv" = "Bartik-Instruments",
  "tatzeit"   = "Wochenarbeitszeit",
  "health"    = "Gesundheitszustands",
  "lifesat"   = "Lebenszufriedenheit",
  "leissat"   = "Freizeitzufriedenheit")

der_vars <- c("tatzeit", "lifesat", "leissat")


# =========================================================
# 35. Plotfunktion für Histogramme
# =========================================================

plot_hist <- function(data, var_code, axis_label, title_label, bins = 30) {
  artikel <- ifelse(var_code %in% der_vars, "der", "des")
  ggplot(data, aes_string(x = var_code)) +
    geom_histogram(bins = bins, fill = "#4e4e4e", color = "white") +
    theme_minimal(base_size = 14) +
    labs(x = axis_label, y = "Häufigkeit",
      title = paste("Verteilung", artikel, title_label)) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.text  = element_text(color = "black"))}


# =========================================================
# 36. Plots speichern
# =========================================================

# Unterordner erstellen
dir.create(here::here("output", "figures", "figureA1"), recursive = TRUE, showWarnings = FALSE)

# Plots speichern
for (v in names(var_labels_axis)) {
  p <- plot_hist(df, v, var_labels_axis[v], var_labels_title[v])
  ggsave(filename = here::here("output", "figures", "figureA1",
        paste0("histogram_", v, ".png")), plot = p, width = 6, height = 4, dpi = 300)}


# =========================================================
# 37. First-Stage-Regression und Scatterplot
# =========================================================

# Regression: First Stage
first_stage <- feols(tatzeit ~ bartik_iv + age + I(age^2) + gender + exp + 
               famstd + income + bilzeit + children | pid + year, data = df, 
               cluster = ~bula:year)

# Ordner anlegen
outdir <- here::here("output", "tables", "table2")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

# Modellausgabe speichern
write_tex_rows(modelsummary::msummary(first_stage, output = "data.frame"),
                      file_base = file.path("table2", "firststage_main"))

# First Stage F-Statistik berechnen und speichern
fs_test <- fixest::wald(first_stage, "bartik_iv")
fs_val <- fs_test$stat
fs_tab <- data.frame(Statistic = "First-stage F", Value = round(fs_val, 2))
write_tex_rows(fs_tab, file_base = file.path("table2", "firststage_F"))

# Durchschnittsplot (Bartik ~ Arbeitszeit auf BL-Jahresebene)
df_avg <- df %>%
  group_by(bula, year) %>%
  summarise(bartik_iv = mean(bartik_iv, na.rm = TRUE), 
            tatzeit = mean(tatzeit, na.rm = TRUE), .groups = "drop")

# Plot erstellen
pl <- ggplot(df_avg, aes(x = bartik_iv, y = tatzeit)) +
  geom_point(size = 2, color = "darkgrey") +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(x = "Bartik-Instrument (Bundesland-Jahr-Ebene)",
    y = "Durchschnittliche Wochenarbeitszeit (Bundesland-Jahr-Ebene)") +
  theme_minimal(base_size = 14)

# Grafik speichern
dir.create(here::here("output", "figures", "figure1"), recursive = TRUE, showWarnings = FALSE)
ggsave(filename = here::here("output", "figures", "figure1", "firststage_scatter.png"),
  plot = pl, width = 7, height = 6, dpi = 300)


# =========================================================
# 38. Second-Stage- und OLS-Regressionen
# =========================================================

# Definition der Outcomes und Kontrollvariablen
outcomes <- c(health   = "health", lifesat  = "lifesat", leissat  = "leissat")
controls <- "age + I(age^2) + gender + exp + famstd + income + bilzeit + children"

# Formelgeneratoren
build_formula <- function(yvar) {as.formula(paste0(yvar, " ~ ", controls,
                                 " | pid + year | tatzeit ~ bartik_iv"))}

build_ols_formula <- function(yvar) {as.formula(paste0(yvar, " ~ tatzeit + ", 
                                     controls, " | pid + year"))}

# Modelle schätzen
iv_models  <- purrr::map(outcomes, ~ feols(build_formula(.x),  data = df, cluster = ~bula:year))
ols_models <- purrr::map(outcomes, ~ feols(build_ols_formula(.x), data = df, cluster = ~bula:year))

# Export: Haupttabellen 
iv_tab <- as.data.frame(etable(iv_models, tex = FALSE, fitstat = ~ n + ivf))
ols_summary_df <- modelsummary::msummary(ols_models, output = "data.frame")
dir.create(here::here("output", "tables", "table3"), recursive = TRUE)
write_tex_rows(iv_tab, file_base = "table3/secondstage_iv")
write_tex_rows(ols_summary_df, file_base = "table3/secondstage_ols")



# =========================================================
# 39. Heterogenitätsanalysen (Subgruppen)
# =========================================================

# Subgruppen definieren
groups <- list(
  female    = df$gender == "Female",
  male      = df$gender == "Male",
  fulltime  = df$emplst == "1",
  parttime  = df$emplst == "2",
  withchild = df$children > 0,
  nochild   = df$children == 0,
  old       = df$age >= 50,
  young     = df$age < 40,
  highinc   = df$income > median(df$income))

outcomes <- c(health = "health", lifesat = "lifesat", leissat = "leissat")
controls <- "age + I(age^2) + gender + exp + famstd + income + bilzeit + children"
build_formula <- function(yvar){
  as.formula(paste0(yvar, " ~ ", controls, " | pid + year | tatzeit ~ bartik_iv"))}

# Ordner erstellen
sub_outdir <- here::here("output", "tables", "table4and5")
if (!dir.exists(sub_outdir)) dir.create(sub_outdir, recursive = TRUE)

# Schätzung pro Subgruppe & Outcome
for (grp in names(groups)) {
  idx <- groups[[grp]]
  if (!any(idx, na.rm = TRUE)) next
    iv_models_g <- lapply(outcomes, function(y)
    feols(build_formula(y), data = df[idx, , drop = FALSE], cluster = ~bula:year))
    etab <- fixest::etable(iv_models_g, tex = FALSE, fitstat = ~ n + ivf,
                         signif.code = NA, digits = 3)
  etab_df <- as.data.frame(etab, stringsAsFactors = FALSE, check.names = FALSE)
    if (is.null(colnames(etab_df)) || colnames(etab_df)[1] == "") {
    colnames(etab_df)[1] <- "term"
  } else if (!"term" %in% colnames(etab_df)) {
    etab_df$term <- rownames(etab_df)
    etab_df <- etab_df[, c(ncol(etab_df), 1:(ncol(etab_df)-1))]}
  
  cn <- colnames(etab_df)
  empty <- is.na(cn) | cn == ""
  if (any(empty)) {cn[empty] <- paste0("Model_", seq_len(sum(empty)))
    colnames(etab_df) <- cn}
  
  # Export
  write_tex_rows(etab_df, file_base = file.path("table4and5", 
                                                paste0("iv_subgroup_", grp)))}


# =========================================================
# 40. Exogenität: Balance-Tests der Kovariaten
# =========================================================

covariates <- c("bilzeit", "income", "children", "famstd", "exp", "emplst_num")
cov_models <- lapply(covariates, function(v) {
  fml <- as.formula(paste0(v, " ~ bartik_iv | pid + year"))
  feols(fml, data = df, cluster = ~bula:year)})

# Export als LaTeX-Zeilen
cov_results <- modelsummary::msummary(cov_models, output = "data.frame")
dir.create(here::here("output", "tables", "table6"), recursive = TRUE)
write_tex_rows(cov_results, file_base = file.path("table6", "balance_tests"))

# =========================================================
# 41. Exogenität: Placebo-Tests mit irrelevanten Outcomes
# =========================================================

# Datensatz laden
placebo <- readRDS(here::here("data", "raw", "pl.rds")) %>%
  select(cid, hid, pid, syear, ple0007, ple0006, plh0007) %>%
  rename(year = syear)

# Mergen mit Hauptpanel
df_placebo <- df %>%
  left_join(placebo, by = c("cid", "hid", "pid", "year")) %>%
  filter(ple0006 > 0, ple0007 > 0, plh0007 > 0)

# Definition der Placebo-Outcomes
placebos <- c(weight = "ple0007", height = "ple0006", politics = "plh0007")
controls <- "bilzeit + income + age + I(age^2) + gender + children + famstd + exp"

# Formel-Generator
build_formula <- function(yvar) {
  as.formula(paste0(yvar, " ~ ", controls, " | pid + year | tatzeit ~ bartik_iv"))}

# Regressionen
placebo_models <- map(placebos, ~ feols(build_formula(.x), 
                                  data = df_placebo, cluster = ~bula:year))

# Export als LaTeX-Zeilen
placebo_tab <- as.data.frame(etable(placebo_models, tex = FALSE, fitstat = ~ n + ivf))
dir.create(here::here("output", "tables", "table7"), recursive = TRUE)
write_tex_rows(placebo_tab, file_base = file.path("table7", "placebo_tests"))

# =========================================================
# 42. Exogenität: Lead-Outcome-Tests
# =========================================================

df_lead <- df %>%
  group_by(pid) %>%
  arrange(year) %>%
  mutate(health_lead  = lead(health), lifesat_lead = lead(lifesat),
    leissat_lead = lead(leissat)) %>%
  ungroup()

lead_outcomes <- c(health_lead  = "health_lead", lifesat_lead = "lifesat_lead",
                   leissat_lead = "leissat_lead")

lead_formula <- function(y) {as.formula(paste0(y, " ~ ", controls, 
                                      " | pid + year | tatzeit ~ bartik_iv"))}

lead_models <- map(lead_outcomes, ~ feols(lead_formula(.x), 
                                          data = df_lead, cluster = ~bula:year))

# Export als LaTeX-Zeilen
lead_tab <- as.data.frame(etable(lead_models, tex = FALSE, fitstat = ~ n + ivf))
dir.create(here::here("output", "tables", "table8"), recursive = TRUE)
write_tex_rows(lead_tab, file_base = file.path("table8", "lead_tests"))


# =========================================================
# 43. Robustheit: Subsample der "Stayer"
# =========================================================

# Ursprünglichen Datensatz laden
combined <- readRDS(here::here("data", "derived", "preinstrument.rds"))

# Nur Personen mit max. 1 Branchenwechsel ("Stayer")
df_stayers <- combined %>% 
  group_by(pid) %>%
  arrange(year) %>%
  mutate(wechsel = branche_section != lag(branche_section)) %>%
  mutate(wechsel_n = sum(wechsel, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(wechsel_n <= 1)

# Bartik-Instrument im Stayer-Sample neu berechnen
shares <- df_stayers %>%
  filter(year %in% 1991:1993) %>%
  group_by(bula, branche_section) %>%
  summarise(emp = n(), .groups = "drop") %>%
  group_by(bula) %>%
  mutate(share = emp / sum(emp)) %>%
  ungroup()

bl_level <- df_stayers %>%
  filter(year >= 1993, year <= 2022) %>%
  group_by(branche_section, year, bula) %>%
  summarise(sum_hours = sum(tatzeit), n_obs = n(), .groups = "drop")

nat_total <- bl_level %>%
  group_by(branche_section, year) %>%
  summarise(sum_nat = sum(sum_hours), n_nat = sum(n_obs), .groups = "drop")

bl_loo <- bl_level %>%
  left_join(nat_total, by = c("branche_section", "year")) %>%
  mutate(mean_nat_loo = (sum_nat - sum_hours) / (n_nat - n_obs)) %>%
  select(branche_section, year, bula, mean_nat_loo)

shift <- bl_loo %>%
  group_by(branche_section, bula) %>%
  arrange(year) %>%
  mutate(shift = mean_nat_loo - lag(mean_nat_loo)) %>%
  ungroup() %>%
  filter(year >= 1994, !is.na(shift))

bartik <- shares %>%
  inner_join(shift, by = c("bula", "branche_section")) %>%
  mutate(component = share * shift) %>%
  group_by(bula, year) %>%
  summarise(bartik_iv = sum(component), .groups = "drop")

df_stayers <- df_stayers %>%
  filter(year >= 1994, year <= 2022) %>%
  left_join(bartik, by = c("bula", "year")) %>%
  drop_na(bartik_iv)

# First Stage (nur Stayer)
first_stage_stayer <- feols(tatzeit ~ bartik_iv + age + I(age^2) + gender + exp 
                            + famstd + income + bilzeit + children | pid + year,
                            data = df_stayers, cluster = ~bula:year)

# Ordner anlegen
outdir <- here::here("output", "tables", "table9")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

# Modellausgabe speichern
write_tex_rows(modelsummary::msummary(first_stage_stayer, output = "data.frame"),
               file_base = file.path("table9", "firststage_stayer"))

# First Stage F-Statistik berechnen und speichern
fs_test <- fixest::wald(first_stage_stayer, "bartik_iv")
fs_val <- fs_test$stat
fs_tab <- data.frame(Statistic = "First-stage F", Value = round(fs_val, 2))
write_tex_rows(fs_tab, file_base = file.path("table9", "firststage_stayer_F"))

# Second Stage (nur Stayer)
controls <- "age + I(age^2) + gender + exp + famstd + income + bilzeit + children"
outcomes <- c("health", "lifesat", "leissat")

build_formula <- function(yvar) {as.formula(paste0(yvar, " ~ ", controls, 
                                 " | pid + year | tatzeit ~ bartik_iv"))}

iv_stayer_models <- map(outcomes, ~ feols(build_formula(.x),
                                    data = df_stayers, cluster = ~bula:year))

# Export als LaTeX-Zeilen
stayersecond_tab <- as.data.frame(etable(iv_stayer_models, tex = FALSE, fitstat = ~ n + ivf))
dir.create(here::here("output", "tables", "table10"), recursive = TRUE)
write_tex_rows(stayersecond_tab, file_base = file.path("table10", "secondstage_stayer"))


# =========================================================
# 44. Robustheit: Wahl des Basisjahres
# =========================================================

combined <- readRDS(here::here("data", "derived", "preinstrument.rds"))

# Outcome und Kontrollen
outcome <- "health"
controls <- "age + I(age^2) + gender + exp + famstd + income + bilzeit + children"
formula_fn <- function(y) {
  as.formula(paste0(y, " ~ ", controls, " | pid + year | tatzeit ~ bartik_iv"))}

# Leere Liste für Ergebnisse
results_byear <- list()

# Schleife über verschiedene Basisjahre
for (basis_year in 1991:1993) {
  # SHARE
  shares <- combined %>%
    filter(year == basis_year) %>%
    group_by(bula, branche_section) %>%
    summarise(emp = n(), .groups = "drop") %>%
    group_by(bula) %>%
    mutate(share = emp / sum(emp)) %>%
    ungroup()
  
  # SHIFT
  first_year <- basis_year + 1
  last_year <- 2022
  
  bl_level <- combined %>%
    filter(year >= first_year - 1, year <= last_year) %>%
    group_by(branche_section, year, bula) %>%
    summarise(sum_hours = sum(tatzeit), n_obs = n(), .groups = "drop")
  
  nat_total <- bl_level %>%
    group_by(branche_section, year) %>%
    summarise(sum_nat = sum(sum_hours), n_nat = sum(n_obs), .groups = "drop")
  
  bl_loo <- bl_level %>%
    left_join(nat_total, by = c("branche_section", "year")) %>%
    mutate(mean_nat_loo = (sum_nat - sum_hours) / (n_nat - n_obs)) %>%
    select(branche_section, year, bula, mean_nat_loo)
  
  shift <- bl_loo %>%
    group_by(branche_section, bula) %>%
    arrange(year) %>%
    mutate(shift = mean_nat_loo - lag(mean_nat_loo)) %>%
    ungroup() %>%
    filter(year >= first_year, !is.na(shift))
  
  # Bartik
  bartik <- shares %>%
    inner_join(shift, by = c("bula", "branche_section")) %>%
    mutate(component = share * shift) %>%
    group_by(bula, year) %>%
    summarise(bartik_iv = sum(component), .groups = "drop")
  
  # Merge mit Panel
  df_tmp <- combined %>%
    filter(year >= first_year, year <= last_year) %>%
    left_join(bartik, by = c("bula", "year")) %>%
    drop_na(bartik_iv, !!sym(outcome), tatzeit)
  
  # IV-Schätzung speichern
  model <- feols(formula_fn(outcome), data = df_tmp, cluster = ~bula:year)
  results_byear[[paste0("bartik_", basis_year)]] <- model}

# Export der Resultate
by_baseyear_tab <- as.data.frame(
  etable(results_byear, tex = FALSE, fitstat = ~ n + ivf))
dir.create(here::here("output", "tables", "table11"), recursive = TRUE)
write_tex_rows(by_baseyear_tab, file_base = file.path("table11", "robust_base_year"))


# =========================================================
# 45. Logfile beenden
# =========================================================

sessionInfo()
sink()