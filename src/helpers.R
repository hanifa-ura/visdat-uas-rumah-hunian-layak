# src/helpers.R: fungsi bantu umum (parsing angka, format, palet, tema)

# Parsing angka
# Dataset mencampur "97,88" (koma desimal), "1,192.25" (koma ribuan),
# "NAN", "–", "#VALUE!". Fungsi ini menyeragamkan semuanya.
to_num <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NAN", "NaN", "–", "-", "#VALUE!", "Tidak Tersedia")] <- NA
  th <- !is.na(x) & grepl("^\\d{1,3}(,\\d{3})+(\\.\\d+)?$", x)   # 1,192.25
  x[th] <- gsub(",", "", x[th])
  dc <- !is.na(x) & grepl("^-?\\d+,\\d+$", x)                      # 97,88
  x[dc] <- sub(",", ".", x[dc])
  round(suppressWarnings(as.numeric(x)), 6)
}

# Format teks
fmt_num <- function(x, d = 2) {
  x <- as.numeric(x)
  ifelse(is.na(x), "–", formatC(x, format = "f", digits = d, big.mark = ".", decimal.mark = ","))
}
fmt_pp <- function(x, d = 2) {
  ifelse(is.na(x), "–",
         paste0(ifelse(x > 0, "+", ifelse(x < 0, "\u2212", "")),
                formatC(abs(x), format = "f", digits = d, decimal.mark = ",")))
}
tc <- function(x) {
  y <- tools::toTitleCase(tolower(x))
  y <- gsub("\\bDki\\b", "DKI", y)
  y <- gsub("\\bDi\\b", "DI", y)
  y
}
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a

hex2rgba <- function(h, a = 1) {
  r <- grDevices::col2rgb(h)
  sprintf("rgba(%d,%d,%d,%.2f)", r[1, ], r[2, ], r[3, ], a)
}

# Spearman + p-value (aman terhadap NA)
sp_test <- function(x, y) {
  ok <- stats::complete.cases(x, y)
  if (sum(ok) < 4) return(c(rho = NA_real_, p = NA_real_, n = sum(ok)))
  r <- suppressWarnings(stats::cor.test(x[ok], y[ok], method = "spearman", exact = FALSE))
  c(rho = unname(r$estimate), p = r$p.value, n = sum(ok))
}
fmt_p <- function(p) {
  ifelse(is.na(p), "–", ifelse(p < 0.001, "< 0,001", formatC(p, format = "f", digits = 3, decimal.mark = ",")))
}

# Palet dari DESIGN.md. Varian *_D dipakai untuk teks agar lolos kontras AA di latar putih.
COL_OCHRE <- "#CA8A04"; COL_OCHRE_D <- "#854D0E"; COL_SLATE <- "#0F172A"; COL_NEUTRAL <- "#1E293B"
COL_TEAL  <- "#0D9488"; COL_TEAL_D  <- "#0F766E"; COL_TERRA <- "#C2410C"; COL_INDIGO <- "#4F46E5"
COL_INK3  <- "#334155"; COL_MUTED   <- "#475569"; COL_GRID  <- "#E3E8F5"; COL_PAPER <- "#F4F6FF"

ISLAND_LEVELS <- c("SUMATERA", "JAWA", "BALI", "NUSA TENGGARA", "KALIMANTAN", "SULAWESI", "MALUKU", "PAPUA")
PAL_ISLAND <- c(SUMATERA = "#0D9488", JAWA = "#CA8A04", BALI = "#DB2777", `NUSA TENGGARA` = "#65A30D",
                KALIMANTAN = "#4F46E5", SULAWESI = "#C2410C", MALUKU = "#0284C7", PAPUA = "#0F172A")
# klaster 1 (paling baik) -> klaster k (paling tertinggal)
PAL_CL   <- c("#0D9488", "#4F46E5", "#CA8A04", "#C2410C", "#0F172A")
COL_UP   <- COL_TERRA   # lebih buruk / naik
COL_DOWN <- COL_TEAL    # lebih baik / turun
COL_INK  <- COL_SLATE
FONT_SANS <- "Poppins, system-ui, -apple-system, Segoe UI, sans-serif"

disc_scale <- function(cols) {
  n <- length(cols)
  lapply(seq_len(n), function(i) list((i - 1) / max(n - 1, 1), cols[i]))
}

# scrollZoom mati agar halaman tetap bisa digulir saat kursor di atas grafik; zoom lewat modebar.
# modebar = FALSE untuk grafik tanpa sumbu kartesius; select = TRUE mempertahankan seleksi kotak (PCA).
theme_plotly <- function(p, modebar = TRUE, select = FALSE) {
  rm_btn <- c("sendDataToCloud", "toggleSpikelines", "hoverClosestCartesian", "hoverCompareCartesian")
  if (!select) rm_btn <- c(rm_btn, "select2d", "lasso2d")
  p %>%
    plotly::layout(
      font = list(family = FONT_SANS, size = 12, color = COL_INK),
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      hoverlabel = list(bgcolor = COL_SLATE, bordercolor = COL_OCHRE,
                        font = list(family = FONT_SANS, size = 12, color = COL_PAPER))
    ) %>%
    plotly::config(displayModeBar = modebar, displaylogo = FALSE, scrollZoom = FALSE,
                   doubleClick = "reset", modeBarButtonsToRemove = rm_btn,
                   toImageButtonOptions = list(format = "png", scale = 2, filename = "visdat-hunian"),
                   responsive = TRUE)
}

# Diameter gelembung dalam piksel. plotly memakai sizeref hanya untuk vektor; trace 1 titik (Bali)
# akan digambar 1.255 px tanpa fungsi ini.
bubble_diam <- function(v, vmax = max(v, na.rm = TRUE), dmax = 56, dmin = 8) {
  pmax(dmin, sqrt(pmax(v, 0) / vmax) * dmax)
}

# Sumber di bawah setiap grafik: Sumber: BPS. <judul>, <tahun>. <URL> (diakses <tanggal>).
# Ganti judul, URL, dan tanggal akses dengan tabel asli yang diunduh.
SRC_AKSES <- "4 Oktober 2026"
SRC <- list(
  nasional = list(who = "BPS", judul = "Statistik Perumahan 2026 (Susenas): Backlog Kepemilikan dan Backlog Kelayakhunian Nasional",
                  tahun = "2025\u20132026", url = "https://www.bps.go.id"),
  komp     = list(who = "BPS", judul = "Statistik Perumahan 2026: Rumah Tangga dengan Akses Hunian Layak dan Empat Komponennya",
                  tahun = "2025\u20132026", url = "https://www.bps.go.id"),
  prov     = list(who = "BPS", judul = "Statistik Perumahan 2026 dan Susenas: Backlog Perumahan menurut Provinsi",
                  tahun = "2025\u20132026", url = "https://www.bps.go.id"),
  kab      = list(who = "BPS", judul = "Susenas: Backlog Kepemilikan dan Kelayakhunian menurut Kabupaten/Kota",
                  tahun = "2025\u20132026", url = "https://www.bps.go.id"),
  kes      = list(who = "BPS dan Kemenkes", judul = "Statistik Kesehatan 2025 (keluhan kesehatan), Profil Kesehatan Indonesia 2025 (diare), tabel dinamis BPS (DBD, cakupan TBC, kemiskinan, penduduk), dan Statistik Perumahan 2026 (hunian)",
                  tahun = "2025", url = "https://www.bps.go.id"),
  cap      = list(who = "BPS dan Kementerian PKP", judul = "Capaian Program Perumahan Pemerintah (BSPS, FLPP, dan lainnya)",
                  tahun = "2025 dan Semester I 2026", url = "https://www.bps.go.id")
)
src_note <- function(key, extra = NULL) {
  s <- SRC[[key]]
  tags$p(class = "viz-src",
         paste0("Sumber: ", s$who, ". "), tags$i(s$judul), paste0(", ", s$tahun, ". "),
         tags$a(href = s$url, target = "_blank", rel = "noopener", s$url),
         paste0(" (diakses ", SRC_AKSES, ")."), if (!is.null(extra)) paste0(" ", extra))
}

# Legenda gradien HTML (dipakai treemap/icicle yang warnanya dihitung manual)
legend_bar <- function(cols, lo, hi, title) {
  grad <- paste0("linear-gradient(to right,", paste(cols, collapse = ","), ")")
  tags$div(class = "legend-bar",
           tags$span(class = "lb-title", title),
           tags$div(class = "lb-grad", style = paste0("background:", grad)),
           tags$div(class = "lb-ends", tags$span(lo), tags$span(hi)))
}

chips <- function(labels, cols) {
  tags$div(class = "chips",
           lapply(seq_along(labels), function(i)
             tags$span(class = "chip", tags$i(style = paste0("background:", cols[i])), labels[i])))
}

# daftar nama "A, B, dan C"
join_id <- function(x) {
  x <- as.character(x); n <- length(x)
  if (n <= 1) return(paste(x, collapse = ""))
  if (n == 2) return(paste(x, collapse = " dan "))
  paste0(paste(x[-n], collapse = ", "), ", dan ", x[n])
}
