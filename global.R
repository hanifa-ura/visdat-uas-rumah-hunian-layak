# global.R: konfigurasi, paket, dan pemuatan data (dijalankan sekali)
suppressPackageStartupMessages({
  library(shiny)
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(plotly)       # >= 4.10.3 (treemap & icicle)
  library(leaflet)
  library(sf)
  library(spdep)
  library(ggdendro)
  library(DT)
  library(viridisLite)
  library(htmlwidgets)
  library(scales)
  # jsonlite sengaja tidak di-library(): jsonlite::validate() menimpa shiny::validate().
})
validate <- shiny::validate   # pengaman ganda bila ada paket lain yang menimpa
need     <- shiny::need
sf::sf_use_s2(FALSE)

# lokasi data
DATA_DIR  <- "data"
F_PROV    <- file.path(DATA_DIR, "dataset(provinsi).xlsx")
F_KAB     <- file.path(DATA_DIR, "backlog_kabkota.xlsx")
F_CAP     <- file.path(DATA_DIR, "capaian_pemerintah.xlsx")
F_CAP_RULES <- file.path(DATA_DIR, "program_dapat_dibandingkan.xlsx")  # program yang bisa dibandingkan antarsemester
F_GEO     <- file.path(DATA_DIR, "Indonesia_KAB_KOTA.geojson")  # batas kab/kota (550 MB)
F_GEO_RDS <- file.path(DATA_DIR, "kabkota_simplified.rds")      # cache hasil penyederhanaan (dibuat otomatis)

# API batas wilayah (Laravel Nusa) - hanya bila GeoJSON tidak ada
NUSA_BASE       <- "https://nusa.creasi.dev/nusa"
NUSA_AUTO_FETCH <- FALSE   # tidak dipakai lagi (GeoJSON lokal tersedia)

# Identitas di footer. Ganti dengan data penulis sebelum dipublikasikan.
AUTHOR <- list(
  judul   = "Hunian Layak Indonesia",
  nama    = "Aura Hanifa Kasetya Putri",
  nim     = "222313003",
  kelas   = "3SD2",
  email   = "222313003@stis.ac.id",
  mk      = "UAS Visualisasi Data",
  kampus  = "Politeknik Statistika STIS",
  tanggal = "Oktober 2026"
)

# angka resmi yang tidak ada di berkas xlsx
AKSES_LAYAK_2026 <- 70.30   # BPS, Statistik Perumahan 2026
# Komponen penyusun rumah layak huni nasional 2026 (BPS, Statistik Perumahan 2026)
KOMP_NAS_2026 <- c(bangunan = 87.52, lantai = 94.91, air = 93.71, sanitasi = 86.73)

# parameter analisis
CORR_DROP_THRESHOLD <- 0.85    # |rho| BABS vs sanitasi > 0,85 -> buang BABS
DROP_VARS           <- character(0)
KNN_K               <- 6       # tetangga terdekat untuk LISA (negara kepulauan)
CL_CLIP             <- 2       # skor-z dipangkas pada +/- 2 SD sebelum klaster/PCA
CL_LOG_VARS         <- c("babs", "nonpln", "asbes", "diare", "dbd")  # variabel menceng -> log(1 + x)

# muat modul
source("src/helpers.R",   local = FALSE)
source("src/nusa_geo.R",  local = FALSE)
source("src/data_prep.R", local = FALSE)
source("src/analysis.R",  local = FALSE)
source("src/plots.R",     local = FALSE)

# penanda versi (diset paling akhir, hanya bila semua modul berhasil dimuat)
# Hentikan dengan pesan jelas bila salah satu berkas src/ belum ikut diperbarui.
if (!exists("PLOTS_VERSION") || !identical(PLOTS_VERSION, "4.2") || !"view" %in% names(formals(draw_choro)))
  stop("src/plots.R masih versi lama. Timpa src/plots.R dengan versi 4.2, lalu jalankan ulang aplikasi.", call. = FALSE)
APP_VERSION <- "4.2"
