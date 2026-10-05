# src/data_prep.R: baca dan siapkan data. Nilai di ketiga Excel sudah sama dengan publikasi BPS
# (lihat sheet "Keterangan" di tiap berkas); di sini hanya penyesuaian nama kolom dan sel kosong.

# 1. Provinsi
read_prov <- function() {
  raw <- readxl::read_excel(F_PROV, sheet = 1, col_types = "text")
  names(raw) <- trimws(names(raw))
  ren <- c(dbd_per100.000 = "dbd_100k", rumah_layak_huni_2025 = "rumah_layak_huni", rumah_layak_huni_2026 = "akses_layak_2026")
  for (from in names(ren)) names(raw)[names(raw) == from] <- ren[[from]]
  num <- setdiff(names(raw), c("kode_prov", "nama_prov", "pulau"))
  raw[num] <- lapply(raw[num], to_num)
  raw$kode_prov <- trimws(as.character(raw$kode_prov))
  raw
}

prep_prov <- function(raw) {
  # Sel kosong (NA di BPS: estimasi tidak disajikan) pada asbes dan BABS dibaca 0 dan dicatat.
  # Non-PLN kosong diisi sisa 100 - PLN - bukan listrik, karena ketiganya berjumlah 100.
  zero_fill <- c("asbes_pct_2025", "asbes_pct_2026", "babs_pct_2025", "babs_pct_2026",
                 "pen_bukanlistrik_2025", "pen_bukanlistrik_2026")
  na_cells <- lapply(setNames(nm = c("asbes_pct_2025", "babs_pct_2025", "pen_nonpln_2025")),
                     function(cn) raw$kode_prov[is.na(raw[[cn]])])
  raw <- raw %>% mutate(across(any_of(zero_fill), ~ replace(.x, is.na(.x), 0)))
  for (y in c("2025", "2026")) {
    np <- paste0("pen_nonpln_", y); pl <- paste0("pen_pln_", y); bl <- paste0("pen_bukanlistrik_", y)
    if (all(c(np, pl, bl) %in% names(raw)))
      raw[[np]] <- ifelse(is.na(raw[[np]]), pmax(0, round(100 - raw[[pl]] - raw[[bl]], 2)), raw[[np]])
  }
  raw <- raw %>% mutate(rt_2025 = b2_n_2025 / (b2_pct_2025 / 100),     # total RT (ribu) dari Backlog 2
                        rt_2026 = b2_n_2026 / (b2_pct_2026 / 100))
  for (cn in c("akses_layak_2026", paste0(c("bangunan", "lantai", "air", "sanitasi"), "_2026")))
    if (!cn %in% names(raw)) raw[[cn]] <- NA_real_
  raw <- raw %>%
    mutate(
      tidak_layak  = 100 - rumah_layak_huni,
      tdk_bangunan = 100 - bangunan_2025,
      tdk_lantai   = 100 - lantai_2025,
      tdk_air      = 100 - air_2025,
      tdk_sanitasi = 100 - sanitasi_2025,
      tidak_layak_2026  = 100 - akses_layak_2026,
      tdk_bangunan_2026 = 100 - bangunan_2026,
      tdk_lantai_2026   = 100 - lantai_2026,
      tdk_air_2026      = 100 - air_2026,
      tdk_sanitasi_2026 = 100 - sanitasi_2026
    )
  nas  <- raw %>% filter(kode_prov == "Indonesia")
  for (v in names(KOMP_NAS_2026)) {
    cn <- paste0(v, "_2026")
    if (!cn %in% names(nas) || is.na(nas[[cn]][1])) nas[[cn]] <- unname(KOMP_NAS_2026[v])
  }
  if (is.na(nas$akses_layak_2026[1])) nas$akses_layak_2026 <- AKSES_LAYAK_2026
  prov <- raw %>% filter(kode_prov != "Indonesia") %>% mutate(provinsi = tc(nama_prov))
  list(nas = nas, prov = prov,
       na_cells = lapply(na_cells, function(k) tc(raw$nama_prov[match(k, raw$kode_prov)])))
}

# 2. Kabupaten/kota
prep_kab <- function(prov) {
  raw <- readxl::read_excel(F_KAB, sheet = 1, col_types = "text")
  names(raw) <- trimws(names(raw))
  txt <- c("nama_prov", "kode_prov", "kode_kabkota", "nama_kabkota", "pulau")
  num <- setdiff(names(raw), txt)
  raw[num] <- lapply(raw[num], to_num)
  # Total RT dari Backlog 2 (atau Backlog 1 bila Backlog 2 kosong); sama dengan rumus kolom rt_* di Excel.
  rt_of <- function(y) {
    a <- raw[[paste0("b2_n_", y)]] / (raw[[paste0("b2_pct_", y)]] / 100)
    b <- raw[[paste0("b1_n_", y)]] / (raw[[paste0("b1_pct_", y)]] / 100)
    ifelse(is.finite(a), a, ifelse(is.finite(b), b, NA_real_))
  }
  raw$rt_2025 <- rt_of("2025"); raw$rt_2026 <- rt_of("2026")
  raw %>%
    mutate(kode_prov = trimws(as.character(kode_prov)),
           kode_kabkota = str_pad(trimws(as.character(kode_kabkota)), 4, pad = "0")) %>%
    select(-nama_prov) %>%
    left_join(prov %>% select(kode_prov, provinsi), by = "kode_prov") %>%
    mutate(
      jenis  = ifelse(as.integer(substr(kode_kabkota, 3, 4)) >= 71, "Kota", "Kab."),
      label  = paste(jenis, tc(nama_kabkota)),
      chg_b1 = b1_pct_2026 - b1_pct_2025,
      chg_b2 = b2_pct_2026 - b2_pct_2025,
      rt1_2026 = b1_n_2026 / (b1_pct_2026 / 100),
      qc_b1_2026 = is.finite(rt1_2026) & is.finite(rt_2026) & abs(rt1_2026 / rt_2026 - 1) > 0.25,
      geo_key = geo_key(kode_prov, jenis == "Kota", nama_kabkota)
    )
}

# 3. Capaian program pemerintah (angka tersimpan sebagai bilangan; sel kosong = data tidak tersedia)
read_cap <- function() {
  raw <- readxl::read_excel(F_CAP, sheet = 1, col_types = "text")[, 1:6]
  names(raw) <- c("program", "satuan", "s1_2025", "s2_2025", "tot_2025", "s1_2026")
  raw %>% filter(!is.na(program)) %>%
    mutate(across(c(s1_2025, s2_2025, tot_2025, s1_2026), to_num))
}

# Aturan pembanding per program (berkas program_dapat_dibandingkan.xlsx, disusun dari BPS Tabel 6.1).
# key = pola untuk mencocokkan nama program di capaian_pemerintah.xlsx.
CAP_KEYS <- c(FLPP = "FLPP", `Pelonggaran GWM BI (debitur)` = "GWM", `Retribusi PBG` = "PBG", CSR = "CSR",
              `Rumah susun` = "Rumah Susun", `BSPS / Bedah Rumah` = "BSPS", `Kredit Program Perumahan (debitur)` = "Kredit Program",
              `Rumah khusus, PSU, sanitasi` = "Rumah Khusus|PSU|Sanitasi", `APBD Provinsi` = "APBD Provinsi",
              `Pembiayaan mikro` = "Mikro", `APBD Kab/Kota` = "APBD Kabupaten", `PPN DTP` = "PPN DTP", `Program K/L` = "Kementerian/Lembaga")
CAP_SHORT <- c(FLPP = "FLPP (KPR subsidi)", GWM = "Pelonggaran GWM BI", PBG = "Pembebasan retribusi PBG",
               CSR = "Program CSR", `Rumah Susun` = "Rumah susun (pembangunan baru)")
read_cap_rules <- function(cap) {
  f <- F_CAP_RULES
  rules <- if (file.exists(f)) {
    r <- readxl::read_excel(f, skip = 4, col_names = FALSE, col_types = "text")
    r <- r[!is.na(r[[1]]), 1:4]
    names(r) <- c("program", "status", "pembanding", "catatan"); r
  } else data.frame(program = c("FLPP", "Pelonggaran GWM BI (debitur)", "Retribusi PBG", "CSR", "Rumah susun"),
                    status = "Ya", pembanding = "S1-2025 vs S1-2026", catatan = "", stringsAsFactors = FALSE)
  rules$key <- unname(CAP_KEYS[rules$program])
  rules$banding <- grepl("^Ya", rules$status)
  cmp <- rules[rules$banding & !is.na(rules$key), ]
  cmp <- do.call(rbind, lapply(seq_len(nrow(cmp)), function(i) {
    j <- grep(cmp$key[i], cap$program)[1]
    if (is.na(j)) return(NULL)
    data.frame(program = cmp$program[i], key = cmp$key[i], nama = unname(CAP_SHORT[cmp$key[i]]) %||% cmp$program[i],
               satuan = cap$satuan[j], s1_2025 = cap$s1_2025[j], s1_2026 = cap$s1_2026[j],
               catatan = cmp$catatan[i], stringsAsFactors = FALSE)
  }))
  cmp$chg <- (cmp$s1_2026 / cmp$s1_2025 - 1) * 100
  list(rules = rules, cmp = cmp)
}

# 4. Batas kab/kota
# GeoJSON memakai kode Kemendagri, data memakai kode BPS, jadi join lewat nama: provinsi | Kab/Kota | nama.
# Provinsi Papua 91-97 digabung karena GeoJSON masih memakai pembagian lama.
GEO_ALIAS <- c(PADANGSIDEMPUAN = "PADANGSIDIMPUAN", PANGKAJENEKEPULAUAN = "PANGKAJENEDANKEPULAUAN")
geo_key <- function(kode_prov, is_kota, nama) {
  n <- toupper(as.character(nama))
  n <- gsub("^(KABUPATEN|KAB\\.?|KOTA)\\s+", "", n)
  n <- gsub("^ADMINISTRASI\\s+", "", n)
  n <- gsub("[^A-Z]", "", n)
  n <- ifelse(n %in% names(GEO_ALIAS), GEO_ALIAS[n], n)
  grp <- ifelse(substr(as.character(kode_prov), 1, 1) == "9", "PAPUA", as.character(kode_prov))
  paste(grp, ifelse(is_kota, "KOTA", "KAB"), n, sep = "|")
}

read_geo_raw <- function() {
  message("Membaca GeoJSON (sekali saja; hasil disimpan ke ", F_GEO_RDS, ") ...")
  g <- sf::st_read(F_GEO, quiet = TRUE)
  g <- sf::st_zm(g, drop = TRUE, what = "ZM")
  if (is.na(sf::st_crs(g))) sf::st_crs(g) <- 4326
  g <- sf::st_transform(g, 4326)
  nm <- names(g)
  if (all(c("kode_prov", "kab_kota", "nama") %in% nm)) {
    g$geo_key <- geo_key(g$kode_prov, grepl("^Kota\\s", g$nama), g$kab_kota)
  } else {
    stop("Kolom kode_prov/kab_kota/nama tidak ditemukan di GeoJSON. Kolom tersedia: ", paste(nm, collapse = ", "))
  }
  g <- g[, "geo_key"]
  # Berkas 550 MB: pakai sf::st_simplify (rmapshaper/V8 bisa kehabisan memori untuk berkas sebesar ini).
  # dTolerance dalam derajat (s2 nonaktif): 0,005 derajat ~ 500 m, cukup untuk peta skala nasional.
  g <- suppressWarnings(sf::st_simplify(g, preserveTopology = TRUE, dTolerance = 0.005))
  g <- suppressWarnings(sf::st_make_valid(g))
  g <- g[!sf::st_is_empty(g), ]
  g <- g %>% dplyr::group_by(geo_key) %>% dplyr::summarise(.groups = "drop")
  saveRDS(g, F_GEO_RDS)
  g
}

load_geo <- function(kab) {
  g <- if (file.exists(F_GEO_RDS)) readRDS(F_GEO_RDS) else if (file.exists(F_GEO)) read_geo_raw() else NULL
  if (is.null(g)) return(NULL)
  out <- dplyr::left_join(g, kab, by = "geo_key")
  message(sprintf("Batas wilayah: %d poligon, %d cocok dengan data kab/kota (%d baris data).",
                  nrow(out), sum(!is.na(out$kode_kabkota)), nrow(kab)))
  out
}

# eksekusi
.p   <- prep_prov(read_prov())
NAS  <- .p$nas
PROV <- .p$prov
KAB  <- prep_kab(PROV)
CAP  <- read_cap()
CAPR <- read_cap_rules(CAP)
GEO  <- tryCatch(load_geo(KAB), error = function(e) {
  message("GAGAL memuat batas wilayah: ", conditionMessage(e)); NULL })

# Definisi variabel untuk klaster/PCA/heatmap (12 variabel 2025)
VARS <- data.frame(
  id    = c("bangunan", "lantai", "air", "sanitasi", "babs", "nonpln", "asbes", "keluhan", "diare", "tbc", "dbd", "miskin"),
  label = c("Ketahanan bangunan (%)", "Kecukupan luas lantai (%)", "Air minum layak (%)", "Sanitasi layak (%)", "BABS (%)",
            "Listrik non-PLN (%)", "Atap asbes (%)", "Keluhan kesehatan (%)", "Kasus diare dilayani (% penduduk)",
            "Cakupan penemuan kasus TBC", "DBD per 100.000", "Kemiskinan (%)"),
  short = c("Bangunan", "Lantai", "Air", "Sanitasi", "BABS", "Non-PLN", "Asbes", "Keluhan", "Diare", "Cakupan TBC", "DBD", "Miskin"),
  col   = c("bangunan_2025", "lantai_2025", "air_2025", "sanitasi_2025", "babs_pct_2025", "pen_nonpln_2025",
            "asbes_pct_2025", "keluhan_kesehatan_pct_2025", "diare_pct", "tbc_cakupan_pct", "dbd_100k", "miskin_2025"),
  good_high = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE),
  digits = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 2, 2),
  stringsAsFactors = FALSE
)

# Indikator untuk diagram peringkat provinsi. Indikator hunian memakai 2026 bila tersedia.
.y26 <- function(c26, c25) if (c26 %in% names(PROV) && any(is.finite(PROV[[c26]]))) c(c26, "2026") else c(c25, "2025")
.hc <- rbind(.y26("tidak_layak_2026", "tidak_layak"), .y26("tdk_bangunan_2026", "tdk_bangunan"),
             .y26("tdk_lantai_2026", "tdk_lantai"), .y26("tdk_air_2026", "tdk_air"),
             .y26("tdk_sanitasi_2026", "tdk_sanitasi"), .y26("pen_nonpln_2026", "pen_nonpln_2025"))
IND <- data.frame(
  id = c("b1", "b2", "tidak_layak", "tdk_bangunan", "tdk_lantai", "tdk_air", "tdk_sanitasi",
         "babs", "asbes", "nonpln", "keluhan", "diare", "tbc", "dbd", "miskin"),
  label = c("Backlog 1: kepemilikan (%)", "Backlog 2: kelayakhunian (%)", "Rumah tidak layak huni (%)",
            "Bangunan tidak memenuhi ketahanan (%)", "Luas lantai tidak mencukupi (%)", "Air minum tidak layak (%)",
            "Sanitasi tidak layak (%)", "BABS (%)", "Atap asbes (%)", "Listrik non-PLN (%)",
            "Keluhan kesehatan (%)", "Kasus diare dilayani (% penduduk)", "Cakupan penemuan kasus TBC", "DBD per 100.000 penduduk", "Kemiskinan (%)"),
  col = c("b1_pct_2026", "b2_pct_2026", .hc[1:5, 1], "babs_pct_2026", "asbes_pct_2026", .hc[6, 1],
          "keluhan_kesehatan_pct_2025", "diare_pct", "tbc_cakupan_pct", "dbd_100k", "miskin_2025"),
  yr = c("2026", "2026", .hc[1:5, 2], "2026", "2026", .hc[6, 2], "2025", "2025", "2025", "2025", "2025"),
  health = c(rep(FALSE, 10), TRUE, TRUE, TRUE, TRUE, FALSE),
  good_high = c(rep(FALSE, 12), TRUE, FALSE, FALSE),
  digits = c(rep(2, 12), 0, 2, 2),
  stringsAsFactors = FALSE
)
rm(.y26, .hc)

# Empat komponen penyusun rumah layak huni (BPS)
COMP <- c(bangunan = "Ketahanan bangunan", lantai = "Kecukupan luas lantai",
          air = "Akses air minum layak", sanitasi = "Akses sanitasi layak")

DQ <- list(
  kab_n          = nrow(KAB),
  kab_b1_flag26  = sum(KAB$qc_b1_2026, na.rm = TRUE),
  kab_b2_na26    = sum(is.na(KAB$b2_pct_2026)),
  kab_b1_na26    = sum(is.na(KAB$b1_pct_2026)),
  na_cells       = .p$na_cells,
  geo_match      = if (is.null(GEO)) 0L else sum(!is.na(GEO$kode_kabkota)),
  geo_n          = if (is.null(GEO)) 0L else nrow(GEO),
  has_2026_komp_prov = all(paste0(names(COMP), "_2026") %in% names(PROV)) &&
    all(is.finite(as.matrix(PROV[, paste0(names(COMP), "_2026")])))
)
rm(.p)
