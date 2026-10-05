# src/analysis.R: klaster, PCA, korelasi, LISA (dihitung sekali saat start)

prep_analysis <- function() {
  vars <- VARS[!VARS$id %in% DROP_VARS, ]
  X <- as.data.frame(PROV[, vars$col])
  names(X) <- vars$id
  rownames(X) <- PROV$provinsi
  
  # Sel kosong (DBD Papua Barat Daya dan Papua Pegunungan) diisi median agar bisa diklaster;
  # imp_mask menyimpan posisinya supaya grafik bisa menandainya.
  imp_mask <- is.na(as.matrix(X))
  imputed <- colSums(imp_mask); imputed <- imputed[imputed > 0]
  # Catatan per sel: median (kosong di sumber) atau dibaca 0 / diisi sisa saat persiapan data.
  imp_note <- ifelse(imp_mask, "Kosong di sumber; diisi median hanya untuk analisis", "")
  filled <- c(asbes = "asbes_pct_2025", babs = "babs_pct_2025", nonpln = "pen_nonpln_2025")
  for (v in intersect(names(filled), colnames(imp_note))) {
    pr <- intersect(DQ$na_cells[[filled[[v]]]], rownames(imp_note))
    imp_note[pr, v] <- if (v == "nonpln") "Kosong di BPS; diisi 100 \u2212 PLN \u2212 bukan listrik" else "Kosong di BPS (estimasi tidak disajikan); dibaca 0"
  }
  for (j in names(X)) X[[j]][is.na(X[[j]])] <- stats::median(X[[j]], na.rm = TRUE)
  
  # Aturan draft: BABS vs sanitasi layak, |Spearman| > 0,85 -> buang BABS
  rho <- suppressWarnings(stats::cor(X$babs, X$sanitasi, method = "spearman"))
  dropped <- character(0)
  if (!is.na(rho) && abs(rho) > CORR_DROP_THRESHOLD && "babs" %in% names(X)) {
    X$babs <- NULL; dropped <- "babs"
  }
  
  # Tanpa log dan pemangkasan, satu-dua provinsi ekstrem membentuk klaster sendiri.
  Y <- X
  for (j in intersect(CL_LOG_VARS, names(Y))) Y[[j]] <- log1p(Y[[j]])
  Z <- scale(Y)
  Z[Z >  CL_CLIP] <-  CL_CLIP
  Z[Z < -CL_CLIP] <- -CL_CLIP
  
  hc  <- stats::hclust(stats::dist(Z), method = "ward.D2")
  pca <- stats::prcomp(Z)
  # orientasi PC1: searah kemiskinan agar mudah dibaca (kanan = lebih tertinggal)
  if ("miskin" %in% colnames(X) && stats::cor(pca$x[, 1], X$miskin) < 0) {
    pca$x[, 1] <- -pca$x[, 1]; pca$rotation[, 1] <- -pca$rotation[, 1]
  }
  list(X = X, Z = Z, hc = hc, pca = pca, rho_babs_sanitasi = rho,
       dropped = dropped, imputed = imputed, imp_mask = imp_mask[, colnames(Z), drop = FALSE],
       imp_note = imp_note[, colnames(Z), drop = FALSE])
}
AN <- prep_analysis()

# klaster 1..k, diurutkan menurut rata-rata PC1 (1 = paling baik, k = paling tertinggal)
get_cl <- function(k) {
  ct  <- stats::cutree(AN$hc, k)
  ord <- order(tapply(AN$pca$x[, 1], ct, mean))
  map <- stats::setNames(seq_along(ord), ord)
  cl  <- as.integer(unname(map[as.character(ct)]))
  names(cl) <- names(ct)
  cl
}

# Nama klaster = tingkat (menurut PC1) + 3 ciri paling menonjol (rata-rata skor-z terbesar)
# Tingkat = posisi rata-rata klaster pada PC1 (sumbu ketertinggalan: kemiskinan & defisit hunian dasar)
CL_TIERS <- list(`3` = c("Ketertinggalan rendah", "Ketertinggalan menengah", "Ketertinggalan tinggi"),
                 `4` = c("Ketertinggalan rendah", "Ketertinggalan menengah", "Ketertinggalan tinggi", "Ketertinggalan sangat tinggi"),
                 `5` = c("Ketertinggalan sangat rendah", "Ketertinggalan rendah", "Ketertinggalan menengah",
                         "Ketertinggalan tinggi", "Ketertinggalan sangat tinggi"))
CIRI_LAB <- c(bangunan = "ketahanan bangunan", lantai = "kecukupan luas lantai", air = "air minum layak",
              sanitasi = "sanitasi layak", babs = "BABS", nonpln = "listrik non-PLN", asbes = "atap asbes",
              keluhan = "keluhan kesehatan", diare = "diare", tbc = "cakupan penemuan TBC", dbd = "DBD", miskin = "kemiskinan")
cluster_info <- function(k) {
  cl <- get_cl(k)
  tiers <- CL_TIERS[[as.character(k)]] %||% paste("Kelompok", seq_len(k))
  do.call(rbind, lapply(seq_len(k), function(j) {
    nm <- names(cl)[cl == j]
    m  <- colMeans(AN$Z[nm, , drop = FALSE])
    top <- names(sort(abs(m), decreasing = TRUE))[1:min(3, length(m))]
    ciri <- paste(sprintf("%s %s", CIRI_LAB[top],
                          ifelse(m[top] > 0, "tinggi", "rendah")), collapse = ", ")
    ex <- nm[order(-PROV$penduduk_2025[match(nm, PROV$provinsi)])]
    data.frame(cl = j, nama = tiers[j], ciri = ciri, n = length(nm),
               anggota = paste(ex, collapse = ", "), contoh = join_id(head(ex, 3)),
               stringsAsFactors = FALSE)
  }))
}

# LISA (Local Moran's I)
# Kab/kota di negara kepulauan banyak yang tidak bersinggungan, jadi tetangga
# ditentukan k-nearest neighbours (jarak lingkaran besar) lalu disimetriskan.
compute_lisa <- function(geo, var, alpha = 0.05) {
  d <- geo[!is.na(geo[[var]]), ]
  if (nrow(d) < 20) return(NULL)
  xy <- suppressWarnings(sf::st_coordinates(sf::st_point_on_surface(sf::st_geometry(d))))
  nb <- spdep::knn2nb(spdep::knearneigh(xy, k = KNN_K, longlat = TRUE))
  nb <- spdep::make.sym.nb(nb)
  lw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
  x  <- d[[var]]
  lm <- spdep::localmoran(x, lw, zero.policy = TRUE)
  gm <- spdep::moran.test(x, lw, zero.policy = TRUE)
  z   <- as.numeric(scale(x))
  lag <- spdep::lag.listw(lw, z, zero.policy = TRUE)
  p   <- lm[, ncol(lm)]
  kat <- dplyr::case_when(
    p >= alpha        ~ "Tidak signifikan",
    z > 0 & lag > 0   ~ "Tinggi\u2013Tinggi (hotspot)",
    z < 0 & lag < 0   ~ "Rendah\u2013Rendah (coldspot)",
    z > 0 & lag < 0   ~ "Tinggi di antara rendah",
    TRUE              ~ "Rendah di antara tinggi"
  )
  list(data = data.frame(kode_kabkota = d$kode_kabkota, kategori = kat, stringsAsFactors = FALSE),
       I = unname(gm$estimate[1]), p = gm$p.value, n = nrow(d))
}

LISA_LEVELS <- c("Tinggi\u2013Tinggi (hotspot)", "Rendah\u2013Rendah (coldspot)",
                 "Tinggi di antara rendah", "Rendah di antara tinggi", "Tidak signifikan")

LISA <- list()
if (!is.null(GEO)) {
  for (b in c("b1", "b2")) for (y in c("2025", "2026")) {
    LISA[[paste(b, y, sep = "_")]] <- tryCatch(compute_lisa(GEO, paste0(b, "_pct_", y)),
                                               error = function(e) { message("LISA gagal: ", conditionMessage(e)); NULL })
  }
}
