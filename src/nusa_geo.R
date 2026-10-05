# src/nusa_geo.R: ambil batas kab/kota dari API Laravel Nusa -> data/kabkota.geojson
# Endpoint: {NUSA_BASE}/regencies?per-page=100&page=N  (field: code "31.71", coordinates)
# Hasil disimpan sekali (cache), jadi API tidak dipanggil lagi pada run berikutnya.
# kedalaman bersarang: 1 = daftar titik, 2 = ring, 3 = daftar ring, 4 = multipolygon
.nest_lvl <- function(x) { d <- 0L; while (is.list(x) && length(x) > 0) { x <- x[[1]]; d <- d + 1L }; d }

.ring_mat <- function(r, swap) {
  m <- do.call(rbind, lapply(r, function(p) c(as.numeric(p[[1]]), as.numeric(p[[2]]))))
  if (swap) m <- m[, 2:1, drop = FALSE]                      # jadikan (lon, lat)
  if (nrow(m) < 3) return(NULL)
  if (!all(m[1, ] == m[nrow(m), ])) m <- rbind(m, m[1, ])    # tutup ring
  if (nrow(m) < 4) return(NULL)
  m
}

# Urutan (lat,lng) atau (lng,lat) dideteksi dari rentang Indonesia
.need_swap <- function(coords) {
  p <- coords
  while (is.list(p) && is.list(p[[1]])) p <- p[[1]]
  a <- as.numeric(p[[1]]); b <- as.numeric(p[[2]])
  !(a >= 94 && a <= 142)   # nilai pertama bukan bujur -> urutannya (lat, lng), perlu ditukar
}

coords_to_sfg <- function(coords) {
  if (is.null(coords) || length(coords) == 0) return(NULL)
  lv <- .nest_lvl(coords); swap <- .need_swap(coords)
  polys <- switch(as.character(lv),
    "2" = list(list(coords)),                                 # satu ring
    "3" = lapply(coords, function(r) list(r)),                # tiap ring = polygon sendiri (pulau)
    "4" = coords,                                             # multipolygon
    return(NULL))
  pl <- lapply(polys, function(pg) Filter(Negate(is.null), lapply(pg, .ring_mat, swap = swap)))
  pl <- Filter(function(x) length(x) > 0, pl)
  if (!length(pl)) return(NULL)
  sf::st_multipolygon(pl)
}

fetch_nusa_geo <- function(out = F_GEO, base = NUSA_BASE, per_page = 100) {
  message("Mengambil batas kab/kota dari ", base, " ...")
  rows <- list(); pg <- 1; last <- 1
  while (pg <= last) {
    url <- sprintf("%s/regencies?per-page=%d&page=%d", sub("/$", "", base), per_page, pg)
    js <- jsonlite::fromJSON(url, simplifyVector = FALSE)
    rows <- c(rows, js$data)
    last <- js$meta$last_page %||% js$meta[["last_page"]] %||% 1
    pg <- pg + 1
  }
  if (!length(rows)) stop("API Nusa tidak mengembalikan data.")
  geoms <- lapply(rows, function(r) tryCatch(coords_to_sfg(r$coordinates), error = function(e) NULL))
  ok <- !vapply(geoms, is.null, logical(1))
  if (!any(ok)) stop("API Nusa tidak menyertakan koordinat batas (field 'coordinates' kosong). ",
                     "Pakai GeoJSON batas kab/kota dari sumber lain (BPS/GADM) dan simpan di ", out)
  message(sprintf("Batas ditemukan untuk %d dari %d kab/kota.", sum(ok), length(rows)))
  g <- sf::st_sf(
    kode_kabkota = gsub("\\D", "", vapply(rows[ok], function(r) as.character(r$code), "")),
    nama_nusa    = vapply(rows[ok], function(r) as.character(r$name), ""),
    geometry     = sf::st_sfc(geoms[ok], crs = 4326))
  g <- sf::st_make_valid(g)
  dir.create(dirname(out), showWarnings = FALSE, recursive = TRUE)
  if (file.exists(out)) file.remove(out)
  sf::st_write(g, out, driver = "GeoJSON", quiet = TRUE)
  message("Tersimpan: ", out)
  invisible(g)
}
