# Fungsi pembangun visualisasi. Nomor di komentar (#1-#15) hanya penanda kode, tidak tampil di halaman.
PLOTS_VERSION <- "4.2"   # dicek oleh global.R agar app.R tidak berjalan dengan plots.R versi lama

# "Tidak signifikan" krem sangat muda; "tanpa data" (COL_NA) abu-abu kebiruan tua, agar keduanya tidak tertukar.
LISA_COLS <- c("#C2410C", "#0D9488", "#F59E6B", "#7FC8BF", "#EEE9DF")

# Ramp warna per jenis backlog (motif DESIGN.md): Backlog 1 = teal, Backlog 2 = ochre.
RAMP <- list(b1 = c("#E6F4F1", "#99E0D4", "#0D9488", "#0F766E", "#134E4A"),
             b2 = c("#FEF3C7", "#FCD34D", "#CA8A04", "#854D0E", "#422006"))
COL_NA <- "#727B8E"
b_name <- function(b) if (b == "b1") "Backlog 1" else "Backlog 2"

# #1 Kartu angka nasional. fill: "b1", "b2", atau "akses".
stat_card <- function(label, v26, v25, sub, good_when = "down", fill = "b1") {
  d <- v26 - v25
  good <- (good_when == "down" && d < 0) || (good_when == "up" && d > 0)
  tags$div(class = paste("stat-card", paste0("fill-", fill)),
           tags$div(class = "stat-label", label),
           tags$div(class = "stat-value", fmt_num(v26, 2), tags$span(class = "stat-unit", "%"),
                    tags$span(class = "stat-yr", "2026")),
           tags$div(class = "stat-sub", sub),
           tags$div(class = "stat-foot",
                    tags$span(class = paste("stat-delta", if (good) "good" else "bad"),
                              paste0(if (d < 0) "\u25BC " else "\u25B2 ", fmt_pp(d), " poin")),
                    tags$span(class = "stat-prev", paste0("2025: ", fmt_num(v25, 2), "%"))))
}

# #2 Dumbbell empat komponen rumah layak huni, 2025 vs 2026.
# Provinsi ikut otomatis bila kolom bangunan_2026, lantai_2026, air_2026, sanitasi_2026 ditambahkan.
komp_data <- function(sel = "Nasional") {
  vars <- names(COMP)
  row  <- if (sel == "Nasional") NAS else PROV[PROV$provinsi == sel, ]
  data.frame(id = vars, komp = unname(COMP),
             a = as.numeric(unlist(row[1, paste0(vars, "_2025")])),
             b = as.numeric(unlist(row[1, paste0(vars, "_2026")])), stringsAsFactors = FALSE)
}
p_dumbbell <- function(sel = "Nasional") {
  d <- komp_data(sel)
  d <- d[order(d$b), ]; d$y <- seq_len(nrow(d))
  lo <- min(80, floor(min(c(d$a, d$b), na.rm = TRUE) / 5) * 5)
  ann <- lapply(seq_len(nrow(d)), function(i) list(
    x = max(d$a[i], d$b[i]), y = d$y[i], text = paste0("<b>", fmt_pp(d$b[i] - d$a[i], 2), " poin</b>"),
    xshift = 14, xanchor = "left", showarrow = FALSE, font = list(size = 12, color = COL_TEAL_D)))
  plot_ly() %>%
    add_segments(data = d, x = ~a, xend = ~b, y = ~y, yend = ~y, line = list(color = "#CFC8BC", width = 6),
                 hoverinfo = "none", showlegend = FALSE) %>%
    add_markers(data = d, x = ~a, y = ~y, name = "2025",
                marker = list(size = 15, symbol = "square", color = "#FFFFFF", line = list(color = COL_SLATE, width = 2.5)),
                hovertemplate = "%{text}<extra>2025</extra>", text = ~sprintf("%s: %s%%", komp, fmt_num(a, 2))) %>%
    add_markers(data = d, x = ~b, y = ~y, name = "2026",
                marker = list(size = 15, symbol = "square", color = COL_OCHRE, line = list(color = COL_SLATE, width = 1)),
                hovertemplate = "%{text}<extra>2026</extra>", text = ~sprintf("%s: %s%%", komp, fmt_num(b, 2))) %>%
    theme_plotly() %>%
    layout(annotations = ann,
           xaxis = list(range = c(lo, 101), title = "% rumah tangga yang memenuhi komponen (sumbu tidak dimulai dari nol)",
                        gridcolor = COL_GRID, zeroline = FALSE),
           yaxis = list(tickmode = "array", tickvals = d$y, ticktext = d$komp, title = "", zeroline = FALSE),
           legend = list(orientation = "h", y = -0.3, x = 0), margin = list(l = 150, r = 30, t = 10, b = 60))
}

# #3 Peringkat provinsi (bar horizontal)
RANK_SCOPE <- c("5 provinsi tertinggi" = "top5", "5 provinsi terendah" = "bot5", "Seluruh provinsi" = "all")

rank_data <- function(id, scope = "top5") {
  m <- IND[IND$id == id, ]
  d <- data.frame(prov = PROV$provinsi, pulau = tc(PROV$pulau), v = PROV[[m$col]], stringsAsFactors = FALSE)
  d <- d[!is.na(d$v), ]
  d <- d[order(-d$v), ]; d$rank <- seq_len(nrow(d))
  d <- switch(scope, top5 = head(d, 5), bot5 = tail(d, 5), d)
  list(m = m, d = d, nat = NAS[[m$col]], n_all = sum(!is.na(PROV[[m$col]])))
}

p_rank <- function(id, scope = "top5") {
  r <- rank_data(id, scope); m <- r$m; d <- r$d; nat <- r$nat; dg <- m$digits
  d$grp <- switch(scope,
                  top5 = "5 provinsi tertinggi",
                  bot5 = "5 provinsi terendah",
                  ifelse(d$v > nat, "Di atas angka nasional", "Di bawah / sama dengan nasional"))
  # Merah bata = kondisi lebih buruk. Untuk indikator "tinggi = baik" (cakupan TBC) warnanya dibalik.
  hi_col <- if (isTRUE(m$good_high)) COL_DOWN else COL_UP
  lo_col <- if (isTRUE(m$good_high)) COL_UP else COL_DOWN
  cols <- c("5 provinsi tertinggi" = hi_col, "5 provinsi terendah" = lo_col,
            "Di atas angka nasional" = hi_col, "Di bawah / sama dengan nasional" = lo_col)
  d$tip <- sprintf("<b>%s</b><br>%s: %s<br>Peringkat %d dari %d \u00B7 %s", d$prov, m$label,
                   fmt_num(d$v, dg), d$rank, r$n_all, d$pulau)
  fig <- plot_ly()
  for (g in unique(d$grp)) {
    dd <- d[d$grp == g, ]
    fig <- fig %>% add_bars(x = dd$v, y = dd$prov, orientation = "h", name = g,
                            marker = list(color = cols[[g]]), text = fmt_num(dd$v, dg),
                            textposition = "outside", cliponaxis = FALSE, hovertext = dd$tip, hoverinfo = "text")
  }
  xr <- c(0, max(c(d$v, nat), na.rm = TRUE) * 1.15)
  fig %>%
    theme_plotly() %>%
    layout(barmode = "overlay", bargap = 0.25,
           xaxis = list(title = paste0(m$label, ", data tahun ", m$yr), gridcolor = COL_GRID, range = xr, zeroline = FALSE),
           yaxis = list(categoryorder = "array", categoryarray = rev(d$prov), title = "", tickfont = list(size = 11)),
           shapes = list(list(type = "line", x0 = nat, x1 = nat, y0 = 0, y1 = 1, yref = "paper",
                              line = list(color = COL_SLATE, width = 1.6, dash = "dash"))),
           annotations = list(list(x = nat, y = 1.0, yref = "paper", yanchor = "bottom", text = paste0("Nasional: ", fmt_num(nat, dg)),
                                   showarrow = FALSE, xanchor = "left", font = list(size = 11, color = COL_SLATE))),
           legend = list(orientation = "h", y = if (scope == "all") -0.05 else -0.3, x = 0),
           margin = list(l = 150, r = 50, t = 30, b = 50))
}

# #4 Choropleth kab/kota, #5 peta lingkaran
# Zoom gulir aktif setelah peta diklik dan mati lagi saat kursor keluar, agar halaman tidak tersangkut.
MAP_JS <- "function(el, x) {
  var map = this;
  map.scrollWheelZoom.disable();
  map.on('click', function() { map.scrollWheelZoom.enable(); el.classList.add('zoom-active'); });
  map.on('mouseout', function() { map.scrollWheelZoom.disable(); el.classList.remove('zoom-active'); });
}"
map_base <- function() {
  leaflet(options = leafletOptions(minZoom = 4, maxZoom = 12, zoomSnap = 0.25, zoomControl = TRUE)) %>%
    addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
    setView(lng = 118, lat = -2.5, zoom = 4.5) %>%
    addEasyButton(easyButton(icon = "fa-crosshairs", title = "Kembali ke seluruh Indonesia",
                             onClick = htmlwidgets::JS("function(btn, map){ map.setView([-2.5, 118], 4.5); }"))) %>%
    htmlwidgets::onRender(MAP_JS)
}

# Isi tooltip (hover) dan popup (klik, untuk layar sentuh): nama wilayah lalu angka yang relevan.
# head = baris tambahan di atas angka, mis. kategori LISA.
pop_html <- function(g, b, head = NULL) {
  p25 <- g[[paste0(b, "_pct_2025")]]; p26 <- g[[paste0(b, "_pct_2026")]]
  n25 <- g[[paste0(b, "_n_2025")]];   n26 <- g[[paste0(b, "_n_2026")]]
  nm  <- ifelse(is.na(g$label), "(tanpa data)", g$label)
  hd  <- if (is.null(head)) "" else paste0("<em>", head, "</em><br>")
  ifelse(is.na(g$label), "<div class='pop'><b>(tanpa data)</b></div>",
         sprintf("<div class='pop'><b>%s</b><br><span>%s</span><hr>%s<b>%s</b><br>2025: %s%% \u00B7 2026: %s%%<br>Perubahan: %s poin<br>Jumlah: %s ribu RT (2025: %s)<br>Total rumah tangga 2026: %s ribu</div>",
                 nm, ifelse(is.na(g$provinsi), "", g$provinsi), hd, b_name(b), fmt_num(p25, 2), fmt_num(p26, 2), fmt_pp(p26 - p25, 2),
                 fmt_num(n26, 2), fmt_num(n25, 2), fmt_num(g$rt_2026, 1)))
}
hover_lab <- function(html) lapply(html, htmltools::HTML)
TIP_OPT <- labelOptions(direction = "auto", className = "map-tip", sticky = TRUE)

# view: "2025" atau "2026" (persentase tahun itu) atau "chg" (perubahan 2025 ke 2026)
draw_choro <- function(b, view) {
  g <- GEO; chg <- identical(view, "chg")
  v <- if (chg) g[[paste0(b, "_pct_2026")]] - g[[paste0(b, "_pct_2025")]] else g[[paste0(b, "_pct_", view)]]
  ttl <- if (chg) paste0("Perubahan ", b_name(b), "<br>2025\u21922026 (poin)") else paste0("% ", b_name(b), " ", view)
  if (chg) {
    # hijau toska = turun (membaik), merah bata = naik, sama dengan COL_DOWN/COL_UP di grafik lain
    m <- max(abs(v), na.rm = TRUE)
    pal <- colorNumeric(c(COL_TEAL_D, "#F7F7F2", COL_TERRA), domain = c(-m, m), na.color = COL_NA)
  } else {
    q <- unique(stats::quantile(v, probs = seq(0, 1, 0.2), na.rm = TRUE))
    pal <- colorBin(grDevices::colorRampPalette(RAMP[[b]])(length(q) - 1), domain = v, bins = q, na.color = COL_NA)
  }
  html <- pop_html(g, b)
  map_base() %>%
    addPolygons(data = g, fillColor = pal(v), fillOpacity = 0.9, weight = 0.4, color = "#ffffff",
                label = hover_lab(html), labelOptions = TIP_OPT, popup = html,
                highlightOptions = highlightOptions(weight = 2, color = COL_SLATE, bringToFront = TRUE)) %>%
    addLegend("bottomright", pal = pal, values = v, title = ttl, opacity = 0.95,
              labFormat = labelFormat(suffix = if (chg) "" else "%", digits = 1)) %>%
    addLegend("bottomleft", colors = COL_NA, labels = "Tanpa data", opacity = 0.95)
}

draw_prop <- function(b, yr) {
  g <- GEO[!is.na(GEO[[paste0(b, "_pct_", yr)]]) & !is.na(GEO[[paste0(b, "_n_", yr)]]), ]
  g$n <- g[[paste0(b, "_n_", yr)]]; g$pct <- g[[paste0(b, "_pct_", yr)]]
  ctr <- suppressWarnings(sf::st_coordinates(sf::st_point_on_surface(sf::st_geometry(g))))
  g$lng <- ctr[, 1]; g$lat <- ctr[, 2]
  g <- g[order(-g$n), ]   # lingkaran besar digambar dulu agar yang kecil tetap terlihat
  br <- stats::quantile(g$pct, c(0, 1/3, 2/3, 1), na.rm = TRUE)
  g$kls <- cut(g$pct, unique(br), labels = c("Rendah", "Sedang", "Tinggi")[seq_len(length(unique(br)) - 1)], include.lowest = TRUE)
  cols <- RAMP[[b]][c(2, 3, 5)]
  pal <- colorFactor(cols, levels = c("Rendah", "Sedang", "Tinggi"))
  g$r <- pmax(2.5, sqrt(g$n / max(g$n, na.rm = TRUE)) * 32)
  top <- head(g, 10)
  map_base() %>%
    addPolygons(data = GEO, fillColor = "#F4F6FF", fillOpacity = 1, weight = 0.3, color = "#C9D2EA") %>%
    addCircleMarkers(data = sf::st_drop_geometry(g), lng = ~lng, lat = ~lat, radius = ~r,
                     fillColor = ~pal(as.character(kls)), fillOpacity = 0.78, color = "#ffffff", weight = 0.7,
                     label = hover_lab(pop_html(g, b)), labelOptions = TIP_OPT, popup = pop_html(g, b)) %>%
    addLabelOnlyMarkers(data = sf::st_drop_geometry(top), lng = ~lng, lat = ~lat, label = ~label,
                        labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, direction = "top",
                                                    style = list("font-size" = "11px", "font-weight" = "600", "color" = COL_SLATE,
                                                                 "text-shadow" = "0 0 3px #fff, 0 0 3px #fff"))) %>%
    addLegend("bottomright", colors = cols, labels = c("Rendah", "Sedang", "Tinggi"),
              title = paste0("Kelas % ", b_name(b), " ", yr), opacity = 0.95) %>%
    addControl(HTML("<div class='map-note'>Ukuran lingkaran = jumlah rumah tangga<br>Label = 10 jumlah terbesar</div>"), position = "bottomleft")
}

# #6 Peta pengelompokan LISA
draw_lisa <- function(b, yr) {
  key <- paste(b, yr, sep = "_")
  res <- LISA[[key]]
  validate(need(!is.null(res), "LISA tidak dapat dihitung untuk kombinasi ini."))
  g <- dplyr::left_join(GEO, res$data, by = "kode_kabkota")
  g$kategori <- factor(g$kategori, levels = LISA_LEVELS)
  pal <- colorFactor(LISA_COLS, levels = LISA_LEVELS, na.color = COL_NA)
  nm <- ifelse(is.na(g$label), "(tanpa data)", g$label)
  kat <- ifelse(is.na(g$kategori), "Tanpa data", as.character(g$kategori))
  map_base() %>%
    addPolygons(data = g, fillColor = pal(g$kategori), fillOpacity = 0.9, weight = 0.4, color = "#ffffff",
                label = hover_lab(pop_html(g, b, head = paste0("Kategori LISA ", yr, ": ", kat))), labelOptions = TIP_OPT,
                popup = pop_html(g, b, head = paste0("Kategori LISA ", yr, ": ", kat)),
                highlightOptions = highlightOptions(weight = 2, color = COL_SLATE, bringToFront = TRUE)) %>%
    addLegend("bottomright", colors = c(LISA_COLS, COL_NA), labels = c(LISA_LEVELS, "Tanpa data"), opacity = 0.95, title = "Kategori LISA (α = 5%)") %>%
    addControl(HTML(sprintf("<div class='map-note'><b>Moran's I = %s</b><br>p-value = %s<br>n = %d kab/kota \u00B7 %d tetangga terdekat</div>",
                            fmt_num(res$I, 3), fmt_p(res$p), res$n, KNN_K)), position = "topright")
}

# #7 Bubble: akses rumah layak vs indikator kesehatan
HEALTH_CHOICES <- c("Keluhan kesehatan (%)" = "keluhan", "Kasus diare dilayani (% penduduk)" = "diare", "Cakupan penemuan TBC (%)" = "tbc", "DBD per 100.000" = "dbd")

ISLAND_CHOICES <- c("Semua pulau" = "ALL", setNames(ISLAND_LEVELS, tc(ISLAND_LEVELS)))

# pulau: "ALL" atau satu pulau -> provinsi pulau tsb disorot & diberi label, lainnya dipudarkan
p_bubble <- function(yvar, pulau, cl, sel) {
  col <- VARS$col[VARS$id == yvar]; dg <- VARS$digits[VARS$id == yvar]
  d <- data.frame(provinsi = PROV$provinsi, pulau = PROV$pulau, x = PROV$rumah_layak_huni,
                  y = PROV[[col]], pop = PROV$penduduk_2025 / 1e6,
                  cl = unname(cl[PROV$provinsi]), stringsAsFactors = FALSE)
  d <- d[!is.na(d$y) & !is.na(d$x), ]
  validate(need(nrow(d) >= 4, "Data tidak cukup untuk indikator ini."))
  focus <- if (is.null(pulau) || pulau == "ALL") rep(TRUE, nrow(d)) else d$pulau == pulau
  if (length(sel)) focus <- focus & d$provinsi %in% sel
  d$op <- ifelse(focus, 0.9, 0.12)
  d$lbl <- if (!is.null(pulau) && pulau != "ALL") ifelse(d$pulau == pulau, d$provinsi, "") else
    ifelse(rank(-abs(as.numeric(scale(d$y))), ties.method = "first") <= 4, d$provinsi, "")
  d$tip <- sprintf("<b>%s</b><br>Akses rumah layak: %s%%<br>%s: %s<br>Penduduk: %s juta<br>Klaster %d \u00B7 %s",
                   d$provinsi, fmt_num(d$x, 2), VARS$label[VARS$id == yvar], fmt_num(d$y, dg), fmt_num(d$pop, 1), d$cl, tc(d$pulau))
  fig <- plot_ly()
  for (j in sort(unique(d$cl))) {
    dj <- d[d$cl == j, ]
    fig <- fig %>% add_markers(x = dj$x, y = dj$y, name = paste("Klaster", j), hovertext = dj$tip, hoverinfo = "text",
                               marker = list(size = bubble_diam(dj$pop, max(d$pop), dmax = 52), sizemode = "diameter", color = PAL_CL[j],
                                             opacity = dj$op, line = list(color = "#ffffff", width = 1)))
  }
  fit <- stats::lm(y ~ x, data = d)
  xs <- seq(min(d$x), max(d$x), length.out = 40)
  dl <- d[d$lbl != "", ]
  fig %>%
    add_lines(x = xs, y = stats::predict(fit, data.frame(x = xs)), name = "Garis tren (semua provinsi)",
              line = list(color = COL_SLATE, dash = "dash", width = 1.6), hoverinfo = "none") %>%
    add_text(x = dl$x, y = dl$y, text = dl$lbl, textposition = "top center", showlegend = FALSE,
             textfont = list(size = 11, color = COL_SLATE), hoverinfo = "none") %>%
    theme_plotly() %>%
    layout(xaxis = list(title = "% rumah tangga dengan akses rumah layak huni (2025)", gridcolor = COL_GRID, zeroline = FALSE),
           yaxis = list(title = VARS$label[VARS$id == yvar], gridcolor = COL_GRID, zeroline = FALSE),
           legend = list(orientation = "h", y = -0.22, x = 0), margin = list(t = 10))
}

# #8 Korelasi Spearman indikator hunian x indikator kesehatan (data asli, pasangan lengkap)
CORR_ROWS <- c(rumah_layak_huni = "Akses rumah layak huni", b2_pct_2025 = "Backlog 2 (2025)",
               bangunan_2025 = "Ketahanan bangunan", lantai_2025 = "Kecukupan luas lantai", air_2025 = "Air minum layak",
               sanitasi_2025 = "Sanitasi layak", babs_pct_2025 = "BABS", pen_nonpln_2025 = "Listrik non-PLN",
               asbes_pct_2025 = "Atap asbes")
CORR_COLS <- c(keluhan_kesehatan_pct_2025 = "Keluhan kesehatan", diare_pct = "Diare", dbd_100k = "DBD",
               tbc_cakupan_pct = "Cakupan TBC", miskin_2025 = "Kemiskinan (pembanding)")
corr_data <- function() {
  g <- expand.grid(r = names(CORR_ROWS), c = names(CORR_COLS), stringsAsFactors = FALSE)
  st <- do.call(rbind, lapply(seq_len(nrow(g)), function(i) sp_test(PROV[[g$r[i]]], PROV[[g$c[i]]])))
  data.frame(g, rho = st[, "rho"], p = st[, "p"], n = st[, "n"], row.names = NULL)
}
p_corr <- function() {
  d <- corr_data()
  nr <- length(CORR_ROWS); nc <- length(CORR_COLS)
  z <- matrix(d$rho, nrow = nr, ncol = nc)
  sig <- matrix(d$p < 0.05, nrow = nr, ncol = nc)
  lab <- matrix(sprintf("%s%s", fmt_num(d$rho, 2), ifelse(d$p < 0.05, "*", "")), nrow = nr, ncol = nc)
  hov <- matrix(sprintf("<b>%s \u00D7 %s</b><br>Spearman \u03C1 = %s<br>p %s \u00B7 n = %d provinsi",
                        CORR_ROWS[d$r], CORR_COLS[d$c], fmt_num(d$rho, 2),
                        ifelse(d$p < 0.001, "< 0,001", paste("=", fmt_p(d$p))), as.integer(d$n)), nrow = nr, ncol = nc)
  ann <- lapply(seq_len(nr * nc), function(k) {
    i <- (k - 1) %% nr + 1; j <- (k - 1) %/% nr + 1
    list(x = unname(CORR_COLS)[j], y = unname(CORR_ROWS)[i], text = lab[i, j], showarrow = FALSE,
         font = list(size = 11, color = if (abs(z[i, j]) >= 0.55) "#ffffff" else COL_SLATE,
                     family = FONT_SANS), xref = "x", yref = "y")
  })
  plot_ly(x = unname(CORR_COLS), y = unname(CORR_ROWS), z = z, type = "heatmap", text = hov, hoverinfo = "text",
          colorscale = list(c(0, "#2F5D8C"), c(0.5, "#FFFFFF"), c(1, COL_TERRA)), zmin = -1, zmax = 1, xgap = 2, ygap = 2,
          colorbar = list(title = "\u03C1", thickness = 12, len = 0.6)) %>%
    theme_plotly() %>%
    layout(annotations = ann,
           xaxis = list(side = "top", title = "", tickfont = list(size = 11)),
           yaxis = list(autorange = "reversed", title = "", tickfont = list(size = 11)),
           margin = list(l = 10, r = 10, t = 40, b = 10))
}

# #9 Heatmap berkelompok + dendrogram + pita warna (disusun manual di plotly)
# Variabel "tinggi = baik" dibalik tandanya dan diberi nama defisit, sehingga merah bata selalu berarti
# lebih buruk dari rata-rata provinsi dan hijau toska lebih baik.
DEFISIT_LAB <- c(bangunan = "Bangunan tak tahan", lantai = "Lantai sempit", air = "Air tak layak",
                 sanitasi = "Sanitasi tak layak", tbc = "Cakupan TBC rendah")
heat_lab <- function(id) ifelse(id %in% names(DEFISIT_LAB), DEFISIT_LAB[id], VARS$short[match(id, VARS$id)])
p_heatmap <- function(k) {
  X <- AN$X
  flip <- ifelse(VARS$good_high[match(colnames(AN$Z), VARS$id)], -1, 1)
  Z <- sweep(AN$Z, 2, flip, `*`)
  hr <- stats::hclust(stats::dist(Z), "ward.D2"); ord <- hr$order
  hc <- stats::hclust(stats::dist(t(Z)), "ward.D2"); ordc <- hc$order
  rn <- rownames(Z)[ord]; cn <- colnames(Z)[ordc]
  cn_lab <- unname(heat_lab(cn))
  zm <- Z[ord, ordc, drop = FALSE]
  note <- AN$imp_note[rn, cn, drop = FALSE]
  imp <- matrix(nzchar(note), nrow = nrow(note), dimnames = dimnames(note))
  hov <- outer(seq_along(rn), seq_along(cn), Vectorize(function(i, j)
    sprintf("<b>%s</b><br>%s: %s<br>skor defisit: %s%s", rn[i], VARS$label[match(cn[j], VARS$id)],
            fmt_num(X[rn[i], cn[j]], VARS$digits[match(cn[j], VARS$id)]), fmt_num(zm[i, j], 2),
            if (imp[i, j]) paste0("<br><i>", note[i, j], "</i>") else "")))
  im <- which(imp, arr.ind = TRUE)
  add_imp <- function(p) {
    if (!nrow(im)) return(p)
    add_markers(p, x = cn_lab[im[, 2]], y = im[, 1], xaxis = "x", yaxis = "y", showlegend = FALSE, hoverinfo = "skip",
                marker = list(symbol = "x-thin-open", size = I(rep(12, nrow(im))), color = COL_SLATE,
                              line = list(width = 2, color = COL_SLATE)))
  }
  cl  <- get_cl(k)[rn]
  isl <- match(PROV$pulau[match(rn, PROV$provinsi)], ISLAND_LEVELS)
  dd  <- ggdendro::dendro_data(hr)$segments
  hmax <- max(dd$y)
  n <- length(rn)
  
  plot_ly() %>%
    add_segments(data = dd, x = ~y, xend = ~yend, y = ~x, yend = ~xend, xaxis = "x2", yaxis = "y",
                 line = list(color = COL_MUTED, width = 1.2), hoverinfo = "none", showlegend = FALSE) %>%
    add_heatmap(x = "Klaster", y = seq_len(n), z = matrix(cl, ncol = 1), xaxis = "x3", yaxis = "y",
                colorscale = disc_scale(PAL_CL[seq_len(k)]), zmin = 1, zmax = k, showscale = FALSE,
                text = matrix(paste0(rn, ": Klaster ", cl), ncol = 1), hoverinfo = "text", xgap = 1, ygap = 1) %>%
    add_heatmap(x = "Pulau", y = seq_len(n), z = matrix(isl, ncol = 1), xaxis = "x4", yaxis = "y",
                colorscale = disc_scale(unname(PAL_ISLAND)), zmin = 1, zmax = length(ISLAND_LEVELS), showscale = FALSE,
                text = matrix(paste0(rn, ": ", tc(ISLAND_LEVELS[isl])), ncol = 1), hoverinfo = "text", xgap = 1, ygap = 1) %>%
    add_heatmap(x = cn_lab, y = seq_len(n), z = zm, xaxis = "x", yaxis = "y",
                colorscale = list(c(0, COL_TEAL), c(0.5, "#ffffff"), c(1, COL_TERRA)), zmin = -CL_CLIP, zmax = CL_CLIP,
                text = hov, hoverinfo = "text", xgap = 1, ygap = 1,
                colorbar = list(title = "lebih baik \u2190 \u2192 lebih buruk", thickness = 10, len = 0.36, x = 0.62, y = 1.1, orientation = "h",
                                xanchor = "center", yanchor = "bottom")) %>%
    add_imp() %>%
    theme_plotly() %>%
    layout(
      xaxis  = list(domain = c(0.235, 1), tickangle = -35, title = "", side = "bottom"),
      xaxis2 = list(domain = c(0, 0.15), anchor = "y", range = c(hmax * 1.02, 0), showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      xaxis3 = list(domain = c(0.16, 0.19), anchor = "y", showticklabels = FALSE, title = "", showgrid = FALSE),
      xaxis4 = list(domain = c(0.195, 0.225), anchor = "y", showticklabels = FALSE, title = "", showgrid = FALSE),
      yaxis  = list(range = c(n + 0.5, 0.5), tickmode = "array", tickvals = seq_len(n), ticktext = rn, side = "right",
                    showgrid = FALSE, zeroline = FALSE, tickfont = list(size = 11), anchor = "x"),
      margin = list(l = 10, r = 150, t = 60, b = 70),
      annotations = list(
        list(x = 0.175, y = 0, xref = "paper", yref = "paper", text = "Klaster", showarrow = FALSE, yshift = -34, textangle = -35, font = list(size = 11)),
        list(x = 0.21, y = 0, xref = "paper", yref = "paper", text = "Pulau", showarrow = FALSE, yshift = -30, textangle = -35, font = list(size = 11)))
    )
}

cluster_table <- function(k) {
  cl <- get_cl(k)
  X <- AN$X
  rows <- lapply(seq_len(k), function(j) {
    nm <- names(cl)[cl == j]
    ex <- nm[order(-PROV$penduduk_2025[match(nm, PROV$provinsi)])]
    means <- colMeans(X[nm, , drop = FALSE])
    data.frame(Klaster = paste("Klaster", j), `Jumlah provinsi` = length(nm),
               `Contoh provinsi` = paste(head(ex, 4), collapse = ", "),
               t(round(means, 2)), check.names = FALSE, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  names(out)[-(1:3)] <- VARS$short[match(names(out)[-(1:3)], VARS$id)]
  out
}

# #10a Muatan PCA, #10b biplot
p_loadings <- function() {
  ld <- AN$pca$rotation[, 1:2]; ve <- summary(AN$pca)$importance[2, 1:2] * 100
  lab <- unname(VARS$short[match(rownames(ld), VARS$id)])
  o <- order(ld[, 1])
  plot_ly(y = lab[o]) %>%
    add_bars(x = ld[o, 1], orientation = "h", name = sprintf("PC1 (%.1f%%)", ve[1]), marker = list(color = COL_SLATE),
             hovertemplate = "%{y}: %{x:.2f}<extra>PC1</extra>") %>%
    add_bars(x = ld[o, 2], orientation = "h", name = sprintf("PC2 (%.1f%%)", ve[2]), marker = list(color = COL_OCHRE),
             hovertemplate = "%{y}: %{x:.2f}<extra>PC2</extra>") %>%
    theme_plotly() %>%
    layout(barmode = "group", bargap = 0.25,
           xaxis = list(title = "Muatan (loading)", gridcolor = COL_GRID, zeroline = TRUE, zerolinecolor = COL_INK3, range = c(-0.75, 0.75)),
           yaxis = list(title = "", categoryorder = "array", categoryarray = lab[o], tickfont = list(size = 11)),
           legend = list(orientation = "h", y = -0.15, x = 0), margin = list(t = 10, l = 10))
}

p_pca <- function(cl, sel) {
  pc <- AN$pca; sc <- pc$x[, 1:2]; ld <- pc$rotation[, 1:2]
  ve <- summary(pc)$importance[2, 1:2] * 100
  arrow_scale <- 0.85 * max(abs(sc)) / max(abs(ld))
  d <- data.frame(provinsi = rownames(sc), PC1 = sc[, 1], PC2 = sc[, 2], cl = unname(cl[rownames(sc)]), stringsAsFactors = FALSE)
  dist <- sqrt(as.numeric(scale(d$PC1))^2 + as.numeric(scale(d$PC2))^2)
  d$lbl <- ifelse(rank(-dist, ties.method = "first") <= 5, d$provinsi, "")   # label hanya 5 pencilan
  d$op <- if (length(sel)) ifelse(d$provinsi %in% sel, 1, 0.2) else 0.85
  imp_var <- apply(AN$imp_note[d$provinsi, , drop = FALSE], 1, function(r) paste(VARS$short[match(names(r)[nzchar(r)], VARS$id)], collapse = ", "))
  d$tip <- sprintf("<b>%s</b><br>Klaster %d<br>PC1: %.2f \u00B7 PC2: %.2f%s", d$provinsi, d$cl, d$PC1, d$PC2,
                   ifelse(nzchar(imp_var), paste0("<br><i>", imp_var, ": kosong di BPS, diisi untuk analisis</i>"), ""))
  arrows <- lapply(rownames(ld), function(v) list(
    x = ld[v, 1] * arrow_scale, y = ld[v, 2] * arrow_scale, ax = 0, ay = 0, axref = "x", ayref = "y", xref = "x", yref = "y",
    showarrow = TRUE, arrowhead = 2, arrowsize = 1, arrowwidth = 1.4, arrowcolor = COL_INK3, text = ""))
  vl <- data.frame(x = ld[, 1] * arrow_scale * 1.1, y = ld[, 2] * arrow_scale * 1.1, t = VARS$short[match(rownames(ld), VARS$id)])
  plot_ly(source = "pca") %>%
    add_markers(data = d, x = ~PC1, y = ~PC2, key = ~provinsi, hovertext = ~tip, hoverinfo = "text", name = "Provinsi",
                marker = list(size = 12, color = PAL_CL[d$cl], opacity = d$op, line = list(color = "#fff", width = 1)), showlegend = FALSE) %>%
    add_text(data = d, x = ~PC1, y = ~PC2, text = ~lbl, textposition = "top center", textfont = list(size = 11, color = COL_INK),
             hoverinfo = "none", showlegend = FALSE) %>%
    add_text(data = vl, x = ~x, y = ~y, text = ~t, textfont = list(size = 11, color = COL_INK3, family = FONT_SANS),
             hoverinfo = "none", showlegend = FALSE) %>%
    theme_plotly(select = TRUE) %>%
    layout(dragmode = "select", annotations = arrows,
           xaxis = list(title = sprintf("PC1 (%.1f%%)", ve[1]), zeroline = TRUE, zerolinecolor = "#ced4da", gridcolor = COL_GRID),
           yaxis = list(title = sprintf("PC2 (%.1f%%)", ve[2]), zeroline = TRUE, zerolinecolor = "#ced4da", gridcolor = COL_GRID),
           margin = list(t = 10))
}

# #11 Treemap & #12 Icicle (hierarki Pulau > Provinsi > Kab/kota)
squish_to <- function(x, rng) pmin(pmax(x, rng[1]), rng[2])

build_tree <- function(yr, size_col, color_col, root = FALSE) {
  leaf <- KAB %>%
    transmute(kode_kabkota, kode_prov, provinsi, pulau, label,
              rt = .data[[paste0("rt_", yr)]],
              size = .data[[size_col]],
              pct = .data[[color_col]],
              b1p = .data[[paste0("b1_pct_", yr)]], b2p = .data[[paste0("b2_pct_", yr)]],
              b2n = .data[[paste0("b2_n_", yr)]]) %>%
    filter(is.finite(size), size > 0, is.finite(rt))
  tot <- sum(leaf$size)
  wm <- function(p, w) { ok <- is.finite(p) & is.finite(w); if (!any(ok)) NA_real_ else sum(p[ok] * w[ok]) / sum(w[ok]) }
  
  prov <- leaf %>% group_by(kode_prov, provinsi, pulau) %>%
    summarise(value = sum(size), pct = wm(pct, rt), rt = sum(rt), b2n = sum(b2n, na.rm = TRUE), .groups = "drop")
  isl <- leaf %>% group_by(pulau) %>%
    summarise(value = sum(size), pct = wm(pct, rt), rt = sum(rt), b2n = sum(b2n, na.rm = TRUE), .groups = "drop")
  
  top <- if (root) "ROOT" else ""
  nodes <- bind_rows(
    if (root) tibble::tibble(id = "ROOT", parent = "", label = "Indonesia", value = sum(isl$value),
                             pct = wm(isl$pct, isl$rt), rt = sum(isl$rt), b2n = sum(isl$b2n), lvl = "Nasional"),
    isl  %>% transmute(id = paste0("I_", pulau), parent = top, label = tc(pulau), value, pct, rt, b2n, lvl = "Pulau"),
    prov %>% transmute(id = paste0("P_", kode_prov), parent = paste0("I_", pulau), label = provinsi, value, pct, rt, b2n, lvl = "Provinsi"),
    leaf %>% transmute(id = paste0("K_", kode_kabkota), parent = paste0("P_", kode_prov), label, value = size, pct, rt, b2n, lvl = "Kab/kota")
  )
  nodes$share <- nodes$value / tot * 100
  nodes
}

tree_hover <- function(nd, pct_label, size_label, unit = "ribu RT") {
  sprintf("<b>%s</b> (%s)<br>%s: %s %s<br>%s: %s%%<br>Kontribusi ke total nasional: %s%%",
          nd$label, nd$lvl, size_label, fmt_num(nd$value, 1), unit, pct_label, fmt_num(nd$pct, 2), fmt_num(nd$share, 1))
}

# Treemap diwarnai % Backlog 2 (ramp ochre), icicle diwarnai % Backlog 1 (ramp teal).
TREE_PAL_1 <- RAMP$b2
TREE_PAL_2 <- RAMP$b1

p_treemap <- function(yr) {
  nd <- build_tree(yr, paste0("rt_", yr), paste0("b2_pct_", yr))
  rng <- stats::quantile(nd$pct[nd$lvl == "Kab/kota"], c(0.02, 0.98), na.rm = TRUE)
  cf <- scales::col_numeric(TREE_PAL_1, domain = rng, na.color = COL_NA)
  plot_ly(type = "treemap", ids = nd$id, labels = nd$label, parents = nd$parent, values = nd$value,
          branchvalues = "total", marker = list(colors = cf(squish_to(nd$pct, rng)), line = list(width = 1, color = "#ffffff")),
          hovertext = tree_hover(nd, paste0("% Backlog 2 ", yr), "Total RT"), hoverinfo = "text",
          customdata = sprintf("B2 %s%%", fmt_num(nd$pct, 1)), texttemplate = "<b>%{label}</b><br>%{customdata}",
          pathbar = list(visible = TRUE, thickness = 28), tiling = list(packing = "squarify"),
          textfont = list(size = 13)) %>%
    theme_plotly(modebar = FALSE) %>% layout(margin = list(l = 0, r = 0, t = 10, b = 0))
}
treemap_legend <- function(yr) {
  nd <- build_tree(yr, paste0("rt_", yr), paste0("b2_pct_", yr))
  rng <- stats::quantile(nd$pct[nd$lvl == "Kab/kota"], c(0.02, 0.98), na.rm = TRUE)
  legend_bar(TREE_PAL_1, paste0(fmt_num(rng[1], 1), "%"), paste0(fmt_num(rng[2], 1), "%"), "Warna = % Backlog 2 (cokelat tua = tinggi)")
}

# Icicle dari atas ke bawah: pulau, provinsi, kab/kota (tanpa simpul Indonesia).
# b menentukan backlog yang dipakai: lebar = jumlah rumah tangga, warna = persentase backlog yang sama.
p_icicle <- function(yr, b = "b2") {
  nd <- build_tree(yr, paste0(b, "_n_", yr), paste0(b, "_pct_", yr))
  rng <- stats::quantile(nd$pct[nd$lvl == "Kab/kota"], c(0.02, 0.98), na.rm = TRUE)
  cf <- scales::col_numeric(RAMP[[b]], domain = rng, na.color = COL_NA)
  info <- sprintf("Porsi %s%%<br>%s%%", fmt_num(nd$share, 1), fmt_num(nd$pct, 1))
  plot_ly(type = "icicle", ids = nd$id, labels = nd$label, parents = nd$parent, values = nd$value,
          branchvalues = "total", maxdepth = 3, customdata = info,
          marker = list(colors = cf(squish_to(nd$pct, rng)), line = list(width = 1.5, color = "#ffffff")),
          hovertext = tree_hover(nd, paste0("% ", b_name(b), " ", yr), paste("Jumlah", b_name(b)), "ribu RT"), hoverinfo = "text",
          texttemplate = "<b>%{label}</b><br>%{customdata}", insidetextfont = list(size = 11),
          pathbar = list(visible = TRUE, thickness = 24, side = "top"), tiling = list(orientation = "v"),
          sort = TRUE) %>%
    theme_plotly(modebar = FALSE) %>% layout(margin = list(l = 0, r = 0, t = 10, b = 0))
}
icicle_legend <- function(yr, b = "b2") {
  nd <- build_tree(yr, paste0(b, "_n_", yr), paste0(b, "_pct_", yr))
  rng <- stats::quantile(nd$pct[nd$lvl == "Kab/kota"], c(0.02, 0.98), na.rm = TRUE)
  legend_bar(RAMP[[b]], paste0(fmt_num(rng[1], 1), "%"), paste0(fmt_num(rng[2], 1), "%"),
             sprintf("Warna = %% %s (makin tua makin tinggi; abu-abu = tanpa data)", b_name(b)))
}

# #13 Capaian program: hanya program yang bisa dibandingkan Semester I 2025 vs Semester I 2026.
# Digambar sebagai baris HTML (small multiples): skala tiap program berbeda jauh (235 sampai 120.976),
# jadi tiap baris punya skala sendiri dan nilainya ditulis langsung.
cap_change_txt <- function(a, b) {
  if (!is.finite(a) || !is.finite(b) || a == 0) return("tidak dapat dihitung")
  r <- b / a
  if (r >= 2) paste0("naik ", fmt_num(r, 1), " kali") else paste0(if (r >= 1) "naik " else "turun ", fmt_num(abs(r - 1) * 100, 1), "%")
}
cap_rows <- function() {
  d <- CAPR$cmp
  if (is.null(d) || !nrow(d)) return(tags$p(class = "empty-state", "Tabel program yang dapat dibandingkan belum tersedia. Letakkan berkas program_dapat_dibandingkan.xlsx di folder data."))
  d <- d[order(d$chg), ]
  bar <- function(lab, v, mx, cls) tags$div(class = "cap-bar",
                                            tags$span(class = "cap-lab", lab),
                                            tags$span(class = "cap-track", tags$span(class = paste("cap-fill", cls), style = sprintf("width:%.2f%%", max(v / mx * 100, 0.6)))),
                                            tags$span(class = "cap-val", fmt_num(v, 0)))
  tags$div(class = "cap-list", role = "list",
           lapply(seq_len(nrow(d)), function(i) {
             r <- d[i, ]; mx <- max(r$s1_2025, r$s1_2026, na.rm = TRUE)
             up <- isTRUE(r$s1_2026 >= r$s1_2025)
             tags$div(class = "cap-row", role = "listitem",
                      tags$div(class = "cap-name", tags$b(r$nama), tags$span(class = "cap-unit", tolower(r$satuan))),
                      tags$div(class = "cap-bars",
                               bar("Sem. I 2025", r$s1_2025, mx, "y25"),
                               bar("Sem. I 2026", r$s1_2026, mx, "y26")),
                      tags$div(class = paste("cap-chg", if (up) "up" else "down"), cap_change_txt(r$s1_2025, r$s1_2026)))
           }))
}

# Skala program terhadap backlog 2026: lebar penuh bilah = seluruh backlog. Ini ukuran skala, bukan perbandingan antartahun.
cap_scale <- function() {
  row <- function(prog, v, bk, blab) {
    if (!is.finite(v) || !is.finite(bk)) return(NULL)
    pct <- v / (bk * 1000) * 100
    tags$div(class = "scale-row",
             tags$div(class = "scale-head", tags$b(prog), sprintf(" %s unit pada Semester I 2026 = %s%% dari %s (%s juta rumah tangga)",
                                                                  fmt_num(v, 0), fmt_num(pct, 2), blab, fmt_num(bk / 1000, 2))),
             tags$div(class = "scale-track", role = "img", `aria-label` = sprintf("%s%% dari %s", fmt_num(pct, 2), blab),
                      tags$div(class = "scale-fill", style = sprintf("width:%.3f%%", max(pct, 0.4)))))
  }
  g <- function(key) CAP$s1_2026[grepl(key, CAP$program)][1]
  tags$div(class = "cap-scale",
           tags$h4(class = "sub", "Dibanding besarnya backlog 2026"),
           row("FLPP (pembiayaan rumah)", g("FLPP"), NAS$b1_n_2026, "Backlog 1"),
           row("BSPS (bedah rumah)", g("BSPS"), NAS$b2_n_2026, "Backlog 2"))
}

# #14 Kuadran Backlog 1 vs Backlog 2
quad_data <- function(yr) {
  data.frame(provinsi = PROV$provinsi, pulau = PROV$pulau,
             x = PROV[[paste0("b1_pct_", yr)]], y = PROV[[paste0("b2_pct_", yr)]], rt = PROV[[paste0("rt_", yr)]],
             bl = PROV[[paste0("b1_n_", yr)]] + PROV[[paste0("b2_n_", yr)]], stringsAsFactors = FALSE)
}
quad_class <- function(d, nx, ny) {
  ifelse(d$x > nx & d$y > ny, "Masalah ganda",
         ifelse(d$x <= nx & d$y > ny, "Masalah kelayakhunian",
                ifelse(d$x > nx & d$y <= ny, "Masalah kepemilikan", "Relatif baik")))
}
# Diameter dihitung sendiri (bubble_diam): trace Bali hanya 1 titik, dan plotly membaca ukuran tunggal sebagai piksel.
p_quadrant <- function(yr) {
  d <- quad_data(yr)
  d <- d[stats::complete.cases(d$x, d$y, d$bl), ]
  nx <- NAS[[paste0("b1_pct_", yr)]]; ny <- NAS[[paste0("b2_pct_", yr)]]
  d$z <- abs(as.numeric(scale(d$x))) + abs(as.numeric(scale(d$y)))
  d$lbl <- ifelse(rank(-d$z, ties.method = "first") <= 6 | rank(-d$bl, ties.method = "first") <= 3, d$provinsi, "")
  d$kuad <- quad_class(d, nx, ny)
  d$tip <- sprintf("<b>%s</b> (%s)<br>Backlog 1: %s%%<br>Backlog 2: %s%%<br>Rumah tangga ber-backlog (B1 + B2): %s ribu<br>Kuadran: %s",
                   d$provinsi, tc(d$pulau), fmt_num(d$x, 2), fmt_num(d$y, 2), fmt_num(d$bl, 0), d$kuad)
  xmax <- max(c(d$x, nx)) * 1.08; ymax <- max(c(d$y, ny)) * 1.16   # ruang di atas agar label kuadran tidak menimpa titik
  qa <- function(x, y, t, xa, ya) list(x = x, y = y, xref = "paper", yref = "paper", text = t, showarrow = FALSE,
                                       xanchor = xa, yanchor = ya, font = list(size = 11, color = COL_INK3))
  qr <- function(x0, x1, y0, y1, col) list(type = "rect", xref = "x", yref = "y", x0 = x0, x1 = x1, y0 = y0, y1 = y1,
                                           fillcolor = col, line = list(width = 0), layer = "below")
  fig <- plot_ly()
  for (isl in intersect(ISLAND_LEVELS, unique(d$pulau))) {
    di <- d[d$pulau == isl, ]
    fig <- fig %>% add_trace(type = "scatter", mode = "markers", x = di$x, y = di$y, name = tc(isl),
                             hovertext = di$tip, hoverinfo = "text",
                             marker = list(size = bubble_diam(di$bl, max(d$bl), dmax = 34, dmin = 6), sizemode = "diameter",
                                           color = PAL_ISLAND[[isl]], opacity = 0.85,
                                           line = list(color = "#ffffff", width = 1.2)))
  }
  dl <- d[d$lbl != "", ]
  fig %>%
    add_trace(type = "scatter", mode = "text", x = dl$x, y = dl$y, text = dl$lbl, textposition = "top center",
              textfont = list(size = 10, color = COL_SLATE), hoverinfo = "none", showlegend = FALSE) %>%
    theme_plotly() %>%
    layout(xaxis = list(title = list(text = paste0("% Backlog 1 (kepemilikan), ", yr), font = list(color = COL_TEAL_D, size = 12)), gridcolor = COL_GRID, zeroline = FALSE, range = c(0, xmax), tickfont = list(size = 11)),
           yaxis = list(title = list(text = paste0("% Backlog 2 (kelayakhunian), ", yr), font = list(color = COL_OCHRE_D, size = 12)), gridcolor = COL_GRID, zeroline = FALSE, range = c(0, ymax), tickfont = list(size = 11)),
           shapes = list(
             qr(0, nx, ny, ymax, "rgba(202,138,4,0.10)"),     # kelayakhunian: ochre (Backlog 2)
             qr(nx, xmax, ny, ymax, "rgba(194,65,12,0.10)"),  # ganda: merah bata
             qr(nx, xmax, 0, ny, "rgba(13,148,136,0.10)"),    # kepemilikan: teal (Backlog 1)
             qr(0, nx, 0, ny, "rgba(71,85,105,0.05)"),        # relatif baik: netral
             list(type = "line", x0 = nx, x1 = nx, y0 = 0, y1 = 1, yref = "paper", line = list(color = COL_SLATE, dash = "dash", width = 1.2)),
             list(type = "line", y0 = ny, y1 = ny, x0 = 0, x1 = 1, xref = "paper", line = list(color = COL_SLATE, dash = "dash", width = 1.2))),
           annotations = list(qa(0.99, 0.99, "<b>Masalah ganda</b>", "right", "top"), qa(0.01, 0.99, "<b>Masalah kelayakhunian</b>", "left", "top"),
                              qa(0.99, 0.01, "<b>Masalah kepemilikan</b>", "right", "bottom"), qa(0.01, 0.01, "<b>Relatif baik</b>", "left", "bottom")),
           legend = list(orientation = "h", y = -0.16, x = 0, font = list(size = 11), title = list(text = "Pulau: ")),
           hoverlabel = list(font = list(size = 12)), margin = list(t = 20))
}

# #15 Perubahan persentase backlog per provinsi, 2025 -> 2026
change_data <- function(b) {
  d <- data.frame(provinsi = PROV$provinsi, a = PROV[[paste0(b, "_pct_2025")]], b = PROV[[paste0(b, "_pct_2026")]],
                  stringsAsFactors = FALSE)
  d <- d[stats::complete.cases(d), ]
  d$chg <- d$b - d$a
  d[order(d$chg), ]
}
p_change <- function(b) {
  d <- change_data(b); d$y <- seq_len(nrow(d))
  d$col <- ifelse(d$chg < 0, COL_TEAL_D, ifelse(d$chg > 0, COL_TERRA, COL_MUTED))
  d$tip <- sprintf("<b>%s</b><br>2025: %s%%<br>2026: %s%%<br>Perubahan: %s poin", d$provinsi,
                   fmt_num(d$a, 2), fmt_num(d$b, 2), fmt_pp(d$chg, 2))
  nat <- NAS[[paste0(b, "_pct_2026")]] - NAS[[paste0(b, "_pct_2025")]]
  segs <- lapply(seq_len(nrow(d)), function(i) list(type = "line", x0 = d$a[i], x1 = d$b[i], y0 = d$y[i], y1 = d$y[i],
                                                    line = list(color = d$col[i], width = 4), layer = "below"))
  plot_ly() %>%
    add_markers(x = d$a, y = d$y, name = "2025", hovertext = d$tip, hoverinfo = "text",
                marker = list(size = 10, color = "#FFFFFF", line = list(color = COL_INK3, width = 2))) %>%
    add_markers(x = d$b, y = d$y, name = "2026", hovertext = d$tip, hoverinfo = "text",
                marker = list(size = 11, color = I(d$col), line = list(color = "#FFFFFF", width = 1))) %>%
    add_text(x = pmax(d$a, d$b), y = d$y, text = paste0(" ", fmt_pp(d$chg, 2)), textposition = "middle right",
             textfont = list(size = 11, color = I(d$col)), hoverinfo = "none", showlegend = FALSE, cliponaxis = FALSE) %>%
    theme_plotly() %>%
    layout(shapes = segs,
           xaxis = list(title = paste0("% ", b_name(b), " (titik putih = 2025, titik berwarna = 2026)"), gridcolor = COL_GRID,
                        zeroline = FALSE, range = c(0, max(c(d$a, d$b)) * 1.12)),
           yaxis = list(tickmode = "array", tickvals = d$y, ticktext = d$provinsi, title = "", tickfont = list(size = 11),
                        autorange = "reversed", zeroline = FALSE),
           annotations = list(list(x = 1, y = 1.01, xref = "paper", yref = "paper", xanchor = "right", yanchor = "bottom",
                                   showarrow = FALSE, font = list(size = 11, color = COL_INK3),
                                   text = sprintf("Nasional: %s poin", fmt_pp(nat, 2)))),
           legend = list(orientation = "h", y = -0.06, x = 0), margin = list(t = 30, l = 10, r = 40))
}
