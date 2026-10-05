# app.R: UI dan server laporan. Jalankan dengan shiny::runApp(); global.R dimuat otomatis.
# Muat ulang global.R bila belum dimuat, versinya lama, atau paket grafik belum terpasang di sesi.
if (!exists("APP_VERSION", inherits = TRUE) || !identical(APP_VERSION, "4.2") ||
    !exists("PLOTS_VERSION", inherits = TRUE) || !identical(PLOTS_VERSION, "4.2") ||
    !all(c("package:plotly", "package:leaflet", "package:DT") %in% search())) source("global.R", local = FALSE)

# Catatan baca di bawah grafik, ditulis seperti catatan kaki: "*Warna: ...".
fig_notes <- function(notes) {
  if (!length(notes)) return(NULL)
  tags$ul(class = "fig-notes",
          lapply(names(notes), function(n) tags$li(tags$span(class = "fn-key", paste0("*", n, ":")), " ", notes[[n]])))
}

# Pertanyaan lanjutan sebagai accordion. details/summary bisa dibuka dengan Tab + Enter tanpa JavaScript.
qa <- function(q, ...) list(q = q, a = tagList(...))
more_box <- function(items) {
  if (!length(items)) return(NULL)
  tags$div(class = "more",
           lapply(items, function(it) tags$details(class = "more-item", tags$summary(it$q), tags$div(class = "more-body", it$a))))
}

# Kartu grafik. Judul = pertanyaan yang dijawab grafik. side = TRUE menaruh interpretasi di samping grafik
# pada layar lebar (grafik tinggi atau peta), side = FALSE menaruhnya di bawah (grafik yang butuh lebar penuh).
viz <- function(title, body, controls = NULL, notes = NULL, insight = NULL, more = NULL,
                src = NULL, extra = NULL, side = TRUE, wide = FALSE, id = NULL) {
  ins <- if (!is.null(insight))
    tags$aside(class = "viz-insight", `aria-label` = paste("Interpretasi:", title),
               tags$h4(class = "ins-title", "Interpretasi"), uiOutput(insight))
  tags$article(id = id, class = paste("viz-card", if (side && !is.null(insight)) "is-side" else "is-stack", if (wide) "scroll-x"),
               tags$header(class = "viz-head", tags$h3(title)),
               if (!is.null(controls)) tags$div(class = "viz-controls", controls),
               tags$div(class = "viz-grid",
                        tags$div(class = "viz-main",
                                 tags$div(class = "viz-body", body),
                                 extra, fig_notes(notes),
                                 if (!is.null(src)) src_note(src)),
                        ins),
               more_box(more))
}

chapter <- function(id, kicker, title, lead, ...) {
  tags$section(id = id, class = "chapter",
               tags$div(class = "chapter-head",
                        tags$span(class = "kicker", kicker),
                        tags$h2(title), if (!is.null(lead)) tags$p(class = "lead", lead)),
               ...)
}

ctl <- function(id, label, choices, selected = NULL, variant = "backlog") {
  tags$div(class = paste("ctl", paste0("ctl-", variant)),
           tags$span(class = "ctl-label", id = paste0(id, "-lbl"), label),
           radioButtons(id, NULL, choices = choices, selected = selected %||% choices[[1]], inline = TRUE))
}
dd <- function(id, label, choices, selected = NULL, width = "280px", multiple = FALSE) {
  tags$div(class = "ctl ctl-select",
           tags$label(class = "ctl-label", `for` = id, label),
           selectInput(id, NULL, choices = choices, selected = selected, width = width, multiple = multiple))
}

B_CHOICES  <- c("Backlog 1 · Kepemilikan" = "b1", "Backlog 2 · Kelayakhunian" = "b2")
YR_CHOICES <- c("2025", "2026")
QUADS      <- c("Masalah ganda", "Masalah kepemilikan", "Masalah kelayakhunian", "Relatif baik")
geo_missing <- is.null(GEO)

# Angka hero, semuanya dari data yang juga tampil di kartu angka dan dumbbell.
H_LAYAK <- NAS$akses_layak_2026
H_B1    <- as.numeric(NAS$b1_n_2026) / 1000
H_B2    <- as.numeric(NAS$b2_n_2026) / 1000
H_SAN   <- 100 - as.numeric(NAS$sanitasi_2026)

NAV <- list(c("gambaran", "Gambaran Nasional"), c("sebaran", "Sebaran Wilayah"), c("dua-wajah", "Dua Wajah Backlog"),
            c("kesehatan", "Hunian & Kesehatan"), c("intervensi", "Intervensi"), c("kesimpulan", "Kesimpulan"),
            c("unduh", "Unduh Data"))

# Baris unduhan: berkas di folder data disajikan apa adanya.
dl_row <- function(id, file, desc) {
  tags$div(class = "dl-row",
           tags$div(class = "dl-text", tags$span(class = "dl-file", file), tags$p(desc)),
           tags$div(class = "dl-actions", downloadButton(id, "Unduh (.xlsx)", class = "btn-dl", icon = NULL)))
}

ui <- fluidPage(
  title = "Hunian Layak Indonesia: Backlog Perumahan dan Kesehatan 2025–2026",
  lang = "id",
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1, viewport-fit=cover"),
    tags$meta(name = "theme-color", content = "#0F172A"),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = NA),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Poppins:ital,wght@0,400;0,500;0,600;0,700;0,800;1,400&display=swap"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(src = "app.js", defer = NA)
  ),
  
  tags$a(class = "skip-link", href = "#gambaran", "Lewati ke isi laporan"),
  tags$header(class = "site-header",
              tags$div(class = "hdr-inner",
                       tags$a(class = "brand", href = "#hero",
                              tags$span(class = "brand-kicker", "Laporan Analisis Data Spasial"),
                              tags$span(class = "brand-name", "Hunian Layak Indonesia")),
                       tags$nav(id = "site-nav", class = "site-nav", `aria-label` = "Bab",
                                lapply(NAV, function(x) tags$a(href = paste0("#", x[1]), x[2]))),
                       tags$button(type = "button", class = "nav-toggle", `aria-controls` = "site-nav", `aria-expanded` = "false",
                                   tags$span(class = "nav-toggle-bars", `aria-hidden` = "true", tags$span(), tags$span(), tags$span()),
                                   tags$span(class = "nav-toggle-txt", "Menu"))),
              tags$div(class = "progress-track", tags$div(id = "progress"))),
  tags$button(id = "to-top", class = "to-top", type = "button", `aria-label` = "Kembali ke atas", "↑"),
  
  tags$main(id = "top", class = "site-main",
            tags$section(id = "hero", class = "hero",
                         tags$div(class = "hero-inner",
                                  tags$h1("Ketimpangan Kepemilikan dan Kelayakhunian Rumah di Indonesia"),
                                  tags$p(class = "hero-sub-title",
                                         sprintf("Backlog perumahan di %d provinsi dan %d kabupaten/kota, Susenas 2025 dan 2026",
                                                 nrow(PROV), nrow(KAB))),
                                  tags$p(class = "hero-lead",
                                         sprintf("Pada 2026, %s%% rumah tangga Indonesia memiliki akses rumah layak huni artinya sekitar %d dari 10 rumah tangga belum memiliki rumah yang layak dihuni. ",
                                                 fmt_num(H_LAYAK), round((100 - H_LAYAK) / 10)),
                                         tags$b(sprintf("%s juta", fmt_num(H_B1, 2))), " rumah tangga belum memiliki rumah sendiri, dan ",
                                         tags$b(sprintf("%s juta", fmt_num(H_B2, 2))), " menempati rumah milik sendiri yang belum layak. ",
                                         sprintf("%s%% rumah tangga juga belum memiliki sanitasi layak, kondisi yang berkaitan dengan penyakit seperti diare.",
                                                 fmt_num(H_SAN))),
                                  tags$div(class = "hero-cta",
                                           tags$a(class = "cta cta-primary", href = "#gambaran", "Mulai membaca")),
                                  tags$p(class = "hero-foot", "Sumber: BPS (Susenas; Statistik Perumahan 2026), Kementerian Kesehatan, Kementrian PKP. "
                                      ))),
            
            chapter("gambaran", "Bab 1 · Gambaran Nasional", "Membaik secara agregat, timpang antarwilayah",
                    tagList("Program perumahan memakai dua indikator untuk menetapkan sasaran. ",
                            tags$b("Backlog 1"), " adalah rumah tangga yang tinggal di hunian bukan miliknya dan tidak memiliki rumah lain. ",
                            tags$b("Backlog 2"), " adalah rumah tangga yang menempati rumah milik sendiri, tetapi rumahnya belum memenuhi standar layak huni. Keduanya tidak tumpang tindih."),
                    viz("Seberapa besar masalah hunian secara nasional, dan apakah membaik dari 2025?",
                        uiOutput("cards"), insight = "ins_cards", src = "nasional", side = FALSE),
                    viz("Komponen rumah layak huni mana yang naik paling banyak, dan mana yang masih tertinggal?",
                        plotlyOutput("p_dumbbell", height = "340px"),
                        controls = if (isTRUE(DQ$has_2026_komp_prov)) dd("db_sel", "Wilayah", c("Nasional", sort(PROV$provinsi))) else NULL,
                        notes = list(Bentuk = "kotak putih = 2025, kotak ochre = 2026, angka hijau di kanan = perubahan dalam poin persen.",
                                     Sumbu = "tidak dimulai dari nol agar selisih kecil tetap terlihat."),
                        insight = "ins_dumbbell", src = "komp",
                        more = list(qa("Apa saja komponen rumah layak huni?",
                                       tags$p("Menurut BPS, rumah tangga memiliki akses rumah layak huni bila memenuhi keempat komponen sekaligus:"),
                                       tags$ol(tags$li("Ketahanan bangunan: atap, dinding, dan lantai berbahan layak."),
                                               tags$li("Kecukupan luas lantai: minimal 7,2 m² per orang."),
                                               tags$li("Akses air minum layak."),
                                               tags$li("Akses sanitasi layak."))))),
                    viz("Provinsi mana yang tertinggi dan terendah untuk tiap indikator?",
                        uiOutput("rank_ui"),
                        controls = tagList(dd("rk_ind", "Indikator", setNames(IND$id, IND$label), selected = "b1", width = "340px"),
                                           ctl("rk_scope", "Tampilkan", RANK_SCOPE, "top5", "scope")),
                        notes = list(Garis = "putus-putus = angka nasional.",
                                     Warna = "merah bata = lebih buruk dari angka nasional, hijau toska = lebih baik. Untuk cakupan penemuan TBC, nilai tinggi berarti lebih baik."),
                        insight = "ins_rank", src = "prov"),
                    viz("Provinsi mana yang membaik dan mana yang memburuk dari 2025 ke 2026?",
                        plotlyOutput("p_change", height = "780px"),
                        controls = ctl("c_b", "Jenis backlog", B_CHOICES, "b2", "backlog"),
                        notes = list(Titik = "putih = 2025, berwarna = 2026; angka di kanan = perubahan dalam poin persen.",
                                     Warna = "hijau toska = turun (membaik), merah bata = naik (memburuk).",
                                     Urutan = "dari penurunan terbesar ke kenaikan terbesar."),
                        insight = "ins_change", wide = TRUE, src = "prov")),
            
            chapter("sebaran", "Bab 2 · Sebaran Wilayah", "Persentase tertinggi tidak selalu berarti jumlah terbanyak",
                    "Rata-rata provinsi menyembunyikan variasi di dalamnya. Di tingkat kabupaten/kota terlihat di mana persentase backlog tinggi, di mana jumlahnya besar, dan apakah wilayah bermasalah saling berdekatan.",
                    if (geo_missing) tags$div(class = "notice", tags$b("Peta belum aktif. "),
                                              "Letakkan berkas batas wilayah di ", tags$code("data/Indonesia_KAB_KOTA.geojson"), ", lalu jalankan ulang aplikasi."),
                    viz("Di kabupaten/kota mana persentase backlog paling tinggi, dan di mana berubah?",
                        leafletOutput("map_choro", height = "580px"),
                        controls = tagList(ctl("m_b", "Jenis backlog", B_CHOICES, "b2", "backlog"),
                                           ctl("m_view", "Tampilan", c("Persentase 2025" = "2025", "Persentase 2026" = "2026", "Perubahan 2025\u21922026" = "chg"), "2026", "mode")),
                        notes = list(Warna = "5 kelas kuantil, makin tua makin tinggi (hijau toska untuk Backlog 1, ochre ke cokelat untuk Backlog 2). Pada tampilan perubahan: hijau toska = turun, merah bata = naik.",
                                     Rincian = "arahkan kursor (atau ketuk) ke wilayah untuk melihat persentase 2025 dan 2026, perubahannya, dan jumlah rumah tangga."),
                        insight = "ins_choro", src = "kab",
                        more = list(qa("Mengapa ada wilayah abu-abu?", uiOutput("na_choro")))),
                    viz("Di mana jumlah rumah tangga ber-backlog paling banyak?",
                        leafletOutput("map_prop", height = "580px"),
                        controls = tagList(ctl("p_b", "Jenis backlog", B_CHOICES, "b2", "backlog"),
                                           ctl("p_yr", "Tahun", YR_CHOICES, "2026", "year")),
                        notes = list(Ukuran = "luas lingkaran = jumlah rumah tangga ber-backlog.",
                                     Warna = "kelas persentase (rendah, sedang, tinggi; masing-masing sepertiga wilayah).",
                                     Label = "10 wilayah dengan jumlah terbesar; arahkan kursor ke lingkaran untuk angkanya."),
                        insight = "ins_prop", src = "kab",
                        more = list(qa("Mengapa sebagian wilayah tidak punya lingkaran?", uiOutput("na_prop")))),
                    viz("Apakah kabupaten/kota dengan backlog tinggi saling berdekatan?",
                        leafletOutput("map_lisa", height = "580px"),
                        controls = tagList(ctl("l_b", "Jenis backlog", B_CHOICES, "b2", "backlog"),
                                           ctl("l_yr", "Tahun", YR_CHOICES, "2026", "year")),
                        notes = list(Warna = "merah bata = hotspot (tinggi dikelilingi tinggi); hijau toska = coldspot (rendah dikelilingi rendah); oranye muda dan hijau muda = berbeda dari tetangganya; krem = tidak signifikan; abu-abu kebiruan = tanpa data.",
                                     Uji = sprintf("Local Moran’s I, %d tetangga terdekat, α = 5%%.", KNN_K)),
                        insight = "ins_lisa", src = "kab",
                        more = list(
                          qa("Apa yang diukur peta LISA?",
                             tags$p("Setiap kabupaten/kota dibandingkan dengan rata-rata ", KNN_K, " tetangga terdekatnya (Local Indicators of Spatial Association). ",
                                    "Peta menjawab apakah backlog tinggi atau rendah di suatu daerah juga terjadi di sekitarnya, atau daerah itu justru berbeda dari lingkungannya. ",
                                    "Moran’s I global merangkum seluruh peta dalam satu angka: mendekati 0 berarti acak, positif berarti wilayah bernilai mirip saling berdekatan.")),
                          qa("Mengapa hanya sebagian wilayah yang berwarna?",
                             tags$p("Warna hanya diberikan pada wilayah dengan pola yang signifikan secara statistik (p < 0,05). ",
                                    "Krem ", tags$b("bukan"), " berarti backlognya rendah, melainkan tidak ada bukti pola pengelompokan lokal.")),
                          qa("Apa batas tafsirnya?",
                             tags$p("Tinggi atau rendah dinilai relatif terhadap rata-rata seluruh kab/kota. Uji diulang untuk ratusan wilayah tanpa koreksi uji berganda, ",
                                    "sehingga sebagian kecil wilayah berwarna bisa muncul kebetulan. Tetangga ditentukan dari ", KNN_K,
                                    " titik terdekat, jadi pulau yang terpisah laut tetap memiliki tetangga.")),
                          qa("Mengapa ada wilayah abu-abu?", uiOutput("na_lisa"))))),
            
            chapter("dua-wajah", "Bab 3 · Dua Wajah Backlog", "Sulit memiliki rumah dan menempati rumah tak layak adalah dua masalah yang berbeda",
                    "DKI Jakarta paling berat dalam kepemilikan, Papua Pegunungan paling berat dalam kelayakhunian. Bab ini menguji apakah keduanya berjalan beriringan dan di mana bebannya menumpuk.",
                    viz("Apakah provinsi yang sulit memiliki rumah juga yang rumahnya tidak layak?",
                        plotlyOutput("p_quad", height = "620px"),
                        controls = ctl("q_yr", "Tahun", YR_CHOICES, "2026", "year"),
                        notes = list(Posisi = "persentase Backlog 1 (mendatar) dan Backlog 2 (tegak); garis putus-putus = angka nasional.",
                                     Ukuran = "jumlah rumah tangga ber-backlog (Backlog 1 + Backlog 2).",
                                     Warna = "pulau."),
                        insight = "ins_quad", wide = TRUE, src = "prov"),
                    viz("Wilayah mana yang besar populasinya sekaligus tinggi Backlog 2-nya?",
                        tagList(plotlyOutput("p_tree", height = "560px"), uiOutput("tree_leg")),
                        controls = ctl("t_yr", "Tahun", YR_CHOICES, "2026", "year"),
                        notes = list(Luas = "jumlah seluruh rumah tangga.",
                                     Warna = "% Backlog 2.",
                                     Interaksi = "klik kotak untuk masuk ke wilayahnya; klik jejak di atas grafik untuk kembali."),
                        insight = "ins_tree", src = "kab", side = FALSE,
                        more = list(qa("Mengapa memakai treemap dan icicle sekaligus?",
                                       tags$p("Keduanya menjawab pertanyaan berbeda. Treemap menimbang beban terhadap jumlah seluruh rumah tangga (ukuran = total RT, warna = % Backlog 2). ",
                                              "Icicle menyorot di mana jumlah rumah tangga ber-backlog menumpuk, berlapis dari pulau ke provinsi ke kab/kota (ukuran = jumlah Backlog 1 atau Backlog 2, warna = persentasenya).")))),
                    viz("Di mana jumlah rumah tangga ber-backlog terkumpul, dari pulau sampai kabupaten/kota?",
                        tagList(plotlyOutput("p_ice", height = "600px"), uiOutput("ice_leg")),
                        controls = tagList(ctl("i_b", "Jenis backlog", B_CHOICES, "b2", "backlog"),
                                           ctl("i_yr", "Tahun", YR_CHOICES, "2026", "year")),
                        notes = list(Arah = "dari atas ke bawah: pulau, provinsi, kab/kota.",
                                     Lebar = "jumlah rumah tangga backlog yang dipilih.",
                                     Warna = "persentase backlog yang sama (hijau toska untuk Backlog 1, ochre untuk Backlog 2).",
                                     Interaksi = "klik pulau atau provinsi untuk memperbesar; klik jejak di atas grafik untuk kembali."),
                        insight = "ins_ice", src = "kab", side = FALSE)),
            
            chapter("kesehatan", "Bab 4 · Hunian dan Kesehatan", "Antarprovinsi, hunian kurang layak tidak selalu berarti penyakit tercatat lebih banyak",
                    "Dua belas indikator hunian, kesehatan, dan kemiskinan tahun 2025 dibandingkan antarprovinsi. Sebagian besar hubungan yang signifikan justru berlawanan dengan dugaan, karena angka penyakit yang tercatat juga bergantung pada layanan kesehatan dan kepadatan kota. Bab ini membaca pola, bukan sebab-akibat.",
                    tags$div(class = "method-box",
                             tags$h3("Provinsi dikelompokkan menurut kemiripan 12 indikator"),
                             tags$p(class = "method-lead", "Empat grafik berikut memakai klaster yang sama. Warna klaster diurutkan dari paling baik (klaster 1) sampai paling tertinggal; ",
                                    "tingkat ketertinggalan diukur dari posisi rata-rata provinsi pada komponen utama pertama PCA, yang searah dengan kemiskinan dan defisit hunian dasar."),
                             uiOutput("cl_cards"),
                             more_box(list(
                               qa("Indikator apa saja yang dipakai?",
                                  tags$p("Empat komponen rumah layak (ketahanan bangunan, luas lantai, air minum, sanitasi), BABS, listrik non-PLN, atap asbes, keluhan kesehatan, diare, cakupan penemuan kasus TBC, DBD, dan kemiskinan; semuanya tahun 2025.")),
                               qa("Bagaimana provinsi dikelompokkan?",
                                  tags$p("Variabel yang menceng (BABS, non-PLN, asbes, diare, DBD) ditransformasi log(1 + x), lalu semua variabel diubah menjadi skor-z dan dipangkas pada ±2 simpangan baku agar satu provinsi ekstrem tidak membentuk klaster sendiri. ",
                                         "Provinsi lalu dikelompokkan dengan klaster hierarkis metode Ward (jarak Euclid). Jumlah klaster bisa diubah 3 sampai 5 lewat pilihan di bar yang menempel di atas grafik.")),
                               qa("Bagaimana sel kosong ditangani?",
                                  tags$p("DBD kosong untuk ", join_id(PROV$provinsi[is.na(PROV$dbd_100k)]), " dan diisi median hanya untuk klaster dan PCA. ",
                                         "Atap asbes dan BABS yang tidak disajikan BPS dibaca 0, sedangkan listrik non-PLN yang tidak disajikan diisi sisa 100 − PLN − bukan listrik. Semua sel itu ditandai × di heatmap berkelompok."))))),
                    tags$div(class = "cl-scope",
                             tags$div(class = "cl-bar", role = "region", `aria-label` = "Pengaturan klaster",
                                      ctl("k", "Jumlah klaster", c("3", "4", "5"), "4", "k"),
                                      uiOutput("cl_legend", class = "cl-legend"),
                                      actionButton("btn_reset", "Hapus sorotan", class = "btn-reset")),
                             viz("Apakah provinsi dengan akses rumah layak lebih rendah memiliki masalah kesehatan lebih tinggi?",
                                 plotlyOutput("p_bubble", height = "540px"),
                                 controls = tagList(dd("b_y", "Indikator kesehatan (sumbu tegak)", HEALTH_CHOICES, width = "260px"),
                                                    dd("b_isl", "Sorot pulau", ISLAND_CHOICES, "ALL", width = "220px")),
                                 notes = list(Ukuran = "jumlah penduduk.", Warna = "klaster.",
                                              Garis = "putus-putus = tren linear seluruh provinsi; memilih pulau tidak mengubah garis ini."),
                                 insight = "ins_bubble", src = "kes",
                                 more = list(qa("Mengapa korelasi antarprovinsi tidak membuktikan sebab-akibat?",
                                                tags$p("Setiap titik adalah rata-rata satu provinsi. Hubungan antarrata-rata wilayah belum tentu berlaku untuk rumah tangga di dalamnya (ecological fallacy), ",
                                                       "dan provinsi berbeda dalam banyak hal sekaligus: kemiskinan, kepadatan, iklim, serta kemampuan layanan kesehatan menemukan dan mencatat kasus.")))),
                             viz("Indikator hunian mana yang paling erat berkaitan dengan indikator kesehatan?",
                                 plotlyOutput("p_corr", height = "520px"),
                                 notes = list(Angka = "korelasi Spearman ρ antarprovinsi dari data asli (n = 38; DBD n = 36).",
                                              Warna = "merah bata = naik bersama, biru = berlawanan arah.",
                                              Bintang = "signifikan pada p < 0,05.",
                                              Kemiskinan = "kolom pembanding, bukan indikator kesehatan."),
                                 insight = "ins_corr", wide = TRUE, src = "kes",
                                 more = list(qa("Apa itu korelasi Spearman?",
                                                tags$p("Korelasi Spearman mengukur apakah dua indikator naik-turun searah berdasarkan peringkat, bukan nilai mentah. Nilainya dari −1 sampai 1: ",
                                                       "mendekati 1 berarti provinsi yang tinggi di satu indikator juga tinggi di indikator lain, mendekati −1 berarti berkebalikan. Spearman dipakai karena jumlah provinsi sedikit dan sebarannya tidak normal.")),
                                             qa("Apa arti “searah dugaan”?",
                                                tags$p("Searah dugaan berarti hunian yang lebih buruk berpasangan dengan kesehatan yang lebih buruk, misalnya sanitasi layak lebih rendah bersama diare lebih tinggi. ",
                                                       "Untuk cakupan penemuan TBC, nilai tinggi berarti lebih baik, jadi arah dugaannya dibalik.")))),
                             viz("Provinsi mana yang berprofil mirip, dan defisit mana yang muncul bersamaan?",
                                 tagList(plotlyOutput("p_heat", height = "800px"), uiOutput("heat_leg"),
                                         tags$h4(class = "sub", "Profil klaster (rata-rata nilai asli)"), uiOutput("tbl_cl")),
                                 notes = list(Warna = "merah bata = lebih buruk dari rata-rata provinsi, hijau toska = lebih baik (skor-z).",
                                              Kolom = "indikator “tinggi = baik” dibalik dan diberi nama defisit, misalnya “Sanitasi tak layak”.",
                                              Tanda = "× = kosong di sumber dan diisi untuk analisis.",
                                              Pita = "klaster dan pulau; arahkan kursor ke sel untuk nilai asli."),
                                 insight = "ins_cluster", wide = TRUE, src = "kes", side = FALSE,
                                 more = list(qa("Apa itu skor-z?",
                                                tags$p("Skor-z menyatakan seberapa jauh nilai suatu provinsi dari rata-rata 38 provinsi, dalam satuan simpangan baku. ",
                                                       "Skor 0 berarti sama dengan rata-rata, +1 berarti satu simpangan baku lebih buruk. Dengan skor-z, indikator bersatuan berbeda (persen, per 100.000) bisa dibandingkan dalam satu warna.")))),
                             tags$div(class = "viz-pair",
                                      viz("Indikator apa yang membentuk dua sumbu biplot?",
                                          plotlyOutput("p_load", height = "460px"),
                                          notes = list(Batang = "muatan (loading). Ke kanan = indikator naik saat provinsi bergeser ke kanan (PC1, slate) atau ke atas (PC2, ochre) pada biplot."),
                                          insight = "ins_load", src = "kes", side = FALSE),
                                      viz("Ke mana provinsi menyebar pada dua sumbu utama, dan mana yang menyimpang?",
                                          plotlyOutput("p_pca", height = "460px"),
                                          notes = list(Panah = "arah naiknya tiap indikator; panah searah = naik bersama.",
                                                       Label = "5 provinsi terjauh dari pusat.",
                                                       Interaksi = "seret kotak di grafik untuk menyorot provinsi yang sama di diagram gelembung."),
                                          insight = "ins_pca", src = "kes", side = FALSE,
                                          more = list(qa("Apa itu PCA?",
                                                         tags$p("Principal Component Analysis merangkum 12 indikator menjadi beberapa sumbu baru (komponen utama) yang menyimpan variasi terbanyak. ",
                                                                "Biplot menampilkan dua sumbu pertama: provinsi yang berdekatan berprofil mirip, dan panah menunjukkan indikator yang membentuk tiap arah."))))))),
            
            chapter("intervensi", "Bab 5 · Intervensi", "Program pemerintah baru menjangkau sebagian kecil kebutuhan",
                    "Hanya program yang capaiannya tercatat pada periode setara yang dibandingkan antartahun: Semester I 2025 dengan Semester I 2026. Program lain dijelaskan di bawah grafik.",
                    viz("Program mana yang capaiannya naik dan turun dibanding periode yang sama tahun lalu?",
                        tagList(cap_rows(), cap_scale()),
                        notes = list(Batang = "abu-abu = Semester I 2025, ochre = Semester I 2026.",
                                     Skala = "panjang batang dihitung per program, karena besaran program berbeda jauh (ratusan sampai ratusan ribu unit).",
                                     Bilah = "di bagian bawah, lebar penuh = seluruh backlog 2026."),
                        insight = "ins_cap", src = "cap",
                        more = list(qa("Mengapa program lain tidak dibandingkan?", uiOutput("cap_rules"))))),
            
            chapter("kesimpulan", "Bab 6 · Kesimpulan", "Apa yang ditunjukkan seluruh visualisasi",
                    NULL,
                    tags$div(class = "conclusion", uiOutput("kesimpulan"))),
            
            tags$section(id = "unduh", class = "chapter downloads",
                         tags$div(class = "chapter-head",
                                  tags$span(class = "kicker", "Data"),
                                  tags$h2("Unduh data yang dipakai laporan ini"),
                                  tags$p(class = "lead", "Nilai perumahan sama dengan tabel BPS, Statistik Perumahan 2026; data kesehatan dari BPS dan Profil Kesehatan Indonesia 2025. Sumber tiap kolom tercantum di sheet Keterangan pada tiap berkas.")),
                         tags$div(class = "dl-list",
                                  dl_row("dl_prov", "dataset(provinsi).xlsx", sprintf("%d provinsi dan angka nasional: backlog, komponen rumah layak 2025 dan 2026, indikator kesehatan, kemiskinan.", nrow(PROV))),
                                  dl_row("dl_kab", "backlog_kabkota.xlsx", sprintf("%d kabupaten/kota: persentase dan jumlah Backlog 1 dan Backlog 2, 2025 dan 2026.", nrow(KAB))),
                                  dl_row("dl_cap", "capaian_pemerintah.xlsx", sprintf("%d program perumahan pemerintah per semester, 2025 sampai Semester I 2026.", nrow(CAP))))))
  ,
  tags$footer(class = "site-footer",
              tags$div(class = "foot-inner",
                       tags$p(class = "foot-id",
                              tags$b(AUTHOR$judul), tags$span(class = "sep", "·"), AUTHOR$nama,
                              tags$span(class = "sep", "·"), paste("NIM", AUTHOR$nim),
                              tags$span(class = "sep", "·"), paste("Kelas", AUTHOR$kelas),
                              tags$span(class = "sep", "·"), tags$a(href = paste0("mailto:", AUTHOR$email), AUTHOR$email)),
                       tags$p(class = "foot-course", AUTHOR$mk, tags$span(class = "sep", "·"), AUTHOR$kampus,
                              tags$span(class = "sep", "·"), AUTHOR$tanggal),
                       tags$p(class = "foot-src", "Data: BPS, Statistik Perumahan 2026 dan Susenas Maret 2025–2026; Kementerian PKP. Dibuat dengan R Shiny, plotly, dan leaflet.")))
)

server <- function(input, output, session) {
  
  need_geo <- function() shiny::validate(shiny::need(!is.null(GEO), "berkas batas wilayah belum tersedia (lihat README)."))
  pfmt <- function(p) ifelse(p < 0.001, "< 0,001", paste("=", fmt_p(p)))
  arah_txt <- function(d) if (!is.finite(d) || d == 0) "tetap" else if (d < 0) "turun" else "naik"
  cap1 <- function(x) paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
  compact <- function(x) Filter(function(e) !is.null(e) && !(is.character(e) && length(e) == 1 && !nzchar(e)), x)
  mx_ok <- function(x) which.max(replace(x, !is.finite(x), -Inf))
  # Pulau dengan proporsi kab/kota terbesar yang memenuhi syarat (minimal 5 kab/kota agar Bali tidak menang karena kecil).
  pulau_share <- function(pulau, hit) {
    n <- table(pulau); k <- table(factor(pulau[hit], levels = names(n)))
    ok <- n >= 5 & k > 0
    if (!any(ok)) return(NULL)
    pct <- as.numeric(k[ok]) / as.numeric(n[ok]) * 100; i <- which.max(pct)
    list(pulau = tc(names(n)[ok][i]), k = as.integer(k[ok][i]), n = as.integer(n[ok][i]), pct = pct[i])
  }
  
  # Interpretasi selalu tiga bagian: isi grafik, yang menonjol (angka dari data), kesimpulan.
  ins <- function(isi, sorot, simpul) {
    items <- compact(if (is.character(sorot)) as.list(sorot) else sorot)
    tagList(
      tags$div(class = "ins-sec", tags$h5("Isi grafik"), tags$p(isi)),
      if (length(items)) tags$div(class = "ins-sec", tags$h5("Yang menonjol"), tags$ul(lapply(items, tags$li))),
      tags$div(class = "ins-sec ins-end", tags$h5("Kesimpulan"), tags$p(simpul)))
  }
  
  # Bab 1
  output$cards <- renderUI({
    tags$div(class = "stat-row",
             stat_card("Backlog 1 · Kepemilikan", NAS$b1_pct_2026, NAS$b1_pct_2025,
                       sprintf("%s juta rumah tangga", fmt_num(NAS$b1_n_2026 / 1000, 2)), fill = "b1"),
             stat_card("Backlog 2 · Kelayakhunian", NAS$b2_pct_2026, NAS$b2_pct_2025,
                       sprintf("%s juta rumah tangga", fmt_num(NAS$b2_n_2026 / 1000, 2)), fill = "b2"),
             stat_card("Akses rumah layak huni", NAS$akses_layak_2026, NAS$rumah_layak_huni,
                       "memenuhi keempat komponen BPS", good_when = "up", fill = "akses"))
  })
  output$ins_cards <- renderUI({
    P <- PROV
    top <- function(col, f = which.max) P[f(P[[col]]), ]
    b1hi <- top("b1_pct_2026"); b1lo <- top("b1_pct_2026", which.min); b1n <- top("b1_n_2026")
    b2hi <- top("b2_pct_2026"); b2lo <- top("b2_pct_2026", which.min); b2n <- top("b2_n_2026")
    d1 <- NAS$b1_pct_2026 - NAS$b1_pct_2025; d2 <- NAS$b2_pct_2026 - NAS$b2_pct_2025
    da <- NAS$akses_layak_2026 - NAS$rumah_layak_huni
    g1 <- b1hi$b1_pct_2026 - b1lo$b1_pct_2026; g2 <- b2hi$b2_pct_2026 - b2lo$b2_pct_2026
    jumlah <- if (identical(b1n$provinsi, b2n$provinsi))
      sprintf("Dari sisi jumlah, %s menanggung beban terbesar untuk keduanya: %s ribu rumah tangga Backlog 1 dan %s ribu rumah tangga Backlog 2.",
              b1n$provinsi, fmt_num(b1n$b1_n_2026, 1), fmt_num(b2n$b2_n_2026, 1))
    else sprintf("Dari sisi jumlah, Backlog 1 terbesar ada di %s (%s ribu rumah tangga) dan Backlog 2 terbesar di %s (%s ribu rumah tangga).",
                 b1n$provinsi, fmt_num(b1n$b1_n_2026, 1), b2n$provinsi, fmt_num(b2n$b2_n_2026, 1))
    ins(
      isi = "Tiga kartu merangkum kondisi nasional 2026: persentase Backlog 1, persentase Backlog 2, dan persentase rumah tangga dengan akses rumah layak huni. Angka di bawah tiap kartu membandingkannya dengan 2025; segitiga menunjukkan arah perubahan dan warnanya menunjukkan apakah perubahan itu membaik (hijau) atau memburuk (merah).",
      sorot = list(
        sprintf("Backlog 1 %s %s poin menjadi %s%% (%s juta rumah tangga), dan Backlog 2 %s %s poin menjadi %s%% (%s juta rumah tangga).",
                arah_txt(d1), fmt_num(abs(d1)), fmt_num(NAS$b1_pct_2026), fmt_num(NAS$b1_n_2026 / 1000, 2),
                arah_txt(d2), fmt_num(abs(d2)), fmt_num(NAS$b2_pct_2026), fmt_num(NAS$b2_n_2026 / 1000, 2)),
        sprintf("Akses rumah layak huni %s %s poin menjadi %s%%, jadi sekitar %d dari 10 rumah tangga masih belum memenuhi keempat komponen.",
                arah_txt(da), fmt_num(abs(da)), fmt_num(NAS$akses_layak_2026), round((100 - NAS$akses_layak_2026) / 10)),
        sprintf("Antarprovinsi, Backlog 1 berkisar dari %s%% (%s) sampai %s%% (%s), dan Backlog 2 dari %s%% (%s) sampai %s%% (%s).",
                fmt_num(b1lo$b1_pct_2026), b1lo$provinsi, fmt_num(b1hi$b1_pct_2026), b1hi$provinsi,
                fmt_num(b2lo$b2_pct_2026), b2lo$provinsi, fmt_num(b2hi$b2_pct_2026), b2hi$provinsi),
        jumlah),
      simpul = sprintf("Arah nasional membaik, tetapi perubahannya hanya %s dan %s poin, kecil dibanding jarak antarprovinsi yang mencapai %s poin untuk Backlog 1 dan %s poin untuk Backlog 2. Persoalan utamanya ada pada ketimpangan antarwilayah, yang ditelusuri di bab berikutnya.",
                       fmt_num(abs(d1)), fmt_num(abs(d2)), fmt_num(g1, 1), fmt_num(g2, 1)))
  })
  
  db_sel <- reactive(if (is.null(input$db_sel)) "Nasional" else input$db_sel)
  output$p_dumbbell <- renderPlotly(p_dumbbell(db_sel()))
  output$ins_dumbbell <- renderUI({
    sel <- db_sel(); d <- komp_data(sel); d$chg <- d$b - d$a
    nat <- komp_data("Nasional")
    up <- sum(d$chg > 0, na.rm = TRUE)
    mx <- d[which.max(d$chg), ]; mn <- d[which.min(d$chg), ]; low <- d[which.min(d$b), ]
    row <- if (sel == "Nasional") NAS else PROV[PROV$provinsi == sel, ]
    a25 <- row$rumah_layak_huni[1]; a26 <- row$akses_layak_2026[1]
    lbl <- if (sel == "Nasional") "Indonesia" else sel
    low_nat <- nat$b[nat$id == low$id]
    ins(
      isi = sprintf("Empat baris adalah komponen rumah layak huni menurut BPS untuk %s: ketahanan bangunan, kecukupan luas lantai, akses air minum layak, dan akses sanitasi layak. Kotak putih menunjukkan 2025, kotak ochre 2026, dan angka di kanan adalah perubahannya dalam poin persen.", lbl),
      sorot = list(
        sprintf("%s. Kenaikan terbesar pada %s (%s poin, menjadi %s%%), perubahan terkecil pada %s (%s poin).",
                if (up == 4) "Keempat komponen meningkat" else sprintf("%d dari empat komponen meningkat", up),
                tolower(mx$komp), fmt_pp(mx$chg), fmt_num(mx$b), tolower(mn$komp), fmt_pp(mn$chg)),
        sprintf("Komponen terendah pada 2026 adalah %s (%s%%).%s", tolower(low$komp), fmt_num(low$b),
                if (sel != "Nasional") sprintf(" Angka nasionalnya %s%%, jadi %s berada %s poin %s angka nasional.",
                                               fmt_num(low_nat), sel, fmt_num(abs(low$b - low_nat)), if (low$b < low_nat) "di bawah" else "di atas") else ""),
        if (is.finite(a25) && is.finite(a26))
          sprintf("Akses rumah layak huni %s %s dari %s%% (2025) menjadi %s%% (2026).", lbl, arah_txt(a26 - a25), fmt_num(a25), fmt_num(a26))),
      simpul = sprintf("Rumah tangga baru tergolong layak huni bila memenuhi keempat komponen sekaligus, sehingga akses rumah layak huni (%s%%) jauh di bawah capaian komponen mana pun. %s menjadi penghambat utama: selama komponen ini tertinggal, kenaikan komponen lain tidak banyak menambah rumah tangga yang tergolong layak huni.",
                       fmt_num(a26), cap1(tolower(low$komp))))
  })
  
  output$rank_ui <- renderUI({
    h <- if (input$rk_scope == "all") "940px" else "330px"
    plotlyOutput("p_rank", height = h)
  })
  output$p_rank <- renderPlotly(p_rank(input$rk_ind, input$rk_scope))
  output$ins_rank <- renderUI({
    r <- rank_data(input$rk_ind, input$rk_scope); m <- r$m; dg <- m$digits; nat <- r$nat
    full <- rank_data(input$rk_ind, "all")$d
    lab <- sub(" \\(.*\\)$", "", m$label)
    pct <- grepl("%", m$label, fixed = TRUE); suf <- if (pct) "%" else ""; un <- if (pct) " poin" else ""
    lst <- function(x) join_id(sprintf("%s (%s%s)", x$prov, fmt_num(x$v, m$digits), suf))
    lo3 <- head(full[order(full$v), ], 3)
    worst <- if (isTRUE(m$good_high)) head(full[order(full$v), ], 5) else head(full, 5)
    pc <- sort(table(worst$pulau), decreasing = TRUE)
    gap <- max(full$v) - min(full$v)
    ratio <- if (min(full$v) > 0) max(full$v) / min(full$v) else NA_real_
    cakupan <- switch(input$rk_scope, top5 = "lima provinsi dengan nilai tertinggi", bot5 = "lima provinsi dengan nilai terendah", "seluruh provinsi")
    ins(
      isi = sprintf("Batang menunjukkan %s untuk %s, data tahun %s. Garis putus-putus adalah angka nasional (%s%s). Merah bata menandai kondisi lebih buruk dari angka nasional dan hijau toska lebih baik%s.",
                    tolower(lab), cakupan, m$yr, fmt_num(nat, dg), suf,
                    if (isTRUE(m$good_high)) "; untuk indikator ini nilai tinggi berarti lebih baik" else ""),
      sorot = list(
        sprintf("Tertinggi: %s.", lst(head(full, 3))),
        sprintf("Terendah: %s.", lst(lo3)),
        sprintf("%d dari %d provinsi berada di atas angka nasional.", sum(full$v > nat), nrow(full)),
        if (pc[[1]] >= 3) sprintf("%d dari lima provinsi dengan kondisi terburuk berada di %s.", as.integer(pc[[1]]), names(pc)[1])),
      simpul = paste0(
        sprintf("Jarak provinsi tertinggi dan terendah mencapai %s%s", fmt_num(gap, dg), un),
        if (is.finite(ratio) && ratio >= 2) sprintf(", atau %s kali lipat. ", fmt_num(ratio, 1)) else ". ",
        if (pc[[1]] >= 3) sprintf("Kondisi terburuk mengumpul di %s, sehingga program untuk indikator ini lebih tepat disasarkan per kawasan. ", names(pc)[1])
        else "Kondisi terburuk tersebar di beberapa pulau, sehingga sasaran program perlu ditetapkan per provinsi. ",
        if (isTRUE(m$health) && !isTRUE(m$good_high)) "Angka penyakit juga mencerminkan kemampuan pelaporan, jadi provinsi dengan nilai rendah belum tentu lebih sehat." else ""))
  })
  
  output$p_change <- renderPlotly(p_change(input$c_b))
  output$ins_change <- renderUI({
    b <- input$c_b; B <- b_name(b); d <- change_data(b)
    nat <- NAS[[paste0(b, "_pct_2026")]] - NAS[[paste0(b, "_pct_2025")]]
    dn <- d[d$chg < 0, ]; up <- d[d$chg > 0, ]; up <- up[order(-up$chg), ]
    lst <- function(x) join_id(sprintf("%s (%s poin)", x$provinsi, fmt_pp(x$chg, 2)))
    ins(
      isi = sprintf("Setiap baris adalah satu provinsi. Titik putih menunjukkan persentase %s tahun 2025, titik berwarna tahun 2026, dan garis di antaranya besarnya perubahan. Provinsi diurutkan dari penurunan terbesar ke kenaikan terbesar.", B),
      sorot = list(
        sprintf("Secara nasional %s %s %s poin, dari %s%% menjadi %s%%.", B, arah_txt(nat), fmt_num(abs(nat), 2),
                fmt_num(NAS[[paste0(b, "_pct_2025")]]), fmt_num(NAS[[paste0(b, "_pct_2026")]])),
        sprintf("Dari %d provinsi, %d turun dan %d naik.", nrow(d), nrow(dn), nrow(up)),
        if (nrow(dn)) sprintf("Penurunan terbesar: %s.", lst(head(dn, 3))),
        if (nrow(up)) sprintf("Kenaikan: %s.", lst(head(up, 3)))),
      simpul = if (!nrow(up))
        sprintf("Perbaikan %s terjadi di seluruh provinsi, tetapi besarnya beragam, dari %s sampai %s poin. Provinsi yang turun paling sedikit perlu dilihat lebih dekat karena hampir tidak bergerak.",
                B, fmt_pp(max(d$chg), 2), fmt_pp(min(d$chg), 2))
      else sprintf("Sebagian besar provinsi membaik, tetapi %s bergerak berlawanan dengan arah nasional. Provinsi ini perlu dipantau pada pendataan berikutnya, termasuk memeriksa apakah kenaikannya melebihi galat sampling.",
                   join_id(head(up$provinsi, 3))))
  })
  
  # Bab 2: peta
  output$map_choro <- renderLeaflet({ need_geo(); draw_choro(input$m_b, input$m_view %||% "2026") })
  output$map_prop  <- renderLeaflet({ need_geo(); draw_prop(input$p_b, input$p_yr) })
  output$map_lisa  <- renderLeaflet({ need_geo(); draw_lisa(input$l_b, input$l_yr) })
  
  output$ins_choro <- renderUI({
    b <- input$m_b; B <- b_name(b)
    view <- input$m_view %||% "2026"
    if (view == "chg") {
      ch <- KAB[[paste0("chg_", b)]]; ok <- is.finite(ch); d <- KAB[ok, ]; ch <- ch[ok]
      pu <- pulau_share(d$pulau, ch > 0)
      ins(
        isi = sprintf("Peta menunjukkan perubahan persentase %s dari 2025 ke 2026 di %d kabupaten/kota. Hijau toska berarti turun (membaik), merah bata berarti naik (memburuk), dan warna pucat berarti hampir tidak berubah.", B, nrow(d)),
        sorot = list(
          sprintf("%s turun di %d kab/kota dan naik di %d.", B, sum(ch < 0), sum(ch > 0)),
          sprintf("Penurunan terbesar di %s (%s poin); kenaikan terbesar di %s (%s poin).",
                  d$label[which.min(ch)], fmt_pp(min(ch)), d$label[which.max(ch)], fmt_pp(max(ch))),
          sprintf("Median perubahan: %s poin.", fmt_pp(stats::median(ch))),
          if (!is.null(pu)) sprintf("Proporsi kab/kota yang memburuk paling besar di %s: %d dari %d wilayah (%s%%).", pu$pulau, pu$k, pu$n, fmt_num(pu$pct, 0))),
        simpul = sprintf("%s%% kab/kota membaik. Wilayah yang memburuk perlu diperiksa satu per satu, karena perubahan kecil di tingkat kab/kota bisa masih berada dalam galat sampling Susenas.",
                         fmt_num(mean(ch < 0) * 100, 0)))
    } else {
      yr <- view; v <- KAB[[paste0(b, "_pct_", yr)]]; ok <- is.finite(v); d <- KAB[ok, ]; v <- v[ok]
      nat <- NAS[[paste0(b, "_pct_", yr)]]; o <- order(-v)
      q <- stats::quantile(v, c(0.2, 0.8))
      hi <- pulau_share(d$pulau, v >= q[2]); lo <- pulau_share(d$pulau, v <= q[1])
      lst <- function(i) join_id(sprintf("%s (%s%%)", d$label[i], fmt_num(v[i])))
      ins(
        isi = sprintf("Peta mewarnai %d kabupaten/kota menurut persentase %s tahun %s. Warna dibagi menjadi lima kelas kuantil yang masing-masing berisi sekitar seperlima wilayah; makin tua warnanya, makin tinggi persentasenya.", nrow(d), B, yr),
        sorot = list(
          sprintf("Tertinggi: %s.", lst(head(o, 3))),
          sprintf("Terendah: %s.", lst(rev(tail(o, 3)))),
          sprintf("%d dari %d kab/kota berada di atas angka nasional (%s%%).", sum(v > nat), nrow(d), fmt_num(nat)),
          if (!is.null(hi)) sprintf("%d dari %d kab/kota di %s (%s%%) masuk kelas tertinggi, proporsi terbesar di antara pulau.", hi$k, hi$n, hi$pulau, fmt_num(hi$pct, 0))),
        simpul = sprintf("%s tinggi paling terkonsentrasi di %s, sedangkan kelas terendah paling banyak ditemukan di %s. Pola yang berkelompok seperti ini lebih tepat ditangani per kawasan; peta LISA di bawah menguji apakah pengelompokannya signifikan.",
                         B, hi$pulau %||% "beberapa pulau", lo$pulau %||% "beberapa pulau"))
    }
  })
  
  output$ins_prop <- renderUI({
    b <- input$p_b; yr <- input$p_yr; B <- b_name(b)
    d <- KAB[is.finite(KAB[[paste0(b, "_n_", yr)]]) & is.finite(KAB[[paste0(b, "_pct_", yr)]]), ]
    n <- d[[paste0(b, "_n_", yr)]]; p <- d[[paste0(b, "_pct_", yr)]]
    rp <- rank(-p, ties.method = "min"); in_ <- which.max(n); ip <- which.max(p)
    top_n <- order(-n)[1:5]
    share10 <- sum(sort(n, decreasing = TRUE)[1:10]) / sum(n) * 100
    mr <- round(mean(rp[top_n]))
    ins(
      isi = sprintf("Setiap lingkaran adalah satu kab/kota. Luas lingkaran sebanding dengan jumlah rumah tangga %s tahun %s, dan warnanya menunjukkan kelas persentase: rendah, sedang, atau tinggi, masing-masing sepertiga wilayah. Sepuluh wilayah dengan jumlah terbesar diberi label.", B, yr),
      sorot = list(
        sprintf("Jumlah terbesar ada di %s (%s ribu rumah tangga), tetapi persentasenya %s%% dan hanya peringkat ke-%d dari %d.",
                d$label[in_], fmt_num(n[in_], 1), fmt_num(p[in_]), rp[in_], nrow(d)),
        sprintf("Persentase tertinggi ada di %s (%s%%) dengan jumlah %s ribu rumah tangga.", d$label[ip], fmt_num(p[ip]), fmt_num(n[ip], 1)),
        sprintf("Lima wilayah dengan jumlah terbesar: %s.", join_id(d$label[top_n])),
        sprintf("Sepuluh wilayah terbesar menampung %s%% dari seluruh rumah tangga %s yang terpetakan.", fmt_num(share10, 1), B)),
      simpul = if (mr > nrow(d) / 4)
        sprintf("Rata-rata peringkat persentase lima wilayah berjumlah terbesar adalah ke-%d. Prioritas menurut jumlah dan menurut persentase menghasilkan daftar wilayah yang berbeda, sehingga keduanya perlu dipakai bersamaan: jumlah untuk besaran anggaran, persentase untuk kedalaman masalah.", mr)
      else sprintf("Rata-rata peringkat persentase lima wilayah berjumlah terbesar adalah ke-%d, jadi wilayah berjumlah besar juga cenderung berpersentase tinggi. Untuk %s tahun %s, prioritas menurut jumlah dan menurut persentase sebagian besar sejalan.", mr, B, yr))
  })
  
  lisa_top <- function(r, lv, n = 3) {
    kd <- r$data$kode_kabkota[r$data$kategori == lv]
    hp <- sort(table(KAB$provinsi[match(kd, KAB$kode_kabkota)]), decreasing = TRUE)
    if (!length(hp)) return(NULL)
    join_id(sprintf("%s (%d)", names(head(hp, n)), as.integer(head(hp, n))))
  }
  output$ins_lisa <- renderUI({
    r <- LISA[[paste(input$l_b, input$l_yr, sep = "_")]]
    req(r)
    B <- b_name(input$l_b)
    tb  <- table(factor(r$data$kategori, levels = LISA_LEVELS))
    sig <- sum(tb[1:4]); pos <- r$I > 0 && r$p < 0.05
    hot <- lisa_top(r, LISA_LEVELS[1]); cold <- lisa_top(r, LISA_LEVELS[2])
    ins(
      isi = sprintf("Peta membandingkan persentase %s tahun %s di setiap kab/kota dengan rata-rata %d tetangga terdekatnya. Hanya wilayah dengan pola signifikan (p < 0,05) yang diberi warna: merah bata untuk hotspot, hijau toska untuk coldspot, dan warna muda untuk wilayah yang berbeda dari tetangganya.",
                    B, input$l_yr, KNN_K),
      sorot = list(
        sprintf("Moran’s I = %s (p %s, n = %d). Bila polanya acak, nilai yang diharapkan sekitar %s.", fmt_num(r$I, 3), pfmt(r$p), r$n, fmt_num(-1 / (r$n - 1), 3)),
        sprintf("%d kab/kota (%s%%) berwarna: %d hotspot, %d coldspot, %d tinggi di antara rendah, dan %d rendah di antara tinggi. Sisanya, %d wilayah, tidak signifikan.",
                sig, fmt_num(sig / r$n * 100, 1), as.integer(tb[[1]]), as.integer(tb[[2]]), as.integer(tb[[3]]), as.integer(tb[[4]]), as.integer(tb[[5]])),
        if (!is.null(hot)) sprintf("Hotspot paling banyak di %s.", hot),
        if (!is.null(cold)) sprintf("Coldspot paling banyak di %s.", cold)),
      simpul = if (pos)
        sprintf("%s tahun %s mengelompok secara spasial: wilayah dengan backlog tinggi cenderung bertetangga dengan wilayah yang juga tinggi. Hotspot menandai kawasan lintas kabupaten yang lebih tepat ditangani sebagai satu wilayah. Karena uji lokal tidak dikoreksi untuk uji berganda, anggap peta ini sebagai indikasi awal.", B, input$l_yr)
      else sprintf("%s tahun %s tidak menunjukkan pengelompokan spasial yang signifikan, jadi wilayah bermasalah tersebar dan perlu disasar satu per satu.", B, input$l_yr))
  })
  
  # Catatan wilayah abu-abu. Sumber tidak mencantumkan alasan per wilayah, jadi alasan tidak ditebak.
  na_box <- function(miss, what, extra = NULL) {
    n <- length(miss)
    n_unmatched <- if (is.null(GEO)) 0L else sum(is.na(GEO$kode_kabkota))
    isi <- if (n == 0) sprintf("Semua kab/kota memiliki nilai %s. ", what)
    else sprintf("%d kab/kota tidak memiliki nilai %s (tertulis NA di tabel BPS): %s. BPS tidak menyajikan angka kab/kota bila sampel Susenas terlalu kecil untuk menghasilkan estimasi yang andal. Aplikasi tidak mengisi atau memperkirakan nilai ini. ",
                 n, what, join_id(head(miss, 25)))
    tags$p(isi,
           if (n_unmatched > 0) sprintf("Selain itu, %d poligon batas wilayah tidak berpasangan dengan baris data karena perbedaan nama atau pembagian wilayah. ", n_unmatched),
           extra)
  }
  output$na_choro <- renderUI({
    req(input$m_b, input$m_view)
    b <- input$m_b; B <- b_name(b); yr <- input$m_view
    if (yr == "chg") { v <- KAB[[paste0("chg_", b)]]; what <- sprintf("perubahan %s 2025→2026 (salah satu tahun kosong)", B) }
    else { v <- KAB[[paste0(b, "_pct_", yr)]]; what <- sprintf("persentase %s %s", B, yr) }
    na_box(KAB$label[is.na(v)], what)
  })
  output$na_prop <- renderUI({
    req(input$p_b, input$p_yr)
    b <- input$p_b; yr <- input$p_yr; B <- b_name(b)
    ok <- is.finite(KAB[[paste0(b, "_pct_", yr)]]) & is.finite(KAB[[paste0(b, "_n_", yr)]])
    na_box(KAB$label[!ok], sprintf("jumlah atau persentase %s %s", B, yr), "Wilayah tanpa lingkaran berarti tidak ada data untuk digambar.")
  })
  output$na_lisa <- renderUI({
    req(input$l_b, input$l_yr)
    b <- input$l_b; yr <- input$l_yr; B <- b_name(b)
    v <- KAB[[paste0(b, "_pct_", yr)]]
    na_box(KAB$label[is.na(v)], sprintf("persentase %s %s", B, yr),
           "Wilayah tanpa data tidak diikutkan dalam perhitungan LISA, sehingga tetangga hanya dipilih dari wilayah yang memiliki nilai.")
  })
  
  # Bab 3
  output$p_quad <- renderPlotly(p_quadrant(input$q_yr))
  output$ins_quad <- renderUI({
    yr <- input$q_yr; d <- quad_data(yr); d <- d[stats::complete.cases(d$x, d$y, d$bl), ]
    nx <- NAS[[paste0("b1_pct_", yr)]]; ny <- NAS[[paste0("b2_pct_", yr)]]
    d$kuad <- quad_class(d, nx, ny); s <- sp_test(d$x, d$y)
    tb <- table(factor(d$kuad, levels = QUADS))
    xhi <- d[which.max(d$x), ]; yhi <- d[which.max(d$y), ]; big <- d[which.max(d$bl), ]
    ganda <- d$provinsi[d$kuad == "Masalah ganda"]
    sig <- !is.na(s["p"]) && s["p"] < 0.05
    hub <- if (sig && s["rho"] < 0) "berlawanan arah" else if (sig) "searah" else "lemah dan tidak signifikan"
    ins(
      isi = "Setiap gelembung adalah satu provinsi. Posisi mendatar menunjukkan persentase Backlog 1, posisi tegak persentase Backlog 2, dan dua garis putus-putus adalah angka nasional yang membagi grafik menjadi empat kuadran. Ukuran gelembung sebanding dengan jumlah rumah tangga ber-backlog (Backlog 1 + Backlog 2), dan warnanya menunjukkan pulau.",
      sorot = list(
        sprintf("Hubungan kedua backlog antarprovinsi %s (Spearman ρ = %s, p %s).", hub, fmt_num(s["rho"]), pfmt(s["p"])),
        sprintf("%s memiliki Backlog 1 tertinggi (%s%%) dengan Backlog 2 %s%%; %s memiliki Backlog 2 tertinggi (%s%%) dengan Backlog 1 %s%%.",
                xhi$provinsi, fmt_num(xhi$x), fmt_num(xhi$y), yhi$provinsi, fmt_num(yhi$y), fmt_num(yhi$x)),
        sprintf("Sebaran %d provinsi: %d masalah kepemilikan, %d masalah kelayakhunian, %d relatif baik, dan %d masalah ganda%s.",
                nrow(d), as.integer(tb[["Masalah kepemilikan"]]), as.integer(tb[["Masalah kelayakhunian"]]), as.integer(tb[["Relatif baik"]]),
                as.integer(tb[["Masalah ganda"]]), if (length(ganda)) paste0(" (", join_id(ganda), ")") else ""),
        sprintf("Gelembung terbesar adalah %s (%s ribu rumah tangga ber-backlog) di kuadran %s.", big$provinsi, fmt_num(big$bl, 0), tolower(big$kuad))),
      simpul = if (!sig || s["rho"] < 0)
        "Sulit memiliki rumah dan menempati rumah tak layak terjadi di provinsi yang berbeda. Satu jenis program tidak dapat menjawab keduanya: provinsi di kuadran kanan bawah membutuhkan akses kepemilikan dan pembiayaan, sedangkan kuadran kiri atas membutuhkan perbaikan rumah yang sudah dimiliki."
      else "Provinsi dengan Backlog 1 tinggi cenderung juga memiliki Backlog 2 tinggi, sehingga program kepemilikan dan perbaikan rumah dapat disasarkan ke provinsi yang sama.")
  })
  
  output$p_tree   <- renderPlotly(p_treemap(input$t_yr))
  output$tree_leg <- renderUI(treemap_legend(input$t_yr))
  output$p_ice    <- renderPlotly(p_icicle(input$i_yr, input$i_b %||% "b2"))
  output$ice_leg  <- renderUI(icicle_legend(input$i_yr, input$i_b %||% "b2"))
  
  output$ins_tree <- renderUI({
    yr <- input$t_yr; nd <- build_tree(yr, paste0("rt_", yr), paste0("b2_pct_", yr))
    isl <- nd[nd$lvl == "Pulau", ]; pv <- nd[nd$lvl == "Provinsi", ]
    isl$b2s <- isl$b2n / sum(isl$b2n) * 100
    i1 <- isl[which.max(isl$value), ]; i2 <- isl[mx_ok(isl$pct), ]
    p1 <- pv[which.max(pv$value), ]; p2 <- pv[mx_ok(pv$pct), ]
    ins(
      isi = "Setiap kotak adalah wilayah: pulau di lapis terluar, lalu provinsi, lalu kab/kota. Luas kotak sebanding dengan jumlah seluruh rumah tangga, dan warnanya menunjukkan persentase Backlog 2 (makin cokelat makin tinggi).",
      sorot = list(
        sprintf("%s menampung %s%% rumah tangga yang terpetakan, dengan rata-rata Backlog 2 %s%%, dan menyumbang %s%% dari seluruh rumah tangga Backlog 2.",
                i1$label, fmt_num(i1$share, 1), fmt_num(i1$pct, 1), fmt_num(i1$b2s, 1)),
        if (!identical(i1$label, i2$label))
          sprintf("Persentase tertinggi antarpulau ada di %s (%s%%), tetapi pulau itu hanya memuat %s%% rumah tangga dan %s%% dari seluruh Backlog 2.",
                  i2$label, fmt_num(i2$pct, 1), fmt_num(i2$share, 1), fmt_num(i2$b2s, 1)),
        sprintf("Antarprovinsi, %s paling besar (%s%% rumah tangga; Backlog 2 %s%%), sedangkan persentase tertinggi ada di %s (%s%%; %s%% rumah tangga).",
                p1$label, fmt_num(p1$share, 1), fmt_num(p1$pct, 1), p2$label, fmt_num(p2$pct, 1), fmt_num(p2$share, 1))),
      simpul = sprintf("Kotak besar berwarna terang, seperti %s, tetap menyumbang banyak rumah tangga ber-backlog karena ukurannya. Kotak kecil berwarna cokelat tua, seperti %s, menunjukkan masalah yang dalam pada populasi kecil. Keduanya butuh pendekatan berbeda: skala anggaran untuk yang pertama, ketepatan sasaran untuk yang kedua.",
                       p1$label, p2$label))
  })
  output$ins_ice <- renderUI({
    yr <- input$i_yr; b <- input$i_b %||% "b2"; B <- b_name(b)
    nd <- build_tree(yr, paste0(b, "_n_", yr), paste0(b, "_pct_", yr))
    isl <- nd[nd$lvl == "Pulau", ]; isl <- isl[order(-isl$value), ]
    pv <- nd[nd$lvl == "Provinsi", ]; kb <- nd[nd$lvl == "Kab/kota", ]
    pv <- pv[order(-pv$value), ]; top3 <- head(pv, 3); kb <- kb[order(-kb$value), ]
    hp <- pv[mx_ok(pv$pct), ]
    ins(
      isi = sprintf("Icicle dibaca dari atas ke bawah: pulau, lalu provinsi, lalu kab/kota. Lebar kotak sebanding dengan jumlah rumah tangga %s tahun %s, dan warnanya menunjukkan persentase %s (makin tua makin tinggi). Klik pulau atau provinsi untuk memperbesar kotak di bawahnya.", B, yr, B),
      sorot = list(
        sprintf("Antarpulau, %s menyumbang %s%% dari seluruh %s yang terpetakan (%s ribu rumah tangga), disusul %s (%s%%).",
                isl$label[1], fmt_num(isl$share[1], 1), B, fmt_num(isl$value[1], 0), isl$label[2], fmt_num(isl$share[2], 1)),
        sprintf("Tiga provinsi penyumbang terbesar: %s, bersama-sama %s%%.",
                join_id(sprintf("%s (%s%%)", top3$label, fmt_num(top3$share, 1))), fmt_num(sum(top3$share), 1)),
        sprintf("Kab/kota dengan jumlah terbesar: %s.", join_id(sprintf("%s (%s ribu RT)", head(kb$label, 3), fmt_num(head(kb$value, 3), 1)))),
        sprintf("Persentase provinsi tertinggi ada di %s (%s%%), tetapi porsinya hanya %s%% dari total.", hp$label, fmt_num(hp$pct, 1), fmt_num(hp$share, 1))),
      simpul = sprintf("Jumlah rumah tangga %s terkumpul di sedikit pulau dan provinsi berpenduduk besar, sedangkan persentase tertingginya berada di wilayah lain. Wilayah berkotak lebar menentukan besaran anggaran, sementara wilayah berwarna paling tua menentukan di mana masalahnya paling dalam.", B))
  })
  
  # Bab 4: klaster dan sorotan lintas grafik
  k_val <- reactive(as.integer(input$k %||% "4"))
  cl  <- reactive(get_cl(k_val()))
  cli <- reactive(cluster_info(k_val()))
  sel_prov <- reactiveVal(character(0))
  
  observeEvent(event_data("plotly_selected", source = "pca"), {
    ed <- event_data("plotly_selected", source = "pca")
    sel_prov(if (is.null(ed) || length(ed) == 0 || is.null(ed$key)) character(0) else as.character(ed$key))
  })
  observeEvent(event_data("plotly_deselect", source = "pca"), sel_prov(character(0)))
  observeEvent(input$btn_reset, {
    sel_prov(character(0))
    plotlyProxy("p_pca", session) %>% plotlyProxyInvoke("relayout", list(selections = list()))
  })
  
  output$cl_cards <- renderUI({
    ci <- cli()
    tags$div(class = "cl-cards",
             lapply(seq_len(nrow(ci)), function(j)
               tags$div(class = "cl-card",
                        tags$div(class = "cl-head", tags$i(class = "cl-dot", style = paste0("background:", PAL_CL[j]), `aria-hidden` = "true"),
                                 sprintf("Klaster %d · %d provinsi", j, ci$n[j])),
                        tags$div(class = "cl-name", ci$nama[j]),
                        tags$div(class = "cl-ciri", "Ciri: ", ci$ciri[j]),
                        tags$div(class = "cl-member", ci$anggota[j]))))
  })
  output$cl_legend <- renderUI({
    ci <- cli()
    tagList(lapply(seq_len(nrow(ci)), function(j)
      tags$span(class = "cl-chip", tags$i(style = paste0("background:", PAL_CL[j]), `aria-hidden` = "true"),
                sprintf("%d %s", j, sub("^Ketertinggalan ", "", ci$nama[j])))),
      if (length(sel_prov())) tags$span(class = "cl-sel", sprintf("Sorotan: %d provinsi", length(sel_prov()))))
  })
  
  output$p_bubble <- renderPlotly(p_bubble(input$b_y, input$b_isl, cl(), sel_prov()))
  output$ins_bubble <- renderUI({
    yv <- input$b_y; col <- VARS$col[VARS$id == yv]; lab <- VARS$label[VARS$id == yv]; dg <- VARS$digits[VARS$id == yv]
    s <- sp_test(PROV$rumah_layak_huni, PROV[[col]])
    P <- PROV[is.finite(PROV[[col]]), ]; y <- P[[col]]
    hi <- P[which.max(y), ]; lo <- P[which.min(y), ]
    sig <- !is.na(s["p"]) && s["p"] < 0.05
    arah <- if (!sig) "lemah dan tidak signifikan" else if (s["rho"] < 0) "negatif: akses rumah layak lebih rendah, nilai indikator lebih tinggi"
    else "positif: akses rumah layak lebih tinggi, nilai indikator juga lebih tinggi"
    pulau <- if (!is.null(input$b_isl) && input$b_isl != "ALL") {
      di <- PROV[PROV$pulau == input$b_isl, ]
      sprintf("Sorotan %s: %d provinsi, rata-rata akses rumah layak %s%% dan rata-rata %s %s.", tc(input$b_isl), nrow(di),
              fmt_num(mean(di$rumah_layak_huni, na.rm = TRUE)), tolower(sub(" \\(.*\\)$", "", lab)), fmt_num(mean(di[[col]], na.rm = TRUE), dg))
    }
    ins(
      isi = sprintf("Setiap gelembung adalah satu provinsi. Posisi mendatar menunjukkan persentase rumah tangga dengan akses rumah layak huni (2025), posisi tegak menunjukkan %s. Ukuran gelembung sebanding dengan jumlah penduduk, warnanya menunjukkan klaster, dan garis putus-putus adalah tren linear seluruh provinsi.",
                    tolower(lab)),
      sorot = list(
        sprintf("Korelasi Spearman ρ = %s (p %s, n = %d): hubungannya %s.", fmt_num(s["rho"]), pfmt(s["p"]), as.integer(s["n"]), arah),
        sprintf("Nilai tertinggi di %s (%s), terendah di %s (%s).", hi$provinsi, fmt_num(hi[[col]], dg), lo$provinsi, fmt_num(lo[[col]], dg)),
        if (yv == "tbc") "Cakupan penemuan TBC mengukur seberapa banyak kasus yang ditemukan dan diobati layanan kesehatan, bukan banyaknya penyakit; nilai tinggi berarti deteksi lebih baik.",
        if (yv == "dbd") sprintf("DBD kosong untuk %s, sehingga keduanya tidak digambar.", join_id(PROV$provinsi[is.na(PROV$dbd_100k)])),
        pulau,
        if (length(sel_prov())) sprintf("Sorotan dari biplot: %s.", join_id(head(sel_prov(), 8)))),
      simpul = if (!sig)
        sprintf("Antarprovinsi, akses rumah layak tidak memberi petunjuk tentang %s. Bila hunian memengaruhi kesehatan, pengaruhnya bekerja di tingkat rumah tangga dan tertutup oleh perbedaan layanan kesehatan, kepadatan, dan iklim antarprovinsi.",
                tolower(sub(" \\(.*\\)$", "", lab)))
      else if (s["rho"] < 0) "Provinsi dengan akses rumah layak lebih rendah memang cenderung memiliki nilai indikator ini lebih tinggi. Hubungan ini antarrata-rata provinsi, jadi belum membuktikan bahwa rumah tidak layak menyebabkannya."
      else "Arah hubungannya berlawanan dengan dugaan. Penjelasan yang lebih masuk akal adalah perbedaan kemampuan pelaporan dan kepadatan kota, bukan bahwa rumah layak memperburuk kesehatan.")
  })
  
  # Ringkasan heatmap korelasi, dipakai juga di bab kesimpulan.
  corr_sum <- reactive({
    d <- corr_data(); h <- d[d$c != "miskin_2025", ]
    # Searah dugaan: hunian lebih buruk bersama kesehatan lebih buruk. Indikator hunian "tinggi = baik"
    # dan kolom cakupan TBC (tinggi = baik) masing-masing membalik tanda yang diharapkan.
    good_row <- h$r %in% c("rumah_layak_huni", "bangunan_2025", "lantai_2025", "air_2025", "sanitasi_2025")
    exp_sign <- ifelse(good_row, -1, 1) * ifelse(h$c == "tbc_cakupan_pct", -1, 1)
    sg <- h$p < 0.05 & is.finite(h$p)
    h$exp <- sign(h$rho) == exp_sign
    list(d = d, h = h, nsig = sum(sg), n_exp = sum(sg & h$exp), n_opp = sum(sg & !h$exp))
  })
  output$p_corr <- renderPlotly(p_corr())
  output$ins_corr <- renderUI({
    cs <- corr_sum(); h <- cs$h; d <- cs$d
    hs <- h[h$p < 0.05 & is.finite(h$p), ]; hs <- hs[order(-abs(hs$rho)), ]
    nm <- function(x) sprintf("%s × %s (ρ = %s)", tolower(CORR_ROWS[x$r]), tolower(CORR_COLS[x$c]), fmt_num(x$rho, 2))
    ex <- hs[hs$exp, ]; op <- hs[!hs$exp, ]
    pm <- d[d$c == "miskin_2025", ]; pm <- pm[order(-abs(pm$rho)), ][1:2, ]
    ins(
      isi = "Setiap sel adalah korelasi Spearman antarprovinsi antara satu indikator hunian (baris) dan satu indikator kesehatan (kolom). Merah bata berarti keduanya naik bersama, biru berarti berlawanan arah, dan tanda bintang menandai korelasi yang signifikan. Kolom kemiskinan ditambahkan sebagai pembanding.",
      sorot = list(
        sprintf("%d dari %d pasangan hunian × kesehatan signifikan pada p < 0,05: %d searah dugaan dan %d berlawanan.", cs$nsig, nrow(h), cs$n_exp, cs$n_opp),
        if (nrow(op)) sprintf("Berlawanan dengan dugaan, yang paling kuat: %s.", join_id(nm(head(op, 2)))),
        if (nrow(ex)) sprintf("Searah dugaan: %s.", join_id(nm(head(ex, 3)))),
        sprintf("Indikator hunian yang paling erat dengan kemiskinan: %s.", join_id(sprintf("%s (ρ = %s)", tolower(CORR_ROWS[pm$r]), fmt_num(pm$rho, 2))))),
      simpul = if (cs$n_opp > cs$n_exp)
        "Sebagian besar hubungan yang signifikan berlawanan dengan dugaan, misalnya provinsi dengan sanitasi lebih layak justru mencatat DBD lebih tinggi. Ini tidak berarti hunian layak menyebabkan penyakit: penyakit lebih banyak tercatat di wilayah dengan layanan kesehatan lebih baik, dan DBD lebih sering terjadi di kota padat yang umumnya juga berhunian lebih baik. Kemiskinan berkorelasi lebih kuat dengan defisit hunian daripada indikator kesehatan mana pun."
      else "Hubungan yang signifikan umumnya searah dugaan: hunian yang lebih buruk berpasangan dengan kesehatan yang lebih buruk. Karena kemiskinan juga berkorelasi dengan defisit hunian, sebagian hubungan ini bisa mencerminkan kemiskinan.")
  })
  
  output$p_heat   <- renderPlotly(p_heatmap(k_val()))
  output$heat_leg <- renderUI(chips(tc(ISLAND_LEVELS), unname(PAL_ISLAND[ISLAND_LEVELS])))
  # Tabel HTML biasa (bukan DT): tidak bergantung pada inisialisasi DataTables di peramban.
  output$tbl_cl   <- renderUI({
    d <- cluster_table(k_val())
    num <- vapply(d, is.numeric, logical(1))
    cell <- function(v, j) if (num[j]) fmt_num(v, if (names(d)[j] == "Jumlah provinsi") 0 else 2) else as.character(v)
    tags$div(class = "tbl-wrap", tabindex = "0", role = "region", `aria-label` = "Profil klaster",
             tags$table(class = "cl-table",
                        tags$thead(tags$tr(lapply(names(d), tags$th))),
                        tags$tbody(lapply(seq_len(nrow(d)), function(i)
                          tags$tr(lapply(seq_along(d), function(j)
                            tags$td(class = if (num[j]) "num" else NULL, cell(d[[j]][i], j))))))))
  })
  output$ins_cluster <- renderUI({
    flip <- ifelse(VARS$good_high[match(colnames(AN$Z), VARS$id)], -1, 1)
    Zd <- sweep(AN$Z, 2, flip, `*`)
    r <- suppressWarnings(stats::cor(Zd, method = "spearman")); r[upper.tri(r, diag = TRUE)] <- NA
    kk <- which(!is.na(r), arr.ind = TRUE); v <- r[kk]; o <- order(-v)[1:3]
    nm <- function(i) tolower(heat_lab(colnames(Zd)[i]))
    pairs <- sprintf("%s dengan %s (ρ = %s)", nm(kk[o, 1]), nm(kk[o, 2]), fmt_num(v[o], 2))
    worst <- names(sort(rowMeans(Zd), decreasing = TRUE))[1:3]
    best  <- names(sort(rowMeans(Zd)))[1:3]
    ci <- cli(); last <- ci[nrow(ci), ]
    ins(
      isi = "Setiap baris adalah satu provinsi dan setiap kolom satu indikator. Warna menunjukkan skor-z: merah bata berarti lebih buruk dari rata-rata provinsi, hijau toska lebih baik. Provinsi yang mirip diletakkan berdekatan menurut dendrogram di kiri, dan pita di sebelahnya menunjukkan klaster serta pulau. Tabel di bawah heatmap berisi rata-rata nilai asli tiap klaster.",
      sorot = list(
        sprintf("Defisit yang paling sering muncul bersamaan: %s.", join_id(pairs)),
        sprintf("Provinsi dengan defisit terbanyak (rata-rata warna paling merah): %s. Paling sedikit: %s.", join_id(worst), join_id(best)),
        sprintf("Dengan %d klaster: %s.", nrow(ci), join_id(sprintf("%s (%d provinsi)", tolower(ci$nama), ci$n))),
        if (any(nzchar(AN$imp_note))) "Tanda × menandai sel yang kosong di sumber dan diisi untuk analisis; arahkan kursor untuk melihat cara pengisiannya."),
      simpul = sprintf("Defisit hunian dan kemiskinan cenderung muncul bersama pada provinsi yang sama. Klaster paling tertinggal (%s: %s) dicirikan oleh %s, sehingga memerlukan intervensi gabungan, bukan program per sektor.",
                       tolower(last$nama), last$anggota, last$ciri))
  })
  
  output$p_load <- renderPlotly(p_loadings())
  output$ins_load <- renderUI({
    ve <- summary(AN$pca)$importance[2, ] * 100; ld <- AN$pca$rotation[, 1:2]
    top <- function(j) { o <- order(-abs(ld[, j]))[1:3]; join_id(sprintf("%s (%s)", CIRI_LAB[rownames(ld)[o]], fmt_num(ld[o, j], 2))) }
    p2hi <- rownames(ld)[which.max(ld[, 2])]; p2lo <- rownames(ld)[which.min(ld[, 2])]
    ins(
      isi = "Batang menunjukkan muatan (loading) tiap indikator pada dua komponen utama PCA. Batang slate untuk PC1 (sumbu mendatar biplot), batang ochre untuk PC2 (sumbu tegak). Batang ke kanan berarti indikator itu naik ketika provinsi bergeser ke kanan atau ke atas pada biplot; batang ke kiri berarti sebaliknya.",
      sorot = list(
        sprintf("PC1 menyimpan %s%% variasi dan paling dibentuk oleh %s.", fmt_num(ve[1], 1), top(1)),
        sprintf("PC2 menyimpan %s%% variasi dan paling dibentuk oleh %s.", fmt_num(ve[2], 1), top(2)),
        sprintf("Bersama-sama keduanya menangkap %s%% variasi; sisanya tersebar di komponen lain.", fmt_num(sum(ve[1:2]), 1))),
      simpul = sprintf("PC1 dapat dibaca sebagai sumbu ketertinggalan karena searah dengan kemiskinan dan defisit hunian dasar. PC2 terutama membedakan provinsi dengan %s tinggi dari provinsi dengan %s tinggi. Karena dua sumbu hanya menangkap %s%% variasi, jarak antarprovinsi di biplot adalah gambaran pendekatan.",
                       CIRI_LAB[[p2hi]], CIRI_LAB[[p2lo]], fmt_num(sum(ve[1:2]), 1)))
  })
  output$p_pca <- renderPlotly(p_pca(cl(), sel_prov()))
  output$ins_pca <- renderUI({
    pc <- AN$pca; sc <- pc$x[, 1:2]
    arah_pc <- function(j) {
      ld <- pc$rotation[, j]; top <- names(sort(abs(ld), decreasing = TRUE))[1:4]
      hi <- top[ld[top] > 0]; lo <- top[ld[top] < 0]
      paste0(if (length(hi)) paste(join_id(CIRI_LAB[hi]), "lebih tinggi") else "",
             if (length(hi) && length(lo)) " serta " else "",
             if (length(lo)) paste(join_id(CIRI_LAB[lo]), "lebih rendah") else "")
    }
    dist <- sqrt(as.numeric(scale(sc[, 1]))^2 + as.numeric(scale(sc[, 2]))^2)
    right <- rownames(sc)[order(-sc[, 1])][1:3]; left <- rownames(sc)[order(sc[, 1])][1:3]
    far <- rownames(sc)[order(-dist)][1:3]
    ins(
      isi = "Setiap titik adalah satu provinsi, diwarnai menurut klaster. Posisinya adalah skor pada dua komponen utama, dan panah abu-abu menunjukkan arah naiknya tiap indikator. Provinsi yang berdekatan berprofil mirip.",
      sorot = list(
        sprintf("Makin ke kanan, provinsi cenderung memiliki %s.", arah_pc(1)),
        sprintf("Makin ke atas, provinsi cenderung memiliki %s.", arah_pc(2)),
        sprintf("Paling kanan: %s. Paling kiri: %s.", join_id(right), join_id(left)),
        sprintf("Tiga titik terjauh dari pusat: %s.", join_id(far)),
        if (length(sel_prov())) sprintf("Sorotan aktif: %s.", join_id(head(sel_prov(), 8)))),
      simpul = sprintf("Provinsi di Papua menempati ujung kanan sumbu ketertinggalan, sedangkan provinsi di Jawa dan kepulauan dekat Sumatera berada di sisi kiri. Titik terjauh dari pusat, seperti %s, memiliki kombinasi indikator yang tidak umum dan layak ditelaah tersendiri sebelum disamakan dengan provinsi lain dalam satu program.",
                       join_id(far[1:2])))
  })
  
  # Bab 5
  output$ins_cap <- renderUI({
    d <- CAPR$cmp
    req(!is.null(d), nrow(d) > 0)
    d$txt <- vapply(seq_len(nrow(d)), function(i) cap_change_txt(d$s1_2025[i], d$s1_2026[i]), character(1))
    up <- d[d$s1_2026 >= d$s1_2025, ]; dn <- d[d$s1_2026 < d$s1_2025, ]
    lst <- function(x) join_id(sprintf("%s (%s, %s menjadi %s)", x$nama, x$txt, fmt_num(x$s1_2025, 0), fmt_num(x$s1_2026, 0)))
    g <- function(key) CAP$s1_2026[grepl(key, CAP$program)][1]
    flpp <- g("FLPP") / (NAS$b1_n_2026 * 1000) * 100
    bsps <- g("BSPS") / (NAS$b2_n_2026 * 1000) * 100
    ins(
      isi = sprintf("Setiap baris adalah satu dari %d program yang capaiannya dapat dibandingkan pada periode setara, yaitu Semester I 2025 dan Semester I 2026. Panjang batang dihitung terhadap nilai terbesar di baris yang sama, karena besaran tiap program berbeda jauh. Bilah di bagian bawah membandingkan capaian Semester I 2026 dengan besarnya backlog.", nrow(d)),
      sorot = list(
        if (nrow(dn)) sprintf("Turun: %s.", lst(dn)),
        if (nrow(up)) sprintf("Naik: %s.", lst(up)),
        if (is.finite(flpp)) sprintf("FLPP Semester I 2026 setara %s%% dari Backlog 1, dan BSPS pada periode yang sama setara %s%% dari Backlog 2.", fmt_num(flpp, 2), fmt_num(bsps, 2))),
      simpul = sprintf("%d dari %d program yang dapat dibandingkan turun pada Semester I 2026%s. Dibanding backlog, skala program tetap kecil: satu semester FLPP setara %s%% Backlog 1. Satu unit program juga belum tentu sama dengan satu rumah tangga yang keluar dari backlog.",
                       nrow(dn), nrow(d), if ("FLPP" %in% dn$key) ", termasuk FLPP sebagai program pembiayaan rumah utama" else "",
                       fmt_num(flpp, 2)))
  })
  output$cap_rules <- renderUI({
    r <- CAPR$rules; r <- r[!r$banding, ]
    if (!nrow(r)) return(tags$p("Semua program dalam tabel dapat dibandingkan."))
    tagList(
      tags$p("Aturan: bandingkan semester yang sama bila ada, dan jangan membandingkan Semester I 2026 dengan total 2025. Program berikut tidak memenuhi syarat itu:"),
      tags$table(class = "rules",
                 tags$thead(tags$tr(tags$th("Program"), tags$th("Status"), tags$th("Alasan"))),
                 tags$tbody(lapply(seq_len(nrow(r)), function(i) tags$tr(tags$td(r$program[i]), tags$td(r$status[i]), tags$td(r$catatan[i]))))),
      tags$p(class = "rules-src", "Disusun dari BPS, Statistik Perumahan 2026, Tabel 6.1 dan Bab 3–5."))
  })
  
  # Bab 6: kesimpulan (semua angka dihitung dari data yang sama dengan grafik)
  output$kesimpulan <- renderUI({
    P <- PROV
    d1 <- NAS$b1_pct_2026 - NAS$b1_pct_2025; d2 <- NAS$b2_pct_2026 - NAS$b2_pct_2025
    b1hi <- P[which.max(P$b1_pct_2026), ]; b2hi <- P[which.max(P$b2_pct_2026), ]
    s12 <- sp_test(P$b1_pct_2026, P$b2_pct_2026)
    big1 <- P[which.max(P$b1_n_2026), ]; big2 <- P[which.max(P$b2_n_2026), ]
    sh1 <- big1$b1_n_2026 / NAS$b1_n_2026 * 100; sh2 <- big2$b2_n_2026 / NAS$b2_n_2026 * 100
    up2 <- P$provinsi[is.finite(P$b2_pct_2026 - P$b2_pct_2025) & P$b2_pct_2026 > P$b2_pct_2025]
    komp <- komp_data("Nasional"); komp$chg <- komp$b - komp$a
    low <- komp[which.min(komp$b), ]; fast <- komp[which.max(komp$chg), ]
    ci <- cli(); lastc <- ci[nrow(ci), ]
    lz <- LISA[["b2_2026"]]
    hot <- if (!is.null(lz)) lisa_top(lz, LISA_LEVELS[1], 2) else NULL
    cs <- corr_sum()
    flpp <- CAP$s1_2026[grepl("FLPP", CAP$program)][1] / (NAS$b1_n_2026 * 1000) * 100
    cmp <- CAPR$cmp; n_dn <- if (is.null(cmp)) 0L else sum(cmp$s1_2026 < cmp$s1_2025)
    find <- function(h, ...) tags$div(class = "find", tags$h4(h), tags$p(...))
    li <- function(...) tags$li(...)
    
    tagList(
      tags$div(class = "concl-block",
               tags$h3("Pola utama"),
               tags$div(class = "find-list",
                        find("Membaik di tingkat nasional, timpang antarwilayah",
                             sprintf("Backlog 1 turun %s poin dan Backlog 2 turun %s poin dari 2025 ke 2026, dan akses rumah layak huni naik menjadi %s%%. ",
                                     fmt_num(abs(d1)), fmt_num(abs(d2)), fmt_num(NAS$akses_layak_2026)),
                             sprintf("Perubahan itu kecil dibanding jarak antarprovinsi: Backlog 2 berkisar sampai %s%% di %s. Sanitasi layak tetap komponen terendah (%s%%).",
                                     fmt_num(b2hi$b2_pct_2026), b2hi$provinsi, fmt_num(low$b))),
                        find("Kepemilikan dan kelayakhunian adalah dua masalah berbeda",
                             sprintf("Backlog 1 tertinggi di %s (%s%%), Backlog 2 tertinggi di %s (%s%%). Antarprovinsi, keduanya %s (ρ = %s), jadi satu program tidak dapat menjawab keduanya.",
                                     b1hi$provinsi, fmt_num(b1hi$b1_pct_2026), b2hi$provinsi, fmt_num(b2hi$b2_pct_2026),
                                     if (!is.na(s12["p"]) && s12["p"] < 0.05) "berkaitan" else "hampir tidak berkaitan", fmt_num(s12["rho"]))),
                        find("Persentase tinggi tidak sama dengan jumlah besar",
                             if (identical(big1$provinsi, big2$provinsi))
                               sprintf("%s menanggung %s%% dari seluruh Backlog 1 dan %s%% dari seluruh Backlog 2 nasional, meskipun persentasenya tidak termasuk yang tertinggi. ",
                                       big1$provinsi, fmt_num(sh1, 1), fmt_num(sh2, 1))
                             else sprintf("%s menanggung %s%% dari seluruh Backlog 1 dan %s menanggung %s%% dari seluruh Backlog 2 nasional, meskipun persentasenya tidak termasuk yang tertinggi. ",
                                          big1$provinsi, fmt_num(sh1, 1), big2$provinsi, fmt_num(sh2, 1)),
                             "Prioritas menurut jumlah dan menurut persentase menghasilkan daftar wilayah yang berbeda."),
                        if (!is.null(lz)) find("Backlog kelayakhunian mengelompok secara spasial",
                                               sprintf("Moran’s I Backlog 2 kab/kota tahun 2026 adalah %s (p %s). ", fmt_num(lz$I, 3), pfmt(lz$p)),
                                               if (!is.null(hot)) sprintf("Hotspot terbanyak di %s, sehingga intervensi lintas kabupaten lebih tepat daripada per wilayah administratif yang terpisah.", hot) else ""),
                        find("Hunian dan kesehatan tidak bergerak searah antarprovinsi",
                             sprintf("Dari %d pasangan indikator hunian × kesehatan yang signifikan, hanya %d searah dugaan dan %d berlawanan. ", cs$nsig, cs$n_exp, cs$n_opp),
                             "Angka penyakit yang tercatat lebih mencerminkan kemampuan pelaporan dan kepadatan kota daripada kondisi rumah. Kemiskinan justru paling erat dengan defisit hunian."),
                        find("Skala program jauh di bawah kebutuhan",
                             sprintf("%d dari %d program yang dapat dibandingkan turun pada Semester I 2026. Satu semester FLPP setara %s%% Backlog 1.",
                                     n_dn, if (is.null(cmp)) 0L else nrow(cmp), fmt_num(flpp, 2))))),
      tags$div(class = "concl-block",
               tags$h3("Yang perlu digarisbawahi"),
               tags$ul(class = "concl-list",
                       li(sprintf("%s adalah komponen rumah layak terendah secara nasional (%s%%)%s; perbaikannya paling menentukan kenaikan akses rumah layak huni.",
                                  cap1(tolower(low$komp)), fmt_num(low$b), if (identical(low$id, fast$id)) sprintf(" sekaligus yang paling cepat membaik (%s poin)", fmt_pp(fast$chg)) else "")),
                       if (length(up2)) li(sprintf("Backlog 2 naik di %s, berlawanan dengan arah nasional.", join_id(up2))),
                       li(sprintf("Klaster paling tertinggal (%s) dicirikan oleh %s.", lastc$anggota, lastc$ciri)),
                       li(sprintf("%s adalah kasus khusus: Backlog 1 tertinggi (%s%%), tetapi Backlog 2-nya %s%%, peringkat ke-%d terendah dari %d provinsi.",
                                  b1hi$provinsi, fmt_num(b1hi$b1_pct_2026), fmt_num(b1hi$b2_pct_2026), as.integer(rank(P$b2_pct_2026)[which.max(P$b1_pct_2026)]), nrow(P))),
                       li(sprintf("Angka kab/kota sama dengan tabel BPS. %d kab/kota tidak memiliki estimasi Backlog 1 2026 karena BPS tidak menyajikannya.", DQ$kab_b1_na26)))),
      tags$div(class = "concl-block",
               tags$h3("Keterbatasan dan saran pengembangan"),
               tags$ul(class = "concl-list",
                       li(tags$b("Hubungan antarprovinsi bersifat korelasional dan ekologis. "), "Pengembangan berikutnya dapat memakai data Susenas tingkat rumah tangga dan model regresi untuk menguji hubungan hunian dan kesehatan secara lebih ketat."),
                       li(tags$b("Indikator kesehatan hanya satu tahun dan hanya tingkat provinsi. "), "Data kesehatan kab/kota dan deret waktu beberapa tahun akan memungkinkan analisis perubahan dan analisis spasial yang setara dengan data perumahan."),
                       li(tags$b("Angka kab/kota Susenas memiliki galat sampling. "), "Publikasi BPS menyertakan sampling error (Tabel 2.11 dan 2.12) yang dapat ditampilkan sebagai selang kepercayaan di peta dan popup."),
                       li(tags$b("Hasil LISA bergantung pada pilihan tetangga dan tidak dikoreksi untuk uji berganda. "), sprintf("Uji sensitivitas dengan jumlah tetangga lain selain %d dan koreksi FDR akan memperkuat temuan hotspot.", KNN_K)),
                       li(tags$b("Capaian program hanya tersedia nasional dan per semester. "), "Data per provinsi (misalnya FLPP menurut provinsi di publikasi yang sama) dapat dipetakan bersama backlog untuk melihat apakah program menyasar wilayah yang tepat."),
                       li(tags$b("Keterbacaan aplikasi belum diuji dengan pengguna. "), "Uji keterpakaian dengan System Usability Scale dapat menilai apakah grafik dan interpretasinya mudah dipahami pembaca awam."))),
      more_box(list(qa("Catatan data dan metode", tags$ul(class = "note-list", data_notes())))))
  })
  
  data_notes <- function() {
    li <- function(...) tags$li(...)
    tagList(
      li(tags$b("Sumber nilai perumahan. "), "Backlog 1 dan Backlog 2 provinsi dan kab/kota, komponen rumah layak, atap asbes, BABS, dan sumber penerangan sama dengan BPS, Statistik Perumahan 2026 (Tabel 2.1 sampai 2.10). Berkas Excel sebelumnya memuat kolom yang bergeser dan tertukar; berkas di folder data sudah diperbaiki dan mencantumkan sumber tiap kolom di sheet Keterangan."),
      li(tags$b("Sumber nilai kesehatan. "), "Keluhan kesehatan dari BPS, Statistik Kesehatan 2025 (Tabel 2.1); kasus diare yang dilayani dari Kemenkes, Profil Kesehatan Indonesia 2025 (Lampiran 6.14), dibagi jumlah penduduk; DBD, cakupan TBC, kemiskinan, dan penduduk dari tabel dinamis BPS."),
      li(tags$b("Kode wilayah peta. "),
         sprintf("GeoJSON memakai kode Kemendagri, sedangkan data memakai kode BPS (mis. 1101 = Simeulue di data, tetapi Aceh Selatan di GeoJSON). Peta digabung berdasarkan nama kab/kota dalam provinsi; %d dari %d poligon cocok.",
                 DQ$geo_match, DQ$geo_n)),
      li(tags$b("BABS dan sanitasi. "), sprintf("Spearman \u03C1 = %s, di bawah ambang 0,85, sehingga keduanya dipakai dalam klaster dan PCA.", fmt_num(AN$rho_babs_sanitasi, 2))),
      li(tags$b("Sel kosong provinsi. "),
         "DBD kosong untuk ", join_id(PROV$provinsi[is.na(PROV$dbd_100k)]), " dan diisi median hanya untuk klaster dan PCA. ",
         if (length(DQ$na_cells$asbes_pct_2025)) sprintf("Atap asbes tidak disajikan BPS untuk %s dan dibaca 0. ", join_id(DQ$na_cells$asbes_pct_2025)),
         if (length(DQ$na_cells$babs_pct_2025)) sprintf("BABS tidak disajikan untuk %s dan dibaca 0. ", join_id(DQ$na_cells$babs_pct_2025)),
         if (length(DQ$na_cells$pen_nonpln_2025)) sprintf("Listrik non-PLN tidak disajikan untuk %s dan diisi sisa 100 \u2212 PLN \u2212 bukan listrik.", join_id(DQ$na_cells$pen_nonpln_2025))),
      li(tags$b("TBC. "), "Kolom tbc_n adalah jumlah penemuan TBC"),
      li(tags$b("Ukuran gelembung kuadran. "), "Backlog 1 dan Backlog 2 dijumlahkan karena definisinya tidak tumpang tindih."),
      li(tags$b("Capaian program. "), "Hanya program dengan periode setara yang dibandingkan antartahun; satu unit program tidak sama dengan satu rumah tangga yang keluar dari backlog."))
  }
  
  # Unduhan
  output$dl_prov <- downloadHandler(filename = function() basename(F_PROV), content = function(file) file.copy(F_PROV, file))
  output$dl_kab  <- downloadHandler(filename = function() basename(F_KAB),  content = function(file) file.copy(F_KAB, file))
  output$dl_cap  <- downloadHandler(filename = function() basename(F_CAP),  content = function(file) file.copy(F_CAP, file))
  
  # Isi accordion ikut dirender walau details masih tertutup, agar langsung tampil saat dibuka.
  for (o in c("na_choro", "na_prop", "na_lisa", "cap_rules")) outputOptions(output, o, suspendWhenHidden = FALSE)
}

shinyApp(ui, server)
