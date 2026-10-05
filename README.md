# Hunian Layak Indonesia

Laman data-story interaktif tentang ketimpangan **kepemilikan rumah (Backlog 1)** dan **kelayakhunian rumah (Backlog 2)** di 38 provinsi dan 514 kabupaten/kota Indonesia, 2025–2026, serta kaitannya dengan indikator kesehatan. Dibangun dengan R Shiny.

- **Aplikasi:** https://8uj79q-hanifa-aura.shinyapps.io/visdat-rumah-hunian-layak/
- Proyek UAS Visualisasi Data dan Informasi (K203407), Program Studi Komputasi Statistik, Politeknik Statistika STIS, semester genap 2025/2026.
- Penyusun: Aura Hanifa Kasetya Putri, 222313003, kelas 3SD2


## Pertanyaan yang dijawab

1. Seberapa besar masalah hunian secara nasional, dan apakah membaik dari 2025 ke 2026?
2. Di mana persentase dan jumlah rumah tangga ber-backlog paling tinggi, dan apakah keduanya berada di tempat yang sama?
3. Apakah kabupaten/kota dengan backlog tinggi saling berdekatan membentuk kelompok spasial?
4. Apakah provinsi dengan hunian kurang layak juga mencatat indikator kesehatan yang lebih buruk?
5. Program perumahan mana yang capaiannya naik atau turun dibanding periode yang sama tahun lalu, dan seberapa besar skalanya dibanding backlog?

## Temuan utama

- Secara nasional ketiga ukuran membaik: Backlog 1 turun dari 13,00% menjadi 12,39% (9,29 juta rumah tangga), Backlog 2 dari 25,33% menjadi 24,03% (18,01 juta), dan akses rumah layak huni naik dari 68,40% menjadi 70,30%.
- Perbaikan tidak merata. Backlog 1 turun di 38 provinsi, tetapi Backlog 2 naik di Papua Pegunungan (+2,62 poin), provinsi dengan Backlog 2 tertinggi (91,76%), dan di Papua Selatan.
- Kedua backlog berada di tempat berbeda: DKI Jakarta tertinggi untuk Backlog 1 (39,36%), Papua Pegunungan untuk Backlog 2. Korelasi keduanya antarprovinsi lemah dan tidak signifikan (Spearman ρ = −0,23; p = 0,17).
- Jumlah absolut terbesar ada di Jawa Barat (2,03 juta rumah tangga Backlog 1 dan 4,53 juta Backlog 2), walaupun persentasenya tidak termasuk yang tertinggi.
- Antarprovinsi, hunian kurang layak tidak selalu berarti penyakit tercatat lebih banyak. Dari 12 pasangan hunian × kesehatan yang signifikan, 9 berlawanan arah dugaan (misalnya sanitasi layak × DBD ρ = +0,61), yang lebih mungkin mencerminkan perbedaan pelaporan dan kepadatan kota daripada sebab-akibat.
- Dari lima program yang dapat dibandingkan pada periode setara (Semester I 2025 vs Semester I 2026), tiga turun (rumah susun baru, FLPP, pelonggaran GWM BI) dan dua naik (pembebasan retribusi PBG, CSR). FLPP Semester I 2026 setara 0,99% Backlog 1, dan BSPS setara 0,49% Backlog 2.

## Pemenuhan ketentuan tugas

Proyek mencakup tiga dari enam topik visualisasi.

| Topik | Ketentuan minimal | Pemenuhan di aplikasi |
|---|---|---|
| Geospasial | Tingkat kab/kota (± 500 unit) | 514 kabupaten/kota |
| | ≥ 2 jenis peta | Choropleth, simbol proporsional, peta LISA |
| | Klasifikasi dan palet dijustifikasi; choropleth memakai rasio | 5 kelas kuantil atas persentase; ramp sekuensial satu rona (teal untuk Backlog 1, ochre untuk Backlog 2); peta perubahan memakai skala divergen berpusat nol |
| | Tooltip, legenda, zoom/pan, kontrol layer | Popup per wilayah, legenda, tombol zoom dan tombol kembali ke seluruh Indonesia, pilihan jenis backlog, tahun, dan mode persentase/perubahan |
| | Opsional: Moran's I/LISA | Moran's I global dan LISA |
| Multivariat | ≥ 8 variabel numerik, ≥ 34 unit | 12 indikator, 38 provinsi |
| | 1 reduksi dimensi + ≥ 2 teknik lain | PCA (bar muatan dan biplot), heatmap berklaster dengan dendrogram, heatmap korelasi, diagram gelembung |
| | Brushing dan linking | Seleksi kotak pada biplot menyorot provinsi yang sama di diagram gelembung; bar klaster menempel di atas keempat grafik multivariat selama digulir |
| | Interpretasi kelompok dan pencilan | Kartu klaster Ward, tabel profil klaster, label pencilan pada biplot |
| Hierarki | ≥ 3 level | Pulau → provinsi → kabupaten/kota |
| | ≥ 2 representasi | Treemap dan icicle (atas ke bawah: Indonesia, pulau, provinsi, kab/kota) |
| | Ukuran dan warna mengodekan dua variabel berbeda | Treemap: ukuran = total rumah tangga, warna = % Backlog 2; icicle: ukuran = jumlah Backlog 2, warna = % Backlog 1 |
| | Drill-down dengan breadcrumb | Klik untuk masuk, jejak (pathbar) untuk kembali |

Ketentuan umum: setiap grafik dan peta memiliki tooltip dan zoom (grafik hierarki memakai drill-down); setiap kartu mencantumkan sumber, catatan baca (`*Warna: ...`), interpretasi tiga bagian (isi grafik, yang menonjol, kesimpulan), dan pertanyaan lanjutan dalam accordion; tampilan responsif hingga lebar ponsel.

## Isi aplikasi

Laman disusun sebagai satu cerita dari gambaran nasional ke detail wilayah. Nomor visual hanya dipakai di dokumen dan komentar kode; di halaman tidak ditampilkan.

| Bab | # | Jenis | Pertanyaan |
|---|---|---|---|
| 1 Gambaran Nasional | 1 | Kartu angka | Seberapa besar masalah hunian secara nasional, dan apakah membaik? |
| | 2 | Dumbbell | Komponen rumah layak huni mana yang naik paling banyak, dan mana yang tertinggal? |
| | 3 | Diagram batang | Provinsi mana yang tertinggi dan terendah untuk tiap indikator? |
| | 15 | Dumbbell perubahan | Provinsi mana yang membaik dan mana yang memburuk dari 2025 ke 2026? |
| 2 Sebaran Wilayah | 4 | Peta choropleth | Di kabupaten/kota mana persentase backlog paling tinggi, dan di mana berubah? |
| | 5 | Peta lingkaran | Di mana jumlah rumah tangga ber-backlog paling banyak? |
| | 6 | Peta LISA | Apakah kabupaten/kota dengan backlog tinggi saling berdekatan? |
| 3 Dua Wajah Backlog | 14 | Diagram kuadran | Apakah provinsi yang sulit memiliki rumah juga yang rumahnya tidak layak? |
| | 11 | Treemap | Wilayah mana yang besar populasinya sekaligus tinggi Backlog 2-nya? |
| | 12 | Icicle | Di mana beban Backlog 2 terkumpul, dan seberapa berat masalah kepemilikannya? |
| 4 Hunian dan Kesehatan | 7 | Diagram gelembung | Apakah provinsi dengan akses rumah layak lebih rendah memiliki masalah kesehatan lebih tinggi? |
| | 8 | Heatmap korelasi | Indikator hunian mana yang paling erat berkaitan dengan indikator kesehatan? |
| | 9 | Heatmap berklaster | Provinsi mana yang berprofil mirip, dan indikator mana yang naik-turun bersama? |
| | 10a, 10b | Muatan PCA, biplot | Indikator apa yang membentuk dua sumbu utama, dan provinsi mana yang menyimpang? |
| 5 Intervensi | 13 | Batang per program + bilah skala | Program mana yang capaiannya naik dan turun dibanding periode yang sama tahun lalu? |
| 6 Kesimpulan | | | Pola utama, hal yang perlu digarisbawahi, keterbatasan dan saran pengembangan |
| Unduh data | | | Tiga berkas Excel di folder data |

## Data

Data utama bersumber dari BPS. Data kesehatan dan batas wilayah adalah data pendukung.

| Berkas | Isi | Sumber |
|---|---|---|
| `data/dataset(provinsi).xlsx` | Backlog 1 dan 2, akses rumah layak huni dan empat komponennya (2025 dan 2026), BABS, atap asbes, sumber penerangan (2025 dan 2026), keluhan kesehatan, diare, cakupan TBC, DBD, kemiskinan, penduduk; 38 provinsi + nasional | BPS, *Statistik Perumahan 2026* Tabel 2.1, 2.3, 2.5–2.10; BPS, *Statistik Kesehatan 2025* Tabel 2.1 (keluhan); Kemenkes, *Profil Kesehatan Indonesia 2025* Lampiran 6.14 (diare dilayani); tabel dinamis BPS (DBD, cakupan TBC, kemiskinan, penduduk) |
| `data/backlog_kabkota.xlsx` | Backlog 1 dan 2 menurut 514 kabupaten/kota, 2025 dan 2026 | BPS, *Statistik Perumahan 2026* Tabel 2.2 dan 2.4 |
| `data/capaian_pemerintah.xlsx` | Capaian program perumahan (BSPS, FLPP, dan lainnya), 2025 dan Semester I 2026 | BPS, *Statistik Perumahan 2026* Tabel 6.1 |
| `data/program_dapat_dibandingkan.xlsx` | Program yang capaiannya bisa dibandingkan antarsemester beserta alasannya | Disusun dari BPS, Statistik Perumahan 2026, Tabel 6.1 |
| `data/kabkota_simplified.rds` | Batas kabupaten/kota yang sudah disederhanakan (± 500 m) dan diberi kunci gabung | Diolah dari `Indonesia_KAB_KOTA.geojson` |

Setiap berkas Excel memiliki sheet **Keterangan** berisi sumber tiap kolom. Angka disimpan sebagai bilangan; sel kosong berarti data tidak disajikan sumber. Kolom turunan (`diare_pct`, `rt_2025`, `rt_2026`, total 2025 capaian) berupa rumus Excel.

Rujukan BPS:
- Statistik Perumahan 2026: https://www.bps.go.id/id/publication/2026/08/31/777a9ca5c6cfd2a1d8626198/statistik-perumahan-2026.html, diakses 4 Oktober 2026
- Berita "BPS rilis perdana Statistik Perumahan 2026": https://www.bps.go.id/id/news/2026/08/22/937/, diakses 4 Oktober 2026

Berkas `Indonesia_KAB_KOTA.geojson` (550 MB) tidak disertakan karena melebihi batas 100 MB GitHub. Aplikasi hanya membutuhkan `kabkota_simplified.rds`. Bila `.rds` dihapus, aplikasi membuatnya ulang dari GeoJSON pada saat pertama dijalankan.

## Pengolahan data

Semua langkah ada di `src/data_prep.R` dan `src/analysis.R`; berkas sumber tidak diubah.

1. **Format angka.** Berkas mencampur koma desimal (`97,88`), koma ribuan (`1,192.25`), dan penanda kosong (`NAN`, `–`, `#VALUE!`). Semuanya diseragamkan oleh `to_num()`. Berkas capaian memakai titik sebagai pemisah ribuan (`45.073` = 45.073 unit) dan dibaca oleh `to_num_id()`.
2. **Total rumah tangga** dihitung dari jumlah dan persentase Backlog 2 (atau Backlog 1 bila Backlog 2 kosong), sama dengan rumus kolom `rt_*` di Excel.
3. **Nilai kosong.** Sel yang tidak disajikan BPS (NA) pada atap asbes dan BABS dibaca 0; listrik non-PLN yang kosong diisi sisa 100 − PLN − bukan listrik. DBD kosong untuk Papua Barat Daya dan Papua Pegunungan diisi median hanya untuk klaster dan PCA. Semua sel itu ditandai × di heatmap berklaster. Kabupaten/kota tanpa nilai backlog (NA di tabel BPS) tidak diisi dan tampil abu-abu di peta.
4. **TBC.** Kolom `tbc_cakupan_pct` adalah cakupan penemuan dan pengobatan kasus TBC (%), dengan nilai tinggi = lebih baik.
5. **Penggabungan peta.** GeoJSON memakai kode Kemendagri, sedangkan data memakai kode BPS, sehingga penggabungan memakai kunci nama: provinsi | Kab/Kota | nama ternormalisasi. Provinsi Papua hasil pemekaran (kode 91–97) digabung menjadi satu grup karena GeoJSON masih memakai pembagian lama.

## Metode analisis

- **Autokorelasi spasial:** Moran's I global (`spdep::moran.test`) dan LISA (`spdep::localmoran`) atas persentase backlog kabupaten/kota. Tetangga = 6 kabupaten/kota terdekat (jarak lingkaran besar), disimetriskan, bobot distandardisasi baris. Tetangga terdekat dipakai alih-alih persinggungan karena banyak wilayah kepulauan tidak bersinggungan. Signifikansi lokal α = 5% tanpa koreksi uji berganda.
- **Klaster:** variabel menceng (BABS, non-PLN, asbes, diare, DBD) ditransformasi `log1p`, seluruh variabel distandardisasi dan dipangkas pada ±2 SD, lalu dikelompokkan dengan Ward (`ward.D2`, jarak Euclid). Jumlah klaster 3–5 dapat dipilih pengguna; klaster diurutkan menurut PC1.
- **PCA:** `prcomp` pada matriks yang sama; PC1 diorientasikan searah kemiskinan.
- **Korelasi:** Spearman ρ dengan pasangan lengkap, karena n = 38 dan sebaran tidak normal. BABS dibuang dari klaster bila |ρ| dengan sanitasi > 0,85 (pada data ini ρ = −0,64, sehingga dipertahankan).

## Rancangan visual

- **Motif warna:** Backlog 1 selalu teal, Backlog 2 selalu ochre, di kartu angka, peta, treemap, icicle, dan sumbu kuadran. Merah bata hanya untuk kondisi lebih buruk atau naik.
- **Ramah buta warna:** peta dan hierarki memakai ramp satu rona yang dibedakan oleh terang-gelap, bukan rona. Wilayah tanpa data diberi abu-abu kebiruan yang kontrasnya ≥ 3,5:1 terhadap warna isi terang dan warna "tidak signifikan" pada LISA. Warna kategori pulau dan klaster selalu disertai label atau tooltip.
- **Kontras teks:** pasangan teks dan latar di antarmuka diukur dengan pemeriksa kontras WCAG dan memenuhi AA (≥ 4,5:1).
- **Heatmap berklaster:** indikator yang "tinggi = baik" dibalik dan diberi nama defisit (misalnya "Sanitasi tak layak"), sehingga merah bata selalu berarti lebih buruk dari rata-rata.
- **Tipografi dan bentuk:** Poppins untuk seluruh teks; kartu bersudut melengkung 20px. Alasan setiap keputusan ada di `DESIGN.md`.

## Struktur repositori

```
.
├── app.R              # UI (alur cerita) dan server
├── global.R           # paket, konfigurasi, memuat modul
├── src/
│   ├── helpers.R      # parsing angka, format, palet, tema plotly, catatan sumber
│   ├── data_prep.R    # membaca dan membersihkan data, definisi variabel
│   ├── analysis.R     # klaster, PCA, LISA
│   ├── plots.R        # fungsi pembangun visualisasi
│   └── nusa_geo.R     # cadangan pengambil batas wilayah via API (tidak dipakai bila .rds ada)
├── www/
│   ├── styles.css     # gaya laman
│   ├── app.js         # navigasi, bilah progres, menu ponsel
│   └── hero.jpg
├── data/              # data sumber dan batas wilayah olahan
├── DESIGN.md          # arah desain dan alasan keputusan
└── CATATAN-REVISI.md  # riwayat perbaikan logika dan visual
```


## Keterbatasan

- Hubungan antarprovinsi bersifat korelasional dan ekologis; tidak dapat ditarik ke tingkat rumah tangga.
- Angka penyakit juga mencerminkan kemampuan deteksi dan pelaporan, sehingga wilayah dengan layanan terbatas dapat tampak lebih baik daripada kenyataan.
- Indikator kesehatan hanya tersedia untuk satu tahun, sehingga perubahan 2025–2026 hanya dapat dianalisis untuk perumahan.
- Indikator kesehatan hanya tersedia di tingkat provinsi (n = 38), sehingga kekuatan uji terbatas.
- Hasil LISA bergantung pada pilihan tetangga (k = 6) dan tidak dikoreksi untuk uji berganda.
- Hanya lima program yang dapat dibandingkan pada periode setara; satu unit program tidak selalu sama dengan satu rumah tangga yang keluar dari backlog.
- Angka kab/kota Susenas memiliki galat sampling yang disajikan pada publikasi BPS (BPS Tabel 2.11–2.12) yang belum ditampilkan.

## Deklarasi penggunaan AI

Asisten AI dipakai sebagai alat bantu untuk merancang ulang antarmuka, meninjau kode, mengaudit desain, memeriksa konsistensi logika data. Seluruh keputusan analisis, isi, dan hasil akhir menjadi tanggung jawab penyusun.
