# Catatan revisi (5 Oktober 2026, versi 4.1)

- Ketiga berkas Excel di folder `data` disusun ulang agar sama dengan publikasi: nilai perumahan dari *Statistik Perumahan 2026* (Tabel 2.1–2.10 dan 6.1), keluhan kesehatan dari *Statistik Kesehatan 2025*, diare dari *Profil Kesehatan Indonesia 2025*, sisanya dari tabel dinamis BPS. Angka disimpan sebagai bilangan, sel kosong = tidak disajikan sumber, dan tiap berkas punya sheet Keterangan.
- Nama kolom yang berubah: `rumah_layak_huni` menjadi `rumah_layak_huni_2025` (ditambah `_2026`), `tbc_n` menjadi `tbc_cakupan_pct`; kolom turunan `tbc_pct`, `dbd_n`, `dbd_pct` dihapus; komponen dan penerangan 2026 per provinsi ditambahkan; capaian mendapat kolom Kelompok dan Sumber data.
- Kode membaca berkas tersebut langsung. Tabel rujukan CSV dan koreksi tukar kolom dihapus karena tidak diperlukan lagi. Fitur unduh kini menyajikan ketiga berkas apa adanya.

# Catatan revisi versi 4.0

## A. Masalah data yang kini diselesaikan di R (Excel tidak diubah)

Saya membandingkan berkas Excel dengan PDF *Statistik Perumahan 2026* sel per sel. Tabel PDF konsisten: jumlah dibagi persentase menghasilkan total rumah tangga yang sama untuk Backlog 1 dan Backlog 2 di semua 496 kab/kota yang lengkap. Berkas Excel tidak:

| Berkas | Masalah | Dampak sebelumnya |
|---|---|---|
| backlog_kabkota.xlsx | Kolom Backlog 1 (persen dan jumlah) bergeser 2–3 baris terhadap tabel BPS; nilai suatu kab/kota tercatat di baris kab/kota lain (394 sel persentase, 414 sel jumlah berbeda) | Peta, popup, dan LISA Backlog 1 memakai nilai kab/kota yang salah; 309 baris tampak "tidak konsisten" |
| backlog_kabkota.xlsx | Jumlah Backlog 2 tertukar antartahun di 232 baris | Total RT yang dihitung dari Backlog 2 salah, sehingga Backlog 1 ikut tampak tidak konsisten |
| dataset(provinsi).xlsx | Jumlah Backlog 1 tertukar antartahun di 25 baris (sudah diatasi sebelumnya) | |
| dataset(provinsi).xlsx | Kolom listrik non-PLN tertukar dengan kolom bukan listrik (mis. Jambi non-PLN 1,28%, di Excel 0,32%) | Variabel non-PLN di klaster, PCA, heatmap, dan peringkat salah |

Solusi: tabel rujukan diekstrak dari PDF (Tabel 2.1–2.10) ke `data/rujukan/*.csv`. `data_prep.R` membaca Excel apa adanya lalu mengganti nilainya dengan nilai rujukan menurut kode wilayah, dan mencatat jumlah sel yang diganti (tampil di Bab 6, accordion "Catatan data"). Bila folder `rujukan` dihapus, aplikasi kembali ke koreksi otomatis lama. Bonus dari tabel rujukan: komponen rumah layak 2026 per provinsi tersedia, sehingga dumbbell komponen kini bisa dipilih per provinsi.

Status butir lain dari daftar:
- Kode wilayah peta: tetap digabung lewat nama, 514 dari 514 poligon cocok.
- BABS vs sanitasi: ρ = −0,64, keduanya dipertahankan.
- Nilai kosong: sel NA di tabel BPS (asbes, BABS) dibaca 0, non-PLN diisi sisa, DBD diisi median hanya untuk klaster/PCA; semuanya kini ditandai × di heatmap dengan keterangan cara pengisiannya.
- Konsistensi B1 kab/kota: setelah koreksi tidak ada lagi baris yang tidak konsisten.
- TBC: tetap memakai tbc_n sebagai cakupan penemuan kasus (%).

## B. Perubahan tampilan dan isi

- Label "#1 · Kartu angka" dan baris "Zoom: ..." dihapus. Petunjuk overlay zoom di peta juga dihapus.
- "Cara membaca" diganti catatan kaki gambar: `*Warna: ...`, `*Ukuran: ...`.
- Pertanyaan lanjutan dan definisi (komponen rumah layak, LISA, Spearman, skor-z, PCA, wilayah abu-abu, program yang tidak dibandingkan) menjadi accordion.
- Interpretasi lebih panjang dan selalu tiga bagian: isi grafik, yang menonjol, kesimpulan. Pada peta dan grafik tinggi posisinya di samping grafik (layar ≥ 1100 px); pada grafik lebar posisinya di bawah dalam tiga kolom.
- Icicle kini dari atas ke bawah (Indonesia, pulau, provinsi), dengan teks porsi dan % Backlog 1 di tiap kotak; kab/kota terbuka saat provinsi diklik.
- Bar klaster (jumlah klaster, legenda, hapus sorotan) menempel di bawah header hanya selama empat grafik Bab 4 terlihat.
- Capaian program: hanya lima program yang bisa dibandingkan pada periode setara (FLPP, GWM BI, retribusi PBG, CSR, rumah susun baru); alasan program lain ada di accordion.
- Bab 6 "Catatan Data" diganti "Kesimpulan": pola utama, yang perlu digarisbawahi, keterbatasan dan saran pengembangan. Semua angka dihitung dari data.
- Bagian "Unduh data": tiga berkas Excel, versi terkoreksi (butuh paket writexl; tanpa itu CSV) dan berkas asli.
- Footer berisi identitas penyusun, mata kuliah, kampus, dan tanggal; isi `AUTHOR` di `global.R`.
- Tombol "Langsung ke peta" di hero dihapus; navigasi titik di kanan layar dihapus (sudah ada menu header).

## C. Yang perlu dicek pemilik
- Isi `AUTHOR` di `global.R` (nama, NIM, kelas, email, tanggal).
- Pasang `writexl` agar unduhan terkoreksi berformat .xlsx.
- Angka di makalah yang berasal dari data kab/kota dan non-PLN perlu disesuaikan (lihat ringkasan di percakapan).

---

# Catatan revisi (5 Oktober 2026)

Tanggapan atas daftar revisi, plus kekeliruan tambahan yang ditemukan saat mengecek ulang data asli.

## A. Tanggapan per poin

| Poin revisi | Sikap | Alasan dan tindakan |
|---|---|---|
| 1. TBC jumlah mutlak, ubah ke per 100.000 | **Masalahnya benar, diagnosisnya keliru** | `tbc_n` bukan jumlah kasus: nilainya 41–112 per provinsi, nasional 79, padahal kasus TBC nasional ratusan ribu per tahun. Jawa Barat (97) hampir sama dengan Kalimantan Utara (64) walau penduduknya 68 kali lipat. Pola dan besarnya sesuai **cakupan penemuan dan pengobatan kasus TBC (%)**: Banten 112 (cakupan bisa > 100), Papua Barat 88 dan peringkat 3; berita 2024 menyebut Papua Barat peringkat ke-4 cakupan penemuan. Membagi dengan penduduk justru merusak angka. Tindakan: label menjadi "Cakupan penemuan kasus TBC (%)", arah dibalik (tinggi = baik) di #3, #9, dan interpretasi #7. **Konfirmasi definisi ke tabel sumber.** |
| 2. Warna heatmap #9 bermakna ganda | Setuju | Indikator "tinggi = baik" dibalik dan diberi nama defisit ("Sanitasi tak layak", "Cakupan TBC rendah"). Merah bata selalu = lebih buruk, hijau toska = lebih baik. |
| 3. Imputasi tampil seperti data asli | Setuju, dengan koreksi | Yang kosong hanya **DBD** untuk Papua Barat Daya dan Papua Pegunungan; TBC tidak kosong (catatan lama keliru menyebut "TBC dan DBD"). Sel imputasi kini ditandai × di #9 dan disebut di tooltip #10b. |
| 4. Ukuran gelembung kuadran #14 | Setuju | Ukuran diganti jumlah Backlog 1 + Backlog 2 (rumah tangga ber-backlog), teks "Cara membaca" dan interpretasi menyesuaikan. Asumsi: B1 dan B2 tidak tumpang tindih, sesuai definisi di Bab 1. |
| 5. Warna LISA "tidak signifikan" vs "tanpa data" | Setuju | Di versi terakhir keduanya #D8D2C6 vs #C9CED8. Kini krem sangat muda vs abu-abu kebiruan tua (#727B8E), beda kontras ≥ 3,5:1 terhadap semua warna isi peta. |
| 6. #13 periode tidak setara, klaim judul tidak terlihat | Setuju | Batang kini Semester I 2025 vs Semester I 2026; total setahun 2025 jadi penanda ◆. Ditambah bilah skala: BSPS 2025 = 0,24% dari Backlog 2, FLPP 2025 = 2,89% dari Backlog 1, sehingga judul bab terbukti di grafik. |
| 7. Garis paralel #8 nilai mentah | Setuju (diselesaikan dengan penggantian #8) | Lihat bagian B. |
| 8. Palet peta viridis | Sudah beres sebelumnya | Sejak audit antislop, peta, treemap, dan icicle memakai ramp teal (Backlog 1) dan ochre (Backlog 2). |
| Gabung #11 Treemap + #12 Icicle | **Tidak setuju** | Draf mensyaratkan 15 visualisasi; menggabungkan berarti tinggal 14. Keduanya juga menjawab pertanyaan berbeda: #11 ukuran = total rumah tangga, warna = %B2 (beban per populasi); #12 ukuran = jumlah B2, warna = %B1 (beban ganda). |
| Ganti #8 dengan heatmap korelasi Spearman | Setuju | #8 kini heatmap korelasi indikator hunian × indikator kesehatan (+ kemiskinan sebagai pembanding), menjawab langsung pertanyaan Bab 4. |
| Ganti scree #10a dengan bar muatan | Setuju | Muatan menjelaskan isi sumbu biplot; persentase variasi tetap di teks. Tetap dua kartu berdampingan. |
| Ganti #15 slope dengan dumbbell perubahan provinsi | Setuju | Slope mengulang pesan #5. #15 kini perubahan B1/B2 2025→2026 per provinsi, dipindah ke Bab 1. |
| Profil klaster muncul 3 kali | Setuju sebagian | Kartu (nama, ciri, anggota) dan tabel (rata-rata angka) tetap karena isinya berbeda; interpretasi #9 kini membaca pola heatmap, bukan mengulang profil. |
| Kartu #1 dan hero mengulang angka | Dibiarkan | Hero adalah ringkasan; #1 memberi perbandingan 2025 dan konteks. Pengulangan wajar untuk pembuka. |

Jumlah visualisasi tetap 15 (#1–#15; #10a dan #10b dihitung satu).

## B. Kekeliruan tambahan yang ditemukan

1. **Backlog 1 nasional 2026 tampil 9,64 juta, seharusnya 9,29 juta.** Koreksi "jumlah B1 tertukar" hanya menukar bila versi asli meleset > 5%. Baris nasional meleset 3,8% sehingga tidak ditukar, padahal versi tukarnya cocok 0,04%. Hal yang sama terjadi pada 15 provinsi lain (termasuk Jawa Barat) dan 7 kab/kota. Aturan diperbaiki: tukar bila versi tukar < 5% **dan** galatnya < 1/4 versi asli. Bukti: jumlah B1 2026 dari 38 provinsi kini 9.289,1 ribu = nasional 9.289,05 ribu, sama dengan rilis BPS. Hero, #1, #14, dan Catatan Data ikut benar.
2. **Judul Bab 4 bertentangan dengan data.** "Hunian kurang layak bergerak bersama masalah kesehatan" tidak didukung: dari 12 pasangan hunian × kesehatan yang signifikan, hanya 3 searah dugaan, 9 berlawanan (mis. sanitasi layak × DBD ρ = +0,61; air layak × diare ρ = +0,43). Judul dan pengantar diganti; interpretasi #8 menghitung arah ini otomatis dan menyebut penjelasan yang masuk akal (pelaporan dan kepadatan kota).
3. **#7 menulis arah hubungan walau tidak signifikan.** Kini kalimat arah hanya muncul bila p < 0,05.
4. **#3 mewarnai "di atas nasional" selalu merah.** Untuk cakupan TBC (tinggi = baik) warnanya kini dibalik.
5. **Non-PLN "NAN" diisi 0.** Kini diisi sisa 100 − PLN − bukan listrik (0,00–0,20%), karena ketiganya berjumlah 100.
6. **`LISA_COLS` didefinisikan dua kali** (analysis.R dan plots.R) dengan nilai berbeda; definisi ganda dihapus.
7. **Interpretasi #15 menulis "turun +0,61 poin"** (tanda ganda); diperbaiki.

## C. Yang perlu dikonfirmasi pemilik
- Definisi `tbc_n` (cakupan penemuan dan pengobatan kasus TBC) ke tabel sumber.
- Bahwa Backlog 1 dan Backlog 2 tidak tumpang tindih (dipakai untuk ukuran gelembung #14).
- URL tabel BPS dan tanggal akses di `SRC` (src/helpers.R).
