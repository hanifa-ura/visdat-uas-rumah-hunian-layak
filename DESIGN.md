# DESIGN.md: Hunian Layak Indonesia

Arah desain yang ditetapkan pemilik proyek, dicatat agar setiap perubahan berikutnya punya acuan.

## Identitas
- Produk: laporan data-story (R Shiny) tentang backlog perumahan dan kesehatan, 38 provinsi dan 514 kab/kota, 2025–2026.
- Pembaca: mahasiswa, dosen, dan pembaca kebijakan yang membaca dari atas ke bawah seperti artikel.
- Acuan suasana: 514-wajah-ekonomi.streamlit.app (hangat, angka besar, kartu melengkung, sumber kecil di bawah grafik). Dipakai sebagai inspirasi, bukan untuk ditiru.

## Palet (dari gambar sistem desain pemilik)
| Peran | Hex | Dipakai untuk |
|---|---|---|
| Primary (ochre) | #CA8A04 | Aksen utama: tombol utama, nav aktif, **semua hal tentang Backlog 2** |
| Secondary (slate) | #0F172A | Teks judul, hero, kartu akses layak |
| Tertiary (teal) | #0D9488 | **Semua hal tentang Backlog 1**, kondisi membaik |
| Neutral | #1E293B | Teks isi, kontrol terpilih |
| Kanvas | #F4F6FF | Latar halaman (warna latar swatch) |
| Semantik "memburuk" | #C2410C | Hanya di grafik (naik/di atas nasional, hotspot) |

Motif identitas: **Backlog 1 = teal, Backlog 2 = ochre**, konsisten di kartu angka, ramp warna peta, treemap, icicle, dan judul sumbu kuadran.
Grafik kategorikal (8 pulau, 3–5 klaster) memakai warna berbeda karena datanya kategorikal, bukan bagian palet antarmuka.

## Tipografi
Poppins untuk semua teks (permintaan eksplisit pemilik; menggantikan Domine/Space Grotesk di gambar swatch).
Alasan: satu keluarga huruf geometris yang tebal di angka besar dan tetap terbaca di teks panjang.

## Bentuk
- Kartu: sudut melengkung 20px (permintaan pemilik), garis tepi tipis, tanpa bayangan besar.
- Kontrol: 12px. Pil penuh hanya untuk chip pilihan ganda.
- Bayangan hanya untuk elemen yang benar-benar mengapung: header saat digulir, bar klaster saat menempel, dropdown, popup peta, tombol ke atas. Header tanpa blur (bukan glassmorphism).

## Dial
`Dial: ENERGY 2 / RHYTHM 2 / MOTION 1`
- ENERGY 2: hero gelap dengan foto, kartu angka berwarna penuh; sisanya tenang.
- RHYTHM 2: kartu grafik berstruktur sama, tetapi tata letaknya bervariasi: interpretasi di samping untuk peta dan grafik tinggi, di bawah dalam tiga kolom untuk grafik yang butuh lebar penuh; jeda berupa hero, kartu angka, kotak metode, pasangan kartu PCA, dan bab kesimpulan.
- MOTION 1: hanya hover, transisi header, panah accordion, dan gulir halus. Tidak ada animasi muncul atau hitung naik.

## Tema
Terang saja. Alasan: laporan dibaca dan dicetak untuk makalah, dan ramp warna peta/grafik dikalibrasi untuk latar terang.

## Keputusan (satu baris per keputusan, R-31)
- Foto hero (rumah bantaran sungai): menunjukkan kelayakhunian dan sanitasi sekaligus, inti dua topik laporan.
- Struktur kartu sama (judul, grafik, catatan baca, sumber, interpretasi, pertanyaan lanjutan): pembaca membandingkan 15 grafik dan tahu di mana mencari tiap bagian.
- Nomor grafik dan label jenis grafik tidak ditampilkan (permintaan pemilik); teks merujuk grafik dengan namanya.
- Judul kartu = pertanyaan yang dijawab grafik.
- Catatan baca ditulis sebagai catatan kaki gambar ("*Warna: ...") agar terbaca sebagai keterangan, bukan kotak instruksi.
- Interpretasi selalu tiga bagian (isi grafik, yang menonjol, kesimpulan); angkanya dihitung dari data agar ikut berubah saat pilihan diubah.
- Pertanyaan lanjutan dan definisi memakai details/summary: tersembunyi bagi yang tidak butuh, bisa dibuka dengan keyboard tanpa JavaScript.
- Bar klaster menempel hanya selama empat grafik yang memakai klaster terlihat (position: sticky di dalam pembungkusnya), karena pilihan klaster memengaruhi keempatnya.
- Kartu klaster memakai titik warna kecil, bukan garis tepi kiri, agar warnanya tetap berfungsi sebagai legenda tanpa menjadi dekorasi.
- Capaian program digambar sebagai baris HTML dengan skala per program: besaran program berbeda dari ratusan sampai ratusan ribu, sehingga satu sumbu bersama akan menyembunyikan program kecil.
- Hero hanya punya satu tombol (Mulai membaca); tombol kedua ke peta dihapus atas permintaan pemilik.
- Tombol menu ponsel berlabel "Menu", bukan ikon hamburger saja.
