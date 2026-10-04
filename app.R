# ==== [A1] paket dan opsi ====
suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(plotly)
  library(leaflet)
  library(sf)
  library(dplyr)
  library(heatmaply)
  library(viridisLite)
  library(DT)
})
options(scipen = 999, stringsAsFactors = FALSE)

# ==== [A2] konfigurasi ====
K_LISA           <- 5L
NSIM_LISA        <- 999L
ALPHA            <- 0.05
Z_PENCILAN       <- 2.5
MIN_VAR_PENCILAN <- 2L
K_KLASTER        <- 4L
SEED             <- 20261003L

PATH_RAW  <- "data/raw"
PATH_RDS  <- "data/olahan.rds"
PATH_CSV  <- "data/olahan"

VAR_MV    <- c("Informal", "TPAK-L", "TPAK-P", "TPT", "Upah Formal", "RLS",
               "%Miskin", "Upah/Jam", "Dikdas", "IPG")
VAR_PERSEN <- c("Informal", "TPAK-L", "TPAK-P", "TPT", "%Miskin", "Dikdas", "IPG")

VAR_Y        <- "Informal"                 # respons (Y); tidak ikut PCA maupun pembentukan klaster
VAR_X        <- setdiff(VAR_MV, VAR_Y)     # 9 faktor (X)
ALPHA_NORMAL <- 0.05
ALPHA_REG    <- 0.10
VIF_BATAS    <- 10
K_PC         <- NA                         # NA = otomatis (eigenvalue > 1); isi bilangan bulat untuk menimpa

SUMBER_SAKERNAS  <- "BPS, Sakernas Agustus 2025"
PERIODE_SAKERNAS <- "Agustus 2025"
SUMBER_PETA <- "BPS, Publikasi Keadaan Angkatan Kerja tiap provinsi (Sakernas Agustus 2025); batas wilayah: Lapak GIS 2024"
SUMBER_BAB3 <- "BPS, tabel dinamis Sakernas/Susenas 2025 dan Statistik Pendidikan Indonesia (Dikdas); lihat README"
URL_REPO    <- "https://github.com/Zidan-NDZ/uas-visdat-informal.git"

OKABE <- c(oranye = "#E69F00", biru_langit = "#56B4E9", hijau = "#009E73", kuning = "#F0E442",
           biru = "#0072B2", vermilion = "#D55E00", ungu = "#CC79A7", hitam = "#000000")
WARNA_LATAR  <- OKABE[["biru_langit"]]
WARNA_NAVBAR <- OKABE[["biru"]]
WARNA_FONT   <- OKABE[["hitam"]]

PALET_DIV <- grDevices::colorRampPalette(c(OKABE[["oranye"]], "#EDEDED", OKABE[["biru"]]))(11)

PALET_URUT5 <- c("#F0E442", "#3CB067", "#008892", "#006299", "#00314F")
WARNA_ABU  <- "#BDBDBD"
PALET_KLASTER <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#0072B2", "#D55E00")
SIMBOL_KLASTER <- c("circle", "square", "diamond", "triangle-up", "x", "cross")
WARNA_LISA <- c(HH = OKABE[["vermilion"]], LL = OKABE[["biru"]], HL = OKABE[["oranye"]],
                LH = OKABE[["biru_langit"]], ns = "#E0E0E0")
LABEL_LISA <- c(HH = "Tinggi\u2013tinggi (HH)", LL = "Rendah\u2013rendah (LL)",
                HL = "Tinggi dikelilingi rendah (HL)", LH = "Rendah dikelilingi tinggi (LH)",
                ns = "Tidak signifikan")

WARNA_LUAR_FILTER <- "#E3E3E3"
WARNA_NETRAL      <- "#D5DBE3"
WARNA_SIMBOL      <- "#5B6B7F"
R_MAKS_SIMBOL     <- 28
BBOX_INDONESIA    <- c(95, -11.5, 141.5, 6.5)

# ==== [A3] fungsi murni ====

fmt_id <- function(x, digits = 0) {
  out <- rep("\u2013", length(x))
  ok <- !is.na(x)
  if (any(ok)) {
    out[ok] <- formatC(round(as.numeric(x[ok]), digits), format = "f", digits = digits,
                       big.mark = ".", decimal.mark = ",")
  }
  out
}

pct_perempuan <- function(df) {
  ifelse(is.na(df$juta_perempuan), NA_real_, df$juta_perempuan / df$juta_orang * 100)
}

titik_tengah_perempuan <- function(sunburst) {
  sunburst$pct_perempuan[sunburst$id == "bekerja"]
}

kpi_beranda <- function(dendrogram, sunburst) {
  usia     <- dendrogram$juta_orang[dendrogram$id == "usia_kerja"]
  bekerja  <- dendrogram$juta_orang[dendrogram$id == "bekerja"]
  informal <- sunburst$juta_orang[sunburst$id == "informal"]
  list(usia_kerja = usia, bekerja = bekerja, informal = informal,
       pct_informal = round(informal / bekerja * 100, 1))
}

action_title_beranda <- function(kpi) {
  awal <- if (kpi$pct_informal > 50) "Lebih dari separuh pekerja Indonesia" else "Sebagian pekerja Indonesia"
  sprintf("%s (%s%%) bekerja secara informal.", awal, fmt_id(kpi$pct_informal, 1))
}

build_sunburst_data <- function(sunburst_df) {
  df <- sunburst_df
  is_root <- is.na(df$induk)
  idx_induk <- match(df$induk, df$id)
  total_root <- sum(df$juta_orang[is_root])
  persen_induk <- df$juta_orang / df$juta_orang[idx_induk] * 100
  persen_usia  <- df$juta_orang / total_root * 100
  label_induk  <- df$label[idx_induk]
  daun_jk <- df$label %in% c("Laki-laki", "Perempuan")
  pct <- df$pct_perempuan
  
  baris_induk <- ifelse(is_root, "", sprintf("<br>%s%% dari %s", fmt_id(persen_induk, 1), label_induk))
  baris_usia  <- ifelse(is_root, "", sprintf("<br>%s%% dari penduduk usia kerja", fmt_id(persen_usia, 1)))
  baris_jk <- ifelse(daun_jk, "",
                     ifelse(is.na(pct), "<br><i>Rincian jenis kelamin tidak tersedia</i>",
                            sprintf("<br>%s%% perempuan", fmt_id(pct, 1))))
  
  list(
    ids = df$id,
    labels = df$label,
    parents = ifelse(is_root, "", df$induk),
    values = df$juta_orang,
    pct_perempuan = pct,
    text = sprintf("%s juta", fmt_id(df$juta_orang, 2)),
    hover = sprintf("<b>%s</b><br>%s juta orang%s%s%s",
                    df$label, fmt_id(df$juta_orang, 2), baris_induk, baris_usia, baris_jk)
  )
}

jalur_simpul <- function(df, id) {
  out <- character()
  cur <- id
  while (!is.na(cur)) {
    out <- c(cur, out)
    cur <- df$induk[match(cur, df$id)]
  }
  out
}

warna_pct_perempuan <- function(pct, mid, lo = 0, hi = 100,
                                palet = PALET_DIV, abu = WARNA_ABU) {
  t <- ifelse(is.na(pct), NA_real_,
              ifelse(pct >= mid,
                     0.5 + 0.5 * (pct - mid) / (hi - mid),
                     0.5 - 0.5 * (mid - pct) / (mid - lo)))
  t <- pmin(pmax(t, 0), 1)
  out <- rep(abu, length(pct))
  ok <- !is.na(t)
  if (any(ok)) {
    m <- grDevices::colorRamp(palet)(t[ok])
    out[ok] <- grDevices::rgb(m[, 1], m[, 2], m[, 3], maxColorValue = 255)
  }
  out
}

css_gradien_puor <- function(mid, palet = PALET_DIV) {
  h <- (length(palet) + 1) / 2
  pos <- c(seq(0, mid, length.out = h), seq(mid, 100, length.out = h)[-1])
  paste0("linear-gradient(to right, ",
         paste0(palet, " ", sprintf("%.2f%%", pos), collapse = ", "), ")")
}

KOLOM_BATAS_KANDIDAT <- list(
  nama     = c("NAMOBJ", "WADMKK", "KAB_KOTA", "KABKOT", "NAMA_KAB", "NM_KAB", "KABUPATEN", "NAME_2", "NAMA"),
  provinsi = c("WADMPR", "PROVINSI", "NAMA_PROV", "NM_PROV", "PROV", "NAME_1"),
  kode     = c("KDPKAB", "KODE_KK", "KDKAB", "KODE_KAB", "KD_KABKOT", "ADM2_PCODE", "KODE")
)

deteksi_kolom_batas <- function(kolom, override = list()) {
  hasil <- lapply(names(KOLOM_BATAS_KANDIDAT), function(k) {
    o <- override[[k]]
    if (!is.null(o) && !is.na(o)) {
      if (!(o %in% kolom)) stop(sprintf("Kolom '%s' (%s) tidak ada. Kolom tersedia: %s", o, k, paste(kolom, collapse = ", ")))
      return(o)
    }
    cocok <- kolom[match(toupper(KOLOM_BATAS_KANDIDAT[[k]]), toupper(kolom))]
    cocok <- cocok[!is.na(cocok)]
    if (!length(cocok)) stop(sprintf("Kolom '%s' tidak terdeteksi. Kolom tersedia: %s. Isi KOLOM_OVERRIDE di skrip.",
                                     k, paste(kolom, collapse = ", ")))
    cocok[1]
  })
  stats::setNames(hasil, names(KOLOM_BATAS_KANDIDAT))
}

kunci_nama <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("administrasi", "adm", x, fixed = TRUE)
  x <- gsub("\\badm\\b\\.?", " ", x)
  x <- gsub("\\bkabupaten\\b|\\bkab\\b\\.?", " ", x)
  x <- gsub("\\bkep\\b\\.?", "kepulauan", x)
  x <- gsub("\\bdan\\b", " ", x)
  gsub("[^a-z0-9]", "", x)
}

# "91.2" -> "91.20" (nol di belakang hilang karena disimpan sebagai angka)
normalisasi_kode_bps <- function(x) {
  x <- trimws(as.character(x))
  vapply(strsplit(x, ".", fixed = TRUE), function(p) {
    if (length(p) == 2 && nzchar(p[2])) paste0(p[1], ".", paste0(p[2], strrep("0", max(0, 2 - nchar(p[2]))))) else paste(p, collapse = ".")
  }, character(1))
}

tumpang_tindih <- function(a, b) {
  # planar (derajat^2) cukup untuk rasio tumpang tindih; menghindari ketergantungan s2/lwgeom
  a <- sf::st_set_crs(sf::st_make_valid(a), NA); b <- sf::st_set_crs(sf::st_make_valid(b), NA)
  ia <- suppressWarnings(sf::st_intersection(a, b))
  if (length(ia) == 0) return(0)
  as.numeric(sum(sf::st_area(ia))) / min(as.numeric(sf::st_area(a)), as.numeric(sf::st_area(b)))
}

luas_planar <- function(g) as.numeric(sf::st_area(sf::st_make_valid(sf::st_set_crs(g, NA))))

bbox_dalam <- function(frag, utama, tol = 0.1) {
  f <- sf::st_bbox(sf::st_set_crs(frag, NA)); u <- sf::st_bbox(sf::st_set_crs(utama, NA))
  unname(f["xmin"] >= u["xmin"] - tol && f["ymin"] >= u["ymin"] - tol &&
           f["xmax"] <= u["xmax"] + tol && f["ymax"] <= u["ymax"] + tol)
}

jarak_planar <- function(a, b) {
  a <- sf::st_make_valid(sf::st_set_crs(a, NA)); b <- sf::st_make_valid(sf::st_set_crs(b, NA))
  min(as.numeric(sf::st_distance(a, b)))
}

gabung_geometri <- function(g) {
  crs <- sf::st_crs(g)
  u <- sf::st_union(sf::st_set_crs(sf::st_make_valid(g), NA))
  sf::st_set_crs(u, crs)
}

info_duplikat <- function(shp, idx) {
  g <- sf::st_set_crs(sf::st_geometry(shp)[idx], NA)
  luas <- as.numeric(sf::st_area(sf::st_make_valid(g)))
  sprintf("      kode %s | %s | luas %.4f derajat^2 | bbox [%s]", shp$kode_kabkota[idx], shp$provinsi_shp[idx], luas,
          vapply(seq_along(idx), function(i) paste(round(as.numeric(sf::st_bbox(g[i])), 2), collapse = ", "), character(1)))
}

clean_batas <- function(shp, n_expected = 514L,
                        gabungan = "Pahuwato",   # salah eja; nama gabungan ber-"/" dibuang otomatis
                        pilih_kode = list(Nabire = "94", Paniai = "94", Deiyai = "94"),
                        pilih_prov = list(Sorong = "Papua Barat Daya"),
                        min_tumpang = 0.8, maks_jarak = 0.1) {
  stopifnot(all(c("kode_kabkota", "nama", "provinsi_shp") %in% names(shp)))
  shp$nama <- trimws(as.character(shp$nama))
  shp <- shp[!is.na(shp$nama) & nzchar(shp$nama), ]
  shp <- shp[!(shp$nama %in% gabungan) & !grepl("/", shp$nama, fixed = TRUE), ]
  shp$kode_kabkota <- normalisasi_kode_bps(shp$kode_kabkota)
  shp$provinsi_shp <- trimws(as.character(shp$provinsi_shp))
  
  buang <- integer(); masalah <- character()
  for (nm in unique(shp$nama[duplicated(shp$nama)])) {
    idx <- which(shp$nama == nm)
    geom <- function(i) sf::st_geometry(shp)[i]
    
    if (!is.null(pilih_kode[[nm]]) || !is.null(pilih_prov[[nm]])) {
      keep <- if (!is.null(pilih_kode[[nm]])) idx[startsWith(shp$kode_kabkota[idx], paste0(pilih_kode[[nm]], "."))]
      else idx[shp$provinsi_shp[idx] == pilih_prov[[nm]]]
      if (length(keep) != 1) {
        masalah <- c(masalah, sprintf("Duplikat '%s': aturan pemilihan menghasilkan %d baris, harusnya 1.", nm, length(keep)))
        next
      }
    } else if (length(unique(shp$kode_kabkota[idx])) == 1) {
      keep <- idx[which.max(luas_planar(geom(idx)))]
    } else {
      masalah <- c(masalah, sprintf("Duplikat '%s' tidak punya aturan pemilihan (kode: %s).", nm,
                                    paste(shp$kode_kabkota[idx], collapse = ", ")))
      next
    }
    
    gabung <- integer(); jauh <- FALSE
    for (j in setdiff(idx, keep)) {
      if (tumpang_tindih(geom(keep), geom(j)) >= min_tumpang) next
      if (bbox_dalam(geom(j), geom(keep), maks_jarak) ||
          jarak_planar(geom(j), geom(keep)) <= maks_jarak) gabung <- c(gabung, j) else jauh <- TRUE
    }
    if (jauh) {
      masalah <- c(masalah, sprintf("Duplikat '%s': geometri berbeda wilayah (tidak tumpang tindih dan terletak jauh dari poligon utama).\n%s",
                                    nm, paste(info_duplikat(shp, idx), collapse = "\n")))
      next
    }
    if (length(gabung)) {
      message(sprintf("Duplikat '%s': %d serpihan digabung ke poligon utama (kode %s, %s).\n%s",
                      nm, length(gabung), shp$kode_kabkota[keep], shp$provinsi_shp[keep],
                      paste(info_duplikat(shp, c(keep, gabung)), collapse = "\n")))
      sf::st_geometry(shp)[keep] <- gabung_geometri(geom(c(keep, gabung)))
    }
    buang <- c(buang, setdiff(idx, keep))
  }
  if (length(masalah)) stop(paste(c("clean_batas: ada duplikat yang tidak bisa diselesaikan:", masalah), collapse = "\n"))
  if (length(buang)) shp <- shp[-buang, ]
  if (nrow(shp) != n_expected) stop(sprintf("clean_batas: %d fitur, diharapkan %d.", nrow(shp), n_expected))
  if (anyDuplicated(shp$kode_kabkota)) stop("clean_batas: kode_kabkota tidak unik: ", paste(shp$kode_kabkota[duplicated(shp$kode_kabkota)], collapse = ", "))
  shp
}

join_kabkota <- function(xlsx_df, batas_sf, kode_prov_tbl, alias = NULL) {
  kb <- kunci_nama(batas_sf$nama)
  if (!is.null(alias) && nrow(alias)) {
    ka <- match(kb, kunci_nama(alias$nama_batas))
    kb[!is.na(ka)] <- kunci_nama(alias$nama_xlsx[ka[!is.na(ka)]])
  }
  kx <- kunci_nama(xlsx_df$nama)
  if (anyDuplicated(kx) || anyDuplicated(kb)) stop("Kunci nama tidak unik setelah normalisasi: ",
                                                   paste(unique(c(xlsx_df$nama[kx %in% kx[duplicated(kx)]], batas_sf$nama[kb %in% kb[duplicated(kb)]])), collapse = ", "))
  tak_batas <- batas_sf$nama[!(kb %in% kx)]
  tak_xlsx  <- xlsx_df$nama[!(kx %in% kb)]
  if (length(tak_batas) || length(tak_xlsx))
    stop("Nama tidak cocok.\n  Hanya di shapefile: ", paste(tak_batas, collapse = "; "),
         "\n  Hanya di Excel: ", paste(tak_xlsx, collapse = "; "),
         "\n  Tambahkan baris ke data/raw/alias_nama.csv (nama_batas,nama_xlsx).")
  ix <- match(kb, kx)
  ip <- match(batas_sf$provinsi_shp, kode_prov_tbl$provinsi_shp)
  if (anyNA(ip)) stop("Provinsi shapefile tak ada di kode_provinsi.csv: ", paste(unique(batas_sf$provinsi_shp[is.na(ip)]), collapse = "; "))
  out <- sf::st_sf(sf::st_drop_geometry(batas_sf)[, 0], geometry = sf::st_geometry(batas_sf))
  out <- out[seq_len(nrow(batas_sf)), ]
  out$kode_kabkota  <- batas_sf$kode_kabkota
  out$kode_prov     <- kode_prov_tbl$kode_prov[ip]
  out$nama          <- xlsx_df$nama[ix]
  out$provinsi      <- kode_prov_tbl$provinsi_multivariat[ip]
  out$informal      <- xlsx_df$informal[ix]
  out$total_pekerja <- xlsx_df$total_pekerja[ix]
  out$rasio         <- out$informal / out$total_pekerja
  q <- stats::quantile(out$rasio, c(.25, .75)); iqr <- q[2] - q[1]
  out$flag_ekstrem  <- out$rasio < q[1] - 1.5 * iqr | out$rasio > q[2] + 1.5 * iqr
  out[, c("kode_kabkota", "kode_prov", "nama", "provinsi", "informal", "total_pekerja", "rasio", "flag_ekstrem")]
}

agg_provinsi <- function(kabkota_df) {
  if (inherits(kabkota_df, "sf")) kabkota_df <- sf::st_drop_geometry(kabkota_df)
  s <- stats::aggregate(cbind(informal, total_pekerja) ~ kode_prov, data = kabkota_df, FUN = sum)
  tibble::tibble(kode_prov = s$kode_prov, informal_pct = s$informal / s$total_pekerja * 100)
}

classify_breaks <- function(x, style = c("jenks", "quantile", "equal"), n = 5L) {
  style <- match.arg(style)
  x <- x[!is.na(x)]
  if (length(unique(x)) <= n) stop("classify_breaks: nilai unik harus lebih banyak dari jumlah kelas")
  b <- switch(style,
              jenks    = as.numeric(classInt::classIntervals(x, n, style = "jenks")$brks),
              quantile = as.numeric(stats::quantile(x, probs = seq(0, 1, length.out = n + 1), names = FALSE)),
              equal    = seq(min(x), max(x), length.out = n + 1))
  b[1] <- min(b[1], min(x)); b[length(b)] <- max(b[length(b)], max(x))
  b
}

kelas_dari_breaks <- function(x, breaks) {
  findInterval(x, breaks, rightmost.closed = TRUE, all.inside = TRUE)
}

label_kelas <- function(breaks, digits = 1) {
  n <- length(breaks) - 1
  sprintf("%s\u2013%s", fmt_id(breaks[-(n + 1)] * 100, digits), fmt_id(breaks[-1] * 100, digits))
}

radius_simbol <- function(x, r_max = 28, x_max = max(x, na.rm = TRUE), r_min = 2) {
  pmax(r_min, r_max * sqrt(x / x_max))
}

dalam_filter <- function(df, kode_prov = "semua", rentang_pct = c(0, 100)) {
  ok <- df$rasio * 100 >= rentang_pct[1] - 1e-9 & df$rasio * 100 <= rentang_pct[2] + 1e-9
  if (!is.null(kode_prov) && !identical(kode_prov, "semua")) ok <- ok & df$kode_prov == kode_prov
  ok
}

teks_tooltip <- function(df) {
  ekstrem <- ifelse(df$flag_ekstrem, "<br><b>\u26A0 Nilai ekstrem</b> (di luar 1,5 \u00D7 IQR)", "")
  sprintf("<b>%s</b><br>%s<br>Rasio informal: %s%%<br>Pekerja informal: %s orang<br>Total pekerja: %s orang%s",
          df$nama, df$provinsi, fmt_id(df$rasio * 100, 1), fmt_id(df$informal), fmt_id(df$total_pekerja), ekstrem)
}

ringkas_kabkota <- function(df, kode) {
  i <- match(kode, df$kode_kabkota)
  if (is.na(i)) return(NULL)
  prov <- df[df$kode_prov == df$kode_prov[i], ]
  list(nama = df$nama[i], provinsi = df$provinsi[i], rasio = df$rasio[i],
       informal = df$informal[i], total = df$total_pekerja[i],
       peringkat = rank(-df$rasio, ties.method = "min")[i], n = nrow(df),
       rata_prov = sum(prov$informal) / sum(prov$total_pekerja),
       rata_nasional = sum(df$informal) / sum(df$total_pekerja),
       ekstrem = isTRUE(df$flag_ekstrem[i]))
}

top_n_kabkota <- function(df, n = 10L, tinggi = TRUE) {
  o <- order(if (tinggi) -df$rasio else df$rasio, df$nama)[seq_len(min(n, nrow(df)))]
  data.frame(peringkat = seq_along(o), nama = df$nama[o], provinsi = df$provinsi[o], rasio = df$rasio[o],
             stringsAsFactors = FALSE)
}

judul_peta <- function(df) {
  sprintf("Rasio pekerja informal berkisar %s%%\u2013%s%% antar kab/kota, dan %s dari %s kab/kota berada di atas 50%%",
          fmt_id(min(df$rasio) * 100, 1), fmt_id(max(df$rasio) * 100, 1),
          fmt_id(sum(df$rasio > 0.5)), fmt_id(nrow(df)))
}

html_legenda_simbol <- function(refs, x_max, r_max = 28) {
  r <- radius_simbol(refs, r_max = r_max, x_max = x_max)
  item <- sprintf('<div class="lg-simbol"><span class="lg-bulat" style="width:%dpx;height:%dpx;"></span><span>%s</span></div>',
                  round(2 * r), round(2 * r), fmt_id(refs))
  paste0('<div class="legenda-simbol"><b>Pekerja informal (orang)</b><br><small>Luas lingkaran sebanding dengan jumlah</small>',
         paste(item, collapse = ""), "</div>")
}

compute_lisa <- function(sf_obj, k = 5L, nsim = 999L, seed = 1L, alpha = 0.05,
                         var = "rasio", id = "kode_kabkota") {
  x <- sf_obj[[var]]
  old <- suppressMessages(sf::sf_use_s2(FALSE)); on.exit(suppressMessages(sf::sf_use_s2(old)), add = TRUE)
  ctr <- suppressWarnings(sf::st_point_on_surface(sf::st_geometry(sf_obj)))
  xy  <- sf::st_coordinates(ctr)[, 1:2, drop = FALSE]
  nb  <- spdep::knn2nb(spdep::knearneigh(xy, k = k, longlat = TRUE))
  lw  <- spdep::nb2listw(nb, style = "W")
  
  set.seed(seed); mc <- spdep::moran.mc(x, lw, nsim = nsim)
  set.seed(seed); lm <- as.data.frame(spdep::localmoran_perm(x, lw, nsim = nsim))
  
  # Nama kolom p-value permutasi berbeda antarversi spdep -> cari yang berakhiran "Sim"
  kol_sim <- grep("Sim$", names(lm), value = TRUE)
  if (!length(kol_sim)) stop("compute_lisa: kolom p-value permutasi tidak ditemukan. Kolom: ", paste(names(lm), collapse = ", "))
  kol_p <- if ("Pr(z != E(Ii)) Sim" %in% kol_sim) "Pr(z != E(Ii)) Sim" else kol_sim[1]
  p <- lm[[kol_p]]
  
  z   <- as.numeric(scale(x))
  lag <- spdep::lag.listw(lw, z)
  kat <- ifelse(p >= alpha | is.na(p), "ns",
                ifelse(z > 0 & lag > 0, "HH",
                       ifelse(z < 0 & lag < 0, "LL",
                              ifelse(z > 0 & lag < 0, "HL", "LH"))))
  list(global = list(I = unname(as.numeric(mc$statistic)), p = as.numeric(mc$p.value)),
       local  = tibble::tibble(kode_kabkota = as.character(sf_obj[[id]]), Ii = lm$Ii, p = p, kategori = kat))
}

.matriks_var <- function(df, vars = VAR_X) as.matrix(as.data.frame(df)[, vars, drop = FALSE])

detect_outliers <- function(df, vars = VAR_X, z = Z_PENCILAN, min_vars = MIN_VAR_PENCILAN) {
  m <- scale(.matriks_var(df, vars))
  as.character(df$provinsi[rowSums(abs(m) > z) >= min_vars])
}

compute_pca <- function(df, exclude = character(), vars = VAR_X) {
  if (VAR_Y %in% vars) stop("compute_pca: '", VAR_Y, "' adalah variabel respons dan tidak boleh ikut PCA.")
  sub <- df[!(df$provinsi %in% exclude), , drop = FALSE]
  p <- stats::prcomp(.matriks_var(sub, vars), center = TRUE, scale. = TRUE)   # z-score dihitung ulang pada subset
  kode <- if (is.null(sub$kode_prov)) rep(NA_character_, nrow(sub)) else sub$kode_prov
  list(scores = dplyr::bind_cols(tibble::tibble(kode_prov = kode, provinsi = sub$provinsi),
                                 tibble::as_tibble(p$x)),
       loadings = p$rotation,
       var_explained = p$sdev^2 / sum(p$sdev^2),
       center = p$center, sdev = p$sdev)
}

hclust_ward <- function(df, vars = VAR_X) stats::hclust(stats::dist(scale(.matriks_var(df, vars))), "ward.D2")

choose_k <- function(df, ks = 2:6, vars = VAR_X) {
  Z <- scale(.matriks_var(df, vars)); d <- stats::dist(Z); hc <- stats::hclust(d, "ward.D2")
  sil <- vapply(ks, function(k) mean(cluster::silhouette(stats::cutree(hc, k), d)[, "sil_width"]), numeric(1))
  wss <- vapply(ks, function(k) {
    cl <- stats::cutree(hc, k)
    sum(vapply(split(seq_len(nrow(Z)), cl), function(i) sum(scale(Z[i, , drop = FALSE], scale = FALSE)^2), numeric(1)))
  }, numeric(1))
  list(k = ks[which.max(sil)], silhouette = stats::setNames(sil, ks), wss = stats::setNames(wss, ks))
}

cluster_ward <- function(df, k, vars = VAR_X) as.integer(stats::cutree(hclust_ward(df, vars), k))

corr_with_informal <- function(df, vars = VAR_X) {
  lain <- setdiff(vars, VAR_Y)
  tibble::tibble(variabel = lain, r = vapply(lain, function(v) stats::cor(df[[v]], df[[VAR_Y]]), numeric(1)))
}

SKALA_REGRESI <- c("Upah Formal" = 1e6, "Upah/Jam" = 1e3)
label_reg  <- function(v) ifelse(v == "Upah Formal", "Upah Formal (juta Rp)", ifelse(v == "Upah/Jam", "Upah/Jam (ribu Rp)", v))
label_resp <- function(v) ifelse(v == VAR_Y, paste0(v, " (respons)"), v)
fmt_p      <- function(p) ifelse(is.na(p), "\u2013", ifelse(p < 0.001, "< 0,001", fmt_id(p, 3)))
tanda_txt  <- function(x) ifelse(is.na(x), "", ifelse(x >= 0, "+", "\u2212"))
fmt_s      <- function(x, d = 2) paste0(tanda_txt(x), fmt_id(abs(x), d))

REF_TANDA <- data.frame(
  variabel = c("TPAK-L", "TPAK-P", "TPT", "Upah Formal", "RLS", "%Miskin", "Upah/Jam", "Dikdas", "IPG"),
  tanda = c(-1, 1, 1, -1, -1, 1, -1, -1, -1),
  acuan = c("Birgitta (2021): TPAK laki-laki bertanda negatif",
            "Birgitta (2021): TPAK perempuan bertanda positif",
            "Birgitta (2021): pengangguran bertanda positif",
            "Birgitta (2021): upah bersih formal bertanda negatif; Sibagariang dkk. (2023): upah naik menarik pekerja ke sektor formal",
            "Teori modal manusia: negatif; Birgitta (2021) justru menemukan positif",
            "Sibagariang dkk. (2023): penduduk miskin bertanda positif",
            "Sibagariang dkk. (2023): upah per jam bertanda negatif",
            "Sibagariang dkk. (2023): penyelesaian pendidikan dasar bertanda negatif",
            "Sibagariang dkk. (2023): korelasi negatif lemah, koefisien regresi tidak signifikan"),
  stringsAsFactors = FALSE)

data_regresi <- function(df) {
  d <- as.data.frame(df)[, c(VAR_Y, VAR_X), drop = FALSE]
  for (v in intersect(names(SKALA_REGRESI), names(d))) d[[v]] <- d[[v]] / SKALA_REGRESI[[v]]
  rownames(d) <- NULL
  d
}

ringkas_normalitas <- function(df, vars = VAR_MV, alpha = ALPHA_NORMAL) {
  out <- do.call(rbind, lapply(vars, function(v) {
    x <- as.numeric(df[[v]]); q <- stats::quantile(x, c(0, .25, .5, .75, 1), names = FALSE); sw <- stats::shapiro.test(x)
    data.frame(variabel = v, n = length(x), min = q[1], q1 = q[2], median = q[3], rata = mean(x), q3 = q[4], maks = q[5],
               sd = stats::sd(x), W = unname(sw$statistic), p = sw$p.value, normal = sw$p.value > alpha, stringsAsFactors = FALSE)
  }))
  out$status <- ifelse(out$normal, "Normal", "Tidak normal")
  out
}

# Pearson bila kedua variabel normal, selain itu Spearman; simpan r, p, dan metode tiap pasangan
korelasi_pasangan <- function(df, vars = VAR_MV, normal, alpha = ALPHA_REG, r_multikol = 0.8) {
  p <- length(vars); mk <- function(x) matrix(x, p, p, dimnames = list(vars, vars))
  r <- mk(NA_real_); pv <- mk(NA_real_); me <- mk(NA_character_)
  diag(r) <- 1; diag(pv) <- 0; diag(me) <- "\u2013"
  for (i in seq_len(p - 1)) for (j in (i + 1):p) {
    met <- if (isTRUE(normal[[vars[i]]]) && isTRUE(normal[[vars[j]]])) "pearson" else "spearman"
    ct <- suppressWarnings(stats::cor.test(df[[vars[i]]], df[[vars[j]]], method = met, exact = FALSE))
    r[i, j] <- r[j, i] <- unname(ct$estimate); pv[i, j] <- pv[j, i] <- ct$p.value
    me[i, j] <- me[j, i] <- if (met == "pearson") "Pearson" else "Spearman"
  }
  x <- setdiff(vars, VAR_Y)
  xy <- data.frame(variabel = x, r = r[x, VAR_Y], p = pv[x, VAR_Y], metode = me[x, VAR_Y], stringsAsFactors = FALSE)
  xy$sig <- ifelse(xy$p < 0.01, "***", ifelse(xy$p < 0.05, "**", ifelse(xy$p < alpha, "*", "")))
  xy <- xy[order(-abs(xy$r)), ]; rownames(xy) <- NULL
  ij <- which(upper.tri(r) & abs(r) >= r_multikol, arr.ind = TRUE)
  ij <- ij[vars[ij[, 1]] != VAR_Y & vars[ij[, 2]] != VAR_Y, , drop = FALSE]
  xx <- data.frame(v1 = vars[ij[, 1]], v2 = vars[ij[, 2]], r = r[ij], metode = me[ij], stringsAsFactors = FALSE)
  list(r = r, p = pv, metode = me, xy = xy, xx_tinggi = xx)
}

vif_manual <- function(X) {   # VIF_j = 1 / (1 - R2_j), R2_j dari regresi X_j pada prediktor lain
  X <- as.matrix(X); p <- ncol(X)
  if (p < 2) return(stats::setNames(rep(1, p), colnames(X)))
  stats::setNames(vapply(seq_len(p), function(j) 1 / (1 - summary(stats::lm(X[, j] ~ X[, -j, drop = FALSE]))$r.squared), numeric(1)), colnames(X))
}

bp_koenker <- function(fit) {   # Breusch-Pagan versi Koenker (sama dengan lmtest::bptest bawaan)
  e2 <- stats::residuals(fit)^2; X <- stats::model.matrix(fit)
  stat <- length(e2) * summary(stats::lm(e2 ~ X[, -1, drop = FALSE]))$r.squared; df <- ncol(X) - 1
  c(stat = stat, df = df, p = stats::pchisq(stat, df, lower.tail = FALSE))
}

.id_x   <- function(v = VAR_X) paste0("X", match(v, VAR_X))
.nama_x <- function(id) VAR_X[as.integer(sub("X", "", id))]

fit_rlb <- function(d) {
  dd <- d; names(dd) <- c("Y", .id_x(VAR_X))
  full <- stats::lm(Y ~ ., data = dd)
  list(full = full, step = stats::step(full, direction = "both", trace = 0))
}

ringkas_model <- function(fit, nama, alpha = ALPHA_REG, nama_fn = .nama_x) {
  mf <- stats::model.frame(fit); y <- mf[[1]]; n <- length(y)
  sm <- summary(fit); cf <- sm$coefficients; ci <- stats::confint(fit, level = 1 - alpha)
  id <- rownames(cf); ada <- id != "(Intercept)"
  sdx <- vapply(id[ada], function(v) stats::sd(mf[[v]]), numeric(1)); sdy <- stats::sd(y)
  nm_var <- rep("(Intercept)", length(id)); nm_var[ada] <- nama_fn(id[ada])   # nama_fn tidak dipanggil untuk intersep
  koef <- data.frame(variabel = nm_var, b = cf[, 1], se = cf[, 2], t = cf[, 3], p = cf[, 4],
                     beta_baku = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_, stringsAsFactors = FALSE)
  koef$beta_baku[ada] <- cf[ada, 1] * sdx / sdy
  koef$ci_lo[ada] <- ci[ada, 1] * sdx / sdy; koef$ci_hi[ada] <- ci[ada, 2] * sdx / sdy
  koef$signifikan <- koef$p < alpha
  koef$keputusan <- ifelse(koef$signifikan, "Signifikan", "Tidak signifikan")
  rownames(koef) <- NULL
  Xm <- stats::model.matrix(fit)[, -1, drop = FALSE]; e <- stats::residuals(fit); h <- stats::hatvalues(fit)
  vif <- vif_manual(Xm); names(vif) <- nama_fn(names(vif))
  f <- sm$fstatistic; sw <- stats::shapiro.test(e)
  list(nama = nama, vars = nm_var[ada], koef = koef, r2 = sm$r.squared, adj_r2 = sm$adj.r.squared,
       f_p = unname(stats::pf(f[1], f[2], f[3], lower.tail = FALSE)), aic = stats::AIC(fit), n = n, k = sum(ada),
       rmse_loo = sqrt(mean((e / (1 - h))^2)),                    # LOOCV lewat hat-matrix
       vif = vif, shapiro = c(W = unname(sw$statistic), p = sw$p.value), bp = bp_koenker(fit),
       cooks = unname(stats::cooks.distance(fit)), cook_batas = 4 / n,
       observed = as.numeric(y), fitted = as.numeric(stats::fitted(fit)), resid = as.numeric(e))
}

kategori_kmo <- function(x) as.character(cut(x, c(-Inf, .5, .6, .7, .8, .9, Inf), right = FALSE,
  labels = c("tidak layak", "buruk", "sedang", "cukup baik", "baik", "sangat baik")))

kmo_bartlett <- function(R, n) {
  p <- ncol(R); S <- solve(R); Q <- -S / sqrt(outer(diag(S), diag(S))); diag(Q) <- 0
  R0 <- R; diag(R0) <- 0
  msa <- rowSums(R0^2) / (rowSums(R0^2) + rowSums(Q^2)); kmo <- sum(R0^2) / (sum(R0^2) + sum(Q^2))
  chi <- -(n - 1 - (2 * p + 5) / 6) * log(det(R)); df <- p * (p - 1) / 2
  list(kmo = kmo, msa = msa, kategori = kategori_kmo(kmo),
       bartlett = c(chisq = chi, df = df, p = stats::pchisq(chi, df, lower.tail = FALSE)))
}

judul_pc <- function(loadings, n_top = 2L) {
  vapply(seq_len(ncol(loadings)), function(j) {
    l <- loadings[, j]; o <- order(abs(l), decreasing = TRUE)[seq_len(min(n_top, length(l)))]
    sprintf("PC%d: %s", j, paste(sprintf("%s (%s%s)", rownames(loadings)[o], ifelse(l[o] >= 0, "+", "\u2212"), fmt_id(abs(l[o]), 2)), collapse = ", "))
  }, character(1))
}

ringkas_pca <- function(df, k_pc = K_PC, vars = VAR_X) {
  X <- .matriks_var(df, vars); kb <- kmo_bartlett(stats::cor(X), nrow(X)); pc <- compute_pca(df, vars = vars)
  ev <- pc$sdev^2; kum <- cumsum(pc$var_explained)
  k_eig <- sum(ev > 1); k_70 <- which(kum >= 0.70)[1]
  k <- if (!is.na(k_pc)) as.integer(k_pc) else max(1L, k_eig)
  stopifnot("K_PC di luar 1..jumlah variabel" = k >= 1 && k <= length(vars))
  list(vars = vars, kmo = kb$kmo, kmo_kategori = kb$kategori, msa = kb$msa, bartlett = kb$bartlett,
       eigen = data.frame(komponen = paste0("PC", seq_along(ev)), eigenvalue = ev, varians = pc$var_explained, kumulatif = kum, stringsAsFactors = FALSE),
       k_eigen = k_eig, k_70 = k_70, k = k, judul_pc = judul_pc(pc$loadings), hasil = pc)
}

fit_pcr <- function(d, pca, k) {
  stopifnot(nrow(d) == nrow(pca$scores))
  S <- as.data.frame(pca$scores[, paste0("PC", seq_len(k)), drop = FALSE]); S$Y <- d[[VAR_Y]]
  stats::lm(Y ~ ., data = S)
}

# koefisien PC -> variabel asli: beta_z = V_k %*% gamma (per 1 SD X, satuan Y); beta_baku = beta_z / sd(Y); beta_asli = beta_z / sd(X)
balik_koef_pcr <- function(fit, pca, k, d) {
  gamma <- stats::coef(fit)[paste0("PC", seq_len(k))]
  bz <- as.numeric(pca$loadings[, seq_len(k), drop = FALSE] %*% gamma)
  sdx <- vapply(d[VAR_X], stats::sd, numeric(1)); mux <- vapply(d[VAR_X], mean, numeric(1))
  ba <- bz / sdx
  list(koef = data.frame(variabel = VAR_X, beta_z = bz, beta_baku = bz / stats::sd(d[[VAR_Y]]), beta_asli = ba, stringsAsFactors = FALSE),
       intersep = unname(stats::coef(fit)[1] - sum(ba * mux)))
}

tabel_perbandingan <- function(...) {
  ms <- list(...)
  do.call(rbind, lapply(ms, function(m) data.frame(
    model = m$nama, prediktor = m$k, r2 = m$r2, adj_r2 = m$adj_r2, aic = m$aic, rmse_loo = m$rmse_loo,
    shapiro_p = m$shapiro[["p"]], lolos_shapiro = m$shapiro[["p"]] > ALPHA_NORMAL,
    bp_p = m$bp[["p"]], lolos_bp = m$bp[["p"]] > ALPHA_REG, stringsAsFactors = FALSE)))
}

analisis_multivar <- function(df, k_pc = K_PC) {
  df <- as.data.frame(df)
  nm <- ringkas_normalitas(df)
  kor <- korelasi_pasangan(df, VAR_MV, stats::setNames(nm$normal, nm$variabel))
  d <- data_regresi(df); fits <- fit_rlb(d)
  full <- ringkas_model(fits$full, "Full model"); stp <- ringkas_model(fits$step, "Stepwise (AIC)")
  pca <- ringkas_pca(df, k_pc)
  fp <- fit_pcr(d, pca$hasil, pca$k)
  pcr <- ringkas_model(fp, sprintf("PCR (k = %d)", pca$k), nama_fn = identity)
  list(n = nrow(df), kode_prov = df$kode_prov, provinsi = df$provinsi, normalitas = nm, korelasi = kor,
       rlb = list(full = full, step = stp), pca = pca,
       pcr = c(list(model = pcr), balik_koef_pcr(fp, pca$hasil, pca$k, d)),
       perbandingan = tabel_perbandingan(full, stp, pcr))
}

laporan_multivar <- function(m, label) {
  f <- m$rlb$full; s <- m$rlb$step; r <- m$pcr$model; pc <- m$pca; nm <- m$normalitas
  c(sprintf("== Skenario %s (n = %d) ==", label, m$n),
    sprintf("Variabel normal: %d dari %d", sum(nm$normal), nrow(nm)),
    paste("Shapiro p:", paste(sprintf("%s=%.4f", nm$variabel, nm$p), collapse = " | ")),
    sprintf("Shapiro residual p: full %.4f | step %.4f | PCR %.4f", f$shapiro[["p"]], s$shapiro[["p"]], r$shapiro[["p"]]),
    sprintf("Breusch-Pagan p: full %.4f | step %.4f | PCR %.4f", f$bp[["p"]], s$bp[["p"]], r$bp[["p"]]),
    sprintf("KMO = %.3f (%s); Bartlett chi2 = %.2f, df = %d, p = %.4g", pc$kmo, pc$kmo_kategori, pc$bartlett[["chisq"]], as.integer(pc$bartlett[["df"]]), pc$bartlett[["p"]]),
    sprintf("k komponen dipakai = %d (eigenvalue > 1: %d; varians kumulatif >= 70%%: %d)", pc$k, pc$k_eigen, pc$k_70),
    sprintf("R2 / Adj R2: full %.3f / %.3f | stepwise %.3f / %.3f | PCR %.3f / %.3f", f$r2, f$adj_r2, s$r2, s$adj_r2, r$r2, r$adj_r2),
    sprintf("VIF terbesar (full): %s = %.2f", names(which.max(f$vif)), max(f$vif)),
    paste("Variabel stepwise:", paste(s$vars, collapse = ", ")))
}

validasi_multivar <- function(pv) {
  sama <- function(a, b, tol = 1e-6) isTRUE(all.equal(a, b, tolerance = tol, check.attributes = FALSE))
  p9 <- compute_pca(pv)
  stopifnot("pca: tepat 9 variabel"            = nrow(p9$loadings) == 9 && length(p9$center) == 9,
            "pca: Informal tak ada di loading" = !(VAR_Y %in% rownames(p9$loadings)),
            "pca: sum(var_explained) = 1"      = sama(sum(p9$var_explained), 1),
            "pca: Informal ditolak"            = inherits(try(compute_pca(pv, vars = VAR_MV), silent = TRUE), "try-error"))
  m <- analisis_multivar(pv)
  stopifnot("KMO di (0,1)"           = m$pca$kmo > 0 && m$pca$kmo < 1,
            "Bartlett p di [0,1]"    = m$pca$bartlett[["p"]] >= 0 && m$pca$bartlett[["p"]] <= 1,
            "stepwise subset dari X" = all(m$rlb$step$vars %in% VAR_X),
            "VIF skor PC = 1"        = all(abs(m$pcr$model$vif - 1) < 1e-6))
  d <- data_regresi(pv); fits <- fit_rlb(d); full <- ringkas_model(fits$full, "F")
  fp9 <- fit_pcr(d, m$pca$hasil, 9); bk <- balik_koef_pcr(fp9, m$pca$hasil, 9, d)
  stopifnot("PCR k=9: fitted = OLS full"    = isTRUE(all.equal(unname(stats::fitted(fp9)), unname(stats::fitted(fits$full)))),
            "PCR k=9: koefisien = OLS full" = isTRUE(all.equal(bk$koef$beta_asli, full$koef$b[-1])),
            "PCR k=9: intersep = OLS full"  = isTRUE(all.equal(bk$intersep, full$koef$b[1])))
  for (v in VAR_MV)
    stopifnot("Shapiro cocok shapiro.test()" = isTRUE(all.equal(m$normalitas$p[m$normalitas$variabel == v], stats::shapiro.test(pv[[v]])$p.value)))
  if (requireNamespace("lmtest", quietly = TRUE)) {
    bp <- lmtest::bptest(fits$full)
    stopifnot("BP manual = lmtest::bptest" = isTRUE(all.equal(unname(bp$p.value), full$bp[["p"]])))
  }
  stopifnot("skenario 35 provinsi" = analisis_multivar(pv[4:38, ])$n == 35)
  invisible(TRUE)
}

apply_selection <- function(df, selected) {
  if (length(selected) == 0) return(rep(FALSE, nrow(df)))
  df$kode_prov %in% selected
}

heatmap_matrix <- function(df, vars = VAR_MV) {
  m <- scale(.matriks_var(df, vars)); rownames(m) <- df$provinsi; colnames(m) <- vars
  attr(m, "scaled:center") <- NULL; attr(m, "scaled:scale") <- NULL
  m
}

bar_korelasi_data <- function(kor_xy) kor_xy[order(-abs(kor_xy$r)), ]

bbox_provinsi <- function(kabkota_sf, kode_prov) {
  if (length(kode_prov) != 1 || is.na(kode_prov)) return(NULL)
  sub <- kabkota_sf[kabkota_sf$kode_prov == kode_prov, ]
  if (!nrow(sub)) return(NULL)
  as.numeric(sf::st_bbox(sub))
}

validasi_fungsi <- function() {
  kotak <- function(x, y = 0, w = 1) sf::st_polygon(list(rbind(c(x, y), c(x + w, y), c(x + w, y + w), c(x, y + w), c(x, y))))
  sama <- function(a, b, tol = 1e-8) isTRUE(all.equal(a, b, tolerance = tol, check.attributes = FALSE))
  
  stopifnot(
    "fmt_id(1234567.8, 1)" = identical(fmt_id(1234567.8, 1), "1.234.567,8"),
    "fmt_id(57.8, 1)"      = identical(fmt_id(57.8, 1), "57,8"),
    "fmt_id(5, 0)"         = identical(fmt_id(5, 0), "5"),
    "fmt_id(NA, 1)"        = identical(fmt_id(NA, 1), "\u2013"),
    "fmt_id vektor"        = identical(fmt_id(c(1000, 2500000), 0), c("1.000", "2.500.000"))
  )
  
  sb <- data.frame(
    id = c("usia_kerja", "bekerja", "bukan_ak", "formal", "informal"),
    induk = c(NA, "usia_kerja", "usia_kerja", "bekerja", "bekerja"),
    label = c("Penduduk Usia Kerja", "Bekerja", "Bukan Angkatan Kerja", "Formal", "Informal"),
    juta_orang = c(100, 60, 40, 20, 40), juta_perempuan = c(NA, 24, NA, 6, 18))
  sb$pct_perempuan <- pct_perempuan(sb)
  stopifnot("pct_perempuan" = sama(sb$pct_perempuan, c(NA, 40, NA, 30, 45)),
            "titik tengah"  = sama(titik_tengah_perempuan(sb), 40))
  dd <- data.frame(id = c("usia_kerja", "bekerja"), juta_orang = c(100, 60))
  kpi <- kpi_beranda(dd, sb)
  stopifnot("kpi_beranda" = sama(unlist(kpi), c(100, 60, 40, 66.7)))
  stopifnot("action_title" = grepl("57,8%", action_title_beranda(list(pct_informal = 57.8)), fixed = TRUE))
  bs <- build_sunburst_data(sb)
  stopifnot("sunburst: parent ada di ids" = all(bs$parents[bs$parents != ""] %in% bs$ids),
            "sunburst: satu root"         = sum(bs$parents == "") == 1,
            "sunburst: anak = induk"      = sama(sum(bs$values[bs$parents == "usia_kerja"]), 100),
            "sunburst: anak bekerja"      = sama(sum(bs$values[bs$parents == "bekerja"]), 60))
  stopifnot("jalur_simpul" = identical(jalur_simpul(sb, "informal"), c("usia_kerja", "bekerja", "informal")))
  w <- warna_pct_perempuan(c(40, NA, 0, 100), mid = 40)
  stopifnot("warna divergen" = identical(w, c(PALET_DIV[6], WARNA_ABU, PALET_DIV[1], PALET_DIV[11])))
  lum_rel <- function(h) {
    r <- grDevices::col2rgb(h)[, 1] / 255
    l <- ifelse(r <= 0.04045, r / 12.92, ((r + 0.055) / 1.055)^2.4)
    sum(l * c(0.2126, 0.7152, 0.0722))
  }
  stopifnot("palet urut: kecerahan turun monoton" = all(diff(vapply(unname(PALET_URUT5), lum_rel, numeric(1))) < 0),
            "palet klaster: Okabe-Ito"            = all(PALET_KLASTER %in% OKABE),
            "palet LISA: Okabe-Ito"               = all(WARNA_LISA[c("HH", "LL", "HL", "LH")] %in% OKABE))
  
  d <- data.frame(
    kode_kabkota = c("11.01", "11.02", "91.2", "91.20", "91.05", "94.05", "92.01", "92.01", "11.03", "11.04", "11.05", NA, "11.06", "11.07"),
    nama = c("A", "B", "Mamberamo Raya", "Mamberamo Raya", "Nabire", "Nabire", "Sorong", "Sorong",
             "Sumbawa/Sumbawa Barat", "Pahuwato", "Minahasa Selatan/Bolaang Mongondwo Timur", NA, "", "C"),
    provinsi_shp = c("Aceh", "Aceh", "Papua", "Papua", "Papua", "Papua Tengah", "Papua Barat Daya", "Papua Barat",
                     "NTB", "Gorontalo", "Sulut", "Aceh", "Aceh", "Aceh"), stringsAsFactors = FALSE)
  shp <- sf::st_sf(d, geometry = sf::st_sfc(lapply(c(0, 2, 4, 4, 6, 6, 8, 8, 10, 12, 14, 16, 18, 20), kotak), crs = 4326))
  h <- clean_batas(shp, n_expected = 6L)
  stopifnot("clean_batas: 6 baris"   = nrow(h) == 6,
            "clean_batas: Mamberamo" = sum(h$nama == "Mamberamo Raya") == 1 && h$kode_kabkota[h$nama == "Mamberamo Raya"] == "91.20",
            "clean_batas: Nabire 94"  = h$kode_kabkota[h$nama == "Nabire"] == "94.05",
            "clean_batas: Sorong PBD" = h$provinsi_shp[h$nama == "Sorong"] == "Papua Barat Daya",
            "clean_batas: tanpa '/'"  = !any(grepl("/", h$nama, fixed = TRUE)) && !anyNA(h$nama))
  stopifnot("clean_batas: jumlah salah harus galat" = inherits(try(clean_batas(shp, n_expected = 514L), silent = TRUE), "try-error"))
  s2 <- shp; sf::st_geometry(s2)[4] <- sf::st_sfc(kotak(30), crs = 4326)
  stopifnot("clean_batas: duplikat beda wilayah harus galat" =
              inherits(try(suppressMessages(clean_batas(s2, n_expected = 6L)), silent = TRUE), "try-error"))
  
  js <- sf::st_sf(kode_kabkota = c("11.01", "11.02", "32.01", "32.71"), nama = c("Kabupaten A", "B", "Bandung", "Kota Bandung"),
                  provinsi_shp = c("Aceh", "Aceh", "Jawa Barat", "Jawa Barat"),
                  geometry = sf::st_sfc(lapply(c(0, 2, 4, 6), kotak), crs = 4326))
  xl <- data.frame(nama = c("A", "B", "Bandung", "Kota Bandung"), informal = c(50, 30, 60, 10), total_pekerja = rep(100, 4))
  kp <- data.frame(provinsi_multivariat = c("Aceh", "Jawa Barat"), provinsi_shp = c("Aceh", "Jawa Barat"), kode_prov = c("11", "32"))
  jk <- join_kabkota(xl, js, kp)
  stopifnot("join_kabkota: kolom" = identical(names(jk), c("kode_kabkota", "kode_prov", "nama", "provinsi", "informal",
                                                           "total_pekerja", "rasio", "flag_ekstrem", "geometry")),
            "join_kabkota: rasio" = sama(jk$rasio, jk$informal / jk$total_pekerja),
            "join_kabkota: kode_prov" = identical(jk$kode_prov, c("11", "11", "32", "32")))
  xl2 <- xl; xl2$nama[2] <- "Beda"
  stopifnot("join_kabkota: nama tak cocok harus galat" = inherits(try(join_kabkota(xl2, js, kp), silent = TRUE), "try-error"))
  ag <- agg_provinsi(jk)
  stopifnot("agg_provinsi" = sama(ag$informal_pct[ag$kode_prov == "11"], 40) && sama(ag$informal_pct[ag$kode_prov == "32"], 35))
  stopifnot("kunci_nama" = kunci_nama("Kota Adm. Jakarta Selatan") == kunci_nama("Kota Administrasi Jakarta Selatan"),
            "normalisasi_kode_bps" = identical(normalisasi_kode_bps(c("91.2", "91.20", "11.01")), c("91.20", "91.20", "11.01")))
  
  x <- c(seq(0.3, 0.8, length.out = 450), seq(0.95, 1, length.out = 64))
  bj <- classify_breaks(x, "jenks", 5); bq <- classify_breaks(x, "quantile", 5); be <- classify_breaks(x, "equal", 5)
  stopifnot("jenks: 6 nilai menaik"  = length(bj) == 6 && all(diff(bj) > 0) && bj[1] <= min(x) && bj[6] >= max(x),
            "quantile: ~20% per kelas" = all(abs(as.numeric(table(kelas_dari_breaks(x, bq))) / length(x) - 0.2) < 0.02),
            "equal: selang sama"     = sama(diff(be), rep(diff(be)[1], 5)),
            "kelas_dari_breaks"      = identical(as.numeric(kelas_dari_breaks(c(0, 0.5, 1), c(0, 0.5, 1))), c(1, 2, 2)),
            "label_kelas"            = identical(label_kelas(c(0.5, 0.75, 1)), c("50,0\u201375,0", "75,0\u2013100,0")),
            "radius_simbol"          = sama(radius_simbol(c(100, 400, 10000), r_max = 20, x_max = 10000, r_min = 2), c(2, 4, 20)))
  ct <- data.frame(kode_kabkota = c("11.01", "11.02", "11.03", "32.01"), kode_prov = c("11", "11", "11", "32"),
                   nama = c("A", "B", "C", "D"), provinsi = c("Aceh", "Aceh", "Aceh", "Jawa Barat"),
                   informal = c(50, 80, 20, 90), total_pekerja = rep(100, 4),
                   flag_ekstrem = c(FALSE, FALSE, TRUE, FALSE), rasio = c(.5, .8, .2, .9))
  r <- ringkas_kabkota(ct, "11.02")
  stopifnot("dalam_filter"  = identical(dalam_filter(ct, "11"), c(TRUE, TRUE, TRUE, FALSE)),
            "ringkas_kabkota" = r$peringkat == 2 && sama(r$rata_prov, 0.5) && sama(r$rata_nasional, 0.6) && is.null(ringkas_kabkota(ct, "99.99")),
            "top_n_kabkota" = identical(top_n_kabkota(ct, 2, TRUE)$nama, c("D", "B")) && identical(top_n_kabkota(ct, 2, FALSE)$nama, c("C", "A")),
            "teks_tooltip"  = grepl("50,0%", teks_tooltip(ct)[1], fixed = TRUE) && grepl("Nilai ekstrem", teks_tooltip(ct)[3], fixed = TRUE),
            "legenda simbol" = grepl("100.000", html_legenda_simbol(1e5, 1e6), fixed = TRUE))
  
  g <- expand.grid(i = 1:6, j = 1:6)
  nilai <- ifelse(g$i <= 2, 0.9, ifelse(g$i >= 5, 0.1, 0.5)) + 0.02 * sin(seq_len(36))
  grid <- sf::st_sf(kode_kabkota = sprintf("g%02d", 1:36), rasio = nilai,
                    geometry = sf::st_sfc(Map(function(a, b) kotak(a, b), g$i, g$j), crs = 4326))
  l1 <- compute_lisa(grid, k = 4L, nsim = 199L, seed = 1L); l2 <- compute_lisa(grid, k = 4L, nsim = 199L, seed = 1L)
  stopifnot("lisa: kolom local"  = identical(names(l1$local), c("kode_kabkota", "Ii", "p", "kategori")),
            "lisa: 36 baris"     = nrow(l1$local) == 36,
            "lisa: kategori sah" = all(l1$local$kategori %in% c("HH", "LL", "HL", "LH", "ns")),
            "lisa: ada HH dan LL" = any(l1$local$kategori == "HH") && any(l1$local$kategori == "LL"),
            "lisa: seed sama"    = identical(l1, l2),
            "lisa: global"       = is.numeric(l1$global$I) && l1$global$p >= 0 && l1$global$p <= 1)
  
  idx <- 1:38
  set.seed(1); f0 <- stats::rnorm(38)
  mk <- function(a, mu, s) mu + s * (a * f0 + sqrt(1 - a^2) * stats::rnorm(38))
  pv0 <- data.frame(kode_prov = sprintf("%02d", idx), provinsi = paste("Prov", idx), stringsAsFactors = FALSE)
  pv0[VAR_MV] <- list(mk(.9, 60, 10), mk(-.3, 80, 3), mk(.5, 60, 6), mk(-.4, 5, 1.5), mk(-.7, 3.2e6, 4e5),
                      mk(-.6, 8.8, .8), mk(.8, 10, 5), mk(-.7, 18000, 4000), mk(-.6, 90, 6), mk(-.3, 91, 3))
  pv <- pv0; pv[1, VAR_X[1:3]] <- pv[1, VAR_X[1:3]] + 400
  stopifnot("detect_outliers" = identical(detect_outliers(pv), "Prov 1"))
  p0 <- compute_pca(pv); p1 <- compute_pca(pv, exclude = c("Prov 1", "Prov 2", "Prov 3"))
  stopifnot("pca: 38 skor"  = nrow(p0$scores) == 38 && sama(sum(p0$var_explained), 1),
            "pca: exclude 35" = nrow(p1$scores) == 35 && sama(unname(p1$center), colMeans(.matriks_var(pv[4:38, ]))),
            "pca: rata skor 0" = max(abs(colMeans(p1$scores[, c("PC1", "PC2")]))) < 1e-8)
  ck <- choose_k(pv)
  cl <- cluster_ward(pv, 3)
  stopifnot("choose_k"     = ck$k %in% 2:6 && length(ck$silhouette) == 5,
            "cluster_ward" = identical(cl, cluster_ward(pv, 3)) && all(cl %in% 1:3),
            "corr: 9 baris" = nrow(corr_with_informal(pv)) == 9 && all(abs(corr_with_informal(pv)$r) <= 1),
            "apply_selection" = identical(apply_selection(pv, "02"), idx == 2) && !any(apply_selection(pv, NULL)) && !any(apply_selection(pv, character())),
            "heatmap_matrix" = identical(dim(heatmap_matrix(pv)), c(38L, 10L)) && identical(colnames(heatmap_matrix(pv)), VAR_MV) &&
              identical(rownames(heatmap_matrix(pv)), pv$provinsi) && max(abs(colMeans(heatmap_matrix(pv)))) < 1e-8,
            "bar_korelasi"   = !is.unsorted(rev(abs(bar_korelasi_data(corr_with_informal(pv))$r))) && nrow(bar_korelasi_data(corr_with_informal(pv))) == 9)
  validasi_multivar(pv0)
  
  bb <- bbox_provinsi(jk, "32")
  stopifnot("bbox: 4 angka" = length(bb) == 4 && bb[1] < bb[3] && bb[2] < bb[4],
            "bbox: kode tak dikenal NULL" = is.null(bbox_provinsi(jk, "99")))
  invisible(TRUE)
}

# ==== [A4] bangun_data(): pra-pemrosesan (hanya jalan bila data/olahan.rds belum ada) ====

validasi_data <- function(o) {
  TOL <- 0.01
  dn <- o$dendrogram
  stopifnot("dendrogram: 8 baris, 4 level" = nrow(dn) == 8 && identical(sort(unique(dn$level)), 1:4),
            "dendrogram: label tanpa spasi tepi" = identical(dn$label, trimws(dn$label)))
  for (i in unique(stats::na.omit(dn$induk))) {
    sel <- dn$juta_orang[dn$id == i] - sum(dn$juta_orang[!is.na(dn$induk) & dn$induk == i])
    if (abs(sel) > TOL) stop(sprintf("dendrogram: induk '%s' != jumlah anak (selisih %.3f)", i, sel))
  }
  v <- function(id) dn$juta_orang[dn$id == id]
  stopifnot("dendrogram: 146,54 / 154,00 / 218,17" = abs(v("bekerja") - 146.54) < TOL && abs(v("angkatan_kerja") - 154) < TOL &&
              abs(v("usia_kerja") - 218.17) < TOL)
  sb <- o$sunburst; s <- function(id) sb$juta_orang[sb$id == id]
  stopifnot("sunburst: kolom" = identical(names(sb), c("id", "induk", "label", "juta_orang", "juta_perempuan", "pct_perempuan")),
            "sunburst: Formal"   = abs(s("formal") - 61.84) < TOL && abs(s("formal_l") + s("formal_p") - s("formal")) < TOL,
            "sunburst: Informal" = abs(s("informal") - 84.70) < TOL && abs(s("informal_l") + s("informal_p") - s("informal")) < TOL,
            "sunburst: semua daun = 218,17" = abs(sum(sb$juta_orang[!(sb$id %in% sb$induk)]) - 218.17) < TOL,
            "sunburst: pct_perempuan konsisten" = isTRUE(all.equal(sb$pct_perempuan, pct_perempuan(sb))))
  kb <- o$kabkota
  stopifnot("kabkota: 514 baris"          = nrow(kb) == 514,
            "kabkota: kode unik"          = !anyDuplicated(kb$kode_kabkota),
            "kabkota: 514 geometri utuh"  = !any(sf::st_is_empty(kb)) && length(sf::st_geometry(kb)) == 514,
            "kabkota: rasio"              = isTRUE(all.equal(kb$rasio, kb$informal / kb$total_pekerja)),
            "kabkota: informal <= total"  = all(kb$informal <= kb$total_pekerja),
            "kabkota: total ~146,54 juta" = abs(sum(kb$total_pekerja) / 1e6 - 146.54) / 146.54 < 0.005)
  stopifnot("breaks: tiga set, masing-masing 6" = all(c("jenks5", "quantile5", "equal5") %in% names(o$breaks)) &&
              all(vapply(o$breaks[c("jenks5", "quantile5", "equal5")], length, integer(1)) == 6),
            "lisa: 514 baris lokal" = nrow(o$lisa$local) == 514 && all(o$lisa$local$kode_kabkota %in% kb$kode_kabkota))
  pv <- o$provinsi
  stopifnot("provinsi: 38 baris"       = nrow(pv) == 38,
            "provinsi: kode_prov unik" = !anyNA(pv$kode_prov) && !anyDuplicated(pv$kode_prov),
            "provinsi: 10 variabel numerik tanpa NA" = all(vapply(pv[VAR_MV], is.numeric, logical(1))) && !anyNA(pv[VAR_MV]),
            "provinsi: persen dalam 0-100" = all(unlist(pv[VAR_PERSEN]) >= 0 & unlist(pv[VAR_PERSEN]) <= 100),
            "provinsi: klaster & flag_pencilan" = all(c("klaster", "flag_pencilan") %in% names(pv)) && !anyNA(pv$klaster))
  ag <- agg_provinsi(kb); sel <- ag$informal_pct - pv$Informal[match(ag$kode_prov, pv$kode_prov)]
  stopifnot("silang cek: semua kode_prov kab/kota ada di provinsi" = !anyNA(sel))
  if (any(abs(sel) > 1.5))
    stop("Silang cek gagal (>1,5 poin) untuk: ", paste(pv$provinsi[match(ag$kode_prov[abs(sel) > 1.5], pv$kode_prov)], collapse = ", "))
  pk <- o$pca_klaster
  stopifnot("pca_klaster: k terkunci" = !is.na(pk$k) && pk$k %in% 2:6 && identical(sort(unique(pv$klaster)), seq_len(pk$k)))
  if (!setequal(pk$pencilan, c("Papua Pegunungan", "Papua Tengah", "DKI Jakarta")))
    stop("Pencilan pada 9 variabel X = {", paste(pk$pencilan, collapse = ", "),
         "}, tidak sama dengan yang ditetapkan (Papua Pegunungan, Papua Tengah, DKI Jakarta). Laporkan ke Claude sebelum lanjut.")
  mv <- o$multivar
  stopifnot("multivar: dua skenario"  = setequal(names(mv), c("semua", "tanpa_pencilan")),
            "multivar: n semua = 38"  = mv$semua$n == 38,
            "multivar: n tanpa_pencilan = 35" = mv$tanpa_pencilan$n == 35 && !any(pk$pencilan %in% mv$tanpa_pencilan$provinsi))
  for (sk in names(mv)) {
    m <- mv[[sk]]; sub <- pv[match(m$kode_prov, pv$kode_prov), ]
    for (vv in VAR_MV)
      if (!isTRUE(all.equal(m$normalitas$p[m$normalitas$variabel == vv], stats::shapiro.test(sub[[vv]])$p.value)))
        stop(sprintf("multivar[%s]: Shapiro %s tidak cocok shapiro.test()", sk, vv))
    stopifnot("PCA 9 variabel, tanpa Informal" = nrow(m$pca$hasil$loadings) == 9 && !(VAR_Y %in% rownames(m$pca$hasil$loadings)),
              "sum(var_explained) = 1" = isTRUE(all.equal(sum(m$pca$hasil$var_explained), 1)),
              "KMO di (0,1)"           = m$pca$kmo > 0 && m$pca$kmo < 1,
              "Bartlett p di [0,1]"    = m$pca$bartlett[["p"]] >= 0 && m$pca$bartlett[["p"]] <= 1,
              "VIF skor PC = 1"        = all(abs(m$pcr$model$vif - 1) < 1e-6),
              "stepwise subset X"      = all(m$rlb$step$vars %in% VAR_X))
  }
  for (kp in c("32", "93", "94", "95", "96")) {
    bb <- bbox_provinsi(kb, kp)
    if (is.null(bb) || bb[1] < 90 || bb[3] > 145 || bb[2] < -15 || bb[4] > 10)
      stop(sprintf("bbox_provinsi('%s') tidak valid atau di luar wilayah Indonesia", kp))
  }
  invisible(TRUE)
}

bangun_data <- function(path_raw = PATH_RAW, path_rds = PATH_RDS, path_csv = PATH_CSV) {
  message("bangun_data(): memulai ...")
  validasi_fungsi()
  for (pk in c("readxl", "rmapshaper", "spdep", "cluster", "classInt"))
    if (!requireNamespace(pk, quietly = TRUE)) stop("Paket '", pk, "' diperlukan untuk bangun_data(). install.packages('", pk, "')")
  xlsx <- file.path(path_raw, "Dataset.xlsx"); stopifnot(file.exists(xlsx))
  slug <- function(x) gsub("^_|_$", "", gsub("[^a-z0-9]+", "_", tolower(x)))
  
  dend_raw <- readxl::read_excel(xlsx, sheet = "Data Dendogram", range = readxl::cell_cols("A:B"),
                                 col_names = c("label", "nilai"), col_types = c("text", "numeric"))
  dend_raw <- dend_raw[!is.na(dend_raw$nilai), ]
  dend_raw$label <- trimws(dend_raw$label)
  struktur <- dplyr::tribble(
    ~id,              ~induk,           ~level, ~label,
    "usia_kerja",     NA_character_,    1L,     "Penduduk Usia Kerja",
    "angkatan_kerja", "usia_kerja",     2L,     "Angkatan Kerja",
    "bukan_ak",       "usia_kerja",     2L,     "Bukan Angkatan Kerja",
    "bekerja",        "angkatan_kerja", 3L,     "Bekerja",
    "pengangguran",   "angkatan_kerja", 3L,     "Pengangguran",
    "pekerja_penuh",  "bekerja",        4L,     "Pekerja Penuh",
    "pekerja_paruh",  "bekerja",        4L,     "Pekerja Paruh Waktu",
    "setengah_peng",  "bekerja",        4L,     "Setengah Pengangguran")
  stopifnot("dendrogram: label di Excel tidak cocok dengan struktur" =
              nrow(dend_raw) == nrow(struktur) && setequal(dend_raw$label, struktur$label))
  dendrogram <- dplyr::mutate(struktur, juta_orang = dend_raw$nilai[match(label, dend_raw$label)])
  
  sb_raw <- readxl::read_excel(xlsx, sheet = "Data Sunburst", range = readxl::cell_cols("A:D"),
                               col_names = c("kategori", "kelompok", "jk", "nilai"),
                               col_types = c("text", "text", "text", "numeric"))
  sb_raw <- sb_raw[!is.na(sb_raw$nilai), ]
  sb_raw$kategori <- trimws(sb_raw$kategori); sb_raw$kelompok <- trimws(sb_raw$kelompok); sb_raw$jk <- trimws(sb_raw$jk)
  stopifnot("sunburst: jenis kelamin harus Laki-laki/Perempuan" = all(sb_raw$jk[!is.na(sb_raw$jk)] %in% c("Laki-laki", "Perempuan")))
  daun_jk <- sb_raw |> dplyr::filter(!is.na(jk)) |>
    dplyr::transmute(id = paste0(slug(kelompok), "_", ifelse(jk == "Perempuan", "p", "l")), induk = slug(kelompok),
                     label = jk, juta_orang = nilai, juta_perempuan = ifelse(jk == "Perempuan", nilai, 0))
  kelompok_jk <- sb_raw |> dplyr::filter(!is.na(jk)) |> dplyr::group_by(kategori, kelompok) |>
    dplyr::summarise(juta_orang = sum(nilai), juta_perempuan = sum(nilai[jk == "Perempuan"]), .groups = "drop") |>
    dplyr::transmute(id = slug(kelompok), induk = slug(kategori), label = kelompok, juta_orang, juta_perempuan)
  kelompok_tanpa_jk <- sb_raw |> dplyr::filter(is.na(jk)) |>
    dplyr::transmute(id = slug(kelompok), induk = slug(kategori), label = kelompok, juta_orang = nilai, juta_perempuan = NA_real_)
  kelompok <- dplyr::bind_rows(kelompok_jk, kelompok_tanpa_jk)
  kat_lab <- dplyr::distinct(sb_raw, kategori)
  kategori <- kelompok |> dplyr::group_by(id = induk) |>
    dplyr::summarise(juta_orang = sum(juta_orang),
                     juta_perempuan = if (anyNA(juta_perempuan)) NA_real_ else sum(juta_perempuan), .groups = "drop")
  kategori$label <- kat_lab$kategori[match(kategori$id, slug(kat_lab$kategori))]
  kategori$induk <- "usia_kerja"
  root <- dplyr::tibble(id = "usia_kerja", induk = NA_character_, label = "Penduduk Usia Kerja",
                        juta_orang = sum(kategori$juta_orang), juta_perempuan = NA_real_)
  sunburst <- dplyr::bind_rows(root, kategori, kelompok, daun_jk) |>
    dplyr::select(id, induk, label, juta_orang, juta_perempuan) |>
    dplyr::mutate(juta_orang = round(juta_orang, 2), juta_perempuan = round(juta_perempuan, 2))
  sunburst$pct_perempuan <- pct_perempuan(sunburst)
  
  mv <- readxl::read_excel(xlsx, sheet = "Multivariat", skip = 2, .name_repair = "minimal")
  stopifnot("sheet Multivariat: kolom tidak sesuai" = identical(names(mv), c("Provinsi", VAR_MV)))
  mv <- mv[!is.na(mv$Informal), ]
  mv[VAR_MV] <- lapply(mv[VAR_MV], as.numeric)
  tabel_kode <- utils::read.csv(file.path(path_raw, "kode_provinsi.csv"), colClasses = "character", fileEncoding = "UTF-8")
  stopifnot("kode_provinsi.csv: 38 baris, cocok dengan sheet Multivariat" =
              nrow(tabel_kode) == 38 && !anyDuplicated(tabel_kode$provinsi_multivariat) &&
              setequal(tabel_kode$provinsi_multivariat, mv$Provinsi))
  provinsi <- dplyr::rename(mv, provinsi = Provinsi)
  provinsi <- dplyr::mutate(provinsi, kode_prov = tabel_kode$kode_prov[match(provinsi, tabel_kode$provinsi_multivariat)], .before = 1)
  
  old_s2 <- suppressMessages(sf::sf_use_s2(FALSE)); on.exit(suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)
  KOLOM_OVERRIDE <- list(nama = NA, provinsi = NA, kode = NA)
  shp_path <- file.path(path_raw, "LapakGIS_Batas_Kabupaten_2024.shp")
  stopifnot("shapefile tidak lengkap (.shp/.shx/.dbf/.prj; OI-4)" =
              all(file.exists(shp_path, sub("\\.shp$", ".shx", shp_path), sub("\\.shp$", ".dbf", shp_path), sub("\\.shp$", ".prj", shp_path))))
  shp <- sf::st_read(shp_path, quiet = TRUE)
  message("Kolom shapefile: ", paste(setdiff(names(shp), attr(shp, "sf_column")), collapse = ", "))
  kol <- deteksi_kolom_batas(names(shp), KOLOM_OVERRIDE)
  message(sprintf("Kolom dipakai -> nama: %s | provinsi: %s | kode: %s", kol$nama, kol$provinsi, kol$kode))
  batas <- sf::st_sf(kode_kabkota = as.character(shp[[kol$kode]]), nama = as.character(shp[[kol$nama]]),
                     provinsi_shp = as.character(shp[[kol$provinsi]]), geometry = sf::st_geometry(shp))
  batas <- sf::st_make_valid(sf::st_transform(batas, 4326))
  batas <- clean_batas(batas, n_expected = 514L)
  
  gx <- suppressWarnings(readxl::read_excel(xlsx, sheet = "Geospasial", skip = 2, range = readxl::cell_cols("A:E"),
                                            col_names = c("no", "nama", "informal", "total_pekerja", "rasio_xlsx"),
                                            col_types = c("numeric", "text", "numeric", "numeric", "numeric")))
  gx <- gx[!is.na(gx$nama) & !is.na(gx$informal), ]
  gx$nama <- trimws(gx$nama)
  stopifnot("sheet Geospasial: harus 514 kab/kota unik" = nrow(gx) == 514 && !anyDuplicated(gx$nama))
  alias_path <- file.path(path_raw, "alias_nama.csv")
  alias <- if (file.exists(alias_path)) utils::read.csv(alias_path, colClasses = "character", fileEncoding = "UTF-8") else NULL
  kabkota <- join_kabkota(gx, batas, tabel_kode, alias)
  stopifnot("rasio hitung ulang != rasio Excel" = max(abs(kabkota$rasio - gx$rasio_xlsx[match(kabkota$nama, gx$nama)])) < 1e-6)
  
  breaks <- list(jenks5    = classify_breaks(kabkota$rasio, "jenks", 5),
                 quantile5 = classify_breaks(kabkota$rasio, "quantile", 5),
                 equal5    = classify_breaks(kabkota$rasio, "equal", 5))
  message("Menghitung LISA (", NSIM_LISA, " simulasi) ...")
  lisa <- compute_lisa(kabkota, k = K_LISA, nsim = NSIM_LISA, seed = SEED, alpha = ALPHA)
  message(sprintf("Moran's I global = %.3f (p = %.3f)", lisa$global$I, lisa$global$p))
  
  kab_s <- NULL
  for (tol in c(0.002, 0.005, 0.01, 0.02, 0.03, 0.05)) {
    kab_s <- sf::st_simplify(kabkota, preserveTopology = TRUE, dTolerance = tol)
    kab_s <- kab_s[!sf::st_is_empty(kab_s), ]
    mb <- length(serialize(kab_s, NULL)) / 1024^2
    message(sprintf("simplify tol = %.3f -> %.2f MB", tol, mb))
    if (mb < 5) break
  }
  stopifnot("simplify menghilangkan kab/kota" = nrow(kab_s) == nrow(kabkota))
  
  kabkota_csv <- sf::st_drop_geometry(kabkota)
  kabkota <- kab_s
  
  ck <- choose_k(provinsi)
  message("Silhouette per k: ", paste(sprintf("k=%s: %.3f", names(ck$silhouette), ck$silhouette), collapse = " | "))
  message("WSS (elbow) per k: ", paste(sprintf("k=%s: %.1f", names(ck$wss), ck$wss), collapse = " | "))
  if (is.na(K_KLASTER))
    stop("K_KLASTER masih NA. Silhouette terbaik: k = ", ck$k, ". Tempelkan keluaran di atas ke Claude untuk memutuskan, ",
         "lalu isi K_KLASTER di bagian [A2] dan jalankan ulang.")
  provinsi$klaster <- cluster_ward(provinsi, K_KLASTER)
  pencilan <- detect_outliers(provinsi)
  message("Pencilan (VAR_X, |z| > ", Z_PENCILAN, " pada >= ", MIN_VAR_PENCILAN, " variabel): ", paste(pencilan, collapse = ", "))
  provinsi$flag_pencilan <- provinsi$provinsi %in% pencilan
  multivar <- list(semua          = analisis_multivar(provinsi),
                   tanpa_pencilan = analisis_multivar(provinsi[!provinsi$flag_pencilan, ]))
  message(paste(laporan_multivar(multivar$semua, "semua"), collapse = "\n"))
  message(paste(laporan_multivar(multivar$tanpa_pencilan, "tanpa_pencilan"), collapse = "\n"))
  pca_klaster <- list(k = K_KLASTER, k_silhouette_terbaik = ck$k, silhouette = ck$silhouette, wss = ck$wss,
                      klaster = provinsi[, c("kode_prov", "provinsi", "klaster")], pencilan = pencilan)
  
  olahan <- list(dendrogram = dendrogram, sunburst = sunburst, kabkota = kabkota, provinsi = provinsi,
                 breaks = breaks, lisa = lisa, pca_klaster = pca_klaster, multivar = multivar)
  validasi_data(olahan)
  dir.create(dirname(path_rds), showWarnings = FALSE, recursive = TRUE)
  dir.create(path_csv, showWarnings = FALSE, recursive = TRUE)
  saveRDS(olahan, path_rds)
  wr <- function(x, f) utils::write.csv(x, file.path(path_csv, f), row.names = FALSE, fileEncoding = "UTF-8", na = "")
  wr(dendrogram, "dendrogram.csv"); wr(sunburst, "sunburst.csv"); wr(provinsi, "provinsi.csv"); wr(kabkota_csv, "kabkota.csv")
  message(sprintf("bangun_data() selesai: %s (%.1f MB)", path_rds, file.size(path_rds) / 1024^2))
  invisible(olahan)
}

# ==== [A5] muat data ====
if (!file.exists(PATH_RDS)) bangun_data()
DATA <- readRDS(PATH_RDS)

# ==== [A6] aplikasi ====
source("komponen/ui.R",     local = TRUE, encoding = "UTF-8")
source("komponen/server.R", local = TRUE, encoding = "UTF-8")
shinyApp(ui, server)
