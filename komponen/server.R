`%||%` <- function(a, b) if (is.null(a)) b else a

kartu_dendrogram <- function(dendrogram) {
  total <- dendrogram$juta_orang[is.na(dendrogram$induk)]
  buat_li <- function(i) {
    r <- dendrogram[i, ]
    anak <- which(dendrogram$induk == r$id)
    if (is.na(r$induk)) {
      detail <- div("Total penduduk usia kerja")
    } else {
      ind <- match(r$induk, dendrogram$id)
      detail <- tagList(
        div(sprintf("%s%% dari %s", fmt_id(r$juta_orang / dendrogram$juta_orang[ind] * 100, 1), dendrogram$label[ind])),
        div(sprintf("%s%% dari usia kerja", fmt_id(r$juta_orang / total * 100, 1))))
    }
    kartu <- div(
      class = paste("kartu", paste0("lv", r$level)), tabindex = "0", role = "group", `data-id` = r$id,
      `aria-label` = sprintf("%s: %s juta orang", r$label, fmt_id(r$juta_orang, 2)),
      div(class = "nama", r$label),
      div(class = "angka", fmt_id(r$juta_orang, 2)),
      div(class = "satuan", "juta orang"),
      div(class = "detail", detail))
    tags$li(kartu, if (length(anak)) tags$ul(lapply(anak, buat_li)))
  }
  akar <- which(is.na(dendrogram$induk))
  div(class = "dendro-wrap", div(class = "dendro", tags$ul(lapply(akar, buat_li))))
}

LISA_KAT <- DATA$lisa$local$kategori[match(KAB_D$kode_kabkota, DATA$lisa$local$kode_kabkota)]
TIP_KAB      <- lapply(teks_tooltip(KAB_D), htmltools::HTML)
TIP_KAB_LISA <- lapply(paste0(teks_tooltip(KAB_D), "<br>Klaster spasial: ", LABEL_LISA[LISA_KAT]), htmltools::HTML)
PUSAT_KAB <- local({
  old <- suppressMessages(sf::sf_use_s2(FALSE)); on.exit(suppressMessages(sf::sf_use_s2(old)))
  sf::st_coordinates(suppressWarnings(sf::st_point_on_surface(sf::st_geometry(DATA$kabkota))))
})

petakan_kartu <- function(id_kartu, dendrogram, sunburst) {
  if (is.na(match(id_kartu, dendrogram$id))) return(NULL)
  kunci <- function(x) tolower(trimws(x))
  cur <- id_kartu; jenis <- "tepat"; sid <- NA_character_
  repeat {
    j <- match(kunci(dendrogram$label[match(cur, dendrogram$id)]), kunci(sunburst$label))
    if (!is.na(j)) { sid <- sunburst$id[j]; break }
    jenis <- "pendekatan"
    cur <- dendrogram$induk[match(cur, dendrogram$id)]
    if (is.na(cur)) { sid <- sunburst$id[is.na(sunburst$induk)][1]; break }
  }
  if (!(sid %in% sunburst$induk)) {
    sid <- sunburst$induk[match(sid, sunburst$id)]
    if (jenis == "tepat") jenis <- "daun"
  }
  list(target = sid, jenis = jenis, label_kartu = dendrogram$label[match(id_kartu, dendrogram$id)],
       label_target = sunburst$label[match(sid, sunburst$id)])
}

server <- function(input, output, session) {
  
  # ==== [S0] state bersama ====
  selected_prov <- reactiveVal(NULL)
  zoom_to_prov  <- reactiveVal(NULL)
  
  # ==== [S1] Beranda ====
  pindah <- function(tujuan) bslib::nav_select("navbar", tujuan, session = session)
  observeEvent(input$mulai_bab1, pindah("bab1"))
  observeEvent(input$mulai_bab2, pindah("bab2"))
  observeEvent(input$mulai_bab3, pindah("bab3"))
  
  # ==== [S2] Bab 1 — hierarki ====
  output$b1_dendro <- renderUI(kartu_dendrogram(DATA$dendrogram))
  
  sunburst <- DATA$sunburst
  sb   <- build_sunburst_data(sunburst)
  akar <- sunburst$id[is.na(sunburst$induk)]
  fokus <- reactiveVal(akar)
  
  output$b1_sunburst <- plotly::renderPlotly({
    p <- plotly::plot_ly(
      type = "sunburst", source = "sunburst",
      ids = sb$ids, labels = sb$labels, parents = sb$parents, values = sb$values,
      branchvalues = "total", sort = FALSE,
      marker = list(colors = warna_pct_perempuan(sb$pct_perempuan, MID_PEREMPUAN,
                                                 lo = MID_PEREMPUAN - LEBAR_WARNA_PDR, hi = MID_PEREMPUAN + LEBAR_WARNA_PDR),
                    line = list(color = "#FFFFFF", width = 2)),
      text = sb$text, textinfo = "label+text",
      hovertext = sb$hover, hoverinfo = "text", hoverlabel = list(align = "left"))
    p <- plotly::layout(p, margin = list(l = 0, r = 0, t = 8, b = 8), paper_bgcolor = "rgba(0,0,0,0)", separators = ",.")
    p <- plotly::config(p, displayModeBar = FALSE)
    htmlwidgets::onRender(p, JS_KLIK_SUNBURST, data = list(input = "b1_klik"))
  })
  
  # klik pusat -> naik satu level; klik irisan bercabang -> masuk; daun -> abaikan
  observeEvent(input$b1_klik, {
    klik <- input$b1_klik; sekarang <- fokus()
    if (!(klik %in% sunburst$id)) return()
    if (identical(klik, sekarang)) {
      induk <- sunburst$induk[match(klik, sunburst$id)]
      if (!is.na(induk)) fokus(induk)
    } else if (klik %in% sunburst$induk) fokus(klik)
  })
  kartu_aktif <- reactiveVal(NULL)
  observeEvent(input$b1_kartu, {
    h <- petakan_kartu(input$b1_kartu, DATA$dendrogram, sunburst)
    req(!is.null(h))
    kartu_aktif(h)
    fokus(h$target)
    session$sendCustomMessage("hierarki_level", list(id = "b1_sunburst", level = h$target))   # tetap jalan bila fokus tak berubah
    session$sendCustomMessage("kartu_sorot", list(id = input$b1_kartu))
  })
  observeEvent(fokus(), {
    h <- kartu_aktif()
    if (!is.null(h) && !identical(h$target, fokus())) {
      kartu_aktif(NULL); session$sendCustomMessage("kartu_sorot", list(id = ""))
    }
  }, ignoreInit = TRUE)
  output$b1_kartu_info <- renderUI({
    h <- kartu_aktif(); req(!is.null(h))
    teks <- switch(h$jenis,
                   tepat      = sprintf("Sunburst difokuskan pada cabang \u201C%s\u201D (dipilih dari dendrogram).", h$label_target),
                   daun       = sprintf("\u201C%s\u201D tidak punya rincian lebih lanjut di sunburst; ditampilkan induknya, \u201C%s\u201D.", h$label_kartu, h$label_target),
                   pendekatan = sprintf("Sunburst tidak merinci \u201C%s\u201D; ditampilkan cabang \u201C%s\u201D yang memuatnya.", h$label_kartu, h$label_target))
    p(class = "small text-muted mb-1", role = "status", icon("circle-info"), " ", teks)
  })
  observeEvent(input$b1_crumb, fokus(input$b1_crumb))
  observeEvent(input$b1_reset, fokus(akar))
  observeEvent(fokus(), {
    session$sendCustomMessage("hierarki_level", list(id = "b1_sunburst", level = fokus()))
  }, ignoreInit = TRUE)
  
  output$b1_breadcrumb <- renderUI({
    jalur <- jalur_simpul(sunburst, fokus())
    item <- lapply(seq_along(jalur), function(i) {
      id_i <- jalur[i]; label <- sunburst$label[match(id_i, sunburst$id)]
      if (i == length(jalur)) {
        tags$li(class = "breadcrumb-item active", `aria-current` = "page", label)
      } else {
        tags$li(class = "breadcrumb-item",
                tags$a(href = "#", label,
                       onclick = sprintf("Shiny.setInputValue('b1_crumb','%s',{priority:'event'});return false;", id_i)))
      }
    })
    tags$nav(`aria-label` = "breadcrumb", tags$ol(class = "breadcrumb mb-0", item))
  })
  
  # ==== [S3] Bab 2 — peta ====
  kabkota <- DATA$kabkota
  d <- KAB_D
  n <- nrow(d)
  lisa_kat    <- LISA_KAT
  labels      <- TIP_KAB
  labels_lisa <- TIP_KAB_LISA
  pusat       <- PUSAT_KAB
  pal <- PALET_URUT5
  
  terpilih <- reactiveVal(NULL)
  siap <- reactiveVal(FALSE)
  observeEvent(input$b2_peta_bounds, siap(TRUE), once = TRUE)
  
  brk <- reactive({ req(input$b2_gaya); DATA$breaks[[input$b2_gaya]] })
  
  output$b2_peta <- leaflet::renderLeaflet({
    leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 4, preferCanvas = TRUE)) |>
      leaflet::addTiles(
        urlTemplate = "https://tile.openstreetmap.org/{z}/{x}/{y}.png",
        attribution = "&copy; <a href='https://www.openstreetmap.org/copyright' target='_blank'>OpenStreetMap</a> contributors",
        options = leaflet::tileOptions(maxZoom = 19)) |>
      leaflet::fitBounds(BBOX_INDONESIA[1], BBOX_INDONESIA[2], BBOX_INDONESIA[3], BBOX_INDONESIA[4])
  })
  
  observe({
    req(siap(), input$b2_dasar)
    dasar <- input$b2_dasar
    b <- brk()
    aktif <- dalam_filter(d, input$b2_prov)
    kelas <- kelas_dari_breaks(d$rasio, b)
    
    warna <- rep(WARNA_LUAR_FILTER, n)
    warna[aktif] <- switch(dasar,
                           choro = pal[kelas][aktif],
                           lisa  = unname(WARNA_LISA[lisa_kat])[aktif],
                           WARNA_NETRAL)
    tip <- if (dasar == "lisa") labels_lisa else labels
    
    proxy <- leaflet::leafletProxy("b2_peta", session) |>
      leaflet::clearGroup("poligon") |> leaflet::clearGroup("simbol") |> leaflet::clearControls() |>
      leaflet::addPolygons(
        data = kabkota, group = "poligon", layerId = ~kode_kabkota,
        fillColor = warna, fillOpacity = ifelse(aktif, 0.85, 0.3),
        color = "#FFFFFF", weight = 0.6, opacity = 1,
        label = tip, labelOptions = leaflet::labelOptions(textsize = "13px"),
        highlightOptions = leaflet::highlightOptions(weight = 3, color = "#000000", bringToFront = FALSE))
    
    if (isTRUE(input$b2_simbol) && any(aktif)) {
      proxy <- proxy |>
        leaflet::addCircleMarkers(
          lng = pusat[aktif, 1], lat = pusat[aktif, 2], group = "simbol", layerId = d$kode_kabkota[aktif],
          radius = radius_simbol(d$informal[aktif], r_max = R_MAKS_SIMBOL, x_max = max(d$informal)),
          stroke = TRUE, color = "#000000", weight = 1, fillColor = WARNA_SIMBOL, fillOpacity = 0.55,
          label = tip[aktif], labelOptions = leaflet::labelOptions(textsize = "13px"))
      refs <- c(1e5, 5e5, 1e6); refs <- refs[refs <= max(d$informal)]
      proxy <- leaflet::addControl(proxy, html = htmltools::HTML(html_legenda_simbol(refs, max(d$informal), R_MAKS_SIMBOL)),
                                   position = "bottomleft")
    }
    if (dasar %in% c("choro", "lisa")) {
      if (dasar == "choro") { warna_leg <- pal; label_leg <- label_kelas(b); judul_leg <- "Rasio pekerja informal (%)" }
      else { warna_leg <- unname(WARNA_LISA); label_leg <- unname(LABEL_LISA); judul_leg <- "Klaster spasial (LISA)" }
      if (!all(aktif)) { warna_leg <- c(warna_leg, WARNA_LUAR_FILTER); label_leg <- c(label_leg, "Di luar filter") }
      leaflet::addLegend(proxy, position = "bottomright", colors = warna_leg, labels = label_leg, title = judul_leg, opacity = 0.9)
    }
  })
  
  observe({
    req(siap())
    kode <- terpilih()
    proxy <- leaflet::leafletProxy("b2_peta", session) |> leaflet::clearGroup("sorot")
    i <- if (is.null(kode)) NA_integer_ else match(kode, d$kode_kabkota)
    if (!is.na(i)) {
      proxy <- leaflet::addPolygons(proxy, data = kabkota[i, ], group = "sorot", fill = FALSE, color = "#FFFFFF",
                                    weight = 7, opacity = 1, options = leaflet::pathOptions(interactive = FALSE))
      leaflet::addPolygons(proxy, data = kabkota[i, ], group = "sorot", fill = FALSE, color = "#000000",
                           weight = 3, opacity = 1, options = leaflet::pathOptions(interactive = FALSE))
    }
  })
  observeEvent(input$b2_peta_shape_click,  terpilih(input$b2_peta_shape_click$id))
  observeEvent(input$b2_peta_marker_click, terpilih(input$b2_peta_marker_click$id))
  
  observeEvent(input$b2_cari, {
    req(nzchar(input$b2_cari))
    i <- match(input$b2_cari, d$nama); req(!is.na(i))
    terpilih(d$kode_kabkota[i])
    leaflet::flyTo(leaflet::leafletProxy("b2_peta", session), lng = pusat[i, 1], lat = pusat[i, 2], zoom = 9)
  })
  
  zoom_ke_prov <- function(kode) {
    bb <- bbox_provinsi(kabkota, kode)
    if (is.null(bb)) return(invisible())
    leaflet::fitBounds(leaflet::leafletProxy("b2_peta", session), bb[1], bb[2], bb[3], bb[4])
  }
  observeEvent(input$b2_prov, {
    req(siap())
    if (identical(input$b2_prov, "semua")) {
      leaflet::fitBounds(leaflet::leafletProxy("b2_peta", session),
                         BBOX_INDONESIA[1], BBOX_INDONESIA[2], BBOX_INDONESIA[3], BBOX_INDONESIA[4])
    } else zoom_ke_prov(input$b2_prov)
  }, ignoreInit = TRUE)
  
  observe({
    req(siap())
    kode <- zoom_to_prov(); req(!is.null(kode), !is.na(kode))
    updateSelectInput(session, "b2_prov", selected = kode)
    zoom_ke_prov(kode)
  })
  
  observeEvent(input$b2_reset, {
    updateRadioButtons(session, "b2_gaya", selected = "jenks5")
    updateSelectInput(session, "b2_prov", selected = "semua")
    updateSelectizeInput(session, "b2_cari", selected = "")
    terpilih(NULL)
    zoom_to_prov(NULL)
  })
  
  output$b2_hist <- plotly::renderPlotly({
    garis <- lapply(brk() * 100, function(x)
      list(type = "line", x0 = x, x1 = x, y0 = 0, y1 = 1, yref = "paper", line = list(color = "#000000", width = 2, dash = "dash")))
    p <- plotly::plot_ly(x = d$rasio * 100, type = "histogram", nbinsx = 30, source = "hist",
                         marker = list(color = "#56B4E9", line = list(color = "#FFFFFF", width = 1)),
                         hovertemplate = "%{y} kab/kota<extra></extra>")
    p <- plotly::layout(p, shapes = garis, separators = ",.", bargap = 0.02, font = list(color = "#000000"),
                        xaxis = list(title = "Rasio pekerja informal (%)"), yaxis = list(title = "Jumlah kab/kota"),
                        margin = list(l = 50, r = 10, t = 10, b = 45), paper_bgcolor = "rgba(0,0,0,0)")
    plotly::config(p, displayModeBar = FALSE)
  })
  
  output$b2_panel <- renderUI({
    r <- if (is.null(terpilih())) NULL else ringkas_kabkota(d, terpilih())
    if (is.null(r)) return(p(class = "text-muted", "Klik sebuah kabupaten/kota di peta, atau cari lewat kotak pencarian."))
    selisih <- (r$rasio - r$rata_prov) * 100
    banding <- if (abs(selisih) < 0.05) "sama dengan" else
      sprintf("%s poin %s dari", fmt_id(abs(selisih), 1), if (selisih > 0) "di atas" else "di bawah")
    kat <- lisa_kat[match(r$nama, d$nama)]
    tagList(
      p(class = "panel-nama", r$nama), p(class = "text-muted panel-prov", r$provinsi),
      tags$table(class = "table table-sm panel-klik",
                 tags$tr(tags$td("Rasio informal"), tags$td(tags$b(sprintf("%s%%", fmt_id(r$rasio * 100, 1))))),
                 tags$tr(tags$td("Pekerja informal"), tags$td(sprintf("%s orang dari %s pekerja", fmt_id(r$informal), fmt_id(r$total)))),
                 tags$tr(tags$td("Peringkat nasional"), tags$td(sprintf("ke-%s dari %s (1 = rasio tertinggi)", fmt_id(r$peringkat), fmt_id(r$n)))),
                 tags$tr(tags$td("Rata-rata provinsi"), tags$td(sprintf("%s%% (kab/kota ini %s rata-rata provinsi)", fmt_id(r$rata_prov * 100, 1), banding))),
                 tags$tr(tags$td("Rata-rata nasional"), tags$td(sprintf("%s%%", fmt_id(r$rata_nasional * 100, 1)))),
                 tags$tr(tags$td("Klaster spasial"), tags$td(LABEL_LISA[[kat]]))),
      if (r$ekstrem) p(class = "small", "\u26A0 Nilai ekstrem: di luar 1,5 \u00D7 IQR sebaran nasional."))
  })
  
  tabel_top <- function(tinggi) {
    t <- top_n_kabkota(d, 10, tinggi)
    ek <- d$flag_ekstrem[match(t$nama, d$nama)]
    data.frame(`#` = t$peringkat, `Kab/kota` = paste0(t$nama, ifelse(ek, " \u26A0", "")),
               Provinsi = t$provinsi, `Rasio (%)` = fmt_id(t$rasio * 100, 1), check.names = FALSE)
  }
  output$b2_top_tinggi <- renderTable(tabel_top(TRUE),  striped = TRUE, spacing = "xs", width = "100%", align = "rllr")
  output$b2_top_rendah <- renderTable(tabel_top(FALSE), striped = TRUE, spacing = "xs", width = "100%", align = "rllr")
  
  output$b2_lisa_teks <- renderUI({
    g <- DATA$lisa$global
    arah <- if (g$I > 0) "kab/kota dengan rasio serupa cenderung berdekatan (mengelompok secara spasial)" else "kab/kota bertetangga cenderung berbeda rasionya"
    sig  <- if (g$p < ALPHA) "signifikan" else "tidak signifikan"
    tagList(
      p(tags$b(sprintf("Moran's I global = %s (p = %s). ", fmt_id(g$I, 3), fmt_id(g$p, 3))),
        sprintf("Pola ini %s pada \u03B1 = %s: %s.", sig, fmt_id(ALPHA, 2), arah)),
      p(class = "text-muted", "Jika rasio informal mengelompok di wilayah tertentu, wilayah itu mungkin berbagi kondisi sosial-ekonomi yang sama. ",
        "Bab 3 menelusuri faktor apa saja yang berasosiasi dengan informalitas di tingkat provinsi."))
  })
  output$b2_lisa_tab <- renderTable({
    tab <- as.numeric(table(factor(DATA$lisa$local$kategori, levels = names(LABEL_LISA))))
    data.frame(Kategori = sprintf('<span style="display:inline-block;width:.9rem;height:.9rem;background:%s;border-radius:3px;margin-right:.4rem;vertical-align:-2px;"></span>%s',
                                  WARNA_LISA, LABEL_LISA),
               `Jumlah kab/kota` = fmt_id(tab), check.names = FALSE)
  }, sanitize.text.function = function(x) x, striped = TRUE, spacing = "xs", align = "lr")
  
  # ==== [S4] Bab 3 — alur analisis (hasil dihitung di bangun_data(); server hanya memilih skenario) ====
  prov <- DATA$provinsi
  K <- DATA$pca_klaster$k
  
  sertakan <- reactive(is.null(input$b3_sertakan) || isTRUE(input$b3_sertakan))
  M  <- reactive(DATA$multivar[[if (sertakan()) "semua" else "tanpa_pencilan"]])
  ps <- reactive(prov[match(M()$kode_prov, prov$kode_prov), ])
  
  skala_div <- list(list(0, "#0072B2"), list(0.5, "#FFFFFF"), list(1, "#D55E00"))
  hc_baris <- hclust_ward(prov); hc_baris$labels <- prov$provinsi
  DEN_BARIS <- stats::as.dendrogram(hc_baris)
  
  anotasi_sel <- function(z, x, y, teks, zmax) {
    g <- expand.grid(i = seq_along(y), j = seq_along(x))
    lapply(seq_len(nrow(g)), function(r) list(
      x = x[g$j[r]], y = y[g$i[r]], text = teks[g$i[r], g$j[r]], showarrow = FALSE,
      font = list(size = 11, color = if (z[g$i[r], g$j[r]] < -0.6 * zmax) "#FFFFFF" else "#000000")))
  }
  df_titik <- function(x, y, hover) {
    d <- ps()
    data.frame(kode_prov = d$kode_prov, provinsi = d$provinsi, klaster = d$klaster, x = x, y = y, hover = hover,
               sorot = apply_selection(d, selected_prov()), stringsAsFactors = FALSE)
  }
  titik_klaster <- function(p, df) {
    ada <- length(selected_prov()) > 0
    for (j in sort(unique(df$klaster))) {
      s <- df[df$klaster == j, ]
      p <- plotly::add_markers(p, data = s, x = ~x, y = ~y, key = ~kode_prov, text = ~hover, hoverinfo = "text",
                               name = paste("Klaster", j), legendgroup = paste0("k", j),
                               marker = list(color = PALET_KLASTER[j], symbol = SIMBOL_KLASTER[j], size = ifelse(s$sorot, 16, 9),
                                             opacity = ifelse(ada & !s$sorot, 0.35, 1),
                                             line = list(color = "#000000", width = ifelse(s$sorot, 3, 0.6))))
    }
    p
  }
  garis_qq <- function(x) {
    qy <- stats::quantile(x, c(.25, .75), names = FALSE); qx <- stats::qnorm(c(.25, .75))
    b <- diff(qy) / diff(qx); a <- qy[1] - b * qx[1]
    function(xx) a + b * xx
  }
  # margin dijadikan argumen sendiri (bawaan t = 10) lalu digabung dengan margin pemanggil,
  # sehingga layout() tidak pernah menerima argumen `margin` ganda.
  tata_dasar <- function(p, ..., margin = list()) {
    mg <- utils::modifyList(list(t = 10), margin)
    plotly::config(plotly::layout(p, separators = ",.", font = list(color = "#000000"),
                                  legend = list(orientation = "h", y = -0.2), margin = mg, ...),
                   displaylogo = FALSE)
  }
  
  lapply(1:5, function(i) observeEvent(input[[paste0("b3_step_", i)]], bslib::nav_select("b3_langkah", paste0("l", i), session = session)))
  observeEvent(input$b3_langkah, session$sendCustomMessage("stepper_aktif", list(i = as.integer(sub("l", "", input$b3_langkah)))))
  
  output$b3_pencilan_teks <- renderUI({
    p(class = "cara-baca",
      sprintf("Pencilan (|z| > %s pada minimal %d variabel X): %s. ", fmt_id(Z_PENCILAN, 1), MIN_VAR_PENCILAN, paste(DATA$pca_klaster$pencilan, collapse = ", ")),
      sprintf("Skenario aktif: %s provinsi. ", fmt_id(M()$n)),
      "Bila pencilan dikeluarkan, Langkah 1\u20135 memakai hasil hitung ulang; keanggotaan dan warna klaster tidak berubah.")
  })
  
  output$b3_ket_norm <- renderUI({
    nm <- M()$normalitas; ok <- nm[nm$normal, ]; tdk <- nm[!nm$normal, ]; n <- nrow(nm)
    jauh <- tdk[which.min(tdk$W), ]
    tags$ul(class = "mt-3 mb-1 ket-norm",
            tags$li(tags$b("Hasil uji: "), sprintf("%s dari %s variabel lolos uji kenormalan Shapiro-Wilk pada taraf %s%% (p > %s).",
                                                   fmt_id(nrow(ok)), fmt_id(n), fmt_id(ALPHA_NORMAL * 100), fmt_id(ALPHA_NORMAL, 2))),
            tags$li(tags$b("Berdistribusi normal: "),
                    if (nrow(ok)) paste(sprintf("%s (W = %s; p = %s)", ok$variabel, fmt_id(ok$W, 3), fmt_p(ok$p)), collapse = ", ") else "tidak ada."),
            tags$li(tags$b("Tidak berdistribusi normal: "),
                    if (nrow(tdk)) paste0(paste(tdk$variabel, collapse = ", "), ". Penyimpangan terbesar pada ", jauh$variabel, " (W = ", fmt_id(jauh$W, 3), ").") else "tidak ada."),
            tags$li(tags$b("Konsekuensi: "), "pada Tahap 2, korelasi antar dua variabel memakai Pearson hanya bila keduanya normal; selain itu memakai Spearman."))
  })
  output$b3_tab_norm <- renderTable({
    nm <- M()$normalitas; dg <- ifelse(nm$maks >= 1000, 0, 2)
    f <- function(x) mapply(fmt_id, x, dg)
    data.frame(Variabel = label_resp(nm$variabel), n = fmt_id(nm$n), Min = f(nm$min), Q1 = f(nm$q1), Median = f(nm$median),
               `Rata-rata` = f(nm$rata), Q3 = f(nm$q3), Maks = f(nm$maks), SD = f(nm$sd),
               W = fmt_id(nm$W, 3), p = fmt_p(nm$p), Status = nm$status, check.names = FALSE)
  }, striped = TRUE, spacing = "xs", align = "l")
  
  dg_var <- function(v) if (v %in% VAR_PERSEN) 1 else if (v == "RLS") 2 else 0
  sat_var <- function(v) if (v %in% VAR_PERSEN) "%" else ""
  fv <- function(x, v) paste0(fmt_id(x, dg_var(v)), sat_var(v))
  output$b3_ket_hist <- renderUI({
    v <- input$b3_var_norm; req(v); x <- ps()[[v]]; r <- M()$normalitas; r <- r[r$variabel == v, ]; req(nrow(r) == 1)
    sk <- mean((x - mean(x))^3) / stats::sd(x)^3
    bentuk <- if (sk > 0.5) "miring ke kanan" else if (sk < -0.5) "miring ke kiri" else "cukup simetris"
    tags$ul(class = "small mt-2 mb-0",
            tags$li(tags$b("Bentuk: "), sprintf("%s; median %s, rata-rata %s, rentang %s\u2013%s.", bentuk, fv(stats::median(x), v), fv(mean(x), v), fv(min(x), v), fv(max(x), v))),
            tags$li(tags$b("Uji: "), sprintf("W = %s, p = %s \u2192 %s.", fmt_id(r$W, 3), fmt_p(r$p), if (r$normal) "normal" else "tidak normal")))
  })
  output$b3_ket_qq <- renderUI({
    v <- input$b3_var_norm; req(v); x <- ps()[[v]]; d <- ps(); r <- M()$normalitas; r <- r[r$variabel == v, ]; req(nrow(r) == 1)
    q <- stats::qqnorm(x, plot.it = FALSE); dev <- x - garis_qq(x)(q$x); o <- order(abs(dev), decreasing = TRUE)[1:2]
    tags$ul(class = "small mt-2 mb-0",
            tags$li(tags$b("Pola: "), if (r$normal) "titik mengikuti garis acuan." else "bagian tengah dekat garis, ujung sebaran menyimpang (ekor lebih berat dari normal)."),
            if (r$normal) tags$li(tags$b("Uji: "), sprintf("p = %s, tidak menolak kenormalan.", fmt_p(r$p)))
            else tags$li(tags$b("Terjauh dari garis: "), sprintf("%s (%s) dan %s (%s).", d$provinsi[o[1]], fv(x[o[1]], v), d$provinsi[o[2]], fv(x[o[2]], v))))
  })
  output$b3_hist <- plotly::renderPlotly({
    v <- input$b3_var_norm; req(v); x <- ps()[[v]]; dn <- stats::density(x); xs <- seq(min(x), max(x), length.out = 120)
    p <- plotly::plot_ly() |>
      plotly::add_histogram(x = x, histnorm = "probability density", nbinsx = 12, name = "Histogram",
                            marker = list(color = "#56B4E9", line = list(color = "#FFFFFF", width = 1)), hoverinfo = "skip") |>
      plotly::add_lines(x = dn$x, y = dn$y, name = "Densitas data", line = list(color = "#D55E00", width = 2.5), hoverinfo = "skip") |>
      plotly::add_lines(x = xs, y = stats::dnorm(xs, mean(x), stats::sd(x)), name = "Kurva normal",
                        line = list(color = "#000000", width = 1.5, dash = "dash"), hoverinfo = "skip")
    tata_dasar(p, xaxis = list(title = label_resp(v)), yaxis = list(title = "Kepadatan"), bargap = 0.03)
  })
  output$b3_qq <- plotly::renderPlotly({
    v <- input$b3_var_norm; req(v); x <- ps()[[v]]; q <- stats::qqnorm(x, plot.it = FALSE); g <- garis_qq(x); xr <- range(q$x)
    p <- plotly::plot_ly() |>
      plotly::add_markers(x = q$x, y = q$y, name = "Provinsi", text = sprintf("%s<br>%s", ps()$provinsi, fmt_id(x, 2)), hoverinfo = "text",
                          marker = list(color = "#0072B2", size = 8, line = list(color = "#000000", width = 0.5))) |>
      plotly::add_lines(x = xr, y = g(xr), name = "Garis acuan", line = list(color = "#000000", dash = "dash"), hoverinfo = "skip")
    tata_dasar(p, xaxis = list(title = "Kuantil normal teoretis"), yaxis = list(title = label_resp(v)))
  })
  
  output$b3_ket_bar <- renderUI({
    kr <- M()$korelasi$xy; b <- kr[1, ]
    p(class = "keterangan",
      sprintf("%s paling kuat berkorelasi dengan Informal (r = %s), dan %s dari 9 variabel berkorelasi signifikan pada \u03B1 = %s%%. Batang biru menunjukkan korelasi positif, sedangkan batang oranye menunjukkan korelasi negatif.",
              b$variabel, fmt_s(b$r, 2), fmt_id(sum(kr$p < ALPHA_REG)), fmt_id(ALPHA_REG * 100)))
  })
  output$b3_kor_bar <- plotly::renderPlotly({
    dk <- M()$korelasi$xy; dk$variabel <- factor(dk$variabel, levels = rev(dk$variabel))
    teks <- paste0(fmt_s(dk$r, 2), " (", dk$metode, ")")
    p <- plotly::plot_ly(dk, x = ~r, y = ~variabel, type = "bar", orientation = "h", source = "korelasi",
                         marker = list(color = ifelse(dk$r >= 0, OKABE[["biru"]], OKABE[["vermilion"]])),
                         text = teks, textposition = "outside", cliponaxis = FALSE,
                         hovertext = sprintf("r = %s (%s), p = %s", fmt_s(dk$r, 2), dk$metode, fmt_p(dk$p)), hoverinfo = "text")
    tata_dasar(p, xaxis = list(title = "Korelasi dengan Informal (r)", range = c(-1.5, 1.5)), yaxis = list(title = ""), showlegend = FALSE)
  })
  output$b3_ket_cm <- renderUI({
    xx <- M()$korelasi$xx_tinggi
    p(class = "keterangan",
      if (!nrow(xx)) "Tidak ada pasangan variabel X dengan |r| \u2265 0,8, sehingga tidak ada indikasi multikolinearitas berat. Warna oranye menunjukkan korelasi positif dan warna biru menunjukkan korelasi negatif."
      else sprintf("%s pasangan variabel X berkorelasi kuat dengan |r| \u2265 0,8 (bertanda \u2021), yaitu %s, sehingga ada indikasi multikolinearitas. Warna oranye menunjukkan korelasi positif dan warna biru menunjukkan korelasi negatif.",
                   fmt_id(nrow(xx)), paste(sprintf("%s\u2013%s (%s)", xx$v1, xx$v2, fmt_s(xx$r, 2)), collapse = "; ")))
  })
  output$b3_kor_heat <- plotly::renderPlotly({
    kr <- M()$korelasi; vars <- rownames(kr$r); n <- length(vars); lab <- label_resp(vars)
    teks <- matrix(fmt_id(as.vector(kr$r), 2), n, n)
    for (a in seq_len(nrow(kr$xx_tinggi))) {
      i <- match(kr$xx_tinggi$v1[a], vars); j <- match(kr$xx_tinggi$v2[a], vars)
      teks[i, j] <- paste0(teks[i, j], "\u2021"); teks[j, i] <- teks[i, j]
    }
    ht <- matrix(sprintf("%s \u00D7 %s<br>r = %s (%s), p = %s", rep(vars, times = n), rep(vars, each = n),
                         fmt_s(as.vector(kr$r), 2), as.vector(kr$metode), fmt_p(as.vector(kr$p))), n, n)
    p <- plotly::plot_ly(x = lab, y = lab, z = kr$r, type = "heatmap", colorscale = skala_div, zmin = -1, zmax = 1,
                         text = ht, hoverinfo = "text", colorbar = list(title = "r"))
    tata_dasar(p, annotations = anotasi_sel(kr$r, lab, lab, teks, 1), yaxis = list(autorange = "reversed", title = ""),
               xaxis = list(title = "", tickangle = -40), margin = list(t = 10, l = 100, b = 90))
  })
  output$b3_ket_sc <- renderUI({
    kr <- M()$korelasi$xy; r <- kr[kr$variabel == input$b3_var_x, ]; req(nrow(r) == 1)
    p(class = "keterangan",
      sprintf("%s berkorelasi %s dengan Informal (r = %s, korelasi %s, p %s) pada %s provinsi. Warna dan bentuk titik menunjukkan kluster provinsi, sedangkan garis putus-putus menunjukkan tren linear.",
              r$variabel, if (r$r >= 0) "positif" else "negatif", fmt_s(r$r, 2), r$metode,
              if (r$p < 0.001) "< 0,001" else paste0("= ", fmt_p(r$p)), fmt_id(M()$n)))
  })
  output$b3_sc <- plotly::renderPlotly({
    v <- input$b3_var_x; req(v); d <- ps(); x <- d[[v]]; y <- d[[VAR_Y]]
    df <- df_titik(x, y, sprintf("%s<br>%s: %s<br>Informal: %s%%", d$provinsi, v, fmt_id(x, 2), fmt_id(y, 1)))
    ft <- stats::lm(y ~ x); xr <- range(x)
    p <- titik_klaster(plotly::plot_ly(source = "scatter"), df) |>
      plotly::add_lines(x = xr, y = stats::coef(ft)[1] + stats::coef(ft)[2] * xr, name = "Garis tren linear",
                        line = list(color = "#000000", width = 2, dash = "dash"), hoverinfo = "skip", inherit = FALSE)
    tata_dasar(p, xaxis = list(title = v), yaxis = list(title = "Informal (%)"))
  })
  
  output$b3_ket_rlb <- renderUI({
    m <- M(); f <- m$rlb$full; s <- m$rlb$step
    p(class = "keterangan",
      sprintf("%s dari 9 variabel signifikan di full model (Adj R\u00B2 = %s); stepwise menyisakan %s variabel (Adj R\u00B2 = %s) pada \u03B1 = %s%%. Variabel signifikan bila p < %s; \u201CTidak masuk model\u201D berarti variabel dikeluarkan oleh seleksi stepwise (AIC). Koefisien b positif berarti Informal naik seiring kenaikan variabel tersebut, negatif berarti turun.",
              fmt_id(sum(f$koef$signifikan[-1])), fmt_id(f$adj_r2, 3), fmt_id(length(s$vars)), fmt_id(s$adj_r2, 3), fmt_id(ALPHA_REG * 100), fmt_id(ALPHA_REG, 2)))
  })
  output$b3_rumus <- renderUI({
    f <- M()$rlb$full$koef; st <- M()$rlb$step$koef
    persamaan <- function(k) {
      i0 <- match("(Intercept)", k$variabel); kk <- k[k$variabel != "(Intercept)", ]
      suku <- unlist(lapply(seq_len(nrow(kk)), function(j) list(
        tags$span(if (kk$b[j] < 0) " \u2212 " else " + "),
        tags$span(fmt_id(abs(kk$b[j]), 3), tags$span("\u00B7"), tags$i(kk$variabel[j])))), recursive = FALSE)
      div(class = "rumus-rlb", tags$i("\u0176"), " = ", fmt_id(k$b[i0], 3), suku)
    }
    ks <- st[st$variabel != "(Intercept)", ]
    tags$div(
      p(class = "small mt-1", tags$b("Model stepwise (AIC)"), " \u2014 model terpilih:"),
      persamaan(st),
      p(class = "small mt-2", tags$b("Full model"), " \u2014 seluruh 9 faktor:"),
      persamaan(f),
      p(class = "small mt-2", tags$b("Arti koefisien model stepwise:")),
      tags$ul(
        tags$li(HTML(sprintf("<i>\u0176</i> adalah dugaan persentase pekerja informal, dan intersep %s adalah dugaannya bila semua variabel bernilai nol.",
                             fmt_id(st$b[match("(Intercept)", st$variabel)], 3)))),
        lapply(seq_len(nrow(ks)), function(j) tags$li(
          tags$b(fmt_s(ks$b[j], 3)), sprintf(" (%s): tiap kenaikan satu satuan, Informal %s %s poin persen (variabel lain tetap).",
                                             label_reg(ks$variabel[j]), if (ks$b[j] >= 0) "naik" else "turun", fmt_id(abs(ks$b[j]), 3))))))
  })
  output$b3_tab_rlb <- renderTable({
    f <- M()$rlb$full$koef; s <- M()$rlb$step$koef; nb <- c("(Intercept)", VAR_X)
    kol <- function(k, aw) {
      i <- match(nb, k$variabel)
      out <- data.frame(b = fmt_id(k$b[i], 3), se = fmt_id(k$se[i], 3), t = fmt_id(k$t[i], 2), p = fmt_p(k$p[i]),
                        ket = ifelse(is.na(i), "Tidak masuk model", ifelse(k$signifikan[i], "Signifikan", "Tidak signifikan")), check.names = FALSE)
      names(out) <- paste0(aw, c(" b", " SE", " t", " p", " keputusan")); out
    }
    cbind(Variabel = ifelse(nb == "(Intercept)", nb, label_reg(nb)), kol(f, "Full"), kol(s, "Stepwise"))
  }, striped = TRUE, spacing = "xs", align = "l")
  output$b3_tab_fit <- renderTable({
    f <- M()$rlb$full; s <- M()$rlb$step
    ring <- function(m) c(fmt_id(m$k), fmt_id(m$r2, 3), fmt_id(m$adj_r2, 3), fmt_p(m$f_p), fmt_id(m$aic, 1),
                          sprintf("W = %s, p = %s", fmt_id(m$shapiro[["W"]], 3), fmt_p(m$shapiro[["p"]])),
                          sprintf("p = %s", fmt_p(m$bp[["p"]])),
                          sprintf("%s (%s)", fmt_id(max(m$vif), 2), names(which.max(m$vif))))
    data.frame(Ukuran = c("Jumlah prediktor", "R\u00B2", "Adj R\u00B2", "p uji F", "AIC", "Shapiro-Wilk residual", "Breusch-Pagan", "VIF terbesar"),
               `Full model` = ring(f), `Stepwise (AIC)` = ring(s), check.names = FALSE)
  }, striped = TRUE, spacing = "xs", align = "l")
  
  output$b3_j_forest <- renderText({
    f <- M()$rlb$full$koef; f <- f[f$variabel != "(Intercept)", ]
    sprintf("Efek terbesar (beta baku): %s (%s); %s variabel memiliki CI %s%% yang tidak memotong nol", f$variabel[which.max(abs(f$beta_baku))],
            fmt_s(f$beta_baku[which.max(abs(f$beta_baku))], 2), fmt_id(sum(f$signifikan)), fmt_id((1 - ALPHA_REG) * 100))
  })
  output$b3_forest <- plotly::renderPlotly({
    ambil <- function(m) { k <- m$koef[m$koef$variabel != "(Intercept)", ]; k[!is.na(k$beta_baku), ] }
    f <- ambil(M()$rlb$full); s <- ambil(M()$rlb$step)
    tr <- function(p, k, nama, warna, simbol) {
      plotly::add_trace(p, type = "scatter", mode = "markers", x = k$beta_baku, y = k$variabel, name = nama,
                        error_x = list(type = "data", symmetric = FALSE, array = k$ci_hi - k$beta_baku, arrayminus = k$beta_baku - k$ci_lo, color = warna, thickness = 2),
                        marker = list(color = warna, symbol = simbol, size = 10, line = list(color = "#000000", width = 0.8)),
                        text = sprintf("%s (%s)<br>beta baku = %s<br>CI %s%%: [%s; %s]<br>p = %s", k$variabel, nama, fmt_s(k$beta_baku, 2), fmt_id((1 - ALPHA_REG) * 100),
                                       fmt_id(k$ci_lo, 2), fmt_id(k$ci_hi, 2), fmt_p(k$p)), hoverinfo = "text")
    }
    p <- plotly::plot_ly()
    p <- tr(p, f, "Full model", OKABE[["biru"]], "circle"); p <- tr(p, s, "Stepwise", OKABE[["vermilion"]], "diamond")
    tata_dasar(p, xaxis = list(title = "Beta baku (CI 90%)", zeroline = TRUE),
               yaxis = list(title = "", categoryorder = "array", categoryarray = rev(VAR_X)))
  })
  
  output$b3_j_vif <- renderText({
    v <- M()$rlb$full$vif
    sprintf("VIF terbesar %s = %s (%s batas %s)", names(which.max(v)), fmt_id(max(v), 2), if (max(v) < VIF_BATAS) "di bawah" else "melampaui", fmt_id(VIF_BATAS))
  })
  output$b3_vif <- plotly::renderPlotly({
    v <- M()$rlb$full$vif; v <- sort(v); dv <- data.frame(variabel = factor(names(v), levels = names(v)), vif = as.numeric(v))
    xm <- max(VIF_BATAS * 1.15, max(v) * 1.15)
    p <- plotly::plot_ly(dv, x = ~vif, y = ~variabel, type = "bar", orientation = "h", marker = list(color = OKABE[["biru"]]),
                         text = fmt_id(dv$vif, 2), textposition = "outside", cliponaxis = FALSE,
                         hovertext = sprintf("%s: VIF = %s", dv$variabel, fmt_id(dv$vif, 2)), hoverinfo = "text")
    tata_dasar(p, showlegend = FALSE, xaxis = list(title = "VIF", range = c(0, xm)), yaxis = list(title = ""), margin = list(t = 35),
               shapes = list(list(type = "line", x0 = VIF_BATAS, x1 = VIF_BATAS, y0 = 0, y1 = 1, yref = "paper", line = list(color = "#000000", dash = "dash", width = 2))),
               annotations = list(list(x = VIF_BATAS, y = 1, yref = "paper", text = sprintf("batas VIF = %s", fmt_id(VIF_BATAS)), showarrow = FALSE, xanchor = "right", yanchor = "bottom")))
  })
  output$b3_vif_catatan <- renderUI({
    v <- M()$rlb$full$vif
    p(class = "keterangan",
      sprintf("VIF terbesar adalah %s (%s). ", names(which.max(v)), fmt_id(max(v), 2)),
      if (max(v) < VIF_BATAS)
        sprintf("Semua VIF berada di bawah batas %s, sehingga tidak ada multikolinearitas berat pada full model dan PCA di Tahap 4 berfungsi sebagai reduksi dimensi.", fmt_id(VIF_BATAS))
      else
        sprintf("Nilai ini melampaui batas %s (garis putus-putus), sehingga ada multikolinearitas berat, koefisien full model tidak stabil, dan PCA/PCR layak dipertimbangkan.",
                fmt_id(VIF_BATAS)))
  })
  
  model_res <- reactive(M()$rlb[[input$b3_model_res %||% "step"]])
  output$b3_j_res <- renderText({
    m <- model_res(); nh <- sum(m$cooks > m$cook_batas)
    sprintf("%s: Breusch-Pagan p = %s (%s homoskedastisitas); %s provinsi berpengaruh besar (Cook\u2019s D > 4/n = %s)", m$nama, fmt_p(m$bp[["p"]]),
            if (m$bp[["p"]] > ALPHA_REG) "mendukung" else "tidak mendukung", fmt_id(nh), fmt_id(m$cook_batas, 3))
  })
  label_cook <- function(p, m, x, y) {
    i <- which(m$cooks > m$cook_batas); if (!length(i)) return(p)
    plotly::add_trace(p, type = "scatter", mode = "text", x = x[i], y = y[i], text = ps()$provinsi[i], textposition = "top center",
                      showlegend = FALSE, hoverinfo = "skip", textfont = list(size = 11, color = "#000000"), inherit = FALSE)
  }
  output$b3_res_fit <- plotly::renderPlotly({
    m <- model_res(); d <- ps()
    df <- df_titik(m$fitted, m$resid, sprintf("%s<br>fitted %s; residual %s<br>Cook\u2019s D = %s", d$provinsi, fmt_id(m$fitted, 1), fmt_s(m$resid, 2), fmt_id(m$cooks, 3)))
    p <- label_cook(titik_klaster(plotly::plot_ly(source = "resfit"), df), m, m$fitted, m$resid)
    tata_dasar(p, xaxis = list(title = "Nilai fitted (% Informal)"), yaxis = list(title = "Residual (poin persen)", zeroline = TRUE))
  })
  output$b3_j_qqres <- renderText({
    m <- model_res()
    sprintf("Shapiro-Wilk residual %s: W = %s, p = %s \u2192 residual %s", tolower(m$nama), fmt_id(m$shapiro[["W"]], 3), fmt_p(m$shapiro[["p"]]),
            if (m$shapiro[["p"]] > ALPHA_NORMAL) "dapat dianggap normal" else "tidak normal")
  })
  output$b3_res_qq <- plotly::renderPlotly({
    m <- model_res(); z <- m$resid / stats::sd(m$resid); q <- stats::qqnorm(z, plot.it = FALSE); g <- garis_qq(z); xr <- range(q$x); d <- ps()
    df <- df_titik(q$x, q$y, sprintf("%s<br>residual terstandar %s", d$provinsi, fmt_s(z, 2)))
    p <- titik_klaster(plotly::plot_ly(source = "qqres"), df) |>
      plotly::add_lines(x = xr, y = g(xr), name = "Garis acuan", line = list(color = "#000000", dash = "dash"), hoverinfo = "skip", inherit = FALSE)
    p <- label_cook(p, m, q$x, q$y)
    tata_dasar(p, xaxis = list(title = "Kuantil normal teoretis"), yaxis = list(title = "Residual terstandar"))
  })
  
  output$b3_j_tanda <- renderText({
    f <- M()$rlb$full$koef; f <- f[match(VAR_X, f$variabel), ]; ref <- REF_TANDA[match(VAR_X, REF_TANDA$variabel), ]
    sprintf("%s dari 9 tanda koefisien full model sejalan dengan acuan Birgitta (2021) dan Sibagariang dkk. (2023)", fmt_id(sum(sign(f$b) == ref$tanda)))
  })
  output$b3_tab_tanda <- renderTable({
    f <- M()$rlb$full$koef; s <- M()$rlb$step$koef; ref <- REF_TANDA[match(VAR_X, REF_TANDA$variabel), ]
    ft <- function(k) { i <- match(VAR_X, k$variabel)
    ifelse(is.na(i), "tidak masuk model", paste0(fmt_s(k$b[i], 3), ifelse(k$signifikan[i], " (signifikan)", " (tidak signifikan)"))) }
    bf <- f$b[match(VAR_X, f$variabel)]
    data.frame(Variabel = label_reg(VAR_X), `Full model` = ft(f), Stepwise = ft(s), `Tanda acuan` = tanda_txt(ref$tanda),
               `Full vs acuan` = ifelse(sign(bf) == ref$tanda, "Sejalan", "Berlawanan"), Acuan = ref$acuan, check.names = FALSE)
  }, striped = TRUE, spacing = "xs", align = "l")
  
  output$b3_kmo <- renderUI({
    pc <- M()$pca; kb <- pc$bartlett; ms <- pc$msa
    kotak <- function(judul, nilai, ...) div(class = "kmo-box bg-primary text-white",
                                             span(class = "kmo-t", judul), span(class = "kmo-v", nilai), ...)
    pb <- if (kb[["p"]] < 0.001) "< 0,001" else paste("=", fmt_id(kb[["p"]], 3))
    div(class = "kmo-grid",
        kotak("KMO keseluruhan", fmt_id(pc$kmo, 3),
              p(class = "kmo-d", sprintf("Kategori: %s (\u2265 0,5 dianggap layak).", pc$kmo_kategori))),
        kotak("Uji Bartlett", sprintf("\u03C7\u00B2 = %s", fmt_id(kb[["chisq"]], 2)),
              p(class = "kmo-d", sprintf("df = %s; p %s.", fmt_id(kb[["df"]]), pb))),
        kotak("KMO per variabel (MSA)", sprintf("%s\u2013%s", fmt_id(min(ms), 2), fmt_id(max(ms), 2)),
              p(class = "kmo-d", paste(sprintf("%s %s", names(ms), fmt_id(ms, 2)), collapse = "; "), ".",
                if (any(ms < 0.5)) sprintf(" MSA < 0,5: %s.", paste(names(ms)[ms < 0.5], collapse = ", ")))))
  })
  output$b3_ket_kmo <- renderUI({
    pc <- M()$pca; kb <- pc$bartlett
    layak <- pc$kmo >= 0.5 && kb[["p"]] < ALPHA_NORMAL
    p(class = "keterangan",
      sprintf("KMO = %s (%s) dan Bartlett p %s: data %s untuk PCA. %s Dihitung dari matriks korelasi 9 variabel X (Informal tidak ikut).",
              fmt_id(pc$kmo, 3), pc$kmo_kategori, if (kb[["p"]] < 0.001) "< 0,001" else paste("=", fmt_id(kb[["p"]], 3)),
              if (layak) "layak" else "kurang layak",
              if (kb[["p"]] < ALPHA_NORMAL) "Matriks korelasi bukan matriks identitas, sehingga ada korelasi antarvariabel yang layak diringkas."
              else "Tidak ada bukti korelasi antarvariabel, sehingga PCA kurang bermakna."))
  })
  
  output$b3_ket_scree <- renderUI({
    pc <- M()$pca; kum <- pc$eigen$kumulatif
    p(class = "keterangan",
      sprintf("%s komponen terpilih (%s) menjelaskan %s%% varians, dan varians \u2265 70%% tercapai pada %s komponen. Batang biru menunjukkan komponen terpilih, sedangkan garis oranye menunjukkan varians kumulatif.",
              fmt_id(pc$k), if (is.na(K_PC)) "eigenvalue > 1" else "diatur manual", fmt_id(kum[pc$k] * 100, 1), fmt_id(pc$k_70)))
  })
  output$b3_scree <- plotly::renderPlotly({
    pc <- M()$pca; e <- pc$eigen; np <- nrow(e); k <- pc$k; ve <- e$varians * 100; kum <- e$kumulatif * 100
    p <- plotly::plot_ly(source = "scree") |>
      plotly::add_bars(x = e$komponen, y = ve, name = "Per komponen", marker = list(color = ifelse(seq_len(np) <= k, "#0072B2", "#BDBDBD")),
                       text = paste0(fmt_id(ve, 1), "%"), textposition = "outside", cliponaxis = FALSE,
                       hovertext = sprintf("%s<br>eigenvalue %s<br>varians %s%%", e$komponen, fmt_id(e$eigenvalue, 2), fmt_id(ve, 1)), hoverinfo = "text") |>
      plotly::add_trace(type = "scatter", mode = "lines+markers", x = e$komponen, y = kum, name = "Kumulatif",
                        line = list(color = "#D55E00", width = 2), marker = list(color = "#D55E00"),
                        text = paste0(fmt_id(kum, 1), "%"), hoverinfo = "text")
    tata_dasar(p, xaxis = list(title = "Komponen utama", categoryorder = "array", categoryarray = e$komponen),
               yaxis = list(title = "Varians dijelaskan (%)", range = c(0, 112)),
               shapes = list(list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = 100 / np, y1 = 100 / np, line = list(color = "#000000", dash = "dash", width = 1.5))),
               annotations = list(
                 list(xref = "paper", x = 1, y = 100 / np, text = "eigenvalue = 1", showarrow = FALSE, xanchor = "right", yanchor = "bottom"),
                 list(x = e$komponen[k], y = kum[k], text = sprintf("k = %d (terpilih)", k), showarrow = TRUE, ax = 0, ay = -35, arrowhead = 2)))
  })
  
  output$b3_load <- plotly::renderPlotly({
    pc <- M()$pca; ld <- pc$hasil$loadings[, seq_len(pc$k), drop = FALSE]; zm <- max(abs(ld))
    teks <- matrix(fmt_s(as.vector(ld), 2), nrow(ld), ncol(ld))
    p <- plotly::plot_ly(x = colnames(ld), y = rownames(ld), z = ld, type = "heatmap", colorscale = skala_div, zmin = -zm, zmax = zm,
                         text = matrix(sprintf("%s pada %s: %s", rownames(ld)[row(ld)], colnames(ld)[col(ld)], teks), nrow(ld)), hoverinfo = "text", colorbar = list(title = "loading"))
    tata_dasar(p, annotations = anotasi_sel(ld, colnames(ld), rownames(ld), teks, zm), yaxis = list(autorange = "reversed", title = ""), xaxis = list(title = "Komponen"))
  })
  output$b3_ket_load <- renderUI({
    pc <- M()$pca
    p(class = "keterangan",
      sprintf("Variabel dengan loading terbesar: %s. Warna oranye menunjukkan loading positif dan warna biru negatif; semakin pekat warnanya, semakin kuat variabel membentuk komponen tersebut.",
              paste(pc$judul_pc[seq_len(pc$k)], collapse = "; ")))
  })
  
  output$b3_j_biplot <- renderText({
    ld <- M()$pca$hasil$loadings
    sprintf("PC1 paling dipengaruhi %s dan PC2 oleh %s", rownames(ld)[which.max(abs(ld[, 1]))], rownames(ld)[which.max(abs(ld[, 2]))])
  })
  output$b3_biplot <- plotly::renderPlotly({
    pc <- M()$pca$hasil; sc <- pc$scores
    sc$klaster <- prov$klaster[match(sc$kode_prov, prov$kode_prov)]
    sel <- selected_prov(); ada <- length(sel) > 0
    sc$sorot <- apply_selection(sc, sel)
    ld <- pc$loadings[, 1:2, drop = FALSE]
    skala <- 0.85 * max(abs(sc$PC1), abs(sc$PC2)) / max(abs(ld))
    ve <- pc$var_explained
    p <- plotly::plot_ly(source = "biplot")
    for (j in seq_len(K)) {
      s <- sc[sc$klaster == j, ]
      if (!nrow(s)) next
      p <- plotly::add_markers(
        p, data = s, x = ~PC1, y = ~PC2, key = ~kode_prov,
        text = ~paste0(provinsi, "<br>Klaster ", klaster), hoverinfo = "text", name = paste("Klaster", j),
        marker = list(color = PALET_KLASTER[j], symbol = SIMBOL_KLASTER[j], size = ifelse(s$sorot, 17, 9),
                      opacity = ifelse(ada & !s$sorot, 0.35, 1),
                      line = list(color = "#000000", width = ifelse(s$sorot, 3, 0.6))))
    }
    ann <- lapply(seq_len(nrow(ld)), function(i) list(
      x = ld[i, 1] * skala, y = ld[i, 2] * skala, ax = 0, ay = 0, axref = "x", ayref = "y", xref = "x", yref = "y",
      text = rownames(ld)[i], showarrow = TRUE, arrowhead = 2, arrowsize = 1, arrowwidth = 1.2,
      arrowcolor = "#000000", font = list(size = 11, color = "#000000")))
    p <- plotly::layout(p, annotations = ann, dragmode = "lasso", separators = ",.", font = list(color = "#000000"),
                        xaxis = list(title = sprintf("PC1 (%s%%)", fmt_id(ve[1] * 100, 1)), zeroline = TRUE),
                        yaxis = list(title = sprintf("PC2 (%s%%)", fmt_id(ve[2] * 100, 1)), zeroline = TRUE),
                        legend = list(orientation = "h", y = -0.15), margin = list(t = 10))
    p <- plotly::config(p, displaylogo = FALSE)
    p <- plotly::event_register(p, "plotly_click")
    p <- plotly::event_register(p, "plotly_selected")
    plotly::event_register(p, "plotly_deselect")
  })
  
  output$b3_tab_banding <- renderTable({
    tb <- M()$perbandingan
    data.frame(Model = tb$model, Prediktor = fmt_id(tb$prediktor), "R\u00B2" = fmt_id(tb$r2, 3), "Adj R\u00B2" = fmt_id(tb$adj_r2, 3),
               AIC = fmt_id(tb$aic, 1), `RMSE LOOCV` = fmt_id(tb$rmse_loo, 2),
               `Shapiro residual` = sprintf("p = %s \u2192 %s", fmt_p(tb$shapiro_p), ifelse(tb$lolos_shapiro, "Lolos", "Tidak lolos")),
               `Breusch-Pagan` = sprintf("p = %s \u2192 %s", fmt_p(tb$bp_p), ifelse(tb$lolos_bp, "Lolos", "Tidak lolos")), check.names = FALSE)
  }, striped = TRUE, spacing = "xs", align = "l")
  output$b3_ket_banding <- renderUI({
    tb <- M()$perbandingan; b <- tb[which.max(tb$adj_r2), ]; r <- M()$pcr$model
    p(class = "keterangan",
      sprintf("Adj R\u00B2 tertinggi: %s (%s) dibanding full model %s dan stepwise %s. RMSE LOOCV yang lebih kecil berarti galat prediksi lebih rendah, sedangkan \u201CLolos\u201D berarti asumsi residual (kenormalan dan homoskedastisitas) terpenuhi. VIF antar skor komponen PCR = %s (ortogonal); PCR mengurangi jumlah prediktor dan menstabilkan koefisien.",
              b$model, fmt_id(b$adj_r2, 3), fmt_id(tb$adj_r2[1], 3), fmt_id(tb$adj_r2[2], 3), fmt_id(max(r$vif), 2)))
  })
  
  output$b3_rumus_pcr <- renderUI({
    pc <- M()$pcr; kf <- pc$model$koef; kc <- kf[kf$variabel != "(Intercept)", ]; k <- nrow(kc)
    suku <- function(b, nama) list(tags$span(if (b < 0) " \u2212 " else " + "), tags$span(fmt_id(abs(b), 3), "\u00B7", tags$i(nama)))
    persamaan <- function(b0, bs, nm) div(class = "rumus-rlb", tags$i("\u0176"), " = ", fmt_id(b0, 3),
                                          unlist(lapply(seq_along(bs), function(j) suku(bs[j], nm[j])), recursive = FALSE))
    ko <- pc$koef[match(VAR_X, pc$koef$variabel), ]
    tags$div(
      p(class = "small mb-1", tags$b("Persamaan terhadap komponen utama:")),
      div(class = "rumus-rlb", HTML(sprintf("<i>\u0176</i> = &gamma;<sub>0</sub> + %s", paste(sprintf("&gamma;<sub>%d</sub><i>PC</i><sub>%d</sub>", seq_len(k), seq_len(k)), collapse = " + ")))),
      persamaan(kf$b[kf$variabel == "(Intercept)"], kc$b, kc$variabel),
      p(class = "small mt-2 mb-1", tags$b("Persamaan dikembalikan ke variabel asli:")),
      persamaan(pc$intersep, ko$beta_asli, ko$variabel),
      p(class = "small mt-2 mb-1", tags$b("Keterangan komponen:")),
      tags$ul(class = "small mb-0",
              tags$li(tags$b(HTML("<i>\u0176</i>")), ": nilai dugaan persentase pekerja informal (Informal, %)."),
              tags$li(tags$b(HTML("&gamma;<sub>0</sub>")), sprintf(" = %s: intersep, dugaan Informal bila seluruh skor komponen bernilai nol (titik rata-rata X).", fmt_id(kf$b[kf$variabel == "(Intercept)"], 3))),
              lapply(seq_len(k), function(j) tags$li(tags$b(HTML(sprintf("&gamma;<sub>%d</sub>", j))),
                                                     sprintf(" = %s (p = %s): bila skor %s naik satu satuan, Informal %s %s poin persen; %s.", fmt_id(kc$b[j], 3), fmt_p(kc$p[j]), kc$variabel[j],
                                                             if (kc$b[j] >= 0) "naik" else "turun", fmt_id(abs(kc$b[j]), 3), if (kc$p[j] < ALPHA_REG) "signifikan" else "tidak signifikan"))),
              tags$li(tags$b(HTML("<i>PC</i><sub>j</sub>")), HTML(": skor komponen utama ke-j, kombinasi linear variabel X terstandar (<i>Z</i>) dengan bobot loading (lihat Tahap 4); antar skor saling bebas (ortogonal).")),
              tags$li(HTML("Persamaan kedua diperoleh dengan mengalikan &gamma; dengan loading lalu membaginya dengan simpangan baku tiap X, sehingga koefisien kembali ke satuan variabel asli (Upah Formal: juta Rp; Upah/Jam: ribu Rp).")),
              tags$li(HTML("<b>&epsilon;</b> (galat acak) tidak dituliskan pada persamaan dugaan."))))
  })
  
  output$b3_bar_koef <- plotly::renderPlotly({
    f <- M()$rlb$full$koef; f <- f[match(VAR_X, f$variabel), ]; r <- M()$pcr$koef[match(VAR_X, M()$pcr$koef$variabel), ]
    p <- plotly::plot_ly() |>
      plotly::add_bars(y = VAR_X, x = f$beta_baku, orientation = "h", name = "Full model", marker = list(color = OKABE[["biru"]]),
                       text = fmt_s(f$beta_baku, 2), textposition = "outside", cliponaxis = FALSE, hoverinfo = "text",
                       hovertext = sprintf("%s (full): %s", VAR_X, fmt_s(f$beta_baku, 3))) |>
      plotly::add_bars(y = VAR_X, x = r$beta_baku, orientation = "h", name = sprintf("PCR (k = %d)", M()$pca$k), marker = list(color = OKABE[["vermilion"]]),
                       text = fmt_s(r$beta_baku, 2), textposition = "outside", cliponaxis = FALSE, hoverinfo = "text",
                       hovertext = sprintf("%s (PCR): %s", VAR_X, fmt_s(r$beta_baku, 3)))
    tata_dasar(p, barmode = "group", margin = list(t = 35), xaxis = list(title = "Beta baku", zeroline = TRUE), yaxis = list(title = "", categoryorder = "array", categoryarray = rev(VAR_X)))
  })
  output$b3_catatan_tanda <- renderUI({
    f <- M()$rlb$full$koef; f <- f[match(VAR_X, f$variabel), ]; r <- M()$pcr$koef[match(VAR_X, M()$pcr$koef$variabel), ]
    beda <- VAR_X[sign(f$beta_baku) != sign(r$beta_baku)]
    p(class = "keterangan",
      "Batang membandingkan koefisien baku (beta baku) full model dan PCR. ",
      if (length(beda)) sprintf("Tanda berbeda pada %s, yang menandakan koefisien full model peka terhadap korelasi antarvariabel sehingga tandanya perlu ditafsirkan hati-hati.", paste(beda, collapse = ", "))
      else "Tidak ada perubahan tanda, dan besaran koefisien PCR cenderung lebih kecil karena hanya memakai k komponen.")
  })
  
  output$b3_ket_ovp <- renderUI({
    mm <- switch(input$b3_model_ovp %||% "pcr", full = M()$rlb$full, step = M()$rlb$step, pcr = M()$pcr$model)
    p(class = "keterangan",
      sprintf("%s: R\u00B2 = %s dan RMSE LOOCV = %s poin persen. Semakin dekat titik dengan garis putus-putus y = x, semakin akurat prediksinya; warna dan bentuk titik menunjukkan kluster provinsi.",
              mm$nama, fmt_id(mm$r2, 3), fmt_id(mm$rmse_loo, 2)))
  })
  output$b3_ovp <- plotly::renderPlotly({
    mm <- switch(input$b3_model_ovp %||% "pcr", full = M()$rlb$full, step = M()$rlb$step, pcr = M()$pcr$model); d <- ps()
    df <- df_titik(mm$fitted, mm$observed, sprintf("%s<br>teramati %s%%; prediksi %s%%", d$provinsi, fmt_id(mm$observed, 1), fmt_id(mm$fitted, 1)))
    rg <- range(c(mm$fitted, mm$observed))
    p <- titik_klaster(plotly::plot_ly(source = "ovp"), df) |>
      plotly::add_lines(x = rg, y = rg, name = "y = x", line = list(color = "#000000", dash = "dash"), hoverinfo = "skip", inherit = FALSE)
    tata_dasar(p, xaxis = list(title = "Prediksi Informal (%)"), yaxis = list(title = "Teramati Informal (%)"))
  })
  
  output$b3_paralel <- plotly::renderPlotly({
    m <- heatmap_matrix(prov); xl <- label_resp(VAR_MV)
    sel <- selected_prov(); ada <- length(sel) > 0
    tampil_legenda <- !duplicated(prov$klaster)
    urut <- order(apply_selection(prov, sel))
    p <- plotly::plot_ly(source = "paralel")
    for (i in urut) {
      s <- prov$kode_prov[i] %in% sel; kl <- prov$klaster[i]
      p <- plotly::add_trace(
        p, type = "scatter", mode = "lines+markers", x = xl, y = as.numeric(m[i, ]),
        name = if (tampil_legenda[i]) paste("Klaster", kl) else prov$provinsi[i],
        legendgroup = paste0("k", kl), showlegend = tampil_legenda[i],
        line = list(color = PALET_KLASTER[kl], width = if (s) 4 else 1.2),
        marker = list(color = PALET_KLASTER[kl], symbol = SIMBOL_KLASTER[kl], size = if (s) 9 else 5),
        opacity = if (ada && !s) 0.2 else 0.85, hoverinfo = "text",
        text = paste0(prov$provinsi[i], " (Klaster ", kl, ")<br>", xl, ": z = ", fmt_id(as.numeric(m[i, ]), 2)))
    }
    tata_dasar(p, xaxis = list(title = "", categoryorder = "array", categoryarray = xl),
               yaxis = list(title = "z-score", zeroline = TRUE))
  })
  
  output$b3_heat <- plotly::renderPlotly({
    m <- heatmap_matrix(prov); colnames(m) <- label_resp(colnames(m))
    ht <- matrix(sprintf("%s<br>%s: z = %s", rownames(m)[row(m)], colnames(m)[col(m)], fmt_id(m, 2)), nrow = nrow(m))
    p <- heatmaply::heatmaply(
      m, custom_hovertext = ht, key.title = "z-score", colors = grDevices::colorRampPalette(c(OKABE[["biru"]], "#F7F7F7", OKABE[["vermilion"]]))(101), limits = c(-3, 3),
      Rowv = DEN_BARIS, hclust_method = "ward.D2", k_row = K, k_col = 2,
      row_side_colors = data.frame(Klaster = factor(paste("Klaster", prov$klaster))),
      row_side_palette = function(n) PALET_KLASTER[seq_len(n)],
      fontsize_row = 8, fontsize_col = 10, margins = c(40, 130, 10, 10),
      showticklabels = c(TRUE, TRUE), plot_method = "plotly")
    plotly::config(p, displaylogo = FALSE)
  })
  
  output$b3_profil <- renderTable({
    ag <- stats::aggregate(prov[VAR_MV], list(Klaster = prov$klaster), mean)
    out <- data.frame(Klaster = ag$Klaster, `Jumlah provinsi` = as.numeric(table(prov$klaster)), check.names = FALSE)
    for (v in VAR_MV) out[[label_resp(v)]] <- fmt_id(ag[[v]], 1)
    out
  }, striped = TRUE, spacing = "xs", align = "l")
  
  output$b3_interpretasi <- renderUI({
    z <- heatmap_matrix(prov, VAR_X)
    baris <- lapply(seq_len(K), function(j) {
      zm <- colMeans(z[prov$klaster == j, , drop = FALSE]); o <- order(zm, decreasing = TRUE)
      tags$li(sprintf("Klaster %d (%s provinsi): relatif tinggi pada %s dan %s; relatif rendah pada %s.",
                      j, fmt_id(sum(prov$klaster == j)), VAR_X[o[1]], VAR_X[o[2]], VAR_X[o[length(o)]]))
    })
    rata_inf <- tapply(prov[[VAR_Y]], prov$klaster, mean)
    tagList(p(class = "cara-baca", tags$b("Ciri menonjol tiap klaster "), "(sembilan variabel X, dibandingkan rata-rata 38 provinsi):"), tags$ul(baris),
            p(class = "cara-baca", tags$b("Validasi dengan Informal "), "(tidak dipakai membentuk klaster): rata-rata tertinggi pada klaster ",
              names(which.max(rata_inf)), sprintf(" (%s%%) dan terendah pada klaster ", fmt_id(max(rata_inf), 1)), names(which.min(rata_inf)),
              sprintf(" (%s%%).", fmt_id(min(rata_inf), 1))))
  })
  
  tab_df <- data.frame(Provinsi = prov$provinsi, Klaster = prov$klaster,
                       `Informal (%)` = fmt_id(prov$Informal, 1),
                       Pencilan = ifelse(prov$flag_pencilan, "Ya", "\u2013"), check.names = FALSE)
  output$b3_tabel <- DT::renderDT(
    DT::datatable(tab_df, selection = "single", rownames = FALSE, options = list(pageLength = 8, dom = "ftip", scrollX = TRUE,
                                                                                 language = list(search = "Cari:", zeroRecords = "Tidak ada provinsi yang cocok",
                                                                                                 info = "Menampilkan _START_\u2013_END_ dari _TOTAL_ provinsi", infoEmpty = "Tidak ada data",
                                                                                                 infoFiltered = "(disaring dari _MAX_ provinsi)",
                                                                                                 paginate = list(previous = "Sebelumnya", `next` = "Berikutnya")))))
  proxy_tab <- DT::dataTableProxy("b3_tabel")
  
  observeEvent(plotly::event_data("plotly_click", source = "biplot"), {
    ed <- plotly::event_data("plotly_click", source = "biplot")
    if (!is.null(ed$key)) selected_prov(as.character(ed$key[1]))
  })
  observeEvent(plotly::event_data("plotly_selected", source = "biplot"), {
    ed <- plotly::event_data("plotly_selected", source = "biplot")
    if (!is.null(ed) && NROW(ed) > 0 && !is.null(ed$key)) selected_prov(unique(as.character(ed$key)))
  })
  observeEvent(plotly::event_data("plotly_deselect", source = "biplot"), selected_prov(NULL))
  observeEvent(input$b3_tabel_rows_selected, {
    i <- input$b3_tabel_rows_selected
    if (length(i)) { kode <- prov$kode_prov[i]; if (!identical(kode, selected_prov())) selected_prov(kode) }
  })
  observeEvent(input$b3_tabel_rows_selected, {
    if (!length(input$b3_tabel_rows_selected) && length(selected_prov()) == 1) selected_prov(NULL)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  observeEvent(plotly::event_data("plotly_click", source = "A"), {
    ed <- plotly::event_data("plotly_click", source = "A")
    i <- match(as.character(ed$y[1]), prov$provinsi)
    if (!is.na(i)) selected_prov(prov$kode_prov[i])
  })
  observeEvent(selected_prov(), {
    sel <- selected_prov()
    DT::selectRows(proxy_tab, if (length(sel) == 1) match(sel, prov$kode_prov) else NULL)
  }, ignoreNULL = FALSE)
  
  output$b3_aksi <- renderUI({
    sel <- selected_prov()
    if (length(sel) == 0) return(p(class = "text-muted small", "Belum ada provinsi terpilih."))
    nama <- prov$provinsi[match(sel, prov$kode_prov)]
    div(class = "baris-fleks",
        span(tags$b("Terpilih: "), paste(nama, collapse = ", ")),
        if (length(sel) == 1) actionButton("b3_lihat_peta", "Lihat di peta", icon = icon("map"), class = "btn-primary btn-sm"),
        actionButton("b3_reset", "Hapus pilihan", icon = icon("xmark"), class = "btn-outline-secondary btn-sm"))
  })
  observeEvent(input$b3_reset, selected_prov(NULL))
  
  # ==== [S5] tautan antarbab + Temuan/Implikasi per bab + Metadata ====
  observeEvent(input$b3_lihat_peta, {
    sel <- selected_prov(); req(length(sel) == 1)
    zoom_to_prov(NULL); zoom_to_prov(sel)      # NULL dulu agar pilihan provinsi yang sama tetap memicu zoom
    bslib::nav_select("navbar", "bab2", session = session)
  })
  
  b1 <- local({
    v  <- function(id) sunburst$juta_orang[match(id, sunburst$id)]
    dv <- function(id) DATA$dendrogram$juta_orang[match(id, DATA$dendrogram$id)]
    inf <- v("informal"); frm <- v("formal")
    list(
      inf = inf, frm = frm, bekerja = dv("bekerja"),
      pct_inf   = inf / (inf + frm) * 100,
      selisih   = inf - frm,
      p_inf     = .p_informal, p_frm = .p_formal, p_all = MID_PEREMPUAN,
      inf_p = v("informal_p") / (v("informal_p") + v("formal_p")) * 100,
      inf_l = v("informal_l") / (v("informal_l") + v("formal_l")) * 100,
      tpt   = dv("pengangguran") / dv("angkatan_kerja") * 100,
      tak_penuh = (dv("pekerja_paruh") + dv("setengah_peng")),
      pct_tak_penuh = (dv("pekerja_paruh") + dv("setengah_peng")) / dv("bekerja") * 100
    )
  })
  output$b1_temuan <- renderUI({
    tags$ul(
      butir("Pekerja informal lebih banyak daripada formal",
            sprintf("Pekerja informal berjumlah %s juta orang (%s%% dari yang bekerja). Jumlah ini lebih banyak %s juta daripada pekerja formal (%s juta).",
                    fmt_id(b1$inf, 2), fmt_id(b1$pct_inf, 1), fmt_id(b1$selisih, 2), fmt_id(b1$frm, 2))),
      butir("Perempuan lebih bertumpu pada sektor informal",
            sprintf("Sebanyak %s%% pekerja perempuan berstatus informal, dibanding %s%% pekerja laki-laki. Perempuan menyumbang %s%% pekerja informal, di atas porsinya pada pekerja formal (%s%%) dan pada seluruh pekerja (%s%%).",
                    fmt_id(b1$inf_p, 1), fmt_id(b1$inf_l, 1), fmt_id(b1$p_inf, 1), fmt_id(b1$p_frm, 1), fmt_id(b1$p_all, 1))),
      butir("Pengangguran kecil, tetapi jam kerja terbatas",
            sprintf("Pengangguran terbuka relatif kecil (%s%% dari angkatan kerja). Meski begitu, %s juta pekerja (%s%%) bekerja paruh waktu atau setengah menganggur.",
                    fmt_id(b1$tpt, 1), fmt_id(b1$tak_penuh, 2), fmt_id(b1$pct_tak_penuh, 1))))
  })
  output$b1_implikasi <- renderUI({
    tags$ul(
      butir("Perluasan perlindungan sosial",
            "Perlindungan yang melekat pada hubungan kerja formal, misalnya jaminan sosial lewat pemberi kerja, tidak menjangkau mayoritas pekerja. Skema yang bisa diikuti pekerja mandiri dan pekerja bebas, misalnya dengan iuran fleksibel atau bersubsidi, perlu menjadi bagian dari kebijakan."),
      butir("Perhatian pada kesetaraan gender",
            "Perempuan lebih banyak berada di sektor informal. Program perlindungan, pelatihan, dan akses modal perlu dirancang dan dipantau dengan pemilahan menurut jenis kelamin."),
      butir("Indikator keberhasilan yang lebih lengkap",
            "Tingkat pengangguran yang rendah belum menggambarkan kualitas pekerjaan. Informalitas dan jam kerja perlu dipantau berdampingan dengan pengangguran."))
  })
  output$b2_temuan <- renderUI({
    g <- DATA$lisa$global; lk <- DATA$lisa$local$kategori
    q <- stats::quantile(d$rasio, c(.25, .5, .75)) * 100
    o10t <- top_n_kabkota(d, 10, TRUE); o10r <- top_n_kabkota(d, 10, FALSE)
    n_papua <- sum(grepl("Papua", o10t$provinsi))
    n_kota  <- sum(grepl("^Kota ", o10r$nama))
    hh_prov <- sort(table(d$provinsi[lk == "HH"]), decreasing = TRUE)
    teks_hh <- if (length(hh_prov) > 0) {
      top2 <- head(hh_prov, 2)
      paste0("Klaster tinggi\u2013tinggi paling banyak berada di ",
             paste(sprintf("%s (%s kab/kota)", names(top2), fmt_id(as.integer(top2))), collapse = " dan "), ".")
    }
    tags$ul(
      butir("Sebaran rasio antar kab/kota",
            paste0(judul_peta(d), ". ",
                   sprintf("Separuh kab/kota memiliki rasio %s%%\u2013%s%% dengan median %s%%.",
                           fmt_id(q[1], 1), fmt_id(q[3], 1), fmt_id(q[2], 1)))),
      butir("Rasio mengelompok secara spasial",
            sprintf("Moran's I = %s (p = %s). Sebanyak %s kab/kota masuk klaster tinggi\u2013tinggi dan %s kab/kota masuk klaster rendah\u2013rendah.",
                    fmt_id(g$I, 3), fmt_id(g$p, 3), fmt_id(sum(lk == "HH")), fmt_id(sum(lk == "LL"))),
            if (!is.null(teks_hh)) paste0(" ", teks_hh)),
      butir("Wilayah dengan rasio tertinggi dan terendah",
            sprintf("%s dari 10 kab/kota dengan rasio tertinggi berada di provinsi Papua. Sebaliknya, %s dari 10 kab/kota dengan rasio terendah berstatus kota. Terdapat %s kab/kota bernilai ekstrem (di luar 1,5 \u00D7 IQR sebaran nasional).",
                    fmt_id(n_papua), fmt_id(n_kota), fmt_id(sum(d$flag_ekstrem)))))
  })
  output$b2_implikasi <- renderUI({
    lk <- DATA$lisa$local$kategori
    besar <- d[order(-d$informal), ][1:3, ]
    tags$ul(
      butir("Prioritas wilayah",
            "Perlindungan pekerja informal perlu diprioritaskan di wilayah klaster tinggi\u2013tinggi, bukan hanya di provinsi dengan rata-rata tinggi."),
      butir("Rasio rendah tidak berarti jumlah kecil",
            sprintf("Kab/kota dengan pekerja informal terbanyak adalah %s (%s orang), %s, dan %s. Program berbasis jumlah orang perlu melihat angka absolut, bukan hanya rasio.",
                    besar$nama[1], fmt_id(besar$informal[1]), besar$nama[2], besar$nama[3])),
      butir("Nilai ekstrem perlu dibaca hati-hati",
            "Rasio yang sangat tinggi (\u26A0) jauh di luar sebaran nasional. Wilayah tersebut mungkin memerlukan pendekatan yang berbeda dari wilayah lain."),
      butir("Pencilan spasial",
            sprintf("%s kab/kota pencilan spasial (HL dan LH) layak ditelaah tersendiri, sebab kondisinya berbeda dari tetangga sekitarnya.",
                    fmt_id(sum(lk %in% c("HL", "LH"))))))
  })
  persamaan_hasil <- function(b0, bs, nm) div(class = "rumus-rlb", tags$i("\u0176"), " = ", fmt_id(b0, 3),
                                              unlist(lapply(seq_along(bs), function(j) list(tags$span(if (bs[j] < 0) " \u2212 " else " + "),
                                                                                            tags$span(fmt_id(abs(bs[j]), 3), "\u00B7", tags$i(nm[j])))), recursive = FALSE))
  persamaan_rlb <- function(k) { kk <- k[k$variabel != "(Intercept)", ]; persamaan_hasil(k$b[k$variabel == "(Intercept)"], kk$b, kk$variabel) }
  ukuran_model <- function(m) sprintf("R\u00B2 = %s; Adj R\u00B2 = %s; AIC = %s; RMSE LOOCV = %s poin persen.", fmt_id(m$r2, 3), fmt_id(m$adj_r2, 3), fmt_id(m$aic, 1), fmt_id(m$rmse_loo, 2))
  poin <- function(judul, ...) tags$li(tags$b(paste0(judul, ": ")), ...)
  bagian <- function(judul, ...) tagList(p(class = "fw-semibold mt-3 mb-1", judul), tags$ul(class = "mb-0", ...))
  
  output$b3_hasil_pra <- renderUI({
    f <- M()$rlb$full; s <- M()$rlb$step; sig <- f$koef$variabel[f$koef$signifikan & f$koef$variabel != "(Intercept)"]
    tags$div(
      p(class = "small mb-1", tags$b("Full model"), sprintf(" (%s prediktor):", fmt_id(f$k))),
      persamaan_rlb(f$koef),
      p(class = "small mt-1 mb-2", ukuran_model(f)),
      p(class = "small mb-1", tags$b("Model stepwise (AIC)"), sprintf(" (%s prediktor):", fmt_id(s$k))),
      persamaan_rlb(s$koef),
      p(class = "small mt-1 mb-2", ukuran_model(s)),
      p(class = "keterangan", sprintf("Pada full model, variabel signifikan pada \u03B1 = %s%% adalah %s. Stepwise mempertahankan %s. VIF terbesar full model: %s (%s).",
                                      fmt_id(ALPHA_REG * 100), if (length(sig)) paste(sig, collapse = ", ") else "tidak ada", paste(s$vars, collapse = ", "),
                                      names(which.max(f$vif)), fmt_id(max(f$vif), 2))))
  })
  output$b3_hasil_pasca <- renderUI({
    m <- M(); r <- m$pcr$model; ko <- m$pcr$koef[match(VAR_X, m$pcr$koef$variabel), ]; kum <- m$pca$eigen$kumulatif[m$pca$k]
    kc <- r$koef[r$koef$variabel != "(Intercept)", ]
    tags$div(
      p(class = "small mb-1", tags$b(sprintf("PCR (k = %d komponen)", m$pca$k)), " pada variabel asli:"),
      persamaan_hasil(m$pcr$intersep, ko$beta_asli, ko$variabel),
      p(class = "small mt-1 mb-2", ukuran_model(r)),
      p(class = "small mb-1", tags$b("Terhadap skor komponen:")),
      persamaan_rlb(r$koef),
      p(class = "keterangan", sprintf("%s komponen utama menjelaskan %s%% varians sembilan variabel X. Skor antar komponen saling bebas (VIF maksimum %s). Komponen signifikan pada \u03B1 = %s%%: %s.",
                                      fmt_id(m$pca$k), fmt_id(kum * 100, 1), fmt_id(max(r$vif), 2), fmt_id(ALPHA_REG * 100),
                                      if (any(kc$signifikan)) paste(kc$variabel[kc$signifikan], collapse = ", ") else "tidak ada")))
  })
  
  output$b3_temuan <- renderUI({
    m <- M(); nm <- m$normalitas; kr <- m$korelasi$xy; f <- m$rlb$full; s <- m$rlb$step; r <- m$pcr$model
    kf <- f$koef[match(VAR_X, f$koef$variabel), ]; kp <- m$pcr$koef[match(VAR_X, m$pcr$koef$variabel), ]
    beda <- VAR_X[sign(kf$beta_baku) != sign(kp$beta_baku)]
    rata_inf <- tapply(prov[[VAR_Y]], prov$klaster, mean)
    z <- heatmap_matrix(prov, VAR_X)
    li_klaster <- lapply(seq_len(K), function(j) {
      idx <- prov$klaster == j; zm <- colMeans(z[idx, , drop = FALSE]); o <- order(zm, decreasing = TRUE)
      tags$li(tags$b(sprintf("Klaster %d", j)),
              sprintf(" (%s provinsi; rata-rata Informal %s%%): tinggi pada %s dan %s, rendah pada %s. Anggota: %s.",
                      fmt_id(sum(idx)), fmt_id(rata_inf[[as.character(j)]], 1), VAR_X[o[1]], VAR_X[o[2]], VAR_X[o[length(o)]],
                      paste(prov$provinsi[idx], collapse = ", ")))
    })
    pen <- DATA$pca_klaster$pencilan
    li_pen <- lapply(pen, function(nm) {
      i <- match(nm, prov$provinsi); zi <- z[i, ]; v <- names(zi)[abs(zi) > Z_PENCILAN]
      tags$li(tags$b(nm), sprintf(" (klaster %d): |z| > %s pada %s.", prov$klaster[i], fmt_id(Z_PENCILAN, 1),
                                  paste(sprintf("%s (z = %s)", v, fmt_s(zi[v], 1)), collapse = ", ")))
    })
    sa <- DATA$multivar$semua$rlb$step; st <- DATA$multivar$tanpa_pencilan$rlb$step
    ket_var <- if (setequal(sa$vars, st$vars)) "variabel terpilih yang sama" else paste("variabel terpilih yang berbeda, yaitu", paste(st$vars, collapse = ", "))
    tagList(
      bagian("1. Hubungan antarvariabel",
             poin("Distribusi", sprintf("%s dari %s variabel berdistribusi normal (Shapiro-Wilk, \u03B1 = %s); korelasi memakai Pearson bila kedua variabel normal, selain itu Spearman.",
                                        fmt_id(sum(nm$normal)), fmt_id(nrow(nm)), fmt_id(ALPHA_NORMAL, 2))),
             poin("Korelasi dengan Informal", sprintf("%s paling kuat (r = %s, %s); %s dari 9 faktor signifikan pada \u03B1 = %s%%.",
                                                      kr$variabel[1], fmt_s(kr$r[1], 2), kr$metode[1], fmt_id(sum(kr$p < ALPHA_REG)), fmt_id(ALPHA_REG * 100))),
             poin("Multikolinearitas", sprintf("VIF terbesar full model %s (%s), %s batas %s.", names(which.max(f$vif)), fmt_id(max(f$vif), 2),
                                               if (max(f$vif) < VIF_BATAS) "di bawah" else "melampaui", fmt_id(VIF_BATAS)))),
      bagian("2. Model regresi",
             poin("Kecocokan", sprintf("Adj R\u00B2 full model %s, stepwise %s, dan PCR %s.", fmt_id(f$adj_r2, 3), fmt_id(s$adj_r2, 3), fmt_id(r$adj_r2, 3))),
             poin("Diagnostik stepwise", sprintf("Shapiro residual p = %s; Breusch-Pagan p = %s.", fmt_p(s$shapiro[["p"]]), fmt_p(s$bp[["p"]]))),
             poin("Tanda koefisien", if (length(beda)) sprintf("Berbeda antara full model dan PCR pada %s, sehingga tanda pada full model perlu dibaca hati-hati.", paste(beda, collapse = ", "))
                  else "Tanda koefisien baku full model dan PCR sama untuk semua variabel.")),
      bagian("3. Klaster provinsi",
             poin("Validasi", sprintf("Klaster dibentuk tanpa Informal, tetapi rata-rata Informal berbeda antarklaster: %s%% (klaster %s) hingga %s%% (klaster %s).",
                                      fmt_id(min(rata_inf), 1), names(which.min(rata_inf)), fmt_id(max(rata_inf), 1), names(which.max(rata_inf)))),
             li_klaster),
      bagian("4. Provinsi pencilan",
             poin("Perlakuan", sprintf("Pencilan tetap disertakan. Bila dikeluarkan, Adj R\u00B2 stepwise berubah dari %s (%s provinsi) menjadi %s (%s provinsi) dengan %s.",
                                       fmt_id(sa$adj_r2, 3), fmt_id(nrow(prov)), fmt_id(st$adj_r2, 3), fmt_id(nrow(prov) - length(pen)), ket_var)),
             li_pen))
  })
  output$b3_implikasi <- renderUI({
    m <- M(); f <- m$rlb$full; s <- m$rlb$step; r <- m$pcr$model
    kp <- m$pcr$koef[match(VAR_X, m$pcr$koef$variabel), ]; kf <- f$koef[match(VAR_X, f$koef$variabel), ]
    stabil <- VAR_X[VAR_X %in% s$vars & kf$signifikan & sign(kf$beta_baku) == sign(kp$beta_baku)]
    tags$ul(
      poin("Faktor prioritas",
           if (length(stabil)) sprintf("%s signifikan di full model dan stepwise dengan tanda sama pada PCR; paling layak ditelaah lebih lanjut dengan data tingkat individu.", paste(stabil, collapse = ", "))
           else "Tidak ada faktor yang konsisten signifikan di full model dan stepwise dengan tanda sama pada PCR, sehingga belum cukup stabil menjadi dasar prioritas kebijakan."),
      poin("Pemilihan model",
           sprintf("Model yang lebih ringkas %s menjelaskan variasi antarprovinsi dengan baik (Adj R\u00B2: full %s, stepwise %s, PCR %s). Pertimbangkan juga keterbacaan koefisien, bukan hanya kecocokan.",
                   if (max(s$adj_r2, r$adj_r2) >= f$adj_r2) "dapat" else "belum dapat", fmt_id(f$adj_r2, 3), fmt_id(s$adj_r2, 3), fmt_id(r$adj_r2, 3))),
      poin("Sasaran program", "Kelompok provinsi pada Tahap 6 dapat membedakan sasaran program, misalnya provinsi berupah rendah dan berkemiskinan tinggi, dengan pencilan ditinjau terpisah."),
      poin("Keterbatasan", "Sampel hanya 38 provinsi pada satu tahun dan hasilnya asosiatif. Jangan ditafsirkan sebagai efek kausal atau berlaku bagi individu (ecological fallacy)."))
  })
  
  tabel_dt <- function(df) {
    dt <- DT::datatable(df, rownames = FALSE, options = list(
      pageLength = 10, scrollX = TRUE,
      language = list(search = "Cari:", lengthMenu = "Tampilkan _MENU_ baris", zeroRecords = "Tidak ada data yang cocok",
                      info = "Menampilkan _START_\u2013_END_ dari _TOTAL_ baris", infoEmpty = "Tidak ada data",
                      infoFiltered = "(disaring dari _MAX_ baris)",
                      paginate = list(previous = "Sebelumnya", `next` = "Berikutnya"))))
    desimal <- names(df)[vapply(df, function(x) is.double(x) && any(x != round(x), na.rm = TRUE), logical(1))]
    if (length(desimal)) dt <- DT::formatRound(dt, desimal, 3)
    dt
  }
  unduh_csv <- function(df, berkas) downloadHandler(
    filename = function() berkas,
    content = function(file) utils::write.csv(df, file, row.names = FALSE, fileEncoding = "UTF-8", na = ""))
  
  lisa_lok <- DATA$lisa$local
  kab_tab  <- cbind(KAB_D, lisa_lok[match(KAB_D$kode_kabkota, lisa_lok$kode_kabkota), c("Ii", "p", "kategori")])
  rownames(kab_tab) <- NULL
  
  output$md_sumber      <- renderTable(SUMBER_TAB, striped = TRUE, align = "l")
  output$md_variabel    <- renderTable(META_VAR, striped = TRUE, align = "l")
  output$md_tab_prov    <- DT::renderDT(tabel_dt(prov))
  output$md_tab_kab     <- DT::renderDT(tabel_dt(kab_tab))
  output$md_tab_dendro  <- DT::renderDT(tabel_dt(DATA$dendrogram))
  output$md_tab_sun     <- DT::renderDT(tabel_dt(DATA$sunburst))
  output$md_unduh_prov   <- unduh_csv(prov, "provinsi.csv")
  output$md_unduh_kab    <- unduh_csv(kab_tab, "kabkota.csv")
  output$md_unduh_dendro <- unduh_csv(DATA$dendrogram, "dendrogram.csv")
  output$md_unduh_sun    <- unduh_csv(DATA$sunburst, "sunburst.csv")
}