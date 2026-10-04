# ==== [U0] tema bslib + CSS inline + JS ====
tema <- bslib::bs_theme(
  version = 5, primary = WARNA_NAVBAR, bg = "#FFFFFF", fg = WARNA_FONT,   # bslib mewajibkan bg dan fg berpasangan; latar halaman biru langit diatur lewat CSS body
  base_font = bslib::font_collection("system-ui", "-apple-system", "Segoe UI", "Roboto", "Helvetica Neue", "Arial", "sans-serif")
)

CSS_APP <- r"---(

body { background-color: #56B4E9; color: #000000; }
.card { background-color: #FFFFFF; }
.text-muted, .text-body-secondary { color: #000000 !important; }
.sumber a, p.sumber a { color: #000000; text-decoration: underline; }
a:focus-visible, button:focus-visible, summary:focus-visible { outline: 3px solid #000000; outline-offset: 2px; }


.navbar { background-color: #0072B2 !important; border-bottom: 3px solid #000000; }
.navbar, .navbar .navbar-brand, .navbar .nav-link { color: #FFFFFF !important; }
.navbar .nav-link { border-bottom: 3px solid transparent; }
.navbar .nav-link:hover { border-bottom-color: #56B4E9; }
.navbar .nav-link.active { font-weight: 700; border-bottom-color: #F0E442; }
.navbar-toggler { border-color: #FFFFFF !important; }
.navbar-toggler-icon { background-image: url("data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 30 30'%3e%3cpath stroke='rgba%28255,255,255,1%29' stroke-linecap='round' stroke-miterlimit='10' stroke-width='2' d='M4 7h22M4 15h22M4 23h22'/%3e%3c/svg%3e") !important; }


.vb-hijau  { background-color: #009E73 !important; color: #000000 !important; }
.vb-oranye { background-color: #E69F00 !important; color: #000000 !important; }
.vb-ungu   { background-color: #CC79A7 !important; color: #000000 !important; }
.vb-hijau *, .vb-oranye *, .vb-ungu * { color: #000000 !important; }
.vb-hijau svg, .vb-oranye svg, .vb-ungu svg, .vb-hijau svg *, .vb-oranye svg *, .vb-ungu svg * { fill: #000000 !important; }
.vb-hijau .value-box-showcase, .vb-oranye .value-box-showcase, .vb-ungu .value-box-showcase { opacity: 1 !important; }


details.encoding { font-size: .85rem; margin: .5rem 0; padding: .4rem .65rem; background: #FFFFFF;
  border: 1px solid #000000; border-left: 6px solid #0072B2; border-radius: .35rem; }
details.encoding summary { cursor: pointer; font-weight: 600; }
details.encoding ul { margin: .4rem 0 0; padding-left: 1.1rem; }
details.encoding li { margin-bottom: .2rem; }



.peta-satu { height: calc(100vh - 5.5rem); min-height: 480px; }
.peta-satu > .card-body { padding: 0; overflow: hidden; }
.peta-satu .card-header { padding: .35rem .8rem; }
.peta-satu .card-footer, .peta-satu .sumber { margin: 0; padding: .25rem .8rem; font-size: .75rem; }
.peta-layout { --_padding: 0; }
.peta-layout .leaflet-container { height: 100% !important; }
.peta-sidebar .sidebar-content { padding: .6rem .8rem !important; gap: .35rem !important; overflow-y: auto; }
.peta-sidebar .sidebar-title { font-size: 1.05rem; margin-bottom: .1rem; }
.peta-sidebar .form-group, .peta-sidebar .shiny-input-container { margin-bottom: .3rem; width: 100%; }
.peta-sidebar label.control-label, .peta-sidebar .shiny-options-group { font-size: .85rem; }
.peta-sidebar .radio, .peta-sidebar .form-check { margin-bottom: 0; min-height: 0; }
.peta-sidebar .irs { font-size: .75rem; }


.satu-layar { display: flex; flex-direction: column; gap: .6rem; height: calc(100vh - 1.4rem); min-height: 560px; margin-top: .6rem; }
.satu-layar > .card { flex: 0 0 auto; }
.satu-layar > .bslib-grid { flex: 1 1 0; min-height: 0; }
.satu-layar .card-header { padding: .35rem .8rem; }
.satu-layar .card-body { padding: .4rem .8rem; overflow-y: auto; }
.satu-layar .card-body p { margin-bottom: .35rem; }
.satu-layar .card-footer { padding: .25rem .8rem; }
.satu-layar .sumber { font-size: .75rem; }
.satu-layar .plotly, .satu-layar .html-widget { min-height: 180px; }
.satu-layar table { margin-bottom: 0; font-size: .82rem; }
.satu-layar table td, .satu-layar table th { padding: .18rem .4rem; }
.satu-layar .card-body > p, .satu-layar .shiny-html-output p { font-size: .85rem; line-height: 1.3; margin-bottom: .25rem; }
.satu-layar .panel-nama { font-size: 1.1rem; font-weight: 600; margin: 0; }
.satu-layar .panel-prov { font-size: .85rem; margin: 0 0 .3rem; }
.satu-layar .panel-klik td:first-child { white-space: nowrap; padding-right: .8rem; }
.satu-layar .tabel-gulir table { min-width: 0; }


.satu-layar-b { margin-top: .8rem; height: auto; min-height: calc(100vh - 1.4rem); }
.satu-layar-b > .bslib-grid:first-child { flex: 0 0 auto; }
.satu-layar-b > .bslib-grid:last-child { flex: 1 1 auto; min-height: min-content; }
.satu-layar-b .card, .satu-layar-b .card-body { overflow: visible; max-height: none; }
.satu-layar-b .card-header { padding: .3rem .8rem; font-size: .9rem; }
.satu-layar-b .card-footer, .satu-layar-b .sumber { padding: .2rem .8rem; font-size: .72rem; }
.satu-layar-b table { font-size: .74rem; margin-bottom: 0; }
.satu-layar-b table td, .satu-layar-b table th { padding: .06rem .4rem; }
.satu-layar-b .card-body { padding: .3rem .8rem; }
.satu-layar-b ul { font-size: .82rem; line-height: 1.28; padding-left: 1.1rem; margin-bottom: 0; }
.satu-layar-b li { margin-bottom: .3rem; }


.satu-layar > .bslib-grid, .satu-layar > .card, .satu-layar > .bslib-grid > .card { margin-bottom: 0 !important; }
.satu-layar:not(.satu-layar-b) { margin-bottom: 0 !important; }

*:has(> .satu-layar-b) { gap: 0 !important; row-gap: 0 !important; }
.satu-layar:not(.satu-layar-b) { margin-top: 1.6rem; }

.satu-layar-b { margin-top: 16px !important; gap: .4rem; }


.kor-baris > .card-body { padding: .3rem .8rem; gap: .2rem; overflow: visible; }
.kor-baris .card-header { padding: .35rem .8rem; }
.kor-baris .card-footer, .kor-baris .sumber { padding: .2rem .8rem; font-size: .72rem; }
.kor-baris .plotly, .kor-baris .html-widget { min-height: 300px; }
.kor-baris .keterangan { font-size: .85rem; line-height: 1.3; margin: .1rem 0 0; }
.kor-rlb .rumus-rlb { font-size: .95rem; line-height: 1.35; padding: .3rem .7rem; }
.kor-rlb p, .kor-rlb ul { margin-bottom: .2rem; }
.kor-rlb ul { padding-left: 1.1rem; font-size: .8rem; line-height: 1.3; }
.kor-rlb li { margin-bottom: .1rem; }
.kor-baris > .card-body { overflow: visible !important; }
.kor-baris #b3_heat { min-height: 520px; }
.kor-rlb .shiny-html-output p.small, .kor-rlb .shiny-html-output .keterangan { font-size: .82rem; line-height: 1.3; }
.kor-hasil { min-height: calc(100vh - 10.5rem); }
.kor-hasil > .card-body { display: flex; flex-direction: column; }
.kor-hasil .shiny-html-output { flex: 1 1 auto; display: flex; flex-direction: column; }
.kor-hasil .shiny-html-output > div { flex: 1 1 auto; display: flex; flex-direction: column; justify-content: space-evenly; }
.kor-hasil .rumus-rlb { font-size: 1rem; line-height: 1.45; padding: .5rem .8rem; }
.kor-hasil .shiny-html-output p { margin-bottom: .15rem; }
.kor-hasil .shiny-html-output p.small, .kor-hasil .shiny-html-output .keterangan { font-size: .9rem; line-height: 1.35; }
.kor-temuan > .card-body { padding-top: .2rem; padding-bottom: .3rem; }
.kor-temuan .shiny-html-output p { font-size: .85rem; font-weight: 600; margin: .3rem 0 .05rem !important; }
.kor-temuan .shiny-html-output ul { padding-left: 1.1rem; margin-bottom: 0; }
.kor-temuan .shiny-html-output li { font-size: .8rem; line-height: 1.25; margin-bottom: .05rem; }
.kor-sc .form-group, .kor-sc .shiny-input-container { margin-bottom: .1rem; }


.hero { padding: .25rem 0 1rem; }


.beranda-satu { display: flex; flex-direction: column; justify-content: space-between; gap: .9rem;
  min-height: calc(100vh - 5.5rem); }
.beranda-satu > * { margin-bottom: 0; }
.beranda-satu .hero { padding: 0; }
.beranda-satu .hero h2 { font-size: clamp(1.25rem, 2.3vw, 1.75rem); margin-bottom: 0; }
.beranda-satu .bslib-grid { gap: .9rem; }
.beranda-satu .card-body { padding: .45rem .8rem; }
.beranda-satu .card-header { padding: .3rem .8rem; }
.beranda-satu .card-body p { margin-bottom: .35rem; }
.beranda-satu .sumber { margin: .4rem 0 0; }
.beranda-satu h3 { margin: 0; }
.beranda-satu .definisi { font-size: .92rem; line-height: 1.35; }
.hero h2 { margin-top: 0; font-weight: 700; }
.beranda-satu { justify-content: flex-start; gap: .75rem; }
.beranda-satu .hero h2 { font-size: clamp(1.2rem, 2.1vw, 1.6rem); }
.beranda-satu .bslib-grid { gap: .75rem; }
.beranda-satu .bslib-value-box { min-height: 0; }
.beranda-satu .bslib-value-box .value-box-title { font-size: .92rem; }
.beranda-satu .bslib-value-box .value-box-value { font-size: 1.8rem; }
.beranda-satu .bslib-value-box .value-box-showcase { max-height: 4rem; }
.beranda-satu .sumber { font-size: .78rem; margin: .3rem 0 0; }
.beranda-satu .definisi { font-size: .88rem; line-height: 1.35; margin: 0; }
.beranda-satu h3 { font-size: 1.05rem; }
.beranda-satu .blok-peta { flex: 1 1 auto; display: flex; flex-direction: column; gap: .4rem; min-height: 0; }
.beranda-satu .blok-peta > .bslib-grid { flex: 1 1 auto; }
.beranda-satu .kotak-bab .card-header { font-size: .92rem; padding: .35rem .8rem; }
.beranda-satu .kotak-bab .card-body { padding: .6rem .8rem; }
.beranda-satu .kotak-bab .ringkasan { font-size: .95rem; line-height: 1.3; margin-bottom: .4rem; }
.beranda-satu .kotak-bab ul.daftar-bab { font-size: .88rem; line-height: 1.35; margin-bottom: .6rem; }
.beranda-satu .kotak-bab ul.daftar-bab li { margin-bottom: .1rem; }
.beranda-satu .kotak-bab .btn { padding: .25rem .8rem; font-size: .88rem; }


.kotak-bab .card-body { display: flex; flex-direction: column; }
.kotak-bab .ringkasan { font-weight: 600; margin-bottom: .35rem; }
.kotak-bab ul.daftar-bab { padding-left: 1.1rem; margin-bottom: .75rem; font-size: .92rem; line-height: 1.35; }
.kotak-bab ul.daftar-bab li { margin-bottom: .15rem; }
.kotak-bab .aksi-mulai { margin-top: auto; }



.beranda-satu .blok-kpi .bslib-grid, .beranda-satu .blok-kpi .bslib-value-box { margin-bottom: 0; }
.beranda-satu .blok-kpi .sumber { margin: .1rem 0 0; }

.kotak-bab .card-body { gap: 0 !important; }
.kotak-bab .card-body > * { flex: 0 0 auto !important; }
.beranda-satu .kotak-bab .ringkasan { margin-bottom: .1rem; }
.beranda-satu .kotak-bab ul.daftar-bab { margin-top: 0; margin-bottom: .2rem; }
.kotak-bab .aksi-mulai { margin-top: auto; padding-top: .4rem; }
.beranda-satu .blok-peta { margin-top: .6rem; }


:root { --lv1:#0072B2; --lv2:#56B4E9; --lv3:#009E73; --lv4:#F0E442; --garis:#000000; --gap:clamp(11px, 2.8vh, 18px); }

.dendro-wrap { overflow-x: auto; padding: .25rem .25rem .4rem; }
.dendro { min-width: 640px; }
.dendro ul { list-style: none; margin: 0; padding: 0; display: flex; justify-content: center; }
.dendro ul ul { position: relative; padding-top: var(--gap); }
.dendro li { position: relative; display: flex; flex-direction: column; align-items: center;
  padding: var(--gap) 8px 0; }
.dendro > ul > li { padding-top: 0; }


.dendro li::before, .dendro li::after {
  content: ""; position: absolute; top: 0; width: 50%; height: var(--gap);
  border-top: 2px solid var(--garis);
}
.dendro li::before { right: 50%; }
.dendro li::after  { left: 50%; border-left: 2px solid var(--garis); }
.dendro li:first-child::before, .dendro li:last-child::after { border: 0 none; }
.dendro li:last-child::before { border-right: 2px solid var(--garis); }
.dendro li:only-child::before, .dendro li:only-child::after { display: none; }
.dendro > ul > li::before, .dendro > ul > li::after { display: none; }
.dendro ul ul::before {
  content: ""; position: absolute; top: 0; left: 50%;
  border-left: 2px solid var(--garis); height: var(--gap);
}


.kartu { position: relative; min-width: 145px; max-width: 185px; padding: clamp(.3rem, 1vh, .5rem) .65rem;
  border-radius: .6rem; text-align: center; color: #000;
  box-shadow: 0 1px 3px rgba(0,0,0,.25); outline: none; }
.kartu:focus-visible { outline: 3px solid #000; outline-offset: 2px; }
.kartu[data-id] { cursor: pointer; }
.kartu.terpilih { outline: 4px solid #D55E00; outline-offset: 3px; }
.kartu .nama   { font-size: .82rem; font-weight: 600; line-height: 1.15; }
.kartu .angka  { font-size: clamp(1.2rem, 3vh, 1.5rem); font-weight: 700; line-height: 1.2; }
.kartu .satuan { font-size: .7rem; line-height: 1.15; opacity: .95; }
.kartu.lv1 { background: var(--lv1); color: #fff; }
.kartu.lv2 { background: var(--lv2); }
.kartu.lv3 { background: var(--lv3); }
.kartu.lv4 { background: var(--lv4); }


.kartu .detail { display: none; position: absolute; inset: 0; border-radius: inherit;
  background: rgba(255,255,255,.97); color: #000; border: 2px solid #000; padding: .4rem .5rem;
  font-size: .78rem; line-height: 1.35; flex-direction: column;
  align-items: center; justify-content: center; z-index: 5; }
.kartu:hover .detail, .kartu:focus .detail { display: flex; }


.legenda-pdr { max-width: 560px; }
.legenda-horizontal { display: none; }

:root { --tinggi-sunburst: calc(100vh - 14.5rem); }
.rumus-rlb { font-size: 1.15rem; padding: .5rem .75rem; background: #f4f8fb; border-radius: .375rem; overflow-x: auto; }
.keterangan { font-size: .88rem; line-height: 1.3; margin: .4rem 0 0; }
@media (min-width: 768px) {
  .kartu-layar { min-height: calc(100vh - 4rem); }
  #b1_sunburst { height: var(--tinggi-sunburst) !important; min-height: 380px; max-height: 760px; }
  .kartu-layar > .card-body { display: flex; flex-direction: column; justify-content: center; padding-top: .5rem; padding-bottom: .5rem; }
  .kartu-layar .baris-fleks { margin-bottom: 0; }
  .kartu-layar .keterangan { margin-top: .25rem; }
  .kartu-layar .bslib-grid { flex-shrink: 0; }
}

.kartu-layar.kartu-dendro { min-height: 0; }
.kartu-layar.kartu-dendro > .card-body { justify-content: flex-start; padding-top: .1rem; padding-bottom: .1rem; }
.kartu-dendro .dendro-wrap { padding-top: .7rem; padding-bottom: .2rem; }
.kartu-dendro .keterangan { margin: .3rem 0 0; }
.kartu-dendro .keterangan ul { margin-bottom: 0; }

.kartu-layar.kartu-sunburst { min-height: 0; }
.kartu-layar.kartu-sunburst > .card-body { justify-content: flex-start; padding-top: .1rem; padding-bottom: .1rem; }
.kartu-sunburst .baris-fleks { margin-top: .1rem; }
.kartu-sunburst #b1_sunburst { height: calc(100vh - 14.5rem) !important; }

[id$="_temuan"] li, [id$="_implikasi"] li { font-size: .88rem; line-height: 1.3; }
[id$="_temuan"] ul, [id$="_implikasi"] ul { margin-bottom: 0; }
.legenda-vert { padding: .25rem 0 .25rem .25rem; }
.lv-badan { display: flex; gap: .6rem; height: 240px; }
.lv-bar { position: relative; width: 16px; flex: none; border-radius: 8px; border: 1px solid #000000; }
.lv-tick { position: absolute; left: -5px; right: -5px; height: 2px; background: #000000; }
.lv-label { position: relative; flex: 1; }
.legenda-pdr .gradien { height: 12px; border-radius: 6px; margin: .25rem 0; }
.swatch-abu { display: inline-block; width: .9rem; height: .9rem; background: #BDBDBD;
  border-radius: 3px; vertical-align: -2px; margin-right: .35rem; }
.baris-fleks { display: flex; flex-wrap: wrap; align-items: center; gap: .5rem; margin-bottom: .5rem; }
.breadcrumb { margin-bottom: 0; }


.legenda-simbol { background: rgba(255,255,255,.92); padding: 6px 10px; border-radius: 5px;
  box-shadow: 0 1px 4px rgba(0,0,0,.3); font-size: 12px; line-height: 1.3; }
.legenda-simbol .lg-simbol { display: flex; align-items: center; gap: 8px; margin-top: 4px; }
.legenda-simbol .lg-bulat { display: inline-block; border-radius: 50%; background: rgba(91,107,127,.55);
  border: 1px solid #000000; flex: none; }
.panel-klik td:first-child { color: #000; width: 38%; }


.card-header { font-weight: 600; line-height: 1.3; white-space: normal; }
.cara-baca { font-size: .875rem; color: #000000; }
.sumber { font-size: .85rem; color: #000000; }
.aksi-pilih { min-height: 2.5rem; }


.tabel-gulir { overflow-x: auto; }
.tabel-gulir table { min-width: 420px; }


.stepper-b3 ol { list-style: none; display: flex; flex-wrap: wrap; gap: .4rem; padding: 0; margin: .4rem 0 .2rem; counter-reset: langkah; }
.stepper-b3 li { counter-increment: langkah; background: #FFFFFF; border: 2px solid #000000; border-radius: 2rem; padding: .15rem .8rem .15rem .3rem; }
.stepper-b3 li::before { content: counter(langkah); display: inline-block; min-width: 1.5rem; height: 1.5rem; line-height: 1.5rem; text-align: center;
  border-radius: 50%; background: #0072B2; color: #FFFFFF; font-weight: 700; margin-right: .4rem; }
.stepper-b3 li.aktif { background: #F0E442; font-weight: 700; }
.stepper-b3 a { color: #000000; text-decoration: none; }
.stepper-b3 a:hover { text-decoration: underline; }
.tabel-b3 table { font-size: .8rem; }
.ket-norm { font-size: .8rem; line-height: 1.35; }
.kmo-grid { display: grid; grid-template-columns: repeat(3, minmax(0, 1fr)); gap: .75rem; align-items: stretch; }
@media (max-width: 767px) { .kmo-grid { grid-template-columns: 1fr; } }
.kmo-box { display: flex; flex-direction: column; gap: .2rem; padding: .6rem .9rem; border-radius: .5rem; }
.kmo-box .kmo-t { font-size: .8rem; font-weight: 600; }
.kmo-box .kmo-v { font-size: 1.5rem; font-weight: 700; line-height: 1.15; }
.kmo-box .kmo-d { font-size: .78rem; line-height: 1.35; margin: 0; }


@media (max-width: 767px) {
  .hero h2 { font-size: 1.35rem; }
  .beranda-satu { min-height: 0; justify-content: flex-start; }
  .kartu .angka { font-size: 1.25rem; }
  #b2_peta   { height: 440px !important; }
  #b1_sunburst { height: 420px !important; }
  #b3_biplot, #b3_scree { height: 420px !important; }
  #b3_paralel { height: 380px !important; }
  .legenda-pdr { max-width: 100%; }
  .legenda-vert { display: none; }
  .legenda-horizontal { display: block; }
  #b2_hist { height: 240px !important; }
  #b3_heat { height: 600px !important; }
  #b3_hist, #b3_qq, #b3_kor_bar, #b3_sc, #b3_forest, #b3_vif, #b3_res_fit, #b3_res_qq, #b3_ovp, #b3_bar_koef { height: 340px !important; }
  #b3_kor_heat, #b3_load { height: 420px !important; }
  .stepper-b3 li { font-size: .8rem; }
  .container-fluid, .bslib-page-main { padding-left: .6rem; padding-right: .6rem; }
  .card-header { font-size: .95rem; }
  details.encoding { font-size: .8rem; }
  .navbar-brand { font-size: 1rem; }
}
)---"

JS_HIERARKI <- "

$(document).on('click', '.kartu[data-id]', function() {
  Shiny.setInputValue('b1_kartu', $(this).attr('data-id'), {priority: 'event'});
});
$(document).on('keydown', '.kartu[data-id]', function(e) {
  if (e.key === 'Enter' || e.key === ' ') { e.preventDefault(); $(this).trigger('click'); }
});
Shiny.addCustomMessageHandler('kartu_sorot', function(m) {
  $('.kartu').removeClass('terpilih');
  if (m.id) { $('.kartu[data-id=\"' + m.id + '\"]').addClass('terpilih'); }
});
Shiny.addCustomMessageHandler('hierarki_level', function(m) {
  var el = document.getElementById(m.id);
  if (el && window.Plotly) { Plotly.restyle(el, {level: m.level}, [0]); }
});
"
JS_KLIK_SUNBURST <- "
function(el, x, data) {
  if (el.removeAllListeners) { el.removeAllListeners('plotly_sunburstclick'); }
  el.on('plotly_sunburstclick', function(ev) {
    if (ev && ev.points && ev.points.length && window.Shiny) {
      Shiny.setInputValue(data.input, ev.points[0].id, {priority: 'event'});
    }
    return false;
  });
}
"

JS_STEPPER <- "
Shiny.addCustomMessageHandler('stepper_aktif', function(m) {
  $('.stepper-b3 li').removeClass('aktif');
  $('#stepper_' + m.i).addClass('aktif');
});
"

JS_ENCODING <- r"---(
document.addEventListener('DOMContentLoaded', function() {
  if (window.matchMedia('(min-width: 768px)').matches) {
    document.querySelectorAll('details.encoding').forEach(function(d) { d.open = true; });
  }
});
)---"
encoding_visual <- function(posisi, warna, ukuran, bentuk) {
  item <- list(Posisi = posisi, Warna = warna, Ukuran = ukuran, Bentuk = bentuk)
  tags$details(class = "encoding",
               tags$summary("Mengapa encoding ini? (posisi, warna, ukuran, bentuk)"),
               tags$ul(lapply(names(item), function(k) tags$li(tags$b(paste0(k, ": ")), item[[k]]))))
}

cara_baca <- function(...) p(class = "cara-baca", tags$b("Cara membaca: "), ...)
footer_sumber <- function(teks) bslib::card_footer(span(class = "sumber", teks))
butir <- function(judul, ...) tags$li(tags$b(judul), tags$br(), ...)
ui_temuan <- function(id) {
  bslib::layout_columns(
    col_widths = bslib::breakpoints(sm = 12, lg = c(6, 6)), fill = FALSE,
    bslib::card(bslib::card_header("Temuan utama"), uiOutput(paste0(id, "_temuan"))),
    bslib::card(bslib::card_header("Implikasi kebijakan"), uiOutput(paste0(id, "_implikasi")))
  )
}

legenda_pdr <- function(mid, lebar = LEBAR_WARNA_PDR) {
  div(class = "legenda-pdr mt-2",
      div(class = "fw-semibold small", "Persentase perempuan"),
      div(class = "gradien", style = paste0("background:", css_gradien_puor(50), ";")),
      div(style = "position:relative;height:2.4rem;",
          span(class = "small", style = "position:absolute;left:0;", sprintf("\u2264 %s%%", fmt_id(mid - lebar, 1))),
          span(class = "small text-center", style = "position:absolute;left:50%;transform:translateX(-50%);",
               sprintf("%s%%", fmt_id(mid, 1)), tags$br(), "rata-rata pekerja"),
          span(class = "small", style = "position:absolute;right:0;", sprintf("\u2265 %s%%", fmt_id(mid + lebar, 1)))),
      div(class = "small mt-1", span(class = "swatch-abu"), "Abu-abu: rincian jenis kelamin tidak tersedia"))
}

legenda_pdr_vertikal <- function(mid, lebar = LEBAR_WARNA_PDR) {
  gr <- sub("to right", "to top", css_gradien_puor(50), fixed = TRUE)
  div(class = "legenda-vert",
      div(class = "fw-semibold small mb-2", "Persentase perempuan"),
      div(class = "lv-badan",
          div(class = "lv-bar", style = paste0("background:", gr, ";"),
              div(class = "lv-tick", style = "bottom:50%;")),
          div(class = "lv-label",
              span(class = "small", style = "position:absolute;top:0;", sprintf("\u2265 %s%%", fmt_id(mid + lebar, 1))),
              span(class = "small", style = "position:absolute;bottom:50%;transform:translateY(50%);",
                   sprintf("%s%% (rata-rata seluruh pekerja)", fmt_id(mid, 1))),
              span(class = "small", style = "position:absolute;bottom:0;", sprintf("\u2264 %s%%", fmt_id(mid - lebar, 1))))),
      div(class = "small mt-3", span(class = "swatch-abu"), "Abu-abu: rincian jenis kelamin tidak tersedia"),
      div(class = "small mt-2", "Besar irisan = jumlah orang"))
}

KPI   <- kpi_beranda(DATA$dendrogram, DATA$sunburst)
KAB_D <- sf::st_drop_geometry(DATA$kabkota)

.q <- stats::quantile(KAB_D$rasio, c(.25, .5, .75), names = FALSE)
JUDUL_HIST <- sprintf("Separuh kab/kota memiliki rasio informal antara %s%% dan %s%% (median %s%%)",
                      fmt_id(.q[1] * 100, 1), fmt_id(.q[3] * 100, 1), fmt_id(.q[2] * 100, 1))
.pv  <- DATA$provinsi
.K   <- DATA$pca_klaster$k
.bk1 <- DATA$multivar$semua$korelasi$xy[1, ]
JUDUL_KOR <- sprintf("%s paling kuat berasosiasi dengan persentase informal antar provinsi (r = %s, %s)", .bk1$variabel, fmt_s(.bk1$r, 2), .bk1$metode)
.zp  <- heatmap_matrix(.pv, VAR_X)
.pisah <- vapply(colnames(.zp), function(v) sum(tapply(.zp[, v], .pv$klaster, function(x) length(x) * mean(x)^2)), numeric(1))
JUDUL_PAR  <- sprintf("Klaster provinsi paling jelas berbeda pada %s", names(which.max(.pisah)))
JUDUL_HEAT <- sprintf("%s provinsi terkelompok menjadi %s klaster berdasarkan kemiripan sembilan variabel faktor (Informal tidak dipakai membentuk klaster)",
                      fmt_id(nrow(.pv)), fmt_id(.K))
.ag <- agg_provinsi(DATA$kabkota)
.maks_sel <- max(abs(.ag$informal_pct - .pv$Informal[match(.ag$kode_prov, .pv$kode_prov)]))

# ==== [U1] Beranda ====
kotak_bab <- function(no, judul, ringkasan, poin, tombol) {
  bslib::card(
    class = "kotak-bab",
    bslib::card_header(sprintf("Bab %d. %s", no, judul)),
    bslib::card_body(
      p(class = "ringkasan", ringkasan),
      tags$ul(class = "daftar-bab", lapply(poin, tags$li)),
      div(class = "aksi-mulai",
          actionButton(tombol, "Mulai", icon = icon("arrow-right"), class = "btn-primary", width = "8rem"))
    )
  )
}

ui_beranda <- bslib::nav_panel(
  "Beranda", value = "beranda", icon = icon("house"),
  div(class = "beranda-satu",
      div(class = "hero", h2(action_title_beranda(KPI))),
      div(class = "blok-kpi",
          bslib::layout_columns(
            col_widths = bslib::breakpoints(sm = 12, md = c(4, 4, 4)), fill = FALSE,
            bslib::value_box("Penduduk usia kerja (juta orang)", fmt_id(KPI$usia_kerja, 2), showcase = icon("users"), class = "vb-hijau"),
            bslib::value_box("Bekerja (juta orang)", fmt_id(KPI$bekerja, 2), showcase = icon("briefcase"), class = "vb-oranye"),
            bslib::value_box(sprintf("Pekerja informal (juta orang)", fmt_id(KPI$pct_informal, 1)),
                             fmt_id(KPI$informal, 2), showcase = icon("user-clock"), class = "vb-ungu")
          ),
          p(class = "sumber", sprintf("Sumber: %s. Periode data: %s.", SUMBER_SAKERNAS, PERIODE_SAKERNAS))
      ),
      bslib::card(
        fill = FALSE,
        p(class = "definisi",
          "Konsep yang digunakan BPS tentang ", tags$b("pekerja formal"), " adalah pekerja yang berstatus buruh/karyawan dan berusaha sendiri dibantu buruh tetap, ",
          "sedangkan ", tags$b("pekerja informal"), " adalah mereka yang berstatus berusaha sendiri, berusaha dibantu buruh tidak tetap, pekerja bebas, dan pekerja keluarga.")
      ),
      div(class = "blok-peta",
          h3(class = "h5", "Peta jalan"),
          bslib::layout_columns(
            col_widths = bslib::breakpoints(sm = 12, md = c(4, 4, 4)), fill = FALSE,
            kotak_bab(1, "Kondisi Umum",
                      "Gambaran struktur ketenagakerjaan dari penduduk usia kerja hingga pekerja informal.",
                      c("Dendrogram struktur penduduk usia kerja",
                        "Sunburst pengelompokan tenaga kerja",
                        "Komposisi pekerja menurut jenis kelamin"),
                      "mulai_bab1"),
            kotak_bab(2, "Sebaran Wilayah",
                      "Sebaran rasio pekerja informal di 514 kabupaten/kota.",
                      c("Peta choropleth dan simbol proporsional",
                        "Klaster spasial (LISA)",
                        "Kabupaten/kota dengan rasio tertinggi dan terendah"),
                      "mulai_bab2"),
            kotak_bab(3, "Faktor yang Menyertai",
                      "Analisis faktor yang menyertai pekerja informal di 38 provinsi.",
                      c("Uji kenormalan dan korelasi",
                        "Regresi berganda",
                        "PCA dan regresi ulang pada komponen utama",
                        "Pengelompokan provinsi dan pencilan",
                        "Temuan utama dan implikasi"),
                      "mulai_bab3")
          ))
  )
)

# ==== [U2] Bab 1 — Kondisi umum (hierarki) ====
.usia    <- DATA$dendrogram$juta_orang[DATA$dendrogram$id == "usia_kerja"]
.bekerja <- DATA$dendrogram$juta_orang[DATA$dendrogram$id == "bekerja"]
.p_formal   <- DATA$sunburst$pct_perempuan[DATA$sunburst$id == "formal"]
.p_informal <- DATA$sunburst$pct_perempuan[DATA$sunburst$id == "informal"]
MID_PEREMPUAN <- titik_tengah_perempuan(DATA$sunburst)
LEBAR_WARNA_PDR <- 10
.angkatan <- DATA$dendrogram$juta_orang[DATA$dendrogram$id == "angkatan_kerja"]
.peng     <- DATA$dendrogram$juta_orang[DATA$dendrogram$id == "pengangguran"]

ui_bab1 <- bslib::nav_panel(
  "1. Kondisi Umum", value = "bab1",
  bslib::card(
    class = "kartu-layar kartu-dendro",
    bslib::card_header("Struktur Ketenagakerjaan"),
    uiOutput("b1_dendro"),
    div(class = "keterangan",
        tags$ul(class = "mb-0",
                tags$li(sprintf("Penduduk usia kerja berjumlah %s juta orang.", fmt_id(.usia, 2))),
                tags$li(sprintf("Sebanyak %s juta orang (%s%%) termasuk angkatan kerja, yaitu mereka yang bekerja atau sedang mencari kerja.",
                                fmt_id(.angkatan, 2), fmt_id(.angkatan / .usia * 100, 1))),
                tags$li(sprintf("Dari angkatan kerja, %s juta orang (%s%% dari penduduk usia kerja) bekerja dan %s juta orang menganggur.",
                                fmt_id(.bekerja, 2), fmt_id(.bekerja / .usia * 100, 1), fmt_id(.peng, 2))))),
    footer_sumber(sprintf("Sumber: %s. Satuan: juta orang.", SUMBER_SAKERNAS))
  ),
  bslib::card(
    class = "kartu-layar kartu-sunburst",
    bslib::card_header("Pengelompokan Tenaga Kerja"),
    uiOutput("b1_kartu_info"),
    div(class = "baris-fleks",
        div(style = "flex:1;min-width:220px;", uiOutput("b1_breadcrumb")),
        actionButton("b1_reset", "Reset", icon = icon("rotate-left"), class = "btn-outline-secondary btn-sm")),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, md = c(9, 3)), fill = FALSE,
      plotly::plotlyOutput("b1_sunburst", height = "420px"),
      div(style = "align-self:center;",
          legenda_pdr_vertikal(MID_PEREMPUAN),
          div(class = "legenda-horizontal",
              legenda_pdr(MID_PEREMPUAN),
              div(class = "small mt-1", "Besar irisan = jumlah orang")))
    ),
    div(class = "keterangan",
        tags$ul(class = "mb-0",
                tags$li(sprintf("Perempuan adalah %s%% dari pekerja informal, lebih tinggi daripada pada pekerja formal (%s%%).",
                                fmt_id(.p_informal, 1), fmt_id(.p_formal, 1))),
                tags$li(sprintf("Rata-rata persentase perempuan pada seluruh pekerja adalah %s%%.", fmt_id(MID_PEREMPUAN, 1))))),
    footer_sumber(sprintf("Sumber: %s. Satuan: juta orang.", SUMBER_SAKERNAS))
  ),
  ui_temuan("b1")
)

# ==== [U3] Bab 2 — Sebaran wilayah (geospasial + LISA) ====
.prov <- KAB_D[!duplicated(KAB_D$kode_prov), c("kode_prov", "provinsi")]
.prov <- .prov[order(.prov$provinsi), ]
PILIHAN_PROV <- c("Semua provinsi" = "semua", stats::setNames(.prov$kode_prov, .prov$provinsi))
SUMBER_PENDEK <- "Sumber: BPS, Publikasi Keadaan Angkatan Kerja tiap provinsi (Sakernas Agustus 2025)"

ui_bab2 <- bslib::nav_panel(
  "2. Sebaran Wilayah", value = "bab2",
  bslib::card(
    class = "peta-satu", fill = FALSE, full_screen = TRUE,
    bslib::card_header("Peta persebaran pekerja informal"),
    bslib::layout_sidebar(
      fillable = TRUE, class = "peta-layout",
      sidebar = bslib::sidebar(
        title = "Kontrol peta", width = 280, open = "desktop", class = "peta-sidebar",
        radioButtons("b2_dasar", "Layer dasar",
                     choices = c("Choropleth rasio informal" = "choro", "Klaster spasial (LISA)" = "lisa", "Netral" = "netral"),
                     selected = "choro"),
        conditionalPanel("input.b2_dasar == 'choro'",
                         radioButtons("b2_gaya", "Klasifikasi choropleth",
                                      choices = c("Jenks (5 kelas)" = "jenks5", "Kuantil (5 kelas)" = "quantile5", "Interval sama (5 kelas)" = "equal5"),
                                      selected = "jenks5")),
        checkboxInput("b2_simbol", "Tambah simbol proporsional (jumlah informal)", value = FALSE),
        selectInput("b2_prov", "Provinsi", choices = PILIHAN_PROV, selected = "semua"),
        selectizeInput("b2_cari", "Cari kabupaten/kota", choices = c("", sort(KAB_D$nama)), selected = "",
                       options = list(placeholder = "Ketik nama kab/kota")),
        actionButton("b2_reset", "Reset filter", icon = icon("rotate-left"), class = "btn-outline-secondary btn-sm")
      ),
      leaflet::leafletOutput("b2_peta", height = "100%")
    ),
    footer_sumber(SUMBER_PENDEK)
  ),
  div(class = "satu-layar",
      bslib::layout_columns(
        col_widths = bslib::breakpoints(sm = 12, lg = c(6, 6)), fill = TRUE,
        bslib::card(bslib::card_header(JUDUL_HIST),
                    plotly::plotlyOutput("b2_hist", height = "100%"),
                    footer_sumber(SUMBER_PENDEK)),
        bslib::card(bslib::card_header("Kabupaten/kota terpilih"),
                    uiOutput("b2_panel"),
                    footer_sumber(SUMBER_PENDEK))
      ),
      bslib::card(
        fill = FALSE,
        bslib::card_header("Klaster spasial (LISA): wilayah tinggi cenderung bertetangga dengan wilayah tinggi"),
        bslib::layout_columns(
          col_widths = bslib::breakpoints(sm = 12, lg = c(7, 5)), fill = FALSE,
          uiOutput("b2_lisa_teks"),
          div(class = "tabel-gulir", tableOutput("b2_lisa_tab"))
        ),
        footer_sumber(paste0(SUMBER_PENDEK, "; perhitungan LISA: pengolah data"))
      )),
  div(class = "satu-layar satu-layar-b",
      bslib::layout_columns(
        col_widths = bslib::breakpoints(sm = 12, lg = c(6, 6)), fill = FALSE,
        bslib::card(bslib::card_header("10 kab/kota dengan rasio tertinggi (\u26A0 = nilai ekstrem, di luar 1,5 \u00D7 IQR)"),
                    div(class = "tabel-gulir", tableOutput("b2_top_tinggi")),
                    footer_sumber(SUMBER_PENDEK)),
        bslib::card(bslib::card_header("10 kab/kota dengan rasio terendah"),
                    div(class = "tabel-gulir", tableOutput("b2_top_rendah")),
                    footer_sumber(SUMBER_PENDEK))
      ),
      ui_temuan("b2"))
)

# ==== [U4] Bab 3 — Faktor yang menyertai (alur analisis 7 langkah) ====
TAHAP_B3    <- c("Kenormalan", "Korelasi", "Regresi berganda", "PCA", "Regresi ulang (PCR)")
PILIHAN_VAR <- stats::setNames(VAR_MV, label_resp(VAR_MV))
PILIHAN_X   <- stats::setNames(VAR_X, VAR_X)

stepper_b3 <- div(class = "stepper-b3", role = "navigation", `aria-label` = "Tahapan analisis",
                  tags$ol(lapply(seq_along(TAHAP_B3), function(i)
                    tags$li(id = paste0("stepper_", i), actionLink(paste0("b3_step_", i), TAHAP_B3[i])))))

kartu_b3 <- function(judul_id, ..., satuan) {
  bslib::card(
    full_screen = TRUE,
    bslib::card_header(textOutput(judul_id, inline = TRUE)),
    ...,
    footer_sumber(sprintf("Sumber: %s. Satuan: %s.", SUMBER_BAB3, satuan)))
}
kartu_kor <- function(judul, ..., class = NULL, sumber = TRUE) {
  isi <- c(list(bslib::card_header(judul)), list(...),
           if (sumber) list(footer_sumber("Sumber: BPS, tabel dinamis Sakernas/Susenas 2025 dan Statistik Pendidikan Indonesia (Dikdas)")))
  do.call(bslib::card, c(list(class = class, full_screen = TRUE), isi))
}
dua_kolom <- function(...) bslib::layout_columns(col_widths = bslib::breakpoints(sm = 12, lg = c(6, 6)), fill = FALSE, ...)

ui_bab3 <- bslib::nav_panel(
  "3. Faktor yang Menyertai", value = "bab3",
  bslib::navset_card_pill(
    id = "b3_langkah", selected = "l1",
    
    bslib::nav_panel(
      "Tahap 1. Kenormalan", value = "l1",
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Uji Distribusi Normal"),
        div(class = "tabel-gulir tabel-b3", tableOutput("b3_tab_norm")),
        uiOutput("b3_ket_norm"),
        footer_sumber("Sumber: BPS, tabel dinamis Sakernas/Susenas 2025 dan Statistik Pendidikan Indonesia (Dikdas)")),
      dua_kolom(
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Grafik Distribusi Variabel"),
          selectInput("b3_var_norm", "Variabel", choices = PILIHAN_VAR, selected = "Informal"),
          plotly::plotlyOutput("b3_hist", height = "340px"),
          uiOutput("b3_ket_hist"),
          footer_sumber("Sumber: BPS, tabel dinamis Sakernas/Susenas 2025 dan Statistik Pendidikan Indonesia (Dikdas)")),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Q-Q Plot Variabel"),
          plotly::plotlyOutput("b3_qq", height = "340px"),
          uiOutput("b3_ket_qq"),
          footer_sumber("Sumber: BPS, tabel dinamis Sakernas/Susenas 2025 dan Statistik Pendidikan Indonesia (Dikdas)"))),
    ),
    
    bslib::nav_panel(
      "Tahap 2. Korelasi", value = "l2",
      dua_kolom(
        kartu_kor("Korelasi terhadap Variabel Respon", class = "kor-baris",
                  plotly::plotlyOutput("b3_kor_bar", height = "calc(100vh - 17rem)"),
                  uiOutput("b3_ket_bar")),
        kartu_kor("Korelasi antar Variabel", class = "kor-baris",
                  plotly::plotlyOutput("b3_kor_heat", height = "calc(100vh - 17rem)"),
                  uiOutput("b3_ket_cm"))),
      kartu_kor("Korelasi terhadap Kluster", class = "kor-baris kor-sc",
                selectInput("b3_var_x", "Faktor (X)", choices = PILIHAN_X, selected = "%Miskin", width = "240px"),
                plotly::plotlyOutput("b3_sc", height = "calc(100vh - 21rem)"),
                uiOutput("b3_ket_sc"))
    ),
    
    bslib::nav_panel(
      "Tahap 3. Regresi berganda", value = "l3",
      kartu_kor("Tabel Signifikansi Variabel",
                div(class = "tabel-gulir tabel-b3", tableOutput("b3_tab_rlb")),
                uiOutput("b3_ket_rlb")),
      dua_kolom(
        kartu_kor("Regresi Linear Berganda", class = "kor-baris kor-rlb",
                  uiOutput("b3_rumus")),
        kartu_kor("Uji VIF", class = "kor-baris",
                  plotly::plotlyOutput("b3_vif", height = "calc(100vh - 15rem)"),
                  uiOutput("b3_vif_catatan")))
    ),
    
    bslib::nav_panel(
      "Tahap 4. PCA", value = "l4",
      kartu_kor("Uji Kelayakan PCA: KMO dan Bartlett",
                uiOutput("b3_kmo"),
                uiOutput("b3_ket_kmo")),
      dua_kolom(
        kartu_kor("Komponen Utama berdasarkan Varians", class = "kor-baris",
                  plotly::plotlyOutput("b3_scree", height = "calc(100vh - 19.5rem)"),
                  uiOutput("b3_ket_scree")),
        kartu_kor("Variabel per Komponen", class = "kor-baris",
                  plotly::plotlyOutput("b3_load", height = "calc(100vh - 19.5rem)"),
                  uiOutput("b3_ket_load")))
    ),
    
    bslib::nav_panel(
      "Tahap 5. Regresi ulang (PCR)", value = "l5",
      kartu_kor("Tabel Uji Komponen",
                div(class = "tabel-gulir tabel-b3", tableOutput("b3_tab_banding")),
                uiOutput("b3_ket_banding")),
      kartu_kor(tags$a(href = "http://repository.unimus.ac.id/4588/11/Manuscript%20Indonesia.pdf", target = "_blank", "Principal Component Regression"),
                uiOutput("b3_rumus_pcr"), class = "kor-baris kor-rlb"),
      dua_kolom(
        kartu_kor("Perbandingan Full Model dan PCR", class = "kor-baris",
                  plotly::plotlyOutput("b3_bar_koef", height = "calc(100vh - 17rem)"),
                  uiOutput("b3_catatan_tanda")),
        kartu_kor("Scatterplot Perbandingan", class = "kor-baris kor-sc",
                  radioButtons("b3_model_ovp", NULL, inline = TRUE,
                               choices = c("Full model" = "full", "Stepwise" = "step", "PCR" = "pcr"), selected = "pcr"),
                  plotly::plotlyOutput("b3_ovp", height = "calc(100vh - 19rem)"),
                  uiOutput("b3_ket_ovp")))
    ),
    
    bslib::nav_panel(
      "Tahap 6. Klaster & pencilan", value = "l6",
      kartu_kor("Klaster Provinsi berdasarkan Variabel", class = "kor-baris",
                plotly::plotlyOutput("b3_heat", height = "calc(100vh - 14rem)"),
                p(class = "keterangan", JUDUL_HEAT, ". Warna menunjukkan z-score tiap variabel: oranye di atas rata-rata 38 provinsi dan biru di bawahnya.")),
      bslib::layout_columns(
        col_widths = bslib::breakpoints(sm = 12, lg = c(7, 5)), fill = FALSE,
        kartu_kor("Profil Klaster",
                  div(class = "tabel-gulir", tableOutput("b3_profil")),
                  uiOutput("b3_interpretasi")),
        kartu_kor("Daftar Provinsi",
                  DT::DTOutput("b3_tabel")))
    ),
    
    bslib::nav_panel(
      "Tahap 7. Temuan & implikasi", value = "l7",
      dua_kolom(
        kartu_kor("Hasil Regresi sebelum PCA", class = "kor-baris kor-hasil", uiOutput("b3_hasil_pra")),
        kartu_kor("Hasil Regresi setelah PCA", class = "kor-baris kor-hasil", uiOutput("b3_hasil_pasca"))),
      bslib::layout_columns(
        col_widths = bslib::breakpoints(sm = 12, lg = c(8, 4)), fill = FALSE,
        kartu_kor("Temuan Utama", class = "kor-baris kor-temuan", sumber = FALSE, uiOutput("b3_temuan")),
        kartu_kor("Implikasi Kebijakan", class = "kor-baris kor-temuan", sumber = FALSE, uiOutput("b3_implikasi"))))
  )
)

# ==== [U5] Metadata ====
SUMBER_TAB <- data.frame(
  Data = c("Struktur penduduk usia kerja, status pekerjaan, jenis kelamin",
           "Pekerja informal dan total pekerja 514 kab/kota",
           "TPAK, TPT, Upah bersih, Upah buruh per jam, RLS, Persentaset Penduduk Miskin, IPG",
           "Tingkat penyelesaian pendidikan dasar",
           "Batas kabupaten/kota"),
  Sumber = c("BPS, Keadaan Angkatan Kerja di Indonesia (Sakernas Agustus 2025)",
             "BPS, Keadaan Angkatan Kerja tiap provinsi (Sakernas Agustus 2025)",
             "BPS, tabel dinamis Sakernas/Susenas",
             "BPS, Statistik Pendidikan Indonesia",
             "Lapak GIS, LapakGIS_Batas_Kabupaten_2024"),
  Tahun = c("2025", "2025", "2025", "2024", "2024"),
  check.names = FALSE, stringsAsFactors = FALSE)

META_VAR <- data.frame(
  Dataset = c(rep("Provinsi (Bab 3)", 13), rep("Kab/kota (Bab 2)", 7), rep("Hierarki (Bab 1)", 6)),
  Variabel = c("Informal", "TPAK-L", "TPAK-P", "TPT", "Upah Formal", "RLS", "%Miskin", "Upah/Jam", "Dikdas", "IPG",
               "kode_prov", "klaster", "flag_pencilan",
               "kode_kabkota, kode_prov", "nama, provinsi", "informal", "total_pekerja", "rasio", "flag_ekstrem",
               "Ii, p, kategori (LISA)",
               "id, induk, label", "level", "juta_orang", "juta_perempuan", "pct_perempuan", "(sunburst) ukuran dan warna"),
  Deskripsi = c(
    "Persentase pekerja informal terhadap seluruh pekerja. Peran: variabel respons (Y); tidak ikut PCA maupun pembentukan klaster. Sembilan variabel lain (TPAK-L sampai IPG) berperan sebagai faktor (X)",
    "Tingkat partisipasi angkatan kerja laki-laki (X)",
    "Tingkat partisipasi angkatan kerja perempuan (X)",
    "Tingkat pengangguran terbuka (X)",
    "Rata-rata upah pekerja formal (X; pada regresi dipakai juta rupiah)",
    "Rata-rata lama sekolah (X)",
    "Persentase penduduk miskin (X)",
    "Rata-rata upah per jam (X; pada regresi dipakai ribu rupiah)",
    "Indikator pendidikan dasar dari Statistik Pendidikan Indonesia; tahun acuan berbeda dari variabel lain (X)",
    "Indeks Pembangunan Gender (X)",
    "Kode provinsi",
    sprintf("Nomor klaster provinsi (klaster hierarkis Ward.D2 pada sembilan variabel X terstandarisasi, Informal tidak ikut; %d klaster)", DATA$pca_klaster$k),
    sprintf("TRUE bila provinsi berjarak |z| > %s pada minimal %d variabel X", fmt_id(Z_PENCILAN, 1), MIN_VAR_PENCILAN),
    "Kode kabupaten/kota dan provinsi induknya",
    "Nama kabupaten/kota dan provinsi induknya",
    "Jumlah pekerja informal",
    "Jumlah seluruh pekerja",
    "informal / total_pekerja (pecahan 0\u20131; ditampilkan sebagai persen di peta)",
    "TRUE bila rasio di luar 1,5 \u00D7 IQR (nilai ekstrem nasional)",
    sprintf("Local Moran's I, p-value permutasi (%s simulasi), dan kategori klaster spasial (HH, LL, HL, LH, tidak signifikan; k = %d tetangga, \u03B1 = %s)", fmt_id(NSIM_LISA), K_LISA, fmt_id(ALPHA, 2)),
    "Kode simpul, kode induknya, dan nama kelompok penduduk",
    "Level simpul pada hierarki (0 = seluruh penduduk usia kerja)",
    "Jumlah orang pada simpul",
    "Jumlah perempuan pada simpul (kosong bila rinciannya tidak tersedia)",
    "Persentase perempuan pada simpul (kosong bila rinciannya tidak tersedia)",
    "Ukuran irisan = juta_orang; warna irisan = pct_perempuan"),
  Satuan = c("%", "%", "%", "%", "rupiah", "tahun", "%", "rupiah", "%", "indeks",
             "\u2013", "\u2013", "\u2013",
             "\u2013", "\u2013", "orang", "orang", "pecahan", "\u2013", "\u2013",
             "\u2013", "\u2013", "juta orang", "juta orang", "%", "\u2013"),
  check.names = FALSE, stringsAsFactors = FALSE)

ui_metadata <- bslib::nav_panel(
  "Metadata", value = "metadata", icon = icon("database"),
  bslib::card(
    fill = FALSE,
    bslib::card_header("Sumber data"),
    div(class = "tabel-gulir", tableOutput("md_sumber")),
    footer_sumber(sprintf("Periode data Sakernas: %s. Daftar lengkap (URL dan tanggal akses): data/sumber.csv di repositori.", PERIODE_SAKERNAS))
  ),
  bslib::navset_card_tab(
    title = "Data yang digunakan",
    bslib::nav_panel("Provinsi (38)",
                     DT::DTOutput("md_tab_prov"),
                     downloadButton("md_unduh_prov", "Unduh CSV", class = "btn-outline-secondary btn-sm")),
    bslib::nav_panel(sprintf("Kab/kota (%s)", fmt_id(nrow(KAB_D))),
                     DT::DTOutput("md_tab_kab"),
                     downloadButton("md_unduh_kab", "Unduh CSV", class = "btn-outline-secondary btn-sm")),
    bslib::nav_panel("Dendrogram",
                     DT::DTOutput("md_tab_dendro"),
                     downloadButton("md_unduh_dendro", "Unduh CSV", class = "btn-outline-secondary btn-sm")),
    bslib::nav_panel("Sunburst",
                     DT::DTOutput("md_tab_sun"),
                     downloadButton("md_unduh_sun", "Unduh CSV", class = "btn-outline-secondary btn-sm")),
    footer = footer_sumber(sprintf("Sumber: %s; %s. Geometri peta tidak disertakan dalam tabel.", SUMBER_SAKERNAS, "perhitungan LISA, PCA, regresi, dan klaster: pengolah data"))
  ),
  bslib::card(
    fill = FALSE,
    bslib::card_header("Metadata variabel"),
    div(class = "tabel-gulir", tableOutput("md_variabel")),
    footer_sumber("Satuan: \u2013 berarti tidak bersatuan (kode, label, atau penanda TRUE/FALSE).")
  ),
  bslib::card(
    fill = FALSE,
    bslib::card_header("Keterbatasan"),
    tags$ol(
      tags$li("Tahun acuan Dikdas berbeda dari variabel lain (Statistik Pendidikan Indonesia)."),
      tags$li("Estimasi kabupaten kecil (mis. Supiori) kurang stabil."),
      tags$li("Analisis korelasional tingkat provinsi (ecological fallacy), n = 38 kecil; Informal adalah variabel respons (Y), sedangkan PCA dan klaster hanya memakai 9 variabel X."),
      tags$li("Regresi memakai data tingkat provinsi satu tahun tanpa transformasi ln; hasil bersifat asosiatif. Stepwise (AIC) bergantung pada sampel; PCR mengorbankan interpretasi langsung per variabel."),
      tags$li(sprintf("Agregasi kab/kota ke provinsi berselisih sampai %s poin persen dari angka provinsi (terbesar: Papua Barat, Papua, Bengkulu).", fmt_id(.maks_sel, 2))),
      tags$li("Hasil LISA sensitif terhadap pilihan k tetangga (k = 5 dipilih secara konvensional)."),
      tags$li("Rincian jenis kelamin Bukan Angkatan Kerja dan Pengangguran tidak tersedia."),
      tags$li("Tidak ada pengujian otomatis tingkat aplikasi; kualitas dijaga lewat validasi data/fungsi dan QA manual."))
  ),
  p(class = "sumber", "Kode, data terolah, dan README: ", tags$a(href = URL_REPO, target = "_blank", URL_REPO))
)

# ==== rangka ====
ui <- bslib::page_navbar(
  id = "navbar",
  title = "Potret Pekerja Informal 2025 di Indonesia",
  theme = tema,
  lang = "id",
  fillable = FALSE,
  collapsible = TRUE,
  header = tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML(CSS_APP)),
    tags$script(HTML(JS_HIERARKI)),
    tags$script(HTML(JS_STEPPER)),
    tags$script(HTML(JS_ENCODING))
  ),
  bslib::nav_spacer(),
  ui_beranda, ui_bab1, ui_bab2, ui_bab3, ui_metadata
)