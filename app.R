library(shiny)
library(shinydashboard)
library(shinythemes)
library(dashboardthemes)
library(DT)
library(ggplot2)
library(dplyr)
library(reshape2)
library(corrplot)
library(caret)
library(randomForest)
library(plotly)
library(rsconnect)
library(echarts4r)
library(tidyr)
library(scales)
library(readr)
library(writexl)
library(slickR)
library(highcharter)
library(readxl)
library(shinyjs)
library(DT)
library(shinyBS)
library(leaflet)
library(dplyr)
library(sf)

#getwd()
#setwd("C:\\Users\\MAWAR JANNAH G\\Downloads\\OJK_DASHBOARD_UPDATE")

data_kontak <- read_excel("kontak_ojk_daerah.xlsx")  # Ganti dengan path file kamu


# Define UI
ui <- navbarPage(
  title = div(
    style = "display: flex; align-items: center;",
    tags$img(src = "https://i.ibb.co.com/S7Jjt1bc/ojk-logo.png", height = "150px", style = "width: 100%; margin-right: 10px; margin-top: -45px; margin-left:30px"),
    ""
  ),
  id = "navbar",
  fluid = TRUE,
  
  tags$head(
    tags$style(
      HTML("
      @import url('https://fonts.googleapis.com/css2?family=Poppins:wght@400;600&display=swap');
      
      /* Mengatur tinggi navbar */
      .navbar {
        min-height: 90px !important; 
      }
      
      
      /* General Body Styling */
      body {
        font-family: 'Poppins', sans-serif; 
        background-color: #f9f9f9;
      }

      /* Navbar Styling */
      .navbar {
        position: sticky;
        top: 0;
        z-index: 1000;
        align-items: center; /* Semua elemen sejajar vertikal */
        justify-content: flex-start; /* Semua elemen berada di kiri */
        padding: 0px; /* Tambahkan padding horizontal */
        box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); /* Shadow untuk navbar */
        background-color: #ffffff; /* Warna latar navbar */
      }
      
      .navbar-nav {
        margin-top: 25px;
        margin-left: 0px; /* Geser menu ke kanan */
        list-style: none; /* Hilangkan bullet point */
        padding-left: 0; /* Hilangkan padding default */
      }
      .navbar-nav > li > a {
        color: #030303 !important; /* Warna teks menu */
        font-weight: bold; /* Bold */
        font-size: 17px; /* ukuran font menu */
        padding: 10px 15px; /* Tambahkan padding menu */
        border-radius: 10px; /* Bentuk lonjong */
        transition: all 0.3s ease-in-out; /* Animasi smooth */
        text-decoration: none; /* Hilangkan garis bawah */
      }
      .navbar-nav > li > a:hover {
        background-color: #FFE4E1 !important; /* Background saat hover */
        color: #030303 !important; /* Warna teks tetap */
      }
      .navbar-nav > li.active > a {
        background-color: #FF4500 !important; /* Background merah untuk menu aktif */
        color: #ffffff !important; /* Teks putih */
        font-weight: bold !important; /* Bold teks */
        border-radius: 10px; /* Bentuk lonjong */
      }
      .navbar-nav > li {
        margin-right: 40px; /* Menambah jarak antar menu */
      }
      
      /* Subset Panel*/
      .nav-tabs > li > a {
        font-size: 16px !important; /* Ubah angka ini sesuai keinginan */
        font-weight: bold; /* (Opsional) Biar lebih tebal */
        color: #030303 !important; /* Warna teks */
      }
      
      /* Card Styling */
      .card {
        margin-top: -250px !important; /* Atur agar lebih dekat dengan carousel */
        border-radius: 15px; 
        border: 1px solid #d8dee9; 
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
        margin-bottom: 30px;
        font-size: 18px;
        display: flex;
        flex-direction: column;
        height: 100%; /* Membuat card memiliki tinggi penuh */
        min-height: 340px; /* Sesuaikan dengan tinggi maksimal kartu terpanjang */
      }
      .card-header {
        background-color: #EE6363; 
        color: white; 
        font-weight: bold; 
        border-radius: 15px 15px 0 0;
        padding: 15px;
      }
      .card-body {
        margin-top: -10px;
        padding: 20px;
        text-align: justify;
        flex-grow: 1; /* Memastikan body card mengisi ruang yang tersedia */
      }
      
      
      /* Pastikan gambar di carousel tetap dalam rasio 16:9 */
      .slick-slide img {
        width: 100% !important; 
        max-height: 1000vh !important; /* Batasi tinggi maksimum */
        object-fit: cover !important; /* Pastikan gambar tidak terdistorsi */
      }

      #image_carousel {
        width: 100vw !important;
        max-width: 1200px;
        height: calc(100vw * 9 / 16) !important; /* Rasio 16:9 */
        margin: 0;
        padding: 0;
      }
      
      /* Hilangkan margin dan padding pada body */
      body {
        margin: 0;
        padding: 0;
        overflow-x: hidden; /* Mencegah scroll horizontal */
      }
      
      
      /* Buat panah lebih besar */
      .slick-prev, .slick-next {
        font-size: 50px !important;  /* Ukuran panah */
        width: 60px !important;
        height: 60px !important;
        z-index: 1000;  /* Supaya panah ada di atas */
        opacity: 1 !important;  /* Pastikan panah selalu terlihat */
        color: white !important; /* Warna panah */
      }

      /* Letakkan panah di atas gambar */
      .slick-prev {
        left: 20px !important; /* Geser panah kiri */
        z-index: 10; /* Pastikan panah tetap di atas gambar */
      }

      .slick-next {
        right: 20px !important; /* Geser panah kanan */
        z-index: 10; /* Pastikan panah tetap di atas gambar */
      }

      /* Buat panah berada di tengah gambar */
      .slick-prev, .slick-next {
        position: absolute;
        top: 50%;
        transform: translateY(-50%);
      }
      
      /* Hilangkan teks Previous & Next tanpa menghapus tombol */
      .slick-prev, .slick-next {
        color: transparent !important; /* Sembunyikan teks */
      }
      
      /* Tambahkan background agar panah terlihat */
      .slick-prev:before, .slick-next:before {
        background-color: rgba(0, 0, 0, 0.5); /* Transparan hitam */
        color: white !important; /* Pastikan panah tetap terlihat */
        padding: 15px;
        border-radius: 50%;
      }

      /* Efek hover saat cursor di atas panah */
      .slick-prev:hover:before, .slick-next:hover:before {
        background-color: rgba(255, 255, 255, 0.8); /* Putih */
        color: black !important;
        
      .small-box {
        border-radius: 10px;
        height: 90px; /* Sesuaikan tinggi */
        padding: 10px 15px; /* Sesuaikan padding */
        display: flex;
        flex-direction: column;
        justify-content: flex-end; /* Teks turun ke bawah */
        position: relative;
        overflow: hidden;
    }
    
    .small-box .inner {
        position: relative; /* Posisi relatif untuk teks */
        z-index: 2; /* Pastikan teks di atas ikon */
        text-align: left; /* Sesuaikan teks */
        padding-bottom: 10px; /* Kurangi padding bawah */
    }
    
    .small-box .inner h3 {
        font-size: 30px; /* Ukuran angka besar */
        margin: 0 0 0 0; /* Tambahkan margin atas untuk menurunkan angka */
        font-weight: bold;
        z-index: 3;
    }
    
    .small-box .inner p {
        font-size: 18px; /* Ukuran subtitle */
        margin: 0; /* Hilangkan margin default */
        z-index: 2;
    }
    
    .small-box .icon-large {
        font-size: 70px; /* Ukuran ikon lebih besar */
        position: absolute; /* Posisi absolut untuk bebas mengatur lokasi */
        top: 5px; /* Geser ikon ke atas */
        right: 10px; /* Geser ikon ke kanan */
        color: rgba(0, 0, 0, 0.1); /* Warna hitam transparan 50% */
        z-index: 1; /* Ikon berada di bawah teks */
    }

          
      .small-box.bg-red { background-color: #dd4b39 !important; color: #fff !important; }
      .small-box.bg-green { background-color: #00a65a !important; color: #fff !important; }
      .small-box.bg-yellow { background-color: #f39c12 !important; color: #fff !important; }
      .small-box.bg-blue { background-color: #0073b7 !important; color: #fff !important; }
      .small-box.bg-aqua { background-color: #00c0ef !important; color: #fff !important; }
      .small-box.bg-purple { background-color: #605ca8 !important; color: #fff !important; }
      .small-box.bg-light-blue { background-color: #3c8dbc !important; color: #fff !important; }
      
        /* Untuk layar kecil seperti HP */
    @media (max-width: 768px) {
      .container {
        flex-direction: column; /* Susun elemen ke bawah */
      }
    }
    
    /* Untuk layar besar seperti PC */
    @media (min-width: 1024px) {
      .container {
        flex-direction: row; /* Susun elemen ke samping */
      }
    }

        
      

    "))
  ),
  collapsible = TRUE, # Navbar collapsible untuk tampilan mobile
  
  # Home Page
  tabPanel("Tentang OJK",
           fluidPage(
             div(
               # Gambar (carousel)
               slickROutput("image_carousel", width = "100%", height = "auto"),
               
             ),
             
             # Row pertama (3 card)
             fluidRow(
               column(4, 
                      div(class = "card",
                          div(class = "card-header", "Visi OJK"),
                          div(class = "card-body",
                              p(HTML("Visi Otoritas Jasa Keuangan (OJK) adalah menjadi <b>lembaga pengawas industri jasa keuangan</b> yang terpercaya, melindungi kepentingan konsumen dan masyarakat, dan mampu mewujudkan industri jasa keuangan menjadi pilar perekonomian nasional yang berdaya saing global serta dapat memajukan kesejahteraan umum.")),
                          )
                      )
               ),
               
               column(4, 
                      div(class = "card",
                          div(class = "card-header", "Misi OJK"),
                          div(class = "card-body",
                              p("Misi Otoritas Jasa Keuangan (OJK) adalah:"),
                              tags$ol(
                                tags$li(HTML("Mewujudkan terselenggaranya seluruh kegiatan di dalam sektor jasa keuangan secara <b>teratur, adil, transparan, dan akuntabel;</b>")),
                                tags$li(HTML("Mewujudkan sistem keuangan yang tumbuh secara <b>berkelanjutan dan stabil;</b>")),
                                tags$li(HTML("Melindungi kepentingan konsumen dan masyarakat."))
                              )
                          )
                      )
               ),
               
               column(4, 
                      div(class = "card",
                          div(class = "card-header", "Tujuan OJK"),
                          div(class = "card-body",
                              p("Otoritas Jasa Keuangan (OJK) dibentuk dengan tujuan agar keseluruhan kegiatan di dalam sektor jasa keuangan:"),
                              tags$ol(
                                tags$li(HTML("Terselenggara secara teratur, adil, transparan, dan akuntabel;")),
                                tags$li(HTML("Mampu mewujudkan sistem keuangan yang tumbuh secara berkelanjutan dan stabil; dan")),
                                tags$li(HTML("Mampu melindungi kepentingan konsumen dan masyarakat.")),
                              )
                          )
                      )
               )
             ),
             
             # Row kedua
             fluidRow(
               style = "margin-top: 250px;",  # Tambahkan jarak agar tidak bertumpuk
               column(12, 
                      div(class = "card",
                          div(class = "card-header", "Struktur Organisasi"),
                          div(class = "card-body",
                              imageOutput("organisasi", inline = TRUE, width = "20px")
                          )
                      )
               )
             ),
             
             # Row ketiga
             fluidRow(
               column(12, 
                      tags$p(tags$i(tags$b("For more information")),
                             style = "margin-top: 30px; margin-bottom: 0px; text-align: center;font-size: 40px"),  # Teks bold "For more info"
                      tags$p("access the link below.",
                             style = "margin-top: 0px; text-align: center;font-size: 25px",),  # Teks biasa
                      tags$a(href = "https://lmsku.ojk.go.id/", 
                             "Learning Mangement System (LMS) OJK", 
                             class = "btn btn-primary",
                             style = "padding: 10px 20px; font-size: 20px; display: block; margin: 10px auto; background-color: #CD8500; color: white; border: none; width: 40%;"),
                      tags$a(href = "https://www.ojk.go.id/id/Default.aspx", 
                             "Portal OJK", 
                             class = "btn btn-primary",
                             style = "padding: 10px 20px; font-size: 20px; display: block; margin: 10px auto 50px auto; background-color: #CD8500; color: white; border: none; width: 40%;")
               )
             )
             
             
             
           )
  ),
  
  # About Page
  tabPanel("Lantai 1",
           fluidPage(
             tabsetPanel(
               # Tab pertama: SLIK
               tabPanel("SLIK",
                        fluidPage(
                          br(),
                          
                          # Card 1 = SLIK
                          fluidRow(
                            style = "margin-top: 250px;",
                            column(12, 
                                   div(class = "card",
                                       div(class = "card-header", "Sistem Layanan Informasi Keuangan"),
                                       div(
                                         class = "card-body",
                                         h2(style = "text-align: left;font-weight: bold;", class = "card-title", "Apa itu SLIK?"),
                                         p(HTML("<b>Sistem Layanan Informasi Keuangan (SLIK)</b> adalah platform yang dikelola oleh Otoritas Jasa Keuangan (OJK) untuk mendukung pengawasan dan memberikan layanan informasi di sektor keuangan. SLIK berperan penting dalam meningkatkan transparansi dan disiplin industri keuangan dengan menyediakan data yang akurat mengenai riwayat kredit dan kualitas debitur.")),
                                         tags$head(
                                           tags$style(HTML("
      .icon-container {
        display: flex;
        justify-content: space-around;
        text-align: center;
        margin-top: 20px;
      }
      .icon-item {
        display: flex;
        flex-direction: column;
        align-items: center;
        text-align: center;
        color: #dc3545;
      }
      .circle {
        width: 80px;
        height: 80px;
        border-radius: 50%;
        background-color: #f8f9fa;
        display: flex;
        align-items: center;
        justify-content: center;
        font-size: 30px;
        box-shadow: 2px 2px 10px rgba(0,0,0,0.1);
      }
      .text {
        margin-top: 10px;
        font-weight: bold;
        text-align: center;
        max-width: 100px;
        color: #000000;
      }
    "))
                                         ),
                                         
                                         p(HTML("Melalui SLIK, lembaga keuangan dapat lebih mudah menilai risiko kredit sebelum memberikan pinjaman, sementara masyarakat dan pelaku usaha bisa mengakses informasi terkait status kredit mereka. Selain itu, sistem ini terdapat beberapa kegunaan, diantaranya:")),
                                         
                                         div(class = "icon-container",
                                             div(class = "icon-item",
                                                 div(class = "circle", HTML("<i class='fa fa-money-bill-wave'></i>")),
                                                 div(class = "text", "Penyediaan Dana")
                                             ),
                                             div(class = "icon-item",
                                                 div(class = "circle", HTML("<i class='fa fa-chart-line'></i>")),
                                                 div(class = "text", "Manajemen Risiko")
                                             ),
                                             div(class = "icon-item",
                                                 div(class = "circle", HTML("<i class='fa fa-user-check'></i>")),
                                                 div(class = "text", "Kualitas Debitur")
                                             ),
                                             div(class = "icon-item",
                                                 div(class = "circle", HTML("<i class='fa fa-users'></i>")),
                                                 div(class = "text", "SDM Pelapor SLIK")
                                             ),
                                             div(class = "icon-item",
                                                 div(class = "circle", HTML("<i class='fa fa-handshake'></i>")),
                                                 div(class = "text", "Kerja Sama")
                                             ),
                                             div(class = "icon-item",
                                                 div(class = "circle", HTML("<i class='fa fa-balance-scale'></i>")),
                                                 div(class = "text", "Disiplin Keuangan")
                                             )
                                         ),
                                         h2(style = "text-align: left;font-weight: bold;", class = "card-title", "Bagaimana Mendapatkan SLIK?"),
                                         p("Debitur dapat meminta Informasi Debitur atas nama Debitur yang bersangkutan kepada OJK atau kepada Pelapor SLIK yang memberikan Fasilitas Penyediaan Dana kepada Debitur yang bersangkutan. Permintaan Informasi Debitur kepada OJK dapat dilakukan secara:"),
                                         
                                         tags$head(
                                           tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.2/css/all.min.css"),
                                           tags$style(HTML("
    .info-container {
      display: flex;
      justify-content: center;
      gap: 20px;
    }
    .info-box {
      width: 45%;
      text-align: center;
      border: 2px solid #ddd;
      border-radius: 10px;
      padding: 20px;
      background-color: #f9f9f9;
    }
    .info-icon {
      font-size: 30px;
      color: #dc3545;
      margin-bottom: 10px;
    }
    .info-text {
      font-size: 16px;
      font-weight: bold;
      margin-bottom: 10px;
    }
    .info-content {
      font-size: 14px;
      color: #333;
      line-height: 1.5;
    }
    .info-box a {
      color: #007bff;
      text-decoration: none;
      font-weight: bold;
    }
    .info-box a:hover {
      text-decoration: underline;
    }
  "))
                                         ),
                                         
                                         div(class = "info-container",
                                             div(class = "info-box",
                                                 tags$i(class = "fa fa-building info-icon"),  # Icon Offline
                                                 div(class = "info-text", "Luring / Offline / Walk-In"),
                                                 div(class = "info-content", 
                                                     "Pemohon SLIK harus datang langsung ke kantor OJK setempat dengan membawa dokumen yang diperlukan. 
            Proses ini memungkinkan verifikasi langsung oleh petugas dan memastikan keabsahan data yang diberikan. 
            Untuk informasi lebih lanjut mengenai dokumen yang harus disiapkan, silakan cek link berikut:",
                                                     br(), br(),
                                                     HTML("<a href='https://ojk.go.id/id/kanal/perbankan/Documents/Pages/Sistem-Layanan-Informasi-Keuangan-SLIK/Informasi%20Dokumen%20Persyaratan.pdf' target='_blank'>📄 Dokumen yang Diperlukan</a>")
                                                 )
                                             ),
                                             
                                             div(class = "info-box",
                                                 tags$i(class = "fa fa-globe info-icon"),  # Icon Online
                                                 div(class = "info-text", "Daring (Online)"),
                                                 div(class = "info-content", 
                                                     "Pemohon SLIK dapat mengajukan permohonan secara daring melalui aplikasi iDebku OJK. 
            Proses ini lebih praktis karena dapat dilakukan dari mana saja, selama koneksi internet tersedia. 
            Permohonan dapat diajukan melalui laman berikut:",
                                                     br(), br(),
                                                     HTML("<a href='https://idebku.ojk.go.id' target='_blank'>🌐 Aplikasi iDebku OJK</a>"),
                                                     br(), br(),
                                                     "Untuk tata cara pengajuan secara online, silakan baca panduan berikut:",
                                                     br(), br(),
                                                     HTML("<a href='https://ojk.go.id/id/kanal/perbankan/Documents/Pages/Sistem-Layanan-Informasi-Keuangan-SLIK/Tata%20Cara%20Permohonan%20Informasi%20Debitur%20SLIK%20Online%20pada%20Aplikasi%20iDebku.pdf' target='_blank'>📘 Panduan secara Online</a>")
                                                 )
                                             )
                                         ),
                                         
                                         br(),  # Tambahkan spasi agar pemisahan terlihat jelas
                                         
                                         h2(style = "text-align: left;font-weight: bold;", class = "card-title", "Bagaimana Membaca SLIK?"),
                                         p(HTML("SLIK digunakan untuk melihat riwayat kredit seseorang atau badan usaha. Data ini biasanya diperiksa oleh bank atau lembaga keuangan sebelum memberikan pinjaman. Berikut informasi yang terdapat dalam laporan SLIK:")),
                                         
                                         tags$head(
                                           tags$style(HTML("
    .info-container {
      display: flex; 
      flex-wrap: wrap; 
      gap: 20px; 
      justify-content: center;
    }
    .info-box {
      width: 30%; 
      text-align: center;
      border: 1px solid #ddd; /* Border tipis */
      border-radius: 8px;
      padding: 15px;
      background-color: #f9f9f9;
      margin: 10px; /* Tambahkan margin antar box */
    }
    .info-icon {
      font-size: 30px;
      color: #dc3545;
      margin-bottom: 10px;
    }
    .info-title {
      font-size: 16px;
      font-weight: bold;
      margin-bottom: 5px;
    }
    .info-content {
      font-size: 14px;
      color: #333;
      line-height: 1.5;
    }
  "))
                                         ),
                                         
                                         div(class = "info-container",
                                             div(class = "info-box", 
                                                 HTML("<i class='fa fa-id-card info-icon'></i>"),
                                                 div(class = "info-title", "Identitas Debitur"),
                                                 div(class = "info-content", "Berisi informasi nama, nomor identitas (KTP/SIUP), alamat, dan NPWP.")
                                             ),
                                             div(class = "info-box", 
                                                 HTML("<i class='fa fa-credit-card info-icon'></i>"),
                                                 div(class = "info-title", "Informasi Fasilitas Kredit"),
                                                 div(class = "info-content", "Menampilkan semua pinjaman yang pernah atau sedang dimiliki, termasuk jumlah dan jenisnya.")
                                             ),
                                             div(class = "info-box", 
                                                 HTML("<i class='fa fa-clock info-icon'></i>"),
                                                 div(class = "info-title", "Riwayat Pembayaran Kredit"),
                                                 div(class = "info-content", "Menampilkan catatan pembayaran cicilan dan skor kolektibilitas.")
                                             ),
                                             div(class = "info-box", 
                                                 HTML("<i class='fa fa-ban info-icon'></i>"),
                                                 div(class = "info-title", "Kredit yang Ditolak"),
                                                 div(class = "info-content", "Menampilkan daftar pengajuan kredit yang pernah ditolak oleh bank atau dibatalkan.")
                                             ),
                                             div(class = "info-box", 
                                                 HTML("<i class='fa fa-home info-icon'></i>"),
                                                 div(class = "info-title", "Status Agunan (Jaminan)"),
                                                 div(class = "info-content", "Jika kredit menggunakan jaminan, informasi jaminan akan dicantumkan.")
                                             ),
                                             div(class = "info-box", 
                                                 HTML("<i class='fa fa-chart-line info-icon'></i>"),
                                                 div(class = "info-title", "Status Kolektibilitas"),
                                                 div(class = "info-content", "Menunjukkan kategori kredit: Lancar, Dalam Perhatian Khusus, Kurang Lancar, Diragukan, atau Macet.")
                                             )  
                                         ),
                                         
                                         br(),
                                         p(HTML("<b>Berikut panduan untuk membaca SLIK lebih lengkap:</b> 
<a href='https://ojk.go.id/id/kanal/perbankan/Documents/Pages/Sistem-Layanan-Informasi-Keuangan-SLIK/Tata%20Cara%20Membaca%20Informasi%20Debitur%20SLIK.pdf' 
target='_blank' style='color: blue; text-decoration: underline;'>Baca Panduan</a>")),
                                         
                                         br(),  # Spasi biar terpisah dari bagian sebelumnya
                                         
                                         # Styling
                                         tags$head(tags$style(HTML("
    .faq-box {
      background: #B22222; 
      color: white; 
      padding: 10px; 
      border-radius: 5px; 
      margin: 5px 0; 
      font-weight: bold; 
      cursor: pointer;
    }
    .faq-answer {
      display: none; 
      padding: 10px; 
      border: 1px solid #ddd; 
      border-radius: 5px; 
      background: #f9f9f9;
      margin-bottom: 10px;
    }
    .input-box {
      margin-top: 20px; 
      padding: 15px; 
      border: 1px solid #ddd; 
      border-radius: 5px; 
      background: #f9f9f9;
    }
  "))),
                                       )
                                   )
                            )
                          ),
                          
                          # Card 2 = FAQ
                          div(class = "card",
                              div(class = "card-header", "Frequently Asked Question"),
                              div(class = "card-body",
                                  fluidRow(
                                    column(6, 
                                           h2("Untuk Kustomer OJK", style = "font-weight: bold;"),
                                           textAreaInput("new_question", "Masukkan Pertanyaan:", width = "90%", height = "130px"),
                                           actionButton("submit_question", "Kirim Pertanyaan", 
                                                        style = "background-color: #28a745; color: white; 
                                       border: 2px solid #28a745; 
                                       box-shadow: 2px 2px 5px rgba(40, 167, 69, 0.5);")
                                    ),
                                    column(6, 
                                           h2("Untuk Pegawai OJK", style = "font-weight: bold;"),
                                           fluidRow(
                                             column(6, passwordInput("admin_key", "Masukkan Kata Kunci:")), 
                                             column(6, selectInput("select_question", "Pilih Pertanyaan:", choices = NULL))
                                           ),
                                           textAreaInput("answer_input", "Jawaban:", height = "50px", width = "100%"),
                                           fluidRow(
                                             column(6,
                                                    actionButton("submit_answer", "Kirim Jawaban", 
                                                                 style = "background-color: #28a745; color: white; 
                                                border: 2px solid #28a745; 
                                                box-shadow: 2px 2px 5px rgba(40, 167, 69, 0.5);")
                                             ),
                                             column(6,
                                                    actionButton("delete_question", "Hapus Pertanyaan Terpilih", 
                                                                 style = "background-color: #dc3545; color: white; 
                                                border: 2px solid #dc3545; 
                                                box-shadow: 2px 2px 5px rgba(220, 53, 69, 0.5);")
                                             )
                                           )
                                    )
                                  )
                              )
                          ),
                          
                          # Card 3 = Database
                          fluidRow(
                            style = "margin-top: 270px;",  # Beri jarak antar section
                            column(12, 
                                   div(class = "card",
                                       div(class = "card-header", 
                                           style = "background-color: #ee6363; color: white; font-weight: bold;",
                                           "Database Pertanyaan & Jawaban"),
                                       div(class = "card-body",
                                           h2(style = "text-align: left; font-weight: bold;", class = "card-title", "Daftar FAQ"),
                                           p("Berikut adalah daftar pertanyaan dan jawaban yang telah disimpan dalam sistem."),
                                           DTOutput("faq_table"),
                                           br(),
                                           downloadButton("download_faq", "Download Excel", 
                                                          style = "background-color: #007bff; color: white; width: 100%;")
                                       )
                                   )
                            )
                          )
                        )
               ),
               
               # Tab kedua: APPK
               tabPanel("APPK",
                        fluidPage(
                          br(),
                          
                          # Card 1 = APPK
                          fluidRow(
                            style = "margin-top: 250px;",
                            column(12, 
                                   div(class = "card",
                                       div(class = "card-header", "Aplikasi Portal Perlindungan Konsumen"),
                                       div(
                                         class = "card-body",
                                         h2(style = "text-align: left;font-weight: bold;", class = "card-title", "Apa itu APPK?"),
                                         p(HTML("<b>Aplikasi Portal Perlindungan Konsumen (APPK)</b> adalah sistem berbasis website yang dapat diakses oleh seluruh pihak sektor jasa keuangan secara daring untuk bertanya, melapor, dan mengadu permasalahan di sektor jasa keuangan. <a href='https://kontak157.ojk.go.id/appkpublicportal/Home' target='_blank'>Klik di sini</a> untuk mengakses portal.")),
                                         
                                         # Tambahkan Video Youtube lebih memanjang ke bawah, di tengah, dan dengan border melengkung
                                         div(style = "display: flex; justify-content: center; margin-top: 15px;", 
                                             tags$iframe(src = "https://www.youtube.com/embed/2qpOjSsN1FE", 
                                                         width = "50%", height = "450px", frameborder = "0", 
                                                         allowfullscreen = TRUE, 
                                                         style = "border-radius: 15px; box-shadow: 2px 2px 10px rgba(0,0,0,0.2);")
                                         ),
                                         
                                         br(),
                                         
                                         h2(style = "text-align: left;font-weight: bold;", class = "card-title", "Apa saja layanan yang diberikan?"),
                                         p(HTML("Berikut adalah Layanan yang disediakan oleh <b>Kontak OJK Online 157</b>:")),
                                         
                                         # Layanan dengan Icon Merah, di Tengah, dengan Border Bayangan, dan Judul
                                         fluidRow(
                                           column(4, div(class = "circle-icon", style = "text-align: center; margin-bottom: 20px;",
                                                         div(style = "display: inline-block; background-color: white; padding: 20px; border-radius: 50%; box-shadow: 2px 2px 10px rgba(0,0,0,0.2);",
                                                             tags$i(class = "fas fa-question-circle fa-3x", style = "color: #dc3545;")
                                                         ),
                                                         tags$h3(tags$b("Pertanyaan")),
                                                         p("Gunakan Layanan ini bila Anda ingin menanyakan hal-hal terkait Produk/Layanan di Sektor Jasa Keuangan kepada OJK.")
                                           )),
                                           column(4, div(class = "circle-icon", style = "text-align: center; margin-bottom: 20px;",
                                                         div(style = "display: inline-block; background-color: white; padding: 20px; border-radius: 50%; box-shadow: 2px 2px 10px rgba(0,0,0,0.2);",
                                                             tags$i(class = "fas fa-info-circle fa-3x", style = "color: #dc3545;")
                                                         ),
                                                         tags$h3(tags$b("Penyampaian Informasi")),
                                                         p("Gunakan Layanan ini bila Anda ingin menyampaikan informasi atau Laporan terkait Produk/Layanan di Sektor Jasa Keuangan kepada OJK.")
                                           )),
                                           column(4, div(class = "circle-icon", style = "text-align: center; margin-bottom: 20px;",
                                                         div(style = "display: inline-block; background-color: white; padding: 20px; border-radius: 50%; box-shadow: 2px 2px 10px rgba(0,0,0,0.2);",
                                                             tags$i(class = "fas fa-exclamation-triangle fa-3x", style = "color: #dc3545;")
                                                         ),
                                                         tags$h3(tags$b("Pengaduan")),
                                                         p("Gunakan Layanan ini bila Anda ingin melakukan Pengaduan sebagai Konsumen di Sektor Jasa Keuangan.")
                                           ))
                                         ),
                                         
                                         fluidRow(
                                           column(6, div(class = "circle-icon", style = "text-align: center; margin-bottom: 20px; margin-left: auto; margin-right: auto;",
                                                         div(style = "display: inline-block; background-color: white; padding: 20px; border-radius: 50%; box-shadow: 2px 2px 10px rgba(0,0,0,0.2);",
                                                             tags$i(class = "fas fa-ban fa-3x", style = "color: #dc3545;")
                                                         ),
                                                         tags$h3(tags$b("Pengaduan Aktivitas Keuangan Ilegal")),
                                                         p("Gunakan Layanan ini bila Anda ingin melaporkan Aktivitas Keuangan Ilegal.")
                                           )),
                                           column(6, div(class = "circle-icon", style = "text-align: center; margin-bottom: 20px; margin-left: auto; margin-right: auto;",
                                                         div(style = "display: inline-block; background-color: white; padding: 20px; border-radius: 50%; box-shadow: 2px 2px 10px rgba(0,0,0,0.2);",
                                                             tags$i(class = "fas fa-balance-scale fa-3x", style = "color: #dc3545;")
                                                         ),
                                                         tags$h3(tags$b("Pelaporan TipisJK")),
                                                         p("Gunakan Layanan ini apabila Anda ingin melaporkan Dugaan Tindak Pidana di Sektor Jasa Keuangan.")
                                           ))
                                         ),
                                         br(),
                                         tags$head(
                                           tags$style(HTML("
      .flow-container {
        display: flex;
        align-items: center;
        justify-content: space-between;
        flex-wrap: wrap;
        gap: 10px;
        margin-top: 20px;
      }
      .step-box-container {
        flex: 1;
        text-align: center;
      }
      .step-icon {
        display: inline-block;
        width: 40px;
        height: 40px;
        text-align: center;
        line-height: 40px;
        background: red;
        color: white;
        border-radius: 50%;
        font-weight: bold;
        margin-bottom: 5px;
      }
      .step-box {
        background: white;
        padding: 15px;
        border-radius: 10px;
        box-shadow: 2px 2px 10px rgba(0,0,0,0.1);
        font-weight: bold;
        width: 100%;
        min-height: 70px;
        display: flex;
        align-items: center;
        justify-content: center;
      }
      .arrow {
        font-size: 24px;
        color: red;
        text-align: center;
        flex: 0.1;
      }
    "))
                                         ),
                                         
                                         h2(style = "text-align: left; font-weight: bold;", "Berkas yang Dibutuhkan?"),
                                         p(style = "text-align: left;", "Debitur dapat membawa berkas di bawah ini sebelum melaporkan ke website APPK."),
                                         
                                         tags$head(
                                           tags$style(HTML("
    .info-container {
      display: flex; 
      flex-wrap: wrap; 
      gap: 20px; 
      justify-content: center;
    }
    .info-box {
      width: 30%; 
      text-align: center;
      border: 1px solid #ddd; /* Border tipis */
      border-radius: 8px;
      padding: 15px;
      background-color: #f9f9f9;
      margin: 10px; /* Tambahkan margin antar box */
    }
    .info-icon {
      font-size: 30px;
      color: #dc3545;
      margin-bottom: 10px;
    }
    .info-title {
      font-size: 16px;
      font-weight: bold;
      margin-bottom: 5px;
    }
    .info-content {
      font-size: 14px;
      color: #333;
      line-height: 1.5;
    }
  "))
                                         ),
                                         
                                         div(class = "info-container",
                                             div(class = "info-box", 
                                                 HTML("<i class='fa fa-id-card info-icon'></i>"),
                                                 div(class = "info-title", "Identitas Debitur"),
                                                 div(class = "info-content", "Membawa identitas seperti KTP asli atau elektronik.")
                                             ),
                                             div(class = "info-box", 
                                                 HTML("<i class='fa fa-phone info info-icon'></i>"),
                                                 div(class = "info-title", "Informasi Nomor Handphone"),
                                                 div(class = "info-content", "Pastikan informasi nomor handphone yang diberikan terhubung dengan aplikasi Whatsapp.")
                                             ),
                                             div(class = "info-box", 
                                                 HTML("<i class='fa fa-envelope info-icon'></i>"),
                                                 div(class = "info-title", "Informasi Alamat Email"),
                                                 div(class = "info-content", "Pastikan informasi alamat email yang diberikan aktif dan tidak penuh ruang penyimpanannya.")
                                             ),
                                             div(class = "info-box", 
                                                 HTML("<i class='fa fa-exclamation-triangle info-icon'></i>"),
                                                 div(class = "info-title", "Dokumen Kronologis Permasalahan"),
                                                 div(class = "info-content", "Bisa berupa file pdf yang berisikan kronologis permasalahan yang dialami.")
                                             ),
                                             div(class = "info-box", 
                                                 HTML("<i class='fa fa-file  info-icon'></i>"),
                                                 div(class = "info-title", "Dokumen Pendukung"),
                                                 div(class = "info-content", "Berisikan mengenai file yang mendukung kejadian seperti bukti chat, bukti transfer, dan lainnya.")
                                             ),
                                             div(class = "info-box", 
                                                 HTML("<i class='fa fa-file-alt info-icon'></i>"),
                                                 div(class = "info-title", "Surat Pernyataan"),
                                                 div(class = "info-content", "Surat pernyataan yang bisa didapat pada website APPK OJK sendiri.")
                                             )  
                                         ),
                                         
                                         br(),
                                         
                                         # Styling
                                         tags$head(tags$style(HTML("
    .faq-box {
      background: #B22222; 
      color: white; 
      padding: 10px; 
      border-radius: 5px; 
      margin: 5px 0; 
      font-weight: bold; 
      cursor: pointer;
    }
    .faq-answer {
      display: none; 
      padding: 10px; 
      border: 1px solid #ddd; 
      border-radius: 5px; 
      background: #f9f9f9;
      margin-bottom: 10px;
    }
    .input-box {
      margin-top: 20px; 
      padding: 15px; 
      border: 1px solid #ddd; 
      border-radius: 5px; 
      background: #f9f9f9;
    }
  ")))
                                       )
                                   )
                            )
                          ),
                          
                          
                          # Card 2 = Kontak OJK tiap Daerah
                          fluidRow(
                            style = "margin-top: 250px;",  
                            column(12, 
                                   div(class = "card",
                                       div(class = "card-header", "Kontak OJK tiap Daerah"),
                                       br(),
                                       div(style = "margin-left: 20px; margin-right: 20px;",  # Tambahin margin samping
                                           DTOutput("table")
                                       )
                                   )
                            )
                          )
                        )
               ),
               
               
               # Tab ketiga: IASC
               tabPanel("IASC",
                        fluidPage(
                          br(),
                          
                          # Card 1 : IASC
                          fluidRow(
                            style = "margin-top: 250px",
                            column(12, 
                                   div(class = "card",
                                       div(class = "card-header", "Indonesia Anti-Scam Centre"),
                                       
                                       h2(style = "text-align: left; font-weight: bold; margin-left: 20px;", class = "card-title", "Apa itu IASC?"),
                                       p(style = "margin-left: 20px;", HTML("IASC atau Indonesia Anti Scam Centre merupakan forum kerjasama antara Satuan Tugas Pemberantasan Aktivitas Keuangan Ilegal (Satgas PASTI) dengan pelaku industri perbankan, penyedia jasa pembayaran, e-commerce, dan pihak terkait lainnya, yang bertujuan untuk menindaklanjuti laporan penipuan (scam) di sektor keuangan Indonesia secara cepat, timely, dan berefek-jera sesuai ketentuan yang berlaku.")),
                                       
                                       br(),
                                       
                                       h2(style = "text-align: left; font-weight: bold; margin-left: 20px;", class = "card-title", "Bagaimana cara melaporkan penipuan ke IASC?"),
                                       
                                       p(style = "margin-left: 20px;", HTML("Bank dan penyedia jasa pembayaran terkait yang tergabung pada IASC akan melakukan hal sebagai berikut:<br><br> 
<ul> 
  <li>Melakukan verifikasi untuk memastikan terjadinya penipuan</li> 
  <li>Melakukan penundaan transaksi penipuan (pemblokiran) dengan cepat dan mengupayakan penyelamatan sisa dana korban</li> 
  <li>Melakukan identifikasi pelaku</li> 
  <li>Melakukan koordinasi penindakan hukum dengan Aparat Penegakan Hukum</li> 
</ul>")),
                                       
                                       br(),
                                       
                                       h2(style = "text-align: left; font-weight: bold; margin-left: 20px;", class = "card-title", "Data apa saja yang diperlukan?"),
                                       p(style = "margin-left: 20px;", HTML("Berikut adalah Layanan yang disediakan oleh <b>Kontak OJK Online 157</b>:")),
                                       
                                       # Container untuk daftar data yang diperlukan
                                       div(style = "display: flex; justify-content: space-around; flex-wrap: wrap; margin-top: 20px; padding: 10px;",
                                           
                                           # Data diri
                                           div(style = "text-align: center; margin: 10px;",
                                               div(style = "width: 90px; height: 90px; border-radius: 50%; background-color: #f0f0f0; display: flex; align-items: center; justify-content: center; box-shadow: 3px 3px 10px rgba(0,0,0,0.2);",
                                                   HTML("<i class='fas fa-id-card' style='font-size: 36px; color: #dc3545;'></i>")
                                               ),
                                               p(style = "font-weight: bold; margin-top: 10px;", "Data diri (KTP, SIM)")
                                           ),
                                           
                                           # Bukti kepemilikan rekening
                                           div(style = "text-align: center; margin: 10px;",
                                               div(style = "width: 90px; height: 90px; border-radius: 50%; background-color: #f0f0f0; display: flex; align-items: center; justify-content: center; box-shadow: 3px 3px 10px rgba(0,0,0,0.2);",
                                                   HTML("<i class='fas fa-university' style='font-size: 36px; color: #dc3545;'></i>")
                                               ),
                                               p(style = "font-weight: bold; margin-top: 10px;", "Bukti kepemilikan rekening bank")
                                           ),
                                           
                                           # Kronologis kejadian
                                           div(style = "text-align: center; margin: 10px;",
                                               div(style = "width: 90px; height: 90px; border-radius: 50%; background-color: #f0f0f0; display: flex; align-items: center; justify-content: center; box-shadow: 3px 3px 10px rgba(0,0,0,0.2);",
                                                   HTML("<i class='fas fa-list-alt' style='font-size: 36px; color: #dc3545;'></i>")
                                               ),
                                               p(style = "font-weight: bold; margin-top: 10px;", "Kronologis terjadinya penipuan")
                                           ),
                                           
                                           # Bukti transaksi
                                           div(style = "text-align: center; margin: 10px;",
                                               div(style = "width: 90px; height: 90px; border-radius: 50%; background-color: #f0f0f0; display: flex; align-items: center; justify-content: center; box-shadow: 3px 3px 10px rgba(0,0,0,0.2);",
                                                   HTML("<i class='fas fa-receipt' style='font-size: 36px; color: #dc3545;'></i>")
                                               ),
                                               p(style = "font-weight: bold; margin-top: 10px;", "Bukti transaksi (contoh: bukti transfer)")
                                           )
                                       )
                                   )
                            )
                          ),
                          fluidRow(
                            style = "margin-top: 250px; margin-bottom: 10px",
                            column(12, 
                                   div(class = "card",
                                       div(class = "card-header", "Website IASC"),
                                       
                                       h2(style = "text-align: left; font-weight: bold; margin-left: 20px;", 
                                          class = "card-title", "Website untuk mengunjungi laman IASC"),
                                       
                                       p(style = "margin-left: 20px;",
                                         HTML("Untuk informasi lebih lanjut atau melaporkan penipuan, silakan kunjungi 
                     <b>website IASC</b> melalui tombol di bawah ini atau datang langsung ke 
                     <b>cabang OJK terdekat</b>.")),
                                       
                                       div(style = "text-align: center; margin-top: 20px;",
                                           actionButton("go_to_iasc", "Kunjungi Website IASC", 
                                                        style = "background-color: green; color: white; font-size: 16px; 
                                       font-weight: bold; padding: 10px 20px; border-radius: 5px;",
                                                        onclick = "window.open('https://iasc.ojk.go.id/', '_blank')"))
                                   )
                            )
                          )
                          
                        )
               )
             )
           )
  ),
  
  # About Page
  tabPanel("Lantai 6",
           fluidPage(
             tabsetPanel(
               # Tab pertama: Pengarsipan
               tabPanel("Pengarsipan",
                        fluidPage(
                          br(),  # Tambahkan ini untuk memberi jarak
                          
                          # Card Pertama
                          fluidRow(
                            style = "margin-top: 250px;",  # Tambahkan jarak agar tidak bertumpuk
                            column(12, 
                                   div(class = "card",
                                       div(class = "card-header", "Arsip Data Pengawasan Perbankan"),
                                       div(
                                         class = "card-body",
                                         h2(style = "text-align: left;font-weight: bold;", class = "card-title", "Perbedaan BPR dan Bank Umum"),
                                         p(HTML("BPR (Bank Perkreditan Rakyat) dan Bank Umum memiliki perbedaan mendasar dalam fungsi dan cakupannya. <b>Bank Umum</b> memiliki izin untuk menghimpun dana dari masyarakat <b>dalam bentuk simpanan seperti giro, tabungan, dan deposito berjangka serta memberikan kredit dalam berbagai bentuk.</b> Sementara itu, <b>BPR</b> hanya dapat <b>menghimpun dana dalam bentuk tabungan dan deposito</b>, dan lebih fokus pada penghimpunan dana serta penyaluran kredit kepada usaha kecil dan menengah serta menyalurkan kredit dengan cakupan lebih terbatas.")),
                                         p(HTML("Terdapat dua jenis usaha BPR, yaitu:")),
                                         tags$ol(
                                           tags$li(HTML("<b>BPR (Bank Perkreditan Rakyat) :</b> Beroperasi dengan sistem konvensional dan terdapat bunga dalam penghimpunan dana dan penyaluran kredit.")),
                                           tags$li(HTML("<b>BPRS (Bank Perkreditan Rakyat Syariah) :</b> Beroperasi berdasarkan prinsip syariah dengan akad-akad seperti mudharabah, musyarakah, dan murabahah, tanpa menerapkan sistem bunga."))
                                         ),
                                         
                                         h2(style = "text-align: left;font-weight: bold;", class = "card-title", "Tujuan Arsip Dokumen"),
                                         p("Arsip dokumen dalam pengawasan perbankan memiliki peran penting dalam menjaga kelengkapan dan validitas data keuangan serta kepatuhan regulasi. Tujuan utama pengarsipan dokumen ini adalah:"),
                                         tags$ul(
                                           tags$li(HTML("<b>Mengantisipasi audit eksternal dan internal :</b> Perusahaan harus memastikan setiap dokumen yang kemungkinan akan diminta tersedia untuk pemeriksaan.")),
                                           tags$li(HTML("<b>Mempermudah akses pengawas perbankan :</b> Jika terjadi permasalahan di kemudian hari terkait BPRS tertentu, dokumen yang diperlukan dapat dengan mudah ditemukan.")),
                                           tags$li(HTML("<b>Mendukung transparansi dan akuntabilitas: :</b> Penyimpanan dokumen yang sistematis memastikan bahwa setiap informasi dapat ditelusuri kembali sesuai kebutuhan regulasi.")),
                                           tags$li(HTML("<b>Mengelola dokumen secara efisien :</b> Arsip dipisahkan berdasarkan kategori <b>aktif, inaktif, permanen, dan non-permanen</b> untuk kemudahan akses dan efisiensi penyimpanan."))
                                         ),
                                         
                                         h2(style = "text-align: left;font-weight: bold;", class = "card-title", "Dokumen Aktif dan Inaktif"),
                                         tags$ul(
                                           tags$li(HTML("<b>Dokumen Aktif :</b> Dokumen yang masih sering digunakan dalam aktivitas operasional dan pengawasan. Per 2025, dokumen aktif dikategorikan sebagai dokumen dari <b>tahun 2022 ke atas.</b> Dokumen ini disimpan dalam lemari untuk mempermudah akses.")),
                                           tags$li(HTML("<b>Dokumen Inaktif :</b> Dokumen yang frekuensi penggunaannya sudah menurun tetapi masih perlu disimpan untuk referensi atau kepatuhan regulasi. Dokumen inaktif dikategorikan sebagai dokumen dari <b>tahun 2021 ke bawah</b> dan dimasukkan kedalam box untuk selanjutnya diproses untuk pemusnahan."))
                                         ),
                                         
                                         h2(style = "text-align: left;font-weight: bold;", class = "card-title", "Dokumen Permanen dan Musnah"),
                                         tags$ul(
                                           tags$li(HTML("<b>Dokumen Permanen :</b> dokumen yang harus disimpan untuk jangka waktu <b>tidak terbatas</b> karena memiliki nilai hukum, administratif, atau sebagai bagian dari rekam jejak penting dalam pengawasan perbankan. Dokumen ini tidak boleh dimusnahkan karena dapat dibutuhkan kapan saja untuk kepentingan audit, analisis historis, ataupun pengambilan keputusan strategis.")),
                                           p("Adapun macam macam jenis dokumen permanen yakni sebagai berikut"),
                                           tags$ol(
                                             tags$li(HTML("Laporan Hasil Pemeriksaan (LHP)")),
                                             tags$li(HTML("Audit Work Plan (AWP)")),
                                             tags$li(HTML("Tingkat Kesehatan Bank (TKS)")),
                                             tags$li(HTML("Laporan Akhir Tahun (LAT)")),
                                             tags$li(HTML("Laporan Akhir Bulan (LAB)")),
                                             tags$li(HTML("Know Your Bank (KYB)")),
                                             tags$li(HTML("Pengawasan Aktivitas Manajemen (PAM)")),
                                             tags$li(HTML("Forum Panel"))
                                           ),
                                           
                                           
                                           tags$li(HTML("<b>Dokumen Musnah :</b> dokumen yang <b>tidak lagi memiliki nilai administratif, hukum, atau operasional</b> setelah mencapai periode tertentu, sehingga dapat dimusnahkan untuk menghemat ruang penyimpanan. Umumnya, dokumen musnah adalah dokumen yang telah melewati masa inaktif.")),
                                           p("Adapun macam macam jenis dokumen musnah yakni sebagai berikut"),
                                           tags$ol(
                                             tags$li(HTML("Izin Operasional")),
                                             tags$li(HTML("Susunan Pengurus")),
                                             tags$li(HTML("Perubahan Pengurus")),
                                             tags$li(HTML("Hasil Fit & Proper Test (FPT)")),
                                             tags$li(HTML("Laporan Rutin"))
                                           )
                                         ),
                                         
                                         h2(style = "text-align: left;font-weight: bold;", class = "card-title", "BPRS yang Diawasi oleh OJK Regional 4"),
                                         tags$ol(
                                           tags$li(HTML("BPRS Sumber Artha Waru Agung")),
                                           tags$li(HTML("BPRS Bhakti Sejahtera")),
                                           tags$li(HTML("BPRS Karya Mugi Sentosa")),
                                           tags$li(HTML("BPRS Lantabur Tebuireng")),
                                           tags$li(HTML("BPRS Mandiri Mitra Sukses")),
                                           tags$li(HTML("BPRS Sarana Prima Mandiri")),
                                           tags$li(HTML("BPRS Situbondo")),
                                           tags$li(HTML("BPR Sriekaya")),
                                           tags$li(HTML("BPR Satya Bhana Artha")),
                                           tags$li(HTML("BPRS Unawi Barokah")),
                                           tags$li(HTML("BPR Central Niaga")),
                                           tags$li(HTML("BPR Guna Yatra")),
                                           tags$li(HTML("BPR Sarana Sukses")),
                                           tags$li(HTML("BPR Djojo Mandiri Raya")),
                                           tags$li(HTML("BPRS Madinah")),
                                           tags$li(HTML("BPRS Annisa Mukti")),
                                           tags$li(HTML("BPRS Bhakti Sumekar")),
                                           tags$li(HTML("BPRS Mojo Artho Kota Mojokerto")),
                                           tags$li(HTML("BPRS Amanah Sejahtera")),
                                           tags$li(HTML("BPRS Bakti Makmur Indah")),
                                           tags$li(HTML("BPRS Bhakti Artha Sejahtera")),
                                           tags$li(HTML("BPR Delta Lamongan")),
                                           tags$li(HTML("BPR Buduran Delta Purnama")),
                                           tags$li(HTML("BPRS Asbisindo DPW Jatim")),
                                           tags$li(HTML("BPR Panji Aronta"))
                                         ),
                                         p(HTML("<i>Dan masih banyak lagi.....</i>")),
                                         
                                         h2(style = "text-align: left;font-weight: bold;", class = "card-title", "Langkah-Langkah Pengarsipan"),
                                         tags$p("Adapun langkah langkah pengarsipan yakni sebagai berikut:"),
                                         
                                         tags$ol(
                                           tags$li(HTML("<b>Pemilahan Dokumen</b> 
            <ul>
              <li>Mengelompokkan dokumen berdasarkan BPRS, TAHUN, dan PENGAWAS.</li>
            </ul>")),
                                           
                                           tags$li(HTML("<b>Pengelompokan Dokumen dalam Map Kuping OJK:</b> 
            <ul>
              <li>Meninjau setiap dokumen berdasarkan perihal surat.</li>
              <li>Mengelompokkan berdasarkan kode perihal yang terdapat dalam <i>'Dataset Perbankan'</i>.</li>
              <li>Menulis label pada map kuping sesuai dengan format berikut.</li>
            </ul>"),
                                                   div(
                                                     style = "align-items: center; gap: 20px; margin-left:30px", 
                                                     div(
                                                       imageOutput("mapkuping", width = "5px", height = "auto")  # Ukuran bisa diatur di sini
                                                     )
                                                   )
                                           ),
                                           
                                           tags$li(HTML("<b>Penyusunan dalam Portapel:</b> 
            <ul>
              <li>Setiap dokumen dalam satu tahun dikategorikan berdasarkan berkas permanen dan berkas musnah.</li>
              <li>Dokumen kemudian dimasukkan ke dalam portapel secara berurutan sesuai dengan kode perihal.</li>
            </ul>")),
                                           
                                           tags$li(HTML("<b>Penyimpanan dalam Box Arsip:</b> 
            <ul>
              <li>Dokumen dari beberapa tahun dapat digabung dalam satu box selama storage masih cukup, satu box berisikan satu BPRS.</li>
            </ul>")),
                                           
                                           tags$li(HTML("<b>Pelabelan Box Arsip:</b> 
            <ul>
              <li>Setiap box diberi label dengan menyantumkan nama BPR/BPRS, macam kode klasifikasi dalam satu box, serta unit pengelola.</li>
            </ul>"))
                                         )
                                         
                                         
                                       )
                                       
                                   )
                            )
                          ),
                          
                          # Card Kedua
                          fluidRow(
                            column(12, 
                                   div(
                                     style = "margin-top: 10px; margin-bottom: 0px; font-size: 18px; border-radius: 15px; border: 1px solid #d8dee9; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);",  
                                     class = "cardpengarsipan",
                                     div(class = "card-header", style = "background-color: #EE6363; color: white; font-weight: bold; border-radius: 15px 15px 0 0; padding: 15px;", "Dataset Pengarsipan Perbankan"),
                                     div(
                                       class = "card-body",
                                       style = "padding: 20px; text-align: justify; flex-grow: 1;",
                                       h2(style = "text-align: left; font-weight: bold;", class = "card-title", "Arsip Perbankan"),
                                       p(HTML("Dataset Arsip Perbankan ini disusun untuk memudahkan rekan-rekan dalam mengklasifikasikan kode sesuai dengan perihalnya. Dataset ini dikumpulkan dari berbagai sumber pengkodean perbankan serta riwayat pengarsipan yang telah terdokumentasi oleh penulis.")),
                                       p(HTML("Jika dokumen atau lampiran yang Anda butuhkan <b>tidak ditemukan dalam dataset ini, silakan menghubungi Mas Toni dan Mbak Shinta Selaku PIC Pengarsipan</b> untuk mendapatkan informasi lebih lanjut. Selain itu, anda juga dapat menambahkan informasi baru yang Anda terima agar data tetap terupdate dan bermanfaat bagi pengguna lain di masa mendatang.")),
                                       DTOutput("arsip_ojk"),
                                       br(),
                                       div(style = "overflow-x: auto; width: 100%;", DTOutput("table1"))
                                     )
                                   )
                            )
                          ),
                          # Form Input Data Baru
                          fluidRow(
                            column(12,
                                   div(
                                     style = "margin-top: 30px; margin-bottom: 30px; font-size: 18px; border-radius: 15px; border: 1px solid #d8dee9; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);",  
                                     class = "cardbaru",
                                     div(class = "card-header", style = "background-color: #EE6363; color: white; font-weight: bold; border-radius: 15px 15px 0 0; padding: 15px;", "Tambah Data Arsip"),
                                     div(class = "card-body", style = "padding: 20px; text-align: justify;",
                                         tags$label(style = "margin-bottom:0px","Masukkan Kata Kunci:"),
                                         tags$br(),
                                         tags$small(style = "color: red; font-style: italic; margin-top:0px", 
                                                    "Apabila hendak mengedit tabel, tanyakan kata kunci pada admin"),  # Dipindah ke sini
                                         div(
                                           style = "display: flex; align-items: center; gap:5px",
                                           uiOutput("password_ui"),  # Output untuk password input
                                           actionButton("toggle_pw", "", icon = icon("eye-slash"), 
                                                        style = "height: 38px; width: 38px; display: flex; align-items: center; justify-content: center;margin-top: 0px; margin-bottom: 17px")  
                                         ),
                                         textInput("kode_klasifikasi", "Kode Klasifikasi:"),
                                         textInput("tentang_perihal", "Tentang/Perihal:"),
                                         textInput("status", "Status:"),
                                         actionButton("addData", "Tambah Data", class = "btn btn-primary"),
                                         actionButton("deleteData", "Hapus Data Terpilih", class = "btn btn-danger"),
                                         downloadButton("downloadData", "Download Excel", class = "btn btn-success")
                                     )
                                     
                                   )
                            )
                          )
                          
                          
                        )
               ),
               
               # Tab kedua: Penginputan
               tabPanel("Penginputan",
                        fluidPage(
                          br(),  # Tambahkan ini untuk memberi jarak
                          
                          # Card Pertama
                          fluidRow(
                            style = "margin-top: 250px;",  # Tambahkan jarak agar tidak bertumpuk
                            column(12, 
                                   div(class = "card",
                                       div(class = "card-header", "Penginputan Data Pengawasan Perbankan"),
                                       div(
                                         class = "card-body",
                                         h2(style = "text-align: left;font-weight: bold;", class = "card-title", "Tujuan Penginputan"),
                                         p("Penginputan data pengawasan perbankan bertujuan untuk mempermudah pencarian dokumen yang diperlukan, sehingga ketika dibutuhkan, baik untuk keperluan internal maupun audit eksternal, dokumen dapat ditemukan dengan cepat dan akurat. Dengan sistem penginputan yang terstruktur, keberadaan dokumen dapat dipastikan dan dikelola secara lebih efisien, mengurangi risiko kehilangan atau kesalahan penyimpanan."),
                                         
                                         h2(style = "text-align: left;font-weight: bold;", class = "card-title", "Dokumen Apa Saja yang Diinput"),
                                         p("Semua dokumen pengawasan perbankan diinput ke dalam sistem, baik yang bersifat aktif maupun inaktif, serta permanen maupun musnah. Penginputan ini mencakup berbagai jenis dokumen sebagai berikut:"),
                                         tags$ul(
                                           tags$li(HTML("<b>Dokumen Aktif:</b> Dokumen yang masih digunakan dalam operasional saat ini.")),
                                           tags$li(HTML("<b>Dokumen Inaktif:</b> Dokumen yang sudah tidak digunakan secara rutin tetapi masih perlu disimpan sebagai arsip.")),
                                           tags$li(HTML("<b>Dokumen Permanen:</b> Dokumen yang harus disimpan selamanya karena memiliki nilai hukum, historis, atau administratif.")),
                                           tags$li(HTML("<b>Dokumen Musnah:</b> Dokumen yang akan dimusnahkan setelah memasuki masa inaktif sesuai kebijakan arsip."))
                                         ),
                                         
                                         h2(style = "text-align: left;font-weight: bold;", class = "card-title", "Langkah Penginputan"),
                                         p("Adapun langkah langkah penginputan:"),
                                         tags$ol(
                                           tags$li(HTML("<b>Pengarsipan Dokumen:</b> Pastikan dokumen telah diarsipkan berdasarkan BPRS, tahun, dan pengawas serta sudah dikelompokkan dalam box sesuai kategori.")),
                                           tags$li(HTML("<b>Menyiapkan Format Excel:</b> Gunakan file Excel dengan dua sheet utama:
      <ul>
        <li><b>Sheet 'KOTAK BOX':</b> Berisi daftar dokumen yang sudah dikelompokkan dalam box berdasarkan tahun dan jenis dokumen.</li>
        <li><b>Sheet 'MAP':</b> Berisi rincian dokumen dalam setiap map, mencatat setiap nomor surat yang diarsipkan.</li>
      </ul>")),
                                           tags$li(HTML("<b>Pengurutan Data:</b>
      <ul>
        <li>Input data dari tahun terbaru ke tahun terlama (misal: 2024, lalu 2023, 2022, dan seterusnya).</li>
        <li>Dokumen dalam setiap map diinput berdasarkan tanggal terbaru ke tanggal terlama (misal: Desember, lalu November, Oktober, hingga Januari).</li>
      </ul>")),
                                           tags$li(HTML("<b>Perhitungan Jumlah Dokumen:</b>
      <ul>
        <li>Setiap dokumen yang tidak dijild dan memiliki nomor surat wajib dihitung jumlah lembarnya.</li>
        <li>Dokumen yang telah terjilid maka keterangan jumlah dapat ditulis <b>1 BERKAS.</b></li>
      </ul>")),
                                           tags$li(HTML("<b>Penomoran dan Labeling:</b>
      <ul>
        <li>Setelah setiap dokumen diinput, berikan nomor pada tiap map kuping untuk memudahkan pencarian.</li>
        <li>Lakukan labeling pada setiap box, termasuk nomor box dan kategori dokumen di dalamnya.</li>
      </ul>")),
                                           tags$li(HTML("<b>Verifikasi dan Finalisasi:</b> Pastikan semua dokumen telah diinput dengan benar dan sesuai urutan. Adapun tampilan excel dari dokumen yang telah terinput yakni sebagai berikut"),
                                                   div(
                                                     style = "display: flex; justify-content: center; margin-bottom: 30px",
                                                     tags$iframe(
                                                       width = "800", height = "450",  # Ukuran video diperbesar
                                                       src = "https://www.youtube.com/embed/SLBmUsJ1R5E",  # Ganti dari youtu.be ke youtube.com/embed
                                                       frameborder = "0", 
                                                       allow = "accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", 
                                                       allowfullscreen = TRUE
                                                     )
                                                   )
                                           )
                                           
                                         )
                                       )
                                       
                                   )
                            )
                          ),
                          fluidRow(
                            div(
                              h1(
                                style = "font-weight: bold; font-style: normal; margin-bottom: 20px; font-size: 50px; color: #030303; text-align: center;",
                                "RESULTS",
                                
                                tags$div(
                                  style = "margin-top: 10px; margin-bottom: 20px; text-align: center;align-items: center;",
                                  imageOutput("gambar1", width = "100%", height = "auto")
                                )
                                
                                
                                
                              )
                            )
                            
                          )
                          
                          
                        )
               )
             )
           )
  ),
  
  # About Page
  tabPanel("Lantai 8",
           fluidPage(
             tabsetPanel(
               # Tab pertama: Pengarsipan
               tabPanel("Pengarsipan",
                        fluidPage(
                          br(),  # Tambahkan ini untuk memberi jarak
                          
                          # Card Pertama
                          fluidRow(
                            style = "margin-top: 250px;",  # Tambahkan jarak agar tidak bertumpuk
                            column(12, 
                                   div(class = "card",
                                       div(class = "card-header", "Arsip Data Anggaran"),
                                       div(
                                         class = "card-body",
                                         h2(style = "text-align: left;font-weight: bold;", class = "card-title", "Jenis Berkas yang Diarsipkan"),
                                         p("Pengarsipan dokumen anggaran mencakup dua jenis utama, yaitu:"),
                                         tags$ul(
                                           tags$li(HTML("<b>Berkas Anggaran Perjalanan Dinas :</b> Berisi dokumen terkait biaya perjalanan dinas bagi Pejabat Negara, Pegawai Negeri, dan Pegawai Tidak Tetap.")),
                                           tags$li(HTML("<b>Berkas Anggaran Non-Perjalanan Dinas :</b> Berisi dokumen yang berkaitan dengan anggaran selain perjalanan dinas, seperti belanja operasional, pengadaan barang dan jasa, serta biaya lainnya yang tidak melibatkan perjalanan dinas."))
                                         ),
                                         
                                         h2(style = "text-align: left;font-weight: bold;", class = "card-title", "Tujuan Pengarsipan Dokumen"),
                                         tags$ul(
                                           tags$li("Mempermudah pencarian dan penelusuran dokumen saat dibutuhkan, baik untuk keperluan internal maupun audit eksternal."),
                                           tags$li("Menjamin keamanan dan keteraturan dalam penyimpanan dokumen, sehingga terhindar dari risiko kehilangan atau kerusakan."),
                                           tags$li("Meningkatkan efisiensi dalam pengelolaan data anggaran agar lebih tertata dan mudah diakses sesuai kebutuhan.")
                                         ),
                                         
                                         h2(style = "text-align: left;font-weight: bold;", class = "card-title", "Langkah-Langkah Pengarsipan"),
                                         tags$ol(
                                           tags$li(HTML("Memilah Dokumen Berdasarkan Tahun dan Bulan
- Dokumen diorganisir secara kronologis dari tahun terbaru hingga tahun terdahulu.
- Setiap tahun dibagi lagi berdasarkan bulan untuk mempermudah pencarian berdasarkan periode waktu.")),
                                           tags$li(HTML("Memisahkan Dokumen Berdasarkan Jenisnya
- Dokumen Perjalanan Dinas dan Non-Perjalanan Dinas disusun secara terpisah untuk menghindari pencampuran data.
- Setiap kategori memiliki tempat penyimpanan tersendiri guna mempermudah proses audit dan referensi ke depan.")),
                                           tags$li(HTML("Penyusunan Dokumen dalam Map dan Box
- Setiap dokumen dalam satu bulan disusun dalam map per tanggal untuk memastikan keteraturan.
- Setiap box hanya berisi dokumen dalam satu bulan, dengan pemisahan antara Perjalanan Dinas dan Non-Perjalanan Dinas dalam box yang berbeda."))
                                         )
                                       )
                                       
                                       
                                       
                                       
                                       
                                   )
                            )
                          ),
                          fluidRow(
                            div(
                              h1(
                                style = "font-weight: bold; font-style: normal; margin-bottom: 20px; font-size: 50px; color: #030303; text-align: center;",
                                "RESULTS",
                                
                                tags$div(
                                  style = "margin-top: 10px; margin-bottom: 20px; text-align: center;align-items: center;",
                                  imageOutput("gambar2", width = "100%", height = "auto")
                                )
                                
                                
                                
                              )
                            )
                            
                          )
                          
                          
                        )
               ),
               
               # Tab kedua: Research Fellow
               tabPanel("Research Fellow",
                        fluidPage(
                          style = "margin-top: 0px;",
                          conditionalPanel(
                            condition = "input.visualize > 0"
                          ),
                          
                          # Teks Awal
                          fluidRow(
                            div(
                              h1(style = "font-weight: bold; font-style: normal; font-size: 60px; color: #FF0000; text-align: center ; text-shadow: 2px 2px 0px black, -2px -2px 0px black, 2px -2px 0px black, -2px 2px 0px black;", "INFOGRAFIS STATISTIK PERBANKAN INDONESIA"),
                              tags$i(tags$a(href = "https://www.ojk.go.id/id/kanal/perbankan/data-dan-statistik/statistik-perbankan-indonesia/Default.aspx", 
                                            "Download Data Statistik Perbankan Disini", 
                                            class = "btn btn-primary",
                                            style = "padding: auto ; font-size: 15px; display: block; margin: auto; background-color: #FFDEAD; color: black; border: none; width: fit-content;")
                              ))
                          ),
                          
                          # Row 1: UPLOAD FILE
                          fluidRow(
                            box(
                              style = "margin-top: 10px; box-shadow: 0px 4px 10px rgba(0, 0, 0, 0.1);",
                              class = "card-rv",
                              width = 12,
                              div(class = "card-header", "Upload dan Pilih Data", style="height: 35px; display: flex; align-items: center; "),
                              div(
                                class = "card-body",
                                style = "
                                        display: flex; 
                                        height: 80px; 
                                        padding: 5px;
                                        margin-top: 5px;
                                        border: none;
                                        justify-content: space-between;
                                      ",
                                status = "primary",
                                solidHeader = TRUE,
                                fileInput("file", "Upload File Excel", accept = c(".xlsx")),
                                selectInput("month", "Pilih Bulan", 
                                            choices = c("", month.name), 
                                            selected = NULL),
                                selectInput("year", "Pilih Tahun", 
                                            choices = c("", as.character(2000:2030)), 
                                            selected = NULL),
                                actionButton("visualize", "Visualisasikan", 
                                             style = "
                                             margin-top: 40 px important;
                                             padding: 10px 15px; 
                                             font-size: 14px; 
                                             width: auto; 
                                             height: 60px;
                                             background-color: #8B8989; 
                                             color: white; 
                                             border: none;
                                             border-radius: 5px;
                                             text-align: center;
                                           ")  # Menyesuaikan ukuran action button
                              )
                            )
                          ),
                          
                          # Row 2: PERKEMBANGAN KREDIT DAN NPL BANK
                          fluidRow(
                            # JUDL SPI BANK UMUM
                            div(
                              h1(style = "font-weight: bold; font-style: normal; font-size: 30px; color: black; text-align: left ; margin-left: 15px; background-color: #FFDEAD; width: fit-content;", "INFOGRAFIS STATISTIK PERBANKAN INDONESIA BANK UMUM")
                            ),
                          ),
                          
                          # Row 3: PERKEMBANGAN KREDIT DAN NPL BANK UMUM
                          #Box : 1 SISI KIRI
                          fluidRow(
                            style = "margin-top: 5px;",
                            
                            # **Box Grafik 1**
                            box(
                              style = "
                              background-color: white;
                              border-radius: 15px;
                              box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
                              padding: 10px;
                              border: 1px solid #d8dee9;
                              display: flex;
                              flex-direction: column;
                              justify-content: center;
                              align-items: center;
                              height: 550px; /* Ukuran diperbesar agar teks masuk */
                              width: 100%;
                              margin-top: 10px;
                              margin-bottom: 10px;
                              padding-bottom: 20px; /* Tambahan padding bawah */
                          ",
                              
                              # **Teks di atas grafik**
                              div(
                                style = "width: 100%; text-align: center; margin-bottom: 10px; margin-top: 20px;",
                                tags$p(
                                  tags$b("Perkembangan Kredit dan NPL Bank Umum"),
                                  style = "font-size: 20px; font-weight: bold; text-align: center; margin-top: 20px;"
                                )
                              ),
                              
                              # **Grafik tengah**
                              div(
                                style = "
                                width: 100%; 
                                height: auto; 
                                display: flex; 
                                justify-content: center; 
                                align-items: center; 
                            ",
                                highchartOutput("combo_chart1", width = "95%", height = "400px")
                              ),
                              
                              # **Teks di bawah grafik (Diolah dari ...)**
                              div(
                                style = "width: 100%; text-align: center; margin-bottom: 10px; background-color: rgba(128, 128, 128, 0.2);",
                                tags$p(
                                  tags$i("Diolah dari Statistik Perbankan Indonesia Tabel 3.13.a Kredit/Pembiayaan dan NPL/NPF Bank Umum Kepada Pihak Ketiga Bukan Bank Berdasarkan Lokasi Dati I Bank Penyalur Kredit/Pembiayaan"),
                                  style = "font-size: 15px; font-weight: normal; text-align: center; margin-bottom: 10px; margin-top: 0px;"
                                )
                              )
                            ),
                            
                            #Box : 2 SISI KANAN
                            box(
                              style = "
                              background-color: white;
                              border-radius: 15px;
                              box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
                              padding: 10px;
                              border: 1px solid #d8dee9;
                              display: flex;
                              flex-direction: column;
                              justify-content: center;
                              align-items: center;
                              height: 550px; /* Ukuran diperbesar agar teks masuk */
                              width: 100%;
                              margin-top: 10px;
                              margin-bottom: 10px;
                              padding-bottom: 20px; /* Tambahan padding bawah */
                          ",
                              
                              # **Teks di atas grafik**
                              div(
                                style = "width: 100%; text-align: center; margin-bottom: 10px; margin-top: 20px;",
                                tags$p(
                                  tags$b("Komposisi Kredit per Jenis Penggunaan Bank Umum"),
                                  style = "font-size: 20px; font-weight: bold; text-align: center; margin-top: 20px;"
                                )
                              ),
                              
                              # **Grafik tengah**
                              div(
                                style = "
                                width: 100%; 
                                height: auto; 
                                display: flex; 
                                justify-content: center; 
                                align-items: center; 
                            ",
                                highchartOutput("combo_chart2", width = "95%", height = "400px")
                              ),
                              
                              # **Teks di bawah grafik (Diolah dari ...)**
                              div(
                                style = "width: 100%; text-align: center; margin-bottom: 10px; background-color: rgba(128, 128, 128, 0.2);",
                                tags$p(
                                  tags$i("Diolah dari Statistik Perbankan Indonesia Tabel 3.6 Kredit/Pembiayaan dan NPL/NPF Bank Umum Kepada Pihak Ketiga Bukan Bank Berdasarkan Jenis Penggunaan dan Orientasi Penggunaan"),
                                  style = "font-size: 15px; font-weight: normal; text-align: center; margin-bottom: 10px; margin-top: 0px;"
                                )
                              )
                            )
                          ),
                          
                          
                          # Row 4: PERKEMBANGAN DANA PIHAK KETIGA BANK UMUM
                          #Box : 1 SISI KIRI
                          fluidRow(
                            style = "margin-top: 5px;",
                            
                            # **Box Grafik 1**
                            box(
                              style = "
                              background-color: white;
                              border-radius: 15px;
                              box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
                              padding: 10px;
                              border: 1px solid #d8dee9;
                              display: flex;
                              flex-direction: column;
                              justify-content: center;
                              align-items: center;
                              height: 550px; /* Ukuran diperbesar agar teks masuk */
                              width: 100%;
                              margin-top: 10px;
                              margin-bottom: 10px;
                              padding-bottom: 20px; /* Tambahan padding bawah */
                          ",
                              
                              # **Teks di atas grafik**
                              div(
                                style = "width: 100%; text-align: center; margin-bottom: 10px; margin-top: 20px;",
                                tags$p(
                                  tags$b("Perkembangan Dana Pihak Ketiga Bank Umum"),
                                  style = "font-size: 20px; font-weight: bold; text-align: center; margin-top: 20px;"
                                )
                              ),
                              
                              # **Grafik tengah**
                              div(
                                style = "
                                width: 100%; 
                                height: auto; 
                                display: flex; 
                                justify-content: center; 
                                align-items: center; 
                            ",
                                highchartOutput("combo_chart3", width = "95%", height = "400px")
                              ),
                              
                              # **Teks di bawah grafik (Diolah dari ...)**
                              div(
                                style = "width: 100%; text-align: center; margin-bottom: 10px; background-color: rgba(128, 128, 128, 0.2);",
                                tags$p(
                                  tags$i("Diolah dari Statistik Perbankan Indonesia Tabel 1.34.a Penghimpunan DPK Bank Umum Berdasarkan Lokasi Bank Penghimpun Dana"),
                                  style = "font-size: 15px; font-weight: normal; text-align: center; margin-bottom: 10px; margin-top: 0px;"
                                )
                              )
                            ),
                            
                            #Box : 2 SISI KANAN
                            box(
                              style = "
                              background-color: white;
                              border-radius: 15px;
                              box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
                              padding: 10px;
                              border: 1px solid #d8dee9;
                              display: flex;
                              flex-direction: column;
                              justify-content: center;
                              align-items: center;
                              height: 550px; /* Ukuran diperbesar agar teks masuk */
                              width: 100%;
                              margin-top: 10px;
                              margin-bottom: 10px;
                              padding-bottom: 20px; /* Tambahan padding bawah */
                          ",
                              
                              # **Teks di atas grafik**
                              div(
                                style = "width: 100%; text-align: center; margin-bottom: 10px; margin-top: 20px;",
                                tags$p(
                                  tags$b("Komposisi Dana Pihak Ketiga Bank Umum"),
                                  style = "font-size: 20px; font-weight: bold; text-align: center; margin-top: 20px;"
                                )
                              ),
                              
                              # **Grafik tengah**
                              div(
                                style = "
                                width: 100%; 
                                height: auto; 
                                display: flex; 
                                justify-content: center; 
                                align-items: center; 
                            ",
                                highchartOutput("combo_chart4", width = "95%", height = "400px")
                              ),
                              
                              # **Teks di bawah grafik (Diolah dari ...)**
                              div(
                                style = "width: 100%; text-align: center; margin-bottom: 10px; background-color: rgba(128, 128, 128, 0.2);",
                                tags$p(
                                  tags$i("Diolah dari Statistik Perbankan Indonesia Tabel 1.28.a Komposisi Dana Pihak Ketiga Bank Umum"),
                                  style = "font-size: 15px; font-weight: normal; text-align: center; margin-bottom: 10px; margin-top: 0px;"
                                )
                              )
                            )
                          ),
                          
                          
                          # Row 5: PERKEMBANGAN SUKU BUNGA RATA RATA DAN KREDIT DPK BANK UMUM
                          #Box : 1 SISI KIRI
                          fluidRow(
                            style = "margin-top: 5px;",
                            
                            # **Box Grafik 1**
                            box(
                              style = "
                              background-color: white;
                              border-radius: 15px;
                              box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
                              padding: 10px;
                              border: 1px solid #d8dee9;
                              display: flex;
                              flex-direction: column;
                              justify-content: center;
                              align-items: center;
                              height: 550px; /* Ukuran diperbesar agar teks masuk */
                              width: 100%;
                              margin-top: 10px;
                              margin-bottom: 10px;
                              padding-bottom: 20px; /* Tambahan padding bawah */
                          ",
                              
                              # **Teks di atas grafik**
                              div(
                                style = "width: 100%; text-align: center; margin-bottom: 10px; margin-top: 20px;",
                                tags$p(
                                  tags$b("Perkembangan Suku Bunga Rata-rata DPK Bank Umum"),
                                  style = "font-size: 20px; font-weight: bold; text-align: center; margin-top: 20px;"
                                ),
                                tags$p("Mata Uang Rupiah - (%)",
                                       style = "font-size: 13px; text-align: center; margin-top: 0px;"
                                )
                              ),
                              
                              # **Grafik tengah**
                              div(
                                style = "
                                width: 100%; 
                                height: auto; 
                                display: flex; 
                                justify-content: center; 
                                align-items: center; 
                            ",
                                highchartOutput("combo_chart5", width = "95%", height = "400px")
                              ),
                              
                              # **Teks di bawah grafik (Diolah dari ...)**
                              div(
                                style = "width: 100%; text-align: center; margin-bottom: 25px; background-color: rgba(128, 128, 128, 0.2);",
                                tags$p(
                                  tags$i("Diolah dari Statistik Perbankan Indonesia Tabel 1.35.a Suku Bunga Rata-rata DPK Bank Umum"),
                                  style = "font-size: 15px; font-weight: normal; text-align: center; margin-bottom: 10px; margin-top: 0px;"
                                )
                              )
                            ),
                            
                            #Box : 2 SISI KANAN
                            box(
                              style = "
                              background-color: white;
                              border-radius: 15px;
                              box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
                              padding: 10px;
                              border: 1px solid #d8dee9;
                              display: flex;
                              flex-direction: column;
                              justify-content: center;
                              align-items: center;
                              height: 550px; /* Ukuran diperbesar agar teks masuk */
                              width: 100%;
                              margin-top: 10px;
                              margin-bottom: 10px;
                              padding-bottom: 20px; /* Tambahan padding bawah */
                          ",  
                              
                              # **Teks di atas grafik**
                              div(
                                style = "width: 100%; text-align: center; margin-bottom: 10px; margin-top: 20px;",
                                tags$p(
                                  tags$b("Perkembangan Suku Bunga Kredit Bank Umum"),
                                  style = "font-size: 20px; font-weight: bold; text-align: center; margin-top: 20px;"
                                ),
                                tags$p("Berdasarkan Jenis Penggunaan - Mata Uang Rupiah - (%)",
                                       style = "font-size: 13px; text-align: center; margin-top: 0px;"
                                )
                              ),
                              
                              # **Grafik tengah**
                              div(
                                style = "
                                width: 100%; 
                                height: auto; 
                                display: flex; 
                                justify-content: center; 
                                align-items: center; 
                            ",
                                highchartOutput("combo_chart6", width = "95%", height = "400px")
                              ),
                              
                              # **Teks di bawah grafik (Diolah dari ...)**
                              div(
                                style = "width: 100%; text-align: center; margin-bottom: 25px; background-color: rgba(128, 128, 128, 0.2);",
                                tags$p(
                                  tags$i("Diolah dari Statistik Perbankan Indonesia Tabel 1.41.a Suku Bunga Rata-rata Kredit Bank Umum Kepada Pihak Ketiga Bukan Bank Berdasarkan Jenis Pengggunaan dan Orientasi Penggunaan"),
                                  style = "font-size: 15px; font-weight: normal; text-align: center; margin-bottom: 10px; margin-top: 0px;"
                                )
                              )
                            )
                          ),
                          
                          # Row 6: INDIKATOR KINERJA BANK UMUM KONVENSIONAL
                          #Box Panjang
                          fluidRow(
                            style = "margin-top: 5px;", 
                            
                            # **Box Grafik 1**
                            box(
                              style = "background-color: white; margin-top: 10px; border-radius: 15px; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);padding: 10px; border: 1px solid #d8dee9; flex-direction: column;
                              justify-content: center;
                              align-items: center;
                              height: 280px; /* Ukuran diperbesar agar teks masuk */
                              width: 100%;
                              margin-top: 10px;
                              margin-bottom: 10px;
                              padding-bottom: 20px; /* Tambahan padding bawah */",
                              width = 12,
                              
                              # **Teks di atas grafik**
                              div(
                                style = "width: 100%; text-align: center; margin-bottom: 10px; margin-top: 10px;",
                                tags$p(
                                  tags$b("Indikator Kinerja Bank Umum Konvensional"),
                                  style = "font-size: 20px; font-weight: bold; text-align: center; margin-top: 10px;"
                                )
                              ),
                              
                              # **Grafik tengah**
                              div(
                                style = "
                                width: 100%; 
                                height: auto; 
                                display: flex; 
                                justify-content: center; 
                                align-items: center; 
                            ",
                                
                                # **ValueBox for CAR**
                                valueBoxOutput("car", width = 2),
                                
                                # **ValueBox for BOPO**
                                valueBoxOutput("bopo", width = 2),
                                
                                # **ValueBox for LDR**
                                valueBoxOutput("ldr", width = 2),
                                
                                # **ValueBox for NIM**
                                valueBoxOutput("nim", width = 2),
                                
                                # **ValueBox for ROA**
                                valueBoxOutput("roa", width = 2),
                                
                                # **ValueBox for NPL 1.16a**
                                valueBoxOutput("npl_116a", width = 2)
                                
                              ),
                              
                              # **Teks di bawah grafik (Diolah dari ...)**
                              div(
                                style = "width: 100%; text-align: center; margin-bottom: 25px; background-color: rgba(128, 128, 128, 0.2);",
                                tags$p(
                                  tags$i("Diolah dari Statistik Perbankan Indonesia Tabel 1.16.a Kinerja Bank Umum Konvensional dan data NPL dari Tabel 3.13.a Kredit dan NPL Bank Umum Kepada Pihak Ketiga Bukan Bank berdasarkan Lokasi Dati I Bank Penyalur Kredit"),
                                  style = "font-size: 15px; font-weight: normal; text-align: center; margin-bottom: 15px; margin-top: 13px;"
                                )
                              )
                              
                              
                            )
                            
                          ),
                          
                          # Row 7: INFOGRAFIS SPI BPR
                          fluidRow(
                            # JUDUL SPI BPR
                            div(
                              h1(style = "font-weight: bold; font-style: normal; font-size: 30px; color: black; text-align: left ; margin-left: 15px ; margin-bottom: 10px ; background-color: #FFDEAD; width: fit-content;", "INFOGRAFIS STATISTIK PERBANKAN INDONESIA BANK PENKREDITAN RAKYAT")
                            ),
                          ),
                          
                          # Row 8: PERKEMBANGAN KREDIT BPR
                          #Box : 1 SISI KIRI
                          fluidRow(
                            style = "margin-top: 5px;",
                            
                            # **Box Grafik 1**
                            box(
                              style = "
                              background-color: white;
                              border-radius: 15px;
                              box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
                              padding: 10px;
                              border: 1px solid #d8dee9;
                              display: flex;
                              flex-direction: column;
                              justify-content: center;
                              align-items: center;
                              height: 530px; /* Ukuran diperbesar agar teks masuk */
                              width: 100%;
                              margin-top: 10px;
                              margin-bottom: 10px;
                              padding-bottom: 20px; /* Tambahan padding bawah */
                          ",
                              
                              # **Teks di atas grafik**
                              div(
                                style = "width: 100%; text-align: center; margin-bottom: 10px; margin-top: 20px;",
                                tags$p(
                                  tags$b("Perkembangan Kredit BPR"),
                                  style = "font-size: 20px; font-weight: bold; text-align: center; margin-top: 20px;"
                                )
                              ),
                              
                              # **Grafik tengah**
                              div(
                                style = "
                                width: 100%; 
                                height: auto; 
                                display: flex; 
                                justify-content: center; 
                                align-items: center; 
                            ",
                                highchartOutput("combo_chart7", width = "95%", height = "400px")
                              ),
                              
                              # **Teks di bawah grafik (Diolah dari ...)**
                              div(
                                style = "width: 100%; text-align: center; margin-bottom: 10px; background-color: rgba(128, 128, 128, 0.2);",
                                tags$p(
                                  tags$i("Diolah dari Statistik Perbankan Indonesia Tabel 2.1 Kegiatan Usaha Bank Perkreditan Rakyat"),
                                  style = "font-size: 15px; font-weight: normal; text-align: center; margin-bottom: 10px; margin-top: 0px;"
                                )
                              )
                            ),
                            
                            #Box : 2 SISI KANAN
                            box(
                              style = "
                              background-color: white;
                              border-radius: 15px;
                              box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
                              padding: 10px;
                              border: 1px solid #d8dee9;
                              display: flex;
                              flex-direction: column;
                              justify-content: center;
                              align-items: center;
                              height: 530px; /* Ukuran diperbesar agar teks masuk */
                              width: 100%;
                              margin-top: 10px;
                              margin-bottom: 10px;
                              padding-bottom: 20px; /* Tambahan padding bawah */
                          ",
                              
                              # **Teks di atas grafik**
                              div(
                                style = "width: 100%; text-align: center; margin-bottom: 10px; margin-top: 20px;",
                                tags$p(
                                  tags$b("Komposisi Kredit BPR"),
                                  style = "font-size: 20px; font-weight: bold; text-align: center; margin-top: 20px;"
                                )
                              ),
                              
                              # **Grafik tengah**
                              div(
                                style = "
                                width: 100%; 
                                height: auto; 
                                display: flex; 
                                justify-content: center; 
                                align-items: center; 
                            ",
                                highchartOutput("combo_chart8", width = "95%", height = "400px")
                              ),
                              
                              # **Teks di bawah grafik (Diolah dari ...)**
                              div(
                                style = "width: 100%; text-align: center; margin-bottom: 10px; background-color: rgba(128, 128, 128, 0.2);",
                                tags$p(
                                  tags$i("Diolah dari Statistik Perbankan Indonesia Tabel 3.17.a Kredit Bank Perkreditan Rakyat Berdasarkan Jenis Penggunaan"),
                                  style = "font-size: 15px; font-weight: normal; text-align: center; margin-bottom: 10px; margin-top: 0px;"
                                )
                              )
                            )
                          ),
                          
                          # Row 9: PERKEMBANGAN DPK BPR
                          #Box : 1 SISI KIRI
                          fluidRow(
                            style = "margin-top: 5px;",
                            
                            # **Box Grafik 1**
                            box(
                              style = "
                              background-color: white;
                              border-radius: 15px;
                              box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
                              padding: 10px;
                              border: 1px solid #d8dee9;
                              display: flex;
                              flex-direction: column;
                              justify-content: center;
                              align-items: center;
                              height: 530px; /* Ukuran diperbesar agar teks masuk */
                              width: 100%;
                              margin-top: 10px;
                              margin-bottom: 10px;
                              padding-bottom: 20px; /* Tambahan padding bawah */
                          ",
                              
                              # **Teks di atas grafik**
                              div(
                                style = "width: 100%; text-align: center; margin-bottom: 10px; margin-top: 20px;",
                                tags$p(
                                  tags$b("Perkembangan DPK BPR"),
                                  style = "font-size: 20px; font-weight: bold; text-align: center; margin-top: 20px;"
                                )
                              ),
                              
                              # **Grafik tengah**
                              div(
                                style = "
                                width: 100%; 
                                height: auto; 
                                display: flex; 
                                justify-content: center; 
                                align-items: center; 
                            ",
                                highchartOutput("combo_chart9", width = "95%", height = "400px")
                              ),
                              
                              # **Teks di bawah grafik (Diolah dari ...)**
                              div(
                                style = "width: 100%; text-align: center; margin-bottom: 10px; background-color: rgba(128, 128, 128, 0.2);",
                                tags$p(
                                  tags$i("Diolah dari Statistik Perbankan Indonesia Tabel 2.1 Kegiatan Usaha Bank Perkreditan Rakyat"),
                                  style = "font-size: 15px; font-weight: normal; text-align: center; margin-bottom: 10px; margin-top: 0px;"
                                )
                              )
                            ),
                            
                            #Box : 2 SISI KANAN
                            box(
                              style = "
                              background-color: white;
                              border-radius: 15px;
                              box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
                              padding: 10px;
                              border: 1px solid #d8dee9;
                              display: flex;
                              flex-direction: column;
                              justify-content: center;
                              align-items: center;
                              height: 530px; /* Ukuran diperbesar agar teks masuk */
                              width: 100%;
                              margin-top: 10px;
                              margin-bottom: 10px;
                              padding-bottom: 20px; /* Tambahan padding bawah */
                          ",
                              
                              # **Teks di atas grafik**
                              div(
                                style = "width: 100%; text-align: center; margin-bottom: 10px; margin-top: 20px;",
                                tags$p(
                                  tags$b("Komposisi DPK BPR"),
                                  style = "font-size: 20px; font-weight: bold; text-align: center; margin-top: 20px;"
                                )
                              ),
                              
                              # **Grafik tengah**
                              div(
                                style = "
                                width: 100%; 
                                height: auto; 
                                display: flex; 
                                justify-content: center; 
                                align-items: center; 
                            ",
                                highchartOutput("combo_chart10", width = "95%", height = "400px")
                              ),
                              
                              # **Teks di bawah grafik (Diolah dari ...)**
                              div(
                                style = "width: 100%; text-align: center; margin-bottom: 10px; background-color: rgba(128, 128, 128, 0.2);",
                                tags$p(
                                  tags$i("Diolah dari Statistik Perbankan Indonesia Tabel 2.7 Komposisi DPK Bank Perkreditan Rakyat"),
                                  style = "font-size: 15px; font-weight: normal; text-align: center; margin-bottom: 20px; margin-top: 0px;"
                                )
                              )
                            )
                          ),
                          
                          br(),  # Tambahkan ini untuk memberi jarak
                          
                          div(style = "height: 2px; background-color: black; margin: 20px 0;
                                      box-shadow: 0px 6px 45px rgba(0, 0, 0, 0.9);"),
                          
                          
                          # Teks Awal
                          fluidRow(
                            div(
                              h1(
                                style = "font-weight: bold; font-style: normal; margin-bottom: 10px; font-size: 60px; color: #FF0000; text-align: center; text-shadow: 2px 2px 0px black, -2px -2px 0px black, 2px -2px 0px black, -2px 2px 0px black;",
                                "PROJECT REQUEST",
                                
                                tags$div(
                                  style = "margin-top: 10px",
                                  slickROutput("image_carousel1", width = "75%", height = "auto")
                                )
                                
                                
                              )
                            )
                            
                          )  
                          
                        )
               )
             )
           )
  ),
  
  # About Page
  tabPanel("Lantai 9",
           fluidPage(
             tabsetPanel(
               # Tab pertama: Konten
               tabPanel("Konten Edukasi",
                        fluidPage(
                          br(),  # Memberi jarak atas
                          
                          # Apa itu Konten Edukasi?
                          fluidRow(
                            style = "margin-top: 250px;",  
                            column(12, 
                                   div(class = "card", style = "min-height: 550px;",  
                                       div(class = "card-header", "Konten Edukasi"),
                                       div(class = "card-body",
                                           
                                           # Apa Itu Konten Edukasi?
                                           h2(style = "text-align: left; font-weight: bold;", class = "card-title", "Apa itu Konten Edukasi?"),
                                           p(HTML("Konten edukasi adalah materi yang dirancang untuk meningkatkan literasi keuangan masyarakat. 
                         Melalui konten ini, OJK Jatim memberikan informasi yang mudah dipahami mengenai pengelolaan keuangan, 
                         investasi yang aman, serta cara menghindari penipuan finansial. Konten ini tersedia dalam berbagai format, 
                         seperti video, artikel, dan infografis, yang dapat diakses secara gratis oleh masyarakat.")),
                                           
                                           br(),  # Jeda antar bagian
                                           
                                           # Manfaat Konten Edukasi
                                           h2(style = "text-align: left; font-weight: bold;", class = "card-title", "Manfaat Konten Edukasi?"),
                                           p(HTML("Berikut manfaat Konten Edukasi bagi masyarakat umum:  
        <ol>
        <li>Meningkatkan literasi keuangan masyarakat dengan menyediakan informasi yang mudah dipahami mengenai konsep keuangan dasar, investasi, dan pengelolaan keuangan pribadi.</li>
        <li>Membantu masyarakat dalam mengambil keputusan finansial yang lebih bijak, seperti menghindari utang berlebihan dan memilih instrumen investasi yang sesuai.</li>
        <li>Memberikan pemahaman tentang cara mendeteksi dan menghindari penipuan finansial agar masyarakat tidak mudah tertipu oleh investasi bodong atau skema ilegal.</li>
        <li>Mendukung inklusi keuangan dengan memastikan bahwa informasi mengenai produk dan layanan keuangan dapat diakses oleh berbagai lapisan masyarakat secara gratis.</li>
        </ol>")),
                                           br(),  # Jeda antar bagian
                                           
                                           # Cek Selengkapnya di instagram
                                           h2(style = "text-align: left; font-weight: bold;", class = "card-title", 
                                              "Cek Instagram OJK Jawa Timur:"),
                                           br(),
                                           a(href = "https://www.instagram.com/ojk_jatim", target = "_blank", 
                                             style = "display: inline-block; background-color: #28a745; color: white; 
           padding: 10px 20px; font-size: 16px; font-weight: bold; 
           text-align: center; text-decoration: none; border-radius: 5px; 
           width: 150px;",
                                             "Klik Disini")
                                       )
                                   )
                            )
                          ),
                          
                          # Card untuk Konten Edukasi
                          fluidRow(
                            style = "margin-top: 250px;",  # Jarak agar tidak terlalu mepet
                            column(12, 
                                   div(class = "card", style = "min-height: 850px;",  # Tambah tinggi card
                                       div(class = "card-header", "Hasil Konten Edukasi"),
                                       div(class = "card-body",
                                           
                                           # Grid 3 kolom untuk 3 video Reels
                                           fluidRow(
                                             
                                             # Reels 1: Data Pribadi yang Tidak Boleh Disebar
                                             column(4, align = "center",
                                                    tags$iframe(
                                                      src = "https://www.instagram.com/reel/DGNDqUcyVu4/embed/",
                                                      width = "100%", height = "800px",
                                                      frameborder = "0", allowfullscreen = NA
                                                    ),
                                                    div(class = "info-box", style = "width: 100%; padding: 10px; 
                                 background: #f8f9fa; 
                                 border-radius: 8px; 
                                 box-shadow: 3px 3px 10px rgba(0,0,0,0.1);
                                 text-align: left; margin-left: -10px;",
                                                        h4("Data Pribadi yang Tidak Boleh Disebar", 
                                                           style = "margin-bottom: 5px; font-weight: bold; font-size: 18px;"),
                                                        p("Ketahui jenis data pribadi yang harus dijaga agar tidak disalahgunakan.",
                                                          style = "font-size: 14px; margin-bottom: 0px;")
                                                    )
                                             ),
                                             
                                             # Reels 2: Waspada Investasi dengan Pengembalian Tinggi
                                             column(4, align = "center",
                                                    tags$iframe(
                                                      src = "https://www.instagram.com/reel/DGcvCwSNRgj/embed/",
                                                      width = "100%", height = "800px",
                                                      frameborder = "0", allowfullscreen = NA
                                                    ),
                                                    div(class = "info-box", style = "width: 100%; padding: 10px; 
                                 background: #f8f9fa; 
                                 border-radius: 8px; 
                                 box-shadow: 3px 3px 10px rgba(0,0,0,0.1);
                                 text-align: left; margin-left: -10px;",
                                                        h4("Waspada Investasi dengan Pengembalian Tinggi", 
                                                           style = "margin-bottom: 5px; font-weight: bold; font-size: 18px;"),
                                                        p("Jangan mudah tergiur investasi bodong yang menjanjikan keuntungan tinggi dalam waktu singkat.",
                                                          style = "font-size: 14px; margin-bottom: 0px;")
                                                    )
                                             ),
                                             
                                             # Reels 3: Tips Terhindar Pinjol Ilegal
                                             column(4, align = "center",
                                                    tags$iframe(
                                                      src = "https://www.instagram.com/reel/DGxU98kNvJ8/embed/",
                                                      width = "100%", height = "800px",
                                                      frameborder = "0", allowfullscreen = NA
                                                    ),
                                                    div(class = "info-box", style = "width: 100%; padding: 10px; 
                                 background: #f8f9fa; 
                                 border-radius: 8px; 
                                 box-shadow: 3px 3px 10px rgba(0,0,0,0.1);
                                 text-align: left; margin-left: -10px;",
                                                        h4("Tips Terhindar Pinjol Ilegal", 
                                                           style = "margin-bottom: 5px; font-weight: bold; font-size: 18px;"),
                                                        p("Kenali ciri-ciri pinjol ilegal dan cara agar tidak terjebak dalam utang yang mencekik.",
                                                          style = "font-size: 14px; margin-bottom: 0px;")
                                                    )
                                             )
                                           )
                                       )
                                   )
                            )
                          )
                        )
               ),
               
               # Tab kedua: Podcast
               tabPanel("Podcast",
                        fluidPage(
                          br(),  # Tambahkan ini untuk memberi jarak
                          
                          # Card Pertama
                          fluidRow(
                            style = "margin-top: 250px;",  # Tambahkan jarak agar tidak bertumpuk
                            column(12, 
                                   div(class = "card",
                                       div(class = "card-header", "Podcast OJK Jawa Timur?"),
                                       div(class = "card-body",
                                           h2(style = "text-align: left; font-weight: bold;", class = "card-title", 
                                              "Apa itu Podcast OJK?"),
                                           p(HTML("Podcast <b>Cangkrukan</b> merupakan program audio dari OJK Jawa Timur yang bertujuan untuk 
                        meningkatkan literasi keuangan masyarakat melalui diskusi santai dan informatif. 
                        Dalam setiap episodenya, podcast ini menghadirkan berbagai topik menarik, mulai dari investasi yang aman, 
                        tips mengelola keuangan, hingga cara mengenali modus penipuan finansial. 
                        Dengan format obrolan yang ringan namun tetap edukatif, Podcast Cangkrukan menjadi media yang mudah diakses 
                        oleh berbagai kalangan, baik melalui platform streaming maupun media sosial.")),
                                           
                                           # Tambahan bagian waktu pelaksanaan
                                           h2(style = "text-align: left; font-weight: bold; margin-top: 20px;", 
                                              class = "card-title", "Kapan Pelaksanaan Podcast?"),
                                           p(HTML("<i class='fa fa-clock-o' style='color: #28a745;'></i> <b>Waktu:</b> Tiap Selasa, pukul 16.00 WIB <br>
                        <i class='fa fa-instagram' style='color: #d62976;'></i> <b>Tempat:</b> Live di Instagram <a href='https://www.instagram.com/ojkjawatimur' target='_blank' style='color: #d62976; text-decoration: none; font-weight: bold;'>@ojkjawatimur</a>"))
                                       )
                                   )
                            )
                          ),
                          
                          # Card Kedua
                          fluidRow(
                            style = "margin-top: 250px;",  # Tambahkan jarak agar tidak bertumpuk
                            column(12, 
                                   div(class = "card",
                                       div(class = "card-header", "Apa Saja yang Dibahas?"),
                                       div(class = "card-body",
                                           h2(style = "text-align: left; font-weight: bold;", class = "card-title", "Apa Saja yang Dibahas?"),
                                           p(HTML("Podcast OJK Jawa Timur 'Cangkrukan' membahas berbagai topik menarik seputar keuangan dan literasi finansial. Berikut beberapa episode yang bisa kamu dengarkan:  
                    <ol>
                    <li><b>Yuk, Jaga Data Pribadi Keuangan Kita! Jangan Asal Kasih Tahu!</b> – <a href='https://www.instagram.com/p/DFji-uYS5Oq/' target='_blank'>Dengarkan di sini</a></li>
                    <li><b>FOMO, YOLO, FOPO</b> – <a href='https://www.instagram.com/p/DF2GW7VyXLR/?img_index=1' target='_blank'>Dengarkan di sini</a></li>
                    <li><b>Ngapain sih Bank Diawasi? Jadi, Bank Aman atau Ilusi? Ini Jawabannya</b> – <a href='https://www.instagram.com/p/DGHbgpsykqZ/' target='_blank'>Dengarkan di sini</a></li>
                    <li><b>Mengapa Data Sektor Keuangan Penting? Manfaat bagi Masyarakat dan Pelaku Usaha</b> – <a href='https://www.instagram.com/p/DGeyotuSRoL/' target='_blank'>Dengarkan di sini</a></li>
                    </ol>"))
                                       )
                                   )
                            )
                          )
                        )
               )
             )
           )
  ),
  
  # About Page
  tabPanel("Developer",
           tabsetPanel(id = "penulis_tabs",
                       
                       tabPanel("Developer 1",
                                div(style = "width: 100%; margin: 0; padding: 0; display: flex; flex-direction: column; align-items: center; text-align: center;",
                                    
                                    # Gambar Author di atas
                                    div(
                                      style = "width: 100%; display: flex; justify-content: center; margin-top:0px; margin-bottom:0px",
                                      imageOutput("author1Image", width = "100%", height = "auto") 
                                    ),
                                    
                                    
                                    # Contact Me di bawah
                                    div(
                                      style = "position: absolute; bottom: -20px; right: 20px; text-align: right; background: white; padding: 5px; border-radius: 10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1);",
                                      h3("Contact Me!"),
                                      div(
                                        style = "display: flex; justify-content: center; gap: 10px;",
                                        
                                        # Gmail
                                        tags$a(
                                          href = "mailto:mawarjannahghaliyah@gmail.com", target = "_blank",
                                          imageOutput("gmailLogo", inline = TRUE, width = "25px")
                                        ),
                                        
                                        # LinkedIn
                                        tags$a(
                                          href = "https://www.linkedin.com/in/mawarjannah/", target = "_blank",
                                          imageOutput("linkedinLogo", inline = TRUE, width = "25px")
                                        ),
                                        
                                        # Instagram
                                        tags$a(
                                          href = "https://www.instagram.com/mawar_jannah", target = "_blank",
                                          imageOutput("instagramLogo", inline = TRUE, width = "25px")
                                        )
                                      )
                                    ),
                                    
                                    # Gambar Author di bawah
                                    div(
                                      style = "width: 100%; display: flex; justify-content: center; margin-top:0px;",
                                      imageOutput("author1Image_bawah", width = "100%", height = "auto") 
                                    )
                                )
                       ),
                       
                       
                       tabPanel("Developer 2",
                                div(style = "width: 100%; margin: 0; padding: 0; display: flex; flex-direction: column; align-items: center; text-align: center;",
                                    
                                    # Gambar Author di atas
                                    div(
                                      style = "width: 100%; display: flex; justify-content: center; margin-top:0px; margin-bottom:0px",
                                      imageOutput("author2Image", width = "100%", height = "auto") 
                                    ),
                                    
                                    
                                    # Contact Me di bawah
                                    div(
                                      style = "position: absolute; bottom: -20px; right: 20px; text-align: right; background: white; padding: 5px; border-radius: 10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1);",
                                      h3("Contact Me!"),
                                      div(
                                        style = "display: flex; justify-content: center; gap: 10px;",
                                        
                                        # Gmail
                                        tags$a(
                                          href = "mailto:fairuzafghan16@gmail.com", target = "_blank",
                                          imageOutput("gmailLogo2", inline = TRUE, width = "25px")
                                        ),
                                        
                                        # LinkedIn
                                        tags$a(
                                          href = "https://www.linkedin.com/in/fairuzafghanbahari/", target = "_blank",
                                          imageOutput("linkedinLogo2", inline = TRUE, width = "25px")
                                        ),
                                        
                                        # Instagram
                                        tags$a(
                                          href = "https://www.instagram.com/fairuzafgg", target = "_blank",
                                          imageOutput("instagramLogo2", inline = TRUE, width = "25px")
                                        )
                                      )
                                    )
                                )
                       )
           )
  )
  
)


# Define Server
server <- function(input, output, session) {
  data_reactive <- eventReactive(input$visualize, {
    req(input$file)
    file_path <- input$file$datapath
    
    # TABEL 1.34a
    PERKEMBANGAN_DPK <- as.numeric(unlist(read_excel(file_path, sheet = "DPK per Lok_1.34.a.", range = "G38:S38", col_names = FALSE)))
    
    # TABEL 2.1 KREDIT
    KREDIT_2.1 = as.numeric(unlist(read_excel(file_path, sheet = "BPR 2.1-2.6", range = "G6:S6", col_names = FALSE)))
    
    # TABEL 2.1 DPK
    DPK_2.1 = as.numeric(unlist(read_excel(file_path, sheet = "BPR 2.1-2.6", range = "G10:S10", col_names = FALSE)))
    
    
    list(
      # TABEL 3.13.a
      SERIES_313a = as.numeric(unlist(read_excel(file_path, sheet = "Kredit per Lok. Dati I_3.13.a", range = "H73:T73", col_names = FALSE))),
      KREDIT_3.13a = as.numeric(unlist(read_excel(file_path, sheet = "Kredit per Lok. Dati I_3.13.a", range = "H74:T74", col_names = FALSE))),
      
      #TABEL 3.6
      P13 = read_excel(file_path, sheet = "Kredit JP-OP_KBMI 3.6.-3.10.", range = "P13", col_names = FALSE)[[1]],
      Q13 = read_excel(file_path, sheet = "Kredit JP-OP_KBMI 3.6.-3.10.", range = "Q13", col_names = FALSE)[[1]],
      
      P7 = read_excel(file_path, sheet = "Kredit JP-OP_KBMI 3.6.-3.10.", range = "P7", col_names = FALSE)[[1]],
      P9 = read_excel(file_path, sheet = "Kredit JP-OP_KBMI 3.6.-3.10.", range = "P9", col_names = FALSE)[[1]],
      P11 = read_excel(file_path, sheet = "Kredit JP-OP_KBMI 3.6.-3.10.", range = "P11", col_names = FALSE)[[1]],
      
      Q7 = read_excel(file_path, sheet = "Kredit JP-OP_KBMI 3.6.-3.10.", range = "Q7", col_names = FALSE)[[1]],
      Q9 = read_excel(file_path, sheet = "Kredit JP-OP_KBMI 3.6.-3.10.", range = "Q9", col_names = FALSE)[[1]],
      Q11 = read_excel(file_path, sheet = "Kredit JP-OP_KBMI 3.6.-3.10.", range = "Q11", col_names = FALSE)[[1]],
      
      # TABEL 1.34a
      PERKEMBANGAN_DPK = PERKEMBANGAN_DPK,
      
      # TABEL 1.28a
      GIRO_J5 = read_excel(file_path, sheet = "Komp.DPK_1.28.a.-1.32.a.", range = "J5", col_names = FALSE)[[1]],
      TABUNGAN_J8 = read_excel(file_path, sheet = "Komp.DPK_1.28.a.-1.32.a.", range = "J8", col_names = FALSE)[[1]],
      DEPOSITO_J11 = read_excel(file_path, sheet = "Komp.DPK_1.28.a.-1.32.a.", range = "J11", col_names = FALSE)[[1]],
      
      #TABEL 1.35a
      GIRO_135a = as.numeric(unlist(read_excel(file_path, sheet = "SB DPK_1.35.a.-1.39.a.", range = "F5:R5", col_names = FALSE))),
      TABUNGAN_135a = as.numeric(unlist(read_excel(file_path, sheet = "SB DPK_1.35.a.-1.39.a.", range = "F8:R8", col_names = FALSE))),
      DEPOSITO_135a = as.numeric(unlist(read_excel(file_path, sheet = "SB DPK_1.35.a.-1.39.a.", range = "F21:R21", col_names = FALSE))),
      
      #TABEL 1.41a
      INVESTASI_141a = as.numeric(unlist(read_excel(file_path, sheet = "SB per JPOP_1.41.a.", range = "G10:S10", col_names = FALSE))),
      KONSUMSI_141a = as.numeric(unlist(read_excel(file_path, sheet = "SB per JPOP_1.41.a.", range = "G13:S13", col_names = FALSE))),
      MODALKERJA_141a = as.numeric(unlist(read_excel(file_path, sheet = "SB per JPOP_1.41.a.", range = "G7:S7", col_names = FALSE))),
      
      #TABEL 1.16a
      CAR = read_excel(file_path, sheet = "Kinerja_1.16.a.-1.20.a.", range = "T4", col_names = FALSE)[[1]],
      BOPO = read_excel(file_path, sheet = "Kinerja_1.16.a.-1.20.a.", range = "T13", col_names = FALSE)[[1]],
      LDR = read_excel(file_path, sheet = "Kinerja_1.16.a.-1.20.a.", range = "T19", col_names = FALSE)[[1]],
      NIM = read_excel(file_path, sheet = "Kinerja_1.16.a.-1.20.a.", range = "T16", col_names = FALSE)[[1]],
      ROA = read_excel(file_path, sheet = "Kinerja_1.16.a.-1.20.a.", range = "T10", col_names = FALSE)[[1]],
      KREDIT_1.16a = read_excel(file_path, sheet = "Kredit per Lok. Dati I_3.13.a", range = "T74", col_names = FALSE)[[1]],
      TOTAL_1.16a = read_excel(file_path, sheet = "Kredit per Lok. Dati I_3.13.a", range = "T73", col_names = FALSE)[[1]],
      
      #TABEL 3.13a
      SERIES_313a = as.numeric(unlist(read_excel(file_path, sheet = "Kredit per Lok. Dati I_3.13.a", range = "H73:T73", col_names = FALSE))),
      KREDIT_3.13a = as.numeric(unlist(read_excel(file_path, sheet = "Kredit per Lok. Dati I_3.13.a", range = "H74:T74", col_names = FALSE))),
      
      #TABEL 2.1 KREDIT DAN DPK
      KREDIT_2.1 = KREDIT_2.1,
      DPK_2.1 = DPK_2.1,
      
      #TABEL 3.17.a
      S25 = read_excel(file_path, sheet = "Kredit BPR per JP_3.17.a.", range = "S25", col_names = FALSE)[[1]],
      S46 = read_excel(file_path, sheet = "Kredit BPR per JP_3.17.a.", range = "S46", col_names = FALSE)[[1]],
      S51 = read_excel(file_path, sheet = "Kredit BPR per JP_3.17.a.", range = "S51", col_names = FALSE)[[1]],
      S52 = read_excel(file_path, sheet = "Kredit BPR per JP_3.17.a.", range = "S52", col_names = FALSE)[[1]],
      
      #TABEL 2.7
      K6 = read_excel(file_path, sheet = "Komp DPK BPR_2.7", range = "K6", col_names = FALSE)[[1]],
      K7 = read_excel(file_path, sheet = "Komp DPK BPR_2.7", range = "K7", col_names = FALSE)[[1]],
      K8 = read_excel(file_path, sheet = "Komp DPK BPR_2.7", range = "K8", col_names = FALSE)[[1]]
      
    )
  })
  
  # PERHITUNGAN NPL 3.13.a
  NPL_313a <- reactive({
    req(data_reactive())  
    SERIES_313a <- data_reactive()$SERIES_313a
    KREDIT_3.13a <- data_reactive()$KREDIT_3.13a
    
    valid_index <- !is.na(SERIES_313a) & !is.na(KREDIT_3.13a) & KREDIT_3.13a != 0
    
    if (any(valid_index)) {
      result <- rep(NA, length(SERIES_313a))
      result[valid_index] <- (KREDIT_3.13a[valid_index] / SERIES_313a[valid_index]) * 100
      return(result)
    } else {
      return(rep(NA, length(SERIES_313a)))  
    }
  })
  
  # PERHITUNGAN 3.6
  calc_percentage <- function(numerator, denominator) {
    if (is.na(numerator) || is.na(denominator) || denominator == 0) {
      return(NA)
    } else {
      return((numerator / denominator) * 100)
    }
  }
  
  # Perhitungan NPL 1.16a
  NPL_116a <- reactive({
    req(data_reactive())  
    KREDIT_1.16a <- data_reactive()$KREDIT_1.16a
    TOTAL_1.16a <- data_reactive()$TOTAL_1.16a
    
    if (is.na(KREDIT_1.16a) || is.na(TOTAL_1.16a) || TOTAL_1.16a == 0) {
      return(NA)  
    } else {
      return((KREDIT_1.16a / TOTAL_1.16a) * 100)
    }
  })
  
  MK_before <- reactive({ calc_percentage(data_reactive()$P7, data_reactive()$P13) })
  Inv_before <- reactive({ calc_percentage(data_reactive()$P9, data_reactive()$P13) })
  Kon_before <- reactive({ calc_percentage(data_reactive()$P11, data_reactive()$P13) })
  
  MK_after <- reactive({ calc_percentage(data_reactive()$Q7, data_reactive()$Q13) })
  Inv_after <- reactive({ calc_percentage(data_reactive()$Q9, data_reactive()$Q13) })
  Kon_after <- reactive({ calc_percentage(data_reactive()$Q11, data_reactive()$Q13) })
  
  
  # Perhitungan Modal Kerja 3.17
  MK317 <- reactive({
    req(data_reactive())  
    S25 <- data_reactive()$S25
    S52 <- data_reactive()$S52
    
    if (is.na(S25) || is.na(S52) || S52 == 0) {
      return(NA)  
    } else {
      return((S25 / S52) * 100)
    }
  })
  
  # Perhitungan Investasi 3.17
  INV317 <- reactive({
    req(data_reactive())  
    S46 <- data_reactive()$S46
    S52 <- data_reactive()$S52
    
    if (is.na(S46) || is.na(S52) || S52 == 0) {
      return(NA)  
    } else {
      return((S46 / S52) * 100)
    }
  })
  
  # Perhitungan Konsumsi 3.17
  KON317 <- reactive({
    req(data_reactive())  
    S51 <- data_reactive()$S51
    S52 <- data_reactive()$S52
    
    if (is.na(S51) || is.na(S52) || S52 == 0) {
      return(NA)  
    } else {
      return((S51 / S52) * 100)
    }
  })
  
  # Perhitungan Deposito 2.7
  Depo27 <- reactive({
    req(data_reactive())  
    K6 <- data_reactive()$K6
    K8 <- data_reactive()$K8
    
    if (is.na(K6) || is.na(K8) || K8 == 0) {
      return(NA)  
    } else {
      return((K6 / K8) * 100)
    }
  })
  
  # Perhitungan Tabungan 2.7
  Tabungan27 <- reactive({
    req(data_reactive())  
    K7 <- data_reactive()$K7
    K8 <- data_reactive()$K8
    
    if (is.na(K7) || is.na(K8) || K8 == 0) {
      return(NA)  
    } else {
      return((K7 / K8) * 100)
    }
  })
  
  
  # BULAN AGAR OTOMATIS
  generate_month_labels <- function(month, year) {
    end_date <- as.Date(paste("01", month, year), format = "%d %B %Y")
    months <- seq(end_date, length.out = 13, by = "-1 month")
    month_labels <- rev(format(months, "%b %Y"))
    return(month_labels)
  }
  
  generate_1month_labels <- function(month, year) {
    end_date <- as.Date(paste("01", month, year), format = "%d %B %Y")
    months <- seq(end_date, length.out = 2, by = "-1 month")
    month_labels <- rev(format(months, "%B %Y"))
    return(month_labels)
  }
  
  generate_month_labels2 <- function(month, year) {
    end_date <- as.Date(paste("01", month, year), format = "%d %B %Y")
    months <- seq(end_date, length.out = 1, by = "-1 month")
    month_labels <- rev(format(months, "%B %Y"))
    return(month_labels)
  }
  
  # OUTPUT CHART 1
  output$combo_chart1 <- renderHighchart({
    req(data_reactive())
    df <- data.frame(
      Periode = seq_along(data_reactive()$SERIES_313a),
      SERIES = data_reactive()$SERIES_313a,
      NPL = NPL_313a()
    )
    
    months <- generate_month_labels(input$month, input$year)
    
    df$SERIES_PERCENT <- ifelse(df$SERIES == 0, NA, round((df$SERIES) * 100, 0))
    df$NPL <- round(df$NPL, 2)
    
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_xAxis(categories = months) %>%
      hc_yAxis_multiples(
        list(title = list(text = "SERIES (%)"), opposite = TRUE),
        list(title = list(text = "NPL"), opposite = FALSE)
      ) %>%
      hc_add_series(name = "SERIES", data = df$SERIES_PERCENT, type = "column", yAxis = 0, stacking = "percent",
                    dataLabels = list(enabled = TRUE, rotation = -90, align = "right", format = "{point.y}")) %>%
      hc_add_series(name = "NPL", data = df$NPL, type = "line", yAxis = 1)
    
  })
  
  # OUTPUT CHART 2
  output$combo_chart2 <- renderHighchart({
    req(MK_before(),Inv_before(),Kon_before(),MK_after(),Inv_after(),Kon_after())
    months <- generate_1month_labels(input$month, input$year)
    
    highchart() %>%
      hc_chart(type = "pie") %>%
      hc_add_series(
        data = list(
          list(name = "Modal Kerja", y = MK_before(), color = "#a3a1fb", size = "80%"),
          list(name = "Investasi", y = Inv_before(), color = "#f78da7", size = "80%"),
          list(name = "Konsumsi", y = Kon_before(), color = "#bdbdbd", size = "80%")
        ),
        dataLabels = list(
          enabled = TRUE, 
          format = "<div style='border-radius: 50%; background-color: white; padding: 5px;'>{point.y:.2f}%</div>"
        ),
        size = "60%",
        innerSize = "40%",
        name = months[1],  # Menggunakan bulan sebelumnya
        borderWidth = 10,  # Menambahkan lebar border untuk jarak
        borderColor = "#ffffff"  # Menambahkan border putih untuk jarak
      ) %>%
      hc_add_series(
        data = list(
          list(name = "Modal Kerja", y = MK_after(), color = "#f7a35c", size = "100%"),
          list(name = "Investasi", y = Inv_after(), color = "#90ed7d", size = "100%"),
          list(name = "Konsumsi", y = Kon_after(), color = "#7cb5ec", size = "100%")
        ),
        dataLabels = list(
          enabled = TRUE, 
          format = "<div style='border-radius: 50%; background-color: white; padding: 5px;'>{point.y:.2f}%</div>"
        ),
        size = "100%",
        innerSize = "60%",
        name = months[2],  # Menggunakan bulan inputan user
        borderWidth = 10,  # Menambahkan lebar border untuk jarak
        borderColor = "#ffffff"  # Menambahkan border putih untuk jarak
      )
  })
  
  # OUTPUT CHART 3
  output$combo_chart3 <- renderHighchart({
    req(data_reactive())
    data <- data_reactive()
    
    months <- generate_month_labels(input$month, input$year)
    
    
    data$PERKEMBANGAN_DPK <- ifelse(data$PERKEMBANGAN_DPK == 0, NA, round((data$PERKEMBANGAN_DPK), 0))
    
    min_value <- min(data$PERKEMBANGAN_DPK, na.rm = TRUE)
    yaxis_min <- floor(min_value / 100000) * 100000 - 50000
    maks_value <- max(data$PERKEMBANGAN_DPK, na.rm = TRUE)
    yaxis_maks <- ceiling(maks_value / 100000) * 100000 + 50000
    
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_xAxis(categories = months) %>%
      hc_yAxis(title = list(text = "Nilai Dana (Rupiah)"), min = yaxis_min, max = yaxis_maks) %>%
      hc_add_series(
        name = "Dana Pihak Ketiga",
        data = data$PERKEMBANGAN_DPK,
        color = "#8FBC8F",
        dataLabels = list(
          enabled = TRUE,
          style = list(fontSize = "12px", fontWeight = "bold", color = "#FFFFFF"),
          rotation = -90,
          y = 0,
          align = "center"
        )
      ) %>%
      hc_legend(enabled = TRUE) %>%
      hc_exporting(enabled = TRUE)
  })
  
  # OUTPUT CHART 4
  output$combo_chart4 <- renderHighchart({
    req(data_reactive())
    data <- data_reactive()
    
    data$GIRO_J5 <- ifelse(data$GIRO_J5 == 0, NA, round((data$GIRO_J5), 0))
    data$TABUNGAN_J8 <- ifelse(data$TABUNGAN_J8 == 0, NA, round((data$TABUNGAN_J8), 0))
    data$DEPOSITO_J11 <- ifelse(data$DEPOSITO_J11 == 0, NA, round((data$DEPOSITO_J11), 0))
    
    categories <- c("Giro", "Tabungan", "Deposito")
    values <- c(data$GIRO_J5, data$TABUNGAN_J8, data$DEPOSITO_J11)
    
    min_value <- min(values, na.rm = TRUE)
    yaxis_min <- floor(min_value / 100000) * 100000 - 50000
    maks_value <- max(values, na.rm = TRUE)
    yaxis_maks <- ceiling(maks_value / 100000) * 100000 + 50000
    
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_xAxis(categories = categories) %>%
      hc_yAxis(title = list(text = "Nilai Dana (Rupiah)"), min = yaxis_min, max = yaxis_maks) %>%
      hc_add_series(
        name = "Komposisi DPK",
        data = values,
        colorByPoint = TRUE,
        dataLabels = list(
          enabled = TRUE,
          style = list(fontSize = "12px", fontWeight = "bold", color = "#FFFFFF"),
          y = 0,
          align = "center"
        )
      ) %>%
      hc_legend(enabled = TRUE) %>%
      hc_exporting(enabled = TRUE)
  })
  
  # OUTPUT CHART 5
  output$combo_chart5 <- renderHighchart({
    req(data_reactive())
    df <- data.frame(
      GIRO = data_reactive()$GIRO_135a,
      TABUNGAN = data_reactive()$TABUNGAN_135a,
      DEPOSITO = data_reactive()$DEPOSITO_135a
      
    )
    
    df$GIRO <- round(df$GIRO, 2)
    df$TABUNGAN <- round(df$TABUNGAN, 2)
    df$DEPOSITO <- round(df$DEPOSITO, 2)
    
    months <- generate_month_labels(input$month, input$year)
    
    highchart() %>%
      hc_xAxis(categories = months) %>%
      hc_yAxis(title = list(text = "Values")) %>%
      hc_add_series(name = "GIRO", data = df$GIRO, type = "line", marker = list(enabled = TRUE)) %>%
      hc_add_series(name = "TABUNGAN", data = df$TABUNGAN, type = "line", marker = list(enabled = TRUE)) %>%
      hc_add_series(name = "DEPOSITO", data = df$DEPOSITO, type = "line", marker = list(enabled = TRUE)) %>%
      hc_legend(enabled = TRUE)
  })
  
  
  # OUTPUT CHART 6
  output$combo_chart6 <- renderHighchart({
    req(data_reactive())
    df <- data.frame(
      INVESTASI = data_reactive()$INVESTASI_141a,
      KONSUMSI = data_reactive()$KONSUMSI_141a,
      MODALKERJA = data_reactive()$MODALKERJA_141a
      
    )
    
    df$INVESTASI <- round(df$INVESTASI, 2)
    df$KONSUMSI <- round(df$KONSUMSI, 2)
    df$MODALKERJA <- round(df$MODALKERJA, 2)
    
    months <- generate_month_labels(input$month, input$year)
    
    highchart() %>%
      hc_xAxis(categories = months) %>%
      hc_yAxis(title = list(text = "Values")) %>%
      hc_add_series(name = "INVESTASI", data = df$INVESTASI, type = "line", marker = list(enabled = TRUE)) %>%
      hc_add_series(name = "KONSUMSI", data = df$KONSUMSI, type = "line", marker = list(enabled = TRUE)) %>%
      hc_add_series(name = "MODAL KERJA", data = df$MODALKERJA, type = "line", marker = list(enabled = TRUE)) %>%
      hc_legend(enabled = TRUE)
  })
  
  # Menampilkan indikator kinerja
  output$car <- renderValueBox({ 
    req(data_reactive())
    bulan <- generate_month_labels2(input$month, input$year)[1]  
    
    valueBox(
      tags$div(
        tags$p("CAR", style = "font-size: 21px; font-weight: bold; color:white; margin: 5px 0px;"),
        tags$p(bulan, style = "font-size: 16px; color:white; font-weight: normal; margin: 5px 0px;"),
        tags$p(paste0(round(data_reactive()$CAR, 2), " %"), style = "font-size: 21px; font-weight: bold; margin: 5px 0px;"),
        style = "border-radius: 50%; background-color: #0073e6; color: white; text-align: center; padding: 15px; height: auto; border: 1px solid black;"
      ),
      "",
      color = #FFFFFF
    ) 
  })
  
  output$bopo <- renderValueBox({ 
    req(data_reactive())
    bulan <- generate_month_labels2(input$month, input$year)[1]  
    
    valueBox(
      tags$div(
        tags$p("BOPO", style = "font-size: 21px; font-weight: bold; color:white; margin: 5px 0px;"),
        tags$p(bulan, style = "font-size: 16px; color:white; font-weight: normal; margin: 5px 0px;"),
        tags$p(paste0(round(data_reactive()$BOPO, 2), " %"), style = "font-size: 21px; font-weight: bold; margin: 5px 0px;"),
        style = "border-radius: 50%; background-color: #28a745; color: white; text-align: center; padding: 15px; height: auto; border: 1px solid black;"
      ),
      "",
      color = #FFFFFF
    ) 
  })
  
  output$ldr <- renderValueBox({ 
    req(data_reactive())
    bulan <- generate_month_labels2(input$month, input$year)[1]  
    
    valueBox(
      tags$div(
        tags$p("LDR", style = "font-size: 21px; font-weight: bold; color:white; margin: 5px 0px;"),
        tags$p(bulan, style = "font-size: 16px; color:white; font-weight: normal; margin: 5px 0px;"),
        tags$p(paste0(round(data_reactive()$LDR, 2), " %"), style = "font-size: 21px; font-weight: bold; margin: 5px 0px;"),
        style = "border-radius: 50%; background-color: #fd7e14; color: white; text-align: center; padding: 15px; height: auto; border: 1px solid black;"
      ),
      "",
      color = #FFFFFF
    ) 
  })
  
  output$nim <- renderValueBox({ 
    req(data_reactive())
    bulan <- generate_month_labels2(input$month, input$year)[1]  
    
    valueBox(
      tags$div(
        tags$p("NIM", style = "font-size: 21px; font-weight: bold; color:white; margin: 5px 0px;"),
        tags$p(bulan, style = "font-size: 16px; color:white; font-weight: normal; margin: 5px 0px;"),
        tags$p(paste0(round(data_reactive()$NIM, 2), " %"), style = "font-size: 21px; font-weight: bold; margin: 5px 0px;"),
        style = "border-radius: 50%; background-color: #dc3545; color: white; text-align: center; padding: 15px; height: auto; border: 1px solid black;"
      ),
      "",
      color = #FFFFFF
    ) 
  })
  
  output$roa <- renderValueBox({ 
    req(data_reactive())
    bulan <- generate_month_labels2(input$month, input$year)[1]  
    
    valueBox(
      tags$div(
        tags$p("ROA", style = "font-size: 21px; font-weight: bold; color:white; margin: 5px 0px;"),
        tags$p(bulan, style = "font-size: 16px; color:white; font-weight: normal; margin: 5px 0px;"),
        tags$p(paste0(round(data_reactive()$ROA, 2), " %"), style = "font-size: 21px; font-weight: bold; margin: 5px 0px;"),
        style = "border-radius: 50%; background-color: #6f42c1; color: white; text-align: center; padding: 15px; height: auto; border: 1px solid black;"
      ),
      "",
      color = #FFFFFF
    ) 
  })
  
  output$npl_116a <- renderValueBox({ 
    req(NPL_116a())
    bulan <- generate_month_labels2(input$month, input$year)[1]  
    
    valueBox(
      tags$div(
        tags$p("NPL", style = "font-size: 21px; font-weight: bold; color:white; margin: 5px 0px;"),
        tags$p(bulan, style = "font-size: 16px; color:white; font-weight: normal; margin: 5px 0px;"),
        tags$p(paste0(round(NPL_116a(), 2), " %"), style = "font-size: 21px; font-weight: bold; margin: 5px 0px;"),
        style = "border-radius: 50%; background-color: black; color: white; text-align: center; padding: 15px; height: auto; border: 1px solid black;"
      ),
      "",
      color = #FFFFFF
    ) 
  })
  
  # OUTPUT CHART 7
  output$combo_chart7 <- renderHighchart({
    req(data_reactive())
    data <- data_reactive()
    
    months <- generate_month_labels(input$month, input$year)
    
    data$KREDIT_2.1 <- ifelse(data$KREDIT_2.1 == 0, NA, round((data$KREDIT_2.1), 0))
    
    min_value <- min(data$KREDIT_2.1, na.rm = TRUE)
    yaxis_min <- floor(min_value / 10000) * 10000 - 5000
    
    maks_value <- max(data$KREDIT_2.1, na.rm = TRUE)
    yaxis_max <- ceiling(maks_value / 10000) * 10000 + 5000
    
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_xAxis(categories = months) %>%
      hc_yAxis(
        title = list(text = "Nilai Dana (Rupiah)"),
        min = yaxis_min,
        max = yaxis_max
      ) %>%
      hc_add_series(
        name = "Dana Pihak Ketiga",
        data = data$KREDIT_2.1,
        color = "#8FBC8F",
        dataLabels = list(
          enabled = TRUE,
          style = list(fontSize = "12px", fontWeight = "bold", color = "#FFFFFF"),
          rotation = -90,
          y = 0,
          align = "center"
        )
      ) %>%
      hc_legend(enabled = TRUE) %>%
      hc_exporting(enabled = TRUE)
  })
  
  # OUTPUT CHART 8
  output$combo_chart8 <- renderHighchart({
    req(MK317(), INV317(), KON317())  
    
    data_pie <- data.frame(
      Kategori = c("Modal Kerja", "Investasi", "Konsumsi"),
      Persentase = c(MK317(), INV317(), KON317())
    )
    
    highchart() %>%
      hc_chart(type = "pie") %>%
      hc_series(list(
        name = "Persentase",
        data = list(
          list(name = "Modal Kerja", y = MK317(), color = "#7cb5ec"),
          list(name = "Investasi", y = INV317(), color = "yellow"),
          list(name = "Konsumsi", y = KON317(), color = "#90ed7d")
        )
      )) %>%
      hc_plotOptions(pie = list(
        dataLabels = list(enabled = TRUE, format = "{point.name}: {point.y:.2f}%")
      ))
  })
  
  # OUTPUT CHART 9
  output$combo_chart9 <- renderHighchart({
    req(data_reactive())
    data <- data_reactive()
    
    months <- generate_month_labels(input$month, input$year)
    
    data$DPK_2.1 <- ifelse(data$DPK_2.1 == 0, NA, round((data$DPK_2.1), 0))
    
    min_value <- min(data$DPK_2.1, na.rm = TRUE)
    yaxis_min <- floor(min_value / 10000) * 10000 - 5000
    
    maks_value <- max(data$DPK_2.1, na.rm = TRUE)
    yaxis_max <- ceiling(maks_value / 10000) * 10000 + 5000
    
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_xAxis(categories = months) %>%
      hc_yAxis(
        title = list(text = "Nilai Dana (Rupiah)"),
        min = yaxis_min,
        max = yaxis_max
      ) %>%
      hc_add_series(
        name = "Dana Pihak Ketiga",
        data = data$DPK_2.1,
        color = "#8FBC8F",
        dataLabels = list(
          enabled = TRUE,
          style = list(fontSize = "12px", fontWeight = "bold", color = "#FFFFFF"),
          rotation = -90,
          y = 0,
          align = "center"
        )
      ) %>%
      hc_legend(enabled = TRUE) %>%
      hc_exporting(enabled = TRUE)
  })
  
  # OUTPUT CHART 10
  output$combo_chart10 <- renderHighchart({
    req(Depo27(), Tabungan27())  
    
    data_pie <- data.frame(
      Kategori = c("Deposito", "Tabungan"),
      Persentase = c(Depo27(), Tabungan27())
    )
    
    highchart() %>%
      hc_chart(type = "pie") %>%
      hc_series(list(
        name = "Persentase",
        data = list(
          list(name = "Deposito", y = Depo27(), color = "#7cb5ec"),
          list(name = "Tabungan", y = Tabungan27(), color = "yellow")
        )
      )) %>%
      hc_plotOptions(pie = list(
        dataLabels = list(enabled = TRUE, format = "{point.name}: {point.y:.2f}%")
      ))
  })
  
  
  #ARSIP LANTAI 6
  output$table1 <- renderDT({
    datatable(df_raw(), options = list(pageLength = 10, scrollX = TRUE), editable = FALSE, selection = 'single')
  })
  
  # Password Admin
  admin_password_arsip <- "ARSIPLANTAI6"
  
  # File Pengarsipan OJk
  arsip_ojk <- "Pengarsipan OJK.csv"
  
  # Pastikan file ada atau buat default
  if (!file.exists(arsip_ojk)) {
    write_csv(data.frame(
      "KODE-KLASIFIKASI" = character(),
      "TENTANG-PERIHAL" = character(),
      "STATUS" = character(),
      stringsAsFactors = FALSE
    ), arsip_ojk)
  }
  
  # Reactive Value untuk database
  df_raw <- reactiveVal({
    if (file.exists(arsip_ojk)) {
      read_csv(arsip_ojk, show_col_types = FALSE) %>% as.data.frame()
    } else {
      data.frame(
        "KODE-KLASIFIKASI" = character(),
        "TENTANG-PERIHAL" = character(),
        "STATUS" = character(),
        stringsAsFactors = FALSE
      )
    }
  })
  
  # Tambah Data Baru dengan Validasi
  observeEvent(input$addData, {
    # Cek apakah ada inputan yang kosong
    if (is.null(input$kata_kunci) || input$kata_kunci == "" ||
        is.null(input$kode_klasifikasi) || input$kode_klasifikasi == "" ||
        is.null(input$tentang_perihal) || input$tentang_perihal == "" ||
        is.null(input$status) || input$status == "") {
      
      showModal(modalDialog(
        title = "Peringatan",
        "Harap melengkapi semua isian terlebih dahulu sebelum mengedit.",
        easyClose = TRUE
      ))
      return()  # Hentikan proses jika inputan tidak lengkap
    }
    
    # Verifikasi password
    if (input$kata_kunci != admin_password_arsip) {
      showModal(modalDialog(
        title = "Akses Ditolak",
        "Kata kunci salah! Anda tidak memiliki izin.",
        easyClose = TRUE
      ))
      return()  # Hentikan proses jika kata kunci salah
    }
    
    # Jika lolos validasi, lanjut menambahkan data
    current_data <- df_raw()
    
    new_entry <- data.frame(
      "KODE-KLASIFIKASI" = input$kode_klasifikasi, 
      "TENTANG-PERIHAL" = input$tentang_perihal, 
      "STATUS" = input$status,
      stringsAsFactors = FALSE
    )
    
    updated_data <- bind_rows(current_data, new_entry)  # Tambahkan data baru ke dalam database
    
    df_raw(updated_data)  # Update reactive value
    write_csv(updated_data, arsip_ojk)  # Simpan ke file CSV agar permanen
  })
  
  #Tombol mata pada kata kunci
  # Gunakan reactiveValues untuk menyimpan status input password
  rv <- reactiveValues(show_pw = FALSE)  
  
  # Render ulang input password saat tombol diklik
  output$password_ui <- renderUI({
    if (rv$show_pw) {
      textInput("kata_kunci", label = NULL, value = input$kata_kunci)  # Mode teks terlihat
    } else {
      passwordInput("kata_kunci", label = NULL, value = input$kata_kunci)  # Mode password tersembunyi
    }
  })
  
  # Event tombol toggle
  observeEvent(input$toggle_pw, {
    rv$show_pw <- !rv$show_pw  # Ubah status
    new_icon <- ifelse(rv$show_pw, "eye", "eye-slash")  
    updateActionButton(session, "toggle_pw", icon = icon(new_icon))  # Ganti ikon sesuai status
  })
  
  
  # Hapus Data Terpilih dengan validasi tambahan
  observeEvent(input$deleteData, {
    # Cek apakah kata kunci telah dimasukkan
    if (is.null(input$kata_kunci) || input$kata_kunci == "") {
      showModal(modalDialog(
        title = "Akses Ditolak",
        "Masukkan kata kunci sebelum mengedit tabel.",
        easyClose = TRUE
      ))
      return()  # Hentikan proses jika tidak ada kata kunci
    }
    
    # Verifikasi apakah kata kunci benar
    if (input$kata_kunci != admin_password_arsip) {
      showModal(modalDialog(
        title = "Akses Ditolak",
        "Kata kunci salah! Anda tidak memiliki izin.",
        easyClose = TRUE
      ))
      return()  # Hentikan proses jika kata kunci salah
    }
    
    # Cek apakah ada baris yang dipilih untuk dihapus
    selected_row <- input$table1_rows_selected
    if (length(selected_row) == 0) {
      showModal(modalDialog(
        title = "Error",
        "Harap pilih baris yang ingin dihapus.",
        easyClose = TRUE
      ))
      return()  # Hentikan proses jika tidak ada baris yang dipilih
    }
    
    # Jika semua validasi lolos, hapus data
    df_temp <- df_raw()
    df_temp <- df_temp[-selected_row, ]
    df_raw(df_temp)
    write_csv(df_temp, arsip_ojk)  # Simpan perubahan ke file CSV
  })
  
  # Download Data
  output$downloadData <- downloadHandler(
    filename = function() { "Pengarsipan_OJK_Terbaru.csv" },
    content = function(file) {
      write_csv(df_raw(), file)
    }
  )
  
  
  # FAQ SLIK
  faq_file <- "faq_database.xlsx"
  
  # Pastikan file ada atau buat default
  if (!file.exists(faq_file)) {
    write_xlsx(data.frame(Pertanyaan = character(), Jawaban = character(), stringsAsFactors = FALSE), faq_file)
  }
  
  # Reactive Value untuk database
  faq_data <- reactiveVal({
    if (file.exists(faq_file)) {
      read_excel(faq_file) %>% as.data.frame()
    } else {
      data.frame(Pertanyaan = character(), Jawaban = character(), stringsAsFactors = FALSE)
    }
  })
  
  # Tambah pertanyaan
  observeEvent(input$submit_question, {
    req(input$new_question)
    
    current_data <- faq_data()
    new_entry <- data.frame(Pertanyaan = input$new_question, Jawaban = "", stringsAsFactors = FALSE)
    updated_data <- rbind(current_data, new_entry)
    
    faq_data(updated_data)
    write_xlsx(updated_data, faq_file)
    
    updateSelectInput(session, "select_question", choices = updated_data$Pertanyaan)
  })
  
  # Kata kunci yang valid
  admin_password <- "CALONPCSOJK10"
  
  # Tambah jawaban (hanya jika password benar)
  observeEvent(input$submit_answer, {
    req(input$select_question, input$answer_input, input$admin_key)
    
    if (input$admin_key != admin_password) {
      showModal(modalDialog(
        title = "Akses Ditolak",
        "Kata kunci salah! Anda tidak memiliki izin.",
        easyClose = TRUE
      ))
      return()  # Hentikan proses jika password salah
    }
    
    current_data <- faq_data()
    idx <- which(current_data$Pertanyaan == input$select_question)
    
    if (length(idx) > 0) {
      current_data$Jawaban[idx] <- input$answer_input
      faq_data(current_data)
      write_xlsx(current_data, faq_file)
    }
  })
  
  # Hapus pertanyaan (hanya jika password benar)
  observeEvent(input$delete_question, {
    # Periksa apakah password sudah diisi, jika belum tampilkan peringatan
    if (is.null(input$admin_key) || input$admin_key == "") {
      showModal(modalDialog(
        title = "Peringatan",
        "Harap masukkan kata kunci sebelum menghapus pertanyaan.",
        easyClose = TRUE
      ))
      return()
    }
    
    # Jika password salah, tampilkan peringatan dan hentikan proses
    if (input$admin_key != admin_password) {
      showModal(modalDialog(
        title = "Akses Ditolak",
        "Kata kunci salah! Anda tidak memiliki izin.",
        easyClose = TRUE
      ))
      return()
    }
    
    # Ambil data yang ada
    current_data <- faq_data()
    
    # Periksa apakah pertanyaan dipilih dari dropdown atau tabel
    selected_question <- input$select_question  # Dari dropdown
    selected_row <- input$faq_table_rows_selected  # Dari tabel (jika ada)
    
    if (!is.null(selected_row) && length(selected_row) > 0) {
      # Jika memilih dari tabel, ambil pertanyaan berdasarkan indeks yang dipilih
      selected_question <- current_data$Pertanyaan[selected_row]
    }
    
    # Jika tidak ada pertanyaan yang dipilih, tampilkan peringatan
    if (is.null(selected_question) || selected_question == "") {
      showModal(modalDialog(
        title = "Peringatan",
        "Silakan pilih pertanyaan yang ingin dihapus.",
        easyClose = TRUE
      ))
      return()
    }
    
    # Hapus pertanyaan yang dipilih
    updated_data <- current_data[!current_data$Pertanyaan %in% selected_question, ]
    
    # Simpan data yang diperbarui
    faq_data(updated_data)
    write_xlsx(updated_data, faq_file)
    
    # Perbarui daftar pertanyaan di dropdown
    updateSelectInput(session, "select_question", choices = c("", updated_data$Pertanyaan), selected = "")
  })
  
  # Render tabel FAQ dengan hanya satu baris yang bisa dipilih
  output$faq_table <- renderDT({
    datatable(faq_data(), options = list(pageLength = 5, autoWidth = TRUE),
              selection = 'single')  # Hanya bisa pilih satu baris
  })
  
  # Download database
  output$download_faq <- downloadHandler(
    filename = function() { "FAQ_Database.xlsx" },
    content = function(file) {
      write_xlsx(faq_data(), file)
    })
  
  output$table <- renderDT({
    datatable(data_kontak, options = list(pageLength = 10))  # Tampilkan tabel
  })
  
  
  # Struktur Organisasi
  output$organisasi <- renderImage({
    list(
      src="www/struktur_organisasi.jpg", style = "width: 100%; height: auto; border-radius: 10px;"
    )
  }, deleteFile = FALSE)
  
  # Data gambar untuk carousel
  output$image_carousel <- renderSlickR({
    images <- list(
      "www/1.png",
      "www/2.png",
      "www/3.png",
      "www/4.png",
      "www/5.png",
      "www/6.png",
      "www/7.png",
      "www/8.png",
      "www/9.png",
      "www/10.png"
    )
    
    slickR(images) + 
      settings(
        slidesToShow = 1, 
        slidesToScroll = 1, 
        infinite = TRUE,
        autoplay = TRUE,
        autoplaySpeed = 3000,
        arrows = TRUE,  # Aktifkan panah
        variableWidth = FALSE, 
        adaptiveHeight = FALSE,  # Pastikan tinggi menyesuaikan layar
        dots = TRUE  
      )
  })
  
  # Data gambar untuk carousel 2
  output$image_carousel1 <- renderSlickR({
    images <- list(
      "www/ipm.png",
      "www/kemiskinan.png",
      "www/rasiogini.png",
      "www/pdrb.png"
    )
    
    slickR(images) + 
      settings(
        slidesToShow = 1, 
        slidesToScroll = 1, 
        infinite = TRUE,
        autoplay = TRUE,
        autoplaySpeed = 10000,
        arrows = TRUE,  # Aktifkan panah
        variableWidth = FALSE, 
        adaptiveHeight = FALSE,  # Pastikan tinggi menyesuaikan layar
        dots = TRUE  
      )
  })
  
  #Gambar Hasil
  output$gambar1 <- renderImage({
    list(
      src = "www/lt6.png",  # Lokasi gambar
      width = "auto",
      height = "360",
      alt = "Gambar tampilan"
    )
  }, deleteFile = FALSE)
  
  output$gambar2 <- renderImage({
    list(
      src = "www/lt8.png",  # Lokasi gambar
      width = "auto",
      height = "360",
      alt = "Gambar tampilan"
    )
  }, deleteFile = FALSE)
  
  output$gambar3 <- renderImage({
    list(
      src = "www/lt1SLIK.jpeg",
      width = "auto",
      height = "360",
      alt = "Gambar tampilan"
    )
  }, deleteFile = FALSE)
  
  
  # Logo Dashboard
  output$logo <- renderImage({
    list(
      src="www/ojk logo.png", style = "height: 130px; margin-top: 6px; margin-left: 120px;"
    )
  }, deleteFile = FALSE)
  
  # Gambar Author 1
  output$author1Image <- renderImage({
    filename <- "www/Mawar_atas.png"  # Pastikan file ada di folder 'www'
    list(
      src = filename,
      contentType = "image/png",
      width = "100%"  # Sesuaikan ukuran lebar (px)
    )
  }, deleteFile = FALSE)
  
  output$author1Image_bawah <- renderImage({
    filename <- "www/Mawar_bawah.png"  # Pastikan file ada di folder 'www'
    list(
      src = filename,
      contentType = "image/png",
      width = "100%"  # Sesuaikan ukuran lebar (px)
    )
  }, deleteFile = FALSE)
  
  # Gambar Author 2
  output$author2Image <- renderImage({
    filename <- "www/Fairuz.png"  # Pastikan file ada di folder 'www'
    list(
      src = filename,
      contentType = "image/png",
      width = "100%"  # Sesuaikan ukuran lebar (px)
    )
  }, deleteFile = FALSE)
  
  # Gambar Map Kuping
  output$mapkuping <- renderImage({
    filename <- "www/map kuping.png"  # Pastikan file ada di folder 'www'
    list(
      src = filename,
      contentType = "image/png"
    )
  }, deleteFile = FALSE)
  
  # Logo Gmail
  output$gmailLogo <- renderImage({
    filename <- "www/Gmail.png"  # Path ke ikon Gmail
    list(
      src = filename,
      contentType = "image/png",
      width = 40,  # Ukuran lebar
      height = 40  # Ukuran tinggi
    )
  }, deleteFile = FALSE)
  
  # Logo LinkedIn
  output$linkedinLogo <- renderImage({
    filename <- "www/Linkedin.png"  # Path ke ikon LinkedIn
    list(
      src = filename,
      contentType = "image/png",
      width = 40,  # Ukuran lebar
      height = 40  # Ukuran tinggi
    )
  }, deleteFile = FALSE)
  
  # Logo Instagram
  output$instagramLogo <- renderImage({
    filename <- "www/Instagram.png"  # Path ke ikon Instagram
    list(
      src = filename,
      contentType = "image/png",
      width = 40,  # Ukuran lebar
      height = 40  # Ukuran tinggi
    )
  }, deleteFile = FALSE)
  
  # Logo Gmail2
  output$gmailLogo2 <- renderImage({
    filename <- "www/Gmail2.png"  # Path ke ikon Gmail
    list(
      src = filename,
      contentType = "image/png",
      width = 40,  # Ukuran lebar
      height = 40  # Ukuran tinggi
    )
  }, deleteFile = FALSE)
  
  # Logo LinkedIn2
  output$linkedinLogo2 <- renderImage({
    filename <- "www/Linkedin2.png"  # Path ke ikon LinkedIn
    list(
      src = filename,
      contentType = "image/png",
      width = 40,  # Ukuran lebar
      height = 40  # Ukuran tinggi
    )
  }, deleteFile = FALSE)
  
  # Logo Instagram2
  output$instagramLogo2 <- renderImage({
    filename <- "www/Instagram2.png"  # Path ke ikon Instagram
    list(
      src = filename,
      contentType = "image/png",
      width = 40,  # Ukuran lebar
      height = 40  # Ukuran tinggi
    )
  }, deleteFile = FALSE)
  
  
  
  
  #########################################################################################################################################
  
  
}

# Run Application
shinyApp(ui, server)

