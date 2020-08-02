##################################################################################################
#-----------------------------PROJECT DQ LAB ANALISIS COVID-19 INDONESIA-------------------------#
#--------------------------------------------JOKO ELIYANTO---------------------------------------#
##################################################################################################

#--------------------------------Install package jika belum tersedia-----------------------------#
#install.packages("httr")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("plotly")
#install.packages("hrbrthemes")
#install.packages("lubridate")
#install.packages("shiny")

#------------------------------------------------------------------------------------------------#


#------------------------------------------Mengaktifkan Library----------------------------------#
library(httr)
library(dplyr)
library(ggplot2)
library(plotly)
library(hrbrthemes)
library(lubridate)
library(shiny)
library(shinythemes)
library(tidyr)
#------------------------------------------------------------------------------------------------#
#-----------------Mengatur User Interface(UI) atau Tampilan Web Apps pada modul Shiny------------#

ui <- fluidPage(theme = shinytheme("cerulean"),      #Mengatur Tema Shiny
                
                #Tema lainnya bisa kunjungi: 
                #https://shiny.rstudio.com/gallery/shiny-theme-selector.html
                
                
                #Memberi nama bar
                navbarPage(
                  "Analisis COVID-19 Provinsi Jawa Barat",
                  tabPanel("Data COVID-19 JABAR",                    
                           sidebarPanel(
                             h4("DQ-LAB Weekend Project"),
                             h4("Sunday, 2 August 2020"),
                             h4("Author: Joko Eliyanto")),
                              
                           mainPanel(
                             h4("Data Hasil Crawling melalui API"), 
                             h3("TABEL DATA COVID-19 PROVINSI JAWA BARAT"),
                             DT::dataTableOutput("Tabel1"),
                             h3("TABEL DATA AKUMULATIF COVID-19 PROVINSI JAWA BARAT"),
                             DT::dataTableOutput("Tabel2")
                           )
                  ),
                  tabPanel("Plain Project Doc",                         
                           sidebarPanel(
                             h4("DQ-LAB Weekend Project"),
                             h4("Sunday, 2 August 2020"),
                             h4("Author: Joko Eliyanto")),
                           mainPanel(
                             h2("Analisis COVID-19 di Indonesia"),            
                             includeMarkdown("COVID19DQLAB.Rmd")               
                           )
                  ),
                  tabPanel("Plain Project R&A",                        
                           sidebarPanel(
                             h4("DQ-LAB Weekend Project"),
                             h4("Sunday, 2 August 2020"),
                             h4("Author: Joko Eliyanto")),
                           mainPanel(
                             h2("Analisis COVID-19 di Indonesia"),           
                             plotOutput(outputId = "G1"),#Harian
                             p("Menggunakan gambar di atas, ingin ditunjukkan
                               jumlah kasus perhari di Jawa Barat. Dari hasil
                               di atas, nampak jelas bahwa terjadi lonjakan
                               jumlah kasus harian pada bulan Juli.
                               Jumlah kasus tersebut bahkan jauh melampaui
                               jumlah kasus pada titik puncak pada hari-hari
                               lainnya"),
                             plotOutput(outputId = "G2"),#Sembuh
                             p("Jumlah pasien Covid-19 di Provinsi Jawa Barat
                               yang sembuh menunjukkan jumlah yang signifikan
                               pada akhir bulan Juli. Hal ini menjadi
                               pertanda baik, bahwa semakin banyak
                               jumlah pasien yang bisa sembuh dari
                               COVID-19 di Provinsi Jawa Barat"),
                             plotOutput(outputId = "G3"),#Meninggal
                             p("Jumlah pasien COVID-19 yang meninggal nampak
                               fluktuatif, namun mencapai puncaknya pada bulan
                               Mei. Jumlah kasus yang meninggal relatif menurun
                               pada 2 bulan terakhir. Ini menunjukkan bahwa
                               COVID-19 di Jawa Barat sudah mampu dikendalikan
                               oleh Pemerintah"),
                             plotOutput(outputId = "G4"),#Mingguan
                             p("Pada grafik di atas ditunjukkan jumlah kasus
                               positif mingguan di Provinsi Jawa Barat. Warna merah
                               berarti menunjukkan bahwa jumlah kasus pada
                               minggu tersebut melebih jumlah kasus pada minggu
                               sebelumnya, sedangkan warna hijau menujukkan
                               sebaliknya. Berdasaarkan gambar, terlihat bahwa
                               pada pekan ke 28 sejak penanganan COVID-19 dicatat di 
                               Provinsi Jawa Barat menunjukkan jumlah kasus yang
                               sangat tinggi. Meskipun pada pekan selanjutnya
                               langsung dapat diturunkan secara signifikan"),
                             plotOutput(outputId = "G5"), #Dinamika
                             p("Dinamika kasus COVID-19 ditunjukkan pada gambar di atas.
                               Terlihat bahwa kasus aktif mulai mampu dikendalikan.
                               Di saat yang sama, terjadi peningkatan angka pada kasus
                               yang sembuh secara signifikan. Pada beberapa hari terakhir
                               bahkan menunjukkan bahwa jumlah pasien sembuh
                               melebihi kasus aktif.")
                             
                             )
                  ),
                  tabPanel("Improvement Project",                         
                           sidebarPanel(
                             h4("DQ-LAB Weekend Project"),
                             h4("Sunday, 2 August 2020"),
                             h4("Author: Joko Eliyanto")),
                           mainPanel(
                             h2("Analisis COVID-19 di Jawa Barat"),            
                             includeMarkdown("COVID19DQLAB2.Rmd") 
                           )
                  ),
                  tabPanel("Improvement Project R&A",                        
                           sidebarPanel(
                             h4("DQ-LAB Weekend Project"),
                             h4("Sunday, 2 August 2020"),
                             h4("Author: Joko Eliyanto")),
                           mainPanel(
                             h2("Analisis COVID-19 di Jawa Barat"),           
                             plotOutput(outputId = "Ga1"),#Laporan
                             p("Pada gambar di atas terlihat bahwa lonjakan 
                               jumlah kasus baru berpengaruh langsung terhadap 
                               jumlah kasus aktif. Jumlah kasus baru yang dapat 
                               dikendalikan berpengaruh langsung terhadap 
                               jumlah kasus aktif di Jawa Barat."),
                             plotOutput(outputId = "Ga2"),#Dinamika
                             p("Dinamika kasus COVID-19 ditunjukkan pada gambar 
                               di atas. Terlihat bahwa kasus aktif mulai mampu 
                               dikendalikan. Di saat yang sama, terjadi 
                               peningkatan angka pada kasus yang sembuh secara 
                               signifikan. Pada beberapa hari terakhir bahkan 
                               menunjukkan bahwa jumlah pasien sembuh melebihi 
                               kasus aktif."),
                             plotOutput(outputId = "Ga3"),#PSBB VS AKB
                             p("Pergerakan data pada blok warna merah merupakan 
                               perkembangan data pada kurun waktu 6 Mei 2020 
                               - 26 Juni 2020 dimana kebijakan PSBB diterapkan. 
                               Terlihat  jelas selisih yang sangat jauh antara 
                               kasus aktif dan kasus sembuh. PSBB yang efektif 
                               mampu menekan jumlah kasus aktif sehingga PSBB 
                               diganti dengan AKB mulai 27 Juni 2020. Setelah 
                               PSBB jumlah kasus sembuh meningkat secara 
                               eksponensial bahkan hingga melampaui jumlah 
                               kasus positif. Meskipun terdapat lonjakan 
                               kasus baru di awal bulan Juli, namun jumlah 
                               kasus sembuh masih lebih dominan untuk 
                               merangkak naik. "),
                             plotOutput(outputId = "Ga4"),#Stacked
                             p("Proporsi jumlah kasus positif, sembuh dan 
                               meninggal ditunjukkan pada gambar di atas. 
                               Terlihat ketika PSBB diterapkan maka jumlah 
                               kasus aktif masih mendominasi. Setelah 
                               diterapkan PSBB, jumlah kasus sembuh mendominasi 
                               daripada kasus aktif dan meninggal. Ini 
                               menunjukkan bahwa PSBB terbukti efektif. "),
                             plotOutput(outputId = "Ga5"), #Persentase
                             p("Persentase kasus sembuh meningkat secara 
                               tajam pada akhir bulan Juli. Ini merupakan 
                               indikasi bahwa penangan COVID-19 di Jawa 
                               Barat cukup efektif.")
                             
                             )
                  ),
                  tabPanel("Kesimpulan & Rekomendasi",                         
                           sidebarPanel(
                             h4("DQ-LAB Weekend Project"),
                             h4("Sunday, 2 August 2020"),
                             h4("Author: Joko Eliyanto")),
                           mainPanel(
                             h2("Kesimpulan"),            
                             h4("Kebijakan yang diterapkan 
                                oleh pemerintah Jawa Barat 
                                terbukti efektif untuk 
                                menurunkan jumlah kasus aktif, 
                                kasus baru harian dan 
                                meningkatkan kasus sembuh."),
                             h2("Rekomendasi"),            
                             h4("Agar pandemi COVID-19 
                                segera berakhir maka kita harus menaati 
                                protokol kesehatan, agar turut berkontribusi 
                                menurunkan tingkat penularan dan kasus aktif."),
                             h4("Kebiasaan hidup sehat juga perlu senantiasa 
                                diterapkan untuk meningkatkan daya tahan tubuh 
                                dari segala penyakit terkhusus virus.")
                           )
                  )
                  
                  
                             ) # akhir navbarPage
                
                  ) # akhir fluidPage

#------------------------------------------------------------------------------------------------#
#----------------------Mendefinisikan Fungsi Hasil Yang AKan Ditampilkan-------------------------#

server <- function(input, output) {
  
  #Output untuk Tabel Data Mentah
  output$Tabel1 = DT::renderDataTable({
    resp_jabar <- GET("https://data.covid19.go.id/public/api/prov_detail_JAWA_BARAT.json")
    cov_jabar_raw <- content(resp_jabar, as = "parsed", simplifyVector = TRUE)
    
    cov_jabar<-cov_jabar_raw$list_perkembangan
    
    new_cov_jabar <-
      cov_jabar %>% 
      select(-contains("DIRAWAT_OR_ISOLASI")) %>% 
      select(-starts_with("AKUMULASI")) %>% 
      rename(
        kasus_baru = KASUS,
        meninggal = MENINGGAL,
        sembuh = SEMBUH
      ) 
    new_cov_jabar$tanggal = as.POSIXct(new_cov_jabar$tanggal / 1000, origin = "1970-01-01")
    new_cov_jabar$tanggal = as.Date(new_cov_jabar$tanggal)
    new_cov_jabar
  })
  output$Tabel2 = DT::renderDataTable({
    resp_jabar <- GET("https://data.covid19.go.id/public/api/prov_detail_JAWA_BARAT.json")
    cov_jabar_raw <- content(resp_jabar, as = "parsed", simplifyVector = TRUE)
    
    cov_jabar<-cov_jabar_raw$list_perkembangan
    
    new_cov_jabar <-
      cov_jabar %>% 
      select(-contains("DIRAWAT_OR_ISOLASI")) %>% 
      select(-starts_with("AKUMULASI")) %>% 
      rename(
        kasus_baru = KASUS,
        meninggal = MENINGGAL,
        sembuh = SEMBUH
      ) 
    new_cov_jabar$tanggal = as.POSIXct(new_cov_jabar$tanggal / 1000, origin = "1970-01-01")
    new_cov_jabar$tanggal = as.Date(new_cov_jabar$tanggal)
    cov_jabar_akumulasi <- 
      new_cov_jabar %>% 
      transmute(
        tanggal,
        akumulasi_positif=cumsum(kasus_baru),
        akumulasi_aktif = cumsum(kasus_baru) - cumsum(sembuh) - cumsum(meninggal),
        akumulasi_sembuh = cumsum(sembuh),
        akumulasi_meninggal = cumsum(meninggal)
      )
  })
  
  output$G1 <- renderPlot({
    
    resp_jabar <- GET("https://data.covid19.go.id/public/api/prov_detail_JAWA_BARAT.json")
    cov_jabar_raw <- content(resp_jabar, as = "parsed", simplifyVector = TRUE)
    
    cov_jabar<-cov_jabar_raw$list_perkembangan
    
    new_cov_jabar <-
      cov_jabar %>% 
      select(-contains("DIRAWAT_OR_ISOLASI")) %>% 
      select(-starts_with("AKUMULASI")) %>% 
      rename(
        kasus_baru = KASUS,
        meninggal = MENINGGAL,
        sembuh = SEMBUH
      ) %>% 
      mutate(
        tanggal = as.POSIXct(tanggal / 1000, origin = "1970-01-01"),
        tanggal = as.Date(tanggal)
      )
    
    ggplot(new_cov_jabar, aes(tanggal, kasus_baru)) +
      geom_col(fill = "salmon") +
      labs(
        x = NULL,
        y = "Jumlah kasus",
        title = "Kasus Harian Positif COVID-19 di Jawa Barat",
        subtitle = "Terjadi pelonjakan kasus di awal bulan Juli akibat klaster Secapa AD Bandung",
        caption = "Sumber data: covid.19.go.id"
      ) +
      theme_ipsum(
        base_size = 14,
        plot_title_size = 21,
        grid = "Y",
        ticks = TRUE
      ) +
      theme(plot.title.position = "plot")
  })
  
  output$G2 <- renderPlot({
    
    resp_jabar <- GET("https://data.covid19.go.id/public/api/prov_detail_JAWA_BARAT.json")
    cov_jabar_raw <- content(resp_jabar, as = "parsed", simplifyVector = TRUE)
    
    cov_jabar<-cov_jabar_raw$list_perkembangan
    
    new_cov_jabar <-
      cov_jabar %>% 
      select(-contains("DIRAWAT_OR_ISOLASI")) %>% 
      select(-starts_with("AKUMULASI")) %>% 
      rename(
        kasus_baru = KASUS,
        meninggal = MENINGGAL,
        sembuh = SEMBUH
      ) %>% 
      mutate(
        tanggal = as.POSIXct(tanggal / 1000, origin = "1970-01-01"),
        tanggal = as.Date(tanggal)
      )
    
    ggplot(new_cov_jabar, aes(tanggal, sembuh)) +
      geom_col(fill = "olivedrab2") +
      labs(
        x = NULL,
        y = "Jumlah kasus",
        title = "Kasus Harian Sembuh Dari COVID-19 di Jawa Barat",
        caption = "Sumber data: covid.19.go.id"
      ) +
      theme_ipsum(
        base_size = 13, 
        plot_title_size = 21,
        grid = "Y",
        ticks = TRUE
      ) +
      theme(plot.title.position = "plot")
  })
  
  output$G3 <- renderPlot({
    
    resp_jabar <- GET("https://data.covid19.go.id/public/api/prov_detail_JAWA_BARAT.json")
    cov_jabar_raw <- content(resp_jabar, as = "parsed", simplifyVector = TRUE)
    
    cov_jabar<-cov_jabar_raw$list_perkembangan
    
    new_cov_jabar <-
      cov_jabar %>% 
      select(-contains("DIRAWAT_OR_ISOLASI")) %>% 
      select(-starts_with("AKUMULASI")) %>% 
      rename(
        kasus_baru = KASUS,
        meninggal = MENINGGAL,
        sembuh = SEMBUH
      ) %>% 
      mutate(
        tanggal = as.POSIXct(tanggal / 1000, origin = "1970-01-01"),
        tanggal = as.Date(tanggal)
      )
    
    ggplot(new_cov_jabar, aes(tanggal, meninggal)) +
      geom_col(fill = "darkslategray4") +
      labs(
        x = NULL,
        y = "Jumlah kasus",
        title = "Kasus Harian Meninggal Akibat COVID-19 di Jawa Barat",
        caption = "Sumber data: covid.19.go.id"
      ) +
      theme_ipsum(
        base_size = 13, 
        plot_title_size = 21,
        grid = "Y",
        ticks = TRUE
      ) +
      theme(plot.title.position = "plot")
  })
  
  output$G4 <- renderPlot({
    
    resp_jabar <- GET("https://data.covid19.go.id/public/api/prov_detail_JAWA_BARAT.json")
    cov_jabar_raw <- content(resp_jabar, as = "parsed", simplifyVector = TRUE)
    
    cov_jabar<-cov_jabar_raw$list_perkembangan
    
    new_cov_jabar <-
      cov_jabar %>% 
      select(-contains("DIRAWAT_OR_ISOLASI")) %>% 
      select(-starts_with("AKUMULASI")) %>% 
      rename(
        kasus_baru = KASUS,
        meninggal = MENINGGAL,
        sembuh = SEMBUH
      ) %>% 
      mutate(
        tanggal = as.POSIXct(tanggal / 1000, origin = "1970-01-01"),
        tanggal = as.Date(tanggal)
      )
    
    cov_jabar_pekanan <- new_cov_jabar %>% 
      count(
        tahun = year(tanggal),
        pekan_ke = week(tanggal),
        wt = kasus_baru,
        name = "jumlah"
      )
    
    cov_jabar_pekanan <-
      cov_jabar_pekanan %>% 
      mutate(
        jumlah_pekanlalu = dplyr::lag(jumlah, 1),
        jumlah_pekanlalu = ifelse(is.na(jumlah_pekanlalu), 0, jumlah_pekanlalu),
        lebih_baik = jumlah < jumlah_pekanlalu
      )
    
    ggplot(cov_jabar_pekanan, aes(pekan_ke, jumlah, fill = lebih_baik)) +
      geom_col(show.legend = FALSE) +
      scale_x_continuous(breaks = 9:29, expand = c(0, 0)) +
      scale_fill_manual(values = c("TRUE" = "seagreen3", "FALSE" = "salmon")) +
      labs(
        x = NULL,
        y = "Jumlah kasus",
        title = "Kasus Pekanan Positif COVID-19 di Jawa Barat",
        subtitle = "Kolom hijau menunjukan penambahan kasus baru lebih sedikit dibandingkan satu pekan sebelumnya",
        caption = "Sumber data: covid.19.go.id"
      ) +
      theme_ipsum(
        base_size = 13,
        plot_title_size = 21,
        grid = "Y",
        ticks = TRUE
      ) +
      theme(plot.title.position = "plot")
  })
  
  output$G5 <- renderPlot({
    
    resp_jabar <- GET("https://data.covid19.go.id/public/api/prov_detail_JAWA_BARAT.json")
    cov_jabar_raw <- content(resp_jabar, as = "parsed", simplifyVector = TRUE)
    
    cov_jabar<-cov_jabar_raw$list_perkembangan
    
    new_cov_jabar <-
      cov_jabar %>% 
      select(-contains("DIRAWAT_OR_ISOLASI")) %>% 
      select(-starts_with("AKUMULASI")) %>% 
      rename(
        kasus_baru = KASUS,
        meninggal = MENINGGAL,
        sembuh = SEMBUH
      ) %>% 
      mutate(
        tanggal = as.POSIXct(tanggal / 1000, origin = "1970-01-01"),
        tanggal = as.Date(tanggal)
      )
    
    cov_jabar_akumulasi <- 
      new_cov_jabar %>% 
      transmute(
        tanggal,
        akumulasi_aktif = cumsum(kasus_baru) - cumsum(sembuh) - cumsum(meninggal),
        akumulasi_sembuh = cumsum(sembuh),
        akumulasi_meninggal = cumsum(meninggal)
      )
    
    cov_jabar_akumulasi_pivot <- 
      cov_jabar_akumulasi %>% 
      gather(
        key = "kategori",
        value = "jumlah",
        -tanggal
      ) %>% 
      mutate(
        kategori = sub(pattern = "akumulasi_", replacement = "", kategori)
      )
    
    cov_jabar_akumulasi_pivot <-
      cov_jabar_akumulasi %>%
      pivot_longer(
        cols = -tanggal,
        names_to = "kategori",
        names_prefix = "akumulasi_",
        values_to = "jumlah"
      )
    
    ggplot(cov_jabar_akumulasi_pivot, aes(tanggal, jumlah, colour = (kategori))) +
      geom_line(size = 0.9) +
      scale_y_continuous(sec.axis = dup_axis(name = NULL)) +
      scale_colour_manual(
        values = c(
          "aktif" = "salmon",
          "meninggal" = "darkslategray4",
          "sembuh" = "olivedrab2"
        ),
        labels = c("Aktif", "Meninggal", "Sembuh")
      ) +
      labs(
        x = NULL,
        y = "Jumlah kasus akumulasi",
        colour = NULL,
        title = "Dinamika Kasus COVID-19 di Jawa Barat",
        caption = "Sumber data: covid.19.go.id"
      ) +
      theme_ipsum(
        base_size = 13,
        plot_title_size = 21,
        grid = "Y",
        ticks = TRUE
      ) +
      theme(
        plot.title = element_text(hjust = 0.5),
        legend.position = "top"
      )
  })
  
  output$Ga1 <- renderPlot({
    
    resp_jabar <- GET("https://data.covid19.go.id/public/api/prov_detail_JAWA_BARAT.json")
    cov_jabar_raw <- content(resp_jabar, as = "parsed", simplifyVector = TRUE)
    
    cov_jabar<-cov_jabar_raw$list_perkembangan
    
    new_cov_jabar <-
      cov_jabar %>% 
      select(-contains("DIRAWAT_OR_ISOLASI")) %>% 
      select(-starts_with("AKUMULASI")) %>% 
      rename(
        kasus_baru = KASUS,
        meninggal = MENINGGAL,
        sembuh = SEMBUH
      ) %>% 
      mutate(
        tanggal = as.POSIXct(tanggal / 1000, origin = "1970-01-01"),
        tanggal = as.Date(tanggal)
      )
    
    cov_jabar_akumulasi <- 
      new_cov_jabar %>% 
      transmute(
        tanggal,
        akumulasi_aktif = cumsum(kasus_baru) - cumsum(sembuh) - cumsum(meninggal),
        akumulasi_sembuh = cumsum(sembuh),
        akumulasi_meninggal = cumsum(meninggal)
      )
    
    cov_jabar_akumulasi2 <- 
      new_cov_jabar %>% 
      transmute(
        tanggal,
        kasus_baru=kasus_baru,
        akumulasi_aktif = cumsum(kasus_baru) - cumsum(sembuh) - cumsum(meninggal),
        akumulasi_sembuh = cumsum(sembuh),
        akumulasi_meninggal = cumsum(meninggal)
      )
    
    cov_jabar_akumulasi_pivot <- 
      cov_jabar_akumulasi %>% 
      gather(
        key = "kategori",
        value = "jumlah",
        -tanggal
      ) %>% 
      mutate(
        kategori = sub(pattern = "akumulasi_", replacement = "", kategori)
      )
    
    cov_jabar_akumulasi_pivot <-
      cov_jabar_akumulasi %>%
      pivot_longer(
        cols = -tanggal,
        names_to = "kategori",
        names_prefix = "akumulasi_",
        values_to = "jumlah"
      )
    
    ggplot(cov_jabar_akumulasi2, aes(x=tanggal)) +
      geom_line(aes(y = kasus_baru, colour = "Kasus Baru"))+
      geom_line(aes(y = akumulasi_aktif, colour = "Akumulasi Kasus Aktif"))+
      scale_y_continuous(sec.axis = sec_axis(~.*5, name = "Akumulasi Kasus Aktif"))+
      scale_colour_manual(values = c("blue", "red"))+
      labs(y = "Kasus Baru",
           x = "Tanggal",
           colour = "Parameter")+
      theme(legend.position = "top")+
      theme(plot.title.position = "plot")
  })
  
  output$Ga2 <- renderPlot({
    
    resp_jabar <- GET("https://data.covid19.go.id/public/api/prov_detail_JAWA_BARAT.json")
    cov_jabar_raw <- content(resp_jabar, as = "parsed", simplifyVector = TRUE)
    
    cov_jabar<-cov_jabar_raw$list_perkembangan
    
    new_cov_jabar <-
      cov_jabar %>% 
      select(-contains("DIRAWAT_OR_ISOLASI")) %>% 
      select(-starts_with("AKUMULASI")) %>% 
      rename(
        kasus_baru = KASUS,
        meninggal = MENINGGAL,
        sembuh = SEMBUH
      ) %>% 
      mutate(
        tanggal = as.POSIXct(tanggal / 1000, origin = "1970-01-01"),
        tanggal = as.Date(tanggal)
      )
    
    cov_jabar_akumulasi <- 
      new_cov_jabar %>% 
      transmute(
        tanggal,
        akumulasi_aktif = cumsum(kasus_baru) - cumsum(sembuh) - cumsum(meninggal),
        akumulasi_sembuh = cumsum(sembuh),
        akumulasi_meninggal = cumsum(meninggal)
      )
    
    cov_jabar_akumulasi2 <- 
      new_cov_jabar %>% 
      transmute(
        tanggal,
        kasus_baru=kasus_baru,
        akumulasi_aktif = cumsum(kasus_baru) - cumsum(sembuh) - cumsum(meninggal),
        akumulasi_sembuh = cumsum(sembuh),
        akumulasi_meninggal = cumsum(meninggal)
      )
    
    cov_jabar_akumulasi_pivot <- 
      cov_jabar_akumulasi %>% 
      gather(
        key = "kategori",
        value = "jumlah",
        -tanggal
      ) %>% 
      mutate(
        kategori = sub(pattern = "akumulasi_", replacement = "", kategori)
      )
    
    cov_jabar_akumulasi_pivot <-
      cov_jabar_akumulasi %>%
      pivot_longer(
        cols = -tanggal,
        names_to = "kategori",
        names_prefix = "akumulasi_",
        values_to = "jumlah"
      )
    
    ggplot(cov_jabar_akumulasi_pivot, aes(tanggal, jumlah, colour = (kategori))) +
      geom_line(size = 0.9) +
      scale_y_continuous(sec.axis = dup_axis(name = NULL)) +
      scale_colour_manual(
        values = c(
          "aktif" = "salmon",
          "meninggal" = "darkslategray4",
          "sembuh" = "olivedrab2"
        ),
        labels = c("Aktif", "Meninggal", "Sembuh")
      ) +
      labs(
        x = NULL,
        y = "Jumlah kasus akumulasi",
        colour = NULL,
        title = "Dinamika Kasus COVID-19 di Jawa Barat",
        caption = "Sumber data: covid.19.go.id"
      ) 
  })
  
  output$Ga3 <- renderPlot({
    
    resp_jabar <- GET("https://data.covid19.go.id/public/api/prov_detail_JAWA_BARAT.json")
    cov_jabar_raw <- content(resp_jabar, as = "parsed", simplifyVector = TRUE)
    
    cov_jabar<-cov_jabar_raw$list_perkembangan
    
    new_cov_jabar <-
      cov_jabar %>% 
      select(-contains("DIRAWAT_OR_ISOLASI")) %>% 
      select(-starts_with("AKUMULASI")) %>% 
      rename(
        kasus_baru = KASUS,
        meninggal = MENINGGAL,
        sembuh = SEMBUH
      ) %>% 
      mutate(
        tanggal = as.POSIXct(tanggal / 1000, origin = "1970-01-01"),
        tanggal = as.Date(tanggal)
      )
    
    cov_jabar_akumulasi <- 
      new_cov_jabar %>% 
      transmute(
        tanggal,
        akumulasi_aktif = cumsum(kasus_baru) - cumsum(sembuh) - cumsum(meninggal),
        akumulasi_sembuh = cumsum(sembuh),
        akumulasi_meninggal = cumsum(meninggal)
      )
    
    cov_jabar_akumulasi2 <- 
      new_cov_jabar %>% 
      transmute(
        tanggal,
        kasus_baru=kasus_baru,
        akumulasi_aktif = cumsum(kasus_baru) - cumsum(sembuh) - cumsum(meninggal),
        akumulasi_sembuh = cumsum(sembuh),
        akumulasi_meninggal = cumsum(meninggal)
      )
    
    cov_jabar_akumulasi_pivot <- 
      cov_jabar_akumulasi %>% 
      gather(
        key = "kategori",
        value = "jumlah",
        -tanggal
      ) %>% 
      mutate(
        kategori = sub(pattern = "akumulasi_", replacement = "", kategori)
      )
    
    cov_jabar_akumulasi_pivot <-
      cov_jabar_akumulasi %>%
      pivot_longer(
        cols = -tanggal,
        names_to = "kategori",
        names_prefix = "akumulasi_",
        values_to = "jumlah"
      )
    
    ggplot(cov_jabar_akumulasi_pivot, aes(tanggal, jumlah, colour = (kategori))) +
      geom_line(size = 0.9) +
      scale_y_continuous(sec.axis = dup_axis(name = NULL)) +
      scale_colour_manual(
        values = c(
          "aktif" = "salmon",
          "meninggal" = "darkslategray4",
          "sembuh" = "olivedrab2"
        ),
        labels = c("Aktif", "Meninggal", "Sembuh")
      ) +
      labs(
        x = NULL,
        y = "Jumlah kasus akumulasi",
        colour = NULL,
        title = "PSBB VS PASCA PSBB",
        caption = "Sumber data: covid.19.go.id"
      ) +
      annotate("rect", xmin=cov_jabar_akumulasi_pivot$tanggal[199], 
               xmax=cov_jabar_akumulasi_pivot$tanggal[354], 
               ymin=min(cov_jabar_akumulasi_pivot$jumlah), 
               ymax=max(cov_jabar_akumulasi_pivot$jumlah), 
               alpha=0.2, 
               color="red", 
               fill="red" 
      )+
      annotate("rect", xmin=cov_jabar_akumulasi_pivot$tanggal[354], 
               xmax=cov_jabar_akumulasi_pivot$tanggal[460], 
               ymin=min(cov_jabar_akumulasi_pivot$jumlah), 
               ymax=max(cov_jabar_akumulasi_pivot$jumlah), 
               alpha=0.2, 
               color="green", 
               fill="green"
      )
  })
  
  output$Ga4 <- renderPlot({
    
    resp_jabar <- GET("https://data.covid19.go.id/public/api/prov_detail_JAWA_BARAT.json")
    cov_jabar_raw <- content(resp_jabar, as = "parsed", simplifyVector = TRUE)
    
    cov_jabar<-cov_jabar_raw$list_perkembangan
    
    new_cov_jabar <-
      cov_jabar %>% 
      select(-contains("DIRAWAT_OR_ISOLASI")) %>% 
      select(-starts_with("AKUMULASI")) %>% 
      rename(
        kasus_baru = KASUS,
        meninggal = MENINGGAL,
        sembuh = SEMBUH
      ) %>% 
      mutate(
        tanggal = as.POSIXct(tanggal / 1000, origin = "1970-01-01"),
        tanggal = as.Date(tanggal)
      )
    
    cov_jabar_akumulasi <- 
      new_cov_jabar %>% 
      transmute(
        tanggal,
        akumulasi_aktif = cumsum(kasus_baru) - cumsum(sembuh) - cumsum(meninggal),
        akumulasi_sembuh = cumsum(sembuh),
        akumulasi_meninggal = cumsum(meninggal)
      )
    
    cov_jabar_akumulasi2 <- 
      new_cov_jabar %>% 
      transmute(
        tanggal,
        kasus_baru=kasus_baru,
        akumulasi_aktif = cumsum(kasus_baru) - cumsum(sembuh) - cumsum(meninggal),
        akumulasi_sembuh = cumsum(sembuh),
        akumulasi_meninggal = cumsum(meninggal)
      )
    
    cov_jabar_akumulasi_pivot <- 
      cov_jabar_akumulasi %>% 
      gather(
        key = "kategori",
        value = "jumlah",
        -tanggal
      ) %>% 
      mutate(
        kategori = sub(pattern = "akumulasi_", replacement = "", kategori)
      )
    
    cov_jabar_akumulasi_pivot <-
      cov_jabar_akumulasi %>%
      pivot_longer(
        cols = -tanggal,
        names_to = "kategori",
        names_prefix = "akumulasi_",
        values_to = "jumlah"
      )
    
    data <- cov_jabar_akumulasi_pivot[199:length(cov_jabar_akumulasi_pivot$tanggal),]
    ggplot(data, aes(tanggal, jumlah)) +
      geom_col(aes(fill = kategori), width = 0.7)+
      labs(
        x = NULL,
        y = "Jumlah kasus akumulasi",
        colour = NULL,
        title = "Proporsi Kasus Aktif, Sembuh, dan Meninggal",
        caption = "Sumber data: covid.19.go.id"
      ) +
      annotate("rect", xmin=data$tanggal[1], 
               xmax=data$tanggal[154], 
               ymin=0, 
               ymax=7000, 
               alpha=0.1, 
               color="red", 
               fill="red"
      )+
      annotate("rect", xmin=data$tanggal[154], 
               xmax=data$tanggal[264], 
               ymin=0, 
               ymax=7000, 
               alpha=0.1, 
               color="green", 
               fill="green"
      )
    
  })
  
  output$Ga5 <- renderPlot({
    
    resp_jabar <- GET("https://data.covid19.go.id/public/api/prov_detail_JAWA_BARAT.json")
    cov_jabar_raw <- content(resp_jabar, as = "parsed", simplifyVector = TRUE)
    
    cov_jabar<-cov_jabar_raw$list_perkembangan
    
    new_cov_jabar <-
      cov_jabar %>% 
      select(-contains("DIRAWAT_OR_ISOLASI")) %>% 
      select(-starts_with("AKUMULASI")) %>% 
      rename(
        kasus_baru = KASUS,
        meninggal = MENINGGAL,
        sembuh = SEMBUH
      ) %>% 
      mutate(
        tanggal = as.POSIXct(tanggal / 1000, origin = "1970-01-01"),
        tanggal = as.Date(tanggal)
      )
    
    cov_jabar_akumulasi <- 
      new_cov_jabar %>% 
      transmute(
        tanggal,
        akumulasi_aktif = cumsum(kasus_baru) - cumsum(sembuh) - cumsum(meninggal),
        akumulasi_sembuh = cumsum(sembuh),
        akumulasi_meninggal = cumsum(meninggal)
      )
    
    cov_jabar_akumulasi2 <- 
      new_cov_jabar %>% 
      transmute(
        tanggal,
        kasus_baru=kasus_baru,
        akumulasi_aktif = cumsum(kasus_baru) - cumsum(sembuh) - cumsum(meninggal),
        akumulasi_sembuh = cumsum(sembuh),
        akumulasi_meninggal = cumsum(meninggal)
      )
    
    cov_jabar_akumulasi_pivot <- 
      cov_jabar_akumulasi %>% 
      gather(
        key = "kategori",
        value = "jumlah",
        -tanggal
      ) %>% 
      mutate(
        kategori = sub(pattern = "akumulasi_", replacement = "", kategori)
      )
    
    cov_jabar_akumulasi_pivot <-
      cov_jabar_akumulasi %>%
      pivot_longer(
        cols = -tanggal,
        names_to = "kategori",
        names_prefix = "akumulasi_",
        values_to = "jumlah"
      )
    
    cov_jabar_akumulasi3 <- 
      new_cov_jabar %>% 
      transmute(
        tanggal,
        persentasesembuh = sembuh/max(kasus_baru)
      )
    
    data1 <- cov_jabar_akumulasi3[67:length(cov_jabar_akumulasi_pivot$tanggal),]
    
    ggplot(data1, aes(tanggal, persentasesembuh)) +
      geom_col(size = 0.9) +
      labs(
        x = NULL,
        y = "Jumlah kasus akumulasi",
        colour = "persentasesembuh",
        title = "Persentase Kasus Sembuh Terhadap Titik Puncak",
        caption = "Sumber data: covid.19.go.id"
      ) 
  })

} # akhir kode server shiny

#------------------------------------------------------------------------------------------------#


#---------------------------------------Menjalankan Shiny App------------------------------------#

shinyApp(ui = ui, server = server)

#------------------------------------------------------------------------------------------------#

  



