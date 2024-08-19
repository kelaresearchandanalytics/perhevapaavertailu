#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @import metathis
#' @import shinyFeedback
#' @import shiny.i18n
#' @import bslib
#' @noRd
app_ui <- function(request) {
  
  i18n <- golem::get_golem_options(which = "translator")
  i18n$set_translation_language("fi")
  
  tagList(
    usei18n(i18n),
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    # suppressDependencies("bootstrap"),
    suppressDependencies("font-awesome"),
    # Your application UI logic
    bslib::page_fluid(
      title = "Perhevapaavertailu |  Jämförelseverktyg för familjeledigheter |  Family leave calculator",
      tags$html(lang="fi"),
      useShinyFeedback(), # include shinyFeedback
      # theme = bslib::bs_theme(#preset = "minty",
      #                         # primary = "#0f73a9",
      #                         version = 5
      #                         # base_font = "Lato",
      #                         # heading_font = "Noto Sans"
      #                         ),
      tags$head(
        # Note the wrapping of the string in HTML()
        
        tags$style(HTML("
        
body {font-family: 'Lato', sans-serif;
  border-top: 5px solid #fdb916;
  border-bottom: 5px solid #fdb916;
  margin-bottom: 1rem;

}

.bg-kela {
  background-color:#f3f5f9!important}

.btn-outline-kela{color:#003580;border-color:#003580}

.shiny-plot-output {
  /*border: 1px solid rgba(0, 0, 0, 0.125);*/
margin-top: 20px;
margin-bottom: 20px;
height: auto;
border:3px solid #fff;
/*box-shadow: 0 4px 8px rgba(0,0,0,0.15);
-moz-box-shadow: 0 4px 8px rgba(0,0,0,0.15);
-webkit-box-shadow: 0 4px 8px rgba(0,0,0,0.15);
-khtml-box-shadow: 0 4px 8px rgba(0,0,0,0.15);*/
 
}

.selectize-input {
  border: 0px solid #ced4da !important;
  background-color: #003580 !important;
  color: #FFF;
  padding-top: 15px;
}

  
.selectize-control.single .selectize-input:not(.no-arrow)::after {
  content: '';
  display: block;
  position: absolute;
  top: 50%;
  right: calc(0.75rem + 5px);
  margin-top: 0px;
  width: 0;
  height: 0;
  border-style: solid;
  border-width: 7px 7px 0px 7px;
  border-color: #FFF transparent transparent transparent;
}

.selectize-dropdown .selected {
  background-color: #f0f5ff;
  color: #000;
}

.btn-outline-default, .btn-default:not(.btn-primary, .btn-secondary, .btn-info, .btn-success, .btn-danger, .btn-warning, .btn-light, .btn-dark, .btn-link, [class*='btn-outline-']) {
  --bs-btn-hover-bg: #003580;
  --bs-btn-hover-color: #FFF;
}

.form-control:focus {
  border: 3px solid #393939;
}

.form:focus {
  border: 3px solid #393939;
}

.accordion-button:focus {
  border: 3px solid #393939;
}

a:focus  {
  border: 3px solid #393939;
}

.form-control.selectize-control:focus-within  {
  
  border: 2px solid #000;
}


"))),
      metathis::meta() %>%
        metathis::meta_description(description = "Perhevapaavertailun avulla voit tarkastella, miten perhevapaiden jakaminen vanhempien kesken vaikuttaa perheen tuloihin.  Med hjälp av jämförelseverktyget för familjeledigheter kan du ta reda på hur familjens inkomster förändras när föräldrarna delar familjeledigheterna på olika sätt.  You can use this calculator to give you an idea of how sharing family leave between parents can affect your family income.") %>%
        metathis::meta_social(
          title = "Perhevapaavertailu |  Jämförelseverktyg för familjeledigheter |  Family leave calculator",
          description = "Perhevapaavertailun avulla voit tarkastella, miten perhevapaiden jakaminen vanhempien kesken vaikuttaa perheen tuloihin.  Med hjälp av jämförelseverktyget för familjeledigheter kan du ta reda på hur familjens inkomster förändras när föräldrarna delar familjeledigheterna på olika sätt.  You can use this calculator to give you an idea of how sharing family leave between parents can affect your family income.",
          url = "https://perhevapaavertailu.kela.fi",
          image = "https://perhevapaavertailu.kela.fi/www/peva_esikatselu.png",
          image_alt = "Perhevapaavertailu |  Jämförelseverktyg för familjeledigheter |  Family leave calculator",
          twitter_creator = "@Kelantutkimus",
          twitter_card_type = "summary_large_image",
          twitter_site = "@Kelantutkimus"
        ),
      
      shinyjs::useShinyjs(),
      #  __   ___ _   _                 
      #  \ \ / / (_) (_)                
      #   \ V /| | __ _  ___  ___  __ _ 
      #    \ / | |/ _` |/ _ \/ __|/ _` |
      #    | | | | (_| | (_) \__ \ (_| |
      #    \_/ |_|\__,_|\___/|___/\__,_|
      #                                 
      tags$html(HTML('<a class="visually-hidden-focusable visually-hidden-focusable-focusable" href="#maincontent">Skip to main</a>')),
      
      tags$html(HTML('<main id="maincontent">')),
      tags$div(class = "row",
               tags$nav(class="navbar navbar-expand g-0", style = "background-color: #003580; height: 4em;",
                        
                        tags$ul(class = "d-flex", style = "margin-left: auto; padding-top: 35px; padding-right: 10px;",
                                tags$li(
                                  tags$html(HTML('<div style = "padding-top: 13px; padding-right: 12px;"> <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" width="1em" height="1em" class="icon-size-s"><path d="M0 0h24v24H0z" fill="none"></path><circle cx="12" cy="12" r="10" fill="none" stroke="#FFF" stroke-width="2" stroke-miterlimit="10"></circle><path d="M2 12h20" fill="none" stroke="#FFF" stroke-width="2" stroke-miterlimit="10"></path><ellipse cx="12" cy="12" rx="4" ry="10" stroke-linejoin="round" stroke="#FFF" stroke-width="2" fill="none"></ellipse></svg></div>'))
                                ),
                                tags$li(
                                  selectizeInput(inputId = "selected_language", 
                                                 label = NULL,
                                                 choices = list("Suomi" = "fi",
                                                                "Svenska" = "sv",
                                                                "English" = "en"), 
                                                 selected = "fi", width = "120px"
                                  )
                                ))
               )
      ),
      tags$div(class  = "row", style = "background-color: #f5f5f5; padding-top: 30px; padding-bottom: 20px",
               tags$div(class = "container_1280",
                        tags$div(class = "row",
                                 tags$div(class="col-md-9",
                                          tags$h1(class = "display-5", i18n$t("Perhevapaavertailu"),tags$span(style='font-size:13px;'
                                                                                                              # ,golem::get_golem_version()
                                          )),
                                          tags$p(class = "lead", 
                                                 i18n$t("Perhevapaavertailun avulla voit tarkastella, miten perhevapaiden jakaminen vanhempien kesken vaikuttaa perheen tuloihin.")),
                                          tags$p(class = "lead",
                                                 i18n$t("Vertailu on tarkoitettu työssäkäyville vanhemmille, joiden lapsi on syntynyt 4.9.2022 tai sen jälkeen. Vertailu on suuntaa antava."))
                                 )
                                 ,tags$div(class="col-md-3",
                                           tags$html(HTML('<div class = "float-end"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 272.6 132" xml:space="preserve" height="48" width="83"><circle cx="242.9" cy="29.8" r="29.1" fill="#fdb913"></circle><path d="M226.8 7.8h5.8v2.6h7.2V7.8h6v2.7h7.2V7.8h5.7l3.8 41.3h-12.6s-.9-10-.9-9.6c.1.3-.9-5.6-6.5-5.2-5.9 0-6.2 5.8-6.2 5.8l-.8 9H223l3.8-41.3z" fill="#fff"></path><circle cx="242.7" cy="22.4" r="6.2" fill="#fdb913"></circle><path d="m43.8 130.1-26-44.6v44.6H.2V44.6h17.6v39l24.4-39h21.2L36 84.2l30.4 45.9H43.8zm40.4-28.3v.5c0 9.7 4.8 15.3 13.3 15.3 5.7 0 10.9-2.1 16-6.3l6.4 9.8c-7.3 5.9-14.9 8.7-23.7 8.7-18.1 0-29.8-12.8-29.8-32.6 0-11.3 2.3-18.8 7.9-25 5.2-5.8 11.4-8.5 19.8-8.5 7.3 0 14.1 2.5 18.2 6.6 5.8 5.9 8.4 14.4 8.4 27.6v3.8H84.2zM103.6 89c0-4.7-.5-7.1-2-9.5-1.6-2.5-3.9-3.7-7.3-3.7-6.3 0-9.8 4.9-9.8 13.7v.2h19.1V89zm44.9 40.6c-7 0-12.7-3.3-14.6-8.6-1.2-3.2-1.5-5.2-1.5-14.1v-47c0-8.2-.2-13.3-.9-18.9l16.9-3.8c.6 3.4.9 7.5.9 16.4v49.1c0 10.8.1 12.3 1.1 14 .6 1.1 2 1.7 3.3 1.7.6 0 1 0 1.8-.2l2.8 9.8c-2.8.9-6.3 1.6-9.8 1.6zm59.6 2.3c-3.8-1.6-6.9-5-8.5-8.2-1.2 1.2-2.6 2.5-3.8 3.3-3.1 2.2-7.5 3.4-12.7 3.4-14 0-21.6-7.1-21.6-19.7 0-14.8 10.2-21.6 30.3-21.6 1.2 0 2.3 0 3.7.1v-2.6c0-7-1.4-9.3-7.4-9.3-5.3 0-11.4 2.6-18.2 7.1l-7-11.8c3.3-2.1 5.8-3.3 10.2-5.2 6.1-2.6 11.4-3.7 17.2-3.7 10.6 0 17.8 3.9 20.3 10.9.9 2.6 1.2 4.6 1.1 11.3l-.4 21.2c-.1 6.9.4 9.8 5.9 14l-9.1 10.8zm-13.6-31.4c-11.4 0-15.4 2.1-15.4 9.6 0 4.9 3.1 8.2 7.3 8.2 3.1 0 6.2-1.6 8.6-4.3l.2-13.5h-.7z" fill="#004895"></path></svg></div>'))
                                 )
                        )
               )),
      # Nootti alkaa!!
      tags$div(class = "container_1280", style = "padding-top: 20px; ",
      tags$div(class="kds-theme-kela kds-alert kds-alert--icon-left kds-alert--primary col-md-8",
               tags$html(HTML('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" width="1em" height="1em" class="kds-alert__icon kds-icon kds-icon--size-s"><path fill="none" d="M0 0h24v24H0z"></path><circle cx="12" cy="12" r="10" fill="none" stroke="currentColor" stroke-miterlimit="10" stroke-width="2"></circle><path fill="none" stroke="currentColor" stroke-linecap="round" stroke-miterlimit="10" stroke-width="2" d="M12 17v-6"></path><circle cx="12" cy="7" r="0.3" fill="none" stroke="currentColor" stroke-miterlimit="10" stroke-width="2"></circle></svg>')),
               tags$div(class="kds-alert__content", "16.1.2024 |", i18n$t("Perhevapaavertailu on päivitetty vuodelle 2024.")
               )
      )
    ),
    # Nootti päättyy!!
      tags$div(class = "container_1280", style = "padding-top: 20px;",
               tags$div(class = "row",
                        tags$div(class="col-md-8",
                                 tags$p(
                                   i18n$t("Vertailussa tarkastellaan eri tapoja jakaa lapsen syntymän jälkeen maksettavat"),
                                   tags$a(href = "https://www.kela.fi/vanhempainraha-ja-vanhempainraha-osittaisena/", 
                                          # href = i18n$t("https://www.kela.fi/vanhempainraha-ja-vanhempainraha-osittaisena/"), 
                                          target = "_blank", i18n$t("vanhempainraha")), i18n$t("ja"),
                                   tags$a(href = "https://www.kela.fi/kotihoidontuki/", 
                                          # href = i18n$t("https://www.kela.fi/kotihoidontuki/"), 
                                          target = "_blank", i18n$t("kotihoidon tuki")),
                                   i18n$t("perheen vanhempien kesken (vanhempi A ja vanhempi B). Raskausraha ei ole vertailussa mukana, joten ei ole väliä, kumpi on vertailussa vanhempi A ja kumpi on vanhempi B.")),
                                 tags$p(i18n$t("Vanhemmat voivat saada vanhempainrahaa yhteensä 320 päivältä, kumpikin 160 päivältä. Näistä enintään 63 vanhempainrahapäivää on mahdollista luovuttaa toiselle vanhemmalle. Molemmilla vanhemmilla on siis 97 sellaista vanhempainrahapäivää, joita ei voi luovuttaa.")),
                                 tags$p(i18n$t("Kun päivität vertailuun perheesi tiedot, se laskee suuntaa antavan arvion perheesi bruttotuloista ja verotuksen jälkeen käteen jäävistä nettotuloista kahden vuoden ajalta.  Tuloksena on laskelma, jonka avulla voit tarkastella, kuinka paljon perheen kokonaistulot kahden vuoden ajalta kasvavat tai vähenevät, jos molemmat vanhemmat käyttävät vapaita.")),
                                 tags$p(i18n$t("Laskelmaan on valittu kolme vaihtoehtoista tapaa jakaa valitsemasi hoitoaika (1–24 kk) vanhempien kesken. Näitä verrataan tilanteeseen, jossa perhevapaita ei jaeta vaan vain toinen vanhemmista käyttää vapaita. Jos valitsemasi hoitoaika on lyhyt, kotihoidon tuki jää laskelmasta pois.")),
                                 tags$p(i18n$t("Seuraavassa kuvataan lyhyesti vertailun lähtökohtana oleva tilanne ja kolme vaihtoehtoista tapaa jakaa perhevapaita:")),
                                 tags$ul(class="list-unstyled", style= "padding-left: 30px;",
                                         tags$li(tags$strong(i18n$t("Vertailukohta")), ": ",i18n$t("Vain vanhempi A käyttää vapaita, eli hän hoitaa lasta koko valitsemasi hoitoajan. Vanhempi B ei käytä vapaita.")),
                                         tags$li(tags$strong(i18n$t("Vaihtoehto"), " 1"), ": ",i18n$t("Vanhempi B käyttää 97 vanhempainrahapäivää. Vanhempi A käyttää kaikki muut vanhempainrahapäivät ja kotihoidon tukea.")),
                                         tags$li(tags$strong(i18n$t("Vaihtoehto"), " 2"), ": ",i18n$t("Vanhempainrahapäivät jaetaan tasan. Vanhempi A käyttää kotihoidon tukea.")),
                                         tags$li(tags$strong(i18n$t("Vaihtoehto"), " 3"), ": ",i18n$t("Vanhempainrahapäivät ja kotihoidon tuki jaetaan tasan vanhempien kesken."))
                                 ),
                                 
                                 # Esimerkki haitarissa
                                 tags$div(class="kds-theme-kela kds-accordion accordion", id="accordionExample", 
                                          tags$div(class="accordion-item kds-theme-kela kds-accordion__toggle",
                                                   tags$h2(class="accordion-header",
                                                           id="flush-headingTwo", 
                                                           tags$button(class="accordion-button kds-btn--with-icon kds-no-focus-ring collapsed", 
                                                                       type="button", 
                                                                       `data-bs-toggle`="collapse", 
                                                                       `data-bs-target`="#flush-collapseTwo", 
                                                                       `aria-expanded`="false", 
                                                                       `aria-controls`="flush-collapseTwo",
                                                                       i18n$t("Katso esimerkki perhevapaiden jakamisesta")#,
                                                                       # tags$span(class="bs-accordion-btn-icon")
                                                                       # tags$html(HTML('<span class="kds-accordion__toggle-icon"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" width="1em" height="1em" class="kds-icon kds-icon--size-s"><path fill="none" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="m3 7 9 10 9-10"></path><path fill="none" d="M0 0h24v24H0z"></path></svg></span>')),
                                                           )
                                                   )
                                          )
                                 ),
                                 tags$div(id="flush-collapseTwo", 
                                          class="accordion-collapse collapse", 
                                          `aria-labelledby`="flush-headingTwo",
                                          tags$div(class="kds-accordion__body kds-collapse kds-collapse--visible",
                                                   tags$div(style="padding: 15px; background-color: #f3f5f9;",
                                                            tags$p(tags$strong(i18n$t("Asetelma:")), i18n$t("Vanhemman A kuukausipalkka on 2 100 euroa, vanhemman B kuukausipalkka on 2 600 euroa, lasta hoidetaan kotona yhteensä 18 kuukautta ja kotihoidon tuki on 380 euroa kuukaudessa.")),
                                                            tags$p(tags$strong(i18n$t("Vertailukohta")), ": ",i18n$t("Vain vanhempi A käyttää vapaita. Vanhempi B luovuttaa 63 vanhempainrahapäivää vanhemmalle A eikä käytä itse mitään vapaita, vaan työskentelee koko kaksivuotisen tarkastelujakson ajan. Vanhemman B vanhempainrahapäivistä 97 jää käyttämättä. Vanhempi A on perhevapaalla 18 kuukautta (saa vanhempainrahaa 160 + 63 päivää ja kotihoidon tukea noin 9 kuukautta) ja palkkatyössä 6 kuukautta. Perheen kahden vuoden aikaiset bruttotulot ovat yhteensä noin 90 630 euroa ja verotuksen jälkeen käteen jäävät nettotulot noin 75 150 euroa.")),
                                                            tags$p(i18n$t("Vaihtoehdoissa 1–3 kuvataan, mitä perheen brutto- ja nettotuloille tapahtuu, jos myös vanhempi B käyttää perhevapaita. Kaikissa vaihtoehdoissa lasta hoidetaan kotona 18 kuukautta.")),
                                                            tags$p(tags$strong(i18n$t("Vaihtoehto"), " 1:"), i18n$t("Vanhempi B käyttää 97 vanhempainrahapäivää. Vanhempi A käyttää kaikki muut vanhempainrahapäivät ja kotihoidon tukea.")),
                                                            tags$p(i18n$t("Vanhempi B luovuttaa 63 päivää vanhemmalle A ja käyttää itse 97 vanhempainrahapäivää (noin 4 kuukautta). Vanhempi A saa vanhempainrahaa noin 9 kuukautta ja kotihoidon tukea noin 5 kuukautta. Vanhemman A kotihoidon tuen jakso siis lyhenee ja työssäkäyntiaika pitenee vertailukohtaan nähden noin 4 kuukautta. Perheen kahden vuoden aikaiset bruttotulot ovat 3 320 ja nettotulot 4 490 euroa suuremmat kuin vertailukohdassa.")),
                                                            tags$p(tags$strong(i18n$t("Vaihtoehto")," 2:"), i18n$t("Vanhempainrahapäivät jaetaan tasan. Vanhempi A käyttää kotihoidon tukea.")),
                                                            tags$p(i18n$t("Molemmat vanhemmat käyttävät 160 vanhempainrahapäivää (reilut 6 kuukautta). Vanhempi A saa kotihoidon tukea noin 5 kuukautta. Vanhempi A työskentelee reilut 12 kuukautta ja vanhempi B noin 18 kuukautta. Perheen kahden vuoden aikaiset bruttotulot ovat 2 860 ja nettotulot 5 180 euroa suuremmat kuin vertailukohdassa.")),
                                                            tags$p(tags$strong(i18n$t("Vaihtoehto"), " 3:"), i18n$t("Vanhempainrahapäivät ja kotihoidon tuki jaetaan tasan.")),
                                                            tags$p(i18n$t("Molemmat vanhemmat käyttävät 160 vanhempainrahapäivää (reilut 6 kuukautta) ja saavat kotihoidon tukea vajaat 3 kuukautta. Molemmat vanhemmat työskentelevät kahden vuoden aikana 15 kuukautta. Perheen kahden vuoden aikaiset bruttotulot ovat 1 560 ja nettotulot 5 190 euroa suuremmat kuin vertailukohdassa.")),
                                                            tags$p(i18n$t("Esimerkissä perheen kannalta taloudellista paras ratkaisu olisi jakaa vapaat tasan. Euromäärät ovat suuntaa antavia."))
                                                   )
                                          )
                                 )
                        )
               )
      ),
      
      
      #   _   _       _ _ _         _   
      #  | | | |     | (_) |       | |  
      #  | | | | __ _| |_| | _____ | |_ 
      #  | | | |/ _` | | | |/ / _ \| __|
      #  \ \_/ / (_| | | |   < (_) | |_ 
      #   \___/ \__,_|_|_|_|\_\___/ \__|
      #                                 
      #
      fluidRow(class = "container_1280", style = "padding-top: 30px; padding-bottom 60px;",
               tags$div(class="col-md-8 input-panel-shiny-kela-noflex",
                        
                        tags$p(tags$strong(i18n$t("Täytä tiedot oman tilanteesi mukaan. Tulokset päivittyvät alle."))),
                        
                        # column(width = 3, 
                        numericInput(inputId = "foo_input_T1",
                                     label = tags$div(
                                       tags$strong(i18n$t("Vanhemman A palkka (e/kk)")),tags$br(),
                                       tags$a(`data-bs-toggle`="collapse", 
                                              href="#collapseInput1", 
                                              `aria-expanded`="false", 
                                              `aria-controls`="collapseInput1",
                                              i18n$t("Ohje"),
                                              bsicons::bs_icon("chevron-down", size = "1rem"),
                                              tags$span(class="visually-hidden-focusable", i18n$t("Vanhemman A palkkatulojen kirjaamisohje"))),
                                       tags$div(class="collapse", id="collapseInput1",
                                                tags$p(i18n$t("Ilmoita vanhemman kuukausipalkka bruttona eli ennen verojen vähentämistä.")),
                                                tags$p(i18n$t("Palkkatiedon perusteella lasketaan vanhempainrahan suuruus. Laskelmassa oletetaan, että vanhempi työskentelee tällä palkalla koko sen ajan, jolloin hän ei saa vanhempainrahaa tai kotihoidon tukea."))
                                       )),
                                     value = 2100, step = 100, min = 0
                        ),
                        numericInput(inputId = "foo_input_T2",
                                     label = tags$div(
                                       tags$strong(i18n$t("Vanhemman B palkka (e/kk)")),tags$br(),
                                       tags$a(`data-bs-toggle`="collapse", 
                                              href="#collapseInput2", 
                                              `aria-expanded`="false", 
                                              `aria-controls`="collapseInput2",
                                              i18n$t("Ohje"),
                                              bsicons::bs_icon("chevron-down", size = "1rem"),
                                              tags$span(class="visually-hidden-focusable", i18n$t("Vanhemman B palkkatulojen kirjaamisohje"))),
                                       tags$div(class="collapse", id="collapseInput2",
                                                tags$p(i18n$t("Ilmoita vanhemman kuukausipalkka bruttona eli ennen verojen vähentämistä.")),
                                                tags$p(i18n$t("Palkkatiedon perusteella lasketaan vanhempainrahan suuruus. Laskelmassa oletetaan, että vanhempi työskentelee tällä palkalla koko sen ajan, jolloin hän ei saa vanhempainrahaa tai kotihoidon tukea."))
                                       )),
                                     value = 2600, step = 100, min = 0
                        ),
                        numericInput(inputId = "foo_input_kokhoitoaika",
                                     label = tags$div(
                                       tags$strong(i18n$t("Aika, joka lasta hoidetaan kotona (1–24 kk)")),tags$br(),
                                       tags$a(`data-bs-toggle`="collapse", 
                                              href="#collapseInput3", 
                                              `aria-expanded`="false", 
                                              `aria-controls`="collapseInput3",
                                              i18n$t("Ohje"),
                                              bsicons::bs_icon("chevron-down", size = "1rem"),
                                              tags$span(class="visually-hidden-focusable", i18n$t("Lapsen kokonaishoitoajan kirjaamisohje"))),
                                       tags$div(class="collapse", id="collapseInput3",
                                                tags$p(i18n$t("Ilmoita aika, jonka perhe hoitaa lasta kotona lapsen kahteen ikävuoteen asti.")),
                                                tags$p(i18n$t("Vertailu esittää erilaisia tapoja jakaa valittu kotihoitoaika vanhempien kesken. Perheen brutto- ja nettotulot lasketaan aina kahden vuoden ajalta, vaikka lasta hoidettaisiin kotona alle 24 kk."))
                                       )),
                                     value = 18, step = 1, min = 1, max = 24
                        ),
                        numericInput(inputId = "foo_input_kotihoidontuki",
                                     label = tags$div(
                                       tags$strong(i18n$t("Kotihoidon tuki (e/kk)")),tags$br(),
                                       tags$a(`data-bs-toggle`="collapse", 
                                              href="#collapseInput4", 
                                              `aria-expanded`="false", 
                                              `aria-controls`="collapseInput4",
                                              i18n$t("Ohje"),
                                              bsicons::bs_icon("chevron-down", size = "1rem"),
                                              tags$span(class="visually-hidden-focusable", i18n$t("Kotihoidontuen määrän kirjaamisohje"))),
                                       tags$div(class="collapse", id="collapseInput4",
                                                tags$p(i18n$t("Ilmoita, kuinka paljon perhe saa kotihoidon tukea kuukaudessa.")), 
                                                tags$p(i18n$t("Yhdestä lapsesta tuki on noin 380 euroa kuukaudessa. Voit muuttaa euromäärää, jos perheessä on muita kotona hoidettavia sisaruksia tai saat kotihoidon tuen kuntalisää tai tulojen perusteella määräytyvää hoitolisää. Lisätietoa kotihoidon tuen määrästä saat osoitteesta"), 
                                                       tags$a(href = "https://www.kela.fi/kotihoidontuki/", 
                                                              # href = i18n$t("https://www.kela.fi/kotihoidontuki/"), 
                                                              target="_blank", 
                                                              i18n$t("kela.fi/kotihoidontuki")
                                                       )))),
                                     value = 380, step = 10, min = 0, max = 2000
                        ),
                        actionButton(inputId = "button_01", label = i18n$t("Päivitä laskelma"), 
                                     class="m-1 kds-theme-kela kds-btn kds-btn--solid kds-btn--primary")
               )
      ),
      
      #   _   __                 _  _   _ _     _ _   
      #  | | / /                | |(_) (_) |   (_) |  
      #  | |/ / _   ___   ____ _| |_ __ _| |__  _| |_ 
      #  |    \| | | \ \ / / _` | __/ _` | '_ \| | __|
      #  | |\  \ |_| |\ V / (_| | || (_| | |_) | | |_ 
      #  \_| \_/\__,_| \_/ \__,_|\__\__,_|_.__/|_|\__|
      #                                               
      #
      tags$div(style = "padding-top: 40px"),
      fluidRow(class = "container_1280", style = "padding-top: 60px; padding-bottom 30px;",
               shinycssloaders::withSpinner(
                 uiOutput("html_table_01")
                 , type = 4, color = "#003580"),
      ),
      fluidRow(class = "container_1280", style = "padding-top: 60px; padding-bottom 30px;",
               uiOutput("ui_plot_bar")
               # shinycssloaders::withSpinner(plotOutput("foo_plot_02", width = "100%", height = 980), type = 4, color = "#003580")
               
      ),
      
      #    ___  _       _           _ _             _ 
      #   / _ \| |     | |         (_) |           (_)
      #  / /_\ \ | __ _| |__   __ _ _| |_ __ _ _ __ _ 
      #  |  _  | |/ _` | '_ \ / _` | | __/ _` | '__| |
      #  | | | | | (_| | | | | (_| | | || (_| | |  | |
      #  \_| |_/_|\__,_|_| |_|\__,_|_|\__\__,_|_|  |_|
      #                                               
      #                                               
      
      
      tags$footer(
        fluidRow(class = "container_1280", style = "padding-top: 5px; padding-bottom 30px;",
                 tags$div(class = "col-sm-8",
                          tags$div(class="kds-theme-kela kds-accordion accordion", id="accordionExtra",
                                   tags$div(class="accordion-item kds-theme-kela kds-accordion__toggle",
                                            tags$h2(class="accordion-header", id="flush-headingOne1", 
                                                    tags$button(class="accordion-button kds-no-focus-ring collapsed", 
                                                                type="button", 
                                                                `data-bs-toggle`="collapse", 
                                                                `data-bs-target`="#flush-collapseOne1", 
                                                                `aria-expanded`="false", 
                                                                `aria-controls`="flush-collapseOne1",
                                                                i18n$t("Tietoa perhevapaavertailusta")
                                                    )
                                            ),
                                            tags$div(id="flush-collapseOne1", 
                                                     class="accordion-collapse collapse",
                                                     `aria-labelledby`="flush-headingOne1",
                                                     `data-bs-parent`="#accordionExtra",
                                                     tags$div(class="kds-accordion__body kds-collapse kds-collapse--visible",
                                                              tags$p(i18n$t("Perhevapaavertailun brutto- ja nettotulojen laskennassa otetaan huomioon vanhempien palkkatulot, vanhempainraha ja kotihoidon tuki. Vanhempainrahan määrä lasketaan vertailuun syötettyjen palkkatulojen perusteella. Pienituloiselle tai tulottomalle vertailussa lasketaan vanhempainrahan vähimmäismäärä (31,99 euroa päivässä vuonna 2024), mutta mahdollisia muita pienituloisen tai tulottoman saamia etuuksia ei oteta huomioon. Vertailu ei siten sovi tilanteisiin, joissa toinen vanhempi on esimerkiksi työttömänä.")),
                                                              tags$p(i18n$t("Nettotulot lasketaan vähentämällä bruttotuloista verot ja sosiaalivakuutusmaksut. Verojen laskentaa varten tulot jaetaan tasan kahdelle verovuodelle. Verot ja sosiaaliturvamaksut lasketaan vuoden 2024 lainsäädännön ja keskimääräisen kunnallisveroprosentin (7,46 %) mukaan. Kirkollisveroa ei oteta laskennassa huomioon. Laskennassa otetaan huomioon Verohallinnon viran puolesta tekemät vähennykset, mutta ei esimerkiksi työmatkavähennystä.")),
                                                              tags$p(i18n$t("Laskennassa ei oteta huomioon mahdollista työnantajan vanhempainvapaan ajalta maksamaa palkkaa. Laskennassa on oletettu, että vanhemmat eivät käytä vanhempainrahaa yhtä aikaa, vaan toisen vanhemman saadessa etuutta toinen työskentelee ja saa ilmoitettua palkkaa.")),
                                                              tags$p(i18n$t("Vertailun tulokset ovat suuntaa antavia. Perheen todellisiin tuloihin vaikuttavat myös muut kuin tässä käsitellyt palkka- ja perhe-etuustulot. Nettotuloihin vaikuttavat myös esimerkiksi tulojen jaksottuminen verovuosille ja verotuksessa itse ilmoitettavat vähennykset.")),
                                                              tags$p(i18n$t("Voit arvioida perhe-etuuksien tarkkoja määriä ja tuloistasi perittävien sosiaalivakuutusmaksujen ja verojen määriä Kelan ja Verohallinnon laskureiden avulla.")),
                                                              tags$ul(
                                                                tags$li(tags$a(href="https://laskurit.kela.fi/raskaus-ja-vanhempainrahapaivien-laskuri/", 
                                                                               # href=i18n$t("https://laskurit.kela.fi/raskaus-ja-vanhempainrahapaivien-laskuri/"), 
                                                                               target="_blank", 
                                                                               i18n$t("Raskaus- ja vanhempainrahapaivien laskuri"))),
                                                                tags$li(tags$a(href="https://laskurit.kela.fi/vanhempainpaivaraha-laskuri", 
                                                                               # href=i18n$t("https://laskurit.kela.fi/vanhempainpaivaraha-laskuri"), 
                                                                               target="_blank", 
                                                                               i18n$t("Vanhempainpäivärahan määrän laskuri"))),
                                                                tags$li(tags$a(href="https://laskurit.kela.fi/lastenhoidon-tukien-maaran-laskuri/", 
                                                                               # href=i18n$t("https://laskurit.kela.fi/lastenhoidon-tukien-maaran-laskuri/"), 
                                                                               target="_blank", 
                                                                               i18n$t("Lastenhoidon tuen määrän laskuri"))),
                                                                tags$li(tags$a(href="https://vero.fi/henkiloasiakkaat/verokortti-ja-veroilmoitus/verokortti/veroprosenttilaskuri/", 
                                                                               # href=i18n$t("https://vero.fi/henkiloasiakkaat/verokortti-ja-veroilmoitus/verokortti/veroprosenttilaskuri/"), 
                                                                               target="_blank", 
                                                                               i18n$t("Veroprosenttilaskuri (vero.fi)")))
                                                              )  
                                                     )
                                                     
                                                     
                                            )
                                            
                                            
                                   )),
                          tags$div(class="kds-theme-kela kds-accordion accordion", id="accordionExtra2",
                                   tags$div(class="accordion-item kds-theme-kela kds-accordion__toggle",
                                            tags$h2(class="accordion-header", id="flush-headingTwo1", 
                                                    tags$button(class="accordion-button kds-no-focus-ring collapsed", 
                                                                type="button", 
                                                                `data-bs-toggle`="collapse", 
                                                                `data-bs-target`="#flush-collapseTwo1", 
                                                                `aria-expanded`="false", 
                                                                `aria-controls`="flush-collapseTwo1",
                                                                i18n$t("Saavutettavuusseloste")
                                                    )
                                            ),
                                            tags$div(id="flush-collapseTwo1", 
                                                     class="accordion-collapse collapse",
                                                     `aria-labelledby`="flush-headingTwo1",
                                                     `data-bs-parent`="#accordionExtra",
                                                     tags$div(class="kds-accordion__body kds-collapse kds-collapse--visible",
                                                              tags$p(i18n$t("Tämä saavutettavuusseloste koskee Perhevapaavertailu -verkkosovellusta. Seloste on laadittu 7.12.2022. Verkkosivuston saavutettavuus on arvioitu Kelassa.")),
                                                              tags$h3(i18n$t("Miten saavutettava verkkopalvelu on?")),
                                                              tags$p(i18n$t("Saavutettavuusvaatimukset on huomioitu, ja palvelu täyttää kriittiset saavutettavuusvaatimukset (WCAG-kriteeristö 2.1 A- ja AA-tasot).")),
                                                              tags$h3(i18n$t("Sisällöt, jotka eivät ole saavutettavia")),
                                                              tags$p(i18n$t("Käyttäjät saattavat edelleen kohdata sivustolla joitakin saavutettavuusongelmia. Jos huomaat sivustolla ongelman, ilmoitathan siitä meille.")),
                                                              tags$h3(i18n$t("Anna palautetta saavutettavuudesta")),
                                                              tags$ul(
                                                                tags$li(tags$a(href = "https://www.kela.fi/saavutettavuuspalaute", 
                                                                               # href = i18n$t("https://www.kela.fi/saavutettavuuspalaute"), 
                                                                               i18n$t("verkkolomakkeella")))
                                                              ),
                                                              tags$p(i18n$t("Saavutettavuuspalautteet Kelassa vastaanottaa Kelan tekninen tuki.")),
                                                              tags$h3(i18n$t("Saavutettavuuden valvonta")),
                                                              tags$p(i18n$t("Jos huomaat sivustolla saavutettavuuteen liittyviä ongelmia, anna ensin palautetta meille. Vastaamme 2 viikon sisällä.")),
                                                              tags$p(i18n$t("Jos et ole tyytyväinen saamaasi vastaukseen tai jos et saa vastausta 2 viikon aikana,"), 
                                                                     tags$a(href = "https://www.saavutettavuusvaatimukset.fi/oikeutesi/",
                                                                            # href = i18n$t("https://www.saavutettavuusvaatimukset.fi/oikeutesi/"), 
                                                                            i18n$t("voit tehdä ilmoituksen Etelä-Suomen aluehallintovirastoon"))),
                                                              tags$h3(i18n$t("Valvontaviranomaisen yhteystiedot")),
                                                              tags$p(tags$strong(i18n$t("Etelä-Suomen aluehallintovirasto")),tags$br(),
                                                                     i18n$t("Saavutettavuuden valvonnan yksikkö"),tags$br(),
                                                                     tags$a(href = "https://www.saavutettavuusvaatimukset.fi/",
                                                                            # href = i18n$t("https://www.saavutettavuusvaatimukset.fi/"), 
                                                                            i18n$t("www.saavutettavuusvaatimukset.fi")),tags$br(),
                                                                     i18n$t("saavutettavuus(at)avi.fi"), tags$br(),
                                                                     i18n$t("puhelinnumero vaihde 0295 016 000")),
                                                              tags$h3(i18n$t("Teemme jatkuvasti työtä saavutettavuuden parantamiseksi")),
                                                              tags$p(i18n$t("Olemme sitoutuneet parantamaan verkkopalveluiden saavutettavuutta. Varmistamme saavutettavuuden mm. seuraavilla toimenpiteillä:")),
                                                              tags$ul(
                                                                tags$li(i18n$t("Otamme saavutettavuusvaatimukset huomioon, kun kehitämme palveluita.")),
                                                                tags$li(i18n$t("Panostamme kielen ymmärrettävyyteen.")),
                                                                tags$li(i18n$t("Otamme saavutettavuusvaatimukset huomioon, kun teemme hankintoja.")),
                                                                tags$li(i18n$t("Tarjoamme työntekijöillemme saavutettavuuskoulutusta."))
                                                              ),
                                                              tags$p(tags$a(href="https://www.finlex.fi/fi/laki/alkup/2019/20190306",
                                                                            # i18n$t("https://www.finlex.fi/fi/laki/alkup/2019/20190306"), 
                                                                            target="_blank", 
                                                                            i18n$t("Laki digitaalisten palveluiden tarjoamisesta (finlex.fi)")))
                                                     )
                                            ))
                          ),
                          tags$code(i18n$t("Sovelluksen versio"),":", golem::get_golem_version()),tags$br(),
                          tags$a(href = "https://github.com/kelaresearchandanalytics/perhevapaavertailu", i18n$t("Lähdekoodi"), target = "_blank")
                 )
        )
      ),
      
      tags$div(style = "padding-top: 50px;"),
      tags$html(HTML('</main>'))
    ))
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www/')
  )
  
  tags$head(
    favicon()
    # ,bundle_resources(
    #   path = app_sys('app/www/kds2'),
    #   app_title = 'Perhevapaavertailu'
    # )
    # , tags$link(rel="shortcut icon", href="https://perhevapaavertailu.kela.fi/www/favicon.ico")
    # , tags$link(rel="stylesheet", href="https://cdn.jsdelivr.net/npm/bootstrap-icons@1.7.1/font/bootstrap-icons.css")
    # , tags$link(rel="stylesheet", href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css")
    # , tags$link(rel="stylesheet", href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.2/dist/css/bootstrap.min.css")
    # , tags$script(src="https://cdn.jsdelivr.net/npm/bootstrap@5.3.2/dist/js/bootstrap.bundle.min.js")
    # , tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/fancybox/3.5.7/jquery.fancybox.min.js")
    # , tags$link(rel="stylesheet", href="www/custom.css")
    , tags$link(rel="stylesheet", href="www/kds/kds-theme-kela.css")
    , tags$link(rel="stylesheet", href="www/peva.css")
    
  )
}
