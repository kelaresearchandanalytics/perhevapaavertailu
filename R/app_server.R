#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shiny.i18n
#' @import ggplot2
#' @import patchwork
#' @keywords internal
#' @noRd
app_server <- function( input, output, session ) {
  
  # output$texti <- renderText({input$input_lang})
  
  i18n <- golem::get_golem_options(which = "translator")
  i18n$set_translation_language("fi")
  
  # keep track of language object as a reactive
  i18n_r <- reactive({
    i18n
  })
  
  # change language
  observeEvent(input[["selected_language"]], {
    shiny.i18n::update_lang(session = session, language = input[["selected_language"]])
    i18n_r()$set_translation_language(input[["selected_language"]])
  })
  
  # haetaan kielivalinta urlista
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['lang']])) {
      updateSelectInput(session, "selected_language", selected = query[['lang']])
    }
  })
  
  observeEvent(input[["selected_language"]], {
    updateQueryString(
      queryString = paste0("?lang=", input[["selected_language"]]),
      mode = "replace"
    )
  })
  
  
  
  get_parameter_user_shiny <- reactive({
    tibble(kokhoitoaika = as.numeric(sub(",", ".", input$foo_input_kokhoitoaika)),
           T1 = as.numeric(sub(",", ".", input$foo_input_T1)),
           T2 = as.numeric(sub(",", ".", input$foo_input_T2)),
           kotihoidontuki_e_per_kk = as.numeric(sub(",", ".", input$foo_input_kotihoidontuki))
    )
  }) 
  
  get_parameter_kokhoitoaika_shiny <- reactive({
    return(as.numeric(sub(",", ".", input$foo_input_kokhoitoaika)))
  }) 
  
  validate_inputs <- function(x, which_input = "tulot"){
    
    x <- sub(",", ".", x)
    res1 <- !any(grepl("^[0-9]{1,10}([,.][0-9]{1,10})?$", x))
    if (!res1){
      if (which_input == "kokhoitoaika"){
        if (as.numeric(x) > 0) res = FALSE else res = TRUE 
      } else {
        if (as.numeric(x) >= 0) res = FALSE else res = TRUE  
      }
    } else res = TRUE
    return(!res)
  }
  
  
  validate_input_parameters <- reactive({
    
    all(
      validate_inputs(input$foo_input_T1),
      validate_inputs(input$foo_input_T2),
      validate_inputs(input$foo_input_kokhoitoaika, which_input = "kokhoitoaika"),
      validate_inputs(input$foo_input_kotihoidontuki)
    )
    
  })
  
  
  
  
  observe({
    value <- validate_input_parameters()
    if (!value){
      shinyjs::disable("button_01")
    } else {
      shinyjs::enable("button_01")
    }
  })
  
  output$txt <- renderText({
    validate_input_parameters()
  })
  
  
  # inputtien herjat!
  
  observeEvent(input$foo_input_T1, {
    if (!validate_inputs(input$foo_input_T1)){
      
      showFeedbackWarning(
        icon = NULL,
        inputId = "foo_input_T1",
        text = i18n$t("Anna numeerinen arvo"),
        color = "#ec3484")
      # }
    } else {
      hideFeedback("foo_input_T1")
    }
  })
  
  observeEvent(input$foo_input_T2, {
    if (!validate_inputs(input$foo_input_T2)){
      
      showFeedbackWarning(
        icon = NULL,
        inputId = "foo_input_T2",
        text = i18n$t("Anna numeerinen arvo"),
        color = "#ec3484")
      # }
    } else {
      hideFeedback("foo_input_T2")
    }
    
  })
  
  observeEvent(input$foo_input_kokhoitoaika, {
    if (!validate_inputs(input$foo_input_kokhoitoaika, which_input = "kokhoitoaika")){
      showFeedbackWarning(
        icon = NULL,
        inputId = "foo_input_kokhoitoaika",
        text = i18n$t("Anna numeerinen arvo"),
        color = "#ec3484")
      # } else if (as.numeric(sub(",", ".", input$foo_input_kokhoitoaika)) > 24) {
    } else if (as.numeric(sub(",", ".", input$foo_input_kokhoitoaika)) > 24 | as.numeric(sub(",", ".", input$foo_input_kokhoitoaika)) < 1) {
      showFeedbackWarning(
        icon = NULL,
        inputId = "foo_input_kokhoitoaika",
        text = i18n$t("Kokonaiskotihoitoajan vähimmäiskesto on 1 ja enimmäiskesto on 24 kuukautta. Yli 24 arvoja ei huomioida"), 
        color = "#ec3484")
    } else {
      hideFeedback("foo_input_kokhoitoaika")
    }
  }, ignoreNULL = FALSE)
  
  
  
  observeEvent(input$foo_input_kotihoidontuki, {
    if (!validate_inputs(input$foo_input_kotihoidontuki)){
      showFeedbackWarning(
        icon = NULL,
        inputId = "foo_input_kotihoidontuki",
        text = i18n$t("Anna numeerinen arvo"),
        color = "#ec3484")
    } else if (as.numeric(sub(",", ".", input$foo_input_kotihoidontuki)) > 2000) {
      showFeedbackWarning(
        icon = NULL,
        inputId = "foo_input_kotihoidontuki",
        text = i18n$t("Kotihoidontuen vähimmäismäärä on 0e ja enimmäismäärä on 2000e. Yli 2000e arvoja ei huomioida."), 
        color = "#ec3484")
    } else {
      hideFeedback("foo_input_kotihoidontuki")
    }
  }, ignoreNULL = FALSE)
  
  
  observeEvent({
    input$button_01
  }, {
    
    params_user <- get_parameter_user_shiny()
    params_user$systime <- Sys.time()
    params_user$token <- session$token
    params_user$lang <- input$selected_language
    params_user$node <- Sys.info()[["nodename"]]
    try(readr::write_csv(x = params_user, file = "/mnt/pevacsv/sessio.csv", append = TRUE))
    
  }, ignoreNULL = FALSE)
  
  
  
  # ***************************************************************************
  # ***************************************************************************
  # PLOT PLOT PLOT PLOT
  # ***************************************************************************
  # ***************************************************************************
  
  plot_height <- reactive({
    height <- ifelse(get_parameter_user_shiny()[["kokhoitoaika"]] > 12, 950, ifelse(get_parameter_user_shiny()[["kokhoitoaika"]] > 7, 750, 520))
    return(height)
  })
  
  draw_plot <- function(final_list){
    
    theme_set(theme_minimal(base_family = "Lato",
                            base_size = 16) +
                theme(plot.subtitle = element_text(size = 14, family = "Noto Sans", colour = "black"),
                      plot.title = element_text(color = "#003580", size = 20, family = "Noto Sans"),
                      # 
                      strip.text = element_text(hjust  = 0, face  = "plain", size = 14, family = "Noto Sans", color = "black"),
                      # 
                      axis.text.y = element_blank(),
                      axis.title.y = element_blank(),
                      #
                      axis.title.x = element_blank(),
                      axis.text.x = element_blank(),
                      #
                      legend.title = element_blank(),
                      legend.text = element_text(color = "black", size = 12, family = "Lato"),
                      panel.grid = element_blank(),
                      # 
                      plot.caption = element_text(family = "Ubuntu Mono", hjust = 1, color = "dim grey")
                )
    )
    
    palette_parent_b <- c("#6b6b6b", # Kela-gray 60
                          "#00265f", # bright-blue 70
                          "#2a69c5", # bright-blue 50
                          "#6b6b6b" # Kela-gray 60)
    )
    palette_parent_a <- c("#00265f", # bright-blue 70
                          "#2a69c5", # bright-blue 50
                          "#6b6b6b", # Kela-gray 60
                          "#6b6b6b" # Kela-gray 60))
    )
    
    option_lst <- list()
    option_nro <- as.integer(sub("option", "", names(final_list)))
    for (i in seq(final_list)){
      dlist <- list()
      for (p in 1:2){
        ptmp <- final_list[[i]][[p]]
        dlist[[p]] <- bind_rows(
          tibble(
            pv = ptmp$vpr_pv,
            mode = "vanhempainraha",
            parent = p
          ),
          tibble(
            pv = ptmp$tyossa_pv,
            mode = "työssä",
            parent = p
          ),
          tibble(
            pv = ptmp$kotiho_pv,
            mode = "kotihoidontuki",
            parent = p
          )
        )
      }
      doption <- do.call("bind_rows", dlist)
      doption$option <- option_nro[i]
      option_lst[[i]] <- doption
    }
    imgdata_raw <- do.call("bind_rows", option_lst)
    
    imgdata_parent_a <- imgdata_raw %>% 
      filter(parent == 1) %>% arrange(option,mode)
    
    tyossa_parent_b <- imgdata_parent_a %>% 
      group_by(option) %>% 
      filter(mode %in% c("vanhempainraha","kotihoidontuki")) %>% 
      summarise(pv1 = sum(pv)) %>% 
      mutate(mode1 = "tyossa0",
             parent = 2) %>% 
      left_join(imgdata_raw %>% 
                  filter(parent == 2, mode == "työssä")) %>% 
      mutate(pv2 = pv-pv1) %>% 
      mutate(pv2 = ifelse(option == 0, 0, pv2),
             pv1 = ifelse(option == 0, 600, pv1))
    
    
    imgdata_parent_b <- bind_rows(
      imgdata_raw %>% 
        filter(parent == 2,
               mode %in% c("vanhempainraha","kotihoidontuki")),
      # for parent b tyossa1 mode is while parent a is either on vanhempainraha or kotihoidontuki
      tyossa_parent_b %>% 
        mutate(pv = pv2) %>% 
        select(option,pv,mode,parent),
      tyossa_parent_b %>% 
        mutate(pv = pv1,
               mode = "tyossa0") %>% 
        select(option,pv,mode,parent)
    )
    
    
    kuvadata <- bind_rows(
      imgdata_parent_a,
      imgdata_parent_b
    ) %>% 
      mutate(mode = factor(mode, levels = c("tyossa0","vanhempainraha","kotihoidontuki","työssä"))) %>% 
      group_by(option,parent) %>% 
      arrange(mode) %>% 
      mutate(lab_pos = cumsum(pv) - pv/2,
             lab_pos = ifelse(is.na(lab_pos), pv/2, lab_pos),
             pv = ifelse(pv == 0, NA, pv)) %>%
      ungroup() %>% 
      arrange(parent,mode) %>% 
      mutate(pv = round(pv/25,1),
             lab_pos = lab_pos/25,
             parent_lab = paste0(i18n$t("Vanhempi")," ", parent))
    
    
    imglist <- list()
    # for (i in 1){
    options <- unique(kuvadata$option)
    for (i in seq(options)){
      kuvadata_tmp <- kuvadata[kuvadata$option == options[i], ] %>% 
        arrange(mode)
      titles <- add_static_texts(option = options[i])
      # parent A
      pa <- ggplot(kuvadata_tmp[kuvadata_tmp$parent == 1,], 
                   aes(y = parent_lab, x = pv, fill = mode)) +
        geom_vline(xintercept = 12, linetype = "solid", color = "dim grey") +
        geom_col(position=position_stack(vjust = .5, reverse = TRUE), 
                 color = "white", 
                 linewidth = 1, 
                 alpha = 1) + 
        geom_label(aes(x = lab_pos, label = pv),
                   fill = NA,
                   color = "white",
                   label.size = 0,
                   family = "Lato",
                   fontface = "bold",  
                   size = 5,
                   label.padding = unit(.2, "lines")) +
        scale_fill_manual(values = palette_parent_a,
                          guide = guide_legend(reverse = FALSE, 
                                               nrow=1,
                                               byrow=TRUE),
                          labels = c(i18n$t("vanhempainraha"),
                                     i18n$t("kotihoidontuki"),
                                     i18n$t("työssä"))) +
        labs(title = i18n$t(titles$title),
             subtitle = i18n$t(titles$subtitle)) + 
        theme(legend.position = "top") +
        theme(plot.margin = unit(c(30, 0, 5, 0), "pt"),
              plot.title = element_text(size = 16)) + 
        theme(legend.position = "top",
              legend.text = element_text(size = 14),
              legend.box.just = "left",
              axis.text.y = element_text(size = 14), 
              plot.title.position = "plot",
              legend.justification = 0)
      
      
      # parent B
      val <- 1:24
      pb <- ggplot(kuvadata_tmp[kuvadata_tmp$parent == 2,], 
                   aes(y = parent_lab, x = pv, fill = mode)) + 
        geom_vline(xintercept = 12, linetype = "solid", color = "dim grey") +
        geom_col(position=position_stack(vjust = .5, reverse = TRUE), 
                 color = "white", 
                 linewidth = 1, 
                 alpha = 1) + 
        geom_label(aes(x = lab_pos, label = pv),
                   fill = NA,
                   color = "white",
                   label.size = 0,
                   family = "Lato",
                   fontface = "bold",  
                   size = 5,
                   label.padding = unit(.2, "lines")) +
        scale_fill_manual(values = palette_parent_b,
                          guide = guide_legend(reverse = FALSE, 
                                               nrow=1,
                                               byrow=TRUE),
                          labels = c(i18n$t("vanhempainraha"),
                                     i18n$t("kotihoidontuki"),
                                     i18n$t("työssä"))) +
        theme(legend.position = "none") +
        theme(plot.margin = unit(c(10, 0, 5, 0), "pt"), 
              plot.title = element_text(size = 16)) +
        theme(legend.position = "none",
              axis.text.x = element_text(),
              axis.text.y = element_text(size = 14),
              axis.title.x = element_text(hjust = 1, size = 10)) +
        scale_x_continuous(breaks = val[lapply(val, "%%", 4) == 0],
                           labels = val[lapply(val, "%%", 4) == 0]) + 
        labs(x = i18n$t("kuukautta"))
      
      
      
      imglist[[i]] <- wrap_plots(list(pa,pb), ncol = 1) + 
        plot_annotation(theme = theme_minimal(base_size = 20))
    }
    if (length(options) == 4){
      heights <- c(1,1,1,1)  
    } else if (length(options) == 3){
      heights <- c(1.3,1.3,1.3)  
    } else {
      heights <- c(1.75,1.75)  
    }
    outplot <- wrap_plots(imglist, ncol = 1, heights = heights) + plot_annotation(title = i18n$t("Vapaajaksojen jakautuminen vanhempien kesken eri vaihtoehdoissa"))
    return(outplot)
  }
  
  
  output$barplot <- renderPlot({
    
    req(input$selected_language)
    param_user <- get_parameter_user_shiny()
    options <- which_options_to_compute(home_care_months = param_user$kokhoitoaika)
    final_list <- compute_options_final(option_to_compute = options,
                                        param_user = param_user)
    draw_plot(final_list)
    
  }, 
  alt = i18n$t("Kuvio havainnollistaa tulostaulukon alarivillä olevat tekstiselitteet. Tekstiselitteissä olevat tiedot vastaavat kuvassa esitettäviä tietoja.")
  ) %>% 
    bindEvent(input$button_01,
              input$selected_language, ignoreNULL = FALSE)
  
  output$ui_plot_bar <- renderUI({
    
    tagList(
      tags$div(style='width:100%; overflow-x: auto; overflow-y: hidden;',
               shinycssloaders::withSpinner(plotOutput("barplot", width = 820, height = plot_height()), type = 4, color = "#003580")
      )
    )
  })  
  
  
  # ***************************************************************************
  # ***************************************************************************
  # TABLE TABLE TABLE TABLE
  # ***************************************************************************
  # ***************************************************************************
  

  add_static_texts <- function(option_nr = 0){
    
    # Staattiset tekstit jotka pysyvät aina samoin ol käännös
    texts <- list(
      option0 = list(title = i18n$t("Vertailukohta"),
                     subtitle = i18n$t("Vain vanhempi A käyttää vapaita")),
      option1 = list(title = paste0(i18n$t("Vaihtoehto")," 1"),
                     subtitle = i18n$t("Vanhempi B käyttää 97 vanhempainrahapäivää ja vanhempi A muut vapaat")),
      option2 = list(title = paste0(i18n$t("Vaihtoehto")," 2"),
                     subtitle = i18n$t("Vanhempainrahapäivät jaetaan tasan")),
      option3 = list(title = paste0(i18n$t("Vaihtoehto")," 3"),
                     subtitle = i18n$t("Vanhempainrahapäivät ja kotihoidon tuki jaetaan tasan"))
    )
    
    txt <- texts[[paste0("option",option_nr)]]
    return(txt)
  }
  
  
  null_to_zero <- function(x) ifelse(is.null(x), 0, x)
  

  create_card <- function(option_nr = 1, res_lst){
    dcol <- res_lst[[paste0("option",option_nr)]]
    tcol <- add_static_texts(option = option_nr)
    
    up_arrow <- tags$span(style="color:#067c30", HTML("&uArr;"))
    down_arrow <- tags$span(style="color:#b8405b", HTML("&dArr;"))
    
    if (dcol$household$brutto_change > 0){
      arrow_brutto <- tags$span(style="color:#067c30", HTML("&uArr;"))
      sign_brutto <- "+"
    } else {
      arrow_brutto <- tags$span(style="color:#b8405b", HTML("&dArr;"))
      sign_brutto <- "" # - added automatically if negative
    }
    
    if (dcol$household$netto_change > 0){
      arrow_netto <- tags$span(style="color:#067c30", HTML("&uArr;"))
      sign_netto <- "+"
    } else {
      arrow_netto <- tags$span(style="color:#b8405b", HTML("&dArr;"))
      sign_netto <- "" # - added automatically if negative
    }
    
    tags$div(class = "col",
             tags$div(class = "card card-peva",
                      # heading
                      tags$div(class = "card-header card-class-head",
                               tags$h4(class = "card-title peva-card-title", tcol$title),
                               tags$p(class = "card-text", tcol$subtitle)
                      ),
                      tags$div(class = "card-body card-class-body",
                               # body
                               tags$ul(class="list-unstyled card-text", style="padding-top: 15px",
                                       tags$li(
                                         tags$strong(i18n$t("kokonaistulot"))
                                       ),
                                       tags$ul(class = "peva-list-item",
                                               tags$li(paste0(i18n$t("brutto"),": ", 
                                                              format(plyr::round_any(null_to_zero(dcol$household$brutto_income_per_year*2), 10), big.mark = " ", nsmall = 0, digits = 1, scientific = FALSE), " e"
                                               )),
                                               tags$li(paste0(i18n$t("netto"),": ", 
                                                              format(plyr::round_any(null_to_zero(dcol$household$netto_income_per_year*2), 10), big.mark = " ", nsmall = 0, digits = 1, scientific = FALSE), " e"
                                               ))
                                       )
                               ),
                               if (option_nr != 0){
                                 tags$ul(class="list-unstyled card-text", style="padding-top: 15px",
                                         tags$li(
                                           tags$strong(i18n$t("muutos"))
                                         ),
                                         tags$ul(class = "peva-list-item",
                                                 tags$li(paste0(i18n$t("brutto"),": ",sign_brutto, 
                                                                format(plyr::round_any(null_to_zero(dcol$household$brutto_change*2), 10), big.mark = " ", nsmall = 0, digits = 1, scientific = FALSE), " e "
                                                 ), arrow_brutto),
                                                 tags$li(paste0(i18n$t("netto"),": ",sign_netto, 
                                                                format(plyr::round_any(null_to_zero(dcol$household$netto_change*2), 10), big.mark = " ", nsmall = 0, digits = 1, scientific = FALSE), " e "
                                                 ), arrow_netto)
                                         )
                                 )
                               }
                      ),
                      # footer
                      tags$div(class = "card-footer card-class-footer",
                               tags$ul(class="list-unstyled card-text",
                                       tags$li(
                                         tags$strong(paste0(i18n$t("Vanhempi")," A"))
                                       ),
                                       tags$ul(class = "peva-list-item",
                                               # tags$li(paste0("Vanhempainrahaa 160 + ", dcol$parent_a$vpr_pv - 160, " arkipäivää (", round(dcol$parent_a$vpr_pv/25,1), " kuukautta)")),
                                               tags$li(paste0(i18n$t("Vanhempainrahaa")," ", dcol$parent_a$vpr_pv, " ", i18n$t("arkipäivää")," (", round(dcol$parent_a$vpr_pv/25,1), " ",i18n$t("kuukautta"),")")),
                                               tags$li(paste0(i18n$t("Kotihoidon tukea")," ", round(dcol$parent_a$kotiho_pv/25,1)), " ",i18n$t("kuukautta")),
                                               tags$li(paste0(i18n$t("Palkkatyössä")," ", round(dcol$parent_a$tyossa_pv/25,1)," ",i18n$t("kuukautta")))
                                       ),
                                       tags$li(
                                         tags$strong(paste0(i18n$t("Vanhempi")," B"))
                                       ),
                                       tags$ul(class = "peva-list-item",
                                               if (!option_nr %in% 0:1) tags$li(paste0(i18n$t("Vanhempainrahaa")," ", dcol$parent_b$vpr_pv, " ", i18n$t("arkipäivää")," (", round(dcol$parent_b$vpr_pv/25,1), " ",i18n$t("kuukautta"),")")),
                                               if (option_nr == 1)      tags$li(paste0(i18n$t("Vanhempainrahaa")," ", dcol$parent_b$vpr_pv, " ", i18n$t("arkipäivää")," (", round(dcol$parent_b$vpr_pv/25,1), " ",i18n$t("kuukautta"),")",i18n$t(", luovuttaa 63 arkipäivää vanhemmalle A"))),
                                               if (option_nr == 0) tags$li(i18n$t("Ei käytä vanhempainrahaa, luovuttaa 63 arkipäivää vanhemmalle A")),
                                               if (option_nr == 3) tags$li(paste0(i18n$t("Kotihoidon tukea")," ", round(dcol$parent_b$kotiho_pv/25,1)), " ", i18n$t("kuukautta")),
                                               if (option_nr %in% 0:2) tags$li(i18n$t("Ei käytä kotihoidon tukea")),
                                               tags$li(paste0(i18n$t("Palkkatyössä")," ", round(dcol$parent_b$tyossa_pv/25,1)," ", i18n$t("kuukautta")))
                                               
                                       ))
                      )
             )) -> res
    return(res)   
  }
  
  
  create_card_group <- function(final_list,options_to_be_computed){
    
    tags$div(class="row row-cols-1 row-cols-sm-2 row-cols-md-2 row-cols-lg-4",
             tags$div(class = "col",
                      create_card(option_nr = 0, 
                                  res_lst = final_list)
             ),
             if (1 %in% options_to_be_computed){
               tags$div(class = "col",
                        create_card(option_nr = 1, 
                                    res_lst = final_list)
               )
             },
             if (2 %in% options_to_be_computed){
               tags$div(class = "col",
                        create_card(option_nr = 2, 
                                    res_lst = final_list)
               )
             },
             if (3 %in% options_to_be_computed){   
               tags$div(class = "col",
                        create_card(option_nr = 3, 
                                    res_lst = final_list)
               )
             }
    ) -> taulu
    return(taulu)
    
  }
  
  
  output$html_table_01 <- renderUI({
    
    
    req(input$selected_language)
    # html_table_reactive_01()
    param_user <- get_parameter_user_shiny()
    # params_user <- get_parameter_user()
    # params_year <- get_parameter_year()
    
    optiot <- which_options_to_compute(home_care_months = param_user$kokhoitoaika)
    final_list <- compute_options_final(option_to_compute = optiot,
                                        param_user = param_user)
    
    html_taulukko <- create_card_group(final_list,optiot)
    
    options_not_to_calculate <- vector()
    if (!3 %in% optiot){
      options_not_to_calculate <- c(options_not_to_calculate,3)
    }
    if (!1 %in% optiot){
      options_not_to_calculate <- c(options_not_to_calculate,1)
    }

    
    # params_user_0 <- params_user
    tagList(
      tags$h3(i18n$t("Perheen tulot ja tulojen muutos")),
      tags$p(i18n$t("Seuraavassa taulukossa näytetään perheen kokonaistulot kahden vuoden ajalta sekä esitetään, miten perheen brutto- ja nettotulot muuttuvat suhteessa vertailukohtaan, kun perhevapaita jaetaan eri tavoin.")),
      if (length(optiot) != 4){
        tags$div(class = "container_1280", style = "padding-top: 20px; ",
                 tags$div(class="kds-theme-kela kds-alert kds-alert--icon-left kds-alert--danger col-md-8",
                          tags$html(HTML('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" width="1em" height="1em" class="kds-alert__icon kds-icon kds-icon--size-s" role="img" aria-hidden="true"><path fill="none" d="M0 0h24v24H0z"></path><path fill="none" stroke="currentColor" stroke-linejoin="round" stroke-width="2" d="M16.14 2H7.86L2 7.86v8.28L7.86 22h8.28L22 16.14V7.86z"></path><path fill="none" stroke="currentColor" stroke-linecap="round" stroke-miterlimit="10" stroke-width="2" d="M12 7v6"></path><circle cx="12" cy="17" r="0.3" fill="none" stroke="currentColor" stroke-miterlimit="10" stroke-width="2"></circle></svg>')),
                          tags$div(class="kds-alert__content", 
                                   i18n$t("Vaihtoehtoa")," ",
                                   glue::glue_collapse(options_not_to_calculate, sep = ", ", last = paste0(" ", i18n$t("ja")," ")), " ", 
                                   i18n$t("ei voi laskea, kun kokonaishoitoaika on lyhyt")
                          )
                 )
        )
      },
      html_taulukko
    )
  }) %>% bindEvent(input$button_01,
                   input$selected_language, ignoreNULL = FALSE)
  
  
  
}
