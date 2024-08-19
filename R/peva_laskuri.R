#' Normaali viitospyöristys
#'
#' Suorittaa normaalin viitospyöristyksen, eli 5-desimaali pyöristetään aina ylöspäin.
#' 
#' @param x pyöristettävä desimaaliluku
#' @param n pyöristettävä desimaali
#' @return pyöristetty luku
#' @examples 
#' t1 <- round(0.5)           # t1 == 0
#' t2 <- viitospyoristys(0.5) # t2 == 1
#' t3 <- round(1.5)           # t3 == 2
#' t4 <- viitospyoristys(1.5) # t2 == 2
#' @encoding UTF-8
#' @keywords internal
#' @noRd
viitospyoristys <- function(x, n = 0){
  posneg = sign(x)
  z = abs(x) * 10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  return(z * posneg)
}




#' Get taxation year parameters for particular year 
#' 
#' @param vuosi year
#' @keywords internal
#' @noRd
get_parameter_year <- function(vuosi = 2024){
  yamli <- '
- vuosi: 2024
  pyor: -1
# vpr:n päivien määrään liittyvät parametrit (näihin tulee muutoksia vain, jos lakiin tulee muutoksia)
  max_oma: 223
  vpr_oma: 160
  max_jakamaton: 97
  korotetut: 16
# prosenttivähennyksen vähennysprosentti
  vahpros: 0.0895
# päivärahakaavojen taitteiden rajatulot ja minimi
  raja1_90: 67296
  raja1_70: 43740
  raja2_70: 67296
  minimi: 31.99
  vanh1_ika: 30
  vanh2_ika: 30
  vanh1_kirkollisvero: 0
  vanh2_kirkollisvero: 0
  #keskimääräinen kunnallisveroprosentti vuonna 2024 on 7,46
  vanh1_kunnallisvero: 0.0746
  vanh2_kunnallisvero: 0.0746
  vanh1_elaketulo: 0
  vanh2_elaketulo: 0
- vuosi: 2022
  pyor: -1
# vpr:n päivien määrään liittyvät parametrit (näihin tulee muutoksia vain, jos lakiin tulee muutoksia)
  max_oma: 223
  vpr_oma: 160
  max_jakamaton: 97
  korotetut: 16
# prosenttivähennyksen vähennysprosentti
  vahpros: 0.0983
# päivärahakaavojen taitteiden rajatulot ja minimi
  raja1_90: 61705
  raja1_70: 40106
  raja2_70: 61705
  minimi: 30.71
  vanh1_ika: 30
  vanh2_ika: 30
  vanh1_kirkollisvero: 0
  vanh2_kirkollisvero: 0
  #keskimääräinen kunnallisveroprosentti vuonna 2021 on 20,02
  vanh1_kunnallisvero: 0.2001
  vanh2_kunnallisvero: 0.2001
  vanh1_elaketulo: 0
  vanh2_elaketulo: 0  
- vuosi: 2021
  pyor: -1
# vpr:n päivien määrään liittyvät parametrit (näihin tulee muutoksia vain, jos lakiin tulee muutoksia)
  max_oma: 223
  vpr_oma: 160
  max_jakamaton: 97
  korotetut: 16
# prosenttivähennyksen vähennysprosentti
  vahpros: 0.0991
# päivärahakaavojen taitteiden rajatulot ja minimi
  raja1_90: 60225
  raja1_70: 39144
  raja2_70: 60225
  minimi: 29.05
  vanh1_ika: 30
  vanh2_ika: 30
  vanh1_kirkollisvero: 0
  vanh2_kirkollisvero: 0
  #keskimääräinen kunnallisveroprosentti vuonna 2021 on 20,02
  vanh1_kunnallisvero: 0.2002
  vanh2_kunnallisvero: 0.2002
  vanh1_elaketulo: 0
  vanh2_elaketulo: 0  

'
  yaml_list <- yaml::read_yaml(text = yamli)
  parameter_year <- plyr::ldply(.data = yaml_list, .fun = as.data.frame)
  # Valitaan vuodeksi 2022 ja tehdään kustakin muuttujan arvosta oma objekti
  parameter_year <- parameter_year[parameter_year$vuosi == vuosi,] # Ota uusin vuosi ie. eka rivi
  return(parameter_year)
}




#' Reason which options should be computed based on number of home care months
#'
#' @param home_care_months parameters set by user through user interface
#' @param year_params year parameters from get_parameter_year function
#'
#' @return list
#' @keywords internal
#' @noRd
which_options_to_compute <- function(home_care_months = 18, 
                                     year_params = get_parameter_year(vuosi = 2024)){
  
  optios_to_be_computed <- vector()
  optios_to_be_computed <- 0
  if (home_care_months*25 >= 2*year_params$max_jakamaton) optios_to_be_computed <- c(optios_to_be_computed,1)
  optios_to_be_computed <- c(optios_to_be_computed,2)
  if (home_care_months*25 > 2*year_params$vpr_oma) optios_to_be_computed <- c(optios_to_be_computed,3)
  return(optios_to_be_computed)
  
}


#' Compute distribution of days for selected option given the user and year parameters
#'
#' @param opt_nr option to be computed
#' @param param_user parameters set by user through user interface
#' @param param_year year parameters from get_parameter_year function
#'
#' @return list
#' @keywords internal
#' @noRd
compute_days <- function(opt_nr = 0,
                         param_user = parameter_user,
                         param_year = get_parameter_year(vuosi = 2024)
){
  
  
  total_homecare_months = param_user$kokhoitoaika
  income_a <- param_user$T1
  income_b <- param_user$T2
  kotihoidontuki_e_per_month <- param_user$kotihoidontuki_e_per_kk
  total_homecare_days <- floor(total_homecare_months * 25)
  total_days_two_year <- floor(24 * 25)
  
  list2env(x = param_year, envir = globalenv())
  
  # vähennetään tuloista ns. prosenttivähennys, jolloin saadaan vpr e/pv:n laskennassa käytettävä tulo
  income_a_vah<-round(12*income_a*(1-vahpros),1)
  income_b_vah<-round(12*income_b*(1-vahpros),1)
  
  vpr_90 <- function(VahTulot) {
    max(minimi,(round(0.9*(VahTulot/300)-0.575*max(0,VahTulot-raja1_90)/300 , 2)))
  }
  
  vpr_70 <- function(VahTulot) {
    max(minimi,(round(0.7*(VahTulot/300)-0.3*max(0,VahTulot-raja1_70)/300-0.15*max(0,VahTulot-raja2_70)/300 , 2)))
  }
  
  #Syötetyillä tuloilla lasketut päivärahat
  vpr1_90 <- vpr_90 (income_a_vah)
  vpr1_70 <- vpr_70 (income_a_vah)
  vpr2_90 <- vpr_90 (income_b_vah)
  vpr2_70 <- vpr_70 (income_b_vah)
  
  compute_net_income <- function(dat, brutto_salary_per_year, brutto_benefit_per_year){
    
    dat$brutto_salary_per_year <- brutto_salary_per_year
    dat$brutto_benefit_per_year <- brutto_benefit_per_year
    resdat <- henkiloverotus_2024(
      dat,
      vanh1_ika,
      vanh1_kunnallisvero,
      vanh1_kirkollisvero,
      vanh1_elaketulo,
      brutto_salary_per_year,
      brutto_benefit_per_year
    )
    return(resdat$netto_maksujen_jalkeen)
  }
  
  
  if (opt_nr == 0){
    # Vain vanhempi A käyttää vapaita
    
    ## Parent A
    a <- list()
    a$vpr_pv <- pmin(total_homecare_days,param_year$max_oma)
    #a$vpr_pv_year1 <- pmin (ceiling(total_homecare_days/2), a$vpr_pv) 
    #a$vpr_pv_year2 <- a$vpr_pv - a$vpr_pv_year1 
    a$vpr_90_pv <- pmin(korotetut,a$vpr_pv)
    a$vpr_70_pv <- a$vpr_pv - a$vpr_90_pv
    a$kotiho_pv <- total_homecare_days - a$vpr_pv
    a$tyossa_pv <- total_days_two_year - total_homecare_days
    
    # bruttopalkkatulojen ja etuustulojen laskenta, sekä bruttotulot yhteensä/vuosi/vanhempi a
    a$brutto_salary_per_year <- (a$tyossa_pv * income_a/25)/2
    a$brutto_benefit_per_year <- (a$vpr_90_pv * vpr1_90 + a$vpr_70_pv * vpr1_70 + a$kotiho_pv * kotihoidontuki_e_per_month/25)/2
    a$brutto_income_per_year <- a$brutto_salary_per_year + a$brutto_benefit_per_year
    
    a$netto_income_per_year <- compute_net_income(dat = param_year,
                                                  brutto_salary_per_year = a$brutto_salary_per_year,
                                                  brutto_benefit_per_year = a$brutto_benefit_per_year)
    
    
    # Parent B
    b <- list()
    b$vpr_90_pv <- 0
    b$vpr_70_pv <- 0
    b$kotiho_pv <- 0
    b$tyossa_pv <- total_days_two_year
    
    b$brutto_salary_per_year <- (b$tyossa_pv * income_b/25)/2
    b$brutto_benefit_per_year <- (b$vpr_90_pv * vpr2_90 + b$vpr_70_pv * vpr1_70 + b$kotiho_pv * kotihoidontuki_e_per_month/25)/2
    b$brutto_income_per_year <- b$brutto_salary_per_year + b$brutto_benefit_per_year
    
    b$netto_income_per_year <- compute_net_income(dat = param_year,
                                                  brutto_salary_per_year = b$brutto_salary_per_year,
                                                  brutto_benefit_per_year = b$brutto_benefit_per_year)
    
    # household
    hh <- list()
    hh$brutto_income_per_year <- a$brutto_income_per_year + b$brutto_income_per_year
    hh$netto_income_per_year <- a$netto_income_per_year + b$netto_income_per_year
    
    
    lst <- list(parent_a = a,
                parent_b = b,
                household = hh)
    
  } else if (opt_nr == 1){
    # Vanhempi B käyttää 97 vanhempainrahapäivää ja vanhempi A muut vapaat
    #oisko parempi tsekata, jo ennen tätä kutsua, voiko ko. optiota laskea?
    #if (päivien_määrä<2*max_jakamaton) return (NA)
    ## Parent A
    a <- list()
    a$vpr_pv <- pmin(total_homecare_days-param_year$max_jakamaton,param_year$max_oma) 
    a$vpr_90_pv <- pmin(param_year$korotetut, a$vpr_pv)
    a$vpr_70_pv <- a$vpr_pv-a$vpr_90_pv
    a$kotiho_pv <- total_homecare_days - a$vpr_pv - param_year$max_jakamaton
    a$tyossa_pv <- total_days_two_year - (a$vpr_pv + a$kotiho_pv) #(a$vpr_70_pv + (total_homecare_days - a$vpr_90_pv - a$vpr_70_pv - 97))
    
    # bruttopalkkatulojen ja etuustulojen laskenta
    a$brutto_salary_per_year <- (a$tyossa_pv * income_a/25)/2
    a$brutto_benefit_per_year <- (a$vpr_90_pv * vpr1_90 + a$vpr_70_pv * vpr1_70 + a$kotiho_pv * kotihoidontuki_e_per_month/25)/2
    a$brutto_income_per_year <- a$brutto_salary_per_year + a$brutto_benefit_per_year
    
    a$netto_income_per_year <- compute_net_income(dat = param_year,
                                                  brutto_salary_per_year = a$brutto_salary_per_year,
                                                  brutto_benefit_per_year = a$brutto_benefit_per_year)
    
    # Parent B
    b <- list()
    b$vpr_pv <- max_jakamaton # turvallisempi: b$vpr_pv <- total_homecare_days - a$vpr_pv - a$kotiho_pv
    b$vpr_90_pv <- pmin(param_year$korotetut, b$vpr_pv)
    b$vpr_70_pv <- b$vpr_pv - b$vpr_90_pv
    b$kotiho_pv <- 0
    b$tyossa_pv <- total_days_two_year - b$vpr_pv
    
    b$brutto_salary_per_year <- (b$tyossa_pv * income_b/25)/2
    b$brutto_benefit_per_year <- (b$vpr_90_pv * vpr2_90 + b$vpr_70_pv * vpr2_70 + b$kotiho_pv * kotihoidontuki_e_per_month/25)/2
    b$brutto_income_per_year <- b$brutto_salary_per_year + b$brutto_benefit_per_year
    
    b$netto_income_per_year <- compute_net_income(dat = param_year,
                                                  brutto_salary_per_year = b$brutto_salary_per_year,
                                                  brutto_benefit_per_year = b$brutto_benefit_per_year)
    
    # household
    hh <- list()
    hh$brutto_income_per_year <- a$brutto_income_per_year + b$brutto_income_per_year
    hh$netto_income_per_year <- a$netto_income_per_year + b$netto_income_per_year
    
    lst <- list(parent_a = a,
                parent_b = b,
                household = hh)
    
  } else if (opt_nr == 2){
    # Vanhempainrahapäivät jaetaan tasan
    
    ## Parent A
    a <- list()
    a$vpr_pv <- pmin(ceiling(total_homecare_days/2),param_year$vpr_oma) 
    a$vpr_90_pv <- pmin(param_year$korotetut, a$vpr_pv)
    a$vpr_70_pv <- a$vpr_pv - a$vpr_90_pv
    a$kotiho_pv <- pmax(0,(total_homecare_days - 2*param_year$vpr_oma))
    a$tyossa_pv <- total_days_two_year - a$vpr_pv - a$kotiho_pv
    
    # bruttopalkkatulojen ja etuustulojen laskenta
    a$brutto_salary_per_year <- (a$tyossa_pv * income_a/25)/2
    a$brutto_benefit_per_year <- (a$vpr_90_pv * vpr1_90 + a$vpr_70_pv * vpr1_70 + a$kotiho_pv * kotihoidontuki_e_per_month/25)/2
    a$brutto_income_per_year <- a$brutto_salary_per_year + a$brutto_benefit_per_year
    
    a$netto_income_per_year <- compute_net_income(dat = param_year,
                                                  brutto_salary_per_year = a$brutto_salary_per_year,
                                                  brutto_benefit_per_year = a$brutto_benefit_per_year)
    
    # Parent B
    b <- list()
    b$vpr_pv <- pmin ((total_homecare_days - a$vpr_pv), param_year$vpr_oma)
    b$vpr_90_pv <- pmin(param_year$korotetut, b$vpr_pv)
    b$vpr_70_pv <- b$vpr_pv - b$vpr_90_pv
    b$kotiho_pv <- 0
    b$tyossa_pv <- total_days_two_year - b$vpr_pv - b$kotiho_pv
    
    # bruttopalkkatulojen ja etuustulojen laskenta
    b$brutto_salary_per_year <- (b$tyossa_pv * income_b/25)/2
    b$brutto_benefit_per_year <- (b$vpr_90_pv * vpr2_90 + b$vpr_70_pv * vpr2_70 + b$kotiho_pv * kotihoidontuki_e_per_month/25)/2
    b$brutto_income_per_year <- b$brutto_salary_per_year + b$brutto_benefit_per_year
    
    b$netto_income_per_year <- compute_net_income(dat = param_year,
                                                  brutto_salary_per_year = b$brutto_salary_per_year,
                                                  brutto_benefit_per_year = b$brutto_benefit_per_year)
    
    # household
    hh <- list()
    hh$brutto_income_per_year <- a$brutto_income_per_year + b$brutto_income_per_year
    hh$netto_income_per_year <- a$netto_income_per_year + b$netto_income_per_year
    
    lst <- list(parent_a = a,
                parent_b = b,
                household = hh)
  } else if (opt_nr == 3){
    # Vanhempainrahapäivät ja kotihoidon tuki jaetaan tasan
    kotihoidontuki_per_vanhempi <- (total_homecare_days - 320)/2
    
    ## Parent A
    a <- list()
    a$vpr_pv <- pmin(ceiling(total_homecare_days/2),param_year$vpr_oma) 
    a$vpr_90_pv <- pmin(param_year$korotetut, a$vpr_pv)
    a$vpr_70_pv <- a$vpr_pv - a$vpr_90_pv
    a$kotiho_pv <- pmax (0, ceiling((total_homecare_days - 2*param_year$vpr_oma)/2))
    a$tyossa_pv <- total_days_two_year - a$vpr_pv - a$kotiho_pv
    
    
    # bruttopalkkatulojen ja etuustulojen laskenta
    a$brutto_salary_per_year <- (a$tyossa_pv * income_a/25)/2
    a$brutto_benefit_per_year <- (a$vpr_90_pv * vpr1_90 + a$vpr_70_pv * vpr1_70 + a$kotiho_pv * kotihoidontuki_e_per_month/25)/2
    a$brutto_income_per_year <- a$brutto_salary_per_year + a$brutto_benefit_per_year
    
    a$netto_income_per_year <- compute_net_income(dat = param_year,
                                                  brutto_salary_per_year = a$brutto_salary_per_year,
                                                  brutto_benefit_per_year = a$brutto_benefit_per_year)
    
    # Parent B
    b <- list()
    b$vpr_pv <- pmin(floor(total_homecare_days/2),param_year$vpr_oma) 
    b$vpr_90_pv <- pmin(param_year$korotetut, b$vpr_pv)
    b$vpr_70_pv <- b$vpr_pv - b$vpr_90_pv
    b$kotiho_pv <- pmax (0, floor((total_homecare_days - 2*param_year$vpr_oma)/2))
    # tai turvallisemmin?: b$kotiho_pv <- pmax(0, (total_homecare_days - a$vpr_pv - a$kotiho_pv - b$vpr_pv))
    b$tyossa_pv <- total_days_two_year - b$vpr_pv - b$kotiho_pv
    
    
    # bruttopalkkatulojen ja etuustulojen laskenta
    b$brutto_salary_per_year <- (b$tyossa_pv * income_b/25)/2
    b$brutto_benefit_per_year <- (b$vpr_90_pv * vpr2_90 + b$vpr_70_pv * vpr2_70 + b$kotiho_pv * kotihoidontuki_e_per_month/25)/2
    b$brutto_income_per_year <- b$brutto_salary_per_year + b$brutto_benefit_per_year
    
    b$netto_income_per_year <- compute_net_income(dat = param_year,
                                                  brutto_salary_per_year = b$brutto_salary_per_year,
                                                  brutto_benefit_per_year = b$brutto_benefit_per_year)
    
    
    
    # household
    hh <- list()
    hh$brutto_income_per_year <- a$brutto_income_per_year + b$brutto_income_per_year
    hh$netto_income_per_year <- a$netto_income_per_year + b$netto_income_per_year
    
    lst <- list(parent_a = a,
                parent_b = b,
                household = hh)
    
  }
  
  
  
  return(lst)
}




#' 
#' Create the final list consumed by functions creating the table and plot
#'
#' @param option_to_compute numeric which option to compute
#' @param param_user parameters set by user through user interface
#' @param param_year year parameters from get_parameter_year function
#'
#' @return list 
#' @keywords internal
#' @noRd
compute_options_final <- function(option_to_compute = NULL, 
                                  param_user = parameter_user,
                                  param_year = get_parameter_year(vuosi = 2024)){
  
  lst_grand <- list()
  
  for (i in seq(option_to_compute)){
    lst_grand[[paste0("option",option_to_compute[i])]] <- compute_days(opt_nr = option_to_compute[i], param_user = param_user)
  }
  
  # Compute the change
  lst_grand$option0$household$brutto_income_per_year
  lst_grand$option0$household$netto_income_per_year
  for (ii in seq(lst_grand)){
    lst_grand[[ii]]$household$brutto_change <- lst_grand[[ii]]$household$brutto_income_per_year - lst_grand$option0$household$brutto_income_per_year
    lst_grand[[ii]]$household$netto_change <- lst_grand[[ii]]$household$netto_income_per_year - lst_grand$option0$household$netto_income_per_year
  }
  
  return(lst_grand)
}




