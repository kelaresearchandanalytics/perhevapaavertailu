#' ---
#' title: Perhevapaavertailun toinen tuleminen
#' author: Markus Kainu
#' output: github_document
#' ---
#'
#' Päivitetty: `r Sys.time()`
#'
#' # Aluksi
#' 
#' Kirjoitta vähän intentiosta että mitä tehdään 

devtools::load_all()

# Ladataan 
parameter_user <- data.frame(
  T1 = 2100,
  T2 = 2100,
  kokhoitoaika = 12,
  kotihoidontuki_e_per_kk = 380
)

year_params <- perhevapaavertailu:::get_parameter_year(vuosi = 2024)
list2env(x = year_params, envir = globalenv())

# Lasketaan vpr, kotihoidontuki ja työssäkäyntipävien määrät per aikuinen per kaks vuotta
## Annoin vaan vpr_70_pv arvoja koska en tiedä miten ne menevät.
## 


compute_days <- function(opt_nr = 0,
                         param_user = parameter_user,
                         param_year = perhevapaavertailu:::get_parameter_year(vuosi = 2024)
){
  
  
  total_homecare_months = param_user$kokhoitoaika
  income_a <- param_user$T1
  income_b <- param_user$T2
  kotihoidontuki_e_per_month <- param_user$kotihoidontuki_e_per_kk
  total_homecare_days <- floor(total_homecare_months * 25)
  total_days_two_year <- floor(24 * 25)
  
  
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
    

    # year one, parent a 
    #(calculating year 1 and year 2 separately)
    # when parent a uses his/her homecare days first and parent b uses his/her homecare days after parent a has used his/hers.
    a$vpr_pv_year_1 <- pmin(300,a$vpr_pv)
    a$vpr_90_pv_year_1 <- pmin(a$vpr_90_pv,a$vpr_pv_year_1)
    a$vpr_70_pv_year_1 <- a$vpr_pv_year_1-a$vpr_90_pv_year_1
    a$kotiho_pv_year_1 <- pmin(300 - a$vpr_pv_year_1,a$kotiho_pv)
    a$tyossa_pv_year_1 <- 300-a$vpr_pv_year_1-a$kotiho_pv_year_1
    
    # year two, parent a
    # when parent a uses his/her homecare days first and parent b uses his/her homecare days after parent a has used his/hers.
    a$vpr_pv_year_2 <- (a$vpr_pv - a$vpr_pv_year_1)
    a$vpr_90_pv_year_2 <- (a$vpr_90_pv - a$vpr_90_pv_year_1)
    a$vpr_70_pv_year_2 <- a$vpr_70_pv - a$vpr_70_pv_year_1
    a$kotiho_pv_year_2 <- a$kotiho_pv - a$kotiho_pv_year_1
    a$tyossa_pv_year_2 <- 300 - a$vpr_pv_year_2 - a$kotiho_pv_year_2 
    
    # bruttopalkkatulojen ja etuustulojen laskenta, sekä bruttotulot yhteensä/ 1. vuosi/vanhempi a
    a$brutto_salary_per_year_1 <- (a$tyossa_pv_year_1 * income_a/25)
    a$brutto_benefit_per_year_1 <- (a$vpr_90_pv_year_1 * vpr1_90 + a$vpr_70_pv_year_1 * vpr1_70 + a$kotiho_pv_year_1 * kotihoidontuki_e_per_month/25)
    a$brutto_income_per_year_1 <- a$brutto_salary_per_year_1 + a$brutto_benefit_per_year_1
    
    a$netto_income_per_year_1 <- compute_net_income(dat = param_year,
                                                    brutto_salary_per_year = a$brutto_salary_per_year_1,
                                                    brutto_benefit_per_year = a$brutto_benefit_per_year_1)
    
    a$brutto_salary_per_year_2 <- (a$tyossa_pv_year_2 * income_a/25)
    a$brutto_benefit_per_year_2 <- (a$vpr_90_pv_year_2 * vpr1_90 + a$vpr_70_pv_year_2 * vpr1_70 + a$kotiho_pv_year_2 * kotihoidontuki_e_per_month/25)
    a$brutto_income_per_year_2 <- a$brutto_salary_per_year_2 + a$brutto_benefit_per_year_2
    
    a$netto_income_per_year_2 <- compute_net_income(dat = param_year,
                                                    brutto_salary_per_year = a$brutto_salary_per_year_2,
                                                    brutto_benefit_per_year = a$brutto_benefit_per_year_2)
    
    
    # Parent B
    b <- list()
    b$vpr_pv <- 0
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

    
    # year one, parent b
    #calculating year 1 and year 2 separately assuming:
    # parent b uses his/her homecare days after parent a has used his/her homecare days
    b$vpr_pv_year_1 <- pmin(300-a$vpr_pv_year_1-a$kotiho_pv_year_1,b$vpr_pv)
    b$vpr_90_pv_year_1 <- pmin(b$vpr_pv_year_1,b$vpr_90_pv)
    b$vpr_70_pv_year_1 <- b$vpr_pv_year_1-b$vpr_90_pv_year_1
    b$kotiho_pv_year_1 <- pmin(300-a$vpr_pv_year_1 - a$kotiho_pv_year_1,b$kotiho_pv)
    b$tyossa_pv_year_1 <- 300-b$vpr_pv_year_1-b$kotiho_pv_year_1 
    # year two, parent b
    b$vpr_pv_year_2 <- (b$vpr_pv-b$vpr_pv_year_1)
    b$vpr_90_pv_year_2 <- (b$vpr_90_pv-b$vpr_90_pv_year_1)
    b$vpr_70_pv_year_2 <- b$vpr_70_pv-b$vpr_70_pv_year_1
    b$kotiho_pv_year_2 <- b$kotiho_pv-b$kotiho_pv_year_1
    b$tyossa_pv_year_2 <- 300-b$vpr_pv_year_2-b$kotiho_pv_year_2
    
    # bruttopalkkatulojen ja etuustulojen laskenta, sekä bruttotulot yhteensä/ 1. vuosi/vanhempi a
    b$brutto_salary_per_year_1 <- (b$tyossa_pv_year_1 * income_b/25)
    b$brutto_benefit_per_year_1 <- (b$vpr_90_pv_year_1 * vpr2_90 + b$vpr_70_pv_year_1 * vpr2_70 + b$kotiho_pv_year_1 * kotihoidontuki_e_per_month/25)
    b$brutto_income_per_year_1 <- b$brutto_salary_per_year_1 + b$brutto_benefit_per_year_1
    
    b$netto_income_per_year_1 <- compute_net_income(dat = param_year,
                                                    brutto_salary_per_year = b$brutto_salary_per_year_1,
                                                    brutto_benefit_per_year = b$brutto_benefit_per_year_1)
    
    b$brutto_salary_per_year_2 <- (b$tyossa_pv_year_2 * income_b/25)
    b$brutto_benefit_per_year_2 <- (b$vpr_90_pv_year_2 * vpr2_90 + b$vpr_70_pv_year_2 * vpr2_70 + b$kotiho_pv_year_2 * kotihoidontuki_e_per_month/25)
    b$brutto_income_per_year_2 <- b$brutto_salary_per_year_2 + b$brutto_benefit_per_year_2
    
    b$netto_income_per_year_2 <- compute_net_income(dat = param_year,
                                                    brutto_salary_per_year = b$brutto_salary_per_year_2,
                                                    brutto_benefit_per_year = b$brutto_benefit_per_year_2)
    
    
    
    
    
    # household
    hh <- list()
    hh$brutto_income_per_year <- a$brutto_income_per_year + b$brutto_income_per_year
    hh$netto_income_per_year <- a$netto_income_per_year + b$netto_income_per_year

    hh$brutto_income_per_year_1 <- a$brutto_income_per_year_1 + b$brutto_income_per_year_1
    hh$brutto_income_per_year_2 <- a$brutto_income_per_year_2 + b$brutto_income_per_year_2
    hh$netto_income_per_year_1 <- a$netto_income_per_year_1 + b$netto_income_per_year_1
    hh$netto_income_per_year_2 <- a$netto_income_per_year_2 + b$netto_income_per_year_2
    hh$brutto_income_per_year_1_2 <- hh$brutto_income_per_year_1 + hh$brutto_income_per_year_2
    hh$netto_income_per_year_1_2 <- hh$netto_income_per_year_1 + hh$netto_income_per_year_2
    
    # uudet vuosittaiseen laskentaan perustuvat tulot
    hh$brutto_income_per_year <- hh$brutto_income_per_year_1_2
    hh$netto_income_per_year <- hh$netto_income_per_year_1_2
    
    
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
    # year one, parent a 
    #(calculating year 1 and year 2 separately)
    # when parent a uses his/her homecare days first and parent b uses his/her homecare days after parent a has used his/hers.
    a$vpr_pv_year_1 <- pmin(300,a$vpr_pv)
    a$vpr_90_pv_year_1 <- pmin(a$vpr_90_pv,a$vpr_pv_year_1)
    a$vpr_70_pv_year_1 <- a$vpr_pv_year_1-a$vpr_90_pv_year_1
    a$kotiho_pv_year_1 <- pmin(300 - a$vpr_pv_year_1,a$kotiho_pv)
    a$tyossa_pv_year_1 <- 300-a$vpr_pv_year_1-a$kotiho_pv_year_1
    
    # year two, parent a
    # when parent a uses his/her homecare days first and parent b uses his/her homecare days after parent a has used his/hers.
    a$vpr_pv_year_2 <- (a$vpr_pv - a$vpr_pv_year_1)
    a$vpr_90_pv_year_2 <- (a$vpr_90_pv - a$vpr_90_pv_year_1)
    a$vpr_70_pv_year_2 <- a$vpr_70_pv - a$vpr_70_pv_year_1
    a$kotiho_pv_year_2 <- a$kotiho_pv - a$kotiho_pv_year_1
    a$tyossa_pv_year_2 <- 300 - a$vpr_pv_year_2 - a$kotiho_pv_year_2 
    
    # bruttopalkkatulojen ja etuustulojen laskenta, sekä bruttotulot yhteensä/ 1. vuosi/vanhempi a
    a$brutto_salary_per_year_1 <- (a$tyossa_pv_year_1 * income_a/25)
    a$brutto_benefit_per_year_1 <- (a$vpr_90_pv_year_1 * vpr1_90 + a$vpr_70_pv_year_1 * vpr1_70 + a$kotiho_pv_year_1 * kotihoidontuki_e_per_month/25)
    a$brutto_income_per_year_1 <- a$brutto_salary_per_year_1 + a$brutto_benefit_per_year_1
    
    a$netto_income_per_year_1 <- compute_net_income(dat = param_year,
                                                    brutto_salary_per_year = a$brutto_salary_per_year_1,
                                                    brutto_benefit_per_year = a$brutto_benefit_per_year_1)
    
    a$brutto_salary_per_year_2 <- (a$tyossa_pv_year_2 * income_a/25)
    a$brutto_benefit_per_year_2 <- (a$vpr_90_pv_year_2 * vpr1_90 + a$vpr_70_pv_year_2 * vpr1_70 + a$kotiho_pv_year_2 * kotihoidontuki_e_per_month/25)
    a$brutto_income_per_year_2 <- a$brutto_salary_per_year_2 + a$brutto_benefit_per_year_2
    
    a$netto_income_per_year_2 <- compute_net_income(dat = param_year,
                                                    brutto_salary_per_year = a$brutto_salary_per_year_2,
                                                    brutto_benefit_per_year = a$brutto_benefit_per_year_2)
    
    
    
    
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

    # year one, parent b
    #calculating year 1 and year 2 separately assuming:
    # parent b uses his/her homecare days after parent a has used his/her homecare days
    b$vpr_pv_year_1 <- pmin(300-a$vpr_pv_year_1-a$kotiho_pv_year_1,b$vpr_pv)
    b$vpr_90_pv_year_1 <- pmin(b$vpr_pv_year_1,b$vpr_90_pv)
    b$vpr_70_pv_year_1 <- b$vpr_pv_year_1-b$vpr_90_pv_year_1
    b$kotiho_pv_year_1 <- pmin(300-a$vpr_pv_year_1 - a$kotiho_pv_year_1,b$kotiho_pv)
    b$tyossa_pv_year_1 <- 300-b$vpr_pv_year_1-b$kotiho_pv_year_1 
    # year two, parent b
    b$vpr_pv_year_2 <- (b$vpr_pv-b$vpr_pv_year_1)
    b$vpr_90_pv_year_2 <- (b$vpr_90_pv-b$vpr_90_pv_year_1)
    b$vpr_70_pv_year_2 <- b$vpr_70_pv-b$vpr_70_pv_year_1
    b$kotiho_pv_year_2 <- b$kotiho_pv-b$kotiho_pv_year_1
    b$tyossa_pv_year_2 <- 300-b$vpr_pv_year_2-b$kotiho_pv_year_2
    
    # bruttopalkkatulojen ja etuustulojen laskenta, sekä bruttotulot yhteensä/ 1. vuosi/vanhempi a
    b$brutto_salary_per_year_1 <- (b$tyossa_pv_year_1 * income_b/25)
    b$brutto_benefit_per_year_1 <- (b$vpr_90_pv_year_1 * vpr2_90 + b$vpr_70_pv_year_1 * vpr2_70 + b$kotiho_pv_year_1 * kotihoidontuki_e_per_month/25)
    b$brutto_income_per_year_1 <- b$brutto_salary_per_year_1 + b$brutto_benefit_per_year_1
    
    b$netto_income_per_year_1 <- compute_net_income(dat = param_year,
                                                    brutto_salary_per_year = b$brutto_salary_per_year_1,
                                                    brutto_benefit_per_year = b$brutto_benefit_per_year_1)
    
    b$brutto_salary_per_year_2 <- (b$tyossa_pv_year_2 * income_b/25)
    b$brutto_benefit_per_year_2 <- (b$vpr_90_pv_year_2 * vpr2_90 + b$vpr_70_pv_year_2 * vpr2_70 + b$kotiho_pv_year_2 * kotihoidontuki_e_per_month/25)
    b$brutto_income_per_year_2 <- b$brutto_salary_per_year_2 + b$brutto_benefit_per_year_2
    
    b$netto_income_per_year_2 <- compute_net_income(dat = param_year,
                                                    brutto_salary_per_year = b$brutto_salary_per_year_2,
                                                    brutto_benefit_per_year = b$brutto_benefit_per_year_2)
    
    
    
    
    # household
    hh <- list()
    hh$brutto_income_per_year <- a$brutto_income_per_year + b$brutto_income_per_year
    hh$netto_income_per_year <- a$netto_income_per_year + b$netto_income_per_year
    
    hh$brutto_income_per_year_1 <- a$brutto_income_per_year_1 + b$brutto_income_per_year_1
    hh$brutto_income_per_year_2 <- a$brutto_income_per_year_2 + b$brutto_income_per_year_2
    hh$netto_income_per_year_1 <- a$netto_income_per_year_1 + b$netto_income_per_year_1
    hh$netto_income_per_year_2 <- a$netto_income_per_year_2 + b$netto_income_per_year_2
    hh$brutto_income_per_year_1_2 <- hh$brutto_income_per_year_1 + hh$brutto_income_per_year_2
    hh$netto_income_per_year_1_2 <- hh$netto_income_per_year_1 + hh$netto_income_per_year_2
    
    # uudet vuosittaiseen laskentaan perustuvat tulot
    hh$brutto_income_per_year <- hh$brutto_income_per_year_1_2
    hh$netto_income_per_year <- hh$netto_income_per_year_1_2
    
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
    
    # year one, parent a 
    #(calculating year 1 and year 2 separately)
    # when parent a uses his/her homecare days first and parent b uses his/her homecare days after parent a has used his/hers.
    a$vpr_pv_year_1 <- pmin(300,a$vpr_pv)
    a$vpr_90_pv_year_1 <- pmin(a$vpr_90_pv,a$vpr_pv_year_1)
    a$vpr_70_pv_year_1 <- a$vpr_pv_year_1-a$vpr_90_pv_year_1
    a$kotiho_pv_year_1 <- pmin(300 - a$vpr_pv_year_1,a$kotiho_pv)
    a$tyossa_pv_year_1 <- 300-a$vpr_pv_year_1-a$kotiho_pv_year_1
    
    # year two, parent a
    # when parent a uses his/her homecare days first and parent b uses his/her homecare days after parent a has used his/hers.
    a$vpr_pv_year_2 <- (a$vpr_pv - a$vpr_pv_year_1)
    a$vpr_90_pv_year_2 <- (a$vpr_90_pv - a$vpr_90_pv_year_1)
    a$vpr_70_pv_year_2 <- a$vpr_70_pv - a$vpr_70_pv_year_1
    a$kotiho_pv_year_2 <- a$kotiho_pv - a$kotiho_pv_year_1
    a$tyossa_pv_year_2 <- 300 - a$vpr_pv_year_2 - a$kotiho_pv_year_2 
    
    # bruttopalkkatulojen ja etuustulojen laskenta, sekä bruttotulot yhteensä/ 1. vuosi/vanhempi a
    a$brutto_salary_per_year_1 <- (a$tyossa_pv_year_1 * income_a/25)
    a$brutto_benefit_per_year_1 <- (a$vpr_90_pv_year_1 * vpr1_90 + a$vpr_70_pv_year_1 * vpr1_70 + a$kotiho_pv_year_1 * kotihoidontuki_e_per_month/25)
    a$brutto_income_per_year_1 <- a$brutto_salary_per_year_1 + a$brutto_benefit_per_year_1
    
    a$netto_income_per_year_1 <- compute_net_income(dat = param_year,
                                                    brutto_salary_per_year = a$brutto_salary_per_year_1,
                                                    brutto_benefit_per_year = a$brutto_benefit_per_year_1)
    
    a$brutto_salary_per_year_2 <- (a$tyossa_pv_year_2 * income_a/25)
    a$brutto_benefit_per_year_2 <- (a$vpr_90_pv_year_2 * vpr1_90 + a$vpr_70_pv_year_2 * vpr1_70 + a$kotiho_pv_year_2 * kotihoidontuki_e_per_month/25)
    a$brutto_income_per_year_2 <- a$brutto_salary_per_year_2 + a$brutto_benefit_per_year_2
    
    a$netto_income_per_year_2 <- compute_net_income(dat = param_year,
                                                    brutto_salary_per_year = a$brutto_salary_per_year_2,
                                                    brutto_benefit_per_year = a$brutto_benefit_per_year_2)
    
    
    
    
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

    # year one, parent b
    #calculating year 1 and year 2 separately assuming:
    # parent b uses his/her homecare days after parent a has used his/her homecare days
    b$vpr_pv_year_1 <- pmin(300-a$vpr_pv_year_1-a$kotiho_pv_year_1,b$vpr_pv)
    b$vpr_90_pv_year_1 <- pmin(b$vpr_pv_year_1,b$vpr_90_pv)
    b$vpr_70_pv_year_1 <- b$vpr_pv_year_1-b$vpr_90_pv_year_1
    b$kotiho_pv_year_1 <- pmin(300-a$vpr_pv_year_1 - a$kotiho_pv_year_1,b$kotiho_pv)
    b$tyossa_pv_year_1 <- 300-b$vpr_pv_year_1-b$kotiho_pv_year_1 
    # year two, parent b
    b$vpr_pv_year_2 <- (b$vpr_pv-b$vpr_pv_year_1)
    b$vpr_90_pv_year_2 <- (b$vpr_90_pv-b$vpr_90_pv_year_1)
    b$vpr_70_pv_year_2 <- b$vpr_70_pv-b$vpr_70_pv_year_1
    b$kotiho_pv_year_2 <- b$kotiho_pv-b$kotiho_pv_year_1
    b$tyossa_pv_year_2 <- 300-b$vpr_pv_year_2-b$kotiho_pv_year_2
    
    # bruttopalkkatulojen ja etuustulojen laskenta, sekä bruttotulot yhteensä/ 1. vuosi/vanhempi a
    b$brutto_salary_per_year_1 <- (b$tyossa_pv_year_1 * income_b/25)
    b$brutto_benefit_per_year_1 <- (b$vpr_90_pv_year_1 * vpr2_90 + b$vpr_70_pv_year_1 * vpr2_70 + b$kotiho_pv_year_1 * kotihoidontuki_e_per_month/25)
    b$brutto_income_per_year_1 <- b$brutto_salary_per_year_1 + b$brutto_benefit_per_year_1
    
    b$netto_income_per_year_1 <- compute_net_income(dat = param_year,
                                                    brutto_salary_per_year = b$brutto_salary_per_year_1,
                                                    brutto_benefit_per_year = b$brutto_benefit_per_year_1)
    
    b$brutto_salary_per_year_2 <- (b$tyossa_pv_year_2 * income_b/25)
    b$brutto_benefit_per_year_2 <- (b$vpr_90_pv_year_2 * vpr2_90 + b$vpr_70_pv_year_2 * vpr2_70 + b$kotiho_pv_year_2 * kotihoidontuki_e_per_month/25)
    b$brutto_income_per_year_2 <- b$brutto_salary_per_year_2 + b$brutto_benefit_per_year_2
    
    b$netto_income_per_year_2 <- compute_net_income(dat = param_year,
                                                    brutto_salary_per_year = b$brutto_salary_per_year_2,
                                                    brutto_benefit_per_year = b$brutto_benefit_per_year_2)
    
    
    
    # household
    hh <- list()
    hh$brutto_income_per_year <- a$brutto_income_per_year + b$brutto_income_per_year
    hh$netto_income_per_year <- a$netto_income_per_year + b$netto_income_per_year
    
    hh$brutto_income_per_year_1 <- a$brutto_income_per_year_1 + b$brutto_income_per_year_1
    hh$brutto_income_per_year_2 <- a$brutto_income_per_year_2 + b$brutto_income_per_year_2
    hh$netto_income_per_year_1 <- a$netto_income_per_year_1 + b$netto_income_per_year_1
    hh$netto_income_per_year_2 <- a$netto_income_per_year_2 + b$netto_income_per_year_2
    hh$brutto_income_per_year_1_2 <- hh$brutto_income_per_year_1 + hh$brutto_income_per_year_2
    hh$netto_income_per_year_1_2 <- hh$netto_income_per_year_1 + hh$netto_income_per_year_2
    
    # uudet vuosittaiseen laskentaan perustuvat tulot
    hh$brutto_income_per_year <- hh$brutto_income_per_year_1_2
    hh$netto_income_per_year <- hh$netto_income_per_year_1_2
    
    lst <- list(parent_a = a,
                parent_b = b,
                household = hh)
  } else if (opt_nr == 3){
    # Vanhempainrahapäivät ja kotihoidon tuki jaetaan tasan
    
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

    # year one, parent a 
    #(calculating year 1 and year 2 separately)
    # when parent a uses his/her homecare days first and parent b uses his/her homecare days after parent a has used his/hers.
    a$vpr_pv_year_1 <- pmin(300,a$vpr_pv)
    a$vpr_90_pv_year_1 <- pmin(a$vpr_90_pv,a$vpr_pv_year_1)
    a$vpr_70_pv_year_1 <- a$vpr_pv_year_1-a$vpr_90_pv_year_1
    a$kotiho_pv_year_1 <- pmin(300 - a$vpr_pv_year_1,a$kotiho_pv)
    a$tyossa_pv_year_1 <- 300-a$vpr_pv_year_1-a$kotiho_pv_year_1
    
    # year two, parent a
    # when parent a uses his/her homecare days first and parent b uses his/her homecare days after parent a has used his/hers.
    a$vpr_pv_year_2 <- (a$vpr_pv - a$vpr_pv_year_1)
    a$vpr_90_pv_year_2 <- (a$vpr_90_pv - a$vpr_90_pv_year_1)
    a$vpr_70_pv_year_2 <- a$vpr_70_pv - a$vpr_70_pv_year_1
    a$kotiho_pv_year_2 <- a$kotiho_pv - a$kotiho_pv_year_1
    a$tyossa_pv_year_2 <- 300 - a$vpr_pv_year_2 - a$kotiho_pv_year_2 
    
    # bruttopalkkatulojen ja etuustulojen laskenta, sekä bruttotulot yhteensä/ 1. vuosi/vanhempi a
    a$brutto_salary_per_year_1 <- (a$tyossa_pv_year_1 * income_a/25)
    a$brutto_benefit_per_year_1 <- (a$vpr_90_pv_year_1 * vpr1_90 + a$vpr_70_pv_year_1 * vpr1_70 + a$kotiho_pv_year_1 * kotihoidontuki_e_per_month/25)
    a$brutto_income_per_year_1 <- a$brutto_salary_per_year_1 + a$brutto_benefit_per_year_1
    
    a$netto_income_per_year_1 <- compute_net_income(dat = param_year,
                                                    brutto_salary_per_year = a$brutto_salary_per_year_1,
                                                    brutto_benefit_per_year = a$brutto_benefit_per_year_1)
    
    a$brutto_salary_per_year_2 <- (a$tyossa_pv_year_2 * income_a/25)
    a$brutto_benefit_per_year_2 <- (a$vpr_90_pv_year_2 * vpr1_90 + a$vpr_70_pv_year_2 * vpr1_70 + a$kotiho_pv_year_2 * kotihoidontuki_e_per_month/25)
    a$brutto_income_per_year_2 <- a$brutto_salary_per_year_2 + a$brutto_benefit_per_year_2
    
    a$netto_income_per_year_2 <- compute_net_income(dat = param_year,
                                                    brutto_salary_per_year = a$brutto_salary_per_year_2,
                                                    brutto_benefit_per_year = a$brutto_benefit_per_year_2)
    
    
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
    
    
    # year one, parent b
    #calculating year 1 and year 2 separately assuming:
    # parent b uses his/her homecare days after parent a has used his/her homecare days
    b$vpr_pv_year_1 <- pmin(300-a$vpr_pv_year_1-a$kotiho_pv_year_1,b$vpr_pv)
    b$vpr_90_pv_year_1 <- pmin(b$vpr_pv_year_1,b$vpr_90_pv)
    b$vpr_70_pv_year_1 <- b$vpr_pv_year_1-b$vpr_90_pv_year_1
    b$kotiho_pv_year_1 <- pmin(300-a$vpr_pv_year_1 - a$kotiho_pv_year_1,b$kotiho_pv)
    b$tyossa_pv_year_1 <- 300-b$vpr_pv_year_1-b$kotiho_pv_year_1 
    # year two, parent b
    b$vpr_pv_year_2 <- (b$vpr_pv-b$vpr_pv_year_1)
    b$vpr_90_pv_year_2 <- (b$vpr_90_pv-b$vpr_90_pv_year_1)
    b$vpr_70_pv_year_2 <- b$vpr_70_pv-b$vpr_70_pv_year_1
    b$kotiho_pv_year_2 <- b$kotiho_pv-b$kotiho_pv_year_1
    b$tyossa_pv_year_2 <- 300-b$vpr_pv_year_2-b$kotiho_pv_year_2
    
    # bruttopalkkatulojen ja etuustulojen laskenta, sekä bruttotulot yhteensä/ 1. vuosi/vanhempi a
    b$brutto_salary_per_year_1 <- (b$tyossa_pv_year_1 * income_b/25)
    b$brutto_benefit_per_year_1 <- (b$vpr_90_pv_year_1 * vpr2_90 + b$vpr_70_pv_year_1 * vpr2_70 + b$kotiho_pv_year_1 * kotihoidontuki_e_per_month/25)
    b$brutto_income_per_year_1 <- b$brutto_salary_per_year_1 + b$brutto_benefit_per_year_1
    
    b$netto_income_per_year_1 <- compute_net_income(dat = param_year,
                                                    brutto_salary_per_year = b$brutto_salary_per_year_1,
                                                    brutto_benefit_per_year = b$brutto_benefit_per_year_1)
    
    b$brutto_salary_per_year_2 <- (b$tyossa_pv_year_2 * income_b/25)
    b$brutto_benefit_per_year_2 <- (b$vpr_90_pv_year_2 * vpr2_90 + b$vpr_70_pv_year_2 * vpr2_70 + b$kotiho_pv_year_2 * kotihoidontuki_e_per_month/25)
    b$brutto_income_per_year_2 <- b$brutto_salary_per_year_2 + b$brutto_benefit_per_year_2
    
    b$netto_income_per_year_2 <- compute_net_income(dat = param_year,
                                                    brutto_salary_per_year = b$brutto_salary_per_year_2,
                                                    brutto_benefit_per_year = b$brutto_benefit_per_year_2)
    
    
    
    # household
    hh <- list()
    hh$brutto_income_per_year <- a$brutto_income_per_year + b$brutto_income_per_year
    hh$netto_income_per_year <- a$netto_income_per_year + b$netto_income_per_year
    
    hh$brutto_income_per_year_1 <- a$brutto_income_per_year_1 + b$brutto_income_per_year_1
    hh$brutto_income_per_year_2 <- a$brutto_income_per_year_2 + b$brutto_income_per_year_2
    hh$netto_income_per_year_1 <- a$netto_income_per_year_1 + b$netto_income_per_year_1
    hh$netto_income_per_year_2 <- a$netto_income_per_year_2 + b$netto_income_per_year_2
    hh$brutto_income_per_year_1_2 <- hh$brutto_income_per_year_1 + hh$brutto_income_per_year_2
    hh$netto_income_per_year_1_2 <- hh$netto_income_per_year_1 + hh$netto_income_per_year_2
    
    # uudet vuosittaiseen laskentaan perustuvat tulot
    hh$brutto_income_per_year <- hh$brutto_income_per_year_1_2
    hh$netto_income_per_year <- hh$netto_income_per_year_1_2
    
    lst <- list(parent_a = a,
                parent_b = b,
                household = hh)

  }

  
  
  return(lst)
}

# testaa yo. funktiota.
res <- compute_days(opt_nr = 0)
res # päivinä
lapply(res, function(x) lapply(x, function(x) x/25)) # kuukausina

# yritetään laskea kaikki neljä vaihtoehtoa 

# ne toimenpiteet, jotka on kaikille optioille samat


parameter_user$kokhoitoaika <- 18


# laskettavien optioiden päätely

which_options_to_compute <- function(home_care_months = 18, 
                                     year_params = perhevapaavertailu:::get_parameter_year(vuosi = 2024)){
  
  optios_to_be_computed <- vector()
  optios_to_be_computed <- 0
  if (home_care_months*25 >= 2*year_params$max_jakamaton) optios_to_be_computed <- c(optios_to_be_computed,1)
  optios_to_be_computed <- c(optios_to_be_computed,2)
  if (home_care_months*25 > 2*year_params$vpr_oma) optios_to_be_computed <- c(optios_to_be_computed,3)
  return(optios_to_be_computed)
  
}

compute_options_final <- function(option_to_compute = NULL, 
                                  param_user = parameter_user,
                                  param_year = perhevapaavertailu:::get_parameter_year(vuosi = 2024)){
  
  lst_grand <- list()
  
  for (i in seq(option_to_compute)){
    lst_grand[[paste0("option",option_to_compute[i])]] <- compute_days(opt_nr = option_to_compute[i])
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

# Staattiset tekstit suomeksi, eli jotka pysyvät aina samoin
static_texts <- list(
  option0 = list(title = "Vertailukohta",
                 subtitle = "Vain vanhempi A käyttää vapaita"),
  option1 = list(title = "Vaihtoehto 1",
                 subtitle = "Vanhempi B käyttää 97 vanhempainrahapäivää ja vanhempi A muut vapaat"),
  option2 = list(title = "Vaihtoehto 2",
                 subtitle = "Vanhempainrahapäivät jaetaan tasan"),
  option3 = list(title = "Vaihtoehto 3",
                 subtitle = "Vanhempainrahapäivät ja kotihoidon tuki jaetaan tasan")
)

add_static_texts <- function(option = 0, 
                             texts = static_texts){
  
  txt <- texts[[paste0("option",option)]]
  return(txt)
}
add_static_texts(option = 3)



# *********************************************************************************
# *********************************************************************************

## Raportointi

null_to_zero <- function(x) ifelse(is.null(x), 0, x)

# *********************************************************************************
## taulukko

library(shiny)
luo_kortti <- function(optio = 1, lang = NULL, res_lst){
  dcol <- res_lst[[paste0("option",optio)]]
  tcol <- add_static_texts(option = optio)
  
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
                                       tags$strong("kokonaistulot")
                                     ),
                                     tags$ul(class = "peva-list-item",
                                             tags$li(paste0("brutto ", null_to_zero(dcol$household$brutto_income_per_year))),
                                             tags$li(paste0("netto ", null_to_zero(dcol$household$netto_income_per_year)))
                                     )
                             ),
                             if (optio != 0){
                               tags$ul(class="list-unstyled card-text", style="padding-top: 15px",
                                       tags$li(
                                         tags$strong("muutos")
                                       ),
                                       tags$ul(class = "peva-list-item",
                                               tags$li(paste0("brutto ", null_to_zero(dcol$household$brutto_change))),
                                               tags$li(paste0("netto ", null_to_zero(dcol$household$netto_change)))
                                       )
                               )
                             }
                    ),
                    # footer
                    tags$div(class = "card-footer card-class-footer",
                             tags$ul(class="list-unstyled card-text",
                                     tags$li(
                                       tags$strong("Vanhempi A")
                                     ),
                                     tags$ul(class = "peva-list-item",
                                             # tags$li(paste0("Vanhempainrahaa 160 + ", dcol$parent_a$vpr_pv - 160, " arkipäivää (", round(dcol$parent_a$vpr_pv/25,1), " kuukautta)")),
                                             tags$li(paste0("Vanhempainrahaa ", dcol$parent_a$vpr_pv, " arkipäivää (", round(dcol$parent_a$vpr_pv/25,1), " kuukautta)")),
                                             tags$li(paste0("Kotihoidon tukea ", round(dcol$parent_a$kotiho_pv/25,1)), " kuukautta"),
                                             tags$li(paste0("Palkkatyössä ", round(dcol$parent_a$tyossa_pv/25,1)," kuukautta"))
                                     ),
                                     tags$li(
                                       tags$strong("Vanhempi B")
                                     ),
                                     tags$ul(class = "peva-list-item",
                                             if (optio != 0) tags$li(paste0("Vanhempainrahaa ", dcol$parent_b$vpr_pv, " arkipäivää (", round(dcol$parent_b$vpr_pv/25,1), " kuukautta)")),
                                             if (optio == 0) tags$li("Ei käytä vanhempainrahaa , luovuttaa 63 arkipäivää vanhemmalle A"),
                                             if (optio == 3) tags$li(paste0("Kotihoidon tukea ", round(dcol$parent_b$kotiho_pv/25,1)), " kuukautta"),
                                             if (optio %in% 0:2) tags$li("Ei käytä kotihoidon tukea"),
                                             tags$li(paste0("Palkkatyössä " , round(dcol$parent_b$tyossa_pv/25,1)," kuukautta"))
                                             
                                     ))
                    )
           )) -> res
  return(res)   
}


luo_taulukko <- function(loppulista,optios_to_be_computed){

  tags$body(
    tags$link(href="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css", rel="stylesheet", integrity="sha384-1BmE4kWBq78iYhFldvKuhfTAU6auU8tT94WrHftjDbrCEXSU1oBoqyl2QvZ6jIW3", crossorigin="anonymous"),
    tags$link(href="https://perhevapaavertailu.kela.fi/www/peva.css", rel="stylesheet", crossorigin="anonymous"),
    tags$div(class="row row-cols-1 row-cols-sm-2 row-cols-md-2 row-cols-lg-4",
             tags$div(class = "col",
                      luo_kortti(optio = 0, 
                                 res_lst = loppulista)
             ),
             if (1 %in% optios_to_be_computed){
               tags$div(class = "col",
                        luo_kortti(optio = 1, 
                                   res_lst = loppulista)
               )
             },
             if (2 %in% optios_to_be_computed){
               tags$div(class = "col",
                        luo_kortti(optio = 2, 
                                   res_lst = loppulista)
               )
             },
             if (3 %in% optios_to_be_computed){   
               tags$div(class = "col",
                        luo_kortti(optio = 3, 
                                   res_lst = loppulista)
               )
             }
    )    
  ) -> taulu
  
  tmpfile <- tempfile(fileext = ".html")
  writeLines(as.character(taulu), tmpfile)
  rstudioapi::viewer(tmpfile)
    
}




# *********************************************************************************
## Kuva

piirra_kuva <- function(loppulista){

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
option_nro <- as.integer(sub("option", "", names(loppulista)))
for (i in seq(loppulista)){
  dlist <- list()
  for (p in 1:2){
    ptmp <- loppulista[[i]][[p]]
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
         parent_lab = paste0("Vanhempi ", parent))


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
                                           byrow=TRUE)) +
    labs(title = titles$title,
         subtitle = titles$subtitle) + 
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
                                           byrow=TRUE)) +
    theme(legend.position = "none") +
    theme(plot.margin = unit(c(10, 0, 5, 0), "pt"), 
          plot.title = element_text(size = 16)) +
    theme(legend.position = "none",
          axis.text.x = element_text(),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(hjust = 1, size = 10)) +
    scale_x_continuous(breaks = val[lapply(val, "%%", 4) == 0],
                       labels = val[lapply(val, "%%", 4) == 0]) + 
    labs(x = "kuukautta")
  
  
  
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
outplot <- wrap_plots(imglist, ncol = 1, heights = heights) + plot_annotation(title = "Vapaajaksojen jakautuminen vanhempien kesken eri vaihtoehdoissa")
return(outplot)
}


# testaa
parameter_user <- list(
  T1 = 2100,
  T2 = 2600,
  kokhoitoaika = 18,
  kotihoidontuki_e_per_kk = 380
)
optiot <- which_options_to_compute(home_care_months = parameter_user$kokhoitoaika)
loppulista <- compute_options_final(option_to_compute = optiot,
                                    param_user = parameter_user)

luo_taulukko(loppulista = loppulista,optios_to_be_computed = optiot)
piirra_kuva(loppulista = loppulista)



