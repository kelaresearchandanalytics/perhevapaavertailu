#' Henkilöverotus vuodelle 2024
#' 
#' Henkilön palkka-, etuus- ja eläketulon verotus vuonna 2024
#' @param aineisto data-frame
#' @param ika henkilön ikä vuoden lopussa
#' @param kunnan_veropros kuntaveroprosentti (esim. 0.05)
#' @param kirkko_veropros kirkollisveroprosentti (esim. 0.01)
#' @param elaketulo elaketulo vuoden aikana
#' @param palkkatulo palkkatulo vuoden aikana
#' @param etuustulo etuustulo vuoden aikana
#' @param pyoristys Kuvien piirtämistä varten lukujen pyöristyksen voi ohittaa
#' @return Verotuksen tulos
#' @examples 
#' # 100 tapausta yksi per data.frame-rivi
#' n <- 100
#' 
#' # generoidaan satunnaista testidataa
#' aineisto<-data.frame(
#' ika = round(runif(n,20,65),0),
#' kunnan_veropros = round(runif(n,0.15,0.25),3),
#' kirkko_veropros = round(runif(n,0.00,0.01),3),
#' elaketulo = round(runif(n,0,10000),0),
#' palkkatulo = round(runif(n,0,50000),0),
#' etuustulo = round(runif(n,0,10000),0)
#' )
#' 
#' # aineiston voi valitaa parametrina
#' tmp2 <- henkiloverotus_2024(aineisto,ika,kunnan_veropros,kirkko_veropros,
#'   elaketulo ,palkkatulo,etuustulo)
#' 
#' # tai vaihtoehtoisesti toimii myos pipe-operaattori
#' tmp1 <- aineisto |> henkiloverotus_2024(ika,kunnan_veropros,kirkko_veropros,
#'   elaketulo,palkkatulo,etuustulo)
#' 
#' @encoding UTF-8
#' @keywords internal
#' @noRd
henkiloverotus_2024<-function(aineisto,ika,kunnan_veropros,kirkko_veropros,elaketulo,palkkatulo,etuustulo,pyoristys=T){
  
  p_ika <- dplyr::enquo(ika)
  p_kunnan_veropros <- dplyr::enquo(kunnan_veropros)
  p_kirkko_veropros <- dplyr::enquo(kirkko_veropros)
  p_elaketulo <- dplyr::enquo(elaketulo)
  p_palkkatulo <- dplyr::enquo(palkkatulo)
  p_etuustulo <- dplyr::enquo(etuustulo)
  
  p_pyoristys4 <- 4
  p_pyoristys2 <- 2
  p_pyoristys0 <- 0
  
  if(!pyoristys){
    p_pyoristys4 <- 9
    p_pyoristys2 <- 9
    p_pyoristys0 <- 9
  }
  
  # Maksut
  # ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
  # Työelakemaksut (alle 53 v tai 63v - 67v)
  p_tyel_pros_alle_53 <- 0.0715 # TyELp1
  
  # Työelakemaksut (53 v - 62 v)
  p_tyel_pros_53 <- 0.0865 # TyELp2
  
  # Työttömyysvakuutusmaksu
  p_tyotvak_pros <- 0.0079 # TVp 
  p_tyotvak_alaikaraja <- 18 # TV_alaikär
  p_tyotvak_ylaikaraja <- 64 # TVikär
  
  # Paivarahamaksu
  p_paivaraha_pros_1_raja <- 16499 # PVRr
  p_paivaraha_pros_1 <- 0 # PVRp1
  p_paivaraha_pros_2 <- 0.0101 # PVRp2
  p_paivarahamaksun_ikaraja <- 68 # PVR_ikäraja
  
  # Sairaanhoitomaksun korotus
  p_sairaanhoitomaksu_korotus_ikaraja <- 68 # PVR_ikäraja
  p_sairaanhoitomaksu_korotus <-  0.0148 # SH_korp
  p_sairaanhoitomaksu_korotus_1 <- 0.0097 # SH_lisap
  
  # Sairaanhoitomaksu
  p_sairhoito_pros <- 0.0051 # SH_p
  
  # YLE-vero
  p_yle_ansioraja <- 14000 # YLEr
  p_yle_max <- 163 # YLEmax
  p_yle_pros <- 0.025 # YLEp
  
  # Tulonhankkimisvahennys
  p_tulonhankkimisvahennys <- 750 # THvahmax
  
  # Kunnallisverotus
  # ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨ 
  # Ansiotulovahennys
  p_ans_vah_raja_1 <- 2500 # ATvahr1
  p_ans_vah_raja_2 <- 7230 # ATvahr2
  p_ans_vah_raja_3 <- 14000 # ATvahr3
  p_ans_vah_max <- 3570 # ATvahmax
  p_ans_vah_pros_1 <- 0.51 # ATvahp1
  p_ans_vah_pros_2 <- 0.28 # ATvahp2
  p_ans_vah_pros_3 <- 0.045 # ATvahp3
  
  # Elaketulovahennys
  p_taysi_kansel <-   775.70 # KEY
  p_elak_vah_kerroin <- 1.173 # ETk
  p_elak_vah_vah <- 0 # KETv
  p_elak_vah_pien_pros1 <- 0.51 # ETvahp1
  
  p_elak_vah_raja2 <- 22500 # ETvahraja
  p_elak_vah_pien_pros2 <- 0.15 # ETvahp2
  
  # KETmaxvah lasketaan myöhemmin kaavalla
  
  # Perusvahennys
  p_perus_vah_max <- 3980 # PEvahmax
  p_perus_vah_pien_pros <- 0.18 # PEvahp
  
  # Valtion verotaulukko
  p_valtionvero_kerroin <- 0 # VETk
  # VETmaxvah lasketaan myöhemmin kaavalla
  p_valtionvero_vah_pros <- 0 # VETvahp
  p_elak_lisa_pros <- 0.0585 # VETlisap
  p_elak_lisa_raja <- 47000 # VETlisar
  
  p_valtionvero_vero_1 <- 0 # valtv1
  p_valtionvero_pros_1 <- 0.1264 # valtp1
  p_valtionvero_raja_1 <- 0 # valtr1
  p_valtionvero_pros_2 <- 0.19 # valtp2
  p_valtionvero_raja_2 <- 20500 # valtr2
  p_valtionvero_pros_3 <- 0.3025 # valtp3
  p_valtionvero_raja_3 <- 30500 # valtr3
  p_valtionvero_pros_4 <- 0.34 # valtp4
  p_valtionvero_raja_4 <- 50400 # valtr4
  p_valtionvero_pros_5 <- 0.42 # valtp5
  p_valtionvero_raja_5 <- 88200 # valtr5
  p_valtionvero_pros_6 <- 0.44 # valtp6
  p_valtionvero_raja_6 <- 150000 # valtr6
  
  # Työtulovahennys
  # ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨ 
  p_tyotulo_vah_max_1 <- 2140 # TTvahmax
  
  p_tyotulo_vah_pros_1 <- 0.12 # TTvahp1
  p_tyotulo_vah_pien_2 <- 0.0203 # TTvahp2
  p_tyotulo_vah_pien_3 <- 0.0121 # TTvahp3
  
  p_tyotulo_vah_enimm_ika1 <- 60 # TTvahikar1
  p_tyotulo_vah_enimm_ika2 <- 62 # TTvahikar2
  p_tyotulo_vah_enimm_ika3 <- 65 # TTvahikar3
  
  p_tyotulo_vah_enimm1 <- 2140 # TTvahmax1
  p_tyotulo_vah_enimm2 <- 2140 # TTvahmax2
  p_tyotulo_vah_enimm3 <- 3340 # TTvahmax3
  
  p_tyotulo_tuloraja_1 <- 0 # TTvahr1
  p_tyotulo_tuloraja_2 <- 23420 # TTvahr2
  p_tyotulo_tuloraja_3 <- 71900 # TTvahr3
  
  # LASKENTA ALKAA TÄSTÄ
  
  dplyr::mutate(aineisto,
                
                # ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨ 
                # Maksut 1
                # ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨ 
                # Työelakemaksut (alle 53 v tai 63v - 67v)
                # Työelakemaksut (53 v - 62 v)
                
                tyoelakemaksu = dplyr::case_when(
                  (!!p_ika < 17) ~ 0,
                  (!!p_ika < 53) ~ viitospyoristys(p_tyel_pros_alle_53 * !!p_palkkatulo, p_pyoristys2),
                  (!!p_ika < 63) ~ viitospyoristys(p_tyel_pros_53 * !!p_palkkatulo, p_pyoristys2),
                  (!!p_ika < 68) ~ viitospyoristys(p_tyel_pros_alle_53 * !!p_palkkatulo, p_pyoristys2),
                  (!!p_ika >= 68) ~ 0
                ),
                # Työttömyysvakuutusmaksu
                tyottomyysvakuutusmaksu = dplyr::case_when(
                  !!p_ika < p_tyotvak_alaikaraja ~ 0,
                  !!p_ika <= p_tyotvak_ylaikaraja ~ viitospyoristys(p_tyotvak_pros * !!p_palkkatulo, p_pyoristys2),
                  !!p_ika > p_tyotvak_ylaikaraja  ~ 0
                ),
                # Paivarahamaksu
                paivarahamaksu = viitospyoristys(
                  ifelse(!!p_ika < p_paivarahamaksun_ikaraja, 
                         ifelse(!!p_palkkatulo < p_paivaraha_pros_1_raja,
                                p_paivaraha_pros_1 * !!p_palkkatulo,
                                p_paivaraha_pros_2 * !!p_palkkatulo
                         ),
                         0
                  ),p_pyoristys2
                ),
                # Sairaanhoitomaksu (näma lasketaan myöhemmin)
                sairaanhoitomaksu_korotus = 0,
                sairaanhoitomaksu_ei_vah = 0,
                sairaanhoitomaksu_vah = 0,
                sairaanhoitomaksu = 0,
                # Kirkollisvero (näma lasketaan myöhemmin)
                kirkollisvero_ei_vah = 0,
                kirkollisvero_vah = 0,
                kirkollisvero = 0,
                # YLE-vero (tama lasketaan myöhemmin)
                yle_vero = 0,
                # Tulonhankkimisvahennys
                tulonhankkimisvahennys = ifelse(
                  (!!p_palkkatulo < p_tulonhankkimisvahennys),
                  viitospyoristys(!!p_palkkatulo, p_pyoristys2),
                  p_tulonhankkimisvahennys
                ),
                # Puhdas ansiotulo <- brutto - tulonhankkimiskulut
                puhdasansiotulo = viitospyoristys(!!p_elaketulo + !!p_palkkatulo + !!p_etuustulo - tulonhankkimisvahennys, p_pyoristys2),
                
                # Kunnallis- ja valtionverotuksen vähennykset
                # ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
                #Ansiotulovähennys
                # ansväh1
                ansvah1 = ifelse(
                  (!!p_palkkatulo < p_ans_vah_raja_2),
                  viitospyoristys(pmax(0,p_ans_vah_pros_1 * (!!p_palkkatulo - p_ans_vah_raja_1)), p_pyoristys2),
                  viitospyoristys(pmax(0,p_ans_vah_pros_1 * (p_ans_vah_raja_2 - p_ans_vah_raja_1)), p_pyoristys2)
                ),
                # ansväh2
                ansvah2 = ifelse(
                  !!p_palkkatulo > p_ans_vah_raja_2,
                  viitospyoristys(p_ans_vah_pros_2 * (!!p_palkkatulo - p_ans_vah_raja_2), p_pyoristys2),
                  0
                ),
                # maks väh
                ansmaxvah = ifelse(
                  (ansvah1 + ansvah2) < p_ans_vah_max,
                  ansvah1 + ansvah2,
                  p_ans_vah_max
                ),
                # pienennys
                ans_vah_pros_vah = ifelse ( 
                  puhdasansiotulo > p_ans_vah_raja_3,
                  viitospyoristys(p_ans_vah_pros_3 * (puhdasansiotulo - p_ans_vah_raja_3), p_pyoristys2),
                  0
                ),
                # Ansiotulovähennys
                ansiotulovahennys = pmax( 0, ansmaxvah - ans_vah_pros_vah),
                # Eläketulovähennys
                # ETmaxvah
                elak_vah_max = viitospyoristys(ceiling((12 * p_taysi_kansel * p_elak_vah_kerroin - p_elak_vah_vah) / 10) * 10, -1),
                # pienennys
                elak_vah_pien=ifelse(puhdasansiotulo>p_elak_vah_raja2,(puhdasansiotulo-p_elak_vah_raja2)*p_elak_vah_pien_pros2+(pmin(puhdasansiotulo,p_elak_vah_raja2)-elak_vah_max)*p_elak_vah_pien_pros1,ifelse(puhdasansiotulo>elak_vah_max,(pmin(puhdasansiotulo,p_elak_vah_raja2)-elak_vah_max)*p_elak_vah_pien_pros1,0)),
                # Eläketulovähennys
                elaketulovahennys = ifelse(
                  !!p_elaketulo > 0 & elak_vah_pien < elak_vah_max,
                  pmin( !!p_elaketulo, elak_vah_max - elak_vah_pien),
                  0
                ),
                #Perusvähennys
                # laskennassa kaytetty tulo
                perus_vah_tulo = pmax( 0,
                                       puhdasansiotulo -
                                         tyoelakemaksu -
                                         tyottomyysvakuutusmaksu -
                                         paivarahamaksu -
                                         ansiotulovahennys -
                                         elaketulovahennys
                ),
                # pienennys
                perus_vah_pien = ifelse(
                  perus_vah_tulo > p_perus_vah_max,
                  viitospyoristys( p_perus_vah_pien_pros * (perus_vah_tulo - p_perus_vah_max), p_pyoristys2),
                  0
                ),
                # Perusvahennys
                perusvahennys = viitospyoristys( pmax( 0, p_perus_vah_max - perus_vah_pien), p_pyoristys2),
                #Verotettava tulo valtion- ja kunnallisverotuksessa
                ver_tulo = pmax(0, perus_vah_tulo - perusvahennys),
                # Kunnallisvero
                # kunnallisvero ennen vähennystä
                kunnallisvero_ei_vah = viitospyoristys(ver_tulo * !!p_kunnan_veropros,2),
                # Kunnallisveron vähennys (lasketaan myöhemmin)
                kunnallisvero_vah = 0,
                #kunnallisvero_ei_vah = viitospyoristys( !!p_kunnan_veropros * kun_ver_tulo, p_pyoristys2),
                # Kunnallisvero  (lasketaan myöhemmin)
                kunnallisvero = 0,
                # Valtionverotus
                # Eläketulon lisävero
                valtionvero_elak_lisa = ifelse(
                  !!p_elaketulo - elaketulovahennys > p_elak_lisa_raja,
                  viitospyoristys( p_elak_lisa_pros * (!!p_elaketulo - elaketulovahennys - p_elak_lisa_raja), p_pyoristys2),
                  0
                ),
                # valtion vero (ilman eläketulon lisäveroa) ennen työtulovähennystä
                valtionvero_ei_vah = viitospyoristys(
                  ifelse(
                    ver_tulo >= p_valtionvero_raja_1,
                    p_valtionvero_vero_1 +
                      pmax( ver_tulo-p_valtionvero_raja_1,0) * (p_valtionvero_pros_1) +
                      pmax( ver_tulo-p_valtionvero_raja_2,0) * (p_valtionvero_pros_2 - p_valtionvero_pros_1) +
                      pmax( ver_tulo-p_valtionvero_raja_3,0) * (p_valtionvero_pros_3 - p_valtionvero_pros_2) +
                      pmax( ver_tulo-p_valtionvero_raja_4,0) * (p_valtionvero_pros_4 - p_valtionvero_pros_3) +
                      pmax( ver_tulo-p_valtionvero_raja_5,0) * (p_valtionvero_pros_5 - p_valtionvero_pros_4) + 
                      pmax( ver_tulo-p_valtionvero_raja_6,0) * (p_valtionvero_pros_6 - p_valtionvero_pros_5),
                    0
                  )
                  , p_pyoristys2),
                # Valtion vero (lasketaan myöhemmin)
                valtionvero = 0,
                # Työtulovähennys
                # ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
                tyotulo_max_vah = viitospyoristys( 
                  ifelse(!!p_ika<p_tyotulo_vah_enimm_ika1,
                         pmin(p_tyotulo_vah_max_1,p_tyotulo_vah_pros_1*pmax(0,(!!p_palkkatulo-p_tyotulo_tuloraja_1))),
                         ifelse(!!p_ika<p_tyotulo_vah_enimm_ika2,
                                pmin(p_tyotulo_vah_enimm1,p_tyotulo_vah_pros_1*pmax(0,(!!p_palkkatulo-p_tyotulo_tuloraja_1))),
                                ifelse(!!p_ika<p_tyotulo_vah_enimm_ika3,
                                       pmin(p_tyotulo_vah_enimm2,p_tyotulo_vah_pros_1*pmax(0,(!!p_palkkatulo-p_tyotulo_tuloraja_1))),
                                       pmin(p_tyotulo_vah_enimm3,p_tyotulo_vah_pros_1*pmax(0,(!!p_palkkatulo-p_tyotulo_tuloraja_1)))
                                )
                         )
                  ), p_pyoristys2),
                # vähennyksen pienenemä
                tyotulo_pien = viitospyoristys( 
                  p_tyotulo_vah_pien_2*pmax(0,pmin(puhdasansiotulo,p_tyotulo_tuloraja_3)-p_tyotulo_tuloraja_2)+p_tyotulo_vah_pien_3*pmax(0,puhdasansiotulo-p_tyotulo_tuloraja_3)
                  , p_pyoristys2),
                # Työtulovähennys
                tyotulovahennys = pmax( 0, tyotulo_max_vah - tyotulo_pien),
                # Työtulovähennyksen tarkistus (lasketaan myöhemmin)
                tyotulovah_tark = 0,
                # Vähennys ylittää tuloveron määrän (lasketaan myöhemmin)
                vahennyksen_tuloveroylitys = ifelse(tyotulovahennys-valtionvero_ei_vah>0, tyotulovahennys-valtionvero_ei_vah,0),
                # Maksut 2 (täydennetään maksujen laskentaa)
                # ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
                # Sairaanhoitomaksun korotus ennen vähennystä
                sairaanhoitomaksu_korotus = ifelse(
                  !!p_ika < p_sairaanhoitomaksu_korotus_ikaraja,
                  viitospyoristys( pmax( 0, p_sairaanhoitomaksu_korotus_1 * (ver_tulo - !!p_palkkatulo)), p_pyoristys2),
                  viitospyoristys( p_sairaanhoitomaksu_korotus_1 * ver_tulo, p_pyoristys2)
                ),
                # Sairaanhoitomaksu ennen vähennystä
                sairaanhoitomaksu_ei_vah = viitospyoristys( p_sairhoito_pros * ver_tulo, p_pyoristys2),
                # Kirkollisvero
                kirkollisvero_ei_vah = viitospyoristys( !!p_kirkko_veropros * ver_tulo, p_pyoristys2),
                # YLE-vero
                yle_vero = ifelse(
                  !!p_ika >= 18,
                  viitospyoristys( pmin( p_yle_pros * pmax( 0, puhdasansiotulo - p_yle_ansioraja), p_yle_max), p_pyoristys2),
                  yle_vero
                ),
                # Työtulovähennyksen tarkistus
                tyotulovah_tark = pmin((valtionvero_ei_vah+kunnallisvero_ei_vah+sairaanhoitomaksu_ei_vah+kirkollisvero_ei_vah),tyotulovahennys),
                # Vähennysten huomiointi
                # ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
                # Sairaanhoitomaksun vähennys
                sairaanhoitomaksu_vah = ifelse( 
                  (kirkollisvero_ei_vah + kunnallisvero_ei_vah + sairaanhoitomaksu_ei_vah + sairaanhoitomaksu_korotus) * 
                    vahennyksen_tuloveroylitys > 0,
                  viitospyoristys( 
                    pmin( 
                      (sairaanhoitomaksu_ei_vah + sairaanhoitomaksu_korotus) /
                        (kirkollisvero_ei_vah + kunnallisvero_ei_vah + sairaanhoitomaksu_ei_vah + sairaanhoitomaksu_korotus) * 
                        vahennyksen_tuloveroylitys,
                      (sairaanhoitomaksu_ei_vah + sairaanhoitomaksu_korotus)),
                    p_pyoristys2),
                  0
                ),
                # Kirkollisveron vähennys
                kirkollisvero_vah = ifelse(
                  (kirkollisvero_ei_vah + kunnallisvero_ei_vah + sairaanhoitomaksu_ei_vah + sairaanhoitomaksu_korotus) * 
                    vahennyksen_tuloveroylitys > 0,
                  viitospyoristys(
                    pmin(
                      kirkollisvero_ei_vah / 
                        (kirkollisvero_ei_vah + kunnallisvero_ei_vah + sairaanhoitomaksu_ei_vah + sairaanhoitomaksu_korotus) * 
                        vahennyksen_tuloveroylitys, 
                      kirkollisvero_ei_vah),
                    p_pyoristys2),
                  0
                ),
                # Kunallisveron vähennys
                kunnallisvero_vah = ifelse(
                  (kunnallisvero_ei_vah + kirkollisvero_ei_vah + sairaanhoitomaksu_ei_vah + sairaanhoitomaksu_korotus) * 
                    vahennyksen_tuloveroylitys > 0,
                  viitospyoristys( 
                    pmin(
                      kunnallisvero_ei_vah / 
                        (kunnallisvero_ei_vah + kirkollisvero_ei_vah + sairaanhoitomaksu_ei_vah + sairaanhoitomaksu_korotus) * 
                        vahennyksen_tuloveroylitys, 
                      kunnallisvero_ei_vah),
                    p_pyoristys2),
                  0
                ),
                # Sairaanhoitomaksu
                sairaanhoitomaksu = sairaanhoitomaksu_ei_vah + sairaanhoitomaksu_korotus - sairaanhoitomaksu_vah,
                # Kirkollisvero
                kirkollisvero = kirkollisvero_ei_vah - kirkollisvero_vah,
                # Kunnallisvero
                kunnallisvero = kunnallisvero_ei_vah - kunnallisvero_vah,
                # Valtion vero
                valtionvero = pmax( 0, valtionvero_ei_vah - tyotulovahennys),
                # Tulokset
                # ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
                # Bruttotulot (elake + etuus + palkka)
                bruttotulo = !!p_elaketulo + !!p_palkkatulo + !!p_etuustulo,
                # Verot yhteensa vahennysten jalkeen
                verot_yhteensa = (
                  pmax( 0,
                        (
                          kunnallisvero +
                            kirkollisvero +
                            sairaanhoitomaksu +
                            valtionvero
                        )
                  ) +
                    paivarahamaksu +
                    valtionvero_elak_lisa +
                    yle_vero
                ),
                # Veroprosentti
                veroprosentti = viitospyoristys( ifelse(bruttotulo>0,verot_yhteensa / bruttotulo,0), p_pyoristys4),
                # Verot ja maksut yhteensa (työelake- ja työttömyysvakuutusmaksu ml.)
                verot_ja_maksut_yht = verot_yhteensa + tyoelakemaksu + tyottomyysvakuutusmaksu,
                # nettotulot
                nettotulo = bruttotulo - verot_yhteensa,
                # nettotulot maksujen jalkeen
                netto_maksujen_jalkeen = bruttotulo - verot_ja_maksut_yht
                
  )
  
}








# ************************************************************************************************
# ************************************************************************************************
# ************************************************************************************************


#' Henkilöverotus vuodelle 2022
#' 
#' Henkilön palkka-, etuus- ja eläketulon verotus vuonna 2022
#' @param aineisto data-frame
#' @param ika henkilön ikä vuoden lopussa
#' @param kunnan_veropros kuntaveroprosentti (esim. 0.2)
#' @param kirkko_veropros kirkollisveroprosentti (esim. 0.01)
#' @param elaketulo elaketulo vuoden aikana
#' @param palkkatulo palkkatulo vuoden aikana
#' @param etuustulo etuustulo vuoden aikana
#' @param pyoristys Kuvien piirtämistä varten lukujen pyöristyksen voi ohittaa
#' @return Verotuksen tulos
#' @examples 
#' # 100 tapausta yksi per data.frame-rivi
#' n <- 100
#' 
#' # generoidaan satunnaista testidataa
#' aineisto<-data.frame(
#' ika = round(runif(n,20,65),0),
#' kunnan_veropros = round(runif(n,0.15,0.25),3),
#' kirkko_veropros = round(runif(n,0.00,0.01),3),
#' elaketulo = round(runif(n,0,10000),0),
#' palkkatulo = round(runif(n,0,50000),0),
#' etuustulo = round(runif(n,0,10000),0)
#' )
#' 
#' # aineiston voi valitaa parametrina
#' tmp2 <- henkiloverotus_2022(aineisto,ika,kunnan_veropros,kirkko_veropros,elaketulo ,palkkatulo,etuustulo)
#' 
#' # tai vaihtoehtoisesti toimii myos %>%-operaattori
#' tmp1 <- aineisto %>% henkiloverotus_2022(ika,kunnan_veropros,kirkko_veropros,elaketulo,palkkatulo,etuustulo)
#' 
#' @encoding UTF-8
#' @keywords internal
#' @noRd
henkiloverotus_2022<-function(aineisto,ika,kunnan_veropros,kirkko_veropros,elaketulo,palkkatulo,etuustulo,pyoristys=T){
  
  p_ika <- dplyr::enquo(ika)
  p_kunnan_veropros <- dplyr::enquo(kunnan_veropros)
  p_kirkko_veropros <- dplyr::enquo(kirkko_veropros)
  p_elaketulo <- dplyr::enquo(elaketulo)
  p_palkkatulo <- dplyr::enquo(palkkatulo)
  p_etuustulo <- dplyr::enquo(etuustulo)
  
  p_pyoristys4 <- 4
  p_pyoristys2 <- 2
  p_pyoristys0 <- 0
  
  if(!pyoristys){
    p_pyoristys4 <- 9
    p_pyoristys2 <- 9
    p_pyoristys0 <- 9
  }
  
  # Maksut
  # ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
  # Työelakemaksut (alle 53 v tai 63v - 67v)
  p_tyel_pros_alle_53 <- 0.0715 # TyELp1
  
  # Työelakemaksut (53 v - 62 v)
  p_tyel_pros_53 <- 0.0865 # TyELp2
  
  p_tyotvak_ikaraja <- 64 # TVikär
  
  # Työttömyysvakuutusmaksu
  p_tyotvak_pros <- 0.0150 # TVp 
  
  # Paivarahamaksu
  p_paivarahamaksun_ikaraja <- 68 # PVR_ikäraja
  p_paivaraha_pros_1_raja <- 15128 # PVRr
  p_paivaraha_pros_1 <- 0 # PVRp1
  p_paivaraha_pros_2 <- 0.0118 # PVRp2
  
  # Sairaanhoitomaksun korotus
  p_sairaanhoitomaksu_korotus_ikaraja <- 68 # PVR_ikäraja
  
  # 0.015 # SH_korp - ei käytetä missään kaavassa
  
  p_sairaanhoitomaksu_korotus_1 <- 0.0097 # SH_lisap
  
  # Sairaanhoitomaksu
  p_sairhoito_pros <- 0.0053 # SH_p
  
  # YLE-vero
  p_yle_ansioraja <- 14000 # YLEr
  p_yle_max <- 163 # YLEmax
  p_yle_pros <- 0.025 # YLEp
  
  # Tulonhankkimisvahennys
  p_tulonhankkimisvahennys <- 750 # THvahmax
  
  # Kunnallisverotus
  # ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨ 
  # Ansiotulovahennys
  p_ans_vah_raja_1 <- 2500 # ATvahr1
  p_ans_vah_raja_2 <- 7230 # ATvahr2
  p_ans_vah_raja_3 <- 14000 # ATvahr3
  p_ans_vah_max <- 3570 # ATvahmax
  p_ans_vah_pros_1 <- 0.51 # ATvahp1
  p_ans_vah_pros_2 <- 0.28 # ATvahp2
  p_ans_vah_pros_3 <- 0.045 # ATvahp3
  
  # Elaketulovahennys
  p_taysi_kansel <-  679.50 # KEY
  p_elak_vah_kerroin <- 1.346 # KETk
  p_elak_vah_vah <- 1480 # KETv
  p_elak_vah_pien_pros <- 0.51 # KETvahp
  # KETmaxvah lasketaan myöhemmin kaavalla
  
  # Perusvahennys
  p_perus_vah_max <- 3740 # PEvahmax
  p_perus_vah_pien_pros <- 0.18 # PEvahp
  
  # Valtion verotaulukko
  p_valtionvero_kerroin <- 3.726 # VETk
  # VETmaxvah lasketaan myöhemmin kaavalla
  p_valtionvero_vah_pros <- 0.38 # VETvahp
  p_elak_lisa_pros <- 0.0585 # VETlisap
  p_elak_lisa_raja <- 47000 # VETlisar
  
  p_valtionvero_vero_1 <- 8.00 # valtv1
  p_valtionvero_pros_1 <- 0.06 # valtp1
  p_valtionvero_raja_1 <- 19200 # valtr1
  p_valtionvero_pros_2 <- 0.1725 # valtp2
  p_valtionvero_raja_2 <- 28700 # valtr2
  p_valtionvero_pros_3 <- 0.2125 # valtp3
  p_valtionvero_raja_3 <- 47300 # valtr3
  p_valtionvero_pros_4 <- 0.3125 # valtp4
  p_valtionvero_raja_4 <- 82900 # valtr4
  
  # Työtulovahennys
  # ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨ 
  p_tyotulo_vah_max_1 <- 1930 # TTvahmax
  p_tyotulo_vah_pros_1 <- 0.130 # TTvahp1
  p_tyotulo_vah_pien_2 <- 0.0196 # TTvahp2
  p_tyotulo_tuloraja_1 <- 2500 # TTvahr1
  p_tyotulo_tuloraja_2 <- 33000 # TTvahr2
  
  dplyr::mutate(aineisto,
                
                # ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨ 
                # Maksut 1
                # ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨ 
                # Työelakemaksut (alle 53 v tai 63v - 67v)
                # Työelakemaksut (53 v - 62 v)
                
                tyoelakemaksu = dplyr::case_when(
                  (ika < 53) ~ viitospyoristys(p_tyel_pros_alle_53 * !!p_palkkatulo, p_pyoristys2),
                  (ika < 63) ~ viitospyoristys(p_tyel_pros_53 * !!p_palkkatulo, p_pyoristys2),
                  (ika < 68) ~ viitospyoristys(p_tyel_pros_alle_53 * !!p_palkkatulo, p_pyoristys2),
                  (ika >= 68) ~ 0
                ),
                
                # Työttömyysvakuutusmaksu
                
                tyottomyysvakuutusmaksu = ifelse(
                  (ika <= p_tyotvak_ikaraja),
                  viitospyoristys(p_tyotvak_pros * !!p_palkkatulo, p_pyoristys2),
                  0
                ),
                
                # Paivarahamaksu
                
                # paivarahamaksu = ifelse(
                #   (!!p_ika < p_paivarahamaksun_ikaraja & !!p_palkkatulo >= p_paivaraha_pros_1_raja),
                #   viitospyoristys(p_paivaraha_pros_2 * !!p_palkkatulo, p_pyoristys2),
                #   0
                # ),
                
                paivarahamaksu = viitospyoristys(
                  ifelse(!!p_ika < p_paivarahamaksun_ikaraja, 
                         ifelse(!!p_palkkatulo < p_paivaraha_pros_1_raja,
                                p_paivaraha_pros_1 * !!p_palkkatulo,
                                p_paivaraha_pros_2 * !!p_palkkatulo
                         ),
                         0
                  ),p_pyoristys2
                ),
                
                # Sairaanhoitomaksu (näma lasketaan myöhemmin)
                
                sairaanhoitomaksu_korotus = 0,
                sairaanhoitomaksu_ei_vah = 0,
                sairaanhoitomaksu_vah = 0,
                sairaanhoitomaksu = 0,
                
                # Kirkollisvero (näma lasketaan myöhemmin)
                
                kirkollisvero_ei_vah = 0,
                kirkollisvero_vah = 0,
                kirkollisvero = 0,
                
                # YLE-vero (tama lasketaan myöhemmin)
                
                yle_vero = 0,
                
                # Tulonhankkimisvahennys
                
                tulonhankkimisvahennys = ifelse(
                  (!!p_palkkatulo < p_tulonhankkimisvahennys),
                  viitospyoristys(!!p_palkkatulo, p_pyoristys2),
                  p_tulonhankkimisvahennys
                ),
                
                # Puhdas ansiotulo <- brutto - tulonhankkimiskulut
                
                puhdasansiotulo = viitospyoristys(!!p_elaketulo + !!p_palkkatulo + !!p_etuustulo - tulonhankkimisvahennys, p_pyoristys2),
                
                # Kunnalisverotus: Ansiotulovahennys
                # ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
                # ansvah1
                
                ansvah1 = ifelse(
                  (!!p_palkkatulo < p_ans_vah_raja_2),
                  viitospyoristys(pmax(0,p_ans_vah_pros_1 * (!!p_palkkatulo - p_ans_vah_raja_1)), p_pyoristys2),
                  viitospyoristys(pmax(0,p_ans_vah_pros_1 * (p_ans_vah_raja_2 - p_ans_vah_raja_1)), p_pyoristys2)
                ),
                
                
                # ansvah2
                ansvah2 = ifelse(
                  !!p_palkkatulo > p_ans_vah_raja_2,
                  viitospyoristys(p_ans_vah_pros_2 * (!!p_palkkatulo - p_ans_vah_raja_2), p_pyoristys2),
                  0
                ),
                
                # maksvah
                
                ansmaxvah = ifelse(
                  (ansvah1 + ansvah2) < p_ans_vah_max,
                  ansvah1 + ansvah2,
                  p_ans_vah_max
                ),
                
                # pienenemis %
                
                ans_vah_pros_vah = ifelse ( 
                  puhdasansiotulo > p_ans_vah_raja_3,
                  viitospyoristys(p_ans_vah_pros_3 * (puhdasansiotulo - p_ans_vah_raja_3), p_pyoristys2),
                  0
                ),
                
                
                # Ansiotulovahennys
                
                ansiotulovahennys = pmax( 0, ansmaxvah - ans_vah_pros_vah),
                
                # Kunnalisverotus: Elaketulovahennys
                # ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
                
                elak_vah_max = viitospyoristys(ceiling((12 * p_taysi_kansel * p_elak_vah_kerroin - p_elak_vah_vah) / 10) * 10, -1),
                
                # pienennys
                
                elak_vah_pien = ifelse(
                  puhdasansiotulo > elak_vah_max,
                  p_elak_vah_pien_pros * (puhdasansiotulo - elak_vah_max),
                  0
                ),
                
                # Elaketulovahennys
                
                elaketulovahennys = ifelse(
                  elaketulo > 0 & elak_vah_pien < elak_vah_max,
                  pmin( !!p_elaketulo, elak_vah_max - elak_vah_pien),
                  0
                ),
                
                # Kunnalisverotus: Perusvahennys
                #¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
                # laskennassa kaytetty tulo
                perus_vah_tulo = pmax( 0,
                                       puhdasansiotulo -
                                         tyoelakemaksu -
                                         tyottomyysvakuutusmaksu -
                                         paivarahamaksu -
                                         ansiotulovahennys -
                                         elaketulovahennys
                ),
                
                # pienennys
                
                perus_vah_pien = ifelse(
                  perus_vah_tulo > p_perus_vah_max,
                  viitospyoristys( p_perus_vah_pien_pros * (perus_vah_tulo - p_perus_vah_max), p_pyoristys2),
                  0
                ),
                
                # Perusvahennys
                
                perusvahennys = viitospyoristys( pmax( 0, p_perus_vah_max - perus_vah_pien), p_pyoristys2),
                
                # Kunnalisverotus
                # ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
                # Verotettava tulo kunnallisverotuksessa
                kun_ver_tulo = pmax(0, perus_vah_tulo - perusvahennys),
                
                # kunnallisvero, vahennys lasketaan myöhemmin
                kunnallisvero_ei_vah = viitospyoristys( !!p_kunnan_veropros * kun_ver_tulo, p_pyoristys2),
                
                # Maksut 2
                # ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
                # Sairaanhoitomaksun korotus
                sairaanhoitomaksu_korotus = ifelse(
                  !!p_ika < p_sairaanhoitomaksu_korotus_ikaraja,
                  viitospyoristys( pmax( 0, p_sairaanhoitomaksu_korotus_1 * (kun_ver_tulo - !!p_palkkatulo)), p_pyoristys2),
                  viitospyoristys( p_sairaanhoitomaksu_korotus_1 * kun_ver_tulo, p_pyoristys2)
                ),
                
                # Sairaanhoitomaksu
                sairaanhoitomaksu_ei_vah = viitospyoristys( p_sairhoito_pros * kun_ver_tulo, p_pyoristys2),
                
                # Kirkollisvero
                kirkollisvero_ei_vah = viitospyoristys( !!p_kirkko_veropros * kun_ver_tulo, p_pyoristys2),
                
                # YLE-vero
                yle_vero = ifelse(
                  !!p_ika >= 18,
                  viitospyoristys( pmin( p_yle_pros * pmax( 0, puhdasansiotulo - p_yle_ansioraja), p_yle_max), p_pyoristys2),
                  yle_vero
                ),
                
                # Valtion verotus
                # ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
                # maks. vah
                valt_max_vah = ceiling( (12 * p_taysi_kansel * p_valtionvero_kerroin - p_valtionvero_raja_1) / 10) * 10,
                
                # pienennys
                valtionvero_pien = ifelse(
                  puhdasansiotulo > valt_max_vah,
                  valtionvero_pien <- viitospyoristys(p_valtionvero_vah_pros * (puhdasansiotulo - valt_max_vah), p_pyoristys2),
                  0
                ),
                
                # Elaketulovahennys valtionverotuksessa
                valtionvero_elak_vah = ifelse(
                  !!p_elaketulo > 0 & valtionvero_pien < valt_max_vah,
                  pmin( !!p_elaketulo, valt_max_vah - valtionvero_pien),
                  0
                ),
                
                # Verotettava tulo valtionverotuksessa
                valtionvero_tulo = pmax( 0,
                                         puhdasansiotulo -
                                           tyoelakemaksu -
                                           tyottomyysvakuutusmaksu -
                                           paivarahamaksu -
                                           valtionvero_elak_vah
                ),
                
                # Elaketulon lisavero
                valtionvero_elak_lisa = ifelse(
                  !!p_elaketulo - valtionvero_elak_vah > p_elak_lisa_raja,
                  viitospyoristys( p_elak_lisa_pros * (!!p_elaketulo - valtionvero_elak_vah - p_elak_lisa_raja), p_pyoristys2),
                  0
                ),
                
                valtionvero_ei_vah = viitospyoristys(
                  ifelse(
                    valtionvero_tulo >= p_valtionvero_raja_1,
                    p_valtionvero_vero_1 +
                      pmax( valtionvero_tulo-p_valtionvero_raja_1,0) * p_valtionvero_pros_1 +
                      pmax( valtionvero_tulo-p_valtionvero_raja_2,0) * (p_valtionvero_pros_2 - p_valtionvero_pros_1) +
                      pmax( valtionvero_tulo-p_valtionvero_raja_3,0) * (p_valtionvero_pros_3 - p_valtionvero_pros_2) +
                      pmax( valtionvero_tulo-p_valtionvero_raja_4,0) * (p_valtionvero_pros_4 - p_valtionvero_pros_3),
                    0
                  )
                  , p_pyoristys2),
                
                
                # Työtulovahnennys
                # ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
                # maks vah
                tyotulo_max_vah = viitospyoristys( 
                  pmin( p_tyotulo_vah_max_1, 
                        p_tyotulo_vah_pros_1 * pmax(0, !!p_palkkatulo - p_tyotulo_tuloraja_1)
                  ), p_pyoristys2),
                
                # vahennyksen pienenema
                tyotulo_pien = viitospyoristys( p_tyotulo_vah_pien_2 * pmax( 0, puhdasansiotulo - p_tyotulo_tuloraja_2), p_pyoristys2),
                
                # Työtulovahennys
                tyotulovahennys = pmax( 0, tyotulo_max_vah - tyotulo_pien),
                
                # Työtulovahennys tark
                tyotulovahennys_tark = pmin( 
                  tyotulovahennys, 
                  valtionvero_ei_vah + kunnallisvero_ei_vah + kirkollisvero_ei_vah + sairaanhoitomaksu_ei_vah
                ),
                
                # Osa, jolla työtuloVähennys ylittää tuloveron
                vahennyksen_tuloveroylitys = pmax( 0, tyotulovahennys - valtionvero_ei_vah),
                
                # Vähennysten huomiointi
                # ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
                
                # Sairaanhoitomaksun, kirkollisveron ja kunallisveron vähennykset
                sairaanhoitomaksu_vah = ifelse( 
                  (kirkollisvero_ei_vah + kunnallisvero_ei_vah + sairaanhoitomaksu_ei_vah + sairaanhoitomaksu_korotus) * 
                    vahennyksen_tuloveroylitys > 0,
                  viitospyoristys( 
                    pmin( 
                      (sairaanhoitomaksu_ei_vah + sairaanhoitomaksu_korotus) /
                        (kirkollisvero_ei_vah + kunnallisvero_ei_vah + sairaanhoitomaksu_ei_vah + sairaanhoitomaksu_korotus) * 
                        vahennyksen_tuloveroylitys,
                      (sairaanhoitomaksu_ei_vah + sairaanhoitomaksu_korotus)),
                    p_pyoristys2),
                  0
                ),
                
                kirkollisvero_vah = ifelse(
                  (kirkollisvero_ei_vah + kunnallisvero_ei_vah + sairaanhoitomaksu_ei_vah + sairaanhoitomaksu_korotus) * 
                    vahennyksen_tuloveroylitys > 0,
                  viitospyoristys(
                    pmin(
                      kirkollisvero_ei_vah / 
                        (kirkollisvero_ei_vah + kunnallisvero_ei_vah + sairaanhoitomaksu_ei_vah + sairaanhoitomaksu_korotus) * 
                        vahennyksen_tuloveroylitys, 
                      kirkollisvero_ei_vah),
                    p_pyoristys2),
                  0
                ),
                
                kunnallisvero_vah = ifelse(
                  (kunnallisvero_ei_vah + kirkollisvero_ei_vah + sairaanhoitomaksu_ei_vah + sairaanhoitomaksu_korotus) * 
                    vahennyksen_tuloveroylitys > 0,
                  viitospyoristys( 
                    pmin(
                      kunnallisvero_ei_vah / 
                        (kunnallisvero_ei_vah + kirkollisvero_ei_vah + sairaanhoitomaksu_ei_vah + sairaanhoitomaksu_korotus) * 
                        vahennyksen_tuloveroylitys, 
                      kunnallisvero_ei_vah),
                    p_pyoristys2),
                  0
                ),
                
                # Sairaanhoitomaksu
                sairaanhoitomaksu = sairaanhoitomaksu_ei_vah + sairaanhoitomaksu_korotus - sairaanhoitomaksu_vah,
                
                # Kirkollisvero
                kirkollisvero = kirkollisvero_ei_vah - kirkollisvero_vah,
                
                # Kunnallisvero
                kunnallisvero = kunnallisvero_ei_vah - kunnallisvero_vah,
                
                # Valtion vero
                valtionvero = pmax( 0, valtionvero_ei_vah - tyotulovahennys),
                
                # Tulokset
                # ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
                # Bruttotulot (elake + etuus + palkka)
                bruttotulo = !!p_elaketulo + !!p_palkkatulo + !!p_etuustulo,
                
                # Verot yhteensa vahennysten jalkeen
                verot_yhteensa = (
                  pmax( 0,
                        (
                          kunnallisvero +
                            kirkollisvero +
                            sairaanhoitomaksu +
                            valtionvero
                        )
                  ) +
                    paivarahamaksu +
                    valtionvero_elak_lisa +
                    yle_vero
                ),
                
                # Veroprosentti
                veroprosentti = viitospyoristys( verot_yhteensa / bruttotulo, p_pyoristys4),
                
                # Verot ja maksut yhteensa (työelake- ja työttömyysvakuutusmaksu ml.)
                verot_ja_maksut_yht = verot_yhteensa + tyoelakemaksu + tyottomyysvakuutusmaksu,
                
                # nettotulot
                nettotulo = bruttotulo - verot_yhteensa,
                
                # nettotulot maksujen jalkeen
                netto_maksujen_jalkeen = bruttotulo - verot_ja_maksut_yht
  )
  
}


# ************************************************************************************************
# ************************************************************************************************
# ************************************************************************************************

#' Henkilöverotus vuodelle 2021
#' 
#' Henkilön palkka-, etuus- ja eläketulon verotus vuonna 2021
#' @param aineisto data-frame
#' @param ika henkilön ikä vuoden lopussa
#' @param kunnan_veropros kuntaveroprosentti (esim. 0.2)
#' @param kirkko_veropros kirkollisveroprosentti (esim. 0.01)
#' @param elaketulo elaketulo vuoden aikana
#' @param palkkatulo palkkatulo vuoden aikana
#' @param etuustulo etuustulo vuoden aikana
#' @param pyoristys Kuvien piirtämistä varten lukujen pyöristyksen voi ohittaa
#' @return Verotuksen tulos
#' @examples 
#' # 100 tapausta yksi per data.frame-rivi
#' n <- 100
#' 
#' # generoidaan satunnaista testidataa
#' aineisto<-data.frame(
#' ika = round(runif(n,20,65),0),
#' kunnan_veropros = round(runif(n,0.15,0.25),3),
#' kirkko_veropros = round(runif(n,0.00,0.01),3),
#' elaketulo = round(runif(n,0,10000),0),
#' palkkatulo = round(runif(n,0,50000),0),
#' etuustulo = round(runif(n,0,10000),0)
#' )
#' 
#' # aineiston voi valitaa parametrina
#' tmp2 <- henkiloverotus_2021(aineisto,ika,kunnan_veropros,kirkko_veropros,elaketulo ,palkkatulo,etuustulo)
#' 
#' # tai vaihtoehtoisesti toimii myos %>%-operaattori
#' tmp1 <- aineisto %>% henkiloverotus_2021(ika,kunnan_veropros,kirkko_veropros,elaketulo,palkkatulo,etuustulo)
#' @encoding UTF-8
#' @keywords internal
#' @noRd
henkiloverotus_2021<-function(aineisto,ika,kunnan_veropros,kirkko_veropros,elaketulo,palkkatulo,etuustulo,pyoristys=T){
  
  p_ika <- dplyr::enquo(ika)
  p_kunnan_veropros <- dplyr::enquo(kunnan_veropros)
  p_kirkko_veropros <- dplyr::enquo(kirkko_veropros)
  p_elaketulo <- dplyr::enquo(elaketulo)
  p_palkkatulo <- dplyr::enquo(palkkatulo)
  p_etuustulo <- dplyr::enquo(etuustulo)
  
  p_pyoristys4 <- 4
  p_pyoristys2 <- 2
  p_pyoristys0 <- 0
  
  if(!pyoristys){
    p_pyoristys4 <- 9
    p_pyoristys2 <- 9
    p_pyoristys0 <- 9
  }
  
  # Maksut
  # ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
  # Työelakemaksut (alle 53 v tai 63v - 67v)
  p_tyel_pros_alle_53 <- 0.0715 # TyELp1
  
  # Työelakemaksut (53 v - 62 v)
  p_tyel_pros_53 <- 0.0865 # TyELp2
  
  p_tyotvak_ikaraja <- 64 # TVikär
  
  # Työttömyysvakuutusmaksu
  p_tyotvak_pros <- 0.0140 # TVp 
  
  # Paivarahamaksu
  p_paivarahamaksun_ikaraja <- 68 # PVR_ikäraja
  p_paivaraha_pros_1_raja <- 14766 # PVRr
  p_paivaraha_pros_1 <- 0 # PVRp1
  p_paivaraha_pros_2 <- 0.0136 # PVRp2
  
  # Sairaanhoitomaksun korotus
  p_sairaanhoitomaksu_korotus_ikaraja <- 68 # PVR_ikäraja
  
  p_sairaanhoitomaksu_korotus_1 <- 0.0097 # SH_lisap
  
  # Sairaanhoitomaksu
  p_sairhoito_pros <- 0.0068 # SH_p
  
  # YLE-vero
  p_yle_ansioraja <- 14000 # YLEr
  p_yle_max <- 163 # YLEmax
  p_yle_pros <- 0.025 # YLEp
  
  # Tulonhankkimisvahennys
  p_tulonhankkimisvahennys <- 750 # THvahmax
  
  # Kunnallisverotus
  # ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨ 
  # Ansiotulovahennys
  p_ans_vah_raja_1 <- 2500 # ATvahr1
  p_ans_vah_raja_2 <- 7230 # ATvahr2
  p_ans_vah_raja_3 <- 14000 # ATvahr3
  p_ans_vah_max <- 3570 # ATvahmax
  p_ans_vah_pros_1 <- 0.51 # ATvahp1
  p_ans_vah_pros_2 <- 0.28 # ATvahp2
  p_ans_vah_pros_3 <- 0.045 # ATvahp3
  
  # Elaketulovahennys
  p_taysi_kansel <-  665.29 # KEY
  p_elak_vah_kerroin <- 1.346 # KETk
  p_elak_vah_vah <- 1480 # KETv
  p_elak_vah_pien_pros <- 0.51 # KETvahp
  
  # Perusvahennys
  p_perus_vah_max <- 3630 # PEvahmax
  p_perus_vah_pien_pros <- 0.18 # PEvahp
  
  # Valtion verotaulukko
  # ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
  p_valtionvero_kerroin <- 3.726 # VETk
  p_valtionvero_vah_pros <- 0.38 # VETvahp
  p_elak_lisa_pros <- 0.0585 # VETlisap
  p_elak_lisa_raja <- 47000 # VETlisar
  
  p_valtionvero_vero_1 <- 8.00 # valtv1
  p_valtionvero_pros_1 <- 0.06 # valtp1
  p_valtionvero_raja_1 <- 18600 # valtr1
  p_valtionvero_pros_2 <- 0.1725 # valtp2
  p_valtionvero_raja_2 <- 27900 # valtr2
  p_valtionvero_pros_3 <- 0.2125 # valtp3
  p_valtionvero_raja_3 <- 45900 # valtr3
  p_valtionvero_pros_4 <- 0.3125 # valtp4
  p_valtionvero_raja_4 <- 80500 # valtr4
  
  # Työtulovahennys
  # ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨ 
  p_tyotulo_vah_max_1 <- 1840 # TTvahmax
  p_tyotulo_vah_pros_1 <- 0.127 # TTvahp1
  p_tyotulo_vah_pien_2 <- 0.0189 # TTvahp2
  p_tyotulo_tuloraja_1 <- 2500 # TTvahr1
  p_tyotulo_tuloraja_2 <- 33000 # TTvahr2
  
  dplyr::mutate(aineisto,
                
                # ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨ 
                # Maksut 1
                # ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨ 
                # Työelakemaksut (alle 53 v tai 63v - 67v)
                # Työelakemaksut (53 v - 62 v)
                
                tyoelakemaksu = dplyr::case_when(
                  (ika < 53) ~ viitospyoristys(p_tyel_pros_alle_53 * !!p_palkkatulo, p_pyoristys2),
                  (ika < 63) ~ viitospyoristys(p_tyel_pros_53 * !!p_palkkatulo, p_pyoristys2),
                  (ika < 68) ~ viitospyoristys(p_tyel_pros_alle_53 * !!p_palkkatulo, p_pyoristys2),
                  (ika >= 68) ~ 0
                ),
                
                # Työttömyysvakuutusmaksu
                
                tyottomyysvakuutusmaksu = ifelse(
                  (ika <= p_tyotvak_ikaraja),
                  viitospyoristys(p_tyotvak_pros * !!p_palkkatulo, p_pyoristys2),
                  0
                ),
                
                # Paivarahamaksu
                
                paivarahamaksu = ifelse(
                  (!!p_ika < p_paivarahamaksun_ikaraja & !!p_palkkatulo >= p_paivaraha_pros_1_raja),
                  viitospyoristys(p_paivaraha_pros_2 * !!p_palkkatulo, p_pyoristys2),
                  0
                ),
                
                # Sairaanhoitomaksu (näma lasketaan myöhemmin)
                
                sairaanhoitomaksu_korotus = 0,
                sairaanhoitomaksu_ei_vah = 0,
                sairaanhoitomaksu_vah = 0,
                sairaanhoitomaksu = 0,
                
                # Kirkollisvero (näma lasketaan myöhemmin)
                
                kirkollisvero_ei_vah = 0,
                kirkollisvero_vah = 0,
                kirkollisvero = 0,
                
                # YLE-vero (tama lasketaan myöhemmin)
                
                yle_vero = 0,
                
                # Tulonhankkimisvahennys
                
                tulonhankkimisvahennys = ifelse(
                  (!!p_palkkatulo < p_tulonhankkimisvahennys),
                  viitospyoristys(!!p_palkkatulo, p_pyoristys2),
                  p_tulonhankkimisvahennys
                ),
                
                # Puhdas ansiotulo <- brutto - tulonhankkimiskulut
                
                puhdasansiotulo = viitospyoristys(!!p_elaketulo + !!p_palkkatulo + !!p_etuustulo - tulonhankkimisvahennys, p_pyoristys2),
                
                # Kunnalisverotus: Ansiotulovahennys
                # ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
                # ansvah1
                
                ansvah1 = ifelse(
                  (!!p_palkkatulo < p_ans_vah_raja_2),
                  viitospyoristys(pmax(0,p_ans_vah_pros_1 * (!!p_palkkatulo - p_ans_vah_raja_1)), p_pyoristys2),
                  viitospyoristys(pmax(0,p_ans_vah_pros_1 * (p_ans_vah_raja_2 - p_ans_vah_raja_1)), p_pyoristys2)
                ),
                
                
                # ansvah2
                ansvah2 = ifelse(
                  !!p_palkkatulo > p_ans_vah_raja_2,
                  viitospyoristys(p_ans_vah_pros_2 * (!!p_palkkatulo - p_ans_vah_raja_2), p_pyoristys2),
                  0
                ),
                
                # maksvah
                
                ansmaxvah = ifelse(
                  (ansvah1 + ansvah2) < p_ans_vah_max,
                  ansvah1 + ansvah2,
                  p_ans_vah_max
                ),
                
                # pienenemis %
                
                ans_vah_pros_vah = ifelse ( 
                  puhdasansiotulo > p_ans_vah_raja_3,
                  viitospyoristys(p_ans_vah_pros_3 * (puhdasansiotulo - p_ans_vah_raja_3), p_pyoristys2),
                  0
                ),
                
                
                # Ansiotulovahennys
                
                ansiotulovahennys = pmax( 0, ansmaxvah - ans_vah_pros_vah),
                
                # Kunnalisverotus: Elaketulovahennys
                # ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
                
                elak_vah_max = viitospyoristys(ceiling((12 * p_taysi_kansel * p_elak_vah_kerroin - p_elak_vah_vah) / 10) * 10, -1),
                
                # pienennys
                
                elak_vah_pien = ifelse(
                  puhdasansiotulo > elak_vah_max,
                  p_elak_vah_pien_pros * (puhdasansiotulo - elak_vah_max),
                  0
                ),
                
                # Elaketulovahennys
                
                elaketulovahennys = ifelse(
                  elaketulo > 0 & elak_vah_pien < elak_vah_max,
                  pmin( !!p_elaketulo, elak_vah_max - elak_vah_pien),
                  0
                ),
                
                # Kunnalisverotus: Perusvahennys
                #¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
                # laskennassa kaytetty tulo
                perus_vah_tulo = pmax( 0,
                                       puhdasansiotulo -
                                         tyoelakemaksu -
                                         tyottomyysvakuutusmaksu -
                                         paivarahamaksu -
                                         ansiotulovahennys -
                                         elaketulovahennys
                ),
                
                # pienennys
                
                perus_vah_pien = ifelse(
                  perus_vah_tulo > p_perus_vah_max,
                  viitospyoristys( p_perus_vah_pien_pros * (perus_vah_tulo - p_perus_vah_max), p_pyoristys2),
                  0
                ),
                
                # Perusvahennys
                
                perusvahennys = viitospyoristys( pmax( 0, p_perus_vah_max - perus_vah_pien), p_pyoristys2),
                
                # Kunnalisverotus
                # ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
                # Verotettava tulo kunnallisverotuksessa
                kun_ver_tulo = pmax(0, perus_vah_tulo - perusvahennys),
                
                # kunnallisvero, vahennys lasketaan myöhemmin
                kunnallisvero_ei_vah = viitospyoristys( !!p_kunnan_veropros * kun_ver_tulo, p_pyoristys2),
                
                # Maksut 2
                # ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
                # Sairaanhoitomaksun korotus
                sairaanhoitomaksu_korotus = ifelse(
                  !!p_ika < p_sairaanhoitomaksu_korotus_ikaraja,
                  viitospyoristys( pmax( 0, p_sairaanhoitomaksu_korotus_1 * (kun_ver_tulo - !!p_palkkatulo)), p_pyoristys2),
                  viitospyoristys( p_sairaanhoitomaksu_korotus_1 * kun_ver_tulo, p_pyoristys2)
                ),
                
                # Sairaanhoitomaksu
                sairaanhoitomaksu_ei_vah = viitospyoristys( p_sairhoito_pros * kun_ver_tulo, p_pyoristys2),
                
                # Kirkollisvero
                kirkollisvero_ei_vah = viitospyoristys( !!p_kirkko_veropros * kun_ver_tulo, p_pyoristys2),
                
                # YLE-vero
                yle_vero = ifelse(
                  !!p_ika >= 18,
                  viitospyoristys( pmin( p_yle_pros * pmax( 0, puhdasansiotulo - p_yle_ansioraja), p_yle_max), p_pyoristys2),
                  yle_vero
                ),
                
                # Valtion verotus
                # ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
                # maks. vah
                valt_max_vah = ceiling( (12 * p_taysi_kansel * p_valtionvero_kerroin - p_valtionvero_raja_1) / 10) * 10,
                
                # pienennys
                valtionvero_pien = ifelse(
                  puhdasansiotulo > valt_max_vah,
                  valtionvero_pien <- viitospyoristys(p_valtionvero_vah_pros * (puhdasansiotulo - valt_max_vah), p_pyoristys2),
                  0
                ),
                
                # Elaketulovahennys valtionverotuksessa
                valtionvero_elak_vah = ifelse(
                  !!p_elaketulo > 0 & valtionvero_pien < valt_max_vah,
                  pmin( !!p_elaketulo, valt_max_vah - valtionvero_pien),
                  0
                ),
                
                # Verotettava tulo valtionverotuksessa
                valtionvero_tulo = pmax( 0,
                                         puhdasansiotulo -
                                           tyoelakemaksu -
                                           tyottomyysvakuutusmaksu -
                                           paivarahamaksu -
                                           valtionvero_elak_vah
                ),
                
                # Elaketulon lisavero
                valtionvero_elak_lisa = ifelse(
                  !!p_elaketulo - valtionvero_elak_vah > p_elak_lisa_raja,
                  viitospyoristys( p_elak_lisa_pros * (!!p_elaketulo - valtionvero_elak_vah - p_elak_lisa_raja), p_pyoristys2),
                  0
                ),
                
                valtionvero_ei_vah = viitospyoristys(
                  ifelse(
                    valtionvero_tulo >= p_valtionvero_raja_1,
                    p_valtionvero_vero_1 +
                      pmax( valtionvero_tulo-p_valtionvero_raja_1,0) * p_valtionvero_pros_1 +
                      pmax( valtionvero_tulo-p_valtionvero_raja_2,0) * (p_valtionvero_pros_2 - p_valtionvero_pros_1) +
                      pmax( valtionvero_tulo-p_valtionvero_raja_3,0) * (p_valtionvero_pros_3 - p_valtionvero_pros_2) +
                      pmax( valtionvero_tulo-p_valtionvero_raja_4,0) * (p_valtionvero_pros_4 - p_valtionvero_pros_3),
                    0
                  )
                  , p_pyoristys2),
                
                
                # Työtulovahnennys
                # ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
                # maks vah
                tyotulo_max_vah = viitospyoristys( 
                  pmin( p_tyotulo_vah_max_1, 
                        p_tyotulo_vah_pros_1 * pmax(0, !!p_palkkatulo - p_tyotulo_tuloraja_1)
                  ), p_pyoristys2),
                
                # vahennyksen pienenema
                tyotulo_pien = viitospyoristys( p_tyotulo_vah_pien_2 * pmax( 0, puhdasansiotulo - p_tyotulo_tuloraja_2), p_pyoristys2),
                
                # Työtulovahennys
                tyotulovahennys = pmax( 0, tyotulo_max_vah - tyotulo_pien),
                
                # Työtulovahennys tark
                tyotulovahennys_tark = pmin( 
                  tyotulovahennys, 
                  valtionvero_ei_vah + kunnallisvero_ei_vah + kirkollisvero_ei_vah + sairaanhoitomaksu_ei_vah
                ),
                
                # Osa, jolla työtuloVähennys ylittää tuloveron
                vahennyksen_tuloveroylitys = pmax( 0, tyotulovahennys - valtionvero_ei_vah),
                
                # Vähennysten huomiointi
                # ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
                
                # Sairaanhoitomaksun, kirkollisveron ja kunallisveron vähennykset
                
                sairaanhoitomaksu_vah = ifelse( 
                  vahennyksen_tuloveroylitys > 0,
                  viitospyoristys( 
                    pmin( 
                      (sairaanhoitomaksu_ei_vah + sairaanhoitomaksu_korotus) /
                        (kirkollisvero_ei_vah + kunnallisvero_ei_vah + sairaanhoitomaksu_ei_vah + sairaanhoitomaksu_korotus) * 
                        vahennyksen_tuloveroylitys,
                      (sairaanhoitomaksu_ei_vah + sairaanhoitomaksu_korotus)),
                    p_pyoristys2),
                  0
                ),
                
                kirkollisvero_vah = ifelse(
                  vahennyksen_tuloveroylitys > 0,
                  viitospyoristys(
                    pmin(
                      kirkollisvero_ei_vah / 
                        (kirkollisvero_ei_vah + kunnallisvero_ei_vah + sairaanhoitomaksu_ei_vah + sairaanhoitomaksu_korotus) * 
                        vahennyksen_tuloveroylitys, 
                      kirkollisvero_ei_vah),
                    p_pyoristys2),
                  0
                ),
                
                kunnallisvero_vah = ifelse(
                  vahennyksen_tuloveroylitys > 0,
                  viitospyoristys( 
                    pmin(
                      kunnallisvero_ei_vah / 
                        (kunnallisvero_ei_vah + kirkollisvero_ei_vah + sairaanhoitomaksu_ei_vah + sairaanhoitomaksu_korotus) * 
                        vahennyksen_tuloveroylitys, 
                      kunnallisvero_ei_vah),
                    p_pyoristys2),
                  0
                ),
                
                # Sairaanhoitomaksu
                sairaanhoitomaksu = sairaanhoitomaksu_ei_vah + sairaanhoitomaksu_korotus - sairaanhoitomaksu_vah,
                
                # Kirkollisvero
                kirkollisvero = kirkollisvero_ei_vah - kirkollisvero_vah,
                
                # Kunnallisvero
                kunnallisvero = kunnallisvero_ei_vah - kunnallisvero_vah,
                
                # Valtion vero
                valtionvero = pmax( 0, valtionvero_ei_vah - tyotulovahennys),
                
                # Tulokset
                # ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
                # Bruttotulot (elake + etuus + palkka)
                bruttotulo = !!p_elaketulo + !!p_palkkatulo + !!p_etuustulo,
                
                # Verot yhteensa vahennysten jalkeen
                verot_yhteensa = (
                  pmax( 0,
                        (
                          kunnallisvero +
                            kirkollisvero +
                            sairaanhoitomaksu +
                            valtionvero
                        )
                  ) +
                    paivarahamaksu +
                    valtionvero_elak_lisa +
                    yle_vero
                ),
                
                # Veroprosentti
                veroprosentti = viitospyoristys( verot_yhteensa / bruttotulo, p_pyoristys4),
                
                # Verot ja maksut yhteensa (työelake- ja työttömyysvakuutusmaksu ml.)
                verot_ja_maksut_yht = verot_yhteensa + tyoelakemaksu + tyottomyysvakuutusmaksu,
                
                # nettotulot
                nettotulo = bruttotulo - verot_yhteensa,
                
                # nettotulot maksujen jalkeen
                netto_maksujen_jalkeen = bruttotulo - verot_ja_maksut_yht
  )
  
}




