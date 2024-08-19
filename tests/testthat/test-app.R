test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that(
  "options to compute",{
    expect_equal(perhevapaavertailu:::which_options_to_compute(home_care_months = 10, year_params = perhevapaavertailu:::get_parameter_year(2024)), c(0,1,2))
  }
)

test_that(
  "compute days",{
    expect_equal(
      
      perhevapaavertailu:::compute_days(opt_nr = 3,
                                        param_user = data.frame(
                                          T1 = 2100,
                                          T2 = 2100,
                                          kokhoitoaika = 12,
                                          kotihoidontuki_e_per_kk = 380
                                        ),
                                        param_year = perhevapaavertailu:::get_parameter_year(2024)), 
      list(parent_a = list(vpr_pv = 150, vpr_90_pv = 16, vpr_70_pv = 134, 
                           kotiho_pv = 0, tyossa_pv = 450, brutto_salary_per_year = 18900, 
                           brutto_benefit_per_year = 4137.82, brutto_income_per_year = 23037.82, 
                           netto_income_per_year = 20059.74), parent_b = list(vpr_pv = 150, 
                                                                              vpr_90_pv = 16, vpr_70_pv = 134, kotiho_pv = 0, tyossa_pv = 450, 
                                                                              brutto_salary_per_year = 18900, brutto_benefit_per_year = 4137.82, 
                                                                              brutto_income_per_year = 23037.82, netto_income_per_year = 20059.74), 
           household = list(brutto_income_per_year = 46075.64, netto_income_per_year = 40119.48))
    )
  }
)


test_that(
  "compute final list",{
    expect_equal(
      
      perhevapaavertailu:::compute_options_final(option_to_compute = c(0,1),
                                                 param_user = data.frame(
                                                   T1 = 2100,
                                                   T2 = 2100,
                                                   kokhoitoaika = 12,
                                                   kotihoidontuki_e_per_kk = 380
                                                 ), 
                                                 param_year = perhevapaavertailu:::get_parameter_year(2024)), 
      list(option0 = list(parent_a = list(vpr_pv = 223, vpr_90_pv = 16, 
                                          vpr_70_pv = 207, kotiho_pv = 77, tyossa_pv = 300, brutto_salary_per_year = 12600, 
                                          brutto_benefit_per_year = 6677.23, brutto_income_per_year = 19277.23, 
                                          netto_income_per_year = 17199.73), parent_b = list(vpr_90_pv = 0, 
                                                                                             vpr_70_pv = 0, kotiho_pv = 0, tyossa_pv = 600, brutto_salary_per_year = 25200, 
                                                                                             brutto_benefit_per_year = 0, brutto_income_per_year = 25200, 
                                                                                             netto_income_per_year = 21224.79), household = list(brutto_income_per_year = 44477.23, 
                                                                                                                                                 netto_income_per_year = 38424.52, brutto_change = 0, netto_change = 0)), 
           option1 = list(parent_a = list(vpr_pv = 203, vpr_90_pv = 16, 
                                          vpr_70_pv = 187, kotiho_pv = 0, tyossa_pv = 397, brutto_salary_per_year = 16674, 
                                          brutto_benefit_per_year = 5556.63, brutto_income_per_year = 22230.63, 
                                          netto_income_per_year = 19469.35), parent_b = list(vpr_pv = 97L, 
                                                                                             vpr_90_pv = 16L, vpr_70_pv = 81L, kotiho_pv = 0, tyossa_pv = 503, 
                                                                                             brutto_salary_per_year = 21126, brutto_benefit_per_year = 2719.01, 
                                                                                             brutto_income_per_year = 23845.01, netto_income_per_year = 20511.02), 
                          household = list(brutto_income_per_year = 46075.64, netto_income_per_year = 39980.37, 
                                           brutto_change = 1598.41, netto_change = 1555.84999999999)))
    )
  }
)
