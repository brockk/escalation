
test_that('BOIN12 matches published example.', {

  model <- get_boin12(num_doses = 5, phi_t = 0.35, phi_e = 0.25,
                      u2 = 40, u3 = 60, n_star = 6) %>%
    stop_at_n(n = 30) %>%
    stop_when_n_at_dose(n = 12, dose = "any")

  # p.1396 of Lin et al.
  fit <- model %>% fit("1NNN 2ENT 3ETT 2EEN")
  expect_equal(recommended_dose(fit), 2)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 2."
  )

  # Section 6 and Figure S2 of Data Supplement
  fit <- model %>% fit("1NNN")
  expect_equal(recommended_dose(fit), 2)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 2."
  )

  fit <- model %>% fit("1NNN 2NNB")
  expect_equal(recommended_dose(fit), 3)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 3."
  )

  fit <- model %>% fit("1NNN 2NNB 3NEE")
  expect_equal(recommended_dose(fit), 3)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 3."
  )

  fit <- model %>% fit("1NNN 2NNB 3NEE 3NTN")
  expect_equal(recommended_dose(fit), 4)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )

  fit <- model %>% fit("1NNN 2NNB 3NEE 3NTN 4ETE")
  expect_equal(recommended_dose(fit), 4)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )

  fit <- model %>% fit("1NNN 2NNB 3NEE 3NTN 4ETE 4NBE")
  expect_equal(recommended_dose(fit), 4)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )

  fit <- model %>% fit("1NNN 2NNB 3NEE 3NTN 4ETE 4NBE 4BNN")
  expect_equal(recommended_dose(fit), 5)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 5."
  )

  fit <- model %>% fit("1NNN 2NNB 3NEE 3NTN 4ETE 4NBE 4BNN 5BEN")
  expect_equal(recommended_dose(fit), 5)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 5."
  )

  fit <- model %>% fit("1NNN 2NNB 3NEE 3NTN 4ETE 4NBE 4BNN 5BEN 5NBT")
  expect_equal(recommended_dose(fit), 4)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )

  fit <- model %>% fit("1NNN 2NNB 3NEE 3NTN 4ETE 4NBE 4BNN 5BEN 5NBT 4NEN")
  expect_equal(recommended_dose(fit), 4)
  expect_equal(continue(fit), FALSE)
  expect_output(
    print(fit),
    "The model advocates stopping and recommending dose 4."
  )

})

test_that('BOIN12 matches MDAnderson Shiny app - parameterisation 1', {

  # See https://biostatistics.mdanderson.org/shinyapps/BOIN12/

  # Default parameterisation
  model <- get_boin12(num_doses = 5, phi_t = 0.35, phi_e = 0.25,
                      u1 = 100, u2 = 40, u3 = 60, u4 = 0,
                      n_star = 6, c_t = 0.95, c_e = 0.9,
                      start_dose = 1, prior_alpha = 1, prior_beta = 1) %>%
    stop_at_n(n = 36) %>%
    stop_when_n_at_dose(n = 12, dose = "any")


  # Starting at bottom dose

  # Breaks the app
  # fit <- model %>% fit("1BBB")
  # expect_equal(recommended_dose(fit), 2)
  # expect_equal(continue(fit), TRUE)

  fit <- model %>% fit("1EBB")
  expect_equal(recommended_dose(fit), 1)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )

  fit <- model %>% fit("1EEB")
  expect_equal(recommended_dose(fit), 1)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )

  fit <- model %>% fit("1EEE")
  expect_equal(recommended_dose(fit), 1)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )

  fit <- model %>% fit("1EET")
  expect_equal(recommended_dose(fit), 1)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )

  fit <- model %>% fit("1ETB")
  expect_equal(recommended_dose(fit), 1)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )

  fit <- model %>% fit("1ETT")
  expect_equal(recommended_dose(fit), 1)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )

  fit <- model %>% fit("1NBB")
  expect_equal(recommended_dose(fit), 1)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )

  fit <- model %>% fit("1NEB")
  expect_equal(recommended_dose(fit), 1)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )

  fit <- model %>% fit("1NEE")
  expect_equal(recommended_dose(fit), 1)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )

  fit <- model %>% fit("1NET")
  expect_equal(recommended_dose(fit), 2)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 2."
  )

  fit <- model %>% fit("1NNB")
  expect_equal(recommended_dose(fit), 2)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 2."
  )

  fit <- model %>% fit("1NNE")
  expect_equal(recommended_dose(fit), 2)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 2."
  )

  fit <- model %>% fit("1NNN")
  expect_equal(recommended_dose(fit), 2)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 2."
  )

  fit <- model %>% fit("1NNT")
  expect_equal(recommended_dose(fit), 2)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 2."
  )

  fit <- model %>% fit("1NTB")
  expect_equal(recommended_dose(fit), 1)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )

  fit <- model %>% fit("1NTT")
  expect_equal(recommended_dose(fit), 1)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )

  # Breaks the app
  # fit <- model %>% fit("1TBB")
  # expect_equal(recommended_dose(fit), 2)
  # expect_equal(continue(fit), TRUE)

  # Breaks the app
  # fit <- model %>% fit("1TTB")
  # expect_equal(recommended_dose(fit), 2)
  # expect_equal(continue(fit), TRUE)

  # Breaks the app
  # fit <- model %>% fit("1TTT")
  # expect_equal(recommended_dose(fit), 2)
  # expect_equal(continue(fit), TRUE)


  # Starting at middling dose
  fit <- model %>% fit("2BBB")
  expect_equal(recommended_dose(fit), 1)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )

  fit <- model %>% fit("2EBB")
  expect_equal(recommended_dose(fit), 1)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )

  fit <- model %>% fit("2EEB")
  expect_equal(recommended_dose(fit), 2)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 2."
  )

  fit <- model %>% fit("2EEE")
  expect_equal(recommended_dose(fit), 2)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 2."
  )

  fit <- model %>% fit("2EET")
  expect_equal(recommended_dose(fit), 2)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 2."
  )

  fit <- model %>% fit("2ETB")
  expect_equal(recommended_dose(fit), 1)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )

  fit <- model %>% fit("2ETT")
  expect_equal(recommended_dose(fit), 1)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )

  fit <- model %>% fit("2NBB")
  expect_equal(recommended_dose(fit), 1)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )

  fit <- model %>% fit("2NEB")
  expect_equal(recommended_dose(fit), 2)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 2."
  )

  fit <- model %>% fit("2NEE")
  expect_equal(recommended_dose(fit), 2)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 2."
  )

  fit <- model %>% fit("2NET")
  expect_equal(recommended_dose(fit), 3)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 3."
  )

  fit <- model %>% fit("2NNB")
  expect_equal(recommended_dose(fit), 3)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 3."
  )

  fit <- model %>% fit("2NNE")
  expect_equal(recommended_dose(fit), 3)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 3."
  )

  fit <- model %>% fit("2NNN")
  expect_equal(recommended_dose(fit), 3)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 3."
  )

  fit <- model %>% fit("2NNT")
  expect_equal(recommended_dose(fit), 3)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 3."
  )

  fit <- model %>% fit("2NTB")
  expect_equal(recommended_dose(fit), 1)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )

  fit <- model %>% fit("2NTT")
  expect_equal(recommended_dose(fit), 1)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )

  fit <- model %>% fit("2TBB")
  expect_equal(recommended_dose(fit), 1)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )

  fit <- model %>% fit("2TTB")
  expect_equal(recommended_dose(fit), 1)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )

  fit <- model %>% fit("2TTT")
  expect_equal(recommended_dose(fit), 1)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )


  # Starting at top dose
  fit <- model %>% fit("5BBB")
  expect_equal(recommended_dose(fit), 4)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )

  fit <- model %>% fit("5EBB")
  expect_equal(recommended_dose(fit), 4)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )

  fit <- model %>% fit("5EEB")
  expect_equal(recommended_dose(fit), 5)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 5."
  )

  fit <- model %>% fit("5EEE")
  expect_equal(recommended_dose(fit), 5)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 5."
  )

  fit <- model %>% fit("5EET")
  expect_equal(recommended_dose(fit), 5)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 5."
  )

  fit <- model %>% fit("5ETB")
  expect_equal(recommended_dose(fit), 4)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )

  fit <- model %>% fit("5ETT")
  expect_equal(recommended_dose(fit), 4)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )

  fit <- model %>% fit("5NBB")
  expect_equal(recommended_dose(fit), 4)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )

  fit <- model %>% fit("5NEB")
  expect_equal(recommended_dose(fit), 5)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 5."
  )

  fit <- model %>% fit("5NEE")
  expect_equal(recommended_dose(fit), 5)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 5."
  )

  fit <- model %>% fit("5NET")
  expect_equal(recommended_dose(fit), 4)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )

  fit <- model %>% fit("5NNB")
  expect_equal(recommended_dose(fit), 4)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )

  fit <- model %>% fit("5NNE")
  expect_equal(recommended_dose(fit), 4)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )

  fit <- model %>% fit("5NNN")
  expect_equal(recommended_dose(fit), 4)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )

  fit <- model %>% fit("5NNT")
  expect_equal(recommended_dose(fit), 4)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )

  fit <- model %>% fit("5NTB")
  expect_equal(recommended_dose(fit), 4)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )

  fit <- model %>% fit("5NTT")
  expect_equal(recommended_dose(fit), 4)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )

  fit <- model %>% fit("5TBB")
  expect_equal(recommended_dose(fit), 4)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )

  fit <- model %>% fit("5TTB")
  expect_equal(recommended_dose(fit), 4)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )

  fit <- model %>% fit("5TTT")
  expect_equal(recommended_dose(fit), 4)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )

})

test_that('BOIN12 matches MDAnderson Shiny app - parameterisation 2', {


  # See https://biostatistics.mdanderson.org/shinyapps/BOIN12/

  # Non-standard parameterisation
  model <- get_boin12(num_doses = 5, phi_t = 0.15, phi_e = 0.4,
                      u1 = 100, u2 = 50, u3 = 30, u4 = 0,
                      n_star = 6, c_t = 0.9, c_e = 0.8,
                      start_dose = 1, prior_alpha = 1, prior_beta = 1) %>%
    stop_at_n(n = 36) %>%
    stop_when_n_at_dose(n = 12, dose = "any")

  # Starting at bottom dose

  # Breaks the app
  # fit <- model %>% fit("1BBB")
  # expect_equal(recommended_dose(fit), 2)
  # expect_equal(continue(fit), TRUE)

  fit <- model %>% fit("1EBB")
  expect_equal(recommended_dose(fit), 1)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )

  fit <- model %>% fit("1EEB")
  expect_equal(recommended_dose(fit), 1)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )

  fit <- model %>% fit("1EEE")
  expect_equal(recommended_dose(fit), 1)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )

  fit <- model %>% fit("1EET")
  expect_equal(recommended_dose(fit), 1)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )

  fit <- model %>% fit("1ETB")
  expect_equal(recommended_dose(fit), 1)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )

  fit <- model %>% fit("1ETT")
  expect_equal(recommended_dose(fit), 1)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )

  fit <- model %>% fit("1NBB")
  expect_equal(recommended_dose(fit), 1)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )

  fit <- model %>% fit("1NEB")
  expect_equal(recommended_dose(fit), 1)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )

  fit <- model %>% fit("1NEE")
  expect_equal(recommended_dose(fit), 1)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )

  fit <- model %>% fit("1NET")
  expect_equal(recommended_dose(fit), 1)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )

  fit <- model %>% fit("1NNB")
  expect_equal(recommended_dose(fit), 1)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )

  fit <- model %>% fit("1NNE")
  expect_equal(recommended_dose(fit), 2)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 2."
  )

  fit <- model %>% fit("1NNN")
  expect_equal(recommended_dose(fit), 2)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 2."
  )

  # Breaks the app
  # fit <- model %>% fit("1NNT")
  # expect_equal(recommended_dose(fit), 2)
  # expect_equal(continue(fit), TRUE)

  fit <- model %>% fit("1NTB")
  expect_equal(recommended_dose(fit), 1)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )

  # Breaks the app
  # fit <- model %>% fit("1NTT")
  # expect_equal(recommended_dose(fit), 1)
  # expect_equal(continue(fit), TRUE)

  # Breaks the app
  # fit <- model %>% fit("1TBB")
  # expect_equal(recommended_dose(fit), 2)
  # expect_equal(continue(fit), TRUE)

  # Breaks the app
  # fit <- model %>% fit("1TTB")
  # expect_equal(recommended_dose(fit), 2)
  # expect_equal(continue(fit), TRUE)

  # Breaks the app
  # fit <- model %>% fit("1TTT")
  # expect_equal(recommended_dose(fit), 2)
  # expect_equal(continue(fit), TRUE)


  # Starting at middling dose
  fit <- model %>% fit("2BBB")
  expect_equal(recommended_dose(fit), 1)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )

  fit <- model %>% fit("2EBB")
  expect_equal(recommended_dose(fit), 1)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )

  fit <- model %>% fit("2EEB")
  expect_equal(recommended_dose(fit), 1)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )

  fit <- model %>% fit("2EEE")
  expect_equal(recommended_dose(fit), 2)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 2."
  )

  fit <- model %>% fit("2EET")
  expect_equal(recommended_dose(fit), 1)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )

  fit <- model %>% fit("2ETB")
  expect_equal(recommended_dose(fit), 1)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )

  fit <- model %>% fit("2ETT")
  expect_equal(recommended_dose(fit), 1)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )

  fit <- model %>% fit("2NBB")
  expect_equal(recommended_dose(fit), 1)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )

  fit <- model %>% fit("2NEB")
  expect_equal(recommended_dose(fit), 1)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )

  fit <- model %>% fit("2NEE")
  expect_equal(recommended_dose(fit), 2)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 2."
  )

  fit <- model %>% fit("2NET")
  expect_equal(recommended_dose(fit), 1)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )

  fit <- model %>% fit("2NNB")
  expect_equal(recommended_dose(fit), 1)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )

  # fit <- model %>% fit("2NNE")
  # expect_equal(recommended_dose(fit), 1)
  # expect_equal(continue(fit), TRUE)
  # In this sitation, d1 and d3 have equal utility. Their Shiny app chooses d1.
  # In other situations, their app will resolve ties by selelcting the higher
  # dose. This remains an ambiguous situation.

  fit <- model %>% fit("2NNN")
  expect_equal(recommended_dose(fit), 3)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 3."
  )

  fit <- model %>% fit("2NNT")
  expect_equal(recommended_dose(fit), 1)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )

  fit <- model %>% fit("2NTB")
  expect_equal(recommended_dose(fit), 1)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )

  fit <- model %>% fit("2NTT")
  expect_equal(recommended_dose(fit), 1)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )

  fit <- model %>% fit("2TBB")
  expect_equal(recommended_dose(fit), 1)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )

  fit <- model %>% fit("2TTB")
  expect_equal(recommended_dose(fit), 1)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )

  fit <- model %>% fit("2TTT")
  expect_equal(recommended_dose(fit), 1)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )

  # Starting at top dose
  fit <- model %>% fit("5BBB")
  expect_equal(recommended_dose(fit), 4)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )

  fit <- model %>% fit("5EBB")
  expect_equal(recommended_dose(fit), 4)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )

  fit <- model %>% fit("5EEB")
  expect_equal(recommended_dose(fit), 4)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )

  fit <- model %>% fit("5EEE")
  expect_equal(recommended_dose(fit), 5)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 5."
  )

  fit <- model %>% fit("5EET")
  expect_equal(recommended_dose(fit), 4)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )

  fit <- model %>% fit("5ETB")
  expect_equal(recommended_dose(fit), 4)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )

  fit <- model %>% fit("5ETT")
  expect_equal(recommended_dose(fit), 4)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )

  fit <- model %>% fit("5NBB")
  expect_equal(recommended_dose(fit), 4)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )

  fit <- model %>% fit("5NEB")
  expect_equal(recommended_dose(fit), 4)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )

  fit <- model %>% fit("5NEE")
  expect_equal(recommended_dose(fit), 5)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 5."
  )

  fit <- model %>% fit("5NET")
  expect_equal(recommended_dose(fit), 4)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )

  fit <- model %>% fit("5NNB")
  expect_equal(recommended_dose(fit), 4)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )

  fit <- model %>% fit("5NNE")
  expect_equal(recommended_dose(fit), 4)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )

  fit <- model %>% fit("5NNN")
  expect_equal(recommended_dose(fit), 4)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )

  fit <- model %>% fit("5NNT")
  expect_equal(recommended_dose(fit), 4)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )

  fit <- model %>% fit("5NTB")
  expect_equal(recommended_dose(fit), 4)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )

  fit <- model %>% fit("5NTT")
  expect_equal(recommended_dose(fit), 4)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )

  fit <- model %>% fit("5TBB")
  expect_equal(recommended_dose(fit), 4)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )

  fit <- model %>% fit("5TTB")
  expect_equal(recommended_dose(fit), 4)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )

  fit <- model %>% fit("5TTT")
  expect_equal(recommended_dose(fit), 4)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )

})

test_that('boin12_selector supports correct interface.', {

  model_fitter <- get_boin12(num_doses = 5, phi_t = 0.35, phi_e = 0.25,
                      u2 = 40, u3 = 60, n_star = 6) %>%
    stop_at_n(n = 30) %>%
    stop_when_n_at_dose(n = 12, dose = "any")

  # Example 1, using outcome string
  x <- fit(model_fitter, '1NEN 2NBT')

  expect_true(is.null(tox_target(x)))

  expect_equal(tox_limit(x), 0.35)
  expect_true(is.numeric(tox_limit(x)))

  expect_equal(eff_limit(x), 0.25)
  expect_true(is.numeric(eff_limit(x)))

  expect_equal(num_patients(x), 6)
  expect_true(is.integer(num_patients(x)))

  expect_equal(cohort(x), c(1,1,1, 2,2,2))
  expect_true(is.integer(cohort(x)))
  expect_equal(length(cohort(x)), num_patients(x))

  expect_equal(doses_given(x), unname(c(1,1,1, 2,2,2)))
  expect_true(is.integer(doses_given(x)))
  expect_equal(length(doses_given(x)), num_patients(x))

  expect_equal(tox(x), c(0,0,0, 0,1,1))
  expect_true(is.integer(tox(x)))
  expect_equal(length(tox(x)), num_patients(x))

  expect_equal(num_tox(x), 2)
  expect_true(is.integer(num_tox(x)))

  expect_equal(eff(x), c(0,1,0, 0,1,0))
  expect_true(is.integer(eff(x)))
  expect_equal(length(eff(x)), num_patients(x))

  expect_equal(num_eff(x), 2)
  expect_true(is.integer(num_eff(x)))

  expect_true(all((model_frame(x) - data.frame(patient = c(1,2,3,4,5,6),
                                               cohort = c(1,1,1,2,2,2),
                                               dose = c(1,1,1,2,2,2),
                                               tox = c(0,0,0,0,1,1),
                                               eff = c(0,1,0,0,1,0))) == 0))
  expect_equal(nrow(model_frame(x)), num_patients(x))

  expect_equal(num_doses(x), 5)
  expect_true(is.integer(tox(x)))

  expect_equal(dose_indices(x), 1:5)
  expect_true(is.integer(dose_indices(x)))
  expect_equal(length(dose_indices(x)), num_doses(x))

  expect_true(is.integer(recommended_dose(x)))
  expect_equal(length(recommended_dose(x)), 1)

  expect_true(is.logical(continue(x)))

  expect_equal(n_at_dose(x), c(3,3,0,0,0))
  expect_true(is.integer(n_at_dose(x)))
  expect_equal(length(n_at_dose(x)), num_doses(x))

  expect_equal(n_at_dose(x, dose = 0), 0)
  expect_true(is.integer(n_at_dose(x, dose = 0)))
  expect_equal(length(n_at_dose(x, dose = 0)), 1)

  expect_equal(n_at_dose(x, dose = 1), 3)
  expect_true(is.integer(n_at_dose(x, dose = 1)))
  expect_equal(length(n_at_dose(x, dose = 1)), 1)

  expect_true(is.integer(n_at_dose(x, dose = 'recommended')))
  expect_equal(length(n_at_dose(x, dose = 'recommended')), 1)

  expect_true(is.integer(n_at_recommended_dose(x)))
  expect_equal(length(n_at_recommended_dose(x)), 1)

  expect_equal(is_randomising(x), FALSE)
  expect_true(is.logical(is_randomising(x)))
  expect_equal(length(is_randomising(x)), 1)

  expect_equal(unname(prob_administer(x)), c(0.5,0.5,0,0,0))
  expect_true(is.numeric(prob_administer(x)))
  expect_equal(length(prob_administer(x)), num_doses(x))

  expect_equal(tox_at_dose(x), c(0,2,0,0,0))
  expect_true(is.integer(tox_at_dose(x)))
  expect_equal(length(tox_at_dose(x)), num_doses(x))

  expect_equal(eff_at_dose(x), c(1,1,0,0,0))
  expect_true(is.integer(eff_at_dose(x)))
  expect_equal(length(eff_at_dose(x)), num_doses(x))

  expect_true(is.numeric(empiric_tox_rate(x)))
  expect_equal(length(empiric_tox_rate(x)), num_doses(x))

  expect_true(is.numeric(mean_prob_tox(x)))
  expect_equal(length(mean_prob_tox(x)), num_doses(x))

  expect_true(is.numeric(median_prob_tox(x)))
  expect_equal(length(median_prob_tox(x)), num_doses(x))

  expect_true(is.numeric(empiric_eff_rate(x)))
  expect_equal(length(empiric_eff_rate(x)), num_doses(x))

  expect_true(is.numeric(mean_prob_eff(x)))
  expect_equal(length(mean_prob_eff(x)), num_doses(x))

  expect_true(is.numeric(median_prob_eff(x)))
  expect_equal(length(median_prob_eff(x)), num_doses(x))

  expect_true(is.logical(dose_admissible(x)))
  expect_equal(length(dose_admissible(x)), num_doses(x))

  expect_true(is.numeric(prob_tox_quantile(x, p = 0.9)))
  expect_equal(length(prob_tox_quantile(x, p = 0.9)), num_doses(x))

  expect_true(is.numeric(prob_tox_exceeds(x, 0.5)))
  expect_equal(length(prob_tox_exceeds(x, 0.5)), num_doses(x))

  expect_true(is.numeric(prob_eff_quantile(x, p = 0.9)))
  expect_equal(length(prob_eff_quantile(x, p = 0.9)), num_doses(x))

  expect_true(is.numeric(prob_eff_exceeds(x, 0.5)))
  expect_equal(length(prob_eff_exceeds(x, 0.5)), num_doses(x))

  expect_true(is.logical(supports_sampling(x)))

  expect_error(prob_tox_samples(x))
  expect_error(prob_tox_samples(x, tall = TRUE))

  expect_error(prob_eff_samples(x))
  expect_error(prob_eff_samples(x, tall = TRUE))

  # Expect summary to not error. This is how that is tested, apparently:
  expect_error(summary(x), NA)
  expect_output(print(x))
  expect_true(tibble::is_tibble(as_tibble(x)))
  expect_true(nrow(as_tibble(x)) >= num_doses(x))



  # Example 2, using trivial outcome string
  x <- fit(model_fitter, '')

  expect_true(is.null(tox_target(x)))

  expect_equal(tox_limit(x), 0.35)
  expect_true(is.numeric(tox_limit(x)))

  expect_equal(eff_limit(x), 0.25)
  expect_true(is.numeric(eff_limit(x)))

  expect_equal(num_patients(x), 0)
  expect_true(is.integer(num_patients(x)))

  expect_equal(cohort(x), integer(length = 0))
  expect_true(is.integer(cohort(x)))
  expect_equal(length(cohort(x)), num_patients(x))

  expect_equal(doses_given(x), integer(length = 0))
  expect_true(is.integer(doses_given(x)))
  expect_equal(length(doses_given(x)), num_patients(x))

  expect_equal(tox(x), integer(length = 0))
  expect_true(is.integer(tox(x)))
  expect_equal(length(tox(x)), num_patients(x))

  expect_equal(num_tox(x), 0)
  expect_true(is.integer(num_tox(x)))

  expect_equal(eff(x), integer(length = 0))
  expect_true(is.integer(eff(x)))
  expect_equal(length(eff(x)), num_patients(x))

  expect_equal(num_eff(x), 0)
  expect_true(is.integer(num_eff(x)))

  mf <- model_frame(x)
  expect_equal(nrow(mf), 0)
  expect_equal(ncol(mf), 5)

  expect_equal(num_doses(x), 5)
  expect_true(is.integer(num_doses(x)))

  expect_equal(dose_indices(x), 1:5)
  expect_true(is.integer(dose_indices(x)))
  expect_equal(length(dose_indices(x)), num_doses(x))

  expect_equal(recommended_dose(x), 1)
  expect_true(is.integer(recommended_dose(x)))
  expect_equal(length(recommended_dose(x)), 1)

  expect_equal(continue(x), TRUE)
  expect_true(is.logical(continue(x)))

  expect_equal(n_at_dose(x), c(0,0,0,0,0))
  expect_true(is.integer(n_at_dose(x)))
  expect_equal(length(n_at_dose(x)), num_doses(x))

  expect_equal(n_at_dose(x, dose = 0), 0)
  expect_true(is.integer(n_at_dose(x, dose = 0)))
  expect_equal(length(n_at_dose(x, dose = 0)), 1)

  expect_equal(n_at_dose(x, dose = 1), 0)
  expect_true(is.integer(n_at_dose(x, dose = 1)))
  expect_equal(length(n_at_dose(x, dose = 1)), 1)

  expect_equal(n_at_dose(x, dose = 'recommended'), 0)
  expect_true(is.integer(n_at_dose(x, dose = 'recommended')))
  expect_equal(length(n_at_dose(x, dose = 'recommended')), 1)

  expect_equal(n_at_recommended_dose(x), 0)
  expect_true(is.integer(n_at_recommended_dose(x)))
  expect_equal(length(n_at_recommended_dose(x)), 1)

  expect_equal(is_randomising(x), FALSE)
  expect_true(is.logical(is_randomising(x)))
  expect_equal(length(is_randomising(x)), 1)

  expect_equal(tox_at_dose(x), c(0,0,0,0,0))
  expect_true(is.integer(tox_at_dose(x)))
  expect_equal(length(tox_at_dose(x)), num_doses(x))

  expect_equal(eff_at_dose(x), c(0,0,0,0,0))
  expect_true(is.integer(eff_at_dose(x)))
  expect_equal(length(eff_at_dose(x)), num_doses(x))

  expect_true(is.numeric(empiric_tox_rate(x)))
  expect_equal(length(empiric_tox_rate(x)), num_doses(x))

  expect_true(is.numeric(mean_prob_tox(x)))
  expect_equal(length(mean_prob_tox(x)), num_doses(x))

  expect_true(is.numeric(median_prob_tox(x)))
  expect_equal(length(median_prob_tox(x)), num_doses(x))

  expect_true(is.numeric(empiric_eff_rate(x)))
  expect_equal(length(empiric_eff_rate(x)), num_doses(x))

  expect_true(is.numeric(mean_prob_eff(x)))
  expect_equal(length(mean_prob_eff(x)), num_doses(x))

  expect_true(is.numeric(median_prob_eff(x)))
  expect_equal(length(median_prob_eff(x)), num_doses(x))

  expect_true(is.logical(dose_admissible(x)))
  expect_equal(length(dose_admissible(x)), num_doses(x))

  expect_true(is.numeric(prob_tox_quantile(x, p = 0.9)))
  expect_equal(length(prob_tox_quantile(x, p = 0.9)), num_doses(x))

  expect_true(is.numeric(prob_tox_exceeds(x, 0.5)))
  expect_equal(length(prob_tox_exceeds(x, 0.5)), num_doses(x))

  expect_true(is.numeric(prob_eff_quantile(x, p = 0.9)))
  expect_equal(length(prob_eff_quantile(x, p = 0.9)), num_doses(x))

  expect_true(is.numeric(prob_eff_exceeds(x, 0.5)))
  expect_equal(length(prob_eff_exceeds(x, 0.5)), num_doses(x))

  expect_true(is.logical(supports_sampling(x)))

  expect_error(prob_tox_samples(x))
  expect_error(prob_tox_samples(x, tall = TRUE))

  expect_error(prob_eff_samples(x))
  expect_error(prob_eff_samples(x, tall = TRUE))

  # Expect summary to not error. This is how that is tested, apparently:
  expect_error(summary(x), NA)
  expect_output(print(x))
  expect_true(tibble::is_tibble(as_tibble(x)))
  expect_true(nrow(as_tibble(x)) >= num_doses(x))



  # Example 3, using tibble of outcomes
  outcomes <- tibble(
    cohort = c(1,1,1, 2,2,2),
    dose = c(1,1,1, 2,2,2),
    tox = c(0,0, 0,0, 1,1),
    eff = c(0,1, 0,0, 1,0)
  )
  x <- fit(model_fitter, outcomes)

  expect_true(is.null(tox_target(x)))

  expect_equal(tox_limit(x), 0.35)
  expect_true(is.numeric(tox_limit(x)))

  expect_equal(eff_limit(x), 0.25)
  expect_true(is.numeric(eff_limit(x)))

  expect_equal(num_patients(x), 6)
  expect_true(is.integer(num_patients(x)))

  expect_equal(cohort(x), c(1,1,1, 2,2,2))
  expect_true(is.integer(cohort(x)))
  expect_equal(length(cohort(x)), num_patients(x))

  expect_equal(doses_given(x), unname(c(1,1,1, 2,2,2)))
  expect_true(is.integer(doses_given(x)))
  expect_equal(length(doses_given(x)), num_patients(x))

  expect_equal(tox(x), c(0,0,0, 0,1,1))
  expect_true(is.integer(tox(x)))
  expect_equal(length(tox(x)), num_patients(x))

  expect_equal(num_tox(x), 2)
  expect_true(is.integer(num_tox(x)))

  expect_equal(eff(x), c(0,1,0, 0,1,0))
  expect_true(is.integer(eff(x)))
  expect_equal(length(eff(x)), num_patients(x))

  expect_equal(num_eff(x), 2)
  expect_true(is.integer(num_eff(x)))

  expect_true(all((model_frame(x) - data.frame(patient = c(1,2,3,4,5,6),
                                               cohort = c(1,1,1,2,2,2),
                                               dose = c(1,1,1,2,2,2),
                                               tox = c(0,0,0,0,1,1),
                                               eff = c(0,1,0,0,1,0))) == 0))
  expect_equal(nrow(model_frame(x)), num_patients(x))

  expect_equal(num_doses(x), 5)
  expect_true(is.integer(tox(x)))

  expect_equal(dose_indices(x), 1:5)
  expect_true(is.integer(dose_indices(x)))
  expect_equal(length(dose_indices(x)), num_doses(x))

  expect_true(is.integer(recommended_dose(x)))
  expect_equal(length(recommended_dose(x)), 1)

  expect_equal(continue(x), TRUE)
  expect_true(is.logical(continue(x)))

  expect_equal(n_at_dose(x), c(3,3,0,0,0))
  expect_true(is.integer(n_at_dose(x)))
  expect_equal(length(n_at_dose(x)), num_doses(x))

  expect_equal(n_at_dose(x, dose = 0), 0)
  expect_true(is.integer(n_at_dose(x, dose = 0)))
  expect_equal(length(n_at_dose(x, dose = 0)), 1)

  expect_equal(n_at_dose(x, dose = 1), 3)
  expect_true(is.integer(n_at_dose(x, dose = 1)))
  expect_equal(length(n_at_dose(x, dose = 1)), 1)

  expect_true(is.integer(n_at_dose(x, dose = 'recommended')))
  expect_equal(length(n_at_dose(x, dose = 'recommended')), 1)

  expect_true(is.integer(n_at_recommended_dose(x)))
  expect_equal(length(n_at_recommended_dose(x)), 1)

  expect_equal(is_randomising(x), FALSE)
  expect_true(is.logical(is_randomising(x)))
  expect_equal(length(is_randomising(x)), 1)

  expect_equal(unname(prob_administer(x)), c(0.5,0.5,0,0,0))
  expect_true(is.numeric(prob_administer(x)))
  expect_equal(length(prob_administer(x)), num_doses(x))

  expect_equal(tox_at_dose(x), c(0,2,0,0,0))
  expect_true(is.integer(tox_at_dose(x)))
  expect_equal(length(tox_at_dose(x)), num_doses(x))

  expect_equal(eff_at_dose(x), c(1,1,0,0,0))
  expect_true(is.integer(eff_at_dose(x)))
  expect_equal(length(eff_at_dose(x)), num_doses(x))

  expect_true(is.numeric(empiric_tox_rate(x)))
  expect_equal(length(empiric_tox_rate(x)), num_doses(x))

  expect_true(is.numeric(mean_prob_tox(x)))
  expect_equal(length(mean_prob_tox(x)), num_doses(x))

  expect_true(is.numeric(median_prob_tox(x)))
  expect_equal(length(median_prob_tox(x)), num_doses(x))

  expect_true(is.numeric(empiric_eff_rate(x)))
  expect_equal(length(empiric_eff_rate(x)), num_doses(x))

  expect_true(is.numeric(mean_prob_eff(x)))
  expect_equal(length(mean_prob_eff(x)), num_doses(x))

  expect_true(is.numeric(median_prob_eff(x)))
  expect_equal(length(median_prob_eff(x)), num_doses(x))

  expect_true(is.logical(dose_admissible(x)))
  expect_equal(length(dose_admissible(x)), num_doses(x))

  expect_true(is.numeric(prob_tox_quantile(x, p = 0.9)))
  expect_equal(length(prob_tox_quantile(x, p = 0.9)), num_doses(x))

  expect_true(is.numeric(prob_tox_exceeds(x, 0.5)))
  expect_equal(length(prob_tox_exceeds(x, 0.5)), num_doses(x))

  expect_true(is.numeric(prob_eff_quantile(x, p = 0.9)))
  expect_equal(length(prob_eff_quantile(x, p = 0.9)), num_doses(x))

  expect_true(is.numeric(prob_eff_exceeds(x, 0.5)))
  expect_equal(length(prob_eff_exceeds(x, 0.5)), num_doses(x))

  expect_true(is.logical(supports_sampling(x)))

  expect_error(prob_tox_samples(x))
  expect_error(prob_tox_samples(x, tall = TRUE))

  expect_error(prob_eff_samples(x))
  expect_error(prob_eff_samples(x, tall = TRUE))

  # Expect summary to not error. This is how that is tested, apparently:
  expect_error(summary(x), NA)
  expect_output(print(x))
  expect_true(tibble::is_tibble(as_tibble(x)))
  expect_true(nrow(as_tibble(x)) >= num_doses(x))

})
