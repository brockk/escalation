
test_that('get_dose_paths does what it should', {

  library(dplyr)

  # Scenario 1 - CRM without stopping ----
  skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
  target <- 0.25
  selector_factory <- get_dfcrm(skeleton = skeleton, target = target)

  cohort_sizes <- c(3, 3)
  paths <- selector_factory %>% get_dose_paths(cohort_sizes = cohort_sizes)
  expect_is(paths, 'dose_paths')

  df <- as_tibble(paths)
  expect_is(df, 'tbl_df')
  expect_equal(df %>% count(.depth) %>% nrow, 1 + length(cohort_sizes))
  exp_num_nodes <- num_dose_path_nodes(num_patient_outcomes = 2,
                                       cohort_sizes = cohort_sizes)
  expect_equal(df %>% count(.depth) %>% select(n) %>% .[[1]], exp_num_nodes)
  expect_equal(length(paths), sum(exp_num_nodes))

  # Trace some nodes
  node1 <- df %>% filter(.depth == 1, outcomes == 'NNN')
  expect_equal(
    node1 %>% select(next_dose) %>% .[[1]],
    selector_factory %>% fit('1NNN') %>% recommended_dose()
  )
  node1_id <- node1 %>% select(.node) %>% .[[1]]

  node2 <- df %>% filter(.depth == 2, .parent == node1_id, outcomes == 'NNT')
  expect_equal(
    node2 %>% select(next_dose) %>% .[[1]],
    selector_factory %>% fit('1NNN 4NNT') %>% recommended_dose()
  )
  node2_id <- node2 %>% select(.node) %>% .[[1]]

  # That should be it in a two-cohort scenario
  expect_equal(df %>% filter(.depth == 3) %>% nrow, 0)


  # Check a node
  node <- tail(paths, 1)[[1]]
  expect_is(node, 'dose_finding_path_node')

  expect_true('.node' %in% names(node))
  expect_is(node$.node, 'numeric')

  expect_true('.parent' %in% names(node))
  expect_is(node$.parent, 'numeric')

  expect_true('.depth' %in% names(node))
  expect_is(node$.depth, 'integer')

  expect_true('outcomes' %in% names(node))

  expect_true('next_dose' %in% names(node))
  expect_is(node$next_dose, 'integer')

  expect_true('fit' %in% names(node))
  expect_is(node$fit, 'selector')
  expect_is(node$fit, 'dfcrm_selector')

  expect_true('parent_fit' %in% names(node))
  expect_is(node$parent_fit, 'selector')
  expect_is(node$parent_fit, 'dfcrm_selector')



  # Scenario 2 - 3+3 ----
  selector_factory <- get_three_plus_three(num_doses = 4)
  cohort_sizes <- c(3, 3, 3)

  paths <- selector_factory %>% get_dose_paths(cohort_sizes = cohort_sizes)
  expect_is(paths, 'dose_paths')

  df <- as_tibble(paths)
  expect_is(df, 'tbl_df')
  expect_equal(df %>% count(.depth) %>% nrow, 1 + length(cohort_sizes))
  exp_num_nodes <- num_dose_path_nodes(num_patient_outcomes = 2,
                                       cohort_sizes = cohort_sizes)
  expect_true(
    all(df %>% count(.depth) %>% select(n) %>% .[[1]] <= exp_num_nodes)
  )
  expect_true(length(paths) <= sum(exp_num_nodes))

  # Trace some nodes
  node1 <- df %>% filter(.depth == 1, outcomes == 'NNN')
  expect_equal(
    node1 %>% select(next_dose) %>% .[[1]],
    selector_factory %>% fit('1NNN') %>% recommended_dose()
  )
  node1_id <- node1 %>% select(.node) %>% .[[1]]

  node2 <- df %>% filter(.depth == 2, .parent == node1_id, outcomes == 'NNT')
  expect_equal(
    node2 %>% select(next_dose) %>% .[[1]],
    selector_factory %>% fit('1NNN 2NNT') %>% recommended_dose()
  )
  node2_id <- node2 %>% select(.node) %>% .[[1]]

  node3 <- df %>% filter(.depth == 3, .parent == node2_id, outcomes == 'TTT')
  expect_equal(
    node3 %>% select(next_dose) %>% .[[1]],
    selector_factory %>% fit('1NNN 2NNT 2TTT') %>% recommended_dose()
  )
  node3_id <- node3 %>% select(.node) %>% .[[1]]

  # That should be it in this scenario
  expect_equal(df %>% filter(.depth == 4) %>% nrow, 0)

  # Check a node
  node <- tail(paths, 1)[[1]]
  expect_is(node, 'dose_finding_path_node')

  expect_true('.node' %in% names(node))
  expect_is(node$.node, 'numeric')

  expect_true('.parent' %in% names(node))
  expect_is(node$.parent, 'numeric')

  expect_true('.depth' %in% names(node))
  expect_is(node$.depth, 'integer')

  expect_true('outcomes' %in% names(node))

  expect_true('next_dose' %in% names(node))
  expect_is(node$next_dose, 'integer')

  expect_true('fit' %in% names(node))
  expect_is(node$fit, 'selector')
  expect_is(node$fit, 'three_plus_three_selector')

  expect_true('parent_fit' %in% names(node))
  expect_is(node$parent_fit, 'selector')
  expect_is(node$parent_fit, 'three_plus_three_selector')


  # Scenario 3 - BOIN with partial outcomes and manualnext-dose ----
  target <- 0.33
  selector_factory <- get_boin(num_doses = 6, target = target)
  cohort_sizes <- c(3, 4, 2)

  paths <- selector_factory %>%
    get_dose_paths(cohort_sizes = cohort_sizes,
                   previous_outcomes = '1NNN', next_dose = 6)
  expect_is(paths, 'dose_paths')

  df <- as_tibble(paths)
  expect_is(df, 'tbl_df')
  expect_equal(df %>% count(.depth) %>% nrow, 1 + length(cohort_sizes))
  exp_num_nodes <- num_dose_path_nodes(num_patient_outcomes = 2,
                                       cohort_sizes = cohort_sizes)
  expect_true(
    all(df %>% count(.depth) %>% select(n) %>% .[[1]] == exp_num_nodes)
  )
  expect_true(length(paths) == sum(exp_num_nodes))

  # Trace some nodes
  node1 <- df %>% filter(.depth == 1, outcomes == 'TTT')
  expect_equal(
    node1 %>% select(next_dose) %>% .[[1]],
    selector_factory %>% fit('1NNN 6TTT') %>% recommended_dose()
  )
  node1_id <- node1 %>% select(.node) %>% .[[1]]

  node2 <- df %>% filter(.depth == 2, .parent == node1_id, outcomes == 'NNTT')
  expect_equal(
    node2 %>% select(next_dose) %>% .[[1]],
    selector_factory %>% fit('1NNN 6TTT 5NNTT') %>% recommended_dose()
  )
  node2_id <- node2 %>% select(.node) %>% .[[1]]

  node3 <- df %>% filter(.depth == 3, .parent == node2_id, outcomes == 'TT')
  expect_equal(
    node3 %>% select(next_dose) %>% .[[1]],
    selector_factory %>% fit('1NNN 6TTT 5NNTT 4TT') %>% recommended_dose()
  )
  node3_id <- node3 %>% select(.node) %>% .[[1]]

  # That should be it in this scenario
  expect_equal(df %>% filter(.depth == 4) %>% nrow, 0)

  # Check a node
  node <- tail(paths, 1)[[1]]
  expect_is(node, 'dose_finding_path_node')

  expect_true('.node' %in% names(node))
  expect_is(node$.node, 'numeric')

  expect_true('.parent' %in% names(node))
  expect_is(node$.parent, 'numeric')

  expect_true('.depth' %in% names(node))
  expect_is(node$.depth, 'integer')

  expect_true('outcomes' %in% names(node))

  expect_true('next_dose' %in% names(node))
  expect_is(node$next_dose, 'integer')

  expect_true('fit' %in% names(node))
  expect_is(node$fit, 'selector')
  expect_is(node$fit, 'boin_selector')

  expect_true('parent_fit' %in% names(node))
  expect_is(node$parent_fit, 'selector')
  expect_is(node$parent_fit, 'boin_selector')

})

test_that('dose_paths supports correct interface.', {

  # Scenario 1 - CRM without stopping ----
  skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
  target <- 0.25
  selector_factory <- get_dfcrm(skeleton = skeleton, target = target)
  cohort_sizes <- c(3, 3)
  paths <- selector_factory %>% get_dose_paths(cohort_sizes = cohort_sizes)

  expect_equal(num_doses(paths), 5)
  expect_is(num_doses(paths), 'integer')
  expect_equal(length(num_doses(paths)), 1)

  expect_equal(dose_indices(paths), seq(1, 5))
  expect_is(dose_indices(paths), 'integer')
  expect_equal(length(dose_indices(paths)), 5)

  df <- as_tibble(paths)
  expect_is(df, 'tbl_df')

  # Scenario 2 - 3+3 ----
  selector_factory <- get_three_plus_three(num_doses = 4)
  cohort_sizes <- c(3, 3, 3)
  paths <- selector_factory %>% get_dose_paths(cohort_sizes = cohort_sizes)

  expect_equal(num_doses(paths), 4)
  expect_is(num_doses(paths), 'integer')
  expect_equal(length(num_doses(paths)), 1)

  expect_equal(dose_indices(paths), seq(1, 4))
  expect_is(dose_indices(paths), 'integer')
  expect_equal(length(dose_indices(paths)), 4)

  df <- as_tibble(paths)
  expect_is(df, 'tbl_df')

  # Scenario 3 - BOIN ----
  target <- 0.33
  selector_factory <- get_boin(num_doses = 6, target = target)
  cohort_sizes <- c(3, 4, 2)

  paths <- selector_factory %>%
    get_dose_paths(cohort_sizes = cohort_sizes,
                   previous_outcomes = '1NNN', next_dose = 6)

  expect_equal(num_doses(paths), 6)
  expect_is(num_doses(paths), 'integer')
  expect_equal(length(num_doses(paths)), 1)

  expect_equal(dose_indices(paths), seq(1, 6))
  expect_is(dose_indices(paths), 'integer')
  expect_equal(length(dose_indices(paths)), 6)

  df <- as_tibble(paths)
  expect_is(df, 'tbl_df')
})
