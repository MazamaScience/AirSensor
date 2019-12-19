context("test-pas_sensorDeploymentID")

# Data used in all examples
data("example_pas")

test_that("input validation works", {
  expect_error(pas_sensorDeploymentID(NULL))
  expect_error(pas_sensorDeploymentID(1:10))
})

test_that("proper column is extracted", { 
  goodIDs <- pas_getIDs(example_pas, states = "MT")
  badIDs <-
    example_pas %>% 
    dplyr::filter(stateCode == "MT") %>% 
    dplyr::filter(!is.na(parentID)) %>% 
    dplyr::pull(ID)
  
  expect_equal(
    pas_sensorDeploymentID(example_pas, goodIDs),
    c(
      "84337a583b0979c7_21317",
      "903d8abbeabedf89_2342",
      "e5cbea3d6a777a45_13975",
      "e4b45078e4dc7281_28423",
      "a787764fcf7fbee1_6726",
      "ee3aa65589c99557_2851",
      "7e4fbde46641122f_3471" 
    )
  )
  
  expect_error(pas_sensorDeploymentID(example_pas, badIDs))

})


