context("test-pas_deviceDeploymentID")

# Data used in all examples
data("example_pas")

test_that("input validation works", {
  expect_error(pas_deviceDeploymentID(NULL))
  expect_error(pas_deviceDeploymentID(1:10))
})

test_that("proper column is extracted", { 
  
  goodIDs <- pas_getIDs(example_pas, states = "SD")
  badIDs <-
    example_pas %>% 
    dplyr::filter(stateCode == "SD") %>% 
    dplyr::filter(!is.na(parentID)) %>% 
    dplyr::pull(ID)
  
  expect_equal(
    pas_deviceDeploymentID(example_pas, goodIDs),
    c(
      "fc9c64c581d17159_3097",
      "0cfbbc79701699e6_26059",
      "936aed4f3c39a9d2_2506",
      "1e8bd73ba1309282_8376",
      "c65684300a680cae_5656"
      )
  )
  
  expect_error(pas_deviceDeploymentID(example_pas, badIDs))

})


