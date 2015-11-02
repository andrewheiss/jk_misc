library(testthat)
# TODO: Set up tests the actual way instead of just sourcing this file?
# test_file("chapter_5/replication.R")

test_that("all models in Table 1 replicate correctly", {
  expect_equal(exp(coef(model1.1)),
               c(1.1717, 0.7357),
               tolerance=0.001, check.attributes=FALSE)
  expect_equal(exp(coef(model1.2)),
               c(1.0755, 0.7383, 1.0855, 1.0961, 2.1008, 0.94375),
               tolerance=0.001, check.attributes=FALSE)
  expect_equal(exp(coef(model1.3)),
               c(1.011469, 0.8305449, 1.06393, 1.102795, 1.358891,
                 1.06361, 1.100245, 1.132697, 1.184096),
               tolerance=0.001, check.attributes=FALSE)
})

test_that("all models in Table 2 replicate correctly", {
  expect_equal(exp(coef(model2.1)),
               c(0.0001443, 1.719672, 1.063219, 1.636929, 0.6071656,
                 2.60628, 1.188164, 0.5675854),
               tolerance=0.001, check.attributes=FALSE)
  expect_equal(exp(coef(model2.2)),
               c(0.0002061, 1.766067, 1.068525, 1.540463, 0.6428639,
                 2.636396, 1.194137, 0.6978541),
               tolerance=0.001, check.attributes=FALSE)
})

test_that("all models in Table 3 replicate correctly", {
  expect_equal(exp(coef(model3.1)),
               c(5.705571, 1.019324, 0.8882119, 4.576474, 
                 1.871581, 1.192389, 0.9561017, 1.146126, 0.9635761),
               tolerance=0.001, check.attributes=FALSE)
  
  expect_equal(exp(coef(model3.2)),
               c(3.608735, 1.019808, 0.8999499, 3.109966, 1.786648, 
                 1.154129, 0.9506058, 1.116099, 1.105339, 1.008152),
               tolerance=0.001, check.attributes=FALSE)
  
  expect_equal(exp(coef(model3.3)),
               c(3.897448, 1.015751, 0.8665679, 
                 4.047808, 1.926994, 1.191708, 0.9778078),
               tolerance=0.001, check.attributes=FALSE)
  
  expect_equal(exp(coef(model3.4)),
               c(2.207884, 1.015453, 0.8643506, 4.398909, 
                 1.888169, 1.202058, 0.9373359, 1.057324),
               tolerance=0.001, check.attributes=FALSE)
  
  expect_equal(exp(coef(model3.5)),
               c(3.437373, 1.016326, 0.843096, 4.453312, 
                 1.809711, 1.212441, 0.9880466, 1.011546),
               tolerance=0.001, check.attributes=FALSE)
})

test_that("all models in Table 4 replicate correctly", {
  expect_equal(exp(coef(model4.1)),
               c(4.574834, 2.517059, 7.324154, 10.57502, 1.021925, 0.7946485, 
                 4.31807, 1.848235, 1.142707, 1.029205, 1.116096, 0.9240479),
               tolerance=0.001, check.attributes=FALSE)
  
  expect_equal(exp(coef(model4.2)),
               c(2.627611, 1.654284, 4.587462, 8.235256, 1.022176, 0.8144571, 
                 3.741983, 1.964803, 1.044241, 0.9612961, 1.082427, 0.9690877, 
                 0.9048028, 1.100628),
               tolerance=0.001, check.attributes=FALSE)
  
  expect_equal(exp(coef(model4.3)),
               c(3.419952, 1.883609, 4.870449, 7.210562, 1.020228, 0.7955807, 
                 4.109518, 1.858802, 1.141449),
               tolerance=0.001, check.attributes=FALSE)
  
  expect_equal(exp(coef(model4.4)),
               c(3.331389, 2.127007, 1.676353, 1.25942, 1.020722, 0.8199253, 
                 4.755657, 1.642887, 1.194363),
               tolerance=0.001, check.attributes=FALSE)
})

# stata.check <- read_stata("~/Desktop/replication/table3_step_models.dta") %>% 
#   arrange(name, year)
# r.check <- df.survivalized1 %>% arrange(name, year)
# print(expect_equal(r.check$start_time2, 
#                    stata.check$`_t0`, tolerance=0.001,
#                    check.attributes = FALSE))
