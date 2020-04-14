context("test gate transform methods")
skip_if(win32_flag)

gs <- load_gs(file.path(system.file("extdata", package="flowWorkspaceData"),"gs_bcell_auto"))
gh <- gs[[1]]

test_ellipsoidGate_node <- "/boundary/nonDebris/lymph"
test_polygonGate_node <- "/boundary/nonDebris/lymph/Live/CD19andCD20/Transitional"
test_rectangleGate_node <- "/boundary/nonDebris/lymph/Live/CD19andCD20/IgD-CD27+"

# Just some factors used for testing
test_scale_uniform <- 4
test_scale_split <- c(2,7)
tol <- 1e-10
dx1 <- 42
dy1 <- 105
dx2 <- -17
dy2 <- 74
new_center <- c(932, 1437)
new_center2 <- c(841, -35)
test_angle <- 117
test_angle2 <- 63

# There are flowCore tests for the logic of the gate transformations themselves. 
# These tests are just to to make sure the wrappers are behaving as expected.

# Test GatingHierarchy methods

test_that("scale polygonGate_gh", {
  test_polygonGate <- gh_pop_get_gate(gh, test_polygonGate_node)
  scale_gate(gh, test_polygonGate_node, test_scale_split)
  scaled_gate <- scale_gate(test_polygonGate, test_scale_split)
  expect_equal(scaled_gate, gh_pop_get_gate(gh, test_polygonGate_node))
})

test_that("scale ellipsoidGate_gh", {
  test_ellipsoidGate <- gh_pop_get_gate(gh, test_ellipsoidGate_node)
  scale_gate(gh, test_ellipsoidGate_node, test_scale_split)
  scaled_gate <- scale_gate(test_ellipsoidGate, test_scale_split)
  expect_equal(scaled_gate, gh_pop_get_gate(gh, test_ellipsoidGate_node))
})

test_that("scale rectangleGate_gh", {
  test_rectangleGate <- gh_pop_get_gate(gh, test_rectangleGate_node)
  expect_error(scale_gate(gh, test_rectangleGate_node, test_scale_split))
  # For the sake of actually testing scale_gate, need to get
  # rid ofthe infinite bounds
  test_rectangleGate@min[1] <- 0
  test_rectangleGate@max[2] <- 3000
  gh_pop_set_gate(gs[[1]], test_rectangleGate_node, test_rectangleGate)
  gh_pop_set_gate(gs[[2]], test_rectangleGate_node, test_rectangleGate)
  
  test_rectangleGate <- gh_pop_get_gate(gh, test_rectangleGate_node)
  scale_gate(gh, test_rectangleGate_node, test_scale_split)
  scaled_gate <- scale_gate(test_rectangleGate, test_scale_split)
  expect_equal(scaled_gate, gh_pop_get_gate(gh, test_rectangleGate_node))
})

test_that("shift polygonGate_gh", {
  test_polygonGate <- gh_pop_get_gate(gh, test_polygonGate_node)
  shift_gate(gh, test_polygonGate_node, c(dx1, dy1))
  shifted_gate <- shift_gate(test_polygonGate, c(dx1, dy1))
  expect_equal(shifted_gate, gh_pop_get_gate(gh, test_polygonGate_node))
})

test_that("shift ellipsoidGate_gh", {
  test_ellipsoidGate <- gh_pop_get_gate(gh, test_ellipsoidGate_node)
  shift_gate(gh, test_ellipsoidGate_node, c(dx1, dy1))
  shifted_gate <- shift_gate(test_ellipsoidGate, c(dx1, dy1))
  expect_equal(shifted_gate, gh_pop_get_gate(gh, test_ellipsoidGate_node))
})

test_that("shift rectangleGate_gh", {
  test_rectangleGate <- gh_pop_get_gate(gh, test_rectangleGate_node)
  shift_gate(gh, test_rectangleGate_node, c(dx1, dy1))
  shifted_gate <- shift_gate(test_rectangleGate, c(dx1, dy1))
  expect_equal(shifted_gate, gh_pop_get_gate(gh, test_rectangleGate_node))
})

test_that("rotate polygonGate_gh", {
  test_polygonGate <- gh_pop_get_gate(gh, test_polygonGate_node)
  rotate_gate(gh, test_polygonGate_node, deg = test_angle)
  rotated_gate <- rotate_gate(test_polygonGate, deg = test_angle)
  expect_equal(rotated_gate, gh_pop_get_gate(gh, test_polygonGate_node))
})

test_that("rotate ellipsoidGate_gh", {
  test_ellipsoidGate <- gh_pop_get_gate(gh, test_ellipsoidGate_node)
  rotate_gate(gh, test_ellipsoidGate_node, deg = test_angle)
  rotated_gate <- rotate_gate(test_ellipsoidGate, deg = test_angle)
  expect_equal(rotated_gate, gh_pop_get_gate(gh, test_ellipsoidGate_node))
})

test_that("rotate rectangleGate_gh", {
  expect_error(rotate_gate(gh, test_rectangleGate_node, deg = test_angle))
})

test_that("transform polygonGate_gh", {
  test_polygonGate <- gh_pop_get_gate(gh, test_polygonGate_node)
  new_polygon <- sweep(test_polygonGate@boundaries, 2, -c(dx1, dy1))
  transform_gate(gh, test_polygonGate_node, scale = test_scale_split, boundaries = new_polygon, 
                                dx = c(dx2, dy2), deg = test_angle)
  transformed_gate <- transform_gate(test_polygonGate, scale = test_scale_split, boundaries = new_polygon, 
                                     dx = c(dx2, dy2), deg = test_angle)
  expect_equal(transformed_gate, gh_pop_get_gate(gh, test_polygonGate_node))
})

test_that("transform ellipsoidGate_gh", {
  test_ellipsoidGate <- gh_pop_get_gate(gh, test_ellipsoidGate_node)
  rad <- test_angle2*(pi/180)
  rot <- rbind(c(cos(rad), -sin(rad)), c(sin(rad), cos(rad)))
  new_cov <- rot%*%(test_ellipsoidGate@cov)%*%t(rot)
  transform_gate(gh, test_ellipsoidGate_node, scale = test_scale_split, mean = new_center,
                 dx = c(dx2, dy2), cov = new_cov, deg = test_angle)
  transformed_gate <- transform_gate(test_ellipsoidGate, scale = test_scale_split, mean = new_center,
                                     dx = c(dx2, dy2), cov = new_cov, deg = test_angle)
  expect_equal(transformed_gate, gh_pop_get_gate(gh, test_ellipsoidGate_node))
})

test_that("transform rectangleGate_gh", {
  test_rectangleGate <- gh_pop_get_gate(gh, test_rectangleGate_node)
  new_min <- test_rectangleGate@min + c(dx1, dy1)
  new_max <- test_rectangleGate@max + c(dx1, dy1)
  transform_gate(gh, test_rectangleGate_node, scale = test_scale_split, min = new_min, 
                 dx = c(dx2, dy2), max = new_max)
  transformed_gate <- transform_gate(test_rectangleGate, scale = test_scale_split, min = new_min, 
                                     dx = c(dx2, dy2), max = new_max)
  expect_equal(transformed_gate, gh_pop_get_gate(gh, test_rectangleGate_node))
})


## GatingSet methods

test_that("scale polygonGate_gs", {
  test_polygonGate <- gs_pop_get_gate(gs, test_polygonGate_node)
  scale_gate(gs, test_polygonGate_node, test_scale_split)
  scaled_gate1 <- scale_gate(test_polygonGate[[1]], test_scale_split)
  scaled_gate2 <- scale_gate(test_polygonGate[[2]], test_scale_split)
  expect_equal(scaled_gate1, gs_pop_get_gate(gs, test_polygonGate_node)[[1]])
  expect_equal(scaled_gate2, gs_pop_get_gate(gs, test_polygonGate_node)[[2]])
})

test_that("scale ellipsoidGate_gs", {
  test_ellipsoidGate <- gs_pop_get_gate(gs, test_ellipsoidGate_node)
  scale_gate(gs, test_ellipsoidGate_node, test_scale_split)
  scaled_gate1 <- scale_gate(test_ellipsoidGate[[1]], test_scale_split)
  scaled_gate2 <- scale_gate(test_ellipsoidGate[[2]], test_scale_split)
  expect_equal(scaled_gate1, gs_pop_get_gate(gs, test_ellipsoidGate_node)[[1]])
  expect_equal(scaled_gate2, gs_pop_get_gate(gs, test_ellipsoidGate_node)[[2]])
})

test_that("scale rectangleGate_gs", {
  test_rectangleGate <- gs_pop_get_gate(gs, test_rectangleGate_node)
  test_rectangleGate[[1]]@min[1] <- 0
  test_rectangleGate[[1]]@max[2] <- 3000
  test_rectangleGate[[2]]@min[1] <- 0
  test_rectangleGate[[2]]@max[2] <- 3000
  gh_pop_set_gate(gs[[1]], test_rectangleGate_node, test_rectangleGate[[1]])
  gh_pop_set_gate(gs[[2]], test_rectangleGate_node, test_rectangleGate[[2]])
  
  test_rectangleGate <- gs_pop_get_gate(gs, test_rectangleGate_node)
  scale_gate(gs, test_rectangleGate_node, test_scale_split)
  scaled_gate1 <- scale_gate(test_rectangleGate[[1]], test_scale_split)
  scaled_gate2 <- scale_gate(test_rectangleGate[[2]], test_scale_split)
  expect_equal(scaled_gate1, gs_pop_get_gate(gs, test_rectangleGate_node)[[1]])
  expect_equal(scaled_gate2, gs_pop_get_gate(gs, test_rectangleGate_node)[[2]])
})

test_that("shift polygonGate_gs", {
  test_polygonGate <- gs_pop_get_gate(gs, test_polygonGate_node)
  shift_gate(gs, test_polygonGate_node, c(dx1, dy1))
  shifted_gate1 <- shift_gate(test_polygonGate[[1]], c(dx1, dy1))
  shifted_gate2 <- shift_gate(test_polygonGate[[2]], c(dx1, dy1))
  expect_equal(shifted_gate1, gs_pop_get_gate(gs, test_polygonGate_node)[[1]])
  expect_equal(shifted_gate2, gs_pop_get_gate(gs, test_polygonGate_node)[[2]])
})

test_that("shift ellipsoidGate_gs", {
  test_ellipsoidGate <- gs_pop_get_gate(gs, test_ellipsoidGate_node)
  shift_gate(gs, test_ellipsoidGate_node, c(dx1, dy1))
  shifted_gate1 <- shift_gate(test_ellipsoidGate[[1]], c(dx1, dy1))
  shifted_gate2 <- shift_gate(test_ellipsoidGate[[2]], c(dx1, dy1))
  expect_equal(shifted_gate1, gs_pop_get_gate(gs, test_ellipsoidGate_node)[[1]])
  expect_equal(shifted_gate2, gs_pop_get_gate(gs, test_ellipsoidGate_node)[[2]])
})

test_that("shift rectangleGate_gs", {
  test_rectangleGate <- gs_pop_get_gate(gs, test_rectangleGate_node)
  shift_gate(gs, test_rectangleGate_node, c(dx1, dy1))
  shifted_gate1 <- shift_gate(test_rectangleGate[[1]], c(dx1, dy1))
  shifted_gate2 <- shift_gate(test_rectangleGate[[2]], c(dx1, dy1))
  expect_equal(shifted_gate1, gs_pop_get_gate(gs, test_rectangleGate_node)[[1]])
  expect_equal(shifted_gate2, gs_pop_get_gate(gs, test_rectangleGate_node)[[2]])
})

test_that("rotate polygonGate_gs", {
  test_polygonGate <- gs_pop_get_gate(gs, test_polygonGate_node)
  rotate_gate(gs, test_polygonGate_node, deg = test_angle)
  rotated_gate1 <- rotate_gate(test_polygonGate[[1]], deg = test_angle)
  rotated_gate2 <- rotate_gate(test_polygonGate[[2]], deg = test_angle)
  expect_equal(rotated_gate1, gs_pop_get_gate(gs, test_polygonGate_node)[[1]])
  expect_equal(rotated_gate2, gs_pop_get_gate(gs, test_polygonGate_node)[[2]])
})

test_that("rotate ellipsoidGate_gs", {
  test_ellipsoidGate <- gs_pop_get_gate(gs, test_ellipsoidGate_node)
  rotate_gate(gs, test_ellipsoidGate_node, deg = test_angle)
  rotated_gate1 <- rotate_gate(test_ellipsoidGate[[1]], deg = test_angle)
  rotated_gate2 <- rotate_gate(test_ellipsoidGate[[2]], deg = test_angle)
  expect_equal(rotated_gate1, gs_pop_get_gate(gs, test_ellipsoidGate_node)[[1]])
  expect_equal(rotated_gate2, gs_pop_get_gate(gs, test_ellipsoidGate_node)[[2]])
})

test_that("rotate rectangleGate_gs", {
  expect_error(rotate_gate(gs, test_rectangleGate_node, deg = test_angle))
})

test_that("transform polygonGate_gs", {
  test_polygonGate <- gs_pop_get_gate(gs, test_polygonGate_node)
  new_polygon <- sweep(test_polygonGate[[1]]@boundaries, 2, -c(dx1, dy1))
  transform_gate(gs, test_polygonGate_node, scale = test_scale_split, boundaries = new_polygon, 
                 dx = c(dx2, dy2), deg = test_angle)
  transformed_gate1 <- transform_gate(test_polygonGate[[1]], scale = test_scale_split, boundaries = new_polygon, 
                                     dx = c(dx2, dy2), deg = test_angle)
  transformed_gate2 <- transform_gate(test_polygonGate[[2]], scale = test_scale_split, boundaries = new_polygon, 
                                      dx = c(dx2, dy2), deg = test_angle)
  expect_equal(transformed_gate1, gs_pop_get_gate(gs, test_polygonGate_node)[[1]])
  expect_equal(transformed_gate2, gs_pop_get_gate(gs, test_polygonGate_node)[[2]])
})

test_that("transform ellipsoidGate_gs", {
  test_ellipsoidGate <- gs_pop_get_gate(gs, test_ellipsoidGate_node)
  rad <- test_angle2*(pi/180)
  rot <- rbind(c(cos(rad), -sin(rad)), c(sin(rad), cos(rad)))
  new_cov <- rot%*%(test_ellipsoidGate[[1]]@cov)%*%t(rot)
  transform_gate(gs, test_ellipsoidGate_node, scale = test_scale_split, mean = new_center,
                 dx = c(dx2, dy2), cov = new_cov, deg = test_angle)
  transformed_gate1 <- transform_gate(test_ellipsoidGate[[1]], scale = test_scale_split, mean = new_center,
                                     dx = c(dx2, dy2), cov = new_cov, deg = test_angle)
  transformed_gate2 <- transform_gate(test_ellipsoidGate[[2]], scale = test_scale_split, mean = new_center,
                                      dx = c(dx2, dy2), cov = new_cov, deg = test_angle)
  expect_equal(transformed_gate1, gs_pop_get_gate(gs, test_ellipsoidGate_node)[[1]])
  expect_equal(transformed_gate2, gs_pop_get_gate(gs, test_ellipsoidGate_node)[[2]])
})

test_that("transform rectangleGate_gs", {
  test_rectangleGate <- gs_pop_get_gate(gs, test_rectangleGate_node)
  new_min <- test_rectangleGate[[1]]@min + c(dx1, dy1)
  new_max <- test_rectangleGate[[1]]@max + c(dx1, dy1)
  transform_gate(gs, test_rectangleGate_node, scale = test_scale_split, min = new_min, 
                 dx = c(dx2, dy2), max = new_max)
  transformed_gate1 <- transform_gate(test_rectangleGate[[1]], scale = test_scale_split, min = new_min, 
                                     dx = c(dx2, dy2), max = new_max)
  transformed_gate2 <- transform_gate(test_rectangleGate[[2]], scale = test_scale_split, min = new_min, 
                                      dx = c(dx2, dy2), max = new_max)
  expect_equal(transformed_gate1, gs_pop_get_gate(gs, test_rectangleGate_node)[[1]])
  expect_equal(transformed_gate2, gs_pop_get_gate(gs, test_rectangleGate_node)[[2]])
})
