context("-- cytoset_factors")

test_that("cytoset_factors class and methods exist", {
  # Test class definitions
  expect_true(isClass("cytoset_factors"))
  expect_true(isClass("GatingSet_factors"))
  
  # Test method definitions
  expect_true(existsMethod("pData", "cytoset_factors"))
  expect_true(existsMethod("pData<-", signature=c("cytoset_factors", "data.frame")))
  expect_true(existsMethod("phenoData", "cytoset_factors"))
})

test_that("flowSet preserves factors in pData (baseline)", {
  # Verify our assumption: flowSet already preserves factors
  fs <- GvHD[1:3]
  pd <- pData(fs)
  
  # Add factor columns with specific levels
  pd$Patient <- factor(c("A", "B", "C"), levels = c("C", "B", "A"))
  pd$Visit <- factor(c("V1", "V2", "V3"), levels = c("V3", "V2", "V1"))
  pd$Grade_char <- c("Low", "Med", "High")
  
  # Assign and retrieve
  pData(fs) <- pd
  pd_retrieved <- pData(fs)
  
  # Verify factors are preserved
  expect_true(is.factor(pd_retrieved$Patient))
  expect_true(is.factor(pd_retrieved$Visit))
  expect_equal(levels(pd_retrieved$Patient), c("C", "B", "A"))
  expect_equal(levels(pd_retrieved$Visit), c("V3", "V2", "V1"))
  
  # Verify non-factors stay as-is
  expect_true(is.character(pd_retrieved$Grade_char))
})

test_that("cytoset does NOT preserve factors (current behavior)", {
  # Load test data
  fcs_files <- list.files(dataDir, "Cyto", full.names = TRUE)
  if(length(fcs_files) == 0) {
    skip("No test FCS files available")
  }
  
  cs <- load_cytoset_from_fcs(fcs_files[1:min(2, length(fcs_files))])
  pd <- pData(cs)
  
  # Add factor columns
  if(nrow(pd) > 0) {
    pd$Patient <- factor(rep("A", nrow(pd)), levels = c("C", "B", "A"))
    pd$Visit <- factor(rep("V1", nrow(pd)), levels = c("V3", "V2", "V1"))
    
    # Assign and retrieve
    pData(cs) <- pd
    pd_retrieved <- pData(cs)
    
    # Verify factors are lost (converted to character)
    expect_false(is.factor(pd_retrieved$Patient))
    expect_false(is.factor(pd_retrieved$Visit))
    expect_true(is.character(pd_retrieved$Patient))
  }
})

test_that("cytoset_factors constructor works", {
  fcs_files <- list.files(dataDir, "Cyto", full.names = TRUE)
  if(length(fcs_files) == 0) {
    skip("No test FCS files available")
  }
  
  cs <- load_cytoset_from_fcs(fcs_files[1])
  cs_f <- cytoset_factors(cs)
  
  expect_is(cs_f, "cytoset_factors")
  expect_is(cs_f, "cytoset")
  expect_is(cs_f, "flowSet")
  
  # Should have same dimensions
  expect_equal(length(cs_f), length(cs))
  expect_equal(sampleNames(cs_f), sampleNames(cs))
})

test_that("cytoset_factors preserves factor levels in pData", {
  fcs_files <- list.files(dataDir, "Cyto", full.names = TRUE)
  if(length(fcs_files) == 0) {
    skip("No test FCS files available")
  }
  
  cs <- load_cytoset_from_fcs(fcs_files[1:min(3, length(fcs_files))])
  cs_f <- cytoset_factors(cs)
  
  pd <- pData(cs_f)
  n_samples <- nrow(pd)
  
  # Add factor columns with specific levels
  pd$Patient <- factor(rep(c("A", "B", "C"), length.out = n_samples), 
                       levels = c("C", "B", "A"))
  pd$Visit <- factor(rep(c("V1", "V2", "V3"), length.out = n_samples),
                     levels = c("V3", "V2", "V1"))
  pd$Status <- factor(rep(c("Pos", "Neg"), length.out = n_samples),
                      levels = c("Neg", "Pos", "Unknown"))
  pd$Grade_char <- rep(c("Low", "High"), length.out = n_samples)
  
  # Assign and retrieve
  pData(cs_f) <- pd
  pd_retrieved <- pData(cs_f)
  
  # Verify factors are preserved with correct levels
  expect_true(is.factor(pd_retrieved$Patient))
  expect_true(is.factor(pd_retrieved$Visit))
  expect_true(is.factor(pd_retrieved$Status))
  expect_equal(levels(pd_retrieved$Patient), c("C", "B", "A"))
  expect_equal(levels(pd_retrieved$Visit), c("V3", "V2", "V1"))
  expect_equal(levels(pd_retrieved$Status), c("Neg", "Pos", "Unknown"))
  
  # Verify non-factors stay as character
  expect_true(is.character(pd_retrieved$Grade_char))
  
  # Verify factor values are correct
  expect_equal(as.character(pd_retrieved$Patient), 
               rep(c("A", "B", "C"), length.out = n_samples))
  expect_equal(as.character(pd_retrieved$Visit),
               rep(c("V1", "V2", "V3"), length.out = n_samples))
})

test_that("cytoset_factors phenoData method works", {
  fcs_files <- list.files(dataDir, "Cyto", full.names = TRUE)
  if(length(fcs_files) == 0) {
    skip("No test FCS files available")
  }
  
  cs <- load_cytoset_from_fcs(fcs_files[1])
  cs_f <- cytoset_factors(cs)
  
  # Get phenoData
  pheno <- phenoData(cs_f)
  expect_is(pheno, "AnnotatedDataFrame")
  
  # Add factor through pData
  pd <- pData(cs_f)
  pd$Treatment <- factor("Control", levels = c("Control", "Treated"))
  pData(cs_f) <- pd
  
  # Retrieve through phenoData
  pheno <- phenoData(cs_f)
  pd_from_pheno <- pData(pheno)
  
  expect_true(is.factor(pd_from_pheno$Treatment))
  expect_equal(levels(pd_from_pheno$Treatment), c("Control", "Treated"))
})

test_that("cytoset_factors preserves factors through multiple updates", {
  fcs_files <- list.files(dataDir, "Cyto", full.names = TRUE)
  if(length(fcs_files) == 0) {
    skip("No test FCS files available")
  }
  
  cs <- load_cytoset_from_fcs(fcs_files[1:min(2, length(fcs_files))])
  cs_f <- cytoset_factors(cs)
  
  # First update: add Patient factor
  pd <- pData(cs_f)
  pd$Patient <- factor(rep("A", nrow(pd)), levels = c("C", "B", "A"))
  pData(cs_f) <- pd
  
  # Second update: add Visit factor, Patient should still be preserved
  pd <- pData(cs_f)
  pd$Visit <- factor(rep("V1", nrow(pd)), levels = c("V3", "V2", "V1"))
  pData(cs_f) <- pd
  
  # Verify both factors are preserved
  pd_final <- pData(cs_f)
  expect_true(is.factor(pd_final$Patient))
  expect_true(is.factor(pd_final$Visit))
  expect_equal(levels(pd_final$Patient), c("C", "B", "A"))
  expect_equal(levels(pd_final$Visit), c("V3", "V2", "V1"))
})

test_that("GatingSet_factors constructor works", {
  fcs_files <- list.files(dataDir, "Cyto", full.names = TRUE)
  if(length(fcs_files) == 0) {
    skip("No test FCS files available")
  }
  
  # Create a simple GatingSet
  cs <- load_cytoset_from_fcs(fcs_files[1:min(2, length(fcs_files))])
  gs <- GatingSet(cs)
  
  # Convert to GatingSet_factors
  gs_f <- GatingSet_factors(gs)
  
  expect_is(gs_f, "GatingSet_factors")
  expect_is(gs_f, "GatingSet")
  
  # Should have same dimensions
  expect_equal(length(gs_f), length(gs))
  expect_equal(sampleNames(gs_f), sampleNames(gs))
})

test_that("GatingSet_factors preserves factors in pData", {
  fcs_files <- list.files(dataDir, "Cyto", full.names = TRUE)
  if(length(fcs_files) == 0) {
    skip("No test FCS files available")
  }
  
  # Create a simple GatingSet
  cs <- load_cytoset_from_fcs(fcs_files[1:min(3, length(fcs_files))])
  gs <- GatingSet(cs)
  gs_f <- GatingSet_factors(gs)
  
  # Add factor columns
  pd <- pData(gs_f)
  n_samples <- nrow(pd)
  pd$Patient <- factor(rep(c("A", "B", "C"), length.out = n_samples),
                       levels = c("C", "B", "A"))
  pd$Visit <- factor(rep(c("V1", "V2"), length.out = n_samples),
                     levels = c("V2", "V1"))
  
  # Assign and retrieve
  pData(gs_f) <- pd
  pd_retrieved <- pData(gs_f)
  
  # Verify factors are preserved
  expect_true(is.factor(pd_retrieved$Patient))
  expect_true(is.factor(pd_retrieved$Visit))
  expect_equal(levels(pd_retrieved$Patient), c("C", "B", "A"))
  expect_equal(levels(pd_retrieved$Visit), c("V2", "V1"))
})

test_that("GatingSet_factors works with inherited methods", {
  fcs_files <- list.files(dataDir, "Cyto", full.names = TRUE)
  if(length(fcs_files) == 0) {
    skip("No test FCS files available")
  }
  
  # Create GatingSet_factors
  cs <- load_cytoset_from_fcs(fcs_files[1:min(2, length(fcs_files))])
  gs <- GatingSet(cs)
  gs_f <- GatingSet_factors(gs)
  
  # Test that standard GatingSet methods work
  expect_equal(length(gs_f), length(gs))
  expect_equal(sampleNames(gs_f), sampleNames(gs))
  
  # phenoData should work
  pheno <- phenoData(gs_f)
  expect_is(pheno, "AnnotatedDataFrame")
  
  # Should be able to access underlying cytoset
  cs_f <- gs_cyto_data(gs_f)
  expect_is(cs_f, "cytoset_factors")
})

test_that("factor preservation with missing levels", {
  fcs_files <- list.files(dataDir, "Cyto", full.names = TRUE)
  if(length(fcs_files) == 0) {
    skip("No test FCS files available")
  }
  
  cs <- load_cytoset_from_fcs(fcs_files[1:min(2, length(fcs_files))])
  cs_f <- cytoset_factors(cs)
  
  # Create factor with unused level
  pd <- pData(cs_f)
  pd$Status <- factor(rep("Present", nrow(pd)), 
                      levels = c("Absent", "Present", "Unknown"))
  pData(cs_f) <- pd
  
  # Retrieve and verify all levels preserved (including unused one)
  pd_retrieved <- pData(cs_f)
  expect_true(is.factor(pd_retrieved$Status))
  expect_equal(levels(pd_retrieved$Status), c("Absent", "Present", "Unknown"))
  expect_equal(as.character(pd_retrieved$Status), rep("Present", nrow(pd)))
})

test_that("factor vs character handling", {
  fcs_files <- list.files(dataDir, "Cyto", full.names = TRUE)
  if(length(fcs_files) == 0) {
    skip("No test FCS files available")
  }
  
  cs <- load_cytoset_from_fcs(fcs_files[1:min(2, length(fcs_files))])
  cs_f <- cytoset_factors(cs)
  
  pd <- pData(cs_f)
  # Mix of factor and character columns
  pd$FactorCol <- factor(c("A", "B")[1:nrow(pd)], levels = c("B", "A", "C"))
  pd$CharCol <- c("X", "Y")[1:nrow(pd)]
  pd$IntCol <- 1:nrow(pd)
  
  pData(cs_f) <- pd
  pd_retrieved <- pData(cs_f)
  
  # Verify types are preserved correctly
  expect_true(is.factor(pd_retrieved$FactorCol))
  expect_true(is.character(pd_retrieved$CharCol))
  expect_true(is.integer(pd_retrieved$IntCol))
  
  # Verify factor details
  expect_equal(levels(pd_retrieved$FactorCol), c("B", "A", "C"))
})
