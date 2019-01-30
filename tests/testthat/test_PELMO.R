library(testthat)
library(pfm)
context("Create PELMO runs from psm files and execute them")
PELMO_base <- system.file("FOCUSPELMO.553", package = "PELMO.installeR")

test_archive <- system.file("testdata/FOCUS_PELMO.tar.bz2", package = "pfm")
test_dir <- tempdir()
untar(test_archive, exdir = test_dir, compressed = "bzip2")

runs <- list(
  list(
    psm = "Pesticide_D",
    fbe = c("Por"),
    vbe = c("Por")),
  list(
    psm = "Pesticide_D_1_day_pre_em_every_third_year",
    pot = c("Cha", "Ham")),
  list(
    psm = "Pesticide_D_1_May_every_other_year_mets",
    win = names(FOCUS_GW_scenarios_2012$names)))

# Check if we have wine on the path
wine_installed <- system('wine --version', ignore.stdout = TRUE) == 0
skip_if_no_wine <- function() {
  if (!wine_installed) skip("wine is not available")
}

test_that("PELMO paths are correctly created", {
  skip_if_no_wine()
  psm_paths = c(
     PELMO_path(runs[[1]]$psm, "fbe", "Por"),
     PELMO_path(runs[[2]]$psm, "pot", "Ham"),
     PELMO_path(runs[[3]]$psm, "win", "Cha"))

  for (i in seq_along(psm_paths)) {
    psm_file <- file.path(test_dir, psm_paths[i], paste0(runs[[i]]$psm, ".psm"))
    expect_true(file.exists(psm_file))
    psm_new_location <- file.path(PELMO_base, basename(psm_file))
    file.copy(psm_file, psm_new_location, overwrite = TRUE)
  }
})

test_that("PELMO runs are correctly set up", {
  skip_if_no_wine()

  # Prepare runs in analogy to the test archive
  skip_on_cran()
  PELMO_runs(runs, psm_dir = PELMO_base, execute = FALSE, evaluate = FALSE, overwrite = TRUE)

  # Check that input files are correctly generated in the right location
  for (run in runs) {
    psm <- run$psm
    crops <- setdiff(names(run), "psm")
    for (crop in crops) {
      for (scenario in run[[crop]]) {
        pp <- PELMO_path(psm, crop, scenario)

        input_new <- readLines(file.path(PELMO_base, "FOCUS", pp, "pelmo.inp"))
        input_test <- readLines(file.path(test_dir, pp, "pelmo.inp"))

        # Check if the input files are correctly reproduced
        expect_identical(input_new, input_test)
      }
    }
  }
})

test_that("PELMO runs can be run and give the expected result files", {
  skip_if_no_wine()
  skip_on_cran()

  run_PELMO(runs, cores = 7)

  plm_files <- c("CHEM.PLM", "ECHO.PLM",
                 "KONZCHEM.PLM", "KONZC_A1", "KONZC_B1",
                 "PLNTPEST.plm", "PLOT.PLM", "WASSER.PLM")

  # Check that if output is the same as in the test archive
  for (run in runs) {
    psm <- run$psm
    crops <- setdiff(names(run), "psm")
    for (crop in crops) {
      for (scenario in run[[crop]]) {
        pp <- PELMO_path(psm, crop, scenario)

        for (plm in plm_files) {
          if (file.exists(file.path(test_dir, pp, plm))) {
            new <- readLines(file.path(PELMO_base, "FOCUS", pp, plm))
            test <- readLines(file.path(test_dir, pp, plm))

            # Don't check for differences in the PESTICIDE BALANCE ERROR
            pest_balance_error <- suppressWarnings(grep("PESTICIDE BALANCE ERROR", new))
            # Suppress warnings about invalid strings in this locale caused by the files

            # Check if the ouput files are correctly reproduced
            expect_identical(new[!pest_balance_error], test[!pest_balance_error])
          }
        }
      }
    }
  }
})

test_that("PELMO runs are correctly evaluated", {
  skip_if_no_wine()
  skip_on_cran()

  pfm_PECgw <- evaluate_PELMO(runs)

  # Check that if output is the same as in the test archive
  for (run in runs) {
    psm <- run$psm
    crops <- setdiff(names(run), "psm")
    for (crop in crops) {
      for (scenario in run[[crop]]) {
        pp <- PELMO_path(psm, crop, scenario)

        period_file <- readLines(file.path(test_dir, pp, "period.plm"), encoding = "latin1")

        result_lines <- grep("^\tResults for.*in the percolate at 1 m soil depth$", period_file)
        acronyms <- gsub(".*\\((.*)\\).*", "\\1", period_file[result_lines])
        names(result_lines) <- acronyms

        results <- list()
        for (acronym in acronyms) {
          results[[acronym]] <- list()
          conc_lines <- result_lines[acronym] + 5:24
          tmp <- read.table(text = period_file[conc_lines], sep = "\t")
          results[[acronym]]$periods <- data.frame(
            period = as.integer(tmp$V2),
            flux = tmp$V3,
            percolate = tmp$V4,
            conc = tmp$V5)
          tmp80 <- read.table(text = period_file[result_lines[acronym] + 27], sep = "\t")
          results[[acronym]]$focus <- tmp80[[1, "V5"]]
        }

        period_pfm_file <- file.path(PELMO_base, "FOCUS", pp, "period_pfm.rda")
        load(period_pfm_file)

        #message(psm, " ", crop, " ", scenario)

        # Test for equality of all the components separately,
        # as we need to adapt the tolerance
        for (acronym in acronyms) {
          p_pelmo <- results[[acronym]]$periods
          p_test <- results_pfm[[acronym]]$periods
          expect_equal(p_test$flux, p_pelmo$flux, tol = 1e-6)
          expect_equal(p_test$percolate, p_pelmo$percolate)
          # PELMO sets the concentration to 0 when the percolate is zero.
          # We get NaN, which is more reasonable, but we need to
          # take this into account for testing
          p_test$conc <- ifelse(is.na(p_test$conc), 0, p_test$conc)
          expect_equal(p_test$conc, p_pelmo$conc, tol = 1e-3, scale = 1)

          # FOCUS PEC
          expect_equal(results_pfm[[acronym]]$focus, results[[acronym]]$focus,
                       tol = 1e-3, scale = 1)
        }
      }
    }
  }
})

test_that("PECgw from FOCUS summary files can be reproduced", {
  skip_if_no_wine()
  skip_on_cran()

  pfm_PECgw <- evaluate_PELMO(runs)
  
  focus_summary <- list()

  for (run in runs) {
    psm <- run$psm
    focus_summary[[psm]] <- list()

    crops <- setdiff(names(run), "psm")
    for (crop in crops) {
      scenarios <- run[[crop]]

      # Read contents of summary text file copied from the GUI output. We only
      # have results for one crop per psm file, so the crop is not in the file
      # name.
      sumfile_path <- system.file(paste0("testdata/FOCUS_Summary_", psm,
                                         ".txt"), package = "pfm")
      sumfile <- readLines(sumfile_path, encoding = "latin1")
      result_anchors <- grep("Results for", sumfile)
      acronyms <- gsub(".*\\((.*)\\).*", "\\1", sumfile[result_anchors])
      names(result_anchors) <- acronyms
      focus_summary[[psm]][[crop]] <- matrix(nrow = length(scenarios), ncol = length(acronyms),
                                             dimnames = list(scenarios, acronyms))
      for (acronym in acronyms) {
        tmp <- sumfile[result_anchors[acronym] + 4 + (1:length(scenarios))]
        tmp_frame <- read.table(text = tmp, sep = "\t")
        PECgw <- tmp_frame$V5
        focus_summary[[psm]][[crop]][, acronym] <- PECgw
      }
    }
  }
  expect_equal(pfm_PECgw, focus_summary)
})
