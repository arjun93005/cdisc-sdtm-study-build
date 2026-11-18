# Unit tests for R/utils/sdtm_utils.R  (run: Rscript tests/run_tests.R)

test_that("raw_to_iso converts complete EDC dates and appends valid times", {
  expect_equal(raw_to_iso("04-MAR-2025"), "2025-03-04")
  expect_equal(raw_to_iso("4-mar-2025"), "2025-03-04")
  expect_equal(raw_to_iso("04-MAR-2025", "9:05"), "2025-03-04T09:05")
  expect_equal(raw_to_iso("04-MAR-2025", "25:00"), "2025-03-04")      # invalid time dropped
})

test_that("raw_to_iso truncates partial dates to known precision (SDTMIG 4.4)", {
  expect_equal(raw_to_iso("UN-MAR-2025"), "2025-03")
  expect_equal(raw_to_iso("UN-UNK-1968"), "1968")
  expect_equal(raw_to_iso("UN-MAR-2025", "10:00"), "2025-03")         # no time on partial date
})

test_that("raw_to_iso rejects impossible or malformed dates", {
  expect_true(is.na(raw_to_iso("31-FEB-2025")))
  expect_true(is.na(raw_to_iso("04-XYZ-2025")))
  expect_true(is.na(raw_to_iso("2025-03-04")))
  expect_true(is.na(raw_to_iso(NA)))
  expect_true(is.na(raw_to_iso("")))
})

test_that("study_day has no day 0 and needs complete dates", {
  expect_equal(study_day("2025-03-04", "2025-03-04T09:30"), 1L)
  expect_equal(study_day("2025-03-05T08:00", "2025-03-04"), 2L)
  expect_equal(study_day("2025-03-03", "2025-03-04"), -1L)
  expect_true(is.na(study_day("2025-03", "2025-03-04")))
  expect_true(is.na(study_day("2025-03-04", NA)))
})

test_that("derive_epoch respects first-dose time, pre-dose flag and last dose", {
  dm <- data.frame(USUBJID = c("S1", "SF"), RFXSTDTC = c("2025-03-04T09:30", NA), RFXENDTC = c("2025-05-27", NA))
  id <- c("S1", "S1", "S1", "S1", "S1", "S1", "SF")
  dtc <- c("2025-03-01", "2025-03-04T08:45", "2025-03-04T10:00", "2025-03-04", "2025-05-27", "2025-05-28", "2025-02-20")
  expect_equal(derive_epoch(id, dtc, dm),
               c("SCREENING", "SCREENING", "TREATMENT", "TREATMENT", "TREATMENT", "FOLLOW-UP", "SCREENING"))
  expect_equal(derive_epoch("S1", "2025-03-04", dm, predose = TRUE), "SCREENING")
  expect_true(is.na(derive_epoch("S1", "2025-03", dm)))
})

test_that("ct_map maps case-insensitively and fails loudly on unmapped values", {
  expect_equal(ct_map(c("Male", "female", NA), c(Male = "M", Female = "F")), c("M", "F", NA))
  expect_error(ct_map("Unknown", c(Male = "M")), "Unmapped")
})

test_that("derive_lobxfl flags the last non-missing pre-dose record per test", {
  dm <- data.frame(USUBJID = "S1", RFXSTDTC = "2025-03-04T09:30")
  vs <- data.frame(USUBJID = "S1", VSTESTCD = c("SYSBP", "SYSBP", "SYSBP", "SYSBP"),
                   VSORRES = c("150", "148", NA, "140"), VSREPNUM = c(1, 2, 3, 1),
                   VSDTC = c("2025-03-04T08:45", "2025-03-04T08:45", "2025-03-04T08:45", "2025-03-18T09:00"))
  out <- derive_lobxfl(vs, "VS", dm)
  expect_equal(out$VSLOBXFL, c(NA, "Y", NA, NA))
})
