
box::use(
  shiny[reactive],
  dplyr,
  data.table[setDT],
)


#' @export
banana <- readRDS(paste0("app/data/all_banana.rds"))

#' @export
flowering <- readRDS(paste0("app/data/all_flowering.rds")) |>
  dplyr::select(Location, everything())

#' @export
overall_banana <- readRDS(paste0("app/data/all_overall_banana.rds"))

#' @export
plantlets <- readRDS(paste0("app/data/all_plantlets.rds"))

#' #' @export
#' germinating_embryos <- readRDS(paste0("app/data/all_germination.rds"))

#' @export
subcultures <- readRDS(paste0("app/data/all_subcultures.rds"))

#' @export
rooting <- readRDS(paste0("app/data/all_rooting.rds"))

#' @export
weaning1 <- readRDS(paste0("app/data/all_weaning1.rds"))

#' @export
weaning2 <- readRDS(paste0("app/data/all_weaning2.rds"))

#' @export
screenhouse <- readRDS(paste0("app/data/all_screenhouse.rds"))

#' @export
hardening <- readRDS(paste0("app/data/all_hardening.rds"))

#' @export
openfield <- readRDS(paste0("app/data/all_openfield.rds"))

#' @export
crosses <- readRDS(paste0("app/data/all_crosses.rds"))

#' @export
bunches <- readRDS(paste0("app/data/all_bunches.rds"))

#' @export
extracted_seeds <- readRDS(paste0("app/data/all_extracted_seeds.rds"))

#' @export
embryo_rescued <- readRDS(paste0("app/data/all_embryo_rescue.rds"))

#' @export
germinating_embryos <- readRDS(paste0("app/data/all_germination.rds"))
