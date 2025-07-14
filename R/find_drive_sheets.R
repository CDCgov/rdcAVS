
#' Obtain spreadsheets within a Drive folder
#'
#' @description
#' Obtains spreadsheets within a folder, which may or may not contain subfolders.
#'
#' @param folder_dribble `dribble` Dribble containing the folder to search to.
#'
#' @returns `dribble` Dribble containing the Google Sheets dribbles.
#' @export
#'
#' @examples
#' \dontrun{
#' folder <- googledrive::drive_get("folder_id")
#' sheets <- find_drive_sheets(folder)
#' }
find_drive_sheets <- function(folder_dribble) {
  sub_folders <- googledrive::drive_ls(folder_dribble)
  sub_folders <- googledrive::drive_reveal(sub_folders, "mimeType")

  sub_folders <- sub_folders |>
    dplyr::filter(mime_type == "application/vnd.google-apps.folder")
  spreadsheets_top_level <- sub_folders |>
    dplyr::filter(mime_type == "application/vnd.google-apps.spreadsheet")

  if (nrow(sub_folders) > 0) {
    doFuture::registerDoFuture()

    if (stringr::str_starts(Sys.getenv("SF_PARTNER"), "posit_workbench")) {
      future::plan(future::multicore)
    } else {
      future::plan(future::multisession)
    }

    options(doFuture.rng.onMisuse = "ignore")
    xs <- 1:nrow(sub_folders)

    progressr::handlers("cli")
    progressr::with_progress({
      p <- progressr::progressor(along = xs)
      y <-
        foreach::`%dopar%`(foreach::foreach(
          x = xs,
          .packages = c("googledrive", "dplyr")
        ), {
          p()
          tryCatch(
            {
              googledrive::drive_auth(TRUE)
              files <- googledrive::drive_ls(sub_folders[x, ], recursive = TRUE)
              spreadsheets <- googledrive::drive_reveal(files, what = "mimeType") |>
                dplyr::filter(mime_type == "application/vnd.google-apps.spreadsheet")
            },
            error = \(e) {
              cli::cli_alert(paste0("Unable to obtain ", sub_folders[x, ]$name))
              NULL
            }
          )
        })
    })
  }

  if (nrow(spreadsheets_top_level) > 0 & nrow(sub_folders) > 0) {
    spreadsheets <- dplyr::bind_rows(spreadsheets_top_level,
                                     dplyr::bind_rows(y))
    return(spreadsheets)
  } else if (nrow(spreadsheets_top_level) == 0 & nrow(sub_folders) > 0) {
    return(dplyr::bind_rows(y))
  } else if (nrow(spreadsheets_top_level) > 0 & nrow(sub_folders) == 0) {
    return(spreadsheets_top_level)
  } else {
    return(NULL)
  }

}
