#' Title
#'
#' @param drug_A
#' @param drug_B
#' @param age
#' @param gender
#' @param weight
#' @param agent
#' @param country
#' @param year
#' @param illnesses
#' @param path
#'
#' @return
#' @export
#'
#' @examples
IFun1 = function (drug_A = c("TACROLIMUS", "TACROLIMUS ", "TACROLIMUS",
                     "TACROLIMUS TACROLIMUS ANHYDROUS", "TACROLIMUS CAP 1MG",
                     "TACROLIMUS HYDRATE", "TACROLIMUS MONOHYDRATE", "TACROLIMUS CAP 0 5MG",
                     "TACROLIMUS SYSTEMIC", "TACROLIMUS  TACROLIMUS ", "TACROLIMUS  UNKNOWN ",
                     "TACROLIMUS CAPSULES", "TACROLIMUS OINTMENT", "TACROLIMUS 1MG",
                     "TACROLIMUS 0 5MG", "TACROLIMUS CAP", "TACROLIMUS CAP 5MG",
                     "TACROLIMUS  WATSON LABORATORIES ", "TACROLIMUS MR4 CAPSULES",
                     "TACROLIMUS 1MG CAP", "TACROLIMUS CAP 1MG ACCORD HEALTHCARE",
                     "PROGRAF  TACROLIMUS ", "TACROLIMUS COMP TAC ", "TACROLIMUS 1 MG",
                     "TACROLIMUS A", "TACROLIMUS MYLAN", "TACROLIMUS  2694A ",
                     "TACROLIMUS  CONTROL FOR SIROLIMUS  TACROLIMUS  CONTROL FOR SIROLIMUS ",
                     "TACROLIMUS 0 5MG CAP", "TACROLIMUS ACCORD", "TACROLIMUS CAP 0 5MG ACCORD HEALTHCARE",
                     "TACROLIMUS  1MG", "TACROLIMUS SANDOZ", "TACROLIMUS  STRIDES  1MG STRIDES PHARMA",
                     "TACROLIMUS 1MG STRIDES PHARMA", "TACROLIMUS 0 5 MG", "TACROLIMUS  NGX ",
                     "FK506  TACROLIMUS  CAPSULE", "TACROLIMUS CAP 1 MG"), drug_B = c("VORICONAZOLE",
                                                                                      "VORICONAZOLE ", "VORICONAZOLE", "VORICONAZOLE  VORICONAZOLE ",
                                                                                      "VORICONAZOLE TABLETS", "VORICONAZOLE  UNKNOWN ", "70436 029 80 VORICONAZOLE FOR INJECTION DRY VIAL 200 MG",
                                                                                      "VORICONAZOLE POWDER FOR SOLUTION FOR INFUSION", "VORICONAZOLE ARROW",
                                                                                      "VORICONAZOLE FILM COATED TABLET", "VORICONAZOLE MYLAN",
                                                                                      "VORICONAZOLE  MANUFACTURER UNKNOWN ", "VORICONAZOLE 200MG",
                                                                                      "VORICONAZOLE ARROW 200 MG POWDER FOR SOLUTION FOR INFUSION",
                                                                                      "VORICONAZOLE VORICONAZOLE ", "VFEND  VORICONAZOLE  INFUSION",
                                                                                      "APO VORICONAZOLE", "VFEND  VORICONAZOLE ", "VORICONAZOLE 200 MG",
                                                                                      "VORICONAZOLE TABLET", "VORICONAZOLE  VFEND ", "VORICONAZOLE 200 MG PFIZER",
                                                                                      "VFEND  VORICONAZOLE  INJECTION", "VFEND  VORICONAZOLE  TABLET",
                                                                                      "VORICONAZOLE TEVA", "VORICONAZOLE TAB", "VORICONAZOLE PFIZER",
                                                                                      "VORICONAZOLE  NGX ", "VORICONAZOLE   VORICONAZOLE ", "VORICONAZOLE ORAL SUSPENSION",
                                                                                      " VORICONAZOLE ", "BLINDED VORICONAZOLE", "VORICONAZOLE 200MG TABLET",
                                                                                      "VORICONAZOLE  VORICONAZOLE   VORICONAZOLE ", "SANDOZ VORICONAZOLE",
                                                                                      "VORICONAZOLE SANDOZ"), age, gender, weight, agent, country,
          year, illnesses, path = "~/结果文件")
{
  library(dplyr)
  res <- IFun2(drug_A, drug_B, age, gender, weight, agent,
               country, year, illnesses, path)
  return(res)
}
