#' Title
#'
#' @param drug
#' @param age
#' @param gender
#' @param weight
#' @param agent
#' @param country
#' @param drugEffect
#' @param year
#' @param illnesses
#' @param ptname
#' @param path
#'
#' @return
#' @export
#'
#' @examples
CFun1 = function (drug, age, gender, weight, agent, country, drugEffect,
                  year, illnesses, ptname, path)
{
  library(dplyr)

  C <- CFun2(drug, age, gender, weight, agent, country,
              drugEffect, year, illnesses, ptname, path)
  return(C)
}
