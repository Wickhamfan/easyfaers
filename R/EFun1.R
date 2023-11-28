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
#' @param path
#'
#' @return
#' @export
#'
#' @examples
EFun1 = function (drug, age, gender, weight, agent, country, drugEffect,
                  year, illnesses, path)
{
  library(dplyr)

  E <- EFun2(drug, age, gender, weight, agent, country,
              drugEffect, year, illnesses, path)
  return(E)
}
