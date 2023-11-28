#' Title
#'
#' @param drug_PS
#' @param drug_concomitant
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
FFun1 = function (drug_PS, drug_concomitant, age, gender, weight, agent,
                  country, drugEffect, year, illnesses, path)
{
  library(dplyr)

  F <- FFun2(drug_PS, drug_concomitant, age, gender, weight,
              agent, country, drugEffect, year, illnesses, path)
  return(F)
}
