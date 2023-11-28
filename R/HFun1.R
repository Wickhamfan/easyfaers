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
HFun1 = function (drug, age, gender, weight, agent, country, drugEffect,
          year, illnesses, path)
{
  library(dplyr)

  H <- HFun2(drug, age, gender, weight, agent, country,
             drugEffect, year, illnesses, path)
  return(H)
}
