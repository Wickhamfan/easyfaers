#' Title
#'
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
GFun1 = function (age, gender, weight, agent, country, drugEffect, year,
                  illnesses, path)
{
  library(dplyr)
  G <- GFun2(age, gender, weight, agent, country, drugEffect,
              year, illnesses, path)
  return(G)
}
