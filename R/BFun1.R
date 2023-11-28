#' Title
#'
#' @param ptname
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
BFun1 = function (ptname, age, gender, weight, agent, country, year,
                  illnesses, path = "D:/")
{
  B <- BFun2(ptname, age, gender, weight, agent, country,
              year, illnesses, path)

  return(B)
}
