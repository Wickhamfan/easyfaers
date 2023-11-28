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
HFun2 = function (drug, age, gender, weight, agent, country, drugEffect,
          year, illnesses, path)
{
  load(paste(setwd(), "DEMO.RData", sep = "/"))
  load(paste(setwd(), "REAC.RData", sep = "/"))
  load(paste(setwd(), "INDI.RData", sep = "/"))
  load(paste(setwd(), "DRUG.RData", sep = "/"))
  load(paste(setwd(), "THER.RData", sep = "/"))
  load(paste(setwd(), "RPSR.RData", sep = "/"))
  load(paste(setwd(), "OUTC.RData", sep = "/"))
  load(paste(setwd(), "DIC.RData", sep = "/"))
  inter.demo <- copy(DEMO)
  REAC <- DIC[REAC, on = .(PT)]
  data.table::setkeyv(REAC, "primaryid")
  if (!missing(year)) {
    inter.demo <- inter.demo[GetDataYear %between% year]
  }
  inter.demo <- inter.demo
  if (!missing(age)) {
    inter.demo <- inter.demo[AGE %between% age]
  }
  inter.demo <- inter.demo
  if (!missing(gender)) {
    inter.demo <- inter.demo[SEX %in% gender]
  }
  inter.demo <- inter.demo
  if (!missing(weight)) {
    inter.demo <- inter.demo[WT %between% weight]
  }
  inter.demo <- inter.demo
  if (!missing(country)) {
    inter.demo <- inter.demo[REPORTER_COUNTRY %in% country]
  }
  inter.demo <- inter.demo
  if (!missing(agent)) {
    inter.demo <- inter.demo[OCCP_COD %in% agent]
  }
  inter.demo <- inter.demo
  if (!missing(illnesses)) {
    inter.demo <- INDI[unique(inter.demo$primaryid)]
    inter.demo <- inter.demo[INDI_PT %in% illnesses]
  }
  inter.demo <- inter.demo
  data.table::setkeyv(inter.demo, "primaryid")
  inter.drug <- DRUG[unique(inter.demo$primaryid)]
  inter.outc <- OUTC[unique(inter.demo$primaryid)]
  inter.indi <- INDI[unique(inter.demo$primaryid)]
  inter.reac <- REAC[unique(inter.demo$primaryid)]
  inter.rpsr <- RPSR[unique(inter.demo$primaryid)]
  inter.ther <- THER[unique(inter.demo$primaryid)]
  inter.demo <- DEMO[unique(inter.demo$primaryid)]
  rm(DEMO)
  rm(DRUG)
  rm(OUTC)
  rm(INDI)
  rm(REAC)
  rm(RPSR)
  rm(THER)
  data.table::setkeyv(inter.demo, "primaryid")
  data.table::setkeyv(inter.drug, "primaryid")
  data.table::setkeyv(inter.outc, "primaryid")
  data.table::setkeyv(inter.indi, "primaryid")
  data.table::setkeyv(inter.reac, "primaryid")
  data.table::setkeyv(inter.rpsr, "primaryid")
  data.table::setkeyv(inter.ther, "primaryid")
  target.drug <- inter.drug[(DRUGNAME %in% drug | prod_ai %in%
                               drug) & (ROLE_COD %in% drugEffect)]
  target.drug <- inter.drug[unique(target.drug$primaryid)]
  target.drug <- target.drug[!DRUGNAME %in% drug | !prod_ai %in%
                               drug]
  okkk1 <- target.drug[, .(ac = as.numeric(.N)), DRUGNAME]
  fwrite(okkk1, file = file.path(path, paste0("与目标药物一起服用的药物统计.csv")))
  return(list(okkk1 = okkk1))
}
