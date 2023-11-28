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
BFun2 = function (ptname, age, gender, weight, agent, country, year,
                   illnesses, path)
{
  library(dplyr)
  load(paste(getwd(), "DEMO.RData", sep = "/"))
  load(paste(getwd(), "REAC.RData", sep = "/"))
  load(paste(getwd(), "INDI.RData", sep = "/"))
  load(paste(getwd(), "DRUG.RData", sep = "/"))
  load(paste(getwd(), "THER.RData", sep = "/"))
  load(paste(getwd(), "RPSR.RData", sep = "/"))
  load(paste(getwd(), "OUTC.RData", sep = "/"))
  load(paste(getwd(), "DIC.RData", sep = "/"))
  inter.demo <- copy(DEMO)
  REAC <- REAC[, `:=`(PT, fcase(PT %in% ptname, "target_PT",
                                !PT %in% ptname, "NO_target_PT"))]
  data.table::setkeyv(inter.demo, "primaryid")
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
  setkeyv(inter.demo, "primaryid")
  inter.drug <- DRUG[unique(inter.demo$primaryid)]
  inter.outc <- OUTC[unique(inter.demo$primaryid)]
  inter.indi <- INDI[unique(inter.demo$primaryid)]
  inter.reac <- REAC[unique(inter.demo$primaryid)]
  inter.rpsr <- RPSR[unique(inter.demo$primaryid)]
  inter.ther <- THER[unique(inter.demo$primaryid)]
  inter.demo <- DEMO[unique(inter.demo$primaryid)]
  setkeyv(inter.drug, "primaryid")
  setkeyv(inter.outc, "primaryid")
  setkeyv(inter.indi, "primaryid")
  setkeyv(inter.reac, "primaryid")
  setkeyv(inter.rpsr, "primaryid")
  setkeyv(inter.ther, "primaryid")
  setkeyv(inter.demo, "primaryid")
  rm(DEMO)
  rm(DRUG)
  rm(OUTC)
  rm(INDI)
  rm(REAC)
  rm(RPSR)
  rm(THER)
  datalib2.reac <- inter.reac[PT %in% c("target_PT")]
  setkeyv(datalib2.reac, "primaryid")
  datalib2.drug <- inter.drug[unique(datalib2.reac$primaryid)]
  datalib2.drug <- datalib2.drug[ROLE_COD %in% c("PS")]
  ok <- datalib2.drug[, .(n = as.numeric(.N)), DRUGNAME]
  ok <- ok[order(-n)]
  if (!file.exists(path))
    dir.create(path, recur = T)
  fwrite(ok, file = file.path(path, paste0("引起特定PT的药物排名.csv")))
  datalib2.demo <- inter.demo[unique(datalib2.drug$primaryid)]
  datalib2.outc <- inter.outc[unique(datalib2.drug$primaryid)]
  datalib2.indi <- inter.indi[unique(datalib2.drug$primaryid)]
  setkeyv(datalib2.demo, "primaryid")
  setkeyv(datalib2.outc, "primaryid")
  setkeyv(datalib2.indi, "primaryid")
  setkeyv(datalib2.drug, "primaryid")
  datalib22.demo <- datalib2.drug[datalib2.demo, on = .(primaryid)]
  datalib22.outc <- datalib2.drug[datalib2.outc, on = .(primaryid)]
  datalib22.indi <- datalib2.drug[datalib2.indi, on = .(primaryid)]
  fwrite(datalib22.demo, file = file.path(path, paste0("引起特定PT的药物排名(人口学信息）.csv")))
  fwrite(datalib22.outc, file = file.path(path, paste0("引起特定PT的药物排名(结局指标信息）.csv")))
  fwrite(datalib22.indi, file = file.path(path, paste0("引起特定PT的药物排名(适应症信息）.csv")))
  return(list(ok = ok, datalib22.demo = datalib22.demo))
}
