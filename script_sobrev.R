
# bibliotecas -------------------------------------------------------------

#sobrevivencia
library(survival)
library(survminer)


# dados -------------------------------------------------------------------

# carregando os dados

d_cint <- readr::read_csv("dados/intestino.csv", show_col_types = FALSE)

# panorama da base de dados

dplyr::glimpse(d_cint) 


# censuras ----------------------------------------------------------------

# checando as 10 primeiras observações

head(Surv(time = d_cint$tempo_meses, event = d_cint$status), n = 100)


# k-m geral ---------------------------------------------------------------

# ajustar modelo não paramétrico baseado no estimador k-m
# k-m geral

km_geral <- survfit(
  Surv(time = tempo_meses, event = status) ~ 1, 
  data = d_cint #
) 

# curva de k-m geral

ggsurvplot(
  km_geral, data = d_cint,
  conf.int.style = "step", surv.median.line = "hv", fun = "pct",
  xlab = "Tempo (em meses)", ylab = "Probabilidade de sobrevida", 
  legend = "none"
)


# k-m, por categoria de atendimento ---------------------------------------

# ajustar modelo não paramétrico baseado no estimador k-m

km_atend <- survfit( 
  Surv(time = tempo_meses, event = status) ~ atendimento_diagnostico, 
  data = d_cint 
) 

# curvas de k-m, por categoria de atendimento

ggsurvplot(
  km_atend, data = d_cint, 
  xlab = "Tempo (em meses)", ylab = "Probabilidade de sobrevida", 
  pval = TRUE, pval.size = 3, 
  risk.table = TRUE, ncensor.plot = TRUE,
  fun = "pct", palette = "jco",
  legend.title = "Categoria de atendimento",
  legend.labs = c("Convenio", "Particular", "SUS")
) 


# k-m, por sexo -----------------------------------------------------------

# ajustar modelo não paramétrico baseado no estimador k-m

km_sexo <- survfit( 
  Surv(time = tempo_meses, event = status) ~ sexo, 
  data = d_cint 
) 

# curvas de k-m, por sexo

ggsurvplot(
  km_sexo, data = d_cint, 
  xlab = "Tempo (em meses)", ylab = "Probabilidade de sobrevida", 
  pval = TRUE, pval.size = 3, 
  risk.table = TRUE, ncensor.plot = TRUE,
  fun = "pct", palette = "jco",
  legend.title = "Sexo",
  legend.labs = c("Feminino", "Masculino")
) 


# modelo de cox - atendimento e sexo --------------------------------------

# ajuste

cox1 <- coxph( 
  Surv(time = tempo_meses, event = status) ~ sexo + atendimento_diagnostico, 
  data =  d_cint 
); summary(cox1) 

# teste de riscos proporcionais

rp1 <- cox.zph(cox1)

# graficos de residuos de schoenfeld

ggcoxzph( 
  rp1,
  point.alpha = .6, 
  point.col = "#000000" 
) 


# modelo de cox - todas as variaveis --------------------------------------

# ajuste

cox2 <- coxph( 
  Surv(time = tempo_meses, event = status) ~ escolaridade + faixa_etaria + sexo + 
    atendimento_diagnostico + estadiamento_clinico, 
  data =  d_cint 
); summary(cox2) 

# teste de riscos proporcionais

rp2 <- cox.zph(cox2)

# graficos de residuos de schoenfeld

ggcoxzph( 
  rp2, 
  point.alpha = .6, 
  point.col = "#000000" 
) 
