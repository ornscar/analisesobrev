
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
# k-m categoria de atendimento

km_atend <- survfit( 
  Surv(time = tempo_meses, event = status) ~ atendimento_diagnostico, 
  data = d_cint 
) 

# curva de k-m, por categoria de atendimento

ggsurvplot(
  km_atend, data = d_cint, 
  xlab = "Tempo (em meses)", ylab = "Probabilidade de sobrevida", 
  pval = TRUE, pval.size = 3, 
  risk.table = TRUE,
  fun = "pct",
  legend.title = "Categoria de atendimento",
  legend.labs = c("Convenio", "Particular", "SUS"),
) 


# modelo de cox -----------------------------------------------------------

# ajuste

mod_cox <- coxph( 
  Surv(time = tempo_meses, event = status) ~ escolaridade + faixa_etaria + sexo + 
    atendimento_diagnostico + estadiamento_clinico, 
  data =  d_cint 
); summary(mod_cox) 

# teste de riscos proporcionais

teste_rp <- cox.zph(mod_cox)

# graficos de residuos de shoenfeld

ggcoxzph( 
  teste_rp, #<<
  point.alpha = .6, 
  point.col = "#000000" 
) 
