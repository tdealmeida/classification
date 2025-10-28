


run_pelt_mean <- function(values) {
  result <- tryCatch({
    # Analyse de changement de moyenne avec PELT
    cpt_result_pelt <- cpt.mean(
      values,
      method = "PELT",
      penalty = "Manual",
      # pen.value = 1,
      pen.value = "0.5*log(n)",  # BIC
      minseglen = 4  # 4 pour avoir au moins 3 segments valides
    )
    
    # Extraire les points de changement
    cpt_pelt <- cpt_result_pelt@cpts
    cpt_pelt <- cpt_pelt[-length(cpt_pelt)]  # retirer le dernier, c'est juste la fin
    
    # Retourner un tibble
    tibble(
      cp = list(cpt_pelt),
      cpt = length(cpt_pelt)
    )
    
  }, error = function(e) {
    # En cas d'erreur -> pas de changements
    tibble(
      cp = list(integer(0)),
      cpt = 0
    )
  })
  
  return(result)
}





run_pelt_meanvar <- function(values) {
  result <- tryCatch({
    # Analyse de changement de moyenne avec PELT
    cpt_result_pelt <- cpt.meanvar(
      values,
      method = "PELT",
      penalty = "Manual",
      # pen.value = 1,
      pen.value = "2.5*log(n)",  # BIC
      minseglen = 4  # 4 pour avoir au moins 3 segments valides
    )
    
    # Extraire les points de changement
    cpt_pelt <- cpt_result_pelt@cpts
    cpt_pelt <- cpt_pelt[-length(cpt_pelt)]  # retirer le dernier, c'est juste la fin
    
    # Retourner un tibble
    tibble(
      cp = list(cpt_pelt),
      cpt = length(cpt_pelt)
    )
    
  }, error = function(e) {
    # En cas d'erreur -> pas de changements
    tibble(
      cp = list(integer(0)),
      cpt = 0
    )
  })
  
  return(result)
}
# ============================================
# 5. Exécution de l'Analyse pour Différentes Variables
# ============================================

# # Analyse pour 'active_channel_width'
# segmentation <- lit_banc %>%
#   group_by(toponyme) %>%
#   summarize(run_change_point_analysis(ACW_star)) %>%
#   filter(cpt > 0)   # Supprimer les lignes avec cpt = 0
