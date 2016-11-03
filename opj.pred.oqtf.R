# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements.  See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership.  The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License.  You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.

require(data.table)
require(magrittr)
require(xgboost) # version Github, non testé avec Cran
require(Matrix)
require(stringi) # Ok licence, éviter stringer en GPL

# TODO
# Annoter les articles avec leur contenu et changer le nom des variables en conséquence pour les rendre plus intelligibles
# Chercher les features qui nuisent à la prédiction, commencer par détecter les chercher très corrélées (de façon scripté)
# Mettre en place un détecteur de features nulles (toujours à 0) et crasher le script (assert / stopifnot / testthat)
# Paralléliser la recherche des patterns (partie la plus lente)
# Optimiser les paramètres (random search / grid search)

# La colonne demande.enrichie contient les demandes, parfois un résumé des moyens et les visa de la demande.
dt.decision <- fread(input = "decisions.oqtf.txt", sep = "|")

# Taux de rejet
dt.decision[, mean(rejet)] %>% print

# Le suffixe supralegem_ permet de repérer les features générées lorsqu'elles sont mélangées à d'autres (pour plus tard)
# fixed est plus rapide que regex mais ne gère pas les accents selon la doc de stringi
dt.patterns <- data.table(
  
  # Chaines de caractères / regex
  supralegem_motivation_insuffisante_ = stri_count(dt.decision$decision, regex = "(suffi|défaut).{1,10}motiv|motivation de cette mesure|motivation distincte"),
  supralegem_dispo_motivation_ = stri_count(dt.decision$decision, regex = "pas à faire l'objet d'une motivation"), # extrait d un article souvent cité
  supralegem_omis_statuer_ = stri_count(dt.decision$decision, regex = "omission à statuer"),
  supralegem_aide_juridictionnelle_ = stri_count(dt.decision$decision, regex = "aide juridictionnelle"),
  supralegem_bureau_aide_juridictionnelle_ = stri_count(dt.decision$demande.enrichi, regex = "bureau d'aide juridictionnelle"),
  supralegem_situation_perso_ = stri_count(dt.decision$decision, regex = "examen de sa situation personnelle"),
  supralegem_saisine_ = stri_count(dt.decision$decision, regex = "commission.{1,5}titre de séjour"),
  supralegem_incompétence_ = stri_count(dt.decision$decision, regex = "tiré.{1,10}incompétence"),
  supralegem_art_8_ = stri_count(dt.decision$decision, regex = "article 8 de la conv"),
  supralegem_annulé_ = stri_count(dt.decision$demande.enrichi, regex = "a annulé.{1,10}(décision|arrêté)"), # dans les demandes
  supralegem_art_9_ = stri_count(dt.decision$decision, regex = "article 9 de la conv"),
  supralegem_art_3.1_ = stri_count(dt.decision$decision, regex = "article 3.{1,10} la convention "),
  supralegem_non_lieu_ = stri_count(dt.decision$decision, regex = "non.?lieu"),
  supralegem_époux_ = stri_count(dt.decision$decision, regex = "époux"),
  supralegem_compétence_talent_ = stri_count(dt.decision$decision, regex = "compétences et talents"), # carte de séjour particulière
  supralegem_refus_ = stri_count(dt.decision$decision, regex = "refus d'autorisation"),
  supralegem_pathologie_ = stri_count(dt.decision$decision, regex = "pathologie"),
  supralegem_santé_ = stri_count(dt.decision$decision, regex = "état de santé"),
  supralegem_rapporteur_pub_ = stri_count(dt.decision$decision, regex = "rapporteur public|commissaire du gouvernement"),
  supralegem_soins_med_ = stri_count(dt.decision$decision, regex = "soins médicaux"),
  supralegem_recour_effectif_ = stri_count(dt.decision$decision, regex = "recours effectif"),
  supralegem_passeport_ = stri_count(dt.decision$decision, regex = "passeport"),
  supralegem_famille_ = stri_count(dt.decision$decision, regex = "situation personnelle et familiale"),
  supralegem_erreur_faits_ = stri_count(dt.decision$decision, regex = "erreur de fait"),
  supralegem_défaut_de_base_ = stri_count(dt.decision$decision, regex = "défaut de base légale"),
  supralegem_logement_ = stri_count(dt.decision$decision, regex = " logement"),
  supralegem_renouv_carte_ = stri_count(dt.decision$decision, regex = "renouvel.{1,20}(carte de séjour|titre de séjour)"),
  supralegem_délégation_ = stri_count(dt.decision$decision, regex = " délégation.{1,20}signature"),
  supralegem_réexamen_ = stri_count(dt.decision$demande.enrichi, regex = " réexamen"),
  supralegem_plein_droit_ = stri_count(dt.decision$decision, regex = " délivrance de plein droit"),
  supralegem_détournement_pouvoir_ = stri_count(dt.decision$decision, regex = " détournement de pouvoir"),
  supralegem_détournement_procedure_ = stri_count(dt.decision$decision, regex = " détournement de procédure"),
  supralegem_délais_ = stri_count(dt.decision$decision, regex = " délai de départ volontaire"),
  supralegem_médecin_agence_ = stri_count(dt.decision$decision, regex = " avis.{1,10}médecin de l'agence"),
  supralegem_médecin_inpecteur_ = stri_count(dt.decision$decision, regex = "médecin inspecteur"),
  supralegem_provisoire_ordonnance_ = stri_count(dt.decision$decision, regex = "annuler l'ordonnance"),
  supralegem_juridic_provisoire_ = stri_count(dt.decision$decision, regex = "juridictionnelle provisoire"),
  supralegem_traitement_ = stri_count(dt.decision$decision, regex = "absence.{1,10}traitement"),
  supralegem_disproportion_ = stri_count(dt.decision$decision, regex = "atteinte dispropor"),
  supralegem_menace_ = stri_count(dt.decision$decision, regex = "menace.{1,10}ordre public"),
  supralegem_référé_ = stri_count(dt.decision$decision, regex = "juge des référés"),
  supralegem_droit_enfant_ = stri_count(dt.decision$decision, regex = "convention .{1,20}droit(s)? de l'enfant"),
  supralegem_insertion_ = stri_count(dt.decision$decision, regex = "insertion"),
  supralegem_refus_délais_ = stri_count(dt.decision$decision, regex = "refus de délai de départ volontaire"),
  supralegem_risque_ = stri_count(dt.decision$decision, regex = "risque"),
  supralegem_assignation_ = stri_count(dt.decision$decision, regex = "assignation à résidence"),
  supralegem_ateinte_vie_priv_ = stri_count(dt.decision$decision, regex = "atteinte disproportionnée"),
  supralegem_erreur_appreciation_ = stri_count(dt.decision$decision, regex = "erreur manifeste d'appréciation"),
  supralegem_erreur_droit_ = stri_count(dt.decision$decision, regex = "erreur de droit"),
  supralegem_euros_ = stri_count(dt.decision$demande.enrichi, regex = "somme de .{1,10} euros"), # ne doit pas voir l ensemble de la décision -> L761 1
  supralegem_cloture_ = stri_count(dt.decision$decision, regex = "clôture de l'instruction"),
  supralegem_audience_ = stri_count(dt.decision$demande.enrichi, regex = "jour de l'audience"),
  supralegem_code_sejour_etranger_ = stri_count(dt.decision$decision, regex = "code de l'entrée et du séjour des étrangers et du droit d'asile"),
  supralegem_prefet_ = stri_count(dt.decision$demande.enrichi, regex = "préfet"),
  supralegem_ordonnance_ = stri_count(dt.decision$demande.enrichi, regex = "l'ordonnance"),
  supralegem_annulé_demande_ = stri_count(dt.decision$demande.enrichi, regex = "annul"),
  supralegem_sursis_ = stri_count(dt.decision$demande.enrichi, regex = "sursis"),
  
  # Pays d'origine
  supralegem_maroc_ = stri_count(dt.decision$decision, fixed = "maroc"),
  supralegem_tunisie_ = stri_count(dt.decision$decision, fixed = "tunisie"),
  supralegem_algérie_ = stri_count(dt.decision$decision, fixed = "algérie"),
  supralegem_turquie_ = stri_count(dt.decision$decision, fixed = "turquie"),
  supralegem_cameroun_ = stri_count(dt.decision$decision, fixed = "cameroun"),
  supralegem_mali_ = stri_count(dt.decision$decision, fixed = "mali"),
  supralegem_russie_ = stri_count(dt.decision$decision, fixed = "russie"),
  supralegem_sénégal_ = stri_count(dt.decision$decision, fixed = "sénégal"),
  supralegem_yougo_ = stri_count(dt.decision$decision, fixed = "yougoslavie"),
  supralegem_serbie_ = stri_count(dt.decision$decision, fixed = "serbie"),
  supralegem_croatie_ = stri_count(dt.decision$decision, fixed = "croatie"),
  supralegem_bosnie_ = stri_count(dt.decision$decision, fixed = "bosnie"),
  supralegem_roumanie_ = stri_count(dt.decision$decision, fixed = "roumanie"),
  
  # Conventions internationales les plus citées (en ajouter ?)
  supralegem_1979_ = stri_count(dt.decision$decision, fixed = "11 juillet 1979"),
  supralegem_2000_ = stri_count(dt.decision$decision, fixed = "12 avril 2000"),
  supralegem_2006_ = stri_count(dt.decision$decision, fixed = "24 juillet 2006"),
  supralegem_1968_ = stri_count(dt.decision$decision, fixed = "27 décembre 1968"),
  supralegem_dublin_ = stri_count(dt.decision$decision, fixed = "26 juin 2013"),  # Règlement (UE) n° 604/2013 du Parlement européen et du Conseil du 26 juin 2013
  
  # Réglements (fixed pour la vitesse)
  supralegem_l_761_1_ = stri_count(dt.decision$demande.enrichi, fixed = "l 761 1"), # ne doit pas voir l ensemble de la décision
  supralegem_r_732_1_1_ = stri_count(dt.decision$decision, fixed =  "r 732 1 1 "),
  supralegem_r_313_22_ = stri_count(dt.decision$decision, fixed =  "r 313 22 "),
  supralegem_r_613_1_ = stri_count(dt.decision$decision, fixed =  "r 613 1 "),
  supralegem_r_222_26_ = stri_count(dt.decision$decision, fixed =  "r 222 26 "),
  supralegem_r_612_3_ = stri_count(dt.decision$decision, fixed =  "r 612 3 "),
  supralegem_r_611_8_ = stri_count(dt.decision$decision, fixed =  "r 611 8 "),
  supralegem_r_351_8_ = stri_count(dt.decision$decision, fixed =  "r 351 8 "),
  supralegem_r_741_2_ = stri_count(dt.decision$decision, fixed =  "r 741 2 "),
  supralegem_r_222_1_ = stri_count(dt.decision$demande.enrichi, fixed =  "r 222 1 "), # desistement  # ne doit pas voir l ensemble de la décision
  supralegem_r_811_17_ = stri_count(dt.decision$decision, fixed =  "r 811 17 "),
  supralegem_r_611_7_ = stri_count(dt.decision$decision, fixed =  "r 611 7 "),
  supralegem_r_611_11_1_ = stri_count(dt.decision$decision, fixed =  "r 611 11 1 "),
  supralegem_r_313_21_ = stri_count(dt.decision$decision, fixed =  "r 313 21 "),
  supralegem_r_5221_20_ = stri_count(dt.decision$decision, fixed =  "r 5221 20 "),
  supralegem_r_311_13_ = stri_count(dt.decision$decision, fixed =  "r 311 13 "),
  supralegem_r_511_1_ = stri_count(dt.decision$decision, fixed =  "r 511 1 "),
  supralegem_r_312_2_ = stri_count(dt.decision$decision, fixed =  "r 312 2 "),
  supralegem_r_311_1_ = stri_count(dt.decision$decision, fixed =  "r 311 1 "),
  supralegem_r_351_3_ = stri_count(dt.decision$decision, fixed =  "r 351 3 "),
  supralegem_r_411_1_ = stri_count(dt.decision$decision, fixed =  "r 411 1 "),
  supralegem_r_761_1_ = stri_count(dt.decision$decision, fixed =  "r 761 1 "),
  supralegem_r_811_15_ = stri_count(dt.decision$decision, fixed =  "r 811 15 "),
  supralegem_r_313_7_ = stri_count(dt.decision$decision, fixed =  "r 313 7 "),
  supralegem_r_776_9_ = stri_count(dt.decision$decision, fixed =  "r 776 9 "),
  supralegem_r_611_1_ = stri_count(dt.decision$demande.enrichi, fixed =  "r 611 1 "),  # ne doit pas voir l ensemble de la décision
  supralegem_r_121_4_ = stri_count(dt.decision$decision, fixed =  "r 121 4 "),
  supralegem_r_311_4_ = stri_count(dt.decision$decision, fixed =  "r 311 4 "),
  supralegem_r_5221_11_ = stri_count(dt.decision$decision, fixed =  "r 5221 11 "),
  supralegem_r_313_1_ = stri_count(dt.decision$decision, fixed =  "r 313 1 "),
  supralegem_r_311_12_ = stri_count(dt.decision$decision, fixed =  "r 311 12 "),
  supralegem_r_613_3_ = stri_count(dt.decision$decision, fixed =  "r 613 3 "),
  supralegem_r_733_20_ = stri_count(dt.decision$decision, fixed =  "r 733 20 "),
  supralegem_r_313_13_ = stri_count(dt.decision$decision, fixed =  "r 313 13 "),
  supralegem_r_776_2_ = stri_count(dt.decision$decision, fixed =  "r 776 2 "),
  supralegem_r_311_2_ = stri_count(dt.decision$decision, fixed =  "r 311 2 "),
  supralegem_r_316_1_ = stri_count(dt.decision$decision, fixed =  "r 316 1 "),
  supralegem_r_5221_1_ = stri_count(dt.decision$decision, fixed =  "r 5221 1 "),
  supralegem_r_775_2_ = stri_count(dt.decision$decision, fixed =  "r 775 2 "),
  supralegem_r_311_10_ = stri_count(dt.decision$decision, fixed =  "r 311 10 "),
  supralegem_r_512_1_ = stri_count(dt.decision$decision, fixed =  "r 512 1 "),
  supralegem_r_811_14_ = stri_count(dt.decision$decision, fixed =  "r 811 14 "),
  supralegem_r_775_10_ = stri_count(dt.decision$decision, fixed =  "r 775 10 "),
  supralegem_r_741_7_ = stri_count(dt.decision$decision, fixed =  "r 741 7 "),
  supralegem_r_211_3_ = stri_count(dt.decision$decision, fixed =  "r 211 3 "),
  supralegem_r_5221_3_ = stri_count(dt.decision$decision, fixed =  "r 5221 3 "),
  supralegem_r_5221_15_ = stri_count(dt.decision$decision, fixed =  "r 5221 15 "),
  supralegem_r_732_1_10_ = stri_count(dt.decision$decision, fixed =  "r 732 1 10 "),
  supralegem_r_313_10_ = stri_count(dt.decision$decision, fixed =  "r 313 10 "),
  supralegem_r_776_11_ = stri_count(dt.decision$decision, fixed =  "r 776 11 "),
  supralegem_r_612_6_ = stri_count(dt.decision$decision, fixed =  "r 612 6 "),
  supralegem_r_776_17_ = stri_count(dt.decision$decision, fixed =  "r 776 17 "),
  supralegem_r_313_16_1_ = stri_count(dt.decision$decision, fixed =  "r 313 16 1 "),
  supralegem_r_742_1_ = stri_count(dt.decision$decision, fixed =  "r 742 1 "),
  supralegem_r_775_1_ = stri_count(dt.decision$decision, fixed =  "r 775 1 "),
  supralegem_r_331_36_ = stri_count(dt.decision$decision, fixed =  "r 331 36 "),
  supralegem_r_5221_17_ = stri_count(dt.decision$decision, fixed =  "r 5221 17 "),
  supralegem_r_621_1_ = stri_count(dt.decision$decision, fixed =  "r 621 1 "),
  supralegem_r_311_3_ = stri_count(dt.decision$decision, fixed =  "r 311 3 "),
  supralegem_r_811_7_ = stri_count(dt.decision$decision, fixed =  "r 811 7 "),
  supralegem_r_212_6_ = stri_count(dt.decision$decision, fixed =  "r 212 6 "),
  supralegem_r_341_7_2_ = stri_count(dt.decision$decision, fixed =  "r 341 7 2 "),
  supralegem_r_776_1_ = stri_count(dt.decision$decision, fixed =  "r 776 1 "),
  supralegem_r_742_3_ = stri_count(dt.decision$decision, fixed =  "r 742 3 "),
  supralegem_r_776_16_ = stri_count(dt.decision$decision, fixed =  "r 776 16 "),
  supralegem_r_613_2_ = stri_count(dt.decision$decision, fixed =  "r 613 2 "),
  supralegem_r_711_3_ = stri_count(dt.decision$decision, fixed =  "r 711 3 "),
  supralegem_r_512_1_1_ = stri_count(dt.decision$decision, fixed =  "r 512 1 1 "),
  supralegem_r_421_5_ = stri_count(dt.decision$decision, fixed =  "r 421 5 "),
  supralegem_r_776_13_ = stri_count(dt.decision$decision, fixed =  "r 776 13 "),
  supralegem_r_5221_33_ = stri_count(dt.decision$decision, fixed =  "r 5221 33 "),
  supralegem_r_313_15_ = stri_count(dt.decision$decision, fixed =  "r 313 15 "),
  supralegem_r_531_10_ = stri_count(dt.decision$decision, fixed =  "r 531 10 "),
  supralegem_r_741_1_ = stri_count(dt.decision$decision, fixed =  "r 741 1 "),
  supralegem_r_211_32_ = stri_count(dt.decision$decision, fixed =  "r 211 32 "),
  supralegem_r_553_14_5_ = stri_count(dt.decision$decision, fixed =  "r 553 14 5 "),
  supralegem_r_742_2_ = stri_count(dt.decision$decision, fixed =  "r 742 2 "),
  supralegem_r_313_16_2_ = stri_count(dt.decision$decision, fixed =  "r 313 16 2 "),
  supralegem_r_511_4_ = stri_count(dt.decision$decision, fixed =  "r 511 4 "),
  supralegem_r_921_6_ = stri_count(dt.decision$decision, fixed =  "r 921 6 "),
  supralegem_r_5221_32_ = stri_count(dt.decision$decision, fixed =  "r 5221 32 "),
  supralegem_r_5221_21_ = stri_count(dt.decision$decision, fixed =  "r 5221 21 "),
  supralegem_r_5221_34_ = stri_count(dt.decision$decision, fixed =  "r 5221 34 "),
  supralegem_r_833_1_ = stri_count(dt.decision$decision, fixed =  "r 833 1 "),
  supralegem_r_776_5_ = stri_count(dt.decision$decision, fixed =  "r 776 5 "),
  supralegem_r_731_2_ = stri_count(dt.decision$decision, fixed =  "r 731 2 "),
  supralegem_r_313_36_1_ = stri_count(dt.decision$decision, fixed =  "r 313 36 1 "),
  supralegem_r_312_8_ = stri_count(dt.decision$decision, fixed =  "r 312 8 "),
  supralegem_r_411_6_ = stri_count(dt.decision$decision, fixed =  "r 411 6 "),
  supralegem_r_315_10_ = stri_count(dt.decision$decision, fixed =  "r 315 10 "),
  supralegem_r_313_6_ = stri_count(dt.decision$decision, fixed =  "r 313 6 "),
  supralegem_r_312_5_ = stri_count(dt.decision$decision, fixed =  "r 312 5 "),
  supralegem_r_741_12_ = stri_count(dt.decision$decision, fixed =  "r 741 12 "),
  supralegem_r_5221_2_ = stri_count(dt.decision$decision, fixed =  "r 5221 2 "),
  supralegem_r_723_3_ = stri_count(dt.decision$decision, fixed =  "r 723 3 "),
  supralegem_r_321_8_ = stri_count(dt.decision$decision, fixed =  "r 321 8 "),
  supralegem_r_511_2_ = stri_count(dt.decision$decision, fixed =  "r 511 2 "),
  supralegem_r_513_3_ = stri_count(dt.decision$decision, fixed =  "r 513 3 "),
  supralegem_r_312_22_ = stri_count(dt.decision$decision, fixed =  "r 312 22 "),
  supralegem_r_775_4_ = stri_count(dt.decision$decision, fixed =  "r 775 4 "),
  supralegem_r_612_1_ = stri_count(dt.decision$decision, fixed =  "r 612 1 "),
  supralegem_r_5221_14_ = stri_count(dt.decision$decision, fixed =  "r 5221 14 "),
  supralegem_r_313_37_ = stri_count(dt.decision$decision, fixed =  "r 313 37 "),
  supralegem_r_313_23_ = stri_count(dt.decision$decision, fixed =  "r 313 23 "),
  supralegem_r_741_2_4_ = stri_count(dt.decision$decision, fixed =  "r 741 2 4 "),
  supralegem_r_13460_ = stri_count(dt.decision$decision, fixed =  "r 13460 "),
  supralegem_r_921_2_ = stri_count(dt.decision$decision, fixed =  "r 921 2 "),
  supralegem_r_341_1_ = stri_count(dt.decision$decision, fixed =  "r 341 1 "),
  supralegem_r_750_ = stri_count(dt.decision$decision, fixed =  "r 750 "),
  supralegem_r_723_1_ = stri_count(dt.decision$decision, fixed =  "r 723 1 "),
  supralegem_r_314_1_1_ = stri_count(dt.decision$decision, fixed =  "r 314 1 1 "),
  supralegem_r_771_9_ = stri_count(dt.decision$decision, fixed =  "r 771 9 "),
  supralegem_r_313_34_2_ = stri_count(dt.decision$decision, fixed =  "r 313 34 2 "),
  supralegem_r_775_8_ = stri_count(dt.decision$decision, fixed =  "r 775 8 "),
  supralegem_r_811_17_1_ = stri_count(dt.decision$decision, fixed =  "r 811 17 1 "),
  supralegem_r_811_10_1_ = stri_count(dt.decision$decision, fixed =  "r 811 10 1 "),
  supralegem_r_222_1_7_ = stri_count(dt.decision$demande.enrichi, fixed =  "r 222 1 7 "), # ne doit pas voir l ensemble de la décision
  supralegem_r_313_24_ = stri_count(dt.decision$decision, fixed =  "r 313 24 "),
  supralegem_r_5521_20_ = stri_count(dt.decision$decision, fixed =  "r 5521 20 "),
  supralegem_r_832_1_ = stri_count(dt.decision$decision, fixed =  "r 832 1 "),
  supralegem_r_2103_ = stri_count(dt.decision$decision, fixed =  "r 2103 "),
  supralegem_r_121_10_ = stri_count(dt.decision$decision, fixed =  "r 121 10 "),
  supralegem_r_431_4_ = stri_count(dt.decision$decision, fixed =  "r 431 4 "),
  supralegem_r_751_2_ = stri_count(dt.decision$decision, fixed =  "r 751 2 "),
  supralegem_r_313_26_ = stri_count(dt.decision$decision, fixed =  "r 313 26 "),
  supralegem_r_311_11_ = stri_count(dt.decision$decision, fixed =  "r 311 11 "),
  supralegem_r_5221_4_ = stri_count(dt.decision$decision, fixed =  "r 5221 4 "),
  supralegem_r_313_35_ = stri_count(dt.decision$decision, fixed =  "r 313 35 "),
  supralegem_r_221_1_ = stri_count(dt.decision$decision, fixed =  "r 221 1 "),
  supralegem_r_311_5_ = stri_count(dt.decision$decision, fixed =  "r 311 5 "),
  supralegem_r_751_3_ = stri_count(dt.decision$decision, fixed =  "r 751 3 "),
  supralegem_r_5221_35_ = stri_count(dt.decision$decision, fixed =  "r 5221 35 "),
  supralegem_r_411_3_ = stri_count(dt.decision$decision, fixed =  "r 411 3 "),
  supralegem_r_811_2_ = stri_count(dt.decision$decision, fixed =  "r 811 2 "),
  supralegem_r_313_16_ = stri_count(dt.decision$decision, fixed =  "r 313 16 "),
  supralegem_r_5221_6_ = stri_count(dt.decision$decision, fixed =  "r 5221 6 "),
  supralegem_r_312_3_ = stri_count(dt.decision$decision, fixed =  "r 312 3 "),
  supralegem_r_313_6_1_ = stri_count(dt.decision$decision, fixed =  "r 313 6 1 "),
  supralegem_r_776_24_ = stri_count(dt.decision$decision, fixed =  "r 776 24 "),
  supralegem_r_313_36_ = stri_count(dt.decision$decision, fixed =  "r 313 36 "),
  supralegem_r_553_14_ = stri_count(dt.decision$decision, fixed =  "r 553 14 "),
  supralegem_r_311_23_ = stri_count(dt.decision$decision, fixed =  "r 311 23 "),
  supralegem_r_776_20_ = stri_count(dt.decision$decision, fixed =  "r 776 20 "),
  supralegem_r_222_22_ = stri_count(dt.decision$decision, fixed =  "r 222 22 "),
  supralegem_r_821_1_ = stri_count(dt.decision$decision, fixed =  "r 821 1 "),
  supralegem_r_341_4_ = stri_count(dt.decision$decision, fixed =  "r 341 4 "),
  supralegem_r_313_20_ = stri_count(dt.decision$decision, fixed =  "r 313 20 "),
  supralegem_r_561_2_ = stri_count(dt.decision$decision, fixed =  "r 561 2 "),
  supralegem_r_312_1_ = stri_count(dt.decision$decision, fixed =  "r 312 1 "),
  supralegem_r_122_1_ = stri_count(dt.decision$decision, fixed =  "r 122 1 "),
  supralegem_r_831_1_ = stri_count(dt.decision$decision, fixed =  "r 831 1 "),
  supralegem_r_331_6_ = stri_count(dt.decision$decision, fixed =  "r 331 6 "),
  supralegem_r_733_32_ = stri_count(dt.decision$decision, fixed =  "r 733 32 "),
  supralegem_r_311_35_ = stri_count(dt.decision$decision, fixed =  "r 311 35 "),
  supralegem_r_771_12_ = stri_count(dt.decision$decision, fixed =  "r 771 12 "),
  supralegem_r_311_15_6_ = stri_count(dt.decision$decision, fixed =  "r 311 15 6 "),
  supralegem_r_313_34_1_ = stri_count(dt.decision$decision, fixed =  "r 313 34 1 "),
  supralegem_r_522_13_ = stri_count(dt.decision$decision, fixed =  "r 522 13 "),
  supralegem_r_313_17_ = stri_count(dt.decision$decision, fixed =  "r 313 17 "),
  supralegem_r_211_27_ = stri_count(dt.decision$decision, fixed =  "r 211 27 "),
  supralegem_r_213_3_ = stri_count(dt.decision$decision, fixed =  "r 213 3 "),
  supralegem_r_11_11_ = stri_count(dt.decision$decision, fixed =  "r 11 11 "),
  supralegem_r_121_12_ = stri_count(dt.decision$decision, fixed =  "r 121 12 "),
  supralegem_r_711_2_ = stri_count(dt.decision$demande.enrichi, fixed =  "r 711 2 "), # ne doit pas voir l ensemble de la décision
  supralegem_r_723_2_ = stri_count(dt.decision$decision, fixed =  "r 723 2 "),
  
  # Lois (fixed pour la vitesse)
  supralegem_l_313_11_ = stri_count(dt.decision$decision, fixed =  "l 313 11 "),
  supralegem_l_313_14_ = stri_count(dt.decision$decision, fixed =  "l 313 14 "),
  supralegem_l_911_1_ = stri_count(dt.decision$demande.enrichi, fixed =  "l 911 1 "), # ne doit pas voir l ensemble de la décision
  supralegem_l_511_1_ = stri_count(dt.decision$decision, fixed =  "l 511 1 "),
  supralegem_l_313_11_7 = stri_count(dt.decision$decision, fixed =  "l 313 11 7"),
  supralegem_l_732_1_ = stri_count(dt.decision$decision, fixed =  "l 732 1 "), # présence ou non d'un rapporteur publique
  supralegem_l_313_11_11 = stri_count(dt.decision$decision, fixed =  "l 313 11 11"),
  supralegem_l_313_7_ = stri_count(dt.decision$decision, fixed =  "l 313 7 "),
  supralegem_l_513_2_ = stri_count(dt.decision$decision, fixed =  "l 513 2 "),
  supralegem_l_314_11_ = stri_count(dt.decision$decision, fixed =  "l 314 11 "),
  supralegem_l_741_4_ = stri_count(dt.decision$decision, fixed =  "l 741 4 "),
  supralegem_l_313_10_ = stri_count(dt.decision$decision, fixed =  "l 313 10 "),
  supralegem_l_512_1_ = stri_count(dt.decision$demande.enrichi, fixed =  "l 512 1 "), # ne doit pas voir l ensemble de la décision
  supralegem_l_551_1_ = stri_count(dt.decision$decision, fixed =  "l 551 1 "),
  supralegem_l_121_1_ = stri_count(dt.decision$decision, fixed =  "l 121 1 "),
  supralegem_l_911_2_ = stri_count(dt.decision$demande.enrichi, fixed =  "l 911 2 "), # ne doit pas voir l ensemble de la décision
  supralegem_l_511_4_ = stri_count(dt.decision$decision, fixed =  "l 511 4 "),
  supralegem_l_312_2_ = stri_count(dt.decision$decision, fixed =  "l 312 2 "),
  supralegem_l_512_4_ = stri_count(dt.decision$demande.enrichi, fixed =  "l 512 4 "), # ne doit pas voir l ensemble de la décision
  supralegem_l_312_1_ = stri_count(dt.decision$decision, fixed =  "l 312 1 "),
  supralegem_l_531_1_ = stri_count(dt.decision$decision, fixed =  "l 531 1 "),
  supralegem_l_742_3_ = stri_count(dt.decision$decision, fixed =  "l 742 3 "),
  supralegem_l_511_3_1 = stri_count(dt.decision$decision, fixed =  "l 511 3 1"),
  supralegem_l_313_12_ = stri_count(dt.decision$decision, fixed =  "l 313 12 "),
  supralegem_l_741_1_ = stri_count(dt.decision$decision, fixed =  "l 741 1 "),
  supralegem_l_313_11_4 = stri_count(dt.decision$decision, fixed =  "l 313 11 4"),
  supralegem_l_313_11_6 = stri_count(dt.decision$decision, fixed =  "l 313 11 6"),
  supralegem_l_911_3_ = stri_count(dt.decision$demande.enrichi, fixed =  "l 911 3 "), # ne doit pas voir l ensemble de la décision
  supralegem_l_111_2_ = stri_count(dt.decision$decision, fixed =  "l 111 2 "),
  supralegem_l_311_7_ = stri_count(dt.decision$decision, fixed =  "l 311 7 "),
  supralegem_l_742_6_ = stri_count(dt.decision$decision, fixed =  "l 742 6 "),
  supralegem_l_314_11_8 = stri_count(dt.decision$decision, fixed =  "l 314 11 8"),
  supralegem_l_742_1_ = stri_count(dt.decision$decision, fixed =  "l 742 1 "),
  supralegem_l_311_12_ = stri_count(dt.decision$decision, fixed =  "l 311 12 "),
  supralegem_l_511_4_10 = stri_count(dt.decision$decision, fixed =  "l 511 4 10"),
  supralegem_l_561_2_ = stri_count(dt.decision$decision, fixed =  "l 561 2 "),
  supralegem_l_211_1_ = stri_count(dt.decision$decision, fixed =  "l 211 1 "),
  supralegem_l_5221_2_ = stri_count(dt.decision$decision, fixed =  "l 5221 2 "),
  supralegem_l_712_1_ = stri_count(dt.decision$decision, fixed =  "l 712 1 "),
  supralegem_l_313_13_ = stri_count(dt.decision$decision, fixed =  "l 313 13 "),
  supralegem_l_113_1_ = stri_count(dt.decision$decision, fixed =  "l 113 1 "),
  supralegem_l_313_4_1 = stri_count(dt.decision$decision, fixed =  "l 313 4 1"),
  supralegem_l_723_1_ = stri_count(dt.decision$decision, fixed =  "l 723 1 "),
  supralegem_l_313_15_ = stri_count(dt.decision$decision, fixed =  "l 313 15 "),
  supralegem_l_314_11_2 = stri_count(dt.decision$decision, fixed =  "l 314 11 2"),
  supralegem_l_513_4_ = stri_count(dt.decision$decision, fixed =  "l 513 4 "),
  supralegem_l_911_4_ = stri_count(dt.decision$demande.enrichi, fixed =  "l 911 4 "), # ne doit pas voir l ensemble de la décision
  supralegem_l_314_8_ = stri_count(dt.decision$decision, fixed =  "l 314 8 "),
  supralegem_l_711_1_ = stri_count(dt.decision$decision, fixed =  "l 711 1 "),
  supralegem_l_121_4_ = stri_count(dt.decision$decision, fixed =  "l 121 4 "),
  supralegem_l_313_10_1 = stri_count(dt.decision$decision, fixed =  "l 313 10 1"),
  supralegem_l_742_7_ = stri_count(dt.decision$decision, fixed =  "l 742 7 "),
  supralegem_l_311_3_ = stri_count(dt.decision$decision, fixed =  "l 311 3 "),
  supralegem_l_741_4_4 = stri_count(dt.decision$decision, fixed =  "l 741 4 4"),
  supralegem_l_512_2_ = stri_count(dt.decision$demande.enrichi, fixed =  "l 512 2 "),
  supralegem_l_411_1_ = stri_count(dt.decision$decision, fixed =  "l 411 1 "),
  supralegem_l_314_9_ = stri_count(dt.decision$decision, fixed =  "l 314 9 "),
  supralegem_l_316_1_ = stri_count(dt.decision$decision, fixed =  "l 316 1 "),
  supralegem_l_741_4_2 = stri_count(dt.decision$decision, fixed =  "l 741 4 2"),
  supralegem_l_313_6_ = stri_count(dt.decision$decision, fixed =  "l 313 6 "),
  supralegem_l_533_1_ = stri_count(dt.decision$decision, fixed =  "l 533 1 "),
  supralegem_l_611_1_1 = stri_count(dt.decision$decision, fixed =  "l 611 1 1"),
  supralegem_l_313_11_2 = stri_count(dt.decision$decision, fixed =  "l 313 11 2"),
  supralegem_l_311_11_7 = stri_count(dt.decision$decision, fixed =  "l 311 11 7"),
  supralegem_l_531_2_ = stri_count(dt.decision$decision, fixed =  "l 531 2 "),
  supralegem_l_551_2_ = stri_count(dt.decision$decision, fixed =  "l 551 2 "),
  supralegem_l_222_1_ = stri_count(dt.decision$decision, fixed =  "l 222 1 "),
  supralegem_l_561_1_ = stri_count(dt.decision$decision, fixed =  "l 561 1 "),
  supralegem_l_521_1_ = stri_count(dt.decision$decision, fixed =  "l 521 1 "),
  supralegem_l_311_11_ = stri_count(dt.decision$decision, fixed =  "l 311 11 "),
  supralegem_l_521_4_ = stri_count(dt.decision$decision, fixed =  "l 521 4 "),
  supralegem_l_313_5_ = stri_count(dt.decision$decision, fixed =  "l 313 5 "),
  supralegem_l_313_10_2 = stri_count(dt.decision$decision, fixed =  "l 313 10 2"),
  supralegem_l_314_6_ = stri_count(dt.decision$decision, fixed =  "l 314 6 "),
  supralegem_l_512_3_ = stri_count(dt.decision$demande.enrichi, fixed =  "l 512 3 "),
  supralegem_l_211_2_1 = stri_count(dt.decision$decision, fixed =  "l 211 2 1"),
  supralegem_l_621_1_ = stri_count(dt.decision$decision, fixed =  "l 621 1 "),
  supralegem_l_311_13_ = stri_count(dt.decision$decision, fixed =  "l 311 13 "),
  supralegem_l_741_3_ = stri_count(dt.decision$decision, fixed =  "l 741 3 "),
  supralegem_l_313_3_ = stri_count(dt.decision$decision, fixed =  "l 313 3 "),
  supralegem_l_313_7_1 = stri_count(dt.decision$decision, fixed =  "l 313 7 1"),
  supralegem_l_313_11_8 = stri_count(dt.decision$decision, fixed =  "l 313 11 8"),
  supralegem_l_541_1_ = stri_count(dt.decision$decision, fixed =  "l 541 1 "),
  supralegem_l_722_1_ = stri_count(dt.decision$decision, fixed =  "l 722 1 "),
  supralegem_l_317_1_ = stri_count(dt.decision$decision, fixed =  "l 317 1 "),
  supralegem_l_431_2_ = stri_count(dt.decision$decision, fixed =  "l 431 2 "),
  supralegem_l_121_2_ = stri_count(dt.decision$decision, fixed =  "l 121 2 "),
  supralegem_l_313_10_3 = stri_count(dt.decision$decision, fixed =  "l 313 10 3"),
  supralegem_l_733_2_ = stri_count(dt.decision$decision, fixed =  "l 733 2 "),
  supralegem_l_311_11_6 = stri_count(dt.decision$decision, fixed =  "l 311 11 6"),
  supralegem_l_313_11_3 = stri_count(dt.decision$decision, fixed =  "l 313 11 3"),
  supralegem_l_542_4_ = stri_count(dt.decision$decision, fixed =  "l 542 4 "),
  supralegem_l_513_11_ = stri_count(dt.decision$decision, fixed =  "l 513 11 "),
  supralegem_l_221_1_ = stri_count(dt.decision$decision, fixed =  "l 221 1 "),
  supralegem_l_521_3_ = stri_count(dt.decision$decision, fixed =  "l 521 3 "),
  supralegem_l_762_1_ = stri_count(dt.decision$decision, fixed =  "l 762 1 "),
  supralegem_l_531_3_ = stri_count(dt.decision$decision, fixed =  "l 531 3 "),
  supralegem_l_121_1_1 = stri_count(dt.decision$decision, fixed =  "l 121 1 1"),
  supralegem_l_313_18_ = stri_count(dt.decision$decision, fixed =  "l 313 18 "),
  supralegem_l_131_10_ = stri_count(dt.decision$decision, fixed =  "l 131 10 "),
  supralegem_l_122_1_ = stri_count(dt.decision$decision, fixed =  "l 122 1 "),
  supralegem_l_313_14_8 = stri_count(dt.decision$decision, fixed =  "l 313 14 8"),
  supralegem_l_523_1_ = stri_count(dt.decision$decision, fixed =  "l 523 1 "),
  supralegem_l_511_11_ = stri_count(dt.decision$decision, fixed =  "l 511 11 "),
  supralegem_l_313_9_ = stri_count(dt.decision$decision, fixed =  "l 313 9 "),
  supralegem_l_314_4_ = stri_count(dt.decision$decision, fixed =  "l 314 4 "),
  supralegem_l_714_1_ = stri_count(dt.decision$decision, fixed =  "l 714 1 "),
  supralegem_l_311_1_ = stri_count(dt.decision$decision, fixed =  "l 311 1 "),
  supralegem_l_313_13_7 = stri_count(dt.decision$decision, fixed =  "l 313 13 7"),
  supralegem_l_313_1_11 = stri_count(dt.decision$decision, fixed =  "l 313 1 11"),
  supralegem_l_311_11_11 = stri_count(dt.decision$decision, fixed =  "l 311 11 11"),
  supralegem_l_511_1_3 = stri_count(dt.decision$decision, fixed =  "l 511 1 3"),
  supralegem_l_1_196_ = stri_count(dt.decision$decision, fixed =  "l 1 196 "),
  supralegem_l_3113_11_11 = stri_count(dt.decision$decision, fixed =  "l 3113 11 11"),
  supralegem_l_716_1_ = stri_count(dt.decision$decision, fixed =  "l 716 1 "),
  supralegem_l_313_10_4 = stri_count(dt.decision$decision, fixed =  "l 313 10 4"),
  supralegem_l_555_1_6 = stri_count(dt.decision$decision, fixed =  "l 555 1 6"),
  supralegem_l_315_1_ = stri_count(dt.decision$decision, fixed =  "l 315 1 "),
  supralegem_l_613_3_ = stri_count(dt.decision$decision, fixed =  "l 613 3 "),
  supralegem_l_611_1_ = stri_count(dt.decision$decision, fixed =  "l 611 1 "),
  supralegem_l_791_1_ = stri_count(dt.decision$decision, fixed =  "l 791 1 "),
  supralegem_l_768_7_ = stri_count(dt.decision$decision, fixed =  "l 768 7 "),
  supralegem_l_911_7_ = stri_count(dt.decision$demande.enrichi, fixed =  "l 911 7 "),
  supralegem_l_316_1_2 = stri_count(dt.decision$decision, fixed =  "l 316 1 2"),
  supralegem_l_313_13_11 = stri_count(dt.decision$decision, fixed =  "l 313 13 11"),
  supralegem_l_313_1_ = stri_count(dt.decision$decision, fixed =  "l 313 1 "),
  supralegem_l_121_1_3 = stri_count(dt.decision$decision, fixed =  "l 121 1 3"),
  supralegem_l_111_6_ = stri_count(dt.decision$decision, fixed =  "l 111 6 "),
  supralegem_l_111_1_ = stri_count(dt.decision$decision, fixed =  "l 111 1 "),
  supralegem_l_131_11_ = stri_count(dt.decision$decision, fixed =  "l 131 11 "),
  supralegem_l_731_1_ = stri_count(dt.decision$decision, fixed =  "l 731 1 "),
  supralegem_l_511_3_ = stri_count(dt.decision$decision, fixed =  "l 511 3 "),
  supralegem_l_8251_1_ = stri_count(dt.decision$decision, fixed =  "l 8251 1 "),
  supralegem_l_313_22_ = stri_count(dt.decision$decision, fixed =  "l 313 22 "),
  supralegem_l_3111_11_ = stri_count(dt.decision$decision, fixed =  "l 3111 11 "),
  supralegem_l_331_11_ = stri_count(dt.decision$decision, fixed =  "l 331 11 "),
  supralegem_l_511_2_ = stri_count(dt.decision$decision, fixed =  "l 511 2 "),
  supralegem_l_411_3_ = stri_count(dt.decision$decision, fixed =  "l 411 3 "),
  supralegem_l_313_4_ = stri_count(dt.decision$decision, fixed =  "l 313 4 "),
  supralegem_l_523_3_ = stri_count(dt.decision$decision, fixed =  "l 523 3 "),
  
  # Ajout des factors (nature des parties, nom de magistrat)
  supralegem_demande_ = dt.decision$demandeurQualite %>% factor, 
  supralegem_defense_ = dt.decision$defendeurQualite %>% factor, 
  supralegem_magistrat_ = stri_replace_all(dt.decision$president, fixed = " ", replacement = "_") %>% factor
)

# Filtrage des features rares (paramétrable)
col.to.remove <- dt.patterns[, 1:(ncol(dt.patterns) - 3), with=F][,colSums(.SD > 0)] %>% {.[which(. < 150)]} %>% names
col.to.keep <- names(dt.patterns) [!names(dt.patterns) %in% col.to.remove]

# Génération des features quadratiques et concaténation avec les features simples puis sparsification pour la vitesse
feature.matrix <- model.matrix(~., data = dt.patterns[,col.to.keep,with=F]) %>% cbind(model.matrix(~.^2, data = dt.patterns[,col.to.keep,with=F])) %>% Matrix(sparse = T)

# On a créé ce nombre de features (19K) : 
ncol(feature.matrix) %>% print

# pas de dev set car utilisation de paramètres standards utilisés sur Kaggle (mettre en place un random search pour optimisation)
rows <- nrow(feature.matrix)
samples <- rows %>% sample
training.selection <- (0.8 * rows) %>% floor %>% {samples[1:.]}

dtrain <- feature.matrix[ training.selection,] %>% xgb.DMatrix(label = dt.decision[training.selection, rejet])
dtest <- feature.matrix[-training.selection,] %>% xgb.DMatrix(label = dt.decision[-training.selection, rejet])

pred.model <- xgb.train(params = list(max.depth = 15, eta = 0.1, booster = "gbtree"), dtrain, nthread = min(parallel::detectCores() - 1, 1), nround = 200, objective = "binary:logistic", watchlist = list(train = dtrain, eval = dtest))

# Taux de prédiction
(predict(pred.model, dtest) > 0.5) %>% {sum(. == getinfo(dtest, "label")) / length(.)} %>% print

# Afficher les features les plus importantes sur R Studio
xgb.importance(feature_names = feature.matrix@Dimnames[[2]], model = pred.model) %>% View

# Extraction des numéros d'articles depuis le texte : 
# dt.decision[,stri_extract(decision, regex = "(r|l) \\d+ (\\d+ )?(\\d+ )?")] %>% {paste0("supralegem_", stri_replace_all_regex(.," ", "_"), " = stri_count(dt.decision$decision, regex = \"", ., "\"),\n" )} %>% cat
