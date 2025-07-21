/*
 * kb.pl - Base de conocimientos para el sistema experto de diagnóstico médico
 * Proyecto académico: Agente inteligente de diagnóstico de Infecciones Respiratorias Agudas
 * Autores:
 * Alan Isaac Pérez Hernández 336008878
 * Molina García Luis Alberto 17715680
 * Ethan David Ríos Beltrán 21257899
 * Fecha: Julio 2025
 * Descripción: Este archivo define hechos y reglas en Prolog para representar
 *              síntomas asociados a enfermedades respiratorias comunes, así como
 *              el mecanismo de diagnóstico basado en el cálculo de un nivel de
 *              confianza proporcional a la coincidencia de síntomas.
 */

% ==================== HECHOS: Relación Enfermedad-Síntoma ====================
% sintoma(Enfermedad, Sintoma).

% 1. Hipertensión arterial
sintoma(hipertension, cefalea).
sintoma(hipertension, mareo).
sintoma(hipertension, palpitaciones).
sintoma(hipertension, vision_borrosa).

% 2. Diabetes mellitus tipo 2
sintoma(diabetes_mellitus, poliuria).
sintoma(diabetes_mellitus, polidipsia).
sintoma(diabetes_mellitus, polifagia).
sintoma(diabetes_mellitus, perdida_de_peso).
sintoma(diabetes_mellitus, fatiga).

% 3. Artrosis
sintoma(artrosis, dolor_articular).
sintoma(artrosis, rigidez_matinal).
sintoma(artrosis, inflamacion_articular).
sintoma(artrosis, limitacion_movimiento).

% 4. Migraña
sintoma(migrena, cefalea_pulsatil).
sintoma(migrena, fotofobia).
sintoma(migrena, fonofobia).
sintoma(migrena, nausea).
sintoma(migrena, vomitos).

% 5. Depresión
sintoma(depresion, anhedonia).
sintoma(depresion, insomnio).
sintoma(depresion, fatiga).
sintoma(depresion, baja_autoestima).
sintoma(depresion, ideas_suicidas).

% 6. Gastroenteritis
sintoma(gastroenteritis, diarrea).
sintoma(gastroenteritis, vomitos).
sintoma(gastroenteritis, dolor_abdominal).
sintoma(gastroenteritis, fiebre).

% 7. Infección del tracto urinario
sintoma(infeccion_urinaria, disuria).
sintoma(infeccion_urinaria, urgencia_miccional).
sintoma(infeccion_urinaria, polaquiuria).
sintoma(infeccion_urinaria, hematuria).
sintoma(infeccion_urinaria, dolor_pelvico).

% 8. Gastritis
sintoma(gastritis, dolor_abdominal).
sintoma(gastritis, nausea).
sintoma(gastritis, vomitos).
sintoma(gastritis, pirosis).
sintoma(gastritis, distension_abdominal).

% 9. Anemia
sintoma(anemia, fatiga).
sintoma(anemia, palidez).
sintoma(anemia, disnea).
sintoma(anemia, taquicardia).
sintoma(anemia, mareo).

% 10. Ansiedad
sintoma(ansiedad, taquicardia).
sintoma(ansiedad, sudoracion).
sintoma(ansiedad, temblores).
sintoma(ansiedad, tension_muscular).
sintoma(ansiedad, preocupacion_excesiva).

% 11. Asma
sintoma(asma, disnea).
sintoma(asma, sibilancias).
sintoma(asma, tos_secundaria).
sintoma(asma, opresion_toracica).

% 12. Hipotiroidismo
sintoma(hipotiroidismo, fatiga).
sintoma(hipotiroidismo, aumento_peso).
sintoma(hipotiroidismo, intolerancia_frio).
sintoma(hipotiroidismo, piel_seca).
sintoma(hipotiroidismo, estreñimiento).

% 13. Artritis reumatoide
sintoma(artritis_reumatoide, dolor_articular).
sintoma(artritis_reumatoide, rigidez_matinal).
sintoma(artritis_reumatoide, inflamacion_simetrica).
sintoma(artritis_reumatoide, fatiga).
sintoma(artritis_reumatoide, fiebre_baja).

% 14. EPOC
sintoma(epoc, disnea).
sintoma(epoc, tos_productiva).
sintoma(epoc, sibilancias).
sintoma(epoc, fatiga).
sintoma(epoc, infecciones_respiratorias_recurrentes).

% 15. Hepatitis viral
sintoma(hepatitis, ictericia).
sintoma(hepatitis, fatiga).
sintoma(hepatitis, anorexia).
sintoma(hepatitis, nausea).
sintoma(hepatitis, dolor_abdominal_superior).

% 16. Insuficiencia renal
sintoma(insuficiencia_renal, oliguria).
sintoma(insuficiencia_renal, edema).
sintoma(insuficiencia_renal, fatiga).
sintoma(insuficiencia_renal, nauseas).
sintoma(insuficiencia_renal, confusión).

% 17. Resfriado común
sintoma(resfriado_comun, congestion_nasal).
sintoma(resfriado_comun, goteo_nasal).
sintoma(resfriado_comun, estornudos).
sintoma(resfriado_comun, dolor_garganta).
sintoma(resfriado_comun, tos).
sintoma(resfriado_comun, malestar_general).

% 18. Gripe (influenza)
sintoma(gripe, fiebre_alta).
sintoma(gripe, escalofrios).
sintoma(gripe, mialgias).
sintoma(gripe, cefalea).
sintoma(gripe, tos_seca).
sintoma(gripe, fatiga).

% 19. COVID-19 (forma leve-moderada)
sintoma(covid19, fiebre).
sintoma(covid19, tos_seca).
sintoma(covid19, anosmia).
sintoma(covid19, disnea).
sintoma(covid19, dolor_garganta).
sintoma(covid19, fatiga).

% 20. Otitis media
sintoma(otitis_media, dolor_oido).
sintoma(otitis_media, fiebre).
sintoma(otitis_media, perdida_auditiva).
sintoma(otitis_media, irritabilidad).
sintoma(otitis_media, supuracion_otica).

% 21. Rinitis alérgica
sintoma(rinitis_alergica, estornudos).
sintoma(rinitis_alergica, rinorrea).
sintoma(rinitis_alergica, congestion_nasal).
sintoma(rinitis_alergica, prurito_nasal).
sintoma(rinitis_alergica, ojos_llorosos).

% 22. Varicela
sintoma(varicela, fiebre).
sintoma(varicela, erupcion_cutanea).
sintoma(varicela, picazon).
sintoma(varicela, malestar_general).
sintoma(varicela, perdida_apetito).

% 23. Mononucleosis infecciosa
sintoma(mononucleosis, fiebre).
sintoma(mononucleosis, linfadenopatia).
sintoma(mononucleosis, fatiga).
sintoma(mononucleosis, faringitis).
sintoma(mononucleosis, esplenomegalia).

% 24. Dermatitis atópica
sintoma(dermatitis_atopica, picazon).
sintoma(dermatitis_atopica, eritema).
sintoma(dermatitis_atopica, piel_seca).
sintoma(dermatitis_atopica, lesiones_cutaneas).
sintoma(dermatitis_atopica, descamacion).

% 25. Conjuntivitis
sintoma(conjuntivitis, enrojecimiento_ocular).
sintoma(conjuntivitis, lagrimeo).
sintoma(conjuntivitis, picazon_ocular).
sintoma(conjuntivitis, secrecion_ocular).
sintoma(conjuntivitis, vision_borrosa).

% 26. Cistitis
sintoma(cistitis, disuria).
sintoma(cistitis, urgencia_miccional).
sintoma(cistitis, dolor_suprapubico).
sintoma(cistitis, orina_turbia).
sintoma(cistitis, hematuria).

% 27. Apendicitis aguda
sintoma(apendicitis, dolor_fosa_iliaca_derecha).
sintoma(apendicitis, fiebre).
sintoma(apendicitis, nauseas).
sintoma(apendicitis, vomitos).
sintoma(apendicitis, perdida_apetito).

% 28. Bronquitis aguda
sintoma(bronquitis, tos_productiva).
sintoma(bronquitis, disnea).
sintoma(bronquitis, dolor_toracico).
sintoma(bronquitis, fiebre_moderada).
sintoma(bronquitis, sibilancias).

% 29. Enfermedad ácido-péptica
sintoma(enfermedad_peptica, dolor_epigastrico).
sintoma(enfermedad_peptica, pirosis).
sintoma(enfermedad_peptica, nausea).
sintoma(enfermedad_peptica, distension_abdominal).
sintoma(enfermedad_peptica, saciedad_temprana).

% 30. Alergia alimentaria
sintoma(alergia_alimentaria, urticaria).
sintoma(alergia_alimentaria, nausea).
sintoma(alergia_alimentaria, vomitos).
sintoma(alergia_alimentaria, dolor_abdominal).
sintoma(alergia_alimentaria, disnea).

% 31. Enfermedad por reflujo gastroesofágico (ERGE)
sintoma(erge, pirosis).
sintoma(erge, regurgitacion).
sintoma(erge, dolor_toracico).
sintoma(erge, disfonia).
sintoma(erge, tos_nocturna).

% 32. Celiaquía
sintoma(celiaquia, diarrea_cronica).
sintoma(celiaquia, perdida_peso).
sintoma(celiaquia, distension_abdominal).
sintoma(celiaquia, anemia).
sintoma(celiaquia, fatiga).

% 33. Enfermedad de Parkinson
sintoma(parkinson, temblor_en_reposo).
sintoma(parkinson, bradicinesia).
sintoma(parkinson, rigidez_muscular).
sintoma(parkinson, inestabilidad_postural).
sintoma(parkinson, dificultad_para_hablar).

% 34. Esquizofrenia
sintoma(esquizofrenia, alucinaciones).
sintoma(esquizofrenia, delirios).
sintoma(esquizofrenia, pensamiento_desorganizado).
sintoma(esquizofrenia, retraimiento_social).
sintoma(esquizofrenia, afecto_plano).

% 35. Lupus eritematoso sistémico
sintoma(lupus, eritema_malar).
sintoma(lupus, fotosensibilidad).
sintoma(lupus, artralgia).
sintoma(lupus, fiebre).
sintoma(lupus, fatiga).

% 36. Enfermedad celíaca
sintoma(enfermedad_celiaca, diarrea).
sintoma(enfermedad_celiaca, distension_abdominal).
sintoma(enfermedad_celiaca, perdida_peso).
sintoma(enfermedad_celiaca, anemia_ferropenica).
sintoma(enfermedad_celiaca, fatiga).

% 37. Hipertiroidismo
sintoma(hipertiroidismo, perdida_peso).
sintoma(hipertiroidismo, taquicardia).
sintoma(hipertiroidismo, nerviosismo).
sintoma(hipertiroidismo, intolerancia_calor).
sintoma(hipertiroidismo, diarrea).

% 38. Síndrome del intestino irritable (SII)
sintoma(sii, dolor_abdominal).
sintoma(sii, distension_abdominal).
sintoma(sii, diarrea).
sintoma(sii, estreñimiento).
sintoma(sii, moco_en_heces).

% 39. Trastorno por déficit de atención e hiperactividad (TDAH)
sintoma(tdah, inatencion).
sintoma(tdah, hiperactividad).
sintoma(tdah, impulsividad).
sintoma(tdah, dificultad_concentracion).
sintoma(tdah, olvidos_frecuentes).

% 40. Fibromialgia
sintoma(fibromialgia, dolor_generalizado).
sintoma(fibromialgia, fatiga).
sintoma(fibromialgia, trastornos_sueño).
sintoma(fibromialgia, rigidez_matinal).
sintoma(fibromialgia, dificultades_cognitivas).

% 41. Enfermedad de Alzheimer
sintoma(alzheimer, perdida_memoria).
sintoma(alzheimer, desorientacion).
sintoma(alzheimer, cambios_conducta).
sintoma(alzheimer, dificultad_lenguaje).
sintoma(alzheimer, dificultad_para_realizar_tareas).

% 42. Trastorno bipolar
sintoma(trastorno_bipolar, euforia).
sintoma(trastorno_bipolar, irritabilidad).
sintoma(trastorno_bipolar, insomnio).
sintoma(trastorno_bipolar, depresion).
sintoma(trastorno_bipolar, impulsividad).

% 43. Endometriosis
sintoma(endometriosis, dismenorrea).
sintoma(endometriosis, dolor_pelvico).
sintoma(endometriosis, dispareunia).
sintoma(endometriosis, infertilidad).
sintoma(endometriosis, sangrado_menstrual_abundante).

% 44. Cáncer de pulmón
sintoma(cancer_pulmon, tos_cronica).
sintoma(cancer_pulmon, hemoptisis).
sintoma(cancer_pulmon, disnea).
sintoma(cancer_pulmon, perdida_peso).
sintoma(cancer_pulmon, dolor_toracico).

% 45. Cáncer de mama
sintoma(cancer_mama, masa_mamaria).
sintoma(cancer_mama, retraccion_pezon).
sintoma(cancer_mama, secrecion_pezon).
sintoma(cancer_mama, cambio_color_piel).
sintoma(cancer_mama, dolor_mama).

% 46. Cáncer colorrectal
sintoma(cancer_colorrectal, rectorragia).
sintoma(cancer_colorrectal, cambio_habito_intestinal).
sintoma(cancer_colorrectal, dolor_abdominal).
sintoma(cancer_colorrectal, perdida_peso).
sintoma(cancer_colorrectal, fatiga).

% 47. Alergia estacional (rinitis estacional)
sintoma(alergia_estacional, estornudos).
sintoma(alergia_estacional, prurito_nasal).
sintoma(alergia_estacional, congestion_nasal).
sintoma(alergia_estacional, lagrimeo).
sintoma(alergia_estacional, tos_seca).

% 48. Esclerosis múltiple
sintoma(esclerosis_multiple, vision_borrosa).
sintoma(esclerosis_multiple, debilidad_muscular).
sintoma(esclerosis_multiple, parestesias).
sintoma(esclerosis_multiple, fatiga).
sintoma(esclerosis_multiple, dificultad_marcha).

% 49. Pancreatitis aguda
sintoma(pancreatitis, dolor_abdominal_intenso).
sintoma(pancreatitis, nausea).
sintoma(pancreatitis, vomitos).
sintoma(pancreatitis, fiebre).
sintoma(pancreatitis, distension_abdominal).

% 50. Dengue clásico
sintoma(dengue, fiebre_alta).
sintoma(dengue, dolor_muscular).
sintoma(dengue, dolor_articular).
sintoma(dengue, cefalea).
sintoma(dengue, erupcion_cutanea).

% ================ Dinámica: Síntomas Reportados por el Usuario ================
% Se declara dinámico para poder asertar/retractar síntomas en tiempo de ejecución
:- dynamic usuario_sintoma/1.

% Para añadir un síntoma: assert(usuario_sintoma(nombre_sintoma)).
% Para limpiar todos los síntomas: retractall(usuario_sintoma(_)).


% ========================= REGLA: Diagnóstico =========================
/*
   diagnostico(+Enf, -Nivel)
   - Enf: átomo que representa la enfermedad
   - Nivel: número real entre 0 y 1 que indica la proporción de síntomas coincidentes
   Descripción:
     1) Se obtienen todas las enfermedades (posibles duplicados) mediante findall/3.
     2) Se eliminan duplicados usando sort/2.
     3) Se itera sobre cada enfermedad.
     4) Se recopilan sus síntomas (TS) y los que reportó el usuario (CS).
     5) Se calcula Nivel = |CS| / |TS|.
*/
diagnostico(Enf, Nivel) :-
    findall(E0, sintoma(E0,_), AllEs),  % 1) Todas las enfermedades
    sort(AllEs, Enfs),                  % 2) Eliminar duplicados
    member(Enf, Enfs),                  % 3) Iterar cada enfermedad
    findall(S, sintoma(Enf, S), TS),    % 4a) Síntomas totales
    findall(S, (member(S, TS), usuario_sintoma(S)), CS),  % 4b) Síntomas coincidentes
    length(CS, N),                      % 5a) Conteos
    length(TS, Total),
    Total > 0,
    Nivel is N/Total.                   % 5b) Cálculo de confianza


% ===================== REGLA: Filtrado por Umbral =====================
/*
   diagnostico_confianza(+Enf, -Nivel, +Umbral)
   - Devuelve solo los diagnósticos cuyo Nivel >= Umbral.
*/
diagnostico_confianza(Enf, Nivel, Umbral) :-
    diagnostico(Enf, Nivel),
    Nivel >= Umbral.
