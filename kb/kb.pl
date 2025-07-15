% kb.pl
% ================ Base de conocimientos ================

% sintoma(Enfermedad, Síntoma).
sintoma(resfriado_comun, congestion_nasal).
sintoma(resfriado_comun, goteo_nasal).
sintoma(resfriado_comun, estornudos).
sintoma(resfriado_comun, dolor_garganta).
sintoma(resfriado_comun, tos).

sintoma(gripe, fiebre_alta).
sintoma(gripe, escalofrios).
sintoma(gripe, mialgias).
sintoma(gripe, cefalea).
sintoma(gripe, tos_seca).
sintoma(gripe, astenia).

sintoma(faringitis_viral, dolor_garganta).
sintoma(faringitis_viral, ronquera).
sintoma(faringitis_viral, secrecion_nasal).
sintoma(faringitis_viral, tos_seca).

sintoma(faringitis_estreptococica, dolor_garganta_intenso).
sintoma(faringitis_estreptococica, fiebre_brusca).
sintoma(faringitis_estreptococica, adenopatias_cervicales).
sintoma(faringitis_estreptococica, exudado_amigdalar).

sintoma(sinusitis_aguda, dolor_facial).
sintoma(sinusitis_aguda, presion_facial).
sintoma(sinusitis_aguda, rinorrea_purulenta).
sintoma(sinusitis_aguda, obstruccion_nasal).
sintoma(sinusitis_aguda, cefalea_inclinacion).

sintoma(otitis_media_aguda, otalgia).
sintoma(otitis_media_aguda, fiebre_moderada).
sintoma(otitis_media_aguda, hipoacusia).
sintoma(otitis_media_aguda, otorrea).

sintoma(laringitis_aguda, disfonia).
sintoma(laringitis_aguda, tos_perruna).
sintoma(laringitis_aguda, carraspera).
sintoma(laringitis_aguda, fiebre_baja).

sintoma(bronquitis_aguda, tos_productiva).
sintoma(bronquitis_aguda, sibilancias).
sintoma(bronquitis_aguda, disnea).
sintoma(bronquitis_aguda, fiebre_occasional).

sintoma(neumonia_comunitaria, fiebre_alta).
sintoma(neumonia_comunitaria, tos_productiva_purulenta).
sintoma(neumonia_comunitaria, dolor_toracico).
sintoma(neumonia_comunitaria, disnea).
sintoma(neumonia_comunitaria, taquipnea).

sintoma(rinitis_alergica, estornudos_salvas).
sintoma(rinitis_alergica, prurito_nasal).
sintoma(rinitis_alergica, prurito_ocular).
sintoma(rinitis_alergica, rinorrea_acuosa).
sintoma(rinitis_alergica, ojos_llorosos).

% ================ Sintomas del usuario (dinámico) ================
:- dynamic usuario_sintoma/1.

% Para agregar: assert(usuario_sintoma(fiebre_alta)). etc.

% ================ Regla genérica de diagnóstico ================
% calcula Nivel = (# síntomas coincidentes) / (# síntomas posibles)
diagnostico(Enf, Nivel) :-
    findall(S, sintoma(Enf, S), TS),        % TS = lista de todos los síntomas de Enf
    include(usuario_sintoma, TS, CS),       % CS = sublista de síntomas que el usuario tiene
    length(CS, N),                          % N = número de coincidencias
    length(TS, Total),                      % Total = número total de síntomas definidos
    Total > 0,
    Nivel is N / Total.

% Opcional: solo mostrar diagnósticos con Nivel >= Umbral (p.ej. 0.5)
diagnostico_confianza(Enf, Nivel, Umbral) :-
    diagnostico(Enf, Nivel),
    Nivel >= Umbral.
