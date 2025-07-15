/*
 * kb.pl - Base de conocimientos para el sistema experto de diagnóstico médico
 * Proyecto académico: Agente inteligente de diagnóstico de Infecciones Respiratorias Agudas
 * Autor: Isaac
 * Fecha: Julio 2025
 * Descripción: Este archivo define hechos y reglas en Prolog para representar
 *              síntomas asociados a enfermedades respiratorias comunes, así como
 *              el mecanismo de diagnóstico basado en el cálculo de un nivel de
 *              confianza proporcional a la coincidencia de síntomas.
 */

% ==================== HECHOS: Relación Enfermedad-Síntoma ====================
% sintoma(Enfermedad, Sintoma).

% Resfriado Común
sintoma(resfriado_comun, congestion_nasal).      % Obstrucción nasal
sintoma(resfriado_comun, goteo_nasal).           % Secreción acuosa
sintoma(resfriado_comun, estornudos).            % Estornudos frecuentes
sintoma(resfriado_comun, dolor_garganta).        % Leve irritación de garganta
sintoma(resfriado_comun, tos).                   % Tos ocasional

% Gripe (Influenza)
sintoma(gripe, fiebre_alta).                     % Fiebre > 38°C
sintoma(gripe, escalofrios).                     % Sensación de frío intenso
sintoma(gripe, mialgias).                        % Dolores musculares
sintoma(gripe, cefalea).                         % Dolor de cabeza intenso
sintoma(gripe, tos_seca).                        % Tos sin producción de moco
sintoma(gripe, astenia).                         % Fatiga marcada

% Faringitis Viral
sintoma(faringitis_viral, dolor_garganta).       % Irritación de garganta
sintoma(faringitis_viral, ronquera).             % Cambios en la voz
sintoma(faringitis_viral, secrecion_nasal).      % Secreción acuosa nasal
sintoma(faringitis_viral, tos_seca).             % Tos ligera

% Faringitis Estreptocócica
sintoma(faringitis_estreptococica, dolor_garganta_intenso).  % Garganta muy dolorosa
sintoma(faringitis_estreptococica, fiebre_brusca).          % Fiebre de aparición súbita
sintoma(faringitis_estreptococica, adenopatias_cervicales). % Ganglios inflamados
sintoma(faringitis_estreptococica, exudado_amigdalar).     % Secreción en amígdalas

% Sinusitis Aguda
sintoma(sinusitis_aguda, dolor_facial).          % Dolor en frente o mejillas
sintoma(sinusitis_aguda, presion_facial).        % Sensación de presión
sintoma(sinusitis_aguda, rinorrea_purulenta).    % Secreción nasal espesa
sintoma(sinusitis_aguda, obstruccion_nasal).     % Dificultad para respirar
sintoma(sinusitis_aguda, cefalea_inclinacion).   % Dolor al inclinar la cabeza

% Otitis Media Aguda
sintoma(otitis_media_aguda, otalgia).            % Dolor de oído
sintoma(otitis_media_aguda, fiebre_moderada).    % Fiebre leve a moderada
sintoma(otitis_media_aguda, hipoacusia).         % Disminución de la audición
sintoma(otitis_media_aguda, otorrea).            % Secreción de oído

% Laringitis Aguda
sintoma(laringitis_aguda, disfonia).             % Pérdida parcial de la voz
sintoma(laringitis_aguda, tos_perruna).          % Tos de tono áspero
sintoma(laringitis_aguda, carraspera).           % Gargujeo persistente
sintoma(laringitis_aguda, fiebre_baja).          % Fiebre leve

% Bronquitis Aguda
sintoma(bronquitis_aguda, tos_productiva).       % Tos con moco
sintoma(bronquitis_aguda, sibilancias).          % Silbidos al respirar
sintoma(bronquitis_aguda, disnea).               % Dificultad respiratoria
sintoma(bronquitis_aguda, fiebre_occasional).    % Fiebre intermitente

% Neumonía Comunitaria
sintoma(neumonia_comunitaria, fiebre_alta).      % Fiebre elevada
sintoma(neumonia_comunitaria, tos_productiva_purulenta). % Tos con moco verde/amarillo
sintoma(neumonia_comunitaria, dolor_toracico).  % Dolor al respirar
sintoma(neumonia_comunitaria, disnea).           % Falta de aire
sintoma(neumonia_comunitaria, taquipnea).        % Respiración acelerada

% Rinitis Alérgica (no infecciosa)
sintoma(rinitis_alergica, estornudos_salvas).   % Estornudos en ráfaga
sintoma(rinitis_alergica, prurito_nasal).       % Picor nasal
sintoma(rinitis_alergica, prurito_ocular).      % Picor en ojos
sintoma(rinitis_alergica, rinorrea_acuosa).     % Secreción clara
sintoma(rinitis_alergica, ojos_llorosos).      % Lagrimeo frecuente

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
