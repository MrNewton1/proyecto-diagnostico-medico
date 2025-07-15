"""
app.py - Aplicación web Flask para el sistema experto de diagnóstico médico
Proyecto académico: Agente inteligente de diagnóstico de Infecciones Respiratorias Agudas
Autor: Isaac
Fecha: Julio 2025

Descripción:
    Este módulo define la aplicación web usando Flask. Proporciona una interfaz
    para que el usuario seleccione síntomas y visualice los diagnósticos generados
    por el controlador Prolog.

Componentes:
    - Creación de la instancia Flask
    - Inicialización de DiagnosticController con la KB Prolog
    - Ruta '/' con métodos GET/POST:
        * GET: muestra formulario de síntomas
        * POST: procesa selección, invoca diagnóstico y renderiza resultados
    - Ejecución en modo debug para desarrollo
"""

from flask import Flask, render_template, request
from controller import DiagnosticController
import os

# ==================== CONFIGURACIÓN DE LA APLICACIÓN ====================
app = Flask(__name__)

# Ruta absoluta al archivo kb.pl (ajustar según entorno de desarrollo)
KB_PATH = '/home/isaac/Descargas/proyecto_diagnostico_medico/kb/kb.pl'
print(f"🔍 Cargando KB desde: {KB_PATH}")

# Inicializar controlador de diagnóstico con la KB especificada
controller = DiagnosticController(kb_path=KB_PATH)

# ====================== RUTA PRINCIPAL '/' ==============================
@app.route('/', methods=['GET', 'POST'])
def index():
    """
    Vista principal de la aplicación.

    GET:
        - Renderiza el formulario HTML para selección de síntomas.
    POST:
        - Lee síntomas seleccionados desde el formulario.
        - Aserta dichos síntomas en la KB Prolog.
        - Obtiene diagnósticos con nivel de confianza >= 0.2.
        - Renderiza los resultados en la misma plantilla.
    """
    # Depuración: mostrar tipo de petición y datos recibidos
    print("👉 Petición:", request.method, "datos:", request.form)

    # Definición de todos los síntomas disponibles para el formulario
    symptoms = [
        'congestion_nasal', 'goteo_nasal', 'estornudos', 'dolor_garganta',
        'tos', 'fiebre_alta', 'escalofrios', 'mialgias', 'cefalea',
        'tos_seca', 'astenia', 'ronquera', 'secrecion_nasal',
        'dolor_garganta_intenso', 'fiebre_brusca', 'adenopatias_cervicales',
        'exudado_amigdalar', 'dolor_facial', 'presion_facial',
        'rinorrea_purulenta', 'obstruccion_nasal', 'cefalea_inclinacion',
        'otalgia', 'fiebre_moderada', 'hipoacusia', 'otorrea',
        'disfonia', 'tos_perruna', 'carraspera', 'fiebre_baja',
        'tos_productiva', 'sibilancias', 'disnea', 'fiebre_occasional',
        'tos_productiva_purulenta', 'dolor_toracico', 'taquipnea',
        'estornudos_salvas', 'prurito_nasal', 'prurito_ocular',
        'rinorrea_acuosa', 'ojos_llorosos'
    ]

    # Lista de resultados inicializada vacía
    results = []

    if request.method == 'POST':
        # Obtener síntomas seleccionados desde el formulario
        selected = request.form.getlist('sintomas')
        # Aserción de síntomas en Prolog
        controller.set_symptoms(selected)
        # Consulta de diagnósticos (umbral de confianza = 0.2)
        results = controller.get_diagnostics(threshold=0.2)
        # Depuración: mostrar diagnósticos obtenidos
        print("👉 Diagnósticos:", results)

    # Renderizar plantilla con datos:
    #   symptoms: lista de checkboxes
    #   results: lista de tuplas (enfermedad, confianza)
    return render_template('index.html', symptoms=symptoms, results=results)

# ===================== PUNTO DE ENTRADA PRINCIPAL ======================
if __name__ == '__main__':
    """
    Ejecuta el servidor Flask en modo debug para desarrollo.
    """
    app.run(debug=True)
