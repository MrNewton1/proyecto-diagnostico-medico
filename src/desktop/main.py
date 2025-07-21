"""
main.py - Interfaz de escritorio (GUI) para el sistema experto de diagnóstico médico
Proyecto académico: Agente inteligente de diagnóstico de enfermedades comunes
Autores:
Alan Isaac Pérez Hernández 336008878
Molina García Luis Alberto 17715680
Ethan David Ríos Beltrán 21257899
Fecha: Julio 2025

Descripción:
    Esta aplicación desktop basada en Tkinter y ttk permite al usuario buscar y seleccionar
    una lista de síntomas generales para generar diagnósticos vía un motor Prolog.
    Emplea un tema oscuro, búsqueda dinámica y listbox de selección múltiple.
"""

import os
import tkinter as tk
from tkinter import ttk, messagebox
from src.controller import DiagnosticController

# =============================
# Configuración de ruta a la KB
# =============================
BASE_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..'))
KB_PATH = os.path.join(BASE_DIR, 'kb', 'kb.pl')  # Ruta absoluta al archivo Prolog

# Inicializar el controlador Prolog
ctrl = DiagnosticController(kb_path=KB_PATH)

# =============================
# Definición de síntomas globales
# =============================
SYMPTOMS = [
    'afecto_plano', 'alucinaciones', 'anemia', 'anemia_ferropenica', 'anhedonia',
    'anorexia', 'anosmia', 'artralgia', 'aumento_peso', 'baja_autoestima',
    'bradicinesia', 'cambio_color_piel', 'cambio_habito_intestinal',
    'cambios_conducta', 'cefalea', 'cefalea_pulsatil', 'congestion_nasal',
    'debilidad_muscular', 'delirios', 'depresion', 'descamacion', 'desorientacion',
    'diarrea', 'diarrea_cronica', 'dificultad_concentracion', 'dificultad_lenguaje',
    'dificultad_marcha', 'dificultad_para_hablar', 'dificultad_para_realizar_tareas',
    'dificultades_cognitivas', 'disfonia', 'dismenorrea', 'disnea', 'dispareunia',
    'distension_abdominal', 'disuria', 'dolor_abdominal', 'dolor_abdominal_intenso',
    'dolor_abdominal_superior', 'dolor_articular', 'dolor_epigastrico',
    'dolor_fosa_iliaca_derecha', 'dolor_garganta', 'dolor_generalizado',
    'dolor_mama', 'dolor_muscular', 'dolor_oido', 'dolor_pelvico',
    'dolor_suprapubico', 'dolor_toracico', 'edema', 'enrojecimiento_ocular',
    'eritema', 'eritema_malar', 'erupcion_cutanea', 'escalofrios', 'esplenomegalia',
    'estornudos', 'euforia', 'faringitis', 'fatiga', 'fiebre', 'fiebre_alta',
    'fiebre_baja', 'fiebre_moderada', 'fonofobia', 'fotofobia', 'fotosensibilidad',
    'goteo_nasal', 'hematuria', 'hemoptisis', 'hiperactividad', 'ictericia',
    'ideas_suicidas', 'impulsividad', 'inatencion', 'inestabilidad_postural',
    'infecciones_respiratorias_recurrentes', 'infertilidad', 'inflamacion_articular',
    'inflamacion_simetrica', 'insomnio', 'intolerancia_calor', 'intolerancia_frio',
    'irritabilidad', 'lagrimeo', 'lesiones_cutaneas', 'limitacion_movimiento',
    'linfadenopatia', 'malestar_general', 'mareo', 'masa_mamaria', 'mialgias',
    'moco_en_heces', 'nausea', 'nauseas', 'nerviosismo', 'ojos_llorosos', 'oliguria',
    'olvidos_frecuentes', 'opresion_toracica', 'orina_turbia', 'palidez',
    'palpitaciones', 'parestesias', 'pensamiento_desorganizado', 'perdida_apetito',
    'perdida_auditiva', 'perdida_de_peso', 'perdida_memoria', 'picazon',
    'picazon_ocular', 'piel_seca', 'pirosis', 'polaquiuria', 'polidipsia',
    'polifagia', 'poliuria', 'preocupacion_excesiva', 'prurito_nasal', 'rectorragia',
    'regurgitacion', 'retraccion_pezon', 'retraimiento_social', 'rigidez_matinal',
    'rigidez_muscular', 'rinorrea', 'saciedad_temprana',
    'sangrado_menstrual_abundante', 'secrecion_ocular', 'secrecion_pezon',
    'sibilancias', 'sudoracion', 'supuracion_otica', 'taquicardia',
    'temblor_en_reposo', 'temblores', 'tension_muscular', 'tos',
    'tos_cronica', 'tos_nocturna', 'tos_productiva', 'tos_seca', 'tos_secundaria',
    'urgencia_miccional', 'urticaria', 'vision_borrosa', 'vomitos'
]

class App(tk.Tk):
    """
    Interfaz de escritorio Tkinter/ttk con tema oscuro.

    Permite búsqueda dinámica de síntomas, selección múltiple y presentación
    de diagnósticos obtenidos desde la base de conocimientos Prolog.
    """
    def __init__(self):
        super().__init__()
        # Configuración de la ventana principal
        self.title("Diagnóstico Médico")
        self.geometry("700x600")
        self.configure(bg='#2e2e2e')  # Fondo oscuro

        # Crear estilos y widgets
        self._create_styles()
        self._create_widgets()

    def _create_styles(self):
        """
        Define estilos personalizados ttk para paleta oscura.
        """
        style = ttk.Style(self)
        style.theme_use('clam')  # Basado en tema ligero para personalizar

        # Colores de la paleta
        bg, fg, accent = '#2e2e2e', 'white', '#448aff'

        # Estilos generales
        style.configure('Dark.TFrame', background=bg)
        style.configure('Dark.TLabel', background=bg, foreground=fg)
        style.configure('Dark.TEntry', fieldbackground='#1e1e1e', foreground=fg)
        style.configure('Dark.TButton', background=accent, foreground=fg,
                        font=('Segoe UI', 10, 'bold'))
        style.map('Dark.TButton', background=[('active', '#2962ff')])

    def _create_widgets(self):
        """
        Construye layout: buscador, listbox con scrollbar, botón y área de texto.
        """
        frame = ttk.Frame(self, padding=10, style='Dark.TFrame')
        frame.pack(fill=tk.BOTH, expand=True)

        # Etiqueta y entrada para búsqueda de síntomas
        lbl_search = ttk.Label(frame, text="Buscar síntoma:", style='Dark.TLabel')
        lbl_search.grid(row=0, column=0, sticky='w')
        self.search_var = tk.StringVar()
        entry_search = ttk.Entry(frame, textvariable=self.search_var, style='Dark.TEntry')
        entry_search.grid(row=0, column=1, columnspan=2, sticky='ew', padx=5)
        # Vincular traza para actualización dinámica de lista
        self.search_var.trace_add('write', lambda *args: self._filter_list())

        # Listbox de selección múltiple con scrollbar
        self.listbox = tk.Listbox(
            frame, selectmode=tk.MULTIPLE,
            bg='#1e1e1e', fg='white', selectbackground='#2962ff', highlightthickness=0
        )
        scrollbar = ttk.Scrollbar(frame, orient='vertical', command=self.listbox.yview)
        self.listbox.config(yscrollcommand=scrollbar.set)
        self.listbox.grid(row=1, column=0, columnspan=2, sticky='nsew', pady=5)
        scrollbar.grid(row=1, column=2, sticky='ns')
        frame.columnconfigure(1, weight=1)
        frame.rowconfigure(1, weight=1)

        # Inicializar lista con todos los síntomas
        self.filtered = SYMPTOMS.copy()
        self._update_listbox()

        # Botón para iniciar diagnóstico
        btn_diag = ttk.Button(frame, text="Diagnosticar", command=self._on_diagnose,
                              style='Dark.TButton')
        btn_diag.grid(row=2, column=0, columnspan=3, pady=10)

        # Widget de texto para mostrar resultados
        self.txt = tk.Text(
            frame, height=8, state=tk.DISABLED,
            bg='#1e1e1e', fg='white', insertbackground='white', font=('Consolas', 10)
        )
        self.txt.grid(row=3, column=0, columnspan=3, sticky='nsew')
        frame.rowconfigure(3, weight=0)

    def _filter_list(self):
        """
        Actualiza la lista de síntomas según el término ingresado.
        """
        term = self.search_var.get().lower()
        self.filtered = [sym for sym in SYMPTOMS if term in sym]
        self._update_listbox()

    def _update_listbox(self):
        """
        Rellena el listbox con los síntomas filtrados.
        """
        self.listbox.delete(0, tk.END)
        for sym in self.filtered:
            # Mostrar de manera legible: reemplazar guiones por espacios y capitalizar
            display = sym.replace('_', ' ').capitalize()
            self.listbox.insert(tk.END, display)

    def _on_diagnose(self):
        """
        Obtiene selección de síntomas, consulta al controlador y muestra resultados.
        """
        selections = self.listbox.curselection()
        if not selections:
            messagebox.showwarning("Aviso", "Selecciona al menos un síntoma.")
            return
        # Traducir índices a nombres de síntomas normalizados
        chosen = [self.filtered[i].lower() for i in selections]
        ctrl.set_symptoms(chosen)
        results = ctrl.get_diagnostics(threshold=0.2)
        self._show_results(results)

    def _show_results(self, results):
        """
        Muestra los diagnósticos en el área de texto con formato.
        """
        self.txt.configure(state=tk.NORMAL)
        self.txt.delete('1.0', tk.END)
        if not results:
            self.txt.insert(tk.END, "No se encontró diagnóstico con el umbral establecido.\n")
        else:
            for enf, nivel in results:
                name = enf.replace('_', ' ').capitalize()
                confidence = nivel * 100
                line = f"{name:<30} {confidence:5.1f}%\n"
                self.txt.insert(tk.END, line)
        self.txt.configure(state=tk.DISABLED)

if __name__ == '__main__':
    # Arrancar la aplicación de forma independiente
    app = App()
    app.mainloop()
