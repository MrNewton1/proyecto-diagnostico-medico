"""
controller.py - Controlador para el sistema experto de diagnóstico médico
Proyecto académico: Agente inteligente de diagnóstico de Infecciones Respiratorias Agudas
Autor: Isaac
Fecha: Julio 2025

Descripción:
    Este módulo define la clase DiagnosticController, que sirve como capa intermedia
    entre la base de conocimientos Prolog (kb.pl) y la aplicación Python. Proporciona
    métodos para:
      - Cargar y validar la KB en Prolog.
      - Gestionar síntomas reportados por el usuario.
      - Consultar diagnósticos con un nivel de confianza basado en coincidencia de síntomas.

"""

import os
from pyswip import Prolog

class DiagnosticController:
    """
    Controlador para interactuar con la base de conocimientos Prolog.

    Métodos principales:
        __init__(kb_path)
        reset_symptoms()
        set_symptoms(symptoms)
        get_diagnostics(threshold)
    """

    def __init__(self, kb_path: str = None):
        """
        Inicializa el intérprete Prolog y carga la base de conocimientos.

        Parámetros:
            kb_path (str): Ruta absoluta o relativa al archivo kb.pl. Si es None,
                           utiliza la ruta por defecto '../kb/kb.pl' relativa a este módulo.

        Excepciones:
            FileNotFoundError: Si el archivo KB no existe en la ruta especificada.
            Exception: Si ocurre un error al consultar la KB en Prolog.
        """
        # Determinación de la ruta de la KB
        if kb_path is None:
            base_dir = os.path.dirname(os.path.abspath(__file__))  # Directorio src/
            kb_path = os.path.normpath(os.path.join(base_dir, os.pardir, 'kb', 'kb.pl'))

        # Verificación de existencia del fichero KB
        print(f"🔍 Comprobando KB en: {kb_path}")
        if not os.path.isfile(kb_path):
            raise FileNotFoundError(f"¡KB no encontrada en {kb_path}!")

        # Inicialización de Prolog y carga de la KB
        self.prolog = Prolog()
        try:
            self.prolog.consult(kb_path)  # Carga de reglas y hechos
            print("✅ KB cargada con éxito")
        except Exception as e:
            # Registro de error para depuración académica
            print(f"❌ Error al cargar KB: {e}")
            raise

    def reset_symptoms(self):
        """
        Limpia el conjunto dinámico de síntomas asertados en Prolog.

        Esta operación elimina todos los hechos usuario_sintoma/1,
        dejando la KB en su estado inicial sin síntomas reportados.
        """
        # Retractar todos los síntomas asertados previamente
        list(self.prolog.query("retractall(usuario_sintoma(_))"))

    def set_symptoms(self, symptoms: list[str]):
        """
        Aserta en Prolog cada síntoma reportado por el usuario.

        Parámetros:
            symptoms (list[str]): Lista de nombres de síntoma, por ejemplo ['fiebre_alta', 'tos_seca'].

        Flujo:
            1. Se limpia el estado previo con reset_symptoms().
            2. Se construye atom adecuadamente y se aserta cada hecho usuario_sintoma(atom).
        """
        # Reiniciar antes de asertar nuevos síntomas
        self.reset_symptoms()
        for s in symptoms:
            atom = s.lower().replace(' ', '_')
            # Asentar el síntoma en la KB Prolog
            self.prolog.assertz(f"usuario_sintoma({atom})")

    def get_diagnostics(self, threshold: float = 0.0) -> list[tuple[str, float]]:
        """
        Consulta la base de conocimientos para obtener diagnósticos.

        Parámetros:
            threshold (float): Valor mínimo de confianza [0.0, 1.0] para filtrar resultados.

        Retorna:
            List[Tuple[str, float]]: Lista de tuplas (enfermedad, nivel_de_confianza),
                                     ordenadas de mayor a menor confianza.
        """
        # Opcional: imprimir todas las soluciones crudas para análisis académico
        raw = list(self.prolog.query("diagnostico(Enf, Nivel)"))
        print("🐛 Prolog raw solutions:", raw)

        results: list[tuple[str, float]] = []
        for sol in raw:
            enf = sol.get('Enf')
            nivel_raw = sol.get('Nivel')
            try:
                nivel = float(nivel_raw)
            except (TypeError, ValueError):
                # Omitir registros no numéricos
                continue
            # Filtrar según umbral
            if nivel >= threshold:
                results.append((enf, nivel))
        # Ordenar de mayor a menor confianza
        results.sort(key=lambda x: x[1], reverse=True)
        return results

if __name__ == '__main__':
    # Modo CLI para pruebas rápidas desde terminal
    import argparse, json

    parser = argparse.ArgumentParser(
        description='CLI: Diagnóstico médico por síntomas')
    parser.add_argument(
        'sintomas', nargs='+', help='Lista de síntomas (e.g. fiebre_alta tos_seca)')
    parser.add_argument(
        '--umbral', '-u', type=float, default=0.2,
        help='Umbral mínimo de confianza (0–1)')
    args = parser.parse_args()

    # Instancia del controlador y ejecución de la consulta
    controller = DiagnosticController()
    controller.set_symptoms(args.sintomas)
    resultados = controller.get_diagnostics(threshold=args.umbral)

    # Salida JSON formateada para uso académico
    print(json.dumps(
        [{'enfermedad': e, 'confianza': round(c, 3)} for e, c in resultados],
        indent=2, ensure_ascii=False
    ))
