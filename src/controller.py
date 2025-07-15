from pyswip import Prolog

class DiagnosticController:
    """
    Controlador para interactuar con la base de conocimientos Prolog.
    """
    def __init__(self, kb_path: str = 'kb.pl'):
        # Inicializa Prolog y carga la KB
        self.prolog = Prolog()
        self.prolog.consult(kb_path)

    def reset_symptoms(self):
        """
        Elimina todos los síntomas de usuario previamente almacenados.
        """
        list(self.prolog.query("retractall(usuario_sintoma(_))"))

    def set_symptoms(self, symptoms: list[str]):
        """
        Agrega los síntomas del usuario a la KB Prolog.
        symptoms: lista de strings, p.ej. ['fiebre_alta', 'tos_seca']
        """
        self.reset_symptoms()
        for s in symptoms:
            atom = s.lower().replace(' ', '_')
            self.prolog.assertz(f"usuario_sintoma({atom})")

    def get_diagnostics(self, threshold: float = 0.0) -> list[tuple[str, float]]:
        """
        Recupera pares (enfermedad, nivel de confianza).
        threshold: umbral mínimo de confianza (0.0-1.0).
        Devuelve lista de tuplas ordenadas de mayor a menor confianza.
        """
        results = []
        for sol in self.prolog.query("diagnostico(Enf, Nivel)"):
            enf = sol['Enf']
            nivel = float(sol['Nivel'])
            if nivel >= threshold:
                results.append((enf, nivel))
        results.sort(key=lambda x: x[1], reverse=True)
        return results


if __name__ == '__main__':
    import sys, json
    # Leer síntomas desde argumentos de línea de comandos:
    # python controller.py fiebre_alta tos_seca
    symptoms = sys.argv[1:]
    controller = DiagnosticController()
    controller.set_symptoms(symptoms)
    diagnostics = controller.get_diagnostics(threshold=0.2)
    # Imprimir JSON con diagnósticos
    output = [{'disease': d, 'confidence': c} for d, c in diagnostics]
    print(json.dumps(output, indent=2))
