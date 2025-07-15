# Sistema Experto de Diagnóstico Médico

**Proyecto académico**: Agente inteligente de diagnóstico de Infecciones Respiratorias Agudas
**Autor**: Isaac
**Fecha**: Julio 2025

---

## Descripción

Este sistema experto combina una **base de conocimientos en Prolog**, un **controlador en Python** y una **interfaz web** basada en Flask para diagnosticar enfermedades respiratorias agudas.
El prototipo permite al usuario seleccionar síntomas comunes y retorna posibles diagnósticos con un nivel de confianza calculado según la proporción de síntomas coincidentes.

---

## Estructura del Proyecto

```
proyecto_diagnostico_medico/
├── kb/
│   └── kb.pl               # Base de conocimientos Prolog
├── src/
│   ├── app.py              # Aplicación web Flask
│   ├── controller.py       # Controlador Python intermedio
│   └── templates/
│       └── index.html      # Plantilla Jinja2 con Bootstrap
├── tests/
│   └── test_controller.py  # Pruebas unitarias con pytest
├── requirements.txt        # Dependencias Python
└── README.md               # Documentación del proyecto
```

---

## Requisitos Previos

* **Python 3.10+**
* **SWI‑Prolog** (asegúrate de que `swipl` esté en el PATH)
* **pip** para gestionar paquetes Python

---

## Instalación y Configuración

1. Clonar el repositorio:

   ```bash
   git clone https://github.com/tu_usuario/proyecto_diagnostico_medico.git
   cd proyecto_diagnostico_medico
   ```
2. Crear y activar un entorno virtual:

   ```bash
   python3 -m venv .venv
   source .venv/bin/activate   # macOS/Linux
   .venv\Scripts\activate    # Windows
   ```
3. Instalar dependencias:

   ```bash
   pip install -r requirements.txt
   ```
4. Verificar instalación de SWI‑Prolog:

   ```bash
   swipl --version
   ```

---

## Uso

### 1. Línea de Comandos (CLI)

Proporciona una forma rápida de probar el controlador:

```bash
python3 src/controller.py fiebre_alta tos_seca --umbral 0.2
```

Salida esperada (JSON):

```json
[
  {"enfermedad": "resfriado_comun", "confianza": 0.4}
]
```

### 2. Interfaz Web

1. Establecer la variable de entorno (opcional):

   ```bash
   export FLASK_ENV=development
   ```
2. Ejecutar el servidor Flask:

   ```bash
   flask run
   ```
3. Abrir en el navegador: `http://localhost:5000`
4. Seleccionar síntomas y pulsar **Diagnosticar**.

---

## Pruebas Unitarias

Se utilizan **pytest** para validar la lógica del controlador.

```bash
pytest -q
```

Ejemplo de prueba (`tests/test_controller.py`):

```python
from src.controller import DiagnosticController

def test_resfriado():
    ctrl = DiagnosticController()
    ctrl.set_symptoms(['congestion_nasal','goteo_nasal'])
    resultados = dict(ctrl.get_diagnostics(threshold=0.2))
    assert 'resfriado_comun' in resultados
    assert resultados['resfriado_comun'] == 0.4
```

---

## Arquitectura

1. **kb/kb.pl**: Hechos `sintoma(Enf,Sintoma)` y reglas `diagnostico/2`, `diagnostico_confianza/3`.
2. **src/controller.py**: Carga la KB en Prolog, maneja síntomas dinámicos y extrae diagnósticos.
3. **src/app.py**: Servidor Flask que renderiza la plantilla y maneja la interacción de usuario.
4. **src/templates/index.html**: Interfaz responsiva con Bootstrap.

---

## Contribuciones

Las contribuciones académicas son bienvenidas. Para sugerencias o mejoras, crea un *issue* o envía un *pull request*.

---

## Licencia

Este proyecto es parte de un ejercicio académico y se distribuye sin fines comerciales.
