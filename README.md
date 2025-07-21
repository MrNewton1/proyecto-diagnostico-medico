# Sistema Experto de Diagnóstico Médico

**Proyecto académico**: Agente inteligente de diagnóstico de enfermedades comunes  
**Autores**: Alan Isaac Pérez Hernández 336008878, Molina García Luis Alberto 17715680, Ethan David Ríos Beltrán 21257899.
**Fecha**: Julio 2025

---

## Descripción
Este sistema experto combina una **base de conocimientos en Prolog**, un **controlador en Python** y una **interfaz de escritorio** ligera con Tkinter para diagnosticar una amplia gama de enfermedades comunes.  
Mediante búsqueda y selección de síntomas generales, el usuario recibe posibles diagnósticos con un nivel de confianza basado en la proporción de síntomas coincidentes.

---

## Estructura del Proyecto
```
proyecto_diagnostico_medico/
├── kb/
│   └── kb.pl               # Base de conocimientos Prolog (enfermedades y síntomas)
├── src/
│   ├── controller.py       # Lógica de interacción Python ↔ Prolog
│   └── desktop/
│       └── main.py         # Interfaz de escritorio Tkinter (tema oscuro con búsqueda)
├── tests/
│   └── test_controller.py  # Pruebas unitarias con pytest
├── requirements.txt        # Dependencias Python
└── README.md               # Documentación del proyecto
```

---

## Requisitos Previos
- **Python 3.10+**  
- **SWI-Prolog** (verificar que `swipl` esté en el PATH)  
- **pip** y **virtualenv** para gestionar entornos Python

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
4. Verificar instalación de SWI-Prolog:
   ```bash
   swipl --version
   ```

---

## Uso

### 1. Línea de Comandos (CLI)
Probar el controlador directamente:
```bash
python3 src/controller.py sintoma1 sintoma2 --umbral 0.2
```
**Ejemplo**:
```bash
python3 src/controller.py fiebre cefalea polidipsia
```
Salida (JSON):
```json
[
  {"enfermedad": "diabetes_mellitus", "confianza": 0.4},
  {"enfermedad": "gripe", "confianza": 0.2}
]
```

### 2. Interfaz de Escritorio (Tkinter)
1. Activar entorno virtual:
   ```bash
   source .venv/bin/activate
   ```
2. Ejecutar la GUI:
   ```bash
   python3 src/desktop/main.py
   ```
3. Buscar o filtrar síntomas en la barra de búsqueda, seleccionar los deseados y pulsar **Diagnosticar**.

---

## Pruebas Unitarias
Se emplea **pytest** para garantizar la lógica del controlador:
```bash
pytest -q
``` 
Ejemplo en `tests/test_controller.py`:
```python
from src.controller import DiagnosticController

def test_diabetes():
    ctrl = DiagnosticController()
    ctrl.set_symptoms(['poliuria','polidipsia','polifagia'])
    res = dict(ctrl.get_diagnostics(threshold=0.3))
    assert 'diabetes_mellitus' in res
    assert res['diabetes_mellitus'] == pytest.approx(0.6)
```

---

## Arquitectura
1. **kb/kb.pl**: Hechos `sintoma(Enf, Sintoma)` para múltiples enfermedades y reglas de diagnóstico.
2. **src/controller.py**: Capa intermedia Python ↔ Prolog.
3. **src/desktop/main.py**: Interfaz de escritorio con tema oscuro, búsqueda de síntomas y resultados.

---

## Contribuciones
Las propuestas académicas y mejoras son bienvenidas. Por favor abre un _issue_ o envía un _pull request_.

---

## Licencia
Proyecto académico sin fines comerciales. Disponible bajo licencia MIT.
