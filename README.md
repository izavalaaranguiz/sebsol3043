## Percepciones de justicia salarial: convergencia Este-Oeste en Alemania

Presentación del curso **Sociología del Estado de Bienestar (SOL3043)**, basada en el artículo:

📄 Adriaans, J., & Sauer, C. (2026). The East–West Gap in Pay Fairness Perceptions Converged Between 2009 and 2023. *Kölner Zeitschrift für Soziologie und Sozialpsychologie*, 78, 413–429. https://doi.org/10.1007/s11577-026-01086-5

📈 [Presentación](https://izavalaaranguiz.github.io/sebsol3043/presentacion_seb/pres_sauer_seb.html)


## Del estallido al Rechazo: cambio y estabilidad de valoraciones hacia políticas indígenas en Chile, 2016–2023

Informe de Investigación para **Sociología del Estado de Bienestar (SOL3043)**

🔎 **Resumen**: En este trabajo estudiamos en qué medida las valoraciones de políticas indígenas muestran cambio persistente o estabilidad, y cómo varía según el tipo de política. Para esto, examinamos el caso de Chile durante el ciclo contencioso del estallido social, la Convención Constitucional y el plebiscito de 2022, que instaló en la agenda pública la discusión por políticas en favor de la población indígena. Usando el *Estudio Longitudinal de Relaciones Interculturales* (*N* = 1.589; 2016-2023), comparamos modelos de actualización activa (AUM) y de disposiciones estables (SDM), y estimamos un modelo quasi-simplex (QSM) que separa el cambio verdadero del error de medición. Mostramos que el modelo SDM con interceptos libres se ajusta mejor en cuatro de cinco ítems evaluados. Las políticas debatidas en la Convención (escaños, autonomía y restitución territoriales) muestran una alta estabilidad antes del estallido y desplazamientos transitorios del nivel en 2021, que tienden a revertirse hacia niveles pre-estallido en 2023, mientras que educación y empleo muestran una respuesta menor o nula. Concluimos que los cambios observados fueron mayoritariamente transitorios: las desviaciones del patrón de estabilidad se concentraron en políticas de alta saliencia durante el proceso constituyente y se disiparon en gran medida tras el Rechazo de la propuesta.



📈 [Presentación](https://izavalaaranguiz.github.io/sebsol3043/elri_seb/pres_estabilidad_politicas_indigenas.html)

📄 [Informe](https://izavalaaranguiz.github.io/sebsol3043/elri_seb/04_informe/build/informe_seb_IZA.pdf)

### Replicación

Los materiales de replicación están en `elri_seb/`:

| Carpeta | Contenido |
|:--|:--|
| `01_input/` | Base de datos del ELRI (`BBDD_ELRI_LONG_4.0.dta`, olas 1 a 4), manual metodológico y diccionario de variables. |
| `02_scripts/` | `analisis_estabilidad_politicas_indigenas.R`: limpieza, Figura 1 y estimación de los modelos SDM, AUM y QSM. |
| `03_output/` | Figura y tablas que genera el script. |
| `04_informe/` | Código LaTeX, referencias y PDF del informe. |

Para reproducir los resultados:

1. Abrir base en `elri_seb/01_input/`.
2. Instalar los paquetes de R: `install.packages(c("tidyverse", "haven", "lavaan"))`.
3. Fijar el directorio de trabajo en `elri_seb/02_scripts/` y correr el script completo.
4. El script guarda en `03_output/` la Figura 1 (`fig_medias_por_ola.png`), la tabla de ajuste de los modelos (`tabla_ajuste_modelos.csv`) y la tabla del QSM (`tabla_qsm.csv`).



## Autor

Ignacio Zavala Aránguiz — SOL3043, 2026.
