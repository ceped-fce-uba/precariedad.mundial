import streamlit as st
import pandas as pd
from PIL import Image

def show_page_info():
    st.markdown("<h1 style='text-align: center;'>Proyecto Precariedad Mundial</h1>", unsafe_allow_html=True)
    st.markdown("### Centro de Estudios sobre Población, Empleo y Desarrollo (CEPED - IIEP – UBA)")
    
    # Logo and introduction
    col_intro1, col_intro2 = st.columns([1, 5])
    with col_intro1:
        try:
            logo = Image.open("app/logo_ceped.png")
            st.image(logo, width=200)
        except:
            st.info("Logo CEPED")
    
    with col_intro2:
        st.markdown(
        """
        <div style='text-align: justify'>
        El proyecto Precariedad Mundial tiene como objetivo aportar argumentos y evidencias empíricas 
        sobre las estructuras de los mercados de trabajo y la incidencia de la precariedad laboral en distintos países del mundo. Para ello, se homogeneizan microdatos provenientes de 
        encuestas de hogares oficiales creando un Dataset unificado, orientado a la generación de estadísticas comparables entre países.
        Así, se espera que el proyecto contribuya al intercambio sobre criterios y enfoques para procesar estadísticas laborales a nivel global.
        Esta aplicación sintetiza la metodología de construcción del dataset y permite visualizar diversas estadísticas derivadas del mismo.             
        </div>
        """,
        unsafe_allow_html=True
        )
    
    # Main sections with tabs
    tab1, tab2, tab3, tab4 = st.tabs([
        "📊 Sobre el Dataset", 
        "🔬 Metodología", 
        "📚 Publicaciones",
        "📈 Ejemplos de Uso"
    ])
    
    with tab1:
        show_project_info()
    
    with tab2:
        show_methodology()
    
    with tab3:
        show_publications()
    
    with tab4:
        show_examples()

def show_project_info():
    
    # Project description
    st.markdown("""
     
    ### Características del Dataset
    - **Cobertura**: 30 países (13 de América Latina, 15 de Europa, China y Estados Unidos)
    - **Período**: 2018-2019 (principalmente)
    - **Población objetivo**: Empleo urbano
    - **Variables principales**: 4 dimensiones de precariedad laboral + variables estructurales
    """)
    
        # Variables description
    st.subheader("Variables Principales")
    
    col1, col2 = st.columns(2)
    
    with col1:
        st.markdown("""
        **📊 Variables de Precariedad:**
        - **PRECAPT**: Part-time involuntario
        - **PRECAREG**: No registro laboral
        - **PRECATEMP**: Trabajo temporario
        - **PRECASEG**: Sin aportes a seguridad social
        
        **👥 Variables Demográficas:**
        - **SEXO**: Sexo
        - **EDAD**: Edad
        - **EDUC**: Nivel educativo
        """)
    
    with col2:
        st.markdown("""
        **💼 Variables Laborales:**
        - **CATOCUP**: Categoría ocupacional
        - **SECTOR**: Sector económico
        - **TAMA**: Tamaño del establecimiento
        - **CALIF**: Calificación del puesto
        
        **💰 Variables de Ingresos:**
        - **ING**: Ingreso en moneda local
        - **ING_PPA**: Ingreso en paridad de poder adquisitivo
        """)

    # Survey table
    st.subheader("Encuestas Procesadas por País")
    
    surveys_data = {
        'País': ['Argentina', 'Bolivia', 'Brasil', 'Chile', 'China', 'Colombia', 'Costa Rica', 
                'Ecuador', 'El Salvador', 'Estados Unidos', 'Europa*', 'Guatemala', 'México', 
                'Paraguay', 'Perú', 'Uruguay'],
        'Encuesta': ['EPH', 'ECE', 'PNAD Contínua', 'ENE-ESI', 'CHIP', 'GEIH', 'ENH',
                    'ENEMDU', 'ENH', 'CPS', 'Eurostat LFS', 'ENEI', 'ENOE', 'EPHC', 'ENAHO', 'ECH'],
        'Año': [2019, 2019, 2019, 2019, 2018, 2019, 2019, 2019, 2019, 2018, 2018, 2019, 2019, 2019, 2019, 2019]
    }
    
    df_surveys = pd.DataFrame(surveys_data)
    st.dataframe(df_surveys, use_container_width=True, hide_index=True)
    st.caption("*Europa incluye: Grecia, Polonia, Italia, Portugal, España, Austria, Reino Unido, Países Bajos, Noruega, Francia, Dinamarca, Suecia y Alemania")
    


def show_methodology():
    st.header("Aclaraciones Metodológicas")
    
    # General methodology
    st.subheader("📋 Decisiones Metodológicas Generales")
    
    with st.expander("Definición de Precariedad Laboral", expanded=True):
        st.markdown("""
        La noción de **"empleo precario"** no es unívoca en la literatura. En nuestra base de datos 
        recabamos **4 expresiones de la precariedad** que pueden evaluarse en la mayoría de países:
        
        1. **Trabajo part-time involuntario**: Trabajadores que desean más horas pero no pueden acceder
        2. **No registro de la relación laboral**: Ausencia de formalización del vínculo laboral
        3. **Trabajo de duración determinada**: Empleos con contratos temporarios
        4. **Falta de aportes a la seguridad social**: Sin protección social
        """)
    
    with st.expander("🔧 Criterios de Homogeneización"):
        st.markdown("""
        **Filtros aplicados:**
        - **Área geográfica**: Solo áreas urbanas (para mayor comparabilidad)
        - **Población objetivo**: Personas ocupadas únicamente
        - **Ocupación de referencia**: Ocupación principal
        
        **Variables estructurales clave:**
        - **Tamaño del establecimiento**: Pequeño (≤10), Mediano (11-49), Grande (≥50)
        - **Calificación del puesto**: Alta, Media, Baja (skill levels de Clasificación ISCO-08)
        """)
    
    # Country-specific notes
    st.subheader("🌍 Aclaraciones Específicas por Región")
    
    col1, col2 = st.columns(2)
    
    with col1:
        st.info("""
        **Europa (Eurostat LFS)**
        - Los cuentapropistas incluyen patrones
        - Información de ingresos complementada con Structure of Earnings Survey
        - Datos de Alemania corresponden a 2017
        """)
    
    with col2:
        st.warning("""
        **Limitaciones Generales**
        - No todas las variables están disponibles para todos los países
        - Algunos cortes de tamaño de establecimiento no son exactos
        - La imputación de ingresos para Europa tiene carácter exploratorio
        """)

def show_examples():
    st.header("Ejemplos de Uso del Dataset")
    
    st.markdown("""
    Esta sección presenta ejemplos prácticos de cómo utilizar la base de datos homogeneizada 
    para análisis comparativos internacionales.
    """)
    
    # Example 1: Employment structure
    st.subheader("🏢 Estructura del Empleo por Tamaño y Calificación")
    
    st.markdown("""
    El dataset permite caracterizar la estructura del mercado de trabajo combinando:
    - **Categoría ocupacional** (CATOCUP): Cuentapropista, Patrón, Asalariado
    - **Tamaño del establecimiento** (TAMA): Pequeño, Mediano, Grande  
    - **Calificación del puesto** (CALIF): Alta, Media, Baja
    """)
    
    with st.expander("💻 Código de ejemplo en R"):
        st.code("""
# Filtrar sector privado y crear perfiles ocupacionales
base_grupos <- base %>% 
  filter(CALIF %in% c("Alta","Media","Baja")) %>% 
  filter(SECTOR == "Priv", !is.na(CALIF), !is.na(TAMA)) %>%
  mutate(grupos = case_when(
    CATOCUP == "Cuenta propia" ~ paste0("Cuentapropista - ",CALIF),
    TRUE ~ paste0(TAMA, " - ",CALIF)
  ))

# Calcular participación por perfil ocupacional
pesos_perfiles <- base_grupos %>% 
  group_by(PAIS, grupos) %>% 
  summarise(casos_pond = sum(WEIGHT, na.rm = T)) %>% 
  group_by(PAIS) %>% 
  mutate(particip.ocup = casos_pond/sum(casos_pond))
        """, language="r")
    
    st.info("""
    **🔍 Principales hallazgos:**
    - América Latina: Mayor peso del empleo en establecimientos pequeños y calificación baja/media
    - Países desarrollados: Mayor participación en establecimientos grandes y alta calificación
    - El cuentapropismo tiene mayor relevancia en economías menos desarrolladas
    """)
    
    # Example 2: Precarity indicators
    st.subheader("⚠️ Indicadores de Precariedad para Asalariados")
    
    st.markdown("""
    Análisis de las expresiones de precariedad laboral en la población asalariada:
    """)
    
    with st.expander("💻 Código para calcular tasas de precariedad"):
        st.code("""
# Filtrar asalariados y calcular expresiones de precariedad
asalariados <- base %>% 
  filter(CATOCUP == "Asalariado")

expresiones_pais <- asalariados %>% 
  group_by(PAIS) %>% 
  summarise(
    part_time = sum(WEIGHT[PRECAPT==1], na.rm=T) / 
                sum(WEIGHT[PRECAPT %in% 0:1], na.rm=T),
    no_registro = sum(WEIGHT[PRECAREG==1], na.rm=T) / 
                  sum(WEIGHT[PRECAREG %in% 0:1], na.rm=T),
    no_seg_social = sum(WEIGHT[PRECASEG==1], na.rm=T) / 
                    sum(WEIGHT[PRECASEG %in% 0:1], na.rm=T),
    temporario = sum(WEIGHT[PRECATEMP==1], na.rm=T) / 
                 sum(WEIGHT[PRECATEMP %in% 0:1], na.rm=T)
  )
        """, language="r")
    
    st.success("""
    **💡 Recomendación:** Siempre ponderar por la variable WEIGHT para obtener 
    estimaciones estadísticamente representativas.
    """)

def show_publications():
    st.header("Publicaciones y Referencias")
    
    st.markdown("""
    Si utilizas información de este proyecto, te pedimos que cites alguna de nuestras publicaciones académicas:
    """)
    
    # Publications
    st.subheader("📖 Artículos Académicos")
    
    st.markdown("""
    ### 1. La calidad del empleo en la Argentina reciente
    **Autores:** J. Graña, G. Weksler, F. Lastra  
    **Revista:** Trabajo y Sociedad 38, 423-446  
    """)
    
    st.markdown("""
    ### 2. Calidad del empleo y estructura del mercado de trabajo en América Latina
    **Autores:** S. Fernández-Franco, J.M. Graña, F. Lastra, G. Weksler  
    **Revista:** Ensayos de Economía 32 (61), 124-151  
    """)
    
    # Data access
    st.subheader("📊 Acceso a los Datos")
    
    col1, col2 = st.columns(2)
    
    with col1:
        st.info("""
        **🔗 Repositorio GitHub**  
        [github.com/ceped-fce-uba/precariedad.mundial](https://github.com/ceped-fce-uba/precariedad.mundial)
        """)
    
    with col2:
        st.success("""
        **📥 Descarga Directa**  
        Dataset principal: `base_homogenea.RDS`  
        Metadatos: `Metadata.xlsx`
        """)
    
    # Contact information
    st.subheader("📧 Contacto")
    
    st.markdown("""
    **Centro de Estudios sobre Población, Empleo y Desarrollo (CEPED)**  
    Instituto Interdisciplinario de Economía Política (IIEP)  
    Universidad de Buenos Aires
    
    Para consultas, sugerencias o propuestas de colaboración, puedes contactarnos 
    a través del repositorio de GitHub creando un issue.
    """)

if __name__ == "__main__":
    show_page_info()
