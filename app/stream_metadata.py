import streamlit as st
import pandas as pd
from PIL import Image

def fetch_metadata():
    diccionario = pd.read_excel('app/data/Metadata.xlsx', sheet_name='Diccionario')
    homogeneizacion = pd.read_excel('app/data/Metadata.xlsx', sheet_name='Homogeneizacion')
    return diccionario, homogeneizacion
 
def show_page_metadata():
    diccionario, homogeneizacion = fetch_metadata()
    st.markdown("<h1 style='text-align: center;'>Metadatos del Proyecto</h1>", unsafe_allow_html=True)
    st.markdown("### Variables y Metodología de Homogeneización")
    
    # Introduction
    st.markdown("""
    Esta sección contiene la documentación técnica completa sobre las variables incluidas en la base de datos 
    homogeneizada y los criterios utilizados para armonizar la información proveniente de las encuestas de 
    hogares de diferentes países.
    
    La base de datos **Precariedad Mundial** unifica información de **16+ países** utilizando **33 variables** 
    clave que permiten el análisis comparativo de las condiciones laborales a nivel internacional.
    """)
    
    # Tabs for better organization
    tab1, tab2 = st.tabs(["📖 Diccionario de Variables", "🔄 Homogeneización por País"])
    
    with tab1:
        show_dictionary_section(diccionario)
    
    with tab2:
        show_homogenization_section(homogeneizacion)

def show_dictionary_section(diccionario):
    st.header("📖 Diccionario de Variables")
    
    st.markdown("""
    El diccionario describe todas las variables incluidas en la base de datos homogeneizada, 
    especificando su tipo, descripción y valores posibles.
    """)
    
    # Filters for dictionary
    col1, col2 = st.columns(2)
    
    with col1:
        # Variable type filter
        tipos_disponibles = ['Todas'] + sorted(diccionario['tipo'].dropna().unique().tolist())
        tipo_seleccionado = st.selectbox("🔍 Filtrar por tipo de variable", tipos_disponibles)
    
    with col2:
        # Variable name filter
        variables_disponibles = ['Todas'] + sorted(diccionario['variable'].unique().tolist())
        variable_seleccionada = st.selectbox("📊 Filtrar por variable específica", variables_disponibles)
    
    # Apply filters
    df_filtrado = diccionario.copy()
    
    if tipo_seleccionado != 'Todas':
        df_filtrado = df_filtrado[df_filtrado['tipo'] == tipo_seleccionado]
    
    if variable_seleccionada != 'Todas':
        df_filtrado = df_filtrado[df_filtrado['variable'] == variable_seleccionada]
    
    # Show summary statistics
#    col_stats1, col_stats2, col_stats3 = st.columns(3)
#    with col_stats1:
#        st.metric("Variables Únicas", len(diccionario['variable'].unique()))
#    with col_stats2:
#        st.metric("Tipos de Datos", len(diccionario['tipo'].dropna().unique()))
#    with col_stats3:
#        st.metric("Registros Mostrados", len(df_filtrado))
    
    # Variable categories explanation
    with st.expander("Categorías de las Variables", expanded=False):
        st.markdown("""
        **Variables Demográficas:**
        - **PAIS**: País de la encuesta
        - **SEXO**: Sexo del encuestado
        - **EDAD**: Edad en años cumplidos
        - **EDUC**: Nivel educativo máximo alcanzado
        
        **Variables Laborales:**
        - **CATOCUP**: Categoría ocupacional (Patrón, Asalariado, Cuenta Propia, Resto)
        - **SECTOR**: Sector económico (Público, Privado, Servicio Doméstico)
        - **TAMA**: Tamaño del establecimiento (Pequeño ≤10, Medio 10-49, Grande ≥50)
        - **CALIF**: Calificación del puesto según CIUO-08
        
        **Variables de Precariedad (0=No, 1=Sí):**
        - **PRECAPT**: Part-time involuntario
        - **PRECAREG**: No registro laboral
        - **PRECATEMP**: Trabajo temporario
        - **PRECASEG**: Sin aportes a seguridad social
        
        **Variables de Ingresos:**
        - **ING**: Ingreso en moneda local
        - **ING_PPA**: Ingreso en paridad de poder adquisitivo
        
        **Variables Técnicas:**
        - **WEIGHT**: Ponderador principal
        - **WEIGHT_W**: Ponderador específico para ingresos
        - **ANO**: Año de referencia
        - **PERIODO**: Período de la encuesta
        """)
    
    # Display filtered data
    st.subheader("📋 Variables en la Base de Datos")
    st.dataframe(
        df_filtrado, 
        use_container_width=True,
        column_config={
            "variable": st.column_config.TextColumn("Variable", width="small"),
            "tipo": st.column_config.TextColumn("Tipo", width="small"),
            "descripcion": st.column_config.TextColumn("Descripción", width="large"),
            "valores": st.column_config.TextColumn("Valores", width="small"),
            "referencia": st.column_config.TextColumn("Referencia", width="medium")
        },
        hide_index=True
    )

def show_homogenization_section(homogeneizacion):
    st.header("🔄 Proceso de Homogeneización")
    
    st.markdown("""
    Esta sección detalla cómo las preguntas y categorías originales de cada encuesta nacional 
    fueron reclasificadas para crear variables comparables entre países.
    """)
    
    # Filters for homogenization
    col1, col2, col3 = st.columns(3)
    
    with col1:
        # Country filter
        paises_disponibles = ['Todos'] + sorted(homogeneizacion['Pais'].unique().tolist())
        pais_seleccionado = st.selectbox("🌍 Filtrar por país", paises_disponibles)
    
    with col2:
        # Variable filter
        variables_homog = ['Todas'] + sorted(homogeneizacion['Variable dataframe'].dropna().unique().tolist())
        variable_homog = st.selectbox("📊 Variable del dataframe", variables_homog)
    
    with col3:
        # Survey filter
        encuestas_disponibles = ['Todas'] + sorted(homogeneizacion['Encuesta'].unique().tolist())
        encuesta_seleccionada = st.selectbox("📋 Filtrar por encuesta", encuestas_disponibles)
    
    # Apply filters
    df_homog_filtrado = homogeneizacion.copy()
    
    if pais_seleccionado != 'Todos':
        df_homog_filtrado = df_homog_filtrado[df_homog_filtrado['Pais'] == pais_seleccionado]
    
    if variable_homog != 'Todas':
        df_homog_filtrado = df_homog_filtrado[df_homog_filtrado['Variable dataframe'] == variable_homog]
    
    if encuesta_seleccionada != 'Todas':
        df_homog_filtrado = df_homog_filtrado[df_homog_filtrado['Encuesta'] == encuesta_seleccionada]
    
    # Summary statistics
    col_stats1, col_stats2, col_stats3, col_stats4 = st.columns(4)
    with col_stats1:
        st.metric("Países", len(homogeneizacion['Pais'].unique()))
    with col_stats2:
        st.metric("Encuestas", len(homogeneizacion['Encuesta'].unique()))
    with col_stats3:
        st.metric("Variables", len(homogeneizacion['Variable dataframe'].dropna().unique()))
    with col_stats4:
        st.metric("Registros Mostrados", len(df_homog_filtrado))
    
    # Methodology explanation
    with st.expander("🔬 Criterios de Homogeneización", expanded=False):
        st.markdown("""
        **Principios Metodológicos:**
        
        1. **Comparabilidad**: Priorizar categorías que permitan comparación entre países
        2. **Representatividad**: Mantener la representatividad estadística de cada encuesta
        3. **Consistencia**: Aplicar criterios uniformes de reclasificación
        4. **Transparencia**: Documentar todos los procedimientos de transformación
        
        **Proceso de Reclasificación:**
        - Mapeo de categorías originales a categorías estándar
        - Validación cruzada entre países similares  
        - Revisión de consistencia temporal
        - Documentación de excepciones y limitaciones
        
        **Limitaciones:**
        - No todas las variables están disponibles en todos los países
        - Algunos cortes no son exactamente comparables
        - Diferencias en los períodos de referencia de las encuestas
        """)
    
    # Example of homogenization
    if not df_homog_filtrado.empty:
        st.subheader("📋 Mapeo de Variables por País")
        
        # Show a clean view of the homogenization process
        columns_to_show = ['Pais', 'Encuesta','Variable original','Etiqueta', 'Variable dataframe','Reclasificación']
        df_display = df_homog_filtrado[columns_to_show].copy()
        
        st.dataframe(
            df_display,
            use_container_width=True,
            column_config={
                "Pais": st.column_config.TextColumn("País", width="small"),
                "Encuesta": st.column_config.TextColumn("Encuesta", width="small"),
                "Variable original": st.column_config.TextColumn("Variable Original", width="small"),
                "Etiqueta": st.column_config.TextColumn("Categoría Original", width="large"),
                "Variable dataframe": st.column_config.TextColumn("Variable", width="small"),
                "Reclasificación": st.column_config.TextColumn("Reclasificación", width="small")
            },
            hide_index=True
        )
        
        # Show example for a specific variable if available
        if variable_homog != 'Todas':
            st.info(f"""
            **💡 Ejemplo de Homogeneización para {variable_homog}:**
            
            Esta tabla muestra cómo las categorías originales de cada encuesta nacional 
            fueron reclasificadas hacia la variable estándar **{variable_homog}** en el dataframe unificado.
            """)
    else:
        st.warning("No se encontraron registros con los filtros seleccionados.")
    
    # Download option
    st.subheader("📥 Descargar Metadatos")
    st.markdown("""
    Para acceder al archivo completo de metadatos con todas las especificaciones técnicas, 
    puedes descargar el archivo **Metadata.xlsx** desde el repositorio principal del proyecto.
    """)

if __name__ == "__main__":
    show_page_metadata()