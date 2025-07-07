import streamlit as st
import pandas as pd
from PIL import Image

def fetch_data_preca():
    df = pd.read_csv('app/data/precariedad_categoria.csv')
    #df = pd.read_csv('data/precariedad_categoria.csv')    
    return df

def show_page_preca():
    dframe = fetch_data_preca()
    unique_categorias = list(set(dframe.variable_interes))
    unique_paises = sorted(list(set(dframe.PAIS)))
    
    # Variables de precariedad con nombres descriptivos
    variables_preca_dict = {
        'tasa_part': 'Trabajo part-time involuntario',
        'tasa_seg': 'Falta de aportes a la seguridad social', 
        'tasa_reg': 'No registro de la relación laboral',
        'tasa_temp': 'Trabajo temporario'
    }
    col0a, col0b = st.columns([6,2])
    with col0a:
        st.title("Tasas de precariedad por país")
        st.markdown("""
        Esta sección permite seleccionar una de las dimensiones de la precariedad laboral y explorar su incidencia en cada país, de manera agregado o aplicando una variable de corte.
        """)
    with col0b:
        st.markdown("### 🔍 Variables de Precariedad Laboral")
        with st.expander("📖 Estas son las dimensiones de la precariedad laboral que analizamos", expanded=False):
            st.markdown("""
            **🕒 Trabajo part-time involuntario (PRECAPT):**  
            Mide la proporción de trabajadores que desean trabajar más horas pero no pueden hacerlo
            
            **🏛️ No registro de la relación laboral (PRECAREG):**  
            Indica el porcentaje de trabajadores asalariados cuya relación laboral no está registrada 
            formalmente
            
            **⏰ Trabajo temporario (PRECATEMP):**  
            Representa la proporción de empleos con contratos de duración determinada
            
            **🛡️ Falta de aportes a la seguridad social (PRECASEG):**  
            Mide el porcentaje de trabajadores que no reciben aportes a sistemas de seguridad social
            """)
    
    # Sidebar for filters
    with st.sidebar:
        st.header("Filtros")
        categoria = st.radio("🎯 Elegí una categoría", unique_categorias)
        preca_key = st.radio("📈 Elegí una variable de precariedad", 
                            list(variables_preca_dict.keys()),
                            format_func=lambda x: variables_preca_dict[x])
        
        # Button to select all countries
        if st.button("Seleccionar todos los países"):
            st.session_state.paises_seleccionados_preca = unique_paises
        
        paises_seleccionados = st.multiselect(
            "Seleccionar países", 
            unique_paises, 
            default=st.session_state.get('paises_seleccionados_preca', unique_paises)
        )
        
        # Update session state
        st.session_state.paises_seleccionados_preca = paises_seleccionados
    
    df_filtrado = dframe[(dframe.variable_interes == categoria) & (dframe.PAIS.isin(paises_seleccionados))]
    st.markdown(f"### Tasa de precariedad laboral según: **{categoria}**")
    st.markdown(f"*Variable analizada: {variables_preca_dict[preca_key]}*")
    
    chart_data = pd.DataFrame(
        {
        "pais": df_filtrado["PAIS"],
        "tasa": df_filtrado[preca_key],
        "categoria": df_filtrado["categoria"],
        }
        )
    st.bar_chart(chart_data, x="pais", y="tasa", color="categoria",stack=False)

if __name__ == "__main__":
    while True:
        show_page_preca()