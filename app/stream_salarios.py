import streamlit as st
import pandas as pd

def fetch_data_salarios():
    df = pd.read_csv('app/data/salarios_categoria.csv')
    df['categoria'] = pd.Categorical(df['categoria'], categories=pd.unique(df['categoria']), ordered=True)
    df2 = pd.read_csv('app/data/salarios_categoria2.csv')
    df2['categoria'] = pd.Categorical(df2['categoria'], categories=pd.unique(df2['categoria']), ordered=True)
    
    return df, df2

def show_page_salarios():
    dframe, dframe2 = fetch_data_salarios()
    unique_categorias = list(set(dframe.variable_interes))
    unique_paises = sorted(list(set(dframe.PAIS)))
    variables_salario_dict = {
        'sal_prom': 'Salario promedio (ponderado)',
        'sal_median': 'Mediana del salario (muestral)', 
    }

    st.title("Salarios en Paridad de Poder Adquisitivo")
    # Sidebar for filters
    with st.sidebar:
        st.header("Filtros")
        categoria = st.radio("🎯 Elegí una categoría", unique_categorias)
        eleccion = st.radio("Desagregar por una segunda variable?", ("No", "Si"))
        if eleccion == "Si":
            categorias2 = st.radio("Elegir segunda categoria", unique_categorias, index=1)
        salario_key = st.radio("📈 Cambiar la estimación del salario", 
                            list(variables_salario_dict.keys()),
                            format_func=lambda x: variables_salario_dict[x])
        
        # Button to select all countries
        if st.button("Seleccionar todos los países"):
            st.session_state.paises_seleccionados_salario = unique_paises
        
        paises_seleccionados = st.multiselect(
            "Seleccionar países", 
            unique_paises, 
            default=st.session_state.get('paises_seleccionados_salario', unique_paises)
        )
        
        # Update session state
        st.session_state.paises_seleccionados_salario = paises_seleccionados
    
    if eleccion == "No":
        df_filtrado = dframe[(dframe.variable_interes == categoria) & (dframe.PAIS.isin(paises_seleccionados))]
        st.write(f"Salario promedio según: {categoria}")
    else:
        combined = f"{categoria}-{categorias2}"
        combined_reverse = f"{categorias2}-{categoria}"
        df_filtrado = dframe2[(dframe2.variable_interes.isin([combined, combined_reverse])) & (dframe2.PAIS.isin(paises_seleccionados))]
        st.write(f"alario promedio según: {categoria} y {categorias2}")

    st.markdown(f"*Variable analizada: {variables_salario_dict[salario_key]}. Estimado en dolares de paridad de poder adquisitivo de 2017*")
    
    chart_data = pd.DataFrame(
        {
        "pais": df_filtrado["PAIS"],
        "salario": df_filtrado[salario_key],
        "categoria": df_filtrado["categoria"],
        }
        )
    
    st.bar_chart(chart_data, x="pais", y="salario", color="categoria",stack=False)


if __name__ == "__main__":
    while True:
        show_page_salarios()