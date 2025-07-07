import streamlit as st
import pandas as pd
import stream_distrib
import stream_preca
import stream_salarios
import stream_metadata
import stream_info
from streamlit_option_menu import option_menu


# Page configurations
st.set_page_config(page_title="Precariedad Mundial", 
                   page_icon="🌍", 
                   layout="wide")

page_selection = option_menu(
            None,  # No menu title
            ["Información", "Distribucion empleo","Tasas de precariedad",  "Salarios","Metadatos"],  # Menu options
            icons=["info-circle", "pie-chart","bar-chart-steps", "arrow-up-circle","file-text"],  # Icons for each option
            menu_icon="cast",  
            default_index=0, 
            orientation="horizontal")

if page_selection == "Información":
    stream_info.show_page_info()
elif page_selection == "Distribucion empleo":
    stream_distrib.show_page_distrib()
elif page_selection == "Tasas de precariedad":
    stream_preca.show_page_preca()
elif page_selection == "Salarios":
    stream_salarios.show_page_salarios()
elif page_selection == "Metadatos":
    stream_metadata.show_page_metadata()
