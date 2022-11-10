import streamlit as st
import pandas as pd
import numpy as np

#DATA_URL = ("https://github.com/chairielazizi/streamlit-collision/blob/master/Motor_Vehicle_Collisions_-_Crashes.csv?raw=true")

DATA_URL1 = ("https://github.com/manuhanono/Proyecto/blob/main/crashes2.parquet?raw=true)


@st.cache(persist=True)
def load_data(rows):
    data = pd.read_parquet(DATA_URL1)
    data = data.head(rows)
    return data

data = load_data(1950000)


# for use with dropdown
original_data = data


st.title("Trabajo Final Proyecto I - NY Crashes")
st.markdown("Manuel Hanono y Bruno Soifer.")

st.sidebar.header("Filtrar por:")

# Create a list of possible values and multiselect menu with them in it.
YEARS = data['YEAR'].unique()
with st.sidebar:
    container = st.container()
    all = st.checkbox("Todos los años")    
    if all:
        YEARS_SELECTED = container.multiselect("Seleccionar uno o mas años:", YEARS,YEARS)
    else:
        YEARS_SELECTED =  container.multiselect("Seleccionar uno o mas años:", YEARS)
# Mask to filter dataframe
mask_years = data['YEAR'].isin(YEARS_SELECTED)
data = data[mask_years]

# Mismo filtro para los barrios (ver si vale la pena)
# BOROUGH = data['BOROUGH'].unique()
# BOROUGH_SELECTED = st.sidebar.multiselect('Barrios:', BOROUGH, default=BOROUGH)
# mask_borough = data['BOROUGH'].isin(BOROUGH_SELECTED)
# data = data[mask_borough]

if st.checkbox("Visualizar Datos Crudos",False):
    st.subheader("Datos Crudos")
    st.write(data.tail(10))
