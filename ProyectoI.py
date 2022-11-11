import streamlit as st
import pandas as pd
import numpy as np
import pydeck as pdk
import plotly.express as px
from datetime import datetime
import plotly.figure_factory as ff

#DATA_URL = ("https://github.com/chairielazizi/streamlit-collision/blob/master/Motor_Vehicle_Collisions_-_Crashes.csv?raw=true")

DATA_URL1 = ("https://github.com/manuhanono/Proyecto/blob/main/crashes2.parquet?raw=true")


@st.cache(persist=True)
def load_data():
    data = pd.read_parquet(DATA_URL1)
    return data

data = load_data()

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

st.header("Where are the most people injured in NYC?")
#injured_people = st.slider("Number of persons injured in NYC",0,19)
st.map(data.query("lat <= 41 & lat > 39 & lon > -80 & lon < -70")[['lat', 'lon']].dropna(how="any"))

st.header("Preg 1")
fig = px.histogram(data, x="WEEKDAY", y="CANT", color = "PINJ")
st.plotly_chart(fig, use_container_width=True)

fig2 = px.bar(data, x="WEEKDAY", y="CANT", color="BOROUGH", barmode="group", facet_col="BOROUGH")
st.plotly_chart(fig2, use_container_width = True)

st.header("AA")
st.bar_chart(data=data, x="PINJ", y="BOROUGH")
# make a dropdown search
st.header("Top 5 dangerous streets affected by types")
select = st.selectbox("Affected by type of people", ['Pedestrians', 'Cyclists', 'Motorists'])

if select == 'Pedestrians':
    st.write(original_data.query("injured_pedestrians >= 1")[['on_street_name', 'injured_pedestrians']].sort_values(by=['injured_pedestrians'], ascending=False).dropna(how='any')[:5])
elif select == 'Cyclists':
    st.write(original_data.query("injured_cyclists >= 1")[['on_street_name', 'injured_cyclists']].sort_values(by=['injured_cyclists'], ascending=False).dropna(how='any')[:5])
elif select == 'Motorists':
    st.write(original_data.query("injured_motorists >= 1")[['on_street_name', 'injured_motorists']].sort_values(by=['injured_motorists'], ascending=False).dropna(how='any')[:5])

