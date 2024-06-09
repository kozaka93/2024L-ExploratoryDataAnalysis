import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
import dash
from dash import dcc, html
from dash.dependencies import Input, Output
import geopandas as gpd
import dash_bootstrap_components as dbc
from bin import create_figure

ofiary_df_woj = pd.read_csv("./data/ofiary_woj.csv")
ofiary_df_pow = pd.read_csv("./data/ofiary_pow.csv")
ofiary_mean = pd.read_csv("./data/ofiary_mean.csv")

wojewodztwa_geojson = gpd.read_file("./geodata/wojewodztwa-medium.geojson")
powiaty_geojson = gpd.read_file("./geodata/powiaty-medium.geojson")

df = pd.read_csv("./data/europa.csv")

custom_colorscale = ["#c6ebbe", "#a9dbb8", "#7ca5b8", "#38369a", "#020887"]

# Aplikacja Dash
app = dash.Dash(__name__, external_stylesheets=[dbc.themes.CERULEAN])

app.layout = html.Div([
    html.H1('Wypadki drogowe w Polsce', style={'text-align': 'center'}),
    html.Div('''W naszej aplikacji chcieliśmy zwizualizować dane dotyczące wypadków w ruchu lądowym w Polsce.
                Korzystaliśmy przy tym z danych pochodzących z GUSu oraz Eurostatu.''',
                style={'text-align': 'left', 'padding': '12px'}),
    html.Div('''W zakładce "Wypadki w Polsce w liczbach" znajdziesz mapę Polski, na której możesz zobaczyć ilość wypadków,
            rannych i ofiar śmiertelnych w poszczególnych województwach. 
            Dodatkowo, w zakładce tej znajdziesz wykres, który pozwala porównać wybrane powiaty oraz tabelę z danymi
            dla województw. W zakładce "Porównanie Polski z krajami Unii Europejskiej" znajdziesz wykres, który pozwala porównać Polskę
            na tle innych krajów w kwestiach takich jak spożycie alkoholu, śmiertelność, czy inwestycje w drogi.''',
            style={'text-align': 'left', 'padding': '12px'}),
    dcc.Tabs(id='tabs', value='tab-1', children=[
        dcc.Tab(label='Wypadki w Polsce w liczbach', value='tab-1'),
        dcc.Tab(label='Porównanie Polski z krajami Unii Europejskiej', value='tab-2')
    ]),
    html.Div(id='tabs-content')
])

@app.callback(
    Output('tabs-content', 'children'),
    [Input('tabs', 'value')]
)
def render_content(tab):
    if tab == 'tab-1':
        return html.Div(children=[
            html.H1("Analiza wypadków w ruchu drogowym na obszarze Polski", style={'textAlign': 'center'}),
            html.Div(style={'display': 'flex'}, children=[
                html.Div(style={'flex': 1, 'padding': '12px'}, children=[
                    dcc.Graph(id='map', style={'margin': 'auto', 'width': 'auto', 'height': 'auto'},
                              config={'displayModeBar': False}),
                    dcc.RangeSlider(
                        id='year-slider',
                        min=min(ofiary_df_pow['Rok']),
                        max=max(ofiary_df_pow['Rok']),
                        value=[min(ofiary_df_pow['Rok']), max(ofiary_df_pow['Rok'])],
                        marks={str(year): {'label': str(year)} for year in
                               range(int(min(ofiary_df_pow['Rok'])), int(max(ofiary_df_pow['Rok']) + 1))},
                        step=None
                    )
                ]),
                html.Div(style={'flex': 1, 'padding': '12px'}, children=[
                    html.P(id='map-description'),
                    html.H3(id='table-title'),
                    html.Div(id='table-container')
                ]),
            ]),
            html.Div(style={'flex': 1, 'padding': '12px'}, children=[
                dcc.Dropdown(
                    id='column-dropdown',
                    options=[
                        {'label': 'Wypadki ogółem', 'value': 'Wypadki ogółem'},
                        {'label': 'Ranni', 'value': 'Ranni'},
                        {'label': 'Ofiary śmiertelne', 'value': 'Ofiary śmiertelne'}
                    ],
                    value='Wypadki ogółem',
                    clearable=False
                ),
                html.Label('Wybierz powiaty do porównania'),
                dcc.Dropdown(id='multiselect-dropdown',
                             options=[{'label': value, 'value': value} for value in
                                      ofiary_df_pow['Powiaty'].unique()],
                             clearable=False,
                             value=['powiat tarnogórski'],
                             multi=True
                             ),
                dcc.Graph(id='graph', style={'width': '100%', 'height': '100%'}, config={'displayModeBar': False}),
            ])
        ])

    elif tab == 'tab-2':
        return html.Div([
            dcc.Graph(id='bar-chart', config={'displayModeBar': False}),
            dcc.RangeSlider(
                id='year-slider',
                min=min(df['year']),
                max=max(df['year']),
                value=[min(df['year']), max(df['year'])],
                marks={str(year): str(year) for year in sorted(df['year'].unique())},
                step=None
            ),
            dcc.Dropdown(
                id='column-dropdown',
                options=[
                    {'label': 'Śmiertelność wypadków', 'value': 'fatal_mIn'},
                    {'label': 'Spożycie alkoholu', 'value': 'alcohol'},
                    {'label': 'Inwestycje w drogi', 'value': 'croad_inv_km'},
                    {'label': 'Indeks zmotoryzowania', 'value': 'mot_index_1000'},
                    {'label': 'Wydatki na utrzymanie dróg', 'value': 'croad_maint_km'}
                ],
                value='fatal_mIn',
                clearable=False
            )
        ])

@app.callback(
    Output('bar-chart', 'figure'),
    [Input('year-slider', 'value'),
     Input('column-dropdown', 'value')]
)
def update_chart(selected_years, selected_column):
    filtered_df = df[(df['year'] >= selected_years[0]) & (df['year'] <= selected_years[1])]
    aggregated_df = filtered_df.groupby('country', as_index=False).agg({selected_column: 'sum'})
    return create_figure(aggregated_df, selected_column)

@app.callback(
    Output('table-container', 'children'),
    [Input('column-dropdown', 'value')]
)
def update_table(selected_column):
    # Tworzenie tabeli z top 10 wierszami
    top_10_ofiary_df_woj = pd.DataFrame(ofiary_df_woj.nlargest(10, selected_column))

    table = html.Table([
        html.Thead(
            html.Tr([html.Th(col) for col in top_10_ofiary_df_woj.columns])
        ),
        html.Tbody([
            html.Tr([
                html.Td(top_10_ofiary_df_woj.iloc[i][col], style={'padding': '5px'}) for col in top_10_ofiary_df_woj.columns
            ]) for i in range(len(top_10_ofiary_df_woj))
        ])
    ], style={'borderSpacing': '12px'})
    return table

@app.callback(
    Output('table-title', 'children'),
    [Input('year-slider', 'value'),
        Input('column-dropdown', 'value')]
)
def update_table_title(selected_years, selected_column):
    rodzaj = ''
    if selected_column == "Wypadki ogółem":
        rodzaj = 'liczbie wypadków'
    elif selected_column == "Ranni":
        rodzaj = 'liczbie rannych'
    else:
        rodzaj = 'liczbie ofiar śmiertelnych'
    lata = ''
    if selected_years[0] == selected_years[1]:
        lata = f'roku {selected_years[0]}'
    else:
        lata = f'latach {selected_years[0]} - {selected_years[1]}'
    return f'Top 10 województw o największej {rodzaj} w {lata}'

@app.callback(
    Output('map-description', 'children'),
    [Input('year-slider', 'value'),
     Input('column-dropdown', 'value')]
)
def update_map_decription(selected_years, selected_column):
    lata = ''
    if selected_years[0] == selected_years[1]:
        lata = f'roku {selected_years[0]}'
    else:
        lata = f'latach {selected_years[0]} - {selected_years[1]}'
    rodzaj = ''
    if selected_column == "Wypadki ogółem":
        rodzaj = 'wypadków'
    elif selected_column == "Ranni":
        rodzaj = 'rannych'
    else:
        rodzaj = 'ofiar śmiertelnych'

    def find_max_value_province():
        max_index = ofiary_df_woj[selected_column].idxmax()
        max_province = ofiary_df_woj.loc[max_index, 'Województwa']
        return max_province
    return f'W {lata} liderem pod względem ilości {rodzaj} jest województwo {find_max_value_province()}.'


@app.callback(
    Output('map', 'figure'),
    [Input('year-slider', 'value'),
     Input('column-dropdown', 'value')]
)
def update_map(selected_years, selected_column):
    filtered_df = ofiary_df_woj[
        (ofiary_df_woj['Rok'] >= selected_years[0]) & (ofiary_df_woj['Rok'] <= selected_years[1])]

    aggregated_df = filtered_df.groupby('Województwa', as_index=False).sum()
    fig = px.choropleth_mapbox(
        aggregated_df,
        geojson=wojewodztwa_geojson,
        locations='Województwa',
        featureidkey='properties.nazwa',
        mapbox_style="carto-positron",
        zoom=4.5,
        color_continuous_scale=custom_colorscale,
        center={"lat": 52, "lon": 19.1},
        opacity=1,
        color=selected_column,
        hover_name='Województwa',
        hover_data={
            'Województwa': False,  # Ukryj kolumnę 'Województwa' z tooltipu
            selected_column: True  # Wyświetl wartości z kolumny wybranej przez użytkownika
        }
    )
    if selected_years[0] == selected_years[1]:
        fig.update_layout(
            title={
                'text': f'{selected_column} w województwach w roku {selected_years[0]}',
                'x': 0.5,
                'xanchor': 'center'}
        )
        return fig
    else:
        fig.update_layout(
            title={
                'text': f'{selected_column} w województwach w latach {selected_years[0]}-{selected_years[1]}',
                'x': 0.5,
                'xanchor': 'center'}
        )
        return fig

@app.callback(
    Output('graph', 'figure'),
    [Input('multiselect-dropdown', 'value'),
     Input('column-dropdown', 'value')]
)
def update_chart_powiaty(selected_powiat, selected_column):
    fig = go.Figure()
    fig.add_trace(go.Scatter(
        x=ofiary_mean['Rok'],
        y=ofiary_mean[selected_column],
        mode='lines+markers',
        name="Średnia wśród wszystkich powiatów",
        hovertemplate='<b>Średnia:</b> %{y}<br>' +
                      '<b>Rok:</b> %{x}<br><extra></extra>'
    ))

    for label in selected_powiat:
        fig.add_trace(go.Scatter(
            x=ofiary_df_pow[ofiary_df_pow['Powiaty'] == label]['Rok'],
            y=ofiary_df_pow[ofiary_df_pow['Powiaty'] == label][selected_column],
            mode='lines+markers',
            name=label,
            hovertemplate='<b>Powiat:</b> %{text}<br>' +
                          '<b>Rok:</b> %{x}<br>' +
                          '<b>' + selected_column + ':</b> %{y}<extra></extra>',
            text=[label] * len(ofiary_df_pow[ofiary_df_pow['Powiaty'] == label])
        ))
        for i, rok in enumerate(ofiary_df_pow[ofiary_df_pow['Powiaty'] == label]['Rok']):
            aktualna_wartosc = ofiary_df_pow[ofiary_df_pow['Powiaty'] == label][selected_column].iloc[i]
            srednia_wartosc = ofiary_mean[ofiary_mean['Rok'] == rok][selected_column].values[0]
            fig.add_shape(
                type="line",
                x0=rok,
                y0=min(aktualna_wartosc, srednia_wartosc),
                x1=rok,
                y1=max(aktualna_wartosc, srednia_wartosc),
                line=dict(color="black", width=1, dash="dash")
            )

    fig.update_layout(
        title={
            'text': f'{selected_column} w latach 2011-2022',
            'x': 0.5,
            'xanchor': 'center'},
        xaxis_title='Rok',
        yaxis_title=selected_column,
        xaxis=dict(
            tickmode='linear',
            dtick=1
        ),
        yaxis=dict(
            rangemode='tozero'  # Oś Y zaczyna się od 0
        ),
        legend=dict(
            orientation='h',  # Ustaw orientację legendy na poziomą (horizontal)
            yanchor='bottom',  # Zakotwicz legendę do dolnej krawędzi wykresu
            y=1.02,  # Odległość od górnej krawędzi wykresu
            xanchor='right',  # Zakotwicz legendę do prawej strony wykresu
            x=1  # Odległość od prawej krawędzi wykresu
        )
    )
    return fig

if __name__ == '__main__':
    app.run_server(debug=False)
