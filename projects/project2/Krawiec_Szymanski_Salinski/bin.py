import pandas as pd
import plotly.express as px

# Tworzenie interaktywnego wykresu
def create_figure(df, column):
    # Ustawienie mapowania kolorów
    color_discrete_map = {'Poland': 'red'}
    default_color = '#7FB3D5'  # Jasny niebieski
    for country in df['country'].unique():
        if country != 'Polska':
            color_discrete_map[country] = default_color

    if column == 'fatal_mIn':
        title = 'Śmiertelność wypadków w Europie'
        y_axis_label = 'Liczba ofiar śmiertelnych na milion mieszkańców'
    elif column == 'alcohol':
        title = 'Spożycie alkoholu w Europie'
        y_axis_label = 'Spożycie alkoholu na osobę w litrach'
    elif column == 'mot_index_1000':
        title = 'Indeks zmotoryzowania'
        y_axis_label = 'Indeks zmotoryzowania'
    elif column == 'croad_maint_km':
        title = 'Wydatki na utrzymanie dróg w przeliczeniu na kilometr €/km (ceny stałe z 2015 r.)'
        y_axis_label = 'Wydatki na utrzymanie dróg'
    else:
        title = 'Inwestycje w drogi w Europie'
        y_axis_label = 'Inwestycje w drogi na km²'

    fig = px.bar(df, x='country', y=column, color='country',
                 title=title,
                 labels={column: y_axis_label, 'country': 'Kraj'},
                 color_discrete_map=color_discrete_map,  # Ustawienie mapowania kolorów
                 category_orders={'country': df.groupby('country')[column].sum().sort_values(ascending=True).index.tolist()},  # Posortowanie krajów rosnąco
                 custom_data=[df[column]],  # Ustawienie danych niestandardowych
                 )

    fig.update_traces(
        hovertemplate='%{customdata[0]:.2f}'  # Formatowanie wyświetlanej wartości
    )

    fig.update_layout(title_font_size=24, title_x=0.5)  # Powiększenie tytułu i wyśrodkowanie
    fig.update_xaxes(title_text="Kraj")  # Zmiana tytułu osi X
    fig.update_yaxes(title_text=y_axis_label)  # Zmiana tytułu osi Y
    return fig
