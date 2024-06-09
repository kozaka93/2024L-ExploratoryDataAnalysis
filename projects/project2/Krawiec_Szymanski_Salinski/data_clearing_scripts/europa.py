import os
import pandas as pd

os.chdir(os.path.dirname(os.path.abspath(__file__)))

# PRZYGOTOWANIE RAMKI DANYCH O KRAJACH EUROPY

def main():
    dane = pd.read_excel('../data/europa.xlsx')
    data = dane[['country', 'year', 'fatal_mIn', 'alcohol', 'croad_inv_km', 'mot_index_1000', 'croad_maint_km']]

    df = pd.DataFrame(data)
    df['fatal_mIn'] = df['fatal_mIn'].str.replace(',', '.').astype(float)
    df['alcohol'] = df['alcohol'].str.replace(',', '.').astype(float)
    df['croad_inv_km'] = df['croad_inv_km'].str.replace(',', '.').astype(float)
    df['mot_index_1000'] = df['mot_index_1000'].str.replace(',', '.').astype(float)
    df['croad_maint_km'] = df['croad_maint_km'].str.replace(',', '.').astype(float)

    df.to_csv('../data/europa.csv', index=False)

if __name__ == "__main__":
    main()
