import os
import pandas as pd

os.chdir(os.path.dirname(os.path.abspath(__file__)))

# PRZYGOTOWANIE RAMKI DANYCH DLA WOJEWÓDZTW

def main():
    ofiary_df_woj = pd.read_csv("../data/ofiary.csv", sep=";", header=0)
    ofiary_df_woj = ofiary_df_woj[ofiary_df_woj["Nazwa"].str.isupper()]
    ofiary_df_woj = ofiary_df_woj[1:(len(ofiary_df_woj["Nazwa"]) + 1)]

    ofiary_df_woj1 = pd.melt(ofiary_df_woj.iloc[:, [1] + list(range(14, 26))], id_vars=["Nazwa"], var_name="Rok",
                            value_name="Wypadki ogółem")
    ofiary_df_woj1["Rok"] = ofiary_df_woj1["Rok"].str.replace("wypadki ogółem;", "").str.replace(";[szt.]", '')

    ofiary_df_woj2 = pd.melt(ofiary_df_woj.iloc[:, [1] + list(range(39, 51))], id_vars=["Nazwa"], var_name="Rok",
                            value_name="Ofiary śmiertelne")
    ofiary_df_woj2["Rok"] = ofiary_df_woj2["Rok"].str.replace("ofiary śmiertelne;", "").str.replace(";[osoba]", '')

    ofiary_df_woj3 = pd.melt(ofiary_df_woj.iloc[:, [1] + list(range(64, 76))], id_vars=["Nazwa"], var_name="Rok",
                            value_name="Ranni")
    ofiary_df_woj3["Rok"] = ofiary_df_woj3["Rok"].str.replace("ranni;", "").str.replace(";[osoba]", '')

    ofiary_df_woj3["Wypadki ogółem"] = ofiary_df_woj1["Wypadki ogółem"]
    ofiary_df_woj3["Ofiary śmiertelne"] = ofiary_df_woj2["Ofiary śmiertelne"]

    ofiary_df_woj = ofiary_df_woj3.rename(columns={
        "Nazwa": "Województwa"
    })
    ofiary_df_woj['Województwa'] = ofiary_df_woj['Województwa'].str.lower()

    wojewodztwa = [
        "śląskie", "opolskie", "wielkopolskie", "zachodniopomorskie", "świętokrzyskie",
        "kujawsko-pomorskie", "podlaskie", "dolnośląskie", "podkarpackie", "małopolskie",
        "pomorskie", "warmińsko-mazurskie", "łódzkie", "mazowieckie", "lubelskie", "lubuskie"
    ]

    wojewodztwa_df = pd.DataFrame(wojewodztwa, columns=["Województwa"])

    ofiary_df_woj = pd.merge(wojewodztwa_df, ofiary_df_woj, how="left", on="Województwa")

    ofiary_df_woj['Rok'] = ofiary_df_woj['Rok'].astype(float)
    ofiary_df_woj['Ranni'] = ofiary_df_woj['Ranni'].astype(float)
    ofiary_df_woj['Wypadki ogółem'] = ofiary_df_woj['Wypadki ogółem'].astype(float)
    ofiary_df_woj['Ofiary śmiertelne'] = ofiary_df_woj['Ofiary śmiertelne'].astype(float)

    ofiary_df_woj.to_csv("../data/ofiary_woj.csv", index=False)

if __name__ == "__main__":
    main()
