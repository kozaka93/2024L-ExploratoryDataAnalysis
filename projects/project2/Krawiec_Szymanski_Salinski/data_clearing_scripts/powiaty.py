import os
import pandas as pd

os.chdir(os.path.dirname(os.path.abspath(__file__)))


# PRZYGOTOWANIE RAMKI DANYCH DLA POWIATÓW

def main():
    ofiary_df_pow = pd.read_csv("../data/ofiary.csv", sep=";", header=0)
    ofiary_df_pow = ofiary_df_pow[~ofiary_df_pow["Nazwa"].str.isupper()]
    ofiary_df_pow["Nazwa"] = ofiary_df_pow["Nazwa"].str.replace(" m. st.", "").str.replace(" m.", '').str.replace(
        "Powiat", "powiat").str.replace(
        " od 2013", "")
    ofiary_df_pow1 = pd.melt(ofiary_df_pow.iloc[:, [1] + list(range(14, 26))], id_vars=["Nazwa"], var_name="Rok",
                             value_name="Wypadki ogółem")
    ofiary_df_pow1["Rok"] = ofiary_df_pow1["Rok"].str.replace("wypadki ogółem;", "").str.replace(";[szt.]", '')

    ofiary_df_pow2 = pd.melt(ofiary_df_pow.iloc[:, [1] + list(range(39, 51))], id_vars=["Nazwa"], var_name="Rok",
                             value_name="Ofiary śmiertelne")
    ofiary_df_pow2["Rok"] = ofiary_df_pow2["Rok"].str.replace("ofiary śmiertelne;", "").str.replace(";[osoba]", '')

    ofiary_df_pow3 = pd.melt(ofiary_df_pow.iloc[:, [1] + list(range(64, 76))], id_vars=["Nazwa"], var_name="Rok",
                             value_name="Ranni")
    ofiary_df_pow3["Rok"] = ofiary_df_pow3["Rok"].str.replace("ranni;", "").str.replace(";[osoba]", '')

    ofiary_df_pow3["Wypadki ogółem"] = ofiary_df_pow1["Wypadki ogółem"]
    ofiary_df_pow3["Ofiary śmiertelne"] = ofiary_df_pow2["Ofiary śmiertelne"]

    ofiary_df_pow = ofiary_df_pow3.rename(columns={
        "Nazwa": "Powiaty"
    })

    powiaty = [
        'powiat ropczycko-sędziszowski', 'powiat łosicki', 'powiat piaseczyński',
        'powiat radomski', 'powiat sierpecki', 'powiat szydłowiecki',
        'powiat węgrowski', 'powiat gostyniński', 'powiat grodziski',
        'powiat łukowski', 'powiat tomaszowski', 'powiat Chełm', 'powiat brzeski',
        'powiat Kraków', 'powiat zgierski', 'powiat sulęciński', 'powiat łańcucki',
        'powiat ostrzeszowski', 'powiat Radom', 'powiat żyrardowski',
        'powiat obornicki', 'powiat leszczyński', 'powiat Siedlce', 'powiat Leszno',
        'powiat kolski', 'powiat Łomża', 'powiat rawicki', 'powiat słupecki',
        'powiat kościerski', 'powiat włoszczowski', 'powiat stargardzki',
        'powiat Wrocław', 'powiat giżycki', 'powiat mrągowski', 'powiat głogowski',
        'powiat choszczeński', 'powiat Sosnowiec', 'powiat rybnicki',
        'powiat Gliwice', 'powiat Piekary Śląskie', 'powiat Jaworzno',
        'powiat inowrocławski', 'powiat brodnicki', 'powiat włocławski',
        'powiat mogileński', 'powiat Toruń', 'powiat tucholski', 'powiat raciborski',
        'powiat cieszyński', 'powiat krośnieński', 'powiat nyski',
        'powiat Jelenia Góra', 'powiat zgorzelecki', 'powiat przasnyski',
        'powiat Ostrołęka', 'powiat średzki', 'powiat jarociński', 'powiat Gdynia',
        'powiat Świnoujście', 'powiat kamiennogórski', 'powiat pabianicki',
        'powiat kolneński', 'powiat Opole', 'powiat ostródzki', 'powiat przemyski',
        'powiat Przemyśl', 'powiat warszawski zachodni', 'powiat włodawski',
        'powiat Lublin', 'powiat bocheński', 'powiat Tarnów', 'powiat wschowski',
        'powiat gorzowski', 'powiat międzyrzecki', 'powiat słubicki',
        'powiat nowotomyski', 'powiat wągrowiecki', 'powiat bielski',
        'powiat suwalski', 'powiat słupski', 'powiat ostrowiecki',
        'powiat skarżyski', 'powiat kartuski', 'powiat częstochowski',
        'powiat pyrzycki', 'powiat Siemianowice Śląskie', 'powiat Elbląg',
        'powiat gryfiński', 'powiat Bytom', 'powiat złotoryjski',
        'powiat wrocławski', 'powiat milicki', 'powiat lubiński', 'powiat lipnowski',
        'powiat żniński', 'powiat radziejowski', 'powiat nakielski',
        'powiat bartoszycki', 'powiat żywiecki', 'powiat sokólski',
        'powiat jasielski', 'powiat głubczycki', 'powiat nowosądecki',
        'powiat Zielona Góra', 'powiat Skierniewice', 'powiat żagański',
        'powiat Gdańsk', 'powiat lęborski', 'powiat Łódź', 'powiat piotrkowski',
        'powiat ostrowski', 'powiat starogardzki', 'powiat Poznań', 'powiat Konin',
        'powiat stalowowolski', 'powiat Tarnobrzeg', 'powiat płoński',
        'powiat mławski', 'powiat siedlecki', 'powiat garwoliński', 'powiat lipski',
        'powiat hrubieszowski', 'powiat kraśnicki', 'powiat łęczyński',
        'powiat opolski', 'powiat rycki', 'powiat Zamość', 'powiat dąbrowski',
        'powiat krakowski', 'powiat wieruszowski', 'powiat żarski', 'powiat dębicki',
        'powiat namysłowski', 'powiat czarnkowsko-trzcianecki', 'powiat Płock',
        'powiat grajewski', 'powiat starachowicki', 'powiat konecki',
        'powiat elbląski', 'powiat nidzicki', 'powiat nowomiejski',
        'powiat Ruda Śląska', 'powiat białogardzki', 'powiat tarnogórski',
        'powiat zawierciański', 'powiat Chorzów', 'powiat Katowice',
        'powiat bolesławiecki', 'powiat Wałbrzych', 'powiat sępoleński',
        'powiat Włocławek', 'powiat hajnowski', 'powiat chrzanowski',
        'powiat wejherowski', 'powiat Szczecin', 'powiat pucki', 'powiat rawski',
        'powiat łowicki', 'powiat skierniewicki', 'powiat iławski',
        'powiat niżański', 'powiat tarnobrzeski', 'powiat nowodworski',
        'powiat pruszkowski', 'powiat przysuski', 'powiat białobrzeski',
        'powiat wyszkowski', 'powiat biłgorajski', 'powiat chełmski',
        'powiat parczewski', 'powiat świdnicki', 'powiat kutnowski',
        'powiat łódzki wschodni', 'powiat kolbuszowski', 'powiat oleski',
        'powiat strzelecki', 'powiat złotowski', 'powiat Suwałki',
        'powiat kościański', 'powiat szamotulski', 'powiat Kielce',
        'powiat staszowski', 'powiat ełcki', 'powiat dzierżoniowski',
        'powiat tczewski', 'powiat kołobrzeski', 'powiat Koszalin',
        'powiat kłobucki', 'powiat gliwicki', 'powiat lubliniecki',
        'powiat Bielsko-Biała', 'powiat Legnica', 'powiat grudziądzki',
        'powiat wołowski', 'powiat toruński', 'powiat chełmiński',
        'powiat wodzisławski', 'powiat leski', 'powiat bieszczadzki',
        'powiat prudnicki', 'powiat ząbkowicki', 'powiat makowski',
        'powiat pszczyński', 'powiat gołdapski', 'powiat Sopot', 'powiat rzeszowski',
        'powiat jeleniogórski', 'powiat opoczyński', 'powiat człuchowski',
        'powiat Kalisz', 'powiat strzyżowski', 'powiat miński', 'powiat zwoleński',
        'powiat żuromiński', 'powiat lubelski', 'powiat proszowicki',
        'powiat wielicki', 'powiat sieradzki', 'powiat brzeziński',
        'powiat strzelecko-drezdenecki', 'powiat świebodziński', 'powiat leżajski',
        'powiat lubaczowski', 'powiat chodzieski', 'powiat międzychodzki',
        'powiat kluczborski', 'powiat jędrzejowski', 'powiat gostyński',
        'powiat kazimierski', 'powiat kaliski', 'powiat siemiatycki',
        'powiat gdański', 'powiat szczycieński', 'powiat łobeski', 'powiat gryficki',
        'powiat myszkowski', 'powiat oleśnicki', 'powiat strzeliński',
        'powiat wąbrzeski', 'powiat kętrzyński', 'powiat białostocki',
        'powiat augustowski', 'powiat wałbrzyski', 'powiat kłodzki', 'powiat Tychy',
        'powiat Rybnik', 'powiat sokołowski', 'powiat grójecki',
        'powiat lubartowski', 'powiat puławski', 'powiat radzyński',
        'powiat Biała Podlaska', 'powiat Nowy Sącz', 'powiat Gorzów Wielkopolski',
        'powiat pilski', 'powiat wysokomazowiecki', 'powiat buski', 'powiat śremski',
        'powiat kępiński', 'powiat turecki', 'powiat opatowski', 'powiat Słupsk',
        'powiat pińczowski', 'powiat koszaliński', 'powiat będziński',
        'powiat węgorzewski', 'powiat bieruńsko-lędziński', 'powiat policki',
        'powiat polkowicki', 'powiat Jastrzębie-Zdrój', 'powiat tatrzański',
        'powiat nowotarski', 'powiat lwówecki', 'powiat legionowski',
        'powiat olecki', 'powiat zambrowski', 'powiat bytowski', 'powiat płocki',
        'powiat pułtuski', 'powiat sochaczewski', 'powiat ciechanowski',
        'powiat bialski', 'powiat krasnostawski', 'powiat zamojski',
        'powiat tarnowski', 'powiat bełchatowski', 'powiat łaski', 'powiat łęczycki',
        'powiat pajęczański', 'powiat radomszczański', 'powiat zduńskowolski',
        'powiat nowosolski', 'powiat jarosławski', 'powiat mielecki',
        'powiat Warszawa', 'powiat krapkowicki', 'powiat moniecki',
        'powiat kielecki', 'powiat wrzesiński', 'powiat gnieźnieński',
        'powiat malborski', 'powiat lidzbarski', 'powiat olsztyński',
        'powiat sławieński', 'powiat działdowski', 'powiat górowski',
        'powiat sztumski', 'powiat kwidzyński', 'powiat Dąbrowa Górnicza',
        'powiat Mysłowice', 'powiat myśliborski', 'powiat golubsko-dobrzyński',
        'powiat aleksandrowski', 'powiat świecki', 'powiat pleszewski',
        'powiat Krosno', 'powiat sejneński', 'powiat sanocki', 'powiat suski',
        'powiat zielonogórski', 'powiat wołomiński', 'powiat mikołowski',
        'powiat goleniowski', 'powiat Rzeszów', 'powiat łomżyński', 'powiat wałecki',
        'powiat drawski', 'powiat chojnicki', 'powiat przeworski', 'powiat otwocki',
        'powiat kozienicki', 'powiat janowski', 'powiat limanowski',
        'powiat miechowski', 'powiat myślenicki', 'powiat olkuski',
        'powiat oświęcimski', 'powiat Piotrków Trybunalski', 'powiat poddębicki',
        'powiat wieluński', 'powiat brzozowski', 'powiat kędzierzyńsko-kozielski',
        'powiat krotoszyński', 'powiat Białystok', 'powiat wolsztyński',
        'powiat sandomierski', 'powiat szczecinecki', 'powiat świdwiński',
        'powiat piski', 'powiat Świętochłowice', 'powiat Zabrze', 'powiat Olsztyn',
        'powiat braniewski', 'powiat kamieński', 'powiat Częstochowa',
        'powiat trzebnicki', 'powiat bydgoski', 'powiat oławski', 'powiat legnicki',
        'powiat Bydgoszcz', 'powiat Grudziądz', 'powiat rypiński', 'powiat gorlicki',
        'powiat lubański', 'powiat wadowicki', 'powiat ostrołęcki', 'powiat Żory',
        'powiat jaworski', 'powiat poznański', 'powiat koniński'
    ]

    powiaty_df = pd.DataFrame(powiaty).rename(columns={
        0: "Powiaty"
    })

    ofiary_df_pow = pd.merge(powiaty_df, ofiary_df_pow, how="left", on="Powiaty")

    ofiary_df_pow['Rok'] = ofiary_df_pow['Rok'].astype(float)
    ofiary_df_pow['Ranni'] = ofiary_df_pow['Ranni'].astype(float)
    ofiary_df_pow['Wypadki ogółem'] = ofiary_df_pow['Wypadki ogółem'].astype(float)
    ofiary_df_pow['Ofiary śmiertelne'] = ofiary_df_pow['Ofiary śmiertelne'].astype(float)

    ofiary_df_pow.to_csv("../data/ofiary_pow.csv", index=False)

    ofiary_mean_wypadki = pd.DataFrame(ofiary_df_pow.groupby('Rok')["Wypadki ogółem"].mean().round(0).reset_index())
    ofiary_mean_ranni = pd.DataFrame(ofiary_df_pow.groupby('Rok')["Ranni"].mean().round(0).reset_index())
    ofiary_mean_ofiary = pd.DataFrame(ofiary_df_pow.groupby('Rok')["Ofiary śmiertelne"].mean().round(0).reset_index())

    ofiary_mean_wypadki["Ranni"] = ofiary_mean_ranni["Ranni"]
    ofiary_mean_wypadki["Ofiary śmiertelne"] = ofiary_mean_ofiary["Ofiary śmiertelne"]
    ofiary_mean = ofiary_mean_wypadki

    ofiary_mean.to_csv("../data/ofiary_mean.csv", index=False)


if __name__ == "__main__":
    main()
