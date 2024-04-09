# potrzebne do czyszczenia kolumny z imionami

pattern <- c('Sandor.*', 'Aerson','.*Rodri\\w*k.*|Cassel','.*Allis\\w*er.*', '.*Barrist.*','.*Benjen.*','Beric.*',
  'Brans Voice', 'Daario.*','D\\w*nerys.*','Dol\\w*rou\\w.*|^Edd$|^Ed$', '.*Drogo$',
  'Balon.*','Eddar\\w.*', 'Edmure', 'El\\w?aria','.*Py\\w*ell\\w*', 'Hizdahr.*',
  'Illyrio.*', 'Janos.*', 'Jeor.*', 'Kev\\wn.*', '.*Joffrey.*', 'Kraznys.*',
  'Lancel.*', 'Lollys.*', 'Lommy.*', 'Loras.*', '^Lyann.*', '.*Luwin', 'Lysa.*',
  'Mace.*','Mance', 'Manderly', 'M\\wryn.*', 'Mhaeg\\wn', 'Mount\\w*n', 'Myrcella.*',
  '.*Yarw\\wck', 'Petyr.*', 'Pyat.*', 'Quaith.*', 'Ra\\w\\wal.*', 'Renly.*', 'Rickard.*',
  'Ri\\w*kon.*', 'Robett.*', 'Robin.*', 'Roose.*', 'Salla.*', '^Saan$',
  'Sam$|^Sammy$|Sam Tarly', '.*Jorah.*', 'Selyse.*', 'Shireen.*', 'Tommen.*',
  '.*Varys', 'Viserys.*','.*Royce|Yohn', '.*Hodor.*', 'Young Ned', 'Brynde.*')


replace <- c('Sandor Clegane', 'Aeron','Rodrik Cassel','Alliser Thorne','Barristan Selmy',
  'Benjen Stark', 'Beric Dondarrion','Bran Stark', 'Daario Naharis',
  'Daenerys Targaryen','Dolorous Edd','Khal Drogo','Balon Greyjoy', 'Eddard Stark',
  'Edmure Tully', 'Elaria Sand','Grand Maester Pycelle','Hizdahr Zo Loraq',
  'Illyrio Mopatis', 'Janos Slynt', 'Jeor Mormont', 'Kevan Lannister','Joffrey Baratheon',
  'Kranzyz Mo Nakloz', 'Lancel Lannister','Lollys Stokeworth', 'Lommy', 'Loras Tyrell',
  'Lyanna Mormont', 'Maester Luwin', 'Lysa Arryn', 'Mace Tyrell', 'Mance Rayder',
  'Wyman Manderly', 'Meryn Trant', 'Mhaegen','Gregor Clegane', 'Myrcella Baratheon',
  'Othell Yarwyck', 'Petyr Baelish', 'Pyat Pree', 'Quaithe', 'Razdal Mo Eraz',
  'Renly Baratheon', 'Rickard Karstark', 'Rickon Stark', 'Robett Glover', 'Robin Arryn',
  'Roose Bolton', 'Salladhor Saan', 'Salladhor Saan', 'Samwell Tarly',
  'Jorah Mormont','Selyse Baratheon', 'Shireen Baratheon',
  'Tommen Baratheon', 'Varys', 'Viserys Targaryen', 'Yohn Royce', 'Hodor',
  'Eddard Stark', 'Brynden Tully')
