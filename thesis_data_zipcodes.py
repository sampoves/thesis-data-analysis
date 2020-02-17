# -*- coding: utf-8 -*-
"""
Created on Mon Feb 17 22:38:53 2020

@author: Sampo Vesanen

Sampo Vesanen Thesis survey datacrunch
"Parking of private cars and spatial accessibility in Helsinki Capital Region"

Hide tedious lists of zipcodes in this Python script.
"""

################################
### Divisions by postal code ###
################################

# Helsinki subdivisions -------------------------------------------------------
# https://www.avoindata.fi/data/fi/dataset/helsinki-alueittain/resource/9e197c6a-1882-4ad9-a50b-9dc7c49cb75a

# Southern subdivision
hkiSouth = ["00250", # Taka-Töölö
            "00260", # Keski-Töölö
            "00100", # Helsinki keskusta - Etu-Töölö
            "00180", # Kamppi-Ruoholahti
            "00200", # Lauttasaari
            "00210", # Vattuniemi
            "00220", # Jätkäsaari
            "00120", # Punavuori
            "00130", # Kaartinkaupunki
            "00140", # Kaivopuisto-Ullanlinna
            "00160", # Katajanokka
            "00150", # Eira-Hernesaari
            "00190", # Suomenlinna
            "00170"] # Kruununhaka

# Western subdivision
hkiWest = ["00290", # Meilahden sairaala-alue
           "00270", # Pohjois-Meilahti
           "00280", # Ruskeasuo
           "00330", # Munkkiniemi
           "00350", # Munkkivuori-Niemenmäki
           "00310", # Kivihaka
           "00320", # Etelä-Haaga
           "00340", # Kuusisaari-Lehtisaari
           "00400", # Pohjois-Haaga
           "00380", # Pitäjänmäen teollisuusalue
           "00360", # Pajamäki
           "00370", # Reimarla
           "00440", # Lassila
           "00390", # Konala
           "00410", # Malminkartano 
           "00420", # Kannelmäki
           "00430", # Maununneva
           "00300"] # Pikku Huopalahti

# Central (Keskinen) subdivision
hkiCentral = ["00230", # Ilmala 
             "00240", # Länsi-Pasila 
             "00610", # Käpylä 
             "00600", # Koskela 
             "00560", # Toukola-Vanhakaupunki 
             "00550", # Vallila 
             "00520", # Itä-Pasila
             "00510", # Etu-Vallila-Alppila
             "00500", # Sörnäinen
             "00530", # Kallio
             "00540", # Kalasatama
             "00580"] # Verkkosaari

# Norther subdivision
hkiNorth = ["00690", # Tuomarinkylä-Torpparinmäki 
            "00670", # Paloheinä
            "00660", # Länsi-Pakila
            "00680", # Itä-Pakila
            "00630", # Maunula-Suursuo
            "00620", # Metsälä-Etelä-Oulunkylä
            "00650", # Veräjämäki
            "00640"] # Oulunkylä-Patola

# Northeastern subdivision
hkiNortheast = ["00740", # Siltamäki
                "00750", # Puistola
                "00760", # Suurmetsä
                "00780", # Tapaninvainio
                "00730", # Tapanila
                "00770", # Jakomäki-Alppikylä
                "00720", # Pukinmäki-Savela
                "00710", # Pihlajamäki
                "00790", # Viikki
                "00700"] # Malmi

# Southeastern subdivision
hkiSoutheast = ["00800", # Länsi-Herttoniemi 
                "00880", # Roihupellon teollisuusalue
                "00820", # Roihuvuori
                "00810", # Herttoniemi
                "00830", # Tammisalo                
                "00570", # Kulosaari
                "00590", # Kaitalahti
                "00870", # Etelä-Laajasalo
                "00850", # Jollas
                "00860", # Santahamina                
                "00840"] # Laajasalo

# Eastern subdivision
hkiEast = ["00940", # Kontula
           "00970", # Mellunmäki
           "00920", # Myllypuro
           "00950", # Vartioharju 
           "00960", # Pohjois-Vuosaari 
           "00980", # Etelä-Vuosaari
           "00990", # Aurinkolahti 
           "00900", # Puotinharju 
           "00910", # Puotila            
           "00930"] # Itäkeskus-Marjaniemi

# Östersundom subdivision
hkiOster = ["00890"] # Östersundom


# Espoo subdivisions ----------------------------------------------------------
#https://www.espoo.fi/fi-fi/Espoon_kaupunki/Tietoa_Espoosta/Tilastot_ja_tutkimukset/Aluejakokartat
# See Excel sheet for exact place names:
# Suur-, tilasto- ja pienalueiden nimet 1.1.2014
    
#Suur-Leppävaara subdivision
# Kanta-Leppävaara, Kilo-Karakallio, Laaksolahti, Viherlaakso-Lippajärvi,
# Sepänkylä
espLeppavaara = ["02620", # Karakallio
                 "02610", # Kilo
                 "02600", # Etelä-Leppävaara 
                 "02650", # Pohjois-Leppävaara 
                 "02660", # Lintuvaara
                 "02710", # Viherlaakso
                 "02720", # Lähderanta
                 "02730", # Jupperi
                 "02630", # Nihtisilta
                 "02680"] # Uusmäki

#Suur-Tapiola subdivision
# Kanta-Tapiola, Otaniemi, Haukilahti-Westend, Mankkaa, Laajalahti
espTapiola = ["02170", # Haukilahti
              "02140", # Laajalahti
              "02180", # Mankkaa
              "02200", # Niittykumpu
              "02150", # Otaniemi
              "02100", # Tapiola
              "02130", # Pohjois-Tapiola
              "02110", # Otsolahti
              "02120", # Länsikorkee-Suvikumpu
              "02160"] # Westend

#Suur-Matinkylä subdivision
# Matinkylä, Olari, Henttaa-Suurpelto
espMatinkyla = ["02250", # Henttaa
                "02240", # Friisilä
                "02210", # Olari
                "02290", # Puolarmetsän sairaala, own placement
                "02230"] # Matinkylä

#Suur-Espoonlahti subdivision
# Kanta-Espoonlahti, Saunalahti, Nöykkiö-Latokaski, Kaitaa, Suvisaaristo
espEspoonlahti = ["02320", # Espoonlahti
                  "02360", # Soukka
                  "02300", # Nöykkiönpuro
                  "02330", # Saunalahti-Kattilalaakso
                  "02340", # Latokaski
                  "02270", # Finnoo-Eestinmalmi
                  "02260", # Kaitaa
                  "02280", # Malminmäki-Eestinlaakso
                  "02380"] # Suvisaaristo

#Suur-Kauklahti subdivision
# Kanta-Kauklahti, Kurttila-Vanttila
espKauklahti = ["02780"] # Kauklahti and all others

#Vanha-Espoo subdivision
# Kanta-Espoo, Muurala-Gumböle, Bemböle, Nuuksio-Nupuri
espVanhaespoo = ["02770", # Espoon keskus
                 "02810", # Gumböle-Karhusuo
                 "02820", # Nupuri-Nuuksio
                 "02760", # Tuomarila-Suvela
                 "02940", # Lippajärvi-Järvenperä, own placement
                 "02740", # Bemböle-Pakankylä
                 "02750", # Sepänkylä-Kuurinniitty, own placement
                 "02860"] # Siikajärvi

#Pohjois-Espoo subdivision
# Vanhakartano-Röylä, Kalajärvi-Lakisto
espPohjoisespoo = ["02970", # Kalajärvi
                   "02920", # Niipperi
                   "02980"] # Lakisto


# Kauniainen ------------------------------------------------------------------
kauniainen = ["02700"]


# Vantaa subdivisions ---------------------------------------------------------
# https://www.vantaa.fi/instancedata/prime_product_julkaisu/vantaa/embeds/vantaawwwstructure/124282_Vantaa_alueittain_2015.pdf
# Myyrmäki subdivision
# Linnainen, Hämevaara, Hämeenkylä, Vapaala, Varisto, Myyrmäki, Kaivoksela,
# Martinlaakso, Vantaanlaakso, Askisto, Petikko
vanMyyrmaki = ["01600", # Myyrmäki
               "01610", # Kaivoksela
               "01620", # Martinlaakso
               "01630", # Hämeenkylä
               "01640", # Hämevaara
               "01650", # Vapaala
               "01660", # Varisto
               "01670", # Vantaanlaakso
               "01680", # Askisto
               "01710", # Pähkinärinne
               "01720", # Petikko
               "01770"] # Martinlaakson teollisuusalue, own placement

# Kivistö subdivision
# Piispankylä, Keimola, Kivistö, Lapinkylä, Myllymäki, Vestra, Luhtaanmäki,
# Riipilä, Seutula, Kiila
vanKivisto = ["01700", # Kivistö
              "01730", # Vantaanpuisto
              "01750", # Keimola
              "01760"] # Seutula

# Aviapoliksen
# Ylästö, Viinikkala, Tammisto, Pakkala, Veromies, Lentokenttä
vanAviapolis = ["01520", # Tammisto
                "01530", # Veromiehenkylä
                "01690", # Ylästö
                "01510", # Kirkonkylä-Veromäki, my own placement
                "01740"] # Tuupakan teollisuusalue

# Tikkurila subdivision
# Hiekkaharju, Tikkurila, Jokiniemi, Viertola, Kuninkaala, Simonkylä,
# Hakkila, Ruskeasanta, Koivuhaka, Helsingin pitäjän kirkonkylä
vanTikkurila = ["01300", # Tikkurila
                "01350", # Hiekkaharju
                "01370", # Jokiniemi
                "01380"] # Kuusikko-Hakkila

# Koivukylä subdivision
# Koivukylä, Ilola, Asola, Rekola, Havukoski, Päiväkumpu
vanKoivukyla = ["01360", # Koivukylä-Havukoski
                "01400", # Rekola
                "01340", # Leinelä, my own placement
                "01390", # Ruskeasanta-Ilola, my own placement
                "01420"] # Päiväkumpu

# Korso subdivision
# Matari, Korso, Mikkola, Metsola, Leppäkorpi, Jokivarsi, Nikinmäki, Vierumäki,
# Vallinoja
vanKorso = ["01450", # Korso
            "01480"] # Mikkola

# Hakunila subdivision
# Länsisalmi, Länsimäki, Ojanko, Vaarala, Hakunila, Rajakylä, Itä-Hakkila,
# Kuninkaanmäki, Sotunki
vanHakunila = ["01200", # Hakunila
               "01230", # Vaarala
               "01260", # Itä-Hakkila
               "01280"] # Länsimäki


# This dictionary helps assigning postal codes to DataFrame records
subdiv_dict = {"hkiSouth": "Helsinki Southern",
               "hkiWest": "Helsinki Western",
               "hkiCentral": "Helsinki Central",
               "hkiNorth": "Helsinki Northern",
               "hkiNortheast": "Helsinki Northeastern",
               "hkiSoutheast": "Helsinki Southeastern",
               "hkiEast": "Helsinki Eastern",
               "hkiOster": "Helsinki Östersundom",
               "espLeppavaara": "Espoo Suur-Leppävaara",
               "espTapiola": "Espoo Suur-Tapiola",
               "espMatinkyla": "Espoo Suur-Matinkylä",
               "espEspoonlahti": "Espoo Suur-Espoonlahti",
               "espKauklahti": "Espoo Suur-Kauklahti",
               "espVanhaespoo": "Espoo Vanha-Espoo",
               "espPohjoisespoo": "Espoo Pohjois-Espoo",
               "vanMyyrmaki": "Vantaa Myyrmäki",
               "vanKivisto": "Vantaa Kivistö",
               "vanAviapolis": "Vantaa Aviapolis",
               "vanTikkurila": "Vantaa Tikkurila",
               "vanKoivukyla": "Vantaa Koivukylä",
               "vanKorso": "Vantaa Korso",
               "vanHakunila": "Vantaa Hakunila",
               "kauniainen": "Kauniainen"}