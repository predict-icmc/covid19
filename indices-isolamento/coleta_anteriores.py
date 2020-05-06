from selenium import webdriver
import time

estados = {'Acre':'AC',
    'Alagoas':'AL',
    'Amapá':'AP',
    'Amazonas':'AM',
    'Bahia':'BA',
    'Ceará':'CE',
    'Distrito Federal':'DF',
    'Espírito Santo':'ES',
    'Goiás':'GO',
    'Maranhão':'MA',
    'Mato Grosso':'MT',
    'Mato Grosso do Sul':'MS',
    'Minas Gerais':'MG', 
    'Pará':'PA', 
    'Paraíba':'PB',
    'Paraná':'PR',
    'Pernambuco':'PE',
    'Piauí':'PI',
    'Rio de Janeiro':'RJ', 
    'Rio Grande do Norte':'RN',
    'Rio Grande do Sul':'RS', 
    'Rondônia':'RO', 
    'Roraima':'RR', 
    'Santa Catarina':'SC',
    'São Paulo':'SP', 
    'Sergipe':'SE',
    'Tocantins':'TO'}

# Printa dados da media dos índices no arquivo
def print_dados(dados):
    arq = open('./dados3.csv','a')
    arq.write(dados+'\n')

# Site para scraping
url = 'https://public.tableau.com/views/MKTScoredeisolamentosocial/VisoGeral?%3Aembed=y&%3AshowVizHome=no&%3Adisplay_count=y&%3Adisplay_static_image=y'

browser = webdriver.Firefox() # Abre browser para scraping
browser.get(url) # Abre site

dados = {}
while True:
    try: 
        estado = estados[browser.find_element_by_xpath("//div[@class='tab-textRegion-content tab-selectable']").text[29::]]
        if estado not in dados.keys():
            dados[estado] = {}
        element = browser.find_element_by_xpath("//div[@class='tab-ubertipContent']")
        element.click()
        s = element.text
        data = s.split('\n')[1].replace('/','-')
        indice = s.split('\n')[4][:4:].replace(',','.')
        if data not in dados[estado].keys():
            dados[estado][data] = True
            print_dados(estado+','+data+','+indice)
            print(estado+','+data+','+indice+'\n')
        
    except:
        time.sleep(0.0001)
