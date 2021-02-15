##
# Este script atualiza os dados de isolamento pelo ranking dos estados
##
from selenium import webdriver
import time

# Printa dados do estado no arquivo
def print_dados(dados):
    arq = open('../../dados/isolamento/','a')
    arq.write(dados+'\n')

def formata_data(str_data):
    d,m,a = str_data.split('/')
    if int(d) < 10:
        d = '0'+d
    if int(m) < 10:
        m = '0'+m
    return d+'-'+m+'-'+a

browser = webdriver.Firefox() # Abre o browser
url = 'https://public.tableau.com/views/MKTScoredeisolamentosocial/Rankingdosestados?%3Aembed=y&%3AshowVizHome=no&%3Adisplay_count=y&%3Adisplay_static_image=y'
browser.get(url) # Acessa o site

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

lista_dados = {}
while len(lista_dados.keys()) != 27: # Enquanto não coletar os dados de todos os estados, execute:
    try:
        # Acessa elemento
        element = browser.find_element_by_xpath("//div[@class='tab-ubertipContent']")
        element.click()
        # Coleta o texto do elemento
        s = element.text
        # Formata dados
        estado = s.split('\n')[0][8::]
        indice = s.split('\n')[1][8:12:].replace(',','.')
        data = formata_data(s.split('\n')[2][10::])
        # Verifica se já pegou o dado, se não pegou ele printa no arquivo
        if not estado in lista_dados:
            print(estados[estado]+','+data+','+indice)
            print_dados(estados[estado]+','+data+','+indice)
            lista_dados[estado] = estados[estado]+','+data+','+indice

    except:
        # Trata erro de quando ainda não existe o elemento
        time.sleep(0.001)

