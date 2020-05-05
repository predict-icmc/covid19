from selenium import webdriver
import time

# Printa dados da media dos índices no arquivo
def print_dados(dados):
    arq = open('./dados2.csv','a')
    arq.write(dados+'\n')

# Site para scraping
url = 'https://public.tableau.com/views/MKTScoredeisolamentosocial/VisoGeral?%3Aembed=y&%3AshowVizHome=no&%3Adisplay_count=y&%3Adisplay_static_image=y'

browser = webdriver.Firefox() # Abre browser para scraping
browser.get(url) # Abre site

dados = {}
while True:
    try: 
        element = browser.find_element_by_xpath("//div[@class='tab-ubertipContent']")
        element.click()
        s = element.text
        data = s.split('\n')[1].replace('/','-')
        indice = s.split('\n')[4][:4:].replace(',','.')
        if not data in dados.keys():
            dados[data] = data+','+indice
            print_dados(data+','+indice)
            print(data+','+indice+'\n')
    except:
        time.sleep(0.1)
