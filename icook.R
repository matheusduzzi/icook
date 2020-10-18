library(rvest)
library(dplyr)
library(stringr)

# pegando links de paginas de receitas
links = c()
for (i in 1:13) {links[i] = paste0('https://pt.petitchef.com/lista-de-receitas-page-',i)}

# pegando links de receitas por pagina
link_receitas = c()
k = 0 
for (i in 1:13) {
  for (j in 1:24) {
    k = k + 1
    text = paste0('//*[@id="recipe-list"]/div[',j)
    text = paste0(text,']/div[2]/h2/a')
    ops = read_html(links[i]) %>% html_nodes(xpath = text) %>% html_attrs()
    if(length(ops) != 0){
      link_receitas[k] = ops[[1]][["href"]]
    }
    if(k >= 200){break}
  }
}

# apontando alergias
link_receitas = na.exclude(link_receitas)
alergias = c('chocolate','atum')
ind_alergias = c()

for (j in 1:length(link_receitas)) {
  
  receita <- "https://pt.petitchef.com/"
  receita <- paste0(receita,link_receitas[j])
  page_test <- read_html(receita)
  tabela_ingredients = c()
  
  for (i in 0:20) {
    
    path = paste0('//*[@id="il-',i)
    path = paste0(path,'"]')
    aux =  page_test %>% html_nodes(xpath = path) %>% html_text()
    
    if(length(aux) == 0){break}
    
    tabela_ingredients[i+1] = aux
    
  }
  
  for (k in 1:length(alergias)) {
    for (w in 1:length(tabela_ingredients)) {
      
      if(length(grep(alergias[k],tabela_ingredients[w])) != 0){
        ind_alergias[j] = "alergia"
      }
      
    }
  }
  
  
}

# tirando as alergias e separando por tipo de prato
rec_alerg = data.frame(receita = link_receitas,ind = c(0))
rec_alerg$ind[1:length(ind_alergias)] = ind_alergias

tipo_prato = str_split(rec_alerg$receita, pattern = "/")

rec_alerg$tipo = c(0)

for (i in 1:length(tipo_prato)) {
  
  rec_alerg$tipo[i] = tipo_prato[[i]][[3]]
  
}

rec_alerg$ind = ifelse(is.na(rec_alerg$ind),0,rec_alerg$ind)

aperitivo = rec_alerg %>% filter(ind != "alergia" & tipo == "aperitivo") %>% select(receita)
sobremesa = rec_alerg %>% filter(ind != "alergia" & tipo == "sobremesa") %>% select(receita)
prato_principal = rec_alerg %>% filter(ind != "alergia" & tipo == "prato-principal") %>% select(receita)
acompanhamento = rec_alerg %>% filter(ind != "alergia" & tipo == "acompanhamento") %>% select(receita)

sugestao_do_chef = data.frame(aperitivo = sample(aperitivo$receita,1), prato_principal = sample(prato_principal$receita,1)
                              ,acompanhamento = sample(acompanhamento$receita,1), sobremesa = sample(sobremesa$receita,1))

# montando o cardapio
for (i in 1:ncol(sugestao_do_chef)) {
  
  receita <- "https://pt.petitchef.com/"
  receita <- paste0(receita,sugestao_do_chef[1,i])
  page <- read_html(receita)
  sugestao_do_chef[1,i] =  page %>% html_nodes(xpath = '//*[@id="p-content"]/div[1]/h1') %>% html_text()
  sugestao_do_chef[2,i] = receita

  
}

print("A sugestão do chef para você é:")
print("Como aperitivo:")
print(sugestao_do_chef$aperitivo[2])
print("Para o prato principal:")
print(sugestao_do_chef$prato_principal[2])
print("Com o acompanhamento:")
print(sugestao_do_chef$acompanhamento[2])
print("E para fechar, a sobremesa:")
print(sugestao_do_chef$sobremesa[2])

