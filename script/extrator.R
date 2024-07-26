## Cria tabela básica de dados: nome_autor, nome_orientador, titulo, ano

dados <- data.frame(id = 1:71)

dados <- dados |> dplyr::mutate(nome_autor = NA_character_, nome_orientador = NA_character_, titulo = NA_character_, ano = NA_integer_)

### nome_autor
## Extrai do nome do arquivo pdf armazenado em '~/tcc/data/' o nome completo do autor

## Obtém a lista de arquivos na pasta
files <- list.files("~/Erre/tcc/data/tcc/", full.names = FALSE)
arquivos <- list.files("~/Erre/tcc/data/tcc/", full.names = TRUE)

## Extrai o nome completo dos autores a partir do nome dos arquivos
nome_autor <- sub("_.*$", "", files)

## Incorpora os dados à tabela
dados$nome_autor <- stringr::str_to_upper(nome_autor)

### nome_orientador
## Extrair das páginas do arquivo pdf armazenado em '~/tcc/data/' o nome completo do orientador

# Carregar a biblioteca pdftools para ler PDFs
library(pdftools)

# Define o caminho da pasta onde estão os arquivos
caminho_pasta <- "~Erre/tcc/data/tcc/"

# Lista de professores conhecidos
lista_professores <- c(
  "Anderson Orestes", 
  "Anderson Lobato",
  "Carlos André", 
  "Cassiane de",
  "Claudete Rodrigues",
  "Eder Dion",
  "Eduardo Pitrez",
  "Elisa Girotti",
  "Fernando Amaral",
  "Felipe Wienke",
  "Felipe Franz Wienke",
  "Felipe Kern",
  "Francisco José",
  "Hector Cury",
  "Jaime John",
  "José Ricardo",
  "Liane Francisca",
  "Luciano Vaz Ferreira",
  "Marcelo Eibs",
  "Maria Claudia",
  "Miguel Antonio",
  "Pericles Antonio",
  "Rafael Fonseca",
  "Raquel Fabiana",
  "Renato Duro",
  "Rita de",
  "Salah Hassan",
  "Salah H.",
  "Sheila Stolz",
  "Simone de",
  "Simone Grohs",
  "Valdenir Aragao",
  "Valdenir Cardoso Aragão",
  "Vanessa Hernandez")

# Função para extrair o texto das 4 primeiras páginas e procurar o nome do orientador
extrair_orientador <- function(caminho_arquivo, lista_professores) {
  # Extrai o texto do PDF
  texto <- pdf_text(caminho_arquivo)
  
  # Junta o texto das 4 primeiras páginas (ou todas as páginas se o PDF tiver menos de 4 páginas)
  texto_completo <- paste(texto[1:min(4, length(texto))], collapse = " ")
  
  # Procura o nome do orientador na lista de professores
  orientador_encontrado <- NULL
  for (professor in lista_professores) {
    if (grepl(professor, texto_completo, ignore.case = TRUE)) {
      orientador_encontrado <- professor
      break
    }
  }
  
  return(orientador_encontrado)
}

# Aplica a função a todos os arquivos PDF
nomes_orientadores <- sapply(arquivos, extrair_orientador, lista_professores = lista_professores)

# Atualiza a coluna 'nome_orientador' no data frame 'dados'
dados$nome_orientador <- as.character(nomes_orientadores)

# Mapeamento de variantes de nomes para uniformização (sem variantes de um só nome)
mapa_variantes <- list(
  "Amanda Netto Brum" = c("Amanda Netto", "Amanda Brum", "Amanda"),
  "Anderson Orestes Cavalcante Lobato" = c("Anderson Orestes", "Anderson Lobato", "Anderson Cavalcante"),
  "Carlos André Sousa Birnfeld" = c("Carlos André", "Carlos Sousa", "Carlos Birnfeld"),
  "Cassia Lobato Marins" = c("Cassia Lobato", "Cassia Marins"),
  "Cassiane de Freitas Paixao" = c("Cassiane Freitas", "Cassiane Paixao", "Cassiane de"),
  "Cristiano Ruiz Engelke" = c("Cristiano Ruiz", "Cristiano Engelke"),
  "David Silva de Souza" = c("David Silva", "David Souza"),
  "Eder Dion de Paula Costa" = c("Eder Dion", "Eder Costa", "Eder Paula"),
  "Eduardo Pitrez de Aguiar Correa" = c("Eduardo Pitrez", "Eduardo Correa", "Eduardo Aguiar"),
  "Efendy Emiliano Maldonado Bravo" = c("Efendy Emiliano", "Efendy Maldonado", "Efendy Bravo"),
  "Elisa Girotti Celmer" = c("Elisa Girotti", "Elisa Celmer"),
  "Enio Duarte Fernandez Junior" = c("Enio Duarte", "Enio Fernandez", "Enio Junior"),
  "Fabiane Simioni" = c("Fabiane Simioni"),
  "Felipe da Costa de Lorenzi" = c("Felipe da Costa", "Felipe Lorenzi"),
  "Felipe Franz Wienke" = c("Felipe Franz", "Felipe Wienke"),
  "Felipe Kern Moreira" = c("Felipe Kern", "Felipe Moreira"),
  "Fernando Amaral" = c("Fernando Amaral"),
  "Fernando Goya Maldonado" = c("Fernando Goya", "Fernando Maldonado"),
  "Francisco José Soller de Mattos" = c("Francisco José", "Francisco Soller", "Francisco Mattos"),
  "Gabriela Braz Lucas" = c("Gabriela Braz", "Gabriela Lucas"),
  "Guaraciaba Ribeiro Duarte de Sousa" = c("Guaraciaba Ribeiro", "Guaraciaba Sousa"),
  "Hector Cury Soares" = c("Hector Cury", "Hector Soares"),
  "Jaime John" = c("Jaime John"),
  "Joanalira Corpes Magalhaes" = c("Joanalira Corpes", "Joanalira Magalhaes"),
  "Jose Ricardo Caetano Costa" = c("Jose Ricardo", "Jose Costa"),
  "Juliana Lapa Rizza" = c("Juliana Lapa", "Juliana Rizza"),
  "Liane Francisca Huning Pazinato" = c("Liane Francisca", "Liane Pazinato"),
  "Lucas Gonçalves Conceição" = c("Lucas Gonçalves", "Lucas Conceição"),
  "Lucas Machado Fagundes" = c("Lucas Machado", "Lucas Fagundes"),
  "Luciano Vaz Ferreira" = c("Luciano Vaz", "Luciano Ferreira"),
  "Marcelo Eibs Cafrune" = c("Marcelo Eibs", "Marcelo Cafrune"),
  "Maria Claudia Crespo Brauner" = c("Maria Claudia", "Maria Crespo", "Maria Brauner"),
  "Maria de Fatima Prado Gauterio" = c("Maria de Fatima", "Maria Gauterio"),
  "Mario Fernando Carvalho Ribeiro" = c("Mario Carvalho", "Mario Ribeiro"),
  "Miguel Antonio Silveira Ramos" = c("Miguel Antonio", "Miguel Silveira"),
  "Paula Correa Henning" = c("Paula Correa", "Paula Henning"),
  "Pericles Antonio Fernandes Goncalves" = c("Pericles Antonio", "Pericles Goncalves"),
  "Rafael Fonseca Ferreira" = c("Rafael Fonseca", "Rafael Ferreira"),
  "Raquel Fabiana Lopes Sparemberger" = c("Raquel Fabiana", "Raquel Lopes Sparemberger", "Raquel Sparemberger"),
  "Renato Duro Dias" = c("Renato Duro", "Renato Dias"),
  "Ricardo Morand Goes" = c("Ricardo Morand", "Ricardo Goes"),
  "Rita de Araujo Neves" = c("Rita de Araujo", "Rita Neves", "Rita de"),
  "Rubens Soares Vellinho" = c("Rubens Soares", "Rubens Vellinho"),
  "Salah Hassan Khaled Junior" = c("Salah Hassan", "Salah Khaled", "Salah H."),
  "Sheila Stolz da Silveira" = c("Sheila Stolz", "Sheila Silveira"),
  "Simone de Biazzi Avila Batista da Silveira" = c("Simone de Biazzi", "Simone Batista", "Simone Silveira", "Simone de"),
  "Simone dos Santos Paludo" = c("Simone dos Santos", "Simone Paludo"),
  "Simone Grohs Freire" = c("Simone Grohs", "Simone Freire"),
  "Thelmo de Carvalho Teixeira Branco Filho" = c("Thelmo Teixeira", "Thelmo Branco"),
  "Valdenir Cardoso Aragao" = c("Valdenir Cardoso", "Valdenir Aragao"),
  "Vanessa Hernandez Caporlingua" = c("Vanessa Hernandez", "Vanessa Caporlingua"),
  "Vinicius Castro da Silva" = c("Vinicius Castro", "Vinicius Silva"),
  "Wagner Silveira Feloniuk" = c("Wagner Silveira", "Wagner Feloniuk")
)

# Função para uniformizar os nomes dos orientadores
uniformizar_orientador <- function(nome_orientador, mapa_variantes) {
  nome_orientador_padrao <- nome_orientador
  for (nome_completo in names(mapa_variantes)) {
    if (nome_orientador %in% mapa_variantes[[nome_completo]]) {
      nome_orientador_padrao <- nome_completo
      break
    }
  }
  return(nome_orientador_padrao)
}

# Aplica a função de uniformização à coluna 'nome_orientador'
dados$nome_orientador <- sapply(dados$nome_orientador, uniformizar_orientador, mapa_variantes = mapa_variantes)

# Exibe os resultados
print(dados)

