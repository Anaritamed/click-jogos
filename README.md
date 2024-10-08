# Click Jogos 🎲
O sistema **Click Jogos** simula uma central de jogos na qual o usuário pode escolher um dentre os jogos disponíveis. **Todos os jogos são multijogadores**, para serem jogados com dois jogadores, que irão competir entre si e no final haverá um vencedor ou, em caso de mesma pontuação, empate.

# Jogos Disponíveis 🎮

### Jogo da forca
Nesse jogo, o jogador 1 escolhe uma palavra para o jogador 2 adivinhar, tendo como dica apenas o tema e a quantidade de letras da palavra. A cada rodada, o jogador 2 dá o palpite de uma letra da palavra. Se acertar, a letra é preenchida na palavra, já se o palpite estiver errado, é desenhada uma parte do corpo do boneco na forca. Caso o boneco seja desenhado por completo, o jogador 2 perde.

### Perguntados
Quiz de perguntas e respostas sobre determinado tema escolhido pelos jogadores no início do jogo. Ganha o jogador que responder mais perguntas corretamente e acumular mais pontos!

### Jogo da velha
O jogo consiste em preencher as lacunas vazias de um tabuleiro com os símbolos “X” (pelo jogador 1) e “O” (pelo jogador 2). Os jogadores jogam alternadamente e vence quem conseguir formar primeiro uma linha com três símbolos iguais. Caso nenhum dos dois consiga esse feito, o jogo termina com empate (velha).

# Tecnologias Utilizadas 🤖

### Haskell
Linguagem de programação puramente funcional, em que tudo é feito através de definições e chamadas de função. Haskell é uma linguagem estaticamente e fortemente tipada, que possui avaliação preguiçosa (lazy evaluation), na qual nada é feito até que seja necessário. Comando para instalar o Haskell e Cabal na máquina a partir do GHCup:
```
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

### Cabal
Sistema para construir e empacotar bibliotecas e programas Haskell. Ele define uma interface em comum para autores e distribuidores de pacotes construírem facilmente seus aplicativos de forma portátil. Para inicializá-lo, foi executado o seguinte comando, que cria um novo diretório com o arquivo .cabal que descreve o projeto e suas dependências.
```
cabal init
```

### Prolog
Linguagem de programação lógica, que é de mais alto nível que a programação imperativa ou funcional. Em Prolog, todo programa implementa uma relação, que pode ser avaliada como verdadeira ou falsa. A partir delas, podemos definir fatos, regras e consultas. Comando para instalar o SWI-Prolog, implementação em código aberto de Prolog:
```
sudo add-apt-repository ppa:swi-prolog/stable
sudo apt-get update
sudo apt-get install swi-prolog
```

# Configurações de Execução 👩🏿‍💻

### Em Haskell
```
cabal update
```
Atualiza o índice de pacotes do Hackage.

```
cabal build
```
Compila o código e depêndencias do projeto.

```
cabal run
```
Compila e roda o sistema Click Jogos, executando o arquivo Main.hs.

### Em Prolog
```
swipl -f -p menuInicial.pl
```
