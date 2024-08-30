# Click Jogos 🎲
O sistema **Click Jogos** simula uma central de jogos na qual o usuário pode escolher um dentre os jogos disponíveis. **Todos os jogos são multijogadores**, para serem jogados com dois jogadores, que irão competir entre si e no final haverá um vencedor ou, em caso de mesma pontuação, empate.

# Jogos Disponíveis 🎮

### Jogo da velha
O jogo consiste em preencher as lacunas vazias de um tabuleiro com os símbolos “X” (pelo jogador 1) e “O” (pelo jogador 2). Os jogadores jogam alternadamente e vence quem conseguir formar primeiro uma linha com três símbolos iguais. Caso nenhum dos dois consiga esse feito, o jogo termina com empate (velha).

### Perguntados
Quiz de perguntas e respostas sobre determinado tema escolhido pelos jogadores no início do jogo. Ganha o jogador que responder mais perguntas corretamente e acumular mais pontos!

### Jogo da forca
Nesse jogo, o jogador 1 escolhe uma palavra para o jogador 2 adivinhar, tendo como dica apenas o tema e a quantidade de letras da palavra. A cada rodada, o jogador 2 dá o palpite de uma letra da palavra. Se acertar, a letra é preenchida na palavra, já se o palpite estiver errado, é desenhada uma parte do corpo do boneco na forca. Caso o boneco seja desenhado por completo, o jogador 2 perde.

# Para executar o sistema ⏩ 
`cabal run`

# Tecnologias
Tecnologias usadas no projeto.  

## Cabal
1. Inicialização e Configuração  
`cabal init`: Foi usado para criar um novo diretório com uma estrutura básica e um arquivo .cabal que descreve o projeto e suas dependências.  

`cabal update`: Use para atualizar o índice de pacotes do Hackage.

2. Construção e Execução  
`cabal build`: Use para compilar o código-fonte do seu projeto. Isso inclui compilar todas as dependências e o próprio código do projeto. Execute este comando após fazer alterações no código ou nas dependências para garantir que tudo esteja construído corretamente.  

`cabal run`: Use para compilar e executar o executável definido no seu arquivo .cabal.
