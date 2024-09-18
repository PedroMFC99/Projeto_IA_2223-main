# **Manual Técnico**

## **Jogo 'Dots and Boxes'**

![](https://drive.google.com/uc?id=1vcXmXlXlJGzmpBU6Rv4jxVC5FXrd99xT)

## **Curso:**

Licenciatura em Engenharia Informática

## **Unidade Curricular:**

Inteligência Artificial

## **Docente de Laboratório:**

Filipe Mariano

## **Trabalho elaborado por:**

Diogo Filipe Brália Letras, Nº 202002529

Pedro Manuel Fialho Cunha, Nº 20200757

## Índice

[**Introdução** ](#introdução)

[**Ambiente de desenvolvimento** ](#ambiente-de-desenvolvimento)

[**Algoritmos** ](#algoritmos)

[**Funções** ](#funções)

[**Análise dos resultados** ](#análise-dos-resultados)

[**Limitações do projeto** ](#limitações-do-projeto)

[**Conclusão** ](#conclusão)

## **Introdução**

No âmbito da unidade curriclar de Inteligência Artificial, os docentes
propuseram o desenvolvimento de um projeto em LISP intitulado \"Dots and
Boxes\". O objetivo deste projeto é aplicar os conhecimentos adquiridos
durante as aulas na criação de um jogo do tipo puzzle.

O puzzle consiste em fechar um determinado número de caixas a partir de
uma configuração inicial do tabuleiro. Para isso, será necessário
desenhar arcos entre dois pontos adjacentes na horizontal ou na
vertical. Quando quatro arcos são desenhados entre pontos adjacentes,
uma caixa é fechada.

Pode ser feita uma partida humano contra computador ou computer contra computador.

## **Ambiente de desenvolvimento**

Abaixo, podem ser visualizadas as ferramentas utilizadas durante o
desenvolvimento do projeto.

| Ferramentas        | Propósito                                     |
| ------------------ | --------------------------------------------- |
| LispWorks          | Utilizada no desenvolvimento                  |
| Visual Studio Code | Utilizada na escrita do relatório             |
| Github             | Ferramenta para gerir as versões de controlo. |

**Requisitos de hardware das ferramentas utilizadas**

Segue-se os requisitos necessários para poder correr as ferramentas sem
constrangimentos.

Nota: os seguintes requisitos supõem que a máquina está a correr o
sistema operativo Windows (64 bits).

| Ferramentas        | Processador           | Espaço no disco |
| ------------------ | --------------------- | --------------- |
| LispWorks          | AMD64, Intel64        | 170MB           |
| Visual Studio Code | 1.6Ghz ou mais rápido | 500MB           |
| Github             | \-                    | \-              |

**Estrutura do projeto**

O projeto contém três ficheiros principais e um ficheiro auxiliar:

-   Jogo.lisp: este ficheiro é responsável pela interação com o
    utilizador, mostrando menus no ecrã com as opções disponíveis.

-   Puzzle.lisp: este ficheiro é responsável pela gestão das regras do
    jogo, incluindo entre outros: operadores, validações e outras
    funções úteis para o desenvolvimento do projeto.

-   Algoritmo.lisp: este ficheiro contém tudo o que esteja relacionado a
    parte algorítmica. Neste caso mais especificamente o algoritmo alfa-beta e as suas funções auxiliares. Responsável pela inteligência das jogadas do computador.

## **Algoritmos**

Foram propostos 3 algoritmos para serem implementados e também outros 3
algoritmos opcionais.

De seguida, serão detalhados os algoritmos que foram implementados no projeto.

**Minimax**

O algoritmo Minimax funciona ao explorar todas as possíveis jogadas e as suas consequências, avaliando a posição final como um valor de vitória ou derrota. O algoritmo então escolhe a jogada que maximiza o valor de vitória para o jogador atual e minimiza o valor de vitória para o jogador adversário. É chamado minimax porque o algoritmo maximiza o valor mínimo esperado para o jogador oponente.

Segue abaixo o pseudocódigo do algoritmo Minimax.

    function minimax(node, depth, maximizingPlayer)
        if depth = 0 or node is a terminal node
            return the value of node
        if maximizingPlayer
            bestValue := -infinity
            for each child of node
                v := minimax(child, depth - 1, FALSE)
                bestValue := max(bestValue, v)
            return bestValue
        else
            bestValue := +infinity
            for each child of node
                v := minimax(child, depth - 1, TRUE)
                bestValue := min(bestValue, v)
            return bestValue

**Alfa-Beta**

O algoritmo Alfa-Beta é uma variação do algoritmo Minimax utilizada para otimizar a sua procura em jogos de tabuleiro. Funciona limitando a busca em árvores de jogadas, descartando ramos da árvore que são considerados inúteis para a decisão final. O algoritmo faz isso através da manutenção de duas variáveis, chamadas alfa e beta, que representam os melhores valores encontrados até agora para o jogador atual e o jogador adversário, respetivamente. Se o algoritmo encontra um valor de vitória para o jogador atual que é melhor do que o valor atual de beta do jogador adversário, o algoritmo pára a busca nessa ramo, pois essa jogada já é suficiente para garantir a vitória. Isso permite uma busca mais eficiente, pois evita a verificação de jogadas que já sabemos que não serão escolhidas.

Segue abaixo o pseudocódigo do algoritmo Alfa-Beta.

    function alphabeta(node, depth, alpha, beta, maximizingPlayer)
        if depth = 0 or node is a terminal node
            return the value of node
        if maximizingPlayer
            for each child of node
                alpha := max(alpha, alphabeta(child, depth - 1, alpha, beta, FALSE))
                if beta <= alpha
                    break
            return alpha
        else
            for each child of node
                beta := min(beta, alphabeta(child, depth - 1, alpha, beta, TRUE))
                if beta <= alpha
                    break
            return beta

## **Funçao de utilidade**

A função de utilidade atribui um valor de utilidade a cada estado do jogo. Esse valor de utilidade é usado pelo algoritmo para determinar qual é o melhor caminho a seguir. O algoritmo alfa-beta usa essa função para maximizar a utilidade para o jogador atual e minimizar a utilidade para o adversário, a fim de encontrar o caminho ótimo para vencer o jogo.

## **Análise dos resultados**

De seguida apresenta-se os resultados da análise aos algoritmos.

Problema A -- 3 caixas a fechar

|                      | BFS          | DFS        | A* HeuristicaOG | A* HeuristicaALT
| -------------------- | ------------ | ---------- | ----------      | ---------- 
| Tempo decorrido      | 0            | 0          | 0               |  0
| Profundidade         | 2            | 6          | 2               |  2
| Nós gerados          | 1129         | 82         | 313             |  298
| Nós expandidos       | 81           | 6          | 22              |  21
| Penetrância          | 0.0017714792 | 0.07317073 | 0.0063897763    |  0.0067114094
| Fator de ramificação | 8.531752     | 1.0284217  | 4.2116528       |  4.097966

Problema B -- 7 caixas a fechar

|                      | BFS         | DFS       | A* HeuristicaOG | A* HeuristicaALT
| -------------------- | ----------- | --------- | ---------       | --------- 
| Tempo decorrido      | 0           | 0         | 0               | 0
| Profundidade         | 1           | 1         | 1               | 1
| Nós gerados          | 198         | 16        | 198             | 198
| Nós expandidos       | 15          | 1         | 15              | 15
| Penetrância          | 0.005050505 | 0.0625    | 0.005050505     | 0.005050505
| Fator de ramificação | 15.125589   | 1.0852652 | 15.125589       | 15.125589

Problema C -- 10 caixas a fechar

|                      | BFS | DFS         | A* HeuristicaOG | A* HeuristicaALT
| -------------------- | --- | ----------- | ---------       | --------- 
| Tempo decorrido      | \-  | 0           | -               | -
| Profundidade         | \-  | 14          | -               | - 
| Nós gerados          | \-  | 162         | -               | - 
| Nós expandidos       | \-  | 14          | -               | - 
| Penetrância          | \-  | 0.086419754 | -               | -
| Fator de ramificação | \-  | 1.0284217   | -               | -


## **Limitações do projeto**

A eficiência da jogada do computador pode não ser a melhor, devido à sua a análise de todas possibilidades de colocar um arco.
Seria também interessante ter uma associação de cores para se poder distinguir nitidamente entre os arcos do jogador 1 e do jogador 2 ao invés da utilização de pontos.

Todos os requisitos foram implementados à exceção da procura quosciente.


## **Conclusão**

A realização deste projeto foi algo desafiante, pois a aplicação foi
desenvolvida na linguagem Lisp, com um paradigma funcional. Esta
abordagem à programação difere em relação ao que os membros do grupo
estavam habituados até agora.

O projeto serviu também para solidificar alguns conceitos que foram
abordados em outras unidades curriculares anteriores e que foram
novamente revisitados nesta unidade curricular de Inteligência
Artificial, nomeadamente o conceito de recursividade.

Portanto, sem dúvidas que a experiência de desenvolvimento deste
projeto, deverá ser útil no futuro académico e profissional dos membros
do grupo.
