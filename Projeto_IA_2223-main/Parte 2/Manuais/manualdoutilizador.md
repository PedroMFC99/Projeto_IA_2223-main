# **Manual de Utilizador**

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

[**Descrição do Funcionamento** ](#descrição-do-funcionamento)

[**Requisitos necessários** ](#requisitos-necessários)

[**Download e instalação do software LispWorks**
](#download-e-instalação-do-software-lispworks)

[**Tutorial de utilização da aplicação**
](#tutorial-de-utilização-da-aplicação)

[**FAQ** ](#faq)

[**Limitações da aplicação** ](#limitações-da-aplicação)

## **Introdução**

Este manual foi criado com o objetivo de ensinar o utilizador a
interagir com o programa, de forma a facilitar a sua compreensão das
opções do jogo e sua interação com o mesmo. O manual do utilizador
contém todas as informações necessárias para que o utilizador possa
explorar.

## **Descrição do Funcionamento**

O presente programa tem como finalidade proporcionar ao utilizador a
oportunidade de jogar o puzzle \"Dots and Boxes\" ou em português
"Pontos e Caixas". O objetivo deste jogo é conseguir fechar o maior
número possível de caixas, adicionando arcos entre pontos adjacentes
tanto na horizontal quanto na vertical, a partir de uma configuração
inicial do tabuleiro.

O jogador poderá jogar contra o computador ou então o computador poderá jogar contra o computador.

O objetivo primário do jogo é resolver um puzzle específico. O puzzle é
composto por um tabuleiro com m arcos horizontais e n arcos verticais,
resultando num total de m \* n caixas. Cada caixa é delimitada por
quatro pontos, ou seja, por quatro arcos, sendo dois horizontais e dois
verticais. Abaixo é mostrado um exemplo do puzzle, com a sua
configuração inicial e a sua configuração final.

![](https://drive.google.com/uc?id=1BcoQSFTQsLxO0k_90bbU0nutSyBKF6rP)

## **Requisitos necessários** 

Para utilizar o programa, é necessário instalar o software LispWorks,
pois o mesmo é essencial para o funcionamento da aplicação. O tutorial a
seguir é direcionado para utilizadores de Windows, mas para outros
sistemas operativos, basta verificar as instruções fornecidas no site do
software.

Em termos de requisitos de hardware, qualquer computador moderno deve
ser capaz de executar a aplicação sem problemas.

## **Download e instalação do software LispWorks**

Para instalar o software, primeiro é preciso registrar-se em [[LispWorks
Personal Edition](http://www.lispworks.com/downloads/)]

Depois de escolher o seu sistema operacional e preencher os campos
necessários, basta clicar no botão \"Proceed to Download\" para fazer
download do ficheiro executável de instalação.

![](https://drive.google.com/uc?id=1i5rDYQ9MaJW2-GYL8zVUk1HwOeZ7ACHg)

## **Tutorial de utilização da aplicação**

Após a instalação do LispWorks, deverá abrir o software e efetuar os
seguintes passos:

1.  Clicar na opção: "File-\>Open..".

![](https://drive.google.com/uc?id=1jd0jHG1feLOiBVR6Kjf4hcKHPwwI3jPw)

2.  Selecionar o ficheiro "projeto.lisp" assinalado e clicar em "abrir".

![](https://drive.google.com/uc?id=1TcdBXMfvf05ZQBENLsXsG2t_hwk2mPOa)

3.  Clicar no botão assinalado "Compile Buffer" para efetuar a
    compilação do ficheiro.

![](https://drive.google.com/uc?id=1NWJmVPaI08w0tTkeyDs1SXp3cVb8rdDq)

4.  Na janela do Listener, deve introduzir a função de iniciar, ou seja,
    a função (start) para inicializar a aplicação.

![](https://drive.google.com/uc?id=11_H8BUvpRH7kWjRrlIE5gDrXoyBrlEen)

5.  De seguida, será pedido para introduzir a diretoria dos ficheiros do
    programa de modo a compilar todos os ficheiros da aplicação. A
    diretoria dos ficheiros pode ser visualizada no passo 2.

![](https://drive.google.com/uc?id=1NKeb5Iu0J72F1uW1qerGXqSKEmhZPYxp)

6.  Será apresentado o menu principal da aplicação. Poderá escolher uma
    das três opções: iniciar uma procura em espaço de estados, ver como
    funciona o puzzle 'dots and boxes' e sair da aplicação.

![](https://drive.google.com/uc?id=1hx2p59EmVHCSkWLY7wGeXGcZFX-jt05W)

7.  Após escolher a opção 1, o utilizador deverá um escolher um
    tabuleiro para ser resolvido, introduzindo o número associdado ao tabuleiro.

![](https://drive.google.com/uc?id=1C2iuBphBObZ8kViX5AsyOUPfysSOKNdX)

8.  De seguida, será solicitado ao utilizador o número de caixas a serem
    fechadas.

![](https://drive.google.com/uc?id=1WMUgb06H3IJD1moaZXbvdIBkzp5VikeN)

9.  O utilizador deverá agora, escolher o algoritmo para realizar a
    procura, introduzindo a abreviatura do nome do algoritmo. Poderá
    escolher o algoritmo em largura (bfs), o algoritmo em profundidade
    (dfs) e o algoritmo A*.

![](https://drive.google.com/uc?id=1d7AXwNEFRaSiz3klPDY90faRD8I8QnX1)


## **FAQ** 

-   **O que posso fazer em caso de erro do programa?**

> Em caso de erro ou bug do programa, deverá clicar em "Abort" para
> limpar a consola.
>
> ![](https://drive.google.com/uc?id=1nWD1rT4g8yZjUfPWPGZia0uM7cMY7HLF)

-   **Os ficheiros têm de estar todos na mesma pasta?**

Sim, caso os ficheiros não estejam todos na mesma pasta, a aplicação não
irá conseguir compilá-los e, portanto, não inicializará.

## **Limitações da aplicação** 

Do ponto visto gráfico, é pouco interessante para o utilizador visualizar os arcos como pontos para se poder distinguir os arcos do jogador1 do jogador2. Seria visualmente mais apelativo a existência uma associação de cor aos arcos de cada jogador.


