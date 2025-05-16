# Ambientes de Execução (_Runtime Environments_)

> Capítulo 7, Compiladores: Princípios e práticas, 2004, Louden e Kenneth

Para que um programa seja executável, faz-se necessária a presença de uma
estrutura pela qual se dá o gerenciamento de memoria e a manutenção das
informações por ele requeridas: um **ambiente de execução**. É o objetivo deste
seminário explicitar aqui explicitar os mecanismos pelos quais um ambiente de
execução pode ser criado e manipulado.

A maioria das linguagens de programação se encontram associadas a um ambiente
que se enquadra em uma das três categorias seguintes:

---

1. Baseado em Pilhas

2. Totalmente estático

3. Totalmente dinâmico

Abordaremos estes em ordem a seguir.

> [!IMPORTANT] Esta é uma lista não exaustiva
>
> Além destas, existem formas híbridas entre estas categorias, mas estas não
> serão aqui abordadas

---

## Ambientes de execução baseados em pilhas

Iniciamos nossa exploração pelo ambiente de execução baseado em pilhas, por este
ser um modelo o qual nossa audiência pode estar familiarizada em função do
conteúdo didático das aulas de Arquitetura e Organização de Computadores, e do
uso habitual de linguagens de programação que fazem uso deste tipo de ambiente:
o C e o C++.

No que tange a este ambiente de execução, existem tipos, começaremos expondo
aspectos gerais para depois abordá-los um a um em ordem crescente de
complexidade.

Alguns aspectos do ambiente de execução baseados em pilhas encontram paralelo
nos demais tipos de ambientes, e como tal serão oportunamente retomados em vez
de reiterados.

---

### Aspectos gerais de ambientes de execução baseados em pilha

---

#### Regiões de memória

Em um ambiente de execução baseado em pilhas, a memória alocada para a execução
de programas é delimitada em duas regiões principais:

- _Área de código_: Onde são armazenadas informações imutáveis durante a
  execução (as instruções, os valores constantes) e variáveis globais. Isto é,
  todas as informações para as quais o endereço de memória é conhecido e
  estático.
- _Área de dados_: Todos os demais dados, armazenados conforme cada contexto de
  execução do programa.

---

Por vez, a área de dados pode ser delimitadas em outras três regiões principais:

- Área de pilha (_stack_): onde são armazenados dados cuja alocação ocorre na
  forma LIFO (_last in, first out_). Frequentemente esta é utilizada para
  criação de uma pilha de procedimentos (ou funções).

- Área de _heap_: onde são armazenados dinamicamente quaisquer outros dados que
  não seguem essa ordenação (ex.: como fazem os ponteiros na linguagem de
  programação C).

> [!IMPORTANT] "Heap", neste contexto, é uma área de memória linear simples.
>
> Não tem qualquer relação com a estrutura de dados "Heap" vista em algoritmos
> como o Heapsort.

- Espaço livre: Área de memória disponível para alocação tanto pela pilha quanto
  pelo heap

---

Dada a porção de memória alocada para o programa representada como subdivisões
de uma área contígua, temos:

![Diagrama da organização da memória de um programa em execução](images/snapshot_2025-05-12_20-13-26.png)

Onde as setas indicam a direção de "crescimento" da pilha e do heap. Ou seja,
novos endereços de memória são alocados de tal forma que o topo da pilha e o
heap convergem a uma mesma posição de memória. Não obstante, a estrutura do
ambiente de execução **não é** contígua: esta faz uso tanto de registradores
quanto memória RAM. Em algumas organizações, a pilha e o heap são alocados em
seções separadas da memória, em vez de ocupar a mesma área.

---

Na pilha de procedimentos ocupam **registros de ativação de procedimentos**, que
contém memória alocada para dados locais de um procedimento ou função, quando
este é acessado. Um registro de ativação é composto por, no mínimo, espaço para
os seguintes elementos:

- Argumentos (ou parâmetros)
- Quaisquer dados locais
- Endereço de retorno

De tal forma que processos podem ser gerados dada a passagem de parâmetros,
persistirem em uma memória local, e eventualmente serem desempilhados em uma
sequência de retorno. Ao conjunto de registros de ativação mantidos em uma pilha
denomina-se o **quadro de pilhas**.

---

#### Registradores

Registradores são convenientes, e são utilizados, para armazenar valores
pertinentes ao momento atual da execução de um programa. Sejam estes locais,
globais e, principalmente, temporários. Além destes, processadores usualmente
possuem e reservam registradores ao uso específico do acompanhamento da
execução, são estes:

- **Contador de programa (_program counter_, pc)**: indica na memória da
  instrução a ser executada, é incrementado durante ou após o estágio de busca
  de instrução

- **Ponteiro de pilha (_stack pointer_, sp)**: indica a posição de memória atual
  do topo da pilha.

- **Ponteiro de quadros (_function pointer_, fp)**: indica a posição de memória
  na pilha em que se inicia o armazenamento do registro de ativação corrente.
  Uma cópia deste, denominada _old fp_ também pode ser gerada pela ativação
  seguinte para apontar o registro de ativação imediatamente anterior.

> [!NOTE] Para que serve o fp?
>
> Enquanto o sp pode mover-se durante a execução de uma função, o fp não se move
> e provê um ponto de referência estável do contexto local desta. A partir
> deste, dados locais podem ser encontrados quando armazenados na pilha a partir
> de um deslocamento com relação à fp. Por isso, setar o sp para a posição do fp
> implica apagar o contexto local da função (_stack unwind_).

- **Ponteiro de argumentos (argument pointer, ap):** aponta para um argumento na
  região de dados reservada para estes no registro de ativação.

> [!NOTE] Para que serve o ap?
>
> O uso do ap é desincentivado em favor de se carregar os valores dos argumentos
> em registradores. Mas, nos casos em que uma função possui um número variável
> de argumentos, seu uso pode ser necessário para empilhá-los e indicar a
> posição na pilha do último argumento.

---

#### Sequências de ativação e retorno

Também denominadas enquanto o _prólogo_ e _epílogo_, são conjuntos de operações
que precedem e seguem a execução de cada função, respectivamente. As operações
que os compõem são detalhadas à seguir:

---

##### Sequência de ativação

- Computação dos argumentos e armazenamento destas em posições adequadas no novo
  registro de ativação (por exemplo, ao colocá-los na pilha em ordem);
- Armazenamento do _fp_ (que em breve será o _old fp_);
- Atribuição do valor de _sp_ para o _fp_;
- Armazenamento do endereço de retorno;
- Deslocamento do _pc_ para apontar à porção de código correspondente a função
  atual.

---

##### Sequência de retorno

- Atribuição do valor de _fp_ para _sp_;
- Atribuição do valor de _old fp_ para _fp_;
- Atribuição do endereço de retorno ao _pc_.

---

### Ambientes de execução baseados em pilha sem procedimentos locais

Nas linguagens de programação em que todos as funções são globais (como a
linguagem C), no que toca a manipulação de registros de ativação faz-se
necessário registrar apenas a ativação corrente e a ativação imediatamente
anterior a esta. Para tal faz-se uso de dois registradores, o _fp_ mais outro
denominado _vinculação de controle_, ou _fp old_, cujo valor é armazenado nos
dados locais à função.

---

#### Exemplo

Considere a seguinte aplicação do algoritmo de Euclides, em C, para computar o
máximo divisor comum (_greatest common divisor, GCD_):

```c
#include <stdio.h>
#include <stdlib.h>

int gcd(int u, int v) {
    if v
        return gcd(v, u % v);
    return u;
}

int main() {
    int u, v;
    scanf(" %d %d", &u, &v);
    printf("%d\n", gcd(u, v));
    return EXIT_SUCCESS;
}
```

Deste programa resultaria a seguinte organização da memória no ambiente de
execução:

![Registros de controle empilhados, ligados entre si por vinculação de
controle](images/snapshot_2025-05-15_18-48-28.png)

## Ambientes de execução totalmente estáticos

O tipo mais simples de ambiente de execução disponível, onde todos os dados
possuem posições fixas na memória, é denominado "totalmente estático". Para
fazer uso deste ambiente, a linguagem de programação necessita não possuir as
condições necessárias a implementação e uso da pilha e do _heap_:

- Ponteiros;
- Alocação dinâmica;
- Recursão.

> [!NOTE] FORTRAN 77
>
> é um exemplo de linguagem de programação que satisfaz estas condições.

---

A organização do ambiente de execução totalmente estático pode ser representado
da seguinte maneira:

![Diagrama da organização de memória de um ambiente de execução totalmente
estático](images/snapshot_2025-05-14_09-57-59.png)

Um ambiente deste tipo necessita de apenas um registrador para acompanhar o
atual estado de execução: o registrador de retorno.

---

> Inserir exemplo de código FORTRAN77, chamar atenção para o fato de que este
> faz uso de variáveis globais e vetores de tamanho fixo.

---

## Ambientes de execução baseados em pilhas

## Memória dinâmica

## Mecanismos para passagem de parâmetros

## Ambiente de execução para a linguagem TINY
