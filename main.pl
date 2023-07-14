:- use_module(library(lists)).
:- dynamic sujeira/1.

% Definição da matriz
matriz([
    [a, b, c, d],
    [e, f, g, h],
    [i, j, k, l],
    [m, n, o, p]
]).


% Definição do estado inicial, obstáculos e sujeiras
estado_inicial(a).

obstaculo(o).
obstaculo(c).
obstaculo(g).
obstaculo(i).


sujeira(d).
sujeira(f).


percorre(1) :- matriz(Matriz), findall(X, sujeira(X),Lista_Sujeiras), percorre_matriz_arestas(Matriz), %Cria todas as arestas possíveis, exceto para bloqueados
percorre_distancia_sujeiras(Lista_Sujeiras). %Cria relacoes das distancias manhattan para cada sujeira

% Percorre a lista de sujeiras e cria as relações de distância manhattan
percorre_distancia_sujeiras([]).
percorre_distancia_sujeiras([Sujeira|Resto]) :-
    percorre_distancia_sujeiras(Resto),
    matriz(Matriz),
    percorre_matriz(Matriz, Sujeira).

% Percorre a matriz e chama percorre_linha para cada linha
percorre_matriz([], _).
percorre_matriz([Linha|Resto], Sujeira) :-
    percorre_linha(Linha, Sujeira),
    percorre_matriz(Resto, Sujeira).

% Percorre uma linha da matriz e calcula a distância manhattan entre cada elemento e a sujeira
percorre_linha([], _).
percorre_linha([Atual|Resto], Sujeira) :-
    distancia_manhattan_aux(Atual, Sujeira),
    percorre_linha(Resto, Sujeira).

% Encontra a sujeira mais próxima do estado inicial
sujeira_mais_proxima([Min], Min, _).
sujeira_mais_proxima([H,K|Resto], Min, Estado_Inicial) :-
    distancia_manhattan_de(Estado_Inicial, H, D_H),
    distancia_manhattan_de(Estado_Inicial, K, D_K),
    D_H < D_K,
    sujeira_mais_proxima([H|Resto], Min, Estado_Inicial).
sujeira_mais_proxima([H,K|Resto], Min, Estado_Inicial) :-
    distancia_manhattan_de(Estado_Inicial, H, D_H),
    distancia_manhattan_de(Estado_Inicial, K, D_K),
    D_H >= D_K,
    sujeira_mais_proxima([K|Resto], Min, Estado_Inicial).


%Hill Climbing
main_hill([], _).
main_hill(Sujeiras, Estado_Inicial) :-
    sujeira_mais_proxima(Sujeiras, Sujeira_Atual, Estado_Inicial),
    distancia_manhattan_de(Estado_Inicial, Sujeira_Atual, Distancia),
    hillClimb([[Distancia,Estado_Inicial]], Solucao, Custo, Sujeira_Atual),

    write('Hill Climbing:\n'),
    format('   Solution: ~w\n', [Solucao]),
    format('   Cost: ~w\n', [Custo]),
    
    subtract(Sujeiras, [Sujeira_Atual], L),
    main_hill(L, Sujeira_Atual).

hillClimb([[_,No|Caminho]|_], Solucao, '-', NoSujeira) :-
    sujeira(No),
    No = NoSujeira,
    reverse([No|Caminho], Solucao).
hillClimb([Caminho|Caminhos], Solucao, Custo, NoSujeira) :-
    estendeH(Caminho, NovosCaminhos, NoSujeira),
    ordenaF(NovosCaminhos, CaminhosOrd),
    concatena(CaminhosOrd, Caminhos, CaminhosTotal),
    hillClimb(CaminhosTotal, Solucao, Custo, NoSujeira).


% Best First
main_best([], _).
main_best(Sujeiras, Estado_Inicial) :-
    sujeira_mais_proxima(Sujeiras, Sujeira_Atual, Estado_Inicial),
    distancia_manhattan_de(Estado_Inicial, Sujeira_Atual, Distancia),
    bestFirst([[Distancia,Estado_Inicial]], Solucao, Custo, Sujeira_Atual),
    write('Best-First Search:\n'),
    format('   Solution: ~w\n', [Solucao]),
    format('   Cost: ~w\n', [Custo]),
    subtract(Sujeiras, [Sujeira_Atual], L),
    main_best(L, Sujeira_Atual).

bestFirst([[_,No|Caminho]|_], Solucao, '-', NoSujeira) :-
    sujeira(No),
    No = NoSujeira,
    reverse([No|Caminho], Solucao).
bestFirst([Caminho|Caminhos], Solucao, Custo, NoSujeira) :-
    estendeH(Caminho, NovosCaminhos, NoSujeira),
    ordenaF(NovosCaminhos, CaminhosOrd),
    concatena(CaminhosOrd, Caminhos, CaminhosTotal),
    bestFirst(CaminhosTotal, Solucao, Custo, NoSujeira).

% Branch And Bound
main_bound([], _).
main_bound(Sujeiras, Estado_Inicial) :-
    sujeira_mais_proxima(Sujeiras, Sujeira_Atual, Estado_Inicial),
    distancia_manhattan_de(Estado_Inicial, Sujeira_Atual, Distancia),
    branchAndBound([[0, Estado_Inicial]], Solucao, Custo, Sujeira_Atual),
    write('Branch and Bound:\n'),
    format('   Solution: ~w\n', [Solucao]),
    format('   Cost: ~w\n', [Custo]),
    subtract(Sujeiras, [Sujeira_Atual], L),
    main_bound(L, Sujeira_Atual).

branchAndBound([[G,No|Caminho]|_], Solucao, G, NoSujeira) :-
    sujeira(No),
    No = NoSujeira,
    reverse([No|Caminho], Solucao).
branchAndBound([Caminho|Caminhos], Solucao, G, NoSujeira) :-
    estendeG(Caminho, NovosCaminhos),
    concatena(Caminhos, NovosCaminhos, CaminhosTotal),
    ordenaF(CaminhosTotal, CaminhosTotOrd),
    branchAndBound(CaminhosTotOrd, Solucao, G, NoSujeira).

% A Estrela
main_estrela([], _).
main_estrela(Sujeiras, Estado_Inicial) :-
    sujeira_mais_proxima(Sujeiras, Sujeira_Atual, Estado_Inicial),
    distancia_manhattan_de(Estado_Inicial, Sujeira_Atual, Distancia),

    aEstrela([[Distancia, 0, Distancia, Estado_Inicial]], Solucao, Custo, Sujeira_Atual),

    write('A*:\n'),
    format('   Solution: ~w\n', [Solucao]),
    format('   Cost: ~w\n', [Custo]),
    subtract(Sujeiras, [Sujeira_Atual], L),
    main_estrela(L, Sujeira_Atual).


aEstrela([[G,_,_,No|Caminho]|_], Solucao, G, NoSujeira) :-
    sujeira(No),
    No = NoSujeira,
    reverse([No|Caminho], Solucao).
aEstrela([Caminho|Caminhos], Solucao, G, NoSujeira) :-
    estendeF(Caminho, NovosCaminhos, NoSujeira),
    concatena(Caminhos, NovosCaminhos, CaminhosTotal),
    ordenaF(CaminhosTotal, CaminhosTotOrd),
    aEstrela(CaminhosTotOrd, Solucao, G, NoSujeira).

% ------------------------------------------------------- %

% Encontra i, j de um determinado elemento na matriz
encontrar_posicao(Elemento, Matriz, Linha, Coluna) :-
    nth0(Linha, Matriz, LinhaElementos),
    nth0(Coluna, LinhaElementos, Elemento).

% Acessa um elemento dado as coordenadas i, j
acessar_elemento(Matriz, X, Y, Elemento) :-
    nth0(X, Matriz, Linha),
    nth0(Y, Linha, Elemento),
    \+ obstaculo(Elemento).

% Calcula a distância manhattan entre dois pontos
distancia_manhattan(X1, Y1, X2, Y2, Distancia) :-
    Distancia is abs(X2 - X1) + abs(Y2 - Y1).

% Calcula a distância manhattan entre dois elementos da matriz
distancia_manhattan_aux(Atual, Sujeira) :-
    matriz(Matriz),
    encontrar_posicao(Atual, Matriz, X_Atual, Y_Atual),
    encontrar_posicao(Sujeira, Matriz, X_Sujeira, Y_Sujeira),
    distancia_manhattan(X_Atual, Y_Atual, X_Sujeira, Y_Sujeira, Distancia),
    encontrar_posicao(Atual, Matriz, Linha, Coluna),
    assertz(distancia_manhattan_de(Atual, Sujeira, Distancia)).

% Define os vizinhos de um elemento da matriz
define_vizinhos_aux(Atual, X_atual, Y_atual) :-
    matriz(Matriz),
    write(Atual),
    X_right is X_atual + 1,
    X_left is X_atual - 1,
    Y_up is Y_atual + 1,
    Y_down is Y_atual - 1,

    % Todas as direções
    (   acessar_elemento(Matriz, X_right, Y_atual, V1),
        assertz(aresta(Atual, V1))
    ;
        true
    ),
    (   acessar_elemento(Matriz, X_left, Y_atual, V2),
        assertz(aresta(Atual, V2))
    ;
        true
    ),
    (   acessar_elemento(Matriz, X_atual, Y_up, V3),
        assertz(aresta(Atual, V3))
    ;
        true
    ),
    (   acessar_elemento(Matriz, X_atual, Y_down, V4),
        assertz(aresta(Atual, V4))
    ;
        true
    ).

% Define os vizinhos para todos os elementos da matriz
define_vizinhos(Atual) :- 
    obstaculo(Atual);
    (matriz(Matriz),
    encontrar_posicao(Atual,Matriz,X_Atual,Y_Atual),
    define_vizinhos_aux(Atual, X_Atual, Y_Atual)).

% Percorre a matriz criando as arestas possíveis
percorre_matriz_arestas([]).
percorre_matriz_arestas([Linha|Resto]) :-
    percorre_linha_arestas(Linha),
    percorre_matriz_arestas(Resto).

percorre_linha_arestas([]).
percorre_linha_arestas([Atual|Resto]) :-
    define_vizinhos(Atual),
    percorre_linha_arestas(Resto).

% Concatena duas listas
concatena([], L, L).
concatena([X|L1], L, [X|L2]) :-
          concatena(L1, L, L2).

% Ordena uma lista pelo valor F
ordenaF(Caminhos, CaminhosOrd) :-
    quicksortF(Caminhos, CaminhosOrd).

particionarF(_, [], [], []).
particionarF(X, [Y|Cauda], [Y|Menor], Maior) :-
    maiorF(X, Y),
    !,
    particionarF(X, Cauda, Menor, Maior).
particionarF(X, [Y|Cauda], Menor, [Y|Maior]) :-
    particionarF(X, Cauda, Menor, Maior).

quicksortF([], []).
quicksortF([X|Cauda], ListaOrd) :-
    particionarF(X, Cauda, Menor, Maior),
    quicksortF(Menor, MenorOrd),
    quicksortF(Maior, MaiorOrd),
    concatena(MenorOrd, [X|MaiorOrd], ListaOrd).

% Verifica se um elemento está presente em uma lista
membro(X, [X|_]) :- !.
membro(X, [_|C]) :-
    membro(X, C).

% Estende o caminho atual para os vizinhos
estende(Caminho, NovosCaminhos).
estende([No|Caminho], NovosCaminhos) :-
    findall([NovoNo,No|Caminho],
            (
                s(No,NovoNo),
                not(membro(NovoNo,[No|Caminho]))
            ),
            NovosCaminhos).

% Estende o caminho atual para os vizinhos com custo G
estendeG([Gc, No | Caminho], NovosCaminhos) :-
    findall([Gnovo, NovoNo, No | Caminho],
        (
            aresta(No, NovoNo),
            not(member(NovoNo, [No | Caminho])),
            Gnovo is Gc + 1
        ),
        NovosCaminhos
    ).

% Estende o caminho atual para os vizinhos com heurística H
estendeH([_,No |Caminho], NovosCaminhos, NoSujeira) :-
    findall([HNovo, NovoNo, No|Caminho],
        (
            sH(HN, No, NovoNo, NoSujeira),
            not(member(NovoNo,[No|Caminho])),
            HNovo is HN
        ),
        NovosCaminhos
    ).

% Estende o caminho atual para os vizinhos com avaliação F
estendeF([_, GC, _, No|Caminho], NovosCaminhos, NoSujeira) :-
    findall([FNovo, GNovo, HNovo, NovoNo, No|Caminho],
        (
            sF(HN, _, No, NovoNo, NoSujeira),
            not(member(NovoNo,[No|Caminho])),
            GNovo is GC + 1,
            HNovo is HN,
            FNovo is GNovo + HNovo
        ),
        NovosCaminhos
    ).

maior([_,_,F1|_], [_,_,F2|_]) :- F1 > F2.

maiorF([F1|_], [F2|_]) :- F1 > F2.

% Função para avaliação H
sH(H, V1, V2, NoSujeira) :-
    aresta(V1, V2),
    distancia_manhattan_de(V2, NoSujeira, H).

% Função para avaliação F
sF(H, F, V1, V2, NoSujeira) :-
    sH(H, V1, V2, NoSujeira),
    F is 1 + H.
