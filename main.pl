matriz([[a,b,c],[d,e,f],[g,h,i]]).

%Encontra i,j de um determinado elemento
encontrar_posicao(Elemento, Matriz, Linha, Coluna) :-
    nth0(Linha, Matriz, LinhaElementos),
    nth0(Coluna, LinhaElementos, Elemento).

%Encontra um determinado elemento dado i,j 
acessar_elemento(Matriz, X, Y, Elemento) :-
    nth0(X, Matriz, Linha),
    nth0(Y, Linha, Elemento).


%Calcula a distancia manhattan entre um elemento da matriz e uma sujeira
distancia_manhattan(X1, Y1, X2, Y2, Distancia) :-
    Distancia is abs(X2 - X1) + abs(Y2 - Y1).

distancia_manhattan_aux(Atual,Sujeira):- 
    matriz(Matriz),
    encontrar_posicao(Atual, Matriz, X_Atual,Y_Atual), 
    encontrar_posicao(Sujeira, Matriz, X_Sujeira,Y_Sujeira),
    distancia_manhattan(X_Atual,Y_Atual, X_Sujeira,Y_Sujeira, Distancia),
    
    write(Atual),nl,write(Sujeira),encontrar_posicao(Atual, Matriz, Linha, Coluna),
    
    assertz(distancia_manhattan_de(Atual,Sujeira,Distancia)).
    
percorre_matriz([],Sujeira).
percorre_matriz([Linha|Resto],Sujeira) :-
    percorre_linha(Linha,Sujeira),
    percorre_matriz(Resto,Sujeira).

percorre_linha([],Sujeira).
percorre_linha([Atual|Resto],Sujeira) :-
    distancia_manhattan_aux(Atual,Sujeira),
    percorre_linha(Resto,Sujeira).




%Transformar matriz em arestas possíveis


define_vizinhos_aux(Atual,X_Atual,Y_Atual):- 
    ()

define_vizinhos(Atual):- 
    matriz(Matriz),
    encontrar_posicao(Atual,Matriz,X_Atual,Y_Atual),
    define_vizinhos_aux(Atual,X_Atual,Y_Atual).
    
percorre_matriz_arestas([]).
percorre_matriz_arestas([Linha|Resto]) :-
    percorre_linha_arestas(Linha),
    percorre_matriz_arestas(Resto).

percorre_linha_arestas([]).
percorre_linha([Atual|Resto]) :-
    define_vizinhos(Atual),
    percorre_linha_arestas(Resto).
