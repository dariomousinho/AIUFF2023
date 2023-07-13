:- use_module(library(lists)).

matriz([[a,b,c],[d,e,f],[g,h,i]]).

estado_inicial(a).
obstaculo(b).
sujeira(c).
sujeira(h).

percorre_distancia_sujeiras([]).
percorre_distancia_sujeiras([Sujeira|Resto]) :- percorre_distancia_sujeiras(Resto), matriz(Matriz), percorre_matriz(Matriz,Sujeira). 


preprocessar(1) :- findall(X,sujeira(X),Lista_Sujeiras), percorre_matriz_arestas([[a,b,c],[d,e,f],[g,h,i]]), %Cria todas as arestas possíveis, exceto para bloqueados
percorre_distancia_sujeiras(Lista_Sujeiras). %Cria relacoes das distancias manhattan para cada sujeira



% PENDENTE DE ORDERNAR A LISTA DE BUSCA!!!!!!
main_hill([],_).
main_hill([Sujeira_Atual|Sujeiras], Estado_Inicial):- 
    distancia_manhattan_de(Estado_Inicial, Sujeira_Atual, Distancia),
    hillClimb([[Distancia,Estado_Inicial]], Solucao, Custo, Sujeira_Atual),
    write(Solucao),
    main_hill(Sujeiras, Sujeira_Atual).





hillClimb([[_,No|Caminho]|_],Solucao,'-', NoSujeira) :-
	sujeira(No), No = NoSujeira,
	reverse([No|Caminho],Solucao).
    
hillClimb([Caminho|Caminhos], Solucao, Custo, NoSujeira) :-
	estendeH(Caminho, NovosCaminhos, NoSujeira),
	ordenaF(NovosCaminhos, CaminhosOrd),
	concatena(CaminhosOrd, Caminhos, CaminhosTotal),
	hillClimb(CaminhosTotal, Solucao, Custo, NoSujeira).


%Encontra i,j de um determinado elemento
encontrar_posicao(Elemento, Matriz, Linha, Coluna) :-
    nth0(Linha, Matriz, LinhaElementos),
    nth0(Coluna, LinhaElementos, Elemento).

%Encontra um determinado elemento dado i,j 
acessar_elemento(Matriz, X, Y, Elemento) :-
    nth0(X, Matriz, Linha),
    nth0(Y, Linha, Elemento),
    \+ obstaculo(Elemento).


%Calcula a distancia manhattan entre um elemento da matriz e uma sujeira
distancia_manhattan(X1, Y1, X2, Y2, Distancia) :-
    Distancia is abs(X2 - X1) + abs(Y2 - Y1).

distancia_manhattan_aux(Atual,Sujeira):- 
    matriz(Matriz),
    encontrar_posicao(Atual, Matriz, X_Atual,Y_Atual), 
    encontrar_posicao(Sujeira, Matriz, X_Sujeira,Y_Sujeira),
    distancia_manhattan(X_Atual,Y_Atual, X_Sujeira,Y_Sujeira, Distancia),
    
   encontrar_posicao(Atual, Matriz, Linha, Coluna),
    
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
define_vizinhos_aux(Atual, X_atual, Y_atual):-
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
    (   acessar_elemento(Matriz,  X_left, Y_atual, V2),
        assertz(aresta(Atual, V2))
    ;
        true
    ),
    (   acessar_elemento(Matriz,  X_atual, Y_up, V3),
        assertz(aresta(Atual, V3))
    ;
        true
    ),
    (  acessar_elemento(Matriz, X_atual, Y_down, V4),
        assertz(aresta(Atual, V4))
    ;
        true
    ).


define_vizinhos(Atual):- 
    obstaculo(Atual) ; 
    (matriz(Matriz),
    encontrar_posicao(Atual,Matriz,X_Atual,Y_Atual),
    define_vizinhos_aux(Atual, X_Atual, Y_Atual)).

percorre_matriz_arestas([]).
percorre_matriz_arestas([Linha|Resto]) :-
    percorre_linha_arestas(Linha),
    percorre_matriz_arestas(Resto).

percorre_linha_arestas([]).
percorre_linha_arestas([Atual|Resto]) :-
    define_vizinhos(Atual),
    percorre_linha_arestas(Resto).


%CONCATENAR
concatena([],L,L).
concatena([X|L1],L,[X|L2]):-
          concatena(L1,L,L2).


%ORDERNAR
ordenaF(Caminhos,CaminhosOrd):-
	quicksortF(Caminhos,CaminhosOrd).

particionarF(_,[],[],[]).
particionarF(X,[Y|Cauda],[Y|Menor],Maior):-
	maiorF(X,Y),!,
	particionarF(X,Cauda,Menor,Maior).
particionarF(X,[Y|Cauda],Menor,[Y|Maior]):-
	particionarF(X,Cauda,Menor,Maior).

quicksortF([],[]).
quicksortF([X|Cauda],ListaOrd):-
	particionarF(X,Cauda,Menor,Maior),
	quicksortF(Menor,MenorOrd),
	quicksortF(Maior,MaiorOrd),
	concatena(MenorOrd,[X|MaiorOrd],ListaOrd).



%MEMBRO
membro(X,[X|_]):-!.
membro(X,[_|C]):-
    membro(X,C).



estende(Caminho,NovosCaminhos).
estende([No|Caminho],NovosCaminhos):-
    findall([NovoNo,No|Caminho],
            (
                s(No,NovoNo),
                not(membro(NovoNo,[No|Caminho]))
            ),
            NovosCaminhos).

estendeG(Caminho,NovosCaminhos).


estendeH([_,No|Caminho],NovosCaminhos, NoSujeira) :-
	findall([HNovo,NovoNo,No|Caminho],
	( 
		sH(HN,No,NovoNo, NoSujeira),
		not(member(NovoNo,[No|Caminho])),
		HNovo is HN),
		NovosCaminhos
	).

% avaliação H
sH(H,V1,V2, NoSujeira):-
    aresta(V1,V2),
    distancia_manhattan_de(V2, NoSujeira, H).




sGB(63,a,b).
sGB(110,a,c).
sGB(53,a,e).
sGB(45,e,b).
sGB(65,b,d).
sGB(67,b,c).
sGB(45,c,d).
sGB(70,d,f).
sGB(52,e,f).
sGB(62,b,f).





%-------------

estendeF(Caminho,NovosCaminhos).

estendeF([_,GC,_,No|Caminho],NovosCaminhos):-
	findall([FNovo,GNovo,HNovo,NovoNo,No|Caminho],
	      (
          	  sF(GN,HN,_,No,NovoNo),
              not(member(NovoNo,[No|Caminho])),
              GNovo is GC + GN, 
          	  HNovo is HN, 
              FNovo is GNovo + HNovo
          ),
	      NovosCaminhos).


maior([_,_,F1|_],[_,_,F2|_]) :- F1 > F2.

maiorF([F1|_],[F2|_]):-F1 > F2.


