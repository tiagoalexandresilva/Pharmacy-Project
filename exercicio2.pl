%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - LEI/3

% Exercicio 2

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: definicoes iniciais

:- op( 900,xfy,'::' ).
:- dynamic '-'/1.
:- dynamic medicamento/1.
:- dynamic armazenado/3.
:- dynamic principioativo/2.
:- dynamic apresfarma/2.
:- dynamic datadevalidade/2.
:- dynamic datadeintroducao/2.
:- dynamic inditerap/2.
:- dynamic apliclinica/2.
:- dynamic precoregime/2.
:- dynamic precorecomendado/2.
:- dynamic precopublico/2.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Base de Conhecimento

medicamento(ben_u_ron).
datadevalidade(ben_u_ron,date(5,12,2013)).
armazenado(ben_u_ron,1,1). % ben_u_ron está na prateleira 1, armario 1
principioativo(ben_u_ron,paracetamol).
apresfarma(ben_u_ron,comprimido).
apresfarma(ben_u_ron,xarope).
apresfarma(ben_u_ron,supositorio).
inditerap(ben_u_ron,analgesico).
inditerap(ben_u_ron,antipiretico).
apliclinica(ben_u_ron,dor).
apliclinica(ben_u_ron,gripe).
apliclinica(ben_u_ron,febre).
precopublico(ben_u_ron,14).
precorecomendado(ben_u_ron,17).
precoregime(ben_u_ron,12).
datadeintroducao(ben_u_ron,date(1-1-1999)).


medicamento(tylenol).
datadevalidade(tylenol,date(29,8,2014)).
armazenado(tylenol,1,2).
principioativo(tylenol,paracetamol).
apresfarma(tylenol,xarope).
apresfarma(tylenol,comprimido).
apresfarma(tylenol,capsula).
inditerap(tylenol,analgesico).
inditerap(tylenol,antipiretico).
apliclinica(tylenol,febre).
apliclinica(tylenol,dor).
precopublico(tylenol,21).
precorecomendado(tylenol,25).
precoregime(tylenol,19).
datadeintroducao(tylenol,date(1-1-1999)).

medicamento(aspirina).
datadevalidade(aspirina,date(2,10,2013)).
armazenado(aspirina,2,1).
principioativo(aspirina,acido_acetilsalicilico).
apresfarma(aspirina,comprimido).
inditerap(aspirina,analgesico).
inditerap(aspirina,antipiretico).
inditerap(aspirina,antiagregante_plaquetario).
apliclinica(aspirina,coagulos).
apliclinica(aspirina,enxaqueca).
apliclinica(aspirina,kawasaki).
precopublico(aspirina,18).
precorecomendado(aspirina,25).
precoregime(aspirina,15).
datadeintroducao(aspirina,date(1-1-1999)).

medicamento(brufen).
datadevalidade(brufen,date(16,5,2014)).
armazenado(brufen,5,3).
principioativo(brufen,ibuprofeno).
apresfarma(aspirina,comprimido).
inditerap(brufen,analgesico).
inditerap(brufen,antipiretico).
inditerap(brufen,anti_inflamatorio).
apliclinica(brufen,reumatologia).
apliclinica(brufen,dor).
apliclinica(brufen,febre).
precopublico(brufen,8).
precorecomendado(brufen,12).
precoregime(brufen,7).
datadeintroducao(brufen,date(1-1-1999)).

medicamento(trifene).
datadevalidade(trifene,date(8,2,2014)).
armazenado(trifene,5,2).
principioativo(trifene,ibuprofeno).
apresfarma(trifene,comprimido).
inditerap(brufen,analgesico).
inditerap(brufen,antipiretico).
inditerap(trifene,anti_inflamatorio).
apliclinica(trifene,dor).
apliclinica(trifene,febre).
precopublico(trifene,11).
precorecomendado(trifene,desconhecido).
precoregime(trifene,15).
datadeintroducao(trifene,date(1-1-1999)).
excepcao(precopublico(trifene,12)).
excepcao(precopublico(trifene,15)).

medicamento(xanax).
datadevalidade(xanax,date(1,1,2015)).
armazenado(xanax,4,1).
principioativo(xanax,alprazolam).
apresfarma(xanax,comprimido).
inditerap(xanax,desconhecido).
apliclinica(xanax,ansiedade).
apliclinica(xanax,panico).
precorecomendado(xanax,32).
precoregime(xanax,25).
datadeintroducao(xanax,date(1-1-1999)).
precopublico(xanax,interdito).


medicamento(reumon_locao).
datadevalidade(reumon_locao,date(3,5,2015)).
armazenado(reumon_locao,8,2).
principioativo(reumon_locao,etofenamato).
apresfarma(reumon_locao,pomada).
inditerap(reumon_locao,anti_inflamatorio).
apliclinica(reumon_locao,artropatias).
apliclinica(reumon_locao,mialgias).
apliclinica(reumon_locao,bursites).
apliclinica(reumon_locao,fibrosites).
apliclinica(reumon_locao,nevralgias).
apliclinica(reumon_locao,contusoes).
apliclinica(reumon_locao,nevralgias).
apliclinica(reumon_locao,entorses).
apliclinica(reumon_locao,distensoes).
precopublico(reumon_locao,65).
precorecomendado(reumon_locao,61).
precoregime(reumon_locao,52).
datadeintroducao(reumon_locao,interdito).

nulo(interdito).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariantes para não permitir conhecimento imperfeito interdito
	+medicamento( M) ::
	(
		M 	\= desconhecido,
		nao(nulo(M))
	).

	+principioativo( M,PA) ::
	(
		M 	\= desconhecido,
		nao(nulo(M)),
		nao(nulo(PA))
	).
	+apresfarma( M,AF) ::
	(
		M 	\= desconhecido,
		nao(nulo(M)),
		nao(nulo(AF))
	).
	+indicterap( M,IT) ::
	(
		M 	\= desconhecido,
		nao(nulo(M)),
		nao(nulo(IT))
	).
	+apliclinica( M,AC) ::
	(
		M 	\= desconhecido,
		nao(nulo(M)),
		nao(nulo(AC))
	).
	+precopublico( M,P) ::
	(
		M 	\= desconhecido,
		nao(nulo(P))
	).
	+precoregime( M,P) ::
	(
		M 	\= desconhecido,
		nao(nulo(M)),
		nao(nulo(P))
	).
	+precorecomendado( M,P) ::
	(
		M 	\= desconhecido,
		nao(nulo(M)),
		nao(nulo(P))
	).
	+datadeintroducao( M,D) ::
	(
		M 	\= desconhecido,
		nao(nulo(M)),
		nao(nulo(D))
	).
	
% Predicado medicamento : Medicamento ->{V,F}

% Invariante para não ter repetidos
+medicamento(M)::(findall(M,(medicamento(M)),L),comprimento(L,N),N==1).
+precoregime(M,P)::(findall(M,(precoregime(M,P)),L),comprimento(L,N),N==1).
+precorecomendado(M,P)::(findall(M,(precorecomendado(M,P)),L),comprimento(L,N),N==1).
+precopublico(M,P)::(findall(M,(precopublico(M,P)),L),comprimento(L,N),N==1).

% Excepções para o desconhecido
	excepcao( principioativo( M,PA) ) :- principioativo(M,desconhecido).
	excepcao( apresfarma(M,AF) ) :- apresfarma(M,desconhecido).
	excepcao( inditerap(M,IT) ) :- inditerap(M,desconhecido).
	excepcao( apliclinica(M,AC) ) :- apliclinica(M,desconhecido).
	excepcao( precoregime( M,P ) ) :- precoregime( M,desconhecido).
	excepcao( datadeintroducao(M,D) ):- datadeintroducao(M,desconhecido).
	excepcao( precorecomendado( M,P ) ) :- precorecomendado( M,desconhecido).
	excepcao( precopublico( M,P ) ) :- precopublico( M,desconhecido).



	
% Excepçoes para o interdito
	% armazenado
	excepcao( armazenado( M,P,A ) ) :- armazenado( M,I,_ ),nulo(I).
	excepcao( armazenado( M,P,A ) ) :- armazenado( M,_,I ),nulo(I).
	% precoespecial
	excepcao( precoregime( M,P ) ) :- precoregime( M,I),nulo(I).
	% precopublico
	excepcao( precopublico( M,P ) ) :- precopublico( M,I),nulo(I).
	% precorecomendado
	excepcao( precorecomendado( M,P ) ) :- precorecomendado( M,I),nulo(I).
	% datadeintroducao
	excepcao( datadeintroducao( M,P ) ) :- datadeintroducao( M,I),nulo(I).
	
% Invariante para não permitir evoluir conhecimento interdito
	+armazenado(M,P,A)::findall(( M,P,A ),(armazenado( M,I,B ),(nulo(I);nulo(B))),[]).
	+precoregime(M,P)::findall(( M,P ),(precoregime( M,I ),(nulo(I))),[]).
	+precorecomenado(M,P)::findall(( M,P ),(precorecomenado( M,I ),(nulo(I))),[]).
	+precopublico(M,P)::findall(( M,P ),(precopublico( M,I ),(nulo(I))),[]).

	
% Invariante para não ter mais do que 10 itens por prateleira e para limitar os armarios a 8 e as prateleiras dos armarios a 4
+armazenado(M,P,A):: (armazenadoPrat(A,P,L),comprimento(L,N),N=<10,P<5,A<9).

% Provar factos falsos
-datadevalidade(M,D) :- nao( dataPvalidade(M,D) ) ,
nao( excepcao( datadevalidade(M,D) ) ) .

-datadeintroducao(M,D) :- nao(datadeintroducao(M,D) ) ,
nao( excepcao(datadeintroducao(M,D) ) ) .

-armazenado(M,P,A) :- nao( armazenado(M,P,A) ) ,
nao( excepcao( armazenado(M,P,A) ) ) .

-precoregime(M,P) :- nao( precoregime(M,P) ) ,
nao( excepcao( precoregime(M,P) ) ) .

-precorecomendado(M,P) :- nao( precorecomendado(M,P) ) ,
nao( excepcao( precorecomendado(M,P) ) ) .

-precopublico(M,P) :- nao( precopublico(M,P) ) ,
nao( excepcao( precopublico(M,P) ) ) .

-apresfarma(M,AF) :- nao( apresfarma(M,AF) ) ,
nao( excepcao( apresfarma(M,AF) ) ) .

-indicterap(M,IT) :- nao( indicterap(M,IT) ) ,
nao( excepcao( indicterap(M,IT) ) ) .

-principioativo(M,PA) :- nao( principioativo(M,PA) ) ,
nao( excepcao( principioativo(M,PA) ) ) .

-apliclinica(M,AC) :- nao( apliclinica(M,AC) ) ,
nao( excepcao( apliclinica(M,AC) ) ) .


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Predicado datadevalidade : Medicamento,Data -> {V,F}


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Predicado precopublico : Medicamento,Preço ->{V,F}


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Predicado precorecomendado : Medicamento,Preço ->{V,F}


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Predicado precoregime : Medicamento,Preço ->{V,F}


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Predicado dataintmercado : Medicamento,Data -> {V,F}


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Predicado armazenado : Medicamento,Prateleira,Armário -> {V,F}
+armazenado(M,P,A):: (armazenadoPrat(A,P,L),comprimento(L,N),N=<10).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Predicado principioativo : Medicamento,Principioativo -> {V,F,D}


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Predicado apresfarma : Medicamento,ApresFarmaceutica -> {V,F,D}


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Predicado inditerap : Medicamento,IndicaçaoTerapeutica -> {V,F,D}


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Predicado apliclinica : Medicamento,AplicacaoClinica -> {V,F,D}


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Predicado armazenadoArmario : Armario,ListaMedicamentos -> {V,F} listar os medicamentos de um armario
armazenadoArmario(A,L) :- findall(M,armazenado(M,P,A),L).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Predicado armazenadoPrat : Armario,Prateleira,ListaMedicamentos -> {V,F} listar os medicamentos de uma prateleira num armario
armazenadoPrat(A,P,L) :- findall(M,armazenado(M,P,A),L).

	
% Extensao do meta-predicado demo: Questao,Resposta -> {V,F}

demo( Q,verdadeiro ) :-
    Q.
demo( Q, falso ) :-
    -Q.
demo( Q,desconhecido ) :-
    nao( Q ),
    nao( -Q ).
	
% Extensao do meta-predicado ldemo: [Questao],Resposta -> {V,F,D}
ldemo([],verdadeiro).
ldemo([Q|LQ],verdadeiro):-demo(Q,verdadeiro),demo(LQ,verdadeiro).
ldemo([Q|LQ],falso):-demo(Q,falso),demo(LQ,_).
ldemo([Q|LQ],falso):-demo(Q,_),demo(LQ,falso).
ldemo([Q|LQ],desconhecido):-demo(Q,desconhecido),demo(LQ,verdadeiro).
ldemo([Q|LQ],desconhecido):-demo(Q,verdadeiro),demo(LQ,desconhecido).
ldemo([Q|LQ],desconhecido):-demo(Q,desconhecido),demo(LQ,desconhecido).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado nao: Questao -> {V,F}

nao( Q ) :-
    Q, !, fail.
nao( Q ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -				  
% Extensão do predicado evolucao: Termo -> {V,F}
evolucao( T ) :-
    findall( I,+T::I,L ),
	insercao( T ),
    teste( L ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado insercao: Termo -> {V,F}
insercao( T ) :-
    assert( T ).
insercao( T ) :-
    retract( T ),!,fail.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado teste: Lista -> {V,F}
teste( [] ).
teste( [R|LR] ) :-
    R,
    teste( LR ).
	
%--------------------------------- - - - - - - - - - -  -  -  -  -   -				  
% Extensão do predicado regressao: Termo -> {V,F}
regressao(T) :-
	remocao(T).
	
%--------------------------------- - - - - - - - - - -  -  -  -  -   -				  
% Extensão do predicado remocao: Termo -> {V,F}
remocao(T) :-
		retract(T).
		
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado comprimento: Lista,Resultado -> {V,F}
comprimento([ ],0).
comprimento([H|T],R) :-
	comprimento(T,X),
		R is X+1.