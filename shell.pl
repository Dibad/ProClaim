% Prolog shell
:- initialization(start).        % Start shell on load

% Define dynamic functions
:- dynamic
  top_goal/1, fact/2, askable/3, rule/3.

% Define not operator
:- op(900, fy, not).


% Prolog user shell - main loop
start :-
  writeln('Prolog Shell para Sistemas Expertos'),
  repeat,
    nl,
    help,
    /* read(Command), */
    do(load),
    do(solve),
    Command = 'exit',
  !.

help :-
  writeln('Comandos: solve. load. o exit.').


% -- COMMANDS --
do('load') :- open_file, !.
do('solve') :- solve, !.
do('exit').
do(X) :-
  write(X),
  writeln(' no es un comando válido!').


% -- Open file for loading kb
open_file :-
  writeln('Escriba el nombre del archivo a cargar entre comillas (''*.kb''): '),
  /* read(File), */
  open_file('car.ckb').

open_file(File) :-
  exists_file(File),
  load_rules(File),
  write(File), writeln(' ha sido cargado!.').

open_file(File) :-
  write('El archivo '), write(File), writeln(' no existe.').


% -- Solve
solve :-
  current_predicate(top_goal/1),
  top_goal(A),
  goal(A),
  print_goal(A),
  !.

solve.


goal(A) :-
  findgoal(av(A, _), _).

print_goal(A) :-
  nl,
  fact(av(A, V), CF),
  CF >= 20,
  write(A), write(': '), write(V), write(' - '), write(CF), nl,
  fail. % Necessary to show all goals

% From problem find questions
findgoal(not Goal, CF) :-
  findgoal(Goal, CF).
  NCF is 100 - CF,
  !.

% Value already known
findgoal(av(A, V), CF) :-
  fact(av(A, V), CF),
  !.


% Ask user
findgoal(av(A, V), CF) :-
  not fact(av(A, _), _),
  askable(A, Menu, Prompt),
  query_user(A, Menu, Prompt),
  !,
  findgoal(av(A, V), CF).


findgoal(Goal, CurCF) :-
  fg(Goal, CurCF),
  !.


query_user(A, Menu, Prompt) :-
  nl,
  write('-> Question about: '), writeln(A),
  write('Options: '), writeln(Menu),
  atomic_list_concat(Prompt, ' ', PromptString),
  writeln(PromptString),
  read(V),
  writeln('CF?'),
  read(CF),
  asserta(fact(av(A, V), CF)).


fg(Goal, CurCF) :-
  rule(_, lhs(IfList), rhs(Goal, CF)),
  atom_number(CF, NumCF),
  prove(IfList, Tally),
  adjust(NumCF, Tally, NewCF),
  update(Goal, NewCF, CurCF),
  CurCF = 100,
  !.

fg(Goal, CF) :-
  fact(Goal, CF).


prove(IfList, Tally) :-
  prov(IfList, 100, Tally).

prov([], Tally, Tally).
prov([H | T], CurTal, Tally) :-
  findgoal(H, CF),
  min(CurTal, CF, Tal),
  Tal >= 20,
  prov(T, Tal, Tally).

min(X, Y, X) :-
  X =< Y,
  !.

min(X, Y, Y) :-
  Y =< X.

adjust(CF1, CF2, CF) :-
  X is CF1 * CF2 / 100,
  int_round(X, CF).

int_round(X, I) :-
  X >= 0,
  I is integer(X + 0.5).

int_round(X, I) :-
  X < 0,
  I is integer(X - 0.5).

update(Goal, NewCF, CF) :-
  fact(Goal, OldCF),
  combine(NewCF, OldCF, CF),
  retract(fact(Goal, OldCF)),
  asserta(fact(Goal, CF)),
  !.

update(Goal, CF, CF) :-
  asserta(fact(Goal, CF)).

combine(CF1, CF2, CF) :-
  CF1 >= 0,
  CF2 >= 0,
  X is CF1 + CF2 * (100 - CF1) / 100,
  int_round(X, CF).

combine(CF1, CF2, CF) :-
  CF1 < 0,
  CF2 < 0,
  X is - ( -CF1 -CF2 * (100 + CF1) / 100 ),
  int_round(X, CF).

combine(CF1, CF2, CF) :-
  (CF1 < 0 ; CF2 < 0),
  (CF1 > 0 ; CF2 > 0),
  abs_minimum(CF1, CF2, MCF),
  X is 100 * (CF1 + CF2) / (100 - MCF),
  int_round(X, CF).

abs_minimum(A,B,X) :-
	absolute(A, AA),
	absolute(B, BB),
	minimum(AA,BB,X).

absolute(X, X) :- X >= 0.
absolute(X, Y) :- X < 0, Y is -X.


% -- Load and Parse Expert System Rules
load_rules(File) :-
  % clean_db,
  see(File),
  lod_ruls,
  writeln('Todas las reglas se han leído correctamente.'),
  seen.
  %!.

lod_ruls :-
  repeat,
    read_sentence(List),
    process(List),
    List = [''],
  !.

process(['']).
process(List) :-
  translate(Rule, List, []),
  assertz(Rule), % Add rule to dynamic database
  writeln(Rule),
  !.

process(List) :-
  writeln('Error de traducción en: '),
  writeln(List),
  fail.

% Split and tokenize a sentence
read_sentence(Rule_List) :-
  read_string(current_input, ".", "\n\r\t ", _, String),
  split_string(String, "\n\t, ", "", LStrings), % Remove special characters
  atomic_list_concat(LStrings,' ', Atom),
  atomic_list_concat(Rule_List,' ', Atom).


% -- DFG for parsing text

% KB statements
translate(top_goal(X)) --> ['goal', X].
translate(top_goal(X)) --> ['goal', 'is', X].
translate(askable(A, M, P)) -->
  ['ask', A], menu(M), prompt(P).
translate(rule(N, lhs(IF), rhs(THEN, CF))) --> id(N), if(IF), then(THEN, CF).


% Menu structure
menu(M) --> ['menu'], itemlist(M).
prompt(P) --> ['prompt'], itemlist(P).

itemlist([Item]) --> [Item].
itemlist([Item | T]) --> [Item], itemlist(T).


% Rule structure
id(N) --> ['rule', N].

if(IF) --> ['if'], iflist(IF).

iflist([IF]) --> statement(IF), ['then'].
iflist([H | T]) --> statement(H), (['and'] ; [', ']), iflist(T).

then(THEN, CF) --> statement(THEN), [cf], [CF].
then(THEN, 100) --> statement(THEN).

statement(av(A, 'no')) --> ['not', A].
statement(av(A, 'no')) --> ['not'], (['a'] ; ['an']), [A].
statement(not av(A, V)) --> ['not'], [A], (['is'] ; ['are']), [V]. % Not defined
statement(av(A, V)) --> [A], (['is'] ; ['are']), [V].
statement(av(A, 'yes')) --> [A].

