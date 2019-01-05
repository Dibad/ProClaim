%  +----------------------------------+
%  |                                  |
%  |             ClamPro              |
%  |                                  |
%  +----------------------------------+
% Prolog Shell for Expert Sytems based on Clam, from the book "Building Expert
% Systems with Prolog" with extended features.


% ============  Prolog Defs  ==============
:- initialization(start). % Initialize shell on start

:- dynamic
  top_goal/1, fact/2, askable/3, rule/3.

:- op(900, fy, not). % Define not operator instead of using \+



% ============  Main Loop  ================
start :-
  writeln('\t-- ClamPro --'), writeln('Prolog Shell for Expert Systems.'),
  repeat,
    nl,
    writeln('Commands: solve. load. or exit.'),
    read(Command),
    do(Command),
    Command = 'exit',
  !.



% ============  Commands  =================
do('solve') :- solve, !.
do('load') :- load_file, !.
do('consult') :- consult, !.
do('exit').
do(X) :-
  write(X),
  writeln(' is not a valid command!').



% ============  Load File  ================

% A. Read file from user input
load_file :-
  writeln('Type the name of the file between single quotes (''*.kb''): '),
  read(File),
  load_file('car.ckb').

% B. Check if file exists and load rules
load_file(File) :-
  exists_file(File),
  load_rules(File),
  write(File), writeln(' rules have been loaded!.').

% C. Fail if couldn't load file
load_file(File) :-
  write('Error! Couldn''t load '), writeln(File).


% -- Solve
solve :-
  current_predicate(top_goal/1),
  top_goal(A),
  goal(A),
  print_goal(A),
  !.

solve.



% ============  Load Rules  ===============

% A. Clean previous rules, set file as stream input and start iterating.
load_rules(File) :-
  % clean_db,
  see(File),
  load_rules,
  writeln('Done loading rules!.'),
  seen.

% B. Iterate through all rules. Doesn't stop if a rule can't be translated
load_rules :-
  repeat,
    read_sentence(List),
    process(List),
    List = [''],
  !.


% Transform rule from DCG to internal format.
process(['']).
process(List) :-
  translate(Rule, List, []), % --> Call to DCG
  assertz(Rule), % Add the Rule to the dynamic database
  writeln(Rule),
  !.

process(List) :-
  writeln('Translation error in:'),
  writeln(List).


% Split a sentence, using spaces as separator and skipping escape characters
% NOTE: MUST be a list of ATOMS ['a', 'b', ...], not STRINGS ["a", "b", ...]
read_sentence(ListAtoms) :-
  read_string(current_input, ",.", "\n\r\t ", _, String),
  split_string(String, "\n\t, ", "", ListStrings), % Remove special characters
  atomic_list_concat(ListStrings,' ', Atom), % ["a", "b"] --> 'a b'
  atomic_list_concat(ListAtoms,' ', Atom). % 'a b' --> ['a', 'b']



goal(A) :-
  findgoal(av(A, _), _).

print_goal(A) :-
  nl,
  fact(av(A, V), CF),
  CF >= 20,
  write(A), write(': '), write(V), write(' - '), write(CF), nl,
  fail. % Necessary to show all goals

findgoal(not Goal, CF) :-
  findgoal(Goal, CF),
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


% Search sub-questions
findgoal(Goal, CurCF) :-
  fg(Goal, CurCF),
  !.


query_user(A, Menu, Prompt) :-
  repeat,
    nl,
    write('-> Question about: '), writeln(A),
    write('Options: '), writeln(Menu),
    atomic_list_concat(Prompt, ' ', PromptString),
    writeln(PromptString),
    /* trace, */
    get_user_answer(V, CF, Menu, Valid),
    Valid = true,
  !,
  save_fact(av(A, V), CF).


get_user_answer(V, CF, Menu, _) :-
  read_sentence(Answer),
  parse_answer(Answer, V, CF),
  check_answer(V, CF, Menu).

get_user_answer(_, _, _, Valid) :-
  writeln('No es una respuesta válida!.'),
  Valid = false.

parse_answer([V], V, 100) :- !.
parse_answer([V, CF], V, NumCF) :-
  atom_number(CF, NumCF),
  !.

check_answer(V, CF, Menu) :-
  member(V, Menu),
  between(0, 100, CF).


save_fact(av(A, 'no'), CF) :-
  NCF is 100 - CF,
  asserta(fact(av(A, 'yes'), NCF)).

save_fact(av(A, V), CF) :-
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


prove(IfList, RuleCF) :-
  prov(IfList, 100, RuleCF).

prov([], RuleCF, RuleCF).

prov([H | T], CurCF, RuleCF) :-
  test_if(H, CurCF, NewCF),
  prov(T, NewCF, RuleCF).

test_if(av(A, V), CurCF, NewCF) :-
  findgoal(av(A, V), CF),
  min_cf(CurCF, CF, NewCF).

test_if(not av(A, V), CurCF, NewCF) :-
  findgoal(not av(A, V), CF),
  ComplCF is 100 - CF,
  min_cf(CurCF, ComplCF, NewCF).

min_cf(A, B, Out) :-
  Out is min(A, B),
  Out >= 20.


adjust(CF1, CF2, CF) :-
  CF is round(CF1 * CF2 / 100).


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
  CF is round(CF1 + CF2 * (100 - CF1) / 100).

combine(CF1, CF2, CF) :-
  CF1 < 0,
  CF2 < 0,
  X is - ( -CF1 -CF2 * (100 + CF1) / 100 ),
  CF is round(X).

combine(CF1, CF2, CF) :-
  (CF1 < 0 ; CF2 < 0),
  (CF1 > 0 ; CF2 > 0),
  abs_minimum(CF1, CF2, MCF),
  X is 100 * (CF1 + CF2) / (100 - MCF),
  CF is round(X).

abs_minimum(A,B,X) :-
	absolute(A, AA),
	absolute(B, BB),
	minimum(AA,BB,X).

absolute(X, X) :- X >= 0.
absolute(X, Y) :- X < 0, Y is -X.



% -- DCG for parsing text

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

statement(not av(A, 'yes')) --> ['not', A].
statement(not av(A, 'yes')) --> ['not'], (['a'] ; ['an']), [A].
statement(av(A, V)) --> [A], (['is'] ; ['are']), [V].
statement(av(A, 'yes')) --> [A].

