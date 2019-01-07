%  +----------------------------------+
%  |                                  |
%  |            ProCla(i)m            |
%  |                                  |
%  +----------------------------------+
% Prolog Shell for Expert Sytems based on Clam, from the book "Building Expert
% Systems with Prolog", with extended features.


% ============  Prolog Defs  ==============
:- initialization(start). % Initialize shell on start

:- dynamic
  fact/2, askable/3, rule/3, ruletrace/0.

:- op(900, fy, not). % Define not operator instead of using \+

ruletrace.


% ============  Main Loop  ================
start :-
  writeln('\t-- ClamPro --'), writeln('Prolog Shell for Expert Systems.'),
  repeat,
    nl,
    writeln('Commands: solve. load. reset. or exit.'),
    read_sentence(Command),
    do(Command),
    Command = ['exit'],
  !.



% ============  Commands  =================
do(['solve']) :- solve, !.
do(['load']) :- load_file, !.
do(['trace' | Val]) :- trace(Val), !.
do(['reset']) :- writeln('Database clear!'), clear_db, !.
do(['exit']).
do(Command) :-
  write_list(Command),
  writeln(' is not a valid command!').



% ============  Clear  ====================

% Delete all knowledge from the dynamic database
clear_db :-
  retractall(top_goal(_)),
  retractall(rule(_, _, _)),
  retractall(askable(_, _, _)),
  clear_facts.

% Delete only facts created from previous consults with solve.
clear_facts :-
  retractall(fact(_, _)).



% ============  Trace  ====================

trace(['on']) :-
  asserta(ruletrace),
  writeln('Trace rule is ON.'),
  !.

trace(['off']) :-
  retract(ruletrace),
  writeln('Trace rule is OFF.'),
  !.

trace([X]) :-
  write(X), writeln(' is not a valid argument! Must be on/off').



% ============  Load File  ================

% A. Read file from user input
load_file :-
  writeln('Type the name of the file between single quotes (''*.kb''): '),
  read(File),
  load_file(File).

% B. Check if file exists and load rules
load_file(File) :-
  atomic(File),
  exists_file(File),
  load_rules(File),
  write(File), writeln(' rules have been loaded!.').

% C. Fail if couldn't load file
load_file(File) :-
  write('Error! Couldn''t load '), writeln(File).



% ============  Load Rules  ===============

% A. Clean previous rules, set file as stream input and start iterating.
load_rules(File) :-
  clear_db,
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
  phrase(translate(Rule), List), % Call DCG
  assertz(Rule), % Add the Rule to the dynamic database
  writeln(Rule),
  !.

process(List) :-
  writeln('Translation error in:'),
  writeln(List).


% Split a sentence, using spaces as separator and skipping escape characters
% NOTE: MUST be a list of ATOMS ['a', 'b', ...], not STRINGS ["a", "b", ...]
read_sentence(ListAtoms) :-
  read_string(current_input, ".", "", _, String),
  split_string(String, "\n\t, ", "()\s\n\t", ListStrings), % Remove special characters
  atomic_list_concat(ListStrings,' ', Atom), % ["a", "b"] --> 'a b'
  atomic_list_concat(ListAtoms,' ', Atom). % 'a b' --> ['a', 'b']


% Functions to easily write a list as a string
write_list(L) :-
  atomic_list_concat(L, ' ', String),
  write(String).

write_list_ln(L) :-
  write_list(L),
  nl.



% ============  Solve  ====================

% A. Start the inference and find a goal with the A from top_goal
solve :-
  clear_facts,
  current_predicate(top_goal/1),
  top_goal(A),
  findgoal(av(A, _), _),
  print_goals(A).

% B. Load knowledge database first (to define top_goal predicate)
solve :-
  not current_predicate(top_goal/1),
  writeln('First, load a knowledge database using load.'),
  !.

% C. No result found
solve :-
  nl,
  writeln('Couldn''t find any goal :(').



% ============  Print Goal  ===============

% Print all the goals found after searching through the dynamic database
print_goals(A) :-
  nl,
  writeln('Sucess! Found: '),
  forall(fact(av(A, V), CF),
    (write('\t'), write(A), write(': '), write(V), write(' cf '), writeln(CF))).



% ============  Find Goal  ================

% Fix for negative values to work
findgoal(not av(A, 'yes'), CF) :-
  findgoal(av(A, 'yes'), CF),
  !.

findgoal(not Goal, NCF) :-
  findgoal(Goal, CF),
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


% Search if Goal appears as a rhs of any rule
findgoal(Goal, CF) :-
  fg(Goal, CF),
  !.

% Search rules with rhs of Goal. Prove its lhs, and update new CF of facts
fg(Goal, CurCF) :-
  rule(N, lhs(IfList), rhs(Goal, CF)),
  atom_number(CF, NumCF),
  bugdisp(['call rule', N]),
  /* trace, */
  prove(N, IfList, LhsCF),
  bugdisp(['exit rule', N]),
  adjust(NumCF, LhsCF, NewCF),
  update(Goal, NewCF, CurCF),
  CurCF = 100,
  !.

fg(Goal, CF) :-
  fact(Goal, CF).



% ============  Bugdisp  ==================

bugdisp(L) :-
  ruletrace,
  nl,
  tab(1), write_list_ln(L),
  !.

bugdisp(_).



% ============  Query user  ===============

% Show a menu to ask the value of the attribute to the user
query_user(A, Menu, Prompt) :-
  repeat,
    nl,
    write('-> Question about: '), writeln(A),
    write('Options: '), writeln(Menu),
    write_list_ln(Prompt),
    get_user_answer(V, CF, Menu, Valid),
    Valid = true,
  !,
  save_fact(av(A, V), CF).


% A. Get user input as a list of words. Parse it and check if its correct
get_user_answer(V, CF, Menu, _) :-
  read_sentence(Answer),
  parse_answer(Answer, V, CF),
  check_answer(V, CF, Menu).

% B. If its not correct, repeat loop by assigning false to Valid
get_user_answer(_, _, _, Valid) :-
  writeln('No es una respuesta válida!.'),
  Valid = false.


% Input can be: value. (implicit cf of 100), or value cf.
parse_answer([V], V, 100) :- !.
parse_answer([V, CF], V, NumCF) :-
  atom_number(CF, NumCF),
  !.


% Chech that answer is a option of the menu, and that CF is between the range
check_answer(V, CF, Menu) :-
  member(V, Menu),
  between(0, 100, CF).


% A. Save negative statements as positive ones with complementary CF
save_fact(av(A, 'no'), CF) :-
  NCF is 100 - CF,
  asserta(fact(av(A, 'yes'), NCF)).

% B. Save positive statements as they are
save_fact(av(A, V), CF) :-
  asserta(fact(av(A, V), CF)).



% ============  Prove     =================

prove(_, IfList, LhsCF) :-
  prov(IfList, 100, LhsCF),
  !.

prove(N, _, _) :-
  bugdisp(['fail rule', N]),
  fail.


prov([], LhsCF, LhsCF).
prov([H | T], CurCF, LhsCF) :-
  test_if(H, CurCF, NewCF),
  prov(T, NewCF, LhsCF).


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



% ============  Adjust  ===================

adjust(CF1, CF2, CF) :-
  CF is round(CF1 * CF2 / 100).



% ============  Update  ===================

% A. Remove old fact and add new one with updated CF
update(Goal, NewCF, CF) :-
  fact(Goal, OldCF),
  combine(NewCF, OldCF, CF),
  retract(fact(Goal, OldCF)),
  asserta(fact(Goal, CF)),
  !.

% B. Add fact for first time.
update(Goal, CF, CF) :-
  asserta(fact(Goal, CF)).


% Equation used to calculate the new CF
combine(CF1, CF2, CF) :-
  CF is round(CF1 + CF2 * (100 - CF1) / 100).



% ============  DCG  ======================

% -- Statements that the DCG can generate
translate(top_goal(X)) --> ['goal', X].
translate(top_goal(X)) --> ['goal', 'is', X].
translate(askable(A, M, P)) --> ['ask', A], menu(M), prompt(P).
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
then(THEN, '100') --> statement(THEN).

statement(not av(A, 'yes')) --> ['not', A].
statement(not av(A, 'yes')) --> ['not'], (['a'] ; ['an']), [A].
statement(av(A, V)) --> [A], (['is'] ; ['are']), [V].
statement(av(A, 'yes')) --> [A].
