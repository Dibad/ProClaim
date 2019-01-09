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
  fact/3, askable/3, rule/3, ruletrace/0, output/3.

:- op(900, fy, not). % Define not operator instead of using \+

ruletrace.


% ============  Main Loop  ================
start :-
  writeln('\t-- ClamPro --'), writeln('Prolog Shell for Expert Systems.'),
  repeat,
    nl,
    writeln('Commands: solve. load. trace. how. reset. or exit.'),
    read_sentence(Command),
    do(Command),
    Command = ['exit'],
  !.



% ============  Commands  =================
do(['solve']) :- solve, !.
do(['load']) :- load_file, !.
do(['trace' | Val]) :- trace(Val), !.
do(['how']) :- how, !.
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
  retractall(fact(_, _, _)).



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
  trace,
  findgoal(av(A, _), _, [goal(A)]),
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
  forall(fact(av(A, V), CF, _),
    (write('\t- '), write(A), write(': '), write(V), write(' cf '), writeln(CF),
    print_solution(A, V))).


print_solution(A, V) :-
  output(A, V, SolutionList),
  write('\t- Solution: '), write_list_ln(SolutionList), nl,
  !.

print_solution(_, _).


% ============  Find Goal  ================

% Fix for negative values to work
findgoal(not Goal, NCF, Hist) :-
  findgoal(Goal, CF, Hist),
  NCF is 100 - CF,
  !.

% Value already known
findgoal(av(A, V), CF, _) :-
  fact(av(A, V), CF, _),
  !.

% Ask user
findgoal(av(A, V), CF, Hist) :-
  not fact(av(A, _), _, _),
  askable(A, Menu, Prompt),
  query_user(A, Menu, Prompt, Hist),
  !,
  findgoal(av(A, V), CF, Hist).


% Search if Goal appears as a rhs of any rule
findgoal(Goal, CF, Hist) :-
  fg(Goal, CF, Hist),
  !.


% Search rules with rhs of Goal. Prove its lhs, and update new CF of facts
fg(Goal, CurCF, Hist) :-
  rule(N, lhs(IfList), rhs(Goal, CF)),
  atom_number(CF, NumCF),
  bugdisp(['call rule', N]),
  prove(N, IfList, LhsCF, Hist),
  bugdisp(['exit rule', N]),
  adjust(NumCF, LhsCF, NewCF),
  update(Goal, NewCF, CurCF, N),
  CurCF = 100,
  !.

fg(Goal, CF, Hist) :-
  fact(Goal, CF, _).



% ============  Bugdisp  ==================

bugdisp(Rule) :-
  ruletrace,
  nl,
  tab(1), write_list_ln(Rule),
  !.

bugdisp(_).



% ============  Query user  ===============

% Show a menu to ask the value of the attribute to the user
query_user(A, Menu, Prompt, Hist) :-
  repeat,
    nl,
    write('-> Question about: '), writeln(A),
    write('Options: '), writeln(Menu),
    write_list_ln(Prompt),
    get_user_answer(V, CF, Menu, Valid, Hist),
    Valid = true,
  !,
  save_fact(av(A, V), CF).


% A. Get user input as a list of words. Parse it and check if its correct
get_user_answer(V, CF, Menu, _, Hist) :-
  read_sentence(Answer),
  parse_answer(Answer, V, CF, Hist),
  check_answer(V, CF, Menu).

% B. If its not correct, repeat loop by assigning false to Valid
get_user_answer(_, _, _, Valid, _) :-
  writeln('No es una respuesta válida!.'),
  Valid = false.


parse_answer(['why'], _, _, Hist) :-
  write_hist(Hist),
  fail,
  !.

% Input can be: value. (implicit cf of 100), or value cf.
parse_answer([V], V, 100, _) :- !.
parse_answer([V, CF], V, NumCF, _) :-
  atom_number(CF, NumCF),
  !.


write_hist([]) :-
  nl.

write_hist([goal(X) | T]) :-
  write_list([goal, X]),
  !,
  write_hist(T).

write_hist([N | T]) :-
  write_rule(N),
  !,
  write_hist(T).


% Chech that answer is a option of the menu, and that CF is between the range
check_answer(V, CF, Menu) :-
  member(V, Menu),
  between(0, 100, CF).


% A. Save negative statements as positive ones with complementary CF
save_fact(av(A, 'no'), CF) :-
  NCF is 100 - CF,
  asserta(fact(av(A, 'yes'), NCF, [])).

% B. Save positive statements as they are
save_fact(av(A, V), CF) :-
  asserta(fact(av(A, V), CF, [])).



% ============  Prove     =================

prove(N, IfList, LhsCF, Hist) :-
  prov(IfList, 100, LhsCF, [N | Hist]),
  !.

prove(N, _, _, _) :-
  bugdisp(['fail rule', N]),
  fail.


prov([], LhsCF, LhsCF, _).
prov([H | T], CurCF, LhsCF, Hist) :-
  findgoal(H, CF, Hist),
  NewCF is min(CurCF, CF),
  NewCF >= 20,
  prov(T, NewCF, LhsCF, Hist).



% ============  Adjust  ===================

adjust(CF1, CF2, CF) :-
  CF is round(CF1 * CF2 / 100).



% ============  Update  ===================

% A. Remove old fact and add new one with updated CF
update(Goal, NewCF, CF, RuleN) :-
  fact(Goal, OldCF, _),
  combine(NewCF, OldCF, CF),
  retract(fact(Goal, OldCF, OldRules)),
  asserta(fact(Goal, CF, [RuleN | OldRules])),
  !.

% B. Add fact for first time.
update(Goal, CF, CF, RuleN) :-
  asserta(fact(Goal, CF, [RuleN])).


% Equation used to calculate the new CF
combine(CF1, CF2, CF) :-
  CF is round(CF1 + CF2 * (100 - CF1) / 100).



% ============  Update  ===================

how :-
  writeln('Goal? '),
  read_sentence(InputList),
  av_list(Goal, InputList),
  how(Goal).

how(Goal) :-
  fact(Goal, CF, Rules),
  CF >= 20,
  Rules \== [],
  av_list(Goal, GoalList),
  nl,
  write_list(GoalList),
  write_list_ln([' was derived from rules:' | Rules]),
  nl,
  write_rules(Rules).

how(_).

av_list(not av(A, 'yes'), [not, A]) :- !.
av_list(not av(A, V), ['not', A, 'is', V]).
av_list(av(A, 'yes'), [A]) :- !.
av_list(av(A, V), [A, 'is', V]).


write_rules([]).
write_rules([N | T]) :-
  write_rule(N),
  how_lhs(N),
  write_rules(T).

write_rule(N) :-
  rule(N, lhs(IfList), rhs(Goal, CF)),
  tab(5), write_list_ln(['Rule ', N]),
  tab(5), write_list_ln(['If']),
  write_ifs(IfList),
  tab(5), write_list_ln(['Then']),
  av_list(Goal, GoalList),
  tab(5), write_list(GoalList),
  write_list_ln([' cf ', CF]).


write_ifs([]).
write_ifs([H | T]) :-
  av_list(H, HList),
  tab(10), write_list_ln(HList),
  write_ifs(T).


how_lhs(N) :-
  rule(N, lhs(IfList), _),
  how_ifs(IfList).

how_ifs([]).
how_ifs([Goal | X]) :-
  how(Goal),
  how_ifs(X).



% ============  DCG  ======================

% -- Statements that the DCG can generate
translate(top_goal(X)) --> ['goal', X].
translate(top_goal(X)) --> ['goal', 'is', X].
translate(askable(A, M, P)) --> ['ask', A], menu(M), prompt(P).
translate(rule(N, lhs(IF), rhs(THEN, CF))) --> id(N), if(IF), then(THEN, CF).
translate(output(A, V, PL)) -->  ['output'], statement(av(A, V)), itemlist(PL).


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
statement(not av(A, V)) --> ['not'], [A], (['is'] ; ['are']), [V].
statement(av(A, V)) --> [A], (['is'] ; ['are']), [V].
statement(av(A, 'yes')) --> [A].
