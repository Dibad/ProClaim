:- dynamic ghoul/1, fact/3, rule/1, rule/3, asked/1, askable/1, askable/4,
	ruletrace/0, multivalued/1.

:- op(900, fy, not).

:- initialization(main).        % Start shell on load

:- use_module('prolog/tokenize.pl').



main :-
	do_over,
	super.

% The main command loop

super :-
	repeat,
	writeln('consult  restart  load  list  trace on/off  how  exit'),
	read_sentence([Command | Arg]),
	doit([Command | Arg]),
	Command == exit.

doit([consult]) :- top_goals,!.
doit([restart]) :- do_over,!.
doit([load]) :- load_rules,!.
doit([list]) :- list_facts,!.
doit([trace,X]) :- set_trace(X),!.
doit([how | Arg]) :- how(Arg),!.
doit([exit]).
doit([Command | Arg]) :-
	write('invalid command : '),
	write([Command | Arg]),nl.

% top_goals works through each of the goals in sequence

top_goals :-
	ghoul(Attr),
	top(Attr),
	print_goal(Attr),
	fail.
top_goals.

% top starts the backward chaining by looking for rules that reference
% the attribute in the RHS. If it is known with certainty 100, then
% no other rules are tried, and other candidates are eliminated.  Otherwise
% other rules which might yield different values for the attribute
% are tried as well

top(Attr) :-
	findgoal(av(Attr,_Val),_CF,[goal(Attr)]),!.
top(_) :- true.

% prints all hypotheses for a given attribute

print_goal(Attr) :-
	nl,
	fact(av(Attr,X),CF,_),
	CF >= 20,
  print(av(Attr, X)),
	outp(av(Attr,X),CF),nl,
	fail.
print_goal(Attr) :-write('done with '),write(Attr),nl,nl.

outp(av(A,V),CF) :-
	output(A,V,PrintList),
	pretty(av(A,V), X),
	printlist(X),
	tab(1),write(cf(CF)),write(': '),
	printlist(PrintList),!.
outp(av(A,V),CF) :-
	pretty(av(A,V), X),
	printlist(X),
	tab(1),write(cf(CF)).

printlist([]).
printlist([H|T]) :-
	write(H),tab(1),
	printlist(T).

% findgoal is the guts of the inference.  It copes with already known
% attribute value pairs, multivalued attributes and single valued
% attributes.  It uses the EMYCIN certainty factor arithmetic to
% propagate uncertainties.

% 1 - if its recorded and the value matches, we're done, if the
%     value doesn't match, but its single valued and known with
%     certainty 100 definitely fail

findgoal(X,_Y,_) :- bugdisp(['  ',X]),fail.

findgoal(not Goal,NCF,Hist) :-
	findgoal(Goal,CF,Hist),
	NCF is - CF, !.
findgoal(Goal,CF,_Hist) :-
	fact(Goal,CF,_), !.
%findgoal(av(Attr,Val),CF) :-
%	bound(Val),
%	fact(av(Attr,V,_),CF),
%	Val \= V,
%	single_valued(Attr),
%	CF=100,
%	!,fail.

% 2 - if its askable, just ask and record the answer

findgoal(Goal,CF,Hist) :-
	can_ask(Goal,Hist),
	!,
	findgoal(Goal,CF,Hist).

% 3 - find a rule with the required attribute on the RHS.  try to prove
%     the LHS.  If its proved, use the certainty of the LHS combined
%     with the certainty of the RHS to compute the cf of the derived
%     result

findgoal(Goal,CurCF,Hist) :-
	fg(Goal,CurCF,Hist).

fg(Goal,CurCF,Hist) :-
	rule(N, lhs(IfList), rhs(Goal,CF)),
	bugdisp(['call rule',N]),
	prove(N,IfList,Tally,Hist),
	bugdisp(['exit rule',N]),
  atom_number(CF, NumCF),
	adjust(NumCF,Tally,NewCF),
	update(Goal,NewCF,CurCF,N),
	CurCF == 100,!.
fg(Goal,CF,_) :- fact(Goal,CF,_).

% can_ask shows how to query the user for various types of goal patterns

can_ask(av(Attr,_Val),Hist) :-
	not asked(av(Attr,_)),
	askable(Attr,Menu,Edit,Prompt),
	query_user(Attr,Prompt,Menu,Edit,Hist),
	asserta( asked(av(Attr,_)) ).

% answer the how question at the top level, to explain how an answer was
% derived.  It can be called successive times to get the whole proof.

how([]) :-
	writeln('Goal? '),
	read_sentence(X),
	pretty(Goal,X),
	how(Goal).
how(X) :-
	pretty(Goal,X),
	nl,
	how(Goal).

how(not Goal) :-
	fact(Goal,CF,Rules),
	CF < -20,
	pretty(not Goal,PG),
	write_line([PG,was,derived,from,'rules: '|Rules]),
	nl,
	list_rules(Rules),
	fail.
how(Goal) :-
	fact(Goal,CF,Rules),
	CF > 20,
	pretty(Goal,PG),
	write_line([PG,was,derived,from,'rules: '|Rules]),
	nl,
	list_rules(Rules),
	fail.
how(_).

list_rules([]).
list_rules([R|X]) :-
	list_rule(R),
%	how_lhs(R),
	list_rules(X).

list_rule(N) :-
	rule(N, lhs(Iflist), rhs(Goal,CF)),
	write_line(['rule  ',N]),
	write_line(['  If']),
	write_ifs(Iflist),
	write_line(['  Then']),
	pretty(Goal,PG),
	write_line(['   ',PG,CF]),nl.

write_ifs([]).
write_ifs([H|T]) :-
	pretty(H,HP),
	tab(4),write_line(HP),
	write_ifs(T).

pretty(av(A,yes),[A]) :- !.
pretty(not av(A,yes), [not,A]) :- !.
pretty(av(A,no),[not,A]) :- !.
pretty(not av(A,V),[not,A,is,V]).
pretty(av(A,V),[A,is,V]).

how_lhs(N) :-
	rule(N, lhs(Iflist), _),
	!, how_ifs(Iflist).

how_ifs([]).
how_ifs([Goal|X]) :-
	how(Goal),
	how_ifs(X).

% get input from the user.  either a straight answer from the menu, or
% an answer with cf N appended to it.

query_user(Attr,Prompt,[yes,no],_,Hist) :-
	!,
	write(Prompt),nl,
	get_user(X,Hist),
	get_vcf(X,Val,CF),
	asserta( fact(av(Attr,Val),CF,[user]) ).
query_user(Attr,Prompt,Menu,_Edit,Hist) :-
	write(Prompt),nl,
	menu_read(VList,Menu,Hist),
	assert_list(Attr,VList).

menu_read(X,Menu,Hist) :-
	write_list(2,Menu),
	get_user(X,Hist).

get_user(X,Hist) :-
	repeat,
	writeln(': '),
	read_sentence(X),
	process_ans(X,Hist).

process_ans([why],Hist) :- nl,write_hist(Hist), !, fail.
process_ans(_,_).

rite_hist([]) :- nl.
write_hist([goal(X)|T]) :-
	write_line([goal,X]),
	!, write_hist(T).
write_hist([N|T]) :-
	list_rule(N),
	!, write_hist(T).

write_list(_,[]).
write_list(N,[H|T]) :-
	tab(N),write(H),
	write_list(N,T).

assert_list(_,[]).
assert_list(Attr,[not,Val,cf,CF|X]) :-
	!,
	NCF is - CF,
	asserta( fact(av(Attr,Val),NCF,[user]) ),
	assert_list(Attr,X).
assert_list(Attr,[not,Val|X]) :-
	!,
	asserta( fact(av(Attr,Val),-100,[user]) ),
	assert_list(Attr,X).
assert_list(Attr,[Val,cf,CF|X]) :-
	!,
	asserta( fact(av(Attr,Val),CF,[user]) ),
	assert_list(Attr,X).
assert_list(Attr,[Val|X]) :-
	asserta( fact(av(Attr,Val),100,[user]) ),
	assert_list(Attr,X).

get_vcf([no],yes,-100).
get_vcf([no,CF],yes,NCF) :- NCF is -CF.
get_vcf([no,cf,CF],yes,NCF) :- NCF is -CF.
get_vcf([Val,CF],Val,CF).
get_vcf([Val,cf,CF],Val,CF).
get_vcf([Val],Val,100).
get_vcf([not,Val],Val,-100).
get_vcf([not,Val,CF],Val,NCF) :- NCF is -CF.
get_vcf([not,Val,cf,CF],Val,NCF) :- NCF is -CF.

% prove works through a LHS list of premises, calling findgoal on
% each one.  the total cf is computed as the minimum cf in the list

prove(N,IfList,Tally,Hist) :-
	prov(IfList,100,Tally,[N|Hist]),!.
prove(N,_,_) :-
	bugdisp(['fail rule',N]),
	fail.

prov([],Tally,Tally,_Hist).
prov([H|T],CurTal,Tally,Hist) :-
	findgoal(H,CF,Hist),
	minimum(CurTal,CF,Tal),
	Tal >= 20,
	prov(T,Tal,Tally,Hist).

% update - if its already known with a given cf, here is the formula
% for adding in the new cf.  this is used in those cases where multiple
% RHS reference the same attr :val

update(Goal,NewCF,CF,RuleN) :-
	fact(Goal,OldCF,_),
	combine(NewCF,OldCF,CF),
	retract( fact(Goal,OldCF,OldRules) ),
	asserta( fact(Goal,CF,[RuleN | OldRules]) ),
	(CF == 100, single_valued(Attr), erase_other(Attr);
	 true),!.
update(Goal,CF,CF,RuleN) :-
	asserta( fact(Goal,CF,[RuleN]) ).

erase_other(Attr) :-
	fact(av(Attr,Val),CF,_),
	CF < 100,
	retract( fact(av(Attr,Val),CF,_) ),
	fail.
erase_other(_Attr) :-true.

adjust(CF1,CF2,CF) :-
	X is CF1 * CF2 / 100,
	int_round(X,CF).

combine(CF1,CF2,CF) :-
	CF1 >= 0,
	CF2 >= 0,
	X is CF1 + CF2*(100 - CF1)/100,
	int_round(X,CF).
combine(CF1,CF2,CF) :-
	CF1 < 0,
	CF2 < 0,
	X is - ( -CF1 -CF2 * (100 + CF1)/100),
	int_round(X,CF).
combine(CF1,CF2,CF) :-
	(CF1 < 0; CF2 < 0),
	(CF1 > 0; CF2 > 0),
	abs_minimum(CF1,CF2,MCF),
	X is 100 * (CF1 + CF2) / (100 - MCF),
	int_round(X,CF).

abs_minimum(A,B,X) :-
	absolute(A, AA),
	absolute(B, BB),
	minimum(AA,BB,X).

absolute(X, X) :- X >= 0.
absolute(X, Y) :- X < 0, Y is -X.

%minimum(A,B,A) :- A =< B.
%minimum(A,B,B) :- B > A.

%min([],X,X).
%min([H|T],Z,X) :-
%	H < Z,
%	min(T,H,X).
%min([H|T],Z,X) :-
%	H >= Z,
%	min(T,Z,X).

minimum(X,Y,X) :- X =< Y,!.
minimum(X,Y,Y) :- Y =< X.

int_round(X,I) :-
	X >= 0,
	I is integer(X + 0.5).
int_round(X,I) :-
	X < 0,
	I is integer(X - 0.5).

set_trace(off) :-
	ruletrace,
	retract( ruletrace ).
set_trace(on) :-
	not ruletrace,
	asserta( ruletrace ).
set_trace(_).

single_valued(A) :-multivalued(A),!,fail.
single_valued(_) :-true.

list_facts :-
	fact(X,Y,_),
	write(fact(X,Y)),nl,
	fail.
list_facts :-true.

%
%
do_over :-
	retractall(asked(_)),
	retractall(fact(_,_,_)).

clear :-
	retractall(asked(_)),
	retractall(fact(_,_,_)),
	retractall(rule(_)),
	retractall(multivalued(_)),
	retractall(askable(_)),
	retractall(ghoul(_)).

blank_lines(0).
blank_lines(N) :-
	nl,
	NN is N - 1,
	blank_lines(NN).

bugdisp(L) :-
	ruletrace,
	write_line(L), !.
bugdisp(_).

write_line(L) :-
	flatten(L,LF),
	write_lin(LF).

write_lin([]) :- nl.
write_lin([H|T]) :-
	write(H), tab(1),
	write_lin(T).

% removed member and flatten, already in SWI-Prolog

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LDRULS - this module reads a rule file and translates it to internal
%          Prolog format for the Clam shell

load_rules :-
	writeln('Enter file name in single quotes (ex. ''car.ckb''.): '),
	/* read(F), */
	load_rules('car.ckb').

load_rules(F) :-
	clear_db,
	see(F),
	lod_ruls,
	write('rules loaded'),nl,
	seen, !.

lod_ruls :-
	repeat,
	read_sentence(L),
%	bug(L),
	process(L),
	L == [''].

process(['']) :- !.
process(L) :-
	trans(R,L,[]),
	bug(R),
	assertz(R), !.
process(L) :-
	write('trans error on:'),nl,
	write(L),nl.

clear_db :-
	retractall(cf_model(_)),
	retractall(ghoul(_)),
	retractall(askable(_,_,_,_)),
	retractall(output(_, _, _)),
	retractall(rule(_, _, _)).

bug(cf_model(X)) :- write(cf_model(X)),nl,!.
bug(ghoul(X)):- write(ghoul(X)),nl,!.
bug(askable(A,_,_,_)):- write('askable '),write(A),nl,!.
bug(output(_A,V,_PL)):- write('output '),write(V),nl,!.
bug(rule(N,_,_)):- write('rule '),write(N),nl,!.
bug(X) :- write(X),nl.

% trans - translates a list of atoms in external rule form to internal
%         rule form

trans(cf_model(X)) --> [cf,model,X].
trans(cf_model(X)) --> [cf,model,is,X].
trans(cf_model(X)) --> [cf,X].
trans(ghoul(X)) --> [goal,is,X].
trans(ghoul(X)) --> [goal,X].
trans(askable(A,M,E,P)) -->
	[ask,A],menux(M),editchk(E),prompt(A,P).
trans(output(A,V,PL)) -->
	[output],phraz(av(A,V)),plist(PL).
trans(rule(N,lhs(IF),rhs(THEN,CF))) --> id(N),if(IF),then(THEN,CF).
trans(multivalued(X)) --> [multivalued,X].
trans('Parsing error'-L,L,_).

% default(D) -->  [default,D].
% default(none) -->  [].

menux(M) -->  [menu], menuxlist(M).

menuxlist([Item]) -->  [Item].
menuxlist([Item|T]) -->  [Item],menuxlist(T).

editchk(E) -->  [edit,E].
editchk(none) -->  [].

prompt(_,P) -->  [prompt,P].
prompt(P,P) -->  [].

id(N) --> [rule,N].

if(IF) --> [if],iflist(IF).

iflist([IF]) --> phraz(IF),[then].
iflist([Hif|Tif]) --> phraz(Hif),[and],iflist(Tif).
iflist([Hif|Tif]) --> phraz(Hif),[','],iflist(Tif).

then(THEN,CF) --> phraz(THEN),[cf],[CF].
then(THEN,100) --> phraz(THEN).

phraz(av(Attr,no)) --> [not,Attr].
phraz(av(Attr,no)) --> [not,a,Attr].
phraz(av(Attr,no)) --> [not,an,Attr].
phraz(av(Attr,Val)) --> [not,Attr,is,Val].
phraz(not av(Attr,Val)) --> [not,Attr,are,Val].
phraz(av(Attr,Val)) --> [Attr,is,Val].
phraz(av(Attr,Val)) --> [Attr,are,Val].
phraz(av(Attr,yes)) --> [Attr].

plist([Text]) --> [Text].
plist([Htext|Ttext]) --> [Htext],plist(Ttext).


% Split and tokenize a sentence
read_sentence(LA) :-
  read_string(current_input, ".", ". \n", _, String),
  split_string(String, '\n\t, ', '. \n', List_Strings),
  atomic_list_concat(List_Strings,' ', Atom),
  atomic_list_concat(LA,' ', Atom).
