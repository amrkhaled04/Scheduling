ta_slot_assignment(TAs, RemTAs, Name) :-
    select(ta(Name, Load), TAs, RestTAs),
    NewLoad is Load - 1, 
    append([ta(Name, NewLoad)], RestTAs, RemTAs). 

slot_assignment(0, TAs, TAs, []).
slot_assignment(LabsNum, TAs, RemTAs, Assignment) :-
    LabsNum > 0,
    select(ta(Name, Load), TAs, RestTAs),
    NewLoad is Load - 1, 
    NewLabs is LabsNum - 1,
    slot_assignment(NewLabs, [ta(Name, NewLoad)|RestTAs], RemTAs, RestAssignment),
    \+ member(Name, RestAssignment), 
    append([Name], RestAssignment, Assignment). 


checkl([]):- !.
checkl([[]]):- !.
checkl(DaySched) :-
	DaySched = [H|T],
	is_list(H),
	length(H,L),
	L > 1,
	checkrep(H),!,
	checkl(T).
checkl(DaySched) :-
	DaySched = [H|T],
	is_list(H),
	length(H,L),
	L = 1,
	checkl(T).
checkl(DaySched) :-
	DaySched = [H|T],
	length(H,R),
	R is 0.
checkrep([]).
checkrep([H|T]):-
	\+member(H,T),
	checkrep(T).

max_slots_per_day([],_).
max_slots_per_day([[]],_):- !.
max_slots_per_day(DaySched,Max):- 
    max_slots_per_day(DaySched,Max,false).

max_slots_per_day(DaySched,Max,false) :-
    flatten(DaySched,L),
	length(L,R),
	R > 0,
    L = [H|T],
    count(H,L,C),
    C =< Max,
    removeocc(H,L,L1),
    max_slots_per_day(DaySched,Max,true),!,
    checkl(DaySched).
	
max_slots_per_day(DaySched,Max,false) :-
    flatten(DaySched,L),
	length(L,R),
	R is 0,
	max_slots_per_day(DaySched,Max,true).

max_slots_per_day(_,_,true):-!.

	
	
removeocc(_,[],[]):-!.
removeocc(X,[X|T],R) :- removeocc(X,T,R),!.
removeocc(X,[H|T],[H|R]) :- dif(H,X), removeocc(X,T,R),!.

count(_, [], 0):- !.
count(X, [X | T], N) :-
  !, count(X, T, N1),
  N is N1 + 1.
count(X, [_ | T], N) :-
  count(X,T,N),!.
	
day_schedule(DaySlots, TAs, RemTAs, Assignment) :-
    slot_assignment_list(DaySlots, TAs, RemTAs, Assignment),
    update_TAs(Assignment, TAs, RemTAs).

slot_assignment_list([], TAs, TAs, []).
slot_assignment_list([Slot|Rest], TAs, RemTAs, [Assignment|RestAssignment]) :-
    slot_assignment(Slot, TAs, TempTAs, Assignment),
    slot_assignment_list(Rest, TempTAs, RemTAs, RestAssignment).

update_TAs([], TAs, TAs).
update_TAs([Slot|Rest], TAs, RemTAs) :-
    update_TA_helper(Slot, TAs, TempTAs),
    update_TAs(Rest, TempTAs, RemTAs).

update_TA_helper([], TAs, TAs).
update_TA_helper([Name|Rest], TAs, RemTAs) :-
    ta_slot_assignment(TAs, TempTAs, Name),
    update_TA_helper(Rest,TempTAs,RemTAs).
	
week_schedule(WeekSlots, TAs, DayMax, WeekSched) :-
    check_week_schedule(WeekSlots, TAs, DayMax, WeekSched).

check_week_schedule([], _, _, []).
check_week_schedule([DaySlots|RestWeekSlots], TAs, DayMax, [DaySchedule|RestWeekSched]) :-
    day_schedule(DaySlots, TAs, RemTAs, DaySchedule),
    max_slots_per_day(DaySchedule, DayMax),
    check_week_schedule(RestWeekSlots,RemTAs,DayMax,RestWeekSched).