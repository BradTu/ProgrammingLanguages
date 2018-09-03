% Assignemnt 7 by Brad Tully, 31 May 2018

% Create the person type and add all of the different names
person(kadja).
person(caledor).
person(crix).
person(enrik).

% Create the illness type and add all of the illnesses
illness(murlok_madness).
illness(demon_spots).
illness(butter_toes).
illness(void_fever).

% Create the place type and add all of the places
place(emerald_sanctuary).
place(steelforge).
place(swamp_of_despair).
place(greyrock_hold).

% Create the potion type and add all of the potions
potion(essence_of_durarara).
potion(dispel_magic).
potion(pixie_blush).
potion(lichen_kings_tea).

% this solves the riddle
solve :-
	% Make sure that all of the people have different illnesses
	illness(KadjaIllness), illness(CaledorIllness), illness(CrixIllness), illness(EnrikIllness),
	all_different([KadjaIllness, CaledorIllness, CrixIllness, EnrikIllness]),
	
	% Make sure that all of the places are different
	place(KadjaPlace), place(CaledorPlace), place(CrixPlace), place(EnrikPlace),
	all_different([KadjaPlace, CaledorPlace, CrixPlace, EnrikPlace]),
	
	% Make sure that all of the potions are different
	potion(KadjaPotion), potion(CaledorPotion), potion(CrixPotion), potion(EnrikPotion),
	all_different([KadjaPotion, CaledorPotion, CrixPotion, EnrikPotion]),
	
	% The different values are assigned for each person
	Quads = [ [kadja, KadjaIllness, KadjaPlace, KadjaPotion],
			  [caledor, CaledorIllness, CaledorPlace, CaledorPotion],
			  [crix, CrixIllness, CrixPlace, CrixPotion],
			  [enrik, EnrikIllness, EnrikPlace, EnrikPotion] ],
			  
			  % person, illness, place, potion
			  
	% 1 The recipe for the potion to treat Kadja's illness (which isn't the Pixie Blush recipe) can only be found in the Emerald Sanctuary.
	 
	 \+ member([kadja, _, _, pixie_blush], Quads),
	 member([kadja, _, emerald_sanctuary, _], Quads),
	
	% 2 The potion used to cure Caledor's malady isn't the one found in Steelforge. Crix's illness was easily treated with the Essence of Durarara potion. 
	
	\+ member([caledor, _, steelforge, _], Quads),
	member([crix, _, _, essence_of_durarara], Quads),
	
	% 3 Enrik was diagnosed with Murlok Madness (which was cured by a simple Dispel Magic potion). 
	%  The potion used to treat Demon Spots isn't the Pixie Blush recipe. 
	
	member([enrik, murlok_madness, _, dispel_magic], Quads),
	\+ member([_, demon_spots, _, pixie_blush], Quads),
	
	% 4 The potion recipe found only in the Swamp of Despair (which isn't the Pixie Blush recipe) is used to cure Butter Toes. 
	
	\+ member([_, _, swamp_of_despair, pixie_blush], Quads),
	member([_, butter_toes, swamp_of_despair, _], Quads),
	
	% Print out the solution to the riddle
	tell(kadja, KadjaIllness, KadjaPlace, KadjaPotion),
	tell(caledor, CaledorIllness, CaledorPlace, CaledorPotion),
	tell(crix, CrixIllness, CrixPlace, CrixPotion),
	tell(enrik, EnrikIllness, EnrikPlace, EnrikPotion).

% Makes sure that everything is different	
all_different([H | T]) :- member(H, T), !, fail.
all_different([_ | T]) :- all_different(T).
all_different([_]).

% Prints out the solutions
tell(A, B, C, D) :-
    write('Person: '), write(A), write(', Illness: '), write(B), write(', Potion Place: '), 
	write(C), write(', Potion: '), write(D), nl.