% Fonction de démarrage du jeu
demarrer() :- 
    demandeNiveau(Niveau),
    ((Niveau = q, writeln("Vous quittez le jeu."), retractall(voiture(_,_,_)), retractall(score(_)));
    (not(init(Niveau)), write("Ce niveau n'existe pas, veuillez reessayer."), demarrer());
    (init(Niveau), jouePartie())).


% Fonctions utilitaires de démarrage
demandeNiveau(Niveau) :-
    write("A quel niveau voulez-vous jouer ('q.' pour quitter) ?   "),
    read(Niveau).

demandeSiRecommence() :-
    write("Voulez vous refaire une partie (oui./non.) ? "),
    read(Reponse),
    ((Reponse = oui, demarrer());
    (Reponse = non, writeln("Vous quittez le jeu."), retractall(voiture(_,_,_)), retractall(score(_)));
    write("Je n'ai pas compris, veuillez reessayer."), demandeSiRecommence()).
    

% Mise en place de la base donnée (voiture & score) selon le niveau choisi
:- dynamic voiture/3, score/1.
score(0).
init(1):-
    % On nettoie la base de donnée d'éventuels variables présentes
    retractall(voiture(_,_,_)), 
    retractall(score(_)),
    % On ajoute les variables correspondantes au niveau (voitures + initialisation du score)
    assert(score(0)),
    assert(voiture(0,(3,3),(3,2))),
    assert(voiture('a',(1,2),(1,1))),
    assert(voiture('b',(4,1),(2,1))),
    assert(voiture('c',(6,1),(5,1))),
    assert(voiture('d',(4,4),(2,4))),
    assert(voiture('e',(3,6),(1,6))),
    assert(voiture('f',(5,6),(5,5))),
    assert(voiture('g',(6,5),(6,3))).

init(2) :-
    retractall(voiture(_,_,_)),
    retractall(score(_)),
    assert(score(0)),
    assert(voiture(0,(3,3),(3,2))),
    assert(voiture('a',(1,2),(1,1))),
    assert(voiture('b',(3,4),(1,4))),
    assert(voiture('c',(2,6),(1,6))),
    assert(voiture('d',(4,1),(2,1))),
    assert(voiture('e',(4,5),(2,5))),
    assert(voiture('f',(4,6),(3,6))),
    assert(voiture('g',(4,4),(4,2))),
    assert(voiture('h',(6,1),(5,1))),
    assert(voiture('i',(5,6),(5,5))),
    assert(voiture('j',(6,6),(6,5))).

init(3) :-
    retractall(voiture(_,_,_)),
    retractall(score(_)),
    assert(score(0)),
    assert(voiture(0,(3,2),(3,1))),
    assert(voiture('a',(2,1),(1,1))),
    assert(voiture('b',(1,6),(1,4))),
    assert(voiture('c',(2,3),(2,2))),
    assert(voiture('d',(3,4),(2,4))),
    assert(voiture('e',(4,3),(3,3))),
    assert(voiture('f',(5,6),(3,6))),
    assert(voiture('g',(6,3),(5,3))),
    assert(voiture('h',(5,5),(5,4))),
    assert(voiture('i',(6,6),(6,4))).


% Fonctions de jeu
jouePartie() :- 
    afficheGrille(),
    afficheScore(), 
    ((not(joueUnCoup()), demandeSiRecommence());
    ((partieGagnee(),
    write("Felicitation vous avez gagne !"),nl,
    write("en "),score(Score),write(Score),write(" coups"),nl,
    demandeSiRecommence());
    jouePartie())).

joueUnCoup() :- 
    recupereInfos(Id, Direction),
    ((Id = q, writeln("Vous quittez la partie."));   % Si le joueur veut quitter la partie
    (deplacement(Id, Direction), incrementeScore());    % Le joueur effectue un déplacement valide
    (not(deplacement(Id, Direction)),   % Le joueur propose un déplacement non valide
    writeln("Ce coup n'est pas possible, veuillez choisir un autre mouvement.  "),
    joueUnCoup())).


% Fonctions utilitaires de jeu
incrementeScore() :- score(Score), NewScore is Score + 1, retract(score(_)), assert(score(NewScore)).
partieGagnee() :- voiture(0,(3,6),(_,_)). % Par convention la voiture à sortir est la voiture d'indice 0.
recupereInfos(Id, Direction) :-
    write("Entrez l'identifiant de la voiture que voulez-vous deplacer ('q.' pour quitter) ?   "),
    read(Id),
    (Id \= q,
    write("Entrez la direction de deplacement souhaitee :   "),
    read(Direction)).


% Vérification de la disponibilité d'une case
caseEstOccupeeVoiture((I,J), Id) :-
    voiture(Id,(I,J1),(I,J2)),
    (J=J1 ; J=J2 ; J is J1-1). %voiture horizontale

caseEstOccupeeVoiture((I,J), Id) :-
    voiture(Id,(I1,J),(I2,J)),
    (I=I1 ; I=I2 ; I is I1-1). %voiture verticale

caseEstOccupee(I,J) :- 
    caseEstOccupeeVoiture((I,J), _).


% Vérification d'un mouvement donné
mouvementHautPossible(Id) :-
    voiture(Id, (_,J),(I2,J)),
    I2 > 1,
    IA is I2 - 1,
    not(caseEstOccupee(IA,J)).

mouvementBasPossible(Id) :-
    voiture(Id,(I1,J),(_,J)),
    I1 < 6,
    IA is I1 + 1,
    not(caseEstOccupee(IA,J)).

mouvementDroitePossible(Id) :-
    voiture(Id,(I,J1),(I,_)),
    J1 < 6,
    JA is J1 + 1,
    not(caseEstOccupee(I,JA)).

mouvementGauchePossible(Id) :-
    voiture(Id,(I,_),(I,J2)),
    J2 > 1,
    JA is J2 -1,
    not(caseEstOccupee(I,JA)).

mouvementPossible(Id, Direction) :-
    (Direction = haut, mouvementHautPossible(Id));
    (Direction = bas, mouvementBasPossible(Id));
    (Direction = droite, mouvementDroitePossible(Id));
    (Direction = gauche, mouvementGauchePossible(Id)).


% Déplacement dans une direction donnée
deplacementHaut(Id) :-
    mouvementHautPossible(Id),
    voiture(Id,(Xa,Ya),(Xb,Yb)),
    NewXa is Xa-1,
    NewXb is Xb-1,
    NewVoiture = voiture(Id,(NewXa,Ya),(NewXb,Yb)),
    retract(voiture(Id,(Xa,Ya),(Xb,Yb))),
    assert(NewVoiture).

deplacementBas(Id) :-
    mouvementBasPossible(Id),
    voiture(Id,(Xa,Ya),(Xb,Yb)),
    NewXa is Xa+1,
    NewXb is Xb+1,
    NewVoiture = voiture(Id,(NewXa,Ya),(NewXb,Yb)),
    retract(voiture(Id,(Xa,Ya),(Xb,Yb))),
    assert(NewVoiture).

deplacementDroite(Id) :-
    mouvementDroitePossible(Id),
    voiture(Id,(Xa,Ya),(Xb,Yb)),
    NewYa is Ya+1,
    NewYb is Yb+1,
    NewVoiture = voiture(Id,(Xa,NewYa),(Xb,NewYb)),
    retract(voiture(Id,(Xa,Ya),(Xb,Yb))),
    assert(NewVoiture).
        
deplacementGauche(Id) :-
    mouvementGauchePossible(Id),
    voiture(Id,(Xa,Ya),(Xb,Yb)),
    NewYa is Ya-1,
    NewYb is Yb-1,
    NewVoiture = voiture(Id,(Xa,NewYa),(Xb,NewYb)),
    retract(voiture(Id,(Xa,Ya),(Xb,Yb))),
    assert(NewVoiture).

deplacement(Id, Direction) :- 
    (Direction = haut, deplacementHaut(Id));
    (Direction = bas, deplacementBas(Id));
    (Direction = droite, deplacementDroite(Id));
    (Direction = gauche, deplacementGauche(Id)).


% Fonctions d'affichage
afficheGrille() :- afficheGrille(1,1).
afficheGrille(1,1) :- write("  ======"), nl, write("||"), afficheCase(1,1), afficheGrille(1,2).% Première case de la première ligne
afficheGrille(6,6) :- afficheCase(6,6), write("||"), nl, write("  ======"),nl. % Dernière case de la dernière ligne
afficheGrille(3,6) :- afficheCase(3,6), write(">>"),nl, afficheGrille(4,1). % Case de sortie
afficheGrille(I,1) :- write("||"), afficheCase(I,1), afficheGrille(I,2). % Première case d'une ligne
afficheGrille(I,6) :- afficheCase(I,6), write("||"),nl,NewI is I+1, afficheGrille(NewI,1). % Dernière case d'une ligne
afficheGrille(I,J) :- afficheCase(I,J),NewJ is J+1, afficheGrille(I,NewJ). % Case pas au bord d'une ligne

afficheScore() :- score(Score),write("Score : "),tab(1),write(Score),nl.
afficheCase(I,J) :- caseEstOccupeeVoiture((I,J),Id),write(Id), !.
afficheCase(I,J) :- not(caseEstOccupeeVoiture((I,J),_)),write("_").