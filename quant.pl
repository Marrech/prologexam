%%%% Furfaro Daniel 810933

%%%Unita  base e derivate:
is_unit(m, metre, 6, [km, hm, dam, m ,dm, cm, mm]).
is_unit(kg, kilogram, 7, [t, q, mag, kg, hg, dag, g]).
is_unit(s, second,5, [ks, hs, das, s, ds, cs, ms]).
is_unit('A', ampere, 4, [kA, hA, daA, 'A', dA, cA, mA]).
is_unit('K', kelvin, 3, [kK, hK, daK, 'K', dK, cK, mK]).
is_unit(cd, candela, 2, [kcd, hcd, dacd, cd, dcd, ccd, mcd]).
is_unit(mol, mole, 1, [kmol, hmol, damol, mol, dmol, cmol, mmol]).

is_der_dimension('S', siemens, -16,
                 kg ** -1 * m ** -2  * s ** 3 * 'A' ** 2,
                 [kS, hS, daS, 'S', dS, cS, mS]).
is_der_dimension('Bq', becquerel, -1,
                 s** -1, [kBq, hBq, daBq, 'Bq', dBq, cBq, mBq]).
is_der_dimension(dc, degreecelsius, -2, 'K',
                 [kdc, hdc, dadc, dc, ddc, cdc, mdc]).
is_der_dimension('C', coulomb, -3,
                 s * 'A', [kC, hC, daC, 'C', dC, cC, mC]).
is_der_dimension('F', farad, -4,
                 kg** -1 * m** -2 * s** 4 * 'A'** 2,
                 [kF, hF, daF, 'F', dF, cF, mF]).
is_der_dimension('Gy', gray, -5, m** 2 * s** -2,
                 [kGy, hGy, daGy, 'Gy', dGy, cGy, mGy]).
is_der_dimension('Hz', hertz, -6,
                 s** -1, [kHz, hHz, daHz, 'Hz', dHz, cHz, mHz]).
is_der_dimension('H', henry, -7, kg * m** 2 * s** -2 * 'A'** -2,
                 [kH, hH, daH, 'H', dH, cH, mH]).
is_der_dimension('J', joule, -8, kg * m** 2 * s** -2,
                 [kJ, hJ, daJ, 'J', dJ, cJ, mJ]).
is_der_dimension(kat, katal, -9, mol * s ** -1,
                 [kkat, hkat, dakat, kat, dkat, ckat, mkat]).
is_der_dimension(lm, lumen, -10, cd * sr,
                 [klm, hlm, dalm, lm, dlm, clm, mlm]).
is_der_dimension(lx, lux, -11, m** -2 * cd,
                 [klx, hlx, dalx, lx, dlx, clx, mlx]).
is_der_dimension('N', newton, -12, kg * m * s** -2,
                 [kN, hN, daN, 'N', dN, cN, mN]).
is_der_dimension('Omega', ohm, -13, kg * m**2 * s** -3 * 'A'** -2,
        [kOmega, hOmega, dOmega, 'Omega', dOmega, cOmega, mOmega]).
is_der_dimension('Pa', pascal, -14, kg * m** -1 * s** -2,
                 [kPa, hPa, daPa, 'Pa', dPa, cPa, mPa]).
is_der_dimension(rad, radian, -15, m * m** -1,
                 [krad, hrad, darad, rad, drad, crad, mrad]).
is_der_dimension('Sv', sievert, -17, m** 2 * s** -2,
                 [kSa, hSa, daSa, 'Sa', dSa, cSa, mSa]).
is_der_dimension(sr, steradian, -18, m**2 * m** -2,
                 [ksr, hsr, dasr, sr, dsr, csr, msr]).
is_der_dimension('T', tesla, -19, kg * s** -2 * 'A'** -1,
                 [kT, hT, daT, 'T', dT, cT, mT]).
is_der_dimension('V', volt, -20, kg * m**2 * s** -3 * 'A'** -1,
                 [kV, hV, daV, 'V', dV, cV, mV]).
is_der_dimension('W', watt, -21, kg * m**2 * s** -3,
                 [kW, hW, daW, 'W', dW, cW, mW]).
is_der_dimension('Wb', weber, -22, kg * m**2 * s** -2 * 'A'** -1,
                 [kWb, hWb, daWb, 'Wb', dWb, cWb, mWb]).

%%%Predicati principali:

is_dimension(A) :-
    is_unit(A, _, _, _),
    !.
is_dimension(A) :-
    check_if_submultiple_rep(A),
    !.
is_dimension(A) :-
    is_der_dimension(A,_,_,_, _),
    !.
is_dimension(A ** N) :-
    is_dimension(A),
    number(N),
    !.
is_dimension(A * B) :-
    is_dimension(A),
    is_dimension(B),
    !.
is_dimension(A) :-
    convert_in_base(A, BaseA),
    is_der_dimension(_,_,_,BaseA, _),
    !.

q(N, D) :-
    number(N),
    is_dimension(D).

is_siu(S) :-
    is_dimension(S),
    !.

is_base_siu(S) :-
    is_unit(S, _, _, _),
    !.

siu_name(S, N) :-
    is_unit(S, N, _, _),
    !.
siu_name(S, N) :-
    is_der_dimension(S, N, _, _, _),
    !.

siu_symbol(N, S) :-
    is_unit(S, N, _, _),
    !.
siu_symbol(N, S) :-
    is_der_dimension(S, N, _, _, _),
    !.

is_quantity(q(N, D)) :-
    number(N),
    is_dimension(D).

cmp_units(Result, U1, U2) :-
    is_siu(U1),
    is_siu(U2),
    get_order_of_unit(U1, I1),
    get_order_of_unit(U2, I2),
    call(Result, I1, I2).

siu_base_expansion(S, Expansion) :-
    is_der_dimension(S, _, _, Sexp, _),
    is_dimension(Expansion),
    is_dimension(Sexp),
    Expansion = Sexp.
siu_base_expansion(S, Expansion) :-
    is_der_dimension(S, _, _, Expansion, _).

normalize(Dim, NewDim) :-
    normalize_dimension(Dim, NewDimTmp),
    convert_list_to_dimension(NewDimTmp, NewDim).

qplus(q(N1, U1), q(N2, U2), QR) :-
    Q1 =.. [q, N1, U1],
    Q2 =.. [q, N2, U2],
    is_temperature(Q1, Q1K),
    is_temperature(Q2, Q2K),
    Q1K =.. [q, NF1, U1F],
    Q2K =.. [q, NF2, U2F],
    is_siu(U2F),
    TmpN is NF1 + NF2,
    QR = q(TmpN, U1F),
    !.
qplus(q(N1, U1), q(N2, U2), QR) :-
    U1 == U2,
    !,
    Q1 =.. [q, N1, U1],
    is_quantity(Q1),
    convert_and_get_list(U1,ListU1),
    sort_dim_list(ListU1, OrdListU1),
    convert_list_to_dimension(OrdListU1, FDim),
    is_der_dimension(D,_,_,FDim, _),
    TmpN is N1 + N2,
    QR = q(TmpN, D).
qplus(q(N1, U1), q(N2, U2), QR) :-
    Q1 =.. [q, N1, U1],
    Q2 =.. [q, N2, U2],
    check_if_submultiple(Q1, NewQ1),
    check_if_submultiple(Q2, NewQ2),
    is_quantity(NewQ1),
    is_quantity(NewQ2),
    NewQ1 =.. [q, N1D, U1D],
    NewQ2 =.. [q, N2D, U2D],
    U1D == U2D,
    normalize(U1D, NewU),
    TmpN is N1D + N2D,
    QR = q(TmpN, NewU).
qplus(q(N1, U1), q(N2, U2), QR) :-
    U1 == U2,
    !,
    Q1 =.. [q, N1, U1],
    Q2 =.. [q, N2, U2],
    is_quantity(Q1),
    is_quantity(Q2),
    normalize(U1, NewU),
    TmpN is N1 + N2,
    QR = q(TmpN, NewU).

qsubtract(q(N1, U1), q(N2, U2), QR) :-
    Q1 =.. [q, N1, U1],
    Q2 =.. [q, N2, U2],
    is_temperature(Q1, Q1K),
    is_temperature(Q2, Q2K),
    Q1K =.. [q, NF1, U1F],
    Q2K =.. [q, NF2, U2F],
    is_siu(U2F),
    TmpN is NF1 - NF2,
    QR = q(TmpN, U1F).
qsubtract(q(N1, U1), q(N2, U2), QR) :-
    U1 == U2,
    Q1 =.. [q, N1, U1],
    is_quantity(Q1),
    convert_and_get_list(U1,ListU1),
    sort_dim_list(ListU1, OrdListU1),
    convert_list_to_dimension(OrdListU1, FDim),
    is_der_dimension(D,_,_,FDim, _),
    TmpN is N1 - N2,
    QR = q(TmpN, D).
qsubtract(q(N1, U1), q(N2, U2), QR) :-
    Q1 =.. [q, N1, U1],
    Q2 =.. [q, N2, U2],
    check_if_submultiple(Q1, NewQ1),
    check_if_submultiple(Q2, NewQ2),
    is_quantity(NewQ1),
    is_quantity(NewQ2),
    NewQ1 =.. [q, N1D, U1D],
    NewQ2 =.. [q, N2D, U2D],
    U1D == U2D,
    normalize(U1D, NewU),
    TmpN is N1D - N2D,
    QR = q(TmpN, NewU).
qsubtract(q(N1, U1), q(N2, U2), QR) :-
    Q1 =.. [q, N1, U1],
    Q2 =.. [q, N2, U2],
    is_quantity(Q1),
    is_quantity(Q2),
    U1 == U2,
    normalize(U1, NewU),
    TmpN is N1 - N2,
    QR = q(TmpN, NewU).

qtimes(q(N1, U1), q(N2, U2), QR) :-
    Q1 =.. [q, N1, U1],
    Q2 =.. [q, N2, U2],
    is_temperature(Q1, Q1K),
    is_temperature(Q2, Q2K),
    Q1K =.. [q, NF1, U1F],
    U1F == 'K',
    Q2K =.. [q, NF2, U2F],
    is_siu(U2F),
    TmpN is NF1 * NF2,
    QR = q(TmpN, 'K').
qtimes(q(N1, U1), q(N2, U2), QR) :-
    U1 == U2,
    Q1 =.. [q, N1, U1],
    is_quantity(Q1),
    convert_and_get_list(U1,ListU1),
    sort_dim_list(ListU1, OrdListU1),
    convert_list_to_dimension(OrdListU1, FDim),
    is_der_dimension(D,_,_,FDim, _),
    TmpN is N1 * N2,
    QR = q(TmpN, D ** 2).
qtimes(q(N1, U1), q(N2, U2), QR) :-
    U1 == U2,
    Q1 =.. [q, N1, U1],
    is_quantity(Q1),
    convert_and_get_list(U1,ListU1),
    sort_dim_list(ListU1, OrdListU1),
    convert_list_to_dimension(OrdListU1, FDim),
    TmpN is N1 * N2,
    QR = q(TmpN, FDim ** 2).
qtimes(q(N1, U1), q(N2, U2), QR) :-
    Q1 =.. [q, N1, U1],
    Q2 =.. [q, N2, U2],
    is_quantity(Q1),
    is_quantity(Q2),
    convert_and_get_list(U1,ListU1),
    convert_and_get_list(U2,ListU2),
    append(ListU1, ListU2, FList),
    sort_dim_list(FList, FlistSorted),
    convert_list_to_dimension(FlistSorted, FDim),
    is_der_dimension(D,_,_,FDim, _),
    TmpN is N1 * N2,
    QR = q(TmpN, D).
qtimes(q(N1, U1), q(N2, U2), QR) :-
    Q1 =.. [q, N1, U1],
    Q2 =.. [q, N2, U2],
    is_quantity(Q1),
    is_quantity(Q2),
    convert_and_get_list(U1,ListU1),
    convert_and_get_list(U2,ListU2),
    append(ListU1, ListU2, FList),
    sort_dim_list(FList, FlistSorted),
    convert_list_to_dimension(FlistSorted, FDim),
    TmpN is N1 * N2,
    QR = q(TmpN, FDim).
qtimes(q(N1, U1), q(N2, U2), QR) :-
    Q1 =.. [q, N1, U1],
    Q2 =.. [q, N2, U2],
    check_if_submultiple(Q1, NewQ1),
    check_if_submultiple(Q2, NewQ2),
    NewQ1 =.. [q, N1D, U1D],
    NewQ2 =.. [q, N2D, U2D],
    convert_and_get_list(U1D,ListU1),
    convert_and_get_list(U2D,ListU2),
    append(ListU1, ListU2, FList),
    sort_dim_list(FList, FlistSorted),
    convert_list_to_dimension(FlistSorted, FDim),
    is_der_dimension(D,_,_,FDim, _),
    TmpN is N1D * N2D,
    QR = q(TmpN, D),
    !.
qtimes(q(N1, U1), q(N2, U2), QR) :-
    Q1 =.. [q, N1, U1],
    Q2 =.. [q, N2, U2],
    check_if_submultiple(Q1, NewQ1),
    check_if_submultiple(Q2, NewQ2),
    NewQ1 =.. [q, N1D, U1D],
    NewQ2 =.. [q, N2D, U2D],
    convert_and_get_list(U1D,ListU1),
    convert_and_get_list(U2D,ListU2),
    append(ListU1, ListU2, FList),
    sort_dim_list(FList, FlistSorted),
    convert_list_to_dimension(FlistSorted, FDim),
    TmpN is N1D * N2D,
    QR = q(TmpN, FDim),
    !.

qdivide(q(N1, U1), q(N2, U2), QR) :-
    Q1 =.. [q, N1, U1],
    Q2 =.. [q, N2, U2],
    is_temperature(Q1, Q1K),
    is_temperature(Q2, Q2K),
    Q1K =.. [q, NF1, U1F],
    U1F == 'K',
    Q2K =.. [q, NF2, U2F],
    is_siu(U2F),
    TmpN is NF1 / NF2,
    QR = q(TmpN, 'K'),
    !.
qdivide(q(N1, U1), q(N2, U2), QR) :-
    U1 == U2,
    Q1 =.. [q, N1, U1],
    is_quantity(Q1),
    TmpN is N1 / N2,
    QR = q(TmpN, 1).
qdivide(q(N1, U1), q(N2, U2), QR) :-
    Q1 =.. [q, N1, U1],
    Q2 =.. [q, N2, U2],
    is_quantity(Q1),
    is_quantity(Q2),
    convert_and_get_list(U1,ListU1),
    convert_and_get_list(U2,ListU2),
    loop_invert_exp(ListU2,OppListU2),
    append(ListU1, OppListU2, FList),
    sort_dim_list(FList, FlistSorted),
    convert_list_to_dimension(FlistSorted, FDim),
    is_der_dimension(D,_,_,FDim, _),
    TmpN is N1 / N2,
    QR = q(TmpN, D).
qdivide(q(N1, U1), q(N2, U2), QR) :-
    Q1 =.. [q, N1, U1],
    Q2 =.. [q, N2, U2],
    check_if_submultiple(Q1, NewQ1),
    check_if_submultiple(Q2, NewQ2),
    NewQ1 =.. [q, N1D, U1D],
    NewQ2 =.. [q, N2D, U2D],
    convert_and_get_list(U1D,ListU1),
    convert_and_get_list(U2D,ListU2),
    loop_invert_exp(ListU2,OppListU2),
    append(ListU1, OppListU2, FList),
    sort_dim_list(FList, FlistSorted),
    convert_list_to_dimension(FlistSorted, FDim),
    is_der_dimension(D,_,_,FDim, _),
    TmpN is N1D / N2D,
    QR = q(TmpN, D),
    !.
qdivide(q(N1, U1), q(N2, U2), QR) :-
    Q1 =.. [q, N1, U1],
    Q2 =.. [q, N2, U2],
    check_if_submultiple(Q1, NewQ1),
    check_if_submultiple(Q2, NewQ2),
    NewQ1 =.. [q, N1D, U1D],
    NewQ2 =.. [q, N2D, U2D],
    convert_and_get_list(U1D,ListU1),
    convert_and_get_list(U2D,ListU2),
    loop_invert_exp(ListU2,OppListU2),
    append(ListU1, OppListU2, FList),
    sort_dim_list(FList, FlistSorted),
    convert_list_to_dimension(FlistSorted, FDim),
    TmpN is N1D / N2D,
    QR = q(TmpN, FDim),
    !.
qdivide(q(N1, U1), q(N2, U2), QR) :-
    Q1 =.. [q, N1, U1],
    Q2 =.. [q, N2, U2],
    is_quantity(Q1),
    is_quantity(Q2),
    convert_and_get_list(U1,ListU1),
    convert_and_get_list(U2,ListU2),
    loop_invert_exp(ListU2,OppListU2),
    append(ListU1, OppListU2, FList),
    sort_dim_list(FList, FlistSorted),
    convert_list_to_dimension(FlistSorted, FDim),
    TmpN is N1 / N2,
    QR = q(TmpN, FDim).

qexpt(N, q(N2, D), QR) :-
    Q1 =.. [q, N2, D],
    is_quantity(Q1),
    normalize(D, ND),
    number(N),
    Op is N ** N2,
    QR =.. [q, Op, **(ND)].

%%% Predicati secondari e di utilita':

%%%is_temperature/2 : Utilizzato per il controllo delle unita di tipo
%%%                  "temperatura" ed eventuali conversioni di queste
is_temperature(q(N, U), Result):-
    U == 'K',
    Result =.. [q, N, U].
is_temperature(q(N, U), Result):-
    U == cd,
    Nu = N + 273.15,
    Result =.. [q, Nu, U].
is_temperature(q(N, U), Result):-
    U == 'F',
    Nu = (5/9) * N + 255.37,
    Result =.. [q, Nu, U].

%%%loop_submult_to_base/2 : Utilizzato per controllare e convertire
%%%                  sottomultipli in unita' base
loop_submult_to_base([], []).
loop_submult_to_base([H|T], [H1|T1]) :-
    check_if_submultiple_in_norm(H, H1),
    loop_submult_to_base(T, T1).

%%%check_if_submultiple_rep/1 : Utilizzato come supporto al predicato
%%%                  "loop_submult_to_base/1"
check_if_submultiple_rep(D):-
    is_unit(_,_,_,List),
    member(D, List).
check_if_submultiple_rep(D):-
    is_unit(_,_,_,List),
    member(D, List).
check_if_submultiple_rep(D):-
    is_der_dimension(_, _, _, _, List),
    member(D, List).

%%%check_if_submultiple_in_norm/2 :  Utilizzato per controllare e
%%%                  convertire sottomultipli in unita' base
%%%                  durante il procedimento di normalizzazione
check_if_submultiple_in_norm(U, R):-
    is_siu(U),
    is_der_dimension(Base, _, _, _, List),
    member(U, List),
    R = Base.
check_if_submultiple_in_norm(U, R):-
    is_siu(U),
    is_unit(Base, _, _, List),
    member(U, List),
    R = Base.
check_if_submultiple_in_norm(U ** N, R):-
    is_siu(U),
    is_der_dimension(Base, _, _, _, List),
    member(U, List),
    R = Base ** N.
check_if_submultiple_in_norm(U ** N, R):-
    is_siu(U),
    is_unit(Base, _, _, List),
    member(U, List),
    R = Base ** N.

%%%check_if_submultiple/2 : Utilizzato per la conversione di Quantita' 
%%%                  conteneti sottomultipli in Quantita' con unita' 
%%%                  base
check_if_submultiple(q(N, D), BaseQ):-
    Q1 =.. [q, N, D],
    is_quantity(Q1),
    is_der_dimension(Base, _, _, _, List),
    member(D, List),
    indexOf(List, D, Exp),
    NewN is N * (10**Exp),
    BaseQ =.. [q, NewN, Base].
check_if_submultiple(q(N, D), BaseQ):-
    Q1 =.. [q, N, D],
    is_quantity(Q1),
    is_unit(Base,_,_,List),
    member(D, List),
    indexOf(List, D, Exp),
    NewN is N * (10**Exp),
    BaseQ =.. [q, NewN, Base].
check_if_submultiple(q(N, D ** N2), BaseQ):-
    Q1 =.. [q, N, D],
    is_quantity(Q1),
    is_der_dimension(Base, _, _, _, List),
    member(D, List),
    indexOf(List, D, Exp),
    NewN is N * (10**Exp),
    BaseQ =.. [q, NewN, Base ** N2].
check_if_submultiple(q(N, D ** N2), BaseQ):-
    Q1 =.. [q, N, D],
    is_quantity(Q1),
    is_unit(Base,_,_,List),
    member(D, List),
    indexOf(List, D, Exp),
    NewN is N * (10**Exp),
    BaseQ =.. [q, NewN, Base ** N2].

%%%get_order_of_unit/2 : Utilizzato per ottenere il valore di "ordine"
%%%                      dell'unita'  passata
get_order_of_unit(D, I) :-
    remove_exp(D, DR),
    is_unit(DR, _, I, _).
get_order_of_unit(D, I) :-
    remove_exp(D, DR),
    is_der_dimension(DR, _, I, _, _).

%%%get_list/2 : Utilizzato per convertire una dimensione in lista
get_list(U * U2 * U3 * U4, List) :-
    List = [U, U2, U3, U4].
get_list(U * U2 * U3, List) :-
    List = [U, U2, U3].
get_list(U * U2, List) :-
    List = [U, U2].
get_list(U, List) :-
    is_siu(U),
    List = [U].
get_list(U ** N, List) :-
    is_siu(U),
    List = [U ** N].

%%%remove_exp/2 : Utilizzato per rimuovere l'esponente da un unita'
remove_exp(U ** N, OnlyU):-
    is_siu(U),
    number(N),
    OnlyU = U.
remove_exp(U, OnlyU):-
    is_siu(U),
    OnlyU = U.
plus_exp(U1 ** N1, U2 ** N2, Result, Exp) :-
    is_siu(U1),
    is_siu(U2),
    U1 == U2,
    number(N1),
    number(N2),
    NR is N1 + N2,
    Result = U1 ** NR,
    Exp = NR.
plus_exp(U1, U2 ** N2, Result, Exp) :-
    is_siu(U1),
    is_siu(U2),
    U1 == U2,
    number(N2),
    NR is 1 + N2,
    Result = U1 ** NR,
    Exp = NR.
plus_exp(U1 ** N1, U2, Result, Exp) :-
    is_siu(U1),
    is_siu(U2),
    U1 == U2,
    number(N1),
    NR is N1 + 1,
    Result = U1 ** NR,
    Exp = NR.
plus_exp(U1, U2, Result, Exp) :-
    is_siu(U1),
    is_siu(U2),
    U1 == U2,
    Result = U1 ** 2,
    Exp = 2.

%%%convert_in_base/2 : Utilizzato per convertire un un unita' derivata
%%%                    in una dimensione equivalente di sole unita' 
%%%                    base
convert_in_base(A, New) :-
    is_der_dimension(A, _, _, New, _).
convert_in_base(A ** N, New) :-
    is_der_dimension(A, _, _, D, _),
    New = D ** N.
convert_in_base(A, New) :-
    is_unit(A, _, _, _),
    New = A.
convert_in_base(A ** N, New) :-
    is_unit(A, _, _, _),
    New = A ** N.

%%%normalize_dimension/2 : Utilizzato come supporto al procedimento
%%%                        si normalizzazione
normalize_dimension(A * B * C * D * E * F * G * H, Result) :-
    TmpResD = [A,B,C,D,E,F, G, H],
    loop_submult_to_base(TmpResD, TmpResE),
    sort_dim_list(TmpResE, Result).
normalize_dimension(A * B * C * D * E * F * G, Result) :-
    TmpResD = [A,B,C,D,E,F, G],
    loop_submult_to_base(TmpResD, TmpResE),
    sort_dim_list(TmpResE, Result).
normalize_dimension(A * B * C * D * E * F, Result) :-
    TmpResD = [A,B,C,D,E,F],
    loop_submult_to_base(TmpResD, TmpResE),
    sort_dim_list(TmpResE, Result).
normalize_dimension(A * B * C * D * E, Result) :-
    TmpResD = [A,B,C,D,E],
    loop_submult_to_base(TmpResD, TmpResE),
    sort_dim_list(TmpResE, Result).
normalize_dimension(A * B * C * D, Result) :-
    TmpResD = [A,B,C,D],
    loop_submult_to_base(TmpResD, TmpResE),
    sort_dim_list(TmpResE, Result).
normalize_dimension(A * B * C, Result) :-
    TmpResC = [A,B,C],
    loop_submult_to_base(TmpResC, TmpResE),
    sort_dim_list(TmpResE, Result).
normalize_dimension(A * B, Result) :-
    TmpRes = [A,B],
    loop_submult_to_base(TmpRes, TmpResE),
    sort_dim_list(TmpResE, Result).
normalize_dimension(A ** N, Result) :-
    is_siu(A),
    number(N),
    TmpRes = [A],
    loop_submult_to_base(TmpRes, TmpResE),
    list_to_dim(TmpResE, B),
    Result = [B ** N].
normalize_dimension(A, Result) :-
    is_siu(A),
    TmpRes = [A],
    loop_submult_to_base(TmpRes, Result).

%%%sort_dim_list/2 : Data una lista di unita' , la riordina in base al
%%%                  loro valore di "ordine"
sort_dim_list(List,Sorted):-
    start_sort(List,[],Sorted).

start_sort([], Sorted, Sorted).

start_sort([H|T],Tmp,Sorted):-
    step_sort(H,Tmp,NTmp),
    start_sort(T,NTmp,Sorted).

step_sort(X,[Y|T],[Y|NT]):-
    get_order_of_unit(X,RX),
    get_order_of_unit(Y,RY),
    RX<RY,
    step_sort(X,T,NT).
step_sort(X,[Y|T],[X,Y|T]):-
    get_order_of_unit(X,RX),
    get_order_of_unit(Y,RY),
    RX>RY.
step_sort(X,[Y|T],[Res|T]):-
    get_order_of_unit(X,RX),
    get_order_of_unit(Y,RY),
    RX == RY,
    plus_exp(X, Y, Res, Exp),
    Exp \= 1,
    Exp \= 0.
step_sort(X,[Y|T],[NewX|T]):-
    get_order_of_unit(X,RX),
    get_order_of_unit(Y,RY),
    RX == RY,
    plus_exp(X, Y, _, Exp),
    Exp == 1,
    remove_exp(X, NewX).
step_sort(X,[Y|T],T):-
    get_order_of_unit(X,RX),
    get_order_of_unit(Y,RY),
    RX == RY,
    plus_exp(X, Y, _, Exp),
    Exp == 0.
step_sort(X,[],[X]).

%%%indexOf/3 : Utilizzato come supporto ai metodi di conversione da
%%%                  sottomultipli a unita' base.
indexOf([E|_], E, 3):- !.
indexOf([_|T], E, I):-
  indexOf(T, E, I1),
  !,
  I is I1-1.

%%%convert_list_to_dimension/2 : Utilizzato per convertire una
%%%                              lista di unita' in una dimensione.
convert_list_to_dimension(List, Dim):-
    list_to_dim(List, Dim).

list_to_dim([U1,U2,U3,U4,U5, U6, U7], Dim) :-
    Dim = U1 * U2 * U3 * U4 * U5 * U6 * U7.
list_to_dim([U1,U2,U3,U4,U5, U6], Dim) :-
    Dim = U1 * U2 * U3 * U4 * U5 * U6.
list_to_dim([U1,U2,U3,U4,U5], Dim) :-
    Dim = U1 * U2 * U3 * U4 * U5.
list_to_dim([U1,U2,U3,U4], Dim) :-
    Dim = U1 * U2 * U3 * U4.
list_to_dim([U1,U2,U3], Dim) :-
    Dim = U1 * U2 * U3.
list_to_dim([U1,U2], Dim) :-
    Dim = U1 * U2.
list_to_dim([U1], Dim) :-
    Dim = U1.

%%%convert_and_get_list/2 : Converte una dimensione di unita' derivate
%%%                         in una lista di unita' base.
convert_and_get_list(A * B * C * D * E * F, Result) :-
    convert_in_base(A, CA),
    get_list(CA, ListCA),
    convert_in_base(B, CB),
    get_list(CB, ListCB),
    convert_in_base(C, CC),
    get_list(CC, ListCC),
    convert_in_base(D, CD),
    get_list(CD, ListCD),
    convert_in_base(E, CE),
    get_list(CE, ListCE),
    convert_in_base(F, CF),
    get_list(CF, ListCF),
    append(ListCA, ListCB, TmpAB),
    append(TmpAB, ListCC, TmpABC),
    append(TmpABC, ListCD, TmpABCD),
    append(TmpABCD, ListCE, TmpABCDE),
    append(TmpABCDE, ListCF, Result).
convert_and_get_list(A * B * C * D * E, Result) :-
    convert_in_base(A, CA),
    get_list(CA, ListCA),
    convert_in_base(B, CB),
    get_list(CB, ListCB),
    convert_in_base(C, CC),
    get_list(CC, ListCC),
    convert_in_base(D, CD),
    get_list(CD, ListCD),
    convert_in_base(E, CE),
    get_list(CE, ListCE),
    append(ListCA, ListCB, TmpAB),
    append(TmpAB, ListCC, TmpABC),
    append(TmpABC, ListCD, TmpABCD),
    append(TmpABCD, ListCE, Result).
convert_and_get_list(A * B * C * D, Result) :-
    convert_in_base(A, CA),
    get_list(CA, ListCA),
    convert_in_base(B, CB),
    get_list(CB, ListCB),
    convert_in_base(C, CC),
    get_list(CC, ListCC),
    convert_in_base(D, CD),
    get_list(CD, ListCD),
    append(ListCA, ListCB, TmpAB),
    append(TmpAB, ListCC, TmpABC),
    append(TmpABC, ListCD, Result).
convert_and_get_list(A * B * C, Result) :-
    convert_in_base(A, CA),
    get_list(CA, ListCA),
    convert_in_base(B, CB),
    get_list(CB, ListCB),
    convert_in_base(C, CC),
    get_list(CC, ListCC),
    append(ListCA, ListCB, TmpAB),
    append(TmpAB, ListCC, Result).
convert_and_get_list(A * B, Result) :-
    convert_in_base(A, CA),
    get_list(CA, ListCA),
    convert_in_base(B, CB),
    get_list(CB, ListCB),
    append(ListCA, ListCB, Result).
convert_and_get_list(A, Result) :-
    convert_in_base(A, CA),
    get_list(CA, ListCA),
    Result = ListCA.
convert_and_get_list(A ** N, Result) :-
    convert_in_base(A, CA),
    number(N),
    Result = (CA ** N).

%%%loop_invert_exp/2 : Utilizzato per ottenere gli opposti delle unita'
%%%                   elevate ad un numero
loop_invert_exp([], []).
loop_invert_exp([H|T], [H1|T1]) :-
    invert_exp(H, H1),
    loop_invert_exp(T, T1).

invert_exp(U1 ** N, Result):-
    N < 0,
    is_siu(U1),
    number(N),
    NN is abs(N),
    Result = U1 ** NN.
invert_exp(U1 ** N, Result):-
    is_siu(U1),
    number(N),
    NN is N - (N * 2),
    Result = U1 ** NN.
invert_exp(U1, Result):-
    is_siu(U1),
    NN = -1,
    Result = U1 ** NN.
