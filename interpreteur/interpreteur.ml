open List;;

let li = 
["if";
 " (";
 "(";
 "(";
 "7";
 " +";
 "2";
 ")";
 "*";
 " 2";
 ")";
 " >";
 "(";
 "5";
 "+";
 "1";
 ")";
 ")";
 "then";
 "if";
 "(";
 "3";
 "<";
 "5";
 ")";
 "then";
 "return";
 "3";
 ";";
 "else";
 "return";
 "2";
 ";";
 "endif";
 ";";
 "else";
 "return";
 "1";
 ";";
 "endif";
 ";";
];;


let li2 = ["("; "("; "10"; "*"; "2"; ")"; "*"; "2"; ")"];;
let li3 = ["("; "10"; "*"; "2"; ")"];;
let li4 = ["("; "3"; "-";"("; "10"; "*"; "2"; ")";")"];;
let li5 = ["fonction"; "fois";"(";"end";";"];;

let li6 = ["Fonction"; "fois"; "("; "int"; "X"; ","; "int"; "Y"; ")";
	   "return"; "("; "X"; "*"; "Y"; ")"; ";"; "END"];;

let string list list li7 = [[""]];;










(***********************************************************)
(*******************FONCTIONS*NON*DEMANDE*******************)

let rec getPlacCaractSys : string list * string * int -> int = fun(li, s, a) ->
  if(getCaractList(li, 0) != s)then
    getPlacCaractSys(getModList(li, 1), s, a+1)
  else
    a+1
;;

let getPlacCaract : string list * string -> int = fun(li, s) ->
  getPlacCaractSys(li, s, 0)
;;



let consStr : string list * string -> string list = fun(li, element) ->
  element :: li
;;



let consList : string list * string list -> string list = fun(li1, li2) ->
  li1@li2
;;

let consListList : string list list * string list -> string list list = fun(li1, li2) ->
  li2 :: li1
;;


(** recupere la liste apres le then voulu **)
let rec getThen : string list -> string list = fun(li) ->
  if(getCaractList(li, 0) = "then")then
    getModList(li, 1)
  else 
    getThen(getModList(li, 1))
;;


(** (int)string (fonction de base trop longue a ecrire) **)

let sti : string -> int = fun(nb) ->
  int_of_string(nb)
;;



(** (string)int **)
let its : int -> string = fun(nb) ->
  string_of_int(nb)
;;



(** recupere le nb de parenthese fermee (systeme) **)

let rec getNumParSys : string list * int -> int = fun(li, a) ->
  if(getCaractList(li, 0) = "(")then
    getNumParSys(getModList(li, 1), a+1)
  else if(getCaractList(li, 0) = ")")then
    a
  else
    getNumParSys(getModList(li, 1), a)
;;



(** appelle de getNumParSys(li, a) **)

let getNumPar : string list -> int = fun(li) ->
  getNumParSys(li, 0)
;;


(** recupere l'index de l'emplacement de ")" (systeme) **)

let rec getPlacParSys : string list * int * int * int -> int = fun(li, a, b, c) ->
  if(getCaractList(li, 0) = ")")then
    if((a+1) = b)then
      c+1
    else 
       getPlacParSys(getModList(li, 1), a+1, b, c+1)
  else 
    getPlacParSys(getModList(li, 1), a, b, c+1)
;;


(** appelle de getPlacParSys(li, a, b, c) **)

let getPlacPar : string list * int -> int = fun(li, b) ->
  getPlacParSys(li, 0, b, 0)
;;

let delSpace : string * string -> bool = fun(sOrigine, sCompar) ->
  delSpaceSys(sOrigine, sCompar, 0, 0)
;;

let rec delSpaceSys : string * string * int * int -> bool = fun(s, s1, i, j) ->
  if(String.length(s) >= String.length(s1))then
    if(j = String.length(s1))then
      if(i = String.length(s))then
	true
      else if(s.[i] = ' ')then
	delSpaceSys(s, s1, i+1, j)
      else 
	false
    else if(j > 0)then
      if(s.[i] = s1.[j])then
	delSpaceSys(s, s1, i+1, j+1)
      else 
	false
    else 
      if(s.[i] = s1.[j])then
	delSpaceSys(s, s1, i+1, j+1)
      else if(s.[i] = ' ')then
	delSpaceSys(s, s1, i+1, j)
      else 
	false
  else
    false

;;


(***********************************************************)
(***********************************************************)










(** Recupere le x+1eme element (fonctionne comme tableau) **) 

let rec getCaractList : string list * int -> string = fun(li, a) ->
  if(a > 0)then
    String.trim(getCaractList(tl(li), a-1))
  else 
    hd(li)
;;


(** Recupere les elements de la liste a partir de l'element a **)

let rec getModList : string list * int -> string list = fun(li, a) ->
  if(a > 0)then
    getModList(tl(li), a-1)
  else 
    li
;;



(** Retourne bool pour savoir si exact 
 ** Modifie pour qu'il trouve la parenthese si y a un if devant **)

let rec getBool : string list -> bool = fun(li) ->
  if(delSpace(getCaractList(li, 0), "("))then
    if(delSpace(getCaractList(li, 2), "<"))then
      if(int_of_string(String.trim(getCaractList(li, 1))) < int_of_string(String.trim(getCaractList(li, 3))))then
	true
      else  
	false
    else  if(delSpace(getCaractList(li, 2), ">"))then
      if(int_of_string(String.trim(getCaractList(li, 1))) > int_of_string(String.trim(getCaractList(li, 3))))then
	true
      else  
	false
    else if(delSpace(getCaractList(li, 10), "<"))then
      if(sti(setCalcul(getModList(li, 2))) < sti((setCalcul(getModList(li, 12)))))then
	true
      else 
	false
    else if(delSpace(getCaractList(li, 10), ">"))then
      if(sti(setCalcul(getModList(li, 2))) > sti(setCalcul(getModList(li, 12))))then
	true
      else 
	false
    else 
      false
  else 
    getBool(getModList(li, 1))
;;


(** retourne le la suite de la liste apres l'element else voulu (systeme) **)

let rec getElseSystem : string list * int -> string list = fun(li, a) ->
  if(delSpace(getCaractList(li, 0), "if")) then
    getElseSystem(getModList(li, 1), a+1)
      
  else if(delSpace(getCaractList(li, 0), "else"))then
    if(a = 1)then
      getModList(li, 1)
    else 
      getElseSystem(getModList(li, 1), a-1)
  else 
    getElseSystem(getModList(li, 1), a)
;; 


(** appelle de la fonction getElseSystem(li, a) **)
let getElse : string list -> string list = fun(li) ->
  getElseSystem(li, 0)
;;



(** recupere la suite de la liste suivant le resultat de l'operation **)

let getOpResult : string list -> string list = fun(li) ->
  if(getBool(li) = true)then
    getThen(li)
  else 
    getElse(li)
;;



(** return du resultat **)

let rec getResult : string list -> string = fun(li) ->
  if(delSpace(getCaractList(getOpResult(li),0), "if"))then
    getResult(getOpResult(li))
  else if(delSpace(getCaractList(getOpResult(li), 0), "return"))then
    getCaractList(getOpResult(li), 1)
  else 
    getResult(getModList(li, 1))
;;


(** calcule les a et b avec op l'operation **)

let rec setCalculSys : int * int * string -> int = fun(a, b, op) ->
  if(op = "*")then
    a * b
  else if(op = "/")then
    a / b
  else if(op = "+")then
    a + b
  else
    a - b
;;


(** systeme du caclul ***)

let rec setCalcul : string list -> string = fun(li) ->
  
  if(getNumPar(li) = 1)then
    if(String.trim(getCaractList(li, 2))="*" || String.trim(getCaractList(li, 2))="/" || String.trim(getCaractList(li, 2))="+" || String.trim(getCaractList(li, 2))="-" )then
      its(setCalculSys(sti(String.trim(getCaractList(li, 1))), sti(String.trim(getCaractList(li, 3))), String.trim(getCaractList(li, 2)))) 
    else
      "Operateur invalide (operateur possible: +, -, *, /)"
  else
    if(getPlacPar(li, 1) = 6)then
      its(setCalculSys(setCalculSys(sti(String.trim(getCaractList(li, 2))), sti(String.trim(getCaractList(li, 4))), String.trim(getCaractList(li, 3))), sti(String.trim(getCaractList(li, getPlacPar(li, 1) + 1))), String.trim(getCaractList(li, getPlacPar(li, 1)))))
    else 
       its(setCalculSys(sti(String.trim(getCaractList(li, getPlacPar(li, 1) - 7))), setCalculSys(sti(String.trim(getCaractList(li, getPlacPar(li, 1) - 4))), sti(String.trim(getCaractList(li, getPlacPar(li, 1) - 2))), String.trim(getCaractList(li, getPlacPar(li, 1) - 3))), String.trim(getCaractList(li, 
getPlacPar(li, 1)-6))))
;;



let rec getFuncSys : string list * int * string list list *string list -> string list list = fun(li, a, lili, li2) ->
  if(length(li) > a)then
    if(getCaractList(li, 0) != "END")then
      getFuncSys(getModList(li, 1), a+1, lili, consStr(li2, getCaractList(li, 0)))
    else 
      getFuncSys(getModList(li, 1), a+1, consListList(lili, li), [])
  else 
    lili
;;

let getFunc : string list -> string list list = fun(li) ->
  getFuncSys(li, 0, [[""]], [])
;;
