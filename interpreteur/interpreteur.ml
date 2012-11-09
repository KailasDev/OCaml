open List;;

let li = 
["if";
 "(";
 "(";
 "(";
 "7";
 "+";
 "2";
 ")";
 "*";
 "2";
 ")";
 ">";
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
 "3";
 ";";
 "else";
 "2";
 ";";
 "endif";
 ";";
 "else";
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
	   "return"; "("; "X"; "*"; "Y"; ")"; ";";
	   "END"; ";";
	   "Fonction"; "fois2"; "("; "int"; "X"; ","; "int"; "Y"; ")";
	   "return"; "("; "X"; "*"; "Y"; ")"; ";";
	   "END"; ";"];;


(** Recupere le x+1eme element (fonctionne comme tableau) **) 

let rec getCaractList : string list * int -> string = fun(li, a) ->
  if(a > 0)then
    getCaractList(tl(li), a-1)
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
  if(getCaractList(li, 0) = "(")then
    if(getCaractList(li, 2) = "<")then
      if(int_of_string(getCaractList(li, 1)) < int_of_string(getCaractList(li, 3)))then
	true
      else  
	false
    else  if(getCaractList(li, 2) = ">")then
      if(int_of_string(getCaractList(li, 1)) > int_of_string(getCaractList(li, 3)))then
	true
      else  
	false
    else if(getCaractList(li, 10) = "<")then
      if(sti(setCalcul(getModList(li, 2))) < sti(setCalcul(getModList(li, 12))))then
	true
      else 
	false
    else if(getCaractList(li, 10) = ">")then
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
  if(getCaractList(li, 0) = "if") then
    getElseSystem(getModList(li, 1), a+1)
      
  else if(getCaractList(li, 0) = "else")then
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


(** recupere la liste apres le then voulu **)
let rec getThen : string list -> string list = fun(li) ->
  if(getCaractList(li, 0) = "then")then
    getModList(li, 1)
  else 
    getThen(getModList(li, 1))
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
  if(getCaractList(getOpResult(li),0) = "if")then
    getResult(getOpResult(li))
  else 
    getCaractList(getOpResult(li), 0)
;;



(** (int)string (fonction de base trop longue a ecrire) **)

let sti : string -> int = fun(nb) ->
  int_of_string(nb)
;;



(** (string)int **)
let its : int -> string = fun(nb) ->
  string_of_int(nb)
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

let rec setCalcul : string list  -> string = fun(li) ->
  if(getNumPar(li) = 1)then
    if(getCaractList(li, 2)="*" || getCaractList(li, 2)="/" || getCaractList(li, 2)="+" || getCaractList(li, 2)="-" )then
      its(setCalculSys(sti(getCaractList(li, 1)), sti(getCaractList(li, 3)), getCaractList(li, 2))) 
    else
      "Operateur invalide (operateur possible: +, -, *, /)"
  else
    if(getPlacPar(li, 1) = 6)then
      its(setCalculSys(setCalculSys(sti(getCaractList(li, 2)), sti(getCaractList(li, 4)), getCaractList(li, 3)), sti(getCaractList(li, getPlacPar(li, 1) + 1)), getCaractList(li, getPlacPar(li, 1))))
    else 
       its(setCalculSys(sti(getCaractList(li, getPlacPar(li, 1) - 7)), setCalculSys(sti(getCaractList(li, getPlacPar(li, 1) - 4)), sti(getCaractList(li, getPlacPar(li, 1) - 2)), getCaractList(li, getPlacPar(li, 1) - 3)), getCaractList(li, getPlacPar(li, 1)-6)))
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


(**let getBetList : string list * int -> string list = fun(li, a) ->
  if(a>0)then

  else 

                                                    
;;**)


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

let consStr2 : string list list * string list -> string list list = fun(li, element) ->
  element :: li
;;


let consList : string list * string list -> string list = fun(li1, li2) ->
  li1@li2
;;

let rec orderList : string list * int * string list -> string list = fun(li, a, li2)->
  if( a = length(li))then
    li2
  else
    orderList(li, a+1,consStr(li2, getCaractList(li, a)))
;;


let rec getFunction : string list * string list -> string list = fun(li, li1) ->
  if(getCaractList(li, 0) = "END")then
    consStr(consStr(li1, "END"), ";")
  else
    getFunction(getModList(li, 1),consStr(li1, getCaractList(li, 0)))
;;


let rec getIndexSys : string list * string * int * int -> int = fun(li, a, i, j)->
  if(getCaractList(li,0) = a)then
    i
  else
     if(j-1!=0)then
       getIndexSys(getModList(li, 1), a, i+1, j-1)
     else
       i-1
;;

let getIndex : string list * string -> int = fun(li, a)->
  getIndexSys(li, a, 0, length(li))
;;


let rec getListFunction : string list * string list list -> string list list = fun(li, li2)->
  if((getCaractList(li, 0) = "Fonction") && ((getIndex(li, "END")+2) < length(li)))then
    getListFunction(getModList(li, getIndex(li, "END")+2) ,consStr2(li2, orderList(getFunction(li,[]), 0, [])))
  else if(getCaractList(li, 0) = "Fonction")then
    consStr2(li2, orderList(getFunction(li,[]), 0, []))
  else
    consStr2(li2, consStr([], getCaractList(li,0)))
;;

let rec getList : string list list * int -> string list = fun(li, a)->
  if(a > 0)then
    getList(tl(li), a-1)
  else 
    hd(li)
;;

let rec getFunctionListSys : string list list * string *int -> string list = fun(li, s, i)->
  if(i < 0  && (getCaractList(getList(li, i), 1) != s))then
    []
  else if(getCaractList(getList(li, i), 1) = s)then
    getList(li, i)
  else 
    getFunctionListSys(li, s, i-1)
;;

let getFunctionList : string list list * string -> string list = fun(li, s)->
  getFunctionListSys(li, s, length(li)-1)
;;
