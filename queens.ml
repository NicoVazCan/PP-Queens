(*Practica competitiva queens*)

let attack (i1,j1) (i2,j2) = 
    (* la reina en la casilla (i1,j1) amenaza la casilla (i2,j2)? *)
    j1 = j2 ||
    abs(i2-i1) = abs(j2-j1);; (*Omitimos la comprobacion de si estan en la misma fila por ser*)
                              (*un caso imposible en el algoritmo que usa esta funcion.      *)
	
let rec compatible p l = 
    (* una reina en la casilla p estaría a salvo de todas las de la lista l? *)
    not (List.exists (attack p) l);; 

let simplify sol n1 =
  let rec rev_espejo lo lr le = match lo with
    [] -> [lr;le]
    |
    (_,h)::t -> rev_espejo t (h::lr) ((n1-h)::le) (*El camino ya esta ordenado descendientemente por filas,      *)
  in rev_espejo sol [] [];; (*por lo que solo hace falta revertirlo y hacerle el espejo horizontal de cada reina.*)

let rec eqlast a = function (*Funcion para comparar la primera reina, es decir, la ultima cabeza de la lista*)
  [] -> false
  |
  h::[] -> h = a
  |
  _::t -> eqlast a t;;

let all_queens_sol n = 
  let mitad = n/2 and impar = n land 1 = 1 and n1 = n+1 in (*"Constantes" para no calcularlas cada iteracion. *)
  let centro = if(impar) then mitad+1 else mitad in        (*Cuando es impar, el centro sera la mitad mas uno.*)
  
    let rec search_all_from path (i,j) =
        if(i > n)
        then simplify path n1
        else 
        if(j > n || (i = 1 && j > centro) || (*Solo comprobamos hasta el centro de la primera fila,    *)
          (impar && i = n && j > mitad && eqlast (1,centro) path)) (*y solo la mitad de la ultima fila *)
        then [] (*cuando la primera reina fue colocada en el cento de la primera fila, ya que al hacer *)
        else    (*efecto espejo horizontal del tablero en cada solucion, obtenemos todas las restantes.*)
        let nextj = match path with 
          [] -> j+1 (*Cuando tenemos una reina encima dos columnas adelante, las tres siguientes celdas   *)
          |         (*obviamente no son compatiles, por lo que las saltamos directamente, ejemplo grafico:*)
          (_,x)::_ -> if(x-2 = j) then j+4 else j+1                                  (*[ ] [ ] [R] [ ] [ ]*)
        in                                                                           (*[?]--X---X---X->[?]*)
        if(compatible (i,j) path)
        then search_all_from ((i,j)::path) (i+1,1)
          @
          search_all_from path (i,nextj)
        else search_all_from path (i,nextj)
    in
    if(n < 1) then [[]]       (*Arreglo para los casos inferiores o iguales a uno, ya que en este caso*)
    else if(n = 1) then [[1]] (*hacer el espejo horizontal solo duplica la unica solucion optenida.   *)
    else search_all_from [] (1,1);; 
(*Es mejor arreglarlo de esta manera ya que solo se comprueba una vez, porque si evitaramos la duplicacion*)
(*explicitamente en estos casos, habria que comprobarlo en cada iteracion recursiva (mas lento).           *)