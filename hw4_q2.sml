use "hw4_q1.sml";
use "./hw4_parser.sml";

exception LispError;
Control.Print.printLength := 100;
Control.Print.printDepth := 100;


(* Helper function - feel free to delete *)
fun first (x, _) = x;
fun tokenize x = 
        String.tokens (fn c: char => c = #" ") 
            (String.translate (fn #"(" => "( " | #")" => " )" | c => str c) x);

fun print_atom (SYMBOL s) = s
  | print_atom NIL = "nil"

fun ends_with_nil (CONS (car, cdr)) = ends_with_nil cdr
  | ends_with_nil (ATOM NIL) = true
  | ends_with_nil _ = false

fun sexp_to_string (ATOM a) = print_atom a
  | sexp_to_string (CONS (car, cdr)) = "(" ^ print_cons (car, cdr) ^ ")"

and print_cons (car, ATOM NIL) = sexp_to_string car
  | print_cons (car, CONS (cadr, cddr)) = (case ends_with_nil (CONS (cadr, cddr)) of
      true => sexp_to_string car ^ " " ^ print_cons (cadr, cddr)
    | false => sexp_to_string car ^ " . " ^ sexp_to_string (CONS (cadr, cddr)))
  | print_cons (car, cdr) = sexp_to_string car ^ " . " ^ sexp_to_string cdr;


local
    (*fun tokenize x = 
        String.tokens (fn c: char => c = #" ") 
            (String.translate (fn #"(" => "( " | #")" => " )" | c => str c) x);*)

    (* Helper functions - feel free to delete *)
    (* ====================================== *)
    fun is_digit c = c >= #"0" andalso c <= #"9";

    fun is_number str =
        let
            fun check [] = true
            | check (c::cs) = is_digit c andalso check cs
            
            val chars = String.explode str
        in
            if List.null chars then false else check chars
        end;
        
    fun char_to_int c = ord(c) - ord(#"0")

    fun string_to_int str =
        let
            fun convert [] acc = acc
            | convert (c::cs) acc = convert cs (10 * acc + char_to_int c)
        in
            convert (String.explode str) 0
        end;

    fun sexp_to_int sexp =
        case sexp of
            ATOM (SYMBOL s) => string_to_int s
          | _ => raise LispError;

    fun is_atom (ATOM NIL) = true
        | is_atom (ATOM (SYMBOL s)) = true
        | is_atom _ = false;

        
    fun eval_aux (CONS (ATOM (SYMBOL "cons"), CONS (sexp1, CONS (sexp2, ATOM NIL))), env) = 
        let
            val (first_exp, env1) = eval_aux (sexp1, env)
            val (second_exp, env2) = eval_aux (sexp2, env1)
            val exp = CONS (first_exp, second_exp)
        in
            (exp, env2)
        end
        | eval_aux (CONS (ATOM (SYMBOL "car"), CONS(sexp, ATOM NIL)), env) =
            let
                val (evaluated_sexp, env1) = eval_aux (sexp, env)
            in
                case evaluated_sexp of
                    (CONS (sexp1, sexp2)) => (sexp1, env1)
                    | _ => (ATOM (SYMBOL "lisp-error"), env)
            end
        | eval_aux (CONS (ATOM (SYMBOL "cdr"), CONS (sexp, ATOM NIL)), env) =
            let
                val (evaluated_sexp, env1) = eval_aux (sexp, env)
            in
                case evaluated_sexp of
                    (CONS (sexp1, sexp2)) => (sexp2, env1)
                    | _ => (ATOM (SYMBOL "lisp-error"), env)
            end
        | eval_aux (CONS (ATOM (SYMBOL "quote"), CONS (sexp, ATOM NIL)), env) = (sexp, env)
        | eval_aux (CONS (ATOM (SYMBOL "atom"), CONS (sexp, ATOM NIL)), env) =
            let
                val (evaluated_sexp, env1) = eval_aux (sexp, env)
            in
                case evaluated_sexp of
                    (ATOM (SYMBOL s)) => (ATOM (SYMBOL "t"), env1)
                    | _ => (ATOM NIL, env1)
            end
        | eval_aux (CONS (ATOM (SYMBOL "null"), CONS (sexp, ATOM NIL)), env) =
            let
                val (evaluated_sexp, env1) = eval_aux (sexp, env)
            in
                case evaluated_sexp of
                    (ATOM NIL) => (ATOM (SYMBOL "t"), env1)
                    | _ => (ATOM NIL, env1)
            end
        | eval_aux (CONS (ATOM (SYMBOL "eq"), CONS (sexp1, CONS(sexp2, ATOM NIL))), env) =
            let
                val (eval_sexp1, env1) = eval_aux (sexp1, env)
                val (eval_sexp2, env2) = eval_aux (sexp2, env1)
                val both_equal = if (eval_sexp1 = eval_sexp2) then true else false
                val both_atom = (is_atom eval_sexp1 andalso is_atom eval_sexp2)
            in
                if both_equal andalso both_atom then
                    (ATOM (SYMBOL "t"), env2)
                else
                    (ATOM NIL, env2)
            end
        | eval_aux (CONS (ATOM (SYMBOL "cond"), sexp), env) =
            let
                fun eval_cond_list (CONS (cond CONS (exp, ATOM NIL)), cond_list) = 

                fun eval_pair (COND (cond, COND (exp, ATOM NIL)), env) =
                    let
                        val (eval_cond, env1) = eval_aux (cond, env)
                        val is_cond_true = case eval_cond of
                            (ATOM (SYMBOL "t")) => true
                            | _ => false
                    in
                        (is_cond_true, sexp)
                    end
            in

            end
        (*| eval_aux (CONS (ATOM (SYMBOL "cond"), CONS (CONS (cond, exp), CONS (sexp_rest, ATOM NIL)))) =*)
        | eval_aux (CONS (ATOM (SYMBOL s), ATOM NIL), env) = eval_aux (ATOM (SYMBOL s), env)
        | eval_aux ((ATOM (SYMBOL s)), env) =
            let
                val result = 
                    if is_number s orelse s = "nil" then 
                        (ATOM (SYMBOL s), env)
                    else 
                        (find s env, env) handle Undefined => (ATOM (SYMBOL "lisp-error"), env)
            in
                result
            end
        | eval_aux (_, env) = (ATOM (SYMBOL "lisp-error"), env);
    (*
        | eval_aux ((CONS ("cons", (CONS (sexp1, sexp2)))), env) = (CONS (first (eval_aux (sexp1, env)), first (eval_aux (sexp2, env))), env)
        | eval_aux ((ATOM (SYMBOL s)), env) =
            if is_number s 
            then 
                (ATOM (SYMBOL s), env) 
            else 
                (find s env, env) handle Undefined => (ATOM (SYMBOL "lisp-error"), env);*)
    (* ====================================== *)

in
    fun eval string_exp env =
        let
            val sexp = parse (tokenize string_exp)
        in
            eval_aux (sexp, env)
        end
    (* TODO, should have `parse (tokenize string_exp)` somewhere *)
end;

val parse_something = parse (tokenize "(1 2 3)");
val env = emptyNestedEnv ();
val env = defineNested "x" env (ATOM (SYMBOL "2"));
val res = first (eval "(quote (1 2 3))" env);
first (eval "(eq 2 2)" env);
first (eval "(eq 2 3)" env);
first (eval "(eq (cons 1 2) (cons 1 2))" env);
parse (tokenize "(1 2 3 4)");