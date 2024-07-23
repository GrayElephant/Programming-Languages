fun parse ("("::xs) = 
    let
        fun get_list (_, 0) = []
            | get_list ("("::xs, diff) = "("::get_list(xs, diff+1)
            | get_list (")"::xs, diff) = ")"::get_list(xs, diff-1)
            | get_list (x::xs, diff) = x::get_list(xs, diff);

        fun skip_list (ls, 0) = ls
            | skip_list (")"::xs, diff) = skip_list(xs, diff-1)
            | skip_list("("::xs, diff) = skip_list(xs, diff+1)
            | skip_list(x::xs, diff) = skip_list(xs, diff);
        
        val ls = get_list(xs, 1);
        val rest = skip_list(xs, 1);
    in
        case rest of
            [] => parse(ls)
            | _ => CONS(parse(ls), parse(rest))
    end
    | parse [")"] = ATOM NIL
    | parse [x] = ATOM(SYMBOL x)
    | parse (x::xs) = CONS(parse([x]), parse(xs));