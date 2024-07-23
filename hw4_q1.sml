datatype Atom =
   NIL | SYMBOL of string

datatype SExp =
   ATOM of Atom | CONS of (SExp * SExp)

exception Undefined;
exception Empty;

fun initEnv () : (string -> SExp) = fn key => raise Undefined;

fun define (name: string) old_env value = fn key => if key = name then value else old_env key;

fun emptyNestedEnv () = [initEnv ()];

fun pushEnv env env_stack = env :: env_stack;

fun popEnv [] = raise Empty
    | popEnv (env::env_stack_tail) = env_stack_tail;

fun topEnv [] = raise Empty
    | topEnv (env::env_stack_tail) = env;

fun defineNested name env_stack value =
    let
        val env = topEnv env_stack
        val env_stack_tail = popEnv env_stack
        val updated_env = define name env value
    in
        pushEnv updated_env env_stack_tail
    end;

fun find (name: string) [] = raise Undefined
    | find (name: string) (env::env_stack_tail) = env name handle Undefined => find name env_stack_tail;