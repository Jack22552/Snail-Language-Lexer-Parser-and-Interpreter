// 139ba267302516bcc9e9e23141d94ac04cd56a91
// All the code we need for evaluating snail programs
open Ast;

// SNAIL RUNTIME SEMANTICS OPERATIONAL SEMANTICS

type location = int 


type value = 
    | Void
    | Bool(bool)
    | String(int, string)
    | Int(int) // FIXME THIS WONT WORK
    | Array(int, Array.t(value))
    | Dyn(string, list((string, location)))


module OrderedLocation = {
    type t = location;
    let compare = compare;
};

// Map module for Store
module LocationMap = Map.Make(OrderedLocation);
// Map module for Envirnonment
module StringMap = Map.Make(String);

type environment = StringMap.t(location);
type store = LocationMap.t(value);

// helper variable bindings
let emptyEnv: environment = StringMap.empty;
let emptyStore: store = LocationMap.empty;

// tracking of locations 
let loc_val : ref(int) = ref(0);
// function to get a new location
// assumption every location is unique
let newloc = () => {
    let l = loc_val^;
    // increment the location value
    incr(loc_val);
    l;
};

// function to collect all member variables of a given type so that we can construct a new objectt
let rec get_members = (prog: program, typ: string) : list(member) => {
    // base case (all built in types have no members)
    // look up the class type in our AST (is this a base case?)
    switch(List.assoc_opt(typ, prog)) {
        | Some(cls) => {
            // recursive case
            // recurse on our parent then append our members to the returned list
            let (_name, inherits, members, _methods) = cls;
            let parent_members = switch(inherits) {
                | Some(parent_typ) => get_members(prog, parent_typ)
                | None => get_members(prog, "Object")
            };
             parent_members @ members;
        }
        | None => {
            // Not found --- check if this is a base case
            switch(typ) {
                | "Array"
                | "Bool"
                | "Int"
                | "IO"
                | "Object"
                | "String" => {
                    // Base Case -> return an empty list of members
                    []
                }
                | _ => {
                    // error handling
                    failwith(Printf.sprintf("Unknown class%s", typ))
                }
            }
        }
    }
}

// looking up a method given an AST and a type name
// method is a tuple: name, list of params, body

let rec find_method = (prog: program, typ: string, method: string) : method => {
    // look up in our AST the class type that we are working with
    switch(List.assoc_opt(typ, prog)) {
        | Some(cls) => {
            // we found the class in the AST
            let (_name, inherits, _members, methods) = cls;
            // see if we can find our target method in the methods list
            // use List.find_opt to search for the correct method
            switch(List.find_opt((m: method) => {
                // extract the method name from the method tuple
                let (nm, _, _) = m;
                let (name, _) = nm;
                // check if the name is the same as the one we're looking for
                name == method;
                }, methods)) {
                | Some(method) => {
                    // we found our target method (return it)
                    method
                }
                | None => {
                    // didn't find it (recurse on parent class)
                    switch(inherits) {
                        | Some(parent) => find_method(prog, parent, method)
                        // inherit from Object by default
                        | None => find_method(prog, "Object", method)
                    }
                }
            }
        }
        | None => {
            // the class was not found in the AST
            // it could be a built in type
            switch(typ) {
                | "Array" => {
                    switch(method) {
                       | "length" => {
                        // return a method tuple for this method
                        // method is a tuple: name, list of prams, body
                        // The body is a snail expression but there is no snail code for our built-in methods
                        (("length",(0,0)), [], (Internal("Array.length"), (0,0)))
                       }
                       | _ => {
                        // unknown method (recurse)
                        find_method(prog, "Object", method);
                       }
                    }
                }
                | "Bool"
                | "Int" => {
                    find_method(prog, "Object", method)
                }
                | "IO" => {
                    switch(method) {
                        | "print_string" => (("print_string", (0, 0)), [(String, 0)], (Internal("IO.print_string"), (0, 0)))
                        | "print_int" => (("print_int", (0, 0)), [(Int, 0)], (Internal("IO.print_int"), (0, 0)))
                        | "read_string" => (("read_string", (0, 0)), [], (Internal("IO.read_string"), (0, 0)))
                        | "read_int" => (("read_int", (0, 0)), [], (Internal("IO.read_int"), (0, 0)))
                        | _ => find_method(prog, "Object", method)
                    }
                }
                | "String" => {
                    switch(method) {
                        | "concat" => (("concat", (0, 0)), [(String, 0)], (Internal("String.concat"), (0, 0)))
                        | "length" => (("length", (0, 0)), [], (Internal("String.length"), (0, 0)))
                        | "substr" => (("substr", (0, 0)), [(Int, 0), (Int, 0)], (Internal("String.substr"), (0, 0)))
                        | _ => find_method(prog, "Object", method)
                    }
                }
                | "Object" => {
                    switch(method) {
                        | "abort" => (("abort", (0, 0)), [], (Internal("Object.abort"), (0, 0)))
                        | "get_type" => (("get_type", (0, 0)), [], (Internal("Object.get_type"), (0, 0)))
                        | "is_a" => (("is_a", (0, 0)), [(String, 0)], (Internal("Object.is_a"), (0, 0)))
                        | _ => failwith(Printf.sprintf("Unknown method %s for class %s", method, typ))
                    }
                }
                | _ => {
                    // unknown class type
                    // have a runtime excpetion
                    failwith(Printf.sprintf("Unknown class %s", typ))
                }
            }
        }
    }
};

// expression evaluation code
let rec evaluate_expression = ((prog: program),
                                (so: value),
                                (e: StringMap.t(location)),
                                (s: LocationMap.t(value)),
                                (exp: expression)) : (value, environment, store)=> {
    // expression = (expr_val, loc)
    // decompose
    let (expval, expl) = exp;

    // swtich on the expval type
    switch (expval) {
        | DynamicDispatch(target, method, arguments) => {
            let (method_name, method_loc) = method;
            ignore(failwith(Printf.sprintf("Unhandled dispatch to method %s\n", method_name)));
            exit(0);
        }

        | New(cls_ident) => {
            // decompose type identifier
            let (t, _location) = cls_ident;
            switch(t) {
                | "Bool" => (Bool(false), e, s)
                | "Int" => (Int(0), e, s)
                | "String" => (String(0, ""), e, s)
                | _ => {
                    // get all the class members
                    let class_members: list(member) = get_members(prog, t);
                    // map each member to create a new location
                    let locs : list(location) = List.map((_m) => newloc(), class_members);
                    let v_1 : value = Dyn(t, List.map2((mem, l) => {
                        let name = switch(mem) {
                            | NoInitMember((n, _l))
                            | InitMmember((n, _l), _) => n
                        };
                        (name, l)
                    }, class_members, locs));
                    // update store (for each location, add to the store Void)
                    let s_1 = List.fold_left((acc, l) => {
                        // add l => void 
                        LocationMap.add(l, Void, acc);
                    }, s, locs);

                    // create environment for the initialization
                    let e_1 = List.fold_left2((acc, member, loc) => {
                        let name : string = switch(member) {
                            | NoInitMember((n, _l))
                            | InitMmember((n, _l), _) => n
                        };
                        StringMap.add(name, loc, acc);
                    }, emptyEnv, class_members, locs);

                    // backwards (need to reverse this)
                    let block_init = List.fold_left((acc, member) => {
                        switch(member) {
                            | NoInitMember(_) => acc
                            | InitMmember(i, init_expr) => {
                                // build up a block for the assignment
                                let blk : expression = (Block([init_expr]), expl);
                                // make an assignment
                                let assn : expression = (
                                    Assign(i, blk),
                                    expl
                                );
                                // prepend assignment
                                [assn, ...acc];
                            }
                        }  
                    }, [], class_members);

                    let blk : expression = (Block(List.rev(block_init)), expl);

                    let (v_2, e_2, s_2) = evaluate_expression(prog, v_1, e_1, s_1, blk);

                    (v_1, e, s_2);
                } 
            }
        }

        // Assignment case
        | Assign((id, _), expr) => {
            let (v1, e1, s1) = evaluate_expression(prog, so, e, s, expr);
            let l1 = StringMap.find(id, e1) 
              |> Option.get_or_else((_) => failwith("Unbound identifier"));
            let s2 = LocationMap.add(l1, v1, s1);
            (v1, e1, s2);
        }

        | Block(exprs) => {
        // Initialize the current environment and store
        let rec eval_block = (exprs, current_env, current_store) => {
            switch(exprs) {
                // If there are no more expressions, return Void
                | [] => (Void, current_env, current_store)
                // Evaluate the first expression and recursively evaluate the rest
                | [first_expr, ...rest_exprs] => {
                    let (value, new_env, new_store) = evaluate_expression(prog, so, current_env, current_store, first_expr);
                    eval_block(rest_exprs, new_env, new_store);
                }
            }
        };
        // Start evaluating the block with the current environment and store
        eval_block(exprs, e, s);
        }

        | If(cond_expr, then_expr, else_expr) => {
            let (cond_val, e1, s1) = evaluate_expression(prog, so, e, s, cond_expr);
                switch (cond_val) {
                    | Bool(true) => evaluate_expression(prog, so, e1, s1, then_expr)
                    | Bool(false) => evaluate_expression(prog, so, e1, s1, else_expr)
                    | _ => failwith("Condition expression did not evaluate to a boolean")
                }
        }

        | While(cond_expr, body_expr) => {
            let rec loop = (e, s) => {
            let (cond_val, e1, s1) = evaluate_expression(prog, so, e, s, cond_expr);
            switch (cond_val) {
                | Bool(true) => {
                    let (_, e2, s2) = evaluate_expression(prog, so, e1, s1, body_expr);
                    loop(e2, s2)
                }
                | Bool(false) => (Void, e1, s1)
                | _ => failwith("Condition expression did not evaluate to a boolean")
            }
            };
            loop(e, s)
        }

        // Array assignment case
        | ArrayAssign((arr_expr, _), (index_expr, _), rhs_expr) => {
            let (v1, e1, s1) = evaluate_expression(prog, so, e, s, rhs_expr);
            let (arr_val, e2, s2) = evaluate_expression(prog, so, e1, s1, arr_expr);
            let (index_val, e3, s3) = evaluate_expression(prog, so, e2, s2, index_expr);
            switch (arr_val, index_val) {
                | (Array(size, arr), Int(i)) when 0 <= i && i < size => {
                    Array.set(arr, i, v1);
                    (v1, e3, s3);
                }
                | (_, _) => failwith("Array assignment failed: invalid array or index")
            };
        }

        | Bool(true) => (Bool(true), e, s);
        | Bool(false) => (Bool(false), e, s);

        // Evaluating an identifier
        | Ident(id) => {
            let loc = StringMap.find_opt(id, e)
              |> Option.get_or_else((_) => failwith("Unbound identifier"));
            let value = LocationMap.find_opt(loc, s)
              |> Option.get_or_else((_) => failwith("Uninitialized variable"));
            (value, e, s);
        }

        | NewArray((size_expr, _)) => {
            // Evaluate the size expression
            let (size_val, e1, s1) = evaluate_expression(prog, so, e, s, size_expr);

            // Check if the size is a valid integer
            switch(size_val) {
                | Int(n) when n >= 0 => {
                    // Create a new array of the specified size
                    let arr = Array.make(n, Void);

                    // Create a new location for the array
                    let loc = newloc();

                    // Add the array to the store
                    let s2 = LocationMap.add(loc, Array(n, arr), s1);

                    // Return the array and the updated store
                    (Array(n, arr), e1, s2);
                }
                | _ => failwith("Array size must be a non-negative integer")
            }
        }

        // Evaluating an array access
        | ArrayAccess(arr_expr, index_expr) => {
            let (arr_val, e1, s1) = evaluate_expression(prog, so, e, s, arr_expr);
            let (index_val, e2, s2) = evaluate_expression(prog, so, e1, s1, index_expr);
            switch (arr_val, index_val) {
                | (Array(size, arr), Int(i)) when 0 <= i && i < size => {
                    let element = Array.get(arr, i);
                    (element, e2, s2);
                }
                | _ => failwith("Array access failed: invalid array or index")
            };
        }

        | Self => (so, e, s);
        // Existing cases (e.g., Assignment, DynamicDispatch, etc.)
        | Assign((id, _), expr) => {
            let (v1, e1, s1) = evaluate_expression(prog, so, e, s, expr);
            let l1 = StringMap.find(id, e1) 
              |> Option.get_or_else((_) => failwith("Unbound identifier"));
            let s2 = LocationMap.add(l1, v1, s1);
            (v1, e1, s2);
        }

        | Let((id, _), init_expr, body_expr) => {
            // Evaluate the initialization expression
            let (init_val, e1, s1) = evaluate_expression(prog, so, e, s, init_expr);
            // Allocate a new location for the variable
            let loc = newloc();
            // Update the environment with the new variable
            let e2 = StringMap.add(id, loc, e1);
            // Update the store with the initial value at the new location
            let s2 = LocationMap.add(loc, init_val, s1);
            // Evaluate the body expression with the updated environment and store
            evaluate_expression(prog, so, e2, s2, body_expr);
        }

        | IsVoid(expr) => {
            // Evaluate the inner expression
            let (v1, e1, s1) = evaluate_expression(prog, so, e, s, expr);
            // Check if the result is Void
            switch v1 {
                | Void => (Bool(true), e1, s1)
                | _ => (Bool(false), e1, s1)
            }
        }

        | Negate(expr) => {
            let (v1, e1, s1) = evaluate_expression(prog, so, e, s, expr);
            switch v1 {
                | Int(n) => (Int(-n), e1, s1)
                | _ => failwith("Negate operation requires an integer operand")
            }
        }

        | Add(lhs, rhs) => {
            let (v1, e1, s1) = evaluate_expression(prog, so, e, s, lhs);
            let (v2, e2, s2) = evaluate_expression(prog, so, e1, s1, rhs);
            switch (v1, v2) {
                | (Int(n1), Int(n2)) => (Int(n1 + n2), e2, s2)
                | _ => failwith("Addition requires integer operands")
            }
        }

        | Subtract(lhs, rhs) => {
            let (v1, e1, s1) = evaluate_expression(prog, so, e, s, lhs);
            let (v2, e2, s2) = evaluate_expression(prog, so, e1, s1, rhs);
            switch (v1, v2) {
                | (Int(n1), Int(n2)) => (Int(n1 - n2), e2, s2)
                | _ => failwith("Subtraction requires integer operands")
            }
        }

        | Multiply(lhs, rhs) => {
            let (v1, e1, s1) = evaluate_expression(prog, so, e, s, lhs);
            let (v2, e2, s2) = evaluate_expression(prog, so, e1, s1, rhs);
            switch (v1, v2) {
                | (Int(n1), Int(n2)) => (Int(n1 * n2), e2, s2)
                | _ => failwith("Multiplication requires integer operands")
            }
        } 

        | Divide(lhs, rhs) => {
            let (v1, e1, s1) = evaluate_expression(prog, so, e, s, lhs);
            let (v2, e2, s2) = evaluate_expression(prog, so, e1, s1, rhs);
            switch (v1, v2) {
            | (Int(_), Int(0)) => failwith("Division by zero error")
            | (Int(n1), Int(n2)) => (Int(n1 / n2), e2, s2)
            | _ => failwith("Division requires integer operands")
            }
        } 

        | StaticDispatch(target, (static_type, _), (method_name, _), arguments) => {
            // Step 1: Evaluate the target expression to get the receiver object
            let (receiver_val, e1, s1) = evaluate_expression(prog, so, e, s, target);
            switch(receiver_val) {
            | Dyn(receiver_type, _) => {
                // Step 2: Ensure the receiver type is a subtype of static_type (type check at runtime)
                let _ = if not (is_subtype(prog, receiver_type, static_type)) {
                    failwith(Printf.sprintf("Static dispatch type mismatch: %s is not a subtype of %s" receiver_type, static_type));
                };

                // Step 3: Find the method in the static type's definition
                let method_def = find_method(prog, static_type, method_name);

                // Step 4: Evaluate arguments and prepare method call
                let (args_values, env_after_args, store_after_args) = 
                    eval_arguments(prog, so, e1, s1, arguments);

                // Step 5: Execute the method
                execute_method(prog, static_type, method_def, receiver_val, args_values, env_after_args, store_after_args);
            }
            | _ => failwith("Static dispatch requires a class object")
            };
        };

        | DynamicDispatch(target, (method_name, _), arguments) => {
            // Step 1: Evaluate the target expression to get the receiver object
            let (receiver_val, e1, s1) = evaluate_expression(prog, so, e, s, target);

            // Step 2: Ensure the receiver is a class object
            switch(receiver_val) {
                | Dyn(receiver_type, _) => {
                    // Step 3: Find the method in the dynamic type's definition
                    let method_def = find_method(prog, receiver_type, method_name);

                    // Step 4: Evaluate arguments and prepare method call
                    let (args_values, env_after_args, store_after_args) = 
                        eval_arguments(prog, so, e1, s1, arguments);

                    // Step 5: Execute the method
                    execute_method(prog, receiver_type, method_def, receiver_val, args_values, env_after_args, store_after_args);
                }
                | _ => failwith("Dynamic dispatch requires a class object")
            };
        };

        | SelfDispatch((method_name, _), arguments) => {
            // Step 1: Ensure we are in a valid object context
            switch(so) {
                | Dyn(current_type, _) => {
                    // Step 2: Find the method in the current type's definition
                    let method_def = find_method(prog, current_type, method_name);

                    // Step 3: Evaluate arguments and prepare method call
                    let (args_values, env_after_args, store_after_args) = 
                        eval_arguments(prog, so, e, s, arguments);

                    // Step 4: Execute the method
                    execute_method(prog, current_type, method_def, so, args_values, env_after_args, store_after_args);
                }
            | _ => failwith("Self dispatch requires an object context")
            };
        };

        | Internal(internal_name) => {
            // FILL OUT THIS SWITCH WITH EVERY INTERNAL METHOD (WRITE CODE THAT DOES THE PROCESSING FOR US)
            switch(internal_name) {
                | "Array.length" => {
                    // implement Array.length functionality
                    // access our self object
                    switch(so){
                        | Array(sz, arr) => {
                            // Return a value with the size
                            // no updates to the environment or store
                            (Int(sz), e, s);

                        }
                        | _ => {
                            // error
                            failwith("Tried to get array length for a non-array")
                        }
                    }
                }
                | "IO.print_string" => {
                    switch(so) {
                        | String(_, s) => {
                            print_endline(s);
                            (Void, e, s);
                        }
                        | _ => failwith("Tried to print a non-string value")
                    }
                }

                | "IO.print_int" => {
                    switch(so) {
                        | Int(i) => {
                            print_endline(string_of_int(i));
                            (Void, e, s);
                        }
                        | _ => failwith("Tried to print a non-integer value")
                    }
                }

                | "IO.read_string" => {
                    let input = input_line(stdin);
                    (String(0, input), e, s);
                }

                | "IO.read_int" => {
                    let input = input_line(stdin);
                    let parsed = try { Some(int_of_string(input)) } with _ => None;
                    switch (parsed) {
                        | Some(i) => (Int(i), e, s)
                        | None => (Int(0), e, s)
                    }
                }

                | "String.concat" => {
                    switch(so) {
                        | String(_, s1) => {
                            let (s2, _loc) = List.hd(arguments);
                            (String(0, s1 ^ s2), e, s);
                        }
                        | _ => failwith("Tried to concatenate a non-string value")
                    }
                }

                | "String.length" => {
                    switch(so) {
                        | String(_, s) => (Int(String.length(s)), e, s)
                        | _ => failwith("Tried to get length of a non-string value")
                    }
                }

                | "String.substr" => {
                    switch(so) {
                        | String(_, s) => {
                            let (start, len) = arguments;
                            (String(0, String.sub(s, start, len)), e, s);
                        }
                        | _ => failwith("Tried to get substring of a non-string value")
                    }
                }
                |_ => {
                    failwith(Printf.sprintf("Unimplemented internal method %s", internal_name))
                }
            }
        }
        | _ => failwith("Unhandled expression type")
    };
};