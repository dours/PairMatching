module G =
  struct

    type t = ((int * int) * int) array * int * int

    let nBoys (_, x, _) = x
    let nGirls (_, _, x) = x

    let edges (x, _, _) = x
    
    let toString (ar, boys, girls) = 
	let buf = Buffer.create 1024 in
	Buffer.add_string buf (Printf.sprintf "%i %i\n" boys girls);
	Array.iter (fun ((boy, girl), w) -> Buffer.add_string buf (Printf.sprintf "  %i %i %i\n" boy girl w)) ar;
	Buffer.contents buf

    let fromFile name = 
	let buf = Scanf.Scanning.from_file name in
	let id x = x in
	let get = Scanf.bscanf buf in
	let n = get "%i" id in
	let m = get " %i\n" id in
	let edges = ref [] in
	try 
	while true do 
	    let boy = get " %i" id in
	    let girl = get " %i " id in
	    let w = get "%i\n" id in
	    edges := ((boy, girl), w) :: !edges;
	done
	with 
	  | _ -> ();
	;
	((Array.of_list (List.rev !edges)), n, m)
	

  end

let id x = x

module DetRandom = struct

    type t = { fileName : string; arrayLen : int; state : Random.State.t }
    
    let loadState fname = 
	let inp = open_in fname in
	let n = (Scanf.fscanf inp "%i\n" id) in
	let a = Array.init n (fun _ -> Scanf.fscanf inp "%i\n" id) in
	close_in inp;
	{fileName = fname; arrayLen = n; state = Random.State.make a }
	
    let setState { state = s } = Random.set_state s
    
    let saveState { fileName = fn; arrayLen = n; state = s } = 
	let outp = open_out fn in
	Printf.fprintf outp "%i\n" n;
	for i = 0 to n - 1 do 
	    Printf.fprintf outp "%i\n" (Random.State.bits s)
	done;
	close_out outp
	
end

module Matcher = PM.Make (G)
module Perebor = PM.MakeExhaustiveSearch (G)

let randomFullGraph nBoys nGirls st = 
    let bound = (max_int / 2) / (max nBoys nGirls) in
    (Array.init (nBoys * nGirls) (fun i -> ((i mod nBoys, i / nBoys), Random.State.int st bound)), nBoys, nGirls)
    
let randomGraph nBoys nGirls prob st = 
(*   !!!!!!!!!!!!! let bound = (max_int / 2) / (max nBoys nGirls) in    *)
    let bound = 50 in

    let rec foldNum f start = function
    | 0 -> start
    | n -> foldNum f (f start (n - 1)) (n - 1)
    in

    let elist = foldNum (fun l boy ->
	foldNum (fun l girl -> 
	    if (Random.State.float st 1.) < prob then 
		((boy, girl), Random.State.int st bound) :: l
	    else l
	) l nGirls)
     [] nBoys
    in
    (Array.of_list elist, nBoys, nGirls)
    

let failedOnce = ref false

let test graph = 
	Printf.fprintf stderr "%s\nINPUT\n" "-----------------------------------------------------------\n\n";
	Printf.fprintf stderr "%s" (G.toString graph);
	Printf.fprintf stderr "%s" "END INPUT\n";
	try
	  let ansFast = Matcher.search graph in
	  let ansFast = List.fold_left (fun s (_, w) -> s + w) 0 ansFast in
	  Printf.fprintf stderr "ansFast = %i\n" ansFast;
	  Printf.fprintf stderr "Starting exhaustive search!\n";
	  flush stderr;
	  let ansSlow = Perebor.search graph in
	  Printf.fprintf stderr "ansSlow = %i\n" ansSlow;
	  if ansFast != ansSlow then raise (Failure "anast != ansSlow")
	with
	    | Failure s -> 
		Printf.fprintf stderr "FAILURE\nFAILURE\nException is Failure \"%s\"\n" s;
		failedOnce := true;
		raise (Failure s)
	    | e -> 
		Printf.fprintf stderr "FAILURE\nFAILURE\nsome other exception \n";
		failedOnce := true;
		raise e

let _ =
    let g = G.fromFile "inp" in
    test g;
    let ( { DetRandom.state = st } as drs) = DetRandom.loadState "randseed" in
(*    List.iter test
    [ 
      (
        [| 
           (0, 1), 1;
           (0, 2), 2;
           (1, 0), 3;
           (1, 1), 4;
           (2, 1), 5
        |], 3, 3
      );
      
      (
        [|
	|], 0, 0
      );
      
      (
        [|
	    (0, 0), 101;
	|],
	1, 1
      );
      
      (
        [|
	    (0, 1), 45;
	    (0, 0), 54;
	|],
	1, 2
      );
      
      (
        [|
	    (0, 0), 45;
	    (0, 1), 54;
	|],
	1, 2
      );
      
      (
        [|
	    (1, 0), 45;
	    (0, 0), 54;
	|],
	2, 1
      );
      
      (
        [|
	    (0, 0), 45;
	    (1, 0), 54;
	|],
	2, 1
      );
      (
	[|
	|],
        10, 10
      );
      
      
    ];*)
    for i = 0 to 1000 do 
      test (randomGraph 2 2 0.5 st);
    done;
    for i = 0 to 1000 do 
      test (randomGraph 3 3 0.3 st);
    done;
    for i = 0 to 1000 do 
      let g = randomFullGraph 8 8 st in
      test g;
    done;
  for i = 0 to 1000 do
      let g = randomGraph  8 8 0.5 st in
      test g;
    done;
    for i = 0 to 1000 do
      let g = randomGraph 8 8 0.2 st in
      test g;
    done;
    for i = 0 to 1000 do
      let g = randomGraph 12 12 0.05 st in
      test g;
    done;
    DetRandom.saveState drs;


    if !failedOnce then exit 1
    
