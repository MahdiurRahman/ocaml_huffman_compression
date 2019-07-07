let rec print_contents contents =
	match contents with
	| [] -> ()
	| hd::tl -> Printf.printf "%s\n" hd; print_contents tl

(* READ FILE + MAKE DATA *)
	let read_file filename = 
		let lines = ref [] in
		let chan = open_in filename in
		try
			while true; do
		    	lines := input_line chan :: !lines
		  	done; !lines
			with End_of_file ->
		  		close_in chan;
		  		List.rev !lines

	let rec add_slash_n ls =
		match ls with
		| hd::tl -> (hd ^ "\n")::add_slash_n tl
		| [] -> []

	let create_data filename =
		let data = read_file filename in
		add_slash_n data

(* MAKE TABLE *)

	let rec update_acc acc char_ =
		match acc with
		| (char__, count)::tl ->
								if char__ = char_ then
									(char__, (count + 1))::tl
								else
									(char__, count)::update_acc tl char_
		| [] -> (char_, 1)::[]

	let rec read_string acc string_ index limit =
		if index < limit then
			read_string 
				(update_acc acc (String.get string_ 0)) 
				(String.sub string_ 1 (String.length string_ - 1)) 
				(index + 1) 
				limit
		else
			acc

	let rec set_up_table acc data =
		match data with
		| hd::tl ->
					set_up_table
						(read_string
							acc
							hd
							0
							(String.length hd))
						tl
		| [] -> acc

	let compare_tuple a b =
		match a, b with
		| (x1, y1), (x2, y2) -> if y1 > y2 then
									1
								else
									if y1 < y2 then
										-1
									else
										0

	let create_frequency_table data =
		let data = set_up_table [] data in
		let data = List.sort compare_tuple data in
		data

(* MAKE TREE *)

	type tree = Leaf of (char * int) | Node of (int * tree * tree)

	let rec convert_table ls =
		match ls with
		| (x, y)::tl -> (Leaf (x, y))::convert_table tl
		| [] -> []

	let return_tuple a b =
		match a, b with
		| Leaf (x1, y1), Leaf (x2, y2) -> (y1, y2)
		| Node (num, l, r), Leaf (x2, y2) -> (num, y2)
		| Leaf (x1, y1), Node (num, l, r) -> (y1, num)
		| Node (num1, l1, r1), Node (num2, l2, r2) -> (num1, num2)

	let compare_node a b =
		let tuple = return_tuple a b in
		match tuple with
		| (a, b) -> compare_tuple ('a', a) ('b', b)

	let combine_node a b =
		match a, b with
		| Leaf (x1, y1), Leaf (x2, y2) -> Node ((y1 + y2), a, b)
		| Node (num, l, r), Leaf (x2, y2) -> Node ((num + y2), a, b)
		| Leaf (x1, y1), Node (num, l, r) -> Node ((y1 + num), a, b)
		| Node (num1, l1, r1), Node (num2, l2, r2) -> Node ((num1 + num2), a, b)

	let rec generate_tree data =
		let data = List.sort compare_node data in
		match data with
		| a::b::tl -> generate_tree ((combine_node a b)::tl)
		| a::[] -> data
		| [] -> data

	let create_tree data =
		let data = convert_table data in
		generate_tree data

(* MAKE CHART *)
	let chart = ref [('@', "")]

	let rec create_chart tree =
		let rec helper acc tree =
			match tree with
			| Leaf (c, num) -> chart := (c, acc)::!chart
			| Node (x, l, r) ->	(helper (acc ^ "0") l;
								helper (acc ^ "1") r)
		in
		match tree with
		| hd::tl -> helper "" hd
		| [] -> ()

(* OUTPUT TREE FILE *)
	let rec string_of_tree tree =
		match tree with
		| Node (num, l, r) -> ( 
							"Node (" ^
							string_of_int num ^
							", " ^
							(string_of_tree l) ^
							", " ^
							(string_of_tree r) ^
							")"
						)
		| Leaf (c, num) ->	( 
							"Leaf (" ^
							"\'" ^ (String.make 1 c) ^ "\'" ^
							", " ^
							string_of_int num ^
							")"
						)

	let convert_tree_to_string tree =
		match tree with
		| hd::tl -> string_of_tree hd
		| [] -> ""

	let create_tree_file tree filename =
		let tree_string = convert_tree_to_string tree in
		let out_stream = open_out filename in
		output_string out_stream tree_string;
		close_out out_stream;
		Printf.printf "Created tree in file called: %s\n" filename;

(* OUTPUT COMPRESSED FILE *)
	let rec search_chart char_ ls =
		match ls with
		| hd::tl ->(
					match hd with
					| (char__, string_) -> 
										if char__ = char_ then
											string_
										else
											search_chart char_ tl
					)
		|[] -> ""

	let rec string_to_binary_helper string_ acc =
		if string_ <> "" then
			string_to_binary_helper 
				(String.sub string_ 1 (String.length string_ - 1)) 
				(acc ^ (search_chart (String.get string_ 0) !chart))
		else
			acc

	let rec string_to_binary ls acc =
		match ls with
		| hd::tl ->	string_to_binary 
						tl 
						(acc ^ (string_to_binary_helper hd ""))
		| [] -> acc

	let create_compressed_file data filename =
		let compressed_string = string_to_binary data "" in
		let out_stream = open_out filename in
		output_string out_stream compressed_string;
		close_out out_stream;
		Printf.printf 
			"Created compressed-version in file called: %s\n" filename

(* let () =
	let file_stream = read_file Sys.argv.(1) in
	let data = create_data file_stream in
	let table = create_frequency_table data in
	let tree = create_tree table in
 *)















