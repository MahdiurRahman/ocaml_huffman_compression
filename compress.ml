(* READ FILE + MAKE DATA *)
	let read_file filename = 
		let data = ref [] in
		let in_stream = open_in filename in
		try
			while true; do
		    	data := input_line in_stream :: !data
		  	done; !data
			with End_of_file ->
		  		close_in in_stream;
		  		List.rev !data

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

	let rec create_chart tree_ =
		let rec helper acc tree_ =
			match tree_ with
			| Leaf (c, num) -> chart := (c, acc)::!chart
			| Node (x, l, r) ->	(helper (acc ^ "0") l;
								helper (acc ^ "1") r)
		in
		match tree_ with
		| hd::tl -> helper "" hd
		| [] -> ()

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

(* CONVERT TREE TO STRING *)
	let rec string_of_tree tree_ =
		match tree_ with
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
							"\'" ^ (
									if c = '\n' then
										"\\n"
									else
										String.make 1 c
									) ^ "\'" ^
							", " ^
							string_of_int num ^
							")"
						)

	let convert_tree_to_string tree_ =
		match tree_ with
		| hd::tl -> string_of_tree hd
		| [] -> ""

(* CREATE DECOMPRESS FILE *)
	let type_string = ref "\ntype tree = Leaf of (char * int) | Node of (int * tree * tree)\n"
	let tree_string = ref "placeholder"
	let decompress_string = ref "\nlet return_string filename =\nlet in_stream = open_in filename in\nlet file_string = input_line in_stream in\nfile_string\nlet string_main = ref \"\"\nlet rec read_tree node string_ =\nif string_ <> \"\" then\n(\nmatch node with\n| Node (num, l, r) ->	if string_.[0] = '0' then\nread_tree \nl \n(String.sub\nstring_ \n1 \n(String.length string_ - 1))\nelse\nread_tree \nr \n(String.sub\nstring_ \n1 \n(String.length string_ - 1))\n| Leaf (c, num) -> \n(string_main := !string_main ^ (String.make 1 c);\nread_tree !tree_main string_)\n)\nelse\n()\nlet () =\nlet compressed_file = return_string Sys.argv.(1) in\nread_tree !tree_main compressed_file;\nlet out_stream = open_out Sys.argv.(2) in\nPrintf.fprintf out_stream \"%s\n\" !string_main;\nclose_out out_stream"

	let make_decompress_file string_ =
		let out_stream = open_out "decompress.ml" in
		Printf.fprintf out_stream "%s\n" string_;
		close_out out_stream

let () =
	let data_file = Sys.argv.(1) in
	let data = ref (create_data data_file) in
	let table = create_frequency_table !data in
	let tree_ = ref (create_tree table) in
	create_chart !tree_;
	create_compressed_file !data Sys.argv.(2);
	tree_string :=
		"let tree_main = ref (" 
		^ (convert_tree_to_string !tree_)
		^ ")";
	let huge_string = !type_string ^ !tree_string ^ !decompress_string in
	make_decompress_file huge_string;
