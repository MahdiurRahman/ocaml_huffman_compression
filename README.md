# Huffman Compression Algorithm in functional language oCaml
This is a Huffman-Code compression program built in oCaml.

ABOUT THIS PROJECT:
This was a self-designed project for a 400-level functional programming class at Hunter College, taught by Alexey Nikolaev. The project ideas were not assigned but were conceptualized by students themselves and agreed upon with the instructor. The projects were also done solo.

This project uses Huffman's Tree algorithm to compress and then decompress files.

NOTE: Due to my limitations with oCaml, I was not able to convert files, read as strings, to smaller binary versions. Hence, the 'compressed' file is actually a string of 1's and 0's and not actual binary bits themselves. Hence this is not, practically speaking, actual compression. HOWEVER, the algorithm works as intended and the only thing missing would be slight tweaks so that the algorithm might write bits of 1's and 0's instead of strings of 1's and 0's.

The file 'compress.ml' essentially creates a second ml file called 'decompress.ml'. This decompress.ml file is unique to whatever file you choose to compress and can ONLY decompress that file.

How to run:

1. open terminal:
2. navigate to folder where files are saved:
3. use command:
	make
4. use command:
	./compress filename1.ext filename2.ext
5. use command:
	make decompress
6. use command
	./decompress filename2.ext filename3.ext

filename1.ext and filename3.ext should have the same contents

Credits:
1. compress.ml [lines 2:11]
StackOverflow user aneccodeal in
https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml/23456034

2. oCaml guide
https://ocaml.org/learn/tutorials/file_manipulation.html
