Simple Bean Compiler
====================

With this README should be the following files:

    * bean.ml
    * bean_lex.mll
    * bean_parse.mly
    * bean_ast.ml
    * bean_ast.mli
    * bean_pprint.ml
    * bean_pprint.mli
    * bean_symtbl.ml
    * bean_symtbl.mli
    * bean_semantic.ml
    * bean_semantic.mli
    * bean_intermediate_code.ml
    * bean_intermediate_code.mli
    * bean_oz.ml
    * bean_oz.mli
    * Makefile

These files compose the Bean compiler source code (so far).

Building Bean
-------------
If your working directory is otherwise empty, run:
    "make"
to construct the Bean compiler executable ("bean").

If you wish to recompile it, run:
    "make clobber"
first to clean the directory of old intermediary OCaml files
before running "make" again. Other Makefile commands are documented
in that file.

If `make` fails the first time, you make need to run:
    "make depend"
first, to generate the dependency files for bean.

Running the Bean Compiler
-------------------------
To run the Bean compiler, execute like so:
    "bean [-p] [-o <output-filepath>] <bean-filepath>"
Or, if the Bean executable is not on your $PATH, but in your local
directory:
    "./bean [-p] [-o <output-filepath>] <bean-filepath>"
For example:
    "./bean -o mung.oz mung.bean"
will compile the file "mung.bean" in the current directory.

No flag will call the Bean compiler. This compiles to Oz code, to run on the
University of Melbourne Oz emulator. If you specify an output file with the
"-o" flag, bean will output to that file. If no file is specified bean will
print to stdout.

The "-p" flag will call the Bean pretty printer on the program specified.
This option is currently well-supported.

Not specifiying a file will cause Bean to compile whatever is passed to it
over stdin. This may be useful if you are a power-user or don't like text
editors (this is probably still more usable than ed), and instead prefer to
write programs like this:
    "cat <<EOF | ./bean
    proc main()
        write "Hello, World!";
    end
    EOF"
