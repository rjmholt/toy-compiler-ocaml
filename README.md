Simple Bean Compiler
====================

Note to Unimelb students
---

If you use this codebase in any other way than as a reference for your
own original project, it will be obvious. I am a PLI tutor. Your work will
be run on an originality checker and compared to this.

I've left it up here because there are not many good resources for writing
a compiler in OCaml on the web -- at least not in full code -- and this is
relatively self-documenting. Don't make me take it down.

Contents
---

With this README should be the following files:

    * bean.ml
    * bean\_lex.mll
    * bean\_parse.mly
    * bean\_ast.ml
    * bean\_ast.mli
    * bean\_pprint.ml
    * bean\_pprint.mli
    * bean\_symtbl.ml
    * bean\_symtbl.mli
    * bean\_semantic.ml
    * bean\_semantic.mli
    * bean\_intermediate\_code.ml
    * bean\_intermediate\_code.mli
    * bean\_oz.ml
    * bean\_oz.mli
    * Makefile

These files compose the Bean compiler source code (so far).

Building Bean
-------------
If your working directory is otherwise empty, run:
    make
to construct the Bean compiler executable ("bean").

If you wish to recompile it, run:
    make clobber
first to clean the directory of old intermediary OCaml files
before running "make" again. Other Makefile commands are documented
in that file.

If `make` fails the first time, you make need to run:

    make depend

first, to generate the dependency files for bean.

Running the Bean Compiler
-------------------------
To run the Bean compiler, execute like so:

    bean [-p] [-o <output-filepath>] <bean-filepath>

Or, if the Bean executable is not on your $PATH, but in your local
directory:

    ./bean [-p] [-o <output-filepath>] <bean-filepath>

For example:

    ./bean -o mung.oz mung.bean

will compile the file "mung.bean" in the current directory to mung.oz.

No flag will call the Bean compiler. This compiles to Oz code, to run on the
University of Melbourne Oz emulator. If you specify an output file with the
"-o" flag, bean will output to that file. If no file is specified bean will
print to stdout.

The "-p" flag will call the Bean pretty printer on the program specified.
This option is currently well-supported.

Not specifiying a file will cause Bean to compile whatever is passed to it
over stdin. This may be useful if you are a power-user or have a moral
disagreement with text editors, and instead prefer to write programs like 
this (this is probably still more usable than `ed`):

    cat <<EOF | ./bean
    proc main()
        write "Hello, World!";
    end
    EOF
