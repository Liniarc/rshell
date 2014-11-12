rshell
======

Project source can be downloaded from https://github.com/Liniarc/rshell.git


###Author & Contributor List
Thomas Liu

###Synopsis

Recreate command shell for CS100 assignment in Haskell 

The shell is capable of taking in a command line and executing it similarlly to how it'd be executed in the Bash shell. 

In addition, there is a clone of ls and cp which which emulates the bash built-in commands.

### Running the file
In order to run this program, you need to have the ghc compiler.

### Bugs
#####rshell
Does not support piping, control characters, assignment operators, etc.

Does not output errors if it is unable to find a command

Ignores "empty" commands.
e.g. `;echo 5` will not give an error and simply print out `5`

Short-circuit of `&&` and `||` will short-circuit all remaining commands even if remaining commands would normally be executed
e.g. `true || echo 1; echo 2` will not run `echo 2`
This is different from the Bash terminal would create sub operators such that the command would run as `(true || echo 1); echo 2` which would call `echo 2`

Parenthesis are not supported of logical operators (or in general)

cd does not work

Will not work if in script mode

#####ls

ls will output uppercase files before lowercase files

ls will not minimize column lengths and pad all files to the length of the longest file

ls -l has set width of columns. If a cell is too long, it will throw off the entire row (i.e. a file with 100+ links to it)

ls displays the files in order horizontally instead of vertically.

ls assumes screen width of 80 rather than dynamically determining that

### Todos
Look into haddoc for haskell documentation

Include support for piping and I/O redirection (hw2)

Include support for finding commands, the cd command, and control characters (hw3)
