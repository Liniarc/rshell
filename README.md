rshell
======

Project source can be downloaded from https://github.com/Liniarc/rshell.git


###Author & Contributor List
Thomas Liu

###Synopsis

Recreate command shell for CS100 assignment in Haskell 

The shell is capable of taking in a command line and executing it similarlly to how it'd be executed in the Bash shell. 

### Running the file
In order to run this program, you need to have the ghc compiler.

### Bugs
Currently treats quotes as normal characters
e.g. `echo "four&nbsp;&nbsp;&nbsp;&nbsp;spaces"` will output `"four spaces"` instead of `four&nbsp;&nbsp;&nbsp;spaces`

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

### Todos
Currently the shell runs via giving rawSystem the commands. Use foriegn imports or process and forking instead

Look into parsec for monadic parsing to simplify parsing commands

Look into haddoc for haskell documentation

Include custom support for the ls command (hw1)

Include support for piping and I/O redirection (hw2)

Include support for finding commands, the cd command, and control characters (hw3)
