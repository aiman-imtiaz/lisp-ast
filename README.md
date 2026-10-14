### AST Printer
1. To build the project, run

    ```
    $ make
    ```
2. In order to run the AST printer on terminal, type:
    ```
    bin/main.native
    ```
3. Type a Lisp program after `>`, for example:
    ```
    > (first (list 1 (+ 2 3) 9))
    ```
4. Press `Ctrl-D` to signal end of file. You will see pretty-printed outcome of the parser:
    ```
    bin/main.native
    > (first (list 1 (+ 2 3) 9))
    [ first [ list 1 [ + 2 3 ] 9 ] ] %
    ```
