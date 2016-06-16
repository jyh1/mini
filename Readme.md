# FOPL Course Project

The Mini Language interpreter, developed with Haskell.

## Building (using [stack](https://github.com/commercialhaskell/stack)):
```
stack setup
stack build
```
## Features:
1. Extended mini language syntax
2. Minimum error message
3. GC (Memory management detail will be printed to standard output)

  ```
  Program1:
    Pro
      Print Num 2 End
      Decl x End
    End
  ErrorMessage1:
    "program.txt" (line 3, column 3):
    unexpected "D"
    expecting "End"

  Program2:
    Pro
      Print Id x End End
    End
  ErrorMessage2:
    Variable x is not in scope! "program.txt" (line 1, column 14)

  Program3:
    Pro
      Decl x End
      Print Id x End End
    End
  ErrorMessage3:
    Unprintable object!
  ```
3. First citizen function, making the use of keyword `FunName` obsolete
  ```
  Program:
    Pro
      Func f Para End
        Pro
          Print Num 2 End End
        End
      End
      Func g Para x End
        Pro
          Return Id x End End
        End
      End
      Decl q End
      LetBe q AppFun g List Id f End End End End
      AppFun q  List End End
    End    
  Output:
    2
  ```

4. Closure
  ```
  Program:
    Pro
      Func f Para n End
        Pro
          Decl x End
          LetBe x Id n End End
          Func g Para m End
            Pro
              LetBe x
                Plus Id x End Id m End End
              End
              Print Id x End End
            End
          End
          Return Id g End End
        End
      End

      Decl add End
      LetBe add
        AppFun f List Num 0 End End End
      End
      AppFun add List Num 2 End End End
      AppFun add List Num 3 End End End
    End
  Output:
    2
    5
  ```
