# FinalCode

This is the final project for ITBA's 2019 first semester Functional Programming course.
The goal of the project is to make a compiler using Haskell and Parsec library, as close as possible to the
javascript language.

# Types:
 * Supported
   * Integer
   * Boolean
   * Functions
 * To be Supported
   * Numeric
   * String

#Operations:
 * Supported
   * Integer
      * Neg
      * Add
      * Subtract
      * Multiply
      * Divide
      * Compare (<,<=,==,>=,>)
   * Boolean
      * Not
      * And
      * Or
   * Function
      * Call Without Parameters
      * Call With Parameters
      * Return value
 * To be Supported
   * Integer
      * Mod
      * Compare (!=)
      * Cast to String
   * Boolean
      * Cast to String
   * String
      * Concatenate
      Compare (<,<=,==,!=,>=,>)

# Statements
 * Supported
    * ';' or '\n' to separate statements
    * If
    * If else
    * While
    * Assign Variable to
      * Integer
      * Boolean
      * Other Variable
      * Function
    * Print (hardcoded string)
 * To be Supported
    * For
    * Assign Variable to String
    * Assign Variable to Undefined
    * Print with variable

# Scope
In javascript one can find 3 types of scopes:
* Global scope
    * Does not need any keyword
    * Accessible by everyone, in or outside functions and/or blocks
* Function scope
    * Preceded with keyword "var"
    * Accessible by everyone within a function, in or outside blocks
* Block scope
    * Preceded with keyword "let"
    * Accessible by everyone within the block and nothing else
    
FinalCode supports all 3 of these scopes, and work as intended

#Other Functionalities
  * Call functions within parameters of another function
  * Call functions while doing arithmetic operations
  * Call functions while doing boolean operations
  * Call functions while doing if conditions
  * Call functions while doing while conditions
  * do arithmetic operations in parameters of a function
  * do boolean operations in parameters of a function
  * return any kind of value or expression
  * reassign variables to another name
  * others...
