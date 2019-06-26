# Presto

### Design Principles

* Correctness
  * Design By Contract
    - Preconditions
    - Invariants
    - Postconditions
  * Unit-Testable
    - Interfaces
    - Mocks
  * Strongly typed
* Concise but clear
  * No need for ugliness like
    * visitor pattern
  * Code shouldn't require deciphering
  * Code shouldn't be intimidating to look at
* Easy to do the right thing, hard to do the wrong thing
  * Immutable by default
* Interoperable
  * Should be able to mix and match languages
* Lightweight Abstractions
  * LINQ should be compiled away whenever possible
* Data-oriented
  * Fundamentally there are 3 types of operations in programming
    * Send/Receive data
      * STD Function Calls
        * file system
        * network
        * IPC mechanism
      * Imported/exported function
    * Validate data
      * Generating/handling errors
    * Transform data
      * From one data structure to another
* No garbage collection
  * Instead, single ownership wherever possible

### To-Do

* Figure out target language
* Add code generator for target language
* Add semantic analysis step

### Ideas

* 