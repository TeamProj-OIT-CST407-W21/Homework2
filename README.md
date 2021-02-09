# Homework2

Haskell implementation of simplified DES block cipher. Algorithm patterned on attached diagrams.

Takes in a binary bitstring from input and changes via the cipher.  Includes basic error checking.

Files in main branch were tested with the GHCi interpreter.  The folder GHC build contains the complied GHC code and executable, compiled on a Windows 10 operating system.  The biggest difference between them is the error handling which largely uses print statements in GHC, but uses exception throwing in the GHCi build. Currently, the GHC build will use prints for general input error, still kills the program if a system level exception is thrown (such as a parse error), does not catch for those kind of errors.
