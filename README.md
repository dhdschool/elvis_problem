# Getting Started
This code base relies on a functional programming language known as Haskell. The main advantages of Haskell for this project include the strong type system that prevents an assortment of errors at runtime, as well as the strong style of pure functional programming that lends itself to a easy translation to analysis.

Before installing, please ensure that your system has ~10GB of free space (if on Windows).

### Installation Guide
Optionally, you may wish to install Visual Studio Code (vscode). This IDE comes with the ability to install extensions that provide syntax and error highlighting for the haskell code provided, as well as an ease of use in navigating the Linux kernal from a Windows user's perspective (see below). Visual studio code is **not** required to run this project, however. If you wish to install vscode (and are on a Windows operating system), you should do so before proceeding further into this installation and download the vscode installer **on Windows**. You can do so at this link https://code.visualstudio.com/download.

Unfortunately for the wide swath of the population that uses Windows (myself included), one of the dependencies for one of the projects requires a tarball installation from a remote repository, which creates a massive headache in non-Linux operating systems. Haskell itself can be run on Windows (albiet somewhat poorly), but it is wise to have a Linux kernal if you are interested in using open-source software, regardless. If you are already working on a Linux operating system, you may skip to the section marked "From Linux".

### Windows Subsystem for Linux
Long gone are the days of needing a dual boot system that may even partition a single hardrive for multiple operating systems. The modern solution for Windows users to access their own Linux kernal is to use a built in Microsoft extension called Windows Subsystem for Linux. To do so, simply open a PowerShell terminal with administrator privileges (this can be done by pressing Windows+R, typing in powershell, and pressing Ctrl-Shift-R to run as administrator). Once you have opened PowerShell, run the following command to install WSL.

```wsl --install -d Ubuntu```

This command will install a new Ubuntu Linux kernal onto your machine. Select an administrative password that you will not forget (it becomes very difficult to reset this password should you forget it). Once the installation is complete, you may access the Linux kernal from PowerShell with the command ```wsl```. When you first open WSL, you will be in the Linux symlink to your Windows operating system, and you should switch to the root directory of your new Linux operating system. To reach the root directory in Linux (this directory is equivalent to C: in Windows), type in the command  ```cd /``` (this stands for change directory to root).

### From Linux
Now that you have access to a Linux kernal, there are some required packages that you must install. The first of these packages is ```ghcup```, an full package installer for Haskell that includes: GHC (the compiler for Haskell); cabal (the package manager for Haskell); HLS (Syntax and error highlighting in real time). To install ghcup on Linux, one must install its dependencies first. These can be obtained with the following set of commands

```
sudo apt-get update
sudo apt-get install curl
sudo apt-get install -y libffi-dev libffi8ubuntu1 libgmp-dev libgmp10 libncurses-dev
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

If this returns no errors, then ghcup has been successfully installed. To use ghcup to install haskell, run the command ```ghcup tui```. This will open a ghcup text user interface (TUI). Install GHC 9.10.1 (the latest version) by navigating to it using the arrow keys, the pressing enter. Press enter on the 'Install' button, then navigate down to the 'set' button and press enter. Next, press advance install. In a similar fashion, install cabal 3.12.1.0, and HLS 2.9.0.1. Once this is done, press the q key to exit the TUI.

Once this process has completed, your Linux kernal should now be able to compile and run Haskell code, as well as import packages using the cabal package manager.

### (Optional) vscode integration
If you have opted to use vscode, there are three extensions you should install for nominal performance. The first of these is the WSL extension by microsoft, which will allow you to open a remote window into your Linux kernal and modify/execute the code directly from Windows. Once this is done, open a new remote window by pressing the blue button on the bottom left of your screen, then selecting "Connect to WSL". Once you have done this, install the Haskell and Haskell Syntax Highlighting extensions into your WSL remote window.

### Getting the repository
The next step is acquiring this code. If your Linux kernal does not already have git (it won't if you have freshly installed WSL), install git by running ```sudo apt-get install git```. I advise you to clone the repository to a new directory in your Linux kernal for performance, but it is possible to clone it to your Windows operating system (contained in the folder /mnt/ on the Linux kernal). To do this, navigate to your user directory (from the Linux terminal). This can be done via the command ```cd /home/{your_username_here}```. To create a new directory to clone the repository to, use the commands 
```
mkdir elvis_problem
cd elvis_problem
git clone "https://github.com/dhdschool/elvis_problem"
``` 
which will make a new folder in your user directory and clone this GitHub repository into that folder.

### Installing the project dependencies
To install the libraries that this project depends on, run the command ```cabal build``` once in the directory of the repository (ideally /home/{username}/elvis_problem or some variant). After this is run, your installation is complete

# Usage
I have not yet decided end-user usage for this project will look like. Currently, my method for using these methods is to run ```cabal repl```, which opens a "read-eval-print-loop" for Haskell. Once inside, any functions contained in the app folder files can be run. To exit the REPL, type ":q". An example of what this may be used for can look as follows

```
ghci> test_v
1.0000000000e0:#1.0000000000e0:#Nil

ghci> single_proj test_func test_v
5.5495973728e-1:#6.6375439001e-1:#Nil
```

This example provides the projection of a vector (in this case, predefined to be test_v) onto the set produced by a convex function (predefined to be test_func) where test_v is the vector (1,1) and test_func is the function (x^2 + ((y+1)^2/4) - 1 <= 0). To create your own vectors and functions, (in this example I will show you R^3, with vector (-5, 2, 1) and function (x^2 + y^2 + z^2 - 1 <= 0)), you may use the format
```
ghci> my_vector = -5:#2:#1:#Nil
ghci> my_function v = (index FZ v)^2 + (index (FS FZ) v)^2 + (index (FS (FS FZ)) v) ^2 - 1
```

Should you want to find the projection of your new vector onto your new function, simply run the command
```
ghci> single_proj my_function my_vector
```
Which will give you (in this case) the result:

```
-8.6590542196e-1:#4.4934348949e-1:#2.1976851544e-1:#Nil
```

Be warned, this code assumes your functions are convex, bounded, and lower semi-continous (and the set that they generate contains the zero vector). If your functions do not satisfy these constraints, there may occur some unexpected behavior (infinite loops, crashes, wrong answers, etc.)

# Documentation

### The Vec type
The singleton natural numbers on the type level, finite numbers contained within their natural bounds, the Vector type (Vec for short), and various operations defined on vectors of all types are defined in the FixedVector.hs file. I am not a Haskell professional, and this code was made solely with the help of a tutorial by Justin Le which can be found here https://blog.jle.im/entry/fixed-length-vector-types-in-haskell.html, modified to some degree for my own use cases. There is a Haskell library for fixed-sized vectors (which no doubt is faster and easier to use), however I made this file to familarize myself with the in-depth nuances of the Haskell language (this is my first Haskell project). 

The Vec type takes in a natural number (on the type level) n, which defines how many object can be placed within the vector, as well as the type of object that the Vec contains. I have defined the Real numbers to be floats of a certain precision in Elvis.hs, and all operations defined on vectors in R^n must be of size natural number n and of object type R.

My vector is defined as having a constructor that leads to an end. Thus, to make a vector in R^2, one must simply type a:#b:#Nil, where :# is the vector construction operator and Nil is the vector end type, and a and b are Real numbers. The Haskell type system is able to infer the size of this vector at compile time. Similarly to make a vector in R^3, one must use a:#b:#c:#Nil, etc.

Some of the operations defined on all vectors are 

|++|, the append operation, which takes two vectors of the same type and of dimensions n, m, and makes a new vector of dimension n+m, with the second vector being added to the end of the first.

index, which takes in a finite natural type and returns the value at that index. Currently, this implementation requires natural succession usage, and so to find x0 of v, one must type index FZ v, and x1 is index (FS FZ) v, x2 index (FS (FS FZ)) v, etc.

vecreplicate, which takes in a value of type a and a natural number size n and returns a Vec n a that contains only values a. For example, the zero vector in R (of dimension 4) can be made using (once the DataKinds and ScopedTypeVariables extensions have been activated): (vecreplicate 0 :: Vec (Lit 4) R)

generate, which takes in a function that operates on the natural numbers between 0 and (n-1), and makes a new vector where xi corresponds to f(xi). For example, the gradient of a function can be defined as follows: generate (\i -> directional_derivative f x (index i identity_matrix)), given that the directional_derivative function and indentity matrix are defined.

### Real Vectors

Some operations that we would like to have defined are not applicable to all vectors, but only to Real vectors. To do this, I have created the typeclass RealVec, which defines the types of all real vector operations, including the norm, dot product, vector addition, vector subtraction, scalar multiplication, the unit vector, the bisector of two vectors, the angle between two vectors, the directional derivative of a vector valued function in direction of a vector v, and the gradient of a real vector valued function.

This class is then instantiated for all vectors of object type R and of dimension n (where n is a natural number). The implementation allows the implicit inferencing of the vectors size, therefore the function 'norm' can be run on vectors in R^2 and R^3 without having to pass in the size of the current vector.

### Convex Sets

Similarly, there are some operations we would like to have defined on Convex sets. Note that this program does not check whether or not a function produces a convex set at compile time (such a program would require extreme use of meta-programming and dependent types at a level that I am not proficient with, and most likely would tank performance), therefore it is up to the user to ensure that all inputs are proper. The operations that are defined however, are set intersection, projection of a real vector onto a set, the corresponding distance of the projection, the minkowski sum, and whether or not a vector is contained within the set. The data type that is instantiated is VSet (short for vector set), which is simply a syntatic equivalent for a vector of dimension m that contains some minkowski sum of a list of vector valued functions that map from R^n -> R. This instantiation is valid for all natural numbers n and m.

### The Proj file
Proj.hs is an internal file that uses numerical methods to approximate the projection. When this project is finished, the users ability to access Proj.hs will be removed (as it is an internal tool), so if one wishes to access those function from ghci at that time, run the command ```:l app/Proj.hs``` to load those functions into the ghci namespace.

</div>