<link rel="stylesheet" type="text/css" media="all" href="/readme/style.css"/>
<img src=./readme/readme.svg width=100 height=100 alt="Error">


<div class="force-code-wrap" markdown="1">

# Getting Started
This code base relies on a functional programming language known as Haskell. The main advantages of Haskell for this project include the strong type system that prevents an assortment of errors at runtime, as well as the strong style of pure functional programming that lends itself to a easy translation to analysis.

Before installing, please ensure that your system has ~10GB of free space (if on Windows).

### Installation Guide
Optionally, you may wish to install Visual Studio Code (vscode). This IDE comes with the ability to install extensions that provide syntax and error highlighting for the haskell code provided, as well as an ease of use in navigating the Linux kernal from a Windows user's perspective (see below). Visual studio code is **not** required to run this project, however. If you wish to install vscode (and are on a Windows operating system), you should do so before proceeding further into this installation and download the vscode installer **on Windows**. You can do so at this link https://code.visualstudio.com/download.

Unfortunately for the wide swath of the population that uses Windows (myself included), one of the dependencies for one of the projects requires a tarball installation from a remote repository, which creates a massive headache in non-Linux operating systems. Haskell itself can be run on Windows (albiet somewhat poorly), but it is wise to have a Linux kernal if you are interested in using open-source software, regardless. If you are already working on a Linux operating system, you may skip to the section marked "From Linux".

### Windows Subsystem for Linux
Long gone are the days of needing a dual boot system that may even partition a single hardrive for multiple operating systems. The modern solution for Windows users to access their own Linux kernal is to use a built in Microsoft extension called Windows Subsystem for Linux. To do so, simply open a PowerShell terminal with administrator privileges (this can be done by pressing Windows+R, typing in powershell, and pressing Ctrl-Shift-R to run as administrator). Once you have opened PowerShell, run the following command to install WSL.

```wsl --install -d Ubuntu```

This command will install a new Ubuntu Linux kernal onto your machine. Once the installation is complete, you may access the Linux kernal from PowerShell with the command ```wsl```. When you first open WSL, you will be in the Linux symlink to your Windows operating system, and you should switch to the root directory of your new Linux operating system. To reach the root directory in Linux (this directory is equivalent to C: in Windows), type in the command  ```cd /``` (this stands for change directory to root).

### From Linux
Now that you have access to a Linux kernal, there are some required packages 
</div>