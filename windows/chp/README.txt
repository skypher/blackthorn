

CHP.EXE (Create Hidden Process)


Synopsis

CHP.EXE is a very simple program utilising the Win32 CreateProcess API to
silently launch GUI and console apps in a hidden window.

Usage

CHP yourapp arg1 arg2 arg3 ...
For example:-
CHP notepad <-- runs notepad.exe in a hidden window

Exit Status

If CHP succeeds, its exit_status is the process ID (PID) of the newly created
process.
If CHP fails to create the specified process, its exit status is the Win32
error_code that caused the failure, multiplied by -1. Use the "NET_HELPMSG"
command to obtain the meaning of the error code.
Although CHP is a windowless GUI application, it writes its exit status to
stdout. In order to see the output, it must be piped into a program that writes
own stdin to stdout (i.e. MORE). For example, in a cmd.exe shell:-
CHP notepad | more

Compiling

This package includes a pre-compiled binary, but if you want to compile
CHP yourself, I recommend either of the following free IDE's:-

* Dev C++, http://www.bloodshed.net/
* Code::Blocks, http://www.codeblocks.org/

Note: The source should be compiled as a GUI (not a console) application.

-- Ritchie
