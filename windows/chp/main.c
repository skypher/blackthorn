/*
 * CHP (Create Hidden Process) -
 * Silently creates a GUI or console process in a hidden window
 *
 * Copyright (C) 2007 Ritchie Lawrence
 * http://www.commandline.co.uk
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#define UNICODE
#include <windows.h>

PTCHAR GetArgs(VOID)
{
	PTCHAR p = GetCommandLine();

	if(*p == TEXT('"'))
	{
		while(*p == TEXT('"')) p++; // skip any leading quotes
		while(*p != TEXT('"')) p++; // skip over exe name
		while(*p == TEXT('"')) p++; // skip any trailing quotes
	}
	else  // didn't start with a quote, so skip non-whitespace
	{
		while((*p) && (*p != TEXT(' ')) && (*p != TEXT('\t'))) p++;
	}
	// skip whitespace
	while((*p) && ((*p == TEXT(' ')) || (*p == TEXT('\t')))) p++;
	return p;
}

INT main(VOID)
{
	PTCHAR Args;
	Args = GetArgs();
	INT ExitCode;

	STARTUPINFO si;
	ZeroMemory(&si, sizeof(si));
	si.cb = sizeof(si);
	si.dwFlags = STARTF_USESHOWWINDOW;
	si.wShowWindow = SW_HIDE;

	PROCESS_INFORMATION pi;
	ZeroMemory(&pi, sizeof(pi));

	// if CreateProcess fails, exit with GetLastError, otherwise exit with (process ID * -1)
	ExitCode = CreateProcess(NULL, Args, NULL, NULL, FALSE, CREATE_NEW_CONSOLE, NULL, NULL, &si, &pi) \
        ? pi.dwProcessId : GetLastError() * -1;

	printf("%d\n", ExitCode);
	exit(ExitCode);
}
