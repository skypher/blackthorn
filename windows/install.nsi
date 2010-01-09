;;;; Blackthorn -- Lisp Game Engine
;;;;
;;;; Copyright (c) 2007-2010, Elliott Slaughter <elliottslaughter@gmail.com>
;;;;
;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation
;;;; files (the "Software"), to deal in the Software without
;;;; restriction, including without limitation the rights to use, copy,
;;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;;; of the Software, and to permit persons to whom the Software is
;;;; furnished to do so, subject to the following conditions:
;;;;
;;;; The above copyright notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;;; DEALINGS IN THE SOFTWARE.
;;;;

;;;; -------------------------------------------------------------------------
;;;; Usage: place script and additional files (is_user_admin) into directory
;;;; where distribution is stored, then compile with NSIS (latest version
;;;; used was 2.31).
;;;;
;;;; Note: this script globs all files in the current directory.
;;;; -------------------------------------------------------------------------

;;; --------------------------------------------------------------------------
;;; General
;;; --------------------------------------------------------------------------

    !include MUI.nsh

    ;; compression method
    SetCompressor /SOLID LZMA
    SetCompressorDictSize 16 ; MB

    ;; http://nsis.sourceforge.net/IsUserAdmin
    !include "is_user_admin.nsh"

    ;; name and file
    !define VERSION "@VERSION@"
    !define NAME "@NAME@"
    !define LONGNAME "@LONGNAME@"
    !define DESCRIPTION "@DESCRIPTION@"
    !define URL "@URL@"

    ;; name and output of file
    Name "${LONGNAME} ${VERSION}"
    OutFile "${NAME}-${VERSION}-win32-install.exe"

    ;; default installation folder
    InstallDir "$PROGRAMFILES\${LONGNAME} ${VERSION}"

    ;; get installation folder from registry if available
    InstallDirRegKey HKCU "Software\${LONGNAME} ${VERSION}" ""

;;; --------------------------------------------------------------------------
;;; Variables
;;; --------------------------------------------------------------------------

    Var UserSetting
    Var STARTMENU_FOLDER
    Var STARTMENU_TEMP

;;; --------------------------------------------------------------------------
;;; Interface Configuration
;;; --------------------------------------------------------------------------

    !define MUI_ABORTWARNING

;;; --------------------------------------------------------------------------
;;; Installation Types
;;; --------------------------------------------------------------------------

InstType "Typical"

;;; --------------------------------------------------------------------------
;;; Page Definitions and Settings
;;; --------------------------------------------------------------------------

    ;;installer pages

    !define MUI_COMPONENTSPAGE_SMALLDESC

    !insertmacro MUI_PAGE_WELCOME
    !insertmacro MUI_PAGE_LICENSE "COPYRIGHT"
    !insertmacro MUI_PAGE_COMPONENTS
    !insertmacro MUI_PAGE_DIRECTORY

    !define MUI_STARTMENUPAGE_DEFAULTFOLDER "${LONGNAME} ${VERSION}"
    !define MUI_STARTMENUPAGE_REGISTRY_ROOT HKCU
    !define MUI_STARTMENUPAGE_REGISTRY_KEY "Software\${LONGNAME} ${VERSION}"
    !define MUI_STARTMENUPAGE_REGISTRY_VALUENAME "Start Menu Folder"
    !insertmacro MUI_PAGE_STARTMENU StartMenu $STARTMENU_FOLDER

    !insertmacro MUI_PAGE_INSTFILES

    ;; uninstaller pages
    !insertmacro MUI_UNPAGE_CONFIRM
    !insertmacro MUI_UNPAGE_INSTFILES

;;; --------------------------------------------------------------------------
;;; Language Files
;;; --------------------------------------------------------------------------

    !insertmacro MUI_LANGUAGE "English"

;;; --------------------------------------------------------------------------
;;; Installer Sections
;;; --------------------------------------------------------------------------

;; install for all users or just current user?
SectionGroup /e "!Install For"

    Section "All Users" SecAllUsers

        ;; set shell var context to all
        SetShellVarContext all

        ;; save in registry
        WriteRegStr HKCU "Software\${LONGNAME} ${VERSION}" "Shell Var Context" "all"

        ;; save env var context in registry
        WriteRegStr HKCU "Software\${LONGNAME} ${VERSION}" "Environment Context" "all"
    SectionEnd

    Section "Just Me" SecCurUser
    SectionIn 1
        ;; set shell var context to current
        SetShellVarContext current

        ;; save in registry
        WriteRegStr HKCU "Software\${LONGNAME} ${VERSION}" "Shell Var Context" "current"

        ;; save env var context in registry
        WriteRegStr HKCU "Software\${LONGNAME} ${VERSION}" "Environment Context" "current"
    SectionEnd

SectionGroupEnd

SectionGroup /e "!${LONGNAME} Core"

    Section "${LONGNAME} ${VERSION}" SecCore
    SectionIn 1 RO

        SetOutPath $INSTDIR

        ;; create files
        File /r /x "*.nsi" /x "*.nsh" /x "text2rtf.lisp" /x "COPYRIGHT.rtf" ".\"

        ;; store installation folder in registry
        WriteRegStr HKCU "Software\${LONGNAME} ${VERSION}" "" $INSTDIR

        ;; create uninstaller
        WriteUninstaller "uninstall.exe"
    SectionEnd

    Section "-Add/Remove Programs"

        ;; add uninstaller to add/remove programs
        StrCmp $UserSetting ${SecAllUsers} WriteRegStr_all
            WriteRegStr HKCU "Software\Microsoft\Windows\CurrentVersion\Uninstall\${LONGNAME} ${VERSION}" \
                "DisplayName" "${LONGNAME} ${VERSION}"
            WriteRegStr HKCU "Software\Microsoft\Windows\CurrentVersion\Uninstall\${LONGNAME} ${VERSION}" \
                "DisplayIcon" "%SystemRoot%\system32\SHELL32.dll,41"
            WriteRegStr HKCU "Software\Microsoft\Windows\CurrentVersion\Uninstall\${LONGNAME} ${VERSION}" \
                "DisplayVersion" "${VERSION}"
            WriteRegStr HKCU "Software\Microsoft\Windows\CurrentVersion\Uninstall\${LONGNAME} ${VERSION}" \
                "InstallLocation" "$INSTDIR"
            WriteRegStr HKCU "Software\Microsoft\Windows\CurrentVersion\Uninstall\${LONGNAME} ${VERSION}" \
                "UninstallString" "$INSTDIR\uninstall.exe"
            WriteRegStr HKCU "Software\Microsoft\Windows\CurrentVersion\Uninstall\${LONGNAME} ${VERSION}" \
                "URLInfoAbout" "${URL}"
            WriteRegStr HKCU "Software\Microsoft\Windows\CurrentVersion\Uninstall\${LONGNAME} ${VERSION}" \
                "URLUpdateInfo" "${URL}"
            WriteRegStr HKCU "Software\Microsoft\Windows\CurrentVersion\Uninstall\${LONGNAME} ${VERSION}" \
                "HelpLink" "${URL}"
            WriteRegDWord HKCU "Software\Microsoft\Windows\CurrentVersion\Uninstall\${LONGNAME} ${VERSION}" \
                "NoModify" 1
            WriteRegDWord HKCU "Software\Microsoft\Windows\CurrentVersion\Uninstall\${LONGNAME} ${VERSION}" \
                "NoRepair" 1
        Goto WriteRegStr_done
        WriteRegStr_all:
            WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${LONGNAME} ${VERSION}" \
                "DisplayName" "${LONGNAME} ${VERSION}"
            WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${LONGNAME} ${VERSION}" \
                "DisplayIcon" "%SystemRoot%\system32\SHELL32.dll,41"
            WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${LONGNAME} ${VERSION}" \
                "DisplayVersion" "${VERSION}"
            WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${LONGNAME} ${VERSION}" \
                "InstallLocation" "$INSTDIR"
            WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${LONGNAME} ${VERSION}" \
                "UninstallString" "$INSTDIR\uninstall.exe"
            WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${LONGNAME} ${VERSION}" \
                "URLInfoAbout" "${URL}"
            WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${LONGNAME} ${VERSION}" \
                "URLUpdateInfo" "${URL}"
            WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${LONGNAME} ${VERSION}" \
                "HelpLink" "${URL}"
            WriteRegDWord HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${LONGNAME} ${VERSION}" \
                "NoModify" 1
            WriteRegDWord HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${LONGNAME} ${VERSION}" \
                "NoRepair" 1
        WriteRegStr_done:
    SectionEnd

    Section "-Start Menu"

        ;; add start menu shortcuts
        !insertmacro MUI_STARTMENU_WRITE_BEGIN StartMenu
            CreateDirectory $SMPROGRAMS\$STARTMENU_FOLDER
            CreateShortCut "$SMPROGRAMS\$STARTMENU_FOLDER\${LONGNAME} ${VERSION}.lnk" @COMMAND@ "$INSTDIR\bt.ico"
            CreateShortCut "$SMPROGRAMS\$STARTMENU_FOLDER\Uninstall ${LONGNAME} ${VERSION}.lnk" "$INSTDIR\uninstall.exe"
        !insertmacro MUI_STARTMENU_WRITE_END
    SectionEnd

    Section "Desktop Shortcut" SecDesktop
    SectionIn 1

        ;; create shortcut
        createShortCut "$DESKTOP\${LONGNAME} ${VERSION}.lnk" @COMMAND@ "$INSTDIR\bt.ico"

        ;; record installation of desktop shortcut in registry
        WriteRegStr HKCU "Software\${LONGNAME} ${VERSION}" "Desktop Shortcut" "true"
    SectionEnd

    Section "File Associations" SecAssoc
    SectionIn 1

        ;; add file associations to registry
        WriteRegStr HKCR ".btg" "" "btgfile"

        WriteRegStr HKCR "btgfile" "" "Blackthorn Saved Game"
        WriteRegDWord HKCR "btgfile" "EditFlags" 0
        WriteRegStr HKCR "btgfile\DefaultIcon" "" "$INSTDIR\bt.ico"
        WriteRegStr HKCR "btgfile\Shell\Open" "" "Open with Blackthorn RPG"
        WriteRegStr HKCR "btgfile\Shell\Open\command" "" '@COMMAND@ -- -load "%1"'

        ;; record installation of file associations in registry
        WriteRegStr HKCU "Software\${LONGNAME} ${VERSION}" "File Associations" "true"
    SectionEnd
SectionGroupEnd

;;; --------------------------------------------------------------------------
;;; Component Descriptions
;;; --------------------------------------------------------------------------

    ;; language strings
    LangString DESC_SecAllUsers ${LANG_ENGLISH} "Install ${LONGNAME} ${VERSION} for all users. (Requires administrative privileges.)"
    LangString DESC_SecCurUser ${LANG_ENGLISH} "Install ${LONGNAME} ${VERSION} for current user only."
    LangString DESC_SecCore ${LANG_ENGLISH} "${DESCRIPTION}"
    LangString DESC_SecDesktop ${LANG_ENGLISH} "Create a desktop shortcut for ${LONGNAME}."
    LangString DESC_SecAssoc ${LANG_ENGLISH} "Associate ${LONGNAME} with files of type .btg."

    ;; assign language strings to sections
    !insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
        !insertmacro MUI_DESCRIPTION_TEXT ${SecAllUsers} $(DESC_SecAllUsers)
        !insertmacro MUI_DESCRIPTION_TEXT ${SecCurUser} $(DESC_SecCurUser)
        !insertmacro MUI_DESCRIPTION_TEXT ${SecCore} $(DESC_SecCore)
        !insertmacro MUI_DESCRIPTION_TEXT ${SecDesktop} $(DESC_SecDesktop)
        !insertmacro MUI_DESCRIPTION_TEXT ${SecAssoc} $(DESC_SecAssoc)
    !insertmacro MUI_FUNCTION_DESCRIPTION_END

;;; --------------------------------------------------------------------------
;;; Installer Functions
;;; --------------------------------------------------------------------------

Function .onInit

    StrCpy $UserSetting ${SecCurUser}

    ;; if the user does *not* have administrator privileges,
    ;; then make section SecAllUsers readonly
    Call IsUserAdmin
    Pop $0
    StrCmp $0 "true" onInit_cont 0
        ;; Set the fifth bit to set the read only flag.
        !define READ_ONLY 0x00000010
        SectionGetFlags ${SecAllUsers} $0
        IntOp $0 $0 | ${READ_ONLY}
        SectionSetFlags ${SecAllUsers} $0
        !undef READ_ONLY
    onInit_cont:

FunctionEnd

Function .onSelChange

    !insertmacro StartRadioButtons $UserSetting
        !insertmacro RadioButton ${SecAllUsers}
        !insertmacro RadioButton ${SecCurUser}
    !insertmacro EndRadioButtons

FunctionEnd

;;; --------------------------------------------------------------------------
;;; Uninstaller Sections
;;; --------------------------------------------------------------------------

Section "un.Install For"

    ;; get state of SetShellVarContext
    ReadRegStr $R0 HKCU "Software\${LONGNAME} ${VERSION}" "Shell Var Context"
    StrCmp $R0 "all" SetShellUserContext_all
        Goto SetShellUserContext_done
    SetShellUserContext_all:
        SetShellVarContext all
    SetShellUserContext_done:

SectionEnd

Section "Uninstall"

    Delete $INSTDIR\uninstall.exe

    ;; delete files
    RMDir /r $INSTDIR

    ;; delete uninstaller from add/remove programs
    ReadRegStr $R0 HKCU "SOFTWARE\${LONGNAME} ${VERSION}" "Environment Context"
    StrCmp $R0 "all" DeleteRegStr_all
        DeleteRegKey HKCU "Software\Microsoft\Windows\CurrentVersion\Uninstall\${LONGNAME} ${VERSION}"
    Goto DeleteRegStr_done
    DeleteRegStr_all:
        DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${LONGNAME} ${VERSION}"
    DeleteRegStr_done:

    ;; delete desktop shortcut if it was created by the installer
    ReadRegStr $R0 HKCU "Software\${LONGNAME} ${VERSION}" "Desktop Shortcut"
    StrCmp $R0 "true" deleteDesktopShortcut
        Goto doneDeletingDesktopShortcut
    deleteDesktopShortcut:
        Delete "$DESKTOP\${LONGNAME} ${VERSION}.lnk"
    doneDeletingDesktopShortcut:

    ;; delete file associations if they were created by the installer
    ReadRegStr $R0 HKCU "Software\${LONGNAME} ${VERSION}" "File Associations"
    StrCmp $R0 "true" DeleteFileAssociations
        Goto DeleteFileAssociations_done
    DeleteFileAssociations:
        DeleteRegKey HKCR ".btg"
        DeleteRegKey HKCR "btgfile"
    DeleteFileAssociations_done:

    ;; delete contents of start menu folder
    !insertmacro MUI_STARTMENU_GETFOLDER StartMenu $STARTMENU_TEMP

    Delete "$SMPROGRAMS\$STARTMENU_TEMP\${LONGNAME} ${VERSION}.lnk"
    Delete "$SMPROGRAMS\$STARTMENU_TEMP\Uninstall ${LONGNAME} ${VERSION}.lnk"

    ;; delete empty start menu parent diretories
    StrCpy $STARTMENU_TEMP "$SMPROGRAMS\$STARTMENU_TEMP"

    startMenuDeleteLoop:
        ClearErrors
        RMDir $STARTMENU_TEMP
        GetFullPathName $STARTMENU_TEMP "$STARTMENU_TEMP\.."

        IfErrors startMenuDeleteLoopDone

        StrCmp $STARTMENU_TEMP $SMPROGRAMS startMenuDeleteLoopDone startMenuDeleteLoop
    startMenuDeleteLoopDone:

    DeleteRegKey /ifempty HKCU "Software\${LONGNAME} ${VERSION}"
SectionEnd
