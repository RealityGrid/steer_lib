# Microsoft Developer Studio Project File - Name="reg_steer_lib" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=reg_steer_lib - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "reg_steer_lib.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "reg_steer_lib.mak" CFG="reg_steer_lib - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "reg_steer_lib - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "reg_steer_lib - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "reg_steer_lib - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD CPP /nologo /MD /W3 /GX /O2 /I "./include" /I "./win32/include" /I ".\win32\rpc" /D "NDEBUG" /D "WIN32" /D "_MBCS" /D "_LIB" /D "REG_STEER_LIB_VERSION#\"1.2a\"" /YX /FD /c
# ADD BASE RSC /l 0x809 /d "NDEBUG"
# ADD RSC /l 0x809 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "reg_steer_lib - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c
# ADD CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /I "./include" /I "./win32/include" /I ".\win32\rpc" /D "_DEBUG" /D "WIN32" /D "_MBCS" /D "_LIB" /D "REG_STEER_LIB_VERSION#\"1.2a\"" /FR /YX /FD /GZ /c
# ADD BASE RSC /l 0x809 /d "_DEBUG"
# ADD RSC /l 0x809 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ENDIF 

# Begin Target

# Name "reg_steer_lib - Win32 Release"
# Name "reg_steer_lib - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=.\src\utils\c_types.c
# End Source File
# Begin Source File

SOURCE=.\src\ReG_Steer_Appside.c
# End Source File
# Begin Source File

SOURCE=.\src\ReG_Steer_Appside_File.c
# End Source File
# Begin Source File

SOURCE=.\src\ReG_Steer_Appside_Soap.c
# End Source File
# Begin Source File

SOURCE=.\src\ReG_Steer_Appside_Sockets.c
# End Source File
# Begin Source File

SOURCE=.\src\ReG_Steer_Browser.c
# End Source File
# Begin Source File

SOURCE=.\src\ReG_Steer_Common.c
# End Source File
# Begin Source File

SOURCE=.\src\ReG_Steer_Logging.c
# End Source File
# Begin Source File

SOURCE=.\src\ReG_Steer_Proxy_utils.c
# End Source File
# Begin Source File

SOURCE=.\src\ReG_Steer_Steerside.c
# End Source File
# Begin Source File

SOURCE=.\src\ReG_Steer_Steerside_Soap.c
# End Source File
# Begin Source File

SOURCE=.\src\ReG_Steer_XML.c
# End Source File
# Begin Source File

SOURCE=.\src\soapC.c
# End Source File
# Begin Source File

SOURCE=.\src\soapClient.c
# End Source File
# Begin Source File

SOURCE=.\src\stdsoap2.c
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=.\include\ReG_Steer_Appside.h
# End Source File
# Begin Source File

SOURCE=.\include\ReG_Steer_Appside_File.h
# End Source File
# Begin Source File

SOURCE=.\include\ReG_Steer_Appside_Globus.h
# End Source File
# Begin Source File

SOURCE=.\include\ReG_Steer_Appside_internal.h
# End Source File
# Begin Source File

SOURCE=.\include\ReG_Steer_Appside_Soap.h
# End Source File
# Begin Source File

SOURCE=.\include\ReG_Steer_Appside_Sockets.h
# End Source File
# Begin Source File

SOURCE=.\include\ReG_Steer_Browser.h
# End Source File
# Begin Source File

SOURCE=.\include\ReG_Steer_Common.h
# End Source File
# Begin Source File

SOURCE=.\include\ReG_Steer_Logging.h
# End Source File
# Begin Source File

SOURCE=.\include\ReG_Steer_Proxy_utils.h
# End Source File
# Begin Source File

SOURCE=.\include\ReG_Steer_Steerside.h
# End Source File
# Begin Source File

SOURCE=.\include\ReG_Steer_Steerside_internal.h
# End Source File
# Begin Source File

SOURCE=.\include\ReG_Steer_Steerside_Soap.h
# End Source File
# Begin Source File

SOURCE=.\include\ReG_Steer_types.h
# End Source File
# Begin Source File

SOURCE=.\include\ReG_Steer_XML.h
# End Source File
# Begin Source File

SOURCE=.\include\soapH.h
# End Source File
# Begin Source File

SOURCE=.\include\soapStub.h
# End Source File
# Begin Source File

SOURCE=.\include\stdsoap2.h
# End Source File
# End Group
# End Target
# End Project
