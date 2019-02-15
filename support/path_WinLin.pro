function path_WinLin, path
;
;+
; Name         : path_WinLin
;
; Category     : folders
;
; Purpose      : Use single path for Windows & Unix programs
;
; Explanation  : Often, when switching between Windows & unix OS's.
;                we have to manually change the path. This program
;                would take care of it.
;
; Syntax       : IDL> outpath = path_WinLin(path)
;
; Inputs       : path - a valid path to a Windows or Linux folder
;
; Optional
; Inputs       : None
;
; Outputs      : None
;
; Optional
; Outputs      : None
;
; Keywords     : None
;
; Common       : None
;
; Calls	       : None
;
; Restrictions : None
;
; History      : 14 Sep 2015 ... written by Anand D. Joshi
;-
;
OSfam = strlowcase(!version.os_family)

win = 0  &  lin = 0
if (strmid(path,0,1) eq '/') then lin = 1 else win = 1

if (OSfam eq 'windows') then begin
  if (win eq 1) then begin
    return, path
  endif else begin
    qath = strmid(path,13)
    qspl = strsplit(qath, '/', /extra, coun=nq)


    qath = strmid(qspl[0],0,1) + ':\' + strjoin(qspl[1:nq-1],'\')

    return, qath
  endelse

endif else if (OSfam eq 'unix') then begin
  if (lin eq 1) then begin
    return, path
  endif else begin
    qspl = strsplit(path, '\', /extra, coun=nq)
    ;
    case strmid(qspl[0],0,1) of
      'A': drive = 'Akash'					; 'Ahead'
      'B': drive = 'Queen'					; 'Behind'
      'C': drive = 'Chari'					; 'Corona'
      'E': drive = 'Espree'
      'J': drive = 'joker'
      'K': drive = 'knight'
      else: message, 'no appropriate drive found'
    endcase


    qath = '/media/anand/' + drive + '/' + strjoin(qspl[1:nq-1],'/')

    return, qath
	endelse

endif


end