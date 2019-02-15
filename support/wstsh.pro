pro wstsh, w
;
;+
; Name		  	: WSTSH
;
; Category		: user utility
;
; Purpose		  : set and show the specified window
;
; Explanation : instead of separately using WSET and
;				        WSHOW, use this procedure
;
; Syntax	  	: IDL> wstsh, w
;
; Inputs	  	: w - index of window to be brought to the front
;
; Outputs	  	: None
;
; Optional
; Outputs	  	: None
;
; Keywords	  : None
;
; Common		  : None
;
; Calls			  : WSET, WSHOW
;
; Restrictions: window w has to exist
;
; History		  : - dd-Mar-2009 written by Anand D. Joshi
;               - dd-Jun-2009 included condition where no window is open   ADJ
;               - 28-Dec-2015 checking if window index provided is
;				                      indeed open ... ADJ 
;                18-Jan-2016 - issue error message if window is
;                              not defined ... ADJ
;-
;
on_error, 2

device, window=wdow
winops = byte(where(wdow ne 0, nwin))

if (n_params() eq 0) then $
					message, 'enter an appropriate window number'

;;;if (!d.window eq -1) then begin
if (where(winops eq w) eq -1) then $
					message, string('window ' + trim(w) + ' NOT open')
;;;  print, 'E   WSTSH: window ', trim(w), ' NOT open', form='(a,a,a)'
;;;  RETURN
;;;endif


wset, w
wshow, w



end