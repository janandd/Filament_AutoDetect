pro K_limb_mskC, aa0, aa1, p=p, t=t, show=show, $
				xc=xc, yc=yc, r0=r0
;
;+
; Name         : limb_masC
;
; Category     : image processing
;
; Purpose      : generate the limb darkening mask
;
; Explanation  : the program obtains the average centre
;                to limb variation of the full disc image,
;                fits a 5th order polynomial to it, and
;                generates a mask for limb darkening
;                correction. one can multiply an image with
;                this mask to remove limb darkening
;
; Syntax       : IDL> limb_masC, aa0, p=p, t=t, aa2, xc=xc, yc=yc, r0=r0
;
; Inputs       : aa0 - input image with darkened limbs
;                xc - x coord centre of disc
;                yc - y coord centre of disc
;                r0 - radius of disc
;                (Wanted to have xc, yc, and r0 as Optional Inputs.
;                However, the program, EDGDETC, for detection of
;                edge of solar disc and determining radius and disc
;                centre from it does not work very well.)
;
; Optional
; Inputs       : p - index to tell if accuracy of 1 deg (p = 1.0)
;                    is desired or half a degree (p = 2.0)
;                t - index to tell if radius to be divided into
;                    200 parts or 400
;
; Outputs      : aa1 - output image with limb darkening removed
;
; Optional
; Outputs      : None
;
; Keywords     : show - display the average centre to limb variation
;                       along with the fitted polynomial
;
; Common       : None
;
; Calls        : edgdetC, car2polC
;
; Restrictions : None
;
; History      : ~ Jan-2009 written by Anand D. Joshi
;              : ~ Nov-2013 after a few revisions, the current
;                           one is realised ... ADJ
;-
;
aa0 = float(aa0)

if ~n_elements(p) then begin
  p = 1.0
endif else if n_elements(p) then begin
  if ((p ne 1.0) and (p ne 2.0)) then p = 1.0
endif
;
if ~n_elements(t) then begin
  t = 200
endif else if n_elements(t) then begin
  if ((t ne 200) and (t ne 400)) then t = 200
endif

if (~n_elements(xc) and ~n_elements(yc) and ~n_elements(r0)) then $
			edgdetC, aa0, xc, yc, r0					;, /show

car2polC, aa0, stck, pola0, p=p, t=t, xc=xc, yc=yc, r0=r0

rmas = pola0[*,*,0]                       	; mask of the radius
rdjn = findgen(t+1)/t                     	; radius for 0 to 1 split into 400 parts
stcl = total(stck, 2)/(360*p+1)

stcl[0] = mean(stcl[1:10])									; jabardasti

q = 5
vrs = fltarr(n_elements(rdjn))
rdst = poly_fit(rdjn[0:0.95*t], stcl[0:0.95*t], q)
for i=0,q do vrs = vrs + rdst[i]*rdjn^i
if (keyword_set(show)) then begin

  a = findgen(4)*2*!pi/3
  usersym, cos(a), sin(a), /fill

  loadct, 0, /sile
  window, 3, xs=500, ys=500, xp=550, yp=200
  plot, [0,0], xr=[-0.05,1.05], xst=1, $
  					yr=plotrange(stcl,vrs), yst=1, color=fsc_color('ivory')
  oplot, rdjn, stcl, psym=8, color=fsc_color('green')	;, symsize=25.0
  oplot, rdjn, vrs, linest=2, color=fsc_color('chocolate'), thick=4.0
endif
;
aa1 = fltarr(1024,1024)
for j=0,1023 do begin
  for k=0,1023 do begin
    jmxc = j - xc
    kmyc = k - yc
    rr = sqrt(jmxc^2 + kmyc^2)
    rt = rr/r0
    tht = atan(kmyc, jmxc)*!radeg
    if (tht lt 0.0) then tht = 360.0 + tht

    if (rt gt 1.0) then begin
      aa1[j,k] = -1.0
    endif

    rsnd = 1.0 - rmas[j,k]
    rdcl = min(abs(rdjn-rsnd), jim)
    aa1[j,k] = vrs[jim]
  endfor
endfor


end
