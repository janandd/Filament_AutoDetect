pro car2polC, a0, a0st, pola0, p=p, t=t, $
					xc=xc, yc=yc, r0=r0, show=show
;
;+
; Name         : car2polC
;
; Category     : image processing
;
; Purpose      : generate a rectangular-polar image from
;                Cartesian image of the solar disc.
;
; Explanation	 : generates a 3 dimensional variable of the form
;                1024 x 1024 x 2, where the 3rd dimension contains
;                fractional radial distance from centre of image
;                and angle from West direction measured in the
;                anitclockwise sense, i.e., the program gives you
;                the polar coordinates of all points on the image.
;
; Syntax       : car2polC, a0, a0st
;
; Inputs       : a0 - input image in regular Cartesian coords
;
; Optional
; Inputs       : p - index to tell if accuracy of 1 deg (p = 1.0)
;                    is needed or half a degree (p = 2.0)
;                t - index to tell if radius to be divided into
;                    200 parts or 400
;                xc - x coord centre of disc
;                yc - y coord centre of disc
;                r0 - radius of disc
;
; Outputs      : a0st - output image in rectangular polar coords
;
; Optional
; Outputs      : pola0 -
;
; Keywords     : show - display the centre-to-limb variation and the
;                       rectangular [r-theta] map of the solar disc
;
; Common       :
;
; Calls        : edgdetC, reverse, fsc_color, poly_fit, wstsh
;
; Restrictions : input image should be square (i.e. naxis1 = naxis2)
;
; History      : 21-Apr-2009 ... written by Anand D. Joshi
;                05-Jul-2017 ... added keyword SHOW
;-
;
pola0 = fltarr(1024,1024,2)
;

if (~n_elements(xc) and ~n_elements(yc) and ~n_elements(r0)) then $
					edgdetC, aa0, xc, yc, r0, show=keyword_set(show)

if (xc ne 512 or yc ne 512) then begin
  a0 = shift(a0, 512-xc, 512-yc)
  xc = float(512)
  yc = float(512)
  r0 = float(r0)				; - 1
endif
;
for j=0,1023 do begin
  for k=0,1023 do begin
    jmxc = j - xc
    kmyc = k - yc
    rr = sqrt(jmxc^2 + kmyc^2)                        ; radial distance (pixels)
    rt = rr/r0                                        ; radial distance as a fraction of solar disc
    tht = atan(kmyc, jmxc)*!radeg
    if (tht lt 0.0) then tht = 360.0 + tht
    ;
    if (rt gt 1.0) then begin
      pola0[j,k,0] = -1.0
      pola0[j,k,1] = -1.0
;      continue
    endif else begin
      pola0[j,k,0] = sqrt(1 - rt^2)
      pola0[j,k,1] = tht
    endelse
  endfor
endfor
;
;tvscl, pola0[*,*,0]
;tvscl, pola0[*,*,1]
;
;
;

if (~n_elements(p)) then begin
  p = 1.0
endif else if (n_elements(p)) then begin
  if ((p ne 1.0) and (p ne 2.0)) then p = 1.0
endif
;
if (~n_elements(t)) then begin
  t = 200
endif else if (n_elements(t)) then begin
  if ((t ne 200) and (t ne 400)) then t = 200
endif

th = round(pola0[*,*,1]*p)/p
;
a0st = fltarr(t+1,360*p+1)
rdjn = findgen(t+1)/t
rmas = pola0[*,*,0]
;
for j=0,360*p do begin
  thz = where(th eq j/p)
  xtz = thz mod 1024
  ytz = thz/1024
  ;
  nxz = min(xtz, max=xxz)
  nyz = min(ytz, max=xyz)
  ;
  if (abs(nxz-512) gt abs(xxz-512)) then lxz = nxz $
          else lxz = xxz
  if (abs(nyz-512) gt abs(xyz-512)) then lyz = nyz $
          else lyz = xyz

  line_coord, 512, 512, lxz, lyz, xtz2, ytz2, nz2

  csl = rmas[xtz2,ytz2]

  ks = 0		;t/20

  for k=ks,t do begin
    rv = 1.0*k/t
    mncl = min(abs(csl-rv), rf)
    a0st[k,j] = a0[xtz2[rf],ytz2[rf]]
  endfor
  ;
endfor
;
a0st = reverse(a0st, 1)
;;;wdef, 19, t+1, 360*p+1, xp=100, yp=70
wstsh, 19
tvscl, a0st


;if ~keyword_set(show) then begin
;  wait, 0.2
;  wdel, 2, 3
;endif

;
;
;
end