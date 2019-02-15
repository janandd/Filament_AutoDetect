pro K_area_prjC, ar0, ar1, xc=xc, yc=yc, r0=r0
;
;+
; Name         : AREA_PROJC
;
; Category     : Pre-processing full-disc H-alpha images
;
; Purpose      : correct foreshortening in solar images
;
; Explanation  : after dark and flat corrections are applied,
;                this program corrects for the foreshortening.
;                algorithm given in Shih & Kowalski 2003
;                there are basically two things to be carried out:
;                a) find position of a small region from input image
;                   in the new image
;                b) find size of the region in the new image
;                   the program does the two things mentioned above
;                   and constructs the corrected image
;                The output image is also sometimes called as the
;                equal area projected image
;
; Syntax       : IDL> area_projC, ar0, ar1, xc=xc, yc=yc, r0=r0
;
; Inputs       : ar0 - input image with foreshortening
;              : xc - x coord centre of disc (pixels)
;                yc - y coord centre of disc (pixels)
;                r0 - radius of disc (pixels)
;                (Wanted to have xc, yc, and r0 as Optional Inputs.
;                However, the program, EDGDETC, for detection of
;                edge of solar disc and determining radius and disc
;                centre from it does not work very well.)
;
; Optional
; Inputs       : None
;
; Outputs      : ar1 - image with foreshortening removed
;
; Optional
; Outputs      : None
;
; Keywords     : None
;
; Common       : None
;
; Calls        : edgdetC
;
; Restrictions : - Assumes 1024x1024 images
;
; History      : ~ Jan-2009 written by Anand D. Joshi
;              : ~ Nov-2013 after a few revisions, the current
;                            one is realised ... ADJ
;-
;
ar1 = fltarr(1024,1024)
ar0 = float(ar0)

if (~n_elements(xc) and ~n_elements(yc) and ~n_elements(r0)) then $
			edgdetC, aa0, xc, yc, r0					;, /show

if (xc ne 512 or yc ne 512) then begin
  ar0 = shift(ar0, 512-xc, 512-yc)
  xc = 512.0
  yc = 512.0
endif
;
for j=0,1023 do begin
  for k=0,1023 do begin
    jmxc = j - xc
    kmyc = k - yc
    rr = sqrt(jmxc^2 + kmyc^2)                        	; radial distance
    rt = rr/r0                                        	; radial distance as a fraction of solar disc
    ;
    if (rt le 0.5) then gs = 12 $
          else if (rt le 0.65) then gs = 6 $
          else gs = 2                                 	; fixing the grid size

    if ((((j+1) mod gs) ne 0) or (((k+1) mod gs) ne 0)) $
              then CONTINUE

    if (rt le 1.0) then begin														; leaving out 20 pixels at rt = 0.9486
      if (rt le 0.5) then ga = 144. $
              else if (rt le 0.65) then ga = 36. $
              else ga = 4                             	; fixing the grid area
      ;
      tht = atan(kmyc, jmxc)
      ;
      rp = r0*sqrt(1 - sqrt(1 - rt^2))
      jp = rp*cos(tht) + xc
      kp = rp*sin(tht) + yc
      ;
      gap = ga/(0.2*rt + sqrt(1 - rt^2))
      gsp = round(sqrt(gap))
      gsb2 = gs/2
      gspb2 = gsp/2.
      ;
      cng = congrid(ar0[j-gsb2:j+gsb2,k-gsb2:k+gsb2], $
      					gsp, gsp, cubic=-0.5)    ;, /interp)
      ;
      scng = size(cng, /dimen)
      if ((scng[0] mod 2) eq 0) then adx=1 else adx=0
      if ((scng[1] mod 2) eq 0) then ady=1 else ady=0
      scx = scng[0]/2
      scy = scng[1]/2

      ar1[jp-scx+adx:jp+scx,kp-scy+ady:kp+scy] = cng

    endif
  endfor
endfor


mdcar1 = median(ar0[xc-20:xc+20,yc-20:yc+20])
mnxar1 = min(ar0[xc-100:xc+100,yc-100:yc+100])
for j=0,1023 do begin
  for k=0,1023 do begin
    jmxc = j - xc
    kmyc = k - yc
    rr = sqrt(jmxc^2 + kmyc^2)						; radial distance
    rt = rr/r0														; radial distance as a fraction of solar disc
    ;
    if (rt le 0.90) then begin						; not counting scaled 20 pixels at rt = 0.82
      if (ar1[j,k] eq 0.) then begin
        if ((ar1[j-2,k] ne 0.) and (ar1[j-1,k] ne 0.) and $
        		(ar1[j+1,k] ne 0.) and (ar1[j+2,k] ne 0.)) then begin
          ar1[j,k] = (ar1[j-2,k] + ar1[j-1,k] + ar1[j+1,k] + ar1[j+2,k])/4.
        endif else if ((ar1[j,k-2] ne 0.) and (ar1[j,k-1] ne 0.) and $
        		(ar1[j,k+1] ne 0.) and (ar1[j,k+2] ne 0.)) then begin
          ar1[j,k] = (ar1[j,k-2] + ar1[j,k-1] + ar1[j,k+1] + ar1[j,k+2])/4.
        endif else begin
          ar1[j,k] = mdcar1
        endelse
      endif
    endif else if (rt le 1.0) then begin
      ar1[j,k] = mdcar1
    endif $
     			else begin											;
      ar1[j,k] = 0.0											; introduced on 17 Sep 09
    endelse																;
  endfor
endfor
;
;
;



end
