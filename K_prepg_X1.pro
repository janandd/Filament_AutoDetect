pro K_prepg_X1
;
;+
; Name         : K_prepg_X1
;
; Category     : pre-processing
;
; Purpose      : Apply limb darkening (LDCOR) and foreshortening (FSCOR)
;                corrections to full-disc H-alpha images taken from
;                any ground-based observatory
;
; Explanation  : correction for limb darkening:
;                A limb darkening correction mask is made for every
;                image, and then the image is divided by this mask.
;                The centre-to-limb variation (CLV) is obtained along
;                several theta directions on the disc. Its average is
;                then used to construct the mask.
;                ;
;                correction for foreshortening:
;                Method described in 2 papers listed below is employed:
;                Enger I. et al., 1966: Air Force Surv. Geophys. 178.
;                Ambastha A. & Bhatnagar A., 1988: J. Astrophys. Astron. 9, 137
;                ;                
;                One may choose to ignore the errors due to foreshortening,
;                or would like to have apparent size and location of
;                solar features. In such a case the portion where
;                K_area_prjC is called can be commented.
;
; Syntax       : This program is not to be run as a library program.
;                However, there need to be a few programs present
;                in the library which are written by ADJ.
;
; Inputs       : None
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
; Calls	       : K_limb_mskC - limb darkening correction
;                K_area_prjC - foreshortening correction
;
; Restrictions : - The dark and flat correction has to be
;                  applied to the input image separately.
;                - Assumes that input image is 1024x1024 in size.
;                  If size is different, it should be resized
;                  before any operation is performed.
;
; History      : ~ Jan-2009 first version written by Anand D Joshi
;              : ~ Nov-2013 after a few revisions, the current
;                           one is realised ... ADJ
;-
;
thap = '/media/anand/Akash/dataK/GrandDaddy/20150316'
cd, thap
;
srst = '*.fits.fz'
;
im = file_search(srst, count=ni)

if (ni eq 0) then message, 'NO ', + srst + ' files found in the specified folder'

print, format="('number of ', a, ' files in the specified folder = ', i03)", srst, ni
;
m0 = 25
m1 = ni
p = 2.0
t = 400
rdjn = findgen(t+1)/t

t0 = systim(1)
;
for i=m0,m1-1 do begin
  a0 = readfits(im[i], hd, /silent)
  sx = fxpar(hd, 'naxis1')					; if (sx ne sy) then
  sy = fxpar(hd, 'naxis2')					; add (or subtract) rows/columns
  ;
  xco = fxpar(hd, 'crpix1')				; use the correct header keyword
  yco = fxpar(hd, 'crpix2')				; use the correct header keyword
  r0o = fxpar(hd, 'radius')				; use the correct header keyword

  if (sx eq sy) then begin
    facxy = sx/1024									; assuming (sx ge 1024)
    facR0 = sy/facxy
  endif else begin
    message, 'add (or subtract) rows/columns to make sx = sy'
  endelse
  ;
  a1 = shift(a0, 512*facxy-xco, 512*facxy-yco)
  ;
  xc = 512.0
  yc = 512.0
  r0 = float(r0o)/facxy
  ;
  sxaddpar, hd, 'crpix1', xc
  sxaddpar, hd, 'crpix2', yc
  sxaddpar, hd, 'radius', r0

  if ((sx ne 1024) and ((sx mod 1024) eq 0)) then begin
    a2 = rebin(a1, 1024, 1024) 		; resize to 1024x1024
    ;
    sxaddpar, hd, 'naxis1', 1024
    sxaddpar, hd, 'naxis2', 1024

    dlt1 = fxpar(hd,'cdelt1')  &  dlt2 = fxpar(hd,'cdelt2')
    ;
    sxaddpar, hd, 'cdelt1', dlt1*facxy
    sxaddpar, hd, 'cdelt2', dlt1*facxy
  endif else begin
    a2 = a1
  endelse

;;  stop

;; *~*~*~*~*~*~*~*~*~*~ limb darkening correction ~*~*~*~*~*~*~*~*~*~*
;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*

  K_limb_mskC, a2, a3, p=p, t=t, xc=xc, yc=yc, r0=r0, /show

  a4 = a2/a3				; a3 is the mask showing average CLV for the disc
  ;
  tt = systim()
  sxaddpar, hd, 'COMMENT', $
  					strcompress('LIMB DARKENING correction appied on ' + tt)

;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
;; *~*~*~*~*~*~*~*~*~*~ limb darkening correction ~*~*~*~*~*~*~*~*~*~*


;; *~*~*~*~*~*~*~*~*~*~ foreshortening correction ~*~*~*~*~*~*~*~*~*~*
;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*

;;  K_area_prjC, a4, a5, xc=xc, yc=yc, r0=r0

;;  tt = systim()
;;  sxaddpar, hd, 'COMMENT', $
;;  					strcompress('FORESHORTENING correction appied on ' + tt)

;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
;; *~*~*~*~*~*~*~*~*~*~ foreshortening correction ~*~*~*~*~*~*~*~*~*~*


;; *~*~*~*~*~*~*~*~*~* uncomment to write FITS file ~*~*~*~*~*~*~*~*~*

;;;  writefits, str_replace(im[i],'.fts.gz','_2a.fts'), a5, hd

;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*

  print, systim(1)-t0
  stop

endfor
;
;
print, format="('total time taken, ttt = ', f8.4, ' min')", (systim(1)-t0)/60
stop
end

