pro K_autodet_X2
;
;+
; Name         : J_jaman_X2
;
; Category     : image segmentation and feature recognition
;
; Purpose      : to detect and track solar filaments in full-disc
;                H-alpha images
;
; Explanation  : This program is lifted from K_jaman_X2.pro located at
;                /usr/local/exelis/idl85/lib/Anand/filame/jaMan
;                However, we would change it for the statistical study
;                of at least 50 erupting/disappearing filaments.
;                Thus, K_prepG_x1.pro located in the same directory
;                would be now included in this program only.
;                We plan to skip the foreshortening correction. ... Apr 2017
;
;                Can be used on full-disc H-alpha images
;                from any observatory. There are just a few header
;                keywords that need to be edited for the program to work.
;                That is because of a lack of uniform headers across the 
;                ground-based observatories.
;                
;                The images have to be pre-processed before this program
;                can be applied. Pre-processing involves flat and dark
;                correction. Additionally, we have also used corrections
;                for limb darkening (LDcor), foreshortening (FSCOR) and
;                varying contrast (CScor).
;                LDcor: removes the centre-to-limb variation present in
;                H-alpha images. Since the program employs local and not
;                global threshold, this need not be extremely important,
;                but it gives better results.
;                FSCOR: corrects the shortening of lengths of solar features
;                due to projection away from the disc centre. Lengths of all
;                filaments after FScor are proportional to their true lengths.
;                CScor: stands for contrast-stretching. This corrects for
;                varying intensity levels across the day. As a results of
;                CScor, intensity of all images lies between 0 and 1.
;                All these three pre-processing steps are present in a
;                separate program, named LIMB_DA_BB.
;
;                There are 2 steps to identify and track filaments
;                START: the 1st step. Fragments separated from each other
;                by less than a certain distance are assigned a label.
;                The labels are given in descending order of size of the
;                largest fragment.
;                FINAL: from the 2nd image onwards. Filament labels are
;                compared to the previous ones, and labels are re-assigned
;                if necessary.
;                ULTIMA: also applied from the 2nd image onwards.
;                Often you will find no difference between FINAL and
;                ULTIMA versions, but it is needed in some cases.
;
;                Details of the technique can be found in:
;                Joshi et al. 2010: Solar Physics, 262, 425
;
; Syntax       : This program is not to be run as a library program.
;                However, there need to be a few programs present
;                in the library which are written by ADJ.
;
; Variables    : The user interacts with the program through the 
;                variables listed below. They can be midified to
;                obtain suit the needs.
;                thap - thap to the full-disc H-alpha fits files
;                srst - search string to search for the desired files
;                glim - no of filaments for whom output files are to be
;                       written. for large filaments, typically getting
;                       output for up to 5 would be enough.
;                ch - =n --> click to proceed at every nth image
;                     =0 --> click only 1st image, rest is just breeze
;                m0 - the image to start with (default=0)
;                m1 - the last image (default=last image in folder)
;                pix40 - maximum separation allowed between two
;                        adjacent fragments in a filament
;                        (Grouping Criterion)
;                pix15 - maximum separation allowed between two
;                        filaments with same labels in consecutive images
;                        (Labelling Criterion)
;                dt - prefix for output text file for each 
;                sx - X size of image (=1024)
;                sy - Y size of image (=1024)
;
;                Apart from the ones above, following is a list of
;                variables that may help in understanding the program:
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
; Common       : JUMBO
;
; Calls	       : These routines are included after $MAIN$
;                threshM - apply local threshold to extract filaments
;                pureFil - filter tiny regions detected by threshold
;                Amnmxmd - extract min, max, median of (x,y)
;                johnnyG - label fragments using Grouping Criterion
;                ifferfi - condition to compare fragments
;                roaster - summary of each filament for output files
;                paintif - draw and colour all fragments
;                avgbrig - extract average intensity of each fragment
;                
;                These functions, built by ADJ, are also needed
;                wstsh - combine WSET and WSHOW for a window
;                dj_gettime - extract time in hours from
;
;                             (YYYY-MM-DDT)HH:MM:SS format
; Restrictions : - built for 1024x1024 images. But with a little
;                  tweaking can be used for any size. 
;
; History      :   -Jan-2009 first version written by Anand D Joshi
;              : ~~-Nov-2013 after numerous revisions, the current
;                            one is realised ... ADJ
;              :   -Apr-2017 to join K_prepG_X1 with this one ... ADJ
;              : 26-Oct-2017 inserted condition wherein if no filament
;                            is identified in PURE_A2, no further
;                            processing will be done, and the concerned
;                            image will be skipped ... ADJ
;-
;
common JUMBO, sx, sy, fscol, pix40, pix15, rad, xc, yc


thap = path_WinLin('/media/anand/Akash/dataK/GrandDaddy/NonErupt/20140730')
cd, thap  &  print, thap
im = file_search('*.fits.fz', count=ni)

m0 = 0							; start from this image number
m1 = ni   					; end with this image number
;
wwf = 0             ; set this flag to 1 iff you want to WDEF windows 5, 6 & 8

p = 2.0  &  t = 400
rdjn = findgen(t+1)/t


;; *~*~*~*~*~*~*~*~*~* files for writing output into *~*~*~*~*~*~*~*~*~*

close, /all, /force
dt = 'R'								; suffix for output file name
;;filtyp = '_2a'				; pre-processed images, hence prefix _2a
;
glim = -1									; no of filaments for which output is desired
for g=1,glim do begin
;;  filnam = strcompress(dt+'_f'+trim(g,'(i02)')+filtyp+'.txt', /remov)
  filnam = strcompress(dt+'_f'+trim(g,'(i02)')+'.txt', /remov)
  openw, 3*g, filnam, /append
  printf, 3*g, strcompress('### ' + systim())

  print, filnam
endfor

wimplo = 'j'
;
if (wimplo eq 'jpg') then begin
  iSav = concat_dir(thap, 'imgU')
  if ~dir_exist(iSav) then file_mkdir, iSav
endif

;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*

;;;
wdef, 3, 500, 500  &  wdef, 19, 201, 361


;; *~*~*~*~*~*~*~*~*~*~*~* read the first image ~*~*~*~*~*~*~*~*~*~*~*~*

J_prepG, im[m0], a2, a4, hda
;;;a4 = bytscl(a4, min=0.8, max=1.2)

;;;a1 = readfits(im[m0], hda, /silent)
tmC = fxpar(hda, 'date-obs')
;
dtQ = strmid(tmC, 0, 10)
tmQ = dj_gettime(tmC)
tmZ = strmid(tmC,0,4) + ' ' + themonths(strmid(tmC,5,2),/abbre) + $
					' ' + strmid(tmC,8,2) + '  ' + strmid(tmC,11,8)


;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*


;; *~*~*~*~*~*~*~*~*~*~ extract imp header keywords ~*~*~*~*~*~*~*~*~*~*

sx = nint(fxpar(hda, 'naxis1'))					; /2.0
sy = nint(fxpar(hda, 'naxis2'))					; /2.0
;
xc = fxpar(hda, 'crpix1')										; X centre of solar disc (pixels) /2.0
yc = fxpar(hda, 'crpix2')										; Y centre of solar disc (pixels) /2.0
;;rad = fxpar(hda,'R0')/fxpar(hda,'cdelt1')	; radius solar disc (pixels) /2.0
rad = fxpar(hda, 'radius')									; radius solar disc (pixels) /2.0
;;rad = 900.0/2
;dlt12 = fxpar(hda, 'cdelt1')*2

;
sun, 2016, 03, 02, 12.0, sd=sd
dlt12 = sd/rad


pix40 = nint(40*dlt12/2)					;; 40
pix15 = nint(15*dlt12/2)					;; 15


;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
;
;~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*


;; *~*~*~*~*~*~*~*~*~*~ declare other variables, etc *~*~*~*~*~*~*~*~*~*

fscol = ['moccasin', 'tomato', 'violet', 'chocolate', 'red', 'cyan', $
					'seagreen', 'skyblue', 'blue', 'yellow', 'khaki', $
					'royalblue', 'saddlebrown', 'magenta', 'hotpink', 'chocolate', $
					'orange', 'dodgerblue', 'green', 'maroon', 'papaya ' , $
					'lawngreen', 'cyan', 'darkkhaki', 'sienna', 'coral', $
					'plum', 'darkorchid', 'deeppink', 'saddlebrown', $
					'powderblue', 'olive', 'Indianred', 'beige', 'rose', $
					'gold', 'salmon', 'lightgray', 'lightcoral', 'burlywood', $
					'turquoise', 'coral', 'lightcyan', 'wheat', 'limegreen']


if (wwf eq 1) then begin
  wdef, 8, sx, sy, xp=245, yp=-50, title='8 COLOURFILL ultima'
  wdef, 6, sx, sy, xp=125, yp=-50, title='6 COLOURFILL final'
  wdef, 5, sx, sy, xp=005, yp=-50, title='5 COLOURFILL start'
endif


ch = 0							; ch=n --> you are asked to click to proceed at every
										;          nth image
										; ch=0 --> click only 1st image, rest is just breeze
!mouse.button = 1		; to reset mouse clicks from previous run

;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*


;; *~*~*~*~*~*~*~*~*~*~* define mask for solar disc ~*~*~*~*~*~*~*~*~*~*

wstsh, 8  &  erase
tvcircle, 0.88*rad, xc, yc, /devi, /fill
wcirc = tvrd()

noise_fac = 15							; ignore regions below this size threshold

;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*


;; *~*~*~*~*~*~*~*~*~*~*~ start with image no = m0 *~*~*~*~*~*~*~*~*~*~*

threshM, a4, thr_a1

filan_a1 = wcirc*(1 - thr_a1)

pureFil, filan_a1, noise_fac, pure_a1

if ((xc ne 512) or (yc ne 512)) then begin			; shift disc centre to
  pure_a1 = shift(pure_a1, 512-xc, 512-yc)			; centre of image
  xc = 512.0
  yc = 512.0
endif

lr1 = label_region(pure_a1)
mxroi1 = max(lr1)
sh1 = lonarr(13, mxroi1) 		; selecting max(lr1)-column and 15-row 2D array.
;
for i=1,mxroi1 do begin
  ds = where(lr1 eq i, cn)
  ;
  sh1[1,i-1] = cn
  sh1[2,i-1] = min(ds)					; sh1 is the list for the base image
  sh1[3,i-1] = max(ds)					; and it is constant for the whole program
  sh1[4,i-1] = sh1[2,i-1] mod sx
  sh1[5,i-1] = sh1[2,i-1]/sx
  sh1[6,i-1] = sh1[3,i-1] mod sx
  sh1[7,i-1] = sh1[3,i-1]/sx
  sh1[8,i-1] = median(ds)
  sh1[9,i-1] = sh1[8,i-1] mod sx
  sh1[10,i-1] = sh1[8,i-1]/sx
  sh1[11,i-1] = i
endfor
;
sorlis = reverse(sort(reform(sh1[1,*])))			; for arranging terms
sh1 = sh1[*,sorlis]													; in descending order


johnnyG, sh1, lr1

shOne = sh1   				; sh1 it is used for the first image only

shPre = shOne


wstsh, 8  &  erase, fsc_color('white')
;
tvcircle, rad, xc, yc, /devi, color=fsc_color('black'), thick=2.0
xyouts, 30, 850, (trim(m0)+' ultima'), /devi, $
						chars=2.0, chart=2.0, color=fsc_color('black')
xyouts, 30, 30, tmZ, /devi, charsi=2.0, charth=2.0, colo=fsc_color('black')

paintif, sh1, lr1

if (wimplo eq 'jpg') then begin
	Z0 = tvrd(true=1)
	
  Vnam = str_replace(im[m0], 'fits.fz', 'jpg')
  Vman = concat_dir(iSav, Vnam)
  ;
  write_jpeg, Vman, Z0, true=1, qual=100
endif

;;;print, 'sh1 OF FILAMENTS :: the BASE list'
;;;for i=0,n_elements(shOne[0,*])-1 do print, shOne[*,i], $
;;;					format='(2i6,2i11,4i7,i11,2i7,2i6)'
;;;print, ''

if (ch ne 0) then cursor, x, y, wait, /down
if (!mouse.button eq 4) then begin
  for g=1,glim do printf, 3*g, ''
  close, /all, /force
  stop
endif


mxstor1 = max(sh1[0,*])
filamentlist1 = strarr(7,mxstor1+1)

roaster, sh1, lr1, filamentlist1

gupto = (n_elements(filamentlist1[0,*])-1)<glim
for g=1,gupto do $
					printf, 3*g, m0, dtQ, tmQ, filamentlist1[1,g], $
					filamentlist1[3,g], filamentlist1[4,g], $
					filamentlist1[5,g], filamentlist1[6,g], $
					format='(i4,a14,f10.4,i8,4i6)'

;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*


;; *~*~*~*~*~*~*~*~*~*~ second time start from here ~*~*~*~*~*~*~*~*~*~*
;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*


t0 = systim(1)
;
for Ooty=m0+1,m1-1 do begin
  print, 'image number = ', Ooty, form='(a,i-4)'

  J_prepG, im[Ooty], b2, b4, hdb, Jcon

  if (Jcon eq 1) then CONTINUE

;;;  a2 = readfits(im[Ooty], hda, /silent)
  tmC = fxpar(hdb, 'date-obs')
  ;
  dtQ = strmid(tmC, 0, 10)
  tmQ = dj_gettime(tmC)
  tmZ = strmid(tmC,0,4) + ' ' + themonths(strmid(tmC,5,2),/abbre) + $
  					' ' + strmid(tmC,8,2) + '  ' + strmid(tmC,11,8)

  xc = fxpar(hda, 'crpix1')									; X centre of solar disc (pixels) /2.0
  yc = fxpar(hda, 'crpix2')									; Y centre of solar disc (pixels) /2.0
;;  rad = fxpar(hda,'R0')/fxpar(hda,'cdelt1')	; radius solar disc (pixels) /2.0
  rad = fxpar(hda, 'radius')								; radius solar disc (pixels) /2.0
;;  rad = 900.0/2

  loadZ0
  threshM, b4, thr_a2


  filan_a2 = wcirc*(1 - thr_a2)
	
  pureFil, filan_a2, noise_fac, pure_a2


  if ((xc ne 512) or (yc ne 512)) then begin
    pure_a2 = shift(pure_a2, 512-xc, 512-yc)
    xc = 512.0
    yc = 512.0
  endif


  lr2 = label_region(pure_a2)
  mxroi2 = max(lr2)

  ; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
  if (mxroi2 eq 0) then begin
    for g=1,gupto do printf, 3*g, '###', dtQ, tmQ, 0, 0, 0, 0, 0, $
    					format='(a3,1x,a14,f10.4,i8,4i6)'
    CONTINUE
  endif
  ; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*

  sh2 = lonarr(13, mxroi2) 		; selecting 13 column and 15 row MATRIX.
  ;
  for i=1,mxroi2 do begin
    ds = where(lr2 eq i, cn)
    ;
    sh2[1,i-1] = cn
    sh2[2,i-1] = min(ds)
    sh2[3,i-1] = max(ds)
    sh2[4,i-1] = sh2[2,i-1] mod sx
    sh2[5,i-1] = sh2[2,i-1]/sx
    sh2[6,i-1] = sh2[3,i-1] mod sx
    sh2[7,i-1] = sh2[3,i-1]/sx
    sh2[8,i-1] = median(ds)
    sh2[9,i-1] = sh2[8,i-1] mod sx
    sh2[10,i-1] = sh2[8,i-1]/sx
    sh2[11,i-1] = i
  endfor
  ;
  sorlis = reverse(sort(reform(sh2[1,*])))
  sh2 = sh2[*,sorlis]


  johnnyG, sh2, lr2

  mxroi2 = max(lr2)
  ;
  wstsh, 5
  loadct, 0, /sil
  erase, fsc_color('white')
  tvcircle, rad, xc, yc, /devi, color=fsc_color('black'), thick=2.0
  xyouts, 30, 850, (trim(Ooty)+' start'), /devi, $
  							chars=2.0, chart=2.0, color=fsc_color( 'black')
  ;
  mxstor2 = max(sh2[0,*])
  filamentlist2 = strarr(7,mxstor2+1)

  roaster, sh2, lr2, filamentlist2

  paintif, sh2, lr2


;;;  print, 'START sh2'
;;;  for i=0,n_elements(sh2[0,*])-1 do print, sh2[*,i], $
;;;  					format='(2i6, 2i11, 4i7, i11, 2i7, 2i6)'
;;;  print, ''
  ;


  ;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~ FINAL LOGIC ~*~*~*~*~*~*~*~*~*~*~*~*~*~*
  ;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*

  z = 1

  sh9 = sh2

  for d=0,mxroi2-1 do begin
  	if (sh2[12,d] ne 0) then continue

  	BB90 = reform(sqrt((shPre[9,*]-sh2[09,d])^2 + (shPre[10,*]-sh2[10,d])^2))
  	;
  	BB20 = reform(sqrt((shPre[4,*]-sh2[4,d])^2 + (shPre[5,*]-sh2[5,d])^2))
  	;
  	BB42 = reform(sqrt((shPre[6,*]-sh2[6,d])^2 + (shPre[7,*]-sh2[7,d])^2))
  	;
  	BBxy = BB90 + BB20 + BB42

  	minB = min(BBxy)

  	whXY = where(BBxy eq minB, nwXY)

  	if (nwXY eq 1) then whXY = whXY[0]


  	sh2[00,d] = shPre[0,whXY]
  	sh2[12,d] = z

  	z++
  endfor

  ;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
  ;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~ FINAL LOGIC ~*~*~*~*~*~*~*~*~*~*~*~*~*~*


  wstsh, 6
  loadct, 0, /sil
  erase, fsc_color('white')
  tvcircle, rad, xc, yc, /devi, color=fsc_color('black'), thick=2.0
  xyouts, 30, 850, (trim(Ooty)+' final'), /devi, $
  						chars=2.0, chart=2.0, color=fsc_color('black')

  paintif, sh2, lr2

;;;  print, 'FINAL sh2'
;;;  for i=0,n_elements(sh2[0,*])-1 do print, sh2[*,i], $
;;;  					format='(2i6, 2i11, 4i7, i11, 2i7, 2i6)'
;;;  print, ''


  ;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~ ULTIMA LOGIC *~*~*~*~*~*~*~*~*~*~*~*~*~*
  ;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*

  mxfirl = max(shOne[0,*])
  fieldlis = intarr(5,mxfirl+1)
  fieldlis[*,0] = 0
  for c=1,mxfirl do begin
    fieldlis[0,c] = c
    dw = where(shOne[0,*] eq c, cn)
    if (cn le 0) then CONTINUE

    ds = shOne[*,dw]
    ;
    fieldlis[1,c] = min(ds[4,*])		; Xmin
    fieldlis[2,c] = min(ds[5,*])		; Ymin
    fieldlis[3,c] = max(ds[6,*])		; Xmax
    fieldlis[4,c] = max(ds[7,*])		; Ymax
    ;
    xdif = nint(pix15/2.0)				; 10		; 0.1*(fieldlis[3,c] - fieldlis[1,c])
    ydif = nint(pix15/2.0)				; 10		; 0.1*(fieldlis[4,c] - fieldlis[2,c])
    ;
    Xnot = fieldlis[1,c]-xdif
    Ynot = fieldlis[2,c]-ydif
    Xone = fieldlis[3,c]+xdif
    Yone = fieldlis[4,c]+ydif
    ;
    plots, [Xnot,Xone], [Ynot,Ynot], /devi, color=fsc_color('black')
    plots, [Xone,Xone], [Ynot,Yone], /devi, color=fsc_color('black')
    plots, [Xone,Xnot], [Yone,Yone], /devi, color=fsc_color('black')
    plots, [Xnot,Xnot], [Yone,Ynot], /devi, color=fsc_color('black')
    ;
    xyouts, fieldlis[3,c]+xdif, fieldlis[4,c]+ydif, trim(c), $
    						/devi, chars=2.0, color=fsc_color('black'), align=0.0

  endfor

  ;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
  ;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~ ULTIMA LOGIC *~*~*~*~*~*~*~*~*~*~*~*~*~*


  mxstorF = max(sh2[0,*])
  filamentlistF = strarr(7,mxstorF+1)

  roaster, sh2, lr2, filamentlistF

  wstsh, 8
  erase, fsc_color('white')

  tvcircle, rad, xc, yc, /devi, color=fsc_color('black'), thick=2.0
  xyouts, 30, 850, (trim(Ooty)+' ultima'), /devi, $
  						chars=2.0, chart=2.0, color=fsc_color('black')
  xyouts, 30, 30, tmZ, /devi, charsi=2.0, charth=2.0, colo=fsc_color('black')

  paintif, sh2, lr2

  if (wimplo eq 'jpg') then begin
    Z0 = tvrd(true=1)

    Vnam = str_replace(im[Ooty], 'fits.fz', 'jpg')
    Vman = concat_dir(iSav, Vnam)
    ;
    write_jpeg, Vman, Z0, true=1, qual=100
  endif


;;;  print, 'ULTIMA sh2'
;;;  for i=0,n_elements(sh2[0,*])-1 do print, sh2[*,i], $
;;;  					format='(2i6, 2i11, 4i7, i11, 2i7, 2i6)'
;;;  print, ''

  if ((ch ne 0) and (Ooty mod ch eq 0)) then cursor, x, y, wait, /down
  if (!mouse.button eq 4) then begin
    for g=1,glim do printf, 3*g, ''
    close, /all, /force
    stop
  endif


  gupto = (n_elements(filamentlistF[0,*])-1)<glim
  for g=1,gupto do $
  					printf, 3*g, Ooty, dtQ, tmQ, filamentlistF[1,g], $
  					filamentlistF[3,g], filamentlistF[4,g], $
  					filamentlistF[5,g], filamentlistF[6,g], $
  					format='(i4,a14,f10.4,i8,4i6)'


;; *~*~*~*~*~*~*~*~*~*~* carry over missing labels *~*~*~*~*~*~*~*~*~*~*
;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*

  mxPre = max(shPre[0,*])
  shTmp = lonarr(13, 200)
  eD = 0
  ;
  for e=1,mxPre do begin
	  wh2 = where(sh2[0,*] eq e, ew2)
    ;
    if (ew2 ge 1) then begin
  	  for eP=0,ew2-1 do begin
  		   shTmp[*,eD] = sh2[*,wh2[eP]]
  		   eD++
       endfor

    endif else begin
      wh3 = where(shPre[0,*] eq e, ew3)
       ;
  	  if (ew3 ge 1) then begin
    	  for eP=0,ew3-1 do begin
  	  	  shTmp[*,eD] = shPre[*,wh3[eP]]
  		    eD++
  		  endfor
      endif
    endelse
  endfor

  if (eD gt 50) then print, Ooty, eD, form='(2i8)'
  shPre = temporary(shTmp[*,0:eD-1])

;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
;; *~*~*~*~*~*~*~*~*~*~* carry over missing labels *~*~*~*~*~*~*~*~*~*~*


;;;  roi1 = roi2
;;;  first_list = list2

endfor


;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
;; *~*~*~*~*~*~*~*~*~*~ second time start from here ~*~*~*~*~*~*~*~*~*~*



for g=1,glim do begin
  printf, 3*g, ''
  free_lun, 3*g
endfor
close, /all, /force

print, thap
;;; print, systim()
;;; print, format='(f07.4)', (systim(1)-t0)/60.0
print, (systim(1)-t0)/60.0, ' min', systim(), format='(f05.2, a, 5x, a)'

stop
end



;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*

pro J_prepG, mig, q2, q4, hd, Jcon

  Jcon = 0

  a0 = readfits(mig, hd, /sile)  &  delvarX, a0
  if (fxpar(hd,'crpix1') eq 0) then begin
    if ~dir_exist('2bdel') then file_mkdir, '2bdel'
    file_move, mig, '2bdel'

    Jcon = 1

    return
  endif


  q0 = readfits(mig, hd, /silent)
  Q_sx = fxpar(hd, 'naxis1')					; if (sx ne sy) then
  Q_sy = fxpar(hd, 'naxis2')					; add (or subtract) rows/columns
  ;
  Q_xco = fxpar(hd, 'crpix1')				; use the correct header keyword
  Q_yco = fxpar(hd, 'crpix2')				; use the correct header keyword
  Q_r0o = fxpar(hd, 'radius')				; use the correct header keyword

  if (Q_sx eq Q_sy) then begin
    facxy = Q_sx/1024									; assuming (sx ge 1024)
    facR0 = Q_sy/facxy
  endif else begin
    message, 'add (or subtract) rows/columns to make sx = sy'
  endelse
  ;
  q1 = shift(q0, 512*facxy-Q_xco, 512*facxy-Q_yco)
  ;
  Q_xc = 512.0
  Q_yc = 512.0
  Q_r0 = float(Q_r0o)/facxy
  ;
  sxaddpar, hd, 'crpix1', Q_xc
  sxaddpar, hd, 'crpix2', Q_yc
  sxaddpar, hd, 'radius', Q_r0

  if ((Q_sx ne 1024) and ((Q_sx mod 1024) eq 0)) then begin
    q2 = rebin(q1, 1024, 1024) 		; resize to 1024x1024
    ;
    sxaddpar, hd, 'naxis1', 1024
    sxaddpar, hd, 'naxis2', 1024

    dlt1 = fxpar(hd,'cdelt1')  &  dlt2 = fxpar(hd,'cdelt2')
    ;
    sxaddpar, hd, 'cdelt1', dlt1*facxy
    sxaddpar, hd, 'cdelt2', dlt1*facxy
    
;    stop
  endif else begin
    q2 = q1
  endelse

  ; *~*~*~*~*~*~*~*~*~*~ limb darkening correction ~*~*~*~*~*~*~*~*~*~*

  ;;p = 2.0  &  t = 400

  J_limb_mskC, q2, q3, p=p, t=t, xc=Q_xc, yc=Q_yc, r0=Q_r0

  q4 = q2/q3				; q3 is the mask showing average CLV for the disc
  ;
  tt = systim()
  sxaddpar, hd, 'COMMENT', $
  					strcompress('LIMB DARKENING correction appied on ' + tt)

  ; *~*~*~*~*~*~*~*~*~*~ limb darkening correction ~*~*~*~*~*~*~*~*~*~*

;;;  ; *~*~*~*~*~*~*~*~*~*~ foreshortening correction ~*~*~*~*~*~*~*~*~*~*

;;;  K_area_prjC, q4, q5, xc=Q_xc, yc=Q_yc, r0=Q_r0

;;;  tt = systim()
;;;  sxaddpar, hd, 'COMMENT', $
;;;            strcompress('FORESHORTENING correction applied on ' + tt)

;;;  ; *~*~*~*~*~*~*~*~*~*~ foreshortening correction ~*~*~*~*~*~*~*~*~*~*

end

;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*


;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*

pro threshM, aa, thr_a

common JUMBO, sx, sy, fscol, pix40, pix15, r0, x0, y0

  inten1 = max(aa) - min(aa)

  th = fltarr(1024,1024)
  thr_a = fltarr(1024,1024)

  low1 = 0.1*inten1 + min(aa)
  hii1 = 0.9*inten1 + min(aa)


  for i=0,1023 do begin
    for j=0,1023 do begin
      if (sqrt((i-x0)^2+(j-y0)^2) gt r0) then CONTINUE

      th[i,j] = median(aa[i-9:i+9,j-9:j+9])

      pxin = aa[i,j]
      medpx = th[i,j]*0.92											; local threshold

      if (pxin le low1) then begin							; for too dark
        thr_a[i,j] = 0
        CONTINUE
      endif else if (pxin ge hii1) then begin		; for too bright
        thr_a[i,j] = 1
        CONTINUE
      endif

      if (medpx le low1) then begin							; setting threshold
        thresh = low1
      endif else if (medpx ge hii1) then begin
        thresh = hii1
      endif else begin
        thresh = medpx
      endelse

      if (pxin ge thresh) then begin						; applying threshold 
        thr_a[i,j] = 1
      endif else begin
        thr_a[i,j] = 0
      endelse
    endfor
  endfor

end

;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*


;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*

pro pureFil, filan_a, noise_fac, pure_a

common JUMBO, sx, sy, fscol, pix40, pix15

  Froi = label_region(filan_a)

  pure_a = filan_a
  if (noise_fac eq 0) then begin
    return
  endif else begin
    for i=1,max(Froi) do begin
      ds = where(Froi eq i, count)
      if (count le noise_fac) then begin
        ax = ds mod sx
        ay = ds/sx
        pure_a[ax,ay] = 0
      endif
    endfor
  endelse

end

;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*


;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*

pro Amnmxmd, lishtt, ijk, minx, miny, maxx, maxy, medx, medy

  minx = lishtt[4,ijk]
  miny = lishtt[5,ijk]
  maxx = lishtt[6,ijk]
  maxy = lishtt[7,ijk]
  medx = lishtt[9,ijk]
  medy = lishtt[10,ijk]

end

;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*


;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*

pro johnnyG, vlist, vroi

	maxroi = max(vroi)

  for adder=1,49 do begin

    p = min(where(vlist[0,*] eq 0))
    if (p eq -1) then BREAK               ; escape if no unassigned fragments

    BLL1 = strarr(maxroi)                 ; string array to store fragment sr nos.
    xb = 0
    BLL1[xb] = trim(p)
    vlist[0,p] = adder
    ;
    xb++

    for xx=0,maxroi-1 do begin						; loop over the string array BLL1
      if (BLL1[xx] eq '') then stop				; but only when BLL1[xx] is non-empty
      xb0 = xb

      Amnmxmd, vlist, BLL1[xx], minx1, miny1, maxx1, maxy1, medx1, medy1

      for q=1,maxroi-1 do begin
        if (vlist[0,q] eq 0) then begin

          Amnmxmd, vlist, q, minx2, miny2, maxx2, maxy2, medx2, medy2

		      ifferfi, minx1, miny1, maxx1, maxy1, medx1, medy1, $
          					minx2, miny2, maxx2, maxy2, medx2, medy2, fer

          if (fer eq 1) then begin
            BLL1[xb] = trim(q)            ; save fragment no. for more comparisons
            vlist[0,q] = adder            ; assign label to fragment
            ;
            xb++													; flag for a positive detection
          endif
        endif
      endfor
      ;
      if (xb eq xb0) then break						; escape if no positive detection
    endfor
  endfor

end

;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*


;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*

pro ifferfi, minx1, miny1, maxx1, maxy1, medx1, medy1, $
	    						minx2, miny2, maxx2, maxy2, medx2, medy2, fer

common JUMBO, sx, sy, fscol, pix40, pix15

  fer = 0
  if ( $
  					((abs(maxx1-minx2) le pix40) and $
  					 (abs(maxy1-miny2) le pix40)) or $			; 1A (slope=+1, 1-2)
  					((abs(maxx2-minx1) le pix40) and $
  					 (abs(maxy2-miny1) le pix40)) or $			; 1B (slope=+1, 2-1)
  					;
  					((abs(maxx1-minx2) le pix40) and $
  					 (abs(miny1-maxy2) le pix40)) or $			; 1C (slope=-1, 1-2)
  					((abs(maxx2-minx1) le pix40) and $
  					 (abs(miny2-maxy1) le pix40)) or $			; 1D (slope=-1, 2-1)
  					;
  					((abs(medx1-medx2) le pix15) and $
  					 (abs(medy1-medy2) le pix15)) $					; A0 (adjacent fragments)
  					;
;;  					((abs(maxx1-minx2) le pix15) and $
;;  					 (abs(maxy1-maxy2) le pix15)) or $			; 2A (^ shape, 1-2)
;;  					((abs(maxx2-minx1) le pix15) and $
;;  					 (abs(maxy2-maxy1) le pix15)) or $			; 2B (^ shape, 2-1)
;;  					((abs(maxx1-minx2) le pix15) and $
;;  					 (abs(miny1-miny2) le pix15)) or $			; 2C (v shape, 1-2)
;;  					((abs(maxx2-minx1) le pix15) and $
;;  					 (abs(miny2-miny1) le pix15)) or $			; 2D (v shape, 2-1)
;;  					;
;;  					((abs(maxx1-maxx2) le pix15) and $
;;  					 (abs(miny1-maxy2) le pix15)) or $			; 3A (> shape, 1-2)
;;  					((abs(maxx2-maxx1) le pix15) and $
;;  					 (abs(miny2-maxy1) le pix15)) or $			; 3B (> shape, 2-1)
;;  					((abs(minx1-minx2) le pix15) and $
;;  					 (abs(miny1-maxy2) le pix15)) or $			; 3C (< shape, 1-2)
;;  					((abs(minx2-minx1) le pix15) and $
;;  					 (abs(miny2-maxy1) le pix15)) or $			; 3C (< shape, 2-1)
  					) then begin
    fer = 1
  endif

end

;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*


;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*

pro roaster, wlist, wroi, fillisht

common JUMBO, sx, sy, fscol, pix40, pix15

  wstor = wlist[0,*]
  mxstor = max(wstor)
  fillisht1 = strarr(7, mxstor+1)

  for num=1,mxstor do begin
    fillisht[0,num] = num										; serial number

    sum = 0																	; total length of a filament
    for i=0,max(wlist[11,*])-1 do $
      				if (wlist[0,i] eq num) then sum = sum + total(wlist[1,i])
    ;
    fillisht[1,num] = nint(sum)							; rounding for shorter display

    if (num lt 45) then $										; colour for labelling
    					fillisht[2,num] = fscol[num] else $
      				fillisht[2,num] = fscol[44]

    pqr = 0																	; no. of fragments
    for i=0,max(wlist[11,*])-1 do $
				      if (wlist[0,i] eq num) then pqr++
    fillisht[3,num] = pqr

    fileq = where(wlist[0,*] eq num, nq)		; total length
    if (nq eq 0) then CONTINUE							; sum of lengths of all fragments
    xymdq = intarr(2,nq)
    ;
    for i=0,nq-1 do begin
      l = fileq[i]
      fillisht[4,num] = fillisht[4,num] + $
        					sqrt( (wlist[4,l] - wlist[6,l])^2 + $
        							  (wlist[5,l] - wlist[7,l])^2 )
      ;
      xymdq[0,i] = wlist[9,l]
      xymdq[1,i] = wlist[10,l]
    endfor
    ;
    fillisht[5,num] = median(xymdq[0,*])		; X median of entire filament
    fillisht[6,num] = median(xymdq[1,*])		; Y median of entire filament
  endfor
  ;
  fillisht[0,0] = 'No.'
  fillisht[1,0] = 'AREA'
  fillisht[2,0] = 'COLOUR'
  fillisht[3,0] = 'PARTS'
  fillisht[4,0] = 'LENGTH'
  fillisht[5,0] = 'xMEDIAN'
  fillisht[6,0] = 'yMEDIAN'

end

;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*


;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*

pro paintif, wlist, wroi

common JUMBO, sx, sy, fscol, pix40, pix15

  wstor = wlist[0,*]
  ;
  for num=1,max(wstor) do begin
    for i=0,max(wlist[11,*])-1 do begin
      if (wlist[0,i] ne num) then CONTINUE

      ds = where(wroi eq wlist[11,i], cn)

   	  xcol = ds mod sx
   	  ycol = ds/sx
   	  ;
      clr = num<44
      xyouts, xcol, ycol, '.', /device, color=fsc_color(fscol[clr]), $
      					align=0.5, chars=0.5

   	  b1 = median(ds) mod sx
      b2 = median(ds)/sx
      ;
      xyouts, b1, b2+7, trim(num), /device, align=0.5, $
      					chars=2.0, chart=2.0, color=fsc_color(fscol[clr])
    endfor
  endfor

end

;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*