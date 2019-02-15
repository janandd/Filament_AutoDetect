function dj_gettime, tm, short=short, days=days
;
;+
;-
;
if n_elements(short) then begin
  return, strmid(tm,00,2)*1.0d + strmid(tm,03,2)/60.0d + strmid(tm,06)/3600.d
endif else if n_elements(days) then begin
  return, strmid(tm,08,2)*1.0d + $
  					(strmid(tm,11,2)*1.0d + strmid(tm,14,2)/60.0d + strmid(tm,17)/3600.d)/24
endif else begin
  return, strmid(tm,11,2)*1.0d + strmid(tm,14,2)/60.0d + strmid(tm,17)/3600.d
endelse

;;IDL> print, hra.date_obs
;;2007-05-19T12:51:45.007
;;01234567890123456789012

;;;sht = n_elements(short)
;;;dys = n_elements(days)
;;;return, strmid(tm,08,2)*(dys and 1.0d)  +  $
;;;					strmid(tm,11-11*sht,2)*1.0d + strmid(tm,14-11*sht,2)/60.0d + strmid(tm,17-11*sht)/3600.d


end